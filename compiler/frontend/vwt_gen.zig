//! Value Witness Table Generation — Swift ABI 1:1
//!
//! Generates VWT witness functions for each concrete type. Each witness is a
//! real callable IR function that goes through the SSA → native/wasm pipeline.
//!
//! Reference: apple/swift lib/IRGen/GenValueWitness.cpp
//!
//! Architecture:
//!   - POD types: memcpy for copy, noop for destroy
//!   - Managed pointers: retain for copy, release for destroy
//!   - Collections (List/Map): retain(buf) for copy, release(buf) for destroy
//!   - Structs: field-by-field using sub-VWT witnesses
//!   - Unions/Enums: tag-switch + variant payload witness dispatch
//!   - Optionals: single-payload enum via extra inhabitants
//!
//! Each witness function takes (*anyopaque, ..., *TypeMetadata) and is emitted
//! as a normal IR function compiled through the standard pipeline.

const std = @import("std");
const ir = @import("ir.zig");
const types = @import("types.zig");
const TypeRegistry = types.TypeRegistry;
const TypeIndex = types.TypeIndex;
const source = @import("source.zig");
const Span = source.Span;

/// Type category for VWT witness generation. Determines which witness pattern to use.
const TypeCategory = enum {
    pod, // All basic types, raw pointers, enums — noop destroy, memcpy copy
    managed_ptr, // Managed pointer (*T from new) — retain/release
    collection, // List, Map, generic collections — retain/release buf
    struct_with_arc, // Struct with at least one non-trivial field
    union_with_arc, // Union with at least one non-trivial variant payload
    optional_with_arc, // Optional wrapping a non-trivial type
    error_union_with_arc, // Error union wrapping a non-trivial type
    array_with_arc, // Array with non-trivial element type
    tuple_with_arc, // Tuple with at least one non-trivial element
};

/// VWT generation context. Tracks which types have had VWTs emitted
/// to avoid duplicate generation across files.
pub const VWTGenerator = struct {
    allocator: std.mem.Allocator,
    type_reg: *TypeRegistry,
    /// Set of type names that already have VWT functions emitted.
    emitted: std.StringHashMap(void),
    /// Collected VWT metadata entries: type_name → VWTEntry.
    entries: std.StringHashMap(VWTEntry),

    pub const VWTEntry = struct {
        type_idx: TypeIndex,
        destroy_fn: []const u8, // name of destroy witness function
        initializeWithCopy_fn: []const u8, // name of initializeWithCopy function
        assignWithCopy_fn: []const u8,
        initializeWithTake_fn: []const u8,
        assignWithTake_fn: []const u8,
        size: u64,
        stride: u64,
        flags: types.ValueWitnessFlags,
        extra_inhabitant_count: u32,
        has_enum_witnesses: bool,
        // Enum witness names (only if has_enum_witnesses)
        getEnumTag_fn: ?[]const u8 = null,
        destructiveProjectEnumData_fn: ?[]const u8 = null,
        destructiveInjectEnumTag_fn: ?[]const u8 = null,
    };

    pub fn init(allocator: std.mem.Allocator, type_reg: *TypeRegistry) VWTGenerator {
        return .{
            .allocator = allocator,
            .type_reg = type_reg,
            .emitted = std.StringHashMap(void).init(allocator),
            .entries = std.StringHashMap(VWTEntry).init(allocator),
        };
    }

    pub fn deinit(self: *VWTGenerator) void {
        self.emitted.deinit();
        self.entries.deinit();
    }

    /// Compute VWT metadata for a concrete type WITHOUT generating witness functions.
    /// This is used to build VWTEntry records that can be emitted later.
    pub fn computeEntry(self: *VWTGenerator, type_idx: TypeIndex, type_name: []const u8) !VWTEntry {
        const info = self.type_reg.get(type_idx);
        const size = self.type_reg.sizeOf(type_idx);
        const alignment = self.type_reg.alignmentOf(type_idx);
        const stride = ((size + alignment - 1) / alignment) * alignment; // round up to alignment
        const is_pod = self.type_reg.isTrivial(type_idx);
        const is_bitwise_takable = self.isBitwiseTakable(type_idx);
        const is_inline = types.ValueWitnessTable.canBeInline(is_bitwise_takable, size, alignment);
        const has_enum = (info == .union_type or info == .optional);
        const extra_inhabitants = self.computeExtraInhabitants(type_idx);

        const flags = types.ValueWitnessFlags.init(
            alignment,
            is_pod,
            is_inline,
            is_bitwise_takable,
            has_enum,
        );

        return VWTEntry{
            .type_idx = type_idx,
            .destroy_fn = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{type_name}),
            .initializeWithCopy_fn = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{type_name}),
            .assignWithCopy_fn = try std.fmt.allocPrint(self.allocator, "__vwt_assignWithCopy_{s}", .{type_name}),
            .initializeWithTake_fn = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithTake_{s}", .{type_name}),
            .assignWithTake_fn = try std.fmt.allocPrint(self.allocator, "__vwt_assignWithTake_{s}", .{type_name}),
            .size = size,
            .stride = if (stride == 0) 1 else stride,
            .flags = flags,
            .extra_inhabitant_count = extra_inhabitants,
            .has_enum_witnesses = has_enum,
            .getEnumTag_fn = if (has_enum)
                try std.fmt.allocPrint(self.allocator, "__vwt_getEnumTag_{s}", .{type_name})
            else
                null,
            .destructiveProjectEnumData_fn = if (has_enum)
                try std.fmt.allocPrint(self.allocator, "__vwt_destructiveProjectEnumData_{s}", .{type_name})
            else
                null,
            .destructiveInjectEnumTag_fn = if (has_enum)
                try std.fmt.allocPrint(self.allocator, "__vwt_destructiveInjectEnumTag_{s}", .{type_name})
            else
                null,
        };
    }

    /// Check if a type is bitwise-takable (can be moved with memcpy).
    /// Swift: most types are bitwise-takable except those with weak references
    /// or other address-sensitive storage.
    /// In Cot, all types are bitwise-takable (no weak refs yet affect layout).
    fn isBitwiseTakable(self: *VWTGenerator, type_idx: TypeIndex) bool {
        _ = self;
        _ = type_idx;
        // Cot: all types are bitwise-takable. Weak references are stored as
        // side-table pointers (i64), not inline in the struct layout.
        return true;
    }

    /// Compute extra inhabitant count for Optional optimization.
    /// Reference: Swift Enum.cpp getNumExtraInhabitants
    ///
    /// Pointers have many extra inhabitants (null, low addresses).
    /// Integers have 0 extra inhabitants.
    /// Structs: minimum of all fields' extra inhabitants.
    fn computeExtraInhabitants(self: *VWTGenerator, type_idx: TypeIndex) u32 {
        const info = self.type_reg.get(type_idx);
        return switch (info) {
            // Pointers: null is an extra inhabitant (at minimum)
            // Swift uses ~2 billion for pointers; we use a conservative count.
            .pointer => 1, // null pointer = .none for Optional
            // Basic types: no extra inhabitants (all bit patterns are valid)
            .basic => 0,
            // Enums: depends on spare bits (conservative: 0)
            .enum_type => 0,
            // Collections: buf pointer has null as extra inhabitant
            .list, .map => 1,
            // Structs: minimum extra inhabitants across fields
            .struct_type => |s| {
                if (s.fields.len == 0) return 0;
                var min: u32 = std.math.maxInt(u32);
                for (s.fields) |field| {
                    const ei = self.computeExtraInhabitants(field.type_idx);
                    if (ei < min) min = ei;
                }
                return if (min == std.math.maxInt(u32)) 0 else min;
            },
            // Unions: conservative 0 (tag is explicit)
            .union_type => 0,
            // Optionals: 0 (already uses extra inhabitants of payload)
            .optional => 0,
            // Everything else: 0
            else => 0,
        };
    }

    // ========================================================================
    // Witness Function Emission
    // ========================================================================
    // Each witness is emitted as a real IR function using the same FuncBuilder
    // API that the lowerer uses. These go through SSA → native/wasm pipeline.
    //
    // Swift reference: lib/IRGen/GenValueWitness.cpp
    //
    // Parameter convention (all witnesses):
    //   param 0: dest/object pointer (i64)
    //   param 1: src pointer (i64) — for copy/assign witnesses
    //   param 2: metadata pointer (i64) — *TypeMetadata (unused in generated code but ABI-required)
    //   return: dest pointer (i64) — for copy/assign; void for destroy

    /// Classify a type into its VWT witness category.
    fn classifyType(self: *VWTGenerator, type_idx: TypeIndex) TypeCategory {
        const info = self.type_reg.get(type_idx);
        const is_pod = self.type_reg.isTrivial(type_idx);

        if (is_pod) return .pod;

        if (info == .pointer and info.pointer.managed) return .managed_ptr;

        if (info == .list or info == .map) return .collection;
        if (info == .struct_type and types.TypeRegistry.isGenericCollection(info.struct_type.name)) return .collection;

        if (info == .struct_type) return .struct_with_arc;
        if (info == .union_type) return .union_with_arc;
        if (info == .optional) return .optional_with_arc;
        if (info == .error_union) return .error_union_with_arc;
        if (info == .array) return .array_with_arc;
        if (info == .tuple) return .tuple_with_arc;

        return .pod; // fallback for types we don't know about
    }

    /// Emit all VWT witness functions for a concrete type into the IR builder.
    /// Skips types that already have witnesses emitted (dedup via `emitted` set).
    /// For composite types: recursively ensures sub-type witnesses are emitted first.
    pub fn emitWitnesses(self: *VWTGenerator, builder: *ir.Builder, type_idx: TypeIndex, type_name: []const u8) !VWTEntry {
        // Dedup: skip if already emitted
        if (self.emitted.contains(type_name)) {
            return self.entries.get(type_name) orelse return error.MissingVWTEntry;
        }

        const entry = try self.computeEntry(type_idx, type_name);
        const info = self.type_reg.get(type_idx);
        const cat = self.classifyType(type_idx);

        // Recursively emit witnesses for non-trivial sub-types
        switch (cat) {
            .struct_with_arc => {
                for (info.struct_type.fields) |field| {
                    if (!self.type_reg.isTrivial(field.type_idx)) {
                        _ = try self.emitWitnesses(builder, field.type_idx, self.type_reg.typeName(field.type_idx));
                    }
                }
            },
            .union_with_arc => {
                for (info.union_type.variants) |variant| {
                    if (variant.payload_type != types.invalid_type and !self.type_reg.isTrivial(variant.payload_type)) {
                        _ = try self.emitWitnesses(builder, variant.payload_type, self.type_reg.typeName(variant.payload_type));
                    }
                }
            },
            .optional_with_arc => {
                _ = try self.emitWitnesses(builder, info.optional.elem, self.type_reg.typeName(info.optional.elem));
            },
            .error_union_with_arc => {
                _ = try self.emitWitnesses(builder, info.error_union.elem, self.type_reg.typeName(info.error_union.elem));
            },
            .array_with_arc => {
                _ = try self.emitWitnesses(builder, info.array.elem, self.type_reg.typeName(info.array.elem));
            },
            .tuple_with_arc => {
                for (info.tuple.element_types) |et| {
                    if (!self.type_reg.isTrivial(et)) {
                        _ = try self.emitWitnesses(builder, et, self.type_reg.typeName(et));
                    }
                }
            },
            else => {},
        }

        // --- destroy ---
        try self.emitDestroyWitness(builder, entry, type_idx, cat);

        // --- initializeWithCopy ---
        try self.emitInitializeWithCopyWitness(builder, entry, type_idx, cat);

        // --- assignWithCopy ---
        try self.emitAssignWithCopyWitness(builder, entry, cat);

        // --- initializeWithTake (always memcpy — all Cot types are bitwise-takable) ---
        try self.emitInitializeWithTakeWitness(builder, entry);

        // --- assignWithTake ---
        try self.emitAssignWithTakeWitness(builder, entry, cat);

        // --- enum witnesses (for unions, optionals, error unions) ---
        if (info == .union_type or info == .optional) {
            try self.emitGetEnumTagWitness(builder, entry);
            try self.emitDestructiveProjectEnumDataWitness(builder, entry);
            try self.emitDestructiveInjectEnumTagWitness(builder, entry);
        }

        // --- single-payload enum witnesses (optionals, error unions) ---
        if (info == .optional or info == .error_union) {
            try self.emitGetEnumTagSinglePayloadWitness(builder, type_name);
            try self.emitStoreEnumTagSinglePayloadWitness(builder, type_name);
        }

        // --- initializeBufferWithCopyOfBuffer (all types) ---
        try self.emitInitializeBufferWithCopyOfBufferWitness(builder, entry, type_name);

        // --- TypeMetadata + VWT globals ---
        try self.emitMetadataGlobals(builder, entry, type_name, type_idx, info);

        try self.emitted.put(type_name, {});
        try self.entries.put(type_name, entry);
        return entry;
    }

    /// Emit global data for VWT table and TypeMetadata for a concrete type.
    /// Swift pattern: VWT is a global constant struct with function pointers + data.
    /// TypeMetadata is a global constant with VWT pointer + kind + type-specific fields.
    ///
    /// VWT layout (88 bytes on 64-bit):
    ///   [0]  initializeBufferWithCopyOfBuffer  (fn ptr, 8B)
    ///   [1]  destroy                           (fn ptr, 8B)
    ///   [2]  initializeWithCopy                (fn ptr, 8B)
    ///   [3]  assignWithCopy                    (fn ptr, 8B)
    ///   [4]  initializeWithTake                (fn ptr, 8B)
    ///   [5]  assignWithTake                    (fn ptr, 8B)
    ///   [6]  getEnumTagSinglePayload           (fn ptr, 8B)
    ///   [7]  storeEnumTagSinglePayload         (fn ptr, 8B)
    ///   [8]  size                              (u64, 8B)
    ///   [9]  stride                            (u64, 8B)
    ///   [10] flags:u32 + extraInhabitantCount:u32 (packed, 8B)
    ///
    /// TypeMetadata layout (32 bytes):
    ///   [0]  vwt pointer                       (ptr to VWT global, 8B) — 0 for trivial types
    ///   [1]  size                              (u64, 8B) — @sizeOf(T)
    ///   [2]  stride                            (u64, 8B)
    ///   [3]  kind                              (TypeMetadataKind, 8B)
    ///
    /// Swift ref: Metadata.h — size/stride are directly in metadata for fast access.
    /// @sizeOf(T) in generic bodies loads metadata[1] (offset 0x08).
    fn emitMetadataGlobals(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry, type_name: []const u8, type_idx: TypeIndex, info: types.Type) !void {
        const vwt_global_name = try std.fmt.allocPrint(self.allocator, "__vwt_table_{s}", .{type_name});
        const metadata_global_name = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{type_name});
        const init_fn_name = try std.fmt.allocPrint(self.allocator, "__vwt_init_{s}", .{type_name});

        // Global for VWT table: 11 i64 slots = 88 bytes
        try builder.addGlobal(ir.Global.initWithSize(vwt_global_name, TypeRegistry.I64, false, Span.zero, 88));

        // Global for TypeMetadata: 5 i64 slots = 40 bytes
        // [0]=vwt_ptr, [1]=size, [2]=stride, [3]=type_idx, [4]=hash_fn_ptr
        try builder.addGlobal(ir.Global.initWithSize(metadata_global_name, TypeRegistry.I64, false, Span.zero, 40));

        // Emit init function that populates VWT with function addresses + data
        builder.startFunc(init_fn_name, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
        const fb = builder.func() orelse return;

        // Look up VWT global index
        const vwt_info = builder.lookupGlobal(vwt_global_name) orelse return;
        const meta_info = builder.lookupGlobal(metadata_global_name) orelse return;

        // VWT base address
        const vwt_addr = try fb.emitAddrGlobal(vwt_info.idx, vwt_global_name, TypeRegistry.I64, Span.zero);

        // Store function pointers at VWT offsets [0]-[7]
        const buffer_copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeBufferWithCopyOfBuffer_{s}", .{type_name});
        try self.storeWitnessSlot(fb, vwt_addr, 0, buffer_copy_name);
        try self.storeWitnessSlot(fb, vwt_addr, 1, entry.destroy_fn);
        try self.storeWitnessSlot(fb, vwt_addr, 2, entry.initializeWithCopy_fn);
        try self.storeWitnessSlot(fb, vwt_addr, 3, entry.assignWithCopy_fn);
        try self.storeWitnessSlot(fb, vwt_addr, 4, entry.initializeWithTake_fn);
        try self.storeWitnessSlot(fb, vwt_addr, 5, entry.assignWithTake_fn);

        // Single-payload witnesses [6]-[7]: use type-specific or noop
        const get_single_name = try std.fmt.allocPrint(self.allocator, "__vwt_getEnumTagSinglePayload_{s}", .{type_name});
        const store_single_name = try std.fmt.allocPrint(self.allocator, "__vwt_storeEnumTagSinglePayload_{s}", .{type_name});
        if (info == .optional or info == .error_union) {
            try self.storeWitnessSlot(fb, vwt_addr, 6, get_single_name);
            try self.storeWitnessSlot(fb, vwt_addr, 7, store_single_name);
        } else {
            // Non-optional types: store 0 (no single-payload witnesses)
            try self.storeDataSlot(fb, vwt_addr, 6, 0);
            try self.storeDataSlot(fb, vwt_addr, 7, 0);
        }

        // Data witnesses [8]-[10]
        try self.storeDataSlot(fb, vwt_addr, 8, @intCast(entry.size));
        try self.storeDataSlot(fb, vwt_addr, 9, @intCast(entry.stride));
        // Pack flags (u32) + extraInhabitantCount (u32) into one i64
        const packed_flags: i64 = @intCast(@as(u64, entry.flags.data) | (@as(u64, entry.extra_inhabitant_count) << 32));
        try self.storeDataSlot(fb, vwt_addr, 10, packed_flags);

        // TypeMetadata: [0]=vwt_ptr, [1]=size, [2]=stride, [3]=kind
        const meta_addr = try fb.emitAddrGlobal(meta_info.idx, metadata_global_name, TypeRegistry.I64, Span.zero);
        // [0] Store VWT pointer (address of VWT global)
        _ = try fb.emitPtrStoreValue(meta_addr, vwt_addr, Span.zero);
        // [1] Store size — direct in metadata for fast @sizeOf(T) access
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const meta_size_addr = try fb.emitBinary(.add, meta_addr, eight, TypeRegistry.I64, Span.zero);
        const size_const = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(meta_size_addr, size_const, Span.zero);
        // [2] Store stride
        const sixteen = try fb.emitConstInt(16, TypeRegistry.I64, Span.zero);
        const meta_stride_addr = try fb.emitBinary(.add, meta_addr, sixteen, TypeRegistry.I64, Span.zero);
        const stride_const = try fb.emitConstInt(@intCast(entry.stride), TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(meta_stride_addr, stride_const, Span.zero);
        // [3] Store type_idx — Swift Metadata.Kind equivalent.
        // Stores the actual TypeIndex for runtime type identity and dispatch.
        const twentyfour = try fb.emitConstInt(24, TypeRegistry.I64, Span.zero);
        const meta_kind_addr = try fb.emitBinary(.add, meta_addr, twentyfour, TypeRegistry.I64, Span.zero);
        const kind_const = try fb.emitConstInt(@intCast(type_idx), TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(meta_kind_addr, kind_const, Span.zero);

        // [4] Store hash_fn_ptr — Swift PWT pattern: protocol witness stored in metadata.
        // For Hashable types (i64, string), store their hash function address.
        // For non-Hashable types, store 0 (unused).
        const thirtytwo = try fb.emitConstInt(32, TypeRegistry.I64, Span.zero);
        const meta_hash_addr = try fb.emitBinary(.add, meta_addr, thirtytwo, TypeRegistry.I64, Span.zero);
        const hash_fn_name = self.getHashFnName(type_idx, type_name);
        if (hash_fn_name) |hfn| {
            if (builder.hasFunc(hfn)) {
                const hash_fn_addr = try fb.emitFuncAddr(hfn, TypeRegistry.I64, Span.zero);
                _ = try fb.emitPtrStoreValue(meta_hash_addr, hash_fn_addr, Span.zero);
            } else {
                const zero_hash = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
                _ = try fb.emitPtrStoreValue(meta_hash_addr, zero_hash, Span.zero);
            }
        } else {
            const zero_hash = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
            _ = try fb.emitPtrStoreValue(meta_hash_addr, zero_hash, Span.zero);
        }

        _ = try fb.emitRet(null, Span.zero);
        try builder.endFunc();
    }

    /// Get the hash function name for a type (Hashable protocol witness).
    /// Returns null for types that don't implement Hashable.
    fn getHashFnName(_: *VWTGenerator, type_idx: TypeIndex, type_name: []const u8) ?[]const u8 {
        // i64/int types use i64_hash
        if (type_idx == TypeRegistry.I64 or type_idx == TypeRegistry.I32 or
            type_idx == TypeRegistry.I16 or type_idx == TypeRegistry.I8 or
            type_idx == TypeRegistry.U64 or type_idx == TypeRegistry.U32 or
            type_idx == TypeRegistry.U16 or type_idx == TypeRegistry.U8)
            return "std.map.i64_hash";
        // String uses string_hash
        if (type_idx == TypeRegistry.STRING) return "std.map.string_hash";
        // Check for known type names (for types not matched by index)
        if (std.mem.eql(u8, type_name, "i64") or std.mem.eql(u8, type_name, "int"))
            return "std.map.i64_hash";
        if (std.mem.eql(u8, type_name, "string"))
            return "std.map.string_hash";
        return null;
    }

    /// Store a function address into a VWT slot.
    fn storeWitnessSlot(self: *VWTGenerator, fb: *ir.FuncBuilder, vwt_addr: ir.NodeIndex, slot: u32, fn_name: []const u8) !void {
        _ = self;
        const fn_addr = try fb.emitFuncAddr(fn_name, TypeRegistry.I64, Span.zero);
        const offset = try fb.emitConstInt(@intCast(@as(u64, slot) * 8), TypeRegistry.I64, Span.zero);
        const slot_addr = try fb.emitBinary(.add, vwt_addr, offset, TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(slot_addr, fn_addr, Span.zero);
    }

    /// Store a data value into a VWT slot.
    fn storeDataSlot(self: *VWTGenerator, fb: *ir.FuncBuilder, vwt_addr: ir.NodeIndex, slot: u32, value: i64) !void {
        _ = self;
        const val = try fb.emitConstInt(value, TypeRegistry.I64, Span.zero);
        const offset = try fb.emitConstInt(@intCast(@as(u64, slot) * 8), TypeRegistry.I64, Span.zero);
        const slot_addr = try fb.emitBinary(.add, vwt_addr, offset, TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(slot_addr, val, Span.zero);
    }

    /// destroy(object_ptr, metadata) → void
    /// Swift ABI: object_ptr is OpaqueValue* — a pointer TO the value's memory.
    /// For a managed pointer type, the value at *object_ptr is the pointer itself.
    /// POD: noop. Managed ptr: release(*object_ptr). Collection: release(buf field).
    fn emitDestroyWitness(
        self: *VWTGenerator,
        builder: *ir.Builder,
        entry: VWTEntry,
        type_idx: TypeIndex,
        cat: TypeCategory,
    ) !void {
        builder.startFunc(entry.destroy_fn, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("object_ptr", TypeRegistry.I64, 8); // pointer TO value
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        switch (cat) {
            .pod => {}, // noop — nothing to release
            .managed_ptr => {
                // Load the pointer value, then release it
                const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
                const ptr_val = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
                var args = [_]ir.NodeIndex{ptr_val};
                _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, Span.zero);
            },
            .collection => {
                // Load buf field (offset 0), if non-null release(buf).
                const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
                const buf = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
                const zero = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
                const is_non_null = try fb.emitBinary(.ne, buf, zero, TypeRegistry.BOOL, Span.zero);
                const release_blk = try fb.newBlock("release");
                const done_blk = try fb.newBlock("done");
                _ = try fb.emitBranch(is_non_null, release_blk, done_blk, Span.zero);
                fb.setBlock(release_blk);
                var args = [_]ir.NodeIndex{buf};
                _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, Span.zero);
                _ = try fb.emitJump(done_blk, Span.zero);
                fb.setBlock(done_blk);
            },
            .struct_with_arc => {
                // Destroy each non-trivial field in reverse order.
                const info = self.type_reg.get(type_idx);
                const fields = info.struct_type.fields;
                const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
                const metadata = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

                var i: usize = fields.len;
                while (i > 0) {
                    i -= 1;
                    const field = fields[i];
                    if (self.type_reg.isTrivial(field.type_idx)) continue;

                    const offset = try fb.emitConstInt(@intCast(field.offset), TypeRegistry.I64, Span.zero);
                    const field_ptr = try fb.emitBinary(.add, obj_ptr, offset, TypeRegistry.I64, Span.zero);

                    const field_type_name = self.type_reg.typeName(field.type_idx);
                    const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{field_type_name});
                    var destroy_args = [_]ir.NodeIndex{ field_ptr, metadata };
                    _ = try fb.emitCall(destroy_name, &destroy_args, false, TypeRegistry.VOID, Span.zero);
                }
            },
            .union_with_arc => {
                // Load tag, branch to destroy active payload.
                // Layout: tag (8 bytes at offset 0) + payload (at offset 8).
                try self.emitTagSwitchDestroy(fb, type_idx);
            },
            .optional_with_arc, .error_union_with_arc => {
                // Layout: tag (8 bytes at offset 0) + payload (at offset 8).
                // Tag 0 = some/ok (has payload), tag 1 = none/error (no ARC payload).
                try self.emitSinglePayloadDestroy(fb, type_idx, cat);
            },
            .array_with_arc => {
                // Array: destroy each element in reverse order.
                // Swift GenValueWitness.cpp: loop over elements, call element's destroy.
                try self.emitArrayDestroy(fb, type_idx);
            },
            .tuple_with_arc => {
                // Tuple: destroy each non-trivial element in reverse order.
                try self.emitTupleDestroy(fb, type_idx);
            },
        }

        _ = try fb.emitRet(null, Span.zero);
        try builder.endFunc();
    }

    /// Emit element-loop destroy for array types.
    /// Swift: for i in (0..<count).reversed() { element_vwt.destroy(&array[i], metadata) }
    fn emitArrayDestroy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex) !void {
        const info = self.type_reg.get(type_idx);
        const arr = info.array;
        const elem_size = self.type_reg.sizeOf(arr.elem);
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const elem_type_name = self.type_reg.typeName(arr.elem);
        const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{elem_type_name});

        // Destroy elements in reverse order (index = length-1 down to 0)
        var i: usize = arr.length;
        while (i > 0) {
            i -= 1;
            const offset = try fb.emitConstInt(@intCast(i * elem_size), TypeRegistry.I64, Span.zero);
            const elem_ptr = try fb.emitBinary(.add, obj_ptr, offset, TypeRegistry.I64, Span.zero);
            var args = [_]ir.NodeIndex{ elem_ptr, metadata };
            _ = try fb.emitCall(destroy_name, &args, false, TypeRegistry.VOID, Span.zero);
        }
    }

    /// Emit per-element destroy for tuple types.
    fn emitTupleDestroy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex) !void {
        const info = self.type_reg.get(type_idx);
        const tup = info.tuple;
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

        // Compute offsets: each element aligned to 8 bytes
        var current_offset: u32 = 0;
        // Collect offsets first (need reverse order for destroy)
        var offsets: [64]u32 = undefined; // max 64 tuple elements
        const count = @min(tup.element_types.len, 64);
        for (0..count) |ei| {
            offsets[ei] = current_offset;
            const es = self.type_reg.sizeOf(tup.element_types[ei]);
            current_offset += ((es + 7) / 8) * 8;
        }

        // Destroy in reverse order
        var i: usize = count;
        while (i > 0) {
            i -= 1;
            if (self.type_reg.isTrivial(tup.element_types[i])) continue;
            const offset = try fb.emitConstInt(@intCast(offsets[i]), TypeRegistry.I64, Span.zero);
            const elem_ptr = try fb.emitBinary(.add, obj_ptr, offset, TypeRegistry.I64, Span.zero);
            const et_name = self.type_reg.typeName(tup.element_types[i]);
            const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{et_name});
            var args = [_]ir.NodeIndex{ elem_ptr, metadata };
            _ = try fb.emitCall(destroy_name, &args, false, TypeRegistry.VOID, Span.zero);
        }
    }

    /// Emit tag-switch destroy for union types.
    fn emitTagSwitchDestroy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex) !void {
        const info = self.type_reg.get(type_idx);
        const variants = info.union_type.variants;
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const tag = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const payload_ptr = try fb.emitBinary(.add, obj_ptr, eight, TypeRegistry.I64, Span.zero);
        const done_blk = try fb.newBlock("done");

        for (variants, 0..) |variant, vi| {
            if (variant.payload_type == types.invalid_type or self.type_reg.isTrivial(variant.payload_type)) continue;

            const tag_val = try fb.emitConstInt(@intCast(vi), TypeRegistry.I64, Span.zero);
            const is_this_tag = try fb.emitBinary(.eq, tag, tag_val, TypeRegistry.BOOL, Span.zero);
            const destroy_blk = try fb.newBlock("destroy_variant");
            const next_blk = try fb.newBlock("next");
            _ = try fb.emitBranch(is_this_tag, destroy_blk, next_blk, Span.zero);

            fb.setBlock(destroy_blk);
            const vt_name = self.type_reg.typeName(variant.payload_type);
            const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{vt_name});
            var destroy_args = [_]ir.NodeIndex{ payload_ptr, metadata };
            _ = try fb.emitCall(destroy_name, &destroy_args, false, TypeRegistry.VOID, Span.zero);
            _ = try fb.emitJump(done_blk, Span.zero);

            fb.setBlock(next_blk);
        }
        _ = try fb.emitJump(done_blk, Span.zero);
        fb.setBlock(done_blk);
    }

    /// Emit destroy for single-payload types (optional, error_union).
    /// Tag 0 = some/ok (payload is valid), tag != 0 = none/error (no ARC payload).
    fn emitSinglePayloadDestroy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex, cat: TypeCategory) !void {
        const info = self.type_reg.get(type_idx);
        const payload_type = switch (cat) {
            .optional_with_arc => info.optional.elem,
            .error_union_with_arc => info.error_union.elem,
            else => unreachable,
        };
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

        // Load tag from offset 0
        const tag = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
        const is_some = try fb.emitBinary(.eq, tag, zero, TypeRegistry.BOOL, Span.zero);

        const destroy_blk = try fb.newBlock("destroy_payload");
        const done_blk = try fb.newBlock("done");
        _ = try fb.emitBranch(is_some, destroy_blk, done_blk, Span.zero);

        fb.setBlock(destroy_blk);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const payload_ptr = try fb.emitBinary(.add, obj_ptr, eight, TypeRegistry.I64, Span.zero);
        const pt_name = self.type_reg.typeName(payload_type);
        const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{pt_name});
        var destroy_args = [_]ir.NodeIndex{ payload_ptr, metadata };
        _ = try fb.emitCall(destroy_name, &destroy_args, false, TypeRegistry.VOID, Span.zero);
        _ = try fb.emitJump(done_blk, Span.zero);

        fb.setBlock(done_blk);
    }

    /// initializeWithCopy(dest_ptr, src_ptr, metadata) → dest_ptr
    /// Swift ABI: dest_ptr and src_ptr are OpaqueValue* — pointers TO value memory.
    /// Copies the value from *src_ptr into uninitialized *dest_ptr.
    /// POD: memcpy. Managed ptr: memcpy + retain(*src_ptr). Collection: memcpy + retain(buf).
    fn emitInitializeWithCopyWitness(
        self: *VWTGenerator,
        builder: *ir.Builder,
        entry: VWTEntry,
        type_idx: TypeIndex,
        cat: TypeCategory,
    ) !void {
        builder.startFunc(entry.initializeWithCopy_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

        switch (cat) {
            .struct_with_arc => {
                // memcpy whole struct, then call initializeWithCopy for each non-trivial field.
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);

                const info = self.type_reg.get(type_idx);
                const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);

                for (info.struct_type.fields) |field| {
                    if (self.type_reg.isTrivial(field.type_idx)) continue;
                    const offset = try fb.emitConstInt(@intCast(field.offset), TypeRegistry.I64, Span.zero);
                    const field_dest = try fb.emitBinary(.add, dest_ptr, offset, TypeRegistry.I64, Span.zero);
                    const field_src = try fb.emitBinary(.add, src_ptr, offset, TypeRegistry.I64, Span.zero);
                    const ft_name = self.type_reg.typeName(field.type_idx);
                    const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{ft_name});
                    var copy_args = [_]ir.NodeIndex{ field_dest, field_src, metadata };
                    _ = try fb.emitCall(copy_name, &copy_args, false, TypeRegistry.I64, Span.zero);
                }
            },
            .union_with_arc => {
                // memcpy whole union, then retain active payload based on tag.
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);
                try self.emitTagSwitchCopy(fb, type_idx, src_ptr, dest_ptr);
            },
            .optional_with_arc, .error_union_with_arc => {
                // memcpy whole value, then retain payload if tag == 0 (some/ok).
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);
                try self.emitSinglePayloadCopy(fb, type_idx, cat, src_ptr, dest_ptr);
            },
            .array_with_arc => {
                // Array: memcpy whole array, then call initializeWithCopy for each element.
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);
                const info2 = self.type_reg.get(type_idx);
                const arr = info2.array;
                const elem_size = self.type_reg.sizeOf(arr.elem);
                const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);
                const et_name = self.type_reg.typeName(arr.elem);
                const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{et_name});
                for (0..arr.length) |ei| {
                    const offset = try fb.emitConstInt(@intCast(ei * elem_size), TypeRegistry.I64, Span.zero);
                    const elem_dest = try fb.emitBinary(.add, dest_ptr, offset, TypeRegistry.I64, Span.zero);
                    const elem_src = try fb.emitBinary(.add, src_ptr, offset, TypeRegistry.I64, Span.zero);
                    var copy_args = [_]ir.NodeIndex{ elem_dest, elem_src, metadata };
                    _ = try fb.emitCall(copy_name, &copy_args, false, TypeRegistry.I64, Span.zero);
                }
            },
            .tuple_with_arc => {
                // Tuple: memcpy whole tuple, then initializeWithCopy for each non-trivial element.
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);
                const info2 = self.type_reg.get(type_idx);
                const tup = info2.tuple;
                const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);
                var current_offset: u32 = 0;
                for (tup.element_types) |et| {
                    if (!self.type_reg.isTrivial(et)) {
                        const offset = try fb.emitConstInt(@intCast(current_offset), TypeRegistry.I64, Span.zero);
                        const elem_dest = try fb.emitBinary(.add, dest_ptr, offset, TypeRegistry.I64, Span.zero);
                        const elem_src = try fb.emitBinary(.add, src_ptr, offset, TypeRegistry.I64, Span.zero);
                        const et_name = self.type_reg.typeName(et);
                        const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{et_name});
                        var copy_args = [_]ir.NodeIndex{ elem_dest, elem_src, metadata };
                        _ = try fb.emitCall(copy_name, &copy_args, false, TypeRegistry.I64, Span.zero);
                    }
                    const es = self.type_reg.sizeOf(et);
                    current_offset += ((es + 7) / 8) * 8;
                }
            },
            else => {
                // All other types: memcpy + type-specific retain
                const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
                var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
                _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);

                switch (cat) {
                    .managed_ptr => {
                        const ptr_val = try fb.emitPtrLoadValue(src_ptr, TypeRegistry.I64, Span.zero);
                        var args = [_]ir.NodeIndex{ptr_val};
                        _ = try fb.emitCall("retain", &args, false, TypeRegistry.I64, Span.zero);
                    },
                    .collection => {
                        const buf = try fb.emitPtrLoadValue(src_ptr, TypeRegistry.I64, Span.zero);
                        const zero = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
                        const is_non_null = try fb.emitBinary(.ne, buf, zero, TypeRegistry.BOOL, Span.zero);
                        const retain_blk = try fb.newBlock("retain");
                        const done_blk = try fb.newBlock("done");
                        _ = try fb.emitBranch(is_non_null, retain_blk, done_blk, Span.zero);
                        fb.setBlock(retain_blk);
                        var args = [_]ir.NodeIndex{buf};
                        _ = try fb.emitCall("retain", &args, false, TypeRegistry.I64, Span.zero);
                        _ = try fb.emitJump(done_blk, Span.zero);
                        fb.setBlock(done_blk);
                    },
                    else => {}, // POD: nothing to retain
                }
            },
        }

        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }

    /// Emit tag-switch copy (retain) for union types. Called after memcpy.
    fn emitTagSwitchCopy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex, src_ptr: ir.NodeIndex, dest_ptr: ir.NodeIndex) !void {
        const info = self.type_reg.get(type_idx);
        const variants = info.union_type.variants;
        const tag = try fb.emitPtrLoadValue(src_ptr, TypeRegistry.I64, Span.zero);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const payload_src = try fb.emitBinary(.add, src_ptr, eight, TypeRegistry.I64, Span.zero);
        const payload_dest = try fb.emitBinary(.add, dest_ptr, eight, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);
        const done_blk = try fb.newBlock("done");

        for (variants, 0..) |variant, vi| {
            if (variant.payload_type == types.invalid_type or self.type_reg.isTrivial(variant.payload_type)) continue;

            const tag_val = try fb.emitConstInt(@intCast(vi), TypeRegistry.I64, Span.zero);
            const is_this_tag = try fb.emitBinary(.eq, tag, tag_val, TypeRegistry.BOOL, Span.zero);
            const copy_blk = try fb.newBlock("copy_variant");
            const next_blk = try fb.newBlock("next");
            _ = try fb.emitBranch(is_this_tag, copy_blk, next_blk, Span.zero);

            fb.setBlock(copy_blk);
            const vt_name = self.type_reg.typeName(variant.payload_type);
            const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{vt_name});
            var copy_args = [_]ir.NodeIndex{ payload_dest, payload_src, metadata };
            _ = try fb.emitCall(copy_name, &copy_args, false, TypeRegistry.I64, Span.zero);
            _ = try fb.emitJump(done_blk, Span.zero);

            fb.setBlock(next_blk);
        }
        _ = try fb.emitJump(done_blk, Span.zero);
        fb.setBlock(done_blk);
    }

    /// Emit single-payload copy (retain) for optional/error_union. Called after memcpy.
    fn emitSinglePayloadCopy(self: *VWTGenerator, fb: *ir.FuncBuilder, type_idx: TypeIndex, cat: TypeCategory, src_ptr: ir.NodeIndex, dest_ptr: ir.NodeIndex) !void {
        const info = self.type_reg.get(type_idx);
        const payload_type = switch (cat) {
            .optional_with_arc => info.optional.elem,
            .error_union_with_arc => info.error_union.elem,
            else => unreachable,
        };
        const tag = try fb.emitPtrLoadValue(src_ptr, TypeRegistry.I64, Span.zero);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, Span.zero);
        const is_some = try fb.emitBinary(.eq, tag, zero, TypeRegistry.BOOL, Span.zero);
        const copy_blk = try fb.newBlock("copy_payload");
        const done_blk = try fb.newBlock("done");
        _ = try fb.emitBranch(is_some, copy_blk, done_blk, Span.zero);

        fb.setBlock(copy_blk);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const payload_src = try fb.emitBinary(.add, src_ptr, eight, TypeRegistry.I64, Span.zero);
        const payload_dest = try fb.emitBinary(.add, dest_ptr, eight, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);
        const pt_name = self.type_reg.typeName(payload_type);
        const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{pt_name});
        var copy_args = [_]ir.NodeIndex{ payload_dest, payload_src, metadata };
        _ = try fb.emitCall(copy_name, &copy_args, false, TypeRegistry.I64, Span.zero);
        _ = try fb.emitJump(done_blk, Span.zero);

        fb.setBlock(done_blk);
    }

    /// assignWithCopy(dest_ptr, src_ptr, metadata) → dest_ptr
    /// Swift ABI: dest_ptr holds a VALID value that must be destroyed before overwriting.
    /// Naive correct impl: destroy(dest_ptr); initializeWithCopy(dest_ptr, src_ptr).
    /// Swift optimizes for common cases but this is semantically correct.
    fn emitAssignWithCopyWitness(
        self: *VWTGenerator,
        builder: *ir.Builder,
        entry: VWTEntry,
        cat: TypeCategory,
    ) !void {
        _ = cat;
        _ = self;
        builder.startFunc(entry.assignWithCopy_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);

        // destroy(dest_ptr, metadata) — release old value
        var destroy_args = [_]ir.NodeIndex{ dest_ptr, metadata };
        _ = try fb.emitCall(entry.destroy_fn, &destroy_args, false, TypeRegistry.VOID, Span.zero);
        // initializeWithCopy(dest_ptr, src_ptr, metadata) — copy new value with retain
        var copy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, metadata };
        _ = try fb.emitCall(entry.initializeWithCopy_fn, &copy_args, false, TypeRegistry.I64, Span.zero);

        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }

    /// initializeWithTake(dest_ptr, src_ptr, metadata) → dest_ptr
    /// Swift ABI: move src value into uninitialized dest. Source is invalidated.
    /// All Cot types are bitwise-takable: just memcpy, no retain/release.
    /// This is the most efficient transfer — no refcount changes.
    fn emitInitializeWithTakeWitness(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry) !void {
        _ = self;
        builder.startFunc(entry.initializeWithTake_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
        var args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
        _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, Span.zero);

        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }

    /// assignWithTake(dest_ptr, src_ptr, metadata) → dest_ptr
    /// Swift ABI: destroy old dest value, then memcpy from src (take=move, no retain).
    fn emitAssignWithTakeWitness(
        self: *VWTGenerator,
        builder: *ir.Builder,
        entry: VWTEntry,
        cat: TypeCategory,
    ) !void {
        _ = self;
        builder.startFunc(entry.assignWithTake_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);

        if (cat != .pod) {
            var destroy_args = [_]ir.NodeIndex{ dest_ptr, metadata };
            _ = try fb.emitCall(entry.destroy_fn, &destroy_args, false, TypeRegistry.VOID, Span.zero);
        }

        // memcpy(dest_ptr, src_ptr, size) — take = move, no retain
        const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
        var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
        _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);

        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }

    // ========================================================================
    // Enum-specific witnesses (only emitted for union types)
    // ========================================================================

    /// getEnumTag(object_ptr, metadata) → tag (i64)
    /// Swift ABI: reads the discriminator tag from the union.
    /// Cot union layout: tag at offset 0 (8 bytes), payload at offset 8.
    fn emitGetEnumTagWitness(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry) !void {
        _ = self;
        const name = entry.getEnumTag_fn orelse return;
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("object_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        // Load tag from offset 0 of the union value
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const tag = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(tag, Span.zero);
        try builder.endFunc();
    }

    /// destructiveProjectEnumData(object_ptr, metadata) → payload_ptr (i64)
    /// Swift ABI: returns a pointer to the payload area, destructively (tag may be overwritten).
    /// Cot union layout: payload starts at offset 8.
    fn emitDestructiveProjectEnumDataWitness(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry) !void {
        _ = self;
        const name = entry.destructiveProjectEnumData_fn orelse return;
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("object_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        // payload_ptr = object_ptr + 8
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
        const payload_ptr = try fb.emitBinary(.add, obj_ptr, eight, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(payload_ptr, Span.zero);
        try builder.endFunc();
    }

    /// destructiveInjectEnumTag(object_ptr, tag, metadata) → void
    /// Swift ABI: writes a tag into the union, potentially overwriting payload.
    /// Cot union layout: tag at offset 0 (8 bytes).
    fn emitDestructiveInjectEnumTagWitness(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry) !void {
        _ = self;
        const name = entry.destructiveInjectEnumTag_fn orelse return;
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("object_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("tag", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        // Store tag at offset 0 of the union value
        const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const tag = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(obj_ptr, tag, Span.zero);
        _ = try fb.emitRet(null, Span.zero);
        try builder.endFunc();
    }

    // ========================================================================
    // Required VWT fields — stubs for complete Swift ABI compliance
    // ========================================================================

    /// getEnumTagSinglePayload(value_ptr, emptyCases, metadata) → unsigned
    /// Swift ABI (offset 0x30): For single-payload enums (e.g., Optional).
    /// Returns 0 if value holds the payload, or (1..emptyCases) for which empty case.
    /// Cot: explicit tag at offset 0. Tag 0 = payload present, tag > 0 = empty case index.
    fn emitGetEnumTagSinglePayloadWitness(self: *VWTGenerator, builder: *ir.Builder, type_name: []const u8) !void {
        const name = try std.fmt.allocPrint(self.allocator, "__vwt_getEnumTagSinglePayload_{s}", .{type_name});
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("value_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("emptyCases", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        // Read tag from offset 0. Tag 0 = payload (return 0), tag N = empty case N.
        const val_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const tag = try fb.emitPtrLoadValue(val_ptr, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(tag, Span.zero);
        try builder.endFunc();
    }

    /// storeEnumTagSinglePayload(value_ptr, whichCase, emptyCases, metadata) → void
    /// Swift ABI (offset 0x38): Store tag for single-payload enum.
    /// whichCase 0 = store payload tag, whichCase > 0 = store empty case tag.
    /// Cot: write whichCase directly to tag field at offset 0.
    fn emitStoreEnumTagSinglePayloadWitness(self: *VWTGenerator, builder: *ir.Builder, type_name: []const u8) !void {
        const name = try std.fmt.allocPrint(self.allocator, "__vwt_storeEnumTagSinglePayload_{s}", .{type_name});
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("value_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("whichCase", TypeRegistry.I64, 8);
        _ = try fb.addParam("emptyCases", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        // Write whichCase to tag field at offset 0.
        const val_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const which = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        _ = try fb.emitPtrStoreValue(val_ptr, which, Span.zero);
        _ = try fb.emitRet(null, Span.zero);
        try builder.endFunc();
    }

    /// initializeBufferWithCopyOfBuffer(dest_buf, src_buf, metadata) → dest_buf
    /// Swift ABI (offset 0x00): Copy value from one existential buffer to another.
    /// If inline: memcpy(dest, src, 24). If out-of-line: alloc box, copy, store ptr.
    /// Cot: all current types are inline (≤24B and bitwise-takable), so just memcpy.
    /// When existential types are added, this needs the full inline/out-of-line logic.
    fn emitInitializeBufferWithCopyOfBufferWitness(self: *VWTGenerator, builder: *ir.Builder, entry: VWTEntry, type_name: []const u8) !void {
        const name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeBufferWithCopyOfBuffer_{s}", .{type_name});
        builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_buf", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_buf", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

        if (entry.flags.isInlineStorage()) {
            // Inline: memcpy the buffer (ValueBuffer = 24 bytes on 64-bit)
            const buf_size = try fb.emitConstInt(24, TypeRegistry.I64, Span.zero);
            var args = [_]ir.NodeIndex{ dest, src, buf_size };
            _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, Span.zero);
        } else {
            // Out-of-line: allocate box, copy value into box, store box ptr in dest[0].
            // For now: just memcpy the pointer (box ptr is at offset 0 of buffer).
            const ptr_size = try fb.emitConstInt(8, TypeRegistry.I64, Span.zero);
            var args = [_]ir.NodeIndex{ dest, src, ptr_size };
            _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, Span.zero);
            // TODO: When existential types are added, allocate new box and deep-copy.
        }

        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "VWTGenerator computeEntry for POD type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);

    // i64 is POD: size=8, stride=8, alignment=8, no ARC
    const entry = try gen.computeEntry(TypeRegistry.I64, "i64");
    try std.testing.expectEqual(@as(u64, 8), entry.size);
    try std.testing.expectEqual(@as(u64, 8), entry.stride);
    try std.testing.expect(entry.flags.isPOD());
    try std.testing.expect(entry.flags.isInlineStorage());
    try std.testing.expect(entry.flags.isBitwiseTakable());
    try std.testing.expect(!entry.has_enum_witnesses);
    try std.testing.expectEqual(@as(u32, 0), entry.extra_inhabitant_count);
}

test "VWTGenerator computeEntry for pointer" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);

    // Raw pointer (*i64): size=8, POD (raw, not managed), has extra inhabitant (null)
    const ptr_type = try reg.makePointer(TypeRegistry.I64);
    const entry = try gen.computeEntry(ptr_type, "ptr_i64");
    try std.testing.expectEqual(@as(u64, 8), entry.size);
    try std.testing.expect(entry.flags.isPOD()); // raw pointer is trivial
    try std.testing.expect(entry.flags.isBitwiseTakable());
    try std.testing.expectEqual(@as(u32, 1), entry.extra_inhabitant_count);
}

test "VWTGenerator computeEntry for optional" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);

    const opt_type = try reg.makeOptional(TypeRegistry.I64);
    const entry = try gen.computeEntry(opt_type, "opt_i64");
    // Optional<i64>: tag(8) + payload(8) = 16 bytes
    try std.testing.expectEqual(@as(u64, 16), entry.size);
    try std.testing.expect(entry.has_enum_witnesses);
    try std.testing.expect(entry.flags.hasEnumWitnesses());
}

test "VWTGenerator emitWitnesses for POD type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    const entry = try gen.emitWitnesses(&builder, TypeRegistry.I64, "i64");

    // 5 base + 1 buffer + 1 metadata init = 7
    try std.testing.expectEqual(@as(usize, 7), builder.funcs.items.len);

    // Verify function names match entry
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[0].name, entry.destroy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[1].name, entry.initializeWithCopy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[2].name, entry.assignWithCopy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[3].name, entry.initializeWithTake_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[4].name, entry.assignWithTake_fn));

    // Dedup: emitting again should not add more functions
    _ = try gen.emitWitnesses(&builder, TypeRegistry.I64, "i64");
    try std.testing.expectEqual(@as(usize, 7), builder.funcs.items.len);
}

test "VWTGenerator emitWitnesses for collection type" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    const list_type = try reg.makeList(TypeRegistry.I64);
    const entry = try gen.emitWitnesses(&builder, list_type, "List_i64");

    // 7 functions (5 base + 1 buffer + 1 metadata init)
    try std.testing.expectEqual(@as(usize, 7), builder.funcs.items.len);
    // Collection is NOT POD
    try std.testing.expect(!entry.flags.isPOD());
    // destroy should have more than just a ret (has null check + release)
    try std.testing.expect(builder.funcs.items[0].blocks.len > 1);
}

test "VWTGenerator emitWitnesses for struct with ARC field" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    // Create a struct: Container { x: i64, items: List(i64) }
    const list_type = try reg.makeList(TypeRegistry.I64);
    const fields = try alloc.dupe(types.StructField, &[_]types.StructField{
        .{ .name = "x", .type_idx = TypeRegistry.I64, .offset = 0 },
        .{ .name = "items", .type_idx = list_type, .offset = 8 },
    });
    const container_type = try reg.add(.{ .struct_type = .{
        .name = "Container",
        .fields = fields,
        .size = 32, // 8 (i64) + 24 (List)
        .alignment = 8,
    } });

    const entry = try gen.emitWitnesses(&builder, container_type, "Container");

    // list: 7 functions + Container: 7 functions = 14 total
    try std.testing.expectEqual(@as(usize, 14), builder.funcs.items.len);

    // Container is NOT POD (has List field)
    try std.testing.expect(!entry.flags.isPOD());

    // Container witnesses start at index 7 (after list's 7 functions)
    const container_destroy = builder.funcs.items[7];
    try std.testing.expect(std.mem.eql(u8, container_destroy.name, "__vwt_destroy_Container"));

    const container_copy = builder.funcs.items[8];
    try std.testing.expect(std.mem.eql(u8, container_copy.name, "__vwt_initializeWithCopy_Container"));

    // Dedup: emitting again should not add more functions
    _ = try gen.emitWitnesses(&builder, container_type, "Container");
    try std.testing.expectEqual(@as(usize, 14), builder.funcs.items.len);
}

test "VWTGenerator emitWitnesses for struct with managed pointer field" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    // Create a struct type to get a managed pointer to it
    const inner_fields = try alloc.dupe(types.StructField, &[_]types.StructField{
        .{ .name = "val", .type_idx = TypeRegistry.I64, .offset = 0 },
    });
    const inner_type = try reg.add(.{ .struct_type = .{
        .name = "Inner",
        .fields = inner_fields,
        .size = 8,
        .alignment = 8,
    } });
    // makePointer to a struct creates a managed pointer
    const ptr_type = try reg.makePointer(inner_type);

    // Create: Holder { data: *Inner }
    const fields = try alloc.dupe(types.StructField, &[_]types.StructField{
        .{ .name = "data", .type_idx = ptr_type, .offset = 0 },
    });
    const holder_type = try reg.add(.{ .struct_type = .{
        .name = "Holder",
        .fields = fields,
        .size = 8,
        .alignment = 8,
    } });

    const entry = try gen.emitWitnesses(&builder, holder_type, "Holder");

    // Sub-type (managed pointer): 7 + Holder: 7 = 14 functions
    try std.testing.expectEqual(@as(usize, 14), builder.funcs.items.len);
    try std.testing.expect(!entry.flags.isPOD());
}

test "VWTGenerator emitWitnesses for POD struct (all trivial fields)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    // Create: Point { x: i64, y: i64 } — all trivial
    const fields = try alloc.dupe(types.StructField, &[_]types.StructField{
        .{ .name = "x", .type_idx = TypeRegistry.I64, .offset = 0 },
        .{ .name = "y", .type_idx = TypeRegistry.I64, .offset = 8 },
    });
    const point_type = try reg.add(.{ .struct_type = .{
        .name = "Point",
        .fields = fields,
        .size = 16,
        .alignment = 8,
    } });

    const entry = try gen.emitWitnesses(&builder, point_type, "Point");

    // POD struct: 7 functions (5 base + 1 buffer + 1 init)
    try std.testing.expectEqual(@as(usize, 7), builder.funcs.items.len);
    try std.testing.expect(entry.flags.isPOD());
}

test "VWTGenerator emitWitnesses for union with ARC variant" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    // Create a managed pointer type (pointer to struct = managed)
    const inner_fields = try alloc.dupe(types.StructField, &[_]types.StructField{
        .{ .name = "val", .type_idx = TypeRegistry.I64, .offset = 0 },
    });
    const inner_type = try reg.add(.{ .struct_type = .{
        .name = "Node",
        .fields = inner_fields,
        .size = 8,
        .alignment = 8,
    } });
    const managed_ptr = try reg.makePointer(inner_type);

    // Union: Result { ok: *Node, err: i64 }
    const variants = try alloc.dupe(types.UnionVariant, &[_]types.UnionVariant{
        .{ .name = "ok", .payload_type = managed_ptr },
        .{ .name = "err", .payload_type = TypeRegistry.I64 },
    });
    const union_type = try reg.add(.{ .union_type = .{
        .name = "Result",
        .variants = variants,
        .tag_type = TypeRegistry.I64,
    } });

    const entry = try gen.emitWitnesses(&builder, union_type, "Result");

    // pointer: 7 + Result: 5 base + 3 enum + 1 buffer + 1 init = 10 → total 17
    try std.testing.expectEqual(@as(usize, 17), builder.funcs.items.len);
    try std.testing.expect(!entry.flags.isPOD());
    try std.testing.expect(entry.has_enum_witnesses);

    // Check enum witness function names
    try std.testing.expect(entry.getEnumTag_fn != null);
    try std.testing.expect(entry.destructiveProjectEnumData_fn != null);
    try std.testing.expect(entry.destructiveInjectEnumTag_fn != null);

    // Verify the destroy witness is index 7 (after pointer's 7 functions)
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[7].name, "__vwt_destroy_Result"));
}

test "VWTGenerator emitWitnesses for POD union (all trivial variants)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var reg = try TypeRegistry.init(alloc);

    var gen = VWTGenerator.init(alloc, &reg);
    var builder = ir.Builder.init(alloc, &reg);

    // Union: IntOrFloat { i: i64, f: f64 } — all trivial payloads
    const variants = try alloc.dupe(types.UnionVariant, &[_]types.UnionVariant{
        .{ .name = "i", .payload_type = TypeRegistry.I64 },
        .{ .name = "f", .payload_type = TypeRegistry.F64 },
    });
    const union_type = try reg.add(.{ .union_type = .{
        .name = "IntOrFloat",
        .variants = variants,
        .tag_type = TypeRegistry.I64,
    } });

    const entry = try gen.emitWitnesses(&builder, union_type, "IntOrFloat");

    // POD union: 5 base + 3 enum + 1 buffer + 1 init = 10 functions
    try std.testing.expectEqual(@as(usize, 10), builder.funcs.items.len);
    try std.testing.expect(entry.flags.isPOD());
    try std.testing.expect(entry.has_enum_witnesses);
}
