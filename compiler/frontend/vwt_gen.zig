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

    /// Emit all VWT witness functions for a concrete type into the IR builder.
    /// Skips types that already have witnesses emitted (dedup via `emitted` set).
    pub fn emitWitnesses(self: *VWTGenerator, builder: *ir.Builder, type_idx: TypeIndex, type_name: []const u8) !VWTEntry {
        // Dedup: skip if already emitted
        if (self.emitted.contains(type_name)) {
            return self.entries.get(type_name) orelse return error.MissingVWTEntry;
        }

        const entry = try self.computeEntry(type_idx, type_name);
        const info = self.type_reg.get(type_idx);
        const is_pod = self.type_reg.isTrivial(type_idx);

        // Determine type category for witness generation
        const is_managed_ptr = (info == .pointer and info.pointer.managed);
        const is_collection = (info == .list or info == .map) or
            (info == .struct_type and types.TypeRegistry.isMonomorphizedCollection(info.struct_type.name));

        // --- destroy ---
        try self.emitDestroyWitness(builder, entry, type_idx, is_pod, is_managed_ptr, is_collection);

        // --- initializeWithCopy ---
        try self.emitInitializeWithCopyWitness(builder, entry, type_idx, is_pod, is_managed_ptr, is_collection);

        // --- assignWithCopy ---
        try self.emitAssignWithCopyWitness(builder, entry, type_idx, is_pod, is_managed_ptr, is_collection);

        // --- initializeWithTake (always memcpy — all Cot types are bitwise-takable) ---
        try self.emitInitializeWithTakeWitness(builder, entry);

        // --- assignWithTake ---
        try self.emitAssignWithTakeWitness(builder, entry, type_idx, is_pod, is_managed_ptr, is_collection);

        try self.emitted.put(type_name, {});
        try self.entries.put(type_name, entry);
        return entry;
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
        is_pod: bool,
        is_managed_ptr: bool,
        is_collection: bool,
    ) !void {
        _ = self;
        builder.startFunc(entry.destroy_fn, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("object_ptr", TypeRegistry.I64, 8); // pointer TO value
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        if (is_pod) {
            // POD: noop — nothing to release
        } else if (is_managed_ptr) {
            // Managed pointer: load the pointer value, then release it
            // object_ptr points to memory containing the managed pointer
            const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
            const ptr_val = try fb.emitPtrLoadValue(obj_ptr, TypeRegistry.I64, Span.zero);
            var args = [_]ir.NodeIndex{ptr_val};
            _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, Span.zero);
        } else if (is_collection) {
            // Collection: object_ptr points to the collection struct.
            // Load buf field (offset 0), if non-null release(buf).
            const obj_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
            // Load buf directly from offset 0 of the pointed-to struct
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
        } else {
            // Struct/union: TODO — field-by-field destroy via sub-VWTs
            // For now: noop (will be filled in when struct/union witness gen is implemented)
        }
        _ = type_idx;

        _ = try fb.emitRet(null, Span.zero);
        try builder.endFunc();
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
        is_pod: bool,
        is_managed_ptr: bool,
        is_collection: bool,
    ) !void {
        _ = self;
        _ = type_idx;
        builder.startFunc(entry.initializeWithCopy_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8); // pointer TO dest value
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8); // pointer TO src value
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);

        // Step 1: memcpy(dest_ptr, src_ptr, size) — bitwise copy of the value
        const size_val = try fb.emitConstInt(@intCast(entry.size), TypeRegistry.I64, Span.zero);
        var memcpy_args = [_]ir.NodeIndex{ dest_ptr, src_ptr, size_val };
        _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, Span.zero);

        // Step 2: retain ARC resources in the copied value
        if (!is_pod) {
            if (is_managed_ptr) {
                // The value at *src_ptr is a managed pointer. Load it and retain.
                const ptr_val = try fb.emitPtrLoadValue(src_ptr, TypeRegistry.I64, Span.zero);
                var args = [_]ir.NodeIndex{ptr_val};
                _ = try fb.emitCall("retain", &args, false, TypeRegistry.I64, Span.zero);
            } else if (is_collection) {
                // The value at *src_ptr is a collection struct. Load buf (offset 0), retain if non-null.
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
            } else {
                // Struct/union: TODO — field-by-field copy via sub-VWTs
            }
        }

        // Return dest_ptr
        const ret = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        _ = try fb.emitRet(ret, Span.zero);
        try builder.endFunc();
    }

    /// assignWithCopy(dest_ptr, src_ptr, metadata) → dest_ptr
    /// Swift ABI: dest_ptr holds a VALID value that must be destroyed before overwriting.
    /// Naive correct impl: destroy(dest_ptr); initializeWithCopy(dest_ptr, src_ptr).
    /// Swift optimizes for common cases but this is semantically correct.
    fn emitAssignWithCopyWitness(
        self: *VWTGenerator,
        builder: *ir.Builder,
        entry: VWTEntry,
        type_idx: TypeIndex,
        is_pod: bool,
        is_managed_ptr: bool,
        is_collection: bool,
    ) !void {
        _ = type_idx;
        _ = is_collection;
        _ = is_managed_ptr;
        _ = is_pod;
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
        type_idx: TypeIndex,
        is_pod: bool,
        is_managed_ptr: bool,
        is_collection: bool,
    ) !void {
        _ = type_idx;
        _ = is_collection;
        _ = is_managed_ptr;
        _ = self;
        builder.startFunc(entry.assignWithTake_fn, TypeRegistry.VOID, TypeRegistry.I64, Span.zero);
        const fb = builder.func() orelse return;
        _ = try fb.addParam("dest_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("src_ptr", TypeRegistry.I64, 8);
        _ = try fb.addParam("metadata", TypeRegistry.I64, 8);

        const dest_ptr = try fb.emitLoadLocal(0, TypeRegistry.I64, Span.zero);
        const src_ptr = try fb.emitLoadLocal(1, TypeRegistry.I64, Span.zero);
        const metadata = try fb.emitLoadLocal(2, TypeRegistry.I64, Span.zero);

        if (!is_pod) {
            // destroy(dest_ptr, metadata) — release old value at dest
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

    // Should have emitted 5 functions: destroy, initializeWithCopy, assignWithCopy,
    // initializeWithTake, assignWithTake
    try std.testing.expectEqual(@as(usize, 5), builder.funcs.items.len);

    // Verify function names match entry
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[0].name, entry.destroy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[1].name, entry.initializeWithCopy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[2].name, entry.assignWithCopy_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[3].name, entry.initializeWithTake_fn));
    try std.testing.expect(std.mem.eql(u8, builder.funcs.items[4].name, entry.assignWithTake_fn));

    // Dedup: emitting again should not add more functions
    _ = try gen.emitWitnesses(&builder, TypeRegistry.I64, "i64");
    try std.testing.expectEqual(@as(usize, 5), builder.funcs.items.len);
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

    // 5 witness functions
    try std.testing.expectEqual(@as(usize, 5), builder.funcs.items.len);
    // Collection is NOT POD
    try std.testing.expect(!entry.flags.isPOD());
    // destroy should have more than just a ret (has null check + release)
    try std.testing.expect(builder.funcs.items[0].blocks.len > 1);
}
