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
const Span = @import("ast.zig").Span;

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
