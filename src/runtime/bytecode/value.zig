//! NaN-boxed Value Type
//!
//! Compact 8-byte representation for all runtime values using NaN-boxing.
//! Reduces memory usage by 50% compared to tagged unions and allows values
//! to fit in CPU registers.
//!
//! ## Encoding Scheme
//!
//! IEEE 754 doubles use 64 bits. NaN values have exponent bits all set to 1
//! and at least one mantissa bit set. We use quiet NaN (qNaN) patterns to
//! encode non-float values:
//!
//! ```
//! Real double:     Any bit pattern where (bits >> 52) != 0x7FF or 0xFFF
//! Tagged values:   Quiet NaN with type info in bits 48-51
//! ```

const std = @import("std");
const arc = @import("arc.zig");

// ============================================================================
// Value Tag Enum (for switch statements)
// ============================================================================

/// Value type discriminator - use with value.tag() for switch statements
pub const Tag = enum {
    null_val,
    boolean,
    integer,
    decimal,
    fixed_string,
    string,
    record_ref,
    handle,
    object, // Extension-defined types (Map, Set, etc.)

    /// Get type name for debugging
    pub fn name(self: Tag) []const u8 {
        return switch (self) {
            .null_val => "null",
            .boolean => "boolean",
            .integer => "integer",
            .decimal => "decimal",
            .fixed_string => "alpha",
            .string => "string",
            .record_ref => "record",
            .handle => "handle",
            .object => "object",
        };
    }
};

// ============================================================================
// Boxed Types (heap allocated for values that don't fit inline)
// ============================================================================

/// Heap-allocated decimal (needs precision byte)
pub const Decimal = struct {
    value: i64,
    precision: u8,
};

/// Heap-allocated large integer
pub const BoxedInt = struct {
    value: i64,
};

/// Record instance
pub const Record = struct {
    type_id: u16,
    data: []u8,
};

/// String with length (for NaN-boxed strings)
pub const StringRef = struct {
    ptr: [*]const u8,
    len: usize,
};

/// Mutable fixed-length string
pub const FixedStringRef = struct {
    ptr: [*]u8,
    len: usize,
};

/// Forward declare OrderedMap (actual type imported when needed)
pub const OrderedMap = @import("ordered_map.zig").OrderedMap;

// ============================================================================
// NaN-boxed Value
// ============================================================================

/// NaN-boxed runtime value - exactly 8 bytes
pub const Value = extern struct {
    bits: u64,

    const Self = @This();

    // ========================================================================
    // NaN-boxing constants
    // ========================================================================

    /// Quiet NaN base pattern
    const QNAN: u64 = 0x7FFC_0000_0000_0000;

    /// Mask for the payload (low 48 bits)
    const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

    // Tag patterns (bits 46+ encode type, bits 0-45 encode payload for pointers)
    const TAG_NULL: u64 = 0x7FFC_0000_0000_0000;
    const TAG_FALSE: u64 = 0x7FFC_0000_0000_0001;
    const TAG_TRUE: u64 = 0x7FFC_0000_0000_0002;
    // Small integers: 0x7FFD + sign-extended 47-bit value
    const TAG_SMALL_INT: u64 = 0x7FFD_0000_0000_0000;
    // Pointers: 0x7FFE/F + 46-bit pointer (bits 46+ for type, bits 0-45 for pointer)
    const TAG_STRING: u64 = 0x7FFE_0000_0000_0000;
    const TAG_FIXED_STR: u64 = 0x7FFE_8000_0000_0000;
    const TAG_RECORD: u64 = 0x7FFF_0000_0000_0000;
    const TAG_HANDLE: u64 = 0x7FFF_4000_0000_0000;
    const TAG_DECIMAL: u64 = 0x7FFF_8000_0000_0000;
    const TAG_BOXED_INT: u64 = 0x7FFF_C000_0000_0000;

    // Extension objects: 0x7FFE_4 prefix + type_id in bits 40-45 + pointer in bits 0-39
    // This allows 64 extension types (6 bits) with 1TB address space (40 bits)
    const TAG_OBJECT: u64 = 0x7FFE_4000_0000_0000;
    const TAG_OBJECT_MASK: u64 = 0xFFFF_C000_0000_0000; // Bits 46-63 for tag check
    const OBJ_TYPE_SHIFT: u6 = 40;
    const OBJ_TYPE_MASK: u64 = 0x0000_3F00_0000_0000; // Bits 40-45 for type_id (6 bits)
    /// Object pointer mask (40 bits) - public for ARC module
    pub const OBJ_PTR_MASK: u64 = 0x0000_00FF_FFFF_FFFF; // Bits 0-39 for pointer (40 bits)

    /// Pointer payload mask (low 46 bits for pointers) - public for ARC module
    pub const PTR_MASK: u64 = 0x0000_3FFF_FFFF_FFFF;

    /// Small integer max/min (47-bit signed)
    const SMALL_INT_MAX: i64 = 0x3FFF_FFFF_FFFF; // 2^46 - 1
    const SMALL_INT_MIN: i64 = -0x4000_0000_0000; // -2^46

    // ========================================================================
    // Constructors
    // ========================================================================

    /// Null value
    pub const null_val = Self{ .bits = TAG_NULL };

    /// Boolean true
    pub const true_val = Self{ .bits = TAG_TRUE };

    /// Boolean false
    pub const false_val = Self{ .bits = TAG_FALSE };

    /// Create a null value
    pub fn initNull() Self {
        return null_val;
    }

    /// Create a boolean value
    pub fn initBool(b: bool) Self {
        return if (b) true_val else false_val;
    }

    /// Create an integer value (inline if small, panics if too large)
    pub fn initInt(i: i64) Self {
        if (i >= SMALL_INT_MIN and i <= SMALL_INT_MAX) {
            // Fits inline - encode as small int
            const payload: u48 = @bitCast(@as(i48, @intCast(i)));
            return .{ .bits = TAG_SMALL_INT | @as(u64, payload) };
        } else {
            @panic("Integer too large for inline storage - use initBoxedInt");
        }
    }

    /// Create an integer, boxing if necessary
    pub fn initIntMaybeBoxed(allocator: std.mem.Allocator, i: i64) !Self {
        if (i >= SMALL_INT_MIN and i <= SMALL_INT_MAX) {
            return initInt(i);
        } else {
            return initBoxedInt(allocator, i);
        }
    }

    /// Create a boxed integer (heap allocated with ARC header)
    pub fn initBoxedInt(allocator: std.mem.Allocator, i: i64) !Self {
        const boxed = try arc.create(allocator, BoxedInt);
        boxed.* = .{ .value = i };
        return .{ .bits = TAG_BOXED_INT | ptrToPayload(boxed) };
    }

    /// Create a decimal value (always boxed with ARC header)
    pub fn initDecimal(allocator: std.mem.Allocator, value: i64, precision: u8) !Self {
        const boxed = try arc.create(allocator, Decimal);
        boxed.* = .{ .value = value, .precision = precision };
        return .{ .bits = TAG_DECIMAL | ptrToPayload(boxed) };
    }

    /// Create a decimal from inline struct (convenience for migration)
    pub fn initDecimalInline(allocator: std.mem.Allocator, d: struct { value: i64, precision: u8 }) !Self {
        return initDecimal(allocator, d.value, d.precision);
    }

    /// Create an immutable string reference (with ARC header)
    pub fn initString(allocator: std.mem.Allocator, s: []const u8) !Self {
        const ref = try arc.create(allocator, StringRef);
        ref.* = .{ .ptr = s.ptr, .len = s.len };
        return .{ .bits = TAG_STRING | ptrToPayload(ref) };
    }

    /// Create a mutable fixed-length string reference (with ARC header)
    pub fn initFixedString(allocator: std.mem.Allocator, s: []u8) !Self {
        const ref = try arc.create(allocator, FixedStringRef);
        ref.* = .{ .ptr = s.ptr, .len = s.len };
        return .{ .bits = TAG_FIXED_STR | ptrToPayload(ref) };
    }

    /// Create a record reference
    pub fn initRecord(r: *Record) Self {
        return .{ .bits = TAG_RECORD | ptrToPayload(r) };
    }

    /// Create a handle value
    pub fn initHandle(h: usize) Self {
        std.debug.assert(h <= PTR_MASK);
        return .{ .bits = TAG_HANDLE | @as(u64, @intCast(h)) };
    }

    /// Create an extension object value with type_id
    /// type_id: Extension-assigned type ID (16-63, 0-15 reserved)
    /// ptr: Pointer to heap-allocated object
    pub fn initObject(type_id: u16, ptr: *anyopaque) Self {
        std.debug.assert(type_id < 64); // 6 bits max
        const addr = @intFromPtr(ptr);
        std.debug.assert(addr <= OBJ_PTR_MASK); // Must fit in 40 bits
        const type_bits = @as(u64, type_id) << OBJ_TYPE_SHIFT;
        return .{ .bits = TAG_OBJECT | type_bits | addr };
    }

    /// Create a map value (convenience wrapper for OrderedMap)
    /// Uses type_id 16 (first extension type) for backward compatibility
    pub fn initMap(m: *OrderedMap) Self {
        return initObject(16, m);
    }

    // ========================================================================
    // Type checking and tagging
    // ========================================================================

    /// Get the type tag for switch statements
    pub fn tag(self: Self) Tag {
        if (self.bits == TAG_NULL) return .null_val;
        if (self.bits == TAG_TRUE or self.bits == TAG_FALSE) return .boolean;

        const upper = self.bits & 0xFFFF_C000_0000_0000;
        if (upper == TAG_SMALL_INT or upper == TAG_BOXED_INT) return .integer;
        // Check OBJECT before STRING since OBJECT uses more specific mask
        // and TAG_OBJECT (0x7FFE_4000) would otherwise match TAG_STRING's mask
        if ((self.bits & TAG_OBJECT_MASK) == TAG_OBJECT) return .object;
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_STRING) return .string;
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_FIXED_STR) return .fixed_string;
        if ((self.bits & 0xFFFF_C000_0000_0000) == TAG_RECORD) return .record_ref;
        if ((self.bits & 0xFFFF_F000_0000_0000) == TAG_HANDLE) return .handle;
        if ((self.bits & 0xFFFF_C000_0000_0000) == TAG_DECIMAL) return .decimal;

        return .null_val; // Fallback
    }

    pub fn isNull(self: Self) bool {
        return self.bits == TAG_NULL;
    }

    pub fn isBool(self: Self) bool {
        return self.bits == TAG_TRUE or self.bits == TAG_FALSE;
    }

    pub fn isInt(self: Self) bool {
        const upper = self.bits & 0xFFFF_C000_0000_0000;
        return upper == TAG_SMALL_INT or upper == TAG_BOXED_INT;
    }

    pub fn isDecimal(self: Self) bool {
        return (self.bits & 0xFFFF_C000_0000_0000) == TAG_DECIMAL;
    }

    pub fn isString(self: Self) bool {
        return (self.bits & 0xFFFF_8000_0000_0000) == TAG_STRING;
    }

    pub fn isFixedString(self: Self) bool {
        return (self.bits & 0xFFFF_8000_0000_0000) == TAG_FIXED_STR;
    }

    pub fn isRecord(self: Self) bool {
        return (self.bits & 0xFFFF_C000_0000_0000) == TAG_RECORD;
    }

    pub fn isHandle(self: Self) bool {
        return (self.bits & 0xFFFF_F000_0000_0000) == TAG_HANDLE;
    }

    /// Check if this is an extension object
    pub fn isObject(self: Self) bool {
        return (self.bits & TAG_OBJECT_MASK) == TAG_OBJECT;
    }

    /// Check if this is a map (object with type_id 16)
    /// Kept for backward compatibility
    pub fn isMap(self: Self) bool {
        if (!self.isObject()) return false;
        return self.objectTypeId() == 16;
    }

    /// Get the type_id of an object value
    pub fn objectTypeId(self: Self) ?u16 {
        if (!self.isObject()) return null;
        return @truncate((self.bits & OBJ_TYPE_MASK) >> OBJ_TYPE_SHIFT);
    }

    /// Get the pointer from an object value
    pub fn objectPtr(self: Self, comptime T: type) ?*T {
        if (!self.isObject()) return null;
        const addr = self.bits & OBJ_PTR_MASK;
        return @ptrFromInt(addr);
    }

    // ========================================================================
    // Value extraction (typed accessors)
    // ========================================================================

    /// Get boolean value
    pub fn asBool(self: Self) bool {
        return self.bits == TAG_TRUE;
    }

    /// Get integer value
    pub fn asInt(self: Self) i64 {
        const upper = self.bits & 0xFFFF_C000_0000_0000;
        if (upper == TAG_SMALL_INT) {
            // Extract 46-bit signed integer and sign-extend
            const payload: u48 = @truncate(self.bits);
            return @as(i48, @bitCast(payload));
        } else if (upper == TAG_BOXED_INT) {
            const boxed = payloadToPtr(*BoxedInt, self.bits);
            return boxed.value;
        }
        return 0;
    }

    /// Get decimal value
    pub fn asDecimal(self: Self) ?Decimal {
        if (!self.isDecimal()) return null;
        const dec = payloadToPtr(*Decimal, self.bits);
        return dec.*;
    }

    /// Get immutable string slice
    pub fn asString(self: Self) []const u8 {
        if (self.isString()) {
            const ref = payloadToPtr(*StringRef, self.bits);
            return ref.ptr[0..ref.len];
        } else if (self.isFixedString()) {
            const ref = payloadToPtr(*FixedStringRef, self.bits);
            return ref.ptr[0..ref.len];
        }
        return "";
    }

    /// Get mutable fixed string slice
    pub fn asFixedString(self: Self) ?[]u8 {
        if (!self.isFixedString()) return null;
        const ref = payloadToPtr(*FixedStringRef, self.bits);
        return ref.ptr[0..ref.len];
    }

    /// Get record reference
    pub fn asRecord(self: Self) ?*Record {
        if (!self.isRecord()) return null;
        return payloadToPtr(*Record, self.bits);
    }

    /// Get handle value
    pub fn asHandle(self: Self) ?usize {
        if (!self.isHandle()) return null;
        return @intCast(self.bits & PTR_MASK);
    }

    /// Get map pointer (for backward compatibility)
    pub fn asMap(self: Self) ?*OrderedMap {
        if (!self.isMap()) return null;
        return self.objectPtr(OrderedMap);
    }

    // ========================================================================
    // Conversion methods (compatibility with old API)
    // ========================================================================

    /// Convert to integer
    pub fn toInt(self: Self) i64 {
        return switch (self.tag()) {
            .integer => self.asInt(),
            .decimal => if (self.asDecimal()) |d|
                @divTrunc(d.value, std.math.pow(i64, 10, d.precision))
            else
                0,
            .boolean => if (self.asBool()) @as(i64, 1) else 0,
            .fixed_string, .string => std.fmt.parseInt(i64, std.mem.trim(u8, self.asString(), " "), 10) catch 0,
            else => 0,
        };
    }

    /// Convert to boolean
    pub fn toBool(self: Self) bool {
        return switch (self.tag()) {
            .null_val => false,
            .boolean => self.asBool(),
            .integer => self.asInt() != 0,
            .decimal => if (self.asDecimal()) |d| d.value != 0 else false,
            .fixed_string => blk: {
                const s = self.asString();
                break :blk s.len > 0 and !std.mem.allEqual(u8, s, ' ');
            },
            .string => self.asString().len > 0,
            else => true,
        };
    }

    /// Get string representation (for string types)
    pub fn toString(self: Self) []const u8 {
        return self.asString();
    }

    // ========================================================================
    // Formatting
    // ========================================================================

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.tag()) {
            .null_val => try writer.writeAll("null"),
            .boolean => try writer.print("{}", .{self.asBool()}),
            .integer => try writer.print("{d}", .{self.asInt()}),
            .decimal => {
                if (self.asDecimal()) |d| {
                    const divisor = std.math.pow(i64, 10, d.precision);
                    const whole = @divTrunc(d.value, divisor);
                    const frac = @abs(@rem(d.value, divisor));
                    try writer.print("{d}.{d}", .{ whole, frac });
                }
            },
            .fixed_string, .string => try writer.print("{s}", .{self.asString()}),
            .record_ref => try writer.writeAll("<record>"),
            .handle => try writer.print("<handle:{d}>", .{self.asHandle() orelse 0}),
            .object => {
                const type_id = self.objectTypeId() orelse 0;
                // For now, special-case Map (type_id 16) for backward compatibility
                // In the future, use type registry for formatting
                if (type_id == 16) {
                    if (self.asMap()) |m| {
                        try writer.print("<map:{d}>", .{m.len()});
                    } else {
                        try writer.writeAll("<map:null>");
                    }
                } else {
                    try writer.print("<object:type={d}>", .{type_id});
                }
            },
        }
    }

    /// Debug representation showing type and value - use for tracing/debugging
    pub fn debugRepr(self: Self, buf: []u8) []const u8 {
        var fbs = std.io.fixedBufferStream(buf);
        const writer = fbs.writer();

        const tag_name = switch (self.tag()) {
            .null_val => "null",
            .boolean => "bool",
            .integer => "int",
            .decimal => "dec",
            .fixed_string => "fstr",
            .string => "str",
            .record_ref => "rec",
            .handle => "hnd",
            .object => "obj",
        };

        writer.print("{s}:", .{tag_name}) catch {};

        switch (self.tag()) {
            .null_val => writer.writeAll("nil") catch {},
            .boolean => writer.print("{}", .{self.asBool()}) catch {},
            .integer => writer.print("{d}", .{self.asInt()}) catch {},
            .decimal => {
                if (self.asDecimal()) |d| {
                    writer.print("{d}p{d}", .{ d.value, d.precision }) catch {};
                }
            },
            .fixed_string, .string => {
                const s = self.asString();
                if (s.len > 20) {
                    writer.print("\"{s}...\"({d})", .{ s[0..20], s.len }) catch {};
                } else {
                    writer.print("\"{s}\"", .{s}) catch {};
                }
            },
            .record_ref => writer.writeAll("<record>") catch {},
            .handle => writer.print("{d}", .{self.asHandle() orelse 0}) catch {},
            .object => writer.print("type={d}", .{self.objectTypeId() orelse 0}) catch {},
        }

        return fbs.getWritten();
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    fn ptrToPayload(ptr: anytype) u64 {
        const addr = @intFromPtr(ptr);
        std.debug.assert(addr <= PTR_MASK);
        return @as(u64, @intCast(addr)) & PTR_MASK;
    }

    fn payloadToPtr(comptime T: type, bits: u64) T {
        const addr = bits & PTR_MASK;
        return @ptrFromInt(addr);
    }

    /// Check equality
    pub fn eql(self: Self, other: Self) bool {
        return self.bits == other.bits;
    }

    /// Get type name for debugging
    pub fn typeName(self: Self) []const u8 {
        return self.tag().name();
    }

    /// Free heap-allocated memory for this value
    /// Call this when a value is being discarded and its memory should be freed
    pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
        const upper = self.bits & 0xFFFF_C000_0000_0000;

        // Check for boxed integer
        if (upper == TAG_BOXED_INT) {
            const boxed = payloadToPtr(*BoxedInt, self.bits);
            allocator.destroy(boxed);
            return;
        }

        // Check for decimal
        if (upper == TAG_DECIMAL) {
            const dec = payloadToPtr(*Decimal, self.bits);
            allocator.destroy(dec);
            return;
        }

        // Check for string (immutable)
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_STRING) {
            const ref = payloadToPtr(*StringRef, self.bits);
            allocator.destroy(ref);
            return;
        }

        // Check for fixed string (mutable)
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_FIXED_STR) {
            const ref = payloadToPtr(*FixedStringRef, self.bits);
            allocator.destroy(ref);
            return;
        }

        // Check for extension objects
        if ((self.bits & TAG_OBJECT_MASK) == TAG_OBJECT) {
            const type_id = self.objectTypeId() orelse return;
            // For now, special-case Map (type_id 16)
            // In the future, use type registry for cleanup
            if (type_id == 16) {
                if (self.objectPtr(OrderedMap)) |map| {
                    map.deinit();
                    allocator.destroy(map);
                }
            }
            // Other object types would be handled via type registry
            return;
        }

        // Record refs point to shared data, don't free here
        // Handles are just numeric IDs, no memory to free
        // Small ints, bools, null are inline - no memory to free
    }

    /// Check if this value has heap-allocated memory that needs cleanup
    pub fn needsCleanup(self: Self) bool {
        const upper = self.bits & 0xFFFF_C000_0000_0000;
        if (upper == TAG_BOXED_INT or upper == TAG_DECIMAL) return true;
        if ((self.bits & TAG_OBJECT_MASK) == TAG_OBJECT) return true;
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_STRING) return true;
        if ((self.bits & 0xFFFF_8000_0000_0000) == TAG_FIXED_STR) return true;
        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Value size is 8 bytes" {
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Value));
}

test "null value" {
    const v = Value.initNull();
    try std.testing.expect(v.isNull());
    try std.testing.expectEqual(Tag.null_val, v.tag());
    try std.testing.expect(!v.toBool());
}

test "boolean values" {
    const t = Value.initBool(true);
    const f = Value.initBool(false);

    try std.testing.expect(t.isBool());
    try std.testing.expect(f.isBool());
    try std.testing.expectEqual(Tag.boolean, t.tag());
    try std.testing.expect(t.asBool());
    try std.testing.expect(!f.asBool());
    try std.testing.expect(t.toBool());
    try std.testing.expect(!f.toBool());
}

test "small integer values" {
    const zero = Value.initInt(0);
    const pos = Value.initInt(42);
    const neg = Value.initInt(-100);
    const large = Value.initInt(1_000_000_000_000);

    try std.testing.expectEqual(Tag.integer, zero.tag());
    try std.testing.expectEqual(Tag.integer, pos.tag());
    try std.testing.expectEqual(Tag.integer, neg.tag());
    try std.testing.expectEqual(Tag.integer, large.tag());

    try std.testing.expectEqual(@as(i64, 0), zero.asInt());
    try std.testing.expectEqual(@as(i64, 42), pos.asInt());
    try std.testing.expectEqual(@as(i64, -100), neg.asInt());
    try std.testing.expectEqual(@as(i64, 1_000_000_000_000), large.asInt());
}

test "boxed integer" {
    const allocator = std.testing.allocator;

    const huge: i64 = 0x7FFF_FFFF_FFFF_FFFF;
    const v = try Value.initBoxedInt(allocator, huge);
    defer arc.release(v, allocator);

    try std.testing.expectEqual(Tag.integer, v.tag());
    try std.testing.expectEqual(huge, v.asInt());
}

test "tag-based switch" {
    const v = Value.initInt(42);

    const result: i64 = switch (v.tag()) {
        .integer => v.asInt() * 2,
        .boolean => if (v.asBool()) 1 else 0,
        else => -1,
    };

    try std.testing.expectEqual(@as(i64, 84), result);
}

test "toInt conversion" {
    try std.testing.expectEqual(@as(i64, 42), Value.initInt(42).toInt());
    try std.testing.expectEqual(@as(i64, 1), Value.initBool(true).toInt());
    try std.testing.expectEqual(@as(i64, 0), Value.initBool(false).toInt());
    try std.testing.expectEqual(@as(i64, 0), Value.initNull().toInt());
}

test "toBool conversion" {
    try std.testing.expect(Value.initInt(1).toBool());
    try std.testing.expect(!Value.initInt(0).toBool());
    try std.testing.expect(Value.initBool(true).toBool());
    try std.testing.expect(!Value.initBool(false).toBool());
    try std.testing.expect(!Value.initNull().toBool());
}
