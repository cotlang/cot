//! DBL Type System Extension
//!
//! Handles DBL-specific type declarations and field specifiers.

const std = @import("std");

/// DBL field specifier from record definitions
/// Examples: "name, a50" or "amount, d10.2" or "flags, i4"
pub const FieldSpec = struct {
    name: []const u8,
    type_char: u8, // 'a', 'd', 'i', 'p'
    size: u32,
    precision: u8 = 0,
    array_dim: ?[]const u32 = null, // Array dimensions

    /// Parse a DBL field definition line
    /// Format: "name, type" or "name, type, dimension"
    pub fn parse(line: []const u8) !FieldSpec {
        // Find comma separating name from type
        const comma_pos = std.mem.indexOf(u8, line, ",") orelse
            return error.InvalidFieldSpec;

        const name = std.mem.trim(u8, line[0..comma_pos], " \t");
        const rest = std.mem.trim(u8, line[comma_pos + 1 ..], " \t");

        if (rest.len == 0) return error.InvalidFieldSpec;

        const type_char = rest[0];
        if (type_char != 'a' and type_char != 'd' and
            type_char != 'i' and type_char != 'p')
        {
            return error.InvalidTypeChar;
        }

        // Parse size
        var size: u32 = 0;
        var precision: u8 = 0;
        var i: usize = 1;

        // Parse main size
        while (i < rest.len and rest[i] >= '0' and rest[i] <= '9') : (i += 1) {
            size = size * 10 + @as(u32, rest[i] - '0');
        }

        // Check for precision
        if (i < rest.len and rest[i] == '.') {
            i += 1;
            while (i < rest.len and rest[i] >= '0' and rest[i] <= '9') : (i += 1) {
                precision = precision * 10 + @as(u8, rest[i] - '0');
            }
        }

        return .{
            .name = name,
            .type_char = type_char,
            .size = if (size == 0) getDefaultSize(type_char) else size,
            .precision = precision,
        };
    }

    fn getDefaultSize(type_char: u8) u32 {
        return switch (type_char) {
            'a' => 1, // Default alpha is 1 char
            'd' => 10, // Default decimal
            'i' => 4, // Default integer (i4)
            'p' => 6, // Default packed
            else => 1,
        };
    }

    /// Get the byte size of this field
    pub fn byteSize(self: FieldSpec) u32 {
        return switch (self.type_char) {
            'a' => self.size, // Alpha: 1 byte per char
            'd' => self.size, // Decimal: 1 byte per digit
            'i' => self.size, // Integer: size is byte count
            'p' => (self.size + 1) / 2, // Packed: 2 digits per byte
            else => self.size,
        };
    }
};

/// DBL overlay/union type
/// Allows multiple interpretations of the same memory
pub const Overlay = struct {
    base_field: []const u8,
    overlay_fields: []OverlayField,
};

pub const OverlayField = struct {
    name: []const u8,
    offset: u32,
    type_char: u8,
    size: u32,
};

test "field spec parsing" {
    const f1 = try FieldSpec.parse("name, a50");
    try std.testing.expectEqualStrings("name", f1.name);
    try std.testing.expect(f1.type_char == 'a');
    try std.testing.expect(f1.size == 50);

    const f2 = try FieldSpec.parse("amount, d10.2");
    try std.testing.expectEqualStrings("amount", f2.name);
    try std.testing.expect(f2.type_char == 'd');
    try std.testing.expect(f2.size == 10);
    try std.testing.expect(f2.precision == 2);
}

test "field byte size" {
    const alpha = FieldSpec{ .name = "x", .type_char = 'a', .size = 50 };
    try std.testing.expect(alpha.byteSize() == 50);

    const packed_field = FieldSpec{ .name = "y", .type_char = 'p', .size = 6 };
    try std.testing.expect(packed_field.byteSize() == 3); // 6 digits = 3 bytes
}
