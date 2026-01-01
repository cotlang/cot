//! DBL Parser Extension
//!
//! Transforms DBL syntax into Core-compatible AST nodes.
//! This enables the compiler to process DBL code using the Core pipeline.

const std = @import("std");
const lexer_ext = @import("lexer_ext.zig");

/// DBL type specifier (e.g., a50, d10, d10.2, i4, p6)
pub const DblTypeSpec = struct {
    kind: Kind,
    size: ?u32 = null,
    precision: ?u32 = null,

    pub const Kind = enum {
        alpha, // a, a50 - string/alpha
        decimal, // d, d10 - decimal number
        integer, // i1, i2, i4, i8 - integer
        packed_decimal, // p6 - packed decimal
        boolean, // boolean
        handle, // @handle - object reference
    };

    /// Parse a DBL type specifier from string
    /// Examples: "a50", "d10", "d10.2", "i4", "p6"
    pub fn parse(s: []const u8) ?DblTypeSpec {
        if (s.len == 0) return null;

        var spec = DblTypeSpec{ .kind = undefined };

        // Determine kind from first character
        switch (s[0]) {
            'a', 'A' => spec.kind = .alpha,
            'd', 'D' => spec.kind = .decimal,
            'i', 'I' => spec.kind = .integer,
            'p', 'P' => spec.kind = .packed_decimal,
            else => return null,
        }

        if (s.len == 1) {
            // Just "a", "d", etc. - no size specified
            return spec;
        }

        // Parse size
        const rest = s[1..];
        if (std.mem.indexOf(u8, rest, ".")) |dot_pos| {
            // Has precision (e.g., d10.2)
            const size_str = rest[0..dot_pos];
            const prec_str = rest[dot_pos + 1 ..];

            spec.size = std.fmt.parseInt(u32, size_str, 10) catch return null;
            spec.precision = std.fmt.parseInt(u32, prec_str, 10) catch return null;
        } else {
            // No precision
            spec.size = std.fmt.parseInt(u32, rest, 10) catch return null;
        }

        return spec;
    }

    /// Convert to a Core type description
    pub fn toCoreType(self: DblTypeSpec) CoreType {
        return switch (self.kind) {
            .alpha => .{ .string = self.size },
            .decimal => .{ .decimal = .{ .size = self.size, .precision = self.precision } },
            .integer => .{ .integer = @as(u8, @intCast(self.size orelse 4)) },
            .packed_decimal => .{ .packed_decimal = self.size },
            .boolean => .{ .boolean = {} },
            .handle => .{ .handle = {} },
        };
    }
};

/// Core type representation (for mapping)
pub const CoreType = union(enum) {
    string: ?u32, // string or string(size)
    decimal: struct { size: ?u32, precision: ?u32 },
    integer: u8, // 1, 2, 4, or 8 bytes
    packed_decimal: ?u32,
    boolean: void,
    handle: void,
};

/// Transform DBL syntax construct to Core equivalent
pub const SyntaxTransform = struct {
    /// Transform "PROC name" to "fn name() {"
    pub fn procToFn(name: []const u8) []const u8 {
        _ = name;
        // In practice this would construct the appropriate AST node
        return "fn";
    }

    /// Transform DBL comparison operator to Core
    pub fn dblOpToCore(op: lexer_ext.DblOperator) []const u8 {
        return switch (op) {
            .eq => "==",
            .ne => "!=",
            .lt => "<",
            .le => "<=",
            .gt => ">",
            .ge => ">=",
            .and_ => "&&",
            .or_ => "||",
            .not_ => "!",
            .xor_ => "^",
            .band => "&",
            .bor => "|",
            .bnot => "~",
        };
    }

    /// Transform "XCALL name(args)" to "name(args)"
    /// The XCALL keyword is stripped and it becomes a normal function call
    pub fn xcallToCall(name: []const u8) []const u8 {
        return name;
    }

    /// Transform "FOR i FROM 1 THRU 10" to "for i in 1..10"
    pub fn forThruToRange(start: i64, end: i64) struct { start: i64, end: i64 } {
        return .{ .start = start, .end = end };
    }
};

test "dbl type spec parsing" {
    const a50 = DblTypeSpec.parse("a50") orelse unreachable;
    try std.testing.expect(a50.kind == .alpha);
    try std.testing.expect(a50.size.? == 50);

    const d10_2 = DblTypeSpec.parse("d10.2") orelse unreachable;
    try std.testing.expect(d10_2.kind == .decimal);
    try std.testing.expect(d10_2.size.? == 10);
    try std.testing.expect(d10_2.precision.? == 2);

    const int4 = DblTypeSpec.parse("i4") orelse unreachable;
    try std.testing.expect(int4.kind == .integer);
    try std.testing.expect(int4.size.? == 4);
}

test "dbl op to core" {
    try std.testing.expectEqualStrings("==", SyntaxTransform.dblOpToCore(.eq));
    try std.testing.expectEqualStrings("&&", SyntaxTransform.dblOpToCore(.and_));
}
