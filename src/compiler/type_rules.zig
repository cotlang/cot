//! Type Compatibility Rules
//!
//! Defines the rules for type compatibility in assignments and operations.
//! Used by the type checker to validate code at compile time.

const std = @import("std");
const ir = @import("../ir/ir.zig");
const Type = ir.Type;

/// Type compatibility result
pub const Compatibility = enum {
    /// Types are directly compatible (no conversion needed)
    compatible,
    /// Types are compatible with implicit conversion
    implicit_conversion,
    /// Types are compatible but may lose precision
    lossy_conversion,
    /// Types are not compatible
    incompatible,
};

/// Dereference a pointer type (used to get the underlying value type)
pub fn derefType(ty: Type) Type {
    return switch (ty) {
        .ptr => |p| p.*,
        else => ty,
    };
}

/// Check if a value type can be assigned to a target type
pub fn isAssignable(target: Type, value: Type) Compatibility {
    // Automatically dereference pointers for assignment compatibility
    const target_deref = derefType(target);
    const value_deref = derefType(value);

    // Same types are always compatible
    if (std.meta.eql(target_deref, value_deref)) {
        return .compatible;
    }

    return switch (target_deref) {
        .void => .incompatible,
        .bool => switch (value_deref) {
            .bool => .compatible,
            else => .incompatible,
        },

        // Dynamic string
        .string => switch (value_deref) {
            .string => .compatible,
            // [N]u8 arrays can be implicitly converted to dynamic string
            .array => |a| if (a.element.* == .u8) .implicit_conversion else .incompatible,
            else => .incompatible,
        },

        // Array assignment compatibility
        .array => |target_arr| blk: {
            // Handle [N]u8 (string-like) arrays specially
            if (target_arr.element.* == .u8) {
                break :blk switch (value_deref) {
                    .string => .implicit_conversion, // Dynamic to fixed may truncate
                    .array => |value_arr| inner: {
                        if (value_arr.element.* != .u8) break :inner .incompatible;
                        if (value_arr.length <= target_arr.length) {
                            break :inner .compatible;
                        }
                        break :inner .lossy_conversion;
                    },
                    else => .incompatible,
                };
            }
            // General array handling
            break :blk switch (value_deref) {
                .array => |value_arr| inner: {
                    if (!std.meta.eql(target_arr.element.*, value_arr.element.*)) {
                        break :inner .incompatible;
                    }
                    if (value_arr.length <= target_arr.length) {
                        break :inner .compatible;
                    }
                    break :inner .incompatible;
                },
                else => .incompatible,
            };
        },

        // Decimal (precision, scale)
        .implied_decimal => |target_dec| switch (value_deref) {
            .implied_decimal => |value_dec| blk: {
                if (value_dec.precision <= target_dec.precision and
                    value_dec.scale <= target_dec.scale)
                {
                    break :blk .compatible;
                }
                break :blk .lossy_conversion;
            },
            .fixed_decimal => .implicit_conversion, // Fixed to implied requires scaling
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .implicit_conversion,
            else => .incompatible,
        },

        // Fixed decimal (no decimal point, just width)
        .fixed_decimal => |target_fixed| switch (value_deref) {
            .fixed_decimal => |value_fixed| blk: {
                if (value_fixed.width <= target_fixed.width) {
                    break :blk .compatible;
                }
                break :blk .lossy_conversion;
            },
            .implied_decimal => .lossy_conversion, // Implied to fixed loses scale
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .implicit_conversion,
            else => .incompatible,
        },

        // Signed integers
        .i8 => switch (value_deref) {
            .i8 => .compatible,
            .u8 => .lossy_conversion, // May overflow
            else => .incompatible,
        },
        .i16 => switch (value_deref) {
            .i8, .i16 => .compatible,
            .u8 => .compatible,
            .u16 => .lossy_conversion,
            else => .incompatible,
        },
        .i32 => switch (value_deref) {
            .i8, .i16, .i32 => .compatible,
            .u8, .u16 => .compatible,
            .u32 => .lossy_conversion,
            .implied_decimal, .fixed_decimal => .lossy_conversion,
            else => .incompatible,
        },
        .i64 => switch (value_deref) {
            .i8, .i16, .i32, .i64 => .compatible,
            .u8, .u16, .u32 => .compatible,
            .u64 => .lossy_conversion,
            .implied_decimal, .fixed_decimal => .lossy_conversion,
            else => .incompatible,
        },

        // Unsigned integers
        .u8 => switch (value_deref) {
            .u8 => .compatible,
            .i8 => .lossy_conversion,
            else => .incompatible,
        },
        .u16 => switch (value_deref) {
            .u8, .u16 => .compatible,
            .i8, .i16 => .lossy_conversion,
            else => .incompatible,
        },
        .u32 => switch (value_deref) {
            .u8, .u16, .u32 => .compatible,
            .i8, .i16, .i32 => .lossy_conversion,
            else => .incompatible,
        },
        .u64 => switch (value_deref) {
            .u8, .u16, .u32, .u64 => .compatible,
            .i8, .i16, .i32, .i64 => .lossy_conversion,
            else => .incompatible,
        },

        // Pointer-sized integers
        .isize => switch (value_deref) {
            .i8, .i16, .i32, .isize => .compatible,
            .i64 => .lossy_conversion, // May lose on 32-bit
            .u8, .u16, .u32 => .compatible,
            .u64, .usize => .lossy_conversion,
            else => .incompatible,
        },
        .usize => switch (value_deref) {
            .u8, .u16, .u32, .usize => .compatible,
            .u64 => .lossy_conversion, // May lose on 32-bit
            .i8, .i16, .i32, .i64, .isize => .lossy_conversion,
            else => .incompatible,
        },

        // Floats
        .f32 => switch (value_deref) {
            .f32 => .compatible,
            .i8, .i16, .u8, .u16 => .implicit_conversion,
            .i32, .u32, .f64 => .lossy_conversion,
            else => .incompatible,
        },
        .f64 => switch (value_deref) {
            .f32, .f64 => .compatible,
            .i8, .i16, .i32, .u8, .u16, .u32 => .implicit_conversion,
            .i64, .u64, .implied_decimal, .fixed_decimal => .lossy_conversion,
            else => .incompatible,
        },

        .ptr => |target_ptr| switch (value_deref) {
            .ptr => |value_ptr| blk: {
                if (typesEqual(target_ptr.*, value_ptr.*)) {
                    break :blk .compatible;
                }
                if (target_ptr.* == .void) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },

        .optional => |target_inner| switch (value_deref) {
            .optional => |value_inner| blk: {
                if (typesEqual(target_inner.*, value_inner.*)) {
                    break :blk .compatible;
                }
                // ?void (null literal) is coercible to any optional type
                if (value_inner.* == .void) {
                    break :blk .implicit_conversion;
                }
                break :blk .incompatible;
            },
            else => blk: {
                // Non-optional can be assigned to optional
                if (typesEqual(target_inner.*, value_deref)) {
                    break :blk .implicit_conversion;
                }
                break :blk .incompatible;
            },
        },

        // General array handling is done above with the [N]u8 string-like case

        .slice => |target_elem| switch (value_deref) {
            .slice => |value_elem| blk: {
                if (typesEqual(target_elem.*, value_elem.*)) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            .array => |arr| blk: {
                if (typesEqual(target_elem.*, arr.element.*)) {
                    break :blk .implicit_conversion;
                }
                break :blk .incompatible;
            },
            // void (empty array literal []) is coercible to any slice type
            .void => .implicit_conversion,
            else => .incompatible,
        },

        .@"struct" => |target_struct| switch (value_deref) {
            .@"struct" => |value_struct| blk: {
                if (std.mem.eql(u8, target_struct.name, value_struct.name)) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },

        .@"union" => |target_union| switch (value_deref) {
            .@"union" => |value_union| blk: {
                if (std.mem.eql(u8, target_union.name, value_union.name)) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },

        .function => .incompatible,
        .map => |target_map| switch (value_deref) {
            .map => |value_map| blk: {
                // Maps with same key and value types are compatible
                if (typesEqual(target_map.key_type.*, value_map.key_type.*) and
                    typesEqual(target_map.value_type.*, value_map.value_type.*))
                {
                    break :blk .compatible;
                }
                // Map<void, void> (from Map.new()) can be assigned to any map type
                if (value_map.key_type.* == .void and value_map.value_type.* == .void) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },
        .list => |target_list| switch (value_deref) {
            .list => |value_list| blk: {
                // Lists with same element types are compatible
                if (typesEqual(target_list.element_type.*, value_list.element_type.*)) {
                    break :blk .compatible;
                }
                // List<void> (from List.new()) can be assigned to any list type
                // since it's an empty list that can be used for any element type
                if (value_list.element_type.* == .void) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },
        .weak => |target_inner| switch (value_deref) {
            .weak => |value_inner| blk: {
                if (std.meta.eql(target_inner.*, value_inner.*)) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible, // Must explicitly create weak reference
        },
        .trait_object => |target_trait| switch (value_deref) {
            // Struct can be coerced to trait object if it implements the trait
            // The actual trait implementation check is done during lowering
            .@"struct" => .implicit_conversion,
            // Same trait object is compatible
            .trait_object => |value_trait| blk: {
                if (std.mem.eql(u8, target_trait.trait_name, value_trait.trait_name)) {
                    break :blk .compatible;
                }
                break :blk .incompatible;
            },
            else => .incompatible,
        },
    };
}

/// Check if two types are semantically equal (by name/structure, not pointer identity)
pub fn typesEqual(a: Type, b: Type) bool {
    const a_tag = @intFromEnum(std.meta.activeTag(a));
    const b_tag = @intFromEnum(std.meta.activeTag(b));
    if (a_tag != b_tag) return false;

    return switch (a) {
        .void, .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .isize, .usize, .f32, .f64, .string => true,
        .implied_decimal => |ad| blk: {
            const bd = b.implied_decimal;
            break :blk ad.precision == bd.precision and ad.scale == bd.scale;
        },
        .fixed_decimal => |ad| blk: {
            const bd = b.fixed_decimal;
            break :blk ad.width == bd.width;
        },
        .ptr => |ap| typesEqual(ap.*, b.ptr.*),
        .optional => |ao| typesEqual(ao.*, b.optional.*),
        .array => |aa| blk: {
            const ba = b.array;
            break :blk aa.length == ba.length and typesEqual(aa.element.*, ba.element.*);
        },
        .slice => |as| typesEqual(as.*, b.slice.*),
        .@"struct" => |as| std.mem.eql(u8, as.name, b.@"struct".name),
        .@"union" => |au| std.mem.eql(u8, au.name, b.@"union".name),
        .function => false, // Function types would need more complex comparison
        .map => true, // Maps are structurally typed (compare key/value types if needed)
        .list => |al| typesEqual(al.element_type.*, b.list.element_type.*),
        .weak => |aw| typesEqual(aw.*, b.weak.*),
        .trait_object => |at| std.mem.eql(u8, at.trait_name, b.trait_object.trait_name),
    };
}

/// Check if an assignment is allowed (compatible or implicit conversion)
pub fn canAssign(target: Type, value: Type) bool {
    const compat = isAssignable(target, value);
    return compat == .compatible or compat == .implicit_conversion;
}

/// Check if an assignment requires explicit conversion
pub fn requiresExplicitConversion(target: Type, value: Type) bool {
    const compat = isAssignable(target, value);
    return compat == .incompatible;
}

/// Check if two types are compatible for binary operations
pub const BinaryOpType = enum {
    arithmetic, // +, -, *, /
    comparison, // ==, !=, <, >, <=, >=
    logical, // .and., .or.
    string_concat, // + for strings
};

/// Result of binary operation type check
pub const BinaryOpResult = struct {
    ok: bool,
    result_type: ?Type,
};

pub fn checkBinaryOp(op: BinaryOpType, lhs: Type, rhs: Type) BinaryOpResult {
    const lhs_deref = derefType(lhs);
    const rhs_deref = derefType(rhs);

    return switch (op) {
        .arithmetic => checkArithmeticOp(lhs_deref, rhs_deref),
        .comparison => checkComparisonOp(lhs_deref, rhs_deref),
        .logical => checkLogicalOp(lhs_deref, rhs_deref),
        .string_concat => checkStringConcat(lhs_deref, rhs_deref),
    };
}

fn checkArithmeticOp(lhs: Type, rhs: Type) BinaryOpResult {
    // String concatenation: string + string -> dynamic string
    if (isString(lhs) and isString(rhs)) {
        // Result is always dynamic string (allocation happens at runtime)
        return .{ .ok = true, .result_type = .{ .string = {} } };
    }

    // String + numeric: implicit conversion to string concat
    if (isString(lhs) or isString(rhs)) {
        return .{ .ok = true, .result_type = .{ .string = {} } };
    }

    // Both must be numeric for arithmetic
    if (!isNumeric(lhs) or !isNumeric(rhs)) {
        return .{ .ok = false, .result_type = null };
    }

    // Result type is the "larger" of the two
    const result = getWiderNumericType(lhs, rhs);
    return .{ .ok = true, .result_type = result };
}

fn checkComparisonOp(lhs: Type, rhs: Type) BinaryOpResult {
    if (isNumeric(lhs) and isNumeric(rhs)) {
        return .{ .ok = true, .result_type = .{ .bool = {} } };
    }

    if (isString(lhs) and isString(rhs)) {
        return .{ .ok = true, .result_type = .{ .bool = {} } };
    }

    return .{ .ok = false, .result_type = null };
}

fn checkLogicalOp(_: Type, _: Type) BinaryOpResult {
    return .{ .ok = true, .result_type = .{ .bool = {} } };
}

fn checkStringConcat(lhs: Type, rhs: Type) BinaryOpResult {
    // Auto-coercion: at least one operand must be a string
    // The VM will auto-convert numeric types to their string representation
    const lhs_is_string = isString(lhs);
    const rhs_is_string = isString(rhs);

    if (!lhs_is_string and !rhs_is_string) {
        return .{ .ok = false, .result_type = null };
    }

    // Result is always dynamic string (allocation happens at runtime)
    return .{ .ok = true, .result_type = .{ .string = {} } };
}

/// Get string length (0 for dynamic strings)
fn getStringLen(ty: Type) u32 {
    return switch (ty) {
        .string => 0,
        // [N]u8 arrays have a fixed length
        .array => |a| if (a.element.* == .u8) a.length else 0,
        else => 0,
    };
}

/// Get the wider numeric type for arithmetic operations
fn getWiderNumericType(lhs: Type, rhs: Type) Type {
    // Decimal is widest (implied_decimal has more precision info)
    if (lhs == .implied_decimal or lhs == .fixed_decimal) return lhs;
    if (rhs == .implied_decimal or rhs == .fixed_decimal) return rhs;

    // Floats next
    if (lhs == .f64 or rhs == .f64) return .{ .f64 = {} };
    if (lhs == .f32 or rhs == .f32) return .{ .f32 = {} };

    // Then signed/unsigned 64-bit
    if (lhs == .i64 or rhs == .i64) return .{ .i64 = {} };
    if (lhs == .u64 or rhs == .u64) return .{ .u64 = {} };

    // Then 32-bit
    if (lhs == .i32 or rhs == .i32) return .{ .i32 = {} };
    if (lhs == .u32 or rhs == .u32) return .{ .u32 = {} };

    // Then 16-bit
    if (lhs == .i16 or rhs == .i16) return .{ .i16 = {} };
    if (lhs == .u16 or rhs == .u16) return .{ .u16 = {} };

    // Default to i32
    return .{ .i32 = {} };
}

/// Check if a type is numeric
pub fn isNumeric(ty: Type) bool {
    return switch (ty) {
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .implied_decimal, .fixed_decimal => true,
        else => false,
    };
}

/// Check if a type is a string type
pub fn isString(ty: Type) bool {
    return switch (ty) {
        .string => true,
        // [N]u8 arrays are treated as fixed-length strings
        .array => |a| a.element.* == .u8,
        else => false,
    };
}

/// Get a human-readable type name for error messages
pub fn typeName(ty: Type) []const u8 {
    return switch (ty) {
        .void => "void",
        .bool => "bool",
        .i8 => "i8",
        .i16 => "i16",
        .i32 => "i32",
        .i64 => "i64",
        .u8 => "u8",
        .u16 => "u16",
        .u32 => "u32",
        .u64 => "u64",
        .isize => "isize",
        .usize => "usize",
        .f32 => "f32",
        .f64 => "f64",
        .string => "string",
        .implied_decimal => "decimal",
        .fixed_decimal => "fixed_decimal",
        .ptr => "pointer",
        .optional => "optional",
        .array => "array",
        .slice => "slice",
        .@"struct" => "struct",
        .@"union" => "union",
        .function => "function",
        .map => "Map",
        .list => "List",
        .weak => "weak",
        .trait_object => "trait_object",
    };
}

/// Format a type with details for error messages
pub fn formatType(ty: Type, buf: []u8) []const u8 {
    var fbs = std.io.fixedBufferStream(buf);
    const writer = fbs.writer();

    switch (ty) {
        .void => writer.writeAll("void") catch {},
        .bool => writer.writeAll("bool") catch {},
        .i8 => writer.writeAll("i8") catch {},
        .i16 => writer.writeAll("i16") catch {},
        .i32 => writer.writeAll("i32") catch {},
        .i64 => writer.writeAll("i64") catch {},
        .u8 => writer.writeAll("u8") catch {},
        .u16 => writer.writeAll("u16") catch {},
        .u32 => writer.writeAll("u32") catch {},
        .u64 => writer.writeAll("u64") catch {},
        .isize => writer.writeAll("isize") catch {},
        .usize => writer.writeAll("usize") catch {},
        .f32 => writer.writeAll("f32") catch {},
        .f64 => writer.writeAll("f64") catch {},
        .string => writer.writeAll("string") catch {},
        .implied_decimal => |d| writer.print("decimal({d},{d})", .{ d.precision, d.scale }) catch {},
        .fixed_decimal => |d| writer.print("fixed_decimal({d})", .{d.width}) catch {},
        .ptr => |p| {
            var inner_buf: [64]u8 = undefined;
            const inner = formatType(p.*, &inner_buf);
            writer.writeAll("*") catch {};
            writer.writeAll(inner) catch {};
        },
        .optional => |o| {
            var inner_buf: [64]u8 = undefined;
            const inner = formatType(o.*, &inner_buf);
            writer.writeAll("?") catch {};
            writer.writeAll(inner) catch {};
        },
        .array => |a| writer.print("[{d}]{s}", .{ a.length, typeName(a.element.*) }) catch {},
        .slice => |s| writer.print("[]{s}", .{typeName(s.*)}) catch {},
        .@"struct" => |s| writer.print("struct({s})", .{s.name}) catch {},
        .@"union" => |u| writer.print("union({s})", .{u.name}) catch {},
        .function => writer.writeAll("function") catch {},
        .map => writer.writeAll("Map") catch {},
        .list => |l| writer.print("List<{s}>", .{typeName(l.element_type.*)}) catch {},
        .weak => |w| {
            var inner_buf: [64]u8 = undefined;
            const inner = formatType(w.*, &inner_buf);
            writer.writeAll("weak ") catch {};
            writer.writeAll(inner) catch {};
        },
        .trait_object => |t| writer.print("dyn {s}", .{t.trait_name}) catch {},
    }

    return fbs.getWritten();
}

// Tests
test "numeric assignment compatibility" {
    const dec: Type = .{ .implied_decimal = .{ .precision = 10, .scale = 2 } };
    const int: Type = .{ .i32 = {} };

    // Integer to decimal is implicit conversion
    try std.testing.expect(isAssignable(dec, int) == .implicit_conversion);
}

test "string assignment compatibility" {
    // Use static u8 type for array element
    const u8_elem: Type = .{ .u8 = {} };
    const str10: Type = .{ .array = .{ .element = &u8_elem, .length = 10 } };
    const str20: Type = .{ .array = .{ .element = &u8_elem, .length = 20 } };
    const dec: Type = .{ .implied_decimal = .{ .precision = 4, .scale = 0 } };

    // Shorter string to longer is compatible
    try std.testing.expect(canAssign(str20, str10));

    // Longer string to shorter is lossy
    try std.testing.expect(isAssignable(str10, str20) == .lossy_conversion);

    // Numeric to string is incompatible
    try std.testing.expect(!canAssign(str10, dec));
}

test "arithmetic operation types" {
    const dec: Type = .{ .implied_decimal = .{ .precision = 4, .scale = 0 } };
    const int: Type = .{ .i32 = {} };
    // Use static u8 type for array element
    const u8_elem: Type = .{ .u8 = {} };
    const str: Type = .{ .array = .{ .element = &u8_elem, .length = 10 } };

    // Decimal + integer is valid
    const result = checkBinaryOp(.arithmetic, dec, int);
    try std.testing.expect(result.ok);

    // String + string is valid (concat)
    const concat_result = checkBinaryOp(.arithmetic, str, str);
    try std.testing.expect(concat_result.ok);
}
