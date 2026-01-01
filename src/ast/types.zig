//! AST Type Types
//!
//! Modern type system - no DBL-isms.
//! DBL types are mapped to these modern concepts by the cot-dbl frontend.

const std = @import("std");

/// Type tag for SoA storage
pub const TypeTag = enum(u8) {
    // ============================================
    // Primitive Types - Integers
    // ============================================

    /// 8-bit signed integer
    i8,

    /// 16-bit signed integer
    i16,

    /// 32-bit signed integer
    i32,

    /// 64-bit signed integer
    i64,

    /// 8-bit unsigned integer
    u8,

    /// 16-bit unsigned integer
    u16,

    /// 32-bit unsigned integer
    u32,

    /// 64-bit unsigned integer
    u64,

    // ============================================
    // Primitive Types - Floats
    // ============================================

    /// 32-bit floating point
    f32,

    /// 64-bit floating point
    f64,

    // ============================================
    // Primitive Types - Other
    // ============================================

    /// Boolean type
    bool,

    /// Void/unit type
    void,

    /// Dynamic string (heap allocated)
    string,

    /// Fixed-length string (for DBL alpha compatibility)
    string_fixed,

    /// Decimal type (for financial calculations)
    decimal,

    // ============================================
    // Compound Types
    // ============================================

    /// Fixed-size array: [N]T
    array,

    /// Dynamic slice: []T
    slice,

    /// Optional type: ?T
    optional,

    /// Pointer type: *T, *const T
    pointer,

    /// Named type reference: MyStruct, MyEnum
    named,

    /// Function type: fn(A, B) -> C
    function,

    /// Tuple type: (A, B, C)
    tuple,

    /// Error union type: T!E
    error_union,

    // ============================================
    // Generic Types
    // ============================================

    /// Type parameter reference: T, U (reference to a declared type param)
    type_param,

    /// Generic type instantiation: Foo<i32, string>
    generic_instance,

    // ============================================
    // Special Types
    // ============================================

    /// Type is inferred
    inferred,

    /// Any type (for generic contexts)
    any,

    /// Never type (for functions that don't return)
    never,

    /// Check if this is a signed integer type
    pub fn isSignedInt(self: TypeTag) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64 => true,
            else => false,
        };
    }

    /// Check if this is an unsigned integer type
    pub fn isUnsignedInt(self: TypeTag) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64 => true,
            else => false,
        };
    }

    /// Check if this is any integer type
    pub fn isInteger(self: TypeTag) bool {
        return self.isSignedInt() or self.isUnsignedInt();
    }

    /// Check if this is a floating point type
    pub fn isFloat(self: TypeTag) bool {
        return switch (self) {
            .f32, .f64 => true,
            else => false,
        };
    }

    /// Check if this is a numeric type
    pub fn isNumeric(self: TypeTag) bool {
        return self.isInteger() or self.isFloat() or self == .decimal;
    }

    /// Check if this is a primitive type
    pub fn isPrimitive(self: TypeTag) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .bool, .void, .string, .string_fixed, .decimal => true,
            else => false,
        };
    }

    /// Check if this is a compound/composite type
    pub fn isCompound(self: TypeTag) bool {
        return switch (self) {
            .array, .slice, .optional, .pointer, .named, .function, .tuple, .error_union => true,
            else => false,
        };
    }

    /// Check if this type can be null/optional
    pub fn canBeNull(self: TypeTag) bool {
        return switch (self) {
            .optional, .pointer => true,
            else => false,
        };
    }

    /// Get the bit width for integer types (0 for non-integers)
    pub fn bitWidth(self: TypeTag) u16 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32, .f32 => 32,
            .i64, .u64, .f64 => 64,
            else => 0,
        };
    }
};

/// Pointer mutability
pub const Mutability = enum(u1) {
    /// Immutable pointer: *const T
    @"const",
    /// Mutable pointer: *T
    mut,
};

// ============================================================
// Tests
// ============================================================

test "TypeTag integer categories" {
    try std.testing.expect(TypeTag.i32.isSignedInt());
    try std.testing.expect(!TypeTag.i32.isUnsignedInt());
    try std.testing.expect(TypeTag.i32.isInteger());

    try std.testing.expect(TypeTag.u64.isUnsignedInt());
    try std.testing.expect(!TypeTag.u64.isSignedInt());
    try std.testing.expect(TypeTag.u64.isInteger());
}

test "TypeTag numeric categories" {
    try std.testing.expect(TypeTag.f64.isFloat());
    try std.testing.expect(TypeTag.f64.isNumeric());
    try std.testing.expect(!TypeTag.f64.isInteger());

    try std.testing.expect(TypeTag.decimal.isNumeric());
    try std.testing.expect(!TypeTag.decimal.isInteger());
    try std.testing.expect(!TypeTag.decimal.isFloat());
}

test "TypeTag primitive vs compound" {
    try std.testing.expect(TypeTag.i32.isPrimitive());
    try std.testing.expect(!TypeTag.i32.isCompound());

    try std.testing.expect(TypeTag.array.isCompound());
    try std.testing.expect(!TypeTag.array.isPrimitive());

    try std.testing.expect(TypeTag.optional.isCompound());
    try std.testing.expect(TypeTag.optional.canBeNull());
}

test "TypeTag bit width" {
    try std.testing.expectEqual(@as(u16, 8), TypeTag.i8.bitWidth());
    try std.testing.expectEqual(@as(u16, 16), TypeTag.u16.bitWidth());
    try std.testing.expectEqual(@as(u16, 32), TypeTag.i32.bitWidth());
    try std.testing.expectEqual(@as(u16, 64), TypeTag.f64.bitWidth());
    try std.testing.expectEqual(@as(u16, 0), TypeTag.bool.bitWidth());
}
