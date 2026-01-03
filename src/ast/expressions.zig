//! AST Expression Types
//!
//! Modern expression types only - no DBL-isms.
//! DBL syntax is mapped to these modern concepts by the cot-dbl frontend.

const std = @import("std");

/// Expression type tag for SoA storage
pub const ExpressionTag = enum(u8) {
    // ============================================
    // Literals
    // ============================================

    /// Integer literal: 42, 0xFF, 0b1010
    int_literal,

    /// Floating point literal: 3.14, 1.0e10
    float_literal,

    /// String literal: "hello"
    string_literal,

    /// Boolean literal: true, false
    bool_literal,

    /// Null literal: null
    null_literal,

    // ============================================
    // Identifiers and Access
    // ============================================

    /// Variable/constant reference: name
    identifier,

    /// Member access: expr.field
    member,

    /// Array/slice indexing: expr[index]
    index,

    // ============================================
    // Operators
    // ============================================

    /// Binary operation: a + b, a && b, a == b
    binary,

    /// Unary operation: -a, !a, &a, *a
    unary,

    // ============================================
    // Calls
    // ============================================

    /// Function call: func(args)
    call,

    /// Method call: expr.method(args)
    method_call,

    // ============================================
    // Compound Expressions
    // ============================================

    /// Range expression: start..end, start..=end
    range,

    /// Array initializer: [1, 2, 3]
    array_init,

    /// Struct initializer: Point{ .x = 1, .y = 2 }
    struct_init,

    /// Lambda/closure: |x| x + 1, |x, y| { return x + y; }
    lambda,

    // ============================================
    // Special
    // ============================================

    /// Compile-time builtin: @sizeof(T), @alignof(T)
    comptime_builtin,

    /// Parenthesized expression: (expr)
    grouping,

    /// If expression: if cond then a else b
    if_expr,

    /// Match expression: match expr { patterns }
    match_expr,

    /// Block expression: { statements; result }
    block_expr,

    /// Check if this is a literal expression
    pub fn isLiteral(self: ExpressionTag) bool {
        return switch (self) {
            .int_literal, .float_literal, .string_literal, .bool_literal, .null_literal => true,
            else => false,
        };
    }

    /// Check if this is an access expression
    pub fn isAccess(self: ExpressionTag) bool {
        return switch (self) {
            .identifier, .member, .index => true,
            else => false,
        };
    }

    /// Check if this is an operator expression
    pub fn isOperator(self: ExpressionTag) bool {
        return switch (self) {
            .binary, .unary => true,
            else => false,
        };
    }

    /// Check if this is a call expression
    pub fn isCall(self: ExpressionTag) bool {
        return switch (self) {
            .call, .method_call => true,
            else => false,
        };
    }

    /// Check if this is a compound expression
    pub fn isCompound(self: ExpressionTag) bool {
        return switch (self) {
            .range, .array_init, .struct_init, .lambda => true,
            else => false,
        };
    }

    /// Check if this is an expression that can contain statements (control flow as expression)
    pub fn isBlockLike(self: ExpressionTag) bool {
        return switch (self) {
            .if_expr, .match_expr, .block_expr, .lambda => true,
            else => false,
        };
    }
};

/// Binary operator types
pub const BinaryOp = enum(u8) {
    // Arithmetic
    add, // +
    sub, // -
    mul, // *
    div, // /
    mod, // %

    // Comparison
    eq, // ==
    ne, // !=
    lt, // <
    le, // <=
    gt, // >
    ge, // >=

    // Logical
    @"and", // &&
    @"or", // ||

    // Bitwise
    bit_and, // &
    bit_or, // |
    bit_xor, // ^
    shl, // <<
    shr, // >>

    // Rounding (DBL legacy: # and ##)
    round, // ## - true rounding to N decimal places
    trunc, // # - truncating round to N decimal places

    // Range (for range expressions in data)
    range, // ..
    range_inclusive, // ..=

    /// Get the precedence of this operator (higher binds tighter)
    pub fn precedence(self: BinaryOp) u8 {
        return switch (self) {
            .@"or" => 1,
            .@"and" => 2,
            .eq, .ne, .lt, .le, .gt, .ge => 3,
            .bit_or => 4,
            .bit_xor => 5,
            .bit_and => 6,
            .shl, .shr => 7,
            .add, .sub => 8,
            .mul, .div, .mod => 9,
            .round, .trunc => 10, // Highest arithmetic precedence (before unary)
            .range, .range_inclusive => 0, // Lowest precedence
        };
    }

    /// Check if this operator is right-associative
    pub fn isRightAssociative(self: BinaryOp) bool {
        _ = self;
        // Currently no right-associative binary operators
        return false;
    }

    /// Check if this is a comparison operator
    pub fn isComparison(self: BinaryOp) bool {
        return switch (self) {
            .eq, .ne, .lt, .le, .gt, .ge => true,
            else => false,
        };
    }

    /// Check if this is a logical operator
    pub fn isLogical(self: BinaryOp) bool {
        return switch (self) {
            .@"and", .@"or" => true,
            else => false,
        };
    }

    /// Check if this is an arithmetic operator
    pub fn isArithmetic(self: BinaryOp) bool {
        return switch (self) {
            .add, .sub, .mul, .div, .mod, .round, .trunc => true,
            else => false,
        };
    }
};

/// Unary operator types
pub const UnaryOp = enum(u8) {
    // Arithmetic
    neg, // -

    // Logical
    not, // !

    // Pointer/Reference
    addr_of, // &
    deref, // *

    // Bitwise
    bit_not, // ~

    /// Check if this operator is a prefix operator
    pub fn isPrefix(self: UnaryOp) bool {
        _ = self;
        // All unary operators are prefix in this language
        return true;
    }
};

// ============================================================
// Tests
// ============================================================

test "ExpressionTag categories" {
    try std.testing.expect(ExpressionTag.int_literal.isLiteral());
    try std.testing.expect(!ExpressionTag.int_literal.isOperator());

    try std.testing.expect(ExpressionTag.binary.isOperator());
    try std.testing.expect(!ExpressionTag.binary.isLiteral());

    try std.testing.expect(ExpressionTag.identifier.isAccess());
    try std.testing.expect(ExpressionTag.member.isAccess());
    try std.testing.expect(ExpressionTag.index.isAccess());

    try std.testing.expect(ExpressionTag.call.isCall());
    try std.testing.expect(ExpressionTag.method_call.isCall());

    try std.testing.expect(ExpressionTag.lambda.isCompound());
    try std.testing.expect(ExpressionTag.lambda.isBlockLike());
}

test "BinaryOp precedence" {
    // Multiplication binds tighter than addition
    try std.testing.expect(BinaryOp.mul.precedence() > BinaryOp.add.precedence());

    // Comparison binds tighter than logical
    try std.testing.expect(BinaryOp.eq.precedence() > BinaryOp.@"and".precedence());

    // And binds tighter than or
    try std.testing.expect(BinaryOp.@"and".precedence() > BinaryOp.@"or".precedence());
}

test "BinaryOp categories" {
    try std.testing.expect(BinaryOp.add.isArithmetic());
    try std.testing.expect(!BinaryOp.add.isComparison());

    try std.testing.expect(BinaryOp.eq.isComparison());
    try std.testing.expect(!BinaryOp.eq.isArithmetic());

    try std.testing.expect(BinaryOp.@"and".isLogical());
}

test "UnaryOp is prefix" {
    try std.testing.expect(UnaryOp.neg.isPrefix());
    try std.testing.expect(UnaryOp.not.isPrefix());
    try std.testing.expect(UnaryOp.addr_of.isPrefix());
}
