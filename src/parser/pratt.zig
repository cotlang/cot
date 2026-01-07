//! Pratt Parser for Expression Parsing
//!
//! Table-driven expression parser using Pratt's algorithm.
//! Replaces the precedence-climbing approach with a single unified parser.
//!
//! Benefits:
//! - Single function handles all precedence levels
//! - Table-driven: easy to add new operators
//! - Better error messages with context
//! - Foundation for custom operators (future)

const std = @import("std");
const TokenType = @import("../lexer/token.zig").TokenType;

// AST imports
const ast = @import("../ast/mod.zig");
const ExprIdx = ast.ExprIdx;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;

/// Operator precedence levels (higher = binds tighter)
pub const Precedence = enum(u8) {
    none = 0,
    assignment = 1, // = += -= *= /= |= &=
    range = 2, // .. ..=
    null_coalesce = 3, // ??
    or_ = 4, // || or
    and_ = 5, // && and
    equality = 6, // == !=
    comparison = 7, // < > <= >=
    is_test = 8, // is (type testing)
    bit_or = 9, // |
    bit_xor = 10, // ^
    bit_and = 11, // &
    shift = 12, // << >>
    term = 13, // + - #
    factor = 14, // * / %
    unary = 15, // ! - ~ & *
    cast = 16, // as (type cast)
    call = 17, // () . [] ?. ?[
    primary = 18,

    /// Get the next higher precedence level
    pub fn next(self: Precedence) Precedence {
        if (self == .primary) return .primary;
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

/// Parse rule for a token type
pub const ParseRule = struct {
    prefix: ?PrefixFn = null,
    infix: ?InfixFn = null,
    precedence: Precedence = .none,
};

/// Prefix parse function type
pub const PrefixFn = *const fn (ctx: *anyopaque) anyerror!ExprIdx;

/// Infix parse function type (takes left-hand side)
pub const InfixFn = *const fn (ctx: *anyopaque, lhs: ExprIdx) anyerror!ExprIdx;

/// Get the parse rule for a token type
pub fn getRule(token_type: TokenType) ParseRule {
    return rules[@intFromEnum(token_type)];
}

/// Get precedence for a token type
pub fn getPrecedence(token_type: TokenType) Precedence {
    return rules[@intFromEnum(token_type)].precedence;
}

/// Token type count for rule table sizing
const TOKEN_COUNT = @typeInfo(TokenType).@"enum".fields.len;

/// Parse rules table - maps token types to their parse behavior
/// Initialized at comptime for zero runtime cost
pub const rules: [TOKEN_COUNT]ParseRule = init: {
    var r: [TOKEN_COUNT]ParseRule = .{ParseRule{}} ** TOKEN_COUNT;

    // Literals - prefix only
    r[@intFromEnum(TokenType.integer_literal)] = .{ .prefix = null, .precedence = .none }; // Handled by parsePrimary
    r[@intFromEnum(TokenType.decimal_literal)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.string_literal)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.identifier)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.kw_true)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.kw_false)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.kw_null)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.kw_new)] = .{ .prefix = null, .precedence = .none };
    r[@intFromEnum(TokenType.kw_self)] = .{ .prefix = null, .precedence = .none };

    // Grouping and collections
    r[@intFromEnum(TokenType.lparen)] = .{ .prefix = null, .infix = null, .precedence = .call }; // () grouping or call
    r[@intFromEnum(TokenType.lbracket)] = .{ .prefix = null, .infix = null, .precedence = .call }; // [] array or index
    r[@intFromEnum(TokenType.pipe)] = .{ .prefix = null, .infix = null, .precedence = .bit_or }; // |x| lambda or bit_or

    // Unary operators (prefix)
    r[@intFromEnum(TokenType.minus)] = .{ .prefix = null, .infix = null, .precedence = .term }; // negation or subtraction
    r[@intFromEnum(TokenType.bang)] = .{ .prefix = null, .precedence = .unary }; // logical not
    r[@intFromEnum(TokenType.kw_not)] = .{ .prefix = null, .precedence = .unary }; // logical not
    r[@intFromEnum(TokenType.tilde)] = .{ .prefix = null, .precedence = .unary }; // bitwise not
    r[@intFromEnum(TokenType.ampersand)] = .{ .prefix = null, .infix = null, .precedence = .bit_and }; // addr_of or bit_and
    r[@intFromEnum(TokenType.star)] = .{ .prefix = null, .infix = null, .precedence = .factor }; // deref or multiply

    // Binary operators - arithmetic
    r[@intFromEnum(TokenType.plus)] = .{ .infix = null, .precedence = .term };
    r[@intFromEnum(TokenType.plus_plus)] = .{ .infix = null, .precedence = .term }; // string concat (Zig-style)
    r[@intFromEnum(TokenType.slash)] = .{ .infix = null, .precedence = .factor };
    r[@intFromEnum(TokenType.percent)] = .{ .infix = null, .precedence = .factor };

    // Binary operators - comparison
    r[@intFromEnum(TokenType.eq)] = .{ .infix = null, .precedence = .equality };
    r[@intFromEnum(TokenType.ne)] = .{ .infix = null, .precedence = .equality };
    r[@intFromEnum(TokenType.lt)] = .{ .infix = null, .precedence = .comparison };
    r[@intFromEnum(TokenType.le)] = .{ .infix = null, .precedence = .comparison };
    r[@intFromEnum(TokenType.gt)] = .{ .infix = null, .precedence = .comparison };
    r[@intFromEnum(TokenType.ge)] = .{ .infix = null, .precedence = .comparison };

    // Binary operators - logical
    r[@intFromEnum(TokenType.amp_amp)] = .{ .infix = null, .precedence = .and_ };
    r[@intFromEnum(TokenType.kw_and)] = .{ .infix = null, .precedence = .and_ };
    r[@intFromEnum(TokenType.pipe_pipe)] = .{ .infix = null, .precedence = .or_ };
    r[@intFromEnum(TokenType.kw_or)] = .{ .infix = null, .precedence = .or_ };

    // Binary operators - bitwise
    r[@intFromEnum(TokenType.caret)] = .{ .infix = null, .precedence = .bit_xor };
    r[@intFromEnum(TokenType.shl)] = .{ .infix = null, .precedence = .shift };
    r[@intFromEnum(TokenType.shr)] = .{ .infix = null, .precedence = .shift };

    // Cast operator
    r[@intFromEnum(TokenType.kw_as)] = .{ .infix = null, .precedence = .cast }; // expr as type

    // Postfix operators
    r[@intFromEnum(TokenType.period)] = .{ .infix = null, .precedence = .call }; // member access

    // Range operators - low precedence (below comparison, above assignment)
    r[@intFromEnum(TokenType.range)] = .{ .prefix = null, .infix = null, .precedence = .range }; // ..
    r[@intFromEnum(TokenType.range_inclusive)] = .{ .prefix = null, .infix = null, .precedence = .range }; // ..=

    // Null-safety operators
    r[@intFromEnum(TokenType.question_question)] = .{ .infix = null, .precedence = .null_coalesce }; // ??
    r[@intFromEnum(TokenType.question_dot)] = .{ .infix = null, .precedence = .call }; // ?.
    r[@intFromEnum(TokenType.question_lbracket)] = .{ .infix = null, .precedence = .call }; // ?[

    // Type testing
    r[@intFromEnum(TokenType.kw_is)] = .{ .infix = null, .precedence = .is_test }; // is

    break :init r;
};

/// Map token type to binary operator
pub fn tokenToBinaryOp(token_type: TokenType) ?BinaryOp {
    return switch (token_type) {
        // Arithmetic
        .plus, .plus_plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .percent => .mod,

        // Comparison
        .eq => .eq,
        .ne => .ne,
        .lt => .lt,
        .le => .le,
        .gt => .gt,
        .ge => .ge,

        // Logical
        .amp_amp, .kw_and => .@"and",
        .pipe_pipe, .kw_or => .@"or",

        // Bitwise
        .ampersand => .bit_and,
        .pipe => .bit_or,
        .caret => .bit_xor,
        .shl => .shl,
        .shr => .shr,

        // Range
        .range => .range,
        .range_inclusive => .range_inclusive,

        // Null-safety
        .question_question => .null_coalesce,

        else => null,
    };
}

/// Map token type to unary operator
/// Note: dereference is handled as postfix .* in parser.zig, not as prefix *
pub fn tokenToUnaryOp(token_type: TokenType) ?UnaryOp {
    return switch (token_type) {
        .minus => .neg,
        .bang, .kw_not => .not,
        .tilde => .bit_not,
        .ampersand => .addr_of,
        else => null,
    };
}

/// Check if token can start a prefix expression
pub fn canStartExpression(token_type: TokenType) bool {
    return switch (token_type) {
        // Literals
        .integer_literal,
        .decimal_literal,
        .string_literal,
        .identifier,
        .kw_true,
        .kw_false,
        .kw_null,
        .kw_self,
        .kw_new,
        => true,

        // Grouping
        .lparen, .lbracket => true,

        // Unary prefix (note: .star removed - dereference is postfix .*)
        .minus, .bang, .kw_not, .tilde, .ampersand => true,

        // Lambda
        .pipe => true,

        // Range (can start with ..)
        .range, .range_inclusive => true,

        else => false,
    };
}

/// Check if token is a binary operator
pub fn isBinaryOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .plus,
        .minus,
        .star,
        .slash,
        .percent,
        .plus_plus,
        .eq,
        .ne,
        .lt,
        .le,
        .gt,
        .ge,
        .amp_amp,
        .kw_and,
        .pipe_pipe,
        .kw_or,
        .ampersand,
        .pipe,
        .caret,
        .shl,
        .shr,
        .range,
        .range_inclusive,
        .question_question,
        => true,
        else => false,
    };
}

/// Check if token is a postfix operator
pub fn isPostfixOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .lparen, // call
        .lbracket, // index
        .period, // member access
        .question_dot, // optional member access
        .question_lbracket, // optional index
        => true,
        else => false,
    };
}

/// Check if token is a cast operator (as)
pub fn isCastOperator(token_type: TokenType) bool {
    return token_type == .kw_as;
}

/// Check if token is a type test operator (is)
pub fn isTypeTestOperator(token_type: TokenType) bool {
    return token_type == .kw_is;
}

// ============================================================
// Tests
// ============================================================

test "precedence ordering" {
    const testing = std.testing;

    // Verify precedence hierarchy
    try testing.expect(@intFromEnum(Precedence.null_coalesce) < @intFromEnum(Precedence.or_));
    try testing.expect(@intFromEnum(Precedence.or_) < @intFromEnum(Precedence.and_));
    try testing.expect(@intFromEnum(Precedence.and_) < @intFromEnum(Precedence.equality));
    try testing.expect(@intFromEnum(Precedence.equality) < @intFromEnum(Precedence.comparison));
    try testing.expect(@intFromEnum(Precedence.comparison) < @intFromEnum(Precedence.is_test));
    try testing.expect(@intFromEnum(Precedence.is_test) < @intFromEnum(Precedence.bit_or));
    try testing.expect(@intFromEnum(Precedence.bit_or) < @intFromEnum(Precedence.bit_xor));
    try testing.expect(@intFromEnum(Precedence.bit_xor) < @intFromEnum(Precedence.bit_and));
    try testing.expect(@intFromEnum(Precedence.term) < @intFromEnum(Precedence.factor));
    try testing.expect(@intFromEnum(Precedence.factor) < @intFromEnum(Precedence.unary));
    try testing.expect(@intFromEnum(Precedence.unary) < @intFromEnum(Precedence.cast));
    try testing.expect(@intFromEnum(Precedence.cast) < @intFromEnum(Precedence.call));
}

test "binary operator mapping" {
    const testing = std.testing;

    try testing.expectEqual(BinaryOp.add, tokenToBinaryOp(.plus).?);
    try testing.expectEqual(BinaryOp.sub, tokenToBinaryOp(.minus).?);
    try testing.expectEqual(BinaryOp.mul, tokenToBinaryOp(.star).?);
    try testing.expectEqual(BinaryOp.div, tokenToBinaryOp(.slash).?);
    try testing.expectEqual(BinaryOp.eq, tokenToBinaryOp(.eq).?);
    try testing.expectEqual(BinaryOp.@"and", tokenToBinaryOp(.amp_amp).?);
    try testing.expectEqual(BinaryOp.@"or", tokenToBinaryOp(.pipe_pipe).?);
    try testing.expectEqual(BinaryOp.null_coalesce, tokenToBinaryOp(.question_question).?);
}

test "unary operator mapping" {
    const testing = std.testing;

    try testing.expectEqual(UnaryOp.neg, tokenToUnaryOp(.minus).?);
    try testing.expectEqual(UnaryOp.not, tokenToUnaryOp(.bang).?);
    try testing.expectEqual(UnaryOp.not, tokenToUnaryOp(.kw_not).?);
    try testing.expectEqual(UnaryOp.bit_not, tokenToUnaryOp(.tilde).?);
    try testing.expectEqual(UnaryOp.addr_of, tokenToUnaryOp(.ampersand).?);
    // Note: .star is not a unary prefix operator - dereference is postfix .*
    try testing.expectEqual(@as(?UnaryOp, null), tokenToUnaryOp(.star));
}

test "expression starters" {
    const testing = std.testing;

    try testing.expect(canStartExpression(.integer_literal));
    try testing.expect(canStartExpression(.identifier));
    try testing.expect(canStartExpression(.lparen));
    try testing.expect(canStartExpression(.minus));
    try testing.expect(canStartExpression(.bang));
    try testing.expect(!canStartExpression(.plus));
    try testing.expect(!canStartExpression(.eq));
    try testing.expect(!canStartExpression(.eof));
}

test "rule table lookup" {
    const testing = std.testing;

    const plus_rule = getRule(.plus);
    try testing.expectEqual(Precedence.term, plus_rule.precedence);

    const star_rule = getRule(.star);
    try testing.expectEqual(Precedence.factor, star_rule.precedence);

    const eq_rule = getRule(.eq);
    try testing.expectEqual(Precedence.equality, eq_rule.precedence);

    // Null-safety operators
    const coalesce_rule = getRule(.question_question);
    try testing.expectEqual(Precedence.null_coalesce, coalesce_rule.precedence);

    const opt_dot_rule = getRule(.question_dot);
    try testing.expectEqual(Precedence.call, opt_dot_rule.precedence);

    const opt_bracket_rule = getRule(.question_lbracket);
    try testing.expectEqual(Precedence.call, opt_bracket_rule.precedence);

    // Type testing
    const is_rule = getRule(.kw_is);
    try testing.expectEqual(Precedence.is_test, is_rule.precedence);
}

test "null-safety operators" {
    const testing = std.testing;

    // ?? is a binary operator
    try testing.expect(isBinaryOperator(.question_question));

    // ?. and ?[ are postfix operators
    try testing.expect(isPostfixOperator(.question_dot));
    try testing.expect(isPostfixOperator(.question_lbracket));

    // 'is' is a binary operator (for type testing)
    try testing.expect(isBinaryOperator(.kw_is));
}
