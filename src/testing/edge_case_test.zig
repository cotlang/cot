//! Edge Case Tests
//!
//! Tests for boundary conditions, special values, and unusual inputs.
//! Ensures robust handling of edge cases across the compilation pipeline.

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig").Lexer;
const Token = @import("../lexer/token.zig").Token;
const TokenType = @import("../lexer/token.zig").TokenType;
const StringInterner = @import("../base/mod.zig").StringInterner;
const NodeStore = @import("../ast/mod.zig").NodeStore;
const NodeData = @import("../ast/mod.zig").NodeData;
const SourceLoc = @import("../ast/mod.zig").SourceLoc;

// ============================================================================
// Numeric Boundary Tests
// ============================================================================

test "lexer: max integer literal" {
    var lexer = Lexer.init("9223372036854775807"); // i64 max
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.integer, tokens[0].type);
}

test "lexer: zero" {
    var lexer = Lexer.init("0");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.integer, tokens[0].type);
    try testing.expectEqualStrings("0", tokens[0].lexeme);
}

test "lexer: negative number as unary minus" {
    var lexer = Lexer.init("-42");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    // Should be minus operator followed by integer
    try testing.expect(tokens.len >= 2);
    try testing.expectEqual(TokenType.minus, tokens[0].type);
    try testing.expectEqual(TokenType.integer, tokens[1].type);
}

test "lexer: very small float" {
    var lexer = Lexer.init("0.000001");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.float, tokens[0].type);
}

test "lexer: hex boundary values" {
    var lexer = Lexer.init("0x0 0xFFFFFFFF 0xFFFFFFFFFFFFFFFF");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.integer, tokens[0].type);
    try testing.expectEqual(TokenType.integer, tokens[1].type);
    try testing.expectEqual(TokenType.integer, tokens[2].type);
}

// ============================================================================
// String Edge Cases
// ============================================================================

test "lexer: empty string" {
    var lexer = Lexer.init("\"\"");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.string, tokens[0].type);
}

test "lexer: string with escaped quote" {
    var lexer = Lexer.init("\"hello\\\"world\"");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.string, tokens[0].type);
}

test "lexer: string with newline escape" {
    var lexer = Lexer.init("\"line1\\nline2\"");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.string, tokens[0].type);
}

test "lexer: string with tab escape" {
    var lexer = Lexer.init("\"col1\\tcol2\"");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.string, tokens[0].type);
}

test "StringInterner: empty string" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const id = try interner.intern("");
    try testing.expectEqualStrings("", interner.get(id));
}

test "StringInterner: very long string" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const long_string = "a" ** 10000;
    const id = try interner.intern(long_string);
    try testing.expectEqualStrings(long_string, interner.get(id));
}

test "StringInterner: many unique strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "string_{d}", .{i}) catch unreachable;
        _ = try interner.intern(str);
    }

    try testing.expectEqual(@as(usize, 1000), interner.count());
}

// ============================================================================
// Identifier Edge Cases
// ============================================================================

test "lexer: single character identifier" {
    var lexer = Lexer.init("a b c x y z");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 6);
    for (tokens[0..6]) |tok| {
        try testing.expectEqual(TokenType.identifier, tok.type);
    }
}

test "lexer: underscore only identifier" {
    var lexer = Lexer.init("_");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.identifier, tokens[0].type);
}

test "lexer: identifier starting with underscore" {
    var lexer = Lexer.init("_private __dunder");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.identifier, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
}

test "lexer: identifier with numbers" {
    var lexer = Lexer.init("var1 x2y3z abc123");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.identifier, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqual(TokenType.identifier, tokens[2].type);
}

test "lexer: very long identifier" {
    const long_ident = "a" ** 256;
    var lexer = Lexer.init(long_ident);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.identifier, tokens[0].type);
}

// ============================================================================
// Whitespace and Comment Edge Cases
// ============================================================================

test "lexer: only newlines" {
    var lexer = Lexer.init("\n\n\n\n\n");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.eof, tokens[tokens.len - 1].type);
}

test "lexer: mixed whitespace" {
    var lexer = Lexer.init("a\t\n \r\nb");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2);
    try testing.expectEqualStrings("a", tokens[0].lexeme);
    try testing.expectEqualStrings("b", tokens[1].lexeme);
}

test "lexer: comment only file" {
    var lexer = Lexer.init("// this is just a comment");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.eof, tokens[tokens.len - 1].type);
}

test "lexer: nested block comments" {
    var lexer = Lexer.init("a /* outer /* inner */ still outer */ b");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    // Different languages handle nested comments differently
    // Just ensure we don't crash
    try testing.expect(tokens.len >= 1);
}

test "lexer: block comment at EOF" {
    var lexer = Lexer.init("x /* unterminated");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    // Should handle gracefully
    try testing.expect(tokens.len >= 1);
}

// ============================================================================
// NodeData Edge Cases
// ============================================================================

test "NodeData: empty is zero" {
    const empty = NodeData.empty;
    try testing.expectEqual(@as(u32, 0), empty.a);
    try testing.expectEqual(@as(u32, 0), empty.b);
}

test "NodeData: int literal max value" {
    const data = NodeData.intLiteral(std.math.maxInt(i64));
    const reconstructed = @as(i64, @bitCast(@as(u64, data.a) | (@as(u64, data.b) << 32)));
    try testing.expectEqual(std.math.maxInt(i64), reconstructed);
}

test "NodeData: int literal min value" {
    const data = NodeData.intLiteral(std.math.minInt(i64));
    const reconstructed = @as(i64, @bitCast(@as(u64, data.a) | (@as(u64, data.b) << 32)));
    try testing.expectEqual(std.math.minInt(i64), reconstructed);
}

test "NodeData: int literal zero" {
    const data = NodeData.intLiteral(0);
    const reconstructed = @as(i64, @bitCast(@as(u64, data.a) | (@as(u64, data.b) << 32)));
    try testing.expectEqual(@as(i64, 0), reconstructed);
}

test "NodeData: int literal negative one" {
    const data = NodeData.intLiteral(-1);
    const reconstructed = @as(i64, @bitCast(@as(u64, data.a) | (@as(u64, data.b) << 32)));
    try testing.expectEqual(@as(i64, -1), reconstructed);
}

// ============================================================================
// SourceLoc Edge Cases
// ============================================================================

test "SourceLoc: zero location" {
    const loc = SourceLoc.zero;
    try testing.expectEqual(@as(u24, 0), loc.line);
    try testing.expectEqual(@as(u8, 0), loc.column);
}

test "SourceLoc: max line number" {
    const loc = SourceLoc.init(std.math.maxInt(u24), 1);
    try testing.expectEqual(std.math.maxInt(u24), loc.line);
}

test "SourceLoc: max column number" {
    const loc = SourceLoc.init(1, std.math.maxInt(u8));
    try testing.expectEqual(std.math.maxInt(u8), loc.column);
}

test "SourceLoc: overflow handling" {
    // Values larger than max should be clamped
    const loc = SourceLoc.init(std.math.maxInt(u32), std.math.maxInt(u32));
    try testing.expectEqual(std.math.maxInt(u24), loc.line);
    try testing.expectEqual(std.math.maxInt(u8), loc.column);
}

// ============================================================================
// Operator Edge Cases
// ============================================================================

test "lexer: all comparison operators together" {
    var lexer = Lexer.init("< > <= >= == !=");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.lt, tokens[0].type);
    try testing.expectEqual(TokenType.gt, tokens[1].type);
    try testing.expectEqual(TokenType.le, tokens[2].type);
    try testing.expectEqual(TokenType.ge, tokens[3].type);
    try testing.expectEqual(TokenType.eq, tokens[4].type);
    try testing.expectEqual(TokenType.ne, tokens[5].type);
}

test "lexer: assignment vs equality" {
    var lexer = Lexer.init("= ==");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.equals, tokens[0].type);
    try testing.expectEqual(TokenType.eq, tokens[1].type);
}

test "lexer: adjacent operators no space" {
    var lexer = Lexer.init("1+2*3/4-5");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 9);
}

test "lexer: increment operator" {
    var lexer = Lexer.init("++ +");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.plus_plus, tokens[0].type);
    try testing.expectEqual(TokenType.plus, tokens[1].type);
}

// ============================================================================
// Token Position Edge Cases
// ============================================================================

test "lexer: first token at column 1" {
    var lexer = Lexer.init("first");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 1), tokens[0].column);
    try testing.expectEqual(@as(usize, 1), tokens[0].line);
}

test "lexer: tokens on many lines" {
    const source = "a\n" ** 100 ++ "b";
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 101);
    try testing.expectEqual(@as(usize, 101), tokens[100].line);
}

test "lexer: very long line" {
    const source = "x " ** 1000;
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1000);
}

// ============================================================================
// NodeStore Edge Cases
// ============================================================================

test "NodeStore: expression count starts at zero" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    try testing.expectEqual(@as(usize, 0), store.expressionCount());
}

test "NodeStore: statement count starts at zero" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    try testing.expectEqual(@as(usize, 0), store.statementCount());
}

test "NodeStore: many expressions" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        _ = try store.addExpression(.int_literal, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 1000), store.expressionCount());
}

test "NodeStore: many statements" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        _ = try store.addStatement(.expression_stmt, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 1000), store.statementCount());
}
