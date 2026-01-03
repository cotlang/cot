//! Error Recovery Tests
//!
//! Comprehensive tests for error handling across the compilation pipeline:
//! - Lexer error handling
//! - Parser error recovery
//! - Type system errors

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const ParseError = @import("../parser/parser.zig").ParseError;
const NodeStore = @import("../ast/mod.zig").NodeStore;
const StringInterner = @import("../ast/mod.zig").StringInterner;
const Token = @import("../lexer/token.zig").Token;
const TokenType = @import("../lexer/token.zig").TokenType;

// ============================================================================
// Lexer Error Tests
// ============================================================================

test "lexer: handles empty input" {
    var lexer = Lexer.init("");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1);
    try testing.expectEqual(TokenType.eof, tokens[tokens.len - 1].type);
}

test "lexer: handles whitespace only input" {
    var lexer = Lexer.init("   \n\t\r\n   ");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1);
    try testing.expectEqual(TokenType.eof, tokens[tokens.len - 1].type);
}

test "lexer: tracks line numbers correctly" {
    var lexer = Lexer.init("a\nb\nc");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(@as(usize, 1), tokens[0].line);
    try testing.expectEqual(@as(usize, 2), tokens[1].line);
    try testing.expectEqual(@as(usize, 3), tokens[2].line);
}

test "lexer: tracks column numbers correctly" {
    var lexer = Lexer.init("abc def");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2);
    try testing.expectEqual(@as(usize, 1), tokens[0].column);
    try testing.expectEqual(@as(usize, 5), tokens[1].column);
}

test "lexer: handles comments at end of file" {
    var lexer = Lexer.init("x // comment");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1);
    try testing.expectEqualStrings("x", tokens[0].lexeme);
}

test "lexer: handles block comments" {
    var lexer = Lexer.init("a /* comment */ b");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2);
    try testing.expectEqualStrings("a", tokens[0].lexeme);
    try testing.expectEqualStrings("b", tokens[1].lexeme);
}

test "lexer: handles multi-line block comments" {
    var lexer = Lexer.init("a /* line1\nline2\nline3 */ b");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2);
    try testing.expectEqualStrings("a", tokens[0].lexeme);
    try testing.expectEqualStrings("b", tokens[1].lexeme);
}

test "lexer: handles consecutive operators" {
    var lexer = Lexer.init("+-*/");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 4);
    try testing.expectEqual(TokenType.plus, tokens[0].type);
    try testing.expectEqual(TokenType.minus, tokens[1].type);
    try testing.expectEqual(TokenType.star, tokens[2].type);
    try testing.expectEqual(TokenType.slash, tokens[3].type);
}

test "lexer: handles compound operators" {
    var lexer = Lexer.init("+= -= *= /=");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 4);
    try testing.expectEqual(TokenType.plus_equals, tokens[0].type);
    try testing.expectEqual(TokenType.minus_equals, tokens[1].type);
    try testing.expectEqual(TokenType.star_equals, tokens[2].type);
    try testing.expectEqual(TokenType.slash_equals, tokens[3].type);
}

test "lexer: handles arrow operator" {
    var lexer = Lexer.init("fn(x: i32) -> i32");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    var found_arrow = false;
    for (tokens) |tok| {
        if (tok.type == .arrow) {
            found_arrow = true;
            break;
        }
    }
    try testing.expect(found_arrow);
}

test "lexer: handles walrus operator" {
    var lexer = Lexer.init("x := 5");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.walrus, tokens[1].type);
}

test "lexer: handles double colon" {
    var lexer = Lexer.init("std::io");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    var found_double_colon = false;
    for (tokens) |tok| {
        if (tok.type == .double_colon) {
            found_double_colon = true;
            break;
        }
    }
    try testing.expect(found_double_colon);
}

// ============================================================================
// Parser Error Tests
// ============================================================================

fn createParser(allocator: std.mem.Allocator, source: []const u8) !struct {
    parser: Parser,
    store: *NodeStore,
    strings: *StringInterner,
    tokens: []const Token,
} {
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);

    const store = try allocator.create(NodeStore);
    store.* = NodeStore.init(allocator);

    const strings = try allocator.create(StringInterner);
    strings.* = StringInterner.init(allocator);

    return .{
        .parser = Parser.init(allocator, tokens, store, strings),
        .store = store,
        .strings = strings,
        .tokens = tokens,
    };
}

fn cleanupParser(allocator: std.mem.Allocator, ctx: anytype) void {
    ctx.parser.deinit();
    ctx.store.deinit();
    ctx.strings.deinit();
    allocator.destroy(ctx.store);
    allocator.destroy(ctx.strings);
    allocator.free(ctx.tokens);
}

test "parser: reports error for missing semicolon" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let x = 5\nlet y = 6");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    // Parser should have recorded errors
    try testing.expect(ctx.parser.hasErrors());
}

test "parser: reports error for unclosed brace" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "fn foo() { return 1;");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
}

test "parser: reports error for unclosed paren" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let x = (1 + 2;");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
}

test "parser: reports error for invalid expression start" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let x = );");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
}

test "parser: reports error for missing function name" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "fn () {}");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
}

test "parser: reports error for missing type annotation" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "fn foo(x:) {}");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
}

test "parser: tracks error location" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let x = ;");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
    try testing.expect(ctx.parser.errors.items.len > 0);
    // Error should be on line 1
    try testing.expectEqual(@as(usize, 1), ctx.parser.errors.items[0].line);
}

test "parser: limits maximum errors" {
    const allocator = testing.allocator;
    // Create source with many errors
    const source = "; ; ; ; ; ; ; ; ; ; " ** 15; // 150 semicolons
    var ctx = try createParser(allocator, source);
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    // Parser should cap errors at MAX_ERRORS (100)
    try testing.expect(ctx.parser.errorCount() <= 100);
}

test "parser: handles empty input" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "");
    defer cleanupParser(allocator, &ctx);

    const result = ctx.parser.parse() catch null;
    // Empty input should either parse successfully with no statements or fail gracefully
    if (result) |r| {
        var res = r;
        res.deinit(allocator);
    }
}

test "parser: handles deeply nested expressions" {
    const allocator = testing.allocator;
    // Create deeply nested parentheses
    const source = "(" ** 50 ++ "1" ++ ")" ** 50;
    var ctx = try createParser(allocator, source);
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch |err| {
        // Should fail with TooNested or similar
        try testing.expect(err == ParseError.TooNested or
            err == ParseError.UnexpectedToken or
            err == ParseError.ExpectedExpression);
        return;
    };
}

// ============================================================================
// Error Message Tests
// ============================================================================

test "parser error has message" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let = 5;");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.hasErrors());
    try testing.expect(ctx.parser.errors.items[0].message.len > 0);
}

test "parser: multiple errors collected" {
    const allocator = testing.allocator;
    var ctx = try createParser(allocator, "let = ;\nlet = ;");
    defer cleanupParser(allocator, &ctx);

    _ = ctx.parser.parse() catch {};

    try testing.expect(ctx.parser.errorCount() >= 1);
}

// ============================================================================
// Token Type Coverage Tests
// ============================================================================

test "lexer: recognizes all bracket types" {
    var lexer = Lexer.init("()[]{}");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.lparen, tokens[0].type);
    try testing.expectEqual(TokenType.rparen, tokens[1].type);
    try testing.expectEqual(TokenType.lbracket, tokens[2].type);
    try testing.expectEqual(TokenType.rbracket, tokens[3].type);
    try testing.expectEqual(TokenType.lbrace, tokens[4].type);
    try testing.expectEqual(TokenType.rbrace, tokens[5].type);
}

test "lexer: recognizes comparison operators" {
    var lexer = Lexer.init("== != < > <= >=");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.eq, tokens[0].type);
    try testing.expectEqual(TokenType.ne, tokens[1].type);
    try testing.expectEqual(TokenType.lt, tokens[2].type);
    try testing.expectEqual(TokenType.gt, tokens[3].type);
    try testing.expectEqual(TokenType.le, tokens[4].type);
    try testing.expectEqual(TokenType.ge, tokens[5].type);
}

test "lexer: recognizes logical operators" {
    var lexer = Lexer.init("&& ||");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.@"and", tokens[0].type);
    try testing.expectEqual(TokenType.@"or", tokens[1].type);
}

test "lexer: recognizes special characters" {
    var lexer = Lexer.init(", ; : @ # % ~ ?");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.comma, tokens[0].type);
    try testing.expectEqual(TokenType.semicolon, tokens[1].type);
    try testing.expectEqual(TokenType.colon, tokens[2].type);
    try testing.expectEqual(TokenType.at, tokens[3].type);
    try testing.expectEqual(TokenType.hash, tokens[4].type);
    try testing.expectEqual(TokenType.percent, tokens[5].type);
    try testing.expectEqual(TokenType.tilde, tokens[6].type);
    try testing.expectEqual(TokenType.question, tokens[7].type);
}

// ============================================================================
// Lexer String and Number Tests
// ============================================================================

test "lexer: handles string literals" {
    var lexer = Lexer.init("\"hello world\"");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1);
    try testing.expectEqual(TokenType.string, tokens[0].type);
}

test "lexer: handles integer literals" {
    var lexer = Lexer.init("42 0 123456");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.integer, tokens[0].type);
    try testing.expectEqual(TokenType.integer, tokens[1].type);
    try testing.expectEqual(TokenType.integer, tokens[2].type);
}

test "lexer: handles float literals" {
    var lexer = Lexer.init("3.14 0.5 123.456");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.float, tokens[0].type);
    try testing.expectEqual(TokenType.float, tokens[1].type);
    try testing.expectEqual(TokenType.float, tokens[2].type);
}

test "lexer: handles hex literals" {
    var lexer = Lexer.init("0xFF 0x1A2B");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2);
    try testing.expectEqual(TokenType.integer, tokens[0].type);
}

test "lexer: handles identifiers" {
    var lexer = Lexer.init("foo bar_baz _underscore");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    try testing.expectEqual(TokenType.identifier, tokens[0].type);
    try testing.expectEqual(TokenType.identifier, tokens[1].type);
    try testing.expectEqual(TokenType.identifier, tokens[2].type);
}

// ============================================================================
// Keyword Recognition Tests
// ============================================================================

test "lexer: recognizes fn keyword" {
    var lexer = Lexer.init("fn");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_fn, tokens[0].type);
}

test "lexer: recognizes let keyword" {
    var lexer = Lexer.init("let");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_let, tokens[0].type);
}

test "lexer: recognizes if/else keywords" {
    var lexer = Lexer.init("if else");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_if, tokens[0].type);
    try testing.expectEqual(TokenType.kw_else, tokens[1].type);
}

test "lexer: recognizes return keyword" {
    var lexer = Lexer.init("return");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_return, tokens[0].type);
}

test "lexer: recognizes while keyword" {
    var lexer = Lexer.init("while");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_while, tokens[0].type);
}

test "lexer: recognizes for keyword" {
    var lexer = Lexer.init("for");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_for, tokens[0].type);
}

test "lexer: recognizes struct keyword" {
    var lexer = Lexer.init("struct");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_struct, tokens[0].type);
}

test "lexer: recognizes true/false keywords" {
    var lexer = Lexer.init("true false");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_true, tokens[0].type);
    try testing.expectEqual(TokenType.kw_false, tokens[1].type);
}

test "lexer: recognizes null keyword" {
    var lexer = Lexer.init("null");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.kw_null, tokens[0].type);
}
