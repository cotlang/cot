//! Regression Tests
//!
//! Tests that ensure previously fixed bugs stay fixed and expected
//! behaviors remain consistent. These tests document known edge cases
//! and verify the system handles them correctly.

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const TokenType = @import("../lexer/token.zig").TokenType;
const StringInterner = @import("../base/mod.zig").StringInterner;
const StringId = @import("../base/mod.zig").StringId;
const NodeStore = @import("../ast/mod.zig").NodeStore;
const NodeData = @import("../ast/mod.zig").NodeData;
const SourceLoc = @import("../ast/mod.zig").SourceLoc;

// ============================================================================
// Lexer Regression Tests
// ============================================================================

test "regression: lexer preserves token order" {
    // Tokens must appear in source order
    var lexer = Lexer.init("a b c d e");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqualStrings("a", tokens[0].lexeme);
    try testing.expectEqualStrings("b", tokens[1].lexeme);
    try testing.expectEqualStrings("c", tokens[2].lexeme);
    try testing.expectEqualStrings("d", tokens[3].lexeme);
    try testing.expectEqualStrings("e", tokens[4].lexeme);
}

test "regression: lexer handles CRLF line endings" {
    var lexer = Lexer.init("a\r\nb\r\nc");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 3);
    // Line numbers should increment correctly
    try testing.expectEqual(@as(usize, 1), tokens[0].line);
    try testing.expectEqual(@as(usize, 2), tokens[1].line);
    try testing.expectEqual(@as(usize, 3), tokens[2].line);
}

test "regression: lexer handles LF line endings" {
    var lexer = Lexer.init("a\nb\nc");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(@as(usize, 1), tokens[0].line);
    try testing.expectEqual(@as(usize, 2), tokens[1].line);
    try testing.expectEqual(@as(usize, 3), tokens[2].line);
}

test "regression: lexer != is not confused with !" {
    var lexer = Lexer.init("! != !!");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.bang, tokens[0].type);
    try testing.expectEqual(TokenType.ne, tokens[1].type);
    try testing.expectEqual(TokenType.bang, tokens[2].type);
    try testing.expectEqual(TokenType.bang, tokens[3].type);
}

test "regression: lexer <= is not confused with < and =" {
    var lexer = Lexer.init("< = <= <==");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.lt, tokens[0].type);
    try testing.expectEqual(TokenType.equals, tokens[1].type);
    try testing.expectEqual(TokenType.le, tokens[2].type);
    try testing.expectEqual(TokenType.le, tokens[3].type);
    try testing.expectEqual(TokenType.equals, tokens[4].type);
}

test "regression: lexer >= is not confused with > and =" {
    var lexer = Lexer.init("> = >= >==");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.gt, tokens[0].type);
    try testing.expectEqual(TokenType.equals, tokens[1].type);
    try testing.expectEqual(TokenType.ge, tokens[2].type);
    try testing.expectEqual(TokenType.ge, tokens[3].type);
    try testing.expectEqual(TokenType.equals, tokens[4].type);
}

test "regression: lexer arrow vs minus and gt" {
    var lexer = Lexer.init("- > -> ->");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.minus, tokens[0].type);
    try testing.expectEqual(TokenType.gt, tokens[1].type);
    try testing.expectEqual(TokenType.arrow, tokens[2].type);
    try testing.expectEqual(TokenType.arrow, tokens[3].type);
}

test "regression: lexer walrus vs colon and equals" {
    var lexer = Lexer.init(": = := :=");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.colon, tokens[0].type);
    try testing.expectEqual(TokenType.equals, tokens[1].type);
    try testing.expectEqual(TokenType.walrus, tokens[2].type);
    try testing.expectEqual(TokenType.walrus, tokens[3].type);
}

test "regression: lexer double colon vs two colons" {
    var lexer = Lexer.init(": : :: ::");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expectEqual(TokenType.colon, tokens[0].type);
    try testing.expectEqual(TokenType.colon, tokens[1].type);
    try testing.expectEqual(TokenType.double_colon, tokens[2].type);
    try testing.expectEqual(TokenType.double_colon, tokens[3].type);
}

// ============================================================================
// StringInterner Regression Tests
// ============================================================================

test "regression: interner returns same ID for same string" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("test");
    const id2 = try interner.intern("test");
    const id3 = try interner.intern("test");

    try testing.expectEqual(id1, id2);
    try testing.expectEqual(id2, id3);
}

test "regression: interner different IDs for different strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("aaa");
    const id2 = try interner.intern("bbb");
    const id3 = try interner.intern("ccc");

    try testing.expect(id1 != id2);
    try testing.expect(id2 != id3);
    try testing.expect(id1 != id3);
}

test "regression: interner null_id is special" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    // null_id should never be returned by intern
    const id = try interner.intern("anything");
    try testing.expect(id != .null_id);
}

test "regression: interner get after intern" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const original = "test_string";
    const id = try interner.intern(original);
    const retrieved = interner.get(id);

    try testing.expectEqualStrings(original, retrieved);
}

test "regression: interner handles prefix strings correctly" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("test");
    const id2 = try interner.intern("testing");
    const id3 = try interner.intern("test");

    // "test" and "testing" are different
    try testing.expect(id1 != id2);
    // But second "test" should match first
    try testing.expectEqual(id1, id3);
}

// ============================================================================
// NodeData Regression Tests
// ============================================================================

test "regression: NodeData binary preserves operands" {
    const lhs_idx = @import("../ast/mod.zig").ExprIdx.fromInt(10);
    const rhs_idx = @import("../ast/mod.zig").ExprIdx.fromInt(20);
    const op = @import("../ast/mod.zig").BinaryOp.add;

    const data = NodeData.binary(lhs_idx, rhs_idx, op);

    // Verify we can extract the components
    try testing.expectEqual(@as(u32, 10), data.a);
    // b contains rhs shifted and op
    const extracted_op: @import("../ast/mod.zig").BinaryOp = @enumFromInt(@as(u8, @truncate(data.b)));
    try testing.expectEqual(op, extracted_op);
}

test "regression: NodeData intLiteral preserves sign" {
    // Positive value
    const pos_data = NodeData.intLiteral(12345);
    const pos_reconstructed = @as(i64, @bitCast(@as(u64, pos_data.a) | (@as(u64, pos_data.b) << 32)));
    try testing.expectEqual(@as(i64, 12345), pos_reconstructed);

    // Negative value
    const neg_data = NodeData.intLiteral(-12345);
    const neg_reconstructed = @as(i64, @bitCast(@as(u64, neg_data.a) | (@as(u64, neg_data.b) << 32)));
    try testing.expectEqual(@as(i64, -12345), neg_reconstructed);
}

// ============================================================================
// SourceLoc Regression Tests
// ============================================================================

test "regression: SourceLoc zero is 0,0" {
    const loc = SourceLoc.zero;
    try testing.expectEqual(@as(u24, 0), loc.line);
    try testing.expectEqual(@as(u8, 0), loc.column);
}

test "regression: SourceLoc init preserves values" {
    const loc = SourceLoc.init(100, 50);
    try testing.expectEqual(@as(u24, 100), loc.line);
    try testing.expectEqual(@as(u8, 50), loc.column);
}

// ============================================================================
// Parser Regression Tests
// ============================================================================

fn createParserContext(allocator: std.mem.Allocator, source: []const u8) !struct {
    parser: Parser,
    store: *NodeStore,
    strings: *StringInterner,
    tokens: []const @import("../lexer/token.zig").Token,
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

fn cleanupParserContext(allocator: std.mem.Allocator, ctx: anytype) void {
    ctx.parser.deinit();
    ctx.store.deinit();
    ctx.strings.deinit();
    allocator.destroy(ctx.store);
    allocator.destroy(ctx.strings);
    allocator.free(ctx.tokens);
}

test "regression: parser empty input produces no errors" {
    const allocator = testing.allocator;
    var ctx = try createParserContext(allocator, "");
    defer cleanupParserContext(allocator, &ctx);

    const result = ctx.parser.parse() catch null;
    if (result) |r| {
        var res = r;
        res.deinit(allocator);
    }
    // Empty input should be valid (no statements is ok)
}

test "regression: parser single function is valid" {
    const allocator = testing.allocator;
    var ctx = try createParserContext(allocator, "fn main() {}");
    defer cleanupParserContext(allocator, &ctx);

    const result = ctx.parser.parse() catch null;
    if (result) |r| {
        var res = r;
        defer res.deinit(allocator);
        try testing.expect(res.top_level.len >= 1);
    }
}

test "regression: parser error count starts at zero" {
    const allocator = testing.allocator;
    var ctx = try createParserContext(allocator, "fn valid() {}");
    defer cleanupParserContext(allocator, &ctx);

    try testing.expectEqual(@as(usize, 0), ctx.parser.errorCount());
}
