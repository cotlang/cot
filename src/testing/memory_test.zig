//! Memory Management Tests
//!
//! Tests for proper memory allocation, deallocation, and cleanup across
//! the Cot codebase. Uses the GPA (GeneralPurposeAllocator) to detect leaks.

const std = @import("std");
const testing = std.testing;

const StringInterner = @import("../base/mod.zig").StringInterner;
const StringId = @import("../base/mod.zig").StringId;
const NodeStore = @import("../ast/mod.zig").NodeStore;
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const Token = @import("../lexer/token.zig").Token;

// ============================================================================
// StringInterner Memory Tests
// ============================================================================

test "StringInterner: no leaks on init/deinit" {
    var interner = StringInterner.init(testing.allocator);
    interner.deinit();
    // If we get here without GPA errors, no leaks
}

test "StringInterner: no leaks after interning strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    _ = try interner.intern("hello");
    _ = try interner.intern("world");
    _ = try interner.intern("test");
    _ = try interner.intern("longer_string_with_more_characters");
}

test "StringInterner: no leaks with duplicate strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("duplicate");
    const id2 = try interner.intern("duplicate");
    const id3 = try interner.intern("duplicate");

    // All should return the same ID
    try testing.expectEqual(id1, id2);
    try testing.expectEqual(id2, id3);
}

test "StringInterner: count tracks unique strings only" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    try testing.expectEqual(@as(usize, 0), interner.count());

    _ = try interner.intern("one");
    try testing.expectEqual(@as(usize, 1), interner.count());

    _ = try interner.intern("two");
    try testing.expectEqual(@as(usize, 2), interner.count());

    _ = try interner.intern("one"); // duplicate
    try testing.expectEqual(@as(usize, 2), interner.count());
}

test "StringInterner: lookup returns null for non-existent" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    _ = try interner.intern("exists");

    try testing.expect(interner.lookup("exists") != null);
    try testing.expect(interner.lookup("nonexistent") == null);
}

test "StringInterner: contains check" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    try testing.expect(!interner.contains("test"));

    _ = try interner.intern("test");

    try testing.expect(interner.contains("test"));
    try testing.expect(!interner.contains("other"));
}

test "StringInterner: get returns empty for null_id" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    try testing.expectEqualStrings("", interner.get(.null_id));
}

test "StringInterner: StringId null check" {
    try testing.expect(StringId.null_id.isNull());

    const non_null: StringId = @enumFromInt(0);
    try testing.expect(!non_null.isNull());
}

// ============================================================================
// NodeStore Memory Tests
// ============================================================================

test "NodeStore: no leaks on init/deinit" {
    var store = NodeStore.init(testing.allocator);
    store.deinit();
}

test "NodeStore: no leaks after adding expressions" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    // Add multiple expressions
    _ = try store.addExpression(.int_literal, .empty, .zero);
    _ = try store.addExpression(.int_literal, .empty, .zero);
    _ = try store.addExpression(.int_literal, .empty, .zero);
}

test "NodeStore: no leaks after adding statements" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    // Add statements
    _ = try store.addStatement(.return_stmt, .empty, .zero);
    _ = try store.addStatement(.expression_stmt, .empty, .zero);
}

test "NodeStore: extra data allocation" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    // Add extra data
    _ = try store.addExtraData(42);
    _ = try store.addExtraData(100);
    _ = try store.addExtraData(255);
}

// ============================================================================
// Lexer Memory Tests
// ============================================================================

test "Lexer: no leaks tokenizing empty input" {
    var lexer = Lexer.init("");
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1);
}

test "Lexer: no leaks tokenizing complex input" {
    const source =
        \\fn main() -> i32 {
        \\    let x = 42;
        \\    let y = "hello world";
        \\    return x + 1;
        \\}
    ;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len > 10);
}

test "Lexer: no leaks with many tokens" {
    // Generate a source with many tokens
    const source = "a b c d e f g h i j k l m n o p q r s t u v w x y z " ** 10;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 260);
}

// ============================================================================
// Parser Memory Tests
// ============================================================================

fn parseAndCleanup(allocator: std.mem.Allocator, source: []const u8) !void {
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(allocator);
    defer allocator.free(tokens);

    const store = try allocator.create(NodeStore);
    store.* = NodeStore.init(allocator);
    defer {
        store.deinit();
        allocator.destroy(store);
    }

    const strings = try allocator.create(StringInterner);
    strings.* = StringInterner.init(allocator);
    defer {
        strings.deinit();
        allocator.destroy(strings);
    }

    var parser = Parser.init(allocator, tokens, store, strings);
    defer parser.deinit();

    const result = parser.parse() catch return;
    var res = result;
    res.deinit(allocator);
}

test "Parser: no leaks parsing empty" {
    try parseAndCleanup(testing.allocator, "");
}

test "Parser: no leaks parsing function" {
    try parseAndCleanup(testing.allocator, "fn foo() {}");
}

test "Parser: no leaks parsing complex program" {
    const source =
        \\fn add(a: i32, b: i32) -> i32 {
        \\    return a + b;
        \\}
        \\
        \\fn main() -> i32 {
        \\    let result = add(1, 2);
        \\    return result;
        \\}
    ;
    try parseAndCleanup(testing.allocator, source);
}

test "Parser: no leaks on parse error" {
    // Invalid syntax should still clean up
    try parseAndCleanup(testing.allocator, "fn () { invalid }");
}

test "Parser: no leaks with many parse errors" {
    try parseAndCleanup(testing.allocator, "; ; ; let = ; fn () {}");
}

// ============================================================================
// Arena Allocator Tests
// ============================================================================

test "Arena: basic allocation and cleanup" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    _ = try allocator.alloc(u8, 100);
    _ = try allocator.alloc(u32, 50);
    _ = try allocator.alloc(u64, 25);
    // No explicit free needed - arena.deinit() handles it
}

test "Arena: many small allocations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        _ = try allocator.alloc(u8, 16);
    }
}

test "Arena: string duplication" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const strings = [_][]const u8{
        "hello",
        "world",
        "test",
        "longer string with more content",
    };

    for (strings) |s| {
        _ = try allocator.dupe(u8, s);
    }
}

// ============================================================================
// ArrayList Memory Tests
// ============================================================================

test "ArrayListUnmanaged: proper cleanup" {
    var list: std.ArrayListUnmanaged(u32) = .{};
    defer list.deinit(testing.allocator);

    try list.append(testing.allocator, 1);
    try list.append(testing.allocator, 2);
    try list.append(testing.allocator, 3);
}

test "ArrayListUnmanaged: toOwnedSlice transfers ownership" {
    var list: std.ArrayListUnmanaged(u32) = .{};

    try list.append(testing.allocator, 1);
    try list.append(testing.allocator, 2);

    const slice = try list.toOwnedSlice(testing.allocator);
    defer testing.allocator.free(slice);

    try testing.expectEqual(@as(usize, 2), slice.len);
}

// ============================================================================
// HashMap Memory Tests
// ============================================================================

test "StringHashMap: proper cleanup" {
    var map: std.StringHashMapUnmanaged(u32) = .{};
    defer map.deinit(testing.allocator);

    try map.put(testing.allocator, "one", 1);
    try map.put(testing.allocator, "two", 2);
    try map.put(testing.allocator, "three", 3);
}

test "AutoHashMap: proper cleanup" {
    var map = std.AutoHashMap(u32, []const u8).init(testing.allocator);
    defer map.deinit();

    try map.put(1, "one");
    try map.put(2, "two");
    try map.put(3, "three");
}
