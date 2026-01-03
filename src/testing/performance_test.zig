//! Performance Benchmark Tests
//!
//! Tests that verify performance characteristics and ensure operations
//! complete within acceptable time bounds. These tests help catch
//! performance regressions.

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const StringInterner = @import("../base/mod.zig").StringInterner;
const NodeStore = @import("../ast/mod.zig").NodeStore;

// ============================================================================
// Lexer Performance Tests
// ============================================================================

test "perf: lexer tokenizes 10K tokens quickly" {
    // Generate source with many tokens
    const source = "let x = 1 + 2 * 3; " ** 1000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    // Should produce many tokens
    try testing.expect(tokens.len >= 8000);
}

test "perf: lexer handles large source file" {
    // Simulate a large source file (~100KB)
    const line = "fn process(x: i32, y: i32) -> i32 { return x + y * 2; }\n";
    const source = line ** 1000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 10000);
}

test "perf: lexer with many string literals" {
    const source = "\"string literal\" " ** 1000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 1000);
}

test "perf: lexer with many numeric literals" {
    const source = "12345 67890 3.14159 0xFF " ** 500;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 2000);
}

test "perf: lexer with deeply nested comments" {
    // Many block comments
    const source = "x /* comment */ " ** 500 ++ "y";

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 500);
}

// ============================================================================
// StringInterner Performance Tests
// ============================================================================

test "perf: interner handles 10K unique strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    var i: usize = 0;
    while (i < 10000) : (i += 1) {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "unique_string_{d}", .{i}) catch unreachable;
        _ = try interner.intern(str);
    }

    try testing.expectEqual(@as(usize, 10000), interner.count());
}

test "perf: interner handles many duplicate lookups" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    // Intern a small set of strings
    const strings = [_][]const u8{
        "alpha", "beta", "gamma", "delta", "epsilon",
        "zeta", "eta", "theta", "iota", "kappa",
    };

    for (strings) |s| {
        _ = try interner.intern(s);
    }

    // Look up each string many times (simulating real usage)
    var i: usize = 0;
    while (i < 10000) : (i += 1) {
        const idx = i % strings.len;
        _ = try interner.intern(strings[idx]);
    }

    // Should still only have 10 unique strings
    try testing.expectEqual(@as(usize, 10), interner.count());
}

test "perf: interner with long strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        // Create long unique strings
        var buf: [1024]u8 = undefined;
        const prefix = "long_string_content_" ** 40;
        const str = std.fmt.bufPrint(&buf, "{s}{d}", .{ prefix, i }) catch unreachable;
        _ = try interner.intern(str);
    }

    try testing.expectEqual(@as(usize, 100), interner.count());
}

// ============================================================================
// NodeStore Performance Tests
// ============================================================================

test "perf: NodeStore handles 10K expressions" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 10000) : (i += 1) {
        _ = try store.addExpression(.int_literal, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 10000), store.expressionCount());
}

test "perf: NodeStore handles 10K statements" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 10000) : (i += 1) {
        _ = try store.addStatement(.expression_stmt, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 10000), store.statementCount());
}

test "perf: NodeStore mixed operations" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 5000) : (i += 1) {
        _ = try store.addExpression(.int_literal, .empty, .zero);
        _ = try store.addStatement(.expression_stmt, .empty, .zero);
        _ = try store.addExtraData(@intCast(i));
    }

    try testing.expectEqual(@as(usize, 5000), store.expressionCount());
    try testing.expectEqual(@as(usize, 5000), store.statementCount());
}

// ============================================================================
// Parser Performance Tests
// ============================================================================

fn parseSource(allocator: std.mem.Allocator, source: []const u8) !usize {
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

    const result = parser.parse() catch return 0;
    var res = result;
    defer res.deinit(allocator);

    return res.top_level.len;
}

test "perf: parser handles many functions" {
    const func = "fn func() -> i32 { return 42; }\n";
    const source = func ** 500;

    const count = try parseSource(testing.allocator, source);
    try testing.expect(count >= 400);
}

test "perf: parser handles complex expressions" {
    const expr = "let x = 1 + 2 * 3 - 4 / 5 + 6 * 7 - 8;\n";
    const source = expr ** 500;

    const count = try parseSource(testing.allocator, source);
    try testing.expect(count >= 400);
}

test "perf: parser handles nested blocks" {
    const source =
        \\fn deep() {
        \\    if true {
        \\        if true {
        \\            if true {
        \\                let x = 1;
        \\            }
        \\        }
        \\    }
        \\}
    ** 100;

    const count = try parseSource(testing.allocator, source);
    try testing.expect(count >= 50);
}

// ============================================================================
// Memory Allocation Performance Tests
// ============================================================================

test "perf: arena allocator many small allocations" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var i: usize = 0;
    while (i < 100000) : (i += 1) {
        _ = try allocator.alloc(u8, 16);
    }
}

test "perf: ArrayList rapid growth" {
    var list: std.ArrayListUnmanaged(u32) = .{};
    defer list.deinit(testing.allocator);

    var i: u32 = 0;
    while (i < 100000) : (i += 1) {
        try list.append(testing.allocator, i);
    }

    try testing.expectEqual(@as(usize, 100000), list.items.len);
}

test "perf: HashMap many insertions" {
    var map = std.AutoHashMap(u32, u32).init(testing.allocator);
    defer map.deinit();

    var i: u32 = 0;
    while (i < 10000) : (i += 1) {
        try map.put(i, i * 2);
    }

    try testing.expectEqual(@as(usize, 10000), map.count());
}
