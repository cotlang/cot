//! Stress Tests
//!
//! Tests that push the system to its limits to ensure stability
//! under extreme conditions. These tests verify the system handles
//! resource pressure gracefully.

const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig").Lexer;
const Parser = @import("../parser/parser.zig").Parser;
const StringInterner = @import("../base/mod.zig").StringInterner;
const NodeStore = @import("../ast/mod.zig").NodeStore;

// ============================================================================
// Large Input Stress Tests
// ============================================================================

test "stress: lexer with 100K character source" {
    // Create a very large source file
    const chunk = "let variable_name = 12345; ";
    const source = chunk ** 4000; // ~100K characters

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 20000);
}

test "stress: lexer with 50K lines" {
    // Many short lines
    const line = "x\n";
    const source = line ** 50000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 50000);
}

test "stress: lexer with very long line" {
    // Single line with many tokens
    const token = "identifier ";
    const source = token ** 10000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 10000);
}

// ============================================================================
// Deep Nesting Stress Tests
// ============================================================================

test "stress: deeply nested parentheses" {
    // Create deeply nested expression within parser limits
    const depth = 64; // Stay under MAX_NESTING_DEPTH of 128
    var buf: [256]u8 = undefined;
    var pos: usize = 0;

    // Build nested parentheses
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        buf[pos] = '(';
        pos += 1;
    }
    buf[pos] = '1';
    pos += 1;
    i = 0;
    while (i < depth) : (i += 1) {
        buf[pos] = ')';
        pos += 1;
    }

    const source = buf[0..pos];
    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= depth * 2 + 1);
}

test "stress: many sequential blocks" {
    const block = "{ let x = 1; } ";
    const source = block ** 1000;

    var lexer = Lexer.init(source);
    const tokens = try lexer.tokenize(testing.allocator);
    defer testing.allocator.free(tokens);

    try testing.expect(tokens.len >= 6000);
}

// ============================================================================
// StringInterner Stress Tests
// ============================================================================

test "stress: interner with 50K unique strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    var i: usize = 0;
    while (i < 50000) : (i += 1) {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "str_{d}", .{i}) catch unreachable;
        _ = try interner.intern(str);
    }

    try testing.expectEqual(@as(usize, 50000), interner.count());
}

test "stress: interner repeated intern/lookup cycle" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    // Create base strings
    var i: usize = 0;
    while (i < 100) : (i += 1) {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "base_{d}", .{i}) catch unreachable;
        _ = try interner.intern(str);
    }

    // Repeatedly look up and intern
    i = 0;
    while (i < 100000) : (i += 1) {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "base_{d}", .{i % 100}) catch unreachable;
        _ = try interner.intern(str);
    }

    try testing.expectEqual(@as(usize, 100), interner.count());
}

test "stress: interner with binary-like strings" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    // Strings that differ only in last byte
    var i: u8 = 0;
    while (i < 255) : (i += 1) {
        var buf: [16]u8 = undefined;
        @memset(&buf, 'x');
        buf[15] = i;
        _ = try interner.intern(&buf);
    }

    try testing.expectEqual(@as(usize, 255), interner.count());
}

// ============================================================================
// NodeStore Stress Tests
// ============================================================================

test "stress: NodeStore with 100K expressions" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 100000) : (i += 1) {
        _ = try store.addExpression(.int_literal, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 100000), store.expressionCount());
}

test "stress: NodeStore with 100K statements" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: usize = 0;
    while (i < 100000) : (i += 1) {
        _ = try store.addStatement(.expression_stmt, .empty, .zero);
    }

    try testing.expectEqual(@as(usize, 100000), store.statementCount());
}

test "stress: NodeStore with 500K extra data entries" {
    var store = NodeStore.init(testing.allocator);
    defer store.deinit();

    var i: u32 = 0;
    while (i < 500000) : (i += 1) {
        _ = try store.addExtraData(i);
    }
}

// ============================================================================
// Repeated Operation Stress Tests
// ============================================================================

test "stress: repeated lexer init/deinit" {
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        var lexer = Lexer.init("let x = 42;");
        const tokens = try lexer.tokenize(testing.allocator);
        testing.allocator.free(tokens);
    }
}

test "stress: repeated interner init/deinit" {
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        var interner = StringInterner.init(testing.allocator);
        _ = try interner.intern("test");
        _ = try interner.intern("hello");
        _ = try interner.intern("world");
        interner.deinit();
    }
}

test "stress: repeated NodeStore init/deinit" {
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        var store = NodeStore.init(testing.allocator);
        _ = try store.addExpression(.int_literal, .empty, .zero);
        _ = try store.addStatement(.expression_stmt, .empty, .zero);
        store.deinit();
    }
}

// ============================================================================
// Mixed Workload Stress Tests
// ============================================================================

test "stress: mixed lexer and interner workload" {
    var interner = StringInterner.init(testing.allocator);
    defer interner.deinit();

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        var buf: [64]u8 = undefined;
        const source = std.fmt.bufPrint(&buf, "let var{d} = {d};", .{ i, i * 2 }) catch unreachable;

        var lexer = Lexer.init(source);
        const tokens = try lexer.tokenize(testing.allocator);
        defer testing.allocator.free(tokens);

        for (tokens) |tok| {
            if (tok.lexeme.len > 0) {
                _ = try interner.intern(tok.lexeme);
            }
        }
    }
}

test "stress: rapid ArrayList growth and shrink" {
    var list: std.ArrayListUnmanaged(u32) = .{};
    defer list.deinit(testing.allocator);

    // Grow
    var i: u32 = 0;
    while (i < 50000) : (i += 1) {
        try list.append(testing.allocator, i);
    }

    // Shrink by clearing
    list.clearRetainingCapacity();

    // Grow again
    i = 0;
    while (i < 50000) : (i += 1) {
        try list.append(testing.allocator, i);
    }

    try testing.expectEqual(@as(usize, 50000), list.items.len);
}

test "stress: HashMap with collision-prone keys" {
    var map = std.AutoHashMap(u32, u32).init(testing.allocator);
    defer map.deinit();

    // Insert many values
    var i: u32 = 0;
    while (i < 50000) : (i += 1) {
        try map.put(i, i);
    }

    // Verify all present
    i = 0;
    while (i < 50000) : (i += 1) {
        try testing.expect(map.get(i) != null);
    }
}
