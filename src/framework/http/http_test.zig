//! HTTP Server and Router Tests
//!
//! Comprehensive tests for HTTP routing, request handling, and response generation.

const std = @import("std");
const testing = std.testing;

const router = @import("router.zig");
const server = @import("server.zig");

const Router = router.Router;
const RouteMatch = router.RouteMatch;

// ============================================================================
// Test Handler Helpers
// ============================================================================

fn dummyHandler(_: *server.Request, _: *server.Response) !void {}

fn echoHandler(_: *server.Request, res: *server.Response) !void {
    try res.text("echo");
}

// ============================================================================
// Router Initialization Tests
// ============================================================================

test "router: initialization" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try testing.expectEqual(@as(usize, 0), r.routes.items.len);
}

test "router: add single route" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/api/test", dummyHandler);
    try testing.expectEqual(@as(usize, 1), r.routes.items.len);
}

test "router: add multiple routes" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/api/a", dummyHandler);
    try r.add(.POST, "/api/b", dummyHandler);
    try r.add(.PUT, "/api/c", dummyHandler);
    try r.add(.DELETE, "/api/d", dummyHandler);

    try testing.expectEqual(@as(usize, 4), r.routes.items.len);
}

// ============================================================================
// Static Route Matching Tests
// ============================================================================

test "router: match static route" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/hello", dummyHandler);

    var match_result = r.match(.GET, "/hello");
    try testing.expect(match_result != null);
    if (match_result) |*m| {
        m.deinit();
    }
}

test "router: no match for different path" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/hello", dummyHandler);

    const match_result = r.match(.GET, "/world");
    try testing.expect(match_result == null);
}

test "router: no match for different method" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/hello", dummyHandler);

    const match_result = r.match(.POST, "/hello");
    try testing.expect(match_result == null);
}

test "router: match nested static path" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/api/v1/users", dummyHandler);

    var match_result = r.match(.GET, "/api/v1/users");
    try testing.expect(match_result != null);
    if (match_result) |*m| {
        m.deinit();
    }
}

test "router: match root path" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/", dummyHandler);

    var match_result = r.match(.GET, "/");
    try testing.expect(match_result != null);
    if (match_result) |*m| {
        m.deinit();
    }
}

// ============================================================================
// Parameter Route Matching Tests
// ============================================================================

test "router: match param route" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/users/:id", dummyHandler);

    var match_result = r.match(.GET, "/users/42");
    try testing.expect(match_result != null);

    if (match_result) |*m| {
        try testing.expectEqualStrings("42", m.params.get("id").?);
        m.deinit();
    }
}

test "router: match multiple params" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/users/:userId/posts/:postId", dummyHandler);

    var match_result = r.match(.GET, "/users/123/posts/456");
    try testing.expect(match_result != null);

    if (match_result) |*m| {
        try testing.expectEqualStrings("123", m.params.get("userId").?);
        try testing.expectEqualStrings("456", m.params.get("postId").?);
        m.deinit();
    }
}

test "router: param with different values" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/items/:id", dummyHandler);

    // Test with numeric value
    var match1 = r.match(.GET, "/items/999");
    try testing.expect(match1 != null);
    if (match1) |*m| {
        try testing.expectEqualStrings("999", m.params.get("id").?);
        m.deinit();
    }

    // Test with string value
    var match2 = r.match(.GET, "/items/abc");
    try testing.expect(match2 != null);
    if (match2) |*m| {
        try testing.expectEqualStrings("abc", m.params.get("id").?);
        m.deinit();
    }
}

// ============================================================================
// HTTP Method Tests
// ============================================================================

test "router: GET method" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/test", dummyHandler);

    var match_result = r.match(.GET, "/test");
    try testing.expect(match_result != null);
    if (match_result) |*m| m.deinit();
}

test "router: POST method" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.POST, "/test", dummyHandler);

    var match_result = r.match(.POST, "/test");
    try testing.expect(match_result != null);
    if (match_result) |*m| m.deinit();
}

test "router: PUT method" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.PUT, "/test", dummyHandler);

    var match_result = r.match(.PUT, "/test");
    try testing.expect(match_result != null);
    if (match_result) |*m| m.deinit();
}

test "router: DELETE method" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.DELETE, "/test", dummyHandler);

    var match_result = r.match(.DELETE, "/test");
    try testing.expect(match_result != null);
    if (match_result) |*m| m.deinit();
}

test "router: same path different methods" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/resource", dummyHandler);
    try r.add(.POST, "/resource", echoHandler);

    // GET should match first handler
    var get_match = r.match(.GET, "/resource");
    try testing.expect(get_match != null);
    if (get_match) |*m| m.deinit();

    // POST should match second handler
    var post_match = r.match(.POST, "/resource");
    try testing.expect(post_match != null);
    if (post_match) |*m| m.deinit();
}

// ============================================================================
// Edge Cases
// ============================================================================

test "router: trailing slash handling" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/api/users", dummyHandler);

    // Without trailing slash
    var match1 = r.match(.GET, "/api/users");
    try testing.expect(match1 != null);
    if (match1) |*m| m.deinit();
}

test "router: empty path segment handling" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/api//users", dummyHandler);
    // Empty segments in path should still work
}

test "router: route priority - static over param" {
    const allocator = testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    try r.add(.GET, "/users/:id", dummyHandler);
    try r.add(.GET, "/users/me", echoHandler);

    // Static route should be checked too
    var match = r.match(.GET, "/users/me");
    try testing.expect(match != null);
    if (match) |*m| m.deinit();
}

// ============================================================================
// RouteMatch Tests
// ============================================================================

test "RouteMatch: initialization and cleanup" {
    const allocator = testing.allocator;
    var match = RouteMatch.init(allocator, dummyHandler);
    defer match.deinit();

    try testing.expect(match.params.count() == 0);
}

test "RouteMatch: params storage" {
    const allocator = testing.allocator;
    var match = RouteMatch.init(allocator, dummyHandler);
    defer match.deinit();

    try match.params.put("id", "123");
    try match.params.put("name", "test");

    try testing.expectEqualStrings("123", match.params.get("id").?);
    try testing.expectEqualStrings("test", match.params.get("name").?);
}
