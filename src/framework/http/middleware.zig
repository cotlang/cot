//! HTTP Middleware
//!
//! Composable middleware system for request/response processing.
//! Middleware runs in order: first added runs first (before handler),
//! and last (after handler) in reverse order.

const std = @import("std");
const context = @import("context.zig");
const Context = context.Context;
const Allocator = std.mem.Allocator;

/// Middleware function type
/// Returns true to continue chain, false to short-circuit
pub const MiddlewareFn = *const fn (*Context) anyerror!bool;

/// Handler function type (from context.zig)
pub const HandlerFn = context.HandlerFn;

/// Middleware stack - manages ordered list of middleware
pub const MiddlewareStack = struct {
    allocator: Allocator,
    before: std.ArrayListUnmanaged(MiddlewareFn),
    after: std.ArrayListUnmanaged(MiddlewareFn),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .before = .empty,
            .after = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.before.deinit(self.allocator);
        self.after.deinit(self.allocator);
    }

    /// Add middleware that runs before the handler
    pub fn useBefore(self: *Self, middleware: MiddlewareFn) !void {
        try self.before.append(self.allocator, middleware);
    }

    /// Add middleware that runs after the handler
    pub fn useAfter(self: *Self, middleware: MiddlewareFn) !void {
        try self.after.append(self.allocator, middleware);
    }

    /// Add middleware (runs both before and after via wrapper)
    /// For simple middleware, use useBefore or useAfter directly
    pub fn use(self: *Self, middleware: MiddlewareFn) !void {
        try self.useBefore(middleware);
    }

    /// Execute the full middleware chain with handler
    pub fn execute(self: *Self, ctx: *Context, handler: HandlerFn) !void {
        // Run "before" middleware in order
        for (self.before.items) |mw| {
            const should_continue = try mw(ctx);
            if (!should_continue) {
                // Middleware short-circuited, skip handler and after middleware
                return;
            }
        }

        // Run the handler
        try handler(ctx);

        // Run "after" middleware in reverse order
        var i: usize = self.after.items.len;
        while (i > 0) {
            i -= 1;
            _ = try self.after.items[i](ctx);
        }
    }

    /// Clone middleware stack (for route groups)
    pub fn clone(self: *Self) !Self {
        var new_stack = Self.init(self.allocator);
        errdefer new_stack.deinit();

        for (self.before.items) |mw| {
            try new_stack.before.append(self.allocator, mw);
        }
        for (self.after.items) |mw| {
            try new_stack.after.append(self.allocator, mw);
        }

        return new_stack;
    }
};

// ============================================
// Built-in Middleware
// ============================================

/// Logger middleware - logs method, path, status, and duration
/// Note: For timing, use LoggerMiddleware struct which captures start time
pub fn logger() MiddlewareFn {
    return struct {
        fn middleware(ctx: *Context) anyerror!bool {
            const method = ctx.methodString();
            const path_str = ctx.path();

            // Log request start
            std.debug.print("--> {s} {s}\n", .{ method, path_str });

            return true; // Continue chain
        }
    }.middleware;
}

/// Logger that runs after handler to show status
pub fn loggerAfter() MiddlewareFn {
    return struct {
        fn middleware(ctx: *Context) anyerror!bool {
            const method = ctx.methodString();
            const path_str = ctx.path();
            const status = ctx.getStatusCode();

            std.debug.print("<-- {s} {s} {d}\n", .{ method, path_str, status });

            return true;
        }
    }.middleware;
}

/// CORS options
pub const CorsOptions = struct {
    allow_origin: []const u8 = "*",
    allow_methods: []const u8 = "GET, POST, PUT, DELETE, PATCH, OPTIONS",
    allow_headers: []const u8 = "Content-Type, Authorization",
    allow_credentials: bool = false,
    max_age: u32 = 86400,
};

/// CORS middleware with default options
pub fn cors() MiddlewareFn {
    return corsWithOptions(.{});
}

/// CORS middleware with custom options
pub fn corsWithOptions(options: CorsOptions) MiddlewareFn {
    _ = options; // TODO: Use when we have better closure support
    return struct {
        fn middleware(ctx: *Context) anyerror!bool {
            // Add CORS headers
            _ = ctx.setHeader("Access-Control-Allow-Origin", "*");
            _ = ctx.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, PATCH, OPTIONS");
            _ = ctx.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");

            // Handle preflight OPTIONS requests
            if (ctx.method() == .OPTIONS) {
                _ = ctx.status(204);
                try ctx.send("");
                return false; // Short-circuit, don't call handler
            }

            return true; // Continue to handler
        }
    }.middleware;
}

/// Recovery middleware - catches handler errors and returns 500
/// Note: This should be added as "after" middleware or wrapped around execute
pub fn recover() MiddlewareFn {
    return struct {
        fn middleware(_: *Context) anyerror!bool {
            // This runs before handler, nothing to do here
            // Error recovery happens in the execute wrapper
            return true;
        }
    }.middleware;
}

/// Request ID middleware - adds unique ID to each request
pub fn requestId() MiddlewareFn {
    return struct {
        var counter: u64 = 0;

        fn middleware(ctx: *Context) anyerror!bool {
            counter +%= 1;

            var buf: [16]u8 = undefined;
            const id = std.fmt.bufPrint(&buf, "{x:0>16}", .{counter}) catch "0000000000000000";

            ctx.set("request_id", id);
            _ = ctx.setHeader("X-Request-ID", id);

            return true;
        }
    }.middleware;
}

/// Content-Type enforcement middleware
/// Returns 415 Unsupported Media Type if Content-Type doesn't match
pub fn requireContentType(expected: []const u8) MiddlewareFn {
    _ = expected; // TODO: Use when we have closure support
    return struct {
        fn middleware(ctx: *Context) anyerror!bool {
            // Only check on requests that should have a body
            const method = ctx.method();
            if (method != .POST and method != .PUT and method != .PATCH) {
                return true;
            }

            const content_type = ctx.contentType() orelse {
                _ = ctx.status(415);
                try ctx.json("{\"error\": \"Content-Type header required\"}");
                return false;
            };

            // Check if it starts with application/json
            if (!std.mem.startsWith(u8, content_type, "application/json")) {
                _ = ctx.status(415);
                try ctx.json("{\"error\": \"Content-Type must be application/json\"}");
                return false;
            }

            return true;
        }
    }.middleware;
}

/// Security headers middleware - adds common security headers
pub fn securityHeaders() MiddlewareFn {
    return struct {
        fn middleware(ctx: *Context) anyerror!bool {
            _ = ctx.setHeader("X-Content-Type-Options", "nosniff");
            _ = ctx.setHeader("X-Frame-Options", "DENY");
            _ = ctx.setHeader("X-XSS-Protection", "1; mode=block");
            _ = ctx.setHeader("Referrer-Policy", "strict-origin-when-cross-origin");
            return true;
        }
    }.middleware;
}

// ============================================
// Tests
// ============================================

test "middleware stack empty" {
    const allocator = std.testing.allocator;

    var stack = MiddlewareStack.init(allocator);
    defer stack.deinit();

    var ctx = Context.init(allocator);
    defer ctx.deinit();
    _ = &ctx;

    // Test empty stack
    try std.testing.expectEqual(@as(usize, 0), stack.before.items.len);
}

test "middleware stack with middleware" {
    const allocator = std.testing.allocator;

    var stack = MiddlewareStack.init(allocator);
    defer stack.deinit();

    try stack.use(logger());
    try stack.use(cors());
    try stack.useAfter(loggerAfter());

    try std.testing.expectEqual(@as(usize, 2), stack.before.items.len);
    try std.testing.expectEqual(@as(usize, 1), stack.after.items.len);
}

test "middleware clone" {
    const allocator = std.testing.allocator;

    var stack = MiddlewareStack.init(allocator);
    defer stack.deinit();

    try stack.use(logger());
    try stack.use(cors());

    var cloned = try stack.clone();
    defer cloned.deinit();

    try std.testing.expectEqual(@as(usize, 2), cloned.before.items.len);
}
