//! HTTP Router
//!
//! Path-based routing with support for:
//! - Static routes: /api/products
//! - Dynamic routes: /api/products/:id
//! - Wildcard routes: /api/*
//! - File-based routing from api/ directory

const std = @import("std");
const Allocator = std.mem.Allocator;
const server = @import("server.zig");

/// Route match result
pub const RouteMatch = struct {
    handler: server.HandlerFn,
    params: std.StringHashMap([]const u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator, handler: server.HandlerFn) RouteMatch {
        return .{
            .handler = handler,
            .params = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RouteMatch) void {
        self.params.deinit();
    }
};

/// Route segment type
const SegmentType = enum {
    static,   // Exact match: "products"
    param,    // Dynamic match: ":id" or "[id]"
    wildcard, // Matches anything: "*"
};

/// A single route segment
const Segment = struct {
    segment_type: SegmentType,
    value: []const u8, // The segment text or param name
};

/// A registered route
const Route = struct {
    method: server.Request.Method,
    pattern: []const u8,
    segments: []const Segment,
    handler: server.HandlerFn,
};

/// HTTP Router
pub const Router = struct {
    allocator: Allocator,
    routes: std.ArrayList(Route),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .routes = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.routes.items) |route| {
            self.allocator.free(route.segments);
        }
        self.routes.deinit(self.allocator);
    }

    /// Add a route
    pub fn add(self: *Self, method: server.Request.Method, pattern: []const u8, handler: server.HandlerFn) !void {
        const segments = try self.parsePattern(pattern);

        try self.routes.append(self.allocator, .{
            .method = method,
            .pattern = pattern,
            .segments = segments,
            .handler = handler,
        });
    }

    /// Parse a route pattern into segments
    fn parsePattern(self: *Self, pattern: []const u8) ![]const Segment {
        var segments: std.ArrayList(Segment) = .{};
        errdefer segments.deinit(self.allocator);

        // Skip leading slash
        var path = pattern;
        if (path.len > 0 and path[0] == '/') {
            path = path[1..];
        }

        // Split by '/'
        var iter = std.mem.splitScalar(u8, path, '/');
        while (iter.next()) |part| {
            if (part.len == 0) continue;

            const segment: Segment = blk: {
                if (part[0] == ':') {
                    // Dynamic param: :id
                    break :blk .{ .segment_type = .param, .value = part[1..] };
                } else if (part[0] == '[' and part[part.len - 1] == ']') {
                    // Next.js style: [id]
                    break :blk .{ .segment_type = .param, .value = part[1 .. part.len - 1] };
                } else if (std.mem.eql(u8, part, "*")) {
                    // Wildcard
                    break :blk .{ .segment_type = .wildcard, .value = "" };
                } else {
                    // Static segment
                    break :blk .{ .segment_type = .static, .value = part };
                }
            };

            try segments.append(self.allocator, segment);
        }

        return try segments.toOwnedSlice(self.allocator);
    }

    /// Match a path against registered routes
    pub fn match(self: *Self, method: server.Request.Method, path: []const u8) ?RouteMatch {
        for (self.routes.items) |route| {
            if (route.method != method) continue;

            if (self.matchRoute(route, path)) |result| {
                return result;
            }
        }
        return null;
    }

    /// Match a single route
    fn matchRoute(self: *Self, route: Route, path: []const u8) ?RouteMatch {
        var result = RouteMatch.init(self.allocator, route.handler);

        // Skip leading slash
        var p = path;
        if (p.len > 0 and p[0] == '/') {
            p = p[1..];
        }

        // Split path into parts
        var path_iter = std.mem.splitScalar(u8, p, '/');
        var segment_idx: usize = 0;

        while (path_iter.next()) |part| {
            if (part.len == 0) continue;

            // Check if we've exhausted segments
            if (segment_idx >= route.segments.len) {
                result.deinit();
                return null;
            }

            const segment = route.segments[segment_idx];
            segment_idx += 1;

            switch (segment.segment_type) {
                .static => {
                    if (!std.mem.eql(u8, segment.value, part)) {
                        result.deinit();
                        return null;
                    }
                },
                .param => {
                    // Store the parameter
                    result.params.put(segment.value, part) catch {
                        result.deinit();
                        return null;
                    };
                },
                .wildcard => {
                    // Wildcard matches rest of path
                    return result;
                },
            }
        }

        // Check we matched all segments (unless last is wildcard)
        if (segment_idx != route.segments.len) {
            if (segment_idx == route.segments.len - 1) {
                if (route.segments[segment_idx].segment_type == .wildcard) {
                    return result;
                }
            }
            result.deinit();
            return null;
        }

        return result;
    }

    /// Print all registered routes (for debugging)
    pub fn printRoutes(self: *const Self, writer: anytype) !void {
        try writer.writeAll("Registered Routes:\n");
        for (self.routes.items) |route| {
            try writer.print("  {s} {s}\n", .{ @tagName(route.method), route.pattern });
        }
    }
};

/// Build routes from file-based API directory
/// Converts file paths to routes:
///   api/products.cot -> GET/POST /api/products
///   api/products/[id].cot -> GET/PUT/DELETE /api/products/:id
pub const FileRouter = struct {
    allocator: Allocator,
    routes: std.ArrayList(FileRoute),

    const FileRoute = struct {
        path: []const u8,
        route: []const u8,
        methods: []const server.Request.Method,
    };

    pub fn init(allocator: Allocator) FileRouter {
        return .{
            .allocator = allocator,
            .routes = .{},
        };
    }

    pub fn deinit(self: *FileRouter) void {
        for (self.routes.items) |route| {
            self.allocator.free(route.path);
            self.allocator.free(route.route);
        }
        self.routes.deinit(self.allocator);
    }

    /// Discover API routes from a directory
    pub fn discover(self: *FileRouter, api_dir: []const u8) !void {
        var dir = std.fs.cwd().openDir(api_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return;
            return err;
        };
        defer dir.close();

        try self.scanDirectory(dir, api_dir, "");
    }

    fn scanDirectory(self: *FileRouter, dir: std.fs.Dir, base_path: []const u8, relative_path: []const u8) !void {
        var it = dir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .directory) {
                // Skip hidden directories
                if (entry.name[0] == '.') continue;

                const sub_rel = if (relative_path.len > 0)
                    try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ relative_path, entry.name })
                else
                    try self.allocator.dupe(u8, entry.name);
                defer self.allocator.free(sub_rel);

                const sub_path = try std.fs.path.join(self.allocator, &.{ base_path, entry.name });
                defer self.allocator.free(sub_path);

                var sub_dir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer sub_dir.close();

                try self.scanDirectory(sub_dir, sub_path, sub_rel);
            } else if (entry.kind == .file) {
                // Check for .cot extension
                if (!std.mem.endsWith(u8, entry.name, ".cot")) continue;

                // Get base name without extension
                const name_end = entry.name.len - 4;
                const base_name = entry.name[0..name_end];

                // Build route path
                const route_path = if (relative_path.len > 0)
                    try std.fmt.allocPrint(self.allocator, "/api/{s}/{s}", .{ relative_path, base_name })
                else
                    try std.fmt.allocPrint(self.allocator, "/api/{s}", .{base_name});

                // Convert [param] to :param in route
                const normalized_route = try self.normalizeRoute(route_path);
                defer self.allocator.free(route_path);

                // Build file path
                const file_path = try std.fs.path.join(self.allocator, &.{ base_path, entry.name });

                try self.routes.append(self.allocator, .{
                    .path = file_path,
                    .route = normalized_route,
                    .methods = &[_]server.Request.Method{ .GET, .POST, .PUT, .DELETE },
                });
            }
        }
    }

    /// Convert [param] to :param
    fn normalizeRoute(self: *FileRouter, route: []const u8) ![]const u8 {
        var result: std.ArrayList(u8) = .{};
        errdefer result.deinit(self.allocator);

        var i: usize = 0;
        while (i < route.len) {
            if (route[i] == '[') {
                try result.append(self.allocator, ':');
                i += 1;
                while (i < route.len and route[i] != ']') {
                    try result.append(self.allocator, route[i]);
                    i += 1;
                }
                if (i < route.len) i += 1; // Skip ']'
            } else {
                try result.append(self.allocator, route[i]);
                i += 1;
            }
        }

        return try result.toOwnedSlice(self.allocator);
    }

    /// Print discovered routes
    pub fn printRoutes(self: *const FileRouter, writer: anytype) !void {
        try writer.writeAll("API Routes:\n");
        for (self.routes.items) |route| {
            try writer.print("  {s} -> {s}\n", .{ route.route, route.path });
        }
    }
};

// Tests
test "router static routes" {
    const allocator = std.testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    const handler = struct {
        fn h(_: *server.Request, _: *server.Response) !void {}
    }.h;

    try r.add(.GET, "/api/products", handler);

    const match_result = r.match(.GET, "/api/products");
    try std.testing.expect(match_result != null);

    if (match_result) |*m| {
        var result = m.*;
        defer result.deinit();
    }
}

test "router param routes" {
    const allocator = std.testing.allocator;
    var r = Router.init(allocator);
    defer r.deinit();

    const handler = struct {
        fn h(_: *server.Request, _: *server.Response) !void {}
    }.h;

    try r.add(.GET, "/api/products/:id", handler);

    var match_result = r.match(.GET, "/api/products/123");
    try std.testing.expect(match_result != null);

    if (match_result) |*m| {
        try std.testing.expectEqualStrings("123", m.params.get("id").?);
        m.deinit();
    }
}

// Pull in comprehensive HTTP tests
test {
    _ = @import("http_test.zig");
}
