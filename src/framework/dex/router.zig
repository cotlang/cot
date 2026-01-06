//! Dex File-Based Router
//!
//! Implements Next.js-style file-based routing with support for:
//! - Static routes: `pages/about.dex` → `/about`
//! - Dynamic segments: `pages/blog/[slug].dex` → `/blog/:slug`
//! - Catch-all routes: `pages/docs/[...path].dex` → `/docs/*`
//! - Optional catch-all: `pages/[[...path]].dex` → `/*` (optional)
//!
//! Route priority (most specific first):
//! 1. Exact static match
//! 2. Dynamic segment
//! 3. Catch-all
//! 4. Optional catch-all

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Type of route segment
pub const SegmentType = enum {
    /// Static segment: `about`
    static,
    /// Dynamic segment: `[slug]`
    dynamic,
    /// Catch-all segment: `[...path]`
    catch_all,
    /// Optional catch-all: `[[...path]]`
    optional_catch_all,
};

/// A single route segment
pub const Segment = struct {
    /// The segment type
    segment_type: SegmentType,
    /// The raw segment string (e.g., "about", "[slug]", "[...path]")
    raw: []const u8,
    /// The parameter name (for dynamic/catch-all, null for static)
    param_name: ?[]const u8,

    /// Parse a segment string into a Segment
    pub fn parse(raw: []const u8) Segment {
        // Optional catch-all: [[...name]]
        if (raw.len > 6 and
            raw[0] == '[' and raw[1] == '[' and
            raw[2] == '.' and raw[3] == '.' and raw[4] == '.' and
            raw[raw.len - 1] == ']' and raw[raw.len - 2] == ']')
        {
            return .{
                .segment_type = .optional_catch_all,
                .raw = raw,
                .param_name = raw[5 .. raw.len - 2],
            };
        }

        // Catch-all: [...name]
        if (raw.len > 5 and
            raw[0] == '[' and
            raw[1] == '.' and raw[2] == '.' and raw[3] == '.' and
            raw[raw.len - 1] == ']')
        {
            return .{
                .segment_type = .catch_all,
                .raw = raw,
                .param_name = raw[4 .. raw.len - 1],
            };
        }

        // Dynamic: [name]
        if (raw.len > 2 and raw[0] == '[' and raw[raw.len - 1] == ']') {
            return .{
                .segment_type = .dynamic,
                .raw = raw,
                .param_name = raw[1 .. raw.len - 1],
            };
        }

        // Static
        return .{
            .segment_type = .static,
            .raw = raw,
            .param_name = null,
        };
    }
};

/// A parsed route
pub const Route = struct {
    /// The URL pattern (e.g., "/blog/:slug")
    pattern: []const u8,
    /// The file path relative to pages dir
    file_path: []const u8,
    /// Parsed segments
    segments: []const Segment,
    /// Priority for matching (lower = higher priority)
    priority: u32,
    /// Whether this is an index route
    is_index: bool,

    /// Calculate route priority based on segment types
    /// Lower number = higher priority
    fn calculatePriority(segments: []const Segment) u32 {
        var priority: u32 = 0;
        for (segments) |seg| {
            priority += switch (seg.segment_type) {
                .static => 0,
                .dynamic => 100,
                .catch_all => 1000,
                .optional_catch_all => 10000,
            };
        }
        return priority;
    }
};

/// Match result with extracted parameters
pub const MatchResult = struct {
    route: *const Route,
    params: std.StringHashMap([]const u8),

    pub fn deinit(self: *MatchResult) void {
        self.params.deinit();
    }
};

/// Redirect definition
pub const Redirect = struct {
    /// Source path (can include patterns)
    source: []const u8,
    /// Destination path
    destination: []const u8,
    /// HTTP status code (301 = permanent, 302 = temporary)
    status: u16 = 302,
    /// Whether source is a regex pattern
    is_pattern: bool = false,
};

/// File-based router
pub const Router = struct {
    allocator: Allocator,
    pages_dir: []const u8,
    routes: std.ArrayListUnmanaged(Route),
    /// Lookup table for fast static route matching
    static_routes: std.StringHashMap(*const Route),
    /// Redirect rules
    redirects: std.ArrayListUnmanaged(Redirect),

    const Self = @This();

    pub fn init(allocator: Allocator, pages_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .pages_dir = pages_dir,
            .routes = .empty,
            .static_routes = std.StringHashMap(*const Route).init(allocator),
            .redirects = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free allocated route data
        for (self.routes.items) |route| {
            self.allocator.free(route.pattern);
            self.allocator.free(route.file_path);
            self.allocator.free(route.segments);
        }
        self.routes.deinit(self.allocator);
        self.static_routes.deinit();

        // Free redirect data
        for (self.redirects.items) |redirect| {
            self.allocator.free(redirect.source);
            self.allocator.free(redirect.destination);
        }
        self.redirects.deinit(self.allocator);
    }

    /// Discover all routes in the pages directory
    pub fn discover(self: *Self) !void {
        // Clear existing routes
        for (self.routes.items) |route| {
            self.allocator.free(route.pattern);
            self.allocator.free(route.file_path);
            self.allocator.free(route.segments);
        }
        self.routes.clearRetainingCapacity();
        self.static_routes.clearRetainingCapacity();

        // Clear existing redirects
        for (self.redirects.items) |redirect| {
            self.allocator.free(redirect.source);
            self.allocator.free(redirect.destination);
        }
        self.redirects.clearRetainingCapacity();

        // Recursively scan pages directory
        try self.scanDirectory("");

        // Load redirects from _redirects.json
        self.loadRedirects() catch {};

        // Sort routes by priority (lower first)
        std.mem.sort(Route, self.routes.items, {}, struct {
            fn lessThan(_: void, a: Route, b: Route) bool {
                return a.priority < b.priority;
            }
        }.lessThan);

        // Build static route lookup
        for (self.routes.items) |*route| {
            if (self.isStaticRoute(route)) {
                try self.static_routes.put(route.pattern, route);
            }
        }
    }

    /// Check if a route is entirely static (no dynamic segments)
    fn isStaticRoute(self: *Self, route: *const Route) bool {
        _ = self;
        for (route.segments) |seg| {
            if (seg.segment_type != .static) return false;
        }
        return true;
    }

    /// Recursively scan a directory for .dex files
    fn scanDirectory(self: *Self, relative_path: []const u8) !void {
        const full_path = if (relative_path.len == 0)
            try self.allocator.dupe(u8, self.pages_dir)
        else
            try std.fs.path.join(self.allocator, &.{ self.pages_dir, relative_path });
        defer self.allocator.free(full_path);

        var dir = std.fs.cwd().openDir(full_path, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return;
            return err;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            // Skip hidden files and special files
            if (entry.name[0] == '.' or entry.name[0] == '_') continue;

            const entry_relative = if (relative_path.len == 0)
                try self.allocator.dupe(u8, entry.name)
            else
                try std.fs.path.join(self.allocator, &.{ relative_path, entry.name });
            defer self.allocator.free(entry_relative);

            switch (entry.kind) {
                .directory => {
                    try self.scanDirectory(entry_relative);
                },
                .file => {
                    if (std.mem.endsWith(u8, entry.name, ".dex")) {
                        try self.addRoute(entry_relative);
                    }
                },
                else => {},
            }
        }
    }

    /// Add a route from a file path
    fn addRoute(self: *Self, file_path: []const u8) !void {
        // Remove .dex extension
        const without_ext = file_path[0 .. file_path.len - 4];

        // Parse segments
        var segments_list: std.ArrayListUnmanaged(Segment) = .empty;
        defer segments_list.deinit(self.allocator);

        var path_iter = std.mem.splitScalar(u8, without_ext, '/');
        var is_index = false;

        while (path_iter.next()) |part| {
            if (std.mem.eql(u8, part, "index")) {
                is_index = true;
                continue; // Don't add "index" as a segment
            }
            try segments_list.append(self.allocator, Segment.parse(part));
        }

        const segments = try segments_list.toOwnedSlice(self.allocator);

        // Build URL pattern
        var pattern_buf: std.ArrayListUnmanaged(u8) = .empty;
        defer pattern_buf.deinit(self.allocator);

        try pattern_buf.append(self.allocator, '/');
        for (segments, 0..) |seg, i| {
            if (i > 0) try pattern_buf.append(self.allocator, '/');
            switch (seg.segment_type) {
                .static => try pattern_buf.appendSlice(self.allocator, seg.raw),
                .dynamic => {
                    try pattern_buf.append(self.allocator, ':');
                    try pattern_buf.appendSlice(self.allocator, seg.param_name.?);
                },
                .catch_all, .optional_catch_all => {
                    try pattern_buf.append(self.allocator, '*');
                    try pattern_buf.appendSlice(self.allocator, seg.param_name.?);
                },
            }
        }

        // Handle root index
        if (segments.len == 0 and is_index) {
            pattern_buf.clearRetainingCapacity();
            try pattern_buf.append(self.allocator, '/');
        }

        const pattern = try pattern_buf.toOwnedSlice(self.allocator);
        const stored_file_path = try self.allocator.dupe(u8, file_path);

        try self.routes.append(self.allocator, .{
            .pattern = pattern,
            .file_path = stored_file_path,
            .segments = segments,
            .priority = Route.calculatePriority(segments),
            .is_index = is_index,
        });
    }

    /// Match a URL path to a route
    pub fn match(self: *Self, url_path: []const u8) ?MatchResult {
        // Normalize path
        const normalized = normalizePath(url_path);

        // Try static lookup first (fast path)
        if (self.static_routes.get(normalized)) |route| {
            return MatchResult{
                .route = route,
                .params = std.StringHashMap([]const u8).init(self.allocator),
            };
        }

        // Try each route in priority order
        for (self.routes.items) |*route| {
            if (self.matchRoute(route, normalized)) |params| {
                return MatchResult{
                    .route = route,
                    .params = params,
                };
            }
        }

        return null;
    }

    /// Try to match a URL against a specific route
    fn matchRoute(self: *Self, route: *const Route, url_path: []const u8) ?std.StringHashMap([]const u8) {
        var params = std.StringHashMap([]const u8).init(self.allocator);
        errdefer params.deinit();

        // Split URL into segments
        var url_iter = std.mem.splitScalar(u8, std.mem.trim(u8, url_path, "/"), '/');
        var route_idx: usize = 0;

        while (url_iter.next()) |url_seg| {
            if (url_seg.len == 0) continue;

            if (route_idx >= route.segments.len) {
                // URL has more segments than route
                // Check if last segment is catch-all
                if (route.segments.len > 0) {
                    const last = route.segments[route.segments.len - 1];
                    if (last.segment_type == .catch_all or last.segment_type == .optional_catch_all) {
                        // Append to catch-all param
                        if (params.get(last.param_name.?)) |existing| {
                            const new_val = std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ existing, url_seg }) catch return null;
                            params.put(last.param_name.?, new_val) catch return null;
                        }
                        continue;
                    }
                }
                params.deinit();
                return null;
            }

            const route_seg = route.segments[route_idx];

            switch (route_seg.segment_type) {
                .static => {
                    if (!std.mem.eql(u8, url_seg, route_seg.raw)) {
                        params.deinit();
                        return null;
                    }
                },
                .dynamic => {
                    params.put(route_seg.param_name.?, url_seg) catch {
                        params.deinit();
                        return null;
                    };
                },
                .catch_all, .optional_catch_all => {
                    // Collect all remaining segments
                    var catch_buf: std.ArrayListUnmanaged(u8) = .empty;
                    catch_buf.appendSlice(self.allocator, url_seg) catch {
                        params.deinit();
                        return null;
                    };

                    while (url_iter.next()) |remaining| {
                        if (remaining.len == 0) continue;
                        catch_buf.append(self.allocator, '/') catch {
                            catch_buf.deinit(self.allocator);
                            params.deinit();
                            return null;
                        };
                        catch_buf.appendSlice(self.allocator, remaining) catch {
                            catch_buf.deinit(self.allocator);
                            params.deinit();
                            return null;
                        };
                    }

                    const catch_value = catch_buf.toOwnedSlice(self.allocator) catch {
                        catch_buf.deinit(self.allocator);
                        params.deinit();
                        return null;
                    };
                    params.put(route_seg.param_name.?, catch_value) catch {
                        self.allocator.free(catch_value);
                        params.deinit();
                        return null;
                    };
                    return params;
                },
            }

            route_idx += 1;
        }

        // Check if we matched all route segments
        if (route_idx < route.segments.len) {
            // Remaining segments must be optional catch-all
            const remaining = route.segments[route_idx];
            if (remaining.segment_type != .optional_catch_all) {
                params.deinit();
                return null;
            }
        }

        return params;
    }

    /// Get the file path for a matched route
    pub fn getFilePath(self: *Self, route: *const Route) []const u8 {
        _ = self;
        return route.file_path;
    }

    /// Get full file path (with pages dir prefix)
    pub fn getFullFilePath(self: *Self, route: *const Route) ![]const u8 {
        return std.fs.path.join(self.allocator, &.{ self.pages_dir, route.file_path });
    }

    /// Load redirects from _redirects.json
    fn loadRedirects(self: *Self) !void {
        const redirects_path = try std.fs.path.join(self.allocator, &.{ self.pages_dir, "_redirects.json" });
        defer self.allocator.free(redirects_path);

        const file = std.fs.cwd().openFile(redirects_path, .{}) catch return;
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(content);

        // Parse JSON redirects
        // Expected format: [{"source": "/old", "destination": "/new", "status": 301}, ...]
        var parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, content, .{});
        defer parsed.deinit();

        if (parsed.value != .array) return;

        for (parsed.value.array.items) |item| {
            if (item != .object) continue;

            const source = item.object.get("source") orelse continue;
            const destination = item.object.get("destination") orelse continue;

            if (source != .string or destination != .string) continue;

            const status: u16 = if (item.object.get("status")) |s|
                if (s == .integer) @intCast(s.integer) else 302
            else
                302;

            try self.redirects.append(self.allocator, .{
                .source = try self.allocator.dupe(u8, source.string),
                .destination = try self.allocator.dupe(u8, destination.string),
                .status = status,
                .is_pattern = false,
            });
        }
    }

    /// Add a redirect rule programmatically
    pub fn addRedirect(self: *Self, source: []const u8, destination: []const u8, status: u16) !void {
        try self.redirects.append(self.allocator, .{
            .source = try self.allocator.dupe(u8, source),
            .destination = try self.allocator.dupe(u8, destination),
            .status = status,
            .is_pattern = false,
        });
    }

    /// Check if a URL should be redirected
    pub fn checkRedirect(self: *Self, url_path: []const u8) ?Redirect {
        const normalized = std.mem.trim(u8, url_path, "/");
        const with_slash = if (normalized.len == 0) "/" else url_path;

        for (self.redirects.items) |redirect| {
            const redirect_source = std.mem.trim(u8, redirect.source, "/");

            // Exact match
            if (std.mem.eql(u8, redirect_source, normalized) or
                std.mem.eql(u8, redirect.source, with_slash))
            {
                return redirect;
            }
        }

        return null;
    }
};

/// Normalize a URL path (lowercase, trim slashes)
fn normalizePath(path: []const u8) []const u8 {
    const normalized = std.mem.trim(u8, path, "/");
    if (normalized.len == 0) return "/";

    // For now, just return with leading slash
    // TODO: Handle case normalization if needed
    return path;
}

// ============================================================================
// Tests
// ============================================================================

test "segment parsing - static" {
    const seg = Segment.parse("about");
    try std.testing.expectEqual(SegmentType.static, seg.segment_type);
    try std.testing.expect(seg.param_name == null);
}

test "segment parsing - dynamic" {
    const seg = Segment.parse("[slug]");
    try std.testing.expectEqual(SegmentType.dynamic, seg.segment_type);
    try std.testing.expectEqualStrings("slug", seg.param_name.?);
}

test "segment parsing - catch-all" {
    const seg = Segment.parse("[...path]");
    try std.testing.expectEqual(SegmentType.catch_all, seg.segment_type);
    try std.testing.expectEqualStrings("path", seg.param_name.?);
}

test "segment parsing - optional catch-all" {
    const seg = Segment.parse("[[...path]]");
    try std.testing.expectEqual(SegmentType.optional_catch_all, seg.segment_type);
    try std.testing.expectEqualStrings("path", seg.param_name.?);
}

test "route priority" {
    // Static route should have lowest priority number (highest priority)
    const static_segs = [_]Segment{Segment.parse("about")};
    const static_priority = Route.calculatePriority(&static_segs);

    // Dynamic route should have higher priority number
    const dynamic_segs = [_]Segment{Segment.parse("[slug]")};
    const dynamic_priority = Route.calculatePriority(&dynamic_segs);

    // Catch-all should have even higher priority number
    const catchall_segs = [_]Segment{Segment.parse("[...path]")};
    const catchall_priority = Route.calculatePriority(&catchall_segs);

    try std.testing.expect(static_priority < dynamic_priority);
    try std.testing.expect(dynamic_priority < catchall_priority);
}

test "router init and deinit" {
    const allocator = std.testing.allocator;

    var router = Router.init(allocator, "pages");
    defer router.deinit();

    try std.testing.expectEqualStrings("pages", router.pages_dir);
}
