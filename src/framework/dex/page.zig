//! Dex Page Component System
//!
//! Handles page conventions and routing for Dex components.
//!
//! Convention:
//!   pages/index.dex  → /
//!   pages/about.dex  → /about
//!   pages/blog/index.dex → /blog
//!   pages/blog/[slug].dex → /blog/:slug (dynamic routes)

const std = @import("std");
const Allocator = std.mem.Allocator;

const compiler = @import("compiler.zig");
const component_parser = @import("component_parser.zig");

/// Page metadata extracted from component
pub const PageMeta = struct {
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    layout: ?[]const u8 = null,
};

/// Request context passed to getServerProps
pub const RequestContext = struct {
    path: []const u8,
    query: std.StringHashMap([]const u8),
    headers: std.StringHashMap([]const u8),
    params: std.StringHashMap([]const u8),
    method: []const u8,
    allocator: Allocator,

    pub fn init(allocator: Allocator) RequestContext {
        return .{
            .path = "/",
            .query = std.StringHashMap([]const u8).init(allocator),
            .headers = std.StringHashMap([]const u8).init(allocator),
            .params = std.StringHashMap([]const u8).init(allocator),
            .method = "GET",
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RequestContext) void {
        self.query.deinit();
        self.headers.deinit();
        self.params.deinit();
    }

    /// Get a query parameter
    pub fn getQuery(self: *const RequestContext, key: []const u8) ?[]const u8 {
        return self.query.get(key);
    }

    /// Get a header value
    pub fn getHeader(self: *const RequestContext, key: []const u8) ?[]const u8 {
        return self.headers.get(key);
    }

    /// Get a route parameter (e.g., :id from /users/:id)
    pub fn getParam(self: *const RequestContext, key: []const u8) ?[]const u8 {
        return self.params.get(key);
    }
};

/// Server-side props returned by getServerProps
pub const ServerProps = struct {
    props: std.StringHashMap([]const u8),
    redirect: ?[]const u8 = null,
    not_found: bool = false,
    allocator: Allocator,

    pub fn init(allocator: Allocator) ServerProps {
        return .{
            .props = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ServerProps) void {
        self.props.deinit();
    }

    pub fn set(self: *ServerProps, key: []const u8, value: []const u8) !void {
        try self.props.put(key, value);
    }
};

/// Page router - maps URLs to page files
pub const PageRouter = struct {
    allocator: Allocator,
    pages_dir: []const u8,
    cache: std.StringHashMap(CachedPage),

    const CachedPage = struct {
        component: compiler.CompiledComponent,
        mtime: i128,
    };

    pub fn init(allocator: Allocator, pages_dir: []const u8) PageRouter {
        return .{
            .allocator = allocator,
            .pages_dir = pages_dir,
            .cache = std.StringHashMap(CachedPage).init(allocator),
        };
    }

    pub fn deinit(self: *PageRouter) void {
        var iter = self.cache.iterator();
        while (iter.next()) |entry| {
            var comp = entry.value_ptr.component;
            comp.deinit();
        }
        self.cache.deinit();
    }

    /// Resolve a URL path to a page file path
    pub fn resolve(self: *PageRouter, url_path: []const u8) !?[]const u8 {
        // Normalize path
        const normalized = if (std.mem.eql(u8, url_path, "/"))
            "index"
        else if (std.mem.startsWith(u8, url_path, "/"))
            url_path[1..]
        else
            url_path;

        // Try exact match first
        const exact_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/{s}.dex",
            .{ self.pages_dir, normalized },
        );

        if (fileExists(exact_path)) {
            return exact_path;
        }
        self.allocator.free(exact_path);

        // Try index.dex in directory
        const index_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/{s}/index.dex",
            .{ self.pages_dir, normalized },
        );

        if (fileExists(index_path)) {
            return index_path;
        }
        self.allocator.free(index_path);

        return null;
    }

    /// Load and compile a page, using cache if available
    /// Returns a fresh CompiledComponent (caller owns it)
    pub fn loadPage(self: *PageRouter, path: []const u8) !compiler.CompiledComponent {
        // Check cache
        if (self.cache.getPtr(path)) |cached| {
            // Check if file was modified
            const current_mtime = getFileMtime(path) catch 0;
            if (current_mtime <= cached.mtime) {
                // Return a copy - caller manages their own lifecycle
                return cached.component;
            }
            // File changed, invalidate cache
            cached.component.deinit();
            _ = self.cache.remove(path);
        }

        // Load and compile
        const source = try readFile(self.allocator, path);
        defer self.allocator.free(source);

        var comp = compiler.Compiler.init(self.allocator);
        const component = try comp.compileSource(source);

        // Cache it
        const mtime = getFileMtime(path) catch 0;
        try self.cache.put(path, .{
            .component = component,
            .mtime = mtime,
        });

        return component;
    }
};

/// Check if a file exists
fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

/// Read a file's contents
fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, 1024 * 1024);
}

/// Get file modification time
fn getFileMtime(path: []const u8) !i128 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const stat = try file.stat();
    return stat.mtime;
}

// ============================================================================
// Tests
// ============================================================================

test "request context init" {
    const allocator = std.testing.allocator;

    var ctx = RequestContext.init(allocator);
    defer ctx.deinit();

    try std.testing.expectEqualStrings("/", ctx.path);
    try std.testing.expectEqualStrings("GET", ctx.method);
}

test "server props" {
    const allocator = std.testing.allocator;

    var props = ServerProps.init(allocator);
    defer props.deinit();

    try props.set("title", "Hello");
    try std.testing.expectEqualStrings("Hello", props.props.get("title").?);
}
