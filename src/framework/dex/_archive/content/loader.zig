//! Content Loader
//!
//! Handles loading and routing of content files (MDX/Markdown).
//!
//! File conventions:
//! - `content/docs/intro.mdx` → `/docs/intro`
//! - `content/blog/2024/post.mdx` → `/blog/2024/post`
//! - `content/docs/index.mdx` → `/docs`
//!
//! Special files:
//! - `_meta.json` - defines sidebar order and labels
//! - `index.mdx` - index page for directory

const std = @import("std");
const Allocator = std.mem.Allocator;
const mdx = @import("mdx.zig");
const markdown = @import("markdown.zig");
const frontmatter = @import("frontmatter.zig");

/// Content file entry
pub const ContentEntry = struct {
    /// File path relative to content root
    path: []const u8,
    /// URL route
    route: []const u8,
    /// Parsed frontmatter
    frontmatter: frontmatter.Frontmatter,
    /// File modification time (for caching)
    mtime: i128,
    /// Parent directory (for navigation)
    parent: ?[]const u8,

    pub fn deinit(self: *ContentEntry, allocator: Allocator) void {
        allocator.free(self.path);
        allocator.free(self.route);
        if (self.parent) |p| allocator.free(p);
        self.frontmatter.deinit();
    }
};

/// Directory metadata from _meta.json
pub const DirMeta = struct {
    /// Ordered list of items (filenames without extension)
    order: std.ArrayListUnmanaged([]const u8),
    /// Display labels override
    labels: std.StringHashMap([]const u8),
    /// Default expanded state
    default_open: bool,

    pub fn init(allocator: Allocator) DirMeta {
        return .{
            .order = .empty,
            .labels = std.StringHashMap([]const u8).init(allocator),
            .default_open = false,
        };
    }

    pub fn deinit(self: *DirMeta, allocator: Allocator) void {
        self.order.deinit(allocator);
        self.labels.deinit();
    }
};

/// Navigation item for sidebar
pub const NavItem = struct {
    /// Display label
    label: []const u8,
    /// URL route
    route: []const u8,
    /// Nested items (for directories)
    children: std.ArrayListUnmanaged(NavItem),
    /// Is this a directory?
    is_dir: bool,
    /// Sidebar position from frontmatter
    position: ?i32,

    pub fn deinit(self: *NavItem, allocator: Allocator) void {
        for (self.children.items) |*child| {
            child.deinit(allocator);
        }
        self.children.deinit(allocator);
    }
};

/// Content loader for a content directory
pub const Loader = struct {
    allocator: Allocator,
    /// Root content directory
    content_dir: []const u8,
    /// Cached content entries by route
    entries: std.StringHashMap(ContentEntry),
    /// Directory metadata
    dir_meta: std.StringHashMap(DirMeta),

    const Self = @This();

    pub fn init(allocator: Allocator, content_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .content_dir = content_dir,
            .entries = std.StringHashMap(ContentEntry).init(allocator),
            .dir_meta = std.StringHashMap(DirMeta).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var entry_it = self.entries.iterator();
        while (entry_it.next()) |entry| {
            var e = entry.value_ptr.*;
            e.deinit(self.allocator);
        }
        self.entries.deinit();

        var meta_it = self.dir_meta.iterator();
        while (meta_it.next()) |entry| {
            var m = entry.value_ptr.*;
            m.deinit(self.allocator);
        }
        self.dir_meta.deinit();
    }

    /// Scan content directory and build route table
    pub fn scan(self: *Self) !void {
        try self.scanDir("");
    }

    /// Recursively scan a directory
    fn scanDir(self: *Self, rel_path: []const u8) !void {
        const full_path = if (rel_path.len > 0)
            try std.fs.path.join(self.allocator, &.{ self.content_dir, rel_path })
        else
            try self.allocator.dupe(u8, self.content_dir);
        defer self.allocator.free(full_path);

        var dir = std.fs.cwd().openDir(full_path, .{ .iterate = true }) catch return;
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            const item_rel = if (rel_path.len > 0)
                try std.fs.path.join(self.allocator, &.{ rel_path, entry.name })
            else
                try self.allocator.dupe(u8, entry.name);

            switch (entry.kind) {
                .directory => {
                    // Recurse into subdirectory
                    try self.scanDir(item_rel);
                    self.allocator.free(item_rel);
                },
                .file => {
                    // Check for content files
                    if (std.mem.endsWith(u8, entry.name, ".mdx") or
                        std.mem.endsWith(u8, entry.name, ".md"))
                    {
                        try self.addContentEntry(item_rel);
                    } else if (std.mem.eql(u8, entry.name, "_meta.json")) {
                        try self.loadDirMeta(rel_path, item_rel);
                        self.allocator.free(item_rel);
                    } else {
                        self.allocator.free(item_rel);
                    }
                },
                else => self.allocator.free(item_rel),
            }
        }
    }

    /// Add a content entry
    fn addContentEntry(self: *Self, rel_path: []const u8) !void {
        const full_path = try std.fs.path.join(self.allocator, &.{ self.content_dir, rel_path });
        defer self.allocator.free(full_path);

        // Read file and parse frontmatter
        const file = std.fs.cwd().openFile(full_path, .{}) catch return;
        defer file.close();

        const stat = try file.stat();
        const source = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(source);

        const fm_result = try frontmatter.parse(self.allocator, source);

        // Build route from path
        const route = try self.pathToRoute(rel_path);

        // Get parent directory
        const parent = if (std.fs.path.dirname(rel_path)) |dir|
            try self.allocator.dupe(u8, dir)
        else
            null;

        const entry = ContentEntry{
            .path = rel_path,
            .route = route,
            .frontmatter = fm_result.frontmatter,
            .mtime = stat.mtime,
            .parent = parent,
        };

        try self.entries.put(route, entry);
    }

    /// Convert file path to URL route
    fn pathToRoute(self: *Self, path: []const u8) ![]const u8 {
        var route: std.ArrayListUnmanaged(u8) = .empty;
        errdefer route.deinit(self.allocator);

        try route.append(self.allocator, '/');

        // Remove extension
        const without_ext = if (std.mem.lastIndexOf(u8, path, ".")) |dot|
            path[0..dot]
        else
            path;

        // Handle index files
        const final_path = if (std.mem.endsWith(u8, without_ext, "/index") or
            std.mem.eql(u8, without_ext, "index"))
            without_ext[0 .. without_ext.len - 5]
        else
            without_ext;

        // Replace path separators and normalize
        for (final_path) |c| {
            if (c == '\\') {
                try route.append(self.allocator, '/');
            } else {
                try route.append(self.allocator, c);
            }
        }

        // Remove trailing slash except for root
        if (route.items.len > 1 and route.items[route.items.len - 1] == '/') {
            _ = route.pop();
        }

        return route.toOwnedSlice(self.allocator);
    }

    /// Load directory metadata
    fn loadDirMeta(self: *Self, dir_path: []const u8, meta_path: []const u8) !void {
        _ = meta_path;
        // For now, just create empty metadata
        // TODO: Parse _meta.json when needed
        const meta = DirMeta.init(self.allocator);
        try self.dir_meta.put(dir_path, meta);
    }

    /// Get content by route
    pub fn get(self: *Self, route: []const u8) ?*ContentEntry {
        return self.entries.getPtr(route);
    }

    /// Render content to HTML
    pub fn render(self: *Self, route: []const u8) !?[]const u8 {
        const entry = self.get(route) orelse return null;

        // Read file
        const full_path = try std.fs.path.join(self.allocator, &.{ self.content_dir, entry.path });
        defer self.allocator.free(full_path);

        const file = std.fs.cwd().openFile(full_path, .{}) catch return null;
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(source);

        // Parse and render
        var parser = mdx.Parser.init(self.allocator, .{});
        defer parser.deinit();

        var result = try parser.parse(source);
        defer result.frontmatter.deinit();
        defer self.allocator.free(result.toc);

        // Return owned HTML
        return result.html;
    }

    /// Build navigation tree for sidebar
    pub fn buildNavigation(self: *Self) !NavItem {
        var root = NavItem{
            .label = "Docs",
            .route = "/",
            .children = .empty,
            .is_dir = true,
            .position = null,
        };

        // Collect entries by directory
        var entries_it = self.entries.iterator();
        while (entries_it.next()) |entry| {
            const e = entry.value_ptr.*;
            const label = e.frontmatter.sidebar_label orelse
                e.frontmatter.title orelse
                std.fs.path.basename(e.path);

            const nav = NavItem{
                .label = label,
                .route = e.route,
                .children = .empty,
                .is_dir = false,
                .position = e.frontmatter.sidebar_position,
            };

            try root.children.append(self.allocator, nav);
        }

        // Sort by position
        std.mem.sort(NavItem, root.children.items, {}, struct {
            fn lessThan(_: void, a: NavItem, b: NavItem) bool {
                const a_pos = a.position orelse 999;
                const b_pos = b.position orelse 999;
                return a_pos < b_pos;
            }
        }.lessThan);

        return root;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "path to route" {
    const allocator = std.testing.allocator;

    var loader = Loader.init(allocator, "content");
    defer loader.deinit();

    const route1 = try loader.pathToRoute("docs/intro.mdx");
    defer allocator.free(route1);
    try std.testing.expectEqualStrings("/docs/intro", route1);

    const route2 = try loader.pathToRoute("index.mdx");
    defer allocator.free(route2);
    try std.testing.expectEqualStrings("/", route2);

    const route3 = try loader.pathToRoute("docs/index.md");
    defer allocator.free(route3);
    try std.testing.expectEqualStrings("/docs", route3);
}
