//! Dex Layout System
//!
//! Handles nested layouts using `_layout.dex` files:
//! - Root layout: `pages/_layout.dex`
//! - Nested layouts: `pages/docs/_layout.dex`
//! - Child content rendered via `{@children}` placeholder
//!
//! Layout hierarchy:
//! ```
//! pages/
//!   _layout.dex          (root layout)
//!   docs/
//!     _layout.dex        (docs layout)
//!     getting-started.dex
//! ```
//!
//! Request `/docs/getting-started`:
//! 1. Render page content
//! 2. Wrap with `docs/_layout.dex`
//! 3. Wrap with root `_layout.dex`

const std = @import("std");
const Allocator = std.mem.Allocator;

const compiler = @import("compiler.zig");
const template_renderer = @import("template/renderer.zig");

/// Layout definition
pub const Layout = struct {
    /// Path to the layout file
    path: []const u8,
    /// Compiled layout component
    component: ?compiler.CompiledComponent,
    /// Depth in the directory hierarchy (0 = root)
    depth: u32,

    pub fn deinit(self: *Layout) void {
        if (self.component) |*comp| {
            comp.deinit();
        }
    }
};

/// Layout chain (ordered from innermost to outermost)
pub const LayoutChain = struct {
    layouts: std.ArrayListUnmanaged(Layout),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .layouts = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.layouts.items) |*layout| {
            layout.deinit();
            self.allocator.free(layout.path);
        }
        self.layouts.deinit(self.allocator);
    }

    /// Add a layout to the chain
    pub fn add(self: *Self, path: []const u8, depth: u32) !void {
        const path_copy = try self.allocator.dupe(u8, path);
        try self.layouts.append(self.allocator, .{
            .path = path_copy,
            .component = null,
            .depth = depth,
        });
    }

    /// Get layouts in render order (outermost first)
    pub fn getRenderOrder(self: *Self) []Layout {
        // Sort by depth descending (outermost = highest depth comes first)
        std.mem.sort(Layout, self.layouts.items, {}, struct {
            fn lessThan(_: void, a: Layout, b: Layout) bool {
                return a.depth > b.depth;
            }
        }.lessThan);
        return self.layouts.items;
    }
};

/// Layout resolver - finds and applies layouts
pub const LayoutResolver = struct {
    allocator: Allocator,
    pages_dir: []const u8,
    /// Cache of compiled layouts
    layout_cache: std.StringHashMap(compiler.CompiledComponent),

    const Self = @This();

    pub fn init(allocator: Allocator, pages_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .pages_dir = pages_dir,
            .layout_cache = std.StringHashMap(compiler.CompiledComponent).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var iter = self.layout_cache.iterator();
        while (iter.next()) |entry| {
            var comp = entry.value_ptr.*;
            comp.deinit();
        }
        self.layout_cache.deinit();
    }

    /// Find all layouts for a page path
    /// Returns layouts from root to page directory
    pub fn findLayouts(self: *Self, page_path: []const u8) !LayoutChain {
        var chain = LayoutChain.init(self.allocator);
        errdefer chain.deinit();

        // Get directory path
        const dir_path = std.fs.path.dirname(page_path) orelse "";

        // Check each directory level for _layout.dex
        var depth: u32 = 0;

        // Root layout
        const root_layout = try std.fs.path.join(self.allocator, &.{ self.pages_dir, "_layout.dex" });
        defer self.allocator.free(root_layout);

        if (fileExists(root_layout)) {
            try chain.add(root_layout, depth);
        }

        // Walk up the directory tree if we have nested directories
        if (dir_path.len > 0) {
            var path_parts = std.mem.splitScalar(u8, dir_path, '/');
            var current_path: std.ArrayListUnmanaged(u8) = .empty;
            defer current_path.deinit(self.allocator);

            try current_path.appendSlice(self.allocator, self.pages_dir);

            while (path_parts.next()) |part| {
                if (part.len == 0) continue;
                depth += 1;

                try current_path.append(self.allocator, '/');
                try current_path.appendSlice(self.allocator, part);

                // Check for _layout.dex in this directory
                var layout_path_buf: std.ArrayListUnmanaged(u8) = .empty;
                defer layout_path_buf.deinit(self.allocator);

                try layout_path_buf.appendSlice(self.allocator, current_path.items);
                try layout_path_buf.appendSlice(self.allocator, "/_layout.dex");

                if (fileExists(layout_path_buf.items)) {
                    try chain.add(layout_path_buf.items, depth);
                }
            }
        }

        return chain;
    }

    /// Wrap content in layouts
    pub fn wrapInLayouts(self: *Self, content: []const u8, chain: *LayoutChain) ![]const u8 {
        var result = try self.allocator.dupe(u8, content);

        // Apply layouts from innermost to outermost
        const layouts = chain.getRenderOrder();

        for (layouts) |*layout| {
            const new_result = try self.applyLayout(layout, result);
            self.allocator.free(result);
            result = new_result;
        }

        return result;
    }

    /// Apply a single layout to content
    fn applyLayout(self: *Self, layout: *Layout, content: []const u8) ![]const u8 {
        // Get or compile the layout
        const comp = try self.getOrCompileLayout(layout.path);

        // Create instance
        var instance = try comp.createInstance();
        defer instance.deinit();

        // Set special @children variable in the context
        // For now, we'll do a simple string replacement in the rendered output
        const layout_html = try instance.render();
        defer self.allocator.free(layout_html);

        // Replace {@children} placeholder with content
        return self.replaceChildren(layout_html, content);
    }

    /// Get a compiled layout from cache or compile it
    fn getOrCompileLayout(self: *Self, path: []const u8) !*compiler.CompiledComponent {
        if (self.layout_cache.getPtr(path)) |cached| {
            return cached;
        }

        // Load and compile
        const source = try readFile(self.allocator, path);
        defer self.allocator.free(source);

        var comp = compiler.Compiler.init(self.allocator);
        const component = try comp.compileSource(source);

        try self.layout_cache.put(path, component);
        return self.layout_cache.getPtr(path).?;
    }

    /// Replace {@children} in layout with actual content
    fn replaceChildren(self: *Self, layout_html: []const u8, content: []const u8) ![]const u8 {
        const children_placeholder = "{@children}";

        // Find placeholder
        if (std.mem.indexOf(u8, layout_html, children_placeholder)) |idx| {
            // Build result with content inserted
            var result: std.ArrayListUnmanaged(u8) = .empty;
            errdefer result.deinit(self.allocator);

            try result.appendSlice(self.allocator, layout_html[0..idx]);
            try result.appendSlice(self.allocator, content);
            try result.appendSlice(self.allocator, layout_html[idx + children_placeholder.len ..]);

            return result.toOwnedSlice(self.allocator);
        }

        // No placeholder found, append content at end
        var result: std.ArrayListUnmanaged(u8) = .empty;
        errdefer result.deinit(self.allocator);

        try result.appendSlice(self.allocator, layout_html);
        try result.appendSlice(self.allocator, content);

        return result.toOwnedSlice(self.allocator);
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

// ============================================================================
// Tests
// ============================================================================

test "layout chain init and deinit" {
    const allocator = std.testing.allocator;

    var chain = LayoutChain.init(allocator);
    defer chain.deinit();

    try chain.add("pages/_layout.dex", 0);
    try chain.add("pages/docs/_layout.dex", 1);

    try std.testing.expectEqual(@as(usize, 2), chain.layouts.items.len);
}

test "layout chain render order" {
    const allocator = std.testing.allocator;

    var chain = LayoutChain.init(allocator);
    defer chain.deinit();

    // Add in reverse order
    try chain.add("pages/_layout.dex", 0);
    try chain.add("pages/docs/_layout.dex", 1);

    const order = chain.getRenderOrder();

    // Outermost (depth 1) should come first
    try std.testing.expectEqual(@as(u32, 1), order[0].depth);
    try std.testing.expectEqual(@as(u32, 0), order[1].depth);
}

test "layout resolver init" {
    const allocator = std.testing.allocator;

    var resolver = LayoutResolver.init(allocator, "pages");
    defer resolver.deinit();

    try std.testing.expectEqualStrings("pages", resolver.pages_dir);
}

test "replace children placeholder" {
    const allocator = std.testing.allocator;

    var resolver = LayoutResolver.init(allocator, "pages");
    defer resolver.deinit();

    const layout = "<div class=\"layout\">{@children}</div>";
    const content = "<p>Hello</p>";

    const result = try resolver.replaceChildren(layout, content);
    defer allocator.free(result);

    try std.testing.expectEqualStrings("<div class=\"layout\"><p>Hello</p></div>", result);
}
