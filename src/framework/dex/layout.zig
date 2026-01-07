//! Dex Layout System
//!
//! Handles layout discovery and chaining for Dex pages.
//! Layout rendering is handled by the frontend framework (React/Vue/Svelte).
//! This module provides layout resolution for the file-based routing system.
//!
//! Layout hierarchy:
//! ```
//! routes/
//!   layout.tsx           (root layout)
//!   layout.cot           (root layout data loader)
//!   users/
//!     layout.tsx         (users layout)
//!     layout.cot         (users layout data loader)
//!     page.tsx           (users page)
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Layout definition
pub const Layout = struct {
    /// Path to the layout file
    path: []const u8,
    /// Depth in the directory hierarchy (0 = root)
    depth: u32,

    pub fn deinit(self: *Layout) void {
        _ = self;
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

/// Layout resolver - finds layouts for a given page path
pub const LayoutResolver = struct {
    allocator: Allocator,
    routes_dir: []const u8,

    const Self = @This();

    pub fn init(allocator: Allocator, routes_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .routes_dir = routes_dir,
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    /// Find all layouts for a page path
    /// Returns layouts from root to page directory
    pub fn findLayouts(self: *Self, page_path: []const u8) !LayoutChain {
        var chain = LayoutChain.init(self.allocator);
        errdefer chain.deinit();

        // Get directory path
        const dir_path = std.fs.path.dirname(page_path) orelse "";

        // Check each directory level for layout.cot (Cot loader)
        var depth: u32 = 0;

        // Root layout
        const root_layout = try std.fs.path.join(self.allocator, &.{ self.routes_dir, "layout.cot" });
        defer self.allocator.free(root_layout);

        if (fileExists(root_layout)) {
            try chain.add(root_layout, depth);
        }

        // Walk up the directory tree if we have nested directories
        if (dir_path.len > 0) {
            var path_parts = std.mem.splitScalar(u8, dir_path, '/');
            var current_path: std.ArrayListUnmanaged(u8) = .empty;
            defer current_path.deinit(self.allocator);

            try current_path.appendSlice(self.allocator, self.routes_dir);

            while (path_parts.next()) |part| {
                if (part.len == 0) continue;
                depth += 1;

                try current_path.append(self.allocator, '/');
                try current_path.appendSlice(self.allocator, part);

                // Check for layout.cot in this directory
                var layout_path_buf: std.ArrayListUnmanaged(u8) = .empty;
                defer layout_path_buf.deinit(self.allocator);

                try layout_path_buf.appendSlice(self.allocator, current_path.items);
                try layout_path_buf.appendSlice(self.allocator, "/layout.cot");

                if (fileExists(layout_path_buf.items)) {
                    try chain.add(layout_path_buf.items, depth);
                }
            }
        }

        return chain;
    }
};

/// Check if a file exists
fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "layout chain init and deinit" {
    const allocator = std.testing.allocator;

    var chain = LayoutChain.init(allocator);
    defer chain.deinit();

    try chain.add("routes/layout.cot", 0);
    try chain.add("routes/docs/layout.cot", 1);

    try std.testing.expectEqual(@as(usize, 2), chain.layouts.items.len);
}

test "layout chain render order" {
    const allocator = std.testing.allocator;

    var chain = LayoutChain.init(allocator);
    defer chain.deinit();

    // Add in reverse order
    try chain.add("routes/layout.cot", 0);
    try chain.add("routes/docs/layout.cot", 1);

    const order = chain.getRenderOrder();

    // Outermost (depth 1) should come first
    try std.testing.expectEqual(@as(u32, 1), order[0].depth);
    try std.testing.expectEqual(@as(u32, 0), order[1].depth);
}

test "layout resolver init" {
    const allocator = std.testing.allocator;

    var resolver = LayoutResolver.init(allocator, "routes");
    defer resolver.deinit();

    try std.testing.expectEqualStrings("routes", resolver.routes_dir);
}
