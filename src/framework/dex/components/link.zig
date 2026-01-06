//! Dex Link Component
//!
//! A navigation link component that renders as an `<a>` tag with:
//! - `href` attribute for the destination URL
//! - `active` class when on the current route
//! - `data-dex-link` attribute for client-side navigation interception
//! - `prefetch` attribute support for preloading

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Link component props
pub const LinkProps = struct {
    /// Destination URL
    href: []const u8,
    /// Optional CSS class
    class: ?[]const u8 = null,
    /// Whether to prefetch the page (for client-side nav)
    prefetch: bool = false,
    /// Target attribute (_blank, _self, etc.)
    target: ?[]const u8 = null,
    /// Whether to use replace instead of push for history
    replace: bool = false,
    /// Custom active class name (default: "active")
    active_class: []const u8 = "active",
    /// Whether to do exact matching for active state
    exact: bool = false,
};

/// Render a Link component
pub fn render(allocator: Allocator, props: LinkProps, children: []const u8, current_path: []const u8) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    // Determine if this link is active
    const is_active = isActivePath(props.href, current_path, props.exact);

    // Build class attribute
    var class_buf: std.ArrayListUnmanaged(u8) = .empty;
    defer class_buf.deinit(allocator);

    if (props.class) |cls| {
        try class_buf.appendSlice(allocator, cls);
    }

    if (is_active) {
        if (class_buf.items.len > 0) {
            try class_buf.append(allocator, ' ');
        }
        try class_buf.appendSlice(allocator, props.active_class);
    }

    // Build the anchor tag
    try html.appendSlice(allocator, "<a href=\"");
    try appendEscaped(&html, allocator, props.href);
    try html.append(allocator, '"');

    // Add class if present
    if (class_buf.items.len > 0) {
        try html.appendSlice(allocator, " class=\"");
        try html.appendSlice(allocator, class_buf.items);
        try html.append(allocator, '"');
    }

    // Add data-dex-link for client-side navigation
    try html.appendSlice(allocator, " data-dex-link");

    // Add prefetch attribute
    if (props.prefetch) {
        try html.appendSlice(allocator, " data-dex-prefetch");
    }

    // Add replace history attribute
    if (props.replace) {
        try html.appendSlice(allocator, " data-dex-replace");
    }

    // Add target attribute
    if (props.target) |t| {
        try html.appendSlice(allocator, " target=\"");
        try appendEscaped(&html, allocator, t);
        try html.append(allocator, '"');
    }

    // Add aria-current for accessibility when active
    if (is_active) {
        try html.appendSlice(allocator, " aria-current=\"page\"");
    }

    try html.append(allocator, '>');

    // Add children (link text)
    try html.appendSlice(allocator, children);

    try html.appendSlice(allocator, "</a>");

    return html.toOwnedSlice(allocator);
}

/// Check if the given href matches the current path
fn isActivePath(href: []const u8, current_path: []const u8, exact: bool) bool {
    // Normalize paths
    const normalized_href = std.mem.trim(u8, href, "/");
    const normalized_current = std.mem.trim(u8, current_path, "/");

    if (exact) {
        // Exact match
        return std.mem.eql(u8, normalized_href, normalized_current);
    }

    // Prefix match (for nested routes)
    if (normalized_href.len == 0) {
        // Root path - only active when at root
        return normalized_current.len == 0;
    }

    if (std.mem.startsWith(u8, normalized_current, normalized_href)) {
        // Check it's a proper prefix (followed by / or end)
        if (normalized_current.len == normalized_href.len) {
            return true;
        }
        if (normalized_current.len > normalized_href.len and normalized_current[normalized_href.len] == '/') {
            return true;
        }
    }

    return false;
}

/// Append HTML-escaped string
fn appendEscaped(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, str: []const u8) !void {
    for (str) |c| {
        switch (c) {
            '<' => try buf.appendSlice(allocator, "&lt;"),
            '>' => try buf.appendSlice(allocator, "&gt;"),
            '&' => try buf.appendSlice(allocator, "&amp;"),
            '"' => try buf.appendSlice(allocator, "&quot;"),
            '\'' => try buf.appendSlice(allocator, "&#x27;"),
            else => try buf.append(allocator, c),
        }
    }
}

/// NavLink - Link with additional navigation features
pub const NavLink = struct {
    /// Render a nav link (alias for render with nav-specific defaults)
    pub fn renderNav(allocator: Allocator, href: []const u8, text: []const u8, current_path: []const u8) ![]const u8 {
        return render(allocator, .{
            .href = href,
            .prefetch = true,
        }, text, current_path);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "render basic link" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, .{
        .href = "/about",
    }, "About", "/");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "href=\"/about\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "data-dex-link") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, ">About</a>") != null);
}

test "render active link" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, .{
        .href = "/about",
    }, "About", "/about");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "class=\"active\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "aria-current=\"page\"") != null);
}

test "render link with custom class" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, .{
        .href = "/about",
        .class = "nav-link",
    }, "About", "/about");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "class=\"nav-link active\"") != null);
}

test "render link with prefetch" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, .{
        .href = "/about",
        .prefetch = true,
    }, "About", "/");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "data-dex-prefetch") != null);
}

test "render link with target" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, .{
        .href = "https://example.com",
        .target = "_blank",
    }, "External", "/");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "target=\"_blank\"") != null);
}

test "isActivePath exact match" {
    try std.testing.expect(isActivePath("/about", "/about", true));
    try std.testing.expect(!isActivePath("/about", "/about/team", true));
}

test "isActivePath prefix match" {
    try std.testing.expect(isActivePath("/docs", "/docs", false));
    try std.testing.expect(isActivePath("/docs", "/docs/getting-started", false));
    try std.testing.expect(!isActivePath("/docs", "/documentation", false));
}

test "isActivePath root" {
    try std.testing.expect(isActivePath("/", "/", false));
    try std.testing.expect(!isActivePath("/", "/about", false));
}
