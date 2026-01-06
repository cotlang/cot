//! Built-in Documentation Components
//!
//! Pre-built components for documentation sites:
//! - Callout: Info, warning, error, tip callouts
//! - Tabs: Tabbed content sections
//! - Steps: Numbered step-by-step guides
//! - Card/Cards: Link cards with icons
//! - FileTree: File/folder structure display
//!
//! All components render semantic HTML with Tailwind-compatible classes.

const std = @import("std");
const Allocator = std.mem.Allocator;
const mdx = @import("mdx.zig");

/// Callout types for different emphasis levels
pub const CalloutType = enum {
    note,
    tip,
    info,
    warning,
    danger,
    caution,

    pub fn icon(self: CalloutType) []const u8 {
        return switch (self) {
            .note => "&#128221;", // Memo
            .tip => "&#128161;", // Lightbulb
            .info => "&#8505;", // Info
            .warning => "&#9888;", // Warning
            .danger => "&#9762;", // Biohazard
            .caution => "&#9888;", // Warning
        };
    }

    pub fn cssClass(self: CalloutType) []const u8 {
        return switch (self) {
            .note => "callout-note",
            .tip => "callout-tip",
            .info => "callout-info",
            .warning => "callout-warning",
            .danger => "callout-danger",
            .caution => "callout-caution",
        };
    }

    pub fn defaultTitle(self: CalloutType) []const u8 {
        return switch (self) {
            .note => "Note",
            .tip => "Tip",
            .info => "Info",
            .warning => "Warning",
            .danger => "Danger",
            .caution => "Caution",
        };
    }
};

/// Render a Callout component
pub fn renderCallout(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    // Determine callout type
    const type_str = props.getString("type") orelse "note";
    const callout_type = std.meta.stringToEnum(CalloutType, type_str) orelse .note;

    // Get title (default to type name)
    const title = props.getString("title") orelse callout_type.defaultTitle();

    // Start callout container
    try html.appendSlice(allocator, "<div class=\"callout ");
    try html.appendSlice(allocator, callout_type.cssClass());
    try html.appendSlice(allocator, "\">\n");

    // Title bar
    try html.appendSlice(allocator, "  <div class=\"callout-title\">\n");
    try html.appendSlice(allocator, "    <span class=\"callout-icon\">");
    try html.appendSlice(allocator, callout_type.icon());
    try html.appendSlice(allocator, "</span>\n");
    try html.appendSlice(allocator, "    <span>");
    try html.appendSlice(allocator, title);
    try html.appendSlice(allocator, "</span>\n");
    try html.appendSlice(allocator, "  </div>\n");

    // Content
    try html.appendSlice(allocator, "  <div class=\"callout-content\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n  </div>\n");

    try html.appendSlice(allocator, "</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Tabs component
pub fn renderTabs(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    // Get tab items from props
    const items_str = props.getString("items") orelse "[]";
    _ = items_str;

    // Generate unique ID for this tabs instance
    const id = @as(u64, @truncate(@as(u128, @bitCast(std.time.nanoTimestamp()))));

    try html.appendSlice(allocator, "<div class=\"tabs\" data-tabs-id=\"");
    var id_buf: [20]u8 = undefined;
    const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{id}) catch "0";
    try html.appendSlice(allocator, id_str);
    try html.appendSlice(allocator, "\">\n");

    // Tab headers (will be populated by JS)
    try html.appendSlice(allocator, "  <div class=\"tabs-header\" role=\"tablist\"></div>\n");

    // Tab content
    try html.appendSlice(allocator, "  <div class=\"tabs-content\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n  </div>\n");

    try html.appendSlice(allocator, "</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Tab panel
pub fn renderTab(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const label = props.getString("label") orelse "Tab";

    try html.appendSlice(allocator, "<div class=\"tab-panel\" data-tab-label=\"");
    try html.appendSlice(allocator, label);
    try html.appendSlice(allocator, "\" role=\"tabpanel\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Steps component
pub fn renderSteps(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    try html.appendSlice(allocator, "<div class=\"steps\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Step component
pub fn renderStep(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const title = props.getString("title") orelse "";

    try html.appendSlice(allocator, "<div class=\"step\">\n");
    try html.appendSlice(allocator, "  <div class=\"step-indicator\"></div>\n");
    try html.appendSlice(allocator, "  <div class=\"step-content\">\n");

    if (title.len > 0) {
        try html.appendSlice(allocator, "    <h4 class=\"step-title\">");
        try html.appendSlice(allocator, title);
        try html.appendSlice(allocator, "</h4>\n");
    }

    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n  </div>\n");
    try html.appendSlice(allocator, "</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Card component
pub fn renderCard(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const title = props.getString("title") orelse "";
    const href = props.getString("href");
    const icon = props.getString("icon");

    // Determine if it's a link card
    if (href) |link| {
        try html.appendSlice(allocator, "<a class=\"card card-link\" href=\"");
        try html.appendSlice(allocator, link);
        try html.appendSlice(allocator, "\">\n");
    } else {
        try html.appendSlice(allocator, "<div class=\"card\">\n");
    }

    // Icon
    if (icon) |i| {
        try html.appendSlice(allocator, "  <div class=\"card-icon\">");
        try html.appendSlice(allocator, i);
        try html.appendSlice(allocator, "</div>\n");
    }

    // Title
    if (title.len > 0) {
        try html.appendSlice(allocator, "  <h3 class=\"card-title\">");
        try html.appendSlice(allocator, title);
        try html.appendSlice(allocator, "</h3>\n");
    }

    // Content
    if (props.children.len > 0) {
        try html.appendSlice(allocator, "  <div class=\"card-content\">\n");
        try html.appendSlice(allocator, props.children);
        try html.appendSlice(allocator, "\n  </div>\n");
    }

    if (href != null) {
        try html.appendSlice(allocator, "</a>\n");
    } else {
        try html.appendSlice(allocator, "</div>\n");
    }

    return html.toOwnedSlice(allocator);
}

/// Render a Cards grid container
pub fn renderCards(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const cols = props.getString("cols") orelse "2";

    try html.appendSlice(allocator, "<div class=\"cards cards-");
    try html.appendSlice(allocator, cols);
    try html.appendSlice(allocator, "\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a FileTree component
pub fn renderFileTree(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    try html.appendSlice(allocator, "<div class=\"file-tree\">\n");
    try html.appendSlice(allocator, "  <ul class=\"file-tree-list\">\n");
    try html.appendSlice(allocator, props.children);
    try html.appendSlice(allocator, "\n  </ul>\n");
    try html.appendSlice(allocator, "</div>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a File component (for FileTree)
pub fn renderFile(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const name = props.getString("name") orelse "file";

    try html.appendSlice(allocator, "<li class=\"file-tree-file\">\n");
    try html.appendSlice(allocator, "  <span class=\"file-icon\">&#128196;</span>\n");
    try html.appendSlice(allocator, "  <span class=\"file-name\">");
    try html.appendSlice(allocator, name);
    try html.appendSlice(allocator, "</span>\n");
    try html.appendSlice(allocator, "</li>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a Folder component (for FileTree)
pub fn renderFolder(allocator: Allocator, props: *const mdx.ComponentProps) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const name = props.getString("name") orelse "folder";
    const open = props.getBool("open") orelse false;

    try html.appendSlice(allocator, "<li class=\"file-tree-folder");
    if (open) try html.appendSlice(allocator, " open");
    try html.appendSlice(allocator, "\">\n");
    try html.appendSlice(allocator, "  <span class=\"folder-icon\">&#128193;</span>\n");
    try html.appendSlice(allocator, "  <span class=\"folder-name\">");
    try html.appendSlice(allocator, name);
    try html.appendSlice(allocator, "</span>\n");

    if (props.children.len > 0) {
        try html.appendSlice(allocator, "  <ul class=\"folder-contents\">\n");
        try html.appendSlice(allocator, props.children);
        try html.appendSlice(allocator, "\n  </ul>\n");
    }

    try html.appendSlice(allocator, "</li>\n");

    return html.toOwnedSlice(allocator);
}

/// Create a component registry with all built-in doc components
pub fn createRegistry(allocator: Allocator) !mdx.ComponentRegistry {
    var registry = mdx.ComponentRegistry.init(allocator);
    errdefer registry.deinit();

    try registry.register("Callout", renderCallout);
    try registry.register("Tabs", renderTabs);
    try registry.register("Tab", renderTab);
    try registry.register("Steps", renderSteps);
    try registry.register("Step", renderStep);
    try registry.register("Card", renderCard);
    try registry.register("Cards", renderCards);
    try registry.register("FileTree", renderFileTree);
    try registry.register("File", renderFile);
    try registry.register("Folder", renderFolder);

    return registry;
}

// ============================================================================
// Tests
// ============================================================================

test "render callout" {
    const allocator = std.testing.allocator;

    var props = mdx.ComponentProps{
        .name = "Callout",
        .props = std.StringHashMap(mdx.PropValue).init(allocator),
        .children = "This is important!",
        .self_closing = false,
    };
    defer props.props.deinit();

    try props.props.put("type", .{ .string = "warning" });

    const html = try renderCallout(allocator, &props);
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "callout-warning") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "This is important!") != null);
}

test "render card" {
    const allocator = std.testing.allocator;

    var props = mdx.ComponentProps{
        .name = "Card",
        .props = std.StringHashMap(mdx.PropValue).init(allocator),
        .children = "Card description",
        .self_closing = false,
    };
    defer props.props.deinit();

    try props.props.put("title", .{ .string = "Getting Started" });
    try props.props.put("href", .{ .string = "/docs/intro" });

    const html = try renderCard(allocator, &props);
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "card-link") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "/docs/intro") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "Getting Started") != null);
}

test "create registry" {
    const allocator = std.testing.allocator;

    var registry = try createRegistry(allocator);
    defer registry.deinit();

    try std.testing.expect(registry.get("Callout") != null);
    try std.testing.expect(registry.get("Tabs") != null);
    try std.testing.expect(registry.get("Card") != null);
    try std.testing.expect(registry.get("NonExistent") == null);
}
