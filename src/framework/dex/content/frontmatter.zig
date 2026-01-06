//! Frontmatter Parser
//!
//! Parses YAML frontmatter from markdown/MDX files:
//!
//! ```
//! ---
//! title: Getting Started
//! description: Learn the basics
//! sidebar_position: 1
//! ---
//!
//! # Content here...
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Frontmatter data
pub const Frontmatter = struct {
    /// Page title
    title: ?[]const u8 = null,
    /// Page description (for meta tags)
    description: ?[]const u8 = null,
    /// Sidebar position for docs
    sidebar_position: ?i32 = null,
    /// Sidebar label (different from title)
    sidebar_label: ?[]const u8 = null,
    /// Custom slug override
    slug: ?[]const u8 = null,
    /// Tags for categorization
    tags: std.ArrayListUnmanaged([]const u8) = .empty,
    /// Draft status (don't show in production)
    draft: bool = false,
    /// Custom metadata (key-value pairs)
    custom: std.StringHashMap([]const u8),
    /// Allocator for cleanup
    allocator: Allocator,

    pub fn init(allocator: Allocator) Frontmatter {
        return .{
            .custom = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Frontmatter) void {
        self.tags.deinit(self.allocator);
        self.custom.deinit();
    }

    /// Get a custom field value
    pub fn get(self: *const Frontmatter, key: []const u8) ?[]const u8 {
        return self.custom.get(key);
    }
};

/// Parse result containing frontmatter and remaining content
pub const ParseResult = struct {
    frontmatter: Frontmatter,
    content: []const u8,
};

/// Parse frontmatter from source
pub fn parse(allocator: Allocator, source: []const u8) !ParseResult {
    var frontmatter = Frontmatter.init(allocator);
    errdefer frontmatter.deinit();

    // Check for frontmatter delimiter
    if (!std.mem.startsWith(u8, source, "---")) {
        return .{
            .frontmatter = frontmatter,
            .content = source,
        };
    }

    // Find closing delimiter
    const rest = source[3..];
    const newline_pos = std.mem.indexOfScalar(u8, rest, '\n') orelse return .{
        .frontmatter = frontmatter,
        .content = source,
    };

    const after_first_line = rest[newline_pos + 1 ..];
    const end_pos = std.mem.indexOf(u8, after_first_line, "\n---") orelse
        std.mem.indexOf(u8, after_first_line, "\r\n---") orelse return .{
        .frontmatter = frontmatter,
        .content = source,
    };

    const yaml_content = after_first_line[0..end_pos];
    const content_start = 3 + newline_pos + 1 + end_pos + 4; // Skip closing ---\n

    // Parse YAML (simple key: value format)
    var lines = std.mem.splitScalar(u8, yaml_content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;

        // Handle list items (for tags)
        if (std.mem.startsWith(u8, trimmed, "- ")) {
            const value = std.mem.trim(u8, trimmed[2..], " \t\"'");
            try frontmatter.tags.append(allocator, value);
            continue;
        }

        // Parse key: value
        const colon_pos = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
        const key = std.mem.trim(u8, trimmed[0..colon_pos], " \t");
        const value = std.mem.trim(u8, trimmed[colon_pos + 1 ..], " \t\"'");

        // Handle known fields
        if (std.mem.eql(u8, key, "title")) {
            frontmatter.title = value;
        } else if (std.mem.eql(u8, key, "description")) {
            frontmatter.description = value;
        } else if (std.mem.eql(u8, key, "sidebar_position")) {
            frontmatter.sidebar_position = std.fmt.parseInt(i32, value, 10) catch null;
        } else if (std.mem.eql(u8, key, "sidebar_label")) {
            frontmatter.sidebar_label = value;
        } else if (std.mem.eql(u8, key, "slug")) {
            frontmatter.slug = value;
        } else if (std.mem.eql(u8, key, "draft")) {
            frontmatter.draft = std.mem.eql(u8, value, "true");
        } else if (!std.mem.eql(u8, key, "tags")) {
            // Store as custom field
            try frontmatter.custom.put(key, value);
        }
    }

    // Find actual content start (skip trailing newlines after ---)
    var content = source[content_start..];
    content = std.mem.trimLeft(u8, content, "\r\n");

    return .{
        .frontmatter = frontmatter,
        .content = content,
    };
}

/// Generate HTML meta tags from frontmatter
pub fn generateMetaTags(allocator: Allocator, fm: *const Frontmatter) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    if (fm.title) |title| {
        try html.appendSlice(allocator, "<title>");
        try appendEscaped(&html, allocator, title);
        try html.appendSlice(allocator, "</title>\n");
    }

    if (fm.description) |desc| {
        try html.appendSlice(allocator, "<meta name=\"description\" content=\"");
        try appendEscaped(&html, allocator, desc);
        try html.appendSlice(allocator, "\">\n");
    }

    // Open Graph tags
    if (fm.title) |title| {
        try html.appendSlice(allocator, "<meta property=\"og:title\" content=\"");
        try appendEscaped(&html, allocator, title);
        try html.appendSlice(allocator, "\">\n");
    }

    if (fm.description) |desc| {
        try html.appendSlice(allocator, "<meta property=\"og:description\" content=\"");
        try appendEscaped(&html, allocator, desc);
        try html.appendSlice(allocator, "\">\n");
    }

    return html.toOwnedSlice(allocator);
}

fn appendEscaped(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, text: []const u8) !void {
    for (text) |c| {
        switch (c) {
            '<' => try buf.appendSlice(allocator, "&lt;"),
            '>' => try buf.appendSlice(allocator, "&gt;"),
            '&' => try buf.appendSlice(allocator, "&amp;"),
            '"' => try buf.appendSlice(allocator, "&quot;"),
            else => try buf.append(allocator, c),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "parse frontmatter" {
    const allocator = std.testing.allocator;

    const source =
        \\---
        \\title: Hello World
        \\description: A test page
        \\sidebar_position: 1
        \\---
        \\
        \\# Content
    ;

    var result = try parse(allocator, source);
    defer result.frontmatter.deinit();

    try std.testing.expectEqualStrings("Hello World", result.frontmatter.title.?);
    try std.testing.expectEqualStrings("A test page", result.frontmatter.description.?);
    try std.testing.expectEqual(@as(i32, 1), result.frontmatter.sidebar_position.?);
    try std.testing.expect(std.mem.indexOf(u8, result.content, "# Content") != null);
}

test "no frontmatter" {
    const allocator = std.testing.allocator;

    const source = "# Just Content";

    var result = try parse(allocator, source);
    defer result.frontmatter.deinit();

    try std.testing.expect(result.frontmatter.title == null);
    try std.testing.expectEqualStrings("# Just Content", result.content);
}

test "draft field" {
    const allocator = std.testing.allocator;

    const source =
        \\---
        \\title: Draft Post
        \\draft: true
        \\---
        \\Content
    ;

    var result = try parse(allocator, source);
    defer result.frontmatter.deinit();

    try std.testing.expect(result.frontmatter.draft);
}

test "custom fields" {
    const allocator = std.testing.allocator;

    const source =
        \\---
        \\title: Test
        \\author: John
        \\category: tutorial
        \\---
        \\Content
    ;

    var result = try parse(allocator, source);
    defer result.frontmatter.deinit();

    try std.testing.expectEqualStrings("John", result.frontmatter.get("author").?);
    try std.testing.expectEqualStrings("tutorial", result.frontmatter.get("category").?);
}

test "generate meta tags" {
    const allocator = std.testing.allocator;

    var fm = Frontmatter.init(allocator);
    defer fm.deinit();
    fm.title = "Test Title";
    fm.description = "Test description";

    const meta = try generateMetaTags(allocator, &fm);
    defer allocator.free(meta);

    try std.testing.expect(std.mem.indexOf(u8, meta, "<title>Test Title</title>") != null);
    try std.testing.expect(std.mem.indexOf(u8, meta, "og:title") != null);
}
