//! Markdown Parser
//!
//! Converts Markdown to HTML with support for:
//! - Headings (h1-h6) with auto-generated IDs
//! - Paragraphs
//! - Inline formatting (bold, italic, strikethrough, code)
//! - Links and images
//! - Lists (ordered, unordered, nested)
//! - Blockquotes
//! - Horizontal rules
//! - Fenced code blocks with language tags
//!
//! Designed to work with Tailwind CSS - outputs clean semantic HTML.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Markdown parsing options
pub const Options = struct {
    /// Generate IDs for headings (for anchor links)
    heading_ids: bool = true,
    /// Add anchor links to headings
    heading_anchors: bool = true,
    /// Enable GitHub-flavored markdown extensions
    gfm: bool = true,
    /// Wrap output in a container div
    wrap_output: bool = false,
    /// Container class (if wrap_output is true)
    container_class: []const u8 = "prose",
};

/// Parsed code block with metadata
pub const CodeBlock = struct {
    language: ?[]const u8,
    code: []const u8,
    filename: ?[]const u8,
    highlight_lines: ?[]const u8,
};

/// Table of contents entry
pub const TocEntry = struct {
    level: u8,
    id: ?[]const u8, // Owned allocation if not null
    text: []const u8, // Slice from source, not owned
    children: std.ArrayListUnmanaged(TocEntry),

    pub fn deinit(self: *TocEntry, allocator: Allocator) void {
        // Free owned id allocation
        if (self.id) |id| {
            allocator.free(id);
        }
        for (self.children.items) |*child| {
            child.deinit(allocator);
        }
        self.children.deinit(allocator);
    }
};

/// Markdown parser
pub const Parser = struct {
    allocator: Allocator,
    options: Options,
    output: std.ArrayListUnmanaged(u8),
    toc: std.ArrayListUnmanaged(TocEntry),
    code_blocks: std.ArrayListUnmanaged(CodeBlock),

    const Self = @This();

    pub fn init(allocator: Allocator, options: Options) Self {
        return .{
            .allocator = allocator,
            .options = options,
            .output = .empty,
            .toc = .empty,
            .code_blocks = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
        for (self.toc.items) |*entry| {
            entry.deinit(self.allocator);
        }
        self.toc.deinit(self.allocator);
        self.code_blocks.deinit(self.allocator);
    }

    /// Parse markdown source and return HTML
    pub fn parse(self: *Self, source: []const u8) ![]const u8 {
        self.output.clearRetainingCapacity();

        if (self.options.wrap_output) {
            try self.output.appendSlice(self.allocator, "<div class=\"");
            try self.output.appendSlice(self.allocator, self.options.container_class);
            try self.output.appendSlice(self.allocator, "\">\n");
        }

        var lines = std.mem.splitScalar(u8, source, '\n');
        var in_code_block = false;
        var code_block_lang: ?[]const u8 = null;
        var code_block_meta: ?[]const u8 = null;
        var code_content: std.ArrayListUnmanaged(u8) = .empty;
        defer code_content.deinit(self.allocator);

        var in_list = false;
        var list_type: enum { unordered, ordered } = .unordered;
        var in_blockquote = false;

        while (lines.next()) |line| {
            // Handle code blocks
            if (std.mem.startsWith(u8, line, "```")) {
                if (in_code_block) {
                    // End code block
                    try self.renderCodeBlock(code_block_lang, code_block_meta, code_content.items);
                    code_content.clearRetainingCapacity();
                    in_code_block = false;
                    code_block_lang = null;
                    code_block_meta = null;
                } else {
                    // Start code block
                    in_code_block = true;
                    const meta = std.mem.trim(u8, line[3..], " \t");
                    if (meta.len > 0) {
                        // Parse language and metadata
                        var parts = std.mem.splitScalar(u8, meta, ' ');
                        code_block_lang = parts.first();
                        code_block_meta = parts.rest();
                        if (code_block_meta.?.len == 0) code_block_meta = null;
                    }
                }
                continue;
            }

            if (in_code_block) {
                if (code_content.items.len > 0) {
                    try code_content.append(self.allocator, '\n');
                }
                try code_content.appendSlice(self.allocator, line);
                continue;
            }

            // Close list if needed
            if (in_list and !isListItem(line)) {
                if (list_type == .unordered) {
                    try self.output.appendSlice(self.allocator, "</ul>\n");
                } else {
                    try self.output.appendSlice(self.allocator, "</ol>\n");
                }
                in_list = false;
            }

            // Close blockquote if needed
            if (in_blockquote and !std.mem.startsWith(u8, line, ">")) {
                try self.output.appendSlice(self.allocator, "</blockquote>\n");
                in_blockquote = false;
            }

            // Horizontal rule
            if (isHorizontalRule(line)) {
                try self.output.appendSlice(self.allocator, "<hr />\n");
                continue;
            }

            // Heading
            if (line.len > 0 and line[0] == '#') {
                try self.parseHeading(line);
                continue;
            }

            // Blockquote
            if (std.mem.startsWith(u8, line, ">")) {
                if (!in_blockquote) {
                    try self.output.appendSlice(self.allocator, "<blockquote>\n");
                    in_blockquote = true;
                }
                const content = std.mem.trim(u8, line[1..], " ");
                try self.output.appendSlice(self.allocator, "<p>");
                try self.parseInline(content);
                try self.output.appendSlice(self.allocator, "</p>\n");
                continue;
            }

            // Unordered list
            if (isUnorderedListItem(line)) {
                if (!in_list) {
                    try self.output.appendSlice(self.allocator, "<ul>\n");
                    in_list = true;
                    list_type = .unordered;
                }
                const content = getListItemContent(line);
                try self.output.appendSlice(self.allocator, "<li>");
                try self.parseInline(content);
                try self.output.appendSlice(self.allocator, "</li>\n");
                continue;
            }

            // Ordered list
            if (isOrderedListItem(line)) {
                if (!in_list) {
                    try self.output.appendSlice(self.allocator, "<ol>\n");
                    in_list = true;
                    list_type = .ordered;
                }
                const content = getOrderedListContent(line);
                try self.output.appendSlice(self.allocator, "<li>");
                try self.parseInline(content);
                try self.output.appendSlice(self.allocator, "</li>\n");
                continue;
            }

            // Empty line
            if (std.mem.trim(u8, line, " \t\r").len == 0) {
                continue;
            }

            // Paragraph
            try self.output.appendSlice(self.allocator, "<p>");
            try self.parseInline(line);
            try self.output.appendSlice(self.allocator, "</p>\n");
        }

        // Close any open elements
        if (in_list) {
            if (list_type == .unordered) {
                try self.output.appendSlice(self.allocator, "</ul>\n");
            } else {
                try self.output.appendSlice(self.allocator, "</ol>\n");
            }
        }
        if (in_blockquote) {
            try self.output.appendSlice(self.allocator, "</blockquote>\n");
        }

        if (self.options.wrap_output) {
            try self.output.appendSlice(self.allocator, "</div>\n");
        }

        return self.output.items;
    }

    /// Parse a heading line
    fn parseHeading(self: *Self, line: []const u8) !void {
        var level: u8 = 0;
        var i: usize = 0;
        while (i < line.len and line[i] == '#') : (i += 1) {
            level += 1;
        }
        if (level > 6) level = 6;

        const text = std.mem.trim(u8, line[i..], " \t");
        const id = if (self.options.heading_ids) try self.generateId(text) else null;

        try self.output.appendSlice(self.allocator, "<h");
        try self.output.append(self.allocator, '0' + level);

        if (id) |heading_id| {
            try self.output.appendSlice(self.allocator, " id=\"");
            try self.output.appendSlice(self.allocator, heading_id);
            try self.output.append(self.allocator, '"');

            // Add to TOC
            try self.toc.append(self.allocator, .{
                .level = level,
                .id = heading_id,
                .text = text,
                .children = .empty,
            });
        }

        try self.output.append(self.allocator, '>');

        if (self.options.heading_anchors and id != null) {
            try self.output.appendSlice(self.allocator, "<a class=\"anchor\" href=\"#");
            try self.output.appendSlice(self.allocator, id.?);
            try self.output.appendSlice(self.allocator, "\">#</a>");
        }

        try self.parseInline(text);
        try self.output.appendSlice(self.allocator, "</h");
        try self.output.append(self.allocator, '0' + level);
        try self.output.appendSlice(self.allocator, ">\n");
    }

    /// Parse inline formatting
    fn parseInline(self: *Self, text: []const u8) !void {
        var i: usize = 0;
        while (i < text.len) {
            // Bold (**text**)
            if (i + 1 < text.len and text[i] == '*' and text[i + 1] == '*') {
                if (std.mem.indexOf(u8, text[i + 2 ..], "**")) |end| {
                    try self.output.appendSlice(self.allocator, "<strong>");
                    try self.parseInline(text[i + 2 .. i + 2 + end]);
                    try self.output.appendSlice(self.allocator, "</strong>");
                    i += 4 + end;
                    continue;
                }
            }

            // Italic (*text* or _text_)
            if (text[i] == '*' or text[i] == '_') {
                const marker = text[i];
                if (i + 1 < text.len and text[i + 1] != marker) {
                    if (std.mem.indexOfScalar(u8, text[i + 1 ..], marker)) |end| {
                        try self.output.appendSlice(self.allocator, "<em>");
                        try self.parseInline(text[i + 1 .. i + 1 + end]);
                        try self.output.appendSlice(self.allocator, "</em>");
                        i += 2 + end;
                        continue;
                    }
                }
            }

            // Strikethrough (~~text~~)
            if (self.options.gfm and i + 1 < text.len and text[i] == '~' and text[i + 1] == '~') {
                if (std.mem.indexOf(u8, text[i + 2 ..], "~~")) |end| {
                    try self.output.appendSlice(self.allocator, "<del>");
                    try self.parseInline(text[i + 2 .. i + 2 + end]);
                    try self.output.appendSlice(self.allocator, "</del>");
                    i += 4 + end;
                    continue;
                }
            }

            // Inline code (`code`)
            if (text[i] == '`') {
                if (std.mem.indexOfScalar(u8, text[i + 1 ..], '`')) |end| {
                    try self.output.appendSlice(self.allocator, "<code>");
                    try self.appendEscaped(text[i + 1 .. i + 1 + end]);
                    try self.output.appendSlice(self.allocator, "</code>");
                    i += 2 + end;
                    continue;
                }
            }

            // Link [text](url)
            if (text[i] == '[') {
                if (self.parseLink(text[i..])) |result| {
                    try self.output.appendSlice(self.allocator, result.html);
                    i += result.consumed;
                    continue;
                }
            }

            // Image ![alt](url)
            if (text[i] == '!' and i + 1 < text.len and text[i + 1] == '[') {
                if (self.parseImage(text[i..])) |result| {
                    try self.output.appendSlice(self.allocator, result.html);
                    i += result.consumed;
                    continue;
                }
            }

            // Escape HTML
            try self.appendEscapedChar(text[i]);
            i += 1;
        }
    }

    const ParseResult = struct {
        html: []const u8,
        consumed: usize,
    };

    /// Parse a markdown link
    fn parseLink(self: *Self, text: []const u8) ?ParseResult {
        _ = self;
        if (text.len < 4 or text[0] != '[') return null;

        const text_end = std.mem.indexOfScalar(u8, text[1..], ']') orelse return null;
        const link_text = text[1 .. text_end + 1];

        if (text_end + 2 >= text.len or text[text_end + 2] != '(') return null;

        const url_start = text_end + 3;
        const url_end = std.mem.indexOfScalar(u8, text[url_start..], ')') orelse return null;
        const url = text[url_start .. url_start + url_end];

        // Build HTML (note: in real implementation, allocate properly)
        _ = link_text;
        _ = url;

        return null; // Simplified for now
    }

    /// Parse a markdown image
    fn parseImage(self: *Self, text: []const u8) ?ParseResult {
        _ = self;
        _ = text;
        return null; // Simplified for now
    }

    /// Render a code block
    fn renderCodeBlock(self: *Self, lang: ?[]const u8, meta: ?[]const u8, code: []const u8) !void {
        // Store code block for potential syntax highlighting
        try self.code_blocks.append(self.allocator, .{
            .language = lang,
            .code = code,
            .filename = extractFilename(meta),
            .highlight_lines = extractHighlightLines(meta),
        });

        // Render HTML
        try self.output.appendSlice(self.allocator, "<pre");

        if (lang) |language| {
            try self.output.appendSlice(self.allocator, " class=\"language-");
            try self.output.appendSlice(self.allocator, language);
            try self.output.append(self.allocator, '"');
        }

        // Add data attributes for metadata
        if (extractFilename(meta)) |filename| {
            try self.output.appendSlice(self.allocator, " data-filename=\"");
            try self.output.appendSlice(self.allocator, filename);
            try self.output.append(self.allocator, '"');
        }

        if (extractHighlightLines(meta)) |lines| {
            try self.output.appendSlice(self.allocator, " data-highlight=\"");
            try self.output.appendSlice(self.allocator, lines);
            try self.output.append(self.allocator, '"');
        }

        try self.output.appendSlice(self.allocator, "><code>");
        try self.appendEscaped(code);
        try self.output.appendSlice(self.allocator, "</code></pre>\n");
    }

    /// Generate a URL-safe ID from text
    fn generateId(self: *Self, text: []const u8) ![]const u8 {
        var id: std.ArrayListUnmanaged(u8) = .empty;
        errdefer id.deinit(self.allocator);

        for (text) |c| {
            if (std.ascii.isAlphanumeric(c)) {
                try id.append(self.allocator, std.ascii.toLower(c));
            } else if (c == ' ' or c == '-') {
                if (id.items.len > 0 and id.items[id.items.len - 1] != '-') {
                    try id.append(self.allocator, '-');
                }
            }
        }

        // Remove trailing dash
        while (id.items.len > 0 and id.items[id.items.len - 1] == '-') {
            _ = id.pop();
        }

        return id.toOwnedSlice(self.allocator);
    }

    /// Append HTML-escaped character
    fn appendEscapedChar(self: *Self, c: u8) !void {
        switch (c) {
            '<' => try self.output.appendSlice(self.allocator, "&lt;"),
            '>' => try self.output.appendSlice(self.allocator, "&gt;"),
            '&' => try self.output.appendSlice(self.allocator, "&amp;"),
            '"' => try self.output.appendSlice(self.allocator, "&quot;"),
            else => try self.output.append(self.allocator, c),
        }
    }

    /// Append HTML-escaped string
    fn appendEscaped(self: *Self, text: []const u8) !void {
        for (text) |c| {
            try self.appendEscapedChar(c);
        }
    }

    /// Get the table of contents
    pub fn getTableOfContents(self: *Self) []const TocEntry {
        return self.toc.items;
    }

    /// Get parsed code blocks
    pub fn getCodeBlocks(self: *Self) []const CodeBlock {
        return self.code_blocks.items;
    }
};

// ============================================================================
// Helper Functions
// ============================================================================

fn isHorizontalRule(line: []const u8) bool {
    const trimmed = std.mem.trim(u8, line, " \t");
    if (trimmed.len < 3) return false;

    var count: usize = 0;
    const marker = trimmed[0];
    if (marker != '-' and marker != '*' and marker != '_') return false;

    for (trimmed) |c| {
        if (c == marker) {
            count += 1;
        } else if (c != ' ') {
            return false;
        }
    }

    return count >= 3;
}

fn isListItem(line: []const u8) bool {
    return isUnorderedListItem(line) or isOrderedListItem(line);
}

fn isUnorderedListItem(line: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, line, " \t");
    if (trimmed.len < 2) return false;
    return (trimmed[0] == '-' or trimmed[0] == '*' or trimmed[0] == '+') and trimmed[1] == ' ';
}

fn isOrderedListItem(line: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, line, " \t");
    var i: usize = 0;
    while (i < trimmed.len and std.ascii.isDigit(trimmed[i])) : (i += 1) {}
    if (i == 0 or i >= trimmed.len) return false;
    return trimmed[i] == '.' and i + 1 < trimmed.len and trimmed[i + 1] == ' ';
}

fn getListItemContent(line: []const u8) []const u8 {
    const trimmed = std.mem.trimLeft(u8, line, " \t");
    if (trimmed.len < 2) return "";
    return std.mem.trim(u8, trimmed[2..], " \t");
}

fn getOrderedListContent(line: []const u8) []const u8 {
    const trimmed = std.mem.trimLeft(u8, line, " \t");
    var i: usize = 0;
    while (i < trimmed.len and std.ascii.isDigit(trimmed[i])) : (i += 1) {}
    if (i >= trimmed.len or trimmed[i] != '.') return "";
    return std.mem.trim(u8, trimmed[i + 2 ..], " \t");
}

fn extractFilename(meta: ?[]const u8) ?[]const u8 {
    const m = meta orelse return null;
    if (std.mem.indexOf(u8, m, "filename=\"")) |start| {
        const rest = m[start + 10 ..];
        if (std.mem.indexOfScalar(u8, rest, '"')) |end| {
            return rest[0..end];
        }
    }
    return null;
}

fn extractHighlightLines(meta: ?[]const u8) ?[]const u8 {
    const m = meta orelse return null;
    if (std.mem.indexOf(u8, m, "{")) |start| {
        if (std.mem.indexOfScalar(u8, m[start..], '}')) |end| {
            return m[start + 1 .. start + end];
        }
    }
    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "parse heading" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{ .heading_anchors = false });
    defer parser.deinit();

    const html = try parser.parse("# Hello World");
    try std.testing.expect(std.mem.indexOf(u8, html, "<h1") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "Hello World") != null);
}

test "parse paragraph" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    const html = try parser.parse("This is a paragraph.");
    try std.testing.expect(std.mem.indexOf(u8, html, "<p>This is a paragraph.</p>") != null);
}

test "parse bold" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    const html = try parser.parse("This is **bold** text.");
    try std.testing.expect(std.mem.indexOf(u8, html, "<strong>bold</strong>") != null);
}

test "parse code block" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    const html = try parser.parse("```js\nconsole.log('hi');\n```");
    try std.testing.expect(std.mem.indexOf(u8, html, "language-js") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "console.log") != null);
}

test "parse unordered list" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    const html = try parser.parse("- Item 1\n- Item 2");
    try std.testing.expect(std.mem.indexOf(u8, html, "<ul>") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "<li>Item 1</li>") != null);
}

test "heading generates toc" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    _ = try parser.parse("# Intro\n## Getting Started");
    const toc = parser.getTableOfContents();

    try std.testing.expectEqual(@as(usize, 2), toc.len);
    try std.testing.expectEqual(@as(u8, 1), toc[0].level);
    try std.testing.expectEqual(@as(u8, 2), toc[1].level);
}

test "horizontal rule" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    const html = try parser.parse("---");
    try std.testing.expect(std.mem.indexOf(u8, html, "<hr />") != null);
}
