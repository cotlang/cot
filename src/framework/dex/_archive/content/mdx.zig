//! MDX Parser
//!
//! Extends Markdown with JSX-like component syntax:
//!
//! ```mdx
//! # Hello World
//!
//! <Callout type="warning">
//! This is a warning
//! </Callout>
//!
//! <Tabs items={["Tab 1", "Tab 2"]}>
//!   <Tab>Content 1</Tab>
//!   <Tab>Content 2</Tab>
//! </Tabs>
//! ```
//!
//! Components are passed through to the Dex template renderer.

const std = @import("std");
const Allocator = std.mem.Allocator;
const markdown = @import("markdown.zig");
const frontmatter = @import("frontmatter.zig");

/// MDX parsing options
pub const Options = struct {
    /// Enable markdown processing
    markdown: bool = true,
    /// Markdown options
    markdown_options: markdown.Options = .{},
    /// Custom component handlers
    components: ?*const ComponentRegistry = null,
};

/// Component props from MDX
pub const ComponentProps = struct {
    /// Component name (e.g., "Callout", "Tabs")
    name: []const u8,
    /// Prop key-value pairs
    props: std.StringHashMap(PropValue),
    /// Children content (may be more MDX)
    children: []const u8,
    /// Self-closing component
    self_closing: bool,

    pub fn deinit(self: *ComponentProps, allocator: Allocator) void {
        self.props.deinit();
        _ = allocator;
    }

    /// Get a string prop
    pub fn getString(self: *const ComponentProps, key: []const u8) ?[]const u8 {
        if (self.props.get(key)) |val| {
            return switch (val) {
                .string => |s| s,
                else => null,
            };
        }
        return null;
    }

    /// Get a boolean prop
    pub fn getBool(self: *const ComponentProps, key: []const u8) ?bool {
        if (self.props.get(key)) |val| {
            return switch (val) {
                .boolean => |b| b,
                else => null,
            };
        }
        return null;
    }
};

/// Prop value types
pub const PropValue = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    array: []const PropValue,
    expression: []const u8, // {expression}
};

/// Component render function type
pub const ComponentRenderer = *const fn (allocator: Allocator, props: *const ComponentProps) anyerror![]const u8;

/// Registry of custom components
pub const ComponentRegistry = struct {
    renderers: std.StringHashMap(ComponentRenderer),

    pub fn init(allocator: Allocator) ComponentRegistry {
        return .{
            .renderers = std.StringHashMap(ComponentRenderer).init(allocator),
        };
    }

    pub fn deinit(self: *ComponentRegistry) void {
        self.renderers.deinit();
    }

    pub fn register(self: *ComponentRegistry, name: []const u8, renderer: ComponentRenderer) !void {
        try self.renderers.put(name, renderer);
    }

    pub fn get(self: *const ComponentRegistry, name: []const u8) ?ComponentRenderer {
        return self.renderers.get(name);
    }
};

/// MDX parse result
pub const ParseResult = struct {
    /// Parsed frontmatter
    frontmatter: frontmatter.Frontmatter,
    /// Rendered HTML content
    html: []const u8,
    /// Table of contents
    toc: []const markdown.TocEntry,
    /// Code blocks for syntax highlighting
    code_blocks: []const markdown.CodeBlock,
};

/// MDX Parser
pub const Parser = struct {
    allocator: Allocator,
    options: Options,
    output: std.ArrayListUnmanaged(u8),
    /// Rendered components stored by index
    component_outputs: std.ArrayListUnmanaged([]const u8),

    const Self = @This();

    /// Placeholder marker for components (uses chars markdown won't escape)
    const component_placeholder_prefix = "[[[MDXC:";
    const component_placeholder_suffix = ":MDXC]]]";

    pub fn init(allocator: Allocator, options: Options) Self {
        return .{
            .allocator = allocator,
            .options = options,
            .output = .empty,
            .component_outputs = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
        for (self.component_outputs.items) |html| {
            self.allocator.free(html);
        }
        self.component_outputs.deinit(self.allocator);
    }

    /// Parse MDX source and return HTML
    pub fn parse(self: *Self, source: []const u8) !ParseResult {
        // Parse frontmatter first
        const fm_result = try frontmatter.parse(self.allocator, source);
        var fm = fm_result.frontmatter;
        errdefer fm.deinit();

        const content = fm_result.content;

        // Process MDX content
        var md_parser = markdown.Parser.init(self.allocator, self.options.markdown_options);
        defer md_parser.deinit();

        // Process line by line, detecting components
        var lines = std.mem.splitScalar(u8, content, '\n');
        var processed: std.ArrayListUnmanaged(u8) = .empty;
        defer processed.deinit(self.allocator);

        var in_component: ?[]const u8 = null;
        var component_content: std.ArrayListUnmanaged(u8) = .empty;
        defer component_content.deinit(self.allocator);
        var component_depth: u32 = 0;

        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t");

            // Check for component start
            if (in_component == null) {
                if (self.parseComponentStart(trimmed)) |component_name| {
                    in_component = component_name;
                    component_depth = 1;

                    // Check if self-closing
                    if (std.mem.endsWith(u8, trimmed, "/>")) {
                        // Self-closing component
                        try self.renderComponent(&processed, trimmed);
                        in_component = null;
                        component_depth = 0;
                    } else {
                        try component_content.appendSlice(self.allocator, line);
                        try component_content.append(self.allocator, '\n');
                    }
                    continue;
                }
            }

            // Inside component
            if (in_component) |comp_name| {
                // Check for nested same component
                if (self.parseComponentStart(trimmed)) |nested_name| {
                    if (std.mem.eql(u8, nested_name, comp_name)) {
                        if (!std.mem.endsWith(u8, trimmed, "/>")) {
                            component_depth += 1;
                        }
                    }
                }

                // Check for component end
                if (self.parseComponentEnd(trimmed, comp_name)) {
                    component_depth -= 1;
                    if (component_depth == 0) {
                        try component_content.appendSlice(self.allocator, line);
                        // Render the complete component
                        try self.renderComponent(&processed, component_content.items);
                        component_content.clearRetainingCapacity();
                        in_component = null;
                        continue;
                    }
                }

                try component_content.appendSlice(self.allocator, line);
                try component_content.append(self.allocator, '\n');
                continue;
            }

            // Regular markdown line
            try processed.appendSlice(self.allocator, line);
            try processed.append(self.allocator, '\n');
        }

        // Parse the processed markdown
        const html_with_placeholders = try md_parser.parse(processed.items);

        // Replace placeholders with actual component HTML
        const html = try self.replacePlaceholders(html_with_placeholders);
        errdefer self.allocator.free(html);

        // Copy TOC entries (note: these reference source strings, not owned)
        const toc = try self.allocator.alloc(markdown.TocEntry, md_parser.toc.items.len);
        @memcpy(toc, md_parser.toc.items);

        return .{
            .frontmatter = fm,
            .html = html,
            .toc = toc,
            .code_blocks = &.{},
        };
    }

    /// Replace component placeholders with actual HTML
    fn replacePlaceholders(self: *Self, html: []const u8) ![]const u8 {
        var result: std.ArrayListUnmanaged(u8) = .empty;
        errdefer result.deinit(self.allocator);

        var pos: usize = 0;
        while (pos < html.len) {
            // Look for placeholder start
            if (std.mem.indexOf(u8, html[pos..], component_placeholder_prefix)) |rel_start| {
                const start = pos + rel_start;
                // Copy everything before the placeholder
                try result.appendSlice(self.allocator, html[pos..start]);

                // Find placeholder end
                const after_prefix = start + component_placeholder_prefix.len;
                if (std.mem.indexOf(u8, html[after_prefix..], component_placeholder_suffix)) |rel_end| {
                    const index_str = html[after_prefix .. after_prefix + rel_end];
                    const index = std.fmt.parseInt(usize, index_str, 10) catch 0;

                    // Insert component HTML
                    if (index < self.component_outputs.items.len) {
                        try result.appendSlice(self.allocator, self.component_outputs.items[index]);
                    }

                    pos = after_prefix + rel_end + component_placeholder_suffix.len;
                } else {
                    // Malformed placeholder, copy as-is
                    try result.append(self.allocator, html[start]);
                    pos = start + 1;
                }
            } else {
                // No more placeholders, copy rest
                try result.appendSlice(self.allocator, html[pos..]);
                break;
            }
        }

        return result.toOwnedSlice(self.allocator);
    }

    /// Check if line starts a component tag
    fn parseComponentStart(self: *Self, line: []const u8) ?[]const u8 {
        _ = self;
        if (line.len < 2) return null;
        if (line[0] != '<') return null;

        // Must start with uppercase (JSX component convention)
        if (line.len < 2 or !std.ascii.isUpper(line[1])) return null;

        // Extract component name
        var end: usize = 1;
        while (end < line.len and (std.ascii.isAlphanumeric(line[end]) or line[end] == '_')) {
            end += 1;
        }

        return line[1..end];
    }

    /// Check if line ends a component
    fn parseComponentEnd(self: *Self, line: []const u8, comp_name: []const u8) bool {
        _ = self;
        if (line.len < comp_name.len + 3) return false;

        // Check for </ComponentName>
        if (!std.mem.startsWith(u8, line, "</")) return false;
        if (!std.mem.startsWith(u8, line[2..], comp_name)) return false;

        const after = line[2 + comp_name.len ..];
        const trimmed = std.mem.trim(u8, after, " \t");
        return std.mem.startsWith(u8, trimmed, ">");
    }

    /// Render a component to HTML, storing it for later placeholder replacement
    fn renderComponent(self: *Self, output: *std.ArrayListUnmanaged(u8), component_src: []const u8) !void {
        // Parse the component
        var props = try self.parseComponentTag(component_src);
        defer props.props.deinit();

        // Build the component HTML
        var component_html: std.ArrayListUnmanaged(u8) = .empty;
        errdefer component_html.deinit(self.allocator);

        // Check for custom renderer
        if (self.options.components) |registry| {
            if (registry.get(props.name)) |renderer| {
                const html = try renderer(self.allocator, &props);
                try component_html.appendSlice(self.allocator, html);
                self.allocator.free(html);
            }
        } else {
            // Default: pass through as custom element (for client-side handling)
            try component_html.appendSlice(self.allocator, "<div data-mdx-component=\"");
            try component_html.appendSlice(self.allocator, props.name);
            try component_html.appendSlice(self.allocator, "\"");

            // Add props as data attributes
            var it = props.props.iterator();
            while (it.next()) |entry| {
                try component_html.appendSlice(self.allocator, " data-");
                try component_html.appendSlice(self.allocator, entry.key_ptr.*);
                try component_html.appendSlice(self.allocator, "=\"");
                switch (entry.value_ptr.*) {
                    .string => |s| try component_html.appendSlice(self.allocator, s),
                    .boolean => |b| try component_html.appendSlice(self.allocator, if (b) "true" else "false"),
                    .number => |n| {
                        var buf: [32]u8 = undefined;
                        const fmt_result = std.fmt.bufPrint(&buf, "{d}", .{n}) catch "";
                        try component_html.appendSlice(self.allocator, fmt_result);
                    },
                    else => {},
                }
                try component_html.appendSlice(self.allocator, "\"");
            }

            try component_html.appendSlice(self.allocator, ">");

            // Process children (may contain more markdown/mdx)
            if (props.children.len > 0) {
                var md_parser = markdown.Parser.init(self.allocator, self.options.markdown_options);
                defer md_parser.deinit();
                const children_html = try md_parser.parse(props.children);
                try component_html.appendSlice(self.allocator, children_html);
            }

            try component_html.appendSlice(self.allocator, "</div>");
        }

        // Store the HTML and write a placeholder
        const index = self.component_outputs.items.len;
        try self.component_outputs.append(self.allocator, try component_html.toOwnedSlice(self.allocator));

        // Write placeholder to output (will be replaced after markdown parsing)
        try output.appendSlice(self.allocator, component_placeholder_prefix);
        var idx_buf: [16]u8 = undefined;
        const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{index}) catch "0";
        try output.appendSlice(self.allocator, idx_str);
        try output.appendSlice(self.allocator, component_placeholder_suffix);
        try output.append(self.allocator, '\n');
    }

    /// Parse component tag into props
    fn parseComponentTag(self: *Self, src: []const u8) !ComponentProps {
        var props = ComponentProps{
            .name = "",
            .props = std.StringHashMap(PropValue).init(self.allocator),
            .children = "",
            .self_closing = false,
        };

        // Find first line (opening tag)
        const first_newline = std.mem.indexOfScalar(u8, src, '\n') orelse src.len;
        const opening = std.mem.trim(u8, src[0..first_newline], " \t\r");

        // Extract component name
        if (opening.len < 2 or opening[0] != '<') return props;
        var name_end: usize = 1;
        while (name_end < opening.len and (std.ascii.isAlphanumeric(opening[name_end]) or opening[name_end] == '_')) {
            name_end += 1;
        }
        props.name = opening[1..name_end];

        // Parse props from opening tag
        var remaining = opening[name_end..];
        while (remaining.len > 0) {
            remaining = std.mem.trimLeft(u8, remaining, " \t");
            if (remaining.len == 0 or remaining[0] == '>' or remaining[0] == '/') break;

            // Parse prop name
            var prop_end: usize = 0;
            while (prop_end < remaining.len and (std.ascii.isAlphanumeric(remaining[prop_end]) or remaining[prop_end] == '_' or remaining[prop_end] == '-')) {
                prop_end += 1;
            }
            if (prop_end == 0) break;

            const prop_name = remaining[0..prop_end];
            remaining = remaining[prop_end..];

            // Check for =
            remaining = std.mem.trimLeft(u8, remaining, " \t");
            if (remaining.len > 0 and remaining[0] == '=') {
                remaining = remaining[1..];
                remaining = std.mem.trimLeft(u8, remaining, " \t");

                // Parse value
                if (remaining.len > 0) {
                    if (remaining[0] == '"' or remaining[0] == '\'') {
                        // String value
                        const quote = remaining[0];
                        remaining = remaining[1..];
                        const end = std.mem.indexOfScalar(u8, remaining, quote) orelse remaining.len;
                        try props.props.put(prop_name, .{ .string = remaining[0..end] });
                        remaining = if (end < remaining.len) remaining[end + 1 ..] else "";
                    } else if (remaining[0] == '{') {
                        // Expression value
                        const end = std.mem.indexOfScalar(u8, remaining, '}') orelse remaining.len;
                        try props.props.put(prop_name, .{ .expression = remaining[1..end] });
                        remaining = if (end < remaining.len) remaining[end + 1 ..] else "";
                    } else if (std.mem.startsWith(u8, remaining, "true")) {
                        try props.props.put(prop_name, .{ .boolean = true });
                        remaining = remaining[4..];
                    } else if (std.mem.startsWith(u8, remaining, "false")) {
                        try props.props.put(prop_name, .{ .boolean = false });
                        remaining = remaining[5..];
                    }
                }
            } else {
                // Boolean prop (just name = true)
                try props.props.put(prop_name, .{ .boolean = true });
            }
        }

        // Check if self-closing
        props.self_closing = std.mem.endsWith(u8, std.mem.trimRight(u8, opening, " \t"), "/>");

        // Extract children (content between opening and closing tags)
        if (!props.self_closing and first_newline < src.len) {
            const rest = src[first_newline + 1 ..];
            // Find closing tag
            const close_tag_start = std.mem.lastIndexOf(u8, rest, "</");
            if (close_tag_start) |pos| {
                props.children = std.mem.trim(u8, rest[0..pos], " \t\r\n");
            } else {
                props.children = std.mem.trim(u8, rest, " \t\r\n");
            }
        }

        return props;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "parse mdx with component" {
    const allocator = std.testing.allocator;

    const source =
        \\---
        \\title: Test
        \\---
        \\
        \\# Hello
        \\
        \\<Callout type="warning">
        \\This is a warning
        \\</Callout>
        \\
        \\More content
    ;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    var result = try parser.parse(source);
    defer result.frontmatter.deinit();
    defer allocator.free(result.toc);
    defer allocator.free(result.html);

    try std.testing.expect(std.mem.indexOf(u8, result.html, "data-mdx-component=\"Callout\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.html, "data-type=\"warning\"") != null);
}

test "parse self-closing component" {
    const allocator = std.testing.allocator;

    const source =
        \\# Hello
        \\
        \\<Divider />
        \\
        \\More content
    ;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    var result = try parser.parse(source);
    defer result.frontmatter.deinit();
    defer allocator.free(result.toc);
    defer allocator.free(result.html);

    try std.testing.expect(std.mem.indexOf(u8, result.html, "data-mdx-component=\"Divider\"") != null);
}

test "parse component props" {
    const allocator = std.testing.allocator;

    var parser = Parser.init(allocator, .{});
    defer parser.deinit();

    var props = try parser.parseComponentTag("<Button variant=\"primary\" disabled>\nClick me\n</Button>");
    defer props.props.deinit();

    try std.testing.expectEqualStrings("Button", props.name);
    try std.testing.expectEqualStrings("primary", props.getString("variant").?);
    try std.testing.expect(props.getBool("disabled").?);
    try std.testing.expectEqualStrings("Click me", props.children);
}
