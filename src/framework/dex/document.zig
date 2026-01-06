//! Dex HTML Document Shell
//!
//! Wraps page content in a full HTML document structure.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Document configuration
pub const DocumentConfig = struct {
    title: []const u8 = "Cot App",
    description: ?[]const u8 = null,
    lang: []const u8 = "en",
    charset: []const u8 = "utf-8",
    viewport: []const u8 = "width=device-width, initial-scale=1",
    favicon: ?[]const u8 = null,
    base_url: ?[]const u8 = null,

    // Development options
    hot_reload: bool = false,
    hot_reload_port: u16 = 3000,

    // Extra head content (styles, scripts)
    head_extra: ?[]const u8 = null,

    // Extra body attributes
    body_class: ?[]const u8 = null,
};

/// Render a full HTML document with the given content
pub fn render(allocator: Allocator, content: []const u8, config: DocumentConfig) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const writer = html.writer(allocator);

    // DOCTYPE and html open
    try writer.writeAll("<!DOCTYPE html>\n");
    try writer.print("<html lang=\"{s}\">\n", .{config.lang});

    // Head
    try writer.writeAll("<head>\n");
    try writer.print("  <meta charset=\"{s}\">\n", .{config.charset});
    try writer.print("  <meta name=\"viewport\" content=\"{s}\">\n", .{config.viewport});
    try writer.print("  <title>{s}</title>\n", .{config.title});

    if (config.description) |desc| {
        try writer.print("  <meta name=\"description\" content=\"{s}\">\n", .{desc});
    }

    if (config.favicon) |fav| {
        try writer.print("  <link rel=\"icon\" href=\"{s}\">\n", .{fav});
    }

    if (config.base_url) |base| {
        try writer.print("  <base href=\"{s}\">\n", .{base});
    }

    // Default styles for dev mode
    try writer.writeAll(
        \\  <style>
        \\    * { box-sizing: border-box; }
        \\    body { margin: 0; font-family: system-ui, -apple-system, sans-serif; }
        \\  </style>
        \\
    );

    if (config.head_extra) |extra| {
        try writer.print("  {s}\n", .{extra});
    }

    try writer.writeAll("</head>\n");

    // Body
    if (config.body_class) |cls| {
        try writer.print("<body class=\"{s}\">\n", .{cls});
    } else {
        try writer.writeAll("<body>\n");
    }

    // Main content wrapper
    try writer.writeAll("  <div id=\"__dex\">\n");
    try writer.print("    {s}\n", .{content});
    try writer.writeAll("  </div>\n");

    // Hydration script placeholder
    try writer.writeAll(
        \\  <script>
        \\    // Dex hydration will be added in Phase 6
        \\    window.__DEX_DATA__ = {};
        \\  </script>
        \\
    );

    // Hot reload script in dev mode
    if (config.hot_reload) {
        try writer.print(
            \\  <script>
            \\    (function() {{
            \\      const ws = new WebSocket('ws://localhost:{d}/__dex_reload');
            \\      ws.onmessage = function(e) {{
            \\        if (e.data === 'reload') window.location.reload();
            \\      }};
            \\      ws.onclose = function() {{
            \\        setTimeout(function() {{ window.location.reload(); }}, 1000);
            \\      }};
            \\    }})();
            \\  </script>
            \\
        , .{config.hot_reload_port});
    }

    try writer.writeAll("</body>\n");
    try writer.writeAll("</html>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a minimal error page
pub fn renderError(allocator: Allocator, status: u16, message: []const u8, detail: ?[]const u8, dev_mode: bool) ![]const u8 {
    var html: std.ArrayListUnmanaged(u8) = .empty;
    errdefer html.deinit(allocator);

    const writer = html.writer(allocator);

    try writer.writeAll("<!DOCTYPE html>\n<html>\n<head>\n");
    try writer.writeAll("  <meta charset=\"utf-8\">\n");
    try writer.writeAll("  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
    try writer.print("  <title>{d} - {s}</title>\n", .{ status, message });
    try writer.writeAll(
        \\  <style>
        \\    body {
        \\      font-family: system-ui;
        \\      display: flex;
        \\      align-items: center;
        \\      justify-content: center;
        \\      min-height: 100vh;
        \\      margin: 0;
        \\      background: #f8fafc;
        \\    }
        \\    .error { text-align: center; padding: 2rem; }
        \\    .status { font-size: 6rem; font-weight: bold; color: #94a3b8; margin: 0; }
        \\    .message { font-size: 1.5rem; color: #475569; margin: 1rem 0; }
        \\    .detail {
        \\      background: #1e293b;
        \\      color: #e2e8f0;
        \\      padding: 1rem;
        \\      border-radius: 0.5rem;
        \\      text-align: left;
        \\      font-family: monospace;
        \\      font-size: 0.875rem;
        \\      white-space: pre-wrap;
        \\      max-width: 600px;
        \\      overflow-x: auto;
        \\    }
        \\    a { color: #3b82f6; }
        \\  </style>
        \\</head>
        \\<body>
        \\  <div class="error">
        \\
    );

    try writer.print("    <p class=\"status\">{d}</p>\n", .{status});
    try writer.print("    <p class=\"message\">{s}</p>\n", .{message});

    if (dev_mode) {
        if (detail) |d| {
            try writer.print("    <pre class=\"detail\">{s}</pre>\n", .{d});
        }
    }

    try writer.writeAll("    <p><a href=\"/\">Go Home</a></p>\n");
    try writer.writeAll("  </div>\n</body>\n</html>\n");

    return html.toOwnedSlice(allocator);
}

/// Render a 404 page
pub fn render404(allocator: Allocator, path: []const u8, dev_mode: bool) ![]const u8 {
    const detail = if (dev_mode)
        try std.fmt.allocPrint(allocator, "No page found at: {s}\n\nCreate pages/index.dex or pages{s}.dex to handle this route.", .{ path, path })
    else
        null;
    defer if (detail) |d| allocator.free(d);

    return renderError(allocator, 404, "Page Not Found", detail, dev_mode);
}

// ============================================================================
// Tests
// ============================================================================

test "render basic document" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, "<h1>Hello</h1>", .{
        .title = "Test Page",
    });
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "<!DOCTYPE html>") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "<title>Test Page</title>") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "<h1>Hello</h1>") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "<div id=\"__dex\">") != null);
}

test "render with hot reload" {
    const allocator = std.testing.allocator;

    const html = try render(allocator, "<p>Content</p>", .{
        .hot_reload = true,
        .hot_reload_port = 3000,
    });
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "__dex_reload") != null);
}

test "render 404 page" {
    const allocator = std.testing.allocator;

    const html = try render404(allocator, "/missing", true);
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "404") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "Page Not Found") != null);
}
