//! Dex Page HTTP Handler
//!
//! Handles HTTP requests for Dex pages, compiling and rendering them on-demand.

const std = @import("std");
const Allocator = std.mem.Allocator;

const context = @import("context.zig");
const Context = context.Context;

// Dex imports
const dex_page = @import("../dex/page.zig");
const dex_document = @import("../dex/document.zig");
const dex_compiler = @import("../dex/compiler.zig");
const dex_renderer = @import("../dex/template/renderer.zig");
const dex_router = @import("../dex/router.zig");
const component_parser = @import("../dex/component_parser.zig");

/// Dex handler configuration
pub const DexHandlerConfig = struct {
    /// Directory containing page components
    pages_dir: []const u8 = "pages",
    /// Enable hot reload
    hot_reload: bool = true,
    /// Hot reload WebSocket port
    hot_reload_port: u16 = 3000,
    /// Development mode (show detailed errors)
    dev_mode: bool = true,
};

/// Dex page handler state
pub const DexHandler = struct {
    allocator: Allocator,
    config: DexHandlerConfig,
    router: dex_router.Router,
    page_router: dex_page.PageRouter,
    routes_discovered: bool,

    const Self = @This();

    pub fn init(allocator: Allocator, config: DexHandlerConfig) Self {
        return .{
            .allocator = allocator,
            .config = config,
            .router = dex_router.Router.init(allocator, config.pages_dir),
            .page_router = dex_page.PageRouter.init(allocator, config.pages_dir),
            .routes_discovered = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.router.deinit();
        self.page_router.deinit();
    }

    /// Handle an HTTP request for a Dex page
    pub fn handle(self: *Self, ctx: *Context) !void {
        // Discover routes on first request (lazy initialization)
        if (!self.routes_discovered) {
            self.router.discover() catch |err| {
                std.debug.print("Route discovery failed: {}\n", .{err});
            };
            self.routes_discovered = true;
        }

        const path = ctx.path();

        // Try to match the URL to a route using the new router
        if (self.router.match(path)) |match_result| {
            var match = match_result;
            defer match.deinit();

            // Get the full file path
            const file_path = self.router.getFullFilePath(match.route) catch {
                try self.render404(ctx, path);
                return;
            };
            defer self.allocator.free(file_path);

            // Render the page with route params
            self.renderPageWithParams(ctx, file_path, &match.params) catch |err| {
                try self.renderError(ctx, err, file_path);
            };
            return;
        }

        // Fall back to old page router for simple resolution
        const page_path = self.page_router.resolve(path) catch null orelse {
            // No page found - try custom 404 page first
            try self.render404(ctx, path);
            return;
        };
        defer self.allocator.free(page_path);

        // Load and render the page
        self.renderPage(ctx, page_path) catch |err| {
            try self.renderError(ctx, err, page_path);
        };
    }

    /// Render a 404 page - tries custom pages/404.dex first
    fn render404(self: *Self, ctx: *Context, url_path: []const u8) !void {
        // Try custom 404 page
        const custom_404_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/404.dx",
            .{self.config.pages_dir},
        );
        defer self.allocator.free(custom_404_path);

        if (fileExists(custom_404_path)) {
            // Render custom 404 page
            self.renderPage(ctx, custom_404_path) catch {
                // Fall back to default if custom page fails
                const html = try dex_document.render404(self.allocator, url_path, self.config.dev_mode);
                defer self.allocator.free(html);
                _ = ctx.status(404);
                try ctx.html(html);
            };
            _ = ctx.status(404);
            return;
        }

        // Default 404 page
        const html = try dex_document.render404(self.allocator, url_path, self.config.dev_mode);
        defer self.allocator.free(html);
        _ = ctx.status(404);
        try ctx.html(html);
    }

    /// Render a page component
    fn renderPage(self: *Self, ctx: *Context, page_path: []const u8) !void {
        // Load the page source
        const source = try readFile(self.allocator, page_path);
        defer self.allocator.free(source);

        // Create error context for detailed error reporting
        var error_context = component_parser.ParseErrorContext.init(self.allocator);
        error_context.setSource(source);
        error_context.setFilePath(page_path);

        // Compile the component with error context
        var compiler = dex_compiler.Compiler.init(self.allocator);
        var component = compiler.compileSourceWithContext(source, &error_context) catch |err| {
            // Render detailed error if available
            try self.renderDetailedError(ctx, err, page_path, &error_context);
            return;
        };
        defer component.deinit();

        // Create instance
        var instance = try component.createInstance();
        defer instance.deinit();

        // Build request context for props
        var req_ctx = dex_page.RequestContext.init(self.allocator);
        defer req_ctx.deinit();
        req_ctx.path = ctx.path();
        req_ctx.method = ctx.method().toString();

        // TODO: Parse query params from ctx
        // TODO: Copy headers from ctx
        // TODO: Call getServerProps if present

        // Render the component
        const content = try instance.render();
        defer self.allocator.free(content);

        // Wrap in document shell
        const html = try dex_document.render(self.allocator, content, .{
            .title = component.name,
            .hot_reload = self.config.hot_reload,
            .hot_reload_port = self.config.hot_reload_port,
        });
        defer self.allocator.free(html);

        try ctx.html(html);
    }

    /// Render a page component with route parameters
    fn renderPageWithParams(self: *Self, ctx: *Context, page_path: []const u8, route_params: *std.StringHashMap([]const u8)) !void {
        // Load the page source
        const source = try readFile(self.allocator, page_path);
        defer self.allocator.free(source);

        // Create error context for detailed error reporting
        var error_context = component_parser.ParseErrorContext.init(self.allocator);
        error_context.setSource(source);
        error_context.setFilePath(page_path);

        // Compile the component with error context
        var compiler = dex_compiler.Compiler.init(self.allocator);
        var component = compiler.compileSourceWithContext(source, &error_context) catch |err| {
            // Render detailed error if available
            try self.renderDetailedError(ctx, err, page_path, &error_context);
            return;
        };
        defer component.deinit();

        // Create instance
        var instance = try component.createInstance();
        defer instance.deinit();

        // Build request context for props with route params
        var req_ctx = dex_page.RequestContext.init(self.allocator);
        defer req_ctx.deinit();
        req_ctx.path = ctx.path();
        req_ctx.method = ctx.method().toString();

        // Copy route params to request context
        var param_iter = route_params.iterator();
        while (param_iter.next()) |entry| {
            try req_ctx.params.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        // TODO: Parse query params from ctx
        // TODO: Copy headers from ctx
        // TODO: Call getServerProps if present

        // Render the component
        const content = try instance.render();
        defer self.allocator.free(content);

        // Wrap in document shell
        const html = try dex_document.render(self.allocator, content, .{
            .title = component.name,
            .hot_reload = self.config.hot_reload,
            .hot_reload_port = self.config.hot_reload_port,
        });
        defer self.allocator.free(html);

        try ctx.html(html);
    }

    /// Render an error page - tries custom pages/_error.dex first
    fn renderError(self: *Self, ctx: *Context, err: anyerror, page_path: []const u8) !void {
        // Try custom error page
        const custom_error_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/_error.dx",
            .{self.config.pages_dir},
        );
        defer self.allocator.free(custom_error_path);

        if (fileExists(custom_error_path)) {
            // Try to render custom error page
            // TODO: Pass error info as props to the error component
            self.renderPage(ctx, custom_error_path) catch {
                // Fall back to default if custom error page also fails
                try self.renderDefaultError(ctx, err, page_path);
            };
            _ = ctx.status(500);
            return;
        }

        // Default error page
        try self.renderDefaultError(ctx, err, page_path);
    }

    /// Render the default error page
    fn renderDefaultError(self: *Self, ctx: *Context, err: anyerror, page_path: []const u8) !void {
        const detail = if (self.config.dev_mode)
            try std.fmt.allocPrint(self.allocator, "Error rendering: {s}\n\nError: {}", .{ page_path, err })
        else
            null;
        defer if (detail) |d| self.allocator.free(d);

        const html = try dex_document.renderError(
            self.allocator,
            500,
            "Internal Server Error",
            detail,
            self.config.dev_mode,
        );
        defer self.allocator.free(html);

        _ = ctx.status(500);
        try ctx.html(html);
    }

    /// Render a detailed error page with parse error context
    fn renderDetailedError(
        self: *Self,
        ctx: *Context,
        err: anyerror,
        page_path: []const u8,
        error_context: *component_parser.ParseErrorContext,
    ) !void {
        if (!self.config.dev_mode) {
            // In production, show generic error
            try self.renderDefaultError(ctx, err, page_path);
            return;
        }

        // Build detailed error message
        var detail_buf: std.ArrayListUnmanaged(u8) = .empty;
        defer detail_buf.deinit(self.allocator);

        const writer = detail_buf.writer(self.allocator);

        // Header with file path
        try writer.print("Parse Error in: {s}\n\n", .{page_path});

        // Add formatted error from error context
        if (error_context.last_error) |parse_err| {
            // Location
            try writer.print("Location: line {d}, column {d}\n", .{ parse_err.line, parse_err.column });

            // Error message
            try writer.print("Error: {s}\n\n", .{parse_err.message});

            // Source context with line numbers and caret
            if (parse_err.source_line) |line| {
                try writer.print("  |\n", .{});
                try writer.print("{d:>3} | {s}\n", .{ parse_err.line, line });
                try writer.print("    | ", .{});
                // Print spaces then caret
                var i: u32 = 1;
                while (i < parse_err.column) : (i += 1) {
                    try writer.writeByte(' ');
                }
                try writer.print("^\n", .{});
            }
        } else {
            // Fallback to basic error info
            try writer.print("Error: {}\n", .{err});
        }

        const detail = try detail_buf.toOwnedSlice(self.allocator);
        defer self.allocator.free(detail);

        const html = try dex_document.renderError(
            self.allocator,
            500,
            "Parse Error",
            detail,
            self.config.dev_mode,
        );
        defer self.allocator.free(html);

        _ = ctx.status(500);
        try ctx.html(html);
    }
};

/// Create a handler function for use with the HTTP server
pub fn createHandler(allocator: Allocator, config: DexHandlerConfig) HandlerWithState {
    return .{
        .handler = DexHandler.init(allocator, config),
    };
}

/// Handler with state wrapper
pub const HandlerWithState = struct {
    handler: DexHandler,

    pub fn handle(self: *HandlerWithState, ctx: *Context) !void {
        try self.handler.handle(ctx);
    }

    pub fn deinit(self: *HandlerWithState) void {
        self.handler.deinit();
    }
};

/// Read a file's contents
fn readFile(allocator: Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, 1024 * 1024);
}

/// Check if a file exists
fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "dex handler init" {
    const allocator = std.testing.allocator;

    var handler = DexHandler.init(allocator, .{});
    defer handler.deinit();

    try std.testing.expectEqualStrings("pages", handler.config.pages_dir);
}
