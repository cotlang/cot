//! Application runner for Cot TUI
//! Main event loop and screen management.

const std = @import("std");
const tui = @import("tui");
const Screen = @import("screen.zig").Screen;
const ScreenManager = @import("screen.zig").ScreenManager;
const Renderer = @import("renderer.zig").Renderer;
const Event = @import("events.zig").Event;
const EventType = @import("events.zig").EventType;
const KeyData = @import("events.zig").KeyData;
const Widget = @import("widgets.zig").Widget;
const widgets = @import("widgets.zig");

/// TUI Application
pub const App = struct {
    renderer: *Renderer,
    screen_manager: ScreenManager,
    running: bool = false,
    allocator: std.mem.Allocator,

    // Terminal input
    input: tui.Input,

    pub fn init(allocator: std.mem.Allocator) !*App {
        const app = try allocator.create(App);

        app.* = .{
            .renderer = try Renderer.init(allocator),
            .screen_manager = ScreenManager.init(allocator),
            .allocator = allocator,
            .input = tui.Input.init(),
        };

        return app;
    }

    pub fn deinit(self: *App) void {
        self.screen_manager.deinit();
        self.renderer.deinit();
        self.allocator.destroy(self);
    }

    /// Register a screen
    pub fn registerScreen(self: *App, screen: *Screen) !void {
        try self.screen_manager.register(screen);
    }

    /// Navigate to a screen by name
    pub fn navigate(self: *App, name: []const u8) !void {
        try self.screen_manager.navigate(name, null);
    }

    /// Go back to previous screen
    pub fn back(self: *App) !void {
        try self.screen_manager.back();
    }

    /// Run the application with the given initial screen
    pub fn run(self: *App, initial_screen: *Screen) !void {
        // Register and navigate to initial screen
        try self.registerScreen(initial_screen);
        try self.navigate(initial_screen.name);

        self.running = true;
        self.renderer.hideCursor();

        // Main event loop
        while (self.running) {
            // Render current screen
            if (self.screen_manager.getCurrent()) |screen| {
                if (screen.is_dirty) {
                    try self.renderer.render(screen);
                    screen.is_dirty = false;
                }

                // Handle input
                if (self.input.next()) |event| {
                    self.handleInputEvent(screen, event);
                }
            } else {
                // No current screen, exit
                self.running = false;
            }

            // Small sleep to prevent busy loop
            std.time.sleep(10 * std.time.ns_per_ms);
        }

        self.renderer.showCursor();
        self.renderer.clear();
    }

    /// Handle a TUI.input event
    fn handleInputEvent(self: *App, screen: *Screen, event: tui.Input.Event) void {
        switch (event) {
            .key => |key| {
                const cot_key = translateKey(key);
                const modifiers = KeyData.Modifiers{
                    .ctrl = key.ctrl,
                    .alt = key.alt,
                    .shift = key.shift,
                };

                // Check for quit (Ctrl+Q or Ctrl+C)
                if (key.ctrl and (key.char == 'q' or key.char == 'c')) {
                    self.running = false;
                    return;
                }

                // Pass to screen
                screen.handleKey(cot_key, modifiers);
            },
            .mouse => |mouse| {
                _ = mouse;
                // TODO: Handle mouse events
            },
            .resize => |size| {
                // Handle terminal resize
                _ = self.renderer.getSize();
                _ = size;
                if (self.screen_manager.getCurrent()) |s| {
                    s.markDirty();
                }
            },
        }
    }

    /// Stop the application
    pub fn quit(self: *App) void {
        self.running = false;
    }

    /// Get current screen
    pub fn getCurrentScreen(self: *App) ?*Screen {
        return self.screen_manager.getCurrent();
    }

    /// Get renderer
    pub fn getRenderer(self: *App) *Renderer {
        return self.renderer;
    }

    /// Show a toast message
    pub fn toast(self: *App, message: []const u8, duration_ms: u32) void {
        _ = self;
        _ = message;
        _ = duration_ms;
        // TODO: Implement toast overlay
    }
};

/// Translate TUI.zig key to Cot key
fn translateKey(key: tui.Input.Key) KeyData.Key {
    // Check for special keys first
    if (key.special) |special| {
        return .{ .special = switch (special) {
            .enter => .enter,
            .escape => .escape,
            .tab => .tab,
            .backspace => .backspace,
            .delete => .delete,
            .up => .up,
            .down => .down,
            .left => .left,
            .right => .right,
            .home => .home,
            .end => .end,
            .page_up => .page_up,
            .page_down => .page_down,
            .insert => .insert,
            else => return .{ .char = ' ' },
        } };
    }

    // Check for function keys
    if (key.function) |f| {
        return .{ .function = f };
    }

    // Regular character
    return .{ .char = key.char orelse ' ' };
}

// ============================================================================
// Builder pattern for creating screens programmatically
// ============================================================================

/// Screen builder for fluent API
pub const ScreenBuilder = struct {
    screen: *Screen,
    current_container: *Widget,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) !ScreenBuilder {
        const screen = try Screen.init(allocator, name);
        const root = try widgets.createWindow(allocator, "root", name, 0, 0);
        screen.setRoot(root);

        return .{
            .screen = screen,
            .current_container = root,
            .allocator = allocator,
        };
    }

    pub fn title(self: *ScreenBuilder, t: []const u8) *ScreenBuilder {
        self.screen.title = t;
        if (self.screen.root) |root| {
            root.data.window.title = t;
        }
        return self;
    }

    pub fn addLabel(self: *ScreenBuilder, id: []const u8, text: []const u8) !*ScreenBuilder {
        const label = try widgets.createLabel(self.allocator, id, text);
        try self.current_container.addChild(label);
        return self;
    }

    pub fn addInput(self: *ScreenBuilder, id: []const u8, width: u16) !*ScreenBuilder {
        const input = try widgets.createInput(self.allocator, id, width);
        try self.current_container.addChild(input);
        return self;
    }

    pub fn addButton(self: *ScreenBuilder, id: []const u8, label: []const u8) !*ScreenBuilder {
        const button = try widgets.createButton(self.allocator, id, label);
        try self.current_container.addChild(button);
        return self;
    }

    pub fn addTable(self: *ScreenBuilder, id: []const u8) !*ScreenBuilder {
        const table = try widgets.createTable(self.allocator, id);
        try self.current_container.addChild(table);
        return self;
    }

    pub fn addSeparator(self: *ScreenBuilder, id: []const u8) !*ScreenBuilder {
        const sep = try widgets.createSeparator(self.allocator, id);
        try self.current_container.addChild(sep);
        return self;
    }

    pub fn beginRow(self: *ScreenBuilder, id: []const u8) !*ScreenBuilder {
        const row = try widgets.createRow(self.allocator, id);
        try self.current_container.addChild(row);
        self.current_container = row;
        return self;
    }

    pub fn endRow(self: *ScreenBuilder) *ScreenBuilder {
        if (self.current_container.parent) |parent| {
            self.current_container = parent;
        }
        return self;
    }

    pub fn beginColumn(self: *ScreenBuilder, id: []const u8) !*ScreenBuilder {
        const col = try widgets.createColumn(self.allocator, id);
        try self.current_container.addChild(col);
        self.current_container = col;
        return self;
    }

    pub fn endColumn(self: *ScreenBuilder) *ScreenBuilder {
        if (self.current_container.parent) |parent| {
            self.current_container = parent;
        }
        return self;
    }

    pub fn beginPanel(self: *ScreenBuilder, id: []const u8, panel_title: []const u8) !*ScreenBuilder {
        const panel = try widgets.createPanel(self.allocator, id, panel_title);
        try self.current_container.addChild(panel);
        self.current_container = panel;
        return self;
    }

    pub fn endPanel(self: *ScreenBuilder) *ScreenBuilder {
        if (self.current_container.parent) |parent| {
            self.current_container = parent;
        }
        return self;
    }

    pub fn build(self: *ScreenBuilder) *Screen {
        return self.screen;
    }
};

/// Create a screen using builder pattern
pub fn createScreen(allocator: std.mem.Allocator, name: []const u8) !ScreenBuilder {
    return ScreenBuilder.init(allocator, name);
}
