//! Cot TUI Framework
//! A modern, component-based terminal user interface system for Cot applications.
//! Uses TUI.zig as the rendering backend.

const std = @import("std");
const tui = @import("tui");

pub const Screen = @import("screen.zig").Screen;
pub const Layout = @import("layout.zig");
pub const Widget = @import("widgets.zig").Widget;
pub const WidgetType = @import("widgets.zig").WidgetType;
pub const Event = @import("events.zig").Event;
pub const EventType = @import("events.zig").EventType;
pub const Renderer = @import("renderer.zig").Renderer;
pub const App = @import("app.zig").App;

/// Style attributes for widgets
pub const Style = struct {
    bold: bool = false,
    dim: bool = false,
    italic: bool = false,
    underline: bool = false,
    blink: bool = false,
    reverse: bool = false,
    fg_color: ?Color = null,
    bg_color: ?Color = null,

    pub fn merge(self: Style, other: Style) Style {
        return .{
            .bold = other.bold or self.bold,
            .dim = other.dim or self.dim,
            .italic = other.italic or self.italic,
            .underline = other.underline or self.underline,
            .blink = other.blink or self.blink,
            .reverse = other.reverse or self.reverse,
            .fg_color = other.fg_color orelse self.fg_color,
            .bg_color = other.bg_color orelse self.bg_color,
        };
    }
};

/// Terminal colors
pub const Color = enum(u8) {
    black = 0,
    red = 1,
    green = 2,
    yellow = 3,
    blue = 4,
    magenta = 5,
    cyan = 6,
    white = 7,
    bright_black = 8,
    bright_red = 9,
    bright_green = 10,
    bright_yellow = 11,
    bright_blue = 12,
    bright_magenta = 13,
    bright_cyan = 14,
    bright_white = 15,
    default = 255,
};

/// Alignment for widgets
pub const Alignment = enum {
    left,
    center,
    right,
};

/// Border styles
pub const BorderStyle = enum {
    none,
    single,
    double,
    rounded,
    heavy,
};

/// Rect for positioning
pub const Rect = struct {
    x: u16,
    y: u16,
    width: u16,
    height: u16,

    pub fn contains(self: Rect, x: u16, y: u16) bool {
        return x >= self.x and x < self.x + self.width and
            y >= self.y and y < self.y + self.height;
    }
};

/// Initialize the TUI system
pub fn init(allocator: std.mem.Allocator) !*App {
    return App.init(allocator);
}

/// Run a screen
pub fn run(app: *App, screen: *Screen) !void {
    try app.run(screen);
}

test "tui module loads" {
    const allocator = std.testing.allocator;
    _ = allocator;
    // Basic smoke test
}
