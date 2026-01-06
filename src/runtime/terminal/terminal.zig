//! Terminal Kit - Minimal terminal control for TUI applications
//!
//! Provides low-level terminal primitives:
//! - Raw mode control (disable line buffering, echo)
//! - Terminal size detection
//! - Alternate screen buffer
//! - Mouse tracking
//!
//! This is a foundation for pure-Cot TUI packages after self-hosting.

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;

pub const event = @import("event.zig");

// Re-export event types
pub const Event = event.Event;
pub const Key = event.Key;
pub const KeyCode = event.KeyCode;
pub const Mouse = event.Mouse;
pub const MouseButton = event.MouseButton;
pub const Modifiers = event.Modifiers;

/// Terminal size in columns and rows
pub const Size = struct {
    cols: u16,
    rows: u16,
};

/// Saved terminal state for restoration
pub const RawState = struct {
    original: posix.termios,
    handle: posix.fd_t,

    /// Restore terminal to original state
    pub fn restore(self: *RawState) void {
        posix.tcsetattr(self.handle, .FLUSH, self.original) catch {};
    }
};

/// Error types for terminal operations
pub const TerminalError = error{
    UnsupportedPlatform,
    IoctlFailed,
    TcgetattrFailed,
    TcsetattrFailed,
};

/// Enable raw mode on the terminal
/// Returns saved state that must be used to restore normal mode
pub fn enableRawMode(handle: posix.fd_t) TerminalError!RawState {
    if (comptime !isPosix()) {
        return TerminalError.UnsupportedPlatform;
    }

    const original = posix.tcgetattr(handle) catch return TerminalError.TcgetattrFailed;

    var raw = original;

    // Input flags: disable break, CR->NL, parity, strip, flow control
    raw.iflag.BRKINT = false;
    raw.iflag.ICRNL = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;
    raw.iflag.IXON = false;

    // Output flags: disable post-processing
    raw.oflag.OPOST = false;

    // Local flags: disable echo, canonical mode, signals, extended input
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.IEXTEN = false;
    raw.lflag.ISIG = false;

    // Control flags: 8-bit chars
    raw.cflag.CSIZE = .CS8;

    // Read returns after 1 byte, no timeout
    raw.cc[@intFromEnum(posix.V.MIN)] = 1;
    raw.cc[@intFromEnum(posix.V.TIME)] = 0;

    posix.tcsetattr(handle, .FLUSH, raw) catch return TerminalError.TcsetattrFailed;

    return RawState{
        .original = original,
        .handle = handle,
    };
}

/// Get terminal size in columns and rows
pub fn getSize(handle: posix.fd_t) TerminalError!Size {
    if (comptime !isPosix()) {
        return TerminalError.UnsupportedPlatform;
    }

    var ws: posix.winsize = undefined;
    const err = posix.system.ioctl(handle, posix.T.IOCGWINSZ, @intFromPtr(&ws));

    if (posix.errno(err) != .SUCCESS) {
        return TerminalError.IoctlFailed;
    }

    return Size{
        .cols = ws.col,
        .rows = ws.row,
    };
}

// ANSI escape sequences as comptime constants
pub const seq = struct {
    /// Enter alternate screen buffer
    pub const alt_screen_enter = "\x1b[?1049h";
    /// Exit alternate screen buffer
    pub const alt_screen_exit = "\x1b[?1049l";

    /// Enable mouse tracking (all events including movement)
    pub const mouse_enable = "\x1b[?1003h";
    /// Disable mouse tracking
    pub const mouse_disable = "\x1b[?1003l";

    /// Hide cursor
    pub const cursor_hide = "\x1b[?25l";
    /// Show cursor
    pub const cursor_show = "\x1b[?25h";

    /// Clear entire screen
    pub const clear_all = "\x1b[2J";
    /// Clear from cursor to end of screen
    pub const clear_below = "\x1b[0J";
    /// Clear from cursor to beginning of screen
    pub const clear_above = "\x1b[1J";

    /// Clear entire line
    pub const clear_line = "\x1b[2K";
    /// Clear from cursor to end of line
    pub const clear_line_right = "\x1b[0K";
    /// Clear from cursor to beginning of line
    pub const clear_line_left = "\x1b[1K";

    /// Move cursor to home (1,1)
    pub const cursor_home = "\x1b[H";
    /// Save cursor position
    pub const cursor_save = "\x1b[s";
    /// Restore cursor position
    pub const cursor_restore = "\x1b[u";

    /// Reset all attributes
    pub const reset = "\x1b[0m";
    /// Bold
    pub const bold = "\x1b[1m";
    /// Dim
    pub const dim = "\x1b[2m";
    /// Italic
    pub const italic = "\x1b[3m";
    /// Underline
    pub const underline = "\x1b[4m";
    /// Reverse video
    pub const reverse = "\x1b[7m";
};

/// Format cursor position command into buffer
/// Returns slice of buffer containing the escape sequence
pub fn cursorTo(buf: []u8, col: u16, row: u16) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[{d};{d}H", .{ row, col }) catch buf[0..0];
}

/// Format cursor up command
pub fn cursorUp(buf: []u8, n: u16) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[{d}A", .{n}) catch buf[0..0];
}

/// Format cursor down command
pub fn cursorDown(buf: []u8, n: u16) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[{d}B", .{n}) catch buf[0..0];
}

/// Format cursor right command
pub fn cursorRight(buf: []u8, n: u16) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[{d}C", .{n}) catch buf[0..0];
}

/// Format cursor left command
pub fn cursorLeft(buf: []u8, n: u16) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[{d}D", .{n}) catch buf[0..0];
}

/// Format 256-color foreground
pub fn fg256(buf: []u8, color: u8) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[38;5;{d}m", .{color}) catch buf[0..0];
}

/// Format 256-color background
pub fn bg256(buf: []u8, color: u8) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[48;5;{d}m", .{color}) catch buf[0..0];
}

/// Format RGB foreground
pub fn fgRgb(buf: []u8, r: u8, g: u8, b: u8) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[38;2;{d};{d};{d}m", .{ r, g, b }) catch buf[0..0];
}

/// Format RGB background
pub fn bgRgb(buf: []u8, r: u8, g: u8, b: u8) []const u8 {
    return std.fmt.bufPrint(buf, "\x1b[48;2;{d};{d};{d}m", .{ r, g, b }) catch buf[0..0];
}

fn isPosix() bool {
    return switch (builtin.os.tag) {
        .linux, .macos, .freebsd, .netbsd, .openbsd, .dragonfly => true,
        else => false,
    };
}

test "getSize returns valid dimensions" {
    if (comptime !isPosix()) return error.SkipZigTest;

    // Only test if we have a real terminal
    const handle = std.io.getStdIn().handle;
    if (getSize(handle)) |size| {
        try std.testing.expect(size.cols > 0);
        try std.testing.expect(size.rows > 0);
    } else |_| {
        // Not a terminal, skip
    }
}

test "cursorTo formats correctly" {
    var buf: [32]u8 = undefined;
    const result = cursorTo(&buf, 10, 20);
    try std.testing.expectEqualStrings("\x1b[20;10H", result);
}
