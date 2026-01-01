//! TUI Runtime for Cot
//!
//! Provides a TUI implementation using TUI.zig's Terminal module.
//! This wraps the terminal handling for immediate-mode drawing via xcalls.
//!
//! This is part of the cot-tui extension package.

const std = @import("std");
const builtin = @import("builtin");
const tui = @import("tui");

// ============================================================================
// Error Types - Proper error handling for debugging
// ============================================================================

/// TUI Error type with context for proper error propagation
pub const TuiError = error{
    /// TUI system not initialized - call t_init first
    NotInitialized,
    /// Terminal device error (write/read failed)
    TerminalError,
    /// Invalid argument passed to TUI function
    InvalidArgument,
    /// Drawing operation would be out of screen bounds
    OutOfBounds,
    /// Memory allocation failed
    OutOfMemory,
    /// Terminal not available (no TTY)
    NoTerminal,
};

/// Rich error context for debugging - stores last error details
pub const TuiErrorContext = struct {
    operation: []const u8 = "",
    file: []const u8 = "",
    line: u32 = 0,
    detail: []const u8 = "",

    pub fn format(self: TuiErrorContext, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("TUI Error in {s}", .{self.operation});
        if (self.file.len > 0) {
            try writer.print(" at {s}:{d}", .{ self.file, self.line });
        }
        if (self.detail.len > 0) {
            try writer.print(": {s}", .{self.detail});
        }
    }
};

/// Global error context - set when an error occurs
pub var last_tui_error: ?TuiErrorContext = null;

/// Set error context for debugging
fn setError(operation: []const u8, src: std.builtin.SourceLocation, detail: []const u8) void {
    last_tui_error = .{
        .operation = operation,
        .file = src.file,
        .line = src.line,
        .detail = detail,
    };
}

/// Get the last TUI error context (for CLI to display)
pub fn getLastError() ?TuiErrorContext {
    return last_tui_error;
}

/// Clear the last error
pub fn clearLastError() void {
    last_tui_error = null;
}

// Central crash handler - provides signal handling with stack traces
const crash = @import("../crash.zig");

/// TUI cleanup callback - called by central crash handler
fn tuiCleanupCallback() void {
    // Deinit terminal state if it was initialized
    if (global_tui) |*state| {
        if (state.terminal) |*term| {
            term.deinit();
        }
        state.terminal = null;
    }
}

// Re-export key types from TUI.zig
pub const Terminal = tui.terminal.Terminal;
pub const TerminalConfig = tui.terminal.TerminalConfig;
pub const Escape = tui.terminal.Escape;
pub const Key = tui.Key;
pub const Event = tui.Event;
pub const InputReader = tui.input.InputReader;

/// Dev console log entry
pub const DevLogEntry = struct {
    message: []const u8,
    timestamp: i64,
};

/// Global TUI state
pub const TuiState = struct {
    terminal: ?Terminal = null,
    input_reader: ?InputReader = null,
    allocator: std.mem.Allocator,
    initialized: bool = false,

    // Current colors (for setStyle)
    current_fg: u8 = 7, // white
    current_bg: u8 = 0, // black
    is_bold: bool = false,
    is_underline: bool = false,

    // Dev mode state
    dev_mode: bool = false,
    app_width: u16 = 0, // Width of app pane (0 = full screen)
    dev_log_lines: [64][]const u8 = undefined, // Circular buffer for log lines
    dev_log_count: usize = 0,
    dev_log_start: usize = 0, // Start index in circular buffer

    // Hot-reload state
    watch_file_path: ?[]const u8 = null, // Path to bytecode file to watch
    watch_file_mtime: i128 = 0, // Last known modification time (ns since epoch)
    reload_pending: bool = false, // True when file changed and reload needed
    poll_counter: u32 = 0, // Counter for file check frequency
};

/// Global TUI instance (singleton for xcall access)
var global_tui: ?TuiState = null;

/// Global dev mode flag - set before TUI init to enable dev mode
var pending_dev_mode: bool = false;

/// Global watch file path - set before TUI init for hot-reload
var pending_watch_file: ?[]const u8 = null;

/// Global reload pending flag - preserved even after TUI deinit
var global_reload_pending: bool = false;

/// Global force reload flag - set when user presses hot-reload key (Ctrl+R)
var global_force_reload: bool = false;

/// Set pending dev mode (call before app starts)
pub fn setPendingDevMode(enabled: bool) void {
    pending_dev_mode = enabled;
}

/// Set pending watch file (call before app starts, will be applied after TUI init)
pub fn setPendingWatchFile(path: []const u8) void {
    pending_watch_file = path;
}

/// Clear pending watch file
pub fn clearPendingWatchFile() void {
    pending_watch_file = null;
}

/// Initialize the TUI system
/// If terminal initialization fails (e.g., no TTY), TUI operates in no-op mode
/// If TUI is disabled at build time, runs in headless mode (for debugging)
pub fn init(allocator: std.mem.Allocator) void {
    const vm = @import("../bytecode/vm.zig");

    if (global_tui != null and global_tui.?.initialized) {
        return; // Already initialized
    }

    var state = TuiState{
        .allocator = allocator,
    };

    // Install central crash handlers and register TUI cleanup callback
    crash.installHandlers();
    _ = crash.registerCleanup(tuiCleanupCallback);

    // Check if we have a TTY before attempting to initialize the terminal
    // This prevents panics in the TUI library when running without a terminal
    const stdin_file = std.fs.File{ .handle = std.posix.STDIN_FILENO };
    if (!stdin_file.isTty()) {
        std.debug.print("[TUI] No TTY detected, running in no-op mode\n", .{});
        state.initialized = true;
        global_tui = state;
        return;
    }

    // Additional defensive check: verify stdout is also a TTY
    const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
    if (!stdout_file.isTty()) {
        std.debug.print("[TUI] stdout is not a TTY, running in no-op mode\n", .{});
        state.initialized = true;
        global_tui = state;
        return;
    }

    // Track native context for crash reporting
    vm.enterNativeContext("tui.terminal.Terminal.init()", @src());
    defer vm.exitNativeContext();

    // Create terminal with TUI mode - if this fails, we run in no-op mode
    state.terminal = tui.terminal.Terminal.init(.{
        .alternate_screen = true,
        .hide_cursor = true,
        .enable_mouse = false,
        .enable_paste = false,
        .enable_focus = false,
    }) catch |err| blk: {
        std.debug.print("[TUI] Terminal init failed: {}, running in no-op mode\n", .{err});
        break :blk null;
    };

    if (state.terminal != null) {
        vm.enterNativeContext("tui.input.InputReader.init()", @src());
        state.input_reader = tui.input.InputReader.init(allocator);
        vm.exitNativeContext();
    }
    state.initialized = true;
    global_tui = state;

    // Check if dev mode was requested before init
    if (pending_dev_mode) {
        setDevMode(true);
        pending_dev_mode = false;
    }

    // Check if watch file was requested before init (for hot-reload)
    if (pending_watch_file) |path| {
        setWatchFile(path);
        // Don't clear pending_watch_file - we need it to persist for reloads
    }
}

/// Shutdown the TUI system
pub fn deinit() void {
    if (global_tui) |*state| {
        if (state.terminal) |*term| {
            term.deinit();
        }
        state.initialized = false;
    }
    global_tui = null;
}

/// Check if TUI is initialized
pub fn isInitialized() bool {
    return global_tui != null and global_tui.?.initialized;
}

/// Get the current TUI state
pub fn getState() ?*TuiState {
    if (global_tui) |*state| {
        return state;
    }
    return null;
}

/// Clear the screen (only clears app pane in dev mode)
pub fn clear() void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (state.dev_mode and state.app_width > 0) {
                // Only clear the app pane area
                const size = getScreenSize();
                var y: u16 = 0;
                while (y < size.height) : (y += 1) {
                    term.moveCursor(0, y) catch {};
                    var x: u16 = 0;
                    while (x < state.app_width) : (x += 1) {
                        term.write(" ") catch {};
                    }
                }
                // Redraw dev pane
                drawDevPane();
            } else {
                term.clear() catch {};
            }
        }
    }
}

/// Set the current drawing style
pub fn setStyle(fg: u8, bg: u8, bold: bool, underline: bool) void {

    if (getState()) |state| {
        state.current_fg = fg;
        state.current_bg = bg;
        state.is_bold = bold;
        state.is_underline = underline;

        if (state.terminal) |*term| {
            applyStyle(term, fg, bg, bold, underline) catch {};
        }
    }
}

/// Apply style to terminal
fn applyStyle(term: anytype, fg: u8, bg: u8, bold: bool, underline: bool) !void {

    // Reset first
    try term.reset();

    // Apply attributes
    if (bold) {
        try term.write(tui.terminal.Escape.bold);
    }
    if (underline) {
        try term.write(tui.terminal.Escape.underline);
    }

    // Apply foreground color (using 256-color palette)
    var buf: [32]u8 = undefined;
    var output = std.fmt.bufPrint(&buf, "\x1b[38;5;{d}m", .{fg}) catch return;
    try term.write(output);

    // Apply background color
    output = std.fmt.bufPrint(&buf, "\x1b[48;5;{d}m", .{bg}) catch return;
    try term.write(output);
}

/// Reset style to default
pub fn resetStyle() void {

    if (getState()) |state| {
        state.current_fg = 7;
        state.current_bg = 0;
        state.is_bold = false;
        state.is_underline = false;

        if (state.terminal) |*term| {
            term.reset() catch {};
        }
    }
}

/// Draw text at position (clips to app pane in dev mode)
pub fn drawText(x: u16, y: u16, text: []const u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, clip to app pane width
            if (state.dev_mode and state.app_width > 0) {
                if (x >= state.app_width) return; // Off screen
                const max_len = state.app_width - x;
                const display_len = @min(text.len, max_len);
                term.moveCursor(x, y) catch {};
                term.write(text[0..display_len]) catch {};
            } else {
                term.moveCursor(x, y) catch {};
                term.write(text) catch {};
            }
        }
    }
}

/// Draw a box/border (clips to app pane in dev mode)
pub fn drawBox(x: u16, y: u16, width: u16, height: u16, style_type: u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, clip width to app pane
            const max_width = if (state.dev_mode and state.app_width > 0 and x < state.app_width)
                @min(width, state.app_width - x)
            else
                width;

            if (max_width < 2) return; // Too narrow to draw

            // Box drawing characters
            const tl: []const u8 = if (style_type == 1) "\xe2\x95\x94" else "\xe2\x94\x8c"; // ╔ or ┌
            const tr: []const u8 = if (style_type == 1) "\xe2\x95\x97" else "\xe2\x94\x90"; // ╗ or ┐
            const bl: []const u8 = if (style_type == 1) "\xe2\x95\x9a" else "\xe2\x94\x94"; // ╚ or └
            const br: []const u8 = if (style_type == 1) "\xe2\x95\x9d" else "\xe2\x94\x98"; // ╝ or ┘
            const h: []const u8 = if (style_type == 1) "\xe2\x95\x90" else "\xe2\x94\x80"; // ═ or ─
            const v: []const u8 = if (style_type == 1) "\xe2\x95\x91" else "\xe2\x94\x82"; // ║ or │

            // Top edge
            term.moveCursor(x, y) catch {};
            term.write(tl) catch {};
            var i: u16 = 0;
            while (i < max_width -| 2) : (i += 1) {
                term.write(h) catch {};
            }
            term.write(tr) catch {};

            // Sides
            var row: u16 = 1;
            while (row < height -| 1) : (row += 1) {
                term.moveCursor(x, y + row) catch {};
                term.write(v) catch {};
                if (x + max_width >= 2) {
                    term.moveCursor(x + max_width - 1, y + row) catch {};
                    term.write(v) catch {};
                }
            }

            // Bottom edge
            term.moveCursor(x, y + height - 1) catch {};
            term.write(bl) catch {};
            i = 0;
            while (i < max_width -| 2) : (i += 1) {
                term.write(h) catch {};
            }
            term.write(br) catch {};
        }
    }
}

/// Draw a horizontal line (clips to app pane in dev mode)
pub fn drawHLine(x: u16, y: u16, length: u16, style_type: u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, clip to app pane
            const max_len = if (state.dev_mode and state.app_width > 0 and x < state.app_width)
                @min(length, state.app_width - x)
            else
                length;

            const ch: []const u8 = if (style_type == 1) "\xe2\x95\x90" else "\xe2\x94\x80"; // ═ or ─

            term.moveCursor(x, y) catch {};
            var i: u16 = 0;
            while (i < max_len) : (i += 1) {
                term.write(ch) catch {};
            }
        }
    }
}

/// Draw a vertical line
pub fn drawVLine(x: u16, y: u16, length: u16, style_type: u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            const ch: []const u8 = if (style_type == 1) "\xe2\x95\x91" else "\xe2\x94\x82"; // ║ or │

            var i: u16 = 0;
            while (i < length) : (i += 1) {
                term.moveCursor(x, y + i) catch {};
                term.write(ch) catch {};
            }
        }
    }
}

/// Fill an area with a character (clips to app pane in dev mode)
pub fn fillArea(x: u16, y: u16, width: u16, height: u16, char: u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, clip to app pane
            const max_width = if (state.dev_mode and state.app_width > 0 and x < state.app_width)
                @min(width, state.app_width - x)
            else
                width;

            const ch_buf = [_]u8{char};

            var row: u16 = 0;
            while (row < height) : (row += 1) {
                term.moveCursor(x, y + row) catch {};
                var col: u16 = 0;
                while (col < max_width) : (col += 1) {
                    term.write(&ch_buf) catch {};
                }
            }
        }
    }
}

/// Poll for keyboard input (non-blocking)
pub fn pollKey() ?Key {
    if (getState()) |state| {
        if (state.input_reader) |*reader| {
            // Use poll to check if data is available (non-blocking)
            var fds = [_]std.posix.pollfd{
                .{
                    .fd = std.posix.STDIN_FILENO,
                    .events = std.posix.POLL.IN,
                    .revents = 0,
                },
            };

            // Poll with 0 timeout for non-blocking check
            const poll_result = std.posix.poll(&fds, 0) catch return null;
            if (poll_result == 0) return null; // No data available

            // Data is available, now read it
            const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };
            var buf: [32]u8 = undefined;

            const bytes_read = stdin.read(&buf) catch return null;
            if (bytes_read == 0) return null;

            // Handle Ctrl+C (byte 3) directly before TUI parser mangles it
            if (bytes_read == 1 and buf[0] == 3) {
                deinit();
                std.process.exit(0);
            }

            // Handle Ctrl+R (byte 18) for hot-reload in dev mode
            if (bytes_read == 1 and buf[0] == 18) {
                if (getState()) |s| {
                    if (s.dev_mode) {
                        global_force_reload = true;
                        devLog(">>> Ctrl+R: Reloading...");
                        // Return special key that signals reload request
                        // The xcall handler will check force_reload and return an error
                        return .escape;
                    }
                }
            }

            if (reader.parse(buf[0..bytes_read]) catch null) |event| {
                switch (event) {
                    .key => |k| return k.key,
                    else => {},
                }
            }
        }
    }
    return null;
}

/// Format a key for dev console logging
fn formatKey(buf: *[64]u8, key: Key) []const u8 {
    return switch (key) {
        .char => |c| std.fmt.bufPrint(buf, "Key: '{u}' (0x{x})", .{ c, c }) catch "Key: ?",
        .enter => "Key: ENTER",
        .escape => "Key: ESC",
        .tab => "Key: TAB",
        .backspace => "Key: BACKSPACE",
        .delete => "Key: DELETE",
        .insert => "Key: INSERT",
        .home => "Key: HOME",
        .end => "Key: END",
        .page_up => "Key: PAGE_UP",
        .page_down => "Key: PAGE_DOWN",
        .up => "Key: UP",
        .down => "Key: DOWN",
        .left => "Key: LEFT",
        .right => "Key: RIGHT",
        .f => |n| std.fmt.bufPrint(buf, "Key: F{}", .{n}) catch "Key: F?",
        else => "Key: (other)",
    };
}

/// Wait for keyboard input (blocking)
/// In dev mode with hot-reload, also checks for file changes periodically
/// Note: Ctrl+C is handled in pollKey before the TUI parser
/// When TUI is disabled (headless mode), blocks indefinitely
// Pool of static buffers for key logging (devLog stores slices, so each needs separate storage)
var key_log_buffers: [16][64]u8 = undefined;
var key_log_index: u8 = 0;

pub fn waitKey() Key {
    // Check if we have a TTY with input reader
    if (getState()) |state| {
        if (state.input_reader != null) {
            // Normal TTY mode - use pollKey
            while (true) {
                if (pollKey()) |key| {
                    // Log keystroke in dev mode
                    if (state.dev_mode) {
                        // Use rotating buffer pool so each log entry has its own storage
                        const buf = &key_log_buffers[key_log_index];
                        key_log_index = (key_log_index + 1) % 16;
                        const msg = formatKey(buf, key);
                        devLog(msg);
                    }
                    return key;
                }

                // In dev mode, check for file changes
                if (state.dev_mode) {
                    checkWatchedFile();
                }

                std.Thread.sleep(10 * std.time.ns_per_ms);
            }
        }
    }

    // No-op mode (no TTY) - read directly from stdin with blocking loop
    const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };
    while (true) {
        var buf: [1]u8 = undefined;
        const bytes_read = stdin.read(&buf) catch {
            std.Thread.sleep(100 * std.time.ns_per_ms);
            continue;
        };
        if (bytes_read == 0) {
            // EOF - sleep and retry to avoid spinning
            std.Thread.sleep(100 * std.time.ns_per_ms);
            continue;
        }
        // Handle Ctrl+C (key code 3) - clean exit
        if (buf[0] == 3) {
            deinit();
            std.process.exit(0);
        }
        return .{ .char = buf[0] };
    }
}

/// Get screen dimensions
pub fn getScreenSize() struct { width: u16, height: u16 } {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            const size = term.getSize() catch return .{ .width = 80, .height = 24 };
            return .{ .width = size.cols, .height = size.rows };
        }
    }
    return .{ .width = 80, .height = 24 };
}

/// Flush/refresh the screen (no-op for now, terminal writes are immediate)
pub fn refresh() void {
    // Terminal writes are synchronous, nothing to flush
}

/// Draw a button widget (clips to app pane in dev mode)
pub fn drawButton(x: u16, y: u16, label: []const u8, focused: bool) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, skip if off screen
            if (state.dev_mode and state.app_width > 0 and x >= state.app_width) return;

            // Set button style
            if (focused) {
                applyStyle(term, 15, 4, true, false) catch {}; // White on blue, bold
            } else {
                applyStyle(term, 7, 0, false, false) catch {}; // Default
            }

            term.moveCursor(x, y) catch {};

            // In dev mode, clip the button text
            if (state.dev_mode and state.app_width > 0) {
                const avail = state.app_width - x;
                if (avail >= 4) {
                    term.write("[ ") catch {};
                    const label_avail = avail - 4; // "[ " and " ]"
                    const label_len = @min(label.len, label_avail);
                    term.write(label[0..label_len]) catch {};
                    if (avail >= label_len + 4) {
                        term.write(" ]") catch {};
                    }
                }
            } else {
                term.write("[ ") catch {};
                term.write(label) catch {};
                term.write(" ]") catch {};
            }

            // Reset style
            term.reset() catch {};
        }
    }
}

/// Draw a table header (clips to app pane in dev mode)
pub fn drawTableHeader(x: u16, y: u16, columns: []const []const u8, widths: []const u16) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Get max x for clipping in dev mode
            const max_x = if (state.dev_mode and state.app_width > 0) state.app_width else 9999;

            // Blue background, white bold text, underlined
            applyStyle(term, 15, 4, true, true) catch {};

            var current_x = x;
            for (columns, 0..) |col, i| {
                if (current_x >= max_x) break; // Stop if we've reached the boundary

                term.moveCursor(current_x, y) catch {};
                const width = if (i < widths.len) widths[i] else 10;
                const avail_width = if (current_x + width > max_x) max_x - current_x else width;

                // Write column header, truncated/padded to width
                const display_len = @min(col.len, avail_width);
                term.write(col[0..display_len]) catch {};

                // Pad with spaces if needed
                const padding = avail_width -| @as(u16, @intCast(display_len));
                var j: u16 = 0;
                while (j < padding) : (j += 1) {
                    term.write(" ") catch {};
                }

                current_x += width + 1; // +1 for column spacing
            }

            term.reset() catch {};
        }
    }
}

/// Draw a table row with proper error handling (clips to app pane in dev mode)
/// Returns TuiError on failure for proper error propagation to CLI
pub fn drawTableRowChecked(x: u16, y: u16, cells: []const []const u8, widths: []const u16, selected: bool) TuiError!void {
    const state = getState() orelse {
        setError("drawTableRow", @src(), "TUI not initialized");
        return TuiError.NotInitialized;
    };
    const term = &(state.terminal orelse {
        setError("drawTableRow", @src(), "No terminal available");
        return TuiError.NoTerminal;
    });

    // Bounds validation
    const size = getScreenSize();
    if (y >= size.height) {
        // Off screen - silently skip (not an error, just clipped)
        return;
    }

    // Get max x for clipping in dev mode
    const max_x = if (state.dev_mode and state.app_width > 0) state.app_width else size.width;

    if (selected) {
        applyStyle(term, 0, 6, false, false) catch {
            setError("drawTableRow", @src(), "Failed to apply selection style");
            return TuiError.TerminalError;
        };
    }

    var current_x = x;
    for (cells, 0..) |cell, i| {
        if (current_x >= max_x) break; // Stop if we've reached the boundary

        term.moveCursor(current_x, y) catch {
            setError("drawTableRow", @src(), "Failed to move cursor");
            return TuiError.TerminalError;
        };

        const width = if (i < widths.len) widths[i] else 10;
        const avail_width = if (current_x + width > max_x) max_x - current_x else width;

        // Write cell content, truncated/padded to width
        if (cell.len > 0) {
            // Trim trailing spaces first
            const trimmed = std.mem.trimRight(u8, cell, " ");
            const display_len = @min(trimmed.len, avail_width);
            if (display_len > 0) {
                term.write(trimmed[0..display_len]) catch {
                    setError("drawTableRow", @src(), "Failed to write cell content");
                    return TuiError.TerminalError;
                };
            }

            // Pad with spaces if needed (important for selection highlight)
            const padding = avail_width -| @as(u16, @intCast(display_len));
            var j: u16 = 0;
            while (j < padding) : (j += 1) {
                term.write(" ") catch {
                    setError("drawTableRow", @src(), "Failed to write padding");
                    return TuiError.TerminalError;
                };
            }
        } else {
            // Empty cell - just write spaces
            var j: u16 = 0;
            while (j < avail_width) : (j += 1) {
                term.write(" ") catch {
                    setError("drawTableRow", @src(), "Failed to write empty cell");
                    return TuiError.TerminalError;
                };
            }
        }

        current_x += width + 1; // +1 for column spacing
    }

    if (selected) {
        term.reset() catch {
            setError("drawTableRow", @src(), "Failed to reset style");
            return TuiError.TerminalError;
        };
    }
}

/// Draw a table row (clips to app pane in dev mode)
/// Legacy version - logs errors to dev console instead of returning them
pub fn drawTableRow(x: u16, y: u16, cells: []const []const u8, widths: []const u16, selected: bool) void {
    drawTableRowChecked(x, y, cells, widths, selected) catch |err| {
        // Log error to dev console if available
        if (isDevMode()) {
            var buf: [128]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "drawTableRow error: {s}", .{@errorName(err)}) catch "drawTableRow error";
            devLog(msg);
            if (getLastError()) |ctx| {
                const detail = std.fmt.bufPrint(&buf, "  at {s}:{d}: {s}", .{ ctx.file, ctx.line, ctx.detail }) catch "";
                devLog(detail);
            }
        }
    };
}

/// Draw an input field (clips to app pane in dev mode)
pub fn drawInput(x: u16, y: u16, width: u16, value: []const u8, cursor_pos: u16, focused: bool) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // In dev mode, clip to app pane
            if (state.dev_mode and state.app_width > 0 and x >= state.app_width) return;
            const max_width = if (state.dev_mode and state.app_width > 0)
                @min(width, state.app_width - x)
            else
                width;

            // Input style - unfocused is darker
            const bg_color: u8 = if (focused) 15 else 235; // White or dark gray
            const fg_color: u8 = if (focused) 0 else 7; // Black or light gray

            applyStyle(term, fg_color, bg_color, false, false) catch {};
            term.moveCursor(x, y) catch {};

            // Draw background spaces
            var i: u16 = 0;
            while (i < max_width) : (i += 1) {
                term.write(" ") catch {};
            }

            // Draw value character by character, highlighting cursor position
            term.moveCursor(x, y) catch {};
            const display_len = @min(value.len, max_width);

            i = 0;
            while (i < max_width) : (i += 1) {
                // At cursor position when focused, use inverted colors
                if (focused and i == cursor_pos) {
                    // Cursor: inverted colors (black on cyan for visibility)
                    applyStyle(term, 0, 14, false, false) catch {};
                } else {
                    // Normal style
                    applyStyle(term, fg_color, bg_color, false, false) catch {};
                }

                // Write character or space
                if (i < display_len) {
                    // Write the actual character
                    var char_buf: [1]u8 = .{value[i]};
                    term.write(&char_buf) catch {};
                } else {
                    term.write(" ") catch {};
                }
            }

            term.reset() catch {};
        }
    }
}

/// Draw a progress bar
pub fn drawProgress(x: u16, y: u16, width: u16, percent: u8) void {

    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};
            term.write("[") catch {};

            const bar_width = width -| 2;
            const filled = (bar_width * @as(u16, percent)) / 100;

            // Filled portion (green)
            applyStyle(term, 0, 2, false, false) catch {}; // Black on green
            var i: u16 = 0;
            while (i < filled) : (i += 1) {
                term.write(" ") catch {};
            }

            // Empty portion (dark gray)
            applyStyle(term, 0, 235, false, false) catch {};
            while (i < bar_width) : (i += 1) {
                term.write(" ") catch {};
            }

            term.reset() catch {};
            term.write("]") catch {};
        }
    }
}

/// Draw a card/panel with title
pub fn drawCard(x: u16, y: u16, width: u16, height: u16, title: []const u8) void {

    // Draw border
    drawBox(x, y, width, height, 0);

    // Draw title
    if (getState()) |state| {
        if (state.terminal) |*term| {
            applyStyle(term, 3, 0, true, false) catch {}; // Yellow, bold
            term.moveCursor(x + 2, y) catch {};
            term.write(" ") catch {};
            term.write(title) catch {};
            term.write(" ") catch {};
            term.reset() catch {};
        }
    }
}

/// Show a modal dialog
pub fn showModal(title: []const u8, message: []const u8) void {

    const size = getScreenSize();
    const modal_width: u16 = 50;
    const modal_height: u16 = 7;
    const modal_x = (size.width -| modal_width) / 2;
    const modal_y = (size.height -| modal_height) / 2;

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Draw modal background
            applyStyle(term, 7, 235, false, false) catch {};
            fillArea(modal_x, modal_y, modal_width, modal_height, ' ');

            // Draw border (double)
            drawBox(modal_x, modal_y, modal_width, modal_height, 1);

            // Draw title
            applyStyle(term, 15, 0, true, false) catch {};
            term.moveCursor(modal_x + 2, modal_y) catch {};
            term.write(" ") catch {};
            term.write(title) catch {};
            term.write(" ") catch {};

            // Draw message
            applyStyle(term, 7, 235, false, false) catch {};
            term.moveCursor(modal_x + 2, modal_y + 2) catch {};
            term.write(message) catch {};

            // Draw OK button
            applyStyle(term, 15, 4, false, false) catch {};
            term.moveCursor(modal_x + (modal_width - 6) / 2, modal_y + 4) catch {};
            term.write("[ OK ]") catch {};

            term.reset() catch {};
        }
    }
}

// ============================================
// Dev Mode Functions
// ============================================

/// Enable dev mode with split-pane layout
pub fn setDevMode(enabled: bool) void {
    if (getState()) |state| {
        state.dev_mode = enabled;
        if (enabled) {
            const size = getScreenSize();
            // Need at least 40 cols for dev mode to make sense
            if (size.width < 40) {
                state.dev_mode = false;
                return;
            }
            // App takes 60% of width, dev console takes 40%
            state.app_width = (size.width * 60) / 100;
            // Initialize log buffer
            state.dev_log_count = 0;
            state.dev_log_start = 0;
            // Initialize log lines array to empty slices
            for (&state.dev_log_lines) |*line| {
                line.* = "";
            }
            // Draw initial dev pane
            drawDevPane();
        } else {
            state.app_width = 0;
        }
    }
}

/// Check if dev mode is enabled
pub fn isDevMode() bool {
    if (getState()) |state| {
        return state.dev_mode;
    }
    return false;
}

/// Get app pane width (for TUI coordinate translation)
pub fn getAppWidth() u16 {
    if (getState()) |state| {
        if (state.dev_mode and state.app_width > 0) {
            return state.app_width;
        }
    }
    return getScreenSize().width;
}

/// Static buffer pool for dev log messages - ensures messages remain valid
/// This prevents crashes when callers use stack-allocated buffers that
/// become invalid after the function returns
var dev_log_buffers: [64][128]u8 = undefined;

/// Add a log message to the dev console
pub fn devLog(message: []const u8) void {
    if (getState()) |state| {
        if (!state.dev_mode) return;

        // Copy message to static buffer to ensure it remains valid
        // This is critical - callers may use stack-allocated buffers
        const idx = (state.dev_log_start + state.dev_log_count) % 64;
        const copy_len = @min(message.len, 127);
        @memcpy(dev_log_buffers[idx][0..copy_len], message[0..copy_len]);

        // Store slice to our static buffer (not the caller's memory)
        if (state.dev_log_count < 64) {
            state.dev_log_lines[idx] = dev_log_buffers[idx][0..copy_len];
            state.dev_log_count += 1;
        } else {
            // Buffer full, overwrite oldest
            state.dev_log_lines[idx] = dev_log_buffers[idx][0..copy_len];
            state.dev_log_start = (state.dev_log_start + 1) % 64;
        }

        // Redraw dev pane
        drawDevPane();
    }
}

/// Draw the dev console pane
pub fn drawDevPane() void {

    if (getState()) |state| {
        if (!state.dev_mode) return;
        if (state.terminal) |*term| {
            const size = getScreenSize();
            // Safety checks for small terminals
            if (size.width <= state.app_width + 2 or size.height < 3) return;

            const dev_x = state.app_width + 1;
            const dev_width = size.width - state.app_width - 1;

            // Draw vertical separator
            applyStyle(term, 8, 0, false, false) catch {}; // Gray
            var y: u16 = 0;
            while (y < size.height) : (y += 1) {
                term.moveCursor(state.app_width, y) catch {};
                term.write("\xe2\x94\x82") catch {}; // │
            }

            // Draw dev pane header
            applyStyle(term, 14, 236, true, false) catch {}; // Cyan on dark gray, bold
            term.moveCursor(dev_x, 0) catch {};
            var i: u16 = 0;
            while (i < dev_width) : (i += 1) {
                term.write(" ") catch {};
            }
            term.moveCursor(dev_x + 1, 0) catch {};
            term.write("Dev Console") catch {};
            term.reset() catch {};

            // Draw log entries
            const max_lines: usize = if (size.height > 2) size.height - 2 else 1; // Leave room for header
            const start_line: usize = if (state.dev_log_count > max_lines)
                state.dev_log_count - max_lines
            else
                0;

            y = 1;
            var line_idx: usize = start_line;
            while (line_idx < state.dev_log_count and y < size.height - 1) : (line_idx += 1) {
                const actual_idx = (state.dev_log_start + line_idx) % 64;
                const msg = state.dev_log_lines[actual_idx];

                // Clear the line first
                applyStyle(term, 7, 0, false, false) catch {};
                term.moveCursor(dev_x, y) catch {};
                i = 0;
                while (i < dev_width) : (i += 1) {
                    term.write(" ") catch {};
                }

                // Write the message (truncated to fit)
                term.moveCursor(dev_x + 1, y) catch {};
                const display_len = @min(msg.len, dev_width - 2);
                term.write(msg[0..display_len]) catch {};

                y += 1;
            }

            term.reset() catch {};
        }
    }
}

// ============================================================================
// Hot-Reload Support
// ============================================================================

/// Set the file to watch for hot-reload (call with bytecode path in dev mode)
pub fn setWatchFile(path: []const u8) void {
    if (getState()) |state| {
        // Store the path (we don't own it, caller must ensure it stays valid)
        state.watch_file_path = path;
        state.reload_pending = false;
        state.poll_counter = 0;

        // Get initial modification time
        state.watch_file_mtime = getFileMtime(path) orelse 0;

        devLog("Watching for changes...");
    }
}

/// Clear the watch file (call when exiting dev mode or before reload)
pub fn clearWatchFile() void {
    if (getState()) |state| {
        state.watch_file_path = null;
        state.watch_file_mtime = 0;
        state.reload_pending = false;
    }
}

/// Check if a reload is pending (uses global flag that survives TUI deinit)
pub fn isReloadPending() bool {
    return global_reload_pending or global_force_reload;
}

/// Check if a force reload was requested (Ctrl+R pressed)
pub fn isForceReload() bool {
    return global_force_reload;
}

/// Clear the reload pending flag (call after handling reload)
pub fn clearReloadPending() void {
    global_reload_pending = false;
    global_force_reload = false;
    if (getState()) |state| {
        state.reload_pending = false;
    }
}

/// Check if the watched file has changed (called periodically from waitKey)
fn checkWatchedFile() void {
    checkWatchedFileImpl(true);
}

/// Check if the watched file has changed (public version for polling during error wait)
pub fn pollWatchedFile() void {
    checkWatchedFileImpl(false);
}

/// Implementation of file change checking
fn checkWatchedFileImpl(use_counter: bool) void {
    if (getState()) |state| {
        if (state.watch_file_path) |path| {
            // Only check every 50 polls (~500ms at 10ms sleep) when using counter
            if (use_counter) {
                state.poll_counter += 1;
                if (state.poll_counter < 50) return;
                state.poll_counter = 0;
            }

            // Get current modification time
            const current_mtime = getFileMtime(path) orelse return;

            // Compare with stored mtime
            if (current_mtime != state.watch_file_mtime and state.watch_file_mtime != 0) {
                state.reload_pending = true;
                global_reload_pending = true; // Also set global flag (survives TUI deinit)
                state.watch_file_mtime = current_mtime;
                devLog(">>> File changed! Ctrl+R to reload");
            }
        }
    }
}

/// Get file modification time (public for hot-reload checking from runner)
pub fn getFileMtime(path: []const u8) ?i128 {
    const file = std.fs.cwd().openFile(path, .{}) catch return null;
    defer file.close();

    const stat = file.stat() catch return null;
    return stat.mtime;
}

// ============================================================================
// Cursor Control
// ============================================================================

/// Show the cursor
pub fn showCursor() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x1b[?25h") catch {};
        }
    }
}

/// Hide the cursor
pub fn hideCursor() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x1b[?25l") catch {};
        }
    }
}

/// Save cursor position
pub fn saveCursor() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x1b7") catch {};
        }
    }
}

/// Restore cursor position
pub fn restoreCursor() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x1b8") catch {};
        }
    }
}

/// Move cursor to position
pub fn moveCursor(x: u16, y: u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};
        }
    }
}

/// Put a single character at position
pub fn putChar(x: u16, y: u16, ch: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};
            const buf = [_]u8{ch};
            term.write(&buf) catch {};
        }
    }
}

// ============================================================================
// Color System
// ============================================================================

/// Set foreground and background colors (256-color palette)
pub fn setColor(fg: u8, bg: u8) void {
    if (getState()) |state| {
        state.current_fg = fg;
        state.current_bg = bg;
        if (state.terminal) |*term| {
            var buf: [32]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[38;5;{d};48;5;{d}m", .{ fg, bg }) catch return;
            term.write(output) catch {};
        }
    }
}

/// Set foreground color only (256-color palette)
pub fn setFg(fg: u8) void {
    if (getState()) |state| {
        state.current_fg = fg;
        if (state.terminal) |*term| {
            var buf: [16]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[38;5;{d}m", .{fg}) catch return;
            term.write(output) catch {};
        }
    }
}

/// Set background color only (256-color palette)
pub fn setBg(bg: u8) void {
    if (getState()) |state| {
        state.current_bg = bg;
        if (state.terminal) |*term| {
            var buf: [16]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[48;5;{d}m", .{bg}) catch return;
            term.write(output) catch {};
        }
    }
}

/// Set foreground and background colors (24-bit RGB)
pub fn setColorRGB(fg_r: u8, fg_g: u8, fg_b: u8, bg_r: u8, bg_g: u8, bg_b: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var buf: [64]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[38;2;{d};{d};{d};48;2;{d};{d};{d}m", .{ fg_r, fg_g, fg_b, bg_r, bg_g, bg_b }) catch return;
            term.write(output) catch {};
        }
    }
}

/// Set foreground color (24-bit RGB)
pub fn setFgRGB(r: u8, g: u8, b: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var buf: [32]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[38;2;{d};{d};{d}m", .{ r, g, b }) catch return;
            term.write(output) catch {};
        }
    }
}

/// Set background color (24-bit RGB)
pub fn setBgRGB(r: u8, g: u8, b: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var buf: [32]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b[48;2;{d};{d};{d}m", .{ r, g, b }) catch return;
            term.write(output) catch {};
        }
    }
}

// ============================================================================
// Text Styles
// ============================================================================

/// Set bold text attribute
pub fn setBold(enabled: bool) void {
    if (getState()) |state| {
        state.is_bold = enabled;
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[1m") catch {};
            } else {
                term.write("\x1b[22m") catch {};
            }
        }
    }
}

/// Set dim text attribute
pub fn setDim(enabled: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[2m") catch {};
            } else {
                term.write("\x1b[22m") catch {};
            }
        }
    }
}

/// Set italic text attribute
pub fn setItalic(enabled: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[3m") catch {};
            } else {
                term.write("\x1b[23m") catch {};
            }
        }
    }
}

/// Set underline text attribute
pub fn setUnderline(enabled: bool) void {
    if (getState()) |state| {
        state.is_underline = enabled;
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[4m") catch {};
            } else {
                term.write("\x1b[24m") catch {};
            }
        }
    }
}

/// Set blink text attribute
pub fn setBlink(enabled: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[5m") catch {};
            } else {
                term.write("\x1b[25m") catch {};
            }
        }
    }
}

/// Set reverse video attribute
pub fn setReverse(enabled: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[7m") catch {};
            } else {
                term.write("\x1b[27m") catch {};
            }
        }
    }
}

/// Set strikethrough text attribute
pub fn setStrikethrough(enabled: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (enabled) {
                term.write("\x1b[9m") catch {};
            } else {
                term.write("\x1b[29m") catch {};
            }
        }
    }
}

// ============================================================================
// Input Handling
// ============================================================================

/// Check if a key is available (non-blocking)
pub fn hasKey() bool {
    var fds = [_]std.posix.pollfd{
        .{
            .fd = std.posix.STDIN_FILENO,
            .events = std.posix.POLL.IN,
            .revents = 0,
        },
    };

    const poll_result = std.posix.poll(&fds, 0) catch return false;
    return poll_result > 0;
}

/// Enable mouse tracking
pub fn enableMouse() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Enable mouse button tracking + SGR extended mode
            term.write("\x1b[?1000h\x1b[?1006h") catch {};
        }
    }
}

/// Disable mouse tracking
pub fn disableMouse() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x1b[?1000l\x1b[?1006l") catch {};
        }
    }
}

/// Mouse state
pub const MouseState = struct {
    button: u8, // 0=none, 1=left, 2=middle, 3=right, 4=release
    x: u16,
    y: u16,
};

/// Last mouse event (stored when parsed)
var last_mouse: MouseState = .{ .button = 0, .x = 0, .y = 0 };

/// Get last mouse event
pub fn getMouse() MouseState {
    return last_mouse;
}

// ============================================================================
// Widget Drawing Functions
// ============================================================================

/// Draw a label (styled text)
pub fn drawLabel(x: u16, y: u16, text: []const u8, style: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Style: 0=normal, 1=bold, 2=dim, 3=accent (cyan)
            switch (style) {
                1 => term.write("\x1b[1m") catch {},
                2 => term.write("\x1b[2m") catch {},
                3 => applyStyle(term, 14, state.current_bg, false, false) catch {},
                else => {},
            }
            term.moveCursor(x, y) catch {};
            term.write(text) catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a checkbox widget
pub fn drawCheckbox(x: u16, y: u16, label: []const u8, checked: bool, focused: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (focused) {
                applyStyle(term, 14, 0, true, false) catch {}; // Cyan, bold
            }
            term.moveCursor(x, y) catch {};
            if (checked) {
                term.write("[x] ") catch {};
            } else {
                term.write("[ ] ") catch {};
            }
            term.write(label) catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a radio button widget
pub fn drawRadio(x: u16, y: u16, label: []const u8, selected: bool, group_id: u8, focused: bool) void {
    _ = group_id; // Group ID is for logical grouping, not visual
    if (getState()) |state| {
        if (state.terminal) |*term| {
            if (focused) {
                applyStyle(term, 14, 0, true, false) catch {}; // Cyan, bold
            }
            term.moveCursor(x, y) catch {};
            if (selected) {
                term.write("(\xe2\x97\x89) ") catch {}; // (●)
            } else {
                term.write("( ) ") catch {};
            }
            term.write(label) catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a slider widget
pub fn drawSlider(x: u16, y: u16, width: u16, min_val: i32, max_val: i32, current: i32, focused: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const range = max_val - min_val;
            if (range <= 0) return;

            const bar_width = width -| 2; // Account for [ ]
            const fill_pos: u16 = @intCast(@divTrunc((current - min_val) * @as(i32, @intCast(bar_width)), range));

            if (focused) {
                applyStyle(term, 14, 0, true, false) catch {}; // Cyan, bold
            }

            term.moveCursor(x, y) catch {};
            term.write("[") catch {};

            var i: u16 = 0;
            while (i < bar_width) : (i += 1) {
                if (i == fill_pos) {
                    term.write("\xe2\x97\x8f") catch {}; // ●
                } else if (i < fill_pos) {
                    term.write("=") catch {};
                } else {
                    term.write("-") catch {};
                }
            }
            term.write("]") catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a progress bar with optional percentage display
pub fn drawProgressBar(x: u16, y: u16, width: u16, percent: u8, show_percent: bool) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const bar_width = if (show_percent) width -| 6 else width -| 2; // Space for " 100%"
            const filled: u16 = (bar_width * @as(u16, percent)) / 100;

            term.moveCursor(x, y) catch {};
            term.write("[") catch {};

            // Filled portion (green)
            applyStyle(term, 0, 2, false, false) catch {}; // Black on green
            var i: u16 = 0;
            while (i < filled) : (i += 1) {
                term.write(" ") catch {};
            }

            // Empty portion (dark gray)
            applyStyle(term, 0, 235, false, false) catch {};
            while (i < bar_width) : (i += 1) {
                term.write(" ") catch {};
            }

            term.reset() catch {};
            term.write("]") catch {};

            if (show_percent) {
                var buf: [8]u8 = undefined;
                const pct_str = std.fmt.bufPrint(&buf, " {d:3}%", .{percent}) catch " ???%";
                term.write(pct_str) catch {};
            }
        }
    }
}

/// Draw a spinner widget
pub fn drawSpinner(x: u16, y: u16, frame: u8, style: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const spinner_chars: []const []const u8 = switch (style) {
                1 => &[_][]const u8{ "\xe2\xa0\x8b", "\xe2\xa0\x99", "\xe2\xa0\xb9", "\xe2\xa0\xb8", "\xe2\xa0\xbc", "\xe2\xa0\xb4", "\xe2\xa0\xa6", "\xe2\xa0\xa7", "\xe2\xa0\x87", "\xe2\xa0\x8f" }, // Braille dots
                2 => &[_][]const u8{ "\xe2\x96\x88", "\xe2\x96\x93", "\xe2\x96\x92", "\xe2\x96\x91", " " }, // Block fade
                else => &[_][]const u8{ "|", "/", "-", "\\" }, // Classic
            };

            const idx = frame % @as(u8, @intCast(spinner_chars.len));
            term.moveCursor(x, y) catch {};
            applyStyle(term, 14, 0, false, false) catch {}; // Cyan
            term.write(spinner_chars[idx]) catch {};
            term.reset() catch {};
        }
    }
}

// ============================================================================
// Container Widgets
// ============================================================================

/// Draw a panel (box with optional style)
pub fn drawPanel(x: u16, y: u16, width: u16, height: u16, style: u8) void {
    // Style: 0=single line, 1=double line, 2=rounded, 3=heavy
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Fill interior
            applyStyle(term, 7, 0, false, false) catch {};
            var row: u16 = 1;
            while (row < height -| 1) : (row += 1) {
                term.moveCursor(x + 1, y + row) catch {};
                var col: u16 = 1;
                while (col < width -| 1) : (col += 1) {
                    term.write(" ") catch {};
                }
            }
            term.reset() catch {};
        }
    }
    // Draw border
    drawBox(x, y, width, height, style);
}

/// Show modal dialog with custom size
pub fn showModalEx(title: []const u8, message: []const u8, width: u16, height: u16) void {
    const size = getScreenSize();
    const modal_x = (size.width -| width) / 2;
    const modal_y = (size.height -| height) / 2;

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Draw modal background
            applyStyle(term, 7, 235, false, false) catch {};
            fillArea(modal_x, modal_y, width, height, ' ');

            // Draw border (double)
            drawBox(modal_x, modal_y, width, height, 1);

            // Draw title
            applyStyle(term, 15, 0, true, false) catch {};
            term.moveCursor(modal_x + 2, modal_y) catch {};
            term.write(" ") catch {};
            term.write(title) catch {};
            term.write(" ") catch {};

            // Draw message (word wrap would be nice but keep it simple)
            applyStyle(term, 7, 235, false, false) catch {};
            term.moveCursor(modal_x + 2, modal_y + 2) catch {};
            const max_msg_width = width -| 4;
            const display_len = @min(message.len, max_msg_width);
            term.write(message[0..display_len]) catch {};

            // Draw OK button
            applyStyle(term, 15, 4, false, false) catch {};
            term.moveCursor(modal_x + (width - 6) / 2, modal_y + height - 2) catch {};
            term.write("[ OK ]") catch {};

            term.reset() catch {};
        }
    }
}

/// Show an alert dialog
pub fn showAlert(title: []const u8, message: []const u8, alert_type: u8) void {
    const size = getScreenSize();
    const width: u16 = 50;
    const height: u16 = 7;
    const modal_x = (size.width -| width) / 2;
    const modal_y = (size.height -| height) / 2;

    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Background color based on alert type: 0=info(blue), 1=warning(yellow), 2=error(red), 3=success(green)
            const bg_color: u8 = switch (alert_type) {
                1 => 3, // Yellow
                2 => 1, // Red
                3 => 2, // Green
                else => 4, // Blue (info)
            };

            applyStyle(term, 15, bg_color, false, false) catch {};
            fillArea(modal_x, modal_y, width, height, ' ');

            // Draw title
            term.moveCursor(modal_x + 2, modal_y + 1) catch {};
            applyStyle(term, 15, bg_color, true, false) catch {};
            term.write(title) catch {};

            // Draw message
            applyStyle(term, 15, bg_color, false, false) catch {};
            term.moveCursor(modal_x + 2, modal_y + 3) catch {};
            term.write(message) catch {};

            // Draw OK button
            applyStyle(term, bg_color, 15, false, false) catch {}; // Inverted
            term.moveCursor(modal_x + (width - 6) / 2, modal_y + 5) catch {};
            term.write("[ OK ]") catch {};

            term.reset() catch {};
        }
    }
}

/// Show a confirmation dialog
pub fn showConfirm(title: []const u8, message: []const u8, default_yes: bool) void {
    const size = getScreenSize();
    const width: u16 = 50;
    const height: u16 = 7;
    const modal_x = (size.width -| width) / 2;
    const modal_y = (size.height -| height) / 2;

    if (getState()) |state| {
        if (state.terminal) |*term| {
            applyStyle(term, 7, 235, false, false) catch {};
            fillArea(modal_x, modal_y, width, height, ' ');

            drawBox(modal_x, modal_y, width, height, 1);

            // Draw title
            applyStyle(term, 15, 0, true, false) catch {};
            term.moveCursor(modal_x + 2, modal_y) catch {};
            term.write(" ") catch {};
            term.write(title) catch {};
            term.write(" ") catch {};

            // Draw message
            applyStyle(term, 7, 235, false, false) catch {};
            term.moveCursor(modal_x + 2, modal_y + 2) catch {};
            term.write(message) catch {};

            // Draw Yes/No buttons
            const yes_x = modal_x + (width / 2) - 10;
            const no_x = modal_x + (width / 2) + 2;

            if (default_yes) {
                applyStyle(term, 15, 4, false, false) catch {}; // Highlight Yes
            } else {
                applyStyle(term, 7, 235, false, false) catch {};
            }
            term.moveCursor(yes_x, modal_y + 4) catch {};
            term.write("[ Yes ]") catch {};

            if (!default_yes) {
                applyStyle(term, 15, 4, false, false) catch {}; // Highlight No
            } else {
                applyStyle(term, 7, 235, false, false) catch {};
            }
            term.moveCursor(no_x, modal_y + 4) catch {};
            term.write("[ No ]") catch {};

            term.reset() catch {};
        }
    }
}

// ============================================================================
// Lists and Tables
// ============================================================================

/// Draw a list widget
pub fn drawList(x: u16, y: u16, width: u16, height: u16, items: []const []const u8, selected: u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Draw border
            drawBox(x, y, width, height, 0);

            // Draw items
            const visible_items = height -| 2;
            const start_idx: usize = if (selected >= visible_items)
                selected - visible_items + 1
            else
                0;

            var row: u16 = 0;
            while (row < visible_items and start_idx + row < items.len) : (row += 1) {
                const idx = start_idx + row;
                term.moveCursor(x + 1, y + 1 + row) catch {};

                if (idx == selected) {
                    applyStyle(term, 0, 6, false, false) catch {}; // Black on cyan
                }

                // Draw item text padded to width
                const item = items[idx];
                const display_len = @min(item.len, width -| 2);
                term.write(item[0..display_len]) catch {};

                // Pad remaining space
                var pad: u16 = @intCast(display_len);
                while (pad < width -| 2) : (pad += 1) {
                    term.write(" ") catch {};
                }

                if (idx == selected) {
                    term.reset() catch {};
                }
            }
        }
    }
}

/// Draw a table row separator
pub fn drawTableSeparator(x: u16, y: u16, widths: []const u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};

            var current_x = x;
            for (widths, 0..) |width, i| {
                // Draw separator for column
                var j: u16 = 0;
                while (j < width) : (j += 1) {
                    term.write("\xe2\x94\x80") catch {}; // ─
                }

                // Draw intersection or corner
                if (i < widths.len - 1) {
                    term.write("\xe2\x94\xbc") catch {}; // ┼
                }
                current_x += width + 1;
            }
        }
    }
}

// ============================================================================
// Visual Elements
// ============================================================================

/// Draw a separator line with optional style
pub fn drawSeparator(x: u16, y: u16, width: u16, style: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const ch: []const u8 = switch (style) {
                1 => "\xe2\x95\x90", // ═ double
                2 => "\xe2\x94\x81", // ━ heavy
                3 => "\xe2\x94\x84", // ┄ dashed
                else => "\xe2\x94\x80", // ─ single
            };

            term.moveCursor(x, y) catch {};
            var i: u16 = 0;
            while (i < width) : (i += 1) {
                term.write(ch) catch {};
            }
        }
    }
}

/// Draw a badge (colored label)
pub fn drawBadge(x: u16, y: u16, text: []const u8, style: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Style determines color: 0=gray, 1=blue, 2=green, 3=yellow, 4=red, 5=magenta
            const bg_color: u8 = switch (style) {
                1 => 4, // Blue
                2 => 2, // Green
                3 => 3, // Yellow
                4 => 1, // Red
                5 => 5, // Magenta
                else => 240, // Gray
            };
            const fg_color: u8 = if (style == 3) 0 else 15; // Dark text on yellow

            applyStyle(term, fg_color, bg_color, false, false) catch {};
            term.moveCursor(x, y) catch {};
            term.write(" ") catch {};
            term.write(text) catch {};
            term.write(" ") catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a tag (outlined label)
pub fn drawTag(x: u16, y: u16, text: []const u8, color: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            applyStyle(term, color, 0, false, false) catch {};
            term.moveCursor(x, y) catch {};
            term.write("[") catch {};
            term.write(text) catch {};
            term.write("]") catch {};
            term.reset() catch {};
        }
    }
}

/// Draw a divider with centered label
pub fn drawDivider(x: u16, y: u16, width: u16, label: []const u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};

            if (label.len == 0) {
                // Just draw a line
                var i: u16 = 0;
                while (i < width) : (i += 1) {
                    term.write("\xe2\x94\x80") catch {}; // ─
                }
            } else {
                // Draw line with centered label
                const label_space = label.len + 2; // " label "
                const left_width = (width -| @as(u16, @intCast(label_space))) / 2;
                const right_width = width -| left_width -| @as(u16, @intCast(label_space));

                var i: u16 = 0;
                while (i < left_width) : (i += 1) {
                    term.write("\xe2\x94\x80") catch {}; // ─
                }
                term.write(" ") catch {};
                applyStyle(term, 8, 0, false, false) catch {}; // Gray
                term.write(label) catch {};
                term.reset() catch {};
                term.write(" ") catch {};
                i = 0;
                while (i < right_width) : (i += 1) {
                    term.write("\xe2\x94\x80") catch {}; // ─
                }
            }
        }
    }
}

// ============================================================================
// Menus and Navigation
// ============================================================================

/// Draw a vertical menu
pub fn drawMenu(x: u16, y: u16, width: u16, items: []const []const u8, selected: u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            // Draw border
            drawBox(x, y, width, @intCast(items.len + 2), 0);

            // Draw items
            for (items, 0..) |item, i| {
                term.moveCursor(x + 1, y + 1 + @as(u16, @intCast(i))) catch {};

                if (i == selected) {
                    applyStyle(term, 0, 6, false, false) catch {}; // Black on cyan
                    term.write("> ") catch {};
                } else {
                    term.write("  ") catch {};
                }

                const display_len = @min(item.len, width -| 4);
                term.write(item[0..display_len]) catch {};

                // Pad remaining
                var pad: u16 = @intCast(display_len + 2);
                while (pad < width -| 2) : (pad += 1) {
                    term.write(" ") catch {};
                }

                if (i == selected) {
                    term.reset() catch {};
                }
            }
        }
    }
}

/// Draw a horizontal menu bar
pub fn drawMenuBar(row: u16, items: []const []const u8, selected: i32) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const size = getScreenSize();

            // Draw background
            applyStyle(term, 0, 7, false, false) catch {}; // Black on white
            term.moveCursor(0, row) catch {};
            var i: u16 = 0;
            while (i < size.width) : (i += 1) {
                term.write(" ") catch {};
            }

            // Draw items
            var x: u16 = 1;
            for (items, 0..) |item, idx| {
                term.moveCursor(x, row) catch {};

                if (@as(i32, @intCast(idx)) == selected) {
                    applyStyle(term, 15, 4, false, false) catch {}; // White on blue
                } else {
                    applyStyle(term, 0, 7, false, false) catch {}; // Black on white
                }

                term.write(" ") catch {};
                term.write(item) catch {};
                term.write(" ") catch {};

                x += @intCast(item.len + 3);
            }

            term.reset() catch {};
        }
    }
}

/// Draw a status bar
pub fn drawStatusBar(row: u16, text: []const u8, style: u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            const size = getScreenSize();

            // Style: 0=default(black on white), 1=info(white on blue), 2=success(white on green), 3=warning(black on yellow)
            const colors: struct { fg: u8, bg: u8 } = switch (style) {
                1 => .{ .fg = 15, .bg = 4 },
                2 => .{ .fg = 15, .bg = 2 },
                3 => .{ .fg = 0, .bg = 3 },
                else => .{ .fg = 0, .bg = 7 },
            };

            applyStyle(term, colors.fg, colors.bg, false, false) catch {};
            term.moveCursor(0, row) catch {};

            // Write text
            const display_len = @min(text.len, size.width);
            term.write(text[0..display_len]) catch {};

            // Fill remaining
            var i: u16 = @intCast(display_len);
            while (i < size.width) : (i += 1) {
                term.write(" ") catch {};
            }

            term.reset() catch {};
        }
    }
}

/// Draw breadcrumb navigation
pub fn drawBreadcrumb(x: u16, y: u16, items: []const []const u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};

            for (items, 0..) |item, i| {
                if (i == items.len - 1) {
                    // Last item is current (bold)
                    applyStyle(term, 15, 0, true, false) catch {};
                } else {
                    applyStyle(term, 8, 0, false, false) catch {}; // Gray
                }

                term.write(item) catch {};
                term.reset() catch {};

                if (i < items.len - 1) {
                    applyStyle(term, 8, 0, false, false) catch {};
                    term.write(" > ") catch {};
                    term.reset() catch {};
                }
            }
        }
    }
}

/// Draw tab bar
pub fn drawTabs(x: u16, y: u16, width: u16, tabs: []const []const u8, active: u16) void {
    _ = width; // We'll use as much space as needed
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.moveCursor(x, y) catch {};

            for (tabs, 0..) |tab, i| {
                if (i == active) {
                    // Active tab: bold, underlined
                    applyStyle(term, 15, 0, true, true) catch {};
                    term.write(" ") catch {};
                    term.write(tab) catch {};
                    term.write(" ") catch {};
                    term.reset() catch {};
                } else {
                    // Inactive tab
                    applyStyle(term, 8, 0, false, false) catch {}; // Gray
                    term.write(" ") catch {};
                    term.write(tab) catch {};
                    term.write(" ") catch {};
                    term.reset() catch {};
                }

                if (i < tabs.len - 1) {
                    term.write("|") catch {};
                }
            }
        }
    }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Sound the terminal bell
pub fn beep() void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            term.write("\x07") catch {}; // BEL character
        }
    }
}

/// Set terminal window title
pub fn setTitle(title: []const u8) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var buf: [256]u8 = undefined;
            const output = std.fmt.bufPrint(&buf, "\x1b]0;{s}\x07", .{title}) catch return;
            term.write(output) catch {};
        }
    }
}

/// Scroll screen up by n lines
pub fn scrollUp(lines: u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var i: u16 = 0;
            while (i < lines) : (i += 1) {
                term.write("\x1b[S") catch {}; // Scroll up one line
            }
        }
    }
}

/// Scroll screen down by n lines
pub fn scrollDown(lines: u16) void {
    if (getState()) |state| {
        if (state.terminal) |*term| {
            var i: u16 = 0;
            while (i < lines) : (i += 1) {
                term.write("\x1b[T") catch {}; // Scroll down one line
            }
        }
    }
}
