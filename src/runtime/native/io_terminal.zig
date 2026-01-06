//! Terminal Native Functions
//!
//! Provides low-level terminal control for TUI applications.
//! These are the primitives needed to build a rich TUI package in Cot.
//!
//! Functions:
//!   term_raw_enable()     - Enable raw mode, returns state handle
//!   term_raw_disable()    - Disable raw mode
//!   term_size()           - Get terminal dimensions (cols, rows)
//!   term_poll(timeout_ms) - Check if input available
//!   term_read()           - Read next terminal event
//!   term_write(data)      - Write raw bytes to terminal
//!   term_flush()          - Flush terminal output
//!
//! Sequences (written via term_write):
//!   term_seq_alt_enter    - Enter alternate screen
//!   term_seq_alt_exit     - Exit alternate screen
//!   term_seq_mouse_on     - Enable mouse tracking
//!   term_seq_mouse_off    - Disable mouse tracking
//!   term_seq_cursor_hide  - Hide cursor
//!   term_seq_cursor_show  - Show cursor
//!   term_seq_clear        - Clear screen

const std = @import("std");
const native = @import("native.zig");
const terminal = @import("../terminal/terminal.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Global raw mode state (per-process)
var raw_state: ?terminal.RawState = null;

/// Register terminal native functions
pub fn register(registry: anytype) !void {
    // Core terminal control
    try registry.registerNative("term_raw_enable", term_raw_enable);
    try registry.registerNative("term_raw_disable", term_raw_disable);
    try registry.registerNative("term_size", term_size);

    // Event handling
    try registry.registerNative("term_poll", term_poll);
    try registry.registerNative("term_read", term_read);

    // Output
    try registry.registerNative("term_write", term_write);
    try registry.registerNative("term_flush", term_flush);

    // Cursor control
    try registry.registerNative("term_cursor_to", term_cursor_to);
    try registry.registerNative("term_cursor_up", term_cursor_up);
    try registry.registerNative("term_cursor_down", term_cursor_down);
    try registry.registerNative("term_cursor_left", term_cursor_left);
    try registry.registerNative("term_cursor_right", term_cursor_right);

    // Sequence constants
    try registry.registerNative("term_seq_alt_enter", term_seq_alt_enter);
    try registry.registerNative("term_seq_alt_exit", term_seq_alt_exit);
    try registry.registerNative("term_seq_mouse_on", term_seq_mouse_on);
    try registry.registerNative("term_seq_mouse_off", term_seq_mouse_off);
    try registry.registerNative("term_seq_cursor_hide", term_seq_cursor_hide);
    try registry.registerNative("term_seq_cursor_show", term_seq_cursor_show);
    try registry.registerNative("term_seq_clear", term_seq_clear);
    try registry.registerNative("term_seq_clear_line", term_seq_clear_line);
    try registry.registerNative("term_seq_reset", term_seq_reset);
}

/// term_raw_enable() - Enable raw terminal mode
/// Returns true on success, false on failure
fn term_raw_enable(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;

    if (raw_state != null) {
        // Already in raw mode
        return Value.initBool(true);
    }

    const stdin: std.fs.File = .stdin();
    raw_state = terminal.enableRawMode(stdin.handle) catch {
        return Value.initBool(false);
    };

    return Value.initBool(true);
}

/// term_raw_disable() - Disable raw terminal mode
fn term_raw_disable(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;

    if (raw_state) |*state| {
        state.restore();
        raw_state = null;
    }

    return null;
}

/// term_size() - Get terminal size
/// Returns a record with cols and rows fields
fn term_size(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const stdout: std.fs.File = .stdout();
    const size = terminal.getSize(stdout.handle) catch {
        // Return default size on failure
        return Value.initInt(80 << 16 | 24); // Packed: cols << 16 | rows
    };

    // Pack cols and rows into a single integer (cols << 16 | rows)
    // Cot can unpack: cols = size >> 16, rows = size & 0xFFFF
    const result: i64 = (@as(i64, size.cols) << 16) | @as(i64, size.rows);
    return Value.initInt(result);
}

/// term_poll(timeout_ms) - Poll for input
/// Returns true if input is available
fn term_poll(ctx: *NativeContext) NativeError!?Value {
    const timeout = ctx.getArgInt(0) catch 0;
    const stdin: std.fs.File = .stdin();

    const has_input = terminal.event.poll(stdin.handle, @intCast(@max(0, timeout))) catch false;
    return Value.initBool(has_input);
}

/// term_read() - Read next terminal event
/// Returns an integer encoding the event type and data
/// Event format: type << 56 | modifier << 48 | data
/// Types: 0=none, 1=key_char, 2=key_special, 3=mouse, 4=timeout, 5=invalid
fn term_read(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;

    const stdin: std.fs.File = .stdin();
    const event = terminal.event.read(stdin) catch {
        return Value.initInt(encodeEvent(.invalid, 0, 0));
    };

    return Value.initInt(encodeEventValue(event));
}

fn encodeEvent(event_type: EventType, mods: u8, data: u32) i64 {
    return (@as(i64, @intFromEnum(event_type)) << 56) |
        (@as(i64, mods) << 48) |
        @as(i64, data);
}

const EventType = enum(u8) {
    none = 0,
    key_char = 1,
    key_special = 2,
    mouse = 3,
    timeout = 4,
    invalid = 5,
    resize = 6,
};

fn encodeEventValue(event: terminal.Event) i64 {
    switch (event) {
        .none => return encodeEvent(.none, 0, 0),
        .timeout => return encodeEvent(.timeout, 0, 0),
        .invalid => return encodeEvent(.invalid, 0, 0),
        .resize => return encodeEvent(.resize, 0, 0),
        .key => |k| {
            const mods: u8 = (@as(u8, @intFromBool(k.mods.shift)) << 0) |
                (@as(u8, @intFromBool(k.mods.alt)) << 1) |
                (@as(u8, @intFromBool(k.mods.ctrl)) << 2);

            switch (k.code) {
                .char => |c| return encodeEvent(.key_char, mods, c),
                else => return encodeEvent(.key_special, mods, @intFromEnum(k.code)),
            }
        },
        .mouse => |m| {
            const mods: u8 = (@as(u8, @intFromBool(m.mods.shift)) << 0) |
                (@as(u8, @intFromBool(m.mods.alt)) << 1) |
                (@as(u8, @intFromBool(m.mods.ctrl)) << 2);

            // Pack: button << 24 | y << 12 | x
            const data: u32 = (@as(u32, @intFromEnum(m.button)) << 24) |
                (@as(u32, m.y) << 12) |
                @as(u32, m.x);

            return encodeEvent(.mouse, mods, data);
        },
    }
}

/// term_write(data) - Write raw bytes to terminal
fn term_write(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return null;

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(data) catch {};

    return null;
}

/// term_flush() - Flush terminal output
fn term_flush(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // stdout is unbuffered by default in Zig, but we ensure any pending writes complete
    return null;
}

/// term_cursor_to(col, row) - Move cursor to position
fn term_cursor_to(ctx: *NativeContext) NativeError!?Value {
    const col = ctx.getArgInt(0) catch 1;
    const row = ctx.getArgInt(1) catch 1;

    var buf: [32]u8 = undefined;
    const seq = terminal.cursorTo(&buf, @intCast(col), @intCast(row));

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(seq) catch {};

    return null;
}

/// term_cursor_up(n) - Move cursor up n rows
fn term_cursor_up(ctx: *NativeContext) NativeError!?Value {
    const n = ctx.getArgInt(0) catch 1;

    var buf: [16]u8 = undefined;
    const seq = terminal.cursorUp(&buf, @intCast(@max(1, n)));

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(seq) catch {};

    return null;
}

/// term_cursor_down(n) - Move cursor down n rows
fn term_cursor_down(ctx: *NativeContext) NativeError!?Value {
    const n = ctx.getArgInt(0) catch 1;

    var buf: [16]u8 = undefined;
    const seq = terminal.cursorDown(&buf, @intCast(@max(1, n)));

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(seq) catch {};

    return null;
}

/// term_cursor_left(n) - Move cursor left n columns
fn term_cursor_left(ctx: *NativeContext) NativeError!?Value {
    const n = ctx.getArgInt(0) catch 1;

    var buf: [16]u8 = undefined;
    const seq = terminal.cursorLeft(&buf, @intCast(@max(1, n)));

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(seq) catch {};

    return null;
}

/// term_cursor_right(n) - Move cursor right n columns
fn term_cursor_right(ctx: *NativeContext) NativeError!?Value {
    const n = ctx.getArgInt(0) catch 1;

    var buf: [16]u8 = undefined;
    const seq = terminal.cursorRight(&buf, @intCast(@max(1, n)));

    const stdout: std.fs.File = .stdout();
    _ = stdout.write(seq) catch {};

    return null;
}

// Sequence constant functions - return the escape sequence string

fn term_seq_alt_enter(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.alt_screen_enter) catch return null;
}

fn term_seq_alt_exit(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.alt_screen_exit) catch return null;
}

fn term_seq_mouse_on(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.mouse_enable) catch return null;
}

fn term_seq_mouse_off(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.mouse_disable) catch return null;
}

fn term_seq_cursor_hide(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.cursor_hide) catch return null;
}

fn term_seq_cursor_show(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.cursor_show) catch return null;
}

fn term_seq_clear(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.clear_all) catch return null;
}

fn term_seq_clear_line(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.clear_line) catch return null;
}

fn term_seq_reset(ctx: *NativeContext) NativeError!?Value {
    return Value.initString(ctx.allocator, terminal.seq.reset) catch return null;
}
