//! Terminal Event Handling
//!
//! Parses terminal input into structured events:
//! - Key events with modifiers (Ctrl, Alt, Shift)
//! - Mouse events (click, scroll, drag)
//! - Special keys (arrows, function keys, etc.)

const std = @import("std");
const posix = std.posix;
const unicode = std.unicode;

/// Keyboard modifiers
pub const Modifiers = packed struct {
    shift: bool = false,
    alt: bool = false,
    ctrl: bool = false,

    pub fn none() Modifiers {
        return .{};
    }
};

/// Key codes for special keys
pub const KeyCode = union(enum) {
    /// Regular character (Unicode codepoint)
    char: u21,
    /// Special keys
    enter,
    esc,
    backspace,
    tab,
    up,
    down,
    left,
    right,
    home,
    end,
    page_up,
    page_down,
    insert,
    delete,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
};

/// A keyboard event
pub const Key = struct {
    code: KeyCode,
    mods: Modifiers = .{},
};

/// Mouse button
pub const MouseButton = enum {
    left,
    middle,
    right,
    release,
    scroll_up,
    scroll_down,
    move,
};

/// A mouse event
pub const Mouse = struct {
    x: u16,
    y: u16,
    button: MouseButton,
    mods: Modifiers = .{},
};

/// Terminal events
pub const Event = union(enum) {
    key: Key,
    mouse: Mouse,
    resize,
    timeout,
    none,
    invalid,

    /// Check if event matches a character with modifiers
    pub fn matchesChar(self: Event, char: u21, mods: Modifiers) bool {
        switch (self) {
            .key => |k| switch (k.code) {
                .char => |c| return c == char and std.meta.eql(k.mods, mods),
                else => return false,
            },
            else => return false,
        }
    }

    /// Check if event matches a special key with modifiers
    pub fn matchesKey(self: Event, code: KeyCode, mods: Modifiers) bool {
        switch (self) {
            .key => |k| return std.meta.eql(k.code, code) and std.meta.eql(k.mods, mods),
            else => return false,
        }
    }
};

/// Poll for input with timeout (milliseconds)
/// Returns true if input is available
pub fn poll(handle: posix.fd_t, timeout_ms: i32) !bool {
    var polls: [1]posix.pollfd = .{.{
        .fd = handle,
        .events = posix.POLL.IN,
        .revents = 0,
    }};

    return (try posix.poll(&polls, timeout_ms)) > 0;
}

/// Read next event from terminal
/// Blocks until an event is available
pub fn read(file: std.fs.File) !Event {
    var buf: [1]u8 = undefined;
    var reader = file.reader(&buf);
    return readFromReader(&reader.interface, file);
}

/// Read next event with timeout
/// Returns .timeout if no event within timeout_ms milliseconds
pub fn readWithTimeout(file: std.fs.File, timeout_ms: i32) !Event {
    if (!try poll(file.handle, timeout_ms)) {
        return .timeout;
    }
    return read(file);
}

fn readFromReader(reader: *std.Io.Reader, file: std.fs.File) !Event {
    const c0 = reader.takeByte() catch |err| switch (err) {
        error.EndOfStream => return .none,
        else => return err,
    };

    switch (c0) {
        // Escape - could be ESC key or start of escape sequence
        '\x1b' => {
            if (!try hasMoreInput(file)) {
                return Event{ .key = .{ .code = .esc } };
            }
            return parseEscapeSequence(reader);
        },

        // Backspace
        '\x08', '\x7f' => return Event{ .key = .{ .code = .backspace } },

        // Tab
        '\x09' => return Event{ .key = .{ .code = .tab } },

        // Enter (LF / CR)
        '\x0a', '\x0d' => return Event{ .key = .{ .code = .enter } },

        // Ctrl+A..Ctrl+Z (excluding special chars above)
        '\x01'...'\x07', '\x0b'...'\x0c', '\x0e'...'\x1a' => {
            return Event{ .key = .{
                .code = .{ .char = c0 + 'a' - 0x01 },
                .mods = .{ .ctrl = true },
            } };
        },

        // Regular characters (including UTF-8)
        else => {
            const len = unicode.utf8ByteSequenceLength(c0) catch return .invalid;
            var utf8_buf: [4]u8 = undefined;
            utf8_buf[0] = c0;

            for (1..len) |i| {
                utf8_buf[i] = reader.takeByte() catch return .invalid;
            }

            const codepoint = switch (len) {
                1 => utf8_buf[0],
                2 => unicode.utf8Decode2(utf8_buf[0..2].*) catch return .invalid,
                3 => unicode.utf8Decode3(utf8_buf[0..3].*) catch return .invalid,
                4 => unicode.utf8Decode4(utf8_buf[0..4].*) catch return .invalid,
                else => return .invalid,
            };

            // Uppercase letters indicate Shift
            if (codepoint >= 'A' and codepoint <= 'Z') {
                return Event{ .key = .{
                    .code = .{ .char = codepoint + 32 },
                    .mods = .{ .shift = true },
                } };
            }

            return Event{ .key = .{ .code = .{ .char = codepoint } } };
        },
    }
}

fn hasMoreInput(file: std.fs.File) !bool {
    return poll(file.handle, 0);
}

fn readByteOrNull(reader: *std.Io.Reader) ?u8 {
    return reader.takeByte() catch null;
}

fn parseEscapeSequence(reader: *std.Io.Reader) !Event {
    const c1 = readByteOrNull(reader) orelse return .invalid;

    switch (c1) {
        '[' => return parseCsiSequence(reader),
        'O' => return parseSs3Sequence(reader),

        // Alt + Ctrl+letter
        '\x01'...'\x1a' => return Event{ .key = .{
            .code = .{ .char = c1 + 'a' - 0x01 },
            .mods = .{ .ctrl = true, .alt = true },
        } },

        // Alt + regular char
        else => return Event{ .key = .{
            .code = .{ .char = c1 },
            .mods = .{ .alt = true },
        } },
    }
}

fn parseCsiSequence(reader: *std.Io.Reader) !Event {
    const c2 = readByteOrNull(reader) orelse return .invalid;

    switch (c2) {
        // Simple arrow keys
        'A' => return Event{ .key = .{ .code = .up } },
        'B' => return Event{ .key = .{ .code = .down } },
        'C' => return Event{ .key = .{ .code = .right } },
        'D' => return Event{ .key = .{ .code = .left } },
        'H' => return Event{ .key = .{ .code = .home } },
        'F' => return Event{ .key = .{ .code = .end } },

        // Mouse event (legacy format)
        'M' => return parseMouseLegacy(reader),

        // Extended sequences
        '0'...'9' => return parseExtendedCsi(reader, c2),

        // SGR mouse format
        '<' => return parseMouseSgr(reader),

        else => return .invalid,
    }
}

fn parseSs3Sequence(reader: *std.Io.Reader) !Event {
    const c2 = readByteOrNull(reader) orelse return .invalid;

    return switch (c2) {
        'P' => Event{ .key = .{ .code = .f1 } },
        'Q' => Event{ .key = .{ .code = .f2 } },
        'R' => Event{ .key = .{ .code = .f3 } },
        'S' => Event{ .key = .{ .code = .f4 } },
        'H' => Event{ .key = .{ .code = .home } },
        'F' => Event{ .key = .{ .code = .end } },
        else => .invalid,
    };
}

fn parseExtendedCsi(reader: *std.Io.Reader, first_digit: u8) !Event {
    var buffer: [32]u8 = undefined;
    buffer[0] = first_digit;
    var i: usize = 1;

    // Read until terminator
    while (i < buffer.len - 1) {
        const c = readByteOrNull(reader) orelse break;
        buffer[i] = c;
        i += 1;

        // Terminators
        if (c == '~' or c == 'M' or c == 'm' or (c >= 'A' and c <= 'Z')) {
            break;
        }
    }

    const seq = buffer[0..i];

    // Special keys: 1~ = Home, 2~ = Insert, etc.
    if (seq.len >= 2 and seq[seq.len - 1] == '~') {
        return parseSpecialKey(seq[0 .. seq.len - 1]);
    }

    // Modified arrow keys: 1;2A = Shift+Up, 1;3A = Alt+Up, etc.
    if (seq.len >= 4 and seq[0] == '1' and seq[1] == ';') {
        const modifier = seq[2] - '1';
        const key = seq[3];
        return parseModifiedArrow(key, modifier);
    }

    return .invalid;
}

fn parseSpecialKey(num_str: []const u8) Event {
    const num = std.fmt.parseInt(u8, num_str, 10) catch return .invalid;

    const code: KeyCode = switch (num) {
        1 => .home,
        2 => .insert,
        3 => .delete,
        4 => .end,
        5 => .page_up,
        6 => .page_down,
        11 => .f1,
        12 => .f2,
        13 => .f3,
        14 => .f4,
        15 => .f5,
        17 => .f6,
        18 => .f7,
        19 => .f8,
        20 => .f9,
        21 => .f10,
        23 => .f11,
        24 => .f12,
        else => return .invalid,
    };

    return Event{ .key = .{ .code = code } };
}

fn parseModifiedArrow(key: u8, modifier: u8) Event {
    const code: KeyCode = switch (key) {
        'A' => .up,
        'B' => .down,
        'C' => .right,
        'D' => .left,
        'H' => .home,
        'F' => .end,
        else => return .invalid,
    };

    // Modifier encoding: 1=none, 2=Shift, 3=Alt, 4=Shift+Alt,
    //                    5=Ctrl, 6=Ctrl+Shift, 7=Ctrl+Alt, 8=Ctrl+Shift+Alt
    const mods = Modifiers{
        .shift = (modifier & 1) != 0,
        .alt = (modifier & 2) != 0,
        .ctrl = (modifier & 4) != 0,
    };

    return Event{ .key = .{ .code = code, .mods = mods } };
}

fn parseMouseLegacy(reader: *std.Io.Reader) !Event {
    const action = readByteOrNull(reader) orelse return .invalid;
    const x = readByteOrNull(reader) orelse return .invalid;
    const y = readByteOrNull(reader) orelse return .invalid;

    var mouse = Mouse{
        .x = x -| 32, // Subtract 32, saturating
        .y = y -| 32,
        .button = .left,
        .mods = .{
            .shift = (action & 0x04) != 0,
            .alt = (action & 0x08) != 0,
            .ctrl = (action & 0x10) != 0,
        },
    };

    // Scroll or movement
    if ((action & 0x40) != 0) {
        mouse.button = switch (action & 0x03) {
            0 => .scroll_up,
            1 => .scroll_down,
            else => .move,
        };
    } else {
        // Button clicks
        mouse.button = switch (action & 0x03) {
            0 => .left,
            1 => .middle,
            2 => .right,
            3 => .release,
            else => .left,
        };
    }

    return Event{ .mouse = mouse };
}

fn parseMouseSgr(reader: *std.Io.Reader) !Event {
    // SGR format: <button;x;y[Mm]
    var buffer: [32]u8 = undefined;
    var i: usize = 0;

    while (i < buffer.len - 1) {
        const c = readByteOrNull(reader) orelse break;
        if (c == 'M' or c == 'm') {
            // Parse the sequence
            const parts = buffer[0..i];
            var iter = std.mem.splitScalar(u8, parts, ';');

            const btn_str = iter.next() orelse return .invalid;
            const x_str = iter.next() orelse return .invalid;
            const y_str = iter.next() orelse return .invalid;

            const btn = std.fmt.parseInt(u8, btn_str, 10) catch return .invalid;
            const x = std.fmt.parseInt(u16, x_str, 10) catch return .invalid;
            const y = std.fmt.parseInt(u16, y_str, 10) catch return .invalid;

            var mouse = Mouse{
                .x = x,
                .y = y,
                .button = .left,
                .mods = .{
                    .shift = (btn & 0x04) != 0,
                    .alt = (btn & 0x08) != 0,
                    .ctrl = (btn & 0x10) != 0,
                },
            };

            if ((btn & 0x40) != 0) {
                mouse.button = if ((btn & 0x01) != 0) .scroll_down else .scroll_up;
            } else if ((btn & 0x20) != 0) {
                mouse.button = .move;
            } else if (c == 'm') {
                mouse.button = .release;
            } else {
                mouse.button = switch (btn & 0x03) {
                    0 => .left,
                    1 => .middle,
                    2 => .right,
                    else => .left,
                };
            }

            return Event{ .mouse = mouse };
        }
        buffer[i] = c;
        i += 1;
    }

    return .invalid;
}

test "parseSpecialKey" {
    try std.testing.expectEqual(KeyCode.home, parseSpecialKey("1").key.code);
    try std.testing.expectEqual(KeyCode.delete, parseSpecialKey("3").key.code);
    try std.testing.expectEqual(KeyCode.f5, parseSpecialKey("15").key.code);
}

test "Modifiers default to false" {
    const mods = Modifiers{};
    try std.testing.expect(!mods.shift);
    try std.testing.expect(!mods.alt);
    try std.testing.expect(!mods.ctrl);
}
