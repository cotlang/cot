//! Cot TUI Extension
//!
//! Provides terminal user interface native functions for Cot programs.
//! This is an optional extension - programs that don't use TUI don't need it.
//!
//! Full-featured TUI library exposing all capabilities of tui.zig:
//! - Terminal control (cursor, screen modes)
//! - Colors (16-color, 256-palette, 24-bit RGB)
//! - Text styles (bold, italic, underline, etc.)
//! - Input handling (keyboard, mouse)
//! - Drawing primitives (boxes, lines, fills)
//! - Widgets (buttons, inputs, checkboxes, sliders, lists, etc.)
//! - Tables and data display
//! - Dialogs and modals

const std = @import("std");
const tui = @import("tui");
const cot_runtime = @import("cot_runtime");

const Allocator = std.mem.Allocator;
const Extension = cot_runtime.extension.Extension;
const NativeDef = cot_runtime.extension.NativeDef;
const NativeContext = cot_runtime.extension.NativeContext;
const NativeError = cot_runtime.extension.NativeError;
const Value = cot_runtime.extension.Value;

// Re-export the TUI runtime for direct use - use cot_runtime's tui_runtime
// to share state with run.zig (dev mode, watch file, etc.)
pub const tui_runtime = cot_runtime.native.tui_runtime;

/// The TUI extension definition
pub const extension = Extension{
    .name = "tui",
    .version = "0.1.0",
    .natives = &natives,
    .init = extensionInit,
    .deinit = extensionDeinit,
};

fn extensionInit(allocator: Allocator) void {
    _ = allocator;
    // TUI init is deferred to t_init xcall
}

fn extensionDeinit() void {
    tui_runtime.deinit();
}

/// All TUI xcalls - comprehensive terminal UI functionality
const natives = [_]NativeDef{
    // ============================================
    // Core Terminal Control
    // ============================================
    .{ .name = "t_init", .handler = native_t_init, .min_args = 0, .max_args = 0 },
    .{ .name = "t_end", .handler = native_t_end, .min_args = 0, .max_args = 0 },
    .{ .name = "t_clear", .handler = native_t_clear, .min_args = 0, .max_args = 0 },
    .{ .name = "t_refresh", .handler = native_t_refresh, .min_args = 0, .max_args = 0 },
    .{ .name = "t_size", .handler = native_t_size, .min_args = 0, .max_args = 2 },

    // Cursor Control
    .{ .name = "t_cursor_show", .handler = native_t_cursor_show, .min_args = 0, .max_args = 0 },
    .{ .name = "t_cursor_hide", .handler = native_t_cursor_hide, .min_args = 0, .max_args = 0 },
    .{ .name = "t_cursor_save", .handler = native_t_cursor_save, .min_args = 0, .max_args = 0 },
    .{ .name = "t_cursor_restore", .handler = native_t_cursor_restore, .min_args = 0, .max_args = 0 },
    .{ .name = "t_goto", .handler = native_t_goto, .min_args = 2, .max_args = 2 },

    // ============================================
    // Colors and Styles
    // ============================================
    // Basic 256-color palette
    .{ .name = "t_color", .handler = native_t_color, .min_args = 2, .max_args = 2 },
    .{ .name = "t_fg", .handler = native_t_fg, .min_args = 1, .max_args = 1 },
    .{ .name = "t_bg", .handler = native_t_bg, .min_args = 1, .max_args = 1 },

    // 24-bit RGB colors
    .{ .name = "t_color_rgb", .handler = native_t_color_rgb, .min_args = 6, .max_args = 6 },
    .{ .name = "t_fg_rgb", .handler = native_t_fg_rgb, .min_args = 3, .max_args = 3 },
    .{ .name = "t_bg_rgb", .handler = native_t_bg_rgb, .min_args = 3, .max_args = 3 },

    // Text attributes
    .{ .name = "t_bold", .handler = native_t_bold, .min_args = 0, .max_args = 1 },
    .{ .name = "t_dim", .handler = native_t_dim, .min_args = 0, .max_args = 1 },
    .{ .name = "t_italic", .handler = native_t_italic, .min_args = 0, .max_args = 1 },
    .{ .name = "t_underline", .handler = native_t_underline, .min_args = 0, .max_args = 1 },
    .{ .name = "t_blink", .handler = native_t_blink, .min_args = 0, .max_args = 1 },
    .{ .name = "t_reverse", .handler = native_t_reverse, .min_args = 0, .max_args = 1 },
    .{ .name = "t_strikethrough", .handler = native_t_strikethrough, .min_args = 0, .max_args = 1 },
    .{ .name = "t_reset", .handler = native_t_reset, .min_args = 0, .max_args = 0 },

    // ============================================
    // Drawing Primitives
    // ============================================
    .{ .name = "t_print", .handler = native_t_print, .min_args = 3, .max_args = 3 },
    .{ .name = "t_putc", .handler = native_t_putc, .min_args = 3, .max_args = 3 },
    .{ .name = "t_box", .handler = native_t_box, .min_args = 4, .max_args = 5 },
    .{ .name = "t_fill", .handler = native_t_fill, .min_args = 5, .max_args = 5 },
    .{ .name = "t_hline", .handler = native_t_hline, .min_args = 3, .max_args = 4 },
    .{ .name = "t_vline", .handler = native_t_vline, .min_args = 3, .max_args = 4 },
    .{ .name = "t_border", .handler = native_t_border, .min_args = 4, .max_args = 5 },

    // ============================================
    // Input Handling
    // ============================================
    .{ .name = "t_getkey", .handler = native_t_getkey, .min_args = 0, .max_args = 1 },
    .{ .name = "t_waitkey", .handler = native_t_waitkey, .min_args = 0, .max_args = 1 },
    .{ .name = "t_haskey", .handler = native_t_haskey, .min_args = 0, .max_args = 0 },
    .{ .name = "t_mouse_on", .handler = native_t_mouse_on, .min_args = 0, .max_args = 0 },
    .{ .name = "t_mouse_off", .handler = native_t_mouse_off, .min_args = 0, .max_args = 0 },
    .{ .name = "t_getmouse", .handler = native_t_getmouse, .min_args = 0, .max_args = 0 },

    // ============================================
    // Basic Widgets
    // ============================================
    .{ .name = "t_label", .handler = native_t_label, .min_args = 3, .max_args = 4 },
    .{ .name = "t_button", .handler = native_t_button, .min_args = 3, .max_args = 4 },
    .{ .name = "t_input", .handler = native_t_input, .min_args = 4, .max_args = 6 },
    .{ .name = "t_checkbox", .handler = native_t_checkbox, .min_args = 4, .max_args = 5 },
    .{ .name = "t_radio", .handler = native_t_radio, .min_args = 5, .max_args = 6 },
    .{ .name = "t_slider", .handler = native_t_slider, .min_args = 6, .max_args = 7 },
    .{ .name = "t_progress", .handler = native_t_progress, .min_args = 4, .max_args = 5 },
    .{ .name = "t_spinner", .handler = native_t_spinner, .min_args = 3, .max_args = 4 },

    // ============================================
    // Container Widgets
    // ============================================
    .{ .name = "t_card", .handler = native_t_card, .min_args = 4, .max_args = 5 },
    .{ .name = "t_panel", .handler = native_t_panel, .min_args = 4, .max_args = 5 },
    .{ .name = "t_modal", .handler = native_t_modal, .min_args = 2, .max_args = 4 },
    .{ .name = "t_alert", .handler = native_t_alert, .min_args = 2, .max_args = 3 },
    .{ .name = "t_confirm", .handler = native_t_confirm, .min_args = 2, .max_args = 3 },

    // ============================================
    // Lists and Tables
    // ============================================
    .{ .name = "t_list", .handler = native_t_list, .min_args = 5, .max_args = 255 },
    .{ .name = "t_table_header", .handler = native_t_table_header, .min_args = 4, .max_args = 255 },
    .{ .name = "t_table_row", .handler = native_t_table_row, .min_args = 5, .max_args = 255 },
    .{ .name = "t_table_sep", .handler = native_t_table_sep, .min_args = 3, .max_args = 255 },

    // ============================================
    // Visual Elements
    // ============================================
    .{ .name = "t_separator", .handler = native_t_separator, .min_args = 3, .max_args = 4 },
    .{ .name = "t_badge", .handler = native_t_badge, .min_args = 3, .max_args = 4 },
    .{ .name = "t_tag", .handler = native_t_tag, .min_args = 3, .max_args = 4 },
    .{ .name = "t_divider", .handler = native_t_divider, .min_args = 3, .max_args = 4 },

    // ============================================
    // Menus and Navigation
    // ============================================
    .{ .name = "t_menu", .handler = native_t_menu, .min_args = 4, .max_args = 255 },
    .{ .name = "t_menubar", .handler = native_t_menubar, .min_args = 2, .max_args = 255 },
    .{ .name = "t_statusbar", .handler = native_t_statusbar, .min_args = 2, .max_args = 3 },
    .{ .name = "t_breadcrumb", .handler = native_t_breadcrumb, .min_args = 3, .max_args = 255 },
    .{ .name = "t_tabs", .handler = native_t_tabs, .min_args = 4, .max_args = 255 },

    // ============================================
    // Utility
    // ============================================
    .{ .name = "t_beep", .handler = native_t_beep, .min_args = 0, .max_args = 0 },
    .{ .name = "t_title", .handler = native_t_title, .min_args = 1, .max_args = 1 },
    .{ .name = "t_scroll_up", .handler = native_t_scroll_up, .min_args = 0, .max_args = 1 },
    .{ .name = "t_scroll_down", .handler = native_t_scroll_down, .min_args = 0, .max_args = 1 },
};

// ============================================
// Helper Functions
// ============================================

/// Extract integer from Value
fn getInt(val: Value) ?i64 {
    return switch (val.tag()) {
        .integer => val.asInt(),
        .implied_decimal => if (val.asDecimal()) |d| @divTrunc(d.value, std.math.pow(i64, 10, d.precision)) else null,
        else => null,
    };
}

/// Extract string from Value
fn getString(val: Value) ?[]const u8 {
    return switch (val.tag()) {
        .string, .fixed_string => val.asString(),
        else => null,
    };
}

// ============================================
// Core Terminal Control
// ============================================

fn native_t_init(ctx: *NativeContext) NativeError!?Value {
    tui_runtime.init(ctx.allocator);
    return null;
}

fn native_t_end(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.deinit();
    return null;
}

fn native_t_clear(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.clear();
    return null;
}

fn native_t_refresh(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.refresh();
    return null;
}

fn native_t_size(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const size = tui_runtime.getScreenSize();
    // Return width * 10000 + height for easy extraction
    return Value.initInt(@as(i64, size.width) * 10000 + size.height);
}

fn native_t_cursor_show(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.showCursor();
    return null;
}

fn native_t_cursor_hide(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.hideCursor();
    return null;
}

fn native_t_cursor_save(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.saveCursor();
    return null;
}

fn native_t_cursor_restore(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.restoreCursor();
    return null;
}

fn native_t_goto(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    tui_runtime.moveCursor(@intCast(col), @intCast(row));
    return null;
}

// ============================================
// Colors and Styles
// ============================================

fn native_t_color(ctx: *NativeContext) NativeError!?Value {
    const fg = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const bg = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    tui_runtime.setColor(@intCast(@mod(fg, 256)), @intCast(@mod(bg, 256)));
    return null;
}

fn native_t_fg(ctx: *NativeContext) NativeError!?Value {
    const fg = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    tui_runtime.setFg(@intCast(@mod(fg, 256)));
    return null;
}

fn native_t_bg(ctx: *NativeContext) NativeError!?Value {
    const bg = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    tui_runtime.setBg(@intCast(@mod(bg, 256)));
    return null;
}

fn native_t_color_rgb(ctx: *NativeContext) NativeError!?Value {
    const fg_r = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const fg_g = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const fg_b = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const bg_r = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const bg_g = ctx.getArgInt(4) catch return NativeError.InvalidArgument;
    const bg_b = ctx.getArgInt(5) catch return NativeError.InvalidArgument;
    tui_runtime.setColorRGB(
        @intCast(@mod(fg_r, 256)),
        @intCast(@mod(fg_g, 256)),
        @intCast(@mod(fg_b, 256)),
        @intCast(@mod(bg_r, 256)),
        @intCast(@mod(bg_g, 256)),
        @intCast(@mod(bg_b, 256)),
    );
    return null;
}

fn native_t_fg_rgb(ctx: *NativeContext) NativeError!?Value {
    const r = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const g = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const b = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    tui_runtime.setFgRGB(@intCast(@mod(r, 256)), @intCast(@mod(g, 256)), @intCast(@mod(b, 256)));
    return null;
}

fn native_t_bg_rgb(ctx: *NativeContext) NativeError!?Value {
    const r = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const g = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const b = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    tui_runtime.setBgRGB(@intCast(@mod(r, 256)), @intCast(@mod(g, 256)), @intCast(@mod(b, 256)));
    return null;
}

fn native_t_bold(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setBold(enabled);
    return null;
}

fn native_t_dim(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setDim(enabled);
    return null;
}

fn native_t_italic(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setItalic(enabled);
    return null;
}

fn native_t_underline(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setUnderline(enabled);
    return null;
}

fn native_t_blink(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setBlink(enabled);
    return null;
}

fn native_t_reverse(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setReverse(enabled);
    return null;
}

fn native_t_strikethrough(ctx: *NativeContext) NativeError!?Value {
    const enabled = if (ctx.args.len > 0) (ctx.getArgInt(0) catch 1) != 0 else true;
    tui_runtime.setStrikethrough(enabled);
    return null;
}

fn native_t_reset(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.resetStyle();
    return null;
}

// ============================================
// Drawing Primitives
// ============================================

fn native_t_print(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const text = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    tui_runtime.drawText(@intCast(col), @intCast(row), text);
    return null;
}

fn native_t_putc(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const char_val = ctx.getArg(2) orelse return NativeError.InvalidArgument;

    const ch: u8 = switch (char_val.tag()) {
        .integer => @intCast(@mod(char_val.asInt(), 256)),
        .fixed_string, .string => blk: {
            const s = char_val.asString();
            break :blk if (s.len > 0) s[0] else ' ';
        },
        else => ' ',
    };

    tui_runtime.putChar(@intCast(col), @intCast(row), ch);
    return null;
}

fn native_t_box(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 4) ctx.getArgInt(4) catch 0 else 0;
    tui_runtime.drawBox(@intCast(col), @intCast(row), @intCast(width), @intCast(height), @intCast(style));
    return null;
}

fn native_t_fill(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const char_val = ctx.getArg(4) orelse return NativeError.InvalidArgument;

    const fill_char: u8 = switch (char_val.tag()) {
        .integer => @intCast(@mod(char_val.asInt(), 256)),
        .fixed_string, .string => blk: {
            const s = char_val.asString();
            break :blk if (s.len > 0) s[0] else ' ';
        },
        else => ' ',
    };

    tui_runtime.fillArea(@intCast(col), @intCast(row), @intCast(width), @intCast(height), fill_char);
    return null;
}

fn native_t_hline(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const length = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawHLine(@intCast(col), @intCast(row), @intCast(length), @intCast(style));
    return null;
}

fn native_t_vline(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const length = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawVLine(@intCast(col), @intCast(row), @intCast(length), @intCast(style));
    return null;
}

fn native_t_border(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 4) ctx.getArgInt(4) catch 0 else 0;
    // Border is same as box but we provide explicit semantics
    tui_runtime.drawBox(@intCast(col), @intCast(row), @intCast(width), @intCast(height), @intCast(style));
    return null;
}

// ============================================
// Input Handling
// ============================================

fn native_t_getkey(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    if (tui_runtime.pollKey()) |key| {
        return keyToValue(key);
    }
    return Value.initInt(0);
}

fn native_t_waitkey(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const key = tui_runtime.waitKey();

    if (tui_runtime.isForceReload()) {
        return NativeError.ReloadRequested;
    }

    return keyToValue(key);
}

fn native_t_haskey(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const has_key = tui_runtime.hasKey();
    return Value.initInt(if (has_key) 1 else 0);
}

fn native_t_mouse_on(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.enableMouse();
    return null;
}

fn native_t_mouse_off(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.disableMouse();
    return null;
}

fn native_t_getmouse(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    const mouse = tui_runtime.getMouse();
    // Pack: button * 1000000 + x * 1000 + y
    return Value.initInt(@as(i64, mouse.button) * 1000000 + @as(i64, mouse.x) * 1000 + mouse.y);
}

/// Convert Key to integer Value
fn keyToValue(key: tui_runtime.Key) Value {
    return switch (key) {
        .char => |c| blk: {
            if (c == 3) {
                tui_runtime.deinit();
                std.process.exit(0);
            }
            break :blk Value.initInt(@intCast(c));
        },
        .enter => Value.initInt(13),
        .escape => Value.initInt(27),
        .tab => Value.initInt(9),
        .backspace => Value.initInt(8),
        .delete => Value.initInt(127),
        .insert => Value.initInt(256 + 5),
        .home => Value.initInt(256 + 6),
        .end => Value.initInt(256 + 7),
        .page_up => Value.initInt(256 + 8),
        .page_down => Value.initInt(256 + 9),
        .up => Value.initInt(256 + 1),
        .down => Value.initInt(256 + 2),
        .left => Value.initInt(256 + 3),
        .right => Value.initInt(256 + 4),
        .f => |n| Value.initInt(256 + 10 + @as(i64, n)), // F1=266, F2=267, etc.
        else => Value.initInt(0),
    };
}

// ============================================
// Basic Widgets
// ============================================

fn native_t_label(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const text = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawLabel(@intCast(col), @intCast(row), text, @intCast(style));
    return null;
}

fn native_t_button(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const label = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const focused = if (ctx.args.len > 3) (ctx.getArgInt(3) catch 0) != 0 else false;
    tui_runtime.drawButton(@intCast(col), @intCast(row), label, focused);
    return null;
}

fn native_t_input(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const value = ctx.getArgString(3) catch return NativeError.InvalidArgument;
    const cursor_pos = if (ctx.args.len > 4) ctx.getArgInt(4) catch 0 else 0;
    const focused = if (ctx.args.len > 5) (ctx.getArgInt(5) catch 0) != 0 else false;
    tui_runtime.drawInput(@intCast(col), @intCast(row), @intCast(width), value, @intCast(cursor_pos), focused);
    return null;
}

fn native_t_checkbox(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const label = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const checked = (ctx.getArgInt(3) catch 0) != 0;
    const focused = if (ctx.args.len > 4) (ctx.getArgInt(4) catch 0) != 0 else false;
    tui_runtime.drawCheckbox(@intCast(col), @intCast(row), label, checked, focused);
    return null;
}

fn native_t_radio(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const label = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const selected = (ctx.getArgInt(3) catch 0) != 0;
    const group_id = ctx.getArgInt(4) catch 0;
    const focused = if (ctx.args.len > 5) (ctx.getArgInt(5) catch 0) != 0 else false;
    tui_runtime.drawRadio(@intCast(col), @intCast(row), label, selected, @intCast(group_id), focused);
    return null;
}

fn native_t_slider(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const min_val = ctx.getArgInt(3) catch 0;
    const max_val = ctx.getArgInt(4) catch 100;
    const current = ctx.getArgInt(5) catch 0;
    const focused = if (ctx.args.len > 6) (ctx.getArgInt(6) catch 0) != 0 else false;
    tui_runtime.drawSlider(@intCast(col), @intCast(row), @intCast(width), @intCast(min_val), @intCast(max_val), @intCast(current), focused);
    return null;
}

fn native_t_progress(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const percent = ctx.getArgInt(3) catch 0;
    const show_percent = if (ctx.args.len > 4) (ctx.getArgInt(4) catch 1) != 0 else true;
    tui_runtime.drawProgressBar(@intCast(col), @intCast(row), @intCast(width), @intCast(@mod(percent, 101)), show_percent);
    return null;
}

fn native_t_spinner(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const frame = ctx.getArgInt(2) catch 0;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawSpinner(@intCast(col), @intCast(row), @intCast(@mod(frame, 256)), @intCast(style));
    return null;
}

// ============================================
// Container Widgets
// ============================================

fn native_t_card(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const title = if (ctx.args.len > 4) ctx.getArgString(4) catch "" else "";
    tui_runtime.drawCard(@intCast(col), @intCast(row), @intCast(width), @intCast(height), title);
    return null;
}

fn native_t_panel(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 4) ctx.getArgInt(4) catch 0 else 0;
    tui_runtime.drawPanel(@intCast(col), @intCast(row), @intCast(width), @intCast(height), @intCast(style));
    return null;
}

fn native_t_modal(ctx: *NativeContext) NativeError!?Value {
    const title = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const message = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const width = if (ctx.args.len > 2) ctx.getArgInt(2) catch 50 else 50;
    const height = if (ctx.args.len > 3) ctx.getArgInt(3) catch 7 else 7;
    tui_runtime.showModalEx(title, message, @intCast(width), @intCast(height));
    return null;
}

fn native_t_alert(ctx: *NativeContext) NativeError!?Value {
    const title = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const message = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const alert_type = if (ctx.args.len > 2) ctx.getArgInt(2) catch 0 else 0;
    tui_runtime.showAlert(title, message, @intCast(alert_type));
    return null;
}

fn native_t_confirm(ctx: *NativeContext) NativeError!?Value {
    const title = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const message = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const default_yes = if (ctx.args.len > 2) (ctx.getArgInt(2) catch 1) != 0 else true;
    tui_runtime.showConfirm(title, message, default_yes);
    return null;
}

// ============================================
// Lists and Tables
// ============================================

fn native_t_list(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const height = ctx.getArgInt(3) catch return NativeError.InvalidArgument;
    const selected = ctx.getArgInt(4) catch 0;

    // Remaining args are list items
    var items: [32][]const u8 = undefined;
    var count: usize = 0;
    var i: usize = 5;
    while (i < ctx.args.len and count < 32) {
        items[count] = getString(ctx.args[i]) orelse "";
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawList(@intCast(col), @intCast(row), @intCast(width), @intCast(height), items[0..count], @intCast(selected));
    }
    return null;
}

fn native_t_table_header(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;

    var columns: [16][]const u8 = undefined;
    var widths: [16]u16 = undefined;
    var count: usize = 0;

    var i: usize = 2;
    while (i + 1 < ctx.args.len and count < 16) {
        columns[count] = getString(ctx.args[i]) orelse "";
        widths[count] = @intCast(getInt(ctx.args[i + 1]) orelse 10);
        count += 1;
        i += 2;
    }

    if (count > 0) {
        tui_runtime.drawTableHeader(@intCast(col), @intCast(row), columns[0..count], widths[0..count]);
    }
    return null;
}

fn native_t_table_row(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const selected = ctx.getArgInt(2) catch 0;

    var cells: [16][]const u8 = undefined;
    var widths: [16]u16 = undefined;
    var count: usize = 0;

    var i: usize = 3;
    while (i + 1 < ctx.args.len and count < 16) {
        cells[count] = getString(ctx.args[i]) orelse "";
        widths[count] = @intCast(getInt(ctx.args[i + 1]) orelse 10);
        count += 1;
        i += 2;
    }

    if (count > 0) {
        tui_runtime.drawTableRow(@intCast(col), @intCast(row), cells[0..count], widths[0..count], selected != 0);
    }
    return null;
}

fn native_t_table_sep(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;

    var widths: [16]u16 = undefined;
    var count: usize = 0;

    var i: usize = 2;
    while (i < ctx.args.len and count < 16) {
        widths[count] = @intCast(getInt(ctx.args[i]) orelse 10);
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawTableSeparator(@intCast(col), @intCast(row), widths[0..count]);
    }
    return null;
}

// ============================================
// Visual Elements
// ============================================

fn native_t_separator(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawSeparator(@intCast(col), @intCast(row), @intCast(width), @intCast(style));
    return null;
}

fn native_t_badge(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const text = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 3) ctx.getArgInt(3) catch 0 else 0;
    tui_runtime.drawBadge(@intCast(col), @intCast(row), text, @intCast(style));
    return null;
}

fn native_t_tag(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const text = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const color = if (ctx.args.len > 3) ctx.getArgInt(3) catch 6 else 6; // Default cyan
    tui_runtime.drawTag(@intCast(col), @intCast(row), text, @intCast(@mod(color, 256)));
    return null;
}

fn native_t_divider(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const label = if (ctx.args.len > 3) ctx.getArgString(3) catch "" else "";
    tui_runtime.drawDivider(@intCast(col), @intCast(row), @intCast(width), label);
    return null;
}

// ============================================
// Menus and Navigation
// ============================================

fn native_t_menu(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const selected = ctx.getArgInt(3) catch 0;

    var items: [16][]const u8 = undefined;
    var count: usize = 0;

    var i: usize = 4;
    while (i < ctx.args.len and count < 16) {
        items[count] = getString(ctx.args[i]) orelse "";
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawMenu(@intCast(col), @intCast(row), @intCast(width), items[0..count], @intCast(selected));
    }
    return null;
}

fn native_t_menubar(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const selected = ctx.getArgInt(1) catch -1;

    var items: [16][]const u8 = undefined;
    var count: usize = 0;

    var i: usize = 2;
    while (i < ctx.args.len and count < 16) {
        items[count] = getString(ctx.args[i]) orelse "";
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawMenuBar(@intCast(row), items[0..count], @intCast(selected));
    }
    return null;
}

fn native_t_statusbar(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const text = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const style = if (ctx.args.len > 2) ctx.getArgInt(2) catch 0 else 0;
    tui_runtime.drawStatusBar(@intCast(row), text, @intCast(style));
    return null;
}

fn native_t_breadcrumb(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;

    var items: [16][]const u8 = undefined;
    var count: usize = 0;

    var i: usize = 2;
    while (i < ctx.args.len and count < 16) {
        items[count] = getString(ctx.args[i]) orelse "";
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawBreadcrumb(@intCast(col), @intCast(row), items[0..count]);
    }
    return null;
}

fn native_t_tabs(ctx: *NativeContext) NativeError!?Value {
    const row = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const col = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const width = ctx.getArgInt(2) catch return NativeError.InvalidArgument;
    const active = ctx.getArgInt(3) catch 0;

    var tabs: [16][]const u8 = undefined;
    var count: usize = 0;

    var i: usize = 4;
    while (i < ctx.args.len and count < 16) {
        tabs[count] = getString(ctx.args[i]) orelse "";
        count += 1;
        i += 1;
    }

    if (count > 0) {
        tui_runtime.drawTabs(@intCast(col), @intCast(row), @intCast(width), tabs[0..count], @intCast(active));
    }
    return null;
}

// ============================================
// Utility
// ============================================

fn native_t_beep(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    tui_runtime.beep();
    return null;
}

fn native_t_title(ctx: *NativeContext) NativeError!?Value {
    const title = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    tui_runtime.setTitle(title);
    return null;
}

fn native_t_scroll_up(ctx: *NativeContext) NativeError!?Value {
    const lines = if (ctx.args.len > 0) ctx.getArgInt(0) catch 1 else 1;
    tui_runtime.scrollUp(@intCast(lines));
    return null;
}

fn native_t_scroll_down(ctx: *NativeContext) NativeError!?Value {
    const lines = if (ctx.args.len > 0) ctx.getArgInt(0) catch 1 else 1;
    tui_runtime.scrollDown(@intCast(lines));
    return null;
}
