//! Renderer for Cot TUI using TUI.zig backend
//! Bridges Cot's widget tree to TUI.zig's component system.

const std = @import("std");
const tui = @import("tui");
const Widget = @import("widgets.zig").Widget;
const WidgetType = @import("widgets.zig").WidgetType;
const Screen = @import("screen.zig").Screen;
const tui_root = @import("tui.zig");
const Style = tui_root.Style;
const BorderStyle = tui_root.BorderStyle;

/// Cot TUI Renderer - bridges to TUI.zig
pub const Renderer = struct {
    screen: *Screen,
    allocator: std.mem.Allocator,

    /// Render callback for TUI.zig
    pub fn render(self: *Renderer, ctx: *tui.RenderContext) void {
        var sub = ctx.getSubScreen();

        if (self.screen.root) |root| {
            self.renderWidget(root, &sub, 0, 0);
        }
    }

    /// Handle events from TUI.zig
    pub fn handleEvent(self: *Renderer, event: tui.Event) tui.EventResult {
        switch (event) {
            .key => |k| {
                // Translate to our key format and pass to screen
                const key = translateKey(k);
                const modifiers = @import("events.zig").KeyData.Modifiers{
                    .ctrl = k.modifiers.ctrl,
                    .alt = k.modifiers.alt,
                    .shift = k.modifiers.shift,
                };

                // Check for quit
                if (k.modifiers.ctrl and (k.key == .char and k.char == 'q' or k.char == 'c')) {
                    return .quit;
                }

                self.screen.handleKey(key, modifiers);
                return .needs_redraw;
            },
            else => return .ignored,
        }
    }

    /// Render a widget to the TUI.zig screen
    fn renderWidget(self: *Renderer, widget: *Widget, screen: *tui.Screen, base_x: u16, base_y: u16) void {
        if (!widget.visible) return;

        const x = base_x + widget.rect.x;
        const y = base_y + widget.rect.y;

        switch (widget.widget_type) {
            .window => self.renderWindow(widget, screen, x, y),
            .panel => self.renderPanel(widget, screen, x, y),
            .label => self.renderLabel(widget, screen, x, y),
            .input => self.renderInput(widget, screen, x, y),
            .button => self.renderButton(widget, screen, x, y),
            .table => self.renderTable(widget, screen, x, y),
            .separator => self.renderSeparator(widget, screen, x, y),
            .checkbox => self.renderCheckbox(widget, screen, x, y),
            .row, .column, .grid => {
                for (widget.children.items) |child| {
                    self.renderWidget(child, screen, x, y);
                }
            },
            else => {},
        }
    }

    fn renderWindow(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        const data = widget.data.window;
        const w = widget.rect.width;
        const h = widget.rect.height;

        if (data.border) {
            // Draw border
            self.drawBox(screen, x, y, w, h, data.border_style);

            // Draw title
            if (data.title.len > 0) {
                screen.putStringAt(x + 2, y, data.title);
            }
        }

        // Render children
        const offset: u16 = if (data.border) 1 else 0;
        for (widget.children.items) |child| {
            self.renderWidget(child, screen, x + offset, y + offset);
        }
    }

    fn renderPanel(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        const data = widget.data.panel;
        const w = widget.rect.width;
        const h = widget.rect.height;

        if (data.border) {
            self.drawBox(screen, x, y, w, h, .single);
            if (data.title.len > 0) {
                screen.putStringAt(x + 2, y, data.title);
            }
        }

        for (widget.children.items) |child| {
            self.renderWidget(child, screen, x + 1, y + 1);
        }
    }

    fn renderLabel(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const data = widget.data.label;
        screen.putStringAt(x, y, data.text);
    }

    fn renderInput(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const data = widget.data.input;
        const w = widget.rect.width;

        // Draw input field with value or placeholder
        var buf: [256]u8 = undefined;
        @memset(&buf, ' ');

        const text = if (data.value.items.len > 0)
            data.value.items
        else
            data.placeholder;

        const copy_len = @min(text.len, w);
        if (data.password) {
            for (0..copy_len) |i| {
                buf[i] = '*';
            }
        } else {
            @memcpy(buf[0..copy_len], text[0..copy_len]);
        }

        // Apply style based on focus
        if (widget.focused) {
            screen.putStringAt(x, y, buf[0..w]);
            // Highlight focused input
            // TODO: Use tui style for reverse video
        } else {
            screen.putStringAt(x, y, buf[0..w]);
        }
    }

    fn renderButton(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const data = widget.data.button;

        var buf: [64]u8 = undefined;
        const text = std.fmt.bufPrint(&buf, "[ {s} ]", .{data.label}) catch data.label;

        screen.putStringAt(x, y, text);
    }

    fn renderTable(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const data = &widget.data.table;
        const w = widget.rect.width;

        if (data.columns.items.len == 0) return;

        var row_y = y;

        // Draw header
        var col_x = x;
        for (data.columns.items) |col| {
            screen.putStringAt(col_x, row_y, col.header);
            col_x += col.width + 1;
        }
        row_y += 1;

        // Draw separator
        var sep_buf: [256]u8 = undefined;
        @memset(sep_buf[0..@min(w, 256)], '-');
        screen.putStringAt(x, row_y, sep_buf[0..@min(w, 256)]);
        row_y += 1;

        // Draw data rows
        const visible_rows = widget.rect.height -| 3;
        const start = data.scroll_offset;
        const end = @min(start + visible_rows, data.rows.items.len);

        for (data.rows.items[start..end], start..) |row, idx| {
            col_x = x;

            // Highlight selected
            const selected = if (data.selected_row) |sel| sel == idx else false;
            _ = selected; // TODO: Apply highlight style

            for (data.columns.items, 0..) |col, col_idx| {
                const cell = if (col_idx < row.cells.items.len) row.cells.items[col_idx] else "";
                const truncated = if (cell.len > col.width) cell[0..col.width] else cell;
                screen.putStringAt(col_x, row_y, truncated);
                col_x += col.width + 1;
            }
            row_y += 1;
        }
    }

    fn renderSeparator(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const w = widget.rect.width;
        var buf: [256]u8 = undefined;
        @memset(buf[0..@min(w, 256)], '-');
        screen.putStringAt(x, y, buf[0..@min(w, 256)]);
    }

    fn renderCheckbox(self: *Renderer, widget: *Widget, screen: *tui.Screen, x: u16, y: u16) void {
        _ = self;
        const data = widget.data.checkbox;

        var buf: [256]u8 = undefined;
        const checkbox = if (data.checked) "[x] " else "[ ] ";
        const text = std.fmt.bufPrint(&buf, "{s}{s}", .{ checkbox, data.label }) catch data.label;

        screen.putStringAt(x, y, text);
    }

    fn drawBox(self: *Renderer, screen: *tui.Screen, x: u16, y: u16, w: u16, h: u16, style: BorderStyle) void {
        _ = self;
        const chars = switch (style) {
            .single => .{ .tl = "┌", .tr = "┐", .bl = "└", .br = "┘", .h = "─", .v = "│" },
            .double => .{ .tl = "╔", .tr = "╗", .bl = "╚", .br = "╝", .h = "═", .v = "║" },
            .rounded => .{ .tl = "╭", .tr = "╮", .bl = "╰", .br = "╯", .h = "─", .v = "│" },
            .heavy => .{ .tl = "┏", .tr = "┓", .bl = "┗", .br = "┛", .h = "━", .v = "┃" },
            .none => return,
        };

        // Top
        screen.putStringAt(x, y, chars.tl);
        var i: u16 = 1;
        while (i < w - 1) : (i += 1) {
            screen.putStringAt(x + i, y, chars.h);
        }
        screen.putStringAt(x + w - 1, y, chars.tr);

        // Sides
        var row: u16 = 1;
        while (row < h - 1) : (row += 1) {
            screen.putStringAt(x, y + row, chars.v);
            screen.putStringAt(x + w - 1, y + row, chars.v);
        }

        // Bottom
        screen.putStringAt(x, y + h - 1, chars.bl);
        i = 1;
        while (i < w - 1) : (i += 1) {
            screen.putStringAt(x + i, y + h - 1, chars.h);
        }
        screen.putStringAt(x + w - 1, y + h - 1, chars.br);
    }
};

fn translateKey(k: tui.Event.Key) @import("events.zig").KeyData.Key {
    if (k.key == .char) {
        return .{ .char = k.char };
    }

    return .{
        .special = switch (k.key) {
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
            else => .enter, // Default
        },
    };
}
