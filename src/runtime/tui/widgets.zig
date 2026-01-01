//! Widget definitions for Cot TUI
//! Provides the core widget types: label, input, button, table, etc.

const std = @import("std");
const tui_root = @import("tui.zig");
const Style = tui_root.Style;
const Color = tui_root.Color;
const Alignment = tui_root.Alignment;
const Rect = tui_root.Rect;

/// Maximum children per widget (fixed for Zig 0.15 compatibility)
pub const MAX_CHILDREN = 32;

/// Widget types supported by the TUI system
pub const WidgetType = enum {
    // Display widgets
    label,
    text,
    badge,
    separator,

    // Input widgets
    input,
    textarea,
    number,
    checkbox,
    radio,
    select,

    // Container widgets
    window,
    panel,
    row,
    column,
    grid,

    // Data widgets
    table,
    list,

    // Action widgets
    button,
    button_group,
};

/// Fixed-size widget list for Zig 0.15 compatibility
pub const WidgetList = struct {
    items: [MAX_CHILDREN]*Widget = undefined,
    len: usize = 0,

    pub fn append(self: *WidgetList, item: *Widget) !void {
        if (self.len >= MAX_CHILDREN) return error.OutOfMemory;
        self.items[self.len] = item;
        self.len += 1;
    }

    pub fn slice(self: *const WidgetList) []const *Widget {
        return self.items[0..self.len];
    }
};

/// A generic widget that can be any type
pub const Widget = struct {
    id: []const u8,
    widget_type: WidgetType,
    rect: Rect,
    style: Style,
    visible: bool = true,
    enabled: bool = true,
    focused: bool = false,
    data: WidgetData,
    children: WidgetList = .{},
    parent: ?*Widget = null,
    allocator: std.mem.Allocator,

    // Event handlers (stored as indices into a handler table)
    on_click: ?usize = null,
    on_change: ?usize = null,
    on_enter: ?usize = null,
    on_focus: ?usize = null,
    on_blur: ?usize = null,

    pub fn init(allocator: std.mem.Allocator, id: []const u8, widget_type: WidgetType) !*Widget {
        const widget = try allocator.create(Widget);
        widget.* = .{
            .id = id,
            .widget_type = widget_type,
            .rect = .{ .x = 0, .y = 0, .width = 0, .height = 0 },
            .style = .{},
            .data = .{ .none = {} },
            .children = .{},
            .allocator = allocator,
        };
        return widget;
    }

    pub fn deinit(self: *Widget) void {
        for (self.children.slice()) |child| {
            child.deinit();
        }
        self.allocator.destroy(self);
    }

    pub fn addChild(self: *Widget, child: *Widget) !void {
        child.parent = self;
        try self.children.append(child);
    }

    pub fn setRect(self: *Widget, x: u16, y: u16, width: u16, height: u16) void {
        self.rect = .{ .x = x, .y = y, .width = width, .height = height };
    }

    pub fn setStyle(self: *Widget, style: Style) void {
        self.style = style;
    }

    pub fn focus(self: *Widget) void {
        self.focused = true;
    }

    pub fn blur(self: *Widget) void {
        self.focused = false;
    }
};

/// Widget-specific data
pub const WidgetData = union(enum) {
    none: void,
    label: LabelData,
    input: InputData,
    button: ButtonData,
    table: TableData,
    window: WindowData,
    panel: PanelData,
    checkbox: CheckboxData,
    separator: SeparatorData,
};

/// Label widget data
pub const LabelData = struct {
    text: []const u8,
    alignment: Alignment = .left,
};

/// Maximum input buffer size
pub const MAX_INPUT_LENGTH = 256;

/// Input widget data
pub const InputData = struct {
    buffer: [MAX_INPUT_LENGTH]u8 = [_]u8{0} ** MAX_INPUT_LENGTH,
    len: usize = 0,
    placeholder: []const u8 = "",
    max_length: usize = MAX_INPUT_LENGTH,
    cursor_pos: usize = 0,
    password: bool = false,
    readonly: bool = false,

    pub fn init() InputData {
        return .{};
    }

    pub fn deinit(self: *InputData) void {
        _ = self;
        // No-op for fixed buffer
    }

    pub fn getValue(self: *const InputData) []const u8 {
        return self.buffer[0..self.len];
    }

    pub fn setValue(self: *InputData, text: []const u8) !void {
        const copy_len = @min(text.len, self.max_length);
        @memcpy(self.buffer[0..copy_len], text[0..copy_len]);
        self.len = copy_len;
        self.cursor_pos = copy_len;
    }

    pub fn insertChar(self: *InputData, c: u8) !void {
        if (self.len >= self.max_length) return;
        // Shift characters right from cursor position
        var i: usize = self.len;
        while (i > self.cursor_pos) : (i -= 1) {
            self.buffer[i] = self.buffer[i - 1];
        }
        self.buffer[self.cursor_pos] = c;
        self.len += 1;
        self.cursor_pos += 1;
    }

    pub fn deleteChar(self: *InputData) void {
        if (self.cursor_pos > 0 and self.len > 0) {
            // Shift characters left
            var i: usize = self.cursor_pos - 1;
            while (i < self.len - 1) : (i += 1) {
                self.buffer[i] = self.buffer[i + 1];
            }
            self.len -= 1;
            self.cursor_pos -= 1;
        }
    }

    pub fn moveCursor(self: *InputData, delta: i32) void {
        const new_pos = @as(i64, @intCast(self.cursor_pos)) + delta;
        if (new_pos < 0) {
            self.cursor_pos = 0;
        } else if (new_pos > self.len) {
            self.cursor_pos = self.len;
        } else {
            self.cursor_pos = @intCast(new_pos);
        }
    }
};

/// Button widget data
pub const ButtonData = struct {
    label: []const u8,
    style_type: ButtonStyle = .default,
    shortcut: ?u8 = null,

    pub const ButtonStyle = enum {
        default,
        primary,
        success,
        warning,
        danger,
    };
};

/// Maximum table dimensions
pub const MAX_COLUMNS = 16;
pub const MAX_ROWS = 256;

/// Table widget data
pub const TableData = struct {
    columns: [MAX_COLUMNS]TableColumn = undefined,
    column_count: usize = 0,
    rows: [MAX_ROWS]TableRow = undefined,
    row_count: usize = 0,
    selected_row: ?usize = null,
    scroll_offset: usize = 0,
    selectable: bool = true,
    sortable: bool = false,
    sort_column: ?usize = null,
    sort_ascending: bool = true,

    pub fn init() TableData {
        return .{};
    }

    pub fn deinit(self: *TableData) void {
        _ = self;
        // No-op for fixed buffers
    }

    pub fn addColumn(self: *TableData, header: []const u8, width: u16) !void {
        if (self.column_count >= MAX_COLUMNS) return error.OutOfMemory;
        self.columns[self.column_count] = .{
            .header = header,
            .width = width,
        };
        self.column_count += 1;
    }

    pub fn addColumnFull(self: *TableData, column: TableColumn) !void {
        if (self.column_count >= MAX_COLUMNS) return error.OutOfMemory;
        self.columns[self.column_count] = column;
        self.column_count += 1;
    }

    pub fn addRow(self: *TableData, cells: []const []const u8) !void {
        if (self.row_count >= MAX_ROWS) return error.OutOfMemory;
        var row = TableRow.init();
        for (cells) |cell| {
            try row.addCell(cell);
        }
        self.rows[self.row_count] = row;
        self.row_count += 1;
    }

    pub fn addRowFull(self: *TableData, row: TableRow) !void {
        if (self.row_count >= MAX_ROWS) return error.OutOfMemory;
        self.rows[self.row_count] = row;
        self.row_count += 1;
    }

    pub fn clearRows(self: *TableData) void {
        self.row_count = 0;
        self.selected_row = null;
        self.scroll_offset = 0;
    }

    pub fn getColumns(self: *const TableData) []const TableColumn {
        return self.columns[0..self.column_count];
    }

    pub fn getRows(self: *const TableData) []const TableRow {
        return self.rows[0..self.row_count];
    }

    pub fn selectNext(self: *TableData) void {
        if (self.row_count == 0) return;
        if (self.selected_row) |sel| {
            if (sel < self.row_count - 1) {
                self.selected_row = sel + 1;
            }
        } else {
            self.selected_row = 0;
        }
    }

    pub fn selectPrev(self: *TableData) void {
        if (self.row_count == 0) return;
        if (self.selected_row) |sel| {
            if (sel > 0) {
                self.selected_row = sel - 1;
            }
        } else {
            self.selected_row = self.row_count - 1;
        }
    }

    pub fn getSelectedRow(self: *const TableData) ?*const TableRow {
        if (self.selected_row) |idx| {
            if (idx < self.row_count) {
                return &self.rows[idx];
            }
        }
        return null;
    }
};

pub const TableColumn = struct {
    header: []const u8,
    width: u16,
    alignment: Alignment = .left,
    field: []const u8 = "",
};

/// Maximum cells per row
pub const MAX_CELLS = 16;

pub const TableRow = struct {
    cells: [MAX_CELLS][]const u8 = undefined,
    cell_count: usize = 0,
    data: ?*anyopaque = null, // User data pointer

    pub fn init() TableRow {
        return .{};
    }

    pub fn deinit(self: *TableRow) void {
        _ = self;
        // No-op for fixed buffer
    }

    pub fn addCell(self: *TableRow, value: []const u8) !void {
        if (self.cell_count >= MAX_CELLS) return error.OutOfMemory;
        self.cells[self.cell_count] = value;
        self.cell_count += 1;
    }

    pub fn getCells(self: *const TableRow) []const []const u8 {
        return self.cells[0..self.cell_count];
    }
};

/// Window widget data
pub const WindowData = struct {
    title: []const u8,
    border: bool = true,
    border_style: tui_root.BorderStyle = .single,
    modal: bool = false,
    closable: bool = true,
};

/// Panel widget data
pub const PanelData = struct {
    title: []const u8 = "",
    border: bool = true,
    collapsible: bool = false,
    collapsed: bool = false,
};

/// Checkbox widget data
pub const CheckboxData = struct {
    label: []const u8,
    checked: bool = false,
};

/// Separator widget data
pub const SeparatorData = struct {
    label: []const u8 = "",
    style: SeparatorStyle = .single,

    pub const SeparatorStyle = enum {
        single,
        double,
        dashed,
    };
};

// ============================================================================
// Widget Factory Functions
// ============================================================================

/// Create a label widget
pub fn createLabel(allocator: std.mem.Allocator, id: []const u8, text: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .label);
    widget.data = .{ .label = .{ .text = text } };
    widget.rect.height = 1;
    widget.rect.width = @intCast(text.len);
    return widget;
}

/// Create an input widget
pub fn createInput(allocator: std.mem.Allocator, id: []const u8, width: u16) !*Widget {
    const widget = try Widget.init(allocator, id, .input);
    widget.data = .{ .input = InputData.init() };
    widget.rect.height = 1;
    widget.rect.width = width;
    return widget;
}

/// Create a button widget
pub fn createButton(allocator: std.mem.Allocator, id: []const u8, label: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .button);
    widget.data = .{ .button = .{ .label = label } };
    widget.rect.height = 1;
    widget.rect.width = @intCast(label.len + 4); // [ label ]
    return widget;
}

/// Create a table widget
pub fn createTable(allocator: std.mem.Allocator, id: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .table);
    widget.data = .{ .table = TableData.init() };
    return widget;
}

/// Create a window widget
pub fn createWindow(allocator: std.mem.Allocator, id: []const u8, title: []const u8, width: u16, height: u16) !*Widget {
    const widget = try Widget.init(allocator, id, .window);
    widget.data = .{ .window = .{ .title = title } };
    widget.rect.width = width;
    widget.rect.height = height;
    return widget;
}

/// Create a panel widget
pub fn createPanel(allocator: std.mem.Allocator, id: []const u8, title: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .panel);
    widget.data = .{ .panel = .{ .title = title } };
    return widget;
}

/// Create a row container
pub fn createRow(allocator: std.mem.Allocator, id: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .row);
    widget.rect.height = 1;
    return widget;
}

/// Create a column container
pub fn createColumn(allocator: std.mem.Allocator, id: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .column);
    return widget;
}

/// Create a separator widget
pub fn createSeparator(allocator: std.mem.Allocator, id: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .separator);
    widget.data = .{ .separator = .{} };
    widget.rect.height = 1;
    return widget;
}

/// Create a checkbox widget
pub fn createCheckbox(allocator: std.mem.Allocator, id: []const u8, label: []const u8) !*Widget {
    const widget = try Widget.init(allocator, id, .checkbox);
    widget.data = .{ .checkbox = .{ .label = label } };
    widget.rect.height = 1;
    widget.rect.width = @intCast(label.len + 4); // [x] label
    return widget;
}
