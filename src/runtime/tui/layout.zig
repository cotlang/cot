//! Layout engine for Cot TUI
//! Supports flex (row/column), grid, and absolute positioning.

const std = @import("std");
const Widget = @import("widgets.zig").Widget;
const WidgetType = @import("widgets.zig").WidgetType;
const Rect = @import("tui.zig").Rect;

/// Layout mode for containers
pub const LayoutMode = enum {
    flex_row,
    flex_column,
    grid,
    absolute,
};

/// Layout constraints for a widget
pub const LayoutConstraints = struct {
    min_width: ?u16 = null,
    max_width: ?u16 = null,
    min_height: ?u16 = null,
    max_height: ?u16 = null,
    preferred_width: ?u16 = null,
    preferred_height: ?u16 = null,
    flex_grow: u8 = 0, // How much to grow in flex layout
    flex_shrink: u8 = 1, // How much to shrink in flex layout
};

/// Spacing/padding configuration
pub const Spacing = struct {
    top: u16 = 0,
    right: u16 = 0,
    bottom: u16 = 0,
    left: u16 = 0,

    pub fn all(value: u16) Spacing {
        return .{ .top = value, .right = value, .bottom = value, .left = value };
    }

    pub fn horizontal(value: u16) Spacing {
        return .{ .left = value, .right = value };
    }

    pub fn vertical(value: u16) Spacing {
        return .{ .top = value, .bottom = value };
    }
};

/// Layout context passed during layout computation
pub const LayoutContext = struct {
    available_width: u16,
    available_height: u16,
    x_offset: u16,
    y_offset: u16,
};

/// Compute layout for a widget tree
pub fn computeLayout(widget: *Widget, ctx: LayoutContext) void {
    // Set widget position based on context
    widget.rect.x = ctx.x_offset;
    widget.rect.y = ctx.y_offset;

    // Handle different widget types
    switch (widget.widget_type) {
        .window => layoutWindow(widget, ctx),
        .panel => layoutPanel(widget, ctx),
        .row => layoutRow(widget, ctx),
        .column => layoutColumn(widget, ctx),
        .grid => layoutGrid(widget, ctx),
        else => layoutLeaf(widget, ctx),
    }
}

/// Layout a window (container with border/title)
fn layoutWindow(widget: *Widget, ctx: LayoutContext) void {
    const window_data = widget.data.window;

    // Window uses available space or its own defined size
    if (widget.rect.width == 0) widget.rect.width = ctx.available_width;
    if (widget.rect.height == 0) widget.rect.height = ctx.available_height;

    // Content area (inside border)
    const border_offset: u16 = if (window_data.border) 1 else 0;
    const title_offset: u16 = if (window_data.border and window_data.title.len > 0) 0 else 0;

    const content_x = widget.rect.x + border_offset;
    const content_y = widget.rect.y + border_offset + title_offset;
    const content_width = widget.rect.width -| (border_offset * 2);
    const content_height = widget.rect.height -| (border_offset * 2) -| title_offset;

    // Layout children as column by default
    var y_cursor: u16 = 0;
    for (widget.children.items) |child| {
        computeLayout(child, .{
            .available_width = content_width,
            .available_height = content_height -| y_cursor,
            .x_offset = content_x,
            .y_offset = content_y + y_cursor,
        });
        y_cursor += child.rect.height;
    }
}

/// Layout a panel (similar to window but simpler)
fn layoutPanel(widget: *Widget, ctx: LayoutContext) void {
    const panel_data = widget.data.panel;

    widget.rect.width = ctx.available_width;

    const border_offset: u16 = if (panel_data.border) 1 else 0;
    const content_x = widget.rect.x + border_offset;
    const content_y = widget.rect.y + border_offset;
    const content_width = ctx.available_width -| (border_offset * 2);

    // Calculate height from children
    var total_height: u16 = 0;
    for (widget.children.items) |child| {
        computeLayout(child, .{
            .available_width = content_width,
            .available_height = ctx.available_height -| total_height -| (border_offset * 2),
            .x_offset = content_x,
            .y_offset = content_y + total_height,
        });
        total_height += child.rect.height;
    }

    widget.rect.height = total_height + (border_offset * 2);
}

/// Layout a row (horizontal flex container)
fn layoutRow(widget: *Widget, ctx: LayoutContext) void {
    widget.rect.width = ctx.available_width;
    widget.rect.height = 1; // Default row height

    if (widget.children.items.len == 0) return;

    // Calculate total fixed width and flex items
    var fixed_width: u16 = 0;
    var flex_count: u16 = 0;

    for (widget.children.items) |child| {
        if (child.rect.width > 0) {
            fixed_width += child.rect.width;
        } else {
            flex_count += 1;
        }
    }

    // Calculate flex item width
    const remaining = ctx.available_width -| fixed_width;
    const flex_width: u16 = if (flex_count > 0) remaining / flex_count else 0;

    // Position children
    var x_cursor: u16 = 0;
    var max_height: u16 = 1;

    for (widget.children.items) |child| {
        const child_width = if (child.rect.width > 0) child.rect.width else flex_width;

        computeLayout(child, .{
            .available_width = child_width,
            .available_height = ctx.available_height,
            .x_offset = widget.rect.x + x_cursor,
            .y_offset = widget.rect.y,
        });

        x_cursor += child.rect.width;
        if (child.rect.height > max_height) max_height = child.rect.height;
    }

    widget.rect.height = max_height;
}

/// Layout a column (vertical flex container)
fn layoutColumn(widget: *Widget, ctx: LayoutContext) void {
    widget.rect.width = ctx.available_width;

    if (widget.children.items.len == 0) {
        widget.rect.height = 0;
        return;
    }

    // Calculate total fixed height and flex items
    var fixed_height: u16 = 0;
    var flex_count: u16 = 0;

    for (widget.children.items) |child| {
        if (child.rect.height > 0) {
            fixed_height += child.rect.height;
        } else {
            flex_count += 1;
        }
    }

    // Calculate flex item height
    const remaining = ctx.available_height -| fixed_height;
    const flex_height: u16 = if (flex_count > 0) remaining / flex_count else 0;

    // Position children
    var y_cursor: u16 = 0;

    for (widget.children.items) |child| {
        const child_height = if (child.rect.height > 0) child.rect.height else flex_height;

        computeLayout(child, .{
            .available_width = ctx.available_width,
            .available_height = child_height,
            .x_offset = widget.rect.x,
            .y_offset = widget.rect.y + y_cursor,
        });

        y_cursor += child.rect.height;
    }

    widget.rect.height = y_cursor;
}

/// Layout a grid container
fn layoutGrid(widget: *Widget, ctx: LayoutContext) void {
    // Grid layout - children are placed based on their position
    // For now, simple auto-grid based on child order
    widget.rect.width = ctx.available_width;

    const cols: u16 = 2; // Default to 2 columns
    const col_width = ctx.available_width / cols;

    var row: u16 = 0;
    var col: u16 = 0;
    var row_height: u16 = 1;
    var total_height: u16 = 0;

    for (widget.children.items) |child| {
        computeLayout(child, .{
            .available_width = col_width,
            .available_height = ctx.available_height -| total_height,
            .x_offset = widget.rect.x + (col * col_width),
            .y_offset = widget.rect.y + total_height,
        });

        if (child.rect.height > row_height) row_height = child.rect.height;

        col += 1;
        if (col >= cols) {
            col = 0;
            row += 1;
            total_height += row_height;
            row_height = 1;
        }
    }

    if (col > 0) total_height += row_height; // Last partial row
    widget.rect.height = total_height;
}

/// Layout a leaf widget (no children)
fn layoutLeaf(widget: *Widget, ctx: LayoutContext) void {
    // Leaf widgets use their own defined size or available space

    // For inputs, ensure minimum width
    if (widget.widget_type == .input) {
        if (widget.rect.width == 0) widget.rect.width = 20;
        widget.rect.height = 1;
    }

    // For buttons, calculate based on label
    if (widget.widget_type == .button) {
        const button_data = widget.data.button;
        widget.rect.width = @intCast(button_data.label.len + 4);
        widget.rect.height = 1;
    }

    // For tables, use available space
    if (widget.widget_type == .table) {
        if (widget.rect.width == 0) widget.rect.width = ctx.available_width;
        if (widget.rect.height == 0) widget.rect.height = ctx.available_height;
    }

    // Labels use text length
    if (widget.widget_type == .label) {
        const label_data = widget.data.label;
        if (widget.rect.width == 0) widget.rect.width = @intCast(label_data.text.len);
        widget.rect.height = 1;
    }

    // Separators use full width
    if (widget.widget_type == .separator) {
        widget.rect.width = ctx.available_width;
        widget.rect.height = 1;
    }
}

/// Center a widget within available space
pub fn center(widget: *Widget, available_width: u16, available_height: u16) void {
    if (widget.rect.width < available_width) {
        widget.rect.x = (available_width - widget.rect.width) / 2;
    }
    if (widget.rect.height < available_height) {
        widget.rect.y = (available_height - widget.rect.height) / 2;
    }
}
