//! Screen definition and management for Cot TUI
//! Screens are the top-level containers for UI, discovered from screens/ directory.

const std = @import("std");
const Widget = @import("widgets.zig").Widget;
const WidgetType = @import("widgets.zig").WidgetType;
const Event = @import("events.zig").Event;
const EventType = @import("events.zig").EventType;
const EventDispatcher = @import("events.zig").EventDispatcher;
const KeyBindingManager = @import("events.zig").KeyBindingManager;
const KeyData = @import("events.zig").KeyData;
const Layout = @import("layout.zig");

/// Screen lifecycle state
pub const ScreenState = enum {
    created,
    initializing,
    active,
    paused, // When another screen is on top
    closing,
    destroyed,
};

/// Maximum focusable widgets per screen
pub const MAX_FOCUSABLE = 64;

/// Fixed-size widget pointer list
pub const FocusableList = struct {
    items: [MAX_FOCUSABLE]*Widget = undefined,
    len: usize = 0,

    pub fn append(self: *FocusableList, widget: *Widget) void {
        if (self.len < MAX_FOCUSABLE) {
            self.items[self.len] = widget;
            self.len += 1;
        }
    }

    pub fn clear(self: *FocusableList) void {
        self.len = 0;
    }

    pub fn slice(self: *const FocusableList) []*Widget {
        return @constCast(self.items[0..self.len]);
    }
};

/// A screen is the top-level UI container
pub const Screen = struct {
    name: []const u8,
    title: []const u8,
    state: ScreenState = .created,
    root: ?*Widget = null,
    focused_widget: ?*Widget = null,
    focusable_widgets: FocusableList = .{},
    focus_index: usize = 0,

    // Event handling
    event_dispatcher: EventDispatcher,
    key_bindings: KeyBindingManager,

    // Lifecycle handlers (indices into handler table)
    on_init_handler: ?usize = null,
    on_focus_handler: ?usize = null,
    on_close_handler: ?usize = null,

    // Navigation
    params: std.StringHashMap([]const u8),
    return_handler: ?*const fn ([]const u8) void = null,

    // State management
    is_dirty: bool = true, // Needs re-render
    prevent_close: bool = false,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) !*Screen {
        const screen = try allocator.create(Screen);
        screen.* = .{
            .name = name,
            .title = name,
            .focusable_widgets = .{},
            .event_dispatcher = EventDispatcher.init(),
            .key_bindings = KeyBindingManager.init(),
            .params = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
        return screen;
    }

    pub fn deinit(self: *Screen) void {
        if (self.root) |root| {
            root.deinit();
        }
        self.event_dispatcher.deinit();
        self.key_bindings.deinit();
        self.params.deinit();
        self.allocator.destroy(self);
    }

    /// Set the root widget
    pub fn setRoot(self: *Screen, widget: *Widget) void {
        self.root = widget;
        self.is_dirty = true;
        self.collectFocusableWidgets();
    }

    /// Collect all focusable widgets in tree order
    fn collectFocusableWidgets(self: *Screen) void {
        self.focusable_widgets.clear();
        if (self.root) |root| {
            self.collectFocusableFromWidget(root);
        }
    }

    fn collectFocusableFromWidget(self: *Screen, widget: *Widget) void {
        // Check if widget is focusable
        if (isFocusable(widget)) {
            self.focusable_widgets.append(widget);
        }

        // Recurse to children
        for (widget.children.slice()) |child| {
            self.collectFocusableFromWidget(child);
        }
    }

    fn isFocusable(widget: *Widget) bool {
        if (!widget.visible or !widget.enabled) return false;

        return switch (widget.widget_type) {
            .input, .textarea, .number, .checkbox, .radio, .select, .button, .table => true,
            else => false,
        };
    }

    /// Focus next focusable widget
    pub fn focusNext(self: *Screen) void {
        if (self.focusable_widgets.len == 0) return;

        if (self.focused_widget) |fw| {
            fw.blur();
        }

        self.focus_index = (self.focus_index + 1) % self.focusable_widgets.len;
        self.focused_widget = self.focusable_widgets.items[self.focus_index];
        if (self.focused_widget) |fw| {
            fw.focus();
        }
        self.is_dirty = true;
    }

    /// Focus previous focusable widget
    pub fn focusPrev(self: *Screen) void {
        if (self.focusable_widgets.len == 0) return;

        if (self.focused_widget) |fw| {
            fw.blur();
        }

        if (self.focus_index == 0) {
            self.focus_index = self.focusable_widgets.len - 1;
        } else {
            self.focus_index -= 1;
        }

        self.focused_widget = self.focusable_widgets.items[self.focus_index];
        if (self.focused_widget) |fw| {
            fw.focus();
        }
        self.is_dirty = true;
    }

    /// Focus a specific widget
    pub fn focusWidget(self: *Screen, widget: *Widget) void {
        if (self.focused_widget) |fw| {
            fw.blur();
        }

        // Find widget in focusable list
        for (self.focusable_widgets.slice(), 0..) |fw, i| {
            if (fw == widget) {
                self.focus_index = i;
                break;
            }
        }

        self.focused_widget = widget;
        widget.focus();
        self.is_dirty = true;
    }

    /// Focus first focusable widget
    pub fn focusFirst(self: *Screen) void {
        if (self.focusable_widgets.len == 0) return;

        if (self.focused_widget) |fw| {
            fw.blur();
        }

        self.focus_index = 0;
        self.focused_widget = self.focusable_widgets.items[0];
        if (self.focused_widget) |fw| {
            fw.focus();
        }
        self.is_dirty = true;
    }

    /// Bind a key to a handler
    pub fn bindKey(self: *Screen, key: KeyData.Key, modifiers: KeyData.Modifiers, handler: *const fn (*Event, ?*anyopaque) void, context: ?*anyopaque, description: []const u8) !void {
        const handler_id = try self.event_dispatcher.on(.key_press, handler, context);
        try self.key_bindings.bind(key, modifiers, handler_id, description);
    }

    /// Handle keyboard input
    pub fn handleKey(self: *Screen, key: KeyData.Key, modifiers: KeyData.Modifiers) void {
        // Check global key bindings first
        if (self.key_bindings.findBinding(key, modifiers)) |_| {
            var event = Event{
                .event_type = .key_press,
                .target = self.focused_widget,
                .data = .{ .key = .{ .key = key, .modifiers = modifiers } },
            };
            self.event_dispatcher.dispatch(&event);
            if (event.handled) return;
        }

        // Handle built-in navigation
        switch (key) {
            .special => |s| switch (s) {
                .tab => {
                    if (modifiers.shift) {
                        self.focusPrev();
                    } else {
                        self.focusNext();
                    }
                    return;
                },
                else => {},
            },
            else => {},
        }

        // Pass to focused widget
        if (self.focused_widget) |fw| {
            self.handleWidgetKey(fw, key, modifiers);
        }
    }

    /// Handle key input for a specific widget
    fn handleWidgetKey(self: *Screen, widget: *Widget, key: KeyData.Key, modifiers: KeyData.Modifiers) void {
        _ = modifiers;

        switch (widget.widget_type) {
            .input => {
                var input_data = &widget.data.input;
                switch (key) {
                    .char => |c| {
                        input_data.insertChar(c) catch {};
                        self.is_dirty = true;
                    },
                    .special => |s| switch (s) {
                        .backspace => {
                            input_data.deleteChar();
                            self.is_dirty = true;
                        },
                        .left => {
                            input_data.moveCursor(-1);
                            self.is_dirty = true;
                        },
                        .right => {
                            input_data.moveCursor(1);
                            self.is_dirty = true;
                        },
                        .home => {
                            input_data.cursor_pos = 0;
                            self.is_dirty = true;
                        },
                        .end => {
                            input_data.cursor_pos = input_data.len;
                            self.is_dirty = true;
                        },
                        .enter => {
                            // Trigger on_enter handler or focus next
                            self.focusNext();
                        },
                        else => {},
                    },
                    else => {},
                }
            },
            .button => {
                switch (key) {
                    .special => |s| switch (s) {
                        .enter => {
                            // Trigger button click
                            var event = Event{
                                .event_type = .click,
                                .target = widget,
                            };
                            self.event_dispatcher.dispatch(&event);
                        },
                        else => {},
                    },
                    .char => |c| {
                        if (c == ' ') {
                            var event = Event{
                                .event_type = .click,
                                .target = widget,
                            };
                            self.event_dispatcher.dispatch(&event);
                        }
                    },
                    else => {},
                }
            },
            .table => {
                var table_data = &widget.data.table;
                switch (key) {
                    .special => |s| switch (s) {
                        .up => {
                            table_data.selectPrev();
                            self.is_dirty = true;
                        },
                        .down => {
                            table_data.selectNext();
                            self.is_dirty = true;
                        },
                        .enter => {
                            // Trigger select event
                            var event = Event{
                                .event_type = .submit,
                                .target = widget,
                            };
                            self.event_dispatcher.dispatch(&event);
                        },
                        else => {},
                    },
                    else => {},
                }
            },
            .checkbox => {
                switch (key) {
                    .special => |s| switch (s) {
                        .enter => {
                            widget.data.checkbox.checked = !widget.data.checkbox.checked;
                            self.is_dirty = true;
                        },
                        else => {},
                    },
                    .char => |c| {
                        if (c == ' ') {
                            widget.data.checkbox.checked = !widget.data.checkbox.checked;
                            self.is_dirty = true;
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    /// Mark screen as needing re-render
    pub fn markDirty(self: *Screen) void {
        self.is_dirty = true;
    }

    /// Compute layout for the screen
    pub fn computeLayout(self: *Screen, width: u16, height: u16) void {
        if (self.root) |root| {
            Layout.computeLayout(root, .{
                .available_width = width,
                .available_height = height,
                .x_offset = 0,
                .y_offset = 0,
            });
        }
    }

    /// Find a widget by ID
    pub fn findWidget(self: *Screen, id: []const u8) ?*Widget {
        if (self.root) |root| {
            return findWidgetInTree(root, id);
        }
        return null;
    }

    fn findWidgetInTree(widget: *Widget, id: []const u8) ?*Widget {
        if (std.mem.eql(u8, widget.id, id)) {
            return widget;
        }

        for (widget.children.slice()) |child| {
            if (findWidgetInTree(child, id)) |found| {
                return found;
            }
        }

        return null;
    }
};

/// Maximum screen stack depth
pub const MAX_SCREEN_STACK = 16;

/// Fixed-size screen stack
pub const ScreenStack = struct {
    items: [MAX_SCREEN_STACK]*Screen = undefined,
    len: usize = 0,

    pub fn push(self: *ScreenStack, screen: *Screen) !void {
        if (self.len >= MAX_SCREEN_STACK) return error.OutOfMemory;
        self.items[self.len] = screen;
        self.len += 1;
    }

    pub fn pop(self: *ScreenStack) ?*Screen {
        if (self.len == 0) return null;
        self.len -= 1;
        return self.items[self.len];
    }

    pub fn isEmpty(self: *const ScreenStack) bool {
        return self.len == 0;
    }
};

/// Screen manager for navigation
pub const ScreenManager = struct {
    screens: std.StringHashMap(*Screen),
    screen_stack: ScreenStack = .{},
    current_screen: ?*Screen = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ScreenManager {
        return .{
            .screens = std.StringHashMap(*Screen).init(allocator),
            .screen_stack = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ScreenManager) void {
        var iter = self.screens.valueIterator();
        while (iter.next()) |screen| {
            screen.*.deinit();
        }
        self.screens.deinit();
    }

    /// Register a screen
    pub fn register(self: *ScreenManager, screen: *Screen) !void {
        try self.screens.put(screen.name, screen);
    }

    /// Navigate to a screen
    pub fn navigate(self: *ScreenManager, name: []const u8, params: ?std.StringHashMap([]const u8)) !void {
        const screen = self.screens.get(name) orelse return error.ScreenNotFound;

        // Pause current screen
        if (self.current_screen) |current| {
            current.state = .paused;
            try self.screen_stack.push(current);
        }

        // Set params
        if (params) |p| {
            var iter = p.iterator();
            while (iter.next()) |entry| {
                try screen.params.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }

        // Activate new screen
        screen.state = .active;
        self.current_screen = screen;
        screen.focusFirst();
    }

    /// Go back to previous screen
    pub fn back(self: *ScreenManager) !void {
        if (self.screen_stack.isEmpty()) return;

        // Close current screen
        if (self.current_screen) |current| {
            current.state = .closing;
            // TODO: Call on_close handler
            current.state = .destroyed;
        }

        // Pop previous screen
        if (self.screen_stack.pop()) |prev| {
            prev.state = .active;
            self.current_screen = prev;
        }
    }

    /// Get current screen
    pub fn getCurrent(self: *ScreenManager) ?*Screen {
        return self.current_screen;
    }
};
