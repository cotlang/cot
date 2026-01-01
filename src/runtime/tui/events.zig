//! Event system for Cot TUI
//! Handles keyboard, mouse, and widget events.

const std = @import("std");
const Widget = @import("widgets.zig").Widget;

/// Event types
pub const EventType = enum {
    // Keyboard events
    key_press,
    key_release,

    // Mouse events (if supported)
    mouse_click,
    mouse_move,
    mouse_scroll,

    // Widget events
    focus,
    blur,
    change,
    submit,
    click,

    // Screen events
    resize,
    close,

    // Navigation
    navigate,
};

/// An event that can be dispatched to handlers
pub const Event = struct {
    event_type: EventType,
    target: ?*Widget = null,
    data: EventData = .{ .none = {} },
    handled: bool = false,
    propagate: bool = true, // Continue bubbling to parent

    pub fn stopPropagation(self: *Event) void {
        self.propagate = false;
    }

    pub fn preventDefault(self: *Event) void {
        self.handled = true;
    }
};

/// Event-specific data
pub const EventData = union(enum) {
    none: void,
    key: KeyData,
    mouse: MouseData,
    change: ChangeData,
    navigate: NavigateData,
    resize: ResizeData,
};

/// Keyboard event data
pub const KeyData = struct {
    key: Key,
    modifiers: Modifiers = .{},

    pub const Key = union(enum) {
        char: u8,
        function: u8, // F1-F12
        special: SpecialKey,
    };

    pub const SpecialKey = enum {
        enter,
        escape,
        tab,
        backspace,
        delete,
        up,
        down,
        left,
        right,
        home,
        end,
        page_up,
        page_down,
        insert,
    };

    pub const Modifiers = struct {
        ctrl: bool = false,
        alt: bool = false,
        shift: bool = false,
    };
};

/// Mouse event data
pub const MouseData = struct {
    x: u16,
    y: u16,
    button: MouseButton = .left,

    pub const MouseButton = enum {
        left,
        right,
        middle,
        scroll_up,
        scroll_down,
    };
};

/// Change event data (for inputs)
pub const ChangeData = struct {
    old_value: []const u8,
    new_value: []const u8,
};

/// Navigation event data
pub const NavigateData = struct {
    target_screen: []const u8,
    params: ?std.StringHashMap([]const u8) = null,
    modal: bool = false,
    replace: bool = false,
};

/// Resize event data
pub const ResizeData = struct {
    width: u16,
    height: u16,
};

/// Event handler function type
pub const EventHandler = *const fn (*Event, ?*anyopaque) void;

/// Maximum event handlers
pub const MAX_HANDLERS = 64;

/// Handler entry for event dispatcher
pub const HandlerEntry = struct {
    handler: EventHandler,
    context: ?*anyopaque,
    event_type: ?EventType, // null means handle all events
    id: usize = 0,
    active: bool = false,
};

/// Event dispatcher for managing handlers
pub const EventDispatcher = struct {
    handlers: [MAX_HANDLERS]HandlerEntry = undefined,
    handler_count: usize = 0,
    next_id: usize = 0,

    pub fn init() EventDispatcher {
        return .{};
    }

    pub fn deinit(self: *EventDispatcher) void {
        _ = self;
        // No-op for fixed buffer
    }

    /// Register an event handler, returns handler ID
    pub fn on(self: *EventDispatcher, event_type: ?EventType, handler: EventHandler, context: ?*anyopaque) !usize {
        if (self.handler_count >= MAX_HANDLERS) return error.OutOfMemory;

        const id = self.next_id;
        self.next_id += 1;

        self.handlers[self.handler_count] = .{
            .handler = handler,
            .context = context,
            .event_type = event_type,
            .id = id,
            .active = true,
        };
        self.handler_count += 1;

        return id;
    }

    /// Remove an event handler
    pub fn off(self: *EventDispatcher, id: usize) void {
        for (self.handlers[0..self.handler_count]) |*entry| {
            if (entry.id == id) {
                entry.active = false;
                break;
            }
        }
    }

    /// Dispatch an event to all matching handlers
    pub fn dispatch(self: *EventDispatcher, event: *Event) void {
        for (self.handlers[0..self.handler_count]) |entry| {
            if (!entry.active) continue;

            // Check if handler matches event type
            if (entry.event_type) |et| {
                if (et != event.event_type) continue;
            }

            // Call handler
            entry.handler(event, entry.context);

            // Stop if propagation was stopped
            if (!event.propagate) break;
        }
    }
};

/// Maximum key bindings
pub const MAX_BINDINGS = 64;

/// Key binding entry
pub const KeyBinding = struct {
    key: KeyData.Key,
    modifiers: KeyData.Modifiers = .{},
    handler_id: usize,
    description: []const u8 = "",
};

/// Key binding manager
pub const KeyBindingManager = struct {
    bindings: [MAX_BINDINGS]KeyBinding = undefined,
    binding_count: usize = 0,

    pub fn init() KeyBindingManager {
        return .{};
    }

    pub fn deinit(self: *KeyBindingManager) void {
        _ = self;
        // No-op for fixed buffer
    }

    pub fn bind(self: *KeyBindingManager, key: KeyData.Key, modifiers: KeyData.Modifiers, handler_id: usize, description: []const u8) !void {
        if (self.binding_count >= MAX_BINDINGS) return error.OutOfMemory;
        self.bindings[self.binding_count] = .{
            .key = key,
            .modifiers = modifiers,
            .handler_id = handler_id,
            .description = description,
        };
        self.binding_count += 1;
    }

    pub fn findBinding(self: *const KeyBindingManager, key: KeyData.Key, modifiers: KeyData.Modifiers) ?usize {
        for (self.bindings[0..self.binding_count]) |binding| {
            const key_match = switch (key) {
                .char => |c| switch (binding.key) {
                    .char => |bc| c == bc,
                    else => false,
                },
                .function => |f| switch (binding.key) {
                    .function => |bf| f == bf,
                    else => false,
                },
                .special => |s| switch (binding.key) {
                    .special => |bs| s == bs,
                    else => false,
                },
            };

            if (key_match and
                binding.modifiers.ctrl == modifiers.ctrl and
                binding.modifiers.alt == modifiers.alt and
                binding.modifiers.shift == modifiers.shift)
            {
                return binding.handler_id;
            }
        }
        return null;
    }
};
