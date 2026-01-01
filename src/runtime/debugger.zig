//! Cot Debugger Support
//!
//! Provides debugging capabilities for the Cot VM including:
//! - Breakpoints (by line number or code offset)
//! - Step execution (step over, step into, step out)
//! - Variable inspection
//! - Call stack inspection

const std = @import("std");
const Module = @import("bytecode/module.zig").Module;

/// Debug event types sent to the debug adapter
pub const DebugEvent = union(enum) {
    /// VM stopped at a breakpoint
    breakpoint: struct {
        line: u32,
        file: ?[]const u8,
    },
    /// VM stopped after a step
    step: struct {
        line: u32,
        file: ?[]const u8,
    },
    /// VM execution completed
    terminated: void,
    /// Output from the program
    output: []const u8,
    /// An error occurred
    error_event: []const u8,
};

/// Debug commands from the debug adapter
pub const DebugCommand = union(enum) {
    /// Continue execution
    @"continue": void,
    /// Step to next line (step over)
    step_over: void,
    /// Step into function call
    step_into: void,
    /// Step out of current function
    step_out: void,
    /// Pause execution
    pause: void,
    /// Set a breakpoint
    set_breakpoint: struct {
        file: ?[]const u8,
        line: u32,
    },
    /// Remove a breakpoint
    remove_breakpoint: struct {
        file: ?[]const u8,
        line: u32,
    },
    /// Get local variables
    get_locals: void,
    /// Get call stack
    get_stack: void,
    /// Evaluate an expression
    evaluate: []const u8,
    /// Disconnect debugger
    disconnect: void,
};

/// Breakpoint definition
pub const Breakpoint = struct {
    id: u32,
    file: ?[]const u8,
    line: u32,
    enabled: bool,
    hit_count: u32,
};

/// Variable information for inspection
pub const VariableInfo = struct {
    name: []const u8,
    value: []const u8,
    var_type: []const u8,
    scope: enum { local, global, parameter },
};

/// Stack frame information
pub const StackFrameInfo = struct {
    id: u32,
    name: []const u8,
    file: ?[]const u8,
    line: u32,
    column: u32,
};

/// Step mode for execution
pub const StepMode = enum {
    /// Run until breakpoint or end
    run,
    /// Step to next line (skip function calls)
    step_over,
    /// Step to next line (enter function calls)
    step_into,
    /// Step until current function returns
    step_out,
    /// Paused, waiting for command
    paused,
};

/// Debugger state attached to the VM
pub const Debugger = struct {
    allocator: std.mem.Allocator,

    /// Whether debug mode is enabled
    enabled: bool,

    /// Current step mode
    step_mode: StepMode,

    /// Current source line (from debug_line opcodes)
    current_line: u32,

    /// Previous source line (for step over detection)
    previous_line: u32,

    /// Call depth when step_out started
    step_out_depth: usize,

    /// Breakpoints by line number
    breakpoints: std.AutoHashMap(u32, Breakpoint),

    /// Next breakpoint ID
    next_breakpoint_id: u32,

    /// Event callback (if set, called when debug events occur)
    event_callback: ?*const fn (DebugEvent) void,

    /// Whether we're waiting for a debug command
    waiting_for_command: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .enabled = false,
            .step_mode = .run,
            .current_line = 0,
            .previous_line = 0,
            .step_out_depth = 0,
            .breakpoints = std.AutoHashMap(u32, Breakpoint).init(allocator),
            .next_breakpoint_id = 1,
            .event_callback = null,
            .waiting_for_command = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.breakpoints.deinit();
    }

    /// Enable debug mode
    pub fn enable(self: *Self) void {
        self.enabled = true;
        self.step_mode = .paused; // Start paused
    }

    /// Set a breakpoint at a line number
    pub fn setBreakpoint(self: *Self, line: u32) u32 {
        const id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        self.breakpoints.put(line, .{
            .id = id,
            .file = null,
            .line = line,
            .enabled = true,
            .hit_count = 0,
        }) catch return 0;

        return id;
    }

    /// Remove a breakpoint at a line number
    pub fn removeBreakpoint(self: *Self, line: u32) bool {
        return self.breakpoints.remove(line);
    }

    /// Check if we should stop at the current line
    pub fn shouldStop(self: *Self, line: u32, call_depth: usize) bool {
        if (!self.enabled) return false;

        // Update line tracking
        self.previous_line = self.current_line;
        self.current_line = line;

        // Check breakpoints
        if (self.breakpoints.get(line)) |*bp| {
            if (bp.enabled) {
                // Update hit count (need to use getPtr for mutation)
                if (self.breakpoints.getPtr(line)) |bp_ptr| {
                    bp_ptr.hit_count += 1;
                }
                self.step_mode = .paused;
                return true;
            }
        }

        // Check step mode
        switch (self.step_mode) {
            .run => return false,
            .paused => return true,
            .step_into => {
                // Stop at every new line
                if (line != self.previous_line) {
                    self.step_mode = .paused;
                    return true;
                }
                return false;
            },
            .step_over => {
                // Stop at next line at same or lower call depth
                if (line != self.previous_line and call_depth <= self.step_out_depth) {
                    self.step_mode = .paused;
                    return true;
                }
                return false;
            },
            .step_out => {
                // Stop when call depth is lower than when we started
                if (call_depth < self.step_out_depth) {
                    self.step_mode = .paused;
                    return true;
                }
                return false;
            },
        }
    }

    /// Continue execution
    pub fn continue_(self: *Self) void {
        self.step_mode = .run;
        self.waiting_for_command = false;
    }

    /// Step over (next line, skip function calls)
    pub fn stepOver(self: *Self, current_call_depth: usize) void {
        self.step_mode = .step_over;
        self.step_out_depth = current_call_depth;
        self.waiting_for_command = false;
    }

    /// Step into (next line, enter function calls)
    pub fn stepInto(self: *Self) void {
        self.step_mode = .step_into;
        self.waiting_for_command = false;
    }

    /// Step out (continue until function returns)
    pub fn stepOut(self: *Self, current_call_depth: usize) void {
        self.step_mode = .step_out;
        self.step_out_depth = current_call_depth;
        self.waiting_for_command = false;
    }

    /// Pause execution
    pub fn pause(self: *Self) void {
        self.step_mode = .paused;
    }

    /// Send an event to the debug adapter
    pub fn sendEvent(self: *Self, event: DebugEvent) void {
        if (self.event_callback) |callback| {
            callback(event);
        }
    }
};

/// Debug protocol message (JSON format for communication with debug adapter)
pub const DebugMessage = struct {
    seq: u32,
    type: []const u8, // "request", "response", "event"
    command: ?[]const u8,
    body: ?std.json.Value,
};

/// Parse a debug command from JSON
pub fn parseCommand(json_str: []const u8, allocator: std.mem.Allocator) !DebugCommand {
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, json_str, .{});
    defer parsed.deinit();

    const root = parsed.value;
    const cmd = root.object.get("command") orelse return error.InvalidCommand;
    const cmd_str = cmd.string;

    if (std.mem.eql(u8, cmd_str, "continue")) {
        return .{ .@"continue" = {} };
    } else if (std.mem.eql(u8, cmd_str, "stepOver") or std.mem.eql(u8, cmd_str, "next")) {
        return .{ .step_over = {} };
    } else if (std.mem.eql(u8, cmd_str, "stepIn") or std.mem.eql(u8, cmd_str, "stepInto")) {
        return .{ .step_into = {} };
    } else if (std.mem.eql(u8, cmd_str, "stepOut")) {
        return .{ .step_out = {} };
    } else if (std.mem.eql(u8, cmd_str, "pause")) {
        return .{ .pause = {} };
    } else if (std.mem.eql(u8, cmd_str, "setBreakpoint")) {
        const args = root.object.get("arguments") orelse return error.InvalidCommand;
        const line = args.object.get("line") orelse return error.InvalidCommand;
        return .{ .set_breakpoint = .{
            .file = null,
            .line = @intCast(line.integer),
        } };
    } else if (std.mem.eql(u8, cmd_str, "disconnect")) {
        return .{ .disconnect = {} };
    }

    return error.UnknownCommand;
}

/// Format a debug event as JSON
pub fn formatEvent(event: DebugEvent, allocator: std.mem.Allocator) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    const writer = buffer.writer();

    switch (event) {
        .breakpoint => |bp| {
            try writer.print(
                \\{{"type":"event","event":"stopped","body":{{"reason":"breakpoint","line":{}}}}}
            , .{bp.line});
        },
        .step => |s| {
            try writer.print(
                \\{{"type":"event","event":"stopped","body":{{"reason":"step","line":{}}}}}
            , .{s.line});
        },
        .terminated => {
            try writer.writeAll(
                \\{"type":"event","event":"terminated"}
            );
        },
        .output => |text| {
            try writer.print(
                \\{{"type":"event","event":"output","body":{{"output":"{s}"}}}}
            , .{text});
        },
        .error_event => |msg| {
            try writer.print(
                \\{{"type":"event","event":"error","body":{{"message":"{s}"}}}}
            , .{msg});
        },
    }

    return buffer.toOwnedSlice();
}

test "debugger breakpoint" {
    const allocator = std.testing.allocator;
    var dbg = Debugger.init(allocator);
    defer dbg.deinit();

    dbg.enable();
    const id = dbg.setBreakpoint(10);
    try std.testing.expect(id > 0);

    // Should stop at line 10
    try std.testing.expect(dbg.shouldStop(10, 0));
}
