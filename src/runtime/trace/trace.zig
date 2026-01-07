//! Execution tracing infrastructure
//!
//! Provides configurable tracing of VM execution with multiple
//! granularity levels and output formats.
//!
//! Usage:
//!   const tracer = Tracer.init(allocator, .{ .level = .opcodes });
//!   vm.setTracer(&tracer);
//!   defer tracer.deinit();
//!
//! The tracer maintains a history ring buffer for crash dumps even
//! when real-time tracing is disabled.

const std = @import("std");
const Opcode = @import("../bytecode/opcodes.zig").Opcode;
const Value = @import("../bytecode/value.zig").Value;
const History = @import("history.zig").History;
const output_mod = @import("output.zig");
const Output = output_mod.Output;
const OutputConfig = output_mod.Config;

/// Trace granularity levels (increasing verbosity)
pub const TraceLevel = enum(u8) {
    /// No tracing output (but history still recorded)
    none = 0,

    /// Routine entry/exit only - minimal overhead
    /// Output: >>> call main | <<< return main
    routines = 1,

    /// Each opcode with IP and current line
    /// Output: [main:10] 0x0004 load_const r0, #3
    opcodes = 2,

    /// Opcodes + register state after each instruction
    /// Output: [main:10] 0x0004 load_const | r0=42 r1=nil
    verbose = 3,

    /// Full state dump including stack
    full = 4,

    /// Slot-level tracing: shows load/store to local slots with values
    /// Output: [SLOT] store $31: old=int:60 new=bool:true (r15)
    /// Critical for debugging struct field corruption bugs
    slots = 5,

    /// Check if level logs opcodes
    pub fn logsOpcodes(self: TraceLevel) bool {
        return @intFromEnum(self) >= @intFromEnum(TraceLevel.opcodes);
    }

    /// Check if level logs routine calls
    pub fn logsRoutines(self: TraceLevel) bool {
        return @intFromEnum(self) >= @intFromEnum(TraceLevel.routines);
    }

    /// Check if level logs register state
    pub fn logsRegisters(self: TraceLevel) bool {
        return @intFromEnum(self) >= @intFromEnum(TraceLevel.verbose);
    }

    /// Check if level logs slot access (load/store to locals)
    pub fn logsSlots(self: TraceLevel) bool {
        return @intFromEnum(self) >= @intFromEnum(TraceLevel.slots);
    }
};

/// Event type for trace entries
pub const TraceEvent = enum(u8) {
    opcode, // Opcode execution
    call, // Routine call
    return_, // Routine return
    native_call, // Native function call
    native_return, // Native function return
    error_, // Error occurred
    breakpoint, // Hit breakpoint
    slot_store, // Store to local slot
    slot_load, // Load from local slot
};

/// Register snapshot with type information for verbose tracing
pub const RegisterSnapshot = struct {
    /// Raw value bits (interpreted based on type)
    bits: u64,
    /// Type tag for proper display
    tag: value_mod.Tag,

    const value_mod = @import("../bytecode/value.zig");

    pub fn fromValue(v: Value) RegisterSnapshot {
        return .{
            .bits = v.bits,
            .tag = v.tag(),
        };
    }

    /// Format for display
    pub fn format(self: RegisterSnapshot, buf: []u8) []const u8 {
        // Reconstruct Value from raw bits for formatting
        const v = Value{ .bits = self.bits };
        return v.debugRepr(buf);
    }
};

/// A single trace entry stored in history
pub const TraceEntry = struct {
    /// Monotonic timestamp (nanoseconds)
    timestamp: i64,

    /// Event type
    event: TraceEvent,

    /// Instruction pointer
    ip: u32,

    /// Opcode (for opcode events)
    opcode: ?Opcode,

    /// Source line number
    line: u32,

    /// Call depth at this point
    call_depth: u16,

    /// Register snapshots (all 16 registers r0-r15 for verbose mode)
    /// Stores type tags for proper display
    registers: [16]RegisterSnapshot,

    /// Additional context depending on event type
    context: Context,

    pub const Context = union(TraceEvent) {
        opcode: void,
        call: struct {
            target_len: u8,
            arg_count: u8,
        },
        return_: struct {
            has_value: bool,
        },
        native_call: struct {
            name_len: u8,
            arg_count: u8,
        },
        native_return: struct {
            has_value: bool,
        },
        error_: struct {
            err: u16, // Error code
        },
        breakpoint: void,
        slot_store: SlotAccess,
        slot_load: SlotAccess,
    };

    /// Context for slot access tracing (critical for debugging struct corruption)
    pub const SlotAccess = struct {
        slot: u16, // Slot number
        reg: u4, // Register involved
        old_value: RegisterSnapshot, // Previous value in slot (for stores)
        new_value: RegisterSnapshot, // Value being stored/loaded
    };
};

/// Filter configuration for selective tracing
pub const TraceFilter = struct {
    /// Only trace specific IP range (null = all)
    ip_start: ?u32 = null,
    ip_end: ?u32 = null,

    /// Only trace specific source lines (null = all)
    line_start: ?u32 = null,
    line_end: ?u32 = null,

    /// Skip native function calls
    exclude_natives: bool = false,

    /// Check if an entry passes the filter
    pub fn matches(self: TraceFilter, ip: u32, line: u32, is_native: bool) bool {
        if (self.exclude_natives and is_native) return false;

        if (self.ip_start) |start| {
            if (ip < start) return false;
        }
        if (self.ip_end) |end| {
            if (ip > end) return false;
        }
        if (self.line_start) |start| {
            if (line < start) return false;
        }
        if (self.line_end) |end| {
            if (line > end) return false;
        }

        return true;
    }
};

/// Main tracer configuration
pub const TraceConfig = struct {
    /// Trace level (verbosity)
    level: TraceLevel = .none,

    /// Output configuration
    output: OutputConfig = .{},

    /// Filter configuration
    filter: TraceFilter = .{},

    /// History buffer size (for crash dumps)
    /// Set to 0 to disable history
    history_size: u16 = 64,

    /// Breakpoint IPs (pause execution when hit)
    breakpoints: []const u32 = &.{},
};

/// Execution statistics
pub const TraceStats = struct {
    opcodes_executed: u64 = 0,
    calls_made: u64 = 0,
    native_calls: u64 = 0,
    returns: u64 = 0,
    errors: u64 = 0,
    start_time: i64 = 0,
    end_time: i64 = 0,

    /// Get execution duration in milliseconds
    pub fn durationMs(self: TraceStats) i64 {
        if (self.start_time == 0) return 0;
        const end: i64 = if (self.end_time == 0) @truncate(std.time.nanoTimestamp()) else self.end_time;
        return @divFloor(end - self.start_time, std.time.ns_per_ms);
    }
};

/// Main tracer - attach to VM for execution tracing
pub const Tracer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    config: TraceConfig,
    output: Output,
    history: History(TraceEntry),
    stats: TraceStats,

    // Current state
    current_routine: ?[]const u8 = null,
    current_line: u32 = 0,
    call_depth: u16 = 0,
    is_active: bool = false,

    // Routine name buffer (to avoid allocations during tracing)
    routine_buf: [128]u8 = undefined,

    pub fn init(allocator: std.mem.Allocator, config: TraceConfig) Self {
        return .{
            .allocator = allocator,
            .config = config,
            .output = Output.init(allocator, config.output),
            .history = History(TraceEntry).init(allocator, config.history_size),
            .stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.history.deinit();
        self.output.deinit();
    }

    /// Start tracing session
    pub fn start(self: *Self) void {
        self.stats.start_time = @truncate(std.time.nanoTimestamp());
        self.is_active = true;
    }

    /// End tracing session
    pub fn stop(self: *Self) void {
        self.stats.end_time = @truncate(std.time.nanoTimestamp());
        self.is_active = false;
    }

    // =========================================================================
    // Event Hooks (called by VM)
    // =========================================================================

    /// Called before executing an opcode
    pub fn onOpcode(self: *Self, ip: u32, opcode: Opcode, line: u32, registers: [16]Value) void {
        self.stats.opcodes_executed += 1;
        self.current_line = line;

        // Check filter
        if (!self.config.filter.matches(ip, line, false)) return;

        // Create register snapshots with type info for all 16 registers
        var reg_snapshots: [16]RegisterSnapshot = undefined;
        for (0..16) |i| {
            reg_snapshots[i] = RegisterSnapshot.fromValue(registers[i]);
        }

        // Create entry
        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .opcode,
            .ip = ip,
            .opcode = opcode,
            .line = line,
            .call_depth = self.call_depth,
            .registers = reg_snapshots,
            .context = .{ .opcode = {} },
        };

        // Always record in history
        self.history.push(entry);

        // Output if level is high enough
        if (self.config.level.logsOpcodes()) {
            self.output.writeOpcode(ip, opcode, line, self.current_routine, null);

            if (self.config.level.logsRegisters()) {
                self.output.writeRegisterSnapshots(&reg_snapshots);
                self.output.newline();
            }
        }
    }

    /// Called when entering a routine
    pub fn onCall(self: *Self, target: []const u8, arg_count: u8, ip: u32) void {
        self.stats.calls_made += 1;

        // Store routine name
        const len = @min(target.len, self.routine_buf.len);
        @memcpy(self.routine_buf[0..len], target[0..len]);
        self.current_routine = self.routine_buf[0..len];

        // Create entry
        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .call,
            .ip = ip,
            .opcode = null,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .call = .{
                .target_len = @intCast(len),
                .arg_count = arg_count,
            } },
        };

        self.history.push(entry);
        self.call_depth += 1;

        if (self.config.level.logsRoutines()) {
            self.output.writeCall(target, arg_count, ip);
        }
    }

    /// Called when returning from a routine
    pub fn onReturn(self: *Self, routine: []const u8, has_value: bool, ip: u32) void {
        self.onReturnWithValue(routine, has_value, ip, null);
    }

    /// Called when returning from a routine with the actual return value
    /// This enhanced version shows what value is being returned (from r15)
    pub fn onReturnWithValue(self: *Self, routine: []const u8, has_value: bool, ip: u32, return_value: ?Value) void {
        self.stats.returns += 1;

        if (self.call_depth > 0) {
            self.call_depth -= 1;
        }

        // Capture return value in register snapshot if provided
        var reg_snapshots = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16;
        if (return_value) |val| {
            // Put return value in r15 slot (index 15)
            reg_snapshots[15] = RegisterSnapshot.fromValue(val);
        }

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .return_,
            .ip = ip,
            .opcode = null,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = reg_snapshots,
            .context = .{ .return_ = .{ .has_value = has_value } },
        };

        self.history.push(entry);

        if (self.config.level.logsRoutines()) {
            self.output.writeReturnWithValue(routine, has_value, ip, return_value);
        }
    }

    /// Called when a native function is invoked
    pub fn onNativeCall(self: *Self, name: []const u8, arg_count: u8, ip: u32) void {
        self.stats.native_calls += 1;

        if (self.config.filter.exclude_natives) return;

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .native_call,
            .ip = ip,
            .opcode = null,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .native_call = .{
                .name_len = @intCast(@min(name.len, 255)),
                .arg_count = arg_count,
            } },
        };

        self.history.push(entry);

        if (self.config.level.logsRoutines()) {
            self.output.writeCall(name, arg_count, ip);
        }
    }

    /// Called when a native function is invoked with actual argument values
    pub fn onNativeCallWithArgs(self: *Self, name: []const u8, args: []const Value, ip: u32) void {
        self.stats.native_calls += 1;

        if (self.config.filter.exclude_natives) return;

        const arg_count: u8 = @intCast(@min(args.len, 255));

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .native_call,
            .ip = ip,
            .opcode = null,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .native_call = .{
                .name_len = @intCast(@min(name.len, 255)),
                .arg_count = arg_count,
            } },
        };

        self.history.push(entry);
        self.call_depth += 1;

        if (self.config.level.logsRoutines()) {
            self.output.writeNativeCall(name, args, ip);
        }
    }

    /// Called when a native function returns
    pub fn onNativeReturn(self: *Self, name: []const u8, result: ?Value, ip: u32) void {
        if (self.config.filter.exclude_natives) return;

        if (self.call_depth > 0) {
            self.call_depth -= 1;
        }

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .native_return,
            .ip = ip,
            .opcode = null,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .native_return = .{ .has_value = result != null } },
        };

        self.history.push(entry);

        if (self.config.level.logsRoutines()) {
            self.output.writeNativeReturn(name, result, ip);
        }
    }

    /// Called when storing to a local slot
    /// Critical for debugging struct field corruption - shows OLD value before overwrite
    pub fn onSlotStore(self: *Self, ip: u32, slot: u16, reg: u4, old_value: Value, new_value: Value) void {
        // Only trace if slots level is enabled
        if (!self.config.level.logsSlots()) return;

        const old_snap = RegisterSnapshot.fromValue(old_value);
        const new_snap = RegisterSnapshot.fromValue(new_value);

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .slot_store,
            .ip = ip,
            .opcode = .store_local,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .slot_store = .{
                .slot = slot,
                .reg = reg,
                .old_value = old_snap,
                .new_value = new_snap,
            } },
        };

        self.history.push(entry);
        self.output.writeSlotStore(ip, slot, reg, old_value, new_value);
    }

    /// Called when loading from a local slot
    pub fn onSlotLoad(self: *Self, ip: u32, slot: u16, reg: u4, value: Value) void {
        // Only trace if slots level is enabled
        if (!self.config.level.logsSlots()) return;

        const val_snap = RegisterSnapshot.fromValue(value);

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .slot_load,
            .ip = ip,
            .opcode = .load_local,
            .line = self.current_line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{
                .slot_load = .{
                    .slot = slot,
                    .reg = reg,
                    .old_value = val_snap, // Not used for loads
                    .new_value = val_snap,
                },
            },
        };

        self.history.push(entry);
        self.output.writeSlotLoad(ip, slot, reg, value);
    }

    /// Called when an error occurs
    pub fn onError(self: *Self, ip: u32, line: u32, err: anyerror, message: []const u8) void {
        self.stats.errors += 1;

        const entry = TraceEntry{
            .timestamp = @truncate(std.time.nanoTimestamp()),
            .event = .error_,
            .ip = ip,
            .opcode = null,
            .line = line,
            .call_depth = self.call_depth,
            .registers = [_]RegisterSnapshot{RegisterSnapshot.fromValue(Value.null_val)} ** 16,
            .context = .{ .error_ = .{ .err = 0 } },
        };

        self.history.push(entry);
        self.output.writeError(ip, line, err, message);
    }

    // =========================================================================
    // Crash Dump
    // =========================================================================

    /// Dump execution history for crash analysis
    pub fn dumpHistory(self: *Self, writer: anytype) !void {
        try writer.print("\n=== Execution History (last {d} events) ===\n\n", .{self.history.len()});

        var iter = self.history.iterator();
        while (iter.next()) |entry| {
            try self.formatEntry(writer, entry);
        }

        try writer.writeAll("\n=== Statistics ===\n");
        try writer.print("Opcodes executed: {d}\n", .{self.stats.opcodes_executed});
        try writer.print("Calls made: {d}\n", .{self.stats.calls_made});
        try writer.print("Native calls: {d}\n", .{self.stats.native_calls});
        try writer.print("Errors: {d}\n", .{self.stats.errors});
        try writer.print("Duration: {d}ms\n", .{self.stats.durationMs()});
    }

    fn formatEntry(self: *Self, writer: anytype, entry: TraceEntry) !void {
        _ = self;

        // Format based on event type
        switch (entry.event) {
            .opcode => {
                try writer.print("[{d}] 0x{x:0>4} {s}\n", .{
                    entry.line,
                    entry.ip,
                    if (entry.opcode) |op| @tagName(op) else "?",
                });
            },
            .call => {
                try writer.print("[{d}] 0x{x:0>4} CALL ({d} args)\n", .{
                    entry.line,
                    entry.ip,
                    entry.context.call.arg_count,
                });
            },
            .return_ => {
                try writer.print("[{d}] 0x{x:0>4} RETURN\n", .{
                    entry.line,
                    entry.ip,
                });
            },
            .native_call => {
                try writer.print("[{d}] 0x{x:0>4} NATIVE CALL ({d} args)\n", .{
                    entry.line,
                    entry.ip,
                    entry.context.native_call.arg_count,
                });
            },
            .native_return => {
                try writer.print("[{d}] 0x{x:0>4} NATIVE RETURN\n", .{
                    entry.line,
                    entry.ip,
                });
            },
            .error_ => {
                try writer.print("[{d}] 0x{x:0>4} ERROR\n", .{
                    entry.line,
                    entry.ip,
                });
            },
            .breakpoint => {
                try writer.print("[{d}] 0x{x:0>4} BREAKPOINT\n", .{
                    entry.line,
                    entry.ip,
                });
            },
            .slot_store => {
                const ctx = entry.context.slot_store;
                var old_buf: [64]u8 = undefined;
                var new_buf: [64]u8 = undefined;
                const old_repr = ctx.old_value.format(&old_buf);
                const new_repr = ctx.new_value.format(&new_buf);
                try writer.print("[{d}] 0x{x:0>4} STORE ${d} <- r{d}: {s} -> {s}\n", .{
                    entry.line,
                    entry.ip,
                    ctx.slot,
                    ctx.reg,
                    old_repr,
                    new_repr,
                });
            },
            .slot_load => {
                const ctx = entry.context.slot_load;
                var buf: [64]u8 = undefined;
                const repr = ctx.new_value.format(&buf);
                try writer.print("[{d}] 0x{x:0>4} LOAD r{d} <- ${d}: {s}\n", .{
                    entry.line,
                    entry.ip,
                    ctx.reg,
                    ctx.slot,
                    repr,
                });
            },
        }
    }

    // =========================================================================
    // Breakpoint Support
    // =========================================================================

    /// Check if IP is a breakpoint
    pub fn isBreakpoint(self: *Self, ip: u32) bool {
        for (self.config.breakpoints) |bp| {
            if (bp == ip) return true;
        }
        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Tracer: basic opcode tracing" {
    const allocator = std.testing.allocator;

    var tracer = Tracer.init(allocator, .{
        .level = .opcodes,
        .output = .{ .format = .compact, .target = .buffer, .color = false },
        .history_size = 16,
    });
    defer tracer.deinit();

    // Simulate some opcodes
    tracer.onOpcode(0x0000, .debug_line, 10, [_]Value{Value.initInt(0)} ** 16);
    tracer.onOpcode(0x0004, .load_const, 10, [_]Value{Value.initInt(42)} ** 16);

    try std.testing.expectEqual(@as(usize, 2), tracer.history.len());
    try std.testing.expectEqual(@as(u64, 2), tracer.stats.opcodes_executed);
}

test "Tracer: call tracking" {
    const allocator = std.testing.allocator;

    var tracer = Tracer.init(allocator, .{
        .level = .routines,
        .history_size = 16,
    });
    defer tracer.deinit();

    tracer.onCall("main", 0, 0x0000);
    try std.testing.expectEqual(@as(u16, 1), tracer.call_depth);

    tracer.onCall("helper", 2, 0x0010);
    try std.testing.expectEqual(@as(u16, 2), tracer.call_depth);

    tracer.onReturn("helper", false, 0x0020);
    try std.testing.expectEqual(@as(u16, 1), tracer.call_depth);
}

test "TraceFilter: ip range" {
    const filter = TraceFilter{
        .ip_start = 0x0010,
        .ip_end = 0x0020,
    };

    try std.testing.expect(!filter.matches(0x0000, 1, false));
    try std.testing.expect(filter.matches(0x0010, 1, false));
    try std.testing.expect(filter.matches(0x0015, 1, false));
    try std.testing.expect(filter.matches(0x0020, 1, false));
    try std.testing.expect(!filter.matches(0x0021, 1, false));
}
