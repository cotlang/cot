//! Debug Adapter Protocol (DAP) Handler for Cot LSP
//!
//! Integrates debugging support into the LSP server, allowing
//! a single server process to handle both language and debug features.

const std = @import("std");
const json = std.json;
const Allocator = std.mem.Allocator;
const cot = @import("cot");

// VM and bytecode types
const VM = cot.bytecode.VM;
const Module = cot.bytecode.Module;
const StopReason = cot.bytecode.StopReason;

/// Debug session state
pub const DebugSession = struct {
    allocator: Allocator,
    initialized: bool = false,
    launched: bool = false,
    program_path: ?[]const u8 = null,
    source_file: ?[]const u8 = null,
    breakpoints: std.AutoHashMap(u32, Breakpoint),
    sequence: i32 = 1,

    // VM state
    vm: ?*VM = null,
    module: ?Module = null,
    current_line: u32 = 0,
    is_running: bool = false,
    is_stopped: bool = false,
    stop_on_entry: bool = false,

    pub const Breakpoint = struct {
        id: u32,
        line: u32,
        verified: bool = true,
    };

    pub fn init(allocator: Allocator) DebugSession {
        return .{
            .allocator = allocator,
            .breakpoints = std.AutoHashMap(u32, Breakpoint).init(allocator),
        };
    }

    pub fn deinit(self: *DebugSession) void {
        self.breakpoints.deinit();
        if (self.program_path) |p| self.allocator.free(p);
        if (self.source_file) |s| self.allocator.free(s);
        if (self.vm) |vm| {
            vm.deinit();
            self.allocator.destroy(vm);
        }
        if (self.module) |*m| m.deinit();
    }

    pub fn nextSeq(self: *DebugSession) i32 {
        const seq = self.sequence;
        self.sequence += 1;
        return seq;
    }

    /// Compile and load the program
    pub fn loadProgram(self: *DebugSession) !void {
        const program_path = self.program_path orelse return error.NoProgramPath;

        // Read source file
        const file = try std.fs.cwd().openFile(program_path, .{});
        defer file.close();
        const source = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(source);

        // Compile to bytecode module
        self.module = try cot.compileToModule(self.allocator, source, "debug");

        // Initialize VM
        const vm = try self.allocator.create(VM);
        vm.* = VM.init(self.allocator);
        vm.initChannels() catch {};
        self.vm = vm;

        // Enable debugger
        vm.debugger.enable();

        // Set up module for execution (but don't run yet)
        vm.current_module = &self.module.?;
        vm.ip = self.module.?.header.entry_point;

        // Reserve space for local variables
        for (self.module.?.routines) |routine| {
            if (routine.code_offset == self.module.?.header.entry_point) {
                for (0..routine.local_count) |i| {
                    vm.stack[i] = cot.bytecode.vm.Value.initInt(0);
                }
                vm.sp = routine.local_count;
                break;
            }
        }
    }

    /// Sync breakpoints from DAP to VM debugger
    pub fn syncBreakpoints(self: *DebugSession) void {
        if (self.vm) |vm| {
            // Clear existing breakpoints in VM
            vm.debugger.breakpoints.clearRetainingCapacity();

            // Add breakpoints from our map
            var it = self.breakpoints.iterator();
            while (it.next()) |entry| {
                _ = vm.debugger.setBreakpoint(entry.key_ptr.*);
            }
        }
    }

    /// Execute and return the stop reason and current line
    pub fn runUntilStop(self: *DebugSession) !struct { reason: StopReason, line: u32 } {
        if (self.vm) |vm| {
            const reason = try vm.runUntilStop();
            const line = vm.getCurrentLine();
            self.current_line = line;
            self.is_stopped = true;
            self.is_running = false;
            return .{ .reason = reason, .line = line };
        }
        return .{ .reason = .completed, .line = 0 };
    }

    /// Continue execution
    pub fn continueExecution(self: *DebugSession) !struct { reason: StopReason, line: u32 } {
        if (self.vm) |vm| {
            const reason = try vm.continueExecution();
            const line = vm.getCurrentLine();
            self.current_line = line;
            self.is_stopped = (reason != .completed);
            self.is_running = false;
            return .{ .reason = reason, .line = line };
        }
        return .{ .reason = .completed, .line = 0 };
    }

    /// Step over
    pub fn stepOver(self: *DebugSession) !struct { reason: StopReason, line: u32 } {
        if (self.vm) |vm| {
            const reason = try vm.stepOver();
            const line = vm.getCurrentLine();
            self.current_line = line;
            self.is_stopped = true;
            self.is_running = false;
            return .{ .reason = reason, .line = line };
        }
        return .{ .reason = .completed, .line = 0 };
    }

    /// Step into
    pub fn stepInto(self: *DebugSession) !struct { reason: StopReason, line: u32 } {
        if (self.vm) |vm| {
            const reason = try vm.stepInto();
            const line = vm.getCurrentLine();
            self.current_line = line;
            self.is_stopped = true;
            self.is_running = false;
            return .{ .reason = reason, .line = line };
        }
        return .{ .reason = .completed, .line = 0 };
    }

    /// Step out
    pub fn stepOut(self: *DebugSession) !struct { reason: StopReason, line: u32 } {
        if (self.vm) |vm| {
            const reason = try vm.stepOut();
            const line = vm.getCurrentLine();
            self.current_line = line;
            self.is_stopped = true;
            self.is_running = false;
            return .{ .reason = reason, .line = line };
        }
        return .{ .reason = .completed, .line = 0 };
    }
};

/// Check if a message is a DAP message (vs LSP)
pub fn isDapMessage(msg: json.Value) bool {
    if (msg.object.get("type")) |msg_type| {
        const type_str = msg_type.string;
        return std.mem.eql(u8, type_str, "request") or
            std.mem.eql(u8, type_str, "response") or
            std.mem.eql(u8, type_str, "event");
    }
    return false;
}

/// Handle a DAP request message
/// Returns response and optional event to send
pub const HandleResult = struct {
    response: json.Value,
    event: ?json.Value = null,
};

pub fn handleRequest(session: *DebugSession, msg: json.Value, allocator: Allocator) !HandleResult {
    const command = msg.object.get("command") orelse return error.InvalidRequest;
    const seq = msg.object.get("seq") orelse return error.InvalidRequest;
    const seq_num: i32 = @intCast(seq.integer);
    const arguments = msg.object.get("arguments");

    const cmd_str = command.string;

    if (std.mem.eql(u8, cmd_str, "initialize")) {
        return .{ .response = try handleInitialize(session, seq_num, arguments, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "launch")) {
        return try handleLaunch(session, seq_num, arguments, allocator);
    } else if (std.mem.eql(u8, cmd_str, "setBreakpoints")) {
        return .{ .response = try handleSetBreakpoints(session, seq_num, arguments, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "configurationDone")) {
        return try handleConfigurationDone(session, seq_num, allocator);
    } else if (std.mem.eql(u8, cmd_str, "threads")) {
        return .{ .response = try handleThreads(session, seq_num, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "stackTrace")) {
        return .{ .response = try handleStackTrace(session, seq_num, arguments, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "scopes")) {
        return .{ .response = try handleScopes(session, seq_num, arguments, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "variables")) {
        return .{ .response = try handleVariables(session, seq_num, arguments, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "continue")) {
        return try handleContinue(session, seq_num, allocator);
    } else if (std.mem.eql(u8, cmd_str, "next")) {
        return try handleNext(session, seq_num, allocator);
    } else if (std.mem.eql(u8, cmd_str, "stepIn")) {
        return try handleStepIn(session, seq_num, allocator);
    } else if (std.mem.eql(u8, cmd_str, "stepOut")) {
        return try handleStepOut(session, seq_num, allocator);
    } else if (std.mem.eql(u8, cmd_str, "pause")) {
        return .{ .response = try handlePause(session, seq_num, allocator) };
    } else if (std.mem.eql(u8, cmd_str, "disconnect")) {
        return .{ .response = try handleDisconnect(session, seq_num, allocator) };
    } else {
        return .{ .response = try makeErrorResponse(seq_num, cmd_str, "Unknown command", allocator) };
    }
}

fn handleInitialize(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !json.Value {
    _ = arguments;
    session.initialized = true;

    // Build capabilities
    var capabilities = json.ObjectMap.init(allocator);
    try capabilities.put("supportsConfigurationDoneRequest", .{ .bool = true });
    try capabilities.put("supportsFunctionBreakpoints", .{ .bool = false });
    try capabilities.put("supportsConditionalBreakpoints", .{ .bool = false });
    try capabilities.put("supportsEvaluateForHovers", .{ .bool = false });
    try capabilities.put("supportsStepBack", .{ .bool = false });
    try capabilities.put("supportsSetVariable", .{ .bool = false });
    try capabilities.put("supportsRestartFrame", .{ .bool = false });
    try capabilities.put("supportsGotoTargetsRequest", .{ .bool = false });
    try capabilities.put("supportsStepInTargetsRequest", .{ .bool = false });
    try capabilities.put("supportsCompletionsRequest", .{ .bool = false });
    try capabilities.put("supportsModulesRequest", .{ .bool = false });
    try capabilities.put("supportsRestartRequest", .{ .bool = false });
    try capabilities.put("supportsExceptionOptions", .{ .bool = false });
    try capabilities.put("supportsValueFormattingOptions", .{ .bool = false });
    try capabilities.put("supportsExceptionInfoRequest", .{ .bool = false });
    try capabilities.put("supportTerminateDebuggee", .{ .bool = true });
    try capabilities.put("supportsDelayedStackTraceLoading", .{ .bool = false });
    try capabilities.put("supportsLoadedSourcesRequest", .{ .bool = false });

    return makeResponse(session, seq, "initialize", true, null, .{ .object = capabilities }, allocator);
}

fn handleLaunch(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !HandleResult {
    if (arguments) |args| {
        if (args.object.get("program")) |program| {
            session.program_path = try allocator.dupe(u8, program.string);
        }
        if (args.object.get("sourceFile")) |sf| {
            session.source_file = try allocator.dupe(u8, sf.string);
        } else if (session.program_path) |pp| {
            session.source_file = try allocator.dupe(u8, pp);
        }
        if (args.object.get("stopOnEntry")) |soe| {
            session.stop_on_entry = soe.bool;
        }
    }

    // Compile and load the program
    session.loadProgram() catch |err| {
        std.debug.print("Failed to load program: {}\n", .{err});
        return .{
            .response = try makeErrorResponse(seq, "launch", "Failed to compile program", allocator),
        };
    };

    session.launched = true;
    session.is_stopped = true;
    session.current_line = 1;

    // Sync breakpoints to VM
    session.syncBreakpoints();

    return .{
        .response = try makeResponse(session, seq, "launch", true, null, null, allocator),
    };
}

fn handleSetBreakpoints(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !json.Value {
    var breakpoints_array = json.Array.init(allocator);
    var next_bp_id: u32 = 1;

    // Clear existing breakpoints
    session.breakpoints.clearRetainingCapacity();

    if (arguments) |args| {
        if (args.object.get("breakpoints")) |bps| {
            for (bps.array.items) |bp| {
                if (bp.object.get("line")) |line_val| {
                    const line: u32 = @intCast(line_val.integer);

                    // Add breakpoint
                    try session.breakpoints.put(line, .{
                        .id = next_bp_id,
                        .line = line,
                        .verified = true,
                    });
                    next_bp_id += 1;

                    // Create response entry
                    var bp_obj = json.ObjectMap.init(allocator);
                    try bp_obj.put("verified", .{ .bool = true });
                    try bp_obj.put("line", .{ .integer = @intCast(line) });
                    try bp_obj.put("id", .{ .integer = @intCast(next_bp_id - 1) });
                    try breakpoints_array.append(.{ .object = bp_obj });
                }
            }
        }
    }

    // Sync to VM if loaded
    session.syncBreakpoints();

    var body = json.ObjectMap.init(allocator);
    try body.put("breakpoints", .{ .array = breakpoints_array });

    return makeResponse(session, seq, "setBreakpoints", true, null, .{ .object = body }, allocator);
}

fn handleConfigurationDone(session: *DebugSession, seq: i32, allocator: Allocator) !HandleResult {
    const response = try makeResponse(session, seq, "configurationDone", true, null, null, allocator);

    // If stopOnEntry is true, send stopped event; otherwise start running
    if (session.stop_on_entry) {
        session.is_stopped = true;
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "entry", allocator),
        };
    } else {
        // Run until first breakpoint (use continueExecution to set run mode)
        const result = session.continueExecution() catch |err| {
            std.debug.print("Execution error: {}\n", .{err});
            return .{
                .response = response,
                .event = try makeStoppedEvent(session, "exception", allocator),
            };
        };

        if (result.reason == .completed) {
            return .{
                .response = response,
                .event = try makeEvent(session, "terminated", null, allocator),
            };
        } else {
            const reason: []const u8 = switch (result.reason) {
                .breakpoint => "breakpoint",
                .step => "step",
                .paused => "pause",
                else => "step",
            };
            return .{
                .response = response,
                .event = try makeStoppedEvent(session, reason, allocator),
            };
        }
    }
}

fn handleThreads(session: *DebugSession, seq: i32, allocator: Allocator) !json.Value {
    _ = session;
    var threads = json.Array.init(allocator);

    var thread = json.ObjectMap.init(allocator);
    try thread.put("id", .{ .integer = 1 });
    try thread.put("name", .{ .string = "main" });
    try threads.append(.{ .object = thread });

    var body = json.ObjectMap.init(allocator);
    try body.put("threads", .{ .array = threads });

    return makeResponseNoSession(seq, "threads", true, null, .{ .object = body }, allocator);
}

fn handleStackTrace(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !json.Value {
    _ = arguments;

    var frames = json.Array.init(allocator);

    var frame = json.ObjectMap.init(allocator);
    try frame.put("id", .{ .integer = 0 });
    try frame.put("name", .{ .string = "main" });
    try frame.put("line", .{ .integer = @intCast(session.current_line) });
    try frame.put("column", .{ .integer = 1 });

    if (session.source_file) |sf| {
        var source = json.ObjectMap.init(allocator);
        try source.put("name", .{ .string = std.fs.path.basename(sf) });
        try source.put("path", .{ .string = sf });
        try frame.put("source", .{ .object = source });
    }

    try frames.append(.{ .object = frame });

    var body = json.ObjectMap.init(allocator);
    try body.put("stackFrames", .{ .array = frames });
    try body.put("totalFrames", .{ .integer = 1 });

    return makeResponse(session, seq, "stackTrace", true, null, .{ .object = body }, allocator);
}

fn handleScopes(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !json.Value {
    _ = arguments;

    var scopes = json.Array.init(allocator);

    var local_scope = json.ObjectMap.init(allocator);
    try local_scope.put("name", .{ .string = "Locals" });
    try local_scope.put("variablesReference", .{ .integer = 1 });
    try local_scope.put("expensive", .{ .bool = false });
    try scopes.append(.{ .object = local_scope });

    var body = json.ObjectMap.init(allocator);
    try body.put("scopes", .{ .array = scopes });

    return makeResponse(session, seq, "scopes", true, null, .{ .object = body }, allocator);
}

fn handleVariables(session: *DebugSession, seq: i32, arguments: ?json.Value, allocator: Allocator) !json.Value {
    _ = arguments;

    var variables = json.Array.init(allocator);

    // Get local variables from VM
    if (session.vm) |vm| {
        const local_count = vm.getLocalCount();
        var buf: [256]u8 = undefined;

        // Find current routine to get local variable names
        const current_routine = blk: {
            if (session.module) |mod| {
                for (mod.routines) |routine| {
                    if (vm.ip >= routine.code_offset and
                        vm.ip < routine.code_offset + routine.code_length)
                    {
                        break :blk routine;
                    }
                }
            }
            break :blk null;
        };

        for (0..local_count) |slot| {
            if (vm.getLocal(slot)) |value| {
                var var_obj = json.ObjectMap.init(allocator);

                // Try to get variable name from debug info
                const name = blk: {
                    if (current_routine) |routine| {
                        for (routine.locals) |local| {
                            if (local.slot == slot) {
                                // Look up name in constant pool
                                if (session.module) |mod| {
                                    if (local.name_index < mod.constants.len) {
                                        switch (mod.constants[local.name_index]) {
                                            .identifier => |id| break :blk try allocator.dupe(u8, id),
                                            .string => |s| break :blk try allocator.dupe(u8, s),
                                            else => {},
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // Fallback to slot index if no name found
                    break :blk try std.fmt.allocPrint(allocator, "local_{d}", .{slot});
                };
                try var_obj.put("name", .{ .string = name });

                // Format value
                const value_str = vm.formatValue(value, &buf);
                const value_copy = try allocator.dupe(u8, value_str);
                try var_obj.put("value", .{ .string = value_copy });

                // Type
                try var_obj.put("type", .{ .string = VM.getValueType(value) });

                // No children for simple types
                try var_obj.put("variablesReference", .{ .integer = 0 });

                try variables.append(.{ .object = var_obj });
            }
        }
    }

    var body = json.ObjectMap.init(allocator);
    try body.put("variables", .{ .array = variables });

    return makeResponse(session, seq, "variables", true, null, .{ .object = body }, allocator);
}

fn handleContinue(session: *DebugSession, seq: i32, allocator: Allocator) !HandleResult {
    var body = json.ObjectMap.init(allocator);
    try body.put("allThreadsContinued", .{ .bool = true });

    const response = try makeResponse(session, seq, "continue", true, null, .{ .object = body }, allocator);

    // Execute until next stop
    const result = session.continueExecution() catch |err| {
        std.debug.print("Continue error: {}\n", .{err});
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "exception", allocator),
        };
    };

    if (result.reason == .completed) {
        return .{
            .response = response,
            .event = try makeEvent(session, "terminated", null, allocator),
        };
    } else {
        const reason: []const u8 = switch (result.reason) {
            .breakpoint => "breakpoint",
            .step => "step",
            .paused => "pause",
            else => "step",
        };
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, reason, allocator),
        };
    }
}

fn handleNext(session: *DebugSession, seq: i32, allocator: Allocator) !HandleResult {
    const response = try makeResponse(session, seq, "next", true, null, null, allocator);

    const result = session.stepOver() catch |err| {
        std.debug.print("Step over error: {}\n", .{err});
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "exception", allocator),
        };
    };

    if (result.reason == .completed) {
        return .{
            .response = response,
            .event = try makeEvent(session, "terminated", null, allocator),
        };
    } else {
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "step", allocator),
        };
    }
}

fn handleStepIn(session: *DebugSession, seq: i32, allocator: Allocator) !HandleResult {
    const response = try makeResponse(session, seq, "stepIn", true, null, null, allocator);

    const result = session.stepInto() catch |err| {
        std.debug.print("Step in error: {}\n", .{err});
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "exception", allocator),
        };
    };

    if (result.reason == .completed) {
        return .{
            .response = response,
            .event = try makeEvent(session, "terminated", null, allocator),
        };
    } else {
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "step", allocator),
        };
    }
}

fn handleStepOut(session: *DebugSession, seq: i32, allocator: Allocator) !HandleResult {
    const response = try makeResponse(session, seq, "stepOut", true, null, null, allocator);

    const result = session.stepOut() catch |err| {
        std.debug.print("Step out error: {}\n", .{err});
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "exception", allocator),
        };
    };

    if (result.reason == .completed) {
        return .{
            .response = response,
            .event = try makeEvent(session, "terminated", null, allocator),
        };
    } else {
        return .{
            .response = response,
            .event = try makeStoppedEvent(session, "step", allocator),
        };
    }
}

fn handlePause(session: *DebugSession, seq: i32, allocator: Allocator) !json.Value {
    session.is_running = false;
    session.is_stopped = true;

    return makeResponse(session, seq, "pause", true, null, null, allocator);
}

fn handleDisconnect(session: *DebugSession, seq: i32, allocator: Allocator) !json.Value {
    session.launched = false;
    session.is_running = false;

    return makeResponse(session, seq, "disconnect", true, null, null, allocator);
}

// ============================================================================
// Response helpers
// ============================================================================

fn makeResponse(
    session: *DebugSession,
    request_seq: i32,
    command: []const u8,
    success: bool,
    message: ?[]const u8,
    body: ?json.Value,
    allocator: Allocator,
) !json.Value {
    var response = json.ObjectMap.init(allocator);
    try response.put("seq", .{ .integer = session.nextSeq() });
    try response.put("type", .{ .string = "response" });
    try response.put("request_seq", .{ .integer = request_seq });
    try response.put("success", .{ .bool = success });
    try response.put("command", .{ .string = command });

    if (message) |msg| {
        try response.put("message", .{ .string = msg });
    }
    if (body) |b| {
        try response.put("body", b);
    }

    return .{ .object = response };
}

fn makeResponseNoSession(
    request_seq: i32,
    command: []const u8,
    success: bool,
    message: ?[]const u8,
    body: ?json.Value,
    allocator: Allocator,
) !json.Value {
    var response = json.ObjectMap.init(allocator);
    try response.put("seq", .{ .integer = 0 });
    try response.put("type", .{ .string = "response" });
    try response.put("request_seq", .{ .integer = request_seq });
    try response.put("success", .{ .bool = success });
    try response.put("command", .{ .string = command });

    if (message) |msg| {
        try response.put("message", .{ .string = msg });
    }
    if (body) |b| {
        try response.put("body", b);
    }

    return .{ .object = response };
}

fn makeErrorResponse(request_seq: i32, command: []const u8, message: []const u8, allocator: Allocator) !json.Value {
    var response = json.ObjectMap.init(allocator);
    try response.put("seq", .{ .integer = 0 });
    try response.put("type", .{ .string = "response" });
    try response.put("request_seq", .{ .integer = request_seq });
    try response.put("success", .{ .bool = false });
    try response.put("command", .{ .string = command });
    try response.put("message", .{ .string = message });

    return .{ .object = response };
}

/// Create a DAP event message
pub fn makeEvent(session: *DebugSession, event: []const u8, body: ?json.Value, allocator: Allocator) !json.Value {
    var evt = json.ObjectMap.init(allocator);
    try evt.put("seq", .{ .integer = session.nextSeq() });
    try evt.put("type", .{ .string = "event" });
    try evt.put("event", .{ .string = event });

    if (body) |b| {
        try evt.put("body", b);
    }

    return .{ .object = evt };
}

/// Create a stopped event
pub fn makeStoppedEvent(session: *DebugSession, reason: []const u8, allocator: Allocator) !json.Value {
    var body = json.ObjectMap.init(allocator);
    try body.put("reason", .{ .string = reason });
    try body.put("threadId", .{ .integer = 1 });
    try body.put("allThreadsStopped", .{ .bool = true });

    return makeEvent(session, "stopped", .{ .object = body }, allocator);
}
