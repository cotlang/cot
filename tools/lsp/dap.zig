//! Debug Adapter Protocol (DAP) Implementation
//!
//! Provides VSCode debugging support for Cot programs.
//! Uses the shared debug infrastructure from src/runtime/debug/.

const std = @import("std");
const protocol = @import("protocol.zig");
const Allocator = std.mem.Allocator;
const JsonValue = protocol.JsonValue;
const JsonObject = protocol.JsonObject;
const JsonArray = protocol.JsonArray;

// Import Cot runtime components
const cot = @import("cot");
const cot_runtime = @import("cot_runtime");
const VM = cot_runtime.bytecode.VM;
const Module = cot_runtime.bytecode.Module;
const debug_tools = cot_runtime.debug_tools;
const BreakpointManager = debug_tools.BreakpointManager;
const StateInspector = debug_tools.StateInspector;
const LocalInfo = debug_tools.LocalInfo;
const FrameInfo = debug_tools.FrameInfo;

/// DAP message types
pub const MessageType = enum {
    request,
    response,
    event,
};

/// DAP request commands we handle
pub const Command = enum {
    initialize,
    launch,
    attach,
    disconnect,
    setBreakpoints,
    setExceptionBreakpoints,
    configurationDone,
    threads,
    stackTrace,
    scopes,
    variables,
    @"continue",
    next,
    stepIn,
    stepOut,
    pause,
    evaluate,
    source,

    pub fn fromString(s: []const u8) ?Command {
        const map = std.StaticStringMap(Command).initComptime(.{
            .{ "initialize", .initialize },
            .{ "launch", .launch },
            .{ "attach", .attach },
            .{ "disconnect", .disconnect },
            .{ "setBreakpoints", .setBreakpoints },
            .{ "setExceptionBreakpoints", .setExceptionBreakpoints },
            .{ "configurationDone", .configurationDone },
            .{ "threads", .threads },
            .{ "stackTrace", .stackTrace },
            .{ "scopes", .scopes },
            .{ "variables", .variables },
            .{ "continue", .@"continue" },
            .{ "next", .next },
            .{ "stepIn", .stepIn },
            .{ "stepOut", .stepOut },
            .{ "pause", .pause },
            .{ "evaluate", .evaluate },
            .{ "source", .source },
        });
        return map.get(s);
    }
};

/// DAP Debug Adapter state
pub const DebugAdapter = struct {
    allocator: Allocator,

    /// Sequence number for responses/events
    seq: i64 = 1,

    /// Whether we've been initialized
    initialized: bool = false,

    /// The VM instance for debugging (heap allocated)
    vm: ?*VM = null,

    /// The loaded module (heap allocated)
    module: ?*Module = null,

    /// Source file path (for source mapping)
    source_file: ?[]const u8 = null,

    /// Source lines (for source display)
    source_lines: ?[][]const u8 = null,

    /// Whether program is running
    is_running: bool = false,

    /// Thread ID (we only have one thread)
    thread_id: i64 = 1,

    /// Buffered output from print/println (for Debug Console)
    output_buffer: std.ArrayListUnmanaged(u8) = .empty,

    const Self = @This();

    /// Static callback function for VM output capture
    /// This is called by the VM when print/println is executed
    fn outputCallback(text: []const u8, context: *anyopaque) void {
        const self: *Self = @ptrCast(@alignCast(context));
        self.output_buffer.appendSlice(self.allocator, text) catch {};
    }

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.vm) |vm| {
            vm.deinit();
            self.allocator.destroy(vm);
            self.vm = null;
        }
        if (self.module) |mod| {
            mod.deinit();
            self.allocator.destroy(mod);
            self.module = null;
        }
        if (self.source_lines) |lines| {
            for (lines) |line| {
                self.allocator.free(line);
            }
            self.allocator.free(lines);
            self.source_lines = null;
        }
        if (self.source_file) |path| {
            self.allocator.free(path);
            self.source_file = null;
        }
        self.output_buffer.deinit(self.allocator);
    }

    /// Check if a message is a DAP message (vs LSP)
    pub fn isDAPMessage(msg: JsonObject) bool {
        // DAP messages have "type" field with "request"/"response"/"event"
        // LSP messages have "jsonrpc" field
        if (msg.get("type")) |type_val| {
            if (type_val == .string) {
                const t = type_val.string;
                return std.mem.eql(u8, t, "request") or
                    std.mem.eql(u8, t, "response") or
                    std.mem.eql(u8, t, "event");
            }
        }
        return false;
    }

    /// Handle a DAP request
    pub fn handleRequest(self: *Self, msg: JsonObject, allocator: Allocator) !JsonValue {
        const seq = if (msg.get("seq")) |s| s.integer else 0;
        const command_str = msg.get("command") orelse {
            return self.makeErrorResponse(seq, "", "Missing command", allocator);
        };
        if (command_str != .string) {
            return self.makeErrorResponse(seq, "", "Invalid command type", allocator);
        }

        std.debug.print("DAP: Received command '{s}'\n", .{command_str.string});

        const command = Command.fromString(command_str.string) orelse {
            std.debug.print("DAP: Unknown command '{s}'\n", .{command_str.string});
            return self.makeErrorResponse(seq, command_str.string, "Unknown command", allocator);
        };

        const arguments = if (msg.get("arguments")) |args| args else JsonValue{ .object = JsonObject.init(allocator) };

        return switch (command) {
            .initialize => self.handleInitialize(seq, arguments, allocator),
            .launch => self.handleLaunch(seq, arguments, allocator),
            .disconnect => self.handleDisconnect(seq, arguments, allocator),
            .setBreakpoints => self.handleSetBreakpoints(seq, arguments, allocator),
            .setExceptionBreakpoints => self.handleSetExceptionBreakpoints(seq, arguments, allocator),
            .configurationDone => self.handleConfigurationDone(seq, arguments, allocator),
            .threads => self.handleThreads(seq, arguments, allocator),
            .stackTrace => self.handleStackTrace(seq, arguments, allocator),
            .scopes => self.handleScopes(seq, arguments, allocator),
            .variables => self.handleVariables(seq, arguments, allocator),
            .@"continue" => self.handleContinue(seq, arguments, allocator),
            .next => self.handleNext(seq, arguments, allocator),
            .stepIn => self.handleStepIn(seq, arguments, allocator),
            .stepOut => self.handleStepOut(seq, arguments, allocator),
            .pause => self.handlePause(seq, arguments, allocator),
            .evaluate => self.handleEvaluate(seq, arguments, allocator),
            .source => self.handleSource(seq, arguments, allocator),
            .attach => self.makeErrorResponse(seq, "attach", "Attach not supported", allocator),
        };
    }

    // =========================================================================
    // DAP Request Handlers
    // =========================================================================

    fn handleInitialize(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;
        self.initialized = true;

        // Build capabilities
        var capabilities = JsonObject.init(allocator);
        try capabilities.put("supportsConfigurationDoneRequest", JsonValue{ .bool = true });
        try capabilities.put("supportsFunctionBreakpoints", JsonValue{ .bool = false });
        try capabilities.put("supportsConditionalBreakpoints", JsonValue{ .bool = false });
        try capabilities.put("supportsEvaluateForHovers", JsonValue{ .bool = true });
        try capabilities.put("supportsStepBack", JsonValue{ .bool = false });
        try capabilities.put("supportsSetVariable", JsonValue{ .bool = false });
        try capabilities.put("supportsRestartFrame", JsonValue{ .bool = false });
        try capabilities.put("supportsGotoTargetsRequest", JsonValue{ .bool = false });
        try capabilities.put("supportsStepInTargetsRequest", JsonValue{ .bool = false });
        try capabilities.put("supportsCompletionsRequest", JsonValue{ .bool = false });
        try capabilities.put("supportsModulesRequest", JsonValue{ .bool = false });
        try capabilities.put("supportsExceptionOptions", JsonValue{ .bool = false });
        try capabilities.put("supportsValueFormattingOptions", JsonValue{ .bool = false });
        try capabilities.put("supportsExceptionInfoRequest", JsonValue{ .bool = false });
        try capabilities.put("supportTerminateDebuggee", JsonValue{ .bool = true });
        try capabilities.put("supportsDelayedStackTraceLoading", JsonValue{ .bool = false });
        try capabilities.put("supportsLoadedSourcesRequest", JsonValue{ .bool = false });

        return self.makeResponse(seq, "initialize", JsonValue{ .object = capabilities }, allocator);
    }

    fn handleLaunch(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        const program = if (args.object.get("program")) |p| p.string else {
            return self.makeErrorResponse(seq, "launch", "No program specified", allocator);
        };

        // Store source file path
        self.source_file = try allocator.dupe(u8, program);

        // Check if it's a .cot file (needs compilation) or .cbo file
        const is_source = std.mem.endsWith(u8, program, ".cot") or std.mem.endsWith(u8, program, ".dbl");

        // Allocate module on heap
        const mod_ptr = try allocator.create(Module);
        errdefer allocator.destroy(mod_ptr);

        if (is_source) {
            // Load and store source lines for display
            const source_content = std.fs.cwd().readFileAlloc(allocator, program, 1024 * 1024) catch |err| {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Cannot read file: {s}", .{@errorName(err)}) catch "Cannot read file";
                return self.makeErrorResponse(seq, "launch", msg, allocator);
            };
            defer allocator.free(source_content);

            // Split into lines
            var lines_list: std.ArrayListUnmanaged([]const u8) = .empty;
            errdefer {
                for (lines_list.items) |line| allocator.free(line);
                lines_list.deinit(allocator);
            }
            var line_iter = std.mem.splitScalar(u8, source_content, '\n');
            while (line_iter.next()) |line| {
                try lines_list.append(allocator, try allocator.dupe(u8, line));
            }
            self.source_lines = try lines_list.toOwnedSlice(allocator);

            // Compile the source
            mod_ptr.* = compileSource(allocator, program, source_content) catch |err| {
                var msg_buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Compilation failed: {s} - check the file for syntax errors or undefined functions", .{@errorName(err)}) catch "Compilation failed";
                return self.makeErrorResponse(seq, "launch", msg, allocator);
            };
        } else {
            // Load bytecode directly
            const file = std.fs.cwd().openFile(program, .{}) catch |err| {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Cannot open bytecode file: {s}", .{@errorName(err)}) catch "Cannot open file";
                return self.makeErrorResponse(seq, "launch", msg, allocator);
            };
            defer file.close();

            const bytes = file.readToEndAlloc(allocator, 1024 * 1024 * 10) catch |err| {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Cannot read bytecode file: {s}", .{@errorName(err)}) catch "Cannot read file";
                return self.makeErrorResponse(seq, "launch", msg, allocator);
            };
            defer allocator.free(bytes);

            var fbs = std.io.fixedBufferStream(bytes);
            mod_ptr.* = Module.deserialize(allocator, fbs.reader().any()) catch |err| {
                var msg_buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&msg_buf, "Invalid bytecode format: {s}", .{@errorName(err)}) catch "Invalid bytecode";
                return self.makeErrorResponse(seq, "launch", msg, allocator);
            };
        }

        self.module = mod_ptr;

        // Create VM on heap
        const vm_ptr = try allocator.create(VM);
        errdefer allocator.destroy(vm_ptr);
        vm_ptr.* = VM.init(allocator);

        vm_ptr.debugger.enable();
        vm_ptr.initForModule(mod_ptr) catch |err| {
            return self.makeErrorResponse(seq, "launch", @errorName(err), allocator);
        };

        // Register output callback for Debug Console
        vm_ptr.setOutputCallback(&outputCallback, @ptrCast(self));

        self.vm = vm_ptr;
        self.is_running = false;

        // Check if we should stop on entry
        const stop_on_entry = if (args.object.get("stopOnEntry")) |s| s.bool else false;

        if (stop_on_entry) {
            // Keep debugger paused so we stop immediately
            vm_ptr.debugger.step_mode = .paused;
        } else {
            // Set to run mode - will start after configurationDone
            vm_ptr.debugger.step_mode = .run;
        }

        // Send initialized event - this triggers VSCode to send setBreakpoints
        try self.sendEvent("initialized", JsonValue{ .object = JsonObject.init(allocator) }, allocator);

        if (stop_on_entry) {
            // Send stopped event immediately
            try self.sendStoppedEvent("entry", allocator);
        }

        return self.makeResponse(seq, "launch", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleDisconnect(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;
        self.is_running = false;

        // Clean up session resources only (not the adapter itself - main's defer handles that)
        if (self.vm) |vm| {
            vm.deinit();
            self.allocator.destroy(vm);
            self.vm = null;
        }
        if (self.module) |mod| {
            mod.deinit();
            self.allocator.destroy(mod);
            self.module = null;
        }

        return self.makeResponse(seq, "disconnect", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleSetBreakpoints(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        std.debug.print("DAP: setBreakpoints called\n", .{});

        const vm = self.vm orelse {
            std.debug.print("DAP: No VM in setBreakpoints!\n", .{});
            return self.makeErrorResponse(seq, "setBreakpoints", "No active debug session", allocator);
        };

        // Clear existing breakpoints
        vm.debugger.bp_manager.clear();
        std.debug.print("DAP: Cleared existing breakpoints\n", .{});

        // Get breakpoints from request
        var result_breakpoints = JsonArray.init(allocator);

        if (args.object.get("breakpoints")) |bps| {
            if (bps == .array) {
                std.debug.print("DAP: Found {} breakpoints to set\n", .{bps.array.items.len});
                for (bps.array.items) |bp| {
                    if (bp.object.get("line")) |line_val| {
                        const line: u32 = @intCast(line_val.integer);
                        const bp_id = vm.debugger.setBreakpoint(line);
                        std.debug.print("DAP: Set breakpoint at line {} -> id {}\n", .{ line, bp_id });

                        var bp_result = JsonObject.init(allocator);
                        try bp_result.put("id", JsonValue{ .integer = @intCast(bp_id) });
                        try bp_result.put("verified", JsonValue{ .bool = bp_id > 0 });
                        try bp_result.put("line", JsonValue{ .integer = @intCast(line) });
                        try result_breakpoints.append(JsonValue{ .object = bp_result });
                    }
                }
            }
        } else {
            std.debug.print("DAP: No breakpoints array in request\n", .{});
        }

        var body = JsonObject.init(allocator);
        try body.put("breakpoints", JsonValue{ .array = result_breakpoints });

        return self.makeResponse(seq, "setBreakpoints", JsonValue{ .object = body }, allocator);
    }

    fn handleSetExceptionBreakpoints(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;
        _ = self;
        // We don't support exception breakpoints yet
        return makeResponseStatic(seq, "setExceptionBreakpoints", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleConfigurationDone(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        // If we have a VM and we're not already stopped, start execution
        if (self.vm) |vm| {
            // Check if we're supposed to stop on entry (already sent stopped event in launch)
            if (!self.is_running and vm.debugger.step_mode != .paused) {
                // Start running the program
                vm.debugger.continue_();
                self.is_running = true;

                _ = vm.runUntilStop() catch |err| {
                    std.debug.print("DAP: Execution error: {}\n", .{err});
                    try self.sendStoppedEvent("exception", allocator);
                    var msg_buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&msg_buf, "Execution error: {s}", .{@errorName(err)}) catch "Execution error";
                    return self.makeErrorResponse(seq, "configurationDone", msg, allocator);
                };

                self.is_running = false;

                // Flush any buffered output to Debug Console
                try self.flushOutput(allocator);

                // Check why we stopped
                if (vm.stop_reason) |reason| {
                    switch (reason) {
                        .completed => try self.sendEvent("terminated", JsonValue{ .object = JsonObject.init(allocator) }, allocator),
                        .breakpoint => try self.sendStoppedEvent("breakpoint", allocator),
                        .step => try self.sendStoppedEvent("step", allocator),
                        .err => try self.sendStoppedEvent("exception", allocator),
                        .paused => try self.sendStoppedEvent("pause", allocator),
                    }
                } else {
                    // No stop reason means completed
                    try self.sendEvent("terminated", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
                }
            }
        }

        return self.makeResponse(seq, "configurationDone", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleThreads(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        var threads = JsonArray.init(allocator);
        var thread = JsonObject.init(allocator);
        try thread.put("id", JsonValue{ .integer = self.thread_id });
        try thread.put("name", JsonValue{ .string = "main" });
        try threads.append(JsonValue{ .object = thread });

        var body = JsonObject.init(allocator);
        try body.put("threads", JsonValue{ .array = threads });

        return self.makeResponse(seq, "threads", JsonValue{ .object = body }, allocator);
    }

    fn handleStackTrace(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "stackTrace", "No active debug session", allocator);
        };

        const inspector = vm.createInspector();
        const snapshot = inspector.getSnapshot();

        var frames = JsonArray.init(allocator);

        // Current frame
        var frame = JsonObject.init(allocator);
        try frame.put("id", JsonValue{ .integer = 0 });
        try frame.put("name", JsonValue{ .string = snapshot.routine orelse "main" });
        try frame.put("line", JsonValue{ .integer = @intCast(snapshot.line) });
        try frame.put("column", JsonValue{ .integer = 1 });

        if (self.source_file) |path| {
            var source = JsonObject.init(allocator);
            try source.put("path", JsonValue{ .string = path });
            try frame.put("source", JsonValue{ .object = source });
        }

        try frames.append(JsonValue{ .object = frame });

        // Add call stack frames
        const call_stack = inspector.getCallStack(allocator) catch &[_]FrameInfo{};
        defer allocator.free(call_stack);

        for (call_stack, 1..) |call_frame, i| {
            var cf = JsonObject.init(allocator);
            try cf.put("id", JsonValue{ .integer = @intCast(i) });
            try cf.put("name", JsonValue{ .string = call_frame.routine_name });
            try cf.put("line", JsonValue{ .integer = @intCast(call_frame.source_line) });
            try cf.put("column", JsonValue{ .integer = 1 });

            if (self.source_file) |path| {
                var source = JsonObject.init(allocator);
                try source.put("path", JsonValue{ .string = path });
                try cf.put("source", JsonValue{ .object = source });
            }

            try frames.append(JsonValue{ .object = cf });
        }

        var body = JsonObject.init(allocator);
        try body.put("stackFrames", JsonValue{ .array = frames });
        try body.put("totalFrames", JsonValue{ .integer = @intCast(frames.items.len) });

        return self.makeResponse(seq, "stackTrace", JsonValue{ .object = body }, allocator);
    }

    fn handleScopes(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        var scopes = JsonArray.init(allocator);

        // Locals scope
        var locals_scope = JsonObject.init(allocator);
        try locals_scope.put("name", JsonValue{ .string = "Locals" });
        try locals_scope.put("variablesReference", JsonValue{ .integer = 1 });
        try locals_scope.put("expensive", JsonValue{ .bool = false });
        try scopes.append(JsonValue{ .object = locals_scope });

        // Registers scope
        var regs_scope = JsonObject.init(allocator);
        try regs_scope.put("name", JsonValue{ .string = "Registers" });
        try regs_scope.put("variablesReference", JsonValue{ .integer = 2 });
        try regs_scope.put("expensive", JsonValue{ .bool = false });
        try scopes.append(JsonValue{ .object = regs_scope });

        var body = JsonObject.init(allocator);
        try body.put("scopes", JsonValue{ .array = scopes });

        return self.makeResponse(seq, "scopes", JsonValue{ .object = body }, allocator);
    }

    fn handleVariables(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "variables", "No active debug session", allocator);
        };

        const ref = if (args.object.get("variablesReference")) |r| r.integer else 0;
        const inspector = vm.createInspector();

        var variables = JsonArray.init(allocator);

        if (ref == 1) {
            // Locals
            const locals = inspector.getLocals(allocator) catch &[_]LocalInfo{};
            defer allocator.free(locals);

            for (locals) |local| {
                var val_buf: [128]u8 = undefined;
                const val_str = local.format(&val_buf);

                var v = JsonObject.init(allocator);
                try v.put("name", JsonValue{ .string = local.name });
                try v.put("value", JsonValue{ .string = try allocator.dupe(u8, val_str) });
                try v.put("type", JsonValue{ .string = local.type_name });
                try v.put("variablesReference", JsonValue{ .integer = 0 });
                try variables.append(JsonValue{ .object = v });
            }
        } else if (ref == 2) {
            // Registers
            for (0..16) |i| {
                const reg_info = inspector.getRegisterInfo(@intCast(i));
                var val_buf: [128]u8 = undefined;
                const val_str = reg_info.format(&val_buf);

                var name_buf: [8]u8 = undefined;
                const name = std.fmt.bufPrint(&name_buf, "r{d}", .{i}) catch "r?";

                var v = JsonObject.init(allocator);
                try v.put("name", JsonValue{ .string = try allocator.dupe(u8, name) });
                try v.put("value", JsonValue{ .string = try allocator.dupe(u8, val_str) });
                try v.put("type", JsonValue{ .string = reg_info.typeName() });
                try v.put("variablesReference", JsonValue{ .integer = 0 });
                try variables.append(JsonValue{ .object = v });
            }
        }

        var body = JsonObject.init(allocator);
        try body.put("variables", JsonValue{ .array = variables });

        return self.makeResponse(seq, "variables", JsonValue{ .object = body }, allocator);
    }

    fn handleContinue(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "continue", "No active debug session", allocator);
        };

        vm.debugger.continue_();
        self.is_running = true;

        // Run until stop
        _ = vm.runUntilStop() catch |err| {
            try self.sendStoppedEvent("exception", allocator);
            return self.makeErrorResponse(seq, "continue", @errorName(err), allocator);
        };

        self.is_running = false;

        // Flush any buffered output to Debug Console
        try self.flushOutput(allocator);

        // Check why we stopped
        if (vm.stop_reason) |reason| {
            switch (reason) {
                .completed => try self.sendEvent("terminated", JsonValue{ .object = JsonObject.init(allocator) }, allocator),
                .breakpoint => try self.sendStoppedEvent("breakpoint", allocator),
                .step => try self.sendStoppedEvent("step", allocator),
                .err => try self.sendStoppedEvent("exception", allocator),
                .paused => try self.sendStoppedEvent("pause", allocator),
            }
        }

        var body = JsonObject.init(allocator);
        try body.put("allThreadsContinued", JsonValue{ .bool = true });

        return self.makeResponse(seq, "continue", JsonValue{ .object = body }, allocator);
    }

    fn handleNext(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "next", "No active debug session", allocator);
        };

        vm.debugger.stepOver(vm.call_stack.len);
        _ = vm.runUntilStop() catch {};

        // Flush any buffered output to Debug Console
        try self.flushOutput(allocator);

        try self.sendStoppedEvent("step", allocator);

        return self.makeResponse(seq, "next", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleStepIn(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "stepIn", "No active debug session", allocator);
        };

        vm.debugger.stepInto();
        _ = vm.runUntilStop() catch {};

        // Flush any buffered output to Debug Console
        try self.flushOutput(allocator);

        try self.sendStoppedEvent("step", allocator);

        return self.makeResponse(seq, "stepIn", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleStepOut(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "stepOut", "No active debug session", allocator);
        };

        vm.debugger.stepOut(vm.call_stack.len);
        _ = vm.runUntilStop() catch {};

        // Flush any buffered output to Debug Console
        try self.flushOutput(allocator);

        try self.sendStoppedEvent("step", allocator);

        return self.makeResponse(seq, "stepOut", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handlePause(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "pause", "No active debug session", allocator);
        };

        vm.debugger.pause();
        try self.sendStoppedEvent("pause", allocator);

        return self.makeResponse(seq, "pause", JsonValue{ .object = JsonObject.init(allocator) }, allocator);
    }

    fn handleEvaluate(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        const vm = self.vm orelse {
            return self.makeErrorResponse(seq, "evaluate", "No active debug session", allocator);
        };

        const expression = if (args.object.get("expression")) |e| e.string else "";
        const inspector = vm.createInspector();

        // Try to find a local variable with this name
        if (inspector.getLocalByName(expression)) |local| {
            var val_buf: [128]u8 = undefined;
            const val_str = local.format(&val_buf);

            var body = JsonObject.init(allocator);
            try body.put("result", JsonValue{ .string = try allocator.dupe(u8, val_str) });
            try body.put("type", JsonValue{ .string = local.type_name });
            try body.put("variablesReference", JsonValue{ .integer = 0 });

            return self.makeResponse(seq, "evaluate", JsonValue{ .object = body }, allocator);
        }

        // Try register (r0-r15)
        if (expression.len >= 2 and expression[0] == 'r') {
            const reg_num = std.fmt.parseInt(u4, expression[1..], 10) catch null;
            if (reg_num) |reg| {
                const reg_info = inspector.getRegisterInfo(reg);
                var val_buf: [128]u8 = undefined;
                const val_str = reg_info.format(&val_buf);

                var body = JsonObject.init(allocator);
                try body.put("result", JsonValue{ .string = try allocator.dupe(u8, val_str) });
                try body.put("type", JsonValue{ .string = reg_info.typeName() });
                try body.put("variablesReference", JsonValue{ .integer = 0 });

                return self.makeResponse(seq, "evaluate", JsonValue{ .object = body }, allocator);
            }
        }

        return self.makeErrorResponse(seq, "evaluate", "Cannot evaluate expression", allocator);
    }

    fn handleSource(self: *Self, seq: i64, args: JsonValue, allocator: Allocator) !JsonValue {
        _ = args;

        if (self.source_lines) |lines| {
            var content: std.ArrayListUnmanaged(u8) = .empty;
            defer content.deinit(allocator);

            for (lines) |line| {
                try content.appendSlice(allocator, line);
                try content.append(allocator, '\n');
            }

            var body = JsonObject.init(allocator);
            try body.put("content", JsonValue{ .string = try content.toOwnedSlice(allocator) });

            return self.makeResponse(seq, "source", JsonValue{ .object = body }, allocator);
        }

        return self.makeErrorResponse(seq, "source", "Source not available", allocator);
    }

    // =========================================================================
    // Response/Event Helpers
    // =========================================================================

    fn makeResponse(self: *Self, request_seq: i64, command: []const u8, body: JsonValue, allocator: Allocator) !JsonValue {
        var response = JsonObject.init(allocator);
        try response.put("seq", JsonValue{ .integer = self.seq });
        self.seq += 1;
        try response.put("type", JsonValue{ .string = "response" });
        try response.put("request_seq", JsonValue{ .integer = request_seq });
        try response.put("success", JsonValue{ .bool = true });
        try response.put("command", JsonValue{ .string = command });
        try response.put("body", body);
        return JsonValue{ .object = response };
    }

    fn makeResponseStatic(request_seq: i64, command: []const u8, body: JsonValue, allocator: Allocator) !JsonValue {
        var response = JsonObject.init(allocator);
        try response.put("seq", JsonValue{ .integer = 0 });
        try response.put("type", JsonValue{ .string = "response" });
        try response.put("request_seq", JsonValue{ .integer = request_seq });
        try response.put("success", JsonValue{ .bool = true });
        try response.put("command", JsonValue{ .string = command });
        try response.put("body", body);
        return JsonValue{ .object = response };
    }

    fn makeErrorResponse(self: *Self, request_seq: i64, command: []const u8, message: []const u8, allocator: Allocator) !JsonValue {
        var response = JsonObject.init(allocator);
        try response.put("seq", JsonValue{ .integer = self.seq });
        self.seq += 1;
        try response.put("type", JsonValue{ .string = "response" });
        try response.put("request_seq", JsonValue{ .integer = request_seq });
        try response.put("success", JsonValue{ .bool = false });
        try response.put("command", JsonValue{ .string = command });
        try response.put("message", JsonValue{ .string = message });
        return JsonValue{ .object = response };
    }

    pub fn sendEvent(self: *Self, event: []const u8, body: JsonValue, allocator: Allocator) !void {
        var msg = JsonObject.init(allocator);
        try msg.put("seq", JsonValue{ .integer = self.seq });
        self.seq += 1;
        try msg.put("type", JsonValue{ .string = "event" });
        try msg.put("event", JsonValue{ .string = event });
        try msg.put("body", body);

        try protocol.writeMessage(JsonValue{ .object = msg }, allocator);
    }

    fn sendStoppedEvent(self: *Self, reason: []const u8, allocator: Allocator) !void {
        var body = JsonObject.init(allocator);
        try body.put("reason", JsonValue{ .string = reason });
        try body.put("threadId", JsonValue{ .integer = self.thread_id });
        try body.put("allThreadsStopped", JsonValue{ .bool = true });

        try self.sendEvent("stopped", JsonValue{ .object = body }, allocator);
    }

    /// Flush any buffered output as DAP "output" events
    /// Called after VM execution to send program output to Debug Console
    fn flushOutput(self: *Self, allocator: Allocator) !void {
        if (self.output_buffer.items.len == 0) return;

        // Create output event body
        var body = JsonObject.init(allocator);
        try body.put("category", JsonValue{ .string = "console" });
        try body.put("output", JsonValue{ .string = try allocator.dupe(u8, self.output_buffer.items) });

        try self.sendEvent("output", JsonValue{ .object = body }, allocator);

        // Clear the buffer
        self.output_buffer.clearRetainingCapacity();
    }
};

// =========================================================================
// Compilation Helper
// =========================================================================

fn compileSource(allocator: Allocator, path: []const u8, source: []const u8) !Module {
    // Determine language from extension
    const is_dbl = std.mem.endsWith(u8, path, ".dbl") or std.mem.endsWith(u8, path, ".dbo");

    // Get directory of the source file for import resolution
    const base_path = std.fs.path.dirname(path);

    if (is_dbl) {
        // DBL compilation path - use the high-level compileToModule function
        const dbl = @import("dbl");
        return dbl.compileToModuleWithPath(allocator, source, "debug", base_path);
    } else {
        // Cot compilation path
        var lexer = cot.lexer.Lexer.init(source);
        const tokens = try lexer.tokenize(allocator);
        defer allocator.free(tokens);

        var strings = cot.base.StringInterner.init(allocator);
        defer strings.deinit();

        var store = cot.ast.NodeStore.init(allocator, &strings);
        defer store.deinit();

        var parser = cot.parser.Parser.init(allocator, tokens, &store, &strings);
        defer parser.deinit();

        const top_level = parser.parse() catch return error.ParseError;
        defer allocator.free(top_level);

        // Lower to IR
        const ir_module = try cot.ir_lower.lower(allocator, &store, &strings, top_level, "debug");
        defer allocator.destroy(ir_module);
        defer ir_module.deinit();

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
        defer emitter.deinit();

        return try emitter.emit(ir_module);
    }
}
