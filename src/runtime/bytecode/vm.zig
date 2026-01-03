//! Cot Virtual Machine
//!
//! Register-based bytecode interpreter for compiled Cot programs.

const std = @import("std");
const builtin = @import("builtin");
const Opcode = @import("opcodes.zig").Opcode;
const opcodes = @import("opcodes.zig");
const Module = @import("module.zig").Module;
const Constant = @import("module.zig").Constant;
const debug = @import("../debug.zig");
const Debugger = @import("../debugger.zig").Debugger;
const trace_mod = @import("../trace/trace.zig");
pub const Tracer = trace_mod.Tracer;
const value_mod = @import("value.zig");
const Profiler = @import("profiler.zig").Profiler;
const JITProfiler = @import("profiler.zig").JITProfiler;
const CompilationTier = @import("profiler.zig").CompilationTier;

// Scoped logging (Ghostty pattern) - enable with std_options or runtime filter
const log = std.log.scoped(.@"vm-exec");

// Import extracted types (following Ghostty pattern)
const vm_types = @import("vm_types.zig");
pub const DispatchResult = vm_types.DispatchResult;
pub const InlineCache = vm_types.InlineCache;
pub const VMError = vm_types.VMError;
pub const RuntimeError = vm_types.RuntimeError;
pub const CrashContext = vm_types.CrashContext;
pub const CallFrame = vm_types.CallFrame;
pub const CallStack = vm_types.CallStack;
pub const StopReason = vm_types.StopReason;

// Get profiling setting from build options (if available)
const build_options = @import("build_options");
const profiling_enabled = if (@hasDecl(build_options, "enable_profiling")) build_options.enable_profiling else false;
const jit_profiling_enabled = if (@hasDecl(build_options, "enable_jit_profiling")) build_options.enable_jit_profiling else true;

// NaN-boxed Value type and related types
pub const Value = value_mod.Value;
pub const Record = value_mod.Record;
pub const Tag = value_mod.Tag;
pub const Decimal = value_mod.Decimal;
pub const OrderedMap = value_mod.OrderedMap;

// Native dependencies (WASM support removed)
const cotdb = @import("cotdb");
const native = @import("../native/native.zig");
// Note: symtable moved to extensions/dbl/

// Extension system
const TypeRegistry = @import("../type_registry.zig").TypeRegistry;
const OpcodeRegistry = @import("../opcode_registry.zig").OpcodeRegistry;
const extension_manager = @import("../extension_manager.zig");
const ExtensionManager = extension_manager.ExtensionManager;
const ExtensionContext = extension_manager.ExtensionContext;
const Extension = extension_manager.Extension;

// Extracted opcode handlers
const vm_opcodes = @import("vm_opcodes.zig");

/// Maximum stack size
const STACK_SIZE = 4096;

/// Maximum call depth (imported from vm_types)
const MAX_CALL_DEPTH = vm_types.MAX_CALL_DEPTH;

/// Opcode handler function type for dispatch table
/// Note: This must stay in vm.zig because it references VM
pub const OpcodeHandler = *const fn (*VM, *const Module) VMError!DispatchResult;


/// Global last error - set whenever a VMError is raised
/// This provides rich context even when only VMError is returned
pub var last_error: ?RuntimeError = null;

/// Static buffer for dynamically formatted error messages
/// This ensures the message slice remains valid after the function returns
var error_message_buf: [128]u8 = undefined;

/// Global crash context accessible from signal handlers
pub var crash_context: CrashContext = .{};

/// Helper to enter native context - use with @src()
pub fn enterNativeContext(context: []const u8, src: std.builtin.SourceLocation) void {
    crash_context.enterNative(context, src.file, src.line);
}

/// Helper to exit native context
pub fn exitNativeContext() void {
    crash_context.exitNative();
}

// Value and Record are now imported from value.zig (NaN-boxed implementation)

/// Virtual Machine
pub const VM = struct {
    allocator: std.mem.Allocator,

    // Execution state
    ip: usize,
    sp: usize,
    fp: usize,

    // Memory
    stack: [STACK_SIZE]Value,
    globals: std.ArrayList(Value),
    global_buffers: std.ArrayList([]u8), // For alpha/string globals
    heap: std.heap.ArenaAllocator,

    // Call stack
    call_stack: CallStack,

    // I/O
    stdout: std.fs.File,

    // Native function registry
    native_registry: native.NativeRegistry,
    cursor_manager: native.CursorManager, // CotDB cursor management for I/O
    channel_manager: ?native.ChannelManager, // Unified text + ISAM channels

    // Extension system
    type_registry: TypeRegistry,
    opcode_registry: OpcodeRegistry,
    extension_manager: ExtensionManager,

    // Modules
    current_module: ?*const Module,
    current_module_index: ?u16, // Index into native_registry.module_globals (null = main module)
    modules: std.ArrayList(*const Module),

    // Error handling
    error_handler_ip: ?u32, // Absolute IP of error handler (null = no handler)
    last_error: ?VMError, // Last error that occurred (for %ERROR function)

    // Debugger support
    debugger: Debugger,
    stop_reason: ?StopReason,
    debug_current_line: u32,

    // Execution tracer (optional, for `cot trace` command)
    tracer: ?*Tracer = null,

    // Profiler (only present when profiling is enabled)
    profiler: if (profiling_enabled) Profiler else void,

    // JIT Profiler for function call/loop tracking (for future JIT compilation)
    jit_profiler: if (jit_profiling_enabled) JITProfiler else void,

    // Register file for register-based bytecode execution
    // 16 virtual registers: r0-r7 (caller-saved), r8-r13 (callee-saved), r14 (fp), r15 (return)
    registers: [16]Value,

    // Inline caches for field access optimization
    // Key: instruction IP, Value: cached type/offset
    inline_caches: std.AutoHashMap(u32, InlineCache),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .ip = 0,
            .sp = 0,
            .fp = 0,
            .stack = undefined,
            .globals = .{},
            .global_buffers = .{},
            .heap = std.heap.ArenaAllocator.init(allocator),
            .call_stack = .{},
            .stdout = std.fs.File.stdout(),
            .native_registry = native.NativeRegistry.init(allocator),
            .cursor_manager = native.CursorManager.init(allocator),
            .channel_manager = null, // Initialized via initChannels()
            .type_registry = TypeRegistry.init(allocator),
            .opcode_registry = OpcodeRegistry.init(),
            .extension_manager = ExtensionManager.init(allocator),
            .current_module = null,
            .current_module_index = null,
            .modules = .{},
            .error_handler_ip = null,
            .last_error = null,
            .debugger = Debugger.init(allocator),
            .stop_reason = null,
            .debug_current_line = 0,
            .profiler = if (profiling_enabled) Profiler.init(allocator) else {},
            .jit_profiler = if (jit_profiling_enabled) JITProfiler.init(allocator) else {},
            .registers = [_]Value{Value.null_val} ** 16,
            .inline_caches = std.AutoHashMap(u32, InlineCache).init(allocator),
        };
    }

    /// Get allocator for Value heap allocations (StringRef, Decimal, etc.)
    /// Uses arena allocator so all values are freed together at VM.deinit
    pub fn valueAllocator(self: *Self) std.mem.Allocator {
        return self.heap.allocator();
    }

    /// Clear inline caches (call when module is reloaded)
    pub fn clearInlineCaches(self: *Self) void {
        self.inline_caches.clearRetainingCapacity();
    }

    /// Get inline cache statistics for profiling
    pub fn getInlineCacheStats(self: *Self) struct { total: u32, hits: u64, misses: u64 } {
        var total: u32 = 0;
        var hits: u64 = 0;
        var misses: u64 = 0;
        var iter = self.inline_caches.valueIterator();
        while (iter.next()) |cache| {
            total += 1;
            hits += cache.hit_count;
            misses += cache.miss_count;
        }
        return .{ .total = total, .hits = hits, .misses = misses };
    }

    /// Get JIT profiler statistics (if JIT profiling is enabled)
    pub fn getJITStats(self: *Self) ?struct { functions: u32, baseline_queue: usize, optimized_queue: usize, deopts: u32 } {
        if (!jit_profiling_enabled) return null;
        return .{
            .functions = self.jit_profiler.total_functions,
            .baseline_queue = self.jit_profiler.baseline_queue.items.len,
            .optimized_queue = self.jit_profiler.optimized_queue.items.len,
            .deopts = self.jit_profiler.total_deopts,
        };
    }

    /// Print JIT profiler report to stderr (if JIT profiling is enabled)
    pub fn printJITReport(self: *Self) void {
        if (!jit_profiling_enabled) return;
        const stderr_file = std.fs.File{ .handle = std.posix.STDERR_FILENO };
        var buffer: [4096]u8 = undefined;
        self.jit_profiler.report(stderr_file.writer(&buffer)) catch {};
    }

    /// Check if JIT profiling is enabled (for extracted opcode handlers)
    pub fn jitProfilingEnabled(self: *Self) bool {
        _ = self;
        return jit_profiling_enabled;
    }

    /// Record a JIT call (for extracted opcode handlers)
    pub fn recordJITCall(self: *Self, module_idx: u16, routine_idx: u16) ?CompilationTier {
        if (!jit_profiling_enabled) return null;
        return self.jit_profiler.recordCall(module_idx, routine_idx);
    }

    /// Initialize the unified channel manager for text + ISAM I/O
    pub fn initChannels(self: *Self) !void {
        if (self.channel_manager != null) return; // Already initialized
        self.channel_manager = try native.ChannelManager.init(self.allocator, &self.cursor_manager);
    }

    // ========================================================================
    // Extension System
    // ========================================================================

    /// Get an extension context for loading extensions or registering types/opcodes
    pub fn extensionContext(self: *Self) ExtensionContext {
        return .{
            .vm = self,
            .allocator = self.allocator,
            .extension_name = "",
            .type_registry = &self.type_registry,
            .opcode_registry = &self.opcode_registry,
            .native_registry = &self.native_registry,
        };
    }

    /// Load an extension into the VM
    pub fn loadExtension(self: *Self, ext: Extension) !void {
        var ctx = self.extensionContext();
        try self.extension_manager.load(ext, &ctx);
    }

    /// Check if an extension is loaded
    pub fn isExtensionLoaded(self: *Self, name: []const u8) bool {
        return self.extension_manager.isLoaded(name);
    }

    /// Look up a type by ID (for extension objects)
    pub fn lookupType(self: *Self, type_id: u16) ?*const TypeRegistry.TypeDef {
        return self.type_registry.lookup(type_id);
    }

    // =========================================================================
    // Execution Tracing
    // =========================================================================

    /// Attach a tracer for execution tracing
    pub fn setTracer(self: *Self, tracer: *Tracer) void {
        self.tracer = tracer;
        tracer.start();
    }

    /// Detach tracer and stop tracing
    pub fn detachTracer(self: *Self) ?*Tracer {
        const t = self.tracer;
        if (t) |tracer| {
            tracer.stop();
        }
        self.tracer = null;
        return t;
    }

    /// Check if tracing is active
    pub fn isTracing(self: *Self) bool {
        return self.tracer != null;
    }

    /// Dump trace history to writer (for crash dumps)
    pub fn dumpTraceHistory(self: *Self, writer: anytype) !void {
        if (self.tracer) |tracer| {
            try tracer.dumpHistory(writer);
        }
    }

    pub fn deinit(self: *Self) void {
        // Free global buffers (the actual string data that FixedStringRef points to)
        for (self.global_buffers.items) |buf| {
            self.allocator.free(buf);
        }
        self.global_buffers.deinit(self.allocator);

        self.globals.deinit(self.allocator);

        // Free all Value heap allocations (StringRef, Decimal, etc.) at once
        // Values use valueAllocator() which returns heap.allocator()
        // This avoids tracking individual allocations or dealing with shared pointers
        self.heap.deinit();
        self.modules.deinit(self.allocator);
        self.native_registry.deinit();

        // Clean up extension system (reverse order of init)
        // Note: symtable cleanup is handled by DBL extension deinit
        var ext_ctx = ExtensionContext{
            .vm = self,
            .allocator = self.allocator,
            .extension_name = "",
            .type_registry = &self.type_registry,
            .opcode_registry = &self.opcode_registry,
            .native_registry = &self.native_registry,
        };
        self.extension_manager.deinit(&ext_ctx);
        self.type_registry.deinit();
        if (self.channel_manager) |*cm| cm.deinit();
        self.cursor_manager.deinit();
        self.debugger.deinit();
        if (profiling_enabled) {
            self.profiler.deinit();
        }
        if (jit_profiling_enabled) {
            self.jit_profiler.deinit();
        }
        self.inline_caches.deinit();
    }

    /// Get current execution location for error reporting
    pub fn getLocation(self: *Self) struct { ip: u32, routine: ?[]const u8 } {
        const module = self.current_module orelse return .{ .ip = @intCast(self.ip), .routine = null };

        // Find which routine we're in based on IP
        var current_routine: ?[]const u8 = null;
        for (module.routines) |routine| {
            if (self.ip >= routine.code_offset) {
                // Get routine name from constants
                if (routine.name_index < module.constants.len) {
                    const c = module.constants[routine.name_index];
                    if (c == .identifier) {
                        current_routine = c.identifier;
                    }
                }
            }
        }

        return .{ .ip = @intCast(self.ip), .routine = current_routine };
    }

    /// Create a rich error with full context and return the VMError
    /// This should be used instead of bare `return VMError.X` to capture context
    pub fn fail(self: *Self, err: VMError, message: []const u8) VMError {
        return self.failWithDetail(err, message, null);
    }

    /// Create a rich error with full context and additional detail
    pub fn failWithDetail(self: *Self, err: VMError, message: []const u8, detail: ?[]const u8) VMError {
        const loc = self.getLocation();
        last_error = RuntimeError{
            .kind = err,
            .message = message,
            .ip = @intCast(self.ip),
            .opcode = crash_context.last_opcode,
            .source_line = self.debug_current_line,
            .routine_name = loc.routine orelse "<unknown>",
            .stack_depth = @intCast(self.sp),
            .detail = detail,
        };
        return err;
    }

    /// Safe instruction fetch - returns error instead of potential segfault
    /// Use these "fetch" methods instead of direct code[] access
    fn fetchByte(self: *Self) VMError!u8 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip >= module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "IP exceeded code length");
        }
        const byte = module.code[self.ip];
        self.ip += 1;
        return byte;
    }

    /// Safe fetch for u16 operand
    fn fetchU16(self: *Self) VMError!u16 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip + 2 > module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "Not enough bytes for u16 operand");
        }
        const bytes = module.code[self.ip..][0..2];
        self.ip += 2;
        return std.mem.readInt(u16, bytes, .little);
    }

    /// Safe fetch for i16 operand
    fn fetchI16(self: *Self) VMError!i16 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip + 2 > module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "Not enough bytes for i16 operand");
        }
        const bytes = module.code[self.ip..][0..2];
        self.ip += 2;
        return std.mem.readInt(i16, bytes, .little);
    }

    /// Safe fetch for u32 operand
    fn fetchU32(self: *Self) VMError!u32 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip + 4 > module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "Not enough bytes for u32 operand");
        }
        const bytes = module.code[self.ip..][0..4];
        self.ip += 4;
        return std.mem.readInt(u32, bytes, .little);
    }

    /// Safe fetch for i32 operand
    fn fetchI32(self: *Self) VMError!i32 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip + 4 > module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "Not enough bytes for i32 operand");
        }
        const bytes = module.code[self.ip..][0..4];
        self.ip += 4;
        return std.mem.readInt(i32, bytes, .little);
    }

    /// Safe fetch for i64 operand
    fn fetchI64(self: *Self) VMError!i64 {
        const module = self.current_module orelse return self.fail(VMError.InvalidRoutine, "No module loaded");
        if (self.ip + 8 > module.code.len) {
            return self.fail(VMError.BytecodeOutOfBounds, "Not enough bytes for i64 operand");
        }
        const bytes = module.code[self.ip..][0..8];
        self.ip += 8;
        return std.mem.readInt(i64, bytes, .little);
    }

    /// Get callstack for error reporting
    pub fn getCallstack(self: *Self, allocator: std.mem.Allocator) ![][]const u8 {
        var stack = std.ArrayListAligned([]const u8, null).empty;
        errdefer stack.deinit(allocator);

        // Add current location
        const loc = self.getLocation();
        if (loc.routine) |name| {
            const entry = try std.fmt.allocPrint(allocator, "{s} @ 0x{x:0>4}", .{ name, loc.ip });
            try stack.append(allocator, entry);
        }

        // Add call stack frames
        for (self.call_stack.slice()) |frame| {
            var frame_name: []const u8 = "unknown";
            if (frame.routine_index < frame.module.routines.len) {
                const routine = frame.module.routines[frame.routine_index];
                if (routine.name_index < frame.module.constants.len) {
                    const c = frame.module.constants[routine.name_index];
                    if (c == .identifier) {
                        frame_name = c.identifier;
                    }
                }
            }
            const entry = try std.fmt.allocPrint(allocator, "{s} @ 0x{x:0>4}", .{ frame_name, frame.return_ip });
            try stack.append(allocator, entry);
        }

        return stack.toOwnedSlice(allocator);
    }

    /// Load a module
    pub fn loadModule(self: *Self, module: *const Module) !void {
        try self.modules.append(self.allocator, module);
    }

    /// Execute a module
    pub fn execute(self: *Self, module: *const Module) VMError!void {
        // Clear any previous error
        last_error = null;

        self.current_module = module;

        debug.section(.vm, "VM Execution");
        debug.print(.vm, "Code size: {d} bytes", .{module.code.len});
        debug.print(.vm, "Constants: {d}", .{module.constants.len});
        debug.print(.vm, "Routines: {d}", .{module.routines.len});

        // Debug: print first 20 bytes of code
        if (module.code.len >= 20) {
            debug.print(.vm, "First 20 code bytes: {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2}", .{
                module.code[0], module.code[1], module.code[2], module.code[3],
                module.code[4], module.code[5], module.code[6], module.code[7],
                module.code[8], module.code[9], module.code[10], module.code[11],
                module.code[12], module.code[13], module.code[14], module.code[15],
                module.code[16], module.code[17], module.code[18], module.code[19],
            });
        }

        // Find entry point
        if (module.header.entry_point == 0xFFFFFFFF) {
            debug.print(.vm, "Error: No entry point (library module)", .{});
            return self.fail(VMError.InvalidRoutine, "No entry point - this is a library module");
        }

        self.ip = module.header.entry_point;
        debug.print(.vm, "Entry point: 0x{x:0>4}", .{self.ip});

        // Find the entry point routine and reserve space for local variables
        for (module.routines) |routine| {
            if (routine.code_offset == module.header.entry_point) {
                // Bounds check on local_count to prevent stack overflow
                if (routine.local_count > STACK_SIZE) {
                    debug.print(.vm, "Error: Entry routine local_count ({d}) exceeds stack size ({d})", .{ routine.local_count, STACK_SIZE });
                    return self.fail(VMError.StackOverflow, "Entry routine requires more locals than stack can hold");
                }
                // Reserve space for local variables and initialize them to 0
                // Locals are stored at stack[fp+0], stack[fp+1], etc.
                for (0..routine.local_count) |i| {
                    self.stack[i] = Value.initInt(0);
                }
                self.sp = routine.local_count;
                debug.print(.vm, "Entry routine local_count: {d}", .{routine.local_count});
                break;
            }
        }

        debug.separator(.vm);
        try self.run();
        debug.print(.vm, "Execution complete", .{});
    }

    /// Get the last runtime error with full context (if any)
    pub fn getLastError() ?RuntimeError {
        return last_error;
    }

    /// Format the last error for display to the user
    pub fn formatLastError(allocator: std.mem.Allocator) !?[]u8 {
        const err = last_error orelse return null;
        var buffer = std.ArrayList(u8).init(allocator);
        try err.format("", .{}, buffer.writer());
        return buffer.toOwnedSlice();
    }

    /// Print last error to stderr (convenience function)
    /// Note: This is user-facing output, so it always prints (not debug-mode dependent)
    pub fn printLastError() void {
        if (last_error) |err| {
            std.debug.print(
                \\Runtime Error: {s}
                \\  Message: {s}
                \\  Location: {s} line {d} (IP: 0x{X:0>4})
                \\  Opcode: {s}
                \\  Stack depth: {d}
                \\
            , .{
                @errorName(err.kind),
                err.message,
                err.routine_name,
                err.source_line,
                err.ip,
                if (err.opcode) |op| @tagName(op) else "<unknown>",
                err.stack_depth,
            });
            if (err.detail) |d| {
                std.debug.print("  Detail: {s}\n", .{d});
            }
        }
    }

    // =========================================================================
    // Profiling API (only available when built with -Dprofiling=true)
    // =========================================================================

    /// Start profiling session (marks start time)
    pub fn startProfiling(self: *Self) void {
        if (profiling_enabled) {
            self.profiler.start();
        }
    }

    /// Stop profiling session (marks end time)
    pub fn stopProfiling(self: *Self) void {
        if (profiling_enabled) {
            self.profiler.stop();
        }
    }

    /// Reset all profiling counters
    pub fn resetProfiling(self: *Self) void {
        if (profiling_enabled) {
            self.profiler.reset();
        }
    }

    /// Write profiling report to stderr
    pub fn dumpProfilingReport(self: *Self) void {
        if (profiling_enabled) {
            const stderr_file = std.fs.File{ .handle = std.posix.STDERR_FILENO };
            var buffer: [4096]u8 = undefined;
            self.profiler.report(stderr_file.writer(&buffer)) catch {};
        }
    }

    /// Write profiling report to a specific writer
    pub fn writeProfilingReport(self: *Self, writer: anytype) !void {
        if (profiling_enabled) {
            try self.profiler.report(writer);
        }
    }

    /// Export profiling data as JSON
    pub fn exportProfilingJson(self: *Self, writer: anytype) !void {
        if (profiling_enabled) {
            try self.profiler.exportJson(writer);
        }
    }

    /// Check if profiling is enabled at compile time
    pub fn isProfilingEnabled() bool {
        return profiling_enabled;
    }

    /// Get total instructions executed (for profiling)
    pub fn getProfilingInstructionCount(self: *Self) u64 {
        if (profiling_enabled) {
            return self.profiler.total_instructions;
        }
        return 0;
    }

    /// Execute until a stop condition (breakpoint, step, completion)
    /// Used by debuggers for controlled execution
    pub fn runUntilStop(self: *Self) VMError!StopReason {
        self.stop_reason = null;
        self.debugger.waiting_for_command = false;

        self.run() catch |err| {
            self.stop_reason = .err;
            return err;
        };

        return self.stop_reason orelse .completed;
    }

    /// Continue execution after a stop (for debugger use)
    pub fn continueExecution(self: *Self) VMError!StopReason {
        self.debugger.continue_();
        return self.runUntilStop();
    }

    /// Step to next line (step over)
    pub fn stepOver(self: *Self) VMError!StopReason {
        self.debugger.stepOver(self.call_stack.len);
        return self.runUntilStop();
    }

    /// Step into function calls
    pub fn stepInto(self: *Self) VMError!StopReason {
        self.debugger.stepInto();
        return self.runUntilStop();
    }

    /// Step out of current function
    pub fn stepOut(self: *Self) VMError!StopReason {
        self.debugger.stepOut(self.call_stack.len);
        return self.runUntilStop();
    }

    /// Get current source line (from last debug_line opcode)
    pub fn getCurrentLine(self: *Self) u32 {
        return self.debug_current_line;
    }

    /// Get local variable count for current frame
    pub fn getLocalCount(self: *Self) usize {
        const module = self.current_module orelse return 0;
        // Find current routine by IP
        for (module.routines) |routine| {
            if (self.ip >= routine.code_offset and
                self.ip < routine.code_offset + routine.code_length)
            {
                return routine.local_count;
            }
        }
        return 0;
    }

    /// Get local variable value by slot index
    pub fn getLocal(self: *Self, slot: usize) ?Value {
        if (self.fp + slot >= self.sp) return null;
        return self.stack[self.fp + slot];
    }

    /// Format a value as a string for debugging
    pub fn formatValue(self: *Self, value: Value, buf: []u8) []const u8 {
        _ = self;
        var stream = std.io.fixedBufferStream(buf);
        const writer = stream.writer();
        value.format("", .{}, writer) catch return "<error>";
        return buf[0..stream.pos];
    }

    /// Get the type name for a value
    pub fn getValueType(val: Value) []const u8 {
        return val.tag().name();
    }

    // ============================================
    // String Helper Functions
    // ============================================

    /// Convert a Value to its string representation for auto-coercion
    /// Uses the provided buffer for numeric conversions, returns slice into buffer or string pointer
    pub fn valueToStringSlice(self: *Self, val: Value, buf: *[32]u8) []const u8 {
        _ = self;
        switch (val.tag()) {
            .string, .fixed_string => return val.asString(),
            .integer => {
                const int_val = val.asInt();
                return std.fmt.bufPrint(buf, "{d}", .{int_val}) catch "";
            },
            .decimal => {
                if (val.asDecimal()) |dval| {
                    const divisor = std.math.pow(i64, 10, dval.precision);
                    const whole = @divTrunc(dval.value, divisor);
                    const frac = @abs(@rem(dval.value, divisor));
                    if (dval.precision > 0) {
                        return std.fmt.bufPrint(buf, "{d}.{d}", .{ whole, frac }) catch "";
                    } else {
                        return std.fmt.bufPrint(buf, "{d}", .{whole}) catch "";
                    }
                }
                return "";
            },
            .boolean => {
                const bool_val = val.asBool();
                return if (bool_val) "1" else "0";
            },
            .null_val => return "",
            .record_ref => return "<record>",
            .handle => return "<handle>",
            .object => return "<object>",
        }
    }

    // ============================================
    // Dispatch Table (Computed Goto)
    // ============================================

    /// Comptime-initialized dispatch table mapping opcode bytes to handlers
    const dispatch_table: [256]OpcodeHandler = init: {
        var table: [256]OpcodeHandler = .{&vm_opcodes.op_invalid} ** 256;

        // Core (0x00-0x0F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.nop)] = &vm_opcodes.op_nop;
        table[@intFromEnum(Opcode.halt)] = &vm_opcodes.op_halt;

        // Register Moves (0x10-0x1F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.mov)] = &vm_opcodes.op_mov;
        table[@intFromEnum(Opcode.movi)] = &vm_opcodes.op_movi;
        table[@intFromEnum(Opcode.movi16)] = &vm_opcodes.op_movi16;
        table[@intFromEnum(Opcode.movi32)] = &vm_opcodes.op_movi32;
        table[@intFromEnum(Opcode.load_const)] = &vm_opcodes.op_load_const;
        table[@intFromEnum(Opcode.load_null)] = &vm_opcodes.op_load_null;
        table[@intFromEnum(Opcode.load_true)] = &vm_opcodes.op_load_true;
        table[@intFromEnum(Opcode.load_false)] = &vm_opcodes.op_load_false;

        // Local/Global (0x20-0x2F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.load_local)] = &vm_opcodes.op_load_local;
        table[@intFromEnum(Opcode.store_local)] = &vm_opcodes.op_store_local;
        table[@intFromEnum(Opcode.load_local16)] = &vm_opcodes.op_load_local16;
        table[@intFromEnum(Opcode.store_local16)] = &vm_opcodes.op_store_local16;
        table[@intFromEnum(Opcode.load_global)] = &vm_opcodes.op_load_global;
        table[@intFromEnum(Opcode.store_global)] = &vm_opcodes.op_store_global;

        // Arithmetic (0x30-0x3F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.add)] = &vm_opcodes.op_add;
        table[@intFromEnum(Opcode.sub)] = &vm_opcodes.op_sub;
        table[@intFromEnum(Opcode.mul)] = &vm_opcodes.op_mul;
        table[@intFromEnum(Opcode.div)] = &vm_opcodes.op_div;
        table[@intFromEnum(Opcode.mod)] = &vm_opcodes.op_mod;
        table[@intFromEnum(Opcode.neg)] = &vm_opcodes.op_neg;
        table[@intFromEnum(Opcode.addi)] = &vm_opcodes.op_addi;
        table[@intFromEnum(Opcode.subi)] = &vm_opcodes.op_subi;
        table[@intFromEnum(Opcode.muli)] = &vm_opcodes.op_muli;
        table[@intFromEnum(Opcode.incr)] = &vm_opcodes.op_incr;
        table[@intFromEnum(Opcode.decr)] = &vm_opcodes.op_decr;

        // Comparison (0x40-0x4F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.cmp_eq)] = &vm_opcodes.op_cmp_eq;
        table[@intFromEnum(Opcode.cmp_ne)] = &vm_opcodes.op_cmp_ne;
        table[@intFromEnum(Opcode.cmp_lt)] = &vm_opcodes.op_cmp_lt;
        table[@intFromEnum(Opcode.cmp_le)] = &vm_opcodes.op_cmp_le;
        table[@intFromEnum(Opcode.cmp_gt)] = &vm_opcodes.op_cmp_gt;
        table[@intFromEnum(Opcode.cmp_ge)] = &vm_opcodes.op_cmp_ge;
        // String comparison
        table[@intFromEnum(Opcode.cmp_str_eq)] = &vm_opcodes.op_cmp_str_eq;
        table[@intFromEnum(Opcode.cmp_str_lt)] = &vm_opcodes.op_cmp_str_lt;
        table[@intFromEnum(Opcode.cmp_str_ne)] = &vm_opcodes.op_cmp_str_ne;
        table[@intFromEnum(Opcode.cmp_str_le)] = &vm_opcodes.op_cmp_str_le;
        table[@intFromEnum(Opcode.cmp_str_gt)] = &vm_opcodes.op_cmp_str_gt;
        table[@intFromEnum(Opcode.cmp_str_ge)] = &vm_opcodes.op_cmp_str_ge;

        // Logical (0x50-0x5F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.log_and)] = &vm_opcodes.op_log_and;
        table[@intFromEnum(Opcode.log_or)] = &vm_opcodes.op_log_or;
        table[@intFromEnum(Opcode.log_not)] = &vm_opcodes.op_log_not;
        table[@intFromEnum(Opcode.is_null)] = &vm_opcodes.op_is_null;
        table[@intFromEnum(Opcode.select)] = &vm_opcodes.op_select;
        table[@intFromEnum(Opcode.ptr_offset)] = &vm_opcodes.op_ptr_offset;

        // Control Flow (0x60-0x6F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.jmp)] = &vm_opcodes.op_jmp;
        table[@intFromEnum(Opcode.jz)] = &vm_opcodes.op_jz;
        table[@intFromEnum(Opcode.jnz)] = &vm_opcodes.op_jnz;
        table[@intFromEnum(Opcode.loop_start)] = &vm_opcodes.op_loop_nop;
        table[@intFromEnum(Opcode.loop_end)] = &vm_opcodes.op_loop_nop;
        table[@intFromEnum(Opcode.clear_error_handler)] = &vm_opcodes.op_loop_nop;

        // Function Calls (0x70-0x7F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.call)] = &vm_opcodes.op_call;
        table[@intFromEnum(Opcode.call_dynamic)] = &vm_opcodes.op_call_dynamic;
        table[@intFromEnum(Opcode.ret)] = &vm_opcodes.op_ret;
        table[@intFromEnum(Opcode.ret_val)] = &vm_opcodes.op_ret_val;

        // Record/Field Operations (0x80-0x8F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.load_record_buf)] = &vm_opcodes.op_load_record_buf;
        table[@intFromEnum(Opcode.store_record_buf)] = &vm_opcodes.op_store_record_buf;

        // String Operations (0x90-0x9F) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.str_concat)] = &vm_opcodes.op_str_concat;

        // Type Conversion (0xA0-0xAF) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.format_decimal)] = &vm_opcodes.op_format_decimal;
        table[@intFromEnum(Opcode.parse_decimal)] = &vm_opcodes.op_parse_decimal;

        // Math Functions (0xC0-0xCF) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.fn_round)] = &vm_opcodes.op_fn_round;
        table[@intFromEnum(Opcode.fn_trunc)] = &vm_opcodes.op_fn_trunc;

        // Console I/O (0xD0-0xD4) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.console_writeln)] = &vm_opcodes.op_console_writeln;
        table[@intFromEnum(Opcode.console_write)] = &vm_opcodes.op_console_write;
        table[@intFromEnum(Opcode.console_log)] = &vm_opcodes.op_console_log;

        // Map Operations (0xD5-0xDF) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.map_new)] = &vm_opcodes.op_map_new;
        table[@intFromEnum(Opcode.map_set)] = &vm_opcodes.op_map_set;
        table[@intFromEnum(Opcode.map_get)] = &vm_opcodes.op_map_get;
        table[@intFromEnum(Opcode.map_delete)] = &vm_opcodes.op_map_delete;
        table[@intFromEnum(Opcode.map_has)] = &vm_opcodes.op_map_has;
        table[@intFromEnum(Opcode.map_len)] = &vm_opcodes.op_map_len;
        table[@intFromEnum(Opcode.map_clear)] = &vm_opcodes.op_map_clear;
        table[@intFromEnum(Opcode.map_keys)] = &vm_opcodes.op_map_keys;
        table[@intFromEnum(Opcode.map_values)] = &vm_opcodes.op_map_values;
        table[@intFromEnum(Opcode.map_get_at)] = &vm_opcodes.op_map_get_at;
        table[@intFromEnum(Opcode.map_set_at)] = &vm_opcodes.op_map_set_at;

        // Debug (0xF0-0xFF) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.debug_break)] = &vm_opcodes.op_debug_break;
        table[@intFromEnum(Opcode.debug_line)] = &vm_opcodes.op_debug_line;
        table[@intFromEnum(Opcode.assert)] = &vm_opcodes.op_assert;

        // Quickened/Specialized Opcodes (0xE0-0xEB) - extracted to vm_opcodes.zig
        table[@intFromEnum(Opcode.add_int)] = &vm_opcodes.op_add_int;
        table[@intFromEnum(Opcode.sub_int)] = &vm_opcodes.op_sub_int;
        table[@intFromEnum(Opcode.mul_int)] = &vm_opcodes.op_mul_int;
        table[@intFromEnum(Opcode.div_int)] = &vm_opcodes.op_div_int;
        table[@intFromEnum(Opcode.cmp_lt_int)] = &vm_opcodes.op_cmp_lt_int;
        table[@intFromEnum(Opcode.cmp_le_int)] = &vm_opcodes.op_cmp_le_int;
        table[@intFromEnum(Opcode.cmp_gt_int)] = &vm_opcodes.op_cmp_gt_int;
        table[@intFromEnum(Opcode.cmp_ge_int)] = &vm_opcodes.op_cmp_ge_int;
        table[@intFromEnum(Opcode.cmp_eq_int)] = &vm_opcodes.op_cmp_eq_int;
        table[@intFromEnum(Opcode.cmp_ne_int)] = &vm_opcodes.op_cmp_ne_int;
        table[@intFromEnum(Opcode.incr_int)] = &vm_opcodes.op_incr_int;
        table[@intFromEnum(Opcode.decr_int)] = &vm_opcodes.op_decr_int;

        break :init table;
    };

    /// Main execution loop using dispatch table (Computed Goto style)
    fn runDispatchTable(self: *Self) VMError!void {
        // Mark VM as active for crash reporting
        crash_context.active = true;
        defer {
            crash_context.active = false;
        }

        while (true) {
            const module = self.current_module orelse return VMError.InvalidRoutine;

            if (self.ip >= module.code.len) break;

            const opcode_byte = module.code[self.ip];

            // Update crash context
            crash_context.ip = @intCast(self.ip);
            crash_context.last_opcode = @enumFromInt(opcode_byte);
            crash_context.source_line = self.debug_current_line;

            // Trace hook (before opcode execution)
            if (self.tracer) |tracer| {
                tracer.onOpcode(
                    @intCast(self.ip),
                    @enumFromInt(opcode_byte),
                    self.debug_current_line,
                    self.registers,
                );
            }

            // Record for profiling
            if (profiling_enabled) {
                self.profiler.recordOpcode(@enumFromInt(opcode_byte));
            }

            self.ip += 1;

            // Indexed dispatch - no switch, no enum conversion
            const result = dispatch_table[opcode_byte](self, module) catch |err| {
                // Trace error if tracer is attached
                if (self.tracer) |tracer| {
                    tracer.onError(
                        @intCast(self.ip),
                        self.debug_current_line,
                        err,
                        @errorName(err),
                    );
                }
                return err;
            };

            switch (result) {
                .continue_dispatch => continue,
                .halt => return,
                .return_from_main => return,
            }
        }
    }

    /// Main execution loop - delegates to dispatch table implementation
    fn run(self: *Self) VMError!void {
        return self.runDispatchTable();
    }

    // Stack operations - with rich error context
    pub fn push(self: *Self, value: Value) VMError!void {
        if (self.sp >= STACK_SIZE) {
            @branchHint(.cold); // Error path is rare
            return self.fail(VMError.StackOverflow, "Stack overflow: maximum stack depth exceeded");
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *Self) VMError!Value {
        if (self.sp == 0) {
            @branchHint(.cold); // Error path is rare
            return self.fail(VMError.StackUnderflow, "Stack underflow: attempted to pop from empty stack");
        }
        self.sp -= 1;
        return self.stack[self.sp];
    }

    /// Convert a constant to a Value (for register-based instructions)
    pub fn constantToValue(self: *Self, constant: Constant) Value {
        return switch (constant) {
            .integer => |ival| Value.initInt(ival),
            .decimal => |dval| Value.initDecimal(self.valueAllocator(), dval.value, dval.precision) catch Value.null_val,
            .string => |sval| Value.initString(self.valueAllocator(), sval) catch Value.null_val,
            .fixed_string => |aval| Value.initFixedString(self.valueAllocator(), @constCast(aval.data)) catch Value.null_val,
            .identifier => |idval| Value.initString(self.valueAllocator(), idval) catch Value.null_val,
            else => Value.null_val,
        };
    }

    // Read helpers
    fn readU16(self: *Self, module: *const Module) u16 {
        const val = std.mem.readInt(u16, module.code[self.ip..][0..2], .little);
        self.ip += 2;
        return val;
    }

    fn readI16(self: *Self, module: *const Module) i16 {
        const val = std.mem.readInt(i16, module.code[self.ip..][0..2], .little);
        self.ip += 2;
        return val;
    }

    /// Read i16 at a specific offset from current IP (without advancing IP)
    fn readI16Offset(self: *Self, module: *const Module, offset: usize) i16 {
        return std.mem.readInt(i16, module.code[self.ip + offset ..][0..2], .little);
    }

    fn readI32(self: *Self, module: *const Module) i32 {
        const val = std.mem.readInt(i32, module.code[self.ip..][0..4], .little);
        self.ip += 4;
        return val;
    }

    fn readI64(self: *Self, module: *const Module) i64 {
        const val = std.mem.readInt(i64, module.code[self.ip..][0..8], .little);
        self.ip += 8;
        return val;
    }

    // Call routine
    fn callRoutine(self: *Self, routine_idx: u16, arg_count: u8) VMError!void {
        const module = self.current_module orelse return VMError.InvalidRoutine;
        const routine = module.getRoutine(routine_idx) orelse
            return VMError.InvalidRoutine;

        // Calculate caller's sp before args were pushed
        const caller_sp = self.sp - @as(u32, arg_count);

        // Save call frame
        self.call_stack.append(.{
            .module = module,
            .routine_index = routine_idx,
            .return_ip = self.ip,
            .base_pointer = self.fp,
            .stack_pointer = caller_sp,
            .caller_module_index = self.current_module_index,
        }) catch return VMError.CallStackOverflow;

        // Set up new frame
        // The args are already on the stack at sp-arg_count to sp-1
        // Set fp so that args become the first local slots (fp+0, fp+1, ...)
        self.fp = self.sp - @as(u32, arg_count);
        self.ip = routine.code_offset;

        // Reserve space for additional local variables beyond the parameters and initialize to 0
        if (routine.local_count > arg_count) {
            const extra_locals = routine.local_count - @as(u32, arg_count);
            for (0..extra_locals) |i| {
                self.stack[self.sp + i] = Value.initInt(0);
            }
            self.sp += extra_locals;
        }
    }

    fn executeLoadRecordBuf(self: *Self, module: *const Module, type_idx: u16, local_base: u16) VMError!void {
        debug.print(.vm, "LOAD_RECORD_BUF: type_idx={d} local_base={d} num_types={d}", .{ type_idx, local_base, module.types.len });

        // Get the type definition
        if (type_idx >= module.types.len) {
            debug.print(.vm, "LOAD_RECORD_BUF: invalid type_idx", .{});
            return VMError.InvalidType;
        }

        const type_def = &module.types[type_idx];

        // Allocate buffer for record
        const buf = self.allocator.alloc(u8, type_def.total_size) catch return VMError.OutOfMemory;
        @memset(buf, ' '); // Initialize with spaces
        try self.global_buffers.append(self.allocator, buf);

        // Fill buffer from LOCAL variables (not globals!)
        // Fields are stored in consecutive local slots starting at local_base
        // Locals are accessed via stack[fp + slot]
        for (type_def.fields, 0..) |field, field_idx| {
            const local_slot = local_base + @as(u16, @intCast(field_idx));
            const offset = field.offset;
            const end = @min(offset + field.size, type_def.total_size);
            const stack_index = self.fp + local_slot;

            debug.print(.vm, "LOAD_RECORD_BUF: field[{d}] local={d} stack_idx={d} offset={d} size={d}", .{ field_idx, local_slot, stack_index, offset, field.size });

            if (stack_index < STACK_SIZE) {
                const val = self.stack[stack_index];
                debug.print(.vm, "LOAD_RECORD_BUF: field val={any}", .{val});

                // Copy value into buffer at field offset
                switch (val.tag()) {
                    .fixed_string, .string => {
                        const a = val.asString();
                        const copy_len = @min(a.len, end - offset);
                        @memcpy(buf[offset .. offset + copy_len], a[0..copy_len]);
                    },
                    .integer => {
                        // Format integer into buffer with ZERO padding for decimal fields
                        var tmp: [32]u8 = undefined;
                        const str = std.fmt.bufPrint(&tmp, "{d}", .{val.asInt()}) catch continue;

                        // Check if this is a decimal field - zero-pad to field width
                        if (field.data_type == .decimal) {
                            // Zero-pad decimal field: first fill with zeros, then copy number right-aligned
                            @memset(buf[offset..end], '0');
                            if (str.len <= field.size) {
                                const start = offset + field.size - str.len;
                                @memcpy(buf[start..end], str);
                            } else {
                                // Number too big - copy what fits
                                @memcpy(buf[offset..end], str[0..field.size]);
                            }
                        } else {
                            // Right-align in field, pad with spaces on left for numeric
                            if (str.len <= field.size) {
                                const start = offset + field.size - str.len;
                                @memcpy(buf[start .. start + str.len], str);
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        debug.print(.vm, "LOAD_RECORD_BUF: result='{s}'", .{buf[0..@min(30, buf.len)]});
        const fixed_str = Value.initFixedString(self.valueAllocator(), buf) catch
            return VMError.OutOfMemory;
        try self.push(fixed_str);
    }

    fn executeStoreRecordBuf(self: *Self, module: *const Module, type_idx: u16, local_base: u16) VMError!void {
        debug.print(.vm, "STORE_RECORD_BUF: type_idx={d} local_base={d}", .{ type_idx, local_base });

        // Read the record buffer from shadow area (where native call stored its return value)
        // Native calls like 'read' store their return value at self.stack[self.sp]
        const record_val = self.stack[self.sp];
        const buf: []const u8 = switch (record_val.tag()) {
            .fixed_string, .string => record_val.asString(),
            else => {
                debug.print(.vm, "STORE_RECORD_BUF: invalid type {s}, using empty buffer", .{record_val.tag().name()});
                return; // No record to store
            },
        };

        debug.print(.vm, "STORE_RECORD_BUF: buf len={d}", .{buf.len});

        // Get the type definition
        if (type_idx >= module.types.len) {
            return VMError.InvalidType;
        }

        const type_def = &module.types[type_idx];

        // Distribute buffer data to LOCAL fields (not globals!)
        // Locals are accessed via stack[fp + slot]
        for (type_def.fields, 0..) |field, field_idx| {
            const local_slot = local_base + @as(u16, @intCast(field_idx));
            const offset = field.offset;
            const end = @min(offset + field.size, buf.len);
            const stack_index = self.fp + local_slot;

            debug.print(.vm, "STORE_RECORD_BUF: field[{d}] local={d} stack_idx={d} offset={d} size={d}", .{ field_idx, local_slot, stack_index, offset, field.size });

            if (offset < buf.len and stack_index < STACK_SIZE) {
                // Extract field data from buffer
                const field_data = buf[offset..@min(end, buf.len)];

                // Allocate copy for the field value
                const field_copy = self.allocator.alloc(u8, field_data.len) catch return VMError.OutOfMemory;
                @memcpy(field_copy, field_data);
                try self.global_buffers.append(self.allocator, field_copy);

                // Store the field value into local slot
                const fixed_str = Value.initFixedString(self.valueAllocator(), field_copy) catch
                    return VMError.OutOfMemory;
                self.stack[stack_index] = fixed_str;
                debug.print(.vm, "STORE_RECORD_BUF: stored '{s}'", .{field_copy[0..@min(20, field_copy.len)]});
            }
        }
    }

    pub fn callNative(self: *Self, module: *const Module, name_idx: u16, arg_count: u8) VMError!void {
        // Get routine name from constants
        const name_const = module.getConstant(name_idx) orelse return VMError.InvalidConstant;
        const routine_name = switch (name_const) {
            .identifier => |n| n,
            .string => |s| s,
            else => return VMError.InvalidConstant,
        };

        debug.print(.vm, "NATIVE: {s} (args={d})", .{ routine_name, arg_count });

        // First, try to find the routine in the current module (internal function)
        // Native calls can invoke both internal and external routines
        // Use "last linked wins" semantics: iterate in reverse order
        var routine_idx: usize = module.routines.len;
        while (routine_idx > 0) {
            routine_idx -= 1;
            const routine = module.routines[routine_idx];
            const rname_const = module.getConstant(routine.name_index) orelse continue;
            const rname = switch (rname_const) {
                .identifier => |n| n,
                .string => |s| s,
                else => continue,
            };
            if (std.mem.eql(u8, rname, routine_name)) {
                // Found internal routine - call it (last linked wins)
                try self.callRoutine(@intCast(routine_idx), arg_count);
                return;
            }
        }

        // Not found in module, try external native function registry
        const def = self.native_registry.lookup(routine_name) orelse {
            debug.print(.vm, "NATIVE {s} failed: function not found", .{routine_name});
            return VMError.InvalidOpcode;
        };

        switch (def.sub_type) {
            .bytecode => {
                // Get the external bytecode module
                const ext_module = self.native_registry.getLoadedModule(def.module_index.?) orelse {
                    debug.print(.vm, "NATIVE {s} failed: module not found", .{routine_name});
                    return VMError.InvalidOpcode;
                };

                // Call into the external module
                // Args are already on the stack - use callExternalRoutine
                try self.callExternalRoutine(ext_module, def.routine_index.?, arg_count);
            },
            .native => {
                // Read arguments from registers (r0, r1, r2, ...)
                // The bytecode emitter loads arguments into sequential registers
                var args: [16]native.Value = undefined;
                var i: usize = 0;
                while (i < arg_count and i < 16) : (i += 1) {
                    args[i] = self.registers[i];
                }

                // Create native function context
                var ctx = native.NativeContext{
                    .allocator = self.allocator,
                    .args = args[0..arg_count],
                    .cursors = &self.cursor_manager,
                    .channels = if (self.channel_manager) |*cm| cm else null,
                };

                // Call the native function
                const result = def.native_fn.?(&ctx) catch |err| {
                    // Check for hot-reload request (special case - not an error)
                    if (err == native.NativeError.ReloadRequested) {
                        return VMError.ReloadRequested;
                    }
                    // Check for I/O errors that should trigger onerror handler
                    if (err == native.NativeError.EndOfFile or
                        err == native.NativeError.RecordNotFound)
                    {
                        debug.print(.vm, "NATIVE {s}: I/O error {}, checking error handler", .{ routine_name, err });
                        if (self.handleIoError(VMError.FileError)) {
                            // Error handler will take over - return without error
                            return;
                        }
                    }
                    // TUI errors include rich context - display it
                    if (err == native.NativeError.TuiError) {
                        std.debug.print("[TUI ERROR] NATIVE {s} failed\n", .{routine_name});
                        if (native.tui_runtime.getLastError()) |tui_err| {
                            std.debug.print("  Operation: {s}\n", .{tui_err.operation});
                            if (tui_err.file.len > 0) {
                                std.debug.print("  Location: {s}:{d}\n", .{ tui_err.file, tui_err.line });
                            }
                            if (tui_err.detail.len > 0) {
                                std.debug.print("  Detail: {s}\n", .{tui_err.detail});
                            }
                        }
                        return VMError.InvalidOpcode;
                    }
                    std.debug.print("NATIVE {s} failed: {}\n", .{ routine_name, err });
                    return VMError.InvalidOpcode;
                };

                // Handle native function return values:
                // Store in r0 for bytecode to use (matches emitter expectation)
                if (result) |res| {
                    self.registers[0] = res;
                    debug.print(.vm, "NATIVE return: r0 = '{s}'", .{res.asString()});
                }
            },
        }
    }

    /// Call a routine in an external module
    fn callExternalRoutine(self: *Self, ext_module: *const Module, routine_idx: u16, arg_count: u8) VMError!void {
        debug.print(.vm, "callExternalRoutine: routine_idx={d}, module has {d} routines", .{ routine_idx, ext_module.routines.len });

        // Debug: check if routines array is valid
        if (ext_module.routines.len == 0) {
            debug.print(.vm, "callExternalRoutine: routines array is empty", .{});
            return VMError.InvalidRoutine;
        }

        debug.print(.vm, "callExternalRoutine: first routine code_offset={d}", .{ext_module.routines[0].code_offset});

        const routine = ext_module.getRoutine(routine_idx) orelse {
            debug.print(.vm, "callExternalRoutine: routine not found!", .{});
            return VMError.InvalidRoutine;
        };

        debug.print(.vm, "callExternalRoutine: routine found, code_offset={d}, local_count={d}", .{ routine.code_offset, routine.local_count });

        // Find the module index for this external module
        var module_index: ?u16 = null;
        for (self.native_registry.loaded_modules.items, 0..) |mod, idx| {
            if (mod == ext_module) {
                module_index = @intCast(idx);
                break;
            }
        }
        debug.print(.vm, "callExternalRoutine: module_index={any}", .{module_index});

        // Calculate caller's sp before args were pushed
        const caller_sp = self.sp - @as(u32, arg_count);
        debug.print(.vm, "callExternalRoutine: caller_sp={d}, self.sp={d}, arg_count={d}", .{ caller_sp, self.sp, arg_count });

        // Save call frame with CURRENT module (for return)
        debug.print(.vm, "callExternalRoutine: saving call frame", .{});
        self.call_stack.append(.{
            .module = self.current_module.?,
            .routine_index = 0, // Not relevant for return
            .return_ip = self.ip,
            .base_pointer = self.fp,
            .stack_pointer = caller_sp,
            .caller_module_index = self.current_module_index,
        }) catch return VMError.CallStackOverflow;

        // Switch to external module and set the module index for globals access
        self.current_module = ext_module;
        self.current_module_index = module_index;
        debug.print(.vm, "callExternalRoutine: switched to module index {any}, current_module now has {d} routines", .{ module_index, ext_module.routines.len });

        // Set up new frame
        self.fp = self.sp - @as(u32, arg_count);
        self.ip = routine.code_offset;
        debug.print(.vm, "callExternalRoutine: set up frame, fp={d}, ip={d}", .{ self.fp, self.ip });

        // Reserve space for additional local variables beyond the parameters and initialize to 0
        if (routine.local_count > arg_count) {
            const extra_locals = routine.local_count - @as(u32, arg_count);
            for (0..extra_locals) |i| {
                self.stack[self.sp + i] = Value.initInt(0);
            }
            self.sp += extra_locals;
        }

        debug.print(.vm, "callExternalRoutine: SUCCESS, sp={d}", .{self.sp});
    }

    /// Convert a GlobalValue to a VM Value
    fn globalValueToValue(self: *Self, gval: native.GlobalValue) Value {
        return switch (gval) {
            .null_val => Value.null_val,
            .integer => |i| Value.initInt(i),
            .decimal => |d| Value.initDecimal(self.valueAllocator(), d.value, d.precision) catch Value.null_val,
            .fixed_string => |a| Value.initFixedString(self.valueAllocator(), a) catch Value.null_val,
            .boolean => |b| Value.initBool(b),
        };
    }

    /// Convert a VM Value to a GlobalValue
    fn valueToGlobalValue(self: *Self, val: Value) native.GlobalValue {
        return switch (val.tag()) {
            .null_val => .{ .null_val = {} },
            .integer => .{ .integer = val.asInt() },
            .decimal => blk: {
                if (val.asDecimal()) |d| {
                    break :blk .{ .decimal = .{ .value = d.value, .precision = d.precision } };
                }
                break :blk .{ .null_val = {} };
            },
            .fixed_string, .string => blk: {
                // Make a copy for persistent storage
                const copy = self.allocator.dupe(u8, val.asString()) catch return .{ .null_val = {} };
                break :blk .{ .fixed_string = copy };
            },
            .boolean => .{ .boolean = val.asBool() },
            else => .{ .null_val = {} },
        };
    }

    fn concatValues(self: *Self, a: Value, b: Value) VMError!Value {
        // Get string representations
        const a_str: []const u8 = switch (a.tag()) {
            .fixed_string, .string => a.asString(),
            .integer => blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const len = std.fmt.bufPrint(buf, "{d}", .{a.asInt()}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk buf[0..len.len];
            },
            else => return Value.null_val,
        };

        const b_str: []const u8 = switch (b.tag()) {
            .fixed_string, .string => b.asString(),
            .integer => blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const len = std.fmt.bufPrint(buf, "{d}", .{b.asInt()}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk buf[0..len.len];
            },
            else => return Value.null_val,
        };

        // Concatenate
        const result = self.allocator.alloc(u8, a_str.len + b_str.len) catch return VMError.OutOfMemory;
        @memcpy(result[0..a_str.len], a_str);
        @memcpy(result[a_str.len..], b_str);
        try self.global_buffers.append(self.allocator, result);

        return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
    }

    /// Convert a value to its string representation
    /// Convert a value to a decimal representation with given precision
    fn toDecimalValue(self: *Self, val: Value, precision: u8) i64 {
        _ = self;
        // Clamp precision to avoid overflow (10^18 is max that fits in i64)
        const safe_precision: u8 = @min(precision, 18);
        const scale = std.math.powi(i64, 10, safe_precision) catch 1;
        return switch (val.tag()) {
            .integer => val.asInt() * scale,
            .decimal => blk: {
                const d = val.asDecimal() orelse break :blk 0;
                // Adjust precision if needed
                if (d.precision == precision) {
                    break :blk d.value;
                } else if (d.precision < precision) {
                    // Need more precision - multiply
                    const diff = precision - d.precision;
                    break :blk d.value * std.math.pow(i64, 10, diff);
                } else {
                    // Less precision needed - divide
                    const diff = d.precision - precision;
                    break :blk @divTrunc(d.value, std.math.pow(i64, 10, diff));
                }
            },
            .fixed_string, .string => blk: {
                // Try to parse as number
                const trimmed = std.mem.trim(u8, val.asString(), " ");
                const parsed = std.fmt.parseInt(i64, trimmed, 10) catch 0;
                break :blk parsed * scale;
            },
            else => 0,
        };
    }

    fn valueToString(self: *Self, val: Value) VMError!Value {
        return switch (val.tag()) {
            .fixed_string, .string => blk: {
                const str = val.asString();
                const copy = self.allocator.dupe(u8, str) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, copy);
                break :blk Value.initFixedString(self.valueAllocator(), copy) catch return VMError.OutOfMemory;
            },
            .integer => blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                const len = std.fmt.bufPrint(buf, "{d}", .{val.asInt()}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk Value.initFixedString(self.valueAllocator(), buf[0..len.len]) catch return VMError.OutOfMemory;
            },
            .decimal => blk: {
                const buf = self.allocator.alloc(u8, 32) catch return VMError.OutOfMemory;
                // Format decimal value - just show as simple integer for now
                // Full decimal formatting can be enhanced later
                const d = val.asDecimal() orelse break :blk Value.null_val;
                const len = std.fmt.bufPrint(buf, "{d}", .{d.value}) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, buf);
                break :blk Value.initFixedString(self.valueAllocator(), buf[0..len.len]) catch return VMError.OutOfMemory;
            },
            .boolean => blk: {
                const str = if (val.asBool()) "true" else "false";
                const copy = self.allocator.dupe(u8, str) catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, copy);
                break :blk Value.initFixedString(self.valueAllocator(), copy) catch return VMError.OutOfMemory;
            },
            .null_val => blk: {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                break :blk Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
            else => blk: {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                break :blk Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
        };
    }

    /// Convert a value to integer
    fn valueToInt(self: *Self, val: Value) VMError!Value {
        _ = self;
        return switch (val.tag()) {
            .integer => Value.initInt(val.asInt()),
            .decimal => blk: {
                const d = val.asDecimal() orelse break :blk Value.initInt(0);
                break :blk Value.initInt(@divTrunc(d.value, std.math.pow(i64, 10, @intCast(d.precision))));
            },
            .fixed_string, .string => Value.initInt(std.fmt.parseInt(i64, val.asString(), 10) catch 0),
            .boolean => Value.initInt(if (val.asBool()) 1 else 0),
            else => Value.initInt(0),
        };
    }

    /// Trim whitespace from a string value
    fn trimString(self: *Self, val: Value) VMError!Value {
        const str = switch (val.tag()) {
            .fixed_string, .string => val.asString(),
            else => {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
        };
        const trimmed = std.mem.trim(u8, str, " \t\r\n");
        const result = self.allocator.alloc(u8, trimmed.len) catch return VMError.OutOfMemory;
        @memcpy(result, trimmed);
        try self.global_buffers.append(self.allocator, result);
        return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
    }

    /// Get string length
    fn stringLength(self: *Self, val: Value) VMError!Value {
        _ = self;
        const len: i64 = switch (val.tag()) {
            .fixed_string, .string => @intCast(val.asString().len),
            else => 0,
        };
        return Value.initInt(len);
    }

    /// Remove last character from string (for backspace)
    fn stringDeleteLast(self: *Self, val: Value) VMError!Value {
        const str = switch (val.tag()) {
            .fixed_string, .string => val.asString(),
            else => {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
        };

        // Trim trailing spaces first
        const trimmed = std.mem.trimRight(u8, str, " ");

        // Remove last character if any
        if (trimmed.len > 0) {
            const new_len = trimmed.len - 1;
            const result = self.allocator.alloc(u8, new_len) catch return VMError.OutOfMemory;
            @memcpy(result, trimmed[0..new_len]);
            try self.global_buffers.append(self.allocator, result);
            return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
        }

        // Empty string
        const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
        try self.global_buffers.append(self.allocator, empty);
        return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
    }

    /// Set character at position in string
    /// If position is beyond string length, extends the string with spaces
    fn stringSetChar(self: *Self, str_val: Value, pos_val: Value, char_val: Value) VMError!Value {
        const str = switch (str_val.tag()) {
            .fixed_string, .string => str_val.asString(),
            else => {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
        };

        const pos: usize = switch (pos_val.tag()) {
            .integer => @intCast(if (pos_val.asInt() >= 0) pos_val.asInt() else 0),
            .decimal => blk: {
                const d = pos_val.asDecimal() orelse break :blk @as(usize, 0);
                break :blk @intCast(if (d.value >= 0) d.value else 0);
            },
            else => 0,
        };

        // Get the character to set
        const ch: u8 = switch (char_val.tag()) {
            .fixed_string, .string => blk: {
                const s = char_val.asString();
                break :blk if (s.len > 0) s[0] else ' ';
            },
            else => ' ',
        };

        // Determine result size - extend if position is beyond current length
        const result_len = if (pos >= str.len) pos + 1 else str.len;
        const result = self.allocator.alloc(u8, result_len) catch return VMError.OutOfMemory;

        // Copy original string content
        if (str.len > 0) {
            @memcpy(result[0..str.len], str);
        }

        // Fill any gap with spaces (if we're extending)
        if (pos >= str.len) {
            @memset(result[str.len..pos], ' ');
        }

        // Set the character at the target position
        result[pos] = ch;

        try self.global_buffers.append(self.allocator, result);
        return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
    }

    /// Extract substring from string
    /// Syntax: variable(start:length) when is_length=true
    ///         variable(start,end) when is_length=false
    /// Note: Cot uses 1-based indexing
    fn stringSubstr(self: *Self, source_val: Value, start_val: Value, length_or_end_val: Value, is_length: bool) VMError!Value {
        const source = switch (source_val.tag()) {
            .fixed_string, .string => source_val.asString(),
            else => {
                const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
                try self.global_buffers.append(self.allocator, empty);
                return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
            },
        };

        // Cot uses 1-based indexing, convert to 0-based
        const start_1based: i64 = switch (start_val.tag()) {
            .integer => start_val.asInt(),
            .decimal => if (start_val.asDecimal()) |d| d.value else 1,
            else => 1,
        };

        const length_or_end: i64 = switch (length_or_end_val.tag()) {
            .integer => length_or_end_val.asInt(),
            .decimal => if (length_or_end_val.asDecimal()) |d| d.value else 1,
            else => 1,
        };

        // Convert to 0-based index
        const start_0based: usize = if (start_1based > 0) @intCast(start_1based - 1) else 0;

        // Calculate the actual length to extract
        const length: usize = if (is_length) blk: {
            // (start:length) syntax - length_or_end is the length
            break :blk if (length_or_end > 0) @intCast(length_or_end) else 0;
        } else blk: {
            // (start,end) syntax - length_or_end is the end position (1-based)
            const end_1based: usize = if (length_or_end > 0) @intCast(length_or_end) else 0;
            if (end_1based >= start_1based) {
                break :blk end_1based - @as(usize, @intCast(start_1based)) + 1;
            } else {
                break :blk 0;
            }
        };

        // Bounds checking
        if (start_0based >= source.len or length == 0) {
            const empty = self.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
            try self.global_buffers.append(self.allocator, empty);
            return Value.initFixedString(self.valueAllocator(), empty) catch return VMError.OutOfMemory;
        }

        const actual_length = @min(length, source.len - start_0based);
        const result = self.allocator.alloc(u8, actual_length) catch return VMError.OutOfMemory;
        @memcpy(result, source[start_0based..][0..actual_length]);

        try self.global_buffers.append(self.allocator, result);
        return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
    }

    /// Store value into substring of target string
    /// Syntax: variable(start:length) = value when is_length=true
    ///         variable(start,end) = value when is_length=false
    /// Note: Cot uses 1-based indexing
    /// Returns the modified string (caller must store it back)
    fn stringSubstrStore(self: *Self, target_val: Value, start_val: Value, length_or_end_val: Value, value_val: Value, is_length: bool) VMError!Value {
        const target = switch (target_val.tag()) {
            .fixed_string, .string => target_val.asString(),
            else => return target_val, // Can't modify non-string
        };

        const value = switch (value_val.tag()) {
            .fixed_string, .string => value_val.asString(),
            else => "",
        };

        // Cot uses 1-based indexing, convert to 0-based
        const start_1based: i64 = switch (start_val.tag()) {
            .integer => start_val.asInt(),
            .decimal => if (start_val.asDecimal()) |d| d.value else 1,
            else => 1,
        };

        const length_or_end: i64 = switch (length_or_end_val.tag()) {
            .integer => length_or_end_val.asInt(),
            .decimal => if (length_or_end_val.asDecimal()) |d| d.value else 1,
            else => 1,
        };

        // Convert to 0-based index
        const start_0based: usize = if (start_1based > 0) @intCast(start_1based - 1) else 0;

        // Calculate the length of the region to replace
        const length: usize = if (is_length) blk: {
            // (start:length) syntax - length_or_end is the length
            break :blk if (length_or_end > 0) @intCast(length_or_end) else 0;
        } else blk: {
            // (start,end) syntax - length_or_end is the end position (1-based)
            const end_1based: usize = if (length_or_end > 0) @intCast(length_or_end) else 0;
            if (end_1based >= start_1based) {
                break :blk end_1based - @as(usize, @intCast(start_1based)) + 1;
            } else {
                break :blk 0;
            }
        };

        // Bounds checking - if out of bounds, return target unchanged
        if (start_0based >= target.len or length == 0) {
            return target_val;
        }

        // Create new string with the value inserted at the substring position
        // The value is truncated or space-padded to fit exactly in the length region
        const result = self.allocator.alloc(u8, target.len) catch return VMError.OutOfMemory;

        // Copy the part before the substring
        if (start_0based > 0) {
            @memcpy(result[0..start_0based], target[0..start_0based]);
        }

        // Calculate how much of the target region we're modifying
        const actual_length = @min(length, target.len - start_0based);

        // Copy the value into the substring region (truncate or pad as needed)
        const copy_len = @min(value.len, actual_length);
        @memcpy(result[start_0based..][0..copy_len], value[0..copy_len]);

        // Pad with spaces if value is shorter than the region
        if (copy_len < actual_length) {
            @memset(result[start_0based + copy_len ..][0 .. actual_length - copy_len], ' ');
        }

        // Copy the part after the substring
        const after_pos = start_0based + actual_length;
        if (after_pos < target.len) {
            @memcpy(result[after_pos..], target[after_pos..]);
        }

        try self.global_buffers.append(self.allocator, result);
        return Value.initFixedString(self.valueAllocator(), result) catch return VMError.OutOfMemory;
    }

    /// Absolute value
    fn absValue(self: *Self, val: Value) VMError!Value {
        return switch (val.tag()) {
            .integer => Value.initInt(@intCast(@abs(val.asInt()))),
            .decimal => blk: {
                if (val.asDecimal()) |d| {
                    break :blk Value.initDecimal(self.valueAllocator(), @intCast(@abs(d.value)), d.precision) catch return VMError.OutOfMemory;
                }
                break :blk val;
            },
            else => val,
        };
    }

    /// Handle an I/O error - either jump to error handler or propagate
    /// Returns true if we jumped to error handler (caller should continue execution)
    /// Returns false if no handler set (caller should propagate error)
    pub fn handleIoError(self: *Self, err: VMError) bool {
        if (self.error_handler_ip) |handler_ip| {
            // Save error for %ERROR function
            self.last_error = err;
            // Jump to error handler
            self.ip = handler_ip;
            debug.print(.vm, "IO Error handled: jumping to IP {d}", .{handler_ip});
            return true;
        }
        return false;
    }
};

// ============================================
// Inline Tests (Ghostty pattern)
// ============================================

test "vm: stack push/pop" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Test stack operations
    try vm.push(Value.initInt(42));
    try vm.push(Value.initInt(10));

    const b = try vm.pop();
    const a = try vm.pop();

    try std.testing.expectEqual(@as(i64, 42), a.toInt());
    try std.testing.expectEqual(@as(i64, 10), b.toInt());
}

test "vm: stack underflow error" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Empty stack should error on pop
    const result = vm.pop();
    try std.testing.expectError(VMError.StackUnderflow, result);
}

test "vm: register operations" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Test register read/write
    vm.registers[0] = Value.initInt(100);
    vm.registers[1] = Value.initInt(200);

    try std.testing.expectEqual(@as(i64, 100), vm.registers[0].toInt());
    try std.testing.expectEqual(@as(i64, 200), vm.registers[1].toInt());
}

test "vm: call stack operations" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Initial call stack should be empty
    try std.testing.expectEqual(@as(usize, 0), vm.call_stack.len);

    // Top of empty stack should be null
    try std.testing.expect(vm.call_stack.top() == null);
}

test "vm: value types" {
    // Test integer values
    const int_val = Value.initInt(42);
    try std.testing.expectEqual(@as(i64, 42), int_val.toInt());

    // Test boolean values
    const true_val = Value.initBool(true);
    const false_val = Value.initBool(false);
    try std.testing.expect(true_val.toBool());
    try std.testing.expect(!false_val.toBool());
}

test "vm: inline cache" {
    var cache = InlineCache{};

    // Initially invalid
    try std.testing.expect(!cache.isValidFor(1));

    // Update cache
    cache.update(1, 8, 4);
    try std.testing.expect(cache.isValidFor(1));
    try std.testing.expect(!cache.isValidFor(2));

    // Record hits/misses
    cache.recordHit();
    cache.recordHit();
    cache.recordMiss();

    try std.testing.expectEqual(@as(u32, 2), cache.hit_count);
    try std.testing.expectEqual(@as(u32, 1), cache.miss_count);

    // Check hit ratio (2/3 ≈ 0.666)
    const ratio = cache.hitRatio();
    try std.testing.expect(ratio > 0.6 and ratio < 0.7);
}

// Pull in opcode handler tests
test {
    _ = @import("vm_opcodes_test.zig");
}
