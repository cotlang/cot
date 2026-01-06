//! VM Types
//!
//! Standalone types used by the Virtual Machine.
//! Extracted from vm.zig following the Ghostty pattern of
//! small, focused type modules.

const std = @import("std");
const Opcode = @import("opcodes.zig").Opcode;

/// Maximum call depth
pub const MAX_CALL_DEPTH = 256;

/// Result of executing an opcode handler
pub const DispatchResult = enum {
    /// Continue to next instruction
    continue_dispatch,
    /// VM should halt (normal termination)
    halt,
    /// Return from main routine (normal completion)
    return_from_main,
};

/// Inline cache entry for field access
/// Caches the type ID and field offset to avoid repeated lookups
pub const InlineCache = struct {
    /// Cached record type ID (0xFFFF = invalid/empty)
    cached_type_id: u16 = 0xFFFF,
    /// Cached field byte offset within record
    cached_offset: u16 = 0,
    /// Cached field size in bytes
    cached_size: u8 = 0,
    /// Cache hit count (for profiling)
    hit_count: u32 = 0,
    /// Cache miss count (for profiling)
    miss_count: u32 = 0,

    /// Check if cache is valid for a given type ID
    pub fn isValidFor(self: InlineCache, type_id: u16) bool {
        return self.cached_type_id != 0xFFFF and self.cached_type_id == type_id;
    }

    /// Update cache with new type/offset
    pub fn update(self: *InlineCache, type_id: u16, offset: u16, size: u8) void {
        self.cached_type_id = type_id;
        self.cached_offset = offset;
        self.cached_size = size;
    }

    /// Record a cache hit
    pub fn recordHit(self: *InlineCache) void {
        self.hit_count +|= 1; // Saturating add
    }

    /// Record a cache miss
    pub fn recordMiss(self: *InlineCache) void {
        self.miss_count +|= 1;
    }

    /// Get hit ratio (0.0 to 1.0)
    pub fn hitRatio(self: InlineCache) f64 {
        const total = self.hit_count + self.miss_count;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hit_count)) / @as(f64, @floatFromInt(total));
    }
};

/// VM error types - these are the actual Zig errors returned
pub const VMError = error{
    StackOverflow,
    StackUnderflow,
    InvalidOpcode,
    InvalidConstant,
    InvalidType,
    InvalidRoutine,
    DivisionByZero,
    OutOfMemory,
    FileError,
    IsamError,
    UnresolvedImport,
    CallStackOverflow,
    Halted,
    ReloadRequested,
    BytecodeOutOfBounds,
    NullPointerAccess,
    InvalidMemoryAccess,
    BadDigit, // DBL: non-numeric character in decimal field
    ArrayOutOfBounds,
    UnhandledException,
};

/// Rich runtime error with full context - this is what gets reported to the user
pub const RuntimeError = struct {
    kind: VMError,
    message: []const u8,
    ip: u32,
    opcode: ?Opcode,
    source_line: u32,
    routine_name: []const u8,
    stack_depth: u32,
    // Additional context for specific errors
    detail: ?[]const u8 = null,

    pub fn format(
        self: RuntimeError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print(
            \\Runtime Error: {s}
            \\  Message: {s}
            \\  Location: {s} line {d} (IP: 0x{X:0>4})
            \\  Opcode: {s}
            \\  Stack depth: {d}
        , .{
            @errorName(self.kind),
            self.message,
            self.routine_name,
            self.source_line,
            self.ip,
            if (self.opcode) |op| @tagName(op) else "<unknown>",
            self.stack_depth,
        });
        if (self.detail) |d| {
            try writer.print("\n  Detail: {s}", .{d});
        }
    }
};

/// Global crash context for signal handler - stores last execution state
pub const CrashContext = struct {
    ip: u32 = 0,
    last_opcode: ?Opcode = null,
    source_line: u32 = 0,
    routine_name: ?[]const u8 = null,
    active: bool = false,

    // Native code tracking - for crashes in native calls or external libraries
    native_context: ?[]const u8 = null, // e.g., "http_get" or "db_query"
    native_file: ?[]const u8 = null, // Source file where native call started
    native_line: u32 = 0, // Line where native call started

    // Pre-captured stack addresses for crash reporting
    stack_addrs: [32]usize = [_]usize{0} ** 32,
    stack_count: usize = 0,

    /// Enter native code context - call before risky operations
    pub fn enterNative(self: *CrashContext, context: []const u8, file: []const u8, line: u32) void {
        self.native_context = context;
        self.native_file = file;
        self.native_line = line;
        // Capture stack trace at this point - before the crash
        var trace: std.builtin.StackTrace = .{
            .index = 0,
            .instruction_addresses = &self.stack_addrs,
        };
        std.debug.captureStackTrace(@returnAddress(), &trace);
        self.stack_count = trace.index;
    }

    /// Exit native code context - call after risky operation completes safely
    pub fn exitNative(self: *CrashContext) void {
        self.native_context = null;
        self.native_file = null;
        self.native_line = 0;
        self.stack_count = 0;
    }
};

/// Call frame
pub const CallFrame = struct {
    module: *const @import("module.zig").Module,
    routine_index: u16,
    return_ip: usize,
    base_pointer: usize,
    stack_pointer: usize, // Caller's sp before pushing args
    caller_module_index: ?u16, // Caller's module index (to restore on return)
};

/// Simple bounded call stack (replacement for removed BoundedArray)
pub const CallStack = struct {
    buffer: [MAX_CALL_DEPTH]CallFrame = undefined,
    len: usize = 0,

    pub fn append(self: *CallStack, item: CallFrame) !void {
        if (self.len >= MAX_CALL_DEPTH) return error.CallStackOverflow;
        self.buffer[self.len] = item;
        self.len += 1;
    }

    pub fn pop(self: *CallStack) ?CallFrame {
        if (self.len == 0) return null;
        self.len -= 1;
        return self.buffer[self.len];
    }

    pub fn slice(self: *CallStack) []CallFrame {
        return self.buffer[0..self.len];
    }

    /// Get the top call frame without popping (peek at current frame)
    pub fn top(self: *CallStack) ?CallFrame {
        if (self.len == 0) return null;
        return self.buffer[self.len - 1];
    }
};

/// Reason why the VM stopped execution
pub const StopReason = enum {
    /// Program completed normally
    completed,
    /// Hit a breakpoint
    breakpoint,
    /// Stopped after a step operation
    step,
    /// Execution paused by user
    paused,
    /// An error occurred
    err,
};
