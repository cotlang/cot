//! Debug logging infrastructure for the Cot compiler.
//!
//! Provides structured, filterable debug output for every compilation phase.
//! Enabled via environment variables:
//!
//!   COT_DEBUG=all                 — log all phases
//!   COT_DEBUG=codegen,ssa         — log specific phases
//!   COT_TRACE=myFunc              — trace one function through ALL passes
//!
//! Each log line includes the phase name and elapsed milliseconds:
//!   [codegen 12ms] emitting function 'main' (3 blocks)
//!
//! The phase filter is read once from the environment at startup and
//! cached globally. Log calls for disabled phases are effectively free
//! (one branch on a cached bool).

const std = @import("std");

/// Compilation phase. Each phase can be independently enabled for logging.
pub const Phase = enum {
    parse,
    check,
    lower,
    ssa,
    deadcode,
    copyelim,
    phielim,
    schedule,
    async_split,
    regalloc,
    codegen,
    strings,
    abi,
};

/// Set of enabled phases, parsed from COT_DEBUG environment variable.
pub const Config = struct {
    enabled: [@typeInfo(Phase).@"enum".fields.len]bool = .{false} ** @typeInfo(Phase).@"enum".fields.len,
    all: bool = false,

    /// Parse from the COT_DEBUG environment variable.
    pub fn fromEnv() Config {
        const env = std.posix.getenv("COT_DEBUG") orelse return .{};
        return parse(env);
    }

    /// Parse a comma-separated list of phase names.
    pub fn parse(str: []const u8) Config {
        var result = Config{};
        var iter = std.mem.splitScalar(u8, str, ',');
        while (iter.next()) |part| {
            const name = std.mem.trim(u8, part, " \t");
            if (std.mem.eql(u8, name, "all")) {
                result.all = true;
                continue;
            }
            inline for (@typeInfo(Phase).@"enum".fields, 0..) |field, i| {
                if (std.mem.eql(u8, name, field.name)) {
                    result.enabled[i] = true;
                }
            }
        }
        return result;
    }

    /// Check if a phase has debug output enabled.
    pub fn isEnabled(self: Config, phase: Phase) bool {
        if (self.all) return true;
        return self.enabled[@intFromEnum(phase)];
    }
};


// Global state — initialized once at startup, read by all log calls.

var global_config: ?Config = null;
var global_trace_func: ?[]const u8 = null;
var start_time: ?i128 = null;

/// Initialize global debug state from environment. Call once at startup.
pub fn init() void {
    global_config = Config.fromEnv();
    global_trace_func = std.posix.getenv("COT_TRACE");
    if (global_trace_func != null) {
        std.debug.print("\n=== COT_TRACE enabled for: {s} ===\n\n", .{global_trace_func.?});
    }
}

/// Check if a phase has debug output enabled.
pub fn isEnabled(phase: Phase) bool {
    const config = global_config orelse return false;
    return config.isEnabled(phase);
}

/// Check if a specific function should be traced through all passes.
pub fn shouldTrace(func_name: []const u8) bool {
    const trace = global_trace_func orelse return false;
    return std.mem.eql(u8, trace, func_name);
}

/// Get the trace function name (if set via COT_TRACE).
pub fn traceFunc() ?[]const u8 {
    return global_trace_func;
}


/// Log a message for a phase. No-op if the phase is disabled.
///
/// Output format: [phase Nms] message
/// Timestamp is milliseconds since first log call.
pub fn log(phase: Phase, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(phase)) return;
    if (start_time == null) start_time = std.time.nanoTimestamp();
    const elapsed_ms = @divTrunc(std.time.nanoTimestamp() - start_time.?, 1_000_000);
    std.debug.print("[{s} {d}ms] " ++ fmt ++ "\n", .{ @tagName(phase), @as(i64, @intCast(elapsed_ms)) } ++ args);
}

/// Log a message with an explicit elapsed time in microseconds.
pub fn logTimed(phase: Phase, comptime fmt: []const u8, elapsed_ns: i128, args: anytype) void {
    if (!isEnabled(phase)) return;
    const us = @as(i64, @intCast(@divTrunc(elapsed_ns, 1000)));
    std.debug.print("[{s}] " ++ fmt ++ " [{d}us]\n", .{@tagName(phase)} ++ args ++ .{us});
}

/// Log without phase prefix or timestamp (for continuation lines).
pub fn logRaw(phase: Phase, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(phase)) return;
    std.debug.print(fmt, args);
}

/// Get current nanosecond timestamp for timing passes.
pub fn timestamp() i128 {
    return std.time.nanoTimestamp();
}


test "parse phase names" {
    const config = Config.parse("parse,ssa,regalloc");
    try std.testing.expect(config.isEnabled(.parse));
    try std.testing.expect(!config.isEnabled(.check));
    try std.testing.expect(!config.isEnabled(.lower));
    try std.testing.expect(config.isEnabled(.ssa));
    try std.testing.expect(config.isEnabled(.regalloc));
}

test "all enables every phase" {
    const config = Config.parse("all");
    try std.testing.expect(config.isEnabled(.parse));
    try std.testing.expect(config.isEnabled(.check));
    try std.testing.expect(config.isEnabled(.lower));
    try std.testing.expect(config.isEnabled(.ssa));
    try std.testing.expect(config.isEnabled(.regalloc));
    try std.testing.expect(config.isEnabled(.codegen));
}

test "individual phases" {
    const config = Config.parse("abi,codegen");
    try std.testing.expect(!config.isEnabled(.parse));
    try std.testing.expect(config.isEnabled(.abi));
    try std.testing.expect(config.isEnabled(.codegen));
}

test "empty string disables all" {
    const config = Config.parse("");
    try std.testing.expect(!config.isEnabled(.parse));
    try std.testing.expect(!config.isEnabled(.ssa));
    try std.testing.expect(!config.all);
}
