//! libts Pipeline Debug Infrastructure
//!
//! Mirrors zig/libcot/debug.zig for the TypeScript/JavaScript frontend.
//! Same environment variable (COT_DEBUG), same API, same output format.
//!
//! TS-specific phases: scan, parse, transform, resolve.
//! libcot phases (check, lower, ssa, codegen, etc.) still work — they're
//! passed through to the shared pipeline once the Cot AST is produced.
//!
//! Usage:
//!   COT_DEBUG=scan,parse ./zig-out/bin/cot build app.ts   # TS frontend phases
//!   COT_DEBUG=transform  ./zig-out/bin/cot build app.ts   # TS→Cot transform
//!   COT_DEBUG=all        ./zig-out/bin/cot build app.ts   # Everything

const std = @import("std");

pub const Phase = enum {
    // TS frontend phases
    scan, // TS/JS lexer
    parse, // TS/JS parser → TS AST
    transform, // TS AST → Cot AST
    resolve, // Node module resolution

    // Shared pipeline phases (once Cot AST is produced, libcot takes over)
    check,
    lower,
    ssa,
    codegen,
};

pub const DebugPhases = struct {
    scan: bool = false,
    parse: bool = false,
    transform: bool = false,
    resolve: bool = false,
    check: bool = false,
    lower: bool = false,
    ssa: bool = false,
    codegen: bool = false,
    all: bool = false,

    pub fn fromEnv() DebugPhases {
        const env = std.posix.getenv("COT_DEBUG") orelse return .{};
        return parseStr(env);
    }

    fn parseStr(str: []const u8) DebugPhases {
        var result = DebugPhases{};
        var iter = std.mem.splitScalar(u8, str, ',');
        while (iter.next()) |phase| {
            const trimmed = std.mem.trim(u8, phase, " \t");
            if (std.mem.eql(u8, trimmed, "all")) result.all = true;
            if (std.mem.eql(u8, trimmed, "scan")) result.scan = true;
            if (std.mem.eql(u8, trimmed, "parse")) result.parse = true;
            if (std.mem.eql(u8, trimmed, "transform")) result.transform = true;
            if (std.mem.eql(u8, trimmed, "resolve")) result.resolve = true;
            if (std.mem.eql(u8, trimmed, "check")) result.check = true;
            if (std.mem.eql(u8, trimmed, "lower")) result.lower = true;
            if (std.mem.eql(u8, trimmed, "ssa")) result.ssa = true;
            if (std.mem.eql(u8, trimmed, "codegen")) result.codegen = true;
        }
        return result;
    }

    pub fn isEnabled(self: DebugPhases, phase: Phase) bool {
        if (self.all) return true;
        return switch (phase) {
            .scan => self.scan,
            .parse => self.parse,
            .transform => self.transform,
            .resolve => self.resolve,
            .check => self.check,
            .lower => self.lower,
            .ssa => self.ssa,
            .codegen => self.codegen,
        };
    }
};

// ============================================================================
// Global Debug State
// ============================================================================

var global_phases: ?DebugPhases = null;
var global_trace_func: ?[]const u8 = null;

/// Initialize global debug state. Call once at startup.
pub fn initGlobal() void {
    global_phases = DebugPhases.fromEnv();
    global_trace_func = std.posix.getenv("COT_TRACE");
    if (global_trace_func != null) {
        std.debug.print("\n=== COT_TRACE enabled for: {s} ===\n\n", .{global_trace_func.?});
    }
}

/// Check if we should trace a specific function.
pub fn shouldTrace(func_name: []const u8) bool {
    const trace = global_trace_func orelse return false;
    return std.mem.eql(u8, trace, func_name);
}

/// Get the trace function name (if any).
pub fn getTraceFunc() ?[]const u8 {
    return global_trace_func;
}

/// Check if a phase has debug output enabled.
pub fn isEnabled(phase: Phase) bool {
    const phases = global_phases orelse return false;
    return phases.isEnabled(phase);
}

/// Log a message if the phase is enabled.
var start_time: ?i128 = null;

pub fn log(phase: Phase, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(phase)) return;
    if (start_time == null) start_time = std.time.nanoTimestamp();
    const elapsed_ms = @divTrunc(std.time.nanoTimestamp() - start_time.?, 1_000_000);
    std.debug.print("[{s} {d}ms] " ++ fmt ++ "\n", .{ @tagName(phase), @as(i64, @intCast(elapsed_ms)) } ++ args);
}

/// Log a message with elapsed time in microseconds.
pub fn logTimed(phase: Phase, comptime fmt: []const u8, elapsed_ns: i128, args: anytype) void {
    if (!isEnabled(phase)) return;
    const us = @as(i64, @intCast(@divTrunc(elapsed_ns, 1000)));
    std.debug.print("[{s}] " ++ fmt ++ " [{d}µs]\n", .{@tagName(phase)} ++ args ++ .{us});
}

/// Get current timestamp for timing passes.
pub fn timestamp() i128 {
    return std.time.nanoTimestamp();
}

/// Log without phase prefix (for continuation lines).
pub fn logRaw(phase: Phase, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(phase)) return;
    std.debug.print(fmt, args);
}

// ============================================================================
// Tests
// ============================================================================

test "DebugPhases.parseStr" {
    const phases = DebugPhases.parseStr("scan,parse,transform");
    try std.testing.expect(phases.scan);
    try std.testing.expect(phases.parse);
    try std.testing.expect(phases.transform);
    try std.testing.expect(!phases.resolve);
    try std.testing.expect(!phases.check);
}

test "DebugPhases.all enables all phases" {
    const phases = DebugPhases.parseStr("all");
    try std.testing.expect(phases.isEnabled(.scan));
    try std.testing.expect(phases.isEnabled(.parse));
    try std.testing.expect(phases.isEnabled(.transform));
    try std.testing.expect(phases.isEnabled(.resolve));
    try std.testing.expect(phases.isEnabled(.check));
    try std.testing.expect(phases.isEnabled(.codegen));
}

test "DebugPhases individual phase check" {
    const phases = DebugPhases.parseStr("resolve,codegen");
    try std.testing.expect(!phases.isEnabled(.scan));
    try std.testing.expect(!phases.isEnabled(.parse));
    try std.testing.expect(phases.isEnabled(.resolve));
    try std.testing.expect(phases.isEnabled(.codegen));
}

test "DebugPhases empty string" {
    const phases = DebugPhases.parseStr("");
    try std.testing.expect(!phases.isEnabled(.scan));
    try std.testing.expect(!phases.isEnabled(.parse));
    try std.testing.expect(!phases.all);
}

test "DebugPhases shared pipeline phases" {
    const phases = DebugPhases.parseStr("check,ssa");
    try std.testing.expect(!phases.isEnabled(.scan));
    try std.testing.expect(phases.isEnabled(.check));
    try std.testing.expect(phases.isEnabled(.ssa));
    try std.testing.expect(!phases.isEnabled(.codegen));
}
