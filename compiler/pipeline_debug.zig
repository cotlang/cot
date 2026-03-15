//! Pipeline Debug Infrastructure
//!
//! Provides systematic debugging output for the compilation pipeline.
//! Set COT_DEBUG=phase1,phase2 or COT_DEBUG=all to enable debug output.
//! Set COT_TRACE=funcname to trace a specific function through ALL passes.

const std = @import("std");

pub const Phase = enum {
    parse,
    check,
    lower,
    ssa,
    deadcode,
    copyelim,
    phielim,
    schedule,
    regalloc,
    codegen,
    strings,
    abi,
};

pub const DebugPhases = struct {
    parse: bool = false,
    check: bool = false,
    lower: bool = false,
    ssa: bool = false,
    deadcode: bool = false,
    copyelim: bool = false,
    phielim: bool = false,
    schedule: bool = false,
    regalloc: bool = false,
    codegen: bool = false,
    strings: bool = false,
    abi: bool = false,
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
            if (std.mem.eql(u8, trimmed, "parse")) result.parse = true;
            if (std.mem.eql(u8, trimmed, "check")) result.check = true;
            if (std.mem.eql(u8, trimmed, "lower")) result.lower = true;
            if (std.mem.eql(u8, trimmed, "ssa")) result.ssa = true;
            if (std.mem.eql(u8, trimmed, "deadcode")) result.deadcode = true;
            if (std.mem.eql(u8, trimmed, "copyelim")) result.copyelim = true;
            if (std.mem.eql(u8, trimmed, "phielim")) result.phielim = true;
            if (std.mem.eql(u8, trimmed, "schedule")) result.schedule = true;
            if (std.mem.eql(u8, trimmed, "regalloc")) result.regalloc = true;
            if (std.mem.eql(u8, trimmed, "codegen")) result.codegen = true;
            if (std.mem.eql(u8, trimmed, "strings")) result.strings = true;
            if (std.mem.eql(u8, trimmed, "abi")) result.abi = true;
        }
        return result;
    }

    pub fn isEnabled(self: DebugPhases, phase: Phase) bool {
        if (self.all) return true;
        return switch (phase) {
            .parse => self.parse,
            .check => self.check,
            .lower => self.lower,
            .ssa => self.ssa,
            .deadcode => self.deadcode,
            .copyelim => self.copyelim,
            .phielim => self.phielim,
            .schedule => self.schedule,
            .regalloc => self.regalloc,
            .codegen => self.codegen,
            .strings => self.strings,
            .abi => self.abi,
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
pub fn log(phase: Phase, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(phase)) return;
    std.debug.print("[{s}] " ++ fmt ++ "\n", .{@tagName(phase)} ++ args);
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
    const phases = DebugPhases.parseStr("parse,ssa,regalloc");
    try std.testing.expect(phases.parse);
    try std.testing.expect(!phases.check);
    try std.testing.expect(!phases.lower);
    try std.testing.expect(phases.ssa);
    try std.testing.expect(phases.regalloc);
}

test "DebugPhases.all enables all phases" {
    const phases = DebugPhases.parseStr("all");
    try std.testing.expect(phases.isEnabled(.parse));
    try std.testing.expect(phases.isEnabled(.check));
    try std.testing.expect(phases.isEnabled(.lower));
    try std.testing.expect(phases.isEnabled(.ssa));
    try std.testing.expect(phases.isEnabled(.regalloc));
    try std.testing.expect(phases.isEnabled(.codegen));
}

test "DebugPhases individual phase check" {
    const phases = DebugPhases.parseStr("abi,codegen");
    try std.testing.expect(!phases.isEnabled(.parse));
    try std.testing.expect(phases.isEnabled(.abi));
    try std.testing.expect(phases.isEnabled(.codegen));
}

test "DebugPhases empty string" {
    const phases = DebugPhases.parseStr("");
    try std.testing.expect(!phases.isEnabled(.parse));
    try std.testing.expect(!phases.isEnabled(.ssa));
    try std.testing.expect(!phases.all);
}
