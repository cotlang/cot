//! Cot Debug Module
//!
//! Comprehensive debugging infrastructure for the cot compiler and runtime.
//!
//! Components:
//! - trace: Execution tracing with configurable levels
//! - history: Ring buffer for execution history
//! - output: Multi-format output (human, JSON, compact)
//!
//! Usage:
//!   const debug = @import("debug");
//!   var tracer = debug.Tracer.init(allocator, .{ .level = .opcodes });
//!   vm.setTracer(&tracer);
//!   defer tracer.deinit();

pub const trace = @import("trace.zig");
pub const history = @import("history.zig");
pub const output = @import("output.zig");

// Re-export main types for convenience
pub const Tracer = trace.Tracer;
pub const TraceLevel = trace.TraceLevel;
pub const TraceConfig = trace.TraceConfig;
pub const TraceEntry = trace.TraceEntry;
pub const TraceEvent = trace.TraceEvent;
pub const TraceFilter = trace.TraceFilter;
pub const TraceStats = trace.TraceStats;

pub const History = history.History;

pub const Output = output.Output;
pub const OutputFormat = output.Format;
pub const OutputConfig = output.Config;
pub const Color = output.Color;

// ============================================================================
// Quick Start Helpers
// ============================================================================

/// Create a tracer with opcode-level tracing to stderr
pub fn opcodeTracer(allocator: @import("std").mem.Allocator) Tracer {
    return Tracer.init(allocator, .{
        .level = .opcodes,
        .output = .{ .format = .human, .color = true },
    });
}

/// Create a tracer with verbose tracing (includes registers)
pub fn verboseTracer(allocator: @import("std").mem.Allocator) Tracer {
    return Tracer.init(allocator, .{
        .level = .verbose,
        .output = .{ .format = .human, .color = true },
    });
}

/// Create a tracer that only records history (no output)
pub fn historyOnlyTracer(allocator: @import("std").mem.Allocator, size: u16) Tracer {
    return Tracer.init(allocator, .{
        .level = .none,
        .history_size = size,
    });
}

/// Create a tracer with JSON output to file
pub fn jsonFileTracer(allocator: @import("std").mem.Allocator, path: []const u8) Tracer {
    return Tracer.init(allocator, .{
        .level = .opcodes,
        .output = .{
            .format = .json,
            .target = .file,
            .file_path = path,
        },
    });
}

// ============================================================================
// Tests
// ============================================================================

test {
    @import("std").testing.refAllDecls(@This());
}
