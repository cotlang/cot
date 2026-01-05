//! Cot Debug Module
//!
//! Comprehensive debugging infrastructure for the Cot compiler and runtime.
//! Provides:
//! - Execution tracing with multiple verbosity levels
//! - Breakpoint management (shared between CLI and LSP/DAP)
//! - Runtime state inspection
//! - Bytecode validation
//!
//! ## Usage
//!
//! ### Basic Tracing
//! ```zig
//! const debug = @import("debug/debug.zig");
//! var tracer = debug.Tracer.init(allocator, .{ .level = .verbose });
//! defer tracer.deinit();
//! vm.setTracer(&tracer);
//! ```
//!
//! ### Breakpoints
//! ```zig
//! var bp_mgr = debug.BreakpointManager.init(allocator);
//! defer bp_mgr.deinit();
//! _ = try bp_mgr.addAtLine(42, null);
//! _ = try bp_mgr.addAtAddress(0x0100);
//! ```
//!
//! ### State Inspection
//! ```zig
//! const inspector = debug.StateInspector{...};
//! const locals = try inspector.getLocals(allocator);
//! defer allocator.free(locals);
//! ```
//!
//! ### Bytecode Validation
//! ```zig
//! var validator = debug.Validator.init(allocator, module);
//! defer validator.deinit();
//! const result = try validator.validate();
//! ```

const std = @import("std");

// Re-export all debug components
pub const breakpoint = @import("breakpoint.zig");
pub const inspector = @import("inspector.zig");
pub const validator = @import("validator.zig");

// Re-export trace components from parent directory
pub const trace = @import("../trace/trace.zig");
pub const history = @import("../trace/history.zig");
pub const output = @import("../trace/output.zig");

// Convenient type aliases
pub const BreakpointManager = breakpoint.BreakpointManager;
pub const Breakpoint = breakpoint.Breakpoint;
pub const BreakpointKind = breakpoint.BreakpointKind;
pub const BreakpointState = breakpoint.BreakpointState;
pub const BreakpointResult = breakpoint.BreakpointResult;

pub const StateInspector = inspector.StateInspector;
pub const LocalInfo = inspector.LocalInfo;
pub const FrameInfo = inspector.FrameInfo;
pub const InstructionInfo = inspector.InstructionInfo;
pub const RegisterInfo = inspector.RegisterInfo;
pub const VMSnapshot = inspector.VMSnapshot;

pub const Validator = validator.Validator;
pub const ValidationResult = validator.ValidationResult;
pub const ValidationIssue = validator.ValidationIssue;
pub const InstructionWalker = validator.InstructionWalker;

pub const Tracer = trace.Tracer;
pub const TraceLevel = trace.TraceLevel;
pub const TraceConfig = trace.TraceConfig;
pub const TraceEntry = trace.TraceEntry;
pub const TraceFilter = trace.TraceFilter;

pub const History = history.History;
pub const Output = output.Output;
pub const OutputConfig = output.Config;
pub const OutputFormat = output.Format;

/// Quick debug configuration presets
pub const Presets = struct {
    /// Minimal tracing - only errors and crash dumps
    pub const minimal = TraceConfig{
        .level = .none,
        .history_size = 64,
    };

    /// Standard development tracing
    pub const standard = TraceConfig{
        .level = .opcodes,
        .history_size = 128,
    };

    /// Verbose debugging with register state
    pub const verbose = TraceConfig{
        .level = .verbose,
        .history_size = 256,
    };

    /// Full debugging with everything
    pub const full = TraceConfig{
        .level = .full,
        .history_size = 512,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "debug module imports" {
    // Ensure all imports are valid
    _ = BreakpointManager;
    _ = StateInspector;
    _ = Validator;
    _ = Tracer;
}
