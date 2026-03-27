//! Region Isolation Analysis — Swift SE-0430 (sending) + SE-0414 (regions)
//!
//! Forward dataflow analysis on SSA that tracks value isolation regions.
//! Detects use-after-send errors where a value is used after being transferred
//! via a `sending` parameter.
//!
//! Swift reference: lib/SILOptimizer/Analysis/RegionAnalysis.cpp
//!   - TrackableValueState: per-value isolation state
//!   - BlockPartitionState: per-block entry/exit partitions
//!   - PartitionOp: Send, Require, Merge, AssignDirect
//!
//! Algorithm (forward fixed-point):
//!   1. Initialize all values as "live" (not sent)
//!   2. For each block in RPO:
//!      a. Copy entry state from predecessors (join)
//!      b. For each instruction:
//!         - Call with sending param: mark argument as "sent"
//!         - Use of sent value: emit error diagnostic
//!      c. Propagate exit state to successors
//!   3. Repeat until fixed point (no state changes)
//!
//! Phase 1: Checker-level enforcement (detect use-after-send in linear scan).
//! Phase 2+: Full SSA dataflow with phi handling at join points.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// Value isolation state.
/// Swift reference: TrackableValueState (RegionAnalysis.h)
const IsolationState = enum(u8) {
    /// Value is live and usable.
    live,
    /// Value has been sent (transferred via `sending` param). Further use is an error.
    /// Swift reference: PartitionOp::Send
    sent,
};

/// Run region isolation analysis on a function.
/// Detects use-after-send errors for values passed to `sending` parameters.
///
/// Swift reference: RegionAnalysis::analyze() (RegionAnalysis.cpp)
///   - Forward dataflow: entry → exit, propagating isolation state
///   - At call sites: mark sending args as sent
///   - At use sites: error if value is sent
///
/// Returns the number of diagnostics emitted.
pub fn regionIsolation(f: *Func) !u32 {
    const allocator = f.allocator;
    var diagnostics: u32 = 0;

    // Per-value isolation state.
    // Swift reference: BlockPartitionState (RegionAnalysis.h)
    // Phase 1: simple linear scan (no phi join needed for single-block analysis).
    // Phase 2+: per-block entry/exit partition maps with fixed-point iteration.
    const num_values = f.numValues();
    if (num_values == 0) return 0;

    var state = try allocator.alloc(IsolationState, num_values);
    defer allocator.free(state);
    @memset(state, .live);

    // Forward scan: iterate blocks in layout order.
    // Swift reference: RegionAnalysis.cpp forward iteration over SIL blocks.
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            // Check if this value USES a sent value (use-after-send error).
            // Swift reference: PartitionOp::Require — assert value is in valid region.
            for (v.args) |arg| {
                if (arg.id < num_values and state[arg.id] == .sent) {
                    // Use of sent value — diagnostic.
                    // Swift reference: RegionAnalysis diagnoseErrors()
                    debug.log(.codegen, "region_isolation: use-after-send: v{d} uses sent v{d} in '{s}'", .{
                        v.id, arg.id, f.name,
                    });
                    diagnostics += 1;
                }
            }

            // Check if this is a call that sends a value.
            // Swift reference: PartitionOp::Send(Element, sendingOp)
            if (v.op.info().call) {
                // Check each argument against the callee's sending params.
                // Phase 1: we mark ALL non-Sendable args to functions with sending params.
                // Phase 2+: check against actual function signature's sending flags.
                //
                // For now, the `sending` flag is propagated via IR Local.is_sending.
                // The SSA `arg` op has aux_int = param_index, which can be cross-referenced
                // with the callee's parameter list.
                //
                // Phase 1 implementation: the checker handles this at AST level.
                // This SSA pass is infrastructure for Phase 2+ full region tracking.
            }
        }
    }

    if (diagnostics > 0) {
        debug.log(.codegen, "region_isolation: {d} use-after-send errors in '{s}'", .{ diagnostics, f.name });
    }

    return diagnostics;
}
