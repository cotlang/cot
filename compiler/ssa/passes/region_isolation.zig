//! Region Isolation Analysis — Swift SE-0430 (sending) + SE-0414 (regions)
//!
//! Forward dataflow analysis on SSA that tracks value isolation regions.
//! Detects use-after-send errors where a value is used after being transferred
//! via a `sending` parameter.
//!
//! Swift reference: lib/SILOptimizer/Analysis/RegionAnalysis.cpp
//!   - Partition: Element → Region map (PartitionUtils.h:771-1051)
//!   - PartitionOp: Send, Require, Merge, AssignDirect, AssignFresh
//!   - BlockPartitionState: per-block entry/exit partitions (RegionAnalysis.h:56-111)
//!   - Fixed-point iteration in reverse post-order (RegionAnalysis.cpp:4641-4704)
//!
//! Algorithm (forward fixed-point):
//!   1. Initialize all values as "live" (region = self, not sent)
//!   2. For each block in RPO:
//!      a. Join entry state from all predecessors
//!      b. Apply PartitionOps for each instruction:
//!         - Call with sending param: Send(arg) — mark region as sent
//!         - Use of sent value: Require(arg) — error if region is sent
//!         - Assignment: AssignDirect(dst, src) — dst joins src's region
//!         - Allocation: AssignFresh(val) — new region
//!      c. Propagate exit state to successors
//!   3. Repeat until fixed point (no state changes)

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// Value isolation state.
/// Swift reference: TrackableValueState (RegionAnalysis.h:166-180)
const IsolationState = enum(u8) {
    /// Value is live and usable. Region has not been sent.
    live,
    /// Value's region has been sent (transferred via `sending` param).
    /// Any further use is a use-after-send error.
    /// Swift reference: PartitionOp::Send marks region, PartitionOp::Require detects
    sent,
};

/// PartitionOp kinds — primitive operations on the isolation partition.
/// Swift reference: PartitionOpKind (PartitionUtils.h:461-542)
const PartitionOpKind = enum(u8) {
    /// Mark a value's region as sent (transferred across isolation boundary).
    /// Swift reference: PartitionOp::Send(element, sendingOp)
    send,
    /// Assert a value's region is not sent (use-after-send check).
    /// Swift reference: PartitionOp::Require(element)
    require,
    /// Assign one value to the same region as another.
    /// Swift reference: PartitionOp::AssignDirect(dst, src)
    assign_direct,
    /// Assign a value to a fresh (new) region.
    /// Swift reference: PartitionOp::AssignFresh(element)
    assign_fresh,
    /// Merge two values into the same region.
    /// Swift reference: PartitionOp::Merge(elt1, elt2)
    merge,
};

/// A PartitionOp instance.
/// Swift reference: PartitionOp (PartitionUtils.h:548-768)
const PartitionOp = struct {
    kind: PartitionOpKind,
    arg1: u32, // Element (SSA value ID)
    arg2: u32 = 0, // Second element (for assign_direct, merge)
};

/// Run region isolation analysis on a function.
/// Detects use-after-send errors for values passed to `sending` parameters.
///
/// Swift reference: RegionAnalysisFunctionInfo::runDataflow() (RegionAnalysis.cpp:4641-4704)
///   - Forward dataflow: entry → exit, propagating isolation state
///   - At call sites with sending params: Send(arg)
///   - At use sites of sent values: Require(arg) → error
///
/// Returns the number of diagnostics emitted.
pub fn regionIsolation(f: *Func) !u32 {
    const allocator = f.allocator;
    var diagnostics: u32 = 0;

    const num_values = f.numValues();
    if (num_values == 0) return 0;

    // Per-value isolation state.
    // Swift reference: Partition element-to-region map.
    // Phase 1: simple per-value state (no region merging yet).
    // Phase 2+: full Partition with Element→Region map and region merging.
    var state = try allocator.alloc(IsolationState, num_values);
    defer allocator.free(state);
    @memset(state, .live);

    // Forward scan: iterate blocks in layout order.
    // Swift reference: RegionAnalysis.cpp:4656 — visit blocks in reverse post-order.
    // Layout order approximates RPO for reducible CFGs (sufficient for Phase 1).
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            // ================================================================
            // PartitionOp::Require — check all arguments for use-after-send.
            // Swift reference: PartitionUtils.h PartitionOp::Require
            //   Evaluator::handleRequire checks if element's region is sent.
            //   If sent, emits LocalUseAfterSendError.
            // ================================================================
            for (v.args) |arg| {
                if (arg.id < num_values and state[arg.id] == .sent) {
                    debug.log(.codegen, "region_isolation: use-after-send: v{d} uses sent v{d} in '{s}'", .{
                        v.id, arg.id, f.name,
                    });
                    diagnostics += 1;
                }
            }

            // ================================================================
            // PartitionOp::Send — mark sending args as sent at call sites.
            // Swift reference: PartitionOpTranslator::getPartitionOpsForInstruction()
            //   For apply instructions with sending parameters, emit Send(arg).
            // ================================================================
            if (v.op.info().call) {
                // Check each argument for is_sending flag via IR locals.
                // The SSA `arg` op carries the parameter index as aux_int.
                // Phase 1: the checker handles use-after-send at AST level.
                // The SSA pass provides additional coverage for complex CFG patterns.
                for (v.args) |arg| {
                    if (arg.op == .arg) {
                        // Check if this arg's corresponding IR local has is_sending.
                        // For now, the checker handles this — SSA pass is future-proofing.
                        if (arg.aux_int != null and f.locals != null) {
                            const param_idx: usize = @intCast(arg.aux_int.?);
                            if (param_idx < f.locals.?.len and f.locals.?[param_idx].is_sending) {
                                if (arg.id < num_values) {
                                    state[arg.id] = .sent;
                                    debug.log(.codegen, "region_isolation: Send(v{d}) in '{s}'", .{ arg.id, f.name });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (diagnostics > 0) {
        debug.log(.codegen, "region_isolation: {d} use-after-send errors in '{s}'", .{ diagnostics, f.name });
    }

    return diagnostics;
}
