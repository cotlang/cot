//! Native Lowering Pass — Prepare SSA for direct CLIF translation.
//!
//! Counterpart to lower_wasm.zig for the direct native backend.
//! Unlike the Wasm path, generic SSA ops map naturally to CLIF, so this pass
//! is mostly identity for now. It exists as the extension point for
//! native-specific transformations (e.g., ABI lowering, struct decomposition).
//!
//! After this pass, ssa_to_clif.zig translates SSA → CLIF IR.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// Lower a function's SSA ops for native CLIF translation.
///
/// Currently an identity pass — generic ops (add, sub, load, store, etc.)
/// are handled directly by ssa_to_clif.zig. This pass will grow to handle:
/// - ABI-specific struct decomposition
/// - Large return lowering (hidden return pointer)
/// - Platform-specific call convention adjustments
pub fn lower(f: *Func) !void {
    debug.log(.codegen, "lower_native: processing '{s}'", .{f.name});

    var lowered: usize = 0;
    var unchanged: usize = 0;

    for (f.blocks.items) |block| {
        for (block.values.items) |v| {
            if (lowerValue(v)) {
                lowered += 1;
            } else {
                unchanged += 1;
            }
        }
    }

    debug.log(.codegen, "  lowered {d}, unchanged {d}", .{ lowered, unchanged });
}

/// Lower a single value. Returns true if the op was changed.
fn lowerValue(v: *Value) bool {
    // Phase 1: identity pass — no transformations needed.
    // Generic ops (add, sub, mul, load, store, etc.) map directly to CLIF.
    // Wasm-specific ops should not appear in the native path.
    _ = v;
    return false;
}
