//! libcir — Cot Intermediate Representation library.
//!
//! Language-agnostic: SSA builder, optimization passes, ARC, VWT,
//! generics, concurrency, Wasm emission, CIR binary serialization.

pub const ir = @import("ir.zig");
pub const arc = @import("arc_insertion.zig");
pub const vwt_gen = @import("vwt_gen.zig");
pub const ssa_op = @import("ssa/op.zig");
pub const ssa_value = @import("ssa/value.zig");
pub const ssa_block = @import("ssa/block.zig");
pub const ssa_func = @import("ssa/func.zig");
pub const ssa_dom = @import("ssa/dom.zig");
pub const ssa_debug = @import("ssa/debug.zig");
pub const ssa_html = @import("ssa/html.zig");
pub const ssa_builder = @import("ssa_builder.zig");
pub const pass_schedule = @import("ssa/passes/schedule.zig");
pub const pass_deadcode = @import("ssa/passes/deadcode.zig");
pub const pass_lower_wasm = @import("ssa/passes/lower_wasm.zig");
pub const pass_lower_native = @import("ssa/passes/lower_native.zig");
pub const pass_decompose = @import("ssa/passes/decompose.zig");
pub const pass_rewritegeneric = @import("ssa/passes/rewritegeneric.zig");
pub const pass_rewritedec = @import("ssa/passes/rewritedec.zig");
pub const pass_copyelim = @import("ssa/passes/copyelim.zig");
pub const pass_phielim = @import("ssa/passes/phielim.zig");
pub const pass_cse = @import("ssa/passes/cse.zig");
pub const pass_async_split = @import("ssa/passes/async_split.zig");
pub const pass_layout = @import("ssa/passes/layout.zig");
pub const pass_region_isolation = @import("ssa/passes/region_isolation.zig");
pub const wasm = @import("wasm/wasm.zig");

test {
    _ = ir;
    _ = arc;
    _ = vwt_gen;
    _ = ssa_op;
    _ = ssa_value;
    _ = ssa_block;
    _ = ssa_func;
    _ = ssa_dom;
    _ = ssa_debug;
    _ = ssa_html;
    _ = ssa_builder;
    _ = pass_schedule;
    _ = pass_deadcode;
    _ = pass_lower_wasm;
    _ = pass_lower_native;
    _ = pass_decompose;
    _ = pass_rewritegeneric;
    _ = pass_rewritedec;
    _ = pass_copyelim;
    _ = pass_phielim;
    _ = pass_cse;
    _ = pass_async_split;
    _ = pass_layout;
    _ = pass_region_isolation;
    _ = wasm;
}
