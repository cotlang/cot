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
}
