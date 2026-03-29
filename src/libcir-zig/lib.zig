//! libcir — Cot Intermediate Representation library.
//!
//! Language-agnostic: SSA builder, optimization passes, ARC, VWT,
//! generics, concurrency, Wasm emission, CIR binary serialization.

pub const ir = @import("ir.zig");
pub const arc = @import("arc_insertion.zig");
pub const vwt_gen = @import("vwt_gen.zig");

test {
    _ = ir;
    _ = arc;
    _ = vwt_gen;
}
