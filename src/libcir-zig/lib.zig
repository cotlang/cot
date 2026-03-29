//! libcir — Cot Intermediate Representation library.
//!
//! Language-agnostic: SSA builder, optimization passes, ARC, VWT,
//! generics, concurrency, Wasm emission, CIR binary serialization.

pub const ir = @import("ir.zig");
pub const arc = @import("arc_insertion.zig");

test {
    _ = ir;
    _ = arc;
}
