//! libcir — Cot Intermediate Representation library.
//!
//! Language-agnostic: SSA builder, optimization passes, ARC, VWT,
//! generics, concurrency, Wasm emission, CIR binary serialization.

pub const ir = @import("ir.zig");

test {
    _ = ir;
}
