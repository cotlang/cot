//! ABI Definitions Stub
//!
//! NOTE: This is a minimal stub for the Cranelift port transition.
//! Full ABI implementation will be added in Phase 3 of the port.
//! See CRANELIFT_PORT_MASTER_PLAN.md for details.
//!
//! The types here are referenced by ssa/value.zig's AuxCall structure,
//! which is used for native function call handling.

/// Register index type placeholder
pub const RegIndex = u8;

/// ABI parameter/result info placeholder
pub const ABIParamResultInfo = struct {
    dummy: u8 = 0,
};
