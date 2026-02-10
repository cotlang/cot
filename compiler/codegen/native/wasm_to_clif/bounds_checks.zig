//! Wasm memory bounds checking.
//!
//! Port of wasmtime/crates/cranelift/src/bounds_checks.rs (simplified)
//!
//! This module provides bounds checking for Wasm memory accesses.
//! For now we use a simple explicit bounds check without guard page
//! optimizations or Spectre mitigations.

const std = @import("std");
const frontend_mod = @import("../frontend/mod.zig");
const heap_mod = @import("heap.zig");

pub const FunctionBuilder = frontend_mod.FunctionBuilder;
pub const Value = frontend_mod.Value;
pub const Type = frontend_mod.Type;
pub const clif = frontend_mod;

pub const HeapData = heap_mod.HeapData;

// ============================================================================
// TrapCode
// Port of cranelift ir::TrapCode
// ============================================================================

/// Trap codes for Wasm runtime errors.
pub const TrapCode = enum(u8) {
    /// Heap out of bounds access.
    heap_out_of_bounds,
    /// Table out of bounds access.
    table_out_of_bounds,
    /// Integer division by zero.
    integer_division_by_zero,
    /// Integer overflow.
    integer_overflow,
    /// Bad signature in indirect call.
    bad_signature,
    /// Unreachable code executed.
    unreachable_code,
};

// ============================================================================
// Bounds Check Implementation
// Simplified port of bounds_checks.rs:bounds_check_field_access
// ============================================================================

/// Bounds check result.
pub const BoundsCheckResult = struct {
    /// The computed native address.
    addr: Value,
    /// Whether the access is known to be in-bounds at compile time.
    known_in_bounds: bool,
};

/// Compute address and perform bounds check for a memory access.
///
/// Port of Cranelift's explicit_check_oob_condition_and_compute_addr
/// (bounds_checks.rs:745-762) with the general case (bounds_checks.rs:565-607).
///
/// Emits: effective_end = index + offset + access_size
///        oob = icmp(uge, effective_end, bound)
///        trapnz(oob, heap_out_of_bounds)
///        addr = base + index + offset
///
/// Parameters:
/// - builder: Function builder for emitting IR
/// - heap: Heap data with base/bound GlobalValues
/// - index: The Wasm memory index (dynamic)
/// - offset: Static offset from memarg
/// - access_size: Size of the memory access in bytes
///
/// Returns the native address to use for the load/store.
pub fn boundsCheckAndComputeAddr(
    builder: *FunctionBuilder,
    heap: *const HeapData,
    index: Value,
    offset: u64,
    access_size: u8,
) !Value {
    const pointer_type = Type.I64;

    // Step 1: Extend index to pointer type if needed
    var addr_index = index;
    const index_type = builder.func.dfg.valueType(index);
    if (index_type.bits() < pointer_type.bits()) {
        addr_index = try builder.ins().uextend(pointer_type, index);
    }

    // Bounds check: emit icmp + trapnz to prevent OOB memory access.
    // Port of Cranelift bounds_checks.rs:565-607 (general case).
    //
    // Load the dynamic bound from vmctx (supports memory.grow).
    // The globalValue+load pattern is required — using iconst triggers a
    // CLIF value aliasing bug in large programs (0xAAAAAAAA in resolveAllAliases).
    const offset_and_size = offset + @as(u64, access_size);
    const oas_val = try builder.ins().iconst(pointer_type, @as(i64, @intCast(offset_and_size)));
    const effective_end = try builder.ins().iadd(addr_index, oas_val);

    const bound_gv_addr = try builder.ins().globalValue(pointer_type, heap.bound);
    const bound = try builder.ins().load(pointer_type, clif.MemFlags.DEFAULT, bound_gv_addr, 0);

    const oob = try builder.ins().icmp(clif.IntCC.uge, effective_end, bound);
    _ = try builder.ins().trapnz(oob, clif.TrapCode.heap_out_of_bounds);

    // Step 5: Compute final address: base + index + offset
    const base_gv_addr = try builder.ins().globalValue(pointer_type, heap.base);
    const base = try builder.ins().load(pointer_type, clif.MemFlags.DEFAULT, base_gv_addr, 0);

    var final_addr = try builder.ins().iadd(base, addr_index);

    if (offset > 0) {
        const offset_val = try builder.ins().iconst(pointer_type, @as(i64, @intCast(offset)));
        final_addr = try builder.ins().iadd(final_addr, offset_val);
    }

    return final_addr;
}

// ============================================================================
// Tests
// ============================================================================

test "TrapCode values" {
    const testing = std.testing;

    try testing.expectEqual(@as(u8, 0), @intFromEnum(TrapCode.heap_out_of_bounds));
    try testing.expectEqual(@as(u8, 1), @intFromEnum(TrapCode.table_out_of_bounds));
}
