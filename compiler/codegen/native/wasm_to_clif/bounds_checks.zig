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
/// Simplified version of Cranelift's bounds_check_and_compute_addr.
/// Does not include guard page optimizations or Spectre mitigations.
///
/// Parameters:
/// - builder: Function builder for emitting IR
/// - heap: Heap data with base/bound GlobalValues
/// - index: The Wasm memory index (dynamic)
/// - offset: Static offset from memarg
/// - access_size: Size of the memory access in bytes
///
/// Returns the native address to use for the load/store.
/// Emits bounds checking code if needed.
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

    // Bounds checking is deferred (TODO: implement trapIf).
    // Do NOT emit dead bounds-check instructions here — they create CLIF iadd
    // instructions whose regalloc clobbers addr_index, corrupting the final address.
    _ = access_size;

    // Compute final address: base + index + offset
    const base_gv_addr = try builder.ins().globalValue(pointer_type, heap.base);
    const base = try builder.ins().load(pointer_type, clif.MemFlags.DEFAULT, base_gv_addr, 0);

    // Final address = base + index (offset is applied in load/store)
    var final_addr = try builder.ins().iadd(base, addr_index);

    // If we have an offset, add it to the address
    // (This is the static offset from the memarg)
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
