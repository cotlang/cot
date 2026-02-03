//! Heap data for WebAssembly linear memories.
//!
//! Port of wasmtime/crates/cranelift/src/translate/heap.rs
//!
//! This module provides the HeapData struct which represents how a Wasm
//! linear memory is mapped to CLIF IR.

const std = @import("std");
const frontend_mod = @import("../frontend/mod.zig");

pub const GlobalValue = frontend_mod.GlobalValue;
pub const Type = frontend_mod.Type;

// ============================================================================
// HeapData
// Port of cranelift-wasm HeapData struct
// ============================================================================

/// A heap implementing a WebAssembly linear memory.
///
/// Code compiled from WebAssembly runs in a sandbox where it can't access all
/// process memory. Instead, it is given a small set of memory areas to work in,
/// and all accesses are bounds checked.
///
/// HeapData stores the GlobalValue references needed to:
/// 1. Compute the base address of the memory
/// 2. Get the current bound (size) of the memory
pub const HeapData = struct {
    /// GlobalValue that computes the base address of the heap.
    /// Loaded from vmctx + heap_base_offset.
    base: GlobalValue,

    /// GlobalValue that computes the address of the heap bound.
    /// Loaded from vmctx + heap_bound_offset.
    bound: GlobalValue,

    /// The type of Wasm memory index (I32 for Wasm32, I64 for Wasm64).
    index_type: Type,

    /// Minimum guaranteed size in bytes.
    min_size: u64,

    /// Maximum size in bytes, if known.
    max_size: ?u64,

    /// Size of the guard region in bytes (for eliding bounds checks).
    /// 0 means no guard pages.
    offset_guard_size: u64,

    const Self = @This();

    /// Check if this heap has guard pages that allow eliding bounds checks.
    pub fn hasGuardPages(self: Self) bool {
        return self.offset_guard_size > 0;
    }

    /// Get the index type bit width.
    pub fn indexTypeBits(self: Self) u32 {
        return self.index_type.bits();
    }
};

// ============================================================================
// MemArg
// Wasm memory argument (offset, alignment, memory index)
// ============================================================================

/// Wasm memory argument.
///
/// Every Wasm load/store instruction has a MemArg immediate that specifies:
/// - offset: static offset to add to the dynamic index
/// - align_: alignment hint (not enforced, just for optimization)
/// - memory: which memory to access (usually 0)
pub const MemArg = struct {
    /// Alignment as a power of 2 (e.g., 2 means 4-byte aligned).
    /// This is a hint; Wasm doesn't require alignment.
    align_: u32,

    /// Static offset added to the dynamic index.
    offset: u64,

    /// Memory index (usually 0 for single-memory modules).
    memory: u32,

    pub const DEFAULT: MemArg = .{
        .align_ = 0,
        .offset = 0,
        .memory = 0,
    };
};

// ============================================================================
// Tests
// ============================================================================

test "HeapData basic" {
    const testing = std.testing;

    const heap = HeapData{
        .base = GlobalValue.fromIndex(0),
        .bound = GlobalValue.fromIndex(1),
        .index_type = Type.I32,
        .min_size = 65536,
        .max_size = null,
        .offset_guard_size = 0,
    };

    try testing.expect(!heap.hasGuardPages());
    try testing.expectEqual(@as(u32, 32), heap.indexTypeBits());
}

test "HeapData with guard pages" {
    const testing = std.testing;

    const heap = HeapData{
        .base = GlobalValue.fromIndex(0),
        .bound = GlobalValue.fromIndex(1),
        .index_type = Type.I64,
        .min_size = 65536,
        .max_size = 1 << 32,
        .offset_guard_size = 1 << 16, // 64KB guard
    };

    try testing.expect(heap.hasGuardPages());
    try testing.expectEqual(@as(u32, 64), heap.indexTypeBits());
}

test "MemArg default" {
    const testing = std.testing;

    const memarg = MemArg.DEFAULT;
    try testing.expectEqual(@as(u32, 0), memarg.align_);
    try testing.expectEqual(@as(u64, 0), memarg.offset);
    try testing.expectEqual(@as(u32, 0), memarg.memory);
}
