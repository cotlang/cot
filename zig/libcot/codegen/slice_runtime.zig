//! Slice Runtime Functions
//!
//! Go reference: runtime/slice.go
//! These are Go-style slice operations, NOT Swift ARC.

const std = @import("std");
const wasm_old = @import("wasm.zig"); // Old module with CodeBuilder
const wasm = @import("wasm/wasm.zig"); // New Go-style module for Linker
const wasm_link = @import("wasm/link.zig");
const wasm_op = @import("wasm_opcodes.zig");
const arc = @import("arc.zig");
const ValType = wasm.ValType;

// Wasm block types (from spec)
const BLOCK_VOID: u8 = wasm_op.BLOCK_VOID;
const BLOCK_I64: u8 = @intFromEnum(wasm_op.ValType.i64);

/// Slice runtime function indices
pub const SliceFunctions = struct {
    growslice_idx: u32,
    nextslicecap_idx: u32,
};

/// Function names
pub const GROWSLICE_NAME = "growslice";
pub const NEXTSLICECAP_NAME = "nextslicecap";

/// Add slice runtime functions to linker.
/// Go reference: runtime/slice.go
pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker, heap_ptr_global: u32) !SliceFunctions {
    // nextslicecap: (newLen, oldCap) -> newCap
    // Go reference: runtime/slice.go nextslicecap (lines 326-358)
    const nextslicecap_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const nextslicecap_body = try generateNextSliceCapBody(allocator);
    const nextslicecap_idx = try linker.addFunc(.{
        .name = NEXTSLICECAP_NAME,
        .type_idx = nextslicecap_type,
        .code = nextslicecap_body,
        .exported = false,
    });

    // growslice: (old_ptr, old_len, new_cap, elem_size) -> new_ptr
    // Go reference: runtime/slice.go growslice (lines 178-287)
    // NOTE: Only handles allocation and copy. Capacity check and element
    // storage are done by the compiler (like Go's walkAppend).
    const growslice_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const growslice_body = try generateGrowSliceBody(allocator, heap_ptr_global);
    const growslice_idx = try linker.addFunc(.{
        .name = GROWSLICE_NAME,
        .type_idx = growslice_type,
        .code = growslice_body,
        .exported = false,
    });

    return SliceFunctions{
        .growslice_idx = growslice_idx,
        .nextslicecap_idx = nextslicecap_idx,
    };
}

/// Go reference: runtime/slice.go nextslicecap (lines 326-358)
///
/// func nextslicecap(newLen, oldCap int) int {
///     newcap := oldCap
///     doublecap := newcap + newcap
///     if newLen > doublecap {
///         return newLen
///     }
///     const threshold = 256
///     if oldCap < threshold {
///         return doublecap
///     }
///     for {
///         newcap += (newcap + 3*threshold) >> 2
///         if uint(newcap) >= uint(newLen) {
///             break
///         }
///     }
///     if newcap <= 0 {
///         return newLen
///     }
///     return newcap
/// }
fn generateNextSliceCapBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm_old.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: local 0 = newLen, local 1 = oldCap
    // Locals: local 2 = newcap, local 3 = doublecap
    _ = try code.declareLocals(&[_]wasm_old.ValType{ .i64, .i64 });

    // newcap = oldCap
    try code.emitLocalGet(1);
    try code.emitLocalSet(2);

    // doublecap = newcap + newcap
    try code.emitLocalGet(2);
    try code.emitLocalGet(2);
    try code.emitI64Add();
    try code.emitLocalSet(3);

    // if (newLen > doublecap) return newLen
    try code.emitLocalGet(0);
    try code.emitLocalGet(3);
    try code.emitI64GtS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(0);
    try code.emitElse();

    // if (oldCap < 256) return doublecap
    try code.emitLocalGet(1);
    try code.emitI64Const(256);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(3);
    try code.emitElse();

    // Loop: grow by ~25% until newcap >= newLen
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);

    // newcap += (newcap + 3*256) >> 2
    try code.emitLocalGet(2);
    try code.emitLocalGet(2);
    try code.emitI64Const(768); // 3 * 256
    try code.emitI64Add();
    try code.emitI64Const(2);
    try code.emitI64ShrS();
    try code.emitI64Add();
    try code.emitLocalSet(2);

    // if (uint(newcap) >= uint(newLen)) break
    try code.emitLocalGet(2);
    try code.emitLocalGet(0);
    try code.emitI64GeU();
    try code.emitBrIf(1);

    // continue loop
    try code.emitBr(0);
    try code.emitEnd(); // end loop
    try code.emitEnd(); // end block

    // if (newcap <= 0) return newLen else return newcap
    try code.emitLocalGet(2);
    try code.emitI64Const(0);
    try code.emitI64LeS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(0);
    try code.emitElse();
    try code.emitLocalGet(2);
    try code.emitEnd();

    try code.emitEnd(); // end else (oldCap >= 256)
    try code.emitEnd(); // end else (newLen <= doublecap)

    return code.finish();
}

/// Go reference: runtime/slice.go growslice (lines 178-287)
///
/// Simplified signature for Cot:
///   growslice(old_ptr, old_len, new_cap, elem_size) -> new_ptr
///
/// Go's original:
///   func growslice(oldPtr unsafe.Pointer, newLen, oldCap, num int, et *_type) slice
///
/// Key differences from Go:
/// - We take new_cap directly (caller computes via nextslicecap)
/// - We don't store the new element (caller does that)
/// - We return just new_ptr (caller knows new_cap already)
///
/// This matches Go's division of labor:
/// - Compiler (walkAppend): capacity check, element storage
/// - Runtime (growslice): allocation, memcpy
fn generateGrowSliceBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm_old.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters:
    //   local 0: old_ptr (i64)
    //   local 1: old_len (i64)
    //   local 2: new_cap (i64)
    //   local 3: elem_size (i64)
    // Locals:
    //   local 4: new_ptr (i32)
    //   local 5: old_size (i32) - bytes to copy
    //   local 6: alloc_size (i32)
    //   local 7: src_i32 (i32) - for byte-copy loop
    //   local 8: counter (i32) - byte-copy loop counter
    _ = try code.declareLocals(&[_]wasm_old.ValType{ .i32, .i32, .i32, .i32, .i32 });

    // old_size = (i32)(old_len * elem_size)
    try code.emitLocalGet(1); // old_len
    try code.emitLocalGet(3); // elem_size
    try code.emitI64Mul();
    try code.emitI32WrapI64();
    try code.emitLocalSet(5);

    // alloc_size = (new_cap * elem_size + ALIGN_MINUS_ONE) & ALIGN_MASK
    try code.emitLocalGet(2); // new_cap
    try code.emitLocalGet(3); // elem_size
    try code.emitI64Mul();
    try code.emitI32WrapI64();
    try code.emitI32Const(arc.ALIGN_MINUS_ONE);
    try code.emitI32Add();
    try code.emitI32Const(arc.ALIGN_MASK);
    try code.emitI32And();
    try code.emitLocalSet(6);

    // new_ptr = heap_ptr (bump allocator)
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalSet(4);

    // heap_ptr += alloc_size
    try code.emitLocalGet(4);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // memcpy(new_ptr, old_ptr, old_size)
    // Go reference: memmove(p, oldPtr, lenmem) at line 284
    try code.emitLocalGet(0); // old_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(7); // src_i32
    // dest=local 4 (new_ptr), src=local 7, len=local 5, counter=local 8
    try code.emitByteCopyLoop(4, 7, 5, 8);

    // return (i64)new_ptr
    try code.emitLocalGet(4);
    try code.emitI64ExtendI32U();

    return code.finish();
}
