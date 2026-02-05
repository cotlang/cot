//! Slice Runtime Functions
//!
//! Go reference: runtime/slice.go
//! These are Go-style slice operations, NOT Swift ARC.

const std = @import("std");
const wasm_old = @import("wasm.zig"); // Old module with CodeBuilder
const wasm = @import("wasm/wasm.zig"); // New Go-style module for Linker
const wasm_link = @import("wasm/link.zig");
const ValType = wasm.ValType;

// Wasm block types (from spec)
const BLOCK_VOID: u8 = 0x40;
const BLOCK_I64: u8 = 0x7E;

/// Slice runtime function indices
pub const SliceFunctions = struct {
    growslice_idx: u32,
    nextslicecap_idx: u32,
};

/// Function names
pub const GROWSLICE_NAME = "cot_growslice";
pub const NEXTSLICECAP_NAME = "cot_nextslicecap";

/// Add slice runtime functions to linker.
/// Go reference: runtime/slice.go
pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker, heap_ptr_global: u32) !SliceFunctions {
    // nextslicecap: (newLen, oldCap) -> newCap
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

    // growslice: (old_ptr, old_len, old_cap, elem_ptr, elem_size) -> new_ptr
    const growslice_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const growslice_body = try generateGrowSliceBody(allocator, heap_ptr_global, nextslicecap_idx);
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
fn generateNextSliceCapBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm_old.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm_old.ValType{ .i64, .i64 });

    try code.emitLocalGet(1);
    try code.emitLocalSet(2);

    try code.emitLocalGet(2);
    try code.emitLocalGet(2);
    try code.emitI64Add();
    try code.emitLocalSet(3);

    try code.emitLocalGet(0);
    try code.emitLocalGet(3);
    try code.emitI64GtS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(0);
    try code.emitElse();

    try code.emitLocalGet(1);
    try code.emitI64Const(256);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(3);
    try code.emitElse();

    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    try code.emitLocalGet(2);
    try code.emitLocalGet(2);
    try code.emitI64Const(768);
    try code.emitI64Add();
    try code.emitI64Const(2);
    try code.emitI64ShrS();
    try code.emitI64Add();
    try code.emitLocalSet(2);

    try code.emitLocalGet(2);
    try code.emitLocalGet(0);
    try code.emitI64GeU();
    try code.emitBrIf(1);

    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    try code.emitLocalGet(2);
    try code.emitI64Const(0);
    try code.emitI64LeS();
    try code.emitIf(BLOCK_I64);
    try code.emitLocalGet(0);
    try code.emitElse();
    try code.emitLocalGet(2);
    try code.emitEnd();

    try code.emitEnd();
    try code.emitEnd();

    return code.finish();
}

/// Go reference: runtime/slice.go growslice (lines 178-287)
fn generateGrowSliceBody(allocator: std.mem.Allocator, heap_ptr_global: u32, nextslicecap_idx: u32) ![]const u8 {
    var code = wasm_old.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm_old.ValType{ .i64, .i64, .i32, .i32, .i32 });

    try code.emitLocalGet(1);
    try code.emitI64Const(1);
    try code.emitI64Add();
    try code.emitLocalSet(5);

    try code.emitLocalGet(1);
    try code.emitLocalGet(4);
    try code.emitI64Mul();
    try code.emitI32WrapI64();
    try code.emitLocalSet(9);

    try code.emitLocalGet(5);
    try code.emitLocalGet(2);
    try code.emitI64LeS();
    try code.emitIf(BLOCK_I64);

    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalGet(9);
    try code.emitI32Add();
    try code.emitLocalGet(3);
    try code.emitI32WrapI64();
    try code.emitLocalGet(4);
    try code.emitI32WrapI64();
    try code.emitMemoryCopy();

    try code.emitLocalGet(0);

    try code.emitElse();

    try code.emitLocalGet(5);
    try code.emitLocalGet(2);
    try code.emitCall(nextslicecap_idx);
    try code.emitLocalSet(6);

    try code.emitLocalGet(1);
    try code.emitLocalGet(4);
    try code.emitI64Mul();
    try code.emitI32WrapI64();
    try code.emitLocalSet(8);

    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalSet(7);

    try code.emitLocalGet(6);
    try code.emitLocalGet(4);
    try code.emitI64Mul();
    try code.emitI32WrapI64();
    try code.emitI32Const(7);
    try code.emitI32Add();
    try code.emitI32Const(-8);
    try code.emitI32And();

    try code.emitLocalGet(7);
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    try code.emitLocalGet(7);
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalGet(8);
    try code.emitMemoryCopy();

    try code.emitLocalGet(7);
    try code.emitLocalGet(9);
    try code.emitI32Add();
    try code.emitLocalGet(3);
    try code.emitI32WrapI64();
    try code.emitLocalGet(4);
    try code.emitI32WrapI64();
    try code.emitMemoryCopy();

    try code.emitLocalGet(7);
    try code.emitI64ExtendI32U();

    try code.emitEnd();

    return code.finish();
}
