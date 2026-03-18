//! Memory utility functions for Wasm (non-ARC)
//!
//! These functions were previously bundled in arc.zig but are general-purpose
//! memory operations needed by stdlib (string.cot, list.cot, etc.) regardless
//! of whether ARC is enabled. Extracted so WasmGC path still has them.
//!
//! Functions: alloc, dealloc, realloc, memcpy, memset_zero, string_eq, string_concat

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/link.zig");
const ValType = @import("wasm/wasm.zig").ValType;
const wasm_op = @import("wasm_opcodes.zig");

const BLOCK_VOID: u8 = 0x40;

pub const ALLOC_NAME = "alloc";
pub const DEALLOC_NAME = "dealloc";
pub const REALLOC_NAME = "realloc";
pub const ALLOC_RAW_NAME = "alloc_raw";
pub const REALLOC_RAW_NAME = "realloc_raw";
pub const DEALLOC_RAW_NAME = "dealloc_raw";
pub const MEMCPY_NAME = "memcpy";
pub const MEMSET_ZERO_NAME = "memset_zero";
pub const STRING_EQ_NAME = "string_eq";
pub const STRING_CONCAT_NAME = "string_concat";

pub const MemFunctions = struct {
    alloc_idx: u32,
    dealloc_idx: u32,
    realloc_idx: u32,
    alloc_raw_idx: u32,
    realloc_raw_idx: u32,
    dealloc_raw_idx: u32,
    memcpy_idx: u32,
    memset_zero_idx: u32,
    string_eq_idx: u32,
    string_concat_idx: u32,
};

pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker, heap_ptr_global: u32) !MemFunctions {
    // alloc(metadata: i64, size: i64) -> i64 (bump allocator, no ARC header)
    const i64_2i64_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const alloc_body = try generateAllocBody(allocator, heap_ptr_global);
    const alloc_idx = try linker.addFunc(.{
        .name = ALLOC_NAME,
        .type_idx = i64_2i64_type,
        .code = alloc_body,
        .exported = false,
    });

    // dealloc(ptr: i64) -> void (no-op in WasmGC, GC handles it)
    const void_1i64_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const dealloc_body = try generateVoidStubBody(allocator);
    const dealloc_idx = try linker.addFunc(.{
        .name = DEALLOC_NAME,
        .type_idx = void_1i64_type,
        .code = dealloc_body,
        .exported = false,
    });

    // realloc(ptr: i64, new_size: i64) -> i64 (bump allocate new block, copy old)
    const realloc_body = try generateReallocBody(allocator, heap_ptr_global);
    const realloc_idx = try linker.addFunc(.{
        .name = REALLOC_NAME,
        .type_idx = i64_2i64_type,
        .code = realloc_body,
        .exported = false,
    });

    // memset_zero(ptr: i64, size: i64) -> void
    const void_2i64_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const memset_zero_body = try generateMemsetZeroBody(allocator);
    const memset_zero_idx = try linker.addFunc(.{
        .name = MEMSET_ZERO_NAME,
        .type_idx = void_2i64_type,
        .code = memset_zero_body,
        .exported = false,
    });

    // memcpy(dst: i64, src: i64, num_bytes: i64) -> void
    const void_3i64_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{},
    );
    const memcpy_body = try generateMemcpyBody(allocator);
    const memcpy_idx = try linker.addFunc(.{
        .name = MEMCPY_NAME,
        .type_idx = void_3i64_type,
        .code = memcpy_body,
        .exported = false,
    });

    // string_eq(s1_ptr, s1_len, s2_ptr, s2_len) -> i64
    const i64_4i64_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const string_eq_body = try generateStringEqBody(allocator);
    const string_eq_idx = try linker.addFunc(.{
        .name = STRING_EQ_NAME,
        .type_idx = i64_4i64_type,
        .code = string_eq_body,
        .exported = false,
    });

    // string_concat(s1_ptr, s1_len, s2_ptr, s2_len) -> new_ptr (i64)
    // Uses bump allocator (heap_ptr_global) — no ARC header
    const string_concat_body = try generateStringConcatBody(allocator, heap_ptr_global);
    const string_concat_idx = try linker.addFunc(.{
        .name = STRING_CONCAT_NAME,
        .type_idx = i64_4i64_type,
        .code = string_concat_body,
        .exported = false,
    });

    // Raw allocation: for WasmGC these are the same as regular alloc
    // (no ARC headers in the Wasm bump allocator either).
    // alloc_raw(size) -> i64
    const i64_1i64_type = try linker.addType(&[_]ValType{.i64}, &[_]ValType{.i64});
    const alloc_raw_body = try generateAllocRawBody(allocator, heap_ptr_global);
    const alloc_raw_idx = try linker.addFunc(.{
        .name = ALLOC_RAW_NAME,
        .type_idx = i64_1i64_type,
        .code = alloc_raw_body,
        .exported = false,
    });
    // realloc_raw: just return ptr (bump allocator can't move)
    const realloc_raw_body = try generateReturnFirstArgBody(allocator);
    const realloc_raw_idx = try linker.addFunc(.{
        .name = REALLOC_RAW_NAME,
        .type_idx = i64_2i64_type,
        .code = realloc_raw_body,
        .exported = false,
    });
    // dealloc_raw: no-op
    const dealloc_raw_body = try generateVoidStubBody(allocator);
    const dealloc_raw_idx = try linker.addFunc(.{
        .name = DEALLOC_RAW_NAME,
        .type_idx = void_1i64_type,
        .code = dealloc_raw_body,
        .exported = false,
    });

    return MemFunctions{
        .alloc_idx = alloc_idx,
        .dealloc_idx = dealloc_idx,
        .realloc_idx = realloc_idx,
        .alloc_raw_idx = alloc_raw_idx,
        .realloc_raw_idx = realloc_raw_idx,
        .dealloc_raw_idx = dealloc_raw_idx,
        .memcpy_idx = memcpy_idx,
        .memset_zero_idx = memset_zero_idx,
        .string_eq_idx = string_eq_idx,
        .string_concat_idx = string_concat_idx,
    };
}

/// alloc_raw(size: i64) -> i64 — bump allocator, no ARC header
/// Same as alloc but without metadata param. Just bumps heap_ptr by size.
fn generateAllocRawBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: size (i64), local 1: result_ptr (i64, declared)
    _ = try code.declareLocals(&[_]wasm.ValType{.i64});

    // result_ptr = heap_ptr (as i64)
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitI64ExtendI32U();
    try code.emitLocalSet(1);

    // heap_ptr += size
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // return result_ptr
    try code.emitLocalGet(1);

    return code.finish();
}

/// return first arg unchanged: (i64, i64) -> i64
fn generateReturnFirstArgBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitLocalGet(0);
    return code.finish();
}

/// memset_zero(ptr: i64, size: i64) -> void
/// Reference: Go's memclrNoHeapPointers / C's memset(p, 0, n)
fn generateMemsetZeroBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: ptr (i64), local 1: size (i64)
    // local 2: addr_i32, local 3: len_i32, local 4: counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32 });

    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(2);
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(3);

    try code.emitI32Const(0);
    try code.emitLocalSet(4);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    try code.emitLocalGet(4);
    try code.emitLocalGet(3);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    try code.emitLocalGet(2);
    try code.emitLocalGet(4);
    try code.emitI32Add();
    try code.emitI64Const(0);
    try code.emitI64Store8(0);
    try code.emitLocalGet(4);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(4);
    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    return code.finish();
}

/// memcpy(dst: i64, src: i64, num_bytes: i64) -> void
/// Handles overlapping regions (memmove semantics).
/// Reference: Go runtime/memmove_*.s
fn generateMemcpyBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: dst, local 1: src, local 2: num_bytes (all i64)
    // local 3: dst_i32, local 4: src_i32, local 5: len_i32, local 6: counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i32 });

    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(3);
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(4);
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalSet(5);

    // if (len == 0) return
    try code.emitLocalGet(5);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // if dst > src: backward copy
    try code.emitLocalGet(3);
    try code.emitLocalGet(4);
    try code.emitI32GtU();
    try code.emitIf(BLOCK_VOID);

    // Backward: counter = len-1; while(counter >= 0) { dst[c] = src[c]; c-- }
    try code.emitLocalGet(5);
    try code.emitI32Const(1);
    try code.emitI32Sub();
    try code.emitLocalSet(6);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    try code.emitLocalGet(6);
    try code.emitI32Const(0);
    try code.buf.append(allocator, wasm_op.Op.i32_lt_s);
    try code.emitBrIf(1);
    try code.emitLocalGet(3);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitLocalGet(4);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitI64Load8U(0);
    try code.emitI64Store8(0);
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Sub();
    try code.emitLocalSet(6);
    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    try code.emitElse();

    // Forward: counter = 0; while(counter < len) { dst[c] = src[c]; c++ }
    try code.emitI32Const(0);
    try code.emitLocalSet(6);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    try code.emitLocalGet(6);
    try code.emitLocalGet(5);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    try code.emitLocalGet(3);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitLocalGet(4);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitI64Load8U(0);
    try code.emitI64Store8(0);
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(6);
    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    try code.emitEnd(); // end if/else

    return code.finish();
}

/// string_eq(s1_ptr, s1_len, s2_ptr, s2_len) -> i64 (1=equal, 0=not)
/// Reference: Go runtime/string.go stringEqual
fn generateStringEqBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0-3: params (i64)
    // local 4: len_i32, local 5: p1_i32, local 6: p2_i32, local 7: counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i32 });

    // if (s1_len != s2_len) return 0
    try code.emitLocalGet(1);
    try code.emitLocalGet(3);
    try code.emitI64Ne();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // Pointer equality fast path
    try code.emitLocalGet(0);
    try code.emitLocalGet(2);
    try code.emitI64Eq();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(1);
    try code.emitReturn();
    try code.emitEnd();

    // len_i32 = (i32)s1_len
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(4);

    // if (len == 0) return 1
    try code.emitLocalGet(4);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(1);
    try code.emitReturn();
    try code.emitEnd();

    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(5);
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalSet(6);

    // Byte-compare loop
    try code.emitI32Const(0);
    try code.emitLocalSet(7);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    try code.emitLocalGet(7);
    try code.emitLocalGet(4);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    try code.emitLocalGet(5);
    try code.emitLocalGet(7);
    try code.emitI32Add();
    try code.emitI64Load8U(0);
    try code.emitLocalGet(6);
    try code.emitLocalGet(7);
    try code.emitI32Add();
    try code.emitI64Load8U(0);
    try code.emitI64Ne();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();
    try code.emitLocalGet(7);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(7);
    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    try code.emitI64Const(1);

    return code.finish();
}

/// string_concat(s1_ptr, s1_len, s2_ptr, s2_len) -> new_ptr (i64)
/// Uses bump allocator via heap_ptr_global — no ARC header.
/// Returns pointer to concatenated string. Caller gets (new_ptr, new_len=s1_len+s2_len).
fn generateStringConcatBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: s1_ptr, local 1: s1_len, local 2: s2_ptr, local 3: s2_len (all i64)
    // local 4: new_len_i64, local 5: new_ptr_i64
    // local 6: tmp_src (i32), local 7: tmp_len (i32), local 8: tmp_dest (i32), local 9: counter (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64, .i32, .i32, .i32, .i32 });

    // new_len = s1_len + s2_len
    try code.emitLocalGet(1);
    try code.emitLocalGet(3);
    try code.emitI64Add();
    try code.emitLocalSet(4);

    // if (new_len == 0) return 0
    try code.emitLocalGet(4);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // Bump allocate: new_ptr = heap_ptr; heap_ptr += new_len
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitI64ExtendI32U();
    try code.emitLocalSet(5); // new_ptr_i64

    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalGet(4);
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // Copy s1: byte_copy(dest=new_ptr, src=s1_ptr, len=s1_len)
    try code.emitLocalGet(5);
    try code.emitI32WrapI64();
    try code.emitLocalSet(8);
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(6);
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(7);
    try code.emitByteCopyLoop(8, 6, 7, 9);

    // Copy s2: byte_copy(dest=new_ptr+s1_len, src=s2_ptr, len=s2_len)
    try code.emitLocalGet(5);
    try code.emitLocalGet(1);
    try code.emitI64Add();
    try code.emitI32WrapI64();
    try code.emitLocalSet(8);
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalSet(6);
    try code.emitLocalGet(3);
    try code.emitI32WrapI64();
    try code.emitLocalSet(7);
    try code.emitByteCopyLoop(8, 6, 7, 9);

    // Return new_ptr
    try code.emitLocalGet(5);

    return code.finish();
}

/// alloc(metadata: i64, size: i64) -> i64
/// Simple bump allocator. Ignores metadata (no ARC header).
/// Returns raw pointer to allocated region.
fn generateAllocBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: metadata (i64, ignored), local 1: size (i64)
    // local 2: result_ptr (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{.i64});

    // result_ptr = heap_ptr (as i64)
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitI64ExtendI32U();
    try code.emitLocalSet(2);

    // heap_ptr += size
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // return result_ptr
    try code.emitLocalGet(2);

    return code.finish();
}

/// Void stub body (single `end` opcode). Used for dealloc no-op.
fn generateVoidStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    return try code.finish();
}

/// realloc(ptr: i64, new_size: i64) -> i64
/// Bump allocator: always allocates new block, does NOT copy old data.
/// Caller (growslice etc.) is responsible for copying.
fn generateReallocBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: ptr (i64, ignored), local 1: new_size (i64)
    // local 2: result_ptr (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{.i64});

    // result_ptr = heap_ptr (as i64)
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitI64ExtendI32U();
    try code.emitLocalSet(2);

    // heap_ptr += new_size
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // return result_ptr
    try code.emitLocalGet(2);

    return code.finish();
}
