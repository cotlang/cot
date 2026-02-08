//! Print Runtime for Cot (WebAssembly)
//!
//! Provides print/println/eprint/eprintln runtime functions.
//! Reference: Go runtime/print.go (type-specific formatters, stack buffers, direct syscall)
//! Reference: Zig std.io.Writer.printIntAny (divmod loop, stack buffer)
//!
//! Three functions:
//!   cot_write(fd, ptr, len) -> i64    — write syscall (stub in Wasm, ARM64 override in native)
//!   cot_print_int(value) -> void      — format i64 to decimal, write to stdout
//!   cot_eprint_int(value) -> void     — format i64 to decimal, write to stderr

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;

const wasm_op = @import("wasm_opcodes.zig");
const BLOCK_VOID: u8 = wasm_op.BLOCK_VOID;
const BLOCK_I64: u8 = @intFromEnum(wasm_op.ValType.i64);

// =============================================================================
// Function Names
// =============================================================================

pub const WRITE_NAME = "cot_write";
pub const PRINT_INT_NAME = "cot_print_int";
pub const EPRINT_INT_NAME = "cot_eprint_int";
pub const INT_TO_STRING_NAME = "cot_int_to_string";

// =============================================================================
// Return Type
// =============================================================================

pub const PrintFunctions = struct {
    write_idx: u32,
    print_int_idx: u32,
    eprint_int_idx: u32,
    int_to_string_idx: u32,
};

// =============================================================================
// addToLinker — register all print runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker) !PrintFunctions {
    // cot_write: (fd: i64, ptr: i64, len: i64) -> i64
    // Must be added FIRST (print_int calls it by index)
    const write_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const write_body = try generateWriteStubBody(allocator);
    const write_idx = try linker.addFunc(.{
        .name = WRITE_NAME,
        .type_idx = write_type,
        .code = write_body,
        .exported = true, // So generateMachO can find it by name
    });

    // cot_print_int: (value: i64) -> void
    const print_int_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const print_int_body = try generatePrintIntBody(allocator, write_idx, 1); // fd=1 (stdout)
    const print_int_idx = try linker.addFunc(.{
        .name = PRINT_INT_NAME,
        .type_idx = print_int_type,
        .code = print_int_body,
        .exported = false,
    });

    // cot_eprint_int: (value: i64) -> void — same as print_int but fd=2
    const eprint_int_body = try generatePrintIntBody(allocator, write_idx, 2); // fd=2 (stderr)
    const eprint_int_idx = try linker.addFunc(.{
        .name = EPRINT_INT_NAME,
        .type_idx = print_int_type, // Same type: (i64) -> void
        .code = eprint_int_body,
        .exported = false,
    });

    // cot_int_to_string: (value: i64, buf_ptr: i64) -> i64 (returns length)
    // Caller provides a 21-byte buffer. Same divmod as print_int.
    const int_to_string_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const int_to_string_body = try generateIntToStringBody(allocator);
    const int_to_string_idx = try linker.addFunc(.{
        .name = INT_TO_STRING_NAME,
        .type_idx = int_to_string_type,
        .code = int_to_string_body,
        .exported = false,
    });

    return PrintFunctions{
        .write_idx = write_idx,
        .print_int_idx = print_int_idx,
        .eprint_int_idx = eprint_int_idx,
        .int_to_string_idx = int_to_string_idx,
    };
}

// =============================================================================
// cot_write stub — drops args, returns 0
// Pure Wasm can't do I/O; native overrides this with ARM64 syscall
// =============================================================================

// =============================================================================
// cot_int_to_string — same divmod as print_int, writes to caller buffer
//
// Signature: (value: i64, buf_ptr: i64) -> i64 (returns string length)
// Caller provides a 21-byte buffer (max i64 decimal = 20 digits + sign).
// Writes digits right-to-left into buffer, returns number of bytes written.
// The string starts at (buf_ptr + 21 - result_len).
//
// Algorithm: same as Go printint (see above), but writes to caller buffer
// instead of calling cot_write.
//
// Wasm locals layout:
//   param 0: value (i64)
//   param 1: buf_ptr (i64) — pointer to 21-byte buffer
//   local 2: buf_i32 (i32) — buf_ptr as i32
//   local 3: idx (i32) — current write position (starts at 20)
//   local 4: u (i64) — unsigned absolute value
//   local 5: is_neg (i32) — 1 if negative, 0 otherwise
// =============================================================================

fn generateIntToStringBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Declare 4 locals: buf_i32 (i32), idx (i32), u (i64), is_neg (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i64, .i32 });

    // buf_i32 = (i32)buf_ptr
    try code.emitLocalGet(1); // buf_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(2);

    // idx = 20 (last byte of 21-byte buffer)
    try code.emitI32Const(20);
    try code.emitLocalSet(3);

    // is_neg = 0
    try code.emitI32Const(0);
    try code.emitLocalSet(5);

    // u = value
    try code.emitLocalGet(0);
    try code.emitLocalSet(4);

    // --- Handle negative ---
    try code.emitLocalGet(0);
    try code.emitI64Const(0);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitI32Const(1);
        try code.emitLocalSet(5);
        try code.emitI64Const(0);
        try code.emitLocalGet(0);
        try code.emitI64Sub();
        try code.emitLocalSet(4);
    }
    try code.emitEnd();

    // --- Handle zero ---
    try code.emitLocalGet(4);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitLocalGet(2);
        try code.emitLocalGet(3);
        try code.emitI32Add();
        try code.emitI64Const('0');
        try code.emitI64Store8(0);
        try code.emitLocalGet(3);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(3);
    }
    try code.emitElse();
    {
        // --- Digit loop ---
        try code.emitBlock(BLOCK_VOID);
        try code.emitLoop(BLOCK_VOID);
        {
            try code.emitLocalGet(4);
            try code.emitI64Eqz();
            try code.emitBrIf(1);

            try code.emitLocalGet(2);
            try code.emitLocalGet(3);
            try code.emitI32Add();
            try code.emitI64Const('0');
            try code.emitLocalGet(4);
            try code.emitI64Const(10);
            try code.emitI64RemU();
            try code.emitI64Add();
            try code.emitI64Store8(0);

            try code.emitLocalGet(4);
            try code.emitI64Const(10);
            try code.emitI64DivU();
            try code.emitLocalSet(4);

            try code.emitLocalGet(3);
            try code.emitI32Const(1);
            try code.emitI32Sub();
            try code.emitLocalSet(3);

            try code.emitBr(0);
        }
        try code.emitEnd(); // end loop
        try code.emitEnd(); // end block
    }
    try code.emitEnd(); // end if/else

    // --- Prepend '-' if negative ---
    try code.emitLocalGet(5);
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitLocalGet(2);
        try code.emitLocalGet(3);
        try code.emitI32Add();
        try code.emitI64Const('-');
        try code.emitI64Store8(0);
        try code.emitLocalGet(3);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(3);
    }
    try code.emitEnd();

    // Return length: 20 - idx (as i64)
    try code.emitI32Const(20);
    try code.emitLocalGet(3);
    try code.emitI32Sub();
    try code.emitI64ExtendI32U();

    return try code.finish();
}

fn generateWriteStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: fd (local 0), ptr (local 1), len (local 2) — all i64
    // Stub: just return 0 (Wasm can't do I/O)
    try code.emitI64Const(0);
    // Stack has i64 return value; finish() adds end opcode
    return try code.finish();
}

// =============================================================================
// cot_print_int — Go's runtime/print.go printint in Wasm bytecode
//
// Algorithm (Go printint):
//   buf [24]byte on stack
//   idx := len(buf) - 1
//   is_neg := false
//   u := uint64(value)
//   if value < 0 { is_neg = true; u = -value }
//   if u == 0 { buf[idx] = '0'; idx-- }
//   else { for u > 0 { buf[idx] = '0' + byte(u % 10); u /= 10; idx-- } }
//   if is_neg { buf[idx] = '-'; idx-- }
//   cot_write(fd, &buf[idx+1], len(buf)-idx-1)
//
// Wasm locals layout:
//   param 0: value (i64)
//   local 1: buf_ptr (i32) — base of 24-byte stack buffer
//   local 2: idx (i32) — current write position (starts at 23)
//   local 3: u (i64) — unsigned absolute value
//   local 4: is_neg (i32) — 1 if negative, 0 otherwise
// =============================================================================

fn generatePrintIntBody(allocator: std.mem.Allocator, write_func_idx: u32, fd: i64) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Declare 4 locals: buf_ptr (i32), idx (i32), u (i64), is_neg (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i64, .i32 });
    // Now: param 0 = value, local 1 = buf_ptr, local 2 = idx, local 3 = u, local 4 = is_neg

    // --- Allocate 24 bytes on Wasm stack ---
    // buf_ptr = SP - 24; SP = buf_ptr
    // Global 0 = __stack_pointer (i32)
    try code.emitGlobalGet(0); // SP (i32)
    try code.emitI32Const(24);
    try code.emitI32Sub();
    try code.emitLocalTee(1); // buf_ptr = SP - 24
    try code.emitGlobalSet(0); // SP = buf_ptr

    // idx = 23 (last byte of buffer)
    try code.emitI32Const(23);
    try code.emitLocalSet(2);

    // is_neg = 0
    try code.emitI32Const(0);
    try code.emitLocalSet(4);

    // u = value (will be adjusted if negative)
    try code.emitLocalGet(0); // value
    try code.emitLocalSet(3); // u = value

    // --- Handle negative: if value <_s 0 ---
    try code.emitLocalGet(0); // value
    try code.emitI64Const(0);
    try code.emitI64LtS(); // value < 0?
    try code.emitIf(BLOCK_VOID);
    {
        // is_neg = 1
        try code.emitI32Const(1);
        try code.emitLocalSet(4);
        // u = 0 - value
        try code.emitI64Const(0);
        try code.emitLocalGet(0); // value
        try code.emitI64Sub();
        try code.emitLocalSet(3); // u = -value
    }
    try code.emitEnd(); // end if

    // --- Handle zero: if u == 0 ---
    try code.emitLocalGet(3); // u
    try code.emitI64Eqz(); // u == 0?
    try code.emitIf(BLOCK_VOID);
    {
        // buf[idx] = '0'
        try code.emitLocalGet(1); // buf_ptr
        try code.emitLocalGet(2); // idx
        try code.emitI32Add(); // buf_ptr + idx
        try code.emitI64Const('0');
        try code.emitI64Store8(0);
        // idx--
        try code.emitLocalGet(2);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(2);
    }
    try code.emitElse();
    {
        // --- Digit loop: while u > 0 ---
        try code.emitBlock(BLOCK_VOID);
        try code.emitLoop(BLOCK_VOID);
        {
            // if u == 0, break
            try code.emitLocalGet(3); // u
            try code.emitI64Eqz();
            try code.emitBrIf(1); // break to outer block

            // buf[idx] = '0' + (u % 10)
            try code.emitLocalGet(1); // buf_ptr
            try code.emitLocalGet(2); // idx
            try code.emitI32Add(); // addr = buf_ptr + idx

            try code.emitI64Const('0');
            try code.emitLocalGet(3); // u
            try code.emitI64Const(10);
            try code.emitI64RemU(); // u % 10 (unsigned)
            try code.emitI64Add(); // '0' + digit

            try code.emitI64Store8(0);

            // u = u / 10 (unsigned)
            try code.emitLocalGet(3);
            try code.emitI64Const(10);
            try code.emitI64DivU();
            try code.emitLocalSet(3);

            // idx--
            try code.emitLocalGet(2);
            try code.emitI32Const(1);
            try code.emitI32Sub();
            try code.emitLocalSet(2);

            // continue loop
            try code.emitBr(0);
        }
        try code.emitEnd(); // end loop
        try code.emitEnd(); // end block
    }
    try code.emitEnd(); // end if/else

    // --- Prepend '-' if negative ---
    try code.emitLocalGet(4); // is_neg
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitLocalGet(1); // buf_ptr
        try code.emitLocalGet(2); // idx
        try code.emitI32Add();
        try code.emitI64Const('-');
        try code.emitI64Store8(0);
        // idx--
        try code.emitLocalGet(2);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(2);
    }
    try code.emitEnd(); // end if

    // --- Call cot_write(fd, start_ptr, len) ---
    // start_ptr = buf_ptr + idx + 1 (as i64)
    // len = 23 - idx (as i64)
    try code.emitI64Const(fd); // fd (stdout=1, stderr=2)

    // ptr = (i64)(buf_ptr + idx + 1)
    try code.emitLocalGet(1); // buf_ptr
    try code.emitLocalGet(2); // idx
    try code.emitI32Add();
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitI64ExtendI32U(); // convert to i64

    // len = (i64)(23 - idx)
    try code.emitI32Const(23);
    try code.emitLocalGet(2); // idx
    try code.emitI32Sub();
    try code.emitI64ExtendI32U(); // convert to i64

    try code.emitCall(write_func_idx);
    try code.emitDrop(); // cot_write returns i64, but print_int is void

    // --- Restore stack pointer ---
    try code.emitLocalGet(1); // buf_ptr (= original SP - 24)
    try code.emitI32Const(24);
    try code.emitI32Add();
    try code.emitGlobalSet(0); // SP = original

    return try code.finish();
}
