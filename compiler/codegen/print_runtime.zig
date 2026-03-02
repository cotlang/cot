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

pub const WRITE_NAME = "write";
pub const PRINT_INT_NAME = "print_int";
pub const EPRINT_INT_NAME = "eprint_int";
pub const INT_TO_STRING_NAME = "int_to_string";
pub const PRINT_FLOAT_NAME = "print_float";
pub const EPRINT_FLOAT_NAME = "eprint_float";
pub const FLOAT_TO_STRING_NAME = "float_to_string";

// =============================================================================
// Return Type
// =============================================================================

pub const PrintFunctions = struct {
    write_idx: u32,
    print_int_idx: u32,
    eprint_int_idx: u32,
    int_to_string_idx: u32,
    print_float_idx: u32,
    eprint_float_idx: u32,
    float_to_string_idx: u32,
};

// =============================================================================
// addToLinker — register all print runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker) !PrintFunctions {
    // cot_write: (fd: i64, ptr: i64, len: i64) -> i64
    // Must be added FIRST (print_int calls it by index)
    // Stub body: native overrides with ARM64/x64 syscall in driver.zig
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

    // cot_print_float: (value: i64) -> void — format f64 to stdout
    const print_float_body = try generatePrintFloatBody(allocator, write_idx, 1);
    const print_float_idx = try linker.addFunc(.{
        .name = PRINT_FLOAT_NAME,
        .type_idx = print_int_type, // Same type: (i64) -> void
        .code = print_float_body,
        .exported = false,
    });

    // cot_eprint_float: (value: i64) -> void — format f64 to stderr
    const eprint_float_body = try generatePrintFloatBody(allocator, write_idx, 2);
    const eprint_float_idx = try linker.addFunc(.{
        .name = EPRINT_FLOAT_NAME,
        .type_idx = print_int_type, // Same type: (i64) -> void
        .code = eprint_float_body,
        .exported = false,
    });

    // cot_float_to_string: (value: i64, buf_ptr: i64) -> i64 (returns length)
    const float_to_string_body = try generateFloatToStringBody(allocator);
    const float_to_string_idx = try linker.addFunc(.{
        .name = FLOAT_TO_STRING_NAME,
        .type_idx = int_to_string_type, // Same type: (i64, i64) -> i64
        .code = float_to_string_body,
        .exported = false,
    });

    return PrintFunctions{
        .write_idx = write_idx,
        .print_int_idx = print_int_idx,
        .eprint_int_idx = eprint_int_idx,
        .int_to_string_idx = int_to_string_idx,
        .print_float_idx = print_float_idx,
        .eprint_float_idx = eprint_float_idx,
        .float_to_string_idx = float_to_string_idx,
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
    // Stub: just return 0 (native overrides with ARM64/x64 syscall)
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

// =============================================================================
// cot_print_float — format f64 to decimal, write to fd
//
// Signature: (value_bits: i64) -> void
// Algorithm: reinterpret i64 as f64, handle NaN/Inf/negative,
// format integer part + '.' + 6 fractional digits (trimmed).
//
// Wasm locals:
//   param 0: value_bits (i64)
//   local 1: buf_ptr (i32) — stack buffer base
//   local 2: idx (i32) — current write position
//   local 3: f_val (f64) — the float value (abs)
//   local 4: int_part (i64) — integer portion
//   local 5: frac_int (i64) — scaled fractional portion (×10^6)
//   local 6: pos (i32) — position for frac digit writing
// =============================================================================

fn generatePrintFloatBody(allocator: std.mem.Allocator, write_func_idx: u32, fd: i64) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Declare locals: buf_ptr (i32), idx (i32), f_val (f64), int_part (i64), frac_int (i64), pos (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .f64, .i64, .i64, .i32 });
    // param 0 = value_bits, local 1 = buf_ptr, local 2 = idx, local 3 = f_val,
    // local 4 = int_part, local 5 = frac_int, local 6 = pos

    // Allocate 32 bytes on Wasm stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Sub();
    try code.emitLocalTee(1);
    try code.emitGlobalSet(0);

    // f_val = reinterpret(value_bits)
    try code.emitLocalGet(0);
    try code.emitF64ReinterpretI64();
    try code.emitLocalSet(3);

    // --- Check NaN: f_val != f_val ---
    try code.emitLocalGet(3);
    try code.emitLocalGet(3);
    try code.emitF64Ne();
    try code.emitIf(BLOCK_VOID);
    {
        // Write "NaN" to buf, call write
        try code.emitLocalGet(1);
        try code.emitI64Const('N');
        try code.emitI64Store8(0);
        try code.emitLocalGet(1);
        try code.emitI64Const('a');
        try code.emitI64Store8(1);
        try code.emitLocalGet(1);
        try code.emitI64Const('N');
        try code.emitI64Store8(2);
        // write(fd, buf, 3)
        try code.emitI64Const(fd);
        try code.emitLocalGet(1);
        try code.emitI64ExtendI32U();
        try code.emitI64Const(3);
        try code.emitCall(write_func_idx);
        try code.emitDrop();
        // Restore SP and return
        try code.emitLocalGet(1);
        try code.emitI32Const(32);
        try code.emitI32Add();
        try code.emitGlobalSet(0);
        try code.emitReturn();
    }
    try code.emitEnd();

    // --- Check negative ---
    try code.emitLocalGet(3);
    try code.emitF64Const(0.0);
    try code.emitF64Lt();
    try code.emitIf(BLOCK_VOID);
    {
        // Write '-'
        try code.emitLocalGet(1);
        try code.emitI64Const('-');
        try code.emitI64Store8(0);
        try code.emitI64Const(fd);
        try code.emitLocalGet(1);
        try code.emitI64ExtendI32U();
        try code.emitI64Const(1);
        try code.emitCall(write_func_idx);
        try code.emitDrop();
        // f_val = -f_val
        try code.emitLocalGet(3);
        try code.emitF64Neg();
        try code.emitLocalSet(3);
    }
    try code.emitEnd();

    // --- Check infinity ---
    try code.emitLocalGet(3);
    try code.emitF64Const(std.math.inf(f64));
    try code.emitF64Eq();
    try code.emitIf(BLOCK_VOID);
    {
        // Write "Infinity"
        const inf_str = "Infinity";
        for (inf_str, 0..) |ch, i| {
            try code.emitLocalGet(1);
            try code.emitI64Const(@intCast(ch));
            try code.emitI64Store8(@intCast(i));
        }
        try code.emitI64Const(fd);
        try code.emitLocalGet(1);
        try code.emitI64ExtendI32U();
        try code.emitI64Const(8);
        try code.emitCall(write_func_idx);
        try code.emitDrop();
        try code.emitLocalGet(1);
        try code.emitI32Const(32);
        try code.emitI32Add();
        try code.emitGlobalSet(0);
        try code.emitReturn();
    }
    try code.emitEnd();

    // --- Integer part ---
    // int_part = i64.trunc_f64_s(f_val)  [truncate toward zero]
    try code.emitLocalGet(3);
    try code.emitF64Trunc();
    try code.emitI64TruncF64S();
    try code.emitLocalSet(4);

    // idx = 23 (write integer digits right-to-left into buf)
    try code.emitI32Const(23);
    try code.emitLocalSet(2);

    // --- Handle zero integer part ---
    try code.emitLocalGet(4);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitLocalGet(1);
        try code.emitLocalGet(2);
        try code.emitI32Add();
        try code.emitI64Const('0');
        try code.emitI64Store8(0);
        try code.emitLocalGet(2);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(2);
    }
    try code.emitElse();
    {
        // --- Integer digit loop ---
        try code.emitBlock(BLOCK_VOID);
        try code.emitLoop(BLOCK_VOID);
        {
            try code.emitLocalGet(4);
            try code.emitI64Eqz();
            try code.emitBrIf(1);

            try code.emitLocalGet(1);
            try code.emitLocalGet(2);
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

            try code.emitLocalGet(2);
            try code.emitI32Const(1);
            try code.emitI32Sub();
            try code.emitLocalSet(2);

            try code.emitBr(0);
        }
        try code.emitEnd(); // loop
        try code.emitEnd(); // block
    }
    try code.emitEnd(); // if/else

    // Write integer digits: write(fd, buf+idx+1, 23-idx)
    try code.emitI64Const(fd);
    try code.emitLocalGet(1);
    try code.emitLocalGet(2);
    try code.emitI32Add();
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitI64ExtendI32U();
    try code.emitI32Const(23);
    try code.emitLocalGet(2);
    try code.emitI32Sub();
    try code.emitI64ExtendI32U();
    try code.emitCall(write_func_idx);
    try code.emitDrop();

    // --- Fractional part ---
    // Write '.' at buf[0]
    try code.emitLocalGet(1);
    try code.emitI64Const('.');
    try code.emitI64Store8(0);

    // frac = f_val - trunc(f_val)
    // frac_int = i64.trunc_f64_s((frac * 1000000.0) + 0.5)
    try code.emitLocalGet(3);
    try code.emitLocalGet(3);
    try code.emitF64Trunc();
    try code.emitF64Sub();
    try code.emitF64Const(1000000.0);
    try code.emitF64Mul();
    try code.emitF64Const(0.5);
    try code.emitF64Add();
    try code.emitF64Trunc();
    try code.emitI64TruncF64S();
    try code.emitLocalSet(5);

    // Write 6 frac digits right-to-left at buf[1..6]
    // pos = 6
    try code.emitI32Const(6);
    try code.emitLocalSet(6);

    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    {
        // Check pos == 0
        try code.emitLocalGet(6);
        try code.emitI32Eqz();
        try code.emitBrIf(1);

        // buf[pos] = '0' + frac_int % 10
        try code.emitLocalGet(1);
        try code.emitLocalGet(6);
        try code.emitI32Add();
        try code.emitI64Const('0');
        try code.emitLocalGet(5);
        try code.emitI64Const(10);
        try code.emitI64RemU();
        try code.emitI64Add();
        try code.emitI64Store8(0);

        // frac_int /= 10
        try code.emitLocalGet(5);
        try code.emitI64Const(10);
        try code.emitI64DivU();
        try code.emitLocalSet(5);

        // pos--
        try code.emitLocalGet(6);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(6);

        try code.emitBr(0);
    }
    try code.emitEnd(); // loop
    try code.emitEnd(); // block

    // --- Trim trailing zeros from buf[6] back to buf[2] (keep at least 1 digit) ---
    try code.emitI32Const(6);
    try code.emitLocalSet(6); // pos = 6 (start checking from rightmost)

    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    {
        // if pos <= 1, stop trimming (keep at least buf[1])
        try code.emitLocalGet(6);
        try code.emitI32Const(1);
        try code.emitI32LeU();
        try code.emitBrIf(1);

        // Load buf[pos], check if '0'
        try code.emitLocalGet(1);
        try code.emitLocalGet(6);
        try code.emitI32Add();
        // Load as i64 via i64.load8_u (need the raw opcode)
        // Actually, we need to load a byte. Use i64.load8_u at offset 0.
        // The Wasm opcode for i64.load8_u is 0x31 with alignment + offset.
        // Let me use a different approach: store/load via i32.
        // Actually I can compare using the value on the stack from i64.store8...
        // Hmm, I need to load. Let me emit the raw opcode.
        try code.buf.append(code.allocator, 0x31); // i64.load8_u
        try code.buf.append(code.allocator, 0x00); // align=0
        try code.buf.append(code.allocator, 0x00); // offset=0
        try code.emitI64Const('0');
        try code.emitI64Eq();
        try code.emitI32Eqz(); // NOT equal to '0' → stop trimming
        try code.emitBrIf(1);

        // It's '0', decrement pos
        try code.emitLocalGet(6);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(6);

        try code.emitBr(0);
    }
    try code.emitEnd(); // loop
    try code.emitEnd(); // block

    // Write '.' + trimmed digits: write(fd, buf, pos+1)
    try code.emitI64Const(fd);
    try code.emitLocalGet(1);
    try code.emitI64ExtendI32U();
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitI64ExtendI32U();
    try code.emitCall(write_func_idx);
    try code.emitDrop();

    // Restore SP
    try code.emitLocalGet(1);
    try code.emitI32Const(32);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// cot_float_to_string — format f64 to caller buffer, return length
//
// Signature: (value_bits: i64, buf_ptr: i64) -> i64 (length)
// Same algorithm as print_float but writes to caller buffer left-to-right.
//
// Wasm locals:
//   param 0: value_bits (i64)
//   param 1: buf_ptr (i64)
//   local 2: buf_i32 (i32) — buf_ptr as i32
//   local 3: f_val (f64) — the float value (abs)
//   local 4: int_part (i64) — integer portion
//   local 5: frac_int (i64) — scaled fractional portion
//   local 6: pos (i32) — current write position in output buf
//   local 7: digit_buf (i32) — scratch SP allocation for integer digits
//   local 8: digit_idx (i32) — index within digit scratch buffer
//   local 9: src_idx (i32) — copy source index
// =============================================================================

fn generateFloatToStringBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Declare 8 locals
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .f64, .i64, .i64, .i32, .i32, .i32, .i32 });

    // buf_i32 = (i32)buf_ptr
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(2);

    // pos = 0
    try code.emitI32Const(0);
    try code.emitLocalSet(6);

    // Allocate 24-byte scratch buffer on Wasm stack for integer digits
    try code.emitGlobalGet(0);
    try code.emitI32Const(24);
    try code.emitI32Sub();
    try code.emitLocalTee(7);
    try code.emitGlobalSet(0);

    // f_val = reinterpret(value_bits)
    try code.emitLocalGet(0);
    try code.emitF64ReinterpretI64();
    try code.emitLocalSet(3);

    // --- Check NaN ---
    try code.emitLocalGet(3);
    try code.emitLocalGet(3);
    try code.emitF64Ne();
    try code.emitIf(BLOCK_VOID);
    {
        const nan_str = "NaN";
        for (nan_str, 0..) |ch, i| {
            try code.emitLocalGet(2);
            try code.emitI64Const(@intCast(ch));
            try code.emitI64Store8(@intCast(i));
        }
        // Restore scratch SP
        try code.emitLocalGet(7);
        try code.emitI32Const(24);
        try code.emitI32Add();
        try code.emitGlobalSet(0);
        try code.emitI64Const(3);
        try code.emitReturn();
    }
    try code.emitEnd();

    // --- Check negative ---
    try code.emitLocalGet(3);
    try code.emitF64Const(0.0);
    try code.emitF64Lt();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitLocalGet(2);
        try code.emitI64Const('-');
        try code.emitI64Store8(0);
        try code.emitI32Const(1);
        try code.emitLocalSet(6); // pos = 1
        try code.emitLocalGet(3);
        try code.emitF64Neg();
        try code.emitLocalSet(3);
    }
    try code.emitEnd();

    // --- Check infinity ---
    try code.emitLocalGet(3);
    try code.emitF64Const(std.math.inf(f64));
    try code.emitF64Eq();
    try code.emitIf(BLOCK_VOID);
    {
        const inf_str = "Infinity";
        for (inf_str, 0..) |ch, i| {
            try code.emitLocalGet(2);
            try code.emitLocalGet(6);
            try code.emitI32Add();
            try code.emitI64Const(@intCast(ch));
            try code.emitI64Store8(@intCast(i));
        }
        try code.emitLocalGet(7);
        try code.emitI32Const(24);
        try code.emitI32Add();
        try code.emitGlobalSet(0);
        try code.emitLocalGet(6);
        try code.emitI32Const(8);
        try code.emitI32Add();
        try code.emitI64ExtendI32U();
        try code.emitReturn();
    }
    try code.emitEnd();

    // --- Integer part: extract to scratch buffer right-to-left ---
    try code.emitLocalGet(3);
    try code.emitF64Trunc();
    try code.emitI64TruncF64S();
    try code.emitLocalSet(4); // int_part

    // digit_idx = 19 (write from position 19 down to 0)
    try code.emitI32Const(19);
    try code.emitLocalSet(8);

    try code.emitLocalGet(4);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    {
        // Zero: write '0' to scratch
        try code.emitLocalGet(7);
        try code.emitLocalGet(8);
        try code.emitI32Add();
        try code.emitI64Const('0');
        try code.emitI64Store8(0);
        try code.emitLocalGet(8);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(8);
    }
    try code.emitElse();
    {
        // Digit loop
        try code.emitBlock(BLOCK_VOID);
        try code.emitLoop(BLOCK_VOID);
        {
            try code.emitLocalGet(4);
            try code.emitI64Eqz();
            try code.emitBrIf(1);

            try code.emitLocalGet(7);
            try code.emitLocalGet(8);
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

            try code.emitLocalGet(8);
            try code.emitI32Const(1);
            try code.emitI32Sub();
            try code.emitLocalSet(8);

            try code.emitBr(0);
        }
        try code.emitEnd(); // loop
        try code.emitEnd(); // block
    }
    try code.emitEnd(); // if/else

    // --- Copy digits from scratch to output buffer ---
    // src starts at digit_idx+1, count = 19 - digit_idx
    // src_idx = digit_idx + 1
    try code.emitLocalGet(8);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(9); // src_idx

    // Copy loop
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    {
        // if src_idx >= 20, break
        try code.emitLocalGet(9);
        try code.emitI32Const(20);
        try code.emitI32GeU();
        try code.emitBrIf(1);

        // buf[pos] = scratch[src_idx]
        try code.emitLocalGet(2);
        try code.emitLocalGet(6);
        try code.emitI32Add();
        // Load byte from scratch[src_idx]
        try code.emitLocalGet(7);
        try code.emitLocalGet(9);
        try code.emitI32Add();
        try code.buf.append(code.allocator, 0x31); // i64.load8_u
        try code.buf.append(code.allocator, 0x00); // align=0
        try code.buf.append(code.allocator, 0x00); // offset=0
        try code.emitI64Store8(0);

        // pos++, src_idx++
        try code.emitLocalGet(6);
        try code.emitI32Const(1);
        try code.emitI32Add();
        try code.emitLocalSet(6);

        try code.emitLocalGet(9);
        try code.emitI32Const(1);
        try code.emitI32Add();
        try code.emitLocalSet(9);

        try code.emitBr(0);
    }
    try code.emitEnd(); // loop
    try code.emitEnd(); // block

    // --- Write '.' at buf[pos], pos++ ---
    try code.emitLocalGet(2);
    try code.emitLocalGet(6);
    try code.emitI32Add();
    try code.emitI64Const('.');
    try code.emitI64Store8(0);
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(6);

    // --- Fractional digits ---
    try code.emitLocalGet(3);
    try code.emitLocalGet(3);
    try code.emitF64Trunc();
    try code.emitF64Sub();
    try code.emitF64Const(1000000.0);
    try code.emitF64Mul();
    try code.emitF64Const(0.5);
    try code.emitF64Add();
    try code.emitF64Trunc();
    try code.emitI64TruncF64S();
    try code.emitLocalSet(5); // frac_int

    // Write 6 frac digits right-to-left at buf[pos..pos+5]
    // Use digit_idx as offset counter (5 down to 0)
    try code.emitI32Const(5);
    try code.emitLocalSet(8); // digit_idx = 5

    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    {
        // buf[pos + digit_idx] = '0' + frac_int % 10
        try code.emitLocalGet(2);
        try code.emitLocalGet(6);
        try code.emitI32Add();
        try code.emitLocalGet(8);
        try code.emitI32Add();
        try code.emitI64Const('0');
        try code.emitLocalGet(5);
        try code.emitI64Const(10);
        try code.emitI64RemU();
        try code.emitI64Add();
        try code.emitI64Store8(0);

        try code.emitLocalGet(5);
        try code.emitI64Const(10);
        try code.emitI64DivU();
        try code.emitLocalSet(5);

        try code.emitLocalGet(8);
        try code.emitI32Eqz();
        try code.emitBrIf(1);

        try code.emitLocalGet(8);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(8);

        try code.emitBr(0);
    }
    try code.emitEnd(); // loop
    try code.emitEnd(); // block

    // pos += 6 (we wrote 6 digits)
    try code.emitLocalGet(6);
    try code.emitI32Const(6);
    try code.emitI32Add();
    try code.emitLocalSet(6);

    // --- Trim trailing zeros ---
    // Check from pos-1 back, keep at least 1 digit after '.'
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Sub();
    try code.emitLocalSet(8); // digit_idx = pos - 1 (last frac digit)

    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    {
        // Load buf[digit_idx]
        try code.emitLocalGet(2);
        try code.emitLocalGet(8);
        try code.emitI32Add();
        try code.buf.append(code.allocator, 0x31); // i64.load8_u
        try code.buf.append(code.allocator, 0x00);
        try code.buf.append(code.allocator, 0x00);
        try code.emitI64Const('0');
        try code.emitI64Ne();
        try code.emitBrIf(1); // not '0' → stop trimming

        // Check: don't trim below 1 digit after '.' (digit_idx > pos-6+1 = pos-5)
        // More simply: keep at least the digit right after '.', i.e. digit_idx > (pos - 6)
        try code.emitLocalGet(8);
        try code.emitLocalGet(6);
        try code.emitI32Const(5);
        try code.emitI32Sub();
        try code.emitI32LeU();
        try code.emitBrIf(1); // at minimum → stop trimming

        try code.emitLocalGet(8);
        try code.emitI32Const(1);
        try code.emitI32Sub();
        try code.emitLocalSet(8);

        try code.emitBr(0);
    }
    try code.emitEnd(); // loop
    try code.emitEnd(); // block

    // Total length = digit_idx + 1
    // Restore scratch SP
    try code.emitLocalGet(7);
    try code.emitI32Const(24);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    // Return length as i64
    try code.emitLocalGet(8);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitI64ExtendI32U();

    return try code.finish();
}
