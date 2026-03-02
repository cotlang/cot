//! Print Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates print functions as CLIF IR. These convert integers to decimal
//! strings on the stack and call write(fd, buf, len).
//!
//! Reference: compiler/codegen/print_runtime.zig (Wasm print path)
//! Reference: cg_clif abi/mod.rs (lib_call pattern)

const std = @import("std");
const Allocator = std.mem.Allocator;

const clif = @import("../../ir/clif/mod.zig");
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
const native_compile = @import("compile.zig");
const arc_native = @import("arc_native.zig");
const RuntimeFunc = arc_native.RuntimeFunc;

const debug = @import("../../pipeline_debug.zig");

/// Generate all print runtime functions as compiled native code.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    // print_int(val: i64) → void  (prints to stdout)
    try result.append(allocator, .{
        .name = "print_int",
        .compiled = try generatePrintInt(allocator, isa, ctrl_plane, func_index_map, 1), // fd=1 (stdout)
    });

    // eprint_int(val: i64) → void  (prints to stderr)
    try result.append(allocator, .{
        .name = "eprint_int",
        .compiled = try generatePrintInt(allocator, isa, ctrl_plane, func_index_map, 2), // fd=2 (stderr)
    });

    // int_to_string(val: i64, buf_ptr: i64) → i64  (returns string length)
    try result.append(allocator, .{
        .name = "int_to_string",
        .compiled = try generateIntToString(allocator, isa, ctrl_plane),
    });

    // print_float(val_bits: i64) → void  (prints f64 to stdout)
    try result.append(allocator, .{
        .name = "print_float",
        .compiled = try generatePrintFloat(allocator, isa, ctrl_plane, func_index_map, 1),
    });

    // eprint_float(val_bits: i64) → void  (prints f64 to stderr)
    try result.append(allocator, .{
        .name = "eprint_float",
        .compiled = try generatePrintFloat(allocator, isa, ctrl_plane, func_index_map, 2),
    });

    // float_to_string(val: f64, buf_ptr: i64) → i64  (returns string length)
    try result.append(allocator, .{
        .name = "float_to_string",
        .compiled = try generateFloatToString(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}

// ============================================================================
// print_int(val: i64) → void
//
// Algorithm: Convert i64 to decimal string in a 20-byte stack buffer,
// then call write(fd, buf+offset, len).
//
// For negative numbers: write '-' first, then negate.
// For zero: write '0'.
//
// This is a loop-based approach: divide by 10, store digit, repeat.
// Digits are stored right-to-left in the buffer, then we write from
// the first digit position.
//
// Reference: print_runtime.zig printI64 function
// ============================================================================

fn generatePrintInt(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    fd: i64,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (val: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    // Stack slot: 24-byte buffer for decimal digits + newline
    // Max i64 is 19 digits + sign + newline = 21 bytes, round up to 24
    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3, // 8-byte aligned
    });

    const block_entry = try builder.createBlock();
    const block_negative = try builder.createBlock();
    const block_positive = try builder.createBlock();
    const block_zero = try builder.createBlock();
    const block_nonzero = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_write = try builder.createBlock();

    // --- Entry block ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const val = builder.blockParams(block_entry)[0];

    // Check if negative
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const is_neg = try ins.icmp(.slt, val, v_zero);
    _ = try ins.brif(is_neg, block_negative, &.{}, block_positive, &.{});

    // --- Negative block: write '-' to buffer, negate value ---
    builder.switchToBlock(block_negative);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        // Store '-' at buffer[0]
        const buf_addr = try ins2.stackAddr(clif.Type.I64, ss, 0);
        const minus_char = try ins2.iconst(clif.Type.I8, 0x2D); // '-'
        _ = try ins2.store(clif.MemFlags.DEFAULT, minus_char, buf_addr, 0);

        // Write '-' to fd
        const write_idx = func_index_map.get("write") orelse 0;
        var write_sig = clif.Signature.init(.system_v);
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const wsig_ref = try builder.importSignature(write_sig);
        const wfunc_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
            .signature = wsig_ref,
            .colocated = false,
        });
        const v_fd = try ins2.iconst(clif.Type.I64, fd);
        const v_one = try ins2.iconst(clif.Type.I64, 1);
        _ = try ins2.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_one });

        // Negate: abs_val = 0 - val
        const abs_val = try ins2.isub(v_zero, val);
        _ = try ins2.jump(block_nonzero, &[_]clif.Value{abs_val});
    }

    // --- Positive block: check if zero ---
    builder.switchToBlock(block_positive);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        const is_zero = try ins2.icmp(.eq, val, v_zero);
        _ = try ins2.brif(is_zero, block_zero, &.{}, block_nonzero, &[_]clif.Value{val});
    }

    // --- Zero block: write "0" (no newline — caller adds it via println) ---
    builder.switchToBlock(block_zero);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        const buf_addr = try ins2.stackAddr(clif.Type.I64, ss, 0);
        const zero_char = try ins2.iconst(clif.Type.I8, 0x30); // '0'
        _ = try ins2.store(clif.MemFlags.DEFAULT, zero_char, buf_addr, 0);

        // write(fd, buf, 1)
        const write_idx = func_index_map.get("write") orelse 0;
        var write_sig = clif.Signature.init(.system_v);
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const wsig_ref = try builder.importSignature(write_sig);
        const wfunc_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
            .signature = wsig_ref,
            .colocated = false,
        });
        const v_fd = try ins2.iconst(clif.Type.I64, fd);
        const v_one2 = try ins2.iconst(clif.Type.I64, 1);
        _ = try ins2.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_one2 });
        _ = try ins2.return_(&[_]clif.Value{});
    }

    // --- Nonzero block: entry to digit loop ---
    // Block param: abs_val (positive value to format)
    _ = try builder.appendBlockParam(block_nonzero, clif.Type.I64);
    builder.switchToBlock(block_nonzero);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        const abs_val = builder.blockParams(block_nonzero)[0];
        // Start writing digits from end of buffer (position 22, leaving room for newline at 23)
        const v_pos = try ins2.iconst(clif.Type.I64, 22);
        _ = try ins2.jump(block_loop, &[_]clif.Value{ abs_val, v_pos });
    }

    // --- Digit loop ---
    // Block params: (remaining_val: i64, pos: i64)
    // Extracts one digit per iteration, stores right-to-left
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64);
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64);
    builder.switchToBlock(block_loop);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        const loop_params = builder.blockParams(block_loop);
        const remaining = loop_params[0];
        const pos = loop_params[1];

        // digit = remaining % 10
        const v_ten = try ins2.iconst(clif.Type.I64, 10);
        const digit = try ins2.urem(remaining, v_ten);
        // char = digit + '0'
        const v_0 = try ins2.iconst(clif.Type.I64, 0x30);
        const char_val = try ins2.iadd(digit, v_0);
        const char_i8 = try ins2.ireduce(clif.Type.I8, char_val);

        // Store char at buf[pos]
        const buf_base = try ins2.stackAddr(clif.Type.I64, ss, 0);
        const write_addr = try ins2.iadd(buf_base, pos);
        _ = try ins2.store(clif.MemFlags.DEFAULT, char_i8, write_addr, 0);

        // next_remaining = remaining / 10
        const next_remaining = try ins2.udiv(remaining, v_ten);
        // next_pos = pos - 1
        const v_one = try ins2.iconst(clif.Type.I64, 1);
        const next_pos = try ins2.isub(pos, v_one);

        // if next_remaining == 0 → go to write, else loop
        const v_zero2 = try ins2.iconst(clif.Type.I64, 0);
        const is_done = try ins2.icmp(.eq, next_remaining, v_zero2);
        _ = try ins2.brif(is_done, block_write, &[_]clif.Value{next_pos}, block_loop, &[_]clif.Value{ next_remaining, next_pos });
    }

    // --- Write block ---
    // Block param: last_pos (position of last written digit - 1, so first digit is at last_pos+1)
    _ = try builder.appendBlockParam(block_write, clif.Type.I64);
    builder.switchToBlock(block_write);
    try builder.ensureInsertedBlock();
    {
        const ins2 = builder.ins();
        const last_pos = builder.blockParams(block_write)[0];

        // First digit is at last_pos + 1
        const v_one = try ins2.iconst(clif.Type.I64, 1);
        const start_pos = try ins2.iadd(last_pos, v_one);

        // No trailing newline — caller (println) adds it
        // Digits are at positions [start_pos..22] inclusive
        const buf_base = try ins2.stackAddr(clif.Type.I64, ss, 0);

        // len = 23 - start_pos (from start_pos to position 22 inclusive)
        const v_23 = try ins2.iconst(clif.Type.I64, 23);
        const len = try ins2.isub(v_23, start_pos);

        // write(fd, buf + start_pos, len)
        const write_addr = try ins2.iadd(buf_base, start_pos);
        const write_idx = func_index_map.get("write") orelse 0;
        var write_sig = clif.Signature.init(.system_v);
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try write_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const wsig_ref = try builder.importSignature(write_sig);
        const wfunc_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
            .signature = wsig_ref,
            .colocated = false,
        });
        const v_fd = try ins2.iconst(clif.Type.I64, fd);
        _ = try ins2.call(wfunc_ref, &[_]clif.Value{ v_fd, write_addr, len });
        _ = try ins2.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// int_to_string(val: i64, buf_ptr: i64) → i64
//
// Converts an integer to decimal string in the caller-provided 21-byte buffer.
// Digits are stored right-to-left starting from position 20.
// Returns the number of bytes written (string length).
// The string starts at buf_ptr + 21 - result_len.
//
// Reference: compiler/codegen/print_runtime.zig generateIntToStringBody
// ============================================================================

fn generateIntToString(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (val: i64, buf_ptr: i64) → i64 (length)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_negative = try builder.createBlock();
    const block_positive = try builder.createBlock();
    const block_zero = try builder.createBlock();
    const block_nonzero = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_done = try builder.createBlock();
    const block_add_sign = try builder.createBlock();
    const block_ret = try builder.createBlock();

    // --- Entry block ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const entry_params = builder.blockParams(block_entry);
    const val = entry_params[0];
    const buf_ptr = entry_params[1];

    // Check if negative
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const is_neg = try ins.icmp(.slt, val, v_zero);
    _ = try ins.brif(is_neg, block_negative, &.{}, block_positive, &.{});

    // --- Negative block: negate, remember sign ---
    builder.switchToBlock(block_negative);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const abs_val = try i.isub(v_zero, val);
        const v_one = try i.iconst(clif.Type.I64, 1); // is_negative = 1
        _ = try i.jump(block_nonzero, &[_]clif.Value{ abs_val, v_one });
    }

    // --- Positive block: check if zero ---
    builder.switchToBlock(block_positive);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const is_zero = try i.icmp(.eq, val, v_zero);
        const v_zero2 = try i.iconst(clif.Type.I64, 0); // is_negative = 0
        _ = try i.brif(is_zero, block_zero, &.{}, block_nonzero, &[_]clif.Value{ val, v_zero2 });
    }

    // --- Zero block: store '0', return length 1 ---
    builder.switchToBlock(block_zero);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();

        // Store '0' at buf[20] (last position in 21-byte buffer)
        const v_20 = try i.iconst(clif.Type.I64, 20);
        const write_addr = try i.iadd(buf_ptr, v_20);
        const zero_char = try i.iconst(clif.Type.I8, 0x30); // '0'
        _ = try i.store(clif.MemFlags.DEFAULT, zero_char, write_addr, 0);
        const v_one = try i.iconst(clif.Type.I64, 1);
        _ = try i.return_(&[_]clif.Value{v_one});
    }

    // --- Nonzero block: entry to digit loop ---
    // Block params: (abs_val: i64, is_negative: i64)
    _ = try builder.appendBlockParam(block_nonzero, clif.Type.I64);
    _ = try builder.appendBlockParam(block_nonzero, clif.Type.I64);
    builder.switchToBlock(block_nonzero);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const nonzero_params = builder.blockParams(block_nonzero);
        const abs_val = nonzero_params[0];
        const is_negative = nonzero_params[1];
        // Start at idx = 20 (rightmost position in 21-byte buffer)
        const v_20 = try i.iconst(clif.Type.I64, 20);
        _ = try i.jump(block_loop, &[_]clif.Value{ abs_val, v_20, is_negative });
    }

    // --- Digit loop ---
    // Block params: (remaining: i64, idx: i64, is_negative: i64)
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64);
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64);
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64);
    builder.switchToBlock(block_loop);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const loop_params = builder.blockParams(block_loop);
        const remaining = loop_params[0];
        const idx = loop_params[1];
        const is_negative = loop_params[2];

        // digit = remaining % 10
        const v_ten = try i.iconst(clif.Type.I64, 10);
        const digit = try i.urem(remaining, v_ten);
        // char = digit + '0'
        const v_0 = try i.iconst(clif.Type.I64, 0x30);
        const char_val = try i.iadd(digit, v_0);
        const char_i8 = try i.ireduce(clif.Type.I8, char_val);

        // Store char at buf[idx]

        const write_addr = try i.iadd(buf_ptr, idx);
        _ = try i.store(clif.MemFlags.DEFAULT, char_i8, write_addr, 0);

        // next_remaining = remaining / 10
        const next_remaining = try i.udiv(remaining, v_ten);
        // next_idx = idx - 1
        const v_one = try i.iconst(clif.Type.I64, 1);
        const next_idx = try i.isub(idx, v_one);

        // if next_remaining == 0 → go to done, else loop
        const v_zero2 = try i.iconst(clif.Type.I64, 0);
        const is_done = try i.icmp(.eq, next_remaining, v_zero2);
        _ = try i.brif(is_done, block_done, &[_]clif.Value{ next_idx, is_negative }, block_loop, &[_]clif.Value{ next_remaining, next_idx, is_negative });
    }

    // --- Done block: check if we need to add sign ---
    // Block params: (last_idx: i64, is_negative: i64)
    _ = try builder.appendBlockParam(block_done, clif.Type.I64);
    _ = try builder.appendBlockParam(block_done, clif.Type.I64);
    builder.switchToBlock(block_done);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const done_params = builder.blockParams(block_done);
        const last_idx = done_params[0];
        const is_negative = done_params[1];

        const v_zero2 = try i.iconst(clif.Type.I64, 0);
        const need_sign = try i.icmp(.ne, is_negative, v_zero2);
        _ = try i.brif(need_sign, block_add_sign, &[_]clif.Value{last_idx}, block_ret, &[_]clif.Value{last_idx});
    }

    // --- Add sign block: store '-' at last_idx ---
    // Block param: (last_idx: i64)
    _ = try builder.appendBlockParam(block_add_sign, clif.Type.I64);
    builder.switchToBlock(block_add_sign);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const last_idx = builder.blockParams(block_add_sign)[0];

        const write_addr = try i.iadd(buf_ptr, last_idx);
        const minus_char = try i.iconst(clif.Type.I8, 0x2D); // '-'
        _ = try i.store(clif.MemFlags.DEFAULT, minus_char, write_addr, 0);

        // Adjust idx: idx - 1
        const v_one = try i.iconst(clif.Type.I64, 1);
        const new_idx = try i.isub(last_idx, v_one);
        _ = try i.jump(block_ret, &[_]clif.Value{new_idx});
    }

    // --- Return block: compute length ---
    // Block param: (final_idx: i64) — position before first character
    _ = try builder.appendBlockParam(block_ret, clif.Type.I64);
    builder.switchToBlock(block_ret);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const final_idx = builder.blockParams(block_ret)[0];
        // length = 20 - final_idx  (characters at final_idx+1..20 inclusive)
        const v_20 = try i.iconst(clif.Type.I64, 20);
        const len = try i.isub(v_20, final_idx);
        _ = try i.return_(&[_]clif.Value{len});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// print_float(val: f64) → void
//
// Formats f64 to decimal string via libc snprintf("%g") and writes to fd.
// Port of io_native.zig variadic calling pattern (fcntl/ioctl).
// On Apple ARM64, variadic args are passed on the stack via X3-X7 padding.
// The f64 value is bitcast to i64 for stack passing — snprintf's va_arg(ap, double)
// reads the same raw bits correctly.
// ============================================================================

fn generatePrintFloat(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    fd: i64,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (val: f64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.F64));

    // Stack slots: 32-byte output buffer, 4-byte format string
    const ss_buf = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 32,
        .align_shift = 3,
    });
    const ss_fmt = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 8,
        .align_shift = 3,
    });

    const block_entry = try builder.createBlock();

    // Import snprintf: int snprintf(char *str, size_t size, const char *format, ...)
    const snprintf_idx = func_index_map.get("snprintf") orelse 0;
    var snprintf_sig = clif.Signature.init(.system_v);
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // buf
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // size
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fmt
    const is_aarch64 = isa == .aarch64;
    if (is_aarch64) {
        // Apple ARM64: pad X3-X7 to force variadic args onto stack
        for (0..5) |_| try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // val_bits (f64 bitcast to i64)
    try snprintf_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const snprintf_sig_ref = try builder.importSignature(snprintf_sig);
    const snprintf_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = snprintf_idx } },
        .signature = snprintf_sig_ref,
        .colocated = false,
    });

    // Import write(fd, buf, len)
    const write_idx = func_index_map.get("write") orelse 0;
    var write_sig = clif.Signature.init(.system_v);
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try write_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const wsig_ref = try builder.importSignature(write_sig);
    const wfunc_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
        .signature = wsig_ref,
        .colocated = false,
    });

    // --- Entry block ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const f_val = builder.blockParams(block_entry)[0];

        // Store "%g\0" format string on stack
        const fmt_addr = try ins.stackAddr(clif.Type.I64, ss_fmt, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, '%'), fmt_addr, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 'g'), fmt_addr, 1);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0), fmt_addr, 2);

        // Bitcast f64 → i64 for variadic stack passing (same raw bits)
        const val_bits = try ins.bitcast(clif.Type.I64, f_val);

        // snprintf(buf, 32, "%g", val_bits)
        const buf = try ins.stackAddr(clif.Type.I64, ss_buf, 0);
        const v_32 = try ins.iconst(clif.Type.I64, 32);
        const len_result = if (is_aarch64) blk: {
            const pad = try ins.iconst(clif.Type.I64, 0);
            break :blk try ins.call(snprintf_ref, &[_]clif.Value{ buf, v_32, fmt_addr, pad, pad, pad, pad, pad, val_bits });
        } else try ins.call(snprintf_ref, &[_]clif.Value{ buf, v_32, fmt_addr, val_bits });
        const len = len_result.results[0];

        // write(fd, buf, len)
        const v_fd = try ins.iconst(clif.Type.I64, fd);
        _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf, len });
        _ = try ins.return_(&.{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// float_to_string(val: f64, buf_ptr: i64) → i64
//
// Formats f64 to decimal string via libc snprintf("%g") into caller buffer.
// Returns the number of bytes written.
// Port of io_native.zig variadic calling pattern (fcntl/ioctl).
// ============================================================================

fn generateFloatToString(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.F64)); // val: f64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // buf_ptr: i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    // Stack slot for format string
    const ss_fmt = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 8,
        .align_shift = 3,
    });

    const block_entry = try builder.createBlock();

    // Import snprintf
    const snprintf_idx = func_index_map.get("snprintf") orelse 0;
    var snprintf_sig = clif.Signature.init(.system_v);
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // buf
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // size
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fmt
    const is_aarch64 = isa == .aarch64;
    if (is_aarch64) {
        for (0..5) |_| try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    try snprintf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // val_bits
    try snprintf_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const snprintf_sig_ref = try builder.importSignature(snprintf_sig);
    const snprintf_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = snprintf_idx } },
        .signature = snprintf_sig_ref,
        .colocated = false,
    });

    // --- Entry block ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const f_val = builder.blockParams(block_entry)[0];
        const buf_ptr = builder.blockParams(block_entry)[1];

        // Store "%g\0" format string on stack
        const fmt_addr = try ins.stackAddr(clif.Type.I64, ss_fmt, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, '%'), fmt_addr, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 'g'), fmt_addr, 1);
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0), fmt_addr, 2);

        // Bitcast f64 → i64 for variadic stack passing
        const val_bits = try ins.bitcast(clif.Type.I64, f_val);

        // snprintf(buf_ptr, 32, "%g", val_bits)
        const v_32 = try ins.iconst(clif.Type.I64, 32);
        const len_result = if (is_aarch64) blk: {
            const pad = try ins.iconst(clif.Type.I64, 0);
            break :blk try ins.call(snprintf_ref, &[_]clif.Value{ buf_ptr, v_32, fmt_addr, pad, pad, pad, pad, pad, val_bits });
        } else try ins.call(snprintf_ref, &[_]clif.Value{ buf_ptr, v_32, fmt_addr, val_bits });
        const len = len_result.results[0];

        _ = try ins.return_(&[_]clif.Value{len});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
