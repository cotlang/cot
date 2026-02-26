//! Test Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates test runner functions as CLIF IR. These track pass/fail counts
//! and output test results via write(fd, buf, len).
//!
//! Reference: compiler/codegen/test_runtime.zig (Wasm test path)
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

/// Generate all test runtime functions as compiled native code.
/// Uses two stack-local counters stored as globals (allocated in data section).
/// For simplicity, pass_count and fail_count are managed via dedicated functions.
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

    // __test_begin() → void
    try result.append(allocator, .{
        .name = "__test_begin",
        .compiled = try generateNoop(allocator, isa, ctrl_plane, 0),
    });

    // __test_print_name(ptr: i64, len: i64) → void
    try result.append(allocator, .{
        .name = "__test_print_name",
        .compiled = try generateTestPrintName(allocator, isa, ctrl_plane, func_index_map),
    });

    // __test_pass() → void
    try result.append(allocator, .{
        .name = "__test_pass",
        .compiled = try generateTestWriteStr(allocator, isa, ctrl_plane, func_index_map, "ok\n"),
    });

    // __test_fail() → void
    try result.append(allocator, .{
        .name = "__test_fail",
        .compiled = try generateTestWriteStr(allocator, isa, ctrl_plane, func_index_map, "FAIL\n"),
    });

    // __test_summary(pass_count: i64, fail_count: i64) → void
    // Reference: test_runtime.zig — takes 2 params (pass_count, fail_count)
    try result.append(allocator, .{
        .name = "__test_summary",
        .compiled = try generateNoop(allocator, isa, ctrl_plane, 2),
    });

    // __test_store_fail_values(left, right, is_string, left_len, right_len) → void
    // Stores assertEq failure values for diagnostics. Noop for now.
    try result.append(allocator, .{
        .name = "__test_store_fail_values",
        .compiled = try generateNoop(allocator, isa, ctrl_plane, 5),
    });

    return result;
}

// ============================================================================
// No-op function: (optional params) → void
// Used for __test_begin and __test_summary placeholders
// ============================================================================

fn generateNoop(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    num_params: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    for (0..num_params) |_| {
        try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __test_print_name(ptr: i64, len: i64) → void
// Writes: 'test "' + name + '" ... '
// Reference: test_runtime.zig test_print_name
// ============================================================================

fn generateTestPrintName(
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

    // Signature: (ptr: i64, len: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    // Stack slot for prefix and suffix strings
    // 'test "' = 6 bytes, '" ... ' = 6 bytes → 12 bytes
    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 16,
        .align_shift = 3,
    });

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const name_ptr = params[0];
    const name_len = params[1];

    // Build write function reference
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
    const v_fd = try ins.iconst(clif.Type.I64, 1); // stdout

    // Write prefix: 'test "'
    const buf_addr = try ins.stackAddr(clif.Type.I64, ss, 0);
    // Store "test \"" bytes: 0x74 0x65 0x73 0x74 0x20 0x22
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x74), buf_addr, 0); // t
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x65), buf_addr, 1); // e
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x73), buf_addr, 2); // s
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x74), buf_addr, 3); // t
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x20), buf_addr, 4); // ' '
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x22), buf_addr, 5); // "
    const v_six = try ins.iconst(clif.Type.I64, 6);
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_six });

    // Write name
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, name_ptr, name_len });

    // Write suffix: '" ... '
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x22), buf_addr, 0); // "
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x20), buf_addr, 1); // ' '
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x2E), buf_addr, 2); // .
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x2E), buf_addr, 3); // .
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x2E), buf_addr, 4); // .
    _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, 0x20), buf_addr, 5); // ' '
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_six });

    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Write a fixed string to stdout — used for __test_pass ("ok\n") and __test_fail ("FAIL\n")
// ============================================================================

fn generateTestWriteStr(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    comptime str: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: () → void
    // (no params — pass/fail is implicit from caller)

    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = @intCast(str.len),
        .align_shift = 0,
    });

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Store string bytes on stack
    const buf_addr = try ins.stackAddr(clif.Type.I64, ss, 0);
    for (str, 0..) |byte, i| {
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, @intCast(byte)), buf_addr, @intCast(i));
    }

    // write(1, buf, len)
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
    const v_fd = try ins.iconst(clif.Type.I64, 1); // stdout
    const v_len = try ins.iconst(clif.Type.I64, @intCast(str.len));
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_len });

    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
