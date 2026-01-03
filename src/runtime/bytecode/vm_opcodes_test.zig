//! VM Opcode Tests
//!
//! Comprehensive tests for all VM opcode handlers.
//! Tests are organized by opcode category matching vm_opcodes.zig structure.

const std = @import("std");
const VM = @import("vm.zig").VM;
const VMError = @import("vm_types.zig").VMError;
const DispatchResult = @import("vm_types.zig").DispatchResult;
const Value = @import("value.zig").Value;
const Module = @import("module.zig").Module;
const Opcode = @import("opcodes.zig").Opcode;
const vm_opcodes = @import("vm_opcodes.zig");

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a minimal test module with given bytecode
fn createTestModule(allocator: std.mem.Allocator, code: []const u8) !*Module {
    const module = try allocator.create(Module);
    module.* = Module{
        .allocator = allocator,
        .code = code,
        .constants = &.{},
        .functions = &.{},
        .debug_info = null,
        .source_map = null,
        .name = "test",
        .imports = &.{},
        .exports = &.{},
        .data_section = null,
        .entry_point = 0,
        .global_count = 0,
        .local_count = 0,
    };
    return module;
}

/// Execute a single opcode handler and return the result
fn execOpcode(
    comptime handler: fn (*VM, *const Module) VMError!DispatchResult,
    vm: *VM,
    module: *const Module,
) VMError!DispatchResult {
    return handler(vm, module);
}

// ============================================================================
// Core Operations Tests
// ============================================================================

test "opcode: nop does nothing" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Bytecode: nop
    const code = [_]u8{ @intFromEnum(Opcode.nop), 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1; // Point to operand byte
    const initial_ip = vm.ip;
    const result = try execOpcode(vm_opcodes.op_nop, &vm, module);

    try std.testing.expectEqual(DispatchResult.continue_dispatch, result);
    try std.testing.expectEqual(initial_ip, vm.ip); // IP unchanged by nop
}

test "opcode: halt stops execution" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.halt), 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    const result = try execOpcode(vm_opcodes.op_halt, &vm, module);
    try std.testing.expectEqual(DispatchResult.halt, result);
}

// ============================================================================
// Register Move Tests
// ============================================================================

test "opcode: mov rd, rs" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // mov r0, r1 - operand byte: r0=0, r1=1 -> 0x01
    const code = [_]u8{ @intFromEnum(Opcode.mov), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1; // Point to operand byte
    vm.registers[1] = Value.initInt(42);

    _ = try execOpcode(vm_opcodes.op_mov, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
    try std.testing.expectEqual(@as(usize, 3), vm.ip); // Advanced by 2
}

test "opcode: mov preserves all register values" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // mov r5, r10 -> operand: 0x5A
    const code = [_]u8{ @intFromEnum(Opcode.mov), 0x5A, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[10] = Value.initInt(-999);

    _ = try execOpcode(vm_opcodes.op_mov, &vm, module);

    try std.testing.expectEqual(@as(i64, -999), vm.registers[5].toInt());
    try std.testing.expectEqual(@as(i64, -999), vm.registers[10].toInt()); // Source unchanged
}

test "opcode: movi rd, imm8 positive" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi r0, 127
    const code = [_]u8{ @intFromEnum(Opcode.movi), 0x00, 127 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_movi, &vm, module);

    try std.testing.expectEqual(@as(i64, 127), vm.registers[0].toInt());
}

test "opcode: movi rd, imm8 negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi r3, -1 (0xFF as signed i8)
    const code = [_]u8{ @intFromEnum(Opcode.movi), 0x30, 0xFF };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_movi, &vm, module);

    try std.testing.expectEqual(@as(i64, -1), vm.registers[3].toInt());
}

test "opcode: movi rd, imm8 zero" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi r7, 0
    const code = [_]u8{ @intFromEnum(Opcode.movi), 0x70, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[7] = Value.initInt(999); // Pre-set to verify overwrite
    _ = try execOpcode(vm_opcodes.op_movi, &vm, module);

    try std.testing.expectEqual(@as(i64, 0), vm.registers[7].toInt());
}

test "opcode: movi16 rd, imm16" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi16 r2, 1000 (little endian: 0xE8, 0x03)
    const code = [_]u8{ @intFromEnum(Opcode.movi16), 0x20, 0xE8, 0x03 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_movi16, &vm, module);

    try std.testing.expectEqual(@as(i64, 1000), vm.registers[2].toInt());
    try std.testing.expectEqual(@as(usize, 4), vm.ip);
}

test "opcode: movi16 negative value" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi16 r4, -500 (0xFE0C in little endian)
    const val: i16 = -500;
    const bytes: [2]u8 = @bitCast(val);
    const code = [_]u8{ @intFromEnum(Opcode.movi16), 0x40, bytes[0], bytes[1] };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_movi16, &vm, module);

    try std.testing.expectEqual(@as(i64, -500), vm.registers[4].toInt());
}

test "opcode: movi32 rd, imm32" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // movi32 r1, 100000
    const val: i32 = 100000;
    const bytes: [4]u8 = @bitCast(val);
    const code = [_]u8{ @intFromEnum(Opcode.movi32), 0x10, bytes[0], bytes[1], bytes[2], bytes[3] };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_movi32, &vm, module);

    try std.testing.expectEqual(@as(i64, 100000), vm.registers[1].toInt());
    try std.testing.expectEqual(@as(usize, 6), vm.ip);
}

test "opcode: load_null" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_null r8
    const code = [_]u8{ @intFromEnum(Opcode.load_null), 0x80, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[8] = Value.initInt(123); // Pre-set
    _ = try execOpcode(vm_opcodes.op_load_null, &vm, module);

    try std.testing.expect(vm.registers[8].isNull());
}

test "opcode: load_true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_true r9
    const code = [_]u8{ @intFromEnum(Opcode.load_true), 0x90, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_load_true, &vm, module);

    try std.testing.expect(vm.registers[9].toBool());
}

test "opcode: load_false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_false r10
    const code = [_]u8{ @intFromEnum(Opcode.load_false), 0xA0, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_load_false, &vm, module);

    try std.testing.expect(!vm.registers[10].toBool());
}

// ============================================================================
// Local/Global Variable Tests
// ============================================================================

test "opcode: load_local" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_local r0, slot 2
    const code = [_]u8{ @intFromEnum(Opcode.load_local), 0x00, 0x02 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.fp = 10;
    vm.stack[12] = Value.initInt(42); // fp + slot = 10 + 2

    _ = try execOpcode(vm_opcodes.op_load_local, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: store_local" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // store_local r1, slot 3
    const code = [_]u8{ @intFromEnum(Opcode.store_local), 0x10, 0x03 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.fp = 5;
    vm.registers[1] = Value.initInt(99);

    _ = try execOpcode(vm_opcodes.op_store_local, &vm, module);

    try std.testing.expectEqual(@as(i64, 99), vm.stack[8].toInt()); // fp + slot = 5 + 3
}

test "opcode: load_global" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_global r2, idx 0
    const code = [_]u8{ @intFromEnum(Opcode.load_global), 0x20, 0x00, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    try vm.globals.append(allocator, Value.initInt(777));

    _ = try execOpcode(vm_opcodes.op_load_global, &vm, module);

    try std.testing.expectEqual(@as(i64, 777), vm.registers[2].toInt());
}

test "opcode: load_global out of bounds returns null" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_global r3, idx 100 (out of bounds)
    const code = [_]u8{ @intFromEnum(Opcode.load_global), 0x30, 0x64, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_load_global, &vm, module);

    try std.testing.expect(vm.registers[3].isNull());
}

test "opcode: store_global extends list" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // store_global r4, idx 5
    const code = [_]u8{ @intFromEnum(Opcode.store_global), 0x40, 0x05, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[4] = Value.initInt(555);

    _ = try execOpcode(vm_opcodes.op_store_global, &vm, module);

    try std.testing.expectEqual(@as(usize, 6), vm.globals.items.len);
    try std.testing.expectEqual(@as(i64, 555), vm.globals.items[5].toInt());
}

// ============================================================================
// Arithmetic Operation Tests
// ============================================================================

test "opcode: add positive numbers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // add r0, r1, r2 -> operands: 0x01, 0x20
    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 30), vm.registers[0].toInt());
}

test "opcode: add negative numbers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-10);
    vm.registers[2] = Value.initInt(-20);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, -30), vm.registers[0].toInt());
}

test "opcode: add mixed signs" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);
    vm.registers[2] = Value.initInt(-30);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 70), vm.registers[0].toInt());
}

test "opcode: add zero" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);
    vm.registers[2] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: sub basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.sub), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);
    vm.registers[2] = Value.initInt(30);

    _ = try execOpcode(vm_opcodes.op_sub, &vm, module);

    try std.testing.expectEqual(@as(i64, 70), vm.registers[0].toInt());
}

test "opcode: sub resulting in negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.sub), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(100);

    _ = try execOpcode(vm_opcodes.op_sub, &vm, module);

    try std.testing.expectEqual(@as(i64, -90), vm.registers[0].toInt());
}

test "opcode: mul basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mul), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(7);
    vm.registers[2] = Value.initInt(6);

    _ = try execOpcode(vm_opcodes.op_mul, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: mul by zero" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mul), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(999);
    vm.registers[2] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_mul, &vm, module);

    try std.testing.expectEqual(@as(i64, 0), vm.registers[0].toInt());
}

test "opcode: mul negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mul), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-5);
    vm.registers[2] = Value.initInt(10);

    _ = try execOpcode(vm_opcodes.op_mul, &vm, module);

    try std.testing.expectEqual(@as(i64, -50), vm.registers[0].toInt());
}

test "opcode: div basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.div), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);
    vm.registers[2] = Value.initInt(5);

    _ = try execOpcode(vm_opcodes.op_div, &vm, module);

    try std.testing.expectEqual(@as(i64, 20), vm.registers[0].toInt());
}

test "opcode: div truncates toward zero" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.div), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(7);
    vm.registers[2] = Value.initInt(3);

    _ = try execOpcode(vm_opcodes.op_div, &vm, module);

    try std.testing.expectEqual(@as(i64, 2), vm.registers[0].toInt());
}

test "opcode: div by zero error" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.div), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);
    vm.registers[2] = Value.initInt(0);

    const result = execOpcode(vm_opcodes.op_div, &vm, module);
    try std.testing.expectError(VMError.DivisionByZero, result);
}

test "opcode: mod basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mod), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(17);
    vm.registers[2] = Value.initInt(5);

    _ = try execOpcode(vm_opcodes.op_mod, &vm, module);

    try std.testing.expectEqual(@as(i64, 2), vm.registers[0].toInt());
}

test "opcode: mod exact division" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mod), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(20);
    vm.registers[2] = Value.initInt(5);

    _ = try execOpcode(vm_opcodes.op_mod, &vm, module);

    try std.testing.expectEqual(@as(i64, 0), vm.registers[0].toInt());
}

test "opcode: mod by zero error" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mod), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);
    vm.registers[2] = Value.initInt(0);

    const result = execOpcode(vm_opcodes.op_mod, &vm, module);
    try std.testing.expectError(VMError.DivisionByZero, result);
}

test "opcode: neg positive to negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // neg r0, r1 -> operands: 0x01, 0x00
    const code = [_]u8{ @intFromEnum(Opcode.neg), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);

    _ = try execOpcode(vm_opcodes.op_neg, &vm, module);

    try std.testing.expectEqual(@as(i64, -42), vm.registers[0].toInt());
}

test "opcode: neg negative to positive" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.neg), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-42);

    _ = try execOpcode(vm_opcodes.op_neg, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: neg zero" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.neg), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_neg, &vm, module);

    try std.testing.expectEqual(@as(i64, 0), vm.registers[0].toInt());
}

test "opcode: addi positive immediate" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // addi r0, r1, 10 -> operands: 0x01, 10
    const code = [_]u8{ @intFromEnum(Opcode.addi), 0x01, 10 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);

    _ = try execOpcode(vm_opcodes.op_addi, &vm, module);

    try std.testing.expectEqual(@as(i64, 110), vm.registers[0].toInt());
}

test "opcode: addi negative immediate" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // addi r0, r1, -5 (0xFB as signed)
    const code = [_]u8{ @intFromEnum(Opcode.addi), 0x01, 0xFB };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);

    _ = try execOpcode(vm_opcodes.op_addi, &vm, module);

    try std.testing.expectEqual(@as(i64, 95), vm.registers[0].toInt());
}

test "opcode: subi basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // subi r0, r1, 20
    const code = [_]u8{ @intFromEnum(Opcode.subi), 0x01, 20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100);

    _ = try execOpcode(vm_opcodes.op_subi, &vm, module);

    try std.testing.expectEqual(@as(i64, 80), vm.registers[0].toInt());
}

test "opcode: muli basic" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // muli r0, r1, 5
    const code = [_]u8{ @intFromEnum(Opcode.muli), 0x01, 5 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);

    _ = try execOpcode(vm_opcodes.op_muli, &vm, module);

    try std.testing.expectEqual(@as(i64, 50), vm.registers[0].toInt());
}

test "opcode: incr" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // incr r5
    const code = [_]u8{ @intFromEnum(Opcode.incr), 0x50, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[5] = Value.initInt(41);

    _ = try execOpcode(vm_opcodes.op_incr, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[5].toInt());
}

test "opcode: decr" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // decr r6
    const code = [_]u8{ @intFromEnum(Opcode.decr), 0x60, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[6] = Value.initInt(43);

    _ = try execOpcode(vm_opcodes.op_decr, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[6].toInt());
}

// ============================================================================
// Comparison Operation Tests
// ============================================================================

test "opcode: cmp_eq true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_eq), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);
    vm.registers[2] = Value.initInt(42);

    _ = try execOpcode(vm_opcodes.op_cmp_eq, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_eq false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_eq), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);
    vm.registers[2] = Value.initInt(99);

    _ = try execOpcode(vm_opcodes.op_cmp_eq, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_ne true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_ne), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);
    vm.registers[2] = Value.initInt(99);

    _ = try execOpcode(vm_opcodes.op_cmp_ne, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_ne false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_ne), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42);
    vm.registers[2] = Value.initInt(42);

    _ = try execOpcode(vm_opcodes.op_cmp_ne, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_lt true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_lt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_lt, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_lt false (equal)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_lt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(20);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_lt, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_lt false (greater)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_lt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(30);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_lt, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_le true (less)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_le), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_le, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_le true (equal)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_le), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(20);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_le, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_le false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_le), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(30);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_le, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_gt true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_gt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(30);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_gt, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_gt false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_gt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_gt, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_ge true (greater)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_ge), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(30);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_ge, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_ge true (equal)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_ge), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(20);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_ge, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_ge false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_ge), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(10);
    vm.registers[2] = Value.initInt(20);

    _ = try execOpcode(vm_opcodes.op_cmp_ge, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: cmp_lt with negative numbers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_lt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-10);
    vm.registers[2] = Value.initInt(-5);

    _ = try execOpcode(vm_opcodes.op_cmp_lt, &vm, module);

    try std.testing.expect(vm.registers[0].toBool()); // -10 < -5
}

// ============================================================================
// Logical Operation Tests
// ============================================================================

test "opcode: log_not true to false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // log_not r0, r1 -> operands: 0x01
    const code = [_]u8{ @intFromEnum(Opcode.log_not), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_log_not, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: log_not false to true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_not), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_log_not, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

// ============================================================================
// Control Flow Tests
// ============================================================================

test "opcode: jmp forward" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jmp +10 (little endian offset)
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jmp), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_jmp, &vm, module);

    try std.testing.expectEqual(@as(usize, 11), vm.ip); // 1 + 10
}

test "opcode: jmp backward" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jmp -5
    const offset: i16 = -5;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{0} ** 10 ++ [_]u8{ @intFromEnum(Opcode.jmp), 0x00, bytes[0], bytes[1] };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 11; // At the jmp instruction operands
    _ = try execOpcode(vm_opcodes.op_jmp, &vm, module);

    try std.testing.expectEqual(@as(usize, 6), vm.ip); // 11 - 5
}

test "opcode: jz jumps when false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jz r0, +10
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_jz, &vm, module);

    try std.testing.expectEqual(@as(usize, 11), vm.ip); // Jumped
}

test "opcode: jz falls through when true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jz r0, +10
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_jz, &vm, module);

    try std.testing.expectEqual(@as(usize, 4), vm.ip); // 1 + 3 (instruction size)
}

test "opcode: jnz jumps when true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jnz r0, +10
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jnz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_jnz, &vm, module);

    try std.testing.expectEqual(@as(usize, 11), vm.ip); // Jumped
}

test "opcode: jnz falls through when false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jnz r0, +10
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jnz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_jnz, &vm, module);

    try std.testing.expectEqual(@as(usize, 4), vm.ip); // 1 + 3 (instruction size)
}

test "opcode: jz with zero integer" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jz r0, +10 - zero int should act like false
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_jz, &vm, module);

    try std.testing.expectEqual(@as(usize, 11), vm.ip); // Should jump (0 is falsy)
}

test "opcode: jnz with non-zero integer" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // jnz r0, +10 - non-zero int should act like true
    const offset: i16 = 10;
    const bytes: [2]u8 = @bitCast(offset);
    const code = [_]u8{ @intFromEnum(Opcode.jnz), 0x00, bytes[0], bytes[1] } ++ [_]u8{0} ** 20;
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.initInt(42);

    _ = try execOpcode(vm_opcodes.op_jnz, &vm, module);

    try std.testing.expectEqual(@as(usize, 11), vm.ip); // Should jump (42 is truthy)
}

// ============================================================================
// Edge Cases and Boundary Tests
// ============================================================================

test "opcode: all 16 registers accessible" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Test all registers r0-r15
    for (0..16) |i| {
        vm.registers[i] = Value.initInt(@intCast(i * 100));
    }

    for (0..16) |i| {
        try std.testing.expectEqual(@as(i64, @intCast(i * 100)), vm.registers[i].toInt());
    }
}

test "opcode: add same register" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // add r0, r1, r1 (doubling)
    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x10 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(21);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: add to self" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // add r0, r0, r1 (r0 = r0 + r1)
    const code = [_]u8{ @intFromEnum(Opcode.add), 0x00, 0x10 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.initInt(10);
    vm.registers[1] = Value.initInt(32);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 42), vm.registers[0].toInt());
}

test "opcode: div negative by positive" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.div), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-10);
    vm.registers[2] = Value.initInt(3);

    _ = try execOpcode(vm_opcodes.op_div, &vm, module);

    try std.testing.expectEqual(@as(i64, -3), vm.registers[0].toInt()); // Truncates toward zero
}

test "opcode: mod with negative dividend" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mod), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-17);
    vm.registers[2] = Value.initInt(5);

    _ = try execOpcode(vm_opcodes.op_mod, &vm, module);

    try std.testing.expectEqual(@as(i64, -2), vm.registers[0].toInt());
}

test "opcode: cmp_eq with zeros" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_eq), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(0);
    vm.registers[2] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_cmp_eq, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: cmp_lt with max values" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.cmp_lt), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(std.math.maxInt(i32));
    vm.registers[2] = Value.initInt(std.math.maxInt(i32));

    _ = try execOpcode(vm_opcodes.op_cmp_lt, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool()); // Equal, not less than
}

test "opcode: multiple operations sequence" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Simulate: r0 = 10, r1 = 20, r2 = r0 + r1
    vm.registers[0] = Value.initInt(10);
    vm.registers[1] = Value.initInt(20);

    const code = [_]u8{ @intFromEnum(Opcode.add), 0x20, 0x10 }; // add r2, r0, r1
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 30), vm.registers[2].toInt());
    try std.testing.expectEqual(@as(i64, 10), vm.registers[0].toInt()); // Unchanged
    try std.testing.expectEqual(@as(i64, 20), vm.registers[1].toInt()); // Unchanged
}

// ============================================================================
// Logical Operation Tests (Extended)
// ============================================================================

test "opcode: log_and both true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // log_and r0, r1, r2 -> operands: 0x01, 0x20
    const code = [_]u8{ @intFromEnum(Opcode.log_and), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.true_val;
    vm.registers[2] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_log_and, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: log_and one false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_and), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.true_val;
    vm.registers[2] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_log_and, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: log_and both false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_and), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.false_val;
    vm.registers[2] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_log_and, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

test "opcode: log_or both true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_or), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.true_val;
    vm.registers[2] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_log_or, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: log_or one true" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_or), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.false_val;
    vm.registers[2] = Value.true_val;

    _ = try execOpcode(vm_opcodes.op_log_or, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: log_or both false" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_or), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.false_val;
    vm.registers[2] = Value.false_val;

    _ = try execOpcode(vm_opcodes.op_log_or, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool());
}

// ============================================================================
// Debug Operation Tests
// ============================================================================

test "opcode: debug_break halts" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.debug_break), 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 0;
    const result = try execOpcode(vm_opcodes.op_debug_break, &vm, module);

    try std.testing.expectEqual(DispatchResult.halt, result);
    try std.testing.expectEqual(@as(?@import("vm_types.zig").StopReason, .breakpoint), vm.stop_reason);
}

test "opcode: debug_line sets line number" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // debug_line with line 42 (0x002A little endian)
    const code = [_]u8{ @intFromEnum(Opcode.debug_line), 0x00, 0x2A, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    _ = try execOpcode(vm_opcodes.op_debug_line, &vm, module);

    try std.testing.expectEqual(@as(u32, 42), vm.debug_current_line);
    try std.testing.expectEqual(@as(usize, 4), vm.ip);
}

test "opcode: loop_nop advances ip" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.loop_nop), 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 0;
    const result = try execOpcode(vm_opcodes.op_loop_nop, &vm, module);

    try std.testing.expectEqual(DispatchResult.continue_dispatch, result);
    try std.testing.expectEqual(@as(usize, 1), vm.ip);
}

// ============================================================================
// Additional Arithmetic Edge Cases
// ============================================================================

test "opcode: add large numbers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.add), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(1000000);
    vm.registers[2] = Value.initInt(2000000);

    _ = try execOpcode(vm_opcodes.op_add, &vm, module);

    try std.testing.expectEqual(@as(i64, 3000000), vm.registers[0].toInt());
}

test "opcode: mul large numbers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.mul), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(100000);
    vm.registers[2] = Value.initInt(100000);

    _ = try execOpcode(vm_opcodes.op_mul, &vm, module);

    try std.testing.expectEqual(@as(i64, 10000000000), vm.registers[0].toInt());
}

test "opcode: div negative by negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.div), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(-100);
    vm.registers[2] = Value.initInt(-5);

    _ = try execOpcode(vm_opcodes.op_div, &vm, module);

    try std.testing.expectEqual(@as(i64, 20), vm.registers[0].toInt());
}

test "opcode: incr from negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.incr), 0x00, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.initInt(-1);

    _ = try execOpcode(vm_opcodes.op_incr, &vm, module);

    try std.testing.expectEqual(@as(i64, 0), vm.registers[0].toInt());
}

test "opcode: decr to negative" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.decr), 0x00, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[0] = Value.initInt(0);

    _ = try execOpcode(vm_opcodes.op_decr, &vm, module);

    try std.testing.expectEqual(@as(i64, -1), vm.registers[0].toInt());
}

// ============================================================================
// Wide Operand Tests
// ============================================================================

test "opcode: load_local16 wide slot" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // load_local16 r0, slot 300 (0x012C little endian)
    const code = [_]u8{ @intFromEnum(Opcode.load_local16), 0x00, 0x2C, 0x01 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.fp = 0;
    vm.stack[300] = Value.initInt(12345);

    _ = try execOpcode(vm_opcodes.op_load_local16, &vm, module);

    try std.testing.expectEqual(@as(i64, 12345), vm.registers[0].toInt());
    try std.testing.expectEqual(@as(usize, 4), vm.ip); // Advanced by 3
}

test "opcode: store_local16 wide slot" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // store_local16 r1, slot 500 (0x01F4 little endian)
    const code = [_]u8{ @intFromEnum(Opcode.store_local16), 0x10, 0xF4, 0x01 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.fp = 0;
    vm.registers[1] = Value.initInt(67890);

    _ = try execOpcode(vm_opcodes.op_store_local16, &vm, module);

    try std.testing.expectEqual(@as(i64, 67890), vm.stack[500].toInt());
}

// ============================================================================
// Return Tests
// ============================================================================

test "opcode: ret from main returns halt" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.ret), 0x00, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    // Empty call stack - returning from main
    const result = try execOpcode(vm_opcodes.op_ret, &vm, module);

    try std.testing.expectEqual(DispatchResult.return_from_main, result);
}

test "opcode: ret_val stores return value" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // ret_val r5 -> stores r5 value into r15
    const code = [_]u8{ @intFromEnum(Opcode.ret_val), 0x50, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[5] = Value.initInt(42);

    const result = try execOpcode(vm_opcodes.op_ret_val, &vm, module);

    try std.testing.expectEqual(DispatchResult.return_from_main, result);
    try std.testing.expectEqual(@as(i64, 42), vm.registers[15].toInt());
}

// ============================================================================
// Invalid Opcode Test
// ============================================================================

test "opcode: invalid opcode returns error" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ 0xFF, 0x00 }; // Invalid opcode
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    const result = execOpcode(vm_opcodes.op_invalid, &vm, module);
    try std.testing.expectError(VMError.InvalidOpcode, result);
}

// ============================================================================
// Boolean Coercion Tests
// ============================================================================

test "opcode: log_not with non-zero int (truthy)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_not), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(42); // Non-zero is truthy

    _ = try execOpcode(vm_opcodes.op_log_not, &vm, module);

    try std.testing.expect(!vm.registers[0].toBool()); // NOT truthy = false
}

test "opcode: log_not with zero int (falsy)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_not), 0x01, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(0); // Zero is falsy

    _ = try execOpcode(vm_opcodes.op_log_not, &vm, module);

    try std.testing.expect(vm.registers[0].toBool()); // NOT falsy = true
}

test "opcode: log_and with integer truthiness" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_and), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(1); // Truthy
    vm.registers[2] = Value.initInt(5); // Truthy

    _ = try execOpcode(vm_opcodes.op_log_and, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

test "opcode: log_or with integer truthiness" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    const code = [_]u8{ @intFromEnum(Opcode.log_or), 0x01, 0x20 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[1] = Value.initInt(0); // Falsy
    vm.registers[2] = Value.initInt(42); // Truthy

    _ = try execOpcode(vm_opcodes.op_log_or, &vm, module);

    try std.testing.expect(vm.registers[0].toBool());
}

// ============================================================================
// Register High-Low Tests
// ============================================================================

test "opcode: mov uses high register (r15)" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // mov r15, r14 -> operand: 0xFE
    const code = [_]u8{ @intFromEnum(Opcode.mov), 0xFE, 0x00 };
    const module = try createTestModule(allocator, &code);
    defer allocator.destroy(module);

    vm.ip = 1;
    vm.registers[14] = Value.initInt(999);

    _ = try execOpcode(vm_opcodes.op_mov, &vm, module);

    try std.testing.expectEqual(@as(i64, 999), vm.registers[15].toInt());
}

test "opcode: movi to all registers" {
    const allocator = std.testing.allocator;
    var vm = VM.init(allocator);
    defer vm.deinit();

    // Test movi to each register r0-r15
    for (0..16) |reg| {
        const r: u4 = @intCast(reg);
        const code = [_]u8{ @intFromEnum(Opcode.movi), @as(u8, r) << 4, @intCast(reg + 10) };
        const module = try createTestModule(allocator, &code);

        vm.ip = 1;
        _ = try execOpcode(vm_opcodes.op_movi, &vm, module);

        try std.testing.expectEqual(@as(i64, @intCast(reg + 10)), vm.registers[reg].toInt());
        allocator.destroy(module);
    }
}
