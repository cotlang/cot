//! VM Opcode Handlers
//!
//! All bytecode instruction implementations extracted from vm.zig.
//! Each handler follows the signature: fn(*VM, *const Module) VMError!DispatchResult
//!
//! This file reduces vm.zig complexity while keeping handlers testable and organized.

const std = @import("std");
const Module = @import("module.zig").Module;
const vm_types = @import("vm_types.zig");
const VMError = vm_types.VMError;
const DispatchResult = vm_types.DispatchResult;
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const OrderedMap = value_mod.OrderedMap;
const arc = @import("arc.zig");

// Forward declaration - VM is imported at comptime to avoid circular import
const VM = @import("vm.zig").VM;

// ============================================================================
// Invalid/Control Flow
// ============================================================================

/// Handler for invalid/unimplemented opcodes
pub fn op_invalid(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = module;
    return vm.fail(VMError.InvalidOpcode, "Invalid or unimplemented opcode");
}

/// nop - no operation
pub fn op_nop(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = vm;
    _ = module;
    return .continue_dispatch;
}

/// halt - stop execution
pub fn op_halt(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = vm;
    _ = module;
    return .halt;
}

// ============================================================================
// Register Moves
// ============================================================================

/// mov rd, rs - register move
pub fn op_mov(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, vm.registers[rs]);
    return .continue_dispatch;
}

/// movi rd, imm8 - move immediate
pub fn op_movi(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i8 = @bitCast(module.code[vm.ip + 1]);
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(imm));
    return .continue_dispatch;
}

/// movi16 rd, imm16 - move 16-bit immediate
pub fn op_movi16(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little));
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(imm));
    return .continue_dispatch;
}

/// movi32 rd, imm32 - move 32-bit immediate
pub fn op_movi32(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i32 = @bitCast(std.mem.readInt(u32, module.code[vm.ip + 1 ..][0..4], .little));
    vm.ip += 5;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(imm));
    return .continue_dispatch;
}

/// load_const rd, idx - load constant
pub fn op_load_const(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    if (idx >= module.constants.len) {
        return vm.fail(VMError.InvalidConstant, "Constant index out of bounds");
    }
    vm.writeRegister(rd, vm.constantToValue(module.constants[idx]));
    return .continue_dispatch;
}

/// load_null rd - load null
pub fn op_load_null(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.null_val);
    return .continue_dispatch;
}

/// load_true rd - load true
pub fn op_load_true(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.true_val);
    return .continue_dispatch;
}

/// load_false rd - load false
pub fn op_load_false(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.false_val);
    return .continue_dispatch;
}

// ============================================================================
// Local/Global Variables
// ============================================================================

/// load_local rd, slot - load from local variable
pub fn op_load_local(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, vm.stack[vm.fp + slot]);
    return .continue_dispatch;
}

/// store_local rs, slot - store to local variable
pub fn op_store_local(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = module.code[vm.ip + 1];
    vm.ip += 2;
    const rs: u4 = @truncate(ops >> 4);
    vm.writeStack(vm.fp + slot, vm.registers[rs]);
    return .continue_dispatch;
}

/// load_local16 rd, slot16 - load from local (wide)
pub fn op_load_local16(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, vm.stack[vm.fp + slot]);
    return .continue_dispatch;
}

/// store_local16 rs, slot16 - store to local (wide)
pub fn op_store_local16(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const rs: u4 = @truncate(ops >> 4);
    vm.writeStack(vm.fp + slot, vm.registers[rs]);
    return .continue_dispatch;
}

/// load_global rd, idx - load global variable
pub fn op_load_global(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    if (idx >= vm.globals.items.len) {
        vm.writeRegister(rd, Value.null_val);
    } else {
        vm.writeRegister(rd, vm.globals.items[idx]);
    }
    return .continue_dispatch;
}

/// store_global rs, idx - store global variable
pub fn op_store_global(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const rs: u4 = @truncate(ops >> 4);

    // Ensure globals list is large enough
    while (vm.globals.items.len <= idx) {
        vm.globals.append(vm.allocator, Value.null_val) catch {
            return vm.fail(VMError.OutOfMemory, "Failed to extend globals");
        };
    }
    // ARC: retain new value, release old
    const new_val = vm.registers[rs];
    const old_val = vm.globals.items[idx];
    arc.retain(new_val);
    arc.release(old_val, vm.allocator);
    vm.globals.items[idx] = new_val;
    return .continue_dispatch;
}

// ============================================================================
// Arithmetic Operations
// ============================================================================

/// add rd, ra, rb - addition
pub fn op_add(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[ra].toInt() + vm.registers[rb].toInt()));
    return .continue_dispatch;
}

/// sub rd, ra, rb - subtraction
pub fn op_sub(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[ra].toInt() - vm.registers[rb].toInt()));
    return .continue_dispatch;
}

/// mul rd, ra, rb - multiplication
pub fn op_mul(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[ra].toInt() * vm.registers[rb].toInt()));
    return .continue_dispatch;
}

/// div rd, ra, rb - division
pub fn op_div(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const divisor = vm.registers[rb].toInt();
    if (divisor == 0) {
        return vm.fail(VMError.DivisionByZero, "Division by zero");
    }
    vm.writeRegister(rd, Value.initInt(@divTrunc(vm.registers[ra].toInt(), divisor)));
    return .continue_dispatch;
}

/// mod rd, ra, rb - modulo
pub fn op_mod(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const divisor = vm.registers[rb].toInt();
    if (divisor == 0) {
        return vm.fail(VMError.DivisionByZero, "Modulo by zero");
    }
    vm.writeRegister(rd, Value.initInt(@rem(vm.registers[ra].toInt(), divisor)));
    return .continue_dispatch;
}

/// neg rd, rs - negation
pub fn op_neg(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initInt(-vm.registers[rs].toInt()));
    return .continue_dispatch;
}

/// addi rd, rs, imm - add immediate
pub fn op_addi(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i8 = @bitCast(module.code[vm.ip + 1]);
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs].toInt() + imm));
    return .continue_dispatch;
}

/// subi rd, rs, imm - subtract immediate
pub fn op_subi(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i8 = @bitCast(module.code[vm.ip + 1]);
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs].toInt() - imm));
    return .continue_dispatch;
}

/// muli rd, rs, imm - multiply immediate
pub fn op_muli(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const imm: i8 = @bitCast(module.code[vm.ip + 1]);
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs].toInt() * imm));
    return .continue_dispatch;
}

/// incr rd - increment register
pub fn op_incr(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rd].toInt() + 1));
    return .continue_dispatch;
}

/// decr rd - decrement register
pub fn op_decr(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rd].toInt() - 1));
    return .continue_dispatch;
}

// ============================================================================
// Comparison Operations
// ============================================================================

/// Helper to check if value is any string type
fn isAnyString(val: Value) bool {
    return val.isString() or val.isFixedString();
}

/// cmp_eq rd, ra, rb - equality comparison (auto-detects strings)
pub fn op_cmp_eq(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        vm.writeRegister(rd, Value.initBool(std.mem.eql(u8, a.asString(), b.asString())));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() == b.toInt()));
    }
    return .continue_dispatch;
}

/// cmp_ne rd, ra, rb - not equal comparison (auto-detects strings)
pub fn op_cmp_ne(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        vm.writeRegister(rd, Value.initBool(!std.mem.eql(u8, a.asString(), b.asString())));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() != b.toInt()));
    }
    return .continue_dispatch;
}

/// cmp_lt rd, ra, rb - less than comparison (auto-detects strings)
pub fn op_cmp_lt(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        vm.writeRegister(rd, Value.initBool(std.mem.order(u8, a.asString(), b.asString()) == .lt));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() < b.toInt()));
    }
    return .continue_dispatch;
}

/// cmp_le rd, ra, rb - less than or equal comparison (auto-detects strings)
pub fn op_cmp_le(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        const ord = std.mem.order(u8, a.asString(), b.asString());
        vm.writeRegister(rd, Value.initBool(ord == .lt or ord == .eq));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() <= b.toInt()));
    }
    return .continue_dispatch;
}

/// cmp_gt rd, ra, rb - greater than comparison (auto-detects strings)
pub fn op_cmp_gt(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        vm.writeRegister(rd, Value.initBool(std.mem.order(u8, a.asString(), b.asString()) == .gt));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() > b.toInt()));
    }
    return .continue_dispatch;
}

/// cmp_ge rd, ra, rb - greater than or equal comparison (auto-detects strings)
pub fn op_cmp_ge(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra];
    const b = vm.registers[rb];
    if (isAnyString(a) or isAnyString(b)) {
        const ord = std.mem.order(u8, a.asString(), b.asString());
        vm.writeRegister(rd, Value.initBool(ord == .gt or ord == .eq));
    } else {
        vm.writeRegister(rd, Value.initBool(a.toInt() >= b.toInt()));
    }
    return .continue_dispatch;
}

// ============================================================================
// String Comparison Operations
// ============================================================================

/// cmp_str_eq rd, ra, rb - string equal comparison
pub fn op_cmp_str_eq(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    vm.writeRegister(rd, Value.initBool(std.mem.eql(u8, a, b)));
    return .continue_dispatch;
}

/// cmp_str_ne rd, ra, rb - string not equal comparison
pub fn op_cmp_str_ne(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    vm.writeRegister(rd, Value.initBool(!std.mem.eql(u8, a, b)));
    return .continue_dispatch;
}

/// cmp_str_lt rd, ra, rb - string less than comparison
pub fn op_cmp_str_lt(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    vm.writeRegister(rd, Value.initBool(std.mem.order(u8, a, b) == .lt));
    return .continue_dispatch;
}

/// cmp_str_le rd, ra, rb - string less than or equal comparison
pub fn op_cmp_str_le(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    const ord = std.mem.order(u8, a, b);
    vm.writeRegister(rd, Value.initBool(ord == .lt or ord == .eq));
    return .continue_dispatch;
}

/// cmp_str_gt rd, ra, rb - string greater than comparison
pub fn op_cmp_str_gt(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    vm.writeRegister(rd, Value.initBool(std.mem.order(u8, a, b) == .gt));
    return .continue_dispatch;
}

/// cmp_str_ge rd, ra, rb - string greater than or equal comparison
pub fn op_cmp_str_ge(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const a = vm.registers[ra].asString();
    const b = vm.registers[rb].asString();
    const ord = std.mem.order(u8, a, b);
    vm.writeRegister(rd, Value.initBool(ord == .gt or ord == .eq));
    return .continue_dispatch;
}

// ============================================================================
// Logical Operations
// ============================================================================

/// log_not rd, rs - logical NOT
pub fn op_log_not(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initBool(!vm.registers[rs].toBool()));
    return .continue_dispatch;
}

/// is_null rd, rs - check if value is null
pub fn op_is_null(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs].isNull()));
    return .continue_dispatch;
}

/// select rd, cond, rtrue, rfalse - conditional select
pub fn op_select(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops1 >> 4);
    const cond: u4 = @truncate(ops1 & 0xF);
    const rtrue: u4 = @truncate(ops2 >> 4);
    const rfalse: u4 = @truncate(ops2 & 0xF);
    vm.writeRegister(rd, if (vm.registers[cond].toBool()) vm.registers[rtrue] else vm.registers[rfalse]);
    return .continue_dispatch;
}

/// ptr_offset rd, rs, offset - byte-level pointer arithmetic for field views
/// Format: [rd:4|rs:4] [offset_lo] [offset_hi]
/// For string/buffer values, this creates a view at the specified byte offset.
/// The offset is stored in the high bits of the value for later use by load/store.
pub fn op_ptr_offset(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little));
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    _ = offset; // Offset handling deferred until byte-level memory is implemented
    // For now, just copy the value - byte-level addressing will be added later
    vm.writeRegister(rd, vm.registers[rs]);
    return .continue_dispatch;
}

// ============================================================================
// Control Flow
// ============================================================================

/// jmp offset16 - unconditional jump
/// Format: [opcode][unused][offset_lo][offset_hi]
/// Offset is relative to end of instruction (after all 4 bytes)
pub fn op_jmp(vm: *VM, module: *const Module) VMError!DispatchResult {
    // Read signed 16-bit offset from bytes 2-3 (vm.ip points to byte 1 after main loop increment)
    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little));

    // Calculate new IP (offset is relative to end of instruction = vm.ip + 3)
    const end_of_instr: i32 = @intCast(vm.ip + 3);
    const new_ip = end_of_instr + offset;

    if (new_ip < 0 or new_ip >= @as(i32, @intCast(module.code.len))) {
        return vm.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
    }

    vm.ip = @intCast(new_ip);
    return .continue_dispatch;
}

/// jz rs, offset16 - jump if zero/false
/// Format: [opcode][rs<<4][offset_lo][offset_hi]
/// Offset is relative to end of instruction (after all 4 bytes)
pub fn op_jz(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little));
    const rs: u4 = @truncate(ops >> 4);

    // Get condition value
    const cond = vm.registers[rs].toBool();

    if (!cond) {
        // Jump if false/zero (offset relative to end of instruction)
        const end_of_instr: i32 = @intCast(vm.ip + 3);
        const new_ip = end_of_instr + offset;

        if (new_ip < 0 or new_ip >= @as(i32, @intCast(module.code.len))) {
            return vm.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
        }

        vm.ip = @intCast(new_ip);
    } else {
        // Skip over the instruction
        vm.ip += 3;
    }
    return .continue_dispatch;
}

/// jnz rs, offset16 - jump if not zero/true
/// Format: [opcode][rs<<4][offset_lo][offset_hi]
/// Offset is relative to end of instruction (after all 4 bytes)
pub fn op_jnz(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little));
    const rs: u4 = @truncate(ops >> 4);

    // Get condition value
    const cond = vm.registers[rs].toBool();

    if (cond) {
        // Jump if true/non-zero (offset relative to end of instruction)
        const end_of_instr: i32 = @intCast(vm.ip + 3);
        const new_ip = end_of_instr + offset;

        if (new_ip < 0 or new_ip >= @as(i32, @intCast(module.code.len))) {
            return vm.fail(VMError.BytecodeOutOfBounds, "Jump target out of bounds");
        }

        vm.ip = @intCast(new_ip);
    } else {
        // Skip over the instruction
        vm.ip += 3;
    }
    return .continue_dispatch;
}

/// loop_nop - placeholder for loop markers
pub fn op_loop_nop(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = module;
    vm.ip += 1;
    return .continue_dispatch;
}

// ============================================================================
// Debug Operations
// ============================================================================

/// debug_break - breakpoint
pub fn op_debug_break(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = module;
    vm.ip += 1;
    vm.stop_reason = .breakpoint;
    return .halt;
}

/// debug_line - set current line number
/// Format: [opcode:1] [0:1] [line:u16]
/// IP is at byte 1 (after opcode was consumed by dispatch loop)
pub fn op_debug_line(vm: *VM, module: *const Module) VMError!DispatchResult {
    // Skip the 0 byte at vm.ip, read u16 line number at vm.ip+1
    const line = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // Skip: 0 byte (1) + line u16 (2) = 3 bytes
    vm.debug_current_line = line;

    // Check if debugger wants to stop at this line
    if (vm.debugger.shouldStop(line, vm.call_stack.len)) {
        vm.stop_reason = .step;
        return .halt;
    }

    return .continue_dispatch;
}

// ============================================================================
// Logical Operations
// ============================================================================

/// log_and rd, rs1, rs2 - logical AND
pub fn op_log_and(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toBool() and vm.registers[rs2].toBool()));
    return .continue_dispatch;
}

/// log_or rd, rs1, rs2 - logical OR
pub fn op_log_or(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toBool() or vm.registers[rs2].toBool()));
    return .continue_dispatch;
}

// ============================================================================
// Function Calls
// ============================================================================

/// call routine_idx, argc - call subroutine
pub fn op_call(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const routine_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;
    const argc: u4 = @truncate(ops >> 4);

    if (routine_idx >= module.routines.len) {
        return vm.fail(VMError.InvalidRoutine, "Routine index out of bounds");
    }

    // JIT Profiling: record function call
    if (vm.jitProfilingEnabled()) {
        const module_idx = vm.current_module_index orelse 0;
        _ = vm.recordJITCall(module_idx, routine_idx);
    }

    // Push call frame
    vm.call_stack.append(.{
        .module = module,
        .routine_index = routine_idx,
        .return_ip = vm.ip,
        .base_pointer = vm.fp,
        .stack_pointer = vm.sp,
        .caller_module_index = vm.current_module_index,
    }) catch return vm.fail(VMError.StackOverflow, "Call stack overflow");

    // Copy args from r0..r(argc-1) to stack (as locals for callee)
    const routine = module.routines[routine_idx];
    vm.fp = vm.sp;
    for (0..argc) |i| {
        vm.stack[vm.sp] = vm.registers[i];
        vm.sp += 1;
    }
    // Reserve space for remaining locals
    for (argc..routine.local_count) |_| {
        vm.stack[vm.sp] = Value.null_val;
        vm.sp += 1;
    }

    // Jump to routine
    vm.ip = routine.code_offset;
    return .continue_dispatch;
}

/// call_dynamic name_idx, argc - call by name at runtime
pub fn op_call_dynamic(vm: *VM, module: *const Module) VMError!DispatchResult {
    // Format: [argc:4|0] [name_idx:16]
    const argc: u8 = @truncate(module.code[vm.ip] >> 4);
    const name_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;

    try vm.callNative(module, name_idx, argc);
    return .continue_dispatch;
}

/// ret - return from subroutine
pub fn op_ret(vm: *VM, module: *const Module) VMError!DispatchResult {
    vm.ip += 2;
    if (vm.call_stack.pop()) |frame| {
        // Copy back ref parameters to caller's registers
        const routine = module.routines[frame.routine_index];
        for (routine.params, 0..) |param, i| {
            if (param.mode == .ref) {
                // Copy value from callee's stack slot back to caller's register
                vm.writeRegister(@truncate(i), vm.stack[vm.fp + i]);
            }
        }

        vm.sp = frame.stack_pointer;
        vm.fp = frame.base_pointer;
        vm.ip = frame.return_ip;
        vm.current_module = frame.module;
        vm.current_module_index = frame.caller_module_index;
        return .continue_dispatch;
    } else {
        return .return_from_main;
    }
}

/// ret_val rs - return with value
pub fn op_ret_val(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rs: u4 = @truncate(ops >> 4);
    // Return value goes in r15
    vm.writeRegister(15, vm.registers[rs]);

    if (vm.call_stack.pop()) |frame| {
        // Copy back ref parameters to caller's registers
        const routine = module.routines[frame.routine_index];
        for (routine.params, 0..) |param, i| {
            if (param.mode == .ref) {
                // Copy value from callee's stack slot back to caller's register
                vm.writeRegister(@truncate(i), vm.stack[vm.fp + i]);
            }
        }

        vm.sp = frame.stack_pointer;
        vm.fp = frame.base_pointer;
        vm.ip = frame.return_ip;
        vm.current_module = frame.module;
        vm.current_module_index = frame.caller_module_index;
        return .continue_dispatch;
    } else {
        return .return_from_main;
    }
}

// ============================================================================
// Console I/O
// ============================================================================

/// println argc - write all args to stdout with newline
pub fn op_println(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    // argc is in low 4 bits (high 4 bits unused now)
    const argc: u8 = @truncate(ops & 0x0F);

    // Build output string in buffer
    var output_buf: [1024]u8 = undefined;
    var output_pos: usize = 0;

    // Print all arguments from registers r0..r(argc-1)
    var buf: [256]u8 = undefined;
    for (0..argc) |i| {
        const val = vm.registers[i];
        var fbs = std.io.fixedBufferStream(&buf);
        val.format("", .{}, fbs.writer()) catch {};
        const written = fbs.getWritten();
        vm.stdout.writeAll(written) catch {};

        // Accumulate for callback
        if (output_pos + written.len < output_buf.len) {
            @memcpy(output_buf[output_pos..][0..written.len], written);
            output_pos += written.len;
        }
    }
    vm.stdout.writeAll("\n") catch {};

    // Add newline to output buffer
    if (output_pos < output_buf.len) {
        output_buf[output_pos] = '\n';
        output_pos += 1;
    }

    // Call output callback if registered (for DAP Debug Console)
    if (vm.output_callback) |callback| {
        if (vm.output_callback_context) |ctx| {
            callback(output_buf[0..output_pos], ctx);
        }
    }

    return .continue_dispatch;
}

/// print argc - write all args to stdout
pub fn op_print(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    // argc is in low 4 bits
    const argc: u8 = @truncate(ops & 0x0F);

    // Build output string in buffer
    var output_buf: [1024]u8 = undefined;
    var output_pos: usize = 0;

    // Print all arguments from registers r0..r(argc-1)
    var buf: [256]u8 = undefined;
    for (0..argc) |i| {
        const val = vm.registers[i];
        var fbs = std.io.fixedBufferStream(&buf);
        val.format("", .{}, fbs.writer()) catch {};
        const written = fbs.getWritten();
        vm.stdout.writeAll(written) catch {};

        // Accumulate for callback
        if (output_pos + written.len < output_buf.len) {
            @memcpy(output_buf[output_pos..][0..written.len], written);
            output_pos += written.len;
        }
    }

    // Call output callback if registered (for DAP Debug Console)
    if (vm.output_callback) |callback| {
        if (vm.output_callback_context) |ctx| {
            callback(output_buf[0..output_pos], ctx);
        }
    }

    return .continue_dispatch;
}

/// log argc - write all args to stderr
pub fn op_log(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const argc: u4 = @truncate(ops & 0xF);

    // Pop arguments from stack (console_log uses stack-based args)
    var i: usize = 0;
    while (i < argc) : (i += 1) {
        const val = vm.pop() catch continue;
        if (val.tag() == .string) {
            // Log to stderr with [DEV] prefix for dev pane simulation
            var stderr = std.fs.File.stderr();
            var buf: [4096]u8 = undefined;
            var writer = stderr.writer(&buf);
            writer.interface.print("[DEV] {s}\n", .{val.asString()}) catch {};
        } else {
            var buf: [256]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{any}", .{val}) catch "";
            var stderr = std.fs.File.stderr();
            var buf2: [4096]u8 = undefined;
            var writer = stderr.writer(&buf2);
            writer.interface.print("[DEV] {s}\n", .{str}) catch {};
        }
    }
    return .continue_dispatch;
}

// ============================================================================
// String Operations
// ============================================================================

/// str_concat rd, rs1, rs2 - rd = rs1 + rs2 (string concatenation)
pub fn op_str_concat(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);

    // Convert values to strings with auto-coercion
    var buf1: [32]u8 = undefined;
    var buf2: [32]u8 = undefined;
    const s1 = vm.valueToStringSlice(vm.registers[rs1], &buf1);
    const s2 = vm.valueToStringSlice(vm.registers[rs2], &buf2);

    // Concatenate strings
    const result = vm.allocator.alloc(u8, s1.len + s2.len) catch return VMError.OutOfMemory;
    @memcpy(result[0..s1.len], s1);
    @memcpy(result[s1.len..], s2);

    // Store result as string value
    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

/// str_slice_store rd, start_reg, len_reg, val_reg - rd[start:start+len] = val
/// Writes val into the string buffer at position start for len bytes.
/// This implements DBL field overlay write-through semantics for records.
/// Format: [rd:4|start_reg:4] [len_reg:4|val_reg:4] [is_length:8]
pub fn op_str_slice_store(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    const is_length = module.code[vm.ip + 2];
    vm.ip += 3;

    const rd: u4 = @truncate(ops1 >> 4);
    const start_reg: u4 = @truncate(ops1 & 0xF);
    const len_reg: u4 = @truncate(ops2 >> 4);
    const val_reg: u4 = @truncate(ops2 & 0xF);
    _ = is_length; // Currently always treated as length

    // Get parameters
    const start: usize = @intCast(vm.registers[start_reg].asInt());
    const length: usize = @intCast(vm.registers[len_reg].asInt());

    // Get the target buffer
    const target = vm.registers[rd];
    const target_str = target.asString();

    // Get the value to write (coerce to string if needed)
    var val_buf: [32]u8 = undefined;
    const val_str = vm.valueToStringSlice(vm.registers[val_reg], &val_buf);

    // Ensure we have a mutable buffer of sufficient size
    const total_len = @max(target_str.len, start + length);
    const result = vm.allocator.alloc(u8, total_len) catch return VMError.OutOfMemory;

    // Copy existing content
    if (target_str.len > 0) {
        const copy_len = @min(target_str.len, result.len);
        @memcpy(result[0..copy_len], target_str[0..copy_len]);
    }

    // Fill with spaces if buffer is being extended
    if (total_len > target_str.len) {
        @memset(result[target_str.len..], ' ');
    }

    // Write the value at the specified position, padding or truncating as needed
    const write_len = @min(length, val_str.len);
    @memcpy(result[start .. start + write_len], val_str[0..write_len]);

    // Pad remaining field space with spaces if value is shorter than field
    if (write_len < length) {
        @memset(result[start + write_len .. start + length], ' ');
    }

    // Store result back to rd
    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

// ============================================================================
// Type Conversion
// ============================================================================

/// format_decimal rd, rs, width - rd = zero-padded decimal string of rs
pub fn op_format_decimal(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const width_byte = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    const width: usize = width_byte;

    // Get the integer value from source register
    const val = vm.registers[rs];
    const int_val: i64 = switch (val.tag()) {
        .integer => val.asInt(),
        .decimal => blk: {
            if (val.asDecimal()) |dval| {
                const divisor = std.math.pow(i64, 10, dval.precision);
                break :blk @divTrunc(dval.value, divisor);
            }
            break :blk 0;
        },
        else => 0,
    };

    // Format as zero-padded string
    const abs_val: u64 = if (int_val < 0) @intCast(-int_val) else @intCast(int_val);
    var buf: [32]u8 = undefined;
    const digits_needed = if (width > 32) 32 else width;

    // Format with leading zeros
    const formatted = std.fmt.bufPrint(&buf, "{d:0>[1]}", .{ abs_val, digits_needed }) catch "";

    // Allocate result string
    const result_len = if (int_val < 0) formatted.len + 1 else formatted.len;
    const result = vm.allocator.alloc(u8, result_len) catch return VMError.OutOfMemory;

    if (int_val < 0) {
        result[0] = '-';
        @memcpy(result[1..], formatted);
    } else {
        @memcpy(result, formatted);
    }

    // Store result as string value
    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

/// parse_decimal rd, rs - rd = integer parsed from string rs
pub fn op_parse_decimal(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    // Get the string value from source register
    const val = vm.registers[rs];
    const str = switch (val.tag()) {
        .string, .fixed_string => val.asString(),
        .integer => {
            // Already an integer - just copy it
            vm.writeRegister(rd, val);
            return .continue_dispatch;
        },
        else => "",
    };

    // Trim leading/trailing whitespace
    const trimmed = std.mem.trim(u8, str, " \t\n\r");

    // Validate and parse the string
    var is_negative = false;
    var start: usize = 0;

    // Check for leading minus sign
    if (trimmed.len > 0 and trimmed[0] == '-') {
        is_negative = true;
        start = 1;
    }

    // Empty string after trimming (or just "-") -> 0
    if (start >= trimmed.len) {
        vm.writeRegister(rd, Value.initInt(0));
        return .continue_dispatch;
    }

    // Validate all remaining characters are digits
    for (trimmed[start..]) |c| {
        if (!std.ascii.isDigit(c)) {
            // Bad digit error - in DBL this is a runtime error
            return VMError.BadDigit;
        }
    }

    // Parse the integer value
    const abs_val = std.fmt.parseInt(i64, trimmed[start..], 10) catch {
        return VMError.BadDigit;
    };

    const result: i64 = if (is_negative) -abs_val else abs_val;
    vm.writeRegister(rd, Value.initInt(result));
    return .continue_dispatch;
}

// ============================================================================
// Debug Operations
// ============================================================================

/// assert rs - assert that rs is true
pub fn op_assert(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rs: u4 = @truncate(ops >> 4);
    if (!vm.registers[rs].toBool()) {
        return vm.fail(VMError.InvalidType, "Assertion failed");
    }
    return .continue_dispatch;
}

// ============================================================================
// Weak Reference Operations (0xF3-0xF4)
// ============================================================================

/// weak_ref rd, rs - create weak reference: rd = weak(rs)
/// Creates a weak reference to rs without incrementing refcount.
/// When rs is freed, rd becomes null automatically.
pub fn op_weak_ref(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;

    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops);

    const source_value = vm.registers[rs];

    // Store the value without retain (weak reference doesn't own the value)
    // We need to clear any previous value in rd first (with release)
    const old_value = vm.registers[rd];
    if (arc.needsArc(old_value)) {
        arc.release(old_value, vm.allocator);
    }
    vm.registers[rd] = source_value;

    // Register this location in the weak registry
    // When source_value is freed, rd will be set to null
    vm.weak_registry.registerWeakRef(source_value, &vm.registers[rd]) catch {
        return VMError.OutOfMemory;
    };

    return .continue_dispatch;
}

/// weak_load rd, rs - load from weak reference: rd = *rs (or null if freed)
/// Returns null if the weak reference target has been freed.
/// Returns the value (with retain) if still alive.
pub fn op_weak_load(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;

    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops);

    const weak_value = vm.registers[rs];

    // Check if the target is still alive
    if (weak_value.isNull()) {
        // Weak ref already null (target was freed)
        vm.writeRegister(rd, Value.null_val);
    } else if (arc.getHeapPtr(weak_value)) |ptr| {
        const header = arc.getHeader(ptr);
        const flags = header.getFlags();

        if (flags.poisoned or header.refcount == 0) {
            // Target was freed - return null
            vm.writeRegister(rd, Value.null_val);
        } else {
            // Target still alive - return it (writeRegister handles retain)
            vm.writeRegister(rd, weak_value);
        }
    } else {
        // Non-heap value (shouldn't have weak ref, but return it anyway)
        vm.writeRegister(rd, weak_value);
    }

    return .continue_dispatch;
}

// ============================================================================
// ARC Operations (0xF5-0xF7)
// Explicit reference counting for compiler integration
// ============================================================================

/// arc_retain rs - increment reference count of heap value
/// No-op for inline values (ints, bools, null).
pub fn op_arc_retain(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;

    const rs: u4 = @truncate(ops >> 4);
    const value = vm.registers[rs];

    // Retain the value (no-op for inline values)
    arc.retain(value);

    return .continue_dispatch;
}

/// arc_release rs - decrement reference count (may free)
/// No-op for inline values. Frees object when refcount reaches 0.
pub fn op_arc_release(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;

    const rs: u4 = @truncate(ops >> 4);
    const value = vm.registers[rs];

    // Release the value using weak-aware release
    arc.releaseWithWeakSupport(value, vm.allocator, &vm.weak_registry);

    return .continue_dispatch;
}

/// arc_move rd, rs - move value without ARC (transfer ownership)
/// Copies rs to rd without retain, sets rs to null.
/// Used for last-use optimization to avoid retain/release pair.
pub fn op_arc_move(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;

    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops);

    // Release old value in destination (if any)
    const old_value = vm.registers[rd];
    if (arc.needsArc(old_value)) {
        arc.releaseWithWeakSupport(old_value, vm.allocator, &vm.weak_registry);
    }

    // Move value: copy to dest, null out source
    vm.registers[rd] = vm.registers[rs];
    vm.registers[rs] = Value.null_val;

    return .continue_dispatch;
}

// ============================================================================
// Quickened/Specialized Integer Handlers (0xE0-0xEB)
// These skip type checking for performance when types are known
// ============================================================================

/// add_int rd, rs1, rs2 - integer-specialized add (no type check)
pub fn op_add_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() + vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// sub_int rd, rs1, rs2 - integer-specialized subtract
pub fn op_sub_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() - vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// mul_int rd, rs1, rs2 - integer-specialized multiply
pub fn op_mul_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() * vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// div_int rd, rs1, rs2 - integer-specialized divide
pub fn op_div_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    const divisor = vm.registers[rs2].toInt();
    if (divisor == 0) {
        return vm.fail(VMError.DivisionByZero, "Division by zero");
    }
    vm.writeRegister(rd, Value.initInt(@divTrunc(vm.registers[rs1].toInt(), divisor)));
    return .continue_dispatch;
}

/// cmp_lt_int rd, rs1, rs2 - integer less-than
pub fn op_cmp_lt_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() < vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// cmp_le_int rd, rs1, rs2 - integer less-equal
pub fn op_cmp_le_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() <= vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// cmp_gt_int rd, rs1, rs2 - integer greater-than
pub fn op_cmp_gt_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() > vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// cmp_ge_int rd, rs1, rs2 - integer greater-equal
pub fn op_cmp_ge_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() >= vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// cmp_eq_int rd, rs1, rs2 - integer equality
pub fn op_cmp_eq_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() == vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// cmp_ne_int rd, rs1, rs2 - integer not-equal
pub fn op_cmp_ne_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const byte2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(byte2 >> 4);
    vm.writeRegister(rd, Value.initBool(vm.registers[rs1].toInt() != vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// incr_int rd - integer increment (no type check)
pub fn op_incr_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rd].toInt() + 1));
    return .continue_dispatch;
}

/// decr_int rd - integer decrement (no type check)
pub fn op_decr_int(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rd].toInt() - 1));
    return .continue_dispatch;
}

// ============================================================================
// Record Buffer Operations
// ============================================================================

/// load_record_buf rd, type_idx, base, flags - serialize record fields to buffer
/// Format: [rd:4|flags:4] [type_idx:16] [base:16]
/// flags: 0=locals, 1=globals
pub fn op_load_record_buf(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const rd: u4 = @truncate(ops >> 4);
    const flags: u4 = @truncate(ops & 0xF);
    const is_global = (flags & 1) != 0;
    const type_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    const base = std.mem.readInt(u16, module.code[vm.ip + 3 ..][0..2], .little);
    vm.ip += 5;

    // Get type definition
    const type_def = module.getType(type_idx) orelse {
        return vm.fail(VMError.InvalidType, "Invalid type index in load_record_buf");
    };

    // DBL RECORD optimization: If the base slot already contains a properly-sized buffer
    // (field overlay model), just use that directly instead of serializing from separate slots.
    // This handles DBL records where all fields overlay a single buffer.
    const base_val = if (is_global) blk: {
        if (base >= vm.globals.items.len) break :blk Value.initNull();
        break :blk vm.globals.items[base];
    } else blk: {
        const slot = vm.fp + base;
        if (slot >= vm.stack.len) break :blk Value.initNull();
        break :blk vm.stack[slot];
    };

    // Check if base slot is already a buffer of the right size
    if (base_val.isString() or base_val.isFixedString()) {
        const existing_buf = base_val.toString();
        if (existing_buf.len == type_def.total_size) {
            // DBL record case: buffer already contains serialized data
            // Just copy it as a fixed string and return
            const result_copy = vm.allocator.alloc(u8, existing_buf.len) catch {
                return vm.fail(VMError.OutOfMemory, "Failed to copy record buffer");
            };
            @memcpy(result_copy, existing_buf);
            const result = Value.initFixedString(vm.valueAllocator(), result_copy) catch {
                vm.allocator.free(result_copy);
                return vm.fail(VMError.OutOfMemory, "Failed to create fixed string");
            };
            vm.writeRegister(rd, result);
            return .continue_dispatch;
        }
    }

    // Cot struct case: Allocate buffer and serialize from separate field slots
    const buffer = vm.allocator.alloc(u8, type_def.total_size) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate record buffer");
    };
    // Initialize to spaces (standard for ISAM records)
    @memset(buffer, ' ');

    // Serialize each field from the appropriate storage
    for (type_def.fields, 0..) |field, i| {
        const val = if (is_global) blk: {
            const slot = base + i;
            if (slot >= vm.globals.items.len) break :blk Value.initNull();
            break :blk vm.globals.items[slot];
        } else blk: {
            const slot = vm.fp + base + i;
            if (slot >= vm.stack.len) break :blk Value.initNull();
            break :blk vm.stack[slot];
        };

        // Get the destination slice in the buffer
        const offset = field.offset;
        const size = field.size;
        if (offset + size > buffer.len) continue;
        const dest = buffer[offset .. offset + size];

        // Serialize based on field type
        switch (field.data_type) {
            .decimal => {
                // For implied decimal semantics, get the raw value (not divided by precision)
                // e.g., d10.2 value 123.45 is stored as 12345, should write "0000012345"
                const raw_val: i64 = if (val.asDecimal()) |d| d.value else val.toInt();
                var num_buf: [32]u8 = undefined;
                const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@abs(raw_val)}) catch "0";

                if (num_str.len >= size) {
                    @memcpy(dest, num_str[num_str.len - size ..]);
                } else {
                    const pad = size - num_str.len;
                    @memset(dest[0..pad], '0');
                    @memcpy(dest[pad..], num_str);
                }
            },
            .string => {
                if (val.isInt()) {
                    const int_val = val.toInt();
                    var num_buf: [32]u8 = undefined;
                    const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{@abs(int_val)}) catch "0";

                    if (num_str.len >= size) {
                        @memcpy(dest, num_str[num_str.len - size ..]);
                    } else {
                        const pad = size - num_str.len;
                        @memset(dest[0..pad], '0');
                        @memcpy(dest[pad..], num_str);
                    }
                } else {
                    const str = val.asString();
                    const copy_len = @min(str.len, size);
                    @memcpy(dest[0..copy_len], str[0..copy_len]);
                }
            },
            else => {
                const str = val.asString();
                const copy_len = @min(str.len, size);
                @memcpy(dest[0..copy_len], str[0..copy_len]);
            },
        }
    }

    // Create a fixed string value from the buffer and store in destination register
    const result = Value.initFixedString(vm.valueAllocator(), buffer) catch {
        vm.allocator.free(buffer);
        return vm.fail(VMError.OutOfMemory, "Failed to create record buffer value");
    };
    vm.writeRegister(rd, result);

    return .continue_dispatch;
}

/// store_record_buf rs, type_idx, base, flags - deserialize buffer to record fields
/// Format: [rs:4|flags:4] [type_idx:16] [base:16]
/// flags: 0=locals, 1=globals
pub fn op_store_record_buf(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const rs: u4 = @truncate(ops >> 4);
    const flags: u4 = @truncate(ops & 0xF);
    const is_global = (flags & 1) != 0;
    const type_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    const base = std.mem.readInt(u16, module.code[vm.ip + 3 ..][0..2], .little);
    vm.ip += 5;

    // Get the buffer from the source register
    const buffer_val = vm.registers[rs];
    const buffer = buffer_val.toString();
    if (buffer.len == 0) {
        return .continue_dispatch;
    }

    // Get type definition
    const type_def = module.getType(type_idx) orelse {
        return vm.fail(VMError.InvalidType, "Invalid type index in store_record_buf");
    };

    // DBL RECORD optimization: If the base slot contains a buffer of the right size
    // (field overlay model), copy the incoming data directly to that buffer.
    // This handles DBL records where all fields overlay a single buffer.
    const base_slot_ptr: ?*Value = if (is_global) blk: {
        if (base >= vm.globals.items.len) break :blk null;
        break :blk &vm.globals.items[base];
    } else blk: {
        const slot = vm.fp + base;
        if (slot >= vm.stack.len) break :blk null;
        break :blk &vm.stack[slot];
    };

    if (base_slot_ptr) |slot_ptr| {
        const base_val = slot_ptr.*;
        if (base_val.isString() or base_val.isFixedString()) {
            const existing_buf = base_val.toString();
            if (existing_buf.len == type_def.total_size and buffer.len == type_def.total_size) {
                // DBL record case: directly copy incoming buffer to the base slot
                const new_buf = vm.allocator.alloc(u8, buffer.len) catch {
                    return vm.fail(VMError.OutOfMemory, "Failed to allocate record buffer");
                };
                @memcpy(new_buf, buffer);
                const new_val = Value.initFixedString(vm.valueAllocator(), new_buf) catch {
                    vm.allocator.free(new_buf);
                    return vm.fail(VMError.OutOfMemory, "Failed to create fixed string");
                };
                // Inline ARC for slot write
                arc.retain(new_val);
                arc.release(slot_ptr.*, vm.allocator);
                slot_ptr.* = new_val;
                return .continue_dispatch;
            }
        }
    }

    // Cot struct case: Deserialize each field from the buffer into separate slots
    for (type_def.fields, 0..) |field, i| {
        // Determine target slot based on storage type
        const slot_ptr: ?*Value = if (is_global) blk: {
            const slot = base + i;
            if (slot >= vm.globals.items.len) {
                // Extend globals if needed
                vm.globals.ensureTotalCapacity(vm.allocator, slot + 1) catch break :blk null;
                vm.globals.items.len = slot + 1;
            }
            break :blk &vm.globals.items[slot];
        } else blk: {
            const slot = vm.fp + base + i;
            if (slot >= vm.stack.len) break :blk null;
            break :blk &vm.stack[slot];
        };
        if (slot_ptr == null) continue;

        const offset = field.offset;
        const size = field.size;
        if (offset + size > buffer.len) continue;
        const src = buffer[offset .. offset + size];

        switch (field.data_type) {
            .decimal => {
                const trimmed = std.mem.trim(u8, src, " \t\x00");
                const int_val = std.fmt.parseInt(i64, trimmed, 10) catch 0;
                // Use field.precision to preserve implied decimal semantics
                // For d10.2, value 12345 with precision 2 represents 123.45
                const new_val = if (field.precision > 0)
                    Value.initDecimal(vm.valueAllocator(), int_val, field.precision) catch Value.initInt(int_val)
                else
                    Value.initInt(int_val);
                // Inline ARC for slot write
                arc.retain(new_val);
                arc.release(slot_ptr.?.*, vm.allocator);
                slot_ptr.?.* = new_val;
            },
            .string => {
                const str_copy = vm.allocator.alloc(u8, size) catch continue;
                @memcpy(str_copy, src);
                const new_val = Value.initFixedString(vm.valueAllocator(), str_copy) catch {
                    vm.allocator.free(str_copy);
                    continue;
                };
                // Inline ARC for slot write
                arc.retain(new_val);
                arc.release(slot_ptr.?.*, vm.allocator);
                slot_ptr.?.* = new_val;
            },
            else => {},
        }
    }

    return .continue_dispatch;
}

// ============================================================================
// Map Operations (0xD5-0xDF)
// ============================================================================

/// map_new rd, flags - create a new map
pub fn op_map_new(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const flags: u4 = @truncate(ops & 0xF);

    const map = arc.create(vm.allocator, OrderedMap) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate map");
    };
    map.* = OrderedMap.init(vm.allocator, .{
        .case_sensitive = (flags & 1) != 0,
        .preserve_spaces = (flags & 2) != 0,
    });

    vm.writeRegister(rd, Value.initMap(map));
    return .continue_dispatch;
}

/// map_set map, key, val - store value in map
pub fn op_map_set(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const map_reg: u4 = @truncate(ops1 >> 4);
    const key_reg: u4 = @truncate(ops1 & 0xF);
    const val_reg: u4 = @truncate(ops2 >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    _ = map.set(key, vm.registers[val_reg]) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to set map value");
    };

    return .continue_dispatch;
}

/// map_get rd, map, key - get value from map
pub fn op_map_get(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops1 >> 4);
    const map_reg: u4 = @truncate(ops1 & 0xF);
    const key_reg: u4 = @truncate(ops2 >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    vm.writeRegister(rd, map.get(key) orelse Value.null_val);
    return .continue_dispatch;
}

/// map_delete map, key - delete key from map
pub fn op_map_delete(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const map_reg: u4 = @truncate(ops1 >> 4);
    const key_reg: u4 = @truncate(ops1 & 0xF);
    _ = ops2;

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    _ = map.delete(key);
    return .continue_dispatch;
}

/// map_has rd, map, key - check if key exists in map
pub fn op_map_has(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops1 >> 4);
    const map_reg: u4 = @truncate(ops1 & 0xF);
    const key_reg: u4 = @truncate(ops2 >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    vm.writeRegister(rd, if (map.has(key)) Value.true_val else Value.false_val);
    return .continue_dispatch;
}

/// map_len rd, map - get number of entries in map
pub fn op_map_len(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const map_reg: u4 = @truncate(ops & 0xF);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    vm.writeRegister(rd, Value.initInt(@intCast(map.len())));
    return .continue_dispatch;
}

/// map_clear map - clear all entries from map
pub fn op_map_clear(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const map_reg: u4 = @truncate(ops >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    map.clear();
    return .continue_dispatch;
}

/// map_keys rd, map - get keys as comma-separated string
pub fn op_map_keys(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const map_reg: u4 = @truncate(ops & 0xF);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    // Build comma-separated string of keys
    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(vm.allocator);

    var iter = map.iterator();
    var first = true;
    while (iter.next()) |entry| {
        if (!first) {
            result.appendSlice(vm.allocator, ",") catch return VMError.OutOfMemory;
        }
        result.appendSlice(vm.allocator, entry.key) catch return VMError.OutOfMemory;
        first = false;
    }

    const str = result.toOwnedSlice(vm.allocator) catch return VMError.OutOfMemory;
    vm.writeRegister(rd, Value.initString(vm.allocator, str) catch {
        vm.allocator.free(str);
        return VMError.OutOfMemory;
    });
    vm.allocator.free(str);
    return .continue_dispatch;
}

/// map_values rd, map - get values as comma-separated string
pub fn op_map_values(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const map_reg: u4 = @truncate(ops & 0xF);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    // Build comma-separated string of values
    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(vm.allocator);

    var val_buf: [32]u8 = undefined;
    var iter = map.iterator();
    var first = true;
    while (iter.next()) |entry| {
        if (!first) {
            result.appendSlice(vm.allocator, ",") catch return VMError.OutOfMemory;
        }
        const val_str = vm.valueToStringSlice(entry.value, &val_buf);
        result.appendSlice(vm.allocator, val_str) catch return VMError.OutOfMemory;
        first = false;
    }

    const str = result.toOwnedSlice(vm.allocator) catch return VMError.OutOfMemory;
    vm.writeRegister(rd, Value.initString(vm.allocator, str) catch {
        vm.allocator.free(str);
        return VMError.OutOfMemory;
    });
    vm.allocator.free(str);
    return .continue_dispatch;
}

/// map_get_at rd, map, access_code - get entry at access code position
pub fn op_map_get_at(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops1 >> 4);
    const map_reg: u4 = @truncate(ops1 & 0xF);
    const idx_reg: u4 = @truncate(ops2 >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    const idx_val = vm.registers[idx_reg];
    if (idx_val.tag() != .integer) {
        return vm.fail(VMError.InvalidType, "Expected integer index");
    }
    const idx = idx_val.asInt();

    if (idx <= 0) {
        vm.writeRegister(rd, Value.null_val);
        return .continue_dispatch;
    }

    const entry = map.getAt(@intCast(idx - 1)); // 1-based to 0-based
    if (entry) |e| {
        vm.writeRegister(rd, e.value);
    } else {
        vm.writeRegister(rd, Value.null_val);
    }
    return .continue_dispatch;
}

/// map_set_at map, access_code, val - set entry at access code position
pub fn op_map_set_at(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const map_reg: u4 = @truncate(ops1 >> 4);
    const idx_reg: u4 = @truncate(ops1 & 0xF);
    const val_reg: u4 = @truncate(ops2 >> 4);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    const idx_val = vm.registers[idx_reg];
    if (idx_val.tag() != .integer) {
        return vm.fail(VMError.InvalidType, "Expected integer index");
    }
    const idx = idx_val.asInt();

    if (idx <= 0) {
        return .continue_dispatch;
    }

    _ = map.putAt(@intCast(idx - 1), vm.registers[val_reg]); // 1-based to 0-based
    return .continue_dispatch;
}

// ============================================================================
// Math Functions (0xC0-0xCF)
// ============================================================================

/// fn_round rd, rs, prec - rd = round(rs, precision)
/// Performs true rounding (banker's rounding) to specified decimal places
/// Format: [rd:4|rs:4] [prec:8]
pub fn op_fn_round(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const prec = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    const val = vm.registers[rs];

    // Get numeric value
    const num: f64 = switch (val.tag()) {
        .integer => @floatFromInt(val.asInt()),
        .decimal => blk: {
            if (val.asDecimal()) |dval| {
                const divisor: f64 = @floatFromInt(std.math.pow(i64, 10, dval.precision));
                break :blk @as(f64, @floatFromInt(dval.value)) / divisor;
            }
            break :blk 0.0;
        },
        else => 0.0,
    };

    // Calculate rounding factor
    const factor: f64 = @floatFromInt(std.math.pow(i64, 10, prec));

    // True rounding: round to nearest, ties to even (banker's rounding)
    const scaled = num * factor;
    const rounded = @round(scaled);
    const result = rounded / factor;

    // Return as decimal if precision > 0, otherwise as integer
    if (prec > 0) {
        const int_val: i64 = @intFromFloat(rounded);
        vm.writeRegister(rd, Value.initDecimal(vm.allocator, int_val, prec) catch return VMError.OutOfMemory);
    } else {
        vm.writeRegister(rd, Value.initInt(@intFromFloat(result)));
    }

    return .continue_dispatch;
}

/// fn_trunc rd, rs, prec - rd = trunc(rs, precision)
/// Truncates (towards zero) to specified decimal places
/// Format: [rd:4|rs:4] [prec:8]
pub fn op_fn_trunc(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const prec = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    const val = vm.registers[rs];

    // Get numeric value
    const num: f64 = switch (val.tag()) {
        .integer => @floatFromInt(val.asInt()),
        .decimal => blk: {
            if (val.asDecimal()) |dval| {
                const divisor: f64 = @floatFromInt(std.math.pow(i64, 10, dval.precision));
                break :blk @as(f64, @floatFromInt(dval.value)) / divisor;
            }
            break :blk 0.0;
        },
        else => 0.0,
    };

    // Calculate truncation factor
    const factor: f64 = @floatFromInt(std.math.pow(i64, 10, prec));

    // Truncate towards zero
    const scaled = num * factor;
    const truncated = @trunc(scaled);
    const result = truncated / factor;

    // Return as decimal if precision > 0, otherwise as integer
    if (prec > 0) {
        const int_val: i64 = @intFromFloat(truncated);
        vm.writeRegister(rd, Value.initDecimal(vm.allocator, int_val, prec) catch return VMError.OutOfMemory);
    } else {
        vm.writeRegister(rd, Value.initInt(@intFromFloat(result)));
    }

    return .continue_dispatch;
}
