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
const Closure = value_mod.Closure;
const TraitObject = value_mod.TraitObject;
const List = value_mod.List;
const StructBox = value_mod.StructBox;
const Record = value_mod.Record;
const Variant = value_mod.Variant;
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
    const prev_ip = vm.ip - 1; // IP of the opcode itself
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const val = vm.stack[vm.fp + slot];

    // Slot-level tracing
    if (vm.tracer) |tracer| {
        tracer.onSlotLoad(@intCast(prev_ip), slot, rd, val);
    }

    vm.writeRegister(rd, val);
    return .continue_dispatch;
}

/// store_local rs, slot - store to local variable
pub fn op_store_local(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = module.code[vm.ip + 1];
    const prev_ip = vm.ip - 1;
    vm.ip += 2;
    const rs: u4 = @truncate(ops >> 4);
    const new_value = vm.registers[rs];
    const stack_idx = vm.fp + slot;

    // Slot-level tracing: capture old value before overwrite (critical for debugging)
    if (vm.tracer) |tracer| {
        const old_value = vm.stack[stack_idx];
        tracer.onSlotStore(@intCast(prev_ip), slot, rs, old_value, new_value);
    }

    vm.writeStack(stack_idx, new_value);
    return .continue_dispatch;
}

/// load_local16 rd, slot16 - load from local (wide)
pub fn op_load_local16(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    const prev_ip = vm.ip - 1;
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    const val = vm.stack[vm.fp + slot];

    // Slot-level tracing
    if (vm.tracer) |tracer| {
        tracer.onSlotLoad(@intCast(prev_ip), slot, rd, val);
    }

    vm.writeRegister(rd, val);
    return .continue_dispatch;
}

/// store_local16 rs, slot16 - store to local (wide)
pub fn op_store_local16(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    const prev_ip = vm.ip - 1;
    vm.ip += 3;
    const rs: u4 = @truncate(ops >> 4);
    const new_value = vm.registers[rs];
    const stack_idx = vm.fp + slot;

    // Slot-level tracing: capture old value before overwrite
    if (vm.tracer) |tracer| {
        const old_value = vm.stack[stack_idx];
        tracer.onSlotStore(@intCast(prev_ip), slot, rs, old_value, new_value);
    }

    vm.writeStack(stack_idx, new_value);
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

/// div rd, ra, rb - division (handles both int and float)
pub fn op_div(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const ra: u4 = @truncate(ops & 0xF);
    const rb: u4 = @truncate(ops2 >> 4);
    const val_a = vm.registers[ra];
    const val_b = vm.registers[rb];

    // Check if either operand is a float - if so, use float division
    if (val_a.isFloat() or val_b.isFloat()) {
        const a_float = if (val_a.isFloat()) val_a.asFloat() else @as(f64, @floatFromInt(val_a.toInt()));
        const b_float = if (val_b.isFloat()) val_b.asFloat() else @as(f64, @floatFromInt(val_b.toInt()));
        if (b_float == 0.0) {
            return vm.fail(VMError.DivisionByZero, "Division by zero");
        }
        // Store result as float (NaN-boxed - bits are the f64 directly)
        vm.writeRegister(rd, Value{ .bits = @bitCast(a_float / b_float) });
    } else {
        // Integer division
        const divisor = val_b.toInt();
        if (divisor == 0) {
            return vm.fail(VMError.DivisionByZero, "Division by zero");
        }
        vm.writeRegister(rd, Value.initInt(@divTrunc(val_a.toInt(), divisor)));
    }
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

/// is_type rd, rs, type_tag - check if value is of given type
/// type_tag: 0=null, 1=bool, 2=int, 3=float, 4=string
pub fn op_is_type(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const type_tag = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    const val = vm.registers[rs];
    const val_tag = val.tag();

    const is_match = switch (type_tag) {
        0 => val_tag == .null_val, // null
        1 => val_tag == .boolean, // bool
        2 => val_tag == .integer, // int/i64
        3 => val_tag == .float or val_tag == .implied_decimal, // float/f64/decimal
        4 => val_tag == .string or val_tag == .fixed_string, // string
        else => false,
    };
    vm.writeRegister(rd, Value.initBool(is_match));
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

/// shl rd, rs1, rs2 - shift left
/// Format: [rd:4|rs1:4] [rs2:4|0]
pub fn op_shl(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(ops2 >> 4);
    const shift_amount: u6 = @intCast(@as(u64, @bitCast(vm.registers[rs2].toInt())) & 63);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() << shift_amount));
    return .continue_dispatch;
}

/// shr rd, rs1, rs2 - shift right (arithmetic)
/// Format: [rd:4|rs1:4] [rs2:4|0]
pub fn op_shr(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(ops2 >> 4);
    const shift_amount: u6 = @intCast(@as(u64, @bitCast(vm.registers[rs2].toInt())) & 63);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() >> shift_amount));
    return .continue_dispatch;
}

/// bit_and rd, rs1, rs2 - bitwise AND
/// Format: [rd:4|rs1:4] [rs2:4|0]
pub fn op_bit_and(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() & vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// bit_or rd, rs1, rs2 - bitwise OR
/// Format: [rd:4|rs1:4] [rs2:4|0]
pub fn op_bit_or(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() | vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// bit_xor rd, rs1, rs2 - bitwise XOR
/// Format: [rd:4|rs1:4] [rs2:4|0]
pub fn op_bit_xor(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs1: u4 = @truncate(ops & 0xF);
    const rs2: u4 = @truncate(ops2 >> 4);
    vm.writeRegister(rd, Value.initInt(vm.registers[rs1].toInt() ^ vm.registers[rs2].toInt()));
    return .continue_dispatch;
}

/// bit_not rd, rs - bitwise NOT
/// Format: [rd:4|rs:4] [0]
pub fn op_bit_not(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    vm.writeRegister(rd, Value.initInt(~vm.registers[rs].toInt()));
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
// Exception Handling
// ============================================================================

/// set_error_handler offset16 - set error handler at relative offset
/// Format: [opcode][offset_lo][offset_hi] = 3 bytes
/// Note: IP is already at opcode+1 when handler is called (dispatch pre-increments)
/// Pushes a new handler onto the stack for nested try/catch support.
pub fn op_set_error_handler(vm: *VM, module: *const Module) VMError!DispatchResult {
    // ip is at offset_lo (dispatch already skipped opcode), read 16-bit offset
    const offset: i16 = @bitCast(std.mem.readInt(u16, module.code[vm.ip..][0..2], .little));

    // Calculate absolute handler IP (offset is relative to end of instruction)
    // End of instruction = vm.ip + 2 (current position + 2 bytes for offset)
    const end_of_instr: i32 = @intCast(vm.ip + 2);
    const handler_ip = end_of_instr + offset;

    if (handler_ip < 0 or handler_ip >= @as(i32, @intCast(module.code.len))) {
        return vm.fail(VMError.BytecodeOutOfBounds, "Error handler target out of bounds");
    }

    // Push new handler onto stack (preserves outer handlers for nested try/catch)
    vm.error_handlers.append(.{
        .ip = @intCast(handler_ip),
        .sp = vm.sp,
        .fp = vm.fp,
        .call_depth = vm.call_stack.len,
    }) catch {
        return vm.fail(VMError.StackOverflow, "Too many nested try/catch blocks");
    };

    vm.ip += 2; // offset (2) = 2 more bytes (3 total with opcode)
    return .continue_dispatch;
}

/// clear_error_handler - remove current error handler
/// Note: This opcode has no operands. The main dispatch loop already advanced
/// IP past the opcode byte, so we don't need to advance IP here.
/// Pops the current handler from the stack, restoring the outer handler (if any).
pub fn op_clear_error_handler(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = module;
    // Pop the current handler from the stack
    _ = vm.error_handlers.popOrNull();
    return .continue_dispatch;
}

/// throw rs - throw exception with value in rs
/// Format: [opcode][rs:4|0][0]
/// Uses the top handler from the stack and pops it (so re-throws use outer handlers).
pub fn op_throw(vm: *VM, module: *const Module) VMError!DispatchResult {
    _ = module;
    const ops = vm.current_module.?.code[vm.ip + 1];
    const rs: u4 = @truncate(ops >> 4);
    const error_value = vm.registers[rs];

    // Check if there's an error handler set (use top of stack)
    if (vm.error_handlers.popOrNull()) |handler| {
        // Store error value in r0 for catch block access
        vm.registers[0] = error_value;

        // Restore stack state to when the handler was set (unwind call stack)
        vm.sp = handler.sp;
        vm.fp = handler.fp;

        // Pop call frames back to the saved depth
        while (vm.call_stack.len > handler.call_depth) {
            _ = vm.call_stack.pop();
        }

        // Jump to handler (handler was already popped from stack)
        vm.ip = handler.ip;
        return .continue_dispatch;
    } else {
        // No handler - abort with unhandled exception
        return vm.fail(VMError.UnhandledException, "Unhandled exception");
    }
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

/// debug_line - set current source line for debugging
/// Format: [opcode][0][line_lo][line_hi] = 4 bytes
/// Note: IP is already at opcode+1 when handler is called (dispatch pre-increments)
pub fn op_debug_line(vm: *VM, module: *const Module) VMError!DispatchResult {
    // ip is at padding byte (dispatch already skipped opcode), read line from ip+1
    const line = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // padding (1) + line u16 (2) = 3 more bytes (4 total with opcode)
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
    const argc: u4 = @truncate(ops >> 4);
    const stack_argc_low: u4 = @truncate(ops & 0xF);

    // Handle extended format: if stack_argc_low == 15, read actual count from next 2 bytes
    const stack_argc: u16 = if (stack_argc_low == 15) blk: {
        const ext = std.mem.readInt(u16, module.code[vm.ip + 3 ..][0..2], .little);
        vm.ip += 5; // opcode byte + ops byte + routine_idx (2) + stack_argc_ext (2)
        break :blk ext;
    } else blk: {
        vm.ip += 3; // opcode byte + ops byte + routine_idx (2)
        break :blk @as(u16, stack_argc_low);
    };

    if (routine_idx >= module.routines.len) {
        return vm.fail(VMError.InvalidRoutine, "Routine index out of bounds");
    }

    // JIT Profiling: record function call
    if (vm.jitProfilingEnabled()) {
        const module_idx = vm.current_module_index orelse 0;
        _ = vm.recordJITCall(module_idx, routine_idx);
    }

    const routine = module.routines[routine_idx];

    // Push call frame
    // stack_pointer saved is BEFORE overflow args, so they're cleaned up on return
    const caller_sp = vm.sp;
    vm.call_stack.append(.{
        .module = module,
        .routine_index = routine_idx,
        .return_ip = vm.ip,
        .base_pointer = vm.fp,
        .stack_pointer = caller_sp - @as(u32, stack_argc),
        .caller_module_index = vm.current_module_index,
    }) catch return vm.fail(VMError.StackOverflow, "Call stack overflow");

    // Copy args from r0..r(argc-1) to stack (as locals for callee)
    // CRITICAL: Must retain heap values - callee will release them when storing
    // new values to these slots, and we need caller's copy to remain valid.
    // NOTE: Stack pointers need their frame_offset adjusted by -1 because the
    // callee's frame is now one level deeper than when the pointer was created.
    vm.fp = vm.sp; // New frame starts after overflow args (they're below fp now)
    for (0..argc) |i| {
        var arg_value = vm.registers[i];
        // Adjust stack pointers: decrement frame_offset to account for new call frame
        if (arg_value.isStackPtr()) {
            if (arg_value.asStackPtr()) |sp| {
                arg_value = Value.initStackPtr(sp.frame_offset - 1, sp.slot);
            }
        }
        if (vm.runtime_arc_enabled) arc.retain(arg_value);
        vm.stack[vm.sp] = arg_value;
        vm.sp += 1;
    }

    // Copy overflow args from caller's stack (below fp) to callee's locals
    // They were pushed at positions caller_sp - stack_argc to caller_sp - 1
    // and need to end up at slots argc..argc+stack_argc-1
    // CRITICAL: Same retain requirement as register args above.
    // NOTE: Stack pointers also need frame_offset adjustment like register args.
    for (0..stack_argc) |i| {
        var arg_value = vm.stack[caller_sp - @as(u32, stack_argc) + i];
        // Adjust stack pointers: decrement frame_offset to account for new call frame
        if (arg_value.isStackPtr()) {
            if (arg_value.asStackPtr()) |sp| {
                arg_value = Value.initStackPtr(sp.frame_offset - 1, sp.slot);
            }
        }
        if (vm.runtime_arc_enabled) arc.retain(arg_value);
        vm.stack[vm.sp] = arg_value;
        vm.sp += 1;
    }

    // Reserve space for remaining locals (after argc + stack_argc)
    const total_args = @as(u32, argc) + @as(u32, stack_argc);
    for (total_args..routine.local_count) |_| {
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

/// ret count - return from subroutine, preserving `count` overflow values on stack
/// Format: [count:8] [0] - count=0 for void return, count>0 for overflow values
pub fn op_ret(vm: *VM, module: *const Module) VMError!DispatchResult {
    const count: u8 = module.code[vm.ip];
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

        // Release all callee's local slots.
        // CRITICAL: This balances the retain done in op_call when copying args.
        if (vm.runtime_arc_enabled) {
            for (0..routine.local_count) |i| {
                arc.release(vm.stack[vm.fp + i], vm.allocator);
            }
        }

        if (count > 0) {
            // Overflow return: preserve `count` values that were pushed via push_arg
            // Copy them from current stack position to caller's stack space
            const src_base = vm.sp - @as(u32, count);
            for (0..count) |i| {
                vm.stack[frame.stack_pointer + i] = vm.stack[src_base + i];
            }
            // Release pushed values (ownership transferred to caller via copy)
            if (vm.runtime_arc_enabled) {
                for (0..count) |i| {
                    arc.release(vm.stack[src_base + i], vm.allocator);
                }
            }
            vm.sp = frame.stack_pointer + @as(u32, count);
        } else {
            vm.sp = frame.stack_pointer;
        }

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
    const return_value = vm.registers[rs];
    vm.writeRegister(15, return_value);

    if (vm.call_stack.pop()) |frame| {
        // Copy back ref parameters to caller's registers
        const routine = module.routines[frame.routine_index];
        for (routine.params, 0..) |param, i| {
            if (param.mode == .ref) {
                // Copy value from callee's stack slot back to caller's register
                vm.writeRegister(@truncate(i), vm.stack[vm.fp + i]);
            }
        }

        // Trace return with actual value (helps debug return value issues)
        if (vm.tracer) |tracer| {
            const routine_name = if (routine.name_index < module.constants.len)
                switch (module.constants[routine.name_index]) {
                    .identifier => |s| s,
                    .string => |s| s,
                    else => "?",
                }
            else
                "?";
            tracer.onReturnWithValue(routine_name, true, @intCast(vm.ip), return_value);
        }

        // Release all callee's local slots.
        // CRITICAL: This balances the retain done in op_call when copying args.
        // See op_ret for detailed explanation.
        if (vm.runtime_arc_enabled) {
            for (0..routine.local_count) |i| {
                arc.release(vm.stack[vm.fp + i], vm.allocator);
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

/// push_arg slot - push local slot value to stack for overflow args
/// Format: [slot:16] = 2 operand bytes. Dispatch already advanced past opcode.
pub fn op_push_arg(vm: *VM, module: *const Module) VMError!DispatchResult {
    const slot = std.mem.readInt(u16, module.code[vm.ip..][0..2], .little);
    vm.ip += 2;

    // Push value from local slot to stack
    // CRITICAL: Must retain because the pushed copy becomes an independent reference.
    // ret with count > 0 will release this when transferring ownership to caller.
    const stack_addr = vm.fp + slot;
    const value = vm.stack[stack_addr];
    if (vm.runtime_arc_enabled) arc.retain(value);
    vm.stack[vm.sp] = value;
    vm.sp += 1;

    return .continue_dispatch;
}

/// push_arg_reg rs - push register value to stack for overflow args
pub fn op_push_arg_reg(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rs: u4 = @truncate(ops >> 4);

    // Push register value to stack
    // CRITICAL: Must retain because the pushed copy becomes an independent reference.
    const value = vm.registers[rs];
    if (vm.runtime_arc_enabled) arc.retain(value);
    vm.stack[vm.sp] = value;
    vm.sp += 1;

    return .continue_dispatch;
}

/// pop_arg slot - pop stack value to local slot (not typically needed, call handles this)
/// Format: [slot:16] = 2 operand bytes. Dispatch already advanced past opcode.
pub fn op_pop_arg(vm: *VM, module: *const Module) VMError!DispatchResult {
    const slot = std.mem.readInt(u16, module.code[vm.ip..][0..2], .little);
    vm.ip += 2;

    // Pop value from stack to local slot
    if (vm.sp == 0) {
        return vm.fail(VMError.StackUnderflow, "Stack underflow in pop_arg");
    }
    vm.sp -= 1;
    vm.stack[vm.fp + slot] = vm.stack[vm.sp];

    return .continue_dispatch;
}

// ============================================================================
// Stack Pointer Operations
// ============================================================================

/// get_local_ptr rd, slot - rd = stack pointer to local slot
/// Creates a StackPtr value pointing to the specified local variable slot.
pub fn op_get_local_ptr(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;

    const rd: u4 = @truncate(ops >> 4);

    // Create a stack pointer to the local slot in the current frame
    // frame_offset=0 means current frame
    vm.registers[rd] = Value.initStackPtr(0, slot);

    return .continue_dispatch;
}

/// load_indirect rd, rs, offset - rd = ptr_val[offset]
/// Loads a value through a pointer with a field offset.
/// Works for both stack pointers and heap records (polymorphic).
pub fn op_load_indirect(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const offset = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;

    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops);

    const ptr_val = vm.registers[rs];

    // Check if it's a stack pointer
    if (ptr_val.asStackPtr()) |sp| {
        // Resolve the frame's base pointer
        const base_ptr = try resolveFrameBasePointer(vm, sp.frame_offset);
        const abs_slot = base_ptr + sp.slot + offset;

        if (abs_slot >= vm.sp) {
            return vm.fail(VMError.StackOverflow, "load_indirect: slot out of bounds");
        }

        vm.registers[rd] = vm.stack[abs_slot];
        return .continue_dispatch;
    }

    // Check if it's a heap record
    if (ptr_val.asRecord()) |record| {
        const field_count = record.data.len / @sizeOf(Value);
        if (offset < field_count) {
            const fields: []Value = @as([*]Value, @ptrCast(@alignCast(record.data.ptr)))[0..field_count];
            vm.writeRegister(rd, fields[offset]);
        } else {
            vm.writeRegister(rd, Value.null_val);
        }
        return .continue_dispatch;
    }

    return vm.fail(VMError.InvalidType, "load_indirect: expected stack pointer or record");
}

/// store_indirect rs_ptr, offset, rs_val - ptr_val[offset] = rs_val
/// Stores a value through a pointer with a field offset.
/// Works for both stack pointers and heap records (polymorphic).
pub fn op_store_indirect(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const offset = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;

    const rs_ptr: u4 = @truncate(ops >> 4);
    const rs_val: u4 = @truncate(ops);

    const ptr_val = vm.registers[rs_ptr];
    const value = vm.registers[rs_val];

    // Check if it's a stack pointer
    if (ptr_val.asStackPtr()) |sp| {
        // Resolve the frame's base pointer
        const base_ptr = try resolveFrameBasePointer(vm, sp.frame_offset);
        const abs_slot = base_ptr + sp.slot + offset;

        if (abs_slot >= vm.sp) {
            return vm.fail(VMError.StackOverflow, "store_indirect: slot out of bounds");
        }

        vm.stack[abs_slot] = value;
        return .continue_dispatch;
    }

    // Check if it's a heap record
    if (ptr_val.asRecord()) |record| {
        const field_count = record.data.len / @sizeOf(Value);
        if (offset < field_count) {
            const fields: []Value = @as([*]Value, @ptrCast(@alignCast(record.data.ptr)))[0..field_count];
            fields[offset] = value;
        }
        return .continue_dispatch;
    }

    return vm.fail(VMError.InvalidType, "store_indirect: expected stack pointer or record");
}

/// Resolve a frame offset to an absolute base pointer
/// frame_offset=0 means current frame (vm.fp)
/// frame_offset=-1 means caller's frame, etc.
fn resolveFrameBasePointer(vm: *VM, frame_offset: i16) VMError!usize {
    if (frame_offset == 0) {
        // Current frame
        return vm.fp;
    } else if (frame_offset < 0) {
        // Walk back through call stack
        const depth: usize = @intCast(-frame_offset);
        if (depth > vm.call_stack.len) {
            return vm.fail(VMError.StackUnderflow, "stack pointer references invalid frame");
        }
        // call_stack[len-1] is the frame we came from (caller)
        // call_stack[len-2] is the caller's caller, etc.
        const frame_idx = vm.call_stack.len - depth;
        return vm.call_stack.buffer[frame_idx].base_pointer;
    } else {
        // Positive frame_offset not supported (would reference future frames)
        return vm.fail(VMError.InvalidType, "stack pointer: positive frame_offset not supported");
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

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, result);

    // Store result as string value
    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

/// str_len rd, rs - rd = len(rs)
/// Returns the length of the string in rs
/// Format: [rd:4|rs:4] [0]
pub fn op_str_len(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    // Get string from register, coerce to string if needed
    var buf: [32]u8 = undefined;
    const s = vm.valueToStringSlice(vm.registers[rs], &buf);

    // Store length as integer
    vm.writeRegister(rd, Value.initInt(@intCast(s.len)));
    return .continue_dispatch;
}

/// str_trim rd, rs - rd = trim(rs)
/// Trims whitespace from both ends of the string in rs
/// Format: [rd:4|rs:4] [0]
pub fn op_str_trim(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    // Get string from register
    var buf: [32]u8 = undefined;
    const s = vm.valueToStringSlice(vm.registers[rs], &buf);

    // Trim whitespace
    const trimmed = std.mem.trim(u8, s, " \t\r\n");

    // Allocate and copy result
    const result = vm.allocator.alloc(u8, trimmed.len) catch return VMError.OutOfMemory;
    @memcpy(result, trimmed);

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, result);

    // Store result as string value
    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

/// fn_size rd, rs - rd = size(rs)
/// Returns the size/length of the value (string length, array length, etc.)
/// Format: [rd:4|rs:4] [0]
pub fn op_fn_size(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);

    // Get string from register, coerce to string if needed
    var buf: [32]u8 = undefined;
    const s = vm.valueToStringSlice(vm.registers[rs], &buf);

    // Store length as integer
    vm.writeRegister(rd, Value.initInt(@intCast(s.len)));
    return .continue_dispatch;
}

/// fn_date rd - rd = current date as YYYYMMDD integer
/// High-performance opcode for date() builtin
/// Format: [rd:4|0:4] [0]
pub fn op_fn_date(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);

    const now = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(now) };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const year: i64 = year_day.year;
    const month: i64 = @intFromEnum(month_day.month);
    const day_num: i64 = month_day.day_index + 1;

    const result = year * 10000 + month * 100 + day_num;
    vm.writeRegister(rd, Value.initInt(result));
    return .continue_dispatch;
}

/// fn_time rd - rd = current time as HHMMSS integer
/// High-performance opcode for time() builtin
/// Format: [rd:4|0:4] [0]
pub fn op_fn_time(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);

    const now = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(now) };
    const day_secs = epoch.getDaySeconds();

    const hours: i64 = day_secs.getHoursIntoDay();
    const mins: i64 = day_secs.getMinutesIntoHour();
    const secs: i64 = day_secs.getSecondsIntoMinute();

    const result = hours * 10000 + mins * 100 + secs;
    vm.writeRegister(rd, Value.initInt(result));
    return .continue_dispatch;
}

/// fn_datetime rd - rd = current datetime as YYYYMMDDHHMMSSUUUUUU (20-digit string)
/// High-performance opcode for datetime() builtin
/// Returns microsecond-precision timestamp as string for DBL compatibility
/// (20 digits exceeds i64 max of ~9.2e18, so returned as string)
/// Format: [rd:4|0:4] [0]
pub fn op_fn_datetime(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);

    // Get nanosecond-precision time (nanoTimestamp returns i128)
    const nanos: i128 = std.time.nanoTimestamp();
    const secs: i64 = @intCast(@divFloor(nanos, std.time.ns_per_s));
    const nanos_rem: i64 = @intCast(@mod(nanos, std.time.ns_per_s));
    const micros: u32 = @intCast(@divFloor(nanos_rem, 1000)); // Convert to microseconds

    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(secs) };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_secs = epoch.getDaySeconds();

    const year: u16 = @intCast(year_day.year);
    const month: u8 = @intFromEnum(month_day.month);
    const day_num: u8 = month_day.day_index + 1;
    const hours: u8 = @intCast(day_secs.getHoursIntoDay());
    const mins: u8 = @intCast(day_secs.getMinutesIntoHour());
    const seconds: u8 = @intCast(day_secs.getSecondsIntoMinute());

    // Build YYYYMMDDHHMMSSUUUUUU (20 digits) as string
    var buf: [20]u8 = undefined;
    _ = std.fmt.bufPrint(&buf, "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>6}", .{
        year, month, day_num, hours, mins, seconds, micros,
    }) catch return VMError.OutOfMemory;

    // Allocate string for result
    const result_str = vm.allocator.alloc(u8, 20) catch return VMError.OutOfMemory;
    @memcpy(result_str, &buf);

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, result_str);

    vm.writeRegister(rd, Value.initString(vm.valueAllocator(), result_str) catch return VMError.OutOfMemory);
    return .continue_dispatch;
}

/// str_slice rd, rs, start_reg, end_or_len_reg - rd = rs[start..end] or rs[start:len]
/// Extracts a substring from the source string.
/// Format: [dest_reg:4|src_reg:4] [start_reg:4|end_or_len_reg:4] [is_length:8]
/// If is_length=0: end_or_len is end index (exclusive, 0-based) for modern Cot syntax s[start..end]
/// If is_length=1: end_or_len is length for DBL syntax s(start:len) (1-based, converted internally)
pub fn op_str_slice(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    const is_length = module.code[vm.ip + 2];
    vm.ip += 3;

    const dest_reg: u4 = @truncate(ops1 >> 4);
    const src_reg: u4 = @truncate(ops1 & 0xF);
    const start_reg: u4 = @truncate(ops2 >> 4);
    const end_or_len_reg: u4 = @truncate(ops2 & 0xF);

    // Get source string
    const source_val = vm.registers[src_reg];
    const source = switch (source_val.tag()) {
        .fixed_string, .string => source_val.asString(),
        else => "",
    };

    // Get start and end/length parameters
    const start_val: i64 = vm.registers[start_reg].asInt();
    const end_or_len_val: i64 = vm.registers[end_or_len_reg].asInt();

    // Debug: show str_slice parameters
    {
        const debug = @import("../../runtime/debug.zig");
        debug.print(.vm, "str_slice: src_len={d} start_val={d} end_val={d} is_length={d}", .{
            source.len, start_val, end_or_len_val, is_length,
        });
    }

    // Calculate actual start and length based on mode
    var start: usize = undefined;
    var length: usize = undefined;

    if (is_length != 0) {
        // DBL mode: 1-based indexing, (start:length) syntax
        start = if (start_val > 0) @intCast(start_val - 1) else 0;
        length = if (end_or_len_val > 0) @intCast(end_or_len_val) else 0;
    } else {
        // Modern Cot mode: 0-based indexing, [start..end] syntax (end is exclusive)
        start = if (start_val >= 0) @intCast(start_val) else 0;
        const end: usize = if (end_or_len_val >= 0) @intCast(end_or_len_val) else 0;
        length = if (end > start) end - start else 0;
    }

    // Bounds checking
    if (start >= source.len or length == 0) {
        const empty = vm.allocator.dupe(u8, "") catch return VMError.OutOfMemory;
        try vm.global_buffers.append(vm.allocator, empty);
        vm.writeRegister(dest_reg, Value.initString(vm.valueAllocator(), empty) catch return VMError.OutOfMemory);
        return .continue_dispatch;
    }

    // Calculate actual extraction length
    const actual_length = @min(length, source.len - start);
    const result = vm.allocator.alloc(u8, actual_length) catch return VMError.OutOfMemory;
    @memcpy(result, source[start..][0..actual_length]);

    try vm.global_buffers.append(vm.allocator, result);
    vm.writeRegister(dest_reg, Value.initString(vm.valueAllocator(), result) catch return VMError.OutOfMemory);
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

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, result);

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
        .implied_decimal => blk: {
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

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, result);

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
    const old_value = vm.registers[rd];

    // If rd already holds a weak reference, unregister it from the registry.
    // IMPORTANT: We do NOT release the old value because weak refs don't own their targets.
    // The weak registry just needs to stop tracking this location.
    if (!old_value.isNull()) {
        vm.weak_registry.unregisterWeakRef(old_value, &vm.registers[rd]);
    }

    // Store the new value (without retain - weak refs don't own their targets)
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
// Closure Operations (0xF8-0xF9)
// ============================================================================

/// make_closure rd, env_reg, fn_idx - create closure from function and environment
/// Creates a Closure object containing the routine index and captured environment.
pub fn op_make_closure(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const fn_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // 1 operand byte + 2 fn_idx bytes (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const env_reg: u4 = @truncate(ops & 0xF);

    // Get the environment map
    const env_value = vm.registers[env_reg];
    const env_map: ?*OrderedMap = if (env_value.isMap()) env_value.getMap() else null;

    // Allocate and initialize closure with ARC header
    const closure = arc.create(vm.allocator, Closure) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate closure");
    };
    closure.* = .{
        .routine_idx = fn_idx,
        .module_idx = vm.current_module_index orelse 0,
        .env = env_map,
    };

    // Create closure value and retain the env map
    const closure_val = Value.initClosure(closure);
    if (env_map != null) {
        arc.retain(env_value);
    }

    vm.writeRegister(rd, closure_val);
    return .continue_dispatch;
}

/// call_closure rd, closure_reg, argc - call a closure
/// Extracts function and env from closure, calls function with env as implicit first arg.
pub fn op_call_closure(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const argc = module.code[vm.ip + 1];
    vm.ip += 2; // 1 operand byte + 1 argc byte (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const closure_reg: u4 = @truncate(ops & 0xF);
    _ = rd; // Result register (for future use)

    // Get the closure
    const closure_val = vm.registers[closure_reg];
    const closure = closure_val.getClosure() orelse {
        return vm.fail(VMError.InvalidType, "Expected closure value");
    };

    // Get the routine from the closure
    const routine_idx = closure.routine_idx;
    if (routine_idx >= module.routines.len) {
        return vm.fail(VMError.InvalidRoutine, "Closure routine index out of bounds");
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

    // Set up stack frame
    const routine = module.routines[routine_idx];
    vm.fp = vm.sp;

    // Only push env if there is one
    const has_env = closure.env != null;
    if (has_env) {
        // First local is the closure environment
        vm.stack[vm.sp] = Value.initMap(closure.env.?);
        vm.sp += 1;
    }

    // Copy regular arguments (r0..r(argc-1))
    for (0..argc) |i| {
        vm.stack[vm.sp] = vm.registers[i];
        vm.sp += 1;
    }

    // Reserve space for remaining locals
    const actual_argc = if (has_env) argc + 1 else argc;
    if (actual_argc < routine.local_count) {
        for (actual_argc..routine.local_count) |_| {
            vm.stack[vm.sp] = Value.null_val;
            vm.sp += 1;
        }
    }

    // Jump to routine
    vm.ip = routine.code_offset;
    return .continue_dispatch;
}

// ============================================================================
// Trait Object Operations (0xFA-0xFB)
// ============================================================================

/// make_trait_object rd, src_reg, vtable_idx - create trait object from value
/// Creates a TraitObject containing the value and vtable reference.
pub fn op_make_trait_object(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const vtable_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // 1 operand byte + 2 vtable_idx bytes (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const src_reg: u4 = @truncate(ops & 0xF);

    // Get the source value
    const src_value = vm.registers[src_reg];

    // Allocate and initialize trait object with ARC header
    const trait_obj = arc.create(vm.allocator, TraitObject) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate trait object");
    };
    trait_obj.* = .{
        .data = src_value,
        .vtable_idx = vtable_idx,
    };

    // Create trait object value and retain the source value
    const trait_obj_val = Value.initTraitObject(trait_obj);
    arc.retain(src_value);

    vm.writeRegister(rd, trait_obj_val);
    return .continue_dispatch;
}

/// call_trait_method rd, obj_reg, method_idx, argc - call method on trait object
/// Looks up method in vtable and calls with the data as self.
pub fn op_call_trait_method(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const method_idx = module.code[vm.ip + 1];
    const argc = module.code[vm.ip + 2];
    vm.ip += 3; // 1 operand byte + 1 method_idx + 1 argc byte

    const obj_reg: u4 = @truncate(ops & 0xF);

    // Get the trait object
    const obj_val = vm.registers[obj_reg];
    const trait_obj = obj_val.asTraitObject() orelse {
        return vm.fail(VMError.InvalidType, "Expected trait object value");
    };

    // Get the vtable from the module
    if (trait_obj.vtable_idx >= module.vtables.len) {
        return vm.fail(VMError.InvalidRoutine, "VTable index out of bounds");
    }
    const vtable = module.vtables[trait_obj.vtable_idx];

    // Get the method function name from the vtable
    if (method_idx >= vtable.methods.len) {
        return vm.fail(VMError.InvalidRoutine, "Method index out of bounds");
    }
    const method = vtable.methods[method_idx];

    // Find the routine by name
    var routine_idx: ?usize = null;
    for (module.routines, 0..) |routine, i| {
        if (routine.name_index < module.constants.len) {
            const name = switch (module.constants[routine.name_index]) {
                .identifier => |s| s,
                .string => |s| s,
                else => continue,
            };
            if (std.mem.eql(u8, name, method.fn_name)) {
                routine_idx = i;
                break;
            }
        }
    }

    const found_idx = routine_idx orelse {
        return vm.fail(VMError.InvalidRoutine, "Method not found in module");
    };
    const routine = module.routines[found_idx];

    // Push self (the concrete data) as first argument
    vm.stack[vm.sp] = trait_obj.data;
    vm.sp += 1;

    // Copy regular arguments (r0..r(argc-1))
    for (0..argc) |i| {
        vm.stack[vm.sp] = vm.registers[i];
        vm.sp += 1;
    }

    // Calculate total arg count (self + passed args)
    const total_argc: u32 = argc + 1;
    const caller_sp = vm.sp - total_argc;

    // Save call frame using the VM's call stack append method
    vm.call_stack.append(.{
        .module = module,
        .routine_index = @intCast(found_idx),
        .return_ip = vm.ip,
        .base_pointer = vm.fp,
        .stack_pointer = caller_sp,
        .caller_module_index = vm.current_module_index,
    }) catch return vm.fail(VMError.InvalidRoutine, "Call stack overflow");

    // Set up new frame - fp points to first argument (self)
    vm.fp = caller_sp;
    vm.ip = routine.code_offset;

    // Reserve space for additional local variables beyond the parameters
    if (routine.local_count > total_argc) {
        const extra_locals = routine.local_count - total_argc;
        for (0..extra_locals) |i| {
            vm.stack[vm.sp + i] = Value.initInt(0);
        }
        vm.sp += extra_locals;
    }

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
// Heap Record Operations (for `new` keyword)
// ============================================================================

/// new_record rd, field_count - create a new heap record with field_count fields
/// Format: [rd:4|0] [field_count:16]
pub fn op_new_record(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    const field_count = std.mem.readInt(u16, module.code[vm.ip - 2 ..][0..2], .little);

    // Allocate a heap record using ARC
    // Records are stored as arrays of Values
    const record = arc.create(vm.allocator, Record) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate record");
    };

    // Allocate field storage
    const data = vm.allocator.alloc(u8, field_count * @sizeOf(Value)) catch {
        // Clean up the record allocation on failure
        // Note: Can't use arc.release here as record hasn't been wrapped in a Value yet
        vm.allocator.destroy(record);
        return vm.fail(VMError.OutOfMemory, "Failed to allocate record fields");
    };

    // Initialize all fields to null
    const fields: []Value = @as([*]Value, @ptrCast(@alignCast(data.ptr)))[0..field_count];
    for (fields) |*f| {
        f.* = Value.null_val;
    }

    record.* = .{ .type_id = 0, .data = data };
    vm.writeRegister(rd, Value.initRecord(record));
    return .continue_dispatch;
}

/// load_field rd, rs, field_idx - rd = rs.field[field_idx]
/// Format: [rd:4|rs:4] [field_idx:16]
pub fn op_load_field(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    const field_idx = std.mem.readInt(u16, module.code[vm.ip - 2 ..][0..2], .little);

    const record_val = vm.registers[rs];
    if (record_val.asRecord()) |record| {
        const field_count = record.data.len / @sizeOf(Value);
        if (field_idx < field_count) {
            const fields: []Value = @as([*]Value, @ptrCast(@alignCast(record.data.ptr)))[0..field_count];
            vm.writeRegister(rd, fields[field_idx]);
        } else {
            vm.writeRegister(rd, Value.null_val);
        }
    } else {
        vm.writeRegister(rd, Value.null_val);
    }
    return .continue_dispatch;
}

/// store_field rd, rs, field_idx - rd.field[field_idx] = rs
/// Format: [rd:4|rs:4] [field_idx:16]
pub fn op_store_field(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 3;
    const rd: u4 = @truncate(ops >> 4);
    const rs: u4 = @truncate(ops & 0xF);
    const field_idx = std.mem.readInt(u16, module.code[vm.ip - 2 ..][0..2], .little);

    const record_val = vm.registers[rd];
    const value = vm.registers[rs];

    if (record_val.asRecord()) |record| {
        const field_count = record.data.len / @sizeOf(Value);
        if (field_idx < field_count) {
            const fields: []Value = @as([*]Value, @ptrCast(@alignCast(record.data.ptr)))[0..field_count];
            fields[field_idx] = value;
        }
    }
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
            // Track buffer for cleanup at VM shutdown
            try vm.global_buffers.append(vm.allocator, result_copy);
            const result = Value.initFixedString(vm.valueAllocator(), result_copy) catch {
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

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, buffer);

    // Create a fixed string value from the buffer and store in destination register
    const result = Value.initFixedString(vm.valueAllocator(), buffer) catch {
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
                // Track buffer for cleanup at VM shutdown
                try vm.global_buffers.append(vm.allocator, new_buf);
                const new_val = Value.initFixedString(vm.valueAllocator(), new_buf) catch {
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
                // Track buffer for cleanup at VM shutdown
                vm.global_buffers.append(vm.allocator, str_copy) catch {
                    vm.allocator.free(str_copy);
                    continue;
                };
                const new_val = Value.initFixedString(vm.valueAllocator(), str_copy) catch {
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

/// alloc_buffer slot, size - allocate mutable buffer at local slot
/// Format: [slot:8] [size:16]
/// Creates a mutable FixedString filled with spaces
pub fn op_alloc_buffer(vm: *VM, module: *const Module) VMError!DispatchResult {
    const slot = module.code[vm.ip];
    const size = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3;

    // Allocate buffer filled with spaces (standard DBL initialization)
    const buffer = vm.allocator.alloc(u8, size) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate buffer");
    };
    @memset(buffer, ' ');

    // Track buffer for cleanup at VM shutdown
    try vm.global_buffers.append(vm.allocator, buffer);

    // Create mutable FixedString value
    const val = Value.initFixedString(vm.valueAllocator(), buffer) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to create fixed string");
    };

    // Store in local slot
    const target_slot = vm.fp + slot;
    if (target_slot >= vm.stack.len) {
        return vm.fail(VMError.StackOverflow, "Buffer slot out of bounds");
    }

    // ARC: release old value, store new value
    arc.release(vm.stack[target_slot], vm.allocator);
    vm.stack[target_slot] = val;
    arc.retain(val);

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

/// map_key_at rd, map, idx - get key at access code position as string
/// Format: [rd:4|map:4] [idx:4|0]
pub fn op_map_key_at(vm: *VM, module: *const Module) VMError!DispatchResult {
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

    const entry = map.getAt(@intCast(idx)); // getAt already expects 1-based
    if (entry) |e| {
        // Duplicate the key string and create a Value
        const key_copy = vm.allocator.dupe(u8, e.key) catch {
            return vm.fail(VMError.OutOfMemory, "Failed to allocate key string");
        };
        const key_val = Value.initFixedString(vm.valueAllocator(), key_copy) catch {
            vm.allocator.free(key_copy);
            return vm.fail(VMError.OutOfMemory, "Failed to create string value");
        };
        vm.writeRegister(rd, key_val);
    } else {
        vm.writeRegister(rd, Value.null_val);
    }
    return .continue_dispatch;
}

// ============================================================================
// List Operations
// ============================================================================

/// list_new rd - create a new empty list
pub fn op_list_new(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);

    const list = arc.create(vm.allocator, List) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate list");
    };
    list.* = List.init(vm.allocator);

    vm.writeRegister(rd, Value.initList(list));
    return .continue_dispatch;
}

/// list_push list, val - push value onto list
pub fn op_list_push(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const list_reg: u4 = @truncate(ops >> 4);
    const val_reg: u4 = @truncate(ops & 0xF);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    list.push(vm.registers[val_reg]) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to push to list");
    };

    return .continue_dispatch;
}

/// list_pop rd, list - pop last value from list
pub fn op_list_pop(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const list_reg: u4 = @truncate(ops & 0xF);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    vm.writeRegister(rd, list.pop() orelse Value.null_val);
    return .continue_dispatch;
}

/// list_get rd, list, idx - get value at index
pub fn op_list_get(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const rd: u4 = @truncate(ops1 >> 4);
    const list_reg: u4 = @truncate(ops1 & 0xF);
    const idx_reg: u4 = @truncate(ops2 >> 4);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    const idx_val = vm.registers[idx_reg];
    if (!idx_val.isInt()) {
        return vm.fail(VMError.InvalidType, "Expected integer index");
    }
    const idx = idx_val.asInt();

    if (idx < 0) {
        vm.writeRegister(rd, Value.null_val);
    } else {
        vm.writeRegister(rd, list.get(@intCast(idx)) orelse Value.null_val);
    }
    return .continue_dispatch;
}

/// list_set list, idx, val - set value at index
pub fn op_list_set(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    vm.ip += 2;
    const list_reg: u4 = @truncate(ops1 >> 4);
    const idx_reg: u4 = @truncate(ops1 & 0xF);
    const val_reg: u4 = @truncate(ops2 >> 4);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    const idx_val = vm.registers[idx_reg];
    if (!idx_val.isInt()) {
        return vm.fail(VMError.InvalidType, "Expected integer index");
    }
    const idx = idx_val.asInt();

    if (idx >= 0) {
        _ = list.set(@intCast(idx), vm.registers[val_reg]);
    }
    return .continue_dispatch;
}

/// list_len rd, list - get list length
pub fn op_list_len(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const list_reg: u4 = @truncate(ops & 0xF);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        // Store register info in the VM's error detail buffer
        const reg_msg = switch (list_reg) {
            0 => "r0",
            1 => "r1",
            2 => "r2",
            3 => "r3",
            4 => "r4",
            5 => "r5",
            6 => "r6",
            7 => "r7",
            8 => "r8",
            9 => "r9",
            10 => "r10",
            11 => "r11",
            12 => "r12",
            13 => "r13",
            14 => "r14",
            15 => "r15",
        };
        const type_name = list_val.typeName();
        vm.setErrorDetail(reg_msg, type_name);
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    vm.writeRegister(rd, Value.initInt(@intCast(list.len())));
    return .continue_dispatch;
}

/// list_clear list - clear all items from list
pub fn op_list_clear(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const list_reg: u4 = @truncate(ops >> 4);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    list.clear();
    return .continue_dispatch;
}

/// list_to_slice rd, list - rd = list.to_slice() (converts List<T> to []T)
/// The result is the same list - in Cot, lists can be used as slices directly
pub fn op_list_to_slice(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2;
    const rd: u4 = @truncate(ops >> 4);
    const list_reg: u4 = @truncate(ops);

    const list_val = vm.registers[list_reg];
    // Verify it's actually a list
    _ = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    // In Cot, lists and slices are interchangeable at runtime
    // The list itself is returned - type difference is compile-time only
    vm.writeRegister(rd, list_val);
    return .continue_dispatch;
}

/// list_push_struct - push struct from consecutive slots as a StructBox
/// Format: [list:4|0] [field_count:8] [base_slot:16]
pub fn op_list_push_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const base_slot: u16 = @as(u16, module.code[vm.ip + 2]) | (@as(u16, module.code[vm.ip + 3]) << 8);
    vm.ip += 4;
    const list_reg: u4 = @truncate(ops >> 4);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    // Create a StructBox to hold the struct fields
    const box = StructBox.init(vm.allocator, field_count) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate StructBox");
    };

    // Copy field values from consecutive slots and retain heap-allocated values
    for (0..field_count) |i| {
        const val = vm.stack[vm.fp + base_slot + i];
        arc.retain(val); // StructBox owns a reference to each field
        box.fields[i] = val;
    }

    // Push the boxed struct to the list
    list.push(Value.initStructBox(box)) catch {
        arc.release(Value.initStructBox(box), vm.allocator);
        return vm.fail(VMError.OutOfMemory, "Failed to push to list");
    };

    return .continue_dispatch;
}

/// list_get_struct - get struct from list and expand to consecutive SLOTS
/// Format: [list:4|idx:4] [field_count:8] [dest_slot:16]
/// This writes directly to slots, avoiding the 16-register limitation.
pub fn op_list_get_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const dest_slot: u16 = @as(u16, module.code[vm.ip + 2]) | (@as(u16, module.code[vm.ip + 3]) << 8);
    vm.ip += 4;
    const list_reg: u4 = @truncate(ops >> 4);
    const idx_reg: u4 = @truncate(ops & 0xF);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    const idx = @as(usize, @intCast(vm.registers[idx_reg].asInt()));
    const boxed_val = list.get(idx) orelse {
        return vm.fail(VMError.ArrayOutOfBounds, "List index out of bounds");
    };

    const box = boxed_val.asStructBox() orelse {
        return vm.fail(VMError.InvalidType, "Expected StructBox in list");
    };

    // Expand struct fields directly to consecutive slots (no register limit)
    const copy_count = @min(field_count, box.field_count);
    for (0..copy_count) |i| {
        const val = box.fields[i];
        arc.retain(val); // Slot gets its own reference
        vm.stack[vm.fp + dest_slot + i] = val;
    }

    return .continue_dispatch;
}

/// list_pop_struct - pop struct from list and expand to consecutive slots
/// Format: [list:4|0] [field_count:8] [dest_slot:16]
pub fn op_list_pop_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const dest_slot: u16 = @as(u16, module.code[vm.ip + 2]) | (@as(u16, module.code[vm.ip + 3]) << 8);
    vm.ip += 4;
    const list_reg: u4 = @truncate(ops >> 4);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    const boxed_val = list.pop() orelse {
        // Return null for all slots if list is empty
        for (0..field_count) |i| {
            vm.stack[vm.fp + dest_slot + i] = Value.null_val;
        }
        return .continue_dispatch;
    };

    const box = boxed_val.asStructBox() orelse {
        return vm.fail(VMError.InvalidType, "Expected StructBox in list");
    };

    // Expand struct fields to consecutive slots with retain
    // (transferring ownership from StructBox to stack slots)
    const copy_count = @min(field_count, box.field_count);
    for (0..copy_count) |i| {
        const val = box.fields[i];
        arc.retain(val); // Stack slot gets its own reference
        vm.stack[vm.fp + dest_slot + i] = val;
    }

    // Release the StructBox after unpacking (this releases its references to fields)
    arc.release(boxed_val, vm.allocator);

    return .continue_dispatch;
}

/// list_set_struct - set struct at index from consecutive slots
/// Format: [list:4|idx:4] [field_count:8] [base_slot:16]
pub fn op_list_set_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const base_slot: u16 = @as(u16, module.code[vm.ip + 2]) | (@as(u16, module.code[vm.ip + 3]) << 8);
    vm.ip += 4;
    const list_reg: u4 = @truncate(ops >> 4);
    const idx_reg: u4 = @truncate(ops & 0xF);

    const list_val = vm.registers[list_reg];
    const list = list_val.asList() orelse {
        return vm.fail(VMError.InvalidType, "Expected list value");
    };

    const idx = @as(usize, @intCast(vm.registers[idx_reg].asInt()));

    // Get existing value and release it if it's a StructBox
    if (list.get(idx)) |existing| {
        if (existing.asStructBox() != null) {
            arc.release(existing, vm.allocator);
        }
    }

    // Create a new StructBox for the replacement value
    const box = StructBox.init(vm.allocator, field_count) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate StructBox");
    };

    // Copy field values from consecutive slots with retain
    for (0..field_count) |i| {
        const val = vm.stack[vm.fp + base_slot + i];
        arc.retain(val); // StructBox owns a reference to each field
        box.fields[i] = val;
    }

    // Set the boxed struct in the list
    if (!list.set(idx, Value.initStructBox(box))) {
        arc.release(Value.initStructBox(box), vm.allocator);
        return vm.fail(VMError.ArrayOutOfBounds, "List index out of bounds");
    }

    return .continue_dispatch;
}

/// map_set_struct - store struct from consecutive slots to map
/// Format: [map:4|key:4] [field_count:8] [base_slot:16]
pub fn op_map_set_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const base_slot: u16 = @as(u16, module.code[vm.ip + 2]) | (@as(u16, module.code[vm.ip + 3]) << 8);
    vm.ip += 4;
    const map_reg: u4 = @truncate(ops >> 4);
    const key_reg: u4 = @truncate(ops & 0xF);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    // Create a StructBox to hold the struct fields
    const box = StructBox.init(vm.allocator, field_count) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate StructBox");
    };

    // Copy field values from consecutive slots and retain heap-allocated values
    for (0..field_count) |i| {
        const val = vm.stack[vm.fp + base_slot + i];
        arc.retain(val); // StructBox owns a reference to each field
        box.fields[i] = val;
    }

    // Store the boxed struct in the map
    _ = map.set(key, Value.initStructBox(box)) catch {
        arc.release(Value.initStructBox(box), vm.allocator);
        return vm.fail(VMError.OutOfMemory, "Failed to set map value");
    };

    return .continue_dispatch;
}

/// map_get_struct - get struct from map and expand to consecutive high registers
/// Format: [map:4|key:4] [field_count:8] [base_reg:8] [0]
pub fn op_map_get_struct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_count = module.code[vm.ip + 1];
    const base_reg = module.code[vm.ip + 2];
    vm.ip += 4;
    const map_reg: u4 = @truncate(ops >> 4);
    const key_reg: u4 = @truncate(ops & 0xF);

    const map_val = vm.registers[map_reg];
    const map = map_val.asMap() orelse {
        return vm.fail(VMError.InvalidType, "Expected map value");
    };

    var key_buf: [32]u8 = undefined;
    const key = vm.valueToStringSlice(vm.registers[key_reg], &key_buf);

    const boxed_val = map.get(key) orelse {
        // Return null for all registers if key not found
        for (0..field_count) |i| {
            vm.registers[base_reg + i] = Value.null_val;
        }
        return .continue_dispatch;
    };

    const box = boxed_val.asStructBox() orelse {
        return vm.fail(VMError.InvalidType, "Expected StructBox in map");
    };

    // Expand struct fields to consecutive high registers with retain
    // (caller will store these to stack slots that get cleaned up)
    const copy_count = @min(field_count, box.field_count);
    for (0..copy_count) |i| {
        const val = box.fields[i];
        arc.retain(val); // Caller gets their own reference
        vm.registers[base_reg + i] = val;
    }

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
        .implied_decimal => blk: {
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
        .implied_decimal => blk: {
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

// ============================================================================
// Array Operations (0xB0-0xBF)
// ============================================================================

/// array_load rd, idx_reg - load from array at stack slot
/// Format: [idx_reg:8] [slot:16]
/// Result stored in r0
pub fn op_array_load(vm: *VM, module: *const Module) VMError!DispatchResult {
    const idx_reg: u4 = @truncate(module.code[vm.ip]);
    const slot_lo = module.code[vm.ip + 1];
    const slot_hi = module.code[vm.ip + 2];
    vm.ip += 3;

    const slot: u16 = (@as(u16, slot_hi) << 8) | slot_lo;

    // Get the index value from register
    const idx_val = vm.registers[idx_reg];
    if (idx_val.tag() != .integer) {
        return vm.fail(VMError.InvalidType, "Array index must be integer");
    }
    const idx = idx_val.asInt();

    if (idx < 0) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array index cannot be negative");
    }

    // The array base is at stack[fp + slot], each element at consecutive slots
    const base_index = vm.fp + slot;
    const elem_index = base_index + @as(usize, @intCast(idx));

    if (elem_index >= vm.stack.len) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array index out of bounds");
    }

    // Load the element into r0
    vm.writeRegister(0, vm.stack[elem_index]);

    return .continue_dispatch;
}

/// array_load_opt rd, idx_reg - optional load from array, returns null if out of bounds
/// Format: [idx_reg:8] [slot:16] [len:16]
/// Result stored in r0, returns null instead of throwing on out-of-bounds
pub fn op_array_load_opt(vm: *VM, module: *const Module) VMError!DispatchResult {
    const idx_reg: u4 = @truncate(module.code[vm.ip]);
    const slot_lo = module.code[vm.ip + 1];
    const slot_hi = module.code[vm.ip + 2];
    const len_lo = module.code[vm.ip + 3];
    const len_hi = module.code[vm.ip + 4];
    vm.ip += 5;

    const slot: u16 = (@as(u16, slot_hi) << 8) | slot_lo;
    const array_len: u16 = (@as(u16, len_hi) << 8) | len_lo;

    // Get the index value from register
    const idx_val = vm.registers[idx_reg];
    if (idx_val.tag() != .integer) {
        // Return null for non-integer index instead of throwing
        vm.writeRegister(0, Value.null_val);
        return .continue_dispatch;
    }
    const idx = idx_val.asInt();

    // Return null for negative index
    if (idx < 0) {
        vm.writeRegister(0, Value.null_val);
        return .continue_dispatch;
    }

    // Return null for out of bounds index (check against array length)
    if (@as(u64, @intCast(idx)) >= array_len) {
        vm.writeRegister(0, Value.null_val);
        return .continue_dispatch;
    }

    // The array base is at stack[fp + slot], each element at consecutive slots
    const base_index = vm.fp + slot;
    const elem_index = base_index + @as(usize, @intCast(idx));

    // Load the element into r0
    vm.writeRegister(0, vm.stack[elem_index]);

    return .continue_dispatch;
}

/// array_store idx_reg, val_reg - store to array at stack slot
/// Format: [idx_reg:4|val_reg:4] [slot:16]
pub fn op_array_store(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const slot_lo = module.code[vm.ip + 1];
    const slot_hi = module.code[vm.ip + 2];
    vm.ip += 3;

    const idx_reg: u4 = @truncate(ops >> 4);
    const val_reg: u4 = @truncate(ops & 0xF);
    const slot: u16 = (@as(u16, slot_hi) << 8) | slot_lo;

    // Get the index value
    const idx_val = vm.registers[idx_reg];
    if (idx_val.tag() != .integer) {
        return vm.fail(VMError.InvalidType, "Array index must be integer");
    }
    const idx = idx_val.asInt();

    if (idx < 0) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array index cannot be negative");
    }

    // Get the value to store
    const value = vm.registers[val_reg];

    // Calculate array element position
    const base_index = vm.fp + slot;
    const elem_index = base_index + @as(usize, @intCast(idx));

    if (elem_index >= vm.stack.len) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array index out of bounds");
    }

    // Store the value
    vm.stack[elem_index] = value;

    return .continue_dispatch;
}

/// array_len rd, slot - get length of array (stored in metadata)
/// Format: [rd:8] [slot:16]
/// Note: For now, array length is not tracked dynamically - this is a stub
pub fn op_array_len(vm: *VM, module: *const Module) VMError!DispatchResult {
    const rd: u4 = @truncate(module.code[vm.ip]);
    const slot_lo = module.code[vm.ip + 1];
    const slot_hi = module.code[vm.ip + 2];
    vm.ip += 3;

    _ = slot_lo;
    _ = slot_hi;

    // TODO: Array length needs to be stored somewhere - for now return 0
    // In a proper implementation, we'd store the length in a metadata slot
    // or use a different array representation
    vm.writeRegister(rd, Value.initInt(0));

    return .continue_dispatch;
}

/// array_slice rd, start_reg, end_reg, slot - rd = arr[start..end] or arr[start..=end]
/// Creates a new list containing elements from start to end (exclusive unless inclusive flag set)
/// Format: [rd:4|inclusive:1|0:3] [start_reg:4|end_reg:4] [slot_lo:8] [slot_hi:8]
pub fn op_array_slice(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops1 = module.code[vm.ip];
    const ops2 = module.code[vm.ip + 1];
    const slot_lo = module.code[vm.ip + 2];
    const slot_hi = module.code[vm.ip + 3];
    vm.ip += 4;

    const rd: u4 = @truncate(ops1 >> 4);
    const inclusive = (ops1 & 0x08) != 0; // bit 3 is inclusive flag
    const start_reg: u4 = @truncate(ops2 >> 4);
    const end_reg: u4 = @truncate(ops2 & 0xF);
    const slot: u16 = (@as(u16, slot_hi) << 8) | slot_lo;

    // Get start and end indices
    const start_val = vm.registers[start_reg];
    const end_val = vm.registers[end_reg];

    if (start_val.tag() != .integer or end_val.tag() != .integer) {
        return vm.fail(VMError.InvalidType, "Array slice indices must be integers");
    }

    const start_idx = start_val.asInt();
    var end_idx = end_val.asInt();

    // For inclusive slicing (..=), add 1 to make end index exclusive
    if (inclusive) {
        end_idx += 1;
    }

    if (start_idx < 0 or end_idx < 0) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array slice indices cannot be negative");
    }

    if (start_idx > end_idx) {
        return vm.fail(VMError.ArrayOutOfBounds, "Array slice start cannot be greater than end");
    }

    // Create a new list to hold the slice
    const list = arc.create(vm.allocator, List) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate list for slice");
    };
    list.* = List.init(vm.allocator);

    // Copy elements from source array (stack slots) to the list
    const base_index = vm.fp + slot;
    const start: usize = @intCast(start_idx);
    const end: usize = @intCast(end_idx);

    for (start..end) |i| {
        const elem_index = base_index + i;
        if (elem_index >= vm.stack.len) {
            return vm.fail(VMError.ArrayOutOfBounds, "Array slice index out of bounds");
        }
        const elem = vm.stack[elem_index];
        // Retain heap-allocated values since list takes ownership
        arc.retain(elem);
        list.push(elem) catch {
            return vm.fail(VMError.OutOfMemory, "Failed to push element to slice list");
        };
    }

    vm.writeRegister(rd, Value.initList(list));
    return .continue_dispatch;
}

// ============================================================================
// Variant (Sum Type) Operations (0x8A-0x8C)
// ============================================================================

/// variant_construct rd, tag, argc - construct a variant with payload
/// Creates a Variant value with the given tag and payload values from registers.
/// Format: [rd:4|argc:4] [tag:16]
/// Payload values are in r0..r(argc-1)
pub fn op_variant_construct(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const tag = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // 1 operand byte + 2 tag bytes (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const argc: u4 = @truncate(ops & 0xF);

    // Create the variant with payload count
    const variant = Variant.init(vm.allocator, tag, argc) catch {
        return vm.fail(VMError.OutOfMemory, "Failed to allocate variant");
    };

    // Copy payload values from registers r0..r(argc-1)
    for (0..argc) |i| {
        const payload_val = vm.registers[i];
        // Retain heap-allocated values since variant takes ownership
        arc.retain(payload_val);
        variant.setPayload(i, payload_val);
    }

    vm.writeRegister(rd, Value.initVariant(variant));
    return .continue_dispatch;
}

/// variant_get_tag rd, src_reg - get the tag from a variant
/// Format: [rd:4|src_reg:4] [0]
pub fn op_variant_get_tag(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    vm.ip += 2; // 1 operand byte + 1 padding byte (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const src_reg: u4 = @truncate(ops & 0xF);

    const src_val = vm.registers[src_reg];
    const variant = src_val.asVariant() orelse {
        return vm.fail(VMError.InvalidType, "Expected variant value");
    };

    vm.writeRegister(rd, Value.initInt(@intCast(variant.tag)));
    return .continue_dispatch;
}

/// variant_get_payload rd, src_reg, field_idx - get payload field from variant
/// Format: [rd:4|src_reg:4] [field_idx:16]
pub fn op_variant_get_payload(vm: *VM, module: *const Module) VMError!DispatchResult {
    const ops = module.code[vm.ip];
    const field_idx = std.mem.readInt(u16, module.code[vm.ip + 1 ..][0..2], .little);
    vm.ip += 3; // 1 operand byte + 2 field_idx bytes (opcode already consumed by dispatcher)

    const rd: u4 = @truncate(ops >> 4);
    const src_reg: u4 = @truncate(ops & 0xF);

    const src_val = vm.registers[src_reg];
    const variant = src_val.asVariant() orelse {
        return vm.fail(VMError.InvalidType, "Expected variant value");
    };

    const payload_val = variant.getPayload(field_idx) orelse {
        return vm.fail(VMError.ArrayOutOfBounds, "Variant payload index out of bounds");
    };

    // Retain the value since we're extracting it from the variant
    arc.retain(payload_val);
    vm.writeRegister(rd, payload_val);
    return .continue_dispatch;
}
