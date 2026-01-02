//! Instruction Emission Handlers
//!
//! Converts IR instructions to bytecode.
//! These are free functions that take the BytecodeEmitter as the first parameter.
//!
//! This module handles emission of all IR instruction types to bytecode.

const std = @import("std");
const ir = @import("ir.zig");
const cot_runtime = @import("cot_runtime");
const module = cot_runtime.bytecode.module;
const opcodes = cot_runtime.bytecode.opcodes;
const debug = cot_runtime.debug;

// Import emitter types
const emit_mod = @import("emit_bytecode.zig");
const BytecodeEmitter = emit_mod.BytecodeEmitter;
const EmitError = emit_mod.EmitError;

// Builtin definitions
const emit_builtins = @import("emit_builtins.zig");
const opcode_builtins = emit_builtins.opcode_builtins;
const io_functions = emit_builtins.io_functions;
const native_functions = emit_builtins.native_functions;

const Opcode = opcodes.Opcode;

// Type aliases for inline anonymous struct payloads in ir.Instruction
const Iconst = std.meta.TagPayload(ir.Instruction, .iconst);
const F32const = std.meta.TagPayload(ir.Instruction, .f32const);
const F64const = std.meta.TagPayload(ir.Instruction, .f64const);
const ConstString = std.meta.TagPayload(ir.Instruction, .const_string);
const IoClose = std.meta.TagPayload(ir.Instruction, .io_close);
const IoDelete = std.meta.TagPayload(ir.Instruction, .io_delete);
const LoadStructBuf = std.meta.TagPayload(ir.Instruction, .load_struct_buf);
const StoreStructBuf = std.meta.TagPayload(ir.Instruction, .store_struct_buf);
const DebugLine = std.meta.TagPayload(ir.Instruction, .debug_line);

// ============================================================================
// Memory Operations
// ============================================================================

/// Emit alloca instruction - allocate local variable
pub fn emitAlloca(e: *BytecodeEmitter, a: ir.Instruction.Alloca) EmitError!void {
    // Check if this is a record field (already registered as global)
    if (e.globals.get(a.name)) |global_info| {
        // Use the existing global slot
        try e.value_slots.put(a.result.id, global_info.slot | 0x8000);
    } else if (e.locals.get(a.name)) |local_info| {
        // This is a parameter - use the existing parameter slot
        try e.value_slots.put(a.result.id, local_info.slot);
    } else {
        // Check if this is a struct type - if so, allocate slots for all fields
        if (a.ty == .@"struct") {
            const struct_type = a.ty.@"struct";
            const base_slot = e.local_count;
            // Register the struct base slot
            try e.locals.put(a.name, .{
                .slot = base_slot,
                .is_global = false,
            });
            try e.value_slots.put(a.result.id, base_slot);
            // Allocate a slot for each field and register with qualified name
            for (struct_type.fields) |field| {
                const qualified_name = std.fmt.allocPrint(
                    e.allocator,
                    "{s}.{s}",
                    .{ a.name, field.name },
                ) catch return EmitError.OutOfMemory;
                try e.locals.put(qualified_name, .{
                    .slot = e.local_count,
                    .is_global = false,
                });
                e.local_count += 1;
            }
        } else {
            // Allocate a new local variable slot (primitive type)
            try e.locals.put(a.name, .{
                .slot = e.local_count,
                .is_global = false,
            });
            try e.value_slots.put(a.result.id, e.local_count);
            e.local_count += 1;
        }
    }
}

/// Emit load instruction
pub fn emitLoad(e: *BytecodeEmitter, l: ir.Instruction.Load) EmitError!void {
    if (e.value_slots.get(l.ptr.id)) |slot_info| {
        try e.value_slots.put(l.result.id, slot_info);
    } else if (e.value_consts.get(l.ptr.id)) |const_idx| {
        try e.value_consts.put(l.result.id, const_idx);
    } else if (e.last_result_value) |last_id| {
        if (last_id == l.ptr.id) {
            e.setLastResult(l.result.id, e.last_result_reg);
        } else {
            debug.print(.emit, "WARNING: .load ptr.id={d} not found, last_result={d}", .{ l.ptr.id, last_id });
        }
    } else {
        debug.print(.emit, "WARNING: .load ptr.id={d} not found", .{l.ptr.id});
    }
}

/// Emit store instruction
pub fn emitStore(e: *BytecodeEmitter, s: ir.Instruction.Store) EmitError!void {
    const src_reg = try e.getValueInReg(s.value, 0);
    if (e.value_slots.get(s.ptr.id)) |slot_info| {
        if (slot_info & 0x8000 != 0) {
            const slot = slot_info & 0x7FFF;
            try e.emitRegStoreGlobal(src_reg, slot);
        } else {
            if (slot_info < 256) {
                try e.emitRegStoreLocal(src_reg, @intCast(slot_info));
            } else {
                return EmitError.TooManyLocals;
            }
        }
    }
}

/// Emit field_ptr instruction
pub fn emitFieldPtr(e: *BytecodeEmitter, fp: ir.Instruction.FieldPtr) EmitError!void {
    if (e.value_slots.get(fp.struct_ptr.id)) |struct_slot| {
        const field_slot = (struct_slot & 0x7FFF) + @as(u16, @intCast(fp.field_index));
        const is_global = (struct_slot & 0x8000) != 0;
        try e.value_slots.put(fp.result.id, field_slot | (if (is_global) @as(u16, 0x8000) else 0));
    } else {
        debug.print(.emit, "WARNING: field_ptr struct_ptr.id={d} not in value_slots", .{fp.struct_ptr.id});
    }
}

// ============================================================================
// Constants
// ============================================================================

/// Emit iconst instruction
pub fn emitIconst(e: *BytecodeEmitter, c: Iconst) EmitError!void {
    const const_idx = try e.addConstant(.{ .integer = c.value });
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit f32const instruction
pub fn emitF32const(e: *BytecodeEmitter, c: F32const) EmitError!void {
    const int_val: i64 = @intFromFloat(@as(f64, c.value) * 100);
    const const_idx = try e.addConstant(.{ .integer = int_val });
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit f64const instruction
pub fn emitF64const(e: *BytecodeEmitter, c: F64const) EmitError!void {
    const int_val: i64 = @intFromFloat(c.value * 100);
    const const_idx = try e.addConstant(.{ .integer = int_val });
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit const_string instruction
pub fn emitConstString(e: *BytecodeEmitter, c: ConstString) EmitError!void {
    const const_idx = try e.addString(c.value);
    try e.value_consts.put(c.result.id, const_idx);
}

// ============================================================================
// Arithmetic Operations
// ============================================================================

/// Emit binary arithmetic instruction
pub fn emitBinaryArith(e: *BytecodeEmitter, op: Opcode, lhs: ir.Value, rhs: ir.Value, result: ir.Value) EmitError!void {
    const lhs_reg = try e.getValueInReg(lhs, 0);
    const rhs_reg = try e.getValueInReg(rhs, 1);
    const dest_reg: u4 = 2;
    try e.emitRegArith(op, dest_reg, lhs_reg, rhs_reg);
    e.setLastResult(result.id, dest_reg);
}

/// Emit ineg instruction
pub fn emitIneg(e: *BytecodeEmitter, n: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(n.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.neg, dest_reg, src_reg);
    e.setLastResult(n.result.id, dest_reg);
}

/// Emit icmp instruction
pub fn emitIcmp(e: *BytecodeEmitter, c: ir.Instruction.IcmpOp) EmitError!void {
    const lhs_reg = try e.getValueInReg(c.lhs, 0);
    const rhs_reg = try e.getValueInReg(c.rhs, 1);
    const dest_reg: u4 = 2;
    const opcode: Opcode = switch (c.cond) {
        .eq => if (c.lhs.ty == .string) .cmp_str_eq else .cmp_eq,
        .ne => .cmp_ne,
        .slt, .ult => .cmp_lt,
        .sle, .ule => .cmp_le,
        .sgt, .ugt => .cmp_gt,
        .sge, .uge => .cmp_ge,
    };
    try e.emitRegArith(opcode, dest_reg, lhs_reg, rhs_reg);
    e.setLastResult(c.result.id, dest_reg);
}

/// Emit logical not instruction
pub fn emitLogNot(e: *BytecodeEmitter, l: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(l.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.log_not, dest_reg, src_reg);
    e.setLastResult(l.result.id, dest_reg);
}

// ============================================================================
// String Operations
// ============================================================================

/// Emit str_concat instruction
pub fn emitStrConcat(e: *BytecodeEmitter, s: ir.Instruction.BinaryOp) EmitError!void {
    debug.print(.emit, "str_concat: last_result_value={?d}, last_result_reg={d}, rhs.id={d}", .{
        e.last_result_value,
        e.last_result_reg,
        s.rhs.id,
    });
    var rs2: u4 = undefined;
    if (e.last_result_value) |last_id| {
        if (last_id == s.rhs.id and e.last_result_reg == 0) {
            try e.emitOpcode(.mov);
            try e.emitU8((1 << 4) | 0);
            try e.emitU8(0);
            rs2 = 1;
            e.last_result_value = null;
        } else {
            rs2 = try e.getValueInReg(s.rhs, 1);
        }
    } else {
        rs2 = try e.getValueInReg(s.rhs, 1);
    }
    const rs1 = try e.getValueInReg(s.lhs, 0);
    const rd: u4 = 2;
    try e.emitOpcode(.str_concat);
    try e.emitU8((@as(u8, rd) << 4) | rs1);
    try e.emitU8(@as(u8, rs2) << 4);
    e.setLastResult(s.result.id, rd);
}

/// Emit str_slice instruction
pub fn emitStrSlice(e: *BytecodeEmitter, s: ir.Instruction.StrSlice) EmitError!void {
    const src_reg = try e.getValueInReg(s.source, 0);
    const start_reg = try e.getValueInReg(s.start, 1);
    const len_reg = try e.getValueInReg(s.length_or_end, 2);
    const dest_reg: u4 = 3;
    try e.emitOpcode(.str_slice);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8((@as(u8, start_reg) << 4) | len_reg);
    try e.emitU8(if (s.is_length) 1 else 0);
    e.setLastResult(s.result.id, dest_reg);
}

/// Emit str_slice_store instruction
pub fn emitStrSliceStore(e: *BytecodeEmitter, s: ir.Instruction.StrSliceStore) EmitError!void {
    const target_reg = try e.getValueInReg(s.target, 0);
    const start_reg = try e.getValueInReg(s.start, 1);
    const len_reg = try e.getValueInReg(s.length_or_end, 2);
    const value_reg = try e.getValueInReg(s.value, 3);

    try e.emitOpcode(.str_slice_store);
    try e.emitU8((@as(u8, target_reg) << 4) | start_reg);
    try e.emitU8((@as(u8, len_reg) << 4) | value_reg);
    try e.emitU8(if (s.is_length) 1 else 0);

    if (e.value_slots.get(s.target_ptr.id)) |slot_info| {
        const is_global = slot_info & 0x8000 != 0;
        const slot = slot_info & 0x7FFF;
        if (is_global) {
            try e.emitRegStoreGlobal(target_reg, slot);
        } else {
            if (slot < 256) {
                try e.emitRegStoreLocal(target_reg, @intCast(slot));
            }
        }
    } else {
        debug.print(.emit, "WARNING: str_substr_store target_ptr id={d} not found in value_slots", .{s.target_ptr.id});
    }
}

// ============================================================================
// Control Flow
// ============================================================================

/// Emit jump instruction
pub fn emitJump(e: *BytecodeEmitter, b: ir.Instruction.Branch) EmitError!void {
    try e.emitRegJmp(b.target);
}

/// Emit brif instruction
pub fn emitBrif(e: *BytecodeEmitter, c: ir.Instruction.CondBranch) EmitError!void {
    const cond_reg = try e.getValueInReg(c.condition, 0);
    try e.emitRegCondJmp(.jz, cond_reg, c.else_block);
    try e.emitRegJmp(c.then_block);
}

/// Emit return instruction
pub fn emitReturn(e: *BytecodeEmitter, r: ?ir.Value) EmitError!void {
    if (r) |val| {
        const src_reg = try e.getValueInReg(val, 0);
        try e.emitRegRet(src_reg);
    } else {
        try e.emitRegRet(null);
    }
}

// ============================================================================
// Function Calls
// ============================================================================

/// Emit call instruction - handles all call types
pub fn emitCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Check for built-in method calls
    if (std.mem.eql(u8, c.callee, "Console.WriteLine")) {
        try emitConsoleCall(e, c, .console_writeln);
        return;
    } else if (std.mem.eql(u8, c.callee, "Console.Write")) {
        try emitConsoleCall(e, c, .console_write);
        return;
    } else if (std.mem.eql(u8, c.callee, "console.log")) {
        try emitConsoleCall(e, c, .console_log);
        return;
    }

    // Core Cot I/O functions: print(), println()
    if (io_functions.get(c.callee)) |opcode| {
        if (c.args.len > 0) {
            const arg = c.args[0];
            if (e.reg_alloc.getRegister(arg.id)) |arg_reg| {
                if (arg_reg != 0) {
                    try e.emitRegMov(0, arg_reg);
                }
            } else {
                try e.emitValueToReg(arg, 0);
            }
        }
        try e.emitOpcode(opcode);
        const argc: u8 = @intCast(c.args.len);
        try e.emitU8((0 << 4) | (argc & 0xF));
        try e.emitU8(0);
        return;
    }

    // Performance-critical functions with dedicated opcodes
    if (opcode_builtins.get(c.callee)) |builtin| {
        for (c.args, 0..) |arg, i| {
            if (i < 8) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
        try e.emitOpcode(builtin.opcode);
        return;
    }

    // Native functions (via xcall)
    if (native_functions.has(c.callee)) {
        try emitNativeCall(e, c);
        return;
    }

    // Regular function call (user-defined)
    if (e.function_indices.get(c.callee)) |routine_idx| {
        try emitUserCall(e, c, routine_idx);
    } else {
        // Dynamic call (function not found at compile time)
        try emitDynamicCall(e, c);
    }
}

fn emitConsoleCall(e: *BytecodeEmitter, c: ir.Instruction.Call, opcode: Opcode) EmitError!void {
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    try e.emitOpcode(opcode);
    const argc: u8 = @intCast(c.args.len);
    try e.emitU8((0 << 4) | (argc & 0xF));
    try e.emitU8(0);
}

fn emitNativeCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Two-pass: first emit last_result args, then others
    if (e.last_result_value) |last_id| {
        for (c.args, 0..) |arg, i| {
            if (i < 8 and arg.id == last_id) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
    }
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            if (e.last_result_value) |last_id| {
                if (arg.id == last_id) continue;
            }
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    const name_idx = try e.addIdentifier(c.callee);
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(@intCast(c.args.len << 4));
    try e.emitU16(name_idx);
    if (c.result) |result| {
        e.setLastResult(result.id, 0);
    }
}

fn emitUserCall(e: *BytecodeEmitter, c: ir.Instruction.Call, routine_idx: u16) EmitError!void {
    // Two-pass argument loading
    if (e.last_result_value) |last_id| {
        for (c.args, 0..) |arg, i| {
            if (i < 8 and arg.id == last_id) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
    }
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            if (e.last_result_value) |last_id| {
                if (arg.id == last_id) continue;
            }
            try e.emitValueToReg(arg, @intCast(i));
        }
    }

    try e.emitOpcode(.call);
    const argc: u8 = @intCast(c.args.len);
    try e.emitU8(argc << 4);
    try e.emitU16(routine_idx);

    // Emit store-back for ref parameters
    if (e.ir_module) |ir_mod| {
        if (routine_idx < ir_mod.functions.items.len) {
            const callee_func = ir_mod.functions.items[routine_idx];
            for (callee_func.signature.params, 0..) |param, i| {
                if (param.is_ref and i < c.args.len) {
                    if (e.value_slots.get(c.args[i].id)) |slot| {
                        if (slot <= 255) {
                            try e.emitOpcode(.store_local);
                            try e.emitU8(@intCast(i));
                            try e.emitU8(@intCast(slot));
                        } else {
                            try e.emitOpcode(.store_local16);
                            try e.emitU8(@intCast(i));
                            try e.emitU16(slot);
                        }
                    }
                }
            }
        }
    }

    if (c.result) |result| {
        e.setLastResult(result.id, 0);
    }
}

fn emitDynamicCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Two-pass argument loading
    if (e.last_result_value) |last_id| {
        for (c.args, 0..) |arg, i| {
            if (i < 8 and arg.id == last_id) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
    }
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            if (e.last_result_value) |last_id| {
                if (arg.id == last_id) continue;
            }
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    const name_idx = try e.addIdentifier(c.callee);
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(@intCast(c.args.len << 4));
    try e.emitU16(name_idx);
    if (c.result) |result| {
        e.setLastResult(result.id, 0);
    }
}

// ============================================================================
// I/O Operations
// ============================================================================

/// Emit io_open instruction
pub fn emitIoOpen(e: *BytecodeEmitter, io: ir.Instruction.IoOpen) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    try e.emitValueToReg(io.filename, 1);
    const name_idx = try e.addIdentifier("open");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(2 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_close instruction
pub fn emitIoClose(e: *BytecodeEmitter, io: IoClose) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    const name_idx = try e.addIdentifier("close");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(1 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_delete instruction
pub fn emitIoDelete(e: *BytecodeEmitter, io: IoDelete) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    const name_idx = try e.addIdentifier("delete");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(1 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_read instruction
pub fn emitIoRead(e: *BytecodeEmitter, r: ir.Instruction.IoRead) EmitError!void {
    try e.emitValueToReg(r.channel, 0);
    if (r.key) |key| {
        // Keyed read: read(cursor_id, key_num, key_value)
        // Load key_num as constant to r1
        const key_num_idx = try e.addConstant(.{ .integer = r.qualifiers.key_index });
        try e.emitOpcode(.load_const);
        try e.emitU8(1); // r1 as destination
        try e.emitU16(key_num_idx);
        // Load key value to r2
        try e.emitValueToReg(key, 2);
        const name_idx = try e.addIdentifier("read");
        try e.emitOpcode(.call_dynamic);
        try e.emitU8(3 << 4);
        try e.emitU16(name_idx);
    } else {
        // Sequential read: reads(cursor_id)
        const name_idx = try e.addIdentifier("reads");
        try e.emitOpcode(.call_dynamic);
        try e.emitU8(1 << 4);
        try e.emitU16(name_idx);
    }
    // If this is a structure read, emit store_record_buf to distribute to locals
    if (r.struct_name) |struct_name| {
        try e.emitStoreRecordBuf(struct_name, r.base_name);
    }
}

/// Emit io_write instruction
pub fn emitIoWrite(e: *BytecodeEmitter, w: ir.Instruction.IoWrite) EmitError!void {
    try e.emitValueToReg(w.channel, 0);
    try e.emitValueToReg(w.buffer, 1);
    const name_idx = try e.addIdentifier(if (w.is_insert) "store" else "write");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(2 << 4);
    try e.emitU16(name_idx);
}

// ============================================================================
// Struct Buffer Operations
// ============================================================================

/// Emit load_struct_buf instruction
pub fn emitLoadStructBuf(e: *BytecodeEmitter, sb: LoadStructBuf) EmitError!void {
    var type_idx: u16 = 0;
    var found = false;
    var type_def: ?*const module.TypeDef = null;
    debug.print(.emit, "load_struct_buf: base='{s}' struct='{s}', types.len={d}", .{ sb.base_name, sb.struct_name, e.types.items.len });

    for (e.types.items, 0..) |*t, i| {
        if (t.name_index < e.constants.items.len) {
            const name_const = e.constants.items[t.name_index];
            const type_name = switch (name_const) {
                .string => |s| s,
                .identifier => |s| s,
                else => continue,
            };
            debug.print(.emit, "  type[{d}] name='{s}'", .{ i, type_name });
            if (std.mem.eql(u8, type_name, sb.struct_name)) {
                type_idx = @intCast(i);
                type_def = t;
                found = true;
                break;
            }
        }
    }
    if (!found) {
        debug.print(.emit, "WARNING: struct '{s}' not found in types!", .{sb.struct_name});
    }

    var local_base: u16 = 0;
    if (type_def) |td| {
        if (td.fields.len > 0) {
            const first_field = td.fields[0];
            if (first_field.name_index < e.constants.items.len) {
                const field_name_const = e.constants.items[first_field.name_index];
                const field_name = switch (field_name_const) {
                    .string => |s| s,
                    .identifier => |s| s,
                    else => "",
                };
                debug.print(.emit, "load_struct_buf: looking up first field '{s}'", .{field_name});
                if (e.locals.get(field_name)) |loc| {
                    local_base = loc.slot;
                    debug.print(.emit, "load_struct_buf: local_base={d} (plain name)", .{local_base});
                } else {
                    var qualified_buf: [256]u8 = undefined;
                    const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ sb.base_name, field_name }) catch "";
                    debug.print(.emit, "load_struct_buf: trying qualified name '{s}'", .{qualified_name});
                    if (e.locals.get(qualified_name)) |loc| {
                        local_base = loc.slot;
                        debug.print(.emit, "load_struct_buf: local_base={d} (qualified)", .{local_base});
                    } else {
                        debug.print(.emit, "WARNING: first field '{s}' not found in locals!", .{field_name});
                    }
                }
            }
        }
    }

    const rd = try e.getOrAllocReg(sb.result);
    debug.print(.emit, "load_struct_buf: allocated r{d} for result", .{rd});
    try e.emitOpcode(.load_record_buf);
    try e.emitU8(@as(u8, rd) << 4);
    try e.emitU16(type_idx);
    try e.emitU16(local_base);
    e.last_result_value = sb.result.id;
    e.last_result_reg = rd;
}

/// Emit store_struct_buf instruction
pub fn emitStoreStructBuf(e: *BytecodeEmitter, sb: StoreStructBuf) EmitError!void {
    try e.emitValueToReg(sb.value, 0);
    debug.print(.emit, "store_struct_buf: base='{s}' struct='{s}'", .{ sb.base_name, sb.struct_name });
    try e.emitStoreRecordBuf(sb.struct_name, sb.base_name);
}

// ============================================================================
// Debug
// ============================================================================

/// Emit debug_line instruction
pub fn emitDebugLine(e: *BytecodeEmitter, d: DebugLine) EmitError!void {
    try e.emitOpcode(.debug_line);
    try e.emitU8(0);
    try e.emitU16(@intCast(d.line));
}

// ============================================================================
// Exception Handling
// ============================================================================

/// Emit try_begin instruction
pub fn emitTryBegin(e: *BytecodeEmitter, t: ir.Instruction.TryBegin) EmitError!void {
    try e.emitOpcode(.set_error_handler);
    try e.addPendingJump(t.catch_block, false);
    try e.emitI16(0);
}

/// Emit try_end instruction
pub fn emitTryEnd(e: *BytecodeEmitter) EmitError!void {
    try e.emitOpcode(.clear_error_handler);
}

/// Emit catch_begin instruction
pub fn emitCatchBegin(e: *BytecodeEmitter) EmitError!void {
    try e.emitOpcode(.nop);
}

/// Emit throw instruction
pub fn emitThrow(e: *BytecodeEmitter, t: ir.Instruction.Throw) EmitError!void {
    try e.emitValueToReg(t.value, 0);
    try e.emitOpcode(.nop);
}

// ============================================================================
// Array Operations
// ============================================================================

/// Emit array_load instruction
pub fn emitArrayLoad(e: *BytecodeEmitter, al: ir.Instruction.ArrayOp) EmitError!void {
    const array_slot = if (e.value_slots.get(al.array_ptr.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    try e.emitValueToReg(al.index, 0);
    try e.emitOpcode(.array_load);
    try e.emitU8(0);
    try e.emitU16(array_slot);
    e.setLastResult(al.result.id, 0);
}

/// Emit array_store instruction
pub fn emitArrayStore(e: *BytecodeEmitter, as: ir.Instruction.ArrayStore) EmitError!void {
    const array_slot = if (e.value_slots.get(as.array_ptr.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    try e.emitValueToReg(as.index, 0);
    try e.emitValueToReg(as.value, 1);
    try e.emitOpcode(.array_store);
    try e.emitU8((0 << 4) | 1);
    try e.emitU16(array_slot);
}

// ============================================================================
// Switch/Branch Table
// ============================================================================

/// Emit br_table instruction
pub fn emitBrTable(e: *BytecodeEmitter, s: ir.Instruction.Switch) EmitError!void {
    const val_reg = try e.getValueInReg(s.value, 0);
    _ = val_reg;

    for (s.cases) |case| {
        const case_idx = try e.addConstant(.{ .integer = case.value });
        try e.emitOpcode(.load_const);
        try e.emitU8(1);
        try e.emitU16(case_idx);

        try e.emitOpcode(.cmp_eq);
        try e.emitU8((0 << 4) | 1);
        try e.emitU8(2);

        try e.emitRegCondJmp(.jnz, 2, case.target);
    }

    try e.emitRegJmp(s.default);
}

// ============================================================================
// Decimal Formatting
// ============================================================================

/// Emit format_decimal instruction
pub fn emitFormatDecimal(e: *BytecodeEmitter, fd: ir.Instruction.FormatDecimal) EmitError!void {
    const src_reg = try e.getValueInReg(fd.value, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.format_decimal);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(@intCast(fd.width));
    e.setLastResult(fd.result.id, dest_reg);
}

/// Emit parse_decimal instruction
pub fn emitParseDecimal(e: *BytecodeEmitter, pd: ir.Instruction.ParseDecimal) EmitError!void {
    const src_reg = try e.getValueInReg(pd.value, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.parse_decimal);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(0);
    e.setLastResult(pd.result.id, dest_reg);
}

// ============================================================================
// Map Operations
// ============================================================================

/// Emit map_new instruction
pub fn emitMapNew(e: *BytecodeEmitter, mn: ir.Instruction.MapNew) EmitError!void {
    const dest_reg: u4 = 0;
    try e.emitOpcode(.map_new);
    try e.emitU8((@as(u8, dest_reg) << 4) | (mn.flags & 0x0F));
    try e.emitU8(0);
    e.setLastResult(mn.result.id, dest_reg);
}

/// Emit map_set instruction
pub fn emitMapSet(e: *BytecodeEmitter, ms: ir.Instruction.MapSet) EmitError!void {
    const map_reg = try e.getValueInReg(ms.map, 0);
    const key_reg = try e.getValueInReg(ms.key, 1);
    const val_reg = try e.getValueInReg(ms.value, 2);
    try e.emitOpcode(.map_set);
    try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
    try e.emitU8((@as(u8, val_reg) << 4) | 0);
}

/// Emit map_get instruction
pub fn emitMapGet(e: *BytecodeEmitter, mg: ir.Instruction.MapGet) EmitError!void {
    const map_reg = try e.getValueInReg(mg.map, 0);
    const key_reg = try e.getValueInReg(mg.key, 1);
    const dest_reg: u4 = 2;
    try e.emitOpcode(.map_get);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8((@as(u8, key_reg) << 4) | 0);
    e.setLastResult(mg.result.id, dest_reg);
}

/// Emit map_delete instruction
pub fn emitMapDelete(e: *BytecodeEmitter, md: ir.Instruction.MapDelete) EmitError!void {
    const map_reg = try e.getValueInReg(md.map, 0);
    const key_reg = try e.getValueInReg(md.key, 1);
    try e.emitOpcode(.map_delete);
    try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
    try e.emitU8(0);
}

/// Emit map_has instruction
pub fn emitMapHas(e: *BytecodeEmitter, mh: ir.Instruction.MapHas) EmitError!void {
    const map_reg = try e.getValueInReg(mh.map, 0);
    const key_reg = try e.getValueInReg(mh.key, 1);
    const dest_reg: u4 = 2;
    try e.emitOpcode(.map_has);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8((@as(u8, key_reg) << 4) | 0);
    e.setLastResult(mh.result.id, dest_reg);
}

/// Emit map_len instruction
pub fn emitMapLen(e: *BytecodeEmitter, ml: ir.Instruction.MapLen) EmitError!void {
    const map_reg = try e.getValueInReg(ml.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_len);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(ml.result.id, dest_reg);
}

/// Emit map_clear instruction
pub fn emitMapClear(e: *BytecodeEmitter, mc: ir.Instruction.MapClear) EmitError!void {
    const map_reg = try e.getValueInReg(mc.map, 0);
    try e.emitOpcode(.map_clear);
    try e.emitU8((@as(u8, map_reg) << 4) | 0);
    try e.emitU8(0);
}

/// Emit map_keys instruction
pub fn emitMapKeys(e: *BytecodeEmitter, mk: ir.Instruction.MapKeys) EmitError!void {
    const map_reg = try e.getValueInReg(mk.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_keys);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(mk.result.id, dest_reg);
}

/// Emit map_values instruction
pub fn emitMapValues(e: *BytecodeEmitter, mv: ir.Instruction.MapValues) EmitError!void {
    const map_reg = try e.getValueInReg(mv.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_values);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(mv.result.id, dest_reg);
}
