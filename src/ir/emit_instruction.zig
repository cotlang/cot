//! Instruction Emission Handlers
//!
//! Converts IR instructions to bytecode.
//! These are free functions that take the BytecodeEmitter as the first parameter.
//!
//! This module handles emission of all IR instruction types to bytecode.

const std = @import("std");
const log = std.log.scoped(.@"bytecode-emit");
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
/// For DBL field views (overlays), uses str_slice_store to write through to the
/// underlying buffer at the field's byte offset. This implements DBL RECORD
/// semantics where field assignments modify the record buffer in place.
pub fn emitStore(e: *BytecodeEmitter, s: ir.Instruction.Store) EmitError!void {
    // Check if this is a field view (overlay) store that needs write-through
    if (e.field_view_info.get(s.ptr.id)) |fv_info| {
        // Field view store: use str_slice_store for write-through semantics
        // Load value FIRST (into r3) to avoid clobbering other registers
        // Then load buffer, offset, length into r0, r1, r2
        // Finally emit str_slice_store and store the result back

        debug.print(.emit, "store: field_view id={d} offset={d} len={d} -> str_slice_store", .{
            s.ptr.id, fv_info.byte_offset, fv_info.length,
        });

        // Load value into r3 FIRST (emitValueToReg may use r0-r2 as temps)
        try e.emitValueToReg(s.value, 3);

        // Load the base buffer into r0
        if (fv_info.is_global) {
            try e.emitOpcode(.load_global);
            try e.emitU8(0); // r0
            try e.emitU16(fv_info.base_slot);
        } else {
            try e.emitOpcode(.load_local);
            try e.emitU8(0 << 4); // r0
            try e.emitU8(@intCast(fv_info.base_slot));
        }

        // Load offset constant into r1
        const offset_idx = try e.addConstant(.{ .integer = fv_info.byte_offset });
        try e.emitOpcode(.load_const);
        try e.emitU8(1 << 4); // r1 in upper nibble
        try e.emitU16(offset_idx);

        // Load length constant into r2
        const len_idx = try e.addConstant(.{ .integer = fv_info.length });
        try e.emitOpcode(.load_const);
        try e.emitU8(2 << 4); // r2 in upper nibble
        try e.emitU16(len_idx);

        // Emit str_slice_store r0, r1, r2, r3 (target, start, len, val)
        try e.emitOpcode(.str_slice_store);
        try e.emitU8((0 << 4) | 1); // r0 (target), r1 (start)
        try e.emitU8((2 << 4) | 3); // r2 (len), r3 (val)
        try e.emitU8(1); // is_length = true

        // Store the modified buffer back to base slot
        if (fv_info.is_global) {
            try e.emitRegStoreGlobal(0, fv_info.base_slot);
        } else {
            try e.emitRegStoreLocal(0, @intCast(fv_info.base_slot));
        }
        return;
    }

    // Regular store
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

    // Allocate a fresh register for the result, spilling if necessary
    const dest_reg = try e.allocateWithSpill(result.id);

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

/// Emit round instruction (DBL ## operator - true rounding)
/// fn_round rd, rs, prec - Format: [rd:4|rs:4] [prec:8]
pub fn emitRound(e: *BytecodeEmitter, r: ir.Instruction.RoundOp) EmitError!void {
    const value_reg = try e.getValueInReg(r.value, 0);
    const dest_reg: u4 = 1;

    // Get precision value - must be a constant or loaded value
    const prec: u8 = if (e.value_consts.get(r.places.id)) |const_idx| blk: {
        // Load the constant value - for now assume it's a small integer
        _ = const_idx;
        break :blk 2; // Default precision
    } else if (e.last_result_value) |last_id| blk: {
        if (last_id == r.places.id) {
            // Places value is the last computed result - use as precision
            // For simplicity, default to 2 decimal places
            break :blk 2;
        }
        break :blk 2;
    } else 2;

    try e.emitOpcode(.fn_round);
    try e.emitU8((@as(u8, dest_reg) << 4) | value_reg);
    try e.emitU8(prec);
    e.setLastResult(r.result.id, dest_reg);
}

/// Emit trunc instruction (DBL # operator - truncating round)
/// fn_trunc rd, rs - Format: [rd:4|rs:4] [0]
pub fn emitTrunc(e: *BytecodeEmitter, t: ir.Instruction.RoundOp) EmitError!void {
    const value_reg = try e.getValueInReg(t.value, 0);
    const dest_reg: u4 = 1;

    // fn_trunc uses the places value as precision
    // For DBL compatibility, places specifies decimal positions to keep
    const prec: u8 = if (e.value_consts.get(t.places.id)) |const_idx| blk: {
        _ = const_idx;
        break :blk 0; // Truncate to integer by default
    } else 0;

    try e.emitOpcode(.fn_trunc);
    try e.emitU8((@as(u8, dest_reg) << 4) | value_reg);
    try e.emitU8(prec);
    e.setLastResult(t.result.id, dest_reg);
}

/// Check if a type is a string type (directly or via pointer/array)
fn isStringType(ty: ir.Type) bool {
    return switch (ty) {
        .string => true,
        .ptr => |pointee| pointee.* == .string,
        .array => |arr| arr.element.* == .u8, // []u8 arrays are strings
        else => false,
    };
}

/// Emit icmp instruction
pub fn emitIcmp(e: *BytecodeEmitter, c: ir.Instruction.IcmpOp) EmitError!void {
    const lhs_reg = try e.getValueInReg(c.lhs, 0);
    const rhs_reg = try e.getValueInReg(c.rhs, 1);

    // Allocate a fresh register for the result, spilling if necessary
    const dest_reg = try e.allocateWithSpill(c.result.id);

    // Check if type is string, or a pointer to string (for record fields)
    const is_string = isStringType(c.lhs.ty);
    const opcode: Opcode = switch (c.cond) {
        .eq => if (is_string) .cmp_str_eq else .cmp_eq,
        .ne => if (is_string) .cmp_str_ne else .cmp_ne,
        .slt, .ult => if (is_string) .cmp_str_lt else .cmp_lt,
        .sle, .ule => if (is_string) .cmp_str_le else .cmp_le,
        .sgt, .ugt => if (is_string) .cmp_str_gt else .cmp_gt,
        .sge, .uge => if (is_string) .cmp_str_ge else .cmp_ge,
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
    // Check for built-in method calls (Synergy.NET compatibility aliases)
    // Note: DBL parser lowercases identifiers, so we check lowercase
    if (std.mem.eql(u8, c.callee, "console.writeline")) {
        try emitConsoleCall(e, c, .println);
        return;
    } else if (std.mem.eql(u8, c.callee, "console.write")) {
        try emitConsoleCall(e, c, .print);
        return;
    }

    // Core Cot I/O functions: print(), println()
    // Load ALL arguments to registers (r0, r1, r2, ...)
    if (io_functions.get(c.callee)) |opcode| {
        for (c.args, 0..) |arg, i| {
            if (i < 8) {
                try e.emitValueToReg(arg, @intCast(i));
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

    // User-defined functions take precedence over native functions
    // This allows users to shadow builtin names like "log" with their own functions
    if (e.function_indices.get(c.callee)) |routine_idx| {
        try emitUserCall(e, c, routine_idx);
        return;
    }

    // Native functions (via xcall) - only if no user function with same name
    if (native_functions.has(c.callee)) {
        try emitNativeCall(e, c);
        return;
    }

    // Dynamic call (function not found at compile time)
    try emitDynamicCall(e, c);
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
/// Format: [rd:4|flags:4] [type_idx:16] [base:16]
/// flags: 0=locals, 1=globals
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

    var base: u16 = 0;
    var is_global: bool = false;
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

                // Try locals first
                if (e.locals.get(field_name)) |loc| {
                    base = loc.slot;
                    is_global = false;
                    debug.print(.emit, "load_struct_buf: base={d} (local)", .{base});
                } else if (e.globals.get(field_name)) |glob| {
                    base = glob.slot;
                    is_global = true;
                    debug.print(.emit, "load_struct_buf: base={d} (global)", .{base});
                } else {
                    // Try qualified names
                    var qualified_buf: [256]u8 = undefined;
                    const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ sb.base_name, field_name }) catch "";
                    debug.print(.emit, "load_struct_buf: trying qualified name '{s}'", .{qualified_name});
                    if (e.locals.get(qualified_name)) |loc| {
                        base = loc.slot;
                        is_global = false;
                        debug.print(.emit, "load_struct_buf: base={d} (local qualified)", .{base});
                    } else if (e.globals.get(qualified_name)) |glob| {
                        base = glob.slot;
                        is_global = true;
                        debug.print(.emit, "load_struct_buf: base={d} (global qualified)", .{base});
                    } else {
                        debug.print(.emit, "WARNING: first field '{s}' not found!", .{field_name});
                    }
                }
            }
        }
    }

    const rd = try e.getOrAllocReg(sb.result);
    const flags: u4 = if (is_global) 1 else 0;
    debug.print(.emit, "load_struct_buf: rd=r{d} flags={d} (is_global={any})", .{ rd, flags, is_global });
    try e.emitOpcode(.load_record_buf);
    try e.emitU8((@as(u8, rd) << 4) | flags);
    try e.emitU16(type_idx);
    try e.emitU16(base);
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

// ============================================================================
// Null/Optional Operations
// ============================================================================

/// Emit is_null instruction: rd = (rs == null)
pub fn emitIsNull(e: *BytecodeEmitter, n: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(n.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.is_null);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(0);
    e.setLastResult(n.result.id, dest_reg);
}

/// Emit select instruction: rd = cond ? rtrue : rfalse
pub fn emitSelect(e: *BytecodeEmitter, s: ir.Instruction.Select) EmitError!void {
    const cond_reg = try e.getValueInReg(s.condition, 0);
    const true_reg = try e.getValueInReg(s.true_val, 1);
    const false_reg = try e.getValueInReg(s.false_val, 2);
    const dest_reg: u4 = 3;
    try e.emitOpcode(.select);
    try e.emitU8((@as(u8, dest_reg) << 4) | cond_reg);
    try e.emitU8((@as(u8, true_reg) << 4) | false_reg);
    e.setLastResult(s.result.id, dest_reg);
}

/// Emit ptr_offset instruction: rd = base + offset (byte-level pointer arithmetic)
/// For DBL field views, this records FieldViewInfo for write-through semantics.
/// When a field view is assigned, emitStore uses str_slice_store to write
/// into the base buffer at the correct offset.
pub fn emitPtrOffset(e: *BytecodeEmitter, p: ir.Instruction.PtrOffset) EmitError!void {
    // Get base slot and global flag for field view tracking
    const base_slot_info = e.value_slots.get(p.base_ptr.id);

    // Determine the field length from the result type
    // Field views have ptr type pointing to the field type (e.g., *[6]u8)
    const field_length: u16 = blk: {
        if (p.result.ty == .ptr) {
            break :blk @intCast(p.result.ty.ptr.*.sizeInBytes());
        } else {
            break :blk @intCast(p.result.ty.sizeInBytes());
        }
    };

    // Record field view info for ALL ptr_offset results (including offset 0)
    // This enables write-through semantics for DBL record field overlays
    if (base_slot_info) |slot_info| {
        const base_slot = slot_info & 0x7FFF;
        const is_global = (slot_info & 0x8000) != 0;

        try e.field_view_info.put(p.result.id, .{
            .base_slot = base_slot,
            .byte_offset = @intCast(p.offset),
            .length = field_length,
            .is_global = is_global,
        });

        debug.print(.emit, "ptr_offset: id={d} -> base_slot={d} offset={d} len={d}", .{
            p.result.id, base_slot, p.offset, field_length,
        });
    }

    // For field views with offset 0, we just need to create an alias in value_slots
    // The new pointer points to the same memory location as the base pointer
    if (p.offset == 0) {
        if (base_slot_info) |slot_info| {
            try e.value_slots.put(p.result.id, slot_info);
            return;
        }
    }

    const base_reg = try e.getValueInReg(p.base_ptr, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.ptr_offset);
    try e.emitU8((@as(u8, dest_reg) << 4) | base_reg);
    // Emit offset as signed 16-bit
    const offset_u16: u16 = @bitCast(@as(i16, @intCast(p.offset)));
    try e.emitU8(@truncate(offset_u16));
    try e.emitU8(@truncate(offset_u16 >> 8));
    e.setLastResult(p.result.id, dest_reg);

    // Also register in value_slots for subsequent loads
    if (base_slot_info) |slot_info| {
        try e.value_slots.put(p.result.id, slot_info);
    }
}
