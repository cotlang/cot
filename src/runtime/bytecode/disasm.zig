//! Cot Bytecode Disassembler
//!
//! Provides human-readable output of register-based bytecode for debugging.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const module = @import("module.zig");

const Opcode = opcodes.Opcode;

/// Disassembler for bytecode modules
pub const Disassembler = struct {
    mod: *const module.Module,
    code: []const u8,
    offset: usize,
    writer: std.ArrayList(u8).Writer,

    const Self = @This();

    pub fn init(mod: *const module.Module, writer: std.ArrayList(u8).Writer) Self {
        return .{
            .mod = mod,
            .code = mod.code,
            .offset = 0,
            .writer = writer,
        };
    }

    /// Disassemble entire module
    pub fn disassembleModule(self: *Self) !void {
        try self.writer.print("; Cot Bytecode Disassembly (Register-Based)\n", .{});
        try self.writer.print("; Version: {}.{}\n", .{
            self.mod.header.version_major,
            self.mod.header.version_minor,
        });
        try self.writer.print("; Entry Point: 0x{X:0>4}\n\n", .{self.mod.header.entry_point});

        // Constants section
        if (self.mod.constants.len > 0) {
            try self.writer.print("; === Constants ({}) ===\n", .{self.mod.constants.len});
            for (self.mod.constants, 0..) |c, i| {
                try self.writer.print(";   [{:0>4}] ", .{i});
                try self.printConstant(c);
                try self.writer.print("\n", .{});
            }
            try self.writer.print("\n", .{});
        }

        // Types section
        if (self.mod.types.len > 0) {
            try self.writer.print("; === Types ({}) ===\n", .{self.mod.types.len});
            for (self.mod.types) |t| {
                try self.printTypeDef(t);
            }
            try self.writer.print("\n", .{});
        }

        // Routines section
        if (self.mod.routines.len > 0) {
            try self.writer.print("; === Routines ({}) ===\n", .{self.mod.routines.len});
            for (self.mod.routines, 0..) |r, i| {
                try self.printRoutineHeader(r, @intCast(i));
            }
            try self.writer.print("\n", .{});
        }

        // Code section
        try self.writer.print("; === Code ({} bytes) ===\n", .{self.code.len});
        try self.disassembleCode(0, self.code.len);
    }

    /// Disassemble a routine
    pub fn disassembleRoutine(self: *Self, routine_idx: u16) !void {
        if (routine_idx >= self.mod.routines.len) return;
        const routine = self.mod.routines[routine_idx];

        try self.printRoutineHeader(routine, routine_idx);
        try self.disassembleCode(routine.code_offset, routine.code_length);
    }

    /// Disassemble code range
    fn disassembleCode(self: *Self, start: usize, length: usize) !void {
        self.offset = start;
        const end = start + length;

        while (self.offset < end) {
            try self.disassembleInstruction();
        }
    }

    /// Disassemble single instruction
    pub fn disassembleInstruction(self: *Self) !void {
        const addr = self.offset;
        const op_byte = self.code[self.offset];
        self.offset += 1;

        const op = std.meta.intToEnum(Opcode, op_byte) catch {
            try self.writer.print("  {X:0>4}:  {X:0>2}              ; <unknown>\n", .{ addr, op_byte });
            return;
        };

        // Print address and opcode
        try self.writer.print("  {X:0>4}:  {X:0>2}", .{ addr, op_byte });

        // Read and print operands
        const operand_size = op.operandSize();
        var operand_bytes: [8]u8 = undefined;
        for (0..operand_size) |i| {
            if (self.offset + i < self.code.len) {
                operand_bytes[i] = self.code[self.offset + i];
                try self.writer.print(" {X:0>2}", .{operand_bytes[i]});
            }
        }
        self.offset += operand_size;

        // Pad to align mnemonic
        const total_bytes = 1 + operand_size;
        const padding: i32 = 14 - @as(i32, @intCast(total_bytes * 3));
        if (padding > 0) {
            for (0..@intCast(padding)) |_| {
                try self.writer.print(" ", .{});
            }
        }

        // Print mnemonic
        try self.writer.print(" {s}", .{op.name()});

        // Print operand interpretation
        try self.printOperandInfo(op, operand_bytes[0..operand_size]);

        try self.writer.print("\n", .{});
    }

    fn printOperandInfo(self: *Self, op: Opcode, operands: []const u8) !void {
        switch (op) {
            // ============================================
            // Core Operations (0x00-0x0F)
            // ============================================
            .nop, .halt => {},

            // ============================================
            // Register Moves & Immediates (0x10-0x1F)
            // ============================================

            // mov rd, rs - [rd:4|rs:4] [0]
            .mov => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // movi rd, imm8 - [rd:4|0] [imm8]
            .movi => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const imm8: i8 = @bitCast(operands[1]);
                try self.writer.print(" r{}, {}", .{ rd, imm8 });
            },

            // movi16 rd, imm16 - [rd:4|0] [imm16:16]
            .movi16 => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const imm16: i16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, {}", .{ rd, imm16 });
            },

            // movi32 rd, imm32 - [rd:4|0] [imm32:32]
            .movi32 => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const imm32: i32 = @bitCast(operands[1..5].*);
                try self.writer.print(" r{}, {}", .{ rd, imm32 });
            },

            // load_const rd, idx - [rd:4|0] [idx:16]
            .load_const => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, #{}", .{ rd, idx });
                if (idx < self.mod.constants.len) {
                    try self.writer.print("  ; ", .{});
                    try self.printConstant(self.mod.constants[idx]);
                }
            },

            // load_null rd, load_true rd, load_false rd - [rd:4|0] [0]
            .load_null, .load_true, .load_false => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // ============================================
            // Local & Global Variables (0x20-0x2F)
            // ============================================

            // load_local rd, slot - [rd:4|0] [slot:8]
            // store_local rs, slot - [rs:4|0] [slot:8]
            .load_local, .store_local => {
                const reg: u4 = @truncate(operands[0] >> 4);
                const slot = operands[1];
                try self.writer.print(" r{}, ${}", .{ reg, slot });
            },

            // load_local16 rd, slot - [rd:4|0] [slot:16]
            .load_local16, .store_local16 => {
                const reg: u4 = @truncate(operands[0] >> 4);
                const slot: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, ${}", .{ reg, slot });
            },

            // load_global rd, idx - [rd:4|0] [idx:16]
            // store_global rs, idx - [rs:4|0] [idx:16]
            .load_global, .store_global => {
                const reg: u4 = @truncate(operands[0] >> 4);
                const idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, @{}", .{ reg, idx });
            },

            // ============================================
            // Arithmetic (0x30-0x3F)
            // ============================================

            // 3-operand: [rd:4|rs1:4] [rs2:4|0]
            .add, .sub, .mul, .div, .mod => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // neg rd, rs - [rd:4|rs:4] [0]
            .neg => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // addi/subi/muli rd, rs, imm8 - [rd:4|rs:4] [imm8]
            .addi, .subi, .muli => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const imm8: i8 = @bitCast(operands[1]);
                try self.writer.print(" r{}, r{}, {}", .{ rd, rs, imm8 });
            },

            // incr/decr rd - [rd:4|0] [0]
            .incr, .decr => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // Decimal ops: [rd:4|rs1:4] [rs2:4|prec:4]
            .add_dec, .sub_dec, .mul_dec, .div_dec => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                const prec: u4 = @truncate(operands[1] & 0xF);
                try self.writer.print(" r{}, r{}, r{}, prec={}", .{ rd, rs1, rs2, prec });
            },

            // ============================================
            // Comparison (0x40-0x4F)
            // ============================================

            // cmp_* rd, rs1, rs2 - [rd:4|rs1:4] [rs2:4|0]
            .cmp_eq, .cmp_ne, .cmp_lt, .cmp_le, .cmp_gt, .cmp_ge, .cmp_str_eq, .cmp_str_lt, .cmp_str_ne, .cmp_str_le, .cmp_str_gt, .cmp_str_ge => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // ============================================
            // Logical & Bitwise (0x50-0x5F)
            // ============================================

            // Binary logical/bitwise: [rd:4|rs1:4] [rs2:4|0]
            .log_and, .log_or, .bit_and, .bit_or, .bit_xor, .shl, .shr => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // Unary logical/bitwise: [rd:4|rs:4] [0]
            .log_not, .bit_not, .is_null => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // is_type rd, rs, type_tag - [rd:4|rs:4] [type_tag:8]
            .is_type => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const type_tag = operands[1];
                const type_name = switch (type_tag) {
                    0 => "null",
                    1 => "bool",
                    2 => "int",
                    3 => "float",
                    4 => "string",
                    else => "unknown",
                };
                try self.writer.print(" r{}, r{}, {s}", .{ rd, rs, type_name });
            },

            // select rd, rcond, rtrue, rfalse - [rd:4|rcond:4] [rtrue:4|rfalse:4]
            .select => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rcond: u4 = @truncate(operands[0] & 0xF);
                const rtrue: u4 = @truncate(operands[1] >> 4);
                const rfalse: u4 = @truncate(operands[1] & 0xF);
                try self.writer.print(" r{}, r{}, r{}, r{}", .{ rd, rcond, rtrue, rfalse });
            },

            // ptr_offset rd, rs, offset - [rd:4|rs:4] [offset:16]
            .ptr_offset => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const offset: i16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, r{}, {}", .{ rd, rs, offset });
            },

            // ============================================
            // Control Flow (0x60-0x6F)
            // ============================================

            // jmp offset - [0] [offset:16]
            .jmp => {
                const offset_val: i16 = @bitCast(operands[1..3].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                if (target >= 0) {
                    try self.writer.print(" -> {X:0>4}", .{@as(u32, @intCast(target))});
                } else {
                    try self.writer.print(" -> (invalid)", .{});
                }
            },

            // jmp32 offset - [0] [offset:32]
            .jmp32 => {
                const offset_val: i32 = @bitCast(operands[1..5].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                if (target >= 0) {
                    try self.writer.print(" -> {X:0>4}", .{@as(u32, @intCast(target))});
                } else {
                    try self.writer.print(" -> (invalid)", .{});
                }
            },

            // jz/jnz rs, offset - [rs:4|0] [offset:16]
            .jz, .jnz => {
                const rs: u4 = @truncate(operands[0] >> 4);
                const offset_val: i16 = @bitCast(operands[1..3].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                if (target >= 0) {
                    try self.writer.print(" r{} -> {X:0>4}", .{ rs, @as(u32, @intCast(target)) });
                } else {
                    try self.writer.print(" r{} -> (invalid)", .{rs});
                }
            },

            // jeq/jne/jlt/jge rs1, rs2, offset - [rs1:4|rs2:4] [offset:16]
            .jeq, .jne, .jlt, .jge => {
                const rs1: u4 = @truncate(operands[0] >> 4);
                const rs2: u4 = @truncate(operands[0] & 0xF);
                const offset_val: i16 = @bitCast(operands[1..3].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                if (target >= 0) {
                    try self.writer.print(" r{}, r{} -> {X:0>4}", .{ rs1, rs2, @as(u32, @intCast(target)) });
                } else {
                    try self.writer.print(" r{}, r{} -> (invalid)", .{ rs1, rs2 });
                }
            },

            .loop_start, .loop_end, .clear_error_handler => {},

            // throw rs - throw exception with value in rs
            .throw => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // set_error_handler offset - [0] [offset:16]
            .set_error_handler => {
                const offset_val: i16 = @bitCast(operands[1..3].*);
                const addr_after: i32 = @intCast(self.offset);
                const target = addr_after + offset_val;
                if (target >= 0) {
                    try self.writer.print(" -> {X:0>4}", .{@as(u32, @intCast(target))});
                } else {
                    try self.writer.print(" -> (invalid)", .{});
                }
            },

            // ============================================
            // Function Calls (0x70-0x7F)
            // ============================================

            // call routine_idx, argc - [argc:4|0] [routine_idx:16]
            .call, .call_external, .call_native => {
                const argc: u4 = @truncate(operands[0] >> 4);
                const idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" routine#{}, argc={}", .{ idx, argc });
            },

            // call_indirect rs, argc - [argc:4|rs:4] [0]
            .call_indirect => {
                const argc: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, argc={}", .{ rs, argc });
            },

            // call_dynamic name_idx, argc - [argc:4|0] [name_idx:16]
            .call_dynamic => {
                const argc: u4 = @truncate(operands[0] >> 4);
                const idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" #{}, argc={}", .{ idx, argc });
                if (idx < self.mod.constants.len) {
                    try self.writer.print("  ; ", .{});
                    try self.printConstant(self.mod.constants[idx]);
                }
            },

            // ret - [0] [0]
            .ret => {},

            // ret_val rs - [rs:4|0] [0]
            .ret_val => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // push_arg slot - [0] [slot:16]
            .push_arg => {
                const slot: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" slot#{}", .{slot});
            },

            // push_arg_reg rs - [rs:4|0] [0]
            .push_arg_reg => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // pop_arg slot - [0] [slot:16]
            .pop_arg => {
                const slot: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" slot#{}", .{slot});
            },

            // ============================================
            // Record/Field Operations (0x80-0x8F)
            // ============================================

            // new_record rd, type_idx - [rd:4|0] [type_idx:16]
            .new_record => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const type_idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, type#{}", .{ rd, type_idx });
            },

            // load_record_buf rd, type_idx, local_base - [rd:4|0] [type_idx:16] [local_base:16]
            .load_record_buf => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const type_idx: u16 = @bitCast(operands[1..3].*);
                const local_base: u16 = @bitCast(operands[3..5].*);
                try self.writer.print(" r{}, type#{}, local#{}", .{ rd, type_idx, local_base });
            },

            // store_record_buf rs, type_idx, local_base - [rs:4|0] [type_idx:16] [local_base:16]
            .store_record_buf => {
                const rs: u4 = @truncate(operands[0] >> 4);
                const type_idx: u16 = @bitCast(operands[1..3].*);
                const local_base: u16 = @bitCast(operands[3..5].*);
                try self.writer.print(" r{}, type#{}, local#{}", .{ rs, type_idx, local_base });
            },

            // free_record rs - [rs:4|0] [0]
            .free_record, .clear_record => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // alloc_buffer slot, size - [slot:8] [size:16]
            .alloc_buffer => {
                const slot = operands[0];
                const size: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" slot#{}, size={}", .{ slot, size });
            },

            // load_field rd, rs, field_idx - [rd:4|rs:4] [field_idx:16]
            .load_field, .store_field => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const field_idx: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, r{}, field#{}", .{ rd, rs, field_idx });
            },

            // load_field_fast rd, rs, offset - [rd:4|rs:4] [offset:8]
            .load_field_fast, .store_field_fast => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const offset = operands[1];
                try self.writer.print(" r{}, r{}, +{}", .{ rd, rs, offset });
            },

            // ============================================
            // String Operations (0x90-0x9F)
            // ============================================

            // str_concat rd, rs1, rs2 - [rd:4|rs1:4] [rs2:4|0]
            .str_concat, .str_find => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // str_len rd, rs - [rd:4|rs:4] [0]
            .str_len, .str_trim, .str_upper, .str_lower => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // str_index rd, rs, idx_reg - [rd:4|rs:4] [idx_reg:4|0]
            .str_index => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const idx_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs, idx_reg });
            },

            // str_slice rd, rs, start_reg, len_reg - [rd:4|rs:4] [start_reg:4|len_reg:4]
            .str_slice => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const start_reg: u4 = @truncate(operands[1] >> 4);
                const len_reg: u4 = @truncate(operands[1] & 0xF);
                try self.writer.print(" r{}, r{}[r{}:r{}]", .{ rd, rs, start_reg, len_reg });
            },

            // str_slice_store rd, start_reg, len_reg, val_reg - [rd:4|start_reg:4] [len_reg:4|val_reg:4]
            .str_slice_store => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const start_reg: u4 = @truncate(operands[0] & 0xF);
                const len_reg: u4 = @truncate(operands[1] >> 4);
                const val_reg: u4 = @truncate(operands[1] & 0xF);
                try self.writer.print(" r{}[r{}:r{}], r{}", .{ rd, start_reg, len_reg, val_reg });
            },

            // str_replace rd, rs, old_reg, new_reg - [rd:4|rs:4] [old_reg:4|new_reg:4]
            .str_replace => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const old_reg: u4 = @truncate(operands[1] >> 4);
                const new_reg: u4 = @truncate(operands[1] & 0xF);
                try self.writer.print(" r{}, r{}, r{}, r{}", .{ rd, rs, old_reg, new_reg });
            },

            // str_setchar rd, idx_reg, char_reg - [rd:4|idx_reg:4] [char_reg:4|0]
            .str_setchar => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const idx_reg: u4 = @truncate(operands[0] & 0xF);
                const char_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}[r{}], r{}", .{ rd, idx_reg, char_reg });
            },

            // ============================================
            // Type Conversion (0xA0-0xAF)
            // ============================================

            // to_* rd, rs - [rd:4|rs:4] [0 or prec:8]
            .to_int, .to_str, .to_bool, .to_char => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // to_dec rd, rs, prec - [rd:4|rs:4] [prec:8]
            .to_dec => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const prec = operands[1];
                try self.writer.print(" r{}, r{}, prec={}", .{ rd, rs, prec });
            },

            // to_fixed_string rd, rs, size - [rd:4|rs:4] [size:16]
            .to_fixed_string => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const size: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" r{}, r{}, size={}", .{ rd, rs, size });
            },

            // format_decimal rd, rs, width - [rd:4|rs:4] [width:8]
            .format_decimal => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const width = operands[1];
                try self.writer.print(" r{}, r{}, width={}", .{ rd, rs, width });
            },

            // parse_decimal rd, rs - [rd:4|rs:4] [0]
            .parse_decimal => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // ============================================
            // Array Operations (0xB0-0xBF)
            // ============================================

            // array_load idx_reg, slot - [idx_reg:8] [slot:16]
            .array_load => {
                const idx_reg = operands[0];
                const slot = std.mem.readInt(u16, operands[1..3], .little);
                try self.writer.print(" r0, ${}[r{}]", .{ slot, idx_reg });
            },

            // array_store idx_reg, val_reg, slot - [idx_reg:4|val_reg:4] [slot:16]
            .array_store => {
                const idx_reg: u4 = @truncate(operands[0] >> 4);
                const val_reg: u4 = @truncate(operands[0] & 0xF);
                const slot = std.mem.readInt(u16, operands[1..3], .little);
                try self.writer.print(" ${}[r{}], r{}", .{ slot, idx_reg, val_reg });
            },

            // array_len rd, slot - [rd:8] [slot:16]
            .array_len => {
                const rd = operands[0];
                const slot = std.mem.readInt(u16, operands[1..3], .little);
                try self.writer.print(" r{}, len(${})", .{ rd, slot });
            },

            // array_slice - [rd:4|inclusive:1|0:3] [start_reg:4|end_reg:4] [slot_lo:8] [slot_hi:8]
            .array_slice => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const inclusive = (operands[0] & 0x08) != 0;
                const start_reg: u4 = @truncate(operands[1] >> 4);
                const end_reg: u4 = @truncate(operands[1] & 0xF);
                const slot = std.mem.readInt(u16, operands[2..4], .little);
                const range_op = if (inclusive) "..=" else "..";
                try self.writer.print(" r{}, ${}[r{}{s}r{}]", .{ rd, slot, start_reg, range_op, end_reg });
            },

            // array_load_opt idx_reg, slot, len - [idx_reg:8] [slot:16] [len:16] - optional load, returns null on OOB
            .array_load_opt => {
                const idx_reg = operands[0];
                const slot = std.mem.readInt(u16, operands[1..3], .little);
                const len = std.mem.readInt(u16, operands[3..5], .little);
                try self.writer.print(" r0, ${}?[r{}] (len={})", .{ slot, idx_reg, len });
            },

            // ============================================
            // Built-in Functions (0xC0-0xCF)
            // ============================================

            // Unary math: [rd:4|rs:4] [0]
            .fn_abs, .fn_sqrt, .fn_sin, .fn_cos, .fn_tan, .fn_log, .fn_log10, .fn_exp, .fn_trunc => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // fn_round rd, rs, prec - [rd:4|rs:4] [prec:8]
            .fn_round => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                const prec = operands[1];
                try self.writer.print(" r{}, r{}, prec={}", .{ rd, rs, prec });
            },

            // Nullary functions: [rd:4|0] [0]
            .fn_date, .fn_time, .fn_datetime, .fn_mem, .fn_error => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // fn_size, fn_instr: [rd:4|rs:4] [rs2 or 0]
            .fn_size => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            .fn_instr => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // ============================================
            // Terminal I/O (0xD0-0xD4)
            // ============================================

            // print/println/log argc - [unused:4|argc:4] [0]
            .print, .println, .log => {
                const rs: u4 = @truncate(operands[0] >> 4);
                const argc: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}", .{rs});
                if (argc > 0) {
                    try self.writer.print(", argc={}", .{argc});
                }
            },

            // readln/readkey rd - [rd:4|0] [0]
            .readln, .readkey => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // ============================================
            // Map Operations (0xD5-0xDF)
            // ============================================

            // map_new rd, flags - [rd:4|flags:4] [0]
            .map_new => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const flags: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, flags=0x{X}", .{ rd, flags });
            },

            // map_set map, key, val - [map:4|key:4] [val:4|0]
            .map_set => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                const key_reg: u4 = @truncate(operands[0] & 0xF);
                const val_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}[r{}] = r{}", .{ map_reg, key_reg, val_reg });
            },

            // map_get rd, map, key - [rd:4|map:4] [key:4|0]
            .map_get => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                const key_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}[r{}]", .{ rd, map_reg, key_reg });
            },

            // map_delete map, key - [map:4|key:4] [0]
            .map_delete => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                const key_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}[r{}]", .{ map_reg, key_reg });
            },

            // map_has rd, map, key - [rd:4|map:4] [key:4|0]
            .map_has => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                const key_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}.has(r{})", .{ rd, map_reg, key_reg });
            },

            // map_len rd, map - [rd:4|map:4] [0]
            .map_len => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}.len()", .{ rd, map_reg });
            },

            // map_clear map - [map:4|0] [0]
            .map_clear => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}.clear()", .{map_reg});
            },

            // map_keys rd, map - [rd:4|map:4] [0]
            .map_keys => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}.keys()", .{ rd, map_reg });
            },

            // map_values rd, map - [rd:4|map:4] [0]
            .map_values => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}.values()", .{ rd, map_reg });
            },

            // map_get_at rd, map, idx - [rd:4|map:4] [idx:4|0]
            .map_get_at => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                const idx_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}[@r{}]", .{ rd, map_reg, idx_reg });
            },

            // map_set_at map, idx, val - [map:4|idx:4] [val:4|0]
            .map_set_at => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                const idx_reg: u4 = @truncate(operands[0] & 0xF);
                const val_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}[@r{}] = r{}", .{ map_reg, idx_reg, val_reg });
            },

            // map_key_at rd, map, idx - [rd:4|map:4] [idx:4|0]
            .map_key_at => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const map_reg: u4 = @truncate(operands[0] & 0xF);
                const idx_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, key_at(r{}, r{})", .{ rd, map_reg, idx_reg });
            },

            // ============================================
            // List Operations
            // ============================================

            // list_new rd - [rd:4|0] [0]
            .list_new => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // list_push list, val - [list:4|val:4] [0]
            .list_push => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const val_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}.push(r{})", .{ list_reg, val_reg });
            },

            // list_pop rd, list - [rd:4|list:4] [0]
            .list_pop => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const list_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}.pop()", .{ rd, list_reg });
            },

            // list_get rd, list, idx - [rd:4|list:4] [idx:4|0]
            .list_get => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const list_reg: u4 = @truncate(operands[0] & 0xF);
                const idx_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}[r{}]", .{ rd, list_reg, idx_reg });
            },

            // list_set list, idx, val - [list:4|idx:4] [val:4|0]
            .list_set => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const idx_reg: u4 = @truncate(operands[0] & 0xF);
                const val_reg: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}[r{}] = r{}", .{ list_reg, idx_reg, val_reg });
            },

            // list_len rd, list - [rd:4|list:4] [0]
            .list_len => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const list_reg: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}.len()", .{ rd, list_reg });
            },

            // list_clear list - [list:4|0] [0]
            .list_clear => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}.clear()", .{list_reg});
            },

            // list_push_struct - [list:4|0] [field_count:8] [base_slot:16]
            .list_push_struct => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const field_count = operands[1];
                const base_slot: u16 = @as(u16, operands[2]) | (@as(u16, operands[3]) << 8);
                try self.writer.print(" r{}.push_struct(slot[{}..{}])", .{ list_reg, base_slot, base_slot + field_count });
            },

            // list_get_struct - [list:4|idx:4] [field_count:8] [base_reg:8] [0]
            .list_get_struct => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const idx_reg: u4 = @truncate(operands[0] & 0xF);
                const field_count = operands[1];
                const base_reg = operands[2];
                try self.writer.print(" r{}..r{} = r{}[r{}]", .{ base_reg, base_reg + field_count - 1, list_reg, idx_reg });
            },

            // list_pop_struct - [list:4|0] [field_count:8] [dest_slot:16]
            .list_pop_struct => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const field_count = operands[1];
                const dest_slot: u16 = @as(u16, operands[2]) | (@as(u16, operands[3]) << 8);
                try self.writer.print(" slot[{}..{}] = r{}.pop_struct()", .{ dest_slot, dest_slot + field_count, list_reg });
            },

            // list_set_struct - [list:4|idx:4] [field_count:8] [base_slot:16]
            .list_set_struct => {
                const list_reg: u4 = @truncate(operands[0] >> 4);
                const idx_reg: u4 = @truncate(operands[0] & 0xF);
                const field_count = operands[1];
                const base_slot: u16 = @as(u16, operands[2]) | (@as(u16, operands[3]) << 8);
                try self.writer.print(" r{}[r{}] = slot[{}..{}]", .{ list_reg, idx_reg, base_slot, base_slot + field_count });
            },

            // map_set_struct - [map:4|key:4] [field_count:8] [base_slot:16]
            .map_set_struct => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                const key_reg: u4 = @truncate(operands[0] & 0xF);
                const field_count = operands[1];
                const base_slot: u16 = @as(u16, operands[2]) | (@as(u16, operands[3]) << 8);
                try self.writer.print(" r{}[r{}] = slot[{}..{}]", .{ map_reg, key_reg, base_slot, base_slot + field_count });
            },

            // map_get_struct - [map:4|key:4] [field_count:8] [base_reg:8] [0]
            .map_get_struct => {
                const map_reg: u4 = @truncate(operands[0] >> 4);
                const key_reg: u4 = @truncate(operands[0] & 0xF);
                const field_count = operands[1];
                const base_reg = operands[2];
                try self.writer.print(" r{}[r{}] -> r{}..r{}", .{ map_reg, key_reg, base_reg, base_reg + field_count - 1 });
            },

            // ============================================
            // Debug & Meta (0xF0-0xFF)
            // ============================================

            .debug_break => {},

            // debug_line line - [0] [line:16]
            .debug_line => {
                const line: u16 = @bitCast(operands[1..3].*);
                try self.writer.print(" line {}", .{line});
            },

            // assert rs - [rs:4|0] [0]
            .assert => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // weak_ref rd, rs - [rd:4|rs:4] [0]
            .weak_ref => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // weak_load rd, rs - [rd:4|rs:4] [0]
            .weak_load => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // ============================================
            // ARC Operations (0xF5-0xF7)
            // ============================================

            // arc_retain rs - [rs:4|0] [0]
            .arc_retain, .arc_release => {
                const rs: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rs});
            },

            // arc_move rd, rs - [rd:4|rs:4] [0]
            .arc_move => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs: u4 = @truncate(operands[0] & 0xF);
                try self.writer.print(" r{}, r{}", .{ rd, rs });
            },

            // ============================================
            // Closure Operations
            // ============================================

            // make_closure rd, env_reg, fn_idx - [rd:4|env_reg:4] [fn_idx:16]
            .make_closure => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const env_reg: u4 = @truncate(operands[0] & 0xF);
                const fn_idx = std.mem.readInt(u16, operands[1..3], .little);
                try self.writer.print(" r{}, r{}, fn[{}]", .{ rd, env_reg, fn_idx });
            },

            // call_closure rd, closure_reg, argc - [rd:4|closure_reg:4] [argc:8]
            .call_closure => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const closure_reg: u4 = @truncate(operands[0] & 0xF);
                const argc = operands[1];
                try self.writer.print(" r{}, r{}, argc={}", .{ rd, closure_reg, argc });
            },

            // make_trait_object rd, src_reg, vtable_idx - [rd:4|src_reg:4] [vtable_idx:16]
            .make_trait_object => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const src_reg: u4 = @truncate(operands[0] & 0xF);
                const vtable_idx = std.mem.readInt(u16, operands[1..3], .little);
                try self.writer.print(" r{}, r{}, vtable[{}]", .{ rd, src_reg, vtable_idx });
            },

            // call_trait_method rd, obj_reg, method_idx, argc - [rd:4|obj_reg:4] [method_idx:8] [argc:8]
            .call_trait_method => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const obj_reg: u4 = @truncate(operands[0] & 0xF);
                const method_idx = operands[1];
                const argc = operands[2];
                try self.writer.print(" r{}, r{}, method[{}], argc={}", .{ rd, obj_reg, method_idx, argc });
            },

            .extended => {
                const sub_opcode = operands[0];
                try self.writer.print(" sub={X:0>2}", .{sub_opcode});
            },

            // ============================================
            // Quickened/Specialized Opcodes (0xE0-0xEF)
            // ============================================

            // Quickened arithmetic: [rd:4|rs1:4] [rs2:4|0]
            .add_int, .sub_int, .mul_int, .div_int => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // Quickened comparison: [rd:4|rs1:4] [rs2:4|0]
            .cmp_lt_int, .cmp_le_int, .cmp_gt_int, .cmp_ge_int, .cmp_eq_int, .cmp_ne_int => {
                const rd: u4 = @truncate(operands[0] >> 4);
                const rs1: u4 = @truncate(operands[0] & 0xF);
                const rs2: u4 = @truncate(operands[1] >> 4);
                try self.writer.print(" r{}, r{}, r{}", .{ rd, rs1, rs2 });
            },

            // Quickened incr/decr: [rd:4|0] [0]
            .incr_int, .decr_int => {
                const rd: u4 = @truncate(operands[0] >> 4);
                try self.writer.print(" r{}", .{rd});
            },

            // Unknown opcodes
            _ => {},
        }
    }

    fn printConstant(self: *Self, c: module.Constant) !void {
        switch (c) {
            .integer => |v| try self.writer.print("int({})", .{v}),
            .decimal => |d| try self.writer.print("dec({}, prec={})", .{ d.value, d.precision }),
            .string => |s| try self.writer.print("str(\"{s}\")", .{s}),
            .fixed_string => |a| try self.writer.print("fixed_string(\"{s}\", size={})", .{ a.data, a.size }),
            .identifier => |s| try self.writer.print("id(\"{s}\")", .{s}),
            .record_ref => |r| try self.writer.print("record#{}", .{r}),
            .routine_ref => |r| try self.writer.print("routine#{}", .{r}),
            .float => |v| try self.writer.print("float({})", .{v}),
            .boolean => |v| try self.writer.print("bool({})", .{v}),
        }
    }

    fn printTypeDef(self: *Self, t: module.TypeDef) !void {
        try self.writer.print(";   type#{}: ", .{t.type_id});
        if (t.name_index < self.mod.constants.len) {
            switch (self.mod.constants[t.name_index]) {
                .identifier => |name_str| try self.writer.print("{s}", .{name_str}),
                else => try self.writer.print("<unknown>", .{}),
            }
        }
        try self.writer.print(" (size={}, fields={})\n", .{ t.total_size, t.fields.len });
        for (t.fields) |f| {
            try self.writer.print(";       +{}: ", .{f.offset});
            if (f.name_index < self.mod.constants.len) {
                switch (self.mod.constants[f.name_index]) {
                    .identifier => |name_str| try self.writer.print("{s}", .{name_str}),
                    else => try self.writer.print("<?>", .{}),
                }
            }
            try self.writer.print(" : {s}({})\n", .{ @tagName(f.data_type), f.size });
        }
    }

    fn printRoutineHeader(self: *Self, r: module.RoutineDef, idx: u16) !void {
        try self.writer.print(";   routine#{}: ", .{idx});
        if (r.name_index < self.mod.constants.len) {
            switch (self.mod.constants[r.name_index]) {
                .identifier => |name_str| try self.writer.print("{s}", .{name_str}),
                else => try self.writer.print("<unknown>", .{}),
            }
        }
        try self.writer.print(" @ 0x{X:0>4} ({} bytes)", .{ r.code_offset, r.code_length });
        if (r.flags.is_function) try self.writer.print(" [FUNC]", .{});
        if (r.flags.is_void) try self.writer.print(" [VOID]", .{});
        try self.writer.print(" locals={} regs={}\n", .{ r.local_count, r.max_stack });
    }
};

/// Disassemble a module to a string
pub fn disassemble(allocator: std.mem.Allocator, mod: *const module.Module) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    var disasm = Disassembler.init(mod, buffer.writer());
    try disasm.disassembleModule();

    return buffer.toOwnedSlice();
}

/// Disassemble code bytes directly (without module context)
pub fn disassembleBytes(allocator: std.mem.Allocator, code: []const u8) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    // Create a minimal module wrapper
    var mod = module.Module.init(allocator);
    mod.code = code;

    var disasm = Disassembler.init(&mod, buffer.writer());
    try disasm.disassembleCode(0, code.len);

    return buffer.toOwnedSlice();
}

test "disassemble register bytecode" {
    const allocator = std.testing.allocator;

    // Register-based: movi r0, 42; movi r1, 10; add r2, r0, r1; halt
    const code = [_]u8{
        0x11, 0x00, 42, // movi r0, 42
        0x11, 0x10, 10, // movi r1, 10
        0x30, 0x20, 0x10, // add r2, r0, r1
        0x01, // halt
    };

    const result = try disassembleBytes(allocator, &code);
    defer allocator.free(result);

    try std.testing.expect(std.mem.indexOf(u8, result, "movi") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "add") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "halt") != null);
}
