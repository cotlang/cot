//! AMD64 code generation for x86-64 / Linux System V ABI.
//! Uses linear scan register allocation and generates ABI-compliant code.

const std = @import("std");
const Func = @import("../../ssa/func.zig").Func;
const Block = @import("../../ssa/block.zig").Block;
const value_mod = @import("../../ssa/value.zig");
const Value = value_mod.Value;
const AuxCall = value_mod.AuxCall;
const Op = @import("../../ssa/op.zig").Op;
const asm_mod = @import("amd64_asm.zig");
const regs = @import("amd64_regs.zig");
const Reg = regs.Reg;
const regalloc = @import("regalloc.zig");
const elf = @import("elf.zig");
const debug = @import("../../pipeline_debug.zig");
const types_mod = @import("../../frontend/types.zig");
const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const ir_mod = @import("../../frontend/ir.zig");

pub const Relocation = struct { offset: u32, target: []const u8 };
const BranchFixup = struct { code_offset: u32, target_block_id: u32, is_jcc: bool };
const StringRef = struct { code_offset: u32, string_data: []const u8 };

/// Check if value is a call result or copy of call (can't rematerialize calls)
fn isCallOrCopyOfCall(v: *const Value) bool {
    if (v.op == .static_call or v.op == .closure_call) return true;
    if (v.op == .copy and v.args.len > 0) {
        const src = v.args[0];
        if (src.op == .static_call or src.op == .closure_call) return true;
    }
    return false;
}

pub const AMD64CodeGen = struct {
    allocator: std.mem.Allocator,
    func: *const Func,
    type_reg: ?*const TypeRegistry = null,
    code: std.ArrayListUnmanaged(u8),
    symbols: std.ArrayListUnmanaged(elf.Symbol),
    relocations: std.ArrayListUnmanaged(Relocation),
    regalloc_state: ?*const regalloc.RegAllocState,
    frame_size: u32 = 16,
    block_offsets: std.AutoHashMapUnmanaged(u32, u32),
    branch_fixups: std.ArrayListUnmanaged(BranchFixup),
    string_refs: std.ArrayListUnmanaged(StringRef),
    globals: []const ir_mod.Global = &.{},
    debug_source_file: []const u8 = "",
    debug_source_text: []const u8 = "",
    has_hidden_return: bool = false, // >16B return needs hidden pointer in RDI
    hidden_ret_ptr_offset: u32 = 0,
    hidden_ret_frame_offset: u32 = 0,
    hidden_ret_space_needed: u32 = 0,
    hidden_ret_offsets: std.AutoHashMapUnmanaged(*const Value, u32),

    pub fn init(allocator: std.mem.Allocator) AMD64CodeGen {
        return .{ .allocator = allocator, .func = undefined, .code = .{}, .symbols = .{},
            .relocations = .{}, .regalloc_state = null, .block_offsets = .{}, .branch_fixups = .{},
            .string_refs = .{}, .hidden_ret_offsets = .{} };
    }

    pub fn deinit(self: *AMD64CodeGen) void {
        self.code.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.relocations.deinit(self.allocator);
        self.block_offsets.deinit(self.allocator);
        self.branch_fixups.deinit(self.allocator);
        for (self.string_refs.items) |str_ref| self.allocator.free(str_ref.string_data);
        self.string_refs.deinit(self.allocator);
        self.hidden_ret_offsets.deinit(self.allocator);
    }

    pub fn setRegAllocState(self: *AMD64CodeGen, state: *const regalloc.RegAllocState) void { self.regalloc_state = state; }
    pub fn setFrameSize(self: *AMD64CodeGen, size: u32) void { self.frame_size = size; }
    pub fn setGlobals(self: *AMD64CodeGen, globs: []const ir_mod.Global) void { self.globals = globs; }
    pub fn setTypeRegistry(self: *AMD64CodeGen, reg: *const TypeRegistry) void { self.type_reg = reg; }
    pub fn setDebugInfo(self: *AMD64CodeGen, source_file: []const u8, source_text: []const u8) void {
        self.debug_source_file = source_file;
        self.debug_source_text = source_text;
    }

    fn getTypeSize(self: *const AMD64CodeGen, type_idx: TypeIndex) u32 {
        if (type_idx < TypeRegistry.FIRST_USER_TYPE) return TypeRegistry.basicTypeSize(type_idx);
        if (self.type_reg) |reg| return reg.sizeOf(type_idx);
        return TypeRegistry.basicTypeSize(type_idx);
    }

    fn isTwoRegStruct(self: *const AMD64CodeGen, type_idx: TypeIndex) bool {
        if (type_idx < TypeRegistry.FIRST_USER_TYPE) return false;
        if (self.type_reg) |reg| {
            const type_info = reg.get(type_idx);
            if (type_info == .struct_type) {
                const size = reg.sizeOf(type_idx);
                return size > 8 and size <= 16;
            }
        }
        return false;
    }

    /// Check if a type is a >16 byte struct (passed by pointer on AMD64)
    fn isLargeStruct(self: *const AMD64CodeGen, type_idx: TypeIndex) bool {
        if (type_idx < TypeRegistry.FIRST_USER_TYPE) return false;
        if (self.type_reg) |reg| {
            const type_info = reg.get(type_idx);
            if (type_info == .struct_type) {
                const size = reg.sizeOf(type_idx);
                return size > 16;
            }
        }
        return false;
    }

    /// Current code offset
    fn offset(self: *const AMD64CodeGen) u32 {
        return @intCast(self.code.items.len);
    }

    /// Emit bytes to code buffer
    fn emitBytes(self: *AMD64CodeGen, bytes: []const u8) !void {
        try self.code.appendSlice(self.allocator, bytes);
    }

    /// Emit a fixed-size instruction
    fn emit(self: *AMD64CodeGen, comptime N: usize, bytes: [N]u8) !void {
        try self.code.appendSlice(self.allocator, &bytes);
    }

    /// Emit variable-length instruction (for instructions that may or may not need REX prefix)
    fn emitVarLen(self: *AMD64CodeGen, data: anytype, len: u8) !void {
        try self.code.appendSlice(self.allocator, data[0..len]);
    }

    /// Store 64-bit register to frame location [RBP + frame_off]
    /// frame_off is typically negative (e.g., -8 for first local)
    fn emitStoreRegToFrame(self: *AMD64CodeGen, src: Reg, frame_off: i32) !void {
        const enc = asm_mod.encodeMovMemReg(.rbp, frame_off, src);
        try self.code.appendSlice(self.allocator, enc.data[0..enc.len]);
    }

    /// Load 64-bit register from frame location [RBP + frame_off]
    fn emitLoadRegFromFrame(self: *AMD64CodeGen, dst: Reg, frame_off: i32) !void {
        const enc = asm_mod.encodeMovRegMemDisp(dst, .rbp, frame_off);
        try self.code.appendSlice(self.allocator, enc.data[0..enc.len]);
    }

    /// Load effective address from frame: LEA dst, [RBP + frame_off]
    fn emitLeaFromFrame(self: *AMD64CodeGen, dst: Reg, frame_off: i32) !void {
        const enc = asm_mod.encodeLeaRegMem(dst, .rbp, frame_off);
        try self.code.appendSlice(self.allocator, enc.data[0..enc.len]);
    }

    // ========================================================================
    // Register Mapping
    // ========================================================================

    /// Map regalloc register number to AMD64 register.
    /// ARM64 codegen uses x0-x28, we use a similar mapping.
    fn regNumToAMD64(reg_num: u5) Reg {
        return switch (reg_num) {
            0 => .rax,
            1 => .rcx,
            2 => .rdx,
            3 => .rbx,
            4 => .rsp, // Should not be used for values
            5 => .rbp, // Frame pointer
            6 => .rsi,
            7 => .rdi,
            8 => .r8,
            9 => .r9,
            10 => .r10,
            11 => .r11,
            12 => .r12,
            13 => .r13,
            14 => .r14,
            15 => .r15,
            else => .rax, // Fallback
        };
    }

    /// Get destination register for a value from regalloc.
    /// First checks regalloc_state.values[].regs, then falls back to value.getHome().
    fn getDestRegForValue(self: *AMD64CodeGen, value: *const Value) Reg {
        // First try regalloc_state
        if (self.regalloc_state) |state| {
            if (value.id < state.values.len) {
                const val_state = state.values[value.id];
                if (val_state.firstReg()) |reg_num| {
                    return regNumToAMD64(@intCast(reg_num));
                }
            }
        }
        // Fall back to home assignment
        if (value.getHome()) |loc| {
            switch (loc) {
                .register => |reg_num| return regNumToAMD64(@intCast(reg_num)),
                .stack => {}, // Fall through to RAX fallback
            }
        }
        // Fallback: use RAX
        return .rax;
    }

    /// Get register for a value that's already been computed.
    /// First checks regalloc_state.values[].regs, then falls back to value.getHome().
    /// The regs mask may be cleared when a value becomes "dead" after its last use,
    /// but the home assignment persists.
    fn getRegForValue(self: *AMD64CodeGen, value: *const Value) ?Reg {
        // First try regalloc_state (for values still "live" in regalloc terms)
        if (self.regalloc_state) |state| {
            if (value.id < state.values.len) {
                const val_state = state.values[value.id];
                if (val_state.firstReg()) |reg_num| {
                    return regNumToAMD64(@intCast(reg_num));
                }
                // BUG-080: Removed the BUG-078 spill check here. The old code returned null
                // if spill != null, which caused incorrect rematerialization of comparison
                // values used in control flow. The home register assignment is still valid
                // even if the value was spilled - the spill slot is a backup copy, not a
                // replacement. Fall through to check the home assignment.
            }
        }
        // Fall back to home assignment (persists even after value is "dead")
        if (value.getHome()) |loc| {
            switch (loc) {
                .register => |reg_num| return regNumToAMD64(@intCast(reg_num)),
                .stack => return null, // Value is spilled, not in a register
            }
        }
        return null;
    }

    fn ensureInReg(self: *AMD64CodeGen, value: *const Value, hint_reg: Reg) !void {
        if (self.getRegForValue(value)) |_| return;
        return self.rematerializeValue(value, hint_reg);
    }

    fn forceInReg(self: *AMD64CodeGen, value: *const Value, hint_reg: Reg) std.mem.Allocator.Error!void {
        return self.rematerializeValue(value, hint_reg);
    }

    fn rematerializeValue(self: *AMD64CodeGen, value: *const Value, hint_reg: Reg) std.mem.Allocator.Error!void {
        // Check for spilled value - reload from stack
        if (self.regalloc_state) |state| {
            if (value.id < state.values.len) {
                if (state.values[value.id].spill) |spill| {
                    const loc = spill.getHome() orelse return self.rematerializeByOp(value, hint_reg);
                    const disp: i32 = -@as(i32, @intCast(loc.stackOffset() + 8));
                    const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                    try self.emitBytes(load.data[0..load.len]);
                    return;
                }
            }
        }
        return self.rematerializeByOp(value, hint_reg);
    }

    fn rematerializeByOp(self: *AMD64CodeGen, value: *const Value, hint_reg: Reg) std.mem.Allocator.Error!void {
        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(hint_reg, value.aux_int),
            .const_bool => try self.emitLoadImmediate(hint_reg, if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emit(3, asm_mod.encodeXorRegReg(hint_reg, hint_reg)),
            .local_addr => {
                const local_idx: usize = @intCast(value.aux_int);
                const disp: i32 = if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len)
                    -(self.func.local_offsets[local_idx] + @as(i32, @intCast(self.func.local_sizes[local_idx])))
                else 0;
                const lea = asm_mod.encodeLeaDisp32(hint_reg, .rbp, disp);
                try self.emitBytes(lea.data[0..lea.len]);
            },
            .load => {
                if (value.args.len > 0) {
                    const scratch: Reg = if (hint_reg == .r11) .r10 else .r11;
                    try self.rematerializeValue(value.args[0], scratch);
                    const type_size = self.getTypeSize(value.type_idx);
                    if (type_size == 1) {
                        const load = asm_mod.encodeLoadByteDisp32(hint_reg, scratch, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else if (type_size == 2) {
                        const load = asm_mod.encodeLoadWordDisp32(hint_reg, scratch, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else if (type_size == 4) {
                        const load = asm_mod.encodeLoadDwordDisp32(hint_reg, scratch, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else {
                        const load = asm_mod.encodeLoadDisp32(hint_reg, scratch, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    }
                }
            },
            .const_string, .const_ptr => {
                const string_index: usize = @intCast(value.aux_int);
                const str_data = if (string_index < self.func.string_literals.len) self.func.string_literals[string_index] else "";
                const lea_offset = self.offset();
                try self.emit(7, asm_mod.encodeLeaRipRel32(hint_reg, 0));
                try self.string_refs.append(self.allocator, .{
                    .code_offset = lea_offset, .string_data = try self.allocator.dupe(u8, str_data),
                });
            },
            .global_addr => {
                const global_name = switch (value.aux) { .string => |s| s, else => "unknown_global" };
                const lea_offset = self.offset();
                try self.emit(7, asm_mod.encodeLeaRipRel32(hint_reg, 0));
                try self.relocations.append(self.allocator, .{ .offset = @intCast(lea_offset + 3), .target = global_name });
            },
            .arg => {
                const user_arg_idx: usize = @intCast(value.aux_int);
                const abi_arg_idx: usize = if (self.has_hidden_return) user_arg_idx + 1 else user_arg_idx;
                const max_reg_args: usize = if (self.has_hidden_return) 5 else 6;

                if (user_arg_idx < max_reg_args and abi_arg_idx < regs.AMD64.arg_regs.len) {
                    const src_reg = regs.AMD64.arg_regs[abi_arg_idx];
                    if (hint_reg != src_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, src_reg));
                    }
                } else {
                    // Stack argument - load from caller's stack frame
                    const stack_arg_idx = if (self.has_hidden_return) user_arg_idx - 5 else user_arg_idx - 6;
                    const stack_offset: i32 = @intCast(16 + stack_arg_idx * 8);
                    const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, stack_offset);
                    try self.emitBytes(load.data[0..load.len]);
                }
            },
            .off_ptr => {
                // Rematerialize pointer offset: LEA hint_reg, [base + offset]
                if (value.args.len > 0) {
                    const base = value.args[0];
                    const field_offset: i64 = value.aux_int;

                    // First get or rematerialize the base pointer
                    var base_reg: Reg = undefined;
                    if (base.op == .local_addr) {
                        // Rematerialize local address first
                        const local_idx: usize = @intCast(base.aux_int);
                        if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                            const local_offset = self.func.local_offsets[local_idx];
                            const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                            const disp: i32 = -(local_offset + local_size);
                            const lea = asm_mod.encodeLeaDisp32(hint_reg, .rbp, disp);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = hint_reg;
                        } else {
                            const lea = asm_mod.encodeLeaDisp32(hint_reg, .rbp, 0);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = hint_reg;
                        }
                    } else {
                        // ALWAYS rematerialize the base - never trust getRegForValue.
                        // During call arg setup, registers can be clobbered by stack arg pushes.
                        const scratch: Reg = if (hint_reg == .r11) .r10 else .r11;
                        try self.rematerializeValue(base, scratch);
                        base_reg = scratch;
                    }

                    // Now add the offset
                    if (field_offset != 0) {
                        const disp: i32 = @intCast(field_offset);
                        const lea = asm_mod.encodeLeaDisp32(hint_reg, base_reg, disp);
                        try self.emitBytes(lea.data[0..lea.len]);
                    } else if (base_reg != hint_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, base_reg));
                    }
                }
            },
            .add_ptr => {
                // Rematerialize pointer addition: hint_reg = base + offset
                // CRITICAL: Don't trust getRegForValue - always rematerialize both operands.
                // Use R8 for base, R9 for offset to avoid conflicts.
                // IMPORTANT: Rematerialize BASE FIRST, then OFFSET.
                // If base is a nested add_ptr, rematerializing it will use r8/r9 internally.
                // We must rematerialize offset AFTER base so r9 has the correct value.
                if (value.args.len >= 2) {
                    const base = value.args[0];
                    const off_val = value.args[1];

                    // Rematerialize base FIRST into R8 (may clobber R9 during recursion)
                    var base_reg: Reg = undefined;
                    if (base.op == .local_addr) {
                        const local_idx: usize = @intCast(base.aux_int);
                        if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                            const local_offset = self.func.local_offsets[local_idx];
                            const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                            const disp: i32 = -(local_offset + local_size);
                            const lea = asm_mod.encodeLeaDisp32(.r8, .rbp, disp);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = .r8;
                        } else {
                            const lea = asm_mod.encodeLeaDisp32(.r8, .rbp, 0);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = .r8;
                        }
                    } else {
                        try self.rematerializeValue(base, .r8);
                        base_reg = .r8;
                    }

                    // Rematerialize offset AFTER base into R9
                    try self.rematerializeValue(off_val, .r9);
                    const off_reg = Reg.r9;

                    // LEA hint_reg, [base + off]
                    const lea = asm_mod.encodeLeaBaseIndex(hint_reg, base_reg, off_reg);
                    try self.emitBytes(lea.data[0..lea.len]);
                }
            },
            .sub_ptr => {
                // Rematerialize pointer subtraction: hint_reg = base - offset
                // CRITICAL: Don't trust getRegForValue - always rematerialize both operands.
                // Use R8 for base, R9 for offset to avoid conflicts.
                if (value.args.len >= 2) {
                    const base = value.args[0];
                    const off_val = value.args[1];

                    // Rematerialize base FIRST into R8
                    var base_reg: Reg = undefined;
                    if (base.op == .local_addr) {
                        const local_idx: usize = @intCast(base.aux_int);
                        if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                            const local_offset = self.func.local_offsets[local_idx];
                            const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                            const disp: i32 = -(local_offset + local_size);
                            const lea = asm_mod.encodeLeaDisp32(.r8, .rbp, disp);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = .r8;
                        } else {
                            const lea = asm_mod.encodeLeaDisp32(.r8, .rbp, 0);
                            try self.emitBytes(lea.data[0..lea.len]);
                            base_reg = .r8;
                        }
                    } else {
                        try self.rematerializeValue(base, .r8);
                        base_reg = .r8;
                    }

                    // Rematerialize offset AFTER base into R9
                    try self.rematerializeValue(off_val, .r9);
                    const off_reg = Reg.r9;

                    // SUB: hint_reg = base - offset
                    // First move base to hint_reg, then subtract offset
                    if (hint_reg != base_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, base_reg));
                    }
                    try self.emit(3, asm_mod.encodeSubRegReg(hint_reg, off_reg));
                }
            },
            .mul => {
                // Rematerialize multiplication
                // CRITICAL: Must use callee-saved registers (r14/r15) with push/pop.
                // ANY caller-saved register can be in use by the main codegen.
                //
                // Strategy depends on hint_reg:
                // - If hint_reg is NOT r14/r15: move result to hint_reg before restore
                // - If hint_reg IS r14: leave result in r14, only restore r15
                // - If hint_reg IS r15: use stack to transfer result
                if (value.args.len >= 2) {
                    const op1 = value.args[0];
                    const op2 = value.args[1];

                    // Save r14 and r15 (callee-saved)
                    const push_r14 = asm_mod.encodePush(.r14);
                    try self.emitBytes(push_r14.data[0..push_r14.len]);
                    const push_r15 = asm_mod.encodePush(.r15);
                    try self.emitBytes(push_r15.data[0..push_r15.len]);

                    // Load op1 into r14, op2 into r15
                    try self.rematerializeValue(op1, .r14);
                    try self.rematerializeValue(op2, .r15);

                    // r14 = r14 * r15 (result in r14)
                    try self.emit(4, asm_mod.encodeImulRegReg(.r14, .r15));

                    if (hint_reg == .r14) {
                        // Result already in r14, restore r15, skip old_r14
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        // Skip saved old_r14 without clobbering any register
                        // ADD RSP, 8 = 48 83 C4 08
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xC4, 0x08 });
                    } else if (hint_reg == .r15) {
                        // Need result in r15. r14 has result, stack has [old_r14] [old_r15]
                        // mov r15, r14 (move result)
                        try self.emit(3, asm_mod.encodeMovRegReg(.r15, .r14));
                        // pop r14 (r14 = old_r15, we'll overwrite it next)
                        const pop_temp = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_temp.data[0..pop_temp.len]);
                        // pop r14 (r14 = old_r14, restored)
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    } else {
                        // hint_reg is not r14 or r15, safe to move before restore
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .r14));
                        // Restore r15 and r14
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    }
                }
            },
            .add => {
                // Rematerialize addition - same strategy as mul
                if (value.args.len >= 2) {
                    const op1 = value.args[0];
                    const op2 = value.args[1];

                    const push_r14 = asm_mod.encodePush(.r14);
                    try self.emitBytes(push_r14.data[0..push_r14.len]);
                    const push_r15 = asm_mod.encodePush(.r15);
                    try self.emitBytes(push_r15.data[0..push_r15.len]);

                    try self.rematerializeValue(op1, .r14);
                    try self.rematerializeValue(op2, .r15);
                    try self.emit(3, asm_mod.encodeAddRegReg(.r14, .r15));

                    if (hint_reg == .r14) {
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xC4, 0x08 }); // add rsp, 8
                    } else if (hint_reg == .r15) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r15, .r14));
                        const pop_temp = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_temp.data[0..pop_temp.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    } else {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .r14));
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    }
                }
            },
            .sub => {
                // Rematerialize subtraction - same strategy as mul
                if (value.args.len >= 2) {
                    const op1 = value.args[0];
                    const op2 = value.args[1];

                    const push_r14 = asm_mod.encodePush(.r14);
                    try self.emitBytes(push_r14.data[0..push_r14.len]);
                    const push_r15 = asm_mod.encodePush(.r15);
                    try self.emitBytes(push_r15.data[0..push_r15.len]);

                    try self.rematerializeValue(op1, .r14);
                    try self.rematerializeValue(op2, .r15);
                    try self.emit(3, asm_mod.encodeSubRegReg(.r14, .r15));

                    if (hint_reg == .r14) {
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xC4, 0x08 }); // add rsp, 8
                    } else if (hint_reg == .r15) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r15, .r14));
                        const pop_temp = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_temp.data[0..pop_temp.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    } else {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .r14));
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    }
                }
            },
            .div => {
                // Rematerialize division - conditional recomputation
                // If either operand is a call result, DON'T recompute (call can't be re-executed)
                // Instead, use the default case (load from home)
                const can_recompute = blk: {
                    if (value.args.len < 2) break :blk false;
                    const op1 = value.args[0];
                    const op2 = value.args[1];
                    // Check if either operand is a call result (or a copy of a call result)
                    // Regalloc creates copy values to relocate call results from RAX
                    if (isCallOrCopyOfCall(op1)) break :blk false;
                    if (isCallOrCopyOfCall(op2)) break :blk false;
                    break :blk true;
                };

                if (can_recompute) {
                    // Safe to recompute - neither operand is a call result
                    const op1 = value.args[0];
                    const op2 = value.args[1];

                    // Save R14 and R15 (callee-saved)
                    const push_r14 = asm_mod.encodePush(.r14);
                    try self.emitBytes(push_r14.data[0..push_r14.len]);
                    const push_r15 = asm_mod.encodePush(.r15);
                    try self.emitBytes(push_r15.data[0..push_r15.len]);

                    // Rematerialize dividend into R14, divisor into R15
                    try self.rematerializeValue(op1, .r14);
                    try self.rematerializeValue(op2, .r15);

                    // Move dividend to RAX, divisor to R11
                    try self.emit(3, asm_mod.encodeMovRegReg(.rax, .r14));
                    try self.emit(3, asm_mod.encodeMovRegReg(.r11, .r15));

                    // CQO - sign extend RAX to RDX:RAX
                    try self.emit(2, asm_mod.encodeCqo());

                    // IDIV R11
                    try self.emit(3, asm_mod.encodeIdivReg(.r11));

                    // Result is in RAX, move to hint_reg if needed
                    if (hint_reg == .r14) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r14, .rax));
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xC4, 0x08 });
                    } else if (hint_reg == .r15) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r15, .rax));
                        const pop_temp = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_temp.data[0..pop_temp.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    } else {
                        if (hint_reg != .rax) {
                            try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .rax));
                        }
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    }
                    debug.log(.codegen, "      rematerialize div (recomputed) to {s}", .{hint_reg.name()});
                } else {
                    // Can't safely recompute - fall through to default (load from home)
                    if (value.getHome()) |loc| {
                        switch (loc) {
                            .stack => |byte_off| {
                                const disp: i32 = -byte_off;
                                const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                                try self.emitBytes(load.data[0..load.len]);
                            },
                            .register => |reg_num| {
                                const src_reg = regNumToAMD64(@intCast(reg_num));
                                if (src_reg != hint_reg) {
                                    try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, src_reg));
                                }
                            },
                        }
                    } else {
                        debug.log(.codegen, "WARNING: div with call operand has no home", .{});
                    }
                }
            },
            .mod => {
                // Rematerialize modulo - same conditional logic as div
                const can_recompute = blk: {
                    if (value.args.len < 2) break :blk false;
                    const op1 = value.args[0];
                    const op2 = value.args[1];
                    // Check if either operand is a call result (or a copy of a call result)
                    // Regalloc creates copy values to relocate call results from RAX
                    if (isCallOrCopyOfCall(op1)) break :blk false;
                    if (isCallOrCopyOfCall(op2)) break :blk false;
                    break :blk true;
                };

                if (can_recompute) {
                    const op1 = value.args[0];
                    const op2 = value.args[1];

                    const push_r14 = asm_mod.encodePush(.r14);
                    try self.emitBytes(push_r14.data[0..push_r14.len]);
                    const push_r15 = asm_mod.encodePush(.r15);
                    try self.emitBytes(push_r15.data[0..push_r15.len]);

                    try self.rematerializeValue(op1, .r14);
                    try self.rematerializeValue(op2, .r15);

                    try self.emit(3, asm_mod.encodeMovRegReg(.rax, .r14));
                    try self.emit(3, asm_mod.encodeMovRegReg(.r11, .r15));
                    try self.emit(2, asm_mod.encodeCqo());
                    try self.emit(3, asm_mod.encodeIdivReg(.r11));

                    if (hint_reg == .r14) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r14, .rdx));
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xC4, 0x08 });
                    } else if (hint_reg == .r15) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r15, .rdx));
                        const pop_temp = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_temp.data[0..pop_temp.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    } else {
                        if (hint_reg != .rdx) {
                            try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .rdx));
                        }
                        const pop_r15 = asm_mod.encodePop(.r15);
                        try self.emitBytes(pop_r15.data[0..pop_r15.len]);
                        const pop_r14 = asm_mod.encodePop(.r14);
                        try self.emitBytes(pop_r14.data[0..pop_r14.len]);
                    }
                    debug.log(.codegen, "      rematerialize mod (recomputed) to {s}", .{hint_reg.name()});
                } else {
                    if (value.getHome()) |loc| {
                        switch (loc) {
                            .stack => |byte_off| {
                                const disp: i32 = -byte_off;
                                const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                                try self.emitBytes(load.data[0..load.len]);
                            },
                            .register => |reg_num| {
                                const src_reg = regNumToAMD64(@intCast(reg_num));
                                if (src_reg != hint_reg) {
                                    try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, src_reg));
                                }
                            },
                        }
                    } else {
                        debug.log(.codegen, "WARNING: mod with call operand has no home", .{});
                    }
                }
            },
            .copy => {
                // Copy needs to trace through to the source value
                if (value.args.len > 0) {
                    try self.rematerializeValue(value.args[0], hint_reg);
                }
            },
            .string_make => {
                // For string_make, we need the ptr component (first arg)
                // This is used when a string is passed as an argument to a function
                if (value.args.len > 0) {
                    try self.rematerializeValue(value.args[0], hint_reg);
                }
            },
            .static_call => {
                // Function call result is in RAX (System V ABI)
                // CRITICAL: Only trust STACK homes - register homes or RAX may have been
                // clobbered. If there's no stack home, we can't rematerialize the call result
                // (we can't re-execute the call due to potential side effects).
                if (value.getHome()) |loc| {
                    switch (loc) {
                        .stack => |byte_off| {
                            const disp: i32 = -@as(i32, @intCast(byte_off));
                            const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                            try self.emitBytes(load.data[0..load.len]);
                            debug.log(.codegen, "      reload static_call from [RBP{d}] to {s}", .{ disp, hint_reg.name() });
                        },
                        .register => {
                            // DON'T trust register homes - they may have been clobbered
                            // Fall through to warning
                            debug.log(.codegen, "WARNING: static_call has register home but may be clobbered, copying from RAX", .{});
                            if (hint_reg != .rax) {
                                try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .rax));
                            }
                        },
                    }
                } else {
                    // Fallback: result should be in RAX (hope it wasn't clobbered)
                    debug.log(.codegen, "WARNING: static_call has no home, copying from RAX (may be stale)", .{});
                    if (hint_reg != .rax) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, .rax));
                    }
                }
            },
            .eq, .ne, .lt, .le, .gt, .ge => {
                // Comparison operations need to be rematerialized by re-doing the comparison
                // CRITICAL: Don't use getRegForValue - always rematerialize operands.
                if (value.args.len >= 2) {
                    // Always rematerialize both operands into fixed scratch registers
                    try self.rematerializeValue(value.args[1], .rcx);
                    const op2_reg = Reg.rcx;

                    try self.rematerializeValue(value.args[0], .r10);
                    const op1_reg = Reg.r10;

                    // CMP op1, op2
                    try self.emit(3, asm_mod.encodeCmpRegReg(op1_reg, op2_reg));

                    // SETcc hint_reg
                    const cond: asm_mod.Cond = switch (value.op) {
                        .eq => .e,
                        .ne => .ne,
                        .lt => .l,
                        .le => .le,
                        .gt => .g,
                        .ge => .ge,
                        else => .e,
                    };
                    try self.emit(4, asm_mod.encodeSetcc(cond, hint_reg));
                    try self.emit(4, asm_mod.encodeMovzxRegReg8(hint_reg, hint_reg));
                    debug.log(.codegen, "      rematerialize comparison {s} to {s}", .{ @tagName(value.op), hint_reg.name() });
                }
            },
            .neg => {
                // Rematerialize negation
                // CRITICAL: Don't use getRegForValue - always rematerialize operand.
                if (value.args.len >= 1) {
                    const scratch: Reg = if (hint_reg == .r10) .r11 else .r10;
                    try self.rematerializeValue(value.args[0], scratch);
                    if (hint_reg != scratch) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, scratch));
                    }
                    try self.emit(3, asm_mod.encodeNegReg(hint_reg));
                    debug.log(.codegen, "      rematerialize neg to {s}", .{hint_reg.name()});
                }
            },
            .not => {
                // Rematerialize NOT - distinguish between boolean and integer
                // CRITICAL: Don't use getRegForValue - always rematerialize operand.
                if (value.args.len >= 1) {
                    const scratch: Reg = if (hint_reg == .r10) .r11 else .r10;
                    try self.rematerializeValue(value.args[0], scratch);
                    if (hint_reg != scratch) {
                        try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, scratch));
                    }
                    // Check if this is a boolean (1-byte type)
                    const type_size = self.getTypeSize(value.type_idx);
                    if (type_size == 1) {
                        // Boolean: XOR with 1
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xF0 | @as(u8, @intFromEnum(hint_reg) & 7), 0x01 });
                        debug.log(.codegen, "      rematerialize not (bool) to {s}", .{hint_reg.name()});
                    } else {
                        // Integer: bitwise NOT
                        try self.emit(3, asm_mod.encodeNotReg(hint_reg));
                        debug.log(.codegen, "      rematerialize not (int) to {s}", .{hint_reg.name()});
                    }
                }
            },
            .load_reg => {
                // load_reg was loaded from a spill slot - always reload from the ORIGINAL spill slot.
                // The value's args[0] contains the spilled value whose home is the stack location.
                // We CANNOT trust the register home because it may have been clobbered.
                if (value.args.len > 0) {
                    const spill_value = value.args[0];
                    if (spill_value.getHome()) |loc| {
                        const byte_off = loc.stackOffset();
                        // Match store_reg offset: -(byte_off + 8)
                        const disp: i32 = -@as(i32, @intCast(byte_off + 8));
                        const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                        try self.emitBytes(load.data[0..load.len]);
                        debug.log(.codegen, "      reload load_reg from spill [RBP{d}] to {s}", .{ disp, hint_reg.name() });
                    } else {
                        debug.log(.codegen, "WARNING: load_reg source has no spill slot", .{});
                    }
                }
            },
            else => {
                // Load from spill slot if available
                if (value.getHome()) |loc| {
                    switch (loc) {
                        .stack => |byte_off| {
                            const disp: i32 = -byte_off;
                            const load = asm_mod.encodeLoadDisp32(hint_reg, .rbp, disp);
                            try self.emitBytes(load.data[0..load.len]);
                            debug.log(.codegen, "      reload {s} from [RBP{d}] to {s}", .{ @tagName(value.op), disp, hint_reg.name() });
                        },
                        .register => |reg_num| {
                            debug.log(.codegen, "      ensureInReg: home.register={d}", .{reg_num});
                            const src_reg = regNumToAMD64(@intCast(reg_num));
                            if (src_reg != hint_reg) {
                                try self.emit(3, asm_mod.encodeMovRegReg(hint_reg, src_reg));
                                debug.log(.codegen, "      reload {s} from {s} to {s}", .{ @tagName(value.op), src_reg.name(), hint_reg.name() });
                            }
                        },
                    }
                } else {
                    debug.log(.codegen, "WARNING: ensureInReg fallback for {s}", .{@tagName(value.op)});
                }
            },
        }
    }

    /// Emit phi moves for an edge from current block to target block.
    /// For each phi in the target block, find this block's corresponding argument
    /// and move it to the phi's register.
    fn emitPhiMoves(self: *AMD64CodeGen, current_block: *const Block, target_block: *const Block) !void {
        // Find which predecessor index we are in target's predecessor list
        var pred_idx: ?usize = null;
        for (target_block.preds, 0..) |pred_edge, i| {
            if (pred_edge.b.id == current_block.id) {
                pred_idx = i;
                break;
            }
        }
        const idx = pred_idx orelse return; // Not a predecessor (shouldn't happen)

        // Parallel copy algorithm:
        // When multiple phis need to be resolved, we can't emit moves sequentially
        // because a move's destination might be another move's source.
        // Example: phi1: rax = rcx, phi2: r8 = rax - if we emit phi1 first, rax is clobbered!
        //
        // Solution: Two-phase approach
        // Phase 1: Save all source values that might be clobbered to temp registers
        // Phase 2: Copy from temps/sources to final destinations

        const PhiMove = struct {
            src_val: *const Value,
            src_reg: ?Reg,
            dest_reg: Reg,
            needs_temp: bool,
            temp_reg: Reg,
        };

        // Collect all phi moves
        var moves = std.ArrayListUnmanaged(PhiMove){};
        defer moves.deinit(self.allocator);

        for (target_block.values.items) |value| {
            if (value.op != .phi) continue;

            const args = value.args;
            if (idx >= args.len) continue;
            const src_val = args[idx];

            const phi_reg = self.getRegForValue(value) orelse continue;
            const src_reg = self.getRegForValue(src_val);

            try moves.append(self.allocator, .{
                .src_val = src_val,
                .src_reg = src_reg,
                .dest_reg = phi_reg,
                .needs_temp = false,
                .temp_reg = .rax, // Placeholder
            });
        }

        if (moves.items.len == 0) return;

        // Detect conflicts: a source reg that will be overwritten before it's read
        // A move needs a temp if its source_reg equals any other move's dest_reg
        const temp_regs = [_]Reg{ .r10, .r11 }; // Scratch registers for cycle breaking
        var temp_idx: usize = 0;
        for (moves.items, 0..) |*move, i| {
            if (move.src_reg) |src| {
                // Check if this source will be clobbered by an earlier move
                for (moves.items[0..i]) |other| {
                    if (other.dest_reg == src) {
                        // This source will be clobbered before we read it
                        move.needs_temp = true;
                        move.temp_reg = temp_regs[temp_idx % temp_regs.len];
                        temp_idx += 1;
                        break;
                    }
                }
            }
        }

        // Phase 1: Save conflicting sources to temp registers
        for (moves.items) |move| {
            if (move.needs_temp) {
                if (move.src_reg) |src| {
                    // MOV temp, src
                    try self.emit(3, asm_mod.encodeMovRegReg(move.temp_reg, src));
                }
            }
        }

        // Phase 2: Emit actual moves
        for (moves.items) |move| {
            if (move.needs_temp) {
                // Source was saved to temp
                if (move.src_reg != null) {
                    // MOV dest, temp
                    try self.emit(3, asm_mod.encodeMovRegReg(move.dest_reg, move.temp_reg));
                } else {
                    // Source wasn't in a register, regenerate to dest
                    try self.ensureInReg(move.src_val, move.dest_reg);
                }
            } else {
                // No conflict, emit directly
                if (move.src_reg) |s| {
                    if (s != move.dest_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(move.dest_reg, s));
                    }
                } else {
                    try self.ensureInReg(move.src_val, move.dest_reg);
                }
            }
        }
    }

    /// Setup call arguments with parallel copy to avoid clobbering.
    /// Returns the stack cleanup amount in bytes.
    fn setupCallArgs(self: *AMD64CodeGen, args: []*Value) !usize {
        if (args.len == 0) return 0;

        // First pass: count how many ABI registers each arg uses
        // (9-16B structs use 2 registers, everything else uses 1)
        var abi_reg_count: usize = 0;
        for (args) |arg| {
            const type_size = self.getTypeSize(arg.type_idx);
            const is_two_reg = self.isTwoRegStruct(arg.type_idx);
            if (is_two_reg) {
                abi_reg_count += 2;
            } else if (type_size > 16) {
                // >16B: passed by pointer, 1 register
                abi_reg_count += 1;
            } else {
                abi_reg_count += 1;
            }
        }

        // Handle stack arguments (args that don't fit in the 6 ABI registers)
        var stack_cleanup: usize = 0;
        if (abi_reg_count > 6) {
            // For simplicity, push excess args in reverse order
            // This is a simplified version - just push the excess as 8-byte values
            var abi_idx: usize = 0;
            var i: usize = args.len;
            // Count how many register slots are used by first N args
            var reg_used: usize = 0;
            for (args) |arg| {
                const is_two_reg = self.isTwoRegStruct(arg.type_idx);
                if (is_two_reg) {
                    reg_used += 2;
                } else {
                    reg_used += 1;
                }
                if (reg_used > 6) break;
                abi_idx += 1;
            }
            // Push remaining args in reverse
            // IMPORTANT: Always use forceInReg to rematerialize each value before pushing.
            // We cannot trust getRegForValue here because by the time we get to this loop,
            // the registers assigned to these values may have been reused for other values.
            // This ensures each value is freshly loaded before being pushed.
            while (i > abi_idx) {
                i -= 1;
                const arg = args[i];
                try self.forceInReg(arg, .rax);
                const push = asm_mod.encodePush(.rax);
                try self.emitBytes(push.data[0..push.len]);
                debug.log(.codegen, "      PUSH (stack arg {d})", .{i});
            }
            stack_cleanup = (args.len - abi_idx) * 8;
        }

        // Collect register argument moves
        // For 9-16B structs, we need 2 consecutive registers
        const Move = struct {
            src: ?Reg, // null if value needs rematerialization
            dest: Reg,
            value: *Value,
            is_hi_half: bool, // true if this is the high half of a 2-reg struct
            struct_base_addr: ?Reg, // if set, load from this base + offset
            offset: i32,
            done: bool,
        };

        var moves: [12]Move = undefined; // 6 args * 2 = 12 max moves
        var num_moves: usize = 0;
        var abi_reg_idx: usize = 0;

        for (args) |arg| {
            if (abi_reg_idx >= 6) break; // No more register args

            const is_two_reg = self.isTwoRegStruct(arg.type_idx);

            if (is_two_reg and abi_reg_idx + 1 < 6) {
                // 9-16B struct: split into 2 registers
                // Need to load from struct's memory location
                const dest_lo = regs.AMD64.arg_regs[abi_reg_idx];
                const dest_hi = regs.AMD64.arg_regs[abi_reg_idx + 1];

                // The arg value should be a load from a local_addr
                // We'll handle this specially in the move execution
                moves[num_moves] = .{
                    .src = null,
                    .dest = dest_lo,
                    .value = arg,
                    .is_hi_half = false,
                    .struct_base_addr = null,
                    .offset = 0,
                    .done = false,
                };
                num_moves += 1;

                moves[num_moves] = .{
                    .src = null,
                    .dest = dest_hi,
                    .value = arg,
                    .is_hi_half = true,
                    .struct_base_addr = null,
                    .offset = 8,
                    .done = false,
                };
                num_moves += 1;

                abi_reg_idx += 2;
            } else if (is_two_reg) {
                // Would need 2 registers but only 1 available - push to stack
                // (already handled above in stack args)
                break;
            } else {
                // Check if this is a >16B struct (passed by pointer)
                const type_size = self.getTypeSize(arg.type_idx);
                const is_large_struct = self.isLargeStruct(arg.type_idx);

                if (is_large_struct) {
                    // >16B struct: pass pointer to struct in register
                    // The arg value is a load from local_addr - we need to pass the address
                    const dest = regs.AMD64.arg_regs[abi_reg_idx];
                    moves[num_moves] = .{
                        .src = null, // Will compute address
                        .dest = dest,
                        .value = arg,
                        .is_hi_half = false,
                        .struct_base_addr = null,
                        .offset = -1, // Marker for "pass address"
                        .done = false,
                    };
                    num_moves += 1;
                    abi_reg_idx += 1;
                    debug.log(.codegen, "      >16B struct arg: will pass ptr in {s}, size={d}", .{ dest.name(), type_size });
                } else {
                    // Single register arg
                    const dest = regs.AMD64.arg_regs[abi_reg_idx];
                    // CRITICAL FIX: When we have stack args, don't trust getRegForValue.
                    // The stack arg pushes use RAX as scratch, which can clobber any value
                    // that was in RAX. By setting src=null, we force rematerialization
                    // via forceInReg, which correctly regenerates the value.
                    const src = if (stack_cleanup > 0) null else self.getRegForValue(arg);
                    moves[num_moves] = .{
                        .src = src,
                        .dest = dest,
                        .value = arg,
                        .is_hi_half = false,
                        .struct_base_addr = null,
                        .offset = 0,
                        .done = (src != null and src.? == dest),
                    };
                    num_moves += 1;
                    abi_reg_idx += 1;
                }
            }
        }

        // Process moves that don't conflict first (dest doesn't clobber any needed source)
        var progress = true;
        while (progress) {
            progress = false;
            for (0..num_moves) |mi| {
                if (moves[mi].done) continue;

                // Check if this move's dest would clobber a source we still need
                var would_clobber = false;
                for (0..num_moves) |oi| {
                    if (moves[oi].done) continue;
                    if (moves[oi].src) |other_src| {
                        if (other_src == moves[mi].dest and oi != mi) {
                            would_clobber = true;
                            break;
                        }
                    }
                }

                if (!would_clobber) {
                    // Safe to do this move
                    const is_two_reg = self.isTwoRegStruct(moves[mi].value.type_idx);
                    const is_large_struct = self.isLargeStruct(moves[mi].value.type_idx);
                    const load_offset = moves[mi].offset;

                    if (is_large_struct and load_offset == -1) {
                        // >16B struct: pass address of struct
                        // The value should be a load from local_addr - pass the address instead
                        const struct_val = moves[mi].value;

                        if (struct_val.op == .load and struct_val.args.len > 0) {
                            const addr_val = struct_val.args[0];
                            if (addr_val.op == .local_addr) {
                                // LEA dest, [RBP - offset] to get struct address
                                const local_idx: usize = @intCast(addr_val.aux_int);
                                if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                                    const byte_offset = self.func.local_offsets[local_idx];
                                    const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                                    const base_disp: i32 = -(byte_offset + local_size);
                                    const lea = asm_mod.encodeLeaDisp32(moves[mi].dest, .rbp, base_disp);
                                    try self.emitBytes(lea.data[0..lea.len]);
                                    debug.log(.codegen, "      >16B struct arg: LEA {s}, [RBP{d}]", .{ moves[mi].dest.name(), base_disp });
                                }
                            } else {
                                // Get address value directly
                                try self.ensureInReg(addr_val, moves[mi].dest);
                                debug.log(.codegen, "      >16B struct arg: addr in {s}", .{moves[mi].dest.name()});
                            }
                        } else {
                            // Fallback
                            try self.ensureInReg(struct_val, moves[mi].dest);
                            debug.log(.codegen, "      >16B struct arg fallback: {s}", .{moves[mi].dest.name()});
                        }
                    } else if (is_two_reg) {
                        // 2-register struct: load from memory
                        // The value should be a load from a local_addr or similar
                        const struct_val = moves[mi].value;

                        // Get the struct's base address
                        // The struct_val is typically a load - we need its source address
                        if (struct_val.op == .load and struct_val.args.len > 0) {
                            const addr_val = struct_val.args[0];
                            if (addr_val.op == .local_addr) {
                                // Load from local variable: compute [RBP - offset]
                                const local_idx: usize = @intCast(addr_val.aux_int);
                                if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                                    const byte_offset = self.func.local_offsets[local_idx];
                                    const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                                    const base_disp: i32 = -(byte_offset + local_size);
                                    const final_disp: i32 = base_disp + load_offset;
                                    const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .rbp, final_disp);
                                    try self.emitBytes(enc.data[0..enc.len]);
                                    debug.log(.codegen, "      2-reg struct arg: MOV {s}, [RBP{d}] (offset {d})", .{ moves[mi].dest.name(), final_disp, load_offset });
                                }
                            } else {
                                // Other address type - get address to R10, then load
                                try self.ensureInReg(addr_val, .r10);
                                const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .r10, load_offset);
                                try self.emitBytes(enc.data[0..enc.len]);
                                debug.log(.codegen, "      2-reg struct arg: MOV {s}, [R10+{d}]", .{ moves[mi].dest.name(), load_offset });
                            }
                        } else {
                            // Fallback: try to get struct address and load
                            try self.ensureInReg(struct_val, .r10);
                            const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .r10, load_offset);
                            try self.emitBytes(enc.data[0..enc.len]);
                            debug.log(.codegen, "      2-reg struct arg fallback: MOV {s}, [R10+{d}]", .{ moves[mi].dest.name(), load_offset });
                        }
                    } else if (moves[mi].src) |src| {
                        try self.emit(3, asm_mod.encodeMovRegReg(moves[mi].dest, src));
                        debug.log(.codegen, "      arg move: {s} -> {s}", .{ src.name(), moves[mi].dest.name() });
                    } else {
                        // Rematerialize value directly to dest
                        // Use forceInReg (not ensureInReg) because the value might be in
                        // a stale register that was clobbered by stack arg pushes.
                        try self.forceInReg(moves[mi].value, moves[mi].dest);
                    }
                    moves[mi].done = true;
                    progress = true;
                }
            }
        }

        // Handle any remaining moves (no cycles for 2-reg struct loads)
        for (0..num_moves) |mi| {
            if (moves[mi].done) continue;

            const is_two_reg = self.isTwoRegStruct(moves[mi].value.type_idx);
            const is_large_struct = self.isLargeStruct(moves[mi].value.type_idx);
            const load_offset = moves[mi].offset;

            if (is_large_struct and load_offset == -1) {
                // >16B struct: pass address (same logic as above)
                const struct_val = moves[mi].value;
                if (struct_val.op == .load and struct_val.args.len > 0) {
                    const addr_val = struct_val.args[0];
                    if (addr_val.op == .local_addr) {
                        const local_idx: usize = @intCast(addr_val.aux_int);
                        if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                            const byte_offset = self.func.local_offsets[local_idx];
                            const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                            const base_disp: i32 = -(byte_offset + local_size);
                            const lea = asm_mod.encodeLeaDisp32(moves[mi].dest, .rbp, base_disp);
                            try self.emitBytes(lea.data[0..lea.len]);
                            debug.log(.codegen, "      >16B struct arg (remaining): LEA {s}, [RBP{d}]", .{ moves[mi].dest.name(), base_disp });
                        }
                    } else {
                        try self.ensureInReg(addr_val, moves[mi].dest);
                    }
                } else {
                    try self.ensureInReg(struct_val, moves[mi].dest);
                }
                moves[mi].done = true;
            } else if (is_two_reg) {
                // 2-register struct: load from memory (same logic as above)
                const struct_val = moves[mi].value;

                if (struct_val.op == .load and struct_val.args.len > 0) {
                    const addr_val = struct_val.args[0];
                    if (addr_val.op == .local_addr) {
                        const local_idx: usize = @intCast(addr_val.aux_int);
                        if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                            const byte_offset = self.func.local_offsets[local_idx];
                            const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                            const base_disp: i32 = -(byte_offset + local_size);
                            const final_disp: i32 = base_disp + load_offset;
                            const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .rbp, final_disp);
                            try self.emitBytes(enc.data[0..enc.len]);
                            debug.log(.codegen, "      2-reg struct arg (remaining): MOV {s}, [RBP{d}]", .{ moves[mi].dest.name(), final_disp });
                        }
                    } else {
                        try self.ensureInReg(addr_val, .r10);
                        const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .r10, load_offset);
                        try self.emitBytes(enc.data[0..enc.len]);
                    }
                } else {
                    try self.ensureInReg(struct_val, .r10);
                    const enc = asm_mod.encodeLoadDisp32(moves[mi].dest, .r10, load_offset);
                    try self.emitBytes(enc.data[0..enc.len]);
                }
                moves[mi].done = true;
            } else if (moves[mi].src) |start_src| {
                // Save the starting value to R11
                try self.emit(3, asm_mod.encodeMovRegReg(.r11, start_src));
                debug.log(.codegen, "      cycle: save {s} -> R11", .{start_src.name()});

                // Trace and execute the cycle
                var current_dest = start_src;
                var iterations: usize = 0;
                const max_iterations: usize = 8;

                while (iterations < max_iterations) {
                    // Find the move that writes to current_dest
                    var found_move: ?usize = null;
                    for (0..num_moves) |oi| {
                        if (moves[oi].done) continue;
                        if (moves[oi].dest == current_dest) {
                            found_move = oi;
                            break;
                        }
                    }

                    if (found_move) |move_idx| {
                        if (moves[move_idx].src) |src| {
                            // If src is the start, use R11 instead
                            const actual_src: Reg = if (src == start_src) .r11 else src;
                            try self.emit(3, asm_mod.encodeMovRegReg(moves[move_idx].dest, actual_src));
                            debug.log(.codegen, "      cycle: {s} -> {s}", .{ actual_src.name(), moves[move_idx].dest.name() });
                            current_dest = src;
                        } else {
                            try self.ensureInReg(moves[move_idx].value, moves[move_idx].dest);
                        }
                        moves[move_idx].done = true;
                    } else {
                        break;
                    }

                    if (current_dest == start_src) break;
                    iterations += 1;
                }

                // Complete the starting move: R11 -> dest
                try self.emit(3, asm_mod.encodeMovRegReg(moves[mi].dest, .r11));
                debug.log(.codegen, "      cycle: R11 -> {s}", .{moves[mi].dest.name()});
                moves[mi].done = true;
            } else {
                // No source register - just rematerialize
                try self.ensureInReg(moves[mi].value, moves[mi].dest);
                moves[mi].done = true;
            }
        }

        return stack_cleanup;
    }

    /// Setup call arguments for calls with hidden return pointer.
    /// RDI is already set up with the hidden return address.
    /// Args go in RSI, RDX, RCX, R8, R9 (only 5 register args, then stack).
    fn setupCallArgsWithHiddenRet(self: *AMD64CodeGen, args: []*Value) !usize {
        return self.setupCallArgsWithHiddenRetSafe(args, false);
    }

    /// Same as setupCallArgsWithHiddenRet, but if rdi_saved is true,
    /// any source register that was RDI should use R11 instead (RDI was saved there).
    fn setupCallArgsWithHiddenRetSafe(self: *AMD64CodeGen, args: []*Value, rdi_saved: bool) !usize {
        if (args.len == 0) return 0;

        debug.log(.codegen, "      setupCallArgsWithHiddenRetSafe: {d} args, rdi_saved={}", .{ args.len, rdi_saved });
        for (args, 0..) |arg, i| {
            if (self.getRegForValue(arg)) |reg| {
                debug.log(.codegen, "        arg[{d}] v{d} op={s} in {s}", .{ i, arg.id, @tagName(arg.op), reg.name() });
            } else {
                debug.log(.codegen, "        arg[{d}] v{d} op={s} NOT in reg", .{ i, arg.id, @tagName(arg.op) });
            }
        }

        // With hidden return, only 5 register args available (RDI is taken)
        const num_stack_args: usize = if (args.len > 5) args.len - 5 else 0;
        var stack_cleanup: usize = 0;

        if (num_stack_args > 0) {
            // Push stack arguments in reverse order
            // IMPORTANT: Always use forceInReg to rematerialize each value before pushing.
            // We cannot trust getRegForValue here because registers may have been reused.
            var i: usize = args.len;
            while (i > 5) {
                i -= 1;
                const arg = args[i];
                try self.forceInReg(arg, .rax);
                const push = asm_mod.encodePush(.rax);
                try self.emitBytes(push.data[0..push.len]);
                debug.log(.codegen, "      PUSH (stack arg {d})", .{i});
            }
            stack_cleanup = num_stack_args * 8;
        }

        // Collect register argument moves (skip RDI, use RSI through R9)
        const max_reg_args = @min(args.len, 5);
        const Move = struct {
            src: ?Reg,
            dest: Reg,
            value: *Value,
            done: bool,
        };

        var moves: [5]Move = undefined;
        var num_moves: usize = 0;

        for (args[0..max_reg_args], 0..) |arg, i| {
            // arg_regs[1..] = RSI, RDX, RCX, R8, R9
            const dest = regs.AMD64.arg_regs[i + 1];
            var src = self.getRegForValue(arg);
            // If RDI was saved to R11, substitute R11 for any RDI source
            if (rdi_saved and src != null and src.? == .rdi) {
                src = .r11;
            }
            moves[num_moves] = .{
                .src = src,
                .dest = dest,
                .value = arg,
                .done = (src != null and src.? == dest),
            };
            num_moves += 1;
        }

        // Process moves that don't conflict first
        var progress = true;
        while (progress) {
            progress = false;
            for (0..num_moves) |mi| {
                if (moves[mi].done) continue;

                var would_clobber = false;
                for (0..num_moves) |oi| {
                    if (moves[oi].done) continue;
                    if (moves[oi].src) |other_src| {
                        // Don't count R11 as conflicting with RDI if we saved RDI there
                        const actual_src = if (rdi_saved and other_src == .r11) Reg.rdi else other_src;
                        if (actual_src == moves[mi].dest and oi != mi) {
                            would_clobber = true;
                            break;
                        }
                    }
                }

                if (!would_clobber) {
                    if (moves[mi].src) |src| {
                        try self.emit(3, asm_mod.encodeMovRegReg(moves[mi].dest, src));
                        debug.log(.codegen, "      arg move (hidden ret): {s} -> {s}", .{ src.name(), moves[mi].dest.name() });
                    } else {
                        try self.ensureInReg(moves[mi].value, moves[mi].dest);
                    }
                    moves[mi].done = true;
                    progress = true;
                }
            }
        }

        // Handle cycles using R11
        for (0..num_moves) |mi| {
            if (moves[mi].done) continue;

            if (moves[mi].src) |start_src| {
                try self.emit(3, asm_mod.encodeMovRegReg(.r11, start_src));
                debug.log(.codegen, "      cycle (hidden ret): save {s} -> R11", .{start_src.name()});

                var current_dest = start_src;
                var iterations: usize = 0;
                const max_iterations: usize = 8;

                while (iterations < max_iterations) {
                    var found_move: ?usize = null;
                    for (0..num_moves) |oi| {
                        if (moves[oi].done) continue;
                        if (moves[oi].dest == current_dest) {
                            found_move = oi;
                            break;
                        }
                    }

                    if (found_move) |move_idx| {
                        if (moves[move_idx].src) |src| {
                            const actual_src: Reg = if (src == start_src) .r11 else src;
                            try self.emit(3, asm_mod.encodeMovRegReg(moves[move_idx].dest, actual_src));
                            debug.log(.codegen, "      cycle (hidden ret): {s} -> {s}", .{ actual_src.name(), moves[move_idx].dest.name() });
                            current_dest = src;
                        } else {
                            try self.ensureInReg(moves[move_idx].value, moves[move_idx].dest);
                        }
                        moves[move_idx].done = true;
                    } else {
                        break;
                    }

                    if (current_dest == start_src) break;
                    iterations += 1;
                }

                try self.emit(3, asm_mod.encodeMovRegReg(moves[mi].dest, .r11));
                debug.log(.codegen, "      cycle (hidden ret): R11 -> {s}", .{moves[mi].dest.name()});
                moves[mi].done = true;
            } else {
                try self.ensureInReg(moves[mi].value, moves[mi].dest);
                moves[mi].done = true;
            }
        }

        return stack_cleanup;
    }

    // ========================================================================
    // Code Generation
    // ========================================================================

    /// Generate binary code for a function.
    pub fn generateBinary(self: *AMD64CodeGen, f: *const Func, name: []const u8) !void {
        self.func = f;
        const start_offset = self.offset();

        // CRITICAL: Clear per-function state before generating code
        // Block IDs are per-function (all functions start with block 0)
        // so we must clear the mapping to avoid cross-function confusion
        self.block_offsets.clearRetainingCapacity();
        self.branch_fixups.clearRetainingCapacity();
        self.hidden_ret_offsets.clearRetainingCapacity();
        self.has_hidden_return = false;
        self.hidden_ret_space_needed = 0;

        debug.log(.codegen, "Generating AMD64 code for '{s}'", .{name});
        debug.log(.codegen, "  Stack frame: {d} bytes", .{self.frame_size});

        // AMD64 System V ABI: Check if this function returns >16B (needs hidden return pointer in RDI)
        if (self.type_reg) |type_reg| {
            const func_type = type_reg.get(f.type_idx);
            if (func_type == .func) {
                const ret_type_idx = func_type.func.return_type;
                const ret_size = self.getTypeSize(ret_type_idx);
                if (ret_size > 16) {
                    self.has_hidden_return = true;
                    debug.log(.codegen, "  Function returns >16B ({d}B), using hidden return via RDI (saved to stack)", .{ret_size});
                }
            }
        }
        // Fallback: scan return blocks for return value type
        if (!self.has_hidden_return) {
            for (f.blocks.items) |block| {
                if (block.kind == .ret and block.numControls() > 0) {
                    const ret_val = block.controlValues()[0];
                    const ret_size = self.getTypeSize(ret_val.type_idx);
                    if (ret_size > 16) {
                        self.has_hidden_return = true;
                        debug.log(.codegen, "  Function returns >16B ({d}B) [from ret block], using hidden return via RDI", .{ret_size});
                        break;
                    }
                }
            }
        }

        // Pre-allocate space for calls that return >16B via hidden pointer
        var cur_hidden_offset: u32 = 0;
        for (f.blocks.items) |block| {
            for (block.values.items) |value| {
                if (value.op == .static_call or value.op == .closure_call) {
                    const ret_size: u32 = if (value.aux_call) |aux_call|
                        aux_call.hiddenReturnSize()
                    else
                        self.getTypeSize(value.type_idx);

                    var uses_hidden = ret_size > 16;
                    if (value.aux_call) |aux_call| {
                        if (aux_call.usesHiddenReturn()) {
                            uses_hidden = true;
                        }
                    }

                    if (uses_hidden and ret_size > 0) {
                        const aligned_size = (ret_size + 15) & ~@as(u32, 15);
                        try self.hidden_ret_offsets.put(self.allocator, value, cur_hidden_offset);
                        debug.log(.codegen, "  Pre-allocated hidden return for call v{d}: offset={d}, size={d}", .{ value.id, cur_hidden_offset, aligned_size });
                        cur_hidden_offset += aligned_size;
                    }
                }
            }
        }
        self.hidden_ret_space_needed = cur_hidden_offset;
        self.hidden_ret_frame_offset = self.frame_size;
        if (cur_hidden_offset > 0) {
            self.frame_size += cur_hidden_offset;
            debug.log(.codegen, "  Added {d}B hidden return space, new frame_size={d}", .{ cur_hidden_offset, self.frame_size });
        }

        // Save space for hidden return pointer when this function returns >16B
        // IMPORTANT: hidden_ret_ptr_offset must be BEYOND existing locals
        // Locals use offsets [8, frame_size] from RBP, so hidden ptr goes at frame_size + 8
        if (self.has_hidden_return) {
            self.frame_size += 8; // Add 8 bytes to ensure we're past locals
            self.hidden_ret_ptr_offset = self.frame_size;
            self.frame_size += 8; // Add 8 more for the pointer itself (16 total for alignment)
            debug.log(.codegen, "  Added 16B for hidden return ptr at offset {d}, new frame_size={d}", .{ self.hidden_ret_ptr_offset, self.frame_size });
        }

        // Add symbol (no underscore prefix for Linux/ELF)
        try self.symbols.append(self.allocator, .{
            .name = name,
            .value = start_offset,
            .section = 1, // .text section
            .binding = elf.STB_GLOBAL,
            .sym_type = elf.STT_FUNC,
        });

        // Emit prologue
        debug.log(.codegen, "  Emitting prologue", .{});
        try self.emitPrologue();

        // Generate code for each block
        for (f.blocks.items) |block| {
            try self.generateBlockBinary(block);
        }

        // Apply branch fixups
        try self.applyBranchFixups();

        debug.log(.codegen, "  Function '{s}' done, code size: {d} bytes", .{ name, self.offset() - start_offset });
    }

    /// Emit function prologue.
    fn emitPrologue(self: *AMD64CodeGen) !void {
        // PUSH RBP
        const push_rbp = asm_mod.encodePush(.rbp);
        try self.emitBytes(push_rbp.data[0..push_rbp.len]);

        // MOV RBP, RSP
        try self.emit(3, asm_mod.encodeMovRegReg(.rbp, .rsp));

        // SUB RSP, frame_size (if needed)
        if (self.frame_size > 0) {
            const aligned_frame = (self.frame_size + 15) & ~@as(u32, 15); // 16-byte align
            if (aligned_frame <= 127) {
                try self.emit(4, asm_mod.encodeSubRegImm8(.rsp, @intCast(aligned_frame)));
            } else {
                try self.emit(7, asm_mod.encodeSubRegImm32(.rsp, @intCast(aligned_frame)));
            }
        }

        // AMD64 System V: Save hidden return pointer (RDI) to stack for >16B returns
        // Caller passes return location in RDI; we save it so we can store result there on return
        if (self.has_hidden_return) {
            // MOV [RBP - hidden_ret_ptr_offset], RDI
            const neg_offset: i32 = -@as(i32, @intCast(self.hidden_ret_ptr_offset));
            try self.emitStoreRegToFrame(.rdi, neg_offset);
            debug.log(.codegen, "  Saved RDI (hidden return ptr) to [RBP{d}]", .{neg_offset});
        }
    }

    /// Emit function epilogue.
    fn emitEpilogue(self: *AMD64CodeGen) !void {
        // MOV RSP, RBP (restore stack pointer)
        try self.emit(3, asm_mod.encodeMovRegReg(.rsp, .rbp));

        // POP RBP
        const pop_rbp = asm_mod.encodePop(.rbp);
        try self.emitBytes(pop_rbp.data[0..pop_rbp.len]);

        // RET
        try self.emit(1, asm_mod.encodeRet());
    }

    /// Apply branch fixups.
    fn applyBranchFixups(self: *AMD64CodeGen) !void {
        for (self.branch_fixups.items) |fixup| {
            const target_offset = self.block_offsets.get(fixup.target_block_id) orelse continue;

            // Calculate relative offset from end of branch instruction
            const branch_end: i32 = @as(i32, @intCast(fixup.code_offset)) + 4; // rel32 is 4 bytes
            const rel32: i32 = @as(i32, @intCast(target_offset)) - branch_end;

            // Patch the rel32
            std.mem.writeInt(i32, self.code.items[fixup.code_offset..][0..4], rel32, .little);
        }
    }

    /// Generate code for a basic block.
    fn generateBlockBinary(self: *AMD64CodeGen, block: *const Block) !void {
        // Record block offset for branch fixups
        try self.block_offsets.put(self.allocator, block.id, self.offset());

        debug.log(.codegen, "  Block b{d} at offset {d}", .{ block.id, self.offset() });

        // BUG-020 FIX: Dead code elimination before generating code.
        // Regalloc may have created store_reg values that increment uses of dead values,
        // making them appear non-dead. We need to work backwards to fix this.
        // First pass: find and mark truly dead values by propagating dead uses backwards
        var i: usize = block.values.items.len;
        while (i > 0) {
            i -= 1;
            const value = block.values.items[i];
            if (value.uses == 0) {
                const has_side_effects = switch (value.op) {
                    .store, .move, .static_call, .closure_call, .load_reg => true,
                    else => false,
                };
                if (!has_side_effects) {
                    // Dead value - decrement uses of its arguments
                    for (value.args) |arg| {
                        if (arg.uses > 0) arg.uses -= 1;
                    }
                }
            }
        }

        // Second pass: generate code, skipping dead values
        for (block.values.items) |value| {
            // Skip dead values (uses == 0) unless they have side effects
            // Note: .copy is included because regalloc creates copy values for register moves
            // that may not have their use counts updated
            if (value.uses == 0) {
                const has_side_effects = switch (value.op) {
                    .store, .move, .static_call, .closure_call, .load_reg, .copy => true,
                    else => false,
                };
                if (!has_side_effects) {
                    debug.log(.codegen, "    v{d} = {s} (skipped - dead value)", .{ value.id, @tagName(value.op) });
                    continue;
                }
            }
            try self.generateValueBinary(value);
        }

        // Handle block terminator
        switch (block.kind) {
            .ret => {
                // Return block: move control value to RAX (and RDX for slices) and return
                if (block.controlValues().len > 0) {
                    const ret_val = block.controlValues()[0];

                    // AMD64 System V: Handle >16B return via hidden pointer (saved RDI)
                    if (self.has_hidden_return) {
                        debug.log(.codegen, "      ret_val.op = {s}, ret_val.id = v{d}", .{ @tagName(ret_val.op), ret_val.id });

                        // Get source address of the struct data
                        var src_addr_reg: Reg = .r10; // Default scratch register
                        if (ret_val.op == .local_addr) {
                            // Local variable - get its stack address
                            const slot: i64 = ret_val.aux_int;
                            const slot_offset: i32 = -@as(i32, @intCast((slot + 1) * 8));
                            // LEA src_addr_reg, [RBP + slot_offset]
                            try self.emitLeaFromFrame(src_addr_reg, slot_offset);
                            debug.log(.codegen, "      ret local_addr: slot {d} -> {s}", .{ slot, src_addr_reg.name() });
                        } else if (ret_val.op == .load and ret_val.args.len > 0) {
                            // Load operation - source address is in the load's argument
                            const src_addr = ret_val.args[0];
                            if (src_addr.op == .local_addr) {
                                // Local address - compute from slot
                                // Same formula as local_addr handler: disp = -(byte_offset + local_size)
                                const local_idx: u32 = @intCast(src_addr.aux_int);
                                if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                                    const byte_offset: i32 = @intCast(self.func.local_offsets[local_idx]);
                                    const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                                    const disp: i32 = -(byte_offset + local_size);
                                    try self.emitLeaFromFrame(src_addr_reg, disp);
                                    debug.log(.codegen, "      ret load: source local_addr {d} at [RBP{d}]", .{ local_idx, disp });
                                }
                            } else {
                                // Other address - get from register
                                const addr_reg = self.getRegForValue(src_addr) orelse blk: {
                                    try self.ensureInReg(src_addr, src_addr_reg);
                                    break :blk src_addr_reg;
                                };
                                if (addr_reg != src_addr_reg) {
                                    try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, addr_reg));
                                }
                            }
                        } else if (ret_val.op == .load_reg) {
                            // The value was loaded from somewhere - get the address from regalloc
                            const ret_reg = self.getRegForValue(ret_val) orelse blk: {
                                try self.ensureInReg(ret_val, src_addr_reg);
                                break :blk src_addr_reg;
                            };
                            if (ret_reg != src_addr_reg) {
                                try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, ret_reg));
                            }
                        } else if (ret_val.op == .static_call or ret_val.op == .closure_call) {
                            // Call result - check if it used hidden return
                            if (self.hidden_ret_offsets.get(ret_val)) |rel_offset| {
                                // Result is at frame location - compute address
                                // Point to the LOW end of the allocated space (highest frame offset)
                                const ret_size_inner = self.getTypeSize(ret_val.type_idx);
                                const aligned_size = (ret_size_inner + 15) & ~@as(u32, 15);
                                const frame_offset: i32 = @intCast(self.hidden_ret_frame_offset + rel_offset + aligned_size);
                                const neg_offset: i32 = -frame_offset;
                                // LEA R10, [RBP - frame_offset]
                                try self.emitLeaFromFrame(.r10, neg_offset);
                                src_addr_reg = .r10;
                                debug.log(.codegen, "      ret static_call: result at frame offset {d}", .{frame_offset});
                            } else {
                                // Normal call - result in register
                                const ret_reg = self.getRegForValue(ret_val) orelse .rax;
                                if (ret_reg != src_addr_reg) {
                                    try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, ret_reg));
                                }
                            }
                        } else {
                            // Other ops - try to get from register
                            const ret_reg = self.getRegForValue(ret_val) orelse blk: {
                                try self.ensureInReg(ret_val, src_addr_reg);
                                break :blk src_addr_reg;
                            };
                            if (ret_reg != src_addr_reg) {
                                try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, ret_reg));
                            }
                        }

                        // Get return type size
                        const ret_size = self.getTypeSize(ret_val.type_idx);

                        // Load hidden return pointer from stack into R11 (temp)
                        const dest_ptr_reg: Reg = .r11;
                        const neg_ptr_offset: i32 = -@as(i32, @intCast(self.hidden_ret_ptr_offset));
                        try self.emitLoadRegFromFrame(dest_ptr_reg, neg_ptr_offset);
                        debug.log(.codegen, "      ret hidden: loaded dest ptr from [RBP{d}] to {s}", .{ neg_ptr_offset, dest_ptr_reg.name() });
                        debug.log(.codegen, "      ret hidden: copying {d}B from [{s}] to [{s}]", .{ ret_size, src_addr_reg.name(), dest_ptr_reg.name() });

                        // Copy data in 8-byte chunks using MOV
                        // Use RCX as scratch (dest_ptr_reg is R11, so can't use R11 here)
                        var copy_off: u32 = 0;
                        while (copy_off < ret_size) : (copy_off += 8) {
                            // MOV RCX, [src + copy_off]
                            const src_enc = asm_mod.encodeMovRegMemDisp(.rcx, src_addr_reg, @intCast(copy_off));
                            try self.code.appendSlice(self.allocator, src_enc.data[0..src_enc.len]);
                            // MOV [dest + copy_off], RCX
                            const dst_enc = asm_mod.encodeMovMemReg(dest_ptr_reg, @intCast(copy_off), .rcx);
                            try self.code.appendSlice(self.allocator, dst_enc.data[0..dst_enc.len]);
                        }

                        // Return destination pointer in RAX
                        try self.emit(3, asm_mod.encodeMovRegReg(.rax, dest_ptr_reg));
                        debug.log(.codegen, "      ret hidden: returning dest ptr in RAX", .{});
                    } else if (ret_val.op == .slice_make and ret_val.args.len >= 2) {
                        // Handle slice returns: ptr in RAX, len in RDX
                        const ptr_val = ret_val.args[0];
                        const len_val = ret_val.args[1];

                        const ptr_reg = self.getRegForValue(ptr_val) orelse blk: {
                            try self.ensureInReg(ptr_val, .rax);
                            break :blk Reg.rax;
                        };
                        const len_reg = self.getRegForValue(len_val) orelse blk: {
                            try self.ensureInReg(len_val, .rdx);
                            break :blk Reg.rdx;
                        };

                        // CRITICAL: Handle register conflicts to avoid clobbering
                        // If len is in RAX and we need to move ptr to RAX, save len first
                        // If ptr is in RDX and we need to move len to RDX, save ptr first
                        if (len_reg == .rax and ptr_reg != .rax) {
                            // len is in RAX, move it to RDX first before ptr clobbers RAX
                            try self.emit(3, asm_mod.encodeMovRegReg(.rdx, .rax));
                            try self.emit(3, asm_mod.encodeMovRegReg(.rax, ptr_reg));
                        } else if (ptr_reg == .rdx and len_reg != .rdx) {
                            // ptr is in RDX, move it to RAX first before len clobbers RDX
                            try self.emit(3, asm_mod.encodeMovRegReg(.rax, .rdx));
                            try self.emit(3, asm_mod.encodeMovRegReg(.rdx, len_reg));
                        } else {
                            // No conflict - can do either order
                            if (ptr_reg != .rax) {
                                try self.emit(3, asm_mod.encodeMovRegReg(.rax, ptr_reg));
                            }
                            if (len_reg != .rdx) {
                                try self.emit(3, asm_mod.encodeMovRegReg(.rdx, len_reg));
                            }
                        }
                        debug.log(.codegen, "      ret slice: ptr={s}->RAX, len={s}->RDX", .{ ptr_reg.name(), len_reg.name() });
                    } else if (ret_val.op == .string_make and ret_val.args.len >= 2) {
                        // Handle string_make returns: ptr in RAX, len in RDX
                        // string_make(ptr, len) aggregates components from const_string/select_n
                        const ptr_val = ret_val.args[0];
                        const len_val = ret_val.args[1];

                        const ptr_reg = self.getRegForValue(ptr_val) orelse blk: {
                            try self.ensureInReg(ptr_val, .rax);
                            break :blk Reg.rax;
                        };
                        const len_reg = self.getRegForValue(len_val) orelse blk: {
                            try self.ensureInReg(len_val, .rdx);
                            break :blk Reg.rdx;
                        };

                        // CRITICAL: Handle register conflicts to avoid clobbering
                        // If len is in RAX and we need to move ptr to RAX, save len first
                        // If ptr is in RDX and we need to move len to RDX, save ptr first
                        if (len_reg == .rax and ptr_reg != .rax) {
                            // len is in RAX, move it to RDX first before ptr clobbers RAX
                            try self.emit(3, asm_mod.encodeMovRegReg(.rdx, .rax));
                            try self.emit(3, asm_mod.encodeMovRegReg(.rax, ptr_reg));
                        } else if (ptr_reg == .rdx and len_reg != .rdx) {
                            // ptr is in RDX, move it to RAX first before len clobbers RDX
                            try self.emit(3, asm_mod.encodeMovRegReg(.rax, .rdx));
                            try self.emit(3, asm_mod.encodeMovRegReg(.rdx, len_reg));
                        } else {
                            // No conflict - can do either order
                            if (ptr_reg != .rax) {
                                try self.emit(3, asm_mod.encodeMovRegReg(.rax, ptr_reg));
                            }
                            if (len_reg != .rdx) {
                                try self.emit(3, asm_mod.encodeMovRegReg(.rdx, len_reg));
                            }
                        }
                        debug.log(.codegen, "      ret string_make: ptr={s}->RAX, len={s}->RDX", .{ ptr_reg.name(), len_reg.name() });
                    } else if (ret_val.op == .const_string) {
                        // String literal return: ptr in RAX, len in RDX
                        // The const_string codegen already put ptr in a register
                        const ptr_reg = self.getRegForValue(ret_val) orelse blk: {
                            try self.ensureInReg(ret_val, .rax);
                            break :blk Reg.rax;
                        };
                        if (ptr_reg != .rax) {
                            try self.emit(3, asm_mod.encodeMovRegReg(.rax, ptr_reg));
                        }
                        // Get string length from string_literals
                        const string_index: usize = @intCast(ret_val.aux_int);
                        const str_len: i64 = if (string_index < self.func.string_literals.len)
                            @intCast(self.func.string_literals[string_index].len)
                        else
                            0;
                        // Emit MOV RDX, #len
                        try self.emitLoadImmediate(.rdx, str_len);
                        debug.log(.codegen, "      ret const_string: ptr={s}->RAX, len={d}->RDX", .{ ptr_reg.name(), str_len });
                    } else if (ret_val.op == .static_call) {
                        // Call result that returns a string: already in RAX/RDX
                        // Just ensure they stay there (no-op, but log it)
                        debug.log(.codegen, "      ret static_call: result already in RAX/RDX", .{});
                    } else {
                        // BUG-078 FIX: Handle 9-16 byte struct returns
                        // These need to return in RAX:RDX (first 8 bytes in RAX, second 8 in RDX)
                        const ret_size = self.getTypeSize(ret_val.type_idx);
                        if (ret_size > 8 and ret_size <= 16) {
                            // Get source address of struct
                            var src_addr_reg: Reg = .r11;
                            if (ret_val.op == .load and ret_val.args.len > 0) {
                                const src_addr = ret_val.args[0];
                                if (src_addr.op == .local_addr) {
                                    const local_idx: u32 = @intCast(src_addr.aux_int);
                                    if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                                        const byte_offset: i32 = @intCast(self.func.local_offsets[local_idx]);
                                        const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                                        const disp: i32 = -(byte_offset + local_size);
                                        try self.emitLeaFromFrame(src_addr_reg, disp);
                                    }
                                } else {
                                    const addr_reg = self.getRegForValue(src_addr) orelse blk: {
                                        try self.ensureInReg(src_addr, src_addr_reg);
                                        break :blk src_addr_reg;
                                    };
                                    if (addr_reg != src_addr_reg) {
                                        try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, addr_reg));
                                    }
                                }
                            } else {
                                // Try to get value address from register
                                const val_reg = self.getRegForValue(ret_val) orelse blk: {
                                    try self.ensureInReg(ret_val, src_addr_reg);
                                    break :blk src_addr_reg;
                                };
                                if (val_reg != src_addr_reg) {
                                    try self.emit(3, asm_mod.encodeMovRegReg(src_addr_reg, val_reg));
                                }
                            }
                            // Load first 8 bytes into RAX
                            const load_rax = asm_mod.encodeMovRegMemDisp(.rax, src_addr_reg, 0);
                            try self.code.appendSlice(self.allocator, load_rax.data[0..load_rax.len]);
                            // Load second 8 bytes into RDX
                            const load_rdx = asm_mod.encodeMovRegMemDisp(.rdx, src_addr_reg, 8);
                            try self.code.appendSlice(self.allocator, load_rdx.data[0..load_rdx.len]);
                            debug.log(.codegen, "      ret {d}B struct: [{s}] -> RAX:RDX", .{ ret_size, src_addr_reg.name() });
                        } else {
                            try self.moveToRAX(ret_val);
                        }
                    }
                }
                try self.emitEpilogue();
            },
            .if_ => {
                // Conditional branch
                if (block.succs.len >= 2) {
                    const cond_val = block.controlValues()[0];
                    const cond_reg = self.getRegForValue(cond_val) orelse blk: {
                        try self.ensureInReg(cond_val, .rax);
                        break :blk Reg.rax;
                    };

                    const then_block = block.succs[0].b;
                    const else_block = block.succs[1].b;

                    // Emit phi moves for the else branch (taken when JE succeeds)
                    try self.emitPhiMoves(block, else_block);

                    // CMP cond_reg, 0
                    try self.emit(4, asm_mod.encodeCmpRegImm8(cond_reg, 0));

                    // JE else_block (jump if zero/false)
                    const je_offset = self.offset();
                    try self.emit(6, asm_mod.encodeJccRel32(.e, 0)); // Placeholder
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = @intCast(je_offset + 2), // Skip 0F 84
                        .target_block_id = else_block.id,
                        .is_jcc = true,
                    });

                    // Emit phi moves for the then branch
                    try self.emitPhiMoves(block, then_block);

                    // Fall through or jump to then_block
                    // If then_block is not the next block, emit JMP
                    const jmp_offset = self.offset();
                    try self.emit(5, asm_mod.encodeJmpRel32(0)); // Placeholder
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = @intCast(jmp_offset + 1), // Skip E9
                        .target_block_id = then_block.id,
                        .is_jcc = false,
                    });
                }
            },
            .plain => {
                // Unconditional branch to successor
                if (block.succs.len > 0) {
                    const succ = block.succs[0].b;

                    // Emit phi moves before branching
                    try self.emitPhiMoves(block, succ);

                    const jmp_offset = self.offset();
                    try self.emit(5, asm_mod.encodeJmpRel32(0)); // Placeholder
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = @intCast(jmp_offset + 1),
                        .target_block_id = succ.id,
                        .is_jcc = false,
                    });
                }
            },
            else => {},
        }
    }

    fn generateValueBinary(self: *AMD64CodeGen, value: *const Value) !void {
        debug.log(.codegen, "    v{d}: {s}", .{ value.id, @tagName(value.op) });

        // Skip evicted rematerializeable values
        switch (value.op) {
            .const_int, .const_64, .const_bool, .local_addr => if (value.regOrNull() == null) return,
            else => {},
        }

        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(self.getDestRegForValue(value), value.aux_int),
            .const_bool => try self.emitLoadImmediate(self.getDestRegForValue(value), if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emit(3, asm_mod.encodeXorRegReg(self.getDestRegForValue(value), self.getDestRegForValue(value))),

            .add => {
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], .rax);
                        break :blk Reg.rax;
                    };
                    const op2_scratch: Reg = if (op1_reg == .rcx) .rdx else .rcx;
                    const op2_reg = self.getRegForValue(args[1]) orelse blk: {
                        try self.ensureInReg(args[1], op2_scratch);
                        break :blk op2_scratch;
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg == op1_reg) {
                        try self.emit(3, asm_mod.encodeAddRegReg(dest_reg, op2_reg));
                    } else if (dest_reg == op2_reg) {
                        try self.emit(3, asm_mod.encodeAddRegReg(dest_reg, op1_reg));
                    } else {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op1_reg));
                        try self.emit(3, asm_mod.encodeAddRegReg(dest_reg, op2_reg));
                    }
                }
            },

            .sub => {
                const args = value.args;
                if (args.len >= 2) {
                    const dest_reg = self.getDestRegForValue(value);
                    const op2_scratch: Reg = if (dest_reg == .rcx) .rdx else .rcx;
                    const op2_reg = self.getRegForValue(args[1]) orelse blk: {
                        try self.ensureInReg(args[1], op2_scratch);
                        break :blk op2_scratch;
                    };
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], .rax);
                        break :blk Reg.rax;
                    };
                    if (dest_reg == op1_reg) {
                        try self.emit(3, asm_mod.encodeSubRegReg(dest_reg, op2_reg));
                    } else if (dest_reg == op2_reg) {
                        // SUB not commutative: NEG + ADD
                        try self.emit(3, asm_mod.encodeNegReg(dest_reg));
                        try self.emit(3, asm_mod.encodeAddRegReg(dest_reg, op1_reg));
                    } else {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op1_reg));
                        try self.emit(3, asm_mod.encodeSubRegReg(dest_reg, op2_reg));
                    }
                }
            },

            .mul => {
                const args = value.args;
                if (args.len >= 2) {
                    const dest_reg = self.getDestRegForValue(value);
                    try self.forceInReg(args[0], dest_reg);
                    const op2_scratch: Reg = if (dest_reg == .r10) .r11 else .r10;
                    try self.forceInReg(args[1], op2_scratch);
                    try self.emit(4, asm_mod.encodeImulRegReg(dest_reg, op2_scratch));
                }
            },

            .div => {
                const args = value.args;
                if (args.len >= 2) {
                    if (isCallOrCopyOfCall(args[1])) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r11, .rax));
                        try self.forceInReg(args[0], .rax);
                    } else {
                        try self.forceInReg(args[0], .rax);
                        try self.forceInReg(args[1], .r11);
                    }
                    try self.emit(2, asm_mod.encodeCqo());
                    try self.emit(3, asm_mod.encodeIdivReg(.r11));
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != .rax) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .rax));
                }
            },

            .mod => {
                const args = value.args;
                if (args.len >= 2) {
                    if (isCallOrCopyOfCall(args[1])) {
                        try self.emit(3, asm_mod.encodeMovRegReg(.r11, .rax));
                        try self.forceInReg(args[0], .rax);
                    } else {
                        try self.forceInReg(args[0], .rax);
                        try self.forceInReg(args[1], .r11);
                    }
                    try self.emit(2, asm_mod.encodeCqo());
                    try self.emit(3, asm_mod.encodeIdivReg(.r11));
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != .rdx) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .rdx));
                }
            },

            .add_ptr => {
                const args = value.args;
                if (args.len >= 2) {
                    const dest_reg = self.getDestRegForValue(value);
                    try self.forceInReg(args[0], .r8);
                    try self.forceInReg(args[1], .r9);
                    const lea = asm_mod.encodeLeaBaseIndex(dest_reg, .r8, .r9);
                    try self.emitBytes(lea.data[0..lea.len]);
                }
            },

            .sub_ptr => {
                const args = value.args;
                if (args.len >= 2) {
                    const dest_reg = self.getDestRegForValue(value);
                    try self.forceInReg(args[0], .r8);
                    try self.forceInReg(args[1], .r9);
                    if (dest_reg == .r9) {
                        try self.emit(3, asm_mod.encodeSubRegReg(.r8, .r9));
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .r8));
                    } else {
                        if (dest_reg != .r8) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .r8));
                        try self.emit(3, asm_mod.encodeSubRegReg(dest_reg, .r9));
                    }
                }
            },

            .eq, .ne, .lt, .le, .gt, .ge => {
                if (value.args.len >= 2) {
                    // Save r12/r13 (callee-saved)
                    try self.emitPushPop(.r12, true);
                    try self.emitPushPop(.r13, true);
                    try self.forceInReg(value.args[1], .r13);
                    try self.forceInReg(value.args[0], .r12);
                    const dest_reg = self.getDestRegForValue(value);
                    try self.emit(3, asm_mod.encodeCmpRegReg(.r12, .r13));
                    const cond: asm_mod.Cond = switch (value.op) {
                        .eq => .e, .ne => .ne, .lt => .l, .le => .le, .gt => .g, .ge => .ge, else => .e,
                    };
                    try self.emit(4, asm_mod.encodeSetcc(cond, dest_reg));
                    try self.emit(4, asm_mod.encodeMovzxRegReg8(dest_reg, dest_reg));
                    try self.emitPushPop(.r13, false);
                    try self.emitPushPop(.r12, false);
                }
            },

            // cond_select(cond, then_val, else_val) -> if cond != 0 then then_val else else_val
            .cond_select => {
                if (value.args.len >= 3) {
                    const cond_val = value.args[0];
                    const then_val = value.args[1];
                    const else_val = value.args[2];
                    const dest_reg = self.getDestRegForValue(value);

                    // Get registers for operands, using scratch regs to avoid conflicts
                    const else_reg = self.getRegForValue(else_val) orelse blk: {
                        try self.ensureInReg(else_val, .r10);
                        break :blk Reg.r10;
                    };
                    const then_reg = self.getRegForValue(then_val) orelse blk: {
                        const scratch: Reg = if (else_reg == .r11) .r10 else .r11;
                        try self.ensureInReg(then_val, scratch);
                        break :blk scratch;
                    };
                    const cond_reg = self.getRegForValue(cond_val) orelse blk: {
                        var scratch: Reg = .rcx;
                        if (scratch == then_reg or scratch == else_reg) scratch = .rax;
                        if (scratch == then_reg or scratch == else_reg) scratch = .rdx;
                        try self.ensureInReg(cond_val, scratch);
                        break :blk scratch;
                    };

                    // Move else_val to dest (default value)
                    if (dest_reg != else_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, else_reg));
                    }

                    // TEST cond, cond (sets ZF if cond is 0)
                    try self.emit(3, asm_mod.encodeTestRegReg(cond_reg, cond_reg));

                    // CMOVNE dest, then_val (if cond != 0, use then_val)
                    try self.emit(4, asm_mod.encodeCmovcc(.ne, dest_reg, then_reg));

                    debug.log(.codegen, "      cond_select: TEST {s}; CMOVNE {s}, {s}", .{ cond_reg.name(), dest_reg.name(), then_reg.name() });
                }
            },

            // Sign extension operations
            // Sign extension operations
            .sign_ext8to16, .sign_ext8to32, .sign_ext8to64, .sign_ext16to32, .sign_ext16to64, .sign_ext32to64 => {
                if (value.args.len > 0) {
                    const dest_reg = self.getDestRegForValue(value);
                    const src_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], dest_reg);
                        break :blk dest_reg;
                    };
                    switch (value.op) {
                        .sign_ext8to16, .sign_ext8to32 => { const enc = asm_mod.encodeMovsxByte32(dest_reg, src_reg); try self.emitVarLen(enc.data, enc.len); },
                        .sign_ext8to64 => try self.emit(4, asm_mod.encodeMovsxByte64(dest_reg, src_reg)),
                        .sign_ext16to32 => { const enc = asm_mod.encodeMovsxWord32(dest_reg, src_reg); try self.emitVarLen(enc.data, enc.len); },
                        .sign_ext16to64 => try self.emit(4, asm_mod.encodeMovsxWord64(dest_reg, src_reg)),
                        .sign_ext32to64 => try self.emit(3, asm_mod.encodeMovsxd(dest_reg, src_reg)),
                        else => unreachable,
                    }
                }
            },

            // Zero extension operations
            .zero_ext8to16, .zero_ext8to32, .zero_ext8to64, .zero_ext16to32, .zero_ext16to64, .zero_ext32to64 => {
                if (value.args.len > 0) {
                    const dest_reg = self.getDestRegForValue(value);
                    const src_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], dest_reg);
                        break :blk dest_reg;
                    };
                    switch (value.op) {
                        .zero_ext8to16, .zero_ext8to32 => { const enc = asm_mod.encodeMovzxByte32(dest_reg, src_reg); try self.emitVarLen(enc.data, enc.len); },
                        .zero_ext8to64 => try self.emit(4, asm_mod.encodeMovzxByte64(dest_reg, src_reg)),
                        .zero_ext16to32 => { const enc = asm_mod.encodeMovzxWord32(dest_reg, src_reg); try self.emitVarLen(enc.data, enc.len); },
                        .zero_ext16to64 => try self.emit(4, asm_mod.encodeMovzxWord64(dest_reg, src_reg)),
                        .zero_ext32to64 => { const enc = asm_mod.encodeMovReg32(dest_reg, src_reg); try self.emitVarLen(enc.data, enc.len); }, // implicit zero ext
                        else => unreachable,
                    }
                }
            },

            // Truncation operations - copy to destination register (use lower bits)
            .trunc64to32, .trunc32to16, .trunc16to8, .trunc64to16, .trunc64to8, .trunc32to8 => {
                const dest_reg = self.getDestRegForValue(value);
                if (value.args.len > 0) {
                    const src = value.args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // Copy to destination register - truncation just uses lower bits
                    if (dest_reg != src_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                    }
                    debug.log(.codegen, "      trunc: MOV {s}, {s}", .{ dest_reg.name(), src_reg.name() });
                }
            },

            .copy => {
                if (value.args.len >= 1) {
                    const src_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], .rax);
                        break :blk Reg.rax;
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != src_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                }
            },

            .phi => {
                // Phi nodes are handled by regalloc shuffle code
                // No code generation needed here
            },

            .arg => {
                // Function argument - System V AMD64 ABI
                // First 6 args in: RDI, RSI, RDX, RCX, R8, R9
                // BUT: when function has hidden return, RDI is the hidden return pointer
                // so user args shift: arg0 in RSI, arg1 in RDX, etc.
                const user_arg_idx: usize = @intCast(value.aux_int);
                const dest_reg = self.getDestRegForValue(value);

                // Adjust for hidden return: user arg N is in arg_regs[N+1]
                const abi_arg_idx: usize = if (self.has_hidden_return) user_arg_idx + 1 else user_arg_idx;
                const max_reg_args: usize = if (self.has_hidden_return) 5 else 6;

                if (user_arg_idx < max_reg_args and abi_arg_idx < regs.AMD64.arg_regs.len) {
                    // Register argument - move from ABI register to destination
                    const src_reg = regs.AMD64.arg_regs[abi_arg_idx];
                    if (dest_reg != src_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                        debug.log(.codegen, "      -> MOV {s}, {s} (user arg {d}, ABI arg {d})", .{ dest_reg.name(), src_reg.name(), user_arg_idx, abi_arg_idx });
                    }
                } else {
                    // Stack argument - load from caller's stack frame
                    // After push rbp; mov rbp,rsp, caller's args are at [rbp+16+N*8]
                    const stack_arg_idx = if (self.has_hidden_return) user_arg_idx - 5 else user_arg_idx - 6;
                    const stack_offset: i32 = @intCast(16 + stack_arg_idx * 8);
                    const load = asm_mod.encodeLoadDisp32(dest_reg, .rbp, stack_offset);
                    try self.emitBytes(load.data[0..load.len]);
                    debug.log(.codegen, "      -> MOV {s}, [rbp+{d}] (stack arg {d})", .{ dest_reg.name(), stack_offset, user_arg_idx });
                }
            },

            .static_call => {
                // Function call - System V AMD64 ABI
                // Get target function name from aux.string
                const target_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown",
                };

                // AMD64 System V: Check if call returns >16B (needs hidden return pointer in RDI)
                if (self.hidden_ret_offsets.get(value)) |rel_offset| {
                    // Get return size from aux_call
                    const ret_size: u32 = if (value.aux_call) |aux_call|
                        aux_call.hiddenReturnSize()
                    else
                        self.getTypeSize(value.type_idx);

                    // Compute frame-relative address for hidden return area
                    // We need to point to the LOW end of the allocated space (highest frame offset)
                    // Space starts at hidden_ret_frame_offset + rel_offset, extends aligned_size bytes
                    const aligned_size = (ret_size + 15) & ~@as(u32, 15);
                    const frame_offset: i32 = @intCast(self.hidden_ret_frame_offset + rel_offset + aligned_size);
                    const neg_offset: i32 = -frame_offset;

                    debug.log(.codegen, "      static_call {s}: >16B return ({d}B), frame offset={d}", .{ target_name, ret_size, frame_offset });

                    // BUG FIX: For calls with hidden return, we ALWAYS save RDI to R11 before
                    // setting up the hidden return pointer. This is necessary because:
                    // 1. Arguments might be in RDI directly
                    // 2. Arguments might be COMPUTED FROM RDI (e.g., field_addr of self)
                    // Case 2 is particularly tricky because the SSA value for field_addr might
                    // be in a different register, but the computation itself needs RDI.
                    // By always saving RDI, we ensure the original value is preserved.
                    try self.emit(3, asm_mod.encodeMovRegReg(.r11, .rdi));
                    debug.log(.codegen, "      MOV R11, RDI (save before hidden return)", .{});

                    // Set RDI to point to return location: LEA RDI, [RBP - frame_offset]
                    try self.emitLeaFromFrame(.rdi, neg_offset);
                    debug.log(.codegen, "      LEA RDI, [RBP{d}] (hidden return ptr)", .{neg_offset});

                    // Setup args shifted by 1 (RDI is hidden return, actual args start at RSI)
                    // Always translate RDI->R11 in the source register since we always save.
                    const stack_cleanup = try self.setupCallArgsWithHiddenRetSafe(value.args, true);

                    // Emit CALL rel32 with relocation
                    const call_offset = self.offset();
                    try self.emit(5, asm_mod.encodeCall(0)); // Placeholder

                    // Clean up stack arguments
                    if (stack_cleanup > 0) {
                        const cleanup_size: i32 = @intCast(stack_cleanup);
                        try self.emit(7, asm_mod.encodeAddRegImm32(.rsp, cleanup_size));
                        debug.log(.codegen, "      ADD RSP, {d} (cleanup stack args)", .{cleanup_size});
                    }

                    // Record relocation
                    try self.relocations.append(self.allocator, .{
                        .offset = @intCast(call_offset + 1),
                        .target = target_name,
                    });

                    debug.log(.codegen, "      CALL {s} (hidden return)", .{target_name});
                } else {
                    // Normal call - use parallel copy to setup arguments
                    const stack_cleanup = try self.setupCallArgs(value.args);

                    // Emit CALL rel32 with relocation
                    const call_offset = self.offset();
                    try self.emit(5, asm_mod.encodeCall(0)); // Placeholder

                    // Clean up stack arguments (caller-cleanup on System V AMD64)
                    if (stack_cleanup > 0) {
                        const cleanup_size: i32 = @intCast(stack_cleanup);
                        try self.emit(7, asm_mod.encodeAddRegImm32(.rsp, cleanup_size));
                        debug.log(.codegen, "      ADD RSP, {d} (cleanup stack args)", .{cleanup_size});
                    }

                    // Record relocation
                    try self.relocations.append(self.allocator, .{
                        .offset = @intCast(call_offset + 1), // Skip E8 opcode
                        .target = target_name,
                    });

                    debug.log(.codegen, "      CALL {s}", .{target_name});
                }
                debug.log(.codegen, "      static_call v{d}: uses={d}, has_home={}", .{ value.id, value.uses, value.getHome() != null });

                // Result is in RAX - regalloc will handle spill/reload if needed
                // Go's approach: don't move to callee-saved, let regalloc spill
                // (Same pattern as ARM64 backend)
            },

            .closure_call => {
                // Indirect function call through function pointer
                // System V AMD64 ABI: args in RDI, RSI, RDX, RCX, R8, R9, result in RAX
                // First arg is function pointer, rest are actual call arguments
                const args = value.args;
                if (args.len == 0) {
                    debug.log(.codegen, "      closure_call: no function pointer!", .{});
                    return;
                }

                // Get function pointer - use R11 as it's caller-saved and not an arg reg
                const fn_ptr = args[0];
                var fn_ptr_reg = self.getRegForValue(fn_ptr) orelse blk: {
                    try self.ensureInReg(fn_ptr, .r11);
                    break :blk Reg.r11;
                };

                // Setup actual arguments (skip args[0] which is the function pointer)
                const actual_args = args[1..];
                if (actual_args.len > 0) {
                    // If fn_ptr is in an arg register, save it to R11 first
                    for (regs.AMD64.arg_regs) |arg_reg| {
                        if (fn_ptr_reg == arg_reg) {
                            try self.emit(3, asm_mod.encodeMovRegReg(.r11, fn_ptr_reg));
                            fn_ptr_reg = .r11;
                            break;
                        }
                    }

                    // Setup actual arguments
                    const stack_cleanup = try self.setupCallArgs(actual_args);

                    // Emit CALL *reg (indirect call)
                    const call_inst = asm_mod.encodeCallReg(fn_ptr_reg);
                    try self.emitBytes(call_inst.data[0..call_inst.len]);

                    // Clean up stack arguments if any
                    if (stack_cleanup > 0) {
                        const cleanup_size: i32 = @intCast(stack_cleanup);
                        try self.emit(7, asm_mod.encodeAddRegImm32(.rsp, cleanup_size));
                        debug.log(.codegen, "      stack cleanup: ADD RSP, {d}", .{cleanup_size});
                    }
                    debug.log(.codegen, "      -> CALL *{s} (indirect with {} args)", .{ fn_ptr_reg.name(), actual_args.len });
                } else {
                    // No arguments, just call through function pointer
                    const call_inst = asm_mod.encodeCallReg(fn_ptr_reg);
                    try self.emitBytes(call_inst.data[0..call_inst.len]);
                    debug.log(.codegen, "      -> CALL *{s} (indirect, no args)", .{fn_ptr_reg.name()});
                }

                // Result is in RAX - regalloc will handle spill/reload if needed
            },

            .string_concat => {
                // String concatenation: call __cot_str_concat(ptr1, len1, ptr2, len2)
                if (value.args.len >= 4) {
                    // Save callee-saved r12-r15, load args, move to ABI regs
                    for ([_]Reg{ .r12, .r13, .r14, .r15 }) |r| try self.emitPushPop(r, true);
                    try self.forceInReg(value.args[0], .r12);
                    try self.forceInReg(value.args[1], .r13);
                    try self.forceInReg(value.args[2], .r14);
                    try self.forceInReg(value.args[3], .r15);
                    try self.emit(3, asm_mod.encodeMovRegReg(.rdi, .r12));
                    try self.emit(3, asm_mod.encodeMovRegReg(.rsi, .r13));
                    try self.emit(3, asm_mod.encodeMovRegReg(.rdx, .r14));
                    try self.emit(3, asm_mod.encodeMovRegReg(.rcx, .r15));
                    const call_offset = self.offset();
                    try self.emit(5, asm_mod.encodeCall(0));
                    try self.relocations.append(self.allocator, .{ .offset = @intCast(call_offset + 1), .target = "__cot_str_concat" });
                    try self.emit(3, asm_mod.encodeMovRegReg(.r8, .rdx)); // Save len to R8
                    for ([_]Reg{ .r15, .r14, .r13, .r12 }) |r| try self.emitPushPop(r, false);
                }
            },

            .select_n => {
                // Extract idx-th result from multi-value call (idx=0→RAX, idx=1→RDX/R8)
                if (value.args.len >= 1) {
                    const idx: usize = @intCast(value.aux_int);
                    const dest_reg = self.getDestRegForValue(value);
                    const src_reg: Reg = if (idx == 0) .rax else if (value.args[0].op == .string_concat) .r8 else .rdx;
                    if (dest_reg != src_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                }
            },

            .load => {
                // Load from memory address
                // arg[0] is the address
                if (value.args.len > 0) {
                    const addr = value.args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    // CRITICAL FIX: Always rematerialize local_addr values.
                    // local_addr is skipped during codegen (rematerializeable), so its
                    // "home" register may contain a completely different value.
                    // getRegForValue returns stale home assignments for skipped values.
                    // Use r10 as scratch if dest_reg is r11, to avoid conflict.
                    const addr_scratch: Reg = if (dest_reg == .r11) .r10 else .r11;
                    const addr_reg = if (addr.op == .local_addr) blk: {
                        try self.forceInReg(addr, addr_scratch);
                        break :blk addr_scratch;
                    } else self.getRegForValue(addr) orelse blk: {
                        // Address should already be computed - use scratch register
                        try self.ensureInReg(addr, addr_scratch);
                        break :blk addr_scratch;
                    };

                    // Use type-sized load instruction
                    const type_size = self.getTypeSize(value.type_idx);

                    if (type_size == 1) {
                        // MOVZX for byte load (zero-extend to 64-bit)
                        const load = asm_mod.encodeLoadByteDisp32(dest_reg, addr_reg, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else if (type_size == 2) {
                        // MOVZX for word load (zero-extend to 64-bit)
                        const load = asm_mod.encodeLoadWordDisp32(dest_reg, addr_reg, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else if (type_size == 4) {
                        // MOV r32 for dword load (implicit zero-extend to 64-bit)
                        const load = asm_mod.encodeLoadDwordDisp32(dest_reg, addr_reg, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    } else {
                        // MOV r64 for qword load (default)
                        const load = asm_mod.encodeLoadDisp32(dest_reg, addr_reg, 0);
                        try self.emitBytes(load.data[0..load.len]);
                    }
                    debug.log(.codegen, "      -> LOAD {s} <- [{s}] ({d}B)", .{ dest_reg.name(), addr_reg.name(), type_size });
                }
            },

            .store => {
                // Store to memory address
                // arg[0] is the address, arg[1] is the value to store
                if (value.args.len >= 2) {
                    const addr = value.args[0];
                    const val = value.args[1];
                    // CRITICAL: If aux_int is set (from store_index_local/store_index_value),
                    // use it as the store size. This fixes storing u8 values where the
                    // source is an untyped integer constant (8 bytes) but destination is 1 byte.
                    const type_size: u32 = if (value.aux_int > 0) @intCast(value.aux_int) else self.getTypeSize(val.type_idx);

                    // CRITICAL: First check what register the value is in, BEFORE we
                    // rematerialize the address. This prevents clobbering the value
                    // when the value happens to be in r11.
                    const val_existing_reg = self.getRegForValue(val);

                    // Get destination address - choose scratch register that won't conflict
                    // CRITICAL FIX: Always rematerialize local_addr (see .load fix above)
                    // Use r10 if value is in r11, otherwise use r11
                    const addr_scratch: Reg = if (val_existing_reg == .r11) .r10 else .r11;
                    const addr_reg = if (addr.op == .local_addr) blk: {
                        try self.forceInReg(addr, addr_scratch);
                        break :blk addr_scratch;
                    } else self.getRegForValue(addr) orelse blk: {
                        try self.ensureInReg(addr, addr_scratch);
                        break :blk addr_scratch;
                    };

                    // Special handling for >16B call results with hidden return
                    if (type_size > 16 and (val.op == .static_call or val.op == .closure_call)) {
                        // Result is at pre-allocated hidden return frame location
                        if (self.hidden_ret_offsets.get(val)) |rel_offset| {
                            // Point to the LOW end of the allocated space (highest frame offset)
                            const aligned_size = (type_size + 15) & ~@as(u32, 15);
                            const frame_offset: i32 = @intCast(self.hidden_ret_frame_offset + rel_offset + aligned_size);
                            const neg_offset: i32 = -frame_offset;
                            // LEA R10, [RBP - frame_offset] (source address)
                            try self.emitLeaFromFrame(.r10, neg_offset);
                            debug.log(.codegen, "      store >16B: src at frame offset {d} -> R10", .{frame_offset});

                            // Copy data in 8-byte chunks
                            // Use RAX as scratch (addr_reg could be any reg including RCX, and R10 is source)
                            // RAX is safe since >16B returns use hidden return, not RAX
                            var copy_off: u32 = 0;
                            while (copy_off < type_size) : (copy_off += 8) {
                                // MOV RAX, [R10 + copy_off]
                                const src_enc = asm_mod.encodeMovRegMemDisp(.rax, .r10, @intCast(copy_off));
                                try self.code.appendSlice(self.allocator, src_enc.data[0..src_enc.len]);
                                // MOV [addr_reg + copy_off], RAX
                                const dst_enc = asm_mod.encodeMovMemReg(addr_reg, @intCast(copy_off), .rax);
                                try self.code.appendSlice(self.allocator, dst_enc.data[0..dst_enc.len]);
                            }
                            debug.log(.codegen, "      -> copied {d}B from hidden ret to [{s}]", .{ type_size, addr_reg.name() });
                        } else {
                            // Fallback: shouldn't happen for >16B call
                            debug.log(.codegen, "      store >16B: WARNING no hidden_ret_offset found", .{});
                        }
                    } else if (type_size == 16 and val.op == .string_concat) {
                        // String concat returns ptr in RAX, len in R8 (we saved it there)
                        // Store as 16-byte pair
                        const store_ptr = asm_mod.encodeMovMemReg(addr_reg, 0, .rax);
                        try self.code.appendSlice(self.allocator, store_ptr.data[0..store_ptr.len]);
                        const store_len = asm_mod.encodeMovMemReg(addr_reg, 8, .r8);
                        try self.code.appendSlice(self.allocator, store_len.data[0..store_len.len]);
                        debug.log(.codegen, "      -> stored string_concat RAX,R8 to [{s}]", .{addr_reg.name()});
                    } else if (type_size > 8 and type_size <= 16 and (val.op == .static_call or val.op == .closure_call)) {
                        // BUG-078 FIX: 9-16 byte struct call results are returned in RAX:RDX
                        // Store RAX at offset 0
                        const store_rax = asm_mod.encodeMovMemReg(addr_reg, 0, .rax);
                        try self.code.appendSlice(self.allocator, store_rax.data[0..store_rax.len]);
                        // Store RDX at offset 8 (for the remaining bytes)
                        const store_rdx = asm_mod.encodeMovMemReg(addr_reg, 8, .rdx);
                        try self.code.appendSlice(self.allocator, store_rdx.data[0..store_rdx.len]);
                        debug.log(.codegen, "      -> stored {d}B call result RAX,RDX to [{s}]", .{ type_size, addr_reg.name() });
                    } else {
                        // Normal store - get value to register
                        const val_reg = self.getRegForValue(val) orelse blk: {
                            try self.ensureInReg(val, .r10);
                            break :blk Reg.r10;
                        };

                        if (type_size == 1) {
                            const store = asm_mod.encodeStoreByteDisp32(addr_reg, 0, val_reg);
                            try self.emitBytes(store.data[0..store.len]);
                        } else if (type_size == 2) {
                            const store = asm_mod.encodeStoreWordDisp32(addr_reg, 0, val_reg);
                            try self.emitBytes(store.data[0..store.len]);
                        } else if (type_size == 4) {
                            const store = asm_mod.encodeStoreDwordDisp32(addr_reg, 0, val_reg);
                            try self.emitBytes(store.data[0..store.len]);
                        } else {
                            const store = asm_mod.encodeStoreDisp32(addr_reg, 0, val_reg);
                            try self.emitBytes(store.data[0..store.len]);
                        }
                        debug.log(.codegen, "      -> STORE [{s}] <- {s} ({d}B)", .{ addr_reg.name(), val_reg.name(), type_size });
                    }
                }
            },

            .local_addr => {
                // Address of local variable on stack
                // Use R11 as default to avoid clobbering values in RAX/RCX
                const dest_reg = if (self.getRegForValue(value)) |r| r else Reg.r11;
                const local_idx: usize = @intCast(value.aux_int);

                // Get stack offset from local_offsets (set by stackalloc)
                // On x86-64, locals are at negative offsets from RBP
                // FIX: Use the END of the allocation, not the start
                // This prevents arrays from overflowing into saved RBP
                if (local_idx < self.func.local_offsets.len and local_idx < self.func.local_sizes.len) {
                    const byte_offset = self.func.local_offsets[local_idx];
                    const local_size: i32 = @intCast(self.func.local_sizes[local_idx]);
                    // local_offsets stores the START of the local, but we need to
                    // position the base address so that arr[N-1] doesn't overflow
                    // disp = -(byte_offset + local_size) positions the base at the
                    // low end, so arr[i] accesses grow toward (but don't reach) RBP
                    const disp: i32 = -(byte_offset + local_size);
                    const lea = asm_mod.encodeLeaDisp32(dest_reg, .rbp, disp);
                    try self.emitBytes(lea.data[0..lea.len]);
                } else {
                    // Fallback: shouldn't happen
                    const lea = asm_mod.encodeLeaDisp32(dest_reg, .rbp, 0);
                    try self.emitBytes(lea.data[0..lea.len]);
                }
            },

            .off_ptr => {
                // Add offset to base pointer (for field/element access)
                // args[0] = base pointer, aux_int = offset
                if (value.args.len > 0) {
                    const base = value.args[0];
                    const field_offset: i64 = value.aux_int;
                    const dest_reg = self.getDestRegForValue(value);

                    // Handle local_addr specially - ALWAYS regenerate to avoid stale registers
                    // CRITICAL FIX: getRegForValue returns stale home assignments for
                    // values that were skipped during codegen (like local_addr).
                    var base_reg: Reg = undefined;
                    if (base.op == .local_addr) {
                        // ALWAYS regenerate local address - never trust getRegForValue
                        try self.forceInReg(base, dest_reg);
                        base_reg = dest_reg;
                    } else {
                        base_reg = self.getRegForValue(base) orelse blk: {
                            try self.ensureInReg(base, dest_reg);
                            break :blk dest_reg;
                        };
                    }

                    // LEA dest, [base + offset] or MOV if offset is 0
                    if (field_offset != 0) {
                        const disp: i32 = @intCast(field_offset);
                        const lea = asm_mod.encodeLeaDisp32(dest_reg, base_reg, disp);
                        try self.emitBytes(lea.data[0..lea.len]);
                        debug.log(.codegen, "      -> LEA {s}, [{s}+{d}] (off_ptr)", .{ dest_reg.name(), base_reg.name(), disp });
                    } else if (base_reg != dest_reg) {
                        try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, base_reg));
                    }
                }
            },

            .const_string, .const_ptr => {
                // String literal: emit LEA with RIP-relative addressing
                // The string index is in aux_int
                const string_index: usize = @intCast(value.aux_int);
                const dest_reg = self.getDestRegForValue(value);

                // Get the string data and make a copy (func may be deinit'd before finalize)
                const str_data = if (string_index < self.func.string_literals.len)
                    self.func.string_literals[string_index]
                else
                    "";

                // Record the offset for relocation fixup
                const lea_offset = self.offset();
                // Emit LEA with disp=0 (linker will fix up via relocation)
                try self.emit(7, asm_mod.encodeLeaRipRel32(dest_reg, 0));

                // Record string reference for relocation during finalize()
                const str_copy = try self.allocator.dupe(u8, str_data);
                try self.string_refs.append(self.allocator, .{
                    .code_offset = lea_offset,
                    .string_data = str_copy,
                });

                debug.log(.codegen, "      -> {s} = str[{d}] len={d} (pending reloc)", .{ dest_reg.name(), string_index, str_data.len });
            },

            .global_addr => {
                // Address of global variable
                const global_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown_global",
                };
                const dest_reg = self.getDestRegForValue(value);

                // Record the offset for relocation fixup
                const lea_offset = self.offset();
                // Emit LEA with RIP-relative addressing (disp=0, linker fills in)
                try self.emit(7, asm_mod.encodeLeaRipRel32(dest_reg, 0));

                // Record global reference for relocation
                try self.relocations.append(self.allocator, .{
                    .offset = @intCast(lea_offset + 3), // Skip REX+opcode+ModRM to get to disp32
                    .target = global_name,
                });

                debug.log(.codegen, "      -> {s} = global '{s}' (pending reloc)", .{ dest_reg.name(), global_name });
            },

            .addr => {
                // Address of a symbol (function for function pointers)
                // aux.string contains the symbol name
                const func_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown",
                };
                const dest_reg = self.getDestRegForValue(value);

                // Record the offset for relocation fixup
                const lea_offset = self.offset();
                // Emit LEA with RIP-relative addressing
                try self.emit(7, asm_mod.encodeLeaRipRel32(dest_reg, 0));

                // Record function reference for relocation
                try self.relocations.append(self.allocator, .{
                    .offset = @intCast(lea_offset + 3), // Skip REX+opcode+ModRM to get to disp32
                    .target = func_name,
                });

                debug.log(.codegen, "      -> {s} = addr '{s}' (pending reloc)", .{ dest_reg.name(), func_name });
            },

            .slice_make => {
                const args = value.args;
                if (args.len >= 2) {
                    const ptr_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], .rax);
                        break :blk Reg.rax;
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    if (ptr_reg != dest_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, ptr_reg));
                }
            },

            .slice_ptr => {
                const slice_val = value.args[0];
                const dest_reg = self.getDestRegForValue(value);
                if (slice_val.op == .const_string or slice_val.op == .const_ptr) {
                    const src_reg = self.getRegForValue(slice_val) orelse blk: {
                        try self.ensureInReg(slice_val, .rax);
                        break :blk Reg.rax;
                    };
                    if (dest_reg != src_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                } else if (slice_val.op == .static_call) {
                    if (dest_reg != .rax) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .rax));
                } else {
                    const slice_reg = self.getRegForValue(slice_val) orelse blk: {
                        try self.ensureInReg(slice_val, .r11);
                        break :blk Reg.r11;
                    };
                    const load = asm_mod.encodeLoadDisp32(dest_reg, slice_reg, 0);
                    try self.emitBytes(load.data[0..load.len]);
                }
            },

            .slice_len => {
                const slice_val = value.args[0];
                const dest_reg = self.getDestRegForValue(value);
                if (slice_val.op == .const_string) {
                    const string_index: usize = @intCast(slice_val.aux_int);
                    const str_len: i64 = if (string_index < self.func.string_literals.len) @intCast(self.func.string_literals[string_index].len) else 0;
                    try self.emitLoadImmediate(dest_reg, str_len);
                } else if (slice_val.op == .static_call) {
                    if (dest_reg != .rdx) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, .rdx));
                } else {
                    const slice_reg = self.getRegForValue(slice_val) orelse blk: {
                        try self.ensureInReg(slice_val, .r11);
                        break :blk Reg.r11;
                    };
                    const load = asm_mod.encodeLoadDisp32(dest_reg, slice_reg, 8);
                    try self.emitBytes(load.data[0..load.len]);
                }
            },

            .string_ptr, .string_len => {
                // Copy ptr/len component to dest register
                if (value.args.len >= 1) {
                    const dest_reg = self.getDestRegForValue(value);
                    const src_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], dest_reg);
                        break :blk dest_reg;
                    };
                    if (src_reg != dest_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, src_reg));
                }
            },

            .string_make => {}, // Virtual op - no code generated

            .and_, .or_, .xor => {
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: { try self.ensureInReg(args[0], .rax); break :blk Reg.rax; };
                    const op2_reg = self.getRegForValue(args[1]) orelse blk: { try self.ensureInReg(args[1], .rcx); break :blk Reg.rcx; };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != op1_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op1_reg));
                    try self.emit(3, switch (value.op) {
                        .and_ => asm_mod.encodeAndRegReg(dest_reg, op2_reg),
                        .or_ => asm_mod.encodeOrRegReg(dest_reg, op2_reg),
                        .xor => asm_mod.encodeXorRegReg(dest_reg, op2_reg),
                        else => unreachable,
                    });
                }
            },

            .shl, .shr => {
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: { try self.ensureInReg(args[0], .rax); break :blk Reg.rax; };
                    if (args[1].op == .const_int or args[1].op == .const_64) {
                        const dest_reg = self.getDestRegForValue(value);
                        if (dest_reg != op1_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op1_reg));
                        const shift_amt: u8 = @intCast(@as(u64, @bitCast(args[1].aux_int)) & 63);
                        try self.emit(4, if (value.op == .shl) asm_mod.encodeShlRegImm8(dest_reg, shift_amt) else asm_mod.encodeShrRegImm8(dest_reg, shift_amt));
                    } else {
                        const assigned_dest = self.getDestRegForValue(value);
                        const compute_reg: Reg = if (assigned_dest == .rcx) .rax else assigned_dest;
                        if (compute_reg != op1_reg) try self.emit(3, asm_mod.encodeMovRegReg(compute_reg, op1_reg));
                        const op2_reg = self.getRegForValue(args[1]) orelse blk: { try self.ensureInReg(args[1], .rcx); break :blk Reg.rcx; };
                        if (op2_reg != .rcx) try self.emit(3, asm_mod.encodeMovRegReg(.rcx, op2_reg));
                        try self.emit(3, if (value.op == .shl) asm_mod.encodeShlRegCl(compute_reg) else asm_mod.encodeShrRegCl(compute_reg));
                        if (compute_reg != assigned_dest) try self.emit(3, asm_mod.encodeMovRegReg(assigned_dest, compute_reg));
                    }
                }
            },

            .sar => {
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: { try self.ensureInReg(args[0], .rax); break :blk Reg.rax; };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != op1_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op1_reg));
                    if (args[1].op == .const_int or args[1].op == .const_64) {
                        const shift_amt: u8 = @intCast(@as(u64, @bitCast(args[1].aux_int)) & 63);
                        try self.emit(4, asm_mod.encodeSarRegImm8(dest_reg, shift_amt));
                    } else {
                        const op2_reg = self.getRegForValue(args[1]) orelse blk: {
                            try self.ensureInReg(args[1], .rcx);
                            break :blk Reg.rcx;
                        };
                        if (op2_reg != .rcx) {
                            try self.emit(3, asm_mod.encodeMovRegReg(.rcx, op2_reg));
                        }
                        try self.emit(3, asm_mod.encodeSarRegCl(dest_reg));
                    }
                }
            },

            .neg => {
                if (value.args.len >= 1) {
                    const op_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], .rax);
                        break :blk Reg.rax;
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != op_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op_reg));
                    try self.emit(3, asm_mod.encodeNegReg(dest_reg));
                }
            },

            .not => {
                if (value.args.len >= 1) {
                    const op_reg = self.getRegForValue(value.args[0]) orelse blk: {
                        try self.ensureInReg(value.args[0], .rax);
                        break :blk Reg.rax;
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    if (dest_reg != op_reg) try self.emit(3, asm_mod.encodeMovRegReg(dest_reg, op_reg));
                    if (self.getTypeSize(value.type_idx) == 1) {
                        try self.emitBytes(&[_]u8{ 0x48, 0x83, 0xF0 | @as(u8, @intFromEnum(dest_reg) & 7), 0x01 }); // XOR dest, 1
                    } else {
                        try self.emit(3, asm_mod.encodeNotReg(dest_reg));
                    }
                }
            },

            .store_reg => {
                // Store value to a stack spill slot
                if (value.args.len > 0) {
                    const src_value = value.args[0];
                    const loc = value.getHome() orelse {
                        debug.log(.codegen, "      store_reg v{d}: NO stack location!", .{value.id});
                        return;
                    };
                    const byte_off = loc.stackOffset();

                    // BUG-078 FIX: Get the register from the HOME assignment, not getRegForValue.
                    // At this point, the value is still in its original register - we haven't
                    // stored it to the spill slot yet! getRegForValue would return null because
                    // the value is marked as spilled, but the spill hasn't executed yet.
                    const src_reg: Reg = if (src_value.getHome()) |home| blk: {
                        switch (home) {
                            .register => |reg_num| break :blk regNumToAMD64(@intCast(reg_num)),
                            .stack => {
                                // This shouldn't happen - src should be in a register for store_reg
                                debug.log(.codegen, "      store_reg v{d}: src home is stack, rematerializing", .{value.id});
                                try self.rematerializeByOp(src_value, .r11);
                                break :blk Reg.r11;
                            },
                        }
                    } else blk: {
                        // No home - rematerialize the value
                        try self.rematerializeByOp(src_value, .r11);
                        break :blk Reg.r11;
                    };

                    // MOV [RBP - (offset + 8)], src_reg
                    // The +8 is because stackalloc assigns the START of the slot,
                    // but AMD64 RBP-relative addressing needs the END (like local_addr).
                    // This matches local_addr which uses -(byte_offset + local_size).
                    const disp: i32 = -@as(i32, @intCast(byte_off + 8));
                    const store = asm_mod.encodeStoreDisp32(.rbp, disp, src_reg);
                    try self.emitBytes(store.data[0..store.len]);
                    debug.log(.codegen, "      -> MOV [RBP{d}], {s}", .{ disp, src_reg.name() });
                }
            },

            .load_reg => {
                // Load value from a stack spill slot
                if (value.args.len > 0) {
                    const spill_value = value.args[0];
                    const loc = spill_value.getHome() orelse {
                        debug.log(.codegen, "      load_reg v{d}: source has NO location!", .{value.id});
                        return;
                    };
                    const byte_off = loc.stackOffset();
                    const dest_reg = self.getDestRegForValue(value);

                    // MOV dest_reg, [RBP - (offset + 8)]
                    // The +8 matches store_reg - see comment there.
                    const disp: i32 = -@as(i32, @intCast(byte_off + 8));
                    const load = asm_mod.encodeLoadDisp32(dest_reg, .rbp, disp);
                    try self.emitBytes(load.data[0..load.len]);
                    debug.log(.codegen, "      -> MOV {s}, [RBP{d}]", .{ dest_reg.name(), disp });
                }
            },

            .move => {
                // Bulk memory copy (OpMove) - used for large struct args/returns
                // args[0] = dest address, args[1] = src value (may be arg pointer)
                // aux_int = size in bytes
                if (value.args.len >= 2) {
                    const dest_addr = value.args[0];
                    const src_val = value.args[1];
                    const type_size: u32 = @intCast(value.aux_int);

                    // Get destination address register
                    const dest_reg = self.getRegForValue(dest_addr) orelse blk: {
                        try self.ensureInReg(dest_addr, .r10);
                        break :blk Reg.r10;
                    };

                    // Get source address - for large struct params, src_val is the arg (pointer)
                    var src_reg: Reg = undefined;

                    // Check if src_val is an arg (pointer passed in register)
                    if (src_val.op == .arg) {
                        // For large struct param, arg is the pointer - get it from ABI register
                        const user_arg_idx: usize = @intCast(src_val.aux_int);
                        const abi_arg_idx: usize = if (self.has_hidden_return) user_arg_idx + 1 else user_arg_idx;
                        if (abi_arg_idx < regs.AMD64.arg_regs.len) {
                            src_reg = regs.AMD64.arg_regs[abi_arg_idx];
                            debug.log(.codegen, "      move: src is arg {d} in {s}", .{ user_arg_idx, src_reg.name() });
                        } else {
                            // Stack arg - load address
                            const stack_arg_idx = if (self.has_hidden_return) user_arg_idx - 5 else user_arg_idx - 6;
                            const stack_offset: i32 = @intCast(16 + stack_arg_idx * 8);
                            const load = asm_mod.encodeLoadDisp32(.r11, .rbp, stack_offset);
                            try self.emitBytes(load.data[0..load.len]);
                            src_reg = .r11;
                        }
                    } else if (self.getRegForValue(src_val)) |reg| {
                        src_reg = reg;
                    } else {
                        try self.ensureInReg(src_val, .r11);
                        src_reg = .r11;
                    }

                    const scratch_reg: Reg = if (src_reg == .rcx or dest_reg == .rcx) .r9 else .rcx;
                    var copy_off: u32 = 0;
                    while (copy_off + 8 <= type_size) : (copy_off += 8) {
                        const load = asm_mod.encodeMovRegMemDisp(scratch_reg, src_reg, @intCast(copy_off));
                        try self.code.appendSlice(self.allocator, load.data[0..load.len]);
                        const store = asm_mod.encodeMovMemReg(dest_reg, @intCast(copy_off), scratch_reg);
                        try self.code.appendSlice(self.allocator, store.data[0..store.len]);
                    }
                    if (copy_off + 4 <= type_size) {
                        const load = asm_mod.encodeLoadDwordDisp32(scratch_reg, src_reg, @intCast(copy_off));
                        try self.emitBytes(load.data[0..load.len]);
                        const store = asm_mod.encodeStoreDwordDisp32(dest_reg, @intCast(copy_off), scratch_reg);
                        try self.emitBytes(store.data[0..store.len]);
                        copy_off += 4;
                    }
                    while (copy_off < type_size) : (copy_off += 1) {
                        const load = asm_mod.encodeLoadByteDisp32(scratch_reg, src_reg, @intCast(copy_off));
                        try self.emitBytes(load.data[0..load.len]);
                        const store = asm_mod.encodeStoreByteDisp32(dest_reg, @intCast(copy_off), scratch_reg);
                        try self.emitBytes(store.data[0..store.len]);
                    }
                }
            },

            else => {},
        }
    }

    /// Emit PUSH or POP for a register
    fn emitPushPop(self: *AMD64CodeGen, reg: Reg, is_push: bool) !void {
        if (is_push) {
            const enc = asm_mod.encodePush(reg);
            try self.emitBytes(enc.data[0..enc.len]);
        } else {
            const enc = asm_mod.encodePop(reg);
            try self.emitBytes(enc.data[0..enc.len]);
        }
    }

    /// Load immediate value into register.
    fn emitLoadImmediate(self: *AMD64CodeGen, reg: Reg, imm: i64) !void {
        if (imm == 0) {
            // XOR reg, reg (smaller and faster)
            try self.emit(3, asm_mod.encodeXorRegReg(reg, reg));
        } else if (imm >= -2147483648 and imm <= 2147483647) {
            // MOV r64, imm32 (sign-extended) - 7 bytes
            try self.emit(7, asm_mod.encodeMovRegImm32(reg, @intCast(imm)));
        } else {
            // MOV r64, imm64 - 10 bytes
            try self.emit(10, asm_mod.encodeMovRegImm64(reg, @bitCast(imm)));
        }
    }

    /// Move a value to RAX for return.
    fn moveToRAX(self: *AMD64CodeGen, value: *const Value) !void {
        if (self.getRegForValue(value)) |reg| {
            if (reg != .rax) {
                try self.emit(3, asm_mod.encodeMovRegReg(.rax, reg));
            }
        } else {
            // Value not in register, need to rematerialize or load
            try self.ensureInReg(value, .rax);
        }
    }

    pub fn finalize(self: *AMD64CodeGen) ![]u8 {
        var elf_writer = elf.ElfWriter.init(self.allocator);
        defer elf_writer.deinit();

        try elf_writer.addCode(self.code.items);
        for (self.symbols.items) |sym| try elf_writer.addSymbol(sym.name, sym.value, sym.section, sym.binding == elf.STB_GLOBAL);
        for (self.relocations.items) |reloc| try elf_writer.addRelocation(reloc.offset, reloc.target);

        for (self.string_refs.items) |str_ref| {
            const sym_name = try elf_writer.addStringLiteral(str_ref.string_data);
            try elf_writer.addDataRelocation(str_ref.code_offset + 3, sym_name);
        }
        for (self.globals) |global| try elf_writer.addGlobalVariable(global.name, @intCast(global.size));

        var output = std.ArrayListUnmanaged(u8){};
        errdefer output.deinit(self.allocator);
        try elf_writer.write(output.writer(self.allocator));
        return output.toOwnedSlice(self.allocator);
    }
};

// =========================================
// Tests
// =========================================

test "AMD64CodeGen basic" {
    const allocator = std.testing.allocator;
    var codegen = AMD64CodeGen.init(allocator);
    defer codegen.deinit();

    // Just verify it initializes correctly
    try std.testing.expect(codegen.code.items.len == 0);
}
