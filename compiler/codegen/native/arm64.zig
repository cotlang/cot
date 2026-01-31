//! ARM64 code generation for Apple Silicon / ARMv8-A.
//! Uses linear scan register allocation and generates ABI-compliant code.

const std = @import("std");
const Func = @import("../../ssa/func.zig").Func;
const Block = @import("../../ssa/block.zig").Block;
const value_mod = @import("../../ssa/value.zig");
const Value = value_mod.Value;
const AuxCall = value_mod.AuxCall;
const Op = @import("../../ssa/op.zig").Op;
const Location = @import("../../ssa/func.zig").Location;
const asm_mod = @import("arm64_asm.zig");
const regalloc = @import("regalloc.zig");
const macho = @import("macho.zig");
const debug = @import("../../pipeline_debug.zig");
const types_mod = @import("../../frontend/types.zig");
const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const ir_mod = @import("../../frontend/ir.zig");
const abi = @import("abi.zig");

/// ARM64 register numbers.
pub const Reg = enum(u8) {
    // General purpose registers
    x0 = 0,
    x1,
    x2,
    x3,
    x4,
    x5,
    x6,
    x7,
    x8,
    x9,
    x10,
    x11,
    x12,
    x13,
    x14,
    x15,
    x16, // IP0
    x17, // IP1
    x18, // Platform register
    x19, // Callee-saved
    x20,
    x21,
    x22,
    x23,
    x24,
    x25,
    x26,
    x27,
    x28,
    x29, // FP
    x30, // LR
    sp, // Stack pointer (x31)

    // Floating point registers
    v0 = 32,
    v1,
    v2,
    v3,
    v4,
    v5,
    v6,
    v7,
    // ... more as needed

    pub fn name(self: Reg) []const u8 {
        return @tagName(self);
    }
};

/// Caller-saved registers (can be clobbered by calls).
pub const caller_saved = [_]Reg{
    .x0, .x1, .x2, .x3, .x4, .x5, .x6, .x7,
    .x8, .x9, .x10, .x11, .x12, .x13, .x14, .x15,
};

/// Callee-saved registers (must be preserved across calls).
pub const callee_saved = [_]Reg{
    .x19, .x20, .x21, .x22, .x23, .x24, .x25, .x26, .x27, .x28,
};

pub const Relocation = struct { offset: u32, target: []const u8 };
const BranchFixup = struct { code_offset: u32, target_block_id: u32, is_cbz: bool };
const StringRef = struct { adrp_offset: u32, add_offset: u32, string_data: []const u8 };
const FuncRef = struct { adrp_offset: u32, add_offset: u32, func_name: []const u8 };

pub const ARM64CodeGen = struct {
    allocator: std.mem.Allocator,
    func: *const Func,
    type_reg: ?*const TypeRegistry = null,
    reg_state: [32]?*const Value = [_]?*const Value{null} ** 32,
    code: std.ArrayListUnmanaged(u8),
    symbols: std.ArrayListUnmanaged(macho.Symbol),
    relocations: std.ArrayListUnmanaged(Relocation),
    regalloc_state: ?*const regalloc.RegAllocState,
    frame_size: u32 = 64, // Minimum 64 bytes: 16 for FP/LR, 48 for Wasm locals
    next_spill_slot: u32 = 0,
    spill_slot_map: std.AutoHashMapUnmanaged(*const Value, u32),
    block_offsets: std.AutoHashMapUnmanaged(u32, u32),
    branch_fixups: std.ArrayListUnmanaged(BranchFixup),
    string_refs: std.ArrayListUnmanaged(StringRef),
    func_refs: std.ArrayListUnmanaged(FuncRef),
    data_relocations: std.ArrayListUnmanaged(macho.ExtRelocation),
    globals: []const ir_mod.Global = &.{},
    has_hidden_return: bool = false, // >16B return needs hidden pointer in x8
    hidden_ret_ptr_offset: u32 = 0,
    hidden_ret_frame_offset: u32 = 0,
    hidden_ret_space_needed: u32 = 0,
    hidden_ret_offsets: std.AutoHashMapUnmanaged(*const Value, u32),
    line_entries: std.ArrayListUnmanaged(LineEntry),
    last_source_offset: u32 = 0,
    debug_source_file: []const u8 = "",
    debug_source_text: []const u8 = "",
    call_stack_adjustment: u32 = 0, // Track SP adjustment during call setup

    pub const LineEntry = struct { code_offset: u32, source_offset: u32 };

    pub fn init(allocator: std.mem.Allocator) ARM64CodeGen {
        return .{ .allocator = allocator, .func = undefined, .code = .{}, .symbols = .{},
            .relocations = .{}, .regalloc_state = null, .spill_slot_map = .{}, .block_offsets = .{},
            .branch_fixups = .{}, .string_refs = .{}, .func_refs = .{}, .data_relocations = .{},
            .hidden_ret_offsets = .{}, .line_entries = .{} };
    }

    pub fn deinit(self: *ARM64CodeGen) void {
        self.code.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.relocations.deinit(self.allocator);
        self.spill_slot_map.deinit(self.allocator);
        self.block_offsets.deinit(self.allocator);
        self.branch_fixups.deinit(self.allocator);
        for (self.string_refs.items) |str_ref| self.allocator.free(str_ref.string_data);
        self.string_refs.deinit(self.allocator);
        for (self.func_refs.items) |func_ref| self.allocator.free(func_ref.func_name);
        self.func_refs.deinit(self.allocator);
        self.data_relocations.deinit(self.allocator);
        self.hidden_ret_offsets.deinit(self.allocator);
        self.line_entries.deinit(self.allocator);
    }

    pub fn setRegAllocState(self: *ARM64CodeGen, state: *const regalloc.RegAllocState) void { self.regalloc_state = state; }
    pub fn setFrameSize(self: *ARM64CodeGen, size: u32) void {
        // Minimum 64 bytes: 16 for FP/LR, 48 for Wasm locals during AOT
        self.frame_size = @max(size, 64);
    }
    pub fn setGlobals(self: *ARM64CodeGen, globs: []const ir_mod.Global) void { self.globals = globs; }
    pub fn setTypeRegistry(self: *ARM64CodeGen, reg: *const TypeRegistry) void { self.type_reg = reg; }
    pub fn setDebugInfo(self: *ARM64CodeGen, source_file: []const u8, source_text: []const u8) void {
        self.debug_source_file = source_file;
        self.debug_source_text = source_text;
    }

    /// Get the size of a type in bytes, using type registry for composite types.
    /// Falls back to basicTypeSize for basic types.
    fn getTypeSize(self: *const ARM64CodeGen, type_idx: TypeIndex) u8 {
        // For basic types, use fast path
        if (type_idx < TypeRegistry.FIRST_USER_TYPE) {
            return TypeRegistry.basicTypeSize(type_idx);
        }
        // For composite types (enums, structs), use type registry
        if (self.type_reg) |reg| {
            return @intCast(reg.sizeOf(type_idx));
        }
        // Fallback if no registry
        return TypeRegistry.basicTypeSize(type_idx);
    }

    // ========================================================================
    // Text Output (for debugging)
    // ========================================================================

    /// Generate ARM64 assembly for a function (text output for debugging).
    pub fn generate(self: *ARM64CodeGen, f: *const Func, writer: anytype) !void {
        self.func = f;

        // Function prologue
        try writer.print("_{s}:\n", .{f.name});
        try writer.writeAll("    stp x29, x30, [sp, #-16]!\n");
        try writer.writeAll("    mov x29, sp\n");

        // Generate code for each block
        for (f.blocks.items) |b| {
            try self.generateBlock(b, writer);
        }
    }

    fn generateBlock(self: *ARM64CodeGen, b: *const Block, writer: anytype) !void {
        try writer.print(".Lb{d}:\n", .{b.id});

        for (b.values.items) |v| {
            try self.generateValue(v, writer);
        }

        // Block terminator
        switch (b.kind) {
            .ret => {
                try writer.writeAll("    ldp x29, x30, [sp], #16\n");
                try writer.writeAll("    ret\n");
            },
            .if_ => {
                if (b.succs.len >= 2) {
                    try writer.print("    b.ne .Lb{d}\n", .{b.succs[0].b.id});
                    try writer.print("    b .Lb{d}\n", .{b.succs[1].b.id});
                }
            },
            .plain => {
                if (b.succs.len > 0) {
                    try writer.print("    b .Lb{d}\n", .{b.succs[0].b.id});
                }
            },
            else => {},
        }
    }

    fn generateValue(self: *ARM64CodeGen, v: *const Value, writer: anytype) !void {
        _ = self;

        switch (v.op) {
            .arm64_add => try writer.writeAll("    add ...\n"),
            .arm64_sub => try writer.writeAll("    sub ...\n"),
            .arm64_mul => try writer.writeAll("    mul ...\n"),
            .arm64_ldr => try writer.writeAll("    ldr ...\n"),
            .arm64_str => try writer.writeAll("    str ...\n"),
            .arm64_movz => try writer.print("    movz x?, #{d}\n", .{v.aux_int}),

            .const_int => {
                if (v.aux_int >= 0 and v.aux_int <= 65535) {
                    try writer.print("    mov x?, #{d}    ; v{d}\n", .{ v.aux_int, v.id });
                } else {
                    try writer.print("    ; v{d} = const {d}\n", .{ v.id, v.aux_int });
                }
            },

            else => {
                try writer.print("    ; v{d} = {s}\n", .{ v.id, @tagName(v.op) });
            },
        }
    }

    // ========================================================================
    // Binary Output (for object files)
    // ========================================================================

    /// Emit a 32-bit instruction (little-endian)
    fn emit(self: *ARM64CodeGen, inst: u32) !void {
        const bytes: [4]u8 = @bitCast(inst);
        try self.code.appendSlice(self.allocator, &bytes);
    }

    /// Emit ADD with potentially large immediate (>4095).
    /// Following Go's ARM64 assembler pattern (case 48 in asm7.go):
    /// - For imm <= 4095: single ADD with shift=0
    /// - For imm > 4095: two ADDs - low 12 bits + high 12 bits with shift=1
    /// BUG-034 fix: Handle large struct field offsets.
    fn emitAddImm(self: *ARM64CodeGen, rd: u5, rn: u5, imm: i64) !void {
        if (imm < 0) {
            // Negative - use SUB instead (shouldn't happen for field offsets)
            const abs_imm: u64 = @intCast(-imm);
            if (abs_imm <= 4095) {
                try self.emit(asm_mod.encodeSUBImm(rd, rn, @intCast(abs_imm), 0));
            } else {
                // Split into two SUBs
                const low12: u12 = @intCast(abs_imm & 0xFFF);
                const high12: u12 = @intCast((abs_imm >> 12) & 0xFFF);
                if (low12 != 0) {
                    try self.emit(asm_mod.encodeSUBImm(rd, rn, low12, 0));
                    if (high12 != 0) {
                        try self.emit(asm_mod.encodeSUBImm(rd, rd, high12, 1));
                    }
                } else if (high12 != 0) {
                    try self.emit(asm_mod.encodeSUBImm(rd, rn, high12, 1));
                }
            }
        } else {
            const uimm: u64 = @intCast(imm);
            if (uimm <= 4095) {
                // Small immediate - single ADD
                try self.emit(asm_mod.encodeADDImm(rd, rn, @intCast(uimm), 0));
            } else if (uimm <= 0xFFFFFF) {
                // Medium immediate - split into low 12 + high 12 bits
                // Go pattern: ADD rd, rn, #(imm & 0xFFF); ADD rd, rd, #(imm >> 12), LSL #12
                const low12: u12 = @intCast(uimm & 0xFFF);
                const high12: u12 = @intCast((uimm >> 12) & 0xFFF);
                if (low12 != 0) {
                    try self.emit(asm_mod.encodeADDImm(rd, rn, low12, 0));
                    if (high12 != 0) {
                        try self.emit(asm_mod.encodeADDImm(rd, rd, high12, 1));
                    }
                } else if (high12 != 0) {
                    try self.emit(asm_mod.encodeADDImm(rd, rn, high12, 1));
                }
            } else {
                // Very large immediate (>16MB) - need to load into scratch register
                // Use x16 (IP0) as scratch, then ADD rd, rn, x16
                try self.emitLoadImmediate(16, @intCast(uimm));
                try self.emit(asm_mod.encodeADDReg(rd, rn, 16));
            }
        }
    }

    /// Current code offset
    fn offset(self: *const ARM64CodeGen) u32 {
        return @intCast(self.code.items.len);
    }

    /// Generate binary code for a function.
    pub fn generateBinary(self: *ARM64CodeGen, f: *const Func, name: []const u8) !void {
        self.func = f;
        const start_offset = self.offset();

        debug.log(.codegen, "Generating code for function '{s}', {d} blocks", .{ name, f.blocks.items.len });

        // Clear state for new function
        // Phase 1: Removed value_regs and next_reg
        self.block_offsets.clearRetainingCapacity();
        self.branch_fixups.clearRetainingCapacity();
        self.hidden_ret_offsets.clearRetainingCapacity();
        self.has_hidden_return = false; // Reset hidden return state
        self.hidden_ret_space_needed = 0;
        self.last_source_offset = 0; // Reset for each function to ensure first line entry is recorded

        // BUG-004: Check if this function returns >16B (needs hidden return pointer)
        // The caller passes the return location in x8; we save it to stack.
        // Try method 1: Check function type signature
        if (self.type_reg) |type_reg| {
            const func_type = type_reg.get(f.type_idx);
            if (func_type == .func) {
                const ret_type_idx = func_type.func.return_type;
                const ret_size = self.getTypeSize(ret_type_idx);
                if (ret_size > 16) {
                    self.has_hidden_return = true;
                    debug.log(.codegen, "  Function returns >16B ({d}B), using hidden return via x8 (saved to stack)", .{ret_size});
                }
            }
        }
        // Method 2: If type_idx not set, scan return blocks for return value type
        if (!self.has_hidden_return) {
            for (f.blocks.items) |block| {
                if (block.kind == .ret and block.numControls() > 0) {
                    const ret_val = block.controlValues()[0];
                    const ret_size = self.getTypeSize(ret_val.type_idx);
                    if (ret_size > 16) {
                        self.has_hidden_return = true;
                        debug.log(.codegen, "  Function returns >16B ({d}B) [from ret block], using hidden return via x8 (saved to stack)", .{ret_size});
                        break;
                    }
                }
            }
        }

        // BUG-004: Pre-scan for calls returning >16B to allocate frame space
        // Go's approach: pre-allocate hidden return space rather than dynamic SP adjustment.
        // This ensures local_addr offsets remain valid.
        var cur_hidden_offset: u32 = 0;
        for (f.blocks.items) |block| {
            for (block.values.items) |value| {
                if (value.op == .static_call or value.op == .closure_call) {
                    // Check if this call uses hidden return
                    // Method 1: aux_call knows about it
                    // Method 2: return type > 16B (ARM64 ABI rule)
                    var uses_hidden = false;
                    var ret_size: u32 = 0;

                    // BUG-075 FIX: Always check type size for hidden return, even if aux_call exists
                    // The aux_call may not have hidden return info set if expand_calls didn't run
                    const type_size = self.getTypeSize(value.type_idx);

                    if (value.aux_call) |aux_call| {
                        if (aux_call.usesHiddenReturn()) {
                            uses_hidden = true;
                            ret_size = aux_call.hiddenReturnSize();
                        } else if (type_size > 16) {
                            // BUG-075: aux_call exists but doesn't know about hidden return
                            // Fall back to type size check
                            uses_hidden = true;
                            ret_size = type_size;
                            debug.log(.codegen, "  WARNING: aux_call missing hidden return for >16B type ({d}B)", .{type_size});
                        }
                    } else {
                        // No aux_call - check type size directly
                        if (type_size > 16) {
                            uses_hidden = true;
                            ret_size = type_size;
                        }
                    }

                    if (uses_hidden and ret_size > 0) {
                        const aligned_size = (ret_size + 15) & ~@as(u32, 15); // 16-byte align
                        try self.hidden_ret_offsets.put(self.allocator, value, cur_hidden_offset);
                        debug.log(.codegen, "  Pre-allocated hidden return for call v{d}: offset={d}, size={d}", .{ value.id, cur_hidden_offset, aligned_size });
                        cur_hidden_offset += aligned_size;
                    }
                }
            }
        }
        self.hidden_ret_space_needed = cur_hidden_offset;
        // The hidden return area starts after locals in the frame
        self.hidden_ret_frame_offset = self.frame_size;
        if (cur_hidden_offset > 0) {
            self.frame_size += cur_hidden_offset;
            debug.log(.codegen, "  Added {d}B hidden return space, new frame_size={d}", .{ cur_hidden_offset, self.frame_size });
        }

        // BUG: Save hidden return pointer (x8) to stack instead of x19 to avoid clobbering caller's x19.
        // Add 16 bytes to frame for the saved pointer (aligned to 16).
        if (self.has_hidden_return) {
            self.hidden_ret_ptr_offset = self.frame_size;
            self.frame_size += 16; // 16 bytes for alignment (only need 8 for pointer)
            debug.log(.codegen, "  Added 16B for hidden return ptr at offset {d}, new frame_size={d}", .{ self.hidden_ret_ptr_offset, self.frame_size });
        }

        // Log frame size (set by setFrameSize from stackalloc, possibly adjusted above)
        debug.log(.codegen, "  Stack frame: {d} bytes", .{self.frame_size});

        // Add symbol (prepend underscore for macOS symbol naming convention)
        // All functions are external so they can be called from other functions
        const sym_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{name});
        try self.symbols.append(self.allocator, .{
            .name = sym_name,
            .value = start_offset,
            .section = 1, // __text section
            .external = true, // All functions are external
        });

        // Pre-allocate registers for all phi nodes
        // Phase 1: Removed preAllocatePhiRegisters call - regalloc handles phi allocation

        // Emit prologue (Go's approach: only save FP/LR)
        debug.log(.codegen, "  Emitting prologue", .{});
        try self.emitPrologue();

        // NOTE: Do not auto-insert install_crash_handler here.
        // The cot0 source code (cot0/main.cot) calls it explicitly as the first statement.
        // Auto-inserting it here causes a bug: the call happens before argc/argv are
        // saved from x0/x1 to the stack, so they get clobbered by the function call.

        // Generate code for each block, recording offsets
        for (f.blocks.items) |block| {
            try self.generateBlockBinary(block);
        }

        // Apply branch fixups now that we know all block offsets
        try self.applyBranchFixups();

        debug.log(.codegen, "  Function '{s}' done, code size: {d} bytes", .{ name, self.offset() - start_offset });
    }

    // Phase 1: Removed preAllocatePhiRegisters - regalloc handles phi allocation

    /// Apply all pending branch fixups.
    fn applyBranchFixups(self: *ARM64CodeGen) !void {
        for (self.branch_fixups.items) |fixup| {
            const target_offset = self.block_offsets.get(fixup.target_block_id) orelse continue;
            // Calculate relative offset in instructions (words, not bytes)
            const branch_addr = fixup.code_offset;
            const target_addr = target_offset;
            const relative_offset: i32 = @as(i32, @intCast(target_addr)) - @as(i32, @intCast(branch_addr));
            const offset_words = @divExact(relative_offset, 4);

            // Patch the instruction
            if (fixup.is_cbz) {
                // CBZ/CBNZ: we emitted CBZ with placeholder, need to patch imm19
                // Re-encode with correct offset
                const offset_i19: i19 = @intCast(offset_words);
                const patched = asm_mod.encodeCBZ(0, offset_i19); // rt=0 as placeholder, will be ORed with existing
                // Read existing instruction to get the register
                const existing = std.mem.readInt(u32, self.code.items[fixup.code_offset..][0..4], .little);
                const rt = existing & 0x1F; // Extract Rt from bits 4-0
                const new_inst = (patched & ~@as(u32, 0x1F)) | rt;
                std.mem.writeInt(u32, self.code.items[fixup.code_offset..][0..4], new_inst, .little);
            } else {
                // Unconditional B: offset_words goes into imm26
                const offset_i26: i26 = @intCast(offset_words);
                const patched = asm_mod.encodeB(offset_i26);
                std.mem.writeInt(u32, self.code.items[fixup.code_offset..][0..4], patched, .little);
            }
        }
    }

    /// Emit function prologue.
    /// Go's approach: only save FP (x29) and LR (x30).
    /// Spilled values go to the stack frame, not callee-saved registers.
    fn emitPrologue(self: *ARM64CodeGen) !void {
        // Use frame_size from stackalloc (already 16-byte aligned)
        const aligned_frame = self.frame_size;

        // STP pre-index can handle up to 504 bytes (7-bit signed * 8)
        // For larger frames, use SUB sp first, then STP with signed offset
        if (aligned_frame <= 504) {
            // STP x29, x30, [sp, #-frame_size]!
            const frame_off: i7 = @intCast(-@divExact(@as(i32, @intCast(aligned_frame)), 8));
            try self.emit(asm_mod.encodeSTPPre(29, 30, 31, frame_off));
        } else {
            // Large frame: SUB sp, sp, #frame_size; STP x29, x30, [sp]
            // SUB immediate can handle up to 4095
            if (aligned_frame <= 4095) {
                try self.emit(asm_mod.encodeSUBImm(31, 31, @intCast(aligned_frame), 0));
            } else {
                // Very large frame: need to use a temp register
                // MOV x16, #frame_size; SUB sp, sp, x16
                // NOTE: Must use extended register form when rd/rn is SP,
                // because shifted register form treats reg 31 as XZR.
                // Go's opxrrr() handles this same case.
                try self.emitLoadImmediate(16, @intCast(aligned_frame));
                try self.emit(asm_mod.encodeSUBExtReg(31, 31, 16));
            }
            // STP x29, x30, [sp] (signed offset 0)
            try self.emit(asm_mod.encodeLdpStp(29, 30, 31, 0, .signed_offset, false));
        }

        // Set up frame pointer: ADD x29, sp, #0
        // (In a full implementation, x29 would point to saved FP location)
        try self.emit(asm_mod.encodeADDImm(29, 31, 0, 0));

        // BUG: Save hidden return pointer (x8) to stack instead of x19.
        // This avoids clobbering caller's x19 which is a callee-saved register.
        if (self.has_hidden_return) {
            // STR x8, [sp, #hidden_ret_ptr_offset]
            const offset_scaled: u12 = @intCast(self.hidden_ret_ptr_offset / 8);
            try self.emit(asm_mod.encodeLdrStr(8, 31, offset_scaled, false)); // false = store
            debug.log(.codegen, "  Saved x8 (hidden return ptr) to [sp+{d}]", .{self.hidden_ret_ptr_offset});
        }
    }

    /// Emit function epilogue and return.
    /// Go's approach: only restore FP (x29) and LR (x30).
    fn emitEpilogue(self: *ARM64CodeGen) !void {
        // Use frame_size from stackalloc (already 16-byte aligned)
        const aligned_frame = self.frame_size;

        // For frames <= 504 bytes, use LDP post-index
        // For larger frames, use LDP with offset 0, then ADD sp
        if (aligned_frame <= 504) {
            // LDP x29, x30, [sp], #frame_size
            const frame_off: i7 = @intCast(@divExact(@as(i32, @intCast(aligned_frame)), 8));
            try self.emit(asm_mod.encodeLDPPost(29, 30, 31, frame_off));
        } else {
            // Large frame: LDP x29, x30, [sp]; ADD sp, sp, #frame_size
            // LDP x29, x30, [sp] (signed offset 0)
            try self.emit(asm_mod.encodeLdpStp(29, 30, 31, 0, .signed_offset, true));
            // ADD sp, sp, #frame_size
            if (aligned_frame <= 4095) {
                try self.emit(asm_mod.encodeADDImm(31, 31, @intCast(aligned_frame), 0));
            } else {
                // Very large frame: use temp register
                // NOTE: Must use extended register form when rd/rn is SP
                try self.emitLoadImmediate(16, @intCast(aligned_frame));
                try self.emit(asm_mod.encodeADDExtReg(31, 31, 16));
            }
        }

        // RET
        try self.emit(asm_mod.encodeRET(30));
    }


    /// Emit phi moves for an edge from current block to target block.
    /// For each phi in the target block, find this block's corresponding argument
    /// and move it to the phi's register.
    fn emitPhiMoves(self: *ARM64CodeGen, current_block: *const Block, target_block: *const Block) !void {
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
        // Example: phi1: x1 = x2, phi2: x5 = x1 - if we emit phi1 first, x1 is clobbered!
        //
        // Solution: Two-phase approach
        // Phase 1: Save all source values that might be clobbered to temp registers
        // Phase 2: Copy from temps/sources to final destinations

        const PhiMove = struct {
            src_val: *const Value,
            src_reg: ?u5,
            dest_reg: u5,
            needs_temp: bool,
            temp_reg: u5,
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
                .temp_reg = 0,
            });
        }

        if (moves.items.len == 0) return;

        // Detect conflicts: a source reg that will be overwritten before it's read
        // A move needs a temp if its source_reg equals any other move's dest_reg
        var temp_counter: u5 = 16; // Start with x16, x17 are scratch registers
        for (moves.items, 0..) |*move, i| {
            if (move.src_reg) |src| {
                // Check if this source will be clobbered by an earlier move
                for (moves.items[0..i]) |other| {
                    if (other.dest_reg == src) {
                        // This source will be clobbered before we read it
                        move.needs_temp = true;
                        move.temp_reg = temp_counter;
                        temp_counter += 1;
                        if (temp_counter > 17) {
                            // Ran out of scratch registers, fall back to x9-x15
                            temp_counter = 9;
                        }
                        break;
                    }
                }
            }
        }

        // Phase 1: Save conflicting sources to temp registers
        for (moves.items) |move| {
            if (move.needs_temp) {
                if (move.src_reg) |src| {
                    // MOV temp, src (encoded as ADD temp, src, #0)
                    try self.emit(asm_mod.encodeADDImm(move.temp_reg, src, 0, 0));
                }
            }
        }

        // Phase 2: Emit actual moves
        for (moves.items) |move| {
            if (move.needs_temp) {
                // Source was saved to temp
                if (move.src_reg != null) {
                    // MOV dest, temp
                    try self.emit(asm_mod.encodeADDImm(move.dest_reg, move.temp_reg, 0, 0));
                } else {
                    // Source wasn't in a register, regenerate to dest
                    try self.ensureInReg(move.src_val, move.dest_reg);
                }
            } else {
                // No conflict, emit directly
                if (move.src_reg) |s| {
                    if (s != move.dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(move.dest_reg, s, 0, 0));
                    }
                } else {
                    try self.ensureInReg(move.src_val, move.dest_reg);
                }
            }
        }
    }

    /// Generate binary code for a block
    fn generateBlockBinary(self: *ARM64CodeGen, block: *const Block) !void {
        debug.log(.codegen, "  Block b{d}: {d} values, kind={s}", .{ block.id, block.values.items.len, @tagName(block.kind) });

        // Record block start offset for branch calculations
        try self.block_offsets.put(self.allocator, block.id, self.offset());

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
                    .store, .store_sp, .move, .static_call, .closure_call, .load_reg => true,
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
            // BUG-071 FIX: Don't skip const_int values - regalloc creates rematerialized
            // const_ints (with uses=0) that MUST be generated. These values have their
            // register home cleared after creation, so we can't check regOrNull().
            // Generating an extra MOV is cheap; skipping it causes register clobbering.
            if (value.uses == 0) {
                const has_side_effects = switch (value.op) {
                    .store, .store_sp, .move, .static_call, .closure_call, .load_reg, .copy => true,
                    // Don't skip const_int - may be a rematerialized value
                    .const_int, .const_64, .const_bool, .const_nil => true,
                    else => false,
                };
                if (!has_side_effects) {
                    debug.log(.codegen, "    v{d} = {s} (skipped - dead value)", .{ value.id, @tagName(value.op) });
                    continue;
                }
            }
            try self.generateValueBinary(value);
        }

        // Generate terminator based on block kind
        switch (block.kind) {
            .ret => ret_blk: {
                // Return block - ensure return value is in x0 (and x1 for slices/strings)
                if (block.numControls() > 0) {
                    const ret_val = block.controlValues()[0];

                    // BUG-004: Handle >16B return via hidden pointer
                    // We need to copy the struct data to the hidden return location (saved in stack).
                    // Reference: Go's expand_calls uses OffsetOfResult to get stack addresses.
                    if (self.has_hidden_return) {
                        debug.log(.codegen, "      ret_val.op = {s}, ret_val.id = v{d}", .{ @tagName(ret_val.op), ret_val.id });

                        // For >16B returns, we need the ADDRESS of the struct data.
                        // Depending on how the return value was produced:
                        // - local_addr: direct address of local variable
                        // - load: we need the source address (args[0])
                        // - static_call: for >16B, the register holds the address (our convention)
                        // - copy: follow through to the source
                        var src_addr_reg: u5 = undefined;
                        if (ret_val.op == .local_addr) {
                            // Direct local address
                            src_addr_reg = self.getRegForValue(ret_val) orelse blk: {
                                try self.ensureInReg(ret_val, 16);
                                break :blk @as(u5, 16);
                            };
                        } else if (ret_val.op == .load and ret_val.args.len > 0) {
                            // Load operation - args[0] is the source address
                            // This is common for local struct returns: load(local_addr)
                            const addr_val = ret_val.args[0];
                            debug.log(.codegen, "      load source: {s} v{d}", .{ @tagName(addr_val.op), addr_val.id });
                            src_addr_reg = self.getRegForValue(addr_val) orelse blk: {
                                try self.ensureInReg(addr_val, 16);
                                break :blk @as(u5, 16);
                            };
                        } else if (ret_val.op == .copy and ret_val.args.len > 0) {
                            // Copy - recurse to source
                            const src_val = ret_val.args[0];
                            if (src_val.op == .local_addr or src_val.op == .load) {
                                const actual_src = if (src_val.op == .load and src_val.args.len > 0) src_val.args[0] else src_val;
                                src_addr_reg = self.getRegForValue(actual_src) orelse blk: {
                                    try self.ensureInReg(actual_src, 16);
                                    break :blk @as(u5, 16);
                                };
                            } else {
                                src_addr_reg = self.getRegForValue(src_val) orelse blk: {
                                    try self.ensureInReg(src_val, 16);
                                    break :blk @as(u5, 16);
                                };
                            }
                        } else if (ret_val.op == .static_call) {
                            // BUG FIX: For >16B call results, the result is at a pre-allocated
                            // frame location, NOT in a register. x0 is garbage after hidden return calls.
                            if (self.hidden_ret_offsets.get(ret_val)) |rel_offset| {
                                // Result is at frame location - compute address into x19
                                // Note: Can't use x16/x17 as they're used as temps in the copy loop
                                const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);
                                try self.emit(asm_mod.encodeADDImm(19, 31, frame_offset, 0));
                                src_addr_reg = 19;
                                debug.log(.codegen, "      ret static_call: result at frame offset {d}, loaded addr to x19", .{frame_offset});
                            } else {
                                // Normal call (<=16B) - result in register
                                src_addr_reg = self.getRegForValue(ret_val) orelse blk: {
                                    try self.ensureInReg(ret_val, 16);
                                    break :blk @as(u5, 16);
                                };
                            }
                        } else {
                            // Fallback
                            src_addr_reg = self.getRegForValue(ret_val) orelse blk: {
                                try self.ensureInReg(ret_val, 16);
                                break :blk @as(u5, 16);
                            };
                        }

                        // Get the return type size
                        const ret_size = self.getTypeSize(ret_val.type_idx);

                        // Load hidden return pointer from stack into x18 (temp register)
                        // We saved x8 to [sp, #hidden_ret_ptr_offset] in prologue
                        const dest_ptr_reg: u5 = 18; // Use x18 as temp for destination pointer
                        const ptr_offset_scaled: u12 = @intCast(self.hidden_ret_ptr_offset / 8);
                        try self.emit(asm_mod.encodeLdrStr(dest_ptr_reg, 31, ptr_offset_scaled, true)); // true = load
                        debug.log(.codegen, "      ret hidden: loaded dest ptr from [sp+{d}] to x{d}", .{ self.hidden_ret_ptr_offset, dest_ptr_reg });

                        debug.log(.codegen, "      ret hidden: copying {d}B from [x{d}] to [x{d}]", .{ ret_size, src_addr_reg, dest_ptr_reg });

                        // Copy data in 16-byte chunks using LDP/STP
                        var copy_off: u32 = 0;
                        while (copy_off + 16 <= ret_size) {
                            // LDP x16, x17, [src_addr, #copy_off]
                            const ldp_off: i7 = @intCast(@divExact(@as(i32, @intCast(copy_off)), 8));
                            try self.emit(asm_mod.encodeLdpStp(16, 17, src_addr_reg, ldp_off, .signed_offset, true));
                            // STP x16, x17, [dest_ptr_reg, #copy_off]
                            try self.emit(asm_mod.encodeLdpStp(16, 17, dest_ptr_reg, ldp_off, .signed_offset, false));
                            copy_off += 16;
                        }
                        // Handle remaining 8 bytes if any
                        if (copy_off + 8 <= ret_size) {
                            const ldr_off: u12 = @intCast(@divExact(copy_off, 8));
                            try self.emit(asm_mod.encodeLdrStr(16, src_addr_reg, ldr_off, true));
                            try self.emit(asm_mod.encodeLdrStr(16, dest_ptr_reg, ldr_off, false));
                            copy_off += 8;
                        }
                        // Handle remaining 4 bytes if any
                        if (copy_off + 4 <= ret_size) {
                            // Use word-sized load/store
                            const ld_size = asm_mod.LdStSize.word;
                            try self.emit(asm_mod.encodeLdrStrSized(16, src_addr_reg, @intCast(copy_off), ld_size, true));
                            try self.emit(asm_mod.encodeLdrStrSized(16, dest_ptr_reg, @intCast(copy_off), ld_size, false));
                            copy_off += 4;
                        }
                        // Handle remaining bytes
                        while (copy_off < ret_size) {
                            const ld_size = asm_mod.LdStSize.byte;
                            try self.emit(asm_mod.encodeLdrStrSized(16, src_addr_reg, @intCast(copy_off), ld_size, true));
                            try self.emit(asm_mod.encodeLdrStrSized(16, dest_ptr_reg, @intCast(copy_off), ld_size, false));
                            copy_off += 1;
                        }

                        debug.log(.codegen, "      ret hidden: done copying {d}B", .{ret_size});
                        try self.emitEpilogue();
                        break :ret_blk;
                    }

                    // Handle slice returns: need both ptr (x0) and len (x1)
                    if (ret_val.op == .slice_make and ret_val.args.len >= 2) {
                        // Put ptr in x0
                        const ptr_val = ret_val.args[0];
                        const ptr_reg = self.getRegForValue(ptr_val) orelse blk: {
                            try self.ensureInReg(ptr_val, 0);
                            break :blk @as(u5, 0);
                        };
                        if (ptr_reg != 0) {
                            try self.emit(asm_mod.encodeADDImm(0, ptr_reg, 0, 0)); // MOV x0, ptr_reg
                        }
                        // Put len in x1
                        const len_val = ret_val.args[1];
                        const len_reg = self.getRegForValue(len_val) orelse blk: {
                            try self.ensureInReg(len_val, 1);
                            break :blk @as(u5, 1);
                        };
                        if (len_reg != 1) {
                            try self.emit(asm_mod.encodeADDImm(1, len_reg, 0, 0)); // MOV x1, len_reg
                        }
                        debug.log(.codegen, "      ret slice: ptr=x{d}->x0, len=x{d}->x1", .{ ptr_reg, len_reg });
                    } else if (ret_val.op == .const_string) {
                        // String literal return: ptr in x0, len in x1
                        // The const_string codegen already put ptr in a register
                        const ptr_reg = self.getRegForValue(ret_val) orelse blk: {
                            try self.ensureInReg(ret_val, 0);
                            break :blk @as(u5, 0);
                        };
                        if (ptr_reg != 0) {
                            try self.emit(asm_mod.encodeADDImm(0, ptr_reg, 0, 0)); // MOV x0, ptr_reg
                        }
                        // Get string length from string_literals
                        const string_index: usize = @intCast(ret_val.aux_int);
                        const str_len: i64 = if (string_index < self.func.string_literals.len)
                            @intCast(self.func.string_literals[string_index].len)
                        else
                            0;
                        // Emit MOV x1, #len
                        if (str_len >= 0 and str_len <= 65535) {
                            try self.emit(asm_mod.encodeMOVZ(1, @intCast(str_len), 0));
                        } else {
                            // For longer strings, use MOVZ + MOVK
                            try self.emit(asm_mod.encodeMOVZ(1, @intCast(str_len & 0xFFFF), 0));
                            if (str_len > 0xFFFF) {
                                try self.emit(asm_mod.encodeMOVK(1, @intCast((str_len >> 16) & 0xFFFF), 1));
                            }
                        }
                        debug.log(.codegen, "      ret const_string: ptr=x{d}->x0, len={d}->x1", .{ ptr_reg, str_len });
                    } else if (ret_val.op == .static_call) {
                        // Call result that returns a string: already in x0/x1
                        // Just ensure they stay there (no-op, but log it)
                        debug.log(.codegen, "      ret static_call: result already in x0/x1", .{});
                    } else {
                        try self.moveToX0(ret_val);
                    }
                }
                try self.emitEpilogue();
            },
            .if_ => {
                // Conditional branch
                // succs[0] = then block, succs[1] = else block
                // Control value is the condition (0 = false, nonzero = true)
                if (block.succs.len >= 2) {
                    const cond_val = block.controlValues()[0];
                    const cond_reg = self.getRegForValue(cond_val) orelse blk: {
                        try self.ensureInReg(cond_val, 8); // Use x8 as temp
                        break :blk @as(u5, 8);
                    };

                    const then_block = block.succs[0].b;
                    const else_block = block.succs[1].b;

                    // Emit phi moves for the else branch (taken when CBZ succeeds)
                    try self.emitPhiMoves(block, else_block);

                    // Emit CBZ (branch to else if condition is zero/false)
                    // Record fixup to patch later
                    const cbz_offset = self.offset();
                    try self.emit(asm_mod.encodeCBZ(cond_reg, 0)); // Placeholder offset
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = cbz_offset,
                        .target_block_id = else_block.id,
                        .is_cbz = true,
                    });

                    // Emit phi moves for the then branch
                    try self.emitPhiMoves(block, then_block);

                    // Emit unconditional branch to then block
                    // (in case then block isn't immediately after)
                    const b_offset = self.offset();
                    try self.emit(asm_mod.encodeB(0)); // Placeholder offset
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = b_offset,
                        .target_block_id = then_block.id,
                        .is_cbz = false,
                    });
                }
            },
            .plain => {
                // Plain block - branch to successor if not falling through
                if (block.succs.len > 0) {
                    const target = block.succs[0].b;

                    // Emit phi moves before branching
                    try self.emitPhiMoves(block, target);

                    const b_offset = self.offset();
                    try self.emit(asm_mod.encodeB(0)); // Placeholder
                    try self.branch_fixups.append(self.allocator, .{
                        .code_offset = b_offset,
                        .target_block_id = target.id,
                        .is_cbz = false,
                    });
                }
            },
            else => {},
        }
    }

    /// Generate binary code for a value
    fn generateValueBinary(self: *ARM64CodeGen, value: *const Value) !void {
        debug.log(.codegen, "    v{d} = {s}", .{ value.id, @tagName(value.op) });

        // Record line entry for DWARF if source position changed
        const source_offset = value.pos.line;
        if (source_offset != 0 and source_offset != self.last_source_offset) {
            try self.line_entries.append(self.allocator, .{ .code_offset = self.offset(), .source_offset = source_offset });
            self.last_source_offset = source_offset;
        }

        // Skip evicted constants (will be rematerialized on demand)
        switch (value.op) {
            .const_int, .const_64, .const_bool => if (value.regOrNull() == null) return,
            else => {},
        }

        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(self.getDestRegForValue(value), value.aux_int),
            .const_bool => try self.emitLoadImmediate(self.getDestRegForValue(value), if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emitLoadImmediate(self.getDestRegForValue(value), 0),

            .const_string, .const_ptr => {
                // String literal: emit ADRP + ADD for linker relocation
                const string_index: usize = @intCast(value.aux_int);
                const dest_reg = self.getDestRegForValue(value);
                const str_data = if (string_index < self.func.string_literals.len) self.func.string_literals[string_index] else "";
                const adrp_offset = self.offset();
                try self.emit(asm_mod.encodeADRP(dest_reg, 0));
                const add_offset = self.offset();
                try self.emit(asm_mod.encodeADDImm(dest_reg, dest_reg, 0, 0));
                try self.string_refs.append(self.allocator, .{
                    .adrp_offset = adrp_offset, .add_offset = add_offset,
                    .string_data = try self.allocator.dupe(u8, str_data),
                });
            },

            .add => try self.emitBinaryRegBug071(value, asm_mod.encodeADDReg),
            .sub => try self.emitBinaryRegBug071(value, asm_mod.encodeSUBReg),
            .mul => try self.emitBinaryRegBug071(value, asm_mod.encodeMUL),
            .div => try self.emitBinaryRegBug071(value, asm_mod.encodeSDIV),

            .add_ptr => try self.emitBinaryReg(value, asm_mod.encodeADDReg),
            .sub_ptr => try self.emitBinaryReg(value, asm_mod.encodeSUBReg),

            // === Slice Operations ===
            // Slices are (ptr, len) pairs. For MVP, we track just the pointer.
            // slice_make creates a slice from ptr and len arguments.
            // We store ptr in the dest register and len in the next register.

            .slice_make => {
                // slice_make(ptr, len) -> creates a slice value
                // For MVP: track ptr in dest_reg, ignore len for now
                // (Full impl would need stack storage for 16-byte slice)
                const args = value.args;
                if (args.len >= 2) {
                    const ptr_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    // Copy ptr to dest
                    if (ptr_reg != dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(dest_reg, ptr_reg, 0, 0));
                    }
                    debug.log(.codegen, "      slice_make ptr=x{d} -> x{d}", .{ ptr_reg, dest_reg });
                }
            },

            .slice_ptr => {
                // slice_ptr(slice) -> extract pointer from slice
                // For call results: ptr is in x0
                // For slice_make: ptr is in args[0]
                const args = value.args;
                if (args.len >= 1) {
                    const slice_val = args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    if (slice_val.op == .static_call) {
                        // Call result: check if it used hidden return
                        if (self.hidden_ret_offsets.get(slice_val)) |rel_offset| {
                            // >16B result at frame location - load ptr from offset 0
                            const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);
                            try self.emit(asm_mod.encodeADDImm(16, 31, frame_offset, 0)); // x16 = frame addr
                            try self.emit(asm_mod.encodeLdrStr(dest_reg, 16, 0, true)); // load ptr
                            debug.log(.codegen, "      slice_ptr (>16B call) [SP+{d}] -> x{d}", .{ frame_offset, dest_reg });
                        } else {
                            // <=16B result in x0
                            if (dest_reg != 0) {
                                try self.emit(asm_mod.encodeADDImm(dest_reg, 0, 0, 0)); // MOV dest, x0
                            }
                            debug.log(.codegen, "      slice_ptr (call result) x0 -> x{d}", .{dest_reg});
                        }
                    } else if ((slice_val.op == .slice_make or slice_val.op == .string_make) and slice_val.args.len >= 1) {
                        // slice_make/string_make: ptr is args[0]
                        const ptr_reg = self.getRegForValue(slice_val.args[0]) orelse blk: {
                            try self.ensureInReg(slice_val.args[0], 0);
                            break :blk @as(u5, 0);
                        };
                        if (ptr_reg != dest_reg) {
                            try self.emit(asm_mod.encodeADDImm(dest_reg, ptr_reg, 0, 0));
                        }
                        debug.log(.codegen, "      slice_ptr (slice/string_make) x{d} -> x{d}", .{ ptr_reg, dest_reg });
                    } else {
                        // Other cases: just copy the register
                        const slice_reg = self.getRegForValue(slice_val) orelse blk: {
                            try self.ensureInReg(slice_val, 0);
                            break :blk @as(u5, 0);
                        };
                        if (slice_reg != dest_reg) {
                            try self.emit(asm_mod.encodeADDImm(dest_reg, slice_reg, 0, 0));
                        }
                        debug.log(.codegen, "      slice_ptr (other) x{d} -> x{d}", .{ slice_reg, dest_reg });
                    }
                }
            },

            .slice_len => {
                // slice_len(slice) -> extract length from slice
                // For call results: slice is returned in x0 (ptr) + x1 (len)
                // For slice_make: len is in args[1]
                const args = value.args;
                if (args.len >= 1) {
                    const slice_val = args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    if (slice_val.op == .static_call or slice_val.op == .string_concat) {
                        // Call result: check if it used hidden return
                        if (slice_val.op == .static_call) {
                            if (self.hidden_ret_offsets.get(slice_val)) |rel_offset| {
                                // >16B result at frame location - load len from offset 8
                                const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);
                                try self.emit(asm_mod.encodeADDImm(16, 31, frame_offset, 0)); // x16 = frame addr
                                try self.emit(asm_mod.encodeLdrStr(dest_reg, 16, 1, true)); // load len at +8
                                debug.log(.codegen, "      slice_len (>16B call) [SP+{d}+8] -> x{d}", .{ frame_offset, dest_reg });
                            } else {
                                // <=16B result: len in x1
                                if (dest_reg != 1) {
                                    try self.emit(asm_mod.encodeADDImm(dest_reg, 1, 0, 0)); // MOV dest, x1
                                }
                                debug.log(.codegen, "      slice_len (call result) x1 -> x{d}", .{dest_reg});
                            }
                        } else {
                            // string_concat: always <=16B, len in x1
                            if (dest_reg != 1) {
                                try self.emit(asm_mod.encodeADDImm(dest_reg, 1, 0, 0)); // MOV dest, x1
                            }
                            debug.log(.codegen, "      slice_len (string_concat) x1 -> x{d}", .{dest_reg});
                        }
                    } else if (slice_val.op == .slice_make and slice_val.args.len >= 2) {
                        // slice_make: len is args[1]
                        const len_reg = self.getRegForValue(slice_val.args[1]) orelse blk: {
                            try self.ensureInReg(slice_val.args[1], 1);
                            break :blk @as(u5, 1);
                        };
                        if (len_reg != dest_reg) {
                            try self.emit(asm_mod.encodeADDImm(dest_reg, len_reg, 0, 0));
                        }
                        debug.log(.codegen, "      slice_len (slice_make) x{d} -> x{d}", .{ len_reg, dest_reg });
                    } else if (slice_val.op == .string_make and slice_val.args.len >= 2) {
                        // string_make: len is args[1]
                        const len_reg = self.getRegForValue(slice_val.args[1]) orelse blk: {
                            try self.ensureInReg(slice_val.args[1], 1);
                            break :blk @as(u5, 1);
                        };
                        if (len_reg != dest_reg) {
                            try self.emit(asm_mod.encodeADDImm(dest_reg, len_reg, 0, 0));
                        }
                        debug.log(.codegen, "      slice_len (string_make) x{d} -> x{d}", .{ len_reg, dest_reg });
                    } else if (slice_val.op == .const_string) {
                        // String literal: get length from the string literal data
                        const string_index: usize = @intCast(slice_val.aux_int);
                        const str_len: i64 = if (string_index < self.func.string_literals.len)
                            @intCast(self.func.string_literals[string_index].len)
                        else
                            0;
                        // Emit MOV dest_reg, #len
                        if (str_len >= 0 and str_len <= 65535) {
                            try self.emit(asm_mod.encodeMOVZ(dest_reg, @intCast(str_len), 0));
                        } else {
                            // For longer strings, use MOVZ + MOVK
                            try self.emit(asm_mod.encodeMOVZ(dest_reg, @intCast(str_len & 0xFFFF), 0));
                            if (str_len > 0xFFFF) {
                                try self.emit(asm_mod.encodeMOVK(dest_reg, @intCast((str_len >> 16) & 0xFFFF), 1));
                            }
                        }
                        debug.log(.codegen, "      slice_len (const_string) -> x{d} = {d}", .{ dest_reg, str_len });
                    } else if (slice_val.op == .load) {
                        // Load of 16-byte string from memory (e.g., struct field)
                        // The 16-byte load was done via LDP dest_reg, dest_reg+1, [addr]
                        // So the len component is already in dest_reg+1
                        const type_size = self.getTypeSize(slice_val.type_idx);
                        if (type_size == 16) {
                            // Length is in slice_val's register + 1
                            const load_dest_reg = self.getRegForValue(slice_val) orelse blk: {
                                // This shouldn't happen - the load should already be computed
                                debug.log(.codegen, "      slice_len (load 16B) - load not computed!", .{});
                                break :blk @as(u5, 0);
                            };
                            const len_reg = load_dest_reg + 1;
                            if (len_reg != dest_reg) {
                                try self.emit(asm_mod.encodeADDImm(dest_reg, len_reg, 0, 0));
                            }
                            debug.log(.codegen, "      slice_len (load 16B) x{d} -> x{d}", .{ len_reg, dest_reg });
                        } else {
                            // Non-16-byte load - fallback
                            const slice_reg = self.getRegForValue(slice_val) orelse blk: {
                                try self.ensureInReg(slice_val, 0);
                                break :blk @as(u5, 0);
                            };
                            try self.emit(asm_mod.encodeADDImm(dest_reg, slice_reg, 0, 0));
                            debug.log(.codegen, "      slice_len (load) x{d} -> x{d}", .{ slice_reg, dest_reg });
                        }
                    } else {
                        // Fallback: this shouldn't normally happen
                        const slice_reg = self.getRegForValue(slice_val) orelse blk: {
                            try self.ensureInReg(slice_val, 0);
                            break :blk @as(u5, 0);
                        };
                        try self.emit(asm_mod.encodeADDImm(dest_reg, slice_reg, 0, 0));
                        debug.log(.codegen, "      slice_len (fallback) x{d} -> x{d}", .{ slice_reg, dest_reg });
                    }
                }
            },

            // === String Operations (after expand_calls decomposition) ===

            .string_ptr => {
                // string_ptr(val) -> copy ptr value to dest register
                //
                // After architectural fix with expand_calls + applyDecRules:
                // - string_ptr(string_make(ptr, len)) → copy(ptr)
                // So this op should only see normal values now.
                const args = value.args;
                if (args.len >= 1) {
                    const ptr_val = args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    const src_reg = self.getRegForValue(ptr_val) orelse blk: {
                        try self.ensureInReg(ptr_val, dest_reg);
                        break :blk dest_reg;
                    };
                    if (src_reg != dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(dest_reg, src_reg, 0, 0));
                    }
                    debug.log(.codegen, "      string_ptr x{d} -> x{d}", .{ src_reg, dest_reg });
                }
            },

            .string_len => {
                // string_len(val) -> copy len value to dest register
                //
                // After architectural fix with expand_calls + applyDecRules:
                // - string_len(string_make(ptr, len)) → copy(len)
                // So this op should only see normal values now.
                const args = value.args;
                if (args.len >= 1) {
                    const len_val = args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    const src_reg = self.getRegForValue(len_val) orelse blk: {
                        try self.ensureInReg(len_val, dest_reg);
                        break :blk dest_reg;
                    };
                    if (src_reg != dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(dest_reg, src_reg, 0, 0));
                    }
                    debug.log(.codegen, "      string_len x{d} -> x{d}", .{ src_reg, dest_reg });
                }
            },

            .string_make => {
                // string_make(ptr, len) -> creates a string value from components
                // This is a VIRTUAL op - it doesn't generate code.
                // Following Go's pattern: OpStringMake just aggregates components.
                // The ptr/len values remain in their original registers.
                // Consumers (slice_ptr, slice_len) look at args directly.
                //
                // We just record that string_make's "location" is the same as its ptr arg.
                const args = value.args;
                if (args.len >= 2) {
                    const ptr_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    // NO code generated - just track that this value is at ptr's location
                    debug.log(.codegen, "      string_make (virtual) ptr=x{d}", .{ptr_reg});
                }
            },

            .select_n => {
                // select_n(call, idx) -> extract the idx-th result from a multi-value call
                //
                // Following Go's architecture: select_n is created IMMEDIATELY after the call
                // by expand_calls, so x0/x1 still have the return values.
                // Regalloc tracks select_n as a normal SSA value.
                //
                // ARM64 ABI for 16-byte return (like string = ptr + len):
                // - idx=0 → x0 (ptr)
                // - idx=1 → x8 (len, saved from x1 by caller to avoid clobbering)
                //
                // The call (string_concat/static_call) saves x1 to x8 immediately after
                // return, so select_n[1] reads from x8. This avoids the problem where
                // regalloc assigns x1 to select_n[0], clobbering x1 before select_n[1].
                const args = value.args;
                if (args.len >= 1) {
                    const idx: u5 = @intCast(value.aux_int);
                    const dest_reg = self.getDestRegForValue(value);

                    // idx=0 → x0, idx=1 → x8 (saved from x1)
                    const src_reg: u5 = if (idx == 0) 0 else 8;

                    if (src_reg != dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(dest_reg, src_reg, 0, 0)); // MOV dest, src
                    }
                    debug.log(.codegen, "      select_n[{d}] x{d} -> x{d}", .{ idx, src_reg, dest_reg });
                }
            },

            .string_concat => {
                // String concatenation: call __cot_str_concat(ptr1, len1, ptr2, len2)
                // After expand_calls: args are 4 scalar values (ptr1, len1, ptr2, len2)
                // Returns new string in x0 (ptr), x1 (len)
                // Type is ssa_results, and select_n ops extract the components
                const args = value.args;
                if (args.len >= 4) {
                    debug.log(.codegen, "      string_concat (decomposed): 4 scalar args", .{});

                    // Get ABI info from AuxCall (attached by expand_calls pass)
                    // Reference: Go's regalloc.go regspec() which queries AuxCall.Reg()
                    var fn_name: []const u8 = "__cot_str_concat";
                    if (value.aux_call) |aux_call| {
                        fn_name = aux_call.fn_name;
                        debug.log(.codegen, "      AuxCall: {s}", .{fn_name});
                        if (aux_call.abi_info) |abi_info| {
                            debug.log(.codegen, "      ABI: in_regs={d}, out_regs={d}", .{
                                abi_info.in_registers_used,
                                abi_info.out_registers_used,
                            });
                            // Log expected register assignments from ABI
                            for (abi_info.in_params, 0..) |param, i| {
                                if (param.registers.len > 0) {
                                    const reg = abi.ARM64.regIndexToArm64(param.registers[0]);
                                    debug.log(.codegen, "        arg[{d}] -> x{d}", .{ i, reg });
                                }
                            }
                        }
                    } else {
                        debug.log(.codegen, "      WARNING: no AuxCall attached!", .{});
                    }

                    // Get current registers for each arg
                    const ptr1_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 8);
                        break :blk @as(u5, 8);
                    };
                    const len1_reg = self.getRegForValue(args[1]) orelse blk: {
                        try self.ensureInReg(args[1], 9);
                        break :blk @as(u5, 9);
                    };
                    const ptr2_reg = self.getRegForValue(args[2]) orelse blk: {
                        try self.ensureInReg(args[2], 10);
                        break :blk @as(u5, 10);
                    };
                    const len2_reg = self.getRegForValue(args[3]) orelse blk: {
                        try self.ensureInReg(args[3], 11);
                        break :blk @as(u5, 11);
                    };

                    debug.log(.codegen, "      Current locations: ptr1=x{d}, len1=x{d}, ptr2=x{d}, len2=x{d}", .{
                        ptr1_reg, len1_reg, ptr2_reg, len2_reg,
                    });

                    // Move args to calling convention registers: x0=ptr1, x1=len1, x2=ptr2, x3=len2
                    // Use parallel assignment via scratch registers to avoid clobbering.
                    // Reference: Go's regalloc.go lines 1588-1656 (allocValToReg with constraints)
                    debug.log(.codegen, "      Parallel assignment to x0-x3 via scratch x10-x13", .{});
                    try self.emit(asm_mod.encodeADDImm(10, ptr1_reg, 0, 0)); // x10 = ptr1
                    try self.emit(asm_mod.encodeADDImm(11, len1_reg, 0, 0)); // x11 = len1
                    try self.emit(asm_mod.encodeADDImm(12, ptr2_reg, 0, 0)); // x12 = ptr2
                    try self.emit(asm_mod.encodeADDImm(13, len2_reg, 0, 0)); // x13 = len2
                    try self.emit(asm_mod.encodeADDImm(0, 10, 0, 0)); // x0 = x10 (ptr1)
                    try self.emit(asm_mod.encodeADDImm(1, 11, 0, 0)); // x1 = x11 (len1)
                    try self.emit(asm_mod.encodeADDImm(2, 12, 0, 0)); // x2 = x12 (ptr2)
                    try self.emit(asm_mod.encodeADDImm(3, 13, 0, 0)); // x3 = x13 (len2)

                    // Emit the call
                    // Always prepend underscore for macOS symbol naming
                    const target_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{fn_name});

                    try self.relocations.append(self.allocator, .{
                        .offset = @intCast(self.offset()),
                        .target = target_name,
                    });
                    try self.emit(asm_mod.encodeBL(0));
                    debug.log(.codegen, "      -> BL {s}", .{target_name});

                    // Save x1 to x8 immediately after call.
                    // This is necessary because regalloc might assign x1 to select_n[0],
                    // which would clobber x1 before select_n[1] can read it.
                    // select_n[1] reads from x8 instead of x1.
                    try self.emit(asm_mod.encodeADDImm(8, 1, 0, 0)); // x8 = x1
                    debug.log(.codegen, "      Results: x0 (ptr), x1->x8 (len)", .{});
                } else if (args.len >= 2) {
                    // Old path (before expand_calls): 2 string args
                    // This shouldn't happen anymore, but keep for safety
                    debug.log(.codegen, "      string_concat (old): 2 string args - unexpected!", .{});
                }
            },

            .neg => {
                // NEG Rd, Rm is an alias for SUB Rd, XZR, Rm
                const args = value.args;
                if (args.len >= 1) {
                    const op_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    // XZR is register 31
                    try self.emit(asm_mod.encodeSUBReg(dest_reg, 31, op_reg));
                }
            },

            .not => {
                // Go's pattern: For booleans, NOT is XOR with 1 (flips 0↔1)
                // For integers, NOT is MVN (bitwise complement)
                // Reference: rewriteARM64.go rewriteValueARM64_OpNot
                const args = value.args;
                if (args.len >= 1) {
                    const op_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const dest_reg = self.getDestRegForValue(value);

                    // Check if this is a boolean (1-byte type)
                    const type_size = self.getTypeSize(value.type_idx);
                    if (type_size == 1) {
                        // Boolean: XOR with 1 to flip the bit
                        // Go: (Not x) -> (XOR (MOVDconst [1]) x)
                        // Use x16 as scratch for the constant 1
                        try self.emit(asm_mod.encodeMOVZ(16, 1, 0)); // MOVZ x16, #1
                        try self.emit(asm_mod.encodeEOR(dest_reg, 16, op_reg)); // EOR dest, x16, op
                        debug.log(.codegen, "      -> NOT (bool): MOVZ x16, #1; EOR x{d}, x16, x{d}", .{ dest_reg, op_reg });
                    } else {
                        // Integer: bitwise NOT via MVN
                        try self.emit(asm_mod.encodeMVN(dest_reg, op_reg));
                        debug.log(.codegen, "      -> NOT (int): MVN x{d}, x{d}", .{ dest_reg, op_reg });
                    }
                }
            },

            .mod => {
                // ARM64 doesn't have modulo, compute as: a % b = a - (a / b) * b
                // Use x16 as scratch register
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const op2_reg = self.getRegForValue(args[1]) orelse blk: {
                        try self.ensureInReg(args[1], 1);
                        break :blk @as(u5, 1);
                    };
                    const dest_reg = self.getDestRegForValue(value);
                    // x16 is scratch (IP0)
                    try self.emit(asm_mod.encodeSDIV(16, op1_reg, op2_reg)); // x16 = a / b
                    try self.emit(asm_mod.encodeMUL(16, 16, op2_reg)); // x16 = (a / b) * b
                    try self.emit(asm_mod.encodeSUBReg(dest_reg, op1_reg, 16)); // dest = a - x16
                }
            },

            // === Bitwise Operations ===
            .and_ => try self.emitBinaryReg(value, asm_mod.encodeAND),
            .or_ => try self.emitBinaryReg(value, asm_mod.encodeORR),
            .xor => try self.emitBinaryReg(value, asm_mod.encodeEOR),
            .shl => try self.emitBinaryReg(value, asm_mod.encodeLSL),
            .shr => try self.emitBinaryReg(value, asm_mod.encodeLSR),

            .arg => {
                // Function argument - ARM64 ABI: x0-x7 for first 8, stack for rest
                const arg_idx: usize = @intCast(value.aux_int);
                if (arg_idx >= 8) {
                    // Stack argument - load from caller's stack frame
                    // After prologue, SP points to our frame.
                    // Caller pushed stack args before calling, and after the call,
                    // they're at [SP + frame_size + N*8] where N = arg_idx - 8
                    const dest_reg = self.getDestRegForValue(value);
                    // Stack args are relative to the frame pointer + frame_size
                    // After STP x29, x30, [SP, #-framesize]!, the caller's stack args
                    // are at [x29 + frame_size + (arg_idx-8)*8]
                    // ARM64 LDR unsigned offset is scaled by 8 for 64-bit ops
                    const byte_offset = self.frame_size + (arg_idx - 8) * 8;
                    const stack_offset: u12 = @intCast(@divExact(byte_offset, 8));
                    try self.emit(asm_mod.encodeLdrStr(dest_reg, 29, stack_offset, true));
                    debug.log(.codegen, "      -> LDR x{d}, [x29, #{d}] (stack arg {d})", .{ dest_reg, byte_offset, arg_idx });
                }
                // For register arguments (arg_idx < 8), regalloc already assigned
                // the correct register, so no code needs to be emitted
            },

            // === Comparison Operations ===
            .eq, .ne, .lt, .le, .gt, .ge => {
                const args = value.args;
                if (args.len >= 2) {
                    const op1_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const op2_reg = self.getRegForValue(args[1]) orelse blk: {
                        try self.ensureInReg(args[1], 1);
                        break :blk @as(u5, 1);
                    };
                    const dest_reg = self.getDestRegForValue(value);

                    // CMP op1, op2 (sets flags)
                    try self.emit(asm_mod.encodeCMPReg(op1_reg, op2_reg));

                    // CSET dest, cond (set dest to 1 if condition true, 0 otherwise)
                    const cond: asm_mod.Cond = switch (value.op) {
                        .eq => .eq,
                        .ne => .ne,
                        .lt => .lt,
                        .le => .le,
                        .gt => .gt,
                        .ge => .ge,
                        else => .eq,
                    };
                    try self.emit(asm_mod.encodeCSET(dest_reg, cond));
                }
            },

            // Wasm eqz: compare with zero, set 1 if zero, 0 otherwise
            .wasm_i32_eqz, .wasm_i64_eqz => {
                const args = value.args;
                if (args.len >= 1) {
                    const op_reg = self.getRegForValue(args[0]) orelse blk: {
                        try self.ensureInReg(args[0], 0);
                        break :blk @as(u5, 0);
                    };
                    const dest_reg = self.getDestRegForValue(value);

                    // CMP op, #0 (sets flags)
                    try self.emit(asm_mod.encodeCMPImm(op_reg, 0));

                    // CSET dest, EQ (set to 1 if equal to zero)
                    try self.emit(asm_mod.encodeCSET(dest_reg, .eq));
                    debug.log(.codegen, "      -> CMP x{d}, #0; CSET x{d}, EQ (eqz)", .{ op_reg, dest_reg });
                }
            },

            .copy => {
                // Copy emits MOV from source to dest
                // Following Go's pattern: copy is a simple register-to-register move
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    // Get source register - must exist in value_regs or regalloc
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        // Source not in a register - try to regenerate
                        debug.log(.codegen, "      copy: src v{d} not in register, regenerating", .{src.id});
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    if (src_reg != dest_reg) {
                        // Emit MOV dest, src (using ADD dest, src, #0)
                        try self.emit(asm_mod.encodeADDImm(dest_reg, src_reg, 0, 0));
                        debug.log(.codegen, "      copy x{d} -> x{d}", .{ src_reg, dest_reg });
                    } else {
                        debug.log(.codegen, "      copy (no-op, same reg x{d})", .{dest_reg});
                    }
                }
            },

            // Sign extension ops
            .sign_ext8to16, .sign_ext8to32, .sign_ext8to64 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // SXTB: sign-extend byte
                    if (value.op == .sign_ext8to64) {
                        try self.emit(asm_mod.encodeSXTB64(dest_reg, src_reg));
                    } else {
                        try self.emit(asm_mod.encodeSXTB32(dest_reg, src_reg));
                    }
                }
            },
            .sign_ext16to32, .sign_ext16to64 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // SXTH: sign-extend halfword
                    if (value.op == .sign_ext16to64) {
                        try self.emit(asm_mod.encodeSXTH64(dest_reg, src_reg));
                    } else {
                        try self.emit(asm_mod.encodeSXTH32(dest_reg, src_reg));
                    }
                }
            },
            .sign_ext32to64 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // SXTW: sign-extend word to 64-bit
                    try self.emit(asm_mod.encodeSXTW(dest_reg, src_reg));
                }
            },

            // Zero extension ops
            .zero_ext8to16, .zero_ext8to32, .zero_ext8to64 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // UXTB: zero-extend byte
                    if (value.op == .zero_ext8to64) {
                        try self.emit(asm_mod.encodeUXTB64(dest_reg, src_reg));
                    } else {
                        try self.emit(asm_mod.encodeUXTB32(dest_reg, src_reg));
                    }
                }
            },
            .zero_ext16to32, .zero_ext16to64 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    const src_reg = self.getRegForValue(src) orelse blk: {
                        try self.ensureInReg(src, dest_reg);
                        break :blk dest_reg;
                    };
                    // UXTH: zero-extend halfword
                    if (value.op == .zero_ext16to64) {
                        try self.emit(asm_mod.encodeUXTH64(dest_reg, src_reg));
                    } else {
                        try self.emit(asm_mod.encodeUXTH32(dest_reg, src_reg));
                    }
                }
            },
            .zero_ext32to64 => {
                // Zero-extend 32-bit to 64-bit: just use 32-bit MOV (upper bits auto-cleared)
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    try self.ensureInReg(src, dest_reg);
                    // Writing to Wd automatically zeroes upper 32 bits of Xd
                }
            },

            // Truncation ops - just use narrower register form (mask if needed)
            .trunc64to32, .trunc32to16, .trunc16to8, .trunc64to16, .trunc64to8, .trunc32to8 => {
                const dest_reg = self.getDestRegForValue(value);
                const args = value.args;
                if (args.len > 0) {
                    const src = args[0];
                    try self.ensureInReg(src, dest_reg);
                    // No extra instruction needed - the narrower value is just
                    // the lower bits of the register. If we need to store it,
                    // the store instruction will use the appropriate size.
                }
            },

            .phi => {
                // Phi values are handled at block boundaries (see emitPhiMoves)
                // Register is assigned by regalloc
                // Nothing to do here - the phi moves happen at predecessor blocks
            },

            .fwd_ref => {
                // Should not appear after phi insertion
                std.debug.panic("fwd_ref should not appear after phi insertion", .{});
            },

            .static_call => static_call_blk: {
                // Function call - ARM64 ABI: args in x0-x7, result in x0
                const args = value.args;

                // Get target function name from aux.string
                const raw_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown",
                };

                // BUG-032 FIX: Check if this is a known variadic function
                // For variadic functions, args beyond fixed_count go on stack
                const variadic_fixed_args = getVariadicFixedArgCount(raw_name);
                if (variadic_fixed_args != null) {
                    debug.log(.codegen, "      variadic call: {s} with {d} fixed args", .{ raw_name, variadic_fixed_args.? });
                }

                // Prepend underscore for macOS symbol naming convention
                const target_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{raw_name});

                // BUG-004: Handle >16B return via hidden pointer (ARM64 AAPCS64)
                // Go's pattern: pre-allocate space in frame rather than dynamic SP adjustment.
                // This keeps local_addr SP-relative offsets stable.
                if (self.hidden_ret_offsets.get(value)) |rel_offset| {
                    // Get return size from aux_call if available, otherwise from type size
                    const ret_size: u32 = if (value.aux_call) |aux_call|
                        aux_call.hiddenReturnSize()
                    else
                        self.getTypeSize(value.type_idx);

                    // Compute frame-relative address for this call's hidden return area
                    // hidden_ret_frame_offset is the base (after locals), rel_offset is per-call
                    const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);

                    debug.log(.codegen, "      static_call {s}: >16B return ({d}B), frame offset={d}", .{ raw_name, ret_size, frame_offset });

                    // BUG-075 FIX: Setup arguments BEFORE setting x8 for hidden return.
                    // This ensures any argument that was in x8 gets moved to x0-x7 first,
                    // before we clobber x8 with the hidden return pointer.
                    // 1. Setup arguments in x0-x7 (and stack for variadic/overflow)
                    const stack_cleanup = try self.setupCallArgsWithVariadic(args, variadic_fixed_args);

                    // 2. Set x8 to point to return location within frame (AFTER args are safe)
                    // ADD x8, sp, #frame_offset
                    try self.emit(asm_mod.encodeADDImm(8, 31, frame_offset, 0));

                    // 3. Record relocation and make the call
                    try self.relocations.append(self.allocator, .{
                        .offset = @intCast(self.offset()),
                        .target = target_name,
                    });
                    try self.emit(asm_mod.encodeBL(0));

                    // 3b. Clean up stack arguments if any
                    if (stack_cleanup > 0) {
                        try self.emit(asm_mod.encodeADDImm(31, 31, stack_cleanup, 0));
                        debug.log(.codegen, "      stack cleanup: ADD SP, SP, #{d}", .{stack_cleanup});
                    }

                    // BUG-072 FIX: Reset stack adjustment after call completes
                    self.call_stack_adjustment = 0;

                    // 4. After call: result is at [sp + frame_offset]. Store address in scratch reg.
                    // CRITICAL: Use x0 since the call is complete and x0 is free.
                    // DO NOT use x16 because the store code uses LDP x16, x17, [src_reg]
                    // which would clobber x16 if src_reg == x16.
                    const dest_reg: u5 = 0; // x0 is free after call
                    // ADD dest_reg, sp, #frame_offset
                    try self.emit(asm_mod.encodeADDImm(dest_reg, 31, frame_offset, 0));

                    // The result "value" is now the ADDRESS where the struct data lives.
                    // Subsequent store operations will copy from this address.
                    debug.log(.codegen, "      -> BL {s}, >16B result at [x{d}] (SP+{d})", .{ raw_name, dest_reg, frame_offset });
                    break :static_call_blk;
                }

                // Normal call path (return <= 16B fits in registers)
                // Use parallel copy to move arguments to x0-x7 (and stack for variadic/overflow)
                // This prevents clobbering when args are in each other's target registers
                // E.g., if arg0 is in x1 and arg1 is in x0, naive sequential moves fail
                const stack_cleanup = try self.setupCallArgsWithVariadic(args, variadic_fixed_args);

                // Record relocation for linker to resolve
                try self.relocations.append(self.allocator, .{
                    .offset = @intCast(self.offset()),
                    .target = target_name,
                });

                // Emit BL with offset 0 (linker will fix)
                try self.emit(asm_mod.encodeBL(0));

                // Clean up stack arguments if any
                if (stack_cleanup > 0) {
                    try self.emit(asm_mod.encodeADDImm(31, 31, stack_cleanup, 0));
                    debug.log(.codegen, "      stack cleanup: ADD SP, SP, #{d}", .{stack_cleanup});
                }

                // BUG-072 FIX: Reset stack adjustment after call completes
                self.call_stack_adjustment = 0;

                // For multi-value returns (16-byte types like strings), save x1 to x8.
                // This is necessary because regalloc might assign x1 to select_n[0],
                // which would clobber x1 before select_n[1] can read it.
                // select_n[1] reads from x8 instead of x1.
                const type_size = self.getTypeSize(value.type_idx);
                const is_multi_value = (type_size == 16) or (value.type_idx == TypeRegistry.SSA_RESULTS);
                if (is_multi_value) {
                    try self.emit(asm_mod.encodeADDImm(8, 1, 0, 0)); // x8 = x1
                    debug.log(.codegen, "      -> BL {s}, result in x0+x1, x1->x8", .{raw_name});
                } else {
                    debug.log(.codegen, "      -> BL {s}, result in x0", .{raw_name});
                }

                // Result is in x0 - regalloc will handle spill/reload if needed
                // Go's approach: don't move to callee-saved, let regalloc spill
            },

            .closure_call => {
                // Indirect function call through function pointer (Go: ClosureCall)
                // ARM64 ABI: args in x0-x7, result in x0
                // First arg is function pointer, rest are actual call arguments
                const args = value.args;
                if (args.len == 0) {
                    debug.log(.codegen, "      closure_call: no function pointer!", .{});
                    return;
                }

                // Get function pointer
                const fn_ptr = args[0];
                const fn_ptr_reg = self.getRegForValue(fn_ptr) orelse blk: {
                    // Load function pointer to x16 (scratch register)
                    try self.ensureInReg(fn_ptr, 16);
                    break :blk @as(u5, 16);
                };

                // Setup actual arguments (skip args[0] which is the function pointer)
                // Use x16 as temp, but function pointer might be there, so save it first
                const actual_args = args[1..];
                if (actual_args.len > 0) {
                    // If fn_ptr is in x16, move it somewhere safe first
                    var target_reg: u5 = fn_ptr_reg;
                    if (fn_ptr_reg < 8 or fn_ptr_reg == 16) {
                        // Function pointer is in an arg register or x16, save to x17
                        try self.emit(asm_mod.encodeADDImm(17, fn_ptr_reg, 0, 0)); // MOV x17, fn_ptr
                        target_reg = 17;
                    }

                    // Setup actual arguments (including stack args if > 8)
                    const stack_cleanup = try self.setupCallArgs(actual_args);

                    // Emit BLR to call through function pointer
                    try self.emit(asm_mod.encodeBLR(target_reg));

                    // Clean up stack arguments if any
                    if (stack_cleanup > 0) {
                        try self.emit(asm_mod.encodeADDImm(31, 31, stack_cleanup, 0));
                        debug.log(.codegen, "      stack cleanup: ADD SP, SP, #{d}", .{stack_cleanup});
                    }
                    debug.log(.codegen, "      -> BLR x{d} (indirect call with {} args), result in x0", .{ target_reg, actual_args.len });
                } else {
                    // No arguments, just call through function pointer
                    try self.emit(asm_mod.encodeBLR(fn_ptr_reg));
                    debug.log(.codegen, "      -> BLR x{d} (indirect call, no args), result in x0", .{fn_ptr_reg});
                }

                // Result is in x0
            },

            .store_reg => {
                // Spill a value to a stack slot (Go's approach)
                // Stack offset assigned by stackalloc via f.setHome()
                if (value.args.len > 0) {
                    const src_value = value.args[0];

                    // Get stack offset from stackalloc's assignment
                    const loc = value.getHome() orelse {
                        debug.log(.codegen, "      store_reg v{d}: NO stack location!", .{value.id});
                        return;
                    };
                    // ARM64 LDR/STR unsigned offset is scaled by 8 for 64-bit ops
                    const byte_off = loc.stackOffset();
                    const spill_off: u12 = @intCast(@divExact(byte_off, 8));

                    // Get the register holding the value to spill
                    const src_reg = self.getRegForValue(src_value) orelse 0;

                    // STR Xn, [SP, #offset]
                    try self.emit(asm_mod.encodeLdrStr(src_reg, 31, spill_off, false)); // false = store
                    debug.log(.codegen, "      -> STR x{d}, [SP, #{d}]", .{ src_reg, spill_off });
                }
            },

            .load_reg => {
                // Reload a value from a stack slot (Go's approach)
                if (value.args.len > 0) {
                    const spill_value = value.args[0];

                    // Get stack offset from the store_reg value's location
                    const loc = spill_value.getHome() orelse {
                        debug.log(.codegen, "      load_reg v{d}: source store_reg has NO location!", .{value.id});
                        return;
                    };
                    // ARM64 LDR/STR unsigned offset is scaled by 8 for 64-bit ops
                    const byte_off = loc.stackOffset();
                    const spill_off: u12 = @intCast(@divExact(byte_off, 8));

                    // Get destination register from regalloc
                    const dest_reg = self.getDestRegForValue(value);

                    // LDR Xn, [SP, #offset]
                    try self.emit(asm_mod.encodeLdrStr(dest_reg, 31, spill_off, true)); // true = load
                    debug.log(.codegen, "      -> LDR x{d}, [SP, #{d}]", .{ dest_reg, spill_off });

                }
            },

            // === Struct Field Access Operations ===
            // Following Go's pattern: LocalAddr + OffPtr + Load/Store

            .local_addr => {
                // Compute address of a local variable on the stack
                // aux_int contains the local index
                const local_idx: usize = @intCast(value.aux_int);
                // BUG-021 FIX: Use regOrNull to handle evicted local_addr values
                // If evicted, the value was rematerialized elsewhere and we can skip this.
                // If not evicted, emit the address computation.
                const maybe_reg = value.regOrNull();
                if (maybe_reg == null) {
                    // This local_addr was evicted - a rematerialized copy was inserted where needed
                    debug.log(.codegen, "      (local_addr skipped - evicted, will be rematerialized)", .{});
                    return;
                }
                const dest_reg: u5 = @intCast(maybe_reg.?);

                // Get stack offset from local_offsets (set by stackalloc)
                if (local_idx < self.func.local_offsets.len) {
                    const byte_off = self.func.local_offsets[local_idx];
                    // Use emitAddImm to handle large offsets (>4095 bytes)
                    try self.emitAddImm(dest_reg, 31, @intCast(byte_off));
                    debug.log(.codegen, "      -> ADD x{d}, SP, #{d} (local_addr {d})", .{ dest_reg, byte_off, local_idx });
                } else {
                    // Fallback: offset 0 (shouldn't happen)
                    try self.emit(asm_mod.encodeADDImm(dest_reg, 31, 0, 0));
                    debug.log(.codegen, "      -> ADD x{d}, SP, #0 (local_addr {d} - NO OFFSET!)", .{ dest_reg, local_idx });
                }
            },

            .global_addr => {
                // Address of a global variable
                // aux.string contains the global variable name
                // BUG-021 FIX: Handle evicted values
                const maybe_reg = value.regOrNull();
                if (maybe_reg == null) {
                    debug.log(.codegen, "      (global_addr skipped - evicted)", .{});
                    return;
                }
                const dest_reg: u5 = @intCast(maybe_reg.?);
                const global_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown_global",
                };

                // Emit ADRP + ADD to load the global address
                // Linker will fix up the actual address via relocations
                const adrp_offset = self.offset();
                try self.emit(asm_mod.encodeADRP(dest_reg, 0));

                const add_offset = self.offset();
                try self.emit(asm_mod.encodeADDImm(dest_reg, dest_reg, 0, 0));

                // Record global reference for relocation during finalize()
                // Prepend underscore for macOS symbol naming convention
                const mangled_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{global_name});
                try self.func_refs.append(self.allocator, .{
                    .adrp_offset = adrp_offset,
                    .add_offset = add_offset,
                    .func_name = mangled_name, // Reusing func_refs for globals too
                });

                debug.log(.codegen, "      -> ADRP+ADD x{d}, _{s} (global)", .{ dest_reg, global_name });
            },

            // === Wasm Stack Pointer Operations for AOT ===
            // In Wasm, global 0 is a pointer into linear memory (the "Wasm stack").
            // For AOT, we use the native stack frame as the Wasm memory area.
            // The prologue already allocates frame space, so we can use SP as the base.
            // Note: store_sp is still a no-op to prevent corrupting the native stack.

            .sp => {
                // Get wasm stack pointer - ALWAYS use x16 to avoid clobbering args.
                // IMPORTANT: Offset past saved FP/LR (16 bytes at [SP]).
                // The prologue does STP x29, x30, [sp, #-frame]!, so [SP] has saved regs.
                // Wasm locals should start at SP+16 to avoid overwriting them.
                const dest_reg: u5 = 16;
                // ADD x16, SP, #16 (skip over saved FP/LR)
                try self.emit(asm_mod.encodeADDImm(dest_reg, 31, 16, 0));
                debug.log(.codegen, "      -> ADD x{d}, SP, #16 (wasm sp, skip FP/LR)", .{dest_reg});
            },

            .store_sp => {
                // Don't actually modify native SP based on Wasm adjustments.
                // The native stack frame is fixed by the prologue.
                // This prevents stack corruption from Wasm's frame allocation patterns.
                debug.log(.codegen, "      (store_sp - ignored for AOT)", .{});
            },

            .addr => {
                // Address of a symbol (function for function pointers)
                // aux.string contains the symbol name
                // BUG-021 FIX: Handle evicted values
                const maybe_reg = value.regOrNull();
                if (maybe_reg == null) {
                    debug.log(.codegen, "      (addr skipped - evicted)", .{});
                    return;
                }
                const dest_reg: u5 = @intCast(maybe_reg.?);
                const func_name = switch (value.aux) {
                    .string => |s| s,
                    else => "unknown",
                };

                // Emit ADRP + ADD to load the function address
                // Linker will fix up the actual address via relocations
                const adrp_offset = self.offset();
                try self.emit(asm_mod.encodeADRP(dest_reg, 0));

                const add_offset = self.offset();
                try self.emit(asm_mod.encodeADDImm(dest_reg, dest_reg, 0, 0));

                // Record function reference for relocation during finalize()
                // Prepend underscore for macOS symbol naming convention
                const mangled_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{func_name});
                try self.func_refs.append(self.allocator, .{
                    .adrp_offset = adrp_offset,
                    .add_offset = add_offset,
                    .func_name = mangled_name,
                });

                debug.log(.codegen, "      -> ADRP+ADD x{d}, _{s}", .{ dest_reg, func_name });
            },

            .off_ptr => {
                // Add field offset to base pointer
                // aux_int contains the offset, arg[0] is the base pointer
                if (value.args.len > 0) {
                    const base = value.args[0];
                    const field_off: i64 = value.aux_int;
                    const dest_reg = self.getDestRegForValue(value);

                    // WORKAROUND: If base is local_addr and its register was clobbered,
                    // regenerate it. This happens when regalloc reuses the register for
                    // a load, but off_ptr still needs the original address.
                    var base_reg: u5 = undefined;
                    if (base.op == .local_addr) {
                        // Regenerate local_addr directly - don't trust the cached register
                        // because it may have been clobbered by a load that used the same register
                        const local_idx: usize = @intCast(base.aux_int);
                        if (local_idx < self.func.local_offsets.len) {
                            const local_offset = self.func.local_offsets[local_idx];
                            // Use emitAddImm to handle large offsets (>4095 bytes)
                            try self.emitAddImm(dest_reg, 31, @intCast(local_offset));
                            base_reg = dest_reg;
                            debug.log(.codegen, "      -> ADD x{d}, SP, #{d} (regen local_addr {d})", .{ dest_reg, local_offset, local_idx });
                        } else {
                            // Fallback - shouldn't happen but be safe
                            base_reg = self.getRegForValue(base) orelse blk: {
                                try self.ensureInReg(base, dest_reg);
                                break :blk dest_reg;
                            };
                        }
                    } else {
                        base_reg = self.getRegForValue(base) orelse blk: {
                            // Need to get base into a register first
                            try self.ensureInReg(base, dest_reg);
                            break :blk dest_reg;
                        };
                    }

                    // ADD Rd, Rn, #offset (only if there's an actual offset to add)
                    if (field_off != 0) {
                        try self.emitAddImm(dest_reg, base_reg, field_off);
                        debug.log(.codegen, "      -> ADD x{d}, x{d}, #{d} (off_ptr)", .{ dest_reg, base_reg, field_off });
                    } else if (base_reg != dest_reg) {
                        try self.emit(asm_mod.encodeADDImm(dest_reg, base_reg, 0, 0));
                        debug.log(.codegen, "      -> ADD x{d}, x{d}, #0 (off_ptr copy)", .{ dest_reg, base_reg });
                    }
                }
            },

            .load => {
                // Load from memory address
                // arg[0] is the address
                if (value.args.len > 0) {
                    const addr = value.args[0];
                    const dest_reg = self.getDestRegForValue(value);

                    const addr_reg = self.getRegForValue(addr) orelse blk: {
                        // Address should already be computed - use x16 as scratch
                        try self.ensureInReg(addr, 16);
                        break :blk @as(u5, 16);
                    };

                    // CRITICAL: Use type-sized load instruction
                    // This is where the bug was: always using 64-bit LDR for all types
                    // BUG-003 fix: Use getTypeSize to handle enum types correctly
                    const type_size = self.getTypeSize(value.type_idx);
                    const type_name = TypeRegistry.basicTypeName(value.type_idx);

                    // BUG-003: Special handling for 16-byte struct loads (returned in x0+x1)
                    if (type_size == 16) {
                        // Use LDP to load both 8-byte halves
                        // LDP dest_reg, dest_reg+1, [addr_reg]
                        // For struct returns, load into x0 and x1
                        try self.emit(asm_mod.encodeLdpStp(dest_reg, dest_reg + 1, addr_reg, 0, .signed_offset, true));
                        debug.log(.codegen, "      -> LDP x{d}, x{d}, [x{d}] (load {s}, {d}B)", .{
                            dest_reg,
                            dest_reg + 1,
                            addr_reg,
                            type_name,
                            type_size,
                        });
                    } else {
                        const ld_size = asm_mod.LdStSize.fromBytes(type_size);
                        try self.emit(asm_mod.encodeLdrStrSized(dest_reg, addr_reg, 0, ld_size, true));
                        debug.log(.codegen, "      -> LDR{s} {s}{d}, [{s}{d}] (load {s}, {d}B)", .{
                            ld_size.name(),
                            if (ld_size == .dword) "x" else "w",
                            dest_reg,
                            "x",
                            addr_reg,
                            type_name,
                            type_size,
                        });
                    }
                }
            },

            .store => {
                // Store to memory address
                // arg[0] is the address, arg[1] is the value to store
                if (value.args.len >= 2) {
                    const addr = value.args[0];
                    const val = value.args[1];

                    const addr_reg = self.getRegForValue(addr) orelse blk: {
                        const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                        try self.ensureInReg(addr, temp_reg);
                        break :blk temp_reg;
                    };

                    // CRITICAL: Use type-sized store instruction
                    // BUG-003 fix: Use getTypeSize to handle enum types correctly
                    // If aux_int is set (from store_index_local/store_index_value),
                    // use it as the store size. This fixes storing u8 values where the
                    // source is an untyped integer constant (8 bytes) but destination is 1 byte.
                    const type_size: u32 = if (value.aux_int > 0) @intCast(value.aux_int) else self.getTypeSize(val.type_idx);

                    // Special handling for 16-byte types (string/slice)
                    // These are stored as (ptr, len) pairs
                    if (type_size == 16 and val.op == .string_concat) {
                        // After string_concat: ptr in x0, len in x8 (saved immediately after call)
                        // IMPORTANT: addr_reg might be x0 which would clobber the ptr
                        // Use x9 as a temp for the address if needed
                        var actual_addr_reg = addr_reg;
                        if (addr_reg == 0 or addr_reg == 8) {
                            actual_addr_reg = 9;
                            try self.emit(asm_mod.encodeADDImm(9, addr_reg, 0, 0)); // x9 = addr_reg
                        }
                        // STP x0, x8, [actual_addr_reg] (ptr and len)
                        try self.emit(asm_mod.encodeLdpStp(0, 8, actual_addr_reg, 0, .signed_offset, false));
                        debug.log(.codegen, "      -> STP x0, x8, [x{d}] (store string_concat 16B)", .{actual_addr_reg});
                    } else if (type_size > 16 and val.op == .static_call) {
                        // BUG-004 FIX: For >16B static_call, result is at pre-allocated frame location
                        // NOT in a register. Use hidden_ret_offsets to find the source address.
                        var src_reg: u5 = undefined;
                        if (self.hidden_ret_offsets.get(val)) |rel_offset| {
                            // Compute source address from frame offset into x19
                            // Note: Can't use x16/x17 as they're used as temps in the copy loop
                            const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);
                            try self.emit(asm_mod.encodeADDImm(19, 31, frame_offset, 0)); // x19 = SP + offset
                            src_reg = 19;
                            debug.log(.codegen, "      store >16B: src at frame offset {d} -> x19", .{frame_offset});
                        } else {
                            // Fallback (shouldn't happen for >16B)
                            src_reg = self.getRegForValue(val) orelse blk: {
                                const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                                try self.ensureInReg(val, temp_reg);
                                break :blk temp_reg;
                            };
                        }

                        debug.log(.codegen, "      store >16B: copying {d}B from [x{d}] to [x{d}]", .{ type_size, src_reg, addr_reg });

                        // Copy data in 16-byte chunks using LDP/STP
                        var copy_off: u32 = 0;
                        while (copy_off + 16 <= type_size) {
                            const ldp_off: i7 = @intCast(@divExact(@as(i32, @intCast(copy_off)), 8));
                            try self.emit(asm_mod.encodeLdpStp(16, 17, src_reg, ldp_off, .signed_offset, true));
                            try self.emit(asm_mod.encodeLdpStp(16, 17, addr_reg, ldp_off, .signed_offset, false));
                            copy_off += 16;
                        }
                        if (copy_off + 8 <= type_size) {
                            const ldr_off: u12 = @intCast(@divExact(copy_off, 8));
                            try self.emit(asm_mod.encodeLdrStr(16, src_reg, ldr_off, true));
                            try self.emit(asm_mod.encodeLdrStr(16, addr_reg, ldr_off, false));
                            copy_off += 8;
                        }
                        while (copy_off < type_size) {
                            const ld_size = asm_mod.LdStSize.byte;
                            try self.emit(asm_mod.encodeLdrStrSized(16, src_reg, @intCast(copy_off), ld_size, true));
                            try self.emit(asm_mod.encodeLdrStrSized(16, addr_reg, @intCast(copy_off), ld_size, false));
                            copy_off += 1;
                        }
                        debug.log(.codegen, "      -> copied {d}B struct from [x{d}] to [x{d}]", .{ type_size, src_reg, addr_reg });
                    } else if (type_size > 16 and val.op == .load) {
                        // BUG-018 FIX: For >16B loads, can't fit in registers.
                        // Do memory-to-memory copy using source address from load's argument.
                        // Go pattern: LoweredMove does memory-to-memory copy.
                        // Reference: ~/learning/go/src/cmd/compile/internal/ssa/_gen/ARM64Ops.go
                        //
                        // CRITICAL: The source address may have been computed earlier and its
                        // register overwritten. We must re-compute it here.
                        const src_addr = val.args[0];
                        var src_reg: u5 = 19; // Use x19 for source address

                        // Re-compute the source address into x19
                        // Check if it's a local_addr - compute fresh from SP
                        if (src_addr.op == .local_addr) {
                            const local_idx: u32 = @intCast(src_addr.aux_int);
                            if (local_idx < self.func.local_offsets.len) {
                                const local_off = self.func.local_offsets[local_idx];
                                // Use emitAddImm to handle large offsets (>4095 bytes)
                                try self.emitAddImm(src_reg, 31, @intCast(local_off));
                                debug.log(.codegen, "      store >16B: recomputed local_addr {d} at SP+{d} -> x{d}", .{ local_idx, local_off, src_reg });
                            } else {
                                debug.log(.codegen, "      store >16B: local_idx {d} out of range!", .{local_idx});
                            }
                        } else {
                            // For other address types, try to get from register or recompute
                            const maybe_reg = self.getRegForValue(src_addr);
                            if (maybe_reg) |reg| {
                                if (reg != addr_reg) {
                                    src_reg = reg;
                                } else {
                                    // Same register - need to save addr_reg first
                                    try self.emit(asm_mod.encodeADDImm(18, addr_reg, 0, 0)); // x18 = dst
                                    try self.ensureInReg(src_addr, 19);
                                    // Restore dst to addr_reg (but we changed it to x18)
                                    // Actually simpler: just put src in x19
                                    try self.ensureInReg(src_addr, src_reg);
                                }
                            } else {
                                try self.ensureInReg(src_addr, src_reg);
                            }
                        }

                        debug.log(.codegen, "      store >16B load: copying {d}B from [x{d}] to [x{d}]", .{ type_size, src_reg, addr_reg });

                        // Copy data in 16-byte chunks using LDP/STP
                        var copy_off: u32 = 0;
                        while (copy_off + 16 <= type_size) {
                            const ldp_off: i7 = @intCast(@divExact(@as(i32, @intCast(copy_off)), 8));
                            try self.emit(asm_mod.encodeLdpStp(16, 17, src_reg, ldp_off, .signed_offset, true));
                            try self.emit(asm_mod.encodeLdpStp(16, 17, addr_reg, ldp_off, .signed_offset, false));
                            copy_off += 16;
                        }
                        if (copy_off + 8 <= type_size) {
                            const ldr_off: u12 = @intCast(@divExact(copy_off, 8));
                            try self.emit(asm_mod.encodeLdrStr(16, src_reg, ldr_off, true));
                            try self.emit(asm_mod.encodeLdrStr(16, addr_reg, ldr_off, false));
                            copy_off += 8;
                        }
                        while (copy_off < type_size) {
                            const ld_size = asm_mod.LdStSize.byte;
                            try self.emit(asm_mod.encodeLdrStrSized(16, src_reg, @intCast(copy_off), ld_size, true));
                            try self.emit(asm_mod.encodeLdrStrSized(16, addr_reg, @intCast(copy_off), ld_size, false));
                            copy_off += 1;
                        }
                        debug.log(.codegen, "      -> copied {d}B struct via load from [x{d}] to [x{d}]", .{ type_size, src_reg, addr_reg });
                    } else if (type_size == 16 and val.op == .static_call) {
                        // After static_call returning struct: first half in x0, second half was saved to x8
                        // BUG-003 fix: The second half was saved to x8 immediately after the call
                        // (because x1 gets clobbered by local_addr before we get here)
                        var actual_addr_reg = addr_reg;
                        if (addr_reg == 0 or addr_reg == 8) {
                            actual_addr_reg = 9;
                            try self.emit(asm_mod.encodeADDImm(9, addr_reg, 0, 0)); // x9 = addr_reg
                        }
                        // x0 = first half, x8 = second half (saved immediately after call)
                        try self.emit(asm_mod.encodeLdpStp(0, 8, actual_addr_reg, 0, .signed_offset, false));
                        debug.log(.codegen, "      -> STP x0, x8, [x{d}] (store call 16B)", .{actual_addr_reg});
                    } else if (type_size == 16 and val.op == .load) {
                        // 16-byte load (struct/slice) - val_reg+1 may have been clobbered by later ops
                        // BUG-067 fix: Reload from source address instead of assuming consecutive regs
                        // The source address is in val.args[0]
                        if (val.args.len > 0) {
                            const src_addr = val.args[0];
                            debug.log(.codegen, "      16B store: src_addr.op={s}, src_addr.id=v{d}", .{ @tagName(src_addr.op), src_addr.id });

                            // CRITICAL: The source address register has likely been clobbered by
                            // intermediate computations. We CANNOT trust getRegForValue here.
                            // For local_addr, regenerate directly from the local offset.
                            const src_scratch: u5 = 16;
                            if (src_addr.op == .local_addr) {
                                // Regenerate local_addr directly - don't trust regalloc
                                const local_idx: usize = @intCast(src_addr.aux_int);
                                if (local_idx < self.func.local_offsets.len) {
                                    const local_offset = self.func.local_offsets[local_idx];
                                    try self.emitAddImm(src_scratch, 31, @intCast(local_offset));
                                    debug.log(.codegen, "      regenerated local_addr {d} (SP+{d}) -> x{d}", .{ local_idx, local_offset, src_scratch });
                                } else {
                                    debug.log(.codegen, "      local_addr {d} out of bounds!", .{local_idx});
                                    try self.emit(asm_mod.encodeMOVZ(src_scratch, 0, 0));
                                }
                            } else {
                                // Other address types - try normal ensureInReg
                                try self.ensureInReg(src_addr, src_scratch);
                            }

                            // Use x14/x15 as dedicated scratch for 16B value to avoid clobber issues
                            const scratch1: u5 = 14;
                            const scratch2: u5 = 15;
                            try self.emit(asm_mod.encodeLdpStp(scratch1, scratch2, src_scratch, 0, .signed_offset, true));
                            try self.emit(asm_mod.encodeLdpStp(scratch1, scratch2, addr_reg, 0, .signed_offset, false));
                            debug.log(.codegen, "      -> LDP x{d}, x{d}, [x{d}] then STP to [x{d}] (store 16B reload)", .{ scratch1, scratch2, src_scratch, addr_reg });
                        } else {
                            // Fallback: try old behavior
                            const val_reg = self.getRegForValue(val) orelse blk: {
                                const temp_reg: u5 = 16;
                                try self.ensureInReg(val, temp_reg);
                                break :blk temp_reg;
                            };
                            try self.emit(asm_mod.encodeLdpStp(val_reg, val_reg + 1, addr_reg, 0, .signed_offset, false));
                            debug.log(.codegen, "      -> STP x{d}, x{d}, [x{d}] (store 16B from load)", .{ val_reg, val_reg + 1, addr_reg });
                        }
                    } else if (type_size == 16 and val.op == .string_make) {
                        // After expand_calls: string_make holds (ptr, len) from decomposed call
                        // val.args[0] = select_n idx=0 (ptr), val.args[1] = select_n idx=1 (len)
                        //
                        // CRITICAL: The select_n values copy from x8/x9 to their assigned registers,
                        // but those registers may have been reused by later instructions (e.g., local_addr).
                        // Instead of trusting getRegForValue() which returns the regalloc assignment
                        // (which may be stale), we use x8/x9 directly - these are the preserved call
                        // result registers that won't be clobbered.
                        //
                        // This is a workaround until we have proper ABI-aware register allocation
                        // where regalloc knows to keep call results alive until they're consumed.

                        // Save addr_reg to scratch register FIRST
                        const saved_addr_reg: u5 = 15; // Use x15 as safe scratch for address
                        try self.emit(asm_mod.encodeADDImm(saved_addr_reg, addr_reg, 0, 0));

                        // Use x8/x9 directly (saved call results) instead of select_n registers
                        // x8 = ptr (idx=0), x9 = len (idx=1)
                        try self.emit(asm_mod.encodeLdpStp(8, 9, saved_addr_reg, 0, .signed_offset, false));
                        debug.log(.codegen, "      -> STP x8, x9, [x{d}] (store string_make 16B via saved regs)", .{saved_addr_reg});
                    } else if (type_size == 16 and val.op == .const_string) {
                        // Store const_string: ptr is in val_reg, len needs to be loaded as immediate
                        const val_reg = self.getRegForValue(val) orelse blk: {
                            const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                            try self.ensureInReg(val, temp_reg);
                            break :blk temp_reg;
                        };

                        // Get string length from string_literals
                        const string_index: usize = @intCast(val.aux_int);
                        const str_len: i64 = if (string_index < self.func.string_literals.len)
                            @intCast(self.func.string_literals[string_index].len)
                        else
                            0;

                        // Store ptr (in val_reg) to addr
                        try self.emit(asm_mod.encodeLdrStr(val_reg, addr_reg, 0, false));

                        // Load len into a temp register and store to addr+8
                        const temp_len_reg: u5 = 16; // Use x16 as temp
                        if (str_len >= 0 and str_len <= 65535) {
                            try self.emit(asm_mod.encodeMOVZ(temp_len_reg, @intCast(str_len), 0));
                        } else {
                            try self.emit(asm_mod.encodeMOVZ(temp_len_reg, @intCast(str_len & 0xFFFF), 0));
                            if (str_len > 0xFFFF) {
                                try self.emit(asm_mod.encodeMOVK(temp_len_reg, @intCast((str_len >> 16) & 0xFFFF), 1));
                            }
                        }
                        // Store len to addr+8 (offset=1 for 64-bit scaled)
                        try self.emit(asm_mod.encodeLdrStr(temp_len_reg, addr_reg, 1, false));
                        debug.log(.codegen, "      -> STR x{d}, [x{d}]; MOV x16, #{d}; STR x16, [x{d}, #8] (store const_string 16B)", .{
                            val_reg, addr_reg, str_len, addr_reg
                        });
                    } else {
                        const val_reg = self.getRegForValue(val) orelse blk: {
                            const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                            try self.ensureInReg(val, temp_reg);
                            break :blk temp_reg;
                        };

                        const st_size = asm_mod.LdStSize.fromBytes(@intCast(type_size));
                        const type_name = TypeRegistry.basicTypeName(val.type_idx);

                        try self.emit(asm_mod.encodeLdrStrSized(val_reg, addr_reg, 0, st_size, false));
                        debug.log(.codegen, "      -> STR{s} {s}{d}, [{s}{d}] (store {s}, {d}B)", .{
                            st_size.name(),
                            if (st_size == .dword) "x" else "w",
                            val_reg,
                            "x",
                            addr_reg,
                            type_name,
                            type_size,
                        });
                    }
                }
            },

            // === Conditional Select ===
            // cond_select(cond, then_val, else_val) -> if cond != 0 then then_val else else_val
            // ARM64: CMP cond, #0; CSEL dest, then, else, NE
            .cond_select => {
                if (value.args.len >= 3) {
                    const cond = value.args[0];
                    const then_val = value.args[1];
                    const else_val = value.args[2];

                    const cond_reg = self.getRegForValue(cond) orelse blk: {
                        try self.ensureInReg(cond, 0);
                        break :blk @as(u5, 0);
                    };
                    const then_reg = self.getRegForValue(then_val) orelse blk: {
                        try self.ensureInReg(then_val, 1);
                        break :blk @as(u5, 1);
                    };
                    const else_reg = self.getRegForValue(else_val) orelse blk: {
                        try self.ensureInReg(else_val, 2);
                        break :blk @as(u5, 2);
                    };
                    const dest_reg = self.getDestRegForValue(value);

                    // CMP cond, #0 (test if condition is non-zero)
                    try self.emit(asm_mod.encodeCMPImm(cond_reg, 0));
                    // CSEL dest, then, else, NE (select then if cond != 0)
                    try self.emit(asm_mod.encodeCSEL(dest_reg, then_reg, else_reg, .ne));

                    debug.log(.codegen, "      -> CMP x{d}, #0; CSEL x{d}, x{d}, x{d}, NE", .{ cond_reg, dest_reg, then_reg, else_reg });
                }
            },

            // === Bulk Memory Copy (OpMove) ===
            // Go reference: OpMove for non-SSA aggregates (>32 bytes)
            // args[0] = dest addr, args[1] = src value (select_n), args[2] = mem (optional)
            // aux_int = size in bytes
            .move => {
                if (value.args.len >= 2) {
                    const dest_addr = value.args[0];
                    const src_val = value.args[1]; // select_n from call result

                    const type_size: u32 = @intCast(value.aux_int);

                    // Get destination address register
                    const dest_reg = self.getRegForValue(dest_addr) orelse blk: {
                        const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                        try self.ensureInReg(dest_addr, temp_reg);
                        break :blk temp_reg;
                    };

                    // Resolve source address from hidden_ret_offsets
                    // For >16B returns via hidden pointer, the result is at a frame offset
                    //
                    // CRITICAL: Regalloc may have rewritten the original call value to a
                    // load_reg or copy. We need to trace back through passthrough ops
                    // to find the original static_call/closure_call value.
                    var src_reg: u5 = undefined;
                    debug.log(.codegen, "      move: src_val v{d} op={s}", .{ src_val.id, @tagName(src_val.op) });

                    // Trace back through passthrough ops to find original call
                    var trace_val = src_val;
                    var found_offset: ?u32 = null;
                    var trace_count: u32 = 0;
                    while (trace_count < 10) : (trace_count += 1) {
                        // Try to find in hidden_ret_offsets
                        if (self.hidden_ret_offsets.get(trace_val)) |rel_offset| {
                            found_offset = rel_offset;
                            debug.log(.codegen, "      move: found offset {d} at v{d} ({s})", .{ rel_offset, trace_val.id, @tagName(trace_val.op) });
                            break;
                        }

                        // Trace through passthrough operations
                        if (trace_val.op == .load_reg and trace_val.args.len > 0) {
                            trace_val = trace_val.args[0];
                        } else if (trace_val.op == .copy and trace_val.args.len > 0) {
                            trace_val = trace_val.args[0];
                        } else if (trace_val.op == .select_n and trace_val.args.len > 0) {
                            trace_val = trace_val.args[0];
                        } else {
                            // Can't trace further
                            break;
                        }
                    }

                    if (found_offset) |rel_offset| {
                        // Compute source address from frame offset into x19
                        const frame_offset: u12 = @intCast(self.hidden_ret_frame_offset + rel_offset);
                        try self.emit(asm_mod.encodeADDImm(19, 31, frame_offset, 0)); // x19 = SP + offset
                        src_reg = 19;
                        debug.log(.codegen, "      move: src at frame offset {d} -> x19", .{frame_offset});
                    } else {
                        // Fallback: use src_val's register (shouldn't happen for >16B)
                        debug.log(.codegen, "      move: WARNING no hidden_ret_offset found, using register", .{});
                        src_reg = self.getRegForValue(src_val) orelse blk: {
                            const temp_reg: u5 = 16; // Phase 1: use x16 as scratch
                            try self.ensureInReg(src_val, temp_reg);
                            break :blk temp_reg;
                        };
                    }

                    debug.log(.codegen, "      -> OpMove {d}B: [x{d}] <- [x{d}]", .{ type_size, dest_reg, src_reg });

                    // Copy data in 16-byte chunks using LDP/STP
                    // Use x16, x17 as temp registers (intra-procedure call scratch)
                    var copy_off: u32 = 0;
                    while (copy_off + 16 <= type_size) {
                        const ldp_off: i7 = @intCast(@divExact(@as(i32, @intCast(copy_off)), 8));
                        try self.emit(asm_mod.encodeLdpStp(16, 17, src_reg, ldp_off, .signed_offset, true)); // LDP
                        try self.emit(asm_mod.encodeLdpStp(16, 17, dest_reg, ldp_off, .signed_offset, false)); // STP
                        copy_off += 16;
                    }
                    // Handle remaining 8 bytes
                    if (copy_off + 8 <= type_size) {
                        const ldr_off: u12 = @intCast(@divExact(copy_off, 8));
                        try self.emit(asm_mod.encodeLdrStr(16, src_reg, ldr_off, true)); // LDR
                        try self.emit(asm_mod.encodeLdrStr(16, dest_reg, ldr_off, false)); // STR
                        copy_off += 8;
                    }
                    // Handle remaining bytes one at a time
                    while (copy_off < type_size) {
                        const ld_size = asm_mod.LdStSize.byte;
                        try self.emit(asm_mod.encodeLdrStrSized(16, src_reg, @intCast(copy_off), ld_size, true));
                        try self.emit(asm_mod.encodeLdrStrSized(16, dest_reg, @intCast(copy_off), ld_size, false));
                        copy_off += 1;
                    }

                    debug.log(.codegen, "      -> copied {d}B via OpMove", .{type_size});
                }
            },

            else => {
                // Unhandled op - skip
            },
        }
    }

    /// Allocate a register for a new value
    // Phase 1: Removed allocateReg() - codegen must only use regalloc assignments

    /// Ensure value is in specified register, regenerating if needed
    fn ensureInReg(self: *ARM64CodeGen, value: *const Value, dest: u5) !void {
        if (self.getRegForValue(value)) |src_reg| {
            if (src_reg != dest) try self.emit(asm_mod.encodeADDImm(dest, src_reg, 0, 0));
            return;
        }
        // Regenerate value
        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(dest, value.aux_int),
            .const_bool => try self.emitLoadImmediate(dest, if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emitLoadImmediate(dest, 0),
            .load => {
                if (value.args.len > 0) {
                    const addr_val = value.args[0];
                    const addr_reg = self.getRegForValue(addr_val) orelse blk: {
                        try self.ensureInReg(addr_val, 16);
                        break :blk @as(u5, 16);
                    };
                    const type_size = self.getTypeSize(value.type_idx);
                    if (type_size == 1) try self.emit(asm_mod.encodeLDRB(dest, addr_reg, 0))
                    else if (type_size == 2) try self.emit(asm_mod.encodeLDRH(dest, addr_reg, 0))
                    else if (type_size == 4) try self.emit(asm_mod.encodeLdrStrSized(dest, addr_reg, 0, .word, true))
                    else try self.emit(asm_mod.encodeLdrStr(dest, addr_reg, 0, true));
                } else try self.emit(asm_mod.encodeMOVZ(dest, 0, 0));
            },
            .const_string, .const_ptr => {
                const string_index: usize = @intCast(value.aux_int);
                const str_data = if (string_index < self.func.string_literals.len) self.func.string_literals[string_index] else "";
                const adrp_offset = self.offset();
                try self.emit(asm_mod.encodeADRP(dest, 0));
                const add_offset = self.offset();
                try self.emit(asm_mod.encodeADDImm(dest, dest, 0, 0));
                try self.string_refs.append(self.allocator, .{
                    .adrp_offset = adrp_offset, .add_offset = add_offset,
                    .string_data = try self.allocator.dupe(u8, str_data),
                });
            },
            .local_addr => {
                const local_idx: usize = @intCast(value.aux_int);
                if (local_idx < self.func.local_offsets.len) {
                    const base_offset: u32 = @intCast(self.func.local_offsets[local_idx]);
                    try self.emitAddImm(dest, 31, @intCast(base_offset + self.call_stack_adjustment));
                } else try self.emit(asm_mod.encodeMOVZ(dest, 0, 0));
            },
            .sp => {
                // Get native stack pointer into dest register (for Wasm memory ops)
                try self.emit(asm_mod.encodeADDImm(dest, 31, 0, 0));
            },
            else => try self.emit(asm_mod.encodeMOVZ(dest, 0, 0)),
        }
    }

    fn getRegForValue(self: *ARM64CodeGen, value: *const Value) ?u5 {
        _ = self;
        // sp values always use x16 to avoid clobbering argument registers
        if (value.op == .sp) return 16;
        return if (value.regOrNull()) |reg| @intCast(reg) else null;
    }

    fn getDestRegForValue(self: *ARM64CodeGen, value: *const Value) u5 {
        _ = self;
        if (value.regOrNull()) |reg| return @intCast(reg);
        std.debug.panic("codegen: v{d} ({s}) has no register assigned", .{ value.id, @tagName(value.op) });
    }

    /// Known variadic libc functions on ARM64 macOS (args after fixed count go on stack)
    fn getVariadicFixedArgCount(func_name: []const u8) ?usize {
        if (std.mem.eql(u8, func_name, "open") or std.mem.eql(u8, func_name, "fcntl") or
            std.mem.eql(u8, func_name, "ioctl")) return 2;
        if (std.mem.eql(u8, func_name, "openat")) return 3;
        return null;
    }

    /// Setup call arguments with parallel copy (uses x16 as scratch for cycles)
    fn setupCallArgsWithVariadic(self: *ARM64CodeGen, args: []*Value, variadic_fixed_args: ?usize) !u12 {
        if (args.len == 0) return 0;

        const fixed_arg_limit: usize = if (variadic_fixed_args) |fixed| fixed else args.len;
        const max_reg_args = @min(@min(args.len, 8), fixed_arg_limit);
        const variadic_stack_args: usize = if (variadic_fixed_args != null and args.len > fixed_arg_limit) args.len - fixed_arg_limit else 0;
        const overflow_stack_args: usize = if (fixed_arg_limit > 8 and args.len > 8) @min(fixed_arg_limit, args.len) - 8 else 0;
        const num_stack_args = variadic_stack_args + overflow_stack_args;
        const stack_size: u12 = @intCast(((num_stack_args * 8 + 15) / 16) * 16);

        if (num_stack_args > 0) {
            try self.emit(asm_mod.encodeSUBImm(31, 31, stack_size, 0));
            self.call_stack_adjustment = stack_size;
            var stack_slot: usize = 0;

            // Store variadic args
            if (variadic_fixed_args != null and args.len > fixed_arg_limit) {
                for (args[fixed_arg_limit..]) |arg| {
                    const scaled_offset: u12 = @intCast(stack_slot);
                    if (self.getRegForValue(arg)) |src_reg| {
                        try self.emit(asm_mod.encodeLdrStr(src_reg, 31, scaled_offset, false));
                    } else {
                        try self.regenerateValue(16, arg);
                        try self.emit(asm_mod.encodeLdrStr(16, 31, scaled_offset, false));
                    }
                    stack_slot += 1;
                }
            }
            // Store overflow args
            if (overflow_stack_args > 0) {
                for (args[8..@min(fixed_arg_limit, args.len)]) |arg| {
                    const scaled_offset: u12 = @intCast(stack_slot);
                    if (self.getRegForValue(arg)) |src_reg| {
                        try self.emit(asm_mod.encodeLdrStr(src_reg, 31, scaled_offset, false));
                    } else {
                        try self.regenerateValue(16, arg);
                        try self.emit(asm_mod.encodeLdrStr(16, 31, scaled_offset, false));
                    }
                    stack_slot += 1;
                }
            }
        }

        const Move = struct { src: ?u5, dest: u5, value: *Value, done: bool };
        var moves: [8]Move = undefined;
        var num_moves: usize = 0;
        for (args[0..max_reg_args], 0..) |arg, i| {
            const dest: u5 = @intCast(i);
            const src = self.getRegForValue(arg);
            moves[num_moves] = .{ .src = src, .dest = dest, .value = arg, .done = (src != null and src.? == dest) };
            num_moves += 1;
        }

        // Process non-conflicting moves first
        var progress = true;
        while (progress) {
            progress = false;
            for (0..num_moves) |mi| {
                if (moves[mi].done) continue;
                var would_clobber = false;
                for (0..num_moves) |oi| {
                    if (moves[oi].done) continue;
                    if (moves[oi].src) |other_src| {
                        if (other_src == moves[mi].dest and oi != mi) { would_clobber = true; break; }
                    }
                }

                if (!would_clobber) {
                    if (moves[mi].src) |src| try self.emit(asm_mod.encodeADDImm(moves[mi].dest, src, 0, 0))
                    else try self.regenerateValue(moves[mi].dest, moves[mi].value);
                    moves[mi].done = true;
                    progress = true;
                }
            }
        }

        // Handle cycles using x16 as temp
        for (0..num_moves) |mi| {
            if (moves[mi].done) continue;
            if (moves[mi].src) |start_src| {
                try self.emit(asm_mod.encodeADDImm(16, start_src, 0, 0)); // Save to x16
                var current_dest = start_src;
                var cycle_len: usize = 0;
                var cycle_order: [8]usize = undefined;
                while (cycle_len < 8) {
                    var found = false;
                    for (0..num_moves) |oi| {
                        if (moves[oi].done) continue;
                        if (moves[oi].dest == current_dest) {
                            cycle_order[cycle_len] = oi;
                            cycle_len += 1;
                            if (moves[oi].src) |next_dest| {
                                if (next_dest == start_src) { found = true; break; }
                                current_dest = next_dest;
                                found = true;
                            }
                            break;
                        }
                    }
                    if (!found or current_dest == start_src) break;
                }
                for (0..cycle_len) |ci| {
                    const move_idx = cycle_order[ci];
                    if (moves[move_idx].src) |src| {
                        try self.emit(asm_mod.encodeADDImm(moves[move_idx].dest, if (src == start_src) 16 else src, 0, 0));
                    } else try self.regenerateValue(moves[move_idx].dest, moves[move_idx].value);
                    moves[move_idx].done = true;
                }
                try self.emit(asm_mod.encodeADDImm(moves[mi].dest, 16, 0, 0));
                moves[mi].done = true;
            } else {
                try self.regenerateValue(moves[mi].dest, moves[mi].value);
                moves[mi].done = true;
            }
        }
        return stack_size;
    }

    fn setupCallArgs(self: *ARM64CodeGen, args: []*Value) !u12 {
        return self.setupCallArgsWithVariadic(args, null);
    }

    fn regenerateValue(self: *ARM64CodeGen, dest: u5, value: *Value) !void {
        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(dest, value.aux_int),
            .const_bool => try self.emitLoadImmediate(dest, if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emitLoadImmediate(dest, 0),
            .local_addr => {
                const local_idx: usize = @intCast(value.aux_int);
                if (local_idx < self.func.local_offsets.len) {
                    const base_off: u32 = @intCast(self.func.local_offsets[local_idx]);
                    try self.emitAddImm(dest, 31, @intCast(base_off + self.call_stack_adjustment));
                } else {
                    try self.emit(asm_mod.encodeADDImm(dest, 31, @intCast(self.call_stack_adjustment), 0));
                }
            },
            .global_addr => {
                const name = switch (value.aux) {
                    .string => |s| s,
                    else => { try self.emit(asm_mod.encodeMOVZ(dest, 0, 0)); return; },
                };
                const adrp_offset = self.offset();
                try self.emit(asm_mod.encodeADRP(dest, 0));
                const add_offset = self.offset();
                try self.emit(asm_mod.encodeADDImm(dest, dest, 0, 0));
                try self.func_refs.append(self.allocator, .{
                    .adrp_offset = adrp_offset, .add_offset = add_offset,
                    .func_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{name}),
                });
            },
            else => try self.emit(asm_mod.encodeMOVZ(dest, 0, 0)),
        }
    }

    fn moveToX0(self: *ARM64CodeGen, value: *const Value) !void {
        const type_size = self.getTypeSize(value.type_idx);
        if (type_size == 16) {
            if (value.op == .load and value.args.len > 0) {
                const src_addr = value.args[0];
                var src_addr_reg: u5 = undefined;
                if (src_addr.op == .local_addr) {
                    const local_idx: usize = @intCast(src_addr.aux_int);
                    if (local_idx < self.func.local_offsets.len) {
                        try self.emitAddImm(16, 31, @intCast(self.func.local_offsets[local_idx]));
                        src_addr_reg = 16;
                    } else {
                        try self.emit(asm_mod.encodeMOVZ(0, 0, 0));
                        try self.emit(asm_mod.encodeMOVZ(1, 0, 0));
                        return;
                    }
                } else {
                    src_addr_reg = self.getRegForValue(src_addr) orelse blk: {
                        try self.ensureInReg(src_addr, 16);
                        break :blk @as(u5, 16);
                    };
                }
                try self.emit(asm_mod.encodeLdpStp(0, 1, src_addr_reg, 0, .signed_offset, true));
                return;
            }
            if (self.getRegForValue(value)) |src_reg| {
                if (src_reg != 0) try self.emit(asm_mod.encodeADDImm(0, src_reg, 0, 0));
                if (src_reg + 1 != 1) try self.emit(asm_mod.encodeADDImm(1, src_reg + 1, 0, 0));
                return;
            }
        }
        try self.moveToReg(0, value);
    }

    fn moveToReg(self: *ARM64CodeGen, dest: u5, value: *const Value) !void {
        if (self.getRegForValue(value)) |src_reg| {
            if (src_reg != dest) try self.emit(asm_mod.encodeADDImm(dest, src_reg, 0, 0));
            return;
        }
        switch (value.op) {
            .const_int, .const_64 => try self.emitLoadImmediate(dest, value.aux_int),
            .const_bool => try self.emitLoadImmediate(dest, if (value.aux_int != 0) 1 else 0),
            .const_nil => try self.emitLoadImmediate(dest, 0),
            .phi => if (value.args.len > 0) try self.moveToReg(dest, value.args[0]) else try self.emit(asm_mod.encodeMOVZ(dest, 0, 0)),
            else => try self.emit(asm_mod.encodeMOVZ(dest, 0, 0)),
        }
    }

    /// Emit binary operation with simple scratch register allocation
    fn emitBinaryReg(self: *ARM64CodeGen, value: *const Value, encode: *const fn (u5, u5, u5) u32) !void {
        const args = value.args;
        if (args.len < 2) return;
        const op1_reg = self.getRegForValue(args[0]) orelse blk: {
            try self.ensureInReg(args[0], 0);
            break :blk @as(u5, 0);
        };
        const op2_reg = self.getRegForValue(args[1]) orelse blk: {
            try self.ensureInReg(args[1], 1);
            break :blk @as(u5, 1);
        };
        try self.emit(encode(self.getDestRegForValue(value), op1_reg, op2_reg));
    }

    /// Emit binary operation with BUG-071 fix (avoid clobbering during regeneration)
    fn emitBinaryRegBug071(self: *ARM64CodeGen, value: *const Value, encode: *const fn (u5, u5, u5) u32) !void {
        const args = value.args;
        if (args.len < 2) return;
        const op2_assigned = self.getRegForValue(args[1]);
        const op1_reg = self.getRegForValue(args[0]) orelse blk: {
            const scratch: u5 = if (op2_assigned == 0) 16 else 0;
            try self.ensureInReg(args[0], scratch);
            break :blk scratch;
        };
        const op2_reg = self.getRegForValue(args[1]) orelse blk: {
            const scratch: u5 = if (op1_reg == 1) 16 else 1;
            try self.ensureInReg(args[1], scratch);
            break :blk scratch;
        };
        try self.emit(encode(self.getDestRegForValue(value), op1_reg, op2_reg));
    }

    /// Emit code to load an immediate value into a register
    fn emitLoadImmediate(self: *ARM64CodeGen, reg: u5, value: i64) !void {
        const uvalue: u64 = @bitCast(value);

        // Check if it fits in 16 bits (most common case)
        if (uvalue <= 0xFFFF) {
            try self.emit(asm_mod.encodeMOVZ(reg, @truncate(uvalue), 0));
            return;
        }

        // Check for negative small number
        if (value < 0 and value >= -65536) {
            const notval: u16 = @truncate(~uvalue);
            try self.emit(asm_mod.encodeMOVN(reg, notval, 0));
            return;
        }

        // Need multiple instructions for larger values
        try self.emit(asm_mod.encodeMOVZ(reg, @truncate(uvalue), 0));

        if ((uvalue >> 16) & 0xFFFF != 0) {
            try self.emit(asm_mod.encodeMOVK(reg, @truncate(uvalue >> 16), 1));
        }
        if ((uvalue >> 32) & 0xFFFF != 0) {
            try self.emit(asm_mod.encodeMOVK(reg, @truncate(uvalue >> 32), 2));
        }
        if ((uvalue >> 48) & 0xFFFF != 0) {
            try self.emit(asm_mod.encodeMOVK(reg, @truncate(uvalue >> 48), 3));
        }
    }

    pub fn finalize(self: *ARM64CodeGen) ![]u8 {
        var writer = macho.MachOWriter.init(self.allocator);
        defer writer.deinit();

        try writer.addCode(self.code.items);
        for (self.symbols.items) |sym| try writer.addSymbol(sym.name, sym.value, sym.section, sym.external);
        for (self.relocations.items) |reloc| try writer.addRelocation(reloc.offset, reloc.target);
        for (self.globals) |global| try writer.addGlobalVariable(global.name, global.size);

        for (self.string_refs.items) |str_ref| {
            const sym_name = try writer.addStringLiteral(str_ref.string_data);
            try writer.addDataRelocation(str_ref.adrp_offset, sym_name, macho.ARM64_RELOC_PAGE21);
            try writer.addDataRelocation(str_ref.add_offset, sym_name, macho.ARM64_RELOC_PAGEOFF12);
        }
        for (self.func_refs.items) |func_ref| {
            try writer.addDataRelocation(func_ref.adrp_offset, func_ref.func_name, macho.ARM64_RELOC_PAGE21);
            try writer.addDataRelocation(func_ref.add_offset, func_ref.func_name, macho.ARM64_RELOC_PAGEOFF12);
        }

        if (self.line_entries.items.len > 0 and self.debug_source_text.len > 0) {
            writer.setDebugInfo(self.debug_source_file, self.debug_source_text);
            for (self.line_entries.items) |entry| {
                try writer.addLineEntries(&[_]macho.LineEntry{.{ .code_offset = entry.code_offset, .source_offset = entry.source_offset }});
            }
            try writer.generateDebugSections();
        }

        var output = std.ArrayListUnmanaged(u8){};
        try writer.write(output.writer(self.allocator));
        return try output.toOwnedSlice(self.allocator);
    }
};

// =========================================
// Tests
// =========================================

test "ARM64CodeGen generates function prologue" {
    const allocator = std.testing.allocator;

    var f = Func.init(allocator, "test_arm64");
    defer f.deinit();

    _ = try f.newBlock(.ret);

    var codegen = ARM64CodeGen.init(allocator);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    try codegen.generate(&f, output.writer(allocator));

    // Should contain function label and prologue
    try std.testing.expect(std.mem.indexOf(u8, output.items, "_test_arm64") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "stp") != null);
}
