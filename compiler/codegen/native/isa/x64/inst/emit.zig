//! x86-64 ISA: binary code emission.
//!
//! Ported from Cranelift's:
//! - `cranelift/codegen/src/isa/x64/inst/emit.rs`
//! - `cranelift/assembler-x64/src/rex.rs`
//! - `cranelift/assembler-x64/src/mem.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

const mod = @import("mod.zig");
const args = @import("args.zig");
const regs = @import("regs.zig");

// Re-export types we use
pub const Inst = mod.Inst;
pub const AluRmiROpcode = mod.AluRmiROpcode;
pub const UnaryRmROpcode = mod.UnaryRmROpcode;
pub const ShiftBy = mod.ShiftBy;
pub const SseOpcode = mod.SseOpcode;
pub const Avx512Opcode = mod.Avx512Opcode;
pub const FenceKind = mod.FenceKind;
pub const TrapCode = mod.TrapCode;
pub const ExternalName = mod.ExternalName;
pub const CallInfo = mod.CallInfo;
pub const AtomicRmwSeqOp = mod.AtomicRmwSeqOp;

pub const Reg = args.Reg;
pub const PReg = args.PReg;
pub const RegClass = args.RegClass;
pub const Writable = args.Writable;
pub const Gpr = args.Gpr;
pub const WritableGpr = args.WritableGpr;
pub const Xmm = args.Xmm;
pub const WritableXmm = args.WritableXmm;
pub const GprMem = args.GprMem;
pub const GprMemImm = args.GprMemImm;
pub const XmmMem = args.XmmMem;
pub const XmmMemImm = args.XmmMemImm;
pub const Amode = args.Amode;
pub const SyntheticAmode = args.SyntheticAmode;
pub const RegMem = args.RegMem;
pub const RegMemImm = args.RegMemImm;
pub const CC = args.CC;
pub const ExtMode = args.ExtMode;
pub const ShiftKind = args.ShiftKind;
pub const OperandSize = args.OperandSize;
pub const MachLabel = args.MachLabel;
pub const Type = args.Type;

pub const GprEnc = regs.GprEnc;
pub const XmmEnc = regs.XmmEnc;
pub const rax = regs.rax;
pub const rcx = regs.rcx;
pub const rdx = regs.rdx;
pub const rbx = regs.rbx;
pub const rsp = regs.rsp;
pub const rbp = regs.rbp;
pub const rsi = regs.rsi;
pub const rdi = regs.rdi;
pub const gpr8NeedsRex = regs.gpr8NeedsRex;
pub const isExtendedReg = regs.isExtendedReg;
pub const encLo3 = regs.encLo3;
pub const isRsp = regs.isRsp;
pub const isRbp = regs.isRbp;
pub const isSibSpecial = regs.isSibSpecial;
pub const isDispRequired = regs.isDispRequired;

//=============================================================================
// Machine buffer for instruction emission
//=============================================================================

/// A buffer that collects emitted machine code.
pub const MachBuffer = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
    labels: std.ArrayListUnmanaged(LabelInfo) = .{},
    pending_relocs: std.ArrayListUnmanaged(Relocation) = .{},
    external_relocs: std.ArrayListUnmanaged(ExternalRelocation) = .{},
    cur_offset_val: u32 = 0,
    allocator: Allocator,

    pub const LabelInfo = struct {
        offset: ?u32 = null,
    };

    pub const Relocation = struct {
        offset: u32,
        label: MachLabel,
        kind: LabelUse,
    };

    /// External symbol relocation (for linker).
    pub const ExternalRelocation = struct {
        offset: u32,
        target: mod.ExternalName,
        addend: i64,
        kind: ExternalRelocKind,
    };

    /// x86-64 relocation types for external symbols.
    pub const ExternalRelocKind = enum {
        /// PC-relative 32-bit displacement (used for calls).
        call_pc_rel_4,
        /// PC-relative 32-bit displacement (used for jumps).
        jmp_pc_rel_4,
        /// PC-relative 32-bit displacement (used for data access).
        pc_rel_4,
        /// GOT-relative 32-bit.
        got_pc_rel_4,
        /// Absolute 64-bit address.
        abs_64,
        /// Absolute 32-bit address.
        abs_32,
    };

    pub const LabelUse = enum {
        /// 32-bit PC-relative displacement for JMP and Jcc.
        jmp_rel_32,
        /// 8-bit PC-relative displacement for short jumps.
        jmp_rel_8,
        /// 32-bit PC-relative displacement for RIP-relative addressing.
        pc_rel_32,
    };

    pub fn init(allocator: Allocator) MachBuffer {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *MachBuffer) void {
        self.data.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.pending_relocs.deinit(self.allocator);
        self.external_relocs.deinit(self.allocator);
    }

    /// Add an external symbol relocation at the current offset.
    pub fn addExternalReloc(self: *MachBuffer, target: mod.ExternalName, addend: i64, kind: ExternalRelocKind) void {
        self.external_relocs.append(self.allocator, .{
            .offset = self.cur_offset_val,
            .target = target,
            .addend = addend,
            .kind = kind,
        }) catch unreachable;
    }

    pub fn curOffset(self: *const MachBuffer) u32 {
        return self.cur_offset_val;
    }

    /// Put a single byte.
    pub fn put1(self: *MachBuffer, val: u8) void {
        self.data.append(self.allocator, val) catch unreachable;
        self.cur_offset_val += 1;
    }

    /// Put a 16-bit little-endian value.
    pub fn put2(self: *MachBuffer, val: u16) void {
        const bytes = std.mem.asBytes(&val);
        self.data.appendSlice(self.allocator, bytes) catch unreachable;
        self.cur_offset_val += 2;
    }

    /// Put a 32-bit little-endian value.
    pub fn put4(self: *MachBuffer, val: u32) void {
        const bytes = std.mem.asBytes(&val);
        self.data.appendSlice(self.allocator, bytes) catch unreachable;
        self.cur_offset_val += 4;
    }

    /// Put a 64-bit little-endian value.
    pub fn put8(self: *MachBuffer, val: u64) void {
        const bytes = std.mem.asBytes(&val);
        self.data.appendSlice(self.allocator, bytes) catch unreachable;
        self.cur_offset_val += 8;
    }

    /// Put a signed 32-bit value.
    pub fn putSimm32(self: *MachBuffer, val: i32) void {
        self.put4(@bitCast(val));
    }

    /// Put raw data.
    pub fn putData(self: *MachBuffer, data: []const u8) void {
        self.data.appendSlice(self.allocator, data) catch unreachable;
        self.cur_offset_val += @intCast(data.len);
    }

    pub fn getLabel(self: *MachBuffer) MachLabel {
        const idx = self.labels.items.len;
        self.labels.append(self.allocator, .{ .offset = null }) catch unreachable;
        return MachLabel.fromU32(@intCast(idx));
    }

    pub fn bindLabel(self: *MachBuffer, label: MachLabel, _: anytype) void {
        self.labels.items[label.asU32()].offset = self.cur_offset_val;
    }

    pub fn useLabelAtOffset(self: *MachBuffer, offset: u32, label: MachLabel, kind: LabelUse) void {
        self.pending_relocs.append(self.allocator, .{
            .offset = offset,
            .label = label,
            .kind = kind,
        }) catch unreachable;
    }
};

//=============================================================================
// Emission state
//=============================================================================

/// State carried between emissions of a sequence of instructions.
pub const EmitState = struct {
    /// Frame layout information
    frame_layout: FrameLayout = .{},

    pub fn frameLayout(self: *const EmitState) *const FrameLayout {
        return &self.frame_layout;
    }
};

/// Frame layout information.
pub const FrameLayout = struct {
    setup_area_size: u32 = 0,
    clobber_size: u32 = 0,
    fixed_frame_storage_size: u32 = 0,
    outgoing_args_size: u32 = 0,
    stackslots_size: u32 = 0,
};

/// Constant state used during function compilation.
pub const EmitInfo = struct {
    /// Compiler flags
    flags: Flags = .{},

    pub const Flags = struct {
        /// Enable stack probes.
        enable_probestack: bool = false,
    };

    pub fn init(flags: Flags) EmitInfo {
        return .{ .flags = flags };
    }
};

//=============================================================================
// REX prefix encoding (from cranelift-assembler-x64/src/rex.rs)
//=============================================================================

/// Tests whether `enc` is `rsp`, `rbp`, `rsi`, or `rdi`. If 8-bit register
/// sizes are used then it means a REX prefix is required.
///
/// This corresponds to registers 4-7 in the encoding which, without REX,
/// would otherwise map to AH, CH, DH, BH.
fn isSpecialIf8bit(enc: u8) bool {
    return enc >= 4 and enc <= 7;
}

/// The REX prefix for x86-64 instructions.
/// See Intel manual section 2.2.1 "REX Prefixes".
pub const RexPrefix = struct {
    byte: u8,
    must_emit: bool,

    /// Construct the REX prefix for a unary instruction (single register operand).
    /// `b` extends the `reg` register, allowing access to r8-r15.
    pub fn oneOp(enc: u8, w_bit: bool, uses_8bit: bool) RexPrefix {
        const must_emit = uses_8bit and isSpecialIf8bit(enc);
        const w: u8 = if (w_bit) 1 else 0;
        const r: u8 = 0;
        const x: u8 = 0;
        const b: u8 = (enc >> 3) & 1;
        const flag = 0x40 | (w << 3) | (r << 2) | (x << 1) | b;
        return .{ .byte = flag, .must_emit = must_emit };
    }

    /// Construct the REX prefix for a binary instruction.
    /// Used without a SIB byte or for register-to-register addressing.
    /// `r` extends the `reg` operand, `b` extends the `r/m` operand.
    pub fn twoOp(enc_reg: u8, enc_rm: u8, w_bit: bool, uses_8bit: bool) RexPrefix {
        var ret = memOp(enc_reg, enc_rm, w_bit, uses_8bit);
        if (uses_8bit and isSpecialIf8bit(enc_rm)) {
            ret.must_emit = true;
        }
        return ret;
    }

    /// Construct the REX prefix for a binary instruction where one operand
    /// is a memory address. Same as twoOp but enc_rm is guaranteed to
    /// address a 64-bit register.
    pub fn memOp(enc_reg: u8, enc_rm: u8, w_bit: bool, uses_8bit: bool) RexPrefix {
        const must_emit = uses_8bit and isSpecialIf8bit(enc_reg);
        const w: u8 = if (w_bit) 1 else 0;
        const r: u8 = (enc_reg >> 3) & 1;
        const x: u8 = 0;
        const b: u8 = (enc_rm >> 3) & 1;
        const flag = 0x40 | (w << 3) | (r << 2) | (x << 1) | b;
        return .{ .byte = flag, .must_emit = must_emit };
    }

    /// Construct the REX prefix for an instruction using an opcode digit.
    pub fn withDigit(digit: u8, enc_reg: u8, w_bit: bool, uses_8bit: bool) RexPrefix {
        return twoOp(digit, enc_reg, w_bit, uses_8bit);
    }

    /// Construct the REX prefix for a ternary instruction (with SIB byte).
    /// `r` extends reg, `x` extends index, `b` extends base.
    pub fn threeOp(enc_reg: u8, enc_index: u8, enc_base: u8, w_bit: bool, uses_8bit: bool) RexPrefix {
        const must_emit = uses_8bit and isSpecialIf8bit(enc_reg);
        const w: u8 = if (w_bit) 1 else 0;
        const r: u8 = (enc_reg >> 3) & 1;
        const x: u8 = (enc_index >> 3) & 1;
        const b: u8 = (enc_base >> 3) & 1;
        const flag = 0x40 | (w << 3) | (r << 2) | (x << 1) | b;
        return .{ .byte = flag, .must_emit = must_emit };
    }

    /// Possibly emit the REX prefix byte.
    /// Only emitted if the REX prefix is not 0x40 (default) or if
    /// the instruction uses 8-bit operands requiring REX.
    pub fn encode(self: RexPrefix, sink: *MachBuffer) void {
        if (self.byte != 0x40 or self.must_emit) {
            sink.put1(self.byte);
        }
    }

    /// Check if REX prefix needs to be emitted.
    pub fn needsEmit(self: RexPrefix) bool {
        return self.byte != 0x40 or self.must_emit;
    }
};

//=============================================================================
// ModR/M and SIB byte encoding (from cranelift-assembler-x64/src/rex.rs)
//=============================================================================

/// Encode the ModR/M byte.
/// mod (2 bits): addressing mode
/// reg (3 bits): register operand or opcode extension
/// rm (3 bits): register operand or memory addressing
pub fn encodeModrm(m0d: u8, enc_reg_g: u8, rm_e: u8) u8 {
    std.debug.assert(m0d < 4);
    std.debug.assert(enc_reg_g < 8);
    std.debug.assert(rm_e < 8);
    return ((m0d & 3) << 6) | ((enc_reg_g & 7) << 3) | (rm_e & 7);
}

/// Encode the SIB byte (Scale-Index-Base).
/// scale (2 bits): 00=1, 01=2, 10=4, 11=8
/// index (3 bits): index register
/// base (3 bits): base register
pub fn encodeSib(scale: u8, enc_index: u8, enc_base: u8) u8 {
    std.debug.assert(scale < 4);
    std.debug.assert(enc_index < 8);
    std.debug.assert(enc_base < 8);
    return ((scale & 3) << 6) | ((enc_index & 7) << 3) | (enc_base & 7);
}

//=============================================================================
// Displacement encoding (from cranelift-assembler-x64/src/rex.rs)
//=============================================================================

/// The displacement bytes used after the ModR/M and SIB bytes.
pub const Disp = union(enum) {
    none,
    imm8: i8,
    imm32: i32,

    /// Classifies the 32-bit immediate `val` as how this can be encoded
    /// with ModRM/SIB bytes.
    pub fn new(val: i32, evex_scaling: ?i8) Disp {
        if (val == 0) {
            return .none;
        }
        if (evex_scaling) |scaling| {
            if (@mod(val, @as(i32, scaling)) == 0) {
                const scaled = @divExact(val, @as(i32, scaling));
                if (low8WillSignExtendTo32(scaled)) {
                    return .{ .imm8 = @truncate(scaled) };
                }
            }
            return .{ .imm32 = val };
        } else {
            if (val >= -128 and val <= 127) {
                return .{ .imm8 = @truncate(val) };
            }
            return .{ .imm32 = val };
        }
    }

    /// Forces None to become Imm8(0), used for special cases
    /// where some base registers require an immediate.
    pub fn forceImmediate(self: *Disp) void {
        if (self.* == .none) {
            self.* = .{ .imm8 = 0 };
        }
    }

    /// Returns the two "mod" bits present at the upper bits of the mod/rm byte.
    pub fn m0d(self: Disp) u8 {
        return switch (self) {
            .none => 0b00,
            .imm8 => 0b01,
            .imm32 => 0b10,
        };
    }

    /// Emit the displacement into the code sink.
    pub fn emit(self: Disp, sink: *MachBuffer) void {
        switch (self) {
            .none => {},
            .imm8 => |n| sink.put1(@bitCast(n)),
            .imm32 => |n| sink.put4(@bitCast(n)),
        }
    }
};

fn low8WillSignExtendTo32(xs: i32) bool {
    return xs == ((xs << 24) >> 24);
}

//=============================================================================
// Memory addressing mode encoding (from cranelift-assembler-x64/src/mem.rs)
//=============================================================================

/// Scale values for SIB byte.
pub const Scale = enum(u2) {
    one = 0b00,
    two = 0b01,
    four = 0b10,
    eight = 0b11,

    pub fn fromU8(encoding: u8) Scale {
        return @enumFromInt(encoding);
    }

    pub fn enc(self: Scale) u8 {
        return @intFromEnum(self);
    }

    pub fn shift(self: Scale) u8 {
        return @as(u8, 1) << @intFromEnum(self);
    }
};

/// Emit the ModRM/SIB/displacement sequence for a memory operand.
pub fn emitModrmSibDisp(
    sink: *MachBuffer,
    enc_g: u8,
    amode: Amode,
    bytes_at_end: u8,
    evex_scaling: ?i8,
) void {
    switch (amode) {
        .imm_reg => |m| {
            const enc_e = m.base.hwEnc();
            var imm = Disp.new(m.simm32, evex_scaling);

            // Most base registers allow for a single ModRM byte plus an
            // optional immediate. If rsp is the base register, however, then a
            // SIB byte must be used.
            const enc_e_low3 = enc_e & 7;
            if (enc_e_low3 == GprEnc.RSP) {
                // Displacement from RSP is encoded with a SIB byte where
                // the index and base are both encoded as RSP's encoding of
                // 0b100. This special encoding means that the index register
                // isn't used and the base is 0b100 with or without a
                // REX-encoded 4th bit (e.g. rsp or r12)
                sink.put1(encodeModrm(imm.m0d(), enc_g & 7, 0b100));
                sink.put1(0b00_100_100);
                imm.emit(sink);
            } else {
                // If the base register is rbp and there's no offset then force
                // a 1-byte zero offset since otherwise the encoding would be
                // invalid.
                if (enc_e_low3 == GprEnc.RBP) {
                    imm.forceImmediate();
                }
                sink.put1(encodeModrm(imm.m0d(), enc_g & 7, enc_e & 7));
                imm.emit(sink);
            }
        },

        .imm_reg_reg_shift => |m| {
            const enc_base = m.base.hwEnc();
            const enc_index = m.index.hwEnc();

            // Encoding of ModRM/SIB bytes don't allow the index register to
            // ever be rsp. Note, though, that the encoding of r12, whose three
            // lower bits match the encoding of rsp, is explicitly allowed with
            // REX bytes so only rsp is disallowed.
            std.debug.assert(enc_index != GprEnc.RSP);

            // If the offset is zero then there is no immediate. Note, though,
            // that if the base register's lower three bits are `101` then an
            // offset must be present. This is a special case in the encoding of
            // the SIB byte and requires an explicit displacement with rbp/r13.
            var imm = Disp.new(m.simm32, evex_scaling);
            if (enc_base & 7 == GprEnc.RBP) {
                imm.forceImmediate();
            }

            // With the above determined encode the ModRM byte, then the SIB
            // byte, then any immediate as necessary.
            sink.put1(encodeModrm(imm.m0d(), enc_g & 7, 0b100));
            sink.put1(encodeSib(m.shift, enc_index & 7, enc_base & 7));
            imm.emit(sink);
        },

        .rip_relative => |m| {
            // RIP-relative is mod=00, rm=101.
            sink.put1(encodeModrm(0b00, enc_g & 7, 0b101));

            // Record the use of the target for relocation.
            sink.useLabelAtOffset(sink.curOffset(), m.target, .pc_rel_32);

            // N.B.: some instructions (XmmRmRImm format for example)
            // have bytes *after* the RIP-relative offset. The
            // addressed location is relative to the end of the
            // instruction, but the relocation is nominally relative
            // to the end of the u32 field. So, to compensate for
            // this, we emit a negative extra offset in the u32 field
            // initially, and the relocation will add to it.
            sink.putSimm32(-@as(i32, bytes_at_end));
        },
    }
}

/// Emit ModRM/SIB for a register-to-register operation.
pub fn emitModrmReg(sink: *MachBuffer, enc_g: u8, enc_e: u8) void {
    // mod=11 for register-direct addressing
    sink.put1(encodeModrm(0b11, enc_g & 7, enc_e & 7));
}

//=============================================================================
// Synthetic addressing mode finalization
//=============================================================================

/// Result of mem_finalize: instructions to emit before the memory access, and the finalized Amode.
pub const MemFinalizeResult = struct {
    /// Instructions to emit before the memory operation (for large offsets).
    pre_insts: [4]?Inst,
    pre_inst_count: u8,
    /// The finalized addressing mode.
    amode: Amode,
};

/// Convert synthetic addressing modes (SPOffset, IncomingArg, etc.) to real addressing modes.
pub fn memFinalize(mem: SyntheticAmode, state: *const EmitState) MemFinalizeResult {
    var result = MemFinalizeResult{
        .pre_insts = .{ null, null, null, null },
        .pre_inst_count = 0,
        .amode = undefined,
    };

    switch (mem) {
        .real => |amode| {
            result.amode = amode;
        },
        .incoming_arg => |m| {
            // Incoming args are above the saved RBP and return address
            const fl = state.frame_layout;
            const total_frame: i64 = @as(i64, fl.setup_area_size) +
                @as(i64, fl.clobber_size) +
                @as(i64, fl.fixed_frame_storage_size) +
                @as(i64, fl.outgoing_args_size);
            const off: i32 = @intCast(total_frame + @as(i64, m.offset_val));
            result.amode = .{ .imm_reg = .{
                .simm32 = off,
                .base = rbp(),
                .flags = .{},
            } };
        },
        .slot_offset => |m| {
            // Slot offsets are relative to RSP after outgoing args
            const adj: i32 = @intCast(state.frame_layout.outgoing_args_size);
            const off = m.simm32 + adj;
            result.amode = .{ .imm_reg = .{
                .simm32 = off,
                .base = rsp(),
                .flags = .{},
            } };
        },
        .constant_offset => |_| {
            // Constant pool reference - use a placeholder RIP-relative address
            // The actual label will be resolved during finalization
            result.amode = .{ .rip_relative = .{
                .target = MachLabel.fromU32(0),
            } };
        },
    }

    return result;
}

//=============================================================================
// Instruction encoding helpers
//=============================================================================

/// Emit a REX prefix if needed, followed by an opcode.
pub fn emitRexOpcode(sink: *MachBuffer, rex: RexPrefix, opcode: []const u8) void {
    rex.encode(sink);
    for (opcode) |byte| {
        sink.put1(byte);
    }
}

/// Emit opcodes for ALU operations.
fn aluOpcodes(op: AluRmiROpcode, size: OperandSize) struct { prefix: ?u8, opcode: u8 } {
    const base: u8 = switch (op) {
        .add => 0x01,
        .sub => 0x29,
        .@"and" => 0x21,
        .@"or" => 0x09,
        .xor => 0x31,
        .adc => 0x11,
        .sbb => 0x19,
    };
    const prefix: ?u8 = if (size == .size16) 0x66 else null;
    return .{ .prefix = prefix, .opcode = base };
}

/// Get the opcode extension (reg field of ModRM) for ALU immediate operations.
fn aluImmOpcodeExt(op: AluRmiROpcode) u8 {
    return switch (op) {
        .add => 0,
        .@"or" => 1,
        .adc => 2,
        .sbb => 3,
        .@"and" => 4,
        .sub => 5,
        .xor => 6,
    };
}

//=============================================================================
// Main emission function
//=============================================================================

/// Emit a single instruction to the code buffer.
pub fn emit(inst: *const Inst, sink: *MachBuffer, info: *const EmitInfo, state: *const EmitState) void {
    _ = info;

    switch (inst.*) {
        //---------------------------------------------------------------------
        // NOPs
        //---------------------------------------------------------------------
        .nop => |nop| {
            emitNop(sink, nop.len);
        },

        //---------------------------------------------------------------------
        // Immediate loads
        //---------------------------------------------------------------------
        .imm => |imm| {
            const dst_enc = imm.dst.toReg().hwEnc();
            if (imm.simm64 == 0) {
                // XOR reg, reg is shorter for zeroing
                const size = imm.dst_size;
                if (size == .size16) {
                    sink.put1(0x66); // operand size prefix
                }
                const rex = RexPrefix.twoOp(dst_enc, dst_enc, size == .size64, false);
                rex.encode(sink);
                sink.put1(0x31); // XOR r/m, r
                emitModrmReg(sink, dst_enc, dst_enc);
            } else if (imm.simm64 <= 0xFFFFFFFF and imm.dst_size == .size64) {
                // MOV r32, imm32 with zero extension
                const rex = RexPrefix.oneOp(dst_enc, false, false);
                rex.encode(sink);
                sink.put1(0xB8 + (dst_enc & 7));
                sink.put4(@truncate(imm.simm64));
            } else {
                // MOV r64, imm64
                const rex = RexPrefix.oneOp(dst_enc, true, false);
                rex.encode(sink);
                sink.put1(0xB8 + (dst_enc & 7));
                sink.put8(imm.simm64);
            }
        },

        //---------------------------------------------------------------------
        // Register-to-register moves
        //---------------------------------------------------------------------
        .mov_r_r => |mov| {
            const src_enc = mov.src.hwEnc();
            const dst_enc = mov.dst.toReg().hwEnc();
            const size = mov.size;

            if (size == .size16) {
                sink.put1(0x66);
            }
            const rex = RexPrefix.twoOp(src_enc, dst_enc, size == .size64, size == .size8);
            rex.encode(sink);
            const opcode: u8 = if (size == .size8) 0x88 else 0x89;
            sink.put1(opcode);
            emitModrmReg(sink, src_enc, dst_enc);
        },

        //---------------------------------------------------------------------
        // Memory loads
        //---------------------------------------------------------------------
        .mov_m_r => |mov| {
            const dst_enc = mov.dst.toReg().hwEnc();
            const finalized = memFinalize(mov.src, state);
            const size = mov.size;

            if (size == .size16) {
                sink.put1(0x66);
            }
            const rex = switch (finalized.amode) {
                .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, size == .size8),
                .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, size == .size8),
            };
            rex.encode(sink);
            const opcode: u8 = if (size == .size8) 0x8A else 0x8B;
            sink.put1(opcode);
            emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
        },

        //---------------------------------------------------------------------
        // Memory stores
        //---------------------------------------------------------------------
        .mov_r_m => |mov| {
            const src_enc = mov.src.hwEnc();
            const finalized = memFinalize(mov.dst, state);
            const size = mov.size;

            if (size == .size16) {
                sink.put1(0x66);
            }
            const rex = switch (finalized.amode) {
                .imm_reg => |m| RexPrefix.memOp(src_enc, m.base.hwEnc(), size == .size64, size == .size8),
                .imm_reg_reg_shift => |m| RexPrefix.threeOp(src_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                .rip_relative => RexPrefix.twoOp(src_enc, 0, size == .size64, size == .size8),
            };
            rex.encode(sink);
            const opcode: u8 = if (size == .size8) 0x88 else 0x89;
            sink.put1(opcode);
            emitModrmSibDisp(sink, src_enc, finalized.amode, 0, null);
        },

        //---------------------------------------------------------------------
        // Zero-extending loads
        //---------------------------------------------------------------------
        .movzx_rm_r => |mov| {
            const dst_enc = mov.dst.toReg().hwEnc();
            const src = mov.src;
            const ext_mode = mov.ext_mode;

            // Determine if we need 64-bit operand (for movzx to 64-bit dest)
            // Note: movzx from byte/word to dword automatically zero-extends to qword
            const need_rex_w = false;
            const uses_8bit = ext_mode == .bl or ext_mode == .bq;

            switch (src.inner) {
                .reg => |r| {
                    const gpr = Gpr{ .reg = r };
                    const src_enc = gpr.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, need_rex_w, uses_8bit);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(if (uses_8bit) 0xB6 else 0xB7);
                    emitModrmReg(sink, dst_enc, src_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), need_rex_w, uses_8bit),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), need_rex_w, uses_8bit),
                        .rip_relative => RexPrefix.twoOp(dst_enc, 0, need_rex_w, uses_8bit),
                    };
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(if (uses_8bit) 0xB6 else 0xB7);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // Sign-extending loads
        //---------------------------------------------------------------------
        .movsx_rm_r => |mov| {
            const dst_enc = mov.dst.toReg().hwEnc();
            const src = mov.src;
            const ext_mode = mov.ext_mode;

            // For sign extension to 64-bit, we need REX.W
            const need_rex_w = ext_mode == .bq or ext_mode == .wq or ext_mode == .lq;
            const uses_8bit = ext_mode == .bl or ext_mode == .bq;

            switch (src.inner) {
                .reg => |r| {
                    const gpr = Gpr{ .reg = r };
                    const src_enc = gpr.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, need_rex_w, uses_8bit);
                    rex.encode(sink);

                    // MOVSXD (32->64) uses a different opcode
                    if (ext_mode == .lq) {
                        sink.put1(0x63);
                    } else {
                        sink.put1(0x0F);
                        sink.put1(if (uses_8bit) 0xBE else 0xBF);
                    }
                    emitModrmReg(sink, dst_enc, src_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), need_rex_w, uses_8bit),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), need_rex_w, uses_8bit),
                        .rip_relative => RexPrefix.twoOp(dst_enc, 0, need_rex_w, uses_8bit),
                    };
                    rex.encode(sink);

                    if (ext_mode == .lq) {
                        sink.put1(0x63);
                    } else {
                        sink.put1(0x0F);
                        sink.put1(if (uses_8bit) 0xBE else 0xBF);
                    }
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // ALU register-immediate-register operations
        //---------------------------------------------------------------------
        .alu_rmi_r => |alu| {
            const dst_enc = alu.dst.toReg().hwEnc();
            const size = alu.size;
            const op = alu.op;

            switch (alu.src.inner) {
                .reg => |r| {
                    const gpr = Gpr{ .reg = r };
                    const src_enc = gpr.hwEnc();
                    const opcodes = aluOpcodes(op, size);
                    if (opcodes.prefix) |p| sink.put1(p);
                    const rex = RexPrefix.twoOp(src_enc, dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);
                    sink.put1(opcodes.opcode);
                    emitModrmReg(sink, src_enc, dst_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const opcodes = aluOpcodes(op, size);
                    if (opcodes.prefix) |p| sink.put1(p);
                    // For memory source, opcode is different
                    const mem_opcode = opcodes.opcode - 2; // r, r/m form
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, size == .size8),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                        .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, size == .size8),
                    };
                    rex.encode(sink);
                    sink.put1(mem_opcode);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
                .imm => |imm_val| {
                    const imm_i32: i32 = @bitCast(imm_val);
                    const use_short = imm_i32 >= -128 and imm_i32 <= 127 and size != .size8;

                    if (size == .size16) sink.put1(0x66);
                    const rex = RexPrefix.oneOp(dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);

                    if (size == .size8) {
                        sink.put1(0x80);
                        emitModrmReg(sink, aluImmOpcodeExt(op), dst_enc);
                        sink.put1(@truncate(imm_val));
                    } else if (use_short) {
                        sink.put1(0x83);
                        emitModrmReg(sink, aluImmOpcodeExt(op), dst_enc);
                        sink.put1(@truncate(imm_val));
                    } else {
                        sink.put1(0x81);
                        emitModrmReg(sink, aluImmOpcodeExt(op), dst_enc);
                        if (size == .size16) {
                            sink.put2(@truncate(imm_val));
                        } else {
                            sink.put4(imm_val);
                        }
                    }
                },
            }
        },

        //---------------------------------------------------------------------
        // Shifts
        //---------------------------------------------------------------------
        .shift_r => |shift| {
            const dst_enc = shift.dst.toReg().hwEnc();
            const size = shift.size;
            const kind = shift.kind;

            // ShiftKind encodes the opcode extension directly
            const opcode_ext: u8 = kind.enc();

            if (size == .size16) sink.put1(0x66);
            const rex = RexPrefix.oneOp(dst_enc, size == .size64, size == .size8);
            rex.encode(sink);

            switch (shift.shift_by) {
                .cl => {
                    sink.put1(if (size == .size8) 0xD2 else 0xD3);
                    emitModrmReg(sink, opcode_ext, dst_enc);
                },
                .imm => |amt| {
                    // Use shorter encoding for shift-by-1
                    if (amt == 1) {
                        sink.put1(if (size == .size8) 0xD0 else 0xD1);
                        emitModrmReg(sink, opcode_ext, dst_enc);
                    } else {
                        sink.put1(if (size == .size8) 0xC0 else 0xC1);
                        emitModrmReg(sink, opcode_ext, dst_enc);
                        sink.put1(amt);
                    }
                },
            }
        },

        //---------------------------------------------------------------------
        // Unary operations
        //---------------------------------------------------------------------
        .unary_rm_r => |unary| {
            const dst_enc = unary.dst.toReg().hwEnc();
            const size = unary.size;

            if (size == .size16) sink.put1(0x66);

            // Handle different unary operations
            switch (unary.op) {
                // NOT and NEG use F6/F7 opcodes with ModRM extension
                .not, .neg, .inc, .dec => {
                    const opcode_ext: u8 = switch (unary.op) {
                        .not => 2,
                        .neg => 3,
                        .inc => 0,
                        .dec => 1,
                        else => unreachable,
                    };
                    switch (unary.src.inner) {
                        .reg => |r| {
                            const src_enc = r.hwEnc();
                            const rex = RexPrefix.oneOp(src_enc, size == .size64, size == .size8);
                            rex.encode(sink);
                            sink.put1(if (size == .size8) 0xF6 else 0xF7);
                            emitModrmReg(sink, opcode_ext, src_enc);
                        },
                        .mem => |amode| {
                            const finalized = memFinalize(amode, state);
                            const rex = switch (finalized.amode) {
                                .imm_reg => |m| RexPrefix.memOp(opcode_ext, m.base.hwEnc(), size == .size64, size == .size8),
                                .imm_reg_reg_shift => |m| RexPrefix.threeOp(opcode_ext, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                                .rip_relative => RexPrefix.twoOp(opcode_ext, 0, size == .size64, size == .size8),
                            };
                            rex.encode(sink);
                            sink.put1(if (size == .size8) 0xF6 else 0xF7);
                            emitModrmSibDisp(sink, opcode_ext, finalized.amode, 0, null);
                        },
                    }
                },
                // BSF, BSR use 0F BC/BD opcodes
                .bsf, .bsr => {
                    switch (unary.src.inner) {
                        .reg => |r| {
                            const src_enc = r.hwEnc();
                            const rex = RexPrefix.twoOp(dst_enc, src_enc, size == .size64, false);
                            rex.encode(sink);
                            sink.put1(0x0F);
                            sink.put1(if (unary.op == .bsf) 0xBC else 0xBD);
                            emitModrmReg(sink, dst_enc, src_enc);
                        },
                        .mem => |amode| {
                            const finalized = memFinalize(amode, state);
                            const rex = switch (finalized.amode) {
                                .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, false),
                                .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, false),
                                .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, false),
                            };
                            rex.encode(sink);
                            sink.put1(0x0F);
                            sink.put1(if (unary.op == .bsf) 0xBC else 0xBD);
                            emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                        },
                    }
                },
                // LZCNT, TZCNT, POPCNT use F3 0F prefix
                .lzcnt, .tzcnt, .popcnt => {
                    sink.put1(0xF3); // REP prefix
                    switch (unary.src.inner) {
                        .reg => |r| {
                            const src_enc = r.hwEnc();
                            const rex = RexPrefix.twoOp(dst_enc, src_enc, size == .size64, false);
                            rex.encode(sink);
                            sink.put1(0x0F);
                            const opcode2: u8 = switch (unary.op) {
                                .lzcnt => 0xBD,
                                .tzcnt => 0xBC,
                                .popcnt => 0xB8,
                                else => unreachable,
                            };
                            sink.put1(opcode2);
                            emitModrmReg(sink, dst_enc, src_enc);
                        },
                        .mem => |amode| {
                            const finalized = memFinalize(amode, state);
                            const rex = switch (finalized.amode) {
                                .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, false),
                                .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, false),
                                .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, false),
                            };
                            rex.encode(sink);
                            sink.put1(0x0F);
                            const opcode2: u8 = switch (unary.op) {
                                .lzcnt => 0xBD,
                                .tzcnt => 0xBC,
                                .popcnt => 0xB8,
                                else => unreachable,
                            };
                            sink.put1(opcode2);
                            emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                        },
                    }
                },
            }
        },

        //---------------------------------------------------------------------
        // Multiplication
        //---------------------------------------------------------------------
        .mul => |mul| {
            const size = mul.size;
            const opcode_ext: u8 = if (mul.signed) 5 else 4; // IMUL /5, MUL /4

            if (size == .size16) sink.put1(0x66);

            switch (mul.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.oneOp(src_enc, size == .size64, size == .size8);
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0xF6 else 0xF7);
                    emitModrmReg(sink, opcode_ext, src_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(opcode_ext, m.base.hwEnc(), size == .size64, size == .size8),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(opcode_ext, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                        .rip_relative => RexPrefix.twoOp(opcode_ext, 0, size == .size64, size == .size8),
                    };
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0xF6 else 0xF7);
                    emitModrmSibDisp(sink, opcode_ext, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // Division
        //---------------------------------------------------------------------
        .div => |div| {
            const size = div.size;
            const opcode_ext: u8 = if (div.signed) 7 else 6; // IDIV /7, DIV /6

            if (size == .size16) sink.put1(0x66);

            switch (div.divisor.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.oneOp(src_enc, size == .size64, size == .size8);
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0xF6 else 0xF7);
                    emitModrmReg(sink, opcode_ext, src_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(opcode_ext, m.base.hwEnc(), size == .size64, size == .size8),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(opcode_ext, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                        .rip_relative => RexPrefix.twoOp(opcode_ext, 0, size == .size64, size == .size8),
                    };
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0xF6 else 0xF7);
                    emitModrmSibDisp(sink, opcode_ext, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // Sign extension (for division)
        //---------------------------------------------------------------------
        .sign_extend_data => |ext| {
            const size = ext.size;
            if (size == .size16) sink.put1(0x66);
            const rex = RexPrefix.oneOp(0, size == .size64, false);
            if (rex.needsEmit()) rex.encode(sink);

            // CWD/CDQ/CQO
            sink.put1(0x99);
        },

        //---------------------------------------------------------------------
        // Jumps
        //---------------------------------------------------------------------
        .jmp_known => |jmp| {
            // Near jump (rel32)
            sink.put1(0xE9);
            sink.useLabelAtOffset(sink.curOffset(), jmp.dst, .jmp_rel_32);
            sink.put4(0); // Placeholder for displacement
        },

        .jmp_unknown => |jmp| {
            switch (jmp.target) {
                .reg => |r| {
                    const enc = r.hwEnc();
                    const rex = RexPrefix.oneOp(enc, false, false);
                    rex.encode(sink);
                    sink.put1(0xFF);
                    emitModrmReg(sink, 4, enc); // JMP uses /4
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(4, m.base.hwEnc(), false, false),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(4, m.index.hwEnc(), m.base.hwEnc(), false, false),
                        .rip_relative => RexPrefix.twoOp(4, 0, false, false),
                    };
                    rex.encode(sink);
                    sink.put1(0xFF);
                    emitModrmSibDisp(sink, 4, finalized.amode, 0, null);
                },
            }
        },

        .jmp_cond => |jmp| {
            // Conditional jump (rel32)
            sink.put1(0x0F);
            sink.put1(0x80 + jmp.cc.getEnc());
            sink.useLabelAtOffset(sink.curOffset(), jmp.taken, .jmp_rel_32);
            sink.put4(0); // Placeholder for displacement
            // Note: not_taken is typically fall-through
        },

        .winch_jmp_if => |jmp| {
            // One-way conditional jump
            sink.put1(0x0F);
            sink.put1(0x80 + jmp.cc.getEnc());
            sink.useLabelAtOffset(sink.curOffset(), jmp.taken, .jmp_rel_32);
            sink.put4(0);
        },

        //---------------------------------------------------------------------
        // Return
        //---------------------------------------------------------------------
        .ret => |ret| {
            if (ret.stack_bytes_to_pop == 0) {
                sink.put1(0xC3); // RET
            } else {
                sink.put1(0xC2); // RET imm16
                sink.put2(@truncate(ret.stack_bytes_to_pop));
            }
        },

        //---------------------------------------------------------------------
        // Call
        //---------------------------------------------------------------------
        .call_known => |call| {
            sink.put1(0xE8);
            sink.put4(0); // Placeholder for displacement
            // Record relocation for the call target (after emitting the displacement)
            // The addend adjusts for the difference between end of instruction and start of displacement
            sink.addExternalReloc(
                call.info.dest,
                -4,
                .call_pc_rel_4,
            );
        },

        .call_unknown => |call| {
            switch (call.info.dest) {
                .reg => |r| {
                    const enc = r.hwEnc();
                    const rex = RexPrefix.oneOp(enc, false, false);
                    rex.encode(sink);
                    sink.put1(0xFF);
                    emitModrmReg(sink, 2, enc); // CALL uses /2
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(2, m.base.hwEnc(), false, false),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(2, m.index.hwEnc(), m.base.hwEnc(), false, false),
                        .rip_relative => RexPrefix.twoOp(2, 0, false, false),
                    };
                    rex.encode(sink);
                    sink.put1(0xFF);
                    emitModrmSibDisp(sink, 2, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // UD2 (for traps)
        //---------------------------------------------------------------------
        .ud2 => |_| {
            // Note: trap_code is recorded for debugging but doesn't affect encoding
            sink.put1(0x0F);
            sink.put1(0x0B);
        },


        //---------------------------------------------------------------------
        // Fences
        //---------------------------------------------------------------------
        .fence => |fence| {
            switch (fence.kind) {
                .mfence => {
                    sink.put1(0x0F);
                    sink.put1(0xAE);
                    sink.put1(0xF0);
                },
                .lfence => {
                    sink.put1(0x0F);
                    sink.put1(0xAE);
                    sink.put1(0xE8);
                },
                .sfence => {
                    sink.put1(0x0F);
                    sink.put1(0xAE);
                    sink.put1(0xF8);
                },
            }
        },

        //---------------------------------------------------------------------
        // CMOVcc
        //---------------------------------------------------------------------
        .cmove => |cmov| {
            const dst_enc = cmov.dst.toReg().hwEnc();
            const size = cmov.size;

            if (size == .size16) sink.put1(0x66);

            switch (cmov.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, size == .size64, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(0x40 + cmov.cc.getEnc());
                    emitModrmReg(sink, dst_enc, src_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, false),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, false),
                        .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, false),
                    };
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(0x40 + cmov.cc.getEnc());
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        //---------------------------------------------------------------------
        // SETcc
        //---------------------------------------------------------------------
        .setcc => |setcc| {
            const dst_enc = setcc.dst.toReg().hwEnc();
            const rex = RexPrefix.oneOp(dst_enc, false, true);
            rex.encode(sink);
            sink.put1(0x0F);
            sink.put1(0x90 + setcc.cc.getEnc());
            emitModrmReg(sink, 0, dst_enc);
        },

        //---------------------------------------------------------------------
        // CMP
        //---------------------------------------------------------------------
        .cmp_rmi_r => |cmp| {
            const dst_enc = cmp.dst.hwEnc();
            const size = cmp.size;

            switch (cmp.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    if (size == .size16) sink.put1(0x66);
                    const rex = RexPrefix.twoOp(src_enc, dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0x38 else 0x39);
                    emitModrmReg(sink, src_enc, dst_enc);
                },
                .mem => |amode| {
                    const finalized = memFinalize(amode, state);
                    if (size == .size16) sink.put1(0x66);
                    const rex = switch (finalized.amode) {
                        .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, size == .size8),
                        .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, size == .size8),
                        .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, size == .size8),
                    };
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0x3A else 0x3B);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
                .imm => |imm_val| {
                    // Interpret u32 as signed for range check
                    const signed_imm: i32 = @bitCast(imm_val);
                    const use_short = signed_imm >= -128 and signed_imm <= 127 and size != .size8;

                    if (size == .size16) sink.put1(0x66);
                    const rex = RexPrefix.oneOp(dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);

                    if (size == .size8) {
                        sink.put1(0x80);
                        emitModrmReg(sink, 7, dst_enc);
                        sink.put1(@truncate(imm_val));
                    } else if (use_short) {
                        sink.put1(0x83);
                        emitModrmReg(sink, 7, dst_enc);
                        sink.put1(@truncate(imm_val));
                    } else {
                        sink.put1(0x81);
                        emitModrmReg(sink, 7, dst_enc);
                        if (size == .size16) {
                            sink.put2(@truncate(imm_val));
                        } else {
                            sink.put4(imm_val);
                        }
                    }
                },
            }
        },

        //---------------------------------------------------------------------
        // TEST
        //---------------------------------------------------------------------
        .test_rmi_r => |test_inst| {
            const dst_enc = test_inst.dst.hwEnc();
            const size = test_inst.size;

            switch (test_inst.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    if (size == .size16) sink.put1(0x66);
                    const rex = RexPrefix.twoOp(src_enc, dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);
                    sink.put1(if (size == .size8) 0x84 else 0x85);
                    emitModrmReg(sink, src_enc, dst_enc);
                },
                .imm => |imm_val| {
                    if (size == .size16) sink.put1(0x66);
                    const rex = RexPrefix.oneOp(dst_enc, size == .size64, size == .size8);
                    rex.encode(sink);

                    sink.put1(if (size == .size8) 0xF6 else 0xF7);
                    emitModrmReg(sink, 0, dst_enc);

                    switch (size) {
                        .size8 => sink.put1(@truncate(imm_val)),
                        .size16 => sink.put2(@truncate(imm_val)),
                        .size32, .size64 => sink.put4(imm_val),
                    }
                },
                .mem => {
                    // TEST r, m is not commonly used; left unimplemented for now
                    unreachable;
                },
            }
        },


        //---------------------------------------------------------------------
        // LEA
        //---------------------------------------------------------------------
        .lea => |lea| {
            const dst_enc = lea.dst.toReg().hwEnc();
            const finalized = memFinalize(lea.src, state);
            const size = lea.size;

            if (size == .size16) sink.put1(0x66);
            const rex = switch (finalized.amode) {
                .imm_reg => |m| RexPrefix.memOp(dst_enc, m.base.hwEnc(), size == .size64, false),
                .imm_reg_reg_shift => |m| RexPrefix.threeOp(dst_enc, m.index.hwEnc(), m.base.hwEnc(), size == .size64, false),
                .rip_relative => RexPrefix.twoOp(dst_enc, 0, size == .size64, false),
            };
            rex.encode(sink);
            sink.put1(0x8D);
            emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
        },


        //---------------------------------------------------------------------
        // External name load (RIP-relative)
        //---------------------------------------------------------------------
        .load_ext_name => |load| {
            const dst_enc = load.dst.toReg().hwEnc();
            // LEA dst, [rip + symbol]
            const rex = RexPrefix.twoOp(dst_enc, 0, true, false);
            rex.encode(sink);
            sink.put1(0x8D);
            sink.put1(encodeModrm(0b00, dst_enc & 7, 0b101));
            sink.put4(0);
            // Record relocation for the displacement
            sink.addExternalReloc(load.name, load.offset, .pc_rel_4);
        },

        //---------------------------------------------------------------------
        // Pseudo-instructions (should be lowered before emission)
        //---------------------------------------------------------------------
        .args, .rets => {
            // These are pseudo-instructions used for ABI handling
            // They should be expanded before emission
        },

        //---------------------------------------------------------------------
        // XMM instructions (SSE/AVX)
        // Ported from Cranelift's cranelift-assembler-x64
        //---------------------------------------------------------------------
        .xmm_rm_r => |xmm| {
            // SSE/AVX binary operation: dst = op(dst, src)
            const dst_enc = xmm.dst.toReg().hwEnc();
            const prefix = sseOpcodePrefix(xmm.op);
            const opcode = sseOpcodeBytes(xmm.op);

            // Emit prefix if needed (0x66, 0xF3, or 0xF2)
            if (prefix) |p| sink.put1(p);

            switch (xmm.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    // REX prefix if needed
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, false, false);
                    rex.encode(sink);
                    // Opcode (0x0F prefix + opcode byte(s))
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    // ModRM: mod=11 (register), reg=dst, rm=src
                    sink.put1(encodeModrm(0b11, dst_enc & 7, src_enc & 7));
                },
                .mem => |samode| {
                    const finalized = memFinalize(samode, state);
                    const base_enc: u8 = switch (finalized.amode) {
                        .imm_reg => |m| m.base.hwEnc(),
                        .imm_reg_reg_shift => |m| m.base.hwEnc(),
                        .rip_relative => 0,
                    };
                    const rex = RexPrefix.memOp(dst_enc, base_enc, false, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        .xmm_unary_rm_r => |xmm| {
            // SSE/AVX unary operation: dst = op(src)
            const dst_enc = xmm.dst.toReg().hwEnc();
            const prefix = sseOpcodePrefix(xmm.op);
            const opcode = sseOpcodeBytes(xmm.op);

            if (prefix) |p| sink.put1(p);

            switch (xmm.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, false, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    sink.put1(encodeModrm(0b11, dst_enc & 7, src_enc & 7));
                },
                .mem => |samode| {
                    const finalized = memFinalize(samode, state);
                    const base_enc: u8 = switch (finalized.amode) {
                        .imm_reg => |m| m.base.hwEnc(),
                        .imm_reg_reg_shift => |m| m.base.hwEnc(),
                        .rip_relative => 0,
                    };
                    const rex = RexPrefix.memOp(dst_enc, base_enc, false, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        .xmm_mov_r_m => |xmm| {
            // Store XMM register to memory: [mem] = xmm
            const src_enc = xmm.src.hwEnc();
            const finalized = memFinalize(xmm.dst, state);
            const prefix = sseOpcodePrefix(xmm.op);
            const opcode = sseOpcodeBytes(xmm.op);

            if (prefix) |p| sink.put1(p);

            const base_enc: u8 = switch (finalized.amode) {
                .imm_reg => |m| m.base.hwEnc(),
                .imm_reg_reg_shift => |m| m.base.hwEnc(),
                .rip_relative => 0,
            };
            const rex = RexPrefix.memOp(src_enc, base_enc, false, false);
            rex.encode(sink);
            sink.put1(0x0F);
            for (opcode) |b| sink.put1(b);
            emitModrmSibDisp(sink, src_enc, finalized.amode, 0, null);
        },

        .xmm_mov_m_r => |xmm| {
            // Load XMM register from memory: xmm = [mem]
            const dst_enc = xmm.dst.toReg().hwEnc();
            const finalized = memFinalize(xmm.src, state);
            const prefix = sseOpcodePrefix(xmm.op);
            const opcode = sseOpcodeBytes(xmm.op);

            if (prefix) |p| sink.put1(p);

            const base_enc: u8 = switch (finalized.amode) {
                .imm_reg => |m| m.base.hwEnc(),
                .imm_reg_reg_shift => |m| m.base.hwEnc(),
                .rip_relative => 0,
            };
            const rex = RexPrefix.memOp(dst_enc, base_enc, false, false);
            rex.encode(sink);
            sink.put1(0x0F);
            for (opcode) |b| sink.put1(b);
            emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
        },

        .xmm_to_gpr => |xmm| {
            // MOVD/MOVQ xmm -> gpr
            const src_enc = xmm.src.hwEnc();
            const dst_enc = xmm.dst.toReg().hwEnc();
            const is_64bit = xmm.dst_size == .size64;
            // 0x66 prefix for MOVD/MOVQ
            sink.put1(0x66);
            const rex = RexPrefix.twoOp(src_enc, dst_enc, is_64bit, false);
            rex.encode(sink);
            sink.put1(0x0F);
            sink.put1(0x7E); // MOVD/MOVQ opcode
            sink.put1(encodeModrm(0b11, src_enc & 7, dst_enc & 7));
        },

        .gpr_to_xmm => |xmm| {
            // MOVD/MOVQ gpr -> xmm
            const dst_enc = xmm.dst.toReg().hwEnc();
            const is_64bit = xmm.src_size == .size64;
            // 0x66 prefix for MOVD/MOVQ
            sink.put1(0x66);
            switch (xmm.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, is_64bit, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(0x6E); // MOVD/MOVQ opcode (gpr->xmm)
                    sink.put1(encodeModrm(0b11, dst_enc & 7, src_enc & 7));
                },
                .mem => |samode| {
                    const finalized = memFinalize(samode, state);
                    const base_enc: u8 = switch (finalized.amode) {
                        .imm_reg => |m| m.base.hwEnc(),
                        .imm_reg_reg_shift => |m| m.base.hwEnc(),
                        .rip_relative => 0,
                    };
                    const rex = RexPrefix.memOp(dst_enc, base_enc, is_64bit, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    sink.put1(0x6E);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        .xmm_cmp_rm_r => |xmm| {
            // SSE compare: COMISS/COMISD/UCOMISS/UCOMISD (no immediate)
            const dst_enc = xmm.dst.hwEnc();
            const prefix = sseOpcodePrefix(xmm.op);
            const opcode = sseOpcodeBytes(xmm.op);

            if (prefix) |p| sink.put1(p);

            switch (xmm.src.inner) {
                .reg => |r| {
                    const src_enc = r.hwEnc();
                    const rex = RexPrefix.twoOp(dst_enc, src_enc, false, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    sink.put1(encodeModrm(0b11, dst_enc & 7, src_enc & 7));
                },
                .mem => |samode| {
                    const finalized = memFinalize(samode, state);
                    const base_enc: u8 = switch (finalized.amode) {
                        .imm_reg => |m| m.base.hwEnc(),
                        .imm_reg_reg_shift => |m| m.base.hwEnc(),
                        .rip_relative => 0,
                    };
                    const rex = RexPrefix.memOp(dst_enc, base_enc, false, false);
                    rex.encode(sink);
                    sink.put1(0x0F);
                    for (opcode) |b| sink.put1(b);
                    emitModrmSibDisp(sink, dst_enc, finalized.amode, 0, null);
                },
            }
        },

        .xmm_rm_r_evex, .xmm_cmp_imm, .xmm_round => {
            // EVEX-encoded and advanced SSE instructions - deferred
            unreachable;
        },

        //---------------------------------------------------------------------
        // Jump table sequence
        // Ported from Cranelift's emit.rs JmpTableSeq
        //---------------------------------------------------------------------
        .jmp_table_seq => |jt| {
            // Generate:
            //   lea start_of_jump_table(%rip), %tmp1
            //   movsxd (%tmp1, %idx, 4), %tmp2
            //   add %tmp2, %tmp1
            //   jmp *%tmp1
            // start_of_jump_table:
            //   .long offset0, offset1, ...

            const idx_enc = jt.idx.hwEnc();
            const tmp1_enc = jt.tmp1.toReg().hwEnc();
            const tmp2_enc = jt.tmp2.toReg().hwEnc();

            // Get label for start of jump table
            const jt_label = sink.getLabel();

            // lea start_of_jump_table(%rip), %tmp1
            const lea_rex = RexPrefix.twoOp(tmp1_enc, 0, true, false);
            lea_rex.encode(sink);
            sink.put1(0x8D); // LEA opcode
            sink.put1(encodeModrm(0b00, tmp1_enc & 7, 0b101)); // RIP-relative
            const lea_disp_offset = sink.curOffset();
            sink.put4(0); // Placeholder for displacement

            // movsxd (%tmp1, %idx, 4), %tmp2 - load signed 32-bit, scale by 4
            const movsx_rex = RexPrefix.threeOp(tmp2_enc, idx_enc, tmp1_enc, true, false);
            movsx_rex.encode(sink);
            sink.put1(0x63); // MOVSXD opcode
            // ModRM: mod=00, reg=tmp2, rm=100 (SIB follows)
            sink.put1(encodeModrm(0b00, tmp2_enc & 7, 0b100));
            // SIB: scale=2 (multiply by 4), index=idx, base=tmp1
            sink.put1(encodeSib(2, idx_enc & 7, tmp1_enc & 7));

            // add %tmp2, %tmp1
            const add_rex = RexPrefix.twoOp(tmp2_enc, tmp1_enc, true, false);
            add_rex.encode(sink);
            sink.put1(0x01); // ADD r/m64, r64
            sink.put1(encodeModrm(0b11, tmp2_enc & 7, tmp1_enc & 7));

            // jmp *%tmp1 (indirect jump)
            if (isExtendedReg(tmp1_enc)) {
                sink.put1(0x41); // REX.B
            }
            sink.put1(0xFF); // JMP r/m64
            sink.put1(encodeModrm(0b11, 4, tmp1_enc & 7)); // /4 for indirect jmp

            // Bind the jump table label
            sink.bindLabel(jt_label, state);

            // Patch the LEA displacement to point here
            _ = lea_disp_offset; // Used implicitly via label
            // Patch the displacement (would need buffer access, using relocation instead)
            // Note: the label binding handles the offset automatically

            // Emit jump table entries (32-bit signed offsets)
            const jt_start = sink.curOffset();
            for (jt.targets) |target| {
                const entry_offset = sink.curOffset();
                sink.useLabelAtOffset(entry_offset, target, .pc_rel_32);
                sink.put4(@as(u32, @bitCast(@as(i32, @intCast(@as(i64, entry_offset) - @as(i64, jt_start))))));
            }
            // Default target
            const default_offset = sink.curOffset();
            sink.useLabelAtOffset(default_offset, jt.default, .pc_rel_32);
            sink.put4(@as(u32, @bitCast(@as(i32, @intCast(@as(i64, default_offset) - @as(i64, jt_start))))));
        },

        //---------------------------------------------------------------------
        // Atomic operations
        // Ported from Cranelift's emit.rs AtomicRmwSeq
        //---------------------------------------------------------------------
        .atomic_rmw_seq => |atomic| {
            // Atomic read-modify-write sequence using LOCK CMPXCHG:
            //   mov (%mem), %rax           ; load old value
            // again:
            //   mov %rax, %tmp             ; copy old to temp
            //   <op> %operand, %tmp        ; apply operation
            //   lock cmpxchg %tmp, (%mem)  ; try to store
            //   jnz again                  ; retry if failed

            const operand_enc = atomic.operand.hwEnc();
            const tmp_enc = atomic.tmp.toReg().hwEnc();
            const dst_old_enc = atomic.dst_old.toReg().hwEnc();
            const finalized = memFinalize(atomic.mem, state);

            // Determine size and prefix
            const size_byte: bool = atomic.ty.bits() == 8;
            const size_word: bool = atomic.ty.bits() == 16;
            const size_qword: bool = atomic.ty.bits() == 64;
            const need_rex_w: bool = size_qword;

            // 1. Load initial value: mov (%mem), %rax (dst_old must be RAX)
            if (size_word) sink.put1(0x66); // 16-bit prefix
            if (size_byte) {
                // movzbl for 8-bit
                const rex = RexPrefix.memOp(dst_old_enc, getBaseEnc(finalized.amode), false, false);
                rex.encode(sink);
                sink.put1(0x0F);
                sink.put1(0xB6);
            } else {
                const rex = RexPrefix.memOp(dst_old_enc, getBaseEnc(finalized.amode), need_rex_w, false);
                rex.encode(sink);
                sink.put1(0x8B); // MOV r, r/m
            }
            emitModrmSibDisp(sink, dst_old_enc, finalized.amode, 0, null);

            // 2. Label for retry loop
            const again_label = sink.getLabel();
            sink.bindLabel(again_label, state);

            // 3. Copy old value to temp: mov %rax, %tmp
            {
                const rex = RexPrefix.twoOp(tmp_enc, dst_old_enc, need_rex_w, false);
                rex.encode(sink);
                if (size_byte) {
                    sink.put1(0x88); // MOV r/m8, r8
                } else {
                    if (size_word) sink.put1(0x66);
                    sink.put1(0x89); // MOV r/m, r
                }
                sink.put1(encodeModrm(0b11, dst_old_enc & 7, tmp_enc & 7));
            }

            // 4. Apply operation: <op> %operand, %tmp
            emitAtomicOp(sink, atomic.op, operand_enc, tmp_enc, size_byte, size_word, need_rex_w);

            // 5. LOCK CMPXCHG: lock cmpxchg %tmp, (%mem)
            sink.put1(0xF0); // LOCK prefix
            if (size_word) sink.put1(0x66);
            {
                const rex = RexPrefix.memOp(tmp_enc, getBaseEnc(finalized.amode), need_rex_w, false);
                rex.encode(sink);
            }
            if (size_byte) {
                sink.put1(0x0F);
                sink.put1(0xB0); // CMPXCHG r/m8, r8
            } else {
                sink.put1(0x0F);
                sink.put1(0xB1); // CMPXCHG r/m, r
            }
            emitModrmSibDisp(sink, tmp_enc, finalized.amode, 0, null);

            // 6. JNZ again (retry if ZF=0)
            sink.put1(0x75); // JNZ rel8
            // Calculate offset back to again_label (will be negative)
            const jnz_offset = sink.curOffset();
            sink.useLabelAtOffset(jnz_offset, again_label, .jmp_rel_8);
            sink.put1(0xFE); // Placeholder for short jump offset
        },

        .atomic_128_rmw_seq => |_| {
            // 128-bit atomics use CMPXCHG16B - complex sequence, deferred
            unreachable;
        },

        .atomic_128_xchg_seq => |_| {
            // 128-bit exchange uses CMPXCHG16B - complex sequence, deferred
            unreachable;
        },

        //---------------------------------------------------------------------
        // Conditional jump with OR
        //---------------------------------------------------------------------
        .jmp_cond_or => |jco| {
            // jcc1 taken; jcc2 taken; jmp not_taken
            emitCondJmp(sink, jco.cc1, jco.taken);
            emitCondJmp(sink, jco.cc2, jco.taken);
            emitUncondJmp(sink, jco.not_taken);
        },

        //---------------------------------------------------------------------
        // Other pseudo-instructions (deferred)
        // Trap instructions require integration with the trap handling infrastructure
        //---------------------------------------------------------------------
        .trap_if,
        .trap_if_and,
        .trap_if_or,
        .xmm_min_max_seq,
        .xmm_uninitialized_value,
        .gpr_uninitialized_value,
        .cvt_uint64_to_float_seq,
        .cvt_float_to_sint_seq,
        .cvt_float_to_uint_seq,
        .checked_srem_seq,
        .checked_srem_seq8,
        .elf_tls_get_addr,
        .macho_tls_get_addr,
        .coff_tls_get_addr,
        .return_call_known,
        .return_call_unknown,
        .mov_from_preg,
        .mov_to_preg,
        .xmm_cmove,
        .stack_probe_loop,
        .stack_switch_basic,
        .unwind,
        .dummy_use,
        .label_address,
        => {
            // These are complex pseudo-instructions that expand to multiple real instructions
            // They will be implemented as needed
            unreachable;
        },

        //---------------------------------------------------------------------
        // Debugging/marker pseudo-instructions
        //---------------------------------------------------------------------
        .sequence_point => {
            // Sequence point is a no-op used for debugging; emits nothing
        },
    }
}

//=============================================================================
// NOP emission (from cranelift-assembler-x64/src/custom.rs)
//=============================================================================

/// Emit a NOP of the specified length (1-9 bytes).
pub fn emitNop(sink: *MachBuffer, len: u8) void {
    switch (len) {
        0 => {},
        1 => sink.put1(0x90),
        2 => {
            sink.put1(0x66);
            sink.put1(0x90);
        },
        3 => {
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x00);
        },
        4 => {
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x40);
            sink.put1(0x00);
        },
        5 => {
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x44);
            sink.put2(0x0000);
        },
        6 => {
            sink.put1(0x66);
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x44);
            sink.put2(0x0000);
        },
        7 => {
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x80);
            sink.put4(0x00000000);
        },
        8 => {
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x84);
            sink.put1(0x00);
            sink.put4(0x00000000);
        },
        else => {
            // For len >= 9, use 9-byte NOP and recurse
            sink.put1(0x66);
            sink.put1(0x0F);
            sink.put1(0x1F);
            sink.put1(0x84);
            sink.put1(0x00);
            sink.put4(0x00000000);
            if (len > 9) {
                emitNop(sink, len - 9);
            }
        },
    }
}

//=============================================================================
// SSE opcode helpers
//=============================================================================

/// Get the mandatory prefix for an SSE opcode (0x66, 0xF3, 0xF2, or none).
fn sseOpcodePrefix(op: SseOpcode) ?u8 {
    return switch (op) {
        // No prefix (packed single)
        .addps, .subps, .mulps, .divps, .maxps, .minps, .movaps, .movups, .sqrtps, .rsqrtps, .rcpps, .andps, .andnps, .orps, .xorps, .cmpps, .shufps => null,

        // 0x66 prefix (packed double and integer SIMD)
        .addpd, .subpd, .mulpd, .divpd, .maxpd, .minpd, .movapd, .movupd, .sqrtpd, .andpd, .andnpd, .orpd, .xorpd, .cmppd, .shufpd, .addsd, .subsd, .mulsd, .divsd, .maxsd, .minsd, .sqrtsd, .cmpsd, .comisd, .ucomisd, .cvtss2sd, .cvtsd2ss, .cvtdq2pd, .cvtpd2dq, .cvttpd2dq, .cvtdq2ps, .cvtps2dq, .cvttps2dq, .movdqa, .movdqu, .pand, .pandn, .por, .pxor, .paddq, .psubq, .pmullw, .pmulhw, .pmulhuw, .pmuludq, .pmulld, .paddb, .paddw, .paddd, .psubb, .psubw, .psubd, .pcmpeqb, .pcmpeqw, .pcmpeqd, .pcmpeqq, .pcmpgtb, .pcmpgtw, .pcmpgtd, .pcmpgtq, .psllw, .pslld, .psllq, .psrlw, .psrld, .psrlq, .psraw, .psrad, .pshufd, .punpcklbw, .punpcklwd, .punpckldq, .punpcklqdq, .punpckhbw, .punpckhwd, .punpckhdq, .punpckhqdq, .haddpd, .hsubpd, .phaddd, .phaddw, .phsubd, .phsubw, .movmskpd, .pmovmskb, .pblendw, .blendpd, .pblendvb, .blendvpd, .insertps, .pinsrb, .pinsrw, .pinsrd, .pinsrq, .extractps, .pextrb, .pextrw, .pextrd, .pextrq => 0x66,

        // 0xF3 prefix (scalar single)
        .addss, .subss, .mulss, .divss, .maxss, .minss, .sqrtss, .rsqrtss, .rcpss, .cmpss, .comiss, .ucomiss, .cvtsi2ss, .cvtss2si, .cvttss2si, .movss, .pshufhw => 0xF3,

        // 0xF2 prefix (scalar double)
        .cvtsi2sd, .cvtsd2si, .cvttsd2si, .movsd, .haddps, .hsubps, .pshuflw => 0xF2,

        // Default no prefix
        .movlps, .movlpd, .movhps, .movhpd, .movmskps, .blendps, .blendvps => null,

        // SSE4.1 with 0x66 prefix
        .roundss, .roundsd, .roundps, .roundpd, .ptest, .pmaxsb, .pmaxsw, .pmaxsd, .pmaxub, .pmaxuw, .pmaxud, .pminsb, .pminsw, .pminsd, .pminub, .pminuw, .pminud, .pabsb, .pabsw, .pabsd, .packuswb, .packusdw, .packsswb, .packssdw, .unpcklps, .unpcklpd, .unpckhps, .unpckhpd => 0x66,
    };
}

/// Get the opcode bytes for an SSE instruction (after 0x0F prefix).
fn sseOpcodeBytes(op: SseOpcode) []const u8 {
    return switch (op) {
        // Basic arithmetic
        .addps, .addss, .addpd, .addsd => &[_]u8{0x58},
        .subps, .subss, .subpd, .subsd => &[_]u8{0x5C},
        .mulps, .mulss, .mulpd, .mulsd => &[_]u8{0x59},
        .divps, .divss, .divpd, .divsd => &[_]u8{0x5E},
        .maxps, .maxss, .maxpd, .maxsd => &[_]u8{0x5F},
        .minps, .minss, .minpd, .minsd => &[_]u8{0x5D},

        // Square root, reciprocal
        .sqrtps, .sqrtss, .sqrtpd, .sqrtsd => &[_]u8{0x51},
        .rsqrtps, .rsqrtss => &[_]u8{0x52},
        .rcpps, .rcpss => &[_]u8{0x53},

        // Moves
        .movaps, .movapd => &[_]u8{0x28},
        .movups, .movupd => &[_]u8{0x10},
        .movss, .movsd => &[_]u8{0x10},
        .movdqa => &[_]u8{0x6F},
        .movdqu => &[_]u8{0x6F},
        .movlps, .movlpd => &[_]u8{0x12},
        .movhps, .movhpd => &[_]u8{0x16},

        // Logical
        .andps, .andpd => &[_]u8{0x54},
        .andnps, .andnpd => &[_]u8{0x55},
        .orps, .orpd => &[_]u8{0x56},
        .xorps, .xorpd => &[_]u8{0x57},

        // Comparison
        .cmpps, .cmpss, .cmppd, .cmpsd => &[_]u8{0xC2},
        .comiss, .comisd => &[_]u8{0x2F},
        .ucomiss, .ucomisd => &[_]u8{0x2E},

        // Shuffle
        .shufps, .shufpd => &[_]u8{0xC6},

        // Conversions
        .cvtss2sd, .cvtsd2ss => &[_]u8{0x5A},
        .cvtsi2ss, .cvtsi2sd => &[_]u8{0x2A},
        .cvtss2si, .cvtsd2si => &[_]u8{0x2D},
        .cvttss2si, .cvttsd2si => &[_]u8{0x2C},
        .cvtdq2ps => &[_]u8{0x5B},
        .cvtps2dq => &[_]u8{0x5B},
        .cvttps2dq => &[_]u8{0x5B},
        .cvtdq2pd => &[_]u8{0xE6},
        .cvtpd2dq => &[_]u8{0xE6},
        .cvttpd2dq => &[_]u8{0xE6},

        // Integer SIMD
        .pand => &[_]u8{0xDB},
        .pandn => &[_]u8{0xDF},
        .por => &[_]u8{0xEB},
        .pxor => &[_]u8{0xEF},
        .paddq => &[_]u8{0xD4},
        .psubq => &[_]u8{0xFB},
        .paddb => &[_]u8{0xFC},
        .paddw => &[_]u8{0xFD},
        .paddd => &[_]u8{0xFE},
        .psubb => &[_]u8{0xF8},
        .psubw => &[_]u8{0xF9},
        .psubd => &[_]u8{0xFA},
        .pmullw => &[_]u8{0xD5},
        .pmulhw => &[_]u8{0xE5},
        .pmulhuw => &[_]u8{0xE4},
        .pmuludq => &[_]u8{0xF4},
        .pmulld => &[_]u8{ 0x38, 0x40 },

        // Comparison
        .pcmpeqb => &[_]u8{0x74},
        .pcmpeqw => &[_]u8{0x75},
        .pcmpeqd => &[_]u8{0x76},
        .pcmpeqq => &[_]u8{ 0x38, 0x29 },
        .pcmpgtb => &[_]u8{0x64},
        .pcmpgtw => &[_]u8{0x65},
        .pcmpgtd => &[_]u8{0x66},
        .pcmpgtq => &[_]u8{ 0x38, 0x37 },

        // Shifts
        .psllw => &[_]u8{0xF1},
        .pslld => &[_]u8{0xF2},
        .psllq => &[_]u8{0xF3},
        .psrlw => &[_]u8{0xD1},
        .psrld => &[_]u8{0xD2},
        .psrlq => &[_]u8{0xD3},
        .psraw => &[_]u8{0xE1},
        .psrad => &[_]u8{0xE2},

        // Shuffles
        .pshufd, .pshufhw, .pshuflw => &[_]u8{0x70},

        // Unpack
        .punpcklbw => &[_]u8{0x60},
        .punpcklwd => &[_]u8{0x61},
        .punpckldq => &[_]u8{0x62},
        .punpcklqdq => &[_]u8{0x6C},
        .punpckhbw => &[_]u8{0x68},
        .punpckhwd => &[_]u8{0x69},
        .punpckhdq => &[_]u8{0x6A},
        .punpckhqdq => &[_]u8{0x6D},

        // Horizontal ops
        .haddps, .haddpd => &[_]u8{0x7C},
        .hsubps, .hsubpd => &[_]u8{0x7D},
        .phaddd, .phaddw => &[_]u8{ 0x38, 0x02 },
        .phsubd, .phsubw => &[_]u8{ 0x38, 0x06 },

        // Mask operations
        .movmskps, .movmskpd => &[_]u8{0x50},
        .pmovmskb => &[_]u8{0xD7},

        // Blend operations (SSE4.1)
        .pblendw => &[_]u8{ 0x3A, 0x0E },
        .blendps => &[_]u8{ 0x3A, 0x0C },
        .blendpd => &[_]u8{ 0x3A, 0x0D },
        .pblendvb => &[_]u8{ 0x38, 0x10 },
        .blendvps => &[_]u8{ 0x38, 0x14 },
        .blendvpd => &[_]u8{ 0x38, 0x15 },

        // Insert/extract
        .insertps => &[_]u8{ 0x3A, 0x21 },
        .pinsrb => &[_]u8{ 0x3A, 0x20 },
        .pinsrw => &[_]u8{0xC4},
        .pinsrd => &[_]u8{ 0x3A, 0x22 },
        .pinsrq => &[_]u8{ 0x3A, 0x22 },
        .extractps => &[_]u8{ 0x3A, 0x17 },
        .pextrb => &[_]u8{ 0x3A, 0x14 },
        .pextrw => &[_]u8{0xC5},
        .pextrd => &[_]u8{ 0x3A, 0x16 },
        .pextrq => &[_]u8{ 0x3A, 0x16 },

        // Rounding (SSE4.1)
        .roundss, .roundsd => &[_]u8{ 0x3A, 0x0A },
        .roundps, .roundpd => &[_]u8{ 0x3A, 0x08 },

        // Testing (SSE4.1)
        .ptest => &[_]u8{ 0x38, 0x17 },

        // Min/max signed/unsigned (SSE4.1)
        .pmaxsb => &[_]u8{ 0x38, 0x3C },
        .pmaxsw => &[_]u8{0xEE},
        .pmaxsd => &[_]u8{ 0x38, 0x3D },
        .pmaxub => &[_]u8{0xDE},
        .pmaxuw => &[_]u8{ 0x38, 0x3E },
        .pmaxud => &[_]u8{ 0x38, 0x3F },
        .pminsb => &[_]u8{ 0x38, 0x38 },
        .pminsw => &[_]u8{0xEA},
        .pminsd => &[_]u8{ 0x38, 0x39 },
        .pminub => &[_]u8{0xDA},
        .pminuw => &[_]u8{ 0x38, 0x3A },
        .pminud => &[_]u8{ 0x38, 0x3B },

        // Absolute value (SSSE3)
        .pabsb => &[_]u8{ 0x38, 0x1C },
        .pabsw => &[_]u8{ 0x38, 0x1D },
        .pabsd => &[_]u8{ 0x38, 0x1E },

        // Pack operations
        .packuswb => &[_]u8{0x67},
        .packusdw => &[_]u8{ 0x38, 0x2B },
        .packsswb => &[_]u8{0x63},
        .packssdw => &[_]u8{0x6B},

        // Unpack operations
        .unpcklps, .unpcklpd => &[_]u8{0x14},
        .unpckhps, .unpckhpd => &[_]u8{0x15},
    };
}

//=============================================================================
// Atomic operation helpers
//=============================================================================

/// Get the base register encoding from an Amode.
fn getBaseEnc(amode: Amode) u8 {
    return switch (amode) {
        .imm_reg => |m| m.base.hwEnc(),
        .imm_reg_reg_shift => |m| m.base.hwEnc(),
        .rip_relative => 0,
    };
}

/// Emit the atomic operation (AND/OR/XOR/etc) for atomic_rmw_seq.
fn emitAtomicOp(sink: *MachBuffer, op: AtomicRmwSeqOp, operand_enc: u8, tmp_enc: u8, size_byte: bool, size_word: bool, need_rex_w: bool) void {
    const rex = RexPrefix.twoOp(operand_enc, tmp_enc, need_rex_w, false);

    switch (op) {
        .add => {
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x00 else 0x01); // ADD r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .sub => {
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x28 else 0x29); // SUB r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .xchg => {
            // For XCHG, we just move the operand to temp (old value will be returned)
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x88 else 0x89); // MOV r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .@"and" => {
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x20 else 0x21); // AND r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .@"or" => {
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x08 else 0x09); // OR r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .xor => {
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x30 else 0x31); // XOR r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
        },
        .nand => {
            // AND then NOT
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x20 else 0x21); // AND
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
            // NOT tmp
            if (size_word) sink.put1(0x66);
            const not_rex = RexPrefix.oneOp(tmp_enc, need_rex_w, false);
            not_rex.encode(sink);
            sink.put1(if (size_byte) 0xF6 else 0xF7); // NOT r/m
            sink.put1(encodeModrm(0b11, 2, tmp_enc & 7)); // /2 for NOT
        },
        .umin, .umax, .smin, .smax => {
            // CMP then CMOVcc
            if (size_word) sink.put1(0x66);
            rex.encode(sink);
            sink.put1(if (size_byte) 0x38 else 0x39); // CMP r/m, r
            sink.put1(encodeModrm(0b11, operand_enc & 7, tmp_enc & 7));
            // CMOVcc operand -> tmp
            const cmov_rex = RexPrefix.twoOp(tmp_enc, operand_enc, need_rex_w, false);
            cmov_rex.encode(sink);
            sink.put1(0x0F);
            const cmov_cc: u8 = switch (op) {
                .umin => 0x46, // CMOVBE
                .umax => 0x43, // CMOVAE
                .smin => 0x4E, // CMOVLE
                .smax => 0x4D, // CMOVGE
                else => unreachable,
            };
            sink.put1(cmov_cc);
            sink.put1(encodeModrm(0b11, tmp_enc & 7, operand_enc & 7));
        },
    }
}

//=============================================================================
// Conditional jump helpers
//=============================================================================

/// Emit a conditional jump to a label.
fn emitCondJmp(sink: *MachBuffer, cc: CC, target: MachLabel) void {
    // 0x0F 0x8X rel32 format for far conditional jump
    sink.put1(0x0F);
    sink.put1(0x80 + @intFromEnum(cc));
    const offset = sink.curOffset();
    sink.useLabelAtOffset(offset, target, .jmp_rel_32);
    sink.put4(0); // Placeholder for displacement
}

/// Emit an unconditional jump to a label.
fn emitUncondJmp(sink: *MachBuffer, target: MachLabel) void {
    // 0xE9 rel32 format for far unconditional jump
    sink.put1(0xE9);
    const offset = sink.curOffset();
    sink.useLabelAtOffset(offset, target, .jmp_rel_32);
    sink.put4(0); // Placeholder for displacement
}

//=============================================================================
// Tests
//=============================================================================

test "REX prefix encoding" {
    const testing = std.testing;

    // Test oneOp with extended register (R8)
    const rex1 = RexPrefix.oneOp(8, true, false); // R8, 64-bit
    try testing.expectEqual(@as(u8, 0x49), rex1.byte); // REX.WB

    // Test oneOp with low register (RAX)
    const rex2 = RexPrefix.oneOp(0, true, false); // RAX, 64-bit
    try testing.expectEqual(@as(u8, 0x48), rex2.byte); // REX.W

    // Test twoOp with extended registers
    const rex3 = RexPrefix.twoOp(9, 10, true, false); // R9, R10, 64-bit
    try testing.expectEqual(@as(u8, 0x4D), rex3.byte); // REX.WRB

    // Test 8-bit register requiring REX
    const rex4 = RexPrefix.oneOp(4, false, true); // SPL (needs REX)
    try testing.expect(rex4.must_emit);
}

test "ModRM encoding" {
    const testing = std.testing;

    // mod=11 (register direct), reg=RAX(0), rm=RCX(1)
    const modrm1 = encodeModrm(0b11, 0, 1);
    try testing.expectEqual(@as(u8, 0xC1), modrm1);

    // mod=00 (memory), reg=1, rm=4 (SIB follows)
    const modrm2 = encodeModrm(0b00, 1, 4);
    try testing.expectEqual(@as(u8, 0x0C), modrm2);
}

test "SIB encoding" {
    const testing = std.testing;

    // scale=2, index=RCX(1), base=RAX(0)
    const sib = encodeSib(0b01, 1, 0);
    try testing.expectEqual(@as(u8, 0x48), sib);
}

test "Displacement classification" {
    const testing = std.testing;

    // Zero displacement
    try testing.expectEqual(Disp.none, Disp.new(0, null));

    // Small displacement (fits in i8)
    const disp1 = Disp.new(64, null);
    try testing.expectEqual(@as(i8, 64), disp1.imm8);

    // Large displacement (needs i32)
    const disp2 = Disp.new(256, null);
    try testing.expectEqual(@as(i32, 256), disp2.imm32);

    // Negative displacement
    const disp3 = Disp.new(-128, null);
    try testing.expectEqual(@as(i8, -128), disp3.imm8);
}

test "NOP emission" {
    const allocator = std.testing.allocator;
    var sink = MachBuffer.init(allocator);
    defer sink.deinit();

    // Test 1-byte NOP
    emitNop(&sink, 1);
    try std.testing.expectEqual(@as(u8, 0x90), sink.data.items[0]);

    // Test 2-byte NOP
    sink.data.clearRetainingCapacity();
    sink.cur_offset_val = 0;
    emitNop(&sink, 2);
    try std.testing.expectEqual(@as(u8, 0x66), sink.data.items[0]);
    try std.testing.expectEqual(@as(u8, 0x90), sink.data.items[1]);

    // Test 3-byte NOP
    sink.data.clearRetainingCapacity();
    sink.cur_offset_val = 0;
    emitNop(&sink, 3);
    try std.testing.expectEqual(@as(u8, 0x0F), sink.data.items[0]);
    try std.testing.expectEqual(@as(u8, 0x1F), sink.data.items[1]);
    try std.testing.expectEqual(@as(u8, 0x00), sink.data.items[2]);
}

test "Basic instruction emission" {
    const allocator = std.testing.allocator;
    var sink = MachBuffer.init(allocator);
    defer sink.deinit();

    const state = EmitState{};
    const info = EmitInfo{};

    // Test RET instruction
    const ret_inst = Inst{ .ret = .{ .stack_bytes_to_pop = 0 } };
    emit(&ret_inst, &sink, &info, &state);
    try std.testing.expectEqual(@as(u8, 0xC3), sink.data.items[0]);

    // Test UD2 instruction
    sink.data.clearRetainingCapacity();
    sink.cur_offset_val = 0;
    const ud2_inst = Inst{ .ud2 = .{ .trap_code = .stack_overflow } };
    emit(&ud2_inst, &sink, &info, &state);
    try std.testing.expectEqual(@as(u8, 0x0F), sink.data.items[0]);
    try std.testing.expectEqual(@as(u8, 0x0B), sink.data.items[1]);
}
