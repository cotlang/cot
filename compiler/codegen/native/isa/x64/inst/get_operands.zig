//! x86-64 ISA: Register operand collection for register allocation.
//!
//! This module implements `x64GetOperands()` which collects register
//! uses and definitions for each instruction type. This is needed by the
//! register allocator to understand register liveness.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/inst/mod.rs`

const std = @import("std");
const mod = @import("mod.zig");
const args = @import("args.zig");
const regs = @import("regs.zig");

const Inst = mod.Inst;
const SyntheticAmode = args.SyntheticAmode;
const Amode = args.Amode;
const RegMem = args.RegMem;
const RegMemImm = args.RegMemImm;
const Reg = args.Reg;
const Gpr = args.Gpr;
const Xmm = args.Xmm;
const Writable = args.Writable;
const PReg = args.PReg;
const GprMem = args.GprMem;
const GprMemImm = args.GprMemImm;
const XmmMem = args.XmmMem;

/// Operand visitor interface for register allocation.
/// Implementations collect register uses and definitions.
pub const OperandVisitor = struct {
    /// Registers used (read from) by this instruction.
    uses: std.ArrayListUnmanaged(Reg),
    /// Registers defined (written to) by this instruction.
    defs: std.ArrayListUnmanaged(Writable(Reg)),
    /// Fixed register constraints (physical registers that must be used).
    fixed_uses: std.ArrayListUnmanaged(struct { vreg: Reg, preg: PReg }),
    fixed_defs: std.ArrayListUnmanaged(struct { vreg: Writable(Reg), preg: PReg }),
    /// Clobbered registers (modified but not as output).
    clobbers: std.ArrayListUnmanaged(PReg),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) OperandVisitor {
        return .{
            .uses = .{},
            .defs = .{},
            .fixed_uses = .{},
            .fixed_defs = .{},
            .clobbers = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *OperandVisitor) void {
        self.uses.deinit(self.allocator);
        self.defs.deinit(self.allocator);
        self.fixed_uses.deinit(self.allocator);
        self.fixed_defs.deinit(self.allocator);
        self.clobbers.deinit(self.allocator);
    }

    /// Mark a register as used (read).
    pub fn regUse(self: *OperandVisitor, reg: Reg) void {
        self.uses.append(self.allocator, reg) catch unreachable;
    }

    /// Mark a register as defined (written).
    pub fn regDef(self: *OperandVisitor, reg: Writable(Reg)) void {
        self.defs.append(self.allocator, reg) catch unreachable;
    }

    /// Mark a register as both used and redefined (read-modify-write).
    pub fn regReuseDef(self: *OperandVisitor, reg: Writable(Reg), _: usize) void {
        // The operand index is for regalloc2's constraint system; we just track the reg.
        self.uses.append(self.allocator, reg.toReg()) catch unreachable;
        self.defs.append(self.allocator, reg) catch unreachable;
    }

    /// Mark a fixed physical register as used.
    pub fn regFixedUse(self: *OperandVisitor, vreg: Reg, preg: PReg) void {
        self.fixed_uses.append(self.allocator, .{ .vreg = vreg, .preg = preg }) catch unreachable;
    }

    /// Mark a fixed physical register as defined.
    pub fn regFixedDef(self: *OperandVisitor, vreg: Writable(Reg), preg: PReg) void {
        self.fixed_defs.append(self.allocator, .{ .vreg = vreg, .preg = preg }) catch unreachable;
    }

    /// Mark a physical register as non-allocatable (e.g., stack pointer).
    pub fn regFixedNonallocatable(self: *OperandVisitor, preg: PReg) void {
        // Non-allocatable registers are tracked but not assigned by regalloc.
        _ = self;
        _ = preg;
    }

    /// Mark a register as clobbered (modified without being an output).
    pub fn regClobber(self: *OperandVisitor, preg: PReg) void {
        self.clobbers.append(self.allocator, preg) catch unreachable;
    }

    // =========================================================================
    // Helper methods for GPR/XMM types
    // =========================================================================

    /// Mark a GPR as used.
    pub fn gprUse(self: *OperandVisitor, gpr: Gpr) void {
        self.regUse(gpr.toReg());
    }

    /// Mark a GPR as defined.
    pub fn gprDef(self: *OperandVisitor, wgpr: args.WritableGpr) void {
        self.regDef(Writable(Reg).fromReg(wgpr.toReg().toReg()));
    }

    /// Mark a GPR as reused (read-modify-write).
    pub fn gprReuseDef(self: *OperandVisitor, wgpr: args.WritableGpr, idx: usize) void {
        self.regReuseDef(Writable(Reg).fromReg(wgpr.toReg().toReg()), idx);
    }

    /// Mark an XMM as used.
    pub fn xmmUse(self: *OperandVisitor, xmm: Xmm) void {
        self.regUse(xmm.toReg());
    }

    /// Mark an XMM as defined.
    pub fn xmmDef(self: *OperandVisitor, wxmm: args.WritableXmm) void {
        self.regDef(Writable(Reg).fromReg(wxmm.toReg().toReg()));
    }

    /// Mark an XMM as reused (read-modify-write).
    pub fn xmmReuseDef(self: *OperandVisitor, wxmm: args.WritableXmm, idx: usize) void {
        self.regReuseDef(Writable(Reg).fromReg(wxmm.toReg().toReg()), idx);
    }
};

/// Collect register operands from a SyntheticAmode.
fn syntheticAmodeOperands(amode: SyntheticAmode, collector: *OperandVisitor) void {
    switch (amode) {
        .real => |a| amodeOperands(a, collector),
        .incoming_arg, .slot_offset, .constant_offset => {
            // These use RSP/RBP implicitly, not tracked as operands.
        },
    }
}

/// Collect register operands from an Amode.
fn amodeOperands(amode: Amode, collector: *OperandVisitor) void {
    switch (amode) {
        .imm_reg => |m| {
            // Don't track RSP/RBP as they're fixed
            if (m.base.hwEnc() != regs.GprEnc.RSP and m.base.hwEnc() != regs.GprEnc.RBP) {
                collector.regUse(m.base);
            }
        },
        .imm_reg_reg_shift => |m| {
            collector.gprUse(m.base);
            collector.gprUse(m.index);
        },
        .rip_relative => {
            // RIP isn't involved in regalloc.
        },
    }
}

/// Collect register operands from a RegMem.
fn regMemOperands(rm: RegMem, collector: *OperandVisitor) void {
    switch (rm) {
        .reg => |r| collector.regUse(r),
        .mem => |amode| syntheticAmodeOperands(amode, collector),
    }
}

/// Collect register operands from a RegMemImm.
fn regMemImmOperands(rmi: RegMemImm, collector: *OperandVisitor) void {
    switch (rmi) {
        .reg => |r| collector.regUse(r),
        .mem => |amode| syntheticAmodeOperands(amode, collector),
        .imm => {},
    }
}

/// Collect register operands from a GprMem.
fn gprMemOperands(gm: GprMem, collector: *OperandVisitor) void {
    regMemOperands(gm.inner, collector);
}

/// Collect register operands from a GprMemImm.
fn gprMemImmOperands(gmi: GprMemImm, collector: *OperandVisitor) void {
    regMemImmOperands(gmi.inner, collector);
}

/// Collect register operands from an XmmMem.
fn xmmMemOperands(xm: XmmMem, collector: *OperandVisitor) void {
    regMemOperands(xm.inner, collector);
}

/// Collect all register operands for an instruction.
/// This is used by the register allocator to determine liveness.
pub fn getOperands(inst: *const Inst, collector: *OperandVisitor) void {
    switch (inst.*) {
        //=====================================================================
        // NOP - no operands
        //=====================================================================
        .nop => {},

        //=====================================================================
        // ALU operations
        //=====================================================================
        .alu_rmi_r => |p| {
            collector.gprReuseDef(p.dst, 0);
            gprMemImmOperands(p.src, collector);
        },

        //=====================================================================
        // Shifts
        //=====================================================================
        .shift_r => |p| {
            collector.gprReuseDef(p.dst, 0);
            switch (p.shift_by) {
                .cl => {
                    // CL register is implicitly used
                    collector.regFixedUse(regs.rcx(), regs.gprPreg(regs.GprEnc.RCX));
                },
                .imm => {},
            }
        },

        //=====================================================================
        // Unary operations
        //=====================================================================
        .unary_rm_r => |p| {
            collector.gprDef(p.dst);
            gprMemOperands(p.src, collector);
        },

        //=====================================================================
        // Multiplication
        //=====================================================================
        .mul => |p| {
            // MUL/IMUL implicitly use RAX and define RAX:RDX
            collector.regFixedUse(regs.rax(), regs.gprPreg(regs.GprEnc.RAX));
            collector.regFixedDef(Writable(Reg).fromReg(regs.rax()), regs.gprPreg(regs.GprEnc.RAX));
            collector.regFixedDef(Writable(Reg).fromReg(regs.rdx()), regs.gprPreg(regs.GprEnc.RDX));
            gprMemOperands(p.src, collector);
        },

        //=====================================================================
        // Division
        //=====================================================================
        .div => |p| {
            // DIV/IDIV: uses dividend_lo, dividend_hi, divisor; defines quotient, remainder
            collector.gprUse(p.dividend_lo);
            collector.gprUse(p.dividend_hi);
            gprMemOperands(p.divisor, collector);
            collector.gprDef(p.dst_quotient);
            collector.gprDef(p.dst_remainder);
        },

        //=====================================================================
        // Sign extension for division
        //=====================================================================
        .sign_extend_data => |p| {
            // CDQ/CQO: sign-extend src into dst
            collector.gprUse(p.src);
            collector.gprDef(p.dst);
        },

        //=====================================================================
        // MOV operations
        //=====================================================================
        .mov_r_r => |p| {
            collector.gprDef(p.dst);
            collector.gprUse(p.src);
        },
        .mov_r_m => |p| {
            syntheticAmodeOperands(p.dst, collector);
            collector.gprUse(p.src);
        },
        .mov_m_r => |p| {
            collector.gprDef(p.dst);
            syntheticAmodeOperands(p.src, collector);
        },
        .imm => |p| {
            collector.gprDef(p.dst);
        },
        .mov_from_preg => |p| {
            collector.gprDef(p.dst);
            collector.regFixedNonallocatable(p.src);
        },
        .mov_to_preg => |p| {
            collector.regFixedNonallocatable(p.dst);
            collector.gprUse(p.src);
        },

        //=====================================================================
        // Zero/Sign extension moves
        //=====================================================================
        .movzx_rm_r => |p| {
            collector.gprDef(p.dst);
            gprMemOperands(p.src, collector);
        },
        .movsx_rm_r => |p| {
            collector.gprDef(p.dst);
            gprMemOperands(p.src, collector);
        },

        //=====================================================================
        // LEA
        //=====================================================================
        .lea => |p| {
            collector.gprDef(p.dst);
            syntheticAmodeOperands(p.src, collector);
        },

        //=====================================================================
        // CMP and TEST
        //=====================================================================
        .cmp_rmi_r => |p| {
            collector.gprUse(p.dst);
            gprMemImmOperands(p.src, collector);
        },
        .test_rmi_r => |p| {
            collector.gprUse(p.dst);
            gprMemImmOperands(p.src, collector);
        },

        //=====================================================================
        // CMOVcc
        //=====================================================================
        .cmove => |p| {
            collector.gprReuseDef(p.dst, 0);
            gprMemOperands(p.src, collector);
        },

        //=====================================================================
        // SETcc
        //=====================================================================
        .setcc => |p| {
            collector.gprDef(p.dst);
        },

        //=====================================================================
        // Jumps and branches
        //=====================================================================
        .jmp_known => {},
        .jmp_unknown => |p| {
            regMemOperands(p.target, collector);
        },
        .jmp_cond => {},

        //=====================================================================
        // Calls
        //=====================================================================
        .call_known => |p| {
            // Arguments are uses
            for (p.info.uses.items) |arg| {
                collector.regFixedUse(arg.vreg, arg.preg);
            }
            // Return values are defs
            for (p.info.defs.items) |ret| {
                collector.regFixedDef(ret.vreg, ret.location.reg);
            }
        },
        .call_unknown => |p| {
            regMemOperands(p.info.dest, collector);
            for (p.info.uses.items) |arg| {
                collector.regFixedUse(arg.vreg, arg.preg);
            }
            for (p.info.defs.items) |ret| {
                collector.regFixedDef(ret.vreg, ret.location.reg);
            }
        },

        //=====================================================================
        // Return
        //=====================================================================
        .ret => {},

        //=====================================================================
        // Return calls (tail calls)
        //=====================================================================
        .return_call_known => |p| {
            for (p.info.uses.items) |arg| {
                collector.regFixedUse(arg.vreg, arg.preg);
            }
        },
        .return_call_unknown => |p| {
            collector.regUse(p.info.dest);
            for (p.info.uses.items) |arg| {
                collector.regFixedUse(arg.vreg, arg.preg);
            }
        },

        //=====================================================================
        // Conditional jumps (or variant)
        //=====================================================================
        .jmp_cond_or => {},
        .winch_jmp_if => {},
        .jmp_table_seq => |p| {
            collector.regUse(p.idx);
            collector.regDef(p.tmp1);
            collector.regDef(p.tmp2);
        },

        //=====================================================================
        // Traps
        //=====================================================================
        .trap_if => {},
        .trap_if_and => {},
        .trap_if_or => {},
        .ud2 => {},

        //=====================================================================
        // Fences
        //=====================================================================
        .fence => {},

        //=====================================================================
        // External name load
        //=====================================================================
        .load_ext_name => |p| {
            collector.gprDef(p.dst);
        },

        //=====================================================================
        // ABI pseudo-instructions
        //=====================================================================
        .args => |p| {
            for (p.args_list) |arg| {
                collector.regFixedDef(Writable(Reg).fromReg(arg.vreg), arg.preg);
            }
        },
        .rets => |p| {
            for (p.rets_list) |ret| {
                collector.regFixedUse(ret.vreg, ret.preg);
            }
        },

        //=====================================================================
        // SSE/XMM operations
        //=====================================================================
        .xmm_rm_r => |p| {
            collector.xmmReuseDef(p.dst, 0);
            xmmMemOperands(p.src, collector);
        },
        .xmm_rm_r_evex => |p| {
            collector.xmmDef(p.dst);
            collector.xmmUse(p.src1);
            xmmMemOperands(p.src2, collector);
        },
        .xmm_unary_rm_r => |p| {
            collector.xmmDef(p.dst);
            xmmMemOperands(p.src, collector);
        },
        .xmm_mov_m_r => |p| {
            collector.xmmDef(p.dst);
            syntheticAmodeOperands(p.src, collector);
        },
        .xmm_mov_r_m => |p| {
            syntheticAmodeOperands(p.dst, collector);
            collector.xmmUse(p.src);
        },
        .xmm_cmp_rm_r => |p| {
            collector.xmmUse(p.dst);
            xmmMemOperands(p.src, collector);
        },
        .xmm_to_gpr => |p| {
            collector.gprDef(p.dst);
            collector.xmmUse(p.src);
        },
        .gpr_to_xmm => |p| {
            collector.xmmDef(p.dst);
            gprMemOperands(p.src, collector);
        },
        .xmm_cmp_imm => |p| {
            collector.xmmReuseDef(p.dst, 0);
            xmmMemOperands(p.src, collector);
        },
        .xmm_round => |p| {
            collector.xmmDef(p.dst);
            xmmMemOperands(p.src, collector);
        },

        //=====================================================================
        // Atomic operations
        //=====================================================================
        .atomic_rmw_seq => |p| {
            syntheticAmodeOperands(p.mem, collector);
            collector.gprUse(p.operand);
            collector.gprDef(p.dst_old);
            collector.gprDef(p.tmp);
        },
        .atomic_128_rmw_seq => |p| {
            syntheticAmodeOperands(p.mem, collector);
            collector.gprUse(p.operand_low);
            collector.gprUse(p.operand_high);
            collector.gprDef(p.temp_low);
            collector.gprDef(p.temp_high);
            collector.gprDef(p.dst_old_low);
            collector.gprDef(p.dst_old_high);
        },
        .atomic_128_xchg_seq => |p| {
            syntheticAmodeOperands(p.mem, collector);
            collector.gprUse(p.operand_low);
            collector.gprUse(p.operand_high);
            collector.gprDef(p.dst_old_low);
            collector.gprDef(p.dst_old_high);
        },

        //=====================================================================
        // TLS operations
        //=====================================================================
        .elf_tls_get_addr => |p| {
            collector.gprDef(p.dst);
        },
        .macho_tls_get_addr => |p| {
            collector.gprDef(p.dst);
        },
        .coff_tls_get_addr => |p| {
            collector.gprDef(p.dst);
            collector.gprDef(p.tmp);
        },

        //=====================================================================
        // Pseudo-instructions
        //=====================================================================
        .checked_srem_seq => |p| {
            collector.gprUse(p.divisor);
            collector.gprUse(p.dividend_lo);
            collector.gprUse(p.dividend_hi);
            collector.gprDef(p.dst_quotient);
            collector.gprDef(p.dst_remainder);
        },
        .checked_srem_seq8 => |p| {
            collector.gprUse(p.divisor);
            collector.gprUse(p.dividend);
            collector.gprDef(p.dst);
        },
        .xmm_uninitialized_value => |p| {
            collector.xmmDef(p.dst);
        },
        .gpr_uninitialized_value => |p| {
            collector.gprDef(p.dst);
        },
        .xmm_min_max_seq => |p| {
            collector.xmmUse(p.lhs);
            collector.xmmUse(p.rhs);
            collector.xmmDef(p.dst);
        },
        .cvt_uint64_to_float_seq => |p| {
            collector.gprUse(p.src);
            collector.xmmDef(p.dst);
            collector.gprDef(p.tmp_gpr1);
            collector.gprDef(p.tmp_gpr2);
        },
        .cvt_float_to_sint_seq => |p| {
            collector.xmmUse(p.src);
            collector.gprDef(p.dst);
            collector.xmmDef(p.tmp_xmm);
            collector.gprDef(p.tmp_gpr);
        },
        .cvt_float_to_uint_seq => |p| {
            collector.xmmUse(p.src);
            collector.gprDef(p.dst);
            collector.xmmDef(p.tmp_xmm);
            collector.xmmDef(p.tmp_xmm2);
            collector.gprDef(p.tmp_gpr);
        },
        .xmm_cmove => |p| {
            collector.xmmUse(p.consequent);
            collector.xmmUse(p.alternative);
            collector.xmmDef(p.dst);
        },
        .stack_probe_loop => |p| {
            collector.gprDef(p.tmp);
        },

        //=====================================================================
        // Miscellaneous
        //=====================================================================
        .stack_switch_basic => |p| {
            collector.gprUse(p.store_context_ptr);
            collector.gprUse(p.load_context_ptr);
            collector.gprUse(p.in_payload0);
            collector.gprDef(p.out_payload0);
        },
        .unwind => {},
        .dummy_use => |p| {
            collector.regUse(p.reg);
        },
        .label_address => |p| {
            collector.gprDef(p.dst);
        },
        .sequence_point => {},

        //=====================================================================
        // Fallback for any unhandled instruction types
        //=====================================================================
    }
}

// =============================================================================
// Tests
// =============================================================================

test "OperandVisitor basic usage" {
    const testing = std.testing;
    var visitor = OperandVisitor.init(testing.allocator);
    defer visitor.deinit();

    const x0 = regs.rax();
    const x1 = regs.rcx();

    visitor.regUse(x0);
    visitor.regUse(x1);
    visitor.regDef(Writable(Reg).fromReg(x0));

    try testing.expectEqual(@as(usize, 2), visitor.uses.items.len);
    try testing.expectEqual(@as(usize, 1), visitor.defs.items.len);
}

test "getOperands alu_rmi_r" {
    const testing = std.testing;
    var visitor = OperandVisitor.init(testing.allocator);
    defer visitor.deinit();

    const rax_reg = regs.rax();
    const rcx_reg = regs.rcx();

    const gpr_rax = args.Gpr.new(rax_reg) orelse unreachable;

    const inst_val = Inst{ .alu_rmi_r = .{
        .size = .size64,
        .op = .add,
        .src = args.GprMemImm{ .inner = .{ .reg = rcx_reg } },
        .dst = args.WritableGpr.fromReg(gpr_rax),
    } };

    getOperands(&inst_val, &visitor);

    // ALU uses src and reuses dst (use + def)
    try testing.expectEqual(@as(usize, 2), visitor.uses.items.len); // dst reuse + src
    try testing.expectEqual(@as(usize, 1), visitor.defs.items.len); // dst def
}

test "getOperands div" {
    const testing = std.testing;
    var visitor = OperandVisitor.init(testing.allocator);
    defer visitor.deinit();

    const rax_reg = regs.rax();
    const rdx_reg = regs.rdx();
    const rbx_reg = regs.rbx();

    const gpr_rax = args.Gpr.new(rax_reg) orelse unreachable;
    const gpr_rdx = args.Gpr.new(rdx_reg) orelse unreachable;

    const inst_val = Inst{ .div = .{
        .size = .size64,
        .signed = false,
        .divisor = args.GprMem{ .inner = .{ .reg = rbx_reg } },
        .dividend_lo = gpr_rax,
        .dividend_hi = gpr_rdx,
        .dst_quotient = args.WritableGpr.fromReg(gpr_rax),
        .dst_remainder = args.WritableGpr.fromReg(gpr_rdx),
        .trap_code = .integer_division_by_zero,
    } };

    getOperands(&inst_val, &visitor);

    // DIV uses dividend_lo, dividend_hi, divisor; defines quotient, remainder
    try testing.expectEqual(@as(usize, 3), visitor.uses.items.len); // dividend_lo, dividend_hi, divisor
    try testing.expectEqual(@as(usize, 2), visitor.defs.items.len); // dst_quotient, dst_remainder
}
