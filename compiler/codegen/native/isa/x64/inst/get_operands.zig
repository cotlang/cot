//! x86-64 ISA: Register operand collection for register allocation.
//!
//! This module implements `getOperands()` which visits register operands
//! for each instruction type. It supports two modes:
//!
//! 1. **Collection mode**: Collects register uses/defs for regalloc input
//! 2. **Callback mode**: Applies allocations during emit by mutating registers
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/inst/mod.rs`
//! and `cranelift/codegen/src/machinst/reg.rs:378-552`

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

//=============================================================================
// Operand metadata
//=============================================================================

/// Operand constraint type.
/// Reference: cranelift/codegen/src/machinst/reg.rs:OperandConstraint
pub const OperandConstraint = enum {
    /// Any register of the appropriate class.
    any,
    /// A specific fixed physical register.
    fixed_reg,
    /// Reuse an input register for the output.
    reuse,
};

/// Operand kind: use (read) or def (write).
/// Reference: cranelift/codegen/src/machinst/reg.rs:OperandKind
pub const OperandKind = enum {
    use,
    def,
};

/// Operand position: early (at instruction start) or late (at instruction end).
/// Reference: cranelift/codegen/src/machinst/reg.rs:OperandPos
pub const OperandPos = enum {
    early,
    late,
};

//=============================================================================
// OperandVisitor - Unified visitor supporting collection and callback modes
//=============================================================================

/// Callback function type for applying allocations.
/// Reference: cranelift/codegen/src/machinst/reg.rs:542
pub const OperandCallbackFn = *const fn (
    ctx: *anyopaque,
    reg: *Reg,
    constraint: OperandConstraint,
    kind: OperandKind,
    pos: OperandPos,
) void;

/// Operand visitor that supports two modes:
/// 1. Collector mode: Appends operands to lists for regalloc
/// 2. Callback mode: Calls a function for each operand (for applying allocations)
///
/// Reference: cranelift/codegen/src/machinst/reg.rs:378-552
pub const OperandVisitor = union(enum) {
    /// Collection mode: gather operands for regalloc.
    collector: *CollectorState,
    /// Callback mode: apply allocations during emit.
    callback: struct {
        ctx: *anyopaque,
        func: OperandCallbackFn,
    },

    /// Collection state for regalloc input.
    /// Port of Cranelift's single flat operands Vec (reg.rs:343).
    /// All operands are stored in SOURCE ORDER to match callback visit order.
    pub const CollectorState = struct {
        /// Flat list of operands in source order (matching callback visit order).
        operands: std.ArrayListUnmanaged(CollectedOperand),
        /// Clobbered registers (separate, not regalloc operands).
        clobbers: std.ArrayListUnmanaged(PReg),
        allocator: std.mem.Allocator,

        /// A single collected operand entry, preserving source order.
        pub const CollectedOperand = struct {
            reg: Reg,
            preg: ?PReg, // non-null for fixed register constraints
            kind: OperandKind,
            pos: OperandPos,
        };

        pub fn init(allocator: std.mem.Allocator) CollectorState {
            return .{
                .operands = .{},
                .clobbers = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *CollectorState) void {
            self.operands.deinit(self.allocator);
            self.clobbers.deinit(self.allocator);
        }

        pub fn clear(self: *CollectorState) void {
            self.operands.clearRetainingCapacity();
            self.clobbers.clearRetainingCapacity();
        }
    };

    /// Create a collector-mode visitor.
    pub fn initCollector(state: *CollectorState) OperandVisitor {
        return .{ .collector = state };
    }

    /// Create a callback-mode visitor.
    pub fn initCallback(ctx: *anyopaque, func: OperandCallbackFn) OperandVisitor {
        return .{ .callback = .{ .ctx = ctx, .func = func } };
    }

    //=========================================================================
    // Core operand methods - match Cranelift's OperandVisitorImpl trait
    // Reference: cranelift/codegen/src/machinst/reg.rs:395-512
    //=========================================================================

    /// Mark a register as used (read), at early position.
    /// Reference: reg.rs:405 fn reg_use
    pub fn regUse(self: *OperandVisitor, reg: *Reg) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = reg.*, .preg = null, .kind = .use, .pos = .early,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, reg, .any, .use, .early),
        }
    }

    /// Mark a register as defined (written), at late position.
    /// Reference: reg.rs:417 fn reg_def
    pub fn regDef(self: *OperandVisitor, reg: *Writable(Reg)) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = reg.toReg(), .preg = null, .kind = .def, .pos = .late,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, reg.regMut(), .any, .def, .late),
        }
    }

    /// Mark a register as defined (written), at early position.
    /// Use this when the def may be written before all uses are read;
    /// the regalloc will ensure that it does not overwrite any uses.
    /// Reference: cranelift/codegen/src/machinst/reg.rs:425 fn reg_early_def
    pub fn regEarlyDef(self: *OperandVisitor, reg: *Writable(Reg)) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = reg.toReg(), .preg = null, .kind = .def, .pos = .early,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, reg.regMut(), .any, .def, .early),
        }
    }

    /// Mark a register as both used and redefined (read-modify-write).
    /// Reference: reg.rs:445 fn reg_reuse_def
    pub fn regReuseDef(self: *OperandVisitor, reg: *Writable(Reg), _: usize) void {
        switch (self.*) {
            .collector => |c| {
                // Append def then use in source order, matching callback order.
                // Both entries refer to the same register (read-modify-write pattern).
                c.operands.append(c.allocator, .{
                    .reg = reg.toReg(), .preg = null, .kind = .def, .pos = .late,
                }) catch unreachable;
                c.operands.append(c.allocator, .{
                    .reg = reg.toReg(), .preg = null, .kind = .use, .pos = .early,
                }) catch unreachable;
            },
            .callback => |cb| {
                // Both refer to the same register (reuse pattern).
                // Order: def first, then use (matching collector order).
                cb.func(cb.ctx, reg.regMut(), .reuse, .def, .late);
                cb.func(cb.ctx, reg.regMut(), .reuse, .use, .early);
            },
        }
    }

    /// Mark a fixed physical register as used.
    /// Fixed registers are not mutated during emit - they stay as the fixed preg.
    /// Reference: reg.rs:468 fn reg_fixed_use
    pub fn regFixedUse(self: *OperandVisitor, vreg: Reg, preg: PReg) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = vreg, .preg = preg, .kind = .use, .pos = .early,
            }) catch unreachable,
            .callback => |cb| {
                // Call callback to consume allocation and keep allocation index in sync.
                var reg_copy = vreg;
                cb.func(cb.ctx, &reg_copy, .fixed_reg, .use, .early);
            },
        }
    }

    /// Mark a fixed physical register as defined.
    /// Fixed registers are not mutated during emit - they stay as the fixed preg.
    /// Reference: reg.rs:476 fn reg_fixed_def
    pub fn regFixedDef(self: *OperandVisitor, vreg: Writable(Reg), preg: PReg) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = vreg.toReg(), .preg = preg, .kind = .def, .pos = .late,
            }) catch unreachable,
            .callback => |cb| {
                // Call callback to consume allocation and keep allocation index in sync.
                var reg_copy = vreg.toReg();
                cb.func(cb.ctx, &reg_copy, .fixed_reg, .def, .late);
            },
        }
    }

    /// Mark a physical register as non-allocatable (e.g., stack pointer).
    /// Reference: reg.rs:397 fn reg_fixed_nonallocatable
    pub fn regFixedNonallocatable(_: *OperandVisitor, _: PReg) void {
        // Non-allocatable registers don't participate in allocation.
    }

    /// Mark registers as clobbered.
    /// Reference: reg.rs:389 fn reg_clobbers
    pub fn regClobbers(self: *OperandVisitor, preg: PReg) void {
        switch (self.*) {
            .collector => |c| c.clobbers.append(c.allocator, preg) catch unreachable,
            .callback => {},
        }
    }

    /// Add all clobbered registers from a PRegSet.
    /// Port of Cranelift's call instruction clobber handling.
    /// This is CRITICAL for correct code generation - without it, values in
    /// caller-saved registers are not preserved across function calls.
    pub fn addClobbers(self: *OperandVisitor, clobbers: mod.PRegSet) void {
        switch (self.*) {
            .collector => |c| {
                // Iterate through all GPRs (RAX=0 through R15=15)
                var i: u8 = 0;
                while (i < 16) : (i += 1) {
                    const preg = regs.gprPreg(i);
                    if (clobbers.contains(preg)) {
                        c.clobbers.append(c.allocator, preg) catch unreachable;
                    }
                }
                // Iterate through all XMM registers (XMM0 through XMM15)
                i = 0;
                while (i < 16) : (i += 1) {
                    const preg = regs.fprPreg(i);
                    if (clobbers.contains(preg)) {
                        c.clobbers.append(c.allocator, preg) catch unreachable;
                    }
                }
            },
            .callback => {},
        }
    }

    //=========================================================================
    // Helper methods for GPR/XMM types
    // These handle the nested type structure: WritableGpr = Writable(Gpr),
    // where Gpr wraps Reg. So WritableGpr.reg is Gpr, and Gpr.reg is Reg.
    //=========================================================================

    /// Mark a GPR as used.
    pub fn gprUse(self: *OperandVisitor, gpr: *Gpr) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = gpr.toReg(), .preg = null, .kind = .use, .pos = .early,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, gpr.regMut(), .any, .use, .early),
        }
    }

    /// Mark a GPR as defined.
    pub fn gprDef(self: *OperandVisitor, wgpr: *args.WritableGpr) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = wgpr.toReg().toReg(), .preg = null, .kind = .def, .pos = .late,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, wgpr.regMut().regMut(), .any, .def, .late),
        }
    }

    /// Mark a GPR as defined at early position.
    /// Use when the def may be written before all uses are read;
    /// the regalloc will ensure it does not overwrite any uses.
    pub fn gprEarlyDef(self: *OperandVisitor, wgpr: *args.WritableGpr) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = wgpr.toReg().toReg(), .preg = null, .kind = .def, .pos = .early,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, wgpr.regMut().regMut(), .any, .def, .early),
        }
    }

    /// Mark a GPR as reused (read-modify-write).
    pub fn gprReuseDef(self: *OperandVisitor, wgpr: *args.WritableGpr, _: usize) void {
        switch (self.*) {
            .collector => |c| {
                const reg = wgpr.toReg().toReg();
                // Append def then use in source order, matching callback order.
                c.operands.append(c.allocator, .{
                    .reg = reg, .preg = null, .kind = .def, .pos = .late,
                }) catch unreachable;
                c.operands.append(c.allocator, .{
                    .reg = reg, .preg = null, .kind = .use, .pos = .early,
                }) catch unreachable;
            },
            .callback => |cb| {
                const reg_ptr = wgpr.regMut().regMut();
                cb.func(cb.ctx, reg_ptr, .reuse, .def, .late);
                cb.func(cb.ctx, reg_ptr, .reuse, .use, .early);
            },
        }
    }

    /// Mark an XMM as used.
    pub fn xmmUse(self: *OperandVisitor, xmm: *Xmm) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = xmm.toReg(), .preg = null, .kind = .use, .pos = .early,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, xmm.regMut(), .any, .use, .early),
        }
    }

    /// Mark an XMM as defined.
    pub fn xmmDef(self: *OperandVisitor, wxmm: *args.WritableXmm) void {
        switch (self.*) {
            .collector => |c| c.operands.append(c.allocator, .{
                .reg = wxmm.toReg().toReg(), .preg = null, .kind = .def, .pos = .late,
            }) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, wxmm.regMut().regMut(), .any, .def, .late),
        }
    }

    /// Mark an XMM as reused (read-modify-write).
    pub fn xmmReuseDef(self: *OperandVisitor, wxmm: *args.WritableXmm, _: usize) void {
        switch (self.*) {
            .collector => |c| {
                const reg = wxmm.toReg().toReg();
                c.operands.append(c.allocator, .{
                    .reg = reg, .preg = null, .kind = .def, .pos = .late,
                }) catch unreachable;
                c.operands.append(c.allocator, .{
                    .reg = reg, .preg = null, .kind = .use, .pos = .early,
                }) catch unreachable;
            },
            .callback => |cb| {
                const reg_ptr = wxmm.regMut().regMut();
                cb.func(cb.ctx, reg_ptr, .reuse, .def, .late);
                cb.func(cb.ctx, reg_ptr, .reuse, .use, .early);
            },
        }
    }
};

//=============================================================================
// Address mode operand collection
//=============================================================================

/// Collect register operands from a SyntheticAmode.
fn syntheticAmodeOperands(amode: *SyntheticAmode, visitor: *OperandVisitor) void {
    switch (amode.*) {
        .real => |*a| amodeOperands(a, visitor),
        .incoming_arg, .slot_offset, .constant_offset => {
            // These use RSP/RBP implicitly, not tracked as operands.
        },
    }
}

/// Collect register operands from an Amode.
fn amodeOperands(amode: *Amode, visitor: *OperandVisitor) void {
    switch (amode.*) {
        .imm_reg => |*m| {
            // Don't track RSP/RBP as they're fixed physical registers.
            // Only check hwEnc for physical registers (virtual registers should be tracked).
            if (m.base.toRealReg()) |rreg| {
                const enc = rreg.preg.hwEnc();
                if (enc == regs.GprEnc.RSP or enc == regs.GprEnc.RBP) {
                    return; // Skip fixed registers
                }
            }
            visitor.regUse(&m.base);
        },
        .imm_reg_reg_shift => |*m| {
            visitor.gprUse(&m.base);
            visitor.gprUse(&m.index);
        },
        .rip_relative => {
            // RIP isn't involved in regalloc.
        },
    }
}

/// Collect register operands from a RegMem.
fn regMemOperands(rm: *RegMem, visitor: *OperandVisitor) void {
    switch (rm.*) {
        .reg => |*r| visitor.regUse(r),
        .mem => |*amode| syntheticAmodeOperands(amode, visitor),
    }
}

/// Collect register operands from a RegMemImm.
fn regMemImmOperands(rmi: *RegMemImm, visitor: *OperandVisitor) void {
    switch (rmi.*) {
        .reg => |*r| visitor.regUse(r),
        .mem => |*amode| syntheticAmodeOperands(amode, visitor),
        .imm => {},
    }
}

/// Collect register operands from a GprMem.
fn gprMemOperands(gm: *GprMem, visitor: *OperandVisitor) void {
    regMemOperands(&gm.inner, visitor);
}

/// Collect register operands from a GprMemImm.
fn gprMemImmOperands(gmi: *GprMemImm, visitor: *OperandVisitor) void {
    regMemImmOperands(&gmi.inner, visitor);
}

/// Collect register operands from an XmmMem.
fn xmmMemOperands(xm: *XmmMem, visitor: *OperandVisitor) void {
    regMemOperands(&xm.inner, visitor);
}

//=============================================================================
// Main getOperands function
//=============================================================================

/// Collect all register operands for an instruction.
///
/// This function is called twice:
/// 1. During regalloc input: visitor is in collector mode, gathers operands
/// 2. During emit: visitor is in callback mode, applies allocations
///
/// Reference: cranelift/codegen/src/isa/x64/inst/mod.rs
pub fn getOperands(inst: *Inst, visitor: *OperandVisitor) void {
    switch (inst.*) {
        //=====================================================================
        // NOP - no operands
        //=====================================================================
        .nop => {},

        //=====================================================================
        // ALU operations
        //=====================================================================
        .alu_rmi_r => |*p| {
            // 3-operand model: dst is early def (safe from overlapping uses),
            // src1 is the value to modify, src2 is the operand.
            // Emit code handles: mov src1→dst, then OP src2, dst.
            visitor.gprEarlyDef(&p.dst);
            visitor.gprUse(&p.src1);
            gprMemImmOperands(&p.src2, visitor);
        },

        //=====================================================================
        // Shifts
        //=====================================================================
        .shift_r => |*p| {
            visitor.gprReuseDef(&p.dst, 0);
            switch (p.shift_by) {
                .cl => {
                    // Shift amount vreg must be in RCX (CL is low byte of RCX).
                    // Port of Cranelift's ShiftR get_operands pattern.
                    visitor.regFixedUse(p.src.toReg(), regs.gprPreg(regs.GprEnc.RCX));
                },
                .imm => {},
            }
        },

        //=====================================================================
        // Unary operations
        //=====================================================================
        .unary_rm_r => |*p| {
            visitor.gprDef(&p.dst);
            gprMemOperands(&p.src, visitor);
        },

        //=====================================================================
        // Multiplication
        //=====================================================================
        .mul => |*p| {
            // MUL/IMUL implicitly use RAX and define RAX:RDX
            visitor.regFixedUse(regs.rax(), regs.gprPreg(regs.GprEnc.RAX));
            visitor.regFixedDef(Writable(Reg).fromReg(regs.rax()), regs.gprPreg(regs.GprEnc.RAX));
            visitor.regFixedDef(Writable(Reg).fromReg(regs.rdx()), regs.gprPreg(regs.GprEnc.RDX));
            gprMemOperands(&p.src, visitor);
        },

        //=====================================================================
        // Division
        //=====================================================================
        .div => |*p| {
            // DIV/IDIV: dividend_lo MUST be in RAX, dividend_hi MUST be in RDX
            // Port of Cranelift inst/mod.rs:879-882
            visitor.regFixedUse(p.dividend_lo.toReg(), regs.gprPreg(regs.GprEnc.RAX));
            visitor.regFixedUse(p.dividend_hi.toReg(), regs.gprPreg(regs.GprEnc.RDX));
            gprMemOperands(&p.divisor, visitor);
            // dst_quotient is defined in RAX, dst_remainder in RDX
            visitor.regFixedDef(Writable(Reg).fromReg(p.dst_quotient.toReg().toReg()), regs.gprPreg(regs.GprEnc.RAX));
            visitor.regFixedDef(Writable(Reg).fromReg(p.dst_remainder.toReg().toReg()), regs.gprPreg(regs.GprEnc.RDX));
        },

        //=====================================================================
        // Sign extension for division
        //=====================================================================
        .sign_extend_data => |*p| {
            // CDQ/CQO: sign-extend RAX into RDX (implicit fixed registers).
            // Port of Cranelift: cqto uses implicit(rax) read, implicit(rdx) write.
            // These MUST be fixed constraints because the hardware always uses RAX/RDX.
            visitor.regFixedUse(p.src.toReg(), regs.gprPreg(regs.GprEnc.RAX));
            visitor.regFixedDef(Writable(Reg).fromReg(p.dst.toReg().toReg()), regs.gprPreg(regs.GprEnc.RDX));
        },

        //=====================================================================
        // MOV operations
        //=====================================================================
        .mov_r_r => |*p| {
            visitor.gprDef(&p.dst);
            visitor.gprUse(&p.src);
        },
        .mov_r_m => |*p| {
            syntheticAmodeOperands(&p.dst, visitor);
            visitor.gprUse(&p.src);
        },
        .mov_m_r => |*p| {
            visitor.gprDef(&p.dst);
            syntheticAmodeOperands(&p.src, visitor);
        },
        .imm => |*p| {
            visitor.gprDef(&p.dst);
        },
        .mov_from_preg => |*p| {
            visitor.gprDef(&p.dst);
            visitor.regFixedNonallocatable(p.src);
        },
        .mov_to_preg => |*p| {
            visitor.regFixedNonallocatable(p.dst);
            visitor.gprUse(&p.src);
        },

        //=====================================================================
        // Zero/Sign extension moves
        //=====================================================================
        .movzx_rm_r => |*p| {
            visitor.gprDef(&p.dst);
            gprMemOperands(&p.src, visitor);
        },
        .movsx_rm_r => |*p| {
            visitor.gprDef(&p.dst);
            gprMemOperands(&p.src, visitor);
        },

        //=====================================================================
        // LEA
        //=====================================================================
        .lea => |*p| {
            visitor.gprDef(&p.dst);
            syntheticAmodeOperands(&p.src, visitor);
        },

        //=====================================================================
        // CMP and TEST
        //=====================================================================
        .cmp_rmi_r => |*p| {
            visitor.gprUse(&p.dst);
            gprMemImmOperands(&p.src, visitor);
        },
        .test_rmi_r => |*p| {
            visitor.gprUse(&p.dst);
            gprMemImmOperands(&p.src, visitor);
        },

        //=====================================================================
        // CMOVcc
        //=====================================================================
        .cmove => |*p| {
            visitor.gprReuseDef(&p.dst, 0);
            gprMemOperands(&p.src, visitor);
        },

        //=====================================================================
        // SETcc
        //=====================================================================
        .setcc => |*p| {
            visitor.gprDef(&p.dst);
        },

        //=====================================================================
        // Jumps and branches
        //=====================================================================
        .jmp_known => {},
        .jmp_unknown => |*p| {
            regMemOperands(&p.target, visitor);
        },
        .jmp_cond => {},

        //=====================================================================
        // Calls
        //=====================================================================
        .call_known => |*p| {
            // Arguments are uses
            for (p.info.uses.items) |arg| {
                visitor.regFixedUse(arg.vreg, arg.preg);
            }
            // Return values are defs
            for (p.info.defs.items) |ret| {
                visitor.regFixedDef(ret.vreg, ret.location.reg);
            }
            // CRITICAL: Add clobbered registers so regalloc knows to spill
            // values that are live across the call. Without this, values in
            // caller-saved registers (RAX, RCX, RDX, RSI, RDI, R8-R11, XMM0-15)
            // would be clobbered by the call but regalloc wouldn't know to save them.
            visitor.addClobbers(p.info.clobbers);
        },
        .call_unknown => |*p| {
            regMemOperands(&p.info.dest, visitor);
            for (p.info.uses.items) |arg| {
                visitor.regFixedUse(arg.vreg, arg.preg);
            }
            for (p.info.defs.items) |ret| {
                visitor.regFixedDef(ret.vreg, ret.location.reg);
            }
            // CRITICAL: Add clobbered registers for indirect calls too
            visitor.addClobbers(p.info.clobbers);
        },

        //=====================================================================
        // Push/Pop
        //=====================================================================
        .push => |p| {
            // Push uses source register (reads the register value)
            visitor.regUse(@constCast(&p.src.toReg()));
        },
        .pop => |*p| {
            // Pop defines destination register (writes to it)
            visitor.regDef(@ptrCast(&p.dst));
        },

        //=====================================================================
        // Return
        //=====================================================================
        .ret => {},

        //=====================================================================
        // Return calls (tail calls)
        //=====================================================================
        .return_call_known => |*p| {
            for (p.info.uses.items) |arg| {
                visitor.regFixedUse(arg.vreg, arg.preg);
            }
        },
        .return_call_unknown => |*p| {
            visitor.regUse(&p.info.dest);
            for (p.info.uses.items) |arg| {
                visitor.regFixedUse(arg.vreg, arg.preg);
            }
        },

        //=====================================================================
        // Conditional jumps (or variant)
        //=====================================================================
        .jmp_cond_or => {},
        .winch_jmp_if => {},
        .jmp_table_seq => |*p| {
            // Port of Cranelift's JmpTableSeq operand handling:
            // Reference: cranelift/codegen/src/isa/x64/inst/mod.rs
            //
            // Visit order must match collectOperands order:
            //   [early_defs..., defs..., uses...] = [tmp1, tmp2, idx]
            //
            // tmp1 is an early_def because it's written (by LEA) before idx is
            // fully consumed (by MOVSXD's SIB addressing).
            // tmp2 is a regular def - it's only written after idx is read.
            visitor.regEarlyDef(&p.tmp1);
            visitor.regDef(&p.tmp2);
            visitor.regUse(&p.idx);
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
        .load_ext_name => |*p| {
            visitor.gprDef(&p.dst);
        },

        //=====================================================================
        // ABI pseudo-instructions
        //=====================================================================
        .args => |*p| {
            for (p.args_list) |arg| {
                visitor.regFixedDef(Writable(Reg).fromReg(arg.vreg), arg.preg);
            }
        },
        .rets => |*p| {
            for (p.rets_list) |ret| {
                visitor.regFixedUse(ret.vreg, ret.preg);
            }
        },
        .ret_value_copy => |*p| {
            visitor.gprUse(&p.src);
            // No regFixedDef - the emit code reads preg directly for the
            // destination. This avoids confusing the regalloc with a fixed def
            // on the same instruction as the use.
        },

        //=====================================================================
        // SSE/XMM operations
        //=====================================================================
        .xmm_rm_r => |*p| {
            visitor.xmmReuseDef(&p.dst, 0);
            xmmMemOperands(&p.src, visitor);
        },
        .xmm_rm_r_evex => |*p| {
            visitor.xmmDef(&p.dst);
            visitor.xmmUse(&p.src1);
            xmmMemOperands(&p.src2, visitor);
        },
        .xmm_unary_rm_r => |*p| {
            visitor.xmmDef(&p.dst);
            xmmMemOperands(&p.src, visitor);
        },
        .xmm_mov_m_r => |*p| {
            visitor.xmmDef(&p.dst);
            syntheticAmodeOperands(&p.src, visitor);
        },
        .xmm_mov_r_m => |*p| {
            syntheticAmodeOperands(&p.dst, visitor);
            visitor.xmmUse(&p.src);
        },
        .xmm_cmp_rm_r => |*p| {
            visitor.xmmUse(&p.dst);
            xmmMemOperands(&p.src, visitor);
        },
        .xmm_to_gpr => |*p| {
            visitor.gprDef(&p.dst);
            visitor.xmmUse(&p.src);
        },
        .gpr_to_xmm => |*p| {
            visitor.xmmDef(&p.dst);
            gprMemOperands(&p.src, visitor);
        },
        .xmm_cmp_imm => |*p| {
            visitor.xmmReuseDef(&p.dst, 0);
            xmmMemOperands(&p.src, visitor);
        },
        .xmm_round => |*p| {
            visitor.xmmDef(&p.dst);
            xmmMemOperands(&p.src, visitor);
        },

        //=====================================================================
        // Atomic operations
        //=====================================================================
        .atomic_rmw_seq => |*p| {
            syntheticAmodeOperands(&p.mem, visitor);
            visitor.gprUse(&p.operand);
            visitor.gprDef(&p.dst_old);
            visitor.gprDef(&p.tmp);
        },
        .atomic_128_rmw_seq => |*p| {
            syntheticAmodeOperands(&p.mem, visitor);
            visitor.gprUse(&p.operand_low);
            visitor.gprUse(&p.operand_high);
            visitor.gprDef(&p.temp_low);
            visitor.gprDef(&p.temp_high);
            visitor.gprDef(&p.dst_old_low);
            visitor.gprDef(&p.dst_old_high);
        },
        .atomic_128_xchg_seq => |*p| {
            syntheticAmodeOperands(&p.mem, visitor);
            visitor.gprUse(&p.operand_low);
            visitor.gprUse(&p.operand_high);
            visitor.gprDef(&p.dst_old_low);
            visitor.gprDef(&p.dst_old_high);
        },

        //=====================================================================
        // TLS operations
        //=====================================================================
        .elf_tls_get_addr => |*p| {
            visitor.gprDef(&p.dst);
        },
        .macho_tls_get_addr => |*p| {
            visitor.gprDef(&p.dst);
        },
        .coff_tls_get_addr => |*p| {
            visitor.gprDef(&p.dst);
            visitor.gprDef(&p.tmp);
        },

        //=====================================================================
        // Pseudo-instructions
        //=====================================================================
        .checked_srem_seq => |*p| {
            visitor.gprUse(&p.divisor);
            visitor.gprUse(&p.dividend_lo);
            visitor.gprUse(&p.dividend_hi);
            visitor.gprDef(&p.dst_quotient);
            visitor.gprDef(&p.dst_remainder);
        },
        .checked_srem_seq8 => |*p| {
            visitor.gprUse(&p.divisor);
            visitor.gprUse(&p.dividend);
            visitor.gprDef(&p.dst);
        },
        .xmm_uninitialized_value => |*p| {
            visitor.xmmDef(&p.dst);
        },
        .gpr_uninitialized_value => |*p| {
            visitor.gprDef(&p.dst);
        },
        .xmm_min_max_seq => |*p| {
            visitor.xmmUse(&p.lhs);
            visitor.xmmUse(&p.rhs);
            visitor.xmmDef(&p.dst);
        },
        .cvt_uint64_to_float_seq => |*p| {
            visitor.gprUse(&p.src);
            visitor.xmmDef(&p.dst);
            visitor.gprDef(&p.tmp_gpr1);
            visitor.gprDef(&p.tmp_gpr2);
        },
        .cvt_float_to_sint_seq => |*p| {
            visitor.xmmUse(&p.src);
            visitor.gprDef(&p.dst);
            visitor.xmmDef(&p.tmp_xmm);
            visitor.gprDef(&p.tmp_gpr);
        },
        .cvt_float_to_uint_seq => |*p| {
            visitor.xmmUse(&p.src);
            visitor.gprDef(&p.dst);
            visitor.xmmDef(&p.tmp_xmm);
            visitor.xmmDef(&p.tmp_xmm2);
            visitor.gprDef(&p.tmp_gpr);
        },
        .xmm_cmove => |*p| {
            visitor.xmmUse(&p.consequent);
            visitor.xmmUse(&p.alternative);
            visitor.xmmDef(&p.dst);
        },
        .stack_probe_loop => |*p| {
            visitor.gprDef(&p.tmp);
        },

        //=====================================================================
        // Miscellaneous
        //=====================================================================
        .stack_switch_basic => |*p| {
            visitor.gprUse(&p.store_context_ptr);
            visitor.gprUse(&p.load_context_ptr);
            visitor.gprUse(&p.in_payload0);
            visitor.gprDef(&p.out_payload0);
        },
        .unwind => {},
        .dummy_use => |*p| {
            visitor.regUse(&p.reg);
        },
        .label_address => |*p| {
            visitor.gprDef(&p.dst);
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

test "CollectorState basic usage" {
    const testing = std.testing;
    var state = OperandVisitor.CollectorState.init(testing.allocator);
    defer state.deinit();

    var visitor = OperandVisitor.initCollector(&state);

    const x0 = regs.rax();
    const x1 = regs.rcx();
    var reg0 = x0;
    var reg1 = x1;
    var def_reg = Writable(Reg).fromReg(x0);

    visitor.regUse(&reg0);
    visitor.regUse(&reg1);
    visitor.regDef(&def_reg);

    // All operands in flat list in source order: use, use, def
    try testing.expectEqual(@as(usize, 3), state.operands.items.len);
}

test "getOperands alu_rmi_r" {
    const testing = std.testing;
    var state = OperandVisitor.CollectorState.init(testing.allocator);
    defer state.deinit();

    var visitor = OperandVisitor.initCollector(&state);

    const rax_reg = regs.rax();
    const rcx_reg = regs.rcx();

    const gpr_rax = args.Gpr.new(rax_reg) orelse unreachable;
    const gpr_rcx = args.Gpr.new(rcx_reg) orelse unreachable;
    const rdx_reg = regs.rdx();

    var inst_val = Inst{ .alu_rmi_r = .{
        .size = .size64,
        .op = .add,
        .src1 = gpr_rax,
        .src2 = args.GprMemImm{ .inner = .{ .reg = rdx_reg } },
        .dst = args.WritableGpr.fromReg(gpr_rcx),
    } };

    getOperands(&inst_val, &visitor);

    // ALU 3-operand: gprEarlyDef(dst) → 1 entry, gprUse(src1) → 1 entry, gprMemImm(src2) → 1 entry
    try testing.expectEqual(@as(usize, 3), state.operands.items.len);
}

test "getOperands div" {
    const testing = std.testing;
    var state = OperandVisitor.CollectorState.init(testing.allocator);
    defer state.deinit();

    var visitor = OperandVisitor.initCollector(&state);

    const rax_reg = regs.rax();
    const rdx_reg = regs.rdx();
    const rbx_reg = regs.rbx();

    const gpr_rax = args.Gpr.new(rax_reg) orelse unreachable;
    const gpr_rdx = args.Gpr.new(rdx_reg) orelse unreachable;

    var inst_val = Inst{ .div = .{
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

    // DIV: 2 fixed uses (dividend_lo, dividend_hi) + 1 use (divisor) + 2 fixed defs (quotient, remainder)
    // All in flat list in source order
    try testing.expectEqual(@as(usize, 5), state.operands.items.len);
}
