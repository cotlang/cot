//! AArch64 ISA: Register operand collection for register allocation.
//!
//! This module implements `aarch64_get_operands()` which collects register
//! uses and definitions for each instruction type. This is needed by the
//! register allocator to understand register liveness.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/mod.rs`

const std = @import("std");
const mod = @import("mod.zig");
const args = @import("args.zig");
const regs = @import("regs.zig");

const Inst = mod.Inst;
const AMode = mod.AMode;
const PairAMode = mod.PairAMode;
const Reg = args.Reg;
const Writable = regs.Writable;
const PReg = regs.PReg;

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
};

/// Collect register operands from an addressing mode.
fn memargOperands(mem: AMode, collector: *OperandVisitor) void {
    switch (mem) {
        .unscaled => |m| collector.regUse(m.rn),
        .unsigned_offset => |m| collector.regUse(m.rn),
        .reg_reg => |m| {
            collector.regUse(m.rn);
            collector.regUse(m.rm);
        },
        .reg_scaled => |m| {
            collector.regUse(m.rn);
            collector.regUse(m.rm);
        },
        .reg_scaled_extended => |m| {
            collector.regUse(m.rn);
            collector.regUse(m.rm);
        },
        .reg_extended => |m| {
            collector.regUse(m.rn);
            collector.regUse(m.rm);
        },
        .reg_offset => |m| collector.regUse(m.rn),
        .sp_offset, .fp_offset, .slot_offset, .incoming_arg => {
            // These use SP/FP implicitly, not tracked as operands.
        },
        .sp_pre_indexed, .sp_post_indexed => {
            // Stack pointer implicit.
        },
        .label, .constant => {
            // PC-relative, no register operands.
        },
    }
}

/// Collect register operands from a pair addressing mode.
fn pairmemargOperands(mem: PairAMode, collector: *OperandVisitor) void {
    switch (mem) {
        .signed_offset => |m| collector.regUse(m.reg),
        .sp_pre_indexed, .sp_post_indexed => {
            // Stack pointer implicit.
        },
    }
}

/// Collect all register operands for an instruction.
/// This is used by the register allocator to determine liveness.
pub fn getOperands(inst: *const Inst, collector: *OperandVisitor) void {
    switch (inst.*) {
        // ALU operations
        .alu_rrr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .alu_rrrr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
            collector.regUse(p.ra);
        },
        .alu_rr_imm12 => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .alu_rr_imm_logic => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .alu_rr_imm_shift => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .alu_rrr_shift => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .alu_rrr_extend => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .bit_rr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },

        // Loads
        .uload8 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .sload8 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .uload16 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .sload16 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .uload32 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .sload32 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .uload64 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },

        // Stores
        .store8 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .store16 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .store32 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .store64 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },

        // Pair loads/stores
        .store_p64 => |p| {
            collector.regUse(p.rt);
            collector.regUse(p.rt2);
            pairmemargOperands(p.mem, collector);
        },
        .load_p64 => |p| {
            collector.regDef(p.rt);
            collector.regDef(p.rt2);
            pairmemargOperands(p.mem, collector);
        },

        // Moves
        .mov => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rm);
        },
        .mov_wide => |p| {
            collector.regDef(p.rd);
        },
        .movk => |p| {
            // Port of Cranelift: collector.reg_use(rn); collector.reg_reuse_def(rd, 0);
            // The use comes first, then reuse_def refers to operand index 0 (the use).
            collector.regUse(p.rn);
            collector.regReuseDef(p.rd, 0);
        },
        .mov_from_preg => |p| {
            collector.regDef(p.rd);
            collector.regFixedNonallocatable(p.rm);
        },
        .mov_to_preg => |p| {
            collector.regFixedNonallocatable(p.rd);
            collector.regUse(p.rm);
        },

        // Conditional operations
        .csel => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .csneg => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .cset => |p| {
            collector.regDef(p.rd);
        },
        .csetm => |p| {
            collector.regDef(p.rd);
        },
        .ccmp_imm => |p| {
            collector.regUse(p.rn);
        },

        // Branches (no register operands for labels)
        .jump, .cond_br, .call => {},
        .call_ind => |p| {
            collector.regUse(p.rn);
        },
        .indirect_br => |p| {
            collector.regUse(p.rn);
        },
        .ret, .brk, .csdb, .fence, .nop0, .nop4 => {},

        // Port of Cranelift: Inst::Rets { rets } => {
        //     for RetPair { vreg, preg } in rets {
        //         collector.reg_fixed_use(vreg, *preg);
        //     }
        // }
        .rets => |p| {
            for (p.rets) |ret_pair| {
                collector.regFixedUse(ret_pair.vreg, ret_pair.preg);
            }
        },

        // Port of Cranelift's Inst::Args operand collection from aarch64/inst/mod.rs:
        // Inst::Args { args } => {
        //     for ArgPair { vreg, preg } in args {
        //         collector.reg_fixed_def(vreg, *preg);
        //     }
        // }
        .args => |p| {
            for (p.args) |arg_pair| {
                collector.regFixedDef(arg_pair.vreg, arg_pair.preg);
            }
        },

        // FPU operations
        .fpu_cmp => |p| {
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .fpu_rr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .fpu_rrr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .fpu_rrrr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
            collector.regUse(p.ra);
        },
        .fpu_to_int => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .int_to_fpu => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .fpu_round => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .fpu_load32 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_load64 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_load128 => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_store32 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_store64 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_store128 => |p| {
            collector.regUse(p.rd);
            memargOperands(p.mem, collector);
        },
        .fpu_move32 => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .fpu_move64 => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .fpu_move128 => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },

        // Vector operations
        .vec_rrr => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_rrr_mod => |p| {
            collector.regReuseDef(p.rd, 1);
            collector.regUse(p.ri);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_misc => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_lanes => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_shift_imm => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_shift_imm_mod => |p| {
            collector.regReuseDef(p.rd, 1);
            collector.regUse(p.ri);
            collector.regUse(p.rn);
        },
        .vec_dup => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_dup_imm => |p| {
            collector.regDef(p.rd);
        },
        .mov_to_vec => |p| {
            collector.regReuseDef(p.rd, 1);
            collector.regUse(p.ri);
            collector.regUse(p.rn);
        },
        .mov_from_vec => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_extend => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_rr_long => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_rr_narrow => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_rrr_long => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_rrr_long_mod => |p| {
            collector.regReuseDef(p.rd, 1);
            collector.regUse(p.ri);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_rr_pair_long => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .vec_extract => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_tbl => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },
        .vec_tbl_ext => |p| {
            collector.regReuseDef(p.rd, 1);
            collector.regUse(p.ri);
            collector.regUse(p.rn);
            collector.regUse(p.rm);
        },

        // Atomics
        .atomic_rmw => |p| {
            // Port of Cranelift: reg_use(rs); reg_def(rt); reg_use(rn);
            collector.regUse(p.rs);
            collector.regDef(p.rt);
            collector.regUse(p.rn);
        },
        .atomic_cas => |p| {
            // Port of Cranelift: reg_reuse_def(rd, 1); reg_use(rs); reg_use(rt); reg_use(rn);
            collector.regReuseDef(p.rd, 1); // reuse `rs`.
            collector.regUse(p.rs);
            collector.regUse(p.rt);
            collector.regUse(p.rn);
        },
        .ldaxr => |p| {
            collector.regDef(p.rt);
            collector.regUse(p.rn);
        },
        .stlxr => |p| {
            collector.regDef(p.rs);
            collector.regUse(p.rt);
            collector.regUse(p.rn);
        },
        .ldar => |p| {
            collector.regDef(p.rt);
            collector.regUse(p.rn);
        },
        .stlr => |p| {
            collector.regUse(p.rt);
            collector.regUse(p.rn);
        },
        .atomic_rmw_loop, .atomic_cas_loop => {
            // These use fixed registers (X24-X28), handled specially.
        },

        // Jump table (uses fixed registers internally)
        .jt_sequence => |p| {
            collector.regUse(p.ridx);
            collector.regDef(p.rtmp1);
            collector.regDef(p.rtmp2);
        },

        // External symbol loading
        .load_ext_name_got => |p| {
            collector.regDef(p.rd);
        },
        .load_ext_name_near => |p| {
            collector.regDef(p.rd);
        },
        .load_ext_name_far => |p| {
            collector.regDef(p.rd);
        },

        // Address operations
        .adr => |p| {
            collector.regDef(p.rd);
        },
        .adrp => |p| {
            collector.regDef(p.rd);
        },
        .load_addr => |p| {
            collector.regDef(p.rd);
            memargOperands(p.mem, collector);
        },

        // Misc
        .extend => |p| {
            collector.regDef(p.rd);
            collector.regUse(p.rn);
        },
        .udf, .trap_if, .bti, .word4, .word8 => {},

        // Fallback for any unhandled instruction types
        else => {},
    }
}

// Tests
test "OperandVisitor basic usage" {
    const testing = std.testing;
    var visitor = OperandVisitor.init(testing.allocator);
    defer visitor.deinit();

    const x0 = regs.xreg(0);
    const x1 = regs.xreg(1);

    visitor.regUse(x0);
    visitor.regUse(x1);
    visitor.regDef(Writable(Reg).fromReg(x0));

    try testing.expectEqual(@as(usize, 2), visitor.uses.items.len);
    try testing.expectEqual(@as(usize, 1), visitor.defs.items.len);
}

test "getOperands alu_rrr" {
    const testing = std.testing;
    var visitor = OperandVisitor.init(testing.allocator);
    defer visitor.deinit();

    const x0 = regs.xreg(0);
    const x1 = regs.xreg(1);
    const x2 = regs.xreg(2);

    const inst = Inst{ .alu_rrr = .{
        .alu_op = .add,
        .size = .size64,
        .rd = Writable(Reg).fromReg(x0),
        .rn = x1,
        .rm = x2,
    } };

    getOperands(&inst, &visitor);

    try testing.expectEqual(@as(usize, 2), visitor.uses.items.len);
    try testing.expectEqual(@as(usize, 1), visitor.defs.items.len);
}
