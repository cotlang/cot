//! AArch64 ISA: Register operand collection for register allocation.
//!
//! This module implements `getOperands()` which visits register operands
//! for each instruction type. It supports two modes:
//!
//! 1. **Collection mode**: Collects register uses/defs for regalloc input
//! 2. **Callback mode**: Applies allocations during emit by mutating registers
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/mod.rs:354-780`
//! and `cranelift/codegen/src/machinst/reg.rs:378-552`

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
/// impl<T: FnMut(&mut Reg, OperandConstraint, OperandKind, OperandPos)> OperandVisitor for T
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
    /// Reference: cranelift/codegen/src/machinst/reg.rs OperandCollector
    pub const CollectorState = struct {
        /// Registers used (read from) by this instruction (early position).
        uses: std.ArrayListUnmanaged(Reg),
        /// Registers defined (written to) by this instruction (late position).
        defs: std.ArrayListUnmanaged(Writable(Reg)),
        /// Early defs - written at instruction start, prevents overlap with uses.
        /// Reference: reg.rs:425 reg_early_def
        early_defs: std.ArrayListUnmanaged(Writable(Reg)),
        /// Fixed register constraints.
        fixed_uses: std.ArrayListUnmanaged(struct { vreg: Reg, preg: PReg }),
        fixed_defs: std.ArrayListUnmanaged(struct { vreg: Writable(Reg), preg: PReg }),
        /// Clobbered registers.
        clobbers: std.ArrayListUnmanaged(PReg),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) CollectorState {
            return .{
                .uses = .{},
                .defs = .{},
                .early_defs = .{},
                .fixed_uses = .{},
                .fixed_defs = .{},
                .clobbers = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *CollectorState) void {
            self.uses.deinit(self.allocator);
            self.defs.deinit(self.allocator);
            self.early_defs.deinit(self.allocator);
            self.fixed_uses.deinit(self.allocator);
            self.fixed_defs.deinit(self.allocator);
            self.clobbers.deinit(self.allocator);
        }

        pub fn clear(self: *CollectorState) void {
            self.uses.clearRetainingCapacity();
            self.defs.clearRetainingCapacity();
            self.early_defs.clearRetainingCapacity();
            self.fixed_uses.clearRetainingCapacity();
            self.fixed_defs.clearRetainingCapacity();
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
            .collector => |c| c.uses.append(c.allocator, reg.*) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, reg, .any, .use, .early),
        }
    }

    /// Mark a register as defined (written), at late position.
    /// Reference: reg.rs:417 fn reg_def
    pub fn regDef(self: *OperandVisitor, reg: *Writable(Reg)) void {
        switch (self.*) {
            .collector => |c| c.defs.append(c.allocator, reg.*) catch unreachable,
            .callback => |cb| cb.func(cb.ctx, reg.regMut(), .any, .def, .late),
        }
    }

    /// Mark a register as both used and redefined (read-modify-write).
    /// Reference: reg.rs:445 fn reg_reuse_def
    pub fn regReuseDef(self: *OperandVisitor, reg: *Writable(Reg), _: usize) void {
        switch (self.*) {
            .collector => |c| {
                c.uses.append(c.allocator, reg.toReg()) catch unreachable;
                c.defs.append(c.allocator, reg.*) catch unreachable;
            },
            .callback => |cb| {
                // For reuse, both use and def refer to the same register
                cb.func(cb.ctx, reg.regMut(), .reuse, .use, .early);
                cb.func(cb.ctx, reg.regMut(), .reuse, .def, .late);
            },
        }
    }

    /// Mark a fixed physical register as used.
    /// Fixed registers are not mutated during emit - they stay as the fixed preg.
    /// Reference: reg.rs:468 fn reg_fixed_use
    pub fn regFixedUse(self: *OperandVisitor, vreg: Reg, preg: PReg) void {
        switch (self.*) {
            .collector => |c| c.fixed_uses.append(c.allocator, .{ .vreg = vreg, .preg = preg }) catch unreachable,
            .callback => {
                // Fixed registers don't get allocation applied - they ARE the allocation
            },
        }
    }

    /// Mark a fixed physical register as defined.
    /// Fixed registers are not mutated during emit - they stay as the fixed preg.
    /// Reference: reg.rs:476 fn reg_fixed_def
    pub fn regFixedDef(self: *OperandVisitor, vreg: Writable(Reg), preg: PReg) void {
        switch (self.*) {
            .collector => |c| c.fixed_defs.append(c.allocator, .{ .vreg = vreg, .preg = preg }) catch unreachable,
            .callback => {
                // Fixed registers don't get allocation applied - they ARE the allocation
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
    pub fn addClobbers(self: *OperandVisitor, clobbers: mod.PRegSet) void {
        switch (self.*) {
            .collector => |c| {
                // Iterate through all possible integer registers (X0-X30)
                var i: u8 = 0;
                while (i < 31) : (i += 1) {
                    const preg = regs.xregPreg(i);
                    if (clobbers.contains(preg)) {
                        c.clobbers.append(c.allocator, preg) catch unreachable;
                    }
                }
                // Iterate through all possible vector registers (V0-V31)
                i = 0;
                while (i < 32) : (i += 1) {
                    const preg = regs.vregPreg(i);
                    if (clobbers.contains(preg)) {
                        c.clobbers.append(c.allocator, preg) catch unreachable;
                    }
                }
            },
            .callback => {},
        }
    }
};

//=============================================================================
// Address mode operand collection
//=============================================================================

/// Collect register operands from an addressing mode.
fn memargOperands(mem: *AMode, visitor: *OperandVisitor) void {
    switch (mem.*) {
        .unscaled => |*m| visitor.regUse(&m.rn),
        .unsigned_offset => |*m| visitor.regUse(&m.rn),
        .reg_reg => |*m| {
            visitor.regUse(&m.rn);
            visitor.regUse(&m.rm);
        },
        .reg_scaled => |*m| {
            visitor.regUse(&m.rn);
            visitor.regUse(&m.rm);
        },
        .reg_scaled_extended => |*m| {
            visitor.regUse(&m.rn);
            visitor.regUse(&m.rm);
        },
        .reg_extended => |*m| {
            visitor.regUse(&m.rn);
            visitor.regUse(&m.rm);
        },
        .reg_offset => |*m| visitor.regUse(&m.rn),
        .sp_offset, .fp_offset, .slot_offset, .incoming_arg => {},
        .sp_pre_indexed, .sp_post_indexed => {},
        .label, .constant => {},
    }
}

/// Collect register operands from a pair addressing mode.
fn pairmemargOperands(mem: *PairAMode, visitor: *OperandVisitor) void {
    switch (mem.*) {
        .signed_offset => |*m| visitor.regUse(&m.reg),
        .sp_pre_indexed, .sp_post_indexed => {},
    }
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
/// Reference: cranelift/codegen/src/isa/aarch64/inst/mod.rs:354-780
pub fn getOperands(inst: *Inst, visitor: *OperandVisitor) void {
    switch (inst.*) {
        // ALU operations
        // Reference: aarch64/inst/mod.rs:356-388
        .alu_rrr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .alu_rrrr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
            visitor.regUse(&p.ra);
        },
        .alu_rr_imm12 => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .alu_rr_imm_logic => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .alu_rr_imm_shift => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .alu_rrr_shift => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .alu_rrr_extend => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .bit_rr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },

        // Loads
        // Reference: aarch64/inst/mod.rs:393-402
        .uload8 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .sload8 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .uload16 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .sload16 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .uload32 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .sload32 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .uload64 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },

        // Stores
        // Reference: aarch64/inst/mod.rs:403-420
        .store8 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .store16 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .store32 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .store64 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },

        // Pair loads/stores
        .store_p64 => |*p| {
            visitor.regUse(&p.rt);
            visitor.regUse(&p.rt2);
            pairmemargOperands(&p.mem, visitor);
        },
        .load_p64 => |*p| {
            visitor.regDef(&p.rt);
            visitor.regDef(&p.rt2);
            pairmemargOperands(&p.mem, visitor);
        },

        // Moves
        // Reference: aarch64/inst/mod.rs:426-455
        .mov => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rm);
        },
        .mov_wide => |*p| {
            visitor.regDef(&p.rd);
        },
        .movk => |*p| {
            visitor.regUse(&p.rn);
            visitor.regReuseDef(&p.rd, 0);
        },
        .mov_from_preg => |*p| {
            visitor.regDef(&p.rd);
            visitor.regFixedNonallocatable(p.rm);
        },
        .mov_to_preg => |*p| {
            visitor.regFixedNonallocatable(p.rd);
            visitor.regUse(&p.rm);
        },

        // Conditional operations
        // Reference: aarch64/inst/mod.rs:541-570
        .csel => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .csneg => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .cset => |*p| {
            visitor.regDef(&p.rd);
        },
        .csetm => |*p| {
            visitor.regDef(&p.rd);
        },
        .ccmp_imm => |*p| {
            visitor.regUse(&p.rn);
        },

        // Branches
        .jump, .cond_br => {},

        // Direct call - operands are tracked via CallInfo.uses/defs/clobbers
        // Port of Cranelift's call operand handling from aarch64/inst/mod.rs
        .call => |p| {
            const info = p.info;
            // Add argument uses: each vreg must be in its designated preg
            for (info.uses.items) |use| {
                visitor.regFixedUse(use.vreg, use.preg);
            }
            // Add return value defs: each vreg is defined in its designated preg
            for (info.defs.items) |def| {
                switch (def.location) {
                    .reg => |preg| visitor.regFixedDef(def.vreg, preg),
                    .stack => {},
                }
            }
            // Add clobbered registers
            visitor.addClobbers(info.clobbers);
        },

        // Indirect call - register operand plus CallIndInfo tracking
        // Port of Cranelift's call_ind operand handling from aarch64/inst/mod.rs
        .call_ind => |p| {
            const info = p.info;
            // The destination register is a use - pass reference to actual field
            // so allocation callback can mutate it (not a local copy)
            visitor.regUse(&info.dest);
            // Add argument uses
            for (info.uses.items) |use| {
                visitor.regFixedUse(use.vreg, use.preg);
            }
            // Add return value defs
            for (info.defs.items) |def| {
                switch (def.location) {
                    .reg => |preg| visitor.regFixedDef(def.vreg, preg),
                    .stack => {},
                }
            }
            // Add clobbered registers
            visitor.addClobbers(info.clobbers);
        },
        .indirect_br => |*p| {
            visitor.regUse(&p.rn);
        },
        .ret, .brk, .csdb, .fence, .nop0, .nop4 => {},

        // Args/Rets pseudo-instructions
        // Reference: aarch64/inst/mod.rs:780-800
        .rets => |p| {
            for (p.rets) |ret_pair| {
                visitor.regFixedUse(ret_pair.vreg, ret_pair.preg);
            }
        },
        .args => |p| {
            for (p.args) |arg_pair| {
                visitor.regFixedDef(arg_pair.vreg, arg_pair.preg);
            }
        },

        // FPU operations
        // Reference: aarch64/inst/mod.rs:456-540
        .fpu_cmp => |*p| {
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .fpu_rr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .fpu_rrr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .fpu_rrrr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
            visitor.regUse(&p.ra);
        },
        .fpu_to_int => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .int_to_fpu => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .fpu_round => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .fpu_load32 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_load64 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_load128 => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_store32 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_store64 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_store128 => |*p| {
            visitor.regUse(&p.rd);
            memargOperands(&p.mem, visitor);
        },
        .fpu_move32 => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .fpu_move64 => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .fpu_move128 => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },

        // Vector operations
        // Reference: aarch64/inst/mod.rs:571-700
        .vec_rrr => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_rrr_mod => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.ri);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_misc => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_lanes => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_shift_imm => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_shift_imm_mod => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.ri);
            visitor.regUse(&p.rn);
        },
        .vec_dup => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_dup_imm => |*p| {
            visitor.regDef(&p.rd);
        },
        .mov_to_vec => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.ri);
            visitor.regUse(&p.rn);
        },
        .mov_from_vec => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_extend => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_rr_long => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_rr_narrow => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_rrr_long => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_rrr_long_mod => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.ri);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_rr_pair_long => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .vec_extract => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_tbl => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },
        .vec_tbl_ext => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.ri);
            visitor.regUse(&p.rn);
            visitor.regUse(&p.rm);
        },

        // Atomics
        // Reference: aarch64/inst/mod.rs:701-750
        .atomic_rmw => |*p| {
            visitor.regUse(&p.rs);
            visitor.regDef(&p.rt);
            visitor.regUse(&p.rn);
        },
        .atomic_cas => |*p| {
            visitor.regReuseDef(&p.rd, 1);
            visitor.regUse(&p.rs);
            visitor.regUse(&p.rt);
            visitor.regUse(&p.rn);
        },
        .ldaxr => |*p| {
            visitor.regDef(&p.rt);
            visitor.regUse(&p.rn);
        },
        .stlxr => |*p| {
            visitor.regDef(&p.rs);
            visitor.regUse(&p.rt);
            visitor.regUse(&p.rn);
        },
        .ldar => |*p| {
            visitor.regDef(&p.rt);
            visitor.regUse(&p.rn);
        },
        .stlr => |*p| {
            visitor.regUse(&p.rt);
            visitor.regUse(&p.rn);
        },
        .atomic_rmw_loop, .atomic_cas_loop => {},

        // Jump table
        // IMPORTANT: Order must match collectOperands in vcode.zig - defs first, then uses
        // Port of Cranelift's JTSequence operand collection
        .jt_sequence => |*p| {
            visitor.regDef(&p.rtmp1);
            visitor.regDef(&p.rtmp2);
            visitor.regUse(&p.ridx);
        },

        // External symbol loading
        .load_ext_name_got => |*p| {
            visitor.regDef(&p.rd);
        },
        .load_ext_name_near => |*p| {
            visitor.regDef(&p.rd);
        },
        .load_ext_name_far => |*p| {
            visitor.regDef(&p.rd);
        },

        // Address operations
        .adr => |*p| {
            visitor.regDef(&p.rd);
        },
        .adrp => |*p| {
            visitor.regDef(&p.rd);
        },
        .load_addr => |*p| {
            visitor.regDef(&p.rd);
            memargOperands(&p.mem, visitor);
        },

        // Misc
        .extend => |*p| {
            visitor.regDef(&p.rd);
            visitor.regUse(&p.rn);
        },
        .udf, .trap_if, .bti, .word4, .word8 => {},

        // Fallback
        else => {},
    }
}

//=============================================================================
// Tests
//=============================================================================

test "CollectorState basic usage" {
    const testing = std.testing;
    var state = OperandVisitor.CollectorState.init(testing.allocator);
    defer state.deinit();

    var visitor = OperandVisitor.initCollector(&state);

    const x0 = regs.xreg(0);
    const x1 = regs.xreg(1);
    var reg0 = x0;
    var reg1 = x1;
    var def_reg = Writable(Reg).fromReg(x0);

    visitor.regUse(&reg0);
    visitor.regUse(&reg1);
    visitor.regDef(&def_reg);

    try testing.expectEqual(@as(usize, 2), state.uses.items.len);
    try testing.expectEqual(@as(usize, 1), state.defs.items.len);
}

test "getOperands alu_rrr collector mode" {
    const testing = std.testing;
    var state = OperandVisitor.CollectorState.init(testing.allocator);
    defer state.deinit();

    var visitor = OperandVisitor.initCollector(&state);

    const x0 = regs.xreg(0);
    const x1 = regs.xreg(1);
    const x2 = regs.xreg(2);

    var inst = Inst{ .alu_rrr = .{
        .alu_op = .add,
        .size = .size64,
        .rd = Writable(Reg).fromReg(x0),
        .rn = x1,
        .rm = x2,
    } };

    getOperands(&inst, &visitor);

    try testing.expectEqual(@as(usize, 2), state.uses.items.len);
    try testing.expectEqual(@as(usize, 1), state.defs.items.len);
}

test "getOperands callback mode mutates registers" {
    const testing = std.testing;

    const x0 = regs.xreg(0);
    const x1 = regs.xreg(1);
    const x2 = regs.xreg(2);
    const x10 = regs.xreg(10);
    const x11 = regs.xreg(11);
    const x12 = regs.xreg(12);

    var inst = Inst{ .alu_rrr = .{
        .alu_op = .add,
        .size = .size64,
        .rd = Writable(Reg).fromReg(x0),
        .rn = x1,
        .rm = x2,
    } };

    // Allocations: x0->x10, x1->x11, x2->x12
    const allocations = [_]Reg{ x10, x11, x12 };
    var alloc_idx: usize = 0;

    const CallbackCtx = struct {
        allocs: []const Reg,
        idx: *usize,

        fn callback(ctx_ptr: *anyopaque, reg: *Reg, _: OperandConstraint, _: OperandKind, _: OperandPos) void {
            const ctx: *@This() = @ptrCast(@alignCast(ctx_ptr));
            if (ctx.idx.* < ctx.allocs.len) {
                reg.* = ctx.allocs[ctx.idx.*];
                ctx.idx.* += 1;
            }
        }
    };

    var ctx = CallbackCtx{ .allocs = &allocations, .idx = &alloc_idx };
    var visitor = OperandVisitor.initCallback(@ptrCast(&ctx), CallbackCtx.callback);

    getOperands(&inst, &visitor);

    // Verify registers were mutated
    try testing.expectEqual(x10, inst.alu_rrr.rd.toReg());
    try testing.expectEqual(x11, inst.alu_rrr.rn);
    try testing.expectEqual(x12, inst.alu_rrr.rm);
}
