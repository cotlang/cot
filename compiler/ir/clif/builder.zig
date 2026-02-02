//! Cranelift instruction builder.
//!
//! Port of cranelift/codegen/src/ir/builder.rs
//!
//! Provides a convenient interface for inserting instructions into a Cranelift
//! function.

const std = @import("std");
const dfg_mod = @import("dfg.zig");
const layout_mod = @import("layout.zig");
const types = @import("types.zig");
const instructions = @import("instructions.zig");

pub const DataFlowGraph = dfg_mod.DataFlowGraph;
pub const Layout = layout_mod.Layout;
pub const Block = dfg_mod.Block;
pub const Inst = dfg_mod.Inst;
pub const Value = dfg_mod.Value;
pub const ValueList = dfg_mod.ValueList;
pub const Type = types.Type;
pub const IntCC = instructions.IntCC;
pub const FloatCC = instructions.FloatCC;
pub const TrapCode = instructions.TrapCode;
pub const Opcode = instructions.Opcode;
pub const FuncRef = dfg_mod.FuncRef;
pub const SigRef = dfg_mod.SigRef;
pub const StackSlot = dfg_mod.StackSlot;
pub const JumpTable = dfg_mod.JumpTable;

// ============================================================================
// Instruction Formats
// Port of cranelift/codegen/meta/src/shared/formats.rs
// ============================================================================

/// Instruction format - describes the layout of operands.
pub const InstructionFormat = enum {
    nullary,
    unary,
    unary_imm,
    unary_ieee32,
    unary_ieee64,
    binary,
    binary_imm64,
    ternary,
    int_compare,
    float_compare,
    jump,
    brif,
    branch_table,
    call,
    call_indirect,
    trap,
    cond_trap,
    load,
    store,
    stack_load,
    stack_store,
    func_addr,
};

// ============================================================================
// Instruction Data
// Port of the generated InstructionData enum
// ============================================================================

/// Instruction operand data.
///
/// Each variant corresponds to an instruction format and holds the
/// immediate operands for instructions of that format.
pub const InstructionData = union(enum) {
    /// No operands (nop, return, etc).
    nullary: struct {
        opcode: Opcode,
    },

    /// Single value operand.
    unary: struct {
        opcode: Opcode,
        arg: Value,
    },

    /// Immediate integer operand.
    unary_imm: struct {
        opcode: Opcode,
        imm: i64,
    },

    /// Immediate 32-bit float operand.
    unary_ieee32: struct {
        opcode: Opcode,
        imm: f32,
    },

    /// Immediate 64-bit float operand.
    unary_ieee64: struct {
        opcode: Opcode,
        imm: f64,
    },

    /// Two value operands.
    binary: struct {
        opcode: Opcode,
        args: [2]Value,
    },

    /// One value and one immediate operand.
    binary_imm64: struct {
        opcode: Opcode,
        arg: Value,
        imm: i64,
    },

    /// Three value operands (select, fma).
    ternary: struct {
        opcode: Opcode,
        args: [3]Value,
    },

    /// Integer comparison.
    int_compare: struct {
        opcode: Opcode,
        cond: IntCC,
        args: [2]Value,
    },

    /// Float comparison.
    float_compare: struct {
        opcode: Opcode,
        cond: FloatCC,
        args: [2]Value,
    },

    /// Unconditional jump with block arguments.
    jump: struct {
        opcode: Opcode,
        destination: Block,
        args: ValueList,
    },

    /// Conditional branch (if-then-else).
    brif: struct {
        opcode: Opcode,
        arg: Value,
        then_block: Block,
        else_block: Block,
        then_args: ValueList,
        else_args: ValueList,
    },

    /// Branch table (switch).
    branch_table: struct {
        opcode: Opcode,
        arg: Value,
        table: JumpTable,
    },

    /// Direct function call.
    call: struct {
        opcode: Opcode,
        func_ref: FuncRef,
        args: ValueList,
    },

    /// Indirect function call.
    call_indirect: struct {
        opcode: Opcode,
        sig_ref: SigRef,
        callee: Value,
        args: ValueList,
    },

    /// Unconditional trap.
    trap: struct {
        opcode: Opcode,
        code: TrapCode,
    },

    /// Conditional trap.
    cond_trap: struct {
        opcode: Opcode,
        arg: Value,
        code: TrapCode,
    },

    /// Memory load.
    load: struct {
        opcode: Opcode,
        flags: MemFlags,
        arg: Value,
        offset: i32,
    },

    /// Memory store.
    store: struct {
        opcode: Opcode,
        flags: MemFlags,
        args: [2]Value, // [value, address]
        offset: i32,
    },

    /// Stack slot load.
    stack_load: struct {
        opcode: Opcode,
        slot: StackSlot,
        offset: i32,
    },

    /// Stack slot store.
    stack_store: struct {
        opcode: Opcode,
        arg: Value,
        slot: StackSlot,
        offset: i32,
    },

    /// Function address.
    func_addr: struct {
        opcode: Opcode,
        func_ref: FuncRef,
    },

    const Self = @This();

    /// Get the opcode of this instruction.
    pub fn opcode(self: Self) Opcode {
        return switch (self) {
            .nullary => |d| d.opcode,
            .unary => |d| d.opcode,
            .unary_imm => |d| d.opcode,
            .unary_ieee32 => |d| d.opcode,
            .unary_ieee64 => |d| d.opcode,
            .binary => |d| d.opcode,
            .binary_imm64 => |d| d.opcode,
            .ternary => |d| d.opcode,
            .int_compare => |d| d.opcode,
            .float_compare => |d| d.opcode,
            .jump => |d| d.opcode,
            .brif => |d| d.opcode,
            .branch_table => |d| d.opcode,
            .call => |d| d.opcode,
            .call_indirect => |d| d.opcode,
            .trap => |d| d.opcode,
            .cond_trap => |d| d.opcode,
            .load => |d| d.opcode,
            .store => |d| d.opcode,
            .stack_load => |d| d.opcode,
            .stack_store => |d| d.opcode,
            .func_addr => |d| d.opcode,
        };
    }

    /// Get the instruction format.
    pub fn format(self: Self) InstructionFormat {
        return switch (self) {
            .nullary => .nullary,
            .unary => .unary,
            .unary_imm => .unary_imm,
            .unary_ieee32 => .unary_ieee32,
            .unary_ieee64 => .unary_ieee64,
            .binary => .binary,
            .binary_imm64 => .binary_imm64,
            .ternary => .ternary,
            .int_compare => .int_compare,
            .float_compare => .float_compare,
            .jump => .jump,
            .brif => .brif,
            .branch_table => .branch_table,
            .call => .call,
            .call_indirect => .call_indirect,
            .trap => .trap,
            .cond_trap => .cond_trap,
            .load => .load,
            .store => .store,
            .stack_load => .stack_load,
            .stack_store => .stack_store,
            .func_addr => .func_addr,
        };
    }
};

// ============================================================================
// Memory Flags
// Port of cranelift memflags
// ============================================================================

/// Memory operation flags.
pub const MemFlags = packed struct(u8) {
    /// Memory is known to be aligned.
    aligned: bool = false,
    /// Memory access is readonly.
    readonly: bool = false,
    /// Trap on null address.
    trap_on_null: bool = false,
    /// Heap memory access.
    heap: bool = false,
    _padding: u4 = 0,

    pub const DEFAULT: MemFlags = .{};

    pub fn withAligned(self: MemFlags) MemFlags {
        var f = self;
        f.aligned = true;
        return f;
    }
};

// ============================================================================
// Function Builder
// Port of cranelift/codegen/src/ir/builder.rs
// ============================================================================

/// Builder for constructing instructions in a function.
///
/// This provides convenient methods for creating instructions and inserting
/// them into the function at the current position.
pub const FuncBuilder = struct {
    /// Allocator for memory allocation.
    allocator: std.mem.Allocator,
    /// Data flow graph.
    dfg: *DataFlowGraph,
    /// Function layout.
    layout: *Layout,
    /// Current block.
    current_block: ?Block,

    const Self = @This();

    /// Create a new function builder.
    pub fn init(allocator: std.mem.Allocator, dfg: *DataFlowGraph, layout: *Layout) Self {
        return .{
            .allocator = allocator,
            .dfg = dfg,
            .layout = layout,
            .current_block = null,
        };
    }

    /// Set the current block.
    pub fn switchToBlock(self: *Self, block: Block) void {
        self.current_block = block;
    }

    /// Create a new block.
    pub fn createBlock(self: *Self) !Block {
        return self.dfg.makeBlock();
    }

    /// Append a block parameter.
    pub fn appendBlockParam(self: *Self, block: Block, ty: Type) !Value {
        return self.dfg.appendBlockParam(block, ty);
    }

    /// Insert an instruction at the end of the current block.
    fn insertInst(self: *Self, data: InstructionData, result_type: ?Type) !struct { inst: Inst, result: ?Value } {
        const block = self.current_block orelse return error.NoCurrentBlock;
        const inst = self.dfg.makeInst();
        try self.layout.appendInst(self.allocator, inst, block);

        // Store instruction data in DFG
        // Extract immediate value if present
        const imm: ?i64 = switch (data) {
            .unary_imm => |d| d.imm,
            .binary_imm64 => |d| d.imm,
            else => null,
        };

        // Extract comparison condition codes if present
        const intcc: ?IntCC = switch (data) {
            .int_compare => |d| d.cond,
            else => null,
        };
        const floatcc: ?FloatCC = switch (data) {
            .float_compare => |d| d.cond,
            else => null,
        };

        // Extract jump table for br_table
        const jump_table: ?JumpTable = switch (data) {
            .branch_table => |d| d.table,
            else => null,
        };

        // Extract stack slot and offset for stack_load/stack_store
        const stack_slot: ?StackSlot = switch (data) {
            .stack_load => |d| d.slot,
            .stack_store => |d| d.slot,
            else => null,
        };
        const stack_offset: ?i32 = switch (data) {
            .stack_load => |d| d.offset,
            .stack_store => |d| d.offset,
            else => null,
        };

        // Extract func_ref for call
        const func_ref: ?FuncRef = switch (data) {
            .call => |d| d.func_ref,
            else => null,
        };

        // Extract sig_ref for call_indirect
        const sig_ref: ?SigRef = switch (data) {
            .call_indirect => |d| d.sig_ref,
            else => null,
        };

        // Extract args for the ValueList
        const args_slice: []const Value = switch (data) {
            .nullary => &[_]Value{},
            .unary => |d| &[_]Value{d.arg},
            .unary_imm, .unary_ieee32, .unary_ieee64 => &[_]Value{},
            .binary => |d| &d.args,
            .binary_imm64 => |d| &[_]Value{d.arg},
            .ternary => |d| &d.args,
            .int_compare => |d| &d.args,
            .float_compare => |d| &d.args,
            .jump => &[_]Value{}, // args in ValueList
            .brif => |d| &[_]Value{d.arg},
            .branch_table => |d| &[_]Value{d.arg},
            .call => &[_]Value{}, // args in ValueList
            .call_indirect => |d| &[_]Value{d.callee}, // callee + args
            .trap => &[_]Value{},
            .cond_trap => |d| &[_]Value{d.arg},
            .load => |d| &[_]Value{d.arg},
            .store => |d| &d.args,
            .stack_load => &[_]Value{},
            .stack_store => |d| &[_]Value{d.arg},
            .func_addr => &[_]Value{},
        };

        const args = try self.dfg.value_lists.alloc(args_slice);

        // Extract branch destinations
        const dest: ?dfg_mod.Block = switch (data) {
            .jump => |d| d.destination,
            else => null,
        };
        const then_dest: ?dfg_mod.Block = switch (data) {
            .brif => |d| d.then_block,
            else => null,
        };
        const else_dest: ?dfg_mod.Block = switch (data) {
            .brif => |d| d.else_block,
            else => null,
        };

        try self.dfg.setInstData(inst, .{
            .opcode = data.opcode(),
            .args = args,
            .ctrl_type = result_type orelse Type.INVALID,
            .dest = dest,
            .then_dest = then_dest,
            .else_dest = else_dest,
            .imm = imm,
            .intcc = intcc,
            .floatcc = floatcc,
            .jump_table = jump_table,
            .stack_slot = stack_slot,
            .stack_offset = stack_offset,
            .func_ref = func_ref,
            .sig_ref = sig_ref,
        });

        // Create result value if needed
        var result: ?Value = null;
        if (result_type) |ty| {
            result = try self.dfg.makeInstResult(inst, ty);
        }

        return .{ .inst = inst, .result = result };
    }

    // ========================================================================
    // Control Flow Instructions
    // ========================================================================

    /// Unconditional jump.
    pub fn jump(self: *Self, destination: Block, args: []const Value) !Inst {
        var arg_list = ValueList.init();
        for (args) |v| {
            arg_list = try self.dfg.value_lists.push(arg_list, v);
        }
        const r = try self.insertInst(.{
            .jump = .{
                .opcode = .jump,
                .destination = destination,
                .args = arg_list,
            },
        }, null);
        return r.inst;
    }

    /// Conditional branch.
    pub fn brif(self: *Self, cond: Value, then_block: Block, else_block: Block) !Inst {
        const r = try self.insertInst(.{
            .brif = .{
                .opcode = .brif,
                .arg = cond,
                .then_block = then_block,
                .else_block = else_block,
                .then_args = ValueList.init(),
                .else_args = ValueList.init(),
            },
        }, null);
        return r.inst;
    }

    /// Branch table.
    pub fn brTable(self: *Self, index: Value, table: JumpTable) !Inst {
        const r = try self.insertInst(.{
            .branch_table = .{
                .opcode = .br_table,
                .arg = index,
                .table = table,
            },
        }, null);
        return r.inst;
    }

    /// Return from function.
    pub fn ret(self: *Self) !Inst {
        const r = try self.insertInst(.{
            .nullary = .{ .opcode = .@"return" },
        }, null);
        return r.inst;
    }

    /// Direct function call.
    pub fn call(self: *Self, func_ref: FuncRef, args: []const Value, result_type: ?Type) !struct { inst: Inst, result: ?Value } {
        var arg_list = ValueList.init();
        for (args) |v| {
            arg_list = try self.dfg.value_lists.push(arg_list, v);
        }
        return self.insertInst(.{
            .call = .{
                .opcode = .call,
                .func_ref = func_ref,
                .args = arg_list,
            },
        }, result_type);
    }

    /// Indirect function call.
    pub fn callIndirect(self: *Self, sig_ref: SigRef, callee: Value, args: []const Value, result_type: ?Type) !struct { inst: Inst, result: ?Value } {
        var arg_list = ValueList.init();
        for (args) |v| {
            arg_list = try self.dfg.value_lists.push(arg_list, v);
        }
        return self.insertInst(.{
            .call_indirect = .{
                .opcode = .call_indirect,
                .sig_ref = sig_ref,
                .callee = callee,
                .args = arg_list,
            },
        }, result_type);
    }

    // ========================================================================
    // Trap Instructions
    // ========================================================================

    /// Unconditional trap.
    pub fn trap(self: *Self, code: TrapCode) !Inst {
        const r = try self.insertInst(.{
            .trap = .{ .opcode = .trap, .code = code },
        }, null);
        return r.inst;
    }

    /// Trap if condition is non-zero.
    pub fn trapnz(self: *Self, cond: Value, code: TrapCode) !Inst {
        const r = try self.insertInst(.{
            .cond_trap = .{ .opcode = .trapnz, .arg = cond, .code = code },
        }, null);
        return r.inst;
    }

    /// Trap if condition is zero.
    pub fn trapz(self: *Self, cond: Value, code: TrapCode) !Inst {
        const r = try self.insertInst(.{
            .cond_trap = .{ .opcode = .trapz, .arg = cond, .code = code },
        }, null);
        return r.inst;
    }

    // ========================================================================
    // Integer Constants
    // ========================================================================

    /// Integer constant.
    pub fn iconst(self: *Self, ty: Type, imm: i64) !Value {
        const r = try self.insertInst(.{
            .unary_imm = .{ .opcode = .iconst, .imm = imm },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Float Constants
    // ========================================================================

    /// 32-bit float constant.
    pub fn f32const(self: *Self, imm: f32) !Value {
        const r = try self.insertInst(.{
            .unary_ieee32 = .{ .opcode = .f32const, .imm = imm },
        }, Type.F32);
        return r.result.?;
    }

    /// 64-bit float constant.
    pub fn f64const(self: *Self, imm: f64) !Value {
        const r = try self.insertInst(.{
            .unary_ieee64 = .{ .opcode = .f64const, .imm = imm },
        }, Type.F64);
        return r.result.?;
    }

    // ========================================================================
    // Integer Arithmetic
    // ========================================================================

    /// Copy a value.
    pub fn copy(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .copy, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Integer addition.
    pub fn iadd(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .iadd, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Integer subtraction.
    pub fn isub(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .isub, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Integer multiplication.
    pub fn imul(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .imul, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Integer negation.
    pub fn ineg(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .ineg, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Unsigned division.
    pub fn udiv(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .udiv, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Signed division.
    pub fn sdiv(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .sdiv, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Unsigned remainder.
    pub fn urem(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .urem, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Signed remainder.
    pub fn srem(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .srem, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    /// Bitwise AND.
    pub fn band(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .band, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Bitwise OR.
    pub fn bor(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .bor, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Bitwise XOR.
    pub fn bxor(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .bxor, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Bitwise NOT.
    pub fn bnot(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .bnot, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Shift left.
    pub fn ishl(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .ishl, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Unsigned shift right.
    pub fn ushr(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .ushr, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Signed shift right.
    pub fn sshr(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .sshr, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Rotate left.
    pub fn rotl(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .rotl, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Rotate right.
    pub fn rotr(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .rotr, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Comparison
    // ========================================================================

    /// Integer comparison.
    pub fn icmp(self: *Self, cond: IntCC, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .int_compare = .{ .opcode = .icmp, .cond = cond, .args = .{ a, b } },
        }, Type.I8);
        return r.result.?;
    }

    /// Float comparison.
    pub fn fcmp(self: *Self, cond: FloatCC, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .float_compare = .{ .opcode = .fcmp, .cond = cond, .args = .{ a, b } },
        }, Type.I8);
        return r.result.?;
    }

    // ========================================================================
    // Float Arithmetic
    // ========================================================================

    /// Float addition.
    pub fn fadd(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .fadd, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Float subtraction.
    pub fn fsub(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .fsub, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Float multiplication.
    pub fn fmul(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .fmul, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Float division.
    pub fn fdiv(self: *Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .fdiv, .args = .{ a, b } },
        }, ty);
        return r.result.?;
    }

    /// Float negation.
    pub fn fneg(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fneg, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Float absolute value.
    pub fn fabs(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fabs, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Float square root.
    pub fn sqrt(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .sqrt, .arg = arg },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Conversions
    // ========================================================================

    /// Zero-extend.
    pub fn uextend(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .uextend, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Sign-extend.
    pub fn sextend(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .sextend, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Reduce integer width.
    pub fn ireduce(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .ireduce, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Promote float.
    pub fn fpromote(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fpromote, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Demote float.
    pub fn fdemote(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fdemote, .arg = arg },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Memory Operations
    // ========================================================================

    /// Load from memory.
    pub fn load(self: *Self, ty: Type, flags: MemFlags, addr: Value, offset: i32) !Value {
        const r = try self.insertInst(.{
            .load = .{ .opcode = .load, .flags = flags, .arg = addr, .offset = offset },
        }, ty);
        return r.result.?;
    }

    /// Store to memory.
    pub fn store(self: *Self, flags: MemFlags, val: Value, addr: Value, offset: i32) !Inst {
        const r = try self.insertInst(.{
            .store = .{ .opcode = .store, .flags = flags, .args = .{ val, addr }, .offset = offset },
        }, null);
        return r.inst;
    }

    /// Load from stack slot.
    pub fn stackLoad(self: *Self, ty: Type, slot: StackSlot, offset: i32) !Value {
        const r = try self.insertInst(.{
            .stack_load = .{ .opcode = .stack_load, .slot = slot, .offset = offset },
        }, ty);
        return r.result.?;
    }

    /// Store to stack slot.
    pub fn stackStore(self: *Self, val: Value, slot: StackSlot, offset: i32) !Inst {
        const r = try self.insertInst(.{
            .stack_store = .{ .opcode = .stack_store, .arg = val, .slot = slot, .offset = offset },
        }, null);
        return r.inst;
    }

    // ========================================================================
    // Select
    // ========================================================================

    /// Conditional select.
    pub fn select(self: *Self, ty: Type, cond: Value, if_true: Value, if_false: Value) !Value {
        const r = try self.insertInst(.{
            .ternary = .{ .opcode = .select, .args = .{ cond, if_true, if_false } },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Miscellaneous
    // ========================================================================

    /// No operation.
    pub fn nop(self: *Self) !Inst {
        const r = try self.insertInst(.{
            .nullary = .{ .opcode = .nop },
        }, null);
        return r.inst;
    }

    /// Function address.
    pub fn funcAddr(self: *Self, ty: Type, func_ref: FuncRef) !Value {
        const r = try self.insertInst(.{
            .func_addr = .{ .opcode = .func_addr, .func_ref = func_ref },
        }, ty);
        return r.result.?;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "instruction data opcode" {
    const testing = std.testing;

    const data = InstructionData{ .nullary = .{ .opcode = .nop } };
    try testing.expectEqual(Opcode.nop, data.opcode());
    try testing.expectEqual(InstructionFormat.nullary, data.format());
}

test "mem flags" {
    const testing = std.testing;

    const flags = MemFlags.DEFAULT;
    try testing.expect(!flags.aligned);
    try testing.expect(!flags.readonly);

    const aligned = flags.withAligned();
    try testing.expect(aligned.aligned);
}

test "builder basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var dfg = DataFlowGraph.init(allocator);
    defer dfg.deinit();
    var layout = Layout.init();
    defer layout.deinit(allocator);

    var builder = FuncBuilder.init(allocator, &dfg, &layout);

    // Create a block
    const block0 = try builder.createBlock();
    try layout.appendBlock(allocator, block0);
    builder.switchToBlock(block0);

    // Add block parameter
    const param = try builder.appendBlockParam(block0, Type.I32);
    try testing.expect(param.index != Value.RESERVED.index);

    // Create iconst
    const c1 = try builder.iconst(Type.I32, 42);
    try testing.expect(c1.index != Value.RESERVED.index);

    // Create iadd
    const sum = try builder.iadd(Type.I32, param, c1);
    try testing.expect(sum.index != Value.RESERVED.index);

    // Check that we have instructions in the block
    var iter = layout.blockInsts(block0);
    const first = iter.next();
    try testing.expect(first != null);
    const second = iter.next();
    try testing.expect(second != null);
}
