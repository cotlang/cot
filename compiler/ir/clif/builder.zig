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
pub const ValueListPool = dfg_mod.ValueListPool;
pub const Type = types.Type;
pub const IntCC = instructions.IntCC;
pub const FloatCC = instructions.FloatCC;
pub const TrapCode = instructions.TrapCode;
pub const Opcode = instructions.Opcode;
pub const FuncRef = dfg_mod.FuncRef;
pub const SigRef = dfg_mod.SigRef;
pub const StackSlot = dfg_mod.StackSlot;
pub const JumpTable = dfg_mod.JumpTable;
pub const JumpTables = dfg_mod.JumpTables;
pub const GlobalValue = dfg_mod.GlobalValue;
pub const BlockCall = dfg_mod.BlockCall;

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
    /// Global value computation.
    /// Port of cranelift UnaryGlobalValue format.
    unary_global_value,
    /// Multiple values (return with args, etc.).
    multi_ary,
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
    /// Port of cranelift Jump format - destination is a BlockCall bundling block + args.
    jump: struct {
        opcode: Opcode,
        destination: BlockCall,
    },

    /// Conditional branch (if-then-else).
    /// Port of cranelift Brif format - blocks is array of 2 BlockCalls.
    /// blocks[0] = then branch, blocks[1] = else branch.
    brif: struct {
        opcode: Opcode,
        arg: Value,
        blocks: [2]BlockCall,
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

    /// Multiple values (return with args, etc.).
    multi_ary: struct {
        opcode: Opcode,
        args: ValueList,
    },

    /// Function address.
    func_addr: struct {
        opcode: Opcode,
        func_ref: FuncRef,
    },

    /// Global value computation.
    ///
    /// Port of cranelift UnaryGlobalValue format.
    /// The global_value instruction takes a GlobalValue reference and
    /// produces the computed address as a Value.
    unary_global_value: struct {
        opcode: Opcode,
        global_value: GlobalValue,
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
            .unary_global_value => |d| d.opcode,
            .multi_ary => |d| d.opcode,
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
            .unary_global_value => .unary_global_value,
            .multi_ary => .multi_ary,
        };
    }

    /// Get the destinations of this instruction, if it's a branch.
    ///
    /// Port of cranelift/codegen/src/ir/instructions.rs:426-443
    /// Returns a slice of BlockCalls for all branch targets.
    /// - Jump: returns single-element slice with destination
    /// - Brif: returns 2-element slice [then, else]
    /// - BranchTable: returns all branches from jump table
    /// - Other: returns empty slice
    pub fn branchDestination(self: *const Self, jump_tables: *const JumpTables) []const BlockCall {
        return switch (self.*) {
            .jump => |*d| @as(*const [1]BlockCall, &d.destination)[0..1],
            .brif => |*d| &d.blocks,
            .branch_table => |d| blk: {
                if (jump_tables.get(d.table)) |jt| {
                    break :blk jt.allBranches();
                }
                break :blk &[_]BlockCall{};
            },
            else => &[_]BlockCall{},
        };
    }

    /// Get the destinations of this instruction mutably.
    ///
    /// Port of cranelift/codegen/src/ir/instructions.rs:448-470
    pub fn branchDestinationMut(self: *Self, jump_tables: *JumpTables) []BlockCall {
        return switch (self.*) {
            .jump => |*d| @as(*[1]BlockCall, &d.destination)[0..1],
            .brif => |*d| &d.blocks,
            .branch_table => |d| blk: {
                if (jump_tables.getMut(d.table)) |jt| {
                    break :blk jt.allBranchesMut();
                }
                break :blk &[_]BlockCall{};
            },
            else => &[_]BlockCall{},
        };
    }

    // ========================================================================
    // Accessor methods for instruction data fields
    // ========================================================================

    /// Get the immediate value (i64) if present.
    pub fn getImmediate(self: Self) ?i64 {
        return switch (self) {
            .unary_imm => |d| d.imm,
            .binary_imm64 => |d| d.imm,
            else => null,
        };
    }

    /// Get the immediate value as unsigned if present.
    pub fn getImmediateUnsigned(self: Self) ?u64 {
        return if (self.getImmediate()) |v| @bitCast(v) else null;
    }

    /// Get the integer comparison condition code.
    pub fn getIntCC(self: Self) ?IntCC {
        return switch (self) {
            .int_compare => |d| d.cond,
            else => null,
        };
    }

    /// Get the float comparison condition code.
    pub fn getFloatCC(self: Self) ?FloatCC {
        return switch (self) {
            .float_compare => |d| d.cond,
            else => null,
        };
    }

    /// Get the jump table reference.
    pub fn getJumpTable(self: Self) ?JumpTable {
        return switch (self) {
            .branch_table => |d| d.table,
            else => null,
        };
    }

    /// Get the stack slot reference.
    pub fn getStackSlot(self: Self) ?StackSlot {
        return switch (self) {
            .stack_load => |d| d.slot,
            .stack_store => |d| d.slot,
            else => null,
        };
    }

    /// Get the stack/memory offset.
    pub fn getOffset(self: Self) ?i32 {
        return switch (self) {
            .stack_load => |d| d.offset,
            .stack_store => |d| d.offset,
            .load => |d| d.offset,
            .store => |d| d.offset,
            else => null,
        };
    }

    /// Get the function reference.
    pub fn getFuncRef(self: Self) ?FuncRef {
        return switch (self) {
            .call => |d| d.func_ref,
            .func_addr => |d| d.func_ref,
            else => null,
        };
    }

    /// Get the signature reference.
    pub fn getSigRef(self: Self) ?SigRef {
        return switch (self) {
            .call_indirect => |d| d.sig_ref,
            else => null,
        };
    }

    /// Get the global value reference.
    pub fn getGlobalValue(self: Self) ?GlobalValue {
        return switch (self) {
            .unary_global_value => |d| d.global_value,
            else => null,
        };
    }

    /// Get the trap code.
    pub fn getTrapCode(self: Self) ?TrapCode {
        return switch (self) {
            .trap => |d| d.code,
            .cond_trap => |d| d.code,
            else => null,
        };
    }

    /// Get the single block destination (for jump instructions).
    pub fn getBlockDest(self: Self) ?Block {
        return switch (self) {
            .jump => |d| d.destination.getBlock(),
            else => null,
        };
    }

    /// Get brif destinations (then and else blocks).
    pub fn getBrifDests(self: Self) ?struct { then_dest: Block, else_dest: Block } {
        return switch (self) {
            .brif => |d| .{
                .then_dest = d.blocks[0].getBlock(),
                .else_dest = d.blocks[1].getBlock(),
            },
            else => null,
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
    /// Now directly stores InstructionData in the DFG, matching Cranelift's design.
    fn insertInst(self: *Self, data: InstructionData, result_type: ?Type) !struct { inst: Inst, result: ?Value } {
        const block = self.current_block orelse return error.NoCurrentBlock;

        // Create instruction with data - directly stores InstructionData like Cranelift
        const inst = try self.dfg.makeInst(data);
        try self.layout.appendInst(self.allocator, inst, block);

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
        // Create BlockCall bundling destination block and arguments
        const dest_call = try BlockCall.withArgsSlice(destination, args, &self.dfg.value_lists);
        const r = try self.insertInst(.{
            .jump = .{
                .opcode = .jump,
                .destination = dest_call,
            },
        }, null);
        return r.inst;
    }

    /// Conditional branch.
    pub fn brif(self: *Self, cond: Value, then_block: Block, else_block: Block) !Inst {
        // Create BlockCalls with empty args - args can be added later with appendBranchArg
        const then_call = BlockCall.init(then_block);
        const else_call = BlockCall.init(else_block);
        const r = try self.insertInst(.{
            .brif = .{
                .opcode = .brif,
                .arg = cond,
                .blocks = .{ then_call, else_call },
            },
        }, null);
        return r.inst;
    }

    /// Conditional branch with args.
    pub fn brifWithArgs(
        self: *Self,
        cond: Value,
        then_block: Block,
        then_args: []const Value,
        else_block: Block,
        else_args: []const Value,
    ) !Inst {
        const then_call = try BlockCall.withArgsSlice(then_block, then_args, &self.dfg.value_lists);
        const else_call = try BlockCall.withArgsSlice(else_block, else_args, &self.dfg.value_lists);
        const r = try self.insertInst(.{
            .brif = .{
                .opcode = .brif,
                .arg = cond,
                .blocks = .{ then_call, else_call },
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
        // Cranelift pattern: callee is the first element of args, then call arguments.
        // instArgs()/numInputs() return [callee, arg0, arg1, ...].
        // The lowering expects input 0 = callee, inputs 1.. = call args.
        var arg_list = ValueList.init();
        arg_list = try self.dfg.value_lists.push(arg_list, callee);
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

    /// Integer add immediate.
    ///
    /// Port of cranelift iadd_imm instruction.
    /// Uses binary_imm64 format: one value operand + one i64 immediate.
    pub fn iaddImm(self: *Self, ty: Type, arg: Value, imm: i64) !Value {
        const r = try self.insertInst(.{
            .binary_imm64 = .{ .opcode = .iadd_imm, .arg = arg, .imm = imm },
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

    /// Atomic load with acquire semantics.
    pub fn atomicLoad(self: *Self, ty: Type, addr: Value) !Value {
        const r = try self.insertInst(.{
            .load = .{ .opcode = .atomic_load, .flags = MemFlags.DEFAULT, .arg = addr, .offset = 0 },
        }, ty);
        return r.result.?;
    }

    /// Atomic store with release semantics.
    pub fn atomicStore(self: *Self, val: Value, addr: Value) !Inst {
        const r = try self.insertInst(.{
            .store = .{ .opcode = .atomic_store, .flags = MemFlags.DEFAULT, .args = .{ val, addr }, .offset = 0 },
        }, null);
        return r.inst;
    }

    /// Atomic read-modify-write add. Returns previous value.
    pub fn atomicRmwAdd(self: *Self, ty: Type, addr: Value, val: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .atomic_rmw_add, .args = .{ addr, val } },
        }, ty);
        return r.result.?;
    }

    /// Atomic read-modify-write exchange. Returns previous value.
    pub fn atomicRmwXchg(self: *Self, ty: Type, addr: Value, val: Value) !Value {
        const r = try self.insertInst(.{
            .binary = .{ .opcode = .atomic_rmw_xchg, .args = .{ addr, val } },
        }, ty);
        return r.result.?;
    }

    /// Atomic compare-and-swap. Returns actual old value.
    pub fn atomicCas(self: *Self, ty: Type, addr: Value, expected: Value, new_val: Value) !Value {
        const r = try self.insertInst(.{
            .ternary = .{ .opcode = .atomic_cas, .args = .{ addr, expected, new_val } },
        }, ty);
        return r.result.?;
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

    /// Get the address of a stack slot.
    ///
    /// Compute the absolute address of a byte in a stack slot.
    /// Returns a pointer-sized value (iAddr / I64).
    ///
    /// Port of cranelift stack_addr instruction.
    /// Reference: cranelift/codegen/meta/src/shared/instructions.rs:1238-1253
    /// Reference: cranelift/codegen/src/machinst/abi.rs:2160-2170 (sized_stackslot_addr)
    /// Uses stack_load InstructionData format (same fields: slot + offset).
    pub fn stackAddr(self: *Self, ty: Type, slot: StackSlot, offset: i32) !Value {
        const r = try self.insertInst(.{
            .stack_load = .{ .opcode = .stack_addr, .slot = slot, .offset = offset },
        }, ty);
        return r.result.?;
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

    // ========================================================================
    // Global Values
    // Port of cranelift/codegen/src/ir/builder.rs global_value
    // ========================================================================

    /// Compute the value of a global variable.
    ///
    /// The `gv` argument must refer to a `GlobalValue` that has been declared
    /// in the function using `Function.createGlobalValue()`. This instruction
    /// produces a value containing the computed address of the global value.
    ///
    /// Port of cranelift InstBuilder::global_value
    pub fn globalValue(self: *Self, ty: Type, gv: GlobalValue) !Value {
        const r = try self.insertInst(.{
            .unary_global_value = .{ .opcode = .global_value, .global_value = gv },
        }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Float-Int Conversions
    // Port of cranelift conversion instructions
    // ========================================================================

    /// Convert float to signed integer (truncating toward zero).
    pub fn fcvtToSint(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fcvt_to_sint, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Convert float to unsigned integer (truncating toward zero).
    pub fn fcvtToUint(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fcvt_to_uint, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Convert signed integer to float.
    pub fn fcvtFromSint(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fcvt_from_sint, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Convert unsigned integer to float.
    pub fn fcvtFromUint(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .fcvt_from_uint, .arg = arg },
        }, ty);
        return r.result.?;
    }

    /// Bitwise reinterpret of value without conversion.
    pub fn bitcast(self: *Self, ty: Type, arg: Value) !Value {
        const r = try self.insertInst(.{
            .unary = .{ .opcode = .bitcast, .arg = arg },
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
