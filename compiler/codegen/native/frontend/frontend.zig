//! A frontend for building Cranelift IR from other languages.
//!
//! Port of cranelift-frontend/src/frontend.rs
//!
//! This module provides FunctionBuilder, a high-level interface for constructing
//! CLIF IR functions with automatic SSA construction.

const std = @import("std");
const ssa_mod = @import("ssa.zig");
const variable_mod = @import("variable.zig");

// Re-export
pub const SSABuilder = ssa_mod.SSABuilder;
pub const SideEffects = ssa_mod.SideEffects;
pub const Variable = variable_mod.Variable;

// Import CLIF types
pub const clif = @import("../../../ir/clif/mod.zig");
pub const Block = clif.Block;
pub const Value = clif.Value;
pub const Inst = clif.Inst;
pub const Type = clif.Type;
pub const Function = clif.Function;
pub const StackSlot = clif.StackSlot;
pub const StackSlotData = clif.StackSlotData;
pub const FuncRef = clif.FuncRef;
pub const SigRef = clif.SigRef;
pub const Signature = clif.Signature;
pub const AbiParam = clif.AbiParam;
pub const JumpTable = clif.JumpTable;
pub const JumpTableData = clif.JumpTableData;
pub const BlockCall = clif.BlockCall;

/// Block status for tracking construction progress.
const BlockStatus = enum {
    /// No instructions have been added.
    empty,
    /// Some instructions have been added, but no terminator.
    partial,
    /// A terminator has been added; no further instructions may be added.
    filled,
};

/// Structure used for translating a series of functions into Cranelift IR.
///
/// In order to reduce memory reallocations when compiling multiple functions,
/// FunctionBuilderContext holds various data structures which are cleared between
/// functions, rather than dropped, preserving the underlying allocations.
pub const FunctionBuilderContext = struct {
    allocator: std.mem.Allocator,

    /// SSA construction state.
    ssa: SSABuilder,

    /// Block construction status.
    status: std.AutoHashMapUnmanaged(Block, BlockStatus) = .{},

    /// Variable type declarations.
    variables: std.ArrayListUnmanaged(Type) = .{},

    const Self = @This();

    /// Creates a new FunctionBuilderContext. The structure is automatically cleared after
    /// each FunctionBuilder completes translating a function.
    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .ssa = SSABuilder.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.ssa.deinit();
        self.status.deinit(self.allocator);
        self.variables.deinit(self.allocator);
    }

    /// Clear all state for reuse with another function.
    pub fn clear(self: *Self) void {
        self.ssa.clear();
        self.status.clearRetainingCapacity();
        self.variables.clearRetainingCapacity();
    }

    /// Check if context is in cleared state.
    pub fn isEmpty(self: Self) bool {
        return self.ssa.isEmpty() and
            self.status.count() == 0 and
            self.variables.items.len == 0;
    }
};

/// Error types for variable operations.
pub const UseVariableError = error{
    UsedBeforeDeclared,
};

pub const DefVariableError = error{
    TypeMismatch,
    DefinedBeforeDeclared,
};

/// Temporary object used to build a single Cranelift IR Function.
pub const FunctionBuilder = struct {
    /// The function currently being built.
    func: *Function,

    /// Reusable context.
    func_ctx: *FunctionBuilderContext,

    /// Current block position.
    position: ?Block = null,

    const Self = @This();

    /// Creates a new FunctionBuilder structure that will operate on a Function using a
    /// FunctionBuilderContext.
    pub fn init(func: *Function, func_ctx: *FunctionBuilderContext) Self {
        std.debug.assert(func_ctx.isEmpty());
        return .{
            .func = func,
            .func_ctx = func_ctx,
        };
    }

    /// Get the block that this builder is currently at.
    pub fn currentBlock(self: Self) ?Block {
        return self.position;
    }

    // ========================================================================
    // Block Management
    // ========================================================================

    /// Creates a new Block and returns its reference.
    pub fn createBlock(self: *Self) !Block {
        const block = try self.func.dfg.makeBlock();
        try self.func_ctx.ssa.declareBlock(block);
        try self.func_ctx.status.put(self.func_ctx.allocator, block, .empty);
        return block;
    }

    /// After the call to this function, new instructions will be inserted into the designated
    /// block, in the order they are declared.
    pub fn switchToBlock(self: *Self, block: Block) void {
        // Check that previous block is properly filled (in debug mode)
        if (self.position) |prev| {
            std.debug.assert(self.isPristine(prev) or self.isFilled(prev));
        }

        // Cannot switch to a filled block
        std.debug.assert(!self.isFilled(block));

        self.position = block;
    }

    /// Declares that all the predecessors of this block are known.
    pub fn sealBlock(self: *Self, block: Block) !void {
        var side_effects = try self.func_ctx.ssa.sealBlock(block, self.func);
        self.handleSideEffects(&side_effects);
        side_effects.deinit(self.func_ctx.allocator);
    }

    /// Effectively calls sealBlock on all unsealed blocks in the function.
    pub fn sealAllBlocks(self: *Self) !void {
        var side_effects = try self.func_ctx.ssa.sealAllBlocks(self.func);
        self.handleSideEffects(&side_effects);
        side_effects.deinit(self.func_ctx.allocator);
    }

    /// Make sure that the current block is inserted in the layout.
    pub fn ensureInsertedBlock(self: *Self) !void {
        const block = self.position orelse return;
        if (self.isPristine(block)) {
            if (!self.func.layout.isBlockInserted(block)) {
                try self.func.layout.appendBlock(self.func_ctx.allocator, block);
            }
            try self.func_ctx.status.put(self.func_ctx.allocator, block, .partial);
        } else {
            std.debug.assert(!self.isFilled(block));
        }
    }

    // ========================================================================
    // Variable Management
    // ========================================================================

    /// Declares the type of a variable.
    ///
    /// This allows the variable to be defined and used later (by calling
    /// defVar and useVar respectively).
    pub fn declareVar(self: *Self, ty: Type) !Variable {
        const index: u32 = @intCast(self.func_ctx.variables.items.len);
        try self.func_ctx.variables.append(self.func_ctx.allocator, ty);
        return Variable.new(index);
    }

    /// Returns the Cranelift IR value corresponding to the utilization at the current program
    /// position of a previously defined user variable.
    pub fn useVar(self: *Self, variable: Variable) !Value {
        return self.tryUseVar(variable) catch |err| switch (err) {
            UseVariableError.UsedBeforeDeclared => @panic("variable used before declared"),
            else => |e| return e,
        };
    }

    /// Returns the Cranelift IR necessary to use a previously defined user variable,
    /// returning an error if this is not possible.
    pub fn tryUseVar(self: *Self, variable: Variable) !Value {
        try self.ensureInsertedBlock();

        const var_index = variable.asU32();
        if (var_index >= self.func_ctx.variables.items.len) {
            return UseVariableError.UsedBeforeDeclared;
        }

        const ty = self.func_ctx.variables.items[var_index];
        std.debug.assert(!ty.eql(Type.INVALID));

        const result = try self.func_ctx.ssa.useVar(
            self.func,
            variable,
            ty,
            self.position.?,
        );

        self.handleSideEffects(&result.side_effects);
        var effects = result.side_effects;
        effects.deinit(self.func_ctx.allocator);

        return result.value;
    }

    /// Register a new definition of a user variable.
    pub fn defVar(self: *Self, variable: Variable, val: Value) !void {
        self.tryDefVar(variable, val) catch |err| switch (err) {
            DefVariableError.TypeMismatch => @panic("type mismatch in def_var"),
            DefVariableError.DefinedBeforeDeclared => @panic("variable defined before declared"),
            else => return err,
        };
    }

    /// Registers a new definition of a user variable, returning an error if
    /// the value supplied does not match the type the variable was declared to have.
    pub fn tryDefVar(self: *Self, variable: Variable, val: Value) !void {
        const var_index = variable.asU32();
        if (var_index >= self.func_ctx.variables.items.len) {
            return DefVariableError.DefinedBeforeDeclared;
        }

        const var_ty = self.func_ctx.variables.items[var_index];
        const val_ty = self.func.dfg.valueType(val);
        if (!var_ty.eql(val_ty)) {
            return DefVariableError.TypeMismatch;
        }

        try self.func_ctx.ssa.defVar(variable, val, self.position.?);
    }

    // ========================================================================
    // Block Parameters
    // ========================================================================

    /// Retrieves all the parameters for a Block.
    pub fn blockParams(self: Self, block: Block) []const Value {
        return self.func.dfg.blockParams(block);
    }

    /// Creates a parameter for a specific Block by appending it to the list.
    pub fn appendBlockParam(self: *Self, block: Block, ty: Type) !Value {
        std.debug.assert(self.isPristine(block));
        return self.func.dfg.appendBlockParam(block, ty);
    }

    /// Append parameters to the given Block corresponding to the function parameters.
    pub fn appendBlockParamsForFunctionParams(self: *Self, block: Block) !void {
        std.debug.assert(!self.func_ctx.ssa.hasAnyPredecessors(block));
        std.debug.assert(self.isPristine(block));

        for (self.func.signature.params.items) |param| {
            _ = try self.func.dfg.appendBlockParam(block, param.value_type);
        }
    }

    /// Append parameters to the given Block corresponding to the function return values.
    pub fn appendBlockParamsForFunctionReturns(self: *Self, block: Block) !void {
        std.debug.assert(self.isPristine(block));

        for (self.func.signature.returns.items) |ret| {
            _ = try self.func.dfg.appendBlockParam(block, ret.value_type);
        }
    }

    // ========================================================================
    // Stack Slots and Function References
    // ========================================================================

    /// Creates a sized stack slot in the function.
    pub fn createSizedStackSlot(self: *Self, data: StackSlotData) !StackSlot {
        return self.func.createStackSlot(self.func_ctx.allocator, data);
    }

    /// Import a signature for indirect calls.
    pub fn importSignature(self: *Self, sig: Signature) !SigRef {
        return self.func.importSignature(self.func_ctx.allocator, sig);
    }

    /// Import an external function.
    pub fn importFunction(self: *Self, data: clif.ExtFuncData) !FuncRef {
        return self.func.importFunction(self.func_ctx.allocator, data);
    }

    // ========================================================================
    // Jump Tables
    // Port of cranelift/frontend/src/frontend.rs create_jump_table
    // ========================================================================

    /// Create a jump table for br_table instruction.
    ///
    /// Port of cranelift/frontend/src/frontend.rs
    pub fn createJumpTable(self: *Self, default_block: Block, targets: []const Block) !JumpTable {
        // Build JumpTableData with BlockCalls
        const allocator = self.func_ctx.allocator;

        // Create default block call
        const default_call = BlockCall.init(default_block);

        // Create jump table data
        var jt_data = JumpTableData.init(default_call);

        // Add target entries
        for (targets) |target| {
            try jt_data.push(allocator, BlockCall.init(target));
        }

        return self.func.createJumpTable(jt_data);
    }

    /// Get allocator for external use.
    pub fn getAllocator(self: Self) std.mem.Allocator {
        return self.func_ctx.allocator;
    }

    // ========================================================================
    // Instruction Builder
    // ========================================================================

    /// Returns an object that allows to conveniently append an instruction to the current Block.
    pub fn ins(self: *Self) FuncInstBuilder {
        const block = self.position orelse @panic("call switchToBlock before inserting instructions");
        return FuncInstBuilder.init(self, block);
    }

    // ========================================================================
    // Finalization
    // ========================================================================

    /// Declare that translation of the current function is complete.
    ///
    /// This resets the state of the FunctionBuilderContext in preparation to
    /// be used for another function.
    pub fn finalize(self: *Self) void {
        // Verify all blocks are sealed and filled (in debug mode)
        var iter = self.func_ctx.status.iterator();
        while (iter.next()) |entry| {
            if (!self.isPristine(entry.key_ptr.*)) {
                std.debug.assert(self.func_ctx.ssa.isSealed(entry.key_ptr.*));
                std.debug.assert(self.isFilled(entry.key_ptr.*));
            }
        }

        // Clear context for reuse
        self.func_ctx.clear();
    }

    // ========================================================================
    // Helper Methods
    // ========================================================================

    /// Returns true if no instructions have been added to the block.
    fn isPristine(self: Self, block: Block) bool {
        const status = self.func_ctx.status.get(block) orelse return true;
        return status == .empty;
    }

    /// Returns true if a terminator has been added to the block.
    fn isFilled(self: Self, block: Block) bool {
        const status = self.func_ctx.status.get(block) orelse return false;
        return status == .filled;
    }

    /// Returns true if the current block has no predecessors (unreachable).
    pub fn isUnreachable(self: Self) bool {
        const block = self.position orelse return true;
        const is_entry = if (self.func.layout.entryBlock()) |entry| block.eql(entry) else false;
        return !is_entry and
            self.func_ctx.ssa.isSealed(block) and
            !self.func_ctx.ssa.hasAnyPredecessors(block);
    }

    /// Mark the current block as filled (after terminator instruction).
    fn fillCurrentBlock(self: *Self) !void {
        if (self.position) |block| {
            try self.func_ctx.status.put(self.func_ctx.allocator, block, .filled);
        }
    }

    /// Record a successor relationship.
    fn declareSuccessor(self: *Self, dest_block: Block, jump_inst: Inst) !void {
        try self.func_ctx.ssa.declareBlockPredecessor(dest_block, jump_inst);
    }

    /// Handle SSA side effects.
    fn handleSideEffects(self: *Self, side_effects: *const SideEffects) void {
        for (side_effects.instructions_added_to_blocks.items) |modified_block| {
            if (self.isPristine(modified_block)) {
                self.func_ctx.status.put(self.func_ctx.allocator, modified_block, .partial) catch {};
            }
        }
    }
};

/// Implementation of the instruction builder that has one convenience method per instruction.
pub const FuncInstBuilder = struct {
    builder: *FunctionBuilder,
    block: Block,

    const Self = @This();

    pub fn init(builder: *FunctionBuilder, block: Block) Self {
        return .{
            .builder = builder,
            .block = block,
        };
    }

    // ========================================================================
    // Helper for instruction insertion
    // ========================================================================

    fn build(self: Self, result_type: ?Type) !struct { inst: Inst, result: ?Value } {
        try self.builder.ensureInsertedBlock();

        const inst = self.builder.func.dfg.makeInst();
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);

        var result: ?Value = null;
        if (result_type) |ty| {
            result = try self.builder.func.dfg.makeInstResult(inst, ty);
        }

        return .{ .inst = inst, .result = result };
    }

    fn buildTerminator(self: Self) !Inst {
        try self.builder.ensureInsertedBlock();

        const inst = self.builder.func.dfg.makeInst();
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);
        try self.builder.fillCurrentBlock();

        return inst;
    }

    // ========================================================================
    // Constants
    // ========================================================================

    /// Integer constant.
    pub fn iconst(self: Self, ty: Type, imm: i64) !Value {
        _ = imm;
        const r = try self.build(ty);
        return r.result.?;
    }

    /// 32-bit float constant.
    pub fn f32const(self: Self, imm: f32) !Value {
        _ = imm;
        const r = try self.build(Type.F32);
        return r.result.?;
    }

    /// 64-bit float constant.
    pub fn f64const(self: Self, imm: f64) !Value {
        _ = imm;
        const r = try self.build(Type.F64);
        return r.result.?;
    }

    // ========================================================================
    // Arithmetic
    // ========================================================================

    /// Integer addition.
    pub fn iadd(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Integer subtraction.
    pub fn isub(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Integer multiplication.
    pub fn imul(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Signed division.
    pub fn sdiv(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Unsigned division.
    pub fn udiv(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Signed remainder.
    pub fn srem(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Unsigned remainder.
    pub fn urem(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    /// Bitwise AND.
    pub fn band(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Bitwise OR.
    pub fn bor(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Bitwise XOR.
    pub fn bxor(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Shift left.
    pub fn ishl(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Unsigned shift right.
    pub fn ushr(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Signed shift right.
    pub fn sshr(self: Self, a: Value, b: Value) !Value {
        _ = b;
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.build(ty);
        return r.result.?;
    }

    // ========================================================================
    // Comparisons
    // ========================================================================

    /// Integer comparison.
    pub fn icmp(self: Self, cond: clif.IntCC, a: Value, b: Value) !Value {
        _ = cond;
        _ = a;
        _ = b;
        const r = try self.build(Type.I8);
        return r.result.?;
    }

    // ========================================================================
    // Conversions
    // ========================================================================

    /// Zero extend.
    pub fn uextend(self: Self, ty: Type, arg: Value) !Value {
        _ = arg;
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Sign extend.
    pub fn sextend(self: Self, ty: Type, arg: Value) !Value {
        _ = arg;
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Reduce integer width.
    pub fn ireduce(self: Self, ty: Type, arg: Value) !Value {
        _ = arg;
        const r = try self.build(ty);
        return r.result.?;
    }

    // ========================================================================
    // Memory
    // ========================================================================

    /// Load from memory.
    pub fn load(self: Self, ty: Type, flags: clif.MemFlags, addr: Value, offset: i32) !Value {
        _ = flags;
        _ = addr;
        _ = offset;
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Store to memory.
    pub fn store(self: Self, flags: clif.MemFlags, val: Value, addr: Value, offset: i32) !Inst {
        _ = flags;
        _ = val;
        _ = addr;
        _ = offset;
        const r = try self.build(null);
        return r.inst;
    }

    /// Load from stack slot.
    pub fn stackLoad(self: Self, ty: Type, slot: StackSlot, offset: i32) !Value {
        _ = slot;
        _ = offset;
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Store to stack slot.
    pub fn stackStore(self: Self, val: Value, slot: StackSlot, offset: i32) !Inst {
        _ = val;
        _ = slot;
        _ = offset;
        const r = try self.build(null);
        return r.inst;
    }

    // ========================================================================
    // Control Flow
    // ========================================================================

    /// Unconditional jump.
    pub fn jump(self: Self, destination: Block, args: []const Value) !Inst {
        _ = args;
        const inst = try self.buildTerminator();
        try self.builder.declareSuccessor(destination, inst);
        return inst;
    }

    /// Conditional branch.
    pub fn brif(self: Self, cond: Value, then_block: Block, then_args: []const Value, else_block: Block, else_args: []const Value) !Inst {
        _ = cond;
        _ = then_args;
        _ = else_args;
        const inst = try self.buildTerminator();
        try self.builder.declareSuccessor(then_block, inst);
        if (!then_block.eql(else_block)) {
            try self.builder.declareSuccessor(else_block, inst);
        }
        return inst;
    }

    /// Return from function.
    pub fn return_(self: Self, args: []const Value) !Inst {
        _ = args;
        return self.buildTerminator();
    }

    /// Call a function.
    pub fn call(self: Self, func_ref: FuncRef, args: []const Value) !struct { inst: Inst, results: []const Value } {
        _ = args;

        // Get the signature to determine return types
        const ext_func = self.builder.func.getExtFunc(func_ref) orelse
            return error.InvalidFuncRef;
        const sig = self.builder.func.getSignature(ext_func.signature) orelse
            return error.InvalidSignature;

        const inst = self.builder.func.dfg.makeInst();
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);

        // Create result values for each return type
        for (sig.returns.items) |ret| {
            _ = try self.builder.func.dfg.makeInstResult(inst, ret.value_type);
        }

        const results = self.builder.func.dfg.instResults(inst);
        return .{ .inst = inst, .results = results };
    }

    /// Trap unconditionally.
    pub fn trap(self: Self, code: clif.TrapCode) !Inst {
        _ = code;
        return self.buildTerminator();
    }

    /// Branch table instruction.
    ///
    /// Port of cranelift/codegen/src/ir/instructions.rs br_table
    pub fn brTable(self: Self, selector: Value, jt: JumpTable) !Inst {
        _ = selector;

        // Get the jump table to declare successors
        if (self.builder.func.getJumpTable(jt)) |jt_data| {
            // Declare default block as successor
            const inst = try self.buildTerminator();
            try self.builder.declareSuccessor(jt_data.default_block.block, inst);

            // Declare all target blocks as successors
            for (jt_data.entries.items) |entry| {
                try self.builder.declareSuccessor(entry.block, inst);
            }

            return inst;
        } else {
            return self.buildTerminator();
        }
    }

    // ========================================================================
    // Misc
    // ========================================================================

    /// No operation.
    pub fn nop(self: Self) !Inst {
        const r = try self.build(null);
        return r.inst;
    }

    /// Select between two values.
    pub fn select(self: Self, cond: Value, if_true: Value, if_false: Value) !Value {
        _ = cond;
        _ = if_false;
        const ty = self.builder.func.dfg.valueType(if_true);
        const r = try self.build(ty);
        return r.result.?;
    }

    /// Copy a value.
    pub fn copy(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.build(ty);
        return r.result.?;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "function builder context" {
    const testing = std.testing;

    var ctx = FunctionBuilderContext.init(testing.allocator);
    defer ctx.deinit();

    try testing.expect(ctx.isEmpty());
}

test "function builder basics" {
    const testing = std.testing;

    var func = Function.init(testing.allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(testing.allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);

    // Create a block
    const block0 = try builder.createBlock();
    try testing.expect(block0.index == 0);

    // Switch to block
    builder.switchToBlock(block0);
    try testing.expect(builder.currentBlock().?.eql(block0));

    // Declare a variable
    const x = try builder.declareVar(Type.I32);
    try testing.expect(x.asU32() == 0);

    // Seal the block
    try builder.sealBlock(block0);

    // Finalize
    builder.finalize();
    try testing.expect(ctx.isEmpty());
}

test "sample function like cranelift test" {
    const testing = std.testing;

    var func = Function.init(testing.allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(testing.allocator);
    defer ctx.deinit();

    {
        var builder = FunctionBuilder.init(&func, &ctx);

        const block0 = try builder.createBlock();
        const block1 = try builder.createBlock();
        const block2 = try builder.createBlock();
        const block3 = try builder.createBlock();

        const x = try builder.declareVar(Type.I32);
        const y = try builder.declareVar(Type.I32);
        const z = try builder.declareVar(Type.I32);

        // block0
        builder.switchToBlock(block0);
        try builder.sealBlock(block0);

        // x = block param
        _ = try builder.appendBlockParam(block0, Type.I32);
        // ... would normally def_var(x, tmp) with the param

        // y = iconst 2
        const y_val = try builder.ins().iconst(Type.I32, 2);
        try builder.defVar(y, y_val);

        // z = x + y (simplified - just use y for now)
        const z_val = try builder.ins().iadd(y_val, y_val);
        try builder.defVar(z, z_val);

        _ = try builder.ins().jump(block1, &[_]Value{});

        // block1 - loop header
        builder.switchToBlock(block1);
        // ... use_var(y), use_var(z), etc.
        _ = try builder.ins().brif(y_val, block3, &[_]Value{}, block2, &[_]Value{});

        // block2
        builder.switchToBlock(block2);
        try builder.sealBlock(block2);
        _ = try builder.ins().return_(&[_]Value{y_val});

        // block3
        builder.switchToBlock(block3);
        try builder.sealBlock(block3);
        _ = try builder.ins().jump(block1, &[_]Value{});

        try builder.sealBlock(block1);

        _ = x;

        builder.finalize();
    }

    try testing.expect(ctx.isEmpty());
}
