//! A frontend for building Cranelift IR from other languages.
//!
//! Port of cranelift-frontend/src/frontend.rs
//!
//! This module provides FunctionBuilder, a high-level interface for constructing
//! CLIF IR functions with automatic SSA construction.

const std = @import("std");
const ssa_mod = @import("ssa.zig");
const variable_mod = @import("variable.zig");
const pipeline_debug = @import("../../../pipeline_debug.zig");

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
pub const GlobalValue = clif.GlobalValue;
pub const GlobalValueData = clif.GlobalValueData;
pub const FloatCC = clif.FloatCC;

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
    /// Port of cranelift-frontend/src/frontend.rs create_block()
    pub fn createBlock(self: *Self) !Block {
        const block = try self.func.dfg.makeBlock();
        try self.func_ctx.ssa.declareBlock(block);
        try self.func_ctx.status.put(self.func_ctx.allocator, block, .empty);
        pipeline_debug.log(.codegen, "createBlock: created block {d} in DFG (status=empty, NOT in Layout yet)", .{block.index});
        return block;
    }

    /// After the call to this function, new instructions will be inserted into the designated
    /// block, in the order they are declared.
    /// Port of cranelift-frontend/src/frontend.rs switch_to_block()
    pub fn switchToBlock(self: *Self, block: Block) void {
        // Check that previous block is properly filled (in debug mode)
        if (self.position) |prev| {
            std.debug.assert(self.isPristine(prev) or self.isFilled(prev));
            pipeline_debug.log(.codegen, "switchToBlock: switching from block {d} to block {d}", .{ prev.index, block.index });
        } else {
            pipeline_debug.log(.codegen, "switchToBlock: switching to block {d} (no previous block)", .{block.index});
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

    /// Change the destination of a branch instruction from old_block to new_block.
    /// Port of cranelift-frontend/src/frontend.rs:777-788
    pub fn changeJumpDestination(self: *Self, inst: Inst, old_block: Block, new_block: Block) !void {
        // Get the instruction data mutably
        const inst_data = self.func.dfg.getInstMut(inst);

        // Iterate over branch destinations and change matching ones
        const destinations = inst_data.branchDestinationMut(&self.func.dfg.jump_tables);
        for (destinations) |*dest| {
            if (dest.block.eql(old_block)) {
                // Remove old_block as predecessor
                self.func_ctx.ssa.removeBlockPredecessor(old_block, inst);
                // Change destination
                dest.block = new_block;
                // Add new_block as predecessor
                try self.func_ctx.ssa.declareBlockPredecessor(new_block, inst);
            }
        }
    }

    /// Make sure that the current block is inserted in the layout.
    /// Port of cranelift-frontend/src/frontend.rs ensure_inserted_block()
    pub fn ensureInsertedBlock(self: *Self) !void {
        const block = self.position orelse {
            pipeline_debug.log(.codegen, "ensureInsertedBlock: position is NULL, returning early", .{});
            return;
        };

        if (self.isPristine(block)) {
            if (!self.func.layout.isBlockInserted(block)) {
                pipeline_debug.log(.codegen, "ensureInsertedBlock: adding block {d} to Layout (was pristine, not in layout)", .{block.index});
                try self.func.layout.appendBlock(self.func_ctx.allocator, block);
            } else {
                pipeline_debug.log(.codegen, "ensureInsertedBlock: block {d} already in Layout (was pristine)", .{block.index});
            }
            try self.func_ctx.status.put(self.func_ctx.allocator, block, .partial);
        } else {
            pipeline_debug.log(.codegen, "ensureInsertedBlock: block {d} not pristine, skipping layout insert", .{block.index});
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

        // Create jump table data with default block
        var jt_data = try JumpTableData.init(allocator, default_call);

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
            const block = entry.key_ptr.*;
            const status = entry.value_ptr.*;

            if (!self.isPristine(block)) {
                std.debug.assert(self.func_ctx.ssa.isSealed(block));
                std.debug.assert(self.isFilled(block));

                // F3 Fix: Verify non-pristine blocks are in Layout
                // This catches the bug where blocks have instructions but aren't compiled
                if (status == .partial or status == .filled) {
                    const is_in_layout = self.func.layout.isBlockInserted(block);
                    if (!is_in_layout) {
                        pipeline_debug.log(.codegen, "FINALIZE ERROR: Block {d} has status {s} but is NOT in Layout!", .{
                            block.index,
                            @tagName(status),
                        });
                    }
                    std.debug.assert(is_in_layout);
                }
            }
        }

        // Log final Layout state
        const layout_entry = self.func.layout.entryBlock();
        if (layout_entry) |entry| {
            pipeline_debug.log(.codegen, "finalize: Layout entry block = {d}", .{entry.index});
        } else {
            pipeline_debug.log(.codegen, "finalize: WARNING - Layout has no entry block!", .{});
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
    // Port of Cranelift - uses InstructionData (typed union) directly
    // ========================================================================

    /// Result type for instruction building
    const BuildResult = struct { inst: Inst, result: ?Value };

    /// Build instruction with proper InstructionData union variant.
    fn buildInst(self: Self, data: clif.InstructionData, result_type: ?Type) !BuildResult {
        try self.builder.ensureInsertedBlock();

        const inst = try self.builder.func.dfg.makeInst(data);
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);

        var result: ?Value = null;
        if (result_type) |ty| {
            result = try self.builder.func.dfg.makeInstResult(inst, ty);
        }

        return .{ .inst = inst, .result = result };
    }

    /// Build terminator instruction.
    fn buildTerminator(self: Self, data: clif.InstructionData) !Inst {
        try self.builder.ensureInsertedBlock();

        const inst = try self.builder.func.dfg.makeInst(data);
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);
        try self.builder.fillCurrentBlock();

        return inst;
    }

    // ========================================================================
    // Format Constructors - create proper InstructionData union variants
    // Port of Cranelift's gen_inst.rs pattern
    // ========================================================================

    /// UnaryImm format: immediate only (iconst)
    fn UnaryImm(self: Self, opcode: clif.Opcode, ty: Type, imm: i64) !BuildResult {
        return self.buildInst(.{ .unary_imm = .{ .opcode = opcode, .imm = imm } }, ty);
    }

    /// UnaryIeee32 format: 32-bit float immediate (f32const)
    fn UnaryIeee32(self: Self, opcode: clif.Opcode, imm: f32) !BuildResult {
        return self.buildInst(.{ .unary_ieee32 = .{ .opcode = opcode, .imm = imm } }, Type.F32);
    }

    /// UnaryIeee64 format: 64-bit float immediate (f64const)
    fn UnaryIeee64(self: Self, opcode: clif.Opcode, imm: f64) !BuildResult {
        return self.buildInst(.{ .unary_ieee64 = .{ .opcode = opcode, .imm = imm } }, Type.F64);
    }

    /// Unary format: single value operand (ineg, fneg, copy, etc.)
    fn Unary(self: Self, opcode: clif.Opcode, ty: Type, arg: Value) !BuildResult {
        return self.buildInst(.{ .unary = .{ .opcode = opcode, .arg = arg } }, ty);
    }

    /// Binary format: two value operands (iadd, isub, imul, etc.)
    fn Binary(self: Self, opcode: clif.Opcode, ty: Type, a: Value, b: Value) !BuildResult {
        return self.buildInst(.{ .binary = .{ .opcode = opcode, .args = .{ a, b } } }, ty);
    }

    /// BinaryImm64 format: one value operand + one i64 immediate (iadd_imm, etc.)
    fn BinaryImm64(self: Self, opcode: clif.Opcode, ty: Type, arg: Value, imm: i64) !BuildResult {
        return self.buildInst(.{ .binary_imm64 = .{ .opcode = opcode, .arg = arg, .imm = imm } }, ty);
    }

    /// IntCompare format: integer comparison (icmp)
    fn IntCompare(self: Self, opcode: clif.Opcode, cond: clif.IntCC, a: Value, b: Value) !BuildResult {
        return self.buildInst(.{ .int_compare = .{ .opcode = opcode, .cond = cond, .args = .{ a, b } } }, Type.I8);
    }

    /// FloatCompare format: floating-point comparison (fcmp)
    fn FloatCompare(self: Self, opcode: clif.Opcode, cond: FloatCC, a: Value, b: Value) !BuildResult {
        return self.buildInst(.{ .float_compare = .{ .opcode = opcode, .cond = cond, .args = .{ a, b } } }, Type.I8);
    }

    /// Load format: memory load
    fn Load(self: Self, opcode: clif.Opcode, ty: Type, flags: clif.MemFlags, addr: Value, offset: i32) !BuildResult {
        return self.buildInst(.{ .load = .{ .opcode = opcode, .flags = flags, .arg = addr, .offset = offset } }, ty);
    }

    /// Store format: memory store (no result)
    fn Store(self: Self, opcode: clif.Opcode, flags: clif.MemFlags, val: Value, addr: Value, offset: i32) !BuildResult {
        return self.buildInst(.{ .store = .{ .opcode = opcode, .flags = flags, .args = .{ val, addr }, .offset = offset } }, null);
    }

    /// StackLoad format: load from stack slot
    fn StackLoad(self: Self, opcode: clif.Opcode, ty: Type, slot: StackSlot, offset: i32) !BuildResult {
        return self.buildInst(.{ .stack_load = .{ .opcode = opcode, .slot = slot, .offset = offset } }, ty);
    }

    /// StackStore format: store to stack slot (no result)
    fn StackStore(self: Self, opcode: clif.Opcode, val: Value, slot: StackSlot, offset: i32) !BuildResult {
        return self.buildInst(.{ .stack_store = .{ .opcode = opcode, .arg = val, .slot = slot, .offset = offset } }, null);
    }

    // Terminator format constructors

    /// Jump format: unconditional jump with block arguments
    /// Creates proper InstructionData.jump with BlockCall bundling block + args
    fn JumpFmt(self: Self, opcode: clif.Opcode, destination: Block, args: []const Value) !Inst {
        const block_call = try self.builder.func.dfg.blockCall(destination, args);
        return self.buildTerminator(.{ .jump = .{ .opcode = opcode, .destination = block_call } });
    }

    /// Brif format: conditional branch with BlockCalls
    /// Creates proper InstructionData.brif with [2]BlockCall array
    fn BrifFmt(self: Self, opcode: clif.Opcode, cond: Value, then_block: Block, then_args: []const Value, else_block: Block, else_args: []const Value) !Inst {
        const then_call = try self.builder.func.dfg.blockCall(then_block, then_args);
        const else_call = try self.builder.func.dfg.blockCall(else_block, else_args);
        return self.buildTerminator(.{ .brif = .{ .opcode = opcode, .arg = cond, .blocks = .{ then_call, else_call } } });
    }

    /// Nullary terminator format: return, trap
    fn NullaryTerminator(self: Self, opcode: clif.Opcode) !Inst {
        return self.buildTerminator(.{ .nullary = .{ .opcode = opcode } });
    }

    /// MultiAry terminator format: return with values
    fn MultiAryTerminator(self: Self, opcode: clif.Opcode, args: []const Value) !Inst {
        const vlist = try self.builder.func.dfg.value_lists.alloc(args);
        return self.buildTerminator(.{ .multi_ary = .{ .opcode = opcode, .args = vlist } });
    }

    // ========================================================================
    // Constants
    // ========================================================================

    /// Integer constant.
    pub fn iconst(self: Self, ty: Type, imm: i64) !Value {
        const r = try self.UnaryImm(.iconst, ty, imm);
        return r.result.?;
    }

    /// 32-bit float constant.
    pub fn f32const(self: Self, imm: f32) !Value {
        const r = try self.UnaryIeee32(.f32const, imm);
        return r.result.?;
    }

    /// 64-bit float constant.
    pub fn f64const(self: Self, imm: f64) !Value {
        const r = try self.UnaryIeee64(.f64const, imm);
        return r.result.?;
    }

    // ========================================================================
    // Arithmetic
    // ========================================================================

    /// Integer addition.
    pub fn iadd(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.iadd, ty, a, b);
        return r.result.?;
    }

    /// Integer subtraction.
    pub fn isub(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.isub, ty, a, b);
        return r.result.?;
    }

    /// Integer multiplication.
    pub fn imul(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.imul, ty, a, b);
        return r.result.?;
    }

    /// Signed division.
    pub fn sdiv(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.sdiv, ty, a, b);
        return r.result.?;
    }

    /// Unsigned division.
    pub fn udiv(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.udiv, ty, a, b);
        return r.result.?;
    }

    /// Signed remainder.
    pub fn srem(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.srem, ty, a, b);
        return r.result.?;
    }

    /// Unsigned remainder.
    pub fn urem(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.urem, ty, a, b);
        return r.result.?;
    }

    /// Integer negation.
    ///
    /// Port of cranelift ineg instruction.
    pub fn ineg(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.Unary(.ineg, ty, arg);
        return r.result.?;
    }

    /// Integer add immediate.
    ///
    /// Port of cranelift iadd_imm instruction.
    /// Uses binary_imm64 format: one value operand + one i64 immediate.
    pub fn iaddImm(self: Self, arg: Value, imm: i64) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.BinaryImm64(.iadd_imm, ty, arg, imm);
        return r.result.?;
    }

    // ========================================================================
    // Bitwise Operations
    // ========================================================================

    /// Bitwise AND.
    pub fn band(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.band, ty, a, b);
        return r.result.?;
    }

    /// Bitwise OR.
    pub fn bor(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.bor, ty, a, b);
        return r.result.?;
    }

    /// Bitwise XOR.
    pub fn bxor(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.bxor, ty, a, b);
        return r.result.?;
    }

    /// Bitwise NOT.
    ///
    /// Port of cranelift bnot instruction.
    pub fn bnot(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.Unary(.bnot, ty, arg);
        return r.result.?;
    }

    /// Shift left.
    pub fn ishl(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.ishl, ty, a, b);
        return r.result.?;
    }

    /// Unsigned shift right.
    pub fn ushr(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.ushr, ty, a, b);
        return r.result.?;
    }

    /// Signed shift right.
    pub fn sshr(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.sshr, ty, a, b);
        return r.result.?;
    }

    /// Rotate left.
    pub fn rotl(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.rotl, ty, a, b);
        return r.result.?;
    }

    /// Rotate right.
    pub fn rotr(self: Self, a: Value, b: Value) !Value {
        const ty = self.builder.func.dfg.valueType(a);
        const r = try self.Binary(.rotr, ty, a, b);
        return r.result.?;
    }

    /// Count leading zeros. Port of Cranelift clz.
    pub fn clz(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.Unary(.clz, ty, arg);
        return r.result.?;
    }

    /// Count trailing zeros. Port of Cranelift ctz.
    pub fn ctz(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.Unary(.ctz, ty, arg);
        return r.result.?;
    }

    /// Population count. Port of Cranelift popcnt.
    pub fn popcnt(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.Unary(.popcnt, ty, arg);
        return r.result.?;
    }

    // ========================================================================
    // Float Arithmetic
    // ========================================================================

    /// Float addition.
    pub fn fadd(self: Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.Binary(.fadd, ty, a, b);
        return r.result.?;
    }

    /// Float subtraction.
    pub fn fsub(self: Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.Binary(.fsub, ty, a, b);
        return r.result.?;
    }

    /// Float multiplication.
    pub fn fmul(self: Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.Binary(.fmul, ty, a, b);
        return r.result.?;
    }

    /// Float division.
    pub fn fdiv(self: Self, ty: Type, a: Value, b: Value) !Value {
        const r = try self.Binary(.fdiv, ty, a, b);
        return r.result.?;
    }

    /// Float absolute value.
    pub fn fabs(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fabs, ty, arg);
        return r.result.?;
    }

    /// Float negation.
    pub fn fneg(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fneg, ty, arg);
        return r.result.?;
    }

    /// Float square root.
    pub fn sqrt(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.sqrt, ty, arg);
        return r.result.?;
    }

    /// Float ceil (round toward positive infinity).
    pub fn ceil(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.ceil, ty, arg);
        return r.result.?;
    }

    /// Float floor (round toward negative infinity).
    pub fn floor(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.floor, ty, arg);
        return r.result.?;
    }

    /// Float trunc (round toward zero).
    pub fn ftrunc(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.trunc, ty, arg);
        return r.result.?;
    }

    /// Float nearest (round to nearest even).
    pub fn nearest(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.nearest, ty, arg);
        return r.result.?;
    }

    // ========================================================================
    // Comparisons
    // ========================================================================

    /// Integer comparison.
    pub fn icmp(self: Self, cond: clif.IntCC, a: Value, b: Value) !Value {
        const r = try self.IntCompare(.icmp, cond, a, b);
        return r.result.?;
    }

    /// Float comparison.
    pub fn fcmp(self: Self, cond: FloatCC, a: Value, b: Value) !Value {
        const r = try self.FloatCompare(.fcmp, cond, a, b);
        return r.result.?;
    }

    // ========================================================================
    // Conversions
    // ========================================================================

    /// Zero extend.
    pub fn uextend(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.uextend, ty, arg);
        return r.result.?;
    }

    /// Sign extend.
    pub fn sextend(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.sextend, ty, arg);
        return r.result.?;
    }

    /// Reduce integer width.
    pub fn ireduce(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.ireduce, ty, arg);
        return r.result.?;
    }

    /// Convert float to signed integer.
    pub fn fcvtToSint(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fcvt_to_sint, ty, arg);
        return r.result.?;
    }

    /// Convert float to unsigned integer.
    pub fn fcvtToUint(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fcvt_to_uint, ty, arg);
        return r.result.?;
    }

    /// Convert signed integer to float.
    pub fn fcvtFromSint(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fcvt_from_sint, ty, arg);
        return r.result.?;
    }

    /// Convert unsigned integer to float.
    pub fn fcvtFromUint(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fcvt_from_uint, ty, arg);
        return r.result.?;
    }

    /// Promote F32 to F64.
    pub fn fpromote(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fpromote, ty, arg);
        return r.result.?;
    }

    /// Demote F64 to F32.
    pub fn fdemote(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.fdemote, ty, arg);
        return r.result.?;
    }

    /// Bitcast (reinterpret bits without conversion).
    pub fn bitcast(self: Self, ty: Type, arg: Value) !Value {
        const r = try self.Unary(.bitcast, ty, arg);
        return r.result.?;
    }

    // ========================================================================
    // Memory
    // ========================================================================

    /// Load from memory.
    pub fn load(self: Self, ty: Type, flags: clif.MemFlags, addr: Value, offset: i32) !Value {
        const r = try self.Load(.load, ty, flags, addr, offset);
        return r.result.?;
    }

    /// Store to memory.
    pub fn store(self: Self, flags: clif.MemFlags, val: Value, addr: Value, offset: i32) !Inst {
        const r = try self.Store(.store, flags, val, addr, offset);
        return r.inst;
    }

    /// Load from stack slot.
    pub fn stackLoad(self: Self, ty: Type, slot: StackSlot, offset: i32) !Value {
        const r = try self.StackLoad(.stack_load, ty, slot, offset);
        return r.result.?;
    }

    /// Store to stack slot.
    pub fn stackStore(self: Self, val: Value, slot: StackSlot, offset: i32) !Inst {
        const r = try self.StackStore(.stack_store, val, slot, offset);
        return r.inst;
    }

    /// Get the address of a stack slot.
    ///
    /// Compute the absolute address of a byte in a stack slot.
    /// Returns a pointer-sized value (iAddr / I64).
    ///
    /// Port of cranelift stack_addr instruction.
    /// Reference: cranelift/codegen/meta/src/shared/instructions.rs:1238-1253
    /// Reference: cranelift/codegen/src/isa/aarch64/inst.isle:4519-4525 (compute_stack_addr)
    /// Reference: cranelift/codegen/src/isa/x64/inst.isle:3810-3816 (stack_addr_impl)
    pub fn stackAddr(self: Self, ty: Type, slot: StackSlot, offset: i32) !Value {
        const r = try self.StackLoad(.stack_addr, ty, slot, offset);
        return r.result.?;
    }

    // ========================================================================
    // Control Flow
    // ========================================================================

    /// Unconditional jump.
    pub fn jump(self: Self, destination: Block, args: []const Value) !Inst {
        const inst = try self.JumpFmt(.jump, destination, args);
        try self.builder.declareSuccessor(destination, inst);
        return inst;
    }

    /// Conditional branch.
    pub fn brif(self: Self, cond: Value, then_block: Block, then_args: []const Value, else_block: Block, else_args: []const Value) !Inst {
        const inst = try self.BrifFmt(.brif, cond, then_block, then_args, else_block, else_args);
        try self.builder.declareSuccessor(then_block, inst);
        if (!then_block.eql(else_block)) {
            try self.builder.declareSuccessor(else_block, inst);
        }
        return inst;
    }

    /// Return from function.
    pub fn return_(self: Self, args: []const Value) !Inst {
        if (args.len == 0) {
            return self.NullaryTerminator(.@"return");
        } else {
            return self.MultiAryTerminator(.@"return", args);
        }
    }

    /// Call a function.
    /// Get the address of a function as a pointer value.
    /// Port of Cranelift's func_addr instruction.
    pub fn funcAddr(self: Self, ty: Type, func_ref: FuncRef) !Value {
        const r = try self.buildInst(.{
            .func_addr = .{ .opcode = .func_addr, .func_ref = func_ref },
        }, ty);
        return r.result.?;
    }

    pub fn call(self: Self, func_ref: FuncRef, args: []const Value) !struct { inst: Inst, results: []const Value } {
        try self.builder.ensureInsertedBlock();

        // Get the signature to determine return types
        const ext_func = self.builder.func.getExtFunc(func_ref) orelse
            return error.InvalidFuncRef;
        const sig = self.builder.func.getSignature(ext_func.signature) orelse
            return error.InvalidSignature;

        const vlist = try self.builder.func.dfg.value_lists.alloc(args);
        const inst = try self.builder.func.dfg.makeInst(.{ .call = .{
            .opcode = .call,
            .func_ref = func_ref,
            .args = vlist,
        } });
        try self.builder.func.layout.appendInst(self.builder.func_ctx.allocator, inst, self.block);

        // Create result values for each return type
        for (sig.returns.items) |ret| {
            _ = try self.builder.func.dfg.makeInstResult(inst, ret.value_type);
        }

        const results = self.builder.func.dfg.instResults(inst);
        return .{ .inst = inst, .results = results };
    }

    /// Indirect call through a function pointer.
    pub fn callIndirect(self: Self, sig_ref: SigRef, callee: Value, args: []const Value) !struct { inst: Inst, results: []const Value } {
        try self.builder.ensureInsertedBlock();

        // Get the signature to determine return types
        const sig = self.builder.func.getSignature(sig_ref) orelse
            return error.InvalidSignature;

        // Cranelift pattern: callee is the first element of args, then call arguments.
        // instArgs()/numInputs() return [callee, arg0, arg1, ...].
        // The lowering expects input 0 = callee, inputs 1.. = call args.
        var vlist = clif.ValueList.init();
        vlist = try self.builder.func.dfg.value_lists.push(vlist, callee);
        for (args) |v| {
            vlist = try self.builder.func.dfg.value_lists.push(vlist, v);
        }
        const inst = try self.builder.func.dfg.makeInst(.{ .call_indirect = .{
            .opcode = .call_indirect,
            .sig_ref = sig_ref,
            .callee = callee,
            .args = vlist,
        } });
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
        return self.buildTerminator(.{ .trap = .{ .opcode = .trap, .code = code } });
    }

    /// Trap if condition is nonzero (true).
    pub fn trapnz(self: Self, cond: Value, code: clif.TrapCode) !Inst {
        const r = try self.buildInst(.{ .cond_trap = .{ .opcode = .trapnz, .arg = cond, .code = code } }, null);
        return r.inst;
    }

    /// Trap if condition is zero (false).
    pub fn trapz(self: Self, cond: Value, code: clif.TrapCode) !Inst {
        const r = try self.buildInst(.{ .cond_trap = .{ .opcode = .trapz, .arg = cond, .code = code } }, null);
        return r.inst;
    }

    /// Branch table instruction.
    pub fn brTable(self: Self, selector: Value, jt: JumpTable) !Inst {
        const inst = try self.buildTerminator(.{ .branch_table = .{
            .opcode = .br_table,
            .arg = selector,
            .table = jt,
        } });

        // Declare successors from jump table.
        // Port of cranelift/frontend/src/frontend.rs lines 151-180:
        // Jump tables can have the same successor appear multiple times,
        // so we must deduplicate before declaring predecessors.
        // "Callers are expected to avoid adding the same predecessor more
        //  than once in the case of a jump table." - cranelift ssa.rs
        if (self.builder.func.getJumpTable(jt)) |jt_data| {
            // Use a simple set to track unique blocks already declared
            const allocator = self.builder.func_ctx.allocator;
            var seen = std.AutoHashMapUnmanaged(u32, void){};
            defer seen.deinit(allocator);

            for (jt_data.allBranches()) |entry| {
                const gop = try seen.getOrPut(allocator, entry.block.index);
                if (!gop.found_existing) {
                    try self.builder.declareSuccessor(entry.block, inst);
                }
            }
        }

        return inst;
    }

    // ========================================================================
    // Misc
    // ========================================================================

    /// No operation.
    pub fn nop(self: Self) !Inst {
        const r = try self.buildInst(.{ .nullary = .{ .opcode = .nop } }, null);
        return r.inst;
    }

    /// Select between two values based on condition.
    pub fn select(self: Self, ty: Type, cond: Value, if_true: Value, if_false: Value) !Value {
        const r = try self.buildInst(.{ .ternary = .{
            .opcode = .select,
            .args = .{ cond, if_true, if_false },
        } }, ty);
        return r.result.?;
    }

    /// Copy a value.
    pub fn copy(self: Self, arg: Value) !Value {
        const ty = self.builder.func.dfg.valueType(arg);
        const r = try self.buildInst(.{ .unary = .{ .opcode = .copy, .arg = arg } }, ty);
        return r.result.?;
    }

    // ========================================================================
    // Global Values
    // ========================================================================

    /// Compute the value of a global variable.
    pub fn globalValue(self: Self, ty: Type, gv: GlobalValue) !Value {
        const r = try self.buildInst(.{ .unary_global_value = .{
            .opcode = .global_value,
            .global_value = gv,
        } }, ty);
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
