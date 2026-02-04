//! A SSA-building API that handles incomplete CFGs.
//!
//! Port of cranelift-frontend/src/ssa.rs
//!
//! The algorithm is based upon Braun M., Buchwald S., Hack S., Leißa R., Mallon C.,
//! Zwinkau A. (2013) Simple and Efficient Construction of Static Single Assignment Form.
//! In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
//! Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
//!
//! https://link.springer.com/content/pdf/10.1007/978-3-642-37051-9_6.pdf

const std = @import("std");
const Variable = @import("variable.zig").Variable;

// Import CLIF types
const clif = @import("../../../ir/clif/mod.zig");
const Block = clif.Block;
const Value = clif.Value;
const Inst = clif.Inst;
const Type = clif.Type;
const Function = clif.Function;

/// Side effects of a `use_var` or a `seal_block` method call.
pub const SideEffects = struct {
    /// When a variable is used but has never been defined before (this happens in the case of
    /// unreachable code), a placeholder `iconst` or `fconst` value is added to the right Block.
    /// This field signals if it is the case and returns the Blocks to which initialization has
    /// been added.
    instructions_added_to_blocks: std.ArrayListUnmanaged(Block) = .{},

    const Self = @This();

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.instructions_added_to_blocks.deinit(allocator);
    }

    pub fn isEmpty(self: Self) bool {
        return self.instructions_added_to_blocks.items.len == 0;
    }

    pub fn clear(self: *Self) void {
        self.instructions_added_to_blocks.clearRetainingCapacity();
    }
};

/// Block sealing state.
const Sealed = union(enum) {
    /// Block is not sealed yet. Contains list of undefined variables.
    no: struct {
        undef_variables: std.ArrayListUnmanaged(Variable),
    },
    /// Block is sealed - all predecessors are known.
    yes,

    pub fn deinit(self: *Sealed, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .no => |*data| data.undef_variables.deinit(allocator),
            .yes => {},
        }
    }
};

/// Data associated with a basic block for SSA construction.
const SSABlockData = struct {
    /// The predecessors of the Block (branch instructions).
    predecessors: std.ArrayListUnmanaged(Inst) = .{},
    /// A block is sealed if all of its predecessors have been declared.
    sealed: Sealed = .{ .no = .{ .undef_variables = .{} } },
    /// If this block is sealed and has exactly one predecessor, this is that predecessor.
    single_predecessor: ?Block = null,

    pub fn deinit(self: *SSABlockData, allocator: std.mem.Allocator) void {
        self.predecessors.deinit(allocator);
        self.sealed.deinit(allocator);
    }
};

/// States for the use_var/predecessors_lookup state machine.
const Call = union(enum) {
    use_var: Inst,
    finish_predecessors_lookup: struct {
        sentinel: Value,
        dest_block: Block,
    },
};

/// Structure containing the data relevant to the construction of SSA for a given function.
///
/// This SSA building module allows you to def and use variables on the fly while you are
/// constructing the CFG, no need for a separate SSA pass after the CFG is completed.
///
/// A basic block is said _filled_ if all the instructions that it contains have been translated,
/// and it is said _sealed_ if all of its predecessors have been declared. Only filled predecessors
/// can be declared.
pub const SSABuilder = struct {
    allocator: std.mem.Allocator,

    /// Records for every variable and for every relevant block, the last definition of
    /// the variable in the block.
    /// Map: Variable -> (Block -> Value)
    variables: std.AutoHashMapUnmanaged(Variable, std.AutoHashMapUnmanaged(Block, Value)) = .{},

    /// Records the position of basic blocks and SSA data.
    ssa_blocks: std.AutoHashMapUnmanaged(Block, SSABlockData) = .{},

    /// Call stack for use in the use_var/predecessors_lookup state machine.
    calls: std.ArrayListUnmanaged(Call) = .{},

    /// Result stack for use in the use_var/predecessors_lookup state machine.
    results: std.ArrayListUnmanaged(Value) = .{},

    /// Side effects accumulated in the use_var/predecessors_lookup state machine.
    side_effects: SideEffects = .{},

    /// Reused storage for cycle detection.
    visited: std.AutoHashMapUnmanaged(Block, void) = .{},

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        // Deinit inner maps
        var var_iter = self.variables.valueIterator();
        while (var_iter.next()) |inner_map| {
            inner_map.deinit(self.allocator);
        }
        self.variables.deinit(self.allocator);

        var block_iter = self.ssa_blocks.valueIterator();
        while (block_iter.next()) |block_data| {
            block_data.deinit(self.allocator);
        }
        self.ssa_blocks.deinit(self.allocator);

        self.calls.deinit(self.allocator);
        self.results.deinit(self.allocator);
        self.side_effects.deinit(self.allocator);
        self.visited.deinit(self.allocator);
    }

    /// Clears the SSABuilder from all its data, letting it in a pristine state without
    /// deallocating memory.
    pub fn clear(self: *Self) void {
        // Clear inner variable maps
        var var_iter = self.variables.valueIterator();
        while (var_iter.next()) |inner_map| {
            inner_map.deinit(self.allocator);
        }
        self.variables.clearRetainingCapacity();

        // Clear block data
        var block_iter = self.ssa_blocks.valueIterator();
        while (block_iter.next()) |block_data| {
            block_data.deinit(self.allocator);
        }
        self.ssa_blocks.clearRetainingCapacity();

        self.calls.clearRetainingCapacity();
        self.results.clearRetainingCapacity();
        self.side_effects.clear();
        self.visited.clearRetainingCapacity();
    }

    /// Tests whether an SSABuilder is in a cleared state.
    pub fn isEmpty(self: Self) bool {
        return self.variables.count() == 0 and
            self.ssa_blocks.count() == 0 and
            self.calls.items.len == 0 and
            self.results.items.len == 0 and
            self.side_effects.isEmpty();
    }

    // ========================================================================
    // Public API
    // ========================================================================

    /// Declares a new definition of a variable in a given basic block.
    pub fn defVar(self: *Self, variable: Variable, val: Value, block: Block) !void {
        const gop = try self.variables.getOrPut(self.allocator, variable);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        try gop.value_ptr.put(self.allocator, block, val);
    }

    /// Declares a use of a variable in a given basic block. Returns the SSA value corresponding
    /// to the current SSA definition of this variable and a list of newly created Blocks that
    /// are the results of critical edge splitting.
    ///
    /// If the variable has never been defined in this block or recursively in its predecessors,
    /// this method will silently create an initializer with iconst or fconst. You are
    /// responsible for making sure that you initialize your variables.
    pub fn useVar(
        self: *Self,
        func: *Function,
        variable: Variable,
        ty: Type,
        block: Block,
    ) !struct { value: Value, side_effects: SideEffects } {
        std.debug.assert(self.calls.items.len == 0);
        std.debug.assert(self.results.items.len == 0);
        std.debug.assert(self.side_effects.isEmpty());

        // Prepare the 'calls' and 'results' stacks for the state machine.
        try self.useVarNonlocal(func, variable, ty, block);
        const value = try self.runStateMachine(func, variable, ty);

        // Move side effects out
        const side_effects = self.side_effects;
        self.side_effects = .{};

        return .{ .value = value, .side_effects = side_effects };
    }

    /// Declares a new basic block to construct corresponding data for SSA construction.
    /// No predecessors are declared here and the block is not sealed.
    /// Predecessors have to be added with declareBlockPredecessor.
    pub fn declareBlock(self: *Self, block: Block) !void {
        const gop = try self.ssa_blocks.getOrPut(self.allocator, block);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
    }

    /// Declares a new predecessor for a Block and records the branch instruction
    /// of the predecessor that leads to it.
    ///
    /// The precedent Block must be filled before added as predecessor.
    pub fn declareBlockPredecessor(self: *Self, block: Block, inst: Inst) !void {
        std.debug.assert(!self.isSealed(block));
        const data = self.ssa_blocks.getPtr(block) orelse unreachable;
        try data.predecessors.append(self.allocator, inst);
    }

    /// Remove a previously declared Block predecessor.
    pub fn removeBlockPredecessor(self: *Self, block: Block, inst: Inst) void {
        std.debug.assert(!self.isSealed(block));
        const data = self.ssa_blocks.getPtr(block) orelse unreachable;
        for (data.predecessors.items, 0..) |pred, i| {
            if (pred.eql(inst)) {
                _ = data.predecessors.swapRemove(i);
                return;
            }
        }
        unreachable; // Predecessor not found
    }

    /// Completes the global value numbering for a Block, all of its predecessors having been
    /// already sealed.
    pub fn sealBlock(self: *Self, block: Block, func: *Function) !SideEffects {
        std.debug.assert(!self.isSealed(block));
        try self.sealOneBlock(block, func);

        const side_effects = self.side_effects;
        self.side_effects = .{};
        return side_effects;
    }

    /// Completes the global value numbering for all unsealed Blocks in func.
    pub fn sealAllBlocks(self: *Self, func: *Function) !SideEffects {
        // Collect keys first to avoid iterator invalidation
        var blocks_to_seal = std.ArrayListUnmanaged(Block){};
        defer blocks_to_seal.deinit(self.allocator);

        var key_iter = self.ssa_blocks.keyIterator();
        while (key_iter.next()) |block| {
            try blocks_to_seal.append(self.allocator, block.*);
        }

        for (blocks_to_seal.items) |block| {
            try self.sealOneBlock(block, func);
        }

        const side_effects = self.side_effects;
        self.side_effects = .{};
        return side_effects;
    }

    /// Returns the list of predecessor instructions for a block.
    pub fn predecessors(self: Self, block: Block) []const Inst {
        const data = self.ssa_blocks.get(block) orelse return &[_]Inst{};
        return data.predecessors.items;
    }

    /// Returns whether the given Block has any predecessors.
    pub fn hasAnyPredecessors(self: Self, block: Block) bool {
        return self.predecessors(block).len > 0;
    }

    /// Returns true if and only if sealBlock has been called on the argument.
    pub fn isSealed(self: Self, block: Block) bool {
        const data = self.ssa_blocks.get(block) orelse return false;
        return data.sealed == .yes;
    }

    // ========================================================================
    // Private implementation
    // ========================================================================

    /// Get variable definition in a block, or null if not defined.
    fn getVarDef(self: Self, variable: Variable, block: Block) ?Value {
        const inner_map = self.variables.get(variable) orelse return null;
        return inner_map.get(block);
    }

    /// Resolve the minimal SSA Value of var in block by traversing predecessors.
    fn useVarNonlocal(self: *Self, func: *Function, variable: Variable, ty: Type, block: Block) !void {
        // First, try Local Value Numbering (Algorithm 1 in the paper).
        // If the variable already has a known Value in this block, use that.
        if (self.getVarDef(variable, block)) |val| {
            try self.results.append(self.allocator, val);
            return;
        }

        // Otherwise, use Global Value Numbering (Algorithm 2 in the paper).
        const result = try self.findVar(func, variable, ty, block);
        const val = result.val;
        const from = result.from;

        // Copy the definition to all intervening blocks on single-predecessor path.
        var current = block;
        while (!current.eql(from)) {
            std.debug.assert(self.getVarDef(variable, current) == null);
            try self.defVar(variable, val, current);
            const data = self.ssa_blocks.get(current) orelse break;
            current = data.single_predecessor orelse break;
        }
    }

    /// Find the most recent definition of this variable.
    fn findVar(
        self: *Self,
        func: *Function,
        variable: Variable,
        ty: Type,
        block_arg: Block,
    ) !struct { val: Value, from: Block } {
        var block = block_arg;

        // Try to find an existing definition along single-predecessor edges first.
        self.visited.clearRetainingCapacity();

        while (true) {
            const data = self.ssa_blocks.get(block) orelse break;
            const pred = data.single_predecessor orelse break;

            // Cycle detection
            const gop = try self.visited.getOrPut(self.allocator, block);
            if (gop.found_existing) break;

            block = pred;

            // Check if variable is defined in this block
            if (self.getVarDef(variable, block)) |val| {
                try self.results.append(self.allocator, val);
                return .{ .val = val, .from = block };
            }
        }

        // We didn't find a usable definition. Create one via block parameter.
        const val = try func.dfg.appendBlockParam(block, ty);
        try self.defVar(variable, val, block);

        // Set up predecessor lookup
        const data = self.ssa_blocks.getPtr(block) orelse unreachable;
        switch (data.sealed) {
            .yes => try self.beginPredecessorsLookup(val, block),
            .no => |*no_data| {
                try no_data.undef_variables.append(self.allocator, variable);
                try self.results.append(self.allocator, val);
            },
        }

        return .{ .val = val, .from = block };
    }

    /// Helper function for seal_block and seal_all_blocks.
    fn sealOneBlock(self: *Self, block: Block, func: *Function) !void {
        const data = self.ssa_blocks.getPtr(block) orelse return;

        // Get undef variables if not already sealed
        var undef_variables: std.ArrayListUnmanaged(Variable) = switch (data.sealed) {
            .no => |no_data| no_data.undef_variables,
            .yes => return,
        };
        data.sealed = .yes;

        const ssa_params = undef_variables.items.len;

        // Set single predecessor if applicable
        if (data.predecessors.items.len == 1) {
            const pred_inst = data.predecessors.items[0];
            const pred_block = func.layout.instBlock(pred_inst);
            data.single_predecessor = pred_block;
        }

        // Process each undefined variable
        for (0..ssa_params) |idx| {
            const variable = undef_variables.items[idx];

            // Get the temporary Value assigned to this Variable
            const block_params = func.dfg.blockParams(block);
            const val = block_params[block_params.len - (ssa_params - idx)];

            std.debug.assert(self.calls.items.len == 0);
            std.debug.assert(self.results.items.len == 0);

            try self.beginPredecessorsLookup(val, block);
            _ = try self.runStateMachine(func, variable, func.dfg.valueType(val));
        }

        undef_variables.deinit(self.allocator);
    }

    /// Begin looking up values from predecessors.
    fn beginPredecessorsLookup(self: *Self, sentinel: Value, dest_block: Block) !void {
        try self.calls.append(self.allocator, .{
            .finish_predecessors_lookup = .{
                .sentinel = sentinel,
                .dest_block = dest_block,
            },
        });

        // Add calls for each predecessor (in reverse order for correct processing)
        const preds = self.predecessors(dest_block);
        var i = preds.len;
        while (i > 0) {
            i -= 1;
            try self.calls.append(self.allocator, .{ .use_var = preds[i] });
        }
    }

    /// Examine the values from predecessors and compute a result value.
    ///
    /// Port of cranelift frontend/src/ssa.rs finish_predecessors_lookup
    fn finishPredecessorsLookup(
        self: *Self,
        func: *Function,
        sentinel: Value,
        dest_block: Block,
    ) !Value {
        const preds = self.predecessors(dest_block);
        const num_predecessors = preds.len;

        // Get values from predecessors
        const start_idx = self.results.items.len - num_predecessors;
        const pred_values = self.results.items[start_idx..];

        // Check if all predecessors provide the same value (excluding sentinel)
        var unique_val: ?Value = null;
        var all_same = true;

        for (pred_values) |val| {
            const resolved = func.dfg.resolveAliases(val);
            if (resolved.eql(sentinel)) continue;

            if (unique_val) |uv| {
                if (!uv.eql(resolved)) {
                    all_same = false;
                    break;
                }
            } else {
                unique_val = resolved;
            }
        }

        if (unique_val) |val| {
            if (all_same) {
                // All predecessors use the same value - remove block param and use that value
                // Port of cranelift ssa.rs lines 555-556:
                //   func.dfg.remove_block_param(sentinel);
                //   func.dfg.change_to_alias(sentinel, pred_val);
                // Truncate results first
                self.results.shrinkRetainingCapacity(start_idx);
                // CRITICAL: Must remove the block param before aliasing!
                // This was the root cause of "branch args must match block params" error
                try func.dfg.removeBlockParam(sentinel);
                func.dfg.changeToAliasOf(sentinel, val);
                return val;
            }
        } else {
            // Variable is used but never defined (unreachable code).
            // Truncate results first
            self.results.shrinkRetainingCapacity(start_idx);

            // Create a zero constant.
            if (!func.layout.isBlockInserted(dest_block)) {
                try func.layout.appendBlock(func.dfg.allocator, dest_block);
            }
            try self.side_effects.instructions_added_to_blocks.append(self.allocator, dest_block);

            const zero = try emitZero(func, func.dfg.valueType(sentinel), dest_block);
            // Also remove the block param for the unreachable case
            try func.dfg.removeBlockParam(sentinel);
            func.dfg.changeToAliasOf(sentinel, zero);
            return zero;
        }

        // Predecessors disagree - keep the block parameter and add arguments to predecessor branches.
        // Port of cranelift ssa.rs lines 561-577
        for (preds, 0..) |branch_inst, idx| {
            const val = pred_values[idx];
            // Append this value as an argument to the branch instruction for dest_block
            try func.dfg.appendBranchArg(branch_inst, dest_block, val);
        }

        // Truncate results after processing
        self.results.shrinkRetainingCapacity(start_idx);

        return sentinel;
    }

    /// The main algorithm state machine.
    fn runStateMachine(self: *Self, func: *Function, variable: Variable, ty: Type) !Value {
        while (self.calls.pop()) |call| {
            switch (call) {
                .use_var => |branch| {
                    const block = func.layout.instBlock(branch) orelse continue;
                    try self.useVarNonlocal(func, variable, ty, block);
                },
                .finish_predecessors_lookup => |data| {
                    const val = try self.finishPredecessorsLookup(func, data.sentinel, data.dest_block);
                    try self.results.append(self.allocator, val);
                },
            }
        }

        std.debug.assert(self.results.items.len == 1);
        return self.results.pop().?;
    }
};

/// Emit instructions to produce a zero value in the given type.
/// Port of cranelift-frontend/src/ssa.rs emit_zero_value
fn emitZero(func: *Function, ty: Type, block: Block) !Value {
    const data: clif.InstructionData = if (ty.isInt())
        .{ .unary_imm = .{ .opcode = .iconst, .imm = 0 } }
    else if (ty.isFloat() and ty.eql(Type.F32))
        .{ .unary_ieee32 = .{ .opcode = .f32const, .imm = 0.0 } }
    else if (ty.isFloat())
        .{ .unary_ieee64 = .{ .opcode = .f64const, .imm = 0.0 } }
    else
        // Default to integer zero for other types
        .{ .unary_imm = .{ .opcode = .iconst, .imm = 0 } };

    const inst = try func.dfg.makeInst(data);
    try func.layout.appendInst(func.dfg.allocator, inst, block);
    return try func.dfg.makeInstResult(inst, ty);
}

// ============================================================================
// Tests
// ============================================================================

test "ssa builder basics" {
    const testing = std.testing;

    var ssa = SSABuilder.init(testing.allocator);
    defer ssa.deinit();

    try testing.expect(ssa.isEmpty());
}

test "declare and seal block" {
    const testing = std.testing;

    var ssa = SSABuilder.init(testing.allocator);
    defer ssa.deinit();

    const block0 = Block.fromIndex(0);
    try ssa.declareBlock(block0);

    try testing.expect(!ssa.isSealed(block0));

    // Can't test sealing fully without a Function
}

test "def and use var simple" {
    const testing = std.testing;

    var ssa = SSABuilder.init(testing.allocator);
    defer ssa.deinit();

    const block0 = Block.fromIndex(0);
    const var0 = Variable.new(0);
    const val0 = Value.fromIndex(0);

    try ssa.declareBlock(block0);
    try ssa.defVar(var0, val0, block0);

    // Check we can get it back
    const retrieved = ssa.getVarDef(var0, block0);
    try testing.expect(retrieved != null);
    try testing.expect(retrieved.?.eql(val0));
}
