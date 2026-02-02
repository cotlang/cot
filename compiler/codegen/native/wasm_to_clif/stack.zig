//! State of the Wasm stack for translation into CLIF.
//!
//! Port of wasmtime/crates/cranelift/src/translate/stack.rs
//!
//! The `TranslationState` struct defined in this module is used to keep
//! track of the WebAssembly value and control stacks during the translation of
//! a single function.

const std = @import("std");

// ============================================================================
// Entity Types - Import from CLIF module
// ============================================================================

pub const clif = @import("../../../ir/clif/mod.zig");
pub const Block = clif.Block;
pub const Value = clif.Value;
pub const Inst = clif.Inst;

// ============================================================================
// ElseData
// Port of translate/stack.rs ElseData
// ============================================================================

/// Information about the presence of an associated `else` for an `if`, or the
/// lack thereof.
pub const ElseData = union(enum) {
    /// The `if` does not already have an `else` block.
    ///
    /// This doesn't mean that it will never have an `else`, just that we
    /// haven't seen it yet.
    no_else: struct {
        /// If we discover that we need an `else` block, this is the jump
        /// instruction that needs to be fixed up to point to the new `else`
        /// block rather than the destination block after the `if...end`.
        branch_inst: Inst,

        /// The placeholder block we're replacing.
        placeholder: Block,
    },

    /// We have already allocated an `else` block.
    ///
    /// Usually we don't know whether we will hit an `if .. end` or an `if
    /// .. else .. end`, but sometimes we can tell based on the block's type
    /// signature that the signature is not valid if there isn't an `else`. In
    /// these cases, we pre-allocate the `else` block.
    with_else: struct {
        /// This is the `else` block.
        else_block: Block,
    },
};

// ============================================================================
// ControlStackFrame
// Port of translate/stack.rs ControlStackFrame
// ============================================================================

/// A control stack frame can be an `if`, a `block` or a `loop`, each one having the following
/// fields:
///
/// - `destination`: reference to the `Block` that will hold the code after the control block;
/// - `num_return_values`: number of values returned by the control block;
/// - `original_stack_size`: size of the value stack at the beginning of the control block.
///
/// The `loop` frame has a `header` field that references the `Block` that contains the beginning
/// of the body of the loop.
pub const ControlStackFrame = union(enum) {
    if_frame: IfFrame,
    block_frame: BlockFrame,
    loop_frame: LoopFrame,

    pub const IfFrame = struct {
        destination: Block,
        else_data: ElseData,
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
        exit_is_branched_to: bool,
        /// Was the head of the `if` reachable?
        head_is_reachable: bool,
        /// What was the reachability at the end of the consequent?
        ///
        /// This is null until we're finished translating the consequent, and
        /// is set to Some either by hitting an `else` when we will begin
        /// translating the alternative, or by hitting an `end` in which case
        /// there is no alternative.
        consequent_ends_reachable: ?bool,
    };

    pub const BlockFrame = struct {
        destination: Block,
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
        exit_is_branched_to: bool,
    };

    pub const LoopFrame = struct {
        destination: Block,
        header: Block,
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
    };

    const Self = @This();

    pub fn numReturnValues(self: Self) usize {
        return switch (self) {
            .if_frame => |f| f.num_return_values,
            .block_frame => |f| f.num_return_values,
            .loop_frame => |f| f.num_return_values,
        };
    }

    pub fn numParamValues(self: Self) usize {
        return switch (self) {
            .if_frame => |f| f.num_param_values,
            .block_frame => |f| f.num_param_values,
            .loop_frame => |f| f.num_param_values,
        };
    }

    /// Get the block after the control construct.
    pub fn followingCode(self: Self) Block {
        return switch (self) {
            .if_frame => |f| f.destination,
            .block_frame => |f| f.destination,
            .loop_frame => |f| f.destination,
        };
    }

    /// Get the branch destination for `br` instructions.
    /// CRITICAL: Loop returns to header, not destination!
    pub fn brDestination(self: Self) Block {
        return switch (self) {
            .if_frame => |f| f.destination,
            .block_frame => |f| f.destination,
            .loop_frame => |f| f.header,
        };
    }

    /// Get the original stack size when entering this frame.
    fn originalStackSize(self: Self) usize {
        return switch (self) {
            .if_frame => |f| f.original_stack_size,
            .block_frame => |f| f.original_stack_size,
            .loop_frame => |f| f.original_stack_size,
        };
    }

    pub fn isLoop(self: Self) bool {
        return switch (self) {
            .loop_frame => true,
            else => false,
        };
    }

    pub fn exitIsBranchedTo(self: Self) bool {
        return switch (self) {
            .if_frame => |f| f.exit_is_branched_to,
            .block_frame => |f| f.exit_is_branched_to,
            .loop_frame => false,
        };
    }

    pub fn setBranchedToExit(self: *Self) void {
        switch (self.*) {
            .if_frame => |*f| f.exit_is_branched_to = true,
            .block_frame => |*f| f.exit_is_branched_to = true,
            .loop_frame => {},
        }
    }

    /// Pop values from the value stack so that it is left at the
    /// input-parameters to an else-block.
    pub fn truncateValueStackToElseParams(self: Self, stack: *std.ArrayListUnmanaged(Value)) void {
        std.debug.assert(self == .if_frame);
        stack.shrinkRetainingCapacity(self.originalStackSize());
    }

    /// Pop values from the value stack so that it is left at the state it was
    /// before this control-flow frame.
    pub fn truncateValueStackToOriginalSize(self: Self, stack: *std.ArrayListUnmanaged(Value)) void {
        // The "If" frame pushes its parameters twice, so they're available to the else block
        // (see also `TranslationState.pushIf`).
        // Yet, the original_stack_size member accounts for them only once, so that the else
        // block can see the same number of parameters as the consequent block. As a matter of
        // fact, we need to subtract an extra number of parameter values for if blocks.
        const num_duplicated_params: usize = switch (self) {
            .if_frame => |f| blk: {
                std.debug.assert(f.num_param_values <= self.originalStackSize());
                break :blk f.num_param_values;
            },
            else => 0,
        };

        const new_len = self.originalStackSize() - num_duplicated_params;
        stack.shrinkRetainingCapacity(new_len);
    }
};

// ============================================================================
// TranslationState
// Port of translate/stack.rs FuncTranslationStacks
// ============================================================================

/// Keeps track of Wasm's operand and control stacks, as well as reachability
/// for each control frame.
pub const TranslationState = struct {
    /// A stack of values corresponding to the active values in the input wasm function at this
    /// point. These are CLIF Values.
    stack: std.ArrayListUnmanaged(Value),
    /// A stack of active control flow operations at this point in the input wasm function.
    control_stack: std.ArrayListUnmanaged(ControlStackFrame),
    /// Is the current translation state still reachable? This is false when translating operators
    /// like End, Return, or Unreachable.
    reachable: bool,
    /// Allocator for dynamic storage.
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Construct a new, empty, `TranslationState`
    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .stack = .{},
            .control_stack = .{},
            .reachable = true,
            .allocator = allocator,
        };
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        self.stack.deinit(self.allocator);
        self.control_stack.deinit(self.allocator);
    }

    /// Clear state for reuse.
    pub fn clear(self: *Self) void {
        std.debug.assert(self.stack.items.len == 0);
        std.debug.assert(self.control_stack.items.len == 0);
        self.reachable = true;
    }

    /// Initialize the state for compiling a function.
    ///
    /// This resets the state to containing only a single block representing the whole function.
    /// The exit block is the last block in the function which will contain the return instruction.
    pub fn initialize(self: *Self, exit_block: Block, num_return_values: usize) !void {
        self.clear();
        try self.pushBlock(exit_block, 0, num_return_values);
    }

    /// True if the current translation state expresses reachable code.
    pub fn isReachable(self: Self) bool {
        return self.reachable;
    }

    // ========================================================================
    // Value Stack Operations
    // ========================================================================

    /// Push a value.
    pub fn push1(self: *Self, val: Value) !void {
        try self.stack.append(self.allocator, val);
    }

    /// Push two values.
    pub fn push2(self: *Self, val1: Value, val2: Value) !void {
        try self.stack.append(self.allocator, val1);
        try self.stack.append(self.allocator, val2);
    }

    /// Push multiple values.
    pub fn pushn(self: *Self, vals: []const Value) !void {
        try self.stack.appendSlice(self.allocator, vals);
    }

    /// Pop one value.
    pub fn pop1(self: *Self) Value {
        return self.stack.pop().?;
    }

    /// Peek at the top of the stack without popping it.
    pub fn peek1(self: Self) Value {
        return self.stack.getLast();
    }

    /// Pop two values. Return them in the order they were pushed.
    pub fn pop2(self: *Self) struct { Value, Value } {
        const v2 = self.stack.pop().?;
        const v1 = self.stack.pop().?;
        return .{ v1, v2 };
    }

    /// Pop three values. Return them in the order they were pushed.
    pub fn pop3(self: *Self) struct { Value, Value, Value } {
        const v3 = self.stack.pop().?;
        const v2 = self.stack.pop().?;
        const v1 = self.stack.pop().?;
        return .{ v1, v2, v3 };
    }

    /// Pop the top `n` values on the stack.
    ///
    /// The popped values are not returned. Use `peekn` to look at them before popping.
    pub fn popn(self: *Self, n: usize) void {
        std.debug.assert(n <= self.stack.items.len);
        self.stack.shrinkRetainingCapacity(self.stack.items.len - n);
    }

    /// Peek at the top `n` values on the stack in the order they were pushed.
    pub fn peekn(self: Self, n: usize) []const Value {
        std.debug.assert(n <= self.stack.items.len);
        return self.stack.items[self.stack.items.len - n ..];
    }

    /// Peek at the top `n` values on the stack (mutable).
    pub fn peeknMut(self: *Self, n: usize) []Value {
        std.debug.assert(n <= self.stack.items.len);
        return self.stack.items[self.stack.items.len - n ..];
    }

    /// Get the current stack length.
    pub fn stackLen(self: Self) usize {
        return self.stack.items.len;
    }

    // ========================================================================
    // Control Stack Operations
    // ========================================================================

    /// Push a block on the control stack.
    pub fn pushBlock(
        self: *Self,
        following_code: Block,
        num_param_types: usize,
        num_result_types: usize,
    ) !void {
        std.debug.assert(num_param_types <= self.stack.items.len);
        try self.control_stack.append(self.allocator, .{
            .block_frame = .{
                .destination = following_code,
                .original_stack_size = self.stack.items.len - num_param_types,
                .num_param_values = num_param_types,
                .num_return_values = num_result_types,
                .exit_is_branched_to = false,
            },
        });
    }

    /// Push a loop on the control stack.
    pub fn pushLoop(
        self: *Self,
        header: Block,
        following_code: Block,
        num_param_types: usize,
        num_result_types: usize,
    ) !void {
        std.debug.assert(num_param_types <= self.stack.items.len);
        try self.control_stack.append(self.allocator, .{
            .loop_frame = .{
                .header = header,
                .destination = following_code,
                .original_stack_size = self.stack.items.len - num_param_types,
                .num_param_values = num_param_types,
                .num_return_values = num_result_types,
            },
        });
    }

    /// Push an if on the control stack.
    pub fn pushIf(
        self: *Self,
        destination: Block,
        else_data: ElseData,
        num_param_types: usize,
        num_result_types: usize,
    ) !void {
        std.debug.assert(num_param_types <= self.stack.items.len);

        // Push a second copy of our `if`'s parameters on the stack. This lets
        // us avoid saving them on the side in the `ControlStackFrame` for our
        // `else` block (if it exists), which would require a second heap
        // allocation. See also the comment in `translate_operator` for
        // `Operator::Else`.
        try self.stack.ensureUnusedCapacity(self.allocator, num_param_types);
        const start_idx = self.stack.items.len - num_param_types;
        for (start_idx..self.stack.items.len) |i| {
            const val = self.stack.items[i];
            self.stack.appendAssumeCapacity(val);
        }

        try self.control_stack.append(self.allocator, .{
            .if_frame = .{
                .destination = destination,
                .else_data = else_data,
                .original_stack_size = self.stack.items.len - num_param_types,
                .num_param_values = num_param_types,
                .num_return_values = num_result_types,
                .exit_is_branched_to = false,
                .head_is_reachable = self.reachable,
                .consequent_ends_reachable = null,
            },
        });
    }

    /// Get the control stack frame at the given relative depth.
    /// Depth 0 is the top of the stack.
    pub fn getFrame(self: Self, relative_depth: u32) *const ControlStackFrame {
        const i = self.control_stack.items.len - 1 - relative_depth;
        return &self.control_stack.items[i];
    }

    /// Get a mutable control stack frame at the given relative depth.
    pub fn getFrameMut(self: *Self, relative_depth: u32) *ControlStackFrame {
        const i = self.control_stack.items.len - 1 - relative_depth;
        return &self.control_stack.items[i];
    }

    /// Pop the top control frame.
    pub fn popFrame(self: *Self) ControlStackFrame {
        return self.control_stack.pop().?;
    }

    /// Get the control stack length.
    pub fn controlStackLen(self: Self) usize {
        return self.control_stack.items.len;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "value stack operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    const v0 = Value.fromIndex(0);
    const v1 = Value.fromIndex(1);
    const v2 = Value.fromIndex(2);

    try state.push1(v0);
    try state.push1(v1);
    try state.push1(v2);

    try testing.expectEqual(@as(usize, 3), state.stackLen());
    try testing.expectEqual(v2.asU32(), state.peek1().asU32());

    const popped = state.pop1();
    try testing.expectEqual(v2.asU32(), popped.asU32());
    try testing.expectEqual(@as(usize, 2), state.stackLen());

    const pop2_result = state.pop2();
    try testing.expectEqual(v0.asU32(), pop2_result[0].asU32());
    try testing.expectEqual(v1.asU32(), pop2_result[1].asU32());
    try testing.expectEqual(@as(usize, 0), state.stackLen());
}

test "push multiple values" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    const vals = [_]Value{ Value.fromIndex(0), Value.fromIndex(1), Value.fromIndex(2) };
    try state.pushn(&vals);

    try testing.expectEqual(@as(usize, 3), state.stackLen());

    const peeked = state.peekn(2);
    try testing.expectEqual(@as(usize, 2), peeked.len);
    try testing.expectEqual(@as(u32, 1), peeked[0].asU32());
    try testing.expectEqual(@as(u32, 2), peeked[1].asU32());

    state.popn(2);
    try testing.expectEqual(@as(usize, 1), state.stackLen());
}

test "control stack block" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    const block0 = Block.fromIndex(0);
    try state.pushBlock(block0, 0, 1);

    try testing.expectEqual(@as(usize, 1), state.controlStackLen());

    const frame = state.getFrame(0);
    try testing.expectEqual(block0.asU32(), frame.followingCode().asU32());
    try testing.expectEqual(block0.asU32(), frame.brDestination().asU32());
    try testing.expectEqual(@as(usize, 1), frame.numReturnValues());
    try testing.expect(!frame.isLoop());

    _ = state.popFrame();
    try testing.expectEqual(@as(usize, 0), state.controlStackLen());
}

test "control stack loop - br_destination is header" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    const header = Block.fromIndex(0);
    const exit = Block.fromIndex(1);

    try state.pushLoop(header, exit, 0, 0);

    const frame = state.getFrame(0);
    try testing.expectEqual(exit.asU32(), frame.followingCode().asU32());
    // CRITICAL: Loop's br_destination is the header, not destination
    try testing.expectEqual(header.asU32(), frame.brDestination().asU32());
    try testing.expect(frame.isLoop());
}

test "control stack if with else data" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    // Push some values to serve as parameters
    try state.push1(Value.fromIndex(0));
    try state.push1(Value.fromIndex(1));

    const destination = Block.fromIndex(0);
    const else_block = Block.fromIndex(1);
    const else_data = ElseData{ .with_else = .{ .else_block = else_block } };

    try state.pushIf(destination, else_data, 2, 1);

    // If frame duplicates parameters
    try testing.expectEqual(@as(usize, 4), state.stackLen());
    try testing.expectEqual(@as(usize, 1), state.controlStackLen());

    const frame = state.getFrame(0);
    try testing.expectEqual(destination.asU32(), frame.brDestination().asU32());
    try testing.expectEqual(@as(usize, 2), frame.numParamValues());
    try testing.expectEqual(@as(usize, 1), frame.numReturnValues());
}

test "set branched to exit" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    const block0 = Block.fromIndex(0);
    try state.pushBlock(block0, 0, 0);

    var frame = state.getFrameMut(0);
    try testing.expect(!frame.exitIsBranchedTo());

    frame.setBranchedToExit();
    try testing.expect(frame.exitIsBranchedTo());
}

test "truncate value stack to original size" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var state = TranslationState.init(allocator);
    defer state.deinit();

    // Push initial values
    try state.push1(Value.fromIndex(0));
    try state.push1(Value.fromIndex(1));

    // Push a block
    const block0 = Block.fromIndex(0);
    try state.pushBlock(block0, 0, 1);

    // Push more values inside the block
    try state.push1(Value.fromIndex(2));
    try state.push1(Value.fromIndex(3));

    try testing.expectEqual(@as(usize, 4), state.stackLen());

    // Pop the block and truncate
    const frame = state.popFrame();
    frame.truncateValueStackToOriginalSize(&state.stack);

    // Should be back to the original 2 values
    try testing.expectEqual(@as(usize, 2), state.stackLen());
}
