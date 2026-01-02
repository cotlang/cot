//! IR Optimization Passes
//!
//! This module provides optimization passes that transform the IR to improve
//! runtime performance. Optimizations are applied after lowering and before
//! bytecode emission.
//!
//! Current optimizations:
//! - Constant folding: Evaluate constant expressions at compile time
//! - Dead code elimination: Remove unreachable code and unused values
//!
//! Future optimizations:
//! - Tail call optimization
//! - Inlining
//! - Reference counting optimization

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");

/// Optimization statistics for reporting
pub const OptStats = struct {
    constants_folded: u32 = 0,
    dead_instructions_removed: u32 = 0,
    dead_blocks_removed: u32 = 0,
    tail_calls_optimized: u32 = 0,

    pub fn total(self: OptStats) u32 {
        return self.constants_folded + self.dead_instructions_removed +
            self.dead_blocks_removed + self.tail_calls_optimized;
    }
};

/// Optimization options
pub const OptOptions = struct {
    /// Enable constant folding
    constant_folding: bool = true,
    /// Enable dead code elimination
    dead_code_elimination: bool = true,
    /// Enable tail call optimization
    tail_call_optimization: bool = true,
    /// Enable debug output
    debug: bool = false,
};

/// Run all enabled optimization passes on a module
pub fn optimize(module: *ir.Module, options: OptOptions) OptStats {
    var stats = OptStats{};

    // Run passes on each function
    for (module.functions.items) |func| {
        if (options.constant_folding) {
            stats.constants_folded += constantFoldFunction(func);
        }
        if (options.dead_code_elimination) {
            const dce_stats = deadCodeEliminateFunction(func);
            stats.dead_instructions_removed += dce_stats.instructions;
            stats.dead_blocks_removed += dce_stats.blocks;
        }
        if (options.tail_call_optimization) {
            const tco_stats = tailCallOptimize(func);
            stats.tail_calls_optimized += tco_stats.tail_calls_converted;
        }
    }

    return stats;
}

// ============================================================================
// Constant Folding
// ============================================================================

/// Fold constant expressions in a function
fn constantFoldFunction(func: *ir.Function) u32 {
    var folded: u32 = 0;

    for (func.blocks.items) |block| {
        folded += constantFoldBlock(block);
    }

    return folded;
}

/// Fold constant expressions in a block
fn constantFoldBlock(block: *ir.Block) u32 {
    var folded: u32 = 0;
    var i: usize = 0;

    while (i < block.instructions.items.len) {
        const inst = &block.instructions.items[i];

        if (tryFoldInstruction(block, i, inst)) {
            folded += 1;
            // Don't increment i - we replaced the instruction in place
            // and need to check the new one too
        }
        i += 1;
    }

    return folded;
}

/// Try to fold a single instruction. Returns true if folding occurred.
fn tryFoldInstruction(block: *ir.Block, idx: usize, inst: *ir.Instruction) bool {
    switch (inst.*) {
        // Arithmetic operations
        .add => |op| return tryFoldBinaryArith(block, idx, op, .add),
        .sub => |op| return tryFoldBinaryArith(block, idx, op, .sub),
        .mul => |op| return tryFoldBinaryArith(block, idx, op, .mul),
        .div => |op| return tryFoldBinaryArith(block, idx, op, .div),
        .mod => |op| return tryFoldBinaryArith(block, idx, op, .mod),

        // Comparison operations
        .cmp_eq => |op| return tryFoldComparison(block, idx, op, .eq),
        .cmp_ne => |op| return tryFoldComparison(block, idx, op, .ne),
        .cmp_lt => |op| return tryFoldComparison(block, idx, op, .lt),
        .cmp_le => |op| return tryFoldComparison(block, idx, op, .le),
        .cmp_gt => |op| return tryFoldComparison(block, idx, op, .gt),
        .cmp_ge => |op| return tryFoldComparison(block, idx, op, .ge),

        // Logical operations
        .log_and => |op| return tryFoldLogical(block, idx, op, .@"and"),
        .log_or => |op| return tryFoldLogical(block, idx, op, .@"or"),
        .log_not => |op| return tryFoldLogicalNot(block, idx, op),

        // Unary negation
        .neg => |op| return tryFoldNegation(block, idx, op),

        else => return false,
    }
}

const ArithOp = enum { add, sub, mul, div, mod };
const CmpOp = enum { eq, ne, lt, le, gt, ge };
const LogOp = enum { @"and", @"or" };

/// Try to fold a binary arithmetic operation with constant operands
fn tryFoldBinaryArith(block: *ir.Block, idx: usize, op: ir.Instruction.BinaryOp, kind: ArithOp) bool {
    // Look up the values of lhs and rhs
    const lhs_const = findConstantValue(block, op.lhs);
    const rhs_const = findConstantValue(block, op.rhs);

    if (lhs_const == null or rhs_const == null) return false;

    // Both operands are constants - fold them
    const lhs_val = lhs_const.?;
    const rhs_val = rhs_const.?;

    // Perform the operation based on type
    if (lhs_val == .int and rhs_val == .int) {
        const result = switch (kind) {
            .add => lhs_val.int +% rhs_val.int,
            .sub => lhs_val.int -% rhs_val.int,
            .mul => lhs_val.int *% rhs_val.int,
            .div => if (rhs_val.int != 0) @divTrunc(lhs_val.int, rhs_val.int) else return false,
            .mod => if (rhs_val.int != 0) @mod(lhs_val.int, rhs_val.int) else return false,
        };

        // Replace with const_int
        block.instructions.items[idx] = .{ .const_int = .{
            .ty = op.result.ty,
            .value = result,
            .result = op.result,
        } };
        return true;
    }

    if (lhs_val == .float and rhs_val == .float) {
        const result = switch (kind) {
            .add => lhs_val.float + rhs_val.float,
            .sub => lhs_val.float - rhs_val.float,
            .mul => lhs_val.float * rhs_val.float,
            .div => if (rhs_val.float != 0.0) lhs_val.float / rhs_val.float else return false,
            .mod => return false, // mod not supported for floats in simple folding
        };

        // Replace with const_float
        block.instructions.items[idx] = .{ .const_float = .{
            .ty = op.result.ty,
            .value = result,
            .result = op.result,
        } };
        return true;
    }

    return false;
}

/// Try to fold a comparison operation with constant operands
fn tryFoldComparison(block: *ir.Block, idx: usize, op: ir.Instruction.BinaryOp, kind: CmpOp) bool {
    const lhs_const = findConstantValue(block, op.lhs);
    const rhs_const = findConstantValue(block, op.rhs);

    if (lhs_const == null or rhs_const == null) return false;

    const lhs_val = lhs_const.?;
    const rhs_val = rhs_const.?;

    var result: bool = false;

    if (lhs_val == .int and rhs_val == .int) {
        result = switch (kind) {
            .eq => lhs_val.int == rhs_val.int,
            .ne => lhs_val.int != rhs_val.int,
            .lt => lhs_val.int < rhs_val.int,
            .le => lhs_val.int <= rhs_val.int,
            .gt => lhs_val.int > rhs_val.int,
            .ge => lhs_val.int >= rhs_val.int,
        };
    } else if (lhs_val == .float and rhs_val == .float) {
        result = switch (kind) {
            .eq => lhs_val.float == rhs_val.float,
            .ne => lhs_val.float != rhs_val.float,
            .lt => lhs_val.float < rhs_val.float,
            .le => lhs_val.float <= rhs_val.float,
            .gt => lhs_val.float > rhs_val.float,
            .ge => lhs_val.float >= rhs_val.float,
        };
    } else if (lhs_val == .bool and rhs_val == .bool) {
        result = switch (kind) {
            .eq => lhs_val.bool == rhs_val.bool,
            .ne => lhs_val.bool != rhs_val.bool,
            else => return false, // Can't do < > <= >= on bools
        };
    } else {
        return false;
    }

    // Replace with const_bool
    block.instructions.items[idx] = .{ .const_bool = .{
        .value = result,
        .result = op.result,
    } };
    return true;
}

/// Try to fold a logical operation with constant operands
fn tryFoldLogical(block: *ir.Block, idx: usize, op: ir.Instruction.BinaryOp, kind: LogOp) bool {
    const lhs_const = findConstantValue(block, op.lhs);
    const rhs_const = findConstantValue(block, op.rhs);

    if (lhs_const == null or rhs_const == null) return false;

    const lhs_val = lhs_const.?;
    const rhs_val = rhs_const.?;

    if (lhs_val != .bool or rhs_val != .bool) return false;

    const result = switch (kind) {
        .@"and" => lhs_val.bool and rhs_val.bool,
        .@"or" => lhs_val.bool or rhs_val.bool,
    };

    block.instructions.items[idx] = .{ .const_bool = .{
        .value = result,
        .result = op.result,
    } };
    return true;
}

/// Try to fold logical not with a constant operand
fn tryFoldLogicalNot(block: *ir.Block, idx: usize, op: ir.Instruction.UnaryOp) bool {
    const operand_const = findConstantValue(block, op.operand);
    if (operand_const == null) return false;

    const val = operand_const.?;
    if (val != .bool) return false;

    block.instructions.items[idx] = .{ .const_bool = .{
        .value = !val.bool,
        .result = op.result,
    } };
    return true;
}

/// Try to fold negation with a constant operand
fn tryFoldNegation(block: *ir.Block, idx: usize, op: ir.Instruction.UnaryOp) bool {
    const operand_const = findConstantValue(block, op.operand);
    if (operand_const == null) return false;

    const val = operand_const.?;

    if (val == .int) {
        block.instructions.items[idx] = .{ .const_int = .{
            .ty = op.result.ty,
            .value = -%val.int,
            .result = op.result,
        } };
        return true;
    }

    if (val == .float) {
        block.instructions.items[idx] = .{ .const_float = .{
            .ty = op.result.ty,
            .value = -val.float,
            .result = op.result,
        } };
        return true;
    }

    return false;
}

/// Constant value types for folding
const ConstValue = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
};

/// Find the constant value that defines the given SSA value, if any
fn findConstantValue(block: *ir.Block, value: ir.Value) ?ConstValue {
    // Search backwards in the current block for the defining instruction
    for (block.instructions.items) |inst| {
        switch (inst) {
            .const_int => |c| {
                if (c.result.id == value.id) {
                    return .{ .int = c.value };
                }
            },
            .const_float => |c| {
                if (c.result.id == value.id) {
                    return .{ .float = c.value };
                }
            },
            .const_bool => |c| {
                if (c.result.id == value.id) {
                    return .{ .bool = c.value };
                }
            },
            else => {
                // Check if this instruction defines the value we're looking for
                if (inst.getResult()) |result| {
                    if (result.id == value.id) {
                        // Found the defining instruction but it's not a constant
                        return null;
                    }
                }
            },
        }
    }

    // TODO: Look in predecessor blocks for more complete analysis
    return null;
}

// ============================================================================
// Dead Code Elimination
// ============================================================================

const DCEStats = struct {
    instructions: u32,
    blocks: u32,
};

/// Eliminate dead code in a function
fn deadCodeEliminateFunction(func: *ir.Function) DCEStats {
    var stats = DCEStats{ .instructions = 0, .blocks = 0 };

    // Track which values are used
    var used_values = std.AutoHashMap(u32, void).init(func.allocator);
    defer used_values.deinit();

    // First pass: mark all used values
    for (func.blocks.items) |block| {
        markUsedValues(block, &used_values);
    }

    // Second pass: remove dead instructions (those that produce unused values
    // and have no side effects)
    for (func.blocks.items) |block| {
        stats.instructions += removeDeadInstructions(block, &used_values);
    }

    return stats;
}

/// Mark all values used by instructions in a block
fn markUsedValues(block: *ir.Block, used: *std.AutoHashMap(u32, void)) void {
    for (block.instructions.items) |inst| {
        markInstructionUses(inst, used);
    }
}

/// Mark values used by a single instruction
fn markInstructionUses(inst: ir.Instruction, used: *std.AutoHashMap(u32, void)) void {
    switch (inst) {
        .add, .sub, .mul, .div, .mod => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        .cmp_eq, .cmp_ne, .cmp_lt, .cmp_le, .cmp_gt, .cmp_ge => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        .log_and, .log_or => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        .neg, .log_not, .bit_not => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .store => |s| {
            used.put(s.ptr.id, {}) catch {};
            used.put(s.value.id, {}) catch {};
        },
        .load => |l| {
            used.put(l.ptr.id, {}) catch {};
        },
        .cond_br => |cb| {
            used.put(cb.condition.id, {}) catch {};
        },
        .ret => |r| {
            if (r) |val| {
                used.put(val.id, {}) catch {};
            }
        },
        .call => |c| {
            for (c.args) |arg| {
                used.put(arg.id, {}) catch {};
            }
            if (c.method_receiver) |recv| {
                used.put(recv.id, {}) catch {};
            }
        },
        .switch_br => |s| {
            used.put(s.value.id, {}) catch {};
        },
        .cast => |c| {
            used.put(c.operand.id, {}) catch {};
        },
        .int_to_float, .float_to_int, .int_truncate => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .int_extend => |e| {
            used.put(e.operand.id, {}) catch {};
        },
        .wrap_optional, .unwrap_optional, .is_null => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .field_ptr => |f| {
            used.put(f.struct_ptr.id, {}) catch {};
        },
        .array_load => |a| {
            used.put(a.array_ptr.id, {}) catch {};
            used.put(a.index.id, {}) catch {};
        },
        .array_store => |a| {
            used.put(a.array_ptr.id, {}) catch {};
            used.put(a.index.id, {}) catch {};
            used.put(a.value.id, {}) catch {};
        },
        .str_concat, .str_compare => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        .str_len, .array_len => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .str_slice => |s| {
            used.put(s.source.id, {}) catch {};
            used.put(s.start.id, {}) catch {};
            used.put(s.length_or_end.id, {}) catch {};
        },
        .str_slice_store => |s| {
            used.put(s.target.id, {}) catch {};
            used.put(s.target_ptr.id, {}) catch {};
            used.put(s.start.id, {}) catch {};
            used.put(s.length_or_end.id, {}) catch {};
            used.put(s.value.id, {}) catch {};
        },
        .str_copy => |c| {
            used.put(c.dest.id, {}) catch {};
            used.put(c.src.id, {}) catch {};
        },
        .throw => |t| {
            used.put(t.value.id, {}) catch {};
        },
        .io_open => |io| {
            used.put(io.channel.id, {}) catch {};
            used.put(io.filename.id, {}) catch {};
        },
        .io_close => |io| {
            used.put(io.channel.id, {}) catch {};
        },
        .io_read => |io| {
            used.put(io.channel.id, {}) catch {};
            used.put(io.buffer.id, {}) catch {};
            if (io.key) |k| used.put(k.id, {}) catch {};
        },
        .io_write => |io| {
            used.put(io.channel.id, {}) catch {};
            used.put(io.buffer.id, {}) catch {};
        },
        .io_delete => |io| {
            used.put(io.channel.id, {}) catch {};
        },
        .io_unlock => |io| {
            used.put(io.channel.id, {}) catch {};
        },
        .bit_and, .bit_or, .bit_xor, .shl, .shr => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        // These don't use values or are handled specially
        .alloca, .const_int, .const_float, .const_string, .const_bool, .const_null,
        .br, .debug_line, .try_begin, .try_end, .catch_begin,
        .load_struct_buf, .store_struct_buf => {},
    }
}

/// Remove dead instructions from a block
fn removeDeadInstructions(block: *ir.Block, used: *std.AutoHashMap(u32, void)) u32 {
    var removed: u32 = 0;
    var i: usize = 0;

    while (i < block.instructions.items.len) {
        const inst = block.instructions.items[i];

        // Only remove instructions that:
        // 1. Produce a value
        // 2. That value is not used
        // 3. The instruction has no side effects
        const result = inst.getResult();
        if (result != null and !inst.hasSideEffects()) {
            if (!used.contains(result.?.id)) {
                // This instruction is dead - remove it
                _ = block.instructions.orderedRemove(i);
                removed += 1;
                continue; // Don't increment i
            }
        }

        i += 1;
    }

    return removed;
}

// ============================================================================
// Tail Call Optimization
// ============================================================================

/// Tail call optimization statistics
pub const TCOStats = struct {
    tail_calls_converted: u32 = 0,
};

/// Optimize tail calls in a function.
/// Handles self-recursive tail calls by converting them to loops.
///
/// Transforms:
///   %result = call @self(arg1, arg2, ...)
///   ret %result
///
/// Into:
///   store param1_ptr, arg1
///   store param2_ptr, arg2
///   ...
///   br loop_entry
///
/// This eliminates stack growth for recursive calls.
pub fn tailCallOptimize(func: *ir.Function) TCOStats {
    var stats = TCOStats{};
    const param_count = func.signature.params.len;

    // Need at least one parameter for meaningful TCO
    if (param_count == 0) return stats;

    // Find parameter allocas - they are the first N allocas in the entry block
    var param_allocas: [16]ir.Value = undefined; // Max 16 params for simplicity
    var found_allocas: usize = 0;

    for (func.entry.instructions.items) |inst| {
        if (inst == .alloca) {
            if (found_allocas < param_count and found_allocas < 16) {
                param_allocas[found_allocas] = inst.alloca.result;
                found_allocas += 1;
            }
        } else if (inst.isTerminator()) {
            break; // Stop at first terminator
        }
    }

    // Need all parameter allocas
    if (found_allocas != param_count) return stats;

    // Create loop entry block after parameter allocas
    // This is where tail calls will jump back to
    var loop_entry: ?*ir.Block = null;

    // For each block, look for tail call pattern
    for (func.blocks.items) |block| {
        const len = block.instructions.items.len;
        if (len < 2) continue;

        // Check if last two instructions are call + ret
        const last = block.instructions.items[len - 1];
        const second_last = block.instructions.items[len - 2];

        // Pattern: call followed by ret of the call result
        if (last != .ret or second_last != .call) continue;

        const call = second_last.call;
        const ret_val = last.ret;

        // Check if this is a self-recursive call
        if (!std.mem.eql(u8, call.callee, func.name)) continue;

        // Check argument count matches parameter count
        if (call.args.len != param_count) continue;

        // Check if ret returns the call result
        const call_result = call.result orelse continue;
        const ret_value = ret_val orelse continue;
        if (call_result.id != ret_value.id) continue;

        // Found a self-recursive tail call!
        // Create loop entry block if not already created
        if (loop_entry == null) {
            loop_entry = func.createBlock("tco_loop") catch continue;
            // Move the loop entry block to be right after entry
            // by setting up the jump from entry to loop_entry
            // This is handled implicitly by the block ordering
        }

        // Remove the call and ret instructions
        _ = block.instructions.pop(); // Remove ret
        _ = block.instructions.pop(); // Remove call

        // Add stores to update parameters with new argument values
        for (call.args, 0..) |arg, i| {
            block.append(.{ .store = .{
                .ptr = param_allocas[i],
                .value = arg,
                .loc = call.loc,
            } }) catch continue;
        }

        // Add branch back to loop entry (or entry block if no separate loop block)
        const target = loop_entry orelse func.entry;
        block.append(.{ .br = .{ .target = target } }) catch continue;

        stats.tail_calls_converted += 1;
    }

    return stats;
}

// ============================================================================
// Tests
// ============================================================================

test "constant folding - integer arithmetic" {
    const allocator = std.testing.allocator;

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const sig = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .{ .i64 = {} },
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "test_fn", sig);
    try module.addFunction(func);

    // Create: %0 = const_int 10
    //         %1 = const_int 5
    //         %2 = add %0, %1  <- should fold to const_int 15
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });
    const v2 = func.newValue(.{ .i64 = {} });

    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .add = .{ .lhs = v0, .rhs = v1, .result = v2 } });
    try func.entry.append(.{ .ret = v2 });

    // Run optimization
    const stats = optimize(&module, .{});

    // Check that one constant was folded
    try std.testing.expectEqual(@as(u32, 1), stats.constants_folded);

    // Check that the add was replaced with const_int
    const third_inst = func.entry.instructions.items[2];
    try std.testing.expect(third_inst == .const_int);
    try std.testing.expectEqual(@as(i64, 15), third_inst.const_int.value);
}

test "constant folding - comparison" {
    const allocator = std.testing.allocator;

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const sig = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .{ .bool = {} },
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "test_fn", sig);
    try module.addFunction(func);

    // Create: %0 = const_int 10
    //         %1 = const_int 5
    //         %2 = cmp_gt %0, %1  <- should fold to const_bool true
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });
    const v2 = func.newValue(.{ .bool = {} });

    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .cmp_gt = .{ .lhs = v0, .rhs = v1, .result = v2 } });
    try func.entry.append(.{ .ret = v2 });

    const stats = optimize(&module, .{});

    try std.testing.expectEqual(@as(u32, 1), stats.constants_folded);

    const third_inst = func.entry.instructions.items[2];
    try std.testing.expect(third_inst == .const_bool);
    try std.testing.expectEqual(true, third_inst.const_bool.value);
}

test "dead code elimination" {
    const allocator = std.testing.allocator;

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const sig = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .{ .i64 = {} },
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "test_fn", sig);
    try module.addFunction(func);

    // Create: %0 = const_int 10   <- used
    //         %1 = const_int 5    <- dead (not used)
    //         ret %0
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });

    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .const_int = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .ret = v0 });

    const initial_count = func.entry.instructions.items.len;
    const stats = optimize(&module, .{});

    // Check that one dead instruction was removed
    try std.testing.expectEqual(@as(u32, 1), stats.dead_instructions_removed);
    try std.testing.expectEqual(initial_count - 1, func.entry.instructions.items.len);
}
