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
    switches_created: u32 = 0,
    functions_inlined: u32 = 0,
    call_sites_inlined: u32 = 0,

    pub fn total(self: OptStats) u32 {
        return self.constants_folded + self.dead_instructions_removed +
            self.dead_blocks_removed + self.tail_calls_optimized +
            self.switches_created + self.call_sites_inlined;
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
    /// Enable switch optimization (convert comparison chains to switch_br)
    switch_optimization: bool = true,
    /// Enable function inlining
    inlining: bool = true,
    /// Enable debug output
    debug: bool = false,
};

/// Run all enabled optimization passes on a module
pub fn optimize(module: *ir.Module, options: OptOptions) OptStats {
    var stats = OptStats{};

    // Run inlining first (module-level pass)
    if (options.inlining) {
        const inline_stats = inlineFunctions(module, .{});
        stats.functions_inlined = inline_stats.functions_inlined;
        stats.call_sites_inlined = inline_stats.call_sites_inlined;
    }

    // Run passes on each function
    for (module.functions.items) |func| {
        if (options.constant_folding) {
            stats.constants_folded += constantFoldFunction(func);
        }
        if (options.switch_optimization) {
            const switch_stats = optimizeSwitches(func);
            stats.switches_created += switch_stats.switches_created;
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
        // Arithmetic operations (Cranelift names)
        .iadd => |op| return tryFoldBinaryArith(block, idx, op, .add),
        .isub => |op| return tryFoldBinaryArith(block, idx, op, .sub),
        .imul => |op| return tryFoldBinaryArith(block, idx, op, .mul),
        .sdiv, .udiv => |op| return tryFoldBinaryArith(block, idx, op, .div),
        .srem, .urem => |op| return tryFoldBinaryArith(block, idx, op, .mod),

        // Comparison operations (Cranelift style - icmp with condition)
        .icmp => |op| return tryFoldIcmp(block, idx, op),

        // Logical operations
        .log_and => |op| return tryFoldLogical(block, idx, op, .@"and"),
        .log_or => |op| return tryFoldLogical(block, idx, op, .@"or"),
        .log_not => |op| return tryFoldLogicalNot(block, idx, op),

        // Unary negation (Cranelift name)
        .ineg => |op| return tryFoldNegation(block, idx, op),

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

        // Replace with iconst (Cranelift name)
        block.instructions.items[idx] = .{ .iconst = .{
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

        // Replace with f64const (Cranelift name)
        block.instructions.items[idx] = .{ .f64const = .{
            .value = result,
            .result = op.result,
        } };
        return true;
    }

    return false;
}

/// Try to fold an icmp comparison operation with constant operands (Cranelift style)
fn tryFoldIcmp(block: *ir.Block, idx: usize, op: ir.Instruction.IcmpOp) bool {
    const lhs_const = findConstantValue(block, op.lhs);
    const rhs_const = findConstantValue(block, op.rhs);

    if (lhs_const == null or rhs_const == null) return false;

    const lhs_val = lhs_const.?;
    const rhs_val = rhs_const.?;

    var result: bool = false;

    if (lhs_val == .int and rhs_val == .int) {
        result = switch (op.cond) {
            .eq => lhs_val.int == rhs_val.int,
            .ne => lhs_val.int != rhs_val.int,
            .slt, .ult => lhs_val.int < rhs_val.int,
            .sle, .ule => lhs_val.int <= rhs_val.int,
            .sgt, .ugt => lhs_val.int > rhs_val.int,
            .sge, .uge => lhs_val.int >= rhs_val.int,
        };
    } else if (lhs_val == .float and rhs_val == .float) {
        result = switch (op.cond) {
            .eq => lhs_val.float == rhs_val.float,
            .ne => lhs_val.float != rhs_val.float,
            .slt, .ult => lhs_val.float < rhs_val.float,
            .sle, .ule => lhs_val.float <= rhs_val.float,
            .sgt, .ugt => lhs_val.float > rhs_val.float,
            .sge, .uge => lhs_val.float >= rhs_val.float,
        };
    } else if (lhs_val == .bool and rhs_val == .bool) {
        result = switch (op.cond) {
            .eq => lhs_val.bool == rhs_val.bool,
            .ne => lhs_val.bool != rhs_val.bool,
            else => return false, // Can't do < > <= >= on bools
        };
    } else {
        return false;
    }

    // Replace with iconst (booleans represented as 0 or 1)
    block.instructions.items[idx] = .{ .iconst = .{
        .ty = .bool,
        .value = if (result) 1 else 0,
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

    // Use iconst with 0 or 1 for boolean result (Cranelift style)
    block.instructions.items[idx] = .{ .iconst = .{
        .ty = .bool,
        .value = if (result) 1 else 0,
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

    // Use iconst with 0 or 1 for boolean result (Cranelift style)
    block.instructions.items[idx] = .{ .iconst = .{
        .ty = .bool,
        .value = if (!val.bool) 1 else 0,
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
        block.instructions.items[idx] = .{ .iconst = .{
            .ty = op.result.ty,
            .value = -%val.int,
            .result = op.result,
        } };
        return true;
    }

    if (val == .float) {
        block.instructions.items[idx] = .{ .f64const = .{
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
            .iconst => |c| {
                if (c.result.id == value.id) {
                    // iconst can represent both integers and booleans
                    if (c.ty == .bool) {
                        return .{ .bool = c.value != 0 };
                    }
                    return .{ .int = c.value };
                }
            },
            .f32const => |c| {
                if (c.result.id == value.id) {
                    return .{ .float = @floatCast(c.value) };
                }
            },
            .f64const => |c| {
                if (c.result.id == value.id) {
                    return .{ .float = c.value };
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
        // Arithmetic (Cranelift names)
        .iadd, .isub, .imul, .sdiv, .udiv, .srem, .urem => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        // Rounding (DBL legacy)
        .round, .trunc => |op| {
            used.put(op.value.id, {}) catch {};
            used.put(op.places.id, {}) catch {};
        },
        // Comparison (Cranelift style)
        .icmp => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        .log_and, .log_or => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        // Unary (Cranelift names)
        .ineg, .log_not, .bnot => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .store => |s| {
            used.put(s.ptr.id, {}) catch {};
            used.put(s.value.id, {}) catch {};
        },
        .load => |l| {
            used.put(l.ptr.id, {}) catch {};
        },
        // Control flow (Cranelift names)
        .brif => |cb| {
            used.put(cb.condition.id, {}) catch {};
        },
        .return_ => |r| {
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
        .call_indirect => |c| {
            used.put(c.callee.id, {}) catch {};
            for (c.args) |arg| {
                used.put(arg.id, {}) catch {};
            }
        },
        .br_table => |s| {
            used.put(s.value.id, {}) catch {};
        },
        // Type conversions (Cranelift names)
        .bitcast => |c| {
            used.put(c.operand.id, {}) catch {};
        },
        .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .ireduce => |op| {
            used.put(op.operand.id, {}) catch {};
        },
        .format_decimal => |f| {
            used.put(f.value.id, {}) catch {};
        },
        .parse_decimal => |p| {
            used.put(p.value.id, {}) catch {};
        },
        .sextend, .uextend => |e| {
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
        // Bitwise (Cranelift names)
        .band, .bor, .bxor, .ishl, .sshr, .ushr => |op| {
            used.put(op.lhs.id, {}) catch {};
            used.put(op.rhs.id, {}) catch {};
        },
        // Trap (Cranelift name)
        .trap => {},
        // These don't use values or are handled specially
        .alloca, .iconst, .f32const, .f64const, .const_string, .const_null,
        .jump, .debug_line, .try_begin, .try_end, .catch_begin,
        .load_struct_buf, .store_struct_buf => {},
        // Map operations
        .map_new => {}, // No input values to mark
        .map_set => |m| {
            used.put(m.map.id, {}) catch {};
            used.put(m.key.id, {}) catch {};
            used.put(m.value.id, {}) catch {};
        },
        .map_get => |m| {
            used.put(m.map.id, {}) catch {};
            used.put(m.key.id, {}) catch {};
        },
        .map_delete => |m| {
            used.put(m.map.id, {}) catch {};
            used.put(m.key.id, {}) catch {};
        },
        .map_has => |m| {
            used.put(m.map.id, {}) catch {};
            used.put(m.key.id, {}) catch {};
        },
        .map_len => |m| {
            used.put(m.map.id, {}) catch {};
        },
        .map_clear => |m| {
            used.put(m.map.id, {}) catch {};
        },
        .map_keys => |m| {
            used.put(m.map.id, {}) catch {};
        },
        .map_values => |m| {
            used.put(m.map.id, {}) catch {};
        },
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
// Switch Optimization (Decision Trees)
// ============================================================================

/// Switch optimization statistics
pub const SwitchStats = struct {
    switches_created: u32 = 0,
    comparisons_eliminated: u32 = 0,
};

/// Optimize sequential match comparisons into switch_br instructions.
///
/// Detects patterns like:
///   %cmp1 = cmp_eq %scrutinee, const1
///   cond_br %cmp1, arm1, check2
///   check2:
///   %cmp2 = cmp_eq %scrutinee, const2
///   cond_br %cmp2, arm2, check3
///   ...
///
/// And converts to:
///   switch_br %scrutinee, [const1 -> arm1, const2 -> arm2, ...], default
///
pub fn optimizeSwitches(func: *ir.Function) SwitchStats {
    var stats = SwitchStats{};

    // Look for chains of icmp eq + brif on the same scrutinee value
    // This is a simplified version - full decision tree compilation would
    // happen during IR lowering, not as an optimization pass

    // For each block, check if it starts a comparison chain
    var block_idx: usize = 0;
    while (block_idx < func.blocks.items.len) : (block_idx += 1) {
        const block = func.blocks.items[block_idx];

        // Try to detect a switch pattern starting from this block
        const result = detectSwitchPattern(func, block);
        if (result.cases.len >= 2) {
            // Found a switch pattern with at least 2 cases
            // Convert to br_table

            // Clear the block and emit br_table
            // Keep instructions before the comparison
            var keep_count: usize = 0;
            for (block.instructions.items, 0..) |inst, i| {
                if (inst == .icmp and inst.icmp.cond == .eq) {
                    keep_count = i;
                    break;
                }
            }

            // Truncate to keep only pre-comparison instructions
            block.instructions.items.len = keep_count;

            // Create cases slice
            var cases = func.allocator.alloc(ir.Instruction.Switch.Case, result.cases.len) catch continue;
            for (result.cases, 0..) |c, i| {
                cases[i] = .{
                    .value = c.value,
                    .target = c.target,
                };
            }

            // Emit br_table (Cranelift name for switch_br)
            block.append(.{ .br_table = .{
                .value = result.scrutinee,
                .cases = cases,
                .default = result.default,
            } }) catch continue;

            stats.switches_created += 1;
            stats.comparisons_eliminated += @intCast(result.cases.len);

            // Skip blocks that were part of this switch chain
            // (they're now dead code and will be cleaned up by DCE)
        }
    }

    return stats;
}

const SwitchCase = struct {
    value: i64,
    target: *ir.Block,
};

const SwitchPattern = struct {
    scrutinee: ir.Value,
    cases: []const SwitchCase,
    default: *ir.Block,
};

/// Detect if a block starts a chain of comparisons that can be converted to a switch
fn detectSwitchPattern(func: *ir.Function, start_block: *ir.Block) SwitchPattern {
    var cases_buf: [64]SwitchCase = undefined;
    var case_count: usize = 0;
    var scrutinee: ?ir.Value = null;
    var current_block = start_block;
    var default_block: ?*ir.Block = null;

    // Walk the comparison chain
    while (case_count < 64) {
        const insts = current_block.instructions.items;
        if (insts.len < 2) break;

        // Look for icmp eq + brif pattern at the end (Cranelift names)
        var cmp_idx: ?usize = null;
        var brif_idx: ?usize = null;

        for (insts, 0..) |inst, i| {
            if (inst == .icmp and inst.icmp.cond == .eq) {
                cmp_idx = i;
            } else if (inst == .brif) {
                brif_idx = i;
            }
        }

        if (cmp_idx == null or brif_idx == null) break;
        if (brif_idx.? != insts.len - 1) break; // brif must be last
        if (brif_idx.? <= cmp_idx.?) break; // brif must be after cmp

        const cmp = insts[cmp_idx.?].icmp;
        const brif = insts[brif_idx.?].brif;

        // Check that brif uses the comparison result
        if (cmp.result.id != brif.condition.id) break;

        // Get the scrutinee (the value being compared)
        const this_scrutinee = cmp.lhs;

        // First iteration: record the scrutinee
        if (scrutinee == null) {
            scrutinee = this_scrutinee;
        } else {
            // Subsequent iterations: verify same scrutinee
            if (scrutinee.?.id != this_scrutinee.id) break;
        }

        // Get the constant value being compared against
        const const_val = findConstantValueById(current_block, cmp.rhs.id) orelse break;
        if (const_val != .int) break;

        // Record this case
        cases_buf[case_count] = .{
            .value = const_val.int,
            .target = brif.then_block,
        };
        case_count += 1;

        // Move to the else block (next comparison in chain)
        const next_block = brif.else_block;

        // Check if next block is a continuation of the chain or the default
        // A continuation has same pattern: compare same scrutinee
        var is_continuation = false;
        for (next_block.instructions.items) |inst| {
            if (inst == .icmp and inst.icmp.cond == .eq) {
                const next_cmp = inst.icmp;
                if (next_cmp.lhs.id == scrutinee.?.id) {
                    is_continuation = true;
                    break;
                }
            }
        }

        if (is_continuation) {
            current_block = next_block;
        } else {
            // This is the default block
            default_block = next_block;
            break;
        }
    }

    // Return empty if not enough cases
    if (case_count < 2 or scrutinee == null or default_block == null) {
        return .{
            .scrutinee = ir.Value{ .id = 0, .ty = .void },
            .cases = &[_]SwitchCase{},
            .default = func.entry,
        };
    }

    return .{
        .scrutinee = scrutinee.?,
        .cases = cases_buf[0..case_count],
        .default = default_block.?,
    };
}

/// Find a constant value by its SSA id in a block
fn findConstantValueById(block: *ir.Block, id: u32) ?ConstValue {
    for (block.instructions.items) |inst| {
        switch (inst) {
            .iconst => |c| {
                if (c.result.id == id) {
                    // iconst can represent both integers and booleans
                    if (c.ty == .bool) {
                        return .{ .bool = c.value != 0 };
                    }
                    return .{ .int = c.value };
                }
            },
            .f32const => |c| {
                if (c.result.id == id) return .{ .float = @floatCast(c.value) };
            },
            .f64const => |c| {
                if (c.result.id == id) return .{ .float = c.value };
            },
            else => {},
        }
    }
    return null;
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

        // Pattern: call followed by return_ of the call result (Cranelift name)
        if (last != .return_ or second_last != .call) continue;

        const call = second_last.call;
        const ret_val = last.return_;

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
        block.append(.{ .jump = .{ .target = target } }) catch continue;

        stats.tail_calls_converted += 1;
    }

    return stats;
}

// ============================================================================
// Function Inlining
// ============================================================================

/// Inlining statistics
pub const InlineStats = struct {
    functions_inlined: u32 = 0,
    call_sites_inlined: u32 = 0,
};

/// Inlining options
pub const InlineOptions = struct {
    /// Maximum instruction count for a function to be inlined
    max_instructions: u32 = 20,
    /// Maximum number of times to inline a single function
    max_inline_count: u32 = 5,
};

/// Perform function inlining on a module.
///
/// Inlines small, non-recursive functions at their call sites.
/// This eliminates call overhead and enables further optimizations.
pub fn inlineFunctions(module: *ir.Module, options: InlineOptions) InlineStats {
    var stats = InlineStats{};

    // Build a map of inlineable functions
    var inlineable = std.StringHashMap(InlineCandidate).init(module.allocator);
    defer inlineable.deinit();

    for (module.functions.items) |func| {
        if (isInlineable(func, options)) {
            inlineable.put(func.name, .{
                .func = func,
                .inline_count = 0,
            }) catch continue;
        }
    }

    if (inlineable.count() == 0) return stats;

    // For each function, look for call sites to inline
    for (module.functions.items) |caller| {
        const result = inlineCallsInFunction(caller, &inlineable, options);
        stats.call_sites_inlined += result.inlined;
    }

    // Count unique functions that were inlined
    var iter = inlineable.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.inline_count > 0) {
            stats.functions_inlined += 1;
        }
    }

    return stats;
}

const InlineCandidate = struct {
    func: *ir.Function,
    inline_count: u32,
};

/// Check if a function is suitable for inlining
fn isInlineable(func: *ir.Function, options: InlineOptions) bool {
    // Don't inline external or exported functions
    if (func.linkage != .internal) return false;

    // Count instructions across all blocks
    var total_instructions: u32 = 0;
    for (func.blocks.items) |block| {
        total_instructions += @intCast(block.instructions.items.len);
    }

    // Too large to inline
    if (total_instructions > options.max_instructions) return false;

    // Check for recursion (self-calls)
    for (func.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (inst == .call) {
                if (std.mem.eql(u8, inst.call.callee, func.name)) {
                    return false; // Recursive function
                }
            }
        }
    }

    return true;
}

const InlineResult = struct {
    inlined: u32,
};

/// Inline eligible calls within a function
fn inlineCallsInFunction(
    caller: *ir.Function,
    inlineable: *std.StringHashMap(InlineCandidate),
    options: InlineOptions,
) InlineResult {
    var result = InlineResult{ .inlined = 0 };

    // For simplicity, we only inline calls that are in simple positions:
    // - The call result is used once (or not at all)
    // - The call is not in a complex control flow situation
    //
    // Full inlining would require:
    // 1. Copying all blocks from callee
    // 2. Remapping all SSA values
    // 3. Replacing parameters with arguments
    // 4. Connecting control flow

    for (caller.blocks.items) |block| {
        var i: usize = 0;
        while (i < block.instructions.items.len) {
            const inst = block.instructions.items[i];

            if (inst == .call) {
                const call = inst.call;

                // Check if this is an inlineable function
                if (inlineable.getPtr(call.callee)) |candidate| {
                    if (candidate.inline_count >= options.max_inline_count) {
                        i += 1;
                        continue;
                    }

                    // For now, only inline very simple functions:
                    // Single block with just a return
                    const callee = candidate.func;
                    if (callee.blocks.items.len == 1) {
                        const callee_block = callee.blocks.items[0];

                        // Check for simple pattern: allocas + computation + return
                        if (tryInlineSimpleFunction(caller, block, i, call, callee, callee_block)) {
                            candidate.inline_count += 1;
                            result.inlined += 1;
                            // Don't increment i - we replaced the instruction
                            continue;
                        }
                    }
                }
            }

            i += 1;
        }
    }

    return result;
}

/// Try to inline a simple single-block function
fn tryInlineSimpleFunction(
    caller: *ir.Function,
    block: *ir.Block,
    call_idx: usize,
    call: ir.Instruction.Call,
    _: *ir.Function, // callee - not used currently
    callee_block: *ir.Block,
) bool {
    // Find the return instruction and its value
    var return_value: ?ir.Value = null;
    var return_idx: ?usize = null;

    for (callee_block.instructions.items, 0..) |inst, idx| {
        if (inst == .return_) {
            return_value = inst.return_;
            return_idx = idx;
            break;
        }
    }

    if (return_idx == null) return false;

    // Build parameter -> argument mapping
    var param_map = std.AutoHashMap(u32, ir.Value).init(caller.allocator);
    defer param_map.deinit();

    // Map parameter allocas to argument values
    var param_idx: usize = 0;
    for (callee_block.instructions.items) |inst| {
        if (inst == .alloca) {
            if (param_idx < call.args.len) {
                // Map this alloca's result ID to the argument value
                param_map.put(inst.alloca.result.id, call.args[param_idx]) catch return false;
                param_idx += 1;
            }
        }
    }

    // For very simple functions (just return a constant or parameter),
    // we can inline by replacing the call with the returned value

    if (return_value) |ret_val| {
        // Check if return value is a constant
        for (callee_block.instructions.items) |inst| {
            const result_id = inst.getResult() orelse continue;
            if (result_id.id != ret_val.id) continue;

            switch (inst) {
                .iconst => |c| {
                    // Replace call with constant (Cranelift name)
                    if (call.result) |call_result| {
                        block.instructions.items[call_idx] = .{
                            .iconst = .{
                                .ty = c.ty,
                                .value = c.value,
                                .result = call_result,
                            },
                        };
                        return true;
                    }
                },
                .f32const => |c| {
                    if (call.result) |call_result| {
                        block.instructions.items[call_idx] = .{
                            .f32const = .{
                                .value = c.value,
                                .result = call_result,
                            },
                        };
                        return true;
                    }
                },
                .f64const => |c| {
                    if (call.result) |call_result| {
                        block.instructions.items[call_idx] = .{
                            .f64const = .{
                                .value = c.value,
                                .result = call_result,
                            },
                        };
                        return true;
                    }
                },
                .const_string => |c| {
                    if (call.result) |call_result| {
                        block.instructions.items[call_idx] = .{
                            .const_string = .{
                                .value = c.value,
                                .result = call_result,
                            },
                        };
                        return true;
                    }
                },
                .load => |l| {
                    // If loading from a parameter, substitute the argument
                    if (param_map.get(l.ptr.id)) |arg_val| {
                        if (call.result) |call_result| {
                            // The function just returns one of its parameters
                            // Replace call with a copy/move of the argument
                            // For now, emit a load from where the arg came from
                            // This is a simplification - full inlining would be more complex
                            _ = call_result;
                            _ = arg_val;
                            // Skip for now - needs more complex handling
                        }
                    }
                },
                else => {},
            }
        }
    }

    return false;
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

    // Create: %0 = iconst 10
    //         %1 = iconst 5
    //         %2 = iadd %0, %1  <- should fold to iconst 15
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });
    const v2 = func.newValue(.{ .i64 = {} });

    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .iadd = .{ .lhs = v0, .rhs = v1, .result = v2 } });
    try func.entry.append(.{ .return_ = v2 });

    // Run optimization
    const stats = optimize(&module, .{});

    // Check that one constant was folded
    try std.testing.expectEqual(@as(u32, 1), stats.constants_folded);

    // Check that the iadd was replaced with iconst
    const third_inst = func.entry.instructions.items[2];
    try std.testing.expect(third_inst == .iconst);
    try std.testing.expectEqual(@as(i64, 15), third_inst.iconst.value);
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

    // Create: %0 = iconst 10
    //         %1 = iconst 5
    //         %2 = icmp sgt %0, %1  <- should fold to iconst 1 (true)
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });
    const v2 = func.newValue(.{ .bool = {} });

    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .icmp = .{ .cond = .sgt, .lhs = v0, .rhs = v1, .result = v2 } });
    try func.entry.append(.{ .return_ = v2 });

    const stats = optimize(&module, .{});

    try std.testing.expectEqual(@as(u32, 1), stats.constants_folded);

    // Folded comparison produces iconst with value 1 (true)
    const third_inst = func.entry.instructions.items[2];
    try std.testing.expect(third_inst == .iconst);
    try std.testing.expectEqual(@as(i64, 1), third_inst.iconst.value);
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

    // Create: %0 = iconst 10   <- used
    //         %1 = iconst 5    <- dead (not used)
    //         return_ %0
    const v0 = func.newValue(.{ .i64 = {} });
    const v1 = func.newValue(.{ .i64 = {} });

    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 10, .result = v0 } });
    try func.entry.append(.{ .iconst = .{ .ty = .{ .i64 = {} }, .value = 5, .result = v1 } });
    try func.entry.append(.{ .return_ = v0 });

    const initial_count = func.entry.instructions.items.len;
    const stats = optimize(&module, .{});

    // Check that one dead instruction was removed
    try std.testing.expectEqual(@as(u32, 1), stats.dead_instructions_removed);
    try std.testing.expectEqual(initial_count - 1, func.entry.instructions.items.len);
}
