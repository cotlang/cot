//! Statement Lowering
//!
//! Converts AST statements to IR instructions.
//! These are free functions that take a Lowerer pointer as the first parameter.
//!
//! This module handles:
//! - Block statements
//! - Control flow (if, while, for, loop, break, continue)
//! - Assignments
//! - Variable declarations (let, const)
//! - Return statements
//! - Field views (overlays)

const std = @import("std");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");
const struct_serial = @import("struct_serialization.zig");
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;

// Scoped logging
const log = std.log.scoped(.@"ir-lower");

// Import Lowerer and types from main module
const lower_mod = @import("lower.zig");
const lower_expr = @import("lower_expr.zig");
const Lowerer = lower_mod.Lowerer;
const LowerError = lower_mod.LowerError;

// NodeStore types
const NodeStore = ast.NodeStore;
const StringInterner = ast.StringInterner;
const StringId = ast.StringId;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const TypeIdx = ast.TypeIdx;
const NodeData = ast.NodeData;
const SourceLoc = ast.SourceLoc;
const ForStmtView = ast.ForStmtView;

// Struct serialization helpers
const StructHelper = struct_serial.StructHelper;

// ============================================================================
// Block and Basic Statements
// ============================================================================

/// Lower a block statement (sequence of statements)
pub fn lowerBlock(l: *Lowerer, data: NodeData) LowerError!void {
    // Push a new defer scope for this block
    try l.pushDeferScope();

    // Push a new variable scope for this block
    try l.scopes.push();

    const span = data.getSpan();
    const stmt_indices = l.store.getStmtSpan(span);
    for (stmt_indices) |idx| {
        try l.lowerStatement(@enumFromInt(idx));
    }

    // Emit ARC releases for block-local variables before exiting
    if (l.emit_arc) {
        try emitCurrentScopeReleases(l);
    }

    // Pop variable scope
    l.scopes.pop();

    // Pop defer scope and emit any defers in reverse order
    try l.popDeferScopeAndEmit();
}

/// Lower a break statement
pub fn lowerBreak(l: *Lowerer) LowerError!void {
    if (l.loop_exit_block) |exit_block| {
        // Emit ARC releases for current scope before breaking
        // Note: We only release the current scope; parent scopes will be released
        // when their containing blocks exit normally
        if (l.emit_arc) {
            try emitCurrentScopeReleases(l);
        }

        // Emit defers for the current scope before breaking
        // Note: The block's popDeferScopeAndEmit won't run since we're jumping out
        if (l.defer_scope_marks.items.len > 0) {
            const mark = l.defer_scope_marks.items[l.defer_scope_marks.items.len - 1];
            const defers_in_scope = l.defers.items[mark..];
            var i: usize = defers_in_scope.len;
            while (i > 0) {
                i -= 1;
                try l.lowerStatement(defers_in_scope[i]);
            }
        }
        try l.emit(.{ .jump = .{ .target = exit_block } });
    } else {
        return LowerError.UnsupportedFeature;
    }
}

/// Lower a continue statement
pub fn lowerContinue(l: *Lowerer) LowerError!void {
    if (l.loop_continue_block) |continue_block| {
        // Emit ARC releases for current scope before continuing
        // Note: We only release the current scope; the loop will re-enter
        // and create new values in subsequent iterations
        if (l.emit_arc) {
            try emitCurrentScopeReleases(l);
        }

        // Emit defers for the current scope before continuing
        // Note: The block's popDeferScopeAndEmit won't run since we're jumping
        if (l.defer_scope_marks.items.len > 0) {
            const mark = l.defer_scope_marks.items[l.defer_scope_marks.items.len - 1];
            const defers_in_scope = l.defers.items[mark..];
            var i: usize = defers_in_scope.len;
            while (i > 0) {
                i -= 1;
                try l.lowerStatement(defers_in_scope[i]);
            }
        }
        try l.emit(.{ .jump = .{ .target = continue_block } });
    } else {
        return LowerError.UnsupportedFeature;
    }
}

/// Lower an expression statement (evaluate for side effects)
pub fn lowerExpressionStmt(l: *Lowerer, data: NodeData) LowerError!void {
    const expr_idx = data.getExpr();
    _ = try l.lowerExpression(expr_idx);
}

/// Lower a throw statement
pub fn lowerThrow(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const data = l.store.stmtData(stmt_idx);
    // data.a = error_expr (ExprIdx)
    const error_expr: ExprIdx = @enumFromInt(data.a);

    // Lower the error expression
    const error_value = try l.lowerExpression(error_expr);

    // Emit throw instruction
    try l.emit(.{ .throw = .{
        .value = error_value,
        .loc = null,
    } });
}

// ============================================================================
// Assignment
// ============================================================================

/// Lower an assignment statement
pub fn lowerAssignment(l: *Lowerer, data: NodeData) LowerError!void {
    const target_idx = data.getTarget();
    const value_idx = data.getValue();

    // Check if target is a map index expression: m["key"] = value
    const target_tag = l.store.exprTag(target_idx);
    if (target_tag == .index) {
        const target_data = l.store.exprData(target_idx);
        const object_idx = target_data.getObject();

        // Try to lower the object to see if it's a map
        const object_val = try l.lowerExpression(object_idx);
        if (Lowerer.isMapType(object_val.ty)) {
            // This is a map index assignment: m["key"] = value
            const key_idx = target_data.getIndex();
            const key_val = try l.lowerExpression(key_idx);
            const value = try l.lowerExpression(value_idx);
            try l.emit(.{
                .map_set = .{
                    .map = object_val,
                    .key = key_val,
                    .value = value,
                    .loc = null,
                },
            });
            return;
        }
    }

    // Check if target is a struct-typed variable (for db_read buffer unpacking)
    // Uses StructHelper for centralized struct detection
    if (StructHelper.detectStructTypeFromExpr(&l.scopes, l.store, l.strings, target_idx)) |info| {
        // This is an assignment to a struct variable (e.g., cust = db_read(...))
        // Lower the value (call expression), then emit store_struct_buf
        log.debug("lowerAssignment: struct target '{s}' type '{s}'", .{ info.base_name, info.structName() });
        debug.print(.ir, "lowerAssignment: target '{s}' is struct type '{s}', emitting store_struct_buf", .{ info.base_name, info.structName() });

        const value = try l.lowerExpression(value_idx);
        const inst = StructHelper.makeStoreStructBufInst(info, value);
        try l.emit(inst);
        return;
    }

    // Normal assignment - lower both sides and emit store
    var value = try l.lowerExpression(value_idx);
    const target = try l.lowerLValue(target_idx);

    // DBL compatibility: when storing a numeric value to a [N]u8 array field,
    // format it with zero-padding to the field width
    const target_type = switch (target.ty) {
        .ptr => |p| p.*,
        else => target.ty,
    };

    // Decimal type validation and conversion
    if (target_type == .decimal) {
        if (!value.isNumeric()) {
            // Non-numeric value (string) being assigned to decimal field
            // Emit parse_decimal to validate and convert at runtime
            // This will raise "bad digit" error if string contains non-numeric chars
            debug.print(.ir, "DBL parse_decimal: string value to decimal, emitting parse_decimal for runtime validation", .{});

            const func = l.current_func orelse return LowerError.OutOfMemory;
            const parsed = func.newValue(.i64);
            try l.emit(.{
                .parse_decimal = .{
                    .value = value,
                    .result = parsed,
                },
            });
            value = parsed;
        }
    }

    // Check if target is a [N]u8 array (fixed-length string)
    if (target_type == .array) {
        const arr = target_type.array;
        if (arr.element.* == .u8 and value.isNumeric()) {
            const width = arr.length;
            debug.print(.ir, "DBL decimal format: numeric value to [{}]u8, emitting format_decimal", .{width});

            // Emit format_decimal instruction to convert integer to zero-padded string
            const func = l.current_func orelse return LowerError.OutOfMemory;

            // Create [N]u8 array type for result
            const u8_type_ptr = try l.allocator.create(ir.Type);
            u8_type_ptr.* = .u8;
            try l.allocated_types.append(l.allocator, u8_type_ptr);
            const formatted = func.newValue(.{ .array = .{ .element = u8_type_ptr, .length = width } });

            try l.emit(.{
                .format_decimal = .{
                    .value = value,
                    .width = width,
                    .result = formatted,
                },
            });
            value = formatted;
        }
    }

    // Check if target is a weak reference type
    // If so, emit weak_ref to create a weak reference instead of a normal store
    if (target_type == .weak) {
        const func = l.current_func orelse return LowerError.OutOfMemory;

        // Create weak reference from the value
        const inner_type = target_type.weak.*;
        const weak_value = func.newValue(.{ .weak = target_type.weak });
        try l.emit(.{
            .weak_ref = .{
                .operand = value,
                .result = weak_value,
            },
        });

        // Store the weak reference
        try l.emit(.{
            .store = .{
                .ptr = target,
                .value = weak_value,
            },
        });
        _ = inner_type;
        return;
    }

    // Emit ARC operations if enabled
    if (l.emit_arc) {
        const arc_target_type = switch (target.ty) {
            .ptr => |p| p.*,
            else => target.ty,
        };

        // If target type needs ARC, release the old value before storing new
        if (arc_target_type.needsArc()) {
            const func = l.current_func orelse return LowerError.OutOfMemory;

            // Load old value from target
            const old_value = func.newValue(arc_target_type);
            try l.emit(.{
                .load = .{
                    .ptr = target,
                    .result = old_value,
                },
            });

            // Release old value (no-op if null/inline)
            try l.emit(.{ .arc_release = .{ .value = old_value } });
        }

        // Retain new value if it needs ARC
        if (value.needsArc()) {
            try l.emit(.{ .arc_retain = .{ .value = value } });
        }
    }

    try l.emit(.{
        .store = .{
            .ptr = target,
            .value = value,
        },
    });
}

// ============================================================================
// Control Flow
// ============================================================================

/// Lower an if statement
pub fn lowerIf(l: *Lowerer, _: StmtIdx, data: NodeData) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;

    const cond_idx = data.getCondition();
    const then_body = data.getThenBody();

    // Get else body from extra_data
    // The data packing puts else_body index in extra_data
    const extra_idx_raw = data.b & 0xFFFF;
    const else_body_raw = l.store.extra_data.items[extra_idx_raw];
    const else_body: StmtIdx = @enumFromInt(else_body_raw);

    // Evaluate condition
    const cond_val = try l.lowerExpression(cond_idx);

    // Create blocks
    const then_block = try func.createBlock("if.then");
    const else_block = try func.createBlock("if.else");
    const merge_block = try func.createBlock("if.merge");

    // Branch based on condition
    try l.emit(.{
        .brif = .{
            .condition = cond_val,
            .then_block = then_block,
            .else_block = else_block,
        },
    });

    // Then block
    l.current_block = then_block;
    try l.lowerStatement(then_body);
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = merge_block } });
    }

    // Else block
    l.current_block = else_block;
    if (else_body != .null) {
        try l.lowerStatement(else_body);
    }
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = merge_block } });
    }

    // Continue at merge block
    l.current_block = merge_block;
}

/// Lower a return statement
pub fn lowerReturn(l: *Lowerer, data: NodeData) LowerError!void {
    const value_idx = data.getReturnValue();

    // Evaluate return value first (before defers, in case they have side effects)
    const return_value = if (value_idx != .null)
        try l.lowerExpression(value_idx)
    else
        null;

    // Emit all defers before returning (in reverse order, LIFO)
    try l.emitAllDefers();

    // Emit ARC operations for return
    if (l.emit_arc) {
        // Retain return value (caller will own it)
        if (return_value) |rv| {
            if (rv.needsArc()) {
                try l.emit(.{ .arc_retain = .{ .value = rv } });
            }
        }

        // Release all locals in current scope chain before returning
        try emitScopeReleases(l);
    }

    // Now emit the actual return
    try l.emit(.{ .return_ = return_value });
}

/// Emit arc_release for all locals that need ARC in the current scope chain.
/// Called at function return to clean up all locals across all scopes.
fn emitScopeReleases(l: *Lowerer) LowerError!void {
    const func = l.current_func orelse return;

    // Iterate all visible variables in scope chain
    var iter = l.scopes.visibleVariables();
    while (iter.next()) |entry| {
        try emitReleaseForVariable(l, func, entry.value);
    }
}

/// Emit arc_release for locals in only the current scope (not parent scopes).
/// Called when exiting a block, break, or continue to release block-local variables.
fn emitCurrentScopeReleases(l: *Lowerer) LowerError!void {
    const func = l.current_func orelse return;

    // Iterate only current scope's variables
    var iter = l.scopes.currentScopeVariables();
    while (iter.next()) |entry| {
        try emitReleaseForVariable(l, func, entry.value);
    }
}

/// Helper: Emit arc_release for a single variable if it needs ARC
fn emitReleaseForVariable(l: *Lowerer, func: *ir.Function, local_ptr: ir.Value) LowerError!void {
    // Get the pointed-to type
    const value_type = switch (local_ptr.ty) {
        .ptr => |p| p.*,
        else => return, // Not a pointer, skip
    };

    // If the type needs ARC, load and release
    if (value_type.needsArc()) {
        // Load the value from the local
        const loaded = func.newValue(value_type);
        try l.emit(.{
            .load = .{
                .ptr = local_ptr,
                .result = loaded,
            },
        });

        // Release it
        try l.emit(.{ .arc_release = .{ .value = loaded } });
    }
}

/// Lower a while loop
pub fn lowerWhile(l: *Lowerer, data: NodeData) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;

    const cond_idx = data.getCondition();
    const body_idx = data.getBody();

    // Create blocks
    const cond_block = try func.createBlock("while.cond");
    const body_block = try func.createBlock("while.body");
    const exit_block = try func.createBlock("while.exit");

    // Save and set loop context
    const prev_exit = l.loop_exit_block;
    const prev_continue = l.loop_continue_block;
    l.loop_exit_block = exit_block;
    l.loop_continue_block = cond_block;

    // Jump to condition
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Condition block
    l.current_block = cond_block;
    const cond_val = try l.lowerExpression(cond_idx);
    try l.emit(.{
        .brif = .{
            .condition = cond_val,
            .then_block = body_block,
            .else_block = exit_block,
        },
    });

    // Body block
    l.current_block = body_block;
    try l.lowerStatement(body_idx);
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = cond_block } });
    }

    // Restore loop context
    l.loop_exit_block = prev_exit;
    l.loop_continue_block = prev_continue;

    // Continue at exit block
    l.current_block = exit_block;
}

/// Lower a for loop
pub fn lowerFor(l: *Lowerer, idx: StmtIdx, _: NodeData) LowerError!void {
    log.debug("lowerFor: entering", .{});

    const func = l.current_func orelse return LowerError.OutOfMemory;

    // Use ForStmtView to correctly extract packed data
    const for_view = ForStmtView.from(l.store, idx);
    const binding_id = for_view.binding;
    const iterable_idx = for_view.iterable;
    const body_idx = for_view.body;

    log.debug("lowerFor: iterable_idx = {d}, body_idx = {d}", .{ @intFromEnum(iterable_idx), @intFromEnum(body_idx) });

    const binding_name = l.strings.get(binding_id);
    if (binding_name.len == 0) return LowerError.UndefinedVariable;

    // Create loop index variable with internal name
    // For range iteration, we'll copy this to binding_name
    // For collection iteration, binding_name will be the element
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = .i64;
    try l.allocated_types.append(l.allocator, ty_ptr);

    const loop_var = func.newValue(.{ .ptr = ty_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = .i64,
            .name = "_iter_idx", // Internal name to avoid conflict with binding_name
            .result = loop_var,
        },
    });
    // Don't bind to scope yet - for range iteration, we'll update the scope below

    // Create blocks
    const cond_block = try func.createBlock("for.cond");
    const body_block = try func.createBlock("for.body");
    const incr_block = try func.createBlock("for.incr");
    const exit_block = try func.createBlock("for.exit");

    // Save and set loop context
    const prev_exit = l.loop_exit_block;
    const prev_continue = l.loop_continue_block;
    l.loop_exit_block = exit_block;
    l.loop_continue_block = incr_block;

    // Lower the iterable expression and extract range bounds
    // Check if iterable is a binary range expression (start..end or start..=end)
    var start_val: ir.Value = undefined;
    var end_val: ir.Value = undefined;
    var is_inclusive = false;

    if (l.store.exprTag(iterable_idx) == .binary) {
        const bin_data = l.store.exprData(iterable_idx);
        const op = bin_data.getBinaryOp();
        if (op == .range or op == .range_inclusive) {
            is_inclusive = (op == .range_inclusive);
            start_val = try lower_expr.lowerExpression(l, bin_data.getLhs());
            end_val = try lower_expr.lowerExpression(l, bin_data.getRhs());
        } else {
            // Not a range expression - try as iterable collection
            const iterable_val = try lower_expr.lowerExpression(l, iterable_idx);
            return lowerCollectionIteration(l, func, binding_name, iterable_val, body_idx, cond_block, body_block, incr_block, exit_block, loop_var, prev_exit, prev_continue);
        }
    } else {
        // Not a binary expression - could be a variable holding a collection
        const iterable_val = try lower_expr.lowerExpression(l, iterable_idx);
        return lowerCollectionIteration(l, func, binding_name, iterable_val, body_idx, cond_block, body_block, incr_block, exit_block, loop_var, prev_exit, prev_continue);
    }

    // For range iteration, the loop variable IS the user-visible binding
    try l.scopes.put(binding_name, loop_var);

    // Initialize loop variable to range start
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = start_val } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Condition block - check loop_var <= end (or < for non-inclusive)
    l.current_block = cond_block;
    const current_val = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = current_val } });
    const cond_result = func.newValue(.bool);
    const cmp_cond: ir.IntCC = if (is_inclusive) .sle else .slt;
    try l.emit(.{ .icmp = .{ .cond = cmp_cond, .lhs = current_val, .rhs = end_val, .result = cond_result } });
    try l.emit(.{ .brif = .{ .condition = cond_result, .then_block = body_block, .else_block = exit_block } });

    // Body block
    l.current_block = body_block;
    try l.lowerStatement(body_idx);
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = incr_block } });
    }

    // Increment block - loop_var = loop_var + 1
    l.current_block = incr_block;
    const loaded_val = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = loaded_val } });
    const one = func.newValue(.i64);
    try l.emit(.{ .iconst = .{ .ty = .i64, .value = 1, .result = one } });
    const incremented = func.newValue(.i64);
    try l.emit(.{ .iadd = .{ .lhs = loaded_val, .rhs = one, .result = incremented } });
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = incremented } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Restore loop context
    l.loop_exit_block = prev_exit;
    l.loop_continue_block = prev_continue;

    // Continue at exit block
    l.current_block = exit_block;
}

/// Lower collection iteration (arrays, maps)
/// Called when for-in iterable is not a range expression
fn lowerCollectionIteration(
    l: *Lowerer,
    func: *ir.Function,
    binding_name: []const u8,
    iterable_val: ir.Value,
    body_idx: StmtIdx,
    cond_block: *ir.Block,
    body_block: *ir.Block,
    incr_block: *ir.Block,
    exit_block: *ir.Block,
    loop_var: ir.Value,
    prev_exit: ?*ir.Block,
    prev_continue: ?*ir.Block,
) LowerError!void {
    // Check the type of the iterable to determine how to iterate
    const iterable_type = iterable_val.ty;

    log.debug("lowerCollectionIteration: iterable_type tag = {s}", .{@tagName(iterable_type)});

    // Handle pointer to array (common case from array literals)
    var element_type: ir.Type = undefined;
    var array_val = iterable_val;

    switch (iterable_type) {
        .ptr => |ptr_type| {
            log.debug("lowerCollectionIteration: ptr_type.* tag = {s}", .{@tagName(ptr_type.*)});
            switch (ptr_type.*) {
                .array => |arr| {
                    log.debug("lowerCollectionIteration: ptr to array, element type = {s}", .{@tagName(arr.element.*)});
                    element_type = arr.element.*;
                    array_val = iterable_val;
                },
                else => return LowerError.UnsupportedFeature,
            }
        },
        .array => |arr| {
            element_type = arr.element.*;
            array_val = iterable_val;
        },
        .slice => |elem_ptr| {
            element_type = elem_ptr.*;
            array_val = iterable_val;
        },
        .map => {
            // Map iteration - iterate over keys
            return lowerMapIteration(l, func, binding_name, iterable_val, body_idx, cond_block, body_block, incr_block, exit_block, loop_var, prev_exit, prev_continue);
        },
        else => return LowerError.UnsupportedFeature,
    }

    // Array iteration: use array_len and array_load

    // Get array length and store it in a variable so it persists across blocks
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = .i64;
    try l.allocated_types.append(l.allocator, ty_ptr);
    const len_var = func.newValue(.{ .ptr = ty_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = .i64,
            .name = "_array_len",
            .result = len_var,
        },
    });
    const len_val = func.newValue(.i64);
    try l.emit(.{ .array_len = .{ .operand = array_val, .result = len_val } });
    try l.emit(.{ .store = .{ .ptr = len_var, .value = len_val } });

    // Initialize index counter to 0
    const zero = func.newValue(.i64);
    try l.emit(.{ .iconst = .{ .ty = .i64, .value = 0, .result = zero } });
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = zero } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Condition block: check index < length
    l.current_block = cond_block;
    const current_idx = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = current_idx } });
    const loaded_len = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = len_var, .result = loaded_len } });
    const cond_result = func.newValue(.bool);
    try l.emit(.{ .icmp = .{ .cond = .slt, .lhs = current_idx, .rhs = loaded_len, .result = cond_result } });
    try l.emit(.{ .brif = .{ .condition = cond_result, .then_block = body_block, .else_block = exit_block } });

    // Body block: load element at current index
    l.current_block = body_block;

    // Load current index for array access
    const body_idx_val = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = body_idx_val } });

    // Load element from array
    const element_val = func.newValue(element_type);
    try l.emit(.{ .array_load = .{ .array_ptr = array_val, .index = body_idx_val, .result = element_val } });

    // Create element variable for loop body and bind the loaded value
    // We need to create an alloca for the element and store into it
    const elem_type_ptr = try l.allocator.create(ir.Type);
    elem_type_ptr.* = element_type;
    try l.allocated_types.append(l.allocator, elem_type_ptr);

    const elem_var = func.newValue(.{ .ptr = elem_type_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = element_type,
            .name = binding_name,
            .result = elem_var,
        },
    });
    try l.emit(.{ .store = .{ .ptr = elem_var, .value = element_val } });

    // Temporarily shadow the loop counter variable with the element variable
    // Save old binding and set new one
    const old_binding = l.scopes.get(binding_name);
    try l.scopes.put(binding_name, elem_var);

    // Lower loop body
    try l.lowerStatement(body_idx);

    // Restore old binding if there was one
    if (old_binding) |old_val| {
        try l.scopes.put(binding_name, old_val);
    } else {
        _ = l.scopes.remove(binding_name);
    }

    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = incr_block } });
    }

    // Increment block: index = index + 1
    l.current_block = incr_block;
    const loaded_idx = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = loaded_idx } });
    const incr_one = func.newValue(.i64);
    try l.emit(.{ .iconst = .{ .ty = .i64, .value = 1, .result = incr_one } });
    const incremented = func.newValue(.i64);
    try l.emit(.{ .iadd = .{ .lhs = loaded_idx, .rhs = incr_one, .result = incremented } });
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = incremented } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Restore loop context
    l.loop_exit_block = prev_exit;
    l.loop_continue_block = prev_continue;

    // Continue at exit block
    l.current_block = exit_block;
}

/// Lower map iteration (keys only for now)
fn lowerMapIteration(
    l: *Lowerer,
    func: *ir.Function,
    binding_name: []const u8,
    map_val: ir.Value,
    body_idx: StmtIdx,
    cond_block: *ir.Block,
    body_block: *ir.Block,
    incr_block: *ir.Block,
    exit_block: *ir.Block,
    loop_var: ir.Value,
    prev_exit: ?*ir.Block,
    prev_continue: ?*ir.Block,
) LowerError!void {
    // Map keys are always strings in Cot
    const key_type: ir.Type = .string;
    const key_type_ptr = try l.allocator.create(ir.Type);
    key_type_ptr.* = key_type;
    try l.allocated_types.append(l.allocator, key_type_ptr);

    // Allocate variable for map length so it persists across blocks
    const len_ty_ptr = try l.allocator.create(ir.Type);
    len_ty_ptr.* = .i64;
    try l.allocated_types.append(l.allocator, len_ty_ptr);
    const len_var = func.newValue(.{ .ptr = len_ty_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = .i64,
            .name = "_map_len",
            .result = len_var,
        },
    });

    // Get map length directly using map_len and store it
    const len_val = func.newValue(.i64);
    try l.emit(.{ .map_len = .{ .map = map_val, .result = len_val } });
    try l.emit(.{ .store = .{ .ptr = len_var, .value = len_val } });

    // Initialize index counter to 1 (map_key_at uses 1-based indexing)
    const one = func.newValue(.i64);
    try l.emit(.{ .iconst = .{ .ty = .i64, .value = 1, .result = one } });
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = one } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Condition block: check index <= length (1-based)
    l.current_block = cond_block;
    const current_idx = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = current_idx } });
    const loaded_len = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = len_var, .result = loaded_len } });
    const cond_result = func.newValue(.bool);
    // Use <= because 1-based: index 1..length inclusive
    try l.emit(.{ .icmp = .{ .cond = .sle, .lhs = current_idx, .rhs = loaded_len, .result = cond_result } });
    try l.emit(.{ .brif = .{ .condition = cond_result, .then_block = body_block, .else_block = exit_block } });

    // Body block: get key at current index using map_key_at
    l.current_block = body_block;

    // Load current index for map_key_at access
    const body_idx_val = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = body_idx_val } });

    // Get key from map using map_key_at (1-based index)
    const key_val = func.newValue(key_type);
    try l.emit(.{ .map_key_at = .{ .map = map_val, .index = body_idx_val, .result = key_val } });

    // Create key variable for loop body
    const elem_var = func.newValue(.{ .ptr = key_type_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = key_type,
            .name = binding_name,
            .result = elem_var,
        },
    });
    try l.emit(.{ .store = .{ .ptr = elem_var, .value = key_val } });

    // Bind the key variable
    const old_binding = l.scopes.get(binding_name);
    try l.scopes.put(binding_name, elem_var);

    // Lower loop body
    try l.lowerStatement(body_idx);

    // Restore old binding
    if (old_binding) |old_val| {
        try l.scopes.put(binding_name, old_val);
    } else {
        _ = l.scopes.remove(binding_name);
    }

    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = incr_block } });
    }

    // Increment block: index = index + 1
    l.current_block = incr_block;
    const loaded_idx = func.newValue(.i64);
    try l.emit(.{ .load = .{ .ptr = loop_var, .result = loaded_idx } });
    const incr_one = func.newValue(.i64);
    try l.emit(.{ .iconst = .{ .ty = .i64, .value = 1, .result = incr_one } });
    const incremented = func.newValue(.i64);
    try l.emit(.{ .iadd = .{ .lhs = loaded_idx, .rhs = incr_one, .result = incremented } });
    try l.emit(.{ .store = .{ .ptr = loop_var, .value = incremented } });
    try l.emit(.{ .jump = .{ .target = cond_block } });

    // Restore loop context
    l.loop_exit_block = prev_exit;
    l.loop_continue_block = prev_continue;

    // Continue at exit block
    l.current_block = exit_block;
}

/// Lower an infinite loop
pub fn lowerLoop(l: *Lowerer, data: NodeData) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;

    const body_idx: StmtIdx = @enumFromInt(data.a);

    // Create blocks
    const body_block = try func.createBlock("loop.body");
    const exit_block = try func.createBlock("loop.exit");

    // Save and set loop context
    const prev_exit = l.loop_exit_block;
    const prev_continue = l.loop_continue_block;
    l.loop_exit_block = exit_block;
    l.loop_continue_block = body_block;

    // Jump to body
    try l.emit(.{ .jump = .{ .target = body_block } });

    // Body block
    l.current_block = body_block;
    try l.lowerStatement(body_idx);
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .jump = .{ .target = body_block } });
    }

    // Restore loop context
    l.loop_exit_block = prev_exit;
    l.loop_continue_block = prev_continue;

    // Continue at exit block
    l.current_block = exit_block;
}

// ============================================================================
// Declarations
// ============================================================================

/// Lower a const declaration (same as let at runtime, but semantically immutable)
pub fn lowerConstDecl(l: *Lowerer, data: NodeData) LowerError!void {
    // Constants are lowered the same as let declarations at runtime
    // The only difference is compile-time enforcement of immutability
    return lowerLetDecl(l, data);
}

/// Lower a let declaration
pub fn lowerLetDecl(l: *Lowerer, data: NodeData) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;

    const name_id = data.getName();
    const name = l.strings.get(name_id);
    if (name.len == 0) return LowerError.UndefinedVariable;

    // Get type and init from packed data
    const type_raw = (data.b >> 16) & 0xFFFF;
    // Check for 16-bit null (0xFFFF) since TypeIdx.null is 32-bit
    const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);
    const extra_idx = data.b & 0xFFFF;
    const init_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_idx]);

    // Determine the variable type - either explicit or inferred from init
    var var_type: ir.Type = undefined;
    var init_val: ?ir.Value = null;

    if (init_idx != .null) {
        // Lower init expression first to get its type for inference
        init_val = try l.lowerExpression(init_idx);
    }

    if (type_idx != .null) {
        // Explicit type annotation
        var_type = try l.lowerTypeIdx(type_idx);
    } else if (init_val) |val| {
        // Infer type from init expression
        var_type = val.ty;
    } else {
        // No type and no init - default to void (error will be caught later)
        var_type = .void;
    }

    // Special case: if init_val is a pointer to a struct (from struct init),
    // use it directly instead of creating a wrapper pointer. This ensures that
    // field_ptr calculations work correctly in the slot-based VM.
    if (init_val) |val| {
        if (val.ty == .ptr) {
            const pointee = val.ty.ptr.*;
            if (pointee == .@"struct") {
                // Register this variable name as an alias to the struct pointer
                // The struct init already allocated slots for the fields
                try l.scopes.put(name, val);
                return;
            }
        }
    }

    // Allocate variable
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = var_type;
    try l.allocated_types.append(l.allocator, ty_ptr);

    const alloca_result = func.newValue(.{ .ptr = ty_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = var_type,
            .name = name,
            .result = alloca_result,
        },
    });
    try l.scopes.put(name, alloca_result);

    // Store init value if present
    if (init_val) |val| {
        // Check if target type is a trait object - need to emit make_trait_object
        if (var_type == .trait_object) {
            const trait_name = var_type.trait_object.trait_name;

            // Get the concrete type name from the value's type
            const type_name = getTypeName(val.ty) orelse {
                debug.print(.ir, "Error: cannot create trait object from non-struct type", .{});
                return LowerError.TypeMismatch;
            };

            // Verify the type implements the trait
            if (!l.implementsTrait(type_name, trait_name)) {
                debug.print(.ir, "Error: type '{s}' does not implement trait '{s}'", .{ type_name, trait_name });
                return LowerError.TypeMismatch;
            }

            // Emit make_trait_object instruction
            const trait_obj_val = func.newValue(var_type);
            try l.emit(.{
                .make_trait_object = .{
                    .value = val,
                    .trait_name = trait_name,
                    .type_name = type_name,
                    .result = trait_obj_val,
                    .loc = null,
                },
            });

            // Store the trait object instead
            try l.emit(.{
                .store = .{
                    .ptr = alloca_result,
                    .value = trait_obj_val,
                },
            });
        } else {
            try l.emit(.{
                .store = .{
                    .ptr = alloca_result,
                    .value = val,
                },
            });
        }
    }
}

/// Get the type name from an IR type (for struct types)
fn getTypeName(ty: ir.Type) ?[]const u8 {
    return switch (ty) {
        .@"struct" => |s| s.name,
        .ptr => |p| getTypeName(p.*),
        else => null,
    };
}

/// Lower a field view (overlay) declaration
/// A field_view creates an alias to another field's memory, optionally with an offset
pub fn lowerFieldView(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.stmtData(stmt_idx);

    // Get view name from data.a
    const name_id: StringId = @enumFromInt(data.a);
    const name = l.strings.get(name_id);
    if (name.len == 0) return LowerError.UndefinedVariable;

    // Get type and extra_start from data.b
    const type_raw = (data.b >> 16) & 0xFFFF;
    const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);
    const extra_start = data.b & 0xFFFF;

    // Get base_field and offset from extra_data
    const base_field_id: StringId = @enumFromInt(l.store.extra_data.items[extra_start]);
    const offset: i32 = @bitCast(l.store.extra_data.items[extra_start + 1]);
    const base_field_name = l.strings.get(base_field_id);

    // Determine the view type
    var view_type: ir.Type = undefined;
    if (type_idx != .null) {
        view_type = try l.lowerTypeIdx(type_idx);
    } else {
        // Default to [1]u8 (equivalent to DBL a1)
        const u8_type_ptr = try l.allocator.create(ir.Type);
        u8_type_ptr.* = .u8;
        try l.allocated_types.append(l.allocator, u8_type_ptr);
        view_type = .{ .array = .{ .element = u8_type_ptr, .length = 1 } };
    }

    // Look up the base field's memory location
    if (l.scopes.get(base_field_name)) |base_ptr| {
        // Field overlay: view shares the same memory as base_ptr (plus offset)
        // Create a new pointer with the correct view_type, even at offset 0
        // This ensures the type information is preserved when accessing the view

        if (offset == 0) {
            // Same memory location but different type - create a bitcast-like view
            // by using ptr_offset with offset 0 to get a new typed pointer
            const ty_ptr = try l.allocator.create(ir.Type);
            ty_ptr.* = view_type;
            try l.allocated_types.append(l.allocator, ty_ptr);

            const typed_ptr = func.newValue(.{ .ptr = ty_ptr });
            try l.emit(.{
                .ptr_offset = .{
                    .base_ptr = base_ptr,
                    .offset = 0,
                    .result = typed_ptr,
                },
            });
            try l.scopes.put(name, typed_ptr);
            debug.print(.ir, "field_view '{s}' overlays '{s}' with type size", .{ name, base_field_name });
        } else {
            // Offset access uses byte-level pointer arithmetic
            // Create a new pointer that points to base_ptr + offset bytes
            const ty_ptr = try l.allocator.create(ir.Type);
            ty_ptr.* = view_type;
            try l.allocated_types.append(l.allocator, ty_ptr);

            const offset_ptr = func.newValue(.{ .ptr = ty_ptr });
            try l.emit(.{
                .ptr_offset = .{
                    .base_ptr = base_ptr,
                    .offset = offset,
                    .result = offset_ptr,
                },
            });
            try l.scopes.put(name, offset_ptr);
            debug.print(.ir, "field_view '{s}' at offset {d} from '{s}'", .{ name, offset, base_field_name });
        }
    } else {
        // Base field not found - create a regular alloca as fallback
        // This allows code to compile even if the base field declaration comes later
        const ty_ptr = try l.allocator.create(ir.Type);
        ty_ptr.* = view_type;
        try l.allocated_types.append(l.allocator, ty_ptr);

        const alloca_result = func.newValue(.{ .ptr = ty_ptr });
        try l.emit(.{
            .alloca = .{
                .ty = view_type,
                .name = name,
                .result = alloca_result,
            },
        });
        try l.scopes.put(name, alloca_result);
    }
}

/// Lower a top-level let declaration as a global variable
/// This is used for DBL common blocks where globals need to be visible in all functions
pub fn lowerGlobalLetDecl(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const data = l.store.stmtData(stmt_idx);
    const name_id = data.getName();
    const name = l.strings.get(name_id);
    if (name.len == 0) return LowerError.UndefinedVariable;

    // Get type from packed data
    const type_raw = (data.b >> 16) & 0xFFFF;
    const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);

    // Determine the variable type
    var var_type: ir.Type = undefined;
    if (type_idx != .null) {
        var_type = try l.lowerTypeIdx(type_idx);
    } else {
        // Default to void if no type (shouldn't happen for common blocks)
        var_type = .void;
    }

    // Create a placeholder value for the global
    // When functions access this, they'll look it up by name
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = var_type;
    try l.allocated_types.append(l.allocator, ty_ptr);

    // Create a synthetic value for the global
    // In the actual implementation, globals would be allocated at module level
    // For now, we just register the name so functions can find it
    const global_val = ir.Value{
        .id = 0xFFFFFF, // Special ID for globals
        .ty = .{ .ptr = ty_ptr },
    };

    try l.global_variables.put(name, global_val);
    debug.print(.ir, "Registered global: {s}", .{name});
}
