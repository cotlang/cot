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

/// Lower a record block (like block but doesn't introduce a new scope)
/// Used for DBL record blocks where variables should remain visible after the block
pub fn lowerRecordBlock(l: *Lowerer, data: NodeData) LowerError!void {
    // No scope push - variables declared here remain visible in enclosing scope
    const span = data.getSpan();
    const stmt_indices = l.store.getStmtSpan(span);
    for (stmt_indices) |idx| {
        try l.lowerStatement(@enumFromInt(idx));
    }
    // No scope pop - variables stay in enclosing scope
}

/// Lower a break statement
pub fn lowerBreak(l: *Lowerer, loc: SourceLoc) LowerError!void {
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
        l.setErrorContext(
            LowerError.UnsupportedFeature,
            "'break' statement outside of loop",
            .{},
            loc,
            "break can only be used inside while, for, or loop statements",
            .{},
        );
        return LowerError.UnsupportedFeature;
    }
}

/// Lower a continue statement
pub fn lowerContinue(l: *Lowerer, loc: SourceLoc) LowerError!void {
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
        l.setErrorContext(
            LowerError.UnsupportedFeature,
            "'continue' statement outside of loop",
            .{},
            loc,
            "continue can only be used inside while, for, or loop statements",
            .{},
        );
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
pub fn lowerAssignment(l: *Lowerer, data: NodeData, loc: ?ir.SourceLoc) LowerError!void {
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
                    .loc = loc,
                },
            });
            return;
        }
    }

    // Check if target is a member access on a heap record: p.x = value
    // Also handles pointer-to-struct types (from function calls returning *Struct)
    if (target_tag == .member) {
        const target_data = l.store.exprData(target_idx);
        const object_idx = target_data.getObject();
        const field_id = target_data.getField();

        // Try to lower the object to see if it's a heap record or pointer-to-struct
        const object_val = try l.lowerExpression(object_idx);

        // Get struct type from heap_record or ptr-to-struct
        const struct_type: ?*const ir.StructType = if (object_val.ty == .heap_record)
            object_val.ty.heap_record
        else if (object_val.ty == .ptr and object_val.ty.ptr.* == .@"struct")
            object_val.ty.ptr.*.@"struct"
        else
            null;

        if (struct_type) |st| {
            const field_name = l.strings.get(field_id);

            // Find field index in struct type
            for (st.fields, 0..) |field, i| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    const value = try l.lowerExpression(value_idx);
                    try l.emit(.{
                        .store_field_heap = .{
                            .record = object_val,
                            .field_index = @intCast(i),
                            .value = value,
                        },
                    });
                    return;
                }
            }
            // Field not found - fall through to normal handling which will error
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
    if (target_type == .implied_decimal) {
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
                .loc = loc,
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
            .loc = loc,
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
            return lowerCollectionIteration(l, func, binding_name, iterable_val, iterable_idx, body_idx, cond_block, body_block, incr_block, exit_block, loop_var, prev_exit, prev_continue);
        }
    } else {
        // Not a binary expression - could be a variable holding a collection
        const iterable_val = try lower_expr.lowerExpression(l, iterable_idx);
        return lowerCollectionIteration(l, func, binding_name, iterable_val, iterable_idx, body_idx, cond_block, body_block, incr_block, exit_block, loop_var, prev_exit, prev_continue);
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
    iterable_idx: ExprIdx,
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
    const loc = l.store.exprLoc(iterable_idx);

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
                else => {
                    l.setErrorContext(
                        LowerError.UnsupportedFeature,
                        "Cannot iterate over pointer to '{s}'",
                        .{@tagName(ptr_type.*)},
                        loc,
                        "for-in loops can iterate over arrays, slices, and maps",
                        .{},
                    );
                    return LowerError.UnsupportedFeature;
                },
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
        else => {
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Cannot iterate over type '{s}'",
                .{@tagName(iterable_type)},
                loc,
                "for-in loops can iterate over arrays, slices, maps, and ranges (0..n)",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
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
        // Debug: log type resolution
        if (std.posix.getenv("COT_DEBUG_TYPE_CHECK")) |_| {
            var buf: [64]u8 = undefined;
            const type_rules = @import("../compiler/type_rules.zig");
            std.debug.print("[TYPE_CHECK] Variable '{s}' has explicit type: {s}\n", .{
                name,
                type_rules.formatType(var_type, &buf),
            });
        }
    } else if (init_val) |val| {
        // Infer type from init expression
        // If init is a pointer to array (from array init expression),
        // infer the underlying type so `var arr = [1, 2, 3]` has type [3]i64, not *[3]i64.
        // This ensures the array aliasing optimization below is triggered.
        //
        // NOTE: We do NOT strip the pointer for struct types anymore.
        // When the init is a method call returning *Struct (like `items.get(0)`),
        // we need to keep the pointer type so the value can be stored and reloaded.
        if (val.ty == .ptr) {
            const pointee = val.ty.ptr.*;
            if (pointee == .array) {
                var_type = pointee;
            } else {
                var_type = val.ty;
            }
        } else {
            var_type = val.ty;
        }
        // Debug: log type inference
        if (std.posix.getenv("COT_DEBUG_TYPE_CHECK")) |_| {
            var buf: [64]u8 = undefined;
            const type_rules = @import("../compiler/type_rules.zig");
            std.debug.print("[TYPE_CHECK] Variable '{s}' inferred type from init: {s}\n", .{
                name,
                type_rules.formatType(var_type, &buf),
            });
        }
    } else {
        // No type and no init - default to void (error will be caught later)
        var_type = .void;
        // Debug: log when void type is assigned due to missing init
        if (std.posix.getenv("COT_DEBUG_TYPE_CHECK")) |_| {
            std.debug.print("[TYPE_CHECK] Void type assigned to variable '{s}' (init_idx={}, type_idx={})\n", .{
                name,
                @intFromEnum(init_idx),
                @intFromEnum(type_idx),
            });
        }
    }

    // Special case: if init_val is a pointer to an array (from array init),
    // AND the declared type is the array itself (not a pointer type),
    // use the pointer directly instead of creating a wrapper pointer. This ensures that
    // array indexing calculations work correctly in the slot-based VM.
    //
    // NOTE: We do NOT apply this optimization for struct pointers anymore.
    // When the init is a method call returning *Struct (like `items.get(0)`),
    // the pointer value must be stored in a local slot so it can be reloaded
    // after register clobbering. The aliasing optimization assumed the pointer
    // would remain in its register, which is not safe.
    //
    // This does NOT apply when the declared type is a pointer (e.g., var ptr: *Node = &node),
    // because in that case we need the normal *(*Node) storage pattern.
    if (init_val) |val| {
        if (val.ty == .ptr and var_type != .ptr) {
            const pointee = val.ty.ptr.*;
            if (pointee == .array) {
                // Register this variable name as an alias to the array pointer
                // The array init already allocated slots for the elements
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
    if (init_val) |init_v| {
        // Get source location from init expression for error reporting
        const init_loc = if (init_idx != .null) l.store.exprLoc(init_idx) else ast.SourceLoc{ .line = 0, .column = 0 };
        const ir_loc = lower_expr.toIrLoc(init_loc);

        // Fix for empty array/slice literals: lowerArrayInit returns void for empty [],
        // but if we have an explicit slice/list type annotation, create a properly typed value
        var val = init_v;
        if (val.ty == .void) {
            switch (var_type) {
                .slice => {
                    // Empty slice: use the declared slice type
                    val = func.newValue(var_type);
                },
                .list => {
                    // Empty list: use the declared list type
                    val = func.newValue(var_type);
                },
                else => {},
            }
        }

        // Check if target type is a trait object - need to emit make_trait_object
        if (var_type == .trait_object) {
            const trait_name = var_type.trait_object.trait_name;

            // Get the concrete type name from the value's type
            const type_name = getTypeName(val.ty) orelse {
                l.setErrorContext(
                    LowerError.TypeMismatch,
                    "cannot create trait object from non-struct type",
                    .{},
                    init_loc,
                    "lowering variable declaration for '{s}'",
                    .{name},
                );
                return LowerError.TypeMismatch;
            };

            // Verify the type implements the trait
            if (!l.implementsTrait(type_name, trait_name)) {
                l.setErrorContext(
                    LowerError.TypeMismatch,
                    "type '{s}' does not implement trait '{s}'",
                    .{ type_name, trait_name },
                    init_loc,
                    "lowering variable declaration for '{s}'",
                    .{name},
                );
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
                    .loc = ir_loc,
                },
            });
        } else {
            // Debug: log store emission for const/let decls
            if (std.posix.getenv("COT_DEBUG_TYPE_CHECK")) |_| {
                var ptr_buf: [64]u8 = undefined;
                var val_buf: [64]u8 = undefined;
                std.debug.print("[TYPE_CHECK] Store in letDecl: var '{s}', ptr_type={s}, val_type={s}, line={d}\n", .{
                    name,
                    @import("../compiler/type_rules.zig").formatType(alloca_result.ty, &ptr_buf),
                    @import("../compiler/type_rules.zig").formatType(val.ty, &val_buf),
                    ir_loc.line,
                });
            }
            try l.emit(.{
                .store = .{
                    .ptr = alloca_result,
                    .value = val,
                    .loc = ir_loc,
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

    // Get type and init from packed data (same layout as lowerLetDecl)
    const type_raw = (data.b >> 16) & 0xFFFF;
    const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);
    const extra_idx = data.b & 0xFFFF;
    const init_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_idx]);

    // Determine the variable type
    var var_type: ir.Type = undefined;
    if (type_idx != .null) {
        var_type = try l.lowerTypeIdx(type_idx);
    } else if (init_idx != .null) {
        // Try to infer type from init expression (for globals like: var x = new List<T>)
        var_type = try inferTypeFromExpr(l, init_idx);
    } else {
        // Default to void if no type and no init
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

    // Also add to the IR module's globals list so the bytecode emitter knows about it
    // This is needed for store_global/load_global to work correctly
    try l.module.addGlobal(name, var_type, false);

    debug.print(.ir, "Registered global: {s}", .{name});
}

/// Infer the type of an expression without lowering it
/// Used during the globals pass to determine global variable types from init expressions
fn inferTypeFromExpr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Type {
    const tag = l.store.exprTag(expr_idx);
    const data = l.store.exprData(expr_idx);

    switch (tag) {
        .new_expr => {
            // new Type{} or new Type<Args>{}
            // data.a = type_name (StringId), data.b = extra_data start
            const type_name_id: StringId = @enumFromInt(data.a);
            const type_name = l.strings.get(type_name_id);

            const extra_start: ast.ExtraIdx = @enumFromInt(data.b);
            const type_arg_count = l.store.getExtra(extra_start);

            // Check for built-in collection types
            if (std.mem.eql(u8, type_name, "List")) {
                // List<T> - get the element type from type arguments
                if (type_arg_count > 0) {
                    const type_arg_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1));
                    const type_arg_idx: ast.TypeIdx = @enumFromInt(type_arg_raw);
                    const elem_type = try l.lowerTypeIdx(type_arg_idx);

                    const elem_type_ptr = try l.allocator.create(ir.Type);
                    elem_type_ptr.* = elem_type;
                    try l.allocated_types.append(l.allocator, elem_type_ptr);

                    const list_type = try l.allocator.create(ir.ListType);
                    list_type.* = .{ .element_type = elem_type_ptr };
                    try l.module.allocated_list_types.append(l.allocator, list_type);

                    return .{ .list = list_type };
                }
                return .void;
            } else if (std.mem.eql(u8, type_name, "Map")) {
                // Map<K, V> - get key and value types
                if (type_arg_count >= 2) {
                    const key_type_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1));
                    const val_type_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 2));
                    const key_type_idx: ast.TypeIdx = @enumFromInt(key_type_raw);
                    const val_type_idx: ast.TypeIdx = @enumFromInt(val_type_raw);
                    const key_type = try l.lowerTypeIdx(key_type_idx);
                    const val_type = try l.lowerTypeIdx(val_type_idx);

                    const key_type_ptr = try l.allocator.create(ir.Type);
                    key_type_ptr.* = key_type;
                    try l.allocated_types.append(l.allocator, key_type_ptr);

                    const val_type_ptr = try l.allocator.create(ir.Type);
                    val_type_ptr.* = val_type;
                    try l.allocated_types.append(l.allocator, val_type_ptr);

                    const map_type = try l.allocator.create(ir.MapType);
                    map_type.* = .{ .key_type = key_type_ptr, .value_type = val_type_ptr };

                    return .{ .map = map_type };
                }
                return .void;
            } else {
                // Regular struct type - look it up or instantiate if generic
                if (type_arg_count > 0) {
                    // Generic struct
                    var type_args: std.ArrayListUnmanaged(ir.Type) = .{};
                    defer type_args.deinit(l.allocator);

                    for (0..type_arg_count) |i| {
                        const type_arg_idx = TypeIdx.fromInt(l.store.extra_data.items[@intFromEnum(extra_start) + 1 + i]);
                        const arg_type = try l.lowerTypeIdx(type_arg_idx);
                        try type_args.append(l.allocator, arg_type);
                    }

                    const struct_type = try l.instantiateGenericStruct(type_name, type_args.items) orelse {
                        return .void;
                    };
                    return .{ .heap_record = struct_type };
                } else {
                    // Non-generic struct
                    const struct_type = l.struct_types.get(type_name) orelse {
                        return .void;
                    };
                    return .{ .heap_record = struct_type };
                }
            }
        },
        .int_literal => return .i64,
        .float_literal => return .f64,
        .string_literal => return .string,
        .bool_literal => return .bool,
        else => {
            // For other expression types, default to void
            // The type checker will catch any mismatches
            return .void;
        },
    }
}
