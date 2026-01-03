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

// Struct serialization helpers
const StructHelper = struct_serial.StructHelper;

// ============================================================================
// Block and Basic Statements
// ============================================================================

/// Lower a block statement (sequence of statements)
pub fn lowerBlock(l: *Lowerer, data: NodeData) LowerError!void {
    const span = data.getSpan();
    const stmt_indices = l.store.getStmtSpan(span);
    for (stmt_indices) |idx| {
        try l.lowerStatement(@enumFromInt(idx));
    }
}

/// Lower a break statement
pub fn lowerBreak(l: *Lowerer) LowerError!void {
    if (l.loop_exit_block) |exit_block| {
        try l.emit(.{ .jump = .{ .target = exit_block } });
    } else {
        return LowerError.UnsupportedFeature;
    }
}

/// Lower a continue statement
pub fn lowerContinue(l: *Lowerer) LowerError!void {
    if (l.loop_continue_block) |continue_block| {
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
pub fn lowerThrow(l: *Lowerer) LowerError!void {
    // Throw generates a return for now (proper exceptions TBD)
    try l.emit(.{ .return_ = null });
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

    if (value_idx != .null) {
        const value = try l.lowerExpression(value_idx);
        try l.emit(.{ .return_ = value });
    } else {
        try l.emit(.{ .return_ = null });
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
pub fn lowerFor(l: *Lowerer, _: StmtIdx, data: NodeData) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;

    const binding_id = data.getBinding();
    const iterable_idx = data.getIterable();

    // Get body from extra_data
    const extra_idx_raw = data.b & 0xFFFF;
    const body_idx: StmtIdx = @enumFromInt(l.store.extra_data.items[extra_idx_raw]);

    const binding_name = l.strings.get(binding_id);
    if (binding_name.len == 0) return LowerError.UndefinedVariable;

    // For now, treat for loops as simple iteration over a range
    // Create loop variable
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = .i64;
    try l.allocated_types.append(l.allocator, ty_ptr);

    const loop_var = func.newValue(.{ .ptr = ty_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = .i64,
            .name = binding_name,
            .result = loop_var,
        },
    });
    try l.scopes.put(binding_name, loop_var);

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
            // Not a range expression - treat as iterable collection (not yet supported)
            return LowerError.UnsupportedFeature;
        }
    } else {
        // Not a binary expression - could be a variable holding a collection
        return LowerError.UnsupportedFeature;
    }

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
        try l.emit(.{
            .store = .{
                .ptr = alloca_result,
                .value = val,
            },
        });
    }
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
        // For offset 0, the view is simply an alias to the same slot
        // For non-zero offsets, we use ptr_offset for byte-level access

        if (offset == 0) {
            // Simple alias - the view name maps to the exact same storage slot as the base
            // No instruction needed - we just register the view name pointing to base_ptr
            try l.scopes.put(name, base_ptr);
            debug.print(.ir, "field_view '{s}' aliased to '{s}' (offset 0)", .{ name, base_field_name });
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
