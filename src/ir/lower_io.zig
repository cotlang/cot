//! I/O Lowering
//!
//! Converts AST I/O statements to IR instructions.
//! These are free functions that take a Lowerer pointer as the first parameter.
//!
//! This module handles:
//! - File open/close operations
//! - Read/write operations
//! - Store (ISAM insert) operations
//! - Delete operations

const std = @import("std");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;

// Import Lowerer and types from main module
const lower_mod = @import("lower.zig");
const Lowerer = lower_mod.Lowerer;
const LowerError = lower_mod.LowerError;

// NodeStore types
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;

// ============================================================================
// I/O Operations
// ============================================================================

/// Lower I/O open statement
pub fn lowerIoOpen(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression, not a literal number
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const path_idx: ExprIdx = @enumFromInt(data.b);

    // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
    const channel_val = try l.lowerExpression(channel_idx);
    const path_val = try l.lowerExpression(path_idx);

    try l.emit(.{
        .io_open = .{
            .channel = channel_val,
            .filename = path_val,
            .mode = .update,
        },
    });
}

/// Lower I/O close statement
pub fn lowerIoClose(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const channel_val = try l.lowerExpression(channel_idx);

    try l.emit(.{
        .io_close = .{
            .channel = channel_val,
        },
    });
}

/// Lower I/O read statement
pub fn lowerIoRead(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression, not a literal number
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const buffer_idx: ExprIdx = @enumFromInt(data.b);

    // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
    const channel_val = try l.lowerExpression(channel_idx);

    const buffer_val = try l.lowerLValueFromExpr(buffer_idx);

    // Check if buffer is a struct-typed variable - if so, we need to unpack after read
    var struct_name: ?[]const u8 = null;
    var base_name: ?[]const u8 = null;
    const expr_tag = l.store.exprTag(buffer_idx);
    if (expr_tag == .identifier) {
        const expr_data = l.store.exprData(buffer_idx);
        const name_id = expr_data.getName();
        const name = l.strings.get(name_id);

        if (l.scopes.get(name)) |var_val| {
            if (var_val.ty == .ptr) {
                const pointee_type = var_val.ty.ptr.*;
                if (pointee_type == .@"struct") {
                    struct_name = pointee_type.@"struct".name;
                    base_name = name; // Capture the variable name for slot lookup
                    debug.print(.ir, "lowerIoRead: buffer '{s}' is struct type '{s}'", .{ name, struct_name.? });
                }
            }
        }
    }

    try l.emit(.{
        .io_read = .{
            .channel = channel_val,
            .buffer = buffer_val,
            .key = null,
            .qualifiers = .{},
            .struct_name = struct_name,
            .base_name = base_name,
        },
    });
}

/// Lower I/O write statement
pub fn lowerIoWrite(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const buffer_idx: ExprIdx = @enumFromInt(data.b);

    // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
    const channel_val = try l.lowerExpression(channel_idx);

    // Check if buffer is a struct type variable - if so, we need to serialize it
    const buffer_val = try l.lowerStructBufferOrExpression(buffer_idx);

    try l.emit(.{
        .io_write = .{
            .channel = channel_val,
            .buffer = buffer_val,
            .is_insert = false,
        },
    });
}

/// Lower I/O store statement
pub fn lowerIoStore(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    debug.print(.ir, ">>> lowerIoStore called", .{});
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const buffer_idx: ExprIdx = @enumFromInt(data.b);
    debug.print(.ir, "lowerIoStore: channel_idx={d} buffer_idx={d}", .{ @intFromEnum(channel_idx), @intFromEnum(buffer_idx) });

    // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
    const channel_val = try l.lowerExpression(channel_idx);

    // Check if buffer is a struct type variable - if so, we need to serialize it
    const buffer_val = try l.lowerStructBufferOrExpression(buffer_idx);

    // Store is like write but for ISAM insert
    try l.emit(.{
        .io_write = .{
            .channel = channel_val,
            .buffer = buffer_val,
            .is_insert = true,
        },
    });
}

/// Lower I/O delete statement
pub fn lowerIoDelete(l: *Lowerer, stmt_idx: StmtIdx) LowerError!void {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    _ = func;
    const data = l.store.stmtData(stmt_idx);

    // data.a is an ExprIdx for the channel expression
    const channel_idx: ExprIdx = @enumFromInt(data.a);
    const channel_val = try l.lowerExpression(channel_idx);

    try l.emit(.{
        .io_delete = .{
            .channel = channel_val,
        },
    });
}
