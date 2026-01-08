//! Expression Lowering
//!
//! Converts AST expressions to IR values.
//! These are free functions that take a Lowerer pointer as the first parameter.
//!
//! This module handles:
//! - Literal expressions (int, float, string, bool, null)
//! - Identifier lookup
//! - Binary and unary operations
//! - Member and index access
//! - Function and method calls
//! - Array and struct initialization
//! - Lambda expressions
//! - Type lowering

const std = @import("std");
const builtin = @import("builtin");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;
const native_types = @import("generated/native_types.zig");

// Scoped logging
const log = std.log.scoped(.@"ir-lower-expr");

// Import Lowerer and types from main module
const lower_mod = @import("lower.zig");
const Lowerer = lower_mod.Lowerer;
const LowerError = lower_mod.LowerError;

// Closure support
const closure = @import("closure.zig");
const FreeVarCollector = closure.FreeVarCollector;

// NodeStore types
const NodeStore = ast.NodeStore;
const StringInterner = ast.StringInterner;
const StringId = ast.StringId;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const TypeIdx = ast.TypeIdx;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;
const InterpStringPartTag = ast.InterpStringPartTag;
const NodeData = ast.NodeData;
const SourceLoc = ast.SourceLoc;
const TypeTag = ast.TypeTag;

// ============================================================================
// Source Location Conversion
// ============================================================================

/// Convert AST SourceLoc to IR SourceLoc for error reporting
pub fn toIrLoc(loc: SourceLoc) ir.SourceLoc {
    return .{
        .line = loc.line,
        .column = loc.column,
    };
}

// ============================================================================
// Qualified Name Extraction for Namespace Chains
// ============================================================================

/// Extract a fully qualified name from a member access chain.
/// For example: `std.math.abs` -> "std.math.abs"
/// Returns null if the expression is not a pure namespace chain (e.g., contains non-identifier base).
fn extractQualifiedName(
    store: *const NodeStore,
    strings: *StringInterner,
    expr_idx: ExprIdx,
    buf: []u8,
) ?[]const u8 {
    const tag = store.exprTag(expr_idx);
    const data = store.exprData(expr_idx);

    switch (tag) {
        .identifier => {
            const name_id = data.getName();
            const name = strings.get(name_id);
            if (name.len > buf.len) return null;
            @memcpy(buf[0..name.len], name);
            return buf[0..name.len];
        },
        .member => {
            // Get the object's qualified name first
            const object_idx = data.getObject();
            const field_id = data.getField();
            const field_name = strings.get(field_id);

            // Recursively get the object's qualified name
            var temp_buf: [256]u8 = undefined;
            const object_name = extractQualifiedName(store, strings, object_idx, &temp_buf) orelse return null;

            // Build "object.field"
            const total_len = object_name.len + 1 + field_name.len;
            if (total_len > buf.len) return null;

            @memcpy(buf[0..object_name.len], object_name);
            buf[object_name.len] = '.';
            @memcpy(buf[object_name.len + 1 ..][0..field_name.len], field_name);
            return buf[0..total_len];
        },
        else => return null, // Not a valid namespace chain
    }
}

/// Check if a qualified name represents a std namespace function
fn isStdNamespaceFunction(name: []const u8) bool {
    // Check for std.* prefix
    return std.mem.startsWith(u8, name, "std.");
}

/// Check if a qualified name represents an imported package function (e.g., "utils.add")
fn isImportedPackageFunction(l: *Lowerer, qualified_name: []const u8) bool {
    // Extract the first component (package name)
    const dot_pos = std.mem.indexOf(u8, qualified_name, ".") orelse return false;
    const package_name = qualified_name[0..dot_pos];

    // Check if this package was imported
    return l.imported_namespaces.contains(package_name);
}

/// Get return type for an imported package function
fn getPackageFunctionReturnType(l: *Lowerer, qualified_name: []const u8) ir.Type {
    // Try to get from fn_return_types (populated from DependencyTypeContext)
    if (l.fn_return_types.get(qualified_name)) |ret_type| {
        return ret_type;
    }

    // Also try just the function name part (after the package prefix)
    if (std.mem.indexOf(u8, qualified_name, ".")) |dot_pos| {
        const fn_name = qualified_name[dot_pos + 1 ..];
        if (l.fn_return_types.get(fn_name)) |ret_type| {
            return ret_type;
        }
    }

    // Default to i64 if unknown (common case for numeric functions)
    return .i64;
}

// ============================================================================
// Main Expression Lowering
// ============================================================================

/// Lower an expression to an IR value
pub fn lowerExpression(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse {
        l.setErrorContext(
            LowerError.UnsupportedFeature,
            "Expression outside of function context",
            .{},
            l.store.exprLoc(expr_idx),
            "lowering expression at top level (no current function)",
            .{},
        );
        return LowerError.UnsupportedFeature;
    };
    const tag = l.store.exprTag(expr_idx);
    const data = l.store.exprData(expr_idx);
    const loc = l.store.exprLoc(expr_idx);

    log.debug("lowerExpression: tag = {s}", .{@tagName(tag)});

    switch (tag) {
        .int_literal => return lowerIntLiteral(l, func, data),
        .float_literal => return lowerFloatLiteral(l, func, data),
        .string_literal => return lowerStringLiteral(l, func, data),
        .bool_literal => return lowerBoolLiteral(l, func, data),
        .null_literal => return lowerNullLiteral(l, func),
        .identifier => return lowerIdentifier(l, func, data, expr_idx),
        .binary => return lowerBinary(l, func, data, expr_idx),
        .unary => return lowerUnary(l, func, data, expr_idx),
        .grouping => return lowerExpression(l, @enumFromInt(data.a)),
        .call => return lowerCall(l, expr_idx),
        .method_call => return lowerMethodCall(l, expr_idx),
        .member => return lowerMember(l, func, expr_idx),
        .index => return lowerIndex(l, func, expr_idx),
        .slice_expr => return lowerSliceExpr(l, func, expr_idx),
        .array_init => return lowerArrayInit(l, expr_idx),
        .struct_init => return lowerStructInit(l, expr_idx),
        .generic_struct_init => return lowerGenericStructInit(l, expr_idx),
        .new_expr => return lowerNewExpr(l, expr_idx),
        .lambda => return lowerLambda(l, expr_idx),
        .range => {
            // Range expressions are lowered by for loop handling, not as standalone values
            // If we get here, a range was used outside of a for loop context
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Range expression (a..b) cannot be used as a standalone value",
                .{},
                loc,
                "range expressions are only valid in for loops (e.g., 'for i in 0..10')",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
        .comptime_builtin => {
            return lowerComptimeBuiltin(l, func, expr_idx, loc);
        },
        .if_expr => {
            return lowerIfExpr(l, func, expr_idx);
        },
        .match_expr, .block_expr => {
            // Expression forms (match/block as expressions) not yet supported
            // These would require SSA phi nodes or temporary variables
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Expression form of '{s}' not yet implemented",
                .{@tagName(tag)},
                loc,
                "use statement form instead (e.g., if/else with assignments inside branches)",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
        .optional_member => return lowerOptionalMember(l, func, expr_idx),
        .optional_index => return lowerOptionalIndex(l, func, expr_idx),
        .is_expr => return lowerIsExpr(l, func, expr_idx),
        .interp_string => return lowerInterpString(l, func, expr_idx),
    }
}

// ============================================================================
// Literal Expressions
// ============================================================================

/// Lower an integer literal
pub fn lowerIntLiteral(l: *Lowerer, func: *ir.Function, data: NodeData) LowerError!ir.Value {
    const value = data.getIntValue();
    const result = func.newValue(.i64);
    try l.emit(.{
        .iconst = .{
            .ty = .i64,
            .value = value,
            .result = result,
        },
    });
    return result;
}

/// Lower a float literal
pub fn lowerFloatLiteral(l: *Lowerer, func: *ir.Function, data: NodeData) LowerError!ir.Value {
    const bits: u64 = (@as(u64, data.b) << 32) | data.a;
    const value: f64 = @bitCast(bits);
    const result = func.newValue(.f64);
    try l.emit(.{
        .f64const = .{
            .value = value,
            .result = result,
        },
    });
    return result;
}

/// Lower a string literal
pub fn lowerStringLiteral(l: *Lowerer, func: *ir.Function, data: NodeData) LowerError!ir.Value {
    const str_id = data.getName();
    const str = l.strings.get(str_id);

    // Create [N]u8 array type for fixed-length string
    const u8_type_ptr = try l.allocator.create(ir.Type);
    u8_type_ptr.* = .u8;
    try l.allocated_types.append(l.allocator, u8_type_ptr);

    const array_type: ir.Type = .{ .array = .{ .element = u8_type_ptr, .length = @intCast(str.len) } };
    const result = func.newValue(array_type);
    try l.emit(.{
        .const_string = .{
            .value = str,
            .result = result,
        },
    });
    return result;
}

/// Lower an interpolated string: "Hello ${name}!"
/// Desugars to a chain of str_concat operations:
/// "Hello ${name}!" => "Hello " ++ to_string(name) ++ "!"
pub fn lowerInterpString(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const parts_info = l.store.getInterpStringParts(expr_idx);
    const count = parts_info.count;
    const parts_data = parts_info.data;

    // Helper to emit an empty string constant
    const emitEmptyString = struct {
        fn emit(lowerer: *Lowerer, f: *ir.Function) LowerError!ir.Value {
            const u8_type_ptr = try lowerer.allocator.create(ir.Type);
            u8_type_ptr.* = .u8;
            try lowerer.allocated_types.append(lowerer.allocator, u8_type_ptr);
            const array_type: ir.Type = .{ .array = .{ .element = u8_type_ptr, .length = 0 } };
            const result = f.newValue(array_type);
            try lowerer.emit(.{ .const_string = .{ .value = "", .result = result } });
            return result;
        }
    }.emit;

    // If no parts, return empty string
    if (count == 0) {
        return emitEmptyString(l, func);
    }

    // Lower each part
    var values = std.ArrayListUnmanaged(ir.Value){};
    defer values.deinit(l.allocator);

    var i: usize = 0;
    while (i < count) : (i += 1) {
        const tag: InterpStringPartTag = @enumFromInt(parts_data[i * 2]);
        const data_val = parts_data[i * 2 + 1];

        const part_value: ir.Value = switch (tag) {
            .string_content => blk: {
                // String content - emit as const_string
                const str_id: StringId = @enumFromInt(data_val);
                const str = l.strings.get(str_id);

                const u8_type_ptr = try l.allocator.create(ir.Type);
                u8_type_ptr.* = .u8;
                try l.allocated_types.append(l.allocator, u8_type_ptr);

                const array_type: ir.Type = .{ .array = .{ .element = u8_type_ptr, .length = @intCast(str.len) } };
                const result = func.newValue(array_type);
                try l.emit(.{ .const_string = .{ .value = str, .result = result } });
                break :blk result;
            },
            .expression => blk: {
                // Expression - lower it, the VM handles auto-conversion to string in str_concat
                const expr: ExprIdx = @enumFromInt(data_val);
                break :blk try lowerExpression(l, expr);
            },
        };

        try values.append(l.allocator, part_value);
    }

    // Interpolated strings always produce strings. If first part is not a string type,
    // prepend an empty string to ensure the concat chain starts with a string.
    // This guarantees type checker validation passes (str_concat requires at least one string).
    var result = values.items[0];
    if (!result.isString()) {
        const empty = try emitEmptyString(l, func);
        const concat_result = func.newValue(.string);
        try l.emit(.{ .str_concat = .{ .lhs = empty, .rhs = result, .result = concat_result } });
        result = concat_result;
    }

    // Concatenate remaining parts using str_concat chain
    // str_concat auto-converts non-strings to string in the VM
    for (values.items[1..]) |part| {
        const concat_result = func.newValue(.string);
        try l.emit(.{ .str_concat = .{ .lhs = result, .rhs = part, .result = concat_result } });
        result = concat_result;
    }

    return result;
}

/// Lower a boolean literal (represented as iconst 0 or 1 to match Cranelift)
pub fn lowerBoolLiteral(l: *Lowerer, func: *ir.Function, data: NodeData) LowerError!ir.Value {
    const value: i64 = if (data.a != 0) 1 else 0;
    const result = func.newValue(.bool);
    try l.emit(.{
        .iconst = .{
            .ty = .bool,
            .value = value,
            .result = result,
        },
    });
    return result;
}

/// Lower a null literal
pub fn lowerNullLiteral(l: *Lowerer, func: *ir.Function) LowerError!ir.Value {
    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = .void;
    try l.allocated_types.append(l.allocator, ty_ptr);
    const result = func.newValue(.{ .optional = ty_ptr });
    try l.emit(.{
        .const_null = .{
            .ty = .{ .optional = ty_ptr },
            .result = result,
        },
    });
    return result;
}

// ============================================================================
// Variable and Identifier Expressions
// ============================================================================

/// Lower an identifier expression (variable load)
pub fn lowerIdentifier(l: *Lowerer, func: *ir.Function, data: NodeData, expr_idx: ExprIdx) LowerError!ir.Value {
    const loc = l.store.exprLoc(expr_idx);
    const name_id = data.getName();
    const name = l.strings.get(name_id);
    if (name.len == 0) {
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "Undefined variable: (empty name)",
            .{},
            loc,
            "lowering identifier expression",
            .{},
        );
        return LowerError.UndefinedVariable;
    }

    // Check if this is a compile-time constant
    if (l.comptime_evaluator.constants.get(name_id)) |ct_value| {
        // Emit the constant value directly using the same patterns as literal lowering
        switch (ct_value) {
            .integer => |i| {
                const result = func.newValue(.i64);
                try l.emit(.{
                    .iconst = .{
                        .ty = .i64,
                        .value = i,
                        .result = result,
                    },
                });
                return result;
            },
            .boolean => |b| {
                const value: i64 = if (b) 1 else 0;
                const result = func.newValue(.bool);
                try l.emit(.{
                    .iconst = .{
                        .ty = .bool,
                        .value = value,
                        .result = result,
                    },
                });
                return result;
            },
            .string => |s| {
                const result = func.newValue(.string);
                try l.emit(.{
                    .const_string = .{
                        .value = s,
                        .result = result,
                    },
                });
                return result;
            },
            .decimal => |d| {
                // Convert scaled integer to float
                const scale: i64 = std.math.powi(i64, 10, d.scale) catch 1;
                const scale_factor: f64 = @floatFromInt(scale);
                const float_val: f64 = @as(f64, @floatFromInt(d.value)) / scale_factor;
                const result = func.newValue(.f64);
                try l.emit(.{
                    .f64const = .{
                        .value = float_val,
                        .result = result,
                    },
                });
                return result;
            },
            .undefined => {
                // Create void optional type for null
                const ty_ptr = try l.allocator.create(ir.Type);
                ty_ptr.* = .void;
                try l.allocated_types.append(l.allocator, ty_ptr);
                const result = func.newValue(.{ .optional = ty_ptr });
                try l.emit(.{
                    .const_null = .{
                        .ty = .{ .optional = ty_ptr },
                        .result = result,
                    },
                });
                return result;
            },
        }
    }

    // Check if this is a captured variable from a closure
    if (l.current_captures) |captures| {
        if (captures.get(name)) |captured_value| {
            // This variable is captured - load from closure environment
            if (l.closure_env_value) |env_value| {
                // Create a string constant for the variable name (map key)
                const key_value = func.newValue(.string);
                try l.emit(.{
                    .const_string = .{
                        .value = name,
                        .result = key_value,
                    },
                });

                // Emit map_get to load from the closure environment
                const result = func.newValue(captured_value.ty);
                try l.emit(.{
                    .map_get = .{
                        .map = env_value,
                        .key = key_value,
                        .result = result,
                    },
                });
                return result;
            }
        }
    }

    // Check local variables first (via scopes), then global variables
    const local_ptr = l.scopes.get(name);
    const global_ptr = if (local_ptr == null) l.global_variables.get(name) else null;

    const ptr = local_ptr orelse global_ptr orelse blk: {
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "Undefined variable: '{s}'",
            .{name},
            loc,
            "lowering identifier expression",
            .{},
        );
        break :blk null;
    } orelse return LowerError.UndefinedVariable;

    // Get the underlying type (dereference pointer)
    const value_type = switch (ptr.ty) {
        .ptr => |p| p.*,
        else => ptr.ty,
    };

    // For global variables, emit an alloca so the bytecode emitter can track the value ID
    // This is necessary because the bytecode emitter's value_slots map is per-function,
    // and the emitAlloca handler checks the globals map and sets the 0x8000 bit.
    if (global_ptr != null) {
        // Create a new value for this reference to the global within this function
        const alloca_result = func.newValue(.{ .ptr = switch (ptr.ty) {
            .ptr => |p| p,
            else => blk: {
                const ty_ptr = l.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
                ty_ptr.* = ptr.ty;
                l.allocated_types.append(l.allocator, ty_ptr) catch return LowerError.OutOfMemory;
                break :blk ty_ptr;
            },
        } });

        // Emit alloca for the global variable
        try l.emit(.{
            .alloca = .{
                .name = name,
                .ty = value_type,
                .result = alloca_result,
            },
        });

        // Now emit load using the alloca result as the ptr
        const result = func.newValue(value_type);
        try l.emit(.{
            .load = .{
                .ptr = alloca_result,
                .result = result,
            },
        });
        return result;
    }

    // Check if this is a weak reference type
    // If so, emit weak_load which returns optional (may be null if target freed)
    if (value_type == .weak) {
        // First load the weak reference itself
        const weak_value = func.newValue(value_type);
        try l.emit(.{
            .load = .{
                .ptr = ptr,
                .result = weak_value,
            },
        });

        // Then emit weak_load to dereference it (may return null)
        // Result type is optional of the inner type
        const inner_type = value_type.weak.*;
        const inner_ptr = l.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
        inner_ptr.* = inner_type;
        try l.allocated_types.append(l.allocator, inner_ptr);

        const result = func.newValue(.{ .optional = inner_ptr });
        try l.emit(.{
            .weak_load = .{
                .operand = weak_value,
                .result = result,
            },
        });
        return result;
    }

    // Load the value from the pointer - result has the dereferenced type (value_type)
    const result = func.newValue(value_type);
    debug.print(.ir, "lowerIdentifier: '{s}' ptr.ty={s} value_type={s} result.ty={s}", .{
        name,
        @tagName(ptr.ty),
        @tagName(value_type),
        @tagName(result.ty),
    });
    try l.emit(.{
        .load = .{
            .ptr = ptr,
            .result = result,
        },
    });
    return result;
}

// ============================================================================
// LValue Expressions (for assignment targets)
// ============================================================================

/// Lower an lvalue expression to get a pointer
pub fn lowerLValue(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const tag = l.store.exprTag(expr_idx);
    const data = l.store.exprData(expr_idx);
    const loc = l.store.exprLoc(expr_idx);

    switch (tag) {
        .identifier => {
            const name_id = data.getName();
            const name = l.strings.get(name_id);
            if (name.len == 0) {
                l.setErrorContext(
                    LowerError.UndefinedVariable,
                    "Undefined lvalue variable: (empty name)",
                    .{},
                    loc,
                    "lowering lvalue identifier",
                    .{},
                );
                return LowerError.UndefinedVariable;
            }

            // Check local variables first (via scopes), then global variables
            if (l.scopes.get(name)) |ptr| {
                return ptr;
            }
            if (l.global_variables.get(name)) |global_ptr| {
                // For global variables, emit an alloca so the emitter can track the value ID
                const func = l.current_func orelse return LowerError.OutOfMemory;

                // Get the underlying type from the pointer
                const var_type = switch (global_ptr.ty) {
                    .ptr => |p| p.*,
                    else => global_ptr.ty,
                };

                // Create a new value with a proper ID
                const result = func.newValue(.{ .ptr = switch (global_ptr.ty) {
                    .ptr => |p| p,
                    else => blk: {
                        const ty_ptr = l.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
                        ty_ptr.* = global_ptr.ty;
                        l.allocated_types.append(l.allocator, ty_ptr) catch return LowerError.OutOfMemory;
                        break :blk ty_ptr;
                    },
                } });

                // Emit alloca for the global variable
                try l.emit(.{
                    .alloca = .{
                        .name = name,
                        .ty = var_type,
                        .result = result,
                    },
                });

                return result;
            }
            l.setErrorContext(
                LowerError.UndefinedVariable,
                "Undefined lvalue variable: '{s}'",
                .{name},
                loc,
                "lowering lvalue identifier",
                .{},
            );
            return LowerError.UndefinedVariable;
        },
        .member => {
            // Member access as lvalue
            return lowerMemberPtr(l, expr_idx);
        },
        .index => {
            // Index access as lvalue
            return lowerIndexPtr(l, expr_idx);
        },
        .unary => {
            // Dereference as lvalue: p.* = value
            const op = data.getUnaryOp();
            if (op == .deref) {
                // The operand is the pointer - just return its value
                const operand_idx = data.getOperand();
                return lowerExpression(l, operand_idx);
            }
            // Other unary ops are not valid lvalues
            l.setErrorContext(
                LowerError.InvalidExpression,
                "Cannot use unary '{s}' expression as assignment target",
                .{@tagName(op)},
                .{ .line = loc.line, .column = loc.column },
                "only dereference can be an lvalue",
                .{},
            );
            return LowerError.InvalidExpression;
        },
        else => {
            const expr_loc = l.store.exprLoc(expr_idx);
            l.setErrorContext(
                LowerError.InvalidExpression,
                "Cannot use '{s}' expression as assignment target",
                .{@tagName(tag)},
                .{ .line = expr_loc.line, .column = expr_loc.column },
                "expression cannot be assigned to",
                .{},
            );
            return LowerError.InvalidExpression;
        },
    }
}

/// Helper for lowerLValue that takes an ExprIdx directly
pub fn lowerLValueFromExpr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    return lowerLValue(l, expr_idx);
}

/// Lower a member access to get a pointer
pub fn lowerMemberPtr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);
    const loc = l.store.exprLoc(expr_idx);
    const object_idx = data.getObject();
    const field_id = data.getField();

    var object_ptr = try lowerLValue(l, object_idx);
    const field_name = l.strings.get(field_id);
    if (field_name.len == 0) {
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "Undefined field: (empty name)",
            .{},
            loc,
            "lowering member access",
            .{},
        );
        return LowerError.UndefinedVariable;
    }

    // Look up field in struct type
    // Handle both direct struct pointers (*Struct) and pointers to struct pointers (**Struct)
    // The latter occurs with `self` parameter in methods: self is *Struct, so lowerLValue
    // returns **Struct (pointer to the local variable holding the pointer)
    const struct_type = switch (object_ptr.ty) {
        .ptr => |p| switch (p.*) {
            .@"struct" => |s| s,
            .ptr => |inner| blk: {
                // Double pointer case: object is a pointer-to-pointer-to-struct
                // Load the inner pointer first
                switch (inner.*) {
                    .@"struct" => |s| {
                        // Load the struct pointer from the double-pointer
                        const loaded_ptr = func.newValue(.{ .ptr = inner });
                        try l.emit(.{
                            .load = .{
                                .ptr = object_ptr,
                                .result = loaded_ptr,
                            },
                        });
                        object_ptr = loaded_ptr;
                        break :blk s;
                    },
                    else => {
                        l.setErrorContext(
                            LowerError.TypeMismatch,
                            "cannot access field '{s}' on non-struct pointer type",
                            .{field_name},
                            loc,
                            "lowering member access (inner pointer is not a struct)",
                            .{},
                        );
                        return LowerError.TypeMismatch;
                    },
                }
            },
            else => {
                l.setErrorContext(
                    LowerError.TypeMismatch,
                    "cannot access field '{s}' on non-struct type",
                    .{field_name},
                    loc,
                    "lowering member access (pointer target is not a struct)",
                    .{},
                );
                return LowerError.TypeMismatch;
            },
        },
        else => {
            l.setErrorContextWithFileId(
                LowerError.TypeMismatch,
                "cannot access field '{s}' - expected pointer type",
                .{field_name},
                l.store.exprFileId(expr_idx),
                loc,
                "lowering member access (object is not a pointer)",
                .{},
            );
            return LowerError.TypeMismatch;
        },
    };

    var field_idx: ?u32 = null;
    for (struct_type.fields, 0..) |f, i| {
        if (std.mem.eql(u8, f.name, field_name)) {
            field_idx = @intCast(i);
            break;
        }
    }

    if (field_idx) |idx| {
        const field = struct_type.fields[idx];

        const ty_ptr = try l.allocator.create(ir.Type);
        ty_ptr.* = field.ty;
        try l.allocated_types.append(l.allocator, ty_ptr);

        const result = func.newValue(.{ .ptr = ty_ptr });
        try l.emit(.{
            .field_ptr = .{
                .struct_ptr = object_ptr,
                .field_index = idx,
                .result = result,
            },
        });
        return result;
    }

    l.setErrorContext(
        LowerError.UndefinedVariable,
        "Field '{s}' not found in struct '{s}'",
        .{ field_name, struct_type.name },
        loc,
        "lowering member access",
        .{},
    );
    return LowerError.UndefinedVariable;
}

/// Lower an index access to get a pointer
pub fn lowerIndexPtr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);
    const loc = l.store.exprLoc(expr_idx);

    const object_idx = data.getObject();
    const index_idx = data.getIndex();

    const object_ptr = try lowerLValue(l, object_idx);
    const index_val = try lowerExpression(l, index_idx);

    // Get element type from array/slice type
    const elem_type = switch (object_ptr.ty) {
        .ptr => |p| switch (p.*) {
            .array => |a| a.element.*,
            .slice => |s| s.*,
            else => {
                l.setErrorContext(
                    LowerError.TypeMismatch,
                    "cannot index into non-array/slice type",
                    .{},
                    loc,
                    "lowering index expression (pointer target is not array or slice)",
                    .{},
                );
                return LowerError.TypeMismatch;
            },
        },
        else => {
            l.setErrorContext(
                LowerError.TypeMismatch,
                "cannot index into non-pointer type",
                .{},
                loc,
                "lowering index expression (object is not a pointer)",
                .{},
            );
            return LowerError.TypeMismatch;
        },
    };

    const ty_ptr = try l.allocator.create(ir.Type);
    ty_ptr.* = elem_type;
    try l.allocated_types.append(l.allocator, ty_ptr);

    // Use array_load since IR doesn't have get_element_ptr
    // Note: This returns a value, not a pointer - mutable array access needs IR extension
    const result = func.newValue(.{ .ptr = ty_ptr });
    try l.emit(.{
        .array_load = .{
            .array_ptr = object_ptr,
            .index = index_val,
            .result = result,
        },
    });
    return result;
}

// ============================================================================
// Binary and Unary Operations
// ============================================================================

/// Lower a binary expression
pub fn lowerBinary(l: *Lowerer, func: *ir.Function, data: NodeData, expr_idx: ExprIdx) LowerError!ir.Value {
    const loc = l.store.exprLoc(expr_idx);
    const lhs_idx = data.getLhs();
    const rhs_idx = data.getRhs();
    const op = data.getBinaryOp();

    // Check for null comparisons before lowering operands
    // For x == null or x != null, we need to use is_null instead of icmp
    const lhs_is_null = l.store.exprTag(lhs_idx) == .null_literal;
    const rhs_is_null = l.store.exprTag(rhs_idx) == .null_literal;

    if ((op == .eq or op == .ne) and (lhs_is_null or rhs_is_null)) {
        // One operand is null - use is_null instruction
        const non_null_idx = if (lhs_is_null) rhs_idx else lhs_idx;
        const non_null_val = try lowerExpression(l, non_null_idx);
        const result = func.newValue(.bool);

        // is_null returns true if the value is null
        try l.emit(.{ .is_null = .{ .operand = non_null_val, .result = result } });

        if (op == .ne) {
            // For != null, we need to negate the result
            const negated = func.newValue(.bool);
            try l.emit(.{ .log_not = .{ .operand = result, .result = negated } });
            return negated;
        }
        return result;
    }

    const lhs = try lowerExpression(l, lhs_idx);
    const rhs = try lowerExpression(l, rhs_idx);

    // Determine the result type based on the operation
    const result_ty: ir.Type = switch (op) {
        // Comparison operations always produce bool
        .eq, .ne, .lt, .le, .gt, .ge => .bool,
        // Logical operations always produce bool
        .@"and", .@"or" => .bool,
        // String operations: if either operand is string, result is string
        .add, .bit_and => if (lhs.isString() or rhs.isString()) .string else lhs.ty,
        // Other arithmetic and bitwise operations inherit operand type
        .sub, .mul, .div, .mod, .bit_or, .bit_xor, .shl, .shr => lhs.ty,
        // Rounding operations produce decimal (or inherit from lhs)
        .round, .trunc => lhs.ty,
        .range, .range_inclusive => lhs.ty,
        // Null coalescing inherits type from lhs (the non-null value)
        .null_coalesce => lhs.ty,
    };

    const result = func.newValue(result_ty);

    const ir_op: ir.Instruction = switch (op) {
        .add => blk: {
            const lhs_is_string = lhs.isString();
            const rhs_is_string = rhs.isString();

            debug.print(.ir, "binary add: lhs.ty={s} rhs.ty={s} lhs_is_string={} rhs_is_string={}", .{
                @tagName(lhs.ty),
                @tagName(rhs.ty),
                lhs_is_string,
                rhs_is_string,
            });

            // Auto-coercion: if either operand is a string, use string concatenation
            // The VM will auto-convert integers/decimals to their string representation
            const is_concat = lhs_is_string or rhs_is_string;
            debug.print(.ir, "binary add decision: {s}", .{if (is_concat) "str_concat (auto-coerce)" else "iadd"});

            break :blk if (is_concat)
                .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } }
            else
                .{ .iadd = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } };
        },
        .sub => .{ .isub = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .mul => .{ .imul = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .div => .{ .sdiv = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .mod => .{ .srem = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .eq => .{ .icmp = .{ .cond = .eq, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .ne => .{ .icmp = .{ .cond = .ne, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .lt => .{ .icmp = .{ .cond = .slt, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .le => .{ .icmp = .{ .cond = .sle, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .gt => .{ .icmp = .{ .cond = .sgt, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .ge => .{ .icmp = .{ .cond = .sge, .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .@"and" => .{ .log_and = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .@"or" => .{ .log_or = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .bit_and => blk: {
            // In DBL, & is overloaded: bitwise AND for integers, concatenation for strings
            const lhs_is_string = lhs.isString();
            const rhs_is_string = rhs.isString();
            const is_concat = lhs_is_string or rhs_is_string;
            break :blk if (is_concat)
                .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } }
            else
                .{ .band = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } };
        },
        .bit_or => .{ .bor = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .bit_xor => .{ .bxor = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .shl => .{ .ishl = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        .shr => .{ .sshr = .{ .lhs = lhs, .rhs = rhs, .result = result, .loc = toIrLoc(loc) } },
        // Rounding operations: value # places (trunc) or value ## places (round)
        .round => .{ .round = .{ .value = lhs, .places = rhs, .result = result, .loc = toIrLoc(loc) } },
        .trunc => .{ .trunc = .{ .value = lhs, .places = rhs, .result = result, .loc = toIrLoc(loc) } },
        .range, .range_inclusive => {
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Range operator '{s}' cannot be used in binary expression context",
                .{if (op == .range) ".." else "..="},
                loc,
                "range expressions are only valid in for loops (e.g., 'for i in 0..10')",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
        .null_coalesce => blk: {
            // null_coalesce: lhs ?? rhs = if (lhs is null) rhs else lhs
            // Lower to: cond = is_null(lhs); result = select(cond, rhs, lhs)
            const cond = func.newValue(.bool);
            try l.emit(.{ .is_null = .{ .operand = lhs, .result = cond } });
            break :blk .{ .select = .{ .condition = cond, .true_val = rhs, .false_val = lhs, .result = result } };
        },
    };

    try l.emit(ir_op);
    return result;
}

/// Lower a unary expression
pub fn lowerUnary(l: *Lowerer, func: *ir.Function, data: NodeData, expr_idx: ExprIdx) LowerError!ir.Value {
    _ = expr_idx;
    const operand_idx = data.getOperand();
    const op = data.getUnaryOp();

    const operand = try lowerExpression(l, operand_idx);

    switch (op) {
        .neg => {
            const result = func.newValue(operand.ty);
            try l.emit(.{ .ineg = .{ .operand = operand, .result = result } });
            return result;
        },
        .not => {
            // Logical not always produces bool
            const result = func.newValue(.bool);
            try l.emit(.{ .log_not = .{ .operand = operand, .result = result } });
            return result;
        },
        .bit_not => {
            const result = func.newValue(operand.ty);
            try l.emit(.{ .bnot = .{ .operand = operand, .result = result } });
            return result;
        },
        .addr_of => {
            // Get address of operand - use lowerLValue to get pointer
            const ptr = try lowerLValue(l, operand_idx);
            return ptr;
        },
        .deref => {
            // Dereference pointer - operand should be a pointer, result is pointee type
            // Handle both *T and ?*T (optional pointer) cases
            const pointee_ty: ir.Type = blk: {
                if (operand.ty == .ptr) {
                    break :blk operand.ty.ptr.*;
                } else if (operand.ty == .optional) {
                    // For ?*T, unwrap the optional to get *T, then get T
                    const inner = operand.ty.optional.*;
                    if (inner == .ptr) {
                        break :blk inner.ptr.*;
                    }
                    break :blk inner;
                } else {
                    break :blk operand.ty;
                }
            };
            const deref_result = func.newValue(pointee_ty);
            try l.emit(.{
                .load = .{
                    .ptr = operand,
                    .result = deref_result,
                },
            });
            return deref_result;
        },
    }
}

// ============================================================================
// Member and Index Access
// ============================================================================

/// Lower a member access expression
pub fn lowerMember(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const object_idx = data.getObject();
    const field_id = data.getField();
    const field_name = l.strings.get(field_id);

    // Check if this is an enum member access (EnumName.Variant)
    const object_tag = l.store.exprTag(object_idx);
    if (object_tag == .identifier) {
        const object_data = l.store.exprData(object_idx);
        const object_name_id = object_data.getName();
        const object_name = l.strings.get(object_name_id);

        // Check if object name is an enum
        if (l.enum_types.get(object_name)) |variants| {
            // Look up the variant value
            if (variants.get(field_name)) |variant_value| {
                // Return constant integer value
                const result = func.newValue(.i64);
                try l.emit(.{
                    .iconst = .{
                        .ty = .i64,
                        .value = variant_value,
                        .result = result,
                    },
                });
                return result;
            } else {
                // Variant not found in this enum
                const loc = l.store.exprLoc(expr_idx);
                l.setErrorContext(
                    LowerError.UndefinedVariable,
                    "Unknown enum variant '{s}.{s}'",
                    .{ object_name, field_name },
                    loc,
                    "variant '{s}' does not exist in enum '{s}'",
                    .{ field_name, object_name },
                );
                return LowerError.UndefinedVariable;
            }
        }
    }

    // Check if object is a call expression - can't get lvalue from call result
    // Need to evaluate call first, store to temp, then access field
    if (object_tag == .call or object_tag == .method_call) {
        const object_val = try lowerExpression(l, object_idx);

        // Check if call returns a heap record - use load_field_heap directly
        if (object_val.ty == .heap_record) {
            const struct_type = object_val.ty.heap_record;
            for (struct_type.fields, 0..) |field, i| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    const result = func.newValue(field.ty);
                    try l.emit(.{
                        .load_field_heap = .{
                            .record = object_val,
                            .field_index = @intCast(i),
                            .result = result,
                        },
                    });
                    return result;
                }
            }
            const loc = l.store.exprLoc(expr_idx);
            l.setErrorContext(
                LowerError.UndefinedVariable,
                "struct '{s}' has no field '{s}'",
                .{ struct_type.name, field_name },
                .{ .line = loc.line, .column = loc.column },
                "field not found in heap record returned by call",
                .{},
            );
            return LowerError.UndefinedVariable;
        }

        // Object should be a struct - get its type
        const struct_type = switch (object_val.ty) {
            .@"struct" => |s| s,
            .ptr => |p| switch (p.*) {
                .@"struct" => |s| s,
                else => {
                    const loc = l.store.exprLoc(expr_idx);
                    l.setErrorContext(
                        LowerError.TypeMismatch,
                        "cannot access field '{s}' on non-struct call result",
                        .{field_name},
                        .{ .line = loc.line, .column = loc.column },
                        "call result is not a struct type",
                        .{},
                    );
                    return LowerError.TypeMismatch;
                },
            },
            else => {
                const loc = l.store.exprLoc(expr_idx);
                l.setErrorContext(
                    LowerError.TypeMismatch,
                    "cannot access field '{s}' on non-struct call result",
                    .{field_name},
                    .{ .line = loc.line, .column = loc.column },
                    "call result is not a struct type",
                    .{},
                );
                return LowerError.TypeMismatch;
            },
        };

        // Find field in struct type
        for (struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                // Create temp storage for the struct value
                const struct_type_ptr = try l.allocator.create(ir.Type);
                struct_type_ptr.* = .{ .@"struct" = struct_type };
                try l.allocated_types.append(l.allocator, struct_type_ptr);

                const temp_ptr = func.newValue(.{ .ptr = struct_type_ptr });
                const temp_name = std.fmt.allocPrint(
                    l.allocator,
                    "__call_result${d}",
                    .{temp_ptr.id},
                ) catch return LowerError.OutOfMemory;

                // Allocate temp storage
                try l.emit(.{
                    .alloca = .{
                        .ty = .{ .@"struct" = struct_type },
                        .name = temp_name,
                        .result = temp_ptr,
                    },
                });

                // Store call result to temp
                try l.emit(.{
                    .store = .{
                        .ptr = temp_ptr,
                        .value = object_val,
                    },
                });

                // Get field pointer from temp
                const field_type_ptr = try l.allocator.create(ir.Type);
                field_type_ptr.* = field.ty;
                try l.allocated_types.append(l.allocator, field_type_ptr);

                const field_ptr = func.newValue(.{ .ptr = field_type_ptr });
                try l.emit(.{
                    .field_ptr = .{
                        .struct_ptr = temp_ptr,
                        .field_index = @intCast(i),
                        .result = field_ptr,
                    },
                });

                // Load field value
                const result = func.newValue(field.ty);
                try l.emit(.{
                    .load = .{
                        .ptr = field_ptr,
                        .result = result,
                    },
                });
                return result;
            }
        }

        // Field not found
        const loc = l.store.exprLoc(expr_idx);
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "struct '{s}' has no field '{s}'",
            .{ struct_type.name, field_name },
            .{ .line = loc.line, .column = loc.column },
            "field not found in struct type",
            .{},
        );
        return LowerError.UndefinedVariable;
    }

    // Check if object is a heap record (created with `new` keyword)
    // In this case, we need to use load_field_heap instead of field_ptr + load
    const object_val = try lowerExpression(l, object_idx);
    if (object_val.ty == .heap_record) {
        const struct_type = object_val.ty.heap_record;

        // Find field in struct type
        for (struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                const result = func.newValue(field.ty);
                try l.emit(.{
                    .load_field_heap = .{
                        .record = object_val,
                        .field_index = @intCast(i),
                        .result = result,
                    },
                });
                return result;
            }
        }

        // Field not found
        const loc = l.store.exprLoc(expr_idx);
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "struct '{s}' has no field '{s}'",
            .{ struct_type.name, field_name },
            .{ .line = loc.line, .column = loc.column },
            "field not found in heap record type",
            .{},
        );
        return LowerError.UndefinedVariable;
    }

    // Not an enum, call, or heap record access - proceed with regular member access via lvalue
    const ptr = try lowerMemberPtr(l, expr_idx);
    // ptr is a pointer to the field, so the loaded value type is the pointee type
    const pointee_ty: ir.Type = switch (ptr.ty) {
        .ptr => |p| p.*,
        else => ptr.ty, // Shouldn't happen, but be safe
    };
    const result = func.newValue(pointee_ty);
    try l.emit(.{
        .load = .{
            .ptr = ptr,
            .result = result,
        },
    });
    return result;
}

/// Lower an index expression
pub fn lowerIndex(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const object_idx = data.getObject();
    const index_idx = data.getIndex();

    // First, try to determine if the object is a map
    const object_val = try lowerExpression(l, object_idx);

    // Check if object is a map type - emit map_get instruction
    if (isMapType(object_val.ty)) {
        const key_val = try lowerExpression(l, index_idx);
        // Get the value type from the map's type information
        const value_type = getMapValueType(object_val.ty);
        const result = func.newValue(value_type);
        try l.emit(.{
            .map_get = .{
                .map = object_val,
                .key = key_val,
                .result = result,
                .loc = null,
            },
        });
        return result;
    }

    // Check if object is a list type - emit list_get instruction
    if (object_val.ty == .list) {
        const index_val = try lowerExpression(l, index_idx);
        const elem_type = object_val.ty.list.element_type.*;
        const result = func.newValue(elem_type);
        try l.emit(.{
            .list_get = .{
                .list = object_val,
                .index = index_val,
                .result = result,
                .loc = null,
            },
        });
        return result;
    }

    // Fallback to array/slice indexing via pointer
    const ptr = try lowerIndexPtr(l, expr_idx);
    // ptr is a pointer to the element, so the loaded value type is the pointee type
    const pointee_ty: ir.Type = switch (ptr.ty) {
        .ptr => |p| p.*,
        else => ptr.ty, // Shouldn't happen, but be safe
    };
    const result = func.newValue(pointee_ty);
    try l.emit(.{
        .load = .{
            .ptr = ptr,
            .result = result,
        },
    });
    return result;
}

/// Lower a slice expression: expr[start..end] or expr[start..=end]
pub fn lowerSliceExpr(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const parts = l.store.getSliceExprParts(expr_idx);
    const slice_loc = l.store.exprLoc(expr_idx);

    // Lower the source expression
    const source_val = try lowerExpression(l, parts.object);

    // Lower the start and end expressions
    const start_val = try lowerExpression(l, parts.start);
    const end_val = try lowerExpression(l, parts.end);

    log.debug("lowerSliceExpr: source_val.ty = {}", .{source_val.ty});

    // Check if source is a string type (including [N]u8 byte arrays which represent string literals)
    const is_string = switch (source_val.ty) {
        .string => true,
        // [N]u8 byte arrays are string literals
        .array => |a| a.element.* == .u8,
        else => false,
    };

    // Check if source is a non-string array type
    const is_array = if (is_string) false else switch (source_val.ty) {
        .array => true,
        .slice => true,
        .list => true,
        .ptr => |p| switch (p.*) {
            .array => true,
            .slice => true,
            else => false,
        },
        else => false,
    };

    if (is_array) {
        // Array slicing - emit array_slice instruction
        // Result is a list type since the VM creates a List
        const elem_type_ptr = try l.allocator.create(ir.Type);
        elem_type_ptr.* = switch (source_val.ty) {
            .array => |a| a.element.*,
            .slice => |s| s.*,
            .list => |lt| lt.element_type.*,
            .ptr => |p| switch (p.*) {
                .array => |a| a.element.*,
                .slice => |s| s.*,
                else => .i64,
            },
            else => .i64,
        };
        try l.allocated_types.append(l.allocator, elem_type_ptr);

        // Create a ListType for the result
        const list_type_ptr = try l.allocator.create(ir.ListType);
        list_type_ptr.* = .{ .element_type = elem_type_ptr };
        try l.allocated_list_types.append(l.allocator, list_type_ptr);

        const result = func.newValue(.{ .list = list_type_ptr });
        try l.emit(.{
            .array_slice = .{
                .source = source_val,
                .start = start_val,
                .end = end_val,
                .inclusive = parts.inclusive,
                .result = result,
                .loc = toIrLoc(slice_loc),
            },
        });
        return result;
    }

    // String slicing - emit str_slice instruction
    const result = func.newValue(.string);
    try l.emit(.{
        .str_slice = .{
            .source = source_val,
            .start = start_val,
            .length_or_end = end_val,
            .is_length = false, // Using end index, not length
            .result = result,
            .loc = toIrLoc(slice_loc),
        },
    });

    return result;
}

/// Lower an optional member access expression: expr?.field
/// Evaluates to null if expr is null, otherwise evaluates to expr.field
pub fn lowerOptionalMember(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const object_idx = data.getObject();
    const field_id = data.getField();
    const field_name = l.strings.get(field_id);

    // Lower the object expression
    const object_val = try lowerExpression(l, object_idx);

    // Check if object is null
    const is_null_cond = func.newValue(.bool);
    try l.emit(.{ .is_null = .{ .operand = object_val, .result = is_null_cond } });

    // Get the struct type from the object value's type
    // Handle optional types by unwrapping to get the inner type
    const inner_type: ir.Type = switch (object_val.ty) {
        .optional => |opt| opt.*,
        .ptr => |p| p.*,
        else => object_val.ty,
    };

    const struct_type: *const ir.StructType = switch (inner_type) {
        .@"struct" => |s| s,
        .ptr => |p| switch (p.*) {
            .@"struct" => |s| s,
            else => {
                const loc = l.store.exprLoc(expr_idx);
                l.setErrorContext(
                    LowerError.UnsupportedFeature,
                    "Optional member access requires a struct type, got pointer to non-struct",
                    .{},
                    loc,
                    "the object type is not a struct",
                    .{},
                );
                return LowerError.UnsupportedFeature;
            },
        },
        else => {
            const loc = l.store.exprLoc(expr_idx);
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Optional member access requires a struct type",
                .{},
                loc,
                "the object type is not a struct",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
    };

    // Find the field in the struct
    for (struct_type.fields, 0..) |fld, i| {
        if (std.mem.eql(u8, fld.name, field_name)) {
            // Found the field - create a pointer type for it
            const ty_ptr = l.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
            ty_ptr.* = fld.ty;
            try l.allocated_types.append(l.allocator, ty_ptr);

            const field_ptr = func.newValue(.{ .ptr = ty_ptr });
            try l.emit(.{
                .field_ptr = .{
                    .struct_ptr = object_val,
                    .field_index = @intCast(i),
                    .result = field_ptr,
                },
            });

            // Load the field value
            const field_val = func.newValue(fld.ty);
            try l.emit(.{ .load = .{ .ptr = field_ptr, .result = field_val } });

            // Create a null constant for the result type
            const null_val = func.newValue(fld.ty);
            try l.emit(.{ .const_null = .{ .ty = fld.ty, .result = null_val } });

            // Select between null and field value based on is_null condition
            const result = func.newValue(fld.ty);
            try l.emit(.{ .select = .{
                .condition = is_null_cond,
                .true_val = null_val,
                .false_val = field_val,
                .result = result,
            } });

            return result;
        }
    }

    // Field not found in struct
    const loc = l.store.exprLoc(expr_idx);
    l.setErrorContext(
        LowerError.UndefinedVariable,
        "struct '{s}' has no field '{s}'",
        .{ struct_type.name, field_name },
        .{ .line = loc.line, .column = loc.column },
        "field not found in struct type for optional access",
        .{},
    );
    return LowerError.UndefinedVariable;
}

/// Lower an optional index expression: expr?[index]
/// Returns null if index is out of bounds, otherwise returns the element
pub fn lowerOptionalIndex(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const object_idx = data.getObject();
    const index_idx = data.getIndex();

    // Lower the object expression
    const object_val = try lowerExpression(l, object_idx);

    // Lower the index expression
    const index_val = try lowerExpression(l, index_idx);

    // Handle list types - list_get already returns null on out-of-bounds
    if (object_val.ty == .list) {
        const elem_type = object_val.ty.list.element_type.*;
        const result = func.newValue(elem_type);
        try l.emit(.{
            .list_get = .{
                .list = object_val,
                .index = index_val,
                .result = result,
                .loc = null,
            },
        });
        return result;
    }

    // Handle array types - use array_load_opt which returns null on out-of-bounds
    const is_array = switch (object_val.ty) {
        .array => true,
        .ptr => |p| switch (p.*) {
            .array => true,
            else => false,
        },
        else => false,
    };

    if (is_array) {
        // Get element type
        const elem_type: ir.Type = switch (object_val.ty) {
            .array => |a| a.element.*,
            .ptr => |p| switch (p.*) {
                .array => |a| a.element.*,
                else => .i64,
            },
            else => .i64,
        };

        // Use array_load_opt which handles bounds checking in the VM
        // and returns null for out-of-bounds indices
        const result = func.newValue(elem_type);
        try l.emit(.{
            .array_load_opt = .{
                .array_ptr = object_val,
                .index = index_val,
                .result = result,
            },
        });

        return result;
    }

    // Unsupported type for optional indexing
    const loc = l.store.exprLoc(expr_idx);
    l.setErrorContext(
        LowerError.UnsupportedFeature,
        "Optional indexing operator '?[' requires array or list type",
        .{},
        loc,
        "the object type does not support indexing",
        .{},
    );
    return LowerError.UnsupportedFeature;
}

/// Lower an if expression using the select instruction
/// if (cond) then_expr else else_expr -> select(cond, then_val, else_val)
pub fn lowerIfExpr(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const parts = l.store.getIfExprParts(expr_idx);
    const loc = l.store.exprLoc(expr_idx);

    // Evaluate condition
    const cond_val = try lowerExpression(l, parts.cond);

    // Lower both branches
    const then_val = try lowerExpression(l, parts.then_expr);
    const else_val = try lowerExpression(l, parts.else_expr);

    // Type check: branches must have compatible types
    if (!typesCompatible(then_val.ty, else_val.ty)) {
        l.setErrorContext(
            LowerError.TypeMismatch,
            "If expression branches have incompatible types",
            .{},
            loc,
            "lowering if expression",
            .{},
        );
        return LowerError.TypeMismatch;
    }

    // Emit select instruction
    const result = func.newValue(then_val.ty);
    try l.emit(.{
        .select = .{
            .condition = cond_val,
            .true_val = then_val,
            .false_val = else_val,
            .result = result,
        },
    });
    return result;
}

/// Check if two types are compatible for if-expression branches
fn typesCompatible(t1: ir.Type, t2: ir.Type) bool {
    // Exact match
    if (std.meta.eql(t1, t2)) return true;

    // String literals are [N]u8 arrays - treat all u8 arrays as compatible strings
    if (isU8Array(t1) and isU8Array(t2)) return true;

    return false;
}

/// Check if a type is a u8 array (string literal type)
fn isU8Array(ty: ir.Type) bool {
    if (ty != .array) return false;
    const elem = ty.array.element.*;
    return elem == .u8;
}

/// Lower a type check expression: expr is Type
/// Returns true if expr's runtime type matches the specified type
pub fn lowerIsExpr(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const value_idx: ExprIdx = @enumFromInt(data.a);
    const type_idx: TypeIdx = @enumFromInt(data.b);

    // Lower the expression to check
    const value = try lowerExpression(l, value_idx);

    // Get the AST type tag
    const type_tag = l.store.typeTag(type_idx);

    // Map AST TypeTag to IR RuntimeTypeTag
    const result = func.newValue(.bool);

    // Map AST type to runtime type tag
    const runtime_tag: ir.Instruction.RuntimeTypeTag = switch (type_tag) {
        // Integer types
        .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .isize, .usize => .integer,
        // Float types
        .f32, .f64, .decimal => .float,
        // Boolean
        .bool => .boolean,
        // String
        .string => .string,
        // For other types, return false (unsupported runtime check)
        else => {
            const loc = l.store.exprLoc(expr_idx);
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Type check 'is' not supported for this type",
                .{},
                loc,
                "runtime type checking is only supported for primitive types",
                .{},
            );
            return LowerError.UnsupportedFeature;
        },
    };

    // Emit is_type instruction
    try l.emit(.{ .is_type = .{
        .operand = value,
        .type_tag = runtime_tag,
        .result = result,
    } });

    return result;
}

// ============================================================================
// Compile-Time Builtins
// ============================================================================

/// Get IR type from an expression that represents a type (e.g., identifier "i64")
fn getTypeFromExpr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Type {
    const expr_tag = l.store.exprTag(expr_idx);

    if (expr_tag == .identifier) {
        // Get the identifier name
        const ident_data = l.store.exprData(expr_idx);
        const name_id: StringId = @enumFromInt(ident_data.a);
        const type_name = l.strings.get(name_id);

        // Map common type names to IR types
        if (std.mem.eql(u8, type_name, "i8")) return .i8;
        if (std.mem.eql(u8, type_name, "i16")) return .i16;
        if (std.mem.eql(u8, type_name, "i32")) return .i32;
        if (std.mem.eql(u8, type_name, "i64")) return .i64;
        if (std.mem.eql(u8, type_name, "u8")) return .u8;
        if (std.mem.eql(u8, type_name, "u16")) return .u16;
        if (std.mem.eql(u8, type_name, "u32")) return .u32;
        if (std.mem.eql(u8, type_name, "u64")) return .u64;
        if (std.mem.eql(u8, type_name, "isize")) return .isize;
        if (std.mem.eql(u8, type_name, "usize")) return .usize;
        if (std.mem.eql(u8, type_name, "f32")) return .f32;
        if (std.mem.eql(u8, type_name, "f64")) return .f64;
        if (std.mem.eql(u8, type_name, "bool")) return .bool;
        if (std.mem.eql(u8, type_name, "void")) return .void;
        if (std.mem.eql(u8, type_name, "string")) return .string;

        // Check for struct types
        if (l.struct_types.get(type_name)) |struct_type| {
            return .{ .@"struct" = struct_type };
        }

        // Unknown type
        return LowerError.UndefinedType;
    }

    // Not an identifier - unsupported
    return LowerError.InvalidExpression;
}

/// Get the name of an IR type as a string
fn getTypeName(ty: ir.Type) []const u8 {
    return switch (ty) {
        .void => "void",
        .bool => "bool",
        .i8 => "i8",
        .i16 => "i16",
        .i32 => "i32",
        .i64 => "i64",
        .u8 => "u8",
        .u16 => "u16",
        .u32 => "u32",
        .u64 => "u64",
        .isize => "isize",
        .usize => "usize",
        .f32 => "f32",
        .f64 => "f64",
        .string => "string",
        .@"struct" => |st| st.name,
        .@"union" => |un| un.name,
        .ptr => "*",
        .optional => "?",
        .weak => "weak",
        .array => "array",
        .slice => "slice",
        .implied_decimal => "decimal",
        .fixed_decimal => "fixed_decimal",
        .function => "fn",
        .trait_object => "trait_object",
        .map => "map",
        .list => "list",
        .heap_record => |hr| hr.name,
        .variant => "variant",
    };
}

/// Lower a compile-time builtin expression (@os, @arch, @file, @line, @sizeof, etc.)
pub fn lowerComptimeBuiltin(l: *Lowerer, func: *ir.Function, expr_idx: ExprIdx, loc: SourceLoc) LowerError!ir.Value {
    const data = l.store.exprData(expr_idx);
    const name_id: StringId = @enumFromInt(data.a);
    const name = l.strings.get(name_id);

    // Extract arguments
    const args_len = data.b >> 16;
    const args_start = data.b & 0xFFFF;

    // @os() - current operating system
    if (std.mem.eql(u8, name, "os")) {
        const os_name = switch (builtin.os.tag) {
            .macos => "macos",
            .linux => "linux",
            .windows => "windows",
            .freebsd => "freebsd",
            .openbsd => "openbsd",
            .netbsd => "netbsd",
            .freestanding => "freestanding",
            else => "unknown",
        };
        const result = func.newValue(.string);
        const name_str = l.allocator.dupe(u8, os_name) catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = name_str,
                .result = result,
            },
        });
        return result;
    }

    // @arch() - current CPU architecture
    if (std.mem.eql(u8, name, "arch")) {
        const arch_name = switch (builtin.cpu.arch) {
            .x86_64 => "x86_64",
            .aarch64 => "arm64",
            .x86 => "x86",
            .arm => "arm",
            .wasm32 => "wasm32",
            .wasm64 => "wasm64",
            .riscv64 => "riscv64",
            else => "unknown",
        };
        const result = func.newValue(.string);
        const name_str = l.allocator.dupe(u8, arch_name) catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = name_str,
                .result = result,
            },
        });
        return result;
    }

    // @file() - current source file path
    if (std.mem.eql(u8, name, "file")) {
        const file_path = l.source_file orelse "unknown";
        const result = func.newValue(.string);
        const path_str = l.allocator.dupe(u8, file_path) catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = path_str,
                .result = result,
            },
        });
        return result;
    }

    // @line() - current line number
    if (std.mem.eql(u8, name, "line")) {
        const result = func.newValue(.i64);
        try l.emit(.{
            .iconst = .{
                .ty = .i64,
                .value = @intCast(loc.line),
                .result = result,
            },
        });
        return result;
    }

    // @sizeof(T) - size of type in bytes
    if (std.mem.eql(u8, name, "sizeof") or std.mem.eql(u8, name, "sizeOf")) {
        if (args_len < 1) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@sizeof requires a type argument",
                .{},
                loc,
                "usage: @sizeof(Type)",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        // Get the expression argument - it should be an identifier representing a type
        const expr_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start]);
        const ir_type = try getTypeFromExpr(l, expr_arg_idx);
        const size = ir_type.sizeInBytes();

        const result = func.newValue(.i64);
        try l.emit(.{
            .iconst = .{
                .ty = .i64,
                .value = @intCast(size),
                .result = result,
            },
        });
        return result;
    }

    // @alignof(T) - alignment of type
    if (std.mem.eql(u8, name, "alignof") or std.mem.eql(u8, name, "alignOf")) {
        if (args_len < 1) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@alignof requires a type argument",
                .{},
                loc,
                "usage: @alignof(Type)",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        // Get the expression argument - it should be an identifier representing a type
        const expr_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start]);
        const ir_type = try getTypeFromExpr(l, expr_arg_idx);
        // For now, use size as alignment (simple alignment)
        const align_val = ir_type.sizeInBytes();

        const result = func.newValue(.i64);
        try l.emit(.{
            .iconst = .{
                .ty = .i64,
                .value = @intCast(align_val),
                .result = result,
            },
        });
        return result;
    }

    // @version() - compiler version
    if (std.mem.eql(u8, name, "version")) {
        const result = func.newValue(.string);
        const version_str = l.allocator.dupe(u8, "0.1.0") catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = version_str,
                .result = result,
            },
        });
        return result;
    }

    // @defined(NAME) - check if constant is defined
    if (std.mem.eql(u8, name, "defined")) {
        // For now, always return false unless we integrate with comptime evaluator
        const result = func.newValue(.bool);
        try l.emit(.{
            .iconst = .{
                .ty = .bool,
                .value = 0, // false
                .result = result,
            },
        });
        return result;
    }

    // @typeName(T) - get the name of a type as a string
    if (std.mem.eql(u8, name, "typeName")) {
        if (args_len < 1) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@typeName requires a type argument",
                .{},
                loc,
                "usage: @typeName(Type)",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        const expr_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start]);
        const ir_type = try getTypeFromExpr(l, expr_arg_idx);
        const type_name_str = getTypeName(ir_type);

        const result = func.newValue(.string);
        const name_str = l.allocator.dupe(u8, type_name_str) catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = name_str,
                .result = result,
            },
        });
        return result;
    }

    // @hasField(T, "field") - check if a struct type has a field
    if (std.mem.eql(u8, name, "hasField")) {
        if (args_len < 2) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@hasField requires a type and field name arguments",
                .{},
                loc,
                "usage: @hasField(Type, \"field_name\")",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        const type_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start]);
        const field_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + 1]);

        const ir_type = try getTypeFromExpr(l, type_arg_idx);

        // Get the field name from the string literal
        const field_expr_tag = l.store.exprTag(field_arg_idx);
        if (field_expr_tag != .string_literal) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@hasField second argument must be a string literal",
                .{},
                loc,
                "usage: @hasField(Type, \"field_name\")",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        const field_data = l.store.exprData(field_arg_idx);
        const field_name_id: StringId = @enumFromInt(field_data.a);
        const field_name = l.strings.get(field_name_id);

        // Check if the type is a struct and has the field
        const has_field = switch (ir_type) {
            .@"struct" => |st| blk: {
                for (st.fields) |f| {
                    if (std.mem.eql(u8, f.name, field_name)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            else => false,
        };

        const result = func.newValue(.bool);
        try l.emit(.{
            .iconst = .{
                .ty = .bool,
                .value = if (has_field) 1 else 0,
                .result = result,
            },
        });
        return result;
    }

    // @fieldNames(T) - get field names of a struct as a comma-separated string
    if (std.mem.eql(u8, name, "fieldNames")) {
        if (args_len < 1) {
            l.setErrorContext(
                LowerError.InvalidExpression,
                "@fieldNames requires a type argument",
                .{},
                loc,
                "usage: @fieldNames(StructType)",
                .{},
            );
            return LowerError.InvalidExpression;
        }
        const expr_arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start]);
        const ir_type = try getTypeFromExpr(l, expr_arg_idx);

        // Check that we have a struct type
        const st = switch (ir_type) {
            .@"struct" => |s| s,
            else => {
                l.setErrorContext(
                    LowerError.InvalidExpression,
                    "@fieldNames requires a struct type",
                    .{},
                    loc,
                    "provided type is not a struct",
                    .{},
                );
                return LowerError.InvalidExpression;
            },
        };

        // Build a comma-separated string of field names
        var names_list: std.ArrayListUnmanaged(u8) = .{};
        defer names_list.deinit(l.allocator);

        for (st.fields, 0..) |f, i| {
            if (i > 0) {
                try names_list.append(l.allocator, ',');
            }
            try names_list.appendSlice(l.allocator, f.name);
        }

        const result = func.newValue(.string);
        const names_str = l.allocator.dupe(u8, names_list.items) catch return LowerError.OutOfMemory;
        try l.emit(.{
            .const_string = .{
                .value = names_str,
                .result = result,
            },
        });
        return result;
    }

    // Unknown builtin
    l.setErrorContext(
        LowerError.UnsupportedFeature,
        "Unknown comptime builtin '@{s}'",
        .{name},
        loc,
        "supported builtins: @os, @arch, @file, @line, @sizeof, @alignof, @version, @typeName, @hasField, @fieldNames",
        .{},
    );
    return LowerError.UnsupportedFeature;
}

// ============================================================================
// Function and Method Calls
// ============================================================================

/// Lower a function call expression
pub fn lowerCall(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);
    const call_loc = l.store.exprLoc(expr_idx);

    const callee_idx = data.getCallee();
    const args_count = data.b >> 16;
    const args_start = data.b & 0xFFFF;

    // Get callee name
    const callee_tag = l.store.exprTag(callee_idx);

    // Check for namespace-qualified function calls (e.g., std.math.abs, std.io.println, utils.add)
    if (callee_tag == .member) {
        var qualified_buf: [256]u8 = undefined;
        if (extractQualifiedName(l.store, l.strings, callee_idx, &qualified_buf)) |qualified_name| {
            // Check if this is a std.* namespace function
            if (isStdNamespaceFunction(qualified_name)) {
                // Lower arguments
                var ns_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer ns_args.deinit(l.allocator);
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try ns_args.append(l.allocator, arg_val);
                }

                // Allocate the function name so it survives beyond this scope
                const native_name = l.allocator.dupe(u8, qualified_name) catch {
                    return LowerError.OutOfMemory;
                };

                const args_slice = try ns_args.toOwnedSlice(l.allocator);
                const result_type = getBuiltinReturnType(native_name, args_slice);
                const result = func.newValue(result_type);

                try l.emit(.{
                    .call = .{
                        .callee = native_name,
                        .args = args_slice,
                        .result = result,
                        .loc = toIrLoc(call_loc),
                    },
                });
                return result;
            }

            // Check if this is an imported package function (e.g., utils.add)
            // BUT NOT if the first component is a local variable (method call on instance)
            // e.g., if we have: var parser = Parser.create(); parser.parseModule()
            // The variable `parser` might shadow the import `parser`, but it's a method call
            const is_local_var = blk: {
                const dot_pos = std.mem.indexOf(u8, qualified_name, ".") orelse break :blk false;
                const first_component = qualified_name[0..dot_pos];
                // Check if this is a local variable in any scope
                if (l.scopes.get(first_component)) |_| {
                    break :blk true;
                }
                // Also check global variables
                if (l.global_variables.contains(first_component)) {
                    break :blk true;
                }
                break :blk false;
            };
            if (!is_local_var and isImportedPackageFunction(l, qualified_name)) {
                // Lower arguments
                var pkg_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer pkg_args.deinit(l.allocator);
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try pkg_args.append(l.allocator, arg_val);
                }

                // Allocate the function name
                const fn_name = l.allocator.dupe(u8, qualified_name) catch {
                    return LowerError.OutOfMemory;
                };

                const args_slice = try pkg_args.toOwnedSlice(l.allocator);

                // Get return type from dependency types if available
                const result_type = getPackageFunctionReturnType(l, qualified_name);
                const result = func.newValue(result_type);

                debug.print(.ir, "Emitting cross-package call to {s}", .{qualified_name});

                try l.emit(.{
                    .call = .{
                        .callee = fn_name,
                        .args = args_slice,
                        .result = result,
                        .loc = toIrLoc(call_loc),
                    },
                });
                return result;
            }
        }
    }

    // Check for Map.new() static method call OR map instance method calls
    if (callee_tag == .member) {
        const callee_data = l.store.exprData(callee_idx);
        const object_idx = callee_data.getObject();
        const field_id = callee_data.getField();
        const field_name = l.strings.get(field_id);
        debug.print(.ir, "lowerCall: member access detected, field={s}", .{field_name});

        // Check if object is "Map" identifier (static method)
        const object_tag = l.store.exprTag(object_idx);
        if (object_tag == .identifier) {
            const object_data = l.store.exprData(object_idx);
            const object_name_id = object_data.getName();
            const object_name = l.strings.get(object_name_id);

            if (std.mem.eql(u8, object_name, "Map") and std.mem.eql(u8, field_name, "new")) {
                // Map.new() - emit map_new instruction
                // Default flags: case_sensitive=true, preserve_spaces=true
                const flags: u8 = 0x03; // bit 0 = case_sensitive, bit 1 = preserve_spaces

                // Create key/value types for the map (default to void, will be inferred at runtime)
                const key_type_ptr = try l.allocator.create(ir.Type);
                key_type_ptr.* = .void;
                try l.allocated_types.append(l.allocator, key_type_ptr);

                const value_type_ptr = try l.allocator.create(ir.Type);
                value_type_ptr.* = .void;
                try l.allocated_types.append(l.allocator, value_type_ptr);

                const map_type = try l.allocator.create(ir.MapType);
                map_type.* = .{ .key_type = key_type_ptr, .value_type = value_type_ptr };

                const result = func.newValue(.{ .map = map_type });
                try l.emit(.{
                    .map_new = .{
                        .flags = flags,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }

            if (std.mem.eql(u8, object_name, "List") and std.mem.eql(u8, field_name, "new")) {
                // List.new() - emit list_new instruction
                // Need to get element type from context (type annotation) or default to void
                // For now, we'll use a runtime-determined type (dynamic list)
                const elem_type_ptr = try l.allocator.create(ir.Type);
                elem_type_ptr.* = .void; // Will be determined at runtime or from annotation
                try l.allocated_types.append(l.allocator, elem_type_ptr);

                const list_type = try l.allocator.create(ir.ListType);
                list_type.* = .{ .element_type = elem_type_ptr };

                const result = func.newValue(.{ .list = list_type });
                try l.emit(.{
                    .list_new = .{
                        .element_type = elem_type_ptr,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }

            // Check for enum variant constructor: EnumName.Variant(payload...)
            if (l.enum_types.get(object_name)) |variants| {
                // This is an enum - check if the variant exists
                if (variants.get(field_name)) |variant_value| {
                    // Get variant info from module.enums to check for payload
                    if (l.module.enums.get(object_name)) |enum_def| {
                        // Find the variant definition
                        for (enum_def.variant_defs) |variant_def| {
                            if (std.mem.eql(u8, variant_def.name, field_name)) {
                                // Check if this variant has a payload
                                if (variant_def.payload_kind != .none) {
                                    // Lower arguments as payload values
                                    var payload_values: std.ArrayListUnmanaged(ir.Value) = .{};
                                    defer payload_values.deinit(l.allocator);

                                    for (0..args_count) |i| {
                                        const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                                        const arg_val = try lowerExpression(l, arg_idx);
                                        try payload_values.append(l.allocator, arg_val);
                                    }

                                    const payload_slice = try payload_values.toOwnedSlice(l.allocator);

                                    // Emit variant_construct instruction
                                    const result = func.newValue(.variant);
                                    try l.emit(.{
                                        .variant_construct = .{
                                            .tag = @intCast(variant_value),
                                            .payload = payload_slice,
                                            .result = result,
                                            .loc = toIrLoc(call_loc),
                                        },
                                    });

                                    debug.print(.ir, "Emitted variant_construct for {s}.{s} with {d} payload values", .{
                                        object_name, field_name, payload_slice.len,
                                    });

                                    return result;
                                } else {
                                    // Variant has no payload but was called with () - error
                                    l.setErrorContext(
                                        LowerError.TypeMismatch,
                                        "Variant '{s}.{s}' does not take arguments",
                                        .{ object_name, field_name },
                                        call_loc,
                                        "use '{s}.{s}' without parentheses",
                                        .{ object_name, field_name },
                                    );
                                    return LowerError.TypeMismatch;
                                }
                            }
                        }
                    }

                    // Fallback: variant without payload info (old-style enum)
                    // If called with args, that's an error
                    if (args_count > 0) {
                        l.setErrorContext(
                            LowerError.TypeMismatch,
                            "Enum variant '{s}.{s}' does not take arguments",
                            .{ object_name, field_name },
                            call_loc,
                            "simple enum variants cannot have payloads",
                            .{},
                        );
                        return LowerError.TypeMismatch;
                    }
                }
            }

            // File.* static methods -> native function calls
            if (std.mem.eql(u8, object_name, "File")) {
                // Lower arguments for File.* call
                var file_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer file_args.deinit(l.allocator);
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try file_args.append(l.allocator, arg_val);
                }

                // Build native function name: "file.<method>" (lowercase for native lookup)
                var native_name_buf: [64]u8 = undefined;
                const native_name_len = std.fmt.bufPrint(&native_name_buf, "file.{s}", .{field_name}) catch {
                    return LowerError.OutOfMemory;
                };

                // Convert to lowercase for consistent native lookup
                for (native_name_len) |*ch| {
                    ch.* = std.ascii.toLower(ch.*);
                }

                // Allocate the function name so it survives beyond this scope
                const native_name = l.allocator.dupe(u8, native_name_len) catch {
                    return LowerError.OutOfMemory;
                };

                const args_slice = try file_args.toOwnedSlice(l.allocator);
                const result_type = getBuiltinReturnType(native_name, args_slice);
                const result = func.newValue(result_type);

                try l.emit(.{
                    .call = .{
                        .callee = native_name,
                        .args = args_slice,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }

            // StructName.method() - check for impl method first
            if (l.struct_types.get(object_name)) |struct_type| {
                // Build qualified method name: TypeName.method_name
                const qualified_name = std.fmt.allocPrint(
                    l.allocator,
                    "{s}.{s}",
                    .{ object_name, field_name },
                ) catch return LowerError.OutOfMemory;

                // Check if this method exists - first check fn_return_types (populated in impl_sigs phase)
                // This is critical: fn_return_types is populated before fn_bodies are lowered,
                // while module.functions is only populated when impl_bodies are lowered.
                // For static method calls like Type.init(), we need the check to work during fn_bodies.
                var method_exists = l.fn_return_types.contains(qualified_name);

                // Also check module.functions for backwards compatibility
                if (!method_exists) {
                    for (l.module.functions.items) |fn_def| {
                        if (std.mem.eql(u8, fn_def.name, qualified_name)) {
                            method_exists = true;
                            break;
                        }
                    }
                }

                if (method_exists) {
                    // Call the impl method as a static function
                    debug.print(.ir, "lowerCall: calling impl method {s}", .{qualified_name});

                    // Lower arguments
                    var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                    defer method_args.deinit(l.allocator);
                    for (0..args_count) |i| {
                        const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                        const arg_val = try lowerExpression(l, arg_idx);
                        try method_args.append(l.allocator, arg_val);
                    }

                    const args_slice = try method_args.toOwnedSlice(l.allocator);

                    // Get return type from impl method signature
                    const result_type = l.fn_return_types.get(qualified_name) orelse
                        ir.Type{ .@"struct" = struct_type };
                    const result = func.newValue(result_type);

                    try l.emit(.{
                        .call = .{
                            .callee = qualified_name,
                            .args = args_slice,
                            .result = result,
                            .loc = toIrLoc(call_loc),
                        },
                    });
                    return result;
                } else if (std.mem.eql(u8, field_name, "new")) {
                    // No impl method found - fallback to default struct allocation
                    l.allocator.free(qualified_name);

                    // Create a pointer to the struct type for the IR value
                    const struct_type_ptr = try l.allocator.create(ir.Type);
                    struct_type_ptr.* = .{ .@"struct" = struct_type };
                    try l.allocated_types.append(l.allocator, struct_type_ptr);

                    // Allocate stack space for struct
                    const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
                    const unique_name = std.fmt.allocPrint(
                        l.allocator,
                        "{s}${d}",
                        .{ object_name, struct_ptr.id },
                    ) catch return LowerError.OutOfMemory;

                    try l.emit(.{
                        .alloca = .{
                            .ty = .{ .@"struct" = struct_type },
                            .name = unique_name,
                            .result = struct_ptr,
                        },
                    });
                    return struct_ptr;
                } else {
                    l.allocator.free(qualified_name);
                }
            }
        }

        // Check if this is a method call on a map instance: m.set(), m.get(), etc.
        const object_val = try lowerExpression(l, object_idx);
        if (isMapType(object_val.ty)) {
            // Lower arguments
            var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
            defer method_args.deinit(l.allocator);
            for (0..args_count) |i| {
                const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                const arg_val = try lowerExpression(l, arg_idx);
                try method_args.append(l.allocator, arg_val);
            }

            // Dispatch to appropriate map operation
            if (std.mem.eql(u8, field_name, "set") and method_args.items.len >= 2) {
                try l.emit(.{
                    .map_set = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .value = method_args.items[1],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "get") and method_args.items.len >= 1) {
                const value_type = getMapValueType(object_val.ty);
                const result = func.newValue(value_type);
                try l.emit(.{
                    .map_get = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "has") and method_args.items.len >= 1) {
                const result = func.newValue(.bool);
                try l.emit(.{
                    .map_has = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "delete") and method_args.items.len >= 1) {
                try l.emit(.{
                    .map_delete = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "len")) {
                const result = func.newValue(.i64);
                try l.emit(.{
                    .map_len = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "clear")) {
                try l.emit(.{
                    .map_clear = .{
                        .map = object_val,
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "keys")) {
                const result = func.newValue(.string);
                try l.emit(.{
                    .map_keys = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "values")) {
                const result = func.newValue(.string);
                try l.emit(.{
                    .map_values = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }
        }

        // Check if this is a method call on a list instance: l.push(), l.get(), etc.
        if (isListType(object_val.ty)) {
            // Lower arguments
            var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
            defer method_args.deinit(l.allocator);
            for (0..args_count) |i| {
                const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                const arg_val = try lowerExpression(l, arg_idx);
                try method_args.append(l.allocator, arg_val);
            }

            // Get element type from list type for proper return types
            const elem_type: ir.Type = if (object_val.ty == .list)
                object_val.ty.list.element_type.*
            else if (object_val.ty == .ptr and object_val.ty.ptr.* == .list)
                object_val.ty.ptr.list.element_type.*
            else
                .void;

            // Dispatch to appropriate list operation
            if (std.mem.eql(u8, field_name, "push") and method_args.items.len >= 1) {
                try l.emit(.{
                    .list_push = .{
                        .list = object_val,
                        .value = method_args.items[0],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "pop")) {
                const result = func.newValue(elem_type);
                try l.emit(.{
                    .list_pop = .{
                        .list = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "get") and method_args.items.len >= 1) {
                const result = func.newValue(elem_type);
                try l.emit(.{
                    .list_get = .{
                        .list = object_val,
                        .index = method_args.items[0],
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "set") and method_args.items.len >= 2) {
                try l.emit(.{
                    .list_set = .{
                        .list = object_val,
                        .index = method_args.items[0],
                        .value = method_args.items[1],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "len")) {
                const result = func.newValue(.i64);
                try l.emit(.{
                    .list_len = .{
                        .list = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, field_name, "clear")) {
                try l.emit(.{
                    .list_clear = .{
                        .list = object_val,
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, field_name, "to_slice")) {
                // Convert List<T> to []T slice
                // The result type is a slice of the element type
                const slice_type_ptr = try l.allocator.create(ir.Type);
                slice_type_ptr.* = elem_type;
                try l.allocated_types.append(l.allocator, slice_type_ptr);

                const result = func.newValue(.{ .slice = slice_type_ptr });
                try l.emit(.{
                    .list_to_slice = .{
                        .list = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }
        }

        // Check if this is a method call on a trait object - emit call_trait_method
        if (object_val.ty == .trait_object) {
            const trait_obj_type = object_val.ty.trait_object;
            const trait_name = trait_obj_type.trait_name;

            debug.print(.ir, "Trait object method call: {s}.{s}", .{ trait_name, field_name });

            // Get method return type from trait definition
            var return_type: ir.Type = .void;
            if (l.trait_defs.get(trait_name)) |trait_def| {
                for (trait_def.methods) |method| {
                    if (std.mem.eql(u8, method.name, field_name)) {
                        // Resolve return type from TypeIdx (safe now since types are registered)
                        return_type = if (method.return_type_idx != .null)
                            l.lowerTypeIdx(method.return_type_idx) catch .void
                        else
                            .void;
                        break;
                    }
                }
            }

            // Lower arguments
            var trait_args: std.ArrayListUnmanaged(ir.Value) = .{};
            defer trait_args.deinit(l.allocator);

            for (0..args_count) |i| {
                const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                const arg_val = try lowerExpression(l, arg_idx);
                try trait_args.append(l.allocator, arg_val);
            }

            const args_slice = try trait_args.toOwnedSlice(l.allocator);
            const method_name = l.allocator.dupe(u8, field_name) catch return LowerError.OutOfMemory;
            const result = func.newValue(return_type);

            try l.emit(.{
                .call_trait_method = .{
                    .trait_object = object_val,
                    .method_name = method_name,
                    .args = args_slice,
                    .result = result,
                },
            });

            return result;
        }

        // Check if this is a method call on a user-defined struct type
        // This handles calls like: point.distance(other) where Point has an impl block
        // Extract struct type from the value's type (may be direct struct or pointer to struct)
        const maybe_struct_type: ?*const ir.StructType = blk: {
            if (object_val.ty == .@"struct") {
                break :blk object_val.ty.@"struct";
            } else if (object_val.ty == .ptr) {
                // Pointer to struct
                const pointee = object_val.ty.ptr;
                if (pointee.* == .@"struct") {
                    break :blk pointee.@"struct";
                } else if (pointee.* == .ptr) {
                    // Pointer to pointer to struct (common pattern)
                    const inner = pointee.ptr;
                    if (inner.* == .@"struct") {
                        break :blk inner.@"struct";
                    }
                }
            }
            break :blk null;
        };

        if (maybe_struct_type) |struct_type| {
            const type_name = struct_type.name;

            log.debug("Method call on struct: type={s} method={s}", .{ type_name, field_name });

            // Look up inherent impl (impl TypeName { ... })
            // For inherent impls, trait_name == type_name
            if (l.lookupTraitMethod(type_name, type_name, field_name)) |_| {
                log.debug("  -> found in inherent impl", .{});
                // Found the method! Lower arguments and emit call with object as first arg
                var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer method_args.deinit(l.allocator);

                // Build qualified method name first to look up param types
                var method_name_buf: [256]u8 = undefined;
                const qualified_method_name = std.fmt.bufPrint(&method_name_buf, "{s}.{s}", .{ type_name, field_name }) catch {
                    return LowerError.OutOfMemory;
                };

                // Pass self as a single struct value (not expanded into fields)
                // The function signature expects one parameter of struct type
                try method_args.append(l.allocator, object_val);

                // Then the explicit arguments
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try method_args.append(l.allocator, arg_val);
                }

                const args_slice = try method_args.toOwnedSlice(l.allocator);

                const method_name = l.allocator.dupe(u8, qualified_method_name) catch {
                    return LowerError.OutOfMemory;
                };

                // Look up return type from fn_return_types first (set when impl method was lowered)
                const result_type: ir.Type = if (l.fn_return_types.get(method_name)) |rt|
                    rt
                else
                    getBuiltinReturnType(method_name, args_slice);
                const result = func.newValue(result_type);

                debug.print(.ir, "Dispatching method call: {s}.{s} with {d} args result_type={s}", .{ type_name, field_name, args_slice.len, @tagName(result_type) });

                try l.emit(.{
                    .call = .{
                        .callee = method_name,
                        .args = args_slice,
                        .result = result,
                        .loc = toIrLoc(call_loc),
                    },
                });
                return result;
            }

            // Look up trait impl (impl TraitName for TypeName { ... })
            // Check all trait implementations for this type
            if (l.findTraitMethodForType(type_name, field_name)) |qualified_name| {
                log.debug("  -> found in trait impl: {s}", .{qualified_name});
                // Found the method in a trait impl! Lower arguments and emit call
                var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer method_args.deinit(l.allocator);

                // Pass self as a single struct value
                try method_args.append(l.allocator, object_val);

                // Then the explicit arguments
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try method_args.append(l.allocator, arg_val);
                }

                const args_slice = try method_args.toOwnedSlice(l.allocator);

                // Look up return type from fn_return_types (set when impl method was lowered)
                // Fall back to getBuiltinReturnType only if not found
                const fn_rt = l.fn_return_types.get(qualified_name);
                log.debug("  -> fn_return_types lookup: {s} -> {?s}", .{
                    qualified_name,
                    if (fn_rt) |t| @tagName(t) else null,
                });
                const result_type: ir.Type = if (fn_rt) |rt|
                    rt
                else
                    getBuiltinReturnType(qualified_name, args_slice);
                const result = func.newValue(result_type);

                log.debug("  -> result_type={s}", .{@tagName(result_type)});

                try l.emit(.{
                    .call = .{
                        .callee = qualified_name,
                        .args = args_slice,
                        .result = result,
                        .loc = toIrLoc(call_loc),
                    },
                });
                return result;
            }

            // Method not found in impl_methods yet (might be because we're lowering default impl)
            // Still emit the call with qualified name - bytecode emitter will resolve it
            {
                var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer method_args.deinit(l.allocator);

                // Pass self as first argument
                try method_args.append(l.allocator, object_val);

                // Then explicit arguments
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try method_args.append(l.allocator, arg_val);
                }

                // Build qualified method name
                var method_name_buf: [256]u8 = undefined;
                const qualified_method_name = std.fmt.bufPrint(&method_name_buf, "{s}.{s}", .{ type_name, field_name }) catch {
                    return LowerError.OutOfMemory;
                };

                const args_slice = try method_args.toOwnedSlice(l.allocator);
                const method_name = l.allocator.dupe(u8, qualified_method_name) catch {
                    return LowerError.OutOfMemory;
                };

                // Look up return type from fn_return_types first
                const result_type: ir.Type = if (l.fn_return_types.get(method_name)) |rt|
                    rt
                else
                    getBuiltinReturnType(method_name, args_slice);
                const result = func.newValue(result_type);

                debug.print(.ir, "Dispatching fallback struct method call: {s} with {d} args result_type={s}", .{ method_name, args_slice.len, @tagName(result_type) });

                try l.emit(.{
                    .call = .{
                        .callee = method_name,
                        .args = args_slice,
                        .result = result,
                        .loc = toIrLoc(call_loc),
                    },
                });
                return result;
            }
        }
    }

    // Dupe the callee name so Block.deinit can consistently free it
    // Also track if this is a method call that needs object as first argument
    // And track the qualified name for return type lookup (TypeName.methodName)
    var method_object: ?ir.Value = null;
    var qualified_callee_for_lookup: ?[]const u8 = null;
    const callee_name = if (callee_tag == .identifier) blk: {
        const callee_data = l.store.exprData(callee_idx);
        const name_id = callee_data.getName();
        const name = l.strings.get(name_id);
        break :blk l.allocator.dupe(u8, name) catch return LowerError.OutOfMemory;
    } else if (callee_tag == .member) blk: {
        // Method call that wasn't handled by special cases above (e.g., enum methods)
        // Extract method name from member expression and lower the object
        const callee_data = l.store.exprData(callee_idx);
        const object_idx = callee_data.getObject();
        const field_id = callee_data.getField();
        const field_name = l.strings.get(field_id);
        // Lower the object so we can pass it as the first argument
        method_object = try lowerExpression(l, object_idx);

        // For return type lookup, we need the qualified name (TypeName.methodName)
        // Get the type name from the object's type
        if (method_object) |obj| {
            const type_name: ?[]const u8 = switch (obj.ty) {
                .@"struct" => |s| s.name,
                .ptr => |p| switch (p.*) {
                    .@"struct" => |s| s.name,
                    else => null,
                },
                else => null,
            };
            if (type_name) |tn| {
                var qualified_buf: [256]u8 = undefined;
                const qualified = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ tn, field_name }) catch null;
                if (qualified) |q| {
                    qualified_callee_for_lookup = l.allocator.dupe(u8, q) catch null;
                }
            }
        }

        break :blk l.allocator.dupe(u8, field_name) catch return LowerError.OutOfMemory;
    } else blk: {
        // Unsupported callee expression type - emit error
        l.setErrorContext(
            LowerError.UnsupportedFeature,
            "Unsupported callee expression type: {s}",
            .{@tagName(callee_tag)},
            call_loc,
            "only identifiers and method calls are supported",
            .{},
        );
        break :blk l.allocator.dupe(u8, "") catch return LowerError.OutOfMemory;
    };
    defer if (qualified_callee_for_lookup) |q| l.allocator.free(q);

    // Check if this is a database I/O call that needs structure serialization
    const is_db_write = std.mem.eql(u8, callee_name, "db_store") or std.mem.eql(u8, callee_name, "db_write");

    // Lower arguments
    var args: std.ArrayListUnmanaged(ir.Value) = .{};
    defer args.deinit(l.allocator);

    // If this is a method call, prepend the object as the first argument
    if (method_object) |obj| {
        try args.append(l.allocator, obj);
    }

    // Get expected parameter types for this function (if known)
    const param_types = l.fn_param_types.get(callee_name);

    for (0..args_count) |i| {
        const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);

        // For db_store/db_write, the second argument (index 1) is the record
        // If it's a struct-type variable, we need to serialize it
        if (is_db_write and i == 1) {
            const arg_val = try l.lowerStructBufferOrExpression(arg_idx);
            try args.append(l.allocator, arg_val);
        } else {
            var arg_val = try lowerExpression(l, arg_idx);

            // Check if we need to coerce struct to trait object
            if (param_types) |types| {
                if (i < types.len) {
                    const expected_type = types[i];
                    if (expected_type == .trait_object and arg_val.ty == .@"struct") {
                        // Emit make_trait_object to coerce struct to trait object
                        const trait_name = expected_type.trait_object.trait_name;
                        const struct_type = arg_val.ty.@"struct";
                        const type_name = struct_type.name;

                        // Verify the struct implements the trait
                        if (!l.implementsTrait(type_name, trait_name)) {
                            const loc = l.store.exprLoc(arg_idx);
                            l.setErrorContext(
                                LowerError.TypeMismatch,
                                "type '{s}' does not implement trait '{s}'",
                                .{ type_name, trait_name },
                                loc,
                                "checking trait implementation",
                                .{},
                            );
                            return LowerError.TypeMismatch;
                        }

                        // Emit make_trait_object instruction
                        const result = func.newValue(expected_type);
                        try l.emit(.{
                            .make_trait_object = .{
                                .value = arg_val,
                                .trait_name = trait_name,
                                .type_name = type_name,
                                .result = result,
                            },
                        });
                        arg_val = result;
                    }
                }
            }

            try args.append(l.allocator, arg_val);
        }
    }

    // Check if we need to fill in default values for missing arguments
    if (l.fn_param_defaults.get(callee_name)) |defaults| {
        const provided_args = args.items.len;
        const total_params = defaults.len;

        // If fewer args provided than params, fill in defaults
        if (provided_args < total_params) {
            for (provided_args..total_params) |i| {
                const default_expr_raw = defaults[i];
                if (default_expr_raw != 0) {
                    // Lower the default expression
                    const default_expr_idx: ExprIdx = @enumFromInt(default_expr_raw);
                    const default_val = try lowerExpression(l, default_expr_idx);
                    args.append(l.allocator, default_val) catch return LowerError.OutOfMemory;
                } else {
                    // Required param missing - compile-time error
                    const loc = l.store.exprLoc(expr_idx);
                    l.setErrorContext(
                        LowerError.MissingRequiredParam,
                        "missing required parameter {d} in call to '{s}'",
                        .{ i + 1, callee_name },
                        loc,
                        "checking function call arguments",
                        .{},
                    );
                    return LowerError.MissingRequiredParam;
                }
            }
        }
    }

    const args_slice = try args.toOwnedSlice(l.allocator);
    // NOTE: args_slice is freed by Block.deinit when the call instruction is cleaned up

    // Special handling for len() - type-aware
    if (std.mem.eql(u8, callee_name, "len") and args_slice.len == 1) {
        const arg = args_slice[0];
        const result = func.newValue(.i64);

        if (arg.ty == .array) {
            // Array length - emit array_len
            try l.emit(.{ .array_len = .{ .operand = arg, .result = result } });
            l.allocator.free(callee_name);
            return result;
        } else if (arg.ty == .list) {
            // Dynamic list length - emit list_len
            try l.emit(.{
                .list_len = .{
                    .list = arg,
                    .result = result,
                    .loc = null,
                },
            });
            l.allocator.free(callee_name);
            return result;
        } else if (arg.ty == .map) {
            // Map length - emit map_len
            try l.emit(.{
                .map_len = .{
                    .map = arg,
                    .result = result,
                    .loc = null,
                },
            });
            l.allocator.free(callee_name);
            return result;
        }
        // Fall through to str_len for strings or other types
    }

    // Check if callee is a local variable (could be a closure)
    // If so, we need to emit call_indirect instead of call
    if (callee_tag == .identifier and l.scopes.get(callee_name) != null) {
        // Callee is a variable - lower it to get the value and use call_indirect
        const callee_value = try lowerExpression(l, callee_idx);

        // Create a dummy function type for the call_indirect
        // In a more complete implementation, we'd infer the actual type
        const dummy_sig = l.allocator.create(ir.FunctionType) catch return LowerError.OutOfMemory;
        dummy_sig.* = .{
            .params = &[_]ir.FunctionType.Param{},
            .return_type = .void,
            .is_variadic = false,
        };

        const result = func.newValue(.void);

        try l.emit(.{
            .call_indirect = .{
                .sig = dummy_sig,
                .callee = callee_value,
                .args = args_slice,
                .result = result,
            },
        });

        return result;
    }

    // Check if this is a call to a generic function that needs instantiation
    // We infer type arguments from the argument types
    var actual_callee: []const u8 = callee_name;
    if (l.generic_fn_defs.get(callee_name)) |generic_def| {
        // Infer type arguments from argument types
        // Simple 1:1 mapping: first N args determine first N type params
        const type_param_count = generic_def.type_param_count;
        if (args_slice.len >= type_param_count) {
            var type_args: [16]ir.Type = undefined; // Max 16 type params
            for (0..type_param_count) |i| {
                var inferred_type = args_slice[i].ty;

                // Coerce [N]u8 array types to string for generic inference
                // String literals have internal type [N]u8, but for generic functions
                // we want to treat them as `string` type so that identity("hello")
                // becomes identity<string> not identity<[5]u8>
                if (inferred_type == .array) {
                    if (inferred_type.array.element.* == .u8) {
                        inferred_type = .{ .string = {} };
                        debug.print(.ir, "Generic type inference: coerced [N]u8 array to string", .{});
                    }
                }

                type_args[i] = inferred_type;
            }

            // Instantiate the generic function
            if (try l.instantiateGenericFn(callee_name, type_args[0..type_param_count])) |instantiated_name| {
                // Free the original callee_name since we're using the instantiated one
                l.allocator.free(callee_name);
                actual_callee = instantiated_name;
                debug.print(.ir, "Calling instantiated generic: {s}", .{actual_callee});
            }
        }
    }

    // Determine result type:
    // 1. For method calls, first try qualified name (TypeName.methodName)
    // 2. Then try the callee name directly
    // 3. Otherwise fall back to builtin return type inference
    const result_type: ir.Type = blk: {
        // First try qualified name for method calls (e.g., Parser.parseModule)
        if (qualified_callee_for_lookup) |qname| {
            if (l.fn_return_types.get(qname)) |rt| {
                break :blk rt;
            }
        }
        // Then try the actual callee name
        if (l.fn_return_types.get(actual_callee)) |rt| {
            break :blk rt;
        }
        // Fall back to builtin return type
        break :blk getBuiltinReturnType(actual_callee, args_slice);
    };
    const result = func.newValue(result_type);

    try l.emit(.{
        .call = .{
            .callee = actual_callee,
            .args = args_slice,
            .result = result,
            .loc = toIrLoc(call_loc),
        },
    });

    return result;
}

/// Lower a method call expression
pub fn lowerMethodCall(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);
    const method_loc = l.store.exprLoc(expr_idx);

    // Method calls have object in a, method name + args in extra
    const object_idx: ExprIdx = @enumFromInt(data.a);
    const extra_start = data.b;

    const method_id: StringId = @enumFromInt(l.store.extra_data.items[extra_start]);
    const args_count = l.store.extra_data.items[extra_start + 1];

    // Get method name from string interner (not duped yet - may not need it for map methods)
    const method_name_borrowed = l.strings.get(method_id);

    // Get object value
    const object_val = try lowerExpression(l, object_idx);

    // Check if object is a map type - emit map instructions
    if (isMapType(object_val.ty)) {
        // Lower arguments
        var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
        defer method_args.deinit(l.allocator);
        for (0..args_count) |i| {
            const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_start + 2 + i]);
            const arg_val = try lowerExpression(l, arg_idx);
            try method_args.append(l.allocator, arg_val);
        }

        // Dispatch to appropriate map operation
        if (std.mem.eql(u8, method_name_borrowed, "set") and method_args.items.len >= 2) {
            try l.emit(.{
                .map_set = .{
                    .map = object_val,
                    .key = method_args.items[0],
                    .value = method_args.items[1],
                    .loc = null,
                },
            });
            return func.newValue(.void);
        } else if (std.mem.eql(u8, method_name_borrowed, "get") and method_args.items.len >= 1) {
            const value_type = getMapValueType(object_val.ty);
            const result = func.newValue(value_type);
            try l.emit(.{
                .map_get = .{
                    .map = object_val,
                    .key = method_args.items[0],
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        } else if (std.mem.eql(u8, method_name_borrowed, "has") and method_args.items.len >= 1) {
            const result = func.newValue(.bool);
            try l.emit(.{
                .map_has = .{
                    .map = object_val,
                    .key = method_args.items[0],
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        } else if (std.mem.eql(u8, method_name_borrowed, "delete") and method_args.items.len >= 1) {
            try l.emit(.{
                .map_delete = .{
                    .map = object_val,
                    .key = method_args.items[0],
                    .loc = null,
                },
            });
            return func.newValue(.void);
        } else if (std.mem.eql(u8, method_name_borrowed, "len")) {
            const result = func.newValue(.i64);
            try l.emit(.{
                .map_len = .{
                    .map = object_val,
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        } else if (std.mem.eql(u8, method_name_borrowed, "clear")) {
            try l.emit(.{
                .map_clear = .{
                    .map = object_val,
                    .loc = null,
                },
            });
            return func.newValue(.void);
        } else if (std.mem.eql(u8, method_name_borrowed, "keys")) {
            const result = func.newValue(.string); // Returns comma-separated keys
            try l.emit(.{
                .map_keys = .{
                    .map = object_val,
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        } else if (std.mem.eql(u8, method_name_borrowed, "values")) {
            const result = func.newValue(.string); // Returns comma-separated values
            try l.emit(.{
                .map_values = .{
                    .map = object_val,
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        }
    }

    // Check if object is a trait object - emit trait method call
    log.debug("Method call on object type: {any}", .{object_val.ty});
    if (object_val.ty == .trait_object) {
        const trait_obj_type = object_val.ty.trait_object;
        const trait_name = trait_obj_type.trait_name;

        // Get method return type from trait definition
        var return_type: ir.Type = .void;
        if (l.trait_defs.get(trait_name)) |trait_def| {
            for (trait_def.methods) |method| {
                if (std.mem.eql(u8, method.name, method_name_borrowed)) {
                    // Resolve return type from TypeIdx (safe now since types are registered)
                    return_type = if (method.return_type_idx != .null)
                        l.lowerTypeIdx(method.return_type_idx) catch .void
                    else
                        .void;
                    break;
                }
            }
        }

        // Lower arguments
        var args: std.ArrayListUnmanaged(ir.Value) = .{};
        defer args.deinit(l.allocator);

        for (0..args_count) |i| {
            const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_start + 2 + i]);
            const arg_val = try lowerExpression(l, arg_idx);
            try args.append(l.allocator, arg_val);
        }

        const args_slice = try args.toOwnedSlice(l.allocator);
        const method_name = l.allocator.dupe(u8, method_name_borrowed) catch return LowerError.OutOfMemory;
        const result = func.newValue(return_type);

        try l.emit(.{
            .call_trait_method = .{
                .trait_object = object_val,
                .method_name = method_name,
                .args = args_slice,
                .result = result,
            },
        });

        return result;
    }

    // Check if this is an impl method call on a struct
    // For struct types, we need to use a qualified name like "Counter.get"
    var callee_name: []const u8 = undefined;
    const self_arg = object_val;

    if (object_val.ty == .@"struct") {
        const struct_type = object_val.ty.@"struct";
        // Construct qualified method name: TypeName.method_name
        callee_name = std.fmt.allocPrint(
            l.allocator,
            "{s}.{s}",
            .{ struct_type.name, method_name_borrowed },
        ) catch return LowerError.OutOfMemory;
        debug.print(.ir, "lowerMethodCall: struct method {s}", .{callee_name});
    } else if (object_val.ty == .ptr) {
        // Pointer to struct - dereference to get struct type name
        const pointee = object_val.ty.ptr.*;
        if (pointee == .@"struct") {
            const struct_type = pointee.@"struct";
            callee_name = std.fmt.allocPrint(
                l.allocator,
                "{s}.{s}",
                .{ struct_type.name, method_name_borrowed },
            ) catch return LowerError.OutOfMemory;
            debug.print(.ir, "lowerMethodCall: struct ptr method {s}", .{callee_name});
        } else {
            callee_name = l.allocator.dupe(u8, method_name_borrowed) catch return LowerError.OutOfMemory;
        }
    } else {
        // Fallback to regular method name
        callee_name = l.allocator.dupe(u8, method_name_borrowed) catch return LowerError.OutOfMemory;
    }

    // Build arguments: self first, then remaining args
    var args: std.ArrayListUnmanaged(ir.Value) = .{};
    defer args.deinit(l.allocator);
    try args.append(l.allocator, self_arg);

    for (0..args_count) |i| {
        const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_start + 2 + i]);
        const arg_val = try lowerExpression(l, arg_idx);
        try args.append(l.allocator, arg_val);
    }

    const args_slice = try args.toOwnedSlice(l.allocator);
    // NOTE: args_slice is freed by Block.deinit when the call instruction is cleaned up

    // Look up return type from fn_return_types first (set when method was lowered)
    // This correctly handles trait impl methods like "Point.to_string" -> string
    const result_type: ir.Type = if (l.fn_return_types.get(callee_name)) |rt|
        rt
    else
        getBuiltinReturnType(callee_name, args_slice);
    const result = func.newValue(result_type);

    debug.print(.ir, "lowerMethodCall: callee={s} result_type={s}", .{ callee_name, @tagName(result_type) });

    try l.emit(.{
        .call = .{
            .callee = callee_name,
            .args = args_slice,
            .result = result,
            .loc = toIrLoc(method_loc),
        },
    });

    return result;
}

// ============================================================================
// Initialization Expressions
// ============================================================================

/// Lower an array initialization expression
pub fn lowerArrayInit(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    log.debug("lowerArrayInit: entering", .{});
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

    // data.a = start index in extra_data (where count is stored)
    // data.b = element count
    const extra_start: ast.ExtraIdx = @enumFromInt(data.a);
    const elem_count = data.b;

    if (elem_count == 0) {
        // Empty array - return null pointer for now
        const result = func.newValue(.void);
        return result;
    }

    // Lower all elements to get their values and infer element type
    var elem_values: std.ArrayListUnmanaged(ir.Value) = .{};
    defer elem_values.deinit(l.allocator);

    var i: u32 = 0;
    while (i < elem_count) : (i += 1) {
        const elem_idx_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1 + i));
        const elem_idx: ExprIdx = @enumFromInt(elem_idx_raw);
        const elem_val = try lowerExpression(l, elem_idx);
        elem_values.append(l.allocator, elem_val) catch return LowerError.OutOfMemory;
    }

    // Use first element's type as array element type
    const elem_type = elem_values.items[0].ty;
    const elem_type_ptr = try l.allocator.create(ir.Type);
    elem_type_ptr.* = elem_type;
    try l.allocated_types.append(l.allocator, elem_type_ptr);

    // Create array type
    const array_type = ir.Type{ .array = .{ .element = elem_type_ptr, .length = elem_count } };
    const array_type_ptr = try l.allocator.create(ir.Type);
    array_type_ptr.* = array_type;
    try l.allocated_types.append(l.allocator, array_type_ptr);

    // Allocate stack space for array
    const array_ptr = func.newValue(.{ .ptr = array_type_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = array_type,
            .name = "_array_init",
            .result = array_ptr,
        },
    });

    // Store each element into the array
    for (elem_values.items, 0..) |elem_val, idx| {
        // Create index constant
        const idx_val = func.newValue(.i64);
        try l.emit(.{
            .iconst = .{ .ty = .i64, .value = @intCast(idx), .result = idx_val },
        });

        // Store element at index
        try l.emit(.{
            .array_store = .{
                .array_ptr = array_ptr,
                .index = idx_val,
                .value = elem_val,
            },
        });
    }

    // Return pointer to array
    return array_ptr;
}

/// Lower a struct initialization expression
pub fn lowerStructInit(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

    // data.a = type_name (StringId)
    // data.b = start index in extra_data
    const type_name_id: StringId = @enumFromInt(data.a);
    const type_name = l.strings.get(type_name_id);
    const loc = l.store.exprLoc(expr_idx);
    if (type_name.len == 0) {
        l.setErrorContext(
            LowerError.UndefinedType,
            "Empty type name in struct literal",
            .{},
            loc,
            "struct literal must have a type name",
            .{},
        );
        return LowerError.UndefinedType;
    }

    // Look up struct type
    const struct_type = l.struct_types.get(type_name) orelse {
        l.setErrorContext(
            LowerError.UndefinedType,
            "Unknown struct type '{s}'",
            .{type_name},
            loc,
            "type '{s}' is not defined",
            .{type_name},
        );
        return LowerError.UndefinedType;
    };

    // Get field count and field data from extra
    const extra_start: ast.ExtraIdx = @enumFromInt(data.b);
    const field_count = l.store.getExtra(extra_start);

    // Create struct type for allocation
    const struct_ir_type = ir.Type{ .@"struct" = struct_type };
    const struct_type_ptr = try l.allocator.create(ir.Type);
    struct_type_ptr.* = struct_ir_type;
    try l.allocated_types.append(l.allocator, struct_type_ptr);

    // Allocate stack space for struct
    // Use a unique name combining type name and value ID to ensure each instance
    // gets its own slots in the bytecode emitter
    const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
    const unique_name = std.fmt.allocPrint(
        l.allocator,
        "{s}${d}",
        .{ type_name, struct_ptr.id },
    ) catch return LowerError.OutOfMemory;
    try l.emit(.{
        .alloca = .{
            .ty = struct_ir_type,
            .name = unique_name,
            .result = struct_ptr,
        },
    });

    // Store each field
    var field_i: u32 = 0;
    while (field_i < field_count) : (field_i += 1) {
        // Each field has: (field_name_id, expr_idx) in extra_data
        const field_name_id_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1 + field_i * 2));
        const field_expr_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 2 + field_i * 2));

        const field_name_id: StringId = @enumFromInt(field_name_id_raw);
        const field_name = l.strings.get(field_name_id);
        const field_expr_idx: ExprIdx = @enumFromInt(field_expr_raw);

        // Find field index in struct type
        var field_index: ?u32 = null;
        for (struct_type.fields, 0..) |field, idx| {
            if (std.mem.eql(u8, field.name, field_name)) {
                field_index = @intCast(idx);
                break;
            }
        }

        if (field_index) |idx| {
            // Lower the field value
            var field_val = try lowerExpression(l, field_expr_idx);

            // Get field type
            const field_type = struct_type.fields[idx].ty;

            // Fix for empty array/slice literals: lowerArrayInit returns void for empty [],
            // but if the field type is slice/list, create a properly typed value
            if (field_val.ty == .void) {
                switch (field_type) {
                    .slice => {
                        // Empty slice: use the field's slice type
                        field_val = func.newValue(field_type);
                    },
                    .list => {
                        // Empty list: use the field's list type
                        field_val = func.newValue(field_type);
                    },
                    else => {},
                }
            }

            debug.print(.ir, "struct init field {d}: '{s}' val.id={d} val.ty={s}", .{
                idx,
                field_name,
                field_val.id,
                @tagName(field_val.ty),
            });
            const field_type_ptr = try l.allocator.create(ir.Type);
            field_type_ptr.* = field_type;
            try l.allocated_types.append(l.allocator, field_type_ptr);

            // Get pointer to field
            const field_ptr = func.newValue(.{ .ptr = field_type_ptr });
            debug.print(.ir, "  emitting field_ptr: struct_ptr.id={d} field_idx={d} result.id={d}", .{
                struct_ptr.id,
                idx,
                field_ptr.id,
            });
            try l.emit(.{
                .field_ptr = .{
                    .struct_ptr = struct_ptr,
                    .field_index = idx,
                    .result = field_ptr,
                },
            });

            // Store value to field
            debug.print(.ir, "  emitting store: ptr.id={d} val.id={d}", .{
                field_ptr.id,
                field_val.id,
            });
            try l.emit(.{
                .store = .{
                    .ptr = field_ptr,
                    .value = field_val,
                },
            });
        } else {
            debug.print(.ir, "struct init: unknown field '{s}'", .{field_name});
        }
        // Skip unknown fields silently (could add warning)
    }

    // Return pointer to struct
    debug.print(.ir, "struct init done: returning ptr.id={d}", .{struct_ptr.id});
    return struct_ptr;
}

/// Lower a generic struct initialization expression: Box<i64>{ .value = 42 }
pub fn lowerGenericStructInit(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

    // data.a = type_name (StringId)
    // data.b = start index in extra_data
    // extra_data layout: [type_arg_count, type_args..., field_count, field_name/value pairs...]
    const type_name_id: StringId = @enumFromInt(data.a);
    const type_name = l.strings.get(type_name_id);
    if (type_name.len == 0) {
        const err_loc = l.store.exprLoc(expr_idx);
        l.setErrorContext(
            LowerError.UndefinedType,
            "Empty type name in generic struct literal",
            .{},
            err_loc,
            "generic struct literal must have a type name",
            .{},
        );
        return LowerError.UndefinedType;
    }

    const extra_start: ast.ExtraIdx = @enumFromInt(data.b);
    const type_arg_count = l.store.getExtra(extra_start);

    // Build type arguments array
    var type_args: std.ArrayListUnmanaged(ir.Type) = .{};
    defer type_args.deinit(l.allocator);

    for (0..type_arg_count) |i| {
        const type_arg_idx = TypeIdx.fromInt(l.store.extra_data.items[@intFromEnum(extra_start) + 1 + i]);
        const arg_type = try lowerTypeIdx(l, type_arg_idx);
        try type_args.append(l.allocator, arg_type);
    }

    // Get source location for error reporting
    const expr_loc = l.store.exprLoc(expr_idx);

    // Check if the generic struct definition exists and provide helpful error
    const generic_def = l.generic_struct_defs.get(type_name);
    if (generic_def == null) {
        // Check if it's a built-in like List or Map
        const is_builtin = std.mem.eql(u8, type_name, "List") or
            std.mem.eql(u8, type_name, "Map") or
            std.mem.eql(u8, type_name, "Option") or
            std.mem.eql(u8, type_name, "Result");

        if (is_builtin) {
            l.setErrorContext(
                LowerError.UndefinedType,
                "Cannot use built-in generic type '{s}' in struct literal syntax",
                .{type_name},
                expr_loc,
                "'{s}' is a built-in type. Use '{s}.new()' or '{s}{{}}' syntax instead",
                .{ type_name, type_name, type_name },
            );
        } else {
            l.setErrorContext(
                LowerError.UndefinedType,
                "Generic struct '{s}' is not defined",
                .{type_name},
                expr_loc,
                "no generic struct named '{s}' was found in scope. Did you forget to define it?",
                .{type_name},
            );
        }
        return LowerError.UndefinedType;
    }

    // Check type argument count
    if (type_args.items.len != generic_def.?.type_param_count) {
        l.setErrorContext(
            LowerError.UndefinedType,
            "Wrong number of type arguments for generic struct '{s}'",
            .{type_name},
            expr_loc,
            "expected {d} type argument(s), but got {d}",
            .{ generic_def.?.type_param_count, type_args.items.len },
        );
        return LowerError.UndefinedType;
    }

    // Instantiate the generic struct
    const struct_type = try l.instantiateGenericStruct(type_name, type_args.items) orelse {
        l.setErrorContext(
            LowerError.UndefinedType,
            "Failed to instantiate generic struct '{s}'",
            .{type_name},
            expr_loc,
            "internal error: instantiation failed after validation passed",
            .{},
        );
        return LowerError.UndefinedType;
    };

    // Get field data from extra_data
    const field_count_offset = @intFromEnum(extra_start) + 1 + type_arg_count;
    const field_count = l.store.extra_data.items[field_count_offset];

    // Create struct type for allocation
    const struct_ir_type = ir.Type{ .@"struct" = struct_type };
    const struct_type_ptr = try l.allocator.create(ir.Type);
    struct_type_ptr.* = struct_ir_type;
    try l.allocated_types.append(l.allocator, struct_type_ptr);

    // Allocate stack space for struct
    // Use a unique name combining type name and value ID to ensure each instance
    // gets its own slots in the bytecode emitter
    const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
    const unique_name = std.fmt.allocPrint(
        l.allocator,
        "{s}${d}",
        .{ type_name, struct_ptr.id },
    ) catch return LowerError.OutOfMemory;
    try l.emit(.{
        .alloca = .{
            .ty = struct_ir_type,
            .name = unique_name,
            .result = struct_ptr,
        },
    });

    // Store each field
    var field_i: u32 = 0;
    while (field_i < field_count) : (field_i += 1) {
        // Each field has: (field_name_id, expr_idx) in extra_data
        const field_name_id_raw = l.store.extra_data.items[field_count_offset + 1 + field_i * 2];
        const field_expr_raw = l.store.extra_data.items[field_count_offset + 2 + field_i * 2];

        const field_name_id: StringId = @enumFromInt(field_name_id_raw);
        const field_name = l.strings.get(field_name_id);
        const field_expr_idx: ExprIdx = @enumFromInt(field_expr_raw);

        // Find field index in struct type
        var field_index: ?u32 = null;
        for (struct_type.fields, 0..) |field, idx| {
            if (std.mem.eql(u8, field.name, field_name)) {
                field_index = @intCast(idx);
                break;
            }
        }

        if (field_index) |idx| {
            // Lower the field value
            const field_val = try lowerExpression(l, field_expr_idx);

            // Get field type
            const field_type = struct_type.fields[idx].ty;
            const field_type_ptr = try l.allocator.create(ir.Type);
            field_type_ptr.* = field_type;
            try l.allocated_types.append(l.allocator, field_type_ptr);

            // Get pointer to field
            const field_ptr = func.newValue(.{ .ptr = field_type_ptr });
            try l.emit(.{
                .field_ptr = .{
                    .struct_ptr = struct_ptr,
                    .field_index = idx,
                    .result = field_ptr,
                },
            });

            // Store value to field
            try l.emit(.{
                .store = .{
                    .ptr = field_ptr,
                    .value = field_val,
                },
            });
        }
        // Skip unknown fields silently
    }

    // Return pointer to struct
    return struct_ptr;
}

// ============================================================================
// New Expression (Heap Allocation)
// ============================================================================

/// Lower a new expression: new Type{ ... } or new Type<T>{ ... } or new Type<T>
/// Allocates on the heap with ARC management
pub fn lowerNewExpr(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);
    const loc = l.store.exprLoc(expr_idx);

    // data.a = type_name (StringId)
    // data.b = start index in extra_data
    const type_name_id: StringId = @enumFromInt(data.a);
    const type_name = l.strings.get(type_name_id);
    if (type_name.len == 0) {
        l.setErrorContext(
            LowerError.UndefinedType,
            "Empty type name in 'new' expression",
            .{},
            loc,
            "'new' expression must have a type name",
            .{},
        );
        return LowerError.UndefinedType;
    }

    const extra_start: ast.ExtraIdx = @enumFromInt(data.b);
    const type_arg_count = l.store.getExtra(extra_start);

    // Calculate where count/marker is located (after type args)
    const count_idx: ast.ExtraIdx = @enumFromInt(@intFromEnum(extra_start) + 1 + type_arg_count);
    const count_or_marker = l.store.getExtra(count_idx);

    // Check if this is element mode (high bit set) - for List/Map collections
    const is_element_mode = (count_or_marker & 0x80000000) != 0;

    if (is_element_mode) {
        // Collection initialization: new List<i64>{ 1, 2, 3 }
        const elem_count = count_or_marker & 0x7FFFFFFF;

        // Check if it's List or Map
        if (std.mem.eql(u8, type_name, "List")) {
            return lowerNewList(l, func, extra_start, type_arg_count, count_idx, elem_count, loc);
        } else if (std.mem.eql(u8, type_name, "Map")) {
            return lowerNewMap(l, func, extra_start, type_arg_count, count_idx, elem_count, loc);
        } else {
            l.setErrorContext(
                LowerError.UnsupportedFeature,
                "Collection-style initialization only valid for List and Map",
                .{},
                loc,
                "use field-style initialization (.field = value) for struct '{s}'",
                .{type_name},
            );
            return LowerError.UnsupportedFeature;
        }
    }

    // Field-style or empty: struct or empty collection
    const field_count = count_or_marker;

    // Check for empty collection: new List<i64> or new Map<K,V>
    if (field_count == 0 and type_arg_count > 0) {
        if (std.mem.eql(u8, type_name, "List")) {
            return lowerNewList(l, func, extra_start, type_arg_count, count_idx, 0, loc);
        } else if (std.mem.eql(u8, type_name, "Map")) {
            return lowerNewMap(l, func, extra_start, type_arg_count, count_idx, 0, loc);
        }
        // Fall through to struct handling for generic structs like Box<i64>{}
    }

    // Struct initialization: new Point{ .x = 1 } or new Box<i64>{ .value = 42 }
    return lowerNewStruct(l, func, type_name, extra_start, type_arg_count, count_idx, field_count, loc);
}

/// Lower new List<T>{ elements... } or new List<T>
fn lowerNewList(
    l: *Lowerer,
    func: *ir.Function,
    extra_start: ast.ExtraIdx,
    type_arg_count: u32,
    count_idx: ast.ExtraIdx,
    elem_count: u32,
    loc: SourceLoc,
) LowerError!ir.Value {
    // Get element type for the list from type arguments
    // extra_data layout: [type_arg_count, type_args..., ...]
    // extra_start points to type_arg_count, so first type arg is at extra_start + 1

    var elem_type: ir.Type = .i64; // Default fallback
    if (type_arg_count > 0) {
        // Read the first type argument (element type)
        const type_arg_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1));
        const type_arg_idx: ast.TypeIdx = @enumFromInt(type_arg_raw);
        elem_type = try lowerTypeIdx(l, type_arg_idx);
    }

    // Allocate element type pointer
    const elem_type_ptr = try l.allocator.create(ir.Type);
    elem_type_ptr.* = elem_type;
    try l.allocated_types.append(l.allocator, elem_type_ptr);

    // Allocate a new list type
    const list_type = try l.allocator.create(ir.ListType);
    list_type.* = .{ .element_type = elem_type_ptr };
    try l.module.allocated_list_types.append(l.allocator, list_type);

    // Create list_new instruction
    const list_val = func.newValue(.{ .list = list_type });
    try l.emit(.{
        .list_new = .{
            .element_type = elem_type_ptr,
            .result = list_val,
            .loc = toIrLoc(loc),
        },
    });

    // Push each element
    for (0..elem_count) |i| {
        const elem_expr_raw = l.store.getExtra(@enumFromInt(@intFromEnum(count_idx) + 1 + i));
        const elem_expr_idx: ExprIdx = @enumFromInt(elem_expr_raw);
        const elem_val = try lowerExpression(l, elem_expr_idx);

        try l.emit(.{
            .list_push = .{
                .list = list_val,
                .value = elem_val,
                .loc = toIrLoc(loc),
            },
        });
    }

    return list_val;
}

/// Lower new Map<K,V>{ ... } or new Map<K,V>
fn lowerNewMap(
    l: *Lowerer,
    func: *ir.Function,
    extra_start: ast.ExtraIdx,
    type_arg_count: u32,
    count_idx: ast.ExtraIdx,
    elem_count: u32,
    loc: SourceLoc,
) LowerError!ir.Value {
    _ = count_idx;
    _ = elem_count;

    // Get key and value types from type arguments
    // extra_data layout: [type_arg_count, type_args..., ...]
    // Map<K, V> has 2 type args: key type at extra_start+1, value type at extra_start+2

    var key_type: ir.Type = .string; // Default fallback
    var val_type: ir.Type = .i64; // Default fallback
    if (type_arg_count >= 2) {
        // Read key type (first type argument)
        const key_type_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1));
        const key_type_idx: ast.TypeIdx = @enumFromInt(key_type_raw);
        key_type = try lowerTypeIdx(l, key_type_idx);

        // Read value type (second type argument)
        const val_type_raw = l.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 2));
        const val_type_idx: ast.TypeIdx = @enumFromInt(val_type_raw);
        val_type = try lowerTypeIdx(l, val_type_idx);
    }

    // Allocate key and value type pointers
    const key_type_ptr = try l.allocator.create(ir.Type);
    key_type_ptr.* = key_type;
    try l.allocated_types.append(l.allocator, key_type_ptr);

    const val_type_ptr = try l.allocator.create(ir.Type);
    val_type_ptr.* = val_type;
    try l.allocated_types.append(l.allocator, val_type_ptr);

    // Allocate map type
    const map_type = try l.allocator.create(ir.MapType);
    map_type.* = .{ .key_type = key_type_ptr, .value_type = val_type_ptr };

    // Create map_new instruction
    const map_val = func.newValue(.{ .map = map_type });
    try l.emit(.{
        .map_new = .{
            .result = map_val,
            .flags = 1, // bit 0 = case_sensitive (true), bit 1 = preserve_spaces (false)
            .loc = toIrLoc(loc),
        },
    });

    // TODO: If we have key-value pairs, add them
    // For now, just create an empty map

    return map_val;
}

/// Lower new StructType{ .field = value, ... }
fn lowerNewStruct(
    l: *Lowerer,
    func: *ir.Function,
    type_name: []const u8,
    extra_start: ast.ExtraIdx,
    type_arg_count: u32,
    count_idx: ast.ExtraIdx,
    field_count: u32,
    loc: SourceLoc,
) LowerError!ir.Value {
    // Look up struct type (handle generics if needed)
    var struct_type: *const ir.StructType = undefined;

    if (type_arg_count > 0) {
        // Generic struct: instantiate with type arguments
        var type_args: std.ArrayListUnmanaged(ir.Type) = .{};
        defer type_args.deinit(l.allocator);

        for (0..type_arg_count) |i| {
            const type_arg_idx = TypeIdx.fromInt(l.store.extra_data.items[@intFromEnum(extra_start) + 1 + i]);
            const arg_type = try lowerTypeIdx(l, type_arg_idx);
            try type_args.append(l.allocator, arg_type);
        }

        struct_type = try l.instantiateGenericStruct(type_name, type_args.items) orelse {
            l.setErrorContext(
                LowerError.UndefinedType,
                "Failed to instantiate generic struct '{s}'",
                .{type_name},
                loc,
                "check that the struct is defined and type arguments are valid",
                .{},
            );
            return LowerError.UndefinedType;
        };
    } else {
        // Non-generic struct
        struct_type = l.struct_types.get(type_name) orelse {
            l.setErrorContext(
                LowerError.UndefinedType,
                "Unknown struct type '{s}'",
                .{type_name},
                loc,
                "type '{s}' is not defined",
                .{type_name},
            );
            return LowerError.UndefinedType;
        };
    }

    // Create struct type for allocation
    const struct_ir_type = ir.Type{ .@"struct" = struct_type };

    // Emit heap_alloc instruction - result is a heap_record type
    const heap_record = func.newValue(.{ .heap_record = struct_type });
    try l.emit(.{
        .heap_alloc = .{
            .ty = struct_ir_type,
            .field_count = @intCast(struct_type.fields.len),
            .result = heap_record,
        },
    });

    // Store each field value
    for (0..field_count) |i| {
        const field_name_id_raw = l.store.getExtra(@enumFromInt(@intFromEnum(count_idx) + 1 + i * 2));
        const field_expr_raw = l.store.getExtra(@enumFromInt(@intFromEnum(count_idx) + 2 + i * 2));

        const field_name_id: StringId = @enumFromInt(field_name_id_raw);
        const field_name = l.strings.get(field_name_id);
        const field_expr_idx: ExprIdx = @enumFromInt(field_expr_raw);

        // Find field index in struct type
        var field_index: ?u32 = null;
        for (struct_type.fields, 0..) |field, idx| {
            if (std.mem.eql(u8, field.name, field_name)) {
                field_index = @intCast(idx);
                break;
            }
        }

        if (field_index) |idx| {
            // Lower the field value
            const field_val = try lowerExpression(l, field_expr_idx);

            // Emit store_field_heap instruction
            try l.emit(.{
                .store_field_heap = .{
                    .record = heap_record,
                    .field_index = idx,
                    .value = field_val,
                },
            });
        }
        // Skip unknown fields silently
    }

    // Return the heap record
    return heap_record;
}

// ============================================================================
// Lambda Expressions
// ============================================================================

/// Lower a lambda expression
/// Handles closures by detecting captured variables from outer scope
pub fn lowerLambda(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const outer_func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

    // data.a = params_start in extra_data
    // extra_data layout: [param_count, param1_name, param1_type, ..., body_stmt]
    const params_start: ast.ExtraIdx = @enumFromInt(data.a);
    const param_count = l.store.getExtra(params_start);

    // Generate unique lambda name
    const lambda_name = std.fmt.allocPrint(l.allocator, "_lambda_{d}", .{l.lambda_counter}) catch return LowerError.OutOfMemory;
    l.lambda_counter += 1;

    // Parse parameters first to know which names are lambda-local
    var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
    defer params.deinit(l.allocator);

    var param_names = std.StringHashMap(void).init(l.allocator);
    defer param_names.deinit();

    var extra_idx = @intFromEnum(params_start) + 1;
    for (0..param_count) |_| {
        const param_name_id: StringId = @enumFromInt(l.store.extra_data.items[extra_idx]);
        const param_type_idx: TypeIdx = @enumFromInt(l.store.extra_data.items[extra_idx + 1]);
        const param_direction_raw: u32 = l.store.extra_data.items[extra_idx + 2];
        extra_idx += 3; // 3 values per param: name, type, direction (matches parser)

        const param_name = l.strings.get(param_name_id);
        if (param_name.len == 0) continue;

        // Track parameter names for capture detection
        param_names.put(param_name, {}) catch return LowerError.OutOfMemory;

        // Use type if specified, otherwise default to void (inferred later)
        const param_type = if (param_type_idx != .null)
            try lowerTypeIdx(l, param_type_idx)
        else
            ir.Type.void;

        // is_ref: 0 = by value, non-zero = by reference
        const is_ref = param_direction_raw != 0;

        params.append(l.allocator, .{
            .name = param_name,
            .ty = param_type,
            .is_ref = is_ref,
        }) catch return LowerError.OutOfMemory;
    }

    // Get body statement index
    const body_idx: StmtIdx = @enumFromInt(l.store.extra_data.items[extra_idx]);

    // ========================================================================
    // Capture Detection - scan lambda body for free variables
    // ========================================================================
    var captures = std.StringHashMap(ir.Value).init(l.allocator);
    defer captures.deinit();

    // Use FreeVarCollector to find all referenced variables in the lambda body
    var collector = FreeVarCollector.init(l.allocator, l.store, l.strings);
    defer collector.deinit();

    // Add lambda parameters as locals (these are NOT captures)
    var param_iter = param_names.keyIterator();
    while (param_iter.next()) |name| {
        collector.addLocal(name.*) catch continue;
    }

    // Scan the body for variable references
    collector.scanStatement(body_idx) catch {};

    // Find which referenced variables exist in outer scope (these are captures)
    const free_vars = collector.getFreeVariables();
    defer l.allocator.free(free_vars);

    for (free_vars) |var_name| {
        // Check if this variable exists in outer scope
        if (l.scopes.get(var_name)) |outer_value| {
            captures.put(var_name, outer_value) catch continue;
            debug.print(.ir, "Lambda {s}: capturing variable '{s}'", .{ lambda_name, var_name });
        }
    }

    const has_captures = captures.count() > 0;

    // ========================================================================
    // If captures exist, add implicit _closure_env parameter
    // ========================================================================
    if (has_captures) {
        // Insert _closure_env as first parameter
        var new_params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        new_params.append(l.allocator, .{
            .name = "_closure_env",
            .ty = .{ .map = undefined }, // Map type
            .is_ref = false,
        }) catch return LowerError.OutOfMemory;

        // Copy existing params
        for (params.items) |param| {
            new_params.append(l.allocator, param) catch return LowerError.OutOfMemory;
        }

        // Replace params list
        params.deinit(l.allocator);
        params = new_params;
    }

    // Create function type (return type will be inferred as void for now)
    const func_type = l.allocator.create(ir.FunctionType) catch return LowerError.OutOfMemory;
    func_type.* = .{
        .params = params.toOwnedSlice(l.allocator) catch return LowerError.OutOfMemory,
        .return_type = .void,
        .is_variadic = false,
    };

    // Create lambda function
    const lambda_func = ir.Function.init(l.allocator, lambda_name, func_type.*) catch return LowerError.OutOfMemory;

    // Save current context
    const saved_func = l.current_func;
    const saved_block = l.current_block;
    const saved_captures = l.current_captures;
    const saved_closure_env = l.closure_env_value;

    // Switch to lambda context
    l.current_func = lambda_func;
    l.current_block = lambda_func.entry;

    // Set capture info for identifier lowering to use
    if (has_captures) {
        l.current_captures = captures;
    } else {
        l.current_captures = null;
    }

    // Push new scope for lambda parameters/locals
    try l.scopes.push();

    // Add parameters as local variables
    var env_value: ?ir.Value = null;
    for (func_type.params, 0..) |param, i| {
        const ty_ptr = l.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
        ty_ptr.* = param.ty;
        l.allocated_types.append(l.allocator, ty_ptr) catch return LowerError.OutOfMemory;

        const alloca_result = lambda_func.newValue(.{ .ptr = ty_ptr });
        try l.emit(.{
            .alloca = .{
                .ty = param.ty,
                .name = param.name,
                .result = alloca_result,
            },
        });
        l.scopes.put(param.name, alloca_result) catch return LowerError.OutOfMemory;

        // Track the closure environment parameter
        if (has_captures and i == 0 and std.mem.eql(u8, param.name, "_closure_env")) {
            env_value = alloca_result;
        }
    }

    // Set closure env value for identifier lowering
    l.closure_env_value = env_value;

    // Lower lambda body
    try l.lowerStatement(body_idx);

    // Add implicit return if not terminated
    if (!l.current_block.?.isTerminated()) {
        try l.emit(.{ .return_ = null });
    }

    // Add lambda function to module
    l.module.addFunction(lambda_func) catch return LowerError.OutOfMemory;

    // Restore context
    l.scopes.pop();
    l.current_func = saved_func;
    l.current_block = saved_block;
    l.current_captures = saved_captures;
    l.closure_env_value = saved_closure_env;

    // ========================================================================
    // Return value: closure with env if captures, function pointer otherwise
    // ========================================================================
    if (has_captures) {
        // Create a map for captured values in the outer function
        const env_map = outer_func.newValue(.{ .map = undefined });
        try l.emit(.{
            .map_new = .{
                .flags = 0x03, // case_sensitive + preserve_spaces
                .result = env_map,
                .loc = null,
            },
        });

        // Store each captured variable in the map
        var cap_iter = captures.iterator();
        while (cap_iter.next()) |entry| {
            const var_name = entry.key_ptr.*;
            const outer_ptr = entry.value_ptr.*;

            // Load the value from the outer scope pointer
            const loaded_val = outer_func.newValue(.void);
            try l.emit(.{
                .load = .{
                    .ptr = outer_ptr,
                    .result = loaded_val,
                },
            });

            // Create string constant for the key
            const key_const = outer_func.newValue(.string);
            try l.emit(.{
                .const_string = .{
                    .value = var_name,
                    .result = key_const,
                },
            });

            // Store in map
            try l.emit(.{
                .map_set = .{
                    .map = env_map,
                    .key = key_const,
                    .value = loaded_val,
                    .loc = null,
                },
            });
        }

        // Create closure value (struct with routine info and env)
        // For now, we'll use a special "make_closure" instruction
        const closure_result = outer_func.newValue(.void);
        try l.emit(.{
            .make_closure = .{
                .func_name = lambda_name,
                .env = env_map,
                .result = closure_result,
            },
        });

        return closure_result;
    } else {
        // No captures - still emit make_closure with null env for consistent calling convention
        const null_env = outer_func.newValue(.void);

        const closure_result = outer_func.newValue(.void);
        try l.emit(.{
            .make_closure = .{
                .func_name = lambda_name,
                .env = null_env,
                .result = closure_result,
            },
        });

        return closure_result;
    }
}

// ============================================================================
// Type Lowering
// ============================================================================

/// Lower a type index to an IR type
pub fn lowerTypeIdx(l: *Lowerer, type_idx: TypeIdx) LowerError!ir.Type {
    if (type_idx == .null) {
        return .void;
    }

    const tag = l.store.typeTag(type_idx);
    const data = l.store.typeData(type_idx);

    return switch (tag) {
        .void => .void,
        .bool => .bool,
        .i8 => .i8,
        .i16 => .i16,
        .i32 => .i32,
        .i64 => .i64,
        .u8 => .u8,
        .u16 => .u16,
        .u32 => .u32,
        .u64 => .u64,
        .isize => .isize,
        .usize => .usize,
        .f32 => .f32,
        .f64 => .f64,
        .string => .string,
        .decimal => .{ .implied_decimal = .{ .precision = @intCast(data.a), .scale = @intCast(data.b) } },
        .array => blk: {
            const elem_type_idx: TypeIdx = @enumFromInt(data.a);
            const length = data.b;
            const elem_type = try lowerTypeIdx(l, elem_type_idx);
            const elem_ptr = try l.allocator.create(ir.Type);
            elem_ptr.* = elem_type;
            try l.allocated_types.append(l.allocator, elem_ptr);
            break :blk .{ .array = .{ .element = elem_ptr, .length = length } };
        },
        .slice => blk: {
            const elem_type_idx: TypeIdx = @enumFromInt(data.a);
            const elem_type = try lowerTypeIdx(l, elem_type_idx);
            const elem_ptr = try l.allocator.create(ir.Type);
            elem_ptr.* = elem_type;
            try l.allocated_types.append(l.allocator, elem_ptr);
            break :blk .{ .slice = elem_ptr };
        },
        .optional => blk: {
            const inner_type_idx: TypeIdx = @enumFromInt(data.a);
            const inner_type = try lowerTypeIdx(l, inner_type_idx);
            const inner_ptr = try l.allocator.create(ir.Type);
            inner_ptr.* = inner_type;
            try l.allocated_types.append(l.allocator, inner_ptr);
            break :blk .{ .optional = inner_ptr };
        },
        .weak => blk: {
            const inner_type_idx: TypeIdx = @enumFromInt(data.a);
            const inner_type = try lowerTypeIdx(l, inner_type_idx);
            const inner_ptr = try l.allocator.create(ir.Type);
            inner_ptr.* = inner_type;
            try l.allocated_types.append(l.allocator, inner_ptr);
            break :blk .{ .weak = inner_ptr };
        },
        .pointer => blk: {
            const pointee_type_idx: TypeIdx = @enumFromInt(data.a);
            const pointee_type = try lowerTypeIdx(l, pointee_type_idx);
            const pointee_ptr = try l.allocator.create(ir.Type);
            pointee_ptr.* = pointee_type;
            try l.allocated_types.append(l.allocator, pointee_ptr);
            break :blk .{ .ptr = pointee_ptr };
        },
        .named => blk: {
            const name_id: StringId = @enumFromInt(data.a);
            const name = l.strings.get(name_id);
            if (name.len == 0) {
                l.setErrorContext(
                    LowerError.UndefinedType,
                    "Empty type reference",
                    .{},
                    l.current_loc,
                    "type reference has no name",
                    .{},
                );
                return LowerError.UndefinedType;
            }
            if (l.struct_types.get(name)) |struct_type| {
                break :blk .{ .@"struct" = struct_type };
            }
            // Check if it's an enum type - enums are represented as i64 at runtime
            if (l.enum_types.contains(name)) {
                break :blk .i64;
            }
            l.setErrorContext(
                LowerError.UndefinedType,
                "Unknown type '{s}'",
                .{name},
                l.current_loc,
                "type '{s}' is not a defined struct or enum",
                .{name},
            );
            return LowerError.UndefinedType;
        },
        // Generic type parameter - look up in substitution map
        .type_param => blk: {
            const name_id: StringId = @enumFromInt(data.a);
            const name = l.strings.get(name_id);
            if (name.len == 0) break :blk .void;
            // Look up the type parameter in current substitutions
            if (l.type_param_substitutions.get(name)) |concrete_type| {
                break :blk concrete_type;
            }
            // Type parameter not substituted - this is an error during instantiation
            break :blk .void;
        },
        // Generic type instantiation - instantiate with concrete type arguments
        .generic_instance => blk: {
            const base_type_idx = TypeIdx.fromInt(data.a);
            const args_start = data.b;

            // Get base type info
            const base_tag = l.store.typeTag(base_type_idx);
            if (base_tag != .named) break :blk .void;

            const base_data = l.store.typeData(base_type_idx);
            const base_name_id: StringId = @enumFromInt(base_data.a);
            const base_name = l.strings.get(base_name_id);
            if (base_name.len == 0) break :blk .void;

            // Get type arguments from extra_data
            const arg_count = l.store.extra_data.items[args_start];
            var type_args: std.ArrayListUnmanaged(ir.Type) = .{};
            defer type_args.deinit(l.allocator);

            for (0..arg_count) |i| {
                const arg_type_idx = TypeIdx.fromInt(l.store.extra_data.items[args_start + 1 + i]);
                const arg_type = try lowerTypeIdx(l, arg_type_idx);
                try type_args.append(l.allocator, arg_type);
            }

            // Try to instantiate the generic type
            const instantiated = try l.instantiateGenericStruct(base_name, type_args.items);
            if (instantiated) |struct_type| {
                break :blk .{ .@"struct" = struct_type };
            }

            // Fallback: check if it's already a non-generic struct
            if (l.struct_types.get(base_name)) |struct_type| {
                break :blk .{ .@"struct" = struct_type };
            }

            break :blk .void;
        },
        // Map type: Map<K, V>
        .map => blk: {
            const key_type_idx: TypeIdx = @enumFromInt(data.a);
            const value_type_idx: TypeIdx = @enumFromInt(data.b);

            const key_type = try lowerTypeIdx(l, key_type_idx);
            const value_type = try lowerTypeIdx(l, value_type_idx);

            const map_type = try l.allocator.create(ir.MapType);
            const key_ptr = try l.allocator.create(ir.Type);
            const value_ptr = try l.allocator.create(ir.Type);

            key_ptr.* = key_type;
            value_ptr.* = value_type;
            try l.allocated_types.append(l.allocator, key_ptr);
            try l.allocated_types.append(l.allocator, value_ptr);

            map_type.* = .{
                .key_type = key_ptr,
                .value_type = value_ptr,
            };

            break :blk .{ .map = map_type };
        },
        // List type: List<T>
        .list => blk: {
            const elem_type_idx: TypeIdx = @enumFromInt(data.a);

            const elem_type = try lowerTypeIdx(l, elem_type_idx);

            const list_type = try l.allocator.create(ir.ListType);
            const elem_ptr = try l.allocator.create(ir.Type);

            elem_ptr.* = elem_type;
            try l.allocated_types.append(l.allocator, elem_ptr);

            list_type.* = .{
                .element_type = elem_ptr,
            };

            break :blk .{ .list = list_type };
        },
        .function => .void, // Function types are handled separately
        .@"union" => .void, // Union types are handled via union_def statement
        // These types don't have IR equivalents yet
        .trait_object => blk: {
            const trait_name_id: StringId = @enumFromInt(data.a);
            const trait_name = l.strings.get(trait_name_id);
            break :blk .{ .trait_object = .{ .trait_name = trait_name } };
        },
        // Associated type reference: Self.Item or T.Item
        .associated_type => blk: {
            const base_type_idx: TypeIdx = @enumFromInt(data.a);
            const assoc_name_id: StringId = @enumFromInt(data.b);
            const assoc_name = l.strings.get(assoc_name_id);

            // Get the base type to determine what struct/type it refers to
            const base_tag = l.store.typeTag(base_type_idx);
            const base_data = l.store.typeData(base_type_idx);

            // If base is a type parameter named "Self" and we're in an impl block
            if (base_tag == .type_param) {
                const param_name_id: StringId = @enumFromInt(base_data.a);
                const param_name = l.strings.get(param_name_id);

                if (std.mem.eql(u8, param_name, "Self")) {
                    // Look up in current impl context
                    if (l.current_impl_key) |impl_key| {
                        if (l.impl_assoc_types.get(impl_key)) |bindings| {
                            for (bindings) |binding| {
                                if (std.mem.eql(u8, binding.name, assoc_name)) {
                                    // Resolve the concrete type
                                    break :blk try lowerTypeIdx(l, binding.concrete_type_idx);
                                }
                            }
                        }
                    }
                }

                // Type parameter substitution: look up what T is substituted with
                if (l.type_param_substitutions.get(param_name)) |_| {
                    // T is substituted, but we need the associated type from its impl
                    // For now, fall through to void - full generic associated type support TBD
                }
            }

            // If base is a named type (concrete struct), try to find its impl
            if (base_tag == .named) {
                const type_name_id: StringId = @enumFromInt(base_data.a);
                const type_name = l.strings.get(type_name_id);

                // Search all impls for this type to find the associated type binding
                var impl_it = l.impl_assoc_types.iterator();
                while (impl_it.next()) |entry| {
                    if (std.mem.eql(u8, entry.key_ptr.type_name, type_name)) {
                        for (entry.value_ptr.*) |binding| {
                            if (std.mem.eql(u8, binding.name, assoc_name)) {
                                break :blk try lowerTypeIdx(l, binding.concrete_type_idx);
                            }
                        }
                    }
                }
            }

            // Associated type not found - return void as fallback
            break :blk .void;
        },
        .tuple, .error_union, .inferred, .any, .never => .void,
    };
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a type is a map type (directly or through a pointer)
pub fn isMapType(ty: ir.Type) bool {
    return switch (ty) {
        .map => true,
        .ptr => |p| p.* == .map,
        else => false,
    };
}

/// Get the value type from a map type (returns .string as fallback)
pub fn getMapValueType(ty: ir.Type) ir.Type {
    return switch (ty) {
        .map => |m| m.value_type.*,
        .ptr => |p| switch (p.*) {
            .map => |m| m.value_type.*,
            else => .string,
        },
        else => .string,
    };
}

/// Check if a type is a list type (directly or through a pointer)
pub fn isListType(ty: ir.Type) bool {
    return switch (ty) {
        .list => true,
        .ptr => |p| p.* == .list,
        else => false,
    };
}

/// Determine return type of builtin functions using generated spec-based lookup.
/// The FunctionReturnTypes map is auto-generated from spec/natives.toml.
pub fn getBuiltinReturnType(name: []const u8, args: []const ir.Value) ir.Type {
    // O(1) lookup in the generated static map from spec/natives.toml
    // This includes all native functions AND opcode builtins (len, size, trim, atrim)
    if (native_types.getReturnType(name)) |ty| {
        return ty;
    }

    // Default: return type of first argument or void
    if (args.len > 0) {
        return args[0].ty;
    }
    return .void;
}
