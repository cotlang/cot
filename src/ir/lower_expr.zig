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

// ============================================================================
// Main Expression Lowering
// ============================================================================

/// Lower an expression to an IR value
pub fn lowerExpression(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.UnsupportedFeature;
    const tag = l.store.exprTag(expr_idx);
    const data = l.store.exprData(expr_idx);

    log.debug("lowerExpression: tag = {s}", .{@tagName(tag)});

    switch (tag) {
        .int_literal => return lowerIntLiteral(l, func, data),
        .float_literal => return lowerFloatLiteral(l, func, data),
        .string_literal => return lowerStringLiteral(l, func, data),
        .bool_literal => return lowerBoolLiteral(l, func, data),
        .null_literal => return lowerNullLiteral(l, func),
        .identifier => return lowerIdentifier(l, func, data),
        .binary => return lowerBinary(l, func, data, expr_idx),
        .unary => return lowerUnary(l, func, data, expr_idx),
        .grouping => return lowerExpression(l, @enumFromInt(data.a)),
        .call => return lowerCall(l, expr_idx),
        .method_call => return lowerMethodCall(l, expr_idx),
        .member => return lowerMember(l, func, expr_idx),
        .index => return lowerIndex(l, func, expr_idx),
        .array_init => return lowerArrayInit(l, expr_idx),
        .struct_init => return lowerStructInit(l, expr_idx),
        .generic_struct_init => return lowerGenericStructInit(l, expr_idx),
        .lambda => return lowerLambda(l, expr_idx),
        .range => {
            // Range expressions are lowered by for loop handling, not as standalone values
            // If we get here, a range was used outside of a for loop context
            return LowerError.UnsupportedFeature;
        },
        .comptime_builtin => {
            // Comptime builtins should be evaluated at compile time by the comptime evaluator
            // If we get here, a comptime builtin wasn't resolved during constant folding
            return LowerError.UnsupportedFeature;
        },
        .if_expr, .match_expr, .block_expr => {
            // Expression forms (if/match/block as expressions) not yet supported
            // These would require SSA phi nodes or temporary variables
            return LowerError.UnsupportedFeature;
        },
        .optional_member, .optional_index => {
            // Optional chaining (?. and ?[]) not yet supported
            return LowerError.UnsupportedFeature;
        },
        .is_expr => {
            // Type checking expressions not yet supported
            return LowerError.UnsupportedFeature;
        },
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
pub fn lowerIdentifier(l: *Lowerer, func: *ir.Function, data: NodeData) LowerError!ir.Value {
    const name_id = data.getName();
    const name = l.strings.get(name_id);
    if (name.len == 0) {
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "Undefined variable: (empty name)",
            .{},
            .{ .line = 0, .column = 0 },
            "lowering identifier expression",
            .{},
        );
        return LowerError.UndefinedVariable;
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
    const ptr = l.scopes.get(name) orelse
        l.global_variables.get(name) orelse blk: {
        l.setErrorContext(
            LowerError.UndefinedVariable,
            "Undefined variable: '{s}'",
            .{name},
            .{ .line = 0, .column = 0 },
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
        else => return LowerError.InvalidExpression,
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
                    else => return LowerError.TypeMismatch,
                }
            },
            else => return LowerError.TypeMismatch,
        },
        else => return LowerError.TypeMismatch,
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

    const object_idx = data.getObject();
    const index_idx = data.getIndex();

    const object_ptr = try lowerLValue(l, object_idx);
    const index_val = try lowerExpression(l, index_idx);

    // Get element type from array/slice type
    const elem_type = switch (object_ptr.ty) {
        .ptr => |p| switch (p.*) {
            .array => |a| a.element.*,
            .slice => |s| s.*,
            else => return LowerError.TypeMismatch,
        },
        else => return LowerError.TypeMismatch,
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
    _ = expr_idx; // Used for error location, currently unused with auto-coercion
    const lhs_idx = data.getLhs();
    const rhs_idx = data.getRhs();
    const op = data.getBinaryOp();

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
                .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result } }
            else
                .{ .iadd = .{ .lhs = lhs, .rhs = rhs, .result = result } };
        },
        .sub => .{ .isub = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .mul => .{ .imul = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .div => .{ .sdiv = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .mod => .{ .srem = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .eq => .{ .icmp = .{ .cond = .eq, .lhs = lhs, .rhs = rhs, .result = result } },
        .ne => .{ .icmp = .{ .cond = .ne, .lhs = lhs, .rhs = rhs, .result = result } },
        .lt => .{ .icmp = .{ .cond = .slt, .lhs = lhs, .rhs = rhs, .result = result } },
        .le => .{ .icmp = .{ .cond = .sle, .lhs = lhs, .rhs = rhs, .result = result } },
        .gt => .{ .icmp = .{ .cond = .sgt, .lhs = lhs, .rhs = rhs, .result = result } },
        .ge => .{ .icmp = .{ .cond = .sge, .lhs = lhs, .rhs = rhs, .result = result } },
        .@"and" => .{ .log_and = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .@"or" => .{ .log_or = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .bit_and => blk: {
            // In DBL, & is overloaded: bitwise AND for integers, concatenation for strings
            const lhs_is_string = lhs.isString();
            const rhs_is_string = rhs.isString();
            const is_concat = lhs_is_string or rhs_is_string;
            break :blk if (is_concat)
                .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result } }
            else
                .{ .band = .{ .lhs = lhs, .rhs = rhs, .result = result } };
        },
        .bit_or => .{ .bor = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .bit_xor => .{ .bxor = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .shl => .{ .ishl = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        .shr => .{ .sshr = .{ .lhs = lhs, .rhs = rhs, .result = result } },
        // Rounding operations: value # places (trunc) or value ## places (round)
        .round => .{ .round = .{ .value = lhs, .places = rhs, .result = result } },
        .trunc => .{ .trunc = .{ .value = lhs, .places = rhs, .result = result } },
        .range, .range_inclusive => return LowerError.UnsupportedFeature,
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
            const pointee_ty = if (operand.ty == .ptr) operand.ty.ptr.* else operand.ty;
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
                return LowerError.UndefinedVariable;
            }
        }
    }

    // Not an enum access - proceed with regular member access
    const ptr = try lowerMemberPtr(l, expr_idx);
    const result = func.newValue(ptr.ty);
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
        const result = func.newValue(.string); // Map values are strings by default
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

    // Fallback to array/slice indexing via pointer
    const ptr = try lowerIndexPtr(l, expr_idx);
    const result = func.newValue(ptr.ty);
    try l.emit(.{
        .load = .{
            .ptr = ptr,
            .result = result,
        },
    });
    return result;
}

// ============================================================================
// Function and Method Calls
// ============================================================================

/// Lower a function call expression
pub fn lowerCall(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

    const callee_idx = data.getCallee();
    const args_count = data.b >> 16;
    const args_start = data.b & 0xFFFF;

    // Get callee name
    const callee_tag = l.store.exprTag(callee_idx);

    // Check for namespace-qualified function calls (e.g., std.math.abs, std.io.println)
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

                const result = func.newValue(.{ .map = undefined });
                try l.emit(.{
                    .map_new = .{
                        .flags = flags,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
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
                    },
                });
                return result;
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
                const result = func.newValue(.string);
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
                        return_type = method.return_type;
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
        debug.print(.ir, "Checking for struct method: field={s}, object_ty={any}", .{ field_name, object_val.ty });

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

            // Look up inherent impl (impl TypeName { ... })
            // For inherent impls, trait_name == type_name
            if (l.lookupTraitMethod(type_name, type_name, field_name)) |_| {
                // Found the method! Lower arguments and emit call with object as first arg
                var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer method_args.deinit(l.allocator);

                // Get struct pointer using lowerLValue to avoid extra loads
                // This gives us the direct pointer to the struct fields
                const struct_lval = try lowerLValue(l, object_idx);

                // Determine the struct pointer to use for field_ptr
                // If lval is **struct (pointer to pointer to struct), load once to get *struct
                var struct_ptr = struct_lval;
                if (struct_lval.ty == .ptr) {
                    const pointee = struct_lval.ty.ptr.*;
                    if (pointee == .ptr) {
                        const inner = pointee.ptr.*;
                        if (inner == .@"struct") {
                            // Load once to get *struct
                            const loaded = func.newValue(.{ .ptr = struct_lval.ty.ptr });
                            try l.emit(.{
                                .load = .{
                                    .ptr = struct_lval,
                                    .result = loaded,
                                },
                            });
                            struct_ptr = loaded;
                        }
                    }
                }

                // Emit field_ptr + load for each field to pass as individual arguments
                for (struct_type.fields, 0..) |field, idx| {
                    const field_ty_ptr = try l.allocator.create(ir.Type);
                    field_ty_ptr.* = field.ty;
                    try l.allocated_types.append(l.allocator, field_ty_ptr);

                    const field_ptr_val = func.newValue(.{ .ptr = field_ty_ptr });
                    try l.emit(.{
                        .field_ptr = .{
                            .struct_ptr = struct_ptr,
                            .field_index = @intCast(idx),
                            .result = field_ptr_val,
                        },
                    });

                    const field_val = func.newValue(field.ty);
                    try l.emit(.{
                        .load = .{
                            .ptr = field_ptr_val,
                            .result = field_val,
                        },
                    });

                    try method_args.append(l.allocator, field_val);
                }

                // Then the explicit arguments
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[args_start + i]);
                    const arg_val = try lowerExpression(l, arg_idx);
                    try method_args.append(l.allocator, arg_val);
                }

                const args_slice = try method_args.toOwnedSlice(l.allocator);

                // Use qualified method name: TypeName.method_name
                var method_name_buf: [256]u8 = undefined;
                const qualified_method_name = std.fmt.bufPrint(&method_name_buf, "{s}.{s}", .{ type_name, field_name }) catch {
                    return LowerError.OutOfMemory;
                };
                const method_name = l.allocator.dupe(u8, qualified_method_name) catch {
                    return LowerError.OutOfMemory;
                };

                // Determine return type from the method's signature
                // For now, use a generic approach - could be improved with proper type inference
                const result_type = getBuiltinReturnType(method_name, args_slice);
                const result = func.newValue(result_type);

                debug.print(.ir, "Dispatching method call: {s}.{s} with {d} args", .{ type_name, field_name, args_slice.len });

                try l.emit(.{
                    .call = .{
                        .callee = method_name,
                        .args = args_slice,
                        .result = result,
                    },
                });
                return result;
            }
        }
    }

    // Dupe the callee name so Block.deinit can consistently free it
    const callee_name = if (callee_tag == .identifier) blk: {
        const callee_data = l.store.exprData(callee_idx);
        const name_id = callee_data.getName();
        const name = l.strings.get(name_id);
        break :blk l.allocator.dupe(u8, name) catch return LowerError.OutOfMemory;
    } else l.allocator.dupe(u8, "") catch return LowerError.OutOfMemory;

    // Check if this is a database I/O call that needs structure serialization
    const is_db_write = std.mem.eql(u8, callee_name, "db_store") or std.mem.eql(u8, callee_name, "db_write");

    // Lower arguments
    var args: std.ArrayListUnmanaged(ir.Value) = .{};
    defer args.deinit(l.allocator);

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

    // Determine result type based on builtin function
    const result_type = getBuiltinReturnType(callee_name, args_slice);
    const result = func.newValue(result_type);

    try l.emit(.{
        .call = .{
            .callee = callee_name,
            .args = args_slice,
            .result = result,
        },
    });

    return result;
}

/// Lower a method call expression
pub fn lowerMethodCall(l: *Lowerer, expr_idx: ExprIdx) LowerError!ir.Value {
    const func = l.current_func orelse return LowerError.OutOfMemory;
    const data = l.store.exprData(expr_idx);

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
            const result = func.newValue(.string); // Map values are strings by default
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
                    return_type = method.return_type;
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

    // Fallback to regular method call
    var args: std.ArrayListUnmanaged(ir.Value) = .{};
    defer args.deinit(l.allocator);
    try args.append(l.allocator, object_val);

    for (0..args_count) |i| {
        const arg_idx: ExprIdx = @enumFromInt(l.store.extra_data.items[extra_start + 2 + i]);
        const arg_val = try lowerExpression(l, arg_idx);
        try args.append(l.allocator, arg_val);
    }

    const args_slice = try args.toOwnedSlice(l.allocator);
    // NOTE: args_slice is freed by Block.deinit when the call instruction is cleaned up

    // Dupe the method name so Block.deinit can consistently free it
    const method_name = l.allocator.dupe(u8, method_name_borrowed) catch return LowerError.OutOfMemory;

    const result_type = getBuiltinReturnType(method_name, args_slice);
    const result = func.newValue(result_type);

    try l.emit(.{
        .call = .{
            .callee = method_name,
            .args = args_slice,
            .result = result,
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
    if (type_name.len == 0) {
        std.debug.print("  Error: Empty type name in struct literal\n", .{});
        return LowerError.UndefinedType;
    }

    // Look up struct type
    const struct_type = l.struct_types.get(type_name) orelse {
        std.debug.print("  Error: Unknown struct type '{s}'\n", .{type_name});
        std.debug.print("  Available types: ", .{});
        var count: usize = 0;
        var it = l.struct_types.keyIterator();
        while (it.next()) |key| {
            if (count > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{key.*});
            count += 1;
            if (count >= 10) {
                std.debug.print("...", .{});
                break;
            }
        }
        std.debug.print("\n", .{});
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
    const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = struct_ir_type,
            .name = type_name,
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
        // Skip unknown fields silently (could add warning)
    }

    // Return pointer to struct
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

    // Instantiate the generic struct
    const struct_type = try l.instantiateGenericStruct(type_name, type_args.items) orelse {
        std.debug.print("  Error: Failed to instantiate generic struct '{s}'\n", .{type_name});
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
    const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
    try l.emit(.{
        .alloca = .{
            .ty = struct_ir_type,
            .name = type_name,
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
        .decimal => .{ .decimal = .{ .precision = @intCast(data.a), .scale = @intCast(data.b) } },
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
                std.debug.print("  Error: Empty named type reference\n", .{});
                return LowerError.UndefinedType;
            }
            if (l.struct_types.get(name)) |struct_type| {
                break :blk .{ .@"struct" = struct_type };
            }
            std.debug.print("  Error: Unknown type '{s}' (not a defined struct)\n", .{name});
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
        .function => .void, // Function types are handled separately
        .@"union" => .void, // Union types are handled via union_def statement
        // These types don't have IR equivalents yet
        .trait_object => blk: {
            const trait_name_id: StringId = @enumFromInt(data.a);
            const trait_name = l.strings.get(trait_name_id);
            break :blk .{ .trait_object = .{ .trait_name = trait_name } };
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

/// Determine return type of builtin functions using generated spec-based lookup.
/// The FunctionReturnTypes map is auto-generated from spec/natives.toml.
pub fn getBuiltinReturnType(name: []const u8, args: []const ir.Value) ir.Type {
    // O(1) lookup in the generated static map from spec/natives.toml
    if (native_types.getReturnType(name)) |ty| {
        return ty;
    }

    // Handle TUI functions (t_*) as void
    if (std.mem.startsWith(u8, name, "t_")) {
        return .void;
    }

    // Default: return type of first argument or void
    if (args.len > 0) {
        return args[0].ty;
    }
    return .void;
}
