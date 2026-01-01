//! Struct Serialization Utilities
//!
//! Provides unified structure detection and serialization info for I/O operations.
//! This module centralizes the logic for detecting struct-typed variables
//! and provides the information needed for serialization/deserialization.
//!
//! Previously this detection logic was scattered across lowerAssignment(),
//! lowerIoWrite(), lowerIoStore(), and lowerStructBufferOrExpression().
//! Centralizing it here prevents bugs where one code path misses struct handling.
//!
//! Usage in lower.zig:
//!   const struct_info = StructHelper.detectStructType(&self.scopes, name);
//!   if (struct_info) |info| {
//!       // Emit load_struct_buf or store_struct_buf
//!   }

const std = @import("std");
const ir = @import("ir.zig");
const ast = @import("../ast/mod.zig");
const scope_stack = @import("scope_stack.zig");

const ScopeStack = scope_stack.ScopeStack;
const ExprIdx = ast.ExprIdx;
const StringInterner = ast.StringInterner;
const NodeStore = ast.NodeStore;

/// Result of struct type detection
pub const StructTypeInfo = struct {
    /// The struct type definition
    struct_type: *const ir.StructType,
    /// The base variable name (e.g., "cust", "app")
    base_name: []const u8,
    /// Total size of the struct in bytes
    size: u32,

    /// Get the struct name for IR instructions
    pub fn structName(self: StructTypeInfo) []const u8 {
        return self.struct_type.name;
    }
};

/// Struct detection and serialization helper
pub const StructHelper = struct {
    /// Detect if a named variable is a struct type
    /// Returns StructTypeInfo if the variable is a pointer to a struct, null otherwise
    pub fn detectStructType(
        scopes: *const ScopeStack,
        name: []const u8,
    ) ?StructTypeInfo {
        const var_val = scopes.get(name) orelse return null;

        // Must be a pointer type
        if (var_val.ty != .ptr) return null;

        const pointee_type = var_val.ty.ptr.*;

        // Must point to a struct
        if (pointee_type != .@"struct") return null;

        const struct_type = pointee_type.@"struct";
        return StructTypeInfo{
            .struct_type = struct_type,
            .base_name = name,
            .size = struct_type.size,
        };
    }

    /// Detect struct type from an expression
    /// Works for identifier expressions only
    pub fn detectStructTypeFromExpr(
        scopes: *const ScopeStack,
        store: *const NodeStore,
        strings: *const StringInterner,
        expr_idx: ExprIdx,
    ) ?StructTypeInfo {
        const expr_tag = store.exprTag(expr_idx);
        if (expr_tag != .identifier) return null;

        const expr_data = store.exprData(expr_idx);
        const name_id = expr_data.getName();
        const name = strings.get(name_id);

        return detectStructType(scopes, name);
    }

    /// Create the IR instruction for serializing a struct to a buffer
    /// (load_struct_buf: collects all fields into contiguous buffer)
    pub fn makeLoadStructBufInst(info: StructTypeInfo, result: ir.Value) ir.Instruction {
        return .{
            .load_struct_buf = .{
                .base_name = info.base_name,
                .struct_name = info.structName(),
                .result = result,
            },
        };
    }

    /// Create the IR instruction for deserializing a buffer to struct fields
    /// (store_struct_buf: distributes buffer to individual field variables)
    pub fn makeStoreStructBufInst(info: StructTypeInfo, value: ir.Value) ir.Instruction {
        return .{
            .store_struct_buf = .{
                .base_name = info.base_name,
                .struct_name = info.structName(),
                .value = value,
            },
        };
    }

    /// Create a result value for serialization (sized to struct)
    pub fn makeResultValue(func: *ir.Function, info: StructTypeInfo) ir.Value {
        return func.newValue(.{ .string_fixed = info.size });
    }
};

test "struct type detection - basic" {
    // This would require setting up a full lowerer context
    // For now, just ensure it compiles
}
