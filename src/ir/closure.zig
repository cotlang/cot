//! Closure Support
//!
//! Provides functionality for detecting captured variables in lambdas
//! and generating closure environments.
//!
//! A closure captures variables from its enclosing scope. When a lambda
//! references a variable that is not a parameter or local, that variable
//! is "captured" and must be stored in a closure environment.
//!
//! Example:
//! ```cot
//! fn makeCounter() fn() i64 {
//!     var count: i64 = 0
//!     return || {
//!         count = count + 1  // 'count' is captured
//!         return count
//!     }
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;

const ast = @import("../ast/mod.zig");
const NodeStore = ast.NodeStore;
const ExprIdx = ast.ExprIdx;
const StmtIdx = ast.StmtIdx;
const TypeIdx = ast.TypeIdx;
const ExpressionTag = ast.ExpressionTag;
const StatementTag = ast.StatementTag;
const StringId = @import("../base/mod.zig").StringId;
const StringInterner = ast.StringInterner;

const ir = @import("ir.zig");

/// Information about a captured variable
pub const CapturedVar = struct {
    name: []const u8,
    /// The IR value in the outer scope (pointer to the variable)
    outer_value: ir.Value,
};

/// Collects free variables (captures) from a lambda body
pub const FreeVarCollector = struct {
    allocator: Allocator,
    store: *const NodeStore,
    strings: *const StringInterner,

    /// Variables defined in the lambda scope (parameters + locals)
    lambda_locals: std.StringHashMap(void),

    /// Variables referenced but not defined locally (potential captures)
    referenced_vars: std.StringHashMap(void),

    /// Initialize the collector
    pub fn init(
        allocator: Allocator,
        store: *const NodeStore,
        strings: *const StringInterner,
    ) FreeVarCollector {
        return .{
            .allocator = allocator,
            .store = store,
            .strings = strings,
            .lambda_locals = std.StringHashMap(void).init(allocator),
            .referenced_vars = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *FreeVarCollector) void {
        self.lambda_locals.deinit();
        self.referenced_vars.deinit();
    }

    /// Add a parameter or local variable name
    pub fn addLocal(self: *FreeVarCollector, name: []const u8) !void {
        try self.lambda_locals.put(name, {});
    }

    /// Scan a statement for variable references
    pub fn scanStatement(self: *FreeVarCollector, stmt_idx: StmtIdx) !void {
        if (stmt_idx == .null) return;

        // Debug validation: check if stmt_idx is valid
        const stmt_count = self.store.stmt_tags.items.len;
        if (@intFromEnum(stmt_idx) >= stmt_count) {
            debug.print(.ir, "FreeVarCollector: Invalid stmt_idx {d} (max={d})", .{ @intFromEnum(stmt_idx), stmt_count });
            return;
        }

        const tag = self.store.stmtTag(stmt_idx);
        const data = self.store.stmtData(stmt_idx);

        switch (tag) {
            .expression => {
                const expr_idx: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(expr_idx);
            },
            .let_decl, .const_decl => {
                // Add the declared variable as a local
                const name_id = data.getName();
                const name = self.strings.get(name_id);
                if (name.len > 0) {
                    try self.addLocal(name);
                }
                // Scan the initializer
                const init_expr: ExprIdx = @enumFromInt(data.b);
                if (init_expr != .null) {
                    try self.scanExpression(init_expr);
                }
            },
            .assignment => {
                const target: ExprIdx = @enumFromInt(data.a);
                const value: ExprIdx = @enumFromInt(data.b);
                try self.scanExpression(target);
                try self.scanExpression(value);
            },
            .if_stmt => {
                const condition: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(condition);
                // Scan then/else blocks via extra_data
                const then_body: StmtIdx = @enumFromInt(data.b >> 16);
                const extra_idx = data.b & 0xFFFF;
                const else_body: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
                try self.scanStatement(then_body);
                try self.scanStatement(else_body);
            },
            .while_stmt => {
                const condition: ExprIdx = @enumFromInt(data.a);
                const body: StmtIdx = @enumFromInt(data.b);
                try self.scanExpression(condition);
                try self.scanStatement(body);
            },
            .for_stmt => {
                // Loop variable is a new local
                const binding_id: StringId = @enumFromInt(data.a);
                const binding_name = self.strings.get(binding_id);
                if (binding_name.len > 0) {
                    try self.addLocal(binding_name);
                }
                // Scan iterable and body
                const extra_start = data.b;
                const iterable: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_start]);
                const body: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1]);
                try self.scanExpression(iterable);
                try self.scanStatement(body);
            },
            .loop_stmt => {
                const body: StmtIdx = @enumFromInt(data.a);
                try self.scanStatement(body);
            },
            .block => {
                const span = data.getSpan();
                const start_idx = @intFromEnum(span.start);
                for (0..span.len) |i| {
                    const child_stmt: StmtIdx = @enumFromInt(self.store.extra_data.items[start_idx + i]);
                    try self.scanStatement(child_stmt);
                }
            },
            .return_stmt => {
                const value: ExprIdx = @enumFromInt(data.a);
                if (value != .null) {
                    try self.scanExpression(value);
                }
            },
            .defer_stmt => {
                const body: StmtIdx = @enumFromInt(data.a);
                try self.scanStatement(body);
            },
            .try_stmt => {
                // Scan try body, catch body, finally body
                const try_body: StmtIdx = @enumFromInt(data.a);
                try self.scanStatement(try_body);
                // Additional bodies in extra_data
            },
            else => {
                // Other statement types - skip for now
            },
        }
    }

    /// Scan an expression for variable references
    pub fn scanExpression(self: *FreeVarCollector, expr_idx: ExprIdx) !void {
        if (expr_idx == .null) return;

        // Debug validation: check if expr_idx is valid
        const expr_count = self.store.expr_tags.items.len;
        if (@intFromEnum(expr_idx) >= expr_count) {
            debug.print(.ir, "FreeVarCollector: Invalid expr_idx {d} (max={d})", .{ @intFromEnum(expr_idx), expr_count });
            return;
        }

        const tag = self.store.exprTag(expr_idx);
        const data = self.store.exprData(expr_idx);

        switch (tag) {
            .identifier => {
                const name_id = data.getName();
                const name = self.strings.get(name_id);
                if (name.len > 0) {
                    // Record this variable reference
                    try self.referenced_vars.put(name, {});
                }
            },
            .binary => {
                const lhs: ExprIdx = @enumFromInt(data.a);
                const rhs: ExprIdx = data.getRhs();
                try self.scanExpression(lhs);
                try self.scanExpression(rhs);
            },
            .unary => {
                const operand: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(operand);
            },
            .grouping => {
                const inner: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(inner);
            },
            .call => {
                // Scan callee and arguments
                const callee: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(callee);
                // Arguments packed in data.b: (count << 16) | start
                const args_count = data.b >> 16;
                const args_start = data.b & 0xFFFF;
                for (0..args_count) |i| {
                    const arg: ExprIdx = @enumFromInt(self.store.extra_data.items[args_start + i]);
                    try self.scanExpression(arg);
                }
            },
            .method_call => {
                const object: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(object);
                // Arguments in extra_data
                const extra_start = data.b;
                const args_count = self.store.extra_data.items[extra_start + 1];
                for (0..args_count) |i| {
                    const arg: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2 + i]);
                    try self.scanExpression(arg);
                }
            },
            .member => {
                const object: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(object);
            },
            .index => {
                const object: ExprIdx = @enumFromInt(data.a);
                const index_expr: ExprIdx = @enumFromInt(data.b);
                try self.scanExpression(object);
                try self.scanExpression(index_expr);
            },
            .array_init => {
                // Elements in extra_data
                const start = data.a;
                const count = data.b;
                for (0..count) |i| {
                    const elem: ExprIdx = @enumFromInt(self.store.extra_data.items[start + i]);
                    try self.scanExpression(elem);
                }
            },
            .struct_init => {
                // Field values in extra_data
                const extra_start = data.b;
                const field_count = self.store.extra_data.items[extra_start];
                for (0..field_count) |i| {
                    // Each field has name + value
                    const value: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1 + i * 2 + 1]);
                    try self.scanExpression(value);
                }
            },
            .generic_struct_init => {
                // Generic struct init: [type_arg_count, type_args..., field_count, field_name/value pairs...]
                const extra_start = data.b;
                const type_arg_count = self.store.extra_data.items[extra_start];
                const field_count_offset = extra_start + 1 + type_arg_count;
                const field_count = self.store.extra_data.items[field_count_offset];
                for (0..field_count) |i| {
                    // Each field has name + value
                    const value: ExprIdx = @enumFromInt(self.store.extra_data.items[field_count_offset + 1 + i * 2 + 1]);
                    try self.scanExpression(value);
                }
            },
            .if_expr => {
                const condition: ExprIdx = @enumFromInt(data.a);
                try self.scanExpression(condition);
                // Then/else in extra_data
            },
            .lambda => {
                // Nested lambdas - don't scan into them (they have their own capture context)
            },
            .interp_string => {
                // Interpolated string parts
                const parts_info = self.store.getInterpStringParts(expr_idx);
                const count = parts_info.count;
                const parts_data = parts_info.data;
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const part_tag = parts_data[i * 2];
                    const part_data = parts_data[i * 2 + 1];
                    if (part_tag == 1) { // expression
                        const expr: ExprIdx = @enumFromInt(part_data);
                        try self.scanExpression(expr);
                    }
                }
            },
            else => {
                // Literals and other expressions - no variable references
            },
        }
    }

    /// Get the list of free variables (referenced but not local)
    /// These are the variables that need to be captured
    pub fn getFreeVariables(self: *const FreeVarCollector) []const []const u8 {
        var result: std.ArrayListUnmanaged([]const u8) = .empty;

        var iter = self.referenced_vars.keyIterator();
        while (iter.next()) |name| {
            // If not a local, it's a free variable
            if (!self.lambda_locals.contains(name.*)) {
                result.append(self.allocator, name.*) catch continue;
            }
        }

        return result.toOwnedSlice(self.allocator) catch &.{};
    }
};

/// Closure environment type
pub const ClosureEnv = struct {
    /// Map from variable name to its value in the environment
    captures: std.StringHashMap(ir.Value),
    allocator: Allocator,

    pub fn init(allocator: Allocator) ClosureEnv {
        return .{
            .captures = std.StringHashMap(ir.Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ClosureEnv) void {
        self.captures.deinit();
    }

    pub fn addCapture(self: *ClosureEnv, name: []const u8, value: ir.Value) !void {
        try self.captures.put(name, value);
    }

    pub fn getCapture(self: *const ClosureEnv, name: []const u8) ?ir.Value {
        return self.captures.get(name);
    }
};

test "free variable collection - simple capture" {
    // This is a placeholder test - actual testing requires AST construction
}
