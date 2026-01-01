//! Compile-Time Evaluator
//!
//! Evaluates compile-time expressions and resolves comptime if statements.
//! This pass runs after parsing and before IR lowering.
//!
//! Responsibilities:
//! 1. Collect and evaluate const declarations
//! 2. Evaluate @builtin() calls
//! 3. Resolve comptime if conditions
//! 4. Remove dead branches from the AST

const std = @import("std");
const ast = @import("../ast/mod.zig");
const NodeStore = ast.NodeStore;
const StringInterner = ast.StringInterner;
const StringId = ast.StringId;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const StatementTag = ast.StatementTag;
const ExpressionTag = ast.ExpressionTag;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;
const Value = @import("value.zig").Value;
const builtins = @import("builtins.zig");

pub const EvalError = error{
    UndefinedConstant,
    InvalidExpression,
    CircularReference,
    TypeMismatch,
    DivisionByZero,
    OutOfMemory,
    BuiltinError,
};

/// Compile-time evaluator
pub const Evaluator = struct {
    allocator: std.mem.Allocator,

    /// Reference to the NodeStore
    store: *const NodeStore,

    /// Reference to the StringInterner
    strings: *const StringInterner,

    /// Map of constant names (StringId) to their values
    constants: std.AutoHashMap(StringId, Value),

    /// Constants currently being evaluated (for cycle detection)
    evaluating: std.AutoHashMap(StringId, void),

    /// External defines (from command line or config) - uses string keys
    external_defines: ?*const std.StringHashMap(Value),

    /// Current source file (for @file())
    source_file: ?[]const u8,

    const Self = @This();

    /// Initialize the evaluator
    pub fn init(
        allocator: std.mem.Allocator,
        store: *const NodeStore,
        strings: *const StringInterner,
    ) Self {
        return .{
            .allocator = allocator,
            .store = store,
            .strings = strings,
            .constants = std.AutoHashMap(StringId, Value).init(allocator),
            .evaluating = std.AutoHashMap(StringId, void).init(allocator),
            .external_defines = null,
            .source_file = null,
        };
    }

    /// Deinitialize
    pub fn deinit(self: *Self) void {
        self.constants.deinit();
        self.evaluating.deinit();
    }

    /// Set external defines (from command line: --define DEBUG=true)
    pub fn setExternalDefines(self: *Self, defines: *const std.StringHashMap(Value)) void {
        self.external_defines = defines;
    }

    /// Set the current source file (for @file() builtin)
    pub fn setSourceFile(self: *Self, path: []const u8) void {
        self.source_file = path;
    }

    /// Process statements - evaluate comptime constructs and return filtered list
    /// Returns a new slice of StmtIdx with comptime_if statements resolved
    pub fn process(self: *Self, statements: []const StmtIdx) EvalError![]const StmtIdx {
        // First pass: collect all const declarations
        for (statements) |stmt_idx| {
            const tag = self.store.getStatementTag(stmt_idx);
            if (tag == .const_decl) {
                try self.registerConst(stmt_idx);
            }
        }

        // Second pass: resolve comptime if statements
        var new_statements: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer new_statements.deinit(self.allocator);

        for (statements) |stmt_idx| {
            try self.processStatement(stmt_idx, &new_statements);
        }

        return new_statements.toOwnedSlice(self.allocator) catch return EvalError.OutOfMemory;
    }

    /// Register a constant and evaluate its value
    fn registerConst(self: *Self, stmt_idx: StmtIdx) EvalError!void {
        const view = ast.views.ConstDeclView.from(self.store, stmt_idx);
        const name_id = view.name;

        // Get string name for external defines lookup
        const name_str = self.strings.get(name_id);

        // Check for external override
        if (self.external_defines) |ext| {
            if (ext.get(name_str)) |val| {
                try self.constants.put(name_id, val);
                return;
            }
        }

        // Check for circular reference
        if (self.evaluating.contains(name_id)) {
            return EvalError.CircularReference;
        }

        // Mark as evaluating
        try self.evaluating.put(name_id, {});
        defer _ = self.evaluating.remove(name_id);

        // Evaluate the value
        if (view.value) |value_expr| {
            const value = try self.evaluateExpression(value_expr);
            try self.constants.put(name_id, value);
        }
    }

    /// Process a statement, expanding comptime if as needed
    fn processStatement(self: *Self, stmt_idx: StmtIdx, output: *std.ArrayListUnmanaged(StmtIdx)) EvalError!void {
        const tag = self.store.getStatementTag(stmt_idx);

        switch (tag) {
            .const_decl => {
                // Const declarations don't generate runtime code
                // They've already been registered, so skip them
            },
            .comptime_if => {
                // Get comptime_if data from extra
                const data = self.store.stmt_data.items[@intFromEnum(stmt_idx)];
                const extra_idx = data.lhs;

                // Read extra data: [condition_expr, then_count, then_stmts..., else_count, else_stmts...]
                const condition_expr = ExprIdx.fromExtra(self.store.extra_data.items[extra_idx]);
                const then_count = self.store.extra_data.items[extra_idx + 1];
                const then_stmts_start = extra_idx + 2;

                // Evaluate the condition
                const condition = try self.evaluateExpression(condition_expr);
                const is_true = condition.isTruthy();

                if (is_true) {
                    // Include then branch statements
                    for (0..then_count) |i| {
                        const then_stmt = StmtIdx.fromExtra(self.store.extra_data.items[then_stmts_start + i]);
                        try self.processStatement(then_stmt, output);
                    }
                } else {
                    // Check for else branch
                    const else_count_idx = then_stmts_start + then_count;
                    const else_count = self.store.extra_data.items[else_count_idx];
                    if (else_count > 0) {
                        const else_stmts_start = else_count_idx + 1;
                        for (0..else_count) |i| {
                            const else_stmt = StmtIdx.fromExtra(self.store.extra_data.items[else_stmts_start + i]);
                            try self.processStatement(else_stmt, output);
                        }
                    }
                }
            },
            else => {
                // Pass through other statements
                output.append(self.allocator, stmt_idx) catch return EvalError.OutOfMemory;
            },
        }
    }

    /// Evaluate a compile-time expression
    pub fn evaluateExpression(self: *Self, expr_idx: ExprIdx) EvalError!Value {
        if (expr_idx == .null) {
            return Value{ .undefined = {} };
        }

        const tag = self.store.getExpressionTag(expr_idx);

        return switch (tag) {
            .int_literal => blk: {
                const view = ast.views.IntLiteralView.from(self.store, expr_idx);
                break :blk Value.fromInt(view.value);
            },
            .float_literal => blk: {
                const view = ast.views.FloatLiteralView.from(self.store, expr_idx);
                // Convert float bits back to float then to decimal representation
                const float_val = @as(f64, @bitCast(view.bits));
                break :blk Value{ .decimal = .{
                    .value = @intFromFloat(float_val * 100),
                    .scale = 2,
                } };
            },
            .string_literal => blk: {
                const view = ast.views.StringLiteralView.from(self.store, expr_idx);
                const str = self.strings.get(view.value);
                break :blk Value.fromString(str);
            },
            .bool_literal => blk: {
                const view = ast.views.BoolLiteralView.from(self.store, expr_idx);
                break :blk Value.fromBool(view.value);
            },
            .null_literal => Value{ .undefined = {} },
            .identifier => blk: {
                const view = ast.views.IdentifierView.from(self.store, expr_idx);
                break :blk try self.resolveIdentifier(view.name);
            },
            .binary => blk: {
                const view = ast.views.BinaryExprView.from(self.store, expr_idx);
                break :blk try self.evaluateBinary(view);
            },
            .unary => blk: {
                const view = ast.views.UnaryExprView.from(self.store, expr_idx);
                break :blk try self.evaluateUnary(view);
            },
            .grouping => blk: {
                const view = ast.views.GroupingView.from(self.store, expr_idx);
                break :blk try self.evaluateExpression(view.inner);
            },
            .comptime_builtin => blk: {
                break :blk try self.evaluateBuiltin(expr_idx);
            },
            // These can't be evaluated at compile time
            .call, .method_call, .index, .member, .range,
            .array_init, .struct_init, .lambda, .ternary,
            .try_expr, .orelse_expr, .catch_expr => EvalError.InvalidExpression,
        };
    }

    /// Resolve an identifier (constant reference)
    fn resolveIdentifier(self: *Self, name_id: StringId) EvalError!Value {
        const name = self.strings.get(name_id);

        // Check for boolean literals
        if (std.mem.eql(u8, name, "true")) return Value.fromBool(true);
        if (std.mem.eql(u8, name, "false")) return Value.fromBool(false);

        // Check external defines first (uses string keys)
        if (self.external_defines) |ext| {
            if (ext.get(name)) |val| return val;
        }

        // Check registered constants (uses StringId keys)
        if (self.constants.get(name_id)) |val| return val;

        // Undefined constant
        return EvalError.UndefinedConstant;
    }

    /// Evaluate a binary expression
    fn evaluateBinary(self: *Self, view: ast.views.BinaryExprView) EvalError!Value {
        const left = try self.evaluateExpression(view.left);
        const right = try self.evaluateExpression(view.right);

        return switch (view.op) {
            // Arithmetic
            .add => Value.fromInt(left.toInt() + right.toInt()),
            .sub => Value.fromInt(left.toInt() - right.toInt()),
            .mul => Value.fromInt(left.toInt() * right.toInt()),
            .div => blk: {
                const r = right.toInt();
                if (r == 0) return EvalError.DivisionByZero;
                break :blk Value.fromInt(@divTrunc(left.toInt(), r));
            },
            .mod => blk: {
                const r = right.toInt();
                if (r == 0) return EvalError.DivisionByZero;
                break :blk Value.fromInt(@mod(left.toInt(), r));
            },

            // Comparison
            .eq => Value.fromBool(left.eql(right)),
            .ne => Value.fromBool(!left.eql(right)),
            .lt => Value.fromBool(left.compare(right) == .lt),
            .le => Value.fromBool(left.compare(right) != .gt),
            .gt => Value.fromBool(left.compare(right) == .gt),
            .ge => Value.fromBool(left.compare(right) != .lt),

            // Logical
            .@"and" => Value.fromBool(left.isTruthy() and right.isTruthy()),
            .@"or" => Value.fromBool(left.isTruthy() or right.isTruthy()),

            // String concatenation (using add for now)
            .concat => blk: {
                const l_str = left.toString(self.allocator) catch return EvalError.OutOfMemory;
                const r_str = right.toString(self.allocator) catch return EvalError.OutOfMemory;
                const result = std.fmt.allocPrint(self.allocator, "{s}{s}", .{ l_str, r_str }) catch return EvalError.OutOfMemory;
                break :blk Value.fromString(result);
            },

            // Bitwise operations
            .bit_and => Value.fromInt(left.toInt() & right.toInt()),
            .bit_or => Value.fromInt(left.toInt() | right.toInt()),
            .bit_xor => Value.fromInt(left.toInt() ^ right.toInt()),
            .shl => Value.fromInt(left.toInt() << @intCast(right.toInt())),
            .shr => Value.fromInt(left.toInt() >> @intCast(right.toInt())),

            // Range - not evaluable at compile time
            .range, .range_inclusive => EvalError.InvalidExpression,
        };
    }

    /// Evaluate a unary expression
    fn evaluateUnary(self: *Self, view: ast.views.UnaryExprView) EvalError!Value {
        const operand = try self.evaluateExpression(view.operand);

        return switch (view.op) {
            .neg => Value.fromInt(-operand.toInt()),
            .not => Value.fromBool(!operand.isTruthy()),
            .bit_not => Value.fromInt(~operand.toInt()),
            .addr_of, .deref => EvalError.InvalidExpression, // Not supported in comptime
        };
    }

    /// Evaluate a comptime builtin (@defined, @os, etc.)
    fn evaluateBuiltin(self: *Self, expr_idx: ExprIdx) EvalError!Value {
        // Get the builtin data
        const data = self.store.expr_data.items[@intFromEnum(expr_idx)];
        const name_id = StringId.fromExtra(data.lhs);
        const name = self.strings.get(name_id);

        // Get location for @line()
        const loc = self.store.expr_locs.items[@intFromEnum(expr_idx)];

        const ctx = builtins.BuiltinContext{
            .constants = &self.constants,
            .strings = self.strings,
            .source_file = self.source_file,
            .line = loc.line,
        };

        // Get arguments from extra data if present
        const args_extra = data.rhs;
        var args: std.ArrayListUnmanaged(ExprIdx) = .{};
        defer args.deinit(self.allocator);

        if (args_extra > 0) {
            const args_count = self.store.extra_data.items[args_extra];
            for (0..args_count) |i| {
                const arg_idx = ExprIdx.fromExtra(self.store.extra_data.items[args_extra + 1 + i]);
                args.append(self.allocator, arg_idx) catch return EvalError.OutOfMemory;
            }
        }

        return builtins.evaluate(name, args.items, &ctx, self) catch EvalError.BuiltinError;
    }

    /// Check if a constant is defined
    pub fn isDefined(self: *const Self, name_id: StringId) bool {
        if (self.external_defines) |ext| {
            const name = self.strings.get(name_id);
            if (ext.contains(name)) return true;
        }
        return self.constants.contains(name_id);
    }

    /// Check if a constant is defined by string name
    pub fn isDefinedByName(self: *const Self, name: []const u8) bool {
        if (self.external_defines) |ext| {
            if (ext.contains(name)) return true;
        }
        // For string-based lookup, we need to search all constants
        var it = self.constants.iterator();
        while (it.next()) |entry| {
            const const_name = self.strings.get(entry.key_ptr.*);
            if (std.mem.eql(u8, const_name, name)) return true;
        }
        return false;
    }

    /// Get a constant value by StringId
    pub fn getConstant(self: *const Self, name_id: StringId) ?Value {
        if (self.external_defines) |ext| {
            const name = self.strings.get(name_id);
            if (ext.get(name)) |val| return val;
        }
        return self.constants.get(name_id);
    }
};

test "evaluator basic expressions" {
    const allocator = std.testing.allocator;

    // Create a minimal NodeStore and StringInterner for testing
    var strings = StringInterner.init(allocator);
    defer strings.deinit();
    var store = NodeStore.init(allocator, &strings);
    defer store.deinit();

    var eval = Evaluator.init(allocator, &store, &strings);
    defer eval.deinit();

    // Test integer literal evaluation
    const int_expr = try store.addIntLiteral(42, .zero);
    const result = try eval.evaluateExpression(int_expr);
    try std.testing.expectEqual(@as(i64, 42), result.toInt());
}

test "evaluator boolean" {
    const allocator = std.testing.allocator;

    var strings = StringInterner.init(allocator);
    defer strings.deinit();
    var store = NodeStore.init(allocator, &strings);
    defer store.deinit();

    var eval = Evaluator.init(allocator, &store, &strings);
    defer eval.deinit();

    // Test boolean literal
    const bool_expr = try store.addBoolLiteral(true, .zero);
    const result = try eval.evaluateExpression(bool_expr);
    try std.testing.expect(result.toBool() == true);
}

test "evaluator binary arithmetic" {
    const allocator = std.testing.allocator;

    var strings = StringInterner.init(allocator);
    defer strings.deinit();
    var store = NodeStore.init(allocator, &strings);
    defer store.deinit();

    var eval = Evaluator.init(allocator, &store, &strings);
    defer eval.deinit();

    // Test 2 + 3
    const left = try store.addIntLiteral(2, .zero);
    const right = try store.addIntLiteral(3, .zero);
    const bin_expr = try store.addBinary(left, .add, right, .zero);
    const result = try eval.evaluateExpression(bin_expr);
    try std.testing.expectEqual(@as(i64, 5), result.toInt());
}

test "evaluator comparison" {
    const allocator = std.testing.allocator;

    var strings = StringInterner.init(allocator);
    defer strings.deinit();
    var store = NodeStore.init(allocator, &strings);
    defer store.deinit();

    var eval = Evaluator.init(allocator, &store, &strings);
    defer eval.deinit();

    // Test 5 > 3
    const left = try store.addIntLiteral(5, .zero);
    const right = try store.addIntLiteral(3, .zero);
    const bin_expr = try store.addBinary(left, .gt, right, .zero);
    const result = try eval.evaluateExpression(bin_expr);
    try std.testing.expect(result.toBool() == true);
}
