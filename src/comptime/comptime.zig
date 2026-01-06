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
            const tag = self.store.stmtTag(stmt_idx);
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
        if (view.init != .null) {
            const value = try self.evaluateExpression(view.init);
            try self.constants.put(name_id, value);
        }
    }

    /// Process a statement, expanding comptime if as needed
    fn processStatement(self: *Self, stmt_idx: StmtIdx, output: *std.ArrayListUnmanaged(StmtIdx)) EvalError!void {
        const tag = self.store.stmtTag(stmt_idx);

        switch (tag) {
            .const_decl => {
                // Const declarations don't generate runtime code
                // They've already been registered, so skip them
            },
            .comptime_if => {
                // Get comptime_if data:
                // - data.a = condition expression
                // - data.b >> 16 = then body statement
                // - data.b & 0xFFFF = index into extra_data for else body
                const data = self.store.stmtData(stmt_idx);
                const condition_expr = ExprIdx.fromInt(data.a);
                const then_body = StmtIdx.fromInt(data.b >> 16);
                const extra_start = data.b & 0xFFFF;
                const else_body = StmtIdx.fromInt(self.store.extra_data.items[extra_start]);

                // Evaluate the condition
                const condition = try self.evaluateExpression(condition_expr);
                const is_true = condition.isTruthy();

                if (is_true) {
                    // Include then branch
                    if (then_body != .null) {
                        try self.processStatement(then_body, output);
                    }
                } else {
                    // Include else branch if present
                    if (else_body != .null) {
                        try self.processStatement(else_body, output);
                    }
                }
            },
            .comptime_block => {
                // Get comptime_block data:
                // - data.a = body statement (a block)
                // For now, just include the block's statements as runtime code
                // In the future, we might actually execute code at compile time
                const data = self.store.stmtData(stmt_idx);
                const body = StmtIdx.fromInt(data.a);

                // The body is a block - expand its statements into the output
                if (body != .null) {
                    const body_tag = self.store.stmtTag(body);
                    if (body_tag == .block) {
                        const view = ast.views.BlockView.from(self.store, body);
                        const stmts = view.getStatements(self.store);
                        for (stmts) |stmt_raw| {
                            const inner_stmt = StmtIdx.fromInt(stmt_raw);
                            try self.processStatement(inner_stmt, output);
                        }
                    } else {
                        // Single statement
                        try self.processStatement(body, output);
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

        const tag = self.store.exprTag(expr_idx);

        return switch (tag) {
            .int_literal => blk: {
                const view = ast.views.IntLiteralView.from(self.store, expr_idx);
                break :blk Value.fromInt(view.value);
            },
            .float_literal => blk: {
                const view = ast.views.FloatLiteralView.from(self.store, expr_idx);
                // Convert float to decimal representation
                break :blk Value{ .decimal = .{
                    .value = @intFromFloat(view.value * 100),
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
            .call, .method_call, .index, .slice_expr, .member, .optional_member, .optional_index, .range, .array_init, .struct_init, .generic_struct_init, .lambda, .if_expr, .match_expr, .block_expr, .is_expr, .interp_string => EvalError.InvalidExpression,
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
        const left = try self.evaluateExpression(view.lhs);
        const right = try self.evaluateExpression(view.rhs);

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

            // Bitwise operations
            .bit_and => Value.fromInt(left.toInt() & right.toInt()),
            .bit_or => Value.fromInt(left.toInt() | right.toInt()),
            .bit_xor => Value.fromInt(left.toInt() ^ right.toInt()),
            .shl => Value.fromInt(left.toInt() << @intCast(right.toInt())),
            .shr => Value.fromInt(left.toInt() >> @intCast(right.toInt())),

            // Rounding operations - treat as division for now
            .round, .trunc => Value.fromInt(@divTrunc(left.toInt(), right.toInt())),

            // Null coalescing - for comptime, just use left if not undefined
            .null_coalesce => if (left != .undefined) left else right,

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
        // Get the builtin data:
        // - data.a = StringId of the builtin name
        // - data.b = (args_len << 16) | args_start
        const data = self.store.exprData(expr_idx);
        const name_id: StringId = @enumFromInt(data.a);
        const name = self.strings.get(name_id);

        // Extract arguments
        const args_len = data.b >> 16;
        const args_start = data.b & 0xFFFF;

        // Build args slice from extra_data
        var args: []const ExprIdx = &.{};
        if (args_len > 0) {
            const raw_args = self.store.extra_data.items[args_start .. args_start + args_len];
            // Cast to ExprIdx slice - the extra_data stores the raw u32 values
            args = @as([*]const ExprIdx, @ptrCast(raw_args.ptr))[0..args_len];
        }

        // Get location for @line()
        const loc = self.store.exprLoc(expr_idx);

        const ctx = builtins.BuiltinContext{
            .constants = &self.constants,
            .strings = self.strings,
            .source_file = self.source_file,
            .line = loc.line,
        };

        return builtins.evaluate(name, args, &ctx, self) catch EvalError.BuiltinError;
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
