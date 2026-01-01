//! AST Visitor Pattern
//!
//! Generic visitor infrastructure for traversing AST nodes.
//! Eliminates duplicated traversal logic across compiler passes.
//!
//! ## Usage
//!
//! ```zig
//! const MyVisitor = struct {
//!     count: usize = 0,
//!
//!     pub fn visitBinary(self: *@This(), store: *const NodeStore, idx: ExprIdx) void {
//!         self.count += 1;
//!     }
//! };
//!
//! var visitor = MyVisitor{};
//! walkExpr(store, root_expr, &visitor);
//! ```

const std = @import("std");
const node_store = @import("node_store.zig");
const NodeStore = node_store.NodeStore;
const NodeData = node_store.NodeData;
const SourceLoc = node_store.SourceLoc;
const ExpressionTag = node_store.ExpressionTag;
const StatementTag = node_store.StatementTag;
const TypeTag = node_store.TypeTag;
const BinaryOp = node_store.BinaryOp;
const UnaryOp = node_store.UnaryOp;

const indices = @import("indices.zig");
const StmtIdx = indices.StmtIdx;
const ExprIdx = indices.ExprIdx;
const TypeIdx = indices.TypeIdx;
const ExtraSpan = indices.ExtraSpan;

const base = @import("../base/mod.zig");
const StringId = base.StringId;

// ============================================================
// Visitor Interfaces
// ============================================================

/// Walk result - controls traversal flow
pub const WalkResult = enum {
    /// Continue traversal normally
    continue_walk,
    /// Skip visiting children of current node
    skip_children,
    /// Stop traversal entirely
    stop,
};

/// Expression visitor interface
/// Implement only the methods you need; unimplemented methods default to continue_walk
pub fn ExprVisitor(comptime Context: type) type {
    return struct {
        // Callbacks for each expression type
        visitIntLiteralFn: ?*const fn (*Context, *const NodeStore, ExprIdx, i64) WalkResult = null,
        visitFloatLiteralFn: ?*const fn (*Context, *const NodeStore, ExprIdx, f64) WalkResult = null,
        visitStringLiteralFn: ?*const fn (*Context, *const NodeStore, ExprIdx, StringId) WalkResult = null,
        visitBoolLiteralFn: ?*const fn (*Context, *const NodeStore, ExprIdx, bool) WalkResult = null,
        visitNullLiteralFn: ?*const fn (*Context, *const NodeStore, ExprIdx) WalkResult = null,
        visitIdentifierFn: ?*const fn (*Context, *const NodeStore, ExprIdx, StringId) WalkResult = null,
        visitMemberFn: ?*const fn (*Context, *const NodeStore, ExprIdx, ExprIdx, StringId) WalkResult = null,
        visitIndexFn: ?*const fn (*Context, *const NodeStore, ExprIdx, ExprIdx, ExprIdx) WalkResult = null,
        visitBinaryFn: ?*const fn (*Context, *const NodeStore, ExprIdx, ExprIdx, BinaryOp, ExprIdx) WalkResult = null,
        visitUnaryFn: ?*const fn (*Context, *const NodeStore, ExprIdx, UnaryOp, ExprIdx) WalkResult = null,
        visitCallFn: ?*const fn (*Context, *const NodeStore, ExprIdx, ExprIdx, []const u32) WalkResult = null,
        visitGroupingFn: ?*const fn (*Context, *const NodeStore, ExprIdx, ExprIdx) WalkResult = null,

        // Generic pre/post visit for any expression
        preVisitFn: ?*const fn (*Context, *const NodeStore, ExprIdx) WalkResult = null,
        postVisitFn: ?*const fn (*Context, *const NodeStore, ExprIdx) void = null,

        const Self = @This();

        pub fn preVisit(self: *const Self, ctx: *Context, store: *const NodeStore, idx: ExprIdx) WalkResult {
            if (self.preVisitFn) |f| return f(ctx, store, idx);
            return .continue_walk;
        }

        pub fn postVisit(self: *const Self, ctx: *Context, store: *const NodeStore, idx: ExprIdx) void {
            if (self.postVisitFn) |f| f(ctx, store, idx);
        }
    };
}

/// Statement visitor interface
pub fn StmtVisitor(comptime Context: type) type {
    return struct {
        visitExprStmtFn: ?*const fn (*Context, *const NodeStore, StmtIdx, ExprIdx) WalkResult = null,
        visitAssignmentFn: ?*const fn (*Context, *const NodeStore, StmtIdx, ExprIdx, ExprIdx) WalkResult = null,
        visitLetDeclFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StringId, TypeIdx, ExprIdx, bool) WalkResult = null,
        visitConstDeclFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StringId, TypeIdx, ExprIdx) WalkResult = null,
        visitReturnFn: ?*const fn (*Context, *const NodeStore, StmtIdx, ExprIdx) WalkResult = null,
        visitBreakFn: ?*const fn (*Context, *const NodeStore, StmtIdx) WalkResult = null,
        visitContinueFn: ?*const fn (*Context, *const NodeStore, StmtIdx) WalkResult = null,
        visitBlockFn: ?*const fn (*Context, *const NodeStore, StmtIdx, []const u32) WalkResult = null,
        visitIfStmtFn: ?*const fn (*Context, *const NodeStore, StmtIdx, ExprIdx, StmtIdx, StmtIdx) WalkResult = null,
        visitWhileStmtFn: ?*const fn (*Context, *const NodeStore, StmtIdx, ExprIdx, StmtIdx) WalkResult = null,
        visitForStmtFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StringId, ExprIdx, StmtIdx) WalkResult = null,
        visitLoopStmtFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StmtIdx) WalkResult = null,
        visitFnDefFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StringId, []const u32, TypeIdx, StmtIdx) WalkResult = null,
        visitImportFn: ?*const fn (*Context, *const NodeStore, StmtIdx, StringId) WalkResult = null,

        // Generic pre/post visit for any statement
        preVisitFn: ?*const fn (*Context, *const NodeStore, StmtIdx) WalkResult = null,
        postVisitFn: ?*const fn (*Context, *const NodeStore, StmtIdx) void = null,

        const Self = @This();

        pub fn preVisit(self: *const Self, ctx: *Context, store: *const NodeStore, idx: StmtIdx) WalkResult {
            if (self.preVisitFn) |f| return f(ctx, store, idx);
            return .continue_walk;
        }

        pub fn postVisit(self: *const Self, ctx: *Context, store: *const NodeStore, idx: StmtIdx) void {
            if (self.postVisitFn) |f| f(ctx, store, idx);
        }
    };
}

// ============================================================
// Walker Functions
// ============================================================

/// Walk an expression tree, calling visitor methods
pub fn walkExpr(
    comptime Context: type,
    store: *const NodeStore,
    idx: ExprIdx,
    ctx: *Context,
    visitor: *const ExprVisitor(Context),
) WalkResult {
    // Pre-visit callback
    const pre_result = visitor.preVisit(ctx, store, idx);
    if (pre_result == .stop) return .stop;
    if (pre_result == .skip_children) {
        visitor.postVisit(ctx, store, idx);
        return .continue_walk;
    }

    const tag = store.exprTag(idx);
    const data = store.exprData(idx);

    const result: WalkResult = switch (tag) {
        .int_literal => blk: {
            if (visitor.visitIntLiteralFn) |f| {
                break :blk f(ctx, store, idx, data.getIntValue());
            }
            break :blk .continue_walk;
        },
        .float_literal => blk: {
            if (visitor.visitFloatLiteralFn) |f| {
                const bits: u64 = (@as(u64, data.b) << 32) | data.a;
                const value: f64 = @bitCast(bits);
                break :blk f(ctx, store, idx, value);
            }
            break :blk .continue_walk;
        },
        .string_literal => blk: {
            if (visitor.visitStringLiteralFn) |f| {
                break :blk f(ctx, store, idx, data.getName());
            }
            break :blk .continue_walk;
        },
        .bool_literal => blk: {
            if (visitor.visitBoolLiteralFn) |f| {
                break :blk f(ctx, store, idx, data.a != 0);
            }
            break :blk .continue_walk;
        },
        .null_literal => blk: {
            if (visitor.visitNullLiteralFn) |f| {
                break :blk f(ctx, store, idx);
            }
            break :blk .continue_walk;
        },
        .identifier => blk: {
            if (visitor.visitIdentifierFn) |f| {
                break :blk f(ctx, store, idx, data.getName());
            }
            break :blk .continue_walk;
        },
        .member => blk: {
            const object = data.getObject();
            const field = data.getField();

            // Visit object first
            const obj_result = walkExpr(Context, store, object, ctx, visitor);
            if (obj_result == .stop) break :blk .stop;

            if (visitor.visitMemberFn) |f| {
                break :blk f(ctx, store, idx, object, field);
            }
            break :blk .continue_walk;
        },
        .index => blk: {
            const object = data.getObject();
            const index_expr = data.getIndex();

            // Visit object and index
            const obj_result = walkExpr(Context, store, object, ctx, visitor);
            if (obj_result == .stop) break :blk .stop;
            const idx_result = walkExpr(Context, store, index_expr, ctx, visitor);
            if (idx_result == .stop) break :blk .stop;

            if (visitor.visitIndexFn) |f| {
                break :blk f(ctx, store, idx, object, index_expr);
            }
            break :blk .continue_walk;
        },
        .binary => blk: {
            const lhs = data.getLhs();
            const rhs = data.getRhs();
            const op = data.getBinaryOp();

            // Visit left and right operands
            const lhs_result = walkExpr(Context, store, lhs, ctx, visitor);
            if (lhs_result == .stop) break :blk .stop;
            const rhs_result = walkExpr(Context, store, rhs, ctx, visitor);
            if (rhs_result == .stop) break :blk .stop;

            if (visitor.visitBinaryFn) |f| {
                break :blk f(ctx, store, idx, lhs, op, rhs);
            }
            break :blk .continue_walk;
        },
        .unary => blk: {
            const operand = data.getOperand();
            const op = data.getUnaryOp();

            // Visit operand
            const operand_result = walkExpr(Context, store, operand, ctx, visitor);
            if (operand_result == .stop) break :blk .stop;

            if (visitor.visitUnaryFn) |f| {
                break :blk f(ctx, store, idx, op, operand);
            }
            break :blk .continue_walk;
        },
        .call => blk: {
            const callee = data.getCallee();
            const args_info = data.b;
            const args_count = args_info >> 16;
            const args_start = args_info & 0xFFFF;
            const args = store.extra_data.items[args_start .. args_start + args_count];

            // Visit callee
            const callee_result = walkExpr(Context, store, callee, ctx, visitor);
            if (callee_result == .stop) break :blk .stop;

            // Visit each argument
            for (args) |arg_raw| {
                const arg_idx = ExprIdx.fromInt(arg_raw);
                const arg_result = walkExpr(Context, store, arg_idx, ctx, visitor);
                if (arg_result == .stop) break :blk .stop;
            }

            if (visitor.visitCallFn) |f| {
                break :blk f(ctx, store, idx, callee, args);
            }
            break :blk .continue_walk;
        },
        .grouping => blk: {
            const inner = ExprIdx.fromInt(data.a);

            // Visit inner expression
            const inner_result = walkExpr(Context, store, inner, ctx, visitor);
            if (inner_result == .stop) break :blk .stop;

            if (visitor.visitGroupingFn) |f| {
                break :blk f(ctx, store, idx, inner);
            }
            break :blk .continue_walk;
        },
        else => .continue_walk, // TODO: Handle remaining expression types
    };

    if (result == .stop) return .stop;

    // Post-visit callback
    visitor.postVisit(ctx, store, idx);

    return .continue_walk;
}

/// Walk a statement tree, calling visitor methods
pub fn walkStmt(
    comptime Context: type,
    store: *const NodeStore,
    idx: StmtIdx,
    ctx: *Context,
    stmt_visitor: *const StmtVisitor(Context),
    expr_visitor: ?*const ExprVisitor(Context),
) WalkResult {
    // Pre-visit callback
    const pre_result = stmt_visitor.preVisit(ctx, store, idx);
    if (pre_result == .stop) return .stop;
    if (pre_result == .skip_children) {
        stmt_visitor.postVisit(ctx, store, idx);
        return .continue_walk;
    }

    const tag = store.stmtTag(idx);
    const data = store.stmtData(idx);

    const result: WalkResult = switch (tag) {
        .expression => blk: {
            const expr = data.getExpr();

            // Visit expression if we have an expr visitor
            if (expr_visitor) |ev| {
                const expr_result = walkExpr(Context, store, expr, ctx, ev);
                if (expr_result == .stop) break :blk .stop;
            }

            if (stmt_visitor.visitExprStmtFn) |f| {
                break :blk f(ctx, store, idx, expr);
            }
            break :blk .continue_walk;
        },
        .assignment => blk: {
            const target = data.getTarget();
            const value = data.getValue();

            // Visit target and value expressions
            if (expr_visitor) |ev| {
                const target_result = walkExpr(Context, store, target, ctx, ev);
                if (target_result == .stop) break :blk .stop;
                const value_result = walkExpr(Context, store, value, ctx, ev);
                if (value_result == .stop) break :blk .stop;
            }

            if (stmt_visitor.visitAssignmentFn) |f| {
                break :blk f(ctx, store, idx, target, value);
            }
            break :blk .continue_walk;
        },
        .return_stmt => blk: {
            const value = data.getReturnValue();

            // Visit return value expression
            if (expr_visitor) |ev| {
                // Check if return value is valid (non-zero index typically means it exists)
                if (value.toInt() != 0) {
                    const value_result = walkExpr(Context, store, value, ctx, ev);
                    if (value_result == .stop) break :blk .stop;
                }
            }

            if (stmt_visitor.visitReturnFn) |f| {
                break :blk f(ctx, store, idx, value);
            }
            break :blk .continue_walk;
        },
        .break_stmt => blk: {
            if (stmt_visitor.visitBreakFn) |f| {
                break :blk f(ctx, store, idx);
            }
            break :blk .continue_walk;
        },
        .continue_stmt => blk: {
            if (stmt_visitor.visitContinueFn) |f| {
                break :blk f(ctx, store, idx);
            }
            break :blk .continue_walk;
        },
        .block => blk: {
            const span = data.getSpan();
            const stmts = store.getStmtSpan(span);

            // Visit each statement in the block
            for (stmts) |stmt_raw| {
                const stmt_idx = StmtIdx.fromInt(stmt_raw);
                const stmt_result = walkStmt(Context, store, stmt_idx, ctx, stmt_visitor, expr_visitor);
                if (stmt_result == .stop) break :blk .stop;
            }

            if (stmt_visitor.visitBlockFn) |f| {
                break :blk f(ctx, store, idx, stmts);
            }
            break :blk .continue_walk;
        },
        .if_stmt => blk: {
            const cond = ExprIdx.fromInt(data.a);
            const then_body = StmtIdx.fromInt(@as(u16, @truncate(data.b >> 16)));
            const else_extra = data.b & 0xFFFF;
            const else_body = StmtIdx.fromInt(store.extra_data.items[else_extra]);

            // Visit condition
            if (expr_visitor) |ev| {
                const cond_result = walkExpr(Context, store, cond, ctx, ev);
                if (cond_result == .stop) break :blk .stop;
            }

            // Visit then and else bodies
            const then_result = walkStmt(Context, store, then_body, ctx, stmt_visitor, expr_visitor);
            if (then_result == .stop) break :blk .stop;
            if (else_body.toInt() != 0) {
                const else_result = walkStmt(Context, store, else_body, ctx, stmt_visitor, expr_visitor);
                if (else_result == .stop) break :blk .stop;
            }

            if (stmt_visitor.visitIfStmtFn) |f| {
                break :blk f(ctx, store, idx, cond, then_body, else_body);
            }
            break :blk .continue_walk;
        },
        .while_stmt => blk: {
            const cond = data.getCondition();
            const body = data.getBody();

            // Visit condition and body
            if (expr_visitor) |ev| {
                const cond_result = walkExpr(Context, store, cond, ctx, ev);
                if (cond_result == .stop) break :blk .stop;
            }
            const body_result = walkStmt(Context, store, body, ctx, stmt_visitor, expr_visitor);
            if (body_result == .stop) break :blk .stop;

            if (stmt_visitor.visitWhileStmtFn) |f| {
                break :blk f(ctx, store, idx, cond, body);
            }
            break :blk .continue_walk;
        },
        .loop_stmt => blk: {
            const body = StmtIdx.fromInt(data.a);

            // Visit body
            const body_result = walkStmt(Context, store, body, ctx, stmt_visitor, expr_visitor);
            if (body_result == .stop) break :blk .stop;

            if (stmt_visitor.visitLoopStmtFn) |f| {
                break :blk f(ctx, store, idx, body);
            }
            break :blk .continue_walk;
        },
        .import_stmt => blk: {
            const module_path = data.getName();
            if (stmt_visitor.visitImportFn) |f| {
                break :blk f(ctx, store, idx, module_path);
            }
            break :blk .continue_walk;
        },
        else => .continue_walk, // TODO: Handle remaining statement types
    };

    if (result == .stop) return .stop;

    // Post-visit callback
    stmt_visitor.postVisit(ctx, store, idx);

    return .continue_walk;
}

/// Walk all top-level statements in the AST
pub fn walkAllStmts(
    comptime Context: type,
    store: *const NodeStore,
    ctx: *Context,
    stmt_visitor: *const StmtVisitor(Context),
    expr_visitor: ?*const ExprVisitor(Context),
) WalkResult {
    for (0..store.stmtCount()) |i| {
        const idx = StmtIdx.fromInt(@intCast(i));
        const result = walkStmt(Context, store, idx, ctx, stmt_visitor, expr_visitor);
        if (result == .stop) return .stop;
    }
    return .continue_walk;
}

// ============================================================
// Built-in Visitors
// ============================================================

/// Counter visitor - counts nodes of each type
pub const NodeCounter = struct {
    int_literals: usize = 0,
    float_literals: usize = 0,
    string_literals: usize = 0,
    bool_literals: usize = 0,
    null_literals: usize = 0,
    identifiers: usize = 0,
    binary_ops: usize = 0,
    unary_ops: usize = 0,
    calls: usize = 0,
    members: usize = 0,
    indices: usize = 0,
    groupings: usize = 0,
    total_exprs: usize = 0,

    blocks: usize = 0,
    if_stmts: usize = 0,
    while_stmts: usize = 0,
    for_stmts: usize = 0,
    loop_stmts: usize = 0,
    returns: usize = 0,
    assignments: usize = 0,
    expr_stmts: usize = 0,
    total_stmts: usize = 0,

    const Self = @This();

    pub fn exprVisitor() ExprVisitor(Self) {
        return .{
            .visitIntLiteralFn = countIntLiteral,
            .visitFloatLiteralFn = countFloatLiteral,
            .visitStringLiteralFn = countStringLiteral,
            .visitBoolLiteralFn = countBoolLiteral,
            .visitNullLiteralFn = countNullLiteral,
            .visitIdentifierFn = countIdentifier,
            .visitBinaryFn = countBinary,
            .visitUnaryFn = countUnary,
            .visitCallFn = countCall,
            .visitMemberFn = countMember,
            .visitIndexFn = countIndex,
            .visitGroupingFn = countGrouping,
            .preVisitFn = countTotalExpr,
        };
    }

    pub fn stmtVisitor() StmtVisitor(Self) {
        return .{
            .visitBlockFn = countBlock,
            .visitIfStmtFn = countIfStmt,
            .visitWhileStmtFn = countWhileStmt,
            .visitLoopStmtFn = countLoopStmt,
            .visitReturnFn = countReturn,
            .visitAssignmentFn = countAssignment,
            .visitExprStmtFn = countExprStmt,
            .preVisitFn = countTotalStmt,
        };
    }

    fn countIntLiteral(self: *Self, _: *const NodeStore, _: ExprIdx, _: i64) WalkResult {
        self.int_literals += 1;
        return .continue_walk;
    }

    fn countFloatLiteral(self: *Self, _: *const NodeStore, _: ExprIdx, _: f64) WalkResult {
        self.float_literals += 1;
        return .continue_walk;
    }

    fn countStringLiteral(self: *Self, _: *const NodeStore, _: ExprIdx, _: StringId) WalkResult {
        self.string_literals += 1;
        return .continue_walk;
    }

    fn countBoolLiteral(self: *Self, _: *const NodeStore, _: ExprIdx, _: bool) WalkResult {
        self.bool_literals += 1;
        return .continue_walk;
    }

    fn countNullLiteral(self: *Self, _: *const NodeStore, _: ExprIdx) WalkResult {
        self.null_literals += 1;
        return .continue_walk;
    }

    fn countIdentifier(self: *Self, _: *const NodeStore, _: ExprIdx, _: StringId) WalkResult {
        self.identifiers += 1;
        return .continue_walk;
    }

    fn countBinary(self: *Self, _: *const NodeStore, _: ExprIdx, _: ExprIdx, _: BinaryOp, _: ExprIdx) WalkResult {
        self.binary_ops += 1;
        return .continue_walk;
    }

    fn countUnary(self: *Self, _: *const NodeStore, _: ExprIdx, _: UnaryOp, _: ExprIdx) WalkResult {
        self.unary_ops += 1;
        return .continue_walk;
    }

    fn countCall(self: *Self, _: *const NodeStore, _: ExprIdx, _: ExprIdx, _: []const u32) WalkResult {
        self.calls += 1;
        return .continue_walk;
    }

    fn countMember(self: *Self, _: *const NodeStore, _: ExprIdx, _: ExprIdx, _: StringId) WalkResult {
        self.members += 1;
        return .continue_walk;
    }

    fn countIndex(self: *Self, _: *const NodeStore, _: ExprIdx, _: ExprIdx, _: ExprIdx) WalkResult {
        self.indices += 1;
        return .continue_walk;
    }

    fn countGrouping(self: *Self, _: *const NodeStore, _: ExprIdx, _: ExprIdx) WalkResult {
        self.groupings += 1;
        return .continue_walk;
    }

    fn countTotalExpr(self: *Self, _: *const NodeStore, _: ExprIdx) WalkResult {
        self.total_exprs += 1;
        return .continue_walk;
    }

    fn countBlock(self: *Self, _: *const NodeStore, _: StmtIdx, _: []const u32) WalkResult {
        self.blocks += 1;
        return .continue_walk;
    }

    fn countIfStmt(self: *Self, _: *const NodeStore, _: StmtIdx, _: ExprIdx, _: StmtIdx, _: StmtIdx) WalkResult {
        self.if_stmts += 1;
        return .continue_walk;
    }

    fn countWhileStmt(self: *Self, _: *const NodeStore, _: StmtIdx, _: ExprIdx, _: StmtIdx) WalkResult {
        self.while_stmts += 1;
        return .continue_walk;
    }

    fn countLoopStmt(self: *Self, _: *const NodeStore, _: StmtIdx, _: StmtIdx) WalkResult {
        self.loop_stmts += 1;
        return .continue_walk;
    }

    fn countReturn(self: *Self, _: *const NodeStore, _: StmtIdx, _: ExprIdx) WalkResult {
        self.returns += 1;
        return .continue_walk;
    }

    fn countAssignment(self: *Self, _: *const NodeStore, _: StmtIdx, _: ExprIdx, _: ExprIdx) WalkResult {
        self.assignments += 1;
        return .continue_walk;
    }

    fn countExprStmt(self: *Self, _: *const NodeStore, _: StmtIdx, _: ExprIdx) WalkResult {
        self.expr_stmts += 1;
        return .continue_walk;
    }

    fn countTotalStmt(self: *Self, _: *const NodeStore, _: StmtIdx) WalkResult {
        self.total_stmts += 1;
        return .continue_walk;
    }
};

// ============================================================
// Tests
// ============================================================

test "visitor counts expressions" {
    const StringInterner = base.StringInterner;

    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Build: 1 + 2 * 3
    const one = try store.addIntLiteral(1, SourceLoc.zero);
    const two = try store.addIntLiteral(2, SourceLoc.zero);
    const three = try store.addIntLiteral(3, SourceLoc.zero);
    const mul = try store.addBinary(two, .mul, three, SourceLoc.zero);
    const add = try store.addBinary(one, .add, mul, SourceLoc.zero);

    var counter = NodeCounter{};
    const visitor = NodeCounter.exprVisitor();
    _ = walkExpr(NodeCounter, &store, add, &counter, &visitor);

    try std.testing.expectEqual(@as(usize, 3), counter.int_literals);
    try std.testing.expectEqual(@as(usize, 2), counter.binary_ops);
    try std.testing.expectEqual(@as(usize, 5), counter.total_exprs);
}

test "visitor counts statements" {
    const StringInterner = base.StringInterner;

    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Build a simple block with two expression statements
    const expr1 = try store.addIntLiteral(1, SourceLoc.zero);
    const stmt1 = try store.addExprStmt(expr1, SourceLoc.zero);
    const expr2 = try store.addIntLiteral(2, SourceLoc.zero);
    const stmt2 = try store.addExprStmt(expr2, SourceLoc.zero);
    const stmts = [_]StmtIdx{ stmt1, stmt2 };
    const block = try store.addBlock(&stmts, SourceLoc.zero);

    var counter = NodeCounter{};
    const stmt_visitor = NodeCounter.stmtVisitor();
    const expr_visitor = NodeCounter.exprVisitor();
    _ = walkStmt(NodeCounter, &store, block, &counter, &stmt_visitor, &expr_visitor);

    try std.testing.expectEqual(@as(usize, 1), counter.blocks);
    try std.testing.expectEqual(@as(usize, 2), counter.expr_stmts);
    try std.testing.expectEqual(@as(usize, 2), counter.int_literals);
}

test "walk result controls traversal" {
    const StringInterner = base.StringInterner;

    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Build: 1 + 2
    const one = try store.addIntLiteral(1, SourceLoc.zero);
    const two = try store.addIntLiteral(2, SourceLoc.zero);
    const add = try store.addBinary(one, .add, two, SourceLoc.zero);

    // Visitor that stops after first int literal
    const StopAfterOne = struct {
        count: usize = 0,

        fn visitInt(self: *@This(), _: *const NodeStore, _: ExprIdx, _: i64) WalkResult {
            self.count += 1;
            return if (self.count >= 1) .stop else .continue_walk;
        }
    };

    var stopper = StopAfterOne{};
    const visitor = ExprVisitor(StopAfterOne){
        .visitIntLiteralFn = StopAfterOne.visitInt,
    };
    const result = walkExpr(StopAfterOne, &store, add, &stopper, &visitor);

    try std.testing.expectEqual(WalkResult.stop, result);
    try std.testing.expectEqual(@as(usize, 1), stopper.count);
}
