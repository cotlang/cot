//! AST View Structs
//!
//! Provides structured views into packed NodeData for easier consumption.
//! Views are reconstructed on-demand from the SoA storage.

const std = @import("std");
const base = @import("../base/mod.zig");
const StringId = base.StringId;

const indices = @import("indices.zig");
const StmtIdx = indices.StmtIdx;
const ExprIdx = indices.ExprIdx;
const TypeIdx = indices.TypeIdx;
const ExtraIdx = indices.ExtraIdx;
const ExtraSpan = indices.ExtraSpan;

const expressions = @import("expressions.zig");
const BinaryOp = expressions.BinaryOp;
const UnaryOp = expressions.UnaryOp;

const node_store = @import("node_store.zig");
const NodeStore = node_store.NodeStore;
const NodeData = node_store.NodeData;
const SourceLoc = node_store.SourceLoc;

// ============================================
// Expression Views
// ============================================

/// View for binary expression
pub const BinaryExprView = struct {
    lhs: ExprIdx,
    rhs: ExprIdx,
    op: BinaryOp,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) BinaryExprView {
        const data = store.exprData(idx);
        return .{
            .lhs = data.getLhs(),
            .rhs = ExprIdx.fromInt(@truncate(data.b >> 8)),
            .op = data.getBinaryOp(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for unary expression
pub const UnaryExprView = struct {
    operand: ExprIdx,
    op: UnaryOp,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) UnaryExprView {
        const data = store.exprData(idx);
        return .{
            .operand = data.getOperand(),
            .op = data.getUnaryOp(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for identifier expression
pub const IdentifierView = struct {
    name: StringId,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) IdentifierView {
        const data = store.exprData(idx);
        return .{
            .name = data.getName(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for integer literal
pub const IntLiteralView = struct {
    value: i64,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) IntLiteralView {
        const data = store.exprData(idx);
        return .{
            .value = data.getIntValue(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for float literal
pub const FloatLiteralView = struct {
    value: f64,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) FloatLiteralView {
        const data = store.exprData(idx);
        const bits: u64 = (@as(u64, data.b) << 32) | data.a;
        return .{
            .value = @bitCast(bits),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for string literal
pub const StringLiteralView = struct {
    value: StringId,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) StringLiteralView {
        const data = store.exprData(idx);
        return .{
            .value = data.getName(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for boolean literal
pub const BoolLiteralView = struct {
    value: bool,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) BoolLiteralView {
        const data = store.exprData(idx);
        return .{
            .value = data.a != 0,
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for member access expression
pub const MemberView = struct {
    object: ExprIdx,
    field: StringId,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) MemberView {
        const data = store.exprData(idx);
        return .{
            .object = data.getObject(),
            .field = data.getField(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for index expression
pub const IndexView = struct {
    object: ExprIdx,
    index: ExprIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) IndexView {
        const data = store.exprData(idx);
        return .{
            .object = data.getObject(),
            .index = data.getIndex(),
            .loc = store.exprLoc(idx),
        };
    }
};

/// View for call expression
pub const CallView = struct {
    callee: ExprIdx,
    args_start: ExtraIdx,
    args_count: u16,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) CallView {
        const data = store.exprData(idx);
        return .{
            .callee = data.getCallee(),
            .args_start = ExtraIdx.fromInt(@truncate(data.b)),
            .args_count = @truncate(data.b >> 16),
            .loc = store.exprLoc(idx),
        };
    }

    /// Get argument expression indices
    pub fn getArgs(self: CallView, store: *const NodeStore) []const u32 {
        const start = self.args_start.toInt();
        return store.extra_data.items[start .. start + self.args_count];
    }
};

/// View for grouping expression
pub const GroupingView = struct {
    inner: ExprIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: ExprIdx) GroupingView {
        const data = store.exprData(idx);
        return .{
            .inner = ExprIdx.fromInt(data.a),
            .loc = store.exprLoc(idx),
        };
    }
};

// ============================================
// Statement Views
// ============================================

/// View for expression statement
pub const ExprStmtView = struct {
    expr: ExprIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) ExprStmtView {
        const data = store.stmtData(idx);
        return .{
            .expr = data.getExpr(),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for assignment statement
pub const AssignmentView = struct {
    target: ExprIdx,
    value: ExprIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) AssignmentView {
        const data = store.stmtData(idx);
        return .{
            .target = data.getTarget(),
            .value = data.getValue(),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for let declaration
pub const LetDeclView = struct {
    name: StringId,
    type_idx: TypeIdx,
    init: ExprIdx,
    is_mut: bool,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) LetDeclView {
        const data = store.stmtData(idx);
        const extra_start = @as(u16, @truncate(data.b));
        return .{
            .name = @enumFromInt(data.a),
            .type_idx = TypeIdx.fromInt(@truncate(data.b >> 16)),
            .init = ExprIdx.fromInt(store.extra_data.items[extra_start]),
            .is_mut = store.extra_data.items[extra_start + 1] != 0,
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for const declaration
pub const ConstDeclView = struct {
    name: StringId,
    type_idx: TypeIdx,
    init: ExprIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) ConstDeclView {
        const data = store.stmtData(idx);
        const extra_start = @as(u16, @truncate(data.b));
        return .{
            .name = @enumFromInt(data.a),
            .type_idx = TypeIdx.fromInt(@truncate(data.b >> 16)),
            .init = ExprIdx.fromInt(store.extra_data.items[extra_start]),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for return statement
pub const ReturnView = struct {
    value: ExprIdx, // null if no return value
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) ReturnView {
        const data = store.stmtData(idx);
        return .{
            .value = data.getReturnValue(),
            .loc = store.stmtLoc(idx),
        };
    }

    pub fn hasValue(self: ReturnView) bool {
        return !self.value.isNull();
    }
};

/// View for block statement
pub const BlockView = struct {
    span: ExtraSpan,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) BlockView {
        const data = store.stmtData(idx);
        return .{
            .span = data.getSpan(),
            .loc = store.stmtLoc(idx),
        };
    }

    /// Get statement indices in this block
    pub fn getStatements(self: BlockView, store: *const NodeStore) []const u32 {
        return store.getStmtSpan(self.span);
    }

    /// Check if block is empty
    pub fn isEmpty(self: BlockView) bool {
        return self.span.isEmpty();
    }
};

/// View for if statement
pub const IfStmtView = struct {
    condition: ExprIdx,
    then_body: StmtIdx,
    else_body: StmtIdx, // null if no else
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) IfStmtView {
        const data = store.stmtData(idx);
        const extra_start = @as(u16, @truncate(data.b));
        return .{
            .condition = ExprIdx.fromInt(data.a),
            .then_body = StmtIdx.fromInt(@truncate(data.b >> 16)),
            .else_body = StmtIdx.fromInt(store.extra_data.items[extra_start]),
            .loc = store.stmtLoc(idx),
        };
    }

    pub fn hasElse(self: IfStmtView) bool {
        return !self.else_body.isNull();
    }
};

/// View for while statement
pub const WhileStmtView = struct {
    condition: ExprIdx,
    body: StmtIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) WhileStmtView {
        const data = store.stmtData(idx);
        return .{
            .condition = data.getCondition(),
            .body = data.getBody(),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for for statement
pub const ForStmtView = struct {
    binding: StringId,
    iterable: ExprIdx,
    body: StmtIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) ForStmtView {
        const data = store.stmtData(idx);
        const extra_start = @as(u16, @truncate(data.b));
        return .{
            .binding = @enumFromInt(data.a),
            .iterable = ExprIdx.fromInt(@truncate(data.b >> 16)),
            .body = StmtIdx.fromInt(store.extra_data.items[extra_start]),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// View for loop statement (infinite loop)
pub const LoopStmtView = struct {
    body: StmtIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) LoopStmtView {
        const data = store.stmtData(idx);
        return .{
            .body = StmtIdx.fromInt(data.a),
            .loc = store.stmtLoc(idx),
        };
    }
};

/// Parameter info stored in extra_data
/// is_ref: false = by value (default for Cot, DBL class methods)
///         true = by reference (default for DBL subroutines/functions)
/// default_value: optional default value expression (null if required)
pub const ParamInfo = struct {
    name: StringId,
    type_idx: TypeIdx,
    is_ref: bool = false,
    default_value: ExprIdx = ExprIdx.null,
};

/// View for function definition
pub const FnDefView = struct {
    name: StringId,
    params_start: ExtraIdx,
    param_count: u32,
    return_type: TypeIdx,
    body: StmtIdx,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) FnDefView {
        const data = store.stmtData(idx);
        const params_start = ExtraIdx.fromInt(data.b);
        const param_count = store.extra_data.items[params_start.toInt()];
        // return_type and body are after params (4 values per param: name, type, is_ref, default_value)
        const after_params = params_start.toInt() + 1 + param_count * 4;
        return .{
            .name = data.getName(),
            .params_start = params_start,
            .param_count = param_count,
            .return_type = TypeIdx.fromInt(store.extra_data.items[after_params]),
            .body = StmtIdx.fromInt(store.extra_data.items[after_params + 1]),
            .loc = store.stmtLoc(idx),
        };
    }

    /// Get parameter at index (4 values per param: name, type, is_ref, default_value)
    pub fn getParam(self: FnDefView, store: *const NodeStore, param_idx: u32) ParamInfo {
        const base_idx = self.params_start.toInt() + 1 + param_idx * 4;
        const is_ref_raw = store.extra_data.items[base_idx + 2];
        const default_raw = store.extra_data.items[base_idx + 3];
        return .{
            .name = @enumFromInt(store.extra_data.items[base_idx]),
            .type_idx = TypeIdx.fromInt(store.extra_data.items[base_idx + 1]),
            .is_ref = is_ref_raw != 0,
            .default_value = ExprIdx.fromInt(default_raw),
        };
    }

    /// Iterate over parameters
    pub fn paramIterator(self: FnDefView, store: *const NodeStore) ParamIterator {
        return .{
            .store = store,
            .view = self,
            .current = 0,
        };
    }

    pub const ParamIterator = struct {
        store: *const NodeStore,
        view: FnDefView,
        current: u32,

        pub fn next(self: *ParamIterator) ?ParamInfo {
            if (self.current >= self.view.param_count) return null;
            const param = self.view.getParam(self.store, self.current);
            self.current += 1;
            return param;
        }
    };
};

/// View for import statement
pub const ImportView = struct {
    module_path: StringId,
    loc: SourceLoc,

    pub fn from(store: *const NodeStore, idx: StmtIdx) ImportView {
        const data = store.stmtData(idx);
        return .{
            .module_path = data.getName(),
            .loc = store.stmtLoc(idx),
        };
    }
};

// ============================================
// Type Views
// ============================================

/// View for array type
pub const ArrayTypeView = struct {
    elem_type: TypeIdx,
    size: u32,

    pub fn from(store: *const NodeStore, idx: TypeIdx) ArrayTypeView {
        const data = store.typeData(idx);
        return .{
            .elem_type = TypeIdx.fromInt(data.a),
            .size = data.b,
        };
    }
};

/// View for slice type
pub const SliceTypeView = struct {
    elem_type: TypeIdx,

    pub fn from(store: *const NodeStore, idx: TypeIdx) SliceTypeView {
        const data = store.typeData(idx);
        return .{
            .elem_type = TypeIdx.fromInt(data.a),
        };
    }
};

/// View for optional type
pub const OptionalTypeView = struct {
    inner_type: TypeIdx,

    pub fn from(store: *const NodeStore, idx: TypeIdx) OptionalTypeView {
        const data = store.typeData(idx);
        return .{
            .inner_type = TypeIdx.fromInt(data.a),
        };
    }
};

/// View for pointer type
pub const PointerTypeView = struct {
    pointee: TypeIdx,
    is_const: bool,

    pub fn from(store: *const NodeStore, idx: TypeIdx) PointerTypeView {
        const data = store.typeData(idx);
        return .{
            .pointee = TypeIdx.fromInt(data.a),
            .is_const = data.b == 0, // 0 = const, 1 = mut
        };
    }
};

/// View for named type
pub const NamedTypeView = struct {
    name: StringId,

    pub fn from(store: *const NodeStore, idx: TypeIdx) NamedTypeView {
        const data = store.typeData(idx);
        return .{
            .name = data.getName(),
        };
    }
};

// ============================================================
// Tests
// ============================================================

const StringInterner = base.StringInterner;

test "BinaryExprView" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    const lhs = try store.addIntLiteral(10, SourceLoc.zero);
    const rhs = try store.addIntLiteral(20, SourceLoc.zero);
    const bin = try store.addBinary(lhs, .add, rhs, SourceLoc.init(5, 10));

    const view = BinaryExprView.from(&store, bin);
    try std.testing.expectEqual(lhs, view.lhs);
    try std.testing.expectEqual(rhs, view.rhs);
    try std.testing.expectEqual(BinaryOp.add, view.op);
    try std.testing.expectEqual(@as(u24, 5), view.loc.line);
}

test "BlockView" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    const expr = try store.addIntLiteral(1, SourceLoc.zero);
    const stmt1 = try store.addExprStmt(expr, SourceLoc.zero);
    const stmt2 = try store.addReturn(expr, SourceLoc.zero);

    const stmts = [_]StmtIdx{ stmt1, stmt2 };
    const block = try store.addBlock(&stmts, SourceLoc.zero);

    const view = BlockView.from(&store, block);
    try std.testing.expectEqual(@as(u32, 2), view.span.len);
    try std.testing.expect(!view.isEmpty());

    const stmt_indices = view.getStatements(&store);
    try std.testing.expectEqual(@as(usize, 2), stmt_indices.len);
}

test "IfStmtView" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    const cond = try store.addBoolLiteral(true, SourceLoc.zero);
    const then_expr = try store.addIntLiteral(1, SourceLoc.zero);
    const then_stmt = try store.addExprStmt(then_expr, SourceLoc.zero);
    const else_expr = try store.addIntLiteral(2, SourceLoc.zero);
    const else_stmt = try store.addExprStmt(else_expr, SourceLoc.zero);

    const if_stmt = try store.addIfStmt(cond, then_stmt, else_stmt, SourceLoc.zero);
    const view = IfStmtView.from(&store, if_stmt);

    try std.testing.expectEqual(cond, view.condition);
    try std.testing.expectEqual(then_stmt, view.then_body);
    try std.testing.expect(view.hasElse());
}

test "FnDefView" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    const fn_name = try interner.intern("add");
    const param_a = try interner.intern("a");
    const param_b = try interner.intern("b");

    const i32_type = try store.addPrimitiveType(.i32);

    // Params: [name, type, is_ref, default_value, name, type, is_ref, default_value]
    const params = [_]u32{
        @intFromEnum(param_a), i32_type.toInt(), 0, 0, // val, no default
        @intFromEnum(param_b), i32_type.toInt(), 0, 0, // val, no default
    };

    const body_expr = try store.addIntLiteral(0, SourceLoc.zero);
    const body = try store.addReturn(body_expr, SourceLoc.zero);

    const fn_def = try store.addFnDef(fn_name, &params, i32_type, body, SourceLoc.zero);
    const view = FnDefView.from(&store, fn_def);

    try std.testing.expect(StringInterner.eql(fn_name, view.name));
    try std.testing.expectEqual(@as(u32, 2), view.param_count);

    const p0 = view.getParam(&store, 0);
    try std.testing.expect(StringInterner.eql(param_a, p0.name));

    const p1 = view.getParam(&store, 1);
    try std.testing.expect(StringInterner.eql(param_b, p1.name));
}
