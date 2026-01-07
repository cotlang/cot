//! AST Node Store - Structure of Arrays Storage
//!
//! Central storage for all AST nodes using cache-friendly SoA layout.
//! All strings are stored via StringInterner (not owned by NodeStore).

const std = @import("std");
const base = @import("../base/mod.zig");
const StringInterner = base.StringInterner;
const StringId = base.StringId;

const indices = @import("indices.zig");
pub const StmtIdx = indices.StmtIdx;
pub const ExprIdx = indices.ExprIdx;
pub const TypeIdx = indices.TypeIdx;
pub const ExtraIdx = indices.ExtraIdx;
pub const ExtraSpan = indices.ExtraSpan;

const statements = @import("statements.zig");
pub const StatementTag = statements.StatementTag;

const expressions = @import("expressions.zig");
pub const ExpressionTag = expressions.ExpressionTag;
pub const BinaryOp = expressions.BinaryOp;
pub const UnaryOp = expressions.UnaryOp;
pub const InterpStringPartTag = expressions.InterpStringPartTag;

const types = @import("types.zig");
pub const TypeTag = types.TypeTag;
pub const Mutability = types.Mutability;

/// Source location for error reporting
pub const SourceLoc = packed struct {
    line: u24,
    column: u8,

    pub const zero: SourceLoc = .{ .line = 0, .column = 0 };

    pub fn init(line: u32, column: u32) SourceLoc {
        return .{
            .line = @intCast(@min(line, std.math.maxInt(u24))),
            .column = @intCast(@min(column, std.math.maxInt(u8))),
        };
    }
};

/// Node data - 12 bytes of packed data per node
/// Interpretation depends on the tag
pub const NodeData = packed struct {
    a: u32 = 0,
    b: u32 = 0,
    c: u32 = 0, // Used for DBL format flag (0 = Cot, 1 = DBL)

    pub const empty: NodeData = .{};

    /// Create data for binary expression: lhs, rhs, op
    pub fn binary(lhs: ExprIdx, rhs: ExprIdx, op: BinaryOp) NodeData {
        // Pack: a = lhs, b = (rhs << 8) | op
        return .{
            .a = lhs.toInt(),
            .b = (rhs.toInt() << 8) | @intFromEnum(op),
        };
    }

    /// Create data for unary expression: operand, op
    pub fn unary(operand: ExprIdx, op: UnaryOp) NodeData {
        return .{
            .a = operand.toInt(),
            .b = @intFromEnum(op),
        };
    }

    /// Create data for identifier: name (StringId)
    pub fn identifier(name: StringId) NodeData {
        return .{
            .a = @intFromEnum(name),
            .b = 0,
        };
    }

    /// Create data for int literal: value (up to 64 bits split)
    pub fn intLiteral(value: i64) NodeData {
        const u: u64 = @bitCast(value);
        return .{
            .a = @truncate(u),
            .b = @truncate(u >> 32),
        };
    }

    /// Create data for member access: object, field name
    pub fn member(object: ExprIdx, field: StringId) NodeData {
        return .{
            .a = object.toInt(),
            .b = @intFromEnum(field),
        };
    }

    /// Create data for index access: object, index
    pub fn index(object: ExprIdx, idx: ExprIdx) NodeData {
        return .{
            .a = object.toInt(),
            .b = idx.toInt(),
        };
    }

    /// Create data for is expression: expr, type
    pub fn isExpr(expr: ExprIdx, type_idx: TypeIdx) NodeData {
        return .{
            .a = expr.toInt(),
            .b = type_idx.toInt(),
        };
    }

    /// Create data for call: callee, args span start
    pub fn call(callee: ExprIdx, args_start: ExtraIdx) NodeData {
        return .{
            .a = callee.toInt(),
            .b = args_start.toInt(),
        };
    }

    /// Create data for assignment: target, value
    pub fn assignment(target: ExprIdx, value: ExprIdx) NodeData {
        return .{
            .a = target.toInt(),
            .b = value.toInt(),
        };
    }

    /// Create data for if statement: condition, then_body, else_body
    pub fn ifStmt(cond: ExprIdx, then_body: StmtIdx, else_body: StmtIdx) NodeData {
        // Pack condition in a, then/else indices in b via extra_data
        // For simplicity: a = cond, b = then_body (else in extra_data)
        _ = else_body;
        return .{
            .a = cond.toInt(),
            .b = then_body.toInt(),
        };
    }

    /// Create data for while loop: condition, body
    pub fn whileStmt(cond: ExprIdx, body: StmtIdx) NodeData {
        return .{
            .a = cond.toInt(),
            .b = body.toInt(),
        };
    }

    /// Create data for for loop: binding, iterable (body in extra_data)
    pub fn forStmt(binding: StringId, iterable: ExprIdx) NodeData {
        return .{
            .a = @intFromEnum(binding),
            .b = iterable.toInt(),
        };
    }

    /// Create data for return: value (or null)
    pub fn returnStmt(value: ExprIdx) NodeData {
        return .{
            .a = value.toInt(),
            .b = 0,
        };
    }

    /// Create data for let/const decl: name, type, init
    pub fn varDecl(name: StringId, type_idx: TypeIdx) NodeData {
        return .{
            .a = @intFromEnum(name),
            .b = type_idx.toInt(),
        };
    }

    /// Create data for function def: name, params span start
    pub fn fnDef(name: StringId, params_start: ExtraIdx) NodeData {
        return .{
            .a = @intFromEnum(name),
            .b = params_start.toInt(),
        };
    }

    /// Create data for block: statements span
    pub fn block(span: ExtraSpan) NodeData {
        return .{
            .a = span.start.toInt(),
            .b = span.len,
        };
    }

    /// Create data for expression statement: expr
    pub fn exprStmt(expr: ExprIdx) NodeData {
        return .{
            .a = expr.toInt(),
            .b = 0,
        };
    }

    // Extractors

    pub fn getLhs(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getRhs(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(@truncate(self.b >> 8));
    }

    pub fn getBinaryOp(self: NodeData) BinaryOp {
        return @enumFromInt(@as(u8, @truncate(self.b)));
    }

    pub fn getOperand(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getUnaryOp(self: NodeData) UnaryOp {
        return @enumFromInt(@as(u8, @truncate(self.b)));
    }

    pub fn getName(self: NodeData) StringId {
        return @enumFromInt(self.a);
    }

    pub fn getIntValue(self: NodeData) i64 {
        const u: u64 = (@as(u64, self.b) << 32) | self.a;
        return @bitCast(u);
    }

    pub fn getObject(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getField(self: NodeData) StringId {
        return @enumFromInt(self.b);
    }

    pub fn getIndex(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.b);
    }

    pub fn getCallee(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getArgsStart(self: NodeData) ExtraIdx {
        return ExtraIdx.fromInt(self.b);
    }

    pub fn getTarget(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getValue(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.b);
    }

    pub fn getCondition(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getThenBody(self: NodeData) StmtIdx {
        // then_body is stored in upper 16 bits of b (lower 16 bits is extra_data index for else)
        return StmtIdx.fromInt(self.b >> 16);
    }

    pub fn getBody(self: NodeData) StmtIdx {
        return StmtIdx.fromInt(self.b);
    }

    pub fn getBinding(self: NodeData) StringId {
        return @enumFromInt(self.a);
    }

    pub fn getIterable(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.b);
    }

    pub fn getReturnValue(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }

    pub fn getTypeIdx(self: NodeData) TypeIdx {
        return TypeIdx.fromInt(self.b);
    }

    pub fn getParamsStart(self: NodeData) ExtraIdx {
        return ExtraIdx.fromInt(self.b);
    }

    pub fn getSpan(self: NodeData) ExtraSpan {
        return .{
            .start = ExtraIdx.fromInt(self.a),
            .len = self.b,
        };
    }

    pub fn getExpr(self: NodeData) ExprIdx {
        return ExprIdx.fromInt(self.a);
    }
};

/// Central AST storage using Structure of Arrays
pub const NodeStore = struct {
    allocator: std.mem.Allocator,

    /// Reference to string interner (not owned)
    strings: *StringInterner,

    // Statement SoA arrays
    stmt_tags: std.ArrayListUnmanaged(StatementTag),
    stmt_locs: std.ArrayListUnmanaged(SourceLoc),
    stmt_data: std.ArrayListUnmanaged(NodeData),

    // Expression SoA arrays
    expr_tags: std.ArrayListUnmanaged(ExpressionTag),
    expr_locs: std.ArrayListUnmanaged(SourceLoc),
    expr_data: std.ArrayListUnmanaged(NodeData),

    // Type SoA arrays
    type_tags: std.ArrayListUnmanaged(TypeTag),
    type_data: std.ArrayListUnmanaged(NodeData),

    // Variable-length extra data (packed u32 values)
    extra_data: std.ArrayListUnmanaged(u32),

    // Scratch buffers for incremental parsing
    scratch_stmts: std.ArrayListUnmanaged(StmtIdx),
    scratch_exprs: std.ArrayListUnmanaged(ExprIdx),
    scratch_u32: std.ArrayListUnmanaged(u32),
    scratch_marks: std.ArrayListUnmanaged(ScratchMark),

    const Self = @This();

    const ScratchMark = struct {
        stmts: usize,
        exprs: usize,
        u32s: usize,
    };

    /// Initialize a new NodeStore
    pub fn init(allocator: std.mem.Allocator, strings: *StringInterner) Self {
        return .{
            .allocator = allocator,
            .strings = strings,
            .stmt_tags = .{},
            .stmt_locs = .{},
            .stmt_data = .{},
            .expr_tags = .{},
            .expr_locs = .{},
            .expr_data = .{},
            .type_tags = .{},
            .type_data = .{},
            .extra_data = .{},
            .scratch_stmts = .{},
            .scratch_exprs = .{},
            .scratch_u32 = .{},
            .scratch_marks = .{},
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *Self) void {
        self.stmt_tags.deinit(self.allocator);
        self.stmt_locs.deinit(self.allocator);
        self.stmt_data.deinit(self.allocator);
        self.expr_tags.deinit(self.allocator);
        self.expr_locs.deinit(self.allocator);
        self.expr_data.deinit(self.allocator);
        self.type_tags.deinit(self.allocator);
        self.type_data.deinit(self.allocator);
        self.extra_data.deinit(self.allocator);
        self.scratch_stmts.deinit(self.allocator);
        self.scratch_exprs.deinit(self.allocator);
        self.scratch_u32.deinit(self.allocator);
        self.scratch_marks.deinit(self.allocator);
    }

    // ========================================
    // Statement Accessors
    // ========================================

    pub fn stmtTag(self: *const Self, idx: StmtIdx) StatementTag {
        return self.stmt_tags.items[idx.toInt()];
    }

    pub fn stmtLoc(self: *const Self, idx: StmtIdx) SourceLoc {
        return self.stmt_locs.items[idx.toInt()];
    }

    pub fn stmtData(self: *const Self, idx: StmtIdx) NodeData {
        return self.stmt_data.items[idx.toInt()];
    }

    // ========================================
    // Expression Accessors
    // ========================================

    pub fn exprTag(self: *const Self, idx: ExprIdx) ExpressionTag {
        return self.expr_tags.items[idx.toInt()];
    }

    pub fn exprLoc(self: *const Self, idx: ExprIdx) SourceLoc {
        return self.expr_locs.items[idx.toInt()];
    }

    pub fn exprData(self: *const Self, idx: ExprIdx) NodeData {
        return self.expr_data.items[idx.toInt()];
    }

    // ========================================
    // Type Accessors
    // ========================================

    pub fn typeTag(self: *const Self, idx: TypeIdx) TypeTag {
        return self.type_tags.items[idx.toInt()];
    }

    pub fn typeData(self: *const Self, idx: TypeIdx) NodeData {
        return self.type_data.items[idx.toInt()];
    }

    // ========================================
    // Extra Data Accessors
    // ========================================

    pub fn getExtra(self: *const Self, idx: ExtraIdx) u32 {
        return self.extra_data.items[idx.toInt()];
    }

    pub fn getExtraSlice(self: *const Self, span: ExtraSpan) []const u32 {
        const start = span.start.toInt();
        return self.extra_data.items[start .. start + span.len];
    }

    /// Get statement indices from extra data span
    pub fn getStmtSpan(self: *const Self, span: ExtraSpan) []const u32 {
        return self.getExtraSlice(span);
    }

    /// Get expression indices from extra data span
    pub fn getExprSpan(self: *const Self, span: ExtraSpan) []const u32 {
        return self.getExtraSlice(span);
    }

    // ========================================
    // Expression Builders
    // ========================================

    /// Add an integer literal expression
    pub fn addIntLiteral(self: *Self, value: i64, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .int_literal);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.intLiteral(value));
        return idx;
    }

    /// Add a float literal expression
    pub fn addFloatLiteral(self: *Self, value: f64, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        const bits: u64 = @bitCast(value);
        try self.expr_tags.append(self.allocator, .float_literal);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, .{
            .a = @truncate(bits),
            .b = @truncate(bits >> 32),
        });
        return idx;
    }

    /// Add a string literal expression
    pub fn addStringLiteral(self: *Self, str_id: StringId, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .string_literal);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.identifier(str_id));
        return idx;
    }

    /// Add a boolean literal expression
    pub fn addBoolLiteral(self: *Self, value: bool, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .bool_literal);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, .{ .a = @intFromBool(value), .b = 0 });
        return idx;
    }

    /// Add a null literal expression
    pub fn addNullLiteral(self: *Self, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .null_literal);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.empty);
        return idx;
    }

    /// Add an identifier expression
    pub fn addIdentifier(self: *Self, name: StringId, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .identifier);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.identifier(name));
        return idx;
    }

    /// Add a binary expression
    pub fn addBinary(self: *Self, lhs: ExprIdx, op: BinaryOp, rhs: ExprIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .binary);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.binary(lhs, rhs, op));
        return idx;
    }

    /// Add a unary expression
    pub fn addUnary(self: *Self, op: UnaryOp, operand: ExprIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .unary);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.unary(operand, op));
        return idx;
    }

    /// Add a member access expression
    pub fn addMember(self: *Self, object: ExprIdx, field: StringId, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .member);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.member(object, field));
        return idx;
    }

    /// Add an optional member access expression (null-safe): expr?.field
    pub fn addOptionalMember(self: *Self, object: ExprIdx, field: StringId, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .optional_member);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.member(object, field));
        return idx;
    }

    /// Add an index expression
    pub fn addIndex(self: *Self, object: ExprIdx, index_expr: ExprIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .index);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.index(object, index_expr));
        return idx;
    }

    /// Add an optional index expression (null-safe): expr?[index]
    pub fn addOptionalIndex(self: *Self, object: ExprIdx, index_expr: ExprIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .optional_index);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.index(object, index_expr));
        return idx;
    }

    /// Add a slice expression: object[start..end] or object[start..=end]
    pub fn addSliceExpr(self: *Self, object: ExprIdx, start_expr: ExprIdx, end_expr: ExprIdx, inclusive: bool, loc: SourceLoc) !ExprIdx {
        // Store start, end, and inclusive flag in extra_data
        const extra_start: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        try self.extra_data.append(self.allocator, start_expr.toInt());
        try self.extra_data.append(self.allocator, end_expr.toInt());
        try self.extra_data.append(self.allocator, @intFromBool(inclusive));

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .slice_expr);
        try self.expr_locs.append(self.allocator, loc);
        // a = object, b = extra_data index for [start, end, inclusive]
        try self.expr_data.append(self.allocator, .{ .a = object.toInt(), .b = extra_start.toInt() });
        return idx;
    }

    /// Get slice expression parts
    pub fn getSliceExprParts(self: *const Self, expr_idx: ExprIdx) struct { object: ExprIdx, start: ExprIdx, end: ExprIdx, inclusive: bool } {
        const data = self.exprData(expr_idx);
        const object = ExprIdx.fromInt(data.a);
        const extra_start = data.b;
        const start = ExprIdx.fromInt(self.extra_data.items[extra_start]);
        const end = ExprIdx.fromInt(self.extra_data.items[extra_start + 1]);
        const inclusive = self.extra_data.items[extra_start + 2] != 0;
        return .{ .object = object, .start = start, .end = end, .inclusive = inclusive };
    }

    /// Add a type test expression (expr is Type)
    pub fn addIsExpr(self: *Self, expr: ExprIdx, type_idx: TypeIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .is_expr);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, NodeData.isExpr(expr, type_idx));
        return idx;
    }

    /// Add a call expression
    pub fn addCall(self: *Self, callee: ExprIdx, args: []const ExprIdx, loc: SourceLoc) !ExprIdx {
        // Store args in extra_data
        const args_start = try self.addExtraSlice(args);

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .call);
        try self.expr_locs.append(self.allocator, loc);
        // Pack: callee in a, args count in b high bits, args start in extra
        try self.expr_data.append(self.allocator, .{
            .a = callee.toInt(),
            .b = (@as(u32, @intCast(args.len)) << 16) | args_start.toInt(),
        });
        return idx;
    }

    /// Add a grouping/parenthesized expression
    pub fn addGrouping(self: *Self, inner: ExprIdx, loc: SourceLoc) !ExprIdx {
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .grouping);
        try self.expr_locs.append(self.allocator, loc);
        try self.expr_data.append(self.allocator, .{ .a = inner.toInt(), .b = 0 });
        return idx;
    }

    /// Add a comptime builtin expression: @name() or @name(args)
    pub fn addComptimeBuiltin(self: *Self, name: StringId, args: []const ExprIdx, loc: SourceLoc) !ExprIdx {
        // Store args in extra_data if present
        const args_start: ExtraIdx = if (args.len > 0) try self.addExtraSlice(args) else ExtraIdx.fromInt(0);

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .comptime_builtin);
        try self.expr_locs.append(self.allocator, loc);
        // Pack: name in a, args count in b high bits, args start in b low bits
        try self.expr_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = (@as(u32, @intCast(args.len)) << 16) | args_start.toInt(),
        });
        return idx;
    }

    /// Add an interpolated string expression: "Hello ${name}!"
    /// parts_data format: alternating [tag, data] pairs where:
    ///   - tag 0 (string_content): data is StringId
    ///   - tag 1 (expression): data is ExprIdx
    pub fn addInterpString(self: *Self, parts_data: []const u32, loc: SourceLoc) !ExprIdx {
        // Store parts in extra_data: [count, tag0, data0, tag1, data1, ...]
        const parts_start: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        const part_count: u32 = @intCast(parts_data.len / 2);
        try self.extra_data.append(self.allocator, part_count);
        for (parts_data) |p| {
            try self.extra_data.append(self.allocator, p);
        }

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.expr_tags.items.len)));
        try self.expr_tags.append(self.allocator, .interp_string);
        try self.expr_locs.append(self.allocator, loc);
        // Pack: parts_start in a, reserved in b
        try self.expr_data.append(self.allocator, .{
            .a = parts_start.toInt(),
            .b = 0,
        });
        return idx;
    }

    /// Get interpolated string parts data
    /// Returns: [count, tag0, data0, tag1, data1, ...] where count is number of parts
    pub fn getInterpStringParts(self: *const Self, idx: ExprIdx) struct { count: u32, data: []const u32 } {
        const data = self.exprData(idx);
        const parts_start = data.a;
        const count = self.extra_data.items[parts_start];
        // Each part has 2 values (tag + data)
        const total_values = count * 2;
        return .{
            .count = count,
            .data = self.extra_data.items[parts_start + 1 .. parts_start + 1 + total_values],
        };
    }

    // ========================================
    // Statement Builders
    // ========================================

    /// Add an expression statement
    pub fn addExprStmt(self: *Self, expr: ExprIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .expression);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.exprStmt(expr));
        return idx;
    }

    /// Add an assignment statement
    pub fn addAssignment(self: *Self, target: ExprIdx, value: ExprIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .assignment);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.assignment(target, value));
        return idx;
    }

    /// Add a let declaration
    pub fn addLetDecl(self: *Self, name: StringId, type_idx: TypeIdx, init_expr: ExprIdx, is_mut: bool, loc: SourceLoc) !StmtIdx {
        // Store init expr in extra_data along with mutability flag
        const extra_start = try self.addExtra(init_expr.toInt());
        _ = try self.addExtra(@intFromBool(is_mut));

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .let_decl);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = (type_idx.toInt() << 16) | extra_start.toInt(),
        });
        return idx;
    }

    /// Add a const declaration
    pub fn addConstDecl(self: *Self, name: StringId, type_idx: TypeIdx, init_expr: ExprIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .const_decl);
        try self.stmt_locs.append(self.allocator, loc);
        // Pack: name in a, type and init packed in b + extra
        const extra_start = try self.addExtra(init_expr.toInt());
        try self.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = (type_idx.toInt() << 16) | extra_start.toInt(),
        });
        return idx;
    }

    /// Add a return statement
    pub fn addReturn(self: *Self, value: ExprIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .return_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.returnStmt(value));
        return idx;
    }

    /// Add a break statement
    pub fn addBreak(self: *Self, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .break_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.empty);
        return idx;
    }

    /// Add a continue statement
    pub fn addContinue(self: *Self, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .continue_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.empty);
        return idx;
    }

    /// Add a try-catch statement
    /// try_body is the block to try, catch_body is the handler
    /// err_binding is optional error variable name (null_id if not bound)
    /// finally_body is optional cleanup block (null if no finally)
    pub fn addTryStmt(self: *Self, try_body: StmtIdx, err_binding: StringId, catch_body: StmtIdx, finally_body: StmtIdx, loc: SourceLoc) !StmtIdx {
        // Store err_binding, catch_body and finally_body in extra_data
        const extra_start = try self.addExtra(@intFromEnum(err_binding));
        _ = try self.addExtra(catch_body.toInt());
        _ = try self.addExtra(finally_body.toInt());

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .try_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = try_body.toInt(),
            .b = extra_start.toInt(),
        });
        return idx;
    }

    /// Add a throw statement
    pub fn addThrowStmt(self: *Self, value: ExprIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .throw_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = value.toInt(),
            .b = 0,
        });
        return idx;
    }

    /// Add a defer statement
    /// The body is a block or expression that executes at scope exit
    pub fn addDeferStmt(self: *Self, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .defer_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = body.toInt(),
            .b = 0,
        });
        return idx;
    }

    /// Add a block statement
    pub fn addBlock(self: *Self, stmts: []const StmtIdx, loc: SourceLoc) !StmtIdx {
        const span = try self.addStmtSpan(stmts);
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .block);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.block(span));
        return idx;
    }

    /// Add a record block (like block but doesn't introduce a new scope - for DBL records)
    pub fn addRecordBlock(self: *Self, stmts: []const StmtIdx, loc: SourceLoc) !StmtIdx {
        const span = try self.addStmtSpan(stmts);
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .record_block);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.block(span));
        return idx;
    }

    /// Add an if statement
    pub fn addIfStmt(self: *Self, cond: ExprIdx, then_body: StmtIdx, else_body: StmtIdx, loc: SourceLoc) !StmtIdx {
        // Store else_body in extra_data
        const extra_start = try self.addExtra(else_body.toInt());

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .if_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = cond.toInt(),
            .b = (then_body.toInt() << 16) | extra_start.toInt(),
        });
        return idx;
    }

    /// Add a while statement
    pub fn addWhileStmt(self: *Self, cond: ExprIdx, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .while_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.whileStmt(cond, body));
        return idx;
    }

    /// Add a for statement
    pub fn addForStmt(self: *Self, binding: StringId, iterable: ExprIdx, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        // Store body in extra_data
        const extra_start = try self.addExtra(body.toInt());

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .for_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(binding),
            .b = (iterable.toInt() << 16) | extra_start.toInt(),
        });
        return idx;
    }

    /// Add a loop statement (infinite loop)
    pub fn addLoopStmt(self: *Self, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .loop_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{ .a = body.toInt(), .b = 0 });
        return idx;
    }

    /// Add a function definition
    /// params format: [name, type, is_ref, default_value, ...] (4 values per param)
    /// type_params format: [type_param_name_id, ...] (StringIds for each type param like T, U)
    pub fn addFnDef(self: *Self, name: StringId, params: []const u32, return_type: TypeIdx, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        // Non-generic function - delegate to generic version with empty type params
        return self.addGenericFnDef(name, &.{}, params, return_type, body, loc);
    }

    /// Add a generic function definition
    /// type_params: pairs of [name, bound] for each type parameter (e.g., T, Trait or T, null)
    /// params format: [name, type, is_ref, default_value, ...] (4 values per param)
    pub fn addGenericFnDef(self: *Self, name: StringId, type_params: []const u32, params: []const u32, return_type: TypeIdx, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        // Store as: [param_count, type_param_count, tp1_name, tp1_bound, tp2_name, tp2_bound, ..., param1_name, param1_type, param1_is_ref, param1_default, ..., return_type, body]
        const params_start: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        try self.extra_data.append(self.allocator, @intCast(params.len / 4)); // param count (4 values per param)
        try self.extra_data.append(self.allocator, @intCast(type_params.len / 2)); // type param count (2 values per type param: name, bound)
        // Store type parameter (name, bound) pairs
        for (type_params) |tp| {
            try self.extra_data.append(self.allocator, tp);
        }
        // Store regular parameters
        for (params) |p| {
            try self.extra_data.append(self.allocator, p);
        }
        // Store return type and body
        try self.extra_data.append(self.allocator, return_type.toInt());
        try self.extra_data.append(self.allocator, body.toInt());

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .fn_def);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.fnDef(name, params_start));
        return idx;
    }

    /// Add an import statement
    pub fn addImport(self: *Self, module_path: StringId, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .import_stmt);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, NodeData.identifier(module_path));
        return idx;
    }

    /// Add a test definition
    /// data.a = name StringId
    /// data.b = body StmtIdx (block statement)
    pub fn addTestDef(self: *Self, name: StringId, body: StmtIdx, loc: SourceLoc) !StmtIdx {
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .test_def);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = body.toInt(),
        });
        return idx;
    }

    /// Get test definition data
    pub fn getTestDef(self: *const Self, idx: StmtIdx) struct { name: StringId, body: StmtIdx } {
        const data = self.stmtData(idx);
        return .{
            .name = @enumFromInt(data.a),
            .body = StmtIdx.fromInt(data.b),
        };
    }

    /// Add a field view (overlay) statement
    /// Used in records for fields that share memory with other fields
    /// base_field is the field name to overlay, offset is the byte offset from that field
    pub fn addFieldView(self: *Self, name: StringId, type_idx: TypeIdx, base_field: StringId, offset: i32, loc: SourceLoc) !StmtIdx {
        // Store base_field and offset in extra_data
        const extra_start = try self.addExtra(@intFromEnum(base_field));
        _ = try self.addExtra(@bitCast(offset));

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.stmt_tags.items.len)));
        try self.stmt_tags.append(self.allocator, .field_view);
        try self.stmt_locs.append(self.allocator, loc);
        try self.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = (type_idx.toInt() << 16) | extra_start.toInt(),
        });
        return idx;
    }

    // ========================================
    // Type Builders
    // ========================================

    /// Add a primitive type
    pub fn addPrimitiveType(self: *Self, tag: TypeTag) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, tag);
        try self.type_data.append(self.allocator, NodeData.empty);
        return idx;
    }

    /// Add a decimal type (DBL decimal type like d6)
    /// precision is the number of digits, scale is decimal places (0 for integers)
    pub fn addDecimalType(self: *Self, precision: u32, scale: u32) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .decimal);
        try self.type_data.append(self.allocator, .{ .a = precision, .b = scale });
        return idx;
    }

    /// Add a named type reference
    pub fn addNamedType(self: *Self, name: StringId) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .named);
        try self.type_data.append(self.allocator, NodeData.identifier(name));
        return idx;
    }

    /// Add a Self type (used for untyped 'self' parameter in methods)
    /// This creates a named type "Self" which gets resolved during type checking
    pub fn addSelfType(self: *Self) !TypeIdx {
        const name = try self.strings.intern("Self");
        return self.addNamedType(name);
    }

    /// Add an array type
    pub fn addArrayType(self: *Self, elem_type: TypeIdx, size: u32) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .array);
        try self.type_data.append(self.allocator, .{ .a = elem_type.toInt(), .b = size });
        return idx;
    }

    /// Add a slice type
    pub fn addSliceType(self: *Self, elem_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .slice);
        try self.type_data.append(self.allocator, .{ .a = elem_type.toInt(), .b = 0 });
        return idx;
    }

    /// Add an optional type
    pub fn addOptionalType(self: *Self, inner_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .optional);
        try self.type_data.append(self.allocator, .{ .a = inner_type.toInt(), .b = 0 });
        return idx;
    }

    /// Add a weak reference type: weak T
    /// Weak references don't keep the referenced value alive (for breaking cycles)
    pub fn addWeakType(self: *Self, inner_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .weak);
        try self.type_data.append(self.allocator, .{ .a = inner_type.toInt(), .b = 0 });
        return idx;
    }

    /// Add an error union type: T!E (value_type!error_type)
    pub fn addErrorUnionType(self: *Self, value_type: TypeIdx, error_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .error_union);
        try self.type_data.append(self.allocator, .{ .a = value_type.toInt(), .b = error_type.toInt() });
        return idx;
    }

    /// Add a map type: Map<K, V>
    pub fn addMapType(self: *Self, key_type: TypeIdx, value_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .map);
        try self.type_data.append(self.allocator, .{ .a = key_type.toInt(), .b = value_type.toInt() });
        return idx;
    }

    /// Add a list type: List<T>
    pub fn addListType(self: *Self, element_type: TypeIdx) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .list);
        try self.type_data.append(self.allocator, .{ .a = element_type.toInt(), .b = 0 });
        return idx;
    }

    /// Add an associated type reference: Self.Item or T.Item
    pub fn addAssociatedType(self: *Self, base_type: TypeIdx, assoc_name: StringId) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .associated_type);
        try self.type_data.append(self.allocator, .{ .a = base_type.toInt(), .b = @intFromEnum(assoc_name) });
        return idx;
    }

    /// Add a trait object type: dyn Trait
    pub fn addTraitObjectType(self: *Self, trait_name: StringId) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .trait_object);
        try self.type_data.append(self.allocator, .{ .a = @intFromEnum(trait_name), .b = 0 });
        return idx;
    }

    /// Add a pointer type
    pub fn addPointerType(self: *Self, pointee: TypeIdx, mutability: Mutability) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .pointer);
        try self.type_data.append(self.allocator, .{ .a = pointee.toInt(), .b = @intFromEnum(mutability) });
        return idx;
    }

    /// Add a type parameter reference (e.g., T in fn foo<T>)
    /// index: the position of this type param in the enclosing generic context (0 for first, 1 for second, etc.)
    pub fn addTypeParam(self: *Self, name: StringId, index: u16) !TypeIdx {
        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .type_param);
        try self.type_data.append(self.allocator, .{ .a = @intFromEnum(name), .b = index });
        return idx;
    }

    /// Add a generic instantiation (e.g., Vec<i32>)
    /// base_type: the generic type being instantiated
    /// type_args: the type arguments
    pub fn addGenericInstance(self: *Self, base_type: TypeIdx, type_args: []const TypeIdx) !TypeIdx {
        // Store type args in extra_data: [count, arg0, arg1, ...]
        const args_start: u32 = @intCast(self.extra_data.items.len);
        try self.extra_data.append(self.allocator, @intCast(type_args.len));
        for (type_args) |arg| {
            try self.extra_data.append(self.allocator, arg.toInt());
        }

        const idx: TypeIdx = @enumFromInt(@as(u32, @intCast(self.type_tags.items.len)));
        try self.type_tags.append(self.allocator, .generic_instance);
        // Pack: base type in a, args_start in b
        try self.type_data.append(self.allocator, .{ .a = base_type.toInt(), .b = args_start });
        return idx;
    }

    // ========================================
    // Extra Data Helpers
    // ========================================

    /// Add a single u32 to extra_data
    fn addExtra(self: *Self, value: u32) !ExtraIdx {
        const idx: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        try self.extra_data.append(self.allocator, value);
        return idx;
    }

    /// Add expression indices as u32 slice to extra_data
    fn addExtraSlice(self: *Self, exprs: []const ExprIdx) !ExtraIdx {
        const idx: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        for (exprs) |expr| {
            try self.extra_data.append(self.allocator, expr.toInt());
        }
        return idx;
    }

    /// Add statement indices to extra_data, returning span
    fn addStmtSpan(self: *Self, stmts: []const StmtIdx) !ExtraSpan {
        const start: ExtraIdx = @enumFromInt(@as(u32, @intCast(self.extra_data.items.len)));
        for (stmts) |stmt| {
            try self.extra_data.append(self.allocator, stmt.toInt());
        }
        return .{ .start = start, .len = @intCast(stmts.len) };
    }

    // ========================================
    // Scratch Buffer Operations
    // ========================================

    /// Mark current scratch positions for potential rollback
    pub fn markScratch(self: *Self) !void {
        try self.scratch_marks.append(self.allocator, .{
            .stmts = self.scratch_stmts.items.len,
            .exprs = self.scratch_exprs.items.len,
            .u32s = self.scratch_u32.items.len,
        });
    }

    /// Rollback scratch buffers to last mark
    pub fn rollbackScratch(self: *Self) void {
        if (self.scratch_marks.items.len > 0) {
            const mark = self.scratch_marks.pop().?;
            self.scratch_stmts.shrinkRetainingCapacity(mark.stmts);
            self.scratch_exprs.shrinkRetainingCapacity(mark.exprs);
            self.scratch_u32.shrinkRetainingCapacity(mark.u32s);
        }
    }

    /// Commit scratch and remove mark (success case)
    /// Also shrinks scratch buffers back to mark position to prevent
    /// nested block statements from leaking into parent blocks
    pub fn commitScratch(self: *Self) void {
        if (self.scratch_marks.items.len > 0) {
            const mark = self.scratch_marks.pop().?;
            // Shrink back to mark position - the caller has already copied
            // the statements they need, so we can safely remove them
            self.scratch_stmts.shrinkRetainingCapacity(mark.stmts);
            self.scratch_exprs.shrinkRetainingCapacity(mark.exprs);
            self.scratch_u32.shrinkRetainingCapacity(mark.u32s);
        }
    }

    /// Push a statement to scratch buffer
    pub fn pushScratchStmt(self: *Self, stmt: StmtIdx) !void {
        try self.scratch_stmts.append(self.allocator, stmt);
    }

    /// Push an expression to scratch buffer
    pub fn pushScratchExpr(self: *Self, expr: ExprIdx) !void {
        try self.scratch_exprs.append(self.allocator, expr);
    }

    /// Push a u32 to scratch buffer
    pub fn pushScratchU32(self: *Self, value: u32) !void {
        try self.scratch_u32.append(self.allocator, value);
    }

    /// Get scratch statements since last mark
    pub fn getScratchStmts(self: *const Self) []const StmtIdx {
        const mark_pos = if (self.scratch_marks.items.len > 0)
            self.scratch_marks.items[self.scratch_marks.items.len - 1].stmts
        else
            0;
        return self.scratch_stmts.items[mark_pos..];
    }

    /// Get scratch expressions since last mark
    pub fn getScratchExprs(self: *const Self) []const ExprIdx {
        const mark_pos = if (self.scratch_marks.items.len > 0)
            self.scratch_marks.items[self.scratch_marks.items.len - 1].exprs
        else
            0;
        return self.scratch_exprs.items[mark_pos..];
    }

    /// Get scratch u32s since last mark
    pub fn getScratchU32s(self: *const Self) []const u32 {
        const mark_pos = if (self.scratch_marks.items.len > 0)
            self.scratch_marks.items[self.scratch_marks.items.len - 1].u32s
        else
            0;
        return self.scratch_u32.items[mark_pos..];
    }

    /// Clear all scratch buffers
    pub fn clearScratch(self: *Self) void {
        self.scratch_stmts.clearRetainingCapacity();
        self.scratch_exprs.clearRetainingCapacity();
        self.scratch_u32.clearRetainingCapacity();
        self.scratch_marks.clearRetainingCapacity();
    }

    // ========================================
    // Utility
    // ========================================

    /// Get total number of statements
    pub fn stmtCount(self: *const Self) usize {
        return self.stmt_tags.items.len;
    }

    /// Get total number of expressions
    pub fn exprCount(self: *const Self) usize {
        return self.expr_tags.items.len;
    }

    /// Get total number of types
    pub fn typeCount(self: *const Self) usize {
        return self.type_tags.items.len;
    }
};

// ============================================================
// Tests
// ============================================================

test "NodeStore basic expressions" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Add integer literal
    const int_idx = try store.addIntLiteral(42, SourceLoc.zero);
    try std.testing.expectEqual(ExpressionTag.int_literal, store.exprTag(int_idx));
    try std.testing.expectEqual(@as(i64, 42), store.exprData(int_idx).getIntValue());

    // Add identifier
    const name = try interner.intern("foo");
    const id_idx = try store.addIdentifier(name, SourceLoc.zero);
    try std.testing.expectEqual(ExpressionTag.identifier, store.exprTag(id_idx));
    try std.testing.expect(StringInterner.eql(name, store.exprData(id_idx).getName()));

    // Add binary expression
    const bin_idx = try store.addBinary(int_idx, .add, id_idx, SourceLoc.zero);
    try std.testing.expectEqual(ExpressionTag.binary, store.exprTag(bin_idx));
    try std.testing.expectEqual(BinaryOp.add, store.exprData(bin_idx).getBinaryOp());
}

test "NodeStore basic statements" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Add expression statement
    const expr = try store.addIntLiteral(123, SourceLoc.zero);
    const stmt = try store.addExprStmt(expr, SourceLoc.zero);
    try std.testing.expectEqual(StatementTag.expression, store.stmtTag(stmt));

    // Add return statement
    const ret = try store.addReturn(expr, SourceLoc.zero);
    try std.testing.expectEqual(StatementTag.return_stmt, store.stmtTag(ret));
}

test "NodeStore scratch buffers" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Mark, add, rollback
    try store.markScratch();
    const expr1 = try store.addIntLiteral(1, SourceLoc.zero);
    try store.pushScratchExpr(expr1);
    try std.testing.expectEqual(@as(usize, 1), store.getScratchExprs().len);
    store.rollbackScratch();
    try std.testing.expectEqual(@as(usize, 0), store.getScratchExprs().len);

    // Mark, add, commit
    try store.markScratch();
    const expr2 = try store.addIntLiteral(2, SourceLoc.zero);
    try store.pushScratchExpr(expr2);
    store.commitScratch();
    // After commit, scratch still has items but no mark
    try std.testing.expectEqual(@as(usize, 1), store.scratch_exprs.items.len);
}

test "NodeStore block statement" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    var store = NodeStore.init(std.testing.allocator, &interner);
    defer store.deinit();

    // Create some statements
    const expr1 = try store.addIntLiteral(1, SourceLoc.zero);
    const stmt1 = try store.addExprStmt(expr1, SourceLoc.zero);
    const expr2 = try store.addIntLiteral(2, SourceLoc.zero);
    const stmt2 = try store.addExprStmt(expr2, SourceLoc.zero);

    // Create block
    const stmts = [_]StmtIdx{ stmt1, stmt2 };
    const block = try store.addBlock(&stmts, SourceLoc.zero);

    try std.testing.expectEqual(StatementTag.block, store.stmtTag(block));
    const span = store.stmtData(block).getSpan();
    try std.testing.expectEqual(@as(u32, 2), span.len);
}
