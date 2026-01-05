//! Cot Source Code Emitter
//!
//! Converts an AST (NodeStore) back into Cot source code.
//! Used for DBL → Cot conversion and code formatting.
//!
//! ## Usage
//!
//! ```zig
//! var buf = std.ArrayList(u8).init(allocator);
//! defer buf.deinit();
//!
//! var emitter = Emitter.init(buf.writer().any(), store, strings, .{});
//! try emitter.emitProgram(statements);
//!
//! const source = buf.items; // Cot source code
//! ```

const std = @import("std");
const ast = @import("mod.zig");
const base = @import("../base/mod.zig");

const NodeStore = ast.NodeStore;
const StringInterner = base.StringInterner;
const StringId = base.StringId;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const TypeIdx = ast.TypeIdx;
const StatementTag = ast.StatementTag;
const ExpressionTag = ast.ExpressionTag;
const TypeTag = ast.TypeTag;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;
const SourceLoc = ast.SourceLoc;

// Views for structured access
const BinaryExprView = ast.BinaryExprView;
const UnaryExprView = ast.UnaryExprView;
const IdentifierView = ast.IdentifierView;
const IntLiteralView = ast.IntLiteralView;
const FloatLiteralView = ast.FloatLiteralView;
const StringLiteralView = ast.StringLiteralView;
const BoolLiteralView = ast.BoolLiteralView;
const MemberView = ast.MemberView;
const IndexView = ast.IndexView;
const CallView = ast.CallView;
const GroupingView = ast.GroupingView;
const ExprStmtView = ast.ExprStmtView;
const AssignmentView = ast.AssignmentView;
const LetDeclView = ast.LetDeclView;
const ConstDeclView = ast.ConstDeclView;
const ReturnView = ast.ReturnView;
const BlockView = ast.BlockView;
const IfStmtView = ast.IfStmtView;
const WhileStmtView = ast.WhileStmtView;
const ForStmtView = ast.ForStmtView;
const LoopStmtView = ast.LoopStmtView;
const FnDefView = ast.FnDefView;
const ImportView = ast.ImportView;
const ArrayTypeView = ast.ArrayTypeView;
const SliceTypeView = ast.SliceTypeView;
const OptionalTypeView = ast.OptionalTypeView;
const PointerTypeView = ast.PointerTypeView;
const NamedTypeView = ast.NamedTypeView;

pub const EmitError = error{
    OutOfMemory,
    NoSpaceLeft,
    DiskQuota,
    InputOutput,
    BrokenPipe,
    InvalidArgument,
    SystemResources,
    OperationAborted,
    LockViolation,
    Unexpected,
    NotOpenForWriting,
    ConnectionResetByPeer,
    WouldBlock,
    DeviceBusy,
};

pub const Emitter = struct {
    writer: std.io.AnyWriter,
    store: *const NodeStore,
    strings: *const StringInterner,
    options: Options,
    indent_level: u32,
    at_line_start: bool,
    needs_newline: bool,

    const Self = @This();

    pub const Options = struct {
        /// Use modern operators (== instead of .eq.)
        modern_operators: bool = true,
        /// Indent size in spaces
        indent_size: u32 = 4,
        /// Add blank lines between top-level items
        blank_lines: bool = true,
    };

    pub fn init(
        writer: std.io.AnyWriter,
        store: *const NodeStore,
        strings: *const StringInterner,
        options: Options,
    ) Self {
        return .{
            .writer = writer,
            .store = store,
            .strings = strings,
            .options = options,
            .indent_level = 0,
            .at_line_start = true,
            .needs_newline = false,
        };
    }

    // ========================================
    // Public API
    // ========================================

    /// Emit all top-level statements
    pub fn emitProgram(self: *Self, stmts: []const StmtIdx) anyerror!void {
        var first = true;
        for (stmts) |stmt_idx| {
            // Add blank line between top-level items
            if (!first and self.options.blank_lines) {
                const tag = self.store.stmtTag(stmt_idx);
                if (tag == .fn_def or tag == .struct_def or tag == .union_def or tag == .enum_def) {
                    try self.writer.writeByte('\n');
                }
            }
            first = false;
            try self.emitStatement(stmt_idx);
        }
    }

    /// Emit a single statement
    pub fn emitStatement(self: *Self, idx: StmtIdx) anyerror!void {
        const tag = self.store.stmtTag(idx);

        switch (tag) {
            .fn_def => try self.emitFnDef(idx),
            .struct_def => try self.emitStructDef(idx),
            .union_def => try self.emitUnionDef(idx),
            .field_view => try self.emitFieldView(idx),
            .enum_def => try self.emitEnumDef(idx),
            .trait_def => try self.emitTraitDef(idx),
            .impl_block => try self.emitImplBlock(idx),
            .const_decl => try self.emitConstDecl(idx),
            .let_decl => try self.emitLetDecl(idx),
            .type_alias => try self.emitTypeAlias(idx),
            .if_stmt => try self.emitIfStmt(idx),
            .match_stmt => try self.emitMatchStmt(idx),
            .for_stmt => try self.emitForStmt(idx),
            .while_stmt => try self.emitWhileStmt(idx),
            .loop_stmt => try self.emitLoopStmt(idx),
            .return_stmt => try self.emitReturnStmt(idx),
            .break_stmt => try self.emitBreakStmt(idx),
            .continue_stmt => try self.emitContinueStmt(idx),
            .try_stmt => try self.emitTryStmt(idx),
            .throw_stmt => try self.emitThrowStmt(idx),
            .defer_stmt => try self.emitDeferStmt(idx),
            .assignment => try self.emitAssignment(idx),
            .expression => try self.emitExprStmt(idx),
            .block => try self.emitBlock(idx),
            .import_stmt => try self.emitImportStmt(idx),
            .io_open => try self.emitIoOpen(idx),
            .io_close => try self.emitIoClose(idx),
            .io_read => try self.emitIoRead(idx),
            .io_write => try self.emitIoWrite(idx),
            .io_store => try self.emitIoStore(idx),
            .io_delete => try self.emitIoDelete(idx),
            .comptime_if => try self.emitComptimeIf(idx),
            .comptime_block => try self.emitComptimeBlock(idx),
            .test_def => try self.emitTestDef(idx),
        }
    }

    /// Emit an expression
    pub fn emitExpression(self: *Self, idx: ExprIdx) anyerror!void {
        if (idx.isNull()) return;

        const tag = self.store.exprTag(idx);

        switch (tag) {
            .int_literal => try self.emitIntLiteral(idx),
            .float_literal => try self.emitFloatLiteral(idx),
            .string_literal => try self.emitStringLiteral(idx),
            .bool_literal => try self.emitBoolLiteral(idx),
            .null_literal => try self.writer.writeAll("null"),
            .identifier => try self.emitIdentifier(idx),
            .member => try self.emitMember(idx),
            .index => try self.emitIndex(idx),
            .binary => try self.emitBinary(idx),
            .unary => try self.emitUnary(idx),
            .call => try self.emitCall(idx),
            .method_call => try self.emitMethodCall(idx),
            .range => try self.emitRange(idx),
            .array_init => try self.emitArrayInit(idx),
            .struct_init => try self.emitStructInit(idx),
            .lambda => try self.emitLambda(idx),
            .comptime_builtin => try self.emitComptimeBuiltin(idx),
            .grouping => try self.emitGrouping(idx),
            .if_expr => try self.emitIfExpr(idx),
            .match_expr => try self.emitMatchExpr(idx),
            .block_expr => try self.emitBlockExpr(idx),
            .optional_member => try self.emitOptionalMember(idx),
            .optional_index => try self.emitOptionalIndex(idx),
            .is_expr => try self.emitIsExpr(idx),
        }
    }

    /// Emit a type annotation
    pub fn emitType(self: *Self, idx: TypeIdx) anyerror!void {
        if (idx.isNull()) return;

        const tag = self.store.typeTag(idx);

        switch (tag) {
            .i8 => try self.writer.writeAll("i8"),
            .i16 => try self.writer.writeAll("i16"),
            .i32 => try self.writer.writeAll("i32"),
            .i64 => try self.writer.writeAll("i64"),
            .u8 => try self.writer.writeAll("u8"),
            .u16 => try self.writer.writeAll("u16"),
            .u32 => try self.writer.writeAll("u32"),
            .u64 => try self.writer.writeAll("u64"),
            .isize => try self.writer.writeAll("isize"),
            .usize => try self.writer.writeAll("usize"),
            .f32 => try self.writer.writeAll("f32"),
            .f64 => try self.writer.writeAll("f64"),
            .bool => try self.writer.writeAll("bool"),
            .void => try self.writer.writeAll("void"),
            .string => try self.writer.writeAll("string"),
            .decimal => {
                const data = self.store.typeData(idx);
                const precision = data.a;
                const scale = data.b;
                if (scale > 0) {
                    try self.writer.print("d{d}.{d}", .{ precision, scale });
                } else {
                    try self.writer.print("d{d}", .{precision});
                }
            },
            .array => {
                const view = ArrayTypeView.from(self.store, idx);
                try self.writer.print("[{d}]", .{view.size});
                try self.emitType(view.elem_type);
            },
            .slice => {
                const view = SliceTypeView.from(self.store, idx);
                try self.writer.writeAll("[]");
                try self.emitType(view.elem_type);
            },
            .optional => {
                const view = OptionalTypeView.from(self.store, idx);
                try self.writer.writeByte('?');
                try self.emitType(view.inner_type);
            },
            .weak => {
                // Weak reference type - uses same layout as optional (inner type in data.a)
                const data = self.store.typeData(idx);
                const inner_type = TypeIdx.fromInt(data.a);
                try self.writer.writeAll("weak ");
                try self.emitType(inner_type);
            },
            .pointer => {
                const view = PointerTypeView.from(self.store, idx);
                if (view.is_const) {
                    try self.writer.writeAll("*const ");
                } else {
                    try self.writer.writeByte('*');
                }
                try self.emitType(view.pointee);
            },
            .named => {
                const view = NamedTypeView.from(self.store, idx);
                try self.writeStringId(view.name);
            },
            .type_param => {
                // Type parameter reference (e.g., T)
                const data = self.store.typeData(idx);
                const name: StringId = @enumFromInt(data.a);
                try self.writeStringId(name);
            },
            .generic_instance => {
                // Generic instantiation (e.g., Vec<i32>)
                const data = self.store.typeData(idx);
                const base_type = TypeIdx.fromInt(data.a);
                const args_start = data.b;

                // Emit base type
                try self.emitType(base_type);

                // Emit type arguments
                try self.writer.writeByte('<');
                const count = self.store.extra_data.items[args_start];
                for (0..count) |i| {
                    if (i > 0) try self.writer.writeAll(", ");
                    const arg_type = TypeIdx.fromInt(self.store.extra_data.items[args_start + 1 + i]);
                    try self.emitType(arg_type);
                }
                try self.writer.writeByte('>');
            },
            .@"union" => try self.writer.writeAll("union"),
            .function => try self.writer.writeAll("fn"),
            .tuple => try self.writer.writeAll("tuple"),
            .error_union => {
                // Emit as Result<ValueType, ErrorType>
                const data = self.store.typeData(idx);
                const value_type = TypeIdx.fromInt(data.a);
                const error_type = TypeIdx.fromInt(data.b);
                try self.writer.writeAll("Result<");
                try self.emitType(value_type);
                try self.writer.writeAll(", ");
                try self.emitType(error_type);
                try self.writer.writeByte('>');
            },
            .map => {
                // Emit as Map<KeyType, ValueType>
                const data = self.store.typeData(idx);
                const key_type = TypeIdx.fromInt(data.a);
                const value_type = TypeIdx.fromInt(data.b);
                try self.writer.writeAll("Map<");
                try self.emitType(key_type);
                try self.writer.writeAll(", ");
                try self.emitType(value_type);
                try self.writer.writeByte('>');
            },
            .inferred => try self.writer.writeAll("_"),
            .any => try self.writer.writeAll("any"),
            .never => try self.writer.writeAll("never"),
        }
    }

    // ========================================
    // Statement Emitters
    // ========================================

    fn emitFnDef(self: *Self, idx: StmtIdx) anyerror!void {
        const view = FnDefView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("fn ");
        try self.writeStringId(view.name);
        try self.writer.writeByte('(');

        // Parameters
        var iter = view.paramIterator(self.store);
        var first = true;
        while (iter.next()) |param| {
            if (!first) try self.writer.writeAll(", ");
            first = false;
            try self.writeStringId(param.name);
            if (!param.type_idx.isNull()) {
                try self.writer.writeAll(": ");
                try self.emitType(param.type_idx);
            }
        }

        try self.writer.writeByte(')');

        // Return type
        if (!view.return_type.isNull()) {
            try self.writer.writeAll(" -> ");
            try self.emitType(view.return_type);
        }

        try self.writer.writeAll(" {\n");
        self.indent_level += 1;

        // Body
        try self.emitStatement(view.body);

        self.indent_level -= 1;
        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitStructDef(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("structure ");
        try self.writeStringId(name);
        try self.writer.writeByte('\n');

        // Fields would be in extra_data - emit placeholder for now
        // TODO: Implement field emission when struct details are available

        try self.writeIndent();
        try self.writer.writeAll("endstructure\n");
    }

    fn emitUnionDef(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);
        const variants_start = data.b;

        try self.writeIndent();
        try self.writer.writeAll("union ");
        try self.writeStringId(name);
        try self.writer.writeAll(" {\n");

        // Emit variants from extra_data
        const variant_count = self.store.extra_data.items[variants_start];
        self.indent_level += 1;
        var i: usize = 0;
        while (i < variant_count) : (i += 1) {
            const variant_name: StringId = @enumFromInt(self.store.extra_data.items[variants_start + 1 + i * 2]);
            const variant_type = TypeIdx.fromInt(self.store.extra_data.items[variants_start + 2 + i * 2]);
            try self.writeIndent();
            try self.writeStringId(variant_name);
            if (variant_type != .null) {
                try self.writer.writeAll(": ");
                try self.emitType(variant_type);
            }
            try self.writer.writeAll(",\n");
        }
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitFieldView(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);
        const extra_start = data.b;

        // Extra data contains: base_field_id, offset, type_idx (optional)
        const base_field_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start]);
        const offset: i32 = @bitCast(self.store.extra_data.items[extra_start + 1]);

        try self.writeIndent();
        try self.writeStringId(name);
        try self.writer.writeAll(", ");
        try self.writeStringId(base_field_id);
        if (offset != 0) {
            try self.writer.print(" + {d}", .{offset});
        }
        try self.writer.writeByte('\n');
    }

    fn emitEnumDef(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("enum ");
        try self.writeStringId(name);
        try self.writer.writeAll(" {\n");

        // Variants would be in extra_data
        // TODO: Implement variant emission

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitTraitDef(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("trait ");
        try self.writeStringId(name);
        try self.writer.writeAll(" {\n");

        // TODO: Emit trait method signatures from extra_data

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitImplBlock(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const trait_type = TypeIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("impl ");
        try self.emitType(trait_type);

        // TODO: Emit "for TargetType" if present

        try self.writer.writeAll(" {\n");

        // TODO: Emit method implementations from extra_data

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitConstDecl(self: *Self, idx: StmtIdx) anyerror!void {
        const view = ConstDeclView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("const ");
        try self.writeStringId(view.name);

        if (!view.type_idx.isNull()) {
            try self.writer.writeAll(": ");
            try self.emitType(view.type_idx);
        }

        try self.writer.writeAll(" = ");
        try self.emitExpression(view.init);
        try self.writer.writeByte('\n');
    }

    fn emitLetDecl(self: *Self, idx: StmtIdx) anyerror!void {
        const view = LetDeclView.from(self.store, idx);

        try self.writeIndent();
        if (view.is_mut) {
            try self.writer.writeAll("let mut ");
        } else {
            try self.writer.writeAll("let ");
        }
        try self.writeStringId(view.name);

        if (!view.type_idx.isNull()) {
            try self.writer.writeAll(": ");
            try self.emitType(view.type_idx);
        }

        if (!view.init.isNull()) {
            try self.writer.writeAll(" = ");
            try self.emitExpression(view.init);
        }
        try self.writer.writeByte('\n');
    }

    fn emitTypeAlias(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const name: StringId = @enumFromInt(data.a);
        const target = TypeIdx.fromInt(data.b);

        try self.writeIndent();
        try self.writer.writeAll("type ");
        try self.writeStringId(name);
        try self.writer.writeAll(" = ");
        try self.emitType(target);
        try self.writer.writeByte('\n');
    }

    fn emitIfStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = IfStmtView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("if ");
        try self.emitExpression(view.condition);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;
        try self.emitStatement(view.then_body);
        self.indent_level -= 1;

        if (view.hasElse()) {
            try self.writeIndent();
            try self.writer.writeAll("} else {\n");
            self.indent_level += 1;
            try self.emitStatement(view.else_body);
            self.indent_level -= 1;
        }

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitMatchStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const value = ExprIdx.fromInt(data.a);
        const arms_start = data.b;

        try self.writeIndent();
        try self.writer.writeAll("match ");
        try self.emitExpression(value);
        try self.writer.writeAll(" {\n");

        // Emit match arms from extra_data
        const arm_count = self.store.extra_data.items[arms_start];
        self.indent_level += 1;
        var i: usize = 0;
        while (i < arm_count) : (i += 1) {
            const pattern = ExprIdx.fromInt(self.store.extra_data.items[arms_start + 1 + i * 2]);
            const body = StmtIdx.fromInt(self.store.extra_data.items[arms_start + 2 + i * 2]);
            try self.writeIndent();
            try self.emitExpression(pattern);
            try self.writer.writeAll(" => ");
            // Check if body is a block or single statement
            const body_tag = self.store.stmtTag(body);
            if (body_tag == .block) {
                try self.writer.writeAll("{\n");
                self.indent_level += 1;
                try self.emitStatement(body);
                self.indent_level -= 1;
                try self.writeIndent();
                try self.writer.writeAll("}");
            } else {
                try self.emitStatement(body);
            }
            try self.writer.writeAll(",\n");
        }
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitForStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = ForStmtView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("for ");
        try self.writeStringId(view.binding);
        try self.writer.writeAll(" in ");
        try self.emitExpression(view.iterable);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;
        try self.emitStatement(view.body);
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitWhileStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = WhileStmtView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("while ");
        try self.emitExpression(view.condition);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;
        try self.emitStatement(view.body);
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitLoopStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = LoopStmtView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("loop {\n");

        self.indent_level += 1;
        try self.emitStatement(view.body);
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitReturnStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = ReturnView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("return");

        if (view.hasValue()) {
            try self.writer.writeByte(' ');
            try self.emitExpression(view.value);
        }

        try self.writer.writeByte('\n');
    }

    fn emitBreakStmt(self: *Self, _: StmtIdx) anyerror!void {
        try self.writeIndent();
        try self.writer.writeAll("break\n");
    }

    fn emitContinueStmt(self: *Self, _: StmtIdx) anyerror!void {
        try self.writeIndent();
        try self.writer.writeAll("continue\n");
    }

    fn emitTryStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const try_body = StmtIdx.fromInt(data.a);
        const extra_start = ast.ExtraIdx.fromInt(data.b);

        // Extract from extra_data: err_binding, catch_body, finally_body
        const extra_base = extra_start.toInt();
        const err_binding: StringId = @enumFromInt(self.store.extra_data.items[extra_base]);
        const catch_body = StmtIdx.fromInt(self.store.extra_data.items[extra_base + 1]);
        const finally_body = StmtIdx.fromInt(self.store.extra_data.items[extra_base + 2]);

        // Emit try block
        try self.writeIndent();
        try self.writer.writeAll("try {\n");
        self.indent_level += 1;
        try self.emitStatement(try_body);
        self.indent_level -= 1;
        try self.writeIndent();
        try self.writer.writeAll("} catch");

        // Emit error binding if present
        if (err_binding != .null_id) {
            try self.writer.writeAll(" (");
            try self.writer.writeAll(self.store.strings.get(err_binding));
            try self.writer.writeByte(')');
        }

        // Emit catch block
        try self.writer.writeAll(" {\n");
        self.indent_level += 1;
        if (!catch_body.isNull()) {
            try self.emitStatement(catch_body);
        }
        self.indent_level -= 1;
        try self.writeIndent();
        try self.writer.writeByte('}');

        // Emit finally block if present
        if (!finally_body.isNull()) {
            try self.writer.writeAll(" finally {\n");
            self.indent_level += 1;
            try self.emitStatement(finally_body);
            self.indent_level -= 1;
            try self.writeIndent();
            try self.writer.writeByte('}');
        }

        try self.writer.writeByte('\n');
    }

    fn emitThrowStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const value = ExprIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("throw ");
        try self.emitExpression(value);
        try self.writer.writeByte('\n');
    }

    fn emitDeferStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const body = StmtIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("defer ");

        // Check if body is a block or expression statement
        const body_tag = self.store.stmtTag(body);
        if (body_tag == .block) {
            try self.emitBlock(body);
        } else if (body_tag == .expression) {
            // Single expression - emit without block braces
            const expr_data = self.store.stmtData(body);
            const expr = ExprIdx.fromInt(expr_data.a);
            try self.emitExpression(expr);
            try self.writer.writeByte('\n');
        } else {
            // Unexpected - emit as block for safety
            try self.writer.writeAll("{\n");
            self.indent_level += 1;
            try self.emitStatement(body);
            self.indent_level -= 1;
            try self.writeIndent();
            try self.writer.writeAll("}\n");
        }
    }

    fn emitAssignment(self: *Self, idx: StmtIdx) anyerror!void {
        const view = AssignmentView.from(self.store, idx);

        try self.writeIndent();
        try self.emitExpression(view.target);
        try self.writer.writeAll(" = ");
        try self.emitExpression(view.value);
        try self.writer.writeByte('\n');
    }

    fn emitExprStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = ExprStmtView.from(self.store, idx);

        try self.writeIndent();
        try self.emitExpression(view.expr);
        try self.writer.writeByte('\n');
    }

    fn emitBlock(self: *Self, idx: StmtIdx) anyerror!void {
        const view = BlockView.from(self.store, idx);
        const stmt_indices = view.getStatements(self.store);

        for (stmt_indices) |stmt_int| {
            const stmt = StmtIdx.fromInt(stmt_int);
            try self.emitStatement(stmt);
        }
    }

    fn emitImportStmt(self: *Self, idx: StmtIdx) anyerror!void {
        const view = ImportView.from(self.store, idx);

        try self.writeIndent();
        try self.writer.writeAll("import \"");
        try self.writeStringId(view.module_path);
        try self.writer.writeAll("\"\n");
    }

    fn emitIoOpen(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);
        const filename = ExprIdx.fromInt(data.b);

        try self.writeIndent();
        try self.writer.writeAll("open(");
        try self.emitExpression(channel);
        try self.writer.writeAll(", ");
        try self.emitExpression(filename);
        try self.writer.writeAll(")\n");
    }

    fn emitIoClose(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("close(");
        try self.emitExpression(channel);
        try self.writer.writeAll(")\n");
    }

    fn emitIoRead(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);
        const buffer = ExprIdx.fromInt(data.b);

        try self.writeIndent();
        try self.writer.writeAll("read(");
        try self.emitExpression(channel);
        try self.writer.writeAll(", ");
        try self.emitExpression(buffer);
        try self.writer.writeAll(")\n");
    }

    fn emitIoWrite(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);
        const buffer = ExprIdx.fromInt(data.b);

        try self.writeIndent();
        try self.writer.writeAll("write(");
        try self.emitExpression(channel);
        try self.writer.writeAll(", ");
        try self.emitExpression(buffer);
        try self.writer.writeAll(")\n");
    }

    fn emitIoStore(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);
        const buffer = ExprIdx.fromInt(data.b);

        try self.writeIndent();
        try self.writer.writeAll("store(");
        try self.emitExpression(channel);
        try self.writer.writeAll(", ");
        try self.emitExpression(buffer);
        try self.writer.writeAll(")\n");
    }

    fn emitIoDelete(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const channel = ExprIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("delete(");
        try self.emitExpression(channel);
        try self.writer.writeAll(")\n");
    }

    fn emitComptimeIf(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const condition = ExprIdx.fromInt(data.a);
        const then_body = StmtIdx.fromInt(data.b >> 16);
        const extra_start = data.b & 0xFFFF;
        const else_body = StmtIdx.fromInt(self.store.extra_data.items[extra_start]);

        try self.writeIndent();
        try self.writer.writeAll("comptime if ");
        try self.emitExpression(condition);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;
        try self.emitStatement(then_body);
        self.indent_level -= 1;

        if (else_body != .null) {
            try self.writeIndent();
            try self.writer.writeAll("} else ");
            // Check if else is another comptime_if (chained)
            const else_tag = self.store.stmtTag(else_body);
            if (else_tag == .comptime_if) {
                // Inline the else-if
                try self.emitComptimeIf(else_body);
                return;
            } else {
                try self.writer.writeAll("{\n");
                self.indent_level += 1;
                try self.emitStatement(else_body);
                self.indent_level -= 1;
            }
        }

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitComptimeBlock(self: *Self, idx: StmtIdx) anyerror!void {
        const data = self.store.stmtData(idx);
        const body = StmtIdx.fromInt(data.a);

        try self.writeIndent();
        try self.writer.writeAll("comptime {\n");

        self.indent_level += 1;
        try self.emitStatement(body);
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    fn emitTestDef(self: *Self, idx: StmtIdx) anyerror!void {
        const test_data = self.store.getTestDef(idx);
        const name = self.strings.get(test_data.name);

        try self.writeIndent();
        try self.writer.writeAll("test \"");
        try self.writer.writeAll(name);
        try self.writer.writeAll("\" {\n");

        self.indent_level += 1;
        // Emit body
        const body_span = self.store.stmtData(test_data.body).getSpan();
        const stmts = self.store.getStmtSpan(body_span);
        for (stmts) |stmt_u32| {
            const stmt_idx = StmtIdx.fromInt(stmt_u32);
            try self.emitStatement(stmt_idx);
        }
        self.indent_level -= 1;

        try self.writeIndent();
        try self.writer.writeAll("}\n");
    }

    // ========================================
    // Expression Emitters
    // ========================================

    fn emitIntLiteral(self: *Self, idx: ExprIdx) anyerror!void {
        const view = IntLiteralView.from(self.store, idx);
        try self.writer.print("{d}", .{view.value});
    }

    fn emitFloatLiteral(self: *Self, idx: ExprIdx) anyerror!void {
        const view = FloatLiteralView.from(self.store, idx);
        try self.writer.print("{d}", .{view.value});
    }

    fn emitStringLiteral(self: *Self, idx: ExprIdx) anyerror!void {
        const view = StringLiteralView.from(self.store, idx);
        try self.writer.writeByte('"');
        const str = self.strings.get(view.value);
        // Escape special characters
        for (str) |c| {
            switch (c) {
                '"' => try self.writer.writeAll("\\\""),
                '\\' => try self.writer.writeAll("\\\\"),
                '\n' => try self.writer.writeAll("\\n"),
                '\r' => try self.writer.writeAll("\\r"),
                '\t' => try self.writer.writeAll("\\t"),
                else => try self.writer.writeByte(c),
            }
        }
        try self.writer.writeByte('"');
    }

    fn emitBoolLiteral(self: *Self, idx: ExprIdx) anyerror!void {
        const view = BoolLiteralView.from(self.store, idx);
        if (view.value) {
            try self.writer.writeAll("true");
        } else {
            try self.writer.writeAll("false");
        }
    }

    fn emitIdentifier(self: *Self, idx: ExprIdx) anyerror!void {
        const view = IdentifierView.from(self.store, idx);
        try self.writeStringId(view.name);
    }

    fn emitMember(self: *Self, idx: ExprIdx) anyerror!void {
        const view = MemberView.from(self.store, idx);
        try self.emitExpression(view.object);
        try self.writer.writeByte('.');
        try self.writeStringId(view.field);
    }

    fn emitIndex(self: *Self, idx: ExprIdx) anyerror!void {
        const view = IndexView.from(self.store, idx);
        try self.emitExpression(view.object);
        try self.writer.writeByte('[');
        try self.emitExpression(view.index);
        try self.writer.writeByte(']');
    }

    fn emitBinary(self: *Self, idx: ExprIdx) anyerror!void {
        const view = BinaryExprView.from(self.store, idx);

        // Check if we need parentheses (simplified - just wrap if nested binary)
        const lhs_tag = self.store.exprTag(view.lhs);
        const needs_lhs_parens = lhs_tag == .binary;

        if (needs_lhs_parens) try self.writer.writeByte('(');
        try self.emitExpression(view.lhs);
        if (needs_lhs_parens) try self.writer.writeByte(')');

        try self.writer.writeByte(' ');
        try self.emitBinaryOp(view.op);
        try self.writer.writeByte(' ');

        const rhs_tag = self.store.exprTag(view.rhs);
        const needs_rhs_parens = rhs_tag == .binary;

        if (needs_rhs_parens) try self.writer.writeByte('(');
        try self.emitExpression(view.rhs);
        if (needs_rhs_parens) try self.writer.writeByte(')');
    }

    fn emitBinaryOp(self: *Self, op: BinaryOp) anyerror!void {
        if (self.options.modern_operators) {
            try self.writer.writeAll(switch (op) {
                .add => "+",
                .sub => "-",
                .mul => "*",
                .div => "/",
                .mod => "%",
                .eq => "==",
                .ne => "!=",
                .lt => "<",
                .le => "<=",
                .gt => ">",
                .ge => ">=",
                .@"and" => "&&",
                .@"or" => "||",
                .bit_and => "&",
                .bit_or => "|",
                .bit_xor => "^",
                .shl => "<<",
                .shr => ">>",
                .round => "##", // True rounding
                .trunc => "#", // Truncating round
                .range => "..",
                .range_inclusive => "..=",
                .null_coalesce => "??",
            });
        } else {
            // DBL-style operators
            try self.writer.writeAll(switch (op) {
                .add => "+",
                .sub => "-",
                .mul => "*",
                .div => "/",
                .mod => "%",
                .eq => ".EQ.",
                .ne => ".NE.",
                .lt => ".LT.",
                .le => ".LE.",
                .gt => ".GT.",
                .ge => ".GE.",
                .@"and" => ".AND.",
                .@"or" => ".OR.",
                .bit_and => ".BAND.",
                .bit_or => ".BOR.",
                .bit_xor => ".BXOR.",
                .shl => "<<",
                .shr => ">>",
                .round => "##", // True rounding
                .trunc => "#", // Truncating round
                .range => "..",
                .range_inclusive => "..=",
                .null_coalesce => "??",
            });
        }
    }

    fn emitUnary(self: *Self, idx: ExprIdx) anyerror!void {
        const view = UnaryExprView.from(self.store, idx);

        switch (view.op) {
            .neg => try self.writer.writeByte('-'),
            .not => {
                if (self.options.modern_operators) {
                    try self.writer.writeByte('!');
                } else {
                    try self.writer.writeAll(".NOT.");
                }
            },
            .addr_of => try self.writer.writeByte('&'),
            .deref => try self.writer.writeByte('*'),
            .bit_not => try self.writer.writeByte('~'),
        }

        try self.emitExpression(view.operand);
    }

    fn emitCall(self: *Self, idx: ExprIdx) anyerror!void {
        const view = CallView.from(self.store, idx);

        try self.emitExpression(view.callee);
        try self.writer.writeByte('(');

        const args = view.getArgs(self.store);
        for (args, 0..) |arg_int, i| {
            if (i > 0) try self.writer.writeAll(", ");
            const arg_idx = ExprIdx.fromInt(arg_int);
            try self.emitExpression(arg_idx);
        }

        try self.writer.writeByte(')');
    }

    fn emitMethodCall(self: *Self, idx: ExprIdx) anyerror!void {
        // Method calls are similar to regular calls but with a receiver
        const data = self.store.exprData(idx);
        const receiver = ExprIdx.fromInt(data.a);

        try self.emitExpression(receiver);
        try self.writer.writeAll(".method()"); // Simplified
    }

    fn emitRange(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const start = ExprIdx.fromInt(data.a);
        const end = ExprIdx.fromInt(data.b);

        try self.emitExpression(start);
        try self.writer.writeAll("..");
        try self.emitExpression(end);
    }

    fn emitArrayInit(self: *Self, idx: ExprIdx) anyerror!void {
        _ = idx;
        try self.writer.writeAll("[]"); // Simplified
    }

    fn emitStructInit(self: *Self, idx: ExprIdx) anyerror!void {
        _ = idx;
        try self.writer.writeAll("{}"); // Simplified
    }

    fn emitLambda(self: *Self, idx: ExprIdx) anyerror!void {
        _ = idx;
        try self.writer.writeAll("|...| { ... }"); // Simplified
    }

    fn emitComptimeBuiltin(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const name: StringId = @enumFromInt(data.a);

        try self.writer.writeByte('@');
        try self.writeStringId(name);
        try self.writer.writeAll("()");
    }

    fn emitGrouping(self: *Self, idx: ExprIdx) anyerror!void {
        const view = GroupingView.from(self.store, idx);
        try self.writer.writeByte('(');
        try self.emitExpression(view.inner);
        try self.writer.writeByte(')');
    }

    fn emitIfExpr(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const cond = ExprIdx.fromInt(data.a);
        const then_expr = ExprIdx.fromInt(data.b);

        try self.writer.writeAll("if ");
        try self.emitExpression(cond);
        try self.writer.writeAll(" then ");
        try self.emitExpression(then_expr);
        try self.writer.writeAll(" else ...");
    }

    fn emitMatchExpr(self: *Self, idx: ExprIdx) anyerror!void {
        _ = idx;
        try self.writer.writeAll("match { ... }");
    }

    fn emitBlockExpr(self: *Self, idx: ExprIdx) anyerror!void {
        _ = idx;
        try self.writer.writeAll("{ ... }");
    }

    fn emitOptionalMember(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const object_idx: ExprIdx = @enumFromInt(data.a);
        const member_id: StringId = @enumFromInt(data.b);
        try self.emitExpression(object_idx);
        try self.writer.writeAll("?.");
        try self.writeStringId(member_id);
    }

    fn emitOptionalIndex(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const object_idx: ExprIdx = @enumFromInt(data.a);
        const index_idx: ExprIdx = @enumFromInt(data.b);
        try self.emitExpression(object_idx);
        try self.writer.writeAll("?[");
        try self.emitExpression(index_idx);
        try self.writer.writeAll("]");
    }

    fn emitIsExpr(self: *Self, idx: ExprIdx) anyerror!void {
        const data = self.store.exprData(idx);
        const expr_idx: ExprIdx = @enumFromInt(data.a);
        const type_idx: TypeIdx = @enumFromInt(data.b);
        try self.emitExpression(expr_idx);
        try self.writer.writeAll(" is ");
        try self.emitType(type_idx);
    }

    // ========================================
    // Helpers
    // ========================================

    fn writeIndent(self: *Self) anyerror!void {
        var i: u32 = 0;
        while (i < self.indent_level * self.options.indent_size) : (i += 1) {
            try self.writer.writeByte(' ');
        }
    }

    fn writeStringId(self: *Self, id: StringId) anyerror!void {
        const str = self.strings.get(id);
        try self.writer.writeAll(str);
    }
};

/// Convenience function to emit AST to a string
pub fn emitToString(
    allocator: std.mem.Allocator,
    store: *const NodeStore,
    strings: *const StringInterner,
    stmts: []const StmtIdx,
    options: Emitter.Options,
) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .{};
    errdefer list.deinit(allocator);

    var emitter = Emitter.init(list.writer(allocator).any(), store, strings, options);
    try emitter.emitProgram(stmts);

    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "emit int literal" {
    const allocator = std.testing.allocator;

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

    var store = NodeStore.init(allocator, &interner);
    defer store.deinit();

    const lit = try store.addIntLiteral(42, SourceLoc.zero);
    const stmt = try store.addExprStmt(lit, SourceLoc.zero);

    const stmts = [_]StmtIdx{stmt};
    const output = try emitToString(allocator, &store, &interner, &stmts, .{});
    defer allocator.free(output);

    try std.testing.expectEqualStrings("42\n", output);
}

test "emit binary expression" {
    const allocator = std.testing.allocator;

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

    var store = NodeStore.init(allocator, &interner);
    defer store.deinit();

    const lhs = try store.addIntLiteral(10, SourceLoc.zero);
    const rhs = try store.addIntLiteral(20, SourceLoc.zero);
    const bin = try store.addBinary(lhs, .add, rhs, SourceLoc.zero);
    const stmt = try store.addExprStmt(bin, SourceLoc.zero);

    const stmts = [_]StmtIdx{stmt};
    const output = try emitToString(allocator, &store, &interner, &stmts, .{});
    defer allocator.free(output);

    try std.testing.expectEqualStrings("10 + 20\n", output);
}

test "emit comparison with modern operators" {
    const allocator = std.testing.allocator;

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

    var store = NodeStore.init(allocator, &interner);
    defer store.deinit();

    const name = try interner.intern("x");
    const id = try store.addIdentifier(name, SourceLoc.zero);
    const lit = try store.addIntLiteral(0, SourceLoc.zero);
    const cmp = try store.addBinary(id, .gt, lit, SourceLoc.zero);
    const stmt = try store.addExprStmt(cmp, SourceLoc.zero);

    const stmts = [_]StmtIdx{stmt};

    // Modern operators
    const modern = try emitToString(allocator, &store, &interner, &stmts, .{ .modern_operators = true });
    defer allocator.free(modern);
    try std.testing.expectEqualStrings("x > 0\n", modern);

    // DBL operators
    const dbl = try emitToString(allocator, &store, &interner, &stmts, .{ .modern_operators = false });
    defer allocator.free(dbl);
    try std.testing.expectEqualStrings("x .GT. 0\n", dbl);
}

test "emit const declaration" {
    const allocator = std.testing.allocator;

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

    var store = NodeStore.init(allocator, &interner);
    defer store.deinit();

    const name = try interner.intern("MAX_SIZE");
    const value = try store.addIntLiteral(100, SourceLoc.zero);
    const i32_type = try store.addPrimitiveType(.i32);
    const decl = try store.addConstDecl(name, i32_type, value, SourceLoc.zero);

    const stmts = [_]StmtIdx{decl};
    const output = try emitToString(allocator, &store, &interner, &stmts, .{});
    defer allocator.free(output);

    try std.testing.expectEqualStrings("const MAX_SIZE: i32 = 100\n", output);
}

test "emit string literal with escapes" {
    const allocator = std.testing.allocator;

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

    var store = NodeStore.init(allocator, &interner);
    defer store.deinit();

    const str = try interner.intern("hello\nworld");
    const lit = try store.addStringLiteral(str, SourceLoc.zero);
    const stmt = try store.addExprStmt(lit, SourceLoc.zero);

    const stmts = [_]StmtIdx{stmt};
    const output = try emitToString(allocator, &store, &interner, &stmts, .{});
    defer allocator.free(output);

    try std.testing.expectEqualStrings("\"hello\\nworld\"\n", output);
}
