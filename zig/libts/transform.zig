//! TS AST → Cot AST Transform
//!
//! Converts libts TS AST nodes (ts_ast.zig) into libcot Cot AST nodes
//! (zig/libcot/frontend/ast.zig) in memory. The resulting Cot AST is
//! indistinguishable from one produced by the Cot parser — the checker
//! and lowerer never know the source was TypeScript.
//!
//! Reference: claude/LIBTS_EXECUTION_PLAN.md §5 (Complex Transform Rules)
//!
//! Key transforms:
//!   console.log(x) → println(x)
//!   function foo() → fn foo()
//!   class → struct + impl
//!   this → self
//!   === / !== → == / !=
//!   ?? → orelse
//!   throw → return error
//!   Top-level statements → fn main() void { ... }

const std = @import("std");
const ts_ast = @import("ts_ast.zig");
const ts_source = @import("source.zig");

// Cot AST types — imported from libcot
const cot_ast = @import("cot_ast");
const cot_source = @import("cot_source");
const cot_token = @import("cot_token");

const Span = ts_source.Span;

pub const TransformError = error{OutOfMemory};

pub const Transformer = struct {
    ts: *const ts_ast.Ast,
    cot: *cot_ast.Ast,
    allocator: std.mem.Allocator,

    pub fn init(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator) Transformer {
        return .{
            .ts = ts,
            .cot = cot,
            .allocator = allocator,
        };
    }

    /// Transform the entire TS source file → Cot AST file.
    /// Top-level statements are wrapped in fn main() void { ... }
    pub fn transformSourceFile(self: *Transformer, filename: []const u8) TransformError!void {
        const sf = self.ts.source_file orelse return;

        var top_decls = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
        var main_stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};

        for (sf.statements) |stmt_idx| {
            const node = self.ts.getNode(stmt_idx) orelse continue;
            switch (node.*) {
                .decl => |d| switch (d) {
                    // Function declarations → top-level fn_decl
                    .function => {
                        const fn_idx = try self.transformFunctionDecl(d.function);
                        try top_decls.append(self.allocator, fn_idx);
                    },
                    // Variable declarations → top-level const/var or main body
                    .variable => {
                        const var_idx = try self.transformVarDecl(d.variable);
                        try main_stmts.append(self.allocator, var_idx);
                    },
                    // Class → struct + impl (top-level)
                    .class => {
                        const struct_idx = try self.transformClassDecl(d.class);
                        try top_decls.append(self.allocator, struct_idx);
                    },
                    // Import declarations → import_decl (for now, skip)
                    .import_decl => {},
                    // Export wrapping a declaration
                    .export_decl => |exp| {
                        if (exp.declaration != ts_ast.null_node) {
                            const inner = self.ts.getNode(exp.declaration) orelse continue;
                            switch (inner.*) {
                                .decl => |inner_d| switch (inner_d) {
                                    .function => {
                                        const fn_idx = try self.transformFunctionDecl(inner_d.function);
                                        try top_decls.append(self.allocator, fn_idx);
                                    },
                                    else => {},
                                },
                                else => {},
                            }
                        }
                    },
                    else => {},
                },
                .stmt => {
                    const cot_stmt = try self.transformStatement(stmt_idx);
                    if (cot_stmt != cot_ast.null_node) {
                        try main_stmts.append(self.allocator, cot_stmt);
                    }
                },
                else => {},
            }
        }

        // If there are any top-level statements, wrap them in fn main() void { ... }
        if (main_stmts.items.len > 0) {
            const main_fn = try self.createMainFunction(main_stmts.items);
            try top_decls.append(self.allocator, main_fn);
        }

        const decls = try self.allocator.dupe(cot_ast.NodeIndex, top_decls.items);
        self.cot.file = .{
            .filename = filename,
            .decls = decls,
            .span = cot_source.Span.zero,
        };
    }

    // ================================================================
    // Declarations
    // ================================================================

    fn transformFunctionDecl(self: *Transformer, func: ts_ast.Decl.FunctionDecl) TransformError!cot_ast.NodeIndex {
        const name = func.name orelse "anonymous";

        // Transform parameters
        const params = try self.transformParams(func.params);

        // Return type: if annotated, transform; otherwise void
        const return_type = if (func.return_type != ts_ast.null_node)
            try self.transformTypeNode(func.return_type)
        else
            try self.cot.addExpr(.{ .type_expr = .{
                .kind = .{ .named = "void" },
                .span = cot_source.Span.zero,
            } });

        // Body
        const body = if (func.body != ts_ast.null_node)
            try self.transformBlock(func.body)
        else
            cot_ast.null_node;

        return self.cot.addDecl(.{ .fn_decl = .{
            .name = name,
            .params = params,
            .return_type = return_type,
            .body = body,
            .is_extern = false,
            .is_export = func.is_export,
            .is_async = func.is_async,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformVarDecl(self: *Transformer, decl: ts_ast.Decl.VarDecl) TransformError!cot_ast.NodeIndex {
        // For now, transform first declarator only
        if (decl.declarators.len == 0) return cot_ast.null_node;
        const d = decl.declarators[0];

        // Get the binding name
        const name = if (self.ts.getNode(d.binding)) |node| switch (node.*) {
            .expr => |e| switch (e) {
                .ident => |id| id.name,
                else => "unknown",
            },
            else => "unknown",
        } else "unknown";

        const is_const = decl.kind == .const_kw;
        const type_expr = if (d.type_ann != ts_ast.null_node)
            try self.transformTypeNode(d.type_ann)
        else
            cot_ast.null_node;

        const value = if (d.init != ts_ast.null_node)
            try self.transformExpr(d.init)
        else
            cot_ast.null_node;

        return self.cot.addStmt(.{ .var_stmt = .{
            .name = name,
            .type_expr = type_expr,
            .value = value,
            .is_const = is_const,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformClassDecl(self: *Transformer, class: ts_ast.Decl.ClassDecl) TransformError!cot_ast.NodeIndex {
        const name = class.name orelse "AnonymousClass";
        // Minimal: just create an empty struct for now
        return self.cot.addDecl(.{ .struct_decl = .{
            .name = name,
            .fields = &.{},
            .span = cot_source.Span.zero,
        } });
    }

    // ================================================================
    // Statements
    // ================================================================

    fn transformStatement(self: *Transformer, idx: ts_ast.NodeIndex) TransformError!cot_ast.NodeIndex {
        const node = self.ts.getNode(idx) orelse return cot_ast.null_node;
        switch (node.*) {
            .stmt => |s| return self.transformStmt(s),
            .decl => |d| switch (d) {
                .variable => return self.transformVarDecl(d.variable),
                else => return cot_ast.null_node,
            },
            else => return cot_ast.null_node,
        }
    }

    fn transformStmt(self: *Transformer, stmt: ts_ast.Stmt) TransformError!cot_ast.NodeIndex {
        return switch (stmt) {
            .expr_stmt => |es| blk: {
                const expr = try self.transformExpr(es.expr);
                break :blk self.cot.addStmt(.{ .expr_stmt = .{
                    .expr = expr,
                    .span = cot_source.Span.zero,
                } });
            },
            .return_stmt => |rs| blk: {
                const value = if (rs.value != ts_ast.null_node)
                    try self.transformExpr(rs.value)
                else
                    cot_ast.null_node;
                break :blk self.cot.addStmt(.{ .return_stmt = .{
                    .value = value,
                    .span = cot_source.Span.zero,
                } });
            },
            .if_stmt => |ifs| blk: {
                const cond = try self.transformExpr(ifs.condition);
                const then_body = try self.transformStatement(ifs.consequent);
                const else_body = if (ifs.alternate != ts_ast.null_node)
                    try self.transformStatement(ifs.alternate)
                else
                    cot_ast.null_node;
                break :blk self.cot.addStmt(.{ .if_stmt = .{
                    .condition = cond,
                    .then_branch = then_body,
                    .else_branch = else_body,
                    .span = cot_source.Span.zero,
                } });
            },
            .while_stmt => |ws| blk: {
                const cond = try self.transformExpr(ws.condition);
                const body = try self.transformStatement(ws.body);
                break :blk self.cot.addStmt(.{ .while_stmt = .{
                    .condition = cond,
                    .body = body,
                    .span = cot_source.Span.zero,
                } });
            },
            .block => |bs| self.transformBlockStmt(bs),
            .var_stmt => |vs| blk: {
                const inner = self.ts.getNode(vs.decl) orelse break :blk cot_ast.null_node;
                switch (inner.*) {
                    .decl => |d| switch (d) {
                        .variable => break :blk self.transformVarDecl(d.variable),
                        else => {},
                    },
                    else => {},
                }
                break :blk cot_ast.null_node;
            },
            else => cot_ast.null_node,
        };
    }

    fn transformBlock(self: *Transformer, idx: ts_ast.NodeIndex) TransformError!cot_ast.NodeIndex {
        const node = self.ts.getNode(idx) orelse return cot_ast.null_node;
        switch (node.*) {
            .stmt => |s| switch (s) {
                .block => |bs| return self.transformBlockStmt(bs),
                else => {},
            },
            else => {},
        }
        return cot_ast.null_node;
    }

    fn transformBlockStmt(self: *Transformer, bs: ts_ast.Stmt.BlockStmt) TransformError!cot_ast.NodeIndex {
        var stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
        for (bs.stmts) |stmt_idx| {
            const cot_stmt = try self.transformStatement(stmt_idx);
            if (cot_stmt != cot_ast.null_node) {
                try stmts.append(self.allocator, cot_stmt);
            }
        }
        const owned = try self.allocator.dupe(cot_ast.NodeIndex, stmts.items);
        return self.cot.addExpr(.{ .block_expr = .{
            .stmts = owned,
            .expr = cot_ast.null_node,
            .span = cot_source.Span.zero,
        } });
    }

    // ================================================================
    // Expressions
    // ================================================================

    fn transformExpr(self: *Transformer, idx: ts_ast.NodeIndex) TransformError!cot_ast.NodeIndex {
        const node = self.ts.getNode(idx) orelse return cot_ast.null_node;
        switch (node.*) {
            .expr => |e| return self.transformExprNode(e),
            else => return cot_ast.null_node,
        }
    }

    fn transformExprNode(self: *Transformer, expr: ts_ast.Expr) TransformError!cot_ast.NodeIndex {
        return switch (expr) {
            .ident => |id| self.cot.addExpr(.{ .ident = .{
                .name = id.name,
                .span = cot_source.Span.zero,
            } }),
            .literal => |lit| self.transformLiteral(lit),
            .call => |c| self.transformCall(c),
            .member => |m| self.transformMember(m),
            .binary => |b| self.transformBinary(b),
            .unary => |u| self.transformUnary(u),
            .assign => |a| self.transformAssign(a),
            .template => |t| self.transformTemplate(t),
            .paren => |p| blk: {
                if (p.inner == ts_ast.null_node) break :blk cot_ast.null_node;
                break :blk self.transformExpr(p.inner);
            },
            .arrow_function => |af| self.transformArrowFunction(af),
            .this_expr => self.cot.addExpr(.{ .ident = .{
                .name = "self",
                .span = cot_source.Span.zero,
            } }),
            .await_expr => |aw| blk: {
                const arg = try self.transformExpr(aw.argument);
                break :blk self.cot.addExpr(.{ .await_expr = .{
                    .operand = arg,
                    .span = cot_source.Span.zero,
                } });
            },
            .new_expr => |n| self.transformNew(n),
            .array_literal => |al| blk: {
                var elems = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                for (al.elements) |elem_idx| {
                    if (elem_idx != ts_ast.null_node) {
                        const e = try self.transformExpr(elem_idx);
                        try elems.append(self.allocator, e);
                    }
                }
                const owned = try self.allocator.dupe(cot_ast.NodeIndex, elems.items);
                break :blk self.cot.addExpr(.{ .array_literal = .{
                    .elements = owned,
                    .span = cot_source.Span.zero,
                } });
            },
            .conditional => |c| blk: {
                const cond = try self.transformExpr(c.condition);
                const then_val = try self.transformExpr(c.consequent);
                const else_val = try self.transformExpr(c.alternate);
                break :blk self.cot.addExpr(.{ .if_expr = .{
                    .condition = cond,
                    .then_branch = then_val,
                    .else_branch = else_val,
                    .span = cot_source.Span.zero,
                } });
            },
            else => cot_ast.null_node,
        };
    }

    fn transformLiteral(self: *Transformer, lit: ts_ast.Expr.Literal) TransformError!cot_ast.NodeIndex {
        const kind: cot_ast.LiteralKind = switch (lit.kind) {
            .string => .string,
            .number => .int, // Simplified: all numbers → int for now
            .bigint => .int,
            .boolean => if (std.mem.eql(u8, lit.value, "true")) .true_lit else .false_lit,
            .null_lit => .null_lit,
            .undefined => .undefined_lit,
            .regex => .string, // Regex as string for now
        };
        return self.cot.addExpr(.{ .literal = .{
            .kind = kind,
            .value = lit.value,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformCall(self: *Transformer, call: ts_ast.Expr.CallExpr) TransformError!cot_ast.NodeIndex {
        // Special case: console.log(x) → println(x)
        if (self.isConsoleLog(call.callee)) {
            const callee = try self.cot.addExpr(.{ .ident = .{
                .name = "println",
                .span = cot_source.Span.zero,
            } });
            var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (call.args) |arg_idx| {
                const a = try self.transformExpr(arg_idx);
                try args.append(self.allocator, a);
            }
            const owned_args = try self.allocator.dupe(cot_ast.NodeIndex, args.items);
            return self.cot.addExpr(.{ .call = .{
                .callee = callee,
                .args = owned_args,
                .span = cot_source.Span.zero,
            } });
        }

        // console.error(x) → eprintln(x)
        if (self.isConsoleError(call.callee)) {
            const callee = try self.cot.addExpr(.{ .ident = .{
                .name = "eprintln",
                .span = cot_source.Span.zero,
            } });
            var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (call.args) |arg_idx| {
                const a = try self.transformExpr(arg_idx);
                try args.append(self.allocator, a);
            }
            const owned_args = try self.allocator.dupe(cot_ast.NodeIndex, args.items);
            return self.cot.addExpr(.{ .call = .{
                .callee = callee,
                .args = owned_args,
                .span = cot_source.Span.zero,
            } });
        }

        // General call
        const callee = try self.transformExpr(call.callee);
        var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
        for (call.args) |arg_idx| {
            const a = try self.transformExpr(arg_idx);
            try args.append(self.allocator, a);
        }
        const owned_args = try self.allocator.dupe(cot_ast.NodeIndex, args.items);
        return self.cot.addExpr(.{ .call = .{
            .callee = callee,
            .args = owned_args,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformMember(self: *Transformer, m: ts_ast.Expr.MemberExpr) TransformError!cot_ast.NodeIndex {
        const base = try self.transformExpr(m.object);
        return self.cot.addExpr(.{ .field_access = .{
            .base = base,
            .field = m.property,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformBinary(self: *Transformer, b: ts_ast.Expr.BinaryExpr) TransformError!cot_ast.NodeIndex {
        const left = try self.transformExpr(b.left);
        const right = try self.transformExpr(b.right);

        // Map TS binary ops to Cot tokens
        const op: cot_token.Token = switch (b.op) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .quo,
            .rem => .rem,
            .eql, .strict_eql => .eql, // === → ==
            .neq, .strict_neq => .neq, // !== → !=
            .lt => .lss,
            .lte => .leq,
            .gt => .gtr,
            .gte => .geq,
            .land => .land,
            .lor => .lor,
            .bitand => .@"and",
            .bitor => .@"or",
            .bitxor => .xor,
            .shl => .shl,
            .shr, .unsigned_shr => .shr,
            .nullish_coalesce => .kw_orelse,
            else => .add, // fallback
        };

        return self.cot.addExpr(.{ .binary = .{
            .op = op,
            .left = left,
            .right = right,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformUnary(self: *Transformer, u: ts_ast.Expr.UnaryExpr) TransformError!cot_ast.NodeIndex {
        const operand = try self.transformExpr(u.operand);
        const op: cot_token.Token = switch (u.op) {
            .neg => .sub,
            .lnot => .lnot,
            .bitnot => .not,
            else => .sub,
        };
        return self.cot.addExpr(.{ .unary = .{
            .op = op,
            .operand = operand,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformAssign(self: *Transformer, a: ts_ast.Expr.AssignExpr) TransformError!cot_ast.NodeIndex {
        const left = try self.transformExpr(a.left);
        const right = try self.transformExpr(a.right);
        const op: cot_token.Token = switch (a.op) {
            .assign => .assign,
            .add_assign => .add_assign,
            .sub_assign => .sub_assign,
            .mul_assign => .mul_assign,
            .div_assign => .quo_assign,
            .rem_assign => .rem_assign,
            else => .assign,
        };
        return self.cot.addStmt(.{ .assign_stmt = .{
            .target = left,
            .value = right,
            .op = op,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformTemplate(self: *Transformer, t: ts_ast.Expr.TemplateLiteral) TransformError!cot_ast.NodeIndex {
        // Template literal → Cot string interpolation
        // `hello ${name}!` → "hello ${name}!"
        if (t.expressions.len == 0 and t.quasis.len == 1) {
            // No substitutions — just a string. Strip backticks.
            const raw = t.quasis[0];
            const stripped = if (raw.len >= 2 and raw[0] == '`' and raw[raw.len - 1] == '`')
                raw[1 .. raw.len - 1]
            else
                raw;
            const quoted = try std.fmt.allocPrint(self.allocator, "\"{s}\"", .{stripped});
            return self.cot.addExpr(.{ .literal = .{
                .kind = .string,
                .value = quoted,
                .span = cot_source.Span.zero,
            } });
        }
        // For now, complex templates → string concatenation placeholder
        // TODO: proper string interpolation transform
        return self.cot.addExpr(.{ .literal = .{
            .kind = .string,
            .value = "\"[template]\"",
            .span = cot_source.Span.zero,
        } });
    }

    fn transformArrowFunction(self: *Transformer, af: ts_ast.Expr.ArrowFunction) TransformError!cot_ast.NodeIndex {
        const params = try self.transformParams(af.params);
        const return_type = if (af.return_type != ts_ast.null_node)
            try self.transformTypeNode(af.return_type)
        else
            cot_ast.null_node;
        const body = if (af.body != ts_ast.null_node)
            try self.transformExpr(af.body)
        else
            cot_ast.null_node;

        return self.cot.addExpr(.{ .closure_expr = .{
            .params = params,
            .return_type = return_type,
            .body = body,
            .span = cot_source.Span.zero,
        } });
    }

    fn transformNew(self: *Transformer, n: ts_ast.Expr.NewExpr) TransformError!cot_ast.NodeIndex {
        // Get type name from callee
        const type_name = if (self.ts.getNode(n.callee)) |node| switch (node.*) {
            .expr => |e| switch (e) {
                .ident => |id| id.name,
                else => "Unknown",
            },
            else => "Unknown",
        } else "Unknown";

        var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
        for (n.args) |arg_idx| {
            const a = try self.transformExpr(arg_idx);
            try args.append(self.allocator, a);
        }
        const owned_args = try self.allocator.dupe(cot_ast.NodeIndex, args.items);

        return self.cot.addExpr(.{ .new_expr = .{
            .type_name = type_name,
            .fields = &.{},
            .constructor_args = owned_args,
            .is_constructor = true,
            .span = cot_source.Span.zero,
        } });
    }

    // ================================================================
    // Types
    // ================================================================

    fn transformTypeNode(self: *Transformer, idx: ts_ast.NodeIndex) TransformError!cot_ast.NodeIndex {
        const node = self.ts.getNode(idx) orelse return cot_ast.null_node;
        switch (node.*) {
            .type_node => |t| return self.transformType(t),
            else => return cot_ast.null_node,
        }
    }

    fn transformType(self: *Transformer, t: ts_ast.TypeNode) TransformError!cot_ast.NodeIndex {
        return switch (t) {
            .keyword => |kw| blk: {
                const name: []const u8 = switch (kw.kind) {
                    .number => "f64",
                    .string => "string",
                    .boolean => "bool",
                    .void_kw => "void",
                    .null_kw, .undefined => "void",
                    .any, .unknown => "i64", // Simplified
                    .never => "noreturn",
                    .object => "i64",
                    .symbol => "i64",
                    .bigint => "i64",
                };
                break :blk self.cot.addExpr(.{ .type_expr = .{
                    .kind = .{ .named = name },
                    .span = cot_source.Span.zero,
                } });
            },
            .reference => |ref| self.cot.addExpr(.{ .type_expr = .{
                .kind = .{ .named = ref.name },
                .span = cot_source.Span.zero,
            } }),
            .array => |arr| blk: {
                const elem = try self.transformTypeNode(arr.element);
                break :blk self.cot.addExpr(.{ .type_expr = .{
                    .kind = .{ .list = elem },
                    .span = cot_source.Span.zero,
                } });
            },
            else => self.cot.addExpr(.{ .type_expr = .{
                .kind = .{ .named = "i64" },
                .span = cot_source.Span.zero,
            } }),
        };
    }

    // ================================================================
    // Parameters
    // ================================================================

    fn transformParams(self: *Transformer, params: []const ts_ast.Param) TransformError![]const cot_ast.Field {
        var fields = std.ArrayListUnmanaged(cot_ast.Field){};
        for (params) |p| {
            const name = if (self.ts.getNode(p.binding)) |node| switch (node.*) {
                .expr => |e| switch (e) {
                    .ident => |id| id.name,
                    else => "_",
                },
                else => "_",
            } else "_";

            const type_expr = if (p.type_ann != ts_ast.null_node)
                try self.transformTypeNode(p.type_ann)
            else
                cot_ast.null_node;

            const default_value = if (p.default_value != ts_ast.null_node)
                try self.transformExpr(p.default_value)
            else
                cot_ast.null_node;

            try fields.append(self.allocator, .{
                .name = name,
                .type_expr = type_expr,
                .default_value = default_value,
                .span = cot_source.Span.zero,
            });
        }
        return self.allocator.dupe(cot_ast.Field, fields.items);
    }

    // ================================================================
    // Helpers
    // ================================================================

    fn createMainFunction(self: *Transformer, stmts: []const cot_ast.NodeIndex) TransformError!cot_ast.NodeIndex {
        const owned_stmts = try self.allocator.dupe(cot_ast.NodeIndex, stmts);
        const body = try self.cot.addExpr(.{ .block_expr = .{
            .stmts = owned_stmts,
            .expr = cot_ast.null_node,
            .span = cot_source.Span.zero,
        } });

        const void_type = try self.cot.addExpr(.{ .type_expr = .{
            .kind = .{ .named = "void" },
            .span = cot_source.Span.zero,
        } });

        return self.cot.addDecl(.{ .fn_decl = .{
            .name = "main",
            .params = &.{},
            .return_type = void_type,
            .body = body,
            .is_extern = false,
            .span = cot_source.Span.zero,
        } });
    }

    /// Check if an expression is `console.log`
    fn isConsoleLog(self: *Transformer, idx: ts_ast.NodeIndex) bool {
        const node = self.ts.getNode(idx) orelse return false;
        switch (node.*) {
            .expr => |e| switch (e) {
                .member => |m| {
                    if (!std.mem.eql(u8, m.property, "log")) return false;
                    const base = self.ts.getNode(m.object) orelse return false;
                    switch (base.*) {
                        .expr => |be| switch (be) {
                            .ident => |id| return std.mem.eql(u8, id.name, "console"),
                            else => return false,
                        },
                        else => return false,
                    }
                },
                else => return false,
            },
            else => return false,
        }
    }

    /// Check if an expression is `console.error`
    fn isConsoleError(self: *Transformer, idx: ts_ast.NodeIndex) bool {
        const node = self.ts.getNode(idx) orelse return false;
        switch (node.*) {
            .expr => |e| switch (e) {
                .member => |m| {
                    if (!std.mem.eql(u8, m.property, "error")) return false;
                    const base = self.ts.getNode(m.object) orelse return false;
                    switch (base.*) {
                        .expr => |be| switch (be) {
                            .ident => |id| return std.mem.eql(u8, id.name, "console"),
                            else => return false,
                        },
                        else => return false,
                    }
                },
                else => return false,
            },
            else => return false,
        }
    }
};
