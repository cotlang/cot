//! cot fmt — canonical source formatter.
//! Architecture follows Go's gofmt: parse → walk AST → emit canonical form.
//! Comments are collected from source text and interleaved during emission
//! (Go printer.go commentInfo pattern).

const std = @import("std");
const ast_mod = @import("ast.zig");
const source_mod = @import("source.zig");
const token_mod = @import("token.zig");

const Ast = ast_mod.Ast;
const Node = ast_mod.Node;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Decl = ast_mod.Decl;
const Expr = ast_mod.Expr;
const Stmt = ast_mod.Stmt;
const Token = token_mod.Token;
const Span = source_mod.Span;
const Pos = source_mod.Pos;

pub const Comment = struct {
    text: []const u8,
    offset: u32,
    is_line: bool, // true for //, false for /* */
};

/// Extract comments from source text. Returns a sorted list.
pub fn collectComments(allocator: std.mem.Allocator, content: []const u8) ![]Comment {
    var comments = std.ArrayListUnmanaged(Comment){};
    var i: u32 = 0;
    while (i < content.len) {
        const c = content[i];
        if (c == '"') {
            // Skip string literals
            i += 1;
            while (i < content.len) {
                if (content[i] == '\\') {
                    i += 2;
                } else if (content[i] == '"') {
                    i += 1;
                    break;
                } else {
                    i += 1;
                }
            }
        } else if (c == '\'' and i + 2 < content.len) {
            // Skip char literals
            i += 1;
            if (i < content.len and content[i] == '\\') {
                i += 2;
            } else {
                i += 1;
            }
            if (i < content.len and content[i] == '\'') i += 1;
        } else if (c == '/' and i + 1 < content.len and content[i + 1] == '/') {
            const start = i;
            // Check if this is a doc comment (///) — skip it, handled by AST doc_comment field
            const is_doc = (i + 2 < content.len and content[i + 2] == '/');
            i += 2;
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            if (is_doc) continue; // Doc comments are stored in AST, not in comment list
            // Trim trailing whitespace
            var end = i;
            while (end > start + 2 and (content[end - 1] == ' ' or content[end - 1] == '\t' or content[end - 1] == '\r')) : (end -= 1) {}
            try comments.append(allocator, .{
                .text = content[start..end],
                .offset = start,
                .is_line = true,
            });
        } else if (c == '/' and i + 1 < content.len and content[i + 1] == '*') {
            const start = i;
            i += 2;
            while (i + 1 < content.len) {
                if (content[i] == '*' and content[i + 1] == '/') {
                    i += 2;
                    break;
                }
                i += 1;
            }
            try comments.append(allocator, .{
                .text = content[start..i],
                .offset = start,
                .is_line = false,
            });
        } else {
            i += 1;
        }
    }
    return comments.toOwnedSlice(allocator);
}

pub const Formatter = struct {
    output: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    tree: *const Ast,
    source: []const u8,
    comments: []const Comment,
    comment_idx: usize,
    indent: u32,

    const INDENT = "    "; // 4 spaces

    pub fn init(allocator: std.mem.Allocator, tree: *const Ast, src: []const u8, comments: []const Comment) Formatter {
        return .{
            .output = .{},
            .allocator = allocator,
            .tree = tree,
            .source = src,
            .comments = comments,
            .comment_idx = 0,
            .indent = 0,
        };
    }

    pub fn format(self: *Formatter) ![]u8 {
        const file = self.tree.file orelse return try self.output.toOwnedSlice(self.allocator);

        for (file.decls, 0..) |decl_idx, i| {
            // Flush any comments before this declaration
            const node = self.tree.getNode(decl_idx) orelse continue;
            self.flushCommentsBefore(node.span().start.offset);

            if (i > 0) try self.newline(); // Blank line between top-level decls
            self.printNode(decl_idx);
            try self.newline();
        }

        // Flush any trailing comments
        self.flushCommentsBefore(@intCast(self.source.len));

        // Ensure file ends with exactly one newline
        const out = self.output.items;
        if (out.len == 0 or out[out.len - 1] != '\n') {
            try self.write("\n");
        }
        // Trim multiple trailing newlines to just one
        while (self.output.items.len > 1 and self.output.items[self.output.items.len - 1] == '\n' and self.output.items[self.output.items.len - 2] == '\n') {
            _ = self.output.pop();
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

    // ====================================================================
    // Comment interleaving (Go printer.go pattern)
    // ====================================================================

    fn flushCommentsBefore(self: *Formatter, offset: u32) void {
        while (self.comment_idx < self.comments.len) {
            const c = self.comments[self.comment_idx];
            if (c.offset >= offset) break;
            self.writeIndent();
            self.write(c.text) catch {};
            self.write("\n") catch {};
            self.comment_idx += 1;
        }
    }

    // ====================================================================
    // Node dispatch
    // ====================================================================

    fn printNode(self: *Formatter, idx: NodeIndex) void {
        if (idx == null_node) return;
        const node = self.tree.getNode(idx) orelse return;
        switch (node) {
            .decl => |d| self.printDecl(d),
            .expr => |e| self.printExpr(e),
            .stmt => |s| self.printStmt(s),
        }
    }

    // ====================================================================
    // Declarations
    // ====================================================================

    /// Print doc comment lines (/// prefix) before a declaration.
    fn printDocComment(self: *Formatter, doc: []const u8) void {
        if (doc.len == 0) return;
        // Split on newlines and emit each line as /// line
        var start: usize = 0;
        while (start < doc.len) {
            var end = start;
            while (end < doc.len and doc[end] != '\n') : (end += 1) {}
            self.writeIndent();
            self.write("/// ") catch {};
            self.write(doc[start..end]) catch {};
            self.write("\n") catch {};
            start = if (end < doc.len) end + 1 else end;
        }
    }

    fn printDecl(self: *Formatter, decl: Decl) void {
        switch (decl) {
            .import_decl => |d| self.printImport(d),
            .fn_decl => |d| self.printFnDecl(d),
            .var_decl => |d| self.printVarDecl(d),
            .struct_decl => |d| self.printStructDecl(d),
            .enum_decl => |d| self.printEnumDecl(d),
            .union_decl => |d| self.printUnionDecl(d),
            .type_alias => |d| self.printTypeAlias(d),
            .impl_block => |d| self.printImplBlock(d),
            .trait_decl => |d| self.printTraitDecl(d),
            .impl_trait => |d| self.printImplTrait(d),
            .error_set_decl => |d| self.printErrorSet(d),
            .test_decl => |d| self.printTestDecl(d),
            .bench_decl => |d| self.printBenchDecl(d),
            .bad_decl => {},
        }
    }

    fn printImport(self: *Formatter, d: ast_mod.ImportDecl) void {
        self.writeIndent();
        self.write("import \"") catch {};
        self.write(d.path) catch {};
        self.write("\"") catch {};
    }

    fn printFnDecl(self: *Formatter, d: ast_mod.FnDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        if (d.is_extern) self.write("extern ") catch {};
        self.write("fn ") catch {};
        self.write(d.name) catch {};

        // Type params
        if (d.type_params.len > 0) {
            self.write("(") catch {};
            for (d.type_params, 0..) |tp, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(tp) catch {};
            }
            self.write(")") catch {};
        }

        // Params
        self.write("(") catch {};
        for (d.params, 0..) |p, i| {
            if (i > 0) self.write(", ") catch {};
            self.write(p.name) catch {};
            self.write(": ") catch {};
            self.printNode(p.type_expr);
        }
        self.write(")") catch {};

        // Return type
        if (d.return_type != null_node) {
            self.write(" ") catch {};
            self.printNode(d.return_type);
        }

        // Body
        if (d.body != null_node) {
            self.write(" ") catch {};
            self.printBlock(d.body);
        }
    }

    fn printVarDecl(self: *Formatter, d: ast_mod.VarDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write(if (d.is_const) "const " else "var ") catch {};
        self.write(d.name) catch {};
        if (d.type_expr != null_node) {
            self.write(": ") catch {};
            self.printNode(d.type_expr);
        }
        if (d.value != null_node) {
            self.write(" = ") catch {};
            self.printNode(d.value);
        }
    }

    fn printStructDecl(self: *Formatter, d: ast_mod.StructDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("struct ") catch {};
        self.write(d.name) catch {};
        if (d.type_params.len > 0) {
            self.write("(") catch {};
            for (d.type_params, 0..) |tp, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(tp) catch {};
            }
            self.write(")") catch {};
        }
        if (d.fields.len <= 3) {
            // Single-line form for short structs
            self.write(" { ") catch {};
            for (d.fields, 0..) |f, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(f.name) catch {};
                self.write(": ") catch {};
                self.printNode(f.type_expr);
                if (f.default_value != null_node) {
                    self.write(" = ") catch {};
                    self.printNode(f.default_value);
                }
            }
            self.write(" }") catch {};
        } else {
            // Multi-line form for larger structs
            self.write(" {") catch {};
            self.newline() catch {};
            self.indent += 1;
            for (d.fields, 0..) |f, i| {
                self.printDocComment(f.doc_comment);
                self.writeIndent();
                self.write(f.name) catch {};
                self.write(": ") catch {};
                self.printNode(f.type_expr);
                if (f.default_value != null_node) {
                    self.write(" = ") catch {};
                    self.printNode(f.default_value);
                }
                if (i + 1 < d.fields.len) self.write(",") catch {};
                self.newline() catch {};
            }
            self.indent -= 1;
            self.writeIndent();
            self.write("}") catch {};
        }
    }

    fn printEnumDecl(self: *Formatter, d: ast_mod.EnumDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("enum ") catch {};
        self.write(d.name) catch {};
        if (d.backing_type != null_node) {
            self.write(": ") catch {};
            self.printNode(d.backing_type);
        }
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;
        for (d.variants, 0..) |v, i| {
            self.writeIndent();
            self.write(v.name) catch {};
            if (v.value != null_node) {
                self.write(" = ") catch {};
                self.printNode(v.value);
            }
            if (i + 1 < d.variants.len) self.write(",") catch {};
            self.newline() catch {};
        }
        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printUnionDecl(self: *Formatter, d: ast_mod.UnionDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("union ") catch {};
        self.write(d.name) catch {};
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;
        for (d.variants, 0..) |v, i| {
            self.writeIndent();
            self.write(v.name) catch {};
            if (v.type_expr != null_node) {
                self.write(": ") catch {};
                self.printNode(v.type_expr);
            }
            if (i + 1 < d.variants.len) self.write(",") catch {};
            self.newline() catch {};
        }
        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printTypeAlias(self: *Formatter, d: ast_mod.TypeAlias) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("type ") catch {};
        self.write(d.name) catch {};
        self.write(" = ") catch {};
        self.printNode(d.target);
    }

    fn printImplBlock(self: *Formatter, d: ast_mod.ImplBlock) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("impl ") catch {};
        self.write(d.type_name) catch {};
        if (d.type_params.len > 0) {
            self.write("(") catch {};
            for (d.type_params, 0..) |tp, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(tp) catch {};
            }
            self.write(")") catch {};
        }
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;
        for (d.methods, 0..) |m, i| {
            if (i > 0) {
                self.newline() catch {};
            }
            self.printNode(m);
            self.newline() catch {};
        }
        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printTraitDecl(self: *Formatter, d: ast_mod.TraitDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("trait ") catch {};
        self.write(d.name) catch {};
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;
        for (d.methods, 0..) |m, i| {
            if (i > 0) self.newline() catch {};
            self.printNode(m);
            self.newline() catch {};
        }
        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printImplTrait(self: *Formatter, d: ast_mod.ImplTraitBlock) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("impl ") catch {};
        self.write(d.trait_name) catch {};
        self.write(" for ") catch {};
        self.write(d.target_type) catch {};
        if (d.type_params.len > 0) {
            self.write("(") catch {};
            for (d.type_params, 0..) |tp, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(tp) catch {};
            }
            self.write(")") catch {};
        }
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;
        for (d.methods, 0..) |m, i| {
            if (i > 0) self.newline() catch {};
            self.printNode(m);
            self.newline() catch {};
        }
        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printErrorSet(self: *Formatter, d: ast_mod.ErrorSetDecl) void {
        self.printDocComment(d.doc_comment);
        self.writeIndent();
        self.write("const ") catch {};
        self.write(d.name) catch {};
        self.write(" = error { ") catch {};
        for (d.variants, 0..) |v, i| {
            if (i > 0) self.write(", ") catch {};
            self.write(v) catch {};
        }
        self.write(" }") catch {};
    }

    fn printTestDecl(self: *Formatter, d: ast_mod.TestDecl) void {
        self.writeIndent();
        self.write("test \"") catch {};
        self.write(d.name) catch {};
        self.write("\" ") catch {};
        self.printBlock(d.body);
    }

    fn printBenchDecl(self: *Formatter, d: ast_mod.BenchDecl) void {
        self.writeIndent();
        self.write("bench \"") catch {};
        self.write(d.name) catch {};
        self.write("\" ") catch {};
        self.printBlock(d.body);
    }

    // ====================================================================
    // Statements
    // ====================================================================

    fn printStmt(self: *Formatter, stmt: Stmt) void {
        switch (stmt) {
            .expr_stmt => |s| {
                self.writeIndent();
                self.printNode(s.expr);
            },
            .return_stmt => |s| {
                self.writeIndent();
                self.write("return") catch {};
                if (s.value != null_node) {
                    self.write(" ") catch {};
                    self.printNode(s.value);
                }
            },
            .var_stmt => |s| {
                self.writeIndent();
                self.write(if (s.is_const) "const " else "var ") catch {};
                self.write(s.name) catch {};
                if (s.type_expr != null_node) {
                    self.write(": ") catch {};
                    self.printNode(s.type_expr);
                }
                if (s.value != null_node) {
                    self.write(" = ") catch {};
                    self.printNode(s.value);
                }
            },
            .destructure_stmt => |s| {
                self.writeIndent();
                self.write(if (s.is_const) "const " else "var ") catch {};
                for (s.bindings, 0..) |b, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.write(b.name) catch {};
                    if (b.type_expr != null_node) {
                        self.write(": ") catch {};
                        self.printNode(b.type_expr);
                    }
                }
                if (s.value != null_node) {
                    self.write(" = ") catch {};
                    self.printNode(s.value);
                }
            },
            .assign_stmt => |s| {
                self.writeIndent();
                self.printNode(s.target);
                self.write(" ") catch {};
                self.write(s.op.string()) catch {};
                self.write(" ") catch {};
                self.printNode(s.value);
            },
            .if_stmt => |s| {
                self.writeIndent();
                self.write("if (") catch {};
                self.printNode(s.condition);
                self.write(")") catch {};
                if (s.capture.len > 0) {
                    // Zig pattern: if (expr) |val| { ... }
                    self.write(" |") catch {};
                    self.write(s.capture) catch {};
                    self.write("|") catch {};
                }
                self.write(" ") catch {};
                self.printBlock(s.then_branch);
                if (s.else_branch != null_node) {
                    self.write(" else ") catch {};
                    const else_node = self.tree.getNode(s.else_branch);
                    if (else_node) |en| {
                        switch (en) {
                            .stmt => |es| switch (es) {
                                .if_stmt => {
                                    self.printStmt(es);
                                    return;
                                },
                                else => {},
                            },
                            else => {},
                        }
                    }
                    self.printBlock(s.else_branch);
                }
            },
            .while_stmt => |s| {
                self.writeIndent();
                if (s.label) |label| {
                    self.write(label) catch {};
                    self.write(": ") catch {};
                }
                self.write("while (") catch {};
                self.printNode(s.condition);
                self.write(")") catch {};
                if (s.capture.len > 0) {
                    self.write(" |") catch {};
                    self.write(s.capture) catch {};
                    self.write("|") catch {};
                }
                if (s.continue_expr != null_node) {
                    self.write(" : (") catch {};
                    self.printNode(s.continue_expr);
                    self.write(")") catch {};
                }
                self.write(" ") catch {};
                self.printBlock(s.body);
            },
            .for_stmt => |s| {
                self.writeIndent();
                if (s.label) |label| {
                    self.write(label) catch {};
                    self.write(": ") catch {};
                }
                if (s.is_inline) self.write("inline ") catch {};
                self.write("for ") catch {};
                if (s.index_binding) |idx| {
                    self.write(idx) catch {};
                    self.write(", ") catch {};
                }
                self.write(s.binding) catch {};
                self.write(" in ") catch {};
                if (s.isRange()) {
                    self.printNode(s.range_start);
                    self.write("..") catch {};
                    self.printNode(s.range_end);
                } else {
                    self.printNode(s.iterable);
                }
                self.write(" ") catch {};
                self.printBlock(s.body);
            },
            .block_stmt => |s| {
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;
                for (s.stmts) |stmt_idx| {
                    self.printNode(stmt_idx);
                    self.newline() catch {};
                }
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            .break_stmt => |s| {
                self.writeIndent();
                self.write("break") catch {};
                if (s.label) |label| {
                    self.write(" :") catch {};
                    self.write(label) catch {};
                }
            },
            .continue_stmt => |s| {
                self.writeIndent();
                self.write("continue") catch {};
                if (s.label) |label| {
                    self.write(" :") catch {};
                    self.write(label) catch {};
                }
            },
            .defer_stmt => |s| {
                self.writeIndent();
                self.write(if (s.is_errdefer) "errdefer " else "defer ") catch {};
                self.printNode(s.expr);
            },
            .bad_stmt => {},
        }
    }

    // ====================================================================
    // Expressions
    // ====================================================================

    fn printExpr(self: *Formatter, expr: Expr) void {
        switch (expr) {
            .ident => |e| self.write(e.name) catch {},
            .literal => |e| self.write(e.value) catch {},
            .binary => |e| {
                self.printNode(e.left);
                self.write(" ") catch {};
                self.write(e.op.string()) catch {};
                self.write(" ") catch {};
                self.printNode(e.right);
            },
            .unary => |e| {
                self.write(e.op.string()) catch {};
                // Space after keyword-style unary ops
                if (e.op == .kw_not) self.write(" ") catch {};
                self.printNode(e.operand);
            },
            .call => |e| {
                self.printNode(e.callee);
                self.write("(") catch {};
                for (e.args, 0..) |arg, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(arg);
                }
                self.write(")") catch {};
            },
            .index => |e| {
                self.printNode(e.base);
                self.write("[") catch {};
                self.printNode(e.idx);
                self.write("]") catch {};
            },
            .slice_expr => |e| {
                self.printNode(e.base);
                self.write("[") catch {};
                if (e.start != null_node) self.printNode(e.start);
                self.write("..") catch {};
                if (e.end != null_node) self.printNode(e.end);
                self.write("]") catch {};
            },
            .field_access => |e| {
                self.printNode(e.base);
                self.write(".") catch {};
                self.write(e.field) catch {};
            },
            .array_literal => |e| {
                self.write("[") catch {};
                for (e.elements, 0..) |elem, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(elem);
                }
                self.write("]") catch {};
            },
            .paren => |e| {
                self.write("(") catch {};
                self.printNode(e.inner);
                self.write(")") catch {};
            },
            .if_expr => |e| {
                self.write("if (") catch {};
                self.printNode(e.condition);
                self.write(")") catch {};
                if (e.capture.len > 0) {
                    // Zig pattern: if (expr) |val| ...
                    self.write(" |") catch {};
                    self.write(e.capture) catch {};
                    self.write("|") catch {};
                }
                self.write(" ") catch {};
                self.printNode(e.then_branch);
                if (e.else_branch != null_node) {
                    self.write(" else ") catch {};
                    self.printNode(e.else_branch);
                }
            },
            .switch_expr => |e| {
                self.write("switch ") catch {};
                self.printNode(e.subject);
                self.write(" {") catch {};
                self.newline() catch {};
                self.indent += 1;
                for (e.cases) |case| {
                    self.writeIndent();
                    if (case.is_range and case.patterns.len >= 2) {
                        // Range pattern: start..end
                        self.printNode(case.patterns[0]);
                        self.write("..") catch {};
                        self.printNode(case.patterns[1]);
                    } else {
                        for (case.patterns, 0..) |pat, i| {
                            if (i > 0) self.write(", ") catch {};
                            self.printNode(pat);
                        }
                    }
                    if (case.capture.len > 0) {
                        self.write(" |") catch {};
                        self.write(case.capture) catch {};
                        self.write("|") catch {};
                    }
                    if (case.guard != null_node) {
                        self.write(" if ") catch {};
                        self.printNode(case.guard);
                    }
                    self.write(" => ") catch {};
                    self.printNode(case.body);
                    self.newline() catch {};
                }
                if (e.else_body != null_node) {
                    self.writeIndent();
                    self.write("else => ") catch {};
                    self.printNode(e.else_body);
                    self.newline() catch {};
                }
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            .block_expr => |e| {
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;
                for (e.stmts) |stmt_idx| {
                    self.printNode(stmt_idx);
                    self.newline() catch {};
                }
                if (e.expr != null_node) {
                    self.writeIndent();
                    self.printNode(e.expr);
                    self.newline() catch {};
                }
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            .struct_init => |e| {
                self.write(e.type_name) catch {};
                if (e.type_args.len > 0) {
                    self.write("(") catch {};
                    for (e.type_args, 0..) |ta, i| {
                        if (i > 0) self.write(", ") catch {};
                        self.printNode(ta);
                    }
                    self.write(")") catch {};
                }
                self.write(" { ") catch {};
                for (e.fields, 0..) |f, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.write(".") catch {};
                    self.write(f.name) catch {};
                    self.write(" = ") catch {};
                    self.printNode(f.value);
                }
                self.write(" }") catch {};
            },
            .new_expr => |e| {
                self.write("new ") catch {};
                self.write(e.type_name) catch {};
                if (e.type_args.len > 0) {
                    self.write("(") catch {};
                    for (e.type_args, 0..) |ta, i| {
                        if (i > 0) self.write(", ") catch {};
                        self.printNode(ta);
                    }
                    self.write(")") catch {};
                }
                self.write(" { ") catch {};
                for (e.fields, 0..) |f, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.write(f.name) catch {};
                    self.write(": ") catch {};
                    self.printNode(f.value);
                }
                self.write(" }") catch {};
            },
            .builtin_call => |e| {
                self.write("@") catch {};
                self.write(e.kind.sourceName()) catch {};
                self.write("(") catch {};
                var first = true;
                if (e.type_arg != null_node) {
                    self.printNode(e.type_arg);
                    first = false;
                }
                for (e.args) |arg| {
                    if (arg == null_node) continue;
                    if (!first) self.write(", ") catch {};
                    self.printNode(arg);
                    first = false;
                }
                self.write(")") catch {};
            },
            .string_interp => |e| {
                // String interpolation — reproduce from source
                self.write(self.source[e.span.start.offset..e.span.end.offset]) catch {};
            },
            .type_expr => |e| self.printTypeExpr(e),
            .try_expr => |e| {
                self.write("try ") catch {};
                self.printNode(e.operand);
            },
            .await_expr => |e| {
                self.write("await ") catch {};
                self.printNode(e.operand);
            },
            .catch_expr => |e| {
                self.printNode(e.operand);
                self.write(" catch ") catch {};
                if (e.capture.len > 0) {
                    self.write("|") catch {};
                    self.write(e.capture) catch {};
                    self.write("| ") catch {};
                }
                self.printNode(e.fallback);
            },
            .error_literal => |e| {
                self.write("error.") catch {};
                self.write(e.error_name) catch {};
            },
            .closure_expr => |e| {
                self.write("fn(") catch {};
                for (e.params, 0..) |p, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.write(p.name) catch {};
                    if (p.type_expr != null_node) {
                        self.write(": ") catch {};
                        self.printNode(p.type_expr);
                    }
                }
                self.write(")") catch {};
                if (e.return_type != null_node) {
                    self.write(" ") catch {};
                    self.printNode(e.return_type);
                }
                self.write(" ") catch {};
                self.printNode(e.body);
            },
            .tuple_literal => |e| {
                self.write("(") catch {};
                for (e.elements, 0..) |elem, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(elem);
                }
                self.write(")") catch {};
            },
            .comptime_block => |e| {
                self.write("comptime ") catch {};
                self.printNode(e.body);
            },
            .zero_init => {
                // ZeroInit is a bare {} — reproduce from source
                self.write("{}") catch {};
            },
            .addr_of => |e| {
                self.write("&") catch {};
                self.printNode(e.operand);
            },
            .deref => |e| {
                self.printNode(e.operand);
                self.write(".*") catch {};
            },
            .bad_expr => {},
        }
    }

    fn printTypeExpr(self: *Formatter, te: ast_mod.TypeExpr) void {
        switch (te.kind) {
            .named => |name| self.write(name) catch {},
            .pointer => |inner| {
                self.write("*") catch {};
                self.printNode(inner);
            },
            .optional => |inner| {
                self.write("?") catch {};
                self.printNode(inner);
            },
            .slice => |elem| {
                self.write("[]") catch {};
                self.printNode(elem);
            },
            .array => |info| {
                self.write("[") catch {};
                self.printNode(info.size);
                self.write("]") catch {};
                self.printNode(info.elem);
            },
            .list => |elem| {
                self.write("List(") catch {};
                self.printNode(elem);
                self.write(")") catch {};
            },
            .function => |info| {
                self.write("fn(") catch {};
                for (info.params, 0..) |p, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(p);
                }
                self.write(")") catch {};
                if (info.ret != null_node) {
                    self.write(" -> ") catch {};
                    self.printNode(info.ret);
                }
            },
            .generic_instance => |gi| {
                self.write(gi.name) catch {};
                self.write("(") catch {};
                for (gi.type_args, 0..) |ta, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(ta);
                }
                self.write(")") catch {};
            },
            .error_union => |eu| {
                if (eu.error_set != null_node) {
                    self.printNode(eu.error_set);
                }
                self.write("!") catch {};
                self.printNode(eu.elem);
            },
            .tuple => |elems| {
                self.write("(") catch {};
                for (elems, 0..) |e, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(e);
                }
                self.write(")") catch {};
            },
            .map => |info| {
                self.write("Map(") catch {};
                self.printNode(info.key);
                self.write(", ") catch {};
                self.printNode(info.value);
                self.write(")") catch {};
            },
        }
    }

    // ====================================================================
    // Block helper
    // ====================================================================

    fn printBlock(self: *Formatter, idx: NodeIndex) void {
        if (idx == null_node) return;
        const node = self.tree.getNode(idx) orelse return;
        switch (node) {
            .stmt => |s| switch (s) {
                .block_stmt => |b| {
                    self.write("{") catch {};
                    self.newline() catch {};
                    self.indent += 1;
                    for (b.stmts) |stmt_idx| {
                        const stmt_node = self.tree.getNode(stmt_idx);
                        if (stmt_node) |sn| self.flushCommentsBefore(sn.span().start.offset);
                        self.printNode(stmt_idx);
                        self.newline() catch {};
                    }
                    self.indent -= 1;
                    self.writeIndent();
                    self.write("}") catch {};
                    return;
                },
                else => {},
            },
            .expr => |e| switch (e) {
                .block_expr => |b| {
                    self.write("{") catch {};
                    self.newline() catch {};
                    self.indent += 1;
                    for (b.stmts) |stmt_idx| {
                        const stmt_node = self.tree.getNode(stmt_idx);
                        if (stmt_node) |sn| self.flushCommentsBefore(sn.span().start.offset);
                        self.printNode(stmt_idx);
                        self.newline() catch {};
                    }
                    if (b.expr != null_node) {
                        self.writeIndent();
                        self.printNode(b.expr);
                        self.newline() catch {};
                    }
                    self.indent -= 1;
                    self.writeIndent();
                    self.write("}") catch {};
                    return;
                },
                else => {},
            },
            else => {},
        }
        // Fallback: just print the node
        self.printNode(idx);
    }

    // ====================================================================
    // Output helpers
    // ====================================================================

    fn write(self: *Formatter, s: []const u8) !void {
        try self.output.appendSlice(self.allocator, s);
    }

    fn newline(self: *Formatter) !void {
        try self.output.append(self.allocator, '\n');
    }

    fn writeIndent(self: *Formatter) void {
        var i: u32 = 0;
        while (i < self.indent) : (i += 1) {
            self.write(INDENT) catch {};
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "format: basic function" {
    const allocator = std.testing.allocator;
    const source_mod2 = @import("source.zig");
    const scanner_mod = @import("scanner.zig");
    const parser_mod = @import("parser.zig");
    const errors_mod = @import("errors.zig");

    const input = "fn   main( ) {  return   42  }";
    var src = source_mod2.Source.init(allocator, "test.cot", input);
    defer src.deinit();
    var err = errors_mod.ErrorReporter.init(&src, null);
    var tree = Ast.init(allocator);
    defer tree.deinit();
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);
    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    try parser.parseFile();

    const comments = try collectComments(allocator, input);
    defer allocator.free(comments);
    var formatter = Formatter.init(allocator, &tree, input, comments);
    const output = try formatter.format();
    defer allocator.free(output);

    try std.testing.expectEqualStrings("fn main() {\n    return 42\n}\n", output);
}

test "format: idempotent" {
    const allocator = std.testing.allocator;
    const source_mod2 = @import("source.zig");
    const scanner_mod = @import("scanner.zig");
    const parser_mod = @import("parser.zig");
    const errors_mod = @import("errors.zig");

    const input = "fn main() {\n    return 42\n}\n";

    // First format
    var src1 = source_mod2.Source.init(allocator, "test.cot", input);
    defer src1.deinit();
    var err1 = errors_mod.ErrorReporter.init(&src1, null);
    var tree1 = Ast.init(allocator);
    defer tree1.deinit();
    var scan1 = scanner_mod.Scanner.initWithErrors(&src1, &err1);
    var p1 = parser_mod.Parser.init(allocator, &scan1, &tree1, &err1);
    try p1.parseFile();
    const c1 = try collectComments(allocator, input);
    defer allocator.free(c1);
    var f1 = Formatter.init(allocator, &tree1, input, c1);
    const output1 = try f1.format();
    defer allocator.free(output1);

    // Second format
    var src2 = source_mod2.Source.init(allocator, "test.cot", output1);
    defer src2.deinit();
    var err2 = errors_mod.ErrorReporter.init(&src2, null);
    var tree2 = Ast.init(allocator);
    defer tree2.deinit();
    var scan2 = scanner_mod.Scanner.initWithErrors(&src2, &err2);
    var p2 = parser_mod.Parser.init(allocator, &scan2, &tree2, &err2);
    try p2.parseFile();
    const c2 = try collectComments(allocator, output1);
    defer allocator.free(c2);
    var f2 = Formatter.init(allocator, &tree2, output1, c2);
    const output2 = try f2.format();
    defer allocator.free(output2);

    try std.testing.expectEqualStrings(output1, output2);
}

test "format: comment preservation" {
    const allocator = std.testing.allocator;
    const source_mod2 = @import("source.zig");
    const scanner_mod = @import("scanner.zig");
    const parser_mod = @import("parser.zig");
    const errors_mod = @import("errors.zig");

    const input = "// top comment\nfn main() {\n    return 0\n}\n";
    var src = source_mod2.Source.init(allocator, "test.cot", input);
    defer src.deinit();
    var err = errors_mod.ErrorReporter.init(&src, null);
    var tree = Ast.init(allocator);
    defer tree.deinit();
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);
    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    try parser.parseFile();

    const comments = try collectComments(allocator, input);
    defer allocator.free(comments);
    var formatter = Formatter.init(allocator, &tree, input, comments);
    const output = try formatter.format();
    defer allocator.free(output);

    // Comment should be preserved
    try std.testing.expect(std.mem.indexOf(u8, output, "// top comment") != null);
}
