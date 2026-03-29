//! Canonical source formatter for Cot.
//!
//! Architecture follows Go's gofmt: parse, walk the compact AST, emit canonical
//! form. Comments are collected from source text and interleaved during emission
//! (Go printer.go commentInfo pattern).

const std = @import("std");
const ast_mod = @import("ast.zig");
const source_mod = @import("foundation").source;
const tok = @import("token.zig");

const Ast = ast_mod.Ast;
const Index = ast_mod.Index;
const OptionalIndex = ast_mod.OptionalIndex;
const SubRange = ast_mod.SubRange;
const ExtraIndex = ast_mod.ExtraIndex;
const TokenIndex = ast_mod.TokenIndex;
const OptionalTokenIndex = ast_mod.OptionalTokenIndex;
const Tag = ast_mod.Node.Tag;
const Token = tok.Token;

pub const Comment = struct {
    text: []const u8,
    offset: u32,
    is_line: bool,
};

/// Extract comments from source text. Returns a sorted list.
pub fn collectComments(allocator: std.mem.Allocator, content: []const u8) ![]Comment {
    var comments = std.ArrayListUnmanaged(Comment){};
    var i: u32 = 0;
    while (i < content.len) {
        const c = content[i];
        if (c == '"') {
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
            i += 1;
            if (i < content.len and content[i] == '\\') {
                i += 2;
            } else {
                i += 1;
            }
            if (i < content.len and content[i] == '\'') i += 1;
        } else if (c == '/' and i + 1 < content.len and content[i + 1] == '/') {
            const start = i;
            const is_doc = (i + 2 < content.len and content[i + 2] == '/');
            i += 2;
            while (i < content.len and content[i] != '\n') : (i += 1) {}
            if (is_doc) continue;
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

/// Walks the compact AST and emits canonical source form.
pub const Formatter = struct {
    output: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,
    tree: *const Ast,
    source: []const u8,
    comments: []const Comment,
    comment_idx: usize,
    indent: u32,

    const INDENT = "    ";

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

    /// Format the entire file and return owned output.
    pub fn format(self: *Formatter) ![]u8 {
        const root_tag = self.tree.nodeTag(@enumFromInt(0));
        if (root_tag != .root) return try self.output.toOwnedSlice(self.allocator);

        const root_data = self.tree.nodeData(@enumFromInt(0));
        const decls = self.tree.extraNodes(root_data.extra_range);

        for (decls, 0..) |decl_idx, i| {
            const offset = self.tree.tokenStart(self.tree.nodeMainToken(decl_idx));
            self.flushCommentsBefore(offset);

            if (i > 0) self.newline() catch {};
            self.printNode(decl_idx);
            self.newline() catch {};
        }

        self.flushCommentsBefore(@intCast(self.source.len));

        const out = self.output.items;
        if (out.len == 0 or out[out.len - 1] != '\n') {
            self.write("\n") catch {};
        }
        while (self.output.items.len > 1 and self.output.items[self.output.items.len - 1] == '\n' and self.output.items[self.output.items.len - 2] == '\n') {
            _ = self.output.pop();
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

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

    fn printNode(self: *Formatter, idx: Index) void {
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        switch (tag) {
            .root => {},
            .bad_node => {},

            .fn_decl, .fn_decl_extern => self.printFnDecl(idx),
            .var_decl => self.printVarDecl(idx),
            .struct_decl => self.printStructDecl(idx),
            .enum_decl => self.printEnumDecl(idx),
            .union_decl => self.printUnionDecl(idx),
            .type_alias, .type_alias_distinct => self.printTypeAlias(idx),
            .import_decl => self.printImport(idx),
            .impl_block => self.printImplBlock(idx),
            .trait_decl => self.printTraitDecl(idx),
            .impl_trait => self.printImplTrait(idx),
            .error_set_decl => self.printErrorSet(idx),
            .test_decl => self.printTestDecl(idx),
            .bench_decl => self.printBenchDecl(idx),
            .unchecked_sendable => {
                self.writeIndent();
                self.write("impl @unchecked Sendable for ") catch {};
                self.write(self.tree.tokenSlice(data.token)) catch {};
                self.write(" {}\n") catch {};
            },

            .ident => self.write(self.tree.tokenSlice(self.tree.nodeMainToken(idx))) catch {},
            .literal_int, .literal_float, .literal_string, .literal_char => {
                self.write(self.tree.tokenSlice(self.tree.nodeMainToken(idx))) catch {};
            },
            .literal_true => self.write("true") catch {},
            .literal_false => self.write("false") catch {},
            .literal_null => self.write("null") catch {},
            .literal_undefined => self.write("undefined") catch {},
            .literal_unreachable => self.write("unreachable") catch {},
            .error_literal => {
                self.write("error.") catch {};
                self.write(self.tree.tokenSlice(data.token)) catch {};
            },

            .binary_add,
            .binary_sub,
            .binary_mul,
            .binary_div,
            .binary_mod,
            .binary_eq,
            .binary_neq,
            .binary_lt,
            .binary_gt,
            .binary_lte,
            .binary_gte,
            .binary_and,
            .binary_or,
            .binary_bit_and,
            .binary_bit_or,
            .binary_bit_xor,
            .binary_shl,
            .binary_shr,
            .binary_concat,
            .binary_pipe,
            => {
                const pair = data.node_and_node;
                self.printNode(pair[0]);
                self.write(" ") catch {};
                self.write(binaryOpString(tag)) catch {};
                self.write(" ") catch {};
                self.printNode(pair[1]);
            },

            .unary_neg => {
                self.write("-") catch {};
                self.printNode(data.node);
            },
            .unary_not => {
                self.write("not ") catch {};
                self.printNode(data.node);
            },
            .unary_bit_not => {
                self.write("~") catch {};
                self.printNode(data.node);
            },
            .unary_addr_of => {
                self.write("&") catch {};
                self.printNode(data.node);
            },
            .unary_deref => {
                self.printNode(data.node);
                self.write(".*") catch {};
            },
            .unary_try => {
                self.write("try ") catch {};
                self.printNode(data.node);
            },
            .unary_await => {
                self.write("await ") catch {};
                self.printNode(data.node);
            },
            .unary_unwrap => {
                self.printNode(data.node);
                self.write(".?") catch {};
            },

            .call_zero, .call_one, .call => self.printCall(idx),

            .index => {
                const pair = data.node_and_node;
                self.printNode(pair[0]);
                self.write("[") catch {};
                self.printNode(pair[1]);
                self.write("]") catch {};
            },
            .slice => {
                const base = data.node_and_extra[0];
                const s = self.tree.extraData(data.node_and_extra[1], ast_mod.SliceData);
                self.printNode(base);
                self.write("[") catch {};
                if (s.start.unwrap()) |start| self.printNode(start);
                self.write("..") catch {};
                if (s.end.unwrap()) |end| self.printNode(end);
                self.write("]") catch {};
            },
            .field_access => {
                const pair = data.node_and_token;
                self.printNode(pair[0]);
                self.write(".") catch {};
                self.write(self.tree.tokenSlice(pair[1])) catch {};
            },

            .array_literal_empty => self.write("[]") catch {},
            .array_literal_one => {
                self.write("[") catch {};
                self.printNode(data.node);
                self.write("]") catch {};
            },
            .array_literal => {
                self.write("[") catch {};
                const elems = self.tree.extraNodes(data.extra_range);
                for (elems, 0..) |elem, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(elem);
                }
                self.write("]") catch {};
            },
            .tuple_literal => {
                self.write("(") catch {};
                const elems = self.tree.extraNodes(data.extra_range);
                for (elems, 0..) |elem, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(elem);
                }
                self.write(")") catch {};
            },

            .paren => {
                self.write("(") catch {};
                self.printNode(data.node);
                self.write(")") catch {};
            },

            .if_simple, .if_full => self.printIfExpr(idx),
            .switch_expr => self.printSwitch(idx),

            .block_one, .block_two, .block, .block_stmt => self.printBlockInline(idx),

            .struct_init_one => self.printStructInitOne(idx),
            .struct_init => self.printStructInit(idx),
            .new_expr => self.printNewExpr(idx),
            .builtin_call => self.printBuiltinCall(idx),
            .string_interp => {
                const main_token = self.tree.nodeMainToken(idx);
                const start_offset = self.tree.tokenStart(main_token);
                const span = self.tree.nodeSpan(idx);
                self.write(self.source[start_offset..span.end.offset]) catch {};
            },

            .type_named => self.write(self.tree.tokenSlice(self.tree.nodeMainToken(idx))) catch {},
            .type_pointer => {
                self.write("*") catch {};
                self.printNode(data.node);
            },
            .type_optional => {
                self.write("?") catch {};
                self.printNode(data.node);
            },
            .type_error_union => {
                self.write("!") catch {};
                self.printNode(data.node);
            },
            .type_error_union_set => {
                const pair = data.node_and_node;
                self.printNode(pair[0]);
                self.write("!") catch {};
                self.printNode(pair[1]);
            },
            .type_slice => {
                self.write("[]") catch {};
                self.printNode(data.node);
            },
            .type_array => {
                const pair = data.node_and_node;
                self.write("[") catch {};
                self.printNode(pair[0]);
                self.write("]") catch {};
                self.printNode(pair[1]);
            },
            .type_map => {
                const pair = data.node_and_node;
                self.write("Map(") catch {};
                self.printNode(pair[0]);
                self.write(", ") catch {};
                self.printNode(pair[1]);
                self.write(")") catch {};
            },
            .type_list => {
                self.write("List(") catch {};
                self.printNode(data.node);
                self.write(")") catch {};
            },
            .type_function => self.printFnType(idx),
            .type_tuple => {
                self.write("(") catch {};
                const elems = self.tree.extraNodes(data.extra_range);
                for (elems, 0..) |elem, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(elem);
                }
                self.write(")") catch {};
            },
            .type_generic => self.printGenericType(idx),
            .type_existential => {
                self.write("any ") catch {};
                self.printNode(data.node);
            },

            .catch_expr => self.printCatch(idx),
            .orelse_expr => self.printOrElse(idx),
            .closure_expr => self.printClosure(idx),
            .comptime_block => {
                self.write("comptime ") catch {};
                self.printNode(data.node);
            },
            .task_expr => self.printTask(idx),
            .zero_init => self.write("{}") catch {},

            .expr_stmt => {
                self.writeIndent();
                self.printNode(data.node);
            },
            .return_expr => {
                self.writeIndent();
                self.write("return ") catch {};
                self.printNode(data.node);
            },
            .return_void => {
                self.writeIndent();
                self.write("return") catch {};
            },

            .var_local, .const_local => self.printLocalVar(idx),

            .assign => {
                self.writeIndent();
                const pair = data.node_and_node;
                self.printNode(pair[0]);
                self.write(" = ") catch {};
                self.printNode(pair[1]);
            },
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            .assign_mod,
            .assign_bit_and,
            .assign_bit_or,
            .assign_bit_xor,
            => {
                self.writeIndent();
                const pair = data.node_and_node;
                self.printNode(pair[0]);
                self.write(" ") catch {};
                self.write(assignOpString(tag)) catch {};
                self.write(" ") catch {};
                self.printNode(pair[1]);
            },

            .if_stmt_simple, .if_stmt => self.printIfStmt(idx),
            .while_stmt => self.printWhile(idx),
            .for_stmt => self.printFor(idx),

            .break_plain => {
                self.writeIndent();
                self.write("break") catch {};
            },
            .break_expr => self.printBreak(idx),
            .continue_plain => {
                self.writeIndent();
                self.write("continue") catch {};
            },
            .continue_labeled => {
                self.writeIndent();
                self.write("continue :") catch {};
                self.write(self.tree.tokenSlice(data.token)) catch {};
            },

            .defer_stmt => {
                self.writeIndent();
                self.write("defer ") catch {};
                self.printNode(data.node);
            },
            .errdefer_stmt => {
                self.writeIndent();
                self.write("errdefer ") catch {};
                self.printNode(data.node);
            },

            .async_let => {
                self.writeIndent();
                const pair = data.token_and_node;
                self.write("async let ") catch {};
                self.write(self.tree.tokenSlice(pair[0])) catch {};
                self.write(" = ") catch {};
                self.printNode(pair[1]);
            },

            .destructure => self.printDestructure(idx),
        }
    }

    fn printFnDecl(self: *Formatter, idx: Index) void {
        const f = self.tree.fnDeclData(idx);

        if (f.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));

        self.writeIndent();
        if (f.is_extern) self.write("extern ") catch {};
        self.write("fn ") catch {};
        self.write(f.name) catch {};

        const tp_nodes = self.tree.extraSlice(f.type_params);
        if (tp_nodes.len > 0) {
            self.write("(") catch {};
            for (tp_nodes, 0..) |tp_tok, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(self.tree.tokenSlice(tp_tok)) catch {};
            }
            self.write(")") catch {};
        }

        self.write("(") catch {};
        const param_extras = self.tree.extraSlice(f.params);
        for (param_extras, 0..) |param_ei, i| {
            if (i > 0) self.write(", ") catch {};
            const field = self.tree.extraData(@enumFromInt(param_ei), ast_mod.Field);
            self.write(self.tree.tokenSlice(field.name_token)) catch {};
            self.write(": ") catch {};
            self.printNode(field.type_expr);
        }
        self.write(")") catch {};

        if (f.return_type.unwrap()) |rt| {
            self.write(" ") catch {};
            self.printNode(rt);
        }

        if (f.body.unwrap()) |body| {
            self.write(" ") catch {};
            self.printBlock(body);
        }
    }

    fn printVarDecl(self: *Formatter, idx: Index) void {
        const v = self.tree.varDeclData(idx);
        if (v.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write(if (v.is_const) "const " else "var ") catch {};
        self.write(v.name) catch {};
        if (v.type_expr.unwrap()) |te| {
            self.write(": ") catch {};
            self.printNode(te);
        }
        if (v.value.unwrap()) |val| {
            self.write(" = ") catch {};
            self.printNode(val);
        }
    }

    fn printStructDecl(self: *Formatter, idx: Index) void {
        const d = self.tree.structDeclData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        switch (d.layout) {
            .@"packed" => self.write("packed struct ") catch {},
            .@"extern" => self.write("extern struct ") catch {},
            .auto => self.write("struct ") catch {},
        }
        self.write(d.name) catch {};

        const tp_nodes = self.tree.extraSlice(d.type_params);
        if (tp_nodes.len > 0) {
            self.write("(") catch {};
            for (tp_nodes, 0..) |tp_tok, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(self.tree.tokenSlice(tp_tok)) catch {};
            }
            self.write(")") catch {};
        }

        const field_extras = self.tree.extraSlice(d.fields);
        if (field_extras.len <= 3) {
            self.write(" { ") catch {};
            for (field_extras, 0..) |fei, i| {
                if (i > 0) self.write(", ") catch {};
                const field = self.tree.extraData(@enumFromInt(fei), ast_mod.Field);
                self.write(self.tree.tokenSlice(field.name_token)) catch {};
                self.write(": ") catch {};
                self.printNode(field.type_expr);
                if (field.default_value.unwrap()) |dv| {
                    self.write(" = ") catch {};
                    self.printNode(dv);
                }
            }
            self.write(" }") catch {};
        } else {
            self.write(" {") catch {};
            self.newline() catch {};
            self.indent += 1;
            for (field_extras, 0..) |fei, i| {
                const field = self.tree.extraData(@enumFromInt(fei), ast_mod.Field);
                self.writeIndent();
                self.write(self.tree.tokenSlice(field.name_token)) catch {};
                self.write(": ") catch {};
                self.printNode(field.type_expr);
                if (field.default_value.unwrap()) |dv| {
                    self.write(" = ") catch {};
                    self.printNode(dv);
                }
                if (i + 1 < field_extras.len) self.write(",") catch {};
                self.newline() catch {};
            }
            self.indent -= 1;
            self.writeIndent();
            self.write("}") catch {};
        }
    }

    fn printEnumDecl(self: *Formatter, idx: Index) void {
        const d = self.tree.enumDeclData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("enum ") catch {};
        self.write(d.name) catch {};
        if (d.backing_type.unwrap()) |bt| {
            self.write(": ") catch {};
            self.printNode(bt);
        }
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const variant_extras = self.tree.extraSlice(d.variants);
        const nested_decl_nodes = self.tree.extraNodes(d.nested_decls);
        for (variant_extras, 0..) |vei, i| {
            const v = self.tree.extraData(@enumFromInt(vei), ast_mod.EnumVariant);
            self.writeIndent();
            self.write(self.tree.tokenSlice(v.name_token)) catch {};
            if (v.value.unwrap()) |val| {
                self.write(" = ") catch {};
                self.printNode(val);
            }
            if (i + 1 < variant_extras.len or nested_decl_nodes.len > 0) self.write(",") catch {};
            self.newline() catch {};
        }
        for (nested_decl_nodes) |nested_idx| {
            self.printNode(nested_idx);
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printUnionDecl(self: *Formatter, idx: Index) void {
        const d = self.tree.unionDeclData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("union ") catch {};
        self.write(d.name) catch {};
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const variant_extras = self.tree.extraSlice(d.variants);
        for (variant_extras, 0..) |vei, i| {
            const v = self.tree.extraData(@enumFromInt(vei), ast_mod.UnionVariant);
            self.writeIndent();
            self.write(self.tree.tokenSlice(v.name_token)) catch {};
            const type_node: Index = v.type_expr;
            if (@intFromEnum(type_node) != 0) {
                self.write(": ") catch {};
                self.printNode(type_node);
            }
            if (i + 1 < variant_extras.len) self.write(",") catch {};
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printTypeAlias(self: *Formatter, idx: Index) void {
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        const pair = data.token_and_node;
        self.writeIndent();
        self.write("type ") catch {};
        self.write(self.tree.tokenSlice(pair[0])) catch {};
        self.write(" = ") catch {};
        if (tag == .type_alias_distinct) self.write("distinct ") catch {};
        self.printNode(pair[1]);
    }

    fn printImport(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        self.writeIndent();
        self.write("import ") catch {};
        self.write(self.tree.tokenSlice(data.token)) catch {};
    }

    fn printImplBlock(self: *Formatter, idx: Index) void {
        const d = self.tree.implBlockData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("impl ") catch {};
        self.write(d.type_name) catch {};

        const tp_nodes = self.tree.extraSlice(d.type_params);
        if (tp_nodes.len > 0) {
            self.write("(") catch {};
            for (tp_nodes, 0..) |tp_tok, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(self.tree.tokenSlice(tp_tok)) catch {};
            }
            self.write(")") catch {};
        }

        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const method_nodes = self.tree.extraNodes(d.methods);
        for (method_nodes, 0..) |m, i| {
            if (i > 0) self.newline() catch {};
            self.printNode(m);
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printTraitDecl(self: *Formatter, idx: Index) void {
        const d = self.tree.traitDeclData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("trait ") catch {};
        self.write(d.name) catch {};
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const method_nodes = self.tree.extraNodes(d.methods);
        for (method_nodes, 0..) |m, i| {
            if (i > 0) self.newline() catch {};
            self.printNode(m);
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printImplTrait(self: *Formatter, idx: Index) void {
        const d = self.tree.implTraitData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("impl ") catch {};
        self.write(d.trait_name) catch {};
        self.write(" for ") catch {};
        self.write(d.target_type) catch {};

        const tp_nodes = self.tree.extraSlice(d.type_params);
        if (tp_nodes.len > 0) {
            self.write("(") catch {};
            for (tp_nodes, 0..) |tp_tok, i| {
                if (i > 0) self.write(", ") catch {};
                self.write(self.tree.tokenSlice(tp_tok)) catch {};
            }
            self.write(")") catch {};
        }

        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const method_nodes = self.tree.extraNodes(d.methods);
        for (method_nodes, 0..) |m, i| {
            if (i > 0) self.newline() catch {};
            self.printNode(m);
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printErrorSet(self: *Formatter, idx: Index) void {
        const d = self.tree.errorSetData(idx);
        if (d.doc_comment.unwrap()) |dc| self.printDocComment(self.tree.tokenSlice(dc));
        self.writeIndent();
        self.write("const ") catch {};
        self.write(d.name) catch {};
        self.write(" = error { ") catch {};

        const variant_toks = self.tree.extraSlice(d.variants);
        for (variant_toks, 0..) |vt, i| {
            if (i > 0) self.write(", ") catch {};
            self.write(self.tree.tokenSlice(vt)) catch {};
        }
        self.write(" }") catch {};
    }

    fn printTestDecl(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const pair = data.token_and_node;
        self.writeIndent();
        self.write("test ") catch {};
        self.write(self.tree.tokenSlice(pair[0])) catch {};
        self.write(" ") catch {};
        self.printBlock(pair[1]);
    }

    fn printBenchDecl(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const pair = data.token_and_node;
        self.writeIndent();
        self.write("bench ") catch {};
        self.write(self.tree.tokenSlice(pair[0])) catch {};
        self.write(" ") catch {};
        self.printBlock(pair[1]);
    }

    fn printCall(self: *Formatter, idx: Index) void {
        const c = self.tree.callData(idx);
        self.printNode(c.callee);
        self.write("(") catch {};

        if (c.single_arg.unwrap()) |arg| {
            self.printNode(arg);
        } else {
            const arg_nodes = self.tree.extraNodes(c.args);
            for (arg_nodes, 0..) |arg, i| {
                if (i > 0) self.write(", ") catch {};
                self.printNode(arg);
            }
        }

        self.write(")") catch {};
    }

    fn printIfExpr(self: *Formatter, idx: Index) void {
        const d = self.tree.ifData(idx);
        self.write("if (") catch {};
        self.printNode(d.condition);
        self.write(")") catch {};
        if (d.capture_token.unwrap()) |ct| {
            self.write(" |") catch {};
            self.write(self.tree.tokenSlice(ct)) catch {};
            self.write("|") catch {};
        }
        self.write(" ") catch {};
        self.printNode(d.then_branch);
        if (d.else_branch.unwrap()) |eb| {
            self.write(" else ") catch {};
            const else_tag = self.tree.nodeTag(eb);
            if (else_tag == .if_stmt or else_tag == .if_stmt_simple) {
                self.printNode(eb);
                return;
            }
            self.printNode(eb);
        }
    }

    fn printIfStmt(self: *Formatter, idx: Index) void {
        const d = self.tree.ifData(idx);
        self.writeIndent();
        self.write("if (") catch {};
        self.printNode(d.condition);
        self.write(")") catch {};
        if (d.capture_token.unwrap()) |ct| {
            self.write(" |") catch {};
            self.write(self.tree.tokenSlice(ct)) catch {};
            self.write("|") catch {};
        }
        self.write(" ") catch {};
        self.printBlock(d.then_branch);
        if (d.else_branch.unwrap()) |eb| {
            self.write(" else ") catch {};
            const else_tag = self.tree.nodeTag(eb);
            if (else_tag == .if_stmt or else_tag == .if_stmt_simple) {
                self.printNode(eb);
                return;
            }
            self.printBlock(eb);
        }
    }

    fn printSwitch(self: *Formatter, idx: Index) void {
        const d = self.tree.switchData(idx);
        self.write("switch ") catch {};
        self.printNode(d.subject);
        self.write(" {") catch {};
        self.newline() catch {};
        self.indent += 1;

        const case_extras = self.tree.extraSlice(d.cases);
        for (case_extras) |cei| {
            const c = self.tree.extraData(@enumFromInt(cei), ast_mod.SwitchCaseData);
            self.writeIndent();

            if (c.flags.is_range) {
                const pats = self.tree.extraNodes(c.patterns);
                if (pats.len >= 2) {
                    self.printNode(pats[0]);
                    self.write("..") catch {};
                    self.printNode(pats[1]);
                }
            } else {
                const pats = self.tree.extraNodes(c.patterns);
                for (pats, 0..) |pat, i| {
                    if (i > 0) self.write(", ") catch {};
                    self.printNode(pat);
                }
            }

            if (c.capture_token.unwrap()) |ct| {
                self.write(" |") catch {};
                self.write(self.tree.tokenSlice(ct)) catch {};
                self.write("|") catch {};
            }
            if (c.guard.unwrap()) |guard| {
                self.write(" if ") catch {};
                self.printNode(guard);
            }
            self.write(" => ") catch {};
            self.printNode(c.body);
            self.newline() catch {};
        }

        if (d.else_body.unwrap()) |eb| {
            self.writeIndent();
            self.write("else => ") catch {};
            self.printNode(eb);
            self.newline() catch {};
        }

        self.indent -= 1;
        self.writeIndent();
        self.write("}") catch {};
    }

    fn printWhile(self: *Formatter, idx: Index) void {
        const w = self.tree.whileData(idx);
        self.writeIndent();
        if (w.label_token.unwrap()) |lt| {
            self.write(self.tree.tokenSlice(lt)) catch {};
            self.write(": ") catch {};
        }
        self.write("while (") catch {};
        self.printNode(w.condition);
        self.write(")") catch {};
        if (w.capture_token.unwrap()) |ct| {
            self.write(" |") catch {};
            self.write(self.tree.tokenSlice(ct)) catch {};
            self.write("|") catch {};
        }
        if (w.continue_expr.unwrap()) |ce| {
            self.write(" : (") catch {};
            self.printNode(ce);
            self.write(")") catch {};
        }
        self.write(" ") catch {};
        self.printBlock(w.body);
    }

    fn printFor(self: *Formatter, idx: Index) void {
        const f = self.tree.forData(idx);
        self.writeIndent();
        if (f.label_token.unwrap()) |lt| {
            self.write(self.tree.tokenSlice(lt)) catch {};
            self.write(": ") catch {};
        }
        if (f.is_inline) self.write("inline ") catch {};
        self.write("for ") catch {};
        if (f.index_binding_token.unwrap()) |ibt| {
            self.write(self.tree.tokenSlice(ibt)) catch {};
            self.write(", ") catch {};
        }
        self.write(f.binding) catch {};
        self.write(" in ") catch {};
        if (f.range_start.unwrap()) |rs| {
            self.printNode(rs);
            self.write("..") catch {};
            if (f.range_end.unwrap()) |re| self.printNode(re);
        } else if (f.iterable.unwrap()) |it| {
            self.printNode(it);
        }
        self.write(" ") catch {};
        self.printBlock(f.body);
    }

    fn printBreak(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const b = self.tree.extraData(data.node_and_extra[1], ast_mod.BreakData);
        self.writeIndent();
        self.write("break") catch {};
        if (b.label_token.unwrap()) |lt| {
            self.write(" :") catch {};
            self.write(self.tree.tokenSlice(lt)) catch {};
        }
        if (b.value.unwrap()) |val| {
            self.write(" ") catch {};
            self.printNode(val);
        }
    }

    fn printLocalVar(self: *Formatter, idx: Index) void {
        const v = self.tree.localVarData(idx);
        self.writeIndent();
        self.write(if (v.is_const) "const " else "var ") catch {};
        self.write(v.name) catch {};
        if (v.type_expr.unwrap()) |te| {
            self.write(": ") catch {};
            self.printNode(te);
        }
        if (v.value.unwrap()) |val| {
            self.write(" = ") catch {};
            self.printNode(val);
        }
    }

    fn printDestructure(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const d = self.tree.extraData(data.node_and_extra[1], ast_mod.DestructureData);
        self.writeIndent();
        self.write(if (d.flags.is_const) "const " else "var ") catch {};

        const binding_raw = self.tree.extraSlice(d.bindings);
        var bi: usize = 0;
        var first = true;
        while (bi + 1 < binding_raw.len) {
            if (!first) self.write(", ") catch {};
            first = false;
            const name_token = binding_raw[bi];
            const type_expr_opt: OptionalIndex = @enumFromInt(binding_raw[bi + 1]);
            self.write(self.tree.tokenSlice(name_token)) catch {};
            if (type_expr_opt.unwrap()) |te| {
                self.write(": ") catch {};
                self.printNode(te);
            }
            bi += 2;
        }

        self.write(" = ") catch {};
        self.printNode(d.value);
    }

    fn printStructInitOne(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const s = self.tree.extraData(data.node_and_extra[1], ast_mod.StructInitOne);
        if (s.type_name_token.unwrap()) |tn| {
            self.write(self.tree.tokenSlice(tn)) catch {};
        }
        self.write(" { .") catch {};
        self.write(self.tree.tokenSlice(s.field_name_token)) catch {};
        self.write(" = ") catch {};
        self.printNode(s.field_value);
        self.write(" }") catch {};
    }

    fn printStructInit(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const s = self.tree.extraData(data.node_and_extra[1], ast_mod.StructInit);
        if (s.type_name_token.unwrap()) |tn| {
            self.write(self.tree.tokenSlice(tn)) catch {};
        }
        const type_arg_nodes = self.tree.extraNodes(s.type_args);
        if (type_arg_nodes.len > 0) {
            self.write("(") catch {};
            for (type_arg_nodes, 0..) |ta, i| {
                if (i > 0) self.write(", ") catch {};
                self.printNode(ta);
            }
            self.write(")") catch {};
        }
        self.write(" { ") catch {};
        const field_raw = self.tree.extraSlice(s.fields);
        var fi: usize = 0;
        var count: usize = 0;
        while (fi + 1 < field_raw.len) {
            if (count > 0) self.write(", ") catch {};
            self.write(".") catch {};
            self.write(self.tree.tokenSlice(field_raw[fi])) catch {};
            self.write(" = ") catch {};
            self.printNode(@enumFromInt(field_raw[fi + 1]));
            fi += 2;
            count += 1;
        }
        self.write(" }") catch {};
    }

    fn printNewExpr(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const n = self.tree.extraData(data.node_and_extra[1], ast_mod.NewExprData);
        self.write("new ") catch {};
        self.write(self.tree.tokenSlice(n.type_name_token)) catch {};

        const type_arg_nodes = self.tree.extraNodes(n.type_args);
        if (type_arg_nodes.len > 0) {
            self.write("(") catch {};
            for (type_arg_nodes, 0..) |ta, i| {
                if (i > 0) self.write(", ") catch {};
                self.printNode(ta);
            }
            self.write(")") catch {};
        }

        self.write(" { ") catch {};
        const field_extras = self.tree.extraSlice(n.fields);
        for (field_extras, 0..) |fei, i| {
            if (i > 0) self.write(", ") catch {};
            const field = self.tree.extraData(@enumFromInt(fei), ast_mod.Field);
            self.write(self.tree.tokenSlice(field.name_token)) catch {};
            self.write(": ") catch {};
            self.printNode(field.type_expr);
        }
        self.write(" }") catch {};
    }

    fn printBuiltinCall(self: *Formatter, idx: Index) void {
        const b = self.tree.builtinCallData(idx);
        self.write("@") catch {};
        self.write(b.kind.sourceName()) catch {};
        self.write("(") catch {};
        var first = true;
        if (b.type_arg.unwrap()) |ta| {
            self.printNode(ta);
            first = false;
        }
        const opt_args = [_]OptionalIndex{ b.arg0, b.arg1, b.arg2 };
        for (opt_args) |oa| {
            if (oa.unwrap()) |arg| {
                if (!first) self.write(", ") catch {};
                self.printNode(arg);
                first = false;
            }
        }
        self.write(")") catch {};
    }

    fn printCatch(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const c = self.tree.extraData(data.node_and_extra[1], ast_mod.CatchData);
        self.printNode(data.node_and_extra[0]);
        self.write(" catch ") catch {};
        if (c.capture_token.unwrap()) |ct| {
            self.write("|") catch {};
            self.write(self.tree.tokenSlice(ct)) catch {};
            self.write("| ") catch {};
        }
        self.printNode(c.fallback);
    }

    fn printOrElse(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const o = self.tree.extraData(data.node_and_extra[1], ast_mod.OrElseData);
        self.printNode(data.node_and_extra[0]);
        self.write(" orelse ") catch {};
        const kind: ast_mod.OrElseFallback = @enumFromInt(o.fallback_kind);
        switch (kind) {
            .expr => self.printNode(o.fallback),
            .return_void => self.write("return") catch {},
            .return_val => {
                self.write("return ") catch {};
                self.printNode(o.fallback);
            },
            .break_val => self.write("break") catch {},
            .continue_val => self.write("continue") catch {},
        }
    }

    fn printClosure(self: *Formatter, idx: Index) void {
        const c = self.tree.closureData(idx);
        self.write("fn(") catch {};
        const param_extras = self.tree.extraSlice(c.params);
        for (param_extras, 0..) |pei, i| {
            if (i > 0) self.write(", ") catch {};
            const field = self.tree.extraData(@enumFromInt(pei), ast_mod.Field);
            self.write(self.tree.tokenSlice(field.name_token)) catch {};
            if (self.tree.nodeTag(field.type_expr) != .bad_node and @intFromEnum(field.type_expr) != 0) {
                self.write(": ") catch {};
                self.printNode(field.type_expr);
            }
        }
        self.write(")") catch {};
        if (c.return_type.unwrap()) |rt| {
            self.write(" ") catch {};
            self.printNode(rt);
        }
        self.write(" ") catch {};
        self.printNode(c.body);
    }

    fn printTask(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const t = self.tree.extraData(data.node_and_extra[1], ast_mod.TaskData);
        if (t.flags.is_detached) {
            self.write("Task.detached ") catch {};
        } else {
            self.write("Task ") catch {};
        }
        self.printNode(t.body);
    }

    fn printFnType(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const ft = self.tree.extraData(data.node_and_extra[1], ast_mod.FnType);
        self.write("fn(") catch {};
        const param_nodes = self.tree.extraNodes(ft.params);
        for (param_nodes, 0..) |p, i| {
            if (i > 0) self.write(", ") catch {};
            self.printNode(p);
        }
        self.write(")") catch {};
        if (ft.ret.unwrap()) |ret| {
            self.write(" -> ") catch {};
            self.printNode(ret);
        }
    }

    fn printGenericType(self: *Formatter, idx: Index) void {
        const data = self.tree.nodeData(idx);
        const gi = self.tree.extraData(data.node_and_extra[1], ast_mod.GenericInstance);
        self.write(self.tree.tokenSlice(gi.name_token)) catch {};
        self.write("(") catch {};
        const type_arg_nodes = self.tree.extraNodes(gi.type_args);
        for (type_arg_nodes, 0..) |ta, i| {
            if (i > 0) self.write(", ") catch {};
            self.printNode(ta);
        }
        self.write(")") catch {};
    }

    fn printBlockInline(self: *Formatter, idx: Index) void {
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        switch (tag) {
            .block_one => {
                const pair = data.node_and_node;
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;
                if (@intFromEnum(pair[0]) != 0) {
                    self.printNode(pair[0]);
                    self.newline() catch {};
                }
                if (@intFromEnum(pair[1]) != 0) {
                    self.writeIndent();
                    self.printNode(pair[1]);
                    self.newline() catch {};
                }
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            .block_two => {
                const pair = data.node_and_node;
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;
                self.printNode(pair[0]);
                self.newline() catch {};
                self.printNode(pair[1]);
                self.newline() catch {};
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            .block, .block_stmt => {
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;
                const stmts = self.tree.extraNodes(data.extra_range);
                for (stmts) |stmt_idx| {
                    const offset = self.tree.tokenStart(self.tree.nodeMainToken(stmt_idx));
                    self.flushCommentsBefore(offset);
                    self.printNode(stmt_idx);
                    self.newline() catch {};
                }
                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            else => self.printNode(idx),
        }
    }

    fn printBlock(self: *Formatter, idx: Index) void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .block_one, .block_two, .block, .block_stmt => {
                const data = self.tree.nodeData(idx);
                self.write("{") catch {};
                self.newline() catch {};
                self.indent += 1;

                switch (tag) {
                    .block_one => {
                        const pair = data.node_and_node;
                        if (@intFromEnum(pair[0]) != 0) {
                            const offset = self.tree.tokenStart(self.tree.nodeMainToken(pair[0]));
                            self.flushCommentsBefore(offset);
                            self.printNode(pair[0]);
                            self.newline() catch {};
                        }
                        if (@intFromEnum(pair[1]) != 0) {
                            self.writeIndent();
                            self.printNode(pair[1]);
                            self.newline() catch {};
                        }
                    },
                    .block_two => {
                        const pair = data.node_and_node;
                        const o0 = self.tree.tokenStart(self.tree.nodeMainToken(pair[0]));
                        self.flushCommentsBefore(o0);
                        self.printNode(pair[0]);
                        self.newline() catch {};
                        const o1 = self.tree.tokenStart(self.tree.nodeMainToken(pair[1]));
                        self.flushCommentsBefore(o1);
                        self.printNode(pair[1]);
                        self.newline() catch {};
                    },
                    .block, .block_stmt => {
                        const stmts = self.tree.extraNodes(data.extra_range);
                        for (stmts) |stmt_idx| {
                            const offset = self.tree.tokenStart(self.tree.nodeMainToken(stmt_idx));
                            self.flushCommentsBefore(offset);
                            self.printNode(stmt_idx);
                            self.newline() catch {};
                        }
                    },
                    else => {},
                }

                self.indent -= 1;
                self.writeIndent();
                self.write("}") catch {};
            },
            else => self.printNode(idx),
        }
    }

    fn printDocComment(self: *Formatter, doc: []const u8) void {
        if (doc.len == 0) return;
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

fn binaryOpString(tag: Tag) []const u8 {
    return switch (tag) {
        .binary_add => "+",
        .binary_sub => "-",
        .binary_mul => "*",
        .binary_div => "/",
        .binary_mod => "%",
        .binary_eq => "==",
        .binary_neq => "!=",
        .binary_lt => "<",
        .binary_gt => ">",
        .binary_lte => "<=",
        .binary_gte => ">=",
        .binary_and => "and",
        .binary_or => "or",
        .binary_bit_and => "&",
        .binary_bit_or => "|",
        .binary_bit_xor => "^",
        .binary_shl => "<<",
        .binary_shr => ">>",
        .binary_concat => "++",
        .binary_pipe => "|>",
        else => "?",
    };
}

fn assignOpString(tag: Tag) []const u8 {
    return switch (tag) {
        .assign_add => "+=",
        .assign_sub => "-=",
        .assign_mul => "*=",
        .assign_div => "/=",
        .assign_mod => "%=",
        .assign_bit_and => "&=",
        .assign_bit_or => "|=",
        .assign_bit_xor => "^=",
        else => "?=",
    };
}
