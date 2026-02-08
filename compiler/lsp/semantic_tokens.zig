//! LSP Semantic Tokens — walks the AST and classifies identifiers/names
//! using the checker's type information for richer coloring than TextMate.
//!
//! TextMate handles keywords, strings, numbers, comments, and operators.
//! Semantic tokens handle everything TextMate can't: distinguishing functions
//! from variables, types from values, parameters from locals, methods from
//! properties, builtin calls, enum members, type parameters, etc.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ast_mod = @import("../frontend/ast.zig");
const source_mod = @import("../frontend/source.zig");
const types_mod = @import("../frontend/types.zig");
const checker_mod = @import("../frontend/checker.zig");
const analysis_mod = @import("analysis.zig");

const Ast = ast_mod.Ast;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Span = source_mod.Span;
const Pos = source_mod.Pos;
const AnalysisResult = analysis_mod.AnalysisResult;
const TypeIndex = types_mod.TypeIndex;

// ============================================================================
// LSP Semantic Token Legend
// ============================================================================

/// Token type indices — must match the legend array order.
pub const TokenType = enum(u32) {
    namespace = 0,
    type = 1,
    @"struct" = 2,
    @"enum" = 3,
    interface = 4,
    type_parameter = 5,
    parameter = 6,
    variable = 7,
    property = 8,
    enum_member = 9,
    function = 10,
    method = 11,
    macro = 12,
    keyword = 13,
    comment = 14,
    string = 15,
    number = 16,
    operator = 17,
};

pub const token_type_legend = [_][]const u8{
    "namespace",     "type",      "struct",    "enum",
    "interface",     "typeParameter", "parameter", "variable",
    "property",      "enumMember", "function",  "method",
    "macro",         "keyword",    "comment",   "string",
    "number",        "operator",
};

pub const token_modifier_legend = [_][]const u8{
    "declaration",
    "definition",
    "readonly",
    "static",
    "defaultLibrary",
};

// Modifier bit flags
const MOD_DECLARATION: u32 = 1 << 0;
const MOD_DEFINITION: u32 = 1 << 1;
const MOD_READONLY: u32 = 1 << 2;
const MOD_DEFAULT_LIBRARY: u32 = 1 << 4;

// ============================================================================
// Token Collection
// ============================================================================

const RawToken = struct {
    offset: u32,
    length: u32,
    token_type: u32,
    modifiers: u32,
};

const TokenCollector = struct {
    tokens: std.ArrayListUnmanaged(RawToken),
    tree: *const Ast,
    content: []const u8,
    expr_types: ?*const std.AutoHashMap(NodeIndex, TypeIndex),
    type_reg: *types_mod.TypeRegistry,
    global_scope: *const checker_mod.Scope,
    allocator: Allocator,
    /// Stack of parameter name sets for classifying params in function bodies.
    param_names: std.ArrayListUnmanaged([]const []const u8),

    fn emit(self: *TokenCollector, offset: u32, length: u32, token_type: TokenType, modifiers: u32) void {
        if (length == 0) return;
        if (offset + length > self.content.len) return;
        self.tokens.append(self.allocator, .{
            .offset = offset,
            .length = length,
            .token_type = @intFromEnum(token_type),
            .modifiers = modifiers,
        }) catch {};
    }

    /// Find `name` as a whole-word match within [span.start, span.end) and emit it.
    fn emitName(self: *TokenCollector, span: Span, name: []const u8, token_type: TokenType, modifiers: u32) void {
        if (name.len == 0) return;
        if (findNameOffset(self.content, span.start.offset, span.end.offset, name)) |offset| {
            self.emit(offset, @intCast(name.len), token_type, modifiers);
        }
    }

    /// Emit field init name (e.g., `x` in `x: value`). Search only the first few bytes
    /// of the span to avoid matching inside the value expression.
    fn emitFieldInitName(self: *TokenCollector, span: Span, name: []const u8, token_type: TokenType, modifiers: u32) void {
        if (name.len == 0) return;
        // Field init name is at the very start of the span, search a small window
        const search_end = @min(span.start.offset + @as(u32, @intCast(name.len)) + 4, span.end.offset);
        if (findNameOffset(self.content, span.start.offset, search_end, name)) |offset| {
            self.emit(offset, @intCast(name.len), token_type, modifiers);
        }
    }

    /// Find `name` searching backwards from span end (for field access where name is at the end).
    fn emitNameReverse(self: *TokenCollector, span: Span, name: []const u8, token_type: TokenType, modifiers: u32) void {
        if (name.len == 0) return;
        if (findLastNameOffset(self.content, span.start.offset, span.end.offset, name)) |offset| {
            self.emit(offset, @intCast(name.len), token_type, modifiers);
        }
    }

    // ========================================================================
    // AST Walking
    // ========================================================================

    fn walkNode(self: *TokenCollector, idx: NodeIndex) void {
        if (idx == null_node) return;
        const node = self.tree.getNode(idx) orelse return;
        switch (node) {
            .decl => |d| self.walkDecl(d),
            .expr => |e| self.walkExpr(e, idx),
            .stmt => |s| self.walkStmt(s),
        }
    }

    fn walkDecl(self: *TokenCollector, decl: ast_mod.Decl) void {
        switch (decl) {
            .fn_decl => |f| {
                self.emitName(f.span, f.name, .function, MOD_DECLARATION | MOD_DEFINITION);
                for (f.type_params) |tp| self.emitName(f.span, tp, .type_parameter, MOD_DECLARATION);
                // Collect param names for classifying references in the body
                var pnames = std.ArrayListUnmanaged([]const u8){};
                for (f.params) |p| {
                    self.emitName(p.span, p.name, .parameter, MOD_DECLARATION);
                    self.walkNode(p.type_expr);
                    pnames.append(self.allocator, p.name) catch {};
                }
                const param_names = pnames.items;
                self.walkNode(f.return_type);
                self.param_names.append(self.allocator, param_names) catch {};
                self.walkNode(f.body);
                if (self.param_names.items.len > 0) self.param_names.items.len -= 1;
            },
            .struct_decl => |s| {
                self.emitName(s.span, s.name, .@"struct", MOD_DECLARATION | MOD_DEFINITION);
                for (s.type_params) |tp| self.emitName(s.span, tp, .type_parameter, MOD_DECLARATION);
                for (s.fields) |fld| {
                    self.emitName(fld.span, fld.name, .property, MOD_DECLARATION);
                    self.walkNode(fld.type_expr);
                    self.walkNode(fld.default_value);
                }
            },
            .enum_decl => |e| {
                self.emitName(e.span, e.name, .@"enum", MOD_DECLARATION | MOD_DEFINITION);
                self.walkNode(e.backing_type);
                for (e.variants) |v| {
                    self.emitName(v.span, v.name, .enum_member, MOD_DECLARATION);
                    self.walkNode(v.value);
                }
            },
            .union_decl => |u| {
                self.emitName(u.span, u.name, .@"struct", MOD_DECLARATION | MOD_DEFINITION);
                for (u.variants) |v| {
                    self.emitName(v.span, v.name, .property, MOD_DECLARATION);
                    self.walkNode(v.type_expr);
                }
            },
            .trait_decl => |t| {
                self.emitName(t.span, t.name, .interface, MOD_DECLARATION | MOD_DEFINITION);
                for (t.methods) |m| self.walkNode(m);
            },
            .impl_block => |i| {
                self.emitName(i.span, i.type_name, .type, 0);
                for (i.type_params) |tp| self.emitName(i.span, tp, .type_parameter, MOD_DECLARATION);
                for (i.methods) |m| self.walkNode(m);
            },
            .impl_trait => |i| {
                self.emitName(i.span, i.trait_name, .interface, 0);
                self.emitName(i.span, i.target_type, .type, 0);
                for (i.type_params) |tp| self.emitName(i.span, tp, .type_parameter, MOD_DECLARATION);
                for (i.methods) |m| self.walkNode(m);
            },
            .var_decl => |v| {
                const mods: u32 = MOD_DECLARATION | if (v.is_const) MOD_READONLY else @as(u32, 0);
                self.emitName(v.span, v.name, .variable, mods);
                self.walkNode(v.type_expr);
                self.walkNode(v.value);
            },
            .type_alias => |t| {
                self.emitName(t.span, t.name, .type, MOD_DECLARATION | MOD_DEFINITION);
                self.walkNode(t.target);
            },
            .import_decl => |i| {
                self.emitName(i.span, i.path, .namespace, 0);
            },
            .error_set_decl => |e| {
                self.emitName(e.span, e.name, .@"enum", MOD_DECLARATION | MOD_DEFINITION);
                for (e.variants) |v| self.emitName(e.span, v, .enum_member, MOD_DECLARATION);
            },
            .test_decl => |t| {
                self.walkNode(t.body);
            },
            .bad_decl => {},
        }
    }

    fn walkExpr(self: *TokenCollector, expr: ast_mod.Expr, node_idx: NodeIndex) void {
        switch (expr) {
            .ident => |i| {
                const classified = self.classifyIdent(i.name, node_idx);
                self.emit(i.span.start.offset, @intCast(i.name.len), classified.token_type, classified.modifiers);
            },
            .field_access => |f| {
                self.walkNode(f.base);
                // Use findNameOffset for robust field position (span.end may not align exactly)
                self.emitNameReverse(f.span, f.field, .property, 0);
            },
            .call => |c| {
                // Handle callee specially to distinguish function/method calls
                self.walkCallCallee(c.callee);
                for (c.args) |arg| self.walkNode(arg);
            },
            .builtin_call => |b| {
                // @name — emit the whole @name as macro
                const at_prefix: u32 = if (b.name.len > 0 and b.name[0] == '@') 0 else 1;
                self.emit(b.span.start.offset, @as(u32, @intCast(b.name.len)) + at_prefix, .macro, MOD_DEFAULT_LIBRARY);
                self.walkNode(b.type_arg);
                for (b.args) |arg| self.walkNode(arg);
            },
            .type_expr => |t| self.walkTypeExpr(t),
            .struct_init => |s| {
                if (s.type_name.len > 0)
                    self.emitName(s.span, s.type_name, .@"struct", 0);
                for (s.type_args) |ta| self.walkNode(ta);
                for (s.fields) |f| {
                    // Narrow search to just the beginning of the field init span
                    // to avoid matching the field name inside the value expression
                    self.emitFieldInitName(f.span, f.name, .property, 0);
                    self.walkNode(f.value);
                }
            },
            .new_expr => |n| {
                if (n.type_name.len > 0)
                    self.emitName(n.span, n.type_name, .@"struct", 0);
                for (n.type_args) |ta| self.walkNode(ta);
                for (n.fields) |f| {
                    self.emitFieldInitName(f.span, f.name, .property, 0);
                    self.walkNode(f.value);
                }
            },
            .error_literal => |e| self.emitName(e.span, e.error_name, .enum_member, 0),
            .closure_expr => |c| {
                var cpnames = std.ArrayListUnmanaged([]const u8){};
                for (c.params) |p| {
                    self.emitName(p.span, p.name, .parameter, MOD_DECLARATION);
                    self.walkNode(p.type_expr);
                    cpnames.append(self.allocator, p.name) catch {};
                }
                const cparam_names = cpnames.items;
                self.walkNode(c.return_type);
                self.param_names.append(self.allocator, cparam_names) catch {};
                self.walkNode(c.body);
                if (self.param_names.items.len > 0) self.param_names.items.len -= 1;
            },
            .catch_expr => |c| {
                self.walkNode(c.operand);
                if (c.capture.len > 0)
                    self.emitName(c.span, c.capture, .variable, MOD_DECLARATION);
                self.walkNode(c.fallback);
            },
            .binary => |b| {
                self.walkNode(b.left);
                self.walkNode(b.right);
            },
            .unary => |u| self.walkNode(u.operand),
            .index => |i| {
                self.walkNode(i.base);
                self.walkNode(i.idx);
            },
            .slice_expr => |s| {
                self.walkNode(s.base);
                self.walkNode(s.start);
                self.walkNode(s.end);
            },
            .paren => |p| self.walkNode(p.inner),
            .if_expr => |i| {
                self.walkNode(i.condition);
                self.walkNode(i.then_branch);
                self.walkNode(i.else_branch);
            },
            .switch_expr => |s| {
                self.walkNode(s.subject);
                for (s.cases) |c| {
                    for (c.patterns) |p| self.walkNode(p);
                    self.walkNode(c.guard);
                    self.walkNode(c.body);
                }
                self.walkNode(s.else_body);
            },
            .block_expr => |b| {
                for (b.stmts) |stmt| self.walkNode(stmt);
                self.walkNode(b.expr);
            },
            .array_literal => |a| {
                for (a.elements) |e| self.walkNode(e);
            },
            .try_expr => |t| self.walkNode(t.operand),
            .string_interp => |s| {
                for (s.segments) |seg| switch (seg) {
                    .expr => |e| self.walkNode(e),
                    .text => {},
                };
            },
            .tuple_literal => |t| {
                for (t.elements) |e| self.walkNode(e);
            },
            .addr_of => |a| self.walkNode(a.operand),
            .deref => |d| self.walkNode(d.operand),
            .literal, .zero_init, .bad_expr => {},
        }
    }

    fn walkStmt(self: *TokenCollector, stmt: ast_mod.Stmt) void {
        switch (stmt) {
            .var_stmt => |v| {
                const mods: u32 = MOD_DECLARATION | if (v.is_const) MOD_READONLY else @as(u32, 0);
                self.emitName(v.span, v.name, .variable, mods);
                self.walkNode(v.type_expr);
                self.walkNode(v.value);
            },
            .assign_stmt => |a| {
                self.walkNode(a.target);
                self.walkNode(a.value);
            },
            .expr_stmt => |e| self.walkNode(e.expr),
            .return_stmt => |r| self.walkNode(r.value),
            .if_stmt => |i| {
                self.walkNode(i.condition);
                self.walkNode(i.then_branch);
                self.walkNode(i.else_branch);
            },
            .while_stmt => |w| {
                self.walkNode(w.condition);
                self.walkNode(w.body);
            },
            .for_stmt => |f| {
                self.emitName(f.span, f.binding, .variable, MOD_DECLARATION);
                if (f.index_binding) |ib| self.emitName(f.span, ib, .variable, MOD_DECLARATION);
                self.walkNode(f.iterable);
                self.walkNode(f.range_start);
                self.walkNode(f.range_end);
                self.walkNode(f.body);
            },
            .block_stmt => |b| {
                for (b.stmts) |s| self.walkNode(s);
            },
            .defer_stmt => |d| self.walkNode(d.expr),
            .break_stmt, .continue_stmt, .bad_stmt => {},
        }
    }

    /// Handle a call's callee: ident → function, field_access → method.
    fn walkCallCallee(self: *TokenCollector, callee_idx: NodeIndex) void {
        if (callee_idx == null_node) return;
        const node = self.tree.getNode(callee_idx) orelse return;
        switch (node) {
            .expr => |e| switch (e) {
                .ident => |i| {
                    self.emit(i.span.start.offset, @intCast(i.name.len), .function, 0);
                },
                .field_access => |f| {
                    self.walkNode(f.base);
                    self.emitNameReverse(f.span, f.field, .method, 0);
                },
                .type_expr => |t| self.walkTypeExpr(t),
                else => self.walkExpr(e, callee_idx),
            },
            else => self.walkNode(callee_idx),
        }
    }

    fn walkTypeExpr(self: *TokenCollector, t: ast_mod.TypeExpr) void {
        switch (t.kind) {
            .named => |name| {
                self.emit(t.span.start.offset, @intCast(name.len), .type, 0);
            },
            .generic_instance => |gi| {
                self.emitName(t.span, gi.name, .type, 0);
                for (gi.type_args) |ta| self.walkNode(ta);
            },
            .pointer => |inner| self.walkNode(inner),
            .optional => |inner| self.walkNode(inner),
            .error_union => |eu| {
                self.walkNode(eu.error_set);
                self.walkNode(eu.elem);
            },
            .slice => |inner| self.walkNode(inner),
            .array => |a| {
                self.walkNode(a.size);
                self.walkNode(a.elem);
            },
            .map => |m| {
                self.walkNode(m.key);
                self.walkNode(m.value);
            },
            .list => |inner| self.walkNode(inner),
            .function => |f| {
                for (f.params) |p| self.walkNode(p);
                self.walkNode(f.ret);
            },
            .tuple => |elems| {
                for (elems) |e| self.walkNode(e);
            },
        }
    }

    // ========================================================================
    // Identifier Classification
    // ========================================================================

    const Classification = struct { token_type: TokenType, modifiers: u32 };

    fn classifyIdent(self: *TokenCollector, name: []const u8, node_idx: NodeIndex) Classification {
        // Check checker's expr_types for this node
        if (self.expr_types) |et| {
            if (et.get(node_idx)) |type_idx| {
                const typ = self.type_reg.get(type_idx);
                if (typ == .func) return .{ .token_type = .function, .modifiers = 0 };
            }
        }

        // Check global scope for symbol info
        if (self.global_scope.lookup(name)) |sym| {
            return switch (sym.kind) {
                .function => .{ .token_type = .function, .modifiers = 0 },
                .type_name => .{ .token_type = .type, .modifiers = 0 },
                .parameter => .{ .token_type = .parameter, .modifiers = 0 },
                .constant => .{ .token_type = .variable, .modifiers = MOD_READONLY },
                .variable => .{ .token_type = .variable, .modifiers = 0 },
            };
        }

        // Check parameter name stack (locals not in global scope)
        if (self.isParam(name))
            return .{ .token_type = .parameter, .modifiers = 0 };

        // Heuristic: uppercase start → likely a type
        if (name.len > 0 and name[0] >= 'A' and name[0] <= 'Z')
            return .{ .token_type = .type, .modifiers = 0 };

        // Default: variable
        return .{ .token_type = .variable, .modifiers = 0 };
    }

    fn isParam(self: *const TokenCollector, name: []const u8) bool {
        // Check from innermost scope outward
        var i = self.param_names.items.len;
        while (i > 0) {
            i -= 1;
            for (self.param_names.items[i]) |pn| {
                if (std.mem.eql(u8, pn, name)) return true;
            }
        }
        return false;
    }
};

// ============================================================================
// Helpers
// ============================================================================

/// Search for `name` as a whole-word match in content[search_start..search_end].
fn findNameOffset(content: []const u8, search_start: u32, search_end: u32, name: []const u8) ?u32 {
    if (name.len == 0) return null;
    const end = @min(search_end, @as(u32, @intCast(content.len)));
    if (search_start + name.len > end) return null;
    var i = search_start;
    while (i + name.len <= end) {
        if (std.mem.eql(u8, content[i .. i + name.len], name)) {
            const before_ok = (i == 0) or !isIdentChar(content[i - 1]);
            const after_idx = i + @as(u32, @intCast(name.len));
            const after_ok = (after_idx >= content.len) or !isIdentChar(content[after_idx]);
            if (before_ok and after_ok) return i;
        }
        i += 1;
    }
    return null;
}

/// Search for `name` as a whole-word match, scanning backwards from search_end.
fn findLastNameOffset(content: []const u8, search_start: u32, search_end: u32, name: []const u8) ?u32 {
    if (name.len == 0) return null;
    const end = @min(search_end, @as(u32, @intCast(content.len)));
    if (search_start + name.len > end) return null;
    var i: u32 = end - @as(u32, @intCast(name.len));
    while (i >= search_start) {
        if (std.mem.eql(u8, content[i .. i + name.len], name)) {
            const before_ok = (i == 0) or !isIdentChar(content[i - 1]);
            const after_idx = i + @as(u32, @intCast(name.len));
            const after_ok = (after_idx >= content.len) or !isIdentChar(content[after_idx]);
            if (before_ok and after_ok) return i;
        }
        if (i == search_start) break;
        i -= 1;
    }
    return null;
}

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_';
}

/// Convert byte offset to (line, character) using precomputed line offsets.
fn offsetToLineChar(offset: u32, line_offsets: []const u32) struct { line: u32, char: u32 } {
    var line: u32 = 0;
    var lo: usize = 0;
    var hi: usize = line_offsets.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (line_offsets[mid] <= offset) {
            line = @intCast(mid);
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return .{ .line = line, .char = offset - line_offsets[line] };
}

/// Delta-encode raw tokens into the LSP u32 array format.
/// Each token: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
fn deltaEncode(allocator: Allocator, tokens: []const RawToken, content: []const u8) ![]const u32 {
    if (tokens.len == 0) return &.{};

    // Precompute line offsets
    var line_count: usize = 1;
    for (content) |c| {
        if (c == '\n') line_count += 1;
    }
    const line_offsets = try allocator.alloc(u32, line_count);
    defer allocator.free(line_offsets);
    line_offsets[0] = 0;
    var idx: usize = 1;
    for (content, 0..) |c, i| {
        if (c == '\n') {
            line_offsets[idx] = @intCast(i + 1);
            idx += 1;
        }
    }

    const data = try allocator.alloc(u32, tokens.len * 5);

    var prev_line: u32 = 0;
    var prev_char: u32 = 0;

    for (tokens, 0..) |tok, i| {
        const pos = offsetToLineChar(tok.offset, line_offsets);
        const delta_line = pos.line - prev_line;
        const delta_char = if (delta_line == 0) pos.char - prev_char else pos.char;

        data[i * 5 + 0] = delta_line;
        data[i * 5 + 1] = delta_char;
        data[i * 5 + 2] = tok.length;
        data[i * 5 + 3] = tok.token_type;
        data[i * 5 + 4] = tok.modifiers;

        prev_line = pos.line;
        prev_char = pos.char;
    }

    return data;
}

// ============================================================================
// Public API
// ============================================================================

/// Walk the AST and return LSP-encoded semantic token data.
pub fn getSemanticTokens(allocator: Allocator, result: *AnalysisResult) ![]const u32 {
    var collector = TokenCollector{
        .tokens = .{},
        .tree = &result.tree,
        .content = result.src.content,
        .expr_types = if (result.checker) |*c| &c.expr_types else null,
        .type_reg = &result.type_reg,
        .global_scope = &result.global_scope,
        .allocator = allocator,
        .param_names = .{},
    };

    // Walk all root declarations
    for (result.tree.getRootDecls()) |decl_idx| {
        collector.walkNode(decl_idx);
    }

    // Sort by offset (AST walk may not produce tokens in source order)
    std.sort.block(RawToken, collector.tokens.items, {}, struct {
        fn f(_: void, a: RawToken, b: RawToken) bool {
            return a.offset < b.offset;
        }
    }.f);

    // Delta-encode for LSP
    return deltaEncode(allocator, collector.tokens.items, result.src.content);
}

// ============================================================================
// Tests
// ============================================================================

test "semantic tokens: struct init with field access" {
    // Use page_allocator since analysis.zig's deinit order doesn't work with GPA's strict checking
    const allocator = std.heap.page_allocator;

    const src =
        \\struct P { x: i64, y: i64 }
        \\impl P {
        \\    fn add(self: *P, other: *P) P {
        \\        return P { .x = self.x + other.x, .y = self.y + other.y }
        \\    }
        \\}
    ;

    var result = analysis_mod.analyze(allocator, src, "test.cot") orelse return error.AnalysisFailed;
    // Note: don't deinit since analysis arena deinit has issues in test context

    // Collect raw tokens (not delta-encoded) so we can inspect them
    var collector = TokenCollector{
        .tokens = .{},
        .tree = &result.tree,
        .content = result.src.content,
        .expr_types = if (result.checker) |*c| &c.expr_types else null,
        .type_reg = &result.type_reg,
        .global_scope = &result.global_scope,
        .allocator = allocator,
        .param_names = .{},
    };
    defer collector.tokens.deinit(allocator);

    for (result.tree.getRootDecls()) |decl_idx| {
        collector.walkNode(decl_idx);
    }

    // Sort by offset
    std.sort.block(RawToken, collector.tokens.items, {}, struct {
        fn f(_: void, a: RawToken, b: RawToken) bool {
            return a.offset < b.offset;
        }
    }.f);

    // Print all tokens unconditionally for debugging
    const tokens = collector.tokens.items;
    std.debug.print("\n=== Semantic Tokens ({d} total) ===\n", .{tokens.len});
    for (tokens) |tok| {
        const text = src[tok.offset .. tok.offset + tok.length];
        const type_name = if (tok.token_type < token_type_legend.len) token_type_legend[tok.token_type] else "?";
        std.debug.print("  [{d}..{d}] \"{s}\" → {s} (mods={d})\n", .{ tok.offset, tok.offset + tok.length, text, type_name, tok.modifiers });
    }
    std.debug.print("=================================\n", .{});

    try std.testing.expect(tokens.len > 0);
}
