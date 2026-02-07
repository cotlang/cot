//! Recursive descent parser for Cot.

const std = @import("std");
const source = @import("source.zig");
const token = @import("token.zig");
const scanner = @import("scanner.zig");
const ast = @import("ast.zig");
const errors = @import("errors.zig");

const Token = token.Token;
const Pos = source.Pos;
const Span = source.Span;
const Scanner = scanner.Scanner;
const TokenInfo = scanner.TokenInfo;
const Ast = ast.Ast;
const NodeIndex = ast.NodeIndex;
const null_node = ast.null_node;
const ErrorReporter = errors.ErrorReporter;
const ErrorCode = errors.ErrorCode;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    scan: *Scanner,
    tree: *Ast,
    err: *ErrorReporter,
    tok: TokenInfo,
    peek_tok: ?TokenInfo,
    nest_lev: u32,

    const max_nest_lev: u32 = 10000;
    pub const ParseError = error{OutOfMemory};

    pub fn init(allocator: std.mem.Allocator, scan: *Scanner, tree: *Ast, err: *ErrorReporter) Parser {
        var p = Parser{ .allocator = allocator, .scan = scan, .tree = tree, .err = err, .tok = undefined, .peek_tok = null, .nest_lev = 0 };
        p.advance();
        return p;
    }

    fn pos(self: *const Parser) Pos { return self.tok.span.start; }

    fn advance(self: *Parser) void {
        if (self.peek_tok) |peek| { self.tok = peek; self.peek_tok = null; } else { self.tok = self.scan.next(); }
    }

    fn peekToken(self: *Parser) TokenInfo {
        if (self.peek_tok == null) self.peek_tok = self.scan.next();
        return self.peek_tok.?;
    }

    fn check(self: *const Parser, t: Token) bool { return self.tok.tok == t; }

    fn peekNextIsPeriod(self: *Parser) bool {
        return self.check(.lbrace) and self.peekToken().tok == .period;
    }

    fn match(self: *Parser, t: Token) bool {
        if (self.check(t)) { self.advance(); return true; }
        return false;
    }

    fn expect(self: *Parser, t: Token) bool {
        if (self.check(t)) { self.advance(); return true; }
        self.unexpectedToken(t);
        return false;
    }

    fn unexpectedToken(self: *Parser, expected: Token) void {
        self.err.errorAt(self.pos(), switch (expected) {
            .ident => "expected identifier", .lbrace => "expected '{'", .rbrace => "expected '}'",
            .lparen => "expected '('", .rparen => "expected ')'", .lbrack => "expected '['",
            .rbrack => "expected ']'", .semicolon => "expected ';'", .colon => "expected ':'",
            .comma => "expected ','", .assign => "expected '='", .fat_arrow => "expected '=>'",
            .kw_in => "expected 'in'", else => "unexpected token",
        });
    }

    fn syntaxError(self: *Parser, msg: []const u8) void { self.err.errorAt(self.pos(), msg); }

    fn incNest(self: *Parser) bool {
        self.nest_lev += 1;
        if (self.nest_lev > max_nest_lev) { self.syntaxError("exceeded maximum nesting depth"); return false; }
        return true;
    }

    fn decNest(self: *Parser) void { self.nest_lev -= 1; }

    // File parsing

    pub fn parseFile(self: *Parser) ParseError!void {
        const start = self.pos();
        var decls = std.ArrayListUnmanaged(NodeIndex){};
        defer decls.deinit(self.allocator);

        while (!self.check(.eof)) {
            if (try self.parseDecl()) |decl_idx| try decls.append(self.allocator, decl_idx) else self.advance();
        }

        self.tree.file = .{
            .filename = self.scan.src.filename,
            .decls = try self.allocator.dupe(NodeIndex, decls.items),
            .span = Span.init(start, self.pos()),
        };
    }

    // Declaration parsing

    fn parseDecl(self: *Parser) ParseError!?NodeIndex {
        return switch (self.tok.tok) {
            .kw_extern => self.parseExternFn(),
            .kw_fn => self.parseFnDecl(false),
            .kw_var => self.parseVarDecl(false),
            .kw_const => self.parseVarDecl(true),
            .kw_struct => self.parseStructDecl(),
            .kw_impl => self.parseImplBlock(),
            .kw_enum => self.parseEnumDecl(),
            .kw_union => self.parseUnionDecl(),
            .kw_type => self.parseTypeAlias(),
            .kw_import => self.parseImportDecl(),
            .kw_test => self.parseTestDecl(),
            else => { self.syntaxError("expected declaration"); return null; },
        };
    }

    fn parseExternFn(self: *Parser) ParseError!?NodeIndex {
        self.advance();
        if (!self.check(.kw_fn)) { self.syntaxError("expected 'fn' after 'extern'"); return null; }
        return self.parseFnDecl(true);
    }

    fn parseFnDecl(self: *Parser, is_extern: bool) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected function name"); return null; }
        const name = self.tok.text;
        self.advance();

        if (!self.expect(.lparen)) return null;

        // Zig-inspired syntax: fn max(T)(a: T, b: T) T
        // Zig uses fn max(comptime T: type) with same paren syntax for comptime/runtime args.
        // Go uses fn max[T comparable](a, b T) with bracket syntax.
        // We use Zig's paren convention: bare idents (no ':') = type params.
        var type_params: []const []const u8 = &.{};
        if (self.check(.ident) and self.peekToken().tok != .colon) {
            var tp = std.ArrayListUnmanaged([]const u8){};
            defer tp.deinit(self.allocator);
            while (!self.check(.rparen) and !self.check(.eof)) {
                if (!self.check(.ident)) { self.syntaxError("expected type parameter name"); return null; }
                try tp.append(self.allocator, self.tok.text);
                self.advance();
                if (!self.match(.comma)) break;
            }
            if (!self.expect(.rparen)) return null;
            type_params = try self.allocator.dupe([]const u8, tp.items);
            // Expect second ( for value parameters
            if (!self.expect(.lparen)) return null;
        }

        const params = try self.parseFieldList(.rparen);
        if (!self.expect(.rparen)) return null;

        var return_type: NodeIndex = null_node;
        if (!self.check(.lbrace) and !self.check(.semicolon) and !self.check(.eof))
            return_type = try self.parseType() orelse null_node;

        var body: NodeIndex = null_node;
        if (is_extern) {
            if (self.check(.lbrace)) { self.syntaxError("extern functions cannot have a body"); return null; }
            if (!self.expect(.semicolon)) return null;
        } else if (self.check(.lbrace)) {
            body = try self.parseBlock() orelse return null;
        }

        return try self.tree.addDecl(.{ .fn_decl = .{ .name = name, .type_params = type_params, .params = params, .return_type = return_type, .body = body, .is_extern = is_extern, .span = Span.init(start, self.pos()) } });
    }

    fn parseFieldList(self: *Parser, end_tok: Token) ParseError![]const ast.Field {
        var fields = std.ArrayListUnmanaged(ast.Field){};
        defer fields.deinit(self.allocator);

        while (!self.check(end_tok) and !self.check(.eof)) {
            const field_start = self.pos();
            if (!self.check(.ident)) break;
            const field_name = self.tok.text;
            self.advance();
            if (!self.expect(.colon)) break;
            const type_expr = try self.parseType() orelse break;
            var default_value: NodeIndex = null_node;
            if (self.match(.assign)) default_value = try self.parseExpr() orelse break;
            try fields.append(self.allocator, .{ .name = field_name, .type_expr = type_expr, .default_value = default_value, .span = Span.init(field_start, self.pos()) });
            if (!self.match(.comma)) break;
        }
        return try self.allocator.dupe(ast.Field, fields.items);
    }

    fn parseVarDecl(self: *Parser, is_const: bool) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected variable name"); return null; }
        const name = self.tok.text;
        self.advance();

        // Check for error set declaration: const MyError = error { Fail, NotFound }
        if (is_const and self.check(.assign)) {
            const saved_tok = self.tok;
            const saved_peek = self.peek_tok;
            self.advance(); // consume '='
            if (self.check(.kw_error) and self.peekToken().tok == .lbrace) {
                self.advance(); // consume 'error'
                self.advance(); // consume '{'
                var variants = std.ArrayListUnmanaged([]const u8){};
                defer variants.deinit(self.allocator);
                while (!self.check(.rbrace) and !self.check(.eof)) {
                    if (!self.check(.ident)) break;
                    try variants.append(self.allocator, self.tok.text);
                    self.advance();
                    if (!self.match(.comma)) break;
                }
                if (!self.expect(.rbrace)) return null;
                return try self.tree.addDecl(.{ .error_set_decl = .{ .name = name, .variants = try self.allocator.dupe([]const u8, variants.items), .span = Span.init(start, self.pos()) } });
            }
            // Not an error set, restore state and continue as normal var decl
            self.tok = saved_tok;
            self.peek_tok = saved_peek;
        }

        var type_expr: NodeIndex = null_node;
        if (self.match(.colon)) type_expr = try self.parseType() orelse null_node;
        var value: NodeIndex = null_node;
        if (self.match(.assign)) value = try self.parseExpr() orelse null_node;
        _ = self.match(.semicolon);
        return try self.tree.addDecl(.{ .var_decl = .{ .name = name, .type_expr = type_expr, .value = value, .is_const = is_const, .span = Span.init(start, self.pos()) } });
    }

    fn parseStructDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected struct name"); return null; }
        const name = self.tok.text;
        self.advance();

        // Parse optional type parameters: struct Pair(T, U) { ... }
        var type_params: []const []const u8 = &.{};
        if (self.check(.lparen)) {
            self.advance();
            var tp = std.ArrayListUnmanaged([]const u8){};
            defer tp.deinit(self.allocator);
            while (!self.check(.rparen) and !self.check(.eof)) {
                if (!self.check(.ident)) { self.syntaxError("expected type parameter name"); return null; }
                try tp.append(self.allocator, self.tok.text);
                self.advance();
                if (!self.match(.comma)) break;
            }
            if (!self.expect(.rparen)) return null;
            type_params = try self.allocator.dupe([]const u8, tp.items);
        }

        if (!self.expect(.lbrace)) return null;
        const fields = try self.parseFieldList(.rbrace);
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .struct_decl = .{ .name = name, .type_params = type_params, .fields = fields, .span = Span.init(start, self.pos()) } });
    }

    fn parseImplBlock(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected type name after 'impl'"); return null; }
        const type_name = self.tok.text;
        self.advance();
        if (!self.expect(.lbrace)) return null;

        var methods = std.ArrayListUnmanaged(NodeIndex){};
        defer methods.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.check(.kw_fn)) {
                if (try self.parseFnDecl(false)) |idx| try methods.append(self.allocator, idx);
            } else { self.syntaxError("expected 'fn' in impl block"); self.advance(); }
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .impl_block = .{ .type_name = type_name, .methods = try self.allocator.dupe(NodeIndex, methods.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseEnumDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected enum name"); return null; }
        const name = self.tok.text;
        self.advance();
        var backing_type: NodeIndex = null_node;
        if (self.match(.colon)) backing_type = try self.parseType() orelse null_node;
        if (!self.expect(.lbrace)) return null;

        var variants = std.ArrayListUnmanaged(ast.EnumVariant){};
        defer variants.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const var_start = self.pos();
            if (!self.check(.ident)) break;
            const var_name = self.tok.text;
            self.advance();
            var value: NodeIndex = null_node;
            if (self.match(.assign)) value = try self.parseExpr() orelse break;
            try variants.append(self.allocator, .{ .name = var_name, .value = value, .span = Span.init(var_start, self.pos()) });
            if (!self.match(.comma)) break;
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .enum_decl = .{ .name = name, .backing_type = backing_type, .variants = try self.allocator.dupe(ast.EnumVariant, variants.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseUnionDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected union name"); return null; }
        const name = self.tok.text;
        self.advance();
        if (!self.expect(.lbrace)) return null;

        var variants = std.ArrayListUnmanaged(ast.UnionVariant){};
        defer variants.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            const var_start = self.pos();
            if (!self.check(.ident)) break;
            const var_name = self.tok.text;
            self.advance();
            var type_expr: NodeIndex = null_node;
            if (self.match(.colon)) type_expr = try self.parseType() orelse break;
            try variants.append(self.allocator, .{ .name = var_name, .type_expr = type_expr, .span = Span.init(var_start, self.pos()) });
            if (!self.match(.comma)) break;
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .union_decl = .{ .name = name, .variants = try self.allocator.dupe(ast.UnionVariant, variants.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseTypeAlias(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected type name"); return null; }
        const name = self.tok.text;
        self.advance();
        if (!self.expect(.assign)) return null;
        const target = try self.parseType() orelse return null;
        _ = self.match(.semicolon);
        return try self.tree.addDecl(.{ .type_alias = .{ .name = name, .target = target, .span = Span.init(start, self.pos()) } });
    }

    fn parseImportDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.string_lit)) { self.syntaxError("expected import path string"); return null; }
        const raw = self.tok.text;
        self.advance();
        const path = if (raw.len >= 2 and raw[0] == '"' and raw[raw.len - 1] == '"') raw[1 .. raw.len - 1] else raw;
        return try self.tree.addDecl(.{ .import_decl = .{ .path = path, .span = Span.init(start, self.pos()) } });
    }

    fn parseTestDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.string_lit)) { self.syntaxError("expected test name string"); return null; }
        const raw = self.tok.text;
        self.advance();
        const name = if (raw.len >= 2 and raw[0] == '"' and raw[raw.len - 1] == '"') raw[1 .. raw.len - 1] else raw;
        const body = try self.parseBlock() orelse return null;
        return try self.tree.addDecl(.{ .test_decl = .{ .name = name, .body = body, .span = Span.init(start, self.pos()) } });
    }

    // Type parsing

    fn parseType(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();

        if (self.match(.question)) {
            const inner = try self.parseType() orelse return null;
            return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .optional = inner }, .span = Span.init(start, self.pos()) } });
        }
        if (self.match(.lnot)) {
            const inner = try self.parseType() orelse return null;
            return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .error_union = .{ .elem = inner } }, .span = Span.init(start, self.pos()) } });
        }
        if (self.match(.mul)) {
            const inner = try self.parseType() orelse return null;
            return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .pointer = inner }, .span = Span.init(start, self.pos()) } });
        }
        if (self.match(.lbrack)) {
            if (self.match(.rbrack)) {
                const elem = try self.parseType() orelse return null;
                return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .slice = elem }, .span = Span.init(start, self.pos()) } });
            } else {
                const size = try self.parseExpr() orelse return null;
                if (!self.expect(.rbrack)) return null;
                const elem = try self.parseType() orelse return null;
                return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .array = .{ .size = size, .elem = elem } }, .span = Span.init(start, self.pos()) } });
            }
        }

        if (self.check(.ident) or self.tok.tok.isTypeKeyword()) {
            const type_name = if (self.tok.text.len > 0) self.tok.text else self.tok.tok.string();
            self.advance();
            // Generic type instantiation: Name(arg1, arg2, ...) or legacy Map<K,V> / List<T>
            if (self.match(.lss)) {
                // Legacy angle bracket syntax for Map<K,V> and List<T>
                if (std.mem.eql(u8, type_name, "Map")) {
                    const key = try self.parseType() orelse return null;
                    if (!self.expect(.comma)) return null;
                    const val = try self.parseType() orelse return null;
                    if (!self.expect(.gtr)) return null;
                    return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .map = .{ .key = key, .value = val } }, .span = Span.init(start, self.pos()) } });
                } else if (std.mem.eql(u8, type_name, "List")) {
                    const elem = try self.parseType() orelse return null;
                    if (!self.expect(.gtr)) return null;
                    return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .list = elem }, .span = Span.init(start, self.pos()) } });
                } else { self.syntaxError("unknown generic type"); return null; }
            }
            // Paren syntax for generics: Name(T1, T2, ...)
            if (self.check(.lparen)) {
                self.advance();
                var args = std.ArrayListUnmanaged(NodeIndex){};
                defer args.deinit(self.allocator);
                while (!self.check(.rparen) and !self.check(.eof)) {
                    const arg = try self.parseType() orelse break;
                    try args.append(self.allocator, arg);
                    if (!self.match(.comma)) break;
                }
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .generic_instance = .{ .name = type_name, .type_args = try self.allocator.dupe(NodeIndex, args.items) } }, .span = Span.init(start, self.pos()) } });
            }
            // Check for E!T (error union with named error set)
            if (self.match(.lnot)) {
                const error_set_node = try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .named = type_name }, .span = Span.init(start, self.pos()) } });
                const elem = try self.parseType() orelse return null;
                return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .error_union = .{ .error_set = error_set_node, .elem = elem } }, .span = Span.init(start, self.pos()) } });
            }
            return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .named = type_name }, .span = Span.init(start, self.pos()) } });
        }

        if (self.match(.kw_fn)) {
            if (!self.expect(.lparen)) return null;
            var params = std.ArrayListUnmanaged(NodeIndex){};
            defer params.deinit(self.allocator);
            while (!self.check(.rparen) and !self.check(.eof)) {
                const param = try self.parseType() orelse break;
                try params.append(self.allocator, param);
                if (!self.match(.comma)) break;
            }
            if (!self.expect(.rparen)) return null;
            var ret: NodeIndex = null_node;
            if (self.match(.arrow)) ret = try self.parseType() orelse null_node;
            return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .function = .{ .params = try self.allocator.dupe(NodeIndex, params.items), .ret = ret } }, .span = Span.init(start, self.pos()) } });
        }
        return null;
    }

    // Expression parsing

    pub fn parseExpr(self: *Parser) ParseError!?NodeIndex { return self.parseBinaryExpr(0); }

    fn parseBinaryExpr(self: *Parser, min_prec: u8) ParseError!?NodeIndex {
        if (!self.incNest()) return null;
        defer self.decNest();

        var left = try self.parseUnaryExpr() orelse return null;
        while (true) {
            // Handle catch as a low-precedence postfix operator (precedence 1)
            if (self.check(.kw_catch) and min_prec <= 1) {
                const left_span = self.tree.getNode(left).?.span();
                self.advance();
                // Optional capture: catch |err| { ... }
                var capture: []const u8 = "";
                if (self.match(.@"or")) {
                    if (self.check(.ident)) { capture = self.tok.text; self.advance(); } else { self.syntaxError("expected identifier for error capture"); return null; }
                    if (!self.expect(.@"or")) return null;
                }
                const fallback = try self.parseBinaryExpr(2) orelse { self.err.errorWithCode(self.pos(), .e201, "expected expression after 'catch'"); return null; };
                left = try self.tree.addExpr(.{ .catch_expr = .{ .operand = left, .capture = capture, .fallback = fallback, .span = Span.init(left_span.start, self.pos()) } });
                continue;
            }
            const op = self.tok.tok;
            const prec = op.precedence();
            if (prec < min_prec or prec == 0) break;
            self.advance();
            const right = try self.parseBinaryExpr(prec + 1) orelse { self.err.errorWithCode(self.pos(), .e201, "expected expression after operator"); return null; };
            const left_span = self.tree.getNode(left).?.span();
            const right_span = self.tree.getNode(right).?.span();
            left = try self.tree.addExpr(.{ .binary = .{ .op = op, .left = left, .right = right, .span = Span.init(left_span.start, right_span.end) } });
        }
        return left;
    }

    fn parseUnaryExpr(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        switch (self.tok.tok) {
            .@"and" => {
                self.advance();
                const operand = try self.parseUnaryExpr() orelse return null;
                return try self.tree.addExpr(.{ .addr_of = .{ .operand = operand, .span = Span.init(start, self.pos()) } });
            },
            .kw_try => {
                self.advance();
                const operand = try self.parsePrimaryExpr() orelse return null;
                return try self.tree.addExpr(.{ .try_expr = .{ .operand = operand, .span = Span.init(start, self.pos()) } });
            },
            .sub, .lnot, .not, .kw_not => {
                const op = self.tok.tok;
                self.advance();
                const operand = try self.parseUnaryExpr() orelse return null;
                return try self.tree.addExpr(.{ .unary = .{ .op = op, .operand = operand, .span = Span.init(start, self.pos()) } });
            },
            else => return self.parsePrimaryExpr(),
        }
    }

    fn parsePrimaryExpr(self: *Parser) ParseError!?NodeIndex {
        var expr = try self.parseOperand() orelse return null;

        while (true) {
            if (self.match(.period_star)) {
                const s = self.tree.getNode(expr).?.span();
                expr = try self.tree.addExpr(.{ .deref = .{ .operand = expr, .span = Span.init(s.start, self.pos()) } });
            } else if (self.match(.period_question)) {
                const s = self.tree.getNode(expr).?.span();
                expr = try self.tree.addExpr(.{ .unary = .{ .op = .question, .operand = expr, .span = Span.init(s.start, self.pos()) } });
            } else if (self.match(.period)) {
                if (self.check(.ident)) {
                    const field = self.tok.text;
                    self.advance();
                    const s = self.tree.getNode(expr).?.span();
                    expr = try self.tree.addExpr(.{ .field_access = .{ .base = expr, .field = field, .span = Span.init(s.start, self.pos()) } });
                } else { self.syntaxError("expected field name after '.'"); return null; }
            } else if (self.match(.lbrack)) {
                const s = self.tree.getNode(expr).?.span();
                if (self.match(.colon)) {
                    var end_idx: NodeIndex = null_node;
                    if (!self.check(.rbrack)) end_idx = try self.parseExpr() orelse return null;
                    if (!self.expect(.rbrack)) return null;
                    expr = try self.tree.addExpr(.{ .slice_expr = .{ .base = expr, .start = null_node, .end = end_idx, .span = Span.init(s.start, self.pos()) } });
                } else {
                    const idx = try self.parseExpr() orelse return null;
                    if (self.match(.colon)) {
                        var end_idx: NodeIndex = null_node;
                        if (!self.check(.rbrack)) end_idx = try self.parseExpr() orelse return null;
                        if (!self.expect(.rbrack)) return null;
                        expr = try self.tree.addExpr(.{ .slice_expr = .{ .base = expr, .start = idx, .end = end_idx, .span = Span.init(s.start, self.pos()) } });
                    } else {
                        if (!self.expect(.rbrack)) return null;
                        expr = try self.tree.addExpr(.{ .index = .{ .base = expr, .idx = idx, .span = Span.init(s.start, self.pos()) } });
                    }
                }
            } else if (self.match(.lparen)) {
                const s = self.tree.getNode(expr).?.span();
                var args = std.ArrayListUnmanaged(NodeIndex){};
                defer args.deinit(self.allocator);
                while (!self.check(.rparen) and !self.check(.eof)) {
                    const arg = try self.parseExpr() orelse break;
                    try args.append(self.allocator, arg);
                    if (!self.match(.comma)) break;
                }
                if (!self.expect(.rparen)) return null;
                expr = try self.tree.addExpr(.{ .call = .{ .callee = expr, .args = try self.allocator.dupe(NodeIndex, args.items), .span = Span.init(s.start, self.pos()) } });
            } else if (self.check(.lbrace)) {
                if (self.tree.getNode(expr)) |n| {
                    if (n.asExpr()) |e| {
                        if (e == .ident) {
                            const type_name = e.ident.name;
                            if (type_name.len > 0 and std.ascii.isUpper(type_name[0]) and self.peekNextIsPeriod()) {
                                const s = self.tree.getNode(expr).?.span();
                                self.advance();
                                var fields = std.ArrayListUnmanaged(ast.FieldInit){};
                                defer fields.deinit(self.allocator);
                                while (!self.check(.rbrace) and !self.check(.eof)) {
                                    if (!self.expect(.period)) return null;
                                    if (!self.check(.ident)) { self.syntaxError("expected field name"); return null; }
                                    const fname = self.tok.text;
                                    const fstart = self.pos();
                                    self.advance();
                                    if (!self.expect(.assign)) return null;
                                    const val = try self.parseExpr() orelse return null;
                                    try fields.append(self.allocator, .{ .name = fname, .value = val, .span = Span.init(fstart, self.pos()) });
                                    if (!self.match(.comma)) break;
                                }
                                if (!self.expect(.rbrace)) return null;
                                expr = try self.tree.addExpr(.{ .struct_init = .{ .type_name = type_name, .fields = try self.allocator.dupe(ast.FieldInit, fields.items), .span = Span.init(s.start, self.pos()) } });
                                continue;
                            }
                        }
                    }
                }
                break;
            } else break;
        }
        return expr;
    }

    fn parseOperand(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        switch (self.tok.tok) {
            .ident => { const n = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .ident = .{ .name = n, .span = Span.init(start, self.pos()) } }); },
            .int_lit => { const v = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .int, .value = v, .span = Span.init(start, self.pos()) } }); },
            .float_lit => { const v = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .float, .value = v, .span = Span.init(start, self.pos()) } }); },
            .string_lit => { const v = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .string, .value = v, .span = Span.init(start, self.pos()) } }); },
            .char_lit => { const v = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .char, .value = v, .span = Span.init(start, self.pos()) } }); },
            .kw_true => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .true_lit, .value = "true", .span = Span.init(start, self.pos()) } }); },
            .kw_false => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .false_lit, .value = "false", .span = Span.init(start, self.pos()) } }); },
            .kw_null => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .null_lit, .value = "null", .span = Span.init(start, self.pos()) } }); },
            .kw_undefined => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .undefined_lit, .value = "undefined", .span = Span.init(start, self.pos()) } }); },
            .lparen => {
                self.advance();
                const inner = try self.parseExpr() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .paren = .{ .inner = inner, .span = Span.init(start, self.pos()) } });
            },
            .lbrack => {
                self.advance();
                var elems = std.ArrayListUnmanaged(NodeIndex){};
                defer elems.deinit(self.allocator);
                while (!self.check(.rbrack) and !self.check(.eof)) {
                    const e = try self.parseExpr() orelse break;
                    try elems.append(self.allocator, e);
                    if (!self.match(.comma)) break;
                }
                if (!self.expect(.rbrack)) return null;
                return try self.tree.addExpr(.{ .array_literal = .{ .elements = try self.allocator.dupe(NodeIndex, elems.items), .span = Span.init(start, self.pos()) } });
            },
            .lbrace => return self.parseBlockExpr(),
            .kw_if => return self.parseIfExpr(),
            .kw_switch => return self.parseSwitchExpr(),
            .kw_new => {
                // new Type { field: value, ... }
                // Reference: Go's walkNew (walk/builtin.go:601-616)
                self.advance();
                if (!self.check(.ident)) {
                    self.err.errorWithCode(self.pos(), .e202, "expected type name after 'new'");
                    return null;
                }
                const type_name = self.tok.text;
                self.advance();
                if (!self.expect(.lbrace)) return null;
                var fields = std.ArrayListUnmanaged(ast.FieldInit){};
                while (!self.check(.rbrace) and !self.check(.eof)) {
                    if (!self.check(.ident)) {
                        self.syntaxError("expected field name");
                        return null;
                    }
                    const fname = self.tok.text;
                    const fstart = self.pos();
                    self.advance();
                    if (!self.expect(.colon)) return null;
                    const fval = try self.parseExpr() orelse return null;
                    try fields.append(self.allocator, .{ .name = fname, .value = fval, .span = Span.init(fstart, self.pos()) });
                    if (!self.check(.rbrace) and !self.expect(.comma)) return null;
                }
                if (!self.expect(.rbrace)) return null;
                return try self.tree.addExpr(.{ .new_expr = .{
                    .type_name = type_name,
                    .fields = try self.allocator.dupe(ast.FieldInit, fields.items),
                    .span = Span.init(start, self.pos()),
                } });
            },
            .kw_fn => {
                // Anonymous function (closure): fn(params) rettype { body }
                self.advance();
                if (!self.expect(.lparen)) return null;
                const params = try self.parseFieldList(.rparen);
                if (!self.expect(.rparen)) return null;
                var return_type: NodeIndex = null_node;
                if (!self.check(.lbrace) and !self.check(.eof))
                    return_type = try self.parseType() orelse null_node;
                const body = try self.parseBlock() orelse return null;
                return try self.tree.addExpr(.{ .closure_expr = .{
                    .params = params,
                    .return_type = return_type,
                    .body = body,
                    .span = Span.init(start, self.pos()),
                } });
            },
            .kw_error => {
                // error.Variant
                self.advance();
                if (!self.expect(.period)) return null;
                if (!self.check(.ident)) { self.syntaxError("expected error variant name"); return null; }
                const error_name = self.tok.text;
                self.advance();
                return try self.tree.addExpr(.{ .error_literal = .{ .error_name = error_name, .span = Span.init(start, self.pos()) } });
            },
            .period => {
                self.advance();
                if (!self.check(.ident)) { self.syntaxError("expected variant name after '.'"); return null; }
                const n = self.tok.text;
                self.advance();
                return try self.tree.addExpr(.{ .field_access = .{ .base = null_node, .field = n, .span = Span.init(start, self.pos()) } });
            },
            .at => return self.parseBuiltinCall(start),
            else => {
                if (self.tok.tok.isTypeKeyword()) {
                    const n = self.tok.tok.string();
                    self.advance();
                    return try self.tree.addExpr(.{ .ident = .{ .name = n, .span = Span.init(start, self.pos()) } });
                }
                self.err.errorWithCode(self.pos(), .e201, "expected expression");
                return null;
            },
        }
    }

    fn parseBuiltinCall(self: *Parser, start: Pos) ParseError!?NodeIndex {
        self.advance(); // @
        if (!self.check(.ident) and !self.check(.kw_string)) { self.syntaxError("expected builtin name after '@'"); return null; }
        const is_string = self.check(.kw_string);
        const name = if (is_string) "string" else self.tok.text;
        self.advance();
        if (!self.expect(.lparen)) return null;

        if (is_string) {
            const ptr = try self.parseExpr() orelse return null;
            if (!self.expect(.comma)) return null;
            const len = try self.parseExpr() orelse return null;
            if (!self.expect(.rparen)) return null;
            return try self.tree.addExpr(.{ .builtin_call = .{ .name = name, .type_arg = null_node, .args = .{ ptr, len }, .span = Span.init(start, self.pos()) } });
        } else if (std.mem.eql(u8, name, "intCast") or std.mem.eql(u8, name, "ptrCast") or std.mem.eql(u8, name, "intToPtr")) {
            const t = try self.parseType() orelse return null;
            if (!self.expect(.comma)) return null;
            const v = try self.parseExpr() orelse return null;
            if (!self.expect(.rparen)) return null;
            return try self.tree.addExpr(.{ .builtin_call = .{ .name = name, .type_arg = t, .args = .{ v, null_node }, .span = Span.init(start, self.pos()) } });
        } else if (std.mem.eql(u8, name, "ptrToInt") or std.mem.eql(u8, name, "assert")) {
            const arg = try self.parseExpr() orelse return null;
            if (!self.expect(.rparen)) return null;
            return try self.tree.addExpr(.{ .builtin_call = .{ .name = name, .type_arg = null_node, .args = .{ arg, null_node }, .span = Span.init(start, self.pos()) } });
        } else {
            const t = try self.parseType() orelse return null;
            if (!self.expect(.rparen)) return null;
            return try self.tree.addExpr(.{ .builtin_call = .{ .name = name, .type_arg = t, .args = .{ null_node, null_node }, .span = Span.init(start, self.pos()) } });
        }
    }

    fn parseBlockExpr(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        if (!self.expect(.lbrace)) return null;
        var stmts = std.ArrayListUnmanaged(NodeIndex){};
        defer stmts.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (try self.parseStmt()) |s| try stmts.append(self.allocator, s) else self.advance();
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addExpr(.{ .block_expr = .{ .stmts = try self.allocator.dupe(NodeIndex, stmts.items), .expr = null_node, .span = Span.init(start, self.pos()) } });
    }

    fn parseIfExpr(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        const cond = try self.parseExpr() orelse return null;
        if (!self.check(.lbrace)) { self.err.errorWithCode(self.pos(), .e204, "expected '{' after if condition"); return null; }
        const then_br = try self.parseBlockExpr() orelse return null;
        var else_br: NodeIndex = null_node;
        if (self.match(.kw_else)) {
            if (self.check(.kw_if)) else_br = try self.parseIfExpr() orelse return null
            else if (self.check(.lbrace)) else_br = try self.parseBlockExpr() orelse return null
            else { self.syntaxError("expected '{' or 'if' after 'else'"); return null; }
        }
        return try self.tree.addExpr(.{ .if_expr = .{ .condition = cond, .then_branch = then_br, .else_branch = else_br, .span = Span.init(start, self.pos()) } });
    }

    fn parseSwitchExpr(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        const subj = try self.parseOperand() orelse return null;
        if (!self.expect(.lbrace)) return null;

        var cases = std.ArrayListUnmanaged(ast.SwitchCase){};
        defer cases.deinit(self.allocator);
        var else_body: NodeIndex = null_node;

        while (!self.check(.rbrace) and !self.check(.eof)) {
            const case_start = self.pos();
            if (self.match(.kw_else)) {
                if (!self.expect(.fat_arrow)) return null;
                else_body = try self.parseExpr() orelse return null;
                _ = self.match(.comma);
                continue;
            }

            var patterns = std.ArrayListUnmanaged(NodeIndex){};
            defer patterns.deinit(self.allocator);
            const first = try self.parsePrimaryExpr() orelse return null;
            try patterns.append(self.allocator, first);
            while (self.check(.comma) and !self.check(.fat_arrow)) {
                self.advance();
                if (self.check(.fat_arrow) or self.check(.kw_else)) break;
                const p = try self.parsePrimaryExpr() orelse return null;
                try patterns.append(self.allocator, p);
            }

            var capture: []const u8 = "";
            if (self.match(.@"or")) {
                if (self.check(.ident)) { capture = self.tok.text; self.advance(); } else { self.syntaxError("expected identifier for payload capture"); return null; }
                if (!self.expect(.@"or")) return null;
            }
            if (!self.expect(.fat_arrow)) return null;
            const body = try self.parseExpr() orelse return null;
            try cases.append(self.allocator, .{ .patterns = try self.allocator.dupe(NodeIndex, patterns.items), .capture = capture, .body = body, .span = Span.init(case_start, self.pos()) });
            _ = self.match(.comma);
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addExpr(.{ .switch_expr = .{ .subject = subj, .cases = try self.allocator.dupe(ast.SwitchCase, cases.items), .else_body = else_body, .span = Span.init(start, self.pos()) } });
    }

    // Statement parsing

    fn parseBlock(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        if (!self.expect(.lbrace)) return null;
        var stmts = std.ArrayListUnmanaged(NodeIndex){};
        defer stmts.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (try self.parseStmt()) |s| try stmts.append(self.allocator, s) else self.advance();
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addStmt(.{ .block_stmt = .{ .stmts = try self.allocator.dupe(NodeIndex, stmts.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseStmt(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        switch (self.tok.tok) {
            .kw_return => {
                self.advance();
                var val: NodeIndex = null_node;
                if (!self.check(.rbrace) and !self.check(.semicolon) and !self.check(.eof)) val = try self.parseExpr() orelse null_node;
                _ = self.match(.semicolon);
                return try self.tree.addStmt(.{ .return_stmt = .{ .value = val, .span = Span.init(start, self.pos()) } });
            },
            .kw_var, .kw_let => return self.parseVarStmt(false),
            .kw_const => return self.parseVarStmt(true),
            .kw_if => return self.parseIfStmt(),
            .kw_while => return self.parseWhileStmt(null),
            .kw_for => return self.parseForStmt(),
            .kw_break => {
                self.advance();
                var label: ?[]const u8 = null;
                if (self.check(.colon)) { self.advance(); if (self.check(.ident)) { label = self.tok.text; self.advance(); } }
                _ = self.match(.semicolon);
                return try self.tree.addStmt(.{ .break_stmt = .{ .label = label, .span = Span.init(start, self.pos()) } });
            },
            .kw_continue => {
                self.advance();
                var label: ?[]const u8 = null;
                if (self.check(.colon)) { self.advance(); if (self.check(.ident)) { label = self.tok.text; self.advance(); } }
                _ = self.match(.semicolon);
                return try self.tree.addStmt(.{ .continue_stmt = .{ .label = label, .span = Span.init(start, self.pos()) } });
            },
            .kw_defer => {
                self.advance();
                // defer can take a block { ... } or a simple expression/assignment
                if (self.check(.lbrace)) {
                    const block = try self.parseBlock() orelse return null;
                    return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = block, .span = Span.init(start, self.pos()) } });
                }
                // Parse expression, then check for assignment operator
                const e = try self.parseExpr() orelse return null;
                if (self.tok.tok == .assign or self.tok.tok.isAssignment()) {
                    const op = self.tok.tok;
                    self.advance();
                    const rhs = try self.parseExpr() orelse return null;
                    _ = self.match(.semicolon);
                    const assign_node = try self.tree.addStmt(.{ .assign_stmt = .{
                        .target = e,
                        .value = rhs,
                        .op = op,
                        .span = Span.init(start, self.pos()),
                    } });
                    return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = assign_node, .span = Span.init(start, self.pos()) } });
                }
                _ = self.match(.semicolon);
                return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = e, .span = Span.init(start, self.pos()) } });
            },
            .ident => {
                if (self.peekToken().tok == .colon) {
                    const label = self.tok.text;
                    self.advance();
                    self.advance();
                    if (self.check(.kw_while)) return self.parseWhileStmt(label);
                    self.err.errorWithCode(self.pos(), .e201, "expected 'while' after label");
                    return null;
                }
                return self.parseExprOrAssign(start);
            },
            else => return self.parseExprOrAssign(start),
        }
    }

    fn parseExprOrAssign(self: *Parser, start: Pos) ParseError!?NodeIndex {
        const expr = try self.parseExpr() orelse return null;
        if (self.tok.tok == .assign or self.tok.tok.isAssignment()) {
            const op = self.tok.tok;
            self.advance();
            const val = try self.parseExpr() orelse return null;
            _ = self.match(.semicolon);
            return try self.tree.addStmt(.{ .assign_stmt = .{ .target = expr, .op = op, .value = val, .span = Span.init(start, self.pos()) } });
        }
        _ = self.match(.semicolon);
        return try self.tree.addStmt(.{ .expr_stmt = .{ .expr = expr, .span = Span.init(start, self.pos()) } });
    }

    fn parseVarStmt(self: *Parser, is_const: bool) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected variable name"); return null; }
        const name = self.tok.text;
        self.advance();
        var type_expr: NodeIndex = null_node;
        if (self.match(.colon)) type_expr = try self.parseType() orelse null_node;
        var val: NodeIndex = null_node;
        if (self.match(.assign)) val = try self.parseExpr() orelse null_node;
        _ = self.match(.semicolon);
        return try self.tree.addStmt(.{ .var_stmt = .{ .name = name, .type_expr = type_expr, .value = val, .is_const = is_const, .span = Span.init(start, self.pos()) } });
    }

    fn parseIfStmt(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        const cond = try self.parseExpr() orelse return null;
        const then_br = if (self.check(.lbrace)) try self.parseBlock() orelse return null else try self.parseStmt() orelse return null;
        var else_br: NodeIndex = null_node;
        if (self.match(.kw_else)) {
            if (self.check(.kw_if)) else_br = try self.parseIfStmt() orelse return null
            else if (self.check(.lbrace)) else_br = try self.parseBlock() orelse return null
            else else_br = try self.parseStmt() orelse return null;
        }
        return try self.tree.addStmt(.{ .if_stmt = .{ .condition = cond, .then_branch = then_br, .else_branch = else_br, .span = Span.init(start, self.pos()) } });
    }

    fn parseWhileStmt(self: *Parser, label: ?[]const u8) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        const cond = try self.parseExpr() orelse return null;
        if (!self.check(.lbrace)) { self.err.errorWithCode(self.pos(), .e204, "expected '{' after while condition"); return null; }
        const body = try self.parseBlock() orelse return null;
        return try self.tree.addStmt(.{ .while_stmt = .{ .condition = cond, .body = body, .label = label, .span = Span.init(start, self.pos()) } });
    }

    fn parseForStmt(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.ident)) {
            self.err.errorWithCode(self.pos(), .e203, "expected loop variable");
            return null;
        }
        const first_binding = self.tok.text;
        self.advance();

        // Check for indexed iteration: for i, item in collection
        var index_binding: ?[]const u8 = null;
        var value_binding: []const u8 = first_binding;
        if (self.check(.comma)) {
            self.advance();
            if (!self.check(.ident)) {
                self.err.errorWithCode(self.pos(), .e203, "expected value binding after ','");
                return null;
            }
            index_binding = first_binding;
            value_binding = self.tok.text;
            self.advance();
        }

        if (!self.expect(.kw_in)) {
            self.syntaxError("expected 'in' in for loop");
            return null;
        }

        // Parse the iterable or range expression
        const iter_or_start = try self.parseExpr() orelse return null;

        // Check for range syntax: for i in start..end
        var range_start: ast.NodeIndex = ast.null_node;
        var range_end: ast.NodeIndex = ast.null_node;
        var iterable: ast.NodeIndex = iter_or_start;

        if (self.match(.period_period)) {
            // Range syntax: start..end
            range_start = iter_or_start;
            range_end = try self.parseExpr() orelse return null;
            iterable = ast.null_node;
        }

        if (!self.check(.lbrace)) {
            self.err.errorWithCode(self.pos(), .e204, "expected '{' after for clause");
            return null;
        }
        const body = try self.parseBlock() orelse return null;

        return try self.tree.addStmt(.{ .for_stmt = .{
            .binding = value_binding,
            .index_binding = index_binding,
            .iterable = iterable,
            .range_start = range_start,
            .range_end = range_end,
            .body = body,
            .span = Span.init(start, self.pos()),
        } });
    }
};

// ============================================================================
// Tests
// ============================================================================

fn testParse(content: []const u8) !struct { Ast, *ErrorReporter, std.heap.ArenaAllocator } {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    const allocator = arena.allocator();
    const src = try allocator.create(source.Source);
    src.* = source.Source.init(allocator, "test.cot", content);
    const err_reporter = try allocator.create(ErrorReporter);
    err_reporter.* = ErrorReporter.init(src, null);
    const scan_ptr = try allocator.create(Scanner);
    scan_ptr.* = Scanner.init(src);
    var tree = Ast.init(allocator);
    var parser = Parser.init(allocator, scan_ptr, &tree, err_reporter);
    try parser.parseFile();
    return .{ tree, err_reporter, arena };
}

test "parser simple function" {
    const tree, const err, var arena = try testParse("fn main() { return 42 }");
    defer arena.deinit();
    try std.testing.expect(tree.file != null);
    try std.testing.expectEqual(@as(usize, 1), tree.file.?.decls.len);
    try std.testing.expect(!err.hasErrors());
}

test "parser variable declaration" {
    const tree, _, var arena = try testParse("var x: int = 42");
    defer arena.deinit();
    try std.testing.expect(tree.file != null);
    const decl = tree.getNode(tree.file.?.decls[0]).?.asDecl().?;
    try std.testing.expect(decl == .var_decl);
    try std.testing.expectEqualStrings("x", decl.var_decl.name);
}

test "parser binary expression precedence" {
    const tree, const err, var arena = try testParse("var x = 1 + 2 * 3");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
    const decl = tree.getNode(tree.file.?.decls[0]).?.asDecl().?.var_decl;
    const expr = tree.getNode(decl.value).?.asExpr().?;
    try std.testing.expect(expr == .binary);
    try std.testing.expectEqual(Token.add, expr.binary.op);
}

test "parser struct declaration" {
    const tree, const err, var arena = try testParse("struct Point { x: int, y: int }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
    const decl = tree.getNode(tree.file.?.decls[0]).?.asDecl().?;
    try std.testing.expect(decl == .struct_decl);
    try std.testing.expectEqualStrings("Point", decl.struct_decl.name);
    try std.testing.expectEqual(@as(usize, 2), decl.struct_decl.fields.len);
}

test "parser enum declaration" {
    const tree, const err, var arena = try testParse("enum Color { red, green, blue }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
    const decl = tree.getNode(tree.file.?.decls[0]).?.asDecl().?;
    try std.testing.expect(decl == .enum_decl);
    try std.testing.expectEqual(@as(usize, 3), decl.enum_decl.variants.len);
}

test "parser union declaration" {
    const tree, const err, var arena = try testParse("union Result { ok: int, err: string }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
    const decl = tree.getNode(tree.file.?.decls[0]).?.asDecl().?;
    try std.testing.expect(decl == .union_decl);
    try std.testing.expectEqual(@as(usize, 2), decl.union_decl.variants.len);
}

test "parser if statement" {
    _, const err, var arena = try testParse("fn foo() { if x == 1 { return 1 } else { return 2 } }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
}

test "parser while loop" {
    _, const err, var arena = try testParse("fn foo() { var i = 0; while i < 10 { i = i + 1 } }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
}

test "parser for loop" {
    _, const err, var arena = try testParse("fn foo() { for x in items { print(x) } }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
}

test "parser array literal" {
    _, const err, var arena = try testParse("var arr = [1, 2, 3]");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
}

test "parser error recovery" {
    _, const err, var arena = try testParse("fn () { }");
    defer arena.deinit();
    try std.testing.expect(err.hasErrors());
}
