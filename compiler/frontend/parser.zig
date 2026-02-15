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
    /// @safe mode: enables implicit self injection in impl blocks
    safe_mode: bool = false,
    /// Current impl block type name (for implicit self injection)
    current_impl_type: ?[]const u8 = null,
    /// Whether current impl block is generic (skip implicit self for generics)
    current_impl_is_generic: bool = false,

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

    /// Check if current position is `{ ident :` or `{ ident ,` or `{ ident }` — colon-syntax struct init
    /// (including field shorthand). Handles the case where peek_tok may already be populated.
    fn peekNextIsIdentColon(self: *Parser) bool {
        if (!self.check(.lbrace)) return false;
        // Save complete parser+scanner state
        const saved_scan_pos = self.scan.pos;
        const saved_scan_ch = self.scan.ch;
        // The next token after { is either in peek_tok (if already peeked) or needs scanning
        const tok_after_brace: TokenInfo = self.peek_tok orelse self.scan.next();
        if (tok_after_brace.tok != .ident) {
            self.scan.pos = saved_scan_pos;
            self.scan.ch = saved_scan_ch;
            return false;
        }
        // Scan one more token: should be : or , or }
        const tok3 = self.scan.next();
        const is_colon_syntax = tok3.tok == .colon or tok3.tok == .comma or tok3.tok == .rbrace;
        // Restore scanner state (parser tok and peek_tok are untouched)
        self.scan.pos = saved_scan_pos;
        self.scan.ch = saved_scan_ch;
        return is_colon_syntax;
    }

    /// Parse colon-syntax struct fields (@safe only): `{ x: 10, y: 20 }` or shorthand `{ x, y }`
    /// Caller must have already consumed the `{`. Returns field list.
    fn parseColonFields(self: *Parser) ParseError![]const ast.FieldInit {
        var fields = std.ArrayListUnmanaged(ast.FieldInit){};
        defer fields.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (!self.check(.ident)) { self.syntaxError("expected field name"); return error.OutOfMemory; }
            const fname = self.tok.text;
            const fstart = self.pos();
            self.advance();
            const fval = if (self.match(.colon))
                try self.parseExpr() orelse return error.OutOfMemory
            else
                // Field shorthand: `{ x }` → `{ x: x }`
                try self.tree.addExpr(.{ .ident = .{ .name = fname, .span = Span.init(fstart, self.pos()) } });
            try fields.append(self.allocator, .{ .name = fname, .value = fval, .span = Span.init(fstart, self.pos()) });
            if (!self.match(.comma)) break;
        }
        return try self.allocator.dupe(ast.FieldInit, fields.items);
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

        // Check for @safe file annotation
        var safe_mode = false;
        if (self.check(.at)) {
            const peek = self.peekToken();
            if (peek.tok == .ident and std.mem.eql(u8, peek.text, "safe")) {
                self.advance(); // consume @
                self.advance(); // consume safe
                safe_mode = true;
                self.safe_mode = true;
            }
        }

        while (!self.check(.eof)) {
            if (try self.parseDecl()) |decl_idx| try decls.append(self.allocator, decl_idx) else self.advance();
        }

        self.tree.file = .{
            .filename = self.scan.src.filename,
            .decls = try self.allocator.dupe(NodeIndex, decls.items),
            .span = Span.init(start, self.pos()),
            .safe_mode = safe_mode,
        };
    }

    // Declaration parsing

    fn parseDecl(self: *Parser) ParseError!?NodeIndex {
        return switch (self.tok.tok) {
            .kw_extern => self.parseExternFn(),
            .kw_fn => self.parseFnDecl(false, false),
            .kw_async => self.parseAsyncFn(),
            .kw_var => self.parseVarDecl(false),
            .kw_const => self.parseVarDecl(true),
            .kw_struct => self.parseStructDecl(),
            .kw_trait => self.parseTraitDecl(),
            .kw_impl => self.parseImplBlock(),
            .kw_enum => self.parseEnumDecl(),
            .kw_union => self.parseUnionDecl(),
            .kw_type => self.parseTypeAlias(),
            .kw_import => self.parseImportDecl(),
            .kw_test => self.parseTestDecl(),
            .kw_bench => self.parseBenchDecl(),
            else => { self.syntaxError("expected declaration"); return null; },
        };
    }

    fn parseExternFn(self: *Parser) ParseError!?NodeIndex {
        self.advance();
        if (!self.check(.kw_fn)) { self.syntaxError("expected 'fn' after 'extern'"); return null; }
        return self.parseFnDecl(true, false);
    }

    fn parseAsyncFn(self: *Parser) ParseError!?NodeIndex {
        self.advance(); // consume 'async'
        if (!self.check(.kw_fn)) { self.syntaxError("expected 'fn' after 'async'"); return null; }
        return self.parseFnDecl(false, true);
    }

    fn parseFnDecl(self: *Parser, is_extern: bool, is_async: bool) ParseError!?NodeIndex {
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

        var params = try self.parseFieldList(.rparen);
        if (!self.expect(.rparen)) return null;

        // @safe implicit self: In safe-mode non-generic impl blocks, inject `self: *TypeName`
        // if the first param is not already named "self"
        if (self.safe_mode and self.current_impl_type != null and !self.current_impl_is_generic) {
            const needs_self = params.len == 0 or !std.mem.eql(u8, params[0].name, "self");
            if (needs_self) {
                const impl_type = self.current_impl_type.?;
                // Create *TypeName type expression: pointer to named type
                const named_type = try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .named = impl_type }, .span = Span.init(start, start) } });
                const ptr_type = try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .pointer = named_type }, .span = Span.init(start, start) } });
                // Prepend self param
                var new_params = try self.allocator.alloc(ast.Field, params.len + 1);
                new_params[0] = .{ .name = "self", .type_expr = ptr_type, .default_value = null_node, .span = Span.init(start, start) };
                @memcpy(new_params[1..], params);
                params = new_params;
            }
        }

        var return_type: NodeIndex = null_node;
        if (!self.check(.lbrace) and !self.check(.semicolon) and !self.check(.eof) and !self.check(.kw_where))
            return_type = try self.parseType() orelse null_node;

        // Rust: `fn sort(T)(list: *List(T)) void where T: Ord`
        // Go 1.18: constraints inline `[T comparable]`. We use Rust's where clause.
        var type_param_bounds: []const ?[]const u8 = &.{};
        if (self.check(.kw_where) and type_params.len > 0) {
            var bounds = try self.allocator.alloc(?[]const u8, type_params.len);
            for (bounds) |*b| b.* = null;
            self.advance(); // consume 'where'
            while (self.check(.ident)) {
                const param_name = self.tok.text;
                self.advance();
                if (!self.expect(.colon)) break;
                if (!self.check(.ident)) { self.syntaxError("expected trait name after ':'"); break; }
                const trait_name = self.tok.text;
                self.advance();
                // Find matching type param and set its bound
                for (type_params, 0..) |tp, i| {
                    if (std.mem.eql(u8, tp, param_name)) {
                        bounds[i] = trait_name;
                        break;
                    }
                }
                if (!self.match(.comma)) break;
            }
            type_param_bounds = bounds;
        }

        var body: NodeIndex = null_node;
        if (is_extern) {
            if (self.check(.lbrace)) { self.syntaxError("extern functions cannot have a body"); return null; }
            if (!self.expect(.semicolon)) return null;
        } else if (self.check(.lbrace)) {
            body = try self.parseBlock() orelse return null;
        }

        return try self.tree.addDecl(.{ .fn_decl = .{ .name = name, .type_params = type_params, .type_param_bounds = type_param_bounds, .params = params, .return_type = return_type, .body = body, .is_extern = is_extern, .is_async = is_async, .span = Span.init(start, self.pos()) } });
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

        // Parse optional type parameters: impl List(T) { ... }
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

        // impl Trait for Type { ... } — trait implementation
        if (self.check(.kw_for)) return self.parseImplTraitBlock(start, type_name, type_params);

        if (!self.expect(.lbrace)) return null;

        // Save/restore impl context for implicit self injection (@safe mode)
        const saved_impl_type = self.current_impl_type;
        const saved_impl_is_generic = self.current_impl_is_generic;
        self.current_impl_type = type_name;
        self.current_impl_is_generic = (type_params.len > 0);
        defer {
            self.current_impl_type = saved_impl_type;
            self.current_impl_is_generic = saved_impl_is_generic;
        }

        var methods = std.ArrayListUnmanaged(NodeIndex){};
        defer methods.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.check(.kw_fn)) {
                if (try self.parseFnDecl(false, false)) |idx| try methods.append(self.allocator, idx);
            } else { self.syntaxError("expected 'fn' in impl block"); self.advance(); }
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .impl_block = .{ .type_name = type_name, .type_params = type_params, .methods = try self.allocator.dupe(NodeIndex, methods.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseTraitDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance(); // consume 'trait'
        if (!self.check(.ident)) { self.err.errorWithCode(self.pos(), .e203, "expected trait name"); return null; }
        const name = self.tok.text;
        self.advance();
        if (!self.expect(.lbrace)) return null;

        var methods = std.ArrayListUnmanaged(NodeIndex){};
        defer methods.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.check(.kw_fn)) {
                // Parse method signature (fn_decl with body = null_node)
                if (try self.parseFnDecl(false, false)) |idx| try methods.append(self.allocator, idx);
            } else { self.syntaxError("expected 'fn' in trait declaration"); self.advance(); }
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .trait_decl = .{ .name = name, .methods = try self.allocator.dupe(NodeIndex, methods.items), .span = Span.init(start, self.pos()) } });
    }

    fn parseImplTraitBlock(self: *Parser, start: Pos, trait_name: []const u8, type_params: []const []const u8) ParseError!?NodeIndex {
        self.advance(); // consume 'for'
        // Target type: identifier or type keyword (i64, i32, etc.)
        var target_type: []const u8 = "";
        if (self.check(.ident)) {
            target_type = self.tok.text;
            self.advance();
        } else if (self.tok.tok.isTypeKeyword()) {
            target_type = self.tok.tok.string();
            self.advance();
        } else {
            self.syntaxError("expected type name after 'for'");
            return null;
        }
        if (!self.expect(.lbrace)) return null;

        // Save/restore impl context for implicit self injection (@safe mode)
        const saved_impl_type = self.current_impl_type;
        const saved_impl_is_generic = self.current_impl_is_generic;
        self.current_impl_type = target_type;
        self.current_impl_is_generic = (type_params.len > 0);
        defer {
            self.current_impl_type = saved_impl_type;
            self.current_impl_is_generic = saved_impl_is_generic;
        }

        var methods = std.ArrayListUnmanaged(NodeIndex){};
        defer methods.deinit(self.allocator);
        while (!self.check(.rbrace) and !self.check(.eof)) {
            if (self.check(.kw_fn)) {
                if (try self.parseFnDecl(false, false)) |idx| try methods.append(self.allocator, idx);
            } else { self.syntaxError("expected 'fn' in impl block"); self.advance(); }
        }
        if (!self.expect(.rbrace)) return null;
        return try self.tree.addDecl(.{ .impl_trait = .{ .trait_name = trait_name, .target_type = target_type, .type_params = type_params, .methods = try self.allocator.dupe(NodeIndex, methods.items), .span = Span.init(start, self.pos()) } });
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

    fn parseBenchDecl(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.check(.string_lit)) { self.syntaxError("expected bench name string"); return null; }
        const raw = self.tok.text;
        self.advance();
        const name = if (raw.len >= 2 and raw[0] == '"' and raw[raw.len - 1] == '"') raw[1 .. raw.len - 1] else raw;
        const body = try self.parseBlock() orelse return null;
        return try self.tree.addDecl(.{ .bench_decl = .{ .name = name, .body = body, .span = Span.init(start, self.pos()) } });
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

        // Tuple type: (T1, T2, ...) — disambiguated from grouping by comma
        if (self.check(.lparen)) {
            self.advance();
            const first = try self.parseType() orelse return null;
            if (self.match(.comma)) {
                // It's a tuple type
                var elems = std.ArrayListUnmanaged(NodeIndex){};
                defer elems.deinit(self.allocator);
                try elems.append(self.allocator, first);
                while (!self.check(.rparen) and !self.check(.eof)) {
                    const elem = try self.parseType() orelse break;
                    try elems.append(self.allocator, elem);
                    if (!self.match(.comma)) break;
                }
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .type_expr = .{ .kind = .{ .tuple = try self.allocator.dupe(NodeIndex, elems.items) }, .span = Span.init(start, self.pos()) } });
            }
            // Single type in parens — not a tuple, just grouping
            if (!self.expect(.rparen)) return null;
            return first;
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
                // Zig pattern: try is prefix unary, can nest other unary ops (try await ...)
                const operand = try self.parseUnaryExpr() orelse return null;
                return try self.tree.addExpr(.{ .try_expr = .{ .operand = operand, .span = Span.init(start, self.pos()) } });
            },
            .kw_await => {
                self.advance();
                // Allow nesting: await can wrap calls and other unary exprs
                const operand = try self.parseUnaryExpr() orelse return null;
                return try self.tree.addExpr(.{ .await_expr = .{ .operand = operand, .span = Span.init(start, self.pos()) } });
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
                } else if (self.check(.int_lit)) {
                    // Tuple index: t.0, t.1, etc.
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
                        // Generic struct literal: List(i64) { .items = 0, ... }
                        // Detected when expr is call(UppercaseIdent, args) followed by { .
                        if (e == .call and (self.peekNextIsPeriod() or (self.safe_mode and self.peekNextIsIdentColon()))) {
                            const use_colon = self.safe_mode and self.peekNextIsIdentColon();
                            const call_node = e.call;
                            if (self.tree.getNode(call_node.callee)) |callee_node| {
                                if (callee_node.asExpr()) |callee_expr| {
                                    if (callee_expr == .ident) {
                                        const callee_name = callee_expr.ident.name;
                                        if (callee_name.len > 0 and std.ascii.isUpper(callee_name[0])) {
                                            const s = self.tree.getNode(expr).?.span();
                                            self.advance(); // consume {
                                            const init_fields = if (use_colon) try self.parseColonFields() else blk: {
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
                                                break :blk try self.allocator.dupe(ast.FieldInit, fields.items);
                                            };
                                            if (!self.expect(.rbrace)) return null;
                                            expr = try self.tree.addExpr(.{ .struct_init = .{
                                                .type_name = callee_name,
                                                .type_args = call_node.args,
                                                .fields = init_fields,
                                                .span = Span.init(s.start, self.pos()),
                                            } });
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
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
                            // Unified init syntax (@safe only): `Point { x: 10, y: 20 }` (colon syntax, with shorthand support)
                            if (self.safe_mode and type_name.len > 0 and std.ascii.isUpper(type_name[0]) and self.peekNextIsIdentColon()) {
                                const s = self.tree.getNode(expr).?.span();
                                self.advance(); // consume {
                                const col_fields = try self.parseColonFields();
                                if (!self.expect(.rbrace)) return null;
                                expr = try self.tree.addExpr(.{ .struct_init = .{ .type_name = type_name, .fields = col_fields, .span = Span.init(s.start, self.pos()) } });
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
            .string_interp_start => return try self.parseStringInterp(),
            .char_lit => { const v = self.tok.text; self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .char, .value = v, .span = Span.init(start, self.pos()) } }); },
            .kw_true => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .true_lit, .value = "true", .span = Span.init(start, self.pos()) } }); },
            .kw_false => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .false_lit, .value = "false", .span = Span.init(start, self.pos()) } }); },
            .kw_null => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .null_lit, .value = "null", .span = Span.init(start, self.pos()) } }); },
            .kw_undefined => { self.advance(); return try self.tree.addExpr(.{ .literal = .{ .kind = .undefined_lit, .value = "undefined", .span = Span.init(start, self.pos()) } }); },
            .lparen => {
                self.advance();
                const inner = try self.parseExpr() orelse return null;
                // Tuple literal: (expr, expr, ...) — disambiguated from paren grouping by comma
                if (self.match(.comma)) {
                    var elems = std.ArrayListUnmanaged(NodeIndex){};
                    defer elems.deinit(self.allocator);
                    try elems.append(self.allocator, inner);
                    while (!self.check(.rparen) and !self.check(.eof)) {
                        const elem = try self.parseExpr() orelse break;
                        try elems.append(self.allocator, elem);
                        if (!self.match(.comma)) break;
                    }
                    if (!self.expect(.rparen)) return null;
                    return try self.tree.addExpr(.{ .tuple_literal = .{ .elements = try self.allocator.dupe(NodeIndex, elems.items), .span = Span.init(start, self.pos()) } });
                }
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
            .kw_comptime => {
                // comptime { ... } — compile-time evaluated block expression
                self.advance();
                if (!self.check(.lbrace)) { self.syntaxError("expected '{' after 'comptime'"); return null; }
                const body = try self.parseBlockExpr() orelse return null;
                return try self.tree.addExpr(.{ .comptime_block = .{ .body = body, .span = Span.init(start, self.pos()) } });
            },
            .kw_new => {
                // new Type { field: value, ... }
                // new List(i64) { field: value, ... } (generic)
                // Reference: Go's walkNew (walk/builtin.go:601-616)
                self.advance();
                if (!self.check(.ident)) {
                    self.err.errorWithCode(self.pos(), .e202, "expected type name after 'new'");
                    return null;
                }
                const type_name = self.tok.text;
                self.advance();
                // Parse optional generic type args: new List(i64) { ... }
                // In @safe mode, also supports constructor args: new Point(10, 20)
                var type_args: []const NodeIndex = &.{};
                var constructor_args: []const NodeIndex = &.{};
                var is_constructor = false;
                if (self.check(.lparen)) {
                    if (self.safe_mode) {
                        // @safe mode: disambiguate type args vs constructor args
                        // Type args start with type-like tokens (uppercase ident, type keyword, *, ?, [)
                        // Constructor args start with value-like tokens (int, string, lowercase ident, etc.)
                        const peek = self.peekToken();
                        const is_type_arg = peek.tok == .mul or peek.tok == .question or peek.tok == .lbrack or
                            peek.tok.isTypeKeyword() or
                            (peek.tok == .ident and peek.text.len > 0 and std.ascii.isUpper(peek.text[0]));
                        if (is_type_arg) {
                            self.advance();
                            var ta = std.ArrayListUnmanaged(NodeIndex){};
                            defer ta.deinit(self.allocator);
                            while (!self.check(.rparen) and !self.check(.eof)) {
                                const arg = try self.parseType() orelse break;
                                try ta.append(self.allocator, arg);
                                if (!self.match(.comma)) break;
                            }
                            if (!self.expect(.rparen)) return null;
                            type_args = try self.allocator.dupe(NodeIndex, ta.items);
                            // After type args, check for constructor args: new List(i64)(16)
                            if (self.check(.lparen)) {
                                self.advance();
                                var ca = std.ArrayListUnmanaged(NodeIndex){};
                                defer ca.deinit(self.allocator);
                                while (!self.check(.rparen) and !self.check(.eof)) {
                                    const arg = try self.parseExpr() orelse break;
                                    try ca.append(self.allocator, arg);
                                    if (!self.match(.comma)) break;
                                }
                                if (!self.expect(.rparen)) return null;
                                constructor_args = try self.allocator.dupe(NodeIndex, ca.items);
                                is_constructor = true;
                            }
                        } else {
                            // Constructor args: new Point(10, 20)
                            self.advance();
                            var ca = std.ArrayListUnmanaged(NodeIndex){};
                            defer ca.deinit(self.allocator);
                            while (!self.check(.rparen) and !self.check(.eof)) {
                                const arg = try self.parseExpr() orelse break;
                                try ca.append(self.allocator, arg);
                                if (!self.match(.comma)) break;
                            }
                            if (!self.expect(.rparen)) return null;
                            constructor_args = try self.allocator.dupe(NodeIndex, ca.items);
                            is_constructor = true;
                        }
                    } else {
                        // Non-safe mode: ( always means type args (original behavior)
                        self.advance();
                        var ta = std.ArrayListUnmanaged(NodeIndex){};
                        defer ta.deinit(self.allocator);
                        while (!self.check(.rparen) and !self.check(.eof)) {
                            const arg = try self.parseType() orelse break;
                            try ta.append(self.allocator, arg);
                            if (!self.match(.comma)) break;
                        }
                        if (!self.expect(.rparen)) return null;
                        type_args = try self.allocator.dupe(NodeIndex, ta.items);
                    }
                }
                // Constructor syntax: `new Point(10, 20)` — no field block needed
                if (is_constructor and !self.check(.lbrace)) {
                    return try self.tree.addExpr(.{ .new_expr = .{
                        .type_name = type_name,
                        .type_args = type_args,
                        .fields = &.{},
                        .constructor_args = constructor_args,
                        .is_constructor = true,
                        .span = Span.init(start, self.pos()),
                    } });
                }
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
                    // Field init shorthand (@safe only): `new Point { x, y }` → `new Point { x: x, y: y }` (ES6/TS pattern)
                    const fval = if (self.match(.colon))
                        try self.parseExpr() orelse return null
                    else if (self.safe_mode)
                        try self.tree.addExpr(.{ .ident = .{ .name = fname, .span = Span.init(fstart, self.pos()) } })
                    else {
                        if (!self.expect(.colon)) return null;
                        return null;
                    };
                    try fields.append(self.allocator, .{ .name = fname, .value = fval, .span = Span.init(fstart, self.pos()) });
                    if (!self.check(.rbrace) and !self.expect(.comma)) return null;
                }
                if (!self.expect(.rbrace)) return null;
                return try self.tree.addExpr(.{ .new_expr = .{
                    .type_name = type_name,
                    .type_args = type_args,
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
                // Check for .{} zero init
                if (self.peekToken().tok == .lbrace) {
                    self.advance(); // consume .
                    self.advance(); // consume {
                    if (!self.expect(.rbrace)) return null;
                    return try self.tree.addExpr(.{ .zero_init = .{ .span = Span.init(start, self.pos()) } });
                }
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

    /// Parse string interpolation: "text${expr}text${expr}text"
    /// Scanner produces: string_interp_start, <expr tokens>, string_interp_mid|end, ...
    /// Token text format:
    ///   start: "text${     (includes leading " and trailing ${)
    ///   mid:   }text${     (includes leading } and trailing ${)
    ///   end:   }text"      (includes leading } and trailing ")
    fn parseStringInterp(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        var segments = std.ArrayListUnmanaged(ast.StringSegment){};
        defer segments.deinit(self.allocator);

        // First token is string_interp_start: "text${
        const start_text = self.tok.text;
        // Strip leading " and trailing ${
        if (start_text.len >= 3) {
            const text_part = start_text[1 .. start_text.len - 2]; // skip " prefix and ${ suffix
            if (text_part.len > 0)
                try segments.append(self.allocator, .{ .text = text_part });
        }
        self.advance(); // consume string_interp_start

        // Now parse the expression
        const expr = try self.parseExpr() orelse return null;
        try segments.append(self.allocator, .{ .expr = expr });

        // Loop: next token is either string_interp_mid or string_interp_end
        while (true) {
            if (self.check(.string_interp_mid)) {
                // mid token: }text${
                const mid_text = self.tok.text;
                if (mid_text.len >= 3) {
                    const text_part = mid_text[1 .. mid_text.len - 2]; // skip } prefix and ${ suffix
                    if (text_part.len > 0)
                        try segments.append(self.allocator, .{ .text = text_part });
                }
                self.advance(); // consume string_interp_mid

                // Parse the next expression
                const next_expr = try self.parseExpr() orelse return null;
                try segments.append(self.allocator, .{ .expr = next_expr });
            } else if (self.check(.string_interp_end)) {
                // end token: }text"
                const end_text = self.tok.text;
                if (end_text.len >= 2) {
                    const text_part = end_text[1 .. end_text.len - 1]; // skip } prefix and " suffix
                    if (text_part.len > 0)
                        try segments.append(self.allocator, .{ .text = text_part });
                }
                self.advance(); // consume string_interp_end
                break;
            } else {
                self.syntaxError("expected string interpolation continuation");
                return null;
            }
        }

        return try self.tree.addExpr(.{ .string_interp = .{
            .segments = try self.allocator.dupe(ast.StringSegment, segments.items),
            .span = Span.init(start, self.pos()),
        } });
    }

    fn parseBuiltinCall(self: *Parser, start: Pos) ParseError!?NodeIndex {
        self.advance(); // @
        if (!self.check(.ident) and !self.check(.kw_string)) { self.syntaxError("expected builtin name after '@'"); return null; }
        const name_str = if (self.check(.kw_string)) "string" else self.tok.text;
        self.advance();
        if (!self.expect(.lparen)) return null;

        const kind = ast.BuiltinKind.fromString(name_str) orelse {
            self.syntaxError("unknown builtin");
            return null;
        };

        switch (kind) {
            // 0 args
            .trap, .time, .args_count, .environ_count, .target_os, .target_arch, .target,
            .kqueue_create, .epoll_create,
            => {
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = null_node, .args = .{ null_node, null_node, null_node }, .span = Span.init(start, self.pos()) } });
            },
            // 1 type arg (no value)
            .size_of, .align_of => {
                const t = try self.parseType() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = t, .args = .{ null_node, null_node, null_node }, .span = Span.init(start, self.pos()) } });
            },
            // 1 type + 1 value arg
            .int_cast, .ptr_cast, .int_to_ptr => {
                const t = try self.parseType() orelse return null;
                if (!self.expect(.comma)) return null;
                const v = try self.parseExpr() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = t, .args = .{ v, null_node, null_node }, .span = Span.init(start, self.pos()) } });
            },
            // 1 value arg
            .ptr_to_int, .assert, .alloc, .dealloc, .ptr_of, .len_of,
            .fd_close, .exit, .arg_len, .arg_ptr, .environ_len, .environ_ptr,
            .compile_error,
            .abs, .ceil, .floor, .trunc, .round, .sqrt,
            .net_accept, .net_set_reuse_addr,
            .set_nonblocking,
            => {
                const arg = try self.parseExpr() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = null_node, .args = .{ arg, null_node, null_node }, .span = Span.init(start, self.pos()) } });
            },
            // 2 value args
            .string, .assert_eq, .realloc, .random, .fmin, .fmax, .net_listen,
            .epoll_del,
            => {
                const a1 = try self.parseExpr() orelse return null;
                if (!self.expect(.comma)) return null;
                const a2 = try self.parseExpr() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = null_node, .args = .{ a1, a2, null_node }, .span = Span.init(start, self.pos()) } });
            },
            // 3 value args
            .memcpy, .fd_write, .fd_read, .fd_seek, .fd_open,
            .net_socket, .net_bind, .net_connect,
            .kevent_add, .kevent_del, .kevent_wait,
            .epoll_add, .epoll_wait,
            => {
                const a1 = try self.parseExpr() orelse return null;
                if (!self.expect(.comma)) return null;
                const a2 = try self.parseExpr() orelse return null;
                if (!self.expect(.comma)) return null;
                const a3 = try self.parseExpr() orelse return null;
                if (!self.expect(.rparen)) return null;
                return try self.tree.addExpr(.{ .builtin_call = .{ .kind = kind, .type_arg = null_node, .args = .{ a1, a2, a3 }, .span = Span.init(start, self.pos()) } });
            },
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
        // Rust/Zig pattern: last expression in a block is the block's value.
        // If the last item is an expr_stmt, extract it as the block's result expr.
        var result_expr: NodeIndex = null_node;
        if (stmts.items.len > 0) {
            const last = stmts.items[stmts.items.len - 1];
            if (self.tree.getNode(last)) |node| {
                if (node.asStmt()) |stmt| {
                    if (stmt == .expr_stmt) {
                        result_expr = stmt.expr_stmt.expr;
                        stmts.items.len -= 1;
                    }
                }
            }
        }
        return try self.tree.addExpr(.{ .block_expr = .{ .stmts = try self.allocator.dupe(NodeIndex, stmts.items), .expr = result_expr, .span = Span.init(start, self.pos()) } });
    }

    /// Zig pattern: if (expr) |val| { ... }
    /// Parentheses required around condition. `)` terminates expr, so `|val|` is unambiguous.
    fn parseIfExpr(self: *Parser) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        if (!self.expect(.lparen)) return null;
        const cond = try self.parseExpr() orelse return null;
        if (!self.expect(.rparen)) return null;
        // Optional capture: |val| (Zig PtrPayload)
        var capture: []const u8 = "";
        if (self.match(.@"or")) {
            if (self.check(.ident)) { capture = self.tok.text; self.advance(); } else { self.syntaxError("expected identifier for optional capture"); return null; }
            if (!self.expect(.@"or")) return null;
        }
        if (!self.check(.lbrace)) { self.err.errorWithCode(self.pos(), .e204, "expected '{' after if condition"); return null; }
        const then_br = try self.parseBlockExpr() orelse return null;
        var else_br: NodeIndex = null_node;
        if (self.match(.kw_else)) {
            if (self.check(.kw_if)) else_br = try self.parseIfExpr() orelse return null
            else if (self.check(.lbrace)) else_br = try self.parseBlockExpr() orelse return null
            else { self.syntaxError("expected '{' or 'if' after 'else'"); return null; }
        }
        return try self.tree.addExpr(.{ .if_expr = .{ .condition = cond, .then_branch = then_br, .else_branch = else_br, .capture = capture, .span = Span.init(start, self.pos()) } });
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

            // Wildcard `_` → treat as else branch (Rust/Zig convention)
            if (self.check(.ident) and std.mem.eql(u8, self.tok.text, "_")) {
                self.advance();
                if (!self.expect(.fat_arrow)) return null;
                else_body = try self.parseExpr() orelse return null;
                _ = self.match(.comma);
                continue;
            }

            var patterns = std.ArrayListUnmanaged(NodeIndex){};
            defer patterns.deinit(self.allocator);
            var is_range = false;
            const first = try self.parsePrimaryExpr() orelse return null;
            try patterns.append(self.allocator, first);

            // Range pattern: `1..5` → inclusive range (Zig: 1...5, Rust: 1..=5)
            if (self.check(.period_period)) {
                self.advance();
                const range_end = try self.parsePrimaryExpr() orelse return null;
                try patterns.append(self.allocator, range_end);
                is_range = true;
            } else {
                while (self.check(.comma) and !self.check(.fat_arrow)) {
                    self.advance();
                    if (self.check(.fat_arrow) or self.check(.kw_else)) break;
                    const p = try self.parsePrimaryExpr() orelse return null;
                    try patterns.append(self.allocator, p);
                }
            }

            var capture: []const u8 = "";
            if (self.match(.@"or")) {
                if (self.check(.ident)) { capture = self.tok.text; self.advance(); } else { self.syntaxError("expected identifier for payload capture"); return null; }
                if (!self.expect(.@"or")) return null;
            }

            // Guard: `pattern if expr =>` (Rust match guard syntax)
            var guard: NodeIndex = null_node;
            if (self.check(.kw_if)) {
                self.advance();
                guard = try self.parseExpr() orelse return null;
            }

            if (!self.expect(.fat_arrow)) return null;
            const body = try self.parseExpr() orelse return null;
            try cases.append(self.allocator, .{ .patterns = try self.allocator.dupe(NodeIndex, patterns.items), .capture = capture, .guard = guard, .is_range = is_range, .body = body, .span = Span.init(case_start, self.pos()) });
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
            .kw_for => return self.parseForStmt(null),
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
            .kw_defer, .kw_errdefer => {
                const is_errdefer = self.tok.tok == .kw_errdefer;
                self.advance();
                // defer/errdefer can take a block { ... } or a simple expression/assignment
                if (self.check(.lbrace)) {
                    const block = try self.parseBlock() orelse return null;
                    return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = block, .is_errdefer = is_errdefer, .span = Span.init(start, self.pos()) } });
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
                    return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = assign_node, .is_errdefer = is_errdefer, .span = Span.init(start, self.pos()) } });
                }
                _ = self.match(.semicolon);
                return try self.tree.addStmt(.{ .defer_stmt = .{ .expr = e, .is_errdefer = is_errdefer, .span = Span.init(start, self.pos()) } });
            },
            .ident => {
                if (self.peekToken().tok == .colon) {
                    const label = self.tok.text;
                    self.advance();
                    self.advance();
                    if (self.check(.kw_while)) return self.parseWhileStmt(label);
                    if (self.check(.kw_for)) return self.parseForStmt(label);
                    self.err.errorWithCode(self.pos(), .e201, "expected 'while' or 'for' after label");
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
        // Zig pattern: if (expr) |val| { ... }
        if (!self.expect(.lparen)) return null;
        const cond = try self.parseExpr() orelse return null;
        if (!self.expect(.rparen)) return null;
        // Optional capture: |val| (Zig PtrPayload)
        var capture: []const u8 = "";
        if (self.match(.@"or")) {
            if (self.check(.ident)) { capture = self.tok.text; self.advance(); } else { self.syntaxError("expected identifier for optional capture"); return null; }
            if (!self.expect(.@"or")) return null;
        }
        const then_br = if (self.check(.lbrace)) try self.parseBlock() orelse return null else try self.parseStmt() orelse return null;
        var else_br: NodeIndex = null_node;
        if (self.match(.kw_else)) {
            if (self.check(.kw_if)) else_br = try self.parseIfStmt() orelse return null
            else if (self.check(.lbrace)) else_br = try self.parseBlock() orelse return null
            else else_br = try self.parseStmt() orelse return null;
        }
        return try self.tree.addStmt(.{ .if_stmt = .{ .condition = cond, .then_branch = then_br, .else_branch = else_br, .capture = capture, .span = Span.init(start, self.pos()) } });
    }

    fn parseWhileStmt(self: *Parser, label: ?[]const u8) ParseError!?NodeIndex {
        const start = self.pos();
        self.advance();
        // Zig pattern: while (expr) { ... }
        if (!self.expect(.lparen)) return null;
        const cond = try self.parseExpr() orelse return null;
        if (!self.expect(.rparen)) return null;
        if (!self.check(.lbrace)) { self.err.errorWithCode(self.pos(), .e204, "expected '{' after while condition"); return null; }
        const body = try self.parseBlock() orelse return null;
        return try self.tree.addStmt(.{ .while_stmt = .{ .condition = cond, .body = body, .label = label, .span = Span.init(start, self.pos()) } });
    }

    fn parseForStmt(self: *Parser, label: ?[]const u8) ParseError!?NodeIndex {
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
            .label = label,
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
    _, const err, var arena = try testParse("fn foo() { if (x == 1) { return 1 } else { return 2 } }");
    defer arena.deinit();
    try std.testing.expect(!err.hasErrors());
}

test "parser while loop" {
    _, const err, var arena = try testParse("fn foo() { var i = 0; while (i < 10) { i = i + 1 } }");
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
