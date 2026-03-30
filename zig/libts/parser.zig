//! TypeScript/JavaScript parser.
//!
//! Ported from references/typescript-go/internal/parser/parser.go.
//! Produces TS AST nodes (ts_ast.zig), which are later transformed to Cot AST.
//!
//! Design: Recursive descent with precedence climbing for binary expressions.
//! Same patterns as zig/libcot/frontend/parser.zig:
//!   - Parser struct with scanner, current token, allocator
//!   - parseXxx() methods return NodeIndex
//!   - Error recovery via synchronization tokens

const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const TokenInfo = @import("scanner.zig").TokenInfo;
const Token = @import("token.zig").Token;
const ts_ast = @import("ts_ast.zig");
const source = @import("source.zig");

const NodeIndex = ts_ast.NodeIndex;
const null_node = ts_ast.null_node;
const Ast = ts_ast.Ast;
const Node = ts_ast.Node;
const Decl = ts_ast.Decl;
const Expr = ts_ast.Expr;
const Stmt = ts_ast.Stmt;
const TypeNode = ts_ast.TypeNode;
const ClassMember = ts_ast.ClassMember;
const Pattern = ts_ast.Pattern;
const Param = ts_ast.Param;
const Span = source.Span;
const Pos = source.Pos;
const Source = source.Source;

pub const Parser = struct {
    scanner: Scanner,
    ast: Ast,
    tok: TokenInfo,
    prev_end: Pos,
    allocator: std.mem.Allocator, // arena-backed: all AST allocations freed in one shot
    arena: std.heap.ArenaAllocator,

    pub fn init(src: *Source, backing_allocator: std.mem.Allocator) Parser {
        var scanner = Scanner.init(src);
        const first_tok = scanner.next();
        return .{
            .scanner = scanner,
            .ast = undefined, // initialized in ensureInit
            .tok = first_tok,
            .prev_end = Pos.zero,
            .allocator = undefined, // initialized in ensureInit
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    /// Must be called after the Parser is at its final memory location.
    /// ArenaAllocator cannot be moved after init (self-referential pointers).
    pub fn ensureInit(self: *Parser) void {
        self.allocator = self.arena.allocator();
        self.ast = Ast.init(self.allocator);
    }

    pub fn deinit(self: *Parser) void {
        // All AST allocations go through the arena — free everything in one shot.
        // No individual frees needed (matches TS-Go's pool pattern).
        self.arena.deinit();
    }

    // ================================================================
    // Token helpers
    // ================================================================

    fn advance(self: *Parser) void {
        self.prev_end = self.tok.span.end;
        self.tok = self.scanner.next();
    }

    fn at(self: *const Parser) Token {
        return self.tok.tok;
    }

    fn expect(self: *Parser, t: Token) void {
        if (self.at() == t) {
            self.advance();
        }
        // In a production parser we'd report an error here.
        // For now, silently continue (error recovery).
    }

    fn eat(self: *Parser, t: Token) bool {
        if (self.at() == t) {
            self.advance();
            return true;
        }
        return false;
    }

    fn span(self: *const Parser, start: Pos) Span {
        return Span.init(start, self.prev_end);
    }

    fn currentSpan(self: *const Parser) Span {
        return self.tok.span;
    }

    fn tokenText(self: *const Parser) []const u8 {
        return self.tok.text;
    }

    // ================================================================
    // Top-level: parse source file
    // ================================================================

    pub fn parseSourceFile(self: *Parser) !void {
        self.ensureInit();
        const start = self.tok.span.start;
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};

        while (self.at() != .eof) {
            const stmt = try self.parseStatementOrDeclaration();
            if (stmt != null_node) {
                try node_buf.append(self.allocator, stmt);
            }
        }

        const stmts = try self.allocNodeList(node_buf.items);
        self.ast.source_file = .{
            .filename = self.scanner.src.filename,
            .statements = stmts,
            .span = self.span(start),
        };
    }

    // ================================================================
    // Statements and declarations
    // ================================================================

    fn parseStatementOrDeclaration(self: *Parser) error{OutOfMemory}!NodeIndex {
        return switch (self.at()) {
            .kw_function => self.parseFunctionDeclaration(false, false),
            .kw_class => self.parseClassDeclaration(false, false),
            .kw_interface => self.parseInterfaceDeclaration(false),
            .kw_type => self.parseTypeAliasDeclaration(false),
            .kw_enum => self.parseEnumDeclaration(false, false),
            .kw_var, .kw_let, .kw_const => self.parseVariableStatement(false),
            .kw_import => self.parseImportDeclaration(),
            .kw_export => self.parseExportDeclaration(),
            .kw_if => self.parseIfStatement(),
            .kw_while => self.parseWhileStatement(),
            .kw_do => self.parseDoWhileStatement(),
            .kw_for => self.parseForStatement(),
            .kw_return => self.parseReturnStatement(),
            .kw_throw => self.parseThrowStatement(),
            .kw_try => self.parseTryStatement(),
            .kw_switch => self.parseSwitchStatement(),
            .kw_break => self.parseBreakStatement(),
            .kw_continue => self.parseContinueStatement(),
            .kw_debugger => self.parseDebuggerStatement(),
            .kw_with => self.parseWithStatement(),
            .lbrace => self.parseBlockStatement(),
            .semicolon => self.parseEmptyStatement(),
            .kw_async => blk: {
                // async function or async arrow — look ahead
                if (self.peekToken() == .kw_function) {
                    break :blk self.parseAsyncFunctionDeclaration(false);
                }
                break :blk self.parseExpressionStatement();
            },
            .kw_abstract => blk: {
                if (self.peekToken() == .kw_class) {
                    break :blk self.parseAbstractClassDeclaration(false);
                }
                break :blk self.parseExpressionStatement();
            },
            .kw_declare => self.parseDeclareStatement(),
            .kw_namespace, .kw_module => self.parseNamespaceDeclaration(false),
            else => self.parseExpressionOrLabeledStatement(),
        };
    }

    fn parseExpressionOrLabeledStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        const expr = try self.parseExpression();

        // Check for labeled statement: ident ":" stmt
        if (self.at() == .colon) {
            if (self.ast.getNode(expr)) |node| {
                switch (node.*) {
                    .expr => |e| switch (e) {
                        .ident => |id| {
                            self.advance(); // consume ':'
                            const body = try self.parseStatementOrDeclaration();
                            return self.ast.addStmt(.{ .labeled = .{
                                .label = id.name,
                                .body = body,
                                .span = self.span(start),
                            } });
                        },
                        else => {},
                    },
                    else => {},
                }
            }
        }

        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .expr_stmt = .{
            .expr = expr,
            .span = self.span(start),
        } });
    }

    fn parseExpressionStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        const expr = try self.parseExpression();
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .expr_stmt = .{
            .expr = expr,
            .span = self.span(start),
        } });
    }

    fn parseBlockStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.expect(.lbrace);

        var stmts_buf = std.ArrayListUnmanaged(NodeIndex){};

        while (self.at() != .rbrace and self.at() != .eof) {
            const stmt = try self.parseStatementOrDeclaration();
            if (stmt != null_node) {
                try stmts_buf.append(self.allocator, stmt);
            }
        }

        self.expect(.rbrace);
        const stmts = try self.allocNodeList(stmts_buf.items);
        return self.ast.addStmt(.{ .block = .{
            .stmts = stmts,
            .span = self.span(start),
        } });
    }

    fn parseEmptyStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume ;
        return self.ast.addStmt(.{ .empty = .{ .span = self.span(start) } });
    }

    fn parseReturnStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'return'
        var value: NodeIndex = null_node;
        if (self.at() != .semicolon and self.at() != .rbrace and self.at() != .eof) {
            value = try self.parseExpression();
        }
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .return_stmt = .{
            .value = value,
            .span = self.span(start),
        } });
    }

    fn parseIfStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'if'
        self.expect(.lparen);
        const condition = try self.parseExpression();
        self.expect(.rparen);
        const consequent = try self.parseStatementOrDeclaration();
        var alternate: NodeIndex = null_node;
        if (self.eat(.kw_else)) {
            alternate = try self.parseStatementOrDeclaration();
        }
        return self.ast.addStmt(.{ .if_stmt = .{
            .condition = condition,
            .consequent = consequent,
            .alternate = alternate,
            .span = self.span(start),
        } });
    }

    fn parseWhileStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'while'
        self.expect(.lparen);
        const condition = try self.parseExpression();
        self.expect(.rparen);
        const body = try self.parseStatementOrDeclaration();
        return self.ast.addStmt(.{ .while_stmt = .{
            .condition = condition,
            .body = body,
            .span = self.span(start),
        } });
    }

    fn parseDoWhileStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'do'
        const body = try self.parseStatementOrDeclaration();
        self.expect(.kw_while);
        self.expect(.lparen);
        const condition = try self.parseExpression();
        self.expect(.rparen);
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .do_while = .{
            .body = body,
            .condition = condition,
            .span = self.span(start),
        } });
    }

    fn parseForStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'for'
        const is_await = self.eat(.kw_await);
        self.expect(.lparen);

        // Parse init — could be var/let/const or expression
        var init_node: NodeIndex = null_node;
        if (self.at() == .kw_var or self.at() == .kw_let or self.at() == .kw_const) {
            init_node = try self.parseVariableDeclarationForInit();
        } else if (self.at() != .semicolon) {
            init_node = try self.parseExpression();
        }

        // Check for for-in or for-of
        if (self.eat(.kw_in)) {
            const right = try self.parseExpression();
            self.expect(.rparen);
            const body = try self.parseStatementOrDeclaration();
            return self.ast.addStmt(.{ .for_in = .{
                .left = init_node,
                .right = right,
                .body = body,
                .span = self.span(start),
            } });
        }
        if (self.eat(.kw_of)) {
            const right = try self.parseExpression();
            self.expect(.rparen);
            const body = try self.parseStatementOrDeclaration();
            return self.ast.addStmt(.{ .for_of = .{
                .left = init_node,
                .right = right,
                .body = body,
                .is_await = is_await,
                .span = self.span(start),
            } });
        }

        // Regular for(init; cond; update)
        self.expect(.semicolon);
        var condition: NodeIndex = null_node;
        if (self.at() != .semicolon) condition = try self.parseExpression();
        self.expect(.semicolon);
        var update: NodeIndex = null_node;
        if (self.at() != .rparen) update = try self.parseExpression();
        self.expect(.rparen);
        const body = try self.parseStatementOrDeclaration();
        return self.ast.addStmt(.{ .for_stmt = .{
            .init = init_node,
            .condition = condition,
            .update = update,
            .body = body,
            .span = self.span(start),
        } });
    }

    fn parseThrowStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'throw'
        const argument = try self.parseExpression();
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .throw_stmt = .{
            .argument = argument,
            .span = self.span(start),
        } });
    }

    fn parseTryStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'try'
        const block = try self.parseBlockStatement();
        var handler: ?Stmt.CatchClause = null;
        var finalizer: NodeIndex = null_node;

        if (self.eat(.kw_catch)) {
            const catch_start = self.prev_end;
            var param: NodeIndex = null_node;
            var type_ann: NodeIndex = null_node;
            if (self.eat(.lparen)) {
                param = try self.parseBindingPattern();
                if (self.eat(.colon)) {
                    type_ann = try self.parseType();
                }
                self.expect(.rparen);
            }
            const catch_body = try self.parseBlockStatement();
            handler = .{
                .param = param,
                .type_ann = type_ann,
                .body = catch_body,
                .span = Span.init(catch_start, self.prev_end),
            };
        }

        if (self.eat(.kw_finally)) {
            finalizer = try self.parseBlockStatement();
        }

        return self.ast.addStmt(.{ .try_stmt = .{
            .block = block,
            .handler = handler,
            .finalizer = finalizer,
            .span = self.span(start),
        } });
    }

    fn parseSwitchStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'switch'
        self.expect(.lparen);
        const discriminant = try self.parseExpression();
        self.expect(.rparen);
        self.expect(.lbrace);

        var cases = std.ArrayListUnmanaged(Stmt.SwitchCase){};

        while (self.at() != .rbrace and self.at() != .eof) {
            const case_start = self.tok.span.start;
            var test_expr: NodeIndex = null_node;
            if (self.eat(.kw_case)) {
                test_expr = try self.parseExpression();
                self.expect(.colon);
            } else if (self.eat(.kw_default)) {
                self.expect(.colon);
            } else break;

            var case_stmts = std.ArrayListUnmanaged(NodeIndex){};
            while (self.at() != .kw_case and self.at() != .kw_default and
                self.at() != .rbrace and self.at() != .eof)
            {
                const stmt = try self.parseStatementOrDeclaration();
                if (stmt != null_node) try case_stmts.append(self.allocator, stmt);
            }

            try cases.append(self.allocator, .{
                .@"test" = test_expr,
                .consequent = try self.allocNodeList(case_stmts.items),
                .span = self.span(case_start),
            });
        }

        self.expect(.rbrace);
        const owned_cases = try self.allocator.dupe(Stmt.SwitchCase, cases.items);
        return self.ast.addStmt(.{ .switch_stmt = .{
            .discriminant = discriminant,
            .cases = owned_cases,
            .span = self.span(start),
        } });
    }

    fn parseBreakStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance();
        var label: ?[]const u8 = null;
        if (self.at() == .ident and !self.hasLineBreakBefore()) {
            label = self.tokenText();
            self.advance();
        }
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .break_stmt = .{ .label = label, .span = self.span(start) } });
    }

    fn parseContinueStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance();
        var label: ?[]const u8 = null;
        if (self.at() == .ident and !self.hasLineBreakBefore()) {
            label = self.tokenText();
            self.advance();
        }
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .continue_stmt = .{ .label = label, .span = self.span(start) } });
    }

    fn parseDebuggerStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance();
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .debugger = .{ .span = self.span(start) } });
    }

    fn parseWithStatement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'with'
        self.expect(.lparen);
        const object = try self.parseExpression();
        self.expect(.rparen);
        const body = try self.parseStatementOrDeclaration();
        return self.ast.addStmt(.{ .with_stmt = .{
            .object = object,
            .body = body,
            .span = self.span(start),
        } });
    }

    fn parseDeclareStatement(self: *Parser) !NodeIndex {
        self.advance(); // consume 'declare'
        return switch (self.at()) {
            .kw_function => self.parseFunctionDeclaration(false, true),
            .kw_class => self.parseClassDeclaration(false, true),
            .kw_var, .kw_let, .kw_const => self.parseVariableStatement(true),
            .kw_interface => self.parseInterfaceDeclaration(true),
            .kw_type => self.parseTypeAliasDeclaration(true),
            .kw_enum => self.parseEnumDeclaration(false, true),
            .kw_namespace, .kw_module => self.parseNamespaceDeclaration(true),
            .kw_abstract => blk: {
                if (self.peekToken() == .kw_class) {
                    break :blk self.parseAbstractClassDeclaration(true);
                }
                break :blk self.parseExpressionStatement();
            },
            else => self.parseExpressionStatement(),
        };
    }

    // ================================================================
    // Function declarations
    // ================================================================

    fn parseFunctionDeclaration(self: *Parser, is_export: bool, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'function'
        const is_generator = self.eat(.mul);
        const name = if (self.at() == .ident) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;
        const type_params = try self.parseOptionalTypeParameters();
        const params = try self.parseParameterList();
        const return_type = try self.parseOptionalTypeAnnotation();
        const body = if (self.at() == .lbrace) try self.parseBlockStatement() else blk: {
            _ = self.eat(.semicolon);
            break :blk null_node;
        };

        return self.ast.addDecl(.{ .function = .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .body = body,
            .is_generator = is_generator,
            .is_export = is_export,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseAsyncFunctionDeclaration(self: *Parser, is_export: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'async'
        self.advance(); // consume 'function'
        const is_generator = self.eat(.mul);
        const name = if (self.at() == .ident) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;
        const type_params = try self.parseOptionalTypeParameters();
        const params = try self.parseParameterList();
        const return_type = try self.parseOptionalTypeAnnotation();
        const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;

        return self.ast.addDecl(.{ .function = .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .body = body,
            .is_async = true,
            .is_generator = is_generator,
            .is_export = is_export,
            .span = self.span(start),
        } });
    }

    // ================================================================
    // Class declarations
    // ================================================================

    fn parseClassDeclaration(self: *Parser, is_export: bool, is_declare: bool) !NodeIndex {
        return self.parseClassDecl(is_export, is_declare, false);
    }

    fn parseAbstractClassDeclaration(self: *Parser, is_declare: bool) !NodeIndex {
        self.advance(); // consume 'abstract'
        return self.parseClassDecl(false, is_declare, true);
    }

    fn parseClassDecl(self: *Parser, is_export: bool, is_declare: bool, is_abstract: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'class'
        const name = if (self.at() == .ident) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;
        const type_params = try self.parseOptionalTypeParameters();

        // extends
        var extends: NodeIndex = null_node;
        if (self.eat(.kw_extends)) {
            extends = try self.parseLeftHandSideExpression();
            // Optional type arguments on extends
            if (self.at() == .lss) {
                _ = try self.parseTypeArguments();
            }
        }

        // implements
        var implements: ts_ast.NodeList = &.{};
        if (self.eat(.kw_implements)) {
            var node_buf = std.ArrayListUnmanaged(NodeIndex){};
            while (true) {
                const impl_type = try self.parseTypeReference();
                try node_buf.append(self.allocator, impl_type);
                if (!self.eat(.comma)) break;
            }
            implements = try self.allocNodeList(node_buf.items);
        }

        // Body
        self.expect(.lbrace);
        var members_buf = std.ArrayListUnmanaged(NodeIndex){};
        while (self.at() != .rbrace and self.at() != .eof) {
            if (self.eat(.semicolon)) continue;
            const member = try self.parseClassElement();
            if (member != null_node) {
                try members_buf.append(self.allocator, member);
            }
        }
        self.expect(.rbrace);

        const members = try self.allocNodeList(members_buf.items);
        return self.ast.addDecl(.{ .class = .{
            .name = name,
            .type_params = type_params,
            .extends = extends,
            .implements = implements,
            .members = members,
            .is_abstract = is_abstract,
            .is_export = is_export,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseClassElement(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;

        // Parse modifiers
        var is_static = false;
        var is_abstract = false;
        var is_readonly = false;
        var is_override = false;
        var is_async = false;
        var accessibility: ts_ast.Accessibility = .none;

        while (true) {
            switch (self.at()) {
                .kw_static => {
                    is_static = true;
                    self.advance();
                },
                .kw_abstract => {
                    is_abstract = true;
                    self.advance();
                },
                .kw_readonly => {
                    is_readonly = true;
                    self.advance();
                },
                .kw_override => {
                    is_override = true;
                    self.advance();
                },
                .kw_async => {
                    is_async = true;
                    self.advance();
                },
                .kw_public => {
                    accessibility = .public;
                    self.advance();
                },
                .kw_protected => {
                    accessibility = .protected;
                    self.advance();
                },
                .kw_private => {
                    accessibility = .private;
                    self.advance();
                },
                else => break,
            }
        }

        // Getter/setter
        if (self.at() == .kw_get and self.peekIsPropertyName()) {
            self.advance();
            const name = try self.parsePropertyName();
            self.expect(.lparen);
            self.expect(.rparen);
            const return_type = try self.parseOptionalTypeAnnotation();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;
            return self.ast.addClassMember(.{ .getter = .{
                .name = name,
                .return_type = return_type,
                .body = body,
                .is_static = is_static,
                .is_abstract = is_abstract,
                .is_override = is_override,
                .accessibility = accessibility,
                .span = self.span(start),
            } });
        }
        if (self.at() == .kw_set and self.peekIsPropertyName()) {
            self.advance();
            const name = try self.parsePropertyName();
            const params = try self.parseParameterList();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;
            return self.ast.addClassMember(.{ .setter = .{
                .name = name,
                .params = params,
                .body = body,
                .is_static = is_static,
                .is_abstract = is_abstract,
                .is_override = is_override,
                .accessibility = accessibility,
                .span = self.span(start),
            } });
        }

        // Constructor
        if (self.at() == .ident and std.mem.eql(u8, self.tokenText(), "constructor")) {
            self.advance();
            const params = try self.parseParameterList();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;
            return self.ast.addClassMember(.{ .constructor = .{
                .params = params,
                .body = body,
                .accessibility = accessibility,
                .span = self.span(start),
            } });
        }

        // Method or property
        const is_generator = self.eat(.mul);
        const name = try self.parsePropertyName();

        const is_optional = self.eat(.question);

        // Method: has ( or < after name
        if (self.at() == .lparen or self.at() == .lss) {
            const type_params = try self.parseOptionalTypeParameters();
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else blk: {
                _ = self.eat(.semicolon);
                break :blk null_node;
            };
            return self.ast.addClassMember(.{ .method = .{
                .name = name,
                .type_params = type_params,
                .params = params,
                .return_type = return_type,
                .body = body,
                .is_static = is_static,
                .is_async = is_async,
                .is_generator = is_generator,
                .is_abstract = is_abstract,
                .is_optional = is_optional,
                .is_override = is_override,
                .accessibility = accessibility,
                .span = self.span(start),
            } });
        }

        // Property
        const type_ann = try self.parseOptionalTypeAnnotation();
        var init_val: NodeIndex = null_node;
        if (self.eat(.assign)) {
            init_val = try self.parseAssignmentExpression();
        }
        _ = self.eat(.semicolon);

        return self.ast.addClassMember(.{ .property = .{
            .name = name,
            .type_ann = type_ann,
            .init = init_val,
            .is_static = is_static,
            .is_readonly = is_readonly,
            .is_optional = is_optional,
            .is_abstract = is_abstract,
            .is_override = is_override,
            .accessibility = accessibility,
            .span = self.span(start),
        } });
    }

    // ================================================================
    // Interface, type alias, enum declarations
    // ================================================================

    fn parseInterfaceDeclaration(self: *Parser, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'interface'
        const name = self.tokenText();
        self.advance();
        const type_params = try self.parseOptionalTypeParameters();

        var extends: ts_ast.NodeList = &.{};
        if (self.eat(.kw_extends)) {
            var node_buf = std.ArrayListUnmanaged(NodeIndex){};
            while (true) {
                const t = try self.parseTypeReference();
                try node_buf.append(self.allocator, t);
                if (!self.eat(.comma)) break;
            }
            extends = try self.allocNodeList(node_buf.items);
        }

        self.expect(.lbrace);
        var member_buf = std.ArrayListUnmanaged(Decl.InterfaceMember){};
        while (self.at() != .rbrace and self.at() != .eof) {
            const member = try self.parseInterfaceMember();
            try member_buf.append(self.allocator, member);
            // Consume separator
            _ = self.eat(.semicolon) or self.eat(.comma);
        }
        self.expect(.rbrace);

        const members = try self.allocator.dupe(Decl.InterfaceMember, member_buf.items);
        return self.ast.addDecl(.{ .interface = .{
            .name = name,
            .type_params = type_params,
            .extends = extends,
            .members = members,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseInterfaceMember(self: *Parser) !Decl.InterfaceMember {
        const start = self.tok.span.start;
        var is_readonly = false;
        if (self.eat(.kw_readonly)) is_readonly = true;

        // Index signature: [key: type]: type
        if (self.at() == .lbrack) {
            self.advance();
            const param_name = self.tokenText();
            _ = param_name;
            self.advance();
            self.expect(.colon);
            _ = try self.parseType();
            self.expect(.rbrack);
            self.expect(.colon);
            const type_ann = try self.parseType();
            return .{
                .kind = .index_signature,
                .name = null,
                .type_ann = type_ann,
                .is_readonly = is_readonly,
                .span = self.span(start),
            };
        }

        const name = if (self.at() == .ident or self.at() == .string_lit) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;

        const is_optional = self.eat(.question);

        // Method signature: name(...): Type
        if (self.at() == .lparen or self.at() == .lss) {
            const type_params = try self.parseOptionalTypeParameters();
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            return .{
                .kind = .method,
                .name = name,
                .type_params = type_params,
                .params = params,
                .return_type = return_type,
                .is_optional = is_optional,
                .is_readonly = is_readonly,
                .span = self.span(start),
            };
        }

        // Property signature: name: Type
        const type_ann = try self.parseOptionalTypeAnnotation();
        return .{
            .kind = .property,
            .name = name,
            .type_ann = type_ann,
            .is_optional = is_optional,
            .is_readonly = is_readonly,
            .span = self.span(start),
        };
    }

    fn parseTypeAliasDeclaration(self: *Parser, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'type'
        const name = self.tokenText();
        self.advance();
        const type_params = try self.parseOptionalTypeParameters();
        self.expect(.assign);
        const type_node = try self.parseType();
        _ = self.eat(.semicolon);
        return self.ast.addDecl(.{ .type_alias = .{
            .name = name,
            .type_params = type_params,
            .type_node = type_node,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseEnumDeclaration(self: *Parser, is_const: bool, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        if (self.at() == .kw_const) self.advance();
        self.advance(); // consume 'enum'
        const name = self.tokenText();
        self.advance();
        self.expect(.lbrace);

        var enum_buf = std.ArrayListUnmanaged(Decl.EnumMember){};
        while (self.at() != .rbrace and self.at() != .eof) {
            const member_start = self.tok.span.start;
            const member_name = self.tokenText();
            self.advance();
            var init_val: NodeIndex = null_node;
            if (self.eat(.assign)) {
                init_val = try self.parseAssignmentExpression();
            }
            try enum_buf.append(self.allocator, .{
                .name = member_name,
                .init = init_val,
                .span = self.span(member_start),
            });
            _ = self.eat(.comma);
        }
        self.expect(.rbrace);

        const members = try self.allocator.dupe(Decl.EnumMember, enum_buf.items);
        return self.ast.addDecl(.{ .enum_decl = .{
            .name = name,
            .members = members,
            .is_const = is_const,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseNamespaceDeclaration(self: *Parser, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'namespace' or 'module'
        const name = self.tokenText();
        self.advance();
        const body = try self.parseBlockStatement();
        return self.ast.addDecl(.{ .namespace = .{
            .name = name,
            .body = body,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    // ================================================================
    // Variable declarations
    // ================================================================

    fn parseVariableStatement(self: *Parser, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        const decl_idx = try self.parseVariableDeclaration(false, is_declare);
        _ = self.eat(.semicolon);
        return self.ast.addStmt(.{ .var_stmt = .{
            .decl = decl_idx,
            .span = self.span(start),
        } });
    }

    fn parseVariableDeclaration(self: *Parser, is_export: bool, is_declare: bool) !NodeIndex {
        const start = self.tok.span.start;
        const kind: Decl.VarDecl.VarKind = switch (self.at()) {
            .kw_var => .var_kw,
            .kw_let => .let,
            .kw_const => .const_kw,
            else => .let,
        };
        self.advance();

        var declarators = std.ArrayListUnmanaged(Decl.Declarator){};

        while (true) {
            const d = try self.parseDeclarator();
            try declarators.append(self.allocator, d);
            if (!self.eat(.comma)) break;
        }

        const owned = try self.allocator.dupe(Decl.Declarator, declarators.items);
        return self.ast.addDecl(.{ .variable = .{
            .kind = kind,
            .declarators = owned,
            .is_export = is_export,
            .is_declare = is_declare,
            .span = self.span(start),
        } });
    }

    fn parseVariableDeclarationForInit(self: *Parser) !NodeIndex {
        return self.parseVariableDeclaration(false, false);
    }

    fn parseDeclarator(self: *Parser) !Decl.Declarator {
        const start = self.tok.span.start;
        const binding = try self.parseBindingPattern();
        const type_ann = try self.parseOptionalTypeAnnotation();
        var init_val: NodeIndex = null_node;
        if (self.eat(.assign)) {
            init_val = try self.parseAssignmentExpression();
        }
        return .{
            .binding = binding,
            .type_ann = type_ann,
            .init = init_val,
            .span = self.span(start),
        };
    }

    fn parseBindingPattern(self: *Parser) error{OutOfMemory}!NodeIndex {
        return switch (self.at()) {
            .lbrace => self.parseObjectBindingPattern(),
            .lbrack => self.parseArrayBindingPattern(),
            else => blk: {
                // Simple identifier
                const ident_start = self.tok.span.start;
                const name = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{
                    .name = name,
                    .span = self.span(ident_start),
                } });
            },
        };
    }

    fn parseObjectBindingPattern(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.expect(.lbrace);
        var props = std.ArrayListUnmanaged(Pattern.ObjectPatternProp){};

        while (self.at() != .rbrace and self.at() != .eof) {
            const prop_start = self.tok.span.start;
            if (self.eat(.ellipsis)) {
                const arg = try self.parseBindingPattern();
                try props.append(self.allocator, .{
                    .key = arg,
                    .is_rest = true,
                    .span = self.span(prop_start),
                });
            } else {
                const key = try self.parsePropertyName();
                var value: NodeIndex = null_node;
                if (self.eat(.colon)) {
                    value = try self.parseBindingPattern();
                }
                var default_val: NodeIndex = null_node;
                if (self.eat(.assign)) {
                    default_val = try self.parseAssignmentExpression();
                }
                try props.append(self.allocator, .{
                    .key = key,
                    .value = value,
                    .default_value = default_val,
                    .span = self.span(prop_start),
                });
            }
            if (!self.eat(.comma)) break;
        }
        self.expect(.rbrace);
        const owned_props = try self.allocator.dupe(Pattern.ObjectPatternProp, props.items);
        return self.ast.addPattern(.{ .object = .{
            .properties = owned_props,
            .span = self.span(start),
        } });
    }

    fn parseArrayBindingPattern(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.expect(.lbrack);
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};

        while (self.at() != .rbrack and self.at() != .eof) {
            if (self.at() == .comma) {
                // Elision/hole
                try node_buf.append(self.allocator, null_node);
                self.advance();
                continue;
            }
            if (self.eat(.ellipsis)) {
                const rest_arg = try self.parseBindingPattern();
                const rest_idx = try self.ast.addPattern(.{ .rest = .{
                    .argument = rest_arg,
                    .span = self.span(start),
                } });
                try node_buf.append(self.allocator, rest_idx);
                break;
            }
            const elem = try self.parseBindingPattern();
            // Optional default
            if (self.eat(.assign)) {
                const default_val = try self.parseAssignmentExpression();
                const assign_pat = try self.ast.addPattern(.{ .assign = .{
                    .left = elem,
                    .right = default_val,
                } });
                try node_buf.append(self.allocator, assign_pat);
            } else {
                try node_buf.append(self.allocator, elem);
            }
            if (!self.eat(.comma)) break;
        }
        self.expect(.rbrack);
        const elems = try self.allocNodeList(node_buf.items);
        return self.ast.addPattern(.{ .array = .{
            .elements = elems,
            .span = self.span(start),
        } });
    }

    // ================================================================
    // Import/Export
    // ================================================================

    fn parseImportDeclaration(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'import'

        const is_type_only = self.at() == .kw_type and self.peekToken() != .lparen;
        if (is_type_only) self.advance();

        // Side-effect import: import 'module'
        if (self.at() == .string_lit) {
            const spec = self.tokenText();
            self.advance();
            _ = self.eat(.semicolon);
            return self.ast.addDecl(.{ .import_decl = .{
                .kind = .side_effect,
                .module_specifier = spec,
                .is_type_only = is_type_only,
                .span = self.span(start),
            } });
        }

        var default_binding: ?[]const u8 = null;
        var namespace_binding: ?[]const u8 = null;
        var import_buf = std.ArrayListUnmanaged(Decl.ImportBinding){};
        var kind: Decl.ImportDecl.ImportKind = .value;

        // import * as ns from 'mod'
        if (self.at() == .mul) {
            self.advance();
            self.expect(.kw_as);
            namespace_binding = self.tokenText();
            self.advance();
            kind = .namespace;
        }
        // import defaultName from 'mod' or import defaultName, { ... } from 'mod'
        else if (self.at() == .ident) {
            default_binding = self.tokenText();
            self.advance();
            kind = .default;
            // import defaultName, { named } from 'mod'
            if (self.eat(.comma)) {
                if (self.at() == .mul) {
                    self.advance();
                    self.expect(.kw_as);
                    namespace_binding = self.tokenText();
                    self.advance();
                } else {
                    try self.parseNamedImports(&import_buf);
                }
            }
        }
        // import { named } from 'mod'
        else if (self.at() == .lbrace) {
            try self.parseNamedImports(&import_buf);
        }

        self.expect(.kw_from);
        const spec = self.tokenText();
        self.advance();
        _ = self.eat(.semicolon);

        const named = try self.allocator.dupe(Decl.ImportBinding, import_buf.items);
        return self.ast.addDecl(.{ .import_decl = .{
            .kind = kind,
            .module_specifier = spec,
            .default_binding = default_binding,
            .namespace_binding = namespace_binding,
            .named_bindings = named,
            .is_type_only = is_type_only,
            .span = self.span(start),
        } });
    }

    fn parseNamedImports(self: *Parser, buf: *std.ArrayListUnmanaged(Decl.ImportBinding)) !void {
        self.expect(.lbrace);
        while (self.at() != .rbrace and self.at() != .eof) {
            const is_type = self.at() == .kw_type and self.peekToken() != .comma and self.peekToken() != .rbrace;
            if (is_type) self.advance();
            const name = self.tokenText();
            self.advance();
            var alias: ?[]const u8 = null;
            if (self.eat(.kw_as)) {
                alias = self.tokenText();
                self.advance();
            }
            try buf.append(self.allocator, .{
                .name = name,
                .alias = alias,
                .is_type = is_type,
            });
            if (!self.eat(.comma)) break;
        }
        self.expect(.rbrace);
    }

    fn parseExportDeclaration(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'export'

        const is_type_only = self.at() == .kw_type and self.peekToken() == .lbrace;
        if (is_type_only) self.advance();

        // export default ...
        if (self.eat(.kw_default)) {
            const decl = switch (self.at()) {
                .kw_function => try self.parseFunctionDeclaration(true, false),
                .kw_class => try self.parseClassDeclaration(true, false),
                .kw_async => blk: {
                    if (self.peekToken() == .kw_function) {
                        break :blk try self.parseAsyncFunctionDeclaration(true);
                    }
                    break :blk try self.parseAssignmentExpression();
                },
                else => blk: {
                    const expr = try self.parseAssignmentExpression();
                    _ = self.eat(.semicolon);
                    break :blk expr;
                },
            };
            return self.ast.addDecl(.{ .export_decl = .{
                .kind = .default,
                .default_expr = decl,
                .span = self.span(start),
            } });
        }

        // export * from 'mod'
        if (self.eat(.mul)) {
            if (self.eat(.kw_as)) {
                // export * as ns from 'mod'
                const alias = self.tokenText();
                _ = alias;
                self.advance();
                self.expect(.kw_from);
                const spec = self.tokenText();
                self.advance();
                _ = self.eat(.semicolon);
                return self.ast.addDecl(.{ .export_decl = .{
                    .kind = .all_as,
                    .module_specifier = spec,
                    .span = self.span(start),
                } });
            }
            self.expect(.kw_from);
            const spec = self.tokenText();
            self.advance();
            _ = self.eat(.semicolon);
            return self.ast.addDecl(.{ .export_decl = .{
                .kind = .all,
                .module_specifier = spec,
                .span = self.span(start),
            } });
        }

        // export { named } or export { named } from 'mod'
        if (self.at() == .lbrace) {
            var export_buf = std.ArrayListUnmanaged(Decl.ExportBinding){};
            self.expect(.lbrace);
            while (self.at() != .rbrace and self.at() != .eof) {
                const name = self.tokenText();
                self.advance();
                var alias: ?[]const u8 = null;
                if (self.eat(.kw_as)) {
                    alias = self.tokenText();
                    self.advance();
                }
                try export_buf.append(self.allocator, .{ .name = name, .alias = alias });
                if (!self.eat(.comma)) break;
            }
            self.expect(.rbrace);
            var spec: ?[]const u8 = null;
            if (self.eat(.kw_from)) {
                spec = self.tokenText();
                self.advance();
            }
            _ = self.eat(.semicolon);
            const named = try self.allocator.dupe(Decl.ExportBinding, export_buf.items);
            return self.ast.addDecl(.{ .export_decl = .{
                .kind = .named,
                .module_specifier = spec,
                .named_bindings = named,
                .is_type_only = is_type_only,
                .span = self.span(start),
            } });
        }

        // export function/class/const/let/var/...
        const decl = try self.parseStatementOrDeclaration();
        return self.ast.addDecl(.{ .export_decl = .{
            .kind = .declaration,
            .declaration = decl,
            .span = self.span(start),
        } });
    }

    // ================================================================
    // Expressions
    // ================================================================

    fn parseExpression(self: *Parser) error{OutOfMemory}!NodeIndex {
        const expr = try self.parseAssignmentExpression();

        // Comma operator (sequence expression)
        if (self.at() == .comma) {
            var node_buf = std.ArrayListUnmanaged(NodeIndex){};
            try node_buf.append(self.allocator, expr);
            while (self.eat(.comma)) {
                const next_expr = try self.parseAssignmentExpression();
                try node_buf.append(self.allocator, next_expr);
            }
            if (node_buf.items.len > 1) {
                const exprs = try self.allocNodeList(node_buf.items);
                const start = if (self.ast.getNode(expr)) |n| switch (n.*) {
                    .expr => |e| switch (e) {
                        .ident => |id| id.span.start,
                        else => Pos.zero,
                    },
                    else => Pos.zero,
                } else Pos.zero;
                return self.ast.addExpr(.{ .sequence = .{
                    .exprs = exprs,
                    .span = self.span(start),
                } });
            }
        }

        return expr;
    }

    fn parseAssignmentExpression(self: *Parser) error{OutOfMemory}!NodeIndex {
        // Check for arrow function: (params) => body or ident => body
        // Simplified: if we see ident followed by =>, parse as arrow
        if (self.at() == .ident and self.peekToken() == .arrow) {
            return self.parseArrowFunction(false);
        }
        if (self.at() == .kw_async and self.peekToken() == .ident) {
            // Could be: async ident => body
            // Save position for potential reparse... simplified for now
        }

        const left = try self.parseConditionalExpression();

        // Assignment
        if (self.at().isAssignment()) {
            const op = self.assignOpFromToken(self.at());
            self.advance();
            const right = try self.parseAssignmentExpression();
            return self.ast.addExpr(.{ .assign = .{
                .op = op,
                .left = left,
                .right = right,
            } });
        }

        return left;
    }

    fn parseConditionalExpression(self: *Parser) !NodeIndex {
        const expr = try self.parseBinaryExpression(0);

        if (self.eat(.question)) {
            const consequent = try self.parseAssignmentExpression();
            self.expect(.colon);
            const alternate = try self.parseAssignmentExpression();
            return self.ast.addExpr(.{ .conditional = .{
                .condition = expr,
                .consequent = consequent,
                .alternate = alternate,
            } });
        }

        return expr;
    }

    /// Precedence climbing binary expression parser.
    fn parseBinaryExpression(self: *Parser, min_precedence: u8) !NodeIndex {
        var left = try self.parseUnaryExpression();

        while (true) {
            const prec = self.at().precedence();
            if (prec == 0 or prec < min_precedence) break;

            const op = self.binaryOpFromToken(self.at());
            self.advance();

            // Right-associative for ** (exponentiation)
            const next_prec: u8 = if (op == .power) prec else prec + 1;
            const right = try self.parseBinaryExpression(next_prec);

            left = try self.ast.addExpr(.{ .binary = .{
                .op = op,
                .left = left,
                .right = right,
            } });
        }

        // Handle 'as' and 'satisfies' (TS postfix type operators)
        if (self.at() == .kw_as) {
            self.advance();
            const type_node = try self.parseType();
            left = try self.ast.addExpr(.{ .as_expr = .{
                .expr = left,
                .type_node = type_node,
            } });
        } else if (self.at() == .kw_satisfies) {
            self.advance();
            const type_node = try self.parseType();
            left = try self.ast.addExpr(.{ .satisfies_expr = .{
                .expr = left,
                .type_node = type_node,
            } });
        }

        return left;
    }

    fn parseUnaryExpression(self: *Parser) !NodeIndex {
        return switch (self.at()) {
            .sub => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .unary = .{ .op = .neg, .operand = operand, .span = self.span(start) } });
            },
            .add => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .unary = .{ .op = .pos, .operand = operand, .span = self.span(start) } });
            },
            .lnot => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .unary = .{ .op = .lnot, .operand = operand, .span = self.span(start) } });
            },
            .not => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .unary = .{ .op = .bitnot, .operand = operand, .span = self.span(start) } });
            },
            .kw_typeof => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .typeof_expr = .{ .operand = operand, .span = self.span(start) } });
            },
            .kw_void => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .void_expr = .{ .operand = operand, .span = self.span(start) } });
            },
            .kw_delete => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .delete_expr = .{ .operand = operand, .span = self.span(start) } });
            },
            .kw_await => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseUnaryExpression();
                break :blk self.ast.addExpr(.{ .await_expr = .{ .argument = operand, .span = self.span(start) } });
            },
            .increment => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseLeftHandSideExpression();
                break :blk self.ast.addExpr(.{ .update = .{ .op = .increment, .operand = operand, .prefix = true, .span = self.span(start) } });
            },
            .decrement => blk: {
                const start = self.tok.span.start;
                self.advance();
                const operand = try self.parseLeftHandSideExpression();
                break :blk self.ast.addExpr(.{ .update = .{ .op = .decrement, .operand = operand, .prefix = true, .span = self.span(start) } });
            },
            else => blk: {
                const expr = try self.parsePostfixExpression();
                break :blk expr;
            },
        };
    }

    fn parsePostfixExpression(self: *Parser) !NodeIndex {
        var expr = try self.parseCallExpression();

        // Postfix ++ / --
        if (!self.hasLineBreakBefore()) {
            if (self.at() == .increment) {
                self.advance();
                expr = try self.ast.addExpr(.{ .update = .{ .op = .increment, .operand = expr, .prefix = false } });
            } else if (self.at() == .decrement) {
                self.advance();
                expr = try self.ast.addExpr(.{ .update = .{ .op = .decrement, .operand = expr, .prefix = false } });
            }
        }

        // Non-null assertion: expr!
        if (self.at() == .lnot and !self.hasLineBreakBefore()) {
            self.advance();
            expr = try self.ast.addExpr(.{ .non_null = .{ .expr = expr } });
        }

        return expr;
    }

    fn parseCallExpression(self: *Parser) !NodeIndex {
        var expr = try self.parseLeftHandSideExpression();

        while (true) {
            if (self.at() == .lparen) {
                const args = try self.parseArguments();
                expr = try self.ast.addExpr(.{ .call = .{
                    .callee = expr,
                    .args = args,
                } });
            } else if (self.at() == .lbrack) {
                self.advance();
                const index = try self.parseExpression();
                self.expect(.rbrack);
                expr = try self.ast.addExpr(.{ .computed_member = .{
                    .object = expr,
                    .property = index,
                } });
            } else if (self.at() == .period) {
                self.advance();
                const prop = self.tokenText();
                self.advance();
                expr = try self.ast.addExpr(.{ .member = .{
                    .object = expr,
                    .property = prop,
                } });
            } else if (self.at() == .optional_chain) {
                self.advance();
                if (self.at() == .lparen) {
                    const args = try self.parseArguments();
                    expr = try self.ast.addExpr(.{ .optional_call = .{
                        .callee = expr,
                        .args = args,
                    } });
                } else if (self.at() == .lbrack) {
                    self.advance();
                    const index = try self.parseExpression();
                    self.expect(.rbrack);
                    expr = try self.ast.addExpr(.{ .computed_member = .{
                        .object = expr,
                        .property = index,
                    } });
                } else {
                    const prop = self.tokenText();
                    self.advance();
                    expr = try self.ast.addExpr(.{ .optional_member = .{
                        .object = expr,
                        .property = prop,
                    } });
                }
            } else if (self.at() == .template_head or self.at() == .template_none) {
                // Tagged template: expr`template`
                const template = try self.parseTemplateLiteral();
                expr = try self.ast.addExpr(.{ .tagged_template = .{
                    .tag = expr,
                    .template = template,
                } });
            } else break;
        }

        return expr;
    }

    fn parseLeftHandSideExpression(self: *Parser) !NodeIndex {
        if (self.at() == .kw_new) {
            return self.parseNewExpression();
        }
        return self.parsePrimaryExpression();
    }

    fn parseNewExpression(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'new'

        // new.target
        if (self.at() == .period) {
            self.advance();
            if (self.at() == .ident and std.mem.eql(u8, self.tokenText(), "target")) {
                self.advance();
                return self.ast.addExpr(.{ .member = .{
                    .object = try self.ast.addExpr(.{ .ident = .{ .name = "new", .span = self.span(start) } }),
                    .property = "target",
                } });
            }
        }

        var callee = try self.parsePrimaryExpression();

        // Member access chain on the callee: new Foo.Bar(...)
        while (self.at() == .period) {
            self.advance();
            const prop = self.tokenText();
            self.advance();
            callee = try self.ast.addExpr(.{ .member = .{
                .object = callee,
                .property = prop,
            } });
        }

        var args: ts_ast.NodeList = &.{};
        if (self.at() == .lparen) {
            args = try self.parseArguments();
        }

        return self.ast.addExpr(.{ .new_expr = .{
            .callee = callee,
            .args = args,
            .span = self.span(start),
        } });
    }

    fn parsePrimaryExpression(self: *Parser) error{OutOfMemory}!NodeIndex {
        const start = self.tok.span.start;

        return switch (self.at()) {
            .ident => blk: {
                const name = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{
                    .name = name,
                    .span = self.span(start),
                } });
            },
            .private_ident => blk: {
                const name = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{
                    .name = name,
                    .span = self.span(start),
                } });
            },
            .int_lit, .float_lit => blk: {
                const text = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{
                    .kind = .number,
                    .value = text,
                    .span = self.span(start),
                } });
            },
            .bigint_lit => blk: {
                const text = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{
                    .kind = .bigint,
                    .value = text,
                    .span = self.span(start),
                } });
            },
            .string_lit => blk: {
                const text = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{
                    .kind = .string,
                    .value = text,
                    .span = self.span(start),
                } });
            },
            .regex_lit => blk: {
                const text = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{
                    .kind = .regex,
                    .value = text,
                    .span = self.span(start),
                } });
            },
            .template_head, .template_none => self.parseTemplateLiteral(),
            .kw_true => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{ .kind = .boolean, .value = "true", .span = self.span(start) } });
            },
            .kw_false => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{ .kind = .boolean, .value = "false", .span = self.span(start) } });
            },
            .kw_null => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{ .kind = .null_lit, .value = "null", .span = self.span(start) } });
            },
            .kw_undefined => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .literal = .{ .kind = .undefined, .value = "undefined", .span = self.span(start) } });
            },
            .kw_this => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .this_expr = .{ .span = self.span(start) } });
            },
            .kw_super => blk: {
                self.advance();
                break :blk self.ast.addExpr(.{ .super_expr = .{ .span = self.span(start) } });
            },
            .lparen => self.parseParenthesizedOrArrow(),
            .lbrack => self.parseArrayLiteral(),
            .lbrace => self.parseObjectLiteral(),
            .kw_function => self.parseFunctionExpression(),
            .kw_class => self.parseClassExpression(),
            .kw_async => blk: {
                if (self.peekToken() == .kw_function) {
                    break :blk self.parseAsyncFunctionExpression();
                }
                // async arrow: async (params) => body
                if (self.peekToken() == .lparen or self.peekToken() == .ident) {
                    break :blk self.parseArrowFunction(true);
                }
                // Just 'async' as identifier
                const name = self.tokenText();
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{
                    .name = name,
                    .span = self.span(start),
                } });
            },
            .kw_yield => blk: {
                self.advance();
                const is_delegate = self.eat(.mul);
                var argument: NodeIndex = null_node;
                if (!self.hasLineBreakBefore() and self.at() != .semicolon and
                    self.at() != .rbrace and self.at() != .rparen and
                    self.at() != .rbrack and self.at() != .colon and
                    self.at() != .comma and self.at() != .eof)
                {
                    argument = try self.parseAssignmentExpression();
                }
                break :blk self.ast.addExpr(.{ .yield_expr = .{
                    .argument = argument,
                    .delegate = is_delegate,
                    .span = self.span(start),
                } });
            },
            .ellipsis => blk: {
                self.advance();
                const operand = try self.parseAssignmentExpression();
                break :blk self.ast.addExpr(.{ .spread = .{
                    .operand = operand,
                    .span = self.span(start),
                } });
            },
            .quo => blk: {
                // Might be a regex — rescan
                const regex = self.scanner.reScanSlashAsRegex(self.tok.span.start);
                self.tok = regex;
                if (regex.tok == .regex_lit) {
                    const text = regex.text;
                    self.advance();
                    break :blk self.ast.addExpr(.{ .literal = .{
                        .kind = .regex,
                        .value = text,
                        .span = self.span(start),
                    } });
                }
                // Not a regex — treat as illegal
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{ .name = "/", .span = self.span(start) } });
            },
            else => blk: {
                // Try contextual keywords as identifiers
                if (self.at().isKeyword()) {
                    const name = self.tokenText();
                    self.advance();
                    break :blk self.ast.addExpr(.{ .ident = .{
                        .name = name,
                        .span = self.span(start),
                    } });
                }
                // Unknown token — skip
                self.advance();
                break :blk self.ast.addExpr(.{ .ident = .{ .name = "", .span = self.span(start) } });
            },
        };
    }

    fn parseTemplateLiteral(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;

        if (self.at() == .template_none) {
            const text = self.tokenText();
            self.advance();
            return self.ast.addExpr(.{ .template = .{
                .quasis = try self.allocator.dupe([]const u8, &.{text}),
                .expressions = &.{},
                .span = self.span(start),
            } });
        }

        // template_head
        var quasis = std.ArrayListUnmanaged([]const u8){};
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};

        try quasis.append(self.allocator, self.tokenText());
        self.advance(); // consume head

        while (true) {
            const expr = try self.parseExpression();
            try node_buf.append(self.allocator, expr);

            if (self.at() == .template_middle) {
                try quasis.append(self.allocator, self.tokenText());
                self.advance();
            } else if (self.at() == .template_tail) {
                try quasis.append(self.allocator, self.tokenText());
                self.advance();
                break;
            } else {
                // Unterminated or error
                break;
            }
        }

        const owned_quasis = try self.allocator.dupe([]const u8, quasis.items);
        const exprs = try self.allocNodeList(node_buf.items);
        return self.ast.addExpr(.{ .template = .{
            .quasis = owned_quasis,
            .expressions = exprs,
            .span = self.span(start),
        } });
    }

    fn parseParenthesizedOrArrow(self: *Parser) error{OutOfMemory}!NodeIndex {
        // Arrow function ambiguity: `(a, b) => ...` vs `(a, b)` as expression.
        // Reference: TypeScript-Go uses mark()/rewind() for speculative parsing.
        // We use a lookahead scan: save scanner state, try to scan past the
        // parameter list to see if `=>` follows. If yes, parse as arrow.
        const start = self.tok.span.start;

        if (self.isArrowFunctionStart()) {
            // Parse as arrow function with full parameter list
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            self.expect(.arrow);
            const body = try self.parseArrowBody();
            return self.ast.addExpr(.{ .arrow_function = .{
                .params = params,
                .return_type = return_type,
                .body = body,
                .span = self.span(start),
            } });
        }

        // Parse as parenthesized expression
        self.advance(); // consume '('
        if (self.at() == .rparen) {
            self.advance();
            // () followed by => (already excluded above) — error
            return self.ast.addExpr(.{ .paren = .{ .inner = null_node } });
        }
        const expr = try self.parseExpression();
        self.expect(.rparen);
        return self.ast.addExpr(.{ .paren = .{
            .inner = expr,
            .span = self.span(start),
        } });
    }

    /// Lookahead: scan ahead from '(' to see if this is an arrow function.
    /// Scans balanced parens, then checks for optional ':' type, then '=>'.
    /// Does NOT consume any tokens (saves/restores scanner state).
    fn isArrowFunctionStart(self: *Parser) bool {
        // Save parser + scanner state
        const saved_tok = self.tok;
        const saved_prev_end = self.prev_end;
        const saved_pos = self.scanner.pos;
        const saved_ch = self.scanner.ch;
        const saved_prev_token = self.scanner.prev_token;
        const saved_in_template = self.scanner.in_template_expr;
        const saved_depth = self.scanner.template_brace_depth;

        defer {
            // Restore all state
            self.tok = saved_tok;
            self.prev_end = saved_prev_end;
            self.scanner.pos = saved_pos;
            self.scanner.ch = saved_ch;
            self.scanner.prev_token = saved_prev_token;
            self.scanner.in_template_expr = saved_in_template;
            self.scanner.template_brace_depth = saved_depth;
        }

        // Must start with '('
        if (self.at() != .lparen) return false;
        self.advance();

        // Scan past balanced parens
        var paren_depth: u32 = 1;
        while (paren_depth > 0 and self.at() != .eof) {
            if (self.at() == .lparen) paren_depth += 1;
            if (self.at() == .rparen) paren_depth -= 1;
            self.advance();
        }

        // Skip optional return type annotation: `: Type`
        if (self.at() == .colon) {
            self.advance();
            // Skip the type — scan until we see => or something that ends the type
            var angle_depth: u32 = 0;
            while (self.at() != .eof) {
                if (self.at() == .arrow and angle_depth == 0) break;
                if (self.at() == .lbrace or self.at() == .semicolon) return false;
                if (self.at() == .lss) angle_depth += 1;
                if (self.at() == .gtr and angle_depth > 0) angle_depth -= 1;
                self.advance();
            }
        }

        return self.at() == .arrow;
    }

    fn parseArrowFunction(self: *Parser, is_async: bool) !NodeIndex {
        const start = self.tok.span.start;
        if (is_async) self.advance(); // consume 'async'

        if (self.at() == .ident) {
            // Single param: x => body
            const param_name = self.tokenText();
            const param_start = self.tok.span.start;
            self.advance();
            self.expect(.arrow);

            const param_binding = try self.ast.addExpr(.{ .ident = .{
                .name = param_name,
                .span = self.span(param_start),
            } });
            const params = try self.allocator.dupe(Param, &.{.{ .binding = param_binding }});
            const body = try self.parseArrowBody();
            return self.ast.addExpr(.{ .arrow_function = .{
                .params = params,
                .body = body,
                .is_async = is_async,
                .span = self.span(start),
            } });
        }

        if (self.at() == .lparen) {
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            self.expect(.arrow);
            const body = try self.parseArrowBody();
            return self.ast.addExpr(.{ .arrow_function = .{
                .params = params,
                .return_type = return_type,
                .body = body,
                .is_async = is_async,
                .span = self.span(start),
            } });
        }

        // Fallback
        return self.parseExpressionStatement();
    }

    fn parseArrowFunctionBody(self: *Parser, _: []const Param, is_async: bool, start: Pos) !NodeIndex {
        self.advance(); // consume '=>'
        const body = try self.parseArrowBody();
        return self.ast.addExpr(.{ .arrow_function = .{
            .body = body,
            .is_async = is_async,
            .span = self.span(start),
        } });
    }

    fn parseArrowBody(self: *Parser) !NodeIndex {
        if (self.at() == .lbrace) {
            return self.parseBlockStatement();
        }
        return self.parseAssignmentExpression();
    }

    fn parseArrayLiteral(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.expect(.lbrack);
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};

        while (self.at() != .rbrack and self.at() != .eof) {
            if (self.at() == .comma) {
                // Elision
                try node_buf.append(self.allocator, null_node);
                self.advance();
                continue;
            }
            if (self.at() == .ellipsis) {
                const spread_start = self.tok.span.start;
                self.advance();
                const operand = try self.parseAssignmentExpression();
                const spread = try self.ast.addExpr(.{ .spread = .{
                    .operand = operand,
                    .span = self.span(spread_start),
                } });
                try node_buf.append(self.allocator, spread);
            } else {
                const elem = try self.parseAssignmentExpression();
                try node_buf.append(self.allocator, elem);
            }
            if (!self.eat(.comma)) break;
        }
        self.expect(.rbrack);

        const elems = try self.allocNodeList(node_buf.items);
        return self.ast.addExpr(.{ .array_literal = .{
            .elements = elems,
            .span = self.span(start),
        } });
    }

    fn parseObjectLiteral(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.expect(.lbrace);
        var prop_buf = std.ArrayListUnmanaged(Expr.ObjectProperty){};

        while (self.at() != .rbrace and self.at() != .eof) {
            const prop = try self.parseObjectProperty();
            try prop_buf.append(self.allocator, prop);
            if (!self.eat(.comma)) break;
        }
        self.expect(.rbrace);

        const props = try self.allocator.dupe(Expr.ObjectProperty, prop_buf.items);
        return self.ast.addExpr(.{ .object_literal = .{
            .properties = props,
            .span = self.span(start),
        } });
    }

    fn parseObjectProperty(self: *Parser) !Expr.ObjectProperty {
        const start = self.tok.span.start;

        // Spread: ...expr
        if (self.at() == .ellipsis) {
            self.advance();
            const operand = try self.parseAssignmentExpression();
            return .{
                .kind = .spread,
                .key = operand,
                .span = self.span(start),
            };
        }

        // Get/set accessors
        if ((self.at() == .kw_get or self.at() == .kw_set) and self.peekIsPropertyName()) {
            const is_get = self.at() == .kw_get;
            self.advance();
            const key = try self.parsePropertyName();
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;
            return .{
                .kind = if (is_get) .get else .set,
                .key = key,
                .params = params,
                .return_type = return_type,
                .body = body,
                .span = self.span(start),
            };
        }

        // async method
        var is_async = false;
        if (self.at() == .kw_async and self.peekIsPropertyName()) {
            is_async = true;
            self.advance();
        }

        var is_generator = false;
        if (self.eat(.mul)) is_generator = true;

        var is_computed = false;
        var key: NodeIndex = undefined;

        if (self.at() == .lbrack) {
            // Computed property: [expr]: value
            is_computed = true;
            self.advance();
            key = try self.parseAssignmentExpression();
            self.expect(.rbrack);
        } else {
            key = try self.parsePropertyName();
        }

        // Method: key(params) { body }
        if (self.at() == .lparen or self.at() == .lss) {
            const type_params = try self.parseOptionalTypeParameters();
            const params = try self.parseParameterList();
            const return_type = try self.parseOptionalTypeAnnotation();
            const body = if (self.at() == .lbrace) try self.parseBlockStatement() else null_node;
            return .{
                .kind = .method,
                .key = key,
                .type_params = type_params,
                .params = params,
                .return_type = return_type,
                .body = body,
                .is_computed = is_computed,
                .is_async = is_async,
                .is_generator = is_generator,
                .span = self.span(start),
            };
        }

        // Shorthand: { x } ≡ { x: x }
        if (self.at() != .colon and !is_computed) {
            return .{
                .kind = .init,
                .key = key,
                .is_shorthand = true,
                .span = self.span(start),
            };
        }

        // Regular property: key: value
        self.expect(.colon);
        const value = try self.parseAssignmentExpression();
        return .{
            .kind = .init,
            .key = key,
            .value = value,
            .is_computed = is_computed,
            .span = self.span(start),
        };
    }

    fn parseFunctionExpression(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'function'
        const is_generator = self.eat(.mul);
        const name = if (self.at() == .ident) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;
        const type_params = try self.parseOptionalTypeParameters();
        const params = try self.parseParameterList();
        const return_type = try self.parseOptionalTypeAnnotation();
        const body = try self.parseBlockStatement();
        return self.ast.addExpr(.{ .function_expr = .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .body = body,
            .is_generator = is_generator,
            .span = self.span(start),
        } });
    }

    fn parseAsyncFunctionExpression(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;
        self.advance(); // consume 'async'
        self.advance(); // consume 'function'
        const is_generator = self.eat(.mul);
        const name = if (self.at() == .ident) blk: {
            const n = self.tokenText();
            self.advance();
            break :blk n;
        } else null;
        const type_params = try self.parseOptionalTypeParameters();
        const params = try self.parseParameterList();
        const return_type = try self.parseOptionalTypeAnnotation();
        const body = try self.parseBlockStatement();
        return self.ast.addExpr(.{ .function_expr = .{
            .name = name,
            .type_params = type_params,
            .params = params,
            .return_type = return_type,
            .body = body,
            .is_async = true,
            .is_generator = is_generator,
            .span = self.span(start),
        } });
    }

    fn parseClassExpression(self: *Parser) !NodeIndex {
        // Reuse class declaration but as expression
        return self.parseClassDeclaration(false, false);
    }

    fn parseArguments(self: *Parser) !ts_ast.NodeList {
        self.expect(.lparen);
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};
        while (self.at() != .rparen and self.at() != .eof) {
            if (self.at() == .ellipsis) {
                const spread_start = self.tok.span.start;
                self.advance();
                const operand = try self.parseAssignmentExpression();
                const spread = try self.ast.addExpr(.{ .spread = .{
                    .operand = operand,
                    .span = self.span(spread_start),
                } });
                try node_buf.append(self.allocator, spread);
            } else {
                const arg = try self.parseAssignmentExpression();
                try node_buf.append(self.allocator, arg);
            }
            if (!self.eat(.comma)) break;
        }
        self.expect(.rparen);
        return self.allocNodeList(node_buf.items);
    }

    // ================================================================
    // Type annotations
    // ================================================================

    fn parseOptionalTypeAnnotation(self: *Parser) !NodeIndex {
        if (self.eat(.colon)) return self.parseType();
        return null_node;
    }

    fn parseType(self: *Parser) error{OutOfMemory}!NodeIndex {
        var left = try self.parseNonUnionType();

        // Union type: T | U
        if (self.at() == .@"or") {
            var node_buf = std.ArrayListUnmanaged(NodeIndex){};
            try node_buf.append(self.allocator, left);
            while (self.eat(.@"or")) {
                const t = try self.parseNonUnionType();
                try node_buf.append(self.allocator, t);
            }
            const types = try self.allocNodeList(node_buf.items);
            left = try self.ast.addType(.{ .union_type = .{ .types = types } });
        }

        // Intersection type: T & U
        if (self.at() == .@"and") {
            var node_buf = std.ArrayListUnmanaged(NodeIndex){};
            try node_buf.append(self.allocator, left);
            while (self.eat(.@"and")) {
                const t = try self.parseNonUnionType();
                try node_buf.append(self.allocator, t);
            }
            const types = try self.allocNodeList(node_buf.items);
            left = try self.ast.addType(.{ .intersection = .{ .types = types } });
        }

        return left;
    }

    fn parseNonUnionType(self: *Parser) !NodeIndex {
        var base = try self.parsePrimaryType();

        // Array type: T[]
        while (self.at() == .lbrack and self.peekToken() == .rbrack) {
            self.advance(); // [
            self.advance(); // ]
            base = try self.ast.addType(.{ .array = .{ .element = base } });
        }

        return base;
    }

    fn parsePrimaryType(self: *Parser) !NodeIndex {
        return switch (self.at()) {
            .kw_number => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .number } });
            },
            .kw_string => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .string } });
            },
            .kw_boolean => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .boolean } });
            },
            .kw_void => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .void_kw } });
            },
            .kw_null => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .null_kw } });
            },
            .kw_undefined => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .undefined } });
            },
            .kw_any => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .any } });
            },
            .kw_unknown => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .unknown } });
            },
            .kw_never => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .never } });
            },
            .kw_object => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .object } });
            },
            .kw_symbol => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .symbol } });
            },
            .kw_bigint => blk: {
                self.advance();
                break :blk self.ast.addType(.{ .keyword = .{ .kind = .bigint } });
            },
            .kw_typeof => blk: {
                self.advance();
                const expr = try self.parsePrimaryExpression();
                break :blk self.ast.addType(.{ .typeof_type = .{ .expr = expr } });
            },
            .kw_keyof => blk: {
                self.advance();
                const operand = try self.parseType();
                break :blk self.ast.addType(.{ .type_operator = .{ .op = .keyof, .operand = operand } });
            },
            .kw_readonly => blk: {
                self.advance();
                const operand = try self.parseType();
                break :blk self.ast.addType(.{ .type_operator = .{ .op = .readonly, .operand = operand } });
            },
            .kw_infer => blk: {
                self.advance();
                const name = self.tokenText();
                self.advance();
                var constraint: NodeIndex = null_node;
                if (self.eat(.kw_extends)) constraint = try self.parseType();
                break :blk self.ast.addType(.{ .infer_type = .{ .name = name, .constraint = constraint } });
            },
            .lparen => blk: {
                // Function type: (params) => ReturnType  or  parenthesized type
                self.advance();
                if (self.at() == .rparen) {
                    self.advance();
                    self.expect(.arrow);
                    const ret = try self.parseType();
                    break :blk self.ast.addType(.{ .function_type = .{ .return_type = ret } });
                }
                const inner = try self.parseType();
                self.expect(.rparen);
                if (self.at() == .arrow) {
                    self.advance();
                    const ret = try self.parseType();
                    break :blk self.ast.addType(.{ .function_type = .{ .return_type = ret } });
                }
                break :blk self.ast.addType(.{ .parenthesized = .{ .inner = inner } });
            },
            .lbrack => blk: {
                // Tuple type: [T, U, V]
                self.advance();
                var node_buf = std.ArrayListUnmanaged(NodeIndex){};
                while (self.at() != .rbrack and self.at() != .eof) {
                    const t = try self.parseType();
                    try node_buf.append(self.allocator, t);
                    if (!self.eat(.comma)) break;
                }
                self.expect(.rbrack);
                const elems = try self.allocNodeList(node_buf.items);
                break :blk self.ast.addType(.{ .tuple = .{ .elements = elems } });
            },
            .lbrace => blk: {
                // Object type: { key: Type; ... }
                self.advance();
                var member_buf = std.ArrayListUnmanaged(Decl.InterfaceMember){};
                while (self.at() != .rbrace and self.at() != .eof) {
                    const m = try self.parseInterfaceMember();
                    try member_buf.append(self.allocator, m);
                    _ = self.eat(.semicolon) or self.eat(.comma);
                }
                self.expect(.rbrace);
                const members = try self.allocator.dupe(Decl.InterfaceMember, member_buf.items);
                break :blk self.ast.addType(.{ .object_type = .{ .members = members } });
            },
            .string_lit, .int_lit, .float_lit, .kw_true, .kw_false => blk: {
                // Literal type
                const lit = try self.parsePrimaryExpression();
                break :blk self.ast.addType(.{ .literal = .{ .value = lit } });
            },
            else => blk: {
                // Type reference: Name or Name<T, U>
                break :blk self.parseTypeReference();
            },
        };
    }

    fn parseTypeReference(self: *Parser) !NodeIndex {
        const name = self.tokenText();
        self.advance();

        // Qualified name: A.B.C
        var full_name = name;
        _ = &full_name;
        // Simplified — for now just use the last segment

        var type_args: ts_ast.NodeList = &.{};
        if (self.at() == .lss) {
            type_args = try self.parseTypeArguments();
        }

        return self.ast.addType(.{ .reference = .{
            .name = name,
            .type_args = type_args,
        } });
    }

    fn parseTypeArguments(self: *Parser) !ts_ast.NodeList {
        self.expect(.lss); // <
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};
        while (self.at() != .gtr and self.at() != .eof) {
            const t = try self.parseType();
            try node_buf.append(self.allocator, t);
            if (!self.eat(.comma)) break;
        }
        self.expect(.gtr); // >
        return self.allocNodeList(node_buf.items);
    }

    fn parseOptionalTypeParameters(self: *Parser) !ts_ast.NodeList {
        if (self.at() != .lss) return &.{};
        self.advance(); // <
        var node_buf = std.ArrayListUnmanaged(NodeIndex){};
        while (self.at() != .gtr and self.at() != .eof) {
            const tp = try self.parseTypeParameter();
            try node_buf.append(self.allocator, tp);
            if (!self.eat(.comma)) break;
        }
        self.expect(.gtr); // >
        return self.allocNodeList(node_buf.items);
    }

    fn parseTypeParameter(self: *Parser) !NodeIndex {
        const name = self.tokenText();
        self.advance();
        var constraint: NodeIndex = null_node;
        if (self.eat(.kw_extends)) constraint = try self.parseType();
        if (self.eat(.assign)) _ = try self.parseType(); // default type (consumed, stored in constraint for now)
        return self.ast.addType(.{ .reference = .{
            .name = name,
            .type_args = if (constraint != null_node) try self.allocNodeList(&.{constraint}) else &.{},
        } });
    }

    // ================================================================
    // Parameters
    // ================================================================

    fn parseParameterList(self: *Parser) ![]const Param {
        self.expect(.lparen);
        var param_buf = std.ArrayListUnmanaged(Param){};
        while (self.at() != .rparen and self.at() != .eof) {
            const param = try self.parseParameter();
            try param_buf.append(self.allocator, param);
            if (!self.eat(.comma)) break;
        }
        self.expect(.rparen);
        return self.allocator.dupe(Param, param_buf.items);
    }

    fn parseParameter(self: *Parser) !Param {
        const start = self.tok.span.start;

        // Accessibility modifier
        var accessibility: ts_ast.Accessibility = .none;
        switch (self.at()) {
            .kw_public => {
                accessibility = .public;
                self.advance();
            },
            .kw_protected => {
                accessibility = .protected;
                self.advance();
            },
            .kw_private => {
                accessibility = .private;
                self.advance();
            },
            else => {},
        }

        var is_readonly = false;
        if (self.eat(.kw_readonly)) is_readonly = true;

        var is_rest = false;
        if (self.eat(.ellipsis)) is_rest = true;

        const binding = try self.parseBindingPattern();

        var is_optional = false;
        if (self.eat(.question)) is_optional = true;

        const type_ann = try self.parseOptionalTypeAnnotation();

        var default_value: NodeIndex = null_node;
        if (self.eat(.assign)) {
            default_value = try self.parseAssignmentExpression();
        }

        return .{
            .binding = binding,
            .type_ann = type_ann,
            .default_value = default_value,
            .is_rest = is_rest,
            .is_optional = is_optional,
            .is_readonly = is_readonly,
            .accessibility = accessibility,
            .span = self.span(start),
        };
    }

    // ================================================================
    // Helpers
    // ================================================================

    fn parsePropertyName(self: *Parser) !NodeIndex {
        const start = self.tok.span.start;

        if (self.at() == .lbrack) {
            // Computed property: [expr]
            self.advance();
            const expr = try self.parseAssignmentExpression();
            self.expect(.rbrack);
            return expr;
        }

        // Identifier, keyword-as-ident, string, or number
        const name = self.tokenText();
        self.advance();
        return self.ast.addExpr(.{ .ident = .{
            .name = name,
            .span = self.span(start),
        } });
    }

    fn peekToken(self: *Parser) Token {
        // Save state
        const saved_pos = self.scanner.pos;
        const saved_ch = self.scanner.ch;
        const saved_prev = self.scanner.prev_token;
        const saved_in_template = self.scanner.in_template_expr;
        const saved_depth = self.scanner.template_brace_depth;

        const next_tok = self.scanner.next();

        // Restore state
        self.scanner.pos = saved_pos;
        self.scanner.ch = saved_ch;
        self.scanner.prev_token = saved_prev;
        self.scanner.in_template_expr = saved_in_template;
        self.scanner.template_brace_depth = saved_depth;

        return next_tok.tok;
    }

    fn peekIsPropertyName(self: *Parser) bool {
        const next = self.peekToken();
        return next == .ident or next == .string_lit or next == .int_lit or
            next == .float_lit or next == .lbrack or next == .private_ident or
            next.isKeyword();
    }

    fn hasLineBreakBefore(self: *const Parser) bool {
        // Simplified: check if there's a newline between prev_end and current token
        const content = self.scanner.src.content;
        var i = self.prev_end.offset;
        const end = @min(self.tok.span.start.offset, @as(u32, @intCast(content.len)));
        while (i < end) : (i += 1) {
            if (content[i] == '\n') return true;
        }
        return false;
    }

    fn allocNodeList(self: *Parser, items: []const NodeIndex) !ts_ast.NodeList {
        return self.ast.allocNodeList(items);
    }

    fn binaryOpFromToken(self: *const Parser, tok: Token) Expr.BinaryOp {
        _ = self;
        return switch (tok) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .quo => .div,
            .rem => .rem,
            .power => .power,
            .eql => .eql,
            .neq => .neq,
            .strict_eql => .strict_eql,
            .strict_neq => .strict_neq,
            .lss => .lt,
            .leq => .lte,
            .gtr => .gt,
            .geq => .gte,
            .shl => .shl,
            .shr => .shr,
            .unsigned_shr => .unsigned_shr,
            .@"and" => .bitand,
            .@"or" => .bitor,
            .xor => .bitxor,
            .land => .land,
            .lor => .lor,
            .nullish_coalesce => .nullish_coalesce,
            .kw_instanceof => .instanceof,
            .kw_in => .in_op,
            else => .add,
        };
    }

    fn assignOpFromToken(self: *const Parser, tok: Token) Expr.AssignExpr.AssignOp {
        _ = self;
        return switch (tok) {
            .assign => .assign,
            .add_assign => .add_assign,
            .sub_assign => .sub_assign,
            .mul_assign => .mul_assign,
            .quo_assign => .div_assign,
            .rem_assign => .rem_assign,
            .power_assign => .power_assign,
            .shl_assign => .shl_assign,
            .shr_assign => .shr_assign,
            .unsigned_shr_assign => .unsigned_shr_assign,
            .and_assign => .bitand_assign,
            .or_assign => .bitor_assign,
            .xor_assign => .bitxor_assign,
            .land_assign => .land_assign,
            .lor_assign => .lor_assign,
            .nullish_assign => .nullish_assign,
            else => .assign,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "parser: variable declaration" {
    const content = "const x = 42;";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expect(parser.ast.source_file != null);
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: function declaration" {
    const content = "function hello(name: string): void { return; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expect(parser.ast.source_file != null);
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: class declaration" {
    const content = "class User { name: string; constructor(name: string) { this.name = name; } greet() { return this.name; } }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expect(parser.ast.source_file != null);
    const stmts = parser.ast.source_file.?.statements;
    try std.testing.expectEqual(@as(usize, 1), stmts.len);

    // Verify it's a class declaration
    const node = parser.ast.getNode(stmts[0]).?;
    switch (node.*) {
        .decl => |d| switch (d) {
            .class => |c| {
                try std.testing.expectEqualStrings("User", c.name.?);
                try std.testing.expectEqual(@as(usize, 3), c.members.len);
            },
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}

test "parser: if/else statement" {
    const content = "if (x > 0) { return x; } else { return -x; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: for-of loop" {
    const content = "for (const item of items) { console.log(item); }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: import declaration" {
    const content = "import { readFile, writeFile } from 'fs';";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);

    const node = parser.ast.getNode(parser.ast.source_file.?.statements[0]).?;
    switch (node.*) {
        .decl => |d| switch (d) {
            .import_decl => |imp| {
                try std.testing.expectEqualStrings("'fs'", imp.module_specifier);
                try std.testing.expectEqual(@as(usize, 2), imp.named_bindings.len);
            },
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}

test "parser: arrow function" {
    const content = "const add = (a: number, b: number) => a + b;";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: interface declaration" {
    const content = "interface Greetable { name: string; greet(): string; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);

    const node = parser.ast.getNode(parser.ast.source_file.?.statements[0]).?;
    switch (node.*) {
        .decl => |d| switch (d) {
            .interface => |iface| {
                try std.testing.expectEqualStrings("Greetable", iface.name);
                try std.testing.expectEqual(@as(usize, 2), iface.members.len);
            },
            else => return error.UnexpectedNode,
        },
        else => return error.UnexpectedNode,
    }
}

test "parser: try/catch/finally" {
    const content = "try { doSomething(); } catch (e) { handleError(e); } finally { cleanup(); }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: template literal" {
    const content = "const msg = `hello ${name}!`;";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: type alias" {
    const content = "type StringOrNumber = string | number;";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: enum declaration" {
    const content = "enum Color { Red, Green, Blue }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: export declaration" {
    const content = "export function hello(): string { return 'world'; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: switch statement" {
    const content = "switch (x) { case 1: break; case 2: return; default: throw new Error(); }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: destructuring" {
    const content = "const { name, age } = user; const [first, ...rest] = items;";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 2), parser.ast.source_file.?.statements.len);
}

test "parser: async/await" {
    const content = "async function fetchData() { const data = await fetch('/api'); return data; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 1), parser.ast.source_file.?.statements.len);
}

test "parser: multiple statements" {
    const content =
        \\const x: number = 1;
        \\let y = "hello";
        \\function add(a: number, b: number): number { return a + b; }
        \\if (x > 0) { console.log(x); }
    ;
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var parser = Parser.init(&src, std.testing.allocator);
    defer parser.deinit();

    try parser.parseSourceFile();
    try std.testing.expectEqual(@as(usize, 4), parser.ast.source_file.?.statements.len);
}
