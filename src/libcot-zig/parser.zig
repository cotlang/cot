//! Recursive descent parser for Cot source code.
//!
//! Produces a compact, data-oriented AST (see ast.zig). The parser operates
//! on a pre-tokenized token array rather than a streaming scanner — all
//! tokens are materialized into a MultiArrayList before parsing begins.
//!
//! Variable-length data (parameter lists, field lists, etc.) is collected
//! into a shared scratch buffer during parsing, then moved to extra_data
//! via listToSpan(). This eliminates per-list heap allocations.

const std = @import("std");
const Ast = @import("ast.zig");
const tok = @import("token.zig");
const src_mod = @import("source.zig");
const scanner_mod = @import("scanner.zig");

const Source = src_mod.Source;
const Scanner = scanner_mod.Scanner;

const Token = tok.Token;
const Node = Ast.Node;
const Tag = Node.Tag;
const Index = Ast.Index;
const OptionalIndex = Ast.OptionalIndex;
const TokenIndex = Ast.TokenIndex;
const OptionalTokenIndex = Ast.OptionalTokenIndex;
const ExtraIndex = Ast.ExtraIndex;
const SubRange = Ast.SubRange;

/// Recursive descent parser producing a compact, data-oriented AST.
/// Operates on a pre-tokenized token array. Variable-length data is
/// collected via a shared scratch buffer into a flat extra_data sidecar.
pub const Parser = struct {
    gpa: std.mem.Allocator,
    source: [:0]const u8,
    token_tags: []const Token,
    token_starts: []const u32,
    token_list: Ast.Ast.TokenList,

    // Mutable output arrays — frozen after parsing
    nodes: Ast.Ast.NodeList,
    extra_data: std.ArrayListUnmanaged(u32),
    errors: std.ArrayListUnmanaged(Ast.Ast.Error),

    // Temporary scratch buffer for collecting variable-length lists
    scratch: std.ArrayListUnmanaged(u32),

    tok_i: TokenIndex,
    safe_mode: bool,

    // Impl block context for @safe mode self injection
    current_impl_type: ?TokenIndex,
    current_impl_is_generic: bool,
    current_impl_is_actor: bool,

    pending_global_actor: ?TokenIndex,
    pending_doc_comment: ?TokenIndex,

    // Nesting depth to prevent stack overflow
    nest_level: u32,

    const max_nest_level: u32 = 10_000;

    pub fn init(gpa: std.mem.Allocator, source_text: [:0]const u8, safe_mode: bool) Parser {
        return .{
            .gpa = gpa,
            .source = source_text,
            .token_tags = &.{},
            .token_starts = &.{},
            .token_list = .{},
            .nodes = .{},
            .extra_data = .{},
            .errors = .{},
            .scratch = .{},
            .tok_i = 0,
            .safe_mode = safe_mode,
            .current_impl_type = null,
            .current_impl_is_generic = false,
            .current_impl_is_actor = false,
            .pending_global_actor = null,
            .pending_doc_comment = null,
            .nest_level = 0,
        };
    }


    /// Tokenize source text into the parser's token arrays.
    pub fn tokenize(p: *Parser) !void {
        // Empirically, Cot source has roughly a 6:1 byte-to-token ratio
        try p.token_list.ensureTotalCapacity(p.gpa, @max(p.source.len / 6, 32));

        // Create a Source object for the scanner
        var source_obj = Source.init(p.gpa, "parser", p.source);
        var scan = Scanner.init(&source_obj);
        while (true) {
            const t = scan.next();
            try p.token_list.append(p.gpa, .{
                .tag = t.tok,
                .start = t.span.start.offset,
            });
            if (t.tok == .eof) break;
        }

        const slice = p.token_list.slice();
        p.token_tags = slice.items(.tag);
        p.token_starts = slice.items(.start);
    }


    /// Append a node to the node list. Returns its index.
    fn addNode(p: *Parser, node: Node) !Index {
        try p.nodes.append(p.gpa, node);
        return @enumFromInt(p.nodes.len - 1);
    }

    /// Reserve a node slot. Returns the raw array index for later setNode().
    fn reserveNode(p: *Parser, tag: Tag) !usize {
        try p.nodes.append(p.gpa, .{
            .tag = tag,
            .main_token = 0,
            .data = .{ .none = {} },
        });
        return p.nodes.len - 1;
    }

    /// Fill a previously reserved node slot.
    fn setNode(p: *Parser, i: usize, node: Node) Index {
        p.nodes.set(i, node);
        return @enumFromInt(i);
    }

    /// Pack a struct into extra_data. Returns its starting ExtraIndex.
    /// SubRange fields consume 2 u32 slots; all others consume 1.
    fn addExtra(p: *Parser, extra: anytype) !ExtraIndex {
        const T = @TypeOf(extra);
        const fields = std.meta.fields(T);
        // Count total u32 slots needed
        comptime var slot_count: usize = 0;
        inline for (fields) |f| {
            slot_count += if (f.type == SubRange) 2 else 1;
        }
        try p.extra_data.ensureUnusedCapacity(p.gpa, slot_count);
        const result: ExtraIndex = @enumFromInt(p.extra_data.items.len);
        inline for (fields) |f| {
            const val = @field(extra, f.name);
            if (f.type == SubRange) {
                p.extra_data.appendAssumeCapacity(@intFromEnum(val.start));
                p.extra_data.appendAssumeCapacity(@intFromEnum(val.end));
            } else {
                const raw: u32 = switch (f.type) {
                    Index => @intFromEnum(val),
                    OptionalIndex => @intFromEnum(val),
                    OptionalTokenIndex => @intFromEnum(val),
                    ExtraIndex => @intFromEnum(val),
                    u32 => val,
                    Ast.FnFlags, Ast.StructFlags, Ast.FieldFlags, Ast.IfFlags,
                    Ast.SwitchCaseFlags, Ast.VarFlags, Ast.LocalVarFlags,
                    Ast.WhileFlags, Ast.ForFlags, Ast.ClosureFlags,
                    Ast.TaskFlags, Ast.CatchFlags, Ast.NewFlags,
                    Ast.DestructureFlags,
                    => @bitCast(val),
                    else => @compileError("unexpected extra_data field type: " ++ @typeName(f.type)),
                };
                p.extra_data.appendAssumeCapacity(raw);
            }
        }
        return result;
    }

    /// Move scratch items into extra_data as a SubRange.
    fn listToSpan(p: *Parser, items: []const u32) !SubRange {
        try p.extra_data.appendSlice(p.gpa, items);
        return .{
            .start = @enumFromInt(p.extra_data.items.len - items.len),
            .end = @enumFromInt(p.extra_data.items.len),
        };
    }



    fn currentTag(p: *const Parser) Token {
        return p.token_tags[p.tok_i];
    }

    fn advance(p: *Parser) TokenIndex {
        const i = p.tok_i;
        p.tok_i += 1;
        return i;
    }

    fn expectToken(p: *Parser, expected: Token) !TokenIndex {
        if (p.currentTag() == expected) return p.advance();
        try p.addError(.unexpected_token);
        return error.ParseError;
    }

    fn eatToken(p: *Parser, expected: Token) ?TokenIndex {
        if (p.currentTag() == expected) return p.advance();
        return null;
    }

    fn tokenText(p: *const Parser, ti: TokenIndex) []const u8 {
        const start = p.token_starts[ti];
        // Determine end by scanning to next token (skip trailing whitespace)
        if (ti + 1 < p.token_tags.len) {
            var end = p.token_starts[ti + 1];
            while (end > start and isWhitespace(p.source[end - 1])) end -= 1;
            return p.source[start..end];
        }
        return p.source[start..];
    }

    fn isWhitespace(c: u8) bool {
        return c == ' ' or c == '\n' or c == '\t' or c == '\r';
    }


    fn addError(p: *Parser, tag: Ast.Ast.ErrorTag) !void {
        try p.errors.append(p.gpa, .{
            .token = p.tok_i,
            .tag = tag,
        });
    }


    fn incNest(p: *Parser) !void {
        p.nest_level += 1;
        if (p.nest_level > max_nest_level) {
            try p.addError(.max_nesting_exceeded);
            return error.ParseError;
        }
    }

    fn decNest(p: *Parser) void {
        p.nest_level -= 1;
    }


    fn collectDocComment(p: *Parser) void {
        if (p.currentTag() != .doc_comment) {
            p.pending_doc_comment = null;
            return;
        }
        p.pending_doc_comment = p.tok_i;
        while (p.currentTag() == .doc_comment) {
            _ = p.advance();
        }
    }

    fn consumeDocComment(p: *Parser) OptionalTokenIndex {
        const doc = p.pending_doc_comment;
        p.pending_doc_comment = null;
        return if (doc) |d| @enumFromInt(d) else .none;
    }


    pub const Error = error{ ParseError, OutOfMemory };

    /// Parse the entire source file. Returns the completed AST.
    pub fn parse(p: *Parser) Error!Ast.Ast {
        // Tokenize
        try p.tokenize();

        // Pre-allocate node storage (estimate: tokens/2 nodes)
        try p.nodes.ensureTotalCapacity(p.gpa, @max((p.token_tags.len + 2) / 2, 1));

        // Root node at index 0 — will be filled in after parsing
        try p.nodes.append(p.gpa, .{
            .tag = .root,
            .main_token = 0,
            .data = .{ .none = {} },
        });

        // Parse top-level declarations
        const root_decls = try p.parseTopLevel();

        // Fill in root node data
        p.nodes.items(.data)[0] = .{ .extra_range = root_decls };

        // Freeze and return
        const extra = try p.extra_data.toOwnedSlice(p.gpa);
        errdefer p.gpa.free(extra);
        const errs = try p.errors.toOwnedSlice(p.gpa);
        errdefer p.gpa.free(errs);

        return .{
            .source = p.source,
            .tokens = .{
                .ptrs = undefined,
                .len = @intCast(p.token_tags.len),
                .capacity = @intCast(p.token_tags.len),
            },
            .nodes = p.nodes.toOwnedSlice(),
            .extra_data = extra,
            .errors = errs,
            .safe_mode = p.safe_mode,
        };
    }


    fn parseTopLevel(p: *Parser) Error!SubRange {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .eof) {
            p.collectDocComment();
            if (try p.parseDecl()) |decl_idx| {
                try p.scratch.append(p.gpa, @intFromEnum(decl_idx));
            } else {
                // Skip bad token for error recovery
                _ = p.advance();
            }
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }


    fn parseDecl(p: *Parser) Error!?Index {
        return switch (p.currentTag()) {
            .kw_fn => p.parseFnDeclFull(.{}),
            .kw_const => p.parseVarOrTypeDecl(true),
            .kw_var => p.parseVarDecl(false),
            .kw_struct => p.parseStructDecl(.auto),
            .kw_union => p.parseUnionDecl(),
            .kw_type => p.parseTypeAlias(),
            .kw_import => p.parseImportDecl(),
            .kw_impl => p.parseImplDecl(),
            .kw_trait => p.parseTraitDecl(),
            .kw_test => p.parseTestDecl(),
            .kw_bench => p.parseBenchDecl(),
            .kw_extern => {
                _ = p.advance(); // consume 'extern'
                if (p.currentTag() == .kw_struct) return p.parseStructDecl(.@"extern");
                if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_extern = true });
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_export => {
                _ = p.advance();
                if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_export = true });
                return p.parseDecl();
            },
            .kw_packed => {
                _ = p.advance();
                return p.parseStructDecl(.@"packed");
            },
            .kw_static => {
                _ = p.advance();
                if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_static = true });
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_async => p.parseAsyncDecl(),
            .kw_nonisolated => {
                _ = p.advance();
                if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_nonisolated = true });
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_actor => p.parseActorDecl(),
            .at => p.parseAttrDecl(),
            else => {
                try p.addError(.unexpected_token);
                return null;
            },
        };
    }

    const FnModifiers = struct {
        is_extern: bool = false,
        is_export: bool = false,
        is_async: bool = false,
        is_static: bool = false,
        is_inlinable: bool = false,
        is_nonisolated: bool = false,
    };

    fn parseFnDeclFull(p: *Parser, mods: FnModifiers) Error!?Index {
        const doc = p.consumeDocComment();
        const fn_token = p.advance(); // consume 'fn'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        _ = try p.expectToken(.lparen);

        // Type params: fn max(T)(a: T, b: T) — bare idents without ':' = type params
        var type_params = SubRange.empty;
        var param_bounds = SubRange.empty;
        if (p.currentTag() == .ident and p.peekIsNotColon()) {
            type_params = try p.parseIdentList(.rparen);
            _ = p.eatToken(.rparen) orelse {
                try p.addError(.expected_rparen);
                return null;
            };
            // Second ( for value params
            _ = try p.expectToken(.lparen);
        }

        // Value parameters
        const params = try p.parseFieldList(.rparen);
        _ = p.eatToken(.rparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };

        // Return type (anything before { or ; or where or eof)
        var return_type: OptionalIndex = .none;
        if (p.currentTag() != .lbrace and p.currentTag() != .semicolon and
            p.currentTag() != .eof and p.currentTag() != .kw_where)
        {
            if (try p.parseType()) |rt| {
                return_type = rt.toOptional();
            }
        }

        // Where clause: fn sort(T)(list: List(T)) where T: Ord
        if (p.currentTag() == .kw_where and type_params.len() > 0) {
            param_bounds = try p.parseWhereClause();
        }

        // Body
        var body_node: OptionalIndex = .none;
        if (mods.is_extern) {
            _ = p.eatToken(.semicolon);
        } else if (p.currentTag() == .lbrace) {
            if (try p.parseBlockStmt()) |b| {
                body_node = b.toOptional();
            }
        }

        const global_actor: OptionalTokenIndex = if (p.pending_global_actor) |ga|
            @enumFromInt(ga)
        else
            .none;
        p.pending_global_actor = null;

        const extra = try p.addExtra(Ast.FnDecl{
            .name_token = name_token,
            .type_params = type_params,
            .param_bounds = param_bounds,
            .params = params,
            .return_type = return_type,
            .flags = .{
                .is_export = mods.is_export,
                .is_async = mods.is_async,
                .is_static = mods.is_static,
                .is_inlinable = mods.is_inlinable,
                .is_nonisolated = mods.is_nonisolated,
            },
            .doc_comment = doc,
            .global_actor = global_actor,
        });

        if (mods.is_extern) {
            return try p.addNode(.{
                .tag = .fn_decl_extern,
                .main_token = fn_token,
                .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
            });
        }

        return try p.addNode(.{
            .tag = .fn_decl,
            .main_token = fn_token,
            .data = .{ .extra_and_node = .{
                extra,
                if (body_node.unwrap()) |b| b else @enumFromInt(0),
            } },
        });
    }

    /// Parse comma-separated `name: Type` fields until `end_tok`.
    fn parseFieldList(p: *Parser, end_tok: Token) Error!SubRange {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != end_tok and p.currentTag() != .eof) {
            p.collectDocComment();
            const field_doc = p.consumeDocComment();

            // Stop if we hit a declaration keyword (nested decl in struct body)
            if (p.currentTag() != .ident) break;

            const field_name_token = p.advance();

            if (p.eatToken(.colon) == null) {
                try p.addError(.unexpected_token);
                break;
            }

            // Check for 'sending' keyword (Swift SE-0430)
            var is_sending = false;
            if (p.currentTag() == .ident and p.isTokenText(p.tok_i, "sending")) {
                is_sending = true;
                _ = p.advance();
            }

            const type_expr = try p.parseType() orelse break;

            var default_value: OptionalIndex = .none;
            if (p.eatToken(.assign) != null) {
                if (try p.parseExpr()) |dv| {
                    default_value = dv.toOptional();
                }
            }

            const field_extra = try p.addExtra(Ast.Field{
                .name_token = field_name_token,
                .type_expr = type_expr,
                .default_value = default_value,
                .flags = .{ .is_sending = is_sending },
            });
            _ = field_doc; // stored as token index, not needed in Field extra
            try p.scratch.append(p.gpa, @intFromEnum(field_extra));

            if (p.eatToken(.comma) == null) break;
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }

    /// Parse comma-separated identifiers until `end_tok`. Returns SubRange of token indices.
    fn parseIdentList(p: *Parser, end_tok: Token) Error!SubRange {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != end_tok and p.currentTag() != .eof) {
            if (p.currentTag() != .ident) break;
            try p.scratch.append(p.gpa, p.advance());
            if (p.eatToken(.comma) == null) break;
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }

    /// Parse `where T: Ord, U: Hash` clause. Returns SubRange of bound token pairs.
    fn parseWhereClause(p: *Parser) Error!SubRange {
        _ = p.advance(); // consume 'where'
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() == .ident) {
            const param_token = p.advance();
            if (p.eatToken(.colon) == null) break;
            const bound_token = p.eatToken(.ident) orelse break;
            try p.scratch.append(p.gpa, param_token);
            try p.scratch.append(p.gpa, bound_token);
            if (p.eatToken(.comma) == null) break;
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }

    fn parseVarOrTypeDecl(p: *Parser, is_const: bool) Error!?Index {
        const doc = p.consumeDocComment();
        const var_token = p.advance(); // consume 'const'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // const Name = enum { ... } or const Name = error { ... }
        if (is_const and p.currentTag() == .assign) {
            const saved = p.tok_i;
            _ = p.advance(); // consume '='

            // const Name = error { Variant1, Variant2 }
            if (p.currentTag() == .kw_error and p.peekIs(.lbrace)) {
                return p.parseErrorSetDecl(var_token, name_token, doc);
            }

            // const Name = enum { ... } or const Name = enum(i32) { ... }
            if (p.currentTag() == .kw_enum) {
                const peek = p.peekTag();
                if (peek == .lbrace or peek == .colon or peek == .lparen) {
                    return p.parseEnumBody(var_token, name_token, doc);
                }
            }

            // Not a type declaration — restore position
            p.tok_i = saved;
        }

        return p.parseVarDecl2(var_token, name_token, is_const, doc);
    }

    fn parseVarDecl(p: *Parser, is_const: bool) Error!?Index {
        const doc = p.consumeDocComment();
        const var_token = p.advance();
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };
        return p.parseVarDecl2(var_token, name_token, is_const, doc);
    }

    fn parseVarDecl2(p: *Parser, var_token: TokenIndex, name_token: TokenIndex, is_const: bool, doc: OptionalTokenIndex) Error!?Index {
        var type_expr: OptionalIndex = .none;
        if (p.eatToken(.colon) != null) {
            if (try p.parseType()) |te| {
                type_expr = te.toOptional();
            }
        }

        var value: OptionalIndex = .none;
        if (p.eatToken(.assign) != null) {
            if (try p.parseExpr()) |v| {
                value = v.toOptional();
            }
        }
        _ = p.eatToken(.semicolon);

        const extra = try p.addExtra(Ast.VarDecl{
            .name_token = name_token,
            .type_expr = type_expr,
            .value = value,
            .doc_comment = doc,
            .flags = .{ .is_const = is_const },
        });

        return try p.addNode(.{
            .tag = .var_decl,
            .main_token = var_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseErrorSetDecl(p: *Parser, main_token: TokenIndex, name_token: TokenIndex, doc: OptionalTokenIndex) Error!?Index {
        _ = p.advance(); // consume 'error'
        _ = p.advance(); // consume '{'

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (p.currentTag() != .ident) break;
            try p.scratch.append(p.gpa, p.advance());
            if (p.eatToken(.comma) == null) break;
        }
        _ = p.eatToken(.rbrace);

        const variants = try p.listToSpan(p.scratch.items[scratch_top..]);
        const extra = try p.addExtra(Ast.ErrorSetDecl{
            .name_token = name_token,
            .variants = variants,
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .error_set_decl,
            .main_token = main_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseEnumBody(p: *Parser, main_token: TokenIndex, name_token: TokenIndex, doc: OptionalTokenIndex) Error!?Index {
        _ = p.advance(); // consume 'enum'

        var backing_type: OptionalIndex = .none;
        if (p.eatToken(.lparen) != null) {
            if (try p.parseType()) |bt| backing_type = bt.toOptional();
            _ = p.eatToken(.rparen);
        } else if (p.eatToken(.colon) != null) {
            if (try p.parseType()) |bt| backing_type = bt.toOptional();
        }

        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        // Set impl context for @safe self injection in enum methods
        const saved_impl = p.current_impl_type;
        const saved_generic = p.current_impl_is_generic;
        p.current_impl_type = name_token;
        p.current_impl_is_generic = false;
        defer {
            p.current_impl_type = saved_impl;
            p.current_impl_is_generic = saved_generic;
        }

        const variants_top = p.scratch.items.len;

        // Nested decls (fn, static, const)
        const decls_top = p.extra_data.items.len;
        var decl_count: u32 = 0;

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (p.isDeclKeyword()) {
                if (try p.parseDecl()) |decl_idx| {
                    try p.extra_data.append(p.gpa, @intFromEnum(decl_idx));
                    decl_count += 1;
                }
                continue;
            }
            if (p.currentTag() != .ident) break;
            const variant_name = p.advance();
            var variant_value: OptionalIndex = .none;
            if (p.eatToken(.assign) != null) {
                if (try p.parseExpr()) |v| variant_value = v.toOptional();
            }
            const ve = try p.addExtra(Ast.EnumVariant{
                .name_token = variant_name,
                .value = variant_value,
            });
            try p.scratch.append(p.gpa, @intFromEnum(ve));
            if (p.eatToken(.comma) == null) break;
        }
        _ = p.eatToken(.rbrace);

        const variants = try p.listToSpan(p.scratch.items[variants_top..]);
        p.scratch.shrinkRetainingCapacity(variants_top);

        const nested_decls: SubRange = if (decl_count > 0) .{
            .start = @enumFromInt(decls_top),
            .end = @enumFromInt(decls_top + decl_count),
        } else SubRange.empty;

        const extra = try p.addExtra(Ast.EnumDecl{
            .name_token = name_token,
            .backing_type = backing_type,
            .variants = variants,
            .nested_decls = nested_decls,
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .enum_decl,
            .main_token = main_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseStructDecl(p: *Parser, layout: Ast.StructLayout) Error!?Index {
        const doc = p.consumeDocComment();
        const struct_token = p.advance(); // consume 'struct'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // Optional type params: struct Pair(T, U) { ... }
        var type_params = SubRange.empty;
        if (p.eatToken(.lparen) != null) {
            type_params = try p.parseIdentList(.rparen);
            _ = p.eatToken(.rparen);
        }

        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        // Set impl context for @safe self injection
        const saved_impl = p.current_impl_type;
        const saved_generic = p.current_impl_is_generic;
        p.current_impl_type = name_token;
        p.current_impl_is_generic = (type_params.len() > 0);
        defer {
            p.current_impl_type = saved_impl;
            p.current_impl_is_generic = saved_generic;
        }

        // Nested decls before fields
        const decls_top = p.extra_data.items.len;
        var decl_count: u32 = 0;
        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (p.isDeclKeyword()) {
                if (try p.parseDecl()) |decl_idx| {
                    try p.extra_data.append(p.gpa, @intFromEnum(decl_idx));
                    decl_count += 1;
                }
            } else break;
        }

        // Fields
        const fields = try p.parseFieldList(.rbrace);

        // Nested decls after fields
        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (p.isDeclKeyword()) {
                if (try p.parseDecl()) |decl_idx| {
                    try p.extra_data.append(p.gpa, @intFromEnum(decl_idx));
                    decl_count += 1;
                }
            } else break;
        }
        _ = p.eatToken(.rbrace);

        const nested_decls: SubRange = if (decl_count > 0) .{
            .start = @enumFromInt(decls_top),
            .end = @enumFromInt(decls_top + decl_count),
        } else SubRange.empty;

        const extra = try p.addExtra(Ast.StructDecl{
            .name_token = name_token,
            .type_params = type_params,
            .fields = fields,
            .nested_decls = nested_decls,
            .layout = @intFromEnum(layout),
            .flags = .{},
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .struct_decl,
            .main_token = struct_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseActorDecl(p: *Parser) Error!?Index {
        const doc = p.consumeDocComment();
        const actor_token = p.advance(); // consume 'actor'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        const saved_impl = p.current_impl_type;
        const saved_generic = p.current_impl_is_generic;
        const saved_actor = p.current_impl_is_actor;
        p.current_impl_type = name_token;
        p.current_impl_is_generic = false;
        p.current_impl_is_actor = true;
        defer {
            p.current_impl_type = saved_impl;
            p.current_impl_is_generic = saved_generic;
            p.current_impl_is_actor = saved_actor;
        }

        const fields = try p.parseFieldList(.rbrace);

        // Nested decls after fields
        const decls_top = p.extra_data.items.len;
        var decl_count: u32 = 0;
        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (try p.parseDecl()) |decl_idx| {
                try p.extra_data.append(p.gpa, @intFromEnum(decl_idx));
                decl_count += 1;
            } else break;
        }
        _ = p.eatToken(.rbrace);

        const nested_decls: SubRange = if (decl_count > 0) .{
            .start = @enumFromInt(decls_top),
            .end = @enumFromInt(decls_top + decl_count),
        } else SubRange.empty;

        const extra = try p.addExtra(Ast.StructDecl{
            .name_token = name_token,
            .type_params = SubRange.empty,
            .fields = fields,
            .nested_decls = nested_decls,
            .layout = @intFromEnum(Ast.StructLayout.auto),
            .flags = .{ .is_actor = true },
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .struct_decl,
            .main_token = actor_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseUnionDecl(p: *Parser) Error!?Index {
        const doc = p.consumeDocComment();
        const union_token = p.advance(); // consume 'union'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };
        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (p.currentTag() != .ident) break;
            const variant_name = p.advance();
            var variant_type: Index = @enumFromInt(0);
            if (p.eatToken(.colon) != null) {
                variant_type = try p.parseType() orelse break;
            }
            const ve = try p.addExtra(Ast.UnionVariant{
                .name_token = variant_name,
                .type_expr = variant_type,
            });
            try p.scratch.append(p.gpa, @intFromEnum(ve));
            if (p.eatToken(.comma) == null) break;
        }
        _ = p.eatToken(.rbrace);

        const variants = try p.listToSpan(p.scratch.items[scratch_top..]);
        const extra = try p.addExtra(Ast.UnionDecl{
            .name_token = name_token,
            .variants = variants,
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .union_decl,
            .main_token = union_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseTypeAlias(p: *Parser) Error!?Index {
        const doc = p.consumeDocComment();
        const type_token = p.advance(); // consume 'type'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };
        _ = p.eatToken(.assign) orelse {
            try p.addError(.unexpected_token);
            return null;
        };

        var is_distinct = false;
        if (p.currentTag() == .kw_distinct or
            (p.currentTag() == .ident and p.isTokenText(p.tok_i, "distinct")))
        {
            is_distinct = true;
            _ = p.advance();
        }

        const target = try p.parseType() orelse {
            try p.addError(.expected_type);
            return null;
        };
        _ = p.eatToken(.semicolon);
        _ = doc;

        return try p.addNode(.{
            .tag = if (is_distinct) .type_alias_distinct else .type_alias,
            .main_token = type_token,
            .data = .{ .token_and_node = .{ name_token, target } },
        });
    }

    fn parseImportDecl(p: *Parser) Error!?Index {
        const import_token = p.advance(); // consume 'import'
        const path_token = p.eatToken(.string_lit) orelse {
            try p.addError(.expected_expression);
            return null;
        };

        return try p.addNode(.{
            .tag = .import_decl,
            .main_token = import_token,
            .data = .{ .token = path_token },
        });
    }

    fn parseImplDecl(p: *Parser) Error!?Index {
        const doc = p.consumeDocComment();
        const impl_token = p.advance(); // consume 'impl'

        // impl Trait for Type { ... } OR impl Type { ... }
        const first_name = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // Check for type params: impl(T) Type { ... }
        var type_params = SubRange.empty;
        if (p.eatToken(.lparen) != null) {
            type_params = try p.parseIdentList(.rparen);
            _ = p.eatToken(.rparen);
        }

        if (p.currentTag() == .kw_for) {
            // impl Trait for Type { methods }
            _ = p.advance(); // consume 'for'
            const target_type = p.eatToken(.ident) orelse {
                try p.addError(.expected_identifier);
                return null;
            };
            return p.parseImplBody(impl_token, first_name, target_type, type_params, doc, true);
        }

        // impl Type { methods }
        return p.parseImplBody(impl_token, first_name, first_name, type_params, doc, false);
    }

    fn parseImplBody(p: *Parser, impl_token: TokenIndex, trait_or_type: TokenIndex, type_name: TokenIndex, type_params: SubRange, doc: OptionalTokenIndex, is_trait_impl: bool) Error!?Index {
        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        const saved_impl = p.current_impl_type;
        const saved_generic = p.current_impl_is_generic;
        p.current_impl_type = type_name;
        p.current_impl_is_generic = (type_params.len() > 0);
        defer {
            p.current_impl_type = saved_impl;
            p.current_impl_is_generic = saved_generic;
        }

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (try p.parseDecl()) |decl_idx| {
                try p.scratch.append(p.gpa, @intFromEnum(decl_idx));
            } else {
                _ = p.advance(); // skip bad token
            }
        }
        _ = p.eatToken(.rbrace);

        const methods = try p.listToSpan(p.scratch.items[scratch_top..]);

        if (is_trait_impl) {
            const extra = try p.addExtra(Ast.ImplTrait{
                .trait_name_token = trait_or_type,
                .target_type_token = type_name,
                .type_params = type_params,
                .methods = methods,
                .assoc_types = SubRange.empty,
                .doc_comment = doc,
            });
            return try p.addNode(.{
                .tag = .impl_trait,
                .main_token = impl_token,
                .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
            });
        }

        const extra = try p.addExtra(Ast.ImplBlock{
            .type_name_token = type_name,
            .type_params = type_params,
            .methods = methods,
            .consts = SubRange.empty,
            .doc_comment = doc,
        });
        return try p.addNode(.{
            .tag = .impl_block,
            .main_token = impl_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseTraitDecl(p: *Parser) Error!?Index {
        const doc = p.consumeDocComment();
        const trait_token = p.advance(); // consume 'trait'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        var type_params = SubRange.empty;
        if (p.eatToken(.lparen) != null) {
            type_params = try p.parseIdentList(.rparen);
            _ = p.eatToken(.rparen);
        }

        _ = p.eatToken(.lbrace) orelse {
            try p.addError(.expected_block);
            return null;
        };

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            p.collectDocComment();
            if (try p.parseDecl()) |decl_idx| {
                try p.scratch.append(p.gpa, @intFromEnum(decl_idx));
            } else break;
        }
        _ = p.eatToken(.rbrace);

        const methods = try p.listToSpan(p.scratch.items[scratch_top..]);
        const extra = try p.addExtra(Ast.TraitDecl{
            .name_token = name_token,
            .type_params = type_params,
            .methods = methods,
            .assoc_types = SubRange.empty,
            .doc_comment = doc,
        });

        return try p.addNode(.{
            .tag = .trait_decl,
            .main_token = trait_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseTestDecl(p: *Parser) Error!?Index {
        const test_token = p.advance(); // consume 'test'
        const name_token = p.eatToken(.string_lit) orelse {
            try p.addError(.expected_expression);
            return null;
        };
        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };

        return try p.addNode(.{
            .tag = .test_decl,
            .main_token = test_token,
            .data = .{ .token_and_node = .{ name_token, body } },
        });
    }

    fn parseBenchDecl(p: *Parser) Error!?Index {
        const bench_token = p.advance();
        const name_token = p.eatToken(.string_lit) orelse {
            try p.addError(.expected_expression);
            return null;
        };
        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };

        return try p.addNode(.{
            .tag = .bench_decl,
            .main_token = bench_token,
            .data = .{ .token_and_node = .{ name_token, body } },
        });
    }

    fn parseAsyncDecl(p: *Parser) Error!?Index {
        _ = p.advance(); // consume 'async'
        // async let x = expr
        if (p.currentTag() == .kw_let or p.currentTag() == .kw_var or p.currentTag() == .kw_const) {
            return p.parseAsyncLet();
        }
        // async fn name(...) { ... }
        if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_async = true });
        try p.addError(.unexpected_token);
        return null;
    }

    fn parseAsyncLet(p: *Parser) Error!?Index {
        _ = p.advance(); // consume let/var/const
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };
        _ = p.eatToken(.assign) orelse {
            try p.addError(.unexpected_token);
            return null;
        };
        const value = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };

        return try p.addNode(.{
            .tag = .async_let,
            .main_token = name_token,
            .data = .{ .token_and_node = .{ name_token, value } },
        });
    }

    fn parseAttrDecl(p: *Parser) Error!?Index {
        _ = p.advance(); // consume '@'
        if (p.currentTag() != .ident) {
            try p.addError(.unexpected_token);
            return null;
        }
        const attr_text = p.tokenText(p.tok_i);

        if (std.mem.eql(u8, attr_text, "inlinable")) {
            _ = p.advance();
            if (p.currentTag() == .kw_fn) return p.parseFnDeclFull(.{ .is_inlinable = true });
            try p.addError(.unexpected_token);
            return null;
        }

        if (std.mem.eql(u8, attr_text, "MainActor") or std.mem.eql(u8, attr_text, "globalActor")) {
            p.pending_global_actor = p.tok_i;
            _ = p.advance();
            if (p.currentTag() == .kw_fn or p.currentTag() == .kw_async)
                return p.parseDecl();
            try p.addError(.unexpected_token);
            return null;
        }

        if (std.mem.eql(u8, attr_text, "unchecked")) {
            const main_token = p.advance();
            // @unchecked(Sendable) TypeName
            _ = p.eatToken(.lparen);
            _ = p.eatToken(.ident); // Sendable
            _ = p.eatToken(.rparen);
            const type_name = p.eatToken(.ident) orelse {
                try p.addError(.expected_identifier);
                return null;
            };
            return try p.addNode(.{
                .tag = .unchecked_sendable,
                .main_token = main_token,
                .data = .{ .token = type_name },
            });
        }

        try p.addError(.unexpected_token);
        return null;
    }


    fn parseExpr(p: *Parser) Error!?Index {
        return p.parseBinaryExpr(0);
    }

    fn parseBinaryExpr(p: *Parser, min_prec: u8) Error!?Index {
        try p.incNest();
        defer p.decNest();

        var left = try p.parseUnaryExpr() orelse return null;

        while (true) {
            // catch as low-precedence postfix (prec 1)
            if (p.currentTag() == .kw_catch and min_prec <= 1) {
                left = try p.parseCatchExpr(left);
                continue;
            }
            // orelse as low-precedence postfix (prec 1)
            if (p.currentTag() == .kw_orelse and min_prec <= 1) {
                left = try p.parseOrElseExpr(left);
                continue;
            }

            const op_tag = p.currentTag();
            const prec = op_tag.precedence();
            if (prec < min_prec or prec == 0) break;

            const op_token = p.advance();
            const right = try p.parseBinaryExpr(prec + 1) orelse {
                try p.addError(.expected_expression);
                return null;
            };

            const bin_tag: Tag = switch (op_tag) {
                .add => .binary_add,
                .sub => .binary_sub,
                .mul => .binary_mul,
                .quo => .binary_div,
                .rem => .binary_mod,
                .eql => .binary_eq,
                .neq => .binary_neq,
                .lss => .binary_lt,
                .gtr => .binary_gt,
                .leq => .binary_lte,
                .geq => .binary_gte,
                .kw_and => .binary_and,
                .kw_or => .binary_or,
                .@"and" => .binary_bit_and,
                .@"or" => .binary_bit_or,
                .xor => .binary_bit_xor,
                .shl => .binary_shl,
                .shr => .binary_shr,
                .concat => .binary_concat,
                else => break,
            };

            left = try p.addNode(.{
                .tag = bin_tag,
                .main_token = op_token,
                .data = .{ .node_and_node = .{ left, right } },
            });
        }
        return left;
    }

    fn parseCatchExpr(p: *Parser, operand: Index) Error!Index {
        const catch_token = p.advance(); // consume 'catch'
        var capture_token: OptionalTokenIndex = .none;
        var capture_is_ptr = false;
        if (p.eatToken(.@"or") != null) {
            if (p.eatToken(.mul) != null) capture_is_ptr = true;
            if (p.currentTag() == .ident) {
                capture_token = @enumFromInt(p.advance());
            }
            _ = p.eatToken(.@"or");
        }
        const fallback = try p.parseBinaryExpr(2) orelse {
            try p.addError(.expected_expression);
            return @enumFromInt(0);
        };
        const extra = try p.addExtra(Ast.CatchData{
            .capture_token = capture_token,
            .fallback = fallback,
            .flags = .{ .capture_is_ptr = capture_is_ptr },
        });
        return try p.addNode(.{
            .tag = .catch_expr,
            .main_token = catch_token,
            .data = .{ .node_and_extra = .{ operand, extra } },
        });
    }

    fn parseOrElseExpr(p: *Parser, operand: Index) Error!Index {
        const orelse_token = p.advance(); // consume 'orelse'

        var fallback_kind: Ast.OrElseFallback = .expr;
        var fallback: Index = @enumFromInt(0);

        if (p.eatToken(.kw_return) != null) {
            if (try p.parseBinaryExpr(2)) |v| {
                fallback = v;
                fallback_kind = .return_val;
            } else {
                fallback_kind = .return_void;
            }
        } else if (p.eatToken(.kw_break) != null) {
            fallback_kind = .break_val;
        } else if (p.eatToken(.kw_continue) != null) {
            fallback_kind = .continue_val;
        } else {
            fallback = try p.parseBinaryExpr(2) orelse {
                try p.addError(.expected_expression);
                return @enumFromInt(0);
            };
        }

        const extra = try p.addExtra(Ast.OrElseData{
            .fallback = fallback,
            .fallback_kind = @intFromEnum(fallback_kind),
        });
        return try p.addNode(.{
            .tag = .orelse_expr,
            .main_token = orelse_token,
            .data = .{ .node_and_extra = .{ operand, extra } },
        });
    }

    fn parseUnaryExpr(p: *Parser) Error!?Index {
        return switch (p.currentTag()) {
            .@"and" => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_addr_of, .main_token = t, .data = .{ .node = operand } });
            },
            .kw_try => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_try, .main_token = t, .data = .{ .node = operand } });
            },
            .kw_await => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_await, .main_token = t, .data = .{ .node = operand } });
            },
            .sub => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_neg, .main_token = t, .data = .{ .node = operand } });
            },
            .lnot => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_not, .main_token = t, .data = .{ .node = operand } });
            },
            .not, .kw_not => {
                const t = p.advance();
                const operand = try p.parseUnaryExpr() orelse return null;
                return try p.addNode(.{ .tag = .unary_bit_not, .main_token = t, .data = .{ .node = operand } });
            },
            else => p.parsePrimaryExpr(),
        };
    }

    fn parsePrimaryExpr(p: *Parser) Error!?Index {
        var expr = try p.parseOperand() orelse return null;

        // Postfix: .field, [idx], (args), .*, .?
        while (true) {
            if (p.eatToken(.period_star) != null) {
                expr = try p.addNode(.{ .tag = .unary_deref, .main_token = p.tok_i - 1, .data = .{ .node = expr } });
            } else if (p.eatToken(.period_question) != null) {
                expr = try p.addNode(.{ .tag = .unary_unwrap, .main_token = p.tok_i - 1, .data = .{ .node = expr } });
            } else if (p.eatToken(.period) != null) {
                if (p.currentTag() == .ident or p.currentTag() == .int_lit) {
                    const field_token = p.advance();
                    expr = try p.addNode(.{ .tag = .field_access, .main_token = field_token, .data = .{ .node_and_token = .{ expr, field_token } } });
                } else {
                    try p.addError(.expected_identifier);
                    return null;
                }
            } else if (p.eatToken(.lbrack) != null) {
                expr = try p.parseIndexOrSlice(expr);
            } else if (p.eatToken(.lparen) != null) {
                expr = try p.parseCallArgs(expr);
            } else if (p.currentTag() == .kw_as) {
                // expr as Type — postfix cast
                const as_token = p.advance();
                const type_node = try p.parseType() orelse {
                    try p.addError(.expected_type);
                    return null;
                };
                const extra = try p.addExtra(Ast.BuiltinCallData{
                    .kind = @intFromEnum(Ast.BuiltinKind.as),
                    .type_arg = type_node.toOptional(),
                    .arg0 = expr.toOptional(),
                    .arg1 = .none,
                    .arg2 = .none,
                });
                expr = try p.addNode(.{ .tag = .builtin_call, .main_token = as_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
            } else if (p.currentTag() == .lbrace) {
                // Could be: struct init, Task { body }, Task.detached { body }, or end of expression
                if (try p.tryParsePostfixBrace(expr)) |new_expr| {
                    expr = new_expr;
                    continue;
                }
                break;
            } else break;
        }
        return expr;
    }

    fn parseIndexOrSlice(p: *Parser, base: Index) Error!Index {
        const bracket_token = p.tok_i - 1;
        // [:end]
        if (p.eatToken(.colon) != null) {
            var end_node: OptionalIndex = .none;
            if (p.currentTag() != .rbrack) {
                if (try p.parseExpr()) |e| end_node = e.toOptional();
            }
            _ = p.eatToken(.rbrack);
            const extra = try p.addExtra(Ast.SliceData{ .start = .none, .end = end_node });
            return try p.addNode(.{ .tag = .slice, .main_token = bracket_token, .data = .{ .node_and_extra = .{ base, extra } } });
        }
        const idx = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            _ = p.eatToken(.rbrack);
            return @enumFromInt(0);
        };
        // [start:end]
        if (p.eatToken(.colon) != null) {
            var end_node: OptionalIndex = .none;
            if (p.currentTag() != .rbrack) {
                if (try p.parseExpr()) |e| end_node = e.toOptional();
            }
            _ = p.eatToken(.rbrack);
            const extra = try p.addExtra(Ast.SliceData{ .start = idx.toOptional(), .end = end_node });
            return try p.addNode(.{ .tag = .slice, .main_token = bracket_token, .data = .{ .node_and_extra = .{ base, extra } } });
        }
        // [index]
        _ = p.eatToken(.rbrack);
        return try p.addNode(.{ .tag = .index, .main_token = bracket_token, .data = .{ .node_and_node = .{ base, idx } } });
    }

    fn parseCallArgs(p: *Parser, callee: Index) Error!Index {
        const lparen_token = p.tok_i - 1;
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rparen and p.currentTag() != .eof) {
            if (try p.parseExpr()) |arg| {
                try p.scratch.append(p.gpa, @intFromEnum(arg));
            } else break;
            if (p.eatToken(.comma) == null) break;
        }
        _ = p.eatToken(.rparen);

        const args = p.scratch.items[scratch_top..];
        if (args.len == 0) {
            return try p.addNode(.{ .tag = .call_zero, .main_token = lparen_token, .data = .{ .node = callee } });
        }
        if (args.len == 1) {
            return try p.addNode(.{ .tag = .call_one, .main_token = lparen_token, .data = .{ .node_and_node = .{ callee, @enumFromInt(args[0]) } } });
        }
        const range = try p.listToSpan(args);
        return try p.addNode(.{ .tag = .call, .main_token = lparen_token, .data = .{ .node_and_extra = .{ callee, @enumFromInt(@intFromEnum(range.start)) } } });
    }

    fn parseOperand(p: *Parser) Error!?Index {
        return switch (p.currentTag()) {
            .ident => {
                // Labeled block: label: { break :label value }
                if (p.peekIs(.colon)) {
                    const label_token = p.advance(); // consume ident
                    _ = p.advance(); // consume colon
                    if (p.currentTag() == .lbrace) {
                        // Parse as block with label — label stored as main_token
                        return p.parseBlockStmt();
                    }
                    try p.addError(.expected_block);
                    _ = label_token;
                    return null;
                }
                const t = p.advance();
                return try p.addNode(.{ .tag = .ident, .main_token = t, .data = .{ .none = {} } });
            },
            .int_lit => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_int, .main_token = t, .data = .{ .none = {} } });
            },
            .float_lit => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_float, .main_token = t, .data = .{ .none = {} } });
            },
            .string_lit => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_string, .main_token = t, .data = .{ .none = {} } });
            },
            .char_lit => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_char, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_true => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_true, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_false => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_false, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_null => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_null, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_undefined => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_undefined, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_unreachable => {
                const t = p.advance();
                return try p.addNode(.{ .tag = .literal_unreachable, .main_token = t, .data = .{ .none = {} } });
            },
            .kw_error => {
                // error.Name
                const t = p.advance();
                _ = p.eatToken(.period);
                const name = p.eatToken(.ident) orelse {
                    try p.addError(.expected_identifier);
                    return null;
                };
                return try p.addNode(.{ .tag = .error_literal, .main_token = t, .data = .{ .token = name } });
            },
            .lparen => p.parseParenOrTuple(),
            .lbrack => p.parseArrayLiteral(),
            .lbrace => p.parseBlockExpr(),
            .kw_if => p.parseIfExpr(),
            .kw_switch => p.parseSwitchExpr(),
            .kw_comptime => {
                const t = p.advance();
                const body = try p.parseBlockStmt() orelse return null;
                return try p.addNode(.{ .tag = .comptime_block, .main_token = t, .data = .{ .node = body } });
            },
            .at => {
                // @Sendable fn(...) — closure with Sendable annotation
                if (p.peekTag() == .ident and p.isTokenText(p.tok_i + 1, "Sendable")) {
                    _ = p.advance(); // @
                    _ = p.advance(); // Sendable
                    if (p.currentTag() == .kw_fn) return p.parseSendableClosure();
                    try p.addError(.unexpected_token);
                    return null;
                }
                return p.parseBuiltinCall();
            },
            .kw_async => {
                // async fn(params) rettype { body } — async closure in expression position
                const async_token = p.advance();
                if (p.currentTag() != .kw_fn) {
                    try p.addError(.unexpected_token);
                    return null;
                }
                return p.parseClosureExprWithFlags(async_token, .{ .is_async = true });
            },
            .kw_fn => p.parseClosureExpr(),
            .kw_new => p.parseNewExpr(),
            .period => {
                const t = p.advance(); // consume .
                if (p.currentTag() == .lbrace) {
                    _ = p.advance(); // consume {
                    // .{} zero init
                    if (p.currentTag() == .rbrace) {
                        _ = p.advance();
                        return try p.addNode(.{ .tag = .zero_init, .main_token = t, .data = .{ .none = {} } });
                    }
                    // .{ .field = value, ... } anonymous struct literal
                    if (p.currentTag() == .period) {
                        const fields = try p.parseDotFields();
                        _ = p.eatToken(.rbrace);
                        const extra = try p.addExtra(Ast.StructInit{
                            .type_name_token = .none,
                            .type_args = SubRange.empty,
                            .fields = fields,
                        });
                        return try p.addNode(.{ .tag = .struct_init, .main_token = t, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                    }
                    // .{ ident: value } colon-syntax (@safe)
                    if (p.safe_mode and p.currentTag() == .ident) {
                        const fields = try p.parseColonFields();
                        _ = p.eatToken(.rbrace);
                        const extra = try p.addExtra(Ast.StructInit{
                            .type_name_token = .none,
                            .type_args = SubRange.empty,
                            .fields = fields,
                        });
                        return try p.addNode(.{ .tag = .struct_init, .main_token = t, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                    }
                    _ = p.eatToken(.rbrace);
                    return try p.addNode(.{ .tag = .zero_init, .main_token = t, .data = .{ .none = {} } });
                }
                // .field_name — enum literal shorthand
                if (p.currentTag() == .ident) {
                    const name = p.advance();
                    return try p.addNode(.{ .tag = .field_access, .main_token = t, .data = .{ .node_and_token = .{ @enumFromInt(0), name } } });
                }
                try p.addError(.expected_expression);
                return null;
            },
            .string_interp_start => p.parseStringInterp(),
            else => {
                try p.addError(.expected_expression);
                return null;
            },
        };
    }

    fn parseParenOrTuple(p: *Parser) Error!?Index {
        const lparen = p.advance(); // consume '('
        const first = try p.parseExpr() orelse {
            _ = p.eatToken(.rparen);
            return try p.addNode(.{ .tag = .paren, .main_token = lparen, .data = .{ .node = @enumFromInt(0) } });
        };
        if (p.eatToken(.rparen) != null) {
            // Single expr in parens
            return try p.addNode(.{ .tag = .paren, .main_token = lparen, .data = .{ .node = first } });
        }
        // Tuple: (a, b, ...)
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        try p.scratch.append(p.gpa, @intFromEnum(first));
        while (p.eatToken(.comma) != null) {
            if (try p.parseExpr()) |elem| {
                try p.scratch.append(p.gpa, @intFromEnum(elem));
            } else break;
        }
        _ = p.eatToken(.rparen);
        const range = try p.listToSpan(p.scratch.items[scratch_top..]);
        return try p.addNode(.{ .tag = .tuple_literal, .main_token = lparen, .data = .{ .extra_range = range } });
    }

    fn parseArrayLiteral(p: *Parser) Error!?Index {
        const lbrack = p.advance(); // consume '['
        if (p.eatToken(.rbrack) != null) {
            return try p.addNode(.{ .tag = .array_literal_empty, .main_token = lbrack, .data = .{ .none = {} } });
        }
        const first = try p.parseExpr() orelse {
            _ = p.eatToken(.rbrack);
            return try p.addNode(.{ .tag = .array_literal_empty, .main_token = lbrack, .data = .{ .none = {} } });
        };
        if (p.eatToken(.rbrack) != null) {
            return try p.addNode(.{ .tag = .array_literal_one, .main_token = lbrack, .data = .{ .node = first } });
        }
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        try p.scratch.append(p.gpa, @intFromEnum(first));
        while (p.eatToken(.comma) != null) {
            if (p.currentTag() == .rbrack) break;
            if (try p.parseExpr()) |elem| {
                try p.scratch.append(p.gpa, @intFromEnum(elem));
            } else break;
        }
        _ = p.eatToken(.rbrack);
        const range = try p.listToSpan(p.scratch.items[scratch_top..]);
        return try p.addNode(.{ .tag = .array_literal, .main_token = lbrack, .data = .{ .extra_range = range } });
    }

    fn parseBlockExpr(p: *Parser) Error!?Index {
        return p.parseBlockStmt();
    }

    fn parseIfExpr(p: *Parser) Error!?Index {
        const if_token = p.advance();
        _ = p.eatToken(.lparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };
        const cond = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.rparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };

        var capture_token: OptionalTokenIndex = .none;
        var capture_is_ptr = false;
        if (p.eatToken(.@"or") != null) {
            if (p.eatToken(.mul) != null) capture_is_ptr = true;
            if (p.currentTag() == .ident) {
                capture_token = @enumFromInt(p.advance());
            }
            _ = p.eatToken(.@"or");
        }

        const then_branch = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };
        if (p.eatToken(.kw_else) == null) {
            if (capture_token == .none) {
                return try p.addNode(.{ .tag = .if_simple, .main_token = if_token, .data = .{ .node_and_node = .{ cond, then_branch } } });
            }
            const extra = try p.addExtra(Ast.IfData{ .then_node = then_branch, .else_node = .none, .capture_token = capture_token, .flags = .{ .capture_is_ptr = capture_is_ptr } });
            return try p.addNode(.{ .tag = .if_full, .main_token = if_token, .data = .{ .node_and_extra = .{ cond, extra } } });
        }
        const else_branch = if (p.currentTag() == .kw_if)
            try p.parseIfExpr() orelse return null
        else
            try p.parseBlockStmt() orelse return null;

        const extra = try p.addExtra(Ast.IfData{ .then_node = then_branch, .else_node = else_branch.toOptional(), .capture_token = capture_token, .flags = .{ .capture_is_ptr = capture_is_ptr } });
        return try p.addNode(.{ .tag = .if_full, .main_token = if_token, .data = .{ .node_and_extra = .{ cond, extra } } });
    }

    fn parseSwitchExpr(p: *Parser) Error!?Index {
        const switch_token = p.advance(); // consume 'switch'
        const subject = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.lbrace);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        var else_body: OptionalIndex = .none;
        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (p.eatToken(.kw_else) != null) {
                _ = p.eatToken(.fat_arrow);
                if (try p.parseExpr()) |eb| else_body = eb.toOptional();
                _ = p.eatToken(.comma);
                continue;
            }
            // Wildcard _ => treat as else
            if (p.currentTag() == .ident and p.isTokenText(p.tok_i, "_")) {
                _ = p.advance();
                _ = p.eatToken(.fat_arrow);
                if (try p.parseExpr()) |eb| else_body = eb.toOptional();
                _ = p.eatToken(.comma);
                continue;
            }
            if (try p.parseSwitchCase()) |case_extra| {
                try p.scratch.append(p.gpa, @intFromEnum(case_extra));
            } else break;
        }
        _ = p.eatToken(.rbrace);

        const cases = try p.listToSpan(p.scratch.items[scratch_top..]);
        const extra = try p.addExtra(Ast.SwitchData{
            .cases = cases,
            .else_body = else_body,
        });
        return try p.addNode(.{ .tag = .switch_expr, .main_token = switch_token, .data = .{ .node_and_extra = .{ subject, extra } } });
    }

    fn parseSwitchCase(p: *Parser) Error!?ExtraIndex {
        const patterns_top = p.scratch.items.len;

        // Wildcard _ => treat as else (handled by caller checking kw_else)
        if (p.currentTag() == .ident and p.isTokenText(p.tok_i, "_")) {
            // Return null to signal caller to treat as else
            return null;
        }

        var is_range = false;
        const first = try p.parsePrimaryExpr() orelse return null;
        try p.scratch.append(p.gpa, @intFromEnum(first));

        // Range pattern: 1..5
        if (p.eatToken(.period_period) != null) {
            const range_end = try p.parsePrimaryExpr() orelse return null;
            try p.scratch.append(p.gpa, @intFromEnum(range_end));
            is_range = true;
        } else {
            // Multiple patterns: a, b, c =>
            while (p.currentTag() == .comma and p.peekTag() != .fat_arrow) {
                _ = p.advance();
                if (p.currentTag() == .fat_arrow or p.currentTag() == .kw_else) break;
                if (try p.parsePrimaryExpr()) |pat| {
                    try p.scratch.append(p.gpa, @intFromEnum(pat));
                } else break;
            }
        }

        const patterns = try p.listToSpan(p.scratch.items[patterns_top..]);
        p.scratch.shrinkRetainingCapacity(patterns_top);

        // Capture: |val| or |*val|
        var capture_token: OptionalTokenIndex = .none;
        var capture_is_ptr = false;
        if (p.eatToken(.@"or") != null) {
            if (p.eatToken(.mul) != null) capture_is_ptr = true;
            if (p.currentTag() == .ident) capture_token = @enumFromInt(p.advance());
            _ = p.eatToken(.@"or");
        }

        // Guard: pattern if expr =>
        var guard: OptionalIndex = .none;
        if (p.eatToken(.kw_if) != null) {
            if (try p.parseExpr()) |g| guard = g.toOptional();
        }

        _ = p.eatToken(.fat_arrow);
        const body = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.comma);

        return try p.addExtra(Ast.SwitchCaseData{
            .patterns = patterns,
            .guard = guard,
            .capture_token = capture_token,
            .body = body,
            .flags = .{ .is_range = is_range, .capture_is_ptr = capture_is_ptr },
        });
    }

    fn parseBuiltinCall(p: *Parser) Error!?Index {
        const at_token = p.advance(); // consume '@'
        if (p.currentTag() != .ident) {
            try p.addError(.expected_identifier);
            return null;
        }
        const name_text = p.tokenText(p.tok_i);
        const kind = Ast.BuiltinKind.fromString(name_text) orelse {
            try p.addError(.unexpected_token);
            return null;
        };
        _ = p.advance(); // consume builtin name

        _ = p.eatToken(.lparen);
        const type_arg: OptionalIndex = .none;
        var args: [3]OptionalIndex = .{ .none, .none, .none };
        var arg_count: usize = 0;

        while (p.currentTag() != .rparen and p.currentTag() != .eof and arg_count < 4) {
            if (try p.parseExpr()) |arg| {
                if (arg_count == 0 and p.currentTag() == .rparen and type_arg == .none) {
                    // Could be type arg or regular arg — treat as type_arg if single capitalized ident
                    args[0] = arg.toOptional();
                    arg_count = 1;
                } else if (arg_count < 3) {
                    args[arg_count] = arg.toOptional();
                    arg_count += 1;
                }
            } else break;
            if (p.eatToken(.comma) == null) break;
        }
        _ = p.eatToken(.rparen);

        const extra = try p.addExtra(Ast.BuiltinCallData{
            .kind = @intFromEnum(kind),
            .type_arg = type_arg,
            .arg0 = args[0],
            .arg1 = args[1],
            .arg2 = args[2],
        });
        return try p.addNode(.{ .tag = .builtin_call, .main_token = at_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    fn parseSendableClosure(p: *Parser) Error!?Index {
        return p.parseClosureExprWithFlags(p.tok_i, .{ .is_sendable = true });
    }

    fn parseClosureExprWithFlags(p: *Parser, main_token: TokenIndex, flags: Ast.ClosureFlags) Error!?Index {
        _ = p.advance(); // consume 'fn'
        _ = p.eatToken(.lparen);
        const params = try p.parseFieldList(.rparen);
        _ = p.eatToken(.rparen);
        var return_type: OptionalIndex = .none;
        if (p.currentTag() != .lbrace and p.currentTag() != .eof) {
            if (try p.parseType()) |rt| return_type = rt.toOptional();
        }
        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };
        const extra = try p.addExtra(Ast.ClosureData{
            .params = params,
            .return_type = return_type,
            .body = body,
            .flags = flags,
        });
        return try p.addNode(.{ .tag = .closure_expr, .main_token = main_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    fn parseClosureExpr(p: *Parser) Error!?Index {
        const fn_token = p.advance(); // consume 'fn'
        _ = p.eatToken(.lparen);
        const params = try p.parseFieldList(.rparen);
        _ = p.eatToken(.rparen);

        var return_type: OptionalIndex = .none;
        if (p.currentTag() != .lbrace and p.currentTag() != .eof) {
            if (try p.parseType()) |rt| return_type = rt.toOptional();
        }
        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };
        const extra = try p.addExtra(Ast.ClosureData{
            .params = params,
            .return_type = return_type,
            .body = body,
            .flags = .{},
        });
        return try p.addNode(.{ .tag = .closure_expr, .main_token = fn_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    fn parseNewExpr(p: *Parser) Error!?Index {
        const new_token = p.advance(); // consume 'new'
        const type_name = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // Skip type args and fields/constructor args for now
        var type_args = SubRange.empty;
        if (p.eatToken(.lparen) != null) {
            // Could be type args or constructor args
            const scratch_top = p.scratch.items.len;
            defer p.scratch.shrinkRetainingCapacity(scratch_top);
            while (p.currentTag() != .rparen and p.currentTag() != .eof) {
                if (try p.parseExpr()) |arg| {
                    try p.scratch.append(p.gpa, @intFromEnum(arg));
                } else break;
                if (p.eatToken(.comma) == null) break;
            }
            _ = p.eatToken(.rparen);
            type_args = try p.listToSpan(p.scratch.items[scratch_top..]);
        }

        // Fields in braces
        var fields = SubRange.empty;
        if (p.eatToken(.lbrace) != null) {
            fields = try p.parseFieldList(.rbrace);
            _ = p.eatToken(.rbrace);
        }

        const extra = try p.addExtra(Ast.NewExprData{
            .type_name_token = type_name,
            .type_args = type_args,
            .fields = fields,
            .constructor_args = SubRange.empty,
            .flags = .{},
        });
        return try p.addNode(.{ .tag = .new_expr, .main_token = new_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    fn parseStringInterp(p: *Parser) Error!?Index {
        const start_token = p.advance(); // consume string_interp_start
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        // Text segment from start token
        try p.scratch.append(p.gpa, Ast.SEGMENT_TEXT);
        try p.scratch.append(p.gpa, start_token);

        // Expression between ${ }
        if (try p.parseExpr()) |expr| {
            try p.scratch.append(p.gpa, Ast.SEGMENT_EXPR);
            try p.scratch.append(p.gpa, @intFromEnum(expr));
        }

        // Mid/end segments
        while (p.currentTag() != .eof) {
            if (p.currentTag() == .string_interp_mid) {
                const mid = p.advance();
                try p.scratch.append(p.gpa, Ast.SEGMENT_TEXT);
                try p.scratch.append(p.gpa, mid);
                if (try p.parseExpr()) |expr| {
                    try p.scratch.append(p.gpa, Ast.SEGMENT_EXPR);
                    try p.scratch.append(p.gpa, @intFromEnum(expr));
                }
            } else if (p.currentTag() == .string_interp_end) {
                const end = p.advance();
                try p.scratch.append(p.gpa, Ast.SEGMENT_TEXT);
                try p.scratch.append(p.gpa, end);
                break;
            } else break;
        }

        const range = try p.listToSpan(p.scratch.items[scratch_top..]);
        return try p.addNode(.{ .tag = .string_interp, .main_token = start_token, .data = .{ .extra_range = range } });
    }


    fn parseType(p: *Parser) Error!?Index {
        if (p.eatToken(.question) != null) {
            const inner = try p.parseType() orelse return null;
            return try p.addNode(.{ .tag = .type_optional, .main_token = p.tok_i - 1, .data = .{ .node = inner } });
        }
        if (p.eatToken(.mul) != null) {
            const inner = try p.parseType() orelse return null;
            return try p.addNode(.{ .tag = .type_pointer, .main_token = p.tok_i - 1, .data = .{ .node = inner } });
        }
        if (p.eatToken(.lnot) != null) {
            if (p.currentTag() == .ident or p.currentTag().isTypeKeyword()) {
                // Could be ErrorSet!T
                const maybe_error_set = p.tok_i;
                if (p.peekIs(.lnot)) {
                    // ErrorSet!T
                    const error_set = try p.addNode(.{ .tag = .type_named, .main_token = p.advance(), .data = .{ .none = {} } });
                    _ = p.advance(); // consume second !
                    const inner = try p.parseType() orelse return null;
                    return try p.addNode(.{ .tag = .type_error_union_set, .main_token = maybe_error_set, .data = .{ .node_and_node = .{ error_set, inner } } });
                }
            }
            const inner = try p.parseType() orelse return null;
            return try p.addNode(.{ .tag = .type_error_union, .main_token = p.tok_i - 1, .data = .{ .node = inner } });
        }
        // []T slice
        if (p.currentTag() == .lbrack) {
            const t = p.advance();
            if (p.eatToken(.rbrack) != null) {
                const elem = try p.parseType() orelse return null;
                return try p.addNode(.{ .tag = .type_slice, .main_token = t, .data = .{ .node = elem } });
            }
            // [N]T array
            const size = try p.parseExpr() orelse return null;
            _ = p.eatToken(.rbrack);
            const elem = try p.parseType() orelse return null;
            return try p.addNode(.{ .tag = .type_array, .main_token = t, .data = .{ .node_and_node = .{ size, elem } } });
        }
        // any Trait (existential)
        if (p.currentTag() == .ident and p.isTokenText(p.tok_i, "any")) {
            const t = p.advance();
            const trait = try p.parseType() orelse return null;
            return try p.addNode(.{ .tag = .type_existential, .main_token = t, .data = .{ .node = trait } });
        }
        // Named type, possibly generic: Type(T, U)
        if (p.currentTag() == .ident or p.currentTag().isTypeKeyword()) {
            const name_token = p.advance();
            // Check for generic args: Type(T, U)
            if (p.eatToken(.lparen) != null) {
                const scratch_top = p.scratch.items.len;
                defer p.scratch.shrinkRetainingCapacity(scratch_top);
                while (p.currentTag() != .rparen and p.currentTag() != .eof) {
                    if (try p.parseType()) |arg| {
                        try p.scratch.append(p.gpa, @intFromEnum(arg));
                    } else break;
                    if (p.eatToken(.comma) == null) break;
                }
                _ = p.eatToken(.rparen);
                const type_args = try p.listToSpan(p.scratch.items[scratch_top..]);
                const extra = try p.addExtra(Ast.GenericInstance{
                    .name_token = name_token,
                    .type_args = type_args,
                });
                return try p.addNode(.{ .tag = .type_generic, .main_token = name_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
            }
            return try p.addNode(.{ .tag = .type_named, .main_token = name_token, .data = .{ .none = {} } });
        }
        // fn(A, B) -> R
        if (p.currentTag() == .kw_fn) {
            const fn_token = p.advance();
            _ = p.eatToken(.lparen);
            const scratch_top = p.scratch.items.len;
            defer p.scratch.shrinkRetainingCapacity(scratch_top);
            while (p.currentTag() != .rparen and p.currentTag() != .eof) {
                if (try p.parseType()) |param| {
                    try p.scratch.append(p.gpa, @intFromEnum(param));
                } else break;
                if (p.eatToken(.comma) == null) break;
            }
            _ = p.eatToken(.rparen);
            var ret: OptionalIndex = .none;
            if (p.currentTag() != .lbrace and p.currentTag() != .eof and p.currentTag() != .rparen and p.currentTag() != .comma) {
                if (try p.parseType()) |r| ret = r.toOptional();
            }
            const fn_params = try p.listToSpan(p.scratch.items[scratch_top..]);
            const extra = try p.addExtra(Ast.FnType{ .params = fn_params, .ret = ret });
            return try p.addNode(.{ .tag = .type_function, .main_token = fn_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
        }
        try p.addError(.expected_type);
        return null;
    }

    fn parseStmt(p: *Parser) Error!?Index {
        return switch (p.currentTag()) {
            .kw_return => p.parseReturnStmt(),
            .kw_var, .kw_let => p.parseLocalVarFull(false, false, false),
            .kw_const => p.parseLocalVarFull(true, false, false),
            .kw_weak => {
                _ = p.advance();
                if (p.currentTag() == .kw_var or p.currentTag() == .kw_let)
                    return p.parseLocalVarFull(false, true, false);
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_unowned => {
                _ = p.advance();
                if (p.currentTag() == .kw_var or p.currentTag() == .kw_let)
                    return p.parseLocalVarFull(false, false, true);
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_if => p.parseIfStmt(),
            .kw_while => p.parseWhileStmtFull(null),
            .kw_for => p.parseForStmtFull(null, false),
            .kw_inline => {
                _ = p.advance();
                if (p.currentTag() == .kw_for) return p.parseForStmtFull(null, true);
                try p.addError(.unexpected_token);
                return null;
            },
            .kw_break => p.parseBreakStmt(),
            .kw_continue => p.parseContinueStmt(),
            .kw_defer => {
                const t = p.advance();
                const expr = try p.parseDeferBody() orelse return null;
                return try p.addNode(.{ .tag = .defer_stmt, .main_token = t, .data = .{ .node = expr } });
            },
            .kw_errdefer => {
                const t = p.advance();
                const expr = try p.parseDeferBody() orelse return null;
                return try p.addNode(.{ .tag = .errdefer_stmt, .main_token = t, .data = .{ .node = expr } });
            },
            // Block-scoped declarations
            .kw_fn => p.parseFnDeclFull(.{}),
            .kw_async => p.parseAsyncDecl(),
            .kw_struct => p.parseStructDecl(.auto),
            .kw_actor => p.parseActorDecl(),
            .kw_union => p.parseUnionDecl(),
            .kw_packed => {
                if (p.peekTag() == .kw_struct) {
                    _ = p.advance();
                    return p.parseStructDecl(.@"packed");
                }
                return p.parseExprOrAssign();
            },
            .at => {
                if (p.peekTag() == .ident) {
                    const peek_text = p.tokenText(p.tok_i + 1);
                    if (std.mem.eql(u8, peek_text, "MainActor") or
                        std.mem.eql(u8, peek_text, "inlinable") or
                        std.mem.eql(u8, peek_text, "globalActor"))
                        return p.parseDecl();
                }
                return p.parseExprOrAssign();
            },
            .ident => {
                // Labeled while/for: label: while/for
                if (p.peekIs(.colon)) {
                    const label_token = p.advance();
                    _ = p.advance(); // consume :
                    if (p.currentTag() == .kw_while) return p.parseWhileStmtFull(label_token);
                    if (p.currentTag() == .kw_for) return p.parseForStmtFull(label_token, false);
                    // Not while/for — fall through to labeled block in parseExprOrAssign
                    p.tok_i -= 2; // back up
                    return p.parseExprOrAssign();
                }
                return p.parseExprOrAssign();
            },
            else => p.parseExprOrAssign(),
        };
    }

    fn parseReturnStmt(p: *Parser) Error!?Index {
        const ret_token = p.advance(); // consume 'return'
        if (p.currentTag() == .semicolon or p.currentTag() == .rbrace or p.currentTag() == .eof) {
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = .return_void, .main_token = ret_token, .data = .{ .none = {} } });
        }
        const value = try p.parseExpr() orelse {
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = .return_void, .main_token = ret_token, .data = .{ .none = {} } });
        };
        _ = p.eatToken(.semicolon);
        return try p.addNode(.{ .tag = .return_expr, .main_token = ret_token, .data = .{ .node = value } });
    }

    fn parseLocalVarFull(p: *Parser, is_const: bool, is_weak: bool, is_unowned: bool) Error!?Index {
        const var_token = p.advance();
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };
        var type_expr: OptionalIndex = .none;
        if (p.eatToken(.colon) != null) {
            if (try p.parseType()) |te| type_expr = te.toOptional();
        }

        // Destructuring: const a, b = expr
        if (p.currentTag() == .comma) {
            return p.parseDestructure(var_token, name_token, type_expr, is_const);
        }

        var value: OptionalIndex = .none;
        if (p.eatToken(.assign) != null) {
            if (try p.parseExpr()) |v| value = v.toOptional();
        }
        _ = p.eatToken(.semicolon);
        const extra = try p.addExtra(Ast.LocalVarData{
            .name_token = name_token,
            .type_expr = type_expr,
            .value = value,
            .flags = .{ .is_weak = is_weak, .is_unowned = is_unowned },
        });
        return try p.addNode(.{
            .tag = if (is_const) .const_local else .var_local,
            .main_token = var_token,
            .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
        });
    }

    fn parseDestructure(p: *Parser, var_token: TokenIndex, first_name: TokenIndex, first_type: OptionalIndex, is_const: bool) Error!?Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        // First binding already parsed
        try p.scratch.append(p.gpa, first_name);
        try p.scratch.append(p.gpa, @intFromEnum(first_type));

        while (p.eatToken(.comma) != null) {
            const bname = p.eatToken(.ident) orelse {
                try p.addError(.expected_identifier);
                return null;
            };
            var btype: OptionalIndex = .none;
            if (p.eatToken(.colon) != null) {
                if (try p.parseType()) |t| btype = t.toOptional();
            }
            try p.scratch.append(p.gpa, bname);
            try p.scratch.append(p.gpa, @intFromEnum(btype));
        }

        _ = p.eatToken(.assign) orelse {
            try p.addError(.unexpected_token);
            return null;
        };
        const value = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.semicolon);

        const bindings = try p.listToSpan(p.scratch.items[scratch_top..]);
        const extra = try p.addExtra(Ast.DestructureData{
            .bindings = bindings,
            .value = value,
            .flags = .{ .is_const = is_const },
        });
        return try p.addNode(.{ .tag = .destructure, .main_token = var_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    /// Parse the body of defer/errdefer — block or expression (possibly assignment).
    fn parseDeferBody(p: *Parser) Error!?Index {
        if (p.currentTag() == .lbrace)
            return p.parseBlockStmt();
        const expr = try p.parseExpr() orelse return null;
        if (p.currentTag() == .assign or p.currentTag().isAssignment()) {
            const op_token = p.advance();
            const rhs = try p.parseExpr() orelse return null;
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = .assign, .main_token = op_token, .data = .{ .node_and_node = .{ expr, rhs } } });
        }
        _ = p.eatToken(.semicolon);
        return expr;
    }

    fn parseIfStmt(p: *Parser) Error!?Index {
        const if_token = p.advance();
        _ = p.eatToken(.lparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };
        const cond = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.rparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };

        // Optional capture: |val| or |*val|
        var capture_token: OptionalTokenIndex = .none;
        var capture_is_ptr = false;
        if (p.eatToken(.@"or") != null) {
            if (p.eatToken(.mul) != null) capture_is_ptr = true;
            if (p.currentTag() == .ident) {
                capture_token = @enumFromInt(p.advance());
            } else {
                try p.addError(.expected_identifier);
                return null;
            }
            _ = p.eatToken(.@"or");
        }

        const then_branch = try p.parseBlockStmt() orelse try p.parseStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };
        if (p.eatToken(.kw_else) == null) {
            if (capture_token == .none) {
                return try p.addNode(.{ .tag = .if_stmt_simple, .main_token = if_token, .data = .{ .node_and_node = .{ cond, then_branch } } });
            }
            const extra = try p.addExtra(Ast.IfData{ .then_node = then_branch, .else_node = .none, .capture_token = capture_token, .flags = .{ .capture_is_ptr = capture_is_ptr } });
            return try p.addNode(.{ .tag = .if_stmt, .main_token = if_token, .data = .{ .node_and_extra = .{ cond, extra } } });
        }
        const else_branch = if (p.currentTag() == .kw_if)
            try p.parseIfStmt() orelse return null
        else
            try p.parseBlockStmt() orelse try p.parseStmt() orelse return null;

        const extra = try p.addExtra(Ast.IfData{ .then_node = then_branch, .else_node = else_branch.toOptional(), .capture_token = capture_token, .flags = .{ .capture_is_ptr = capture_is_ptr } });
        return try p.addNode(.{ .tag = .if_stmt, .main_token = if_token, .data = .{ .node_and_extra = .{ cond, extra } } });
    }

    fn parseWhileStmtFull(p: *Parser, label: ?TokenIndex) Error!?Index {
        const while_token = p.advance();
        _ = p.eatToken(.lparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };
        const cond = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };
        _ = p.eatToken(.rparen) orelse {
            try p.addError(.expected_rparen);
            return null;
        };

        // Optional capture: |val| or |*val|
        var capture_token: OptionalTokenIndex = .none;
        var capture_is_ptr = false;
        if (p.eatToken(.@"or") != null) {
            if (p.eatToken(.mul) != null) capture_is_ptr = true;
            if (p.currentTag() == .ident) {
                capture_token = @enumFromInt(p.advance());
            } else {
                try p.addError(.expected_identifier);
                return null;
            }
            _ = p.eatToken(.@"or");
        }

        // Continue expression: : (expr)
        var continue_expr: OptionalIndex = .none;
        if (p.eatToken(.colon) != null) {
            _ = p.eatToken(.lparen);
            if (try p.parseExprOrAssignInner()) |ce| continue_expr = ce.toOptional();
            _ = p.eatToken(.rparen);
        }

        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };
        const label_tok: OptionalTokenIndex = if (label) |l| @enumFromInt(l) else .none;
        const extra = try p.addExtra(Ast.WhileData{
            .body = body,
            .label_token = label_tok,
            .capture_token = capture_token,
            .continue_expr = continue_expr,
            .flags = .{ .capture_is_ptr = capture_is_ptr },
        });
        return try p.addNode(.{ .tag = .while_stmt, .main_token = while_token, .data = .{ .node_and_extra = .{ cond, extra } } });
    }

    fn parseForStmtFull(p: *Parser, label: ?TokenIndex, is_inline: bool) Error!?Index {
        const for_token = p.advance();

        // for await val in seq { body } — desugar to while
        if (p.eatToken(.kw_await) != null) {
            const binding = p.eatToken(.ident) orelse {
                try p.addError(.expected_identifier);
                return null;
            };
            _ = p.eatToken(.kw_in);
            const sequence = try p.parseExpr() orelse return null;

            // Synthesize seq.next() call
            const field = try p.addNode(.{ .tag = .field_access, .main_token = p.tok_i, .data = .{ .node_and_token = .{ sequence, p.tok_i } } });
            const next_call = try p.addNode(.{ .tag = .call_zero, .main_token = p.tok_i, .data = .{ .node = field } });

            const body = try p.parseBlockStmt() orelse {
                try p.addError(.expected_block);
                return null;
            };
            const label_tok: OptionalTokenIndex = if (label) |l| @enumFromInt(l) else .none;
            const extra = try p.addExtra(Ast.WhileData{
                .body = body,
                .label_token = label_tok,
                .capture_token = @enumFromInt(binding),
                .continue_expr = .none,
                .flags = .{},
            });
            return try p.addNode(.{ .tag = .while_stmt, .main_token = for_token, .data = .{ .node_and_extra = .{ next_call, extra } } });
        }

        const first_binding = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // for i, item in collection — indexed iteration
        var index_binding: OptionalTokenIndex = .none;
        var value_binding: TokenIndex = first_binding;
        if (p.eatToken(.comma) != null) {
            if (p.currentTag() == .ident) {
                index_binding = @enumFromInt(first_binding);
                value_binding = p.advance();
            }
        }

        _ = p.eatToken(.kw_in) orelse {
            try p.addError(.unexpected_token);
            return null;
        };

        const iter_or_start = try p.parseExpr() orelse {
            try p.addError(.expected_expression);
            return null;
        };

        // Range syntax: for i in start..end
        var range_start: OptionalIndex = .none;
        var range_end: OptionalIndex = .none;
        var iterable: OptionalIndex = iter_or_start.toOptional();

        if (p.eatToken(.period_period) != null) {
            range_start = iter_or_start.toOptional();
            if (try p.parseExpr()) |re| range_end = re.toOptional();
            iterable = .none;
        }

        const body = try p.parseBlockStmt() orelse {
            try p.addError(.expected_block);
            return null;
        };

        const label_tok: OptionalTokenIndex = if (label) |l| @enumFromInt(l) else .none;
        const extra = try p.addExtra(Ast.ForData{
            .binding_token = value_binding,
            .index_binding_token = index_binding,
            .iterable = iterable,
            .range_start = range_start,
            .range_end = range_end,
            .body = body,
            .label_token = label_tok,
            .flags = .{ .is_inline = is_inline },
        });
        return try p.addNode(.{ .tag = .for_stmt, .main_token = for_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    /// Parse expression, optionally followed by assignment. Returns the node.
    fn parseExprOrAssignInner(p: *Parser) Error!?Index {
        const expr = try p.parseExpr() orelse return null;
        const assign_tag: ?Tag = switch (p.currentTag()) {
            .assign => .assign,
            .add_assign => .assign_add,
            .sub_assign => .assign_sub,
            .mul_assign => .assign_mul,
            .quo_assign => .assign_div,
            .rem_assign => .assign_mod,
            .and_assign => .assign_bit_and,
            .or_assign => .assign_bit_or,
            .xor_assign => .assign_bit_xor,
            else => null,
        };
        if (assign_tag) |tag| {
            const op_token = p.advance();
            const value = try p.parseExpr() orelse return null;
            return try p.addNode(.{ .tag = tag, .main_token = op_token, .data = .{ .node_and_node = .{ expr, value } } });
        }
        return expr;
    }

    fn parseBreakStmt(p: *Parser) Error!?Index {
        const break_token = p.advance();
        if (p.currentTag() == .semicolon or p.currentTag() == .rbrace or p.currentTag() == .eof) {
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = .break_plain, .main_token = break_token, .data = .{ .none = {} } });
        }
        var label: OptionalTokenIndex = .none;
        if (p.eatToken(.colon) != null) {
            if (p.currentTag() == .ident) label = @enumFromInt(p.advance());
        }
        var value: OptionalIndex = .none;
        if (p.currentTag() != .semicolon and p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (try p.parseExpr()) |v| value = v.toOptional();
        }
        _ = p.eatToken(.semicolon);
        const extra = try p.addExtra(Ast.BreakData{ .label_token = label, .value = value });
        return try p.addNode(.{ .tag = .break_expr, .main_token = break_token, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
    }

    fn parseContinueStmt(p: *Parser) Error!?Index {
        const cont_token = p.advance();
        if (p.eatToken(.colon) != null and p.currentTag() == .ident) {
            const label = p.advance();
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = .continue_labeled, .main_token = cont_token, .data = .{ .token = label } });
        }
        _ = p.eatToken(.semicolon);
        return try p.addNode(.{ .tag = .continue_plain, .main_token = cont_token, .data = .{ .none = {} } });
    }

    fn parseExprOrAssign(p: *Parser) Error!?Index {
        const expr = try p.parseExpr() orelse return null;
        // Check for assignment operators
        const assign_tag: ?Tag = switch (p.currentTag()) {
            .assign => .assign,
            .add_assign => .assign_add,
            .sub_assign => .assign_sub,
            .mul_assign => .assign_mul,
            .quo_assign => .assign_div,
            .rem_assign => .assign_mod,
            .and_assign => .assign_bit_and,
            .or_assign => .assign_bit_or,
            .xor_assign => .assign_bit_xor,
            else => null,
        };
        if (assign_tag) |tag| {
            const op_token = p.advance();
            const value = try p.parseExpr() orelse {
                try p.addError(.expected_expression);
                return null;
            };
            _ = p.eatToken(.semicolon);
            return try p.addNode(.{ .tag = tag, .main_token = op_token, .data = .{ .node_and_node = .{ expr, value } } });
        }
        _ = p.eatToken(.semicolon);
        return try p.addNode(.{ .tag = .expr_stmt, .main_token = p.nodeMainToken(expr), .data = .{ .node = expr } });
    }

    fn parseBlockStmt(p: *Parser) Error!?Index {
        if (p.currentTag() != .lbrace) return null;
        const lbrace = p.advance();

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (try p.parseStmt()) |stmt| {
                try p.scratch.append(p.gpa, @intFromEnum(stmt));
            } else {
                _ = p.advance(); // skip bad token for error recovery
            }
        }
        _ = p.eatToken(.rbrace);

        const stmts = p.scratch.items[scratch_top..];
        if (stmts.len == 0) {
            return try p.addNode(.{ .tag = .block_one, .main_token = lbrace, .data = .{ .node_and_node = .{ @enumFromInt(0), @enumFromInt(0) } } });
        }
        if (stmts.len == 1) {
            return try p.addNode(.{ .tag = .block_one, .main_token = lbrace, .data = .{ .node_and_node = .{ @enumFromInt(stmts[0]), @enumFromInt(0) } } });
        }
        if (stmts.len == 2) {
            return try p.addNode(.{ .tag = .block_two, .main_token = lbrace, .data = .{ .node_and_node = .{ @enumFromInt(stmts[0]), @enumFromInt(stmts[1]) } } });
        }
        const range = try p.listToSpan(stmts);
        return try p.addNode(.{ .tag = .block_stmt, .main_token = lbrace, .data = .{ .extra_range = range } });
    }

    fn nodeMainToken(p: *const Parser, node: Index) TokenIndex {
        return p.nodes.items(.main_token)[@intFromEnum(node)];
    }



    /// With pre-tokenized input, lookahead is trivial — just index into the array.
    fn peekNextIsPeriod(p: *const Parser) bool {
        return p.currentTag() == .lbrace and p.peekTag() == .period;
    }

    /// Check if current is `{ ident :` or `{ ident ,` or `{ ident }` — colon-syntax struct init.
    fn peekNextIsIdentColon(p: *const Parser) bool {
        if (p.currentTag() != .lbrace) return false;
        if (p.tok_i + 2 >= p.token_tags.len) return false;
        if (p.token_tags[p.tok_i + 1] != .ident) return false;
        const tok3 = p.token_tags[p.tok_i + 2];
        return tok3 == .colon or tok3 == .comma or tok3 == .rbrace;
    }

    /// Parse colon-syntax struct fields (@safe): `{ x: 10, y: 20 }` or shorthand `{ x, y }`
    fn parseColonFields(p: *Parser) Error!SubRange {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            if (p.currentTag() != .ident) break;
            const name_token = p.advance();
            var value: Index = undefined;
            if (p.eatToken(.colon) != null) {
                value = try p.parseExpr() orelse break;
            } else {
                // Field shorthand: `{ x }` -> `{ x: x }`
                value = try p.addNode(.{ .tag = .ident, .main_token = name_token, .data = .{ .none = {} } });
            }
            // Store as pairs: (name_token, value_node)
            try p.scratch.append(p.gpa, name_token);
            try p.scratch.append(p.gpa, @intFromEnum(value));
            if (p.eatToken(.comma) == null) break;
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }

    /// Parse `.field = value` struct init fields.
    fn parseDotFields(p: *Parser) Error!SubRange {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (p.currentTag() != .rbrace and p.currentTag() != .eof) {
            _ = p.eatToken(.period) orelse break;
            const name_token = p.eatToken(.ident) orelse {
                try p.addError(.expected_identifier);
                break;
            };
            _ = p.eatToken(.assign) orelse {
                try p.addError(.unexpected_token);
                break;
            };
            const value = try p.parseExpr() orelse break;
            try p.scratch.append(p.gpa, name_token);
            try p.scratch.append(p.gpa, @intFromEnum(value));
            if (p.eatToken(.comma) == null) break;
        }

        return p.listToSpan(p.scratch.items[scratch_top..]);
    }

    /// Try to parse a postfix brace as struct init, Task creation, or generic struct init.
    /// Returns null if the brace should not be consumed (end of expression).
    fn tryParsePostfixBrace(p: *Parser, expr: Index) Error!?Index {
        const expr_tag = p.nodes.items(.tag)[@intFromEnum(expr)];
        const expr_data = p.nodes.items(.data)[@intFromEnum(expr)];

        // Task.detached { body } — check for field_access("detached") on ident("Task")
        if (expr_tag == .field_access) {
            const base_node_raw = expr_data.node_and_token[0];
            const field_token = expr_data.node_and_token[1];
            if (@intFromEnum(base_node_raw) < p.nodes.len) {
                const base_tag = p.nodes.items(.tag)[@intFromEnum(base_node_raw)];
                if (base_tag == .ident) {
                    const base_main = p.nodes.items(.main_token)[@intFromEnum(base_node_raw)];
                    if (p.isTokenText(base_main, "Task") and p.isTokenText(field_token, "detached")) {
                        if (p.peekTag() != .rbrace and p.peekTag() != .period) {
                            const body = try p.parseBlockStmt() orelse return null;
                            const extra = try p.addExtra(Ast.TaskData{ .body = body, .flags = .{ .is_detached = true } });
                            return try p.addNode(.{ .tag = .task_expr, .main_token = p.nodeMainToken(expr), .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                        }
                    }
                }
            }
        }

        // Generic struct init: call(UpperIdent, args) followed by { . or { ident:
        if (expr_tag == .call or expr_tag == .call_one or expr_tag == .call_zero) {
            if (p.peekNextIsPeriod() or (p.safe_mode and p.peekNextIsIdentColon())) {
                // Check callee is uppercase identifier
                const callee_idx = switch (expr_tag) {
                    .call_zero => expr_data.node,
                    .call_one => expr_data.node_and_node[0],
                    .call => expr_data.node_and_extra[0],
                    else => unreachable,
                };
                const callee_tag = p.nodes.items(.tag)[@intFromEnum(callee_idx)];
                if (callee_tag == .ident) {
                    const callee_main = p.nodes.items(.main_token)[@intFromEnum(callee_idx)];
                    const callee_text = p.tokenText(callee_main);
                    if (callee_text.len > 0 and std.ascii.isUpper(callee_text[0])) {
                        _ = p.advance(); // consume {
                        const use_colon = p.safe_mode and p.peekNextIsIdentColon();
                        const fields = if (use_colon) try p.parseColonFields() else try p.parseDotFields();
                        _ = p.eatToken(.rbrace);
                        const extra = try p.addExtra(Ast.StructInit{
                            .type_name_token = @enumFromInt(callee_main),
                            .type_args = SubRange.empty,
                            .fields = fields,
                        });
                        return try p.addNode(.{ .tag = .struct_init, .main_token = callee_main, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                    }
                }
            }
        }

        // Ident { ... } — struct init or Task creation
        if (expr_tag == .ident) {
            const ident_main = p.nodes.items(.main_token)[@intFromEnum(expr)];
            const ident_text = p.tokenText(ident_main);

            // Task { body } — unstructured task creation
            if (std.mem.eql(u8, ident_text, "Task")) {
                if (p.peekTag() != .rbrace and p.peekTag() != .period and
                    !(p.safe_mode and p.peekNextIsIdentColon()))
                {
                    const body = try p.parseBlockStmt() orelse return null;
                    const extra = try p.addExtra(Ast.TaskData{ .body = body, .flags = .{} });
                    return try p.addNode(.{ .tag = .task_expr, .main_token = ident_main, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                }
            }

            // Type {} — empty struct init
            if (ident_text.len > 0 and std.ascii.isUpper(ident_text[0]) and p.peekTag() == .rbrace) {
                _ = p.advance(); // consume {
                _ = p.advance(); // consume }
                const extra = try p.addExtra(Ast.StructInit{ .type_name_token = @enumFromInt(ident_main), .type_args = SubRange.empty, .fields = SubRange.empty });
                return try p.addNode(.{ .tag = .struct_init, .main_token = ident_main, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
            }

            // Type { .field = value } or Type { field: value } (@safe)
            if (ident_text.len > 0 and std.ascii.isUpper(ident_text[0])) {
                if (p.peekNextIsPeriod() or (p.safe_mode and p.peekNextIsIdentColon())) {
                    _ = p.advance(); // consume {
                    const use_colon = p.safe_mode and p.peekNextIsIdentColon();
                    const fields = if (use_colon) try p.parseColonFields() else try p.parseDotFields();
                    _ = p.eatToken(.rbrace);
                    const extra = try p.addExtra(Ast.StructInit{ .type_name_token = @enumFromInt(ident_main), .type_args = SubRange.empty, .fields = fields });
                    return try p.addNode(.{ .tag = .struct_init, .main_token = ident_main, .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } } });
                }
            }
        }

        return null; // Don't consume the brace — it's not part of this expression
    }

    fn peekIsNotColon(p: *const Parser) bool {
        if (p.tok_i + 1 >= p.token_tags.len) return true;
        return p.token_tags[p.tok_i + 1] != .colon;
    }

    fn peekIs(p: *const Parser, expected: Token) bool {
        if (p.tok_i + 1 >= p.token_tags.len) return false;
        return p.token_tags[p.tok_i + 1] == expected;
    }

    fn peekTag(p: *const Parser) Token {
        if (p.tok_i + 1 >= p.token_tags.len) return .eof;
        return p.token_tags[p.tok_i + 1];
    }

    fn isTokenText(p: *const Parser, ti: TokenIndex, expected: []const u8) bool {
        return std.mem.eql(u8, p.tokenText(ti), expected);
    }

    fn isDeclKeyword(p: *const Parser) bool {
        return switch (p.currentTag()) {
            .kw_fn, .kw_static, .kw_const, .kw_enum, .kw_type, .kw_struct, .at => true,
            else => false,
        };
    }




    pub fn deinit(p: *Parser) void {
        p.token_list.deinit(p.gpa);
        p.nodes.deinit(p.gpa);
        p.extra_data.deinit(p.gpa);
        p.errors.deinit(p.gpa);
        p.scratch.deinit(p.gpa);
    }
};

// Tests

test "parser infrastructure: addNode and addExtra round-trip" {
    var p = Parser.init(std.testing.allocator, "fn main() {}", false);
    defer p.deinit();
    try p.tokenize();

    // Manually add a node
    const idx = try p.addNode(.{
        .tag = .literal_int,
        .main_token = 0,
        .data = .{ .none = {} },
    });
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(idx));
    try std.testing.expectEqual(Tag.literal_int, p.nodes.items(.tag)[0]);
}

test "parser infrastructure: addExtra with SubRange" {
    var p = Parser.init(std.testing.allocator, "fn main() {}", false);
    defer p.deinit();

    const extra_idx = try p.addExtra(Ast.EnumDecl{
        .name_token = 42,
        .backing_type = .none,
        .variants = .{ .start = @enumFromInt(10), .end = @enumFromInt(20) },
        .nested_decls = SubRange.empty,
        .doc_comment = .none,
    });

    // Verify we can read it back
    try std.testing.expectEqual(@as(u32, 42), p.extra_data.items[@intFromEnum(extra_idx)]);
}

test "parser infrastructure: listToSpan" {
    var p = Parser.init(std.testing.allocator, "fn main() {}", false);
    defer p.deinit();

    const items = [_]u32{ 1, 2, 3, 4, 5 };
    const range = try p.listToSpan(&items);
    try std.testing.expectEqual(@as(u32, 5), range.len());
    try std.testing.expectEqual(@as(u32, 3), p.extra_data.items[@intFromEnum(range.start) + 2]);
}

test "parser infrastructure: scratch buffer" {
    var p = Parser.init(std.testing.allocator, "fn main() {}", false);
    defer p.deinit();

    const scratch_top = p.scratch.items.len;
    try p.scratch.append(p.gpa, 10);
    try p.scratch.append(p.gpa, 20);
    try p.scratch.append(p.gpa, 30);

    const range = try p.listToSpan(p.scratch.items[scratch_top..]);
    p.scratch.shrinkRetainingCapacity(scratch_top);

    try std.testing.expectEqual(@as(u32, 3), range.len());
    try std.testing.expectEqual(@as(usize, 0), p.scratch.items.len);
}

test "parser: tokenize basic source" {
    var p = Parser.init(std.testing.allocator, "fn main() { return 42; }", false);
    defer p.deinit();
    try p.tokenize();

    try std.testing.expectEqual(Token.kw_fn, p.token_tags[0]);
    try std.testing.expectEqual(Token.ident, p.token_tags[1]);
    try std.testing.expectEqual(Token.lparen, p.token_tags[2]);
    try std.testing.expectEqual(Token.rparen, p.token_tags[3]);
    try std.testing.expectEqual(Token.lbrace, p.token_tags[4]);
    try std.testing.expectEqual(Token.eof, p.token_tags[p.token_tags.len - 1]);
}

test "parser: parse import declaration" {
    var p = Parser.init(std.testing.allocator, "import \"std/io\"", false);
    defer p.deinit();
    try p.tokenize();

    // Reserve root node
    try p.nodes.append(p.gpa, .{ .tag = .root, .main_token = 0, .data = .{ .none = {} } });
    p.tok_i = 0;

    const decl = try p.parseDecl();
    try std.testing.expect(decl != null);
    try std.testing.expectEqual(Tag.import_decl, p.nodes.items(.tag)[@intFromEnum(decl.?)]);
}

test "parser: parse fn declaration stub" {
    var p = Parser.init(std.testing.allocator, "fn hello() { return 1; }", false);
    defer p.deinit();
    try p.tokenize();

    try p.nodes.append(p.gpa, .{ .tag = .root, .main_token = 0, .data = .{ .none = {} } });
    p.tok_i = 0;

    const decl = try p.parseDecl();
    try std.testing.expect(decl != null);
    try std.testing.expectEqual(Tag.fn_decl, p.nodes.items(.tag)[@intFromEnum(decl.?)]);
}

test "parser: parse test declaration stub" {
    var p = Parser.init(std.testing.allocator, "test \"arithmetic\" { var x = 1; }", false);
    defer p.deinit();
    try p.tokenize();

    try p.nodes.append(p.gpa, .{ .tag = .root, .main_token = 0, .data = .{ .none = {} } });
    p.tok_i = 0;

    const decl = try p.parseDecl();
    try std.testing.expect(decl != null);
    try std.testing.expectEqual(Tag.test_decl, p.nodes.items(.tag)[@intFromEnum(decl.?)]);
}


fn initTestParser(allocator: std.mem.Allocator, src: [:0]const u8) !Parser {
    var p = Parser.init(allocator, src, false);
    try p.tokenize();
    try p.nodes.append(p.gpa, .{ .tag = .root, .main_token = 0, .data = .{ .none = {} } });
    return p;
}

fn testAst(p: *const Parser) Ast.Ast {
    return .{
        .source = p.source,
        .tokens = p.token_list.slice(),
        .nodes = p.nodes.slice(),
        .extra_data = p.extra_data.items,
        .errors = &.{},
        .safe_mode = false,
    };
}

test "e2e: fn with params and return type" {
    var p = try initTestParser(std.testing.allocator, "fn add(a: i64, b: i64) i64 { return a; }");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    try std.testing.expectEqual(Tag.fn_decl, p.nodes.items(.tag)[@intFromEnum(decl)]);
    const a = testAst(&p);
    const f = a.fnDeclData(decl);
    try std.testing.expectEqualStrings("add", f.name);
    try std.testing.expect(!f.is_extern);
    try std.testing.expect(f.params.len() > 0);
    try std.testing.expect(f.return_type != .none);
    try std.testing.expect(f.body != .none);
}

test "e2e: extern fn" {
    var p = try initTestParser(std.testing.allocator, "extern fn exit(code: i32)");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    try std.testing.expectEqual(Tag.fn_decl_extern, p.nodes.items(.tag)[@intFromEnum(decl)]);
}

test "e2e: struct with fields" {
    var p = try initTestParser(std.testing.allocator, "struct Point { x: i64, y: i64 }");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    try std.testing.expectEqual(Tag.struct_decl, p.nodes.items(.tag)[@intFromEnum(decl)]);
    const a = testAst(&p);
    const s = a.structDeclData(decl);
    try std.testing.expectEqualStrings("Point", s.name);
    try std.testing.expect(s.fields.len() > 0);
    try std.testing.expect(!s.is_actor);
}

test "e2e: var with type and value" {
    var p = try initTestParser(std.testing.allocator, "var x: i64 = 42");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    const a = testAst(&p);
    const v = a.varDeclData(decl);
    try std.testing.expectEqualStrings("x", v.name);
    try std.testing.expect(!v.is_const);
    try std.testing.expect(v.type_expr != .none);
    try std.testing.expect(v.value != .none);
}

test "e2e: type alias distinct" {
    var p = try initTestParser(std.testing.allocator, "type RawPtr = distinct i64");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    try std.testing.expectEqual(Tag.type_alias_distinct, p.nodes.items(.tag)[@intFromEnum(decl)]);
}

test "e2e: trait declaration" {
    var p = try initTestParser(std.testing.allocator, "trait Hashable { fn hash() i64 }");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    const a = testAst(&p);
    const t = a.traitDeclData(decl);
    try std.testing.expectEqualStrings("Hashable", t.name);
    try std.testing.expect(t.methods.len() > 0);
}

test "e2e: union declaration" {
    var p = try initTestParser(std.testing.allocator, "union Result { ok: i64, err: string }");
    defer p.deinit();
    const decl = (try p.parseDecl()).?;
    const a = testAst(&p);
    const u = a.unionDeclData(decl);
    try std.testing.expectEqualStrings("Result", u.name);
    try std.testing.expect(u.variants.len() > 0);
}

test "e2e: binary expression in fn body" {
    var p = try initTestParser(std.testing.allocator, "fn f() { 1 + 2; }");
    defer p.deinit();
    _ = try p.parseDecl();
    var found = false;
    for (p.nodes.items(.tag)) |tag| {
        if (tag == .binary_add) found = true;
    }
    try std.testing.expect(found);
}

test "e2e: call expression" {
    var p = try initTestParser(std.testing.allocator, "fn f() { print(42); }");
    defer p.deinit();
    _ = try p.parseDecl();
    var found = false;
    for (p.nodes.items(.tag)) |tag| {
        if (tag == .call_one) found = true;
    }
    try std.testing.expect(found);
}

test "e2e: multiple top-level declarations" {
    var p = try initTestParser(std.testing.allocator,
        \\import "std/io"
        \\fn main() { return 0; }
        \\const PI = 3
    );
    defer p.deinit();
    const top = try p.parseTopLevel();
    try std.testing.expectEqual(@as(u32, 3), top.len());
}
