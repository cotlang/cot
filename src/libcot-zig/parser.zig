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

    // Parser state
    tok_i: TokenIndex,
    safe_mode: bool,

    // Impl block context for @safe mode self injection
    current_impl_type: ?TokenIndex,
    current_impl_is_generic: bool,
    current_impl_is_actor: bool,

    // Pending attributes
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

    // ========================================================================
    // Tokenization — pre-tokenize source into flat arrays
    // ========================================================================

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

    // ========================================================================
    // Core: addNode, addExtra, listToSpan, scratch
    // ========================================================================

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

    /// Append a single u32 to extra_data.
    fn appendExtra(p: *Parser, val: u32) !void {
        try p.extra_data.append(p.gpa, val);
    }

    // ========================================================================
    // Token navigation
    // ========================================================================

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

    // ========================================================================
    // Error handling
    // ========================================================================

    fn addError(p: *Parser, tag: Ast.Ast.ErrorTag) !void {
        try p.errors.append(p.gpa, .{
            .token = p.tok_i,
            .tag = tag,
        });
    }

    fn addErrorPrev(p: *Parser, tag: Ast.Ast.ErrorTag) !void {
        try p.errors.append(p.gpa, .{
            .token = p.tok_i -| 1,
            .tag = tag,
            .token_is_prev = true,
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

    // ========================================================================
    // Doc comments
    // ========================================================================

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

    // ========================================================================
    // Public API
    // ========================================================================

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
            .tokens = .{ // TODO: store tokens properly
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

    // ========================================================================
    // Top-level parsing
    // ========================================================================

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

    // ========================================================================
    // Declaration parsing — placeholder stubs
    // ========================================================================

    fn parseDecl(p: *Parser) Error!?Index {
        const tag = p.currentTag();
        return switch (tag) {
            .kw_fn => try p.parseFnDecl(false, false),
            .kw_const => try p.parseVarDecl(true),
            .kw_var => try p.parseVarDecl(false),
            .kw_struct => try p.parseStructDecl(),
            .kw_enum => try p.parseEnumDecl(),
            .kw_union => try p.parseUnionDecl(),
            .kw_type => try p.parseTypeAlias(),
            .kw_import => try p.parseImportDecl(),
            .kw_impl => try p.parseImplDecl(),
            .kw_trait => try p.parseTraitDecl(),
            .kw_test => try p.parseTestDecl(),
            .kw_extern => try p.parseExternDecl(),
            .kw_export => try p.parseExportDecl(),
            else => {
                try p.addError(.unexpected_token);
                return null;
            },
        };
    }

    // Stub declarations — each returns a node index
    // These will be fully implemented in subsequent steps

    fn parseFnDecl(p: *Parser, is_extern: bool, is_export: bool) Error!?Index {
        const fn_token = p.advance(); // consume 'fn'
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        // Skip to end of fn for now (minimal stub)
        // TODO: parse params, return type, body
        var brace_depth: u32 = 0;
        while (p.currentTag() != .eof) {
            const t = p.currentTag();
            if (t == .lbrace) brace_depth += 1;
            if (t == .rbrace) {
                if (brace_depth <= 1) { _ = p.advance(); break; }
                brace_depth -= 1;
            }
            if (t == .semicolon and brace_depth == 0) { _ = p.advance(); break; }
            _ = p.advance();
        }

        const doc = p.consumeDocComment();
        const extra = try p.addExtra(Ast.FnDecl{
            .name_token = name_token,
            .type_params = SubRange.empty,
            .param_bounds = SubRange.empty,
            .params = SubRange.empty,
            .return_type = .none,
            .flags = .{
                .is_export = is_export,
                .is_async = false,
                .is_static = false,
                .is_inlinable = false,
                .is_nonisolated = false,
            },
            .doc_comment = doc,
            .global_actor = .none,
        });

        if (is_extern) {
            return try p.addNode(.{
                .tag = .fn_decl_extern,
                .main_token = fn_token,
                .data = .{ .node_and_extra = .{ @enumFromInt(0), extra } },
            });
        }

        return try p.addNode(.{
            .tag = .fn_decl,
            .main_token = fn_token,
            .data = .{ .extra_and_node = .{ extra, @enumFromInt(0) } },
        });
    }

    fn parseVarDecl(p: *Parser, is_const: bool) Error!?Index {
        const var_token = p.advance(); // consume var/const
        const name_token = p.eatToken(.ident) orelse {
            try p.addError(.expected_identifier);
            return null;
        };

        const type_expr: OptionalIndex = .none;
        if (p.eatToken(.colon) != null) {
            // Skip type expression for now
            while (p.currentTag() != .assign and p.currentTag() != .semicolon and p.currentTag() != .eof) {
                _ = p.advance();
            }
        }

        const value: OptionalIndex = .none;
        if (p.eatToken(.assign) != null) {
            // Skip value expression for now
            while (p.currentTag() != .semicolon and p.currentTag() != .eof) {
                _ = p.advance();
            }
        }
        _ = p.eatToken(.semicolon);

        const doc = p.consumeDocComment();
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

    fn parseStructDecl(p: *Parser) Error!?Index {
        _ = p.advance(); // consume 'struct'
        // TODO: implement — skip to matching rbrace for now
        p.skipBraceBlock();
        return null;
    }

    fn parseEnumDecl(p: *Parser) Error!?Index {
        _ = p.advance();
        p.skipBraceBlock();
        return null;
    }

    fn parseUnionDecl(p: *Parser) Error!?Index {
        _ = p.advance();
        p.skipBraceBlock();
        return null;
    }

    fn parseTypeAlias(p: *Parser) Error!?Index {
        _ = p.advance();
        while (p.currentTag() != .semicolon and p.currentTag() != .eof) _ = p.advance();
        _ = p.eatToken(.semicolon);
        return null;
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
        _ = p.advance();
        p.skipBraceBlock();
        return null;
    }

    fn parseTraitDecl(p: *Parser) Error!?Index {
        _ = p.advance();
        p.skipBraceBlock();
        return null;
    }

    fn parseTestDecl(p: *Parser) Error!?Index {
        const test_token = p.advance(); // consume 'test'
        const name_token = p.eatToken(.string_lit) orelse {
            try p.addError(.expected_expression);
            return null;
        };

        // Skip body block for now
        var brace_depth: u32 = 0;
        while (p.currentTag() != .eof) {
            if (p.currentTag() == .lbrace) brace_depth += 1;
            if (p.currentTag() == .rbrace) {
                _ = p.advance();
                if (brace_depth <= 1) break;
                brace_depth -= 1;
            } else {
                _ = p.advance();
            }
        }

        return try p.addNode(.{
            .tag = .test_decl,
            .main_token = test_token,
            .data = .{ .token_and_node = .{ name_token, @enumFromInt(0) } },
        });
    }

    fn parseExternDecl(p: *Parser) Error!?Index {
        _ = p.advance(); // consume 'extern'
        if (p.currentTag() == .kw_fn) return p.parseFnDecl(true, false);
        try p.addError(.unexpected_token);
        return null;
    }

    fn parseExportDecl(p: *Parser) Error!?Index {
        _ = p.advance(); // consume 'export'
        if (p.currentTag() == .kw_fn) return p.parseFnDecl(false, true);
        return p.parseDecl();
    }

    // ========================================================================
    // Skip helpers (for stubs)
    // ========================================================================

    fn skipBraceBlock(p: *Parser) void {
        // Skip tokens until we find the identifier/name, then the brace block
        while (p.currentTag() != .lbrace and p.currentTag() != .eof) _ = p.advance();
        if (p.currentTag() == .eof) return;
        var depth: u32 = 0;
        while (p.currentTag() != .eof) {
            if (p.currentTag() == .lbrace) depth += 1;
            if (p.currentTag() == .rbrace) {
                _ = p.advance();
                depth -= 1;
                if (depth == 0) return;
            } else {
                _ = p.advance();
            }
        }
    }

    // ========================================================================
    // Cleanup
    // ========================================================================

    pub fn deinit(p: *Parser) void {
        p.token_list.deinit(p.gpa);
        p.nodes.deinit(p.gpa);
        p.extra_data.deinit(p.gpa);
        p.errors.deinit(p.gpa);
        p.scratch.deinit(p.gpa);
    }
};

// ============================================================================
// Tests
// ============================================================================

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
