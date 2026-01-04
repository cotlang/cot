//! LSP Server State and Document Management
//!
//! Manages open documents and provides language detection for .cot and .dbl files.

const std = @import("std");
const cot = @import("cot");
const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;
const JsonValue = protocol.JsonValue;
const JsonObject = protocol.JsonObject;
const JsonArray = protocol.JsonArray;

// ============================================================
// Language Detection
// ============================================================

pub const Language = enum {
    cot, // Modern .cot syntax
    dbl, // Legacy .dbl syntax

    pub fn fromUri(uri: []const u8) Language {
        // Check last few characters for extension
        if (uri.len >= 4) {
            const ext = uri[uri.len - 4 ..];
            if (std.ascii.eqlIgnoreCase(ext, ".dbl") or
                std.ascii.eqlIgnoreCase(ext, ".dbo"))
            {
                return .dbl;
            }
        }
        return .cot;
    }
};

// ============================================================
// Document State
// ============================================================

pub const Document = struct {
    uri: []const u8,
    content: []const u8,
    version: i64,
    language: Language,

    pub fn deinit(self: *Document, allocator: Allocator) void {
        allocator.free(self.uri);
        allocator.free(self.content);
    }
};

// ============================================================
// Symbol Table
// ============================================================

pub const SymbolKind = enum {
    function,
    structure,
    record,
    enumeration,
    constant,
    variable,
    parameter,
    field,
    label,
    test_def,
};

/// Function parameter information for signature help
pub const FunctionParam = struct {
    name: []const u8,
    type_str: []const u8, // Type as string (e.g., "i64", "string")
};

pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    uri: []const u8,
    line: u32, // 0-indexed
    column: u32, // 0-indexed
    end_line: u32,
    end_column: u32,
    // Optional: parent symbol (for nested symbols like struct fields)
    parent: ?[]const u8 = null,
    // For functions: parameter list and return type
    params: ?[]const FunctionParam = null,
    return_type: ?[]const u8 = null,
};

/// A reference to a symbol (where it's used, not defined)
pub const Reference = struct {
    uri: []const u8,
    line: u32,
    column: u32,
    end_line: u32,
    end_column: u32,
};

/// Entry for tracking references by document
pub const RefEntry = struct {
    name: []const u8,
    ref: Reference,
};

pub const SymbolTable = struct {
    allocator: Allocator,
    // Map from symbol name to list of definitions (same name can be defined in multiple files)
    definitions: std.StringHashMap(std.ArrayListUnmanaged(Symbol)),
    // Map from URI to list of symbols defined in that document
    by_document: std.StringHashMap(std.ArrayListUnmanaged(Symbol)),
    // Map from symbol name to list of references (where the symbol is used)
    references: std.StringHashMap(std.ArrayListUnmanaged(Reference)),
    // Map from URI to list of references in that document (for cleanup)
    refs_by_document: std.StringHashMap(std.ArrayListUnmanaged(RefEntry)),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .definitions = std.StringHashMap(std.ArrayListUnmanaged(Symbol)).init(allocator),
            .by_document = std.StringHashMap(std.ArrayListUnmanaged(Symbol)).init(allocator),
            .references = std.StringHashMap(std.ArrayListUnmanaged(Reference)).init(allocator),
            .refs_by_document = std.StringHashMap(std.ArrayListUnmanaged(RefEntry)).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Free definition lists
        var def_it = self.definitions.valueIterator();
        while (def_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.definitions.deinit();

        // Free document lists
        var doc_it = self.by_document.valueIterator();
        while (doc_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.by_document.deinit();

        // Free reference lists
        var ref_it = self.references.valueIterator();
        while (ref_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.references.deinit();

        // Free refs_by_document lists
        var rbd_it = self.refs_by_document.valueIterator();
        while (rbd_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.refs_by_document.deinit();
    }

    /// Add a symbol to the table
    pub fn addSymbol(self: *Self, symbol: Symbol) !void {
        // Duplicate the name since it may point to temporary memory (like StringInterner)
        const name_copy = try self.allocator.dupe(u8, symbol.name);
        errdefer self.allocator.free(name_copy);

        // Duplicate params if present
        var params_copy: ?[]const FunctionParam = null;
        if (symbol.params) |params| {
            var param_list = try self.allocator.alloc(FunctionParam, params.len);
            for (params, 0..) |p, i| {
                param_list[i] = .{
                    .name = try self.allocator.dupe(u8, p.name),
                    .type_str = try self.allocator.dupe(u8, p.type_str),
                };
            }
            params_copy = param_list;
        }

        // Duplicate return type if present
        const return_type_copy: ?[]const u8 = if (symbol.return_type) |rt|
            try self.allocator.dupe(u8, rt)
        else
            null;

        // Create symbol with owned strings (uri is already owned by the document)
        const owned_symbol = Symbol{
            .name = name_copy,
            .kind = symbol.kind,
            .uri = symbol.uri,
            .line = symbol.line,
            .column = symbol.column,
            .end_line = symbol.end_line,
            .end_column = symbol.end_column,
            .parent = symbol.parent,
            .params = params_copy,
            .return_type = return_type_copy,
        };

        // Add to definitions map
        const def_result = try self.definitions.getOrPut(name_copy);
        if (!def_result.found_existing) {
            def_result.value_ptr.* = .empty;
        }
        try def_result.value_ptr.append(self.allocator, owned_symbol);

        // Add to document map
        const doc_result = try self.by_document.getOrPut(symbol.uri);
        if (!doc_result.found_existing) {
            doc_result.value_ptr.* = .empty;
        }
        try doc_result.value_ptr.append(self.allocator, owned_symbol);
    }

    /// Find definitions of a symbol by name
    pub fn findDefinitions(self: *Self, name: []const u8) ?[]const Symbol {
        if (self.definitions.get(name)) |list| {
            return list.items;
        }
        return null;
    }

    /// Find definition at a specific location (for go-to-definition from a reference)
    pub fn findDefinitionAt(self: *Self, uri: []const u8, line: u32, column: u32) ?Symbol {
        // First, find what symbol is at this position
        if (self.by_document.get(uri)) |symbols| {
            for (symbols.items) |sym| {
                if (sym.line == line and column >= sym.column and column < sym.end_column) {
                    return sym;
                }
            }
        }
        return null;
    }

    /// Clear all symbols for a document (called before re-indexing)
    pub fn clearDocument(self: *Self, uri: []const u8) void {
        // Remove from document map
        if (self.by_document.fetchRemove(uri)) |entry| {
            // Also remove these symbols from the definitions map
            for (entry.value.items) |sym| {
                if (self.definitions.getPtr(sym.name)) |def_list| {
                    // Remove symbols with this URI
                    var i: usize = 0;
                    while (i < def_list.items.len) {
                        if (std.mem.eql(u8, def_list.items[i].uri, uri)) {
                            _ = def_list.swapRemove(i);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
            var list = entry.value;
            list.deinit(self.allocator);
        }
    }

    /// Get all symbols in a document
    pub fn getDocumentSymbols(self: *Self, uri: []const u8) ?[]const Symbol {
        if (self.by_document.get(uri)) |list| {
            return list.items;
        }
        return null;
    }

    /// Add a reference to a symbol
    pub fn addReference(self: *Self, name: []const u8, ref: Reference) !void {
        // Duplicate the name since it may point to temporary memory (like token lexemes)
        const name_copy = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_copy);

        // Add to references map
        const ref_result = try self.references.getOrPut(name_copy);
        if (!ref_result.found_existing) {
            ref_result.value_ptr.* = .empty;
        }
        try ref_result.value_ptr.append(self.allocator, ref);

        // Add to refs_by_document for cleanup
        const rbd_result = try self.refs_by_document.getOrPut(ref.uri);
        if (!rbd_result.found_existing) {
            rbd_result.value_ptr.* = .empty;
        }
        try rbd_result.value_ptr.append(self.allocator, RefEntry{ .name = name_copy, .ref = ref });
    }

    /// Find all references to a symbol by name
    pub fn findReferences(self: *Self, name: []const u8) ?[]const Reference {
        if (self.references.get(name)) |list| {
            return list.items;
        }
        return null;
    }

    /// Clear all references for a document
    pub fn clearDocumentReferences(self: *Self, uri: []const u8) void {
        if (self.refs_by_document.fetchRemove(uri)) |entry| {
            // Remove these references from the references map
            for (entry.value.items) |item| {
                if (self.references.getPtr(item.name)) |ref_list| {
                    var i: usize = 0;
                    while (i < ref_list.items.len) {
                        if (std.mem.eql(u8, ref_list.items[i].uri, uri)) {
                            _ = ref_list.swapRemove(i);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
            var list = entry.value;
            list.deinit(self.allocator);
        }
    }
};

// ============================================================
// Server State
// ============================================================

pub const Server = struct {
    allocator: Allocator,
    documents: std.StringHashMap(Document),
    symbols: SymbolTable,
    initialized: bool = false,
    shutdown_requested: bool = false,
    workspace_root: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .documents = std.StringHashMap(Document).init(allocator),
            .symbols = SymbolTable.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var it = self.documents.valueIterator();
        while (it.next()) |doc| {
            var d = doc.*;
            d.deinit(self.allocator);
        }
        self.documents.deinit();
        self.symbols.deinit();
        if (self.workspace_root) |root| {
            self.allocator.free(root);
        }
    }

    /// Set workspace root from initialize params
    pub fn setWorkspaceRoot(self: *Self, root_uri: []const u8) !void {
        const path = if (std.mem.startsWith(u8, root_uri, "file://"))
            root_uri[7..]
        else
            root_uri;

        self.workspace_root = try self.allocator.dupe(u8, path);
    }

    /// Open a document
    pub fn openDocument(self: *Self, uri: []const u8, content: []const u8, version: i64) !void {
        const uri_copy = try self.allocator.dupe(u8, uri);
        const content_copy = try self.allocator.dupe(u8, content);

        const doc = Document{
            .uri = uri_copy,
            .content = content_copy,
            .version = version,
            .language = Language.fromUri(uri),
        };
        try self.documents.put(uri_copy, doc);

        // Index symbols in the new document (skip if content is too short or empty)
        if (content.len > 0) {
            self.indexDocument(&doc) catch {};
        }
    }

    /// Update a document
    pub fn updateDocument(self: *Self, uri: []const u8, content: []const u8, version: i64) !void {
        if (self.documents.getPtr(uri)) |doc| {
            self.allocator.free(doc.content);
            doc.content = try self.allocator.dupe(u8, content);
            doc.version = version;

            // Don't re-index on every keystroke - it causes crashes on malformed mid-edit content
            // Indexing happens on document open and can be triggered manually
        }
    }

    /// Re-index a document (call after save or when needed)
    pub fn reindexDocument(self: *Self, uri: []const u8) void {
        if (self.documents.getPtr(uri)) |doc| {
            self.symbols.clearDocument(uri);
            self.symbols.clearDocumentReferences(uri);
            self.indexDocument(doc) catch {};
        }
    }

    /// Close a document
    pub fn closeDocument(self: *Self, uri: []const u8) void {
        // Clear symbols and references for this document
        self.symbols.clearDocument(uri);
        self.symbols.clearDocumentReferences(uri);

        if (self.documents.fetchRemove(uri)) |entry| {
            var doc = entry.value;
            doc.deinit(self.allocator);
        }
    }

    /// Get a document by URI
    pub fn getDocument(self: *Self, uri: []const u8) ?*Document {
        return self.documents.getPtr(uri);
    }

    /// Index all symbols in a document
    pub fn indexDocument(self: *Self, doc: *const Document) !void {
        switch (doc.language) {
            .cot => try self.indexCotDocument(doc),
            .dbl => try self.indexDblDocument(doc),
        }
    }

    fn indexCotDocument(self: *Self, doc: *const Document) !void {
        // Tokenize
        var lexer = cot.lexer.Lexer.init(doc.content);
        const tokens = lexer.tokenize(self.allocator) catch return;
        defer self.allocator.free(tokens);

        // Extract references from tokens (all identifier usages)
        try self.extractReferencesFromTokens(doc.uri, tokens);

        // Parse
        var strings = cot.base.StringInterner.init(self.allocator);
        defer strings.deinit();

        var store = cot.ast.NodeStore.init(self.allocator, &strings);
        defer store.deinit();

        var parser = cot.parser.Parser.init(self.allocator, tokens, &store, &strings);
        defer parser.deinit();

        const stmts = parser.parse() catch return;
        defer self.allocator.free(stmts);

        // Extract symbols from statements
        try self.extractSymbolsFromStatements(doc.uri, &store, &strings, stmts);
    }

    fn indexDblDocument(self: *Self, doc: *const Document) !void {
        const dbl = @import("dbl");

        // Tokenize
        var lexer = dbl.Lexer.init(doc.content);
        const tokens = lexer.tokenize(self.allocator) catch return;
        defer self.allocator.free(tokens);

        // Extract references from tokens (all identifier usages)
        try self.extractDblReferencesFromTokens(doc.uri, tokens);

        // Parse
        var strings = cot.base.StringInterner.init(self.allocator);
        defer strings.deinit();

        var store = cot.ast.NodeStore.init(self.allocator, &strings);
        defer store.deinit();

        var parser = dbl.Parser.init(self.allocator, tokens, &store, &strings);
        defer parser.deinit();

        const stmts = parser.parse() catch return;
        defer self.allocator.free(stmts);

        // Extract symbols from statements
        try self.extractSymbolsFromStatements(doc.uri, &store, &strings, stmts);
    }

    /// Convert a TypeIdx to a display string
    fn typeIdxToString(store: *const cot.ast.NodeStore, strings: *const cot.base.StringInterner, type_idx: cot.ast.TypeIdx) []const u8 {
        if (type_idx.isNull()) return "void";
        const tag = store.typeTag(type_idx);
        return switch (tag) {
            .i8 => "i8",
            .i16 => "i16",
            .i32 => "i32",
            .i64 => "i64",
            .u8 => "u8",
            .u16 => "u16",
            .u32 => "u32",
            .u64 => "u64",
            .isize => "isize",
            .usize => "usize",
            .f32 => "f32",
            .f64 => "f64",
            .bool => "bool",
            .string => "string",
            .void => "void",
            .any => "any",
            .decimal => "decimal",
            .named => blk: {
                const data = store.typeData(type_idx);
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                break :blk strings.get(name_id);
            },
            .optional => "?",
            .array => "[]",
            .map => "map",
            .slice => "[]",
            .pointer => "*",
            .error_union => "!",
            .function => "fn",
            .tuple => "()",
            .@"union" => "union",
            .type_param => "T",
            .generic_instance => "generic",
            .inferred => "auto",
            else => "unknown",
        };
    }

    fn extractSymbolsFromStatements(
        self: *Self,
        uri: []const u8,
        store: *cot.ast.NodeStore,
        strings: *cot.base.StringInterner,
        stmts: []const cot.ast.StmtIdx,
    ) !void {
        for (stmts) |stmt_idx| {
            const tag = store.stmtTag(stmt_idx);
            const loc = store.stmtLoc(stmt_idx);
            const data = store.stmtData(stmt_idx);

            // Handle function definitions specially to extract parameters
            if (tag == .fn_def) {
                const view = cot.ast.FnDefView.from(store, stmt_idx);
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const fn_name = strings.get(name_id);

                // Extract parameters
                var params: std.ArrayListUnmanaged(FunctionParam) = .empty;
                defer params.deinit(self.allocator);

                var param_iter = view.paramIterator(store);
                while (param_iter.next()) |param| {
                    const param_name = strings.get(param.name);
                    const type_str = typeIdxToString(store, strings, param.type_idx);
                    try params.append(self.allocator, .{
                        .name = param_name,
                        .type_str = type_str,
                    });
                }

                // Get return type
                const return_type = typeIdxToString(store, strings, view.return_type);

                const name_len: u32 = @intCast(fn_name.len);
                const line: u32 = if (loc.line > 0) loc.line - 1 else 0;
                const col: u32 = if (loc.column > 0) loc.column - 1 else 0;

                try self.symbols.addSymbol(.{
                    .name = fn_name,
                    .kind = .function,
                    .uri = uri,
                    .line = line,
                    .column = col,
                    .end_line = line,
                    .end_column = col + name_len,
                    .params = params.items,
                    .return_type = return_type,
                });
                continue;
            }

            const symbol_info: ?struct { name: []const u8, kind: SymbolKind } = switch (tag) {
                .struct_def => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .structure };
                },
                .union_def => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .structure };
                },
                .enum_def => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .enumeration };
                },
                .const_decl => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .constant };
                },
                .let_decl => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .variable };
                },
                .test_def => blk: {
                    const name_id: cot.base.StringId = @enumFromInt(data.a);
                    break :blk .{ .name = strings.get(name_id), .kind = .test_def };
                },
                else => null,
            };

            if (symbol_info) |info| {
                const name_len: u32 = @intCast(info.name.len);
                // Convert from 1-based (AST) to 0-based (LSP)
                const line: u32 = if (loc.line > 0) loc.line - 1 else 0;
                const col: u32 = if (loc.column > 0) loc.column - 1 else 0;
                try self.symbols.addSymbol(.{
                    .name = info.name,
                    .kind = info.kind,
                    .uri = uri,
                    .line = line,
                    .column = col,
                    .end_line = line,
                    .end_column = col + name_len,
                });
            }
        }
    }

    /// Extract references from tokens (identifiers that reference symbols)
    fn extractReferencesFromTokens(self: *Self, uri: []const u8, tokens: []const cot.token.Token) !void {
        for (tokens) |token| {
            if (token.type == .identifier) {
                // Bounds check to prevent crashes on invalid token data
                if (token.line > 1_000_000 or token.column > 10_000) continue;
                if (token.lexeme.len == 0 or token.lexeme.len > 10_000) continue;

                const line: u32 = if (token.line > 0) @intCast(token.line - 1) else 0;
                const col: u32 = if (token.column > 0) @intCast(token.column - 1) else 0;
                const len: u32 = @intCast(token.lexeme.len);

                self.symbols.addReference(token.lexeme, .{
                    .uri = uri,
                    .line = line,
                    .column = col,
                    .end_line = line,
                    .end_column = col + len,
                }) catch continue;
            }
        }
    }

    /// Extract references from DBL tokens
    fn extractDblReferencesFromTokens(self: *Self, uri: []const u8, tokens: anytype) !void {
        const dbl = @import("dbl");
        for (tokens) |token| {
            if (token.type == dbl.TokenType.identifier) {
                // Bounds check to prevent crashes on invalid token data
                if (token.line > 1_000_000 or token.column > 10_000) continue;
                if (token.lexeme.len == 0 or token.lexeme.len > 10_000) continue;

                const line: u32 = if (token.line > 0) @intCast(token.line - 1) else 0;
                const col: u32 = if (token.column > 0) @intCast(token.column - 1) else 0;
                const len: u32 = @intCast(token.lexeme.len);

                self.symbols.addReference(token.lexeme, .{
                    .uri = uri,
                    .line = line,
                    .column = col,
                    .end_line = line,
                    .end_column = col + len,
                }) catch continue;
            }
        }
    }
};

// ============================================================
// Analysis Functions
// ============================================================

/// Parse a document and return diagnostics
pub fn analyzeDocument(allocator: Allocator, doc: *const Document) ![]protocol.Diagnostic {
    var diagnostics: std.ArrayListUnmanaged(protocol.Diagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    switch (doc.language) {
        .cot => try analyzeCot(allocator, doc.content, &diagnostics),
        .dbl => try analyzeDbl(allocator, doc.content, &diagnostics),
    }

    return diagnostics.toOwnedSlice(allocator);
}

fn analyzeCot(allocator: Allocator, source: []const u8, diagnostics: *std.ArrayListUnmanaged(protocol.Diagnostic)) !void {
    // Tokenize
    var lexer = cot.lexer.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch |err| {
        try diagnostics.append(allocator, .{
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 1 },
            },
            .severity = .Error,
            .message = @errorName(err),
        });
        return;
    };
    defer allocator.free(tokens);

    // Parse
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parser = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    defer parser.deinit();

    const top_level = parser.parse() catch {
        // Collect parse errors - must dupe messages since parser will be freed
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.line > 0) err.line - 1 else 0);
            const col: u32 = @intCast(if (err.column > 0) err.column - 1 else 0);
            const msg_copy = try allocator.dupe(u8, err.message);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + 10 },
                },
                .severity = .Error,
                .message = msg_copy,
            });
        }
        return;
    };
    defer allocator.free(top_level);

    // Collect parse errors (multi-error collection) - must dupe messages
    if (parser.hasErrors()) {
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.line > 0) err.line - 1 else 0);
            const col: u32 = @intCast(if (err.column > 0) err.column - 1 else 0);
            const msg_copy = try allocator.dupe(u8, err.message);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + 10 },
                },
                .severity = .Error,
                .message = msg_copy,
            });
        }
        return; // Don't run semantic analysis if there are parse errors
    }

    // Run semantic analysis to catch type errors, missing required params, etc.
    const semantic_diags = cot.analyzeSemantics(allocator, &store, &strings, top_level) catch {
        return; // Semantic analysis failed, just report parse results
    };
    defer {
        for (semantic_diags) |diag| {
            allocator.free(diag.message);
        }
        allocator.free(semantic_diags);
    }

    // Convert semantic diagnostics to LSP diagnostics
    for (semantic_diags) |diag| {
        const line: u32 = if (diag.line > 0) diag.line - 1 else 0;
        const col: u32 = if (diag.column > 0) diag.column - 1 else 0;
        try diagnostics.append(allocator, .{
            .range = .{
                .start = .{ .line = line, .character = col },
                .end = .{ .line = line, .character = col + 20 },
            },
            .severity = switch (diag.severity) {
                .@"error" => .Error,
                .warning => .Warning,
            },
            .message = try allocator.dupe(u8, diag.message),
        });
    }
}

fn analyzeDbl(allocator: Allocator, source: []const u8, diagnostics: *std.ArrayListUnmanaged(protocol.Diagnostic)) !void {
    // Use DBL lexer and parser
    const dbl = @import("dbl");

    // Tokenize
    var lexer = dbl.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch |err| {
        try diagnostics.append(allocator, .{
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 1 },
            },
            .severity = .Error,
            .message = @errorName(err),
        });
        return;
    };
    defer allocator.free(tokens);

    // Parse
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parser = dbl.Parser.init(allocator, tokens, &store, &strings);
    defer parser.deinit();

    const top_level = parser.parse() catch {
        // Collect parse errors - must dupe messages since parser will be freed
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.token.line > 0) err.token.line - 1 else 0);
            const col: u32 = @intCast(if (err.token.column > 0) err.token.column - 1 else 0);
            const len: u32 = @intCast(err.token.lexeme.len);
            const msg_copy = try allocator.dupe(u8, err.message);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + len },
                },
                .severity = .Error,
                .message = msg_copy,
            });
        }
        return;
    };
    defer allocator.free(top_level);

    // Collect parse errors (multi-error collection) - must dupe messages
    if (parser.errors.items.len > 0) {
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.token.line > 0) err.token.line - 1 else 0);
            const col: u32 = @intCast(if (err.token.column > 0) err.token.column - 1 else 0);
            const len: u32 = @intCast(err.token.lexeme.len);
            const msg_copy = try allocator.dupe(u8, err.message);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + len },
                },
                .severity = .Error,
                .message = msg_copy,
            });
        }
        return; // Don't run semantic analysis if there are parse errors
    }

    // Run semantic analysis to catch type errors, missing required params, etc.
    const semantic_diags = cot.analyzeSemantics(allocator, &store, &strings, top_level) catch {
        return; // Semantic analysis failed, just report parse results
    };
    defer {
        for (semantic_diags) |diag| {
            allocator.free(diag.message);
        }
        allocator.free(semantic_diags);
    }

    // Convert semantic diagnostics to LSP diagnostics
    for (semantic_diags) |diag| {
        const line: u32 = if (diag.line > 0) diag.line - 1 else 0;
        const col: u32 = if (diag.column > 0) diag.column - 1 else 0;
        try diagnostics.append(allocator, .{
            .range = .{
                .start = .{ .line = line, .character = col },
                .end = .{ .line = line, .character = col + 20 },
            },
            .severity = switch (diag.severity) {
                .@"error" => .Error,
                .warning => .Warning,
            },
            .message = try allocator.dupe(u8, diag.message),
        });
    }
}

/// Get document symbols
pub fn getDocumentSymbols(allocator: Allocator, doc: *const Document) ![]protocol.DocumentSymbol {
    var symbols: std.ArrayListUnmanaged(protocol.DocumentSymbol) = .empty;
    errdefer symbols.deinit(allocator);

    switch (doc.language) {
        .cot => try getCotSymbols(allocator, doc.content, &symbols),
        .dbl => try getDblSymbols(allocator, doc.content, &symbols),
    }

    return symbols.toOwnedSlice(allocator);
}

fn getCotSymbols(allocator: Allocator, source: []const u8, symbols: *std.ArrayListUnmanaged(protocol.DocumentSymbol)) !void {
    // Tokenize
    var lexer = cot.lexer.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    // Parse
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parser = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    defer parser.deinit();

    const stmts = parser.parse() catch return;
    defer allocator.free(stmts);

    // Extract symbols from top-level statements
    for (stmts) |stmt_idx| {
        const tag = store.stmtTag(stmt_idx);
        const loc = store.stmtLoc(stmt_idx);
        const data = store.stmtData(stmt_idx);

        const symbol: ?protocol.DocumentSymbol = switch (tag) {
            .fn_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                // Duplicate name since StringInterner will be freed when this function exits
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                break :blk .{
                    .name = name_copy,
                    .kind = .Function,
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                };
            },
            .struct_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                break :blk .{
                    .name = name_copy,
                    .kind = .Struct,
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                };
            },
            .union_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                break :blk .{
                    .name = name_copy,
                    .kind = .Struct, // Union shown as struct in symbol outline
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                };
            },
            .enum_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                break :blk .{
                    .name = name_copy,
                    .kind = .Enum,
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                };
            },
            .const_decl => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                break :blk .{
                    .name = name_copy,
                    .kind = .Constant,
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len },
                    },
                };
            },
            .test_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                // Create display name with test prefix - use allocator to ensure string outlives this scope
                const display_name = std.fmt.allocPrint(allocator, "test \"{s}\"", .{name}) catch break :blk null;
                break :blk .{
                    .name = display_name,
                    .kind = .Function, // Tests are essentially functions
                    .range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len + 7 }, // +7 for 'test ""'
                    },
                    .selection_range = .{
                        .start = .{ .line = loc.line, .character = loc.column },
                        .end = .{ .line = loc.line, .character = loc.column + name_len + 7 },
                    },
                };
            },
            else => null,
        };

        if (symbol) |s| {
            try symbols.append(allocator, s);
        }
    }
}

fn getDblSymbols(allocator: Allocator, source: []const u8, symbols: *std.ArrayListUnmanaged(protocol.DocumentSymbol)) !void {
    const dbl = @import("dbl");

    // Tokenize
    var lexer = dbl.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    // Scan tokens for structure declarations
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];

        // Look for subroutine/function/main declarations
        if (token.type == .kw_subroutine or token.type == .kw_function or token.type == .kw_main) {
            if (i + 1 < tokens.len and tokens[i + 1].type == .identifier) {
                const name_token = tokens[i + 1];
                const line: u32 = @intCast(if (token.line > 0) token.line - 1 else 0);
                const col: u32 = @intCast(if (name_token.column > 0) name_token.column - 1 else 0);
                const name_len: u32 = @intCast(name_token.lexeme.len);
                try symbols.append(allocator, .{
                    .name = name_token.lexeme,
                    .kind = .Function,
                    .range = .{
                        .start = .{ .line = line, .character = 0 },
                        .end = .{ .line = line, .character = 80 },
                    },
                    .selection_range = .{
                        .start = .{ .line = line, .character = col },
                        .end = .{ .line = line, .character = col + name_len },
                    },
                });
            }
        }

        // Look for structure declarations
        if (token.type == .kw_structure) {
            if (i + 1 < tokens.len and tokens[i + 1].type == .identifier) {
                const name_token = tokens[i + 1];
                const line: u32 = @intCast(if (token.line > 0) token.line - 1 else 0);
                const col: u32 = @intCast(if (name_token.column > 0) name_token.column - 1 else 0);
                const name_len: u32 = @intCast(name_token.lexeme.len);
                try symbols.append(allocator, .{
                    .name = name_token.lexeme,
                    .kind = .Struct,
                    .range = .{
                        .start = .{ .line = line, .character = 0 },
                        .end = .{ .line = line, .character = 80 },
                    },
                    .selection_range = .{
                        .start = .{ .line = line, .character = col },
                        .end = .{ .line = line, .character = col + name_len },
                    },
                });
            }
        }

        // Look for test declarations: test "name"
        if (token.type == .kw_test) {
            if (i + 1 < tokens.len and tokens[i + 1].type == .string_literal) {
                const name_token = tokens[i + 1];
                const line: u32 = @intCast(if (token.line > 0) token.line - 1 else 0);
                const col: u32 = @intCast(if (token.column > 0) token.column - 1 else 0);
                // Create display name including "test" prefix - use allocator to ensure string outlives this scope
                const display_name = std.fmt.allocPrint(allocator, "test {s}", .{name_token.lexeme}) catch name_token.lexeme;
                const display_len: u32 = @intCast(display_name.len);
                try symbols.append(allocator, .{
                    .name = display_name,
                    .kind = .Function,
                    .range = .{
                        .start = .{ .line = line, .character = 0 },
                        .end = .{ .line = line, .character = 80 },
                    },
                    .selection_range = .{
                        .start = .{ .line = line, .character = col },
                        .end = .{ .line = line, .character = col + display_len },
                    },
                });
            }
        }

        // Look for class declarations
        if (token.type == .kw_class) {
            if (i + 1 < tokens.len and tokens[i + 1].type == .identifier) {
                const name_token = tokens[i + 1];
                const line: u32 = @intCast(if (token.line > 0) token.line - 1 else 0);
                const col: u32 = @intCast(if (name_token.column > 0) name_token.column - 1 else 0);
                const name_len: u32 = @intCast(name_token.lexeme.len);
                try symbols.append(allocator, .{
                    .name = name_token.lexeme,
                    .kind = .Class,
                    .range = .{
                        .start = .{ .line = line, .character = 0 },
                        .end = .{ .line = line, .character = 80 },
                    },
                    .selection_range = .{
                        .start = .{ .line = line, .character = col },
                        .end = .{ .line = line, .character = col + name_len },
                    },
                });
            }
        }
    }
}

// ============================================================
// Document Formatting
// ============================================================

/// Text edit for formatting
pub const TextEdit = struct {
    range: protocol.Range,
    new_text: []const u8,
};

/// Format a document and return text edits
pub fn formatDocument(allocator: Allocator, doc: *const Document, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    return switch (doc.language) {
        .dbl => formatDbl(allocator, doc.content, tab_size, insert_spaces),
        .cot => formatCot(allocator, doc.content, tab_size, insert_spaces),
    };
}

/// Format DBL source code
fn formatDbl(allocator: Allocator, source: []const u8, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    var edits: std.ArrayListUnmanaged(TextEdit) = .empty;
    errdefer edits.deinit(allocator);

    // Build the indent string
    var indent_buf: [16]u8 = undefined;
    const indent_str = if (insert_spaces) blk: {
        const size = @min(tab_size, 16);
        @memset(indent_buf[0..size], ' ');
        break :blk indent_buf[0..size];
    } else "\t";

    // Keywords that increase indent level
    // Note: begin/end work like braces - control flow keywords (if/while/for/etc.)
    // don't increase indent; only begin does
    const indent_keywords = [_][]const u8{
        "record", "group", "structure", "common", "global", "literal",
        "begin", "using",
        "class", "method", "property", "namespace", "interface", "try",
        "subroutine", "function", "test",
    };

    // Keywords that should be at indent 0 but set indent to 1 for following lines
    // proc is special: it aligns with function/endfunction but its body is indented
    const proc_like_keywords = [_][]const u8{
        "proc",
    };

    // Keywords that decrease indent level by 1
    const dedent_keywords = [_][]const u8{
        "endrecord", "endgroup", "endstructure", "endcommon", "endglobal", "endliteral",
        "end", "endusing", "endclass", "endmethod", "endproperty",
        "endnamespace", "endinterface", "endtry", "endproc",
    };

    // Keywords that reset indent to 0 (end of top-level blocks)
    // These end blocks that can contain multiple nested indent levels
    const reset_indent_keywords = [_][]const u8{
        "endmain", "endfunction", "endsubroutine", "endtest",
    };

    // Control flow keywords that indent the next single statement (if no begin)
    const control_flow_keywords = [_][]const u8{
        "if", "else", "while", "for", "do", "using", "case",
    };

    // Process line by line
    var line_num: u32 = 0;
    var line_start: usize = 0;
    var indent_level: u32 = 0;
    var pending_single_indent: bool = false; // Next line gets +1 for single-statement body

    var i: usize = 0;
    while (i <= source.len) : (i += 1) {
        const at_end = i == source.len;
        const at_newline = !at_end and source[i] == '\n';

        if (at_newline or at_end) {
            const line_end = i;
            const line = source[line_start..line_end];

            // Skip empty lines
            const trimmed = std.mem.trim(u8, line, " \t");
            if (trimmed.len > 0) {
                // Check if line starts with reset-indent keyword (top-level block enders)
                var should_reset = false;
                for (reset_indent_keywords) |kw| {
                    if (startsWithKeyword(trimmed, kw)) {
                        should_reset = true;
                        break;
                    }
                }

                // Check if line starts with proc-like keyword (indent 0, then set to 1)
                var is_proc_like = false;
                if (!should_reset) {
                    for (proc_like_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            is_proc_like = true;
                            break;
                        }
                    }
                }

                // Check if line starts with dedent keyword
                var should_dedent = false;
                if (!should_reset and !is_proc_like) {
                    for (dedent_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            should_dedent = true;
                            break;
                        }
                    }
                }

                // Apply indent changes before this line
                if (should_reset) {
                    indent_level = 0;
                    pending_single_indent = false;
                } else if (is_proc_like) {
                    indent_level = 0; // proc itself at level 0
                    pending_single_indent = false;
                } else if (should_dedent and indent_level > 0) {
                    indent_level -= 1;
                    pending_single_indent = false;
                }

                // Calculate expected indent (add 1 if this is a single-statement body)
                var effective_indent = indent_level;
                if (pending_single_indent) {
                    effective_indent += 1;
                    pending_single_indent = false; // Only applies to one line
                }
                const expected_indent = effective_indent * @as(u32, @intCast(indent_str.len));

                // Find actual indent
                var actual_indent: u32 = 0;
                for (line) |c| {
                    if (c == ' ') {
                        actual_indent += 1;
                    } else if (c == '\t') {
                        actual_indent += tab_size;
                    } else {
                        break;
                    }
                }

                // If indent doesn't match, create edit
                if (actual_indent != expected_indent) {
                    // Build new indent
                    const new_indent = try allocator.alloc(u8, expected_indent);
                    if (insert_spaces) {
                        @memset(new_indent, ' ');
                    } else {
                        const tabs = expected_indent / tab_size;
                        @memset(new_indent[0..tabs], '\t');
                    }

                    // Find where content starts
                    var content_start: u32 = 0;
                    for (line) |c| {
                        if (c != ' ' and c != '\t') break;
                        content_start += 1;
                    }

                    try edits.append(allocator, .{
                        .range = .{
                            .start = .{ .line = line_num, .character = 0 },
                            .end = .{ .line = line_num, .character = content_start },
                        },
                        .new_text = new_indent,
                    });
                }

                // Update indent for next line
                if (is_proc_like) {
                    // proc sets indent to 1 for following lines
                    indent_level = 1;
                } else {
                    // Check if line starts with indent keyword
                    for (indent_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            indent_level += 1;
                            break;
                        }
                    }

                    // Check for control flow without begin (single-statement body)
                    // e.g., "if (x)" without "begin" -> next line gets +1 indent
                    for (control_flow_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            // Check if line contains or ends with "begin"
                            const lower_trimmed = trimmed; // Already lowercase comparison in startsWithKeyword
                            var has_begin = false;
                            var j: usize = 0;
                            while (j + 5 <= trimmed.len) : (j += 1) {
                                if (std.ascii.eqlIgnoreCase(trimmed[j .. j + 5], "begin")) {
                                    // Check word boundary
                                    const before_ok = j == 0 or !std.ascii.isAlphanumeric(trimmed[j - 1]);
                                    const after_ok = j + 5 >= trimmed.len or !std.ascii.isAlphanumeric(trimmed[j + 5]);
                                    if (before_ok and after_ok) {
                                        has_begin = true;
                                        break;
                                    }
                                }
                            }
                            _ = lower_trimmed;
                            if (!has_begin) {
                                pending_single_indent = true;
                            }
                            break;
                        }
                    }
                }
            }

            line_num += 1;
            line_start = i + 1;
        }
    }

    return edits.toOwnedSlice(allocator);
}

/// Check if line starts with a keyword (case-insensitive, word boundary)
fn startsWithKeyword(line: []const u8, keyword: []const u8) bool {
    if (line.len < keyword.len) return false;

    // Case-insensitive comparison
    for (keyword, 0..) |kc, j| {
        const lc = std.ascii.toLower(line[j]);
        const kwc = std.ascii.toLower(kc);
        if (lc != kwc) return false;
    }

    // Check word boundary
    if (line.len == keyword.len) return true;
    const next_char = line[keyword.len];
    return !std.ascii.isAlphanumeric(next_char) and next_char != '_';
}

/// Format Cot source code
fn formatCot(allocator: Allocator, source: []const u8, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    var edits: std.ArrayListUnmanaged(TextEdit) = .empty;
    errdefer edits.deinit(allocator);

    // Build the indent string
    var indent_buf: [16]u8 = undefined;
    const indent_str = if (insert_spaces) blk: {
        const size = @min(tab_size, 16);
        @memset(indent_buf[0..size], ' ');
        break :blk indent_buf[0..size];
    } else "\t";

    // Track brace depth for indentation
    var indent_level: u32 = 0;
    var line_num: u32 = 0;
    var line_start: usize = 0;

    var i: usize = 0;
    while (i <= source.len) : (i += 1) {
        const at_end = i == source.len;
        const at_newline = !at_end and source[i] == '\n';

        if (at_newline or at_end) {
            const line_end = i;
            const line = source[line_start..line_end];

            const trimmed = std.mem.trim(u8, line, " \t");
            if (trimmed.len > 0) {
                // Check if line starts with closing brace
                const starts_with_close = trimmed[0] == '}';

                // Apply dedent before this line if it starts with }
                if (starts_with_close and indent_level > 0) {
                    indent_level -= 1;
                }

                // Calculate expected indent
                const expected_indent = indent_level * @as(u32, @intCast(indent_str.len));

                // Find actual indent
                var actual_indent: u32 = 0;
                for (line) |c| {
                    if (c == ' ') {
                        actual_indent += 1;
                    } else if (c == '\t') {
                        actual_indent += tab_size;
                    } else {
                        break;
                    }
                }

                // If indent doesn't match, create edit
                if (actual_indent != expected_indent) {
                    const new_indent = try allocator.alloc(u8, expected_indent);
                    if (insert_spaces) {
                        @memset(new_indent, ' ');
                    } else {
                        const tabs = expected_indent / tab_size;
                        @memset(new_indent[0..tabs], '\t');
                    }

                    var content_start: u32 = 0;
                    for (line) |c| {
                        if (c != ' ' and c != '\t') break;
                        content_start += 1;
                    }

                    try edits.append(allocator, .{
                        .range = .{
                            .start = .{ .line = line_num, .character = 0 },
                            .end = .{ .line = line_num, .character = content_start },
                        },
                        .new_text = new_indent,
                    });
                }

                // Count braces on this line to update indent for next line
                var in_string = false;
                var in_comment = false;
                for (trimmed, 0..) |c, j| {
                    if (in_comment) break;
                    if (c == '"' and (j == 0 or trimmed[j - 1] != '\\')) {
                        in_string = !in_string;
                    } else if (!in_string) {
                        if (c == '/' and j + 1 < trimmed.len and trimmed[j + 1] == '/') {
                            in_comment = true;
                        } else if (c == '{') {
                            indent_level += 1;
                        } else if (c == '}' and !starts_with_close) {
                            // Only dedent for } not at start of line
                            if (indent_level > 0) indent_level -= 1;
                        }
                    }
                }
            }

            line_num += 1;
            line_start = i + 1;
        }
    }

    return edits.toOwnedSlice(allocator);
}
