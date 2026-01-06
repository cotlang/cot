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
    dex, // Dex component files

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
        // Check for .dx (3 chars)
        if (uri.len >= 3) {
            const ext3 = uri[uri.len - 3 ..];
            if (std.ascii.eqlIgnoreCase(ext3, ".dx")) {
                return .dex;
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
    // Map from type name to list of impl methods for that type
    impl_methods: std.StringHashMap(std.ArrayListUnmanaged(Symbol)),
    // Map from URI to list of (type_name, method) pairs for cleanup
    impl_by_document: std.StringHashMap(std.ArrayListUnmanaged(ImplEntry)),

    const Self = @This();

    pub const ImplEntry = struct {
        type_name: []const u8,
        method: Symbol,
    };

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .definitions = std.StringHashMap(std.ArrayListUnmanaged(Symbol)).init(allocator),
            .by_document = std.StringHashMap(std.ArrayListUnmanaged(Symbol)).init(allocator),
            .references = std.StringHashMap(std.ArrayListUnmanaged(Reference)).init(allocator),
            .refs_by_document = std.StringHashMap(std.ArrayListUnmanaged(RefEntry)).init(allocator),
            .impl_methods = std.StringHashMap(std.ArrayListUnmanaged(Symbol)).init(allocator),
            .impl_by_document = std.StringHashMap(std.ArrayListUnmanaged(ImplEntry)).init(allocator),
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

        // Free impl_methods lists
        var impl_it = self.impl_methods.valueIterator();
        while (impl_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.impl_methods.deinit();

        // Free impl_by_document lists
        var ibd_it = self.impl_by_document.valueIterator();
        while (ibd_it.next()) |list| {
            list.deinit(self.allocator);
        }
        self.impl_by_document.deinit();
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

    /// Add an impl method for a type
    pub fn addImplMethod(self: *Self, type_name: []const u8, method: Symbol, uri: []const u8) !void {
        // Duplicate type name
        const type_name_copy = try self.allocator.dupe(u8, type_name);
        errdefer self.allocator.free(type_name_copy);

        // Duplicate method name
        const method_name_copy = try self.allocator.dupe(u8, method.name);
        errdefer self.allocator.free(method_name_copy);

        // Duplicate params if present
        var params_copy: ?[]const FunctionParam = null;
        if (method.params) |params| {
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
        const return_type_copy: ?[]const u8 = if (method.return_type) |rt|
            try self.allocator.dupe(u8, rt)
        else
            null;

        const owned_method = Symbol{
            .name = method_name_copy,
            .kind = method.kind,
            .uri = method.uri,
            .line = method.line,
            .column = method.column,
            .end_line = method.end_line,
            .end_column = method.end_column,
            .parent = type_name_copy,
            .params = params_copy,
            .return_type = return_type_copy,
        };

        // Add to impl_methods map
        const impl_result = try self.impl_methods.getOrPut(type_name_copy);
        if (!impl_result.found_existing) {
            impl_result.value_ptr.* = .empty;
        }
        try impl_result.value_ptr.append(self.allocator, owned_method);

        // Add to impl_by_document for cleanup
        const uri_copy = try self.allocator.dupe(u8, uri);
        const ibd_result = try self.impl_by_document.getOrPut(uri_copy);
        if (!ibd_result.found_existing) {
            ibd_result.value_ptr.* = .empty;
        }
        try ibd_result.value_ptr.append(self.allocator, ImplEntry{ .type_name = type_name_copy, .method = owned_method });
    }

    /// Get impl methods for a type
    pub fn getImplMethods(self: *Self, type_name: []const u8) ?[]const Symbol {
        if (self.impl_methods.get(type_name)) |list| {
            return list.items;
        }
        return null;
    }

    /// Clear impl methods for a document
    pub fn clearDocumentImplMethods(self: *Self, uri: []const u8) void {
        if (self.impl_by_document.fetchRemove(uri)) |entry| {
            // Remove these methods from the impl_methods map
            for (entry.value.items) |item| {
                if (self.impl_methods.getPtr(item.type_name)) |method_list| {
                    var i: usize = 0;
                    while (i < method_list.items.len) {
                        if (std.mem.eql(u8, method_list.items[i].uri, uri)) {
                            _ = method_list.swapRemove(i);
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
            self.symbols.clearDocumentImplMethods(uri);
            self.indexDocument(doc) catch {};
        }
    }

    /// Close a document
    pub fn closeDocument(self: *Self, uri: []const u8) void {
        // Clear symbols, references, and impl methods for this document
        self.symbols.clearDocument(uri);
        self.symbols.clearDocumentReferences(uri);
        self.symbols.clearDocumentImplMethods(uri);

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
            .dex => {}, // DEX component files - no indexing yet
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

            // Handle impl blocks to extract methods
            if (tag == .impl_block) {
                // data.a = trait_type (TypeIdx) - for inherent impls, this is the type being implemented
                // data.b = methods_start (index into extra_data)
                // Layout: [method_count, trait_type, target_type, method_indices...]
                const extra_data = store.extra_data.items;
                const methods_start = data.b;
                if (methods_start + 2 >= extra_data.len) continue;

                const method_count = extra_data[methods_start];
                const trait_type_int = extra_data[methods_start + 1];
                const target_type_int = extra_data[methods_start + 2];

                // Determine the type name - use target_type if present (trait impl), otherwise use trait_type (inherent impl)
                // TypeIdx.null is maxInt(u32), so check against that instead of 0
                const target_type: cot.ast.TypeIdx = @enumFromInt(target_type_int);
                const trait_type: cot.ast.TypeIdx = @enumFromInt(trait_type_int);
                const type_idx = if (!target_type.isNull()) target_type else trait_type;

                // Get the type name
                const type_name = blk: {
                    if (type_idx.isNull()) break :blk null;
                    const type_tag = store.typeTag(type_idx);
                    if (type_tag == .named) {
                        const type_data_item = store.type_data.items[type_idx.toInt()];
                        const name_id: cot.base.StringId = @enumFromInt(type_data_item.a);
                        break :blk strings.get(name_id);
                    }
                    break :blk null;
                };

                if (type_name) |tn| {
                    // Extract each method
                    var i: u32 = 0;
                    while (i < method_count) : (i += 1) {
                        const method_idx_int = extra_data[methods_start + 3 + i];
                        const method_idx: cot.ast.StmtIdx = @enumFromInt(method_idx_int);

                        // Get method details (it's a fn_def)
                        if (store.stmtTag(method_idx) == .fn_def) {
                            const method_view = cot.ast.FnDefView.from(store, method_idx);
                            const method_data = store.stmtData(method_idx);
                            const method_loc = store.stmtLoc(method_idx);
                            const method_name_id: cot.base.StringId = @enumFromInt(method_data.a);
                            const method_name = strings.get(method_name_id);

                            // Extract parameters
                            var method_params: std.ArrayListUnmanaged(FunctionParam) = .empty;
                            defer method_params.deinit(self.allocator);

                            var param_iter = method_view.paramIterator(store);
                            while (param_iter.next()) |param| {
                                const param_name = strings.get(param.name);
                                const type_str = typeIdxToString(store, strings, param.type_idx);
                                try method_params.append(self.allocator, .{
                                    .name = param_name,
                                    .type_str = type_str,
                                });
                            }

                            // Get return type
                            const return_type = typeIdxToString(store, strings, method_view.return_type);

                            const method_name_len: u32 = @intCast(method_name.len);
                            const method_line: u32 = if (method_loc.line > 0) method_loc.line - 1 else 0;
                            const method_col: u32 = if (method_loc.column > 0) method_loc.column - 1 else 0;

                            try self.symbols.addImplMethod(tn, .{
                                .name = method_name,
                                .kind = .function,
                                .uri = uri,
                                .line = method_line,
                                .column = method_col,
                                .end_line = method_line,
                                .end_column = method_col + method_name_len,
                                .params = method_params.items,
                                .return_type = return_type,
                            }, uri);
                        }
                    }
                }
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
        .dex => try analyzeDex(allocator, doc.content, &diagnostics),
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

fn analyzeDex(allocator: Allocator, source: []const u8, diagnostics: *std.ArrayListUnmanaged(protocol.Diagnostic)) !void {
    // Use the Dex component parser through the framework
    const dex = cot.framework.dex;

    // Tokenize using ComponentLexer
    var lexer = dex.component_parser.ComponentLexer.init(allocator, source);
    const tokens = lexer.tokenize() catch |err| {
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

    // Parse tokens with error context for detailed error reporting
    var error_context = dex.component_parser.ParseErrorContext.init(allocator);
    error_context.setSource(source);

    var parser = dex.component_parser.ComponentParser.init(allocator, tokens, &error_context);
    defer parser.deinit();

    _ = parser.parse() catch {
        // Use detailed error from error context if available
        if (error_context.last_error) |parse_err| {
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = if (parse_err.line > 0) parse_err.line - 1 else 0, .character = if (parse_err.column > 0) parse_err.column - 1 else 0 },
                    .end = .{ .line = if (parse_err.line > 0) parse_err.line - 1 else 0, .character = parse_err.column },
                },
                .severity = .Error,
                .message = parse_err.message,
            });
        } else {
            // Fallback to generic error
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = 0, .character = 0 },
                    .end = .{ .line = 0, .character = 1 },
                },
                .severity = .Error,
                .message = "Dex component parse error",
            });
        }
        return;
    };
}

/// Get document symbols
pub fn getDocumentSymbols(allocator: Allocator, doc: *const Document) ![]protocol.DocumentSymbol {
    var symbols: std.ArrayListUnmanaged(protocol.DocumentSymbol) = .empty;
    errdefer symbols.deinit(allocator);

    switch (doc.language) {
        .cot => try getCotSymbols(allocator, doc.content, &symbols),
        .dbl => try getDblSymbols(allocator, doc.content, &symbols),
        .dex => try getDexSymbols(allocator, doc.content, &symbols),
    }

    return symbols.toOwnedSlice(allocator);
}

/// Extract struct fields as DocumentSymbol children
fn extractStructFieldChildren(
    allocator: Allocator,
    store: *const cot.ast.NodeStore,
    strings: *const cot.base.StringInterner,
    fields_start: u32,
    parent_loc: cot.ast.SourceLoc,
) !?[]protocol.DocumentSymbol {
    const extra_data = store.extra_data.items;
    if (fields_start >= extra_data.len) return null;

    const field_count = extra_data[fields_start];
    if (field_count == 0) return null;

    var children: std.ArrayListUnmanaged(protocol.DocumentSymbol) = .empty;
    errdefer children.deinit(allocator);

    // Fields start at fields_start + 3 (after count, type_param_count, type_params_start)
    // Each field is stored as [name_id, type_idx]
    var i: u32 = 0;
    while (i < field_count) : (i += 1) {
        const field_offset = fields_start + 3 + (i * 2);
        if (field_offset + 1 >= extra_data.len) break;

        const field_name_id: cot.base.StringId = @enumFromInt(extra_data[field_offset]);
        const field_type_idx: cot.ast.TypeIdx = @enumFromInt(extra_data[field_offset + 1]);

        const field_name = strings.get(field_name_id);
        const field_name_copy = try allocator.dupe(u8, field_name);
        const field_name_len: u32 = @intCast(field_name.len);

        // Get type string for detail
        const type_str = getTypeStringForSymbol(store, strings, field_type_idx);

        // Create child symbol for field - position on following lines from parent
        const field_line = parent_loc.line + 1 + i;
        try children.append(allocator, .{
            .name = field_name_copy,
            .detail = type_str,
            .kind = .Field,
            .range = .{
                .start = .{ .line = field_line, .character = 4 }, // Indented
                .end = .{ .line = field_line, .character = 4 + field_name_len },
            },
            .selection_range = .{
                .start = .{ .line = field_line, .character = 4 },
                .end = .{ .line = field_line, .character = 4 + field_name_len },
            },
        });
    }

    if (children.items.len == 0) return null;
    const slice = try children.toOwnedSlice(allocator);
    return slice;
}

/// Extract function parameters as DocumentSymbol children
fn extractFunctionParamChildren(
    allocator: Allocator,
    store: *const cot.ast.NodeStore,
    strings: *const cot.base.StringInterner,
    params_start: u32,
    parent_loc: cot.ast.SourceLoc,
) !?[]protocol.DocumentSymbol {
    const extra_data = store.extra_data.items;
    if (params_start >= extra_data.len) return null;

    const param_count = extra_data[params_start];
    if (param_count == 0) return null;

    var children: std.ArrayListUnmanaged(protocol.DocumentSymbol) = .empty;
    errdefer children.deinit(allocator);

    // Params are stored as [count, name1, type1, is_ref1, default1, name2, type2, ...]
    // Each param is 4 values
    var i: u32 = 0;
    while (i < param_count) : (i += 1) {
        const param_offset = params_start + 1 + (i * 4);
        if (param_offset >= extra_data.len) break;

        const param_name_id: cot.base.StringId = @enumFromInt(extra_data[param_offset]);
        const param_type_idx: cot.ast.TypeIdx = @enumFromInt(extra_data[param_offset + 1]);

        const param_name = strings.get(param_name_id);
        const param_name_copy = try allocator.dupe(u8, param_name);
        const param_name_len: u32 = @intCast(param_name.len);

        // Get type string for detail
        const type_str = getTypeStringForSymbol(store, strings, param_type_idx);

        // Create child symbol for parameter - all on the same line as function
        try children.append(allocator, .{
            .name = param_name_copy,
            .detail = type_str,
            .kind = .Variable, // Parameters shown as variables
            .range = .{
                .start = .{ .line = parent_loc.line, .character = 0 },
                .end = .{ .line = parent_loc.line, .character = param_name_len },
            },
            .selection_range = .{
                .start = .{ .line = parent_loc.line, .character = 0 },
                .end = .{ .line = parent_loc.line, .character = param_name_len },
            },
        });
    }

    if (children.items.len == 0) return null;
    const slice = try children.toOwnedSlice(allocator);
    return slice;
}

/// Get type string for document symbol detail
fn getTypeStringForSymbol(
    store: *const cot.ast.NodeStore,
    strings: *const cot.base.StringInterner,
    type_idx: cot.ast.TypeIdx,
) ?[]const u8 {
    const idx = type_idx.toInt();
    // TypeIdx of max value typically means "no type" - check for valid range only
    if (idx >= store.type_tags.items.len) return null;

    const type_tag = store.type_tags.items[idx];
    return switch (type_tag) {
        .i8 => "i8",
        .i16 => "i16",
        .i32 => "i32",
        .i64 => "i64",
        .u8 => "u8",
        .u16 => "u16",
        .u32 => "u32",
        .u64 => "u64",
        .f32 => "f32",
        .f64 => "f64",
        .bool => "bool",
        .string => "string",
        .void => "void",
        .named => blk: {
            const type_data = store.type_data.items[idx];
            const name_id: cot.base.StringId = @enumFromInt(type_data.a);
            break :blk strings.get(name_id);
        },
        else => null,
    };
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
                const params_start = data.b;
                const param_children = extractFunctionParamChildren(allocator, &store, &strings, params_start, loc) catch null;
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
                    .children = param_children,
                };
            },
            .struct_def => blk: {
                const name_id: cot.base.StringId = @enumFromInt(data.a);
                const name = strings.get(name_id);
                const name_len: u32 = @intCast(name.len);
                const name_copy = allocator.dupe(u8, name) catch break :blk null;
                const fields_start = data.b;
                const field_children = extractStructFieldChildren(allocator, &store, &strings, fields_start, loc) catch null;
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
                    .children = field_children,
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

fn getDexSymbols(allocator: Allocator, source: []const u8, symbols: *std.ArrayListUnmanaged(protocol.DocumentSymbol)) !void {
    // Parse Dex component to extract symbols using unified parser
    const dex = cot.framework.dex;

    // Use the unified parser that handles both page and component formats
    var error_context = dex.component_parser.ParseErrorContext.init(allocator);
    error_context.setSource(source);

    const parsed = dex.component_parser.parseSource(allocator, source, &error_context) catch return;

    // Add component name as class symbol
    try symbols.append(allocator, .{
        .name = try allocator.dupe(u8, parsed.name),
        .kind = .Class,
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 100, .character = 0 },
        },
        .selection_range = .{
            .start = .{ .line = 0, .character = 10 },
            .end = .{ .line = 0, .character = 10 + @as(u32, @intCast(parsed.name.len)) },
        },
    });

    // Add methods as function symbols
    for (parsed.methods, 0..) |method, i| {
        const line: u32 = @intCast(10 + i * 5); // Approximate line numbers
        try symbols.append(allocator, .{
            .name = try allocator.dupe(u8, method.name),
            .kind = .Method,
            .range = .{
                .start = .{ .line = line, .character = 0 },
                .end = .{ .line = line + 5, .character = 0 },
            },
            .selection_range = .{
                .start = .{ .line = line, .character = 5 },
                .end = .{ .line = line, .character = 5 + @as(u32, @intCast(method.name.len)) },
            },
        });
    }
}

// ============================================================
// Document Formatting
// ============================================================

/// Text edit for formatting (LSP format)
pub const TextEdit = struct {
    range: protocol.Range,
    new_text: []const u8,
};

/// Format a document and return text edits
/// Uses the shared formatter module and converts to LSP format
pub fn formatDocument(allocator: Allocator, doc: *const Document, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    const formatter = cot.formatter;

    // Map LSP language to formatter language
    const lang: formatter.Language = switch (doc.language) {
        .cot => .cot,
        .dbl => .dbl,
        .dex => return &.{}, // Dex files don't have formatting yet
    };

    // Get edits from shared formatter
    const shared_edits = try formatter.formatToEdits(allocator, doc.content, lang, .{
        .tab_size = tab_size,
        .insert_spaces = insert_spaces,
    });
    defer allocator.free(shared_edits);

    // Convert to LSP TextEdit format
    var lsp_edits: std.ArrayListUnmanaged(TextEdit) = .empty;
    errdefer {
        for (lsp_edits.items) |edit| {
            allocator.free(edit.new_text);
        }
        lsp_edits.deinit(allocator);
    }

    for (shared_edits) |edit| {
        // Duplicate the new_text since shared_edits will be freed
        const new_text = try allocator.dupe(u8, edit.new_text);
        errdefer allocator.free(new_text);

        try lsp_edits.append(allocator, .{
            .range = .{
                .start = .{ .line = edit.start_line, .character = edit.start_char },
                .end = .{ .line = edit.end_line, .character = edit.end_char },
            },
            .new_text = new_text,
        });
    }

    // Free the original new_text from shared formatter
    for (shared_edits) |edit| {
        allocator.free(edit.new_text);
    }

    return lsp_edits.toOwnedSlice(allocator);
}
