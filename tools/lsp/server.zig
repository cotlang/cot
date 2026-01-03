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
// Server State
// ============================================================

pub const Server = struct {
    allocator: Allocator,
    documents: std.StringHashMap(Document),
    initialized: bool = false,
    shutdown_requested: bool = false,
    workspace_root: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .documents = std.StringHashMap(Document).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var it = self.documents.valueIterator();
        while (it.next()) |doc| {
            var d = doc.*;
            d.deinit(self.allocator);
        }
        self.documents.deinit();
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

        try self.documents.put(uri_copy, .{
            .uri = uri_copy,
            .content = content_copy,
            .version = version,
            .language = Language.fromUri(uri),
        });
    }

    /// Update a document
    pub fn updateDocument(self: *Self, uri: []const u8, content: []const u8, version: i64) !void {
        if (self.documents.getPtr(uri)) |doc| {
            self.allocator.free(doc.content);
            doc.content = try self.allocator.dupe(u8, content);
            doc.version = version;
        }
    }

    /// Close a document
    pub fn closeDocument(self: *Self, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |entry| {
            var doc = entry.value;
            doc.deinit(self.allocator);
        }
    }

    /// Get a document by URI
    pub fn getDocument(self: *Self, uri: []const u8) ?*Document {
        return self.documents.getPtr(uri);
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
        // Collect parse errors
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.line > 0) err.line - 1 else 0);
            const col: u32 = @intCast(if (err.column > 0) err.column - 1 else 0);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + 10 },
                },
                .severity = .Error,
                .message = err.message,
            });
        }
        return;
    };
    defer allocator.free(top_level);

    // Collect parse errors (multi-error collection)
    if (parser.hasErrors()) {
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.line > 0) err.line - 1 else 0);
            const col: u32 = @intCast(if (err.column > 0) err.column - 1 else 0);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + 10 },
                },
                .severity = .Error,
                .message = err.message,
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
        // Collect parse errors
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.token.line > 0) err.token.line - 1 else 0);
            const col: u32 = @intCast(if (err.token.column > 0) err.token.column - 1 else 0);
            const len: u32 = @intCast(err.token.lexeme.len);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + len },
                },
                .severity = .Error,
                .message = err.message,
            });
        }
        return;
    };
    defer allocator.free(top_level);

    // Collect parse errors (multi-error collection)
    if (parser.errors.items.len > 0) {
        for (parser.errors.items) |err| {
            const line: u32 = @intCast(if (err.token.line > 0) err.token.line - 1 else 0);
            const col: u32 = @intCast(if (err.token.column > 0) err.token.column - 1 else 0);
            const len: u32 = @intCast(err.token.lexeme.len);
            try diagnostics.append(allocator, .{
                .range = .{
                    .start = .{ .line = line, .character = col },
                    .end = .{ .line = line, .character = col + len },
                },
                .severity = .Error,
                .message = err.message,
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
                break :blk .{
                    .name = name,
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
                break :blk .{
                    .name = name,
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
                break :blk .{
                    .name = name,
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
                break :blk .{
                    .name = name,
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
                break :blk .{
                    .name = name,
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
                const display_name = std.fmt.allocPrint(allocator, "test \"{s}\"", .{name}) catch name;
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
        "subroutine", "function", "main", "proc", "test",
    };

    // Keywords that decrease indent level
    // Note: else/catch/finally stay at the same level as begin/end, not dedented
    const dedent_keywords = [_][]const u8{
        "endrecord", "endgroup", "endstructure", "endcommon", "endglobal", "endliteral",
        "end", "endusing", "endclass", "endmethod", "endproperty",
        "endnamespace", "endinterface", "endtry", "endsubroutine", "endfunction",
        "endmain", "endproc", "endtest",
    };

    // Process line by line
    var line_num: u32 = 0;
    var line_start: usize = 0;
    var indent_level: u32 = 0;

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
                // Check if line starts with dedent keyword
                var should_dedent = false;
                for (dedent_keywords) |kw| {
                    if (startsWithKeyword(trimmed, kw)) {
                        should_dedent = true;
                        break;
                    }
                }

                // Apply dedent before this line
                if (should_dedent and indent_level > 0) {
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

                // Check if line starts with indent keyword
                for (indent_keywords) |kw| {
                    if (startsWithKeyword(trimmed, kw)) {
                        indent_level += 1;
                        break;
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
