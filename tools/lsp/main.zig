//! Cot Language Server
//!
//! A Language Server Protocol implementation for Cot and DBL.
//! Supports both .cot (modern syntax) and .dbl (legacy syntax) files.

const std = @import("std");
const protocol = @import("protocol.zig");
const server_mod = @import("server.zig");

const json = std.json;
const Allocator = std.mem.Allocator;
const JsonValue = protocol.JsonValue;
const JsonObject = protocol.JsonObject;
const JsonArray = protocol.JsonArray;
const Server = server_mod.Server;

// ============================================================
// LSP Handlers
// ============================================================

const Handlers = struct {
    /// Handle initialize request
    fn initialize(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        server.initialized = true;

        // Extract workspace root from params
        if (params.object.get("rootUri")) |root_uri| {
            if (root_uri != .null) {
                server.setWorkspaceRoot(root_uri.string) catch {};
            }
        } else if (params.object.get("rootPath")) |root_path| {
            if (root_path != .null) {
                server.setWorkspaceRoot(root_path.string) catch {};
            }
        }

        // Build capabilities
        var capabilities = JsonObject.init(allocator);

        // Text document sync - full sync mode
        try capabilities.put("textDocumentSync", JsonValue{ .integer = 1 });

        // Completion support
        var completion_options = JsonObject.init(allocator);
        var trigger_chars = JsonArray.init(allocator);
        try trigger_chars.append(JsonValue{ .string = "." });
        try trigger_chars.append(JsonValue{ .string = "%" });
        try completion_options.put("triggerCharacters", JsonValue{ .array = trigger_chars });
        try capabilities.put("completionProvider", JsonValue{ .object = completion_options });

        // Hover support
        try capabilities.put("hoverProvider", JsonValue{ .bool = true });

        // Document symbol support
        try capabilities.put("documentSymbolProvider", JsonValue{ .bool = true });

        // Semantic tokens support
        var semantic_tokens_options = JsonObject.init(allocator);
        var token_types = JsonArray.init(allocator);
        for (protocol.SemanticTokenTypes) |tt| {
            try token_types.append(JsonValue{ .string = tt });
        }
        var legend = JsonObject.init(allocator);
        try legend.put("tokenTypes", JsonValue{ .array = token_types });
        try legend.put("tokenModifiers", JsonValue{ .array = JsonArray.init(allocator) });
        try semantic_tokens_options.put("legend", JsonValue{ .object = legend });
        try semantic_tokens_options.put("full", JsonValue{ .bool = true });
        try capabilities.put("semanticTokensProvider", JsonValue{ .object = semantic_tokens_options });

        // Build result
        var result = JsonObject.init(allocator);
        try result.put("capabilities", JsonValue{ .object = capabilities });

        var server_info = JsonObject.init(allocator);
        try server_info.put("name", JsonValue{ .string = "cot-lsp" });
        try server_info.put("version", JsonValue{ .string = "0.2.0" });
        try result.put("serverInfo", JsonValue{ .object = server_info });

        return protocol.makeResponse(id, JsonValue{ .object = result }, allocator);
    }

    /// Handle shutdown request
    fn shutdown(server: *Server, id: JsonValue, allocator: Allocator) !JsonValue {
        server.shutdown_requested = true;
        return protocol.makeResponse(id, JsonValue.null, allocator);
    }

    /// Handle textDocument/didOpen notification
    fn didOpen(server: *Server, params: JsonValue) !void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const text = text_document.object.get("text") orelse return;
        const version = text_document.object.get("version") orelse return;

        try server.openDocument(uri.string, text.string, version.integer);

        // Publish diagnostics for the new document
        try publishDiagnostics(server, uri.string);
    }

    /// Handle textDocument/didChange notification
    fn didChange(server: *Server, params: JsonValue) !void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const version = text_document.object.get("version") orelse return;
        const changes = params.object.get("contentChanges") orelse return;

        if (changes.array.items.len > 0) {
            const last_change = changes.array.items[changes.array.items.len - 1];
            if (last_change.object.get("text")) |new_text| {
                try server.updateDocument(uri.string, new_text.string, version.integer);
                try publishDiagnostics(server, uri.string);
            }
        }
    }

    /// Handle textDocument/didClose notification
    fn didClose(server: *Server, params: JsonValue) void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        server.closeDocument(uri.string);
    }

    /// Handle textDocument/completion request
    fn completion(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        var items = JsonArray.init(allocator);

        // Get document language if available
        const doc_uri = blk: {
            const text_document = params.object.get("textDocument") orelse break :blk null;
            const uri = text_document.object.get("uri") orelse break :blk null;
            break :blk uri.string;
        };

        const language = if (doc_uri) |uri|
            if (server.getDocument(uri)) |doc| doc.language else server_mod.Language.cot
        else
            server_mod.Language.cot;

        // Provide language-appropriate completions
        switch (language) {
            .cot => {
                const cot_keywords = [_][]const u8{
                    "fn",       "struct",   "enum",     "trait",    "impl",
                    "const",    "let",      "type",     "import",   "if",
                    "else",     "match",    "for",      "while",    "loop",
                    "return",   "break",    "continue", "try",      "catch",
                    "throw",    "comptime", "pub",      "mut",
                };

                for (cot_keywords) |kw| {
                    try items.append(try (protocol.CompletionItem{
                        .label = kw,
                        .kind = .Keyword,
                        .detail = "Cot keyword",
                    }).toJson(allocator));
                }
            },
            .dbl => {
                const dbl_keywords = [_][]const u8{
                    "record",    "endrecord", "proc",      "end",       "main",
                    "endmain",   "subroutine","endsubroutine", "function", "endfunction",
                    "structure", "endstructure", "if",     "then",      "else",
                    "begin",     "while",     "do",        "until",     "for",
                    "from",      "thru",      "case",      "using",     "endusing",
                    "xcall",     "return",    "goto",      "display",   "open",
                    "close",     "read",      "write",     "store",     "delete",
                    "find",      "clear",     "init",      "incr",      "decr",
                };

                for (dbl_keywords) |kw| {
                    try items.append(try (protocol.CompletionItem{
                        .label = kw,
                        .kind = .Keyword,
                        .detail = "DBL keyword",
                    }).toJson(allocator));
                }

                // DBL builtins
                const builtins = [_][]const u8{
                    "%size", "%len", "%trim", "%atrim", "%string", "%integer",
                    "%date", "%time", "%abs", "%int", "%frac", "%round",
                };

                for (builtins) |fn_name| {
                    try items.append(try (protocol.CompletionItem{
                        .label = fn_name,
                        .kind = .Function,
                        .detail = "Built-in function",
                    }).toJson(allocator));
                }
            },
        }

        return protocol.makeResponse(id, JsonValue{ .array = items }, allocator);
    }

    /// Handle textDocument/hover request
    fn hover(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const position = params.object.get("position") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        const line_num: usize = @intCast(position.object.get("line").?.integer);
        const char_num: usize = @intCast(position.object.get("character").?.integer);

        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        const hover_text = getKeywordDoc(word, doc.language) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        var contents = JsonObject.init(allocator);
        try contents.put("kind", JsonValue{ .string = "markdown" });
        try contents.put("value", JsonValue{ .string = hover_text });

        var result = JsonObject.init(allocator);
        try result.put("contents", JsonValue{ .object = contents });

        return protocol.makeResponse(id, JsonValue{ .object = result }, allocator);
    }

    /// Handle textDocument/documentSymbol request
    fn documentSymbol(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const symbols = server_mod.getDocumentSymbols(allocator, doc) catch
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        defer allocator.free(symbols);

        var result = JsonArray.init(allocator);
        for (symbols) |sym| {
            try result.append(try sym.toJson(allocator));
        }

        return protocol.makeResponse(id, JsonValue{ .array = result }, allocator);
    }

    /// Handle textDocument/semanticTokens/full request
    fn semanticTokens(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Build semantic tokens based on language
        var data = JsonArray.init(allocator);

        switch (doc.language) {
            .cot => try buildCotSemanticTokens(allocator, doc.content, &data),
            .dbl => try buildDblSemanticTokens(allocator, doc.content, &data),
        }

        var result = JsonObject.init(allocator);
        try result.put("data", JsonValue{ .array = data });

        return protocol.makeResponse(id, JsonValue{ .object = result }, allocator);
    }
};

// ============================================================
// Helper Functions
// ============================================================

/// Publish diagnostics for a document
fn publishDiagnostics(server: *Server, uri: []const u8) !void {
    const doc = server.getDocument(uri) orelse return;

    const diagnostics = server_mod.analyzeDocument(server.allocator, doc) catch |err| {
        std.debug.print("Analysis error: {}\n", .{err});
        return;
    };
    defer server.allocator.free(diagnostics);

    var diag_array = JsonArray.init(server.allocator);
    for (diagnostics) |diag| {
        try diag_array.append(try diag.toJson(server.allocator));
    }

    var params = JsonObject.init(server.allocator);
    try params.put("uri", JsonValue{ .string = uri });
    try params.put("diagnostics", JsonValue{ .array = diag_array });

    const notification = try protocol.makeNotification("textDocument/publishDiagnostics", JsonValue{ .object = params }, server.allocator);
    try protocol.writeMessage(notification, server.allocator);
}

/// Get the word at a given position in the document
fn getWordAtPosition(content: []const u8, line: usize, char: usize) ?[]const u8 {
    var current_line: usize = 0;
    var line_start: usize = 0;

    for (content, 0..) |c, i| {
        if (current_line == line) {
            line_start = i;
            break;
        }
        if (c == '\n') current_line += 1;
    }

    var line_end = line_start;
    while (line_end < content.len and content[line_end] != '\n') : (line_end += 1) {}

    const line_content = content[line_start..line_end];
    if (char >= line_content.len) return null;

    var word_start = char;
    while (word_start > 0 and isWordChar(line_content[word_start - 1])) : (word_start -= 1) {}

    var word_end = char;
    while (word_end < line_content.len and isWordChar(line_content[word_end])) : (word_end += 1) {}

    if (word_start == word_end) return null;
    return line_content[word_start..word_end];
}

fn isWordChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_' or c == '%';
}

/// Get documentation for a keyword
fn getKeywordDoc(word: []const u8, language: server_mod.Language) ?[]const u8 {
    _ = language;

    const docs = .{
        // Control flow
        .{ "if", "**if** condition { body }\n\nConditional execution." },
        .{ "else", "**else** { body }\n\nAlternate branch of conditional." },
        .{ "while", "**while** condition { body }\n\nLoop while condition is true." },
        .{ "for", "**for** item **in** iterable { body }\n\nIterate over a collection." },
        .{ "return", "**return** [value]\n\nReturn from function." },
        .{ "match", "**match** expr { patterns }\n\nPattern matching." },

        // Declarations
        .{ "fn", "**fn** name(params) -> Type { body }\n\nFunction definition." },
        .{ "struct", "**struct** Name { fields }\n\nStructure definition." },
        .{ "enum", "**enum** Name { variants }\n\nEnumeration definition." },
        .{ "const", "**const** name = value\n\nConstant declaration." },
        .{ "let", "**let** [mut] name = value\n\nVariable declaration." },

        // DBL-specific
        .{ "record", "**record** [name]\n\nDefines a data record structure." },
        .{ "proc", "**proc**\n\nStarts the procedural code section." },
        .{ "display", "**display** expr\n\nOutputs values to the terminal." },
        .{ "xcall", "**xcall** subroutine\n\nCalls an external subroutine." },
        .{ "open", "**open**(channel, mode, file)\n\nOpens a file." },
        .{ "read", "**read**(channel, record)\n\nReads a record." },
        .{ "write", "**write**(channel, record)\n\nWrites a record." },
        .{ "store", "**store**(channel, record)\n\nStores a new ISAM record." },
        .{ "find", "**find**(channel, record, key)\n\nFinds a record by key." },
    };

    inline for (docs) |entry| {
        if (std.ascii.eqlIgnoreCase(word, entry[0])) {
            return entry[1];
        }
    }
    return null;
}

/// Build semantic tokens for Cot syntax
fn buildCotSemanticTokens(allocator: Allocator, source: []const u8, data: *JsonArray) !void {
    const cot = @import("cot");

    var lexer = cot.lexer.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    var prev_line: usize = 0;
    var prev_char: usize = 0;

    for (tokens) |token| {
        // Simple token type mapping based on lexeme patterns
        const token_type: ?usize = blk: {
            const TokenType = cot.token.TokenType;
            break :blk switch (token.type) {
                // Identifiers -> 7
                TokenType.identifier => 7,
                // Strings -> 14
                TokenType.string_literal => 14,
                // Numbers -> 15
                TokenType.integer_literal, TokenType.decimal_literal => 15,
                // Operators -> 16
                TokenType.plus, TokenType.minus, TokenType.star, TokenType.slash, TokenType.equals => 16,
                else => blk2: {
                    // Check if it's a keyword (starts with kw_)
                    const name = @tagName(token.type);
                    if (name.len > 3 and name[0] == 'k' and name[1] == 'w' and name[2] == '_') {
                        break :blk2 @as(?usize, 11); // keyword
                    }
                    break :blk2 null;
                },
            };
        };

        if (token_type) |tt| {
            const line = if (token.line > 0) token.line - 1 else 0;
            const char = if (token.column > 0) token.column - 1 else 0;
            const delta_line = line -| prev_line;
            const delta_char = if (delta_line == 0) char -| prev_char else char;

            try data.append(JsonValue{ .integer = @intCast(delta_line) });
            try data.append(JsonValue{ .integer = @intCast(delta_char) });
            try data.append(JsonValue{ .integer = @intCast(token.lexeme.len) });
            try data.append(JsonValue{ .integer = @intCast(tt) });
            try data.append(JsonValue{ .integer = 0 }); // No modifiers

            prev_line = line;
            prev_char = char;
        }
    }
}

/// Build semantic tokens for DBL syntax
fn buildDblSemanticTokens(allocator: Allocator, source: []const u8, data: *JsonArray) !void {
    const dbl = @import("dbl");

    var lexer = dbl.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    var prev_line: usize = 0;
    var prev_char: usize = 0;

    for (tokens) |token| {
        // Simple token type mapping based on lexeme patterns
        const token_type: ?usize = blk: {
            const TokenType = dbl.TokenType;
            break :blk switch (token.type) {
                // Identifiers -> 7
                TokenType.identifier => 7,
                // Strings -> 14
                TokenType.string_literal => 14,
                // Numbers -> 15
                TokenType.integer_literal, TokenType.decimal_literal => 15,
                // Operators -> 16
                TokenType.plus, TokenType.minus, TokenType.star, TokenType.slash, TokenType.equals => 16,
                else => blk2: {
                    // Check if it's a keyword (starts with kw_)
                    const name = @tagName(token.type);
                    if (name.len > 3 and name[0] == 'k' and name[1] == 'w' and name[2] == '_') {
                        break :blk2 @as(?usize, 11); // keyword
                    }
                    break :blk2 null;
                },
            };
        };

        if (token_type) |tt| {
            const line = if (token.line > 0) token.line - 1 else 0;
            const char = if (token.column > 0) token.column - 1 else 0;
            const delta_line = line -| prev_line;
            const delta_char = if (delta_line == 0) char -| prev_char else char;

            try data.append(JsonValue{ .integer = @intCast(delta_line) });
            try data.append(JsonValue{ .integer = @intCast(delta_char) });
            try data.append(JsonValue{ .integer = @intCast(token.lexeme.len) });
            try data.append(JsonValue{ .integer = @intCast(tt) });
            try data.append(JsonValue{ .integer = 0 }); // No modifiers

            prev_line = line;
            prev_char = char;
        }
    }
}

// ============================================================
// Main Entry Point
// ============================================================

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = Server.init(allocator);
    defer server.deinit();

    // Main message loop
    while (!server.shutdown_requested) {
        const msg = protocol.readMessage(allocator) catch continue orelse break;

        // Handle LSP message
        const method = msg.object.get("method") orelse continue;
        const id = msg.object.get("id");
        const params = msg.object.get("params") orelse JsonValue.null;

        const response: ?JsonValue = blk: {
            if (std.mem.eql(u8, method.string, "initialize")) {
                break :blk try Handlers.initialize(&server, id.?, params, allocator);
            } else if (std.mem.eql(u8, method.string, "initialized")) {
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "shutdown")) {
                break :blk try Handlers.shutdown(&server, id.?, allocator);
            } else if (std.mem.eql(u8, method.string, "exit")) {
                break;
            } else if (std.mem.eql(u8, method.string, "textDocument/didOpen")) {
                try Handlers.didOpen(&server, params);
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/didChange")) {
                try Handlers.didChange(&server, params);
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/didClose")) {
                Handlers.didClose(&server, params);
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/completion")) {
                break :blk try Handlers.completion(&server, id.?, params, allocator);
            } else if (std.mem.eql(u8, method.string, "textDocument/hover")) {
                break :blk try Handlers.hover(&server, id.?, params, allocator);
            } else if (std.mem.eql(u8, method.string, "textDocument/documentSymbol")) {
                break :blk try Handlers.documentSymbol(&server, id.?, params, allocator);
            } else if (std.mem.eql(u8, method.string, "textDocument/semanticTokens/full")) {
                break :blk try Handlers.semanticTokens(&server, id.?, params, allocator);
            } else {
                if (id) |req_id| {
                    break :blk try protocol.makeErrorResponse(req_id, protocol.ErrorCode.MethodNotFound, "Method not found", allocator);
                }
                break :blk null;
            }
        };

        if (response) |resp| {
            try protocol.writeMessage(resp, allocator);
        }
    }
}
