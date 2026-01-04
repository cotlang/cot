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

        // Signature help support (function parameter hints)
        var sig_help_options = JsonObject.init(allocator);
        var sig_trigger_chars = JsonArray.init(allocator);
        try sig_trigger_chars.append(JsonValue{ .string = "(" });
        try sig_trigger_chars.append(JsonValue{ .string = "," });
        try sig_help_options.put("triggerCharacters", JsonValue{ .array = sig_trigger_chars });
        try capabilities.put("signatureHelpProvider", JsonValue{ .object = sig_help_options });

        // Hover support
        try capabilities.put("hoverProvider", JsonValue{ .bool = true });

        // Document symbol support
        try capabilities.put("documentSymbolProvider", JsonValue{ .bool = true });

        // Go to definition support
        try capabilities.put("definitionProvider", JsonValue{ .bool = true });

        // Find references support
        try capabilities.put("referencesProvider", JsonValue{ .bool = true });

        // Document highlight support (highlight all occurrences of symbol under cursor)
        try capabilities.put("documentHighlightProvider", JsonValue{ .bool = true });

        // Rename symbol support
        var rename_options = JsonObject.init(allocator);
        try rename_options.put("prepareProvider", JsonValue{ .bool = true });
        try capabilities.put("renameProvider", JsonValue{ .object = rename_options });

        // Document formatting support
        try capabilities.put("documentFormattingProvider", JsonValue{ .bool = true });

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

    /// Handle textDocument/didSave notification
    fn didSave(server: *Server, params: JsonValue) void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;

        // Re-index the document on save (safe because content is valid)
        server.reindexDocument(uri.string);
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
                    "fn",       "struct",   "union",    "view",     "enum",
                    "trait",    "impl",     "const",    "let",      "type",
                    "import",   "if",       "else",     "match",    "for",
                    "while",    "loop",     "return",   "break",    "continue",
                    "try",      "catch",    "throw",    "comptime", "pub",
                    "mut",
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

    /// Handle textDocument/signatureHelp request
    fn signatureHelp(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
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

        // Find the function call context at the cursor position
        const call_info = findFunctionCallContext(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Look up the function definition
        if (server.symbols.findDefinitions(call_info.function_name)) |definitions| {
            for (definitions) |def| {
                if (def.kind == .function) {
                    // Build signature based on document language
                    var sig_buf: [1024]u8 = undefined;
                    var sig_stream = std.io.fixedBufferStream(&sig_buf);
                    const writer = sig_stream.writer();

                    const is_dbl = doc.language == .dbl;

                    // Function keyword and name
                    if (is_dbl) {
                        writer.print("function {s}(", .{def.name}) catch return protocol.makeResponse(id, JsonValue.null, allocator);
                    } else {
                        writer.print("fn {s}(", .{def.name}) catch return protocol.makeResponse(id, JsonValue.null, allocator);
                    }

                    // Build parameters array for highlighting
                    var params_array = JsonArray.init(allocator);

                    if (def.params) |fn_params| {
                        for (fn_params, 0..) |p, i| {
                            if (i > 0) writer.writeAll(", ") catch {};

                            // Record parameter start position for highlighting
                            const param_start = sig_stream.pos;

                            writer.print("{s}: {s}", .{ p.name, p.type_str }) catch {};

                            const param_end = sig_stream.pos;

                            // Create parameter info
                            var param_info = JsonObject.init(allocator);
                            var label_array = JsonArray.init(allocator);
                            try label_array.append(JsonValue{ .integer = @intCast(param_start) });
                            try label_array.append(JsonValue{ .integer = @intCast(param_end) });
                            try param_info.put("label", JsonValue{ .array = label_array });
                            try params_array.append(JsonValue{ .object = param_info });
                        }
                    }

                    writer.writeByte(')') catch {};

                    // Add return type for Cot only (DBL doesn't show return types)
                    if (!is_dbl) {
                        if (def.return_type) |rt| {
                            if (!std.mem.eql(u8, rt, "void")) {
                                writer.print(": {s}", .{rt}) catch {};
                            }
                        }
                    }

                    const signature_str = sig_buf[0..sig_stream.pos];
                    const signature_copy = try allocator.dupe(u8, signature_str);

                    // Build signature info
                    var sig_info = JsonObject.init(allocator);
                    try sig_info.put("label", JsonValue{ .string = signature_copy });
                    try sig_info.put("parameters", JsonValue{ .array = params_array });

                    // Build signatures array
                    var signatures = JsonArray.init(allocator);
                    try signatures.append(JsonValue{ .object = sig_info });

                    // Build result
                    var result = JsonObject.init(allocator);
                    try result.put("signatures", JsonValue{ .array = signatures });
                    try result.put("activeSignature", JsonValue{ .integer = 0 });
                    try result.put("activeParameter", JsonValue{ .integer = @intCast(call_info.active_param) });

                    return protocol.makeResponse(id, JsonValue{ .object = result }, allocator);
                }
            }
        }

        return protocol.makeResponse(id, JsonValue.null, allocator);
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

    /// Handle textDocument/definition request
    fn definition(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
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

        // Get the word at the cursor position
        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Look up the symbol definition
        if (server.symbols.findDefinitions(word)) |definitions| {
            if (definitions.len > 0) {
                // Return the first definition (could return multiple for overloads)
                const def = definitions[0];

                var location = JsonObject.init(allocator);
                try location.put("uri", JsonValue{ .string = def.uri });

                var range = JsonObject.init(allocator);
                var start_pos = JsonObject.init(allocator);
                try start_pos.put("line", JsonValue{ .integer = @intCast(def.line) });
                try start_pos.put("character", JsonValue{ .integer = @intCast(def.column) });
                var end_pos = JsonObject.init(allocator);
                try end_pos.put("line", JsonValue{ .integer = @intCast(def.end_line) });
                try end_pos.put("character", JsonValue{ .integer = @intCast(def.end_column) });
                try range.put("start", JsonValue{ .object = start_pos });
                try range.put("end", JsonValue{ .object = end_pos });
                try location.put("range", JsonValue{ .object = range });

                return protocol.makeResponse(id, JsonValue{ .object = location }, allocator);
            }
        }

        return protocol.makeResponse(id, JsonValue.null, allocator);
    }

    /// Handle textDocument/references request
    fn references(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        const position = params.object.get("position") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const line_num: usize = @intCast(position.object.get("line").?.integer);
        const char_num: usize = @intCast(position.object.get("character").?.integer);

        // Get the word at the cursor position
        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        var locations = JsonArray.init(allocator);

        // Check if user wants to include the declaration
        const include_declaration = if (params.object.get("context")) |ctx|
            if (ctx.object.get("includeDeclaration")) |incl| incl.bool else true
        else
            true;

        // Note: We don't separately add definitions here because the reference extraction
        // already captures all identifier tokens, including the function/struct name at
        // the definition site. This gives us correct column positions (pointing to the
        // name, not the keyword like "function" or "struct").
        _ = include_declaration;

        // Add all references (includes definition sites via token extraction)
        if (server.symbols.findReferences(word)) |refs| {
            for (refs) |ref| {
                var location = JsonObject.init(allocator);
                try location.put("uri", JsonValue{ .string = ref.uri });

                var range = JsonObject.init(allocator);
                var start_pos = JsonObject.init(allocator);
                try start_pos.put("line", JsonValue{ .integer = @intCast(ref.line) });
                try start_pos.put("character", JsonValue{ .integer = @intCast(ref.column) });
                var end_pos = JsonObject.init(allocator);
                try end_pos.put("line", JsonValue{ .integer = @intCast(ref.end_line) });
                try end_pos.put("character", JsonValue{ .integer = @intCast(ref.end_column) });
                try range.put("start", JsonValue{ .object = start_pos });
                try range.put("end", JsonValue{ .object = end_pos });
                try location.put("range", JsonValue{ .object = range });

                try locations.append(JsonValue{ .object = location });
            }
        }

        return protocol.makeResponse(id, JsonValue{ .array = locations }, allocator);
    }

    /// Handle textDocument/documentHighlight request
    /// Returns all occurrences of the symbol under the cursor in the current document
    fn documentHighlight(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        const position = params.object.get("position") orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        const line_num: usize = @intCast(position.object.get("line").?.integer);
        const char_num: usize = @intCast(position.object.get("character").?.integer);

        // Get the word at the cursor position
        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);

        var highlights = JsonArray.init(allocator);

        // Find all references to this symbol in the current document only
        if (server.symbols.findReferences(word)) |refs| {
            for (refs) |ref| {
                // Only include references from the current document
                if (!std.mem.eql(u8, ref.uri, uri.string)) continue;

                var highlight = JsonObject.init(allocator);

                var range = JsonObject.init(allocator);
                var start_pos = JsonObject.init(allocator);
                try start_pos.put("line", JsonValue{ .integer = @intCast(ref.line) });
                try start_pos.put("character", JsonValue{ .integer = @intCast(ref.column) });
                var end_pos = JsonObject.init(allocator);
                try end_pos.put("line", JsonValue{ .integer = @intCast(ref.end_line) });
                try end_pos.put("character", JsonValue{ .integer = @intCast(ref.end_column) });
                try range.put("start", JsonValue{ .object = start_pos });
                try range.put("end", JsonValue{ .object = end_pos });
                try highlight.put("range", JsonValue{ .object = range });

                // DocumentHighlightKind: 1 = Text, 2 = Read, 3 = Write
                // For now, mark all as Text (1) - we could enhance to detect read vs write
                try highlight.put("kind", JsonValue{ .integer = 1 });

                try highlights.append(JsonValue{ .object = highlight });
            }
        }

        return protocol.makeResponse(id, JsonValue{ .array = highlights }, allocator);
    }

    /// Handle textDocument/prepareRename request
    /// Validates that rename is possible at the given position and returns the range to rename
    fn prepareRename(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
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

        // Get the word at the cursor position
        const word_info = getWordRangeAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Check if this symbol exists in our references (is it a valid identifier?)
        if (server.symbols.findReferences(word_info.word) == null) {
            return protocol.makeResponse(id, JsonValue.null, allocator);
        }

        // Return the range that will be renamed
        var range = JsonObject.init(allocator);
        var start_pos = JsonObject.init(allocator);
        try start_pos.put("line", JsonValue{ .integer = @intCast(line_num) });
        try start_pos.put("character", JsonValue{ .integer = @intCast(word_info.start_col) });
        var end_pos = JsonObject.init(allocator);
        try end_pos.put("line", JsonValue{ .integer = @intCast(line_num) });
        try end_pos.put("character", JsonValue{ .integer = @intCast(word_info.end_col) });
        try range.put("start", JsonValue{ .object = start_pos });
        try range.put("end", JsonValue{ .object = end_pos });

        // Return PrepareRenameResult with range and placeholder
        var result = JsonObject.init(allocator);
        try result.put("range", JsonValue{ .object = range });
        try result.put("placeholder", JsonValue{ .string = word_info.word });

        return protocol.makeResponse(id, JsonValue{ .object = result }, allocator);
    }

    /// Handle textDocument/rename request
    /// Returns WorkspaceEdit with all text edits to rename the symbol
    fn rename(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        const text_document = params.object.get("textDocument") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const uri = text_document.object.get("uri") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const position = params.object.get("position") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);
        const new_name = params.object.get("newName") orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Validate new name is not a keyword
        if (isKeyword(new_name.string)) {
            return protocol.makeErrorResponse(id, -32602, "Cannot rename to a keyword", allocator);
        }

        // Validate new name is a valid identifier
        if (!isValidIdentifier(new_name.string)) {
            return protocol.makeErrorResponse(id, -32602, "Invalid identifier name", allocator);
        }

        const doc = server.getDocument(uri.string) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        const line_num: usize = @intCast(position.object.get("line").?.integer);
        const char_num: usize = @intCast(position.object.get("character").?.integer);

        // Get the word at the cursor position
        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return protocol.makeResponse(id, JsonValue.null, allocator);

        // Build WorkspaceEdit with changes grouped by document URI
        var changes = JsonObject.init(allocator);

        // Find all references and group by document, deduplicating by position
        if (server.symbols.findReferences(word)) |refs| {
            // Use a map to deduplicate references by URI and position
            const PosKey = struct { line: u32, col: u32 };
            var seen_positions = std.StringHashMap(std.AutoHashMap(PosKey, void)).init(allocator);
            defer {
                var it = seen_positions.valueIterator();
                while (it.next()) |pos_map| {
                    pos_map.deinit();
                }
                seen_positions.deinit();
            }

            var edits_by_uri = std.StringHashMap(JsonArray).init(allocator);
            defer edits_by_uri.deinit();

            for (refs) |ref| {
                // Get or create position set for this URI
                const seen_result = seen_positions.getOrPut(ref.uri) catch continue;
                if (!seen_result.found_existing) {
                    seen_result.value_ptr.* = std.AutoHashMap(PosKey, void).init(allocator);
                }

                // Check if we've seen this position already
                const pos_key = PosKey{ .line = ref.line, .col = ref.column };
                if (seen_result.value_ptr.contains(pos_key)) {
                    continue; // Skip duplicate
                }
                seen_result.value_ptr.put(pos_key, {}) catch continue;

                // Get or create edit array for this URI
                const edit_result = edits_by_uri.getOrPut(ref.uri) catch continue;
                if (!edit_result.found_existing) {
                    edit_result.value_ptr.* = JsonArray.init(allocator);
                }

                var edit = JsonObject.init(allocator);
                var range = JsonObject.init(allocator);
                var start_pos = JsonObject.init(allocator);
                try start_pos.put("line", JsonValue{ .integer = @intCast(ref.line) });
                try start_pos.put("character", JsonValue{ .integer = @intCast(ref.column) });
                var end_pos = JsonObject.init(allocator);
                try end_pos.put("line", JsonValue{ .integer = @intCast(ref.end_line) });
                try end_pos.put("character", JsonValue{ .integer = @intCast(ref.end_column) });
                try range.put("start", JsonValue{ .object = start_pos });
                try range.put("end", JsonValue{ .object = end_pos });
                try edit.put("range", JsonValue{ .object = range });
                try edit.put("newText", JsonValue{ .string = new_name.string });

                try edit_result.value_ptr.append(JsonValue{ .object = edit });
            }

            // Add all edits to changes object
            var it = edits_by_uri.iterator();
            while (it.next()) |entry| {
                try changes.put(entry.key_ptr.*, JsonValue{ .array = entry.value_ptr.* });
            }
        }

        var workspace_edit = JsonObject.init(allocator);
        try workspace_edit.put("changes", JsonValue{ .object = changes });

        return protocol.makeResponse(id, JsonValue{ .object = workspace_edit }, allocator);
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

    /// Handle textDocument/formatting request
    fn formatting(server: *Server, id: JsonValue, params: JsonValue, allocator: Allocator) !JsonValue {
        std.debug.print("LSP: formatting request received\n", .{});

        const text_document = params.object.get("textDocument") orelse {
            std.debug.print("LSP: no textDocument in params\n", .{});
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        };
        const uri = text_document.object.get("uri") orelse {
            std.debug.print("LSP: no uri in textDocument\n", .{});
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        };

        std.debug.print("LSP: formatting uri={s}\n", .{uri.string});

        const doc = server.getDocument(uri.string) orelse {
            std.debug.print("LSP: document not found\n", .{});
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        };

        std.debug.print("LSP: document found, language={s}, len={d}\n", .{@tagName(doc.language), doc.content.len});

        // Get formatting options
        const options = params.object.get("options") orelse JsonValue{ .object = JsonObject.init(allocator) };
        const tab_size: u32 = if (options.object.get("tabSize")) |ts| @intCast(ts.integer) else 4;
        const insert_spaces = if (options.object.get("insertSpaces")) |is| is.bool else true;

        std.debug.print("LSP: tab_size={d}, insert_spaces={}\n", .{tab_size, insert_spaces});

        // Format the document
        const edits = server_mod.formatDocument(allocator, doc, tab_size, insert_spaces) catch |err| {
            std.debug.print("LSP: formatDocument error: {}\n", .{err});
            return protocol.makeResponse(id, JsonValue{ .array = JsonArray.init(allocator) }, allocator);
        };

        std.debug.print("LSP: formatDocument returned {d} edits\n", .{edits.len});
        defer allocator.free(edits);

        // Convert to JSON
        var result = JsonArray.init(allocator);
        for (edits) |edit| {
            var range = JsonObject.init(allocator);
            var start_pos = JsonObject.init(allocator);
            try start_pos.put("line", JsonValue{ .integer = @intCast(edit.range.start.line) });
            try start_pos.put("character", JsonValue{ .integer = @intCast(edit.range.start.character) });
            var end_pos = JsonObject.init(allocator);
            try end_pos.put("line", JsonValue{ .integer = @intCast(edit.range.end.line) });
            try end_pos.put("character", JsonValue{ .integer = @intCast(edit.range.end.character) });
            try range.put("start", JsonValue{ .object = start_pos });
            try range.put("end", JsonValue{ .object = end_pos });

            var edit_obj = JsonObject.init(allocator);
            try edit_obj.put("range", JsonValue{ .object = range });
            try edit_obj.put("newText", JsonValue{ .string = edit.new_text });

            try result.append(JsonValue{ .object = edit_obj });
        }

        return protocol.makeResponse(id, JsonValue{ .array = result }, allocator);
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

/// Information about a function call context for signature help
const FunctionCallContext = struct {
    function_name: []const u8,
    active_param: usize,
};

/// Find the function call context at a given position
/// Returns the function name being called and which parameter is active
fn findFunctionCallContext(content: []const u8, line: usize, char: usize) ?FunctionCallContext {
    // Get position in content
    var current_line: usize = 0;
    var line_start: usize = 0;

    for (content, 0..) |c, i| {
        if (current_line == line) {
            line_start = i;
            break;
        }
        if (c == '\n') current_line += 1;
    }

    const cursor_pos = line_start + char;
    if (cursor_pos >= content.len) return null;

    // Search backwards from cursor to find the opening parenthesis
    var paren_depth: i32 = 0;
    var comma_count: usize = 0;
    var i: usize = cursor_pos;

    while (i > 0) {
        i -= 1;
        const c = content[i];

        if (c == ')') {
            paren_depth += 1;
        } else if (c == '(') {
            if (paren_depth == 0) {
                // Found our opening paren, now find the function name
                if (i > 0) {
                    // Skip whitespace before the paren
                    var j = i - 1;
                    while (j > 0 and (content[j] == ' ' or content[j] == '\t')) {
                        j -= 1;
                    }

                    // Find the end of the function name
                    const name_end = j + 1;

                    // Find the start of the function name
                    while (j > 0 and isWordChar(content[j])) {
                        j -= 1;
                    }
                    if (!isWordChar(content[j])) j += 1;

                    if (name_end > j) {
                        return .{
                            .function_name = content[j..name_end],
                            .active_param = comma_count,
                        };
                    }
                }
                return null;
            } else {
                paren_depth -= 1;
            }
        } else if (c == ',' and paren_depth == 0) {
            comma_count += 1;
        } else if (c == '\n') {
            // Don't search across too many lines
            if (current_line > 0) {
                current_line -= 1;
            } else {
                break;
            }
        }
    }

    return null;
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

/// Get the word and its column range at a given position
const WordRange = struct {
    word: []const u8,
    start_col: usize,
    end_col: usize,
};

fn getWordRangeAtPosition(content: []const u8, line: usize, char: usize) ?WordRange {
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
    return .{
        .word = line_content[word_start..word_end],
        .start_col = word_start,
        .end_col = word_end,
    };
}

fn isWordChar(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_' or c == '%';
}

/// Check if a name is a reserved keyword (both Cot and DBL)
fn isKeyword(name: []const u8) bool {
    const keywords = [_][]const u8{
        // Cot keywords
        "fn",       "struct",   "enum",     "union",    "const",
        "var",      "let",      "if",       "else",     "for",
        "while",    "loop",     "break",    "continue", "return",
        "match",    "true",     "false",    "null",     "and",
        "or",       "not",      "import",   "pub",      "test",
        // DBL keywords
        "main",     "endmain",  "proc",     "endproc",  "function",
        "endfunction", "subroutine", "endsubroutine", "record",
        "endrecord", "begin",   "end",      "if",       "else",
        "then",     "using",    "case",     "endusing", "do",
        "until",    "while",    "for",      "from",     "thru",
        "by",       "exitloop", "nextloop", "goto",     "call",
        "xcall",    "xreturn",  "freturn",  "return",   "stop",
        "data",     "global",   "external", "common",   "literal",
        "group",    "endgroup", "class",    "endclass", "method",
        "endmethod", "property", "endproperty", "namespace",
        "endnamespace", "enum", "endenum",  "structure", "endstructure",
        "assert",   "test",     "endtest",
    };

    for (keywords) |kw| {
        if (std.mem.eql(u8, name, kw)) return true;
    }
    return false;
}

/// Check if a name is a valid identifier
fn isValidIdentifier(name: []const u8) bool {
    if (name.len == 0) return false;

    // First character must be letter or underscore
    const first = name[0];
    if (!std.ascii.isAlphabetic(first) and first != '_') return false;

    // Rest must be alphanumeric or underscore
    for (name[1..]) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_') return false;
    }

    return true;
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
        .{ "union", "**union** Name { variants }\n\nUnion definition. All variants share the same memory (overlay)." },
        .{ "view", "**view** name: Type = @base [+ offset]\n\nMemory view/alias. Creates an alias to another field's memory." },
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

/// Semantic token for sorting
const SemanticToken = struct {
    line: usize,
    column: usize,
    length: usize,
    token_type: usize,
};

/// Find comments in source code and add them as semantic tokens
fn findComments(allocator: Allocator, source: []const u8, tokens_list: *std.ArrayListUnmanaged(SemanticToken), comment_prefix: []const u8) void {
    const STI = protocol.SemanticTokenIndex;
    var line: usize = 0;
    var line_start: usize = 0;
    var i: usize = 0;

    while (i < source.len) {
        if (source[i] == '\n') {
            line += 1;
            line_start = i + 1;
            i += 1;
            continue;
        }

        // Check for line comment (// or ;)
        if (i + comment_prefix.len <= source.len and std.mem.eql(u8, source[i .. i + comment_prefix.len], comment_prefix)) {
            const comment_start = i;
            const column = i - line_start;

            // Find end of line
            while (i < source.len and source[i] != '\n') : (i += 1) {}

            const comment_len = i - comment_start;
            tokens_list.append(allocator, .{
                .line = line,
                .column = column,
                .length = comment_len,
                .token_type = STI.Comment,
            }) catch {};
            continue;
        }

        // Check for block comment /* */
        if (i + 2 <= source.len and source[i] == '/' and source[i + 1] == '*') {
            const comment_start_line = line;
            const comment_start_col = i - line_start;
            var comment_len: usize = 2;
            i += 2;

            // Find end of block comment
            while (i + 1 < source.len) {
                if (source[i] == '\n') {
                    line += 1;
                    line_start = i + 1;
                } else if (source[i] == '*' and source[i + 1] == '/') {
                    comment_len = i + 2 - (comment_start_col + (line_start - comment_start_col));
                    i += 2;
                    break;
                }
                i += 1;
                comment_len += 1;
            }

            // For multiline comments, just emit the first line for now
            tokens_list.append(allocator, .{
                .line = comment_start_line,
                .column = comment_start_col,
                .length = comment_len,
                .token_type = STI.Comment,
            }) catch {};
            continue;
        }

        i += 1;
    }
}

/// Build semantic tokens for Cot syntax (modern syntax with // comments)
fn buildCotSemanticTokens(allocator: Allocator, source: []const u8, data: *JsonArray) !void {
    const cot = @import("cot");

    var lexer = cot.lexer.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    // Collect all semantic tokens (including comments)
    var tokens_list = std.ArrayListUnmanaged(SemanticToken).empty;
    defer tokens_list.deinit(allocator);

    // First pass: find comments (lexer skips them)
    findComments(allocator, source, &tokens_list, "//");

    // Second pass: process lexer tokens with context awareness
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];
        const prev_token: ?cot.token.Token = if (i > 0) tokens[i - 1] else null;
        const next_token: ?cot.token.Token = if (i + 1 < tokens.len) tokens[i + 1] else null;

        const token_type: ?usize = getCotSemanticTokenType(token, prev_token, next_token);

        if (token_type) |tt| {
            const line = if (token.line > 0) token.line - 1 else 0;
            const char = if (token.column > 0) token.column - 1 else 0;

            try tokens_list.append(allocator, .{
                .line = line,
                .column = char,
                .length = token.lexeme.len,
                .token_type = tt,
            });
        }
    }

    // Sort tokens by position
    std.mem.sort(SemanticToken, tokens_list.items, {}, struct {
        fn lessThan(_: void, a: SemanticToken, b: SemanticToken) bool {
            if (a.line != b.line) return a.line < b.line;
            return a.column < b.column;
        }
    }.lessThan);

    // Emit delta-encoded tokens
    var prev_line: usize = 0;
    var prev_char: usize = 0;

    for (tokens_list.items) |st| {
        const delta_line = st.line -| prev_line;
        const delta_char = if (delta_line == 0) st.column -| prev_char else st.column;

        try data.append(JsonValue{ .integer = @intCast(delta_line) });
        try data.append(JsonValue{ .integer = @intCast(delta_char) });
        try data.append(JsonValue{ .integer = @intCast(st.length) });
        try data.append(JsonValue{ .integer = @intCast(st.token_type) });
        try data.append(JsonValue{ .integer = 0 }); // No modifiers

        prev_line = st.line;
        prev_char = st.column;
    }
}

/// Get semantic token type for Cot token with context awareness
fn getCotSemanticTokenType(token: anytype, prev_token: anytype, next_token: anytype) ?usize {
    const cot = @import("cot");
    const TokenType = cot.token.TokenType;
    const STI = protocol.SemanticTokenIndex;

    // Handle identifiers with context
    if (token.type == TokenType.identifier) {
        // Check context from previous token
        if (prev_token) |prev| {
            // After dot = property access
            if (prev.type == TokenType.period) {
                return STI.Property;
            }
        }

        // Check context from next token
        if (next_token) |next| {
            // Before lparen = function call
            if (next.type == TokenType.lparen) {
                return STI.Function;
            }
        }

        // Default: regular variable
        return STI.Variable;
    }

    // Strings
    if (token.type == TokenType.string_literal) return STI.String;

    // Numbers
    if (token.type == TokenType.integer_literal or token.type == TokenType.decimal_literal) return STI.Number;

    // Operators
    switch (token.type) {
        TokenType.plus, TokenType.minus, TokenType.star, TokenType.slash, TokenType.equals,
        TokenType.eq, TokenType.ne, TokenType.lt, TokenType.le, TokenType.gt, TokenType.ge,
        TokenType.amp_amp, TokenType.pipe_pipe, TokenType.bang,
        => return STI.Operator,
        else => {},
    }

    // Keyword categorization
    const name = @tagName(token.type);
    if (name.len > 3 and name[0] == 'k' and name[1] == 'w' and name[2] == '_') {
        // Function/procedure keywords
        if (token.type == TokenType.kw_fn or token.type == TokenType.kw_pub) {
            return STI.Function;
        }

        // Type declaration keywords
        if (token.type == TokenType.kw_struct or token.type == TokenType.kw_union or
            token.type == TokenType.kw_view or token.type == TokenType.kw_enum or
            token.type == TokenType.kw_trait or token.type == TokenType.kw_impl)
        {
            return STI.Class;
        }

        // Namespace/module keywords
        if (token.type == TokenType.kw_import or token.type == TokenType.kw_type) {
            return STI.Namespace;
        }

        // Error handling keywords
        if (token.type == TokenType.kw_try or token.type == TokenType.kw_catch or
            token.type == TokenType.kw_throw)
        {
            return STI.Exception;
        }

        // Access modifiers
        if (token.type == TokenType.kw_pub or
            token.type == TokenType.kw_const or token.type == TokenType.kw_comptime)
        {
            return STI.Modifier;
        }

        // Default: control flow keyword
        return STI.Keyword;
    }

    return null;
}

/// Build semantic tokens for DBL syntax (legacy syntax with ; comments)
fn buildDblSemanticTokens(allocator: Allocator, source: []const u8, data: *JsonArray) !void {
    const dbl = @import("dbl");

    var lexer = dbl.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch return;
    defer allocator.free(tokens);

    // Collect all semantic tokens (including comments)
    var tokens_list = std.ArrayListUnmanaged(SemanticToken).empty;
    defer tokens_list.deinit(allocator);

    // First pass: find comments (lexer skips them)
    // DBL uses ; for line comments, but also supports // and /* */
    findComments(allocator, source, &tokens_list, ";");
    findComments(allocator, source, &tokens_list, "//");

    // Second pass: process lexer tokens with context awareness
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];
        const prev_token: ?dbl.Token = if (i > 0) tokens[i - 1] else null;
        const next_token: ?dbl.Token = if (i + 1 < tokens.len) tokens[i + 1] else null;
        const next_next_token: ?dbl.Token = if (i + 2 < tokens.len) tokens[i + 2] else null;

        const token_type: ?usize = getDblSemanticTokenType(token, prev_token, next_token, next_next_token);

        if (token_type) |tt| {
            const line = if (token.line > 0) token.line - 1 else 0;
            const char = if (token.column > 0) token.column - 1 else 0;

            try tokens_list.append(allocator, .{
                .line = line,
                .column = char,
                .length = token.lexeme.len,
                .token_type = tt,
            });
        }
    }

    // Sort tokens by position
    std.mem.sort(SemanticToken, tokens_list.items, {}, struct {
        fn lessThan(_: void, a: SemanticToken, b: SemanticToken) bool {
            if (a.line != b.line) return a.line < b.line;
            return a.column < b.column;
        }
    }.lessThan);

    // Emit delta-encoded tokens
    var prev_line: usize = 0;
    var prev_char: usize = 0;

    for (tokens_list.items) |st| {
        const delta_line = st.line -| prev_line;
        const delta_char = if (delta_line == 0) st.column -| prev_char else st.column;

        try data.append(JsonValue{ .integer = @intCast(delta_line) });
        try data.append(JsonValue{ .integer = @intCast(delta_char) });
        try data.append(JsonValue{ .integer = @intCast(st.length) });
        try data.append(JsonValue{ .integer = @intCast(st.token_type) });
        try data.append(JsonValue{ .integer = 0 }); // No modifiers

        prev_line = st.line;
        prev_char = st.column;
    }
}

/// Get semantic token type for DBL token with context awareness
fn getDblSemanticTokenType(token: anytype, prev_token: anytype, next_token: anytype, next_next_token: anytype) ?usize {
    const dbl = @import("dbl");
    const TokenType = dbl.TokenType;
    const STI = protocol.SemanticTokenIndex;

    // Handle identifiers with context
    if (token.type == TokenType.identifier) {
        // Check if it's a label reference after onerror or goto
        if (prev_token) |prev| {
            if (prev.type == TokenType.kw_onerror or prev.type == TokenType.kw_goto) {
                return STI.Label;
            }
            // Check if it's a type reference after comma (e.g., ",customer")
            if (prev.type == TokenType.comma) {
                return STI.Type;
            }
            // Check if it's a property after dot (e.g., "app.ch_cust")
            if (prev.type == TokenType.period) {
                return STI.Property;
            }
        }

        // Check if it's a label definition (identifier followed by comma, where comma is NOT followed by a type)
        if (next_token) |next| {
            if (next.type == TokenType.comma) {
                // Check what comes after the comma
                if (next_next_token) |after_comma| {
                    // If comma is followed by type specifier or identifier, it's a field declaration
                    // If comma is followed by keyword/newline/etc, it's a label definition
                    switch (after_comma.type) {
                        TokenType.identifier, TokenType.at => {}, // Field declaration like "cust ,customer" or ",@type"
                        else => return STI.Label, // Label definition like "eof_reached,"
                    }
                } else {
                    // Comma at end of tokens = label definition
                    return STI.Label;
                }
            }
            // Check if it's a function call (identifier followed by lparen)
            if (next.type == TokenType.lparen) {
                // Special case: console
                if (std.mem.eql(u8, token.lexeme, "console")) {
                    return STI.Class;
                }
                return STI.Function;
            }
        }

        // Check for "console" specifically (even without following paren)
        if (std.mem.eql(u8, token.lexeme, "console")) {
            return STI.Class;
        }

        // Default: regular variable
        return STI.Variable;
    }

    // Handle percent (builtin function prefix like %trim)
    if (token.type == TokenType.percent) {
        return STI.Macro;
    }

    // Strings
    if (token.type == TokenType.string_literal) return STI.String;

    // Numbers
    if (token.type == TokenType.integer_literal or token.type == TokenType.decimal_literal) return STI.Number;

    // Operators
    switch (token.type) {
        TokenType.plus, TokenType.minus, TokenType.star, TokenType.slash, TokenType.hash, TokenType.equals,
        TokenType.op_eq, TokenType.op_ne, TokenType.op_lt, TokenType.op_le, TokenType.op_gt, TokenType.op_ge,
        TokenType.op_and, TokenType.op_or, TokenType.op_not, TokenType.op_xor,
        => return STI.Operator,
        else => {},
    }

    // Detailed keyword categorization (matching the old working LSP)
    switch (token.type) {
        // Function/subroutine declarations -> function
        TokenType.kw_main, TokenType.kw_endmain, TokenType.kw_subroutine, TokenType.kw_endsubroutine,
        TokenType.kw_function, TokenType.kw_endfunction, TokenType.kw_method, TokenType.kw_endmethod,
        => return STI.Function,

        // Type declarations -> class
        TokenType.kw_record, TokenType.kw_endrecord, TokenType.kw_structure, TokenType.kw_endstructure,
        TokenType.kw_class, TokenType.kw_endclass, TokenType.kw_interface, TokenType.kw_endinterface,
        TokenType.kw_group, TokenType.kw_endgroup,
        => return STI.Class,

        // Namespace keywords -> namespace
        TokenType.kw_namespace, TokenType.kw_endnamespace, TokenType.kw_import,
        TokenType.kw_common, TokenType.kw_endcommon, TokenType.kw_literal, TokenType.kw_endliteral,
        => return STI.Namespace,

        // I/O keywords -> IO (regexp)
        TokenType.kw_find, TokenType.kw_unlock, TokenType.kw_flush, TokenType.kw_accept,
        => return STI.IO,

        // Data manipulation -> Data (decorator)
        TokenType.kw_clear, TokenType.kw_init, TokenType.kw_incr, TokenType.kw_decr,
        TokenType.kw_locase, TokenType.kw_upcase,
        => return STI.Data,

        // Exception handling -> Exception (event)
        TokenType.kw_try, TokenType.kw_catch, TokenType.kw_finally, TokenType.kw_endtry,
        TokenType.kw_throw, TokenType.kw_onerror, TokenType.kw_offerror,
        => return STI.Exception,

        // Access modifiers -> modifier
        TokenType.kw_public, TokenType.kw_private, TokenType.kw_protected,
        TokenType.kw_static, TokenType.kw_virtual, TokenType.kw_override,
        => return STI.Modifier,

        // OOP keywords -> class
        TokenType.kw_new, TokenType.kw_extends, TokenType.kw_implements,
        TokenType.kw_property, TokenType.kw_endproperty,
        => return STI.Class,

        // Control flow keywords -> keyword
        TokenType.kw_if, TokenType.kw_then, TokenType.kw_else, TokenType.kw_begin,
        TokenType.kw_case, TokenType.kw_endcase, TokenType.kw_select,
        TokenType.kw_do, TokenType.kw_while, TokenType.kw_endwhile, TokenType.kw_for,
        TokenType.kw_from, TokenType.kw_thru, TokenType.kw_until, TokenType.kw_foreach,
        TokenType.kw_in, TokenType.kw_forever, TokenType.kw_repeat,
        TokenType.kw_exitloop, TokenType.kw_nextloop, TokenType.kw_loop,
        TokenType.kw_break, TokenType.kw_continue, TokenType.kw_step,
        TokenType.kw_return, TokenType.kw_freturn, TokenType.kw_mreturn, TokenType.kw_xreturn,
        TokenType.kw_xcall, TokenType.kw_call, TokenType.kw_goto, TokenType.kw_stop, TokenType.kw_exit,
        TokenType.kw_proc, TokenType.kw_end, TokenType.kw_data, TokenType.kw_using, TokenType.kw_endusing,
        => return STI.Keyword,

        // Boolean/null constants
        TokenType.kw_true, TokenType.kw_false,
        => return STI.Number, // Treat as constants

        // Compile-time keywords -> modifier
        TokenType.kw_comptime, TokenType.kw_const, TokenType.kw_inline,
        => return STI.Modifier,

        // Parameter modifiers -> parameter
        TokenType.kw_out, TokenType.kw_inout,
        => return STI.Parameter,

        else => {},
    }

    // Fallback: check if it's any keyword we missed
    const name = @tagName(token.type);
    if (name.len > 3 and name[0] == 'k' and name[1] == 'w' and name[2] == '_') {
        return STI.Keyword;
    }

    return null;
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
                break :blk Handlers.initialize(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Initialize error: {}\n", .{err});
                    break :blk null;
                };
            } else if (std.mem.eql(u8, method.string, "initialized")) {
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "shutdown")) {
                break :blk Handlers.shutdown(&server, id.?, allocator) catch |err| {
                    std.debug.print("Shutdown error: {}\n", .{err});
                    break :blk null;
                };
            } else if (std.mem.eql(u8, method.string, "exit")) {
                break;
            } else if (std.mem.eql(u8, method.string, "textDocument/didOpen")) {
                Handlers.didOpen(&server, params) catch |err| {
                    std.debug.print("didOpen error: {}\n", .{err});
                };
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/didChange")) {
                Handlers.didChange(&server, params) catch |err| {
                    std.debug.print("didChange error: {}\n", .{err});
                };
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/didSave")) {
                Handlers.didSave(&server, params);
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/didClose")) {
                Handlers.didClose(&server, params);
                break :blk null;
            } else if (std.mem.eql(u8, method.string, "textDocument/completion")) {
                break :blk Handlers.completion(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Completion error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/signatureHelp")) {
                break :blk Handlers.signatureHelp(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("SignatureHelp error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/hover")) {
                break :blk Handlers.hover(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Hover error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/definition")) {
                break :blk Handlers.definition(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Definition error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/references")) {
                break :blk Handlers.references(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("References error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/documentHighlight")) {
                break :blk Handlers.documentHighlight(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("DocumentHighlight error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/prepareRename")) {
                break :blk Handlers.prepareRename(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("PrepareRename error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/rename")) {
                break :blk Handlers.rename(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Rename error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/documentSymbol")) {
                break :blk Handlers.documentSymbol(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("DocumentSymbol error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/semanticTokens/full")) {
                break :blk Handlers.semanticTokens(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("SemanticTokens error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else if (std.mem.eql(u8, method.string, "textDocument/formatting")) {
                break :blk Handlers.formatting(&server, id.?, params, allocator) catch |err| {
                    std.debug.print("Formatting error: {}\n", .{err});
                    break :blk protocol.makeErrorResponse(id.?, protocol.ErrorCode.InternalError, "Handler error", allocator) catch null;
                };
            } else {
                if (id) |req_id| {
                    break :blk protocol.makeErrorResponse(req_id, protocol.ErrorCode.MethodNotFound, "Method not found", allocator) catch null;
                }
                break :blk null;
            }
        };

        if (response) |resp| {
            protocol.writeMessage(resp, allocator) catch |err| {
                std.debug.print("Write error: {}\n", .{err});
            };
        }
    }
}
