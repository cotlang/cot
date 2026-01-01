//! Cot Language Server
//!
//! A Language Server Protocol implementation for Cot,
//! providing IDE features like diagnostics, completion, and hover.
//! Also includes Debug Adapter Protocol (DAP) support for debugging.

const std = @import("std");
const cot = @import("cot");
const dap = @import("dap.zig");

const json = std.json;
const Allocator = std.mem.Allocator;

/// Workspace information
const WorkspaceInfo = struct {
    name: []const u8,
    root_path: []const u8,
    is_monorepo: bool,
    apps: std.ArrayList([]const u8),
    packages: std.ArrayList([]const u8),
};

/// LSP Server state
const Server = struct {
    allocator: Allocator,
    documents: std.StringHashMap(Document),
    initialized: bool = false,
    shutdown_requested: bool = false,
    workspace_root: ?[]const u8 = null,
    workspace_info: ?WorkspaceInfo = null,
    debug_session: ?dap.DebugSession = null,

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
            self.allocator.free(doc.content);
            self.allocator.free(doc.uri);
        }
        self.documents.deinit();
        if (self.workspace_root) |root| {
            self.allocator.free(root);
        }
        if (self.workspace_info) |*info| {
            self.allocator.free(info.name);
            self.allocator.free(info.root_path);
            for (info.apps.items) |app| {
                self.allocator.free(app);
            }
            info.apps.deinit(self.allocator);
            for (info.packages.items) |pkg| {
                self.allocator.free(pkg);
            }
            info.packages.deinit(self.allocator);
        }
        if (self.debug_session) |*session| {
            session.deinit();
        }
    }

    /// Set workspace root from initialize params
    pub fn setWorkspaceRoot(self: *Self, root_uri: []const u8) !void {
        // Convert file:// URI to path
        const path = if (std.mem.startsWith(u8, root_uri, "file://"))
            root_uri[7..]
        else
            root_uri;

        self.workspace_root = try self.allocator.dupe(u8, path);

        // Try to load workspace info
        self.loadWorkspaceInfo() catch {};
    }

    /// Load workspace info from cot.json
    fn loadWorkspaceInfo(self: *Self) !void {
        const root = self.workspace_root orelse return;

        // Try to read cot.json
        var path_buf: [4096]u8 = undefined;
        const cot_json_path = try std.fmt.bufPrint(&path_buf, "{s}/cot.json", .{root});

        var file = std.fs.cwd().openFile(cot_json_path, .{}) catch return;
        defer file.close();

        var content_buf: [65536]u8 = undefined;
        const bytes_read = try file.readAll(&content_buf);
        const content = content_buf[0..bytes_read];

        // Simple JSON parsing for workspace name
        // Look for "name": "..."
        if (std.mem.indexOf(u8, content, "\"name\"")) |name_idx| {
            const after_name = content[name_idx + 7 ..];
            if (std.mem.indexOf(u8, after_name, "\"")) |start_idx| {
                const name_start = after_name[start_idx + 1 ..];
                if (std.mem.indexOf(u8, name_start, "\"")) |end_idx| {
                    const name = name_start[0..end_idx];

                    var info = WorkspaceInfo{
                        .name = try self.allocator.dupe(u8, name),
                        .root_path = try self.allocator.dupe(u8, root),
                        .is_monorepo = std.mem.indexOf(u8, content, "\"workspaces\"") != null,
                        .apps = .{},
                        .packages = .{},
                    };

                    // Discover apps and packages if monorepo
                    if (info.is_monorepo) {
                        self.discoverWorkspaceMembers(&info, root) catch {};
                    }

                    self.workspace_info = info;
                }
            }
        }
    }

    /// Discover apps and packages in workspace
    fn discoverWorkspaceMembers(self: *Self, info: *WorkspaceInfo, root: []const u8) !void {
        // Scan apps directory
        var apps_path_buf: [4096]u8 = undefined;
        const apps_path = try std.fmt.bufPrint(&apps_path_buf, "{s}/apps", .{root});

        var apps_dir = std.fs.cwd().openDir(apps_path, .{ .iterate = true }) catch {
            // Apps directory doesn't exist, skip
            return self.discoverPackages(info, root);
        };
        defer apps_dir.close();

        var apps_it = apps_dir.iterate();
        while (try apps_it.next()) |entry| {
            if (entry.kind == .directory and entry.name[0] != '.') {
                try info.apps.append(self.allocator, try self.allocator.dupe(u8, entry.name));
            }
        }

        try self.discoverPackages(info, root);
    }

    fn discoverPackages(self: *Self, info: *WorkspaceInfo, root: []const u8) !void {
        // Scan packages directory
        var pkg_path_buf: [4096]u8 = undefined;
        const pkg_path = try std.fmt.bufPrint(&pkg_path_buf, "{s}/packages", .{root});

        var pkg_dir = std.fs.cwd().openDir(pkg_path, .{ .iterate = true }) catch {
            // Packages directory doesn't exist, skip
            return;
        };
        defer pkg_dir.close();

        var pkg_it = pkg_dir.iterate();
        while (try pkg_it.next()) |entry| {
            if (entry.kind == .directory and entry.name[0] != '.') {
                try info.packages.append(self.allocator, try self.allocator.dupe(u8, entry.name));
            }
        }
    }
};

/// Document state
const Document = struct {
    uri: []const u8,
    content: []const u8,
    version: i64,
};

/// Read a line from stdin (up to newline, strips \r\n)
fn readLine(buf: []u8) !?[]const u8 {
    const stdin = std.fs.File.stdin();
    var i: usize = 0;

    while (i < buf.len) {
        var byte: [1]u8 = undefined;
        const n = stdin.read(&byte) catch return null;
        if (n == 0) return null;

        if (byte[0] == '\n') {
            // Strip trailing \r if present
            const end = if (i > 0 and buf[i - 1] == '\r') i - 1 else i;
            return buf[0..end];
        }
        buf[i] = byte[0];
        i += 1;
    }
    return buf[0..i];
}

/// Read exact number of bytes from stdin
fn readExact(buf: []u8) !void {
    const stdin = std.fs.File.stdin();
    var total: usize = 0;
    while (total < buf.len) {
        const n = stdin.read(buf[total..]) catch return error.IoError;
        if (n == 0) return error.EndOfStream;
        total += n;
    }
}

/// Write to stdout
fn writeStdout(data: []const u8) !void {
    const stdout = std.fs.File.stdout();
    _ = try stdout.write(data);
}

/// Read a JSON-RPC message from stdin
fn readMessage(allocator: Allocator) !?json.Value {
    var content_length: ?usize = null;
    var line_buf: [1024]u8 = undefined;

    // Read headers
    while (true) {
        const line = try readLine(&line_buf) orelse return null;

        if (line.len == 0) break; // Empty line signals end of headers

        if (std.mem.startsWith(u8, line, "Content-Length: ")) {
            content_length = try std.fmt.parseInt(usize, line[16..], 10);
        }
    }

    const len = content_length orelse return error.MissingContentLength;

    // Read content
    const content = try allocator.alloc(u8, len);
    defer allocator.free(content);

    try readExact(content);

    // Parse JSON
    const parsed = try json.parseFromSlice(json.Value, allocator, content, .{});
    return parsed.value;
}

/// Write a JSON-RPC message to stdout
fn writeMessage(msg: json.Value, allocator: Allocator) !void {
    // Serialize JSON using Zig 0.15's API
    const content = try json.Stringify.valueAlloc(allocator, msg, .{});
    defer allocator.free(content);

    var header_buf: [64]u8 = undefined;
    const header = try std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{content.len});

    try writeStdout(header);
    try writeStdout(content);
}

/// LSP message handlers
const Handlers = struct {
    /// Semantic token types - order matters, indices used in encoding
    const semantic_token_types = [_][]const u8{
        "namespace",      // 0 - namespace declarations
        "type",           // 1 - type references (,customer)
        "class",          // 2 - class/structure declarations
        "enum",           // 3
        "interface",      // 4
        "struct",         // 5 - structure keyword
        "parameter",      // 6
        "variable",       // 7 - identifiers
        "property",       // 8 - member access (after .)
        "function",       // 9 - function/subroutine declarations
        "method",         // 10 - method calls
        "keyword",        // 11 - control flow (if, while, for)
        "modifier",       // 12 - access modifiers (public, private)
        "comment",        // 13
        "string",         // 14
        "number",         // 15
        "operator",       // 16
        "macro",          // 17 - builtins like %trim
        "regexp",         // 18 - I/O keywords (read, write, open)
        "decorator",      // 19 - data keywords (clear, init)
        "event",          // 20 - exception keywords (try, catch)
        "label",          // 21 - labels (error handlers, goto targets)
    };

    /// Handle initialize request
    fn initialize(server: *Server, id: json.Value, params: json.Value, allocator: Allocator) !json.Value {
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
        var capabilities = json.ObjectMap.init(allocator);

        // Text document sync - full sync mode
        try capabilities.put("textDocumentSync", json.Value{ .integer = 1 });

        // Completion support
        var completion_options = json.ObjectMap.init(allocator);
        var trigger_chars = json.Array.init(allocator);
        try trigger_chars.append(json.Value{ .string = "." });
        try trigger_chars.append(json.Value{ .string = "%" });
        try completion_options.put("triggerCharacters", json.Value{ .array = trigger_chars });
        try capabilities.put("completionProvider", json.Value{ .object = completion_options });

        // Hover support
        try capabilities.put("hoverProvider", json.Value{ .bool = true });

        // Document symbol support
        try capabilities.put("documentSymbolProvider", json.Value{ .bool = true });

        // Semantic tokens support - context-aware for better color variety
        var semantic_tokens_options = json.ObjectMap.init(allocator);
        var token_types = json.Array.init(allocator);
        for (semantic_token_types) |tt| {
            try token_types.append(json.Value{ .string = tt });
        }
        var legend = json.ObjectMap.init(allocator);
        try legend.put("tokenTypes", json.Value{ .array = token_types });
        try legend.put("tokenModifiers", json.Value{ .array = json.Array.init(allocator) });
        try semantic_tokens_options.put("legend", json.Value{ .object = legend });
        try semantic_tokens_options.put("full", json.Value{ .bool = true });
        try capabilities.put("semanticTokensProvider", json.Value{ .object = semantic_tokens_options });

        // Build result
        var result = json.ObjectMap.init(allocator);
        try result.put("capabilities", json.Value{ .object = capabilities });

        var server_info = json.ObjectMap.init(allocator);
        try server_info.put("name", json.Value{ .string = "cot-lsp" });
        try server_info.put("version", json.Value{ .string = "0.1.0" });
        try result.put("serverInfo", json.Value{ .object = server_info });

        return makeResponse(id, json.Value{ .object = result }, allocator);
    }

    /// Handle initialized notification
    fn initialized(server: *Server) void {
        _ = server;
    }

    /// Handle shutdown request
    fn shutdown(server: *Server, id: json.Value, allocator: Allocator) !json.Value {
        server.shutdown_requested = true;
        return makeResponse(id, json.Value.null, allocator);
    }

    /// Handle textDocument/didOpen notification
    fn didOpen(server: *Server, params: json.Value) !void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const text = text_document.object.get("text") orelse return;
        const version = text_document.object.get("version") orelse return;

        const uri_str = try server.allocator.dupe(u8, uri.string);
        const content = try server.allocator.dupe(u8, text.string);

        try server.documents.put(uri_str, .{
            .uri = uri_str,
            .content = content,
            .version = version.integer,
        });

        // Publish diagnostics for the new document
        try publishDiagnostics(server, uri_str);
    }

    /// Handle textDocument/didChange notification
    fn didChange(server: *Server, params: json.Value) !void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;
        const version = text_document.object.get("version") orelse return;
        const changes = params.object.get("contentChanges") orelse return;

        if (server.documents.getPtr(uri.string)) |doc| {
            if (changes.array.items.len > 0) {
                const last_change = changes.array.items[changes.array.items.len - 1];
                if (last_change.object.get("text")) |new_text| {
                    server.allocator.free(doc.content);
                    doc.content = try server.allocator.dupe(u8, new_text.string);
                    doc.version = version.integer;
                    try publishDiagnostics(server, uri.string);
                }
            }
        }
    }

    /// Handle textDocument/didClose notification
    fn didClose(server: *Server, params: json.Value) void {
        const text_document = params.object.get("textDocument") orelse return;
        const uri = text_document.object.get("uri") orelse return;

        if (server.documents.fetchRemove(uri.string)) |entry| {
            server.allocator.free(entry.value.content);
            server.allocator.free(entry.value.uri);
        }
    }

    /// Handle textDocument/completion request
    fn completion(server: *Server, id: json.Value, params: json.Value, allocator: Allocator) !json.Value {
        var items = json.Array.init(allocator);

        // Extract position and document from params
        const text_document = params.object.get("textDocument") orelse
            return makeResponse(id, json.Value{ .array = items }, allocator);
        const uri = text_document.object.get("uri") orelse
            return makeResponse(id, json.Value{ .array = items }, allocator);
        const position = params.object.get("position") orelse
            return makeResponse(id, json.Value{ .array = items }, allocator);
        const line: usize = @intCast(position.object.get("line").?.integer);
        const character: usize = @intCast(position.object.get("character").?.integer);

        const doc = server.documents.get(uri.string) orelse
            return makeResponse(id, json.Value{ .array = items }, allocator);

        // Check if we're completing after a '.' (member access)
        if (getMemberAccessContext(doc.content, line, character)) |var_name| {
            // Try to get field completions for this variable
            if (getFieldCompletions(doc.content, var_name, allocator)) |fields| {
                defer allocator.free(fields);
                for (fields) |field| {
                    var item = json.ObjectMap.init(allocator);
                    try item.put("label", json.Value{ .string = field.name });
                    try item.put("kind", json.Value{ .integer = 5 }); // Field
                    try item.put("detail", json.Value{ .string = field.type_str });
                    try items.append(json.Value{ .object = item });
                }
                return makeResponse(id, json.Value{ .array = items }, allocator);
            }
        }

        const keywords = [_][]const u8{
            "record",    "endrecord", "proc",    "end",      "main",
            "subroutine","function",  "if",      "then",     "else",
            "begin",     "while",     "do",      "until",    "for",
            "from",      "thru",      "case",    "using",    "endusing",
            "xcall",     "return",    "goto",    "display",  "open",
            "close",     "read",      "write",   "store",    "delete",
            "find",      "clear",     "init",    "incr",     "decr",
        };

        for (keywords) |kw| {
            var item = json.ObjectMap.init(allocator);
            try item.put("label", json.Value{ .string = kw });
            try item.put("kind", json.Value{ .integer = 14 }); // Keyword
            try item.put("detail", json.Value{ .string = "Cot keyword" });
            try items.append(json.Value{ .object = item });
        }

        const builtins = [_][]const u8{
            "%size", "%len", "%trim", "%atrim", "%string", "%integer",
            "%date", "%time", "%abs", "%int", "%frac", "%round",
        };

        for (builtins) |fn_name| {
            var item = json.ObjectMap.init(allocator);
            try item.put("label", json.Value{ .string = fn_name });
            try item.put("kind", json.Value{ .integer = 3 }); // Function
            try item.put("detail", json.Value{ .string = "Built-in function" });
            try items.append(json.Value{ .object = item });
        }

        // Framework-specific completions (only if in a workspace)
        if (server.workspace_info != null) {
            // API route handler patterns
            const api_patterns = [_]struct { label: []const u8, detail: []const u8, snippet: []const u8 }{
                .{ .label = "subroutine get", .detail = "GET request handler", .snippet = "subroutine get\n    req         ,@request\n    res         ,@response\nproc\n    res.json('{\"status\": \"ok\"}')\n    xreturn\nendsubroutine" },
                .{ .label = "subroutine post", .detail = "POST request handler", .snippet = "subroutine post\n    req         ,@request\n    res         ,@response\nproc\n    data body, a1000\n    body = req.body\n    res.status(201)\n    res.json('{\"created\": true}')\n    xreturn\nendsubroutine" },
                .{ .label = "subroutine put", .detail = "PUT request handler", .snippet = "subroutine put\n    req         ,@request\n    res         ,@response\nproc\n    res.json('{\"updated\": true}')\n    xreturn\nendsubroutine" },
                .{ .label = "subroutine delete", .detail = "DELETE request handler", .snippet = "subroutine delete\n    req         ,@request\n    res         ,@response\nproc\n    res.status(204)\n    xreturn\nendsubroutine" },
            };

            for (api_patterns) |pattern| {
                var item = json.ObjectMap.init(allocator);
                try item.put("label", json.Value{ .string = pattern.label });
                try item.put("kind", json.Value{ .integer = 15 }); // Snippet
                try item.put("detail", json.Value{ .string = pattern.detail });
                try item.put("insertText", json.Value{ .string = pattern.snippet });
                try item.put("insertTextFormat", json.Value{ .integer = 2 }); // Snippet format
                try items.append(json.Value{ .object = item });
            }

            // HTTP request/response objects
            const http_objects = [_]struct { label: []const u8, detail: []const u8 }{
                .{ .label = "req.body", .detail = "Request body content" },
                .{ .label = "req.method", .detail = "HTTP method (GET, POST, etc.)" },
                .{ .label = "req.path", .detail = "Request path" },
                .{ .label = "req.query", .detail = "Query string parameters" },
                .{ .label = "req.getHeader", .detail = "Get request header value" },
                .{ .label = "req.getParam", .detail = "Get route parameter value" },
                .{ .label = "res.json", .detail = "Send JSON response" },
                .{ .label = "res.html", .detail = "Send HTML response" },
                .{ .label = "res.text", .detail = "Send plain text response" },
                .{ .label = "res.status", .detail = "Set response status code" },
                .{ .label = "res.setHeader", .detail = "Set response header" },
                .{ .label = "res.redirect", .detail = "Redirect to URL" },
            };

            for (http_objects) |obj| {
                var item = json.ObjectMap.init(allocator);
                try item.put("label", json.Value{ .string = obj.label });
                try item.put("kind", json.Value{ .integer = 6 }); // Property
                try item.put("detail", json.Value{ .string = obj.detail });
                try items.append(json.Value{ .object = item });
            }

            // Add workspace apps as import completions
            if (server.workspace_info) |info| {
                for (info.apps.items) |app| {
                    var item = json.ObjectMap.init(allocator);
                    try item.put("label", json.Value{ .string = app });
                    try item.put("kind", json.Value{ .integer = 9 }); // Module
                    try item.put("detail", json.Value{ .string = "Workspace app" });
                    try items.append(json.Value{ .object = item });
                }

                for (info.packages.items) |pkg| {
                    var item = json.ObjectMap.init(allocator);
                    try item.put("label", json.Value{ .string = pkg });
                    try item.put("kind", json.Value{ .integer = 9 }); // Module
                    try item.put("detail", json.Value{ .string = "Workspace package" });
                    try items.append(json.Value{ .object = item });
                }
            }
        }

        return makeResponse(id, json.Value{ .array = items }, allocator);
    }

    /// Handle textDocument/hover request
    fn hover(server: *Server, id: json.Value, params: json.Value, allocator: Allocator) !json.Value {
        const text_document = params.object.get("textDocument") orelse
            return makeResponse(id, json.Value.null, allocator);
        const uri = text_document.object.get("uri") orelse
            return makeResponse(id, json.Value.null, allocator);
        const position = params.object.get("position") orelse
            return makeResponse(id, json.Value.null, allocator);

        const doc = server.documents.get(uri.string) orelse
            return makeResponse(id, json.Value.null, allocator);

        const line_num = @as(usize, @intCast(position.object.get("line").?.integer));
        const char_num = @as(usize, @intCast(position.object.get("character").?.integer));

        const word = getWordAtPosition(doc.content, line_num, char_num) orelse
            return makeResponse(id, json.Value.null, allocator);

        const hover_text = getKeywordDoc(word) orelse
            return makeResponse(id, json.Value.null, allocator);

        var contents = json.ObjectMap.init(allocator);
        try contents.put("kind", json.Value{ .string = "markdown" });
        try contents.put("value", json.Value{ .string = hover_text });

        var result = json.ObjectMap.init(allocator);
        try result.put("contents", json.Value{ .object = contents });

        return makeResponse(id, json.Value{ .object = result }, allocator);
    }

    /// Handle textDocument/documentSymbol request
    fn documentSymbol(server: *Server, id: json.Value, params: json.Value, allocator: Allocator) !json.Value {
        const text_document = params.object.get("textDocument") orelse
            return makeResponse(id, json.Value{ .array = json.Array.init(allocator) }, allocator);
        const uri = text_document.object.get("uri") orelse
            return makeResponse(id, json.Value{ .array = json.Array.init(allocator) }, allocator);

        const doc = server.documents.get(uri.string) orelse
            return makeResponse(id, json.Value{ .array = json.Array.init(allocator) }, allocator);

        var symbols = json.Array.init(allocator);

        // Parse the document to find symbols
        var lex = cot.lexer.Lexer.init(doc.content);
        const tokens = lex.tokenize(allocator) catch
            return makeResponse(id, json.Value{ .array = symbols }, allocator);
        defer allocator.free(tokens);

        var i: usize = 0;
        while (i < tokens.len) : (i += 1) {
            const token = tokens[i];

            // Check for record declarations
            if (token.type == .kw_record) {
                if (i + 1 < tokens.len and tokens[i + 1].type == .identifier) {
                    var symbol = json.ObjectMap.init(allocator);
                    try symbol.put("name", json.Value{ .string = tokens[i + 1].lexeme });
                    try symbol.put("kind", json.Value{ .integer = 23 }); // Struct

                    var range = json.ObjectMap.init(allocator);
                    var start = json.ObjectMap.init(allocator);
                    try start.put("line", json.Value{ .integer = @intCast(token.line) });
                    try start.put("character", json.Value{ .integer = 0 });
                    var end_pos = json.ObjectMap.init(allocator);
                    try end_pos.put("line", json.Value{ .integer = @intCast(token.line) });
                    try end_pos.put("character", json.Value{ .integer = 80 });
                    try range.put("start", json.Value{ .object = start });
                    try range.put("end", json.Value{ .object = end_pos });
                    try symbol.put("range", json.Value{ .object = range });
                    try symbol.put("selectionRange", json.Value{ .object = range });

                    try symbols.append(json.Value{ .object = symbol });
                }
            } else if (token.type == .kw_subroutine or token.type == .kw_function) {
                if (i + 1 < tokens.len and tokens[i + 1].type == .identifier) {
                    var symbol = json.ObjectMap.init(allocator);
                    try symbol.put("name", json.Value{ .string = tokens[i + 1].lexeme });
                    try symbol.put("kind", json.Value{ .integer = 12 }); // Function

                    var range = json.ObjectMap.init(allocator);
                    var start = json.ObjectMap.init(allocator);
                    try start.put("line", json.Value{ .integer = @intCast(token.line) });
                    try start.put("character", json.Value{ .integer = 0 });
                    var end_pos = json.ObjectMap.init(allocator);
                    try end_pos.put("line", json.Value{ .integer = @intCast(token.line) });
                    try end_pos.put("character", json.Value{ .integer = 80 });
                    try range.put("start", json.Value{ .object = start });
                    try range.put("end", json.Value{ .object = end_pos });
                    try symbol.put("range", json.Value{ .object = range });
                    try symbol.put("selectionRange", json.Value{ .object = range });

                    try symbols.append(json.Value{ .object = symbol });
                }
            }
        }

        return makeResponse(id, json.Value{ .array = symbols }, allocator);
    }

    /// Handle textDocument/semanticTokens/full request
    fn semanticTokens(server: *Server, id: json.Value, params: json.Value, allocator: Allocator) !json.Value {
        const text_document = params.object.get("textDocument") orelse
            return makeResponse(id, json.Value.null, allocator);
        const uri = text_document.object.get("uri") orelse
            return makeResponse(id, json.Value.null, allocator);

        const doc = server.documents.get(uri.string) orelse
            return makeResponse(id, json.Value.null, allocator);

        // Tokenize the document
        var lex = cot.lexer.Lexer.init(doc.content);
        const tokens = lex.tokenize(allocator) catch
            return makeResponse(id, json.Value.null, allocator);
        defer allocator.free(tokens);

        // Build semantic tokens data array
        // Format: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
        var data = json.Array.init(allocator);

        var prev_line: usize = 0;
        var prev_char: usize = 0;

        // Context-aware token analysis
        var i: usize = 0;
        while (i < tokens.len) : (i += 1) {
            const token = tokens[i];
            const prev_token: ?cot.token.Token = if (i > 0) tokens[i - 1] else null;
            const next_token: ?cot.token.Token = if (i + 1 < tokens.len) tokens[i + 1] else null;
            const next_next_token: ?cot.token.Token = if (i + 2 < tokens.len) tokens[i + 2] else null;

            // Determine token type with context awareness
            const token_type: ?usize = getContextAwareTokenType(token, prev_token, next_token, next_next_token);

            if (token_type) |tt| {
                // Calculate deltas - convert from 1-based (lexer) to 0-based (LSP)
                const line = token.line - 1;
                const char = token.column - 1;
                const delta_line = line - prev_line;
                const delta_char = if (delta_line == 0) char - prev_char else char;

                try data.append(json.Value{ .integer = @intCast(delta_line) });
                try data.append(json.Value{ .integer = @intCast(delta_char) });
                try data.append(json.Value{ .integer = @intCast(token.lexeme.len) });
                try data.append(json.Value{ .integer = @intCast(tt) });
                try data.append(json.Value{ .integer = 0 }); // No modifiers

                prev_line = line;
                prev_char = char;
            }
        }

        var result = json.ObjectMap.init(allocator);
        try result.put("data", json.Value{ .array = data });

        return makeResponse(id, json.Value{ .object = result }, allocator);
    }

    /// Context-aware token type determination
    fn getContextAwareTokenType(
        token: cot.token.Token,
        prev_token: ?cot.token.Token,
        next_token: ?cot.token.Token,
        next_next_token: ?cot.token.Token,
    ) ?usize {
        // Handle identifiers with context
        if (token.type == .identifier) {
            // Check if it's a label reference after onerror or goto
            if (prev_token) |prev| {
                if (prev.type == .kw_onerror or prev.type == .kw_goto) {
                    return 21; // label
                }
                // Check if it's a type reference after comma (e.g., ",customer")
                if (prev.type == .comma) {
                    return 1; // type
                }
                // Check if it's a property after dot (e.g., "app.ch_cust")
                if (prev.type == .period) {
                    return 8; // property
                }
            }

            // Check if it's a label definition (identifier followed by comma, where comma is NOT followed by a type)
            if (next_token) |next| {
                if (next.type == .comma) {
                    // Check what comes after the comma
                    if (next_next_token) |after_comma| {
                        // If comma is followed by type specifier or identifier, it's a field declaration
                        // If comma is followed by keyword/newline/etc, it's a label definition
                        switch (after_comma.type) {
                            .identifier, .at => {}, // Field declaration like "cust ,customer" or ",@type"
                            else => return 21, // Label definition like "eof_reached,"
                        }
                    } else {
                        // Comma at end of tokens = label definition
                        return 21;
                    }
                }
                // Check if it's a function call (identifier followed by lparen)
                if (next.type == .lparen) {
                    // Special case: console.log
                    if (std.mem.eql(u8, token.lexeme, "console")) {
                        return 2; // class (for console)
                    }
                    return 9; // function
                }
            }

            // Check for "console" specifically (even without following paren)
            if (std.mem.eql(u8, token.lexeme, "console")) {
                return 2; // class
            }

            // Default: regular variable
            return 7; // variable
        }

        // Handle percent (builtin function prefix)
        if (token.type == .percent) {
            return 17; // macro (for builtins)
        }

        // Use the standard token type mapping for everything else
        return getSemanticTokenType(token.type);
    }

    /// Map Cot token types to semantic token indices
    fn getSemanticTokenType(token_type: cot.token.TokenType) ?usize {
        return switch (token_type) {
            // Function/subroutine declarations -> 9 (function)
            .kw_main, .kw_endmain, .kw_subroutine, .kw_endsubroutine,
            .kw_function, .kw_endfunction, .kw_method, .kw_endmethod,
            => 9, // function

            // Type declarations -> 2 (class)
            .kw_record, .kw_endrecord, .kw_structure, .kw_endstructure,
            .kw_class, .kw_endclass, .kw_interface, .kw_endinterface,
            .kw_enum, .kw_endenum, .kw_delegate, .kw_enddelegate,
            .kw_group, .kw_endgroup,
            => 2, // class/type declaration

            // Namespace keywords -> 0 (namespace)
            .kw_namespace, .kw_endnamespace, .kw_import, .kw_using, .kw_endusing,
            .kw_common, .kw_endcommon, .kw_literal, .kw_endliteral,
            => 0, // namespace

            // Control flow -> 11 (keyword)
            .kw_if, .kw_then, .kw_else, .kw_begin, .kw_case, .kw_endcase,
            .kw_select, .kw_default,
            .kw_do, .kw_while, .kw_endwhile, .kw_for, .kw_from, .kw_thru,
            .kw_until, .kw_foreach, .kw_in, .kw_forever, .kw_repeat,
            .kw_exitloop, .kw_nextloop,
            .kw_return, .kw_freturn, .kw_mreturn, .kw_xreturn,
            .kw_xcall, .kw_call, .kw_goto, .kw_stop, .kw_exit,
            .kw_proc, .kw_end, .kw_data,
            => 11, // keyword (control flow)

            // I/O keywords -> 18 (regexp - using as I/O category)
            .kw_open, .kw_close, .kw_read, .kw_reads, .kw_write, .kw_writes,
            .kw_store, .kw_delete, .kw_find, .kw_unlock, .kw_flush,
            .kw_display, .kw_accept, .kw_get, .kw_gets, .kw_put, .kw_puts,
            => 18, // I/O operations

            // Data manipulation -> 19 (decorator)
            .kw_clear, .kw_init, .kw_incr, .kw_decr, .kw_locase, .kw_upcase,
            .kw_set,
            => 19, // data operations

            // Exception handling -> 20 (event)
            .kw_try, .kw_catch, .kw_finally, .kw_endtry,
            .kw_throw, .kw_onerror, .kw_offerror,
            => 20, // exception handling

            // OOP keywords -> 2 (class)
            .kw_new, .kw_extends, .kw_implements,
            .kw_property, .kw_endproperty,
            => 2, // class-related

            // This/parent/self -> 7 (variable)
            .kw_this, .kw_parent,
            => 7, // variable.language

            // Parameter modifiers -> 6 (parameter)
            .kw_out, .kw_inout,
            => 6, // parameter

            // Async -> 12 (modifier)
            .kw_async, .kw_await,
            => 12, // modifier

            // Access modifiers -> 12 (modifier)
            .kw_public, .kw_private, .kw_protected, .kw_internal,
            .kw_static, .kw_virtual, .kw_override, .kw_abstract, .kw_sealed,
            => 12, // modifier

            // Strings -> 14
            .string_literal => 14,

            // Numbers -> 15
            .integer_literal, .decimal_literal => 15,

            // Operators -> 16
            .plus, .minus, .star, .slash, .hash, .equals,
            .op_eq, .op_ne, .op_lt, .op_le, .op_gt, .op_ge,
            .op_and, .op_or, .op_not, .op_xor,
            .op_band, .op_bor, .op_bnot, .op_bxor,
            => 16, // operator

            // Variables/identifiers -> 7
            .identifier => 7,

            // Skip punctuation, eof, etc.
            else => null,
        };
    }

    /// Field info for completions
    const FieldInfo = struct {
        name: []const u8,
        type_str: []const u8,
    };

    /// Check if cursor is after a '.' and return the variable name before it
    fn getMemberAccessContext(content: []const u8, line: usize, character: usize) ?[]const u8 {
        // Find the line in the content
        var line_start: usize = 0;
        var current_line: usize = 0;
        for (content, 0..) |c, i| {
            if (current_line == line) {
                line_start = i;
                break;
            }
            if (c == '\n') {
                current_line += 1;
            }
        }

        // Get position in the line
        if (character == 0) return null;
        const pos = line_start + character - 1;
        if (pos >= content.len) return null;

        // Check if character before cursor is '.'
        if (content[pos] != '.') return null;

        // Find the identifier before the '.'
        const end = pos;
        var start = end;
        while (start > 0 and (std.ascii.isAlphanumeric(content[start - 1]) or content[start - 1] == '_')) {
            start -= 1;
        }

        if (start == end) return null;
        return content[start..end];
    }

    /// Get field completions for a variable by parsing the document
    fn getFieldCompletions(content: []const u8, var_name: []const u8, allocator: Allocator) ?[]FieldInfo {
        // Parse the document to get the AST
        var lex = cot.lexer.Lexer.init(content);
        const tokens = lex.tokenize(allocator) catch return null;
        defer allocator.free(tokens);

        var parse = cot.parser.Parser.init(allocator, tokens);
        defer parse.deinit();
        var program = parse.parse() catch return null;
        defer program.deinit(allocator);

        // Build structure definitions map
        var structures = std.StringHashMap([]cot.ast.FieldDef).init(allocator);
        defer structures.deinit();

        for (program.statements) |stmt| {
            if (stmt == .structure) {
                structures.put(stmt.structure.name, stmt.structure.fields) catch continue;
            }
        }

        // Find the variable's type
        var var_type: ?[]const u8 = null;
        for (program.statements) |stmt| {
            switch (stmt) {
                .record => |record| {
                    for (record.fields) |field| {
                        if (std.mem.eql(u8, field.name, var_name)) {
                            // Get the type name
                            if (field.data_type == .structure_ref) {
                                var_type = field.data_type.structure_ref;
                                break;
                            }
                        }
                    }
                },
                .subroutine_def => |sub| {
                    // Check subroutine parameters
                    for (sub.parameters) |param| {
                        if (std.mem.eql(u8, param.name, var_name)) {
                            if (param.data_type == .structure_ref) {
                                var_type = param.data_type.structure_ref;
                                break;
                            }
                        }
                    }
                    // Check subroutine local records (in body)
                    for (sub.body) |body_stmt| {
                        if (body_stmt == .record) {
                            for (body_stmt.record.fields) |field| {
                                if (std.mem.eql(u8, field.name, var_name)) {
                                    if (field.data_type == .structure_ref) {
                                        var_type = field.data_type.structure_ref;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                },
                .function_def => |func| {
                    for (func.parameters) |param| {
                        if (std.mem.eql(u8, param.name, var_name)) {
                            if (param.data_type == .structure_ref) {
                                var_type = param.data_type.structure_ref;
                                break;
                            }
                        }
                    }
                    // Check function local records (in body)
                    for (func.body) |body_stmt| {
                        if (body_stmt == .record) {
                            for (body_stmt.record.fields) |field| {
                                if (std.mem.eql(u8, field.name, var_name)) {
                                    if (field.data_type == .structure_ref) {
                                        var_type = field.data_type.structure_ref;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                },
                else => {},
            }
            if (var_type != null) break;
        }

        // If we found the type, get its fields
        if (var_type) |type_name| {
            if (structures.get(type_name)) |fields| {
                var result = allocator.alloc(FieldInfo, fields.len) catch return null;
                for (fields, 0..) |field, i| {
                    result[i] = .{
                        .name = field.name,
                        .type_str = dataTypeToString(field.data_type),
                    };
                }
                return result;
            }
        }

        return null;
    }

    /// Convert DataType to a display string
    fn dataTypeToString(dt: cot.ast.DataType) []const u8 {
        return switch (dt) {
            .alpha => |a| if (a.size) |s| blk: {
                _ = s;
                break :blk "alpha";
            } else "alpha",
            .decimal => "decimal",
            .implied_decimal => "implied decimal",
            .integer => "integer",
            .packed_decimal => "packed",
            .string => "string",
            .handle => "handle",
            .structure_ref => |name| name,
            .class_ref => |name| name,
        };
    }
};

/// Publish diagnostics for a document
fn publishDiagnostics(server: *Server, uri: []const u8) !void {
    const doc = server.documents.get(uri) orelse return;

    var diagnostics = json.Array.init(server.allocator);

    // Try to parse the document and collect errors
    var lex = cot.lexer.Lexer.init(doc.content);
    const tokens = lex.tokenize(server.allocator) catch {
        var diagnostic = json.ObjectMap.init(server.allocator);

        var range = json.ObjectMap.init(server.allocator);
        var start = json.ObjectMap.init(server.allocator);
        try start.put("line", json.Value{ .integer = 0 });
        try start.put("character", json.Value{ .integer = 0 });
        var end_pos = json.ObjectMap.init(server.allocator);
        try end_pos.put("line", json.Value{ .integer = 0 });
        try end_pos.put("character", json.Value{ .integer = 1 });
        try range.put("start", json.Value{ .object = start });
        try range.put("end", json.Value{ .object = end_pos });

        try diagnostic.put("range", json.Value{ .object = range });
        try diagnostic.put("severity", json.Value{ .integer = 1 });
        try diagnostic.put("message", json.Value{ .string = "Lexer error" });
        try diagnostic.put("source", json.Value{ .string = "cot" });

        try diagnostics.append(json.Value{ .object = diagnostic });
        try sendNotification("textDocument/publishDiagnostics", uri, diagnostics, server.allocator);
        return;
    };
    defer server.allocator.free(tokens);

    // Try to parse
    var parser = cot.parser.Parser.init(server.allocator, tokens);
    defer parser.deinit();

    _ = parser.parse() catch {
        var diagnostic = json.ObjectMap.init(server.allocator);

        const err_line = if (parser.current < tokens.len) tokens[parser.current].line else 0;

        var range = json.ObjectMap.init(server.allocator);
        var start = json.ObjectMap.init(server.allocator);
        try start.put("line", json.Value{ .integer = @intCast(err_line) });
        try start.put("character", json.Value{ .integer = 0 });
        var end_pos = json.ObjectMap.init(server.allocator);
        try end_pos.put("line", json.Value{ .integer = @intCast(err_line) });
        try end_pos.put("character", json.Value{ .integer = 80 });
        try range.put("start", json.Value{ .object = start });
        try range.put("end", json.Value{ .object = end_pos });

        try diagnostic.put("range", json.Value{ .object = range });
        try diagnostic.put("severity", json.Value{ .integer = 1 });
        try diagnostic.put("message", json.Value{ .string = "Parse error" });
        try diagnostic.put("source", json.Value{ .string = "cot" });

        try diagnostics.append(json.Value{ .object = diagnostic });
    };

    try sendNotification("textDocument/publishDiagnostics", uri, diagnostics, server.allocator);
}

/// Send a notification to the client
fn sendNotification(method: []const u8, uri: []const u8, diagnostics: json.Array, allocator: Allocator) !void {
    var params = json.ObjectMap.init(allocator);
    try params.put("uri", json.Value{ .string = uri });
    try params.put("diagnostics", json.Value{ .array = diagnostics });

    var notification = json.ObjectMap.init(allocator);
    try notification.put("jsonrpc", json.Value{ .string = "2.0" });
    try notification.put("method", json.Value{ .string = method });
    try notification.put("params", json.Value{ .object = params });

    try writeMessage(json.Value{ .object = notification }, allocator);
}

/// Build a JSON-RPC response
fn makeResponse(id: json.Value, result: json.Value, allocator: Allocator) !json.Value {
    var response = json.ObjectMap.init(allocator);
    try response.put("jsonrpc", json.Value{ .string = "2.0" });
    try response.put("id", id);
    try response.put("result", result);
    return json.Value{ .object = response };
}

/// Build a JSON-RPC error response
fn makeErrorResponse(id: json.Value, code: i64, message: []const u8, allocator: Allocator) !json.Value {
    var err_obj = json.ObjectMap.init(allocator);
    try err_obj.put("code", json.Value{ .integer = code });
    try err_obj.put("message", json.Value{ .string = message });

    var response = json.ObjectMap.init(allocator);
    try response.put("jsonrpc", json.Value{ .string = "2.0" });
    try response.put("id", id);
    try response.put("error", json.Value{ .object = err_obj });
    return json.Value{ .object = response };
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
fn getKeywordDoc(word: []const u8) ?[]const u8 {
    const docs = .{
        .{ "record", "**record** [name]\n\nDefines a data record structure." },
        .{ "proc", "**proc**\n\nStarts the procedural code section." },
        .{ "display", "**display** expr\n\nOutputs values to the terminal." },
        .{ "if", "**if** condition [then]\n\nConditional execution." },
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

/// Main entry point
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = Server.init(allocator);
    defer server.deinit();

    // Main message loop
    while (!server.shutdown_requested) {
        const msg = readMessage(allocator) catch continue orelse break;

        // Check if this is a DAP message (has "type" field instead of "method")
        if (dap.isDapMessage(msg)) {
            // Initialize debug session if needed
            if (server.debug_session == null) {
                server.debug_session = dap.DebugSession.init(allocator);
            }

            // Handle DAP request
            const result = dap.handleRequest(&server.debug_session.?, msg, allocator) catch |err| {
                std.debug.print("DAP error: {}\n", .{err});
                continue;
            };

            // Send the response
            try writeMessage(result.response, allocator);

            // Send initialized event after initialize response
            const command = msg.object.get("command");
            if (command != null and std.mem.eql(u8, command.?.string, "initialize")) {
                const init_event = try dap.makeEvent(&server.debug_session.?, "initialized", null, allocator);
                try writeMessage(init_event, allocator);
            }

            // Send any event returned by the handler
            if (result.event) |event| {
                try writeMessage(event, allocator);
            }

            continue;
        }

        // Handle LSP message
        const method = msg.object.get("method") orelse continue;
        const id = msg.object.get("id");
        const params = msg.object.get("params") orelse json.Value.null;

        const response: ?json.Value = blk: {
            if (std.mem.eql(u8, method.string, "initialize")) {
                break :blk try Handlers.initialize(&server, id.?, params, allocator);
            } else if (std.mem.eql(u8, method.string, "initialized")) {
                Handlers.initialized(&server);
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
                    break :blk try makeErrorResponse(req_id, -32601, "Method not found", allocator);
                }
                break :blk null;
            }
        };

        if (response) |resp| {
            try writeMessage(resp, allocator);
        }
    }
}

test "lsp server init" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();
    try std.testing.expect(!server.initialized);
}
