//! LSP JSON-RPC dispatch and response building.

const std = @import("std");
const document_store = @import("document_store.zig");
const diagnostics_mod = @import("diagnostics.zig");
const hover_mod = @import("hover.zig");
const goto_mod = @import("goto.zig");
const doc_symbol = @import("document_symbol.zig");
const lsp_types = @import("types.zig");

const DocumentStore = document_store.DocumentStore;

pub const ServerStatus = enum {
    uninitialized,
    initializing,
    initialized,
    shutdown,
    exit,
};

pub const Server = struct {
    allocator: std.mem.Allocator,
    status: ServerStatus,
    store: DocumentStore,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .status = .uninitialized,
            .store = DocumentStore.init(allocator),
        };
    }

    pub fn deinit(self: *Server) void {
        self.store.deinit();
    }

    /// Handle a raw JSON-RPC message. Returns zero or more response messages.
    /// Each message is a complete JSON string ready to be framed with Content-Length.
    pub fn handleMessage(self: *Server, arena: std.mem.Allocator, json_bytes: []const u8) !?[]const u8 {
        // Parse the JSON-RPC message
        const parsed = std.json.parseFromSlice(std.json.Value, arena, json_bytes, .{}) catch {
            return try self.errorResponse(arena, null, -32700, "Parse error");
        };
        const root = parsed.value;

        const method_val = root.object.get("method") orelse {
            // Response to a request we sent — ignore
            return null;
        };
        const method = method_val.string;

        // Extract id (null for notifications)
        const id = root.object.get("id");
        const params = root.object.get("params");

        // Dispatch
        if (std.mem.eql(u8, method, "initialize")) {
            return try self.handleInitialize(arena, id);
        } else if (std.mem.eql(u8, method, "initialized")) {
            self.status = .initialized;
            return null;
        } else if (std.mem.eql(u8, method, "shutdown")) {
            self.status = .shutdown;
            return try self.nullResponse(arena, id);
        } else if (std.mem.eql(u8, method, "exit")) {
            self.status = .exit;
            return null;
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            return try self.handleDidOpen(arena, params);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            return try self.handleDidChange(arena, params);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            return try self.handleDidClose(arena, params);
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            return try self.handleHover(arena, id, params);
        } else if (std.mem.eql(u8, method, "textDocument/definition")) {
            return try self.handleDefinition(arena, id, params);
        } else if (std.mem.eql(u8, method, "textDocument/documentSymbol")) {
            return try self.handleDocumentSymbol(arena, id, params);
        } else {
            // Unknown method — if it has an id, send MethodNotFound
            if (id != null) {
                return try self.errorResponse(arena, id, -32601, "Method not found");
            }
            return null;
        }
    }

    // ========================================================================
    // Handlers
    // ========================================================================

    fn handleInitialize(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        self.status = .initializing;
        const result =
            \\{"capabilities":{"textDocumentSync":{"openClose":true,"change":1},"hoverProvider":true,"definitionProvider":true,"documentSymbolProvider":true},"serverInfo":{"name":"cot-lsp","version":"0.1.0"}}
        ;
        return try self.successResponse(arena, id, result);
    }

    fn handleDidOpen(self: *Server, arena: std.mem.Allocator, params: ?std.json.Value) !?[]const u8 {
        const p = params orelse return null;
        const td = p.object.get("textDocument") orelse return null;
        const uri = (td.object.get("uri") orelse return null).string;
        const version = (td.object.get("version") orelse return null).integer;
        const text = (td.object.get("text") orelse return null).string;

        self.store.open(uri, version, text) catch return null;

        return try self.publishDiagnostics(arena, uri);
    }

    fn handleDidChange(self: *Server, arena: std.mem.Allocator, params: ?std.json.Value) !?[]const u8 {
        const p = params orelse return null;
        const td = p.object.get("textDocument") orelse return null;
        const uri = (td.object.get("uri") orelse return null).string;
        const version = (td.object.get("version") orelse return null).integer;
        const changes = (p.object.get("contentChanges") orelse return null).array;

        if (changes.items.len == 0) return null;

        // Full text sync: take the last change's text
        const last_change = changes.items[changes.items.len - 1];
        const new_text = (last_change.object.get("text") orelse return null).string;

        self.store.change(uri, version, new_text) catch return null;

        return try self.publishDiagnostics(arena, uri);
    }

    fn handleDidClose(self: *Server, arena: std.mem.Allocator, params: ?std.json.Value) !?[]const u8 {
        const p = params orelse return null;
        const td = p.object.get("textDocument") orelse return null;
        const uri = (td.object.get("uri") orelse return null).string;

        self.store.close(uri);

        // Publish empty diagnostics to clear them
        const uri_escaped = lsp_types.jsonEscape(arena, uri) catch return null;
        return try std.fmt.allocPrint(arena,
            \\{{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{{"uri":"{s}","diagnostics":[]}}}}
        , .{uri_escaped});
    }

    fn handleHover(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        const p = params orelse return try self.nullResponse(arena, id);
        const td = p.object.get("textDocument") orelse return try self.nullResponse(arena, id);
        const uri = (td.object.get("uri") orelse return try self.nullResponse(arena, id)).string;
        const pos = p.object.get("position") orelse return try self.nullResponse(arena, id);
        const line: u32 = @intCast(pos.object.get("line").?.integer);
        const character: u32 = @intCast(pos.object.get("character").?.integer);

        const handle = self.store.get(uri) orelse return try self.nullResponse(arena, id);
        var result = handle.result orelse return try self.nullResponse(arena, id);

        const byte_offset = lsp_types.lspToByteOffset(result.src.content, .{ .line = line, .character = character });
        const hover_info = hover_mod.getHover(arena, &result, byte_offset) orelse return try self.nullResponse(arena, id);

        const value_escaped = lsp_types.jsonEscape(arena, hover_info.value) catch return try self.nullResponse(arena, id);
        const hover_result = try std.fmt.allocPrint(arena,
            \\{{"contents":{{"kind":"{s}","value":"{s}"}}}}
        , .{ hover_info.kind, value_escaped });
        return try self.successResponse(arena, id, hover_result);
    }

    fn handleDefinition(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        const p = params orelse return try self.nullResponse(arena, id);
        const td = p.object.get("textDocument") orelse return try self.nullResponse(arena, id);
        const uri = (td.object.get("uri") orelse return try self.nullResponse(arena, id)).string;
        const pos = p.object.get("position") orelse return try self.nullResponse(arena, id);
        const line: u32 = @intCast(pos.object.get("line").?.integer);
        const character: u32 = @intCast(pos.object.get("character").?.integer);

        const handle = self.store.get(uri) orelse return try self.nullResponse(arena, id);
        var result = handle.result orelse return try self.nullResponse(arena, id);

        const byte_offset = lsp_types.lspToByteOffset(result.src.content, .{ .line = line, .character = character });
        const loc = goto_mod.getDefinition(&result, byte_offset, uri) orelse return try self.nullResponse(arena, id);

        const uri_escaped = lsp_types.jsonEscape(arena, loc.uri) catch return try self.nullResponse(arena, id);
        const def_result = try std.fmt.allocPrint(arena,
            \\{{"uri":"{s}","range":{{"start":{{"line":{d},"character":{d}}},"end":{{"line":{d},"character":{d}}}}}}}
        , .{
            uri_escaped,
            loc.range.start.line,
            loc.range.start.character,
            loc.range.end.line,
            loc.range.end.character,
        });
        return try self.successResponse(arena, id, def_result);
    }

    fn handleDocumentSymbol(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        const p = params orelse return try self.nullResponse(arena, id);
        const td = p.object.get("textDocument") orelse return try self.nullResponse(arena, id);
        const uri = (td.object.get("uri") orelse return try self.nullResponse(arena, id)).string;

        const handle = self.store.get(uri) orelse return try self.successResponse(arena, id, "[]");
        var result = handle.result orelse return try self.successResponse(arena, id, "[]");

        const symbols = doc_symbol.getDocumentSymbols(arena, &result) catch return try self.successResponse(arena, id, "[]");

        if (symbols.len == 0) return try self.successResponse(arena, id, "[]");

        // Serialize symbols to JSON
        var json = std.ArrayListUnmanaged(u8){};
        try json.append(arena, '[');
        for (symbols, 0..) |sym, i| {
            if (i > 0) try json.append(arena, ',');
            try serializeSymbol(arena, &json, sym);
        }
        try json.append(arena, ']');

        return try self.successResponse(arena, id, json.items);
    }

    // ========================================================================
    // Response builders
    // ========================================================================

    fn successResponse(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value, result: []const u8) ![]const u8 {
        _ = self;
        const id_str = try formatId(arena, id);
        return try std.fmt.allocPrint(arena,
            \\{{"jsonrpc":"2.0","id":{s},"result":{s}}}
        , .{ id_str, result });
    }

    fn nullResponse(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        const id_str = try formatId(arena, id);
        return try std.fmt.allocPrint(arena,
            \\{{"jsonrpc":"2.0","id":{s},"result":null}}
        , .{id_str});
    }

    fn errorResponse(self: *Server, arena: std.mem.Allocator, id: ?std.json.Value, code: i32, message: []const u8) ![]const u8 {
        _ = self;
        const id_str = try formatId(arena, id);
        const msg_escaped = lsp_types.jsonEscape(arena, message) catch message;
        return try std.fmt.allocPrint(arena,
            \\{{"jsonrpc":"2.0","id":{s},"error":{{"code":{d},"message":"{s}"}}}}
        , .{ id_str, code, msg_escaped });
    }

    fn publishDiagnostics(self: *Server, arena: std.mem.Allocator, uri: []const u8) !?[]const u8 {
        const handle = self.store.get(uri) orelse return null;
        const result = handle.result orelse return null;

        const uri_escaped = lsp_types.jsonEscape(arena, uri) catch return null;

        if (result.errors.len == 0) {
            return try std.fmt.allocPrint(arena,
                \\{{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{{"uri":"{s}","diagnostics":[]}}}}
            , .{uri_escaped});
        }

        const diags = diagnostics_mod.convertErrors(arena, result.src.content, result.errors) catch return null;

        var json = std.ArrayListUnmanaged(u8){};
        try json.appendSlice(arena, "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"");
        try json.appendSlice(arena, uri_escaped);
        try json.appendSlice(arena, "\",\"diagnostics\":[");

        for (diags, 0..) |diag, i| {
            if (i > 0) try json.append(arena, ',');
            const msg_escaped = lsp_types.jsonEscape(arena, diag.message) catch diag.message;
            const code_part: []const u8 = if (diag.code) |c|
                (std.fmt.allocPrint(arena, ",\"code\":\"{s}\"", .{c}) catch "")
            else
                "";
            const diag_json = try std.fmt.allocPrint(arena,
                \\{{"range":{{"start":{{"line":{d},"character":{d}}},"end":{{"line":{d},"character":{d}}}}},"severity":{d}{s},"message":"{s}"}}
            , .{
                diag.range.start.line,
                diag.range.start.character,
                diag.range.end.line,
                diag.range.end.character,
                @intFromEnum(diag.severity),
                code_part,
                msg_escaped,
            });
            try json.appendSlice(arena, diag_json);
        }

        try json.appendSlice(arena, "]}}");

        return json.items;
    }
};

// ============================================================================
// Helpers
// ============================================================================

fn formatId(arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
    if (id) |v| {
        switch (v) {
            .integer => |n| return try std.fmt.allocPrint(arena, "{d}", .{n}),
            .string => |s| {
                const escaped = lsp_types.jsonEscape(arena, s) catch s;
                return try std.fmt.allocPrint(arena, "\"{s}\"", .{escaped});
            },
            else => return "null",
        }
    }
    return "null";
}

fn serializeSymbol(arena: std.mem.Allocator, json: *std.ArrayListUnmanaged(u8), sym: lsp_types.DocumentSymbol) !void {
    const name_escaped = lsp_types.jsonEscape(arena, sym.name) catch sym.name;
    const header = try std.fmt.allocPrint(arena,
        \\{{"name":"{s}","kind":{d},"range":{{"start":{{"line":{d},"character":{d}}},"end":{{"line":{d},"character":{d}}}}},"selectionRange":{{"start":{{"line":{d},"character":{d}}},"end":{{"line":{d},"character":{d}}}}}
    , .{
        name_escaped,
        sym.kind.value(),
        sym.range.start.line,
        sym.range.start.character,
        sym.range.end.line,
        sym.range.end.character,
        sym.selection_range.start.line,
        sym.selection_range.start.character,
        sym.selection_range.end.line,
        sym.selection_range.end.character,
    });
    try json.appendSlice(arena, header);

    if (sym.children.len > 0) {
        try json.appendSlice(arena, ",\"children\":[");
        for (sym.children, 0..) |child, i| {
            if (i > 0) try json.append(arena, ',');
            try serializeSymbol(arena, json, child);
        }
        try json.append(arena, ']');
    }

    try json.append(arena, '}');
}
