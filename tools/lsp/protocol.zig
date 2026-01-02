//! LSP Protocol Types and JSON Helpers
//!
//! Provides types and utilities for the Language Server Protocol.

const std = @import("std");
const json = std.json;
const Allocator = std.mem.Allocator;

// ============================================================
// JSON-RPC Message Types
// ============================================================

pub const JsonValue = json.Value;
pub const JsonObject = json.ObjectMap;
pub const JsonArray = json.Array;

/// Read a line from stdin (up to newline, strips \r\n)
pub fn readLine(buf: []u8) !?[]const u8 {
    const stdin = std.fs.File.stdin();
    var i: usize = 0;

    while (i < buf.len) {
        var byte: [1]u8 = undefined;
        const n = stdin.read(&byte) catch return null;
        if (n == 0) return null;

        if (byte[0] == '\n') {
            const end = if (i > 0 and buf[i - 1] == '\r') i - 1 else i;
            return buf[0..end];
        }
        buf[i] = byte[0];
        i += 1;
    }
    return buf[0..i];
}

/// Read exact number of bytes from stdin
pub fn readExact(buf: []u8) !void {
    const stdin = std.fs.File.stdin();
    var total: usize = 0;
    while (total < buf.len) {
        const n = stdin.read(buf[total..]) catch return error.IoError;
        if (n == 0) return error.EndOfStream;
        total += n;
    }
}

/// Write to stdout
pub fn writeStdout(data: []const u8) !void {
    const stdout = std.fs.File.stdout();
    _ = try stdout.write(data);
}

/// Read a JSON-RPC message from stdin
pub fn readMessage(allocator: Allocator) !?JsonValue {
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
    const parsed = try json.parseFromSlice(JsonValue, allocator, content, .{});
    return parsed.value;
}

/// Write a JSON-RPC message to stdout
pub fn writeMessage(msg: JsonValue, allocator: Allocator) !void {
    const content = try json.Stringify.valueAlloc(allocator, msg, .{});
    defer allocator.free(content);

    var header_buf: [64]u8 = undefined;
    const header = try std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{content.len});

    try writeStdout(header);
    try writeStdout(content);
}

// ============================================================
// Response Builders
// ============================================================

/// Build a JSON-RPC success response
pub fn makeResponse(id: JsonValue, result: JsonValue, allocator: Allocator) !JsonValue {
    var response = JsonObject.init(allocator);
    try response.put("jsonrpc", JsonValue{ .string = "2.0" });
    try response.put("id", id);
    try response.put("result", result);
    return JsonValue{ .object = response };
}

/// Build a JSON-RPC error response
pub fn makeErrorResponse(id: JsonValue, code: i64, message: []const u8, allocator: Allocator) !JsonValue {
    var err_obj = JsonObject.init(allocator);
    try err_obj.put("code", JsonValue{ .integer = code });
    try err_obj.put("message", JsonValue{ .string = message });

    var response = JsonObject.init(allocator);
    try response.put("jsonrpc", JsonValue{ .string = "2.0" });
    try response.put("id", id);
    try response.put("error", JsonValue{ .object = err_obj });
    return JsonValue{ .object = response };
}

/// Build a JSON-RPC notification (no id)
pub fn makeNotification(method: []const u8, params: JsonValue, allocator: Allocator) !JsonValue {
    var notification = JsonObject.init(allocator);
    try notification.put("jsonrpc", JsonValue{ .string = "2.0" });
    try notification.put("method", JsonValue{ .string = method });
    try notification.put("params", params);
    return JsonValue{ .object = notification };
}

// ============================================================
// LSP Error Codes
// ============================================================

pub const ErrorCode = struct {
    pub const ParseError: i64 = -32700;
    pub const InvalidRequest: i64 = -32600;
    pub const MethodNotFound: i64 = -32601;
    pub const InvalidParams: i64 = -32602;
    pub const InternalError: i64 = -32603;
    pub const ServerNotInitialized: i64 = -32002;
    pub const UnknownErrorCode: i64 = -32001;
    pub const RequestCancelled: i64 = -32800;
    pub const ContentModified: i64 = -32801;
};

// ============================================================
// LSP Types
// ============================================================

pub const Position = struct {
    line: u32,
    character: u32,

    pub fn toJson(self: Position, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("line", JsonValue{ .integer = self.line });
        try obj.put("character", JsonValue{ .integer = self.character });
        return JsonValue{ .object = obj };
    }

    pub fn fromJson(value: JsonValue) ?Position {
        const obj = value.object;
        const line = obj.get("line") orelse return null;
        const char = obj.get("character") orelse return null;
        return Position{
            .line = @intCast(line.integer),
            .character = @intCast(char.integer),
        };
    }
};

pub const Range = struct {
    start: Position,
    end: Position,

    pub fn toJson(self: Range, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("start", try self.start.toJson(allocator));
        try obj.put("end", try self.end.toJson(allocator));
        return JsonValue{ .object = obj };
    }
};

pub const Location = struct {
    uri: []const u8,
    range: Range,

    pub fn toJson(self: Location, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("uri", JsonValue{ .string = self.uri });
        try obj.put("range", try self.range.toJson(allocator));
        return JsonValue{ .object = obj };
    }
};

pub const DiagnosticSeverity = enum(u8) {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
};

pub const Diagnostic = struct {
    range: Range,
    severity: DiagnosticSeverity,
    message: []const u8,
    source: []const u8 = "cot",

    pub fn toJson(self: Diagnostic, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("range", try self.range.toJson(allocator));
        try obj.put("severity", JsonValue{ .integer = @intFromEnum(self.severity) });
        try obj.put("message", JsonValue{ .string = self.message });
        try obj.put("source", JsonValue{ .string = self.source });
        return JsonValue{ .object = obj };
    }
};

pub const SymbolKind = enum(u8) {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26,
};

pub const DocumentSymbol = struct {
    name: []const u8,
    kind: SymbolKind,
    range: Range,
    selection_range: Range,
    children: ?[]DocumentSymbol = null,

    pub fn toJson(self: DocumentSymbol, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("name", JsonValue{ .string = self.name });
        try obj.put("kind", JsonValue{ .integer = @intFromEnum(self.kind) });
        try obj.put("range", try self.range.toJson(allocator));
        try obj.put("selectionRange", try self.selection_range.toJson(allocator));

        if (self.children) |children| {
            var arr = JsonArray.init(allocator);
            for (children) |child| {
                try arr.append(try child.toJson(allocator));
            }
            try obj.put("children", JsonValue{ .array = arr });
        }

        return JsonValue{ .object = obj };
    }
};

pub const CompletionItemKind = enum(u8) {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: CompletionItemKind,
    detail: ?[]const u8 = null,
    insert_text: ?[]const u8 = null,

    pub fn toJson(self: CompletionItem, allocator: Allocator) !JsonValue {
        var obj = JsonObject.init(allocator);
        try obj.put("label", JsonValue{ .string = self.label });
        try obj.put("kind", JsonValue{ .integer = @intFromEnum(self.kind) });
        if (self.detail) |d| {
            try obj.put("detail", JsonValue{ .string = d });
        }
        if (self.insert_text) |t| {
            try obj.put("insertText", JsonValue{ .string = t });
        }
        return JsonValue{ .object = obj };
    }
};

// ============================================================
// Semantic Token Types
// ============================================================

/// Semantic token types - order matters, indices used in encoding
/// These match the LSP specification and provide rich syntax highlighting
pub const SemanticTokenTypes = [_][]const u8{
    "namespace",  // 0 - namespace declarations
    "type",       // 1 - type references (,customer)
    "class",      // 2 - class/structure declarations
    "enum",       // 3
    "interface",  // 4
    "struct",     // 5 - structure keyword
    "parameter",  // 6
    "variable",   // 7 - identifiers
    "property",   // 8 - member access (after .)
    "function",   // 9 - function/subroutine declarations
    "method",     // 10 - method calls
    "keyword",    // 11 - control flow (if, while, for)
    "modifier",   // 12 - access modifiers (public, private)
    "comment",    // 13
    "string",     // 14
    "number",     // 15
    "operator",   // 16
    "macro",      // 17 - builtins like %trim
    "regexp",     // 18 - I/O keywords (read, write, open)
    "decorator",  // 19 - data keywords (clear, init)
    "event",      // 20 - exception keywords (try, catch)
    "label",      // 21 - labels (error handlers, goto targets)
};

/// Semantic token type indices for convenience
pub const SemanticTokenIndex = struct {
    pub const Namespace: usize = 0;
    pub const Type: usize = 1;
    pub const Class: usize = 2;
    pub const Enum: usize = 3;
    pub const Interface: usize = 4;
    pub const Struct: usize = 5;
    pub const Parameter: usize = 6;
    pub const Variable: usize = 7;
    pub const Property: usize = 8;
    pub const Function: usize = 9;
    pub const Method: usize = 10;
    pub const Keyword: usize = 11;
    pub const Modifier: usize = 12;
    pub const Comment: usize = 13;
    pub const String: usize = 14;
    pub const Number: usize = 15;
    pub const Operator: usize = 16;
    pub const Macro: usize = 17;
    pub const IO: usize = 18;       // regexp - used for I/O keywords
    pub const Data: usize = 19;      // decorator - used for data keywords
    pub const Exception: usize = 20; // event - used for exception keywords
    pub const Label: usize = 21;
};

// ============================================================
// Helper Functions
// ============================================================

/// Get a string from a JSON value, returns null if not a string
pub fn getString(value: ?JsonValue) ?[]const u8 {
    const v = value orelse return null;
    return switch (v) {
        .string => |s| s,
        else => null,
    };
}

/// Get an integer from a JSON value, returns null if not an integer
pub fn getInteger(value: ?JsonValue) ?i64 {
    const v = value orelse return null;
    return switch (v) {
        .integer => |i| i,
        else => null,
    };
}

/// Get an object from a JSON value, returns null if not an object
pub fn getObject(value: ?JsonValue) ?JsonObject {
    const v = value orelse return null;
    return switch (v) {
        .object => |o| o,
        else => null,
    };
}
