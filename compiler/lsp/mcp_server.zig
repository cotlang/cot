//! MCP server — JSON-RPC 2.0 dispatch and tool handlers.
//!
//! Provides compiler-powered tools for AI assistants (Claude Code, etc.).
//! Reuses LSP analysis infrastructure for dynamic tools (check_file, list_symbols, etc.).
//! Static tools (syntax reference, stdlib docs) ported from mcp/cot-mcp.cot.
//!
//! Reference: MCP specification (2024-11-05), ZLS feature patterns.

const std = @import("std");
const analysis = @import("analysis.zig");
const doc_symbol = @import("document_symbol.zig");
const diagnostics_mod = @import("diagnostics.zig");
const hover_mod = @import("hover.zig");
const goto_mod = @import("goto.zig");
const references_mod = @import("references.zig");
const lsp_types = @import("types.zig");

const AnalysisResult = analysis.AnalysisResult;

pub const McpServer = struct {
    allocator: std.mem.Allocator,
    should_exit: bool = false,

    pub fn init(allocator: std.mem.Allocator) McpServer {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *McpServer) void {
        _ = self;
    }

    /// Handle a raw JSON-RPC message. Returns a response string or null for notifications.
    pub fn handleMessage(self: *McpServer, arena: std.mem.Allocator, json_bytes: []const u8) !?[]const u8 {
        const parsed = std.json.parseFromSlice(std.json.Value, arena, json_bytes, .{}) catch {
            return try jsonRpcError(arena, null, -32700, "Parse error");
        };
        const root = parsed.value;

        const method_val = root.object.get("method") orelse {
            // Response or malformed — ignore
            return null;
        };
        const method = method_val.string;
        const id = root.object.get("id");
        const params = root.object.get("params");

        // Dispatch
        if (std.mem.eql(u8, method, "initialize")) {
            return try self.handleInitialize(arena, id);
        } else if (std.mem.eql(u8, method, "notifications/initialized") or std.mem.eql(u8, method, "initialized")) {
            return null;
        } else if (std.mem.eql(u8, method, "tools/list")) {
            return try self.handleToolsList(arena, id);
        } else if (std.mem.eql(u8, method, "tools/call")) {
            return try self.handleToolsCall(arena, id, params);
        } else if (std.mem.eql(u8, method, "resources/list")) {
            return try self.handleResourcesList(arena, id);
        } else if (std.mem.eql(u8, method, "resources/read")) {
            return try self.handleResourcesRead(arena, id, params);
        } else if (std.mem.eql(u8, method, "prompts/list")) {
            return try self.handlePromptsList(arena, id);
        } else if (std.mem.eql(u8, method, "prompts/get")) {
            return try self.handlePromptsGet(arena, id, params);
        } else {
            if (id != null) {
                return try jsonRpcError(arena, id, -32601, "Method not found");
            }
            return null;
        }
    }

    // ========================================================================
    // MCP Protocol Handlers
    // ========================================================================

    fn handleInitialize(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        const result =
            \\{"protocolVersion":"2024-11-05","capabilities":{"tools":{"listChanged":false},"resources":{"listChanged":false},"prompts":{"listChanged":false}},"serverInfo":{"name":"cot-tools","version":"0.3.2"}}
        ;
        return try jsonRpcSuccess(arena, id, result);
    }

    fn handleToolsList(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        const result =
            \\{"tools":[
            \\{"name":"get_syntax_reference","description":"Get Cot language syntax cheat sheet","inputSchema":{"type":"object"},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false}},
            \\{"name":"get_stdlib_docs","description":"Get Cot stdlib function signatures","inputSchema":{"type":"object","properties":{"module":{"type":"string","description":"Module: json string fs io list map os time random sort set math"}}},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false}},
            \\{"name":"get_project_info","description":"Get Cot build/test commands and project structure","inputSchema":{"type":"object"},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false}},
            \\{"name":"check_file","description":"Parse and type-check a Cot file, return diagnostics","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"}},"required":["path"]},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":true}},
            \\{"name":"list_symbols","description":"List all declarations in a Cot file","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"}},"required":["path"]},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":true}},
            \\{"name":"get_type","description":"Get type information at a position in a Cot file","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"},"line":{"type":"integer","description":"Line number (0-based)"},"character":{"type":"integer","description":"Column number (0-based)"}},"required":["path","line","character"]},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":true}},
            \\{"name":"find_definition","description":"Find where a symbol is defined","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"},"line":{"type":"integer","description":"Line number (0-based)"},"character":{"type":"integer","description":"Column number (0-based)"}},"required":["path","line","character"]},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":true}},
            \\{"name":"find_references","description":"Find all references to a symbol","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"},"line":{"type":"integer","description":"Line number (0-based)"},"character":{"type":"integer","description":"Column number (0-based)"}},"required":["path","line","character"]},"annotations":{"readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":true}},
            \\{"name":"build","description":"Compile a Cot file, return success or errors","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"},"target":{"type":"string","description":"Target: native, wasm32, wasm32-wasi (default: native)"}},"required":["path"]},"annotations":{"readOnlyHint":false,"destructiveHint":false,"idempotentHint":false,"openWorldHint":true}},
            \\{"name":"run_tests","description":"Run tests in a Cot file, return results","inputSchema":{"type":"object","properties":{"path":{"type":"string","description":"Path to .cot file"},"filter":{"type":"string","description":"Only run tests matching this string"}},"required":["path"]},"annotations":{"readOnlyHint":false,"destructiveHint":false,"idempotentHint":false,"openWorldHint":true}}
            \\]}
        ;
        return try jsonRpcSuccess(arena, id, result);
    }

    fn handleToolsCall(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        const p = params orelse return try jsonRpcError(arena, id, -32602, "Missing params");
        const name = (p.object.get("name") orelse return try jsonRpcError(arena, id, -32602, "Missing tool name")).string;
        const args = p.object.get("arguments");

        if (std.mem.eql(u8, name, "get_syntax_reference")) {
            return try self.toolSyntaxReference(arena, id);
        } else if (std.mem.eql(u8, name, "get_stdlib_docs")) {
            const module = if (args) |a| if (a.object.get("module")) |m| m.string else "" else "";
            return try self.toolStdlibDocs(arena, id, module);
        } else if (std.mem.eql(u8, name, "get_project_info")) {
            return try self.toolProjectInfo(arena, id);
        } else if (std.mem.eql(u8, name, "check_file")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            return try self.toolCheckFile(arena, id, path);
        } else if (std.mem.eql(u8, name, "list_symbols")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            return try self.toolListSymbols(arena, id, path);
        } else if (std.mem.eql(u8, name, "get_type")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            const line = if (args) |a| if (a.object.get("line")) |v| jsonToU32(v) else null else null;
            const character = if (args) |a| if (a.object.get("character")) |v| jsonToU32(v) else null else null;
            return try self.toolGetType(arena, id, path, line, character);
        } else if (std.mem.eql(u8, name, "find_definition")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            const line = if (args) |a| if (a.object.get("line")) |v| jsonToU32(v) else null else null;
            const character = if (args) |a| if (a.object.get("character")) |v| jsonToU32(v) else null else null;
            return try self.toolFindDefinition(arena, id, path, line, character);
        } else if (std.mem.eql(u8, name, "find_references")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            const line = if (args) |a| if (a.object.get("line")) |v| jsonToU32(v) else null else null;
            const character = if (args) |a| if (a.object.get("character")) |v| jsonToU32(v) else null else null;
            return try self.toolFindReferences(arena, id, path, line, character);
        } else if (std.mem.eql(u8, name, "build")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            const target = if (args) |a| if (a.object.get("target")) |v| v.string else null else null;
            return try self.toolBuild(arena, id, path, target);
        } else if (std.mem.eql(u8, name, "run_tests")) {
            const path = if (args) |a| if (a.object.get("path")) |v| v.string else null else null;
            const filter = if (args) |a| if (a.object.get("filter")) |v| v.string else null else null;
            return try self.toolRunTests(arena, id, path, filter);
        } else {
            return try toolError(arena, id, "Unknown tool");
        }
    }

    // ========================================================================
    // Dynamic Tools (compiler-powered)
    // ========================================================================

    fn toolCheckFile(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");

        const text = std.fs.cwd().readFileAlloc(arena, file_path, 1024 * 1024) catch {
            return try toolError(arena, id, try std.fmt.allocPrint(arena, "Cannot read file: {s}", .{file_path}));
        };

        var result = analysis.analyze(arena, text, file_path) orelse {
            return try toolError(arena, id, "Analysis failed");
        };
        defer result.deinit();

        var out = std.ArrayListUnmanaged(u8){};

        if (result.errors.len == 0) {
            try out.appendSlice(arena, "No errors found in ");
            try out.appendSlice(arena, file_path);
        } else {
            for (result.errors) |err| {
                if (out.items.len > 0) try out.append(arena, '\n');
                const range = lsp_types.spanToRange(result.src.content, err.span);
                const line = try std.fmt.allocPrint(arena, "{s}:{d}:{d}: error", .{
                    file_path,
                    range.start.line + 1,
                    range.start.character + 1,
                });
                try out.appendSlice(arena, line);
                if (err.err_code) |code| {
                    const code_str = try std.fmt.allocPrint(arena, " E{d}", .{code.code()});
                    try out.appendSlice(arena, code_str);
                }
                try out.appendSlice(arena, ": ");
                try out.appendSlice(arena, err.msg);
            }
        }

        return try toolResult(arena, id, out.items);
    }

    fn toolListSymbols(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");

        const text = std.fs.cwd().readFileAlloc(arena, file_path, 1024 * 1024) catch {
            return try toolError(arena, id, try std.fmt.allocPrint(arena, "Cannot read file: {s}", .{file_path}));
        };

        var result = analysis.analyze(arena, text, file_path) orelse {
            return try toolError(arena, id, "Analysis failed");
        };
        defer result.deinit();

        const symbols = doc_symbol.getDocumentSymbols(arena, &result) catch {
            return try toolError(arena, id, "Failed to extract symbols");
        };

        var out = std.ArrayListUnmanaged(u8){};
        for (symbols) |sym| {
            if (out.items.len > 0) try out.append(arena, '\n');
            const kind_str = symbolKindName(sym.kind);
            const line = try std.fmt.allocPrint(arena, "{s} {s} (line {d})", .{
                kind_str,
                sym.name,
                sym.range.start.line + 1,
            });
            try out.appendSlice(arena, line);

            // Show children (fields, methods)
            for (sym.children) |child| {
                try out.append(arena, '\n');
                const child_kind = symbolKindName(child.kind);
                const child_line = try std.fmt.allocPrint(arena, "  {s} {s} (line {d})", .{
                    child_kind,
                    child.name,
                    child.range.start.line + 1,
                });
                try out.appendSlice(arena, child_line);
            }
        }

        if (out.items.len == 0) {
            try out.appendSlice(arena, "No symbols found in ");
            try out.appendSlice(arena, file_path);
        }

        return try toolResult(arena, id, out.items);
    }

    fn toolGetType(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8, line: ?u32, character: ?u32) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");
        const l = line orelse return try toolError(arena, id, "Missing required argument: line");
        const c = character orelse return try toolError(arena, id, "Missing required argument: character");

        const text = std.fs.cwd().readFileAlloc(arena, file_path, 1024 * 1024) catch {
            return try toolError(arena, id, try std.fmt.allocPrint(arena, "Cannot read file: {s}", .{file_path}));
        };

        var result = analysis.analyze(arena, text, file_path) orelse {
            return try toolError(arena, id, "Analysis failed");
        };
        defer result.deinit();

        const byte_offset = lsp_types.lspToByteOffset(result.src.content, .{ .line = l, .character = c });
        const hover = hover_mod.getHover(arena, &result, byte_offset) orelse {
            return try toolResult(arena, id, "No type information at this position");
        };

        return try toolResult(arena, id, hover.value);
    }

    fn toolFindDefinition(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8, line: ?u32, character: ?u32) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");
        const l = line orelse return try toolError(arena, id, "Missing required argument: line");
        const c = character orelse return try toolError(arena, id, "Missing required argument: character");

        const text = std.fs.cwd().readFileAlloc(arena, file_path, 1024 * 1024) catch {
            return try toolError(arena, id, try std.fmt.allocPrint(arena, "Cannot read file: {s}", .{file_path}));
        };

        var result = analysis.analyze(arena, text, file_path) orelse {
            return try toolError(arena, id, "Analysis failed");
        };
        defer result.deinit();

        const byte_offset = lsp_types.lspToByteOffset(result.src.content, .{ .line = l, .character = c });
        const location = goto_mod.getDefinition(&result, byte_offset, file_path) orelse {
            return try toolResult(arena, id, "No definition found at this position");
        };

        const out = try std.fmt.allocPrint(arena, "{s}:{d}:{d}", .{
            location.uri,
            location.range.start.line + 1,
            location.range.start.character + 1,
        });
        return try toolResult(arena, id, out);
    }

    fn toolFindReferences(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8, line: ?u32, character: ?u32) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");
        const l = line orelse return try toolError(arena, id, "Missing required argument: line");
        const c = character orelse return try toolError(arena, id, "Missing required argument: character");

        const text = std.fs.cwd().readFileAlloc(arena, file_path, 1024 * 1024) catch {
            return try toolError(arena, id, try std.fmt.allocPrint(arena, "Cannot read file: {s}", .{file_path}));
        };

        var result = analysis.analyze(arena, text, file_path) orelse {
            return try toolError(arena, id, "Analysis failed");
        };
        defer result.deinit();

        const byte_offset = lsp_types.lspToByteOffset(result.src.content, .{ .line = l, .character = c });
        const locations = references_mod.getReferences(arena, &result, byte_offset, file_path, true) catch {
            return try toolError(arena, id, "Failed to find references");
        };

        if (locations.len == 0) {
            return try toolResult(arena, id, "No references found at this position");
        }

        var out = std.ArrayListUnmanaged(u8){};
        for (locations) |loc| {
            if (out.items.len > 0) try out.append(arena, '\n');
            const line_str = try std.fmt.allocPrint(arena, "{s}:{d}:{d}", .{
                loc.uri,
                loc.range.start.line + 1,
                loc.range.start.character + 1,
            });
            try out.appendSlice(arena, line_str);
        }
        return try toolResult(arena, id, out.items);
    }

    fn toolBuild(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8, target: ?[]const u8) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");

        // Build the command
        var argv = std.ArrayListUnmanaged([]const u8){};
        try argv.append(arena, "cot");
        try argv.append(arena, "build");
        try argv.append(arena, file_path);
        if (target) |t| {
            const flag = try std.fmt.allocPrint(arena, "--target={s}", .{t});
            try argv.append(arena, flag);
        }

        return try runSubprocess(arena, id, argv.items);
    }

    fn toolRunTests(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, path: ?[]const u8, filter: ?[]const u8) ![]const u8 {
        _ = self;
        const file_path = path orelse return try toolError(arena, id, "Missing required argument: path");

        var argv = std.ArrayListUnmanaged([]const u8){};
        try argv.append(arena, "cot");
        try argv.append(arena, "test");
        try argv.append(arena, file_path);
        if (filter) |f| {
            const flag = try std.fmt.allocPrint(arena, "--filter={s}", .{f});
            try argv.append(arena, flag);
        }

        return try runSubprocess(arena, id, argv.items);
    }

    // ========================================================================
    // Static Tools (ported from mcp/cot-mcp.cot)
    // ========================================================================

    fn toolSyntaxReference(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        return try toolResult(arena, id, syntax_reference_text);
    }

    fn toolStdlibDocs(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, module: []const u8) ![]const u8 {
        _ = self;
        const text = getStdlibDocs(module);
        return try toolResult(arena, id, text);
    }

    fn toolProjectInfo(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        return try toolResult(arena, id, project_info_text);
    }

    // ========================================================================
    // Resources Handlers
    // ========================================================================

    fn handleResourcesList(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        const result =
            \\{"resources":[
            \\{"uri":"cot://syntax-reference","name":"Cot Syntax Reference","mimeType":"text/plain"},
            \\{"uri":"cot://project-info","name":"Cot Project Info","mimeType":"text/plain"}
            \\],"resourceTemplates":[
            \\{"uriTemplate":"cot://stdlib/{module}","name":"Stdlib Docs","mimeType":"text/plain"}
            \\]}
        ;
        return try jsonRpcSuccess(arena, id, result);
    }

    fn handleResourcesRead(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        _ = self;
        const p = params orelse return try jsonRpcError(arena, id, -32602, "Missing params");
        const uri = (p.object.get("uri") orelse return try jsonRpcError(arena, id, -32602, "Missing uri")).string;

        if (std.mem.eql(u8, uri, "cot://syntax-reference")) {
            return try resourceResult(arena, id, uri, syntax_reference_text);
        } else if (std.mem.eql(u8, uri, "cot://project-info")) {
            return try resourceResult(arena, id, uri, project_info_text);
        } else if (std.mem.startsWith(u8, uri, "cot://stdlib/")) {
            const module = uri["cot://stdlib/".len..];
            const text = getStdlibDocs(module);
            return try resourceResult(arena, id, uri, text);
        } else {
            return try jsonRpcError(arena, id, -32602, "Unknown resource URI");
        }
    }

    // ========================================================================
    // Prompts Handlers
    // ========================================================================

    fn handlePromptsList(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value) ![]const u8 {
        _ = self;
        const result =
            \\{"prompts":[
            \\{"name":"debug-error","description":"Help debug a Cot compilation error","arguments":[{"name":"error_message","description":"The error message to debug","required":true},{"name":"file_path","description":"Path to the file with the error","required":false}]},
            \\{"name":"add-test","description":"Generate a test for a Cot function","arguments":[{"name":"function_name","description":"Name of the function to test","required":true},{"name":"file_path","description":"Path to the file containing the function","required":false}]},
            \\{"name":"explain-code","description":"Explain what a Cot code snippet does","arguments":[{"name":"code","description":"The Cot code to explain","required":true}]}
            \\]}
        ;
        return try jsonRpcSuccess(arena, id, result);
    }

    fn handlePromptsGet(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, params: ?std.json.Value) ![]const u8 {
        const p = params orelse return try jsonRpcError(arena, id, -32602, "Missing params");
        const name = (p.object.get("name") orelse return try jsonRpcError(arena, id, -32602, "Missing prompt name")).string;
        const prompt_args = p.object.get("arguments");

        if (std.mem.eql(u8, name, "debug-error")) {
            return try self.promptDebugError(arena, id, prompt_args);
        } else if (std.mem.eql(u8, name, "add-test")) {
            return try self.promptAddTest(arena, id, prompt_args);
        } else if (std.mem.eql(u8, name, "explain-code")) {
            return try self.promptExplainCode(arena, id, prompt_args);
        } else {
            return try jsonRpcError(arena, id, -32602, "Unknown prompt name");
        }
    }

    fn promptDebugError(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, args: ?std.json.Value) ![]const u8 {
        _ = self;
        const error_msg = if (args) |a| if (a.object.get("error_message")) |v| v.string else "" else "";
        const file_path = if (args) |a| if (a.object.get("file_path")) |v| v.string else null else null;

        var out = std.ArrayListUnmanaged(u8){};
        try out.appendSlice(arena, "I'm getting a Cot compilation error and need help debugging it.\n\n");
        try out.appendSlice(arena, "Error message:\n```\n");
        try out.appendSlice(arena, error_msg);
        try out.appendSlice(arena, "\n```\n\n");
        if (file_path) |fp| {
            try out.appendSlice(arena, "File: ");
            try out.appendSlice(arena, fp);
            try out.appendSlice(arena, "\n\n");
        }
        try out.appendSlice(arena, "Here is the Cot syntax reference for context:\n\n");
        try out.appendSlice(arena, syntax_reference_text);

        return try promptResult(arena, id, out.items);
    }

    fn promptAddTest(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, args: ?std.json.Value) ![]const u8 {
        _ = self;
        const func_name = if (args) |a| if (a.object.get("function_name")) |v| v.string else "" else "";
        const file_path = if (args) |a| if (a.object.get("file_path")) |v| v.string else null else null;

        var out = std.ArrayListUnmanaged(u8){};
        try out.appendSlice(arena, "Generate a test for the Cot function `");
        try out.appendSlice(arena, func_name);
        try out.appendSlice(arena, "`.\n\n");
        if (file_path) |fp| {
            try out.appendSlice(arena, "The function is in file: ");
            try out.appendSlice(arena, fp);
            try out.appendSlice(arena, "\n\n");
        }
        try out.appendSlice(arena, "Cot tests use this format:\n```cot\ntest \"description\" {\n    @assertEq(actual, expected)\n}\n```\n\n");
        try out.appendSlice(arena, "Here is the Cot syntax reference:\n\n");
        try out.appendSlice(arena, syntax_reference_text);

        return try promptResult(arena, id, out.items);
    }

    fn promptExplainCode(self: *McpServer, arena: std.mem.Allocator, id: ?std.json.Value, args: ?std.json.Value) ![]const u8 {
        _ = self;
        const code = if (args) |a| if (a.object.get("code")) |v| v.string else "" else "";

        var out = std.ArrayListUnmanaged(u8){};
        try out.appendSlice(arena, "Explain what the following Cot code does:\n\n```cot\n");
        try out.appendSlice(arena, code);
        try out.appendSlice(arena, "\n```\n\n");
        try out.appendSlice(arena, "For reference, here is the Cot syntax:\n\n");
        try out.appendSlice(arena, syntax_reference_text);

        return try promptResult(arena, id, out.items);
    }

    // ========================================================================
    // Response Builders (MCP JSON-RPC)
    // ========================================================================

    /// Build a tool result response with text content.
    fn toolResult(arena: std.mem.Allocator, id: ?std.json.Value, text: []const u8) ![]const u8 {
        const escaped = lsp_types.jsonEscape(arena, text) catch text;
        const result = try std.fmt.allocPrint(arena,
            \\{{"content":[{{"type":"text","text":"{s}"}}]}}
        , .{escaped});
        return try jsonRpcSuccess(arena, id, result);
    }

    /// Build a resource read response with text content.
    fn resourceResult(arena: std.mem.Allocator, id: ?std.json.Value, uri: []const u8, text: []const u8) ![]const u8 {
        const escaped_uri = lsp_types.jsonEscape(arena, uri) catch uri;
        const escaped_text = lsp_types.jsonEscape(arena, text) catch text;
        const result = try std.fmt.allocPrint(arena,
            \\{{"contents":[{{"uri":"{s}","mimeType":"text/plain","text":"{s}"}}]}}
        , .{ escaped_uri, escaped_text });
        return try jsonRpcSuccess(arena, id, result);
    }

    /// Build a prompt result with a single user message.
    fn promptResult(arena: std.mem.Allocator, id: ?std.json.Value, text: []const u8) ![]const u8 {
        const escaped = lsp_types.jsonEscape(arena, text) catch text;
        const result = try std.fmt.allocPrint(arena,
            \\{{"messages":[{{"role":"user","content":{{"type":"text","text":"{s}"}}}}]}}
        , .{escaped});
        return try jsonRpcSuccess(arena, id, result);
    }

    /// Build a tool error response (application-level, not protocol error).
    /// Uses isError flag per MCP spec for tool failures.
    fn toolError(arena: std.mem.Allocator, id: ?std.json.Value, msg: []const u8) ![]const u8 {
        const escaped = lsp_types.jsonEscape(arena, msg) catch msg;
        const result = try std.fmt.allocPrint(arena,
            \\{{"content":[{{"type":"text","text":"{s}"}}],"isError":true}}
        , .{escaped});
        return try jsonRpcSuccess(arena, id, result);
    }
};

// ============================================================================
// JSON-RPC helpers
// ============================================================================

fn jsonRpcSuccess(arena: std.mem.Allocator, id: ?std.json.Value, result: []const u8) ![]const u8 {
    const id_str = try formatId(arena, id);
    return try std.fmt.allocPrint(arena,
        \\{{"jsonrpc":"2.0","id":{s},"result":{s}}}
    , .{ id_str, result });
}

fn jsonRpcError(arena: std.mem.Allocator, id: ?std.json.Value, code: i32, message: []const u8) ![]const u8 {
    const id_str = try formatId(arena, id);
    const msg_escaped = lsp_types.jsonEscape(arena, message) catch message;
    return try std.fmt.allocPrint(arena,
        \\{{"jsonrpc":"2.0","id":{s},"error":{{"code":{d},"message":"{s}"}}}}
    , .{ id_str, code, msg_escaped });
}

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

fn jsonToU32(val: std.json.Value) ?u32 {
    switch (val) {
        .integer => |n| return if (n >= 0) @intCast(@as(u64, @intCast(n))) else null,
        .float => |f| return if (f >= 0) @intFromFloat(f) else null,
        else => return null,
    }
}

fn symbolKindName(kind: lsp_types.SymbolKind) []const u8 {
    return switch (kind) {
        .function => "fn",
        .method => "method",
        .variable => "var",
        .constant => "const",
        .@"struct" => "struct",
        .@"enum" => "enum",
        .interface => "trait",
        .class => "impl",
        .field => "field",
        .module => "import",
        .type_parameter => "type",
        else => "symbol",
    };
}

/// Run a subprocess and return its output as a tool result.
fn runSubprocess(arena: std.mem.Allocator, id: ?std.json.Value, argv: []const []const u8) ![]const u8 {
    var child = std.process.Child.init(argv, arena);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch |e| {
        const msg = try std.fmt.allocPrint(arena, "Failed to spawn process: {any}", .{e});
        return try McpServer.toolError(arena, id, msg);
    };

    // Read all output
    const max_output: usize = 1024 * 64;
    const stdout_data = child.stdout.?.readToEndAlloc(arena, max_output) catch "";
    const stderr_data = child.stderr.?.readToEndAlloc(arena, max_output) catch "";

    const result = child.wait() catch |e| {
        const msg = try std.fmt.allocPrint(arena, "Process wait failed: {any}", .{e});
        return try McpServer.toolError(arena, id, msg);
    };

    // Combine output
    var out = std.ArrayListUnmanaged(u8){};
    if (stdout_data.len > 0) try out.appendSlice(arena, stdout_data);
    if (stderr_data.len > 0) {
        if (out.items.len > 0) try out.append(arena, '\n');
        try out.appendSlice(arena, stderr_data);
    }

    const exit_code = result.Exited;
    if (exit_code != 0) {
        if (out.items.len == 0) {
            const msg = try std.fmt.allocPrint(arena, "Process exited with code {d}", .{exit_code});
            try out.appendSlice(arena, msg);
        }
        return try McpServer.toolError(arena, id, out.items);
    }

    if (out.items.len == 0) {
        try out.appendSlice(arena, "Success");
    }

    return try McpServer.toolResult(arena, id, out.items);
}

// ============================================================================
// Static Content
// ============================================================================

const syntax_reference_text =
    \\# Cot Language Syntax Reference
    \\
    \\## Variables & Constants
    \\const x: i64 = 10    // immutable, typed
    \\const y = 20         // immutable, inferred
    \\var z = 30           // mutable, inferred
    \\No semicolons.
    \\
    \\## Types
    \\Primitives: i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bool void
    \\Aliases: int=i64, float=f64, byte=u8, string=[]u8
    \\Composite: *T (pointer), ?T (optional), E!T (error union)
    \\
    \\## Functions
    \\fn add(a: i64, b: i64) i64 { return a + b }
    \\fn noop() void { }
    \\fn apply(f: fn(i64) -> i64, x: i64) i64 { return f(x) }
    \\
    \\## Generics (separate parens, NOT angle brackets)
    \\fn max(T)(a: T, b: T) T { if (a > b) { return a } return b }
    \\struct Box(T) { value: T }
    \\var b = Box(i64) { .value = 42 }
    \\
    \\## Structs
    \\struct Point { x: i64, y: i64 }
    \\// Stack init (PERIOD prefix, EQUALS sign):
    \\var p = Point { .x = 10, .y = 20 }
    \\// Heap init (NO period, COLON separator):
    \\var h = new Point { x: 10, y: 20 }
    \\
    \\## Methods (explicit self, like Zig)
    \\impl Point {
    \\    fn sum(self: Point) i64 { return self.x + self.y }
    \\}
    \\
    \\## Traits
    \\trait Comparable { fn compare(self: *Self, other: *Self) i64 }
    \\impl Comparable for Point { fn compare(self: *Point, other: *Point) i64 { return self.x - other.x } }
    \\
    \\## Control Flow (PARENS REQUIRED for conditions)
    \\if (condition) { } else if (other) { } else { }
    \\if (optional) |val| { }  // Zig-style optional unwrap
    \\while (condition) { }
    \\for item in collection { }
    \\for i, item in collection { }  // indexed
    \\for i in 0..10 { }             // range
    \\break / continue
    \\label: while (cond) { break label }  // labeled loops
    \\Logical: and, or, not (NOT &&, ||, !)
    \\
    \\## Error Handling (Zig-style)
    \\const MyError = error { NotFound, IoError }
    \\fn read() MyError!i64 { return error.IoError }
    \\var val = read() catch 0       // catch with default
    \\var val = try read()           // propagate error
    \\errdefer cleanup()             // runs on error path only
    \\catch |err| switch (err) { error.NotFound => 0, else => -1 }
    \\
    \\## Imports
    \\import "std/json"          // stdlib module
    \\import "std/string"        // stdlib module
    \\import "./myfile"          // relative file
    \\
    \\## Builtins
    \\@alloc(size)  @dealloc(ptr)  @realloc(ptr, size)
    \\@memcpy(dst, src, len)  @sizeOf(T)  @intCast(T, val)
    \\@intToPtr(*T, addr)  @string(ptr, len)
    \\@ptrOf(s)  @lenOf(s)  @assert(cond)  @assertEq(a, b)
    \\print(val)  println(val)  eprint(val)  eprintln(val)
    \\@trap()  @targetOs()  @targetArch()
    \\@fd_read(fd, buf, len)  @fd_write(fd, buf, len)
    \\@fd_open(ptr, len, flags)  @fd_close(fd)  @fd_seek(fd, off, whence)
    \\
    \\## Tests
    \\test "my test" { @assertEq(1 + 1, 2) }
    \\Run: cot test file.cot
    \\Run filtered: cot test file.cot --filter="my test"
;

const project_info_text =
    \\# Cot Project Info
    \\
    \\## CLI Commands
    \\cot <file.cot>                  # Implicit build (compile to native)
    \\cot build <file.cot> [-o name]  # Compile to native executable
    \\cot build <file.cot> --target=wasm32  # Compile to .wasm
    \\cot run <file.cot> [-- args]    # Compile + run + cleanup
    \\cot test <file.cot>             # Run tests (test blocks)
    \\cot test <file.cot> --filter="name"  # Run specific tests
    \\cot fmt <file.cot> [-w]         # Format source code
    \\cot init [name]                 # Create new project
    \\cot lsp                         # Language server (stdio)
    \\cot mcp                         # MCP server for AI tools (stdio)
    \\cot version                     # Print version
    \\cot help [command]              # Print help
    \\
    \\## Project Structure
    \\compiler/           Zig compiler source
    \\stdlib/             Standard library (.cot files)
    \\  json.cot          JSON parser + encoder
    \\  string.cot        String ops + StringBuilder
    \\  list.cot          Generic dynamic array List(T)
    \\  map.cot           Generic hash map Map(K, V)
    \\  set.cot           Generic hash set Set(T)
    \\  fs.cot            File I/O (File struct, readFile, writeFile)
    \\  io.cot            Buffered I/O (BufferedReader, BufferedWriter)
    \\  os.cot            Process args, env, exit
    \\  time.cot          Timestamps, Timer
    \\  random.cot        Random bytes, integers
    \\  sort.cot          Insertion sort for List(T)
    \\  math.cot          abs, min, max, clamp, gcd, lcm
    \\test/e2e/           End-to-end tests
    \\test/cases/         Category unit tests
    \\
    \\## Writing Tests
    \\test "name" { @assertEq(1 + 1, 2) }
    \\test "strings" { @assertEq("hello", "hello") }
    \\Run: cot test file.cot
    \\
    \\## Targets
    \\native (default)     ARM64/x64 native binary
    \\wasm32               WebAssembly module (WasmGC)
    \\wasm32-wasi          WebAssembly with WASI imports (WasmGC)
    \\
    \\## Architecture
    \\Cot -> Scanner -> Parser -> Checker -> IR -> SSA
    \\  -> Wasm bytecode
    \\    -> .wasm file (--target=wasm32)
    \\    -> native AOT via CLIF (default)
    \\Compiler is written in Zig. Language follows Zig patterns.
;

fn getStdlibDocs(module: []const u8) []const u8 {
    if (module.len == 0) return stdlib_overview;
    if (std.mem.eql(u8, module, "json")) return stdlib_json;
    if (std.mem.eql(u8, module, "string")) return stdlib_string;
    if (std.mem.eql(u8, module, "fs")) return stdlib_fs;
    if (std.mem.eql(u8, module, "io")) return stdlib_io;
    if (std.mem.eql(u8, module, "list")) return stdlib_list;
    if (std.mem.eql(u8, module, "map")) return stdlib_map;
    if (std.mem.eql(u8, module, "os")) return stdlib_os;
    if (std.mem.eql(u8, module, "time")) return stdlib_time;
    if (std.mem.eql(u8, module, "random")) return stdlib_random;
    if (std.mem.eql(u8, module, "sort")) return stdlib_sort;
    if (std.mem.eql(u8, module, "set")) return stdlib_set;
    if (std.mem.eql(u8, module, "math")) return stdlib_math;
    return "Unknown module. Available: json, string, fs, io, list, map, os, time, random, sort, set, math";
}

const stdlib_overview =
    \\# Cot Stdlib Modules
    \\
    \\Available: json, string, fs, io, list, map, os, time, random, sort, set, math
    \\Use get_stdlib_docs with module param for details.
;

const stdlib_json =
    \\# std/json
    \\
    \\JSON parser + encoder. Values are heap-allocated i64 pointers.
    \\
    \\## Constructors (return i64)
    \\jsonNull()  jsonBool(val: bool)  jsonInt(val: i64)  jsonString(val: string)
    \\jsonArray()  jsonObject()
    \\
    \\## Accessors
    \\jsonTag(val: i64) i64       // 0=null 1=bool 2=int 3=string 4=array 5=object
    \\jsonIsNull(val: i64) bool
    \\jsonGetBool(val: i64) bool  jsonGetInt(val: i64) i64  jsonGetString(val: i64) string
    \\
    \\## Array ops
    \\jsonArrayLen(val: i64) i64  jsonArrayGet(val: i64, index: i64) i64
    \\jsonArrayPush(arr: i64, val: i64) void
    \\
    \\## Object ops
    \\jsonObjectLen(val: i64) i64  jsonObjectPut(obj: i64, key: string, val: i64) void
    \\jsonObjectGet(obj: i64, key: string) i64
    \\jsonObjectGetString(obj: i64, key: string) string
    \\jsonObjectGetInt(obj: i64, key: string) i64
    \\jsonObjectGetBool(obj: i64, key: string) bool
    \\
    \\## Parse/Encode
    \\parse(input: string) i64    encode(val: i64) string
;

const stdlib_string =
    \\# std/string
    \\
    \\charAt(s, index) i64  indexOf(s, needle) i64  lastIndexOf(s, needle) i64
    \\contains(s, needle) bool  startsWith(s, prefix) bool  endsWith(s, suffix) bool
    \\count(s, needle) i64  substring(s, start, end) string
    \\trim(s) string  trimLeft(s) string  trimRight(s) string
    \\toUpper(s) string  toLower(s) string  replace(s, old, new) string
    \\repeat(s, n) string  parseInt(s) i64  intToString(n) string
    \\compare(a, b) i64  splitInto(s, sep, result: *List(string)) void
    \\
    \\## StringBuilder
    \\var sb = StringBuilder { .buf = 0, .len = 0, .cap = 0 }
    \\sb.append(s)  sb.appendByte(b)  sb.appendInt(n)
    \\sb.toString() string  sb.length() i64  sb.clear()  sb.free()
;

const stdlib_fs =
    \\# std/fs
    \\
    \\struct File { fd: i64 }
    \\openFile(path: string, flags: i64) File
    \\createFile(path: string) File
    \\readFile(path: string) string  writeFile(path: string, data: string) void
    \\stdin() File  stdout() File  stderr() File
    \\
    \\## File methods
    \\f.read(buf, len) FsError!i64  f.write(buf, len) FsError!i64
    \\f.writeAll(s: string) FsError!i64  f.close() void  f.isValid() i64
    \\f.seekTo(pos) FsError!i64  f.seekBy(delta) FsError!i64  f.getPos() FsError!i64
    \\
    \\## Flags
    \\O_RDONLY=0  O_WRONLY=1  O_RDWR=2  O_CREATE  O_WRITE_CREATE
;

const stdlib_io =
    \\# std/io
    \\
    \\Buffered I/O. State is heap-allocated (i64 pointer).
    \\
    \\## BufferedReader
    \\newBufferedReader(fd: i64) i64
    \\newBufferedReaderSize(fd: i64, size: i64) i64
    \\readByte(r: i64) i64           // -1 on EOF
    \\readLine(r: i64) string         // empty string on EOF
    \\readerFill(r: i64) void
    \\
    \\## BufferedWriter
    \\newBufferedWriter(fd: i64) i64
    \\newBufferedWriterSize(fd: i64, size: i64) i64
    \\writeByte(w: i64, b: i64) void
    \\writeString(w: i64, s: string) void
    \\writerWriteAll(w: i64, ptr: i64, len: i64) void
    \\writerFlush(w: i64) void
;

const stdlib_list =
    \\# std/list
    \\
    \\struct List(T) { items: i64, count: i64, capacity: i64 }
    \\var l: List(i64) = .{}
    \\
    \\## Core
    \\l.append(val)  l.get(i) T  l.set(i, val)  l.pop() T
    \\l.len() i64  l.cap() i64  l.first() T  l.last() T
    \\
    \\## Modify
    \\l.insert(i, val)  l.orderedRemove(i) T  l.swapRemove(i) T
    \\l.appendSlice(src, n)  l.reverse()  l.clone() List(T)
    \\l.clear()  l.free()  l.resize(n)  l.sort(cmp)
    \\
    \\## Search
    \\l.indexOf(val) i64  l.contains(val) i64  l.equal(other) i64
;

const stdlib_map =
    \\# std/map
    \\
    \\struct Map(K, V) -- hash map with open addressing.
    \\var m: Map(i64, i64) = .{}
    \\
    \\m.set(key, val)  m.get(key) V  m.contains(key) i64
    \\m.remove(key) i64  m.len() i64  m.clear()  m.free()
    \\m.keys() List(K)  m.values() List(V)
;

const stdlib_os =
    \\# std/os
    \\
    \\exit(code: i64)  argsCount() i64  argLen(n) i64  argPtr(n) i64
    \\arg(n: i64) string  environ(n: i64) string
    \\environCount() i64  environLen(n) i64  environPtr(n) i64
;

const stdlib_time =
    \\# std/time
    \\
    \\nanoTimestamp() i64  milliTimestamp() i64  timestamp() i64
    \\struct Timer { start_time: i64 }
    \\t.elapsed() i64  t.reset() void
    \\ns_per_ms=1000000  ns_per_s=1000000000  ms_per_s=1000
;

const stdlib_random =
    \\# std/random
    \\
    \\fillBytes(buf: i64, len: i64) i64
    \\randomInt() i64  randomRange(max: i64) i64
;

const stdlib_sort =
    \\# std/sort
    \\
    \\insertionSort for List(T) with comparator function.
    \\insertionSortWith(list: *List(T), cmp: fn(T, T) -> i64) void
    \\reverse(list: *List(T)) void
;

const stdlib_set =
    \\# std/set
    \\
    \\struct Set(T) -- hash set (wraps Map(T, i64)).
    \\var s: Set(i64) = .{}
    \\s.add(val)  s.contains(val) i64  s.remove(val) i64
    \\s.len() i64  s.clear()  s.free()  s.items() []T
;

const stdlib_math =
    \\# std/math
    \\
    \\abs(x) i64  min(a, b) i64  max(a, b) i64  clamp(x, lo, hi) i64
    \\ipow(base, exp) i64  gcd(a, b) i64  lcm(a, b) i64
    \\fabs(x) f64  ceil(x) f64  floor(x) f64  sqrt(x) f64
    \\fmin(a, b) f64  fmax(a, b) f64
    \\Constants: PI, E
;

// ============================================================================
// Tests
// ============================================================================

test "MCP: initialize response" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp != null);

    // Verify it contains protocol version
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "2024-11-05") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "cot-tools") != null);
}

test "MCP: tools/list response" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp != null);

    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "get_syntax_reference") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "check_file") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "list_symbols") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "build") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "run_tests") != null);
}

test "MCP: tools/call syntax reference" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"get_syntax_reference","arguments":{}}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp != null);

    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "Cot Language Syntax") != null);
}

test "MCP: tools/call stdlib docs" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"get_stdlib_docs","arguments":{"module":"json"}}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp != null);

    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "jsonObject") != null);
}

test "MCP: unknown method" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":5,"method":"unknown/method","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp != null);

    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "-32601") != null);
}

test "MCP: notification returns null" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","method":"notifications/initialized"}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    try std.testing.expect(resp == null);
}

test "MCP: initialize includes resources and prompts capabilities" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "\"resources\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "\"prompts\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "\"tools\"") != null);
}

test "MCP: tools/list has annotations" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "annotations") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "readOnlyHint") != null);
    // New tools present
    try std.testing.expect(std.mem.indexOf(u8, response, "get_type") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "find_definition") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "find_references") != null);
}

test "MCP: resources/list response" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":10,"method":"resources/list","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "cot://syntax-reference") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "cot://project-info") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "cot://stdlib/{module}") != null);
}

test "MCP: resources/read syntax reference" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":11,"method":"resources/read","params":{"uri":"cot://syntax-reference"}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "Cot Language Syntax") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "contents") != null);
}

test "MCP: resources/read stdlib module" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":12,"method":"resources/read","params":{"uri":"cot://stdlib/json"}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "jsonObject") != null);
}

test "MCP: prompts/list response" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":20,"method":"prompts/list","params":{}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "debug-error") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "add-test") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "explain-code") != null);
}

test "MCP: prompts/get debug-error" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":21,"method":"prompts/get","params":{"name":"debug-error","arguments":{"error_message":"undeclared identifier"}}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "messages") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "undeclared identifier") != null);
}

test "MCP: prompts/get explain-code" {
    const allocator = std.testing.allocator;
    var server = McpServer.init(allocator);
    defer server.deinit();

    const msg =
        \\{"jsonrpc":"2.0","id":22,"method":"prompts/get","params":{"name":"explain-code","arguments":{"code":"fn add(a: i64, b: i64) i64 { return a + b }"}}}
    ;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const resp = try server.handleMessage(arena.allocator(), msg);
    const response = resp.?;
    try std.testing.expect(std.mem.indexOf(u8, response, "messages") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "fn add") != null);
}
