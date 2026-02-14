//! textDocument/completion — autocomplete suggestions.
//! Supports: @ builtins, dot-completion for struct fields/methods,
//! prefix filtering, keyword completions, and snippet templates.

const std = @import("std");
const analysis = @import("analysis.zig");
const checker_mod = @import("../frontend/checker.zig");
const types_mod = @import("../frontend/types.zig");

const AnalysisResult = analysis.AnalysisResult;
const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const Type = types_mod.Type;
const invalid_type = types_mod.invalid_type;

/// LSP CompletionItemKind values.
const CompletionItemKind = struct {
    const function = 3;
    const variable = 6;
    const class = 7;
    const property = 10; // For struct fields
    const method = 2; // For methods
    const constant = 21;
    const keyword = 14;
    const snippet = 15;
    const type_param = 25;
    const enum_member = 20;
};

/// LSP InsertTextFormat values.
pub const InsertTextFormat = enum(u32) {
    plain_text = 1,
    snippet = 2,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: u32,
    detail: ?[]const u8 = null,
    insert_text: ?[]const u8 = null,
    insert_text_format: InsertTextFormat = .plain_text,
};

/// Builtin names for @ completions (sorted for display).
const builtin_names = [_][]const u8{
    "abs",           "alignOf",       "alloc",         "arg_len",
    "arg_ptr",       "args_count",    "assert",        "assert_eq",
    "ceil",          "compileError",  "dealloc",       "environ_count",
    "environ_len",   "environ_ptr",   "exit",          "fmax",
    "fmin",          "fd_close",      "fd_open",       "fd_read",
    "fd_seek",       "fd_write",      "floor",         "intCast",
    "intToPtr",      "lenOf",         "memcpy",        "ptrCast",
    "ptrOf",         "ptrToInt",      "random",        "realloc",
    "round",         "sizeOf",        "sqrt",          "string",
    "target",        "target_arch",   "target_os",     "time",
    "trap",          "trunc",
};

/// Cot keywords for statement-position completions.
const keyword_names = [_][]const u8{
    "break",    "comptime",  "const",    "continue", "defer",
    "else",     "enum",      "errdefer", "error",    "extern",
    "false",    "fn",        "for",      "if",       "impl",
    "import",   "in",        "let",      "new",      "not",
    "null",     "or",        "and",      "return",   "struct",
    "switch",   "test",      "trait",    "true",     "try",
    "type",     "undefined", "union",    "var",      "void",
    "where",    "while",
};

/// Code snippet templates for common patterns.
const snippets = [_]struct { label: []const u8, detail: []const u8, insert_text: []const u8 }{
    .{ .label = "fn", .detail = "function declaration", .insert_text = "fn ${1:name}(${2:}) ${3:void} {\n\t$0\n}" },
    .{ .label = "test", .detail = "test block", .insert_text = "test \"${1:name}\" {\n\t$0\n}" },
    .{ .label = "if", .detail = "if statement", .insert_text = "if (${1:condition}) {\n\t$0\n}" },
    .{ .label = "if else", .detail = "if-else statement", .insert_text = "if (${1:condition}) {\n\t$2\n} else {\n\t$0\n}" },
    .{ .label = "while", .detail = "while loop", .insert_text = "while (${1:condition}) {\n\t$0\n}" },
    .{ .label = "for", .detail = "for loop", .insert_text = "for (${1:items}) |${2:item}| {\n\t$0\n}" },
    .{ .label = "struct", .detail = "struct declaration", .insert_text = "struct ${1:Name} {\n\t${2:field}: ${3:i64}\n}" },
    .{ .label = "impl", .detail = "impl block", .insert_text = "impl ${1:Type} {\n\tfn ${2:method}(self: *${1:Type}) ${3:void} {\n\t\t$0\n\t}\n}" },
    .{ .label = "switch", .detail = "switch expression", .insert_text = "switch (${1:value}) {\n\t${2:pattern} => $0,\n}" },
    .{ .label = "import", .detail = "import statement", .insert_text = "import \"${1:module}\"" },
};

/// Get completion items for the given byte offset.
pub fn getCompletions(allocator: std.mem.Allocator, result: *AnalysisResult, byte_offset: u32) ![]CompletionItem {
    var items = std.ArrayListUnmanaged(CompletionItem){};

    const content = result.src.content;
    if (byte_offset == 0 or byte_offset > content.len) {
        return addGeneralCompletions(allocator, result, &items, "");
    }

    // Determine trigger context
    const prev_char = content[byte_offset - 1];

    if (prev_char == '@') {
        // @ trigger: show builtins
        for (&builtin_names) |name| {
            try items.append(allocator, .{
                .label = name,
                .kind = CompletionItemKind.function,
                .detail = "builtin",
            });
        }
        return items.toOwnedSlice(allocator);
    }

    if (prev_char == '.') {
        // Dot trigger: find the expression before the dot and show its fields/methods
        return getDotCompletions(allocator, result, &items, byte_offset);
    }

    // General completion: get the prefix the user is typing
    const prefix = getPrefix(content, byte_offset);
    return addGeneralCompletions(allocator, result, &items, prefix);
}

/// Get dot-completion items: struct fields + methods for the type before the dot.
fn getDotCompletions(allocator: std.mem.Allocator, result: *AnalysisResult, items: *std.ArrayListUnmanaged(CompletionItem), byte_offset: u32) ![]CompletionItem {
    // Find the identifier before the dot
    const content = result.src.content;
    if (byte_offset < 2) return items.toOwnedSlice(allocator);

    // Walk backwards past the dot to find the base expression
    const dot_pos = byte_offset - 1;
    if (dot_pos == 0) return items.toOwnedSlice(allocator);

    // Get the identifier before the dot
    var end = dot_pos;
    while (end > 0 and (std.ascii.isAlphanumeric(content[end - 1]) or content[end - 1] == '_')) {
        end -= 1;
    }
    const base_name = content[end..dot_pos];
    if (base_name.len == 0) return items.toOwnedSlice(allocator);

    // Look up the symbol to get its type
    var type_idx: TypeIndex = invalid_type;

    if (result.global_scope.lookup(base_name)) |sym| {
        type_idx = sym.type_idx;
    }

    if (type_idx == invalid_type) return items.toOwnedSlice(allocator);

    // Resolve through pointers
    var resolved_idx = type_idx;
    const t = result.type_reg.get(resolved_idx);
    switch (t) {
        .pointer => |ptr| resolved_idx = ptr.elem,
        else => {},
    }

    const resolved = result.type_reg.get(resolved_idx);
    const type_name = result.type_reg.typeName(resolved_idx);

    switch (resolved) {
        .struct_type => |st| {
            // Add struct fields
            for (st.fields) |field| {
                try items.append(allocator, .{
                    .label = field.name,
                    .kind = CompletionItemKind.property,
                    .detail = result.type_reg.typeName(field.type_idx),
                });
            }

            // Add methods from method registry
            if (result.type_reg.method_registry.get(type_name)) |methods| {
                for (methods.items) |method| {
                    const detail = methodDetail(allocator, &result.type_reg, method);
                    try items.append(allocator, .{
                        .label = method.name,
                        .kind = CompletionItemKind.method,
                        .detail = detail,
                    });
                }
            }
        },
        .enum_type => |et| {
            // Add enum variants
            for (et.variants) |v| {
                try items.append(allocator, .{
                    .label = v.name,
                    .kind = CompletionItemKind.enum_member,
                    .detail = "enum variant",
                });
            }
        },
        else => {},
    }

    return items.toOwnedSlice(allocator);
}

/// Format method detail: (param1: Type1, ...) -> ReturnType
fn methodDetail(allocator: std.mem.Allocator, reg: *const TypeRegistry, method: types_mod.MethodInfo) ?[]const u8 {
    const mt = reg.get(method.func_type);
    switch (mt) {
        .func => |ft| {
            var sig = std.ArrayListUnmanaged(u8){};
            sig.append(allocator, '(') catch return null;
            // Skip self param
            const start: usize = if (ft.params.len > 0) 1 else 0;
            for (ft.params[start..], 0..) |param, i| {
                if (i > 0) sig.appendSlice(allocator, ", ") catch return null;
                sig.appendSlice(allocator, param.name) catch return null;
                sig.appendSlice(allocator, ": ") catch return null;
                sig.appendSlice(allocator, reg.typeName(param.type_idx)) catch return null;
            }
            sig.appendSlice(allocator, ") ") catch return null;
            sig.appendSlice(allocator, reg.typeName(ft.return_type)) catch return null;
            return sig.toOwnedSlice(allocator) catch null;
        },
        else => return null,
    }
}

/// Add general completions (scope symbols, keywords, snippets) filtered by prefix.
fn addGeneralCompletions(allocator: std.mem.Allocator, result: *AnalysisResult, items: *std.ArrayListUnmanaged(CompletionItem), prefix: []const u8) ![]CompletionItem {
    // Scope symbols
    var scope_iter = result.global_scope.symbols.iterator();
    while (scope_iter.next()) |entry| {
        const name = entry.key_ptr.*;
        if (prefix.len > 0 and !hasPrefix(name, prefix)) continue;

        const sym = entry.value_ptr.*;
        const kind: u32 = switch (sym.kind) {
            .function => CompletionItemKind.function,
            .type_name => CompletionItemKind.class,
            .constant => CompletionItemKind.constant,
            .variable, .parameter => CompletionItemKind.variable,
        };
        const detail = typeDetail(allocator, &result.type_reg, sym);
        try items.append(allocator, .{
            .label = name,
            .kind = kind,
            .detail = detail,
        });
    }

    // Keywords (filtered by prefix)
    for (&keyword_names) |name| {
        if (prefix.len > 0 and !hasPrefix(name, prefix)) continue;
        try items.append(allocator, .{
            .label = name,
            .kind = CompletionItemKind.keyword,
            .detail = "keyword",
        });
    }

    // Snippets (only when prefix is empty or matches)
    for (&snippets) |s| {
        if (prefix.len > 0 and !hasPrefix(s.label, prefix)) continue;
        try items.append(allocator, .{
            .label = s.label,
            .kind = CompletionItemKind.snippet,
            .detail = s.detail,
            .insert_text = s.insert_text,
            .insert_text_format = .snippet,
        });
    }

    return items.toOwnedSlice(allocator);
}

/// Get short type detail for a symbol (for completion detail field).
fn typeDetail(allocator: std.mem.Allocator, reg: *const TypeRegistry, sym: checker_mod.Symbol) ?[]const u8 {
    const type_name = reg.typeName(sym.type_idx);
    if (std.mem.eql(u8, type_name, "invalid")) return null;

    switch (sym.kind) {
        .function => {
            const t = reg.get(sym.type_idx);
            switch (t) {
                .func => |ft| {
                    var sig = std.ArrayListUnmanaged(u8){};
                    sig.appendSlice(allocator, "fn(") catch return type_name;
                    for (ft.params, 0..) |param, i| {
                        if (i > 0) sig.appendSlice(allocator, ", ") catch return type_name;
                        sig.appendSlice(allocator, reg.typeName(param.type_idx)) catch return type_name;
                    }
                    sig.appendSlice(allocator, ") ") catch return type_name;
                    sig.appendSlice(allocator, reg.typeName(ft.return_type)) catch return type_name;
                    return sig.toOwnedSlice(allocator) catch type_name;
                },
                else => return type_name,
            }
        },
        else => return type_name,
    }
}

/// Get the identifier prefix being typed at the cursor position.
fn getPrefix(content: []const u8, byte_offset: u32) []const u8 {
    var start = byte_offset;
    while (start > 0 and (std.ascii.isAlphanumeric(content[start - 1]) or content[start - 1] == '_')) {
        start -= 1;
    }
    return content[start..byte_offset];
}

/// Case-insensitive prefix match.
fn hasPrefix(str: []const u8, prefix: []const u8) bool {
    if (prefix.len > str.len) return false;
    for (str[0..prefix.len], prefix) |a, b| {
        if (std.ascii.toLower(a) != std.ascii.toLower(b)) return false;
    }
    return true;
}
