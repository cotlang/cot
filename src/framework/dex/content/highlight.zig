//! Syntax Highlighting
//!
//! Token-based syntax highlighting for code blocks.
//! Generates HTML with CSS classes for styling with Tailwind or custom themes.
//!
//! Supported languages:
//! - cot, dex, zig (native languages)
//! - js, ts, javascript, typescript
//! - json, html, css
//! - bash, sh, shell
//! - rust, go, python

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Token type for syntax highlighting
pub const TokenType = enum {
    keyword,
    string,
    number,
    comment,
    function,
    type_name,
    operator,
    punctuation,
    variable,
    attribute,
    tag,
    plain,

    pub fn cssClass(self: TokenType) []const u8 {
        return switch (self) {
            .keyword => "hl-keyword",
            .string => "hl-string",
            .number => "hl-number",
            .comment => "hl-comment",
            .function => "hl-function",
            .type_name => "hl-type",
            .operator => "hl-operator",
            .punctuation => "hl-punctuation",
            .variable => "hl-variable",
            .attribute => "hl-attribute",
            .tag => "hl-tag",
            .plain => "hl-plain",
        };
    }
};

/// A highlighted token
pub const Token = struct {
    text: []const u8,
    token_type: TokenType,
};

/// Language definition
pub const Language = struct {
    name: []const u8,
    keywords: []const []const u8,
    types: []const []const u8,
    single_comment: ?[]const u8,
    multi_comment_start: ?[]const u8,
    multi_comment_end: ?[]const u8,
    string_delimiters: []const u8,
};

/// Predefined languages
pub const languages = struct {
    pub const zig = Language{
        .name = "zig",
        .keywords = &.{
            "const",       "var",       "fn",       "pub",     "return",   "if",
            "else",        "while",     "for",      "switch",  "break",    "continue",
            "try",         "catch",     "errdefer", "defer",   "struct",   "enum",
            "union",       "error",     "comptime", "inline",  "extern",   "export",
            "test",        "and",       "or",       "orelse",  "undefined", "null",
            "true",        "false",     "unreachable",
        },
        .types = &.{
            "u8",    "u16",   "u32",   "u64",    "u128",  "usize",
            "i8",    "i16",   "i32",   "i64",    "i128",  "isize",
            "f16",   "f32",   "f64",   "f128",   "bool",  "void",
            "type",  "anytype", "anyerror", "noreturn",
        },
        .single_comment = "//",
        .multi_comment_start = null,
        .multi_comment_end = null,
        .string_delimiters = "\"'",
    };

    pub const cot = Language{
        .name = "cot",
        .keywords = &.{
            "fn",       "let",      "const",    "var",      "if",       "else",
            "while",    "for",      "return",   "break",    "continue", "struct",
            "enum",     "trait",    "impl",     "import",   "pub",      "match",
            "true",     "false",    "null",     "self",     "dyn",
        },
        .types = &.{
            "int",      "float",    "string",   "bool",     "void",     "any",
            "i8",       "i16",      "i32",      "i64",      "u8",       "u16",
            "u32",      "u64",      "f32",      "f64",
        },
        .single_comment = "//",
        .multi_comment_start = "/*",
        .multi_comment_end = "*/",
        .string_delimiters = "\"'",
    };

    pub const dex = Language{
        .name = "dex",
        .keywords = &.{
            "component", "state",    "props",    "handlers", "template", "fn",
            "let",       "const",    "if",       "else",     "for",      "in",
            "self",      "true",     "false",    "null",
        },
        .types = &.{
            "int",      "float",    "string",   "bool",     "list",     "map",
        },
        .single_comment = "//",
        .multi_comment_start = null,
        .multi_comment_end = null,
        .string_delimiters = "\"'",
    };

    pub const javascript = Language{
        .name = "javascript",
        .keywords = &.{
            "const",     "let",       "var",       "function", "return",   "if",
            "else",      "while",     "for",       "switch",   "case",     "break",
            "continue",  "try",       "catch",     "finally",  "throw",    "new",
            "class",     "extends",   "import",    "export",   "default",  "async",
            "await",     "this",      "super",     "typeof",   "instanceof", "in",
            "of",        "true",      "false",     "null",     "undefined",
        },
        .types = &.{
            "Array",    "Object",   "String",   "Number",   "Boolean",  "Function",
            "Promise",  "Map",      "Set",      "Date",     "RegExp",
        },
        .single_comment = "//",
        .multi_comment_start = "/*",
        .multi_comment_end = "*/",
        .string_delimiters = "\"'`",
    };

    pub const typescript = javascript;
    pub const js = javascript;
    pub const ts = javascript;

    pub const json = Language{
        .name = "json",
        .keywords = &.{ "true", "false", "null" },
        .types = &.{},
        .single_comment = null,
        .multi_comment_start = null,
        .multi_comment_end = null,
        .string_delimiters = "\"",
    };

    pub const bash = Language{
        .name = "bash",
        .keywords = &.{
            "if",       "then",     "else",     "elif",     "fi",       "for",
            "while",    "do",       "done",     "case",     "esac",     "in",
            "function", "return",   "exit",     "export",   "local",    "readonly",
            "shift",    "echo",     "cd",       "pwd",      "ls",       "rm",
            "mkdir",    "cp",       "mv",       "cat",      "grep",     "sed",
            "awk",      "git",      "npm",      "yarn",     "cot",      "zig",
        },
        .types = &.{},
        .single_comment = "#",
        .multi_comment_start = null,
        .multi_comment_end = null,
        .string_delimiters = "\"'",
    };

    pub const sh = bash;
    pub const shell = bash;

    pub const html = Language{
        .name = "html",
        .keywords = &.{},
        .types = &.{},
        .single_comment = null,
        .multi_comment_start = "<!--",
        .multi_comment_end = "-->",
        .string_delimiters = "\"'",
    };

    pub const css = Language{
        .name = "css",
        .keywords = &.{
            "import",   "media",    "keyframes", "font-face", "supports",
        },
        .types = &.{},
        .single_comment = null,
        .multi_comment_start = "/*",
        .multi_comment_end = "*/",
        .string_delimiters = "\"'",
    };
};

/// Get language by name (case-insensitive)
pub fn getLanguage(name: []const u8) ?Language {
    if (std.ascii.eqlIgnoreCase(name, "zig")) return languages.zig;
    if (std.ascii.eqlIgnoreCase(name, "cot")) return languages.cot;
    if (std.ascii.eqlIgnoreCase(name, "dex")) return languages.dex;
    if (std.ascii.eqlIgnoreCase(name, "js") or std.ascii.eqlIgnoreCase(name, "javascript")) return languages.javascript;
    if (std.ascii.eqlIgnoreCase(name, "ts") or std.ascii.eqlIgnoreCase(name, "typescript")) return languages.typescript;
    if (std.ascii.eqlIgnoreCase(name, "json")) return languages.json;
    if (std.ascii.eqlIgnoreCase(name, "bash") or std.ascii.eqlIgnoreCase(name, "sh") or std.ascii.eqlIgnoreCase(name, "shell")) return languages.bash;
    if (std.ascii.eqlIgnoreCase(name, "html")) return languages.html;
    if (std.ascii.eqlIgnoreCase(name, "css")) return languages.css;

    return null;
}

/// Syntax highlighter
pub const Highlighter = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    /// Highlight code and return HTML
    pub fn highlight(self: *Self, code: []const u8, lang_name: ?[]const u8) ![]const u8 {
        const lang = if (lang_name) |name| getLanguage(name) else null;

        var html: std.ArrayListUnmanaged(u8) = .empty;
        errdefer html.deinit(self.allocator);

        var i: usize = 0;
        while (i < code.len) {
            // Check for comments
            if (lang) |l| {
                if (l.single_comment) |comment_start| {
                    if (std.mem.startsWith(u8, code[i..], comment_start)) {
                        // Single-line comment
                        const end = std.mem.indexOfScalar(u8, code[i..], '\n') orelse code.len - i;
                        try self.appendToken(&html, code[i .. i + end], .comment);
                        i += end;
                        continue;
                    }
                }

                if (l.multi_comment_start) |mc_start| {
                    if (std.mem.startsWith(u8, code[i..], mc_start)) {
                        const end_marker = l.multi_comment_end.?;
                        const end = std.mem.indexOf(u8, code[i + mc_start.len ..], end_marker) orelse code.len - i - mc_start.len;
                        try self.appendToken(&html, code[i .. i + mc_start.len + end + end_marker.len], .comment);
                        i += mc_start.len + end + end_marker.len;
                        continue;
                    }
                }
            }

            // Check for strings
            if (lang) |l| {
                var found_string = false;
                for (l.string_delimiters) |delim| {
                    if (code[i] == delim) {
                        const str_end = findStringEnd(code[i + 1 ..], delim);
                        try self.appendToken(&html, code[i .. i + 2 + str_end], .string);
                        i += 2 + str_end;
                        found_string = true;
                        break;
                    }
                }
                if (found_string) continue;
            }

            // Check for numbers
            if (std.ascii.isDigit(code[i])) {
                var end = i + 1;
                while (end < code.len and (std.ascii.isDigit(code[end]) or code[end] == '.' or code[end] == 'x' or
                    code[end] == 'b' or code[end] == 'o' or
                    (code[end] >= 'a' and code[end] <= 'f') or
                    (code[end] >= 'A' and code[end] <= 'F') or code[end] == '_')) : (end += 1)
                {}
                try self.appendToken(&html, code[i..end], .number);
                i = end;
                continue;
            }

            // Check for identifiers
            if (std.ascii.isAlphabetic(code[i]) or code[i] == '_' or code[i] == '@') {
                var end = i + 1;
                while (end < code.len and (std.ascii.isAlphanumeric(code[end]) or code[end] == '_')) : (end += 1) {}
                const ident = code[i..end];

                const token_type = if (lang) |l| blk: {
                    // Check if it's a keyword
                    for (l.keywords) |kw| {
                        if (std.mem.eql(u8, ident, kw)) break :blk TokenType.keyword;
                    }
                    // Check if it's a type
                    for (l.types) |t| {
                        if (std.mem.eql(u8, ident, t)) break :blk TokenType.type_name;
                    }
                    // Check if followed by (
                    if (end < code.len and code[end] == '(') break :blk TokenType.function;
                    break :blk TokenType.plain;
                } else TokenType.plain;

                try self.appendToken(&html, ident, token_type);
                i = end;
                continue;
            }

            // Operators and punctuation
            if (isOperator(code[i])) {
                try self.appendToken(&html, code[i .. i + 1], .operator);
                i += 1;
                continue;
            }

            if (isPunctuation(code[i])) {
                try self.appendToken(&html, code[i .. i + 1], .punctuation);
                i += 1;
                continue;
            }

            // Plain text (whitespace, etc.)
            try appendEscaped(&html, self.allocator, code[i .. i + 1]);
            i += 1;
        }

        return html.toOwnedSlice(self.allocator);
    }

    fn appendToken(self: *Self, html: *std.ArrayListUnmanaged(u8), text: []const u8, token_type: TokenType) !void {
        if (token_type == .plain) {
            try appendEscaped(html, self.allocator, text);
        } else {
            try html.appendSlice(self.allocator, "<span class=\"");
            try html.appendSlice(self.allocator, token_type.cssClass());
            try html.appendSlice(self.allocator, "\">");
            try appendEscaped(html, self.allocator, text);
            try html.appendSlice(self.allocator, "</span>");
        }
    }
};

fn findStringEnd(text: []const u8, delim: u8) usize {
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (text[i] == '\\' and i + 1 < text.len) {
            i += 1; // Skip escaped character
            continue;
        }
        if (text[i] == delim) {
            return i;
        }
    }
    return text.len;
}

fn isStringDelimiter(c: u8, delimiters: []const u8) bool {
    for (delimiters) |d| {
        if (c == d) return true;
    }
    return false;
}

fn isOperator(c: u8) bool {
    return switch (c) {
        '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|', '^', '~', '?' => true,
        else => false,
    };
}

fn isPunctuation(c: u8) bool {
    return switch (c) {
        '(', ')', '[', ']', '{', '}', ',', '.', ';', ':', '@', '#' => true,
        else => false,
    };
}

fn appendEscaped(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator, text: []const u8) !void {
    for (text) |c| {
        switch (c) {
            '<' => try buf.appendSlice(allocator, "&lt;"),
            '>' => try buf.appendSlice(allocator, "&gt;"),
            '&' => try buf.appendSlice(allocator, "&amp;"),
            '"' => try buf.appendSlice(allocator, "&quot;"),
            else => try buf.append(allocator, c),
        }
    }
}

/// Generate CSS for syntax highlighting (Tailwind-compatible)
pub fn generateThemeCSS(dark_mode: bool) []const u8 {
    if (dark_mode) {
        return
            \\.hl-keyword { color: #c678dd; font-weight: 600; }
            \\.hl-string { color: #98c379; }
            \\.hl-number { color: #d19a66; }
            \\.hl-comment { color: #5c6370; font-style: italic; }
            \\.hl-function { color: #61afef; }
            \\.hl-type { color: #e5c07b; }
            \\.hl-operator { color: #56b6c2; }
            \\.hl-punctuation { color: #abb2bf; }
            \\.hl-variable { color: #e06c75; }
            \\.hl-attribute { color: #d19a66; }
            \\.hl-tag { color: #e06c75; }
            \\.hl-plain { color: #abb2bf; }
        ;
    } else {
        return
            \\.hl-keyword { color: #8250df; font-weight: 600; }
            \\.hl-string { color: #0a3069; }
            \\.hl-number { color: #0550ae; }
            \\.hl-comment { color: #6e7781; font-style: italic; }
            \\.hl-function { color: #8250df; }
            \\.hl-type { color: #953800; }
            \\.hl-operator { color: #cf222e; }
            \\.hl-punctuation { color: #24292f; }
            \\.hl-variable { color: #953800; }
            \\.hl-attribute { color: #0550ae; }
            \\.hl-tag { color: #116329; }
            \\.hl-plain { color: #24292f; }
        ;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "highlight javascript" {
    const allocator = std.testing.allocator;

    var highlighter = Highlighter.init(allocator);
    const html = try highlighter.highlight("const x = 42;", "js");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "hl-keyword") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "hl-number") != null);
}

test "highlight zig" {
    const allocator = std.testing.allocator;

    var highlighter = Highlighter.init(allocator);
    const html = try highlighter.highlight("fn main() void {}", "zig");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "hl-keyword") != null);
    try std.testing.expect(std.mem.indexOf(u8, html, "hl-type") != null);
}

test "highlight comment" {
    const allocator = std.testing.allocator;

    var highlighter = Highlighter.init(allocator);
    const html = try highlighter.highlight("// comment\ncode", "js");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "hl-comment") != null);
}

test "highlight string" {
    const allocator = std.testing.allocator;

    var highlighter = Highlighter.init(allocator);
    const html = try highlighter.highlight("\"hello\"", "js");
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "hl-string") != null);
}

test "get language" {
    try std.testing.expect(getLanguage("zig") != null);
    try std.testing.expect(getLanguage("js") != null);
    try std.testing.expect(getLanguage("javascript") != null);
    try std.testing.expect(getLanguage("unknown") == null);
}
