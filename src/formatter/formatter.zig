//! Code Formatter for Cot and DBL source files
//!
//! This module provides formatting functionality shared between:
//! - CLI: `cot fmt <files...>`
//! - LSP: textDocument/formatting
//!
//! Inspired by `zig fmt`, this formatter uses a line-by-line approach
//! that tracks indentation based on braces (Cot) or keywords (DBL).

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Detected language for formatting
pub const Language = enum {
    cot,
    dbl,

    /// Detect language from file extension
    pub fn fromPath(path: []const u8) ?Language {
        if (std.mem.endsWith(u8, path, ".cot")) return .cot;
        if (std.mem.endsWith(u8, path, ".dbl")) return .dbl;
        return null;
    }
};

/// A text edit representing a range replacement
pub const TextEdit = struct {
    /// Starting line (0-indexed)
    start_line: u32,
    /// Starting character in the line (0-indexed)
    start_char: u32,
    /// Ending line (0-indexed)
    end_line: u32,
    /// Ending character in the line (0-indexed)
    end_char: u32,
    /// Replacement text
    new_text: []const u8,
};

/// Formatting options
pub const FormatOptions = struct {
    /// Number of spaces per indent level (default: 4)
    tab_size: u32 = 4,
    /// Use spaces instead of tabs (default: true)
    insert_spaces: bool = true,
};

/// Format source code and return the formatted result
/// Returns null if no changes needed, otherwise returns the formatted source
pub fn format(allocator: Allocator, source: []const u8, language: Language, options: FormatOptions) !?[]const u8 {
    const edits = try formatToEdits(allocator, source, language, options);
    defer {
        for (edits) |edit| {
            allocator.free(edit.new_text);
        }
        allocator.free(edits);
    }

    if (edits.len == 0) {
        return null; // No changes needed
    }

    // Apply edits to produce formatted source
    return try applyEdits(allocator, source, edits);
}

/// Format source code and return text edits (for LSP integration)
pub fn formatToEdits(allocator: Allocator, source: []const u8, language: Language, options: FormatOptions) ![]TextEdit {
    return switch (language) {
        .cot => formatCot(allocator, source, options.tab_size, options.insert_spaces),
        .dbl => formatDbl(allocator, source, options.tab_size, options.insert_spaces),
    };
}

/// Apply text edits to source and produce formatted output
fn applyEdits(allocator: Allocator, source: []const u8, edits: []const TextEdit) ![]const u8 {
    // Build line starts array for efficient offset calculation
    var line_starts: std.ArrayListUnmanaged(usize) = .empty;
    defer line_starts.deinit(allocator);
    try line_starts.append(allocator, 0);

    for (source, 0..) |c, i| {
        if (c == '\n') {
            try line_starts.append(allocator, i + 1);
        }
    }

    // Calculate result size
    var result_size: usize = source.len;
    for (edits) |edit| {
        const start_offset = getOffset(line_starts.items, edit.start_line, edit.start_char);
        const end_offset = getOffset(line_starts.items, edit.end_line, edit.end_char);
        const old_len = end_offset - start_offset;
        result_size = result_size - old_len + edit.new_text.len;
    }

    // Build result
    var result = try allocator.alloc(u8, result_size);
    var src_pos: usize = 0;
    var dst_pos: usize = 0;

    for (edits) |edit| {
        const start_offset = getOffset(line_starts.items, edit.start_line, edit.start_char);
        const end_offset = getOffset(line_starts.items, edit.end_line, edit.end_char);

        // Copy unchanged portion
        const unchanged_len = start_offset - src_pos;
        @memcpy(result[dst_pos .. dst_pos + unchanged_len], source[src_pos..start_offset]);
        dst_pos += unchanged_len;

        // Apply edit
        @memcpy(result[dst_pos .. dst_pos + edit.new_text.len], edit.new_text);
        dst_pos += edit.new_text.len;

        src_pos = end_offset;
    }

    // Copy remaining
    const remaining = source.len - src_pos;
    @memcpy(result[dst_pos .. dst_pos + remaining], source[src_pos..]);

    return result;
}

fn getOffset(line_starts: []const usize, line: u32, char: u32) usize {
    if (line >= line_starts.len) {
        return line_starts[line_starts.len - 1];
    }
    return line_starts[line] + char;
}

/// Format Cot source code (brace-based indentation)
fn formatCot(allocator: Allocator, source: []const u8, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    var edits: std.ArrayListUnmanaged(TextEdit) = .empty;
    errdefer {
        for (edits.items) |edit| {
            allocator.free(edit.new_text);
        }
        edits.deinit(allocator);
    }

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
                        .start_line = line_num,
                        .start_char = 0,
                        .end_line = line_num,
                        .end_char = content_start,
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

/// Format DBL source code (keyword-based indentation)
fn formatDbl(allocator: Allocator, source: []const u8, tab_size: u32, insert_spaces: bool) ![]TextEdit {
    var edits: std.ArrayListUnmanaged(TextEdit) = .empty;
    errdefer {
        for (edits.items) |edit| {
            allocator.free(edit.new_text);
        }
        edits.deinit(allocator);
    }

    // Build the indent string
    var indent_buf: [16]u8 = undefined;
    const indent_str = if (insert_spaces) blk: {
        const size = @min(tab_size, 16);
        @memset(indent_buf[0..size], ' ');
        break :blk indent_buf[0..size];
    } else "\t";

    // Keywords that increase indent level
    const indent_keywords = [_][]const u8{
        "record",    "group", "structure",  "common",   "global",   "literal",
        "begin",     "using", "class",      "method",   "property", "namespace",
        "interface", "try",   "subroutine", "function", "test",
    };

    // Keywords that should be at indent 0 but set indent to 1 for following lines
    const proc_like_keywords = [_][]const u8{"proc"};

    // Keywords that decrease indent level by 1
    const dedent_keywords = [_][]const u8{
        "endrecord",    "endgroup",   "endstructure", "endcommon",
        "endglobal",    "endliteral", "end",          "endusing",
        "endclass",     "endmethod",  "endproperty",  "endnamespace",
        "endinterface", "endtry",     "endproc",
    };

    // Keywords that reset indent to 0 (end of top-level blocks)
    const reset_indent_keywords = [_][]const u8{
        "endmain", "endfunction", "endsubroutine", "endtest",
    };

    // Control flow keywords that indent the next single statement (if no begin)
    const control_flow_keywords = [_][]const u8{
        "if", "else", "while", "for", "do", "using", "case",
    };

    // Process line by line
    var line_num: u32 = 0;
    var line_start: usize = 0;
    var indent_level: u32 = 0;
    var pending_single_indent: bool = false;

    var i: usize = 0;
    while (i <= source.len) : (i += 1) {
        const at_end = i == source.len;
        const at_newline = !at_end and source[i] == '\n';

        if (at_newline or at_end) {
            const line_end = i;
            const line = source[line_start..line_end];

            const trimmed = std.mem.trim(u8, line, " \t");
            if (trimmed.len > 0) {
                // Check if line starts with reset-indent keyword
                var should_reset = false;
                for (reset_indent_keywords) |kw| {
                    if (startsWithKeyword(trimmed, kw)) {
                        should_reset = true;
                        break;
                    }
                }

                // Check if line starts with proc-like keyword
                var is_proc_like = false;
                if (!should_reset) {
                    for (proc_like_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            is_proc_like = true;
                            break;
                        }
                    }
                }

                // Check if line starts with dedent keyword
                var should_dedent = false;
                if (!should_reset and !is_proc_like) {
                    for (dedent_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            should_dedent = true;
                            break;
                        }
                    }
                }

                // Apply indent changes before this line
                if (should_reset) {
                    indent_level = 0;
                    pending_single_indent = false;
                } else if (is_proc_like) {
                    indent_level = 0;
                    pending_single_indent = false;
                } else if (should_dedent and indent_level > 0) {
                    indent_level -= 1;
                    pending_single_indent = false;
                }

                // Calculate expected indent
                var effective_indent = indent_level;
                if (pending_single_indent) {
                    effective_indent += 1;
                    pending_single_indent = false;
                }
                const expected_indent = effective_indent * @as(u32, @intCast(indent_str.len));

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
                        .start_line = line_num,
                        .start_char = 0,
                        .end_line = line_num,
                        .end_char = content_start,
                        .new_text = new_indent,
                    });
                }

                // Update indent for next line
                if (is_proc_like) {
                    indent_level = 1;
                } else {
                    for (indent_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            indent_level += 1;
                            break;
                        }
                    }

                    // Check for control flow without begin
                    for (control_flow_keywords) |kw| {
                        if (startsWithKeyword(trimmed, kw)) {
                            if (!containsKeyword(trimmed, "begin")) {
                                pending_single_indent = true;
                            }
                            break;
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

/// Check if line starts with a keyword (case-insensitive, word boundary)
fn startsWithKeyword(line: []const u8, keyword: []const u8) bool {
    if (line.len < keyword.len) return false;

    for (keyword, 0..) |kc, j| {
        const lc = std.ascii.toLower(line[j]);
        const kwc = std.ascii.toLower(kc);
        if (lc != kwc) return false;
    }

    if (line.len == keyword.len) return true;
    const next_char = line[keyword.len];
    return !std.ascii.isAlphanumeric(next_char) and next_char != '_';
}

/// Check if line contains a keyword (case-insensitive, word boundary)
fn containsKeyword(line: []const u8, keyword: []const u8) bool {
    var j: usize = 0;
    while (j + keyword.len <= line.len) : (j += 1) {
        if (std.ascii.eqlIgnoreCase(line[j .. j + keyword.len], keyword)) {
            const before_ok = j == 0 or !std.ascii.isAlphanumeric(line[j - 1]);
            const after_ok = j + keyword.len >= line.len or !std.ascii.isAlphanumeric(line[j + keyword.len]);
            if (before_ok and after_ok) {
                return true;
            }
        }
    }
    return false;
}

// ============================================================
// Tests
// ============================================================

test "format cot - basic braces" {
    const allocator = std.testing.allocator;
    const source =
        \\fn main() {
        \\let x = 1
        \\if (x > 0) {
        \\println("positive")
        \\}
        \\}
    ;
    const expected =
        \\fn main() {
        \\    let x = 1
        \\    if (x > 0) {
        \\        println("positive")
        \\    }
        \\}
    ;

    const result = try format(allocator, source, .cot, .{});
    defer if (result) |r| allocator.free(r);

    try std.testing.expectEqualStrings(expected, result.?);
}

test "format cot - already formatted" {
    const allocator = std.testing.allocator;
    const source =
        \\fn main() {
        \\    let x = 1
        \\}
    ;

    const result = try format(allocator, source, .cot, .{});
    try std.testing.expect(result == null);
}

test "format dbl - begin/end" {
    const allocator = std.testing.allocator;
    const source =
        \\proc main
        \\begin
        \\x = 1
        \\end
    ;
    const expected =
        \\proc main
        \\    begin
        \\        x = 1
        \\    end
    ;

    const result = try format(allocator, source, .dbl, .{});
    defer if (result) |r| allocator.free(r);

    try std.testing.expectEqualStrings(expected, result.?);
}
