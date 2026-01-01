//! Diagnostic Formatter
//!
//! Formats diagnostics for terminal output with colors, source excerpts,
//! and suggestions in a style similar to Rust/Zig compilers.

const std = @import("std");
const diagnostics = @import("diagnostics.zig");
const Diagnostic = diagnostics.Diagnostic;
const Level = diagnostics.Level;
const Code = diagnostics.Code;
const DiagnosticCollector = @import("diagnostic_collector.zig");

/// ANSI color codes
const Color = struct {
    const reset = "\x1b[0m";
    const bold = "\x1b[1m";
    const red = "\x1b[31m";
    const green = "\x1b[32m";
    const yellow = "\x1b[33m";
    const blue = "\x1b[34m";
    const magenta = "\x1b[35m";
    const cyan = "\x1b[36m";
    const white = "\x1b[37m";
    const dim = "\x1b[2m";
};

/// Format options
pub const FormatOptions = struct {
    use_color: bool = true,
    show_source: bool = true,
    show_suggestions: bool = true,
    context_lines: u32 = 0, // Lines before/after error line (0 = just error line)
};

/// Format a single diagnostic to a writer
pub fn formatDiagnostic(
    diag: Diagnostic,
    options: FormatOptions,
    writer: anytype,
) !void {
    const use_color = options.use_color;

    // Header: file:line:column: level[code]: message
    if (use_color) {
        try writer.writeAll(Color.bold);
    }
    try writer.print("{s}:{d}:{d}: ", .{
        diag.file_path,
        diag.location.line,
        diag.location.column,
    });

    // Level with color
    if (use_color) {
        try writer.writeAll(switch (diag.level) {
            .@"error" => Color.red,
            .warning => Color.yellow,
            .hint => Color.cyan,
        });
    }
    try writer.print("{s}[{s}]", .{
        diag.level.toString(),
        formatCode(diag.code),
    });

    if (use_color) {
        try writer.writeAll(Color.reset);
        try writer.writeAll(Color.bold);
    }
    try writer.print(": {s}", .{diag.message});
    if (use_color) {
        try writer.writeAll(Color.reset);
    }
    try writer.writeAll("\n");

    // Source context
    if (options.show_source and diag.source_line != null) {
        const source_line = diag.source_line.?;
        const line_num = diag.location.line;

        // Line number gutter
        if (use_color) try writer.writeAll(Color.blue);
        try writer.print("{d:>5} | ", .{line_num});
        if (use_color) try writer.writeAll(Color.reset);

        // Source line content
        try writer.print("{s}\n", .{source_line});

        // Error pointer line
        if (use_color) try writer.writeAll(Color.blue);
        try writer.writeAll("      | ");
        if (use_color) try writer.writeAll(Color.reset);

        // Spaces to column position
        var col: u32 = 1;
        for (source_line) |c| {
            if (col >= diag.location.column) break;
            if (c == '\t') {
                try writer.writeAll("    "); // Tab = 4 spaces
            } else {
                try writer.writeAll(" ");
            }
            col += 1;
        }

        // Caret/underline
        if (use_color) {
            try writer.writeAll(switch (diag.level) {
                .@"error" => Color.red,
                .warning => Color.yellow,
                .hint => Color.cyan,
            });
        }

        const span_length = diag.location.end_column - diag.location.column;
        if (span_length <= 1) {
            try writer.writeAll("^");
        } else {
            var i: u32 = 0;
            while (i < span_length) : (i += 1) {
                try writer.writeAll("^");
            }
        }

        if (use_color) try writer.writeAll(Color.reset);
        try writer.writeAll("\n");
    }

    // Suggestions
    if (options.show_suggestions and diag.suggestions.len > 0) {
        for (diag.suggestions) |suggestion| {
            if (use_color) try writer.writeAll(Color.green);
            try writer.writeAll("      = help: ");
            if (use_color) try writer.writeAll(Color.reset);
            try writer.print("{s}\n", .{suggestion.message});

            if (suggestion.replacement) |replacement| {
                if (use_color) try writer.writeAll(Color.dim);
                try writer.print("              {s}\n", .{replacement});
                if (use_color) try writer.writeAll(Color.reset);
            }
        }
    }

    try writer.writeAll("\n");
}

/// Format error code for display
fn formatCode(code: Code) []const u8 {
    // Return just the code part (E200, W001, etc.)
    const tag = @tagName(code);
    // Tag format is like "E200_type_mismatch" - return "E200"
    var end: usize = 0;
    for (tag) |c| {
        if (c == '_') break;
        end += 1;
    }
    return tag[0..end];
}

/// Format all diagnostics from a collector
pub fn formatAll(
    collector: *const DiagnosticCollector,
    options: FormatOptions,
    writer: anytype,
) !void {
    for (collector.items()) |diag| {
        try formatDiagnostic(diag, options, writer);
    }

    // Summary line
    if (collector.error_count > 0 or collector.warning_count > 0) {
        try writer.writeAll("\n");
        if (options.use_color) try writer.writeAll(Color.bold);

        if (collector.error_count > 0) {
            if (options.use_color) try writer.writeAll(Color.red);
            try writer.print("error: ", .{});
            if (options.use_color) try writer.writeAll(Color.reset);
            if (options.use_color) try writer.writeAll(Color.bold);
            try writer.print("could not compile due to {d} error{s}", .{
                collector.error_count,
                if (collector.error_count == 1) "" else "s",
            });
        }

        if (collector.warning_count > 0) {
            if (collector.error_count > 0) {
                try writer.writeAll("; ");
            }
            if (options.use_color) try writer.writeAll(Color.yellow);
            try writer.print("{d} warning{s}", .{
                collector.warning_count,
                if (collector.warning_count == 1) "" else "s",
            });
        }

        if (options.use_color) try writer.writeAll(Color.reset);
        try writer.writeAll("\n");
    }
}

/// Format diagnostics to a string (for testing or capture)
pub fn formatToString(
    allocator: std.mem.Allocator,
    collector: *const DiagnosticCollector,
    options: FormatOptions,
) ![]u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    errdefer buffer.deinit();

    try formatAll(collector, options, buffer.writer());

    return buffer.toOwnedSlice();
}

/// Print diagnostics to stderr
pub fn printToStderr(
    collector: *const DiagnosticCollector,
    options: FormatOptions,
) void {
    // Zig 0.15: use std.fs.File.stderr() with buffered writer
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    formatAll(collector, options, &stderr_writer.interface) catch {};
    stderr_writer.interface.flush() catch {};
}

// Tests
test "format single error" {
    const allocator = std.testing.allocator;

    const diag = Diagnostic{
        .level = .@"error",
        .code = .E200_type_mismatch,
        .file_path = "test.cot",
        .location = .{ .line = 15, .column = 5, .end_column = 10 },
        .message = "cannot assign 'alpha(5)' to 'decimal(8,2)'",
        .source_line = "    price = \"hello\"",
    };

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    try formatDiagnostic(diag, .{ .use_color = false }, buffer.writer());

    const output = buffer.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "test.cot:15:5") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "error[E200]") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "type_mismatch") == null); // Only code shown
}

test "format code extraction" {
    try std.testing.expectEqualStrings("E200", formatCode(.E200_type_mismatch));
    try std.testing.expectEqualStrings("W001", formatCode(.W001_unused_variable));
    try std.testing.expectEqualStrings("E100", formatCode(.E100_unexpected_token));
}
