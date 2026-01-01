//! Cot Compiler Diagnostics
//!
//! Provides structured diagnostic types for compile-time error reporting.
//! These types are used throughout the compiler pipeline (lexer, parser, IR, type checker)
//! to collect and report errors with rich context.

const std = @import("std");

/// Source location range for error highlighting
pub const SourceRange = struct {
    line: u32,
    column: u32,
    end_column: u32,

    pub const none: SourceRange = .{ .line = 0, .column = 0, .end_column = 0 };

    /// Create from AST SourceLoc
    pub fn fromLoc(line: u32, column: u32) SourceRange {
        return .{
            .line = line,
            .column = column,
            .end_column = column + 1,
        };
    }

    /// Create with span
    pub fn withSpan(line: u32, column: u32, length: u32) SourceRange {
        return .{
            .line = line,
            .column = column,
            .end_column = column + length,
        };
    }
};

/// Diagnostic severity level
pub const Level = enum {
    @"error",
    warning,
    hint,

    pub fn toString(self: Level) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .hint => "hint",
        };
    }

    pub fn color(self: Level) []const u8 {
        return switch (self) {
            .@"error" => "\x1b[31m", // Red
            .warning => "\x1b[33m", // Yellow
            .hint => "\x1b[36m", // Cyan
        };
    }
};

/// Error codes for categorization and documentation
pub const Code = enum(u16) {
    // Lexer errors (E001-E099)
    E001_invalid_character = 1,
    E002_unclosed_string = 2,
    E003_unclosed_comment = 3,
    E004_invalid_number = 4,
    E005_invalid_escape = 5,

    // Parser errors (E100-E199)
    E100_unexpected_token = 100,
    E101_expected_expression = 101,
    E102_expected_statement = 102,
    E103_expected_identifier = 103,
    E104_expected_type = 104,
    E105_unclosed_block = 105,
    E106_invalid_declaration = 106,
    E107_duplicate_definition = 107,

    // Type errors (E200-E299)
    E200_type_mismatch = 200,
    E201_undefined_variable = 201,
    E202_undefined_type = 202,
    E203_undefined_field = 203,
    E204_argument_count_mismatch = 204,
    E205_argument_type_mismatch = 205,
    E206_incompatible_operands = 206,
    E207_not_callable = 207,
    E208_array_index_type = 208,
    E209_array_bounds = 209,
    E210_return_type_mismatch = 210,
    E211_missing_return = 211,
    E212_void_value = 212,
    E213_not_assignable = 213,
    E214_readonly_assignment = 214,

    // Semantic errors (E300-E399)
    E300_undefined_label = 300,
    E301_duplicate_label = 301,
    E302_break_outside_loop = 302,
    E303_continue_outside_loop = 303,
    E304_unreachable_code = 304,

    // I/O errors (E400-E499)
    E400_channel_not_open = 400,
    E401_invalid_channel = 401,
    E402_record_mismatch = 402,

    // Warnings (W001-W099)
    W001_unused_variable = 1001,
    W002_unused_parameter = 1002,
    W003_implicit_conversion = 1003,
    W004_shadowed_variable = 1004,
    W005_unreachable_code = 1005,
    W006_empty_block = 1006,

    pub fn toString(self: Code) []const u8 {
        return @tagName(self);
    }

    pub fn number(self: Code) u16 {
        return @intFromEnum(self);
    }

    pub fn isWarning(self: Code) bool {
        return @intFromEnum(self) >= 1000;
    }
};

/// A suggestion for how to fix an error
pub const Suggestion = struct {
    message: []const u8,
    replacement: ?[]const u8 = null,
};

/// A single diagnostic message with full context
pub const Diagnostic = struct {
    level: Level,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    message: []const u8,
    source_line: ?[]const u8 = null,
    suggestions: []const Suggestion = &[_]Suggestion{},

    /// Create an error diagnostic
    pub fn err(
        code: Code,
        file_path: []const u8,
        location: SourceRange,
        message: []const u8,
    ) Diagnostic {
        return .{
            .level = .@"error",
            .code = code,
            .file_path = file_path,
            .location = location,
            .message = message,
        };
    }

    /// Create a warning diagnostic
    pub fn warn(
        code: Code,
        file_path: []const u8,
        location: SourceRange,
        message: []const u8,
    ) Diagnostic {
        return .{
            .level = .warning,
            .code = code,
            .file_path = file_path,
            .location = location,
            .message = message,
        };
    }

    /// Create a hint diagnostic
    pub fn hint(
        code: Code,
        file_path: []const u8,
        location: SourceRange,
        message: []const u8,
    ) Diagnostic {
        return .{
            .level = .hint,
            .code = code,
            .file_path = file_path,
            .location = location,
            .message = message,
        };
    }

    /// Add a suggestion to this diagnostic
    pub fn withSuggestion(self: Diagnostic, suggestion: Suggestion) Diagnostic {
        var new = self;
        new.suggestions = &[_]Suggestion{suggestion};
        return new;
    }

    /// Add source line context
    pub fn withSourceLine(self: Diagnostic, source_line: []const u8) Diagnostic {
        var new = self;
        new.source_line = source_line;
        return new;
    }
};

/// Type information for error messages
pub const TypeInfo = struct {
    name: []const u8,
    detail: ?[]const u8 = null,

    pub fn format(
        self: TypeInfo,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(self.name);
        if (self.detail) |d| {
            try writer.print("({s})", .{d});
        }
    }
};

// Common type descriptions
pub const type_alpha = TypeInfo{ .name = "alpha" };
pub const type_decimal = TypeInfo{ .name = "decimal" };
pub const type_integer = TypeInfo{ .name = "integer" };
pub const type_void = TypeInfo{ .name = "void" };

pub fn alphaType(length: u32) TypeInfo {
    var buf: [32]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "{d}", .{length}) catch "?";
    return .{ .name = "alpha", .detail = detail };
}

pub fn decimalType(length: u32, precision: u32) TypeInfo {
    var buf: [32]u8 = undefined;
    const detail = if (precision > 0)
        std.fmt.bufPrint(&buf, "{d}.{d}", .{ length, precision }) catch "?"
    else
        std.fmt.bufPrint(&buf, "{d}", .{length}) catch "?";
    return .{ .name = "decimal", .detail = detail };
}

pub fn integerType(size: u8) TypeInfo {
    var buf: [32]u8 = undefined;
    const detail = std.fmt.bufPrint(&buf, "i{d}", .{size}) catch "?";
    return .{ .name = "integer", .detail = detail };
}
