//! Diagnostic Error Accumulation
//!
//! Follows the Ghostty ErrorList pattern for accumulating multiple errors
//! during compilation instead of failing on the first error. This enables
//! better developer experience by showing all issues at once.
//!
//! Usage:
//!   var diagnostics = DiagnosticList.init(allocator);
//!   defer diagnostics.deinit();
//!
//!   // During parsing/type-checking, accumulate errors
//!   diagnostics.add(.{
//!       .message = "undefined variable 'x'",
//!       .location = .{ .line = 10, .column = 5, .file = "main.cot" },
//!       .severity = .err,
//!   }) catch {};
//!
//!   // Check if any errors occurred
//!   if (diagnostics.hasErrors()) {
//!       diagnostics.printAll();
//!       return error.CompilationFailed;
//!   }

const std = @import("std");

/// Source location for diagnostic messages
pub const SourceLocation = struct {
    line: u32 = 0,
    column: u32 = 0,
    file: []const u8 = "<unknown>",
    /// Optional: end position for range highlighting
    end_line: ?u32 = null,
    end_column: ?u32 = null,
};

/// Diagnostic severity levels
pub const Severity = enum {
    /// Informational hint (doesn't block compilation)
    hint,
    /// Warning (compilation continues)
    warning,
    /// Error (compilation will fail)
    err,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .hint => "hint",
            .warning => "warning",
            .err => "error",
        };
    }

    pub fn color(self: Severity) []const u8 {
        return switch (self) {
            .hint => "\x1b[36m", // cyan
            .warning => "\x1b[33m", // yellow
            .err => "\x1b[31m", // red
        };
    }
};

/// A single diagnostic message
pub const Diagnostic = struct {
    /// The diagnostic message
    message: []const u8,
    /// Source location where the issue occurred
    location: SourceLocation,
    /// Severity level
    severity: Severity = .err,
    /// Optional hint for how to fix the issue
    hint: ?[]const u8 = null,
    /// Optional related location (e.g., "previously defined here")
    related_location: ?SourceLocation = null,
    /// Optional related message
    related_message: ?[]const u8 = null,
    /// Error code (for documentation lookup)
    code: ?[]const u8 = null,

    /// Format the diagnostic for display
    pub fn format(
        self: Diagnostic,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        const reset = "\x1b[0m";
        const bold = "\x1b[1m";
        const dim = "\x1b[2m";

        // Main error line: file:line:column: severity: message
        try writer.print("{s}{s}:{d}:{d}:{s} {s}{s}{s}: {s}\n", .{
            bold,
            self.location.file,
            self.location.line,
            self.location.column,
            reset,
            self.severity.color(),
            self.severity.toString(),
            reset,
            self.message,
        });

        // Error code if present
        if (self.code) |code| {
            try writer.print("{s}  [{s}]{s}\n", .{ dim, code, reset });
        }

        // Hint if present
        if (self.hint) |h| {
            try writer.print("{s}  hint:{s} {s}\n", .{ "\x1b[36m", reset, h });
        }

        // Related location if present
        if (self.related_location) |rel| {
            if (self.related_message) |msg| {
                try writer.print("{s}  --> {s}:{d}:{d}: {s}{s}\n", .{
                    dim,
                    rel.file,
                    rel.line,
                    rel.column,
                    msg,
                    reset,
                });
            }
        }
    }
};

/// List of accumulated diagnostics
pub const DiagnosticList = struct {
    list: std.ArrayList(Diagnostic),
    allocator: std.mem.Allocator,
    /// Count of errors (for quick check)
    error_count: u32 = 0,
    /// Count of warnings
    warning_count: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) DiagnosticList {
        return .{
            .list = std.ArrayList(Diagnostic).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DiagnosticList) void {
        self.list.deinit();
    }

    /// Add a diagnostic to the list
    pub fn add(self: *DiagnosticList, diag: Diagnostic) !void {
        try self.list.append(diag);
        switch (diag.severity) {
            .err => self.error_count += 1,
            .warning => self.warning_count += 1,
            .hint => {},
        }
    }

    /// Add an error with just message and location
    pub fn addError(self: *DiagnosticList, message: []const u8, location: SourceLocation) !void {
        try self.add(.{
            .message = message,
            .location = location,
            .severity = .err,
        });
    }

    /// Add a warning with just message and location
    pub fn addWarning(self: *DiagnosticList, message: []const u8, location: SourceLocation) !void {
        try self.add(.{
            .message = message,
            .location = location,
            .severity = .warning,
        });
    }

    /// Check if any errors occurred
    pub fn hasErrors(self: DiagnosticList) bool {
        return self.error_count > 0;
    }

    /// Check if any diagnostics (including warnings) exist
    pub fn hasAny(self: DiagnosticList) bool {
        return self.list.items.len > 0;
    }

    /// Get all diagnostics
    pub fn items(self: DiagnosticList) []const Diagnostic {
        return self.list.items;
    }

    /// Print all diagnostics to stderr
    pub fn printAll(self: DiagnosticList) void {
        const stderr_file = std.fs.File{ .handle = std.posix.STDERR_FILENO };
        var buffer: [4096]u8 = undefined;
        const stderr = stderr_file.writer(&buffer);
        for (self.list.items) |diag| {
            diag.format("", .{}, stderr) catch {};
        }

        // Summary line
        if (self.error_count > 0 or self.warning_count > 0) {
            stderr.print("\n", .{}) catch {};
            if (self.error_count > 0) {
                stderr.print("\x1b[31merror\x1b[0m: compilation failed with {d} error(s)", .{self.error_count}) catch {};
            }
            if (self.warning_count > 0) {
                if (self.error_count > 0) {
                    stderr.print(" and ", .{}) catch {};
                }
                stderr.print("{d} warning(s)", .{self.warning_count}) catch {};
            }
            stderr.print("\n", .{}) catch {};
        }
    }

    /// Format all diagnostics to a writer
    pub fn formatAll(self: DiagnosticList, writer: anytype) !void {
        for (self.list.items) |diag| {
            try diag.format("", .{}, writer);
        }
    }

    /// Clear all diagnostics
    pub fn clear(self: *DiagnosticList) void {
        self.list.clearRetainingCapacity();
        self.error_count = 0;
        self.warning_count = 0;
    }
};

// ============================================
// Inline Tests (Ghostty pattern)
// ============================================

test "diagnostic: add and check errors" {
    const allocator = std.testing.allocator;
    var diagnostics = DiagnosticList.init(allocator);
    defer diagnostics.deinit();

    // Initially no errors
    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expect(!diagnostics.hasAny());

    // Add an error
    try diagnostics.addError("undefined variable 'x'", .{ .line = 10, .column = 5, .file = "test.cot" });

    try std.testing.expect(diagnostics.hasErrors());
    try std.testing.expect(diagnostics.hasAny());
    try std.testing.expectEqual(@as(u32, 1), diagnostics.error_count);
}

test "diagnostic: warnings don't count as errors" {
    const allocator = std.testing.allocator;
    var diagnostics = DiagnosticList.init(allocator);
    defer diagnostics.deinit();

    try diagnostics.addWarning("unused variable 'y'", .{ .line = 5, .column = 1 });

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expect(diagnostics.hasAny());
    try std.testing.expectEqual(@as(u32, 0), diagnostics.error_count);
    try std.testing.expectEqual(@as(u32, 1), diagnostics.warning_count);
}

test "diagnostic: full diagnostic with hint" {
    const allocator = std.testing.allocator;
    var diagnostics = DiagnosticList.init(allocator);
    defer diagnostics.deinit();

    try diagnostics.add(.{
        .message = "type mismatch: expected 'i32', found 'str'",
        .location = .{ .line = 15, .column = 10, .file = "main.cot" },
        .severity = .err,
        .hint = "consider using parse_int() to convert the string",
        .code = "E0308",
    });

    try std.testing.expectEqual(@as(usize, 1), diagnostics.items().len);
    try std.testing.expect(diagnostics.hasErrors());
}

test "diagnostic: clear resets counts" {
    const allocator = std.testing.allocator;
    var diagnostics = DiagnosticList.init(allocator);
    defer diagnostics.deinit();

    try diagnostics.addError("error 1", .{});
    try diagnostics.addWarning("warning 1", .{});

    diagnostics.clear();

    try std.testing.expect(!diagnostics.hasErrors());
    try std.testing.expect(!diagnostics.hasAny());
    try std.testing.expectEqual(@as(u32, 0), diagnostics.error_count);
    try std.testing.expectEqual(@as(u32, 0), diagnostics.warning_count);
}
