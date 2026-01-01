//! Diagnostic Collector
//!
//! Collects diagnostics from all compilation stages and manages source context.
//! Passed through the entire compilation pipeline to accumulate errors.

const std = @import("std");
const Allocator = std.mem.Allocator;
const diagnostics = @import("diagnostics.zig");
const Diagnostic = diagnostics.Diagnostic;
const SourceRange = diagnostics.SourceRange;
const Code = diagnostics.Code;
const Level = diagnostics.Level;
const Suggestion = diagnostics.Suggestion;

const Self = @This();

allocator: Allocator,
diagnostics_list: std.ArrayListUnmanaged(Diagnostic),
source_cache: std.StringHashMapUnmanaged([]const u8),
error_count: u32,
warning_count: u32,

/// Initialize a new diagnostic collector
pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .diagnostics_list = .{},
        .source_cache = .{},
        .error_count = 0,
        .warning_count = 0,
    };
}

/// Clean up resources
pub fn deinit(self: *Self) void {
    // Free allocated messages
    for (self.diagnostics_list.items) |diag| {
        // Note: We own the message strings allocated via addError/addWarning
        // The file_path and source_line are borrowed
        self.allocator.free(diag.message);
    }
    self.diagnostics_list.deinit(self.allocator);
    self.source_cache.deinit(self.allocator);
}

/// Cache source code for a file (for showing context in errors)
pub fn cacheSource(self: *Self, file_path: []const u8, source: []const u8) !void {
    try self.source_cache.put(self.allocator, file_path, source);
}

/// Get a specific line from cached source
pub fn getSourceLine(self: *Self, file_path: []const u8, line_num: u32) ?[]const u8 {
    const source = self.source_cache.get(file_path) orelse return null;

    var current_line: u32 = 1;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (current_line == line_num) {
            // Find end of this line
            var line_end = i;
            while (line_end < source.len and source[line_end] != '\n') {
                line_end += 1;
            }
            return source[line_start..line_end];
        }
        if (c == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }

    // Check if we're at the last line
    if (current_line == line_num and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}

/// Add an error diagnostic with formatted message
pub fn addError(
    self: *Self,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    comptime fmt: []const u8,
    args: anytype,
) void {
    self.addDiagnostic(.@"error", code, file_path, location, fmt, args);
}

/// Add a warning diagnostic with formatted message
pub fn addWarning(
    self: *Self,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    comptime fmt: []const u8,
    args: anytype,
) void {
    self.addDiagnostic(.warning, code, file_path, location, fmt, args);
}

/// Add a hint diagnostic with formatted message
pub fn addHint(
    self: *Self,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    comptime fmt: []const u8,
    args: anytype,
) void {
    self.addDiagnostic(.hint, code, file_path, location, fmt, args);
}

/// Internal: add a diagnostic of any level
fn addDiagnostic(
    self: *Self,
    level: Level,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    comptime fmt: []const u8,
    args: anytype,
) void {
    // Format the message
    const message = std.fmt.allocPrint(self.allocator, fmt, args) catch {
        // If allocation fails, we can't report this error
        return;
    };

    // Get source line context if available
    const source_line = self.getSourceLine(file_path, location.line);

    const diag = Diagnostic{
        .level = level,
        .code = code,
        .file_path = file_path,
        .location = location,
        .message = message,
        .source_line = source_line,
    };

    self.diagnostics_list.append(self.allocator, diag) catch {
        self.allocator.free(message);
        return;
    };

    // Update counts
    switch (level) {
        .@"error" => self.error_count += 1,
        .warning => self.warning_count += 1,
        .hint => {},
    }
}

/// Add an error with a suggestion
pub fn addErrorWithSuggestion(
    self: *Self,
    code: Code,
    file_path: []const u8,
    location: SourceRange,
    message: []const u8,
    suggestion: Suggestion,
) void {
    const msg = self.allocator.dupe(u8, message) catch return;
    const source_line = self.getSourceLine(file_path, location.line);

    const diag = Diagnostic{
        .level = .@"error",
        .code = code,
        .file_path = file_path,
        .location = location,
        .message = msg,
        .source_line = source_line,
        .suggestions = &[_]Suggestion{suggestion},
    };

    self.diagnostics_list.append(self.allocator, diag) catch {
        self.allocator.free(msg);
        return;
    };
    self.error_count += 1;
}

/// Check if there are any errors
pub fn hasErrors(self: *const Self) bool {
    return self.error_count > 0;
}

/// Check if there are any warnings
pub fn hasWarnings(self: *const Self) bool {
    return self.warning_count > 0;
}

/// Get total diagnostic count
pub fn count(self: *const Self) usize {
    return self.diagnostics_list.items.len;
}

/// Get all diagnostics
pub fn items(self: *const Self) []const Diagnostic {
    return self.diagnostics_list.items;
}

/// Sort diagnostics by file then line number
pub fn sort(self: *Self) void {
    std.mem.sort(Diagnostic, self.diagnostics_list.items, {}, struct {
        fn lessThan(_: void, a: Diagnostic, b: Diagnostic) bool {
            const file_cmp = std.mem.order(u8, a.file_path, b.file_path);
            if (file_cmp != .eq) return file_cmp == .lt;
            if (a.location.line != b.location.line) return a.location.line < b.location.line;
            return a.location.column < b.location.column;
        }
    }.lessThan);
}

/// Clear all diagnostics
pub fn clear(self: *Self) void {
    for (self.diagnostics_list.items) |diag| {
        self.allocator.free(diag.message);
    }
    self.diagnostics_list.clearRetainingCapacity();
    self.error_count = 0;
    self.warning_count = 0;
}

// Tests
test "diagnostic collector basic usage" {
    const allocator = std.testing.allocator;
    var collector = init(allocator);
    defer collector.deinit();

    try collector.cacheSource("test.cot", "line1\nline2\nline3");

    collector.addError(
        .E200_type_mismatch,
        "test.cot",
        .{ .line = 2, .column = 5, .end_column = 10 },
        "expected {s}, found {s}",
        .{ "decimal", "alpha" },
    );

    try std.testing.expectEqual(@as(u32, 1), collector.error_count);
    try std.testing.expect(collector.hasErrors());

    const diags = collector.items();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqualStrings("line2", diags[0].source_line.?);
}

test "source line extraction" {
    const allocator = std.testing.allocator;
    var collector = init(allocator);
    defer collector.deinit();

    try collector.cacheSource("test.cot", "first\nsecond\nthird");

    try std.testing.expectEqualStrings("first", collector.getSourceLine("test.cot", 1).?);
    try std.testing.expectEqualStrings("second", collector.getSourceLine("test.cot", 2).?);
    try std.testing.expectEqualStrings("third", collector.getSourceLine("test.cot", 3).?);
    try std.testing.expect(collector.getSourceLine("test.cot", 4) == null);
}
