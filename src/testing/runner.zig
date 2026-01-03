//! Test Runner Module
//!
//! Provides structured test execution with output capture and result collection.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Result of running a single test
pub const TestResult = struct {
    /// Test name
    name: []const u8,
    /// Whether the test passed
    passed: bool,
    /// Execution duration in nanoseconds
    duration_ns: u64,
    /// Captured stdout output (if capture enabled)
    stdout: []const u8,
    /// Captured stderr output (if capture enabled)
    stderr: []const u8,
    /// Error message if test failed
    error_message: ?[]const u8,
    /// Error code if test failed
    error_code: ?anyerror,

    allocator: Allocator,

    pub fn deinit(self: *TestResult) void {
        self.allocator.free(self.name);
        if (self.stdout.len > 0) self.allocator.free(self.stdout);
        if (self.stderr.len > 0) self.allocator.free(self.stderr);
        if (self.error_message) |msg| self.allocator.free(msg);
    }

    /// Create a passing test result
    pub fn pass(allocator: Allocator, name: []const u8, duration_ns: u64) !TestResult {
        return .{
            .name = try allocator.dupe(u8, name),
            .passed = true,
            .duration_ns = duration_ns,
            .stdout = "",
            .stderr = "",
            .error_message = null,
            .error_code = null,
            .allocator = allocator,
        };
    }

    /// Create a failing test result
    pub fn fail(allocator: Allocator, name: []const u8, duration_ns: u64, err: anyerror, message: ?[]const u8) !TestResult {
        return .{
            .name = try allocator.dupe(u8, name),
            .passed = false,
            .duration_ns = duration_ns,
            .stdout = "",
            .stderr = "",
            .error_message = if (message) |msg| try allocator.dupe(u8, msg) else null,
            .error_code = err,
            .allocator = allocator,
        };
    }

    /// Get duration in milliseconds
    pub fn durationMs(self: *const TestResult) u64 {
        return self.duration_ns / 1_000_000;
    }
};

/// Collection of test results
pub const TestSuite = struct {
    /// Name of the test suite (usually filename)
    name: []const u8,
    /// Individual test results
    results: std.ArrayListUnmanaged(TestResult),
    /// Total execution time in nanoseconds
    total_duration_ns: u64,

    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8) !TestSuite {
        return .{
            .name = try allocator.dupe(u8, name),
            .results = .empty,
            .total_duration_ns = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TestSuite) void {
        for (self.results.items) |*result| {
            result.deinit();
        }
        self.results.deinit(self.allocator);
        self.allocator.free(self.name);
    }

    pub fn addResult(self: *TestSuite, result: TestResult) !void {
        self.total_duration_ns += result.duration_ns;
        try self.results.append(self.allocator, result);
    }

    /// Count of passed tests
    pub fn passedCount(self: *const TestSuite) usize {
        var count: usize = 0;
        for (self.results.items) |result| {
            if (result.passed) count += 1;
        }
        return count;
    }

    /// Count of failed tests
    pub fn failedCount(self: *const TestSuite) usize {
        var count: usize = 0;
        for (self.results.items) |result| {
            if (!result.passed) count += 1;
        }
        return count;
    }

    /// Total test count
    pub fn totalCount(self: *const TestSuite) usize {
        return self.results.items.len;
    }

    /// Get total duration in milliseconds
    pub fn totalDurationMs(self: *const TestSuite) u64 {
        return self.total_duration_ns / 1_000_000;
    }

    /// Check if all tests passed
    pub fn allPassed(self: *const TestSuite) bool {
        return self.failedCount() == 0;
    }
};

/// Output capture buffer for tests
pub const OutputCapture = struct {
    stdout_buffer: std.ArrayListUnmanaged(u8),
    stderr_buffer: std.ArrayListUnmanaged(u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator) OutputCapture {
        return .{
            .stdout_buffer = .empty,
            .stderr_buffer = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *OutputCapture) void {
        self.stdout_buffer.deinit(self.allocator);
        self.stderr_buffer.deinit(self.allocator);
    }

    /// Write to captured stdout
    pub fn writeStdout(self: *OutputCapture, data: []const u8) !void {
        try self.stdout_buffer.appendSlice(self.allocator, data);
    }

    /// Write to captured stderr
    pub fn writeStderr(self: *OutputCapture, data: []const u8) !void {
        try self.stderr_buffer.appendSlice(self.allocator, data);
    }

    /// Get captured stdout
    pub fn getStdout(self: *const OutputCapture) []const u8 {
        return self.stdout_buffer.items;
    }

    /// Get captured stderr
    pub fn getStderr(self: *const OutputCapture) []const u8 {
        return self.stderr_buffer.items;
    }

    /// Clear captured output
    pub fn clear(self: *OutputCapture) void {
        self.stdout_buffer.clearRetainingCapacity();
        self.stderr_buffer.clearRetainingCapacity();
    }

    /// Take ownership of captured stdout (caller must free)
    pub fn takeStdout(self: *OutputCapture) ![]const u8 {
        if (self.stdout_buffer.items.len == 0) return "";
        const result = try self.allocator.dupe(u8, self.stdout_buffer.items);
        self.stdout_buffer.clearRetainingCapacity();
        return result;
    }

    /// Take ownership of captured stderr (caller must free)
    pub fn takeStderr(self: *OutputCapture) ![]const u8 {
        if (self.stderr_buffer.items.len == 0) return "";
        const result = try self.allocator.dupe(u8, self.stderr_buffer.items);
        self.stderr_buffer.clearRetainingCapacity();
        return result;
    }
};

// ============================================================================
// Exit Codes
// ============================================================================

/// Exit codes for the test runner
pub const ExitCode = enum(u8) {
    /// All tests passed
    success = 0,
    /// One or more tests failed
    test_failed = 1,
    /// Compilation error
    compile_error = 2,
    /// Runtime error (not a test assertion)
    runtime_error = 3,
    /// Invalid arguments or configuration
    invalid_args = 4,
    /// Internal error
    internal_error = 5,

    pub fn toInt(self: ExitCode) u8 {
        return @intFromEnum(self);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TestResult: pass creation" {
    var result = try TestResult.pass(std.testing.allocator, "my_test", 1_000_000);
    defer result.deinit();

    try std.testing.expect(result.passed);
    try std.testing.expectEqualStrings("my_test", result.name);
    try std.testing.expectEqual(@as(u64, 1), result.durationMs());
}

test "TestResult: fail creation" {
    var result = try TestResult.fail(std.testing.allocator, "failing_test", 2_000_000, error.AssertionFailed, "expected true");
    defer result.deinit();

    try std.testing.expect(!result.passed);
    try std.testing.expectEqualStrings("failing_test", result.name);
    try std.testing.expectEqual(error.AssertionFailed, result.error_code.?);
    try std.testing.expectEqualStrings("expected true", result.error_message.?);
}

test "TestSuite: aggregation" {
    var suite = try TestSuite.init(std.testing.allocator, "test_file.cot");
    defer suite.deinit();

    const pass1 = try TestResult.pass(std.testing.allocator, "test1", 1_000_000);
    try suite.addResult(pass1);

    const pass2 = try TestResult.pass(std.testing.allocator, "test2", 2_000_000);
    try suite.addResult(pass2);

    const fail1 = try TestResult.fail(std.testing.allocator, "test3", 3_000_000, error.TestFailed, null);
    try suite.addResult(fail1);

    try std.testing.expectEqual(@as(usize, 3), suite.totalCount());
    try std.testing.expectEqual(@as(usize, 2), suite.passedCount());
    try std.testing.expectEqual(@as(usize, 1), suite.failedCount());
    try std.testing.expectEqual(@as(u64, 6), suite.totalDurationMs());
    try std.testing.expect(!suite.allPassed());
}

test "OutputCapture: basic capture" {
    var capture = OutputCapture.init(std.testing.allocator);
    defer capture.deinit();

    try capture.writeStdout("hello ");
    try capture.writeStdout("world");
    try capture.writeStderr("error!");

    try std.testing.expectEqualStrings("hello world", capture.getStdout());
    try std.testing.expectEqualStrings("error!", capture.getStderr());
}
