//! Cot Testing Utilities
//!
//! Provides test infrastructure for the Cot compiler and runtime:
//! - TestContext: Full compile-and-run testing with temp file management
//! - Custom assertions for compile errors, runtime errors, output verification
//! - Fixture loading from testdata/ directory
//!
//! Usage:
//! ```zig
//! const testing_utils = @import("testing");
//!
//! test "integration: basic arithmetic" {
//!     var ctx = try testing_utils.TestContext.init(std.testing.allocator);
//!     defer ctx.deinit();
//!
//!     const result = try ctx.compileAndRun("fn main() i64 { return 2 + 2 }");
//!     try std.testing.expectEqual(@as(i64, 4), result.asInt());
//! }
//! ```

const std = @import("std");

// Re-export all testing utilities
pub const TestContext = @import("context.zig").TestContext;
pub const assertions = @import("assertions.zig");
pub const fixtures = @import("fixtures.zig");
pub const runner = @import("runner.zig");

// Runner types
pub const TestResult = runner.TestResult;
pub const TestSuite = runner.TestSuite;
pub const OutputCapture = runner.OutputCapture;
pub const ExitCode = runner.ExitCode;

// Convenience re-exports from assertions
pub const expectCompileError = assertions.expectCompileError;
pub const expectRuntimeError = assertions.expectRuntimeError;
pub const expectOutput = assertions.expectOutput;
pub const expectParseError = assertions.expectParseError;

// Convenience re-exports from fixtures
pub const loadFixture = fixtures.loadFixture;
pub const FixtureType = fixtures.FixtureType;

/// Common test patterns
pub const patterns = struct {
    /// Create a minimal valid Cot program with an expression
    pub fn wrapExpr(expr: []const u8) []const u8 {
        return std.fmt.comptimePrint("fn main() {{ return {s} }}", .{expr});
    }

    /// Create a program with a main function body
    pub fn wrapBody(body: []const u8) []const u8 {
        return std.fmt.comptimePrint("fn main() {{ {s} }}", .{body});
    }
};

test {
    // Reference all test declarations
    std.testing.refAllDecls(@This());
    // Pull in integration tests
    _ = @import("integration_test.zig");
    // Pull in error recovery tests
    _ = @import("error_recovery_test.zig");
    // Pull in memory management tests
    _ = @import("memory_test.zig");
    // Pull in edge case tests
    _ = @import("edge_case_test.zig");
    // Pull in performance tests
    _ = @import("performance_test.zig");
    // Pull in stress tests
    _ = @import("stress_test.zig");
    // Pull in regression tests
    _ = @import("regression_test.zig");
}
