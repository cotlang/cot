//! Test Fixtures
//!
//! Utilities for loading test data from the testdata/ directory.

const std = @import("std");

/// Types of test fixtures
pub const FixtureType = enum {
    /// Valid Cot programs that should compile and run
    cot_valid,
    /// Invalid Cot programs that should fail to compile (syntax errors)
    cot_syntax_error,
    /// Invalid Cot programs that should fail type checking
    cot_type_error,
    /// Cot programs for runtime testing
    cot_runtime,
    /// Valid DBL programs
    dbl_valid,
    /// Invalid DBL programs
    dbl_invalid,
    /// Pre-compiled bytecode modules
    bytecode,
    /// Database schema fixtures
    schema,
    /// Golden output files for snapshot testing
    golden,

    pub fn path(self: FixtureType) []const u8 {
        return switch (self) {
            .cot_valid => "testdata/cot/valid",
            .cot_syntax_error => "testdata/cot/invalid/syntax_errors",
            .cot_type_error => "testdata/cot/invalid/type_errors",
            .cot_runtime => "testdata/cot/runtime",
            .dbl_valid => "testdata/dbl/valid",
            .dbl_invalid => "testdata/dbl/invalid",
            .bytecode => "testdata/bytecode",
            .schema => "testdata/schema",
            .golden => "testdata/golden",
        };
    }
};

/// Load a fixture file by name and type
pub fn loadFixture(allocator: std.mem.Allocator, fixture_type: FixtureType, name: []const u8) ![]const u8 {
    const base_path = fixture_type.path();
    const full_path = try std.fs.path.join(allocator, &.{ base_path, name });
    defer allocator.free(full_path);

    const file = std.fs.cwd().openFile(full_path, .{}) catch |err| {
        std.debug.print("Failed to open fixture: {s}/{s}: {}\n", .{ base_path, name, err });
        return error.FixtureNotFound;
    };
    defer file.close();

    return try file.readToEndAlloc(allocator, 1024 * 1024);
}

/// Load all fixtures of a given type
pub fn loadAllFixtures(allocator: std.mem.Allocator, fixture_type: FixtureType) ![]const Fixture {
    const base_path = fixture_type.path();

    var dir = std.fs.cwd().openDir(base_path, .{ .iterate = true }) catch {
        return &[_]Fixture{};
    };
    defer dir.close();

    var fixtures: std.ArrayListUnmanaged(Fixture) = .empty;
    errdefer {
        for (fixtures.items) |*f| f.deinit(allocator);
        fixtures.deinit(allocator);
    }

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;

        const content = try loadFixture(allocator, fixture_type, entry.name);
        const name = try allocator.dupe(u8, entry.name);

        try fixtures.append(allocator, .{
            .name = name,
            .content = content,
        });
    }

    return fixtures.toOwnedSlice(allocator);
}

/// A loaded fixture
pub const Fixture = struct {
    name: []const u8,
    content: []const u8,

    pub fn deinit(self: *Fixture, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        allocator.free(self.content);
    }
};

/// Common test programs as inline fixtures
pub const inline_fixtures = struct {
    /// Minimal valid program
    pub const empty_main =
        \\fn main() {}
    ;

    /// Simple arithmetic
    pub const arithmetic =
        \\fn main() {
        \\    var x = 2 + 3 * 4
        \\    var y = x - 1
        \\}
    ;

    /// Function with parameters
    pub const function_params =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() {
        \\    var result = add(10, 20)
        \\}
    ;

    /// Recursive function
    pub const recursive =
        \\fn factorial(n: i64) i64 {
        \\    if n <= 1 {
        \\        return 1
        \\    }
        \\    return n * factorial(n - 1)
        \\}
        \\fn main() {
        \\    var result = factorial(5)
        \\}
    ;

    /// Control flow
    pub const control_flow =
        \\fn main() {
        \\    var x = 0
        \\    if x == 0 {
        \\        x = 1
        \\    } else {
        \\        x = 2
        \\    }
        \\}
    ;

    /// While loop
    pub const while_loop =
        \\fn main() {
        \\    var i = 0
        \\    while i < 10 {
        \\        i = i + 1
        \\    }
        \\}
    ;

    /// Struct definition and use
    pub const struct_basic =
        \\struct Point {
        \\    x: i64,
        \\    y: i64,
        \\}
        \\fn main() {
        \\    var p = Point { x: 10, y: 20 }
        \\}
    ;

    /// Test definition
    pub const test_basic =
        \\test "basic test" {
        \\    assert(1)
        \\}
        \\fn main() {}
    ;
};

/// Invalid programs for error testing
pub const invalid_fixtures = struct {
    /// Missing closing brace
    pub const missing_brace =
        \\fn main() {
        \\    var x = 1
    ;

    /// Missing closing paren
    pub const missing_paren =
        \\fn main( {}
    ;

    /// Invalid token
    pub const invalid_token =
        \\fn main() {
        \\    var x = @#$
        \\}
    ;

    /// Undefined variable
    pub const undefined_var =
        \\fn main() {
        \\    var x = undefined_variable
        \\}
    ;

    /// Type mismatch
    pub const type_mismatch =
        \\fn foo() i64 {
        \\    return "not an int"
        \\}
        \\fn main() {}
    ;
};

// ============================================================
// Tests
// ============================================================

test "fixtures: inline fixtures compile" {
    const cot = @import("../root.zig");

    // Test that all inline fixtures compile successfully
    const fixtures_to_test = [_][]const u8{
        inline_fixtures.empty_main,
        inline_fixtures.arithmetic,
        inline_fixtures.function_params,
        inline_fixtures.control_flow,
        inline_fixtures.while_loop,
    };

    for (fixtures_to_test) |source| {
        var module = cot.compileToModule(std.testing.allocator, source, "test") catch |err| {
            std.debug.print("Failed to compile fixture: {}\n", .{err});
            return err;
        };
        module.deinit();
    }
}
