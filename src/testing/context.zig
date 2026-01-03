//! Test Context
//!
//! Provides a complete test environment for integration testing:
//! - Compilation of Cot source to bytecode
//! - VM execution with stdlib loaded
//! - Temporary file management
//! - Output capture

const std = @import("std");
const cot = @import("../root.zig");
const bytecode = cot.bytecode;
const native = cot.native;

pub const TestContext = struct {
    allocator: std.mem.Allocator,
    temp_dir: ?std.fs.Dir,
    temp_path: ?[]const u8,
    captured_output: std.ArrayList(u8),

    const Self = @This();

    /// Initialize a new test context
    pub fn init(allocator: std.mem.Allocator) !Self {
        // Create a temp directory for test files
        const temp_path = try createTempDir(allocator);

        return .{
            .allocator = allocator,
            .temp_dir = std.fs.openDirAbsolute(temp_path, .{}) catch null,
            .temp_path = temp_path,
            .captured_output = std.ArrayList(u8).init(allocator),
        };
    }

    /// Clean up test context
    pub fn deinit(self: *Self) void {
        self.captured_output.deinit();

        if (self.temp_dir) |*dir| {
            dir.close();
        }

        // Clean up temp directory
        if (self.temp_path) |path| {
            std.fs.deleteTreeAbsolute(path) catch {};
            self.allocator.free(path);
        }
    }

    /// Compile source code to a bytecode module
    pub fn compile(self: *Self, source: []const u8) !bytecode.Module {
        _ = self;
        return cot.compileToModule(std.testing.allocator, source, "test");
    }

    /// Compile and execute source, returning the result value
    pub fn compileAndRun(self: *Self, source: []const u8) !bytecode.Value {
        var module = try self.compile(source);
        defer module.deinit();

        return self.runModule(&module);
    }

    /// Execute a compiled module
    pub fn runModule(self: *Self, module: *bytecode.Module) !bytecode.Value {
        _ = self;
        var vm = bytecode.VM.init(std.testing.allocator);
        defer vm.deinit();

        // Load standard library
        var stdlib = native.Stdlib.init(std.testing.allocator, &vm.native_registry);
        defer stdlib.deinit();
        try stdlib.loadAll();

        // Execute
        try vm.execute(module);

        // Return the result (top of stack or default)
        return vm.getResult() orelse bytecode.Value.nil;
    }

    /// Compile source and expect it to succeed
    pub fn expectCompiles(self: *Self, source: []const u8) !void {
        var module = self.compile(source) catch |err| {
            std.debug.print("Expected compilation to succeed, but got: {}\n", .{err});
            return error.UnexpectedCompileFailure;
        };
        module.deinit();
    }

    /// Compile source and expect it to fail
    pub fn expectCompileFails(self: *Self, source: []const u8) !void {
        var module = self.compile(source) catch {
            return; // Expected failure
        };
        module.deinit();
        return error.ExpectedCompileFailure;
    }

    /// Create a temporary file with content
    pub fn createTempFile(self: *Self, name: []const u8, content: []const u8) ![]const u8 {
        const dir = self.temp_dir orelse return error.NoTempDir;
        const file = try dir.createFile(name, .{});
        defer file.close();
        try file.writeAll(content);

        // Build full path
        const full_path = try std.fs.path.join(self.allocator, &.{ self.temp_path.?, name });
        return full_path;
    }

    /// Check if a file exists in temp directory
    pub fn fileExists(self: *Self, name: []const u8) bool {
        const dir = self.temp_dir orelse return false;
        dir.access(name, .{}) catch return false;
        return true;
    }

    /// Check if a directory exists in temp directory
    pub fn dirExists(self: *Self, name: []const u8) bool {
        const dir = self.temp_dir orelse return false;
        var sub = dir.openDir(name, .{}) catch return false;
        sub.close();
        return true;
    }

    /// Read content from a temp file
    pub fn readTempFile(self: *Self, name: []const u8) ![]const u8 {
        const dir = self.temp_dir orelse return error.NoTempDir;
        const file = try dir.openFile(name, .{});
        defer file.close();
        return try file.readToEndAlloc(self.allocator, 1024 * 1024);
    }

    fn createTempDir(allocator: std.mem.Allocator) ![]const u8 {
        const base = "/tmp";
        const timestamp = std.time.nanoTimestamp();
        const name = try std.fmt.allocPrint(allocator, "{s}/cot-test-{d}", .{ base, timestamp });
        try std.fs.makeDirAbsolute(name);
        return name;
    }
};

// ============================================================
// Tests
// ============================================================

test "TestContext: init and deinit" {
    var ctx = try TestContext.init(std.testing.allocator);
    defer ctx.deinit();

    try std.testing.expect(ctx.temp_dir != null);
    try std.testing.expect(ctx.temp_path != null);
}

test "TestContext: create temp file" {
    var ctx = try TestContext.init(std.testing.allocator);
    defer ctx.deinit();

    const path = try ctx.createTempFile("test.txt", "hello world");
    defer ctx.allocator.free(path);

    try std.testing.expect(ctx.fileExists("test.txt"));
}

test "TestContext: compile valid source" {
    var ctx = try TestContext.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.expectCompiles("fn main() {}");
}

test "TestContext: compile invalid source fails" {
    var ctx = try TestContext.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.expectCompileFails("fn main( {}"); // Missing )
}

test "TestContext: compile and run arithmetic" {
    var ctx = try TestContext.init(std.testing.allocator);
    defer ctx.deinit();

    // Note: This test depends on the VM properly returning values
    // For now just verify it compiles and runs without error
    var module = try ctx.compile(
        \\fn main() {
        \\    var x = 2 + 2
        \\}
    );
    defer module.deinit();

    _ = try ctx.runModule(&module);
}
