//! Project configuration — reads cot.json manifest files.
//!
//! Reference: deno.json (Deno), package.json (Node).
//! cot.json is the single project manifest for Cot projects.

const std = @import("std");

pub const ProjectConfig = struct {
    name: ?[]const u8 = null,
    version: ?[]const u8 = null,
    main: ?[]const u8 = null,
    safe: ?bool = null,
};

/// Attempt to load cot.json from the given directory (or cwd if null).
/// Returns null if no cot.json exists. Returns error on parse failure.
pub fn loadConfig(allocator: std.mem.Allocator, dir: ?[]const u8) !?LoadedConfig {
    const path = if (dir) |d|
        try std.fmt.allocPrint(allocator, "{s}/cot.json", .{d})
    else
        try allocator.dupe(u8, "cot.json");
    defer allocator.free(path);

    const file_contents = std.fs.cwd().readFileAlloc(allocator, path, 64 * 1024) catch |e| {
        if (e == error.FileNotFound) return null;
        return e;
    };
    defer allocator.free(file_contents);

    const result = try parseConfig(allocator, file_contents);
    return result;
}

fn parseConfig(allocator: std.mem.Allocator, json_text: []const u8) !LoadedConfig {
    const parsed = std.json.parseFromSlice(ProjectConfig, allocator, json_text, .{
        .ignore_unknown_fields = true,
    }) catch {
        return error.InvalidProjectConfig;
    };
    return .{ .parsed = parsed };
}

pub const LoadedConfig = struct {
    parsed: std.json.Parsed(ProjectConfig),

    pub fn value(self: *const LoadedConfig) ProjectConfig {
        return self.parsed.value;
    }

    pub fn deinit(self: *LoadedConfig) void {
        self.parsed.deinit();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "loadConfig: parse valid cot.json" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\    "name": "myapp",
        \\    "version": "0.1.0",
        \\    "main": "src/main.cot"
        \\}
    ;
    var loaded = try parseConfig(allocator, json);
    defer loaded.deinit();
    const config = loaded.value();
    try std.testing.expectEqualStrings("myapp", config.name.?);
    try std.testing.expectEqualStrings("0.1.0", config.version.?);
    try std.testing.expectEqualStrings("src/main.cot", config.main.?);
}

test "loadConfig: ignore unknown fields" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\    "name": "test",
        \\    "future_field": true,
        \\    "main": "app.cot"
        \\}
    ;
    var loaded = try parseConfig(allocator, json);
    defer loaded.deinit();
    const config = loaded.value();
    try std.testing.expectEqualStrings("test", config.name.?);
    try std.testing.expectEqualStrings("app.cot", config.main.?);
    try std.testing.expect(config.version == null);
}

test "loadConfig: empty object" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    const config = loaded.value();
    try std.testing.expect(config.name == null);
    try std.testing.expect(config.version == null);
    try std.testing.expect(config.main == null);
}

test "loadConfig: parse safe field" {
    const allocator = std.testing.allocator;
    const json =
        \\{
        \\    "name": "myapp",
        \\    "safe": true
        \\}
    ;
    var loaded = try parseConfig(allocator, json);
    defer loaded.deinit();
    const config = loaded.value();
    try std.testing.expectEqualStrings("myapp", config.name.?);
    try std.testing.expect(config.safe.? == true);
}

test "loadConfig: safe defaults to null" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    const config = loaded.value();
    try std.testing.expect(config.safe == null);
}

test "loadConfig: invalid json returns error" {
    const allocator = std.testing.allocator;
    const result = parseConfig(allocator, "not json");
    try std.testing.expectError(error.InvalidProjectConfig, result);
}
