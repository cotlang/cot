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
    // Don't free file_contents here — parsed string fields (name, version, main)
    // are slices into this buffer. Stored in LoadedConfig, freed in deinit().

    const result = try parseConfig(allocator, file_contents);
    return .{ .parsed = result.parsed, .file_contents = file_contents, .file_allocator = allocator };
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
    file_contents: []const u8 = &.{},
    file_allocator: std.mem.Allocator = undefined,

    pub fn value(self: *const LoadedConfig) ProjectConfig {
        return self.parsed.value;
    }

    /// Look up a task command by name from the "tasks" object in cot.json.
    /// Re-parses the raw JSON to extract the tasks map (Deno pattern).
    pub fn getTask(self: *const LoadedConfig, allocator: std.mem.Allocator, name: []const u8) ?[]const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const tasks = root.object.get("tasks") orelse return null;
        if (tasks != .object) return null;
        const cmd = tasks.object.get(name) orelse return null;
        if (cmd != .string) return null;
        // Dupe so it outlives the parsed JSON
        return allocator.dupe(u8, cmd.string) catch null;
    }

    /// List all available task names from "tasks" object.
    pub fn listTasks(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const tasks = root.object.get("tasks") orelse return null;
        if (tasks != .object) return null;
        var names = std.ArrayListUnmanaged([]const u8){};
        var it = tasks.object.iterator();
        while (it.next()) |entry| {
            // Only include tasks whose value is a string (Deno validates at parse time)
            if (entry.value_ptr.* != .string) continue;
            names.append(allocator, allocator.dupe(u8, entry.key_ptr.*) catch continue) catch continue;
        }
        return names.toOwnedSlice(allocator) catch null;
    }

    /// Read the "libs" array from cot.json for native library linking.
    /// Returns library names (e.g., ["sqlite3"]) or null if not specified.
    pub fn getLibs(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const libs = root.object.get("libs") orelse return null;
        if (libs != .array) return null;
        var names = std.ArrayListUnmanaged([]const u8){};
        for (libs.array.items) |item| {
            if (item != .string) continue;
            names.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
        }
        if (names.items.len == 0) return null;
        return names.toOwnedSlice(allocator) catch null;
    }

    /// Read the "c_sources" array from cot.json for bundling C code.
    /// Returns file paths (e.g., ["vendor/sqlite3.c"]) or null if not specified.
    pub fn getCsources(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const sources = root.object.get("c_sources") orelse return null;
        if (sources != .array) return null;
        var paths = std.ArrayListUnmanaged([]const u8){};
        for (sources.array.items) |item| {
            if (item != .string) continue;
            paths.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
        }
        if (paths.items.len == 0) return null;
        return paths.toOwnedSlice(allocator) catch null;
    }

    /// Read the "c_flags" array from cot.json for C compilation flags.
    /// Returns flags (e.g., ["-DSQLITE_THREADSAFE=0"]) or null if not specified.
    pub fn getCflags(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const flags = root.object.get("c_flags") orelse return null;
        if (flags != .array) return null;
        var names = std.ArrayListUnmanaged([]const u8){};
        for (flags.array.items) |item| {
            if (item != .string) continue;
            names.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
        }
        if (names.items.len == 0) return null;
        return names.toOwnedSlice(allocator) catch null;
    }

    /// Read the "rpath" array from cot.json for runtime library search paths.
    /// Returns paths (e.g., ["vendor"]) or null if not specified.
    /// The compiler resolves relative paths to absolute based on the project root.
    pub fn getRpaths(self: *const LoadedConfig, allocator: std.mem.Allocator) ?[]const []const u8 {
        if (self.file_contents.len == 0) return null;
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, self.file_contents, .{}) catch return null;
        defer parsed.deinit();
        const root = parsed.value;
        if (root != .object) return null;
        const rpaths = root.object.get("rpath") orelse return null;
        if (rpaths != .array) return null;
        var names = std.ArrayListUnmanaged([]const u8){};
        for (rpaths.array.items) |item| {
            if (item != .string) continue;
            names.append(allocator, allocator.dupe(u8, item.string) catch continue) catch continue;
        }
        if (names.items.len == 0) return null;
        return names.toOwnedSlice(allocator) catch null;
    }

    pub fn deinit(self: *LoadedConfig) void {
        self.parsed.deinit();
        if (self.file_contents.len > 0) {
            self.file_allocator.free(self.file_contents);
        }
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

test "getLibs reads libs array" {
    const allocator = std.testing.allocator;
    const json =
        \\{"name":"test","libs":["sqlite3","curl"]}
    ;
    const file_contents = try allocator.dupe(u8, json);
    var loaded = try parseConfig(allocator, json);
    loaded.file_contents = file_contents;
    loaded.file_allocator = allocator;
    defer loaded.deinit();
    const libs = loaded.getLibs(allocator).?;
    defer allocator.free(libs);
    try std.testing.expectEqual(@as(usize, 2), libs.len);
    try std.testing.expectEqualStrings("sqlite3", libs[0]);
    try std.testing.expectEqualStrings("curl", libs[1]);
    allocator.free(libs[0]);
    allocator.free(libs[1]);
}

test "getLibs returns null when no libs" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    try std.testing.expect(loaded.getLibs(allocator) == null);
}

test "getCsources reads c_sources array" {
    const allocator = std.testing.allocator;
    const json =
        \\{"name":"test","c_sources":["vendor/sqlite3.c","vendor/foo.c"]}
    ;
    const file_contents = try allocator.dupe(u8, json);
    var loaded = try parseConfig(allocator, json);
    loaded.file_contents = file_contents;
    loaded.file_allocator = allocator;
    defer loaded.deinit();
    const sources = loaded.getCsources(allocator).?;
    defer allocator.free(sources);
    try std.testing.expectEqual(@as(usize, 2), sources.len);
    try std.testing.expectEqualStrings("vendor/sqlite3.c", sources[0]);
    try std.testing.expectEqualStrings("vendor/foo.c", sources[1]);
    allocator.free(sources[0]);
    allocator.free(sources[1]);
}

test "getCsources returns null when no c_sources" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    try std.testing.expect(loaded.getCsources(allocator) == null);
}

test "getCflags reads c_flags array" {
    const allocator = std.testing.allocator;
    const json =
        \\{"name":"test","c_flags":["-DSQLITE_THREADSAFE=0","-Os"]}
    ;
    const file_contents = try allocator.dupe(u8, json);
    var loaded = try parseConfig(allocator, json);
    loaded.file_contents = file_contents;
    loaded.file_allocator = allocator;
    defer loaded.deinit();
    const flags = loaded.getCflags(allocator).?;
    defer allocator.free(flags);
    try std.testing.expectEqual(@as(usize, 2), flags.len);
    try std.testing.expectEqualStrings("-DSQLITE_THREADSAFE=0", flags[0]);
    try std.testing.expectEqualStrings("-Os", flags[1]);
    allocator.free(flags[0]);
    allocator.free(flags[1]);
}

test "getCflags returns null when no c_flags" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    try std.testing.expect(loaded.getCflags(allocator) == null);
}

test "getRpaths reads rpath array" {
    const allocator = std.testing.allocator;
    const json =
        \\{"name":"test","rpath":["vendor","lib"]}
    ;
    const file_contents = try allocator.dupe(u8, json);
    var loaded = try parseConfig(allocator, json);
    loaded.file_contents = file_contents;
    loaded.file_allocator = allocator;
    defer loaded.deinit();
    const rpaths = loaded.getRpaths(allocator).?;
    defer allocator.free(rpaths);
    try std.testing.expectEqual(@as(usize, 2), rpaths.len);
    try std.testing.expectEqualStrings("vendor", rpaths[0]);
    try std.testing.expectEqualStrings("lib", rpaths[1]);
    allocator.free(rpaths[0]);
    allocator.free(rpaths[1]);
}

test "getRpaths returns null when no rpath" {
    const allocator = std.testing.allocator;
    var loaded = try parseConfig(allocator, "{}");
    defer loaded.deinit();
    try std.testing.expect(loaded.getRpaths(allocator) == null);
}
