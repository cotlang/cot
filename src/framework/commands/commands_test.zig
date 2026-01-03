//! Framework Command Tests
//!
//! Comprehensive tests for CLI commands: init, new, build, run, etc.

const std = @import("std");
const testing = std.testing;

const init_cmd = @import("init.zig");
const new_cmd = @import("new.zig");
const build_cmd = @import("build.zig");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");

// ============================================================================
// Init Command Tests
// ============================================================================

test "init: parseArgs with no arguments" {
    const args: []const []const u8 = &.{};
    const options = init_cmd.parseArgs(args);

    try testing.expect(options.name == null);
    try testing.expect(!options.force);
}

test "init: parseArgs with name" {
    const args = &[_][]const u8{"myproject"};
    const options = init_cmd.parseArgs(args);

    try testing.expectEqualStrings("myproject", options.name.?);
}

test "init: parseArgs with --name flag" {
    const args = &[_][]const u8{ "--name", "myproject" };
    const options = init_cmd.parseArgs(args);

    try testing.expectEqualStrings("myproject", options.name.?);
}

test "init: parseArgs with -n flag" {
    const args = &[_][]const u8{ "-n", "myproject" };
    const options = init_cmd.parseArgs(args);

    try testing.expectEqualStrings("myproject", options.name.?);
}

test "init: parseArgs with --force flag" {
    const args = &[_][]const u8{"--force"};
    const options = init_cmd.parseArgs(args);

    try testing.expect(options.force);
}

test "init: parseArgs with -f flag" {
    const args = &[_][]const u8{"-f"};
    const options = init_cmd.parseArgs(args);

    try testing.expect(options.force);
}

test "init: parseArgs with name and force" {
    const args = &[_][]const u8{ "myproject", "--force" };
    const options = init_cmd.parseArgs(args);

    try testing.expectEqualStrings("myproject", options.name.?);
    try testing.expect(options.force);
}

// ============================================================================
// New Command Tests
// ============================================================================

test "new: parseArgs with no arguments" {
    const args: []const []const u8 = &.{};
    const options = new_cmd.parseArgs(args);

    try testing.expect(options.name == null);
    try testing.expectEqual(new_cmd.Template.app, options.template);
    try testing.expectEqual(new_cmd.Syntax.cot, options.syntax);
}

test "new: parseArgs with name only" {
    const args = &[_][]const u8{"myapp"};
    const options = new_cmd.parseArgs(args);

    try testing.expectEqualStrings("myapp", options.name.?);
}

test "new: parseArgs with --template app" {
    const args = &[_][]const u8{ "myapp", "--template", "app" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqualStrings("myapp", options.name.?);
    try testing.expectEqual(new_cmd.Template.app, options.template);
}

test "new: parseArgs with --template library" {
    const args = &[_][]const u8{ "mylib", "--template", "library" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqualStrings("mylib", options.name.?);
    try testing.expectEqual(new_cmd.Template.library, options.template);
}

test "new: parseArgs with -t shorthand" {
    const args = &[_][]const u8{ "myapp", "-t", "app" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Template.app, options.template);
}

test "new: parseArgs with --dbl flag" {
    const args = &[_][]const u8{ "myapp", "--dbl" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Syntax.dbl, options.syntax);
}

test "new: parseArgs default syntax is cot" {
    const args = &[_][]const u8{"myapp"};
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Syntax.cot, options.syntax);
}

test "new: parseArgs with all options" {
    const args = &[_][]const u8{ "myapp", "--template", "library", "--dbl" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqualStrings("myapp", options.name.?);
    try testing.expectEqual(new_cmd.Template.library, options.template);
    try testing.expectEqual(new_cmd.Syntax.dbl, options.syntax);
}

test "new: parseArgs with --app shorthand" {
    const args = &[_][]const u8{ "myapp", "--app" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Template.app, options.template);
}

test "new: parseArgs with --library shorthand" {
    const args = &[_][]const u8{ "mylib", "--library" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Template.library, options.template);
}

test "new: parseArgs with --lib shorthand" {
    const args = &[_][]const u8{ "mylib", "--lib" };
    const options = new_cmd.parseArgs(args);

    try testing.expectEqual(new_cmd.Template.library, options.template);
}

// ============================================================================
// Config Tests
// ============================================================================

test "config: parse minimal app config" {
    const allocator = testing.allocator;
    const json =
        \\{
        \\    "name": "myapp",
        \\    "type": "app"
        \\}
    ;

    var loader = config.ConfigLoader.init(allocator);
    const cfg = try loader.parseConfig(json);

    try testing.expectEqualStrings("myapp", cfg.name);
    try testing.expectEqual(config.ProjectType.app, cfg.project_type);
}

test "config: parse app with version" {
    const allocator = testing.allocator;
    const json =
        \\{
        \\    "name": "myapp",
        \\    "type": "app",
        \\    "version": "1.0.0"
        \\}
    ;

    var loader = config.ConfigLoader.init(allocator);
    const cfg = try loader.parseConfig(json);

    try testing.expectEqualStrings("myapp", cfg.name);
    try testing.expectEqualStrings("1.0.0", cfg.version.?);
}

test "config: parse library config" {
    const allocator = testing.allocator;
    const json =
        \\{
        \\    "name": "@company/common",
        \\    "type": "library"
        \\}
    ;

    var loader = config.ConfigLoader.init(allocator);
    const cfg = try loader.parseConfig(json);

    try testing.expectEqualStrings("@company/common", cfg.name);
    try testing.expectEqual(config.ProjectType.library, cfg.project_type);
}

test "config: parse workspace config" {
    const allocator = testing.allocator;
    const json =
        \\{
        \\    "name": "myworkspace",
        \\    "type": "workspace"
        \\}
    ;

    var loader = config.ConfigLoader.init(allocator);
    const cfg = try loader.parseConfig(json);

    try testing.expectEqual(config.ProjectType.workspace, cfg.project_type);
}
