//! CLI argument parsing for `cot build`, `cot run`, `cot test`, etc.

const std = @import("std");
const build_options = @import("build_options");
const Target = @import("core/target.zig").Target;

pub const version: []const u8 = build_options.version;

/// "0.3.1" → "0.3" (major.minor only, for help banner)
pub const version_short: []const u8 = blk: {
    // Find second dot
    var dots: usize = 0;
    for (version, 0..) |c, i| {
        if (c == '.') {
            dots += 1;
            if (dots == 2) break :blk version[0..i];
        }
    }
    break :blk version; // no second dot, use full version
};

pub const BuildOptions = struct {
    input_file: []const u8,
    output_name: ?[]const u8 = null,
    target: Target = Target.native(),
};

pub const RunOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
    program_args: []const []const u8 = &.{},
};

pub const TestOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
};

pub const HelpOptions = struct {
    subcommand: ?[]const u8 = null,
};

pub const Command = union(enum) {
    build: BuildOptions,
    run: RunOptions,
    @"test": TestOptions,
    lsp,
    version,
    help: HelpOptions,
};

pub fn parseArgs(allocator: std.mem.Allocator) ?Command {
    var args = std.process.args();
    _ = args.skip(); // skip executable name

    const first = args.next() orelse return .{ .help = .{} };

    // Known subcommands
    if (std.mem.eql(u8, first, "build")) return parseBuild(&args);
    if (std.mem.eql(u8, first, "run")) return parseRun(allocator, &args);
    if (std.mem.eql(u8, first, "test")) return parseTest(&args);
    if (std.mem.eql(u8, first, "lsp")) return .lsp;
    if (std.mem.eql(u8, first, "version")) return .version;
    if (std.mem.eql(u8, first, "help")) {
        const sub = args.next();
        return .{ .help = .{ .subcommand = sub } };
    }

    // Implicit build: `cot file.cot` or `cot --target=wasm32 file.cot`
    if (std.mem.endsWith(u8, first, ".cot")) {
        return parseImplicitBuild(first, &args, null);
    }

    // Could be flags before file: `cot --target=wasm32 file.cot`
    if (std.mem.startsWith(u8, first, "-")) {
        return parseImplicitBuild(null, &args, first);
    }

    std.debug.print("Error: Unknown command '{s}'\n\n", .{first});
    printUsage();
    return null;
}

fn parseBuild(args: *std.process.ArgIterator) ?Command {
    var opts = BuildOptions{ .input_file = undefined };
    var has_input = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-o")) {
            opts.output_name = args.next() orelse {
                std.debug.print("Error: -o requires an argument\n", .{});
                return null;
            };
        } else if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        std.debug.print("Error: No input file\nUsage: cot build <file.cot> [-o name] [--target=<t>]\n", .{});
        return null;
    }
    return .{ .build = opts };
}

fn parseRun(allocator: std.mem.Allocator, args: *std.process.ArgIterator) ?Command {
    var opts = RunOptions{ .input_file = undefined };
    var has_input = false;
    var program_args = std.ArrayListUnmanaged([]const u8){};

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--")) {
            // Everything after -- is passed to the program
            while (args.next()) |parg| {
                program_args.append(allocator, parg) catch return null;
            }
            break;
        } else if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    opts.program_args = program_args.items;

    if (!has_input) {
        std.debug.print("Error: No input file\nUsage: cot run <file.cot> [--target=<t>] [-- args...]\n", .{});
        return null;
    }
    return .{ .run = opts };
}

fn parseTest(args: *std.process.ArgIterator) ?Command {
    var opts = TestOptions{ .input_file = undefined };
    var has_input = false;

    while (args.next()) |arg| {
        if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        std.debug.print("Error: No input file\nUsage: cot test <file.cot> [--target=<t>]\n", .{});
        return null;
    }
    return .{ .@"test" = opts };
}

fn parseImplicitBuild(maybe_file: ?[]const u8, args: *std.process.ArgIterator, first_flag: ?[]const u8) ?Command {
    var opts = BuildOptions{ .input_file = undefined };
    var has_input = false;

    if (maybe_file) |f| {
        opts.input_file = f;
        has_input = true;
    }

    // Process the pre-consumed flag if any
    if (first_flag) |flag| {
        if (isTargetFlag(flag)) {
            opts.target = parseTarget(flag, args) orelse return null;
        } else if (std.mem.eql(u8, flag, "-o")) {
            opts.output_name = args.next() orelse {
                std.debug.print("Error: -o requires an argument\n", .{});
                return null;
            };
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{flag});
            return null;
        }
    }

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-o")) {
            opts.output_name = args.next() orelse {
                std.debug.print("Error: -o requires an argument\n", .{});
                return null;
            };
        } else if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        std.debug.print("Error: No input file\n", .{});
        printUsage();
        return null;
    }
    return .{ .build = opts };
}

/// Parse --target=<t> or --target <t>. Caller must check isTargetFlag first.
/// Returns null on error (invalid value or missing argument).
fn parseTarget(arg: []const u8, args: *std.process.ArgIterator) ?Target {
    if (std.mem.startsWith(u8, arg, "--target=")) {
        const val = arg[9..];
        return Target.parse(val) orelse {
            std.debug.print("Error: Unknown target '{s}'. Use: wasm32, wasm32-wasi, arm64-macos, amd64-linux\n", .{val});
            return null;
        };
    }
    if (std.mem.eql(u8, arg, "--target")) {
        const val = args.next() orelse {
            std.debug.print("Error: --target requires an argument\n", .{});
            return null;
        };
        return Target.parse(val) orelse {
            std.debug.print("Error: Unknown target '{s}'. Use: wasm32, wasm32-wasi, arm64-macos, amd64-linux\n", .{val});
            return null;
        };
    }
    return null;
}

/// Returns true if the arg looks like a target flag (even if invalid).
fn isTargetFlag(arg: []const u8) bool {
    if (std.mem.startsWith(u8, arg, "--target")) return true;
    return false;
}

/// Derive output name from input file path.
/// Strip directory path, strip .cot extension, append .wasm for wasm targets.
pub fn deriveOutputName(allocator: std.mem.Allocator, input_file: []const u8, target: Target) ![]const u8 {
    // Get basename
    const basename = std.fs.path.basename(input_file);

    // Strip .cot extension
    const stem = if (std.mem.endsWith(u8, basename, ".cot"))
        basename[0 .. basename.len - 4]
    else
        basename;

    if (target.isWasm()) {
        return std.fmt.allocPrint(allocator, "{s}.wasm", .{stem});
    }
    return allocator.dupe(u8, stem);
}

pub fn printVersion() void {
    const arch = Target.native().arch.name();
    const os = Target.native().os.name();
    std.debug.print("cot {s} ({s}-{s})\n", .{ version, arch, os });
}

pub fn printHelp(subcommand: ?[]const u8) void {
    if (subcommand) |sub| {
        if (std.mem.eql(u8, sub, "build")) {
            printBuildHelp();
        } else if (std.mem.eql(u8, sub, "run")) {
            printRunHelp();
        } else if (std.mem.eql(u8, sub, "test")) {
            printTestHelp();
        } else {
            std.debug.print("Unknown command: {s}\n\n", .{sub});
            printUsage();
        }
        return;
    }
    printUsage();
}

fn printUsage() void {
    std.debug.print("Cot {s} — Write like TypeScript, run like Rust\n" ++
        \\
        \\Usage:
        \\  cot <file.cot>                  Compile to native executable
        \\  cot build <file.cot> [-o name]  Compile with options
        \\  cot run <file.cot> [-- args]    Compile and run
        \\  cot test <file.cot>             Run tests
        \\  cot lsp                         Start language server (LSP)
        \\  cot version                     Print version
        \\  cot help [command]              Print help
        \\
        \\Flags:
        \\  -o <name>       Output name (default: input filename without .cot)
        \\  --target=<t>    Target: native, wasm32, wasm32-wasi, arm64-macos, amd64-linux
        \\
    , .{version_short});
}

fn printBuildHelp() void {
    std.debug.print(
        \\Usage: cot build <file.cot> [-o name] [--target=<t>]
        \\
        \\Compile a Cot source file to an executable or Wasm module.
        \\
        \\Flags:
        \\  -o <name>       Output name (default: input filename without .cot)
        \\  --target=<t>    Target: wasm32, wasm32-wasi, arm64-macos, amd64-linux
        \\
        \\Examples:
        \\  cot build app.cot               Produces ./app
        \\  cot build app.cot -o myapp      Produces ./myapp
        \\  cot build app.cot --target=wasm32  Produces ./app.wasm
        \\  cot build app.cot --target=wasm32-wasi  Produces ./app.wasm (with WASI imports)
        \\
    , .{});
}

fn printRunHelp() void {
    std.debug.print(
        \\Usage: cot run <file.cot> [--target=<t>] [-- args...]
        \\
        \\Compile and run a Cot source file. The executable is placed in a
        \\temporary directory and cleaned up after execution.
        \\
        \\Flags:
        \\  --target=<t>    Target: arm64-macos, amd64-linux (wasm32 not supported)
        \\  -- args...      Arguments passed to the program
        \\
        \\Examples:
        \\  cot run app.cot                 Compile and run
        \\  cot run app.cot -- hello world  Pass arguments to program
        \\
    , .{});
}

fn printTestHelp() void {
    std.debug.print(
        \\Usage: cot test <file.cot> [--target=<t>]
        \\
        \\Compile and run a Cot source file in test mode.
        \\
        \\Flags:
        \\  --target=<t>    Target: arm64-macos, amd64-linux
        \\
        \\Examples:
        \\  cot test app.cot                Run tests in app.cot
        \\
    , .{});
}

// ============================================================================
// Tests
// ============================================================================

test "deriveOutputName: native" {
    const allocator = std.testing.allocator;
    const name = try deriveOutputName(allocator, "app.cot", Target.native());
    defer allocator.free(name);
    try std.testing.expectEqualStrings("app", name);
}

test "deriveOutputName: native with path" {
    const allocator = std.testing.allocator;
    const name = try deriveOutputName(allocator, "src/main.cot", Target.native());
    defer allocator.free(name);
    try std.testing.expectEqualStrings("main", name);
}

test "deriveOutputName: wasm" {
    const allocator = std.testing.allocator;
    const name = try deriveOutputName(allocator, "app.cot", Target.wasm32);
    defer allocator.free(name);
    try std.testing.expectEqualStrings("app.wasm", name);
}

test "deriveOutputName: no extension" {
    const allocator = std.testing.allocator;
    const name = try deriveOutputName(allocator, "app", Target.native());
    defer allocator.free(name);
    try std.testing.expectEqualStrings("app", name);
}
