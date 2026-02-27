//! CLI argument parsing for `cot build`, `cot run`, `cot test`, etc.

const std = @import("std");
const build_options = @import("build_options");
const Target = @import("frontend/target.zig").Target;

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
    watch: bool = false,
    release: bool = false,
    lib: bool = false,
    direct_native: bool = false,
};

pub const RunOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
    program_args: []const []const u8 = &.{},
    watch: bool = false,
    release: bool = false,
    direct_native: bool = false,
};

pub const CheckOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
};

pub const LintOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
};

pub const TestOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
    filter: ?[]const u8 = null,
    verbose: bool = false,
    watch: bool = false,
    release: bool = false,
    fail_fast: bool = false,
};

pub const BenchOptions = struct {
    input_file: []const u8,
    target: Target = Target.native(),
    filter: ?[]const u8 = null,
    n: ?i64 = null,
};

pub const FmtOptions = struct {
    input_file: []const u8,
    check: bool = false,
    stdout: bool = false,
};

pub const InitOptions = struct {
    project_name: ?[]const u8 = null,
    lib: bool = false,
};

pub const DocOptions = struct {
    input_file: []const u8 = "",
    output: []const u8 = "docs",
};

pub const TaskOptions = struct {
    task_name: []const u8 = "",
};

pub const HelpOptions = struct {
    subcommand: ?[]const u8 = null,
};

pub const Command = union(enum) {
    build: BuildOptions,
    run: RunOptions,
    @"test": TestOptions,
    bench: BenchOptions,
    check: CheckOptions,
    lint: LintOptions,
    fmt: FmtOptions,
    doc: DocOptions,
    task: TaskOptions,
    info,
    init: InitOptions,
    lsp,
    mcp,
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
    if (std.mem.eql(u8, first, "bench")) return parseBench(&args);
    if (std.mem.eql(u8, first, "check")) return parseCheck(&args);
    if (std.mem.eql(u8, first, "lint")) return parseLint(&args);
    if (std.mem.eql(u8, first, "fmt")) return parseFmt(&args);
    if (std.mem.eql(u8, first, "doc")) return parseDoc(&args);
    if (std.mem.eql(u8, first, "task")) {
        const name = args.next() orelse {
            return .{ .task = .{} };
        };
        return .{ .task = .{ .task_name = name } };
    }
    if (std.mem.eql(u8, first, "info")) return .info;
    if (std.mem.eql(u8, first, "init")) return parseInit(&args);
    if (std.mem.eql(u8, first, "lsp")) return .lsp;
    if (std.mem.eql(u8, first, "mcp")) return .mcp;
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
        } else if (std.mem.eql(u8, arg, "--watch") or std.mem.eql(u8, arg, "-w")) {
            opts.watch = true;
        } else if (std.mem.eql(u8, arg, "--release")) {
            opts.release = true;
        } else if (std.mem.eql(u8, arg, "--lib")) {
            opts.lib = true;
        } else if (std.mem.eql(u8, arg, "--direct-native")) {
            opts.direct_native = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        opts.input_file = "";
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
        } else if (std.mem.eql(u8, arg, "--watch") or std.mem.eql(u8, arg, "-w")) {
            opts.watch = true;
        } else if (std.mem.eql(u8, arg, "--release")) {
            opts.release = true;
        } else if (std.mem.eql(u8, arg, "--direct-native")) {
            opts.direct_native = true;
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
        opts.input_file = "";
    }
    return .{ .run = opts };
}

fn parseTest(args: *std.process.ArgIterator) ?Command {
    var opts = TestOptions{ .input_file = undefined };
    var has_input = false;

    while (args.next()) |arg| {
        if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            opts.filter = arg[9..];
        } else if (std.mem.eql(u8, arg, "--filter")) {
            opts.filter = args.next() orelse {
                std.debug.print("Error: --filter requires an argument\n", .{});
                return null;
            };
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            opts.verbose = true;
        } else if (std.mem.eql(u8, arg, "--fail-fast") or std.mem.eql(u8, arg, "-x")) {
            opts.fail_fast = true;
        } else if (std.mem.eql(u8, arg, "--watch") or std.mem.eql(u8, arg, "-w")) {
            opts.watch = true;
        } else if (std.mem.eql(u8, arg, "--release")) {
            opts.release = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        opts.input_file = "";
    }
    return .{ .@"test" = opts };
}

fn parseBench(args: *std.process.ArgIterator) ?Command {
    var opts = BenchOptions{ .input_file = undefined };
    var has_input = false;

    while (args.next()) |arg| {
        if (isTargetFlag(arg)) {
            opts.target = parseTarget(arg, args) orelse return null;
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            opts.filter = arg[9..];
        } else if (std.mem.eql(u8, arg, "--filter")) {
            opts.filter = args.next() orelse {
                std.debug.print("Error: --filter requires an argument\n", .{});
                return null;
            };
        } else if (std.mem.startsWith(u8, arg, "--n=")) {
            const val_str = arg[4..];
            opts.n = std.fmt.parseInt(i64, val_str, 10) catch {
                std.debug.print("Error: --n requires a number, got '{s}'\n", .{val_str});
                return null;
            };
        } else if (std.mem.eql(u8, arg, "--n")) {
            const val_str = args.next() orelse {
                std.debug.print("Error: --n requires an argument\n", .{});
                return null;
            };
            opts.n = std.fmt.parseInt(i64, val_str, 10) catch {
                std.debug.print("Error: --n requires a number, got '{s}'\n", .{val_str});
                return null;
            };
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (!has_input) {
        opts.input_file = "";
    }
    return .{ .bench = opts };
}

fn parseCheck(args: *std.process.ArgIterator) ?Command {
    var opts = CheckOptions{ .input_file = undefined };
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
        opts.input_file = "";
    }
    return .{ .check = opts };
}

fn parseLint(args: *std.process.ArgIterator) ?Command {
    var opts = LintOptions{ .input_file = undefined };
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
        opts.input_file = "";
    }
    return .{ .lint = opts };
}

fn parseFmt(args: *std.process.ArgIterator) ?Command {
    var opts = FmtOptions{ .input_file = undefined };
    var has_input = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--check")) {
            opts.check = true;
        } else if (std.mem.eql(u8, arg, "--stdout")) {
            opts.stdout = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
            has_input = true;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }

    if (opts.check and opts.stdout) {
        std.debug.print("Error: --check and --stdout are mutually exclusive\n", .{});
        return null;
    }

    if (!has_input) {
        opts.input_file = "";
    }
    return .{ .fmt = opts };
}

fn parseDoc(args: *std.process.ArgIterator) ?Command {
    var opts = DocOptions{};
    while (args.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "-o") or std.mem.startsWith(u8, arg, "--output")) {
            if (std.mem.startsWith(u8, arg, "-o=") or std.mem.startsWith(u8, arg, "--output=")) {
                const eq_pos = std.mem.indexOf(u8, arg, "=") orelse continue;
                opts.output = arg[eq_pos + 1 ..];
            } else {
                opts.output = args.next() orelse {
                    std.debug.print("Error: -o requires an argument\n", .{});
                    return null;
                };
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.input_file = arg;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }
    return .{ .doc = opts };
}

fn parseInit(args: *std.process.ArgIterator) ?Command {
    var opts = InitOptions{};
    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "--lib")) {
            opts.lib = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            opts.project_name = arg;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return null;
        }
    }
    return .{ .init = opts };
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
        } else if (std.mem.eql(u8, sub, "bench")) {
            printBenchHelp();
        } else if (std.mem.eql(u8, sub, "check")) {
            printCheckHelp();
        } else if (std.mem.eql(u8, sub, "lint")) {
            printLintHelp();
        } else if (std.mem.eql(u8, sub, "fmt")) {
            printFmtHelp();
        } else if (std.mem.eql(u8, sub, "doc")) {
            printDocHelp();
        } else if (std.mem.eql(u8, sub, "init")) {
            printInitHelp();
        } else if (std.mem.eql(u8, sub, "task")) {
            printTaskHelp();
        } else if (std.mem.eql(u8, sub, "mcp")) {
            printMcpHelp();
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
        \\  cot bench <file.cot>            Run benchmarks
        \\  cot check <file.cot>            Type-check without compiling
        \\  cot lint <file.cot>             Check for warnings
        \\  cot fmt <file.cot>              Format source code (in-place)
        \\  cot doc <file.cot>              Generate HTML documentation
        \\  cot task <name>                 Run a task from cot.json
        \\  cot info                        Show project information
        \\  cot init [name]                 Create a new project
        \\  cot lsp                         Start language server (LSP)
        \\  cot mcp                         Start MCP server for AI tools
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
        \\Usage: cot build <file.cot> [-o name] [--target=<t>] [--watch]
        \\
        \\Compile a Cot source file to an executable or Wasm module.
        \\
        \\Flags:
        \\  -o <name>       Output name (default: input filename without .cot)
        \\  --target=<t>    Target: wasm32, wasm32-wasi, arm64-macos, amd64-linux
        \\  --watch, -w     Recompile on file changes
        \\
        \\Examples:
        \\  cot build app.cot               Produces ./app
        \\  cot build app.cot -o myapp      Produces ./myapp
        \\  cot build app.cot --watch       Recompile on save
        \\  cot build app.cot --target=wasm32  Produces ./app.wasm
        \\
    , .{});
}

fn printRunHelp() void {
    std.debug.print(
        \\Usage: cot run <file.cot> [--target=<t>] [--watch] [-- args...]
        \\
        \\Compile and run a Cot source file. The executable is placed in a
        \\temporary directory and cleaned up after execution.
        \\
        \\Flags:
        \\  --target=<t>    Target: arm64-macos, amd64-linux, wasm32 (via wasmtime)
        \\  --watch, -w     Recompile and rerun on file changes
        \\  -- args...      Arguments passed to the program
        \\
        \\Examples:
        \\  cot run app.cot                        Compile and run
        \\  cot run app.cot --target=wasm32         Run via wasmtime
        \\  cot run app.cot --watch                 Rerun on save
        \\  cot run app.cot -- hello world          Pass arguments to program
        \\
    , .{});
}

fn printTestHelp() void {
    std.debug.print(
        \\Usage: cot test <file.cot> [--target=<t>] [--filter=<str>] [--fail-fast] [--watch]
        \\
        \\Compile and run a Cot source file in test mode.
        \\
        \\Flags:
        \\  --target=<t>    Target: arm64-macos, amd64-linux, wasm32
        \\  --filter=<str>  Only run tests whose name contains <str>
        \\  --fail-fast, -x Stop after first test failure
        \\  --verbose, -v   Show detailed test output
        \\  --watch, -w     Retest on file changes
        \\
        \\Examples:
        \\  cot test app.cot                    Run all tests in app.cot
        \\  cot test app.cot --filter=math       Run only tests matching "math"
        \\  cot test app.cot --fail-fast         Stop on first failure
        \\  cot test app.cot --watch             Retest on save
        \\
    , .{});
}

fn printBenchHelp() void {
    std.debug.print(
        \\Usage: cot bench <file.cot> [--target=<t>] [--filter=<str>] [--n=<count>]
        \\
        \\Compile and run a Cot source file in benchmark mode.
        \\Uses Go-style adaptive calibration to target ~1s per benchmark.
        \\
        \\Flags:
        \\  --target=<t>    Target: arm64-macos, amd64-linux
        \\  --filter=<str>  Only run benchmarks whose name contains <str>
        \\  --n=<count>     Fixed iteration count (skip auto-calibration)
        \\
        \\Examples:
        \\  cot bench app.cot                    Run all benchmarks
        \\  cot bench app.cot --filter=fib       Run only benchmarks matching "fib"
        \\  cot bench app.cot --n=100            Run each benchmark 100 times
        \\
    , .{});
}

fn printCheckHelp() void {
    std.debug.print(
        \\Usage: cot check <file.cot> [--target=<t>]
        \\
        \\Type-check a Cot source file without compiling. Fast feedback loop
        \\for catching type errors during development.
        \\
        \\Flags:
        \\  --target=<t>    Target: wasm32, arm64-macos, amd64-linux
        \\
        \\Examples:
        \\  cot check app.cot                   Type-check app.cot
        \\  cot check app.cot --target=wasm32   Type-check for Wasm target
        \\
    , .{});
}

fn printLintHelp() void {
    std.debug.print(
        \\Usage: cot lint <file.cot> [--target=<t>]
        \\
        \\Check a Cot source file for common issues.
        \\
        \\Flags:
        \\  --target=<t>    Target: wasm32, arm64-macos, amd64-linux
        \\
        \\Rules:
        \\  W001  Unused variable        Local variable defined but never used
        \\  W002  Unused parameter        Function parameter never referenced
        \\  W003  Variable shadowing      Local shadows an outer scope variable
        \\  W004  Unreachable code        Code after return/break/continue
        \\  W005  Empty block             Empty if/while/for body
        \\
        \\Examples:
        \\  cot lint app.cot                    Lint app.cot
        \\  cot lint src/main.cot               Lint with imports
        \\
    , .{});
}

fn printTaskHelp() void {
    const help =
        "Usage: cot task <name>\n" ++
        "\n" ++
        "Run a named task defined in cot.json.\n" ++
        "\n" ++
        "Tasks are shell commands defined in the \"tasks\" object:\n" ++
        "\n" ++
        "  {\n" ++
        "    \"tasks\": {\n" ++
        "      \"dev\": \"cot run --watch src/main.cot\",\n" ++
        "      \"build\": \"cot build src/main.cot -o app\",\n" ++
        "      \"test\": \"cot test test/main.cot\"\n" ++
        "    }\n" ++
        "  }\n" ++
        "\n" ++
        "Run without a name to list available tasks:\n" ++
        "  cot task\n" ++
        "\n" ++
        "Examples:\n" ++
        "  cot task dev                     Run the \"dev\" task\n" ++
        "  cot task build                   Run the \"build\" task\n" ++
        "  cot task                         List available tasks\n";
    std.debug.print("{s}", .{help});
}

fn printInitHelp() void {
    std.debug.print(
        \\Usage: cot init [name] [--lib]
        \\
        \\Create a new Cot project with cot.json, source files, tests, and .gitignore.
        \\
        \\Arguments:
        \\  name            Project name and directory (default: current directory)
        \\
        \\Options:
        \\  --lib           Create a library project (src/lib.cot instead of src/main.cot)
        \\
        \\Examples:
        \\  cot init                        Initialize in current directory
        \\  cot init myapp                  Create myapp/ directory with project
        \\  cot init mylib --lib            Create a library project
        \\
    , .{});
}

fn printMcpHelp() void {
    std.debug.print(
        \\Usage: cot mcp
        \\
        \\Start a Model Context Protocol (MCP) server over stdio.
        \\Provides compiler-powered tools for AI assistants (Claude Code, etc.).
        \\
        \\Tools provided:
        \\  get_syntax_reference   Cot language syntax cheat sheet
        \\  get_stdlib_docs        Standard library function signatures
        \\  get_project_info       CLI commands and project structure
        \\  check_file             Parse and type-check a file, return diagnostics
        \\  list_symbols           List all declarations in a file
        \\  build                  Compile a file, return success/errors
        \\  run_tests              Run tests in a file, return results
        \\
        \\Protocol: JSON-RPC 2.0 over stdio, newline-delimited.
        \\
        \\Configuration (.mcp.json):
        \\  {{
        \\    "mcpServers": {{
        \\      "cot-tools": {{
        \\        "type": "stdio",
        \\        "command": "cot",
        \\        "args": ["mcp"]
        \\      }}
        \\    }}
        \\  }}
        \\
    , .{});
}

fn printFmtHelp() void {
    std.debug.print(
        \\Usage: cot fmt <file-or-dir> [--check] [--stdout]
        \\
        \\Format Cot source files in-place (like go fmt, zig fmt).
        \\When given a directory, recursively formats all .cot files.
        \\
        \\Flags:
        \\  --check         Check if files are formatted (exit 1 if not, for CI)
        \\  --stdout        Write formatted output to stdout (single file only)
        \\
        \\Examples:
        \\  cot fmt app.cot                 Format file in-place
        \\  cot fmt src/                    Format all .cot files in src/
        \\  cot fmt --check .               Check formatting for CI
        \\  cot fmt --stdout app.cot        Print formatted output to stdout
        \\
    , .{});
}

fn printDocHelp() void {
    std.debug.print(
        \\Usage: cot doc <file.cot> [-o dir]
        \\
        \\Generate HTML documentation from /// doc comments.
        \\
        \\Flags:
        \\  -o <dir>        Output directory (default: docs/)
        \\
        \\Examples:
        \\  cot doc src/main.cot             Generate docs in docs/
        \\  cot doc lib.cot -o api           Generate docs in api/
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
