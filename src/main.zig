//! Cot - A modern programming language for business applications
//! Business apps with ISAM database and project framework
//!
//! Usage:
//!   cot <file.cot>              Run a Cot program (interpreter)
//!   cot run <file.cot|.cbo>     Run a program (auto-detect mode)
//!   cot trace <file.cot|.cbo>   Run with execution tracing enabled
//!   cot compile <file.cot>      Compile to bytecode (.cbo) via IR
//!   cot fmt <files...>          Format source files
//!   cot disasm <file.cot|.cbo>  Disassemble to readable output
//!   cot dump-ir <file.cot>      Dump IR for debugging
//!   cot repl                    Start interactive REPL
//!   cot init                    Initialize a new workspace
//!   cot new <name>              Create a new app or package
//!   cot --help                  Show help
//!   cot --version               Show version
//!
//! For DBL syntax files (.dbl), use the cot-dbl frontend instead.
//!
//! Logging (Ghostty pattern):
//!   Set COT_LOG environment variable to control log output.
//!   Examples:
//!     COT_LOG=debug         Show all debug messages
//!     COT_LOG=info          Show info and above (default)
//!     COT_LOG=warn          Show warnings and errors only
//!
//!   Scoped logs available: ir-lower, vm-exec, bytecode-emit, parser

const std = @import("std");

// Configure std.log for scoped logging (Ghostty pattern)
// Control via COT_LOG environment variable at runtime
pub const std_options: std.Options = .{
    // Allow all log levels at compile time, filter at runtime
    .log_level = .debug,

    // Custom log function that respects scopes and COT_LOG env var
    .logFn = cotLogFn,
};

/// Cached log level from environment (initialized on first log call)
var cached_log_level: ?std.log.Level = null;

fn getRuntimeLogLevel() std.log.Level {
    if (cached_log_level) |level| return level;

    const env_val = std.posix.getenv("COT_LOG") orelse {
        cached_log_level = .info;
        return .info;
    };

    const level: std.log.Level = if (std.mem.eql(u8, env_val, "debug"))
        .debug
    else if (std.mem.eql(u8, env_val, "info"))
        .info
    else if (std.mem.eql(u8, env_val, "warn"))
        .warn
    else if (std.mem.eql(u8, env_val, "err"))
        .err
    else
        .info;

    cached_log_level = level;
    return level;
}

fn cotLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    // Runtime log level filtering
    const runtime_level = getRuntimeLogLevel();
    if (@intFromEnum(level) > @intFromEnum(runtime_level)) return;

    // Format: [scope] message
    const scope_name = @tagName(scope);
    const prefix = comptime if (scope == .default) "" else "[" ++ scope_name ++ "] ";

    const level_txt = comptime switch (level) {
        .err => "\x1b[31merror\x1b[0m: ",
        .warn => "\x1b[33mwarn\x1b[0m: ",
        .info => "",
        .debug => "\x1b[36mdebug\x1b[0m: ",
    };

    std.debug.print(level_txt ++ prefix ++ format ++ "\n", args);
}
const cot = @import("cot");
const build_options = @import("build_options");
const frontends = @import("frontends.zig");
const runtime_selector = @import("runtime_selector.zig");
const trace_mod = @import("cot_runtime").trace;
const dbl_ext = @import("cot_runtime").dbl_ext;
const debug_tools = @import("cot_runtime").debug_tools;

// Framework commands (imported through cot module to avoid module conflicts)
const init_cmd = cot.framework.commands.init;
const new_cmd = cot.framework.commands.new;
const build_cmd = cot.framework.commands.build;
const run_cmd = cot.framework.commands.run;
const dev_cmd = cot.framework.commands.dev;
const schema_cmd = cot.framework.commands.schema;
const data_cmd = cot.framework.commands.data;
const convert_cmd = cot.framework.commands.convert;
const gen_cmd = cot.framework.commands.gen;
const deps_cmd = cot.framework.commands.deps;

// ============================================================
// Command Dispatch System
// ============================================================

/// Command handler function type
const CommandFn = *const fn (std.mem.Allocator, []const []const u8) anyerror!void;

/// Command definition
const Command = struct {
    name: []const u8,
    aliases: []const []const u8 = &.{},
    handler: CommandFn,
    description: []const u8,
    usage: []const u8 = "",
    category: Category = .core,

    const Category = enum { core, workspace, debug };
};

/// All available commands
const commands = [_]Command{
    // Core commands
    .{ .name = "run", .handler = cmdRun, .description = "Run a program or workspace project", .usage = "<file|project> [--runtime=zig|rs]", .category = .core },
    .{ .name = "compile", .handler = cmdCompile, .description = "Compile to bytecode (.cbo)", .usage = "<file.cot> [-o output.cbo]", .category = .core },
    .{ .name = "fmt", .aliases = &.{"format"}, .handler = cmdFmt, .description = "Format source files", .usage = "<files...> [--check]", .category = .core },
    .{ .name = "test", .handler = cmdTest, .description = "Run tests in a file", .usage = "<file.cot> [--filter=<name>]", .category = .core },
    .{ .name = "repl", .handler = cmdRepl, .description = "Start interactive REPL", .category = .core },

    // Debug commands
    .{ .name = "trace", .handler = cmdTrace, .description = "Run with execution tracing", .usage = "<file> [--level=opcodes|verbose|slots]", .category = .debug },
    .{ .name = "debug", .handler = cmdDebug, .description = "Interactive debugger", .usage = "<file>", .category = .debug },
    .{ .name = "validate", .handler = cmdValidate, .description = "Validate bytecode integrity", .usage = "<file.cbo> [--strict]", .category = .debug },
    .{ .name = "disasm", .handler = cmdDisasm, .description = "Disassemble bytecode", .usage = "<file.cot|.cbo>", .category = .debug },
    .{ .name = "dump-ir", .handler = cmdDumpIR, .description = "Dump IR for debugging", .usage = "<file.cot>", .category = .debug },

    // Workspace commands
    .{ .name = "init", .handler = cmdInit, .description = "Initialize a new workspace", .usage = "[name]", .category = .workspace },
    .{ .name = "new", .handler = cmdNew, .description = "Create a new app or package", .usage = "<name>", .category = .workspace },
    .{ .name = "build", .handler = cmdBuild, .description = "Build workspace projects", .usage = "[project]", .category = .workspace },
    .{ .name = "dev", .handler = cmdDev, .description = "Start development server", .category = .workspace },
    .{ .name = "schema", .handler = cmdSchema, .description = "Manage database schema", .usage = "<subcommand>", .category = .workspace },
    .{ .name = "data", .handler = cmdData, .description = "Load/unload fixed-width data", .usage = "<subcommand>", .category = .workspace },
    .{ .name = "convert", .handler = cmdConvert, .description = "Convert DBL to Cot syntax", .usage = "<file.dbl>", .category = .workspace },
    .{ .name = "gen", .handler = cmdGen, .description = "Generate code (components, etc.)", .usage = "<generator> <name>", .category = .workspace },
    .{ .name = "deps", .handler = cmdDeps, .description = "Show package dependency graph", .usage = "[--json]", .category = .workspace },
};

/// Find a command by name or alias
fn findCommand(name: []const u8) ?*const Command {
    for (&commands) |*cmd| {
        if (std.mem.eql(u8, cmd.name, name)) return cmd;
        for (cmd.aliases) |alias| {
            if (std.mem.eql(u8, alias, name)) return cmd;
        }
    }
    return null;
}

pub fn main() !void {
    // Install crash handlers FIRST
    cot.crash.installHandlers();

    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const cmd_name = args[1];

    // Handle global flags
    if (std.mem.eql(u8, cmd_name, "--help") or std.mem.eql(u8, cmd_name, "-h")) {
        try printUsage();
        return;
    }
    if (std.mem.eql(u8, cmd_name, "--version") or std.mem.eql(u8, cmd_name, "-v")) {
        try printVersion();
        return;
    }

    // Find and execute command
    if (findCommand(cmd_name)) |cmd| {
        try cmd.handler(allocator, args[2..]);
    } else {
        // Assume it's a filename - run with interpreter
        try runFile(allocator, cmd_name);
    }
}

// ============================================================
// Command Handlers
// ============================================================

fn cmdRun(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var selected_runtime: runtime_selector.Runtime = .zig;
    var file_target: ?[]const u8 = null;

    // Parse args until we hit "--" (which separates cot args from program args)
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--")) {
            break; // Stop parsing cot args, rest are for the program
        } else if (std.mem.startsWith(u8, arg, "--runtime=")) {
            const runtime_name = arg[10..];
            if (runtime_selector.Runtime.fromString(runtime_name)) |r| {
                selected_runtime = r;
            } else {
                try printStderr("Error: Unknown runtime '{s}'. Use 'zig' or 'rs'.\n", .{runtime_name});
                return;
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            file_target = arg;
        }
    }

    if (file_target) |target| {
        // Single-file mode: .cot, .cbo files, or stdin (-)
        if (std.mem.endsWith(u8, target, ".cot") or std.mem.endsWith(u8, target, ".cbo") or std.mem.eql(u8, target, "-")) {
            switch (selected_runtime) {
                .zig => try runFileAuto(allocator, target),
                .rs => try runWithRustRuntime(allocator, target),
            }
            return;
        }
    }

    if (selected_runtime == .rs) {
        try printErr("Error: --runtime=rs only works with file targets, not workspace projects.\n");
        return;
    }

    const options = run_cmd.parseArgs(args);
    run_cmd.run(allocator, options) catch |err| {
        if (err == error.NoWorkspace) {
            if (args.len == 0) {
                try printErr("Error: run requires a filename or project target\n");
                try printErr("Usage: cot run <file.cot|file.cbo|project> [--runtime=zig|rs]\n");
            } else {
                try printStderr("Error: {}\n", .{err});
            }
        } else if (err != error.NoApps and err != error.ProjectNotFound and err != error.NotCompiled) {
            try printStderr("Error: {}\n", .{err});
        }
    };
}

fn runWithRustRuntime(allocator: std.mem.Allocator, target: []const u8) !void {
    var bytecode_path: []const u8 = target;
    var allocated = false;

    if (std.mem.endsWith(u8, target, ".cot")) {
        const basename = std.fs.path.stem(target);
        bytecode_path = try std.fmt.allocPrint(allocator, "/tmp/{s}.cbo", .{basename});
        allocated = true;
        compileFile(allocator, target, bytecode_path) catch |err| {
            if (allocated) allocator.free(bytecode_path);
            if (err == error.CompilationFailed) std.process.exit(1);
            return err;
        };
    }
    defer if (allocated) allocator.free(bytecode_path);

    runtime_selector.runWithRustRuntime(allocator, bytecode_path) catch |err| {
        if (err == error.RuntimeNotFound) {
            try printErr("Error: Rust runtime (cot-rs) not found.\n");
            try printErr("Install it or add to PATH.\n");
        }
    };
}

fn cmdCompile(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: compile requires a filename\n");
        try printErr("Usage: cot compile <file.cot> [-o output.cbo]\n");
        return;
    }

    var output_file: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o") and i + 1 < args.len) {
            output_file = args[i + 1];
            i += 1;
        } else if (std.mem.eql(u8, args[i], "--packages") or std.mem.eql(u8, args[i], "--no-packages")) {
            // Legacy flags - ignore, we auto-detect now
        }
    }

    // Simple single-file compilation - just works
    compileFile(allocator, args[0], output_file) catch |err| {
        if (err == error.CompilationFailed) std.process.exit(1);
        return err;
    };
}

/// Check if there's a cot.json in the file's directory or any parent
fn hasWorkspaceConfig(allocator: std.mem.Allocator, filename: []const u8) bool {
    const file_dir = std.fs.path.dirname(filename) orelse ".";
    const abs_file_dir = std.fs.cwd().realpathAlloc(allocator, file_dir) catch return false;
    defer allocator.free(abs_file_dir);

    var search_dir: []const u8 = abs_file_dir;
    while (true) {
        const config_path = std.fs.path.join(allocator, &.{ search_dir, "cot.json" }) catch return false;
        defer allocator.free(config_path);

        if (std.fs.cwd().access(config_path, .{})) |_| {
            return true;
        } else |_| {}

        // Try parent directory
        if (std.fs.path.dirname(search_dir)) |parent| {
            if (std.mem.eql(u8, parent, search_dir)) break;
            search_dir = parent;
        } else {
            break;
        }
    }
    return false;
}

fn cmdFmt(allocator: std.mem.Allocator, args: []const []const u8) !void {
    try formatCommand(allocator, args);
}

fn cmdTest(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: test requires a filename\n");
        try printErr("Usage: cot test <file.cot> [--filter=<name>]\n");
        return;
    }

    var filter: ?[]const u8 = null;
    for (args[1..]) |arg| {
        if (std.mem.startsWith(u8, arg, "--filter=")) {
            filter = arg["--filter=".len..];
        }
    }

    runTests(allocator, args[0], filter) catch |err| {
        if (err == error.TestsFailed) std.process.exit(1);
        return err;
    };
}

fn cmdRepl(allocator: std.mem.Allocator, _: []const []const u8) !void {
    try runRepl(allocator);
}

fn cmdTrace(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: trace requires a filename\n");
        try printErr("Usage: cot trace <file.cot|file.cbo> [--level=opcodes|routines|verbose|slots]\n");
        return;
    }

    var trace_level: trace_mod.TraceLevel = .opcodes;
    for (args[1..]) |arg| {
        if (std.mem.startsWith(u8, arg, "--level=")) {
            const level_str = arg[8..];
            trace_level = if (std.mem.eql(u8, level_str, "none"))
                .none
            else if (std.mem.eql(u8, level_str, "routines"))
                .routines
            else if (std.mem.eql(u8, level_str, "opcodes"))
                .opcodes
            else if (std.mem.eql(u8, level_str, "verbose"))
                .verbose
            else if (std.mem.eql(u8, level_str, "full"))
                .full
            else if (std.mem.eql(u8, level_str, "slots"))
                .slots
            else
                .opcodes;
        }
    }

    try traceFileAuto(allocator, args[0], trace_level);
}

fn cmdDebug(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: debug requires a filename\n");
        try printErr("Usage: cot debug <file.cot|file.cbo>\n");
        return;
    }
    try debugFile(allocator, args[0]);
}

fn cmdValidate(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: validate requires a filename\n");
        try printErr("Usage: cot validate <file.cbo> [--strict]\n");
        return;
    }

    var strict = false;
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--strict")) strict = true;
    }

    try validateBytecode(allocator, args[0], strict);
}

fn cmdDisasm(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: disasm requires a filename\n");
        try printErr("Usage: cot disasm <file.cot|file.cbo>\n");
        return;
    }
    try disasmFile(allocator, args[0]);
}

fn cmdDumpIR(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        try printErr("Error: dump-ir requires a filename\n");
        try printErr("Usage: cot dump-ir <file.cot>\n");
        return;
    }
    try dumpIR(allocator, args[0]);
}

fn cmdInit(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and std.mem.eql(u8, args[0], "--help")) {
        try init_cmd.printHelp();
        return;
    }
    const options = init_cmd.parseArgs(args);
    init_cmd.run(allocator, options) catch |err| {
        if (err != error.AlreadyExists) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdNew(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and std.mem.eql(u8, args[0], "--help")) {
        try new_cmd.printHelp();
        return;
    }
    const options = new_cmd.parseArgs(args);
    new_cmd.run(allocator, options) catch |err| {
        if (err != error.MissingName and err != error.NoWorkspace) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdBuild(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const options = build_cmd.parseArgs(args);
    build_cmd.run(allocator, options) catch |err| {
        if (err != error.NoWorkspace and err != error.BuildFailed) try printStderr("Error: {}\n", .{err});
        std.process.exit(1);
    };
}

fn cmdDev(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and std.mem.eql(u8, args[0], "--help")) {
        try dev_cmd.printHelp();
        return;
    }
    const options = dev_cmd.parseArgs(args);
    dev_cmd.run(allocator, options) catch |err| {
        if (err != error.NoWorkspace) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdSchema(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and (std.mem.eql(u8, args[0], "--help") or std.mem.eql(u8, args[0], "-h"))) {
        try schema_cmd.printHelp();
        return;
    }
    const options = schema_cmd.parseArgs(args);
    schema_cmd.run(allocator, options) catch |err| {
        if (err != error.NoWorkspace and err != error.SchemaLoadFailed) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdData(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and (std.mem.eql(u8, args[0], "--help") or std.mem.eql(u8, args[0], "-h"))) {
        try data_cmd.printHelp();
        return;
    }
    const options = data_cmd.parseArgs(args);
    data_cmd.run(allocator, options) catch |err| {
        if (err != error.NoWorkspace) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdConvert(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const options = convert_cmd.parseArgs(args);
    convert_cmd.run(allocator, options) catch |err| {
        if (err != error.NoInputFile and err != error.NotDblFile) try printStderr("Error: {}\n", .{err});
    };
}

fn cmdGen(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len > 0 and std.mem.eql(u8, args[0], "--help")) {
        try gen_cmd.printHelp();
        return;
    }
    const options = gen_cmd.parseArgs(args);
    gen_cmd.run(allocator, options) catch |err| {
        if (err != error.MissingGenerator and err != error.MissingName and
            err != error.InvalidName and err != error.FileExists and err != error.NameTooLong)
        {
            try printStderr("Error: {}\n", .{err});
        }
    };
}

fn cmdDeps(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var options = deps_cmd.DepsOptions{};

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            options.json = true;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            options.workspace_root = arg;
        }
    }

    deps_cmd.run(allocator, options) catch |err| {
        try printStderr("Error: {}\n", .{err});
    };
}

fn printUsage() !void {
    var stdout_buffer: [8192]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Header
    try stdout.writeAll(
        \\Cot - A modern systems language for business applications
        \\ISAM database and full-stack development
        \\
        \\Usage:
        \\  cot <file.cot>              Run a Cot program directly
        \\  cot <command> [args]        Run a command
        \\  cot --help                  Show this help
        \\  cot --version               Show version
        \\
        \\
    );

    // Generate command list by category
    try stdout.writeAll("Commands:\n");
    try printCommandCategory(stdout, .core);

    try stdout.writeAll("\nWorkspace Commands:\n");
    try printCommandCategory(stdout, .workspace);

    try stdout.writeAll("\nDebug Commands:\n");
    try printCommandCategory(stdout, .debug);

    // Additional info
    try stdout.writeAll(
        \\
        \\Options:
        \\  compile -o <file>           Output file (default: <input>.cbo)
        \\  trace --level=<level>       none, routines, opcodes, verbose, full, slots
        \\  run --runtime=<rt>          zig (default), rs (Rust runtime)
        \\
        \\Examples:
        \\  cot hello.cot               Run a program
        \\  cot compile hello.cot       Compile to bytecode
        \\  cot fmt src/*.cot           Format source files
        \\  cot trace hello.cot         Run with tracing
        \\  cot init my-project         Create a workspace
        \\
    );
    try stdout.flush();
}

fn printCommandCategory(stdout: anytype, category: Command.Category) !void {
    for (commands) |cmd| {
        if (cmd.category == category) {
            // Format: "  cot name usage          description"
            try stdout.print("  cot {s}", .{cmd.name});

            // Add usage if present
            if (cmd.usage.len > 0) {
                try stdout.print(" {s}", .{cmd.usage});
            }

            // Calculate padding for alignment (target column 30)
            const name_len = cmd.name.len + cmd.usage.len + (if (cmd.usage.len > 0) @as(usize, 1) else 0);
            const padding = if (name_len < 24) 24 - name_len else 2;
            var i: usize = 0;
            while (i < padding) : (i += 1) {
                try stdout.writeAll(" ");
            }

            try stdout.print("{s}\n", .{cmd.description});
        }
    }
}

fn printVersion() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Cot version {s}\n", .{cot.version});
    try stdout.writeAll("A modern systems language for business applications\n");
    try stdout.writeAll("https://cot.dev\n");
    try stdout.flush();
}

fn printErr(msg: []const u8) !void {
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    try stderr.writeAll(msg);
    try stderr.flush();
}

fn printStdout(comptime fmt: []const u8, args: anytype) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.print(fmt, args);
    try stdout.flush();
}

fn printStderr(comptime fmt: []const u8, args: anytype) !void {
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;
    try stderr.print(fmt, args);
    try stderr.flush();
}

/// Print a runtime error in compiler-style format: file:line: error[EXXX]: message
fn printRuntimeError(vm: *cot.bytecode.VM, mod: *const cot.bytecode.Module, err: anyerror, bytecode_file: []const u8) void {
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const w = &stderr_writer.interface;

    // Get location info from VM
    const loc = vm.getLocation();
    const rich_err = cot.bytecode.VM.getLastError();

    // Determine source file (from module debug info, or fall back to bytecode file)
    const source_file = mod.source_file orelse bytecode_file;

    // Get line number (from rich error if available, otherwise 0)
    const line: u32 = if (rich_err) |re| re.source_line else 0;

    // Print in compiler-style format: file:line: error: message
    if (line > 0) {
        w.print("\x1b[1m{s}:{d}: \x1b[31merror\x1b[0m\x1b[1m: {s}\x1b[0m\n", .{
            source_file,
            line,
            @errorName(err),
        }) catch {};

        // Try to read and display the source line
        blk: {
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const alloc = gpa.allocator();
            const file = std.fs.cwd().openFile(source_file, .{}) catch break :blk;
            defer file.close();
            const content = file.readToEndAlloc(alloc, 10 * 1024 * 1024) catch break :blk;
            defer alloc.free(content);
            // Find the requested line
            var line_num: u32 = 0;
            var start: usize = 0;
            for (content, 0..) |c, i| {
                if (c == '\n') {
                    line_num += 1;
                    if (line_num == line) {
                        const line_content = content[start..i];
                        w.print("  {d: >4} | {s}\n", .{ line, line_content }) catch {};
                        break;
                    }
                    start = i + 1;
                }
            }
        }
    } else {
        w.print("\x1b[1m{s}: \x1b[31merror\x1b[0m\x1b[1m: {s}\x1b[0m\n", .{
            source_file,
            @errorName(err),
        }) catch {};
    }

    // Print rich error message if available
    if (rich_err) |re| {
        if (re.message.len > 0) {
            w.print("  \x1b[36mmessage\x1b[0m: {s}\n", .{re.message}) catch {};
        }
        if (re.detail) |detail| {
            if (detail.len > 0) {
                w.print("  \x1b[36mdetail\x1b[0m: {s}\n", .{detail}) catch {};
            }
        }
    }

    // Print location details
    const routine_name = loc.routine orelse "main";
    w.print("  \x1b[36mat\x1b[0m: {s} (IP: 0x{x:0>4})\n", .{ routine_name, loc.ip }) catch {};

    // Print opcode if available
    if (rich_err) |re| {
        if (re.opcode) |op| {
            w.print("  \x1b[36mopcode\x1b[0m: {s}\n", .{@tagName(op)}) catch {};
        }
    }

    w.flush() catch {};
}

/// Read a line from stdin into buffer, returning slice or null on EOF
fn readLine(buf: []u8) ?[]u8 {
    const stdin_file = std.fs.File{ .handle = std.posix.STDIN_FILENO };
    var len: usize = 0;
    while (len < buf.len) {
        var byte_buf: [1]u8 = undefined;
        const n = stdin_file.read(&byte_buf) catch return null;
        if (n == 0) {
            // EOF
            if (len == 0) return null;
            break;
        }
        if (byte_buf[0] == '\n') break;
        buf[len] = byte_buf[0];
        len += 1;
    }
    return buf[0..len];
}

/// Check if a file needs an external frontend and dispatch if so
/// Returns true if dispatched to external frontend, false if cot should handle it
fn tryDispatchToFrontend(allocator: std.mem.Allocator, filename: []const u8, extra_args: []const []const u8) !bool {
    const ext = frontends.getExtension(filename) orelse return false;

    if (frontends.findFrontendForExtension(ext)) |frontend| {
        // Build args: [filename, extra_args...]
        var args: std.ArrayListAligned([]const u8, null) = .empty;
        defer args.deinit(allocator);

        try args.append(allocator, filename);
        for (extra_args) |arg| {
            try args.append(allocator, arg);
        }

        frontends.runFrontend(allocator, frontend, args.items) catch |err| {
            if (err == error.FrontendNotFound) {
                try printStderr("Error: Frontend '{s}' not found.\n", .{frontend.executable});
                try printStderr("Install it with: cot install {s}\n", .{frontend.name});
                try printStderr("Or run directly: {s} {s}\n", .{ frontend.executable, filename });
                return true;
            }
            return err;
        };
        return true;
    }

    return false;
}

fn runFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Try to dispatch to external frontend
    if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{})) {
        return;
    }

    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(source);

    // Change to the source file's directory so relative paths work correctly
    if (std.fs.path.dirname(filename)) |dir| {
        std.posix.chdir(dir) catch {};
    }

    cot.runWithFile(allocator, source, filename) catch |err| {
        // Check if this is a compile-time error by looking at the error name
        const err_name = @errorName(err);
        const is_compile_error = std.mem.startsWith(u8, err_name, "Undefined") or
            std.mem.startsWith(u8, err_name, "Type") or
            std.mem.startsWith(u8, err_name, "Invalid") or
            std.mem.startsWith(u8, err_name, "Unsupported") or
            std.mem.startsWith(u8, err_name, "Unknown") or
            std.mem.startsWith(u8, err_name, "Verification") or
            std.mem.startsWith(u8, err_name, "Missing") or
            std.mem.eql(u8, err_name, "LowerError");

        if (is_compile_error) {
            // Compile-time error - error details were already printed by the compiler
            try printStderr("{s}: Compilation failed: {s}\n", .{ filename, err_name });
        } else {
            try printStderr("{s}: Runtime error: {s}\n", .{ filename, err_name });
        }
    };
}

fn runFileAuto(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Check file extension for compiled bytecode formats
    if (std.mem.endsWith(u8, filename, ".cbo") or // compiled object
        std.mem.endsWith(u8, filename, ".clb") or // compiled library
        std.mem.endsWith(u8, filename, ".cbr")) // compiled runnable
    {
        try runBytecodeFile(allocator, filename);
    } else {
        try runFile(allocator, filename);
    }
}

fn runBytecodeFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    const extension = cot.extension;

    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(bytes);

    // NOTE: Unlike source files, bytecode files should NOT change the working directory.
    // A compiled program behaves like a normal executable - its CWD is wherever it was
    // invoked from, not where the .cbo file is located. This allows compiled programs
    // to work with relative paths in the caller's directory.

    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Deserialize and run
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer mod.deinit();

    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    // Load DBL extension for channel-based I/O (db_open, db_store, etc.)
    // This is needed for .cbo files compiled from DBL sources
    try vm.loadExtension(dbl_ext.dbl_extension);

    vm.execute(&mod) catch |err| {
        printRuntimeError(&vm, &mod, err, filename);
    };
}

fn traceFileAuto(allocator: std.mem.Allocator, filename: []const u8, level: trace_mod.TraceLevel) !void {
    // Check file extension for compiled bytecode formats
    if (std.mem.endsWith(u8, filename, ".cbo") or
        std.mem.endsWith(u8, filename, ".clb") or
        std.mem.endsWith(u8, filename, ".cbr"))
    {
        try traceBytecodeFile(allocator, filename, level);
    } else {
        try traceSourceFile(allocator, filename, level);
    }
}

fn traceSourceFile(allocator: std.mem.Allocator, filename: []const u8, level: trace_mod.TraceLevel) !void {
    // Try to dispatch to external frontend with trace level argument
    const level_str: []const u8 = switch (level) {
        .none => "--level=none",
        .routines => "--level=routines",
        .opcodes => "--level=opcodes",
        .verbose => "--level=verbose",
        .full => "--level=full",
        .slots => "--level=slots",
    };
    if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{ "trace", level_str })) {
        return;
    }

    const compiler = cot.compiler;
    const DiagnosticCollector = compiler.DiagnosticCollector;
    const formatter = compiler.formatter;
    const diagnostics = compiler.diagnostics;

    // Initialize diagnostic collector
    var collector = DiagnosticCollector.init(allocator);
    defer collector.deinit();

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // Change to the source file's directory so relative paths work correctly
    if (std.fs.path.dirname(filename)) |dir| {
        std.posix.chdir(dir) catch {};
    }

    // Cache source for diagnostic context
    collector.cacheSource(filename, source) catch {};

    // Tokenize
    var lex = cot.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        collector.addError(
            .E001_invalid_character,
            filename,
            diagnostics.SourceRange.none,
            "Lexer error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };
    defer allocator.free(tokens);

    // Create NodeStore and StringInterner
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    // Register file for error tracking
    const file_id = store.registerFile(filename) catch .unknown;
    store.setCurrentFile(file_id);

    // Parse
    var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();
    const top_level = parse.parse() catch |err| {
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
        if (collector.error_count == 0) {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.none,
                "Parser error: {}",
                .{err},
            );
        }
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };

    // Check for non-fatal parse errors
    if (parse.errors.items.len > 0) {
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
    }

    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    }

    // Run compile-time evaluation (resolve comptime if, evaluate const)
    var evaluator = cot.comptime_eval.Evaluator.init(allocator, &store, &strings);
    defer evaluator.deinit();
    evaluator.setSourceFile(filename);

    const processed_stmts = evaluator.process(top_level) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "Compile-time evaluation error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };
    defer allocator.free(processed_stmts);

    // Lower AST to IR using detailed error reporting
    const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{ .source_file = filename });
    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                // Use file path from error detail if available (for imports), otherwise use main filename
                const error_file = detail.file_path orelse filename;
                collector.addError(
                    .E300_undefined_label,
                    error_file,
                    diagnostics.SourceRange.fromLoc(detail.line, detail.column),
                    "{s}",
                    .{detail.message},
                );
                if (detail.context.len > 0) {
                    try printStderr("  while {s}\n", .{detail.context});
                }
            } else {
                collector.addError(
                    .E300_undefined_label,
                    filename,
                    diagnostics.SourceRange.none,
                    "IR lowering error: {}",
                    .{e.kind},
                );
            }
            formatter.printToStderr(&collector, .{ .use_color = true });
            return;
        },
    };
    defer ir_module.deinit();

    // Type check
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(allocator, &collector, ir_module, filename);
    type_checker.check();

    // Print all diagnostics (errors and warnings)
    if (collector.hasErrors() or collector.hasWarnings()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
    }

    if (collector.hasErrors()) {
        return;
    }

    // Optimize IR
    _ = cot.ir_optimize.optimize(ir_module, .{});

    // Emit bytecode
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
    defer emitter.deinit();

    var mod = emitter.emit(ir_module) catch |err| {
        if (emitter.getLastError()) |ctx| {
            collector.addError(
                .E300_undefined_label,
                filename,
                diagnostics.SourceRange.none,
                "{s}",
                .{ctx.message},
            );
            if (ctx.detail.len > 0) {
                try printStderr("  Note: {s}\n", .{ctx.detail});
            }
        } else {
            collector.addError(
                .E300_undefined_label,
                filename,
                diagnostics.SourceRange.none,
                "Bytecode emission error: {}",
                .{err},
            );
        }
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    defer mod.deinit();

    // Set source file for runtime error reporting
    mod.source_file = try allocator.dupe(u8, filename);

    // Create tracer
    // At --level=full, show raw value bits for debugging NaN-boxing issues
    var tracer = trace_mod.Tracer.init(allocator, .{
        .level = level,
        .output = .{
            .format = .human,
            .target = .stderr,
            .color = true,
            .show_raw_bits = level == .full,
        },
        .history_size = 256,
    });
    defer tracer.deinit();

    // Create VM and attach tracer
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    vm.setTracer(&tracer);

    try printStderr("=== Execution Trace ===\n", .{});
    vm.execute(&mod) catch |err| {
        try printStderr("\n", .{});
        printRuntimeError(&vm, &mod, err, filename);
        // Dump history on error
        try printStderr("\n", .{});
        const stderr_file: std.fs.File = .stderr();
        var buf: [4096]u8 = undefined;
        var stderr_writer = stderr_file.writer(&buf);
        vm.dumpTraceHistory(&stderr_writer.interface) catch {};
    };
    try printStderr("\n=== Trace Statistics ===\n", .{});
    try printStderr("Opcodes executed: {d}\n", .{tracer.stats.opcodes_executed});
    try printStderr("Calls made: {d}\n", .{tracer.stats.calls_made});
    try printStderr("Duration: {d}ms\n", .{tracer.stats.durationMs()});
}

fn traceBytecodeFile(allocator: std.mem.Allocator, filename: []const u8, level: trace_mod.TraceLevel) !void {
    const extension = cot.extension;

    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(bytes);

    // NOTE: Unlike source files, bytecode files should NOT change the working directory.
    // A compiled program behaves like a normal executable - its CWD is wherever it was
    // invoked from, not where the .cbo file is located.

    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Deserialize
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer mod.deinit();

    // Create tracer
    // At --level=full, show raw value bits for debugging NaN-boxing issues
    var tracer = trace_mod.Tracer.init(allocator, .{
        .level = level,
        .output = .{
            .format = .human,
            .target = .stderr,
            .color = true,
            .show_raw_bits = level == .full,
        },
        .history_size = 256,
    });
    defer tracer.deinit();

    // Create VM and attach tracer
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    // Load DBL extension for channel-based I/O (db_open, db_store, etc.)
    try vm.loadExtension(dbl_ext.dbl_extension);

    vm.setTracer(&tracer);

    try printStderr("=== Execution Trace ===\n", .{});
    vm.execute(&mod) catch |err| {
        try printStderr("\n", .{});
        printRuntimeError(&vm, &mod, err, filename);
        // Dump history on error
        try printStderr("\n", .{});
        // Use stderr for trace dump - requires a buffer for Zig 0.15
        const stderr_file: std.fs.File = .stderr();
        var buf: [4096]u8 = undefined;
        var stderr_writer = stderr_file.writer(&buf);
        vm.dumpTraceHistory(&stderr_writer.interface) catch {};
    };
    try printStderr("\n=== Trace Statistics ===\n", .{});
    try printStderr("Opcodes executed: {d}\n", .{tracer.stats.opcodes_executed});
    try printStderr("Calls made: {d}\n", .{tracer.stats.calls_made});
    try printStderr("Duration: {d}ms\n", .{tracer.stats.durationMs()});
}

fn debugFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    const is_bytecode = std.mem.endsWith(u8, filename, ".cbo");

    // For .cot files, compile first
    var module: cot.bytecode.Module = undefined;
    var temp_bytes: ?[]u8 = null;
    defer if (temp_bytes) |b| allocator.free(b);

    // Store source for list command (only available for .cot files)
    var source_lines: ?[]const []const u8 = null;
    var source_storage: ?[]u8 = null;
    defer if (source_storage) |s| allocator.free(s);
    defer if (source_lines) |lines| allocator.free(lines);

    if (is_bytecode) {
        // Load bytecode directly
        const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
            try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
            return;
        };
        defer file.close();

        temp_bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
        var fbs = std.io.fixedBufferStream(temp_bytes.?);
        module = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
            try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
            return;
        };
    } else {
        // Compile .cot file first
        try printStdout("Compiling {s}...\n", .{filename});

        // Read source and keep it for list command
        source_storage = std.fs.cwd().readFileAlloc(allocator, filename, 1024 * 1024) catch |err| {
            try printStderr("Error: Could not read file '{s}': {}\n", .{ filename, err });
            return;
        };

        // Split into lines for list command
        var line_list: std.ArrayListUnmanaged([]const u8) = .empty;
        var iter = std.mem.splitScalar(u8, source_storage.?, '\n');
        while (iter.next()) |line| {
            line_list.append(allocator, line) catch break;
        }
        source_lines = line_list.toOwnedSlice(allocator) catch null;

        // Compile to module (uses IR pipeline)
        module = cot.compileToModule(allocator, source_storage.?, std.fs.path.stem(filename)) catch |err| {
            try printStderr("Error: Compilation failed: {}\n", .{err});
            return;
        };
    }
    defer module.deinit();

    // Initialize VM
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    // Enable debugger
    vm.debugger.enable();

    // Load module and initialize for execution
    try vm.loadModule(&module);
    vm.initForModule(&module) catch |err| {
        try printStderr("Error initializing module: {}\n", .{err});
        return;
    };

    // Print welcome message
    try printStdout("\n=== Cot Interactive Debugger ===\n", .{});
    try printStdout("File: {s}\n", .{filename});
    try printStdout("Entry point: 0x{x:0>4}\n\n", .{module.header.entry_point});

    try printStdout("Commands:\n", .{});
    try printStdout("  r, run        - Run/continue execution\n", .{});
    try printStdout("  s, step       - Step one instruction\n", .{});
    try printStdout("  n, next       - Step over (to next line)\n", .{});
    try printStdout("  o, out        - Step out of current function\n", .{});
    try printStdout("  b <line>      - Set breakpoint at line\n", .{});
    try printStdout("  d <line>      - Delete breakpoint at line\n", .{});
    try printStdout("  i, info       - Show VM state\n", .{});
    try printStdout("  reg           - Show registers\n", .{});
    try printStdout("  locals        - Show local variables\n", .{});
    try printStdout("  stack         - Show stack\n", .{});
    try printStdout("  bt, backtrace - Show call stack\n", .{});
    try printStdout("  l, list       - Show source around current line\n", .{});
    try printStdout("  disasm        - Disassemble around current IP\n", .{});
    try printStdout("  q, quit       - Exit debugger\n\n", .{});

    // Interactive REPL
    var line_buf: [256]u8 = undefined;

    while (true) {
        // Show current position
        const inspector = vm.createInspector();
        const snapshot = inspector.getSnapshot();

        try printStdout("(debug) IP=0x{x:0>4} L{d} > ", .{ snapshot.ip, snapshot.line });

        // Read command - simple byte-by-byte read
        const line = readLine(&line_buf) orelse break;

        const cmd = std.mem.trim(u8, line, " \t\r\n");
        if (cmd.len == 0) continue;

        // Parse and execute command
        if (std.mem.eql(u8, cmd, "q") or std.mem.eql(u8, cmd, "quit")) {
            try printStdout("Exiting debugger.\n", .{});
            break;
        } else if (std.mem.eql(u8, cmd, "r") or std.mem.eql(u8, cmd, "run")) {
            vm.debugger.continue_();
            _ = vm.runUntilStop() catch |err| {
                try printStderr("Execution error: {}\n", .{err});
            };
            if (vm.stop_reason) |reason| {
                switch (reason) {
                    .completed => try printStdout("Program completed.\n", .{}),
                    .breakpoint => try printStdout("Hit breakpoint.\n", .{}),
                    .step => try printStdout("Step completed.\n", .{}),
                    .err => try printStderr("Error occurred.\n", .{}),
                    .paused => try printStdout("Execution paused.\n", .{}),
                }
            }
        } else if (std.mem.eql(u8, cmd, "s") or std.mem.eql(u8, cmd, "step")) {
            vm.debugger.stepInto();
            _ = vm.runUntilStop() catch |err| {
                try printStderr("Execution error: {}\n", .{err});
            };
        } else if (std.mem.eql(u8, cmd, "n") or std.mem.eql(u8, cmd, "next")) {
            vm.debugger.stepOver(vm.call_stack.len);
            _ = vm.runUntilStop() catch |err| {
                try printStderr("Execution error: {}\n", .{err});
            };
        } else if (std.mem.eql(u8, cmd, "o") or std.mem.eql(u8, cmd, "out")) {
            vm.debugger.stepOut(vm.call_stack.len);
            _ = vm.runUntilStop() catch |err| {
                try printStderr("Execution error: {}\n", .{err});
            };
        } else if (std.mem.startsWith(u8, cmd, "b ") or std.mem.startsWith(u8, cmd, "break ")) {
            const arg = if (std.mem.startsWith(u8, cmd, "b ")) cmd[2..] else cmd[6..];
            const line_num = std.fmt.parseInt(u32, std.mem.trim(u8, arg, " "), 10) catch {
                try printStderr("Invalid line number: {s}\n", .{arg});
                continue;
            };
            const bp_id = vm.debugger.setBreakpoint(line_num);
            try printStdout("Breakpoint {d} set at line {d}\n", .{ bp_id, line_num });
        } else if (std.mem.startsWith(u8, cmd, "d ") or std.mem.startsWith(u8, cmd, "delete ")) {
            const arg = if (std.mem.startsWith(u8, cmd, "d ")) cmd[2..] else cmd[7..];
            const line_num = std.fmt.parseInt(u32, std.mem.trim(u8, arg, " "), 10) catch {
                try printStderr("Invalid line number: {s}\n", .{arg});
                continue;
            };
            if (vm.debugger.removeBreakpoint(line_num)) {
                try printStdout("Breakpoint at line {d} removed\n", .{line_num});
            } else {
                try printStderr("No breakpoint at line {d}\n", .{line_num});
            }
        } else if (std.mem.eql(u8, cmd, "i") or std.mem.eql(u8, cmd, "info")) {
            try printStdout("IP: 0x{x:0>4}\n", .{snapshot.ip});
            try printStdout("Line: {d}\n", .{snapshot.line});
            try printStdout("SP: {d}\n", .{snapshot.sp});
            try printStdout("FP: {d}\n", .{snapshot.fp});
            try printStdout("Call depth: {d}\n", .{snapshot.call_depth});
            if (snapshot.routine) |r| {
                try printStdout("Routine: {s}\n", .{r});
            }
        } else if (std.mem.eql(u8, cmd, "reg") or std.mem.eql(u8, cmd, "registers")) {
            var reg_buf: [512]u8 = undefined;
            const reg_str = inspector.formatRegisters(&reg_buf);
            try printStdout("Registers: {s}\n", .{reg_str});
        } else if (std.mem.eql(u8, cmd, "stack")) {
            try printStdout("Stack (top 8 entries, SP={d}):\n", .{snapshot.sp});
            const top = inspector.getTopOfStack(8);
            for (top, 0..) |val, i| {
                var val_buf: [64]u8 = undefined;
                const val_str = val.debugRepr(&val_buf);
                try printStdout("  [{d}]: {s}\n", .{ snapshot.sp - top.len + i, val_str });
            }
        } else if (std.mem.eql(u8, cmd, "bt") or std.mem.eql(u8, cmd, "backtrace")) {
            var bt_buf: [1024]u8 = undefined;
            const bt_str = inspector.formatBacktrace(&bt_buf);
            if (bt_str.len > 0) {
                try printStdout("Call stack:\n{s}", .{bt_str});
            } else {
                try printStdout("(empty call stack)\n", .{});
            }
        } else if (std.mem.eql(u8, cmd, "locals")) {
            // Show local variables
            const locals = inspector.getLocals(allocator) catch |err| {
                try printStderr("Error getting locals: {}\n", .{err});
                continue;
            };
            defer allocator.free(locals);

            if (locals.len == 0) {
                try printStdout("(no local variables)\n", .{});
            } else {
                try printStdout("Local variables:\n", .{});
                for (locals) |local| {
                    var val_buf: [64]u8 = undefined;
                    const val_str = local.format(&val_buf);
                    const scope_str = switch (local.scope) {
                        .parameter => "param",
                        .local => "local",
                        .global => "global",
                    };
                    try printStdout("  {s} {s}: {s} = {s}\n", .{
                        scope_str,
                        local.name,
                        local.type_name,
                        val_str,
                    });
                }
            }
        } else if (std.mem.eql(u8, cmd, "l") or std.mem.eql(u8, cmd, "list")) {
            // Show source code around current line
            if (source_lines) |lines| {
                const current = snapshot.line;
                const start = if (current > 5) current - 5 else 1;
                const end = @min(current + 5, @as(u32, @intCast(lines.len)));

                try printStdout("Source ({s}):\n", .{filename});
                var line_num = start;
                while (line_num <= end) : (line_num += 1) {
                    const idx = line_num - 1;
                    if (idx < lines.len) {
                        const marker: []const u8 = if (line_num == current) " => " else "    ";
                        try printStdout("{s}{d:4}: {s}\n", .{ marker, line_num, lines[idx] });
                    }
                }
            } else {
                try printStdout("(source not available for bytecode files)\n", .{});
            }
        } else if (std.mem.eql(u8, cmd, "disasm")) {
            // Disassemble around current IP
            const ip = snapshot.ip;
            const code = module.code;

            try printStdout("Disassembly around IP=0x{x:0>4}:\n", .{ip});

            // Show 5 instructions before and after (approximately)
            var disasm_ip: usize = if (ip > 20) ip - 20 else 0;
            var count: usize = 0;
            while (disasm_ip < code.len and count < 15) : (count += 1) {
                const opcode: cot.bytecode.Opcode = @enumFromInt(code[disasm_ip]);
                const size = 1 + opcode.operandSize();
                const marker: []const u8 = if (disasm_ip == ip) " => " else "    ";
                try printStdout("{s}0x{x:0>4}: {s}\n", .{ marker, disasm_ip, @tagName(opcode) });
                disasm_ip += size;
            }
        } else if (std.mem.eql(u8, cmd, "h") or std.mem.eql(u8, cmd, "help") or std.mem.eql(u8, cmd, "?")) {
            try printStdout("Commands:\n", .{});
            try printStdout("  r, run        - Run/continue execution\n", .{});
            try printStdout("  s, step       - Step one instruction\n", .{});
            try printStdout("  n, next       - Step over (to next line)\n", .{});
            try printStdout("  o, out        - Step out of current function\n", .{});
            try printStdout("  b <line>      - Set breakpoint at line\n", .{});
            try printStdout("  d <line>      - Delete breakpoint at line\n", .{});
            try printStdout("  i, info       - Show VM state\n", .{});
            try printStdout("  reg           - Show registers\n", .{});
            try printStdout("  locals        - Show local variables\n", .{});
            try printStdout("  stack         - Show stack\n", .{});
            try printStdout("  bt, backtrace - Show call stack\n", .{});
            try printStdout("  l, list       - Show source around current line\n", .{});
            try printStdout("  disasm        - Disassemble around current IP\n", .{});
            try printStdout("  h, help, ?    - Show this help\n", .{});
            try printStdout("  q, quit       - Exit debugger\n", .{});
        } else {
            try printStderr("Unknown command: {s}\n", .{cmd});
        }
    }
}

/// Format source files (cot fmt)
fn formatCommand(allocator: std.mem.Allocator, args: []const []const u8) !void {
    const formatter = cot.formatter;

    // Parse options
    var check_only = false;
    var stdin_mode = false;
    var files: std.ArrayListUnmanaged([]const u8) = .empty;
    defer files.deinit(allocator);

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--check")) {
            check_only = true;
        } else if (std.mem.eql(u8, arg, "--stdin")) {
            stdin_mode = true;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try printFmtHelp();
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            try files.append(allocator, arg);
        }
    }

    // Handle stdin mode
    if (stdin_mode) {
        const stdin = std.fs.File.stdin();
        const source = try stdin.readToEndAlloc(allocator, 10 * 1024 * 1024); // 10MB max
        defer allocator.free(source);

        // Default to .cot for stdin
        const result = try formatter.format(allocator, source, .cot, .{});
        if (result) |formatted| {
            defer allocator.free(formatted);
            const stdout = std.fs.File.stdout();
            _ = try stdout.write(formatted);
        } else {
            const stdout = std.fs.File.stdout();
            _ = try stdout.write(source);
        }
        return;
    }

    // Need files to format
    if (files.items.len == 0) {
        try printErr("Error: No files specified\n");
        try printErr("Usage: cot fmt <files...> [--check]\n");
        return;
    }

    var any_changed = false;
    var any_error = false;

    for (files.items) |filepath| {
        // Detect language from extension
        const language = formatter.Language.fromPath(filepath) orelse {
            try printStderr("Warning: Skipping '{s}' (unknown file type)\n", .{filepath});
            continue;
        };

        // Read file
        const file = std.fs.cwd().openFile(filepath, .{}) catch |err| {
            try printStderr("Error: Could not open '{s}': {}\n", .{ filepath, err });
            any_error = true;
            continue;
        };
        defer file.close();

        const source = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch |err| {
            try printStderr("Error: Could not read '{s}': {}\n", .{ filepath, err });
            any_error = true;
            continue;
        };
        defer allocator.free(source);

        // Format
        const result = formatter.format(allocator, source, language, .{}) catch |err| {
            try printStderr("Error: Format failed for '{s}': {}\n", .{ filepath, err });
            any_error = true;
            continue;
        };

        if (result) |formatted| {
            defer allocator.free(formatted);
            any_changed = true;

            if (check_only) {
                try printStdout("{s}\n", .{filepath});
            } else {
                // Write formatted content back
                const out_file = std.fs.cwd().createFile(filepath, .{}) catch |err| {
                    try printStderr("Error: Could not write '{s}': {}\n", .{ filepath, err });
                    any_error = true;
                    continue;
                };
                defer out_file.close();
                _ = out_file.write(formatted) catch |err| {
                    try printStderr("Error: Write failed for '{s}': {}\n", .{ filepath, err });
                    any_error = true;
                    continue;
                };
            }
        }
    }

    if (check_only and any_changed) {
        std.process.exit(1);
    }
    if (any_error) {
        std.process.exit(1);
    }
}

fn printFmtHelp() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\cot fmt - Format Cot and DBL source files
        \\
        \\Usage:
        \\  cot fmt <files...>      Format files in place
        \\  cot fmt --check <files> Check if files need formatting
        \\  cot fmt --stdin         Format stdin to stdout
        \\
        \\Options:
        \\  --check                 Check only, don't write (exit 1 if changes needed)
        \\  --stdin                 Read from stdin, write to stdout
        \\  --help, -h              Show this help
        \\
        \\Examples:
        \\  cot fmt src/*.cot       Format all Cot files in src/
        \\  cot fmt --check .       Check all files (for CI)
        \\  cat file.cot | cot fmt --stdin > formatted.cot
        \\
    );
    try stdout.flush();
}

fn validateBytecode(allocator: std.mem.Allocator, filename: []const u8, strict: bool) !void {
    // Open and read bytecode file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 10); // 10MB max
    defer allocator.free(bytes);

    // Deserialize bytecode module
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
        try printStderr("  This may indicate a corrupted or incompatible bytecode version.\n", .{});
        return;
    };
    defer mod.deinit();

    // Run validator
    var validator = debug_tools.Validator.init(allocator, &mod);
    defer validator.deinit();

    const result = validator.validate() catch |err| {
        try printStderr("Error: Validation failed: {}\n", .{err});
        return;
    };

    // Print results
    try printStdout("\n=== Bytecode Validation Report ===\n\n", .{});
    try printStdout("File: {s}\n", .{filename});
    try printStdout("Code size: {d} bytes\n", .{result.stats.code_size});
    try printStdout("Instructions: {d}\n", .{result.stats.instruction_count});
    try printStdout("Routines: {d}\n", .{result.stats.routine_count});
    try printStdout("Constants: {d}\n", .{result.stats.constant_count});
    try printStdout("Types: {d}\n\n", .{result.stats.type_count});

    // Print issues
    if (result.issues.len == 0) {
        try printStdout("No issues found.\n", .{});
    } else {
        try printStdout("Issues ({d}):\n", .{result.issues.len});
        for (result.issues) |issue| {
            const severity_str = switch (issue.severity) {
                .@"error" => "\x1b[31mERROR\x1b[0m",
                .warning => "\x1b[33mWARNING\x1b[0m",
                .info => "\x1b[36mINFO\x1b[0m",
            };
            try printStdout("  [{s}] 0x{x:0>4}: {s}\n", .{
                severity_str,
                issue.offset,
                issue.message,
            });
        }
        try printStdout("\n", .{});
    }

    // Summary
    try printStdout("Errors: {d}, Warnings: {d}\n", .{ result.stats.error_count, result.stats.warning_count });
    try printStdout("Code coverage (reachable): {d:.1}%\n\n", .{result.stats.code_coverage});

    if (result.is_valid) {
        try printStdout("\x1b[32mValidation PASSED\x1b[0m\n", .{});
    } else {
        try printStdout("\x1b[31mValidation FAILED\x1b[0m\n", .{});
        if (strict) {
            std.process.exit(1);
        }
    }

    // In strict mode, treat warnings as errors too
    if (strict and result.stats.warning_count > 0) {
        try printStdout("\n\x1b[33m--strict: Warnings treated as errors\x1b[0m\n", .{});
        std.process.exit(1);
    }
}

/// Parse a single source file into an existing NodeStore
/// Returns the top-level statements from this file
fn parseFileIntoStore(
    allocator: std.mem.Allocator,
    filename: []const u8,
    store: *cot.ast.NodeStore,
    strings: *cot.base.StringInterner,
    collector: *cot.compiler.DiagnosticCollector,
) ![]const cot.ast.StmtIdx {
    const diagnostics = cot.compiler.diagnostics;
    const formatter = cot.compiler.formatter;

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return error.CompilationFailed;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    collector.cacheSource(filename, source) catch {};

    // Register file for error tracking and set as current file for parsing
    const file_id = store.registerFile(filename) catch .unknown;
    store.setCurrentFile(file_id);

    // Tokenize
    var lex = cot.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        collector.addError(
            .E001_invalid_character,
            filename,
            diagnostics.SourceRange.none,
            "Lexer error: {}",
            .{err},
        );
        formatter.printToStderr(collector, .{ .use_color = true });
        return error.CompilationFailed;
    };

    // Parse into the shared store
    var parse = cot.parser.Parser.init(allocator, tokens, store, strings);
    const top_level = parse.parse() catch |err| {
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
        if (collector.error_count == 0) {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.none,
                "Parser error: {}",
                .{err},
            );
        }
        formatter.printToStderr(collector, .{ .use_color = true });
        return error.CompilationFailed;
    };

    // Check for non-fatal parser errors
    if (parse.errors.items.len > 0) {
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
    }

    return top_level;
}

/// Import info with source location for error reporting
const ImportInfo = struct {
    name: []const u8,
    line: u32,
    column: u32,
};

/// Extract import paths from parsed statements
fn extractImports(
    allocator: std.mem.Allocator,
    store: *const cot.ast.NodeStore,
    strings: *const cot.base.StringInterner,
    stmts: []const cot.ast.StmtIdx,
) ![]ImportInfo {
    var imports: std.ArrayListUnmanaged(ImportInfo) = .{};

    for (stmts) |stmt_idx| {
        const tag = store.stmtTag(stmt_idx);
        if (tag == .import_stmt) {
            const data = store.stmtData(stmt_idx);
            const module_name = strings.get(data.getName());
            const loc = store.stmtLoc(stmt_idx);
            // Duplicate the string since we need to return owned data
            const duped = try allocator.dupe(u8, module_name);
            try imports.append(allocator, .{
                .name = duped,
                .line = loc.line,
                .column = loc.column,
            });
        }
    }

    return imports.toOwnedSlice(allocator);
}

/// Resolve an import module name to a file path
fn resolveImportPath(
    allocator: std.mem.Allocator,
    base_dir: []const u8,
    module_name: []const u8,
) ![]const u8 {
    // If module_name already has an extension, use it directly
    if (std.mem.endsWith(u8, module_name, ".cot")) {
        return std.fs.path.join(allocator, &.{ base_dir, module_name });
    }
    // Otherwise, add .cot extension
    const filename = try std.fmt.allocPrint(allocator, "{s}.cot", .{module_name});
    defer allocator.free(filename);
    return std.fs.path.join(allocator, &.{ base_dir, filename });
}

/// Compile using the package system
/// Discovers packages from cot.json, compiles in dependency order, and links
fn compileWithPackages(backing_allocator: std.mem.Allocator, filename: []const u8, output_file: ?[]const u8) !void {
    const framework = cot.framework;
    const pkg_module = framework.package;
    const pkg_compiler = framework.package_compiler;

    // Find workspace root by looking for cot.json
    const file_dir = std.fs.path.dirname(filename) orelse ".";
    const abs_file_dir = try std.fs.cwd().realpathAlloc(backing_allocator, file_dir);
    defer backing_allocator.free(abs_file_dir);

    // Look for cot.json in current dir or parent dirs
    var workspace_root: ?[]const u8 = null;
    var search_dir: []const u8 = abs_file_dir;

    while (true) {
        const config_path = try std.fs.path.join(backing_allocator, &.{ search_dir, "cot.json" });
        defer backing_allocator.free(config_path);

        if (std.fs.cwd().access(config_path, .{})) |_| {
            workspace_root = try backing_allocator.dupe(u8, search_dir);
            break;
        } else |_| {}

        // Try parent directory
        if (std.fs.path.dirname(search_dir)) |parent| {
            if (std.mem.eql(u8, parent, search_dir)) break; // Reached root
            search_dir = parent;
        } else {
            break;
        }
    }

    if (workspace_root == null) {
        try printStderr("Error: No cot.json found. Create one or use compile without --packages.\n", .{});
        return error.CompilationFailed;
    }
    defer backing_allocator.free(workspace_root.?);

    try printStderr("Using package system with workspace: {s}\n", .{workspace_root.?});

    // Initialize package manager
    var pm = try pkg_module.PackageManager.init(backing_allocator, workspace_root.?);
    defer pm.deinit();

    // Discover packages
    try pm.discoverPackages();

    // Scan for dependencies
    try scanPackageDependencies(backing_allocator, &pm);

    // Compute build order
    pm.computeBuildOrder() catch |err| {
        if (err == error.CircularDependency) {
            try printStderr("Error: Circular dependency detected\n", .{});
            return error.CompilationFailed;
        }
        return err;
    };

    try printStderr("Build order: ", .{});
    for (pm.build_order.items, 0..) |name, i| {
        if (i > 0) try printStderr(" -> ", .{});
        try printStderr("{s}", .{name});
    }
    try printStderr("\n\n", .{});

    // Create workspace compiler
    var wc = pkg_compiler.WorkspaceCompiler.init(backing_allocator, &pm);
    defer wc.deinit();

    // Set up build cache in workspace
    var cache_path_buf: [512]u8 = undefined;
    const cache_path = std.fmt.bufPrint(&cache_path_buf, "{s}/.cot-cache", .{workspace_root.?}) catch workspace_root.?;
    wc.setCacheDir(cache_path) catch {};

    // Compile all packages
    try printStderr("Compiling packages...\n", .{});
    wc.compileAll() catch |err| {
        try printStderr("Package compilation failed: {}\n", .{err});
        return error.CompilationFailed;
    };

    // Report results
    var it = wc.compiled.iterator();
    while (it.next()) |entry| {
        const compiled = entry.value_ptr.*;
        try printStderr("  {s}: {} bytes, {} exports\n", .{
            compiled.name,
            compiled.bytecode.len,
            compiled.exports.entries.count(),
        });
    }

    // Determine entry package (from filename)
    const basename = std.fs.path.basename(filename);
    const entry_name = if (std.mem.endsWith(u8, basename, ".cot"))
        basename[0 .. basename.len - 4]
    else
        basename;

    // Find package containing the entry file
    var entry_package: ?[]const u8 = null;
    var pkg_it = pm.packages.iterator();
    while (pkg_it.next()) |entry| {
        const pkg = entry.value_ptr.*;
        for (pkg.source_files.items) |src| {
            if (std.mem.endsWith(u8, src, basename)) {
                entry_package = pkg.name;
                break;
            }
        }
        if (entry_package != null) break;
    }

    if (entry_package == null) {
        // Fallback: use filename without extension
        entry_package = entry_name;
    }

    // Link and output
    const out_path = output_file orelse blk: {
        // Generate output path
        const out = try std.fmt.allocPrint(backing_allocator, "{s}.cbo", .{entry_name});
        break :blk out;
    };
    defer if (output_file == null) backing_allocator.free(out_path);

    try printStderr("\nLinking to: {s}\n", .{out_path});
    wc.link(entry_package.?, out_path) catch |err| {
        try printStderr("Linking failed: {}\n", .{err});
        return error.CompilationFailed;
    };

    try printStderr("Compiled: {s} -> {s}\n", .{ filename, out_path });
}

/// Scan package source files for import dependencies
fn scanPackageDependencies(allocator: std.mem.Allocator, pm: *cot.framework.package.PackageManager) !void {
    var it = pm.packages.iterator();
    while (it.next()) |entry| {
        const pkg = entry.value_ptr.*;

        for (pkg.source_files.items) |source_file| {
            try scanFileImports(allocator, pm, pkg, source_file);
        }
    }
}

/// Scan a single file for import statements
fn scanFileImports(
    allocator: std.mem.Allocator,
    pm: *cot.framework.package.PackageManager,
    pkg: *cot.framework.package.Package,
    file_path: []const u8,
) !void {
    const file = std.fs.cwd().openFile(file_path, .{}) catch return;
    defer file.close();

    const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch return;
    defer allocator.free(content);

    // Simple import scanning - look for `import "..."` patterns
    var i: usize = 0;
    while (i < content.len) {
        // Look for 'import' keyword
        if (i + 6 < content.len and std.mem.eql(u8, content[i..][0..6], "import")) {
            i += 6;

            // Skip whitespace
            while (i < content.len and (content[i] == ' ' or content[i] == '\t')) {
                i += 1;
            }

            // Check for quote
            if (i < content.len and content[i] == '"') {
                i += 1;
                const start = i;

                // Find end quote
                while (i < content.len and content[i] != '"') {
                    i += 1;
                }

                if (i < content.len) {
                    const import_path = content[start..i];

                    // Resolve import to package name
                    if (try pm.resolveImport(pkg.name, import_path)) |dep_name| {
                        // Don't add self-dependency
                        if (!std.mem.eql(u8, dep_name, pkg.name)) {
                            try pkg.addDependency(dep_name);
                        }
                    }
                }
            }
        }
        i += 1;
    }
}

fn compileFile(backing_allocator: std.mem.Allocator, filename: []const u8, output_file: ?[]const u8) !void {
    // Try to dispatch to external frontend
    if (output_file) |of| {
        if (try tryDispatchToFrontend(backing_allocator, filename, &[_][]const u8{ "compile", "-o", of })) {
            return;
        }
    } else {
        if (try tryDispatchToFrontend(backing_allocator, filename, &[_][]const u8{"compile"})) {
            return;
        }
    }

    // Use arena allocator for all compilation phases - enables bulk deallocation,
    // better cache locality, and faster allocation (bump pointer)
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const compiler = cot.compiler;
    const DiagnosticCollector = compiler.DiagnosticCollector;
    const formatter = compiler.formatter;
    const diagnostics = compiler.diagnostics;

    // Initialize diagnostic collector
    var collector = DiagnosticCollector.init(allocator);
    // No defer collector.deinit() needed - arena handles cleanup

    // Create NodeStore and StringInterner for parsing all files
    var strings = cot.base.StringInterner.init(allocator);
    // No defer strings.deinit() needed - arena handles cleanup

    var store = cot.ast.NodeStore.init(allocator, &strings);
    // No defer store.deinit() needed - arena handles cleanup

    // Get base directory for resolving relative imports
    const base_dir = std.fs.path.dirname(filename) orelse ".";

    // Track which files we've already parsed to avoid duplicates
    var parsed_files = std.StringHashMap(void).init(allocator);

    // Collect all statements from all files (imports first, then main file)
    var all_stmts: std.ArrayListUnmanaged(cot.ast.StmtIdx) = .{};

    // Parse main file first to discover imports
    const main_stmts = try parseFileIntoStore(allocator, filename, &store, &strings, &collector);
    try parsed_files.put(filename, {});

    // If we have errors after parsing main file, stop
    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    }

    // Extract imports from main file and parse them (recursively)
    const imports = try extractImports(allocator, &store, &strings, main_stmts);

    // Read source file for error display
    const source_content = std.fs.cwd().readFileAlloc(allocator, filename, 1024 * 1024) catch "";

    // Parse each imported file (in order, non-recursive for now)
    for (imports) |import_info| {
        const import_path = try resolveImportPath(allocator, base_dir, import_info.name);

        // Skip if already parsed
        if (parsed_files.contains(import_path)) continue;

        // Check if file exists
        std.fs.cwd().access(import_path, .{}) catch {
            // Print error with source location
            try printStderr("\x1b[1;31merror\x1b[0m: cannot find import '\x1b[1m{s}\x1b[0m'\n", .{import_info.name});
            try printStderr("  \x1b[1;34m-->\x1b[0m {s}:{d}:{d}\n", .{ filename, import_info.line, import_info.column });
            try printStderr("   \x1b[1;34m|\x1b[0m\n", .{});

            // Show the source line if we have it
            if (source_content.len > 0) {
                var line_num: u32 = 1;
                var line_start: usize = 0;
                var line_end: usize = 0;
                for (source_content, 0..) |c, idx| {
                    if (c == '\n') {
                        if (line_num == import_info.line) {
                            line_end = idx;
                            break;
                        }
                        line_num += 1;
                        line_start = idx + 1;
                    }
                }
                if (line_end == 0) line_end = source_content.len;
                const line_content = source_content[line_start..line_end];
                try printStderr("\x1b[1;34m{d:>3} |\x1b[0m {s}\n", .{ import_info.line, line_content });
                try printStderr("   \x1b[1;34m|\x1b[0m \x1b[1;31m", .{});
                // Print caret at column position
                var col: u32 = 1;
                while (col < import_info.column) : (col += 1) {
                    try printStderr(" ", .{});
                }
                try printStderr("^\x1b[0m\n", .{});
            }

            try printStderr("\n\x1b[1;36mhint\x1b[0m: if '{s}' is a workspace package, use '\x1b[1mcot build\x1b[0m' instead\n", .{import_info.name});
            return error.CompilationFailed;
        };

        // Parse the imported file
        const import_stmts = try parseFileIntoStore(allocator, import_path, &store, &strings, &collector);
        try parsed_files.put(import_path, {});

        // Add imported statements FIRST (so their types are defined before main file)
        try all_stmts.appendSlice(allocator, import_stmts);

        if (collector.hasErrors()) {
            formatter.printToStderr(&collector, .{ .use_color = true });
            return error.CompilationFailed;
        }
    }

    // Add main file statements AFTER imports
    try all_stmts.appendSlice(allocator, main_stmts);

    const top_level = all_stmts.items;

    // Run compile-time evaluation (resolve comptime if, evaluate const)
    var evaluator = cot.comptime_eval.Evaluator.init(allocator, &store, &strings);
    defer evaluator.deinit();
    evaluator.setSourceFile(filename);

    const processed_stmts = evaluator.process(top_level) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "Compile-time evaluation error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    defer allocator.free(processed_stmts);

    // Lower AST to IR using NodeStore directly (with details for better error messages)
    const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{ .source_file = filename });
    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                // Use file path from error detail if available (for imports), otherwise use main filename
                const error_file = detail.file_path orelse filename;
                collector.addError(
                    .E300_undefined_label,
                    error_file,
                    diagnostics.SourceRange.fromLoc(detail.line, detail.column),
                    "{s}",
                    .{detail.message},
                );
            } else {
                collector.addError(
                    .E300_undefined_label,
                    filename,
                    diagnostics.SourceRange.none,
                    "IR lowering error: {}",
                    .{e.kind},
                );
            }
            formatter.printToStderr(&collector, .{ .use_color = true });
            return error.CompilationFailed;
        },
    };
    // No defer ir_module.deinit() needed - arena handles cleanup

    // Type check the IR
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(allocator, &collector, ir_module, filename);
    type_checker.check();

    // Print all diagnostics (errors and warnings)
    if (collector.hasErrors() or collector.hasWarnings()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
    }

    if (collector.hasErrors()) {
        return error.CompilationFailed;
    }

    // Run optimization passes
    _ = cot.ir_optimize.optimize(ir_module, .{});

    // Emit bytecode from IR
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
    // No defer emitter.deinit() needed - arena handles cleanup

    var mod = emitter.emit(ir_module) catch |err| {
        if (emitter.getLastError()) |ctx| {
            collector.addError(
                .E300_undefined_label,
                filename,
                diagnostics.SourceRange.none,
                "{s}",
                .{ctx.message},
            );
            if (ctx.detail.len > 0) {
                try printStderr("  Note: {s}\n", .{ctx.detail});
            }
        } else {
            collector.addError(
                .E300_undefined_label,
                filename,
                diagnostics.SourceRange.none,
                "Bytecode emission error: {}",
                .{err},
            );
        }
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    // No defer mod.deinit() needed - arena handles cleanup

    // Set source file for runtime error reporting
    mod.source_file = try allocator.dupe(u8, filename);

    // Determine output filename
    const out_name = if (output_file) |of|
        of
    else blk: {
        // Get the base filename without extension
        const basename = std.fs.path.basename(filename);
        const base_no_ext = if (std.mem.lastIndexOf(u8, basename, ".")) |dot_idx|
            basename[0..dot_idx]
        else
            basename;

        // Try to find workspace root for output directory
        var config_loader = cot.framework.ConfigLoader.init(allocator);

        const cwd = std.fs.cwd().realpathAlloc(allocator, ".") catch {
            // Fallback to .cot-out in current directory
            break :blk try std.fmt.allocPrint(allocator, ".cot-out/{s}.cbo", .{base_no_ext});
        };
        defer allocator.free(cwd);

        if (config_loader.findWorkspaceRoot(cwd) catch null) |workspace_root| {
            defer allocator.free(workspace_root);
            // Output to workspace's .cot-out/.standalone/
            break :blk try std.fmt.allocPrint(allocator, "{s}/.cot-out/.standalone/{s}.cbo", .{ workspace_root, base_no_ext });
        } else {
            // No workspace, output to .cot-out/ in current directory
            break :blk try std.fmt.allocPrint(allocator, ".cot-out/{s}.cbo", .{base_no_ext});
        }
    };
    defer if (output_file == null) allocator.free(out_name);

    // Create output directory if needed
    if (std.fs.path.dirname(out_name)) |dir| {
        std.fs.cwd().makePath(dir) catch {};
    }

    // Write bytecode file
    const out_file = std.fs.cwd().createFile(out_name, .{}) catch |err| {
        try printStderr("Error: Could not create output file '{s}': {}\n", .{ out_name, err });
        return;
    };
    defer out_file.close();

    var write_buffer: [4096]u8 = undefined;
    var buffered_writer = out_file.writer(&write_buffer);

    mod.serialize(&buffered_writer.interface) catch |err| {
        try printStderr("Error: Failed to write bytecode: {}\n", .{err});
        return;
    };
    buffered_writer.interface.flush() catch |err| {
        try printStderr("Error: Failed to flush bytecode: {}\n", .{err});
        return;
    };

    try printStdout("Compiled: {s} -> {s}\n", .{ filename, out_name });
}

/// Run inline tests in a Cot source file
fn runTests(allocator: std.mem.Allocator, filename: []const u8, filter: ?[]const u8) !void {
    // Try to dispatch to external frontend
    if (filter) |f| {
        var filter_arg_buf: [256]u8 = undefined;
        const filter_arg = std.fmt.bufPrint(&filter_arg_buf, "--filter={s}", .{f}) catch {
            try printStderr("Error: filter name too long\n", .{});
            return;
        };
        if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{ "test", filter_arg })) {
            return;
        }
    } else {
        if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{"test"})) {
            return;
        }
    }

    const compiler = cot.compiler;
    const DiagnosticCollector = compiler.DiagnosticCollector;
    const formatter = compiler.formatter;
    const diagnostics = compiler.diagnostics;

    // Initialize diagnostic collector
    var collector = DiagnosticCollector.init(allocator);
    defer collector.deinit();

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // Cache source for diagnostic context
    collector.cacheSource(filename, source) catch {};

    // Tokenize
    var lex = cot.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        collector.addError(
            .E001_invalid_character,
            filename,
            diagnostics.SourceRange.none,
            "Lexer error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };
    defer allocator.free(tokens);

    // Create NodeStore and StringInterner for new parser
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    // Register file for error tracking
    const file_id = store.registerFile(filename) catch .unknown;
    store.setCurrentFile(file_id);

    // Parse
    var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();
    const top_level = parse.parse() catch |err| {
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
        if (collector.error_count == 0) {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.none,
                "Parser error: {}",
                .{err},
            );
        }
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };

    // If we have errors after parsing, stop and report
    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    }

    // Run compile-time evaluation (resolve comptime if, evaluate const)
    var evaluator = cot.comptime_eval.Evaluator.init(allocator, &store, &strings);
    defer evaluator.deinit();
    evaluator.setSourceFile(filename);

    const processed_stmts = evaluator.process(top_level) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "Compile-time evaluation error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };
    defer allocator.free(processed_stmts);

    // Lower AST to IR with detailed error reporting
    const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{ .source_file = filename });
    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                collector.addError(
                    .E300_undefined_label,
                    filename,
                    diagnostics.SourceRange.none,
                    "IR lowering error: {} - {s} ({s})",
                    .{ e.kind, detail.message, detail.context },
                );
            } else {
                collector.addError(
                    .E300_undefined_label,
                    filename,
                    diagnostics.SourceRange.none,
                    "IR lowering error: {}",
                    .{e.kind},
                );
            }
            formatter.printToStderr(&collector, .{ .use_color = true });
            return;
        },
    };
    defer allocator.destroy(ir_module);
    defer ir_module.deinit();

    // Discover test functions
    var tests_found: usize = 0;
    var tests_passed: usize = 0;
    var tests_failed: usize = 0;

    try printStdout("\nRunning tests in {s}:\n", .{filename});

    for (ir_module.functions.items) |func| {
        if (func.is_test) {
            const test_name = func.test_name orelse func.name;

            // Apply filter if specified
            if (filter) |f| {
                if (!std.mem.eql(u8, test_name, f)) {
                    continue;
                }
            }

            tests_found += 1;

            // Emit bytecode for this test function
            var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
            defer emitter.deinit();

            // Create a minimal module with just this test
            // Note: We only deinit the functions list, not the functions themselves
            // (they're owned by ir_module)
            var test_ir = cot.ir.Module.init(allocator, filename);
            defer test_ir.functions.deinit(allocator);

            test_ir.addFunction(func) catch {
                try printStdout("  \x1b[31m✗\x1b[0m {s}: failed to compile\n", .{test_name});
                tests_failed += 1;
                continue;
            };

            var mod = emitter.emit(&test_ir) catch {
                try printStdout("  \x1b[31m✗\x1b[0m {s}: failed to emit bytecode\n", .{test_name});
                tests_failed += 1;
                continue;
            };
            defer mod.deinit();

            // Run the test
            var vm = cot.bytecode.VM.init(allocator);
            defer vm.deinit();

            // Register stdlib functions including assert
            var stdlib = cot.native.Stdlib.init(allocator, &vm.native_registry);
            defer stdlib.deinit();
            stdlib.loadAll() catch {};

            const start_time = std.time.nanoTimestamp();
            vm.execute(&mod) catch |err| {
                const duration_ns: u64 = @intCast(std.time.nanoTimestamp() - start_time);
                const duration_ms = duration_ns / 1_000_000;
                try printStdout("  \x1b[31m✗\x1b[0m {s} ({d}ms): {}\n", .{ test_name, duration_ms, err });
                tests_failed += 1;
                continue;
            };

            const duration_ns: u64 = @intCast(std.time.nanoTimestamp() - start_time);
            const duration_ms = duration_ns / 1_000_000;
            try printStdout("  \x1b[32m✓\x1b[0m {s} ({d}ms)\n", .{ test_name, duration_ms });
            tests_passed += 1;
        }
    }

    // Print summary
    try printStdout("\n", .{});
    if (tests_found == 0) {
        try printStdout("No tests found.\n", .{});
    } else {
        try printStdout("{d} passed, {d} failed ({d} total)\n", .{ tests_passed, tests_failed, tests_found });
    }

    // Return error if any tests failed
    if (tests_failed > 0) {
        return error.TestsFailed;
    }
}

fn dumpIR(backing_allocator: std.mem.Allocator, filename: []const u8) !void {
    // Try to dispatch to external frontend
    if (try tryDispatchToFrontend(backing_allocator, filename, &[_][]const u8{"dump-ir"})) {
        return;
    }

    // Use arena allocator for all compilation phases
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);

    // Tokenize
    var lex = cot.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        try printStderr("Lexer error: {}\n", .{err});
        return;
    };

    // Create StringInterner and NodeStore for new parser API
    var strings = cot.base.StringInterner.init(allocator);
    var store = cot.ast.NodeStore.init(allocator, &strings);

    // Register file for error tracking
    const file_id = store.registerFile(filename) catch .unknown;
    store.setCurrentFile(file_id);

    // Parse
    var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    const top_level = parse.parse() catch |err| {
        try printStderr("Parser error: {}\n", .{err});
        return;
    };

    // Run compile-time evaluation (resolve comptime if, evaluate const)
    var evaluator = cot.comptime_eval.Evaluator.init(allocator, &store, &strings);
    defer evaluator.deinit();
    evaluator.setSourceFile(filename);

    const processed_stmts = evaluator.process(top_level) catch |err| {
        try printStderr("Compile-time evaluation error: {}\n", .{err});
        return;
    };
    defer allocator.free(processed_stmts);

    // Lower to IR using detailed error reporting
    const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{ .source_file = filename });
    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                const error_file = detail.file_path orelse filename;
                try printStderr("{s}:{d}:{d}: error: {s}\n", .{ error_file, detail.line, detail.column, detail.message });
                if (detail.context.len > 0) {
                    try printStderr("  while {s}\n", .{detail.context});
                }
            } else {
                try printStderr("IR lowering error: {}\n", .{e.kind});
            }
            return;
        },
    };

    // Print IR
    var output: std.ArrayList(u8) = .{};

    var printer = cot.ir_printer.Printer.init(output.writer(allocator).any());
    printer.printModule(ir_module) catch |err| {
        try printStderr("IR print error: {}\n", .{err});
        return;
    };

    // Write to stdout
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.writeAll(output.items);
    try stdout.flush();
}

fn disasmFile(backing_allocator: std.mem.Allocator, filename: []const u8) !void {
    // Try to dispatch to external frontend
    if (try tryDispatchToFrontend(backing_allocator, filename, &[_][]const u8{"disasm"})) {
        return;
    }

    // Use arena allocator for all compilation phases
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);

    var mod: cot.bytecode.Module = undefined;

    // Check if it's a bytecode file or source file
    if (std.mem.endsWith(u8, filename, ".cbo") or
        std.mem.endsWith(u8, filename, ".clb") or
        std.mem.endsWith(u8, filename, ".cbr"))
    {
        // Deserialize bytecode
        var fbs = std.io.fixedBufferStream(source);
        mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
            try printStderr("Error: Invalid bytecode file: {}\n", .{err});
            return;
        };
    } else {
        // Compile source to bytecode first using IR pipeline
        var lex = cot.lexer.Lexer.init(source);
        const tokens = lex.tokenize(allocator) catch |err| {
            try printStderr("Lexer error: {}\n", .{err});
            return;
        };

        // Create StringInterner and NodeStore for new parser API
        var strings = cot.base.StringInterner.init(allocator);
        var store = cot.ast.NodeStore.init(allocator, &strings);

        // Register file for error tracking
        const file_id = store.registerFile(filename) catch .unknown;
        store.setCurrentFile(file_id);

        var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
        const top_level = parse.parse() catch |err| {
            try printStderr("Parser error: {}\n", .{err});
            return;
        };

        // Run compile-time evaluation (resolve comptime if, evaluate const)
        var evaluator = cot.comptime_eval.Evaluator.init(allocator, &store, &strings);
        defer evaluator.deinit();
        evaluator.setSourceFile(filename);

        const processed_stmts = evaluator.process(top_level) catch |err| {
            try printStderr("Compile-time evaluation error: {}\n", .{err});
            return;
        };
        defer allocator.free(processed_stmts);

        // Lower to IR using detailed error reporting
        const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{ .source_file = filename });
        const ir_module = switch (lower_result) {
            .ok => |module| module,
            .err => |e| {
                if (e.detail) |detail| {
                    const error_file = detail.file_path orelse filename;
                    try printStderr("{s}:{d}:{d}: error: {s}\n", .{ error_file, detail.line, detail.column, detail.message });
                    if (detail.context.len > 0) {
                        try printStderr("  while {s}\n", .{detail.context});
                    }
                } else {
                    try printStderr("IR lowering error: {}\n", .{e.kind});
                }
                return;
            },
        };

        // Run optimization passes
        _ = cot.ir_optimize.optimize(ir_module, .{});

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);

        mod = emitter.emit(ir_module) catch |err| {
            if (emitter.getLastError()) |ctx| {
                try printStderr("Error: {s}\n", .{ctx.message});
                if (ctx.detail.len > 0) {
                    try printStderr("  Note: {s}\n", .{ctx.detail});
                }
            } else {
                try printStderr("Bytecode emission error: {}\n", .{err});
            }
            return;
        };
    }
    // No defer mod.deinit() needed - arena handles cleanup

    // Disassemble
    var output: std.ArrayList(u8) = .{};

    var disasm = cot.bytecode.Disassembler.init(&mod, output.writer(allocator));
    disasm.disassembleModule() catch |err| {
        try printStderr("Disassembly error: {}\n", .{err});
        return;
    };

    // Write to stdout
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.writeAll(output.items);
    try stdout.flush();
}

fn runRepl(allocator: std.mem.Allocator) !void {
    _ = allocator;
    try printStdout("Cot REPL v{s}\n", .{cot.version});
    try printErr("REPL not yet implemented.\n");
    try printErr("Use: cot <file.cot> to run a Cot program\n");
}

test "main module loads" {
    try std.testing.expect(true);
}
