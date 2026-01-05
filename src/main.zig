//! Cot - A modern programming language for business applications
//! Business apps with ISAM database, TUI, and project framework
//!
//! Usage:
//!   cot <file.cot>              Run a Cot program (interpreter)
//!   cot run <file.cot|.cbo>     Run a program (auto-detect mode)
//!   cot trace <file.cot|.cbo>   Run with execution tracing enabled
//!   cot compile <file.cot>      Compile to bytecode (.cbo) via IR
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

pub fn main() !void {
    // Install crash handlers FIRST - ensures crash reporting works for all commands
    cot.crash.installHandlers();

    // Use page_allocator as the backing allocator - all memory is reclaimed on process exit
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        try printUsage();
    } else if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        try printVersion();
    } else if (std.mem.eql(u8, command, "repl")) {
        try runRepl(allocator);
    } else if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            try printErr("Error: compile requires a filename\n");
            try printErr("Usage: cot compile <file.cot> [-o output.cbo]\n");
            return;
        }
        // Parse optional arguments
        var output_file: ?[]const u8 = null;
        var i: usize = 3;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "-o") and i + 1 < args.len) {
                output_file = args[i + 1];
                i += 1;
            }
        }
        compileFile(allocator, args[2], output_file) catch |err| {
            if (err == error.CompilationFailed) {
                // Error already printed, just exit with failure code
                std.process.exit(1);
            }
            return err;
        };
    } else if (std.mem.eql(u8, command, "disasm")) {
        if (args.len < 3) {
            try printErr("Error: disasm requires a filename\n");
            try printErr("Usage: cot disasm <file.cot|file.cbo>\n");
            return;
        }
        try disasmFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "dump-ir")) {
        if (args.len < 3) {
            try printErr("Error: dump-ir requires a filename\n");
            try printErr("Usage: cot dump-ir <file.cot>\n");
            return;
        }
        try dumpIR(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "run")) {
        // Parse runtime option (--runtime=zig|rs)
        var selected_runtime: runtime_selector.Runtime = .zig;
        var file_target: ?[]const u8 = null;

        // Parse arguments
        for (args[2..]) |arg| {
            if (std.mem.startsWith(u8, arg, "--runtime=")) {
                const runtime_name = arg[10..];
                if (runtime_selector.Runtime.fromString(runtime_name)) |r| {
                    selected_runtime = r;
                } else {
                    try printStderr("Error: Unknown runtime '{s}'. Use 'zig' or 'rs'.\n", .{runtime_name});
                    return;
                }
            } else if (!std.mem.startsWith(u8, arg, "-")) {
                // Non-option argument is the target file/project
                file_target = arg;
            }
        }

        // Check if this looks like a file or a workspace target
        if (file_target) |target| {
            // If it's a file path (has .cot or .cbo extension), use file runner
            if (std.mem.endsWith(u8, target, ".cot") or std.mem.endsWith(u8, target, ".cbo")) {
                // Use selected runtime
                switch (selected_runtime) {
                    .zig => try runFileAuto(allocator, target),
                    .rs => {
                        // For .cot files, compile first then run with Rust runtime
                        var bytecode_path: []const u8 = undefined;
                        if (std.mem.endsWith(u8, target, ".cot")) {
                            // Compile to temp .cbo file
                            const basename = std.fs.path.stem(target);
                            bytecode_path = try std.fmt.allocPrint(allocator, "/tmp/{s}.cbo", .{basename});
                            defer allocator.free(bytecode_path);
                            compileFile(allocator, target, bytecode_path) catch |err| {
                                if (err == error.CompilationFailed) {
                                    std.process.exit(1);
                                }
                                return err;
                            };
                            runtime_selector.runWithRustRuntime(allocator, bytecode_path) catch |err| {
                                if (err == error.RuntimeNotFound) {
                                    try printErr("Error: Rust runtime (cot-rs) not found.\n");
                                    try printErr("Install it or add to PATH.\n");
                                }
                                return;
                            };
                        } else {
                            // Already .cbo, run directly
                            runtime_selector.runWithRustRuntime(allocator, target) catch |err| {
                                if (err == error.RuntimeNotFound) {
                                    try printErr("Error: Rust runtime (cot-rs) not found.\n");
                                    try printErr("Install it or add to PATH.\n");
                                }
                                return;
                            };
                        }
                    },
                }
                return;
            }
        }

        // Otherwise, try workspace runner (only supports zig runtime for now)
        if (selected_runtime == .rs) {
            try printErr("Error: --runtime=rs only works with file targets, not workspace projects.\n");
            return;
        }
        const options = run_cmd.parseArgs(args[2..]);
        run_cmd.run(allocator, options) catch |err| {
            if (err == error.NoWorkspace) {
                // Fall back to file usage message
                if (args.len < 3) {
                    try printErr("Error: run requires a filename or project target\n");
                    try printErr("Usage: cot run <file.cot|file.cbo|project> [--runtime=zig|rs]\n");
                } else {
                    try printStderr("Error: {}\n", .{err});
                }
            } else if (err != error.NoApps and err != error.ProjectNotFound and err != error.NotCompiled) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "trace")) {
        // Trace command - runs with execution tracing enabled
        if (args.len < 3) {
            try printErr("Error: trace requires a filename\n");
            try printErr("Usage: cot trace <file.cot|file.cbo> [--level=opcodes|routines|verbose]\n");
            return;
        }
        const target = args[2];
        // Parse trace level option
        var trace_level: trace_mod.TraceLevel = .opcodes; // default
        for (args[3..]) |arg| {
            if (std.mem.startsWith(u8, arg, "--level=")) {
                const level_str = arg[8..];
                if (std.mem.eql(u8, level_str, "none")) {
                    trace_level = .none;
                } else if (std.mem.eql(u8, level_str, "routines")) {
                    trace_level = .routines;
                } else if (std.mem.eql(u8, level_str, "opcodes")) {
                    trace_level = .opcodes;
                } else if (std.mem.eql(u8, level_str, "verbose")) {
                    trace_level = .verbose;
                } else if (std.mem.eql(u8, level_str, "full")) {
                    trace_level = .full;
                }
            }
        }
        try traceFileAuto(allocator, target, trace_level);
    } else if (std.mem.eql(u8, command, "debug")) {
        // Interactive debugger command
        if (args.len < 3) {
            try printErr("Error: debug requires a filename\n");
            try printErr("Usage: cot debug <file.cot|file.cbo>\n");
            return;
        }
        try debugFile(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "init")) {
        // Check for help flag
        if (args.len > 2 and std.mem.eql(u8, args[2], "--help")) {
            try init_cmd.printHelp();
            return;
        }
        const options = init_cmd.parseArgs(args[2..]);
        init_cmd.run(allocator, options) catch |err| {
            if (err != error.AlreadyExists) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "new")) {
        // Check for help flag
        if (args.len > 2 and std.mem.eql(u8, args[2], "--help")) {
            try new_cmd.printHelp();
            return;
        }
        const options = new_cmd.parseArgs(args[2..]);
        new_cmd.run(allocator, options) catch |err| {
            if (err != error.MissingName and err != error.NoWorkspace) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "build")) {
        const options = build_cmd.parseArgs(args[2..]);
        build_cmd.run(allocator, options) catch |err| {
            if (err != error.NoWorkspace and err != error.BuildFailed) {
                try printStderr("Error: {}\n", .{err});
            }
            std.process.exit(1);
        };
    } else if (std.mem.eql(u8, command, "convert")) {
        const options = convert_cmd.parseArgs(args[2..]);
        convert_cmd.run(allocator, options) catch |err| {
            if (err != error.NoInputFile and err != error.NotDblFile) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "dev")) {
        // Check for help flag
        if (args.len > 2 and std.mem.eql(u8, args[2], "--help")) {
            try dev_cmd.printHelp();
            return;
        }
        const options = dev_cmd.parseArgs(args[2..]);
        dev_cmd.run(allocator, options) catch |err| {
            if (err != error.NoWorkspace) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "schema")) {
        // Check for help flag
        if (args.len > 2 and (std.mem.eql(u8, args[2], "--help") or std.mem.eql(u8, args[2], "-h"))) {
            try schema_cmd.printHelp();
            return;
        }
        const options = schema_cmd.parseArgs(args[2..]);
        schema_cmd.run(allocator, options) catch |err| {
            if (err != error.NoWorkspace and err != error.SchemaLoadFailed) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "data")) {
        // Check for help flag
        if (args.len > 2 and (std.mem.eql(u8, args[2], "--help") or std.mem.eql(u8, args[2], "-h"))) {
            try data_cmd.printHelp();
            return;
        }
        const options = data_cmd.parseArgs(args[2..]);
        data_cmd.run(allocator, options) catch |err| {
            if (err != error.NoWorkspace) {
                try printStderr("Error: {}\n", .{err});
            }
        };
    } else if (std.mem.eql(u8, command, "test")) {
        if (args.len < 3) {
            try printErr("Error: test requires a filename\n");
            try printErr("Usage: cot test <file.cot> [--filter=<name>]\n");
            return;
        }
        // Parse optional --filter argument
        var filter: ?[]const u8 = null;
        for (args[3..]) |arg| {
            if (std.mem.startsWith(u8, arg, "--filter=")) {
                filter = arg["--filter=".len..];
            }
        }
        runTests(allocator, args[2], filter) catch |err| {
            if (err == error.TestsFailed) {
                // Test failure already reported - exit with non-zero code
                std.process.exit(1);
            }
            return err;
        };
    } else if (std.mem.eql(u8, command, "validate")) {
        // Bytecode validation command
        if (args.len < 3) {
            try printErr("Error: validate requires a filename\n");
            try printErr("Usage: cot validate <file.cbo> [--strict]\n");
            return;
        }
        // Parse options
        var strict = false;
        for (args[3..]) |arg| {
            if (std.mem.eql(u8, arg, "--strict")) {
                strict = true;
            }
        }
        try validateBytecode(allocator, args[2], strict);
    } else {
        // Assume it's a filename - run with interpreter
        try runFile(allocator, command);
    }
}

fn printUsage() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Cot - A modern systems language for business applications
        \\ISAM database, TUI interfaces, and full-stack development
        \\
        \\Usage:
        \\  cot <file.cot>              Run a Cot program (interpreter)
        \\  cot run <file|project>      Run a program or workspace project
        \\  cot trace <file>            Run with execution tracing
        \\  cot debug <file>            Interactive debugger
        \\  cot validate <file.cbo>     Validate bytecode integrity
        \\  cot compile <file.cot>      Compile to bytecode (.cbo)
        \\  cot disasm <file.cot|.cbo>  Disassemble to readable output
        \\  cot dump-ir <file.cot>      Dump IR for debugging
        \\  cot repl                    Start interactive REPL
        \\  cot --help                  Show this help message
        \\  cot --version               Show version information
        \\
        \\Workspace Commands:
        \\  cot init [name]             Initialize a new workspace
        \\  cot new <name>              Create a new app or package
        \\  cot build [project]         Build workspace projects
        \\  cot run [project]           Run a workspace project
        \\  cot dev                     Start development server
        \\  cot schema <subcommand>     Manage database schema
        \\  cot data <subcommand>       Load/unload fixed-width data
        \\  cot convert <file.dbl>      Convert DBL to Cot syntax
        \\
        \\Compile Options:
        \\  -o <file>                   Output file (default: <input>.cbo)
        \\
        \\File Extensions:
        \\  .cot                        Cot source (modern syntax)
        \\  .cbo                        Compiled bytecode object
        \\  .clb                        Compiled library
        \\  .cbr                        Compiled runnable (main entry)
        \\
        \\Note: For DBL syntax (.dbl files), use the cot-dbl frontend.
        \\
        \\Trace Options:
        \\  --level=routines            Trace routine calls only
        \\  --level=opcodes             Trace each opcode (default)
        \\  --level=verbose             Include register state
        \\
        \\Runtime Options:
        \\  --runtime=zig               Use built-in Zig VM (default)
        \\  --runtime=rs                Use Rust runtime (cot-rs)
        \\
        \\Examples:
        \\  cot hello.cot               Run hello.cot with interpreter
        \\  cot compile hello.cot       Compile to hello.cbo
        \\  cot run bin/hello.cbo       Run compiled bytecode
        \\  cot run hello.cbo --runtime=rs  Run with Rust runtime
        \\  cot trace hello.cot         Run with execution trace
        \\  cot init my-company         Create a workspace
        \\  cot new inventory           Create an app
        \\  cot build                   Build all projects
        \\  cot run inventory           Run an app
        \\  cot dev                     Start dev server on :3000
        \\
    );
    try stdout.flush();
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

    cot.run(allocator, source) catch |err| {
        try printStderr("Runtime error: {}\n", .{err});
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

    // Change to the source file's directory so relative paths work correctly
    if (std.fs.path.dirname(filename)) |dir| {
        std.posix.chdir(dir) catch {};
    }
    defer allocator.free(bytes);

    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Register TUI extension when available
    if (comptime build_options.enable_tui) {
        extension.registerExtension(cot.cot_tui.extension) catch {};
    }

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
        try printStderr("VM error: {}\n", .{err});
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

    // Lower AST to IR
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, processed_stmts, filename) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "IR lowering error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    };
    defer ir_module.deinit();

    // Type check
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(allocator, &collector, ir_module, filename);
    type_checker.check();

    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    }

    // Optimize IR
    _ = cot.ir_optimize.optimize(ir_module, .{});

    // Emit bytecode
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
    defer emitter.deinit();

    var mod = emitter.emit(ir_module) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "Bytecode emission error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    defer mod.deinit();

    // Create tracer
    var tracer = trace_mod.Tracer.init(allocator, .{
        .level = level,
        .output = .{ .format = .human, .target = .stderr, .color = true },
        .history_size = 256,
    });
    defer tracer.deinit();

    // Create VM and attach tracer
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    vm.setTracer(&tracer);

    try printStderr("=== Execution Trace ===\n", .{});
    vm.execute(&mod) catch |err| {
        try printStderr("\nVM error: {}\n", .{err});
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

    // Change to the source file's directory so relative paths work correctly
    if (std.fs.path.dirname(filename)) |dir| {
        std.posix.chdir(dir) catch {};
    }

    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Register TUI extension when available
    if (comptime build_options.enable_tui) {
        extension.registerExtension(cot.cot_tui.extension) catch {};
    }

    // Deserialize
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        try printStderr("Error: Invalid bytecode file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer mod.deinit();

    // Create tracer
    var tracer = trace_mod.Tracer.init(allocator, .{
        .level = level,
        .output = .{ .format = .human, .target = .stderr, .color = true },
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
        try printStderr("\nVM error: {}\n", .{err});
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

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return error.CompilationFailed;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    // No defer allocator.free(source) needed - arena handles cleanup

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
        return error.CompilationFailed;
    };
    // No defer allocator.free(tokens) needed - arena handles cleanup

    // Create NodeStore and StringInterner for new parser
    var strings = cot.base.StringInterner.init(allocator);
    // No defer strings.deinit() needed - arena handles cleanup

    var store = cot.ast.NodeStore.init(allocator, &strings);
    // No defer store.deinit() needed - arena handles cleanup

    // Parse
    var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    // No defer parse.deinit() needed - arena handles cleanup
    const top_level = parse.parse() catch |err| {
        // Report parser's collected errors
        for (parse.errors.items) |parse_err| {
            collector.addError(
                .E100_unexpected_token,
                filename,
                diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
        // Also report the fatal error if we have no collected errors
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
        return error.CompilationFailed;
    };

    // Check if parser collected any non-fatal errors
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

    // If we have errors after parsing, stop and report
    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
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
        return error.CompilationFailed;
    };
    defer allocator.free(processed_stmts);

    // Lower AST to IR using NodeStore directly
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, processed_stmts, filename) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "IR lowering error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    // No defer ir_module.deinit() needed - arena handles cleanup

    // Type check the IR
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(allocator, &collector, ir_module, filename);
    type_checker.check();

    // If type checking found errors, report them and stop
    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    }

    // Run optimization passes
    _ = cot.ir_optimize.optimize(ir_module, .{});

    // Emit bytecode from IR
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
    // No defer emitter.deinit() needed - arena handles cleanup

    var mod = emitter.emit(ir_module) catch |err| {
        collector.addError(
            .E300_undefined_label,
            filename,
            diagnostics.SourceRange.none,
            "Bytecode emission error: {}",
            .{err},
        );
        formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    };
    // No defer mod.deinit() needed - arena handles cleanup

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
    const lower_result = cot.ir_lower.lowerWithDetails(allocator, &store, &strings, processed_stmts, filename, .{});
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

    // Lower to IR using NodeStore directly
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, processed_stmts, filename) catch |err| {
        try printStderr("IR lowering error: {}\n", .{err});
        return;
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

        // Lower to IR using NodeStore directly
        const ir_module = cot.ir_lower.lower(allocator, &store, &strings, processed_stmts, filename) catch |err| {
            try printStderr("IR lowering error: {}\n", .{err});
            return;
        };

        // Run optimization passes
        _ = cot.ir_optimize.optimize(ir_module, .{});

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);

        mod = emitter.emit(ir_module) catch |err| {
            try printStderr("Bytecode emission error: {}\n", .{err});
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
