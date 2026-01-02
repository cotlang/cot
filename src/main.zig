//! Cot - A modern programming language for business applications
//! Business apps with ISAM database, TUI, and project framework
//!
//! Usage:
//!   cot <file.cot>              Run a Cot program (interpreter)
//!   cot run <file.cot|.cbo>     Run a program (auto-detect mode)
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

const std = @import("std");
const cot = @import("cot");
const build_options = @import("build_options");
const frontends = @import("frontends.zig");

// Framework commands
const init_cmd = @import("framework/commands/init.zig");
const new_cmd = @import("framework/commands/new.zig");
const build_cmd = @import("framework/commands/build.zig");
const run_cmd = @import("framework/commands/run.zig");
const dev_cmd = @import("framework/commands/dev.zig");
const schema_cmd = @import("framework/commands/schema.zig");
const data_cmd = @import("framework/commands/data.zig");
const convert_cmd = @import("framework/commands/convert.zig");

pub fn main() !void {
    // Install crash handlers FIRST - ensures crash reporting works for all commands
    cot.crash.installHandlers();

    // Use page_allocator directly instead of GPA to avoid leak detection noise.
    // The IR/AST has complex ownership that would require arena allocators to
    // properly clean up. All memory is reclaimed on process exit anyway.
    // TODO: Implement proper arena-based allocation for compilation passes
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
        try compileFile(allocator, args[2], output_file);
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
        // Check if this looks like a file or a workspace target
        if (args.len >= 3) {
            const target = args[2];
            // If it's a file path (has .cot or .cbo extension), use file runner
            if (std.mem.endsWith(u8, target, ".cot") or std.mem.endsWith(u8, target, ".cbo")) {
                try runFileAuto(allocator, target);
                return;
            }
        }
        // Otherwise, try workspace runner
        const options = run_cmd.parseArgs(args[2..]);
        run_cmd.run(allocator, options) catch |err| {
            if (err == error.NoWorkspace) {
                // Fall back to file usage message
                if (args.len < 3) {
                    try printErr("Error: run requires a filename or project target\n");
                    try printErr("Usage: cot run <file.cot|file.cbo|project>\n");
                } else {
                    try printStderr("Error: {}\n", .{err});
                }
            } else if (err != error.NoApps and err != error.ProjectNotFound and err != error.NotCompiled) {
                try printStderr("Error: {}\n", .{err});
            }
        };
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
        \\Examples:
        \\  cot hello.cot               Run hello.cot with interpreter
        \\  cot compile hello.cot       Compile to hello.cbo
        \\  cot run bin/hello.cbo       Run compiled bytecode
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

    vm.execute(&mod) catch |err| {
        try printStderr("VM error: {}\n", .{err});
    };
}

fn compileFile(allocator: std.mem.Allocator, filename: []const u8, output_file: ?[]const u8) !void {
    // Try to dispatch to external frontend
    if (output_file) |of| {
        if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{ "compile", "-o", of })) {
            return;
        }
    } else {
        if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{"compile"})) {
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
        return;
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
        return;
    }

    // Lower AST to IR using NodeStore directly
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, top_level, filename) catch |err| {
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

    // Type check the IR
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(allocator, &collector, ir_module, filename);
    type_checker.check();

    // If type checking found errors, report them and stop
    if (collector.hasErrors()) {
        formatter.printToStderr(&collector, .{ .use_color = true });
        return;
    }

    // Emit bytecode from IR
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
        return;
    };
    defer mod.deinit();

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
        const config = @import("framework/config.zig");
        var config_loader = config.ConfigLoader.init(allocator);

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

fn dumpIR(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Try to dispatch to external frontend
    if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{"dump-ir"})) {
        return;
    }

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // Tokenize
    var lex = cot.lexer.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        try printStderr("Lexer error: {}\n", .{err});
        return;
    };
    defer allocator.free(tokens);

    // Create StringInterner and NodeStore for new parser API
    var strings = cot.base.StringInterner.init(allocator);
    defer strings.deinit();
    var store = cot.ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    // Parse
    var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();
    const top_level = parse.parse() catch |err| {
        try printStderr("Parser error: {}\n", .{err});
        return;
    };

    // Lower to IR using NodeStore directly
    var ir_module = cot.ir_lower.lower(allocator, &store, &strings, top_level, filename) catch |err| {
        try printStderr("IR lowering error: {}\n", .{err});
        return;
    };
    defer ir_module.deinit();

    // Print IR
    var output: std.ArrayList(u8) = .{};
    defer output.deinit(allocator);

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

fn disasmFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    // Try to dispatch to external frontend
    if (try tryDispatchToFrontend(allocator, filename, &[_][]const u8{"disasm"})) {
        return;
    }

    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        try printStderr("Error: Could not open file '{s}': {}\n", .{ filename, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    var mod: cot.bytecode.Module = undefined;
    var owns_module = false;

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
        owns_module = true;
    } else {
        // Compile source to bytecode first using IR pipeline
        var lex = cot.lexer.Lexer.init(source);
        const tokens = lex.tokenize(allocator) catch |err| {
            try printStderr("Lexer error: {}\n", .{err});
            return;
        };
        defer allocator.free(tokens);

        // Create StringInterner and NodeStore for new parser API
        var strings = cot.base.StringInterner.init(allocator);
        defer strings.deinit();
        var store = cot.ast.NodeStore.init(allocator, &strings);
        defer store.deinit();

        var parse = cot.parser.Parser.init(allocator, tokens, &store, &strings);
        defer parse.deinit();
        const top_level = parse.parse() catch |err| {
            try printStderr("Parser error: {}\n", .{err});
            return;
        };

        // Lower to IR using NodeStore directly
        const ir_module = cot.ir_lower.lower(allocator, &store, &strings, top_level, filename) catch |err| {
            try printStderr("IR lowering error: {}\n", .{err});
            return;
        };
        defer ir_module.deinit();

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(allocator);
        defer emitter.deinit();

        mod = emitter.emit(ir_module) catch |err| {
            try printStderr("Bytecode emission error: {}\n", .{err});
            return;
        };
        owns_module = true;
    }
    defer if (owns_module) mod.deinit();

    // Disassemble
    var output: std.ArrayList(u8) = .{};
    defer output.deinit(allocator);

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
