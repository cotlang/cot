//! Cot-DBL Compiler
//!
//! A DBL syntax frontend for the Cot language.
//! Compiles .dbl files using DBL syntax through the standard Cot pipeline.

const std = @import("std");
const cot = @import("cot");
const cot_tui = @import("cot_tui");
const cot_runtime = @import("cot_runtime");
const dbl = @import("root.zig");

const TraceLevel = cot_runtime.trace.TraceLevel;

// Configure std.log for scoped logging (same as main cot)
// Control via COT_LOG environment variable at runtime
pub const std_options: std.Options = .{
    .log_level = .debug,
    .logFn = cotLogFn,
};

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
    const runtime_level = getRuntimeLogLevel();
    if (@intFromEnum(level) > @intFromEnum(runtime_level)) return;

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

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        return;
    }

    var filename: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;
    var command: enum { run, trace, compile, @"test" } = .run;
    var trace_level: TraceLevel = .opcodes;
    var test_filter: ?[]const u8 = null;

    // Parse arguments
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "trace")) {
            command = .trace;
        } else if (std.mem.eql(u8, arg, "compile")) {
            command = .compile;
        } else if (std.mem.eql(u8, arg, "test")) {
            command = .@"test";
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            test_filter = arg["--filter=".len..];
        } else if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output requires a path argument\n", .{});
                return;
            }
            i += 1;
            output_path = args[i];
        } else if (std.mem.startsWith(u8, arg, "--level=")) {
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
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (arg[0] == '-') {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return;
        } else {
            filename = arg;
        }
    }

    if (filename == null) {
        std.debug.print("Error: No input file specified\n", .{});
        printUsage();
        return;
    }

    // Read source file
    const file = std.fs.cwd().openFile(filename.?, .{}) catch |err| {
        std.debug.print("Error: Could not open file '{s}': {}\n", .{ filename.?, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // Initialize extension registry and register TUI extension
    cot.extension.initRegistry(allocator);
    defer cot.extension.deinitRegistry();

    // Register TUI extension for t_* native functions
    cot.extension.registerExtension(cot_tui.extension) catch |err| {
        std.debug.print("Warning: Could not register TUI extension: {}\n", .{err});
    };

    const source_dir = std.fs.path.dirname(filename.?);

    switch (command) {
        .compile => {
            const out_path = output_path orelse blk: {
                // Generate default output path
                const basename = std.fs.path.basename(filename.?);
                const base_no_ext = if (std.mem.lastIndexOf(u8, basename, ".")) |dot|
                    basename[0..dot]
                else
                    basename;
                break :blk std.fmt.allocPrint(allocator, "{s}.cbo", .{base_no_ext}) catch {
                    std.debug.print("Error: Out of memory\n", .{});
                    return;
                };
            };
            defer if (output_path == null) allocator.free(out_path);

            compileToFile(allocator, source, filename.?, out_path) catch |err| {
                std.debug.print("Error: Compilation failed: {}\n", .{err});
                std.process.exit(1);
            };
        },
        .trace => {
            dbl.traceWithRegistryAndPath(allocator, source, source_dir, trace_level) catch |err| {
                std.debug.print("Error: {}\n", .{err});
            };
        },
        .run => {
            dbl.runWithRegistryAndPath(allocator, source, source_dir) catch |err| {
                std.debug.print("Error: {}\n", .{err});
            };
        },
        .@"test" => {
            runTests(allocator, source, filename.?, test_filter) catch |err| {
                if (err == error.TestsFailed) {
                    std.process.exit(1);
                }
                std.debug.print("Error: {}\n", .{err});
            };
        },
    }
}

fn printUsage() void {
    std.debug.print(
        \\Cot-DBL: DBL syntax frontend for Cot
        \\
        \\Usage: cot-dbl <file.dbl> [command] [options]
        \\
        \\Commands:
        \\  (default)            Run the program
        \\  test                 Run inline tests
        \\  trace                Run with execution tracing
        \\  compile              Compile to bytecode file
        \\
        \\Options:
        \\  -o, --output <path>  Output file for compile command
        \\  --filter=<name>      Run only tests matching name (for test command)
        \\  --level=<level>      Trace level: none, routines, opcodes, verbose, full
        \\  -h, --help           Show this help message
        \\
        \\Examples:
        \\  cot-dbl program.dbl                       Run program.dbl
        \\  cot-dbl program.dbl test                  Run all tests in program.dbl
        \\  cot-dbl program.dbl test --filter="my test" Run specific test
        \\  cot-dbl program.dbl trace                 Run with opcode tracing
        \\  cot-dbl program.dbl trace --level=verbose Trace with register state
        \\  cot-dbl program.dbl compile -o out.cbo    Compile to out.cbo
        \\
    , .{});
}

fn compileToFile(allocator: std.mem.Allocator, source: []const u8, source_name: []const u8, output_path: []const u8) !void {
    // Extract module name from source filename
    const basename = std.fs.path.basename(source_name);
    const module_name = if (std.mem.lastIndexOf(u8, basename, ".")) |dot|
        basename[0..dot]
    else
        basename;

    // Compile to bytecode module
    var module = try dbl.compileToModule(allocator, source, module_name);
    defer module.deinit();

    // Create output file
    var out_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        std.debug.print("Error: Could not create output file '{s}': {}\n", .{ output_path, err });
        return error.OutputError;
    };
    defer out_file.close();

    // Serialize bytecode to file
    var write_buffer: [8192]u8 = undefined;
    var buffered = out_file.writer(&write_buffer);

    try module.serialize(&buffered.interface);
    try buffered.interface.flush();

    std.debug.print("Compiled {s} -> {s}\n", .{ source_name, output_path });
}

fn runTests(allocator: std.mem.Allocator, source: []const u8, filename: []const u8, filter: ?[]const u8) !void {
    const ir = cot.ir;
    const bytecode = cot_runtime.bytecode;
    const native = cot_runtime.native;
    const base = cot.base;
    const ast = cot.ast;

    // Tokenize with DBL lexer
    var lex = dbl.Lexer.init(source);
    const tokens = lex.tokenize(allocator) catch |err| {
        std.debug.print("Lexer error: {}\n", .{err});
        return error.CompilationFailed;
    };
    defer allocator.free(tokens);

    // Parse using NodeStore-based parser
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = dbl.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = parse.parse() catch |err| {
        for (parse.errors.items) |e| {
            std.debug.print("Parse error at line {d}: {s}\n", .{ e.token.line, e.message });
        }
        return err;
    };
    defer allocator.free(top_level);

    if (parse.hasErrors()) {
        for (parse.errors.items) |e| {
            std.debug.print("Parse error at line {d}: {s}\n", .{ e.token.line, e.message });
        }
        return error.ParseError;
    }

    // Lower to IR
    const ir_module = cot.ir_lower.lower(allocator, &store, &strings, top_level, filename) catch |err| {
        std.debug.print("IR lowering error: {}\n", .{err});
        return error.LowerError;
    };
    defer allocator.destroy(ir_module);
    defer ir_module.deinit();

    // Discover test functions
    var tests_found: usize = 0;
    var tests_passed: usize = 0;
    var tests_failed: usize = 0;

    std.debug.print("\nRunning tests in {s}:\n", .{filename});

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
            var test_ir = ir.Module.init(allocator, filename);
            defer test_ir.functions.deinit(allocator);

            test_ir.addFunction(func) catch {
                std.debug.print("  \x1b[31m✗\x1b[0m {s}: failed to compile\n", .{test_name});
                tests_failed += 1;
                continue;
            };

            var mod = emitter.emit(&test_ir) catch {
                std.debug.print("  \x1b[31m✗\x1b[0m {s}: failed to emit bytecode\n", .{test_name});
                tests_failed += 1;
                continue;
            };
            defer mod.deinit();

            // Run the test
            var vm = bytecode.VM.init(allocator);
            defer vm.deinit();

            // Register stdlib functions including assert
            var stdlib = native.Stdlib.init(allocator, &vm.native_registry);
            defer stdlib.deinit();
            stdlib.loadAll() catch {};

            const start_time = std.time.nanoTimestamp();
            vm.execute(&mod) catch |err| {
                const duration_ns: u64 = @intCast(std.time.nanoTimestamp() - start_time);
                const duration_ms = duration_ns / 1_000_000;
                std.debug.print("  \x1b[31m✗\x1b[0m {s} ({d}ms): {}\n", .{ test_name, duration_ms, err });
                tests_failed += 1;
                continue;
            };

            const duration_ns: u64 = @intCast(std.time.nanoTimestamp() - start_time);
            const duration_ms = duration_ns / 1_000_000;
            std.debug.print("  \x1b[32m✓\x1b[0m {s} ({d}ms)\n", .{ test_name, duration_ms });
            tests_passed += 1;
        }
    }

    // Print summary
    std.debug.print("\n", .{});
    if (tests_found == 0) {
        std.debug.print("No tests found.\n", .{});
    } else {
        std.debug.print("{d} passed, {d} failed ({d} total)\n", .{ tests_passed, tests_failed, tests_found });
    }

    // Return error if any tests failed
    if (tests_failed > 0) {
        return error.TestsFailed;
    }
}
