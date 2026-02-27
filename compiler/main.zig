//! Cot Bootstrap Compiler - Entry point and module exports.

const std = @import("std");

// Frontend modules
pub const token = @import("frontend/token.zig");
pub const source = @import("frontend/source.zig");
pub const errors = @import("frontend/errors.zig");
pub const scanner = @import("frontend/scanner.zig");
pub const ast = @import("frontend/ast.zig");
pub const parser = @import("frontend/parser.zig");
pub const types = @import("frontend/types.zig");
pub const checker = @import("frontend/checker.zig");
pub const ir = @import("frontend/ir.zig");
pub const lower = @import("frontend/lower.zig");
pub const formatter = @import("frontend/formatter.zig");
pub const ssa_builder = @import("frontend/ssa_builder.zig");

// SSA modules
pub const ssa_op = @import("ssa/op.zig");
pub const ssa_value = @import("ssa/value.zig");
pub const ssa_block = @import("ssa/block.zig");
pub const ssa_func = @import("ssa/func.zig");
pub const ssa_dom = @import("ssa/dom.zig");
pub const ssa_test_helpers = @import("ssa/test_helpers.zig");
pub const ssa_debug = @import("ssa/debug.zig");
pub const ssa_schedule = @import("ssa/passes/schedule.zig");
pub const ssa_lower_wasm = @import("ssa/passes/lower_wasm.zig");
pub const ssa_layout = @import("ssa/passes/layout.zig");

// Debug and pipeline
pub const pipeline_debug = @import("pipeline_debug.zig");
pub const driver = @import("driver.zig");
pub const cli = @import("cli.zig");
pub const project = @import("project.zig");
pub const lsp_main = @import("lsp/main.zig");
pub const mcp_main = @import("lsp/mcp_main.zig");

// Native codegen (AOT compiler path)
// Cranelift-style pipeline: Wasm → CLIF IR → MachInst → ARM64/x64
// See CRANELIFT_PORT_MASTER_PLAN.md for architecture details.
pub const native_dwarf = @import("codegen/native/dwarf.zig");
pub const native_macho = @import("codegen/native/macho.zig");
pub const native_elf = @import("codegen/native/elf.zig");
pub const native_wasm_parser = @import("codegen/native/wasm_parser.zig");

// Wasm codegen
pub const wasm = @import("codegen/wasm.zig");
pub const wasm_opcodes = @import("codegen/wasm_opcodes.zig");
pub const wasm_encode = @import("codegen/wasm_encode.zig");
pub const wasm_gen = @import("codegen/wasm_gen.zig");
pub const test_runtime = @import("codegen/test_runtime.zig");
pub const bench_runtime = @import("codegen/bench_runtime.zig");
pub const file_watcher = @import("file_watcher.zig");
pub const js_glue = @import("codegen/js_glue.zig");

const Target = @import("frontend/target.zig").Target;
const Driver = driver.Driver;

/// Resolve input file: if explicit file given, use it. Otherwise try cot.json.
/// Returns the input file path or exits with an error message.
fn resolveInputFile(allocator: std.mem.Allocator, explicit: []const u8, usage: []const u8) []const u8 {
    if (explicit.len > 0) return explicit;

    // Try cot.json in current directory.
    // Read the file ourselves (don't use loadConfig which frees the buffer
    // that parsed JSON strings may reference).
    const json_text = std.fs.cwd().readFileAlloc(allocator, "cot.json", 64 * 1024) catch |e| {
        if (e == error.FileNotFound) {
            std.debug.print("Error: No input file\n{s}\n", .{usage});
            std.process.exit(1);
        }
        std.debug.print("Error: Failed to read cot.json: {any}\n", .{e});
        std.process.exit(1);
    };
    // Don't free json_text — parsed strings reference it (arena allocator reclaims all at exit)

    const parsed = std.json.parseFromSlice(project.ProjectConfig, allocator, json_text, .{
        .ignore_unknown_fields = true,
    }) catch {
        std.debug.print("Error: Failed to parse cot.json\n", .{});
        std.process.exit(1);
    };

    if (parsed.value.main) |main_file| {
        return main_file;
    } else {
        std.debug.print("Error: cot.json found but has no \"main\" field\n", .{});
        std.process.exit(1);
    }
}

/// Find runtime library in known locations.
fn findRuntimePath(allocator: std.mem.Allocator, tgt: Target) ![]const u8 {
    const runtime_name: []const u8 = if (tgt.os == .linux) "cot_runtime_linux.o" else "cot_runtime.o";
    const search_paths = [_][]const u8{ "runtime/", "../runtime/" };

    for (search_paths) |dir_path| {
        const rel_path = try std.fmt.allocPrint(allocator, "{s}{s}", .{ dir_path, runtime_name });
        defer allocator.free(rel_path);
        if (std.fs.cwd().access(rel_path, .{})) |_| {
            return try allocator.dupe(u8, rel_path);
        } else |_| {}
    }

    // Try relative to executable
    var exe_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fs.selfExeDirPath(&exe_dir_buf)) |dir| {
        const runtime_rel = try std.fmt.allocPrint(allocator, "../../runtime/{s}", .{runtime_name});
        defer allocator.free(runtime_rel);
        const full_path = try std.fs.path.join(allocator, &.{ dir, runtime_rel });
        defer allocator.free(full_path);
        if (std.fs.cwd().access(full_path, .{})) |_| {
            return try allocator.dupe(u8, full_path);
        } else |_| {}
    } else |_| {}

    return error.RuntimeNotFound;
}

pub fn main() !void {
    pipeline_debug.initGlobal();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const command = cli.parseArgs(allocator) orelse {
        std.process.exit(1);
    };

    switch (command) {
        .build => |opts| buildCommand(allocator, opts),
        .run => |opts| runCommand(allocator, opts),
        .@"test" => |opts| testCommand(allocator, opts),
        .bench => |opts| benchCommand(allocator, opts),
        .check => |opts| checkCommand(allocator, opts),
        .lint => |opts| lintCommand(allocator, opts),
        .fmt => |opts| fmtCommand(allocator, opts),
        .doc => |opts| docCommand(allocator, opts),
        .task => |opts| taskCommand(allocator, opts),
        .info => infoCommand(allocator),
        .init => |opts| initCommand(allocator, opts),
        .lsp => lsp_main.run(allocator),
        .mcp => mcp_main.run(allocator),
        .version => cli.printVersion(),
        .help => |opts| cli.printHelp(opts.subcommand),
    }
}

fn buildCommand(allocator: std.mem.Allocator, opts: cli.BuildOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot build <file.cot> [-o name] [--target=<t>]");
    const compile_target = opts.target;
    const output_name = opts.output_name orelse blk: {
        // Output to cot-out/ directory (like zig → zig-out/)
        const stem = cli.deriveOutputName(allocator, input_file, compile_target) catch {
            std.debug.print("Error: Failed to derive output name\n", .{});
            std.process.exit(1);
        };
        std.fs.cwd().makePath("cot-out") catch {
            std.debug.print("Error: Failed to create cot-out/ directory\n", .{});
            std.process.exit(1);
        };
        if (opts.lib) {
            // Shared library: lib<name>.dylib (macOS) or lib<name>.so (Linux)
            const ext: []const u8 = if (compile_target.os == .linux) ".so" else ".dylib";
            break :blk std.fmt.allocPrint(allocator, "cot-out/lib{s}{s}", .{ stem, ext }) catch {
                std.debug.print("Error: Allocation failed\n", .{});
                std.process.exit(1);
            };
        }
        break :blk std.fmt.allocPrint(allocator, "cot-out/{s}", .{stem}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    };

    if (opts.watch) {
        // Build subprocess argv: cot build <file> [-o name] [--target=X] (no --watch)
        var argv = std.ArrayListUnmanaged([]const u8){};
        const self_path = getSelfPath(allocator);
        argv.append(allocator, self_path) catch {};
        argv.append(allocator, "build") catch {};
        argv.append(allocator, input_file) catch {};
        if (opts.output_name) |o| {
            argv.append(allocator, "-o") catch {};
            argv.append(allocator, o) catch {};
        }
        watchLoop(allocator, input_file, argv.items);
    } else {
        compileAndLinkFull(allocator, input_file, output_name, compile_target, false, false, null, false, null, null, opts.release, false, opts.lib, opts.direct_native);
    }
}

fn runCommand(allocator: std.mem.Allocator, opts: cli.RunOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot run <file.cot> [--target=<t>] [-- args...]");

    if (opts.watch) {
        var argv = std.ArrayListUnmanaged([]const u8){};
        const self_path = getSelfPath(allocator);
        argv.append(allocator, self_path) catch {};
        argv.append(allocator, "run") catch {};
        argv.append(allocator, input_file) catch {};
        watchLoop(allocator, input_file, argv.items);
        return;
    }

    runOnce(allocator, input_file, opts.target, opts.program_args, opts.release, opts.direct_native);
}

fn runOnce(allocator: std.mem.Allocator, input_file: []const u8, compile_target: Target, program_args: []const []const u8, release: bool, direct_native: bool) void {
    // Compile to temp directory
    const tmp_dir = "/tmp/cot-run";
    std.fs.cwd().makePath(tmp_dir) catch {
        std.debug.print("Error: Failed to create temp directory {s}\n", .{tmp_dir});
        std.process.exit(1);
    };

    const stem = blk: {
        const basename = std.fs.path.basename(input_file);
        break :blk if (std.mem.endsWith(u8, basename, ".cot"))
            basename[0 .. basename.len - 4]
        else
            basename;
    };
    const tmp_output = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, stem }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };

    compileAndLinkFull(allocator, input_file, tmp_output, compile_target, false, true, null, false, null, null, release, false, false, direct_native);

    // Build argv: wasmtime for wasm targets, direct execution for native
    const run_path = if (compile_target.isWasm())
        std.fmt.allocPrint(allocator, "{s}.wasm", .{tmp_output}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        }
    else
        tmp_output;

    var run_args = std.ArrayListUnmanaged([]const u8){};
    if (compile_target.isWasm()) {
        run_args.append(allocator, "wasmtime") catch {};
        run_args.append(allocator, "-W") catch {};
        run_args.append(allocator, "gc=y") catch {};
        run_args.append(allocator, run_path) catch {};
    } else {
        run_args.append(allocator, run_path) catch {};
    }
    for (program_args) |parg| {
        run_args.append(allocator, parg) catch {};
    }

    var child = std.process.Child.init(run_args.items, allocator);
    // stdin/stdout/stderr default to .Inherit
    const result = child.spawnAndWait() catch |e| {
        if (compile_target.isWasm()) {
            std.debug.print("Error: Failed to run wasmtime (is it installed?): {any}\n", .{e});
        } else {
            std.debug.print("Error: Failed to run program: {any}\n", .{e});
        }
        cleanup(tmp_dir);
        std.process.exit(1);
    };

    // Clean up temp
    cleanup(tmp_dir);

    // Forward exit code
    switch (result) {
        .Exited => |code| std.process.exit(code),
        .Signal => |sig| {
            std.debug.print("Program killed by signal: {d}\n", .{sig});
            std.process.exit(1);
        },
        else => std.process.exit(1),
    }
}

fn testCommand(allocator: std.mem.Allocator, opts: cli.TestOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot test <file.cot> [--target=<t>] [--filter=<str>] [--verbose]");

    if (opts.watch) {
        var argv = std.ArrayListUnmanaged([]const u8){};
        const self_path = getSelfPath(allocator);
        argv.append(allocator, self_path) catch {};
        argv.append(allocator, "test") catch {};
        argv.append(allocator, input_file) catch {};
        if (opts.filter) |f| {
            const filter_arg = std.fmt.allocPrint(allocator, "--filter={s}", .{f}) catch "";
            argv.append(allocator, filter_arg) catch {};
        }
        watchLoop(allocator, input_file, argv.items);
        return;
    }

    // Compile to temp directory
    const tmp_dir = "/tmp/cot-run";
    std.fs.cwd().makePath(tmp_dir) catch {
        std.debug.print("Error: Failed to create temp directory {s}\n", .{tmp_dir});
        std.process.exit(1);
    };

    const stem = blk: {
        const basename = std.fs.path.basename(input_file);
        break :blk if (std.mem.endsWith(u8, basename, ".cot"))
            basename[0 .. basename.len - 4]
        else
            basename;
    };
    const tmp_output = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, stem }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };

    compileAndLinkFull(allocator, input_file, tmp_output, opts.target, true, true, opts.filter, false, null, null, opts.release, opts.fail_fast, false, false);

    // Run the test: wasmtime for wasm targets, direct execution for native
    const run_path = if (opts.target.isWasm())
        std.fmt.allocPrint(allocator, "{s}.wasm", .{tmp_output}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        }
    else
        tmp_output;

    const argv: []const []const u8 = if (opts.target.isWasm())
        &.{ "wasmtime", "-W", "gc=y", run_path }
    else
        &.{run_path};

    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Ignore;
    const result = child.spawnAndWait() catch |e| {
        if (opts.target.isWasm()) {
            std.debug.print("Error: Failed to run wasmtime (is it installed?): {any}\n", .{e});
        } else {
            std.debug.print("Error: Failed to run tests: {any}\n", .{e});
        }
        cleanup(tmp_dir);
        std.process.exit(1);
    };

    cleanup(tmp_dir);

    switch (result) {
        .Exited => |code| {
            // On Wasm, cot_write is a stub so the binary can't produce output.
            // Print host-side summary based on exit code (native has Deno-style output).
            if (opts.target.isWasm()) {
                if (code == 0) {
                    std.debug.print("\x1b[1;32mok\x1b[0m | all tests passed\n", .{});
                } else {
                    std.debug.print("\x1b[1;31mFAILED\x1b[0m | {d} test(s) failed\n", .{code});
                }
            }
            std.process.exit(code);
        },
        .Signal => |sig| {
            std.debug.print("Test program killed by signal: {d}\n", .{sig});
            std.process.exit(1);
        },
        else => std.process.exit(1),
    }
}

fn benchCommand(allocator: std.mem.Allocator, opts: cli.BenchOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot bench <file.cot> [--target=<t>] [--filter=<str>] [--n=<count>]");

    // Compile to temp directory
    const tmp_dir = "/tmp/cot-run";
    std.fs.cwd().makePath(tmp_dir) catch {
        std.debug.print("Error: Failed to create temp directory {s}\n", .{tmp_dir});
        std.process.exit(1);
    };

    const stem = blk: {
        const basename = std.fs.path.basename(input_file);
        break :blk if (std.mem.endsWith(u8, basename, ".cot"))
            basename[0 .. basename.len - 4]
        else
            basename;
    };
    const tmp_output = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, stem }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };

    compileAndLinkFull(allocator, input_file, tmp_output, opts.target, false, true, null, true, opts.filter, opts.n, false, false, false, false);

    // Run the benchmark: wasmtime for wasm targets, direct execution for native
    const run_path = if (opts.target.isWasm())
        std.fmt.allocPrint(allocator, "{s}.wasm", .{tmp_output}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        }
    else
        tmp_output;

    const argv: []const []const u8 = if (opts.target.isWasm())
        &.{ "wasmtime", "-W", "gc=y", run_path }
    else
        &.{run_path};

    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Ignore;
    const result = child.spawnAndWait() catch |e| {
        if (opts.target.isWasm()) {
            std.debug.print("Error: Failed to run wasmtime (is it installed?): {any}\n", .{e});
        } else {
            std.debug.print("Error: Failed to run benchmarks: {any}\n", .{e});
        }
        cleanup(tmp_dir);
        std.process.exit(1);
    };

    cleanup(tmp_dir);

    switch (result) {
        .Exited => |code| {
            if (opts.target.isWasm()) {
                if (code == 0) {
                    std.debug.print("\x1b[1;32mok\x1b[0m | benchmarks completed\n", .{});
                } else {
                    std.debug.print("\x1b[1;31mFAILED\x1b[0m | benchmark exited with code {d}\n", .{code});
                }
            }
            std.process.exit(code);
        },
        .Signal => |sig| {
            std.debug.print("Benchmark killed by signal: {d}\n", .{sig});
            std.process.exit(1);
        },
        else => std.process.exit(1),
    }
}

fn checkCommand(allocator: std.mem.Allocator, opts: cli.CheckOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot check <file.cot> [--target=<t>]");

    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(opts.target);

    compile_driver.checkFile(input_file) catch |e| {
        if (e != error.ParseError and e != error.TypeCheckError) {
            std.debug.print("Check failed: {any}\n", .{e});
        }
        std.process.exit(1);
    };
    // Deno pattern: on success, print "Check <file>" in green and exit 0
    const is_tty = std.posix.isatty(2);
    const green = if (is_tty) "\x1b[1;32m" else "";
    const reset = if (is_tty) "\x1b[0m" else "";
    std.debug.print("{s}Check{s} {s}\n", .{ green, reset, input_file });
}

fn lintCommand(allocator: std.mem.Allocator, opts: cli.LintOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot lint <file.cot> [--target=<t>]");

    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(opts.target);

    const warning_count = compile_driver.lintFile(input_file) catch |e| {
        if (e != error.ParseError and e != error.TypeCheckError) {
            std.debug.print("Lint failed: {any}\n", .{e});
        }
        std.process.exit(1);
    };

    const is_tty = std.posix.isatty(2);
    const green = if (is_tty) "\x1b[1;32m" else "";
    const reset = if (is_tty) "\x1b[0m" else "";

    if (warning_count == 0) {
        std.debug.print("{s}Checked{s} {s}\n", .{ green, reset, input_file });
    } else {
        std.debug.print("\nFound {d} warning{s}\n", .{ warning_count, if (warning_count == 1) "" else "s" });
    }
}

fn fmtCommand(allocator: std.mem.Allocator, opts: cli.FmtOptions) void {
    const input = resolveInputFile(allocator, opts.input_file, "Usage: cot fmt <file-or-dir> [--check] [--stdout]");

    // Check if input is a directory
    const stat = std.fs.cwd().statFile(input) catch {
        std.debug.print("Error: Cannot access {s}\n", .{input});
        std.process.exit(1);
    };

    if (stat.kind == .directory) {
        if (opts.stdout) {
            std.debug.print("Error: --stdout cannot be used with directories\n", .{});
            std.process.exit(1);
        }
        fmtDirectory(allocator, input, opts.check);
    } else {
        fmtSingleFile(allocator, input, opts);
    }
}

fn fmtDirectory(allocator: std.mem.Allocator, dir_path: []const u8, check: bool) void {
    var unformatted_count: u32 = 0;
    var formatted_count: u32 = 0;
    var file_count: u32 = 0;

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch {
        std.debug.print("Error: Cannot open directory {s}\n", .{dir_path});
        std.process.exit(1);
    };
    defer dir.close();

    var walker = dir.walk(allocator) catch {
        std.debug.print("Error: Failed to walk directory\n", .{});
        std.process.exit(1);
    };
    defer walker.deinit();

    while (walker.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.basename, ".cot")) continue;

        const full_path = std.fs.path.join(allocator, &.{ dir_path, entry.path }) catch continue;
        defer allocator.free(full_path);

        file_count += 1;
        const result = fmtOneFile(allocator, full_path);
        switch (result) {
            .changed => {
                if (check) {
                    std.debug.print("{s}\n", .{full_path});
                    unformatted_count += 1;
                } else {
                    std.fs.cwd().writeFile(.{ .sub_path = full_path, .data = result.output().? }) catch |e| {
                        std.debug.print("Error: Failed to write {s}: {any}\n", .{ full_path, e });
                        return;
                    };
                    formatted_count += 1;
                    std.debug.print("Formatted {s}\n", .{full_path});
                }
            },
            .unchanged => {},
            .parse_error => {
                std.debug.print("Warning: Skipping {s} (parse error)\n", .{full_path});
            },
        }
        result.deinit(allocator);
    }

    if (check) {
        if (unformatted_count > 0) {
            std.debug.print("\n{d} file{s} need formatting\n", .{ unformatted_count, if (unformatted_count == 1) @as([]const u8, "") else "s" });
            std.process.exit(1);
        }
    } else {
        if (formatted_count > 0) {
            std.debug.print("\nFormatted {d} of {d} file{s}\n", .{ formatted_count, file_count, if (file_count == 1) @as([]const u8, "") else "s" });
        }
    }
}

const FmtResult = union(enum) {
    changed: []const u8,
    unchanged: void,
    parse_error: void,

    fn output(self: FmtResult) ?[]const u8 {
        return switch (self) {
            .changed => |o| o,
            else => null,
        };
    }

    fn deinit(self: FmtResult, allocator: std.mem.Allocator) void {
        switch (self) {
            .changed => |o| allocator.free(o),
            else => {},
        }
    }
};

fn fmtOneFile(allocator: std.mem.Allocator, file_path: []const u8) FmtResult {
    const fmt_mod = @import("frontend/formatter.zig");

    const source_text = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch return .parse_error;
    defer allocator.free(source_text);

    const comments = fmt_mod.collectComments(allocator, source_text) catch return .parse_error;
    defer allocator.free(comments);

    var src = source.Source.init(allocator, file_path, source_text);
    defer src.deinit();
    var err_reporter = errors.ErrorReporter.init(&src, null);
    var tree = ast.Ast.init(allocator);
    defer tree.deinit();
    var scan = scanner.Scanner.initWithErrors(&src, &err_reporter);
    var parser_inst = parser.Parser.init(allocator, &scan, &tree, &err_reporter);
    parser_inst.parseFile() catch return .parse_error;
    if (err_reporter.hasErrors()) return .parse_error;

    var fmtr = fmt_mod.Formatter.init(allocator, &tree, source_text, comments);
    const formatted = fmtr.format() catch return .parse_error;

    if (std.mem.eql(u8, source_text, formatted)) {
        allocator.free(formatted);
        return .unchanged;
    }
    return .{ .changed = formatted };
}

fn fmtSingleFile(allocator: std.mem.Allocator, input_file: []const u8, opts: cli.FmtOptions) void {
    const result = fmtOneFile(allocator, input_file);
    defer result.deinit(allocator);

    switch (result) {
        .parse_error => {
            std.debug.print("Error: Parse failed for {s}\n", .{input_file});
            std.process.exit(1);
        },
        .unchanged => {
            if (opts.stdout) {
                // --stdout with unchanged file: still write original to stdout
                const source_text = std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024) catch {
                    std.process.exit(1);
                };
                defer allocator.free(source_text);
                const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
                stdout_file.writeAll(source_text) catch {};
            }
            // --check with unchanged: exit 0 (already formatted)
        },
        .changed => |formatted| {
            if (opts.stdout) {
                const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
                stdout_file.writeAll(formatted) catch {
                    std.debug.print("Error: Failed to write to stdout\n", .{});
                    std.process.exit(1);
                };
            } else if (opts.check) {
                std.debug.print("{s}\n", .{input_file});
                std.process.exit(1);
            } else {
                std.fs.cwd().writeFile(.{ .sub_path = input_file, .data = formatted }) catch |e| {
                    std.debug.print("Error: Failed to write {s}: {any}\n", .{ input_file, e });
                    std.process.exit(1);
                };
                std.debug.print("Formatted {s}\n", .{input_file});
            }
        },
    }
}

fn docCommand(allocator: std.mem.Allocator, opts: cli.DocOptions) void {
    const input_file = resolveInputFile(allocator, opts.input_file, "Usage: cot doc <file.cot> [-o dir]");

    // Read + parse source
    const source_text = std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024) catch |e| {
        std.debug.print("Error: Failed to read {s}: {any}\n", .{ input_file, e });
        std.process.exit(1);
    };
    defer allocator.free(source_text);

    var src = source.Source.init(allocator, input_file, source_text);
    defer src.deinit();
    var err_reporter = errors.ErrorReporter.init(&src, null);
    var tree = ast.Ast.init(allocator);
    defer tree.deinit();
    var scan = scanner.Scanner.initWithErrors(&src, &err_reporter);
    var parser_inst = parser.Parser.init(allocator, &scan, &tree, &err_reporter);
    parser_inst.parseFile() catch {
        std.debug.print("Error: Parse failed for {s}\n", .{input_file});
        std.process.exit(1);
    };
    if (err_reporter.hasErrors()) {
        std.process.exit(1);
    }

    const file = tree.file orelse {
        std.debug.print("Error: No declarations found in {s}\n", .{input_file});
        std.process.exit(1);
    };

    // Derive module name from filename
    const basename = std.fs.path.basename(input_file);
    const module_name = if (std.mem.endsWith(u8, basename, ".cot")) basename[0 .. basename.len - 4] else basename;

    // Generate HTML
    var html = std.ArrayListUnmanaged(u8){};
    defer html.deinit(allocator);

    // HTML head — append raw CSS (no fmt, CSS braces conflict with std.fmt)
    html.appendSlice(allocator, "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n<title>") catch {};
    html.appendSlice(allocator, module_name) catch {};
    html.appendSlice(allocator, " \xe2\x80\x94 Cot Documentation</title>\n<style>\n:root { --bg: #fff; --fg: #1a1a1a; --code-bg: #f5f5f5; --border: #e0e0e0; --accent: #0066cc; --accent-light: #e6f0ff; }\n@media (prefers-color-scheme: dark) {\n  :root { --bg: #1a1a2e; --fg: #e0e0e0; --code-bg: #16213e; --border: #2a2a4a; --accent: #4da6ff; --accent-light: #1a2a4a; }\n}\n* { margin: 0; padding: 0; box-sizing: border-box; }\nbody { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: var(--bg); color: var(--fg); max-width: 900px; margin: 0 auto; padding: 2rem; line-height: 1.6; }\nh1 { font-size: 2rem; margin-bottom: 0.5rem; }\nh2 { font-size: 1.4rem; margin-top: 2rem; margin-bottom: 0.5rem; padding-bottom: 0.3rem; border-bottom: 1px solid var(--border); }\nh3 { font-size: 1.1rem; margin-top: 1.5rem; }\n.decl { margin: 1rem 0; padding: 1rem; border: 1px solid var(--border); border-radius: 6px; }\n.decl:hover { border-color: var(--accent); }\n.sig { font-family: 'SF Mono', Monaco, monospace; font-size: 0.9rem; background: var(--code-bg); padding: 0.5rem 0.75rem; border-radius: 4px; overflow-x: auto; }\n.doc { margin-top: 0.5rem; color: var(--fg); }\n.tag { display: inline-block; font-size: 0.75rem; padding: 0.1rem 0.5rem; border-radius: 3px; background: var(--accent-light); color: var(--accent); margin-right: 0.5rem; }\n.fields { margin-top: 0.5rem; padding-left: 1.5rem; }\n.field { margin: 0.25rem 0; font-family: monospace; font-size: 0.9rem; }\n.field-doc { color: #666; font-family: sans-serif; font-size: 0.85rem; margin-left: 1rem; }\na { color: var(--accent); text-decoration: none; }\na:hover { text-decoration: underline; }\n.toc { margin: 1rem 0; padding: 1rem; background: var(--code-bg); border-radius: 6px; }\n.toc ul { list-style: none; padding-left: 1rem; }\n.toc li { margin: 0.2rem 0; }\n</style>\n</head>\n<body>\n") catch {};

    // Title
    html.appendSlice(allocator, "<h1>") catch {};
    html.appendSlice(allocator, module_name) catch {};
    html.appendSlice(allocator, "</h1>\n<p>Source: <code>") catch {};
    html.appendSlice(allocator, input_file) catch {};
    html.appendSlice(allocator, "</code></p>\n") catch {};

    // Table of contents + sections
    var structs = std.ArrayListUnmanaged([]const u8){};
    var functions = std.ArrayListUnmanaged([]const u8){};
    var constants = std.ArrayListUnmanaged([]const u8){};
    var enums_list = std.ArrayListUnmanaged([]const u8){};
    var traits = std.ArrayListUnmanaged([]const u8){};
    defer structs.deinit(allocator);
    defer functions.deinit(allocator);
    defer constants.deinit(allocator);
    defer enums_list.deinit(allocator);
    defer traits.deinit(allocator);

    // Categorize declarations
    for (file.decls) |decl_idx| {
        const node = tree.getNode(decl_idx) orelse continue;
        const decl = node.asDecl() orelse continue;
        switch (decl) {
            .struct_decl => |s| structs.append(allocator, s.name) catch {},
            .fn_decl => |f| {
                if (!std.mem.eql(u8, f.name, "main")) functions.append(allocator, f.name) catch {};
            },
            .var_decl => |v| {
                if (v.is_const) constants.append(allocator, v.name) catch {};
            },
            .enum_decl => |e| enums_list.append(allocator, e.name) catch {},
            .trait_decl => |t| traits.append(allocator, t.name) catch {},
            else => {},
        }
    }

    // TOC
    html.appendSlice(allocator, "<nav class=\"toc\">\n<strong>Contents</strong>\n<ul>\n") catch {};
    if (structs.items.len > 0) {
        html.appendSlice(allocator, "<li><a href=\"#structs\">Structs</a></li>\n") catch {};
    }
    if (enums_list.items.len > 0) {
        html.appendSlice(allocator, "<li><a href=\"#enums\">Enums</a></li>\n") catch {};
    }
    if (traits.items.len > 0) {
        html.appendSlice(allocator, "<li><a href=\"#traits\">Traits</a></li>\n") catch {};
    }
    if (functions.items.len > 0) {
        html.appendSlice(allocator, "<li><a href=\"#functions\">Functions</a></li>\n") catch {};
    }
    if (constants.items.len > 0) {
        html.appendSlice(allocator, "<li><a href=\"#constants\">Constants</a></li>\n") catch {};
    }
    html.appendSlice(allocator, "</ul>\n</nav>\n") catch {};

    // Emit each declaration section
    if (structs.items.len > 0) {
        html.appendSlice(allocator, "<h2 id=\"structs\">Structs</h2>\n") catch {};
        for (file.decls) |decl_idx| {
            const node = tree.getNode(decl_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl != .struct_decl) continue;
            const s = decl.struct_decl;
            emitDeclHtml(allocator, &html, "struct", s.name, s.doc_comment, null);
            // Fields
            if (s.fields.len > 0) {
                html.appendSlice(allocator, "<div class=\"fields\">\n") catch {};
                for (s.fields) |f| {
                    html.appendSlice(allocator, "<div class=\"field\">") catch {};
                    html.appendSlice(allocator, f.name) catch {};
                    html.appendSlice(allocator, "</div>\n") catch {};
                    if (f.doc_comment.len > 0) {
                        html.appendSlice(allocator, "<div class=\"field-doc\">") catch {};
                        html.appendSlice(allocator, f.doc_comment) catch {};
                        html.appendSlice(allocator, "</div>\n") catch {};
                    }
                }
                html.appendSlice(allocator, "</div>\n") catch {};
            }
            // Associated methods from impl blocks
            for (file.decls) |impl_idx| {
                const impl_node = tree.getNode(impl_idx) orelse continue;
                const impl_decl = impl_node.asDecl() orelse continue;
                if (impl_decl != .impl_block) continue;
                if (!std.mem.eql(u8, impl_decl.impl_block.type_name, s.name)) continue;
                for (impl_decl.impl_block.methods) |m_idx| {
                    const m_node = tree.getNode(m_idx) orelse continue;
                    const m_decl = m_node.asDecl() orelse continue;
                    if (m_decl != .fn_decl) continue;
                    const m = m_decl.fn_decl;
                    const sig = buildFnSig(allocator, m);
                    emitDeclHtml(allocator, &html, "method", m.name, m.doc_comment, sig);
                }
            }
            html.appendSlice(allocator, "</div>\n") catch {};
        }
    }

    if (enums_list.items.len > 0) {
        html.appendSlice(allocator, "<h2 id=\"enums\">Enums</h2>\n") catch {};
        for (file.decls) |decl_idx| {
            const node = tree.getNode(decl_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl != .enum_decl) continue;
            const e = decl.enum_decl;
            emitDeclHtml(allocator, &html, "enum", e.name, e.doc_comment, null);
            // Variants
            if (e.variants.len > 0) {
                html.appendSlice(allocator, "<div class=\"fields\">\n") catch {};
                for (e.variants) |v| {
                    html.appendSlice(allocator, "<div class=\"field\">") catch {};
                    html.appendSlice(allocator, v.name) catch {};
                    html.appendSlice(allocator, "</div>\n") catch {};
                }
                html.appendSlice(allocator, "</div>\n") catch {};
            }
            html.appendSlice(allocator, "</div>\n") catch {};
        }
    }

    if (traits.items.len > 0) {
        html.appendSlice(allocator, "<h2 id=\"traits\">Traits</h2>\n") catch {};
        for (file.decls) |decl_idx| {
            const node = tree.getNode(decl_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl != .trait_decl) continue;
            const t = decl.trait_decl;
            emitDeclHtml(allocator, &html, "trait", t.name, t.doc_comment, null);
            html.appendSlice(allocator, "</div>\n") catch {};
        }
    }

    if (functions.items.len > 0) {
        html.appendSlice(allocator, "<h2 id=\"functions\">Functions</h2>\n") catch {};
        for (file.decls) |decl_idx| {
            const node = tree.getNode(decl_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl != .fn_decl) continue;
            const f = decl.fn_decl;
            if (std.mem.eql(u8, f.name, "main")) continue;
            const sig = buildFnSig(allocator, f);
            emitDeclHtml(allocator, &html, "fn", f.name, f.doc_comment, sig);
            html.appendSlice(allocator, "</div>\n") catch {};
        }
    }

    if (constants.items.len > 0) {
        html.appendSlice(allocator, "<h2 id=\"constants\">Constants</h2>\n") catch {};
        for (file.decls) |decl_idx| {
            const node = tree.getNode(decl_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl != .var_decl) continue;
            const v = decl.var_decl;
            if (!v.is_const) continue;
            emitDeclHtml(allocator, &html, "const", v.name, v.doc_comment, null);
            html.appendSlice(allocator, "</div>\n") catch {};
        }
    }

    // Footer
    html.appendSlice(allocator, "\n<hr style=\"margin-top:2rem\">\n<p style=\"font-size:0.8rem;color:#888\">Generated by <code>cot doc</code></p>\n</body>\n</html>\n") catch {};

    // Write output
    std.fs.cwd().makePath(opts.output) catch |e| {
        std.debug.print("Error: Failed to create output directory '{s}': {any}\n", .{ opts.output, e });
        std.process.exit(1);
    };
    const out_path = std.fmt.allocPrint(allocator, "{s}/{s}.html", .{ opts.output, module_name }) catch {
        std.debug.print("Error: allocation failed\n", .{});
        std.process.exit(1);
    };
    std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = html.items }) catch |e| {
        std.debug.print("Error: Failed to write {s}: {any}\n", .{ out_path, e });
        std.process.exit(1);
    };
    std.debug.print("Generated {s}\n", .{out_path});
}

fn buildFnSig(allocator: std.mem.Allocator, f: ast.FnDecl) []const u8 {
    var sig = std.ArrayListUnmanaged(u8){};
    sig.appendSlice(allocator, "fn ") catch return "";
    sig.appendSlice(allocator, f.name) catch return "";
    sig.append(allocator, '(') catch return "";
    for (f.params, 0..) |p, i| {
        if (i > 0) sig.appendSlice(allocator, ", ") catch {};
        sig.appendSlice(allocator, p.name) catch {};
    }
    sig.append(allocator, ')') catch return "";
    return sig.toOwnedSlice(allocator) catch "";
}

fn emitDeclHtml(allocator: std.mem.Allocator, html: *std.ArrayListUnmanaged(u8), tag: []const u8, name: []const u8, doc: []const u8, sig: ?[]const u8) void {
    html.appendSlice(allocator, "<div class=\"decl\">\n") catch {};
    html.appendSlice(allocator, "<span class=\"tag\">") catch {};
    html.appendSlice(allocator, tag) catch {};
    html.appendSlice(allocator, "</span>\n") catch {};
    html.appendSlice(allocator, "<h3 id=\"") catch {};
    html.appendSlice(allocator, name) catch {};
    html.appendSlice(allocator, "\">") catch {};
    html.appendSlice(allocator, name) catch {};
    html.appendSlice(allocator, "</h3>\n") catch {};
    if (sig) |s| {
        html.appendSlice(allocator, "<pre class=\"sig\">") catch {};
        html.appendSlice(allocator, s) catch {};
        html.appendSlice(allocator, "</pre>\n") catch {};
    }
    if (doc.len > 0) {
        html.appendSlice(allocator, "<p class=\"doc\">") catch {};
        // Replace newlines with <br> for multi-line docs
        var start: usize = 0;
        while (start < doc.len) {
            var end = start;
            while (end < doc.len and doc[end] != '\n') : (end += 1) {}
            html.appendSlice(allocator, doc[start..end]) catch {};
            if (end < doc.len) html.appendSlice(allocator, "<br>") catch {};
            start = if (end < doc.len) end + 1 else end;
        }
        html.appendSlice(allocator, "</p>\n") catch {};
    }
}

fn taskCommand(allocator: std.mem.Allocator, opts: cli.TaskOptions) void {
    // Load cot.json
    var loaded = project.loadConfig(allocator, null) catch {
        std.debug.print("Error: Failed to read cot.json\n", .{});
        std.process.exit(1);
    } orelse {
        std.debug.print("Error: No cot.json found in current directory.\n\nRun 'cot init' to create a project.\n", .{});
        std.process.exit(1);
    };
    defer loaded.deinit();

    // No task name: list available tasks
    if (opts.task_name.len == 0) {
        if (loaded.listTasks(allocator)) |tasks| {
            if (tasks.len == 0) {
                std.debug.print("No tasks defined in cot.json.\n\nAdd tasks to cot.json:\n\n  {{\n    \"tasks\": {{\n      \"dev\": \"cot run --watch src/main.cot\"\n    }}\n  }}\n", .{});
                return;
            }
            std.debug.print("Available tasks:\n\n", .{});
            for (tasks) |name| {
                // Look up the command for display
                if (loaded.getTask(allocator, name)) |cmd| {
                    std.debug.print("  {s}", .{name});
                    // Pad to 16 chars
                    var pad: usize = 0;
                    while (pad + name.len < 16) : (pad += 1) std.debug.print(" ", .{});
                    std.debug.print(" {s}\n", .{cmd});
                } else {
                    std.debug.print("  {s}\n", .{name});
                }
            }
            std.debug.print("\nRun a task with: cot task <name>\n", .{});
        } else {
            std.debug.print("No tasks defined in cot.json.\n\nAdd tasks to cot.json:\n\n  {{\n    \"tasks\": {{\n      \"dev\": \"cot run --watch src/main.cot\"\n    }}\n  }}\n", .{});
        }
        return;
    }

    // Look up the task
    const cmd = loaded.getTask(allocator, opts.task_name) orelse {
        std.debug.print("Error: Unknown task '{s}'\n\nRun 'cot task' to see available tasks.\n", .{opts.task_name});
        std.process.exit(1);
    };

    // Run the command via shell (Deno pattern: spawn shell process)
    const is_tty = std.posix.isatty(2);
    const cyan = if (is_tty) "\x1b[1;36m" else "";
    const reset = if (is_tty) "\x1b[0m" else "";
    std.debug.print("{s}Task {s}{s} $ {s}\n", .{ cyan, opts.task_name, reset, cmd });

    const argv = &[_][]const u8{ "/bin/sh", "-c", cmd };
    var child = std.process.Child.init(argv, allocator);
    const result = child.spawnAndWait() catch |e| {
        std.debug.print("Error: Failed to run task: {any}\n", .{e});
        std.process.exit(1);
    };
    // Forward exit code (same pattern as cot run)
    switch (result) {
        .Exited => |code| std.process.exit(code),
        .Signal => |sig| {
            std.debug.print("Task killed by signal: {d}\n", .{sig});
            std.process.exit(1);
        },
        else => std.process.exit(1),
    }
}

fn infoCommand(allocator: std.mem.Allocator) void {
    const build_options = @import("build_options");
    const version_str = build_options.version;

    // Try to load cot.json
    const json_text = std.fs.cwd().readFileAlloc(allocator, "cot.json", 64 * 1024) catch |e| {
        if (e == error.FileNotFound) {
            std.debug.print("No cot.json found in current directory.\n\nRun 'cot init' to create a project.\n", .{});
            return;
        }
        std.debug.print("Error: Failed to read cot.json: {any}\n", .{e});
        std.process.exit(1);
    };

    const parsed = std.json.parseFromSlice(project.ProjectConfig, allocator, json_text, .{
        .ignore_unknown_fields = true,
    }) catch {
        std.debug.print("Error: Failed to parse cot.json\n", .{});
        std.process.exit(1);
    };
    const config = parsed.value;

    std.debug.print("Cot Project Info\n", .{});
    std.debug.print("================\n", .{});
    if (config.name) |name| {
        std.debug.print("  Name:       {s}\n", .{name});
    }
    if (config.version) |ver| {
        std.debug.print("  Version:    {s}\n", .{ver});
    }
    if (config.main) |main_file| {
        std.debug.print("  Entry:      {s}\n", .{main_file});
    }
    if (config.safe) |safe| {
        std.debug.print("  Safe mode:  {s}\n", .{if (safe) "enabled" else "disabled"});
    }
    std.debug.print("  Compiler:   cot {s}\n", .{version_str});

    // Show target info
    const tgt = Target.native();
    std.debug.print("  Platform:   {s}-{s}\n", .{ @tagName(tgt.arch), @tagName(tgt.os) });
}

// =============================================================================
// Watch mode (Deno pattern: poll-based file watching with debounce)
// Reference: Deno's cli/util/file_watcher.rs, Zig's Build/Watch.zig
// =============================================================================

const CLEAR_SCREEN = "\x1B[H\x1B[2J\x1B[3J"; // Deno: clear screen between runs
const POLL_INTERVAL_NS: u64 = 300_000_000; // 300ms poll interval (Deno uses 200ms debounce)

/// Watch loop: re-invoke cot as a subprocess on file changes (Deno pattern).
/// Uses subprocess to handle compileAndLink calling std.process.exit on errors.
fn watchLoop(allocator: std.mem.Allocator, input_file: []const u8, sub_argv: []const []const u8) void {
    var watcher = file_watcher.FileWatcher.init(allocator);
    defer watcher.deinit();
    watcher.addFileAndSiblings(input_file);

    std.debug.print("Watching for changes... (Ctrl+C to exit)\n\n", .{});

    // Initial run
    runSubprocess(allocator, sub_argv);

    while (true) {
        std.posix.nanosleep(0, POLL_INTERVAL_NS);
        if (watcher.pollForChanges()) {
            std.debug.print("{s}", .{CLEAR_SCREEN});
            runSubprocess(allocator, sub_argv);
            watcher.addFileAndSiblings(input_file);
        }
    }
}

fn getSelfPath(allocator: std.mem.Allocator) []const u8 {
    var self_buf: [std.fs.max_path_bytes]u8 = undefined;
    const self_path = std.fs.selfExePath(&self_buf) catch return "cot";
    return allocator.dupe(u8, self_path) catch "cot";
}

fn runSubprocess(allocator: std.mem.Allocator, argv: []const []const u8) void {
    var child = std.process.Child.init(argv, allocator);
    _ = child.spawnAndWait() catch |e| {
        std.debug.print("Error: Failed to run subprocess: {any}\n", .{e});
    };
}

fn initCommand(allocator: std.mem.Allocator, opts: cli.InitOptions) void {
    // Determine project directory and name
    const project_name = opts.project_name;
    const in_subdir = project_name != null;

    // If a name is given, create the directory
    if (in_subdir) {
        std.fs.cwd().makePath(project_name.?) catch |e| {
            std.debug.print("Error: Failed to create directory '{s}': {any}\n", .{ project_name.?, e });
            std.process.exit(1);
        };
    }

    // Resolve display name (for cot.json "name" field)
    const display_name = project_name orelse blk: {
        // Use current directory name
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const cwd = std.fs.cwd().realpath(".", &buf) catch {
            break :blk "myapp";
        };
        break :blk std.fs.path.basename(cwd);
    };

    // Check if cot.json already exists
    const base_dir = project_name orelse ".";
    const manifest_path = std.fmt.allocPrint(allocator, "{s}/cot.json", .{base_dir}) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    if (std.fs.cwd().access(manifest_path, .{})) |_| {
        std.debug.print("Error: cot.json already exists in {s}\n", .{base_dir});
        std.process.exit(1);
    } else |_| {}

    // Create src/ directory
    const src_dir = std.fmt.allocPrint(allocator, "{s}/src", .{base_dir}) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    std.fs.cwd().makePath(src_dir) catch |e| {
        std.debug.print("Error: Failed to create src/: {any}\n", .{e});
        std.process.exit(1);
    };

    // Write cot.json
    const main_file = if (opts.lib) "src/lib.cot" else "src/main.cot";
    const manifest_content = std.fmt.allocPrint(allocator,
        \\{{
        \\    "name": "{s}",
        \\    "version": "0.1.0",
        \\    "main": "{s}",
        \\    "safe": true
        \\}}
        \\
    , .{ display_name, main_file }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    std.fs.cwd().writeFile(.{ .sub_path = manifest_path, .data = manifest_content }) catch |e| {
        std.debug.print("Error: Failed to write cot.json: {any}\n", .{e});
        std.process.exit(1);
    };

    if (opts.lib) {
        // Write src/lib.cot (library template)
        const lib_path = std.fmt.allocPrint(allocator, "{s}/src/lib.cot", .{base_dir}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
        const lib_content =
            \\fn add(a: i64, b: i64) i64 {
            \\    return a + b
            \\}
            \\
        ;
        std.fs.cwd().writeFile(.{ .sub_path = lib_path, .data = lib_content }) catch |e| {
            std.debug.print("Error: Failed to write src/lib.cot: {any}\n", .{e});
            std.process.exit(1);
        };

        // Write src/lib_test.cot (library tests)
        const lib_test_path = std.fmt.allocPrint(allocator, "{s}/src/lib_test.cot", .{base_dir}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
        const lib_test_content =
            \\import "lib.cot"
            \\
            \\test "add positive numbers" {
            \\    @assertEq(add(2, 3), 5)
            \\}
            \\
            \\test "add with zero" {
            \\    @assertEq(add(0, 42), 42)
            \\}
            \\
        ;
        std.fs.cwd().writeFile(.{ .sub_path = lib_test_path, .data = lib_test_content }) catch |e| {
            std.debug.print("Error: Failed to write src/lib_test.cot: {any}\n", .{e});
            std.process.exit(1);
        };
    } else {
        // Write src/main.cot (executable template)
        const main_path = std.fmt.allocPrint(allocator, "{s}/src/main.cot", .{base_dir}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
        const main_content =
            \\fn main() i64 {
            \\    println("Hello, world!")
            \\    return 0
            \\}
            \\
        ;
        std.fs.cwd().writeFile(.{ .sub_path = main_path, .data = main_content }) catch |e| {
            std.debug.print("Error: Failed to write src/main.cot: {any}\n", .{e});
            std.process.exit(1);
        };

        // Write src/main_test.cot (executable tests)
        const test_path = std.fmt.allocPrint(allocator, "{s}/src/main_test.cot", .{base_dir}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
        const test_content =
            \\test "it works" {
            \\    @assertEq(1 + 1, 2)
            \\}
            \\
        ;
        std.fs.cwd().writeFile(.{ .sub_path = test_path, .data = test_content }) catch |e| {
            std.debug.print("Error: Failed to write src/main_test.cot: {any}\n", .{e});
            std.process.exit(1);
        };
    }

    // Write .gitignore
    const gitignore_path = std.fmt.allocPrint(allocator, "{s}/.gitignore", .{base_dir}) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    const gitignore_content =
        \\# Build output
        \\*.wasm
        \\*.o
        \\
        \\# Editor
        \\.vscode/
        \\.idea/
        \\
    ;
    // Only write .gitignore if it doesn't exist
    if (std.fs.cwd().access(gitignore_path, .{})) |_| {
        // Already exists, skip
    } else |_| {
        std.fs.cwd().writeFile(.{ .sub_path = gitignore_path, .data = gitignore_content }) catch |e| {
            std.debug.print("Error: Failed to write .gitignore: {any}\n", .{e});
            std.process.exit(1);
        };
    }

    // Print success with next steps (Deno pattern: actionable output)
    const test_file = if (opts.lib) "src/lib_test.cot" else "src/main_test.cot";
    if (in_subdir) {
        std.debug.print("Created project '{s}' in {s}/\n\nNext steps:\n", .{ display_name, project_name.? });
        std.debug.print("  cd {s}\n", .{project_name.?});
        if (!opts.lib) {
            std.debug.print("  cot run              # run the project\n", .{});
        }
        std.debug.print("  cot test {s}  # run tests\n", .{test_file});
        std.debug.print("  cot fmt {s}   # format code\n\n", .{main_file});
    } else {
        std.debug.print("Initialized project '{s}'\n\nNext steps:\n", .{display_name});
        if (!opts.lib) {
            std.debug.print("  cot run              # run the project\n", .{});
        }
        std.debug.print("  cot test {s}  # run tests\n", .{test_file});
        std.debug.print("  cot fmt {s}   # format code\n\n", .{main_file});
    }
}

fn cleanup(dir: []const u8) void {
    std.fs.cwd().deleteTree(dir) catch {};
}

/// Core compile + link logic shared by build, run, test, and bench commands.
fn compileAndLink(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    output_name: []const u8,
    compile_target: Target,
    test_mode: bool,
    quiet: bool,
    test_filter: ?[]const u8,
) void {
    compileAndLinkFull(allocator, input_file, output_name, compile_target, test_mode, quiet, test_filter, false, null, null, false, false, false, false);
}

fn compileAndLinkFull(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    output_name: []const u8,
    compile_target: Target,
    test_mode: bool,
    quiet: bool,
    test_filter: ?[]const u8,
    bench_mode: bool,
    bench_filter: ?[]const u8,
    bench_n: ?i64,
    release_mode: bool,
    fail_fast: bool,
    lib_mode: bool,
    direct_native: bool,
) void {
    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(compile_target);
    compile_driver.release_mode = release_mode;
    compile_driver.lib_mode = lib_mode;
    // Direct native path is the default for native targets.
    // The indirect (Wasm → CLIF) path is only used for --target=wasm32.
    // --direct-native flag is kept for backward compat (no-op for native targets).
    compile_driver.direct_native = direct_native or (compile_target.arch != .wasm32);
    if (test_mode) compile_driver.setTestMode(true);
    if (fail_fast) compile_driver.setFailFast(true);
    if (test_filter) |f| compile_driver.setTestFilter(f);
    if (bench_mode) compile_driver.setBenchMode(true);
    if (bench_filter) |f| compile_driver.setBenchFilter(f);
    if (bench_n) |n| compile_driver.setBenchN(n);

    const code = compile_driver.compileFile(input_file) catch |e| {
        if (e == error.NoTestsMatched) {
            // Already printed "0 tests matched filter" — exit success
            std.process.exit(0);
        }
        if (e == error.NoBenchesMatched) {
            std.process.exit(0);
        }
        std.debug.print("Compilation failed: {any}\n", .{e});
        std.process.exit(1);
    };
    defer allocator.free(code);

    // For Wasm target, output .wasm directly (no linking needed)
    if (compile_target.isWasm()) {
        const wasm_path = if (std.mem.endsWith(u8, output_name, ".wasm"))
            allocator.dupe(u8, output_name) catch {
                std.debug.print("Error: Allocation failed\n", .{});
                std.process.exit(1);
            }
        else
            std.fmt.allocPrint(allocator, "{s}.wasm", .{output_name}) catch {
                std.debug.print("Error: Allocation failed\n", .{});
                std.process.exit(1);
            };

        std.fs.cwd().writeFile(.{ .sub_path = wasm_path, .data = code }) catch |e| {
            std.debug.print("Failed to write Wasm file: {any}\n", .{e});
            std.process.exit(1);
        };
        if (!quiet) std.debug.print("Success: {s} ({d} bytes)\n", .{ wasm_path, code.len });

        // Generate JS glue for browser Wasm targets (not WASI)
        if (!compile_target.isWasi()) {
            const js_path = blk: {
                if (std.mem.endsWith(u8, wasm_path, ".wasm")) {
                    break :blk std.fmt.allocPrint(allocator, "{s}.js", .{wasm_path[0 .. wasm_path.len - 5]}) catch {
                        std.debug.print("Error: Allocation failed\n", .{});
                        std.process.exit(1);
                    };
                } else {
                    break :blk std.fmt.allocPrint(allocator, "{s}.js", .{wasm_path}) catch {
                        std.debug.print("Error: Allocation failed\n", .{});
                        std.process.exit(1);
                    };
                }
            };
            const wasm_basename = std.fs.path.basename(wasm_path);
            const glue = js_glue.generate(allocator, wasm_basename) catch {
                std.debug.print("Error: Failed to generate JS glue\n", .{});
                std.process.exit(1);
            };
            std.fs.cwd().writeFile(.{ .sub_path = js_path, .data = glue }) catch |e| {
                std.debug.print("Failed to write JS glue: {any}\n", .{e});
                std.process.exit(1);
            };
            if (!quiet) std.debug.print("Generated: {s}\n", .{js_path});
        }

        return;
    }

    // Native target: write object file and link
    const obj_path = if (std.mem.endsWith(u8, output_name, ".o"))
        allocator.dupe(u8, output_name) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        }
    else
        std.fmt.allocPrint(allocator, "{s}.o", .{output_name}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = code }) catch |e| {
        std.debug.print("Failed to write object file: {any}\n", .{e});
        std.process.exit(1);
    };

    if (std.mem.endsWith(u8, output_name, ".o")) {
        if (!quiet) std.debug.print("Success: {s}\n", .{output_name});
        return;
    }

    // Link with zig cc
    const runtime_path = findRuntimePath(allocator, compile_target) catch null;

    var link_args = std.ArrayListUnmanaged([]const u8){};

    if (compile_target.arch != Target.native().arch or compile_target.os != Target.native().os) {
        const triple: []const u8 = switch (compile_target.os) {
            .linux => if (compile_target.arch == .amd64) "x86_64-linux-gnu" else "aarch64-linux-gnu",
            .macos => if (compile_target.arch == .arm64) "aarch64-macos" else "x86_64-macos",
            .freestanding, .wasi => unreachable, // Handled above with isWasm()
        };
        link_args.appendSlice(allocator, &.{ "zig", "cc", "-target", triple, "-o", output_name, obj_path }) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    } else {
        link_args.appendSlice(allocator, &.{ "zig", "cc", "-o", output_name, obj_path }) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    }

    // Shared library mode: add -shared flag
    if (lib_mode) {
        link_args.append(allocator, "-shared") catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    }

    if (runtime_path) |rp| link_args.append(allocator, rp) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    if (compile_target.os == .macos) {
        link_args.appendSlice(allocator, &.{ "-Wl,-stack_size,0x10000000", "-lSystem" }) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    } else if (compile_target.os == .linux) {
        link_args.append(allocator, "-lc") catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    }

    var child = std.process.Child.init(link_args.items, allocator);
    const result = child.spawnAndWait() catch |e| {
        std.debug.print("Linker failed: {any}\n", .{e});
        std.process.exit(1);
    };

    if (result.Exited == 0) {
        if (std.fs.cwd().openFile(output_name, .{})) |f| {
            defer f.close();
            f.chmod(0o755) catch {};
        } else |_| {}
        // Clean up intermediate .o file after successful link
        std.fs.cwd().deleteFile(obj_path) catch {};
        if (!quiet) std.debug.print("Success: {s}\n", .{output_name});
    } else {
        std.debug.print("Link failed with code: {d}\n", .{result.Exited});
        std.process.exit(1);
    }
}

// Initialize debug before tests
test {
    pipeline_debug.initGlobal();
}

// Include all tests from submodules
test {
    @import("std").testing.refAllDecls(@This());
}

// E2E tests (separate import to avoid circular deps)
test {
    _ = @import("frontend/e2e_test.zig");
    _ = @import("frontend/integration_test.zig");
    _ = @import("codegen/wasm_e2e_test.zig");
    _ = @import("codegen/native_e2e_test.zig");
}

test "main: findRuntimePath returns error when not found" {
    const allocator = std.testing.allocator;
    const result = findRuntimePath(allocator, Target.native());
    try std.testing.expectError(error.RuntimeNotFound, result);
}
