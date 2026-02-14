//! Cot Bootstrap Compiler - Entry point and module exports.

const std = @import("std");

// Core modules
pub const core_types = @import("core/types.zig");
pub const core_errors = @import("core/errors.zig");
pub const core_target = @import("core/target.zig");
pub const core_testing = @import("core/testing.zig");

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

const Target = core_target.Target;
const Driver = driver.Driver;

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
        .fmt => |opts| fmtCommand(allocator, opts),
        .init => |opts| initCommand(allocator, opts),
        .lsp => lsp_main.run(allocator),
        .mcp => mcp_main.run(allocator),
        .version => cli.printVersion(),
        .help => |opts| cli.printHelp(opts.subcommand),
    }
}

fn buildCommand(allocator: std.mem.Allocator, opts: cli.BuildOptions) void {
    const compile_target = opts.target;
    const output_name = opts.output_name orelse (cli.deriveOutputName(allocator, opts.input_file, compile_target) catch {
        std.debug.print("Error: Failed to derive output name\n", .{});
        std.process.exit(1);
    });

    compileAndLink(allocator, opts.input_file, output_name, compile_target, false, false, null);
}

fn runCommand(allocator: std.mem.Allocator, opts: cli.RunOptions) void {
    if (opts.target.isWasm()) {
        std.debug.print("Error: 'cot run' does not support --target=wasm32\nUse 'cot build --target=wasm32' to produce a .wasm file\n", .{});
        std.process.exit(1);
    }

    // Compile to temp directory
    const tmp_dir = "/tmp/cot-run";
    std.fs.cwd().makePath(tmp_dir) catch {
        std.debug.print("Error: Failed to create temp directory {s}\n", .{tmp_dir});
        std.process.exit(1);
    };

    const stem = blk: {
        const basename = std.fs.path.basename(opts.input_file);
        break :blk if (std.mem.endsWith(u8, basename, ".cot"))
            basename[0 .. basename.len - 4]
        else
            basename;
    };
    const tmp_output = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, stem }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };

    compileAndLink(allocator, opts.input_file, tmp_output, opts.target, false, true, null);

    // Run the compiled executable
    var run_args = std.ArrayListUnmanaged([]const u8){};
    run_args.append(allocator, tmp_output) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    for (opts.program_args) |parg| {
        run_args.append(allocator, parg) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        };
    }

    var child = std.process.Child.init(run_args.items, allocator);
    // stdin/stdout/stderr default to .Inherit
    const result = child.spawnAndWait() catch |e| {
        std.debug.print("Error: Failed to run program: {any}\n", .{e});
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
    // Compile to temp directory
    const tmp_dir = "/tmp/cot-run";
    std.fs.cwd().makePath(tmp_dir) catch {
        std.debug.print("Error: Failed to create temp directory {s}\n", .{tmp_dir});
        std.process.exit(1);
    };

    const stem = blk: {
        const basename = std.fs.path.basename(opts.input_file);
        break :blk if (std.mem.endsWith(u8, basename, ".cot"))
            basename[0 .. basename.len - 4]
        else
            basename;
    };
    const tmp_output = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, stem }) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };

    compileAndLink(allocator, opts.input_file, tmp_output, opts.target, true, true, opts.filter);

    // Run the test: wasmtime for wasm targets, direct execution for native
    const run_path = if (opts.target.isWasm())
        std.fmt.allocPrint(allocator, "{s}.wasm", .{tmp_output}) catch {
            std.debug.print("Error: Allocation failed\n", .{});
            std.process.exit(1);
        }
    else
        tmp_output;

    const argv: []const []const u8 = if (opts.target.isWasmGC())
        &.{ "wasmtime", "-W", "gc=y", run_path }
    else if (opts.target.isWasm())
        &.{ "wasmtime", run_path }
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
            if (code == 0) {
                std.debug.print("Tests passed\n", .{});
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

fn fmtCommand(allocator: std.mem.Allocator, opts: cli.FmtOptions) void {
    const fmt_mod = @import("frontend/formatter.zig");

    // Read source file
    const source_text = std.fs.cwd().readFileAlloc(allocator, opts.input_file, 1024 * 1024) catch |e| {
        std.debug.print("Error: Failed to read {s}: {any}\n", .{ opts.input_file, e });
        std.process.exit(1);
    };
    defer allocator.free(source_text);

    // Collect comments from source
    const comments = fmt_mod.collectComments(allocator, source_text) catch {
        std.debug.print("Error: Failed to collect comments\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(comments);

    // Parse
    var src = source.Source.init(allocator, opts.input_file, source_text);
    defer src.deinit();
    var err_reporter = errors.ErrorReporter.init(&src, null);
    var tree = ast.Ast.init(allocator);
    defer tree.deinit();
    var scan = scanner.Scanner.initWithErrors(&src, &err_reporter);
    var parser_inst = parser.Parser.init(allocator, &scan, &tree, &err_reporter);
    parser_inst.parseFile() catch {
        std.debug.print("Error: Parse failed\n", .{});
        std.process.exit(1);
    };
    if (err_reporter.hasErrors()) {
        std.process.exit(1);
    }

    // Format
    var fmtr = fmt_mod.Formatter.init(allocator, &tree, source_text, comments);
    const output = fmtr.format() catch {
        std.debug.print("Error: Format failed\n", .{});
        std.process.exit(1);
    };
    defer allocator.free(output);

    if (opts.write) {
        // Write back to file
        std.fs.cwd().writeFile(.{ .sub_path = opts.input_file, .data = output }) catch |e| {
            std.debug.print("Error: Failed to write {s}: {any}\n", .{ opts.input_file, e });
            std.process.exit(1);
        };
    } else {
        // Write to stdout
        const stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
        stdout.writeAll(output) catch {
            std.debug.print("Error: Failed to write to stdout\n", .{});
            std.process.exit(1);
        };
    }
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
    const manifest_content = std.fmt.allocPrint(allocator,
        \\{{
        \\    "name": "{s}",
        \\    "version": "0.1.0",
        \\    "main": "src/main.cot",
        \\    "safe": true
        \\}}
        \\
    , .{display_name}) catch {
        std.debug.print("Error: Allocation failed\n", .{});
        std.process.exit(1);
    };
    std.fs.cwd().writeFile(.{ .sub_path = manifest_path, .data = manifest_content }) catch |e| {
        std.debug.print("Error: Failed to write cot.json: {any}\n", .{e});
        std.process.exit(1);
    };

    // Write src/main.cot
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

    // Print success
    if (in_subdir) {
        std.debug.print("Created project '{s}' in {s}/\n", .{ display_name, project_name.? });
        std.debug.print("\n  cd {s}\n  cot run src/main.cot\n\n", .{project_name.?});
    } else {
        std.debug.print("Initialized project '{s}'\n", .{display_name});
        std.debug.print("\n  cot run src/main.cot\n\n", .{});
    }
}

fn cleanup(dir: []const u8) void {
    std.fs.cwd().deleteTree(dir) catch {};
}

/// Core compile + link logic shared by build, run, and test commands.
fn compileAndLink(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    output_name: []const u8,
    compile_target: Target,
    test_mode: bool,
    quiet: bool,
    test_filter: ?[]const u8,
) void {
    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(compile_target);
    if (test_mode) compile_driver.setTestMode(true);
    if (test_filter) |f| compile_driver.setTestFilter(f);

    const code = compile_driver.compileFile(input_file) catch |e| {
        if (e == error.NoTestsMatched) {
            // Already printed "0 tests matched filter" — exit success
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
