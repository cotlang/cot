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
pub const lsp_main = @import("lsp/main.zig");

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
        .lsp => lsp_main.run(allocator),
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

    compileAndLink(allocator, opts.input_file, output_name, compile_target, false, false);
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

    compileAndLink(allocator, opts.input_file, tmp_output, opts.target, false, true);

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
    if (opts.target.isWasm()) {
        std.debug.print("Error: 'cot test' does not support --target=wasm32\n", .{});
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

    compileAndLink(allocator, opts.input_file, tmp_output, opts.target, true, true);

    // Run the test executable
    var child = std.process.Child.init(&.{tmp_output}, allocator);
    // stdin/stdout/stderr default to .Inherit
    const result = child.spawnAndWait() catch |e| {
        std.debug.print("Error: Failed to run tests: {any}\n", .{e});
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
) void {
    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(compile_target);
    if (test_mode) compile_driver.setTestMode(true);

    const code = compile_driver.compileFile(input_file) catch |e| {
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
            .freestanding => unreachable, // Handled above with isWasm()
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
