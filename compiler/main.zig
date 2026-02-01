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
pub const ssa_compile = @import("ssa/compile.zig");
pub const ssa_schedule = @import("ssa/passes/schedule.zig");
pub const ssa_lower_wasm = @import("ssa/passes/lower_wasm.zig");
pub const ssa_layout = @import("ssa/passes/layout.zig");

// Debug and pipeline
pub const pipeline_debug = @import("pipeline_debug.zig");
pub const driver = @import("driver.zig");

// Native codegen (AOT compiler path)
// NOTE: Most native codegen was removed for Cranelift-style rewrite.
// See CRANELIFT_PORT_MASTER_PLAN.md for progress.
// Kept files: object file formats and instruction encoding (architecture agnostic)
pub const native_dwarf = @import("codegen/native/dwarf.zig");
pub const native_macho = @import("codegen/native/macho.zig");
pub const native_elf = @import("codegen/native/elf.zig");
pub const native_wasm_parser = @import("codegen/native/wasm_parser.zig");
pub const native_arm64_asm = @import("codegen/native/arm64_asm.zig");
pub const native_amd64_regs = @import("codegen/native/amd64_regs.zig");
pub const native_amd64_asm = @import("codegen/native/amd64_asm.zig");

// Wasm codegen
pub const wasm = @import("codegen/wasm.zig");
pub const wasm_opcodes = @import("codegen/wasm_opcodes.zig");
pub const wasm_encode = @import("codegen/wasm_encode.zig");
pub const wasm_gen = @import("codegen/wasm_gen.zig");

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

    var args = std.process.args();
    _ = args.skip();

    var input_file: ?[]const u8 = null;
    var output_name: []const u8 = "a.out";
    var compile_target = Target.native();
    var test_mode = false;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-o")) {
            output_name = args.next() orelse {
                std.debug.print("Error: -o requires an argument\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "-test") or std.mem.eql(u8, arg, "--test")) {
            test_mode = true;
        } else if (std.mem.startsWith(u8, arg, "--target=")) {
            compile_target = Target.parse(arg[9..]) orelse {
                std.debug.print("Error: Unknown target. Use: wasm32, arm64-macos, amd64-linux\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "--target")) {
            compile_target = Target.parse(args.next() orelse {
                std.debug.print("Error: --target requires an argument\n", .{});
                return;
            }) orelse {
                std.debug.print("Error: Unknown target. Use: wasm32, arm64-macos, amd64-linux\n", .{});
                return;
            };
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            input_file = arg;
        } else {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return;
        }
    }

    const actual_input = input_file orelse {
        std.debug.print("Usage: cot [--target=<t>] [-test] <input.cot> [-o <output>]\n", .{});
        return;
    };

    std.debug.print("Cot 0.3 Bootstrap Compiler\n", .{});
    std.debug.print("Input: {s}, Target: {s}\n", .{ actual_input, compile_target.name() });

    var compile_driver = Driver.init(allocator);
    compile_driver.setTarget(compile_target);
    if (test_mode) compile_driver.setTestMode(true);

    const code = compile_driver.compileFile(actual_input) catch |e| {
        std.debug.print("Compilation failed: {any}\n", .{e});
        return;
    };
    defer allocator.free(code);

    // For Wasm target, output .wasm directly (no linking needed)
    if (compile_target.isWasm()) {
        const wasm_path = if (std.mem.endsWith(u8, output_name, ".wasm"))
            try allocator.dupe(u8, output_name)
        else
            try std.fmt.allocPrint(allocator, "{s}.wasm", .{output_name});
        defer allocator.free(wasm_path);

        std.fs.cwd().writeFile(.{ .sub_path = wasm_path, .data = code }) catch |e| {
            std.debug.print("Failed to write Wasm file: {any}\n", .{e});
            return;
        };
        std.debug.print("Success: {s} ({d} bytes)\n", .{ wasm_path, code.len });
        return;
    }

    // Native target: write object file and link
    const obj_path = if (std.mem.endsWith(u8, output_name, ".o"))
        try allocator.dupe(u8, output_name)
    else
        try std.fmt.allocPrint(allocator, "{s}.o", .{output_name});
    defer allocator.free(obj_path);

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = code }) catch |e| {
        std.debug.print("Failed to write object file: {any}\n", .{e});
        return;
    };
    std.debug.print("Wrote: {s}\n", .{obj_path});

    if (std.mem.endsWith(u8, output_name, ".o")) return;

    // Link with zig cc
    const runtime_path = findRuntimePath(allocator, compile_target) catch null;
    defer if (runtime_path) |p| allocator.free(p);

    var link_args = std.ArrayListUnmanaged([]const u8){};
    defer link_args.deinit(allocator);

    if (compile_target.arch != Target.native().arch or compile_target.os != Target.native().os) {
        const triple: []const u8 = switch (compile_target.os) {
            .linux => if (compile_target.arch == .amd64) "x86_64-linux-gnu" else "aarch64-linux-gnu",
            .macos => if (compile_target.arch == .arm64) "aarch64-macos" else "x86_64-macos",
            .freestanding => unreachable, // Handled above with isWasm()
        };
        try link_args.appendSlice(allocator, &.{ "zig", "cc", "-target", triple, "-o", output_name, obj_path });
    } else {
        try link_args.appendSlice(allocator, &.{ "zig", "cc", "-o", output_name, obj_path });
    }

    if (runtime_path) |rp| try link_args.append(allocator, rp);
    if (compile_target.os == .macos) {
        try link_args.appendSlice(allocator, &.{ "-Wl,-stack_size,0x10000000", "-lSystem" });
    } else if (compile_target.os == .linux) {
        try link_args.append(allocator, "-lc");
    }

    var child = std.process.Child.init(link_args.items, allocator);
    const result = child.spawnAndWait() catch |e| {
        std.debug.print("Linker failed: {any}\n", .{e});
        return;
    };

    if (result.Exited == 0) {
        if (std.fs.cwd().openFile(output_name, .{})) |f| {
            defer f.close();
            f.chmod(0o755) catch {};
        } else |_| {}
        std.debug.print("Success: {s}\n", .{output_name});
    } else {
        std.debug.print("Link failed with code: {d}\n", .{result.Exited});
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
}

test "main: findRuntimePath returns error when not found" {
    const allocator = std.testing.allocator;
    const result = findRuntimePath(allocator, Target.native());
    try std.testing.expectError(error.RuntimeNotFound, result);
}
