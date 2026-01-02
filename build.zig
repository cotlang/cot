const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options
    const enable_tui = b.option(bool, "tui", "Enable TUI support (default: true)") orelse true;

    // Build options module for compile-time checks
    const options = b.addOptions();
    options.addOption(bool, "enable_tui", enable_tui);
    const build_options_mod = options.createModule();

    // ========================================================================
    // External dependency: tui.zig (terminal library)
    // ========================================================================
    const tui_dep = b.dependency("tui", .{
        .target = target,
        .optimize = optimize,
    });

    // ========================================================================
    // Internal modules (all in this repo now)
    // ========================================================================

    // CotDB module (src/cotdb/)
    const cotdb_mod = b.addModule("cotdb", .{
        .root_source_file = b.path("src/cotdb/cotdb.zig"),
        .target = target,
    });
    cotdb_mod.linkSystemLibrary("sqlite3", .{});
    cotdb_mod.linkSystemLibrary("c", .{});

    // Cot Runtime module (src/runtime/)
    const cot_runtime_mod = b.addModule("cot_runtime", .{
        .root_source_file = b.path("src/runtime/cot_runtime.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "tui", .module = tui_dep.module("tui") },
            .{ .name = "cotdb", .module = cotdb_mod },
            .{ .name = "build_options", .module = build_options_mod },
        },
    });
    cot_runtime_mod.linkSystemLibrary("sqlite3", .{});
    cot_runtime_mod.linkSystemLibrary("c", .{});

    // TUI Extension module (src/tui_ext/) - only when TUI enabled
    var cot_tui_mod: ?*std.Build.Module = null;
    if (enable_tui) {
        cot_tui_mod = b.addModule("cot_tui", .{
            .root_source_file = b.path("src/tui_ext/cot_tui.zig"),
            .target = target,
            .imports = &.{
                .{ .name = "tui", .module = tui_dep.module("tui") },
                .{ .name = "cot_runtime", .module = cot_runtime_mod },
                .{ .name = "build_options", .module = build_options_mod },
            },
        });
    }

    // Main cot module (compiler + framework)
    const cot_mod = b.addModule("cot", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "tui", .module = tui_dep.module("tui") },
            .{ .name = "cotdb", .module = cotdb_mod },
            .{ .name = "cot_runtime", .module = cot_runtime_mod },
            .{ .name = "build_options", .module = build_options_mod },
        },
    });
    if (cot_tui_mod) |tm| {
        cot_mod.addImport("cot_tui", tm);
    }
    cot_mod.linkSystemLibrary("sqlite3", .{});
    cot_mod.linkSystemLibrary("c", .{});

    // ========================================================================
    // Main executable: cot
    // ========================================================================
    const exe = b.addExecutable(.{
        .name = "cot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "tui", .module = tui_dep.module("tui") },
                .{ .name = "cotdb", .module = cotdb_mod },
                .{ .name = "cot_runtime", .module = cot_runtime_mod },
                .{ .name = "build_options", .module = build_options_mod },
            },
        }),
    });
    if (cot_tui_mod) |tm| {
        exe.root_module.addImport("cot_tui", tm);
    }
    exe.linkSystemLibrary("sqlite3");
    exe.linkSystemLibrary("c");
    b.installArtifact(exe);

    // macOS: Generate dSYM for better crash stack traces
    if (target.result.os.tag == .macos) {
        const dsymutil = b.addSystemCommand(&.{"dsymutil"});
        dsymutil.addArtifactArg(exe);
        dsymutil.addArg("-o");
        dsymutil.addArg(b.getInstallPath(.bin, "cot.dSYM"));
        b.getInstallStep().dependOn(&dsymutil.step);
    }

    // ========================================================================
    // DBL module (src/dbl/) - DBL syntax frontend
    // ========================================================================
    const dbl_mod = b.addModule("dbl", .{
        .root_source_file = b.path("src/dbl/root.zig"),
        .target = target,
        .imports = &.{
            .{ .name = "cot", .module = cot_mod },
        },
    });

    // ========================================================================
    // DBL executable: cot-dbl
    // ========================================================================
    const dbl_exe = b.addExecutable(.{
        .name = "cot-dbl",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/dbl/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "dbl", .module = dbl_mod },
                .{ .name = "cot_runtime", .module = cot_runtime_mod },
                .{ .name = "build_options", .module = build_options_mod },
            },
        }),
    });
    if (cot_tui_mod) |tm| {
        dbl_exe.root_module.addImport("cot_tui", tm);
    }
    dbl_exe.linkSystemLibrary("sqlite3");
    dbl_exe.linkSystemLibrary("c");
    b.installArtifact(dbl_exe);

    // ========================================================================
    // LSP executable: cot-lsp
    // ========================================================================
    const lsp_exe = b.addExecutable(.{
        .name = "cot-lsp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/lsp/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "dbl", .module = dbl_mod },
                .{ .name = "cot_runtime", .module = cot_runtime_mod },
                .{ .name = "build_options", .module = build_options_mod },
            },
        }),
    });
    lsp_exe.linkSystemLibrary("sqlite3");
    lsp_exe.linkSystemLibrary("c");
    b.installArtifact(lsp_exe);

    // LSP-only build step
    const lsp_step = b.step("lsp", "Build only the LSP server");
    lsp_step.dependOn(&b.addInstallArtifact(lsp_exe, .{}).step);

    // ========================================================================
    // Run step
    // ========================================================================
    const run_step = b.step("run", "Run the cot compiler");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // ========================================================================
    // Test step
    // ========================================================================
    const mod_tests = b.addTest(.{
        .root_module = cot_mod,
    });
    const run_mod_tests = b.addRunArtifact(mod_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_mod_tests.step);
}
