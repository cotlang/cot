const std = @import("std");
const build_config = @import("src/build/config.zig");

pub fn build(b: *std.Build) void {
    // Initialize configuration from command-line options
    const config = build_config.Config.init(b);

    // Initialize shared dependencies
    const deps = build_config.SharedDeps.init(b, &config);

    // Create main cot module
    const cot_mod = deps.createCotModule(b, &config);

    // JIT library path (if enabled)
    const jit_lib_path = config.getJitLibPath(b);

    // ========================================================================
    // Main executable: cot
    // ========================================================================
    const exe = b.addExecutable(.{
        .name = "cot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = config.target,
            .optimize = config.optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "tui", .module = deps.tui },
                .{ .name = "cotdb", .module = deps.cotdb },
                .{ .name = "cot_runtime", .module = deps.cot_runtime },
                .{ .name = "build_options", .module = deps.build_options },
            },
        }),
    });
    if (deps.cot_tui) |tm| {
        exe.root_module.addImport("cot_tui", tm);
    }
    exe.linkSystemLibrary("sqlite3");
    exe.linkSystemLibrary("c");

    // Link Cranelift JIT library when enabled
    if (jit_lib_path) |lib_path| {
        exe.addLibraryPath(lib_path);
        exe.linkSystemLibrary("cot_cranelift");
    }

    b.installArtifact(exe);

    // macOS: Generate dSYM for better crash stack traces
    if (config.target.result.os.tag == .macos) {
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
        .target = config.target,
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
            .target = config.target,
            .optimize = config.optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "dbl", .module = dbl_mod },
                .{ .name = "cot_runtime", .module = deps.cot_runtime },
                .{ .name = "build_options", .module = deps.build_options },
            },
        }),
    });
    if (deps.cot_tui) |tm| {
        dbl_exe.root_module.addImport("cot_tui", tm);
    }
    dbl_exe.linkSystemLibrary("sqlite3");
    dbl_exe.linkSystemLibrary("c");

    // Link Cranelift JIT library when enabled
    if (jit_lib_path) |lib_path| {
        dbl_exe.addLibraryPath(lib_path);
        dbl_exe.linkSystemLibrary("cot_cranelift");
    }

    b.installArtifact(dbl_exe);

    // ========================================================================
    // LSP executable: cot-lsp
    // ========================================================================
    const lsp_exe = b.addExecutable(.{
        .name = "cot-lsp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/lsp/main.zig"),
            .target = config.target,
            .optimize = config.optimize,
            .imports = &.{
                .{ .name = "cot", .module = cot_mod },
                .{ .name = "dbl", .module = dbl_mod },
                .{ .name = "cot_runtime", .module = deps.cot_runtime },
                .{ .name = "build_options", .module = deps.build_options },
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
