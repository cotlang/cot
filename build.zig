const std = @import("std");
const build_config = @import("src/build/config.zig");

pub fn build(b: *std.Build) void {
    // Initialize configuration from command-line options
    const config = build_config.Config.init(b);

    // ========================================================================
    // Codegen: generate native function types from spec
    // ========================================================================
    const gen_natives = b.addSystemCommand(&.{
        "zig", "run", "tools/codegen/gen_natives.zig", "--",
        "spec/natives.toml", "src/ir/generated/native_types.zig",
    });

    // Initialize shared dependencies
    const deps = build_config.SharedDeps.init(b, &config);

    // Create main cot module
    const cot_mod = deps.createCotModule(b, &config);

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
                .{ .name = "cotdb", .module = deps.cotdb },
                .{ .name = "cot_runtime", .module = deps.cot_runtime },
                .{ .name = "build_options", .module = deps.build_options },
            },
        }),
    });
    exe.linkSystemLibrary("sqlite3");
    exe.linkSystemLibrary("c");
    exe.step.dependOn(&gen_natives.step); // Codegen runs before compile

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
            .{ .name = "cot_runtime", .module = deps.cot_runtime },
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

    // Test filtering option: zig build test -Dtest-filter="pattern"
    const test_filter = b.option(
        []const u8,
        "test-filter",
        "Filter tests by name pattern",
    );

    const mod_tests = b.addTest(.{
        .root_module = cot_mod,
        .filters = if (test_filter) |f| &.{f} else &.{},
    });
    const run_mod_tests = b.addRunArtifact(mod_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_mod_tests.step);

    // ========================================================================
    // Manual codegen step (also runs automatically as part of build)
    // ========================================================================
    const gen_step = b.step("gen-natives", "Regenerate native function type mappings from spec/natives.toml");
    gen_step.dependOn(&gen_natives.step);

    // ========================================================================
    // Integration test step
    // ========================================================================
    const integration_step = b.step("test-integration", "Run integration tests on fixture files");
    integration_step.dependOn(b.getInstallStep());

    // ========================================================================
    // Test all step (unit + integration)
    // ========================================================================
    const test_all_step = b.step("test-all", "Run all tests (unit + integration)");
    test_all_step.dependOn(&run_mod_tests.step);
    test_all_step.dependOn(integration_step);

    // ========================================================================
    // Format steps
    // ========================================================================
    // zig build fmt - Format all source files
    const fmt_step = b.step("fmt", "Format source files");
    const fmt_cmd = b.addSystemCommand(&.{ "zig", "fmt", "src/" });
    fmt_step.dependOn(&fmt_cmd.step);

    // zig build fmt-check - Check formatting (for CI)
    const fmt_check_step = b.step("fmt-check", "Check source file formatting");
    const fmt_check_cmd = b.addSystemCommand(&.{ "zig", "fmt", "--check", "src/" });
    fmt_check_step.dependOn(&fmt_check_cmd.step);
}
