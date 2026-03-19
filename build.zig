const std = @import("std");

/// Single source of truth: read from the VERSION file at the repo root.
const version = std.mem.trim(u8, @embedFile("VERSION"), &std.ascii.whitespace);

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options: inject version string into compiler at comptime
    const options = b.addOptions();
    options.addOption([]const u8, "version", version);

    // DWARF runtime reader: compile for each target arch, embed as .o bytes.
    // Zig reference: std.debug reads DWARF from own binary at crash time.
    // We pre-compile this Zig module and embed it so the Cot compiler can
    // include it in debug binaries without requiring Zig at Cot compile time.
    const dwarf_arm64 = b.addObject(.{
        .name = "dwarf_reader_arm64",
        .root_module = b.createModule(.{
            .root_source_file = b.path("compiler/codegen/native/dwarf_reader.zig"),
            .target = b.resolveTargetQuery(.{ .cpu_arch = .aarch64, .os_tag = .macos }),
            .optimize = .ReleaseFast,
            .link_libc = true,
        }),
    });
    const dwarf_x64 = b.addObject(.{
        .name = "dwarf_reader_x64",
        .root_module = b.createModule(.{
            .root_source_file = b.path("compiler/codegen/native/dwarf_reader.zig"),
            .target = b.resolveTargetQuery(.{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu }),
            .optimize = .ReleaseFast,
            .link_libc = true,
        }),
    });

    // Compiler executable
    const exe = b.addExecutable(.{
        .name = "cot",
        .root_module = b.createModule(.{
            .root_source_file = b.path("compiler/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    // Embed pre-compiled DWARF reader .o files
    exe.root_module.addAnonymousImport("dwarf_reader_arm64_o", .{
        .root_source_file = dwarf_arm64.getEmittedBin(),
    });
    exe.root_module.addAnonymousImport("dwarf_reader_x64_o", .{
        .root_source_file = dwarf_x64.getEmittedBin(),
    });
    exe.root_module.addOptions("build_options", options);
    b.installArtifact(exe);

    // Run: zig build run -- <args>
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    b.step("run", "Run the compiler").dependOn(&run_cmd.step);

    // Test: zig build test
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("compiler/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    tests.root_module.addOptions("build_options", options);
    const run_tests = b.addRunArtifact(tests);
    if (b.args) |args| run_tests.addArgs(args);
    b.step("test", "Run unit tests").dependOn(&run_tests.step);
}
