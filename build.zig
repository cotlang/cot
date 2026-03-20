const std = @import("std");

/// Single source of truth: read from the VERSION file at the repo root.
const version = std.mem.trim(u8, @embedFile("VERSION"), &std.ascii.whitespace);

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options: inject version string into compiler at comptime
    const options = b.addOptions();
    options.addOption([]const u8, "version", version);

    // DWARF runtime reader: compile for the HOST platform, embed as .o bytes.
    // Zig reference: std.debug reads DWARF from own binary at crash time.
    // We pre-compile this Zig module and embed it so the Cot compiler can
    // include it in debug binaries without requiring Zig at Cot compile time.
    // Only compile for the host platform to avoid cross-compilation issues.
    const native_os = @import("builtin").os.tag;
    const native_arch = @import("builtin").cpu.arch;

    const dwarf_obj = b.addObject(.{
        .name = "dwarf_reader",
        .root_module = b.createModule(.{
            .root_source_file = b.path("compiler/codegen/native/dwarf_reader.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = native_arch,
                .os_tag = native_os,
                .abi = if (native_os == .linux) .gnu else null,
            }),
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
    // Embed pre-compiled DWARF reader .o file for the host platform
    exe.root_module.addAnonymousImport("dwarf_reader_native_o", .{
        .root_source_file = dwarf_obj.getEmittedBin(),
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
