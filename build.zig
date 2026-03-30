const std = @import("std");

/// Root build.zig — delegates to zig/build.zig.
/// Kept at repo root so `zig build` works from the project root.
///
/// Build flow:
///   1. cargo build --release (rust/libclif — Cranelift native backend)
///   2. zig build (zig/libcot — compiler, links rust/libclif)
///
/// The binary is output to zig/zig-out/bin/cot.

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Read VERSION from the file system at build time
    const version_str = blk: {
        const file = std.fs.cwd().openFile("VERSION", .{}) catch break :blk "0.0.0";
        defer file.close();
        var buf: [32]u8 = undefined;
        const len = file.readAll(&buf) catch break :blk "0.0.0";
        const trimmed = std.mem.trim(u8, buf[0..len], &std.ascii.whitespace);
        break :blk b.dupe(trimmed);
    };

    const options = b.addOptions();
    options.addOption([]const u8, "version", version_str);

    const native_os = @import("builtin").os.tag;
    const native_arch = @import("builtin").cpu.arch;

    // DWARF runtime reader
    const dwarf_obj = b.addObject(.{
        .name = "dwarf_reader",
        .root_module = b.createModule(.{
            .root_source_file = b.path("zig/libcot/codegen/native/dwarf_reader.zig"),
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
            .root_source_file = b.path("zig/libcot/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addAnonymousImport("dwarf_reader_native_o", .{
        .root_source_file = dwarf_obj.getEmittedBin(),
    });
    exe.root_module.addOptions("build_options", options);

    // Link against rust/libclif (Cranelift native backend)
    exe.addLibraryPath(.{ .cwd_relative = "rust/libclif/target/release" });
    exe.linkSystemLibrary("clif");
    exe.linkLibC();
    if (native_os == .macos) {
        exe.linkFramework("CoreFoundation");
        exe.linkFramework("Security");
    }

    b.installArtifact(exe);

    // Run
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    b.step("run", "Run the compiler").dependOn(&run_cmd.step);

    // Test
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("zig/libcot/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    tests.root_module.addOptions("build_options", options);
    tests.root_module.addAnonymousImport("dwarf_reader_native_o", .{
        .root_source_file = dwarf_obj.getEmittedBin(),
    });
    const run_tests = b.addRunArtifact(tests);
    if (b.args) |args| run_tests.addArgs(args);
    b.step("test", "Run unit tests").dependOn(&run_tests.step);
}
