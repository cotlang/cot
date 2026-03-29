const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Tests — lib.zig pulls in all modules transitively
    const tests = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = b.path("lib.zig"),
        .target = target,
        .optimize = optimize,
    }) });
    const run_tests = b.addRunArtifact(tests);
    b.step("test", "Run all libcot tests").dependOn(&run_tests.step);
}
