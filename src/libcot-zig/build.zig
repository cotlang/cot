const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Tests — parser.zig is the root, transitively tests all modules
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("parser.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run all libcot tests");
    test_step.dependOn(&run_tests.step);
}
