const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Foundation — shared types that cross library boundaries
    const foundation = b.createModule(.{
        .root_source_file = b.path("foundation/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    // libcir — IR + passes (language-agnostic)
    const libcir = b.createModule(.{
        .root_source_file = b.path("libcir-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    libcir.addImport("foundation", foundation);

    // libcot — frontend (language-specific)
    const libcot = b.createModule(.{
        .root_source_file = b.path("libcot-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    libcot.addImport("foundation", foundation);
    libcot.addImport("cir", libcir);

    // Test targets
    const test_foundation = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = b.path("foundation/lib.zig"),
        .target = target,
        .optimize = optimize,
    }) });

    const test_libcir = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = b.path("libcir-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    }) });
    test_libcir.root_module.addImport("foundation", foundation);

    const test_libcot = b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = b.path("libcot-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    }) });
    test_libcot.root_module.addImport("foundation", foundation);
    test_libcot.root_module.addImport("cir", libcir);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(test_foundation).step);
    test_step.dependOn(&b.addRunArtifact(test_libcir).step);
    test_step.dependOn(&b.addRunArtifact(test_libcot).step);

    // Individual library test targets
    b.step("test-foundation", "Test foundation").dependOn(&b.addRunArtifact(test_foundation).step);
    b.step("test-cir", "Test libcir").dependOn(&b.addRunArtifact(test_libcir).step);
    b.step("test-cot", "Test libcot").dependOn(&b.addRunArtifact(test_libcot).step);
}
