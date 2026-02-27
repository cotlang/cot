//! Target platform configuration.

const std = @import("std");
const builtin = @import("builtin");

pub const Arch = enum {
    arm64,
    amd64,
    wasm32,

    pub fn name(self: Arch) []const u8 {
        return @tagName(self);
    }

    pub fn isWasm(self: Arch) bool {
        return self == .wasm32;
    }
};

pub const Os = enum {
    macos,
    linux,
    freestanding, // For Wasm (no OS)
    wasi, // WebAssembly System Interface

    pub fn name(self: Os) []const u8 {
        return @tagName(self);
    }
};

pub const Target = struct {
    arch: Arch,
    os: Os,
    gc: bool = false,

    pub const arm64_macos = Target{ .arch = .arm64, .os = .macos };
    pub const amd64_linux = Target{ .arch = .amd64, .os = .linux };
    pub const wasm32 = Target{ .arch = .wasm32, .os = .freestanding, .gc = true };
    pub const wasm32_wasi = Target{ .arch = .wasm32, .os = .wasi, .gc = true };

    pub fn native() Target {
        const arch: Arch = switch (builtin.cpu.arch) {
            .aarch64 => .arm64,
            .x86_64 => .amd64,
            else => .arm64,
        };
        const os: Os = switch (builtin.os.tag) {
            .macos => .macos,
            .linux => .linux,
            else => .linux,
        };
        return .{ .arch = arch, .os = os };
    }

    pub fn isWasmGC(self: Target) bool {
        return self.arch.isWasm() and self.gc;
    }

    pub fn name(self: Target) []const u8 {
        if (self.arch == .wasm32 and self.os == .wasi) return "wasm32-wasi";
        if (self.arch == .wasm32) return "wasm32";
        if (self.arch == .arm64 and self.os == .macos) return "arm64-macos";
        if (self.arch == .amd64 and self.os == .linux) return "amd64-linux";
        if (self.arch == .arm64 and self.os == .linux) return "arm64-linux";
        if (self.arch == .amd64 and self.os == .macos) return "amd64-macos";
        return "unknown";
    }

    pub fn parse(s: []const u8) ?Target {
        if (std.mem.eql(u8, s, "wasm32-wasi") or std.mem.eql(u8, s, "wasi")) return wasm32_wasi;
        if (std.mem.eql(u8, s, "wasm32") or std.mem.eql(u8, s, "wasm")) return wasm32;
        if (std.mem.eql(u8, s, "arm64-macos")) return arm64_macos;
        if (std.mem.eql(u8, s, "amd64-linux")) return amd64_linux;
        if (std.mem.eql(u8, s, "arm64-linux")) return .{ .arch = .arm64, .os = .linux };
        if (std.mem.eql(u8, s, "amd64-macos")) return .{ .arch = .amd64, .os = .macos };
        if (std.mem.eql(u8, s, "x86_64-linux") or std.mem.eql(u8, s, "x86-64-linux")) return amd64_linux;
        return null;
    }

    pub fn isWasm(self: Target) bool {
        return self.arch.isWasm();
    }

    pub fn isWasi(self: Target) bool {
        return self.os == .wasi;
    }

    pub inline fn usesMachO(self: Target) bool {
        return self.os == .macos;
    }

    pub inline fn usesELF(self: Target) bool {
        return self.os == .linux;
    }

    pub inline fn pointerSize(_: Target) u32 {
        return 8; // 64-bit only
    }

    pub inline fn stackAlign(_: Target) u32 {
        return 16;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Target.native" {
    const t = Target.native();
    _ = t.arch;
    _ = t.os;
}

test "Target.parse" {
    try std.testing.expectEqual(Target.amd64_linux, Target.parse("amd64-linux").?);
    try std.testing.expectEqual(Target.arm64_macos, Target.parse("arm64-macos").?);
    try std.testing.expectEqual(@as(?Target, null), Target.parse("invalid"));
}

test "Target.isWasmGC" {
    // All wasm targets are GC
    try std.testing.expect(Target.wasm32.isWasmGC());
    try std.testing.expect(Target.wasm32.isWasm());
    try std.testing.expect(Target.wasm32.gc);
    try std.testing.expect(!Target.arm64_macos.isWasmGC());
    try std.testing.expectEqualStrings("wasm32", Target.wasm32.name());
}

test "Target.parse wasm32" {
    const t = Target.parse("wasm32").?;
    try std.testing.expect(t.isWasmGC());
    try std.testing.expect(t.gc);
    try std.testing.expectEqualStrings("wasm32", t.name());
    const t2 = Target.parse("wasm").?;
    try std.testing.expect(t2.isWasmGC());
    try std.testing.expect(Target.parse("wasm32-gc") == null);
}

test "Target.usesMachO" {
    try std.testing.expect(Target.arm64_macos.usesMachO());
    try std.testing.expect(!Target.amd64_linux.usesMachO());
}

test "Target.usesELF" {
    try std.testing.expect(Target.amd64_linux.usesELF());
    try std.testing.expect(!Target.arm64_macos.usesELF());
}
