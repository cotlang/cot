//! Target platform configuration.
//!
//! Defines the compilation target: CPU architecture + operating system.
//! Used throughout the compiler to make target-specific decisions (Wasm
//! vs native codegen, MachO vs ELF object format, pointer size, etc.).
//!
//! The native target is detected at compile time via Zig's builtin
//! constants. Cross-compilation targets are parsed from CLI strings
//! like "wasm", "arm64-macos", "amd64-linux".

const std = @import("std");
const builtin = @import("builtin");

/// CPU architecture.
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

/// Operating system.
pub const Os = enum {
    macos,
    linux,
    freestanding, // Wasm without WASI (browser)
    wasi,         // WebAssembly System Interface (server-side Wasm)

    pub fn name(self: Os) []const u8 {
        return @tagName(self);
    }
};

/// Compilation target: architecture + OS + optional flags.
///
/// Pre-defined constants for common targets avoid repeated construction.
/// `native()` detects the host platform at compile time.
/// `parse()` converts CLI strings like "wasm" or "arm64-macos".
pub const Target = struct {
    arch: Arch,
    os: Os,
    gc: bool = false,

    pub const arm64_macos = Target{ .arch = .arm64, .os = .macos };
    pub const amd64_linux = Target{ .arch = .amd64, .os = .linux };
    pub const wasm32 = Target{ .arch = .wasm32, .os = .freestanding, .gc = false };
    pub const wasm32_wasi = Target{ .arch = .wasm32, .os = .wasi, .gc = false };

    /// Detect the host platform at compile time.
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

    /// Parse a target string from the CLI (e.g., "wasm", "arm64-macos").
    /// Returns null for unrecognized strings.
    pub fn parse(s: []const u8) ?Target {
        if (std.mem.eql(u8, s, "wasm")) return wasm32_wasi;
        if (std.mem.eql(u8, s, "js")) return wasm32;
        if (std.mem.eql(u8, s, "wasm32-wasi") or std.mem.eql(u8, s, "wasi")) return wasm32_wasi;
        if (std.mem.eql(u8, s, "wasm32")) return wasm32;
        if (std.mem.eql(u8, s, "arm64-macos")) return arm64_macos;
        if (std.mem.eql(u8, s, "amd64-linux")) return amd64_linux;
        if (std.mem.eql(u8, s, "arm64-linux")) return .{ .arch = .arm64, .os = .linux };
        if (std.mem.eql(u8, s, "amd64-macos")) return .{ .arch = .amd64, .os = .macos };
        if (std.mem.eql(u8, s, "x86_64-linux") or std.mem.eql(u8, s, "x86-64-linux")) return amd64_linux;
        return null;
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

    pub fn isWasm(self: Target) bool {
        return self.arch.isWasm();
    }

    pub fn isWasi(self: Target) bool {
        return self.os == .wasi;
    }

    pub fn isWasmGC(self: Target) bool {
        return self.arch.isWasm() and self.gc;
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


test "native detection" {
    const t = Target.native();
    _ = t.arch;
    _ = t.os;
}

test "parse target strings" {
    try std.testing.expectEqual(Target.amd64_linux, Target.parse("amd64-linux").?);
    try std.testing.expectEqual(Target.arm64_macos, Target.parse("arm64-macos").?);
    try std.testing.expectEqual(Target.wasm32_wasi, Target.parse("wasm").?);
    try std.testing.expectEqual(Target.wasm32, Target.parse("js").?);
    try std.testing.expectEqual(@as(?Target, null), Target.parse("invalid"));
}

test "wasm properties" {
    try std.testing.expect(Target.wasm32.isWasm());
    try std.testing.expect(!Target.wasm32.isWasmGC());
    try std.testing.expect(!Target.wasm32.gc);
    try std.testing.expect(!Target.arm64_macos.isWasm());
    try std.testing.expectEqualStrings("wasm32", Target.wasm32.name());
}

test "object format" {
    try std.testing.expect(Target.arm64_macos.usesMachO());
    try std.testing.expect(!Target.arm64_macos.usesELF());
    try std.testing.expect(Target.amd64_linux.usesELF());
    try std.testing.expect(!Target.amd64_linux.usesMachO());
}
