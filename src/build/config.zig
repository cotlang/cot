//! Build Configuration
//!
//! Centralized build configuration following the Ghostty pattern.
//! All build options are defined here and can be accessed at compile time.
//!
//! Usage in build.zig:
//!   const config = @import("src/build/config.zig");
//!   const cfg = config.Config.init(b);
//!
//! Usage in source files (via build_options):
//!   const build_options = @import("build_options");
//!   if (build_options.enable_profiling) { ... }

const std = @import("std");

/// Cot Build Configuration
pub const Config = struct {
    /// Standard build settings
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,

    /// Feature flags
    enable_profiling: bool,
    enable_jit_profiling: bool,
    enable_postgres: bool,

    /// Debug options
    enable_debug_info: bool,
    verbose_ir: bool,

    /// Initialize configuration from build options
    pub fn init(b: *std.Build) Config {
        const target = b.standardTargetOptions(.{});
        const optimize = b.standardOptimizeOption(.{});

        return .{
            .target = target,
            .optimize = optimize,

            // Feature flags with defaults
            .enable_profiling = b.option(
                bool,
                "profiling",
                "Enable runtime profiling (default: false)",
            ) orelse false,

            .enable_jit_profiling = b.option(
                bool,
                "jit-profiling",
                "Enable JIT function call profiling (default: true)",
            ) orelse true,

            .enable_postgres = b.option(
                bool,
                "postgres",
                "Enable PostgreSQL support via libpq (default: false)",
            ) orelse false,

            // Debug options - default to true in Debug mode
            .enable_debug_info = b.option(
                bool,
                "debug-info",
                "Include debug info in bytecode (default: true in Debug)",
            ) orelse (optimize == .Debug),

            .verbose_ir = b.option(
                bool,
                "verbose-ir",
                "Print verbose IR during compilation (default: false)",
            ) orelse false,
        };
    }

    /// Create the build options module for compile-time access
    pub fn createOptionsModule(self: Config, b: *std.Build) *std.Build.Module {
        const options = b.addOptions();
        options.addOption(bool, "enable_profiling", self.enable_profiling);
        options.addOption(bool, "enable_jit_profiling", self.enable_jit_profiling);
        options.addOption(bool, "enable_postgres", self.enable_postgres);
        options.addOption(bool, "enable_debug_info", self.enable_debug_info);
        options.addOption(bool, "verbose_ir", self.verbose_ir);
        return options.createModule();
    }

    /// Check if this is a debug build
    pub fn isDebug(self: Config) bool {
        return self.optimize == .Debug;
    }

    /// Check if this is a release build
    pub fn isRelease(self: Config) bool {
        return self.optimize == .ReleaseFast or
            self.optimize == .ReleaseSafe or
            self.optimize == .ReleaseSmall;
    }

    /// Print configuration summary (for debugging build issues)
    pub fn printSummary(self: Config) void {
        std.debug.print(
            \\Cot Build Configuration:
            \\  optimize:           {s}
            \\  enable_profiling:   {}
            \\  enable_jit_profiling: {}
            \\  enable_postgres:    {}
            \\  enable_debug_info:  {}
            \\  verbose_ir:         {}
            \\
        , .{
            @tagName(self.optimize),
            self.enable_profiling,
            self.enable_jit_profiling,
            self.enable_postgres,
            self.enable_debug_info,
            self.verbose_ir,
        });
    }
};

/// Shared dependencies used across all modules
pub const SharedDeps = struct {
    cotdb: *std.Build.Module,
    cot_runtime: *std.Build.Module,
    build_options: *std.Build.Module,

    /// Initialize all shared dependencies
    pub fn init(b: *std.Build, config: *const Config) SharedDeps {
        // Build options module
        const build_options = config.createOptionsModule(b);

        // CotDB module
        const cotdb = b.addModule("cotdb", .{
            .root_source_file = b.path("src/cotdb/cotdb.zig"),
            .target = config.target,
        });
        cotdb.linkSystemLibrary("sqlite3", .{});
        cotdb.linkSystemLibrary("c", .{});

        // Cot Runtime module
        const cot_runtime = b.addModule("cot_runtime", .{
            .root_source_file = b.path("src/runtime/cot_runtime.zig"),
            .target = config.target,
            .imports = &.{
                .{ .name = "cotdb", .module = cotdb },
                .{ .name = "build_options", .module = build_options },
            },
        });
        cot_runtime.linkSystemLibrary("sqlite3", .{});
        cot_runtime.linkSystemLibrary("c", .{});

        // PostgreSQL support (optional)
        if (config.enable_postgres) {
            // Add libpq include and library paths for macOS Homebrew
            cot_runtime.addIncludePath(.{ .cwd_relative = "/opt/homebrew/opt/libpq/include" });
            cot_runtime.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/opt/libpq/lib" });
            cot_runtime.linkSystemLibrary("pq", .{});
        }

        return .{
            .cotdb = cotdb,
            .cot_runtime = cot_runtime,
            .build_options = build_options,
        };
    }

    /// Create the main cot module
    pub fn createCotModule(self: *const SharedDeps, b: *std.Build, config: *const Config) *std.Build.Module {
        const cot_mod = b.addModule("cot", .{
            .root_source_file = b.path("src/root.zig"),
            .target = config.target,
            .imports = &.{
                .{ .name = "cotdb", .module = self.cotdb },
                .{ .name = "cot_runtime", .module = self.cot_runtime },
                .{ .name = "build_options", .module = self.build_options },
            },
        });
        // Self-import: allow framework files within cot to import("cot")
        // This enables framework/commands/*.zig to access cot.* exports
        cot_mod.addImport("cot", cot_mod);
        cot_mod.linkSystemLibrary("sqlite3", .{});
        cot_mod.linkSystemLibrary("c", .{});
        return cot_mod;
    }
};
