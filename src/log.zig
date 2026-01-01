//! Build-time logging for Cot compiler
//!
//! This module provides structured logging during compilation with multiple
//! verbosity levels. It is separate from the runtime debug system (debug.zig)
//! which is used for debugging VM execution.
//!
//! Log levels:
//!   Level 0 (silent):  Errors only - for CI/production
//!   Level 1 (normal):  Progress + warnings + errors (default)
//!   Level 2 (verbose): + compilation stages, file counts, timing
//!   Level 3 (debug):   + IR generation, instruction counts
//!   Level 4 (trace):   + every instruction, value tracking
//!
//! Usage:
//!   CLI:              cot build -v (verbose), -vv (debug), -vvv (trace)
//!   Environment:      COT_LOG_LEVEL=2
//!
//! Note: VM execution tracing still uses COT_DEBUG_VM=1 (see debug.zig)

const std = @import("std");
const builtin = @import("builtin");

/// Log verbosity levels
pub const Level = enum(u8) {
    silent = 0,  // Errors only
    normal = 1,  // Progress + warnings + errors (default)
    verbose = 2, // + stages, file counts, timing
    debug = 3,   // + IR/bytecode details
    trace = 4,   // + individual instructions

    /// Get log level from environment variable
    pub fn fromEnv() Level {
        const val = std.posix.getenv("COT_LOG_LEVEL") orelse return .normal;
        const level = std.fmt.parseInt(u8, val, 10) catch return .normal;
        return @enumFromInt(@min(level, 4));
    }

    /// Parse from -v flags count
    pub fn fromVerboseCount(count: u8) Level {
        return switch (count) {
            0 => .normal,
            1 => .verbose,
            2 => .debug,
            else => .trace,
        };
    }
};

/// Timer for measuring compilation phases
pub const Timer = struct {
    name: []const u8,
    start: i128,

    pub fn init(name: []const u8) Timer {
        return .{
            .name = name,
            .start = std.time.nanoTimestamp(),
        };
    }

    pub fn elapsed(self: Timer) u64 {
        const now = std.time.nanoTimestamp();
        return @intCast(@max(0, now - self.start));
    }

    pub fn elapsedMs(self: Timer) u64 {
        return self.elapsed() / std.time.ns_per_ms;
    }
};

/// Build statistics for a project
pub const BuildStats = struct {
    files_parsed: u32 = 0,
    ir_instructions: u32 = 0,
    bytecode_bytes: u32 = 0,
    parse_time_ns: u64 = 0,
    lower_time_ns: u64 = 0,
    emit_time_ns: u64 = 0,
    bundle_time_ns: u64 = 0,

    pub fn totalTimeMs(self: BuildStats) u64 {
        return (self.parse_time_ns + self.lower_time_ns + self.emit_time_ns + self.bundle_time_ns) / std.time.ns_per_ms;
    }
};

/// Build logger with verbosity control
pub const Logger = struct {
    level: Level,

    // Internal buffer for formatting
    buffer: [4096]u8 = undefined,

    pub fn init(level: Level) Logger {
        return .{ .level = level };
    }

    pub fn initFromEnv() Logger {
        return .{ .level = Level.fromEnv() };
    }

    /// Check if a level is enabled
    pub fn isEnabled(self: *const Logger, level: Level) bool {
        return @intFromEnum(level) <= @intFromEnum(self.level);
    }

    /// Log at normal level (default, always shown unless silent)
    pub fn info(self: *Logger, comptime fmt: []const u8, args: anytype) void {
        self.log(.normal, fmt, args);
    }

    /// Log at verbose level
    pub fn verbose(self: *Logger, comptime fmt: []const u8, args: anytype) void {
        self.log(.verbose, fmt, args);
    }

    /// Log at debug level
    pub fn debug(self: *Logger, comptime fmt: []const u8, args: anytype) void {
        self.log(.debug, fmt, args);
    }

    /// Log at trace level
    pub fn trace(self: *Logger, comptime fmt: []const u8, args: anytype) void {
        self.log(.trace, fmt, args);
    }

    /// Log a warning (always shown unless silent)
    pub fn warn(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        if (@intFromEnum(self.level) < @intFromEnum(Level.normal)) return;
        // Use std.debug.print which goes to stderr
        std.debug.print("\x1b[33mwarning:\x1b[0m " ++ fmt ++ "\n", args);
    }

    /// Log an error (always shown)
    pub fn err(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        _ = self;
        // Use std.debug.print which goes to stderr
        std.debug.print("\x1b[31merror:\x1b[0m " ++ fmt ++ "\n", args);
    }

    /// Internal log function
    fn log(self: *const Logger, comptime level: Level, comptime fmt: []const u8, args: anytype) void {
        if (!self.isEnabled(level)) return;

        const prefix = comptime switch (level) {
            .silent => "",
            .normal => "",
            .verbose => "",
            .debug => "[debug] ",
            .trace => "[trace] ",
        };

        // Use std.debug.print which goes to stderr
        std.debug.print(prefix ++ fmt ++ "\n", args);
    }

    /// Start a timed phase
    pub fn startPhase(self: *const Logger, name: []const u8) Timer {
        if (self.isEnabled(.verbose)) {
            std.debug.print("  {s}...", .{name});
        }
        return Timer.init(name);
    }

    /// End a timed phase
    pub fn endPhase(self: *const Logger, timer: Timer) void {
        if (self.isEnabled(.verbose)) {
            std.debug.print(" ({d}ms)\n", .{timer.elapsedMs()});
        }
    }

    /// Log project build start
    pub fn projectStart(self: *const Logger, name: []const u8, index: usize, total: usize) void {
        if (self.isEnabled(.verbose)) {
            std.debug.print("\n[{d}/{d}] {s}\n", .{ index + 1, total, name });
        }
    }

    /// Log project build completion
    pub fn projectDone(self: *const Logger, name: []const u8, stats: BuildStats) void {
        if (self.isEnabled(.debug)) {
            std.debug.print("  {s}: {d} files, {d} IR insts, {d} bytes ({d}ms)\n", .{
                name,
                stats.files_parsed,
                stats.ir_instructions,
                stats.bytecode_bytes,
                stats.totalTimeMs(),
            });
        }
    }

    /// Log IR instruction (trace level)
    pub fn irInst(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        if (self.isEnabled(.trace)) {
            std.debug.print("    IR: " ++ fmt ++ "\n", args);
        }
    }

    /// Log bytecode emission (trace level)
    pub fn emitInst(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        if (self.isEnabled(.trace)) {
            std.debug.print("    EMIT: " ++ fmt ++ "\n", args);
        }
    }
};

/// Global logger instance (initialized lazily)
var global_logger: ?Logger = null;

/// Get or initialize the global logger
pub fn getLogger() *Logger {
    if (global_logger == null) {
        global_logger = Logger.initFromEnv();
    }
    return &global_logger.?;
}

/// Set the global log level
pub fn setLevel(level: Level) void {
    if (global_logger == null) {
        global_logger = Logger.init(level);
    } else {
        global_logger.?.level = level;
    }
}

/// Convenience functions using global logger
pub fn info(comptime fmt: []const u8, args: anytype) void {
    getLogger().info(fmt, args);
}

pub fn verbose(comptime fmt: []const u8, args: anytype) void {
    getLogger().verbose(fmt, args);
}

pub fn debug(comptime fmt: []const u8, args: anytype) void {
    getLogger().debug(fmt, args);
}

pub fn trace(comptime fmt: []const u8, args: anytype) void {
    getLogger().trace(fmt, args);
}

pub fn warn(comptime fmt: []const u8, args: anytype) void {
    getLogger().warn(fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    getLogger().err(fmt, args);
}

test "log level from env" {
    // Default should be normal
    const level = Level.fromEnv();
    try std.testing.expect(level == .normal);
}

test "verbose count mapping" {
    try std.testing.expect(Level.fromVerboseCount(0) == .normal);
    try std.testing.expect(Level.fromVerboseCount(1) == .verbose);
    try std.testing.expect(Level.fromVerboseCount(2) == .debug);
    try std.testing.expect(Level.fromVerboseCount(3) == .trace);
    try std.testing.expect(Level.fromVerboseCount(10) == .trace);
}
