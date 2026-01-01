//! Cot Standard Library
//!
//! Manages the standard library of subroutines available to all Cot programs.
//! The stdlib combines:
//! - Native (Zig) implementations for performance-critical and system-level code
//! - Cot implementations for higher-level utilities (once bytecode loading works)

const std = @import("std");
const NativeRegistry = @import("native.zig").NativeRegistry;
const NativeContext = @import("native.zig").NativeContext;
const NativeError = @import("native.zig").NativeError;
const Value = @import("native.zig").Value;
const Linker = @import("linker.zig").Linker;

/// Standard library manager
pub const Stdlib = struct {
    allocator: std.mem.Allocator,
    registry: *NativeRegistry,
    linker: Linker,
    stdlib_path: ?[]const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, registry: *NativeRegistry) Self {
        return Self{
            .allocator = allocator,
            .registry = registry,
            .linker = Linker.init(allocator),
            .stdlib_path = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.linker.deinit();
        if (self.stdlib_path) |sp| {
            self.allocator.free(sp);
        }
    }

    /// Set the stdlib path and load standard modules
    pub fn setPath(self: *Self, path: []const u8) !void {
        if (self.stdlib_path) |old| {
            self.allocator.free(old);
        }
        self.stdlib_path = try self.allocator.dupe(u8, path);
        try self.linker.setStdlibPath(path);
    }

    /// Load all standard library modules
    pub fn loadAll(self: *Self) !void {
        // Register additional native subroutines for stdlib
        try self.registerStdlibNatives();

        // Load compiled Cot stdlib modules if stdlib path is set
        if (self.stdlib_path) |path| {
            try self.loadCotModules(path);
        }
    }

    /// Register native stdlib subroutines
    fn registerStdlibNatives(self: *Self) !void {
        // Date/Time functions
        try self.registry.registerNative("date", native_date);
        try self.registry.registerNative("time", native_time);
        try self.registry.registerNative("datetime", native_datetime);

        // String functions
        try self.registry.registerNative("locase", native_locase);
        try self.registry.registerNative("upcase", native_upcase);
        try self.registry.registerNative("instr", native_instr);
        try self.registry.registerNative("char", native_char);

        // Math functions
        try self.registry.registerNative("abs", native_abs);
        try self.registry.registerNative("int", native_int);
        try self.registry.registerNative("frac", native_frac);

        // System functions
        try self.registry.registerNative("sleep", native_sleep);
        try self.registry.registerNative("spawn", native_spawn);

        // Memory functions
        try self.registry.registerNative("mem_alloc", native_mem_alloc);
        try self.registry.registerNative("mem_free", native_mem_free);
    }

    /// Load Cot stdlib modules from path
    fn loadCotModules(self: *Self, path: []const u8) !void {
        // Try to open the stdlib directory
        var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch return;
        defer dir.close();

        // Iterate through .cbo files
        var iter = dir.iterate();
        while (iter.next() catch return) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".cbo")) {
                // Load the module
                var path_buf: [512]u8 = undefined;
                const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ path, entry.name }) catch continue;
                _ = self.linker.loadModuleFromPath(full_path) catch continue;
            }
        }
    }
};

// ============================================================
// Native Stdlib Subroutine Implementations
// ============================================================

/// DATE - Get current date
fn native_date(ctx: *NativeContext) NativeError!?Value {
    // Return date in YYYYMMDD format
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const day = epoch.getEpochDay();
    const ymd = day.calculateYearDay().calculateMonthDay();

    const result = ctx.allocator.alloc(u8, 8) catch return NativeError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>4}{d:0>2}{d:0>2}", .{
        ymd.year,
        @intFromEnum(ymd.month),
        ymd.day,
    }) catch return NativeError.OutOfMemory;

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// TIME - Get current time
fn native_time(ctx: *NativeContext) NativeError!?Value {
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const day_secs = epoch.getDaySeconds();

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();

    const result = ctx.allocator.alloc(u8, 6) catch return NativeError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>2}{d:0>2}{d:0>2}", .{ hours, mins, secs }) catch return NativeError.OutOfMemory;

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// DATETIME - Get current date and time
fn native_datetime(ctx: *NativeContext) NativeError!?Value {
    const ts = std.time.timestamp();
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };

    const day = epoch.getEpochDay();
    const ymd = day.calculateYearDay().calculateMonthDay();
    const day_secs = epoch.getDaySeconds();

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();

    const result = ctx.allocator.alloc(u8, 14) catch return NativeError.OutOfMemory;
    _ = std.fmt.bufPrint(result, "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}", .{
        ymd.year,
        @intFromEnum(ymd.month),
        ymd.day,
        hours,
        mins,
        secs,
    }) catch return NativeError.OutOfMemory;

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// LOCASE - Convert string to lowercase
fn native_locase(ctx: *NativeContext) NativeError!?Value {
    const input = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    defer ctx.allocator.free(input);

    const result = ctx.allocator.dupe(u8, input) catch return NativeError.OutOfMemory;
    _ = std.ascii.lowerString(result, input);

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// UPCASE - Convert string to uppercase
fn native_upcase(ctx: *NativeContext) NativeError!?Value {
    const input = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    defer ctx.allocator.free(input);

    const result = ctx.allocator.dupe(u8, input) catch return NativeError.OutOfMemory;
    _ = std.ascii.upperString(result, input);

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// INSTR - Find position of substring
fn native_instr(ctx: *NativeContext) NativeError!?Value {
    const start = ctx.getArgInt(0) catch 1;
    const haystack = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    defer ctx.allocator.free(haystack);
    const needle = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    defer ctx.allocator.free(needle);

    const start_idx: usize = if (start > 0) @intCast(start - 1) else 0;

    if (start_idx >= haystack.len) return Value.initInt(0);

    if (std.mem.indexOf(u8, haystack[start_idx..], needle)) |pos| {
        return Value.initInt(@intCast(pos + start_idx + 1));
    }

    return Value.initInt(0);
}

/// CHAR - Get character from code
fn native_char(ctx: *NativeContext) NativeError!?Value {
    const code = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    if (code < 0 or code > 255) return NativeError.InvalidArgument;

    var result = ctx.allocator.alloc(u8, 1) catch return NativeError.OutOfMemory;
    result[0] = @intCast(code);

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// ABS - Absolute value
fn native_abs(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    return Value.initInt(@intCast(@abs(val)));
}

/// INT - Integer part
fn native_int(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    return Value.initInt(val);
}

/// FRAC - Fractional part (for fixed-point decimals)
fn native_frac(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    switch (val.tag()) {
        .decimal => {
            if (val.asDecimal()) |d| {
                const divisor = std.math.pow(i64, 10, d.precision);
                return Value.initDecimal(ctx.allocator, @rem(d.value, divisor), d.precision) catch return NativeError.OutOfMemory;
            }
            return Value.initInt(0);
        },
        else => return Value.initInt(0),
    }
}

/// SLEEP - Pause execution
fn native_sleep(ctx: *NativeContext) NativeError!?Value {
    const ms = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    if (ms > 0) {
        std.time.sleep(@intCast(ms * 1_000_000));
    }
    return null;
}

/// SPAWN - Execute system command
fn native_spawn(ctx: *NativeContext) NativeError!?Value {
    const cmd = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    defer ctx.allocator.free(cmd);

    // Create null-terminated command
    const cmd_z = ctx.allocator.dupeZ(u8, cmd) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(cmd_z);

    // Execute via shell
    var child = std.process.Child.init(&[_][]const u8{ "/bin/sh", "-c", cmd_z }, ctx.allocator);
    child.spawn() catch return NativeError.FileError;
    const result = child.wait() catch return NativeError.FileError;

    return switch (result) {
        .Exited => |code| Value.initInt(code),
        else => Value.initInt(-1),
    };
}

/// MEM_ALLOC - Allocate memory (returns handle)
fn native_mem_alloc(ctx: *NativeContext) NativeError!?Value {
    const size = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    if (size <= 0) return NativeError.InvalidArgument;

    const mem = ctx.allocator.alloc(u8, @intCast(size)) catch return NativeError.OutOfMemory;
    @memset(mem, 0);

    // Return pointer as handle
    return Value.initHandle(@intFromPtr(mem.ptr));
}

/// MEM_FREE - Free allocated memory
fn native_mem_free(ctx: *NativeContext) NativeError!?Value {
    const handle = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    switch (handle.tag()) {
        .handle => {
            // Get size from allocation tracking would be needed for proper free
            // For now, this is a stub - proper implementation needs allocation tracking
            _ = handle.asHandle();
        },
        else => return NativeError.InvalidArgument,
    }

    return null;
}

test "stdlib init" {
    const allocator = std.testing.allocator;
    var registry = @import("native.zig").NativeRegistry.init(allocator);
    defer registry.deinit();

    var stdlib = Stdlib.init(allocator, &registry);
    defer stdlib.deinit();

    try stdlib.loadAll();

    // Check that date function is registered
    const date_fn = registry.lookup("date");
    try std.testing.expect(date_fn != null);
}
