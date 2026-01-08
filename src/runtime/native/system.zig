//! System native functions
//!
//! Namespace: std.system
//! Functions: flags, getlog, setlog, error, mem
//!
//! In .cot files: requires `import std.system` then call as `std.system.getlog(x)`
//! In .dbl files: available directly as `getlog(x)` (DBL compatibility)

const std = @import("std");
const native = @import("native.zig");
const value_mod = @import("../bytecode/value.zig");
const arc = @import("../bytecode/arc.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;
const List = value_mod.List;

/// Register all system functions with both namespaced and short names
pub fn register(registry: anytype) !void {
    // Namespaced names (std.system.*)
    try registry.registerNative("std.system.flags", flags);
    try registry.registerNative("std.system.getlog", getlog);
    try registry.registerNative("std.system.setlog", setlog);
    try registry.registerNative("std.system.error", getError);
    try registry.registerNative("std.system.mem", mem);

    // Process functions
    try registry.registerNative("std.process.args", processArgs);
    try registry.registerNative("process_args", processArgs);

    // Short names (DBL compatibility)
    try registry.registerNative("flags", flags);
    try registry.registerNative("getlog", getlog);
    try registry.registerNative("setlog", setlog);
    try registry.registerNative("error", getError);
    try registry.registerNative("mem", mem);
}

/// FLAGS - Set runtime flags
pub fn flags(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // TODO: Implement flags
    return null;
}

/// GETLOG - Get logical name (environment variable)
pub fn getlog(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 1) return NativeError.InvalidArgument;

    const name = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const value = std.process.getEnvVarOwned(ctx.allocator, name) catch {
        return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;
    };

    return Value.initString(ctx.allocator, value) catch return NativeError.OutOfMemory;
}

/// SETLOG - Set logical name (environment variable)
pub fn setlog(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // Environment variables can't be set in this process to affect it
    return null;
}

/// ERROR - Get last error code
pub fn getError(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // TODO: Track actual error codes from I/O operations
    return Value.initInt(0);
}

/// MEM - Allocate memory handle
pub fn mem(ctx: *NativeContext) NativeError!?Value {
    const size_val = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    if (size_val <= 0) return NativeError.InvalidArgument;

    const buf = ctx.allocator.alloc(u8, @intCast(size_val)) catch return NativeError.OutOfMemory;
    @memset(buf, 0);

    // Return handle (pointer as integer)
    return Value.initHandle(@intFromPtr(buf.ptr));
}

/// std.process.args() -> List<string>
/// Returns command-line arguments as a list of strings.
/// First element is the program name, followed by arguments.
pub fn processArgs(ctx: *NativeContext) NativeError!?Value {
    // Get args iterator
    var args_iter = std.process.args();

    // Create list for results using ARC allocation (includes ARC header)
    const list_ptr = arc.create(ctx.allocator, List) catch return NativeError.OutOfMemory;
    list_ptr.* = List.init(ctx.allocator);

    // Iterate and collect all args
    while (args_iter.next()) |arg| {
        const str_val = Value.initString(ctx.allocator, arg) catch return NativeError.OutOfMemory;
        list_ptr.push(str_val) catch return NativeError.OutOfMemory;
    }

    return Value.initList(list_ptr);
}
