//! Buffer native functions
//!
//! Namespace: std.mem
//! Functions: alloc, free, resize, size, get, set, clear
//!
//! Modern replacement for DBL's memory handles (^M, D_HANDLE, %MEM_PROC).
//! Provides a safe, managed byte buffer with automatic memory management.
//!
//! In .cot files: requires `import std.mem` then call as `std.mem.alloc(size)`
//! In .dbl files: available as `mem_alloc(size)` (DBL compatibility)
//!
//! DBL Legacy:
//!   handle = %mem_proc(DM_ALLOC, size)
//!   %mem_proc(DM_FREE, handle)
//!   %mem_proc(DM_RESIZ, handle, new_size)
//!
//! Cot Modern:
//!   handle = std.mem.alloc(size)
//!   std.mem.free(handle)
//!   std.mem.resize(handle, new_size)

const std = @import("std");
const native = @import("native.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Buffer handle registry - maps integer handles to buffers
var handle_registry: ?std.AutoHashMap(i64, *ManagedBuffer) = null;
var next_handle: i64 = 1;
var registry_allocator: ?std.mem.Allocator = null;

/// A managed byte buffer with automatic growth
pub const ManagedBuffer = struct {
    data: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, initial_size: usize) !*ManagedBuffer {
        const buf = try allocator.create(ManagedBuffer);
        buf.* = .{
            .data = .empty,
            .allocator = allocator,
        };
        try buf.data.resize(allocator, initial_size);
        // Zero-initialize
        @memset(buf.data.items, 0);
        return buf;
    }

    pub fn deinit(self: *ManagedBuffer, allocator: std.mem.Allocator) void {
        self.data.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn resize(self: *ManagedBuffer, new_size: usize) !void {
        const old_size = self.data.items.len;
        try self.data.resize(self.allocator, new_size);
        // Zero-initialize new space
        if (new_size > old_size) {
            @memset(self.data.items[old_size..], 0);
        }
    }

    pub fn size(self: *const ManagedBuffer) usize {
        return self.data.items.len;
    }

    pub fn get(self: *const ManagedBuffer, offset: usize, len: usize) ?[]const u8 {
        if (offset + len > self.data.items.len) return null;
        return self.data.items[offset..][0..len];
    }

    pub fn set(self: *ManagedBuffer, offset: usize, data: []const u8) bool {
        if (offset + data.len > self.data.items.len) return false;
        @memcpy(self.data.items[offset..][0..data.len], data);
        return true;
    }
};

/// Initialize the buffer registry (called once at startup)
fn ensureRegistry(allocator: std.mem.Allocator) void {
    if (handle_registry == null) {
        handle_registry = std.AutoHashMap(i64, *ManagedBuffer).init(allocator);
        registry_allocator = allocator;
    }
}

/// Register all buffer functions with both namespaced and short names
pub fn register(registry: anytype) !void {
    // Namespaced names (std.mem.*)
    try registry.registerNative("std.mem.alloc", mem_alloc);
    try registry.registerNative("std.mem.free", mem_free);
    try registry.registerNative("std.mem.resize", mem_resize);
    try registry.registerNative("std.mem.size", mem_size);
    try registry.registerNative("std.mem.get", mem_get);
    try registry.registerNative("std.mem.set", mem_set);
    try registry.registerNative("std.mem.clear", mem_clear);

    // Short names (DBL compatibility - mem_ prefix style)
    try registry.registerNative("mem_alloc", mem_alloc);
    try registry.registerNative("mem_free", mem_free);
    try registry.registerNative("mem_resize", mem_resize);
    try registry.registerNative("mem_size", mem_size);
    try registry.registerNative("mem_get", mem_get);
    try registry.registerNative("mem_set", mem_set);
    try registry.registerNative("mem_clear", mem_clear);

    // DBL compatibility - %MEM_PROC wrapper
    try registry.registerNative("mem_proc", mem_proc);
}

/// MEM_ALLOC - Allocate a new buffer
/// mem_alloc(size) -> handle
pub fn mem_alloc(ctx: *NativeContext) NativeError!?Value {
    ensureRegistry(ctx.allocator);

    const size_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const size: usize = @intCast(@max(0, size_val.toInt()));

    const buffer = ManagedBuffer.init(ctx.allocator, size) catch return NativeError.OutOfMemory;

    const handle = next_handle;
    next_handle += 1;

    if (handle_registry) |*reg| {
        reg.put(handle, buffer) catch return NativeError.OutOfMemory;
    }

    return Value.initInt(handle);
}

/// MEM_FREE - Free a buffer
/// mem_free(handle) -> 0 (success) or -1 (error)
pub fn mem_free(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const handle = handle_val.toInt();

    if (handle_registry) |*reg| {
        if (reg.fetchRemove(handle)) |kv| {
            kv.value.deinit(registry_allocator.?);
            return Value.initInt(0);
        }
    }

    return Value.initInt(-1);
}

/// MEM_RESIZE - Resize a buffer
/// mem_resize(handle, new_size) -> 0 (success) or -1 (error)
pub fn mem_resize(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const size_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;

    const handle = handle_val.toInt();
    const new_size: usize = @intCast(@max(0, size_val.toInt()));

    if (handle_registry) |*reg| {
        if (reg.get(handle)) |buffer| {
            buffer.resize(new_size) catch return NativeError.OutOfMemory;
            return Value.initInt(0);
        }
    }

    return Value.initInt(-1);
}

/// MEM_SIZE - Get buffer size
/// mem_size(handle) -> size or -1 (error)
pub fn mem_size(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const handle = handle_val.toInt();

    if (handle_registry) |*reg| {
        if (reg.get(handle)) |buffer| {
            return Value.initInt(@intCast(buffer.size()));
        }
    }

    return Value.initInt(-1);
}

/// MEM_GET - Read data from buffer
/// mem_get(handle, offset, length) -> string or ""
pub fn mem_get(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const offset_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
    const len_val = ctx.getArg(2) orelse return NativeError.InvalidArgument;

    const handle = handle_val.toInt();
    const offset: usize = @intCast(@max(0, offset_val.toInt()));
    const len: usize = @intCast(@max(0, len_val.toInt()));

    if (handle_registry) |*reg| {
        if (reg.get(handle)) |buffer| {
            if (buffer.get(offset, len)) |data| {
                return Value.initString(ctx.allocator, data) catch return NativeError.OutOfMemory;
            }
        }
    }

    return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;
}

/// MEM_SET - Write data to buffer
/// mem_set(handle, offset, data) -> bytes_written or -1 (error)
pub fn mem_set(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const offset_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
    const data_val = ctx.getArg(2) orelse return NativeError.InvalidArgument;

    const handle = handle_val.toInt();
    const offset: usize = @intCast(@max(0, offset_val.toInt()));
    const data = data_val.toString();

    if (handle_registry) |*reg| {
        if (reg.get(handle)) |buffer| {
            if (buffer.set(offset, data)) {
                return Value.initInt(@intCast(data.len));
            }
        }
    }

    return Value.initInt(-1);
}

/// MEM_CLEAR - Clear buffer (zero-fill)
/// mem_clear(handle) -> 0 (success) or -1 (error)
pub fn mem_clear(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const handle = handle_val.toInt();

    if (handle_registry) |*reg| {
        if (reg.get(handle)) |buffer| {
            @memset(buffer.data.items, 0);
            return Value.initInt(0);
        }
    }

    return Value.initInt(-1);
}

// DBL compatibility constants
const DM_ALLOC: i64 = 1;
const DM_RESIZ: i64 = 2;
const DM_FREE: i64 = 3;
const DM_GETSIZE: i64 = 4;

/// MEM_PROC - DBL compatibility wrapper for %MEM_PROC
/// mem_proc(operation, handle, [size]) -> result
/// Operations: DM_ALLOC=1, DM_RESIZ=2, DM_FREE=3, DM_GETSIZE=4
pub fn mem_proc(ctx: *NativeContext) NativeError!?Value {
    const op_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const op = op_val.toInt();

    switch (op) {
        DM_ALLOC => {
            // %mem_proc(DM_ALLOC, size) -> handle
            const size_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
            // Repack args for mem_alloc
            var new_ctx = NativeContext{
                .allocator = ctx.allocator,
                .args = &[_]Value{size_val},
                .handles = ctx.handles,
            };
            return mem_alloc(&new_ctx);
        },
        DM_FREE => {
            // %mem_proc(DM_FREE, handle) -> status
            const handle_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
            var new_ctx = NativeContext{
                .allocator = ctx.allocator,
                .args = &[_]Value{handle_val},
                .handles = ctx.handles,
            };
            return mem_free(&new_ctx);
        },
        DM_RESIZ => {
            // %mem_proc(DM_RESIZ, handle, new_size) -> status
            const handle_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
            const size_val = ctx.getArg(2) orelse return NativeError.InvalidArgument;
            var new_ctx = NativeContext{
                .allocator = ctx.allocator,
                .args = &[_]Value{ handle_val, size_val },
                .handles = ctx.handles,
            };
            return mem_resize(&new_ctx);
        },
        DM_GETSIZE => {
            // %mem_proc(DM_GETSIZE, handle) -> size
            const handle_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
            var new_ctx = NativeContext{
                .allocator = ctx.allocator,
                .args = &[_]Value{handle_val},
                .handles = ctx.handles,
            };
            return mem_size(&new_ctx);
        },
        else => return Value.initInt(-1),
    }
}
