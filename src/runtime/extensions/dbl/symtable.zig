//! DBL Symbol Table API Compatibility Layer
//!
//! Provides NSPC_* functions that wrap the modern OrderedMap type.
//! This allows legacy DBL code to work with the new map implementation.
//!
//! Handle-based API: Integer IDs map to OrderedMap pointers, providing
//! backwards compatibility with DBL's %NSPC_OPEN/%NSPC_CLOSE pattern.

const std = @import("std");
const native = @import("../../native/native.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;
const OrderedMap = @import("../../bytecode/ordered_map.zig").OrderedMap;
const arc = @import("../../bytecode/arc.zig");

/// DBL namespace flags (from namspc.def)
pub const D_NSPC_NONE: i64 = 0;
pub const D_NSPC_CASE: i64 = 1; // Case-sensitive keys
pub const D_NSPC_SPACE: i64 = 2; // Preserve spaces in keys

/// Handle registry: maps integer handles to OrderedMap pointers
/// This is a singleton that persists for the lifetime of the program.
var handle_registry: ?std.AutoHashMap(i64, *OrderedMap) = null;
var next_handle: i64 = 1;
var registry_allocator: ?std.mem.Allocator = null;

/// Initialize the handle registry if needed
fn ensureRegistry(allocator: std.mem.Allocator) !void {
    if (handle_registry == null) {
        handle_registry = std.AutoHashMap(i64, *OrderedMap).init(allocator);
        registry_allocator = allocator;
    }
}

/// Clean up all resources - should be called at program shutdown
pub fn deinit() void {
    if (handle_registry) |*registry| {
        // Clean up all remaining maps
        var iter = registry.iterator();
        while (iter.next()) |entry| {
            const map = entry.value_ptr.*;
            map.deinit();
            if (registry_allocator) |alloc| {
                arc.destroy(alloc, OrderedMap, map);
            }
        }
        registry.deinit();
        handle_registry = null;
    }
    registry_allocator = null;
    next_handle = 1;
}

/// Register all NSPC functions
pub fn register(registry: anytype) !void {
    try registry.registerNative("nspc_open", nspc_open);
    try registry.registerNative("nspc_add", nspc_add);
    try registry.registerNative("nspc_find", nspc_find);
    try registry.registerNative("nspc_delete", nspc_delete);
    try registry.registerNative("nspc_move", nspc_move);
    try registry.registerNative("nspc_getdata", nspc_getdata);
    try registry.registerNative("nspc_putdata", nspc_putdata);
    try registry.registerNative("nspc_reset", nspc_reset);
    try registry.registerNative("nspc_close", nspc_close);
    try registry.registerNative("nspc_count", nspc_count);
}

/// %NSPC_OPEN(flags, size, reserved, reserved, begin_count, expand_count) -> handle
///
/// Creates a new symbol table and returns an integer handle.
/// Only the flags parameter is used; size/reserved/counts are ignored
/// as the modern implementation uses dynamic allocation.
pub fn nspc_open(ctx: *NativeContext) NativeError!?Value {
    try ensureRegistry(ctx.allocator);

    const flags = ctx.getArgInt(0) catch 0;
    // size, reserved, begin_count, expand_count are ignored

    const map_flags = OrderedMap.Flags{
        .case_sensitive = (flags & D_NSPC_CASE) != 0,
        .preserve_spaces = (flags & D_NSPC_SPACE) != 0,
    };

    const map = arc.create(ctx.allocator, OrderedMap) catch return NativeError.OutOfMemory;
    map.* = OrderedMap.init(ctx.allocator, map_flags);

    const handle = next_handle;
    next_handle += 1;
    handle_registry.?.put(handle, map) catch {
        map.deinit();
        arc.destroy(ctx.allocator, OrderedMap, map);
        return NativeError.OutOfMemory;
    };

    return Value.initInt(handle);
}

/// %NSPC_ADD(id, entry_name, data) -> access_code
///
/// Adds a new entry to the symbol table.
/// Returns 1-based access code on success, 0 if key already exists.
pub fn nspc_add(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const data = ctx.getArgString(2) catch "";

    if (handle_registry == null) return NativeError.InvalidArgument;
    const map = handle_registry.?.get(id) orelse return NativeError.InvalidArgument;

    // Check if already exists - NSPC_ADD fails if key exists
    if (map.find(name) != null) {
        return Value.initInt(0); // Already exists
    }

    // Create string value for the data
    const data_value = Value.initString(ctx.allocator, data) catch return NativeError.OutOfMemory;

    const access_code = map.set(name, data_value) catch return NativeError.OutOfMemory;
    return Value.initInt(@intCast(access_code)); // 1-based
}

/// %NSPC_FIND(id, entry_name, data) -> access_code
///
/// Finds an entry by name and optionally returns its data.
/// Returns 1-based access code on success, 0 if not found.
/// The data argument is an output parameter (if provided).
pub fn nspc_find(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    // data output arg (index 2) would need register-based output - for now we just return access code

    if (handle_registry == null) return Value.initInt(0);
    const map = handle_registry.?.get(id) orelse return Value.initInt(0);

    const access_code = map.find(name) orelse return Value.initInt(0);
    return Value.initInt(@intCast(access_code)); // 1-based
}

/// NSPC_DELETE(id, access_code)
///
/// Deletes an entry by access code (1-based position).
/// This is a statement, not a function - returns null.
pub fn nspc_delete(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const access_code = ctx.getArgInt(1) catch return NativeError.InvalidArgument;

    if (handle_registry == null) return null;
    const map = handle_registry.?.get(id) orelse return null;

    _ = map.deleteAt(@intCast(access_code));
    return null;
}

/// NSPC_MOVE(id, from_access, to_access)
///
/// Moves an entry from one position to another.
/// This is a statement, not a function - returns null.
pub fn nspc_move(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const from_code = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const to_code = ctx.getArgInt(2) catch return NativeError.InvalidArgument;

    if (handle_registry == null) return null;
    const map = handle_registry.?.get(id) orelse return null;

    _ = map.move(@intCast(from_code), @intCast(to_code));
    return null;
}

/// NSPC_GETDATA(id, access_code, data)
///
/// Gets the data for an entry at the given access code.
/// Returns the data as a string value.
pub fn nspc_getdata(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const access_code = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    // data output arg (index 2) would need register-based output

    if (handle_registry == null) return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;
    const map = handle_registry.?.get(id) orelse return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;

    const entry = map.getAt(@intCast(access_code)) orelse return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;

    // Return the entry's value (should be a string for DBL compatibility)
    return entry.value;
}

/// NSPC_PUTDATA(id, access_code, data)
///
/// Updates the data for an entry at the given access code.
/// This is a statement - returns null.
pub fn nspc_putdata(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const access_code = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    const data = ctx.getArgString(2) catch "";

    if (handle_registry == null) return null;
    const map = handle_registry.?.get(id) orelse return null;

    const data_value = Value.initString(ctx.allocator, data) catch return NativeError.OutOfMemory;
    _ = map.putAt(@intCast(access_code), data_value);
    return null;
}

/// NSPC_RESET(id)
///
/// Clears all entries from the symbol table.
/// This is a statement - returns null.
pub fn nspc_reset(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    if (handle_registry == null) return null;
    const map = handle_registry.?.get(id) orelse return null;

    map.clear();
    return null;
}

/// NSPC_CLOSE(id)
///
/// Closes and deallocates the symbol table.
/// This is a statement - returns null.
pub fn nspc_close(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    if (handle_registry == null) return null;

    if (handle_registry.?.fetchRemove(id)) |kv| {
        const map = kv.value;
        map.deinit();
        arc.destroy(ctx.allocator, OrderedMap, map);
    }
    return null;
}

/// %NSPC_COUNT(id) -> count
///
/// Returns the number of entries in the symbol table.
/// This is an extension not in original DBL but useful for debugging.
pub fn nspc_count(ctx: *NativeContext) NativeError!?Value {
    const id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    if (handle_registry == null) return Value.initInt(0);
    const map = handle_registry.?.get(id) orelse return Value.initInt(0);

    return Value.initInt(@intCast(map.len()));
}
