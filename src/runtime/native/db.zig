//! Database native functions
//!
//! ISAM file creation and cursor-based database operations

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const cotdb = @import("cotdb");
const sqlite_isam = cotdb.sqlite_isam;
const SqliteIsam = sqlite_isam.SqliteIsam;
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all database functions
pub fn register(registry: anytype) !void {
    // ISAM file operations
    try registry.registerNative("isamc", isamc);
    try registry.registerNative("isutl", isutl);

    // Modern db_* prefix
    try registry.registerNative("db_open", db_open);
    try registry.registerNative("db_close", db_close);
    try registry.registerNative("db_read", db_read);
    try registry.registerNative("db_readnext", db_readnext);
    try registry.registerNative("db_store", db_store);
    try registry.registerNative("db_write", db_write);
    try registry.registerNative("db_delete", db_delete);
    try registry.registerNative("db_find", db_find);

    // Legacy I/O function names
    try registry.registerNative("open", db_open);
    try registry.registerNative("close", db_close);
    try registry.registerNative("read", db_read);
    try registry.registerNative("reads", db_readnext);
    try registry.registerNative("store", db_store);
    try registry.registerNative("write", db_write);
    try registry.registerNative("delete", db_delete);
    try registry.registerNative("find", db_find);
}

/// ISAMC - Create an ISAM file (SQLite backend)
pub fn isamc(ctx: *NativeContext) NativeError!?Value {
    debug.print(.general, "ISAMC called with {d} args", .{ctx.args.len});
    for (ctx.args, 0..) |arg, i| {
        debug.print(.general, "  arg[{d}]: tag={}", .{ i, arg.tag() });
    }
    if (ctx.args.len < 4) return NativeError.InvalidArgument;

    const file_spec = ctx.getArgString(0) catch |err| {
        debug.print(.general, "  getArgString(0) failed: {}", .{err});
        return NativeError.InvalidArgument;
    };
    const rec_size: u32 = @intCast(ctx.getArgInt(1) catch return NativeError.InvalidArgument);
    const num_keys: usize = @intCast(ctx.getArgInt(2) catch return NativeError.InvalidArgument);

    debug.print(.general, "ISAMC: file='{s}' rec_size={d} num_keys={d}", .{ file_spec, rec_size, num_keys });

    if (num_keys == 0 or num_keys > 255) return NativeError.InvalidArgument;

    // Parse key specifications into SqliteIsam format
    var key_defs = std.ArrayListAligned(sqlite_isam.KeyDef, null).empty;
    defer key_defs.deinit(ctx.allocator);

    var key_segments_storage = std.ArrayListAligned([]const sqlite_isam.KeySegment, null).empty;
    defer {
        for (key_segments_storage.items) |segs| {
            ctx.allocator.free(segs);
        }
        key_segments_storage.deinit(ctx.allocator);
    }

    var key_idx: usize = 0;
    var arg_idx: usize = 3;
    while (key_idx < num_keys and arg_idx < ctx.args.len) : ({
        key_idx += 1;
        arg_idx += 1;
    }) {
        const key_spec = ctx.getArgString(arg_idx) catch continue;

        debug.print(.general, "  key_spec[{d}]: '{s}'", .{ key_idx, key_spec });

        const parsed = parseKeySpecSqlite(ctx.allocator, key_spec) catch |err| {
            debug.print(.general, "  parseKeySpec failed: {}", .{err});
            continue;
        };
        key_segments_storage.append(ctx.allocator, parsed.segments) catch return NativeError.OutOfMemory;
        key_defs.append(ctx.allocator, .{
            .name = "key",
            .segments = parsed.segments,
            .unique = !parsed.allow_dups,
            .primary = key_idx == 0,
        }) catch return NativeError.OutOfMemory;
    }

    debug.print(.general, "  parsed {d} key definitions", .{key_defs.items.len});

    if (key_defs.items.len == 0) return NativeError.InvalidArgument;

    // Auto-create parent directory if it doesn't exist
    if (std.fs.path.dirname(file_spec)) |dir| {
        std.fs.cwd().makePath(dir) catch |err| {
            debug.print(.general, "  Could not create directory '{s}': {}", .{ dir, err });
        };
    }

    // Create database path (add .db extension)
    const db_path = ctx.allocator.alloc(u8, file_spec.len + 3) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(db_path);
    @memcpy(db_path[0..file_spec.len], file_spec);
    @memcpy(db_path[file_spec.len..], ".db");

    const table_name = std.fs.path.basename(file_spec);

    const tables = [_]sqlite_isam.TableDef{
        .{
            .name = table_name,
            .record_size = rec_size,
            .keys = key_defs.items,
        },
    };

    // Remove existing database if present (ISAMC overwrites)
    std.fs.cwd().deleteFile(db_path) catch {};

    var sqlite_db = SqliteIsam.create(ctx.allocator, db_path, &tables) catch |err| {
        debug.print(.general, "  SqliteIsam.create failed: {}", .{err});
        return NativeError.FileError;
    };

    sqlite_db.close();
    debug.print(.general, "  SQLite ISAM file created successfully: {s}", .{db_path});

    return null;
}

/// Parse key specification for SQLite ISAM format
fn parseKeySpecSqlite(allocator: std.mem.Allocator, spec: []const u8) !struct {
    segments: []const sqlite_isam.KeySegment,
    allow_dups: bool,
} {
    var start: u16 = 0;
    var length: u16 = 0;
    var key_type: sqlite_isam.KeyType = .string;
    var allow_dups: bool = false;

    var iter = std.mem.splitScalar(u8, spec, ',');
    while (iter.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " ");
        if (trimmed.len == 0) continue;

        if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
            const key = std.mem.trim(u8, trimmed[0..eq_pos], " ");
            const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " ");

            if (std.ascii.eqlIgnoreCase(key, "START")) {
                start = (std.fmt.parseInt(u16, value, 10) catch 1) - 1;
            } else if (std.ascii.eqlIgnoreCase(key, "LENGTH")) {
                length = std.fmt.parseInt(u16, value, 10) catch 0;
            } else if (std.ascii.eqlIgnoreCase(key, "TYPE")) {
                if (std.ascii.eqlIgnoreCase(value, "ALPHA")) {
                    key_type = .string;
                } else if (std.ascii.eqlIgnoreCase(value, "NOCASE")) {
                    key_type = .string_nocase;
                } else if (std.ascii.eqlIgnoreCase(value, "DECIMAL")) {
                    key_type = .decimal;
                } else if (std.ascii.eqlIgnoreCase(value, "INTEGER")) {
                    key_type = .integer;
                }
            }
        } else {
            if (std.ascii.eqlIgnoreCase(trimmed, "DUPS")) {
                allow_dups = true;
            }
        }
    }

    if (length == 0) return error.InvalidKeySpec;

    const segments = allocator.alloc(sqlite_isam.KeySegment, 1) catch return error.OutOfMemory;
    segments[0] = .{
        .start = start,
        .length = length,
        .key_type = key_type,
    };

    return .{
        .segments = segments,
        .allow_dups = allow_dups,
    };
}

/// Parse key specification string (legacy format)
pub fn parseKeySpec(allocator: std.mem.Allocator, spec: []const u8, key_num: u8) !struct {
    segments: []sqlite_isam.KeySegment,
    allow_dups: bool,
    modifiable: bool,
} {
    var starts: [16]u32 = undefined;
    var lengths: [16]u32 = undefined;
    var types: [16]sqlite_isam.KeyType = undefined;
    var start_count: usize = 0;
    var length_count: usize = 0;
    var type_count: usize = 0;

    var allow_dups = false;
    var modifiable = true;
    _ = key_num;

    var iter = std.mem.splitAny(u8, spec, ", ");
    while (iter.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t");
        if (trimmed.len == 0) continue;

        var upper_buf: [64]u8 = undefined;
        const upper = std.ascii.upperString(&upper_buf, trimmed);

        if (std.mem.startsWith(u8, upper, "START=")) {
            const value_part = trimmed[6..];
            start_count = parseIntArray(value_part, &starts);
            for (starts[0..start_count]) |*s| {
                if (s.* > 0) s.* -= 1;
            }
        } else if (std.mem.startsWith(u8, upper, "LENGTH=")) {
            const value_part = trimmed[7..];
            length_count = parseIntArray(value_part, &lengths);
        } else if (std.mem.startsWith(u8, upper, "TYPE=")) {
            const value_part = trimmed[5..];
            type_count = parseTypeArray(value_part, &types);
        } else if (std.mem.eql(u8, upper[0..@min(upper.len, 4)], "DUPS")) {
            allow_dups = true;
        } else if (std.mem.eql(u8, upper[0..@min(upper.len, 8)], "NOMODIFY")) {
            modifiable = false;
        }
    }

    var seg_count = @max(start_count, length_count);
    if (seg_count == 0) seg_count = 1;

    if (seg_count > 0 and length_count == 0) return error.InvalidArgument;

    if (length_count > 0 and start_count == 0) {
        var pos: u32 = 0;
        for (0..length_count) |i| {
            starts[i] = pos;
            pos += lengths[i];
        }
        start_count = length_count;
    }

    const segments = try allocator.alloc(sqlite_isam.KeySegment, seg_count);
    errdefer allocator.free(segments);

    for (0..seg_count) |i| {
        segments[i] = .{
            .start = if (i < start_count) starts[i] else 0,
            .length = if (i < length_count) lengths[i] else 0,
            .key_type = if (i < type_count) types[i] else .string,
        };

        if (segments[i].length == 0) {
            allocator.free(segments);
            return error.InvalidArgument;
        }
    }

    return .{
        .segments = segments,
        .allow_dups = allow_dups,
        .modifiable = modifiable,
    };
}

fn parseIntArray(value: []const u8, out: []u32) usize {
    var count: usize = 0;
    var iter = std.mem.splitScalar(u8, value, ':');
    while (iter.next()) |num_str| {
        if (count >= out.len) break;
        const trimmed = std.mem.trim(u8, num_str, " \t");
        out[count] = std.fmt.parseInt(u32, trimmed, 10) catch 0;
        count += 1;
    }
    return count;
}

fn parseTypeArray(value: []const u8, out: []sqlite_isam.KeyType) usize {
    var count: usize = 0;
    var iter = std.mem.splitScalar(u8, value, ':');
    while (iter.next()) |type_str| {
        if (count >= out.len) break;
        const trimmed = std.mem.trim(u8, type_str, " \t");
        var upper_buf: [16]u8 = undefined;
        const upper = std.ascii.upperString(&upper_buf, trimmed);

        out[count] = if (std.mem.eql(u8, upper[0..@min(upper.len, 6)], "NOCASE"))
            sqlite_isam.KeyType.string_nocase
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 7)], "DECIMAL"))
            sqlite_isam.KeyType.decimal
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 7)], "INTEGER"))
            sqlite_isam.KeyType.integer
        else if (std.mem.eql(u8, upper[0..@min(upper.len, 6)], "PACKED"))
            sqlite_isam.KeyType.binary_packed
        else
            sqlite_isam.KeyType.string;

        count += 1;
    }
    return count;
}

/// ISUTL - ISAM utility functions
pub fn isutl(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    return null;
}

/// DB_OPEN - Open a database cursor
/// Supports both 2-arg (cursor_id, filename) and 3-arg (cursor_id, mode, filename) forms
/// The 3-arg form is used by DBL's open(channel, mode, filename) statement
pub fn db_open(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    // Handle both 2-arg and 3-arg forms:
    // 2-arg: db_open(cursor_id, filename)
    // 3-arg: open(cursor_id, mode, filename) - DBL's native form
    const filename = if (ctx.args.len >= 3)
        ctx.getArgString(2) catch return NativeError.InvalidArgument
    else
        ctx.getArgString(1) catch return NativeError.InvalidArgument;

    debug.print(.general, "db_open: cursor={d} filename='{s}'", .{ cursor_id, filename });

    cursors.open(@intCast(cursor_id), filename) catch |err| {
        debug.print(.general, "db_open failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// DB_CLOSE - Close a database cursor
pub fn db_close(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    cursors.close(@intCast(cursor_id));

    return Value.initInt(0);
}

/// DB_READ - Read a record by key
pub fn db_read(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const key_num: u8 = @intCast(ctx.getArgInt(1) catch 0);

    var key_buf: [32]u8 = undefined;
    const key_value: []const u8 = blk: {
        const str_val = ctx.getArgString(2) catch "";
        if (str_val.len > 0) {
            break :blk str_val;
        }
        const int_val = ctx.getArgInt(2) catch 0;
        const formatted = std.fmt.bufPrint(&key_buf, "{d:0>6}", .{int_val}) catch "";
        break :blk formatted;
    };

    debug.print(.general, "db_read: cursor={d} key_num={d} key_value='{s}'", .{ cursor_id, key_num, key_value });

    const record = cursors.read(@intCast(cursor_id), key_num, key_value, .greater_equal) catch |err| {
        debug.print(.general, "db_read failed: {}", .{err});
        return Value.initString(ctx.allocator, "") catch return NativeError.OutOfMemory;
    };

    const result = ctx.allocator.dupe(u8, record) catch return NativeError.OutOfMemory;
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// DB_READNEXT - Read the next sequential record
pub fn db_readnext(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    const record = cursors.readNext(@intCast(cursor_id)) catch |err| {
        debug.print(.general, "db_readnext: {}", .{err});
        return NativeError.EndOfFile;
    };

    const result = ctx.allocator.dupe(u8, record) catch return NativeError.OutOfMemory;
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// DB_STORE - Insert a new record
pub fn db_store(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const record = ctx.getArgString(1) catch |err| {
        debug.print(.general, "db_store: getArgString(1) failed: {} - args.len={d}", .{ err, ctx.args.len });
        for (ctx.args, 0..) |arg, i| {
            debug.print(.general, "  arg[{d}]: tag={}", .{ i, arg.tag() });
        }
        return NativeError.InvalidArgument;
    };

    debug.print(.general, "db_store: cursor={d} record.len={d}", .{ cursor_id, record.len });

    cursors.store(@intCast(cursor_id), record) catch |err| {
        debug.print(.general, "db_store failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// DB_WRITE - Update the current record
pub fn db_write(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const record = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    cursors.write(@intCast(cursor_id), record) catch |err| {
        debug.print(.general, "db_write failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// DB_DELETE - Delete the current record
pub fn db_delete(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;

    cursors.delete(@intCast(cursor_id)) catch |err| {
        debug.print(.general, "db_delete failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// DB_FIND - Position cursor without reading
pub fn db_find(ctx: *NativeContext) NativeError!?Value {
    const cursors = ctx.cursors orelse return NativeError.NotImplemented;

    const cursor_id = ctx.getArgInt(0) catch return NativeError.InvalidArgument;
    const key_num: u8 = @intCast(ctx.getArgInt(1) catch 0);
    const key_value = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const mode_str = ctx.getArgString(3) catch "exact";

    const match_mode: cotdb.MatchMode = blk: {
        var lower_buf: [16]u8 = undefined;
        const lower = std.ascii.lowerString(&lower_buf, mode_str);
        if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "ge") or
            std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "greater_equal"))
        {
            break :blk .greater_equal;
        } else if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "gt") or
            std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "greater"))
        {
            break :blk .greater;
        } else if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "partial")) {
            break :blk .partial;
        } else {
            break :blk .exact;
        }
    };

    cursors.find(@intCast(cursor_id), key_num, key_value, match_mode) catch |err| {
        debug.print(.general, "db_find failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}
