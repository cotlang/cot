//! CotDB Connection Manager
//!
//! Manages open database cursors for table access.
//!
//! Each cursor tracks:
//! - The database file and table name
//! - Current position (for sequential reads)
//! - Current key index (for indexed access)
//! - Lock state

const std = @import("std");
const SqliteIsam = @import("sqlite_isam.zig").SqliteIsam;
const MatchMode = @import("sqlite_isam.zig").MatchMode;
const IsamError = @import("sqlite_isam.zig").IsamError;

/// Maximum number of open cursors
pub const MAX_CURSORS: usize = 1024;

/// A database cursor (open connection to a table)
pub const Cursor = struct {
    /// Whether this cursor slot is in use
    is_open: bool = false,

    /// The underlying database handle
    db: ?*SqliteIsam = null,

    /// Table name within the database
    table_name: ?[]const u8 = null,

    /// Current key index for reads (0 = primary key)
    current_key: u8 = 0,

    /// Whether we're positioned on a valid record
    has_current_record: bool = false,

    /// End of file reached
    eof: bool = false,

    /// Record buffer for the current record
    record_buffer: [4096]u8 = undefined,
    record_size: usize = 0,

    const Self = @This();

    pub fn reset(self: *Self) void {
        self.is_open = false;
        self.db = null;
        self.table_name = null;
        self.current_key = 0;
        self.has_current_record = false;
        self.eof = false;
        self.record_size = 0;
    }
};

/// Cursor Manager - manages all open database cursors
pub const CursorManager = struct {
    allocator: std.mem.Allocator,
    cursors: []Cursor, // Heap-allocated cursor array
    next_auto_id: u32,

    const Self = @This();

    /// Initialize a new cursor manager
    pub fn init(allocator: std.mem.Allocator) Self {
        // Allocate cursors on the heap to avoid stack overflow
        const cursors = allocator.alloc(Cursor, MAX_CURSORS) catch {
            // If allocation fails, create with empty slice (no cursors available)
            return Self{
                .allocator = allocator,
                .cursors = &[_]Cursor{},
                .next_auto_id = 1,
            };
        };

        // Initialize all cursor slots
        for (cursors) |*cursor| {
            cursor.* = Cursor{};
        }

        return Self{
            .allocator = allocator,
            .cursors = cursors,
            .next_auto_id = 1,
        };
    }

    /// Clean up and close all cursors
    pub fn deinit(self: *Self) void {
        for (self.cursors, 0..) |*cursor, i| {
            if (cursor.is_open) {
                self.close(@intCast(i));
            }
        }
        if (self.cursors.len > 0) {
            self.allocator.free(self.cursors);
        }
    }

    /// Get next available cursor_id (auto-assignment)
    pub fn getNextId(self: *Self) ?u32 {
        if (self.cursors.len == 0) return null;

        const start = self.next_auto_id;
        var id = start;

        while (true) {
            if (id < self.cursors.len and !self.cursors[id].is_open) {
                self.next_auto_id = if (id + 1 >= self.cursors.len) 1 else @intCast(id + 1);
                return id;
            }
            id = if (id + 1 >= self.cursors.len) 1 else @intCast(id + 1);
            if (id == start) return null; // All slots in use
        }
    }

    /// Open a cursor to a table
    /// filename: path to the .db file (without extension) or full path
    pub fn open(self: *Self, cursor_id: u32, filename: []const u8) !void {
        if (cursor_id >= self.cursors.len) return error.InvalidCursorId;

        var cursor = &self.cursors[cursor_id];
        if (cursor.is_open) {
            self.close(cursor_id);
        }

        cursor.reset();

        // Derive database path (add .db extension if not present)
        const db_path = if (std.mem.endsWith(u8, filename, ".db"))
            try self.allocator.dupe(u8, filename)
        else blk: {
            const path = try self.allocator.alloc(u8, filename.len + 3);
            @memcpy(path[0..filename.len], filename);
            @memcpy(path[filename.len..], ".db");
            break :blk path;
        };
        defer self.allocator.free(db_path);

        // Table name is the filename without path/extension
        const table_name = std.fs.path.stem(std.fs.path.basename(filename));
        cursor.table_name = try self.allocator.dupe(u8, table_name);

        // Allocate and open the database
        const db = try self.allocator.create(SqliteIsam);
        errdefer self.allocator.destroy(db);

        db.* = SqliteIsam.open(self.allocator, db_path) catch |err| {
            if (cursor.table_name) |tn| self.allocator.free(tn);
            cursor.table_name = null;
            return err;
        };

        cursor.db = db;
        cursor.is_open = true;
    }

    /// Close a cursor
    pub fn close(self: *Self, cursor_id: u32) void {
        if (cursor_id >= self.cursors.len) return;

        var cursor = &self.cursors[cursor_id];
        if (!cursor.is_open) return;

        if (cursor.db) |db| {
            db.close();
            self.allocator.destroy(db);
        }

        if (cursor.table_name) |tn| {
            self.allocator.free(tn);
        }

        cursor.reset();
    }

    /// Get a cursor (returns null if not open)
    pub fn get(self: *Self, cursor_id: u32) ?*Cursor {
        if (cursor_id >= self.cursors.len) return null;
        const cursor = &self.cursors[cursor_id];
        if (!cursor.is_open) return null;
        return cursor;
    }

    /// Check if a cursor is open
    pub fn isOpen(self: *Self, cursor_id: u32) bool {
        if (cursor_id >= self.cursors.len) return false;
        return self.cursors[cursor_id].is_open;
    }

    /// Read a record by key
    /// Returns the record data in the cursor's buffer
    pub fn read(self: *Self, cursor_id: u32, key_num: u8, key_value: []const u8, match_mode: MatchMode) ![]const u8 {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;
        const table = cursor.table_name orelse return error.NotOpen;

        try db.read(table, key_num, key_value, match_mode, &cursor.record_buffer);

        cursor.current_key = key_num;
        cursor.has_current_record = true;
        cursor.eof = false;

        // Get the record size from the database
        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        cursor.record_size = meta.record_size;

        return cursor.record_buffer[0..cursor.record_size];
    }

    /// Read the first record (positions cursor at start)
    pub fn readFirst(self: *Self, cursor_id: u32, key_num: u8) ![]const u8 {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;
        const table = cursor.table_name orelse return error.NotOpen;

        try db.readFirst(table, key_num, &cursor.record_buffer);

        cursor.current_key = key_num;
        cursor.has_current_record = true;
        cursor.eof = false;

        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        cursor.record_size = meta.record_size;

        return cursor.record_buffer[0..cursor.record_size];
    }

    /// Read the next sequential record
    pub fn readNext(self: *Self, cursor_id: u32) ![]const u8 {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;
        const table = cursor.table_name orelse return error.NotOpen;

        db.readNext(&cursor.record_buffer) catch |err| {
            if (err == IsamError.EndOfFile) {
                cursor.eof = true;
                cursor.has_current_record = false;
            }
            return err;
        };

        cursor.has_current_record = true;

        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        cursor.record_size = meta.record_size;

        return cursor.record_buffer[0..cursor.record_size];
    }

    /// Store (insert) a new record
    pub fn store(self: *Self, cursor_id: u32, record: []const u8) !void {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;
        const table = cursor.table_name orelse return error.NotOpen;

        _ = try db.store(table, record);

        // Copy to cursor buffer and mark as current
        @memcpy(cursor.record_buffer[0..record.len], record);
        cursor.record_size = record.len;
        cursor.has_current_record = true;
    }

    /// Write (update) the current record
    pub fn write(self: *Self, cursor_id: u32, record: []const u8) !void {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;

        if (!cursor.has_current_record) return error.NoCurrentRecord;

        try db.write(record);

        // Update cursor buffer
        @memcpy(cursor.record_buffer[0..record.len], record);
        cursor.record_size = record.len;
    }

    /// Delete the current record
    pub fn delete(self: *Self, cursor_id: u32) !void {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;

        if (!cursor.has_current_record) return error.NoCurrentRecord;

        try db.delete();

        cursor.has_current_record = false;
    }

    /// Find (position without reading) - sets cursor position
    /// This is the same as read() but semantically indicates positioning
    pub fn find(self: *Self, cursor_id: u32, key_num: u8, key_value: []const u8, match_mode: MatchMode) !void {
        const cursor = self.get(cursor_id) orelse return error.NotOpen;
        const db = cursor.db orelse return error.NotOpen;
        const table = cursor.table_name orelse return error.NotOpen;

        // Use read to position - find and read are the same in SQLite backend
        try db.read(table, key_num, key_value, match_mode, &cursor.record_buffer);

        cursor.current_key = key_num;
        cursor.has_current_record = true;
        cursor.eof = false;

        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        cursor.record_size = meta.record_size;
    }
};

/// Cursor errors
pub const CursorError = error{
    InvalidCursorId,
    NotOpen,
    AlreadyOpen,
    NoCurrentRecord,
    TableNotFound,
};

// Legacy aliases for compatibility
pub const Connection = Cursor;
pub const ConnectionManager = CursorManager;
pub const ConnectionError = CursorError;

test "cursor manager init" {
    const allocator = std.testing.allocator;
    var mgr = CursorManager.init(allocator);
    defer mgr.deinit();

    // All cursors should be closed initially
    try std.testing.expect(!mgr.isOpen(0));
    try std.testing.expect(!mgr.isOpen(1));
}

test "auto id assignment" {
    const allocator = std.testing.allocator;
    var mgr = CursorManager.init(allocator);
    defer mgr.deinit();

    const id1 = mgr.getNextId();
    try std.testing.expect(id1 != null);
    try std.testing.expectEqual(@as(u32, 1), id1.?);
}
