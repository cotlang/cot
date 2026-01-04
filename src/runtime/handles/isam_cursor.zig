//! ISAM Cursor Handle
//!
//! Manages an ISAM database cursor with read/write operations.
//! Used by both Cot Core Db.* API and DBL channel-based I/O.

const std = @import("std");
const cotdb = @import("cotdb");
const SqliteIsam = cotdb.SqliteIsam;
const MatchMode = cotdb.MatchMode;
const IsamError = cotdb.IsamError;

/// ISAM database cursor
pub const IsamCursor = struct {
    allocator: std.mem.Allocator,

    /// The underlying database handle
    db: ?*SqliteIsam = null,

    /// Table name within the database
    table_name: ?[]const u8 = null,

    /// Current key index for reads (0 = primary key)
    current_key: u8 = 0,

    /// Whether we're positioned on a valid record
    has_current_record: bool = false,

    /// End of file reached
    eof_reached: bool = false,

    /// Record buffer for the current record
    record_buffer: [4096]u8 = undefined,
    record_size: usize = 0,

    const Self = @This();

    /// Open an ISAM database file
    pub fn open(allocator: std.mem.Allocator, filename: []const u8) !Self {
        var self = Self{
            .allocator = allocator,
        };

        // Derive database path (add .db extension if not present)
        const db_path = if (std.mem.endsWith(u8, filename, ".db"))
            try allocator.dupe(u8, filename)
        else blk: {
            const path = try allocator.alloc(u8, filename.len + 3);
            @memcpy(path[0..filename.len], filename);
            @memcpy(path[filename.len..], ".db");
            break :blk path;
        };
        defer allocator.free(db_path);

        // Table name is the filename without path/extension
        const table_name = std.fs.path.stem(std.fs.path.basename(filename));
        self.table_name = try allocator.dupe(u8, table_name);

        // Allocate and open the database
        const db = try allocator.create(SqliteIsam);
        errdefer allocator.destroy(db);

        db.* = SqliteIsam.open(allocator, db_path) catch |err| {
            if (self.table_name) |tn| allocator.free(tn);
            self.table_name = null;
            return err;
        };

        self.db = db;
        return self;
    }

    /// Close the cursor and release resources
    pub fn close(self: *Self) void {
        if (self.db) |db| {
            db.close();
            self.allocator.destroy(db);
            self.db = null;
        }

        if (self.table_name) |tn| {
            self.allocator.free(tn);
            self.table_name = null;
        }

        self.current_key = 0;
        self.has_current_record = false;
        self.eof_reached = false;
        self.record_size = 0;
    }

    /// Read a record by key
    /// Returns the record data
    pub fn read(self: *Self, key_num: u8, key_value: []const u8, match_mode: MatchMode) ![]const u8 {
        const db = self.db orelse return error.NotOpen;
        const table = self.table_name orelse return error.NotOpen;

        try db.read(table, key_num, key_value, match_mode, &self.record_buffer);

        self.current_key = key_num;
        self.has_current_record = true;
        self.eof_reached = false;

        // Get the record size from the database
        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        self.record_size = meta.record_size;

        return self.record_buffer[0..self.record_size];
    }

    /// Read the first record (positions cursor at start)
    pub fn readFirst(self: *Self, key_num: u8) ![]const u8 {
        const db = self.db orelse return error.NotOpen;
        const table = self.table_name orelse return error.NotOpen;

        try db.readFirst(table, key_num, &self.record_buffer);

        self.current_key = key_num;
        self.has_current_record = true;
        self.eof_reached = false;

        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        self.record_size = meta.record_size;

        return self.record_buffer[0..self.record_size];
    }

    /// Read the next sequential record
    pub fn readNext(self: *Self) ![]const u8 {
        const db = self.db orelse return error.NotOpen;
        const table = self.table_name orelse return error.NotOpen;

        db.readNext(&self.record_buffer) catch |err| {
            if (err == IsamError.EndOfFile) {
                self.eof_reached = true;
                self.has_current_record = false;
            }
            return err;
        };

        self.has_current_record = true;

        const meta = db.getTableMeta(table) catch return error.TableNotFound;
        self.record_size = meta.record_size;

        return self.record_buffer[0..self.record_size];
    }

    /// Store (insert) a new record
    pub fn store(self: *Self, record: []const u8) !void {
        const db = self.db orelse return error.NotOpen;
        const table = self.table_name orelse return error.NotOpen;

        _ = try db.store(table, record);

        // Copy to cursor buffer and mark as current
        @memcpy(self.record_buffer[0..record.len], record);
        self.record_size = record.len;
        self.has_current_record = true;
    }

    /// Write (update) the current record
    pub fn write(self: *Self, record: []const u8) !void {
        const db = self.db orelse return error.NotOpen;

        if (!self.has_current_record) return error.NoCurrentRecord;

        try db.write(record);

        // Update cursor buffer
        @memcpy(self.record_buffer[0..record.len], record);
        self.record_size = record.len;
    }

    /// Delete the current record
    pub fn delete(self: *Self) !void {
        const db = self.db orelse return error.NotOpen;

        if (!self.has_current_record) return error.NoCurrentRecord;

        try db.delete();

        self.has_current_record = false;
    }

    /// Find (position without returning data)
    pub fn find(self: *Self, key_num: u8, key_value: []const u8, match_mode: MatchMode) !void {
        _ = try self.read(key_num, key_value, match_mode);
    }

    /// Check if EOF has been reached
    pub fn eof(self: *const Self) bool {
        return self.eof_reached;
    }

    /// Get the current record (if positioned)
    pub fn getCurrentRecord(self: *const Self) ?[]const u8 {
        if (!self.has_current_record) return null;
        return self.record_buffer[0..self.record_size];
    }
};

/// ISAM cursor errors
pub const IsamCursorError = error{
    NotOpen,
    NoCurrentRecord,
    TableNotFound,
};
