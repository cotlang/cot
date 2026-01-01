const std = @import("std");
const c = @cImport({
    @cInclude("sqlite3.h");
});
const ulid_mod = @import("ulid.zig");
pub const ULID = ulid_mod.ULID;

// SQLite destructor constant - using null (SQLITE_STATIC) since our data
// remains valid during statement execution
const SQLITE_STATIC: c.sqlite3_destructor_type = null;

/// Key types for index comparisons
pub const KeyType = enum {
    string, // Case-sensitive string comparison
    string_nocase, // Case-insensitive string comparison
    decimal, // Numeric string (right-aligned, zero-padded)
    integer, // Binary integer
    binary_packed, // Packed binary format
    descending, // Descending order (any base type)
};

/// A segment within a composite key
pub const KeySegment = struct {
    start: u16, // Starting byte position in record
    length: u16, // Length in bytes
    key_type: KeyType,
    descending: bool = false,
};

/// Key definition with multiple segments for composite keys
pub const KeyDef = struct {
    name: []const u8,
    segments: []const KeySegment,
    unique: bool = false,
    primary: bool = false,
};

/// Table definition
pub const TableDef = struct {
    name: []const u8,
    record_size: u32,
    keys: []const KeyDef,
};

/// Match modes for read operations
pub const MatchMode = enum {
    exact, // Exact match only
    greater_equal, // >= key value
    greater, // > key value
    partial, // Prefix match
};

/// Error types
pub const IsamError = error{
    DatabaseError,
    TableNotFound,
    KeyNotFound,
    RecordNotFound,
    DuplicateKey,
    InvalidRecord,
    InvalidKey,
    EndOfFile,
    NotOpen,
    AlreadyOpen,
    OutOfMemory,
};

/// SQLite-backed ISAM database
pub const SqliteIsam = struct {
    db: ?*c.sqlite3,
    allocator: std.mem.Allocator,
    path: []const u8,

    // Current position tracking per table
    current_table: ?[]const u8,
    current_key_index: u8,
    current_rowid: ?i64,
    current_ulid: ?ULID,

    // Prepared statements cache (lazily created)
    stmt_cache: std.StringHashMap(*c.sqlite3_stmt),

    const Self = @This();

    /// Create a new ISAM database with the given tables
    pub fn create(allocator: std.mem.Allocator, path: []const u8, tables: []const TableDef) IsamError!Self {
        var self = Self{
            .db = null,
            .allocator = allocator,
            .path = allocator.dupe(u8, path) catch return IsamError.OutOfMemory,
            .current_table = null,
            .current_key_index = 0,
            .current_rowid = null,
            .current_ulid = null,
            .stmt_cache = std.StringHashMap(*c.sqlite3_stmt).init(allocator),
        };

        // Open/create the database
        const path_z = allocator.dupeZ(u8, path) catch return IsamError.OutOfMemory;
        defer allocator.free(path_z);

        const open_rc = c.sqlite3_open(path_z.ptr, &self.db);
        if (open_rc != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Enable WAL mode for better concurrency
        self.execSimple("PRAGMA journal_mode=WAL") catch return IsamError.DatabaseError;
        self.execSimple("PRAGMA synchronous=NORMAL") catch return IsamError.DatabaseError;

        // Create metadata tables
        self.execSimple(
            \\CREATE TABLE IF NOT EXISTS _isam_tables (
            \\    name TEXT PRIMARY KEY,
            \\    record_size INTEGER NOT NULL
            \\)
        ) catch return IsamError.DatabaseError;

        self.execSimple(
            \\CREATE TABLE IF NOT EXISTS _isam_keys (
            \\    table_name TEXT NOT NULL,
            \\    key_index INTEGER NOT NULL,
            \\    key_name TEXT NOT NULL,
            \\    is_unique INTEGER NOT NULL,
            \\    is_primary INTEGER NOT NULL,
            \\    PRIMARY KEY (table_name, key_index)
            \\)
        ) catch return IsamError.DatabaseError;

        self.execSimple(
            \\CREATE TABLE IF NOT EXISTS _isam_segments (
            \\    table_name TEXT NOT NULL,
            \\    key_index INTEGER NOT NULL,
            \\    segment_index INTEGER NOT NULL,
            \\    start_pos INTEGER NOT NULL,
            \\    length INTEGER NOT NULL,
            \\    key_type TEXT NOT NULL,
            \\    descending INTEGER NOT NULL,
            \\    PRIMARY KEY (table_name, key_index, segment_index)
            \\)
        ) catch return IsamError.DatabaseError;

        // Create each user table
        for (tables) |table| {
            self.createTable(table) catch return IsamError.DatabaseError;
        }

        return self;
    }

    /// Open an existing ISAM database
    pub fn open(allocator: std.mem.Allocator, path: []const u8) IsamError!Self {
        var self = Self{
            .db = null,
            .allocator = allocator,
            .path = allocator.dupe(u8, path) catch return IsamError.OutOfMemory,
            .current_table = null,
            .current_key_index = 0,
            .current_rowid = null,
            .current_ulid = null,
            .stmt_cache = std.StringHashMap(*c.sqlite3_stmt).init(allocator),
        };

        const path_z = allocator.dupeZ(u8, path) catch return IsamError.OutOfMemory;
        defer allocator.free(path_z);

        if (c.sqlite3_open(path_z.ptr, &self.db) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Verify it's a valid ISAM database
        self.execSimple("SELECT 1 FROM _isam_tables LIMIT 1") catch return IsamError.DatabaseError;

        return self;
    }

    /// Close the database
    pub fn close(self: *Self) void {
        // Finalize all cached statements
        var it = self.stmt_cache.iterator();
        while (it.next()) |entry| {
            _ = c.sqlite3_finalize(entry.value_ptr.*);
        }
        self.stmt_cache.deinit();

        if (self.db) |db| {
            _ = c.sqlite3_close(db);
            self.db = null;
        }

        self.allocator.free(self.path);
    }

    /// Store a new record, returns the generated ULID
    pub fn store(self: *Self, table: []const u8, record: []const u8) IsamError!ULID {
        const new_ulid = ULID.new();
        try self.storeWithUlid(table, record, new_ulid);
        return new_ulid;
    }

    /// Store a record with a specific ULID
    pub fn storeWithUlid(self: *Self, table: []const u8, record: []const u8, record_ulid: ULID) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;

        // Get table metadata
        const table_meta = self.getTableMeta(table) catch return IsamError.TableNotFound;
        defer self.allocator.free(table_meta.keys_json);

        if (record.len != table_meta.record_size) {
            return IsamError.InvalidRecord;
        }

        // Build INSERT statement with key columns
        const key_count = self.getKeyCount(table) catch return IsamError.TableNotFound;

        // Build SQL: INSERT INTO table (_ulid, _record, _key0, _key1, ...) VALUES (?, ?, ?, ?, ...)
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..], "INSERT INTO \"{s}\" (_ulid, _record", .{table}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (0..key_count) |i| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d}", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;
        }

        const values_prefix = std.fmt.bufPrint(sql_buf[sql_len..], ") VALUES (?, ?", .{}) catch return IsamError.DatabaseError;
        sql_len += values_prefix.len;

        for (0..key_count) |_| {
            const placeholder = std.fmt.bufPrint(sql_buf[sql_len..], ", ?", .{}) catch return IsamError.DatabaseError;
            sql_len += placeholder.len;
        }

        sql_buf[sql_len] = ')';
        sql_len += 1;
        sql_buf[sql_len] = 0;

        // Prepare statement
        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, &sql_buf, @intCast(sql_len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // Bind ULID
        if (c.sqlite3_bind_blob(stmt, 1, &record_ulid.bytes, 16, SQLITE_STATIC) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Bind record
        if (c.sqlite3_bind_blob(stmt, 2, record.ptr, @intCast(record.len), SQLITE_STATIC) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Extract and bind each key - must keep values alive until after sqlite3_step
        var key_values: [16]?[]u8 = .{null} ** 16;
        defer {
            for (key_values) |maybe_kv| {
                if (maybe_kv) |kv| self.allocator.free(kv);
            }
        }

        for (0..key_count) |key_idx| {
            const key_value = self.extractKey(table, @intCast(key_idx), record) catch return IsamError.DatabaseError;
            key_values[key_idx] = key_value;

            if (c.sqlite3_bind_blob(stmt, @intCast(3 + key_idx), key_value.ptr, @intCast(key_value.len), SQLITE_STATIC) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
        }

        // Execute
        const rc = c.sqlite3_step(stmt);
        if (rc == c.SQLITE_CONSTRAINT) {
            return IsamError.DuplicateKey;
        } else if (rc != c.SQLITE_DONE) {
            return IsamError.DatabaseError;
        }

        // Update current position
        self.current_table = table;
        self.current_rowid = c.sqlite3_last_insert_rowid(db);
        self.current_ulid = record_ulid;
    }

    /// Read by key value
    pub fn read(self: *Self, table: []const u8, key_index: u8, key_value: []const u8, mode: MatchMode, record_buf: []u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;

        // Build SELECT based on match mode
        var sql_buf: [1024]u8 = undefined;
        const op = switch (mode) {
            .exact => "=",
            .greater_equal => ">=",
            .greater => ">",
            .partial => "LIKE",
        };

        const sql = if (mode == .partial)
            std.fmt.bufPrintZ(&sql_buf, "SELECT rowid, _ulid, _record FROM \"{s}\" WHERE _key{d} LIKE ? || '%' ORDER BY _key{d} LIMIT 1", .{ table, key_index, key_index }) catch return IsamError.DatabaseError
        else
            std.fmt.bufPrintZ(&sql_buf, "SELECT rowid, _ulid, _record FROM \"{s}\" WHERE _key{d} {s} ? ORDER BY _key{d} LIMIT 1", .{ table, key_index, op, key_index }) catch return IsamError.DatabaseError;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql.ptr, @intCast(sql.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // Bind key value
        if (c.sqlite3_bind_blob(stmt, 1, key_value.ptr, @intCast(key_value.len), SQLITE_STATIC) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Execute
        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return IsamError.RecordNotFound;
        }

        // Get results
        const rowid = c.sqlite3_column_int64(stmt, 0);
        const ulid_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 1));
        const record_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 2));
        const record_len: usize = @intCast(c.sqlite3_column_bytes(stmt, 2));

        if (record_ptr == null) {
            return IsamError.RecordNotFound;
        }

        if (record_len > record_buf.len) {
            return IsamError.InvalidRecord;
        }

        @memcpy(record_buf[0..record_len], record_ptr.?[0..record_len]);

        // Update current position
        self.current_table = table;
        self.current_key_index = key_index;
        self.current_rowid = rowid;
        if (ulid_ptr) |ptr| {
            self.current_ulid = ULID{ .bytes = ptr[0..16].* };
        }
    }

    /// Read by ULID
    pub fn readByUlid(self: *Self, table: []const u8, record_ulid: ULID, record_buf: []u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;

        var sql_buf: [256]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf, "SELECT rowid, _record FROM \"{s}\" WHERE _ulid = ?", .{table}) catch return IsamError.DatabaseError;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql.ptr, @intCast(sql.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        if (c.sqlite3_bind_blob(stmt, 1, &record_ulid.bytes, 16, SQLITE_STATIC) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return IsamError.RecordNotFound;
        }

        const rowid = c.sqlite3_column_int64(stmt, 0);
        const record_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 1));
        const record_len: usize = @intCast(c.sqlite3_column_bytes(stmt, 1));

        if (record_ptr == null or record_len > record_buf.len) {
            return IsamError.RecordNotFound;
        }

        @memcpy(record_buf[0..record_len], record_ptr.?[0..record_len]);

        self.current_table = table;
        self.current_rowid = rowid;
        self.current_ulid = record_ulid;
    }

    /// Read next sequential record by current key
    pub fn readNext(self: *Self, record_buf: []u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const current_rowid = self.current_rowid orelse return IsamError.RecordNotFound;

        var sql_buf: [512]u8 = undefined;
        const ki = self.current_key_index;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT rowid, _ulid, _record FROM "{s}"
            \\WHERE _key{d} > (SELECT _key{d} FROM "{s}" WHERE rowid = ?)
            \\   OR (_key{d} = (SELECT _key{d} FROM "{s}" WHERE rowid = ?) AND rowid > ?)
            \\ORDER BY _key{d}, rowid LIMIT 1
        , .{ table, ki, ki, table, ki, ki, table, ki }) catch return IsamError.DatabaseError;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql.ptr, @intCast(sql.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // Bind current rowid for all three placeholders
        if (c.sqlite3_bind_int64(stmt, 1, current_rowid) != c.SQLITE_OK) return IsamError.DatabaseError;
        if (c.sqlite3_bind_int64(stmt, 2, current_rowid) != c.SQLITE_OK) return IsamError.DatabaseError;
        if (c.sqlite3_bind_int64(stmt, 3, current_rowid) != c.SQLITE_OK) return IsamError.DatabaseError;

        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return IsamError.EndOfFile;
        }

        const rowid = c.sqlite3_column_int64(stmt, 0);
        const ulid_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 1));
        const record_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 2));
        const record_len: usize = @intCast(c.sqlite3_column_bytes(stmt, 2));

        if (record_ptr == null or record_len > record_buf.len) {
            return IsamError.RecordNotFound;
        }

        @memcpy(record_buf[0..record_len], record_ptr.?[0..record_len]);

        self.current_rowid = rowid;
        if (ulid_ptr) |ptr| {
            self.current_ulid = ULID{ .bytes = ptr[0..16].* };
        }
    }

    /// Read the first record in key order
    pub fn readFirst(self: *Self, table: []const u8, key_index: u8, record_buf: []u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;

        var sql_buf: [256]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT rowid, _ulid, _record FROM "{s}"
            \\ORDER BY _key{d}, rowid LIMIT 1
        , .{ table, key_index }) catch return IsamError.DatabaseError;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql.ptr, @intCast(sql.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return IsamError.EndOfFile;
        }

        const rowid = c.sqlite3_column_int64(stmt, 0);
        const ulid_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 1));
        const record_ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, 2));
        const record_len: usize = @intCast(c.sqlite3_column_bytes(stmt, 2));

        if (record_ptr == null or record_len > record_buf.len) {
            return IsamError.RecordNotFound;
        }

        @memcpy(record_buf[0..record_len], record_ptr.?[0..record_len]);

        // Update current position
        self.current_table = table;
        self.current_key_index = key_index;
        self.current_rowid = rowid;
        if (ulid_ptr) |ptr| {
            self.current_ulid = ULID{ .bytes = ptr[0..16].* };
        }
    }

    /// Update current record
    pub fn write(self: *Self, record: []const u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const rowid = self.current_rowid orelse return IsamError.RecordNotFound;

        const key_count = self.getKeyCount(table) catch return IsamError.TableNotFound;

        // Build UPDATE statement
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..], "UPDATE \"{s}\" SET _record = ?", .{table}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (0..key_count) |i| {
            const key_set = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d} = ?", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_set.len;
        }

        const where = std.fmt.bufPrint(sql_buf[sql_len..], " WHERE rowid = ?", .{}) catch return IsamError.DatabaseError;
        sql_len += where.len;
        sql_buf[sql_len] = 0;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, &sql_buf, @intCast(sql_len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        // Bind record
        if (c.sqlite3_bind_blob(stmt, 1, record.ptr, @intCast(record.len), SQLITE_STATIC) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Extract and bind each key - must keep values alive until after sqlite3_step
        var key_values: [16]?[]u8 = .{null} ** 16;
        defer {
            for (key_values) |maybe_kv| {
                if (maybe_kv) |kv| self.allocator.free(kv);
            }
        }

        for (0..key_count) |key_idx| {
            const key_value = self.extractKey(table, @intCast(key_idx), record) catch return IsamError.DatabaseError;
            key_values[key_idx] = key_value;

            if (c.sqlite3_bind_blob(stmt, @intCast(2 + key_idx), key_value.ptr, @intCast(key_value.len), SQLITE_STATIC) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
        }

        // Bind rowid
        if (c.sqlite3_bind_int64(stmt, @intCast(2 + key_count), rowid) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return IsamError.DatabaseError;
        }
    }

    /// Delete current record
    pub fn delete(self: *Self) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const rowid = self.current_rowid orelse return IsamError.RecordNotFound;

        var sql_buf: [256]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf, "DELETE FROM \"{s}\" WHERE rowid = ?", .{table}) catch return IsamError.DatabaseError;

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql.ptr, @intCast(sql.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        if (c.sqlite3_bind_int64(stmt, 1, rowid) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
            return IsamError.DatabaseError;
        }

        self.current_rowid = null;
        self.current_ulid = null;
    }

    /// Get current record's ULID
    pub fn getCurrentUlid(self: *Self) ?ULID {
        return self.current_ulid;
    }

    /// Execute raw SQL query (for native SQL access)
    pub fn execSql(self: *Self, sql: []const u8) IsamError!void {
        const db = self.db orelse return IsamError.NotOpen;

        const sql_z = self.allocator.dupeZ(u8, sql) catch return IsamError.OutOfMemory;
        defer self.allocator.free(sql_z);

        if (c.sqlite3_exec(db, sql_z.ptr, null, null, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
    }

    /// Query with results (for native SQL access)
    pub fn query(self: *Self, sql: []const u8) IsamError!QueryResult {
        const db = self.db orelse return IsamError.NotOpen;

        const sql_z = self.allocator.dupeZ(u8, sql) catch return IsamError.OutOfMemory;
        defer self.allocator.free(sql_z);

        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql_z.ptr, @intCast(sql_z.len), &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        return QueryResult{
            .stmt = stmt,
            .allocator = self.allocator,
        };
    }

    // ============ Private helpers ============

    fn createTable(self: *Self, table: TableDef) !void {
        const db = self.db orelse return IsamError.NotOpen;

        // Insert table metadata
        {
            const sql = "INSERT INTO _isam_tables (name, record_size) VALUES (?, ?)";
            var stmt: ?*c.sqlite3_stmt = null;
            if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
            defer _ = c.sqlite3_finalize(stmt);

            const name_z = self.allocator.dupeZ(u8, table.name) catch return IsamError.OutOfMemory;
            defer self.allocator.free(name_z);

            _ = c.sqlite3_bind_text(stmt, 1, name_z.ptr, @intCast(table.name.len), SQLITE_STATIC);
            _ = c.sqlite3_bind_int(stmt, 2, @intCast(table.record_size));

            if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
                return IsamError.DatabaseError;
            }
        }

        // Build CREATE TABLE statement
        var sql_buf: [8192]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..], "CREATE TABLE \"{s}\" (_ulid BLOB NOT NULL UNIQUE, _record BLOB NOT NULL", .{table.name}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (table.keys, 0..) |key, key_idx| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d} BLOB", .{key_idx}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;

            // Store key metadata
            try self.storeKeyMeta(table.name, @intCast(key_idx), key);
        }

        sql_buf[sql_len] = ')';
        sql_len += 1;
        sql_buf[sql_len] = 0;

        if (c.sqlite3_exec(db, &sql_buf, null, null, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }

        // Create indexes
        for (table.keys, 0..) |key, key_idx| {
            var idx_buf: [512]u8 = undefined;
            const unique_str = if (key.unique) "UNIQUE " else "";
            const idx_sql = std.fmt.bufPrintZ(&idx_buf, "CREATE {s}INDEX \"idx_{s}_key{d}\" ON \"{s}\" (_key{d})", .{ unique_str, table.name, key_idx, table.name, key_idx }) catch return IsamError.DatabaseError;

            if (c.sqlite3_exec(db, idx_sql.ptr, null, null, null) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
        }
    }

    fn storeKeyMeta(self: *Self, table_name: []const u8, key_index: u8, key: KeyDef) !void {
        const db = self.db orelse return IsamError.NotOpen;

        // Store key definition
        {
            const sql = "INSERT INTO _isam_keys (table_name, key_index, key_name, is_unique, is_primary) VALUES (?, ?, ?, ?, ?)";
            var stmt: ?*c.sqlite3_stmt = null;
            if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
            defer _ = c.sqlite3_finalize(stmt);

            const table_z = self.allocator.dupeZ(u8, table_name) catch return IsamError.OutOfMemory;
            defer self.allocator.free(table_z);
            const name_z = self.allocator.dupeZ(u8, key.name) catch return IsamError.OutOfMemory;
            defer self.allocator.free(name_z);

            _ = c.sqlite3_bind_text(stmt, 1, table_z.ptr, @intCast(table_name.len), SQLITE_STATIC);
            _ = c.sqlite3_bind_int(stmt, 2, key_index);
            _ = c.sqlite3_bind_text(stmt, 3, name_z.ptr, @intCast(key.name.len), SQLITE_STATIC);
            _ = c.sqlite3_bind_int(stmt, 4, if (key.unique) 1 else 0);
            _ = c.sqlite3_bind_int(stmt, 5, if (key.primary) 1 else 0);

            if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
                return IsamError.DatabaseError;
            }
        }

        // Store segments
        for (key.segments, 0..) |seg, seg_idx| {
            const sql = "INSERT INTO _isam_segments (table_name, key_index, segment_index, start_pos, length, key_type, descending) VALUES (?, ?, ?, ?, ?, ?, ?)";
            var stmt: ?*c.sqlite3_stmt = null;
            if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
                return IsamError.DatabaseError;
            }
            defer _ = c.sqlite3_finalize(stmt);

            const table_z = self.allocator.dupeZ(u8, table_name) catch return IsamError.OutOfMemory;
            defer self.allocator.free(table_z);

            const type_str = @tagName(seg.key_type);

            _ = c.sqlite3_bind_text(stmt, 1, table_z.ptr, @intCast(table_name.len), SQLITE_STATIC);
            _ = c.sqlite3_bind_int(stmt, 2, key_index);
            _ = c.sqlite3_bind_int(stmt, 3, @intCast(seg_idx));
            _ = c.sqlite3_bind_int(stmt, 4, seg.start);
            _ = c.sqlite3_bind_int(stmt, 5, seg.length);
            _ = c.sqlite3_bind_text(stmt, 6, type_str.ptr, @intCast(type_str.len), SQLITE_STATIC);
            _ = c.sqlite3_bind_int(stmt, 7, if (seg.descending) 1 else 0);

            if (c.sqlite3_step(stmt) != c.SQLITE_DONE) {
                return IsamError.DatabaseError;
            }
        }
    }

    fn execSimple(self: *Self, sql: [*:0]const u8) !void {
        const db = self.db orelse return IsamError.NotOpen;
        if (c.sqlite3_exec(db, sql, null, null, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
    }

    const TableMeta = struct {
        record_size: u32,
        keys_json: []u8, // placeholder for now
    };

    pub fn getTableMeta(self: *Self, table: []const u8) !TableMeta {
        const db = self.db orelse return IsamError.NotOpen;

        const sql = "SELECT record_size FROM _isam_tables WHERE name = ?";
        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        _ = c.sqlite3_bind_text(stmt, 1, table_z.ptr, @intCast(table.len), SQLITE_STATIC);

        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return IsamError.TableNotFound;
        }

        return TableMeta{
            .record_size = @intCast(c.sqlite3_column_int(stmt, 0)),
            .keys_json = self.allocator.alloc(u8, 0) catch return IsamError.OutOfMemory,
        };
    }

    fn getKeyCount(self: *Self, table: []const u8) !usize {
        const db = self.db orelse return IsamError.NotOpen;

        const sql = "SELECT COUNT(*) FROM _isam_keys WHERE table_name = ?";
        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        _ = c.sqlite3_bind_text(stmt, 1, table_z.ptr, @intCast(table.len), SQLITE_STATIC);

        if (c.sqlite3_step(stmt) != c.SQLITE_ROW) {
            return 0;
        }

        return @intCast(c.sqlite3_column_int(stmt, 0));
    }

    fn extractKey(self: *Self, table: []const u8, key_index: u8, record: []const u8) ![]u8 {
        const db = self.db orelse return IsamError.NotOpen;

        // Get segments for this key
        const sql = "SELECT start_pos, length FROM _isam_segments WHERE table_name = ? AND key_index = ? ORDER BY segment_index";
        var stmt: ?*c.sqlite3_stmt = null;
        if (c.sqlite3_prepare_v2(db, sql, -1, &stmt, null) != c.SQLITE_OK) {
            return IsamError.DatabaseError;
        }
        defer _ = c.sqlite3_finalize(stmt);

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        _ = c.sqlite3_bind_text(stmt, 1, table_z.ptr, @intCast(table.len), SQLITE_STATIC);
        _ = c.sqlite3_bind_int(stmt, 2, key_index);

        // Calculate total key length and extract segments
        var total_len: usize = 0;
        var segments: [16]struct { start: usize, len: usize } = undefined;
        var seg_count: usize = 0;

        while (c.sqlite3_step(stmt) == c.SQLITE_ROW) {
            const start: usize = @intCast(c.sqlite3_column_int(stmt, 0));
            const len: usize = @intCast(c.sqlite3_column_int(stmt, 1));
            segments[seg_count] = .{ .start = start, .len = len };
            total_len += len;
            seg_count += 1;
        }

        // Build composite key
        var key_buf = self.allocator.alloc(u8, total_len) catch return IsamError.OutOfMemory;
        var offset: usize = 0;

        for (segments[0..seg_count]) |seg| {
            if (seg.start + seg.len <= record.len) {
                @memcpy(key_buf[offset .. offset + seg.len], record[seg.start .. seg.start + seg.len]);
            }
            offset += seg.len;
        }

        return key_buf;
    }
};

/// Result set from a SQL query
pub const QueryResult = struct {
    stmt: ?*c.sqlite3_stmt,
    allocator: std.mem.Allocator,

    pub fn next(self: *QueryResult) bool {
        if (self.stmt) |stmt| {
            return c.sqlite3_step(stmt) == c.SQLITE_ROW;
        }
        return false;
    }

    pub fn columnCount(self: *QueryResult) usize {
        if (self.stmt) |stmt| {
            return @intCast(c.sqlite3_column_count(stmt));
        }
        return 0;
    }

    pub fn columnText(self: *QueryResult, col: usize) ?[]const u8 {
        if (self.stmt) |stmt| {
            const ptr: ?[*:0]const u8 = @ptrCast(c.sqlite3_column_text(stmt, @intCast(col)));
            if (ptr) |p| {
                const len: usize = @intCast(c.sqlite3_column_bytes(stmt, @intCast(col)));
                return p[0..len];
            }
        }
        return null;
    }

    pub fn columnBlob(self: *QueryResult, col: usize) ?[]const u8 {
        if (self.stmt) |stmt| {
            const ptr: ?[*]const u8 = @ptrCast(c.sqlite3_column_blob(stmt, @intCast(col)));
            if (ptr) |p| {
                const len: usize = @intCast(c.sqlite3_column_bytes(stmt, @intCast(col)));
                return p[0..len];
            }
        }
        return null;
    }

    pub fn columnInt(self: *QueryResult, col: usize) i64 {
        if (self.stmt) |stmt| {
            return c.sqlite3_column_int64(stmt, @intCast(col));
        }
        return 0;
    }

    pub fn deinit(self: *QueryResult) void {
        if (self.stmt) |stmt| {
            _ = c.sqlite3_finalize(stmt);
            self.stmt = null;
        }
    }
};

// ============ Tests ============

test "create database with tables" {
    const allocator = std.testing.allocator;

    const tables = [_]TableDef{
        .{
            .name = "customers",
            .record_size = 100,
            .keys = &[_]KeyDef{
                .{
                    .name = "by_id",
                    .segments = &[_]KeySegment{
                        .{ .start = 0, .length = 6, .key_type = .decimal },
                    },
                    .unique = true,
                    .primary = true,
                },
                .{
                    .name = "by_name",
                    .segments = &[_]KeySegment{
                        .{ .start = 6, .length = 30, .key_type = .alpha },
                    },
                },
            },
        },
    };

    // Create in-memory database for testing
    var db = try SqliteIsam.create(allocator, ":memory:", &tables);
    defer db.close();

    // Store a record
    var record: [100]u8 = undefined;
    @memset(&record, ' ');
    @memcpy(record[0..6], "000001");
    @memcpy(record[6..16], "John Smith");

    const ulid = try db.store("customers", &record);
    try std.testing.expect(ulid.bytes[0] != 0);

    // Read by key
    var read_buf: [100]u8 = undefined;
    try db.read("customers", 0, "000001", .exact, &read_buf);
    try std.testing.expectEqualStrings("000001", read_buf[0..6]);
}

test "sequential read" {
    const allocator = std.testing.allocator;

    const tables = [_]TableDef{
        .{
            .name = "items",
            .record_size = 20,
            .keys = &[_]KeyDef{
                .{
                    .name = "by_code",
                    .segments = &[_]KeySegment{
                        .{ .start = 0, .length = 4, .key_type = .alpha },
                    },
                },
            },
        },
    };

    var db = try SqliteIsam.create(allocator, ":memory:", &tables);
    defer db.close();

    // Store records out of order
    var rec1: [20]u8 = undefined;
    @memset(&rec1, ' ');
    @memcpy(rec1[0..4], "CCCC");
    _ = try db.store("items", &rec1);

    var rec2: [20]u8 = undefined;
    @memset(&rec2, ' ');
    @memcpy(rec2[0..4], "AAAA");
    _ = try db.store("items", &rec2);

    var rec3: [20]u8 = undefined;
    @memset(&rec3, ' ');
    @memcpy(rec3[0..4], "BBBB");
    _ = try db.store("items", &rec3);

    // Read first by key (should get AAAA)
    var buf: [20]u8 = undefined;
    try db.read("items", 0, "AAAA", .greater_equal, &buf);
    try std.testing.expectEqualStrings("AAAA", buf[0..4]);

    // Read next (should get BBBB)
    try db.readNext(&buf);
    try std.testing.expectEqualStrings("BBBB", buf[0..4]);

    // Read next (should get CCCC)
    try db.readNext(&buf);
    try std.testing.expectEqualStrings("CCCC", buf[0..4]);

    // Read next (should be EOF)
    try std.testing.expectError(IsamError.EndOfFile, db.readNext(&buf));
}

test "native SQL access" {
    const allocator = std.testing.allocator;

    const tables = [_]TableDef{
        .{
            .name = "products",
            .record_size = 50,
            .keys = &[_]KeyDef{
                .{
                    .name = "by_sku",
                    .segments = &[_]KeySegment{
                        .{ .start = 0, .length = 10, .key_type = .alpha },
                    },
                    .unique = true,
                },
            },
        },
    };

    var db = try SqliteIsam.create(allocator, ":memory:", &tables);
    defer db.close();

    // Store via ISAM
    var rec: [50]u8 = undefined;
    @memset(&rec, ' ');
    @memcpy(rec[0..10], "SKU-001   ");
    _ = try db.store("products", &rec);

    // Query via native SQL
    var result = try db.query("SELECT _key0 FROM products WHERE _key0 LIKE 'SKU%'");
    defer result.deinit();

    try std.testing.expect(result.next());
    const key = result.columnBlob(0);
    try std.testing.expect(key != null);
    try std.testing.expectEqualStrings("SKU-001   ", key.?);
}
