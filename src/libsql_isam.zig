const std = @import("std");
const cotdb = @import("cotdb");
pub const ULID = cotdb.ULID;

// libSQL C API - compatible with SQLite but supports remote connections
// Connection URLs:
//   - Local: "file:path/to/db.db"
//   - Remote: "libsql://your-db.turso.io?authToken=xxx"
const c = @cImport({
    @cInclude("libsql.h");
});

// Destructor constant
const LIBSQL_STATIC: c.libsql_destructor_type = null;

/// Key types matching legacy ISAM semantics
pub const KeyType = enum {
    alpha,
    nocase,
    decimal,
    integer,
    descending,
};

/// A segment within a composite key
pub const KeySegment = struct {
    start: u16,
    length: u16,
    key_type: KeyType,
    descending: bool = false,
};

/// Key definition
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

/// Match modes for key positioning
pub const MatchMode = enum {
    exact,
    greater_equal,
    greater,
    partial,
};

pub const IsamError = error{
    NotOpen,
    DatabaseError,
    TableNotFound,
    RecordNotFound,
    DuplicateKey,
    EndOfFile,
    OutOfMemory,
    InvalidKey,
    ConnectionFailed,
};

/// Buffered record for prefetching
const BufferedRecord = struct {
    rowid: i64,
    ulid: ?ULID,
    data: []u8,
    key_value: []u8,
};

/// LibSqlIsam - ISAM interface over libSQL with read buffering
///
/// Supports both local SQLite files and remote Turso databases.
/// Uses prefetching to make sequential reads fast over network.
pub const LibSqlIsam = struct {
    db: ?*c.libsql_database_t,
    conn: ?*c.libsql_connection_t,
    allocator: std.mem.Allocator,
    path: []const u8,

    // Current position
    current_table: ?[]const u8,
    current_key_index: u8,
    current_rowid: ?i64,
    current_ulid: ?ULID,

    // Read buffer - prefetched records for fast sequential access
    buffer: std.ArrayListAligned(BufferedRecord, null),
    buffer_pos: usize,
    buffer_exhausted: bool,
    buffer_size: usize,

    // Statement cache
    stmt_cache: std.StringHashMap(*c.libsql_stmt_t),

    const Self = @This();

    /// Open database - supports local files and remote Turso URLs
    ///
    /// Examples:
    ///   - Local: "file:./mydb.db"
    ///   - Remote: "libsql://mydb-myorg.turso.io?authToken=eyJ..."
    pub fn open(allocator: std.mem.Allocator, url: []const u8) IsamError!Self {
        var self = Self{
            .db = null,
            .conn = null,
            .allocator = allocator,
            .path = allocator.dupe(u8, url) catch return IsamError.OutOfMemory,
            .current_table = null,
            .current_key_index = 0,
            .current_rowid = null,
            .current_ulid = null,
            .buffer = std.ArrayListAligned(BufferedRecord, null).empty,
            .buffer_pos = 0,
            .buffer_exhausted = false,
            .buffer_size = 100, // Prefetch 100 records at a time
            .stmt_cache = std.StringHashMap(*c.libsql_stmt_t).init(allocator),
        };

        const url_z = allocator.dupeZ(u8, url) catch return IsamError.OutOfMemory;
        defer allocator.free(url_z);

        // Open database
        var db: ?*c.libsql_database_t = null;
        const desc = c.libsql_database_desc{
            .url = url_z.ptr,
            .auth_token = null, // Token is in URL for Turso
            .encryption_key = null,
        };

        if (c.libsql_database_init(&desc, &db) != 0) {
            return IsamError.ConnectionFailed;
        }
        self.db = db;

        // Create connection
        var conn: ?*c.libsql_connection_t = null;
        if (c.libsql_database_connect(db, &conn) != 0) {
            c.libsql_database_deinit(db);
            return IsamError.ConnectionFailed;
        }
        self.conn = conn;

        return self;
    }

    /// Create a new database with tables
    pub fn create(allocator: std.mem.Allocator, url: []const u8, tables: []const TableDef) IsamError!Self {
        var self = try open(allocator, url);

        // Create metadata tables
        try self.execSimple(
            \\CREATE TABLE IF NOT EXISTS _isam_tables (
            \\    name TEXT PRIMARY KEY,
            \\    record_size INTEGER NOT NULL
            \\)
        );

        try self.execSimple(
            \\CREATE TABLE IF NOT EXISTS _isam_keys (
            \\    table_name TEXT NOT NULL,
            \\    key_index INTEGER NOT NULL,
            \\    key_name TEXT NOT NULL,
            \\    is_unique INTEGER NOT NULL,
            \\    is_primary INTEGER NOT NULL,
            \\    PRIMARY KEY (table_name, key_index)
            \\)
        );

        try self.execSimple(
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
        );

        // Create user tables
        for (tables) |table| {
            try self.createTable(table);
        }

        return self;
    }

    /// Close database and free resources
    pub fn close(self: *Self) void {
        self.clearBuffer();
        self.buffer.deinit(self.allocator);

        // Finalize cached statements
        var it = self.stmt_cache.iterator();
        while (it.next()) |entry| {
            c.libsql_stmt_deinit(entry.value_ptr.*);
        }
        self.stmt_cache.deinit();

        if (self.conn) |conn| c.libsql_connection_deinit(conn);
        if (self.db) |db| c.libsql_database_deinit(db);
        if (self.current_table) |t| self.allocator.free(t);
        self.allocator.free(self.path);
    }

    // ========================================================================
    // Read operations with buffering
    // ========================================================================

    /// Read - Position to key and prefetch buffer
    pub fn read(
        self: *Self,
        table: []const u8,
        key_index: u8,
        key_value: []const u8,
        mode: MatchMode,
        record_buf: []u8,
    ) IsamError!void {
        // Update current position
        if (self.current_table) |t| self.allocator.free(t);
        self.current_table = self.allocator.dupe(u8, table) catch return IsamError.OutOfMemory;
        self.current_key_index = key_index;

        // Clear and refill buffer from this key position
        self.clearBuffer();
        try self.fillBuffer(table, key_index, key_value, mode);

        if (self.buffer.items.len == 0) {
            return IsamError.RecordNotFound;
        }

        // Return first buffered record
        const first = self.buffer.items[0];
        const copy_len = @min(first.data.len, record_buf.len);
        @memcpy(record_buf[0..copy_len], first.data[0..copy_len]);

        self.current_rowid = first.rowid;
        self.current_ulid = first.ulid;
        self.buffer_pos = 1;
    }

    /// ReadFirst - Position to first record and prefetch
    pub fn readFirst(self: *Self, table: []const u8, key_index: u8, record_buf: []u8) IsamError!void {
        if (self.current_table) |t| self.allocator.free(t);
        self.current_table = self.allocator.dupe(u8, table) catch return IsamError.OutOfMemory;
        self.current_key_index = key_index;

        self.clearBuffer();
        try self.fillBufferFromStart(table, key_index);

        if (self.buffer.items.len == 0) {
            return IsamError.EndOfFile;
        }

        const first = self.buffer.items[0];
        const copy_len = @min(first.data.len, record_buf.len);
        @memcpy(record_buf[0..copy_len], first.data[0..copy_len]);

        self.current_rowid = first.rowid;
        self.current_ulid = first.ulid;
        self.buffer_pos = 1;
    }

    /// ReadNext - Sequential read from buffer (fast!)
    /// Only hits the database when buffer is exhausted
    pub fn readNext(self: *Self, record_buf: []u8) IsamError!void {
        const table = self.current_table orelse return IsamError.RecordNotFound;

        // Check if we have buffered records
        if (self.buffer_pos < self.buffer.items.len) {
            const rec = self.buffer.items[self.buffer_pos];
            const copy_len = @min(rec.data.len, record_buf.len);
            @memcpy(record_buf[0..copy_len], rec.data[0..copy_len]);

            self.current_rowid = rec.rowid;
            self.current_ulid = rec.ulid;
            self.buffer_pos += 1;
            return;
        }

        // Buffer exhausted
        if (self.buffer_exhausted) {
            return IsamError.EndOfFile;
        }

        // Refill buffer from current position
        const last_rec = if (self.buffer.items.len > 0)
            self.buffer.items[self.buffer.items.len - 1]
        else
            return IsamError.EndOfFile;

        self.clearBuffer();
        try self.fillBufferAfter(table, self.current_key_index, last_rec.key_value, last_rec.rowid);

        if (self.buffer.items.len == 0) {
            self.buffer_exhausted = true;
            return IsamError.EndOfFile;
        }

        // Return first from new buffer
        const first = self.buffer.items[0];
        const copy_len = @min(first.data.len, record_buf.len);
        @memcpy(record_buf[0..copy_len], first.data[0..copy_len]);

        self.current_rowid = first.rowid;
        self.current_ulid = first.ulid;
        self.buffer_pos = 1;
    }

    // ========================================================================
    // Write operations
    // ========================================================================

    /// Store - Insert new record
    pub fn store(self: *Self, table: []const u8, record: []const u8) IsamError!ULID {
        const conn = self.conn orelse return IsamError.NotOpen;

        const key_count = try self.getKeyCount(table);
        const ulid = ULID.new();

        // Build INSERT statement
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..], "INSERT INTO \"{s}\" (_ulid, _record", .{table}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (0..key_count) |i| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d}", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;
        }

        const values_start = std.fmt.bufPrint(sql_buf[sql_len..], ") VALUES (?, ?", .{}) catch return IsamError.DatabaseError;
        sql_len += values_start.len;

        for (0..key_count) |_| {
            const placeholder = std.fmt.bufPrint(sql_buf[sql_len..], ", ?", .{}) catch return IsamError.DatabaseError;
            sql_len += placeholder.len;
        }

        sql_buf[sql_len] = ')';
        sql_len += 1;
        sql_buf[sql_len] = 0;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, &sql_buf, @intCast(sql_len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        // Bind ULID
        if (c.libsql_stmt_bind_blob(stmt, 1, &ulid.bytes, 16, LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }

        // Bind record
        if (c.libsql_stmt_bind_blob(stmt, 2, record.ptr, @intCast(record.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }

        // Bind keys extracted from record
        const segments = try self.getKeySegments(table);
        defer self.allocator.free(segments);

        for (0..key_count) |i| {
            const key_data = try self.extractKey(record, segments[i]);
            defer self.allocator.free(key_data);
            if (c.libsql_stmt_bind_blob(stmt, @intCast(i + 3), key_data.ptr, @intCast(key_data.len), LIBSQL_STATIC) != 0) {
                return IsamError.DatabaseError;
            }
        }

        if (c.libsql_stmt_step(stmt) != c.LIBSQL_DONE) {
            return IsamError.DatabaseError;
        }

        // Invalidate buffer since data changed
        self.clearBuffer();

        return ulid;
    }

    /// Write - Update current record
    pub fn write(self: *Self, record: []const u8) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const rowid = self.current_rowid orelse return IsamError.RecordNotFound;

        const key_count = try self.getKeyCount(table);

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

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, &sql_buf, @intCast(sql_len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        if (c.libsql_stmt_bind_blob(stmt, 1, record.ptr, @intCast(record.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }

        // Bind key values
        const segments = try self.getKeySegments(table);
        defer self.allocator.free(segments);

        for (0..key_count) |i| {
            const key_data = try self.extractKey(record, segments[i]);
            defer self.allocator.free(key_data);
            if (c.libsql_stmt_bind_blob(stmt, @intCast(i + 2), key_data.ptr, @intCast(key_data.len), LIBSQL_STATIC) != 0) {
                return IsamError.DatabaseError;
            }
        }

        if (c.libsql_stmt_bind_int64(stmt, @intCast(key_count + 2), rowid) != 0) {
            return IsamError.DatabaseError;
        }

        if (c.libsql_stmt_step(stmt) != c.LIBSQL_DONE) {
            return IsamError.DatabaseError;
        }

        // Invalidate buffer
        self.clearBuffer();
    }

    /// Delete current record
    pub fn delete(self: *Self) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const rowid = self.current_rowid orelse return IsamError.RecordNotFound;

        var sql_buf: [256]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf, "DELETE FROM \"{s}\" WHERE rowid = ?", .{table}) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        if (c.libsql_stmt_bind_int64(stmt, 1, rowid) != 0) {
            return IsamError.DatabaseError;
        }

        if (c.libsql_stmt_step(stmt) != c.LIBSQL_DONE) {
            return IsamError.DatabaseError;
        }

        self.current_rowid = null;
        self.clearBuffer();
    }

    // ========================================================================
    // Buffer management - the key to fast sequential reads
    // ========================================================================

    fn clearBuffer(self: *Self) void {
        for (self.buffer.items) |item| {
            self.allocator.free(item.data);
            self.allocator.free(item.key_value);
        }
        self.buffer.clearRetainingCapacity();
        self.buffer_pos = 0;
        self.buffer_exhausted = false;
    }

    fn fillBuffer(
        self: *Self,
        table: []const u8,
        key_index: u8,
        key_value: []const u8,
        mode: MatchMode,
    ) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;

        const op = switch (mode) {
            .exact => "=",
            .greater_equal => ">=",
            .greater => ">",
            .partial => "LIKE",
        };

        var sql_buf: [512]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT rowid, _ulid, _record, _key{d} FROM "{s}"
            \\WHERE _key{d} {s} ?
            \\ORDER BY _key{d}, rowid
            \\LIMIT {d}
        , .{ key_index, table, key_index, op, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        // Bind key value (with wildcard for partial match)
        if (mode == .partial) {
            var key_pattern = self.allocator.alloc(u8, key_value.len + 1) catch return IsamError.OutOfMemory;
            defer self.allocator.free(key_pattern);
            @memcpy(key_pattern[0..key_value.len], key_value);
            key_pattern[key_value.len] = '%';
            if (c.libsql_stmt_bind_blob(stmt, 1, key_pattern.ptr, @intCast(key_pattern.len), LIBSQL_STATIC) != 0) {
                return IsamError.DatabaseError;
            }
        } else {
            if (c.libsql_stmt_bind_blob(stmt, 1, key_value.ptr, @intCast(key_value.len), LIBSQL_STATIC) != 0) {
                return IsamError.DatabaseError;
            }
        }

        try self.fetchIntoBuffer(stmt);
    }

    fn fillBufferFromStart(self: *Self, table: []const u8, key_index: u8) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [512]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT rowid, _ulid, _record, _key{d} FROM "{s}"
            \\ORDER BY _key{d}, rowid
            \\LIMIT {d}
        , .{ key_index, table, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        try self.fetchIntoBuffer(stmt);
    }

    fn fillBufferAfter(
        self: *Self,
        table: []const u8,
        key_index: u8,
        last_key: []const u8,
        last_rowid: i64,
    ) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [512]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT rowid, _ulid, _record, _key{d} FROM "{s}"
            \\WHERE _key{d} > ? OR (_key{d} = ? AND rowid > ?)
            \\ORDER BY _key{d}, rowid
            \\LIMIT {d}
        , .{ key_index, table, key_index, key_index, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        if (c.libsql_stmt_bind_blob(stmt, 1, last_key.ptr, @intCast(last_key.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }
        if (c.libsql_stmt_bind_blob(stmt, 2, last_key.ptr, @intCast(last_key.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }
        if (c.libsql_stmt_bind_int64(stmt, 3, last_rowid) != 0) {
            return IsamError.DatabaseError;
        }

        try self.fetchIntoBuffer(stmt);
    }

    fn fetchIntoBuffer(self: *Self, stmt: ?*c.libsql_stmt_t) IsamError!void {
        var count: usize = 0;

        while (c.libsql_stmt_step(stmt) == c.LIBSQL_ROW) {
            const rowid = c.libsql_stmt_column_int64(stmt, 0);

            const ulid_ptr: ?[*]const u8 = @ptrCast(c.libsql_stmt_column_blob(stmt, 1));
            const ulid: ?ULID = if (ulid_ptr) |ptr| ULID{ .bytes = ptr[0..16].* } else null;

            const record_ptr: ?[*]const u8 = @ptrCast(c.libsql_stmt_column_blob(stmt, 2));
            const record_len: usize = @intCast(c.libsql_stmt_column_bytes(stmt, 2));

            const key_ptr: ?[*]const u8 = @ptrCast(c.libsql_stmt_column_blob(stmt, 3));
            const key_len: usize = @intCast(c.libsql_stmt_column_bytes(stmt, 3));

            if (record_ptr == null) continue;

            const data = self.allocator.alloc(u8, record_len) catch continue;
            @memcpy(data, record_ptr.?[0..record_len]);

            const key_value = self.allocator.alloc(u8, key_len) catch {
                self.allocator.free(data);
                continue;
            };
            if (key_ptr) |kp| {
                @memcpy(key_value, kp[0..key_len]);
            }

            self.buffer.append(self.allocator, .{
                .rowid = rowid,
                .ulid = ulid,
                .data = data,
                .key_value = key_value,
            }) catch {
                self.allocator.free(data);
                self.allocator.free(key_value);
                continue;
            };

            count += 1;
        }

        // If we got fewer than buffer_size, we're at the end
        if (count < self.buffer_size) {
            self.buffer_exhausted = true;
        }
    }

    // ========================================================================
    // Helper functions
    // ========================================================================

    fn execSimple(self: *Self, sql: [*:0]const u8) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;
        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql, -1, &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);
        _ = c.libsql_stmt_step(stmt);
    }

    fn createTable(self: *Self, table: TableDef) IsamError!void {
        // Build CREATE TABLE statement
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..],
            \\CREATE TABLE IF NOT EXISTS "{s}" (
            \\    _ulid BLOB NOT NULL,
            \\    _record BLOB NOT NULL
        , .{table.name}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (table.keys, 0..) |_, i| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ",\n    _key{d} BLOB NOT NULL", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;
        }

        const suffix = std.fmt.bufPrint(sql_buf[sql_len..], "\n)", .{}) catch return IsamError.DatabaseError;
        sql_len += suffix.len;
        sql_buf[sql_len] = 0;

        try self.execSimple(@ptrCast(&sql_buf));

        // Create indexes
        for (table.keys, 0..) |_, i| {
            var idx_sql: [256]u8 = undefined;
            const idx = std.fmt.bufPrintZ(&idx_sql, "CREATE INDEX IF NOT EXISTS \"idx_{s}_key{d}\" ON \"{s}\" (_key{d})", .{ table.name, i, table.name, i }) catch return IsamError.DatabaseError;
            try self.execSimple(idx.ptr);
        }

        // Register in metadata
        var meta_sql: [256]u8 = undefined;
        const meta = std.fmt.bufPrintZ(&meta_sql, "INSERT OR REPLACE INTO _isam_tables (name, record_size) VALUES ('{s}', {d})", .{ table.name, table.record_size }) catch return IsamError.DatabaseError;
        try self.execSimple(meta.ptr);
    }

    fn getKeyCount(self: *Self, table: []const u8) IsamError!usize {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [128]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf, "SELECT COUNT(*) FROM _isam_keys WHERE table_name = ?", .{}) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        if (c.libsql_stmt_bind_text(stmt, 1, table_z.ptr, @intCast(table.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }

        if (c.libsql_stmt_step(stmt) != c.LIBSQL_ROW) {
            return 0;
        }

        return @intCast(c.libsql_stmt_column_int64(stmt, 0));
    }

    fn getKeySegments(self: *Self, table: []const u8) IsamError![]const []const KeySegment {
        _ = self;
        _ = table;
        // TODO: Load from _isam_segments table
        // For now return empty - this is a stub
        return &[_][]const KeySegment{};
    }

    fn extractKey(self: *Self, record: []const u8, segments: []const KeySegment) IsamError![]u8 {
        var total_len: usize = 0;
        for (segments) |seg| {
            total_len += seg.length;
        }

        var key = self.allocator.alloc(u8, total_len) catch return IsamError.OutOfMemory;
        var offset: usize = 0;

        for (segments) |seg| {
            const start = seg.start;
            const end = @min(start + seg.length, record.len);
            if (start < record.len) {
                @memcpy(key[offset .. offset + (end - start)], record[start..end]);
            }
            offset += seg.length;
        }

        return key;
    }

    // ========================================================================
    // Table metadata
    // ========================================================================

    pub const TableMeta = struct {
        record_size: u32,
        keys_json: []u8, // Placeholder for compatibility
    };

    pub fn getTableMeta(self: *Self, table: []const u8) IsamError!TableMeta {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [128]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf, "SELECT record_size FROM _isam_tables WHERE name = ?", .{}) catch return IsamError.DatabaseError;

        var stmt: ?*c.libsql_stmt_t = null;
        if (c.libsql_connection_prepare(conn, sql.ptr, @intCast(sql.len), &stmt) != 0) {
            return IsamError.DatabaseError;
        }
        defer c.libsql_stmt_deinit(stmt);

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        if (c.libsql_stmt_bind_text(stmt, 1, table_z.ptr, @intCast(table.len), LIBSQL_STATIC) != 0) {
            return IsamError.DatabaseError;
        }

        if (c.libsql_stmt_step(stmt) != c.LIBSQL_ROW) {
            return IsamError.TableNotFound;
        }

        return TableMeta{
            .record_size = @intCast(c.libsql_stmt_column_int(stmt, 0)),
            .keys_json = self.allocator.alloc(u8, 0) catch return IsamError.OutOfMemory,
        };
    }
};
