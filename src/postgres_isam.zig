const std = @import("std");
const cotdb = @import("cotdb");
pub const ULID = cotdb.ULID;

// PostgreSQL C client library (libpq)
// Works with any Postgres: Neon, Supabase, local, etc.
const c = @cImport({
    @cInclude("libpq-fe.h");
});

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
    id: i64, // Primary key / row id
    ulid: ?ULID,
    data: []u8,
    key_value: []u8,
};

/// PostgresIsam - ISAM interface over Postgres with read buffering
///
/// Uses libpq for direct Postgres wire protocol connections.
/// Works with Neon, Supabase Postgres, local Postgres, etc.
///
/// Connection string examples:
///   - Neon: "postgres://user:pass@ep-xxx.us-east-2.aws.neon.tech/dbname?sslmode=require"
///   - Local: "postgres://localhost/mydb"
///   - Supabase: "postgres://postgres:pass@db.xxx.supabase.co:5432/postgres"
pub const PostgresIsam = struct {
    conn: ?*c.PGconn,
    allocator: std.mem.Allocator,
    conninfo: []const u8,

    // Current position
    current_table: ?[]const u8,
    current_key_index: u8,
    current_id: ?i64,
    current_ulid: ?ULID,

    // Read buffer - prefetched records for fast sequential access
    buffer: std.ArrayListAligned(BufferedRecord, null),
    buffer_pos: usize,
    buffer_exhausted: bool,
    buffer_size: usize,

    const Self = @This();

    /// Open connection to Postgres database
    pub fn open(allocator: std.mem.Allocator, conninfo: []const u8) IsamError!Self {
        var self = Self{
            .conn = null,
            .allocator = allocator,
            .conninfo = allocator.dupe(u8, conninfo) catch return IsamError.OutOfMemory,
            .current_table = null,
            .current_key_index = 0,
            .current_id = null,
            .current_ulid = null,
            .buffer = std.ArrayListAligned(BufferedRecord, null).empty,
            .buffer_pos = 0,
            .buffer_exhausted = false,
            .buffer_size = 100, // Prefetch 100 records at a time
        };

        const conninfo_z = allocator.dupeZ(u8, conninfo) catch return IsamError.OutOfMemory;
        defer allocator.free(conninfo_z);

        self.conn = c.PQconnectdb(conninfo_z.ptr);

        if (c.PQstatus(self.conn) != c.CONNECTION_OK) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.debug.print("[PostgresIsam] Connection failed: {s}\n", .{err_msg});
            c.PQfinish(self.conn);
            self.conn = null;
            return IsamError.ConnectionFailed;
        }

        return self;
    }

    /// Create database with tables (open + create schema)
    pub fn create(allocator: std.mem.Allocator, conninfo: []const u8, tables: []const TableDef) IsamError!Self {
        var self = try open(allocator, conninfo);

        // Create metadata tables
        try self.exec(
            \\CREATE TABLE IF NOT EXISTS _isam_tables (
            \\    name TEXT PRIMARY KEY,
            \\    record_size INTEGER NOT NULL
            \\)
        );

        try self.exec(
            \\CREATE TABLE IF NOT EXISTS _isam_keys (
            \\    table_name TEXT NOT NULL,
            \\    key_index INTEGER NOT NULL,
            \\    key_name TEXT NOT NULL,
            \\    is_unique BOOLEAN NOT NULL,
            \\    is_primary BOOLEAN NOT NULL,
            \\    PRIMARY KEY (table_name, key_index)
            \\)
        );

        try self.exec(
            \\CREATE TABLE IF NOT EXISTS _isam_segments (
            \\    table_name TEXT NOT NULL,
            \\    key_index INTEGER NOT NULL,
            \\    segment_index INTEGER NOT NULL,
            \\    start_pos INTEGER NOT NULL,
            \\    length INTEGER NOT NULL,
            \\    key_type TEXT NOT NULL,
            \\    descending BOOLEAN NOT NULL,
            \\    PRIMARY KEY (table_name, key_index, segment_index)
            \\)
        );

        // Create user tables
        for (tables) |table| {
            try self.createTable(table);
        }

        return self;
    }

    /// Close connection and free resources
    pub fn close(self: *Self) void {
        self.clearBuffer();
        self.buffer.deinit(self.allocator);

        if (self.conn) |conn| {
            c.PQfinish(conn);
        }

        if (self.current_table) |t| self.allocator.free(t);
        self.allocator.free(self.conninfo);
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

        self.current_id = first.id;
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

        self.current_id = first.id;
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

            self.current_id = rec.id;
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
        try self.fillBufferAfter(table, self.current_key_index, last_rec.key_value, last_rec.id);

        if (self.buffer.items.len == 0) {
            self.buffer_exhausted = true;
            return IsamError.EndOfFile;
        }

        // Return first from new buffer
        const first = self.buffer.items[0];
        const copy_len = @min(first.data.len, record_buf.len);
        @memcpy(record_buf[0..copy_len], first.data[0..copy_len]);

        self.current_id = first.id;
        self.current_ulid = first.ulid;
        self.buffer_pos = 1;
    }

    // ========================================================================
    // Write operations
    // ========================================================================

    /// Store - Insert new record
    pub fn store(self: *Self, table: []const u8, record: []const u8) IsamError!ULID {
        const conn = self.conn orelse return IsamError.NotOpen;

        const ulid = ULID.new();
        const key_count = try self.getKeyCount(table);

        // Build INSERT with parameters
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..],
            \\INSERT INTO "{s}" (_ulid, _record
        , .{table}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (0..key_count) |i| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d}", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;
        }

        const values_start = std.fmt.bufPrint(sql_buf[sql_len..], ") VALUES ($1, $2", .{}) catch return IsamError.DatabaseError;
        sql_len += values_start.len;

        for (0..key_count) |i| {
            const placeholder = std.fmt.bufPrint(sql_buf[sql_len..], ", ${d}", .{i + 3}) catch return IsamError.DatabaseError;
            sql_len += placeholder.len;
        }

        const suffix = std.fmt.bufPrint(sql_buf[sql_len..], ") RETURNING id", .{}) catch return IsamError.DatabaseError;
        sql_len += suffix.len;
        sql_buf[sql_len] = 0;

        // Prepare parameters
        const param_count = 2 + key_count;
        var param_values: [16]?[*]const u8 = undefined;
        var param_lengths: [16]c_int = undefined;
        var param_formats: [16]c_int = undefined;

        // ULID (binary)
        param_values[0] = &ulid.bytes;
        param_lengths[0] = 16;
        param_formats[0] = 1; // binary

        // Record (binary)
        param_values[1] = record.ptr;
        param_lengths[1] = @intCast(record.len);
        param_formats[1] = 1; // binary

        // Key values (extract from record based on segments)
        const segments = try self.getKeySegments(table);
        defer {
            for (segments) |seg_slice| {
                self.allocator.free(seg_slice);
            }
            self.allocator.free(segments);
        }

        var key_buffers: [14][]u8 = undefined;
        var key_buf_count: usize = 0;

        for (0..key_count) |i| {
            const key_data = try self.extractKey(record, if (i < segments.len) segments[i] else &[_]KeySegment{});
            key_buffers[key_buf_count] = key_data;
            key_buf_count += 1;

            param_values[i + 2] = key_data.ptr;
            param_lengths[i + 2] = @intCast(key_data.len);
            param_formats[i + 2] = 1; // binary
        }
        defer {
            for (0..key_buf_count) |i| {
                self.allocator.free(key_buffers[i]);
            }
        }

        const result = c.PQexecParams(
            conn,
            &sql_buf,
            @intCast(param_count),
            null, // param types (let server infer)
            &param_values,
            &param_lengths,
            &param_formats,
            0, // text result
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            const err = c.PQerrorMessage(conn);
            std.debug.print("[PostgresIsam] store failed: {s}\n", .{err});
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
        const id = self.current_id orelse return IsamError.RecordNotFound;

        const key_count = try self.getKeyCount(table);

        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        const prefix = std.fmt.bufPrint(sql_buf[sql_len..],
            \\UPDATE "{s}" SET _record = $1
        , .{table}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (0..key_count) |i| {
            const key_set = std.fmt.bufPrint(sql_buf[sql_len..], ", _key{d} = ${d}", .{ i, i + 2 }) catch return IsamError.DatabaseError;
            sql_len += key_set.len;
        }

        const where = std.fmt.bufPrint(sql_buf[sql_len..], " WHERE id = ${d}", .{key_count + 2}) catch return IsamError.DatabaseError;
        sql_len += where.len;
        sql_buf[sql_len] = 0;

        // Prepare parameters
        const param_count = 2 + key_count;
        var param_values: [16]?[*]const u8 = undefined;
        var param_lengths: [16]c_int = undefined;
        var param_formats: [16]c_int = undefined;

        // Record
        param_values[0] = record.ptr;
        param_lengths[0] = @intCast(record.len);
        param_formats[0] = 1;

        // Keys
        const segments = try self.getKeySegments(table);
        defer {
            for (segments) |seg_slice| {
                self.allocator.free(seg_slice);
            }
            self.allocator.free(segments);
        }

        var key_buffers: [14][]u8 = undefined;
        var key_buf_count: usize = 0;

        for (0..key_count) |i| {
            const key_data = try self.extractKey(record, if (i < segments.len) segments[i] else &[_]KeySegment{});
            key_buffers[key_buf_count] = key_data;
            key_buf_count += 1;

            param_values[i + 1] = key_data.ptr;
            param_lengths[i + 1] = @intCast(key_data.len);
            param_formats[i + 1] = 1;
        }
        defer {
            for (0..key_buf_count) |i| {
                self.allocator.free(key_buffers[i]);
            }
        }

        // ID parameter
        var id_buf: [8]u8 = undefined;
        std.mem.writeInt(i64, &id_buf, id, .big);
        param_values[key_count + 1] = &id_buf;
        param_lengths[key_count + 1] = 8;
        param_formats[key_count + 1] = 1;

        const result = c.PQexecParams(
            conn,
            &sql_buf,
            @intCast(param_count),
            null,
            &param_values,
            &param_lengths,
            &param_formats,
            0,
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_COMMAND_OK) {
            return IsamError.DatabaseError;
        }

        self.clearBuffer();
    }

    /// Delete current record
    pub fn delete(self: *Self) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;
        const table = self.current_table orelse return IsamError.RecordNotFound;
        const id = self.current_id orelse return IsamError.RecordNotFound;

        var sql_buf: [256]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\DELETE FROM "{s}" WHERE id = $1
        , .{table}) catch return IsamError.DatabaseError;

        var id_buf: [8]u8 = undefined;
        std.mem.writeInt(i64, &id_buf, id, .big);

        const param_values = [_]?[*]const u8{&id_buf};
        const param_lengths = [_]c_int{8};
        const param_formats = [_]c_int{1};

        const result = c.PQexecParams(
            conn,
            sql.ptr,
            1,
            null,
            &param_values,
            &param_lengths,
            &param_formats,
            0,
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_COMMAND_OK) {
            return IsamError.DatabaseError;
        }

        self.current_id = null;
        self.clearBuffer();
    }

    // ========================================================================
    // Buffer management
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
            \\SELECT id, _ulid, _record, _key{d} FROM "{s}"
            \\WHERE _key{d} {s} $1
            \\ORDER BY _key{d}, id
            \\LIMIT {d}
        , .{ key_index, table, key_index, op, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        // Handle partial match (append %)
        var key_param: []u8 = undefined;
        if (mode == .partial) {
            key_param = self.allocator.alloc(u8, key_value.len + 1) catch return IsamError.OutOfMemory;
            @memcpy(key_param[0..key_value.len], key_value);
            key_param[key_value.len] = '%';
        } else {
            key_param = self.allocator.dupe(u8, key_value) catch return IsamError.OutOfMemory;
        }
        defer self.allocator.free(key_param);

        const param_values = [_]?[*]const u8{key_param.ptr};
        const param_lengths = [_]c_int{@intCast(key_param.len)};
        const param_formats = [_]c_int{1}; // binary

        const result = c.PQexecParams(
            conn,
            sql.ptr,
            1,
            null,
            &param_values,
            &param_lengths,
            &param_formats,
            1, // binary result
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            return IsamError.DatabaseError;
        }

        try self.fetchResultsIntoBuffer(result.?);
    }

    fn fillBufferFromStart(self: *Self, table: []const u8, key_index: u8) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [512]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT id, _ulid, _record, _key{d} FROM "{s}"
            \\ORDER BY _key{d}, id
            \\LIMIT {d}
        , .{ key_index, table, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        const result = c.PQexec(conn, sql.ptr);
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            return IsamError.DatabaseError;
        }

        try self.fetchResultsIntoBuffer(result.?);
    }

    fn fillBufferAfter(
        self: *Self,
        table: []const u8,
        key_index: u8,
        last_key: []const u8,
        last_id: i64,
    ) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;

        var sql_buf: [512]u8 = undefined;
        const sql = std.fmt.bufPrintZ(&sql_buf,
            \\SELECT id, _ulid, _record, _key{d} FROM "{s}"
            \\WHERE _key{d} > $1 OR (_key{d} = $2 AND id > $3)
            \\ORDER BY _key{d}, id
            \\LIMIT {d}
        , .{ key_index, table, key_index, key_index, key_index, self.buffer_size }) catch return IsamError.DatabaseError;

        var id_buf: [8]u8 = undefined;
        std.mem.writeInt(i64, &id_buf, last_id, .big);

        const param_values = [_]?[*]const u8{ last_key.ptr, last_key.ptr, &id_buf };
        const param_lengths = [_]c_int{ @intCast(last_key.len), @intCast(last_key.len), 8 };
        const param_formats = [_]c_int{ 1, 1, 1 };

        const result = c.PQexecParams(
            conn,
            sql.ptr,
            3,
            null,
            &param_values,
            &param_lengths,
            &param_formats,
            1,
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            return IsamError.DatabaseError;
        }

        try self.fetchResultsIntoBuffer(result.?);
    }

    fn fetchResultsIntoBuffer(self: *Self, result: *c.PGresult) IsamError!void {
        const nrows = c.PQntuples(result);

        for (0..@intCast(nrows)) |row_idx| {
            const row: c_int = @intCast(row_idx);

            // Column 0: id (int8)
            const id_ptr: ?[*]const u8 = @ptrCast(c.PQgetvalue(result, row, 0));
            const id: i64 = if (id_ptr) |ptr|
                std.mem.readInt(i64, ptr[0..8], .big)
            else
                0;

            // Column 1: _ulid (bytea, 16 bytes)
            const ulid_ptr: ?[*]const u8 = @ptrCast(c.PQgetvalue(result, row, 1));
            const ulid_len = c.PQgetlength(result, row, 1);
            const ulid: ?ULID = if (ulid_ptr != null and ulid_len >= 16)
                ULID{ .bytes = ulid_ptr.?[0..16].* }
            else
                null;

            // Column 2: _record (bytea)
            const record_ptr: ?[*]const u8 = @ptrCast(c.PQgetvalue(result, row, 2));
            const record_len: usize = @intCast(c.PQgetlength(result, row, 2));

            // Column 3: _keyN (bytea)
            const key_ptr: ?[*]const u8 = @ptrCast(c.PQgetvalue(result, row, 3));
            const key_len: usize = @intCast(c.PQgetlength(result, row, 3));

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
                .id = id,
                .ulid = ulid,
                .data = data,
                .key_value = key_value,
            }) catch {
                self.allocator.free(data);
                self.allocator.free(key_value);
                continue;
            };
        }

        // If we got fewer than buffer_size, we're at the end
        if (self.buffer.items.len < self.buffer_size) {
            self.buffer_exhausted = true;
        }
    }

    // ========================================================================
    // Helper functions
    // ========================================================================

    fn exec(self: *Self, sql: [*:0]const u8) IsamError!void {
        const conn = self.conn orelse return IsamError.NotOpen;
        const result = c.PQexec(conn, sql);
        defer c.PQclear(result);

        const status = c.PQresultStatus(result);
        if (status != c.PGRES_COMMAND_OK and status != c.PGRES_TUPLES_OK) {
            const err = c.PQerrorMessage(conn);
            std.debug.print("[PostgresIsam] exec failed: {s}\n", .{err});
            return IsamError.DatabaseError;
        }
    }

    fn createTable(self: *Self, table: TableDef) IsamError!void {
        var sql_buf: [4096]u8 = undefined;
        var sql_len: usize = 0;

        // Postgres uses SERIAL/BIGSERIAL for auto-increment
        const prefix = std.fmt.bufPrint(sql_buf[sql_len..],
            \\CREATE TABLE IF NOT EXISTS "{s}" (
            \\    id BIGSERIAL PRIMARY KEY,
            \\    _ulid BYTEA NOT NULL,
            \\    _record BYTEA NOT NULL
        , .{table.name}) catch return IsamError.DatabaseError;
        sql_len += prefix.len;

        for (table.keys, 0..) |_, i| {
            const key_col = std.fmt.bufPrint(sql_buf[sql_len..], ",\n    _key{d} BYTEA NOT NULL", .{i}) catch return IsamError.DatabaseError;
            sql_len += key_col.len;
        }

        const suffix = std.fmt.bufPrint(sql_buf[sql_len..], "\n)", .{}) catch return IsamError.DatabaseError;
        sql_len += suffix.len;
        sql_buf[sql_len] = 0;

        try self.exec(@ptrCast(&sql_buf));

        // Create indexes for each key
        for (table.keys, 0..) |_, i| {
            var idx_sql: [256]u8 = undefined;
            const idx = std.fmt.bufPrintZ(&idx_sql,
                \\CREATE INDEX IF NOT EXISTS "idx_{s}_key{d}" ON "{s}" (_key{d})
            , .{ table.name, i, table.name, i }) catch return IsamError.DatabaseError;
            try self.exec(idx.ptr);
        }

        // Register in metadata
        var meta_sql: [256]u8 = undefined;
        const meta = std.fmt.bufPrintZ(&meta_sql,
            \\INSERT INTO _isam_tables (name, record_size)
            \\VALUES ('{s}', {d})
            \\ON CONFLICT (name) DO UPDATE SET record_size = EXCLUDED.record_size
        , .{ table.name, table.record_size }) catch return IsamError.DatabaseError;
        try self.exec(meta.ptr);
    }

    fn getKeyCount(self: *Self, table: []const u8) IsamError!usize {
        const conn = self.conn orelse return IsamError.NotOpen;

        const table_z = self.allocator.dupeZ(u8, table) catch return IsamError.OutOfMemory;
        defer self.allocator.free(table_z);

        const param_values = [_]?[*]const u8{table_z.ptr};
        const param_lengths = [_]c_int{@intCast(table.len)};
        const param_formats = [_]c_int{0}; // text

        const result = c.PQexecParams(
            conn,
            "SELECT COUNT(*) FROM _isam_keys WHERE table_name = $1",
            1,
            null,
            &param_values,
            &param_lengths,
            &param_formats,
            0,
        );
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            return 0;
        }

        if (c.PQntuples(result) == 0) {
            return 0;
        }

        const val_ptr = c.PQgetvalue(result, 0, 0);
        if (val_ptr == null) return 0;

        const val_str: [*:0]const u8 = @ptrCast(val_ptr);
        return std.fmt.parseInt(usize, std.mem.span(val_str), 10) catch 0;
    }

    fn getKeySegments(self: *Self, table: []const u8) IsamError![]const []const KeySegment {
        _ = self;
        _ = table;
        // TODO: Load from _isam_segments table
        return &[_][]const KeySegment{};
    }

    fn extractKey(self: *Self, record: []const u8, segments: []const KeySegment) IsamError![]u8 {
        var total_len: usize = 0;
        for (segments) |seg| {
            total_len += seg.length;
        }
        if (total_len == 0) {
            // Default: use first 6 bytes as key
            total_len = @min(6, record.len);
            const key = self.allocator.alloc(u8, total_len) catch return IsamError.OutOfMemory;
            @memcpy(key, record[0..total_len]);
            return key;
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
        const sql = std.fmt.bufPrintZ(&sql_buf, "SELECT record_size FROM _isam_tables WHERE name = '{s}'", .{table}) catch return IsamError.DatabaseError;

        const result = c.PQexec(conn, sql.ptr);
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_TUPLES_OK) {
            return IsamError.DatabaseError;
        }

        if (c.PQntuples(result) == 0) {
            return IsamError.TableNotFound;
        }

        const val_ptr = c.PQgetvalue(result, 0, 0);
        if (val_ptr == null) return IsamError.TableNotFound;

        const val_str: [*:0]const u8 = @ptrCast(val_ptr);
        const record_size = std.fmt.parseInt(u32, std.mem.span(val_str), 10) catch return IsamError.DatabaseError;

        return TableMeta{
            .record_size = record_size,
            .keys_json = self.allocator.alloc(u8, 0) catch return IsamError.OutOfMemory,
        };
    }
};
