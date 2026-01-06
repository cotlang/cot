//! std.sql - Unified SQL Database Interface
//!
//! Provides a common interface for SQL databases with backend auto-detection.
//!
//! Connection:
//!   sql_open(url)              - Open connection (auto-detect backend from URL)
//!   sql_close(handle)          - Close connection
//!
//! Execution:
//!   sql_exec(handle, sql)      - Execute statement, return rows affected
//!   sql_query(handle, sql)     - Execute query, return result set handle
//!   sql_fetch(result)          - Fetch next row from result set
//!   sql_fetch_all(result)      - Fetch all rows as JSON array
//!
//! Result Inspection:
//!   sql_column_count(result)   - Number of columns
//!   sql_column_name(result, i) - Name of column i
//!   sql_row_get(row, col)      - Get column value from row
//!
//! Transactions:
//!   sql_begin(handle)          - Begin transaction
//!   sql_commit(handle)         - Commit transaction
//!   sql_rollback(handle)       - Rollback transaction
//!
//! URL Schemes:
//!   sqlite:path/to/db.sqlite   - SQLite file database
//!   sqlite::memory:            - SQLite in-memory database
//!   postgres://user:pass@host/db - PostgreSQL connection
//!
//! Parameters are passed as additional arguments after the SQL string.
//! Use ? placeholders in SQL: sql_exec(h, "INSERT INTO t VALUES (?, ?)", val1, val2)
//!
//! PostgreSQL Backend:
//!   PostgreSQL support is disabled by default. To enable:
//!   1. Install libpq: `brew install libpq` (macOS) or `apt install libpq-dev` (Linux)
//!   2. Build with: `zig build -Dpostgres=true`
//!
//!   Note: PostgreSQL uses $1, $2, etc. for parameter placeholders instead of ?

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const build_options = @import("build_options");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

const c = @cImport({
    @cInclude("sqlite3.h");
});

// PostgreSQL support - controlled by build option: zig build -Dpostgres=true
const postgres_enabled = build_options.enable_postgres;
const pq = if (postgres_enabled) @cImport({
    @cInclude("libpq-fe.h");
}) else struct {
    // Stub types for when PostgreSQL is disabled
    pub const PGconn = opaque {};
    pub const PGresult = opaque {};
    pub const CONNECTION_OK = 0;
    pub const PGRES_COMMAND_OK = 1;
    pub const PGRES_TUPLES_OK = 2;
    pub fn PQconnectdb(_: [*:0]const u8) ?*PGconn {
        return null;
    }
    pub fn PQfinish(_: *PGconn) void {}
    pub fn PQstatus(_: *PGconn) c_int {
        return 0;
    }
    pub fn PQerrorMessage(_: *PGconn) [*:0]const u8 {
        return "PostgreSQL not enabled";
    }
    pub fn PQclear(_: *PGresult) void {}
    pub fn PQexec(_: *PGconn, _: [*:0]const u8) ?*PGresult {
        return null;
    }
    pub fn PQexecParams(_: *PGconn, _: [*:0]const u8, _: c_int, _: ?*anyopaque, _: ?[*][*c]const u8, _: ?*anyopaque, _: ?*anyopaque, _: c_int) ?*PGresult {
        return null;
    }
    pub fn PQresultStatus(_: *PGresult) c_int {
        return 0;
    }
    pub fn PQcmdTuples(_: *PGresult) ?[*:0]const u8 {
        return null;
    }
    pub fn PQnfields(_: *PGresult) c_int {
        return 0;
    }
    pub fn PQntuples(_: *PGresult) c_int {
        return 0;
    }
    pub fn PQfname(_: *PGresult, _: c_int) ?[*:0]const u8 {
        return null;
    }
    pub fn PQgetisnull(_: *PGresult, _: c_int, _: c_int) c_int {
        return 1;
    }
    pub fn PQgetvalue(_: *PGresult, _: c_int, _: c_int) ?[*]const u8 {
        return null;
    }
    pub fn PQgetlength(_: *PGresult, _: c_int, _: c_int) c_int {
        return 0;
    }
};

// SQLite destructor constant - null means data is static/valid during statement execution
const SQLITE_STATIC: c.sqlite3_destructor_type = null;

// =============================================================================
// Connection Handle Management
// =============================================================================

/// SQL Connection types
const BackendType = enum {
    sqlite,
    postgres,
    // libsql, // Future
};

/// SQLite connection wrapper
const SqliteConn = struct {
    db: *c.sqlite3,
    in_transaction: bool = false,
};

/// PostgreSQL connection wrapper
const PostgresConn = struct {
    conn: *pq.PGconn,
    in_transaction: bool = false,
};

/// Result set for queries
const ResultSet = struct {
    backend: BackendType,
    // SQLite result
    sqlite_stmt: ?*c.sqlite3_stmt = null,
    // PostgreSQL result
    pg_result: ?*pq.PGresult = null,
    pg_row_idx: c_int = 0,
    pg_num_rows: c_int = 0,
    // Common fields
    column_count: usize = 0,
    column_names: [][]const u8 = &.{},
    exhausted: bool = false,
    allocator: std.mem.Allocator,

    fn deinit(self: *ResultSet) void {
        if (self.sqlite_stmt) |stmt| {
            _ = c.sqlite3_finalize(stmt);
        }
        if (self.pg_result) |res| {
            pq.PQclear(res);
        }
        for (self.column_names) |name| {
            self.allocator.free(name);
        }
        if (self.column_names.len > 0) {
            self.allocator.free(self.column_names);
        }
    }
};

/// SQL Handle - either a connection or result set
const SqlHandle = union(enum) {
    connection: struct {
        backend: BackendType,
        sqlite: ?SqliteConn,
        postgres: ?PostgresConn,
    },
    result_set: ResultSet,
};

/// Global handle storage
var handles: std.AutoHashMap(u32, SqlHandle) = undefined;
var next_handle_id: u32 = 1;
var handles_initialized: bool = false;
var handles_allocator: std.mem.Allocator = undefined;

fn initHandles(allocator: std.mem.Allocator) void {
    if (!handles_initialized) {
        handles = std.AutoHashMap(u32, SqlHandle).init(allocator);
        handles_allocator = allocator;
        handles_initialized = true;
    }
}

fn allocHandle(handle: SqlHandle) !u32 {
    const id = next_handle_id;
    next_handle_id += 1;
    try handles.put(id, handle);
    return id;
}

fn getHandle(id: u32) ?*SqlHandle {
    return handles.getPtr(id);
}

fn freeHandle(id: u32) void {
    if (handles.fetchRemove(id)) |kv| {
        var h = kv.value;
        switch (h) {
            .connection => |conn| {
                if (conn.sqlite) |sq| {
                    _ = c.sqlite3_close(sq.db);
                }
                if (conn.postgres) |pg| {
                    pq.PQfinish(pg.conn);
                }
            },
            .result_set => |*rs| {
                rs.deinit();
            },
        }
    }
}

// =============================================================================
// URL Parsing
// =============================================================================

fn parseUrl(url: []const u8) struct { backend: BackendType, path: []const u8 } {
    if (std.mem.startsWith(u8, url, "sqlite:")) {
        return .{ .backend = .sqlite, .path = url[7..] };
    } else if (std.mem.startsWith(u8, url, "postgres://") or std.mem.startsWith(u8, url, "postgresql://")) {
        return .{ .backend = .postgres, .path = url };
    } else {
        // Default to SQLite for bare paths
        return .{ .backend = .sqlite, .path = url };
    }
}

// =============================================================================
// Registration
// =============================================================================

/// Register std.sql functions
pub fn register(registry: anytype) !void {
    // Connection management
    try registry.registerNative("std.sql.open", sql_open);
    try registry.registerNative("std.sql.close", sql_close);

    // Query execution
    try registry.registerNative("std.sql.exec", sql_exec);
    try registry.registerNative("std.sql.query", sql_query);
    try registry.registerNative("std.sql.fetch", sql_fetch);
    try registry.registerNative("std.sql.fetch_all", sql_fetch_all);

    // Result inspection
    try registry.registerNative("std.sql.column_count", sql_column_count);
    try registry.registerNative("std.sql.column_name", sql_column_name);

    // Transactions
    try registry.registerNative("std.sql.begin", sql_begin);
    try registry.registerNative("std.sql.commit", sql_commit);
    try registry.registerNative("std.sql.rollback", sql_rollback);

    // Short names
    try registry.registerNative("sql_open", sql_open);
    try registry.registerNative("sql_close", sql_close);
    try registry.registerNative("sql_exec", sql_exec);
    try registry.registerNative("sql_query", sql_query);
    try registry.registerNative("sql_fetch", sql_fetch);
    try registry.registerNative("sql_fetch_all", sql_fetch_all);
    try registry.registerNative("sql_begin", sql_begin);
    try registry.registerNative("sql_commit", sql_commit);
    try registry.registerNative("sql_rollback", sql_rollback);
}

// =============================================================================
// Connection Functions
// =============================================================================

/// sql_open(url) -> handle
/// Open a database connection. URL determines backend:
///   sqlite:path.db, sqlite::memory:, postgres://...
fn sql_open(ctx: *NativeContext) NativeError!?Value {
    initHandles(ctx.allocator);

    const url = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const parsed = parseUrl(url);

    switch (parsed.backend) {
        .sqlite => {
            // Handle :memory: special case
            const path_z = if (std.mem.eql(u8, parsed.path, ":memory:"))
                ":memory:"
            else blk: {
                const buf = ctx.allocator.allocSentinel(u8, parsed.path.len, 0) catch return NativeError.OutOfMemory;
                @memcpy(buf, parsed.path);
                break :blk buf;
            };
            defer if (!std.mem.eql(u8, parsed.path, ":memory:")) ctx.allocator.free(path_z);

            var db: ?*c.sqlite3 = null;
            const rc = c.sqlite3_open(path_z.ptr, &db);
            if (rc != c.SQLITE_OK) {
                if (db) |d| _ = c.sqlite3_close(d);
                debug.print(.general, "sql_open: SQLite error: {s}", .{c.sqlite3_errmsg(db)});
                return NativeError.FileError;
            }

            const handle = SqlHandle{
                .connection = .{
                    .backend = .sqlite,
                    .sqlite = SqliteConn{ .db = db.? },
                    .postgres = null,
                },
            };

            const id = allocHandle(handle) catch return NativeError.OutOfMemory;
            debug.print(.general, "sql_open: opened SQLite connection, handle={}", .{id});
            return Value.initInt(@intCast(id));
        },
        .postgres => {
            if (comptime !postgres_enabled) {
                debug.print(.general, "sql_open: PostgreSQL not enabled", .{});
                return NativeError.NotImplemented;
            }

            // Null-terminate URL for libpq
            const url_z = ctx.allocator.allocSentinel(u8, url.len, 0) catch return NativeError.OutOfMemory;
            defer ctx.allocator.free(url_z);
            @memcpy(url_z, url);

            const conn = pq.PQconnectdb(url_z.ptr);
            if (conn == null) {
                debug.print(.general, "sql_open: PostgreSQL connection failed (null)", .{});
                return NativeError.FileError;
            }

            if (pq.PQstatus(conn.?) != pq.CONNECTION_OK) {
                const err_msg = pq.PQerrorMessage(conn.?);
                debug.print(.general, "sql_open: PostgreSQL error: {s}", .{err_msg});
                pq.PQfinish(conn.?);
                return NativeError.FileError;
            }

            const handle = SqlHandle{
                .connection = .{
                    .backend = .postgres,
                    .sqlite = null,
                    .postgres = PostgresConn{ .conn = conn.? },
                },
            };

            const id = allocHandle(handle) catch return NativeError.OutOfMemory;
            debug.print(.general, "sql_open: opened PostgreSQL connection, handle={}", .{id});
            return Value.initInt(@intCast(id));
        },
    }
}

/// sql_close(handle) -> void
fn sql_close(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    freeHandle(handle_id);
    debug.print(.general, "sql_close: closed handle={}", .{handle_id});
    return null;
}

// =============================================================================
// Query Execution
// =============================================================================

/// sql_exec(handle, sql, params...) -> rows_affected
/// Execute a statement (INSERT, UPDATE, DELETE, CREATE, etc.)
fn sql_exec(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const sql = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .connection => |conn| {
            switch (conn.backend) {
                .sqlite => {
                    const sq = conn.sqlite orelse return NativeError.InvalidArgument;
                    return sqliteExec(ctx, sq.db, sql);
                },
                .postgres => {
                    const pg = conn.postgres orelse return NativeError.InvalidArgument;
                    return postgresExec(ctx, pg.conn, sql);
                },
            }
        },
        .result_set => return NativeError.InvalidArgument,
    }
}

fn sqliteExec(ctx: *NativeContext, db: *c.sqlite3, sql: []const u8) NativeError!?Value {
    // Null-terminate SQL
    const sql_z = ctx.allocator.allocSentinel(u8, sql.len, 0) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(sql_z);
    @memcpy(sql_z, sql);

    var stmt: ?*c.sqlite3_stmt = null;
    const rc = c.sqlite3_prepare_v2(db, sql_z.ptr, @intCast(sql.len), &stmt, null);
    if (rc != c.SQLITE_OK) {
        debug.print(.general, "sql_exec: prepare error: {s}", .{c.sqlite3_errmsg(db)});
        return NativeError.FileError;
    }
    defer _ = c.sqlite3_finalize(stmt);

    // Bind parameters (args 2+)
    var param_idx: c_int = 1;
    for (2..ctx.args.len) |i| {
        const arg = ctx.args[i];
        if (arg.isInt()) {
            _ = c.sqlite3_bind_int64(stmt, param_idx, arg.toInt());
        } else if (arg.isDecimal()) {
            // Decimal stored as scaled integer - convert to double for SQL
            const dec_val: f64 = @floatFromInt(arg.toInt());
            _ = c.sqlite3_bind_double(stmt, param_idx, dec_val / 100.0); // Assume 2 decimal places
        } else if (arg.isString()) {
            const s = arg.toString();
            _ = c.sqlite3_bind_text(stmt, param_idx, s.ptr, @intCast(s.len), SQLITE_STATIC);
        } else if (arg.isNull()) {
            _ = c.sqlite3_bind_null(stmt, param_idx);
        }
        param_idx += 1;
    }

    const step_rc = c.sqlite3_step(stmt);
    if (step_rc != c.SQLITE_DONE and step_rc != c.SQLITE_ROW) {
        debug.print(.general, "sql_exec: step error: {s}", .{c.sqlite3_errmsg(db)});
        return NativeError.FileError;
    }

    const changes = c.sqlite3_changes(db);
    return Value.initInt(changes);
}

fn postgresExec(ctx: *NativeContext, conn: *pq.PGconn, sql: []const u8) NativeError!?Value {
    if (comptime !postgres_enabled) {
        return NativeError.NotImplemented;
    }

    // Null-terminate SQL
    const sql_z = ctx.allocator.allocSentinel(u8, sql.len, 0) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(sql_z);
    @memcpy(sql_z, sql);

    // Count parameters (args 2+)
    const param_count = ctx.args.len - 2;

    if (param_count == 0) {
        // No parameters - use simple exec
        const result = pq.PQexec(conn, sql_z.ptr);
        if (result == null) {
            debug.print(.general, "sql_exec: PostgreSQL exec failed (null result)", .{});
            return NativeError.FileError;
        }
        defer pq.PQclear(result.?);

        const status = pq.PQresultStatus(result.?);
        if (status != pq.PGRES_COMMAND_OK and status != pq.PGRES_TUPLES_OK) {
            debug.print(.general, "sql_exec: PostgreSQL error: {s}", .{pq.PQerrorMessage(conn)});
            return NativeError.FileError;
        }

        // Get rows affected
        const cmd_tuples = pq.PQcmdTuples(result.?);
        var rows_affected: i64 = 0;
        if (cmd_tuples) |ct| {
            const ct_slice = std.mem.span(ct);
            if (ct_slice.len > 0) {
                rows_affected = std.fmt.parseInt(i64, ct_slice, 10) catch 0;
            }
        }
        return Value.initInt(rows_affected);
    }

    // Build parameter arrays for PQexecParams
    const param_values = ctx.allocator.alloc([*c]const u8, param_count) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(param_values);

    // Storage for string representations of non-string values
    var param_buffers: std.ArrayListUnmanaged([]u8) = .empty;
    defer {
        for (param_buffers.items) |buf| ctx.allocator.free(buf);
        param_buffers.deinit(ctx.allocator);
    }

    for (0..param_count) |i| {
        const arg = ctx.args[i + 2];
        if (arg.isNull()) {
            param_values[i] = null;
        } else if (arg.isString()) {
            const s = arg.toString();
            // Need null-terminated copy
            const s_z = ctx.allocator.allocSentinel(u8, s.len, 0) catch return NativeError.OutOfMemory;
            @memcpy(s_z, s);
            param_buffers.append(ctx.allocator, s_z) catch return NativeError.OutOfMemory;
            param_values[i] = s_z.ptr;
        } else if (arg.isInt()) {
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{arg.toInt()}) catch "0";
            const copy = ctx.allocator.allocSentinel(u8, formatted.len, 0) catch return NativeError.OutOfMemory;
            @memcpy(copy, formatted);
            param_buffers.append(ctx.allocator, copy) catch return NativeError.OutOfMemory;
            param_values[i] = copy.ptr;
        } else if (arg.isDecimal()) {
            const dec_val: f64 = @as(f64, @floatFromInt(arg.toInt())) / 100.0;
            var buf: [32]u8 = undefined;
            const formatted = std.fmt.bufPrint(&buf, "{d}", .{dec_val}) catch "0";
            const copy = ctx.allocator.allocSentinel(u8, formatted.len, 0) catch return NativeError.OutOfMemory;
            @memcpy(copy, formatted);
            param_buffers.append(ctx.allocator, copy) catch return NativeError.OutOfMemory;
            param_values[i] = copy.ptr;
        } else {
            param_values[i] = null;
        }
    }

    // Execute with parameters
    const result = pq.PQexecParams(
        conn,
        sql_z.ptr,
        @intCast(param_count),
        null, // Let PostgreSQL infer types
        @ptrCast(param_values.ptr),
        null, // lengths (NULL for text)
        null, // formats (NULL = text)
        0, // result format (text)
    );

    if (result == null) {
        debug.print(.general, "sql_exec: PostgreSQL execParams failed (null result)", .{});
        return NativeError.FileError;
    }
    defer pq.PQclear(result.?);

    const status = pq.PQresultStatus(result.?);
    if (status != pq.PGRES_COMMAND_OK and status != pq.PGRES_TUPLES_OK) {
        debug.print(.general, "sql_exec: PostgreSQL error: {s}", .{pq.PQerrorMessage(conn)});
        return NativeError.FileError;
    }

    // Get rows affected
    const cmd_tuples = pq.PQcmdTuples(result.?);
    var rows_affected: i64 = 0;
    if (cmd_tuples) |ct| {
        const ct_slice = std.mem.span(ct);
        if (ct_slice.len > 0) {
            rows_affected = std.fmt.parseInt(i64, ct_slice, 10) catch 0;
        }
    }

    return Value.initInt(rows_affected);
}

/// sql_query(handle, sql, params...) -> result_handle
/// Execute a query and return a result set handle for iteration
fn sql_query(ctx: *NativeContext) NativeError!?Value {
    initHandles(ctx.allocator);

    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const sql = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .connection => |conn| {
            switch (conn.backend) {
                .sqlite => {
                    const sq = conn.sqlite orelse return NativeError.InvalidArgument;
                    return sqliteQuery(ctx, sq.db, sql);
                },
                .postgres => {
                    const pg = conn.postgres orelse return NativeError.InvalidArgument;
                    return postgresQuery(ctx, pg.conn, sql);
                },
            }
        },
        .result_set => return NativeError.InvalidArgument,
    }
}

fn sqliteQuery(ctx: *NativeContext, db: *c.sqlite3, sql: []const u8) NativeError!?Value {
    // Null-terminate SQL
    const sql_z = ctx.allocator.allocSentinel(u8, sql.len, 0) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(sql_z);
    @memcpy(sql_z, sql);

    var stmt: ?*c.sqlite3_stmt = null;
    const rc = c.sqlite3_prepare_v2(db, sql_z.ptr, @intCast(sql.len), &stmt, null);
    if (rc != c.SQLITE_OK) {
        debug.print(.general, "sql_query: prepare error: {s}", .{c.sqlite3_errmsg(db)});
        return NativeError.FileError;
    }

    // Bind parameters (args 2+)
    var param_idx: c_int = 1;
    for (2..ctx.args.len) |i| {
        const arg = ctx.args[i];
        if (arg.isInt()) {
            _ = c.sqlite3_bind_int64(stmt, param_idx, arg.toInt());
        } else if (arg.isDecimal()) {
            const dec_val: f64 = @floatFromInt(arg.toInt());
            _ = c.sqlite3_bind_double(stmt, param_idx, dec_val / 100.0);
        } else if (arg.isString()) {
            const s = arg.toString();
            _ = c.sqlite3_bind_text(stmt, param_idx, s.ptr, @intCast(s.len), SQLITE_STATIC);
        } else if (arg.isNull()) {
            _ = c.sqlite3_bind_null(stmt, param_idx);
        }
        param_idx += 1;
    }

    // Get column info
    const col_count: usize = @intCast(c.sqlite3_column_count(stmt));
    const col_names = ctx.allocator.alloc([]const u8, col_count) catch return NativeError.OutOfMemory;
    for (0..col_count) |i| {
        const name_ptr = c.sqlite3_column_name(stmt, @intCast(i));
        if (name_ptr) |np| {
            const name_len = std.mem.len(np);
            const name_copy = ctx.allocator.alloc(u8, name_len) catch return NativeError.OutOfMemory;
            @memcpy(name_copy, np[0..name_len]);
            col_names[i] = name_copy;
        } else {
            col_names[i] = "";
        }
    }

    const result = SqlHandle{
        .result_set = ResultSet{
            .backend = .sqlite,
            .sqlite_stmt = stmt,
            .column_count = col_count,
            .column_names = col_names,
            .allocator = ctx.allocator,
        },
    };

    const id = allocHandle(result) catch return NativeError.OutOfMemory;
    debug.print(.general, "sql_query: created result set, handle={}, cols={}", .{ id, col_count });
    return Value.initInt(@intCast(id));
}

fn postgresQuery(ctx: *NativeContext, conn: *pq.PGconn, sql: []const u8) NativeError!?Value {
    if (comptime !postgres_enabled) {
        return NativeError.NotImplemented;
    }

    // Null-terminate SQL
    const sql_z = ctx.allocator.allocSentinel(u8, sql.len, 0) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(sql_z);
    @memcpy(sql_z, sql);

    // Count parameters (args 2+)
    const param_count = ctx.args.len - 2;

    var pg_result: ?*pq.PGresult = null;

    if (param_count == 0) {
        // No parameters - use simple exec
        pg_result = pq.PQexec(conn, sql_z.ptr);
    } else {
        // Build parameter arrays
        const param_values = ctx.allocator.alloc([*c]const u8, param_count) catch return NativeError.OutOfMemory;
        defer ctx.allocator.free(param_values);

        var param_buffers: std.ArrayListUnmanaged([]u8) = .empty;
        defer {
            for (param_buffers.items) |buf| ctx.allocator.free(buf);
            param_buffers.deinit(ctx.allocator);
        }

        for (0..param_count) |i| {
            const arg = ctx.args[i + 2];
            if (arg.isNull()) {
                param_values[i] = null;
            } else if (arg.isString()) {
                const s = arg.toString();
                const s_z = ctx.allocator.allocSentinel(u8, s.len, 0) catch return NativeError.OutOfMemory;
                @memcpy(s_z, s);
                param_buffers.append(ctx.allocator, s_z) catch return NativeError.OutOfMemory;
                param_values[i] = s_z.ptr;
            } else if (arg.isInt()) {
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{arg.toInt()}) catch "0";
                const copy = ctx.allocator.allocSentinel(u8, formatted.len, 0) catch return NativeError.OutOfMemory;
                @memcpy(copy, formatted);
                param_buffers.append(ctx.allocator, copy) catch return NativeError.OutOfMemory;
                param_values[i] = copy.ptr;
            } else if (arg.isDecimal()) {
                const dec_val: f64 = @as(f64, @floatFromInt(arg.toInt())) / 100.0;
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{dec_val}) catch "0";
                const copy = ctx.allocator.allocSentinel(u8, formatted.len, 0) catch return NativeError.OutOfMemory;
                @memcpy(copy, formatted);
                param_buffers.append(ctx.allocator, copy) catch return NativeError.OutOfMemory;
                param_values[i] = copy.ptr;
            } else {
                param_values[i] = null;
            }
        }

        pg_result = pq.PQexecParams(
            conn,
            sql_z.ptr,
            @intCast(param_count),
            null,
            @ptrCast(param_values.ptr),
            null,
            null,
            0,
        );
    }

    if (pg_result == null) {
        debug.print(.general, "sql_query: PostgreSQL query failed (null result)", .{});
        return NativeError.FileError;
    }

    const status = pq.PQresultStatus(pg_result.?);
    if (status != pq.PGRES_TUPLES_OK and status != pq.PGRES_COMMAND_OK) {
        debug.print(.general, "sql_query: PostgreSQL error: {s}", .{pq.PQerrorMessage(conn)});
        pq.PQclear(pg_result.?);
        return NativeError.FileError;
    }

    // Get column info
    const col_count: usize = @intCast(pq.PQnfields(pg_result.?));
    const num_rows = pq.PQntuples(pg_result.?);

    const col_names = ctx.allocator.alloc([]const u8, col_count) catch {
        pq.PQclear(pg_result.?);
        return NativeError.OutOfMemory;
    };

    for (0..col_count) |i| {
        const name_ptr = pq.PQfname(pg_result.?, @intCast(i));
        if (name_ptr) |np| {
            const name_len = std.mem.len(np);
            const name_copy = ctx.allocator.alloc(u8, name_len) catch {
                // Clean up already allocated names
                for (0..i) |j| ctx.allocator.free(col_names[j]);
                ctx.allocator.free(col_names);
                pq.PQclear(pg_result.?);
                return NativeError.OutOfMemory;
            };
            @memcpy(name_copy, np[0..name_len]);
            col_names[i] = name_copy;
        } else {
            col_names[i] = "";
        }
    }

    const result = SqlHandle{
        .result_set = ResultSet{
            .backend = .postgres,
            .pg_result = pg_result,
            .pg_row_idx = 0,
            .pg_num_rows = num_rows,
            .column_count = col_count,
            .column_names = col_names,
            .allocator = ctx.allocator,
        },
    };

    const id = allocHandle(result) catch {
        for (col_names) |name| ctx.allocator.free(name);
        ctx.allocator.free(col_names);
        pq.PQclear(pg_result.?);
        return NativeError.OutOfMemory;
    };

    debug.print(.general, "sql_query: created PostgreSQL result set, handle={}, cols={}, rows={}", .{ id, col_count, num_rows });
    return Value.initInt(@intCast(id));
}

/// sql_fetch(result_handle) -> ?string (JSON row or null if exhausted)
fn sql_fetch(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .result_set => |*rs| {
            if (rs.exhausted) return Value.initNull();

            switch (rs.backend) {
                .sqlite => {
                    const stmt = rs.sqlite_stmt orelse return NativeError.InvalidArgument;
                    const rc = c.sqlite3_step(stmt);

                    if (rc == c.SQLITE_DONE) {
                        rs.exhausted = true;
                        return Value.initNull();
                    } else if (rc != c.SQLITE_ROW) {
                        return NativeError.FileError;
                    }

                    // Build JSON object for row
                    return buildRowJson(ctx, stmt, rs.column_names);
                },
                .postgres => {
                    const pg_res = rs.pg_result orelse return NativeError.InvalidArgument;

                    if (rs.pg_row_idx >= rs.pg_num_rows) {
                        rs.exhausted = true;
                        return Value.initNull();
                    }

                    // Build JSON object for current row
                    const result_json = try buildPgRowJson(ctx, pg_res, rs.pg_row_idx, rs.column_names);
                    rs.pg_row_idx += 1;
                    return result_json;
                },
            }
        },
        .connection => return NativeError.InvalidArgument,
    }
}

fn buildRowJson(ctx: *NativeContext, stmt: *c.sqlite3_stmt, col_names: [][]const u8) NativeError!?Value {
    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(ctx.allocator);

    result.append(ctx.allocator, '{') catch return NativeError.OutOfMemory;

    for (col_names, 0..) |name, i| {
        if (i > 0) {
            result.append(ctx.allocator, ',') catch return NativeError.OutOfMemory;
        }

        // Key
        result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
        result.appendSlice(ctx.allocator, name) catch return NativeError.OutOfMemory;
        result.appendSlice(ctx.allocator, "\":") catch return NativeError.OutOfMemory;

        // Value
        const col_type = c.sqlite3_column_type(stmt, @intCast(i));
        switch (col_type) {
            c.SQLITE_NULL => {
                result.appendSlice(ctx.allocator, "null") catch return NativeError.OutOfMemory;
            },
            c.SQLITE_INTEGER => {
                const val = c.sqlite3_column_int64(stmt, @intCast(i));
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
                result.appendSlice(ctx.allocator, formatted) catch return NativeError.OutOfMemory;
            },
            c.SQLITE_FLOAT => {
                const val = c.sqlite3_column_double(stmt, @intCast(i));
                var buf: [32]u8 = undefined;
                const formatted = std.fmt.bufPrint(&buf, "{d}", .{val}) catch "0";
                result.appendSlice(ctx.allocator, formatted) catch return NativeError.OutOfMemory;
            },
            c.SQLITE_TEXT => {
                const text_ptr = c.sqlite3_column_text(stmt, @intCast(i));
                const text_len = c.sqlite3_column_bytes(stmt, @intCast(i));
                result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
                if (text_ptr) |tp| {
                    // Escape JSON special chars
                    const text = tp[0..@intCast(text_len)];
                    for (text) |ch| {
                        switch (ch) {
                            '"' => result.appendSlice(ctx.allocator, "\\\"") catch return NativeError.OutOfMemory,
                            '\\' => result.appendSlice(ctx.allocator, "\\\\") catch return NativeError.OutOfMemory,
                            '\n' => result.appendSlice(ctx.allocator, "\\n") catch return NativeError.OutOfMemory,
                            '\r' => result.appendSlice(ctx.allocator, "\\r") catch return NativeError.OutOfMemory,
                            '\t' => result.appendSlice(ctx.allocator, "\\t") catch return NativeError.OutOfMemory,
                            else => result.append(ctx.allocator, ch) catch return NativeError.OutOfMemory,
                        }
                    }
                }
                result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
            },
            c.SQLITE_BLOB => {
                result.appendSlice(ctx.allocator, "null") catch return NativeError.OutOfMemory; // TODO: base64
            },
            else => {
                result.appendSlice(ctx.allocator, "null") catch return NativeError.OutOfMemory;
            },
        }
    }

    result.append(ctx.allocator, '}') catch return NativeError.OutOfMemory;

    const owned = result.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, owned) catch return NativeError.OutOfMemory;
}

fn buildPgRowJson(ctx: *NativeContext, pg_result: *pq.PGresult, row_idx: c_int, col_names: [][]const u8) NativeError!?Value {
    if (comptime !postgres_enabled) {
        return NativeError.NotImplemented;
    }

    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(ctx.allocator);

    result.append(ctx.allocator, '{') catch return NativeError.OutOfMemory;

    for (col_names, 0..) |name, i| {
        if (i > 0) {
            result.append(ctx.allocator, ',') catch return NativeError.OutOfMemory;
        }

        // Key
        result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
        result.appendSlice(ctx.allocator, name) catch return NativeError.OutOfMemory;
        result.appendSlice(ctx.allocator, "\":") catch return NativeError.OutOfMemory;

        // Value
        const col_idx: c_int = @intCast(i);
        if (pq.PQgetisnull(pg_result, row_idx, col_idx) == 1) {
            result.appendSlice(ctx.allocator, "null") catch return NativeError.OutOfMemory;
        } else {
            const val_ptr = pq.PQgetvalue(pg_result, row_idx, col_idx);
            const val_len: usize = @intCast(pq.PQgetlength(pg_result, row_idx, col_idx));

            if (val_ptr) |vp| {
                const val_slice = vp[0..val_len];

                // Try to parse as integer
                if (std.fmt.parseInt(i64, val_slice, 10)) |_| {
                    result.appendSlice(ctx.allocator, val_slice) catch return NativeError.OutOfMemory;
                } else |_| {
                    // Try to parse as float
                    if (std.fmt.parseFloat(f64, val_slice)) |_| {
                        result.appendSlice(ctx.allocator, val_slice) catch return NativeError.OutOfMemory;
                    } else |_| {
                        // Treat as string - need JSON escaping
                        result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
                        for (val_slice) |ch| {
                            switch (ch) {
                                '"' => result.appendSlice(ctx.allocator, "\\\"") catch return NativeError.OutOfMemory,
                                '\\' => result.appendSlice(ctx.allocator, "\\\\") catch return NativeError.OutOfMemory,
                                '\n' => result.appendSlice(ctx.allocator, "\\n") catch return NativeError.OutOfMemory,
                                '\r' => result.appendSlice(ctx.allocator, "\\r") catch return NativeError.OutOfMemory,
                                '\t' => result.appendSlice(ctx.allocator, "\\t") catch return NativeError.OutOfMemory,
                                else => result.append(ctx.allocator, ch) catch return NativeError.OutOfMemory,
                            }
                        }
                        result.append(ctx.allocator, '"') catch return NativeError.OutOfMemory;
                    }
                }
            } else {
                result.appendSlice(ctx.allocator, "null") catch return NativeError.OutOfMemory;
            }
        }
    }

    result.append(ctx.allocator, '}') catch return NativeError.OutOfMemory;

    const owned = result.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, owned) catch return NativeError.OutOfMemory;
}

/// sql_fetch_all(result_handle) -> string (JSON array of all rows)
fn sql_fetch_all(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .result_set => |*rs| {
            var rows: std.ArrayListUnmanaged(u8) = .empty;
            defer rows.deinit(ctx.allocator);

            rows.append(ctx.allocator, '[') catch return NativeError.OutOfMemory;

            var first = true;
            while (true) {
                if (rs.exhausted) break;

                switch (rs.backend) {
                    .sqlite => {
                        const stmt = rs.sqlite_stmt orelse break;
                        const rc = c.sqlite3_step(stmt);

                        if (rc == c.SQLITE_DONE) {
                            rs.exhausted = true;
                            break;
                        } else if (rc != c.SQLITE_ROW) {
                            break;
                        }

                        if (!first) {
                            rows.append(ctx.allocator, ',') catch return NativeError.OutOfMemory;
                        }
                        first = false;

                        // Build row JSON inline
                        const row_json = try buildRowJson(ctx, stmt, rs.column_names);
                        if (row_json) |rj| {
                            const json_str = rj.toString();
                            rows.appendSlice(ctx.allocator, json_str) catch return NativeError.OutOfMemory;
                        }
                    },
                    .postgres => {
                        const pg_res = rs.pg_result orelse break;

                        if (rs.pg_row_idx >= rs.pg_num_rows) {
                            rs.exhausted = true;
                            break;
                        }

                        if (!first) {
                            rows.append(ctx.allocator, ',') catch return NativeError.OutOfMemory;
                        }
                        first = false;

                        // Build row JSON
                        const row_json = try buildPgRowJson(ctx, pg_res, rs.pg_row_idx, rs.column_names);
                        if (row_json) |rj| {
                            const json_str = rj.toString();
                            rows.appendSlice(ctx.allocator, json_str) catch return NativeError.OutOfMemory;
                        }
                        rs.pg_row_idx += 1;
                    },
                }
            }

            rows.append(ctx.allocator, ']') catch return NativeError.OutOfMemory;

            const owned = rows.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
            return Value.initString(ctx.allocator, owned) catch return NativeError.OutOfMemory;
        },
        .connection => return NativeError.InvalidArgument,
    }
}

// =============================================================================
// Result Inspection
// =============================================================================

/// sql_column_count(result_handle) -> i64
fn sql_column_count(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .result_set => |rs| {
            return Value.initInt(@intCast(rs.column_count));
        },
        .connection => return NativeError.InvalidArgument,
    }
}

/// sql_column_name(result_handle, index) -> string
fn sql_column_name(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const idx: usize = @intCast(ctx.getArgInt(1) catch return NativeError.InvalidArgument);

    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .result_set => |rs| {
            if (idx >= rs.column_names.len) return NativeError.InvalidArgument;
            const name = ctx.allocator.dupe(u8, rs.column_names[idx]) catch return NativeError.OutOfMemory;
            return Value.initString(ctx.allocator, name) catch return NativeError.OutOfMemory;
        },
        .connection => return NativeError.InvalidArgument,
    }
}

// =============================================================================
// Transaction Functions
// =============================================================================

/// sql_begin(handle) -> void
fn sql_begin(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .connection => |*conn| {
            switch (conn.backend) {
                .sqlite => {
                    if (conn.sqlite) |*sq| {
                        _ = sqliteExecSimple(sq.db, "BEGIN");
                        sq.in_transaction = true;
                    }
                },
                .postgres => {
                    if (conn.postgres) |*pg| {
                        if (!postgresExecSimple(pg.conn, "BEGIN")) {
                            return NativeError.FileError;
                        }
                        pg.in_transaction = true;
                    }
                },
            }
        },
        .result_set => return NativeError.InvalidArgument,
    }
    return null;
}

/// sql_commit(handle) -> void
fn sql_commit(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .connection => |*conn| {
            switch (conn.backend) {
                .sqlite => {
                    if (conn.sqlite) |*sq| {
                        _ = sqliteExecSimple(sq.db, "COMMIT");
                        sq.in_transaction = false;
                    }
                },
                .postgres => {
                    if (conn.postgres) |*pg| {
                        if (!postgresExecSimple(pg.conn, "COMMIT")) {
                            return NativeError.FileError;
                        }
                        pg.in_transaction = false;
                    }
                },
            }
        },
        .result_set => return NativeError.InvalidArgument,
    }
    return null;
}

/// sql_rollback(handle) -> void
fn sql_rollback(ctx: *NativeContext) NativeError!?Value {
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const handle = getHandle(handle_id) orelse return NativeError.InvalidArgument;

    switch (handle.*) {
        .connection => |*conn| {
            switch (conn.backend) {
                .sqlite => {
                    if (conn.sqlite) |*sq| {
                        _ = sqliteExecSimple(sq.db, "ROLLBACK");
                        sq.in_transaction = false;
                    }
                },
                .postgres => {
                    if (conn.postgres) |*pg| {
                        if (!postgresExecSimple(pg.conn, "ROLLBACK")) {
                            return NativeError.FileError;
                        }
                        pg.in_transaction = false;
                    }
                },
            }
        },
        .result_set => return NativeError.InvalidArgument,
    }
    return null;
}

fn sqliteExecSimple(db: *c.sqlite3, sql: [*:0]const u8) bool {
    var err_msg: [*c]u8 = null;
    const rc = c.sqlite3_exec(db, sql, null, null, &err_msg);
    if (err_msg) |msg| c.sqlite3_free(msg);
    return rc == c.SQLITE_OK;
}

fn postgresExecSimple(conn: *pq.PGconn, sql: [*:0]const u8) bool {
    if (comptime !postgres_enabled) {
        return false;
    }

    const result = pq.PQexec(conn, sql);
    if (result == null) {
        return false;
    }
    defer pq.PQclear(result.?);

    const status = pq.PQresultStatus(result.?);
    return status == pq.PGRES_COMMAND_OK or status == pq.PGRES_TUPLES_OK;
}
