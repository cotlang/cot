//! Schema Metadata Storage
//!
//! Stores schema information in the database for runtime introspection.
//! This allows querying table structures, constraints, and versioning
//! directly from the database.
//!
//! ## Tables Created
//!
//! - `_cot_schema`: Schema version tracking
//! - `_cot_tables`: Table metadata
//! - `_cot_columns`: Column metadata with decorators

const std = @import("std");
const types = @import("types.zig");

const SchemaFile = types.SchemaFile;
const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const SqlDialect = types.SqlDialect;

/// SQL statements to create schema metadata tables
pub const CreateMetadataTablesSql = struct {
    /// Create the schema version tracking table
    pub const schema_table =
        \\CREATE TABLE IF NOT EXISTS _cot_schema (
        \\    version INTEGER PRIMARY KEY,
        \\    applied_at INTEGER NOT NULL,
        \\    description TEXT
        \\);
    ;

    /// Create the tables metadata table
    pub const tables_table =
        \\CREATE TABLE IF NOT EXISTS _cot_tables (
        \\    name TEXT PRIMARY KEY,
        \\    schema_version INTEGER NOT NULL,
        \\    record_size INTEGER,
        \\    created_at INTEGER,
        \\    updated_at INTEGER
        \\);
    ;

    /// Create the columns metadata table
    pub const columns_table =
        \\CREATE TABLE IF NOT EXISTS _cot_columns (
        \\    table_name TEXT NOT NULL,
        \\    name TEXT NOT NULL,
        \\    position INTEGER NOT NULL,
        \\    data_type TEXT NOT NULL,
        \\    length INTEGER,
        \\    decimal_places INTEGER,
        \\    is_primary INTEGER DEFAULT 0,
        \\    is_auto INTEGER DEFAULT 0,
        \\    is_not_null INTEGER DEFAULT 0,
        \\    is_unique INTEGER DEFAULT 0,
        \\    is_indexed INTEGER DEFAULT 0,
        \\    default_value TEXT,
        \\    foreign_table TEXT,
        \\    foreign_column TEXT,
        \\    on_delete TEXT,
        \\    is_audit_created INTEGER DEFAULT 0,
        \\    is_audit_updated INTEGER DEFAULT 0,
        \\    PRIMARY KEY (table_name, name)
        \\);
    ;

    /// Create index on columns table for table lookups
    pub const columns_index =
        \\CREATE INDEX IF NOT EXISTS idx_cot_columns_table
        \\ON _cot_columns(table_name);
    ;
};

/// Get all SQL statements needed to initialize metadata tables
pub fn getCreateMetadataTablesSql() []const []const u8 {
    return &[_][]const u8{
        CreateMetadataTablesSql.schema_table,
        CreateMetadataTablesSql.tables_table,
        CreateMetadataTablesSql.columns_table,
        CreateMetadataTablesSql.columns_index,
    };
}

/// Generate SQL to insert/update schema version
pub fn generateSchemaVersionSql(
    allocator: std.mem.Allocator,
    version: u32,
    description: []const u8,
) ![]const u8 {
    const timestamp = std.time.timestamp();
    return try std.fmt.allocPrint(
        allocator,
        "INSERT OR REPLACE INTO _cot_schema (version, applied_at, description) VALUES ({d}, {d}, '{s}');",
        .{ version, timestamp, description },
    );
}

/// Generate SQL to insert table metadata
pub fn generateTableMetadataSql(
    allocator: std.mem.Allocator,
    table: *const TableSchema,
    schema_version: u32,
) ![]const u8 {
    const timestamp = std.time.timestamp();
    const record_size = table.recordSize();

    return try std.fmt.allocPrint(
        allocator,
        "INSERT OR REPLACE INTO _cot_tables (name, schema_version, record_size, created_at, updated_at) " ++
            "VALUES ('{s}', {d}, {d}, {d}, {d});",
        .{ table.name, schema_version, record_size, timestamp, timestamp },
    );
}

/// Generate SQL to insert column metadata
pub fn generateColumnMetadataSql(
    allocator: std.mem.Allocator,
    table_name: []const u8,
    field: *const FieldSchema,
    position: usize,
) ![]const u8 {
    // Determine data type string and length
    const type_info = getDataTypeInfo(field.data_type);
    const d = field.decorators;

    // Build foreign key references
    const foreign_table = if (d.foreign_key) |fk| fk.table else "";
    const foreign_column = if (d.foreign_key) |fk| fk.column else "";
    const on_delete = if (d.on_delete) |action| switch (action) {
        .cascade => "CASCADE",
        .set_null => "SET NULL",
        .restrict => "RESTRICT",
    } else "";

    const default_val = d.default_value orelse "";

    return try std.fmt.allocPrint(
        allocator,
        "INSERT OR REPLACE INTO _cot_columns (" ++
            "table_name, name, position, data_type, length, decimal_places, " ++
            "is_primary, is_auto, is_not_null, is_unique, is_indexed, " ++
            "default_value, foreign_table, foreign_column, on_delete, " ++
            "is_audit_created, is_audit_updated" ++
            ") VALUES (" ++
            "'{s}', '{s}', {d}, '{s}', {d}, {d}, " ++
            "{d}, {d}, {d}, {d}, {d}, " ++
            "'{s}', '{s}', '{s}', '{s}', " ++
            "{d}, {d}" ++
            ");",
        .{
            table_name,
            field.name,
            position,
            type_info.type_name,
            type_info.length,
            type_info.decimal_places,
            @intFromBool(d.primary),
            @intFromBool(d.auto),
            @intFromBool(d.not_null),
            @intFromBool(d.unique),
            @intFromBool(d.index),
            default_val,
            foreign_table,
            foreign_column,
            on_delete,
            @intFromBool(d.audit_created),
            @intFromBool(d.audit_updated),
        },
    );
}

/// Data type information for storage
const DataTypeInfo = struct {
    type_name: []const u8,
    length: usize,
    decimal_places: usize,
};

/// Extract data type information for metadata storage
fn getDataTypeInfo(data_type: types.SchemaDataType) DataTypeInfo {
    return switch (data_type) {
        .text => |t| .{
            .type_name = "text",
            .length = t.max_length orelse 0,
            .decimal_places = 0,
        },
        .integer => |i| .{
            .type_name = "integer",
            .length = switch (i) {
                .tiny => 1,
                .small => 2,
                .medium => 4,
                .big => 8,
            },
            .decimal_places = 0,
        },
        .decimal => |d| .{
            .type_name = "decimal",
            .length = d.precision,
            .decimal_places = d.scale,
        },
        .boolean => .{
            .type_name = "boolean",
            .length = 1,
            .decimal_places = 0,
        },
        .blob => |b| .{
            .type_name = "blob",
            .length = b.max_size orelse 0,
            .decimal_places = 0,
        },
        .datetime => |dt| .{
            .type_name = switch (dt) {
                .date => "date",
                .time => "time",
                .datetime => "datetime",
                .timestamp => "timestamp",
            },
            .length = 8,
            .decimal_places = 0,
        },
        .ulid => .{
            .type_name = "ulid",
            .length = 26, // ULID is 26 characters
            .decimal_places = 0,
        },
        .reference => .{
            .type_name = "reference",
            .length = 26, // Same as ULID
            .decimal_places = 0,
        },
    };
}

/// Generate all metadata SQL for a schema file
pub fn generateSchemaMetadataSql(
    allocator: std.mem.Allocator,
    schema: *const SchemaFile,
) ![]const u8 {
    var sql: std.ArrayListAligned(u8, null) = .empty;
    defer sql.deinit(allocator);

    // Add schema version
    const version_sql = try generateSchemaVersionSql(allocator, schema.version, schema.database_name);
    defer allocator.free(version_sql);
    try sql.appendSlice(allocator, version_sql);
    try sql.appendSlice(allocator, "\n");

    // Add table and column metadata for each table
    for (schema.tables) |table| {
        // Table metadata
        const table_sql = try generateTableMetadataSql(allocator, &table, schema.version);
        defer allocator.free(table_sql);
        try sql.appendSlice(allocator, table_sql);
        try sql.appendSlice(allocator, "\n");

        // Column metadata
        for (table.fields, 0..) |field, pos| {
            const col_sql = try generateColumnMetadataSql(allocator, table.name, &field, pos);
            defer allocator.free(col_sql);
            try sql.appendSlice(allocator, col_sql);
            try sql.appendSlice(allocator, "\n");
        }
    }

    return try allocator.dupe(u8, sql.items);
}

/// Query to get current schema version
pub const GetSchemaVersionSql = "SELECT MAX(version) FROM _cot_schema;";

/// Query to get table metadata
pub const GetTableMetadataSql = "SELECT * FROM _cot_tables WHERE name = ?;";

/// Query to get all columns for a table
pub const GetTableColumnsSql = "SELECT * FROM _cot_columns WHERE table_name = ? ORDER BY position;";

/// Query to check if a table exists in metadata
pub const TableExistsSql = "SELECT 1 FROM _cot_tables WHERE name = ? LIMIT 1;";

// ============================================================================
// Tests
// ============================================================================

test "create metadata tables sql" {
    const statements = getCreateMetadataTablesSql();
    try std.testing.expectEqual(@as(usize, 4), statements.len);

    // Verify each statement exists and is non-empty
    for (statements) |stmt| {
        try std.testing.expect(stmt.len > 0);
        try std.testing.expect(std.mem.indexOf(u8, stmt, "CREATE") != null);
    }
}

test "generate schema version sql" {
    const sql = try generateSchemaVersionSql(std.testing.allocator, 1, "initial");
    defer std.testing.allocator.free(sql);

    try std.testing.expect(std.mem.indexOf(u8, sql, "INSERT OR REPLACE INTO _cot_schema") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "1") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "initial") != null);
}

test "generate column metadata sql" {
    const field = FieldSchema{
        .name = "id",
        .data_type = .{ .decimal = .{ .size = 6 } },
        .decorators = .{
            .primary = true,
            .auto = true,
        },
    };

    const sql = try generateColumnMetadataSql(std.testing.allocator, "customer", &field, 0);
    defer std.testing.allocator.free(sql);

    try std.testing.expect(std.mem.indexOf(u8, sql, "INSERT OR REPLACE INTO _cot_columns") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "customer") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "id") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "decimal") != null);
}
