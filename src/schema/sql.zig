//! SQL Dialect Abstraction
//!
//! Provides multi-backend SQL generation for SQLite, Postgres, and Turso.
//! This module centralizes all dialect-specific SQL generation to ensure
//! consistent behavior across different database backends.
//!
//! ## Supported Dialects
//!
//! - **SQLite**: Local file-based database, limited ALTER TABLE
//! - **Postgres**: Full-featured SQL with advanced type system
//! - **Turso**: LibSQL (SQLite-compatible) with edge deployment
//!
//! ## Type Mapping
//!
//! | Cot Type         | SQLite      | Postgres        | Turso      |
//! |------------------|-------------|-----------------|------------|
//! | `dN` (decimal)   | INTEGER     | INTEGER         | INTEGER    |
//! | `dN.P` (implied) | REAL        | NUMERIC(N,P)    | REAL       |
//! | `aN` (alpha)     | TEXT        | VARCHAR(N)      | TEXT       |
//! | `i1/i2` (int)    | INTEGER     | SMALLINT        | INTEGER    |
//! | `i4` (int)       | INTEGER     | INTEGER         | INTEGER    |
//! | `i8` (int)       | INTEGER     | BIGINT          | INTEGER    |
//! | packed           | BLOB        | BYTEA           | BLOB       |
//! | string           | TEXT        | TEXT            | TEXT       |
//! | handle           | INTEGER     | BIGINT          | INTEGER    |

const std = @import("std");
const types = @import("types.zig");

const SchemaDataType = types.SchemaDataType;
const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const Decorators = types.Decorators;
const KeyDefinition = types.KeyDefinition;
const ForeignKey = types.ForeignKey;
const OnDeleteAction = types.OnDeleteAction;
const SqlDialect = types.SqlDialect;

/// SQL generation options
pub const SqlOptions = struct {
    /// Include IF NOT EXISTS clauses
    if_not_exists: bool = true,
    /// Include IF EXISTS clauses for drops
    if_exists: bool = true,
    /// Pretty print with indentation
    pretty: bool = true,
    /// Include comments describing changes
    include_comments: bool = true,
    /// Schema/catalog prefix (e.g., "public." for Postgres)
    schema_prefix: ?[]const u8 = null,
};

/// SQL Builder for a specific dialect
pub const SqlBuilder = struct {
    allocator: std.mem.Allocator,
    dialect: SqlDialect,
    options: SqlOptions,
    buffer: std.ArrayListAligned(u8, null),

    pub fn init(allocator: std.mem.Allocator, dialect: SqlDialect, options: SqlOptions) SqlBuilder {
        return .{
            .allocator = allocator,
            .dialect = dialect,
            .options = options,
            .buffer = .empty,
        };
    }

    pub fn deinit(self: *SqlBuilder) void {
        self.buffer.deinit(self.allocator);
    }

    /// Get the generated SQL
    pub fn toOwnedSlice(self: *SqlBuilder) ![]const u8 {
        return try self.allocator.dupe(u8, self.buffer.items);
    }

    /// Clear the buffer for reuse
    pub fn reset(self: *SqlBuilder) void {
        self.buffer.clearRetainingCapacity();
    }

    // ========================================================================
    // DDL Generation
    // ========================================================================

    /// Generate CREATE TABLE statement
    pub fn createTable(self: *SqlBuilder, table: *const TableSchema) !void {
        if (self.options.include_comments) {
            const comment = try std.fmt.allocPrint(self.allocator, "-- Table: {s}\n", .{table.name});
            defer self.allocator.free(comment);
            try self.buffer.appendSlice(self.allocator, comment);
        }

        try self.buffer.appendSlice(self.allocator, "CREATE TABLE ");
        if (self.options.if_not_exists) {
            try self.buffer.appendSlice(self.allocator, "IF NOT EXISTS ");
        }
        if (self.options.schema_prefix) |prefix| {
            try self.buffer.appendSlice(self.allocator, prefix);
        }
        try self.buffer.appendSlice(self.allocator, table.name);
        try self.buffer.appendSlice(self.allocator, " (\n");

        // Generate column definitions
        for (table.fields, 0..) |field, i| {
            if (i > 0) try self.buffer.appendSlice(self.allocator, ",\n");
            try self.buffer.appendSlice(self.allocator, "    ");
            try self.columnDefinition(&field);
        }

        // Generate table-level constraints for Postgres
        if (self.dialect == .postgres) {
            try self.tableConstraints(table);
        }

        try self.buffer.appendSlice(self.allocator, "\n);\n");

        // Generate indexes for indexed fields
        try self.fieldIndexes(table);
    }

    /// Generate DROP TABLE statement
    pub fn dropTable(self: *SqlBuilder, table_name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, "DROP TABLE ");
        if (self.options.if_exists) {
            try self.buffer.appendSlice(self.allocator, "IF EXISTS ");
        }
        if (self.options.schema_prefix) |prefix| {
            try self.buffer.appendSlice(self.allocator, prefix);
        }
        try self.buffer.appendSlice(self.allocator, table_name);
        try self.buffer.appendSlice(self.allocator, ";\n");
    }

    /// Generate ALTER TABLE ADD COLUMN
    pub fn addColumn(self: *SqlBuilder, table_name: []const u8, field: *const FieldSchema) !void {
        try self.buffer.appendSlice(self.allocator, "ALTER TABLE ");
        if (self.options.schema_prefix) |prefix| {
            try self.buffer.appendSlice(self.allocator, prefix);
        }
        try self.buffer.appendSlice(self.allocator, table_name);
        try self.buffer.appendSlice(self.allocator, " ADD COLUMN ");
        try self.columnDefinition(field);
        try self.buffer.appendSlice(self.allocator, ";\n");
    }

    /// Generate ALTER TABLE DROP COLUMN
    pub fn dropColumn(self: *SqlBuilder, table_name: []const u8, column_name: []const u8) !void {
        switch (self.dialect) {
            .sqlite, .turso => {
                if (self.options.include_comments) {
                    try self.buffer.appendSlice(self.allocator, "-- Note: SQLite requires 3.35.0+ for DROP COLUMN\n");
                }
            },
            .postgres => {},
        }
        try self.buffer.appendSlice(self.allocator, "ALTER TABLE ");
        if (self.options.schema_prefix) |prefix| {
            try self.buffer.appendSlice(self.allocator, prefix);
        }
        try self.buffer.appendSlice(self.allocator, table_name);
        try self.buffer.appendSlice(self.allocator, " DROP COLUMN ");
        try self.buffer.appendSlice(self.allocator, column_name);
        try self.buffer.appendSlice(self.allocator, ";\n");
    }

    /// Generate ALTER TABLE MODIFY COLUMN (type change)
    pub fn alterColumnType(self: *SqlBuilder, table_name: []const u8, field: *const FieldSchema) !void {
        switch (self.dialect) {
            .sqlite, .turso => {
                if (self.options.include_comments) {
                    try self.buffer.appendSlice(self.allocator, "-- WARNING: SQLite does not support ALTER COLUMN TYPE\n");
                    try self.buffer.appendSlice(self.allocator, "-- Table recreation required. See: https://sqlite.org/lang_altertable.html\n");
                }
            },
            .postgres => {
                try self.buffer.appendSlice(self.allocator, "ALTER TABLE ");
                if (self.options.schema_prefix) |prefix| {
                    try self.buffer.appendSlice(self.allocator, prefix);
                }
                try self.buffer.appendSlice(self.allocator, table_name);
                try self.buffer.appendSlice(self.allocator, " ALTER COLUMN ");
                try self.buffer.appendSlice(self.allocator, field.name);
                try self.buffer.appendSlice(self.allocator, " TYPE ");
                try self.dataType(field.data_type);

                // Add USING clause for safe casting
                try self.buffer.appendSlice(self.allocator, " USING ");
                try self.buffer.appendSlice(self.allocator, field.name);
                try self.buffer.appendSlice(self.allocator, "::");
                try self.dataType(field.data_type);
                try self.buffer.appendSlice(self.allocator, ";\n");
            },
        }
    }

    /// Generate ALTER TABLE SET/DROP NOT NULL
    pub fn alterColumnNullable(self: *SqlBuilder, table_name: []const u8, column_name: []const u8, not_null: bool) !void {
        switch (self.dialect) {
            .sqlite, .turso => {
                if (self.options.include_comments) {
                    try self.buffer.appendSlice(self.allocator, "-- WARNING: SQLite does not support ALTER COLUMN constraints\n");
                }
            },
            .postgres => {
                try self.buffer.appendSlice(self.allocator, "ALTER TABLE ");
                if (self.options.schema_prefix) |prefix| {
                    try self.buffer.appendSlice(self.allocator, prefix);
                }
                try self.buffer.appendSlice(self.allocator, table_name);
                try self.buffer.appendSlice(self.allocator, " ALTER COLUMN ");
                try self.buffer.appendSlice(self.allocator, column_name);
                if (not_null) {
                    try self.buffer.appendSlice(self.allocator, " SET NOT NULL;\n");
                } else {
                    try self.buffer.appendSlice(self.allocator, " DROP NOT NULL;\n");
                }
            },
        }
    }

    /// Generate ALTER TABLE SET/DROP DEFAULT
    pub fn alterColumnDefault(self: *SqlBuilder, table_name: []const u8, column_name: []const u8, default_value: ?[]const u8, data_type: SchemaDataType) !void {
        switch (self.dialect) {
            .sqlite, .turso => {
                if (self.options.include_comments) {
                    try self.buffer.appendSlice(self.allocator, "-- WARNING: SQLite does not support ALTER COLUMN DEFAULT\n");
                }
            },
            .postgres => {
                try self.buffer.appendSlice(self.allocator, "ALTER TABLE ");
                if (self.options.schema_prefix) |prefix| {
                    try self.buffer.appendSlice(self.allocator, prefix);
                }
                try self.buffer.appendSlice(self.allocator, table_name);
                try self.buffer.appendSlice(self.allocator, " ALTER COLUMN ");
                try self.buffer.appendSlice(self.allocator, column_name);

                if (default_value) |val| {
                    try self.buffer.appendSlice(self.allocator, " SET DEFAULT ");
                    try self.defaultValue(val, data_type);
                } else {
                    try self.buffer.appendSlice(self.allocator, " DROP DEFAULT");
                }
                try self.buffer.appendSlice(self.allocator, ";\n");
            },
        }
    }

    /// Generate CREATE INDEX
    pub fn createIndex(self: *SqlBuilder, table_name: []const u8, index_name: []const u8, columns: []const []const u8, unique: bool) !void {
        if (unique) {
            try self.buffer.appendSlice(self.allocator, "CREATE UNIQUE INDEX ");
        } else {
            try self.buffer.appendSlice(self.allocator, "CREATE INDEX ");
        }
        if (self.options.if_not_exists) {
            try self.buffer.appendSlice(self.allocator, "IF NOT EXISTS ");
        }
        try self.buffer.appendSlice(self.allocator, index_name);
        try self.buffer.appendSlice(self.allocator, " ON ");
        if (self.options.schema_prefix) |prefix| {
            try self.buffer.appendSlice(self.allocator, prefix);
        }
        try self.buffer.appendSlice(self.allocator, table_name);
        try self.buffer.appendSlice(self.allocator, "(");

        for (columns, 0..) |col, i| {
            if (i > 0) try self.buffer.appendSlice(self.allocator, ", ");
            try self.buffer.appendSlice(self.allocator, col);
        }

        try self.buffer.appendSlice(self.allocator, ");\n");
    }

    /// Generate DROP INDEX
    pub fn dropIndex(self: *SqlBuilder, index_name: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, "DROP INDEX ");
        if (self.options.if_exists) {
            try self.buffer.appendSlice(self.allocator, "IF EXISTS ");
        }
        try self.buffer.appendSlice(self.allocator, index_name);
        try self.buffer.appendSlice(self.allocator, ";\n");
    }

    /// Generate composite key index from KeyDefinition
    pub fn createKeyIndex(self: *SqlBuilder, key: *const KeyDefinition) !void {
        if (key.segments.len == 0) return;
        if (key.primary) return; // Primary keys handled in table definition

        const index_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ key.table_name, key.name });
        defer self.allocator.free(index_name);

        try self.createIndex(key.table_name, index_name, key.segments, key.unique);
    }

    // ========================================================================
    // Transaction Helpers
    // ========================================================================

    /// Generate BEGIN TRANSACTION
    pub fn beginTransaction(self: *SqlBuilder) !void {
        switch (self.dialect) {
            .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "BEGIN TRANSACTION;\n"),
            .postgres => try self.buffer.appendSlice(self.allocator, "BEGIN;\n"),
        }
    }

    /// Generate COMMIT
    pub fn commit(self: *SqlBuilder) !void {
        try self.buffer.appendSlice(self.allocator, "COMMIT;\n");
    }

    /// Generate ROLLBACK
    pub fn rollback(self: *SqlBuilder) !void {
        try self.buffer.appendSlice(self.allocator, "ROLLBACK;\n");
    }

    // ========================================================================
    // Internal Helpers
    // ========================================================================

    /// Generate column definition
    fn columnDefinition(self: *SqlBuilder, field: *const FieldSchema) !void {
        try self.buffer.appendSlice(self.allocator, field.name);
        try self.buffer.appendSlice(self.allocator, " ");
        try self.dataType(field.data_type);

        // Handle auto-increment primary key
        if (field.decorators.primary and field.decorators.auto) {
            switch (self.dialect) {
                .sqlite, .turso => {
                    // SQLite: INTEGER PRIMARY KEY is auto-increment
                    try self.buffer.appendSlice(self.allocator, " PRIMARY KEY");
                },
                .postgres => {
                    // Postgres: Use GENERATED ALWAYS AS IDENTITY
                    try self.buffer.appendSlice(self.allocator, " GENERATED ALWAYS AS IDENTITY PRIMARY KEY");
                },
            }
            return;
        }

        // Primary key (non-auto)
        if (field.decorators.primary) {
            try self.buffer.appendSlice(self.allocator, " PRIMARY KEY");
        }

        // NOT NULL (implicit for primary keys)
        if (field.decorators.not_null and !field.decorators.primary) {
            try self.buffer.appendSlice(self.allocator, " NOT NULL");
        }

        // UNIQUE (implicit for primary keys)
        if (field.decorators.unique and !field.decorators.primary) {
            try self.buffer.appendSlice(self.allocator, " UNIQUE");
        }

        // DEFAULT value
        if (field.decorators.default_value) |default| {
            try self.buffer.appendSlice(self.allocator, " DEFAULT ");
            try self.defaultValue(default, field.data_type);
        }

        // Foreign key reference (inline for SQLite, table-level for Postgres)
        if (self.dialect != .postgres) {
            if (field.decorators.foreign_key) |fk| {
                try self.buffer.appendSlice(self.allocator, " REFERENCES ");
                try self.buffer.appendSlice(self.allocator, fk.table);
                try self.buffer.appendSlice(self.allocator, "(");
                try self.buffer.appendSlice(self.allocator, fk.column);
                try self.buffer.appendSlice(self.allocator, ")");

                if (field.decorators.on_delete) |action| {
                    try self.buffer.appendSlice(self.allocator, " ON DELETE ");
                    try self.onDeleteAction(action);
                }
            }
        }
    }

    /// Generate SQL data type
    fn dataType(self: *SqlBuilder, dt: SchemaDataType) !void {
        switch (dt) {
            .text => |t| {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "TEXT"),
                    .postgres => {
                        const size = t.max_length orelse 255;
                        const type_str = try std.fmt.allocPrint(self.allocator, "VARCHAR({d})", .{size});
                        defer self.allocator.free(type_str);
                        try self.buffer.appendSlice(self.allocator, type_str);
                    },
                }
            },
            .integer => |i| {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "INTEGER"),
                    .postgres => {
                        switch (i) {
                            .tiny, .small => try self.buffer.appendSlice(self.allocator, "SMALLINT"),
                            .medium => try self.buffer.appendSlice(self.allocator, "INTEGER"),
                            .big => try self.buffer.appendSlice(self.allocator, "BIGINT"),
                        }
                    },
                }
            },
            .decimal => |d| {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "REAL"),
                    .postgres => {
                        const type_str = try std.fmt.allocPrint(self.allocator, "NUMERIC({d},{d})", .{ d.precision, d.scale });
                        defer self.allocator.free(type_str);
                        try self.buffer.appendSlice(self.allocator, type_str);
                    },
                }
            },
            .boolean => {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "INTEGER"),
                    .postgres => try self.buffer.appendSlice(self.allocator, "BOOLEAN"),
                }
            },
            .blob => {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "BLOB"),
                    .postgres => try self.buffer.appendSlice(self.allocator, "BYTEA"),
                }
            },
            .datetime => |dt_type| {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, switch (dt_type) {
                        .date => "TEXT",
                        .time => "TEXT",
                        .datetime => "TEXT",
                        .timestamp => "INTEGER",
                    }),
                    .postgres => try self.buffer.appendSlice(self.allocator, switch (dt_type) {
                        .date => "DATE",
                        .time => "TIME",
                        .datetime => "TIMESTAMP",
                        .timestamp => "BIGINT",
                    }),
                }
            },
            .ulid => {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "TEXT"),
                    .postgres => try self.buffer.appendSlice(self.allocator, "CHAR(26)"),
                }
            },
            .reference => {
                switch (self.dialect) {
                    .sqlite, .turso => try self.buffer.appendSlice(self.allocator, "TEXT"),
                    .postgres => try self.buffer.appendSlice(self.allocator, "CHAR(26)"),
                }
            },
        }
    }

    /// Generate default value with proper quoting
    fn defaultValue(self: *SqlBuilder, value: []const u8, dt: SchemaDataType) !void {
        switch (dt) {
            .text, .ulid, .reference => {
                // Quote string values
                try self.buffer.append(self.allocator, '\'');
                // Escape single quotes
                for (value) |c| {
                    if (c == '\'') {
                        try self.buffer.appendSlice(self.allocator, "''");
                    } else {
                        try self.buffer.append(self.allocator, c);
                    }
                }
                try self.buffer.append(self.allocator, '\'');
            },
            else => {
                // Numeric and boolean values don't need quoting
                try self.buffer.appendSlice(self.allocator, value);
            },
        }
    }

    /// Generate ON DELETE action
    fn onDeleteAction(self: *SqlBuilder, action: OnDeleteAction) !void {
        switch (action) {
            .cascade => try self.buffer.appendSlice(self.allocator, "CASCADE"),
            .set_null => try self.buffer.appendSlice(self.allocator, "SET NULL"),
            .restrict => try self.buffer.appendSlice(self.allocator, "RESTRICT"),
        }
    }

    /// Generate table-level constraints (Postgres style)
    fn tableConstraints(self: *SqlBuilder, table: *const TableSchema) !void {
        for (table.fields) |field| {
            if (field.decorators.foreign_key) |fk| {
                try self.buffer.appendSlice(self.allocator, ",\n    CONSTRAINT fk_");
                try self.buffer.appendSlice(self.allocator, table.name);
                try self.buffer.appendSlice(self.allocator, "_");
                try self.buffer.appendSlice(self.allocator, field.name);
                try self.buffer.appendSlice(self.allocator, " FOREIGN KEY (");
                try self.buffer.appendSlice(self.allocator, field.name);
                try self.buffer.appendSlice(self.allocator, ") REFERENCES ");
                try self.buffer.appendSlice(self.allocator, fk.table);
                try self.buffer.appendSlice(self.allocator, "(");
                try self.buffer.appendSlice(self.allocator, fk.column);
                try self.buffer.appendSlice(self.allocator, ")");

                if (field.decorators.on_delete) |action| {
                    try self.buffer.appendSlice(self.allocator, " ON DELETE ");
                    try self.onDeleteAction(action);
                }
            }
        }
    }

    /// Generate indexes for fields with @index decorator
    fn fieldIndexes(self: *SqlBuilder, table: *const TableSchema) !void {
        for (table.fields) |field| {
            // Skip if primary or unique (already indexed)
            if (field.decorators.primary or field.decorators.unique) continue;

            if (field.decorators.index) {
                const index_name = try std.fmt.allocPrint(self.allocator, "idx_{s}_{s}", .{ table.name, field.name });
                defer self.allocator.free(index_name);

                try self.createIndex(table.name, index_name, &[_][]const u8{field.name}, false);
            }
        }
    }
};

// ============================================================================
// Convenience Functions
// ============================================================================

/// Generate CREATE TABLE SQL for a table
pub fn generateCreateTable(
    allocator: std.mem.Allocator,
    table: *const TableSchema,
    dialect: SqlDialect,
) ![]const u8 {
    var builder = SqlBuilder.init(allocator, dialect, .{});
    defer builder.deinit();

    try builder.createTable(table);
    return builder.toOwnedSlice();
}

/// Generate DROP TABLE SQL
pub fn generateDropTable(
    allocator: std.mem.Allocator,
    table_name: []const u8,
    dialect: SqlDialect,
) ![]const u8 {
    var builder = SqlBuilder.init(allocator, dialect, .{});
    defer builder.deinit();

    try builder.dropTable(table_name);
    return builder.toOwnedSlice();
}

/// Generate all CREATE TABLE statements for a schema
pub fn generateSchema(
    allocator: std.mem.Allocator,
    tables: []const TableSchema,
    keys: []const KeyDefinition,
    dialect: SqlDialect,
) ![]const u8 {
    var builder = SqlBuilder.init(allocator, dialect, .{});
    defer builder.deinit();

    // Create tables
    for (tables) |*table| {
        try builder.createTable(table);
        try builder.buffer.appendSlice(allocator, "\n");
    }

    // Create composite key indexes
    for (keys) |*key| {
        try builder.createKeyIndex(key);
    }

    return builder.toOwnedSlice();
}

/// Check if a dialect supports a feature
pub fn dialectSupports(dialect: SqlDialect, feature: DialectFeature) bool {
    return switch (feature) {
        .alter_column_type => dialect == .postgres,
        .alter_column_nullable => dialect == .postgres,
        .alter_column_default => dialect == .postgres,
        .drop_column => true, // SQLite 3.35.0+ supports this
        .json_type => dialect == .postgres,
        .array_type => dialect == .postgres,
        .generated_columns => true,
        .partial_indexes => dialect == .postgres,
        .concurrent_index => dialect == .postgres,
    };
}

/// Dialect-specific features
pub const DialectFeature = enum {
    alter_column_type,
    alter_column_nullable,
    alter_column_default,
    drop_column,
    json_type,
    array_type,
    generated_columns,
    partial_indexes,
    concurrent_index,
};

/// Get the timestamp type for a dialect
pub fn timestampType(dialect: SqlDialect) []const u8 {
    return switch (dialect) {
        .sqlite, .turso => "INTEGER", // Unix timestamp
        .postgres => "TIMESTAMP WITH TIME ZONE",
    };
}

/// Get the boolean type for a dialect
pub fn booleanType(dialect: SqlDialect) []const u8 {
    return switch (dialect) {
        .sqlite, .turso => "INTEGER", // 0/1
        .postgres => "BOOLEAN",
    };
}

/// Get the JSON type for a dialect
pub fn jsonType(dialect: SqlDialect) []const u8 {
    return switch (dialect) {
        .sqlite, .turso => "TEXT",
        .postgres => "JSONB",
    };
}

/// Get the current timestamp expression for a dialect
pub fn currentTimestamp(dialect: SqlDialect) []const u8 {
    return switch (dialect) {
        .sqlite, .turso => "strftime('%s', 'now')",
        .postgres => "CURRENT_TIMESTAMP",
    };
}

/// Get the auto-increment type for a dialect
pub fn autoIncrementType(dialect: SqlDialect, size: ?usize) []const u8 {
    return switch (dialect) {
        .sqlite, .turso => "INTEGER",
        .postgres => if (size) |s| (if (s > 9) "BIGSERIAL" else "SERIAL") else "SERIAL",
    };
}

// ============================================================================
// Tests
// ============================================================================

test "SqlBuilder create table SQLite" {
    const allocator = std.testing.allocator;

    var builder = SqlBuilder.init(allocator, .sqlite, .{ .include_comments = false });
    defer builder.deinit();

    const fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true, .auto = true },
        },
        .{
            .name = "name",
            .data_type = .{ .alpha = .{ .size = 30 } },
            .decorators = .{ .not_null = true },
        },
        .{
            .name = "email",
            .data_type = .{ .alpha = .{ .size = 50 } },
            .decorators = .{ .unique = true, .index = true },
        },
    };

    const table = TableSchema{
        .name = "customer",
        .fields = @constCast(&fields),
    };

    try builder.createTable(&table);
    const sql = builder.buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE TABLE IF NOT EXISTS customer") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "id INTEGER PRIMARY KEY") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "name TEXT NOT NULL") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "email TEXT UNIQUE") != null);
}

test "SqlBuilder create table Postgres" {
    const allocator = std.testing.allocator;

    var builder = SqlBuilder.init(allocator, .postgres, .{ .include_comments = false });
    defer builder.deinit();

    const fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true, .auto = true },
        },
        .{
            .name = "name",
            .data_type = .{ .alpha = .{ .size = 30 } },
            .decorators = .{ .not_null = true },
        },
        .{
            .name = "balance",
            .data_type = .{ .implied_decimal = .{ .total_digits = 10, .precision = 2 } },
            .decorators = .{ .default_value = "0" },
        },
    };

    const table = TableSchema{
        .name = "customer",
        .fields = @constCast(&fields),
    };

    try builder.createTable(&table);
    const sql = builder.buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE TABLE IF NOT EXISTS customer") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "GENERATED ALWAYS AS IDENTITY PRIMARY KEY") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "name VARCHAR(30) NOT NULL") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "balance NUMERIC(10,2) DEFAULT 0") != null);
}

test "SqlBuilder foreign key" {
    const allocator = std.testing.allocator;

    var builder = SqlBuilder.init(allocator, .sqlite, .{ .include_comments = false });
    defer builder.deinit();

    const fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 8 } },
            .decorators = .{ .primary = true },
        },
        .{
            .name = "customer_id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{
                .foreign_key = .{ .table = "customer", .column = "id" },
                .on_delete = .cascade,
            },
        },
    };

    const table = TableSchema{
        .name = "order",
        .fields = @constCast(&fields),
    };

    try builder.createTable(&table);
    const sql = builder.buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, sql, "REFERENCES customer(id)") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "ON DELETE CASCADE") != null);
}

test "SqlBuilder alter column" {
    const allocator = std.testing.allocator;

    var builder = SqlBuilder.init(allocator, .postgres, .{});
    defer builder.deinit();

    const field = FieldSchema{
        .name = "name",
        .data_type = .{ .alpha = .{ .size = 50 } },
        .decorators = .{},
    };

    try builder.alterColumnType("customer", &field);
    const sql = builder.buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, sql, "ALTER TABLE customer ALTER COLUMN name TYPE VARCHAR(50)") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "USING name::VARCHAR(50)") != null);
}

test "SqlBuilder create index" {
    const allocator = std.testing.allocator;

    var builder = SqlBuilder.init(allocator, .sqlite, .{});
    defer builder.deinit();

    try builder.createIndex("customer", "idx_customer_email", &[_][]const u8{"email"}, true);
    const sql = builder.buffer.items;

    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE UNIQUE INDEX IF NOT EXISTS idx_customer_email ON customer(email)") != null);
}

test "generateSchema multiple tables" {
    const allocator = std.testing.allocator;

    const customer_fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true },
        },
    };

    const order_fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 8 } },
            .decorators = .{ .primary = true },
        },
        .{
            .name = "customer_id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .index = true },
        },
    };

    const tables = [_]TableSchema{
        .{ .name = "customer", .fields = @constCast(&customer_fields) },
        .{ .name = "order", .fields = @constCast(&order_fields) },
    };

    const sql = try generateSchema(allocator, &tables, &[_]KeyDefinition{}, .sqlite);
    defer allocator.free(sql);

    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE TABLE IF NOT EXISTS customer") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE TABLE IF NOT EXISTS order") != null);
    try std.testing.expect(std.mem.indexOf(u8, sql, "CREATE INDEX IF NOT EXISTS idx_order_customer_id") != null);
}

test "dialect features" {
    try std.testing.expect(dialectSupports(.postgres, .alter_column_type));
    try std.testing.expect(!dialectSupports(.sqlite, .alter_column_type));
    try std.testing.expect(dialectSupports(.sqlite, .drop_column));
}

test "dialect helper functions" {
    try std.testing.expectEqualStrings("INTEGER", timestampType(.sqlite));
    try std.testing.expectEqualStrings("TIMESTAMP WITH TIME ZONE", timestampType(.postgres));

    try std.testing.expectEqualStrings("INTEGER", booleanType(.sqlite));
    try std.testing.expectEqualStrings("BOOLEAN", booleanType(.postgres));

    try std.testing.expectEqualStrings("TEXT", jsonType(.sqlite));
    try std.testing.expectEqualStrings("JSONB", jsonType(.postgres));
}
