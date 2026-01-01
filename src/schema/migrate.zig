//! Schema Migration Generator
//!
//! Generates SQL migration scripts from schema diffs.
//! Supports multiple database backends (SQLite, Postgres, Turso).
//!
//! Migration files are stored in the migrations/ directory and include:
//! - Up migration (apply changes)
//! - Down migration (rollback changes)

const std = @import("std");
const types = @import("types.zig");
const diff_mod = @import("diff.zig");

const SchemaFile = types.SchemaFile;
const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const KeyDefinition = types.KeyDefinition;
const SqlDialect = types.SqlDialect;
const SchemaDiff = diff_mod.SchemaDiff;
const TableDiff = diff_mod.TableDiff;
const FieldDiff = diff_mod.FieldDiff;
const ChangeType = diff_mod.ChangeType;

/// A complete migration with up and down scripts
pub const Migration = struct {
    /// Migration version number
    version: u32,
    /// Unix timestamp when generated
    timestamp: i64,
    /// Human-readable description
    description: []const u8,
    /// SQL to apply changes
    up_sql: []const u8,
    /// SQL to rollback changes
    down_sql: []const u8,
    /// Which dialect this was generated for
    dialect: SqlDialect,

    allocator: std.mem.Allocator,

    pub fn deinit(self: *Migration) void {
        self.allocator.free(self.description);
        self.allocator.free(self.up_sql);
        self.allocator.free(self.down_sql);
    }
};

/// Generate a migration from a schema diff
pub fn generateMigration(
    allocator: std.mem.Allocator,
    schema_diff: *const SchemaDiff,
    old_schema: *const SchemaFile,
    new_schema: *const SchemaFile,
    dialect: SqlDialect,
) !Migration {
    const timestamp = std.time.timestamp();

    // Generate description
    var desc_parts: std.ArrayListAligned(u8, null) = .empty;
    defer desc_parts.deinit(allocator);

    if (schema_diff.added_tables.len > 0) {
        try desc_parts.appendSlice(allocator, "add_");
        for (schema_diff.added_tables, 0..) |table, i| {
            if (i > 0) try desc_parts.append(allocator, '_');
            try desc_parts.appendSlice(allocator, table.name);
        }
    }
    if (schema_diff.removed_tables.len > 0) {
        if (desc_parts.items.len > 0) try desc_parts.append(allocator, '_');
        try desc_parts.appendSlice(allocator, "remove_");
        for (schema_diff.removed_tables, 0..) |name, i| {
            if (i > 0) try desc_parts.append(allocator, '_');
            try desc_parts.appendSlice(allocator, name);
        }
    }
    if (schema_diff.modified_tables.len > 0) {
        if (desc_parts.items.len > 0) try desc_parts.append(allocator, '_');
        try desc_parts.appendSlice(allocator, "modify_");
        for (schema_diff.modified_tables, 0..) |table, i| {
            if (i > 0) try desc_parts.append(allocator, '_');
            try desc_parts.appendSlice(allocator, table.name);
        }
    }

    if (desc_parts.items.len == 0) {
        try desc_parts.appendSlice(allocator, "no_changes");
    }

    const description = try allocator.dupe(u8, desc_parts.items);
    errdefer allocator.free(description);

    // Generate UP SQL
    var up_sql: std.ArrayListAligned(u8, null) = .empty;
    errdefer up_sql.deinit(allocator);

    try generateUpMigration(allocator, &up_sql, schema_diff, old_schema, new_schema, dialect);

    const up_sql_owned = try allocator.dupe(u8, up_sql.items);
    errdefer allocator.free(up_sql_owned);

    // Generate DOWN SQL
    var down_sql: std.ArrayListAligned(u8, null) = .empty;
    errdefer down_sql.deinit(allocator);

    try generateDownMigration(allocator, &down_sql, schema_diff, old_schema, new_schema, dialect);

    const down_sql_owned = try allocator.dupe(u8, down_sql.items);

    return Migration{
        .version = new_schema.version,
        .timestamp = timestamp,
        .description = description,
        .up_sql = up_sql_owned,
        .down_sql = down_sql_owned,
        .dialect = dialect,
        .allocator = allocator,
    };
}

/// Generate UP migration SQL
fn generateUpMigration(
    allocator: std.mem.Allocator,
    out: *std.ArrayListAligned(u8, null),
    schema_diff: *const SchemaDiff,
    old_schema: *const SchemaFile,
    new_schema: *const SchemaFile,
    dialect: SqlDialect,
) !void {
    _ = old_schema;

    // Create new tables
    for (schema_diff.added_tables) |table| {
        try generateCreateTable(allocator, out, &table, dialect);
        try out.appendSlice(allocator, "\n");
    }

    // Modify existing tables
    for (schema_diff.modified_tables) |table_diff| {
        try generateTableAlterations(allocator, out, &table_diff, new_schema, dialect);
    }

    // Drop removed tables
    for (schema_diff.removed_tables) |table_name| {
        const line = try std.fmt.allocPrint(allocator, "DROP TABLE IF EXISTS {s};\n", .{table_name});
        defer allocator.free(line);
        try out.appendSlice(allocator, line);
    }

    // Add new composite keys
    for (schema_diff.added_keys) |key| {
        try generateCreateIndex(allocator, out, &key, dialect);
    }

    // Drop removed composite keys
    for (schema_diff.removed_keys) |key_id| {
        // key_id is "table.keyname" format
        const line = try std.fmt.allocPrint(allocator, "DROP INDEX IF EXISTS {s};\n", .{key_id});
        defer allocator.free(line);
        try out.appendSlice(allocator, line);
    }
}

/// Generate DOWN migration SQL (rollback)
fn generateDownMigration(
    allocator: std.mem.Allocator,
    out: *std.ArrayListAligned(u8, null),
    schema_diff: *const SchemaDiff,
    old_schema: *const SchemaFile,
    new_schema: *const SchemaFile,
    dialect: SqlDialect,
) !void {
    _ = new_schema;

    // Recreate dropped tables
    for (schema_diff.removed_tables) |table_name| {
        // Find the table in old schema
        for (old_schema.tables) |table| {
            if (std.mem.eql(u8, table.name, table_name)) {
                try generateCreateTable(allocator, out, &table, dialect);
                try out.appendSlice(allocator, "\n");
                break;
            }
        }
    }

    // Reverse table modifications
    for (schema_diff.modified_tables) |table_diff| {
        try generateTableAlterationsReverse(allocator, out, &table_diff, old_schema, dialect);
    }

    // Drop added tables
    for (schema_diff.added_tables) |table| {
        const line = try std.fmt.allocPrint(allocator, "DROP TABLE IF EXISTS {s};\n", .{table.name});
        defer allocator.free(line);
        try out.appendSlice(allocator, line);
    }

    // Recreate removed keys
    for (schema_diff.removed_keys) |key_id| {
        // Find the key in old schema
        for (old_schema.keys) |key| {
            const full_name = std.fmt.allocPrint(allocator, "{s}.{s}", .{ key.table_name, key.name }) catch continue;
            defer allocator.free(full_name);
            if (std.mem.eql(u8, full_name, key_id)) {
                try generateCreateIndex(allocator, out, &key, dialect);
                break;
            }
        }
    }

    // Drop added keys
    for (schema_diff.added_keys) |key| {
        const line = try std.fmt.allocPrint(allocator, "DROP INDEX IF EXISTS {s}_{s};\n", .{ key.table_name, key.name });
        defer allocator.free(line);
        try out.appendSlice(allocator, line);
    }
}

/// Generate CREATE TABLE statement
fn generateCreateTable(allocator: std.mem.Allocator, out: *std.ArrayListAligned(u8, null), table: *const TableSchema, dialect: SqlDialect) !void {
    const header = try std.fmt.allocPrint(allocator, "CREATE TABLE IF NOT EXISTS {s} (\n", .{table.name});
    defer allocator.free(header);
    try out.appendSlice(allocator, header);

    for (table.fields, 0..) |field, i| {
        if (i > 0) try out.appendSlice(allocator, ",\n");
        try out.appendSlice(allocator, "    ");
        try generateColumnDef(allocator, out, &field, dialect);
    }

    try out.appendSlice(allocator, "\n);\n");

    // Generate indexes for indexed fields
    for (table.fields) |field| {
        if (field.decorators.index and !field.decorators.primary and !field.decorators.unique) {
            const idx = try std.fmt.allocPrint(allocator, "CREATE INDEX IF NOT EXISTS idx_{s}_{s} ON {s}({s});\n", .{
                table.name,
                field.name,
                table.name,
                field.name,
            });
            defer allocator.free(idx);
            try out.appendSlice(allocator, idx);
        }
    }
}

/// Generate column definition
fn generateColumnDef(allocator: std.mem.Allocator, out: *std.ArrayListAligned(u8, null), field: *const FieldSchema, dialect: SqlDialect) !void {
    const sql_type = types.dataTypeToSql(field.data_type, dialect);

    // Add size for VARCHAR in postgres
    if (dialect == .postgres and field.data_type == .alpha) {
        const size = field.data_type.alpha.size orelse 255;
        const col = try std.fmt.allocPrint(allocator, "{s} {s}({d})", .{ field.name, sql_type, size });
        defer allocator.free(col);
        try out.appendSlice(allocator, col);
    } else {
        const col = try std.fmt.allocPrint(allocator, "{s} {s}", .{ field.name, sql_type });
        defer allocator.free(col);
        try out.appendSlice(allocator, col);
    }

    // Add constraints
    if (field.decorators.primary) {
        try out.appendSlice(allocator, " PRIMARY KEY");
    }

    if (field.decorators.not_null and !field.decorators.primary) {
        try out.appendSlice(allocator, " NOT NULL");
    }

    if (field.decorators.unique and !field.decorators.primary) {
        try out.appendSlice(allocator, " UNIQUE");
    }

    if (field.decorators.default_value) |default| {
        switch (field.data_type) {
            .alpha, .string => {
                const def = try std.fmt.allocPrint(allocator, " DEFAULT '{s}'", .{default});
                defer allocator.free(def);
                try out.appendSlice(allocator, def);
            },
            else => {
                const def = try std.fmt.allocPrint(allocator, " DEFAULT {s}", .{default});
                defer allocator.free(def);
                try out.appendSlice(allocator, def);
            },
        }
    }

    // Foreign key reference
    if (field.decorators.foreign_key) |fk| {
        const fk_ref = try std.fmt.allocPrint(allocator, " REFERENCES {s}({s})", .{ fk.table, fk.column });
        defer allocator.free(fk_ref);
        try out.appendSlice(allocator, fk_ref);
        if (field.decorators.on_delete) |action| {
            const on_del = try std.fmt.allocPrint(allocator, " ON DELETE {s}", .{switch (action) {
                .cascade => "CASCADE",
                .set_null => "SET NULL",
                .restrict => "RESTRICT",
            }});
            defer allocator.free(on_del);
            try out.appendSlice(allocator, on_del);
        }
    }
}

/// Generate table alterations (add/modify/drop columns)
fn generateTableAlterations(
    allocator: std.mem.Allocator,
    out: *std.ArrayListAligned(u8, null),
    table_diff: *const TableDiff,
    new_schema: *const SchemaFile,
    dialect: SqlDialect,
) !void {
    _ = new_schema; // May be used for future enhancements

    // Add new columns
    for (table_diff.added_fields) |field| {
        const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ADD COLUMN ", .{table_diff.name});
        defer allocator.free(alter);
        try out.appendSlice(allocator, alter);
        try generateColumnDef(allocator, out, &field, dialect);
        try out.appendSlice(allocator, ";\n");
    }

    // Modify columns (database-specific)
    for (table_diff.modified_fields) |field_diff| {
        try generateColumnModification(allocator, out, table_diff.name, &field_diff, dialect);
    }

    // Drop columns
    for (table_diff.removed_fields) |field_name| {
        switch (dialect) {
            .sqlite, .turso => {
                // SQLite doesn't support DROP COLUMN before 3.35.0
                // We'll add a comment noting this limitation
                try out.appendSlice(allocator, "-- SQLite: DROP COLUMN requires SQLite 3.35.0+\n");
                const drop = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} DROP COLUMN {s};\n", .{ table_diff.name, field_name });
                defer allocator.free(drop);
                try out.appendSlice(allocator, drop);
            },
            .postgres => {
                const drop = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} DROP COLUMN {s};\n", .{ table_diff.name, field_name });
                defer allocator.free(drop);
                try out.appendSlice(allocator, drop);
            },
        }
    }
}

/// Generate column modification SQL
fn generateColumnModification(
    allocator: std.mem.Allocator,
    out: *std.ArrayListAligned(u8, null),
    table_name: []const u8,
    field_diff: *const FieldDiff,
    dialect: SqlDialect,
) !void {
    switch (dialect) {
        .sqlite, .turso => {
            // SQLite has limited ALTER TABLE support
            // For type changes, we need to recreate the table
            switch (field_diff.change_type) {
                .size_increased, .decorators_changed => {
                    // These might work with ALTER in newer SQLite
                    try out.appendSlice(allocator, "-- Note: SQLite may require table recreation for this change\n");
                    const comment = try std.fmt.allocPrint(allocator, "-- ALTER TABLE {s} MODIFY COLUMN ", .{table_name});
                    defer allocator.free(comment);
                    try out.appendSlice(allocator, comment);
                    try generateColumnDef(allocator, out, &field_diff.new_field, dialect);
                    try out.appendSlice(allocator, ";\n");
                },
                .size_decreased, .type_changed, .precision_changed, .multiple_changes => {
                    try out.appendSlice(allocator, "-- WARNING: This change requires table recreation in SQLite\n");
                    try out.appendSlice(allocator, "-- See: https://sqlite.org/lang_altertable.html\n");
                    const field = try std.fmt.allocPrint(allocator, "-- Field: {s}.{s}\n", .{ table_name, field_diff.name });
                    defer allocator.free(field);
                    try out.appendSlice(allocator, field);
                },
            }
        },
        .postgres => {
            switch (field_diff.change_type) {
                .size_increased => {
                    const sql_type = types.dataTypeToSql(field_diff.new_field.data_type, dialect);
                    if (field_diff.new_field.data_type == .alpha) {
                        const size = field_diff.new_field.data_type.alpha.size orelse 255;
                        const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} TYPE {s}({d});\n", .{
                            table_name,
                            field_diff.name,
                            sql_type,
                            size,
                        });
                        defer allocator.free(alter);
                        try out.appendSlice(allocator, alter);
                    } else {
                        const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} TYPE {s};\n", .{
                            table_name,
                            field_diff.name,
                            sql_type,
                        });
                        defer allocator.free(alter);
                        try out.appendSlice(allocator, alter);
                    }
                },
                .decorators_changed => {
                    // Handle constraint changes
                    if (field_diff.old_field.decorators.not_null != field_diff.new_field.decorators.not_null) {
                        if (field_diff.new_field.decorators.not_null) {
                            const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} SET NOT NULL;\n", .{ table_name, field_diff.name });
                            defer allocator.free(alter);
                            try out.appendSlice(allocator, alter);
                        } else {
                            const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} DROP NOT NULL;\n", .{ table_name, field_diff.name });
                            defer allocator.free(alter);
                            try out.appendSlice(allocator, alter);
                        }
                    }
                    if (field_diff.old_field.decorators.default_value == null and field_diff.new_field.decorators.default_value != null) {
                        const prefix = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} SET DEFAULT ", .{ table_name, field_diff.name });
                        defer allocator.free(prefix);
                        try out.appendSlice(allocator, prefix);
                        switch (field_diff.new_field.data_type) {
                            .alpha, .string => {
                                const val = try std.fmt.allocPrint(allocator, "'{s}';\n", .{field_diff.new_field.decorators.default_value.?});
                                defer allocator.free(val);
                                try out.appendSlice(allocator, val);
                            },
                            else => {
                                const val = try std.fmt.allocPrint(allocator, "{s};\n", .{field_diff.new_field.decorators.default_value.?});
                                defer allocator.free(val);
                                try out.appendSlice(allocator, val);
                            },
                        }
                    }
                },
                .size_decreased, .type_changed, .precision_changed, .multiple_changes => {
                    try out.appendSlice(allocator, "-- WARNING: Data loss possible. Manual review required.\n");
                    const sql_type = types.dataTypeToSql(field_diff.new_field.data_type, dialect);
                    const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ALTER COLUMN {s} TYPE {s} USING {s}::{s};\n", .{
                        table_name,
                        field_diff.name,
                        sql_type,
                        field_diff.name,
                        sql_type,
                    });
                    defer allocator.free(alter);
                    try out.appendSlice(allocator, alter);
                },
            }
        },
    }
}

/// Generate reverse table alterations for rollback
fn generateTableAlterationsReverse(
    allocator: std.mem.Allocator,
    out: *std.ArrayListAligned(u8, null),
    table_diff: *const TableDiff,
    old_schema: *const SchemaFile,
    dialect: SqlDialect,
) !void {
    // Find the old table definition
    var old_table: ?*const TableSchema = null;
    for (old_schema.tables) |*table| {
        if (std.mem.eql(u8, table.name, table_diff.name)) {
            old_table = table;
            break;
        }
    }

    // Drop added columns
    for (table_diff.added_fields) |field| {
        const drop = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} DROP COLUMN {s};\n", .{ table_diff.name, field.name });
        defer allocator.free(drop);
        try out.appendSlice(allocator, drop);
    }

    // Reverse modified columns
    for (table_diff.modified_fields) |field_diff| {
        // Create a reverse diff
        const reverse_diff = FieldDiff{
            .name = field_diff.name,
            .old_field = field_diff.new_field,
            .new_field = field_diff.old_field,
            .change_type = field_diff.change_type,
        };
        try generateColumnModification(allocator, out, table_diff.name, &reverse_diff, dialect);
    }

    // Re-add removed columns
    if (old_table) |table| {
        for (table_diff.removed_fields) |field_name| {
            for (table.fields) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    const alter = try std.fmt.allocPrint(allocator, "ALTER TABLE {s} ADD COLUMN ", .{table_diff.name});
                    defer allocator.free(alter);
                    try out.appendSlice(allocator, alter);
                    try generateColumnDef(allocator, out, &field, dialect);
                    try out.appendSlice(allocator, ";\n");
                    break;
                }
            }
        }
    }
}

/// Generate CREATE INDEX for composite key
fn generateCreateIndex(allocator: std.mem.Allocator, out: *std.ArrayListAligned(u8, null), key: *const KeyDefinition, dialect: SqlDialect) !void {
    _ = dialect;

    if (key.segments.len == 0) return;

    if (key.primary) {
        // Primary key handled in table definition
        return;
    }

    if (key.unique) {
        const idx = try std.fmt.allocPrint(allocator, "CREATE UNIQUE INDEX IF NOT EXISTS {s}_{s} ON {s}(", .{
            key.table_name,
            key.name,
            key.table_name,
        });
        defer allocator.free(idx);
        try out.appendSlice(allocator, idx);
    } else {
        const idx = try std.fmt.allocPrint(allocator, "CREATE INDEX IF NOT EXISTS {s}_{s} ON {s}(", .{
            key.table_name,
            key.name,
            key.table_name,
        });
        defer allocator.free(idx);
        try out.appendSlice(allocator, idx);
    }

    for (key.segments, 0..) |seg, i| {
        if (i > 0) try out.appendSlice(allocator, ", ");
        try out.appendSlice(allocator, seg);
    }

    try out.appendSlice(allocator, ");\n");
}

/// Write migration to a file
pub fn writeMigrationFile(
    allocator: std.mem.Allocator,
    migration: *const Migration,
    migrations_dir: []const u8,
) ![]const u8 {
    // Create migrations directory if needed
    std.fs.cwd().makePath(migrations_dir) catch {};

    // Generate filename: TIMESTAMP_description.sql
    const filename = try std.fmt.allocPrint(allocator, "{d:0>14}_{s}.sql", .{
        migration.timestamp,
        migration.description,
    });
    defer allocator.free(filename);

    const filepath = try std.fs.path.join(allocator, &.{ migrations_dir, filename });
    errdefer allocator.free(filepath);

    // Write migration file
    const file = try std.fs.cwd().createFile(filepath, .{});
    defer file.close();

    var write_buffer: [8192]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    // Header
    try writer.print("-- Migration: v{d} - {s}\n", .{ migration.version, migration.description });
    try writer.print("-- Generated: {d}\n", .{migration.timestamp});
    try writer.print("-- Dialect: {s}\n", .{@tagName(migration.dialect)});
    try writer.writeAll("\n");

    // UP migration
    try writer.writeAll("-- ============================================\n");
    try writer.writeAll("-- UP MIGRATION\n");
    try writer.writeAll("-- ============================================\n\n");
    try writer.writeAll(migration.up_sql);
    try writer.writeAll("\n");

    // DOWN migration
    try writer.writeAll("-- ============================================\n");
    try writer.writeAll("-- DOWN MIGRATION (for rollback)\n");
    try writer.writeAll("-- ============================================\n\n");
    try writer.writeAll("-- To rollback, run the following SQL:\n");
    try writer.writeAll("/*\n");
    try writer.writeAll(migration.down_sql);
    try writer.writeAll("*/\n");

    try buffered.interface.flush();

    return filepath;
}

// ============================================================================
// Tests
// ============================================================================

test "generate migration for added table" {
    const parser = @import("parser.zig");

    const old_source =
        \\@database test
        \\@version 1
        \\@table customer
        \\    id ,d6 @primary
        \\@endtable
    ;

    const new_source =
        \\@database test
        \\@version 2
        \\@table customer
        \\    id ,d6 @primary
        \\@endtable
        \\@table order
        \\    id ,d8 @primary
        \\    customer_id ,d6
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff_mod.diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    var migration = try generateMigration(std.testing.allocator, &schema_diff, &old_schema, &new_schema, .sqlite);
    defer migration.deinit();

    try std.testing.expect(std.mem.indexOf(u8, migration.up_sql, "CREATE TABLE") != null);
    try std.testing.expect(std.mem.indexOf(u8, migration.up_sql, "order") != null);
    try std.testing.expect(std.mem.indexOf(u8, migration.down_sql, "DROP TABLE") != null);
}

test "generate migration for added column" {
    const parser = @import("parser.zig");

    const old_source =
        \\@database test
        \\@version 1
        \\@table customer
        \\    id ,d6 @primary
        \\@endtable
    ;

    const new_source =
        \\@database test
        \\@version 2
        \\@table customer
        \\    id ,d6 @primary
        \\    name ,a30 @not_null
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff_mod.diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    var migration = try generateMigration(std.testing.allocator, &schema_diff, &old_schema, &new_schema, .postgres);
    defer migration.deinit();

    try std.testing.expect(std.mem.indexOf(u8, migration.up_sql, "ALTER TABLE") != null);
    try std.testing.expect(std.mem.indexOf(u8, migration.up_sql, "ADD COLUMN") != null);
    try std.testing.expect(std.mem.indexOf(u8, migration.up_sql, "name") != null);
}
