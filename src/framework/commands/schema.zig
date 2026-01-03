//! cot schema command
//!
//! Manages database schema definitions and migrations.
//!
//! Subcommands:
//!   cot schema status     Show schema version status
//!   cot schema diff       Show pending schema changes
//!   cot schema migrate    Generate migration file
//!   cot schema apply      Apply pending migrations
//!   cot schema rollback   Rollback last migration

const std = @import("std");
const cot = @import("cot");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const Allocator = std.mem.Allocator;

const schema = cot.schema;

/// Schema command options
pub const SchemaOptions = struct {
    /// Subcommand to run
    subcommand: Subcommand = .status,
    /// Project to operate on (null = current directory)
    project: ?[]const u8 = null,
    /// Verbose output
    verbose: bool = false,
    /// Force operation (skip confirmations)
    force: bool = false,
};

pub const Subcommand = enum {
    status,
    diff,
    migrate,
    apply,
    rollback,
    help,
};

/// Run the schema command
pub fn run(allocator: Allocator, options: SchemaOptions) !void {
    switch (options.subcommand) {
        .status => try runStatus(allocator, options),
        .diff => try runDiff(allocator, options),
        .migrate => try runMigrate(allocator, options),
        .apply => try runApply(allocator, options),
        .rollback => try runRollback(allocator, options),
        .help => try printHelp(),
    }
}

/// Show schema version status
fn runStatus(allocator: Allocator, options: SchemaOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Find workspace and project
    const project_info = try findProject(allocator, options.project);
    defer if (project_info.project_path) |p| allocator.free(p);

    // Load schema file
    const schema_config = project_info.config.schema orelse {
        try stdout.print("No schema configured for this project.\n", .{});
        try stdout.print("Add a 'schema' section to cot.json to enable schema management.\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const schema_path = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var schema_file = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema file '{s}': {}\n", .{ schema_path, err });
        try stdout_writer.interface.flush();
        return error.SchemaLoadFailed;
    };
    defer schema_file.deinit();

    try stdout.print("\n", .{});
    try stdout.print("  Schema Status\n", .{});
    try stdout.print("  =============\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Database:     {s}\n", .{schema_file.database_name});
    try stdout.print("  File version: {d}\n", .{schema_file.version});
    try stdout.print("  Schema file:  {s}\n", .{schema_path});
    try stdout.print("  Backend:      {s}\n", .{schema_config.backend});
    try stdout.print("  Auto-migrate: {}\n", .{schema_config.auto_migrate});
    try stdout.print("\n", .{});
    try stdout.print("  Tables ({d}):\n", .{schema_file.tables.len});

    for (schema_file.tables) |table| {
        try stdout.print("    - {s} ({d} fields)\n", .{ table.name, table.fields.len });

        if (options.verbose) {
            for (table.fields) |field| {
                const type_str = dataTypeToString(field.data_type);
                var decorators_buf: [256]u8 = undefined;
                const decorators = formatDecorators(field.decorators, &decorators_buf);
                try stdout.print("        {s: <20} {s: <10} {s}\n", .{ field.name, type_str, decorators });
            }
        }
    }

    // Display composite keys if any
    if (schema_file.keys.len > 0) {
        try stdout.print("\n", .{});
        try stdout.print("  Keys ({d}):\n", .{schema_file.keys.len});

        for (schema_file.keys) |key| {
            var key_type: []const u8 = "";
            if (key.primary) {
                key_type = "@primary";
            } else if (key.unique) {
                key_type = "@unique";
            }

            try stdout.print("    - {s}.{s} (", .{ key.table_name, key.name });
            for (key.segments, 0..) |seg, i| {
                if (i > 0) try stdout.print(", ", .{});
                try stdout.print("{s}", .{seg});
            }
            try stdout.print(") {s}\n", .{key_type});
        }
    }

    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Show pending schema changes (diff between file and database)
fn runDiff(allocator: Allocator, options: SchemaOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Find workspace and project
    const project_info = try findProject(allocator, options.project);
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("No schema configured for this project.\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const project_path = project_info.project_path orelse ".";

    const schema_path = try std.fs.path.join(allocator, &.{
        project_path,
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var new_schema = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema file: {}\n", .{err});
        try stdout_writer.interface.flush();
        return error.SchemaLoadFailed;
    };
    defer new_schema.deinit();

    try stdout.print("\n", .{});
    try stdout.print("  Schema Diff\n", .{});
    try stdout.print("  ===========\n", .{});
    try stdout.print("\n", .{});

    // Try to load previous schema version from migrations/applied directory
    const applied_schema_path = try std.fs.path.join(allocator, &.{
        project_path,
        "migrations",
        "applied.schema.cot",
    });
    defer allocator.free(applied_schema_path);

    var old_schema_result = schema.parseSchemaFile(allocator, applied_schema_path);
    if (old_schema_result) |*old_schema| {
        defer old_schema.deinit();

        // Use the diff module to compare schemas
        var schema_diff = schema.diff.diff(allocator, old_schema, &new_schema) catch |err| {
            try stdout.print("  Error computing diff: {}\n", .{err});
            try stdout_writer.interface.flush();
            return;
        };
        defer schema_diff.deinit();

        if (!schema_diff.has_changes) {
            try stdout.print("  No changes detected.\n", .{});
            try stdout.print("  Schema is up to date (version {d}).\n", .{new_schema.version});
        } else {
            try stdout.print("  Comparing version {d} → {d}\n\n", .{ old_schema.version, new_schema.version });
            try printSchemaDiff(stdout, &schema_diff, options.verbose);
        }
    } else |_| {
        // No previous schema found - show current as new
        try stdout.print("  No previous schema snapshot found.\n", .{});
        try stdout.print("  To enable schema comparison:\n", .{});
        try stdout.print("    1. Run 'cot schema apply' to create initial migration\n", .{});
        try stdout.print("    2. The applied schema will be saved for future comparisons\n", .{});
        try stdout.print("\n", .{});
        try stdout.print("  Current schema file (version {d}):\n\n", .{new_schema.version});

        for (new_schema.tables) |table| {
            try stdout.print("  + Table: {s}\n", .{table.name});
            for (table.fields) |field| {
                const type_str = dataTypeToString(field.data_type);
                try stdout.print("    + {s: <20} {s}\n", .{ field.name, type_str });
            }
        }
    }

    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Print a schema diff in a human-readable format
fn printSchemaDiff(stdout: anytype, schema_diff: *const schema.SchemaDiff, verbose: bool) !void {
    // Added tables
    if (schema_diff.added_tables.len > 0) {
        try stdout.print("  Added Tables:\n", .{});
        for (schema_diff.added_tables) |table| {
            try stdout.print("    \x1b[32m+ {s}\x1b[0m\n", .{table.name});
            if (verbose) {
                for (table.fields) |field| {
                    const type_str = dataTypeToString(field.data_type);
                    try stdout.print("        {s: <20} {s}\n", .{ field.name, type_str });
                }
            }
        }
        try stdout.print("\n", .{});
    }

    // Removed tables
    if (schema_diff.removed_tables.len > 0) {
        try stdout.print("  Removed Tables:\n", .{});
        for (schema_diff.removed_tables) |name| {
            try stdout.print("    \x1b[31m- {s}\x1b[0m\n", .{name});
        }
        try stdout.print("\n", .{});
    }

    // Modified tables
    if (schema_diff.modified_tables.len > 0) {
        try stdout.print("  Modified Tables:\n", .{});
        for (schema_diff.modified_tables) |table_diff| {
            try stdout.print("    \x1b[33m~ {s}\x1b[0m\n", .{table_diff.name});

            // Added fields
            for (table_diff.added_fields) |field| {
                const type_str = dataTypeToString(field.data_type);
                try stdout.print("        \x1b[32m+ {s: <20} {s}\x1b[0m\n", .{ field.name, type_str });
            }

            // Removed fields
            for (table_diff.removed_fields) |name| {
                try stdout.print("        \x1b[31m- {s}\x1b[0m\n", .{name});
            }

            // Modified fields
            for (table_diff.modified_fields) |field_diff| {
                const old_type = dataTypeToString(field_diff.old_field.data_type);
                const new_type = dataTypeToString(field_diff.new_field.data_type);
                const change_desc = switch (field_diff.change_type) {
                    .type_changed => "type changed",
                    .size_increased => "size increased",
                    .size_decreased => "size decreased",
                    .precision_changed => "precision changed",
                    .decorators_changed => "decorators changed",
                    .multiple_changes => "multiple changes",
                };
                try stdout.print("        \x1b[33m~ {s}: {s} → {s} ({s})\x1b[0m\n", .{
                    field_diff.name,
                    old_type,
                    new_type,
                    change_desc,
                });
            }
        }
        try stdout.print("\n", .{});
    }

    // Added keys
    if (schema_diff.added_keys.len > 0) {
        try stdout.print("  Added Keys:\n", .{});
        for (schema_diff.added_keys) |key| {
            try stdout.print("    \x1b[32m+ {s}.{s}\x1b[0m\n", .{ key.table_name, key.name });
        }
        try stdout.print("\n", .{});
    }

    // Removed keys
    if (schema_diff.removed_keys.len > 0) {
        try stdout.print("  Removed Keys:\n", .{});
        for (schema_diff.removed_keys) |key_id| {
            try stdout.print("    \x1b[31m- {s}\x1b[0m\n", .{key_id});
        }
        try stdout.print("\n", .{});
    }
}

/// Generate a migration file
fn runMigrate(allocator: Allocator, options: SchemaOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const project_info = try findProject(allocator, options.project);
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("No schema configured for this project.\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const schema_path = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var schema_file = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema file: {}\n", .{err});
        try stdout_writer.interface.flush();
        return error.SchemaLoadFailed;
    };
    defer schema_file.deinit();

    // Create migrations directory
    const migrations_dir = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        "migrations",
    });
    defer allocator.free(migrations_dir);

    std.fs.cwd().makePath(migrations_dir) catch {};

    // Generate migration filename
    const timestamp = std.time.timestamp();
    const migration_filename = try std.fmt.allocPrint(allocator, "{d:0>14}_schema_v{d}.sql", .{
        timestamp,
        schema_file.version,
    });
    defer allocator.free(migration_filename);

    const migration_path = try std.fs.path.join(allocator, &.{ migrations_dir, migration_filename });
    defer allocator.free(migration_path);

    // Generate SQL
    const file = try std.fs.cwd().createFile(migration_path, .{});
    defer file.close();

    var write_buffer: [8192]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    try writer.print("-- Migration: Schema version {d}\n", .{schema_file.version});
    try writer.print("-- Database: {s}\n", .{schema_file.database_name});
    try writer.print("-- Generated: {d}\n", .{timestamp});
    try writer.print("-- Backend: {s}\n", .{schema_config.backend});
    try writer.print("\n", .{});

    // Determine SQL dialect
    const dialect: schema.SqlDialect = if (std.mem.eql(u8, schema_config.backend, "postgres"))
        .postgres
    else if (std.mem.eql(u8, schema_config.backend, "turso"))
        .turso
    else
        .sqlite;

    // UP MIGRATION
    try writer.print("-- ============================================\n", .{});
    try writer.print("-- UP MIGRATION\n", .{});
    try writer.print("-- ============================================\n\n", .{});

    // Generate CREATE TABLE statements
    for (schema_file.tables) |table| {
        try generateCreateTable(writer, &table, dialect);
        try writer.print("\n", .{});
    }

    // Generate composite key indexes
    for (schema_file.keys) |key| {
        try generateCompositeKeyIndex(writer, &key, dialect);
    }

    // DOWN MIGRATION (for rollback)
    try writer.print("-- ============================================\n", .{});
    try writer.print("-- DOWN MIGRATION (for rollback)\n", .{});
    try writer.print("-- ============================================\n\n", .{});
    try writer.print("/*\n", .{});

    // Generate DROP TABLE statements in reverse order
    var i: usize = schema_file.tables.len;
    while (i > 0) {
        i -= 1;
        try writer.print("DROP TABLE IF EXISTS {s};\n", .{schema_file.tables[i].name});
    }

    // Drop composite key indexes (in reverse)
    var j: usize = schema_file.keys.len;
    while (j > 0) {
        j -= 1;
        const key = schema_file.keys[j];
        try writer.print("DROP INDEX IF EXISTS {s}_{s};\n", .{ key.table_name, key.name });
    }

    try writer.print("*/\n", .{});

    try buffered.interface.flush();

    try stdout.print("\n", .{});
    try stdout.print("  Migration Generated\n", .{});
    try stdout.print("  ===================\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Created: {s}\n", .{migration_path});
    try stdout.print("\n", .{});
    try stdout.print("  Next steps:\n", .{});
    try stdout.print("    1. Review the migration file\n", .{});
    try stdout.print("    2. Run: cot schema apply\n", .{});
    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Apply pending migrations
fn runApply(allocator: Allocator, options: SchemaOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const project_info = try findProject(allocator, options.project);
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("No schema configured for this project.\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    // Determine SQL dialect
    const dialect: schema.SqlDialect = if (std.mem.eql(u8, schema_config.backend, "postgres"))
        .postgres
    else if (std.mem.eql(u8, schema_config.backend, "turso"))
        .turso
    else
        .sqlite;

    const migrations_dir = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        "migrations",
    });
    defer allocator.free(migrations_dir);

    // Create migration runner
    const runner_inst = schema.MigrationRunner.init(allocator, dialect, migrations_dir);

    // Load pending migrations (assuming version 0 for now - would query DB normally)
    const pending = runner_inst.loadPendingMigrations(0) catch |err| {
        try stdout.print("Error loading migrations: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer runner_inst.freeMigrations(pending);

    try stdout.print("\n", .{});
    try stdout.print("  Apply Migrations\n", .{});
    try stdout.print("  ================\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Backend: {s}\n", .{schema_config.backend});
    try stdout.print("  Migrations directory: {s}\n", .{migrations_dir});
    try stdout.print("\n", .{});

    if (pending.len == 0) {
        try stdout.print("  No pending migrations found.\n", .{});
        try stdout.print("\n", .{});
        try stdout_writer.interface.flush();
        return;
    }

    try stdout.print("  Pending migrations ({d}):\n", .{pending.len});
    for (pending) |migration| {
        try stdout.print("    - {s}\n", .{migration.filename});
    }
    try stdout.print("\n", .{});

    // Generate combined apply script
    try stdout.print("  Generating apply script...\n", .{});

    const apply_script_path = try std.fs.path.join(allocator, &.{ migrations_dir, "_apply_pending.sql" });
    defer allocator.free(apply_script_path);

    const file = std.fs.cwd().createFile(apply_script_path, .{}) catch |err| {
        try stdout.print("Error creating apply script: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer file.close();

    var write_buffer: [8192]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const file_writer = &buffered.interface;

    // Write header
    try file_writer.writeAll("-- Combined migration apply script\n");
    try file_writer.writeAll("-- Run this script against your database to apply all pending migrations\n\n");

    // First, ensure migrations table exists
    try file_writer.writeAll(runner_inst.getCreateMigrationsTableSql());
    try file_writer.writeAll("\n\n");

    // Create schema metadata tables
    try file_writer.writeAll("-- Schema metadata tables\n");
    const metadata_stmts = schema.getCreateMetadataTablesSql();
    for (metadata_stmts) |stmt| {
        try file_writer.writeAll(stmt);
        try file_writer.writeAll("\n");
    }
    try file_writer.writeAll("\n");

    // Write each migration
    for (pending) |migration| {
        const apply_sql = schema.runner.generateApplyScript(allocator, &migration) catch continue;
        defer allocator.free(apply_sql);
        try file_writer.writeAll(apply_sql);
        try file_writer.writeAll("\n");
    }

    // Generate schema metadata if we have a schema file
    if (project_info.config.schema) |schema_cfg| {
        const schema_file_path = try std.fs.path.join(allocator, &.{
            project_info.project_path orelse ".",
            schema_cfg.file,
        });
        defer allocator.free(schema_file_path);

        if (schema.parseSchemaFile(allocator, schema_file_path)) |parsed_schema_val| {
            var parsed_schema = parsed_schema_val;
            defer parsed_schema.deinit();

            try file_writer.writeAll("\n-- Schema metadata\n");
            const metadata_sql = schema.generateSchemaMetadataSql(allocator, &parsed_schema) catch null;
            if (metadata_sql) |sql| {
                defer allocator.free(sql);
                try file_writer.writeAll(sql);
            }
        } else |_| {
            // Schema file not found or parse error, skip metadata
        }
    }

    try buffered.interface.flush();

    try stdout.print("\n", .{});
    try stdout.print("  Created: {s}\n", .{apply_script_path});
    try stdout.print("\n", .{});
    try stdout.print("  To apply migrations, run this SQL against your database:\n", .{});
    try stdout.print("    sqlite3 your_database.db < {s}\n", .{apply_script_path});
    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Rollback the last migration
fn runRollback(allocator: Allocator, options: SchemaOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const project_info = try findProject(allocator, options.project);
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("No schema configured for this project.\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    // Determine SQL dialect
    const dialect: schema.SqlDialect = if (std.mem.eql(u8, schema_config.backend, "postgres"))
        .postgres
    else if (std.mem.eql(u8, schema_config.backend, "turso"))
        .turso
    else
        .sqlite;

    const migrations_dir = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        "migrations",
    });
    defer allocator.free(migrations_dir);

    // Create migration runner
    const runner_inst = schema.MigrationRunner.init(allocator, dialect, migrations_dir);

    // Load all migrations to find the latest one
    const migrations = runner_inst.loadPendingMigrations(0) catch |err| {
        try stdout.print("Error loading migrations: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer runner_inst.freeMigrations(migrations);

    try stdout.print("\n", .{});
    try stdout.print("  Rollback Migration\n", .{});
    try stdout.print("  ==================\n", .{});
    try stdout.print("\n", .{});

    if (migrations.len == 0) {
        try stdout.print("  No migrations found to rollback.\n", .{});
        try stdout.print("\n", .{});
        try stdout_writer.interface.flush();
        return;
    }

    // Get the last migration
    const last_migration = migrations[migrations.len - 1];

    try stdout.print("  Rolling back: {s}\n", .{last_migration.filename});
    try stdout.print("\n", .{});

    // Generate rollback script
    const rollback_script_path = try std.fs.path.join(allocator, &.{ migrations_dir, "_rollback_last.sql" });
    defer allocator.free(rollback_script_path);

    const file = std.fs.cwd().createFile(rollback_script_path, .{}) catch |err| {
        try stdout.print("Error creating rollback script: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer file.close();

    var write_buffer: [8192]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const file_writer = &buffered.interface;

    const rollback_sql = schema.runner.generateRollbackScript(allocator, &last_migration) catch |err| {
        try stdout.print("Error generating rollback script: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer allocator.free(rollback_sql);

    try file_writer.writeAll(rollback_sql);
    try buffered.interface.flush();

    try stdout.print("  Created: {s}\n", .{rollback_script_path});
    try stdout.print("\n", .{});
    try stdout.print("  To rollback, run this SQL against your database:\n", .{});
    try stdout.print("    sqlite3 your_database.db < {s}\n", .{rollback_script_path});
    try stdout.print("\n", .{});
    try stdout.print("  WARNING: Rollback may cause data loss. Review the script before executing.\n", .{});
    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

// ============================================================================
// Helper functions
// ============================================================================

const ProjectInfo = struct {
    config: config.ProjectConfig,
    project_path: ?[]const u8,
};

fn findProject(allocator: Allocator, project_name: ?[]const u8) !ProjectInfo {
    _ = project_name;

    // Try to load config from current directory
    var loader = config.ConfigLoader.init(allocator);
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");

    const cfg = loader.load(cwd) catch {
        // Try to find workspace root
        if (try loader.findWorkspaceRoot(cwd)) |ws_root| {
            defer allocator.free(ws_root);
            allocator.free(cwd);
            return ProjectInfo{
                .config = try loader.load(ws_root),
                .project_path = try allocator.dupe(u8, ws_root),
            };
        }
        allocator.free(cwd);
        return error.NoWorkspace;
    };

    return ProjectInfo{
        .config = cfg,
        .project_path = cwd,
    };
}

fn dataTypeToString(dt: schema.SchemaDataType) []const u8 {
    return switch (dt) {
        .text => "text",
        .integer => "integer",
        .decimal => "decimal",
        .boolean => "boolean",
        .blob => "blob",
        .datetime => "datetime",
        .ulid => "ulid",
        .reference => "reference",
    };
}

fn formatDecorators(d: schema.Decorators, buf: []u8) []const u8 {
    var pos: usize = 0;

    if (d.primary) {
        const s = "@primary ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.auto) {
        const s = "@auto ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.not_null) {
        const s = "@not_null ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.unique) {
        const s = "@unique ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.index) {
        const s = "@index ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.audit_created) {
        const s = "@audit_created ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.audit_updated) {
        const s = "@audit_updated ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }

    return buf[0..pos];
}

fn generateCreateTable(writer: anytype, table: *const schema.TableSchema, dialect: schema.SqlDialect) !void {
    try writer.print("CREATE TABLE IF NOT EXISTS {s} (\n", .{table.name});

    var primary_key: ?[]const u8 = null;
    var first = true;

    for (table.fields) |field| {
        if (!first) {
            try writer.print(",\n", .{});
        }
        first = false;

        const sql_type = schema.dataTypeToSql(field.data_type, dialect);

        // Add size for VARCHAR in postgres
        if (dialect == .postgres and field.data_type == .text) {
            const size = field.data_type.text.max_length orelse 255;
            try writer.print("    {s} {s}({d})", .{ field.name, sql_type, size });
        } else {
            try writer.print("    {s} {s}", .{ field.name, sql_type });
        }

        // Add constraints
        if (field.decorators.primary) {
            primary_key = field.name;
            if (field.decorators.auto) {
                switch (dialect) {
                    .sqlite, .turso => try writer.print(" PRIMARY KEY", .{}),
                    .postgres => try writer.print(" PRIMARY KEY", .{}),
                }
            }
        }

        if (field.decorators.not_null and !field.decorators.primary) {
            try writer.print(" NOT NULL", .{});
        }

        if (field.decorators.unique and !field.decorators.primary) {
            try writer.print(" UNIQUE", .{});
        }

        if (field.decorators.default_value) |default| {
            switch (field.data_type) {
                .text, .ulid, .reference => try writer.print(" DEFAULT '{s}'", .{default}),
                else => try writer.print(" DEFAULT {s}", .{default}),
            }
        }
    }

    // Add primary key constraint if not inline
    if (primary_key != null and dialect == .postgres) {
        // Already handled inline for postgres
    }

    try writer.print("\n);\n", .{});

    // Generate indexes
    for (table.fields) |field| {
        if (field.decorators.index and !field.decorators.primary and !field.decorators.unique) {
            try writer.print("CREATE INDEX IF NOT EXISTS idx_{s}_{s} ON {s}({s});\n", .{
                table.name,
                field.name,
                table.name,
                field.name,
            });
        }
    }
}

fn generateCompositeKeyIndex(writer: anytype, key: *const schema.KeyDefinition, dialect: schema.SqlDialect) !void {
    _ = dialect;

    if (key.segments.len == 0) return;

    // For primary keys, we don't generate a separate index (handled in table definition)
    // For unique composite keys, generate a unique index
    // For regular composite keys, generate a non-unique index

    if (key.primary) {
        // Primary key handled differently - could be added to table definition
        // For now, skip as SQLite/Postgres handle composite PKs differently
        return;
    }

    if (key.unique) {
        try writer.print("CREATE UNIQUE INDEX IF NOT EXISTS {s}_{s} ON {s}(", .{
            key.table_name,
            key.name,
            key.table_name,
        });
    } else {
        try writer.print("CREATE INDEX IF NOT EXISTS {s}_{s} ON {s}(", .{
            key.table_name,
            key.name,
            key.table_name,
        });
    }

    for (key.segments, 0..) |seg, i| {
        if (i > 0) try writer.print(", ", .{});
        try writer.print("{s}", .{seg});
    }

    try writer.print(");\n", .{});
}

/// Parse schema command arguments
pub fn parseArgs(args: []const []const u8) SchemaOptions {
    var options = SchemaOptions{};
    var i: usize = 0;

    // First argument is subcommand
    if (i < args.len) {
        const subcmd = args[i];
        if (std.mem.eql(u8, subcmd, "status")) {
            options.subcommand = .status;
        } else if (std.mem.eql(u8, subcmd, "diff")) {
            options.subcommand = .diff;
        } else if (std.mem.eql(u8, subcmd, "migrate")) {
            options.subcommand = .migrate;
        } else if (std.mem.eql(u8, subcmd, "apply")) {
            options.subcommand = .apply;
        } else if (std.mem.eql(u8, subcmd, "rollback")) {
            options.subcommand = .rollback;
        } else if (std.mem.eql(u8, subcmd, "help") or std.mem.eql(u8, subcmd, "--help") or std.mem.eql(u8, subcmd, "-h")) {
            options.subcommand = .help;
        }
        i += 1;
    }

    // Parse remaining flags
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            options.verbose = true;
        } else if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            options.force = true;
        } else if (std.mem.eql(u8, arg, "--project") or std.mem.eql(u8, arg, "-p")) {
            if (i + 1 < args.len) {
                i += 1;
                options.project = args[i];
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            options.project = arg;
        }
    }

    return options;
}

pub fn printHelp() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Usage: cot schema <subcommand> [options]
        \\
        \\Manage database schema definitions and migrations.
        \\
        \\Subcommands:
        \\  status      Show schema version and table information
        \\  diff        Show pending schema changes
        \\  migrate     Generate a migration SQL file
        \\  apply       Apply pending migrations to database
        \\  rollback    Rollback the last applied migration
        \\
        \\Options:
        \\  -v, --verbose   Show detailed output
        \\  -f, --force     Skip confirmation prompts
        \\  -p, --project   Specify project (default: current directory)
        \\  --help          Show this help message
        \\
        \\Examples:
        \\  cot schema status              Show current schema status
        \\  cot schema status -v           Show status with field details
        \\  cot schema diff                Show pending changes
        \\  cot schema migrate             Generate migration file
        \\  cot schema apply               Apply migrations
        \\  cot schema rollback            Rollback last migration
        \\
        \\Schema File Format (schema.cot):
        \\  @database myapp
        \\  @version 1
        \\
        \\  @table customer
        \\      id          ,d6     @primary @auto
        \\      name        ,a30    @not_null
        \\      email       ,a50    @unique @index
        \\      created_at  ,d14    @audit_created
        \\  @endtable
        \\
        \\Configuration (cot.json):
        \\  {
        \\    "schema": {
        \\      "file": "schema.cot",
        \\      "autoMigrate": false,
        \\      "backend": "sqlite"
        \\    }
        \\  }
        \\
    );
    try stdout_writer.interface.flush();
}
