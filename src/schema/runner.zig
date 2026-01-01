//! Schema Migration Runner
//!
//! Applies and rolls back migrations against a database.
//! Tracks applied migrations in a _cot_migrations table.
//!
//! Usage:
//!   - runner.apply(db, migrations) - Apply pending migrations
//!   - runner.rollback(db, n) - Rollback last n migrations
//!   - runner.status(db) - Get current migration status

const std = @import("std");
const types = @import("types.zig");
const migrate = @import("migrate.zig");

const SqlDialect = types.SqlDialect;
const Migration = migrate.Migration;

/// Migration record stored in database
pub const MigrationRecord = struct {
    /// Migration version
    version: u32,
    /// Filename of migration
    filename: []const u8,
    /// Unix timestamp when applied
    applied_at: i64,
    /// Checksum of migration content
    checksum: u32,
};

/// Migration status information
pub const MigrationStatus = struct {
    /// Current schema version in database
    current_version: u32,
    /// List of applied migrations
    applied: []const MigrationRecord,
    /// Number of pending migrations
    pending_count: usize,

    allocator: std.mem.Allocator,

    pub fn deinit(self: *MigrationStatus) void {
        for (self.applied) |record| {
            self.allocator.free(record.filename);
        }
        self.allocator.free(self.applied);
    }
};

/// Result of applying migrations
pub const ApplyResult = struct {
    /// Number of migrations applied
    applied_count: usize,
    /// New schema version
    new_version: u32,
    /// Errors encountered (if any)
    errors: []const []const u8,

    allocator: std.mem.Allocator,

    pub fn deinit(self: *ApplyResult) void {
        for (self.errors) |err| {
            self.allocator.free(err);
        }
        self.allocator.free(self.errors);
    }
};

/// Migration runner that manages database migrations
pub const MigrationRunner = struct {
    allocator: std.mem.Allocator,
    dialect: SqlDialect,
    migrations_dir: []const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, dialect: SqlDialect, migrations_dir: []const u8) Self {
        return .{
            .allocator = allocator,
            .dialect = dialect,
            .migrations_dir = migrations_dir,
        };
    }

    /// Get SQL to create the migrations tracking table
    pub fn getCreateMigrationsTableSql(self: *const Self) []const u8 {
        _ = self;
        return
            \\CREATE TABLE IF NOT EXISTS _cot_migrations (
            \\    version INTEGER PRIMARY KEY,
            \\    filename TEXT NOT NULL,
            \\    applied_at INTEGER NOT NULL,
            \\    checksum INTEGER NOT NULL
            \\);
        ;
    }

    /// Get SQL to insert a migration record
    pub fn getInsertMigrationSql(self: *const Self) []const u8 {
        _ = self;
        return "INSERT INTO _cot_migrations (version, filename, applied_at, checksum) VALUES (?, ?, ?, ?);";
    }

    /// Get SQL to delete a migration record
    pub fn getDeleteMigrationSql(self: *const Self) []const u8 {
        _ = self;
        return "DELETE FROM _cot_migrations WHERE version = ?;";
    }

    /// Get SQL to query current version
    pub fn getVersionQuerySql(self: *const Self) []const u8 {
        _ = self;
        return "SELECT COALESCE(MAX(version), 0) FROM _cot_migrations;";
    }

    /// Get SQL to query all applied migrations
    pub fn getAppliedMigrationsSql(self: *const Self) []const u8 {
        _ = self;
        return "SELECT version, filename, applied_at, checksum FROM _cot_migrations ORDER BY version;";
    }

    /// Load pending migration files from the migrations directory
    pub fn loadPendingMigrations(self: *const Self, current_version: u32) ![]MigrationFile {
        var migrations: std.ArrayListAligned(MigrationFile, null) = .empty;
        errdefer {
            for (migrations.items) |*m| {
                self.allocator.free(m.filename);
                self.allocator.free(m.up_sql);
                self.allocator.free(m.down_sql);
            }
            migrations.deinit(self.allocator);
        }

        // Open migrations directory
        var dir = std.fs.cwd().openDir(self.migrations_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) {
                return try migrations.toOwnedSlice(self.allocator);
            }
            return err;
        };
        defer dir.close();

        // Collect all .sql files
        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".sql")) continue;

            // Parse version from filename (format: TIMESTAMP_description.sql)
            const version = parseVersionFromFilename(entry.name) orelse continue;
            if (version <= current_version) continue;

            // Load migration content
            const filepath = try std.fs.path.join(self.allocator, &.{ self.migrations_dir, entry.name });
            defer self.allocator.free(filepath);

            const file = try std.fs.cwd().openFile(filepath, .{});
            defer file.close();

            const content = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024); // 10MB max
            defer self.allocator.free(content);

            // Parse up and down SQL from content
            const up_sql = try extractUpSql(self.allocator, content);
            errdefer self.allocator.free(up_sql);

            const down_sql = try extractDownSql(self.allocator, content);

            try migrations.append(self.allocator, .{
                .version = version,
                .filename = try self.allocator.dupe(u8, entry.name),
                .up_sql = up_sql,
                .down_sql = down_sql,
                .checksum = calculateChecksum(content),
            });
        }

        // Sort by version
        std.mem.sort(MigrationFile, migrations.items, {}, struct {
            fn lessThan(_: void, a: MigrationFile, b: MigrationFile) bool {
                return a.version < b.version;
            }
        }.lessThan);

        return try migrations.toOwnedSlice(self.allocator);
    }

    /// Free migration files
    pub fn freeMigrations(self: *const Self, migrations: []MigrationFile) void {
        for (migrations) |*m| {
            self.allocator.free(m.filename);
            self.allocator.free(m.up_sql);
            self.allocator.free(m.down_sql);
        }
        self.allocator.free(migrations);
    }
};

/// A migration file loaded from disk
pub const MigrationFile = struct {
    version: u32,
    filename: []const u8,
    up_sql: []const u8,
    down_sql: []const u8,
    checksum: u32,
};

/// Parse version number from migration filename
fn parseVersionFromFilename(filename: []const u8) ?u32 {
    // Format: TIMESTAMP_description.sql or VERSION_description.sql
    // We use the timestamp as version for ordering

    // Find the first underscore
    const underscore_pos = std.mem.indexOf(u8, filename, "_") orelse return null;
    if (underscore_pos == 0) return null;

    const version_str = filename[0..underscore_pos];

    // Try to parse as number
    return std.fmt.parseInt(u32, version_str, 10) catch {
        // If it's a timestamp (14 digits), take last 9 digits to fit in u32
        if (version_str.len >= 9) {
            return std.fmt.parseInt(u32, version_str[version_str.len - 9 ..], 10) catch null;
        }
        return null;
    };
}

/// Extract UP migration SQL from file content
fn extractUpSql(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Look for UP MIGRATION section
    const up_marker = "-- UP MIGRATION";
    const down_marker = "-- DOWN MIGRATION";

    const up_start = std.mem.indexOf(u8, content, up_marker) orelse {
        // No markers, assume entire file is up migration
        return try allocator.dupe(u8, content);
    };

    const after_up = up_start + up_marker.len;
    // Skip to end of line
    var sql_start = after_up;
    while (sql_start < content.len and content[sql_start] != '\n') {
        sql_start += 1;
    }
    if (sql_start < content.len) sql_start += 1;

    const sql_end = std.mem.indexOf(u8, content[sql_start..], down_marker) orelse content.len - sql_start;

    const sql = std.mem.trim(u8, content[sql_start .. sql_start + sql_end], " \t\r\n");
    return try allocator.dupe(u8, sql);
}

/// Extract DOWN migration SQL from file content
fn extractDownSql(allocator: std.mem.Allocator, content: []const u8) ![]const u8 {
    // Look for DOWN MIGRATION section (inside comment block)
    const down_marker = "-- DOWN MIGRATION";
    const comment_start = "/*";
    const comment_end = "*/";

    const down_pos = std.mem.indexOf(u8, content, down_marker) orelse {
        return try allocator.dupe(u8, "");
    };

    // Find the /* after down marker
    const after_down = content[down_pos..];
    const sql_start_pos = std.mem.indexOf(u8, after_down, comment_start) orelse {
        return try allocator.dupe(u8, "");
    };
    const sql_start = down_pos + sql_start_pos + comment_start.len;

    // Find the closing */
    const remaining = content[sql_start..];
    const sql_end_pos = std.mem.indexOf(u8, remaining, comment_end) orelse remaining.len;

    const sql = std.mem.trim(u8, remaining[0..sql_end_pos], " \t\r\n");
    return try allocator.dupe(u8, sql);
}

/// Calculate a simple checksum for migration content
fn calculateChecksum(content: []const u8) u32 {
    var hash: u32 = 0;
    for (content) |byte| {
        hash = hash *% 31 +% byte;
    }
    return hash;
}

/// Generate SQL to apply a single migration (for manual execution)
pub fn generateApplyScript(
    allocator: std.mem.Allocator,
    migration: *const MigrationFile,
) ![]const u8 {
    const timestamp = std.time.timestamp();

    return try std.fmt.allocPrint(allocator,
        \\-- Applying migration: {s}
        \\BEGIN TRANSACTION;
        \\
        \\{s}
        \\
        \\INSERT INTO _cot_migrations (version, filename, applied_at, checksum) VALUES ({d}, '{s}', {d}, {d});
        \\
        \\COMMIT;
        \\
    , .{ migration.filename, migration.up_sql, migration.version, migration.filename, timestamp, migration.checksum });
}

/// Generate SQL to rollback a single migration (for manual execution)
pub fn generateRollbackScript(
    allocator: std.mem.Allocator,
    migration: *const MigrationFile,
) ![]const u8 {
    return try std.fmt.allocPrint(allocator,
        \\-- Rolling back migration: {s}
        \\BEGIN TRANSACTION;
        \\
        \\{s}
        \\
        \\DELETE FROM _cot_migrations WHERE version = {d};
        \\
        \\COMMIT;
        \\
    , .{ migration.filename, migration.down_sql, migration.version });
}

// ============================================================================
// Tests
// ============================================================================

test "parse version from filename" {
    try std.testing.expectEqual(@as(?u32, 1), parseVersionFromFilename("1_initial.sql"));
    try std.testing.expectEqual(@as(?u32, 123), parseVersionFromFilename("123_add_table.sql"));
    try std.testing.expect(parseVersionFromFilename("invalid.sql") == null);
    try std.testing.expect(parseVersionFromFilename("_no_version.sql") == null);
}

test "calculate checksum" {
    const content1 = "CREATE TABLE test (id INTEGER);";
    const content2 = "CREATE TABLE test (id INTEGER);";
    const content3 = "CREATE TABLE other (id INTEGER);";

    try std.testing.expectEqual(calculateChecksum(content1), calculateChecksum(content2));
    try std.testing.expect(calculateChecksum(content1) != calculateChecksum(content3));
}

test "extract up sql" {
    const content =
        \\-- Migration: v1
        \\-- ============================================
        \\-- UP MIGRATION
        \\-- ============================================
        \\
        \\CREATE TABLE test (id INTEGER);
        \\
        \\-- ============================================
        \\-- DOWN MIGRATION (for rollback)
        \\-- ============================================
        \\
        \\/*
        \\DROP TABLE test;
        \\*/
    ;

    const up_sql = try extractUpSql(std.testing.allocator, content);
    defer std.testing.allocator.free(up_sql);

    try std.testing.expect(std.mem.indexOf(u8, up_sql, "CREATE TABLE test") != null);
    try std.testing.expect(std.mem.indexOf(u8, up_sql, "DROP TABLE") == null);
}

test "extract down sql" {
    const content =
        \\-- Migration: v1
        \\-- ============================================
        \\-- UP MIGRATION
        \\-- ============================================
        \\
        \\CREATE TABLE test (id INTEGER);
        \\
        \\-- ============================================
        \\-- DOWN MIGRATION (for rollback)
        \\-- ============================================
        \\
        \\/*
        \\DROP TABLE test;
        \\*/
    ;

    const down_sql = try extractDownSql(std.testing.allocator, content);
    defer std.testing.allocator.free(down_sql);

    try std.testing.expect(std.mem.indexOf(u8, down_sql, "DROP TABLE test") != null);
}
