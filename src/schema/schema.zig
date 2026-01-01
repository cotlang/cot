//! Schema Repository Module
//!
//! Provides Prisma-inspired schema-first database definitions for Cot.
//! Schema files serve as the single source of truth for database structures,
//! eliminating the need to manually define structures in code.
//!
//! ## Usage
//!
//! 1. Create a `schema.cot` file:
//! ```
//! @database myapp
//! @version 1
//!
//! @table customer
//!     id          ,ulid       @primary @auto
//!     name        ,text(30)   @not_null
//!     email       ,text(50)   @unique @index
//!     created_at  ,datetime   @audit_created
//!     updated_at  ,datetime   @audit_updated
//! @endtable
//! ```
//!
//! 2. Reference in `cot.json`:
//! ```json
//! {
//!   "schema": {
//!     "file": "schema.cot",
//!     "auto_migrate": false
//!   }
//! }
//! ```
//!
//! 3. Use in code:
//! ```cot
//! fn main() {
//!     let cust = Customer{}
//!     cust.name = "John"
//!     db.insert(cust)  // created_at auto-populated
//! }
//! ```
//!
//! ## Features
//!
//! - **Schema-first**: Define tables once, use everywhere
//! - **Decorators**: @primary, @auto, @not_null, @unique, @index, @default
//! - **Foreign keys**: @foreign(table.column) with @on_delete actions
//! - **Audit fields**: @audit_created, @audit_updated for automatic timestamps
//! - **Migrations**: Track schema changes between versions
//! - **Multi-backend**: SQLite, Postgres, Turso support
//!
//! ## Modern Type Syntax
//!
//! | Type         | Description              |
//! |--------------|--------------------------|
//! | ulid         | ULID primary key         |
//! | text(N)      | Text with max length N   |
//! | integer      | 32-bit integer           |
//! | bigint       | 64-bit integer           |
//! | decimal(P,S) | Decimal with precision   |
//! | boolean      | Boolean                  |
//! | datetime     | Date and time            |
//! | date         | Date only                |
//! | time         | Time only                |
//! | blob         | Binary data              |

const std = @import("std");

// Re-export all schema types
pub const types = @import("types.zig");
pub const parser = @import("parser.zig");
pub const converter = @import("converter.zig");
pub const diff = @import("diff.zig");
pub const migrate = @import("migrate.zig");
pub const runner = @import("runner.zig");
pub const audit = @import("audit.zig");
pub const foreign_key = @import("foreign_key.zig");
pub const metadata = @import("metadata.zig");
pub const sql = @import("sql.zig");

// Convenience type aliases
pub const SchemaFile = types.SchemaFile;
pub const TableSchema = types.TableSchema;
pub const FieldSchema = types.FieldSchema;
pub const SchemaDataType = types.SchemaDataType;
pub const Decorators = types.Decorators;
pub const ForeignKey = types.ForeignKey;
pub const OnDeleteAction = types.OnDeleteAction;
pub const KeyDefinition = types.KeyDefinition;
pub const SqlDialect = types.SqlDialect;
pub const SchemaError = types.SchemaError;

pub const SchemaParser = parser.SchemaParser;
pub const SchemaMetadata = converter.SchemaMetadata;
pub const TableInfo = converter.TableInfo;
pub const FieldInfo = converter.FieldInfo;
pub const getTableInfo = converter.getTableInfo;
pub const freeTableInfo = converter.freeTableInfo;

// Diff types
pub const SchemaDiff = diff.SchemaDiff;
pub const TableDiff = diff.TableDiff;
pub const FieldDiff = diff.FieldDiff;
pub const ChangeType = diff.ChangeType;

// Migration types
pub const Migration = migrate.Migration;
pub const MigrationRunner = runner.MigrationRunner;
pub const MigrationFile = runner.MigrationFile;
pub const MigrationStatus = runner.MigrationStatus;

// Audit types
pub const AuditResult = audit.AuditResult;
pub const applyAuditFields = audit.applyAuditFields;
pub const hasAuditFields = audit.hasAuditFields;

// Foreign key types
pub const ForeignKeyError = foreign_key.ForeignKeyError;
pub const ForeignKeyLookup = foreign_key.ForeignKeyLookup;
pub const ValidationResult = foreign_key.ValidationResult;
pub const validateForeignKeys = foreign_key.validateForeignKeys;
pub const hasForeignKeys = foreign_key.hasForeignKeys;

// Metadata types and functions
pub const CreateMetadataTablesSql = metadata.CreateMetadataTablesSql;
pub const getCreateMetadataTablesSql = metadata.getCreateMetadataTablesSql;
pub const generateSchemaMetadataSql = metadata.generateSchemaMetadataSql;

// SQL dialect types and functions
pub const SqlBuilder = sql.SqlBuilder;
pub const SqlOptions = sql.SqlOptions;
pub const DialectFeature = sql.DialectFeature;
pub const generateCreateTable = sql.generateCreateTable;
pub const generateDropTable = sql.generateDropTable;
pub const generateSchema = sql.generateSchema;
pub const dialectSupports = sql.dialectSupports;
pub const timestampType = sql.timestampType;
pub const booleanType = sql.booleanType;
pub const jsonType = sql.jsonType;
pub const currentTimestamp = sql.currentTimestamp;
pub const autoIncrementType = sql.autoIncrementType;

/// Parse a schema from source string
pub fn parseSchema(allocator: std.mem.Allocator, source: []const u8) SchemaError!SchemaFile {
    return parser.parseSchema(allocator, source);
}

/// Parse a schema from a file path
pub fn parseSchemaFile(allocator: std.mem.Allocator, path: []const u8) !SchemaFile {
    return parser.parseSchemaFile(allocator, path);
}

/// Convert a schema data type to SQL for a given dialect
pub fn dataTypeToSql(data_type: SchemaDataType, dialect: SqlDialect) []const u8 {
    return types.dataTypeToSql(data_type, dialect);
}

// ============================================================================
// Tests
// ============================================================================

test "schema module integration" {
    const source =
        \\@database integration_test
        \\@version 1
        \\
        \\@table user
        \\    id      ,ulid       @primary @auto
        \\    name    ,text(50)   @not_null
        \\    email   ,text(100)  @unique @index
        \\@endtable
    ;

    // Parse schema
    var schema = try parseSchema(std.testing.allocator, source);
    defer schema.deinit();

    try std.testing.expectEqualStrings("integration_test", schema.database_name);
    try std.testing.expectEqual(@as(u32, 1), schema.version);

    // Create metadata for runtime queries
    var schema_meta = try SchemaMetadata.init(std.testing.allocator, &schema);
    defer schema_meta.deinit();

    try std.testing.expect(schema_meta.isPrimaryKey("user", "id"));
    try std.testing.expect(schema_meta.isAutoGenerated("user", "id"));

    // Get table info
    var info = try getTableInfo(std.testing.allocator, &schema.tables[0]);
    defer freeTableInfo(std.testing.allocator, &info);

    try std.testing.expectEqualStrings("user", info.name);
    try std.testing.expectEqual(@as(usize, 3), info.fields.len);
}
