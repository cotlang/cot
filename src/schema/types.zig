//! Schema Repository Types
//!
//! Types for representing database schema definitions that serve as the
//! single source of truth for ISAM table structures. Inspired by Prisma's
//! schema-first approach.
//!
//! This module is self-contained and does not depend on the compiler's AST.
//! It uses its own type system optimized for database schema representation.

const std = @import("std");

/// Schema-specific data types (independent from compiler AST)
/// These map directly to SQL/database types for schema definitions
pub const SchemaDataType = union(enum) {
    /// Fixed-length string (TEXT in SQL, VARCHAR in Postgres)
    text: TextType,
    /// Integer types
    integer: IntegerSize,
    /// Floating-point/decimal numbers
    decimal: DecimalType,
    /// Boolean
    boolean: void,
    /// Binary/blob data
    blob: BlobType,
    /// Date/time types
    datetime: DateTimeType,
    /// ULID (for primary keys)
    ulid: void,
    /// Reference to another table (foreign key)
    reference: []const u8,

    pub const TextType = struct {
        /// Maximum length (null = unlimited TEXT)
        max_length: ?usize = null,
    };

    pub const IntegerSize = enum {
        /// 8-bit signed integer
        tiny, // i8
        /// 16-bit signed integer
        small, // i16
        /// 32-bit signed integer (default)
        medium, // i32
        /// 64-bit signed integer
        big, // i64
    };

    pub const DecimalType = struct {
        /// Total digits (precision)
        precision: u8 = 10,
        /// Decimal places (scale)
        scale: u8 = 2,
    };

    pub const BlobType = struct {
        /// Maximum size (null = unlimited)
        max_size: ?usize = null,
    };

    pub const DateTimeType = enum {
        /// Date only (YYYY-MM-DD)
        date,
        /// Time only (HH:MM:SS)
        time,
        /// Date and time
        datetime,
        /// Unix timestamp
        timestamp,
    };

    /// Get the byte size of this type (for fixed-width types)
    pub fn byteSize(self: SchemaDataType) usize {
        return switch (self) {
            .text => |t| t.max_length orelse 0,
            .integer => |i| switch (i) {
                .tiny => 1,
                .small => 2,
                .medium => 4,
                .big => 8,
            },
            .decimal => 8, // Stored as fixed-point i64
            .boolean => 1,
            .blob => |b| b.max_size orelse 0,
            .datetime => |d| switch (d) {
                .date => 4, // Days since epoch
                .time => 4, // Seconds since midnight
                .datetime => 8, // Unix timestamp with subseconds
                .timestamp => 8, // Unix timestamp
            },
            .ulid => 16, // 128 bits
            .reference => 16, // Same as ULID
        };
    }
};

/// A complete schema file definition
pub const SchemaFile = struct {
    /// Database name (e.g., "customers", "inventory")
    database_name: []const u8,
    /// Schema version for migration tracking
    version: u32,
    /// Table definitions
    tables: []TableSchema,
    /// Composite key definitions
    keys: []KeyDefinition = &.{},
    /// Allocator used for this schema
    allocator: std.mem.Allocator,
    /// Source buffer that string slices point into (owned by this struct)
    source: ?[]const u8 = null,

    pub fn deinit(self: *SchemaFile) void {
        for (self.tables) |*table| {
            table.deinit(self.allocator);
        }
        self.allocator.free(self.tables);
        // Free key definitions
        for (self.keys) |*key| {
            var k = key.*;
            k.deinit(self.allocator);
        }
        if (self.keys.len > 0) {
            self.allocator.free(self.keys);
        }
        // Free the source buffer if we own it
        if (self.source) |src| {
            self.allocator.free(src);
        }
    }

    /// Get all keys for a specific table
    pub fn getKeysForTable(self: *const SchemaFile, table_name: []const u8) []const KeyDefinition {
        var count: usize = 0;
        for (self.keys) |key| {
            if (std.mem.eql(u8, key.table_name, table_name)) {
                count += 1;
            }
        }
        if (count == 0) return &.{};

        // Return a view into the keys array (caller should not free)
        // Note: This is a simplification - in practice we might want to return indices
        return self.keys;
    }
};

/// A table definition within the schema
pub const TableSchema = struct {
    /// Table name (e.g., "customer", "order")
    name: []const u8,
    /// Field definitions
    fields: []FieldSchema,

    pub fn deinit(self: *TableSchema, allocator: std.mem.Allocator) void {
        for (self.fields) |*field| {
            field.deinit(allocator);
        }
        allocator.free(self.fields);
    }

    /// Calculate the total record size in bytes
    pub fn recordSize(self: *const TableSchema) usize {
        var size: usize = 0;
        for (self.fields) |field| {
            size += field.byteSize();
        }
        return size;
    }
};

/// A field definition within a table
pub const FieldSchema = struct {
    /// Field name (e.g., "id", "name", "email")
    name: []const u8,
    /// Data type (schema-specific)
    data_type: SchemaDataType,
    /// Field decorators/constraints
    decorators: Decorators,

    pub fn deinit(self: *FieldSchema, allocator: std.mem.Allocator) void {
        // Default values point into the source string, so we don't free them
        _ = self;
        _ = allocator;
    }

    /// Get the byte size of this field
    pub fn byteSize(self: *const FieldSchema) usize {
        return self.data_type.byteSize();
    }
};

/// Field decorators/constraints
pub const Decorators = struct {
    /// Primary key field
    primary: bool = false,
    /// Auto-generate value (ULID for primary keys)
    auto: bool = false,
    /// Field cannot be null/empty
    not_null: bool = false,
    /// Field must be unique across all records
    unique: bool = false,
    /// Create an index on this field
    index: bool = false,
    /// Default value as string (will be parsed based on type)
    default_value: ?[]const u8 = null,
    /// Foreign key reference
    foreign_key: ?ForeignKey = null,
    /// Action on delete of referenced record
    on_delete: ?OnDeleteAction = null,
    /// Automatically set on record creation
    audit_created: bool = false,
    /// Automatically set on record update
    audit_updated: bool = false,
};

/// Foreign key reference to another table
pub const ForeignKey = struct {
    /// Referenced table name
    table: []const u8,
    /// Referenced column name
    column: []const u8,
};

/// Action to take when a referenced record is deleted
pub const OnDeleteAction = enum {
    /// Delete all referencing records
    cascade,
    /// Set foreign key to null
    set_null,
    /// Prevent deletion (default)
    restrict,
};

/// Composite key definition (can span multiple fields)
pub const KeyDefinition = struct {
    /// Key name (e.g., "pk1", "by_customer")
    name: []const u8,
    /// Table this key belongs to
    table_name: []const u8,
    /// Field names in order (key segments)
    segments: []const []const u8,
    /// Is this the primary key?
    primary: bool = false,
    /// Is this a unique constraint?
    unique: bool = false,
    /// Key number for ISAM (access order)
    key_number: ?u8 = null,

    pub fn deinit(self: *KeyDefinition, allocator: std.mem.Allocator) void {
        // Segments slice is allocated, but segment strings point into source
        allocator.free(self.segments);
    }
};

/// SQL dialect for multi-backend support
pub const SqlDialect = enum {
    sqlite,
    postgres,
    turso,
};

/// Convert schema data type to SQL type string for a given dialect
pub fn dataTypeToSql(data_type: SchemaDataType, dialect: SqlDialect) []const u8 {
    return switch (data_type) {
        .text => |t| blk: {
            _ = t;
            break :blk switch (dialect) {
                .sqlite, .turso => "TEXT",
                .postgres => "VARCHAR", // Size added separately
            };
        },
        .integer => |i| switch (i) {
            .tiny, .small => switch (dialect) {
                .sqlite, .turso => "INTEGER",
                .postgres => "SMALLINT",
            },
            .medium => switch (dialect) {
                .sqlite, .turso => "INTEGER",
                .postgres => "INTEGER",
            },
            .big => switch (dialect) {
                .sqlite, .turso => "INTEGER",
                .postgres => "BIGINT",
            },
        },
        .decimal => switch (dialect) {
            .sqlite, .turso => "REAL",
            .postgres => "NUMERIC",
        },
        .boolean => switch (dialect) {
            .sqlite, .turso => "INTEGER",
            .postgres => "BOOLEAN",
        },
        .blob => switch (dialect) {
            .sqlite, .turso => "BLOB",
            .postgres => "BYTEA",
        },
        .datetime => |d| switch (d) {
            .date => switch (dialect) {
                .sqlite, .turso => "TEXT",
                .postgres => "DATE",
            },
            .time => switch (dialect) {
                .sqlite, .turso => "TEXT",
                .postgres => "TIME",
            },
            .datetime => switch (dialect) {
                .sqlite, .turso => "TEXT",
                .postgres => "TIMESTAMP",
            },
            .timestamp => switch (dialect) {
                .sqlite, .turso => "INTEGER",
                .postgres => "BIGINT",
            },
        },
        .ulid => switch (dialect) {
            .sqlite, .turso => "TEXT",
            .postgres => "CHAR(26)",
        },
        .reference => switch (dialect) {
            .sqlite, .turso => "TEXT",
            .postgres => "CHAR(26)",
        },
    };
}

/// Schema parsing error types
pub const SchemaError = error{
    /// Unexpected token during parsing
    UnexpectedToken,
    /// Missing required field
    MissingField,
    /// Invalid decorator syntax
    InvalidDecorator,
    /// Invalid data type specification
    InvalidDataType,
    /// Duplicate table name
    DuplicateTable,
    /// Duplicate field name within table
    DuplicateField,
    /// Invalid foreign key reference
    InvalidForeignKey,
    /// End of file reached unexpectedly
    UnexpectedEof,
    /// Memory allocation failed
    OutOfMemory,
    /// Invalid version number
    InvalidVersion,
    /// Missing @database directive
    MissingDatabase,
    /// Missing @version directive
    MissingVersion,
    /// Invalid default value for type
    InvalidDefaultValue,
};

test "field byte size calculation" {
    const field_int = FieldSchema{
        .name = "id",
        .data_type = .{ .integer = .medium },
        .decorators = .{},
    };
    try std.testing.expectEqual(@as(usize, 4), field_int.byteSize());

    const field_text = FieldSchema{
        .name = "name",
        .data_type = .{ .text = .{ .max_length = 30 } },
        .decorators = .{},
    };
    try std.testing.expectEqual(@as(usize, 30), field_text.byteSize());

    const field_big = FieldSchema{
        .name = "count",
        .data_type = .{ .integer = .big },
        .decorators = .{},
    };
    try std.testing.expectEqual(@as(usize, 8), field_big.byteSize());
}

test "data type to SQL conversion" {
    // SQLite
    try std.testing.expectEqualStrings("INTEGER", dataTypeToSql(.{ .integer = .medium }, .sqlite));
    try std.testing.expectEqualStrings("TEXT", dataTypeToSql(.{ .text = .{ .max_length = 30 } }, .sqlite));
    try std.testing.expectEqualStrings("REAL", dataTypeToSql(.{ .decimal = .{} }, .sqlite));

    // Postgres
    try std.testing.expectEqualStrings("INTEGER", dataTypeToSql(.{ .integer = .medium }, .postgres));
    try std.testing.expectEqualStrings("VARCHAR", dataTypeToSql(.{ .text = .{ .max_length = 30 } }, .postgres));
    try std.testing.expectEqualStrings("NUMERIC", dataTypeToSql(.{ .decimal = .{} }, .postgres));
}
