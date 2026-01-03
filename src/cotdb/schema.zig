//! CotDB Schema - Self-describing database metadata
//!
//! The schema system provides rich column metadata for:
//! - Runtime introspection of database structure
//! - Query validation and type checking
//! - CLI display formatting
//! - .NET/external interop with auto-generated types
//! - Multi-table support with tag byte discrimination

const std = @import("std");

/// Maximum lengths for names
pub const MAX_NAME_LEN: usize = 64;
pub const MAX_COLUMNS_PER_TABLE: usize = 256;
pub const MAX_TABLES_PER_SCHEMA: usize = 64;

/// Column data types
pub const ColumnType = enum(u8) {
    // Core types
    string = 0, // Fixed-length string (space-padded)
    decimal = 1, // Fixed-point decimal number
    integer = 2, // Binary integer (signed)
    binary_packed = 3, // Packed binary format

    // Extended types
    date = 10, // Date stored as YYYYMMDD
    time = 11, // Time stored as HHMMSS
    datetime = 12, // Combined date/time
    boolean = 13, // Single byte 0/1
    binary = 14, // Raw bytes
    uuid = 15, // 16-byte UUID
    json = 16, // JSON text blob
    array = 17, // Array of values (future)

    pub fn toSql(self: ColumnType) []const u8 {
        return switch (self) {
            .string => "TEXT",
            .decimal => "DECIMAL",
            .integer => "INTEGER",
            .binary_packed => "DECIMAL",
            .date => "DATE",
            .time => "TIME",
            .datetime => "DATETIME",
            .boolean => "BOOLEAN",
            .binary => "BLOB",
            .uuid => "UUID",
            .json => "JSON",
            .array => "ARRAY",
        };
    }

    pub fn toZig(self: ColumnType) []const u8 {
        return switch (self) {
            .string => "[]const u8",
            .decimal, .binary_packed => "i64",
            .integer => "i64",
            .date, .time, .datetime => "i64",
            .boolean => "bool",
            .binary => "[]const u8",
            .uuid => "[16]u8",
            .json => "[]const u8",
            .array => "[]const Value",
        };
    }
};

/// Column constraints
pub const Constraints = packed struct(u16) {
    /// Column is part of primary key
    primary_key: bool = false,
    /// Column is unique (no duplicates)
    unique: bool = false,
    /// Column cannot be null/empty
    not_null: bool = false,
    /// Column has a default value
    has_default: bool = false,
    /// Column is auto-increment
    auto_increment: bool = false,
    /// Column is indexed
    indexed: bool = false,
    /// Column is a foreign key
    foreign_key: bool = false,
    /// Reserved
    _reserved: u9 = 0,
};

/// Column definition with rich metadata
pub const ColumnDef = struct {
    /// Column name (e.g., "customer_id")
    name: []const u8,

    /// Data type
    column_type: ColumnType,

    /// Start position in record (0-based)
    position: u32,

    /// Length in bytes
    length: u32,

    /// Decimal places (for decimal types)
    precision: u8,

    /// Constraints
    constraints: Constraints,

    /// Display format (e.g., "###,###.##" for currency)
    format: ?[]const u8,

    /// Description/comment
    description: ?[]const u8,

    /// Default value (as string representation)
    default_value: ?[]const u8,

    /// Reference for foreign keys: "table.column"
    references: ?[]const u8,

    /// Check if this column is a key column
    pub fn isKey(self: ColumnDef) bool {
        return self.constraints.primary_key or self.constraints.indexed;
    }

    /// Get SQL-style column definition
    pub fn toSqlDef(self: ColumnDef, allocator: std.mem.Allocator) ![]u8 {
        var parts: std.ArrayListAligned(u8, null) = .empty;
        defer parts.deinit(allocator);

        const writer = parts.writer(allocator);
        try writer.print("{s} {s}", .{ self.name, self.column_type.toSql() });

        if (self.column_type == .decimal or self.column_type == .packed_decimal) {
            try writer.print("({d},{d})", .{ self.length, self.precision });
        } else if (self.column_type == .alpha) {
            try writer.print("({d})", .{self.length});
        }

        if (self.constraints.primary_key) try writer.writeAll(" PRIMARY KEY");
        if (self.constraints.not_null) try writer.writeAll(" NOT NULL");
        if (self.constraints.unique) try writer.writeAll(" UNIQUE");
        if (self.default_value) |def| try writer.print(" DEFAULT '{s}'", .{def});

        return try parts.toOwnedSlice(allocator);
    }

    /// Serialize to bytes
    pub fn serialize(self: ColumnDef, writer: anytype) !void {
        // Name length + name
        const name_len: u8 = @intCast(self.name.len);
        try writer.writeByte(name_len);
        try writer.writeAll(self.name);

        // Type
        try writer.writeByte(@intFromEnum(self.column_type));

        // Position, length, precision
        try writer.writeInt(u32, self.position, .little);
        try writer.writeInt(u32, self.length, .little);
        try writer.writeByte(self.precision);

        // Constraints
        try writer.writeInt(u16, @as(u16, @bitCast(self.constraints)), .little);

        // Optional fields (use 0xFF as "none" marker for lengths)
        if (self.format) |fmt| {
            try writer.writeByte(@intCast(fmt.len));
            try writer.writeAll(fmt);
        } else {
            try writer.writeByte(0);
        }

        if (self.description) |desc| {
            const desc_len: u16 = @intCast(desc.len);
            try writer.writeInt(u16, desc_len, .little);
            try writer.writeAll(desc);
        } else {
            try writer.writeInt(u16, 0, .little);
        }

        if (self.default_value) |def| {
            try writer.writeByte(@intCast(def.len));
            try writer.writeAll(def);
        } else {
            try writer.writeByte(0);
        }

        if (self.references) |ref| {
            try writer.writeByte(@intCast(ref.len));
            try writer.writeAll(ref);
        } else {
            try writer.writeByte(0);
        }
    }

    /// Deserialize from bytes
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !ColumnDef {
        // Name
        const name_len = try reader.readByte();
        const name = try allocator.alloc(u8, name_len);
        _ = try reader.readAll(name);

        // Type
        const type_byte = try reader.readByte();
        const column_type: ColumnType = @enumFromInt(type_byte);

        // Position, length, precision
        const position = try reader.readInt(u32, .little);
        const length = try reader.readInt(u32, .little);
        const precision = try reader.readByte();

        // Constraints
        const constraints_int = try reader.readInt(u16, .little);
        const constraints: Constraints = @bitCast(constraints_int);

        // Format
        const fmt_len = try reader.readByte();
        const format: ?[]const u8 = if (fmt_len > 0) blk: {
            const fmt = try allocator.alloc(u8, fmt_len);
            _ = try reader.readAll(fmt);
            break :blk fmt;
        } else null;

        // Description
        const desc_len = try reader.readInt(u16, .little);
        const description: ?[]const u8 = if (desc_len > 0) blk: {
            const desc = try allocator.alloc(u8, desc_len);
            _ = try reader.readAll(desc);
            break :blk desc;
        } else null;

        // Default value
        const def_len = try reader.readByte();
        const default_value: ?[]const u8 = if (def_len > 0) blk: {
            const def = try allocator.alloc(u8, def_len);
            _ = try reader.readAll(def);
            break :blk def;
        } else null;

        // References
        const ref_len = try reader.readByte();
        const references: ?[]const u8 = if (ref_len > 0) blk: {
            const ref = try allocator.alloc(u8, ref_len);
            _ = try reader.readAll(ref);
            break :blk ref;
        } else null;

        return ColumnDef{
            .name = name,
            .column_type = column_type,
            .position = position,
            .length = length,
            .precision = precision,
            .constraints = constraints,
            .format = format,
            .description = description,
            .default_value = default_value,
            .references = references,
        };
    }

    pub fn deinit(self: *ColumnDef, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.format) |f| allocator.free(f);
        if (self.description) |d| allocator.free(d);
        if (self.default_value) |d| allocator.free(d);
        if (self.references) |r| allocator.free(r);
    }
};

/// Table definition
pub const TableDef = struct {
    /// Table name
    name: []const u8,

    /// Tag byte for multi-table files (0 for single-table)
    tag: u8,

    /// Total record size
    record_size: u32,

    /// Column definitions
    columns: []ColumnDef,

    /// Table description
    description: ?[]const u8,

    /// Table flags
    flags: TableFlags,

    pub const TableFlags = packed struct(u8) {
        is_default: bool = false,
        read_only: bool = false,
        _reserved: u6 = 0,
    };

    /// Get column by name
    pub fn getColumn(self: TableDef, name: []const u8) ?*const ColumnDef {
        for (self.columns) |*col| {
            if (std.mem.eql(u8, col.name, name)) {
                return col;
            }
        }
        return null;
    }

    /// Get primary key columns
    pub fn getPrimaryKeyColumns(self: TableDef, allocator: std.mem.Allocator) ![]const *const ColumnDef {
        var keys = std.ArrayList(*const ColumnDef).init(allocator);
        for (self.columns) |*col| {
            if (col.constraints.primary_key) {
                try keys.append(col);
            }
        }
        return try keys.toOwnedSlice();
    }

    /// Serialize table definition
    pub fn serialize(self: TableDef, writer: anytype) !void {
        // Name
        const name_len: u8 = @intCast(self.name.len);
        try writer.writeByte(name_len);
        try writer.writeAll(self.name);

        // Tag, record size, flags
        try writer.writeByte(self.tag);
        try writer.writeInt(u32, self.record_size, .little);
        try writer.writeByte(@as(u8, @bitCast(self.flags)));

        // Description
        if (self.description) |desc| {
            const desc_len: u16 = @intCast(desc.len);
            try writer.writeInt(u16, desc_len, .little);
            try writer.writeAll(desc);
        } else {
            try writer.writeInt(u16, 0, .little);
        }

        // Columns
        const col_count: u16 = @intCast(self.columns.len);
        try writer.writeInt(u16, col_count, .little);
        for (self.columns) |col| {
            try col.serialize(writer);
        }
    }

    /// Deserialize table definition
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !TableDef {
        // Name
        const name_len = try reader.readByte();
        const name = try allocator.alloc(u8, name_len);
        _ = try reader.readAll(name);

        // Tag, record size, flags
        const tag = try reader.readByte();
        const record_size = try reader.readInt(u32, .little);
        const flags_byte = try reader.readByte();
        const flags: TableDef.TableFlags = @bitCast(flags_byte);

        // Description
        const desc_len = try reader.readInt(u16, .little);
        const description: ?[]const u8 = if (desc_len > 0) blk: {
            const desc = try allocator.alloc(u8, desc_len);
            _ = try reader.readAll(desc);
            break :blk desc;
        } else null;

        // Columns
        const col_count = try reader.readInt(u16, .little);
        const columns = try allocator.alloc(ColumnDef, col_count);
        for (columns) |*col| {
            col.* = try ColumnDef.deserialize(reader, allocator);
        }

        return TableDef{
            .name = name,
            .tag = tag,
            .record_size = record_size,
            .columns = columns,
            .description = description,
            .flags = flags,
        };
    }

    pub fn deinit(self: *TableDef, allocator: std.mem.Allocator) void {
        for (self.columns) |*col| {
            col.deinit(allocator);
        }
        allocator.free(self.columns);
        allocator.free(self.name);
        if (self.description) |d| allocator.free(d);
    }
};

/// Database schema
pub const Schema = struct {
    /// Schema version
    version: u32,

    /// Database description
    description: []const u8,

    /// Tag byte position (0xFFFFFFFF if no tag)
    tag_position: u32,

    /// Tables
    tables: []TableDef,

    /// Schema flags
    flags: SchemaFlags,

    pub const SchemaFlags = packed struct(u16) {
        modified: bool = false,
        strict_types: bool = true,
        _reserved: u14 = 0,
    };

    pub const MAGIC = [4]u8{ 'C', 'D', 'B', 'S' }; // CotDB Schema
    pub const CURRENT_VERSION: u32 = 1;
    pub const NO_TAG: u32 = 0xFFFFFFFF;

    /// Check if this is a multi-table schema
    pub fn isMultiTable(self: Schema) bool {
        return self.tag_position != NO_TAG and self.tables.len > 1;
    }

    /// Get table by tag
    pub fn getTableByTag(self: Schema, tag: u8) ?*const TableDef {
        for (self.tables) |*table| {
            if (table.tag == tag) return table;
        }
        for (self.tables) |*table| {
            if (table.flags.is_default) return table;
        }
        return null;
    }

    /// Get table by name
    pub fn getTableByName(self: Schema, name: []const u8) ?*const TableDef {
        for (self.tables) |*table| {
            if (std.mem.eql(u8, table.name, name)) return table;
        }
        return null;
    }

    /// Get the default table
    pub fn getDefaultTable(self: Schema) ?*const TableDef {
        if (self.tables.len == 0) return null;
        for (self.tables) |*table| {
            if (table.flags.is_default) return table;
        }
        if (self.tables.len == 1) return &self.tables[0];
        return null;
    }

    /// Calculate serialized size in bytes
    pub fn serializedSize(self: *const Schema) u64 {
        // Magic (4) + version (4) + desc_len (2) + description + tag_pos (4) + flags (2) + table_count (2)
        var size: u64 = 4 + 4 + 2 + self.description.len + 4 + 2 + 2;

        for (self.tables) |table| {
            // name_len (1) + name + tag (1) + record_size (4) + flags (1) + desc_len (2) + description + col_count (2)
            size += 1 + table.name.len + 1 + 4 + 1 + 2;
            if (table.description) |d| {
                size += d.len;
            }
            size += 2; // column count

            for (table.columns) |col| {
                // name_len (1) + name + type (1) + position (4) + length (4) + precision (1) + constraints (2)
                size += 1 + col.name.len + 1 + 4 + 4 + 1 + 2;
                // format_len (1) + format
                size += 1;
                if (col.format) |f| size += f.len;
                // desc_len (2) + description
                size += 2;
                if (col.description) |d| size += d.len;
                // default_len (1) + default
                size += 1;
                if (col.default_value) |d| size += d.len;
                // ref_len (1) + references
                size += 1;
                if (col.references) |r| size += r.len;
            }
        }

        return size;
    }

    /// Serialize schema
    pub fn serialize(self: Schema, writer: anytype) !void {
        try writer.writeAll(&MAGIC);
        try writer.writeInt(u32, self.version, .little);

        const desc_len: u16 = @intCast(self.description.len);
        try writer.writeInt(u16, desc_len, .little);
        try writer.writeAll(self.description);

        try writer.writeInt(u32, self.tag_position, .little);
        try writer.writeInt(u16, @as(u16, @bitCast(self.flags)), .little);

        const table_count: u16 = @intCast(self.tables.len);
        try writer.writeInt(u16, table_count, .little);
        for (self.tables) |table| {
            try table.serialize(writer);
        }
    }

    /// Deserialize schema
    pub fn deserialize(reader: anytype, allocator: std.mem.Allocator) !*Schema {
        var magic: [4]u8 = undefined;
        _ = try reader.readAll(&magic);
        if (!std.mem.eql(u8, &magic, &MAGIC)) {
            return error.InvalidSchema;
        }

        const version = try reader.readInt(u32, .little);
        const desc_len = try reader.readInt(u16, .little);
        const description = try allocator.alloc(u8, desc_len);
        _ = try reader.readAll(description);

        const tag_position = try reader.readInt(u32, .little);
        const flags_int = try reader.readInt(u16, .little);
        const flags: SchemaFlags = @bitCast(flags_int);

        const table_count = try reader.readInt(u16, .little);
        const tables = try allocator.alloc(TableDef, table_count);
        for (tables) |*table| {
            table.* = try TableDef.deserialize(reader, allocator);
        }

        const schema = try allocator.create(Schema);
        schema.* = .{
            .version = version,
            .description = description,
            .tag_position = tag_position,
            .tables = tables,
            .flags = flags,
        };
        return schema;
    }

    pub fn deinit(self: *Schema, allocator: std.mem.Allocator) void {
        if (self.description.len > 0) allocator.free(self.description);
        for (self.tables) |*table| {
            table.deinit(allocator);
        }
        if (self.tables.len > 0) allocator.free(self.tables);
        allocator.destroy(self);
    }

    /// Create a schema builder
    pub fn builder(allocator: std.mem.Allocator) SchemaBuilder {
        return SchemaBuilder.init(allocator);
    }
};

/// Fluent builder for schemas
pub const SchemaBuilder = struct {
    allocator: std.mem.Allocator,
    description: []const u8,
    tag_position: u32,
    tables: std.ArrayListUnmanaged(TableDef),

    pub fn init(allocator: std.mem.Allocator) SchemaBuilder {
        return .{
            .allocator = allocator,
            .description = "",
            .tag_position = Schema.NO_TAG,
            .tables = .{},
        };
    }

    pub fn describe(self: *SchemaBuilder, desc: []const u8) *SchemaBuilder {
        self.description = desc;
        return self;
    }

    pub fn withTagAt(self: *SchemaBuilder, pos: u32) *SchemaBuilder {
        self.tag_position = pos;
        return self;
    }

    pub fn addTable(self: *SchemaBuilder, name: []const u8) *TableBuilder {
        const tb = self.allocator.create(TableBuilder) catch @panic("OOM");
        tb.* = TableBuilder.init(self.allocator, name, self);
        return tb;
    }

    pub fn addTableDef(self: *SchemaBuilder, table: TableDef) !*SchemaBuilder {
        try self.tables.append(self.allocator, table);
        return self;
    }

    pub fn build(self: *SchemaBuilder) !*Schema {
        const schema = try self.allocator.create(Schema);
        schema.* = .{
            .version = Schema.CURRENT_VERSION,
            .description = if (self.description.len > 0)
                try self.allocator.dupe(u8, self.description)
            else
                "",
            .tag_position = self.tag_position,
            .tables = try self.tables.toOwnedSlice(self.allocator),
            .flags = .{},
        };
        return schema;
    }

    pub fn deinit(self: *SchemaBuilder) void {
        self.tables.deinit(self.allocator);
    }
};

/// Fluent builder for tables
pub const TableBuilder = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    tag: u8,
    columns: std.ArrayListUnmanaged(ColumnDef),
    description: ?[]const u8,
    is_default: bool,
    parent: *SchemaBuilder,
    current_position: u32,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, parent: *SchemaBuilder) TableBuilder {
        return .{
            .allocator = allocator,
            .name = name,
            .tag = 0,
            .columns = .{},
            .description = null,
            .is_default = false,
            .parent = parent,
            .current_position = 0,
        };
    }

    pub fn withTag(self: *TableBuilder, tag: u8) *TableBuilder {
        self.tag = tag;
        return self;
    }

    pub fn asDefault(self: *TableBuilder) *TableBuilder {
        self.is_default = true;
        return self;
    }

    pub fn describe(self: *TableBuilder, desc: []const u8) *TableBuilder {
        self.description = desc;
        return self;
    }

    /// Add a simple column
    pub fn column(self: *TableBuilder, name: []const u8, col_type: ColumnType, length: u32) *TableBuilder {
        const col = ColumnDef{
            .name = self.allocator.dupe(u8, name) catch @panic("OOM"),
            .column_type = col_type,
            .position = self.current_position,
            .length = length,
            .precision = 0,
            .constraints = .{},
            .format = null,
            .description = null,
            .default_value = null,
            .references = null,
        };
        self.columns.append(self.allocator, col) catch @panic("OOM");
        self.current_position += length;
        return self;
    }

    /// Add a decimal column with precision
    pub fn decimal(self: *TableBuilder, name: []const u8, length: u32, precision: u8) *TableBuilder {
        const col = ColumnDef{
            .name = self.allocator.dupe(u8, name) catch @panic("OOM"),
            .column_type = .decimal,
            .position = self.current_position,
            .length = length,
            .precision = precision,
            .constraints = .{},
            .format = null,
            .description = null,
            .default_value = null,
            .references = null,
        };
        self.columns.append(self.allocator, col) catch @panic("OOM");
        self.current_position += length;
        return self;
    }

    /// Add a primary key column
    pub fn primaryKey(self: *TableBuilder, name: []const u8, col_type: ColumnType, length: u32) *TableBuilder {
        const col = ColumnDef{
            .name = self.allocator.dupe(u8, name) catch @panic("OOM"),
            .column_type = col_type,
            .position = self.current_position,
            .length = length,
            .precision = 0,
            .constraints = .{ .primary_key = true, .not_null = true },
            .format = null,
            .description = null,
            .default_value = null,
            .references = null,
        };
        self.columns.append(self.allocator, col) catch @panic("OOM");
        self.current_position += length;
        return self;
    }

    /// Finish table and return to schema builder
    pub fn done(self: *TableBuilder) *SchemaBuilder {
        // Calculate record size
        var max_end: u32 = 0;
        for (self.columns.items) |col| {
            const end = col.position + col.length;
            if (end > max_end) max_end = end;
        }

        const table = TableDef{
            .name = self.allocator.dupe(u8, self.name) catch @panic("OOM"),
            .tag = self.tag,
            .record_size = max_end,
            .columns = self.columns.toOwnedSlice(self.allocator) catch @panic("OOM"),
            .description = if (self.description) |d|
                self.allocator.dupe(u8, d) catch @panic("OOM")
            else
                null,
            .flags = .{ .is_default = self.is_default },
        };

        const parent = self.parent;
        parent.tables.append(self.allocator, table) catch @panic("OOM");

        // Free the TableBuilder itself
        self.allocator.destroy(self);

        return parent;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "schema builder fluent API" {
    const allocator = std.testing.allocator;

    var builder = Schema.builder(allocator);
    defer builder.deinit();

    const schema = try builder
        .describe("Customer database")
        .addTable("customers")
        .primaryKey("id", .alpha, 8)
        .column("name", .alpha, 50)
        .decimal("balance", 10, 2)
        .describe("Main customer table")
        .done()
        .build();
    defer schema.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), schema.tables.len);
    try std.testing.expectEqual(@as(u32, 68), schema.tables[0].record_size);
    try std.testing.expectEqual(@as(usize, 3), schema.tables[0].columns.len);

    const id_col = schema.tables[0].getColumn("id");
    try std.testing.expect(id_col != null);
    try std.testing.expect(id_col.?.constraints.primary_key);
}

test "column SQL generation" {
    const allocator = std.testing.allocator;

    const col = ColumnDef{
        .name = "balance",
        .column_type = .decimal,
        .position = 0,
        .length = 10,
        .precision = 2,
        .constraints = .{ .not_null = true },
        .format = null,
        .description = null,
        .default_value = "0.00",
        .references = null,
    };

    const sql = try col.toSqlDef(allocator);
    defer allocator.free(sql);

    try std.testing.expectEqualStrings("balance DECIMAL(10,2) NOT NULL DEFAULT '0.00'", sql);
}
