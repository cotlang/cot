//! Schema Diff Module
//!
//! Compares two schema versions and produces a diff that describes
//! the changes needed to migrate from one version to another.
//!
//! Supports detecting:
//! - Added/removed tables
//! - Added/removed/modified fields
//! - Changed field types or sizes
//! - Changed decorators (constraints, indexes)
//! - Added/removed composite keys

const std = @import("std");
const types = @import("types.zig");

const SchemaFile = types.SchemaFile;
const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const KeyDefinition = types.KeyDefinition;
const Decorators = types.Decorators;

/// Result of comparing two schema versions
pub const SchemaDiff = struct {
    /// Tables that exist in new schema but not in old
    added_tables: []const TableSchema,
    /// Table names that exist in old schema but not in new
    removed_tables: []const []const u8,
    /// Tables that exist in both but have changes
    modified_tables: []const TableDiff,
    /// Composite keys added
    added_keys: []const KeyDefinition,
    /// Composite key names removed (table.keyname format)
    removed_keys: []const []const u8,
    /// Whether there are any changes
    has_changes: bool,

    allocator: std.mem.Allocator,

    pub fn deinit(self: *SchemaDiff) void {
        self.allocator.free(self.added_tables);
        self.allocator.free(self.removed_tables);
        for (self.modified_tables) |*td| {
            var table_diff = td.*;
            table_diff.deinit(self.allocator);
        }
        self.allocator.free(self.modified_tables);
        self.allocator.free(self.added_keys);
        self.allocator.free(self.removed_keys);
    }
};

/// Changes to a single table
pub const TableDiff = struct {
    /// Table name
    name: []const u8,
    /// Fields added to this table
    added_fields: []const FieldSchema,
    /// Field names removed from this table
    removed_fields: []const []const u8,
    /// Fields with changed type or decorators
    modified_fields: []const FieldDiff,

    pub fn deinit(self: *TableDiff, allocator: std.mem.Allocator) void {
        allocator.free(self.added_fields);
        allocator.free(self.removed_fields);
        allocator.free(self.modified_fields);
    }

    pub fn hasChanges(self: *const TableDiff) bool {
        return self.added_fields.len > 0 or
            self.removed_fields.len > 0 or
            self.modified_fields.len > 0;
    }
};

/// Changes to a single field
pub const FieldDiff = struct {
    /// Field name
    name: []const u8,
    /// Old field definition
    old_field: FieldSchema,
    /// New field definition
    new_field: FieldSchema,
    /// What changed
    change_type: ChangeType,
};

/// Type of change detected
pub const ChangeType = enum {
    /// Data type changed (e.g., d6 -> d8, a30 -> a50)
    type_changed,
    /// Size increased (can be done with ALTER in most DBs)
    size_increased,
    /// Size decreased (requires data migration)
    size_decreased,
    /// Precision changed for decimals
    precision_changed,
    /// Decorators changed (constraints, indexes)
    decorators_changed,
    /// Multiple changes
    multiple_changes,
};

/// Compare two schema versions and produce a diff
pub fn diff(allocator: std.mem.Allocator, old_schema: *const SchemaFile, new_schema: *const SchemaFile) !SchemaDiff {
    var added_tables: std.ArrayListUnmanaged(TableSchema) = .empty;
    errdefer added_tables.deinit(allocator);

    var removed_tables: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer removed_tables.deinit(allocator);

    var modified_tables: std.ArrayListUnmanaged(TableDiff) = .empty;
    errdefer {
        for (modified_tables.items) |*td| {
            td.deinit(allocator);
        }
        modified_tables.deinit(allocator);
    }

    var added_keys: std.ArrayListUnmanaged(KeyDefinition) = .empty;
    errdefer added_keys.deinit(allocator);

    var removed_keys: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer removed_keys.deinit(allocator);

    // Find added and modified tables
    for (new_schema.tables) |new_table| {
        if (findTable(old_schema.tables, new_table.name)) |old_table| {
            // Table exists in both - check for modifications
            const table_diff = try diffTable(allocator, old_table, &new_table);
            if (table_diff.hasChanges()) {
                try modified_tables.append(allocator, table_diff);
            } else {
                // No changes, free the empty diff
                var td = table_diff;
                td.deinit(allocator);
            }
        } else {
            // Table is new
            try added_tables.append(allocator, new_table);
        }
    }

    // Find removed tables
    for (old_schema.tables) |old_table| {
        if (findTable(new_schema.tables, old_table.name) == null) {
            try removed_tables.append(allocator, old_table.name);
        }
    }

    // Find added keys
    for (new_schema.keys) |new_key| {
        if (findKey(old_schema.keys, new_key.table_name, new_key.name) == null) {
            try added_keys.append(allocator, new_key);
        }
    }

    // Find removed keys
    for (old_schema.keys) |old_key| {
        if (findKey(new_schema.keys, old_key.table_name, old_key.name) == null) {
            // Create table.keyname format
            const key_id = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ old_key.table_name, old_key.name });
            try removed_keys.append(allocator, key_id);
        }
    }

    const has_changes = added_tables.items.len > 0 or
        removed_tables.items.len > 0 or
        modified_tables.items.len > 0 or
        added_keys.items.len > 0 or
        removed_keys.items.len > 0;

    return SchemaDiff{
        .added_tables = try added_tables.toOwnedSlice(allocator),
        .removed_tables = try removed_tables.toOwnedSlice(allocator),
        .modified_tables = try modified_tables.toOwnedSlice(allocator),
        .added_keys = try added_keys.toOwnedSlice(allocator),
        .removed_keys = try removed_keys.toOwnedSlice(allocator),
        .has_changes = has_changes,
        .allocator = allocator,
    };
}

/// Compare two tables and produce a diff
fn diffTable(allocator: std.mem.Allocator, old_table: *const TableSchema, new_table: *const TableSchema) !TableDiff {
    var added_fields: std.ArrayListUnmanaged(FieldSchema) = .empty;
    errdefer added_fields.deinit(allocator);

    var removed_fields: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer removed_fields.deinit(allocator);

    var modified_fields: std.ArrayListUnmanaged(FieldDiff) = .empty;
    errdefer modified_fields.deinit(allocator);

    // Find added and modified fields
    for (new_table.fields) |new_field| {
        if (findField(old_table.fields, new_field.name)) |old_field| {
            // Field exists in both - check for modifications
            if (try diffField(old_field, &new_field)) |field_diff| {
                try modified_fields.append(allocator, field_diff);
            }
        } else {
            // Field is new
            try added_fields.append(allocator, new_field);
        }
    }

    // Find removed fields
    for (old_table.fields) |old_field| {
        if (findField(new_table.fields, old_field.name) == null) {
            try removed_fields.append(allocator, old_field.name);
        }
    }

    return TableDiff{
        .name = new_table.name,
        .added_fields = try added_fields.toOwnedSlice(allocator),
        .removed_fields = try removed_fields.toOwnedSlice(allocator),
        .modified_fields = try modified_fields.toOwnedSlice(allocator),
    };
}

/// Compare two fields and return a diff if they're different
fn diffField(old_field: *const FieldSchema, new_field: *const FieldSchema) !?FieldDiff {
    var changes: u32 = 0;
    var change_type: ChangeType = .decorators_changed;

    // Compare data types
    const type_changed = !dataTypesEqual(old_field.data_type, new_field.data_type);
    if (type_changed) {
        changes += 1;
        change_type = categorizeTypeChange(old_field.data_type, new_field.data_type);
    }

    // Compare decorators
    const decorators_changed = !decoratorsEqual(&old_field.decorators, &new_field.decorators);
    if (decorators_changed) {
        changes += 1;
        if (change_type != .type_changed and change_type != .size_increased and change_type != .size_decreased) {
            change_type = .decorators_changed;
        }
    }

    if (changes == 0) {
        return null;
    }

    if (changes > 1) {
        change_type = .multiple_changes;
    }

    return FieldDiff{
        .name = new_field.name,
        .old_field = old_field.*,
        .new_field = new_field.*,
        .change_type = change_type,
    };
}

/// Categorize the type of data type change
fn categorizeTypeChange(old_type: types.SchemaDataType, new_type: types.SchemaDataType) ChangeType {
    // Compare sizes for same base type
    switch (old_type) {
        .text => |old_t| {
            if (new_type == .text) {
                const new_t = new_type.text;
                const old_len = old_t.max_length orelse 0;
                const new_len = new_t.max_length orelse 0;
                if (new_len > old_len) return .size_increased;
                if (new_len < old_len and new_len > 0) return .size_decreased;
            }
        },
        .decimal => |old_d| {
            if (new_type == .decimal) {
                const new_d = new_type.decimal;
                if (old_d.scale != new_d.scale) return .precision_changed;
                if (new_d.precision > old_d.precision) return .size_increased;
                if (new_d.precision < old_d.precision) return .size_decreased;
            }
        },
        .blob => |old_b| {
            if (new_type == .blob) {
                const new_b = new_type.blob;
                const old_size = old_b.max_size orelse 0;
                const new_size = new_b.max_size orelse 0;
                if (new_size > old_size) return .size_increased;
                if (new_size < old_size and new_size > 0) return .size_decreased;
            }
        },
        else => {},
    }

    return .type_changed;
}

/// Check if two data types are equal
fn dataTypesEqual(a: types.SchemaDataType, b: types.SchemaDataType) bool {
    const TagType = @typeInfo(types.SchemaDataType).@"union".tag_type.?;
    const a_tag = @as(TagType, a);
    const b_tag = @as(TagType, b);

    if (a_tag != b_tag) return false;

    return switch (a) {
        .text => |at| blk: {
            const bt = b.text;
            break :blk (at.max_length orelse 0) == (bt.max_length orelse 0);
        },
        .integer => |ai| b.integer == ai,
        .decimal => |ad| blk: {
            const bd = b.decimal;
            break :blk ad.precision == bd.precision and ad.scale == bd.scale;
        },
        .boolean => true,
        .blob => |ab| blk: {
            const bb = b.blob;
            break :blk (ab.max_size orelse 0) == (bb.max_size orelse 0);
        },
        .datetime => |adt| b.datetime == adt,
        .ulid => true,
        .reference => |ar| std.mem.eql(u8, ar, b.reference),
    };
}

/// Check if two decorator sets are equal
fn decoratorsEqual(a: *const Decorators, b: *const Decorators) bool {
    if (a.primary != b.primary) return false;
    if (a.auto != b.auto) return false;
    if (a.not_null != b.not_null) return false;
    if (a.unique != b.unique) return false;
    if (a.index != b.index) return false;
    if (a.audit_created != b.audit_created) return false;
    if (a.audit_updated != b.audit_updated) return false;

    // Compare default values
    if (a.default_value == null and b.default_value != null) return false;
    if (a.default_value != null and b.default_value == null) return false;
    if (a.default_value != null and b.default_value != null) {
        if (!std.mem.eql(u8, a.default_value.?, b.default_value.?)) return false;
    }

    // Compare foreign keys
    if (a.foreign_key == null and b.foreign_key != null) return false;
    if (a.foreign_key != null and b.foreign_key == null) return false;
    if (a.foreign_key != null and b.foreign_key != null) {
        if (!std.mem.eql(u8, a.foreign_key.?.table, b.foreign_key.?.table)) return false;
        if (!std.mem.eql(u8, a.foreign_key.?.column, b.foreign_key.?.column)) return false;
    }

    // Compare on_delete
    if (a.on_delete != b.on_delete) return false;

    return true;
}

/// Find a table by name in a slice of tables
fn findTable(tables: []const TableSchema, name: []const u8) ?*const TableSchema {
    for (tables) |*table| {
        if (std.mem.eql(u8, table.name, name)) {
            return table;
        }
    }
    return null;
}

/// Find a field by name in a slice of fields
fn findField(fields: []const FieldSchema, name: []const u8) ?*const FieldSchema {
    for (fields) |*field| {
        if (std.mem.eql(u8, field.name, name)) {
            return field;
        }
    }
    return null;
}

/// Find a key by table and key name
fn findKey(keys: []const KeyDefinition, table_name: []const u8, key_name: []const u8) ?*const KeyDefinition {
    for (keys) |*key| {
        if (std.mem.eql(u8, key.table_name, table_name) and std.mem.eql(u8, key.name, key_name)) {
            return key;
        }
    }
    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "diff detects added table" {
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
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    try std.testing.expect(schema_diff.has_changes);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.added_tables.len);
    try std.testing.expectEqualStrings("order", schema_diff.added_tables[0].name);
    try std.testing.expectEqual(@as(usize, 0), schema_diff.removed_tables.len);
}

test "diff detects removed table" {
    const parser = @import("parser.zig");

    const old_source =
        \\@database test
        \\@version 1
        \\@table customer
        \\    id ,d6 @primary
        \\@endtable
        \\@table order
        \\    id ,d8 @primary
        \\@endtable
    ;

    const new_source =
        \\@database test
        \\@version 2
        \\@table customer
        \\    id ,d6 @primary
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    try std.testing.expect(schema_diff.has_changes);
    try std.testing.expectEqual(@as(usize, 0), schema_diff.added_tables.len);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.removed_tables.len);
    try std.testing.expectEqualStrings("order", schema_diff.removed_tables[0]);
}

test "diff detects added field" {
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
        \\    name ,a30
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    try std.testing.expect(schema_diff.has_changes);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.modified_tables.len);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.modified_tables[0].added_fields.len);
    try std.testing.expectEqualStrings("name", schema_diff.modified_tables[0].added_fields[0].name);
}

test "diff detects field size change" {
    const parser = @import("parser.zig");

    const old_source =
        \\@database test
        \\@version 1
        \\@table customer
        \\    id ,d6 @primary
        \\    name ,a30
        \\@endtable
    ;

    const new_source =
        \\@database test
        \\@version 2
        \\@table customer
        \\    id ,d6 @primary
        \\    name ,a50
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, old_source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, new_source);
    defer new_schema.deinit();

    var schema_diff = try diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    try std.testing.expect(schema_diff.has_changes);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.modified_tables.len);
    try std.testing.expectEqual(@as(usize, 1), schema_diff.modified_tables[0].modified_fields.len);
    try std.testing.expectEqual(ChangeType.size_increased, schema_diff.modified_tables[0].modified_fields[0].change_type);
}

test "diff detects no changes" {
    const parser = @import("parser.zig");

    const source =
        \\@database test
        \\@version 1
        \\@table customer
        \\    id ,d6 @primary
        \\    name ,a30
        \\@endtable
    ;

    var old_schema = try parser.parseSchema(std.testing.allocator, source);
    defer old_schema.deinit();

    var new_schema = try parser.parseSchema(std.testing.allocator, source);
    defer new_schema.deinit();

    var schema_diff = try diff(std.testing.allocator, &old_schema, &new_schema);
    defer schema_diff.deinit();

    try std.testing.expect(!schema_diff.has_changes);
}
