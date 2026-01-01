//! Foreign Key Validation
//!
//! Validates foreign key constraints before storing or updating records.
//! Ensures referential integrity by checking that referenced records exist.
//!
//! Usage:
//!   - Call validateForeignKeys() before store/write operations
//!   - Returns error.ForeignKeyViolation if any constraint fails

const std = @import("std");
const types = @import("types.zig");

const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;
const ForeignKey = types.ForeignKey;
const OnDeleteAction = types.OnDeleteAction;

/// Foreign key validation error details
pub const ForeignKeyError = struct {
    /// Field that has the foreign key constraint
    field_name: []const u8,
    /// The value that failed validation
    value: []const u8,
    /// Referenced table
    referenced_table: []const u8,
    /// Referenced column
    referenced_column: []const u8,
};

/// Result of foreign key validation
pub const ValidationResult = struct {
    /// Whether all constraints passed
    valid: bool,
    /// Number of foreign keys checked
    checked_count: usize,
    /// Details of the first failed constraint (if any)
    error_details: ?ForeignKeyError,
};

/// Database interface for foreign key lookups
/// This allows the validation to work with different database backends
pub const ForeignKeyLookup = struct {
    /// Context pointer (e.g., SqliteIsam pointer)
    ctx: *anyopaque,
    /// Function to check if a value exists in a table/column
    /// Returns true if the value exists
    existsFn: *const fn (ctx: *anyopaque, table: []const u8, column: []const u8, value: []const u8) bool,

    pub fn exists(self: *const ForeignKeyLookup, table: []const u8, column: []const u8, value: []const u8) bool {
        return self.existsFn(self.ctx, table, column, value);
    }
};

/// Validate all foreign key constraints for a record
///
/// Args:
///   record: The record buffer to validate
///   table_schema: Schema defining the table structure
///   lookup: Interface for checking referenced records
///
/// Returns:
///   ValidationResult indicating whether all constraints passed
pub fn validateForeignKeys(
    record: []const u8,
    table_schema: *const TableSchema,
    lookup: *const ForeignKeyLookup,
) ValidationResult {
    var result = ValidationResult{
        .valid = true,
        .checked_count = 0,
        .error_details = null,
    };

    var offset: usize = 0;
    for (table_schema.fields) |field| {
        const field_size = field.byteSize();

        if (field.decorators.foreign_key) |fk| {
            // Extract the field value from the record
            if (offset + field_size <= record.len) {
                const value = record[offset..][0..field_size];
                const trimmed_value = std.mem.trimRight(u8, value, " \x00");

                // Skip validation if the field is empty (null foreign key)
                if (trimmed_value.len > 0 and !isAllZeros(trimmed_value)) {
                    result.checked_count += 1;

                    // Check if the referenced record exists
                    if (!lookup.exists(fk.table, fk.column, trimmed_value)) {
                        result.valid = false;
                        result.error_details = .{
                            .field_name = field.name,
                            .value = trimmed_value,
                            .referenced_table = fk.table,
                            .referenced_column = fk.column,
                        };
                        return result;
                    }
                }
            }
        }

        offset += field_size;
    }

    return result;
}

/// Check if a value is all zeros (empty numeric field)
fn isAllZeros(value: []const u8) bool {
    for (value) |byte| {
        if (byte != '0' and byte != 0) {
            return false;
        }
    }
    return true;
}

/// Extract the value of a specific field from a record buffer
pub fn extractFieldValue(
    record: []const u8,
    table_schema: *const TableSchema,
    field_name: []const u8,
) ?[]const u8 {
    var offset: usize = 0;
    for (table_schema.fields) |field| {
        const field_size = field.byteSize();

        if (std.mem.eql(u8, field.name, field_name)) {
            if (offset + field_size <= record.len) {
                return record[offset..][0..field_size];
            }
            return null;
        }

        offset += field_size;
    }
    return null;
}

/// Get all foreign key fields from a table schema
pub fn getForeignKeyFields(table_schema: *const TableSchema) []const FieldSchema {
    // This returns a view, not an allocated slice
    // Caller can iterate and check decorators.foreign_key != null
    return table_schema.fields;
}

/// Check if a table has any foreign key constraints
pub fn hasForeignKeys(table_schema: *const TableSchema) bool {
    for (table_schema.fields) |field| {
        if (field.decorators.foreign_key != null) {
            return true;
        }
    }
    return false;
}

/// Get the on_delete action for a foreign key field
pub fn getOnDeleteAction(field: *const FieldSchema) OnDeleteAction {
    return field.decorators.on_delete orelse .restrict;
}

/// Build a SQL query to check foreign key existence
/// Caller must free the returned string
pub fn buildExistenceQuery(
    allocator: std.mem.Allocator,
    table: []const u8,
    column: []const u8,
) ![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        "SELECT 1 FROM \"{s}\" WHERE \"{s}\" = ? LIMIT 1",
        .{ table, column },
    );
}

/// Build a SQL query to find referencing records (for cascade/restrict checks)
/// Caller must free the returned string
pub fn buildReferencingQuery(
    allocator: std.mem.Allocator,
    referencing_table: []const u8,
    referencing_column: []const u8,
) ![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        "SELECT COUNT(*) FROM \"{s}\" WHERE \"{s}\" = ?",
        .{ referencing_table, referencing_column },
    );
}

/// Cascade delete action - builds SQL to delete referencing records
pub fn buildCascadeDeleteSql(
    allocator: std.mem.Allocator,
    referencing_table: []const u8,
    referencing_column: []const u8,
) ![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        "DELETE FROM \"{s}\" WHERE \"{s}\" = ?",
        .{ referencing_table, referencing_column },
    );
}

/// Set null action - builds SQL to nullify referencing foreign keys
pub fn buildSetNullSql(
    allocator: std.mem.Allocator,
    referencing_table: []const u8,
    referencing_column: []const u8,
) ![]const u8 {
    return try std.fmt.allocPrint(
        allocator,
        "UPDATE \"{s}\" SET \"{s}\" = NULL WHERE \"{s}\" = ?",
        .{ referencing_table, referencing_column, referencing_column },
    );
}

// ============================================================================
// Tests
// ============================================================================

test "is all zeros" {
    try std.testing.expect(isAllZeros("000000"));
    try std.testing.expect(isAllZeros(&[_]u8{ 0, 0, 0 }));
    try std.testing.expect(!isAllZeros("000100"));
    try std.testing.expect(!isAllZeros("123456"));
}

test "has foreign keys" {
    var fields_with_fk = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{},
        },
        .{
            .name = "customer_id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{
                .foreign_key = .{ .table = "customer", .column = "id" },
            },
        },
    };

    const table_with_fk = TableSchema{
        .name = "order",
        .fields = &fields_with_fk,
    };

    try std.testing.expect(hasForeignKeys(&table_with_fk));

    var fields_without_fk = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{},
        },
    };

    const table_without_fk = TableSchema{
        .name = "customer",
        .fields = &fields_without_fk,
    };

    try std.testing.expect(!hasForeignKeys(&table_without_fk));
}

test "extract field value" {
    var fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{},
        },
        .{
            .name = "name",
            .data_type = .{ .alpha = .{ .size = 10 } },
            .decorators = .{},
        },
    };

    const table_schema = TableSchema{
        .name = "test",
        .fields = &fields,
    };

    const record = "123456John      ";

    const id_value = extractFieldValue(record, &table_schema, "id");
    try std.testing.expect(id_value != null);
    try std.testing.expectEqualStrings("123456", id_value.?);

    const name_value = extractFieldValue(record, &table_schema, "name");
    try std.testing.expect(name_value != null);
    try std.testing.expectEqualStrings("John      ", name_value.?);

    const missing = extractFieldValue(record, &table_schema, "nonexistent");
    try std.testing.expect(missing == null);
}

test "build existence query" {
    const query = try buildExistenceQuery(std.testing.allocator, "customer", "id");
    defer std.testing.allocator.free(query);

    try std.testing.expectEqualStrings(
        "SELECT 1 FROM \"customer\" WHERE \"id\" = ? LIMIT 1",
        query,
    );
}

test "validate foreign keys - mock lookup" {
    // Create a mock lookup that always returns true
    const MockLookup = struct {
        fn exists(_: *anyopaque, _: []const u8, _: []const u8, _: []const u8) bool {
            return true;
        }
    };

    var dummy: u8 = 0;
    const lookup = ForeignKeyLookup{
        .ctx = @ptrCast(&dummy),
        .existsFn = MockLookup.exists,
    };

    var fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true },
        },
        .{
            .name = "customer_id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{
                .foreign_key = .{ .table = "customer", .column = "id" },
            },
        },
    };

    const table_schema = TableSchema{
        .name = "order",
        .fields = &fields,
    };

    const record = "000001000042";

    const result = validateForeignKeys(record, &table_schema, &lookup);

    try std.testing.expect(result.valid);
    try std.testing.expectEqual(@as(usize, 1), result.checked_count);
    try std.testing.expect(result.error_details == null);
}

test "validate foreign keys - failure case" {
    // Create a mock lookup that always returns false
    const MockLookup = struct {
        fn exists(_: *anyopaque, _: []const u8, _: []const u8, _: []const u8) bool {
            return false;
        }
    };

    var dummy: u8 = 0;
    const lookup = ForeignKeyLookup{
        .ctx = @ptrCast(&dummy),
        .existsFn = MockLookup.exists,
    };

    var fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true },
        },
        .{
            .name = "customer_id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{
                .foreign_key = .{ .table = "customer", .column = "id" },
            },
        },
    };

    const table_schema = TableSchema{
        .name = "order",
        .fields = &fields,
    };

    const record = "000001999999"; // 999999 doesn't exist

    const result = validateForeignKeys(record, &table_schema, &lookup);

    try std.testing.expect(!result.valid);
    try std.testing.expect(result.error_details != null);
    try std.testing.expectEqualStrings("customer_id", result.error_details.?.field_name);
    try std.testing.expectEqualStrings("customer", result.error_details.?.referenced_table);
}
