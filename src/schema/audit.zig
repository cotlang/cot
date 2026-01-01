//! Audit Field Handler
//!
//! Automatically populates audit fields (created_at, updated_at) when
//! storing or updating records. This provides automatic timestamp tracking
//! for records without requiring explicit code.
//!
//! Usage:
//!   - Fields with @audit_created are set on INSERT (store)
//!   - Fields with @audit_updated are set on INSERT and UPDATE (store/write)

const std = @import("std");
const types = @import("types.zig");

const TableSchema = types.TableSchema;
const FieldSchema = types.FieldSchema;

/// Result of applying audit fields
pub const AuditResult = struct {
    /// Number of fields that were updated
    fields_updated: usize,
    /// Whether created_at was set
    created_at_set: bool,
    /// Whether updated_at was set
    updated_at_set: bool,
};

/// Apply audit field values to a record buffer
///
/// Args:
///   record: The record buffer to modify
///   table_schema: Schema defining the table structure
///   is_update: true if this is an UPDATE operation (write), false for INSERT (store)
///
/// Returns:
///   AuditResult indicating which fields were set
pub fn applyAuditFields(record: []u8, table_schema: *const TableSchema, is_update: bool) AuditResult {
    const now = std.time.timestamp();

    var result = AuditResult{
        .fields_updated = 0,
        .created_at_set = false,
        .updated_at_set = false,
    };

    var offset: usize = 0;
    for (table_schema.fields) |field| {
        const field_size = field.byteSize();

        if (field.decorators.audit_created and !is_update) {
            // Set created_at only on INSERT
            writeTimestampToField(record, offset, field_size, now);
            result.fields_updated += 1;
            result.created_at_set = true;
        }

        if (field.decorators.audit_updated) {
            // Set updated_at on both INSERT and UPDATE
            writeTimestampToField(record, offset, field_size, now);
            result.fields_updated += 1;
            result.updated_at_set = true;
        }

        offset += field_size;
    }

    return result;
}

/// Write a timestamp value to a field in the record buffer
fn writeTimestampToField(record: []u8, offset: usize, field_size: usize, timestamp: i64) void {
    if (offset + field_size > record.len) return;

    // Format timestamp based on field size
    // d14 = 14 digits: YYYYMMDDHHmmss format
    // d10 = 10 digits: Unix timestamp
    // Other sizes: truncated or zero-padded as appropriate

    if (field_size >= 14) {
        // Full datetime: YYYYMMDDHHmmss
        const dt = timestampToDatetime(timestamp);
        _ = std.fmt.bufPrint(record[offset..][0..14], "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}", .{
            dt.year,
            dt.month,
            dt.day,
            dt.hour,
            dt.minute,
            dt.second,
        }) catch {
            // On error, fill with zeros
            @memset(record[offset..][0..field_size], '0');
        };
    } else if (field_size >= 10) {
        // Unix timestamp (10 digits)
        _ = std.fmt.bufPrint(record[offset..][0..10], "{d:0>10}", .{
            @as(u64, @intCast(@max(0, timestamp))),
        }) catch {
            @memset(record[offset..][0..field_size], '0');
        };
    } else if (field_size >= 8) {
        // Date only: YYYYMMDD
        const dt = timestampToDatetime(timestamp);
        _ = std.fmt.bufPrint(record[offset..][0..8], "{d:0>4}{d:0>2}{d:0>2}", .{
            dt.year,
            dt.month,
            dt.day,
        }) catch {
            @memset(record[offset..][0..field_size], '0');
        };
    } else {
        // Field too small for meaningful timestamp, fill with zeros
        @memset(record[offset..][0..field_size], '0');
    }
}

/// Simple datetime structure for formatting
const Datetime = struct {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
};

/// Convert Unix timestamp to Datetime components
fn timestampToDatetime(timestamp: i64) Datetime {
    // Use Zig's epoch seconds conversion
    const epoch_seconds = std.time.epoch.EpochSeconds{
        .secs = @intCast(@max(0, timestamp)),
    };

    const epoch_day = epoch_seconds.getEpochDay();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch_seconds.getDaySeconds();

    return .{
        .year = year_day.year,
        .month = month_day.month.numeric(),
        .day = month_day.day_index + 1,
        .hour = day_seconds.getHoursIntoDay(),
        .minute = day_seconds.getMinutesIntoHour(),
        .second = day_seconds.getSecondsIntoMinute(),
    };
}

/// Get the current timestamp as a formatted string for a given field size
pub fn getCurrentTimestampString(allocator: std.mem.Allocator, field_size: usize) ![]const u8 {
    const now = std.time.timestamp();
    const dt = timestampToDatetime(now);

    if (field_size >= 14) {
        return try std.fmt.allocPrint(allocator, "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}", .{
            dt.year,
            dt.month,
            dt.day,
            dt.hour,
            dt.minute,
            dt.second,
        });
    } else if (field_size >= 10) {
        return try std.fmt.allocPrint(allocator, "{d:0>10}", .{
            @as(u64, @intCast(@max(0, now))),
        });
    } else if (field_size >= 8) {
        return try std.fmt.allocPrint(allocator, "{d:0>4}{d:0>2}{d:0>2}", .{
            dt.year,
            dt.month,
            dt.day,
        });
    } else {
        // Return zeros for small fields
        const zeros = try allocator.alloc(u8, field_size);
        @memset(zeros, '0');
        return zeros;
    }
}

/// Check if a table has any audit fields
pub fn hasAuditFields(table_schema: *const TableSchema) bool {
    for (table_schema.fields) |field| {
        if (field.decorators.audit_created or field.decorators.audit_updated) {
            return true;
        }
    }
    return false;
}

/// Get the field schema for the created_at audit field (if any)
pub fn getCreatedAtField(table_schema: *const TableSchema) ?*const FieldSchema {
    for (table_schema.fields) |*field| {
        if (field.decorators.audit_created) {
            return field;
        }
    }
    return null;
}

/// Get the field schema for the updated_at audit field (if any)
pub fn getUpdatedAtField(table_schema: *const TableSchema) ?*const FieldSchema {
    for (table_schema.fields) |*field| {
        if (field.decorators.audit_updated) {
            return field;
        }
    }
    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "timestamp to datetime conversion" {
    // Test epoch (1970-01-01 00:00:00)
    const epoch_dt = timestampToDatetime(0);
    try std.testing.expectEqual(@as(u16, 1970), epoch_dt.year);
    try std.testing.expectEqual(@as(u8, 1), epoch_dt.month);
    try std.testing.expectEqual(@as(u8, 1), epoch_dt.day);
    try std.testing.expectEqual(@as(u8, 0), epoch_dt.hour);
    try std.testing.expectEqual(@as(u8, 0), epoch_dt.minute);
    try std.testing.expectEqual(@as(u8, 0), epoch_dt.second);

    // Test a known date: 2024-03-15 10:30:45 = 1710498645
    const known_dt = timestampToDatetime(1710498645);
    try std.testing.expectEqual(@as(u16, 2024), known_dt.year);
    try std.testing.expectEqual(@as(u8, 3), known_dt.month);
    try std.testing.expectEqual(@as(u8, 15), known_dt.day);
    try std.testing.expectEqual(@as(u8, 10), known_dt.hour);
    try std.testing.expectEqual(@as(u8, 30), known_dt.minute);
    try std.testing.expectEqual(@as(u8, 45), known_dt.second);
}

test "write timestamp to field" {
    var buffer: [20]u8 = undefined;
    @memset(&buffer, ' ');

    // Test 14-digit field (full datetime)
    writeTimestampToField(&buffer, 0, 14, 1710498645);
    try std.testing.expectEqualStrings("20240315103045", buffer[0..14]);

    // Test 10-digit field (unix timestamp)
    @memset(&buffer, ' ');
    writeTimestampToField(&buffer, 0, 10, 1710498645);
    try std.testing.expectEqualStrings("1710498645", buffer[0..10]);

    // Test 8-digit field (date only)
    @memset(&buffer, ' ');
    writeTimestampToField(&buffer, 0, 8, 1710498645);
    try std.testing.expectEqualStrings("20240315", buffer[0..8]);
}

test "apply audit fields" {
    // Create a simple table schema with audit fields
    var fields = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{ .primary = true },
        },
        .{
            .name = "name",
            .data_type = .{ .alpha = .{ .size = 10 } },
            .decorators = .{},
        },
        .{
            .name = "created_at",
            .data_type = .{ .decimal = .{ .size = 14 } },
            .decorators = .{ .audit_created = true },
        },
        .{
            .name = "updated_at",
            .data_type = .{ .decimal = .{ .size = 14 } },
            .decorators = .{ .audit_updated = true },
        },
    };

    const table_schema = TableSchema{
        .name = "test_table",
        .fields = &fields,
    };

    // Create a record buffer (6 + 10 + 14 + 14 = 44 bytes)
    var record: [44]u8 = undefined;
    @memset(&record, ' ');

    // Apply audit fields on INSERT
    const result = applyAuditFields(&record, &table_schema, false);

    try std.testing.expectEqual(@as(usize, 2), result.fields_updated);
    try std.testing.expect(result.created_at_set);
    try std.testing.expect(result.updated_at_set);

    // Verify created_at was set (starts at offset 16)
    try std.testing.expect(record[16] != ' ');

    // Verify updated_at was set (starts at offset 30)
    try std.testing.expect(record[30] != ' ');
}

test "has audit fields" {
    var fields_with_audit = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{},
        },
        .{
            .name = "created_at",
            .data_type = .{ .decimal = .{ .size = 14 } },
            .decorators = .{ .audit_created = true },
        },
    };

    const table_with_audit = TableSchema{
        .name = "with_audit",
        .fields = &fields_with_audit,
    };

    try std.testing.expect(hasAuditFields(&table_with_audit));

    var fields_without_audit = [_]FieldSchema{
        .{
            .name = "id",
            .data_type = .{ .decimal = .{ .size = 6 } },
            .decorators = .{},
        },
    };

    const table_without_audit = TableSchema{
        .name = "without_audit",
        .fields = &fields_without_audit,
    };

    try std.testing.expect(!hasAuditFields(&table_without_audit));
}
