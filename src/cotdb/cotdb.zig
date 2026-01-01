//! CotDB - SQLite-backed Database for Zig
//!
//! CotDB provides indexed database storage with:
//! - SQLite-backed storage for reliability and compatibility
//! - ULID (Universally Unique Lexicographically Sortable Identifier) for records
//! - Works with Turso for edge deployment
//!
//! ## Quick Start
//!
//! ```zig
//! const cdb = @import("cotdb");
//!
//! // Define tables with keys
//! const tables = [_]cdb.TableDef{
//!     .{
//!         .name = "customers",
//!         .record_size = 100,
//!         .keys = &[_]cdb.KeyDef{
//!             .{ .name = "pk", .segments = &.{
//!                 .{ .start = 0, .length = 8, .key_type = .string }
//!             }, .primary = true },
//!         },
//!     },
//! };
//!
//! var db = try cdb.Database.create(allocator, "mydata.db", &tables);
//! defer db.close();
//!
//! // Insert a record
//! const ulid = try db.store("customers", record_bytes);
//! ```

const std = @import("std");

// Schema and metadata
pub const schema = @import("schema.zig");
pub const Schema = schema.Schema;
pub const ColumnDef = schema.ColumnDef;
pub const ColumnType = schema.ColumnType;
pub const SchemaBuilder = schema.SchemaBuilder;
pub const TableBuilder = schema.TableBuilder;

// SQLite-backed database
pub const sqlite_isam = @import("sqlite_isam.zig");
pub const Database = sqlite_isam.SqliteIsam;
pub const SqliteIsam = sqlite_isam.SqliteIsam; // Backwards compat

// Direct exports from database
pub const KeyDef = sqlite_isam.KeyDef;
pub const KeySegment = sqlite_isam.KeySegment;
pub const KeyType = sqlite_isam.KeyType;
pub const TableDef = sqlite_isam.TableDef;
pub const MatchMode = sqlite_isam.MatchMode;
pub const DatabaseError = sqlite_isam.IsamError;
pub const IsamError = sqlite_isam.IsamError; // Backwards compat

// Lock modes for record access
pub const LockMode = enum {
    none, // No locking
    read, // Shared read lock
    write, // Exclusive write lock
};

// Unique identifiers
pub const ulid = @import("ulid.zig");
pub const ULID = ulid.ULID;

// Cursor management (database table access)
pub const connections = @import("connections.zig");
pub const CursorManager = connections.CursorManager;
pub const Cursor = connections.Cursor;
pub const MAX_CURSORS = connections.MAX_CURSORS;

// Errors
pub const Error = @import("errors.zig").Error;

// Version info
pub const version = "0.3.0";
pub const format_version: u32 = 3;

test {
    _ = sqlite_isam;
    _ = schema;
    _ = ulid;
    _ = connections;
}
