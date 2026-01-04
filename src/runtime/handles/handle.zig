//! Handle Union Type
//!
//! A polymorphic handle that can be either a text file or ISAM cursor.
//! Provides unified interface for handle operations.

const std = @import("std");
const cotdb = @import("cotdb");
const TextFileHandle = @import("text_file.zig").TextFileHandle;
const TextFileMode = @import("text_file.zig").Mode;
const IsamCursor = @import("isam_cursor.zig").IsamCursor;
const MatchMode = cotdb.MatchMode;

/// Handle type tag
pub const HandleType = enum {
    text_file,
    isam_cursor,
};

/// Unified handle for all I/O operations
pub const Handle = union(HandleType) {
    text_file: TextFileHandle,
    isam_cursor: IsamCursor,

    const Self = @This();

    /// Open a text file handle
    pub fn openTextFile(path: []const u8, mode: TextFileMode) !Self {
        return Self{
            .text_file = try TextFileHandle.open(path, mode),
        };
    }

    /// Open an ISAM cursor handle
    pub fn openIsam(allocator: std.mem.Allocator, path: []const u8) !Self {
        return Self{
            .isam_cursor = try IsamCursor.open(allocator, path),
        };
    }

    /// Close the handle
    pub fn close(self: *Self) void {
        switch (self.*) {
            .text_file => |*tf| tf.close(),
            .isam_cursor => |*ic| ic.close(),
        }
    }

    /// Get handle type
    pub fn getType(self: *const Self) HandleType {
        return self.*;
    }

    // ========================================================================
    // Text File Operations
    // ========================================================================

    /// Read a line (text file only)
    pub fn readLine(self: *Self, allocator: std.mem.Allocator) !?[]u8 {
        switch (self.*) {
            .text_file => |*tf| return tf.readLine(allocator),
            .isam_cursor => return error.InvalidHandleType,
        }
    }

    /// Write a line with newline (text file only)
    pub fn writeLine(self: *Self, data: []const u8) !void {
        switch (self.*) {
            .text_file => |*tf| try tf.writeLine(data),
            .isam_cursor => return error.InvalidHandleType,
        }
    }

    /// Write raw data without newline (text file only)
    pub fn writeRaw(self: *Self, data: []const u8) !void {
        switch (self.*) {
            .text_file => |*tf| try tf.write(data),
            .isam_cursor => return error.InvalidHandleType,
        }
    }

    // ========================================================================
    // ISAM Operations
    // ========================================================================

    /// Read a record by key (ISAM only)
    pub fn isamRead(self: *Self, key_num: u8, key_value: []const u8, match_mode: MatchMode) ![]const u8 {
        switch (self.*) {
            .isam_cursor => |*ic| return ic.read(key_num, key_value, match_mode),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Read the first record (ISAM only)
    pub fn isamReadFirst(self: *Self, key_num: u8) ![]const u8 {
        switch (self.*) {
            .isam_cursor => |*ic| return ic.readFirst(key_num),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Read the next record (ISAM only)
    pub fn isamReadNext(self: *Self) ![]const u8 {
        switch (self.*) {
            .isam_cursor => |*ic| return ic.readNext(),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Store (insert) a record (ISAM only)
    pub fn isamStore(self: *Self, record: []const u8) !void {
        switch (self.*) {
            .isam_cursor => |*ic| try ic.store(record),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Write (update) current record (ISAM only)
    pub fn isamWrite(self: *Self, record: []const u8) !void {
        switch (self.*) {
            .isam_cursor => |*ic| try ic.write(record),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Delete current record (ISAM only)
    pub fn isamDelete(self: *Self) !void {
        switch (self.*) {
            .isam_cursor => |*ic| try ic.delete(),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Find/position by key (ISAM only)
    pub fn isamFind(self: *Self, key_num: u8, key_value: []const u8, match_mode: MatchMode) !void {
        switch (self.*) {
            .isam_cursor => |*ic| try ic.find(key_num, key_value, match_mode),
            .text_file => return error.InvalidHandleType,
        }
    }

    /// Get current record (ISAM only)
    pub fn isamGetRecord(self: *const Self) ?[]const u8 {
        switch (self.*) {
            .isam_cursor => |*ic| return ic.getCurrentRecord(),
            .text_file => return null,
        }
    }

    // ========================================================================
    // Common Operations
    // ========================================================================

    /// Check if EOF has been reached
    pub fn eof(self: *const Self) bool {
        switch (self.*) {
            .text_file => |*tf| return tf.eof(),
            .isam_cursor => |*ic| return ic.eof(),
        }
    }
};

/// Handle errors
pub const HandleError = error{
    InvalidHandleType,
    NotOpen,
    NoCurrentRecord,
    TableNotFound,
};
