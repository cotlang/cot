//! Unified Handle Manager
//!
//! Manages all I/O handles (text files and ISAM cursors) in a single
//! HashMap-based registry. Supports both auto-assigned IDs (Cot Core style)
//! and explicit IDs (DBL channel style).

const std = @import("std");
const Handle = @import("handle.zig").Handle;
const HandleType = @import("handle.zig").HandleType;
const TextFileHandle = @import("text_file.zig").TextFileHandle;
const TextFileMode = @import("text_file.zig").Mode;
const IsamCursor = @import("isam_cursor.zig").IsamCursor;

/// Unified handle manager
pub const UnifiedHandleManager = struct {
    allocator: std.mem.Allocator,
    handles: std.AutoHashMap(u32, Handle),
    next_auto_id: u32,

    const Self = @This();

    /// Reserved handle IDs (0 is invalid)
    pub const INVALID_HANDLE: u32 = 0;

    /// Initialize a new handle manager
    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .handles = std.AutoHashMap(u32, Handle).init(allocator),
            .next_auto_id = 1,
        };
    }

    /// Clean up and close all handles
    pub fn deinit(self: *Self) void {
        // Close all open handles
        var iter = self.handles.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.close();
        }
        self.handles.deinit();
    }

    // ========================================================================
    // Handle Allocation
    // ========================================================================

    /// Allocate a new handle ID (auto-assignment for Cot Core style)
    /// Returns the new handle ID
    pub fn allocate(self: *Self) u32 {
        const id = self.next_auto_id;
        self.next_auto_id += 1;
        return id;
    }

    /// Check if a handle ID is available (for explicit assignment)
    pub fn isAvailable(self: *Self, id: u32) bool {
        if (id == INVALID_HANDLE) return false;
        return !self.handles.contains(id);
    }

    // ========================================================================
    // Text File Operations
    // ========================================================================

    /// Open a text file with auto-assigned handle ID
    /// Returns the handle ID
    pub fn openTextFile(self: *Self, path: []const u8, mode: TextFileMode) !u32 {
        const id = self.allocate();
        errdefer {} // ID is lost but that's OK

        const handle = try Handle.openTextFile(path, mode);
        try self.handles.put(id, handle);
        return id;
    }

    /// Open a text file at a specific handle ID (for DBL channel syntax)
    pub fn openTextFileAt(self: *Self, id: u32, path: []const u8, mode: TextFileMode) !void {
        if (id == INVALID_HANDLE) return error.InvalidHandle;

        // Close existing handle if open
        if (self.handles.getPtr(id)) |existing| {
            existing.close();
            _ = self.handles.remove(id);
        }

        const handle = try Handle.openTextFile(path, mode);
        try self.handles.put(id, handle);
    }

    // ========================================================================
    // ISAM Operations
    // ========================================================================

    /// Open an ISAM database with auto-assigned handle ID
    /// Returns the handle ID
    pub fn openIsam(self: *Self, path: []const u8) !u32 {
        const id = self.allocate();
        errdefer {} // ID is lost but that's OK

        const handle = try Handle.openIsam(self.allocator, path);
        try self.handles.put(id, handle);
        return id;
    }

    /// Open an ISAM database at a specific handle ID (for DBL channel syntax)
    pub fn openIsamAt(self: *Self, id: u32, path: []const u8) !void {
        if (id == INVALID_HANDLE) return error.InvalidHandle;

        // Close existing handle if open
        if (self.handles.getPtr(id)) |existing| {
            existing.close();
            _ = self.handles.remove(id);
        }

        const handle = try Handle.openIsam(self.allocator, path);
        try self.handles.put(id, handle);
    }

    // ========================================================================
    // Handle Access
    // ========================================================================

    /// Get a handle by ID (returns null if not found)
    pub fn get(self: *Self, id: u32) ?*Handle {
        return self.handles.getPtr(id);
    }

    /// Check if a handle is open
    pub fn isOpen(self: *Self, id: u32) bool {
        return self.handles.contains(id);
    }

    /// Get handle type (returns null if not found)
    pub fn getType(self: *Self, id: u32) ?HandleType {
        if (self.handles.getPtr(id)) |handle| {
            return handle.getType();
        }
        return null;
    }

    // ========================================================================
    // Handle Lifecycle
    // ========================================================================

    /// Close a handle by ID
    pub fn close(self: *Self, id: u32) void {
        if (self.handles.getPtr(id)) |handle| {
            handle.close();
            _ = self.handles.remove(id);
        }
    }

    /// Get number of open handles
    pub fn count(self: *Self) usize {
        return self.handles.count();
    }
};

/// Handle manager errors
pub const HandleManagerError = error{
    InvalidHandle,
    HandleNotFound,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

test "UnifiedHandleManager: basic lifecycle" {
    const allocator = std.testing.allocator;
    var mgr = UnifiedHandleManager.init(allocator);
    defer mgr.deinit();

    // Initially empty
    try std.testing.expectEqual(@as(usize, 0), mgr.count());

    // Open a text file
    const id = try mgr.openTextFile("/tmp/test_handle_mgr.txt", .write);
    try std.testing.expectEqual(@as(usize, 1), mgr.count());
    try std.testing.expect(mgr.isOpen(id));

    // Write to it
    if (mgr.get(id)) |handle| {
        try handle.writeLine("test data");
    }

    // Close it
    mgr.close(id);
    try std.testing.expectEqual(@as(usize, 0), mgr.count());
    try std.testing.expect(!mgr.isOpen(id));
}

test "UnifiedHandleManager: explicit ID" {
    const allocator = std.testing.allocator;
    var mgr = UnifiedHandleManager.init(allocator);
    defer mgr.deinit();

    // Open at explicit channel ID (DBL style)
    try mgr.openTextFileAt(42, "/tmp/test_handle_ch42.txt", .write);
    try std.testing.expect(mgr.isOpen(42));
    try std.testing.expectEqual(HandleType.text_file, mgr.getType(42).?);

    mgr.close(42);
    try std.testing.expect(!mgr.isOpen(42));
}

test "UnifiedHandleManager: auto ID increment" {
    const allocator = std.testing.allocator;
    var mgr = UnifiedHandleManager.init(allocator);
    defer mgr.deinit();

    const id1 = try mgr.openTextFile("/tmp/test_auto1.txt", .write);
    const id2 = try mgr.openTextFile("/tmp/test_auto2.txt", .write);
    const id3 = try mgr.openTextFile("/tmp/test_auto3.txt", .write);

    try std.testing.expectEqual(@as(u32, 1), id1);
    try std.testing.expectEqual(@as(u32, 2), id2);
    try std.testing.expectEqual(@as(u32, 3), id3);
    try std.testing.expectEqual(@as(usize, 3), mgr.count());
}
