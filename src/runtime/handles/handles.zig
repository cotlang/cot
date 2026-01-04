//! Handles Module
//!
//! Unified I/O handle management for text files and ISAM databases.
//! This module provides the core infrastructure for both Cot Core
//! method-style I/O and DBL channel-based I/O.

pub const UnifiedHandleManager = @import("manager.zig").UnifiedHandleManager;
pub const HandleManagerError = @import("manager.zig").HandleManagerError;

pub const Handle = @import("handle.zig").Handle;
pub const HandleType = @import("handle.zig").HandleType;
pub const HandleError = @import("handle.zig").HandleError;

pub const TextFileHandle = @import("text_file.zig").TextFileHandle;
pub const TextFileMode = @import("text_file.zig").Mode;

pub const IsamCursor = @import("isam_cursor.zig").IsamCursor;
pub const IsamCursorError = @import("isam_cursor.zig").IsamCursorError;

test {
    _ = @import("manager.zig");
    _ = @import("handle.zig");
    _ = @import("text_file.zig");
    _ = @import("isam_cursor.zig");
}
