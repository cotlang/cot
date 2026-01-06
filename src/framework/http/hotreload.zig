//! Hot Reload for Dex Components
//!
//! Provides live reload functionality for Dex components during development.
//! Uses WebSockets to notify browsers when files change.

const std = @import("std");
const websocket = @import("websocket.zig");
const Allocator = std.mem.Allocator;
const Thread = std.Thread;
const Mutex = Thread.Mutex;

/// Hot reload server configuration
pub const HotReloadConfig = struct {
    /// Directories to watch for changes
    watch_dirs: []const []const u8 = &.{},
    /// File extensions to watch
    extensions: []const []const u8 = &.{ ".dx", ".cot" },
    /// Poll interval in milliseconds
    poll_interval_ms: u32 = 500,
};

/// Client-side JavaScript for hot reload
pub const client_script =
    \\<script>
    \\(function() {
    \\  const ws = new WebSocket('ws://' + location.host + '/__dex_reload');
    \\  ws.onopen = function() {
    \\    console.log('[Dex] Hot reload connected');
    \\  };
    \\  ws.onmessage = function(event) {
    \\    const msg = JSON.parse(event.data);
    \\    if (msg.type === 'reload') {
    \\      console.log('[Dex] Reloading: ' + msg.file);
    \\      location.reload();
    \\    } else if (msg.type === 'css') {
    \\      // Hot swap CSS without full reload
    \\      const links = document.querySelectorAll('link[rel="stylesheet"]');
    \\      links.forEach(function(link) {
    \\        link.href = link.href.split('?')[0] + '?t=' + Date.now();
    \\      });
    \\    }
    \\  };
    \\  ws.onclose = function() {
    \\    console.log('[Dex] Hot reload disconnected, attempting reconnect...');
    \\    setTimeout(function() { location.reload(); }, 2000);
    \\  };
    \\  ws.onerror = function(err) {
    \\    console.error('[Dex] WebSocket error:', err);
    \\  };
    \\})();
    \\</script>
;

/// File modification tracker
pub const FileTracker = struct {
    allocator: Allocator,
    files: std.StringHashMapUnmanaged(i128),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .files = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all keys
        var iter = self.files.keyIterator();
        while (iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.files.deinit(self.allocator);
    }

    /// Check if any tracked files have changed
    pub fn checkChanges(self: *Self, watch_dirs: []const []const u8, extensions: []const []const u8) !?[]const u8 {
        for (watch_dirs) |dir| {
            if (try self.scanDirectory(dir, extensions)) |changed| {
                return changed;
            }
        }
        return null;
    }

    fn scanDirectory(self: *Self, path: []const u8, extensions: []const []const u8) !?[]const u8 {
        const dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return null;
            return err;
        };
        var dir_copy = dir;
        defer dir_copy.close();

        var iter = dir_copy.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind == .directory) {
                // Recurse into subdirectory
                var sub_path_buf: [std.fs.max_path_bytes]u8 = undefined;
                const sub_path = std.fmt.bufPrint(&sub_path_buf, "{s}/{s}", .{ path, entry.name }) catch continue;
                if (try self.scanDirectory(sub_path, extensions)) |changed| {
                    return changed;
                }
                continue;
            }

            if (entry.kind != .file) continue;

            // Check extension
            var matches_ext = false;
            for (extensions) |ext| {
                if (std.mem.endsWith(u8, entry.name, ext)) {
                    matches_ext = true;
                    break;
                }
            }
            if (!matches_ext) continue;

            // Build full path
            var full_path_buf: [std.fs.max_path_bytes]u8 = undefined;
            const full_path = std.fmt.bufPrint(&full_path_buf, "{s}/{s}", .{ path, entry.name }) catch continue;

            // Get modification time
            const stat = dir_copy.statFile(entry.name) catch continue;
            const mtime = stat.mtime;

            // Check if file is new or changed
            const gop = try self.files.getOrPut(self.allocator, full_path);
            if (!gop.found_existing) {
                // New file - copy the key
                gop.key_ptr.* = try self.allocator.dupe(u8, full_path);
                gop.value_ptr.* = mtime;
                // Don't trigger reload for initial discovery
            } else if (gop.value_ptr.* != mtime) {
                // File changed
                gop.value_ptr.* = mtime;
                // Return the path (owned by the map)
                return gop.key_ptr.*;
            }
        }
        return null;
    }
};

/// Generate HTML page with hot reload script injected
pub fn injectHotReloadScript(html: []const u8, buffer: []u8) ![]const u8 {
    // Find </body> or </html> to inject before
    const inject_point = std.mem.lastIndexOf(u8, html, "</body>") orelse
        std.mem.lastIndexOf(u8, html, "</html>") orelse
        html.len;

    const prefix = html[0..inject_point];
    const suffix = html[inject_point..];

    return std.fmt.bufPrint(buffer, "{s}{s}{s}", .{ prefix, client_script, suffix }) catch {
        return error.BufferTooSmall;
    };
}

// ============================================================================
// Tests
// ============================================================================

test "inject hot reload script" {
    const html = "<html><body><h1>Test</h1></body></html>";
    var buffer: [2048]u8 = undefined;

    const result = try injectHotReloadScript(html, &buffer);

    try std.testing.expect(std.mem.indexOf(u8, result, "WebSocket") != null);
    try std.testing.expect(std.mem.indexOf(u8, result, "__dex_reload") != null);
    try std.testing.expect(std.mem.endsWith(u8, result, "</body></html>"));
}
