//! File watcher for `cot --watch` mode.
//!
//! Reference: Deno's file_watcher.rs (debounce pattern), Zig's Build/Watch.zig (mtime polling).
//! Uses mtime polling (simplest, zero platform dependencies).
//! Deno uses 200ms debounce interval; we use 300ms poll interval for simplicity.

const std = @import("std");

pub const FileWatcher = struct {
    allocator: std.mem.Allocator,
    /// Map of watched file paths → last known mtime (nanoseconds)
    files: std.StringHashMapUnmanaged(i128),

    pub fn init(allocator: std.mem.Allocator) FileWatcher {
        return .{
            .allocator = allocator,
            .files = .{},
        };
    }

    pub fn deinit(self: *FileWatcher) void {
        // Free all owned path strings
        var it = self.files.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.files.deinit(self.allocator);
    }

    /// Add a file to the watch list. Records current mtime.
    pub fn addFile(self: *FileWatcher, path: []const u8) void {
        if (self.files.contains(path)) return;
        const owned = self.allocator.dupe(u8, path) catch return;
        const mtime = getFileMtime(path);
        self.files.put(self.allocator, owned, mtime) catch {
            self.allocator.free(owned);
        };
    }

    /// Add a file and all .cot files in its directory (for import tracking).
    pub fn addFileAndSiblings(self: *FileWatcher, path: []const u8) void {
        self.addFile(path);
        // Watch all .cot files in the same directory (catches import changes)
        const dir_path = std.fs.path.dirname(path) orelse ".";
        var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
        defer dir.close();
        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".cot")) continue;
            const full = std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ dir_path, entry.name }) catch continue;
            if (self.files.contains(full)) {
                self.allocator.free(full);
                continue;
            }
            const mtime = getFileMtime(full);
            self.files.put(self.allocator, full, mtime) catch {
                self.allocator.free(full);
            };
        }
    }

    /// Poll for changes. Returns true if any file changed.
    /// Deno pattern: poll at interval, return on first change detected.
    pub fn pollForChanges(self: *FileWatcher) bool {
        var changed = false;
        var it = self.files.iterator();
        while (it.next()) |entry| {
            const new_mtime = getFileMtime(entry.key_ptr.*);
            if (new_mtime != entry.value_ptr.*) {
                entry.value_ptr.* = new_mtime;
                changed = true;
            }
        }
        return changed;
    }

    /// Get file modification time, or 0 if file doesn't exist.
    fn getFileMtime(path: []const u8) i128 {
        const file = std.fs.cwd().openFile(path, .{}) catch return 0;
        defer file.close();
        const stat = file.stat() catch return 0;
        return stat.mtime;
    }
};
