//! Dex DOM Diffing
//!
//! Compares old and new rendered HTML and generates minimal patches
//! to send to the client over WebSocket.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Patch operation types
pub const PatchOp = enum {
    replace,    // Replace element/content
    insert,     // Insert new element
    remove,     // Remove element
    attr_set,   // Set attribute
    attr_remove,// Remove attribute
    text,       // Update text content
};

/// A single DOM patch
pub const Patch = struct {
    op: PatchOp,
    path: []const u8,      // CSS selector or path to target
    html: ?[]const u8,     // HTML content (for replace/insert)
    attr: ?[]const u8,     // Attribute name
    value: ?[]const u8,    // Attribute value
};

/// Patch set to send to client
pub const PatchSet = struct {
    component_id: []const u8,
    patches: []const Patch,

    /// Serialize to JSON for WebSocket transmission
    pub fn toJson(self: *const PatchSet, allocator: Allocator) ![]const u8 {
        var buffer: std.ArrayListUnmanaged(u8) = .empty;
        defer buffer.deinit(allocator);

        try buffer.appendSlice(allocator, "{\"type\":\"patch\",\"component_id\":\"");
        try buffer.appendSlice(allocator, self.component_id);
        try buffer.appendSlice(allocator, "\",\"patches\":[");

        for (self.patches, 0..) |patch, i| {
            if (i > 0) try buffer.append(allocator, ',');
            try buffer.appendSlice(allocator, "{\"op\":\"");
            try buffer.appendSlice(allocator, @tagName(patch.op));
            try buffer.appendSlice(allocator, "\",\"path\":\"");
            try appendEscaped(&buffer, allocator, patch.path);
            try buffer.append(allocator, '"');

            if (patch.html) |html| {
                try buffer.appendSlice(allocator, ",\"html\":\"");
                try appendEscaped(&buffer, allocator, html);
                try buffer.append(allocator, '"');
            }

            if (patch.attr) |attr| {
                try buffer.appendSlice(allocator, ",\"attr\":\"");
                try appendEscaped(&buffer, allocator, attr);
                try buffer.append(allocator, '"');
            }

            if (patch.value) |value| {
                try buffer.appendSlice(allocator, ",\"value\":\"");
                try appendEscaped(&buffer, allocator, value);
                try buffer.append(allocator, '"');
            }

            try buffer.append(allocator, '}');
        }

        try buffer.appendSlice(allocator, "]}");
        return buffer.toOwnedSlice(allocator);
    }
};

fn appendEscaped(buffer: *std.ArrayListUnmanaged(u8), allocator: Allocator, str: []const u8) !void {
    for (str) |c| {
        switch (c) {
            '"' => try buffer.appendSlice(allocator, "\\\""),
            '\\' => try buffer.appendSlice(allocator, "\\\\"),
            '\n' => try buffer.appendSlice(allocator, "\\n"),
            '\r' => try buffer.appendSlice(allocator, "\\r"),
            '\t' => try buffer.appendSlice(allocator, "\\t"),
            else => try buffer.append(allocator, c),
        }
    }
}

/// Diff engine
pub const Differ = struct {
    allocator: Allocator,
    patches: std.ArrayListUnmanaged(Patch),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .patches = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.patches.deinit(self.allocator);
    }

    /// Compare old and new HTML, generating patches
    /// For now, this is a simple full-replace strategy.
    /// Future optimization: implement proper DOM diffing.
    pub fn diff(self: *Self, component_id: []const u8, old_html: []const u8, new_html: []const u8) !PatchSet {
        self.patches.clearRetainingCapacity();

        // Simple strategy: if content differs, replace entire component
        if (!std.mem.eql(u8, old_html, new_html)) {
            try self.patches.append(self.allocator, Patch{
                .op = .replace,
                .path = "",  // Root of component
                .html = new_html,
                .attr = null,
                .value = null,
            });
        }

        return PatchSet{
            .component_id = component_id,
            .patches = self.patches.items,
        };
    }

    /// Check if there are any patches
    pub fn hasChanges(self: *const Self) bool {
        return self.patches.items.len > 0;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "diff no changes" {
    const allocator = std.testing.allocator;

    var differ = Differ.init(allocator);
    defer differ.deinit();

    const html = "<div>Hello</div>";
    const patch_set = try differ.diff("comp-1", html, html);

    try std.testing.expectEqual(@as(usize, 0), patch_set.patches.len);
}

test "diff with changes" {
    const allocator = std.testing.allocator;

    var differ = Differ.init(allocator);
    defer differ.deinit();

    const old_html = "<div>Count: 0</div>";
    const new_html = "<div>Count: 1</div>";
    const patch_set = try differ.diff("comp-1", old_html, new_html);

    try std.testing.expectEqual(@as(usize, 1), patch_set.patches.len);
    try std.testing.expectEqual(PatchOp.replace, patch_set.patches[0].op);
}

test "patch set to json" {
    const allocator = std.testing.allocator;

    var differ = Differ.init(allocator);
    defer differ.deinit();

    const patch_set = try differ.diff("comp-1", "<div>A</div>", "<div>B</div>");
    const json = try patch_set.toJson(allocator);
    defer allocator.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "\"type\":\"patch\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"component_id\":\"comp-1\"") != null);
}
