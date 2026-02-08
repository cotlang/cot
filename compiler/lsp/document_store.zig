//! Manages open documents for the LSP server.

const std = @import("std");
const analysis = @import("analysis.zig");

const AnalysisResult = analysis.AnalysisResult;

pub const DocumentHandle = struct {
    uri: []const u8,
    version: i64,
    text: []const u8,
    result: ?AnalysisResult,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *DocumentHandle) void {
        if (self.result) |*r| r.deinit();
        self.allocator.free(self.text);
        self.allocator.free(self.uri);
    }
};

pub const DocumentStore = struct {
    documents: std.StringHashMap(DocumentHandle),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) DocumentStore {
        return .{
            .documents = std.StringHashMap(DocumentHandle).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DocumentStore) void {
        var it = self.documents.valueIterator();
        while (it.next()) |handle| {
            handle.deinit();
        }
        self.documents.deinit();
    }

    pub fn open(self: *DocumentStore, uri: []const u8, version: i64, text: []const u8) !void {
        const uri_dupe = try self.allocator.dupe(u8, uri);
        const text_dupe = try self.allocator.dupe(u8, text);

        // Extract filename from URI (strip file:// prefix)
        const filename = uriToFilename(uri);

        var result = analysis.analyze(self.allocator, text_dupe, filename);

        const handle = DocumentHandle{
            .uri = uri_dupe,
            .version = version,
            .text = text_dupe,
            .result = result,
            .allocator = self.allocator,
        };

        // Use uri_dupe as key (same memory as handle.uri)
        try self.documents.put(uri_dupe, handle);
        _ = &result;
    }

    pub fn change(self: *DocumentStore, uri: []const u8, version: i64, new_text: []const u8) !void {
        if (self.documents.getPtr(uri)) |handle| {
            // Free old analysis and text
            if (handle.result) |*r| r.deinit();
            self.allocator.free(handle.text);

            const text_dupe = try self.allocator.dupe(u8, new_text);
            const filename = uriToFilename(uri);

            handle.version = version;
            handle.text = text_dupe;
            handle.result = analysis.analyze(self.allocator, text_dupe, filename);
        }
    }

    pub fn close(self: *DocumentStore, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |entry| {
            var handle = entry.value;
            handle.deinit();
        }
    }

    pub fn get(self: *DocumentStore, uri: []const u8) ?*DocumentHandle {
        return self.documents.getPtr(uri);
    }
};

/// Extract a usable filename from a file:// URI.
fn uriToFilename(uri: []const u8) []const u8 {
    if (std.mem.startsWith(u8, uri, "file://")) {
        return uri["file://".len..];
    }
    return uri;
}

// ============================================================================
// Tests
// ============================================================================

test "uriToFilename: file URI" {
    try std.testing.expectEqualStrings("/Users/john/hello.cot", uriToFilename("file:///Users/john/hello.cot"));
}

test "uriToFilename: no scheme" {
    try std.testing.expectEqualStrings("hello.cot", uriToFilename("hello.cot"));
}
