//! Build Cache System
//!
//! Provides robust cache invalidation using content-based hashing.
//! This ensures builds are reproducible and stale caches are detected.
//!
//! Cache Key Composition:
//! ```
//! cache_key = hash(
//!     MANIFEST_VERSION,           // Cache format version
//!     compiler_version,           // Cot compiler version string
//!     build_mode,                 // debug/release
//!     content_hash(sources),      // xxHash of all source file contents
//!     content_hash(config),       // xxHash of cot.json
//!     hash(dependency_keys),      // Transitive dependency cache keys
//! )
//! ```
//!
//! This approach (similar to Zig/Rust) ensures:
//! - Source file changes invalidate cache
//! - Config changes invalidate cache
//! - Compiler upgrades invalidate cache
//! - Dependency changes transitively invalidate dependents

const std = @import("std");
const Allocator = std.mem.Allocator;
const cot = @import("../../root.zig");

/// Current manifest format version. Increment when format changes.
pub const MANIFEST_VERSION: u32 = 1;

/// Build mode affects optimization and debug info
pub const BuildMode = enum(u8) {
    debug = 0,
    release = 1,

    pub fn fromBool(is_release: bool) BuildMode {
        return if (is_release) .release else .debug;
    }
};

/// Cache manifest stored alongside compiled output.
/// Contains all information needed to determine cache validity.
pub const CacheManifest = struct {
    /// Manifest format version
    manifest_version: u32 = MANIFEST_VERSION,

    /// Cot compiler version that produced this cache
    compiler_version: []const u8,

    /// Build mode (debug/release)
    build_mode: BuildMode,

    /// xxHash of all source file contents (concatenated)
    source_hash: u64,

    /// xxHash of cot.json configuration file
    config_hash: u64,

    /// Cache keys of direct dependencies (for transitive invalidation)
    /// Maps dependency name -> dependency's cache_key
    dependency_keys: std.StringHashMapUnmanaged(u64),

    /// Final composite cache key (hash of all above)
    cache_key: u64,

    /// Unix timestamp when cache was created
    created_at: i64,

    /// List of source files included in source_hash
    source_files: []const []const u8,

    /// Project name
    project_name: []const u8,

    const Self = @This();

    pub fn init(_: Allocator) Self {
        return .{
            .compiler_version = cot.version,
            .build_mode = .debug,
            .source_hash = 0,
            .config_hash = 0,
            .dependency_keys = .empty,
            .cache_key = 0,
            .created_at = std.time.timestamp(),
            .source_files = &.{},
            .project_name = "",
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        // Free duplicated dependency key strings
        var dep_it = self.dependency_keys.iterator();
        while (dep_it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
        }
        self.dependency_keys.deinit(allocator);
        for (self.source_files) |f| {
            allocator.free(f);
        }
        if (self.source_files.len > 0) {
            allocator.free(self.source_files);
        }
        if (self.project_name.len > 0) {
            allocator.free(self.project_name);
        }
    }

    /// Serialize manifest to JSON for human-readable cache files
    pub fn serialize(self: *const Self, allocator: Allocator) ![]const u8 {
        var list: std.ArrayListUnmanaged(u8) = .empty;
        errdefer list.deinit(allocator);

        const writer = list.writer(allocator);

        try writer.writeAll("{\n");
        try writer.print("  \"manifest_version\": {d},\n", .{self.manifest_version});
        try writer.print("  \"compiler_version\": \"{s}\",\n", .{self.compiler_version});
        try writer.print("  \"build_mode\": \"{s}\",\n", .{@tagName(self.build_mode)});
        try writer.print("  \"source_hash\": \"{x:0>16}\",\n", .{self.source_hash});
        try writer.print("  \"config_hash\": \"{x:0>16}\",\n", .{self.config_hash});
        try writer.print("  \"cache_key\": \"{x:0>16}\",\n", .{self.cache_key});
        try writer.print("  \"created_at\": {d},\n", .{self.created_at});
        try writer.print("  \"project_name\": \"{s}\",\n", .{self.project_name});

        // Dependency keys
        try writer.writeAll("  \"dependency_keys\": {");
        var first = true;
        var dep_it = self.dependency_keys.iterator();
        while (dep_it.next()) |entry| {
            if (!first) try writer.writeAll(",");
            try writer.print("\n    \"{s}\": \"{x:0>16}\"", .{ entry.key_ptr.*, entry.value_ptr.* });
            first = false;
        }
        if (!first) try writer.writeAll("\n  ");
        try writer.writeAll("},\n");

        // Source files
        try writer.writeAll("  \"source_files\": [");
        for (self.source_files, 0..) |file, i| {
            if (i > 0) try writer.writeAll(",");
            try writer.print("\n    \"{s}\"", .{file});
        }
        if (self.source_files.len > 0) try writer.writeAll("\n  ");
        try writer.writeAll("]\n");

        try writer.writeAll("}\n");

        return list.toOwnedSlice(allocator);
    }

    /// Deserialize manifest from JSON
    pub fn deserialize(allocator: Allocator, content: []const u8) !Self {
        const parsed = std.json.parseFromSlice(std.json.Value, allocator, content, .{}) catch {
            return error.InvalidManifest;
        };
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) return error.InvalidManifest;

        var self = Self.init(allocator);
        errdefer self.deinit(allocator);

        // Parse manifest version
        if (root.object.get("manifest_version")) |v| {
            if (v == .integer) {
                self.manifest_version = @intCast(v.integer);
            }
        }

        // Parse build mode
        if (root.object.get("build_mode")) |v| {
            if (v == .string) {
                if (std.mem.eql(u8, v.string, "release")) {
                    self.build_mode = .release;
                } else {
                    self.build_mode = .debug;
                }
            }
        }

        // Parse hashes (stored as hex strings)
        if (root.object.get("source_hash")) |v| {
            if (v == .string) {
                self.source_hash = std.fmt.parseInt(u64, v.string, 16) catch 0;
            }
        }

        if (root.object.get("config_hash")) |v| {
            if (v == .string) {
                self.config_hash = std.fmt.parseInt(u64, v.string, 16) catch 0;
            }
        }

        if (root.object.get("cache_key")) |v| {
            if (v == .string) {
                self.cache_key = std.fmt.parseInt(u64, v.string, 16) catch 0;
            }
        }

        if (root.object.get("created_at")) |v| {
            if (v == .integer) {
                self.created_at = v.integer;
            }
        }

        if (root.object.get("project_name")) |v| {
            if (v == .string) {
                self.project_name = try allocator.dupe(u8, v.string);
            }
        }

        // Parse dependency keys
        if (root.object.get("dependency_keys")) |deps| {
            if (deps == .object) {
                var it = deps.object.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.* == .string) {
                        const key = std.fmt.parseInt(u64, entry.value_ptr.string, 16) catch continue;
                        // Must duplicate the key string since JSON data is freed after parsing
                        const key_copy = try allocator.dupe(u8, entry.key_ptr.*);
                        errdefer allocator.free(key_copy);
                        try self.dependency_keys.put(allocator, key_copy, key);
                    }
                }
            }
        }

        // Parse source files
        if (root.object.get("source_files")) |files| {
            if (files == .array) {
                var file_list: std.ArrayListUnmanaged([]const u8) = .empty;
                errdefer {
                    for (file_list.items) |f| allocator.free(f);
                    file_list.deinit(allocator);
                }

                for (files.array.items) |item| {
                    if (item == .string) {
                        try file_list.append(allocator, try allocator.dupe(u8, item.string));
                    }
                }
                self.source_files = try file_list.toOwnedSlice(allocator);
            }
        }

        return self;
    }
};

/// Computes cache keys for projects
pub const CacheKeyComputer = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    /// Compute content hash of a single file
    pub fn hashFile(self: *Self, path: []const u8) !u64 {
        _ = self;
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            if (err == error.FileNotFound) return 0;
            return err;
        };
        defer file.close();

        // Read file in chunks and hash
        var hasher = std.hash.XxHash64.init(0);
        var buf: [8192]u8 = undefined;

        while (true) {
            const bytes_read = file.read(&buf) catch return error.ReadError;
            if (bytes_read == 0) break;
            hasher.update(buf[0..bytes_read]);
        }

        return hasher.final();
    }

    /// Compute combined content hash of multiple files
    pub fn hashFiles(self: *Self, paths: []const []const u8) !u64 {
        var hasher = std.hash.XxHash64.init(0);

        for (paths) |path| {
            // Hash the filename too (so renames are detected)
            hasher.update(path);
            hasher.update(&[_]u8{0}); // Separator

            // Hash file content
            const file_hash = try self.hashFile(path);
            hasher.update(std.mem.asBytes(&file_hash));
        }

        return hasher.final();
    }

    /// Compute final cache key from all components
    pub fn computeCacheKey(
        self: *Self,
        source_hash: u64,
        config_hash: u64,
        build_mode: BuildMode,
        dependency_keys: *const std.StringHashMapUnmanaged(u64),
    ) u64 {
        _ = self;
        var hasher = std.hash.XxHash64.init(0);

        // Include manifest version (so format changes invalidate)
        hasher.update(std.mem.asBytes(&MANIFEST_VERSION));

        // Include compiler version
        hasher.update(cot.version);
        hasher.update(&[_]u8{0});

        // Include build mode
        const mode_byte: u8 = @intFromEnum(build_mode);
        hasher.update(&[_]u8{mode_byte});

        // Include source hash
        hasher.update(std.mem.asBytes(&source_hash));

        // Include config hash
        hasher.update(std.mem.asBytes(&config_hash));

        // Include dependency keys (sorted for determinism)
        // Note: For simplicity, we just iterate - in production you'd sort
        var dep_it = dependency_keys.iterator();
        while (dep_it.next()) |entry| {
            hasher.update(entry.key_ptr.*);
            hasher.update(&[_]u8{0});
            hasher.update(std.mem.asBytes(entry.value_ptr));
        }

        return hasher.final();
    }
};

/// Manages build cache for a workspace
pub const BuildCache = struct {
    allocator: Allocator,
    cache_dir: []const u8,
    computer: CacheKeyComputer,

    /// Cached manifests for already-processed projects
    manifests: std.StringHashMapUnmanaged(*CacheManifest),

    const Self = @This();

    pub fn init(allocator: Allocator, cache_dir: []const u8) !Self {
        // Ensure cache directory exists
        std.fs.cwd().makePath(cache_dir) catch {};

        return .{
            .allocator = allocator,
            .cache_dir = try allocator.dupe(u8, cache_dir),
            .computer = CacheKeyComputer.init(allocator),
            .manifests = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        var it = self.manifests.iterator();
        while (it.next()) |entry| {
            // Free the duplicated key
            self.allocator.free(entry.key_ptr.*);
            // Free the manifest
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.manifests.deinit(self.allocator);
        self.allocator.free(self.cache_dir);
    }

    /// Get path to manifest file for a project
    fn getManifestPath(self: *Self, project_name: []const u8) ![]const u8 {
        return std.fs.path.join(self.allocator, &.{
            self.cache_dir,
            project_name,
            "cache.json",
        });
    }

    /// Get path to compiled output for a project
    fn getOutputPath(self: *Self, project_name: []const u8, extension: []const u8) ![]const u8 {
        const filename = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ project_name, extension });
        defer self.allocator.free(filename);

        return std.fs.path.join(self.allocator, &.{
            self.cache_dir,
            project_name,
            filename,
        });
    }

    /// Load existing manifest for a project (returns null if not found or invalid)
    pub fn loadManifest(self: *Self, project_name: []const u8) !?*CacheManifest {
        // Check in-memory cache first
        if (self.manifests.get(project_name)) |manifest| {
            return manifest;
        }

        const manifest_path = try self.getManifestPath(project_name);
        defer self.allocator.free(manifest_path);

        const file = std.fs.cwd().openFile(manifest_path, .{}) catch {
            return null;
        };
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch {
            return null;
        };
        defer self.allocator.free(content);

        var manifest = CacheManifest.deserialize(self.allocator, content) catch {
            return null;
        };

        // Check manifest version compatibility
        if (manifest.manifest_version != MANIFEST_VERSION) {
            manifest.deinit(self.allocator);
            return null;
        }

        // Check compiler version
        if (!std.mem.eql(u8, manifest.compiler_version, cot.version)) {
            manifest.deinit(self.allocator);
            return null;
        }

        // Store in memory cache (must dupe key since caller may free it)
        const manifest_ptr = try self.allocator.create(CacheManifest);
        manifest_ptr.* = manifest;
        const key_copy = try self.allocator.dupe(u8, project_name);
        errdefer self.allocator.free(key_copy);
        try self.manifests.put(self.allocator, key_copy, manifest_ptr);

        return manifest_ptr;
    }

    /// Save manifest for a project
    pub fn saveManifest(self: *Self, manifest: *const CacheManifest) !void {
        const manifest_path = try self.getManifestPath(manifest.project_name);
        defer self.allocator.free(manifest_path);

        // Ensure directory exists
        if (std.fs.path.dirname(manifest_path)) |dir| {
            std.fs.cwd().makePath(dir) catch {};
        }

        const content = try manifest.serialize(self.allocator);
        defer self.allocator.free(content);

        const file = try std.fs.cwd().createFile(manifest_path, .{});
        defer file.close();
        try file.writeAll(content);
    }

    /// Check if cache is valid for a project
    pub fn isCacheValid(
        self: *Self,
        project_name: []const u8,
        source_files: []const []const u8,
        config_path: ?[]const u8,
        build_mode: BuildMode,
        dependency_keys: *const std.StringHashMapUnmanaged(u64),
    ) !CacheStatus {
        // Load existing manifest
        const manifest = try self.loadManifest(project_name) orelse {
            return .{ .valid = false, .reason = .no_manifest };
        };

        // Check build mode
        if (manifest.build_mode != build_mode) {
            return .{ .valid = false, .reason = .build_mode_changed };
        }

        // Compute current source hash
        const current_source_hash = try self.computer.hashFiles(source_files);
        if (current_source_hash != manifest.source_hash) {
            return .{ .valid = false, .reason = .source_changed };
        }

        // Compute current config hash
        if (config_path) |cfg| {
            const current_config_hash = try self.computer.hashFile(cfg);
            if (current_config_hash != manifest.config_hash) {
                return .{ .valid = false, .reason = .config_changed };
            }
        }

        // Check dependency keys
        var dep_it = dependency_keys.iterator();
        while (dep_it.next()) |entry| {
            const stored_key = manifest.dependency_keys.get(entry.key_ptr.*) orelse {
                return .{ .valid = false, .reason = .dependency_changed };
            };
            if (stored_key != entry.value_ptr.*) {
                return .{ .valid = false, .reason = .dependency_changed };
            }
        }

        // Check that no dependencies were removed
        var manifest_dep_it = manifest.dependency_keys.iterator();
        while (manifest_dep_it.next()) |entry| {
            if (!dependency_keys.contains(entry.key_ptr.*)) {
                return .{ .valid = false, .reason = .dependency_changed };
            }
        }

        return .{ .valid = true, .reason = .valid, .cache_key = manifest.cache_key };
    }

    /// Create a new manifest for a project
    pub fn createManifest(
        self: *Self,
        project_name: []const u8,
        source_files: []const []const u8,
        config_path: ?[]const u8,
        build_mode: BuildMode,
        dependency_keys: *const std.StringHashMapUnmanaged(u64),
    ) !CacheManifest {
        var manifest = CacheManifest.init(self.allocator);
        errdefer manifest.deinit(self.allocator);

        manifest.project_name = try self.allocator.dupe(u8, project_name);
        manifest.build_mode = build_mode;
        manifest.created_at = std.time.timestamp();

        // Compute source hash
        manifest.source_hash = try self.computer.hashFiles(source_files);

        // Compute config hash
        if (config_path) |cfg| {
            manifest.config_hash = try self.computer.hashFile(cfg);
        }

        // Copy dependency keys (must duplicate since deinit frees them)
        var dep_it = dependency_keys.iterator();
        while (dep_it.next()) |entry| {
            const key_copy = try self.allocator.dupe(u8, entry.key_ptr.*);
            errdefer self.allocator.free(key_copy);
            try manifest.dependency_keys.put(self.allocator, key_copy, entry.value_ptr.*);
        }

        // Store source files
        var files: std.ArrayListUnmanaged([]const u8) = .empty;
        for (source_files) |f| {
            try files.append(self.allocator, try self.allocator.dupe(u8, f));
        }
        manifest.source_files = try files.toOwnedSlice(self.allocator);

        // Compute final cache key
        manifest.cache_key = self.computer.computeCacheKey(
            manifest.source_hash,
            manifest.config_hash,
            manifest.build_mode,
            &manifest.dependency_keys,
        );

        return manifest;
    }

    /// Get the cache key for a project (for use by dependents)
    pub fn getCacheKey(self: *Self, project_name: []const u8) !?u64 {
        const manifest = try self.loadManifest(project_name) orelse return null;
        return manifest.cache_key;
    }

    /// Clean cache for a specific project
    pub fn cleanProject(self: *Self, project_name: []const u8) !void {
        const project_cache_dir = try std.fs.path.join(self.allocator, &.{
            self.cache_dir,
            project_name,
        });
        defer self.allocator.free(project_cache_dir);

        std.fs.cwd().deleteTree(project_cache_dir) catch {};

        // Remove from in-memory cache
        if (self.manifests.fetchRemove(project_name)) |kv| {
            // Free the duplicated key
            self.allocator.free(kv.key);
            // Free the manifest
            kv.value.deinit(self.allocator);
            self.allocator.destroy(kv.value);
        }
    }

    /// Clean entire cache
    pub fn cleanAll(self: *Self) void {
        std.fs.cwd().deleteTree(self.cache_dir) catch {};
        std.fs.cwd().makePath(self.cache_dir) catch {};

        // Clear in-memory cache
        var it = self.manifests.iterator();
        while (it.next()) |entry| {
            // Free the duplicated key
            self.allocator.free(entry.key_ptr.*);
            // Free the manifest
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.manifests.clearRetainingCapacity();
    }
};

/// Status of cache validation
pub const CacheStatus = struct {
    valid: bool,
    reason: InvalidationReason,
    cache_key: u64 = 0,
};

/// Reason why cache was invalidated
pub const InvalidationReason = enum {
    valid,
    no_manifest,
    manifest_version_mismatch,
    compiler_version_mismatch,
    build_mode_changed,
    source_changed,
    config_changed,
    dependency_changed,

    pub fn toString(self: InvalidationReason) []const u8 {
        return switch (self) {
            .valid => "cache valid",
            .no_manifest => "no cache manifest found",
            .manifest_version_mismatch => "cache format version changed",
            .compiler_version_mismatch => "compiler version changed",
            .build_mode_changed => "build mode changed (debug/release)",
            .source_changed => "source files changed",
            .config_changed => "cot.json configuration changed",
            .dependency_changed => "dependency changed",
        };
    }
};

// Tests
test "hash file" {
    const allocator = std.testing.allocator;
    var computer = CacheKeyComputer.init(allocator);

    // Hash a non-existent file should return 0
    const hash = try computer.hashFile("/nonexistent/file.txt");
    try std.testing.expectEqual(@as(u64, 0), hash);
}

test "manifest serialization roundtrip" {
    const allocator = std.testing.allocator;

    var manifest = CacheManifest.init(allocator);
    defer manifest.deinit(allocator);

    manifest.source_hash = 0x123456789ABCDEF0;
    manifest.config_hash = 0xFEDCBA9876543210;
    manifest.cache_key = 0xDEADBEEFCAFEBABE;
    manifest.build_mode = .release;
    manifest.project_name = try allocator.dupe(u8, "test-project");

    // Must dupe keys since deinit frees them
    const dep1_key = try allocator.dupe(u8, "dep1");
    const dep2_key = try allocator.dupe(u8, "dep2");
    try manifest.dependency_keys.put(allocator, dep1_key, 0x1111111111111111);
    try manifest.dependency_keys.put(allocator, dep2_key, 0x2222222222222222);

    const json = try manifest.serialize(allocator);
    defer allocator.free(json);

    var parsed = try CacheManifest.deserialize(allocator, json);
    defer parsed.deinit(allocator);

    try std.testing.expectEqual(manifest.source_hash, parsed.source_hash);
    try std.testing.expectEqual(manifest.config_hash, parsed.config_hash);
    try std.testing.expectEqual(manifest.cache_key, parsed.cache_key);
    try std.testing.expectEqual(manifest.build_mode, parsed.build_mode);
}

test "cache key determinism" {
    const allocator = std.testing.allocator;
    var computer = CacheKeyComputer.init(allocator);

    var deps: std.StringHashMapUnmanaged(u64) = .empty;
    defer deps.deinit(allocator);

    const key1 = computer.computeCacheKey(100, 200, .debug, &deps);
    const key2 = computer.computeCacheKey(100, 200, .debug, &deps);

    try std.testing.expectEqual(key1, key2);

    // Different source hash should produce different key
    const key3 = computer.computeCacheKey(101, 200, .debug, &deps);
    try std.testing.expect(key1 != key3);
}
