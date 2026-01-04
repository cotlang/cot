//! Cot Project Configuration
//!
//! Parses and validates cot.json configuration files.
//! Supports both workspace root configs and app/package configs.

const std = @import("std");
const json = std.json;
const Allocator = std.mem.Allocator;

/// Project type
pub const ProjectType = enum {
    app,
    library,
    workspace,
};

/// Workspace configuration (root cot.json)
pub const WorkspaceConfig = struct {
    name: []const u8,
    version: []const u8 = "1.0.0",
    private: bool = true,
    workspaces: []const []const u8 = &.{ "apps/*", "packages/*" },

    pub fn deinit(self: *WorkspaceConfig, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.version);
        for (self.workspaces) |ws| {
            allocator.free(ws);
        }
        allocator.free(self.workspaces);
    }
};

/// Schema repository configuration
pub const SchemaConfig = struct {
    /// Path to schema file (relative to project root)
    file: []const u8 = "schema.cot",
    /// Automatically run migrations on build
    auto_migrate: bool = false,
    /// Database backend (sqlite, postgres, turso)
    backend: []const u8 = "sqlite",

    pub fn deinit(self: *SchemaConfig, allocator: Allocator) void {
        allocator.free(self.file);
        allocator.free(self.backend);
    }
};

/// App or package configuration
pub const ProjectConfig = struct {
    name: []const u8,
    version: []const u8,
    project_type: ProjectType = .app,
    main: ?[]const u8 = null,
    dependencies: std.StringHashMap([]const u8),
    exports: std.StringHashMap([]const u8), // Library exports: export name -> path

    // Include paths for .include directive (logical name -> path)
    // Paths are relative to cot.json and support ${ENV_VAR} expansion
    include_paths: std.StringHashMap([]const u8),

    // Schema repository configuration
    schema: ?SchemaConfig = null,

    // Convention directories
    screens_dir: []const u8,
    reports_dir: []const u8,
    jobs_dir: []const u8,
    api_dir: []const u8,
    subroutines_dir: []const u8,
    records_dir: []const u8,

    // Build settings
    output_dir: []const u8,
    target: []const u8,
    bundle: bool, // Bundle all modules into single executable
    generate_manifest: bool, // Generate manifest.json with build info
    static_link: bool, // Statically link dependencies

    allocator: Allocator,

    pub fn init(allocator: Allocator) !ProjectConfig {
        return .{
            .name = try allocator.dupe(u8, ""),
            .version = try allocator.dupe(u8, "1.0.0"),
            .screens_dir = try allocator.dupe(u8, "screens"),
            .reports_dir = try allocator.dupe(u8, "reports"),
            .jobs_dir = try allocator.dupe(u8, "jobs"),
            .api_dir = try allocator.dupe(u8, "api"),
            .subroutines_dir = try allocator.dupe(u8, "subroutines"),
            .records_dir = try allocator.dupe(u8, "records"),
            .output_dir = try allocator.dupe(u8, "bin"),
            .target = try allocator.dupe(u8, "bytecode"),
            .bundle = true,
            .generate_manifest = true,
            .static_link = true,
            .dependencies = std.StringHashMap([]const u8).init(allocator),
            .exports = std.StringHashMap([]const u8).init(allocator),
            .include_paths = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ProjectConfig) void {
        self.allocator.free(self.name);
        self.allocator.free(self.version);
        if (self.main) |m| self.allocator.free(m);
        self.allocator.free(self.screens_dir);
        self.allocator.free(self.reports_dir);
        self.allocator.free(self.jobs_dir);
        self.allocator.free(self.api_dir);
        self.allocator.free(self.subroutines_dir);
        self.allocator.free(self.records_dir);
        self.allocator.free(self.output_dir);
        self.allocator.free(self.target);

        // Clean up schema config if present
        if (self.schema) |*s| {
            var schema_cfg = s.*;
            schema_cfg.deinit(self.allocator);
        }

        var it = self.dependencies.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.dependencies.deinit();

        var exp_it = self.exports.iterator();
        while (exp_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.exports.deinit();

        var inc_it = self.include_paths.iterator();
        while (inc_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.include_paths.deinit();
    }
};

/// Configuration loader
pub const ConfigLoader = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    /// Load cot.json from a directory
    pub fn load(self: *Self, dir_path: []const u8) !ProjectConfig {
        const config_path = try std.fs.path.join(self.allocator, &.{ dir_path, "cot.json" });
        defer self.allocator.free(config_path);

        return self.loadFile(config_path);
    }

    /// Load cot.json from a specific file path
    pub fn loadFile(self: *Self, file_path: []const u8) !ProjectConfig {
        const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return error.ConfigNotFound;
            }
            return err;
        };
        defer file.close();

        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024); // 1MB max
        defer self.allocator.free(content);

        return self.parse(content);
    }

    /// Parse cot.json content
    pub fn parse(self: *Self, content: []const u8) !ProjectConfig {
        const parsed = try json.parseFromSlice(json.Value, self.allocator, content, .{});
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) return error.InvalidConfig;

        var config = try ProjectConfig.init(self.allocator);
        errdefer config.deinit();

        // Required: name
        if (root.object.get("name")) |name_val| {
            if (name_val == .string) {
                self.allocator.free(config.name); // Free default
                config.name = try self.allocator.dupe(u8, name_val.string);
            } else {
                return error.InvalidConfig;
            }
        } else {
            return error.MissingName;
        }

        // Optional: version
        if (root.object.get("version")) |ver_val| {
            if (ver_val == .string) {
                self.allocator.free(config.version); // Free default
                config.version = try self.allocator.dupe(u8, ver_val.string);
            }
        }

        // Optional: type
        if (root.object.get("type")) |type_val| {
            if (type_val == .string) {
                if (std.mem.eql(u8, type_val.string, "app")) {
                    config.project_type = .app;
                } else if (std.mem.eql(u8, type_val.string, "library")) {
                    config.project_type = .library;
                } else if (std.mem.eql(u8, type_val.string, "workspace")) {
                    config.project_type = .workspace;
                }
            }
        }

        // Optional: main
        if (root.object.get("main")) |main_val| {
            if (main_val == .string) {
                config.main = try self.allocator.dupe(u8, main_val.string);
            }
        }

        // Optional: dependencies
        if (root.object.get("dependencies")) |deps_val| {
            if (deps_val == .object) {
                var it = deps_val.object.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.* == .string) {
                        const key = try self.allocator.dupe(u8, entry.key_ptr.*);
                        const val = try self.allocator.dupe(u8, entry.value_ptr.string);
                        try config.dependencies.put(key, val);
                    }
                }
            }
        }

        // Optional: exports (for library packages)
        if (root.object.get("exports")) |exports_val| {
            if (exports_val == .object) {
                var it = exports_val.object.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.* == .string) {
                        const key = try self.allocator.dupe(u8, entry.key_ptr.*);
                        const val = try self.allocator.dupe(u8, entry.value_ptr.string);
                        try config.exports.put(key, val);
                    }
                }
            }
        }

        // Optional: includePaths (for .include directive)
        // Maps logical names to paths, e.g. "comdef": "./defines"
        if (root.object.get("includePaths")) |include_val| {
            if (include_val == .object) {
                var it = include_val.object.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.* == .string) {
                        const key = try self.allocator.dupe(u8, entry.key_ptr.*);
                        const val = try self.allocator.dupe(u8, entry.value_ptr.string);
                        try config.include_paths.put(key, val);
                    }
                }
            }
        }

        // Optional: screens config
        if (root.object.get("screens")) |screens_val| {
            if (screens_val == .object) {
                if (screens_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.screens_dir); // Free default
                        config.screens_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: reports config
        if (root.object.get("reports")) |reports_val| {
            if (reports_val == .object) {
                if (reports_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.reports_dir); // Free default
                        config.reports_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: jobs config
        if (root.object.get("jobs")) |jobs_val| {
            if (jobs_val == .object) {
                if (jobs_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.jobs_dir);
                        config.jobs_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: api config
        if (root.object.get("api")) |api_val| {
            if (api_val == .object) {
                if (api_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.api_dir);
                        config.api_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: subroutines config
        if (root.object.get("subroutines")) |sub_val| {
            if (sub_val == .object) {
                if (sub_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.subroutines_dir);
                        config.subroutines_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: records config
        if (root.object.get("records")) |rec_val| {
            if (rec_val == .object) {
                if (rec_val.object.get("directory")) |dir_val| {
                    if (dir_val == .string) {
                        self.allocator.free(config.records_dir);
                        config.records_dir = try self.allocator.dupe(u8, dir_val.string);
                    }
                }
            }
        }

        // Optional: build config
        if (root.object.get("build")) |build_val| {
            if (build_val == .object) {
                if (build_val.object.get("output")) |out_val| {
                    if (out_val == .string) {
                        self.allocator.free(config.output_dir); // Free default
                        config.output_dir = try self.allocator.dupe(u8, out_val.string);
                    }
                }
                if (build_val.object.get("target")) |target_val| {
                    if (target_val == .string) {
                        self.allocator.free(config.target); // Free default
                        config.target = try self.allocator.dupe(u8, target_val.string);
                    }
                }
                if (build_val.object.get("bundle")) |bundle_val| {
                    if (bundle_val == .bool) {
                        config.bundle = bundle_val.bool;
                    }
                }
                if (build_val.object.get("generateManifest")) |manifest_val| {
                    if (manifest_val == .bool) {
                        config.generate_manifest = manifest_val.bool;
                    }
                }
            }
        }

        // Optional: link config
        if (root.object.get("link")) |link_val| {
            if (link_val == .object) {
                if (link_val.object.get("static")) |static_val| {
                    if (static_val == .bool) {
                        config.static_link = static_val.bool;
                    }
                }
            }
        }

        // Optional: schema repository config
        if (root.object.get("schema")) |schema_val| {
            if (schema_val == .object) {
                var schema_config = SchemaConfig{
                    .file = try self.allocator.dupe(u8, "schema.cot"),
                    .auto_migrate = false,
                    .backend = try self.allocator.dupe(u8, "sqlite"),
                };

                if (schema_val.object.get("file")) |file_val| {
                    if (file_val == .string) {
                        self.allocator.free(schema_config.file);
                        schema_config.file = try self.allocator.dupe(u8, file_val.string);
                    }
                }

                if (schema_val.object.get("autoMigrate")) |migrate_val| {
                    if (migrate_val == .bool) {
                        schema_config.auto_migrate = migrate_val.bool;
                    }
                }

                if (schema_val.object.get("backend")) |backend_val| {
                    if (backend_val == .string) {
                        self.allocator.free(schema_config.backend);
                        schema_config.backend = try self.allocator.dupe(u8, backend_val.string);
                    }
                }

                config.schema = schema_config;
            }
        }

        return config;
    }

    /// Check if a directory contains a cot.json
    pub fn exists(dir_path: []const u8) bool {
        const config_path = std.fs.path.join(std.heap.page_allocator, &.{ dir_path, "cot.json" }) catch return false;
        defer std.heap.page_allocator.free(config_path);

        std.fs.cwd().access(config_path, .{}) catch return false;
        return true;
    }

    /// Find the workspace root by walking up the directory tree
    pub fn findWorkspaceRoot(self: *Self, start_path: []const u8) !?[]const u8 {
        var current = try self.allocator.dupe(u8, start_path);

        while (true) {
            // Check if this directory has a cot.json with workspaces
            const config_path = try std.fs.path.join(self.allocator, &.{ current, "cot.json" });
            defer self.allocator.free(config_path);

            if (std.fs.cwd().openFile(config_path, .{})) |file| {
                file.close();

                // Check if it's a workspace config
                const config = self.loadFile(config_path) catch {
                    // Not a valid config, continue up
                    const parent = std.fs.path.dirname(current);
                    if (parent == null or std.mem.eql(u8, parent.?, current)) {
                        self.allocator.free(current);
                        return null;
                    }
                    const old = current;
                    current = try self.allocator.dupe(u8, parent.?);
                    self.allocator.free(old);
                    continue;
                };
                defer {
                    var cfg = config;
                    cfg.deinit();
                }

                if (config.project_type == .workspace) {
                    return current;
                }
            } else |_| {
                // No config here
            }

            // Move to parent directory
            const parent = std.fs.path.dirname(current);
            if (parent == null or std.mem.eql(u8, parent.?, current)) {
                self.allocator.free(current);
                return null;
            }
            const old = current;
            current = try self.allocator.dupe(u8, parent.?);
            self.allocator.free(old);
        }
    }
};

/// Write a default cot.json for an app to a file
pub fn writeAppConfig(file: std.fs.File, name: []const u8) !void {
    var write_buffer: [4096]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    try writer.print(
        \\{{
        \\  "name": "{s}",
        \\  "version": "1.0.0",
        \\  "type": "app",
        \\  "main": "main.cot",
        \\  "dependencies": {{}},
        \\  "screens": {{
        \\    "autoDiscover": true,
        \\    "directory": "screens"
        \\  }},
        \\  "reports": {{
        \\    "autoDiscover": true,
        \\    "directory": "reports"
        \\  }},
        \\  "jobs": {{
        \\    "autoDiscover": true,
        \\    "directory": "jobs"
        \\  }},
        \\  "subroutines": {{
        \\    "directory": "subroutines"
        \\  }},
        \\  "build": {{
        \\    "output": "bin",
        \\    "target": "bytecode",
        \\    "bundle": true,
        \\    "generateManifest": true
        \\  }},
        \\  "link": {{
        \\    "static": true
        \\  }}
        \\}}
        \\
    , .{name});
    try buffered.interface.flush();
}

/// Write a cot.json for an app with a custom main file
pub fn writeAppConfigWithMain(file: std.fs.File, name: []const u8, main_file: []const u8) !void {
    var write_buffer: [4096]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    try writer.print(
        \\{{
        \\  "name": "{s}",
        \\  "version": "1.0.0",
        \\  "type": "app",
        \\  "main": "{s}",
        \\  "dependencies": {{}},
        \\  "screens": {{
        \\    "autoDiscover": true,
        \\    "directory": "screens"
        \\  }},
        \\  "reports": {{
        \\    "autoDiscover": true,
        \\    "directory": "reports"
        \\  }},
        \\  "jobs": {{
        \\    "autoDiscover": true,
        \\    "directory": "jobs"
        \\  }},
        \\  "subroutines": {{
        \\    "directory": "subroutines"
        \\  }},
        \\  "build": {{
        \\    "output": "bin",
        \\    "target": "bytecode",
        \\    "bundle": true,
        \\    "generateManifest": true
        \\  }},
        \\  "link": {{
        \\    "static": true
        \\  }}
        \\}}
        \\
    , .{ name, main_file });
    try buffered.interface.flush();
}

/// Write a default cot.json for a library to a file
pub fn writeLibraryConfig(file: std.fs.File, name: []const u8) !void {
    var write_buffer: [4096]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    try writer.print(
        \\{{
        \\  "name": "@company/{s}",
        \\  "version": "1.0.0",
        \\  "type": "library",
        \\  "main": "src/index.cot",
        \\  "exports": {{}}
        \\}}
        \\
    , .{name});
    try buffered.interface.flush();
}

/// Write a default cot.json for a workspace to a file
pub fn writeWorkspaceConfig(file: std.fs.File, name: []const u8) !void {
    var write_buffer: [4096]u8 = undefined;
    var buffered = file.writer(&write_buffer);
    const writer = &buffered.interface;

    try writer.print(
        \\{{
        \\  "name": "{s}",
        \\  "version": "1.0.0",
        \\  "private": true,
        \\  "type": "workspace",
        \\  "workspaces": ["apps/*", "packages/*"],
        \\  "cot": {{
        \\    "version": "0.1.0"
        \\  }}
        \\}}
        \\
    , .{name});
    try buffered.interface.flush();
}

/// Generate a default cot.json for an app (deprecated - use writeAppConfig)
pub fn generateAppConfig(name: []const u8) ![]const u8 {
    _ = name;
    return error.Deprecated;
}

/// Generate a default cot.json for a library (deprecated - use writeLibraryConfig)
pub fn generateLibraryConfig(name: []const u8) ![]const u8 {
    _ = name;
    return error.Deprecated;
}

/// Generate a default cot.json for a workspace (deprecated - use writeWorkspaceConfig)
pub fn generateWorkspaceConfig(name: []const u8) ![]const u8 {
    _ = name;
    return error.Deprecated;
}

// Tests
test "parse app config" {
    const allocator = std.testing.allocator;
    var loader = ConfigLoader.init(allocator);

    const content =
        \\{
        \\  "name": "test-app",
        \\  "version": "1.0.0",
        \\  "type": "app",
        \\  "main": "main.cot"
        \\}
    ;

    var config = try loader.parse(content);
    defer config.deinit();

    try std.testing.expectEqualStrings("test-app", config.name);
    try std.testing.expectEqualStrings("1.0.0", config.version);
    try std.testing.expect(config.project_type == .app);
    try std.testing.expectEqualStrings("main.cot", config.main.?);
}

test "parse library config" {
    const allocator = std.testing.allocator;
    var loader = ConfigLoader.init(allocator);

    const content =
        \\{
        \\  "name": "@company/common",
        \\  "type": "library"
        \\}
    ;

    var config = try loader.parse(content);
    defer config.deinit();

    try std.testing.expectEqualStrings("@company/common", config.name);
    try std.testing.expect(config.project_type == .library);
}

test "parse schema config" {
    const allocator = std.testing.allocator;
    var loader = ConfigLoader.init(allocator);

    const content =
        \\{
        \\  "name": "test-app",
        \\  "type": "app",
        \\  "schema": {
        \\    "file": "db/schema.cot",
        \\    "autoMigrate": true,
        \\    "backend": "postgres"
        \\  }
        \\}
    ;

    var config = try loader.parse(content);
    defer config.deinit();

    try std.testing.expect(config.schema != null);
    try std.testing.expectEqualStrings("db/schema.cot", config.schema.?.file);
    try std.testing.expect(config.schema.?.auto_migrate);
    try std.testing.expectEqualStrings("postgres", config.schema.?.backend);
}
