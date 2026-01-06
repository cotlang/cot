//! Package System
//!
//! Go-inspired package-based compilation model.
//! A package is a directory containing .cot files that compile together
//! as a single unit, producing one .cbo file.

const std = @import("std");
const config = @import("config.zig");
const workspace = @import("workspace.zig");
const Allocator = std.mem.Allocator;

/// Symbol visibility
pub const Visibility = enum {
    /// Public - exported from package, accessible via pkg.Symbol
    public,
    /// Private - only accessible within the package
    private,
};

/// Kind of exported symbol
pub const SymbolKind = enum {
    function,
    struct_type,
    enum_type,
    const_value,
    global_var,
};

/// A symbol exported from a package
pub const ExportedSymbol = struct {
    name: []const u8,
    kind: SymbolKind,
    /// Offset in the package's bytecode (for functions)
    /// or type descriptor index (for types)
    offset: u32,
    /// Type information for cross-package type checking
    type_desc: ?TypeDescriptor,
};

/// Type descriptor for cross-package type identity
/// Two types are equal if package + name match
pub const TypeDescriptor = struct {
    package: []const u8,
    name: []const u8,
    kind: TypeDescriptorKind,
    /// For structs: field information
    fields: ?[]const FieldDescriptor,
    /// For enums: variant names
    variants: ?[]const []const u8,
    /// For functions: signature
    signature: ?FunctionSignature,

    pub fn eql(self: TypeDescriptor, other: TypeDescriptor) bool {
        return std.mem.eql(u8, self.package, other.package) and
            std.mem.eql(u8, self.name, other.name);
    }

    pub fn hash(self: TypeDescriptor) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(self.package);
        h.update(":");
        h.update(self.name);
        return h.final();
    }
};

pub const TypeDescriptorKind = enum {
    primitive,
    struct_type,
    enum_type,
    function_type,
    pointer,
    optional,
    array,
    slice,
};

pub const FieldDescriptor = struct {
    name: []const u8,
    type_ref: []const u8, // "pkg.Type" or primitive name
    offset: u32,
};

pub const FunctionSignature = struct {
    params: []const []const u8, // type refs
    return_type: []const u8,
};

/// An import reference that needs to be resolved at link time
pub const ImportRef = struct {
    /// Package name being imported
    package: []const u8,
    /// Symbol name being used
    symbol: []const u8,
    /// Where in bytecode this reference occurs
    usage_offset: u32,
    /// What kind of reference
    kind: enum { call, type_ref, const_ref, field_access },
};

/// Export table for a compiled package
pub const ExportTable = struct {
    entries: std.StringHashMapUnmanaged(ExportedSymbol),
    allocator: Allocator,
    /// If true, this table owns its key strings and must free them
    owns_keys: bool,

    pub fn init(allocator: Allocator) ExportTable {
        return .{
            .entries = .empty,
            .allocator = allocator,
            .owns_keys = false,
        };
    }

    pub fn deinit(self: *ExportTable) void {
        if (self.owns_keys) {
            // Free allocated name strings (from deserialization)
            var it = self.entries.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
        }
        self.entries.deinit(self.allocator);
    }

    pub fn put(self: *ExportTable, name: []const u8, symbol: ExportedSymbol) !void {
        try self.entries.put(self.allocator, name, symbol);
    }

    pub fn get(self: *const ExportTable, name: []const u8) ?ExportedSymbol {
        return self.entries.get(name);
    }

    /// Serialize export table to bytes for embedding in .cbo
    pub fn serialize(self: *const ExportTable, allocator: Allocator) ![]u8 {
        var list: std.ArrayListUnmanaged(u8) = .empty;
        errdefer list.deinit(allocator);

        // Write entry count
        const count: u32 = @intCast(self.entries.count());
        try list.appendSlice(allocator, std.mem.asBytes(&count));

        // Write each entry
        var it = self.entries.iterator();
        while (it.next()) |entry| {
            // Name length + name
            const name_len: u32 = @intCast(entry.key_ptr.len);
            try list.appendSlice(allocator, std.mem.asBytes(&name_len));
            try list.appendSlice(allocator, entry.key_ptr.*);

            // Kind
            try list.append(allocator, @intFromEnum(entry.value_ptr.kind));

            // Offset
            try list.appendSlice(allocator, std.mem.asBytes(&entry.value_ptr.offset));
        }

        return list.toOwnedSlice(allocator);
    }

    /// Deserialize export table from bytes
    pub fn deserialize(allocator: Allocator, bytes: []const u8) !ExportTable {
        var table = ExportTable.init(allocator);
        table.owns_keys = true; // Deserialized tables own their string allocations
        errdefer table.deinit();

        var offset: usize = 0;

        // Read entry count
        const count = std.mem.readInt(u32, bytes[offset..][0..4], .little);
        offset += 4;

        // Read each entry
        for (0..count) |_| {
            // Name
            const name_len = std.mem.readInt(u32, bytes[offset..][0..4], .little);
            offset += 4;
            const name = try allocator.dupe(u8, bytes[offset..][0..name_len]);
            offset += name_len;

            // Kind
            const kind: SymbolKind = @enumFromInt(bytes[offset]);
            offset += 1;

            // Offset
            const sym_offset = std.mem.readInt(u32, bytes[offset..][0..4], .little);
            offset += 4;

            try table.put(name, .{
                .name = name,
                .kind = kind,
                .offset = sym_offset,
                .type_desc = null, // TODO: deserialize type descriptors
            });
        }

        return table;
    }
};

/// A package in the compilation system
pub const Package = struct {
    /// Package name (e.g., "token", "lexer")
    name: []const u8,
    /// Filesystem path to package directory
    path: []const u8,
    /// Source files in this package
    source_files: std.ArrayListUnmanaged([]const u8),
    /// Packages this one depends on (import names)
    dependencies: std.ArrayListUnmanaged([]const u8),
    /// Export table (populated after compilation)
    exports: ?ExportTable,
    /// Compiled bytecode (populated after compilation)
    bytecode: ?[]u8,
    /// Whether this package has been compiled
    compiled: bool,
    /// Allocator used for this package
    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8, path: []const u8) !Package {
        return .{
            .name = try allocator.dupe(u8, name),
            .path = try allocator.dupe(u8, path),
            .source_files = .empty,
            .dependencies = .empty,
            .exports = null,
            .bytecode = null,
            .compiled = false,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Package) void {
        self.allocator.free(self.name);
        self.allocator.free(self.path);

        for (self.source_files.items) |f| {
            self.allocator.free(f);
        }
        self.source_files.deinit(self.allocator);

        for (self.dependencies.items) |d| {
            self.allocator.free(d);
        }
        self.dependencies.deinit(self.allocator);

        if (self.exports) |*exp| {
            exp.deinit();
        }

        if (self.bytecode) |bc| {
            self.allocator.free(bc);
        }
    }

    /// Add a source file to this package
    pub fn addSourceFile(self: *Package, file_path: []const u8) !void {
        try self.source_files.append(self.allocator, try self.allocator.dupe(u8, file_path));
    }

    /// Add a dependency on another package
    pub fn addDependency(self: *Package, package_name: []const u8) !void {
        // Don't add duplicates
        for (self.dependencies.items) |dep| {
            if (std.mem.eql(u8, dep, package_name)) return;
        }
        try self.dependencies.append(self.allocator, try self.allocator.dupe(u8, package_name));
    }
};

/// Manages all packages in a workspace
pub const PackageManager = struct {
    /// All discovered packages by name
    packages: std.StringHashMapUnmanaged(*Package),
    /// Build order (topological sort of dependencies)
    build_order: std.ArrayListUnmanaged([]const u8),
    /// Workspace root path
    workspace_root: []const u8,
    /// Allocator
    allocator: Allocator,

    pub fn init(allocator: Allocator, workspace_root: []const u8) !PackageManager {
        return .{
            .packages = .empty,
            .build_order = .empty,
            .workspace_root = try allocator.dupe(u8, workspace_root),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *PackageManager) void {
        var it = self.packages.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.packages.deinit(self.allocator);

        for (self.build_order.items) |name| {
            self.allocator.free(name);
        }
        self.build_order.deinit(self.allocator);

        self.allocator.free(self.workspace_root);
    }

    /// Register a package
    pub fn registerPackage(self: *PackageManager, pkg: *Package) !void {
        try self.packages.put(self.allocator, pkg.name, pkg);
    }

    /// Look up a package by name
    pub fn getPackage(self: *const PackageManager, name: []const u8) ?*Package {
        return self.packages.get(name);
    }

    /// Discover packages in the workspace
    /// This reads cot.json and finds all package directories
    pub fn discoverPackages(self: *PackageManager) !void {
        // First try to load cot.json for explicit package mappings
        const config_path = try std.fs.path.join(self.allocator, &.{ self.workspace_root, "cot.json" });
        defer self.allocator.free(config_path);

        if (std.fs.cwd().openFile(config_path, .{})) |file| {
            defer file.close();
            try self.loadFromConfig(file);
        } else |_| {
            // No cot.json - use auto-discovery
            try self.autoDiscoverPackages();
        }
    }

    /// Load package definitions from cot.json
    fn loadFromConfig(self: *PackageManager, file: std.fs.File) !void {
        const content = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(content);

        const parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, content, .{});
        defer parsed.deinit();

        const root = parsed.value;

        // Check for "packages" object in cot.json
        if (root.object.get("packages")) |packages_val| {
            if (packages_val == .object) {
                var pkg_it = packages_val.object.iterator();
                while (pkg_it.next()) |entry| {
                    const pkg_name = entry.key_ptr.*;
                    const pkg_path_val = entry.value_ptr.*;

                    if (pkg_path_val == .string) {
                        const rel_path = pkg_path_val.string;
                        const abs_path = try std.fs.path.join(self.allocator, &.{ self.workspace_root, rel_path });
                        defer self.allocator.free(abs_path);

                        try self.discoverPackageAt(pkg_name, abs_path);
                    }
                }
            }
        }

        // Also check for "exports" which maps export names to paths
        if (root.object.get("exports")) |exports_val| {
            if (exports_val == .object) {
                var exp_it = exports_val.object.iterator();
                while (exp_it.next()) |entry| {
                    const export_name = entry.key_ptr.*;
                    const export_path = entry.value_ptr.*;

                    if (export_path == .string) {
                        // Convert export path to package
                        // e.g., "./src/token.cot" -> package "token" at "src/token"
                        const rel_path = export_path.string;

                        // Skip the "./" prefix if present
                        const clean_path = if (std.mem.startsWith(u8, rel_path, "./"))
                            rel_path[2..]
                        else
                            rel_path;

                        // Remove .cot extension to get directory
                        const without_ext = if (std.mem.endsWith(u8, clean_path, ".cot"))
                            clean_path[0 .. clean_path.len - 4]
                        else
                            clean_path;

                        // Get package name from export name
                        // e.g., "./token" -> "token", "." -> use project name
                        var pkg_name: []const u8 = undefined;
                        if (std.mem.eql(u8, export_name, ".")) {
                            // Main export - use directory name
                            pkg_name = std.fs.path.basename(without_ext);
                        } else if (std.mem.startsWith(u8, export_name, "./")) {
                            pkg_name = export_name[2..];
                        } else {
                            pkg_name = export_name;
                        }

                        const abs_path = try std.fs.path.join(self.allocator, &.{ self.workspace_root, without_ext });
                        defer self.allocator.free(abs_path);

                        // Check if it's a directory or a single file
                        if (std.fs.cwd().openDir(abs_path, .{})) |_| {
                            // It's a directory
                            try self.discoverPackageAt(pkg_name, abs_path);
                        } else |_| {
                            // It's a single file - create a single-file package
                            const file_path_with_ext = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{abs_path});
                            defer self.allocator.free(file_path_with_ext);
                            try self.discoverSingleFilePackage(pkg_name, file_path_with_ext);
                        }
                    }
                }
            }
        }
    }

    /// Auto-discover packages by scanning directories
    fn autoDiscoverPackages(self: *PackageManager) !void {
        // Check common locations: src/, packages/, apps/
        const locations = [_][]const u8{ "src", "packages", "apps" };

        for (locations) |loc| {
            const loc_path = try std.fs.path.join(self.allocator, &.{ self.workspace_root, loc });
            defer self.allocator.free(loc_path);

            if (std.fs.cwd().openDir(loc_path, .{ .iterate = true })) |dir| {
                var loc_dir = dir;
                defer loc_dir.close();

                var it = loc_dir.iterate();
                while (try it.next()) |entry| {
                    if (entry.kind == .directory) {
                        const pkg_path = try std.fs.path.join(self.allocator, &.{ loc_path, entry.name });
                        defer self.allocator.free(pkg_path);

                        // Check if directory contains .cot files
                        if (try self.hasCotFiles(pkg_path)) {
                            try self.discoverPackageAt(entry.name, pkg_path);
                        }
                    }
                }
            } else |_| {
                // Directory doesn't exist, skip
            }
        }

        // Also check if workspace root itself has .cot files (single-package project)
        if (try self.hasCotFiles(self.workspace_root)) {
            const name = std.fs.path.basename(self.workspace_root);
            try self.discoverPackageAt(name, self.workspace_root);
        }
    }

    /// Check if a directory contains .cot files
    fn hasCotFiles(self: *PackageManager, dir_path: []const u8) !bool {
        _ = self;
        if (std.fs.cwd().openDir(dir_path, .{ .iterate = true })) |dir| {
            var check_dir = dir;
            defer check_dir.close();

            var it = check_dir.iterate();
            while (try it.next()) |entry| {
                if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".cot")) {
                    return true;
                }
            }
        } else |_| {
            return false;
        }
        return false;
    }

    /// Discover a package at the given path
    fn discoverPackageAt(self: *PackageManager, name: []const u8, path: []const u8) !void {
        // Skip if already discovered
        if (self.packages.contains(name)) return;

        const pkg_ptr = try self.allocator.create(Package);
        pkg_ptr.* = try Package.init(self.allocator, name, path);
        errdefer {
            pkg_ptr.deinit();
            self.allocator.destroy(pkg_ptr);
        }

        // Find all .cot files in the package directory
        if (std.fs.cwd().openDir(path, .{ .iterate = true })) |dir| {
            var pkg_dir = dir;
            defer pkg_dir.close();

            var it = pkg_dir.iterate();
            while (try it.next()) |entry| {
                if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".cot")) {
                    const file_path = try std.fs.path.join(self.allocator, &.{ path, entry.name });
                    try pkg_ptr.addSourceFile(file_path);
                    self.allocator.free(file_path);
                }
            }
        } else |_| {
            // Check if it's a single file
            const file_with_ext = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{path});
            defer self.allocator.free(file_with_ext);

            if (std.fs.cwd().access(file_with_ext, .{})) |_| {
                try pkg_ptr.addSourceFile(file_with_ext);
            } else |_| {}
        }

        try self.registerPackage(pkg_ptr);
    }

    /// Discover a single-file package
    fn discoverSingleFilePackage(self: *PackageManager, name: []const u8, file_path: []const u8) !void {
        // Skip if already discovered
        if (self.packages.contains(name)) return;

        // Use the file's directory as the package path
        const dir_path = std.fs.path.dirname(file_path) orelse self.workspace_root;

        const pkg_ptr = try self.allocator.create(Package);
        pkg_ptr.* = try Package.init(self.allocator, name, dir_path);
        errdefer {
            pkg_ptr.deinit();
            self.allocator.destroy(pkg_ptr);
        }

        // Check if file exists and add it
        if (std.fs.cwd().access(file_path, .{})) |_| {
            try pkg_ptr.addSourceFile(file_path);
        } else |_| {
            // File doesn't exist, clean up and return
            pkg_ptr.deinit();
            self.allocator.destroy(pkg_ptr);
            return;
        }

        try self.registerPackage(pkg_ptr);
    }

    /// Compute build order using topological sort
    /// Must be called after all dependencies are discovered
    pub fn computeBuildOrder(self: *PackageManager) !void {
        // Clear existing build order
        for (self.build_order.items) |name| {
            self.allocator.free(name);
        }
        self.build_order.clearRetainingCapacity();

        // Track visited packages
        var visited = std.StringHashMapUnmanaged(bool).empty;
        defer visited.deinit(self.allocator);

        var in_progress = std.StringHashMapUnmanaged(bool).empty;
        defer in_progress.deinit(self.allocator);

        // Visit each package
        var it = self.packages.iterator();
        while (it.next()) |entry| {
            try self.visitPackage(entry.key_ptr.*, &visited, &in_progress);
        }
    }

    /// DFS visit for topological sort
    fn visitPackage(
        self: *PackageManager,
        name: []const u8,
        visited: *std.StringHashMapUnmanaged(bool),
        in_progress: *std.StringHashMapUnmanaged(bool),
    ) !void {
        if (visited.contains(name)) return;

        if (in_progress.contains(name)) {
            // Circular dependency detected
            std.debug.print("Error: Circular dependency detected involving package '{s}'\n", .{name});
            return error.CircularDependency;
        }

        try in_progress.put(self.allocator, name, true);

        // Visit dependencies first
        if (self.packages.get(name)) |pkg| {
            for (pkg.dependencies.items) |dep| {
                try self.visitPackage(dep, visited, in_progress);
            }
        }

        _ = in_progress.remove(name);
        try visited.put(self.allocator, name, true);
        try self.build_order.append(self.allocator, try self.allocator.dupe(u8, name));
    }

    /// Resolve an import path to a package
    /// Returns the package name for the given import
    pub fn resolveImport(
        self: *const PackageManager,
        from_package: []const u8,
        import_path: []const u8,
    ) !?[]const u8 {
        // Handle relative imports (../foo, ./bar)
        if (std.mem.startsWith(u8, import_path, ".")) {
            const from_pkg = self.packages.get(from_package) orelse return null;
            const resolved = try std.fs.path.resolve(self.allocator, &.{ from_pkg.path, import_path });
            defer self.allocator.free(resolved);

            // Find package at that path
            var it = self.packages.iterator();
            while (it.next()) |entry| {
                if (std.mem.eql(u8, entry.value_ptr.*.path, resolved)) {
                    return entry.key_ptr.*;
                }
            }
            return null;
        }

        // Direct package name lookup
        if (self.packages.contains(import_path)) {
            return import_path;
        }

        return null;
    }

    /// Print package dependency graph
    pub fn printDependencyGraph(self: *const PackageManager, writer: anytype) !void {
        try writer.writeAll("Package Dependency Graph:\n");
        try writer.writeAll("========================\n\n");

        var it = self.packages.iterator();
        while (it.next()) |entry| {
            const pkg = entry.value_ptr.*;
            try writer.print("{s}:\n", .{pkg.name});
            try writer.print("  path: {s}\n", .{pkg.path});
            try writer.print("  files: {d}\n", .{pkg.source_files.items.len});

            if (pkg.dependencies.items.len > 0) {
                try writer.writeAll("  depends on: ");
                for (pkg.dependencies.items, 0..) |dep, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{s}", .{dep});
                }
                try writer.writeAll("\n");
            }
            try writer.writeAll("\n");
        }

        if (self.build_order.items.len > 0) {
            try writer.writeAll("Build order: ");
            for (self.build_order.items, 0..) |name, i| {
                if (i > 0) try writer.writeAll(" -> ");
                try writer.print("{s}", .{name});
            }
            try writer.writeAll("\n");
        }
    }
};

// Tests
test "package init and deinit" {
    const allocator = std.testing.allocator;
    var pkg = try Package.init(allocator, "test", "/path/to/test");
    defer pkg.deinit();

    try pkg.addSourceFile("/path/to/test/foo.cot");
    try pkg.addDependency("other");

    try std.testing.expectEqual(@as(usize, 1), pkg.source_files.items.len);
    try std.testing.expectEqual(@as(usize, 1), pkg.dependencies.items.len);
}

test "export table serialize/deserialize" {
    const allocator = std.testing.allocator;
    var table = ExportTable.init(allocator);
    defer table.deinit();

    try table.put("foo", .{
        .name = "foo",
        .kind = .function,
        .offset = 42,
        .type_desc = null,
    });

    const bytes = try table.serialize(allocator);
    defer allocator.free(bytes);

    var restored = try ExportTable.deserialize(allocator, bytes);
    defer restored.deinit();

    const sym = restored.get("foo");
    try std.testing.expect(sym != null);
    try std.testing.expectEqual(@as(u32, 42), sym.?.offset);
}
