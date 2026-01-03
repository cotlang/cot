//! Cot Module Linker
//!
//! Loads compiled bytecode modules (.cbo files) and links their
//! exported subroutines into the runtime environment.

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");

/// Linked module information
pub const LinkedModule = struct {
    name: []const u8,
    module: *bytecode.Module,
    path: []const u8,
    load_order: u32,
};

/// Module search paths
pub const SearchPaths = struct {
    paths: std.ArrayListAligned([]const u8, null),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .paths = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.paths.items) |path| {
            self.allocator.free(path);
        }
        self.paths.deinit(self.allocator);
    }

    pub fn addPath(self: *Self, path: []const u8) !void {
        const duped = try self.allocator.dupe(u8, path);
        try self.paths.append(self.allocator, duped);
    }

    /// Find a module file by name
    pub fn findModule(self: *Self, name: []const u8) ?[]const u8 {
        // First try the name as-is
        if (std.fs.cwd().access(name, .{})) {
            return name;
        } else |_| {}

        // Try with .cbo extension
        var buf: [512]u8 = undefined;
        const with_ext = std.fmt.bufPrint(&buf, "{s}.cbo", .{name}) catch return null;

        if (std.fs.cwd().access(with_ext, .{})) {
            return with_ext;
        } else |_| {}

        // Search in configured paths
        for (self.paths.items) |search_path| {
            // Try path/name.cbo
            const full_path = std.fmt.bufPrint(&buf, "{s}/{s}.cbo", .{ search_path, name }) catch continue;

            if (std.fs.cwd().access(full_path, .{})) {
                return full_path;
            } else |_| {}
        }

        return null;
    }
};

/// Linker errors
pub const LinkerError = error{
    ModuleNotFound,
    InvalidModule,
    CircularDependency,
    SymbolConflict,
    UnresolvedImport,
    OutOfMemory,
    IoError,
};

/// Module linker
pub const Linker = struct {
    allocator: std.mem.Allocator,
    loaded_modules: std.StringHashMap(*LinkedModule),
    search_paths: SearchPaths,
    load_counter: u32,
    stdlib_path: ?[]const u8,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var linker = Self{
            .allocator = allocator,
            .loaded_modules = std.StringHashMap(*LinkedModule).init(allocator),
            .search_paths = SearchPaths.init(allocator),
            .load_counter = 0,
            .stdlib_path = null,
        };

        // Add default search paths
        linker.addDefaultPaths() catch {};

        return linker;
    }

    pub fn deinit(self: *Self) void {
        var it = self.loaded_modules.valueIterator();
        while (it.next()) |linked| {
            self.allocator.free(linked.*.name);
            self.allocator.free(linked.*.path);
            linked.*.module.deinit();
            self.allocator.destroy(linked.*.module);
            self.allocator.destroy(linked.*);
        }
        self.loaded_modules.deinit();
        self.search_paths.deinit();
        if (self.stdlib_path) |sp| {
            self.allocator.free(sp);
        }
    }

    /// Add default module search paths
    fn addDefaultPaths(self: *Self) !void {
        // Current directory
        try self.search_paths.addPath(".");

        // Standard library path (if set)
        if (std.process.getEnvVarOwned(self.allocator, "COT_LIB")) |lib_path| {
            defer self.allocator.free(lib_path);
            try self.search_paths.addPath(lib_path);
        } else |_| {}

        // Home directory lib
        if (std.process.getEnvVarOwned(self.allocator, "HOME")) |home| {
            defer self.allocator.free(home);
            var buf: [512]u8 = undefined;
            const lib = std.fmt.bufPrint(&buf, "{s}/.cot/lib", .{home}) catch return;
            try self.search_paths.addPath(lib);
        } else |_| {}
    }

    /// Set the standard library path
    pub fn setStdlibPath(self: *Self, path: []const u8) !void {
        if (self.stdlib_path) |old| {
            self.allocator.free(old);
        }
        self.stdlib_path = try self.allocator.dupe(u8, path);
        try self.search_paths.addPath(path);
    }

    /// Load a module by name
    pub fn loadModule(self: *Self, name: []const u8) LinkerError!*LinkedModule {
        // Check if already loaded
        if (self.loaded_modules.get(name)) |existing| {
            return existing;
        }

        // Find module file
        const path = self.search_paths.findModule(name) orelse {
            return LinkerError.ModuleNotFound;
        };

        // Open and read module file into memory
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return LinkerError.IoError;
        };
        defer file.close();

        const bytes = file.readToEndAlloc(self.allocator, 1024 * 1024 * 100) catch {
            return LinkerError.IoError;
        };
        defer self.allocator.free(bytes);

        // Create module
        const mod = self.allocator.create(bytecode.Module) catch {
            return LinkerError.OutOfMemory;
        };
        errdefer self.allocator.destroy(mod);

        // Deserialize from memory buffer
        var fbs = std.io.fixedBufferStream(bytes);
        mod.* = bytecode.Module.deserialize(self.allocator, fbs.reader()) catch {
            return LinkerError.InvalidModule;
        };
        errdefer mod.deinit();

        // Create linked module info
        const linked = self.allocator.create(LinkedModule) catch {
            return LinkerError.OutOfMemory;
        };
        errdefer self.allocator.destroy(linked);

        linked.* = .{
            .name = self.allocator.dupe(u8, name) catch return LinkerError.OutOfMemory,
            .module = mod,
            .path = self.allocator.dupe(u8, path) catch return LinkerError.OutOfMemory,
            .load_order = self.load_counter,
        };
        self.load_counter += 1;

        // Register in map
        self.loaded_modules.put(linked.name, linked) catch {
            return LinkerError.OutOfMemory;
        };

        // TODO: Process imports and load dependencies

        return linked;
    }

    /// Load a module from a specific path
    pub fn loadModuleFromPath(self: *Self, path: []const u8) LinkerError!*LinkedModule {
        // Extract module name from path
        const base = std.fs.path.basename(path);
        const name = if (std.mem.endsWith(u8, base, ".cbo"))
            base[0 .. base.len - 4]
        else
            base;

        // Check if already loaded
        if (self.loaded_modules.get(name)) |existing| {
            return existing;
        }

        // Open and read module file into memory
        const file = std.fs.cwd().openFile(path, .{}) catch {
            return LinkerError.IoError;
        };
        defer file.close();

        const bytes = file.readToEndAlloc(self.allocator, 1024 * 1024 * 100) catch {
            return LinkerError.IoError;
        };
        defer self.allocator.free(bytes);

        // Create module
        const mod = self.allocator.create(bytecode.Module) catch {
            return LinkerError.OutOfMemory;
        };
        errdefer self.allocator.destroy(mod);

        // Deserialize from memory buffer
        var fbs = std.io.fixedBufferStream(bytes);
        mod.* = bytecode.Module.deserialize(self.allocator, fbs.reader()) catch {
            return LinkerError.InvalidModule;
        };
        errdefer mod.deinit();

        // Create linked module info
        const linked = self.allocator.create(LinkedModule) catch {
            return LinkerError.OutOfMemory;
        };
        errdefer self.allocator.destroy(linked);

        linked.* = .{
            .name = self.allocator.dupe(u8, name) catch return LinkerError.OutOfMemory,
            .module = mod,
            .path = self.allocator.dupe(u8, path) catch return LinkerError.OutOfMemory,
            .load_order = self.load_counter,
        };
        self.load_counter += 1;

        // Register in map
        self.loaded_modules.put(linked.name, linked) catch {
            return LinkerError.OutOfMemory;
        };

        return linked;
    }

    /// Get all exported subroutine names from a module
    pub fn getExports(self: *Self, module_name: []const u8) ![]const []const u8 {
        const linked = self.loaded_modules.get(module_name) orelse {
            return LinkerError.ModuleNotFound;
        };

        var exports = std.ArrayListAligned([]const u8, null).empty;
        errdefer exports.deinit(self.allocator);

        for (linked.module.exports) |exp| {
            if (exp.kind == .routine) {
                if (linked.module.getConstant(exp.name_index)) |c| {
                    switch (c) {
                        .identifier => |name| try exports.append(self.allocator, name),
                        else => {},
                    }
                }
            }
        }

        return exports.toOwnedSlice(self.allocator);
    }

    /// Get a specific module
    pub fn getModule(self: *Self, name: []const u8) ?*bytecode.Module {
        const linked = self.loaded_modules.get(name) orelse return null;
        return linked.module;
    }

    /// Check if a module is loaded
    pub fn isLoaded(self: *Self, name: []const u8) bool {
        return self.loaded_modules.contains(name);
    }

    /// Get all loaded module names
    pub fn getLoadedModules(self: *Self) ![]const []const u8 {
        var names = std.ArrayListAligned([]const u8, null).empty;
        errdefer names.deinit(self.allocator);

        var it = self.loaded_modules.keyIterator();
        while (it.next()) |key| {
            try names.append(self.allocator, key.*);
        }

        return names.toOwnedSlice(self.allocator);
    }
};

test "linker init" {
    const allocator = std.testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    // Should have search paths
    try std.testing.expect(linker.search_paths.paths.items.len >= 1);
}
