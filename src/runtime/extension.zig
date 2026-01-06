//! Cot Extension Interface
//!
//! Extensions are optional modules that provide additional native functions to Cot programs.
//! Examples: HTTP client, database drivers, etc.
//!
//! Extensions are linked at build time based on the "extensions" field in cot.json.
//! This keeps the core runtime lean while allowing modular feature additions.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// VM value type (imported from bytecode)
pub const Value = @import("bytecode/vm.zig").Value;

/// Native function context
pub const NativeContext = @import("native/native.zig").NativeContext;

/// Error type for native function handlers
pub const NativeError = @import("native/native.zig").NativeError;

/// Native function handler signature
pub const NativeHandler = *const fn (*NativeContext) NativeError!?Value;

/// Definition of a single native function provided by an extension
pub const NativeDef = struct {
    /// Name of the function (e.g., "http_get", "db_connect")
    name: []const u8,
    /// Handler function
    handler: NativeHandler,
    /// Minimum number of arguments
    min_args: u8 = 0,
    /// Maximum number of arguments (255 = unlimited)
    max_args: u8 = 255,
};

/// Extension definition
/// Each extension package exports a const `extension` of this type.
pub const Extension = struct {
    /// Extension identifier (e.g., "http", "postgres")
    name: []const u8,
    /// Semantic version string
    version: []const u8,
    /// List of native functions this extension provides
    natives: []const NativeDef,
    /// Optional initialization function (called once when extension loads)
    init: ?*const fn (Allocator) void = null,
    /// Optional cleanup function (called on shutdown)
    deinit: ?*const fn () void = null,
};

/// Extension registry - tracks all loaded extensions
pub const ExtensionRegistry = struct {
    allocator: Allocator,
    extensions: std.ArrayListUnmanaged(Extension),
    native_map: std.StringHashMap(NativeHandler),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .extensions = .{},
            .native_map = std.StringHashMap(NativeHandler).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // Call deinit on all extensions
        for (self.extensions.items) |ext| {
            if (ext.deinit) |deinit_fn| {
                deinit_fn();
            }
        }
        self.extensions.deinit(self.allocator);
        self.native_map.deinit();
    }

    /// Register an extension
    pub fn register(self: *Self, ext: Extension) !void {
        // Call init if provided
        if (ext.init) |init_fn| {
            init_fn(self.allocator);
        }

        // Register all native functions
        for (ext.natives) |native| {
            try self.native_map.put(native.name, native.handler);
        }

        try self.extensions.append(self.allocator, ext);
    }

    /// Look up a native function handler by name
    pub fn getNative(self: *Self, name: []const u8) ?NativeHandler {
        return self.native_map.get(name);
    }

    /// Check if an extension is loaded
    pub fn hasExtension(self: *Self, name: []const u8) bool {
        for (self.extensions.items) |ext| {
            if (std.mem.eql(u8, ext.name, name)) {
                return true;
            }
        }
        return false;
    }
};

/// Global extension registry instance
var global_registry: ?ExtensionRegistry = null;

/// Initialize the global extension registry
pub fn initRegistry(allocator: Allocator) void {
    if (global_registry == null) {
        global_registry = ExtensionRegistry.init(allocator);
    }
}

/// Get the global extension registry
pub fn getRegistry() ?*ExtensionRegistry {
    if (global_registry) |*reg| {
        return reg;
    }
    return null;
}

/// Shutdown the global extension registry
pub fn deinitRegistry() void {
    if (global_registry) |*reg| {
        reg.deinit();
        global_registry = null;
    }
}

/// Register an extension with the global registry
pub fn registerExtension(ext: Extension) !void {
    if (getRegistry()) |reg| {
        try reg.register(ext);
    }
}
