//! Extension Manager
//!
//! Manages loading, initialization, and cleanup of VM extensions.
//! Handles dependency resolution and load order.

const std = @import("std");
const Allocator = std.mem.Allocator;

const type_registry = @import("type_registry.zig");
const TypeRegistry = type_registry.TypeRegistry;
const TypeDef = type_registry.TypeDef;
const OpcodeRegistry = @import("opcode_registry.zig").OpcodeRegistry;

/// Extension definition - provided by extension authors
pub const Extension = struct {
    /// Unique name for this extension
    name: []const u8,

    /// Version number (for compatibility checking)
    version: u32,

    /// Dependencies on other extensions (loaded first)
    dependencies: []const []const u8 = &.{},

    /// Called when extension is loaded into VM
    init: *const fn (ctx: *ExtensionContext) anyerror!void,

    /// Called when VM shuts down (optional cleanup)
    deinit: ?*const fn (ctx: *ExtensionContext) void = null,
};

/// Context provided to extensions during init/deinit
pub const ExtensionContext = struct {
    /// The VM this extension is being loaded into (as anyopaque to avoid circular import)
    vm: *anyopaque,

    /// Allocator for extension use
    allocator: Allocator,

    /// Name of this extension (for error messages)
    extension_name: []const u8,

    /// Type registry for registering custom types
    type_registry: *TypeRegistry,

    /// Opcode registry for registering custom opcodes
    opcode_registry: *OpcodeRegistry,

    /// Native function registry (as anyopaque to avoid circular import)
    native_registry: *anyopaque,

    // ========================================================================
    // Type Registration
    // ========================================================================

    /// Register a custom value type
    /// Returns the assigned type ID (use this in initObject calls)
    pub fn registerType(self: *ExtensionContext, def: TypeDef) !u16 {
        // Create a mutable copy with extension filled in
        var stored_def = def;
        stored_def.extension = self.extension_name;
        return self.type_registry.register(stored_def, self.extension_name);
    }

    /// Look up a type ID by name (for interop with other extensions)
    pub fn getTypeId(self: *ExtensionContext, type_name: []const u8) ?u16 {
        return self.type_registry.getIdByName(type_name);
    }

    // ========================================================================
    // Opcode Registration
    // ========================================================================

    /// Register a custom opcode handler
    /// Opcode must be in extension range (0xD0-0xFE)
    pub fn registerOpcode(
        self: *ExtensionContext,
        opcode: u8,
        name: []const u8,
        handler: OpcodeRegistry.OpcodeHandler,
    ) !void {
        try self.opcode_registry.register(opcode, handler, .{
            .name = name,
            .extension = self.extension_name,
        });
    }

    // ========================================================================
    // Native Function Registration
    // ========================================================================

    /// Register a native function
    /// Note: Uses anyopaque for native_registry to avoid circular imports
    /// The actual VM casts this and calls the appropriate register method
    pub fn registerNative(self: *ExtensionContext, name: []const u8, func: anytype) !void {
        // This is called through the VM which handles the actual registration
        _ = self;
        _ = name;
        _ = func;
        // Implementation delegated to VM.registerExtensionNative()
    }
};

/// Loaded extension state
const LoadedExtension = struct {
    extension: Extension,
    initialized: bool,
};

/// Manages all loaded extensions
pub const ExtensionManager = struct {
    allocator: Allocator,
    extensions: std.StringHashMapUnmanaged(*LoadedExtension),
    load_order: std.ArrayListUnmanaged([]const u8),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .extensions = .empty,
            .load_order = .empty,
        };
    }

    pub fn deinit(self: *Self, ctx: *ExtensionContext) void {
        // Deinit in reverse load order
        var i: usize = self.load_order.items.len;
        while (i > 0) {
            i -= 1;
            const name = self.load_order.items[i];
            if (self.extensions.get(name)) |loaded| {
                if (loaded.initialized) {
                    if (loaded.extension.deinit) |deinit_fn| {
                        var ext_ctx = ctx.*;
                        ext_ctx.extension_name = name;
                        deinit_fn(&ext_ctx);
                    }
                }
                self.allocator.destroy(loaded);
            }
        }

        self.extensions.deinit(self.allocator);
        self.load_order.deinit(self.allocator);
    }

    /// Load an extension
    pub fn load(self: *Self, ext: Extension, ctx: *ExtensionContext) !void {
        // Check if already loaded
        if (self.extensions.get(ext.name) != null) {
            return; // Already loaded
        }

        // Load dependencies first
        for (ext.dependencies) |dep_name| {
            if (self.extensions.get(dep_name) == null) {
                return error.MissingDependency;
            }
        }

        // Create loaded extension record
        const loaded = try self.allocator.create(LoadedExtension);
        loaded.* = .{
            .extension = ext,
            .initialized = false,
        };

        // Add to registry
        try self.extensions.put(self.allocator, ext.name, loaded);
        try self.load_order.append(self.allocator, ext.name);

        // Initialize the extension
        var ext_ctx = ctx.*;
        ext_ctx.extension_name = ext.name;
        try ext.init(&ext_ctx);
        loaded.initialized = true;
    }

    /// Check if an extension is loaded
    pub fn isLoaded(self: *const Self, name: []const u8) bool {
        return self.extensions.get(name) != null;
    }

    /// Get loaded extension count
    pub fn count(self: *const Self) usize {
        return self.extensions.count();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ExtensionManager basic operations" {
    const allocator = std.testing.allocator;

    var test_type_registry = TypeRegistry.init(allocator);
    defer test_type_registry.deinit();

    var test_opcode_registry = OpcodeRegistry.init();

    var manager = ExtensionManager.init(allocator);

    var ctx = ExtensionContext{
        .vm = undefined,
        .allocator = allocator,
        .extension_name = "",
        .type_registry = &test_type_registry,
        .opcode_registry = &test_opcode_registry,
        .native_registry = undefined,
    };

    defer manager.deinit(&ctx);

    // Define a test extension
    const test_ext = Extension{
        .name = "test",
        .version = 1,
        .init = struct {
            fn init(_: *ExtensionContext) !void {}
        }.init,
    };

    // Load extension
    try manager.load(test_ext, &ctx);

    try std.testing.expect(manager.isLoaded("test"));
    try std.testing.expectEqual(@as(usize, 1), manager.count());
}
