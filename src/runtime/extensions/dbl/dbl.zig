//! DBL Extension
//!
//! Provides DBL-specific runtime features:
//! - Map type (OrderedMap) for symbol tables
//! - NSPC_* native functions for legacy compatibility
//!
//! This extension is loaded for DBL programs to provide
//! backward-compatible symbol table functionality.

const std = @import("std");
const extension_manager = @import("../../extension_manager.zig");
const Extension = extension_manager.Extension;
const ExtensionContext = extension_manager.ExtensionContext;
const TypeRegistry = @import("../../type_registry.zig").TypeRegistry;
const TypeContext = @import("../../type_registry.zig").TypeContext;
const OrderedMap = @import("../../bytecode/ordered_map.zig").OrderedMap;
const Value = @import("../../bytecode/value.zig").Value;
const native = @import("../../native/native.zig");

// Re-export symtable for access to constants
pub const symtable = @import("symtable.zig");

/// Map type ID - reserved as first extension type
pub const MAP_TYPE_ID: u16 = 16;

/// DBL extension definition
pub const dbl_extension = Extension{
    .name = "dbl",
    .version = 1,
    .dependencies = &.{},
    .init = init,
    .deinit = deinit,
};

/// Initialize the DBL extension
fn init(ctx: *ExtensionContext) anyerror!void {
    // Register the Map type
    const type_id = try ctx.registerType(.{
        .name = "Map",
        .extension = "dbl",
        .create = null, // Maps are created via NSPC_OPEN or Map.new()
        .destroy = destroyMap,
        .format = formatMap,
        .clone = null, // Maps are reference types, not value types
        .eql = null, // Reference equality
    });

    // Verify we got the expected type ID
    std.debug.assert(type_id == MAP_TYPE_ID);

    // Register NSPC_* native functions
    // The native_registry is passed as anyopaque to avoid circular imports
    // We need to cast it to NativeRegistry
    const registry: *native.NativeRegistry = @ptrCast(@alignCast(ctx.native_registry));
    try symtable.register(registry);
}

/// Cleanup the DBL extension
fn deinit(_: *ExtensionContext) void {
    // Clean up the handle registry
    symtable.deinit();
}

/// Destroy a Map instance
fn destroyMap(ctx: *TypeContext, ptr: *anyopaque) void {
    const map: *OrderedMap = @ptrCast(@alignCast(ptr));
    map.deinit();
    ctx.allocator.destroy(map);
}

/// Format a Map for display
fn formatMap(ptr: *anyopaque, writer: std.io.AnyWriter) anyerror!void {
    const map: *OrderedMap = @ptrCast(@alignCast(ptr));
    try writer.print("<map:{d}>", .{map.len()});
}

// ============================================================================
// Tests
// ============================================================================

test "DBL extension basic test" {
    const allocator = std.testing.allocator;

    var type_registry = TypeRegistry.init(allocator);
    defer type_registry.deinit();

    var opcode_registry = @import("../../opcode_registry.zig").OpcodeRegistry.init();

    var native_registry = native.NativeRegistry.init(allocator);
    defer native_registry.deinit();

    var ext_manager = extension_manager.ExtensionManager.init(allocator);

    var ctx = ExtensionContext{
        .vm = undefined,
        .allocator = allocator,
        .extension_name = "",
        .type_registry = &type_registry,
        .opcode_registry = &opcode_registry,
        .native_registry = &native_registry,
    };

    defer ext_manager.deinit(&ctx);

    // Load the DBL extension
    try ext_manager.load(dbl_extension, &ctx);

    // Verify extension is loaded
    try std.testing.expect(ext_manager.isLoaded("dbl"));

    // Verify Map type is registered
    const map_type = type_registry.lookup(MAP_TYPE_ID);
    try std.testing.expect(map_type != null);
    try std.testing.expectEqualStrings("Map", map_type.?.name);
}
