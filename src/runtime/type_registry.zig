//! Type Registry
//!
//! Manages runtime type information for extension-defined types.
//! Extensions register their types here to get a unique type ID that
//! can be embedded in NaN-boxed object values.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("bytecode/value.zig").Value;

/// First type ID available for extensions (0-15 reserved for core)
pub const FIRST_EXTENSION_TYPE_ID: u16 = 16;

/// Maximum type ID (6 bits in NaN-boxing = 64 types)
pub const MAX_TYPE_ID: u16 = 63;

/// Context passed to type callbacks
pub const TypeContext = struct {
    allocator: Allocator,
    vm: *anyopaque, // Avoid circular import, cast to *VM when needed
};

/// Type definition for extension-defined types
pub const TypeDef = struct {
    /// Type name (used for lookups and error messages)
    name: []const u8,

    /// Extension that registered this type
    extension: []const u8,

    /// Create a new instance of this type (optional)
    create: ?*const fn (ctx: *TypeContext, args: []const Value) anyerror!*anyopaque = null,

    /// Destroy an instance (free memory) - required
    destroy: *const fn (ctx: *TypeContext, ptr: *anyopaque) void,

    /// Format for display (optional)
    format: ?*const fn (ptr: *anyopaque, writer: std.io.AnyWriter) anyerror!void = null,

    /// Clone/copy for value semantics (optional)
    clone: ?*const fn (ctx: *TypeContext, ptr: *anyopaque) anyerror!*anyopaque = null,

    /// Equality comparison (optional)
    eql: ?*const fn (a: *anyopaque, b: *anyopaque) bool = null,
};

/// Registry for extension-defined types
pub const TypeRegistry = struct {
    allocator: Allocator,
    types: std.ArrayListUnmanaged(TypeDef),
    name_to_id: std.StringHashMapUnmanaged(u16),
    next_id: u16,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .types = .empty,
            .name_to_id = .empty,
            .next_id = FIRST_EXTENSION_TYPE_ID,
        };
    }

    pub fn deinit(self: *Self) void {
        self.types.deinit(self.allocator);
        self.name_to_id.deinit(self.allocator);
    }

    /// Register a new type, returns assigned type ID
    pub fn register(self: *Self, def: TypeDef, extension: []const u8) !u16 {
        // Check if we have room for more types
        if (self.next_id > MAX_TYPE_ID) {
            return error.TooManyTypes;
        }

        // Check for duplicate name
        if (self.name_to_id.get(def.name) != null) {
            return error.DuplicateTypeName;
        }

        const type_id = self.next_id;
        self.next_id += 1;

        // Store the definition with extension info
        var stored_def = def;
        stored_def.extension = extension;
        try self.types.append(self.allocator, stored_def);

        // Map name to ID
        try self.name_to_id.put(self.allocator, def.name, type_id);

        return type_id;
    }

    /// Look up type definition by ID
    pub fn lookup(self: *const Self, type_id: u16) ?*const TypeDef {
        if (type_id < FIRST_EXTENSION_TYPE_ID) return null;
        const idx = type_id - FIRST_EXTENSION_TYPE_ID;
        if (idx >= self.types.items.len) return null;
        return &self.types.items[idx];
    }

    /// Look up type ID by name
    pub fn getIdByName(self: *const Self, name: []const u8) ?u16 {
        return self.name_to_id.get(name);
    }

    /// Get number of registered types
    pub fn count(self: *const Self) usize {
        return self.types.items.len;
    }

    /// Destroy an object using its type's destroy callback
    pub fn destroyObject(self: *const Self, type_id: u16, ptr: *anyopaque, ctx: *TypeContext) void {
        if (self.lookup(type_id)) |def| {
            def.destroy(ctx, ptr);
        }
    }

    /// Format an object using its type's format callback
    pub fn formatObject(self: *const Self, type_id: u16, ptr: *anyopaque, writer: std.io.AnyWriter) !void {
        if (self.lookup(type_id)) |def| {
            if (def.format) |fmt_fn| {
                try fmt_fn(ptr, writer);
                return;
            }
        }
        // Default formatting
        try writer.print("<object:type={d}>", .{type_id});
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TypeRegistry basic operations" {
    const allocator = std.testing.allocator;
    var registry = TypeRegistry.init(allocator);
    defer registry.deinit();

    // Register a type
    const type_id = try registry.register(.{
        .name = "TestType",
        .extension = "test",
        .destroy = struct {
            fn destroy(_: *TypeContext, _: *anyopaque) void {}
        }.destroy,
    }, "test");

    try std.testing.expectEqual(@as(u16, 16), type_id);
    try std.testing.expectEqual(@as(usize, 1), registry.count());

    // Lookup by ID
    const def = registry.lookup(type_id);
    try std.testing.expect(def != null);
    try std.testing.expectEqualStrings("TestType", def.?.name);

    // Lookup by name
    const id = registry.getIdByName("TestType");
    try std.testing.expectEqual(type_id, id.?);
}
