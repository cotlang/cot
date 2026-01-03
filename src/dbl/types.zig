//! DBL Type System Extension
//!
//! Handles DBL-specific type declarations and field specifiers.

const std = @import("std");

/// DBL field specifier from record definitions
/// Examples: "name, a50" or "amount, d10.2" or "flags, i4"
pub const FieldSpec = struct {
    name: []const u8,
    type_char: u8, // 'a', 'd', 'i', 'p'
    size: u32,
    precision: u8 = 0,
    array_dim: ?[]const u32 = null, // Array dimensions

    /// Parse a DBL field definition line
    /// Format: "name, type" or "name, type, dimension"
    pub fn parse(line: []const u8) !FieldSpec {
        // Find comma separating name from type
        const comma_pos = std.mem.indexOf(u8, line, ",") orelse
            return error.InvalidFieldSpec;

        const name = std.mem.trim(u8, line[0..comma_pos], " \t");
        const rest = std.mem.trim(u8, line[comma_pos + 1 ..], " \t");

        if (rest.len == 0) return error.InvalidFieldSpec;

        const type_char = rest[0];
        // Note: 'p' (packed) types are not supported - removed as they're not used in legacy app
        if (type_char != 'a' and type_char != 'd' and type_char != 'i') {
            return error.InvalidTypeChar;
        }

        // Parse size
        var size: u32 = 0;
        var precision: u8 = 0;
        var i: usize = 1;

        // Parse main size
        while (i < rest.len and rest[i] >= '0' and rest[i] <= '9') : (i += 1) {
            size = size * 10 + @as(u32, rest[i] - '0');
        }

        // Check for precision
        if (i < rest.len and rest[i] == '.') {
            i += 1;
            while (i < rest.len and rest[i] >= '0' and rest[i] <= '9') : (i += 1) {
                precision = precision * 10 + @as(u8, rest[i] - '0');
            }
        }

        return .{
            .name = name,
            .type_char = type_char,
            .size = if (size == 0) getDefaultSize(type_char) else size,
            .precision = precision,
        };
    }

    fn getDefaultSize(type_char: u8) u32 {
        return switch (type_char) {
            'a' => 1, // Default alpha is 1 char
            'd' => 10, // Default decimal
            'i' => 4, // Default integer (i4)
            else => 1,
        };
    }

    /// Get the byte size of this field
    pub fn byteSize(self: FieldSpec) u32 {
        return switch (self.type_char) {
            'a' => self.size, // Alpha: 1 byte per char
            'd' => self.size, // Decimal: 1 byte per digit
            'i' => self.size, // Integer: size is byte count
            else => self.size,
        };
    }
};

/// DBL overlay/union type
/// Allows multiple interpretations of the same memory
pub const Overlay = struct {
    base_field: []const u8,
    overlay_fields: []OverlayField,
};

pub const OverlayField = struct {
    name: []const u8,
    offset: u32,
    type_char: u8,
    size: u32,
};

test "field spec parsing" {
    const f1 = try FieldSpec.parse("name, a50");
    try std.testing.expectEqualStrings("name", f1.name);
    try std.testing.expect(f1.type_char == 'a');
    try std.testing.expect(f1.size == 50);

    const f2 = try FieldSpec.parse("amount, d10.2");
    try std.testing.expectEqualStrings("amount", f2.name);
    try std.testing.expect(f2.type_char == 'd');
    try std.testing.expect(f2.size == 10);
    try std.testing.expect(f2.precision == 2);
}

test "field byte size" {
    const alpha = FieldSpec{ .name = "x", .type_char = 'a', .size = 50 };
    try std.testing.expect(alpha.byteSize() == 50);

    const decimal = FieldSpec{ .name = "y", .type_char = 'd', .size = 10 };
    try std.testing.expect(decimal.byteSize() == 10);
}

// ============================================================================
// OOP Type Definitions (DBL Classes, Methods, Properties)
// ============================================================================

/// Access modifier for class members
pub const AccessModifier = enum {
    public,
    protected,
    private,
    internal, // .NET assembly-level visibility

    pub fn fromToken(token_type: anytype) AccessModifier {
        return switch (token_type) {
            .kw_public => .public,
            .kw_protected => .protected,
            .kw_private => .private,
            else => .private, // Default to private
        };
    }
};

/// Class-level modifiers (abstract, sealed, partial)
pub const ClassModifiers = struct {
    is_abstract: bool = false,
    is_sealed: bool = false,
    is_partial: bool = false,
};

/// Information about a class field
pub const ClassFieldInfo = struct {
    name: []const u8,
    type_spec: FieldSpec,
    access: AccessModifier = .private,
    is_static: bool = false,
    init_expr: ?usize = null, // Expression index for initializer
};

/// Information about a method parameter
pub const MethodParamInfo = struct {
    name: []const u8,
    type_str: []const u8,
    is_out: bool = false,
    is_inout: bool = false,
    is_optional: bool = false,
};

/// Information about a class method
pub const MethodInfo = struct {
    name: []const u8,
    params: []MethodParamInfo,
    return_type: ?[]const u8, // null for void
    access: AccessModifier = .public,
    is_static: bool = false,
    is_virtual: bool = false,
    is_abstract: bool = false,
    is_override: bool = false,
    body_stmt: ?usize = null, // Statement index for body (null for abstract)
};

/// Constructor initializer type
pub const ConstructorInitializerKind = enum {
    parent, // Call parent class constructor
    this, // Call sibling constructor
};

/// Information about a constructor initializer (parent(...) or this(...))
pub const ConstructorInitializer = struct {
    kind: ConstructorInitializerKind,
    args: []usize, // Expression indices for arguments
};

/// Information about a class constructor
pub const ConstructorInfo = struct {
    params: []MethodParamInfo,
    access: AccessModifier = .public,
    initializer: ?ConstructorInitializer = null, // parent(...) or this(...)
    body_stmt: ?usize = null, // Statement index for body
};

/// Information about a class destructor
pub const DestructorInfo = struct {
    body_stmt: ?usize = null, // Statement index for body
};

/// Information about a property getter/setter
pub const PropertyAccessorInfo = struct {
    body_stmt: ?usize = null,
};

/// Information about a class property
pub const PropertyInfo = struct {
    name: []const u8,
    type_str: []const u8,
    access: AccessModifier = .public,
    getter: ?PropertyAccessorInfo = null,
    setter: ?PropertyAccessorInfo = null,
    is_indexer: bool = false,
    index_params: ?[]MethodParamInfo = null,
};

/// Complete information about a DBL class
/// This is used during parsing to collect class elements,
/// then transformed into cot core AST (struct_def + impl_block)
pub const ClassInfo = struct {
    name: []const u8,
    namespace: ?[]const u8 = null,
    base_class: ?[]const u8 = null, // extends
    interfaces: []const []const u8 = &.{}, // implements
    access: AccessModifier = .public,
    is_abstract: bool = false,
    is_sealed: bool = false,
    is_partial: bool = false,

    // Class members
    fields: []ClassFieldInfo = &.{},
    methods: []MethodInfo = &.{},
    constructors: []ConstructorInfo = &.{},
    destructor: ?DestructorInfo = null,
    properties: []PropertyInfo = &.{},

    /// Check if this class has a default (parameterless) constructor
    pub fn hasDefaultConstructor(self: ClassInfo) bool {
        for (self.constructors) |ctor| {
            if (ctor.params.len == 0) return true;
        }
        return false;
    }

    /// Check if a method with the given name exists
    pub fn hasMethod(self: ClassInfo, name: []const u8) bool {
        for (self.methods) |method| {
            if (std.mem.eql(u8, method.name, name)) return true;
        }
        return false;
    }
};

/// Information about an interface method signature
pub const InterfaceMethodInfo = struct {
    name: []const u8,
    params: []MethodParamInfo,
    return_type: ?[]const u8,
};

/// Information about a DBL interface
pub const InterfaceInfo = struct {
    name: []const u8,
    namespace: ?[]const u8 = null,
    methods: []InterfaceMethodInfo = &.{},
};

/// Information about a namespace
pub const NamespaceInfo = struct {
    name: []const u8,
    classes: []const []const u8 = &.{}, // Class names in this namespace
    interfaces: []const []const u8 = &.{}, // Interface names
};
