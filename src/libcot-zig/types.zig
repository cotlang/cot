//! Type representation for Cot.
//!
//! Every type in a Cot program (primitives, pointers, structs, enums, etc.)
//! is represented as a TypeIndex — an integer into the TypeRegistry. The
//! registry interns types to ensure deduplication: two `*i64` pointers are
//! always the same TypeIndex.
//!
//! Also contains Value Witness Table definitions (Swift ABI) for generic
//! ARC dispatch — see ValueWitnessTable and TypeMetadata at the bottom.

const std = @import("std");
const ast = @import("ast.zig");

/// Index into the type registry. Wrapped in enum(u32) for type safety.
pub const TypeIndex = enum(u32) {
    invalid = std.math.maxInt(u32),
    _,

    pub fn toInt(self: TypeIndex) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(v: u32) TypeIndex {
        return @enumFromInt(v);
    }
};

/// Primitive types. Each has a name, size, and category predicates.
/// Adding a new basic type: add the variant, then add entries to `names` and `sizes`.
pub const BasicKind = enum(u8) {
    invalid,
    bool_type,
    i8_type,
    i16_type,
    i32_type,
    i64_type,
    u8_type,
    u16_type,
    u32_type,
    u64_type,
    f32_type,
    f64_type,
    void_type,
    noreturn_type,
    untyped_int,
    untyped_float,
    untyped_bool,
    untyped_null,

    const names = [_][]const u8{
        "invalid", "bool", "i8", "i16", "i32", "i64",
        "u8", "u16", "u32", "u64", "f32", "f64",
        "void", "noreturn",
        "untyped int", "untyped float", "untyped bool", "untyped null",
    };

    const sizes = [_]u8{
        0, 1, 1, 2, 4, 8,
        1, 2, 4, 8, 4, 8,
        0, 0,
        0, 0, 0, 0,
    };

    pub fn name(self: BasicKind) []const u8 {
        return names[@intFromEnum(self)];
    }

    pub fn size(self: BasicKind) u8 {
        return sizes[@intFromEnum(self)];
    }

    pub fn isNumeric(self: BasicKind) bool {
        return self.isInteger() or self.isFloat();
    }

    pub fn isInteger(self: BasicKind) bool {
        return switch (self) {
            .i8_type, .i16_type, .i32_type, .i64_type,
            .u8_type, .u16_type, .u32_type, .u64_type,
            .untyped_int,
            => true,
            else => false,
        };
    }

    pub fn isSigned(self: BasicKind) bool {
        return switch (self) {
            .i8_type, .i16_type, .i32_type, .i64_type => true,
            else => false,
        };
    }

    pub fn isUnsigned(self: BasicKind) bool {
        return switch (self) {
            .u8_type, .u16_type, .u32_type, .u64_type => true,
            else => false,
        };
    }

    pub fn isFloat(self: BasicKind) bool {
        return switch (self) {
            .f32_type, .f64_type, .untyped_float => true,
            else => false,
        };
    }

    pub fn isUntyped(self: BasicKind) bool {
        return switch (self) {
            .untyped_int, .untyped_float, .untyped_bool, .untyped_null => true,
            else => false,
        };
    }
};

/// Pointer with packed flags for future extension (const, volatile, alignment).
pub const PointerType = struct {
    elem: TypeIndex,
    flags: Flags = .{},

    pub const Flags = packed struct(u8) {
        is_managed: bool = false,
        _padding: u7 = 0,
    };
};

pub const OptionalType = struct { elem: TypeIndex };
pub const ErrorSetType = struct { name: []const u8, variants: []const []const u8 };
pub const ErrorUnionType = struct { elem: TypeIndex, error_set: TypeIndex = .invalid };
pub const SliceType = struct { elem: TypeIndex };
pub const ArrayType = struct { elem: TypeIndex, length: u64 };
pub const MapType = struct { key: TypeIndex, value: TypeIndex };
pub const ListType = struct { elem: TypeIndex };
pub const TupleType = struct { element_types: []const TypeIndex };
pub const DistinctType = struct { name: []const u8, underlying: TypeIndex };

/// Swift-style Task wrapping an async function result.
pub const TaskType = struct { result_type: TypeIndex };

/// Swift OpaqueExistentialContainer: 40 bytes = 3-word buffer + metadata ptr + witness table ptr.
pub const ExistentialType = struct {
    trait_name: []const u8,
    method_names: []const []const u8,
    method_count: u32,
    conforming_types: []const []const u8 = &.{},
};

pub const StructField = struct {
    name: []const u8,
    type_idx: TypeIndex,
    offset: u32,
    bit_offset: u8 = 0,
    bit_width: u8 = 0,
    default_value: ast.OptionalIndex = .none,
};

pub const StructLayout = ast.StructLayout;

pub const StructType = struct {
    name: []const u8,
    fields: []const StructField,
    size: u32,
    alignment: u8,
    layout: StructLayout = .auto,
    backing_int: TypeIndex = .invalid,
};

pub const EnumVariant = struct { name: []const u8, value: i64 };
pub const EnumType = struct { name: []const u8, variants: []const EnumVariant, backing_type: TypeIndex };
pub const UnionVariant = struct { name: []const u8, payload_type: TypeIndex };
pub const UnionType = struct { name: []const u8, variants: []const UnionVariant, tag_type: TypeIndex };

pub const FuncParam = struct {
    name: []const u8,
    type_idx: TypeIndex,
    is_sending: bool = false,
};

pub const FuncType = struct { params: []const FuncParam, return_type: TypeIndex };

/// Tagged union of all Cot types. The TypeRegistry stores an array of these.
pub const Type = union(enum) {
    basic: BasicKind,
    pointer: PointerType,
    optional: OptionalType,
    error_union: ErrorUnionType,
    error_set: ErrorSetType,
    slice: SliceType,
    array: ArrayType,
    map: MapType,
    list: ListType,
    tuple: TupleType,
    struct_type: StructType,
    enum_type: EnumType,
    union_type: UnionType,
    func: FuncType,
    distinct: DistinctType,
    existential: ExistentialType,
    task: TaskType,

    pub fn isInvalid(self: Type) bool {
        return self == .basic and self.basic == .invalid;
    }
};

/// Method registered on a named type (struct, enum, union).
pub const MethodInfo = struct {
    name: []const u8,
    func_name: []const u8,
    func_type: TypeIndex,
    receiver_is_ptr: bool,
    is_static: bool = false,
    is_nonisolated: bool = false,
    source_tree: ?*const anyopaque = null,
};

/// Central type registry. Interns all types and provides construction,
/// deduplication, querying, and method dispatch.
pub const TypeRegistry = struct {
    types: std.ArrayListUnmanaged(Type),
    allocator: std.mem.Allocator,
    name_map: std.StringHashMap(TypeIndex),
    method_registry: std.StringHashMap(std.ArrayListUnmanaged(MethodInfo)),

    pub const INVALID: TypeIndex = @enumFromInt(0);
    pub const BOOL: TypeIndex = @enumFromInt(1);
    pub const I8: TypeIndex = @enumFromInt(2);
    pub const I16: TypeIndex = @enumFromInt(3);
    pub const I32: TypeIndex = @enumFromInt(4);
    pub const I64: TypeIndex = @enumFromInt(5);
    pub const U8: TypeIndex = @enumFromInt(6);
    pub const U16: TypeIndex = @enumFromInt(7);
    pub const U32: TypeIndex = @enumFromInt(8);
    pub const U64: TypeIndex = @enumFromInt(9);
    pub const F32: TypeIndex = @enumFromInt(10);
    pub const F64: TypeIndex = @enumFromInt(11);
    pub const VOID: TypeIndex = @enumFromInt(12);
    pub const UNTYPED_INT: TypeIndex = @enumFromInt(13);
    pub const UNTYPED_FLOAT: TypeIndex = @enumFromInt(14);
    pub const UNTYPED_BOOL: TypeIndex = @enumFromInt(15);
    pub const UNTYPED_NULL: TypeIndex = @enumFromInt(16);
    pub const STRING: TypeIndex = @enumFromInt(17);
    pub const SSA_MEM: TypeIndex = @enumFromInt(18);
    pub const SSA_FLAGS: TypeIndex = @enumFromInt(19);
    pub const SSA_TUPLE: TypeIndex = @enumFromInt(20);
    pub const SSA_RESULTS: TypeIndex = @enumFromInt(21);
    pub const NORETURN: TypeIndex = @enumFromInt(22);
    pub const FIRST_USER_TYPE: TypeIndex = @enumFromInt(23);
    pub const INT: TypeIndex = I64;
    pub const FLOAT: TypeIndex = F64;

    pub fn init(allocator: std.mem.Allocator) !TypeRegistry {
        var reg = TypeRegistry{
            .types = .{},
            .allocator = allocator,
            .name_map = std.StringHashMap(TypeIndex).init(allocator),
            .method_registry = std.StringHashMap(std.ArrayListUnmanaged(MethodInfo)).init(allocator),
        };

        inline for (.{
            .invalid, .bool_type, .i8_type, .i16_type, .i32_type, .i64_type,
            .u8_type, .u16_type, .u32_type, .u64_type, .f32_type, .f64_type,
            .void_type, .untyped_int, .untyped_float, .untyped_bool, .untyped_null,
        }) |k| {
            try reg.types.append(allocator, .{ .basic = k });
        }
        try reg.types.append(allocator, .{ .slice = .{ .elem = U8 } }); // STRING
        for (0..4) |_| try reg.types.append(allocator, .{ .basic = .void_type }); // SSA placeholders
        try reg.types.append(allocator, .{ .basic = .noreturn_type }); // NORETURN

        inline for (.{
            .{ "bool", BOOL }, .{ "i8", I8 }, .{ "i16", I16 }, .{ "i32", I32 },
            .{ "i64", I64 },   .{ "int", INT }, .{ "u8", U8 }, .{ "u16", U16 },
            .{ "u32", U32 },   .{ "u64", U64 }, .{ "f32", F32 }, .{ "f64", F64 },
            .{ "float", F64 }, .{ "void", VOID }, .{ "string", STRING },
            .{ "byte", U8 },   .{ "noreturn", NORETURN },
        }) |pair| try reg.name_map.put(pair[0], pair[1]);

        return reg;
    }

    pub fn deinit(self: *TypeRegistry) void {
        self.types.deinit(self.allocator);
        self.name_map.deinit();
        var it = self.method_registry.valueIterator();
        while (it.next()) |list| list.deinit(self.allocator);
        self.method_registry.deinit();
    }

    pub fn get(self: *const TypeRegistry, idx: TypeIndex) Type {
        const i = @intFromEnum(idx);
        if (idx == .invalid or i >= self.types.items.len) return .{ .basic = .invalid };
        return self.types.items[i];
    }

    pub fn add(self: *TypeRegistry, t: Type) !TypeIndex {
        const idx = TypeIndex.fromInt(@intCast(self.types.items.len));
        try self.types.append(self.allocator, t);
        return idx;
    }

    pub fn registerNamed(self: *TypeRegistry, n: []const u8, idx: TypeIndex) !void {
        try self.name_map.put(n, idx);
    }

    pub fn lookupByName(self: *const TypeRegistry, n: []const u8) ?TypeIndex {
        return self.name_map.get(n);
    }

    pub fn registerMethod(self: *TypeRegistry, type_name: []const u8, method: MethodInfo) !void {
        const gop = try self.method_registry.getOrPut(type_name);
        if (!gop.found_existing) gop.value_ptr.* = .{};
        try gop.value_ptr.append(self.allocator, method);
    }

    pub fn lookupMethod(self: *const TypeRegistry, type_name: []const u8, method_name: []const u8) ?MethodInfo {
        const methods = self.method_registry.get(type_name) orelse return null;
        for (methods.items) |m| if (std.mem.eql(u8, m.name, method_name)) return m;
        return null;
    }

    pub fn makePointer(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        const pointee = self.get(elem);
        const is_managed = pointee == .struct_type or pointee == .enum_type or pointee == .union_type;
        for (self.types.items, 0..) |t, i| {
            if (t == .pointer and t.pointer.elem == elem and t.pointer.flags.is_managed == is_managed)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .pointer = .{ .elem = elem, .flags = .{ .is_managed = is_managed } } });
    }

    pub fn makeRawPointer(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .pointer and t.pointer.elem == elem and !t.pointer.flags.is_managed)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .pointer = .{ .elem = elem, .flags = .{} } });
    }

    pub fn makeOptional(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .optional and t.optional.elem == elem) return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .optional = .{ .elem = elem } });
    }

    pub fn makeErrorUnion(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .error_union and t.error_union.elem == elem and t.error_union.error_set == .invalid)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .error_union = .{ .elem = elem } });
    }

    pub fn makeErrorUnionWithSet(self: *TypeRegistry, elem: TypeIndex, error_set: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .error_union and t.error_union.elem == elem and t.error_union.error_set == error_set)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .error_union = .{ .elem = elem, .error_set = error_set } });
    }

    pub fn makeSlice(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .slice and t.slice.elem == elem) return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .slice = .{ .elem = elem } });
    }

    pub fn makeArray(self: *TypeRegistry, elem: TypeIndex, len: u64) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .array and t.array.elem == elem and t.array.length == len)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .array = .{ .elem = elem, .length = len } });
    }

    pub fn makeMap(self: *TypeRegistry, key: TypeIndex, value: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .map and t.map.key == key and t.map.value == value)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .map = .{ .key = key, .value = value } });
    }

    pub fn makeList(self: *TypeRegistry, elem: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .list and t.list.elem == elem) return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .list = .{ .elem = elem } });
    }

    pub fn makeTuple(self: *TypeRegistry, element_types: []const TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .tuple and t.tuple.element_types.len == element_types.len and blk: {
                for (t.tuple.element_types, element_types) |a, b| {
                    if (a != b) break :blk false;
                }
                break :blk true;
            }) return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .tuple = .{ .element_types = try self.allocator.dupe(TypeIndex, element_types) } });
    }

    pub fn makeExistential(self: *TypeRegistry, trait_name: []const u8, method_names: []const []const u8) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .existential and std.mem.eql(u8, t.existential.trait_name, trait_name))
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .existential = .{
            .trait_name = trait_name,
            .method_names = try self.allocator.dupe([]const u8, method_names),
            .method_count = @intCast(method_names.len),
        } });
    }

    pub fn makeFunc(self: *TypeRegistry, params: []const FuncParam, ret: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .func and t.func.return_type == ret and t.func.params.len == params.len and blk: {
                for (t.func.params, params) |a, b| {
                    if (a.type_idx != b.type_idx) break :blk false;
                }
                break :blk true;
            }) return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .func = .{ .params = try self.allocator.dupe(FuncParam, params), .return_type = ret } });
    }

    pub fn makeTask(self: *TypeRegistry, result_type: TypeIndex) !TypeIndex {
        for (self.types.items, 0..) |t, i| {
            if (t == .task and t.task.result_type == result_type)
                return TypeIndex.fromInt(@intCast(i));
        }
        return self.add(.{ .task = .{ .result_type = result_type } });
    }

    pub fn isPointer(self: *const TypeRegistry, idx: TypeIndex) bool {
        return self.get(idx) == .pointer;
    }

    pub fn pointerElem(self: *const TypeRegistry, idx: TypeIndex) TypeIndex {
        return if (self.get(idx) == .pointer) self.get(idx).pointer.elem else .invalid;
    }

    pub fn isArray(self: *const TypeRegistry, idx: TypeIndex) bool {
        return self.get(idx) == .array;
    }

    pub fn isSlice(self: *const TypeRegistry, idx: TypeIndex) bool {
        return self.get(idx) == .slice;
    }

    pub fn basicTypeName(type_idx: TypeIndex) []const u8 {
        return switch (@intFromEnum(type_idx)) {
            0 => "invalid", 1 => "bool", 2 => "i8", 3 => "i16", 4 => "i32", 5 => "i64",
            6 => "u8", 7 => "u16", 8 => "u32", 9 => "u64", 10 => "f32", 11 => "f64",
            12 => "void", 13 => "untyped_int", 14 => "untyped_float",
            15 => "untyped_bool", 16 => "untyped_null", 17 => "string",
            18 => "ssa_mem", 19 => "ssa_flags", 20 => "ssa_tuple", 21 => "ssa_results",
            22 => "noreturn",
            else => "composite",
        };
    }

    pub fn basicTypeSize(type_idx: TypeIndex) u8 {
        return switch (@intFromEnum(type_idx)) {
            12, 18, 19, 20, 21, 22 => 0, // void, ssa_*, noreturn
            1, 15, 2, 6 => 1,             // bool, untyped_bool, i8, u8
            3, 7 => 2,                     // i16, u16
            4, 8, 10 => 4,                 // i32, u32, f32
            5, 9, 11, 13 => 8,            // i64, u64, f64, untyped_int
            17 => 16,                      // string
            else => 8,
        };
    }

    /// Display name for diagnostics.
    pub fn typeName(self: *const TypeRegistry, idx: TypeIndex) []const u8 {
        if (idx == STRING) return "string";
        return switch (self.get(idx)) {
            .basic => |bk| bk.name(),
            .struct_type => |s| s.name,
            .enum_type => |e| e.name,
            .union_type => |u| u.name,
            .pointer => "pointer",
            .list => "list",
            .map => "map",
            .optional => "optional",
            .error_union => "error_union",
            .error_set => "error_set",
            .slice => "slice",
            .array => "array",
            .tuple => "tuple",
            .func => "function",
            .distinct => |d| d.name,
            .existential => |e| e.trait_name,
            .task => "Task",
        };
    }

    /// Resolve distinct types to their underlying type (recursively).
    pub fn resolveDistinct(self: *const TypeRegistry, idx: TypeIndex) TypeIndex {
        const t = self.get(idx);
        if (t == .distinct) return self.resolveDistinct(t.distinct.underlying);
        return idx;
    }

    pub fn parseBitWidth(n: []const u8) ?u8 {
        if (n.len >= 2 and n.len <= 3 and (n[0] == 'u' or n[0] == 'i')) {
            var width: u32 = 0;
            for (n[1..]) |c| {
                if (c >= '0' and c <= '9') {
                    width = width * 10 + (c - '0');
                } else return null;
            }
            if (width >= 1 and width <= 64) return @intCast(width);
        }
        return null;
    }

    pub fn lookupBitfieldType(_: *const TypeRegistry, n: []const u8) ?TypeIndex {
        if (n.len >= 2 and n.len <= 3 and (n[0] == 'u' or n[0] == 'i')) {
            var width: u32 = 0;
            var valid = true;
            for (n[1..]) |c| {
                if (c >= '0' and c <= '9') {
                    width = width * 10 + (c - '0');
                } else {
                    valid = false;
                    break;
                }
            }
            if (valid and width >= 1 and width <= 64) {
                if (n[0] == 'i') {
                    if (width <= 8) return I8;
                    if (width <= 16) return I16;
                    if (width <= 32) return I32;
                    return I64;
                } else {
                    if (width <= 8) return U8;
                    if (width <= 16) return U16;
                    if (width <= 32) return U32;
                    return U64;
                }
            }
        }
        return null;
    }

    pub fn tupleElementOffset(self: *const TypeRegistry, tuple_idx: TypeIndex, index: u32) u32 {
        const tup = self.get(tuple_idx).tuple;
        var offset: u32 = 0;
        for (0..index) |i| offset += ((self.sizeOf(tup.element_types[i]) + 7) / 8) * 8;
        return offset;
    }

    pub fn sizeOf(self: *const TypeRegistry, idx: TypeIndex) u32 {
        if (idx == UNTYPED_INT or idx == UNTYPED_FLOAT) return 8;
        return switch (self.get(idx)) {
            .basic => |k| k.size(),
            .pointer, .map, .list, .func, .error_set => 8,
            .tuple => |tup| blk: {
                var total: u32 = 0;
                for (tup.element_types) |et| total += ((self.sizeOf(et) + 7) / 8) * 8;
                break :blk total;
            },
            .optional => |opt| blk: {
                const elem_size = self.sizeOf(opt.elem);
                const payload_size: u32 = if (elem_size <= 8) 8 else ((elem_size + 7) / 8) * 8;
                break :blk 8 + payload_size;
            },
            .error_union => |eu| blk: {
                const elem_size = self.sizeOf(eu.elem);
                const payload_size: u32 = if (elem_size <= 8) 8 else ((elem_size + 7) / 8) * 8;
                break :blk 8 + payload_size;
            },
            .slice => 24,
            .array => |a| @intCast(self.sizeOf(a.elem) * a.length),
            .struct_type => |s| s.size,
            .enum_type => |e| self.sizeOf(e.backing_type),
            .union_type => |u| blk: {
                var max_payload: u32 = 0;
                for (u.variants) |v| {
                    if (v.payload_type != .invalid) {
                        const ps = self.sizeOf(v.payload_type);
                        if (ps > max_payload) max_payload = ps;
                    }
                }
                const payload_aligned = if (max_payload == 0) @as(u32, 0) else ((max_payload + 7) / 8) * 8;
                break :blk 8 + payload_aligned;
            },
            .distinct => |d| self.sizeOf(d.underlying),
            .existential => 40,
            .task => 8,
        };
    }

    pub fn alignmentOf(self: *const TypeRegistry, idx: TypeIndex) u32 {
        return switch (self.get(idx)) {
            .basic => |k| if (k.size() == 0) 1 else k.size(),
            .pointer, .func, .optional, .error_union, .error_set, .slice,
            .map, .list, .union_type, .tuple, .existential, .task,
            => 8,
            .array => |a| self.alignmentOf(a.elem),
            .struct_type => |s| s.alignment,
            .enum_type => |e| self.alignmentOf(e.backing_type),
            .distinct => |d| self.alignmentOf(d.underlying),
        };
    }

    pub fn alignOf(self: *const TypeRegistry, idx: TypeIndex) u32 {
        return self.alignmentOf(idx);
    }

    pub fn isGenericCollection(n: []const u8) bool {
        return (n.len > 5 and std.mem.startsWith(u8, n, "List(")) or
            (n.len > 4 and std.mem.startsWith(u8, n, "Map("));
    }

    /// True if the type is trivial (no ARC needed). Copies Swift TypeLowering::isTrivial().
    pub fn isTrivial(self: *const TypeRegistry, idx: TypeIndex) bool {
        const t = self.get(idx);
        return switch (t) {
            .basic, .func, .error_set => true,
            .pointer => |p| !p.flags.is_managed,
            .enum_type => |e| self.isTrivial(e.backing_type),
            .array => |a| self.isTrivial(a.elem),
            .slice => |s| self.isTrivial(s.elem),
            .optional => |o| self.isTrivial(o.elem),
            .error_union => |e| self.isTrivial(e.elem),
            .struct_type => |s| {
                if (isGenericCollection(s.name)) return false;
                for (s.fields) |field| {
                    if (!self.isTrivial(field.type_idx)) return false;
                }
                return true;
            },
            .map, .list => false,
            .union_type => |u| {
                for (u.variants) |v| {
                    if (v.payload_type != .invalid and !self.isTrivial(v.payload_type)) return false;
                }
                return true;
            },
            .tuple => |tup| {
                for (tup.element_types) |et| if (!self.isTrivial(et)) return false;
                return true;
            },
            .distinct => |d| self.isTrivial(d.underlying),
            .existential => false,
            .task => true,
        };
    }

    pub fn needsARC(self: *const TypeRegistry, idx: TypeIndex) bool {
        return !self.isTrivial(idx);
    }

    /// True if type is or contains ARC-managed values.
    pub fn couldBeARC(self: *const TypeRegistry, idx: TypeIndex) bool {
        const t = self.get(idx);
        if (t == .pointer) return t.pointer.flags.is_managed;
        if (t == .optional) return self.couldBeARC(t.optional.elem);
        if (t == .struct_type) {
            if (isGenericCollection(t.struct_type.name)) return true;
            for (t.struct_type.fields) |field| {
                if (self.couldBeARC(field.type_idx)) return true;
            }
        }
        if (t == .union_type) {
            for (t.union_type.variants) |v| {
                if (v.payload_type != .invalid and self.couldBeARC(v.payload_type)) return true;
            }
        }
        if (t == .tuple) {
            for (t.tuple.element_types) |et| {
                if (self.couldBeARC(et)) return true;
            }
        }
        if (t == .error_union) return self.couldBeARC(t.error_union.elem);
        if (t == .list or t == .map) return true;
        if (t == .array) return self.couldBeARC(t.array.elem);
        if (t == .slice) return self.couldBeARC(t.slice.elem);
        if (t == .distinct) return self.couldBeARC(t.distinct.underlying);
        if (t == .existential) return true;
        return false;
    }

    /// Common type for binary operations between two numeric types.
    pub fn commonType(a: TypeIndex, b: TypeIndex) TypeIndex {
        if (a == b) return a;
        if (a == UNTYPED_INT) return if (isBasicNumeric(b)) b else I64;
        if (b == UNTYPED_INT) return if (isBasicNumeric(a)) a else I64;
        if (a == UNTYPED_FLOAT) return if (isBasicFloat(b)) b else F64;
        if (b == UNTYPED_FLOAT) return if (isBasicFloat(a)) a else F64;
        if (isBasicFloat(a) and isBasicFloat(b))
            return if (basicTypeSize(a) >= basicTypeSize(b)) a else b;
        if (isBasicFloat(a)) return a;
        if (isBasicFloat(b)) return b;
        const a_size = basicTypeSize(a);
        const b_size = basicTypeSize(b);
        if (a_size > b_size) return a;
        if (b_size > a_size) return b;
        if (isBasicSigned(a) != isBasicSigned(b)) {
            return switch (a_size) {
                1 => I16, 2 => I32, 4 => I64, else => I64,
            };
        }
        if (isBasicSigned(a)) return a;
        if (isBasicSigned(b)) return b;
        return a;
    }

    fn isBasicNumeric(idx: TypeIndex) bool {
        return switch (@intFromEnum(idx)) {
            2, 3, 4, 5, 6, 7, 8, 9, 10, 11 => true,
            else => false,
        };
    }

    fn isBasicFloat(idx: TypeIndex) bool {
        return switch (@intFromEnum(idx)) { 10, 11, 14 => true, else => false };
    }

    fn isBasicSigned(idx: TypeIndex) bool {
        return switch (@intFromEnum(idx)) { 2, 3, 4, 5 => true, else => false };
    }

    pub fn equal(self: *const TypeRegistry, a: TypeIndex, b: TypeIndex) bool {
        if (a == b) return true;
        if (a == .invalid or b == .invalid) return false;
        const ta = self.get(a);
        const tb = self.get(b);
        if (@intFromEnum(ta) != @intFromEnum(tb)) return false;
        return switch (ta) {
            .basic => |ka| tb.basic == ka,
            .pointer => |pa| self.equal(pa.elem, tb.pointer.elem),
            .optional => |oa| self.equal(oa.elem, tb.optional.elem),
            .error_union => |ea| self.equal(ea.elem, tb.error_union.elem),
            .slice => |sa| self.equal(sa.elem, tb.slice.elem),
            .array => |aa| aa.length == tb.array.length and self.equal(aa.elem, tb.array.elem),
            .map => |ma| self.equal(ma.key, tb.map.key) and self.equal(ma.value, tb.map.value),
            .list => |la| self.equal(la.elem, tb.list.elem),
            .struct_type => |sa| std.mem.eql(u8, sa.name, tb.struct_type.name),
            .enum_type => |ea| std.mem.eql(u8, ea.name, tb.enum_type.name),
            .union_type => |ua| std.mem.eql(u8, ua.name, tb.union_type.name),
            .error_set => |es| std.mem.eql(u8, es.name, tb.error_set.name),
            .tuple => |tup_a| {
                if (tup_a.element_types.len != tb.tuple.element_types.len) return false;
                for (tup_a.element_types, tb.tuple.element_types) |ae, be| {
                    if (!self.equal(ae, be)) return false;
                }
                return true;
            },
            .func => false,
            .distinct => |da| std.mem.eql(u8, da.name, tb.distinct.name),
            .existential => |ea| std.mem.eql(u8, ea.trait_name, tb.existential.trait_name),
            .task => |task_a| self.equal(task_a.result_type, tb.task.result_type),
        };
    }

    pub fn isAssignable(self: *const TypeRegistry, from: TypeIndex, to: TypeIndex) bool {
        if (from == to) return true;
        if (from == .invalid or to == .invalid) return true;
        if (from == NORETURN) return true;
        const from_t = self.get(from);
        const to_t = self.get(to);

        if (from_t == .basic) {
            if (from_t.basic == .untyped_int and to_t == .basic and to_t.basic.isInteger()) return true;
            if (from_t.basic == .untyped_float and to_t == .basic and to_t.basic.isFloat()) return true;
            if (from_t.basic == .untyped_bool and to_t == .basic and to_t.basic == .bool_type) return true;
            if (from_t.basic == .untyped_null and (to_t == .optional or to_t == .pointer)) return true;
        }

        if (to_t == .distinct) {
            if (from_t == .distinct) return std.mem.eql(u8, from_t.distinct.name, to_t.distinct.name);
            if (from_t == .basic and from_t.basic.isUntyped()) return self.isAssignable(from, to_t.distinct.underlying);
            return false;
        }
        if (from_t == .distinct) return false;

        if (to_t == .pointer) {
            const to_elem = self.get(to_t.pointer.elem);
            if (to_elem == .existential) return self.isAssignable(from, to_t.pointer.elem);
        }

        if (to_t == .optional) return self.isAssignable(from, to_t.optional.elem);

        if (to_t == .error_union) {
            if (self.isAssignable(from, to_t.error_union.elem)) return true;
            if (from_t == .error_set) return true;
        }

        if (from_t == .basic and to_t == .basic and from_t.basic == to_t.basic) return true;

        if (from_t == .basic and to_t == .basic and from_t.basic.isInteger() and to_t.basic.isInteger()) {
            const from_size = from_t.basic.size();
            const to_size = to_t.basic.size();
            const from_signed = from_t.basic.isSigned();
            const to_signed = to_t.basic.isSigned();
            if (from_signed == to_signed) return to_size >= from_size;
            if (!from_signed and to_signed) return to_size > from_size;
            return false;
        }

        if (from_t == .slice and to_t == .slice) return self.equal(from_t.slice.elem, to_t.slice.elem);
        if (from_t == .array and to_t == .slice) return self.equal(from_t.array.elem, to_t.slice.elem);
        if (from == STRING and to_t == .pointer and to_t.pointer.elem == U8) return true;
        if (from_t == .struct_type and to_t == .struct_type) return std.mem.eql(u8, from_t.struct_type.name, to_t.struct_type.name);
        if (from_t == .enum_type and to_t == .enum_type) return std.mem.eql(u8, from_t.enum_type.name, to_t.enum_type.name);
        if (from_t == .enum_type) return self.isAssignable(from_t.enum_type.backing_type, to);
        if (from_t == .union_type) return self.isAssignable(from_t.union_type.tag_type, to);
        if (from_t == .pointer and to_t == .pointer) return self.equal(from_t.pointer.elem, to_t.pointer.elem);
        if (from_t == .array and to_t == .array) return from_t.array.length == to_t.array.length and self.isAssignable(from_t.array.elem, to_t.array.elem);

        if (from_t == .tuple and to_t == .tuple) {
            if (from_t.tuple.element_types.len != to_t.tuple.element_types.len) return false;
            for (from_t.tuple.element_types, to_t.tuple.element_types) |fe, te| {
                if (!self.isAssignable(fe, te)) return false;
            }
            return true;
        }

        if (to_t == .existential) {
            const from_name = self.typeName(from);
            for (to_t.existential.conforming_types) |conforming| {
                if (std.mem.eql(u8, from_name, conforming)) return true;
            }
            if (from_t == .pointer) {
                const elem_name = self.typeName(from_t.pointer.elem);
                for (to_t.existential.conforming_types) |conforming| {
                    if (std.mem.eql(u8, elem_name, conforming)) return true;
                }
            }
            return false;
        }

        if (from_t == .func and to_t == .func) {
            if (from_t.func.params.len != to_t.func.params.len) return false;
            for (from_t.func.params, to_t.func.params) |fp, tp| if (!self.equal(fp.type_idx, tp.type_idx)) return false;
            return self.equal(from_t.func.return_type, to_t.func.return_type);
        }

        return false;
    }
};

pub fn isNumeric(t: Type) bool { return if (t == .basic) t.basic.isNumeric() else false; }
pub fn isInteger(t: Type) bool { return if (t == .basic) t.basic.isInteger() else false; }
pub fn isBool(t: Type) bool { return if (t == .basic) (t.basic == .bool_type or t.basic == .untyped_bool) else false; }
pub fn isUntyped(t: Type) bool { return if (t == .basic) t.basic.isUntyped() else false; }

/// Swift ABI ValueWitnessFlags. Alignment packed into bits 0-7 as (alignment - 1).
pub const ValueWitnessFlags = struct {
    data: u32,

    pub const AlignmentMask: u32 = 0x000000FF;
    pub const IsNonPOD: u32 = 0x00010000;
    pub const IsNonInline: u32 = 0x00020000;
    pub const IsNonBitwiseTakable: u32 = 0x00100000;
    pub const HasEnumWitnesses: u32 = 0x00200000;

    pub fn init(alignment: u32, is_pod: bool, is_inline: bool, is_bitwise_takable: bool, has_enum_witnesses: bool) ValueWitnessFlags {
        var d: u32 = (alignment - 1) & AlignmentMask;
        if (!is_pod) d |= IsNonPOD;
        if (!is_inline) d |= IsNonInline;
        if (!is_bitwise_takable) d |= IsNonBitwiseTakable;
        if (has_enum_witnesses) d |= HasEnumWitnesses;
        return .{ .data = d };
    }

    pub fn getAlignment(self: ValueWitnessFlags) u32 { return (self.data & AlignmentMask) + 1; }
    pub fn isPOD(self: ValueWitnessFlags) bool { return (self.data & IsNonPOD) == 0; }
    pub fn isInlineStorage(self: ValueWitnessFlags) bool { return (self.data & IsNonInline) == 0; }
    pub fn isBitwiseTakable(self: ValueWitnessFlags) bool { return (self.data & IsNonBitwiseTakable) == 0; }
    pub fn hasEnumWitnesses(self: ValueWitnessFlags) bool { return (self.data & HasEnumWitnesses) != 0; }
};

pub const WitnessFn = struct {
    pub const InitializeBufferWithCopyOfBuffer = *const fn (*[3]usize, *[3]usize, *const TypeMetadata) callconv(.c) *anyopaque;
    pub const Destroy = *const fn (*anyopaque, *const TypeMetadata) callconv(.c) void;
    pub const InitializeWithCopy = *const fn (*anyopaque, *anyopaque, *const TypeMetadata) callconv(.c) *anyopaque;
    pub const AssignWithCopy = *const fn (*anyopaque, *anyopaque, *const TypeMetadata) callconv(.c) *anyopaque;
    pub const InitializeWithTake = *const fn (*anyopaque, *anyopaque, *const TypeMetadata) callconv(.c) *anyopaque;
    pub const AssignWithTake = *const fn (*anyopaque, *anyopaque, *const TypeMetadata) callconv(.c) *anyopaque;
    pub const GetEnumTagSinglePayload = *const fn (*const anyopaque, u32, *const TypeMetadata) callconv(.c) u32;
    pub const StoreEnumTagSinglePayload = *const fn (*anyopaque, u32, u32, *const TypeMetadata) callconv(.c) void;
    pub const GetEnumTag = *const fn (*const anyopaque, *const TypeMetadata) callconv(.c) u32;
    pub const DestructiveProjectEnumData = *const fn (*anyopaque, *const TypeMetadata) callconv(.c) void;
    pub const DestructiveInjectEnumTag = *const fn (*anyopaque, u32, *const TypeMetadata) callconv(.c) void;
};

/// Swift ABI ValueWitnessTable. 88 bytes on 64-bit (base) or 112 (with enum witnesses).
pub const ValueWitnessTable = struct {
    initializeBufferWithCopyOfBuffer: WitnessFn.InitializeBufferWithCopyOfBuffer,
    destroy: WitnessFn.Destroy,
    initializeWithCopy: WitnessFn.InitializeWithCopy,
    assignWithCopy: WitnessFn.AssignWithCopy,
    initializeWithTake: WitnessFn.InitializeWithTake,
    assignWithTake: WitnessFn.AssignWithTake,
    getEnumTagSinglePayload: WitnessFn.GetEnumTagSinglePayload,
    storeEnumTagSinglePayload: WitnessFn.StoreEnumTagSinglePayload,
    size: u64,
    stride: u64,
    flags: ValueWitnessFlags,
    extraInhabitantCount: u32,

    pub const NumWords_ValueBuffer: usize = 3;
    pub const ValueBufferSize: usize = NumWords_ValueBuffer * @sizeOf(usize);

    pub fn canBeInline(is_bitwise_takable: bool, sz: u64, alignment: u64) bool {
        return is_bitwise_takable and sz <= ValueBufferSize and alignment <= @alignOf(usize);
    }

    pub fn isPOD(self: *const ValueWitnessTable) bool { return self.flags.isPOD(); }
};

pub const EnumValueWitnessTable = struct {
    base: ValueWitnessTable,
    getEnumTag: WitnessFn.GetEnumTag,
    destructiveProjectEnumData: WitnessFn.DestructiveProjectEnumData,
    destructiveInjectEnumTag: WitnessFn.DestructiveInjectEnumTag,
};

pub const TypeMetadataKind = enum(u64) {
    struct_type = 0x200,
    enum_type = 0x201,
    optional_type = 0x202,
    tuple_type = 0x301,
    function_type = 0x302,
    list_type = 0x400,
    map_type = 0x401,
};

/// Per-type metadata with VWT pointer. Generic functions receive this as hidden param.
pub const TypeMetadata = struct {
    vwt: *const ValueWitnessTable,
    kind: TypeMetadataKind,
    type_idx: TypeIndex,
};

test "BasicKind comptime arrays match enum order" {
    try std.testing.expectEqualStrings("bool", BasicKind.bool_type.name());
    try std.testing.expectEqualStrings("i64", BasicKind.i64_type.name());
    try std.testing.expectEqual(@as(u8, 8), BasicKind.i64_type.size());
    try std.testing.expectEqual(@as(u8, 1), BasicKind.bool_type.size());
}

test "BasicKind predicates" {
    try std.testing.expect(BasicKind.i32_type.isInteger());
    try std.testing.expect(BasicKind.i32_type.isNumeric());
    try std.testing.expect(BasicKind.i32_type.isSigned());
    try std.testing.expect(!BasicKind.i32_type.isUnsigned());
    try std.testing.expect(BasicKind.u64_type.isUnsigned());
    try std.testing.expect(BasicKind.f64_type.isFloat());
    try std.testing.expect(BasicKind.untyped_int.isUntyped());
}

test "TypeRegistry init and lookup" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    try std.testing.expectEqual(TypeRegistry.BOOL, reg.lookupByName("bool").?);
    try std.testing.expectEqual(TypeRegistry.I64, reg.lookupByName("int").?);
    try std.testing.expectEqual(TypeRegistry.STRING, reg.lookupByName("string").?);
    try std.testing.expectEqual(BasicKind.bool_type, reg.get(TypeRegistry.BOOL).basic);
}

test "TypeRegistry make composite types with dedup" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    const ptr1 = try reg.makePointer(TypeRegistry.I32);
    const ptr2 = try reg.makePointer(TypeRegistry.I32);
    try std.testing.expectEqual(ptr1, ptr2);
    try std.testing.expect(reg.get(ptr1) == .pointer);
    const arr = try reg.makeArray(TypeRegistry.I32, 10);
    try std.testing.expectEqual(@as(u64, 10), reg.get(arr).array.length);
}

test "sizeOf" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    try std.testing.expectEqual(@as(u32, 1), reg.sizeOf(TypeRegistry.BOOL));
    try std.testing.expectEqual(@as(u32, 8), reg.sizeOf(TypeRegistry.I64));
    try std.testing.expectEqual(@as(u32, 24), reg.sizeOf(TypeRegistry.STRING));
}

test "isTrivial and needsARC" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    try std.testing.expect(reg.isTrivial(TypeRegistry.I64));
    try std.testing.expect(reg.isTrivial(TypeRegistry.BOOL));
    const list = try reg.makeList(TypeRegistry.I64);
    try std.testing.expect(!reg.isTrivial(list));
    try std.testing.expect(reg.needsARC(list));
}

test "managed pointer flag" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    const ptr_i32 = try reg.makePointer(TypeRegistry.I32);
    try std.testing.expect(!reg.get(ptr_i32).pointer.flags.is_managed);
    const my_struct = try reg.add(.{ .struct_type = .{ .name = "S", .fields = &.{}, .size = 8, .alignment = 8 } });
    const ptr_struct = try reg.makePointer(my_struct);
    try std.testing.expect(reg.get(ptr_struct).pointer.flags.is_managed);
    try std.testing.expect(reg.couldBeARC(ptr_struct));
}

test "ValueWitnessTable layout matches Swift ABI" {
    try std.testing.expectEqual(@as(usize, 88), @sizeOf(ValueWitnessTable));
    try std.testing.expectEqual(@as(usize, 112), @sizeOf(EnumValueWitnessTable));
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(TypeMetadata));
    try std.testing.expectEqual(@as(usize, 24), ValueWitnessTable.ValueBufferSize);
}

test "ValueWitnessFlags packing" {
    const f = ValueWitnessFlags.init(8, true, true, true, false);
    try std.testing.expectEqual(@as(u32, 8), f.getAlignment());
    try std.testing.expect(f.isPOD());
    try std.testing.expect(f.isInlineStorage());
}

test "type predicates" {
    try std.testing.expect(isNumeric(.{ .basic = .i32_type }));
    try std.testing.expect(!isNumeric(.{ .basic = .bool_type }));
    try std.testing.expect(isBool(.{ .basic = .bool_type }));
    try std.testing.expect(isUntyped(.{ .basic = .untyped_int }));
}
