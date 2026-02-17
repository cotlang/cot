//! Type representation for Cot.

const std = @import("std");

pub const TypeIndex = u32;
pub const invalid_type: TypeIndex = std.math.maxInt(TypeIndex);

pub const BasicKind = enum(u8) {
    invalid,
    bool_type,
    i8_type, i16_type, i32_type, i64_type,
    u8_type, u16_type, u32_type, u64_type,
    f32_type, f64_type,
    void_type,
    noreturn_type,
    untyped_int, untyped_float, untyped_bool, untyped_null,

    pub fn name(self: BasicKind) []const u8 {
        return switch (self) {
            .invalid => "invalid", .bool_type => "bool", .void_type => "void", .noreturn_type => "noreturn",
            .i8_type => "i8", .i16_type => "i16", .i32_type => "i32", .i64_type => "i64",
            .u8_type => "u8", .u16_type => "u16", .u32_type => "u32", .u64_type => "u64",
            .f32_type => "f32", .f64_type => "f64",
            .untyped_int => "untyped int", .untyped_float => "untyped float",
            .untyped_bool => "untyped bool", .untyped_null => "untyped null",
        };
    }

    pub fn isNumeric(self: BasicKind) bool { return self.isInteger() or self.isFloat(); }

    pub fn isInteger(self: BasicKind) bool {
        return switch (self) {
            .i8_type, .i16_type, .i32_type, .i64_type, .u8_type, .u16_type, .u32_type, .u64_type, .untyped_int => true,
            else => false,
        };
    }

    pub fn isSigned(self: BasicKind) bool {
        return switch (self) { .i8_type, .i16_type, .i32_type, .i64_type => true, else => false };
    }

    pub fn isUnsigned(self: BasicKind) bool {
        return switch (self) { .u8_type, .u16_type, .u32_type, .u64_type => true, else => false };
    }

    pub fn isFloat(self: BasicKind) bool {
        return switch (self) { .f32_type, .f64_type, .untyped_float => true, else => false };
    }

    pub fn isUntyped(self: BasicKind) bool {
        return switch (self) { .untyped_int, .untyped_float, .untyped_bool, .untyped_null => true, else => false };
    }

    pub fn size(self: BasicKind) u8 {
        return switch (self) {
            .bool_type, .i8_type, .u8_type => 1,
            .i16_type, .u16_type => 2,
            .i32_type, .u32_type, .f32_type => 4,
            .i64_type, .u64_type, .f64_type => 8,
            else => 0,
        };
    }
};

// Composite types
pub const PointerType = struct { elem: TypeIndex };
pub const OptionalType = struct { elem: TypeIndex };
pub const ErrorSetType = struct { name: []const u8, variants: []const []const u8 };
pub const ErrorUnionType = struct { elem: TypeIndex, error_set: TypeIndex = std.math.maxInt(TypeIndex) };
pub const SliceType = struct { elem: TypeIndex };
pub const ArrayType = struct { elem: TypeIndex, length: u64 };
pub const MapType = struct { key: TypeIndex, value: TypeIndex };
pub const ListType = struct { elem: TypeIndex };
pub const TupleType = struct { element_types: []const TypeIndex };
pub const FutureType = struct { result_type: TypeIndex };

// Aggregate types
pub const StructField = struct { name: []const u8, type_idx: TypeIndex, offset: u32, default_value: @import("ast.zig").NodeIndex = @import("ast.zig").null_node };
pub const StructLayout = @import("ast.zig").StructLayout;
pub const StructType = struct { name: []const u8, fields: []const StructField, size: u32, alignment: u8, layout: StructLayout = .auto };
pub const EnumVariant = struct { name: []const u8, value: i64 };
pub const EnumType = struct { name: []const u8, variants: []const EnumVariant, backing_type: TypeIndex };
pub const UnionVariant = struct { name: []const u8, payload_type: TypeIndex };
pub const UnionType = struct { name: []const u8, variants: []const UnionVariant, tag_type: TypeIndex };
pub const FuncParam = struct { name: []const u8, type_idx: TypeIndex };
pub const FuncType = struct { params: []const FuncParam, return_type: TypeIndex };

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
    future: FutureType,

    pub fn underlying(self: Type) Type { return self; }
    pub fn isInvalid(self: Type) bool { return self == .basic and self.basic == .invalid; }
};

pub const MethodInfo = struct {
    name: []const u8,
    func_name: []const u8,
    func_type: TypeIndex,
    receiver_is_ptr: bool,
};

pub const TypeRegistry = struct {
    types: std.ArrayListUnmanaged(Type),
    allocator: std.mem.Allocator,
    name_map: std.StringHashMap(TypeIndex),
    method_registry: std.StringHashMap(std.ArrayListUnmanaged(MethodInfo)),

    // Pre-registered basic type indices
    pub const INVALID: TypeIndex = 0;
    pub const BOOL: TypeIndex = 1;
    pub const I8: TypeIndex = 2;
    pub const I16: TypeIndex = 3;
    pub const I32: TypeIndex = 4;
    pub const I64: TypeIndex = 5;
    pub const U8: TypeIndex = 6;
    pub const U16: TypeIndex = 7;
    pub const U32: TypeIndex = 8;
    pub const U64: TypeIndex = 9;
    pub const F32: TypeIndex = 10;
    pub const F64: TypeIndex = 11;
    pub const VOID: TypeIndex = 12;
    pub const UNTYPED_INT: TypeIndex = 13;
    pub const UNTYPED_FLOAT: TypeIndex = 14;
    pub const UNTYPED_BOOL: TypeIndex = 15;
    pub const UNTYPED_NULL: TypeIndex = 16;
    pub const STRING: TypeIndex = 17;
    pub const SSA_MEM: TypeIndex = 18;
    pub const SSA_FLAGS: TypeIndex = 19;
    pub const SSA_TUPLE: TypeIndex = 20;
    pub const SSA_RESULTS: TypeIndex = 21;
    pub const NORETURN: TypeIndex = 22;
    pub const FIRST_USER_TYPE: TypeIndex = 23;
    pub const INT: TypeIndex = I64;
    pub const FLOAT: TypeIndex = F64;

    pub fn basicTypeName(type_idx: TypeIndex) []const u8 {
        return switch (type_idx) {
            INVALID => "invalid", BOOL => "bool", VOID => "void",
            I8 => "i8", I16 => "i16", I32 => "i32", I64 => "i64",
            U8 => "u8", U16 => "u16", U32 => "u32", U64 => "u64",
            F32 => "f32", F64 => "f64",
            UNTYPED_INT => "untyped_int", UNTYPED_FLOAT => "untyped_float",
            UNTYPED_BOOL => "untyped_bool", UNTYPED_NULL => "untyped_null",
            STRING => "string", SSA_MEM => "ssa_mem", SSA_FLAGS => "ssa_flags",
            SSA_TUPLE => "ssa_tuple", SSA_RESULTS => "ssa_results", NORETURN => "noreturn",
            else => "composite",
        };
    }

    pub fn basicTypeSize(type_idx: TypeIndex) u8 {
        return switch (type_idx) {
            VOID, SSA_MEM, SSA_FLAGS, SSA_TUPLE, SSA_RESULTS, NORETURN => 0,
            BOOL, UNTYPED_BOOL, I8, U8 => 1,
            I16, U16 => 2,
            I32, U32, F32 => 4,
            I64, U64, F64, UNTYPED_INT => 8,
            STRING => 16,
            else => 8,
        };
    }

    pub fn init(allocator: std.mem.Allocator) !TypeRegistry {
        var reg = TypeRegistry{
            .types = .{},
            .allocator = allocator,
            .name_map = std.StringHashMap(TypeIndex).init(allocator),
            .method_registry = std.StringHashMap(std.ArrayListUnmanaged(MethodInfo)).init(allocator),
        };

        // Register basic types in order (0-16)
        inline for (.{ .invalid, .bool_type, .i8_type, .i16_type, .i32_type, .i64_type, .u8_type, .u16_type, .u32_type, .u64_type, .f32_type, .f64_type, .void_type, .untyped_int, .untyped_float, .untyped_bool, .untyped_null }) |k| {
            try reg.types.append(allocator, .{ .basic = k });
        }
        try reg.types.append(allocator, .{ .slice = .{ .elem = U8 } }); // 17 = STRING
        // SSA placeholders (18-21)
        for (0..4) |_| try reg.types.append(allocator, .{ .basic = .void_type });
        // 22 = NORETURN
        try reg.types.append(allocator, .{ .basic = .noreturn_type });

        // Register type names
        const names = .{ .{ "bool", BOOL }, .{ "i8", I8 }, .{ "i16", I16 }, .{ "i32", I32 }, .{ "i64", I64 }, .{ "int", INT }, .{ "u8", U8 }, .{ "u16", U16 }, .{ "u32", U32 }, .{ "u64", U64 }, .{ "f32", F32 }, .{ "f64", F64 }, .{ "float", F64 }, .{ "void", VOID }, .{ "string", STRING }, .{ "byte", U8 }, .{ "noreturn", NORETURN } };
        inline for (names) |pair| try reg.name_map.put(pair[0], pair[1]);

        return reg;
    }

    pub fn deinit(self: *TypeRegistry) void {
        self.types.deinit(self.allocator);
        self.name_map.deinit();
        var it = self.method_registry.valueIterator();
        while (it.next()) |list| list.deinit(self.allocator);
        self.method_registry.deinit();
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

    pub fn get(self: *const TypeRegistry, idx: TypeIndex) Type {
        if (idx == invalid_type or idx >= self.types.items.len) return .{ .basic = .invalid };
        return self.types.items[idx];
    }

    pub fn lookupByName(self: *const TypeRegistry, n: []const u8) ?TypeIndex { return self.name_map.get(n); }

    /// Returns the display name of a type for diagnostics and trait bound validation.
    /// Rust: ty::TyKind::to_string(). Go 1.18: types2.Type.String().
    pub fn typeName(self: *const TypeRegistry, idx: TypeIndex) []const u8 {
        return switch (self.get(idx)) {
            .basic => |bk| bk.name(),
            .struct_type => |s| s.name,
            .enum_type => |e| e.name,
            .pointer => "pointer",
            .tuple => "tuple",
            .future => "Future",
            else => "unknown",
        };
    }

    pub fn add(self: *TypeRegistry, t: Type) !TypeIndex {
        const idx: TypeIndex = @intCast(self.types.items.len);
        try self.types.append(self.allocator, t);
        return idx;
    }

    pub fn registerNamed(self: *TypeRegistry, n: []const u8, idx: TypeIndex) !void { try self.name_map.put(n, idx); }

    pub fn makePointer(self: *TypeRegistry, elem: TypeIndex) !TypeIndex { return self.add(.{ .pointer = .{ .elem = elem } }); }
    pub fn makeOptional(self: *TypeRegistry, elem: TypeIndex) !TypeIndex { return self.add(.{ .optional = .{ .elem = elem } }); }
    pub fn makeErrorUnion(self: *TypeRegistry, elem: TypeIndex) !TypeIndex { return self.add(.{ .error_union = .{ .elem = elem } }); }
    pub fn makeErrorUnionWithSet(self: *TypeRegistry, elem: TypeIndex, error_set: TypeIndex) !TypeIndex { return self.add(.{ .error_union = .{ .elem = elem, .error_set = error_set } }); }
    pub fn makeSlice(self: *TypeRegistry, elem: TypeIndex) !TypeIndex { return self.add(.{ .slice = .{ .elem = elem } }); }
    pub fn makeArray(self: *TypeRegistry, elem: TypeIndex, len: u64) !TypeIndex { return self.add(.{ .array = .{ .elem = elem, .length = len } }); }
    pub fn makeMap(self: *TypeRegistry, key: TypeIndex, value: TypeIndex) !TypeIndex { return self.add(.{ .map = .{ .key = key, .value = value } }); }
    pub fn makeList(self: *TypeRegistry, elem: TypeIndex) !TypeIndex { return self.add(.{ .list = .{ .elem = elem } }); }
    pub fn makeTuple(self: *TypeRegistry, element_types: []const TypeIndex) !TypeIndex {
        return self.add(.{ .tuple = .{ .element_types = try self.allocator.dupe(TypeIndex, element_types) } });
    }

    /// Returns the byte offset of element `index` within a tuple.
    pub fn tupleElementOffset(self: *const TypeRegistry, tuple_idx: TypeIndex, index: u32) u32 {
        const tup = self.get(tuple_idx).tuple;
        var offset: u32 = 0;
        for (0..index) |i| offset += ((self.sizeOf(tup.element_types[i]) + 7) / 8) * 8;
        return offset;
    }

    pub fn makeFunc(self: *TypeRegistry, params: []const FuncParam, ret: TypeIndex) !TypeIndex {
        return self.add(.{ .func = .{ .params = try self.allocator.dupe(FuncParam, params), .return_type = ret } });
    }

    pub fn makeFuture(self: *TypeRegistry, result_type: TypeIndex) !TypeIndex {
        return self.add(.{ .future = .{ .result_type = result_type } });
    }

    pub fn isPointer(self: *const TypeRegistry, idx: TypeIndex) bool { return self.get(idx) == .pointer; }
    pub fn pointerElem(self: *const TypeRegistry, idx: TypeIndex) TypeIndex {
        return if (self.get(idx) == .pointer) self.get(idx).pointer.elem else invalid_type;
    }
    pub fn isArray(self: *const TypeRegistry, idx: TypeIndex) bool { return self.get(idx) == .array; }
    pub fn isSlice(self: *const TypeRegistry, idx: TypeIndex) bool { return self.get(idx) == .slice; }

    pub fn sizeOf(self: *const TypeRegistry, idx: TypeIndex) u32 {
        if (idx == UNTYPED_INT or idx == UNTYPED_FLOAT) return 8;
        return switch (self.get(idx)) {
            .basic => |k| k.size(),
            .pointer, .map, .list, .func, .error_set, .future => 8,
            .tuple => |tup| blk: {
                var total: u32 = 0;
                for (tup.element_types) |et| total += ((self.sizeOf(et) + 7) / 8) * 8;
                break :blk total;
            },
            .optional, .error_union => 16,
            .slice => 24,  // Go's slice: (ptr=8, len=8, cap=8)
            .array => |a| @intCast(self.sizeOf(a.elem) * a.length),
            .struct_type => |s| s.size,
            .enum_type => |e| self.sizeOf(e.backing_type),
            .union_type => |u| blk: {
                var max_payload: u32 = 0;
                for (u.variants) |v| {
                    if (v.payload_type != invalid_type) {
                        const ps = self.sizeOf(v.payload_type);
                        if (ps > max_payload) max_payload = ps;
                    }
                }
                const payload_aligned = if (max_payload == 0) @as(u32, 0) else ((max_payload + 7) / 8) * 8;
                break :blk 8 + payload_aligned;
            },
        };
    }

    pub fn alignmentOf(self: *const TypeRegistry, idx: TypeIndex) u32 {
        return switch (self.get(idx)) {
            .basic => |k| if (k.size() == 0) 1 else k.size(),
            .pointer, .func, .optional, .error_union, .error_set, .slice, .map, .list, .union_type, .tuple, .future => 8,
            .array => |a| self.alignmentOf(a.elem),
            .struct_type => |s| s.alignment,
            .enum_type => |e| self.alignmentOf(e.backing_type),
        };
    }

    pub fn alignOf(self: *const TypeRegistry, idx: TypeIndex) u32 { return self.alignmentOf(idx); }

    /// Returns true if the type is "trivial" (doesn't need ARC).
    /// Copies Swift's TypeLowering::isTrivial() pattern.
    /// Trivial types: primitives, raw pointers, void
    /// Non-trivial types: heap-allocated objects (require retain/release)
    ///
    /// Reference: swift/lib/SIL/TypeLowering.cpp
    pub fn isTrivial(self: *const TypeRegistry, idx: TypeIndex) bool {
        const t = self.get(idx);
        return switch (t) {
            // All basic types are trivial (primitives, void)
            .basic => true,
            // Raw pointers are trivial (no ownership)
            .pointer => true,
            // Functions are trivial (just code pointers)
            .func => true,
            // Error sets are trivial (just integer indices)
            .error_set => true,
            // Enums with trivial backing type are trivial
            .enum_type => |e| self.isTrivial(e.backing_type),
            // Arrays/slices of trivial elements are trivial
            .array => |a| self.isTrivial(a.elem),
            .slice => |s| self.isTrivial(s.elem),
            // Optionals of trivial types are trivial
            .optional => |o| self.isTrivial(o.elem),
            .error_union => |e| self.isTrivial(e.elem),
            // Struct types: check if they have a heap flag
            // For now, all structs are stack-allocated (trivial)
            // When M18 adds heap allocation, this will check for heap objects
            .struct_type => true,
            // Collections and futures are non-trivial (heap-allocated)
            .map, .list, .future => false,
            // Union types are trivial for now
            .union_type => true,
            // Tuples are trivial if all elements are trivial
            .tuple => |tup| {
                for (tup.element_types) |et| if (!self.isTrivial(et)) return false;
                return true;
            },
        };
    }

    /// Returns true if the type needs ARC (opposite of isTrivial).
    /// Reference: Swift's ManagedValue pattern
    pub fn needsARC(self: *const TypeRegistry, idx: TypeIndex) bool {
        return !self.isTrivial(idx);
    }

    /// Returns true if type could be an ARC-managed heap pointer.
    /// `new T` creates ARC-managed *T for struct, enum, or union types.
    /// Also handles ?*T (optional pointer wrapping an ARC type).
    /// Reference: Swift TypeLowering — class types (heap) are non-trivial.
    pub fn couldBeARC(self: *const TypeRegistry, idx: TypeIndex) bool {
        const t = self.get(idx);
        if (t == .pointer) {
            const pointee = self.get(t.pointer.elem);
            return pointee == .struct_type or pointee == .enum_type or pointee == .union_type;
        }
        // Optional wrapping an ARC pointer: ?*T
        if (t == .optional) {
            return self.couldBeARC(t.optional.elem);
        }
        return false;
    }

    pub fn equal(self: *const TypeRegistry, a: TypeIndex, b: TypeIndex) bool {
        if (a == b) return true;
        if (a == invalid_type or b == invalid_type) return false;
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
                for (tup_a.element_types, tb.tuple.element_types) |a_elem, b_elem| {
                    if (!self.equal(a_elem, b_elem)) return false;
                }
                return true;
            },
            .func => false,
            .future => |fa| self.equal(fa.result_type, tb.future.result_type),
        };
    }

    pub fn isAssignable(self: *const TypeRegistry, from: TypeIndex, to: TypeIndex) bool {
        if (from == to) return true;
        if (from == invalid_type or to == invalid_type) return true;
        // noreturn is the bottom type — coerces to anything (Zig pattern)
        if (from == NORETURN) return true;
        const from_t = self.get(from);
        const to_t = self.get(to);

        // Untyped conversions
        if (from_t == .basic) {
            if (from_t.basic == .untyped_int and to_t == .basic and to_t.basic.isInteger()) return true;
            if (from_t.basic == .untyped_float and to_t == .basic and to_t.basic.isFloat()) return true;
            if (from_t.basic == .untyped_bool and to_t == .basic and to_t.basic == .bool_type) return true;
            if (from_t.basic == .untyped_null and (to_t == .optional or to_t == .pointer)) return true;
        }

        // T -> ?T
        if (to_t == .optional) return self.isAssignable(from, to_t.optional.elem);

        // T -> E!T (success value coercion) and ErrorSet -> E!T (error coercion)
        if (to_t == .error_union) {
            if (self.isAssignable(from, to_t.error_union.elem)) return true;
            if (from_t == .error_set) return true;
        }

        // Same basic types
        if (from_t == .basic and to_t == .basic and from_t.basic == to_t.basic) return true;

        // Integer widening: smaller integers can be assigned to larger integers
        // u8 -> u16/u32/u64/i16/i32/i64, i8 -> i16/i32/i64, etc.
        if (from_t == .basic and to_t == .basic and from_t.basic.isInteger() and to_t.basic.isInteger()) {
            const from_size = from_t.basic.size();
            const to_size = to_t.basic.size();
            const from_signed = from_t.basic.isSigned();
            const to_signed = to_t.basic.isSigned();

            // Same signedness: allow if target is larger or equal
            if (from_signed == to_signed) return to_size >= from_size;

            // Unsigned to signed: allow if target is strictly larger (to fit sign bit)
            if (!from_signed and to_signed) return to_size > from_size;

            // Signed to unsigned: not allowed implicitly (could lose sign)
            return false;
        }

        // Slices
        if (from_t == .slice and to_t == .slice) return self.equal(from_t.slice.elem, to_t.slice.elem);
        if (from_t == .array and to_t == .slice) return self.equal(from_t.array.elem, to_t.slice.elem);

        // STRING -> *u8
        if (from == STRING and to_t == .pointer and to_t.pointer.elem == U8) return true;

        // Named types
        if (from_t == .struct_type and to_t == .struct_type) return std.mem.eql(u8, from_t.struct_type.name, to_t.struct_type.name);
        if (from_t == .enum_type and to_t == .enum_type) return std.mem.eql(u8, from_t.enum_type.name, to_t.enum_type.name);

        // Enum to backing type (implicit conversion)
        // Go reference: Go enums are typed ints, implicit conversion allowed
        if (from_t == .enum_type) return self.isAssignable(from_t.enum_type.backing_type, to);

        // Union to tag type (implicit conversion for simple unions without payload)
        if (from_t == .union_type) return self.isAssignable(from_t.union_type.tag_type, to);
        if (from_t == .pointer and to_t == .pointer) return self.equal(from_t.pointer.elem, to_t.pointer.elem);
        if (from_t == .array and to_t == .array) return from_t.array.length == to_t.array.length and self.isAssignable(from_t.array.elem, to_t.array.elem);

        // Tuple types
        if (from_t == .tuple and to_t == .tuple) {
            if (from_t.tuple.element_types.len != to_t.tuple.element_types.len) return false;
            for (from_t.tuple.element_types, to_t.tuple.element_types) |fe, te| {
                if (!self.isAssignable(fe, te)) return false;
            }
            return true;
        }

        // Function types
        if (from_t == .func and to_t == .func) {
            if (from_t.func.params.len != to_t.func.params.len) return false;
            for (from_t.func.params, to_t.func.params) |fp, tp| if (!self.equal(fp.type_idx, tp.type_idx)) return false;
            return self.equal(from_t.func.return_type, to_t.func.return_type);
        }

        return false;
    }
};

// Type predicates
pub fn isNumeric(t: Type) bool { return if (t == .basic) t.basic.isNumeric() else false; }
pub fn isInteger(t: Type) bool { return if (t == .basic) t.basic.isInteger() else false; }
pub fn isBool(t: Type) bool { return if (t == .basic) (t.basic == .bool_type or t.basic == .untyped_bool) else false; }
pub fn isUntyped(t: Type) bool { return if (t == .basic) t.basic.isUntyped() else false; }

// ============================================================================
// Tests
// ============================================================================

test "BasicKind predicates" {
    try std.testing.expect(BasicKind.i32_type.isInteger());
    try std.testing.expect(BasicKind.i32_type.isNumeric());
    try std.testing.expect(BasicKind.i32_type.isSigned());
    try std.testing.expect(!BasicKind.i32_type.isUnsigned());
    try std.testing.expect(BasicKind.u64_type.isUnsigned());
    try std.testing.expect(BasicKind.f64_type.isFloat());
    try std.testing.expect(BasicKind.untyped_int.isUntyped());
}

test "BasicKind size" {
    try std.testing.expectEqual(@as(u8, 1), BasicKind.bool_type.size());
    try std.testing.expectEqual(@as(u8, 4), BasicKind.i32_type.size());
    try std.testing.expectEqual(@as(u8, 8), BasicKind.i64_type.size());
}

test "TypeRegistry init and lookup" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    try std.testing.expectEqual(TypeRegistry.BOOL, reg.lookupByName("bool").?);
    try std.testing.expectEqual(TypeRegistry.I64, reg.lookupByName("int").?);
    try std.testing.expectEqual(TypeRegistry.STRING, reg.lookupByName("string").?);
    try std.testing.expectEqual(BasicKind.bool_type, reg.get(TypeRegistry.BOOL).basic);
}

test "TypeRegistry make composite types" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    const ptr_i32 = try reg.makePointer(TypeRegistry.I32);
    try std.testing.expect(reg.get(ptr_i32) == .pointer);
    try std.testing.expectEqual(TypeRegistry.I32, reg.get(ptr_i32).pointer.elem);
    const arr = try reg.makeArray(TypeRegistry.I32, 10);
    try std.testing.expectEqual(@as(u64, 10), reg.get(arr).array.length);
}

test "Type predicates" {
    try std.testing.expect(isNumeric(.{ .basic = .i32_type }));
    try std.testing.expect(!isNumeric(.{ .basic = .bool_type }));
    try std.testing.expect(isBool(.{ .basic = .bool_type }));
    try std.testing.expect(isUntyped(.{ .basic = .untyped_int }));
}

test "TypeRegistry sizeOf" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();
    try std.testing.expectEqual(@as(u32, 1), reg.sizeOf(TypeRegistry.BOOL));
    try std.testing.expectEqual(@as(u32, 8), reg.sizeOf(TypeRegistry.I64));
    try std.testing.expectEqual(@as(u32, 24), reg.sizeOf(TypeRegistry.STRING));  // Go slice: (ptr=8, len=8, cap=8)
}

test "invalid_type" {
    try std.testing.expectEqual(std.math.maxInt(u32), invalid_type);
}

test "isTrivial for ARC" {
    var reg = try TypeRegistry.init(std.testing.allocator);
    defer reg.deinit();

    // Basic types are trivial
    try std.testing.expect(reg.isTrivial(TypeRegistry.I64));
    try std.testing.expect(reg.isTrivial(TypeRegistry.F64));
    try std.testing.expect(reg.isTrivial(TypeRegistry.BOOL));
    try std.testing.expect(reg.isTrivial(TypeRegistry.VOID));

    // Pointers are trivial (raw, no ownership)
    const ptr_i32 = try reg.makePointer(TypeRegistry.I32);
    try std.testing.expect(reg.isTrivial(ptr_i32));

    // Arrays of trivial elements are trivial
    const arr_i64 = try reg.makeArray(TypeRegistry.I64, 10);
    try std.testing.expect(reg.isTrivial(arr_i64));

    // Slices of trivial elements are trivial
    const slice_i64 = try reg.makeSlice(TypeRegistry.I64);
    try std.testing.expect(reg.isTrivial(slice_i64));

    // Lists are non-trivial (heap-allocated)
    const list_i64 = try reg.makeList(TypeRegistry.I64);
    try std.testing.expect(!reg.isTrivial(list_i64));
    try std.testing.expect(reg.needsARC(list_i64));

    // Maps are non-trivial (heap-allocated)
    const map_i64_i64 = try reg.makeMap(TypeRegistry.I64, TypeRegistry.I64);
    try std.testing.expect(!reg.isTrivial(map_i64_i64));
    try std.testing.expect(reg.needsARC(map_i64_i64));
}
