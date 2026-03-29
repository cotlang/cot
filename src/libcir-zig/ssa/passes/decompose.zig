//! Decompose Pass - Decompose compound type phi nodes.
//!
//! This pass decomposes phi ops on compound builtin types into phi
//! ops on simple types, then rewritedec handles the extraction decomposition.
//!
//! Pattern for slices:
//!   phi<slice>(s1, s2) ->
//!   ptr_phi = phi(slice_ptr(s1), slice_ptr(s2))
//!   len_phi = phi(slice_len(s1), slice_len(s2))
//!   result = slice_make(ptr_phi, len_phi)
//!
//! Pattern for strings:
//!   phi<string>(s1, s2) ->
//!   ptr_phi = phi(string_ptr(s1), string_ptr(s2))
//!   len_phi = phi(string_len(s1), string_len(s2))
//!   result = string_make(ptr_phi, len_phi)

const std = @import("std");
const Value = @import("../value.zig").Value;
const TypeIndex = foundation.types.TypeIndex;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const debug = foundation.debug;

pub fn decompose(allocator: std.mem.Allocator, f: *Func, type_reg: ?*TypeRegistry) !void {
    var total_phis: usize = 0;
    var total_values: usize = 0;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            total_values += 1;
            if (v.op == .phi) total_phis += 1;
        }
    }
    debug.log(.codegen, "=== Decompose pass for '{s}' ({d} blocks, {d} values, {d} phis) ===", .{
        f.name, f.blocks.items.len, total_values, total_phis,
    });

    var decomposed: usize = 0;

    for (f.blocks.items) |block| {
        for (block.values.items) |v| {
            if (v.op != .phi) continue;

            if (try decomposeBuiltinPhi(allocator, f, block, v, type_reg)) {
                decomposed += 1;
            }
        }
    }

    debug.log(.codegen, "=== Decompose complete for '{s}': {d} phis decomposed ===", .{
        f.name, decomposed,
    });
}

fn decomposeBuiltinPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value, type_reg: ?*TypeRegistry) !bool {
    if (isSliceType(v.type_idx, f)) {
        return try decomposeSlicePhi(allocator, f, block, v);
    }

    if (v.type_idx == TypeRegistry.STRING) {
        return try decomposeStringPhi(allocator, f, block, v);
    }

    if (type_reg) |tr| {
        if (isOptPtrType(v.type_idx, tr)) {
            return try decomposeOptPtrPhi(allocator, f, block, v);
        }
    }

    return false;
}

fn decomposeSlicePhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing slice phi with {d} args", .{ v.id, v.args.len });

    const ptr_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, ptr_phi);

    const len_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, len_phi);

    const cap_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, cap_phi);

    for (v.args) |a| {
        const arg_block = a.block orelse block;

        const ptr_extract = try f.newValue(.slice_ptr, TypeRegistry.I64, arg_block, v.pos);
        ptr_extract.addArg(a);
        try arg_block.addValue(allocator, ptr_extract);
        try ptr_phi.addArgAlloc(ptr_extract, allocator);

        const len_extract = try f.newValue(.slice_len, TypeRegistry.I64, arg_block, v.pos);
        len_extract.addArg(a);
        try arg_block.addValue(allocator, len_extract);
        try len_phi.addArgAlloc(len_extract, allocator);

        const cap_extract = try f.newValue(.slice_cap, TypeRegistry.I64, arg_block, v.pos);
        cap_extract.addArg(a);
        try arg_block.addValue(allocator, cap_extract);
        try cap_phi.addArgAlloc(cap_extract, allocator);
    }

    const result = try f.newValue(.slice_make, v.type_idx, block, v.pos);
    result.addArg(ptr_phi);
    result.addArg(len_phi);
    try result.addArgAlloc(cap_phi, allocator);
    try block.addValue(allocator, result);

    copyOf(v, result);

    return true;
}

fn decomposeStringPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing string phi with {d} args", .{ v.id, v.args.len });

    const ptr_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, ptr_phi);

    const len_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, len_phi);

    for (v.args) |a| {
        const arg_block = a.block orelse block;

        const ptr_extract = try f.newValue(.string_ptr, TypeRegistry.I64, arg_block, v.pos);
        ptr_extract.addArg(a);
        try arg_block.addValue(allocator, ptr_extract);
        try ptr_phi.addArgAlloc(ptr_extract, allocator);

        const len_extract = try f.newValue(.string_len, TypeRegistry.I64, arg_block, v.pos);
        len_extract.addArg(a);
        try arg_block.addValue(allocator, len_extract);
        try len_phi.addArgAlloc(len_extract, allocator);
    }

    const result = try f.newValue(.string_make, TypeRegistry.STRING, block, v.pos);
    result.addArg2(ptr_phi, len_phi);
    try block.addValue(allocator, result);

    copyOf(v, result);

    return true;
}

fn decomposeOptPtrPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing opt_ptr phi with {d} args", .{ v.id, v.args.len });

    const tag_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, tag_phi);

    const data_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, data_phi);

    for (v.args) |a| {
        const arg_block = a.block orelse block;

        const tag_extract = try f.newValue(.opt_tag, TypeRegistry.I64, arg_block, v.pos);
        tag_extract.addArg(a);
        try arg_block.addValue(allocator, tag_extract);
        try tag_phi.addArgAlloc(tag_extract, allocator);

        const data_extract = try f.newValue(.opt_data, TypeRegistry.I64, arg_block, v.pos);
        data_extract.addArg(a);
        try arg_block.addValue(allocator, data_extract);
        try data_phi.addArgAlloc(data_extract, allocator);
    }

    const result = try f.newValue(.opt_make, v.type_idx, block, v.pos);
    result.addArg2(tag_phi, data_phi);
    try block.addValue(allocator, result);

    copyOf(v, result);

    return true;
}

fn isOptPtrType(type_idx: TypeIndex, type_reg: *TypeRegistry) bool {
    const info = type_reg.get(type_idx);
    if (info != .optional) return false;
    const elem_info = type_reg.get(info.optional.elem);
    return elem_info == .pointer and elem_info.pointer.flags.is_managed;
}

fn isSliceType(type_idx: TypeIndex, f: *Func) bool {
    _ = f;
    return type_idx != TypeRegistry.STRING and
        type_idx != TypeRegistry.I64 and
        type_idx != TypeRegistry.I32 and
        type_idx != TypeRegistry.BOOL and
        type_idx != TypeRegistry.VOID and
        type_idx != TypeRegistry.SSA_MEM;
}

fn copyOf(v: *Value, src: *Value) void {
    for (v.args) |arg| {
        arg.uses -= 1;
    }

    v.op = .copy;
    v.aux_int = 0;
    v.aux = .none;

    v.args_storage[0] = src;
    v.args = v.args_storage[0..1];
    v.args_dynamic = false;
    v.args_capacity = 0;

    src.uses += 1;
}

const testing = std.testing;

test "decomposeSlicePhi basic" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_slice_phi");
    defer f.deinit();

    const SLICE_TYPE: TypeIndex = @enumFromInt(100);

    const block1 = try f.newBlock(.first);
    const block2 = try f.newBlock(.plain);

    const ptr1 = try f.newValue(.const_64, TypeRegistry.I64, block1, .{});
    ptr1.aux_int = 100;
    try block1.addValue(allocator, ptr1);

    const len1 = try f.newValue(.const_64, TypeRegistry.I64, block1, .{});
    len1.aux_int = 10;
    try block1.addValue(allocator, len1);

    const slice1 = try f.newValue(.slice_make, SLICE_TYPE, block1, .{});
    slice1.addArg(ptr1);
    slice1.addArg(len1);
    try slice1.addArgAlloc(len1, allocator);
    try block1.addValue(allocator, slice1);

    const ptr2 = try f.newValue(.const_64, TypeRegistry.I64, block2, .{});
    ptr2.aux_int = 200;
    try block2.addValue(allocator, ptr2);

    const len2 = try f.newValue(.const_64, TypeRegistry.I64, block2, .{});
    len2.aux_int = 20;
    try block2.addValue(allocator, len2);

    const slice2 = try f.newValue(.slice_make, SLICE_TYPE, block2, .{});
    slice2.addArg(ptr2);
    slice2.addArg(len2);
    try slice2.addArgAlloc(len2, allocator);
    try block2.addValue(allocator, slice2);

    const merge = try f.newBlock(.plain);
    const phi = try f.newValue(.phi, SLICE_TYPE, merge, .{});
    phi.addArg2(slice1, slice2);
    try merge.addValue(allocator, phi);

    try decompose(allocator, &f, null);

    try testing.expectEqual(Op.copy, phi.op);
}
