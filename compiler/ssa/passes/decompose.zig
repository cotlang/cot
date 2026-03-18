//! Decompose Pass - Decompose compound type phi nodes.
//!
//! Go reference: cmd/compile/internal/ssa/decompose.go
//!
//! This pass decomposes phi ops on compound builtin types into phi
//! ops on simple types, then rewritedec handles the extraction decomposition.
//!
//! Pattern for slices (Go: decomposeSlicePhi lines 159-176):
//!   phi<slice>(s1, s2)
//!   →
//!   ptr_phi = phi(slice_ptr(s1), slice_ptr(s2))
//!   len_phi = phi(slice_len(s1), slice_len(s2))
//!   result = slice_make(ptr_phi, len_phi)
//!
//! Pattern for strings (Go: decomposeStringPhi lines 143-157):
//!   phi<string>(s1, s2)
//!   →
//!   ptr_phi = phi(string_ptr(s1), string_ptr(s2))
//!   len_phi = phi(string_len(s1), string_len(s2))
//!   result = string_make(ptr_phi, len_phi)

const std = @import("std");
const Value = @import("../value.zig").Value;
const TypeIndex = @import("../value.zig").TypeIndex;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const TypeRegistry = @import("../../frontend/types.zig").TypeRegistry;
const debug = @import("../../pipeline_debug.zig");

/// Run the decomposition pass.
/// Go reference: decompose.go decomposeBuiltin
pub fn decompose(allocator: std.mem.Allocator, f: *Func, type_reg: ?*TypeRegistry) !void {
    debug.log(.codegen, "decompose: processing '{s}'", .{f.name});

    var decomposed: usize = 0;

    // Decompose phi nodes on compound types
    // Go: lines 17-25
    for (f.blocks.items) |block| {
        for (block.values.items) |v| {
            if (v.op != .phi) continue;

            if (try decomposeBuiltinPhi(allocator, f, block, v, type_reg)) {
                decomposed += 1;
            }
        }
    }

    debug.log(.codegen, "  decomposed {d} phi nodes", .{decomposed});
}

/// Decompose a phi node if it's a compound builtin type.
/// Go reference: decompose.go decomposeBuiltinPhi (lines 124-141)
fn decomposeBuiltinPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value, type_reg: ?*TypeRegistry) !bool {
    // Check if phi type is a slice
    if (isSliceType(v.type_idx, f)) {
        return try decomposeSlicePhi(allocator, f, block, v);
    }

    // Check if phi type is a string
    if (v.type_idx == TypeRegistry.STRING) {
        return try decomposeStringPhi(allocator, f, block, v);
    }

    // Check if phi type is ?*T (optional managed pointer)
    if (type_reg) |tr| {
        if (isOptPtrType(v.type_idx, tr)) {
            return try decomposeOptPtrPhi(allocator, f, block, v);
        }
    }

    return false;
}

/// Decompose a slice phi node.
/// Go reference: decompose.go decomposeSlicePhi (lines 159-176)
///
/// Before: phi<slice>(s1, s2)
/// After:  ptr_phi = phi(slice_ptr(s1), slice_ptr(s2))
///         len_phi = phi(slice_len(s1), slice_len(s2))
///         cap_phi = phi(slice_cap(s1), slice_cap(s2))
///         result = slice_make(ptr_phi, len_phi, cap_phi)
/// Go reference: decompose.go always creates 3-component SliceMake
fn decomposeSlicePhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing slice phi with {d} args", .{ v.id, v.args.len });

    // Create ptr_phi = phi<ptr>()
    const ptr_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, ptr_phi);

    // Create len_phi = phi<i64>()
    const len_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, len_phi);

    // Create cap_phi = phi<i64>()
    const cap_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, cap_phi);

    // For each arg of the original phi, extract ptr, len, and cap
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

    // Create result = slice_make(ptr_phi, len_phi, cap_phi)
    // Go: SliceMake always has 3 args
    const result = try f.newValue(.slice_make, v.type_idx, block, v.pos);
    result.addArg(ptr_phi);
    result.addArg(len_phi);
    try result.addArgAlloc(cap_phi, allocator);
    try block.addValue(allocator, result);

    // Replace v with result (make v a copy of result)
    // Go: v.reset(OpSliceMake)
    copyOf(v, result);

    return true;
}

/// Decompose a string phi node.
/// Go reference: decompose.go decomposeStringPhi (lines 143-157)
///
/// Before: phi<string>(s1, s2)
/// After:  ptr_phi = phi(string_ptr(s1), string_ptr(s2))
///         len_phi = phi(string_len(s1), string_len(s2))
///         result = string_make(ptr_phi, len_phi)
fn decomposeStringPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing string phi with {d} args", .{ v.id, v.args.len });

    // Create ptr_phi = phi<ptr>()
    const ptr_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, ptr_phi);

    // Create len_phi = phi<i64>()
    const len_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, len_phi);

    // For each arg of the original phi, extract ptr and len
    for (v.args) |a| {
        const arg_block = a.block orelse block; // Use phi's block if arg has no block

        // Create string_ptr(a) and add to ptr_phi
        const ptr_extract = try f.newValue(.string_ptr, TypeRegistry.I64, arg_block, v.pos);
        ptr_extract.addArg(a);
        try arg_block.addValue(allocator, ptr_extract);
        try ptr_phi.addArgAlloc(ptr_extract, allocator);

        // Create string_len(a) and add to len_phi
        const len_extract = try f.newValue(.string_len, TypeRegistry.I64, arg_block, v.pos);
        len_extract.addArg(a);
        try arg_block.addValue(allocator, len_extract);
        try len_phi.addArgAlloc(len_extract, allocator);
    }

    // Create result = string_make(ptr_phi, len_phi)
    const result = try f.newValue(.string_make, TypeRegistry.STRING, block, v.pos);
    result.addArg2(ptr_phi, len_phi);
    try block.addValue(allocator, result);

    // Replace v with result
    copyOf(v, result);

    return true;
}

/// Decompose an optional pointer (?*T) phi node.
/// Go reference: decompose.go decomposeInterfacePhi (ITab/IData pattern)
///
/// Before: phi<?*T>(o1, o2)
/// After:  tag_phi = phi(opt_tag(o1), opt_tag(o2))
///         data_phi = phi(opt_data(o1), opt_data(o2))
///         result = opt_make(tag_phi, data_phi)
fn decomposeOptPtrPhi(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len == 0) return false;

    debug.log(.codegen, "  v{d}: decomposing opt_ptr phi with {d} args", .{ v.id, v.args.len });

    // Create tag_phi = phi<i64>()
    const tag_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, tag_phi);

    // Create data_phi = phi<i64>()
    const data_phi = try f.newValue(.phi, TypeRegistry.I64, block, v.pos);
    try block.addValue(allocator, data_phi);

    // For each arg of the original phi, extract tag and data
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

    // Create result = opt_make(tag_phi, data_phi)
    const result = try f.newValue(.opt_make, v.type_idx, block, v.pos);
    result.addArg2(tag_phi, data_phi);
    try block.addValue(allocator, result);

    // Replace v with result
    copyOf(v, result);

    return true;
}

/// Check if a type is an optional managed pointer (?*T).
fn isOptPtrType(type_idx: TypeIndex, type_reg: *TypeRegistry) bool {
    const info = type_reg.get(type_idx);
    if (info != .optional) return false;
    const elem_info = type_reg.get(info.optional.elem);
    return elem_info == .pointer and elem_info.pointer.managed;
}

/// Check if a type is a slice type.
/// Go reference: Type.IsSlice()
fn isSliceType(type_idx: TypeIndex, f: *Func) bool {
    _ = f; // Func not used for now

    // Check if it's not a basic type (slice types have higher indices)
    // This is a heuristic - proper implementation would check type registry
    return type_idx != TypeRegistry.STRING and
        type_idx != TypeRegistry.I64 and
        type_idx != TypeRegistry.I32 and
        type_idx != TypeRegistry.BOOL and
        type_idx != TypeRegistry.VOID;
}

/// Copy a value's identity to another (Go's v.copyOf).
fn copyOf(v: *Value, src: *Value) void {
    // Decrement uses on old args
    for (v.args) |arg| {
        arg.uses -= 1;
    }

    // Set as copy of src
    v.op = .copy;
    v.aux_int = 0;
    v.aux = .none;

    // Reset args to just src
    v.args_storage[0] = src;
    v.args = v.args_storage[0..1];
    v.args_dynamic = false;
    v.args_capacity = 0;

    src.uses += 1;
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "decomposeSlicePhi basic" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_slice_phi");
    defer f.deinit();

    // Use a fake slice type index (high number that passes isSliceType check)
    const SLICE_TYPE: TypeIndex = 100;

    // Create two blocks for the phi
    const block1 = try f.newBlock(.first);
    const block2 = try f.newBlock(.plain);

    // Create slice_make values in each block
    const ptr1 = try f.newValue(.const_64, TypeRegistry.I64, block1, .{});
    ptr1.aux_int = 100;
    try block1.addValue(allocator, ptr1);

    const len1 = try f.newValue(.const_64, TypeRegistry.I64, block1, .{});
    len1.aux_int = 10;
    try block1.addValue(allocator, len1);

    // Go: SliceMake always 3 args (ptr, len, cap)
    const slice1 = try f.newValue(.slice_make, SLICE_TYPE, block1, .{});
    slice1.addArg(ptr1);
    slice1.addArg(len1);
    try slice1.addArgAlloc(len1, allocator); // cap = len
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
    try slice2.addArgAlloc(len2, allocator); // cap = len
    try block2.addValue(allocator, slice2);

    // Create merge block with phi
    const merge = try f.newBlock(.plain);
    const phi = try f.newValue(.phi, SLICE_TYPE, merge, .{});
    phi.addArg2(slice1, slice2);
    try merge.addValue(allocator, phi);

    // Run decompose - phi should be decomposed because SLICE_TYPE is not a basic type
    try decompose(allocator, &f, null);

    // Verify phi was decomposed (became a copy to the result)
    try testing.expectEqual(Op.copy, phi.op);
}
