//! Decomposition Rewrite Pass - Decompose compound type extractions.
//!
//! Go reference: cmd/compile/internal/ssa/rewritedec.go
//!
//! This pass decomposes extraction operations on compound types:
//! - slice_ptr(slice_make(ptr, len, cap)) → ptr
//! - slice_len(slice_make(ptr, len, cap)) → len
//! - string_ptr(string_make(ptr, len)) → ptr
//! - string_len(string_make(ptr, len)) → len
//!
//! For loads from memory, it also decomposes:
//! - slice_ptr(load<slice> ptr) → load<ptr>(ptr)
//! - slice_len(load<slice> ptr) → load<i64>(ptr + 8)
//! - string_ptr(load<string> ptr) → load<ptr>(ptr)
//! - string_len(load<string> ptr) → load<i64>(ptr + 8)
//!
//! This pass runs AFTER rewritegeneric (which converts const_string to string_make).

const std = @import("std");
const Value = @import("../value.zig").Value;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const TypeIndex = @import("../value.zig").TypeIndex;
const TypeRegistry = @import("../../frontend/types.zig").TypeRegistry;
const debug = @import("../../pipeline_debug.zig");

/// Run the decomposition rewrite pass.
/// Go reference: rewritedec.go main loop
pub fn rewrite(allocator: std.mem.Allocator, f: *Func) !void {
    debug.log(.codegen, "rewritedec: processing '{s}'", .{f.name});

    var rewritten: usize = 0;
    var iterations: usize = 0;
    const max_iterations = 100; // Safety limit

    // Iterate until no more rewrites (fixpoint)
    // Go does multiple passes to handle nested decompositions
    while (iterations < max_iterations) {
        var changed = false;
        iterations += 1;

        for (f.blocks.items) |block| {
            for (block.values.items) |v| {
                const did_rewrite = try rewriteValue(allocator, f, block, v);
                if (did_rewrite) {
                    changed = true;
                    rewritten += 1;
                }
            }
        }

        if (!changed) break;
    }

    debug.log(.codegen, "  rewritten {d} values in {d} iterations", .{ rewritten, iterations });
}

/// Rewrite a single value if applicable.
/// Go reference: rewriteValuedec dispatch
fn rewriteValue(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    return switch (v.op) {
        // ====================================================================
        // Slice decomposition (Go: rewriteValuedec_OpSlicePtr/Len)
        // ====================================================================
        .slice_ptr => rewriteSlicePtr(allocator, f, block, v),
        .slice_len => rewriteSliceLen(allocator, f, block, v),

        // ====================================================================
        // String decomposition (Go: rewriteValuedec_OpStringPtr/Len)
        // ====================================================================
        .string_ptr => rewriteStringPtr(allocator, f, block, v),
        .string_len => rewriteStringLen(allocator, f, block, v),

        else => false,
    };
}

/// Rewrite SlicePtr extraction.
/// Go reference: rewriteValuedec_OpSlicePtr (lines 539-572)
///
/// Patterns:
///   (SlicePtr (SliceMake ptr _ _)) → ptr
///   (SlicePtr (Load<slice> ptr mem)) → (Load<ptr> ptr mem)
fn rewriteSlicePtr(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = v.args[0];

    // Pattern 1: SlicePtr(SliceMake ptr _ _) → ptr
    // Go: lines 542-551
    if (v_0.op == .slice_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: slice_ptr(slice_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

    // Pattern 2: SlicePtr(Load<slice> ptr mem) → Load<ptr> ptr mem
    // Go: lines 552-571
    if (v_0.op == .load and isSliceType(v_0.type_idx)) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_ptr(load<slice>) -> load<ptr>", .{v.id});

            // Create new load of ptr type
            const load_val = try f.newValue(.load, TypeRegistry.I64, block, v.pos);
            load_val.addArg(ptr);
            // Memory arg if present
            if (v_0.args.len >= 2) {
                load_val.addArg(v_0.args[1]);
            }
            try block.addValue(allocator, load_val);

            copyOf(v, load_val);
            return true;
        }
    }

    // Pattern 3: SlicePtr(StringMake ptr _) → ptr (strings use slice ops too)
    if (v_0.op == .string_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: slice_ptr(string_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

    return false;
}

/// Rewrite SliceLen extraction.
/// Go reference: rewriteValuedec_OpSliceLen (lines 499-537)
///
/// Patterns:
///   (SliceLen (SliceMake _ len _)) → len
///   (SliceLen (Load<slice> ptr mem)) → (Load<i64> (OffPtr ptr 8) mem)
fn rewriteSliceLen(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = v.args[0];

    // Pattern 1: SliceLen(SliceMake _ len _) → len
    // Go: lines 504-513
    if (v_0.op == .slice_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: slice_len(slice_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    // Pattern 2: SliceLen(Load<slice> ptr mem) → Load<i64> (OffPtr ptr 8) mem
    // Go: lines 514-536
    if (v_0.op == .load and isSliceType(v_0.type_idx)) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_len(load<slice>) -> load<i64>(off_ptr 8)", .{v.id});

            // Create OffPtr to add 8 to ptr (PtrSize for 64-bit)
            const off_ptr = try f.newValue(.off_ptr, TypeRegistry.I64, block, v.pos);
            off_ptr.aux_int = 8; // offset to length field
            off_ptr.addArg(ptr);
            try block.addValue(allocator, off_ptr);

            // Create load from offset ptr
            const load_val = try f.newValue(.load, TypeRegistry.I64, block, v.pos);
            load_val.addArg(off_ptr);
            if (v_0.args.len >= 2) {
                load_val.addArg(v_0.args[1]); // mem
            }
            try block.addValue(allocator, load_val);

            copyOf(v, load_val);
            return true;
        }
    }

    // Pattern 3: SliceLen(StringMake _ len) → len (strings use slice ops too)
    if (v_0.op == .string_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: slice_len(string_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    return false;
}

/// Rewrite StringPtr extraction.
/// Go reference: rewriteValuedec_OpStringPtr (lines 795-829)
fn rewriteStringPtr(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = v.args[0];

    // Pattern 1: StringPtr(StringMake ptr _) → ptr
    if (v_0.op == .string_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: string_ptr(string_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

    // Pattern 2: StringPtr(Load<string> ptr mem) → Load<ptr> ptr mem
    if (v_0.op == .load and v_0.type_idx == TypeRegistry.STRING) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: string_ptr(load<string>) -> load<ptr>", .{v.id});

            const load_val = try f.newValue(.load, TypeRegistry.I64, block, v.pos);
            load_val.addArg(ptr);
            if (v_0.args.len >= 2) {
                load_val.addArg(v_0.args[1]);
            }
            try block.addValue(allocator, load_val);

            copyOf(v, load_val);
            return true;
        }
    }

    return false;
}

/// Rewrite StringLen extraction.
/// Go reference: rewriteValuedec_OpStringLen (lines 755-793)
fn rewriteStringLen(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = v.args[0];

    // Pattern 1: StringLen(StringMake _ len) → len
    if (v_0.op == .string_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: string_len(string_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    // Pattern 2: StringLen(Load<string> ptr mem) → Load<i64> (OffPtr ptr 8) mem
    if (v_0.op == .load and v_0.type_idx == TypeRegistry.STRING) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: string_len(load<string>) -> load<i64>(off_ptr 8)", .{v.id});

            const off_ptr = try f.newValue(.off_ptr, TypeRegistry.I64, block, v.pos);
            off_ptr.aux_int = 8;
            off_ptr.addArg(ptr);
            try block.addValue(allocator, off_ptr);

            const load_val = try f.newValue(.load, TypeRegistry.I64, block, v.pos);
            load_val.addArg(off_ptr);
            if (v_0.args.len >= 2) {
                load_val.addArg(v_0.args[1]);
            }
            try block.addValue(allocator, load_val);

            copyOf(v, load_val);
            return true;
        }
    }

    return false;
}

/// Check if a type is a slice type.
fn isSliceType(type_idx: TypeIndex) bool {
    // For now, check if it's not a basic type and not STRING
    // A proper implementation would check the type registry
    return type_idx != TypeRegistry.STRING and
        type_idx != TypeRegistry.I64 and
        type_idx != TypeRegistry.I32 and
        type_idx != TypeRegistry.BOOL;
}

/// Copy a value's identity to another (Go's v.copyOf).
/// This makes v become a copy of src.
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

test "rewriteSliceLen from slice_make" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_slice_len");
    defer f.deinit();

    const block = try f.newBlock(.first);

    // Create ptr and len values
    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    ptr_val.aux_int = 100;
    try block.addValue(allocator, ptr_val);

    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    len_val.aux_int = 42;
    try block.addValue(allocator, len_val);

    // Create slice_make(ptr, len)
    const slice_make = try f.newValue(.slice_make, TypeRegistry.I64, block, .{});
    slice_make.addArg2(ptr_val, len_val);
    try block.addValue(allocator, slice_make);

    // Create slice_len(slice_make)
    const slice_len = try f.newValue(.slice_len, TypeRegistry.I64, block, .{});
    slice_len.addArg(slice_make);
    try block.addValue(allocator, slice_len);

    // Run rewrite
    try rewrite(allocator, &f);

    // Verify: slice_len should become copy(len_val)
    try testing.expectEqual(Op.copy, slice_len.op);
    try testing.expectEqual(@as(usize, 1), slice_len.args.len);
    try testing.expectEqual(len_val, slice_len.args[0]);
}

test "rewriteStringLen from string_make" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_string_len");
    defer f.deinit();

    const block = try f.newBlock(.first);

    // Create ptr and len values
    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    ptr_val.aux_int = 100;
    try block.addValue(allocator, ptr_val);

    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    len_val.aux_int = 5;
    try block.addValue(allocator, len_val);

    // Create string_make(ptr, len)
    const string_make = try f.newValue(.string_make, TypeRegistry.STRING, block, .{});
    string_make.addArg2(ptr_val, len_val);
    try block.addValue(allocator, string_make);

    // Create string_len(string_make)
    const string_len = try f.newValue(.string_len, TypeRegistry.I64, block, .{});
    string_len.addArg(string_make);
    try block.addValue(allocator, string_len);

    // Run rewrite
    try rewrite(allocator, &f);

    // Verify: string_len should become copy(len_val)
    try testing.expectEqual(Op.copy, string_len.op);
    try testing.expectEqual(@as(usize, 1), string_len.args.len);
    try testing.expectEqual(len_val, string_len.args[0]);
}
