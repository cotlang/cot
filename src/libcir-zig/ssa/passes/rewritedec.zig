//! Decomposition Rewrite Pass - Decompose compound type extractions.
//!
//! This pass decomposes extraction operations on compound types:
//! - slice_ptr(slice_make(ptr, len, cap)) -> ptr
//! - slice_len(slice_make(ptr, len, cap)) -> len
//! - string_ptr(string_make(ptr, len)) -> ptr
//! - string_len(string_make(ptr, len)) -> len
//!
//! For loads from memory, it also decomposes:
//! - slice_ptr(load<slice> ptr) -> load<ptr>(ptr)
//! - slice_len(load<slice> ptr) -> load<i64>(ptr + 8)
//! - string_ptr(load<string> ptr) -> load<ptr>(ptr)
//! - string_len(load<string> ptr) -> load<i64>(ptr + 8)
//!
//! This pass runs AFTER rewritegeneric (which converts const_string to string_make).

const std = @import("std");
const value_mod = @import("../value.zig");
const Value = value_mod.Value;
const TypeIndex = foundation.types.TypeIndex;
const Pos = value_mod.Pos;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const debug = foundation.debug;

pub fn rewrite(allocator: std.mem.Allocator, f: *Func) !void {
    var total_values: usize = 0;
    for (f.blocks.items) |b| total_values += b.values.items.len;
    debug.log(.codegen, "=== Rewritedec pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name, f.blocks.items.len, total_values,
    });

    var rewritten: usize = 0;
    var iterations: usize = 0;
    const max_iterations = 100;

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

    debug.log(.codegen, "=== Rewritedec complete for '{s}': {d} rewrites in {d} iterations ===", .{
        f.name, rewritten, iterations,
    });
}

fn rewriteValue(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    return switch (v.op) {
        .slice_ptr => rewriteSlicePtr(allocator, f, block, v),
        .slice_len => rewriteSliceLen(allocator, f, block, v),
        .slice_cap => rewriteSliceCap(allocator, f, block, v),
        .string_ptr => rewriteStringPtr(allocator, f, block, v),
        .string_len => rewriteStringLen(allocator, f, block, v),
        .string_concat => rewriteStringConcat(allocator, f, block, v),
        .opt_tag => rewriteOptTag(allocator, f, block, v),
        .opt_data => rewriteOptData(allocator, f, block, v),
        else => false,
    };
}

fn rewriteSlicePtr(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .slice_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: slice_ptr(slice_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

    if (v_0.op == .load and isSliceType(v_0.type_idx)) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_ptr(load<slice>) -> load<ptr>", .{v.id});

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

    if (v_0.op == .string_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: slice_ptr(string_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

    if (v_0.op == .load and v_0.type_idx == TypeRegistry.STRING) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_ptr(load<string>) -> load<ptr>", .{v.id});

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

    if (v_0.op == .const_nil and (v_0.type_idx == TypeRegistry.STRING or isSliceType(v_0.type_idx))) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: slice_ptr(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteSliceLen(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .slice_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: slice_len(slice_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    if (v_0.op == .load and isSliceType(v_0.type_idx)) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_len(load<slice>) -> load<i64>(off_ptr 8)", .{v.id});

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

    if (v_0.op == .string_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: slice_len(string_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    if (v_0.op == .load and v_0.type_idx == TypeRegistry.STRING) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_len(load<string>) -> load<i64>(off_ptr 8)", .{v.id});

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

    if (v_0.op == .const_nil and (v_0.type_idx == TypeRegistry.STRING or isSliceType(v_0.type_idx))) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: slice_len(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteSliceCap(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .slice_make and v_0.args.len >= 3) {
        const cap = v_0.args[2];
        debug.log(.codegen, "  v{d}: slice_cap(slice_make) -> copy v{d}", .{ v.id, cap.id });
        copyOf(v, cap);
        return true;
    }

    if (v_0.op == .slice_make and v_0.args.len == 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: slice_cap(slice_make[2-arg]) -> copy v{d} (cap=len)", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

    if (v_0.op == .load and isSliceType(v_0.type_idx)) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: slice_cap(load<slice>) -> load<i64>(off_ptr 16)", .{v.id});

            const off_ptr = try f.newValue(.off_ptr, TypeRegistry.I64, block, v.pos);
            off_ptr.aux_int = 16;
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

    if (v_0.op == .const_nil and isSliceType(v_0.type_idx)) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: slice_cap(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteStringPtr(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .string_make and v_0.args.len >= 1) {
        const ptr = v_0.args[0];
        debug.log(.codegen, "  v{d}: string_ptr(string_make) -> copy v{d}", .{ v.id, ptr.id });
        copyOf(v, ptr);
        return true;
    }

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

    if (v_0.op == .const_nil and v_0.type_idx == TypeRegistry.STRING) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: string_ptr(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteStringLen(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .string_make and v_0.args.len >= 2) {
        const len = v_0.args[1];
        debug.log(.codegen, "  v{d}: string_len(string_make) -> copy v{d}", .{ v.id, len.id });
        copyOf(v, len);
        return true;
    }

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

    if (v_0.op == .const_nil and v_0.type_idx == TypeRegistry.STRING) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: string_len(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteStringConcat(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 2) return false;
    const s1 = v.args[0];
    const s2 = v.args[1];

    debug.log(.codegen, "  v{d}: string_concat -> static_call + string_make", .{v.id});

    const s1_ptr = try extractStringPtr(allocator, f, block, s1, v.pos);
    const s1_len = try extractStringLen(allocator, f, block, s1, v.pos);

    const s2_ptr = try extractStringPtr(allocator, f, block, s2, v.pos);
    const s2_len = try extractStringLen(allocator, f, block, s2, v.pos);

    const call = try f.newValue(.static_call, TypeRegistry.I64, block, v.pos);
    call.aux = .{ .string = "string_concat" };
    call.addArg(s1_ptr);
    call.addArg(s1_len);
    call.addArg(s2_ptr);
    try call.addArgAlloc(s2_len, allocator);
    try block.addValue(allocator, call);

    const new_len = try f.newValue(.add, TypeRegistry.I64, block, v.pos);
    new_len.addArg2(s1_len, s2_len);
    try block.addValue(allocator, new_len);

    const result = try f.newValue(.string_make, TypeRegistry.STRING, block, v.pos);
    result.addArg2(call, new_len);
    try block.addValue(allocator, result);

    copyOf(v, result);
    return true;
}

fn rewriteOptTag(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .opt_make and v_0.args.len >= 1) {
        const tag = v_0.args[0];
        debug.log(.codegen, "  v{d}: opt_tag(opt_make) -> copy v{d}", .{ v.id, tag.id });
        copyOf(v, tag);
        return true;
    }

    if (v_0.op == .load) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: opt_tag(load) -> load<i64>", .{v.id});
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

    if (v_0.op == .const_nil) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: opt_tag(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn rewriteOptData(allocator: std.mem.Allocator, f: *Func, block: *Block, v: *Value) !bool {
    if (v.args.len < 1) return false;
    const v_0 = followCopy(v.args[0]);

    if (v_0.op == .opt_make and v_0.args.len >= 2) {
        const data = v_0.args[1];
        debug.log(.codegen, "  v{d}: opt_data(opt_make) -> copy v{d}", .{ v.id, data.id });
        copyOf(v, data);
        return true;
    }

    if (v_0.op == .load) {
        if (v_0.args.len >= 1) {
            const ptr = v_0.args[0];
            debug.log(.codegen, "  v{d}: opt_data(load) -> load<i64>(off_ptr 8)", .{v.id});
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

    if (v_0.op == .const_nil) {
        const zero = try f.newValue(.const_int, TypeRegistry.I64, block, v.pos);
        zero.aux_int = 0;
        try block.addValue(allocator, zero);
        debug.log(.codegen, "  v{d}: opt_data(const_nil) -> const_int(0)", .{v.id});
        copyOf(v, zero);
        return true;
    }

    return false;
}

fn extractStringPtr(allocator: std.mem.Allocator, f: *Func, block: *Block, s: *Value, pos: Pos) !*Value {
    if ((s.op == .string_make or s.op == .slice_make) and s.args.len >= 1) {
        return s.args[0];
    }

    if (s.op == .load and s.type_idx == TypeRegistry.STRING) {
        if (s.args.len >= 1) {
            const base_ptr = s.args[0];
            const load_val = try f.newValue(.load, TypeRegistry.I64, block, pos);
            load_val.addArg(base_ptr);
            if (s.args.len >= 2) {
                load_val.addArg(s.args[1]);
            }
            try block.addValue(allocator, load_val);
            return load_val;
        }
    }

    const ptr_val = try f.newValue(.string_ptr, TypeRegistry.I64, block, pos);
    ptr_val.addArg(s);
    try block.addValue(allocator, ptr_val);
    return ptr_val;
}

fn extractStringLen(allocator: std.mem.Allocator, f: *Func, block: *Block, s: *Value, pos: Pos) !*Value {
    if ((s.op == .string_make or s.op == .slice_make) and s.args.len >= 2) {
        return s.args[1];
    }

    if (s.op == .load and s.type_idx == TypeRegistry.STRING) {
        if (s.args.len >= 1) {
            const base_ptr = s.args[0];
            const off_ptr = try f.newValue(.off_ptr, TypeRegistry.I64, block, pos);
            off_ptr.aux_int = 8;
            off_ptr.addArg(base_ptr);
            try block.addValue(allocator, off_ptr);

            const load_val = try f.newValue(.load, TypeRegistry.I64, block, pos);
            load_val.addArg(off_ptr);
            if (s.args.len >= 2) {
                load_val.addArg(s.args[1]);
            }
            try block.addValue(allocator, load_val);
            return load_val;
        }
    }

    const len_val = try f.newValue(.string_len, TypeRegistry.I64, block, pos);
    len_val.addArg(s);
    try block.addValue(allocator, len_val);
    return len_val;
}

fn followCopy(v: *Value) *Value {
    var current = v;
    while (current.op == .copy and current.args.len >= 1) {
        current = current.args[0];
    }
    return current;
}

fn isSliceType(type_idx: TypeIndex) bool {
    return type_idx != TypeRegistry.STRING and
        type_idx != TypeRegistry.I64 and
        type_idx != TypeRegistry.I32 and
        type_idx != TypeRegistry.BOOL;
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

test "rewriteSliceLen from slice_make" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_slice_len");
    defer f.deinit();

    const block = try f.newBlock(.first);

    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    ptr_val.aux_int = 100;
    try block.addValue(allocator, ptr_val);

    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    len_val.aux_int = 42;
    try block.addValue(allocator, len_val);

    const slice_make = try f.newValue(.slice_make, TypeRegistry.I64, block, .{});
    slice_make.addArg(ptr_val);
    slice_make.addArg(len_val);
    try slice_make.addArgAlloc(len_val, allocator);
    try block.addValue(allocator, slice_make);

    const slice_len = try f.newValue(.slice_len, TypeRegistry.I64, block, .{});
    slice_len.addArg(slice_make);
    try block.addValue(allocator, slice_len);

    try rewrite(allocator, &f);

    try testing.expectEqual(Op.copy, slice_len.op);
    try testing.expectEqual(@as(usize, 1), slice_len.args.len);
    try testing.expectEqual(len_val, slice_len.args[0]);
}

test "rewriteStringLen from string_make" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_string_len");
    defer f.deinit();

    const block = try f.newBlock(.first);

    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    ptr_val.aux_int = 100;
    try block.addValue(allocator, ptr_val);

    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, .{});
    len_val.aux_int = 5;
    try block.addValue(allocator, len_val);

    const string_make = try f.newValue(.string_make, TypeRegistry.STRING, block, .{});
    string_make.addArg2(ptr_val, len_val);
    try block.addValue(allocator, string_make);

    const string_len = try f.newValue(.string_len, TypeRegistry.I64, block, .{});
    string_len.addArg(string_make);
    try block.addValue(allocator, string_len);

    try rewrite(allocator, &f);

    try testing.expectEqual(Op.copy, string_len.op);
    try testing.expectEqual(@as(usize, 1), string_len.args.len);
    try testing.expectEqual(len_val, string_len.args[0]);
}
