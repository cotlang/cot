//! Assemble — convert preprocessed Prog chain into Wasm binary bytes.

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const Prog = prog.Prog;
const Addr = prog.Addr;
const Symbol = prog.Symbol;

const debug = @import("foundation").debug;
fn debugLog(comptime fmt: []const u8, args: anytype) void {
    debug.log(.codegen, fmt, args);
}

pub const AssembledFunc = struct {
    code: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *AssembledFunc) void {
        self.allocator.free(self.code);
    }
};

const RegVar = struct {
    global: bool,
    index: u64,
};

const VarDecl = struct {
    count: u64,
    typ: c.ValType,
};

pub fn assemble(allocator: std.mem.Allocator, sym: *Symbol) !AssembledFunc {
    debugLog("assemble: '{s}'", .{sym.name});

    var w = std.ArrayListUnmanaged(u8){};
    errdefer w.deinit(allocator);

    var max_local_idx: u64 = 0;
    var has_dispatch_loop = false;
    var p: ?*Prog = sym.text;
    while (p) |current| {
        switch (current.as) {
            .local_get => {
                if (current.from.type == .const_int) {
                    const idx: u64 = @intCast(current.from.offset);
                    if (idx >= max_local_idx) max_local_idx = idx + 1;
                }
            },
            .local_set, .local_tee => {
                if (current.to.type == .const_int) {
                    const idx: u64 = @intCast(current.to.offset);
                    if (idx >= max_local_idx) max_local_idx = idx + 1;
                }
            },
            .br_table => has_dispatch_loop = true,
            else => {},
        }
        p = current.link;
    }

    var reg_vars: [128]?RegVar = [_]?RegVar{null} ** 128;
    reg_vars[@intFromEnum(c.Reg.sp)] = .{ .global = true, .index = 0 };
    reg_vars[@intFromEnum(c.Reg.ctxt)] = .{ .global = true, .index = 1 };
    reg_vars[@intFromEnum(c.Reg.g)] = .{ .global = true, .index = 2 };
    reg_vars[@intFromEnum(c.Reg.ret0)] = .{ .global = true, .index = 3 };
    reg_vars[@intFromEnum(c.Reg.ret1)] = .{ .global = true, .index = 4 };
    reg_vars[@intFromEnum(c.Reg.ret2)] = .{ .global = true, .index = 5 };
    reg_vars[@intFromEnum(c.Reg.ret3)] = .{ .global = true, .index = 6 };
    reg_vars[@intFromEnum(c.Reg.pause)] = .{ .global = true, .index = 7 };
    reg_vars[@intFromEnum(c.Reg.pc_b)] = .{ .global = false, .index = sym.param_count };

    const param_count: u64 = sym.param_count;
    const float_count: u64 = sym.float_local_count;

    const gc_ref_locals = sym.gc_ref_locals;
    const gc_ref_count: u64 = gc_ref_locals.len;

    const total_value_locals: u64 = if (max_local_idx > param_count + 1)
        max_local_idx - param_count - 1
    else
        0;
    const i64_value_locals: u64 = if (total_value_locals >= float_count + gc_ref_count)
        total_value_locals - float_count - gc_ref_count
    else if (total_value_locals >= float_count)
        total_value_locals - float_count
    else
        total_value_locals;

    var num_groups: u64 = 0;
    if (has_dispatch_loop) num_groups += 1;
    if (i64_value_locals > 0) num_groups += 1;
    if (float_count > 0) num_groups += 1;
    num_groups += gc_ref_count;

    if (num_groups == 0 and !has_dispatch_loop and max_local_idx <= param_count) {
        try writeULEB128(allocator, &w, 0);
    } else {
        try writeULEB128(allocator, &w, num_groups);
        if (has_dispatch_loop) {
            try writeULEB128(allocator, &w, 1);
            try w.append(allocator, @intFromEnum(c.ValType.i32));
        }
        if (i64_value_locals > 0) {
            try writeULEB128(allocator, &w, i64_value_locals);
            try w.append(allocator, @intFromEnum(c.ValType.i64));
        }
        if (float_count > 0) {
            try writeULEB128(allocator, &w, float_count);
            try w.append(allocator, @intFromEnum(c.ValType.f64));
        }
        for (gc_ref_locals) |gc_type_idx| {
            try writeULEB128(allocator, &w, 1);
            try w.append(allocator, c.GC_REF_TYPE_NULL);
            try writeULEB128(allocator, &w, gc_type_idx);
        }
    }

    debugLog("  {d} total locals, {d} params, {d} i64, {d} f64", .{ max_local_idx, param_count, i64_value_locals, float_count });

    if (gc_ref_locals.len > 0) {
        const ref_local_start: u64 = if (has_dispatch_loop) param_count + 1 + i64_value_locals + float_count else param_count + i64_value_locals + float_count;
        for (gc_ref_locals, 0..) |gc_type_idx, i| {
            const ref_local_idx = ref_local_start + i;
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_NEW_DEFAULT);
            try writeULEB128(allocator, &w, gc_type_idx);
            try w.append(allocator, 0x21);
            try writeULEB128(allocator, &w, ref_local_idx);
        }
    }

    p = sym.text;
    while (p) |current| {
        try encodeInstruction(allocator, &w, current, &reg_vars);
        p = current.link;
    }

    try w.append(allocator, c.As.end.opcode().?);

    debugLog("  assembled {d} bytes", .{w.items.len});

    return .{
        .code = try w.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

fn encodeInstruction(
    allocator: std.mem.Allocator,
    w: *std.ArrayListUnmanaged(u8),
    p: *const Prog,
    reg_vars: *const [128]?RegVar,
) !void {
    switch (p.as) {
        .get => {
            if (p.from.type != .reg) {
                return error.BadGetOperand;
            }
            const reg = p.from.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            if (v.global) {
                try writeOpcode(allocator, w, .global_get);
            } else {
                try writeOpcode(allocator, w, .local_get);
            }
            try writeULEB128(allocator, w, v.index);
        },

        .set => {
            if (p.to.type != .reg) {
                return error.BadSetOperand;
            }
            const reg = p.to.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            if (v.global) {
                try writeOpcode(allocator, w, .global_set);
            } else {
                try writeOpcode(allocator, w, .local_set);
            }
            try writeULEB128(allocator, w, v.index);
        },

        .tee => {
            if (p.to.type != .reg) {
                return error.BadTeeOperand;
            }
            const reg = p.to.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            try writeOpcode(allocator, w, .local_tee);
            try writeULEB128(allocator, w, v.index);
        },

        .not => {
            try writeOpcode(allocator, w, .i32_eqz);
        },

        .block, .loop, .@"if" => {
            try writeOpcode(allocator, w, p.as);
            if (p.from.offset != 0) {
                const block_type: u8 = @truncate(@as(u64, @bitCast(0x80 - p.from.offset)));
                try w.append(allocator, block_type);
            } else {
                try w.append(allocator, c.BLOCK_VOID);
            }
        },

        .br, .br_if => {
            try writeOpcode(allocator, w, p.as);
            if (p.to.type != .const_int) {
                return error.BadBranchOperand;
            }
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .br_table => {
            try writeOpcode(allocator, w, .br_table);
            const idxs = p.to.val.br_table;
            try writeULEB128(allocator, w, idxs.len - 1);
            for (idxs) |idx| {
                try writeULEB128(allocator, w, idx);
            }
        },

        .call, .return_call => {
            try writeOpcode(allocator, w, p.as);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            } else if (p.to.sym) |s| {
                try writeULEB128(allocator, w, s.index);
            } else {
                return error.BadCallOperand;
            }
        },

        .call_indirect => {
            try writeOpcode(allocator, w, .call_indirect);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            try w.append(allocator, c.TABLE_IDX_ZERO);
        },

        .i32_const, .i64_const => {
            try writeOpcode(allocator, w, p.as);
            if (p.from.name == .@"extern") {
                if (p.from.sym) |s| {
                    try writeSLEB128(allocator, w, @as(i64, @intCast(s.index)) + p.from.offset);
                } else {
                    try writeSLEB128(allocator, w, p.from.offset);
                }
            } else {
                try writeSLEB128(allocator, w, p.from.offset);
            }
        },

        .f32_const => {
            try writeOpcode(allocator, w, .f32_const);
            const val: f32 = @floatCast(p.from.val.float);
            const bytes = @as([4]u8, @bitCast(val));
            try w.appendSlice(allocator, &bytes);
        },

        .f64_const => {
            try writeOpcode(allocator, w, .f64_const);
            const bytes = @as([8]u8, @bitCast(p.from.val.float));
            try w.appendSlice(allocator, &bytes);
        },

        .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
            try writeOpcode(allocator, w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .i32_store, .i64_store, .f32_store, .f64_store, .i32_store8, .i32_store16, .i64_store8, .i64_store16, .i64_store32 => {
            try writeOpcode(allocator, w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .memory_size, .memory_grow, .memory_fill => {
            try writeOpcode(allocator, w, p.as);
            try w.append(allocator, c.MEMORY_IDX_ZERO);
        },

        .memory_copy => {
            try writeOpcode(allocator, w, p.as);
            try w.append(allocator, c.MEMORY_IDX_ZERO);
            try w.append(allocator, c.MEMORY_IDX_ZERO);
        },

        .nop, .text, .func_data, .pc_data => {},

        .undef => {
            try writeOpcode(allocator, w, .@"unreachable");
        },

        .local_get => {
            try writeOpcode(allocator, w, .local_get);
            if (p.from.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            }
        },

        .local_set => {
            try writeOpcode(allocator, w, .local_set);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        .local_tee => {
            try writeOpcode(allocator, w, .local_tee);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        .global_get => {
            try writeOpcode(allocator, w, .global_get);
            if (p.from.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            }
        },

        .global_set => {
            try writeOpcode(allocator, w, .global_set);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        .gc_struct_new => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_NEW);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_struct_get => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_GET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .gc_struct_set => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_SET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .gc_array_new => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_NEW);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_array_new_default => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_NEW_DEFAULT);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_array_new_fixed => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_NEW_FIXED);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .gc_array_new_data => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_NEW_DATA);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .gc_array_get => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_GET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_array_set => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_SET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_array_len => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_LEN);
        },

        .gc_array_copy => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_ARRAY_COPY);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .ref_test => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_REF_TEST);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .ref_cast => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_REF_CAST);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .br_on_cast => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_BR_ON_CAST);
            const flags: u64 = @intCast(p.from.offset & 0xFF);
            const from_ht: u64 = @intCast(@as(u64, @bitCast(p.from.offset)) >> 8);
            const label: u64 = @intCast(p.to.offset & 0xFFFFFFFF);
            const to_ht: u64 = @intCast(@as(u64, @bitCast(p.to.offset)) >> 32);
            try writeULEB128(allocator, w, flags);
            try writeULEB128(allocator, w, label);
            try writeULEB128(allocator, w, from_ht);
            try writeULEB128(allocator, w, to_ht);
        },

        .br_on_cast_fail => {
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_BR_ON_CAST_FAIL);
            const flags: u64 = @intCast(p.from.offset & 0xFF);
            const from_ht: u64 = @intCast(@as(u64, @bitCast(p.from.offset)) >> 8);
            const label: u64 = @intCast(p.to.offset & 0xFFFFFFFF);
            const to_ht: u64 = @intCast(@as(u64, @bitCast(p.to.offset)) >> 32);
            try writeULEB128(allocator, w, flags);
            try writeULEB128(allocator, w, label);
            try writeULEB128(allocator, w, from_ht);
            try writeULEB128(allocator, w, to_ht);
        },

        .ref_null => {
            try w.append(allocator, c.REF_NULL);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .ref_func => {
            try w.append(allocator, c.REF_FUNC);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .call_ref => {
            try w.append(allocator, c.CALL_REF);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .return_call_ref => {
            try w.append(allocator, c.RETURN_CALL_REF);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .ref_eq => {
            try w.append(allocator, c.REF_EQ);
        },

        .ref_as_not_null => {
            try w.append(allocator, c.REF_AS_NOT_NULL);
        },

        .br_on_null => {
            try w.append(allocator, c.BR_ON_NULL);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .br_on_non_null => {
            try w.append(allocator, c.BR_ON_NON_NULL);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        else => {
            try writeOpcode(allocator, w, p.as);
        },
    }
}

fn alignment(as: c.As) u64 {
    return switch (as) {
        .i32_load8_s, .i32_load8_u, .i64_load8_s, .i64_load8_u, .i32_store8, .i64_store8 => 0,
        .i32_load16_s, .i32_load16_u, .i64_load16_s, .i64_load16_u, .i32_store16, .i64_store16 => 1,
        .i32_load, .f32_load, .i64_load32_s, .i64_load32_u, .i32_store, .f32_store, .i64_store32 => 2,
        .i64_load, .f64_load, .i64_store, .f64_store => 3,
        else => 0,
    };
}

fn writeOpcode(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), as: c.As) !void {
    if (as.isFcPrefixed()) {
        try w.append(allocator, c.FC_PREFIX);
        try w.append(allocator, as.fcOpcode().?);
    } else if (as.opcode()) |op| {
        try w.append(allocator, op);
    }
}

const LEB128_LOW_BITS_MASK: u8 = 0x7F;
const LEB128_CONTINUATION_BIT: u8 = 0x80;
const LEB128_SIGN_BIT: u8 = 0x40;

pub fn writeULEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: u64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(v & LEB128_LOW_BITS_MASK);
        v >>= 7;
        if (v == 0) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | LEB128_CONTINUATION_BIT);
    }
}

pub fn writeSLEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(@as(u64, @bitCast(v)) & LEB128_LOW_BITS_MASK);
        const s: u8 = @truncate(@as(u64, @bitCast(v)) & LEB128_SIGN_BIT);
        v >>= 7;
        const done = (v == 0 and s == 0) or (v == -1 and s != 0);
        if (done) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | LEB128_CONTINUATION_BIT);
    }
}

const testing = std.testing;

test "ULEB128 encoding" {
    const allocator = testing.allocator;
    var w = std.ArrayListUnmanaged(u8){};
    defer w.deinit(allocator);

    try writeULEB128(allocator, &w, 0);
    try testing.expectEqualSlices(u8, &[_]u8{0x00}, w.items);

    w.clearRetainingCapacity();
    try writeULEB128(allocator, &w, 127);
    try testing.expectEqualSlices(u8, &[_]u8{0x7F}, w.items);

    w.clearRetainingCapacity();
    try writeULEB128(allocator, &w, 128);
    try testing.expectEqualSlices(u8, &[_]u8{ 0x80, 0x01 }, w.items);
}

test "SLEB128 encoding" {
    const allocator = testing.allocator;
    var w = std.ArrayListUnmanaged(u8){};
    defer w.deinit(allocator);

    try writeSLEB128(allocator, &w, 0);
    try testing.expectEqualSlices(u8, &[_]u8{0x00}, w.items);

    w.clearRetainingCapacity();
    try writeSLEB128(allocator, &w, -1);
    try testing.expectEqualSlices(u8, &[_]u8{0x7F}, w.items);

    w.clearRetainingCapacity();
    try writeSLEB128(allocator, &w, 42);
    try testing.expectEqualSlices(u8, &[_]u8{42}, w.items);
}

test "alignment values" {
    try testing.expectEqual(@as(u64, 0), alignment(.i64_load8_s));
    try testing.expectEqual(@as(u64, 1), alignment(.i64_load16_s));
    try testing.expectEqual(@as(u64, 2), alignment(.i32_load));
    try testing.expectEqual(@as(u64, 3), alignment(.i64_load));
}
