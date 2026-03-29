//! SSA code generator for WebAssembly — translates SSA values and blocks into Prog chains.

const std = @import("std");
const c = @import("constants.zig");
const prog_mod = @import("prog.zig");
const Prog = prog_mod.Prog;
const Addr = prog_mod.Addr;
const Symbol = prog_mod.Symbol;
const ProgBuilder = prog_mod.ProgBuilder;

const SsaValue = @import("../ssa/value.zig").Value;
const SsaBlock = @import("../ssa/block.zig").Block;
const BlockKind = @import("../ssa/block.zig").BlockKind;
const SsaFunc = @import("../ssa/func.zig").Func;
const SsaOp = @import("../ssa/op.zig").Op;

const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const debug = foundation.debug;

pub const GenError = error{OutOfMemory};

pub const Branch = struct {
    prog: *Prog,
    target_block_id: u32,
};

pub const FuncIndexMap = std.StringHashMapUnmanaged(u32);

pub const GenState = struct {
    allocator: std.mem.Allocator,
    builder: ProgBuilder,
    func: *const SsaFunc,

    value_to_local: std.AutoHashMapUnmanaged(u32, u32),
    next_local: u32 = 0,
    param_count: u32 = 0,

    branches: std.ArrayListUnmanaged(Branch),

    bstart: std.AutoHashMapUnmanaged(u32, *Prog),

    on_wasm_stack_skipped: i32 = 0,

    frame_size: i32 = 0,

    float_local_count: u32 = 0,
    float_local_start: u32 = 0,

    gc_ref_locals: std.ArrayListUnmanaged(u32) = .{},

    func_indices: ?*const FuncIndexMap = null,

    string_offsets: ?*const std.StringHashMap(i32) = null,

    metadata_offsets: ?*const std.StringHashMap(i32) = null,

    func_table_indices: ?*const std.StringHashMap(u32) = null,

    gc_struct_name_map: ?*const std.StringHashMapUnmanaged(u32) = null,

    gc_array_name_map: ?*const std.StringHashMapUnmanaged(u32) = null,

    type_reg: ?*const TypeRegistry = null,

    func_type_indices: ?*const std.StringHashMap(u32) = null,

    compound_len_locals: std.AutoHashMapUnmanaged(u32, u32) = .{},

    pub fn init(allocator: std.mem.Allocator, func: *const SsaFunc) GenState {
        return .{
            .allocator = allocator,
            .builder = ProgBuilder.init(allocator),
            .func = func,
            .value_to_local = .{},
            .branches = .{},
            .bstart = .{},
        };
    }

    pub fn setFuncIndices(self: *GenState, indices: *const FuncIndexMap) void {
        self.func_indices = indices;
    }

    pub fn setStringOffsets(self: *GenState, offsets: *const std.StringHashMap(i32)) void {
        self.string_offsets = offsets;
    }

    pub fn setMetadataOffsets(self: *GenState, offsets: *const std.StringHashMap(i32)) void {
        self.metadata_offsets = offsets;
    }

    pub fn setFuncTableIndices(self: *GenState, indices: *const std.StringHashMap(u32)) void {
        self.func_table_indices = indices;
    }

    pub fn setGcStructNameMap(self: *GenState, map: *const std.StringHashMapUnmanaged(u32)) void {
        self.gc_struct_name_map = map;
    }

    pub fn setGcArrayNameMap(self: *GenState, map: *const std.StringHashMapUnmanaged(u32)) void {
        self.gc_array_name_map = map;
    }

    pub fn setTypeReg(self: *GenState, reg: *const TypeRegistry) void {
        self.type_reg = reg;
    }

    pub fn setFuncTypeIndices(self: *GenState, indices: *const std.StringHashMap(u32)) void {
        self.func_type_indices = indices;
    }

    pub fn deinit(self: *GenState) void {
        self.builder.deinit();
        self.value_to_local.deinit(self.allocator);
        self.compound_len_locals.deinit(self.allocator);
        self.gc_ref_locals.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.bstart.deinit(self.allocator);
    }

    pub fn br(self: *GenState, target: *const SsaBlock) !*Prog {
        const p = try self.builder.append(.jmp);
        p.to.type = .branch;
        try self.branches.append(self.allocator, .{
            .prog = p,
            .target_block_id = target.id,
        });
        return p;
    }

    pub fn ssaGenBlock(self: *GenState, b: *const SsaBlock, next: ?*const SsaBlock) !void {
        switch (b.kind) {
            .plain, .first, .defer_ => {
                if (b.succs.len > 0) {
                    const succ = b.succs[0].b;
                    if (next == null or next.?.id != succ.id) {
                        _ = try self.br(succ);
                    }
                }
            },

            .if_ => {
                if (b.succs.len < 2) return;

                const succ0 = b.succs[0].b;
                const succ1 = b.succs[1].b;

                if (b.controls[0]) |cond| {
                    try self.getValue32(cond);
                }

                if (next != null and next.?.id == succ0.id) {
                    _ = try self.builder.append(.i32_eqz);
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ1);
                    _ = try self.builder.append(.end);
                } else if (next != null and next.?.id == succ1.id) {
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ0);
                    _ = try self.builder.append(.end);
                } else {
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ0);
                    _ = try self.builder.append(.end);
                    _ = try self.br(succ1);
                }
            },

            .ret => {
                if (b.controls[0]) |ret_val| {
                    if (ret_val.op == .string_make or ret_val.op == .slice_make or ret_val.op == .opt_make) {
                        if (ret_val.args.len >= 2) {
                            try self.getValue64(ret_val.args[0]);
                            try self.getValue64(ret_val.args[1]);
                        }
                    } else if (ret_val.type_idx == TypeRegistry.STRING and
                        self.compound_len_locals.get(ret_val.id) != null)
                    {
                        try self.getValue64(ret_val);
                        const len_local = self.compound_len_locals.get(ret_val.id).?;
                        _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(len_local));
                    } else {
                        try self.getValue64(ret_val);
                    }
                }
                _ = try self.builder.append(.aret);
            },

            .exit => {},

            else => {},
        }

        _ = try self.builder.append(.resume_point);

        if (self.on_wasm_stack_skipped != 0) {
            debug.log(.codegen, "wasm: bad stack in block b{d}", .{b.id});
        }
    }

    fn emitPhiMoves(self: *GenState, block: *const SsaBlock) !void {
        for (block.succs) |edge| {
            const succ = edge.b;
            const pred_idx = edge.i;
            for (succ.values.items) |v| {
                if (v.op != .phi) continue;
                if (v.type_idx == TypeRegistry.SSA_MEM) continue;
                if (pred_idx >= v.args.len) continue;
                const arg = v.args[pred_idx];
                try self.getValue64(arg);
                if (self.value_to_local.get(v.id)) |local_idx| {
                    const src_local = self.value_to_local.get(arg.id);
                    const src_is_f64 = if (src_local) |sl| self.isFloatLocal(sl) else isFloatType(arg.type_idx);
                    const dst_is_f64 = self.isFloatLocal(local_idx);
                    if (src_is_f64 and !dst_is_f64) {
                        _ = try self.builder.append(.i64_reinterpret_f64);
                    } else if (!src_is_f64 and dst_is_f64) {
                        _ = try self.builder.append(.f64_reinterpret_i64);
                    }
                    _ = try self.builder.appendTo(.local_set, prog_mod.constAddr(local_idx));
                }
            }
        }
    }

    pub fn getValue32(self: *GenState, v: *const SsaValue) GenError!void {
        if (!isRematerializable(v)) {
            if (self.value_to_local.get(v.id)) |local_idx| {
                _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
                _ = try self.builder.append(.i32_wrap_i64);
                return;
            }
        }

        try self.ssaGenValueOnStack(v);

        if (!isCmp(v)) {
            _ = try self.builder.append(.i32_wrap_i64);
        }
    }

    fn isFloatLocal(self: *const GenState, local_idx: u32) bool {
        return local_idx >= self.float_local_start and
            local_idx < self.float_local_start + self.float_local_count;
    }

    pub fn getValue64(self: *GenState, v: *const SsaValue) GenError!void {
        if (isRematerializable(v)) {
            try self.ssaGenValueOnStack(v);
            return;
        }

        if (self.value_to_local.get(v.id)) |local_idx| {
            _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            return;
        }

        try self.ssaGenValueOnStack(v);
        if (isCmp(v)) {
            _ = try self.builder.append(.i64_extend_i32_u);
        }
    }

    pub fn setReg(self: *GenState, v: *const SsaValue) GenError!void {
        if (self.value_to_local.get(v.id)) |local_idx| {
            _ = try self.builder.appendTo(.local_set, prog_mod.constAddr(local_idx));
        }
    }

    pub fn ssaGenValueOnStack(self: *GenState, v: *const SsaValue) GenError!void {
        switch (v.op) {
            .wasm_i64_const, .const_int, .const_64 => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(v.aux_int));
            },
            .wasm_i32_const, .const_32 => {
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(v.aux_int));
            },
            .wasm_f64_const, .const_float => {
                _ = try self.builder.appendFrom(.f64_const, prog_mod.floatAddr(@bitCast(v.aux_int)));
            },
            .const_bool => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(if (v.aux_int != 0) @as(i64, 1) else @as(i64, 0)));
            },
            .const_nil => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
            },

            .string_make, .slice_make, .opt_make => {
                debug.log(.codegen, "wasm/gen: skip compound type op {s}", .{@tagName(v.op)});
            },

            .string_ptr, .slice_ptr => {
                try self.getValue64(v.args[0]);
            },
            .string_len, .slice_len => {
                if (self.compound_len_locals.get(v.args[0].id)) |len_local| {
                    _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(len_local));
                } else {
                    debug.log(.codegen, "wasm/gen: string_len without compound local for v{d}", .{v.args[0].id});
                    try self.getValue64(v.args[0]);
                }
            },
            .opt_tag => {
                try self.getValue64(v.args[0]);
            },
            .opt_data => {
                if (self.compound_len_locals.get(v.args[0].id)) |data_local| {
                    _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(data_local));
                } else {
                    debug.log(.codegen, "wasm/gen: opt_data without compound local for v{d}", .{v.args[0].id});
                    try self.getValue64(v.args[0]);
                }
            },

            .wasm_i64_add => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_add);
            },
            .wasm_i64_sub => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_sub);
            },
            .wasm_i64_mul => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_mul);
            },
            .wasm_i64_div_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_div_s);
            },
            .wasm_i64_rem_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_rem_s);
            },

            .wasm_i64_and => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_and);
            },
            .wasm_i64_or => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_or);
            },
            .wasm_i64_xor => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_xor);
            },
            .wasm_i64_shl => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_shl);
            },
            .wasm_i64_shr_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_shr_s);
            },
            .wasm_i64_shr_u => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_shr_u);
            },

            .wasm_i64_clz => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_clz);
            },
            .wasm_i64_ctz => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_ctz);
            },
            .wasm_i64_popcnt => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_popcnt);
            },

            .wasm_i32_and => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_and);
                _ = try self.builder.append(.i64_extend_i32_u);
            },
            .wasm_i32_or => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_or);
                _ = try self.builder.append(.i64_extend_i32_u);
            },
            .wasm_i32_xor => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_xor);
                _ = try self.builder.append(.i64_extend_i32_u);
            },
            .wasm_i32_shl => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_shl);
                _ = try self.builder.append(.i64_extend_i32_u);
            },
            .wasm_i32_shr_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_shr_s);
                _ = try self.builder.append(.i64_extend_i32_s);
            },
            .wasm_i32_shr_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                _ = try self.builder.append(.i32_shr_u);
                _ = try self.builder.append(.i64_extend_i32_u);
            },

            .not => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(-1));
                _ = try self.builder.append(.i64_xor);
            },

            .wasm_i64_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_eq);
            },
            .wasm_i64_ne => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_ne);
            },
            .wasm_i64_lt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_lt_s);
            },
            .wasm_i64_le_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_le_s);
            },
            .wasm_i64_gt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_gt_s);
            },
            .wasm_i64_ge_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_ge_s);
            },
            .wasm_i64_lt_u => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_lt_u);
            },
            .wasm_i64_le_u => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_le_u);
            },
            .wasm_i64_gt_u => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_gt_u);
            },
            .wasm_i64_ge_u => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_ge_u);
            },
            .wasm_i64_eqz => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_eqz);
            },

            .wasm_f64_add => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_add);
            },
            .wasm_f64_sub => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_sub);
            },
            .wasm_f64_mul => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_mul);
            },
            .wasm_f64_div => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_div);
            },
            .wasm_f64_neg => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_neg);
            },
            .wasm_f64_abs => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_abs);
            },
            .wasm_f64_ceil => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_ceil);
            },
            .wasm_f64_floor => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_floor);
            },
            .wasm_f64_trunc => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_trunc);
            },
            .wasm_f64_nearest => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_nearest);
            },
            .wasm_f64_sqrt => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_sqrt);
            },
            .wasm_f64_min => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_min);
            },
            .wasm_f64_max => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_max);
            },

            .neg => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_sub);
            },

            .wasm_f64_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_eq);
            },
            .wasm_f64_ne => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_ne);
            },
            .wasm_f64_lt => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_lt);
            },
            .wasm_f64_le => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_le);
            },
            .wasm_f64_gt => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_gt);
            },
            .wasm_f64_ge => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.f64_ge);
            },

            .wasm_f64_reinterpret_i64 => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.f64_reinterpret_i64);
            },
            .wasm_i64_reinterpret_f64 => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_reinterpret_f64);
            },

            .wasm_i64_load => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_store => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                const p = try self.builder.append(.i64_store);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_f64_load => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.f64_load);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_f64_store => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                const p = try self.builder.append(.f64_store);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_i64_load8_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load8_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load8_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load8_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load16_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load16_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load16_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load16_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load32_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load32_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load32_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load32_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },

            .wasm_i64_store8 => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                const p = try self.builder.append(.i64_store8);
                p.to = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_store16 => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                const p = try self.builder.append(.i64_store16);
                p.to = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_store32 => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                const p = try self.builder.append(.i64_store32);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .local_addr => {
                _ = try self.builder.appendFrom(.get, prog_mod.regAddr(.sp));
                _ = try self.builder.append(.i64_extend_i32_u);
                const slot_offset: i64 = v.aux_int;
                const byte_offset: i64 = slot_offset * 8;
                if (byte_offset != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(byte_offset));
                    _ = try self.builder.append(.i64_add);
                }
            },

            .global_addr => {
                const GLOBAL_BASE: i64 = 0x20000;
                const global_idx: i64 = v.aux_int;
                const addr: i64 = GLOBAL_BASE + (global_idx * 8);
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(addr));
            },

            .metadata_addr => {
                const type_name = v.aux.string;
                if (self.metadata_offsets) |offsets| {
                    if (offsets.get(type_name)) |offset| {
                        _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    } else {
                        _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                    }
                } else {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                }
            },

            .addr => {
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (fn_name) |name| {
                    if (self.func_table_indices) |indices| {
                        if (indices.get(name)) |table_idx| {
                            debug.log(.codegen, "wasm/gen: func_addr '{s}' -> table_idx={d}", .{ name, table_idx });
                            _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(@intCast(table_idx)));
                        } else {
                            debug.log(.codegen, "wasm/gen: addr op: function '{s}' not in func_table_indices", .{name});
                            _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                        }
                    } else {
                        debug.log(.codegen, "wasm/gen: addr op: no func_table_indices available", .{});
                        _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                    }
                } else {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                }
            },

            .off_ptr => {
                try self.getValue64(v.args[0]);
                const offset = v.aux_int;
                if (offset != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    _ = try self.builder.append(.i64_add);
                }
            },

            .add_ptr => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_add);
            },

            .sub_ptr => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_sub);
            },

            .arg => {
                const local_idx = self.value_to_local.get(v.id) orelse @as(u32, @intCast(v.aux_int));
                _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            },

            .copy => {
                try self.getValue64(v.args[0]);
                const src_local = self.value_to_local.get(v.args[0].id);
                const dst_local = self.value_to_local.get(v.id);
                const src_is_f64 = if (src_local) |sl| self.isFloatLocal(sl) else false;
                const dst_is_f64 = if (dst_local) |dl| self.isFloatLocal(dl) else false;
                if (src_is_f64 and !dst_is_f64) {
                    _ = try self.builder.append(.i64_reinterpret_f64);
                } else if (!src_is_f64 and dst_is_f64) {
                    _ = try self.builder.append(.f64_reinterpret_i64);
                }
            },

            .phi => {
                if (self.value_to_local.get(v.id)) |local_idx| {
                    _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
                }
            },

            .cond_select => {
                try self.getValue64(v.args[1]);
                try self.getValue64(v.args[2]);
                try self.getValue32(v.args[0]);
                _ = try self.builder.append(.select);
            },

            .wasm_call => {
                const wc_end = if (v.memoryArg() != null and v.args.len > 0) v.args.len - 1 else v.args.len;
                for (v.args[0..wc_end]) |arg| {
                    try self.getValue64(arg);
                }
                if (v.aux_int == 0) {
                    const func_name_str = if (v.aux.string.len > 0) v.aux.string else "<no_name>";
                    debug.log(.codegen, "WARNING: call to func_idx=0 (fd_write fallback) — likely unresolved '{s}' (args={d})", .{ func_name_str, v.args.len });
                }
                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_return_call => {
                const rc_end = if (v.memoryArg() != null and v.args.len > 0) v.args.len - 1 else v.args.len;
                for (v.args[0..rc_end]) |arg| {
                    try self.getValue64(arg);
                }
                const rc_fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                const rc_func_idx: i64 = if (rc_fn_name) |name_str| blk: {
                    if (self.func_indices) |indices| {
                        if (indices.get(name_str)) |idx| {
                            break :blk @intCast(idx);
                        }
                    }
                    break :blk v.aux_int;
                } else v.aux_int;

                const rc_p = try self.builder.append(.return_call);
                rc_p.to = prog_mod.constAddr(rc_func_idx);
            },

            .wasm_lowered_static_call => {
                const sc_end = if (v.memoryArg() != null and v.args.len > 0) v.args.len - 1 else v.args.len;
                for (v.args[0..sc_end]) |arg| {
                    try self.getValue64(arg);
                }
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                const func_idx: i64 = if (fn_name) |name| blk: {
                    if (self.func_indices) |indices| {
                        if (indices.get(name)) |idx| {
                            break :blk @intCast(idx);
                        }
                    }
                    debug.log(.codegen, "UNRESOLVED: static call '{s}' not in func_indices — emitting unreachable", .{name});
                    _ = try self.builder.append(.@"unreachable");
                    break :blk v.aux_int;
                } else v.aux_int;

                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(func_idx);
            },

            .wasm_lowered_closure_call => {
                const args = v.args;
                try self.getValue64(args[1]);
                {
                    const p = try self.builder.append(.global_set);
                    p.to = prog_mod.constAddr(1);
                }
                const cc_end = if (v.memoryArg() != null and args.len > 0) args.len - 1 else args.len;
                for (args[2..cc_end]) |arg| {
                    try self.getValue64(arg);
                }
                try self.getValue64(args[0]);
                _ = try self.builder.append(.i32_wrap_i64);

                const p = try self.builder.append(.call_indirect);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_lowered_inter_call => {
                const args = v.args;
                const ic_end = if (v.memoryArg() != null and args.len > 0) args.len - 1 else args.len;
                for (args[1..ic_end]) |arg| {
                    try self.getValue64(arg);
                }
                try self.getValue64(args[0]);
                _ = try self.builder.append(.i32_wrap_i64);

                const p = try self.builder.append(.call_indirect);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_global_get => {
                const p = try self.builder.append(.global_get);
                p.from = prog_mod.constAddr(v.aux_int);
            },

            .wasm_global_set => {
                try self.getValue64(v.args[0]);
                const p = try self.builder.append(.global_set);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_gc_struct_new => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const p = try self.builder.append(.gc_struct_new);
                p.from = prog_mod.constAddr(gc_type_idx);
            },

            .wasm_gc_struct_get => {
                try self.getValue64(v.args[0]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const field_idx = v.aux_int;
                const p = try self.builder.append(.gc_struct_get);
                p.from = prog_mod.constAddr(gc_type_idx);
                p.to = prog_mod.constAddr(field_idx);
            },

            .wasm_gc_struct_set => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const field_idx = v.aux_int;
                const p = try self.builder.append(.gc_struct_set);
                p.from = prog_mod.constAddr(gc_type_idx);
                p.to = prog_mod.constAddr(field_idx);
            },

            .wasm_gc_array_new => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const p = try self.builder.append(.gc_array_new);
                p.from = prog_mod.constAddr(gc_type_idx);
            },

            .wasm_gc_array_new_default => {
                try self.getValue64(v.args[0]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const p = try self.builder.append(.gc_array_new_default);
                p.from = prog_mod.constAddr(gc_type_idx);
            },

            .wasm_gc_array_new_data => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const data_idx = v.aux_int;
                const p = try self.builder.append(.gc_array_new_data);
                p.from = prog_mod.constAddr(gc_type_idx);
                p.to = prog_mod.constAddr(data_idx);
            },

            .wasm_gc_array_new_fixed => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const count = v.aux_int;
                const p = try self.builder.append(.gc_array_new_fixed);
                p.from = prog_mod.constAddr(gc_type_idx);
                p.to = prog_mod.constAddr(count);
            },

            .wasm_gc_array_get => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const p = try self.builder.append(.gc_array_get);
                p.from = prog_mod.constAddr(gc_type_idx);
            },

            .wasm_gc_array_set => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.getValue64(v.args[2]);
                const type_name = v.aux.string;
                const gc_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(type_name) orelse 0) else 0;
                const p = try self.builder.append(.gc_array_set);
                p.from = prog_mod.constAddr(gc_type_idx);
            },

            .wasm_gc_array_len => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.gc_array_len);
            },

            .wasm_gc_array_copy => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                const dst_type_name = v.aux.string;
                const dst_type_idx: i64 = if (self.gc_array_name_map) |m| @intCast(m.get(dst_type_name) orelse 0) else 0;
                const src_type_idx = v.aux_int;
                const p = try self.builder.append(.gc_array_copy);
                p.from = prog_mod.constAddr(dst_type_idx);
                p.to = prog_mod.constAddr(src_type_idx);
            },

            .wasm_gc_ref_test => {
                try self.getValue64(v.args[0]);
                const type_name = v.aux.string;
                const heap_type: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else v.aux_int;
                const p = try self.builder.append(.ref_test);
                p.from = prog_mod.constAddr(heap_type);
            },

            .wasm_gc_ref_cast => {
                try self.getValue64(v.args[0]);
                const type_name = v.aux.string;
                const heap_type: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else v.aux_int;
                const p = try self.builder.append(.ref_cast);
                p.from = prog_mod.constAddr(heap_type);
            },

            .wasm_gc_ref_null => {
                const type_name = v.aux.string;
                const heap_type: i64 = if (self.gc_struct_name_map) |m| @intCast(m.get(type_name) orelse 0) else v.aux_int;
                const p = try self.builder.append(.ref_null);
                p.from = prog_mod.constAddr(heap_type);
            },

            .wasm_gc_ref_is_null => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.ref_is_null);
            },

            .wasm_gc_ref_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.ref_eq);
            },

            .wasm_gc_ref_func => {
                const func_name = v.aux.string;
                const func_idx: i64 = if (self.func_indices) |fi| @intCast(fi.get(func_name) orelse 0) else 0;
                const p = try self.builder.append(.ref_func);
                p.from = prog_mod.constAddr(func_idx);
            },

            .wasm_gc_call_ref => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                const type_idx = v.aux_int;
                const p = try self.builder.append(.call_ref);
                p.from = prog_mod.constAddr(type_idx);
            },

            .wasm_gc_return_call_ref => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                const type_idx = v.aux_int;
                const p = try self.builder.append(.return_call_ref);
                p.from = prog_mod.constAddr(type_idx);
            },

            .convert => {
                const from_type = v.aux.type_ref;
                const to_type = v.type_idx;

                try self.getValue64(v.args[0]);

                const from_is_float = (from_type == TypeRegistry.F32 or from_type == TypeRegistry.F64 or from_type == TypeRegistry.UNTYPED_FLOAT);
                const to_is_float = (to_type == TypeRegistry.F32 or to_type == TypeRegistry.F64 or to_type == TypeRegistry.UNTYPED_FLOAT);
                const from_is_32 = (from_type == TypeRegistry.I32);
                const to_is_32 = (to_type == TypeRegistry.I32);

                if (from_is_float and to_is_float) {
                    const to_is_f32 = (to_type == TypeRegistry.F32);
                    if (to_is_f32) {
                        _ = try self.builder.append(.f32_demote_f64);
                        _ = try self.builder.append(.f64_promote_f32);
                    }
                } else if (from_is_float and !to_is_float) {
                    if (to_is_32) {
                        _ = try self.builder.append(.i32_trunc_sat_f64_s);
                        _ = try self.builder.append(.i64_extend_i32_s);
                    } else {
                        _ = try self.builder.append(.i64_trunc_sat_f64_s);
                    }
                } else if (!from_is_float and to_is_float) {
                    _ = try self.builder.append(.f64_convert_i64_s);
                } else if (!from_is_32 and to_is_32) {
                    _ = try self.builder.append(.i32_wrap_i64);
                    _ = try self.builder.append(.i64_extend_i32_s);
                } else if (from_is_32 and !to_is_32) {
                    // Already extended by getValue64
                }
            },

            .wasm_lowered_move => {
                const size: i32 = @intCast(v.aux_int);
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i32_wrap_i64);
                const size_op = try self.builder.append(.i32_const);
                size_op.from = prog_mod.constAddr(size);
                _ = try self.builder.append(.memory_copy);
            },

            .wasm_lowered_zero => {
                const size: i32 = @intCast(v.aux_int);
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const zero_op = try self.builder.append(.i32_const);
                zero_op.from = prog_mod.constAddr(0);
                const size_op = try self.builder.append(.i32_const);
                size_op.from = prog_mod.constAddr(size);
                _ = try self.builder.append(.memory_fill);
            },

            .wasm_unreachable => {
                _ = try self.builder.append(.@"unreachable");
            },

            else => {
                debug.log(.codegen, "wasm/gen: unhandled op {s}", .{@tagName(v.op)});
            },
        }
    }

    pub fn generate(self: *GenState) !void {
        const blocks = self.func.blocks.items;

        debug.log(.codegen, "wasm/gen: generating '{s}' ({d} blocks)", .{
            self.func.name,
            blocks.len,
        });

        try self.allocateLocals();

        self.frame_size = self.computeFrameSize();

        const text = try self.builder.append(.text);
        text.from = prog_mod.constAddr(self.frame_size);

        debug.log(.codegen, "wasm/gen: frame_size={d}, {d} locals allocated", .{
            self.frame_size, self.next_local,
        });

        for (blocks, 0..) |block, i| {
            const next: ?*const SsaBlock = if (i + 1 < blocks.len) blocks[i + 1] else null;
            debug.log(.codegen, "wasm/gen: block b{d} ({s}, {d} values, {d} succs)", .{
                block.id, @tagName(block.kind), block.values.items.len, block.succs.len,
            });

            const first_prog = self.builder.last;

            for (block.values.items) |v| {
                try self.ssaGenValue(v);
            }

            if (self.builder.last != first_prog) {
                if (self.builder.last) |last| {
                    try self.bstart.put(self.allocator, block.id, last);
                }
            } else if (self.builder.last) |last| {
                try self.bstart.put(self.allocator, block.id, last);
            }

            try self.emitPhiMoves(block);

            try self.ssaGenBlock(block, next);
        }

        for (self.branches.items) |branch| {
            if (self.bstart.get(branch.target_block_id)) |target_prog| {
                branch.prog.to.branch_target = target_prog;
            }
        }

        debug.log(.codegen, "wasm/gen: generated {d} instructions, {d} branches", .{
            self.builder.count,
            self.branches.items.len,
        });
    }

    pub fn ssaGenValue(self: *GenState, v: *const SsaValue) !void {
        if (v.uses == 0 and !v.hasSideEffects()) return;

        if (v.op == .slice_make or v.op == .string_make or v.op == .opt_make) return;

        if (isRematerializable(v)) return;

        if (isCmp(v)) return;

        if (v.op == .phi) return;

        if (v.type_idx == TypeRegistry.SSA_MEM) return;
        if (v.op == .init_mem) return;

        try self.ssaGenValueOnStack(v);

        if (self.value_to_local.get(v.id)) |li| {
            debug.log(.codegen, "  ssaGenValue: v{d} op={s} type={d} -> local.set {d} (float_local={s})", .{
                v.id, @tagName(v.op), v.type_idx, li,
                if (self.isFloatLocal(li)) "yes" else "no",
            });
        }

        if (v.op == .wasm_return_call) return;

        if (v.op == .wasm_i64_store or v.op == .wasm_i64_store8 or
            v.op == .wasm_i64_store16 or v.op == .wasm_i64_store32 or
            v.op == .wasm_f64_store or v.op == .wasm_gc_struct_set or
            v.op == .wasm_gc_array_set or v.op == .wasm_gc_array_copy) return;

        const is_call = v.op == .wasm_call or v.op == .wasm_lowered_static_call;
        const is_compound_ret = is_call and (v.type_idx == TypeRegistry.STRING);
        if (is_compound_ret) {
            const len_local = self.compound_len_locals.get(v.id) orelse blk: {
                const l = self.next_local;
                self.next_local += 1;
                try self.compound_len_locals.put(self.allocator, v.id, l);
                break :blk l;
            };
            _ = try self.builder.appendTo(.local_set, prog_mod.constAddr(len_local));
            if (v.uses > 0) {
                try self.setReg(v);
            }
        } else if (v.uses > 0 and v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM) {
            try self.setReg(v);
        } else if (v.uses > 0 and v.type_idx == TypeRegistry.VOID and v.op.producesAddressValue()) {
            try self.setReg(v);
        } else if (v.op.info().call and v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM) {
            _ = try self.builder.append(.drop);
        } else if (v.op == .wasm_gc_struct_new or v.op == .wasm_gc_array_new or
            v.op == .wasm_gc_array_new_default or v.op == .wasm_gc_array_new_fixed or
            v.op == .wasm_gc_array_new_data)
        {
            _ = try self.builder.append(.drop);
        }
    }

    fn allocateLocals(self: *GenState) !void {
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) {
                    const arg_idx: u32 = @intCast(v.aux_int);
                    try self.value_to_local.put(self.allocator, v.id, arg_idx);
                    if (arg_idx >= self.param_count) {
                        self.param_count = arg_idx + 1;
                    }
                    if (v.type_idx == TypeRegistry.STRING) {
                        try self.compound_len_locals.put(self.allocator, v.id, arg_idx + 1);
                        if (arg_idx + 1 >= self.param_count) {
                            self.param_count = arg_idx + 2;
                        }
                    }
                }
            }
        }
        self.next_local = self.param_count + 1;

        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) continue;
                if (v.uses == 0 and !v.hasSideEffects()) continue;
                if (isRematerializable(v)) continue;
                if (v.type_idx == TypeRegistry.SSA_MEM or v.op == .init_mem) continue;
                if (isCmp(v)) continue;
                if (v.op == .slice_make or v.op == .string_make or v.op == .opt_make or v.op == .wasm_lowered_move or v.op == .wasm_lowered_zero or v.op == .wasm_return_call) continue;
                if (v.op == .wasm_i64_store or v.op == .wasm_i64_store8 or
                    v.op == .wasm_i64_store16 or v.op == .wasm_i64_store32 or
                    v.op == .wasm_f64_store or v.op == .wasm_gc_struct_set or
                    v.op == .wasm_gc_array_set or v.op == .wasm_gc_array_copy) continue;
                if (isFloatType(v.type_idx)) continue;
                if (v.op == .wasm_gc_struct_new or v.op == .wasm_gc_array_new or
                    v.op == .wasm_gc_array_new_default or v.op == .wasm_gc_array_new_fixed or
                    v.op == .wasm_gc_array_new_data) continue;
                if (isGcRefType(v.type_idx, self.type_reg, self.gc_struct_name_map)) continue;

                const local_idx = self.next_local;
                self.next_local += 1;
                try self.value_to_local.put(self.allocator, v.id, local_idx);

                const is_call_p1 = v.op == .wasm_call or v.op == .wasm_lowered_static_call;
                if (is_call_p1 and v.type_idx == TypeRegistry.STRING) {
                    const len_local = self.next_local;
                    self.next_local += 1;
                    try self.compound_len_locals.put(self.allocator, v.id, len_local);
                }
            }
        }

        self.float_local_start = self.next_local;
        var float_count: u32 = 0;
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) continue;
                if (v.uses == 0 and !v.hasSideEffects()) continue;
                if (isRematerializable(v)) continue;
                if (isCmp(v)) continue;
                if (!isFloatType(v.type_idx)) continue;

                const local_idx = self.next_local;
                self.next_local += 1;
                float_count += 1;
                try self.value_to_local.put(self.allocator, v.id, local_idx);
            }
        }
        self.float_local_count = float_count;

        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) continue;
                if (v.uses == 0) continue;

                const is_gc_struct_new = v.op == .wasm_gc_struct_new;
                const is_gc_array_alloc = v.op == .wasm_gc_array_new or
                    v.op == .wasm_gc_array_new_default or v.op == .wasm_gc_array_new_fixed or
                    v.op == .wasm_gc_array_new_data;
                const is_gc_ref = isGcRefType(v.type_idx, self.type_reg, self.gc_struct_name_map);
                if (!is_gc_struct_new and !is_gc_array_alloc and !is_gc_ref) continue;

                if (self.value_to_local.contains(v.id)) continue;

                const local_idx = self.next_local;
                self.next_local += 1;
                const gc_type_idx: u32 = blk: {
                    if (is_gc_struct_new) {
                        const type_name = v.aux.string;
                        break :blk if (self.gc_struct_name_map) |m| m.get(type_name) orelse 0 else 0;
                    }
                    if (is_gc_array_alloc) {
                        const type_name = v.aux.string;
                        break :blk if (self.gc_array_name_map) |m| m.get(type_name) orelse 0 else 0;
                    }
                    if (self.type_reg) |reg| {
                        const t = reg.get(v.type_idx);
                        const name = switch (t) {
                            .struct_type => |st| st.name,
                            .pointer => |ptr| switch (reg.get(ptr.elem)) {
                                .struct_type => |st| st.name,
                                else => null,
                            },
                            else => null,
                        };
                        if (name) |n| {
                            break :blk if (self.gc_struct_name_map) |m| m.get(n) orelse 0 else 0;
                        }
                    }
                    break :blk 0;
                };
                try self.gc_ref_locals.append(self.allocator, gc_type_idx);
                try self.value_to_local.put(self.allocator, v.id, local_idx);
            }
        }
    }

    fn getLocalOffset(self: *const GenState, local_idx: usize) i64 {
        if (self.func.local_sizes.len == 0) {
            return @intCast(local_idx * 8);
        }
        var offset: i64 = 0;
        const count = @min(local_idx, self.func.local_sizes.len);
        for (0..count) |i| {
            const size: i64 = @intCast(self.func.local_sizes[i]);
            offset += if (size < 8) 8 else size;
        }
        return offset;
    }

    fn computeFrameSize(self: *const GenState) i32 {
        if (self.func.local_sizes.len > 0) {
            var total: i32 = 0;
            for (self.func.local_sizes) |size| {
                total += if (size < 8) 8 else @as(i32, @intCast(size));
            }
            return @divTrunc((total + 15), 16) * 16;
        }

        var max_slot: i32 = -1;
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .local_addr) {
                    const slot: i32 = @intCast(v.aux_int);
                    if (slot > max_slot) max_slot = slot;
                }
            }
        }
        if (max_slot < 0) return 0;
        const size = (max_slot + 1) * 8;
        return @divTrunc((size + 15), 16) * 16;
    }
};

fn isRematerializable(v: *const SsaValue) bool {
    return switch (v.op) {
        .wasm_i64_const, .wasm_i32_const, .wasm_f64_const,
        .const_int, .const_32, .const_64, .const_float, .const_bool,
        .local_addr, .global_addr, .metadata_addr,
        => true,
        else => false,
    };
}

fn isCmp(v: *const SsaValue) bool {
    return switch (v.op) {
        .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s,
        .wasm_i64_gt_s, .wasm_i64_ge_s,
        .wasm_i64_lt_u, .wasm_i64_le_u, .wasm_i64_gt_u, .wasm_i64_ge_u,
        .wasm_i64_eqz,
        .wasm_f64_eq, .wasm_f64_ne, .wasm_f64_lt, .wasm_f64_le,
        .wasm_f64_gt, .wasm_f64_ge,
        => true,
        else => false,
    };
}

fn isFloatType(type_idx: foundation.types.TypeIndex) bool {
    return type_idx == TypeRegistry.F64 or type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.UNTYPED_FLOAT;
}

fn isGcRefType(type_idx: foundation.types.TypeIndex, type_reg: ?*const TypeRegistry, gc_map: ?*const std.StringHashMapUnmanaged(u32)) bool {
    const reg = type_reg orelse return false;
    const map = gc_map orelse return false;
    const t = reg.get(type_idx);
    return switch (t) {
        .struct_type => |st| map.contains(st.name),
        .pointer => |ptr| blk: {
            if (!ptr.flags.is_managed) break :blk false;
            const elem = reg.get(ptr.elem);
            break :blk switch (elem) {
                .struct_type => |st| map.contains(st.name),
                else => false,
            };
        },
        else => false,
    };
}

const testing = std.testing;

test "GenState init/deinit" {
    const allocator = testing.allocator;

    var func = SsaFunc.init(allocator, "test");
    defer func.deinit();

    var state = GenState.init(allocator, &func);
    defer state.deinit();

    try testing.expectEqual(@as(u32, 0), state.next_local);
}
