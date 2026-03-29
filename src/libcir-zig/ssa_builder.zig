//! IR to SSA Conversion — Go's FwdRef pattern for deferred phi insertion.
//!
//! Converts the block-structured IR (from the checker/lowerer) into SSA form.
//! Uses forward references (FwdRef) at block boundaries that are later resolved
//! into Phi nodes by the insertPhis pass. Memory state is threaded through as
//! a special MEM_VAR variable, following Go's memory threading pattern.

const std = @import("std");
const ir = @import("ir.zig");
const foundation = @import("foundation");
const types = foundation.types;
const source = foundation.source;
const target_mod = foundation.target;
const debug = foundation.debug;
const ssa = @import("ssa/func.zig");
const ssa_block = @import("ssa/block.zig");
const ssa_value = @import("ssa/value.zig");
const ssa_op = @import("ssa/op.zig");

const Allocator = std.mem.Allocator;
const Target = target_mod.Target;
const TypeRegistry = types.TypeRegistry;
const TypeIndex = types.TypeIndex;

pub const Func = ssa.Func;
pub const Block = ssa_block.Block;
pub const BlockKind = ssa_block.BlockKind;
pub const Value = ssa_value.Value;
pub const Op = ssa_op.Op;
pub const Pos = ssa_value.Pos;

/// Errors from the SSA conversion pass.
pub const ConvertError = error{
    MissingValue,
    NoCurrentBlock,
    OutOfMemory,
    NeedAllocator,
};

/// Builds SSA form from the block-structured IR for a single function.
pub const SSABuilder = struct {
    allocator: Allocator,
    func: *Func,
    ir_func: *const ir.Func,
    ir_globals: []const ir.Global,
    type_registry: *TypeRegistry,
    target: Target,
    vars: std.AutoHashMap(ir.LocalIdx, *Value),
    fwd_vars: std.AutoHashMap(ir.LocalIdx, *Value),
    defvars: std.AutoHashMap(u32, std.AutoHashMap(ir.LocalIdx, *Value)),
    cur_block: ?*Block,
    block_map: std.AutoHashMap(ir.BlockIndex, *Block),
    node_values: std.AutoHashMap(ir.NodeIndex, *Value),
    loop_stack: std.ArrayListUnmanaged(LoopContext),
    cur_pos: Pos,
    local_slot_offsets: []u32,

    const LoopContext = struct { continue_block: *Block, break_block: *Block };

    /// Sentinel local index for memory state, avoiding collision with real IR locals.
    pub const MEM_VAR: ir.LocalIdx = std.math.maxInt(ir.LocalIdx);

    /// Get the current memory state for this block.
    fn mem(self: *SSABuilder) !*Value {
        return self.variable(MEM_VAR, TypeRegistry.SSA_MEM);
    }

    /// Set the current memory state.
    fn setMem(self: *SSABuilder, v: *Value) void {
        if (v.type_idx != TypeRegistry.SSA_MEM) {
            const cur = self.cur_block orelse {
                self.assign(MEM_VAR, v);
                return;
            };
            const wrapper = self.func.newValue(.copy, TypeRegistry.SSA_MEM, cur, self.cur_pos) catch {
                self.assign(MEM_VAR, v);
                return;
            };
            wrapper.addArg(v);
            cur.addValue(self.allocator, wrapper) catch {
                self.assign(MEM_VAR, v);
                return;
            };
            self.assign(MEM_VAR, wrapper);
        } else {
            self.assign(MEM_VAR, v);
        }
    }

    pub fn init(allocator: Allocator, ir_func: *const ir.Func, ir_globals: []const ir.Global, type_registry: *TypeRegistry, target: Target) !SSABuilder {
        const func = try allocator.create(Func);
        func.* = Func.init(allocator, ir_func.name);
        func.is_export = ir_func.is_export;
        const entry = try func.newBlock(.plain);
        func.entry = entry;

        const num_locals = ir_func.locals.len;
        const slot_offsets = try allocator.alloc(u32, num_locals);
        {
            var has_overlap = false;
            for (ir_func.locals) |local| {
                if (local.overlap_group > 0) { has_overlap = true; break; }
            }

            if (!has_overlap) {
                var next_slot: u32 = 0;
                for (ir_func.locals, 0..) |local, idx| {
                    slot_offsets[idx] = next_slot;
                    const num_slots = @max(1, (local.size + 7) / 8);
                    next_slot += num_slots;
                }
            } else {
                var arm_sizes = std.AutoHashMapUnmanaged(u32, u32){};
                defer arm_sizes.deinit(allocator);
                for (ir_func.locals) |local| {
                    if (local.overlap_group > 0) {
                        const key = (@as(u32, local.overlap_group) << 16) | @as(u32, local.overlap_arm);
                        const num_slots = @max(1, (local.size + 7) / 8);
                        const arm_entry = try arm_sizes.getOrPut(allocator, key);
                        if (arm_entry.found_existing) {
                            arm_entry.value_ptr.* += num_slots;
                        } else {
                            arm_entry.value_ptr.* = num_slots;
                        }
                    }
                }

                var group_max = std.AutoHashMapUnmanaged(u16, u32){};
                defer group_max.deinit(allocator);
                var arm_it = arm_sizes.iterator();
                while (arm_it.next()) |arm_size_entry| {
                    const group: u16 = @intCast(arm_size_entry.key_ptr.* >> 16);
                    const arm_total = arm_size_entry.value_ptr.*;
                    const gentry = try group_max.getOrPut(allocator, group);
                    if (gentry.found_existing) {
                        gentry.value_ptr.* = @max(gentry.value_ptr.*, arm_total);
                    } else {
                        gentry.value_ptr.* = arm_total;
                    }
                }

                var group_base = std.AutoHashMapUnmanaged(u16, u32){};
                defer group_base.deinit(allocator);
                var arm_offsets = std.AutoHashMapUnmanaged(u32, u32){};
                defer arm_offsets.deinit(allocator);
                var next_slot: u32 = 0;
                for (ir_func.locals, 0..) |local, idx| {
                    if (local.overlap_group == 0) {
                        slot_offsets[idx] = next_slot;
                        const num_slots = @max(1, (local.size + 7) / 8);
                        next_slot += num_slots;
                    } else {
                        const gentry = try group_base.getOrPut(allocator, local.overlap_group);
                        if (!gentry.found_existing) {
                            gentry.value_ptr.* = next_slot;
                            next_slot += group_max.get(local.overlap_group) orelse 0;
                        }
                        const base = gentry.value_ptr.*;
                        const key = (@as(u32, local.overlap_group) << 16) | @as(u32, local.overlap_arm);
                        const aentry = try arm_offsets.getOrPut(allocator, key);
                        if (!aentry.found_existing) aentry.value_ptr.* = 0;
                        slot_offsets[idx] = base + aentry.value_ptr.*;
                        const num_slots = @max(1, (local.size + 7) / 8);
                        aentry.value_ptr.* += num_slots;
                    }
                }
            }
        }

        var vars = std.AutoHashMap(ir.LocalIdx, *Value).init(allocator);

        const init_mem = try func.newValue(.init_mem, TypeRegistry.SSA_MEM, entry, .{});
        try entry.addValue(allocator, init_mem);
        try vars.put(MEM_VAR, init_mem);

        var phys_reg_idx: i32 = 0;
        const is_wasm_gc = target.isWasmGC();
        for (ir_func.params, 0..) |param, i| {
            const local_type = type_registry.get(param.type_idx);
            const is_string_or_slice = param.type_idx == TypeRegistry.STRING or local_type == .slice;
            const type_size = type_registry.sizeOf(param.type_idx);
            const is_opt_ptr = local_type == .optional and blk: {
                const ei = type_registry.get(local_type.optional.elem);
                break :blk ei == .pointer and ei.pointer.flags.is_managed;
            };
            const is_compound_opt = local_type == .optional and type_registry.get(local_type.optional.elem) != .pointer and !is_opt_ptr;
            const is_large_struct = (!is_wasm_gc and (local_type == .struct_type or local_type == .union_type or local_type == .tuple) and type_size > 8) or (is_compound_opt and type_size > 8);

            if (is_opt_ptr) {
                const tag_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                tag_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, tag_val);
                phys_reg_idx += 1;

                const data_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                data_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, data_val);
                phys_reg_idx += 1;

                const opt_val = try func.newValue(.opt_make, param.type_idx, entry, .{});
                opt_val.addArg(tag_val);
                opt_val.addArg(data_val);
                try entry.addValue(allocator, opt_val);
                try vars.put(@intCast(i), opt_val);

                const addr_v = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                addr_v.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, addr_v);
                const tag_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                tag_store.addArg3(addr_v, tag_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, tag_store);
                try vars.put(MEM_VAR, tag_store);
                const data_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                data_addr.aux_int = 8;
                data_addr.addArg(addr_v);
                try entry.addValue(allocator, data_addr);
                const data_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                data_store.addArg3(data_addr, data_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, data_store);
                try vars.put(MEM_VAR, data_store);
            } else if (is_string_or_slice) {
                const ptr_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                ptr_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, ptr_val);
                phys_reg_idx += 1;

                const len_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                len_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, len_val);
                phys_reg_idx += 1;

                const slice_val = try func.newValue(.slice_make, param.type_idx, entry, .{});
                slice_val.addArg(ptr_val);
                slice_val.addArg(len_val);
                try slice_val.addArgAlloc(len_val, allocator);
                try entry.addValue(allocator, slice_val);
                try vars.put(@intCast(i), slice_val);

                const addr_v = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                addr_v.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, addr_v);
                const ptr_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                ptr_store.addArg3(addr_v, ptr_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, ptr_store);
                try vars.put(MEM_VAR, ptr_store);
                const len_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                len_addr.aux_int = 8;
                len_addr.addArg(addr_v);
                try entry.addValue(allocator, len_addr);
                const len_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                len_store.addArg3(len_addr, len_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, len_store);
                try vars.put(MEM_VAR, len_store);
            } else if (is_large_struct) {
                const num_slots_ls: u32 = @intCast((type_size + 7) / 8);
                const base_addr = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                base_addr.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, base_addr);

                for (0..num_slots_ls) |slot| {
                    const chunk_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                    chunk_val.aux_int = phys_reg_idx;
                    try entry.addValue(allocator, chunk_val);
                    phys_reg_idx += 1;

                    if (slot == 0) {
                        const store_v = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                        store_v.addArg3(base_addr, chunk_val, vars.get(MEM_VAR).?);
                        try entry.addValue(allocator, store_v);
                        try vars.put(MEM_VAR, store_v);
                    } else {
                        const off_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                        off_addr.aux_int = @intCast(slot * 8);
                        off_addr.addArg(base_addr);
                        try entry.addValue(allocator, off_addr);
                        const store_v = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                        store_v.addArg3(off_addr, chunk_val, vars.get(MEM_VAR).?);
                        try entry.addValue(allocator, store_v);
                        try vars.put(MEM_VAR, store_v);
                    }
                }
            } else {
                const arg_val = try func.newValue(.arg, param.type_idx, entry, .{});
                arg_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, arg_val);
                phys_reg_idx += 1;
                try vars.put(@intCast(i), arg_val);

                const is_gc_ref_param = is_wasm_gc and (local_type == .struct_type or
                    (local_type == .pointer and local_type.pointer.flags.is_managed and type_registry.get(local_type.pointer.elem) == .struct_type));
                if (!is_gc_ref_param) {
                    const addr_v = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                    addr_v.aux_int = @intCast(slot_offsets[i]);
                    try entry.addValue(allocator, addr_v);
                    const store_v = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                    store_v.addArg3(addr_v, arg_val, vars.get(MEM_VAR).?);
                    try entry.addValue(allocator, store_v);
                    try vars.put(MEM_VAR, store_v);
                }
            }
        }

        return .{
            .allocator = allocator,
            .func = func,
            .ir_func = ir_func,
            .ir_globals = ir_globals,
            .type_registry = type_registry,
            .target = target,
            .vars = vars,
            .fwd_vars = std.AutoHashMap(ir.LocalIdx, *Value).init(allocator),
            .defvars = std.AutoHashMap(u32, std.AutoHashMap(ir.LocalIdx, *Value)).init(allocator),
            .cur_block = entry,
            .block_map = std.AutoHashMap(ir.BlockIndex, *Block).init(allocator),
            .node_values = std.AutoHashMap(ir.NodeIndex, *Value).init(allocator),
            .loop_stack = .{},
            .cur_pos = .{},
            .local_slot_offsets = slot_offsets,
        };
    }

    pub fn deinit(self: *SSABuilder) void {
        self.vars.deinit();
        self.fwd_vars.deinit();
        var it = self.defvars.valueIterator();
        while (it.next()) |v| v.deinit();
        self.defvars.deinit();
        self.block_map.deinit();
        self.node_values.deinit();
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.local_slot_offsets);
    }

    pub fn takeFunc(self: *SSABuilder) *Func {
        const f = self.func;
        self.func = undefined;
        return f;
    }

    pub fn startBlock(self: *SSABuilder, block: *Block) void {
        self.saveDefvars();
        self.cur_block = block;
        self.vars.clearRetainingCapacity();
        self.fwd_vars.clearRetainingCapacity();
    }

    pub fn endBlock(self: *SSABuilder) ?*Block {
        self.saveDefvars();
        const b = self.cur_block;
        self.cur_block = null;
        return b;
    }

    fn saveDefvars(self: *SSABuilder) void {
        const block = self.cur_block orelse return;
        const gop = self.defvars.getOrPut(block.id) catch return;
        if (!gop.found_existing) gop.value_ptr.* = std.AutoHashMap(ir.LocalIdx, *Value).init(self.allocator);
        var it_inner = self.vars.iterator();
        while (it_inner.next()) |entry_item| gop.value_ptr.put(entry_item.key_ptr.*, entry_item.value_ptr.*) catch {};
    }

    pub fn assign(self: *SSABuilder, local_idx: ir.LocalIdx, value: *Value) void {
        self.vars.put(local_idx, value) catch {};
    }

    fn emitMemStore(self: *SSABuilder, store_op: Op, addr_v: *Value, value: *Value, cur: *Block) !*Value {
        const store_v = try self.func.newValue(store_op, TypeRegistry.SSA_MEM, cur, self.cur_pos);
        store_v.addArg2(addr_v, value);
        const cur_mem = try self.mem();
        try store_v.addArgAlloc(cur_mem, self.allocator);
        try cur.addValue(self.allocator, store_v);
        self.setMem(store_v);
        return store_v;
    }

    fn emitMemLoad(self: *SSABuilder, load_op: Op, addr_v: *Value, type_idx: TypeIndex, cur: *Block) !*Value {
        const load_v = try self.func.newValue(load_op, type_idx, cur, self.cur_pos);
        load_v.addArg(addr_v);
        const cur_mem = try self.mem();
        try load_v.addArgAlloc(cur_mem, self.allocator);
        try cur.addValue(self.allocator, load_v);
        return load_v;
    }

    fn variable(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex) !*Value {
        if (self.vars.get(local_idx)) |v| return v;
        if (self.fwd_vars.get(local_idx)) |v| return v;
        const cur = self.cur_block orelse return error.NoCurrentBlock;
        const fwd = try self.func.newValue(.fwd_ref, type_idx, cur, self.cur_pos);
        fwd.aux_int = @intCast(local_idx);
        try cur.addValue(self.allocator, fwd);
        try self.fwd_vars.put(local_idx, fwd);
        return fwd;
    }

    fn getOrCreateBlock(self: *SSABuilder, ir_block_idx: ir.BlockIndex) !*Block {
        const gop = try self.block_map.getOrPut(ir_block_idx);
        if (!gop.found_existing) gop.value_ptr.* = try self.func.newBlock(.plain);
        return gop.value_ptr.*;
    }

    fn emitConst(self: *SSABuilder, op: Op, type_idx: TypeIndex, value: i64, cur: *Block) !*Value {
        const val = try self.func.newValue(op, type_idx, cur, self.cur_pos);
        val.aux_int = value;
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn emitLocalAddr(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.local_addr, type_idx, cur, self.cur_pos);
        val.aux_int = @intCast(self.getSlotOffset(local_idx));
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn getSlotOffset(self: *const SSABuilder, local_idx: ir.LocalIdx) u32 {
        if (local_idx < self.local_slot_offsets.len) {
            return self.local_slot_offsets[local_idx];
        }
        var next_slot: u32 = 0;
        for (0..self.ir_func.locals.len) |i_inner| {
            if (i_inner == local_idx) return next_slot;
            const num_slots = @max(1, (self.ir_func.locals[i_inner].size + 7) / 8);
            next_slot += num_slots;
        }
        return next_slot;
    }

    fn getLoadOp(self: *SSABuilder, type_idx: TypeIndex) Op {
        const type_info = self.type_registry.get(type_idx);
        if (type_info == .basic) {
            return switch (type_info.basic) {
                .bool_type => .load8,
                .i8_type => .load8s,
                .u8_type => .load8,
                .i16_type => .load16s,
                .u16_type => .load16,
                .i32_type => .load32s,
                .u32_type => .load32,
                .f32_type => .load32,
                .f64_type => .load,
                else => .load,
            };
        }
        if (type_info == .enum_type) {
            return self.getLoadOp(type_info.enum_type.backing_type);
        }
        return .load;
    }

    fn getStoreOp(self: *SSABuilder, type_idx: TypeIndex) Op {
        const type_info = self.type_registry.get(type_idx);
        if (type_info == .basic) {
            return switch (type_info.basic) {
                .bool_type, .i8_type, .u8_type => .store8,
                .i16_type, .u16_type => .store16,
                .i32_type, .u32_type, .f32_type => .store32,
                else => .store,
            };
        }
        if (type_info == .enum_type) {
            return self.getStoreOp(type_info.enum_type.backing_type);
        }
        return .store;
    }

    pub fn insertPhis(self: *SSABuilder) !void {
        var fwd_refs = std.ArrayListUnmanaged(*Value){};
        defer fwd_refs.deinit(self.allocator);

        for (self.func.blocks.items) |block| {
            for (block.values.items) |value| {
                if (value.op == .fwd_ref) {
                    try fwd_refs.append(self.allocator, value);
                    const local_idx: ir.LocalIdx = @intCast(value.aux_int);
                    try self.ensureDefvar(block.id, local_idx, value);
                }
            }
        }

        var args_list = std.ArrayListUnmanaged(*Value){};
        defer args_list.deinit(self.allocator);

        while (fwd_refs.pop()) |fwd| {
            const block = fwd.block orelse continue;
            if (block == self.func.entry) continue;
            if (block.preds.len == 0) {
                const local_idx_orphan: ir.LocalIdx = @intCast(fwd.aux_int);
                if (self.func.entry) |entry_blk| {
                    if (self.defvars.get(entry_blk.id)) |entry_defs| {
                        if (entry_defs.get(local_idx_orphan)) |val| {
                            fwd.op = .copy;
                            fwd.addArg(val);
                            continue;
                        }
                    }
                }
                continue;
            }

            const local_idx: ir.LocalIdx = @intCast(fwd.aux_int);
            args_list.clearRetainingCapacity();

            for (block.preds) |pred_edge| {
                const val = try self.lookupVarOutgoing(pred_edge.b, local_idx, fwd.type_idx, &fwd_refs);
                try args_list.append(self.allocator, val);
            }

            var witness: ?*Value = null;
            var need_phi = false;
            for (args_list.items) |a| {
                if (a == fwd) continue;
                if (witness == null) witness = a else if (a != witness) { need_phi = true; break; }
            }

            if (need_phi) {
                fwd.op = .phi;
                for (args_list.items) |v| try fwd.addArgAlloc(v, self.allocator);
            } else if (witness) |w| {
                fwd.op = .copy;
                fwd.addArg(w);
            }
        }

        try self.reorderPhis();
    }

    fn reorderPhis(self: *SSABuilder) !void {
        for (self.func.blocks.items) |block| {
            var phis = std.ArrayListUnmanaged(*Value){};
            defer phis.deinit(self.allocator);
            var non_phis = std.ArrayListUnmanaged(*Value){};
            defer non_phis.deinit(self.allocator);

            for (block.values.items) |v| {
                if (v.op == .phi) try phis.append(self.allocator, v) else try non_phis.append(self.allocator, v);
            }

            block.values.clearRetainingCapacity();
            for (phis.items) |v| try block.values.append(self.allocator, v);
            for (non_phis.items) |v| try block.values.append(self.allocator, v);
        }
    }

    fn ensureDefvar(self: *SSABuilder, block_id: u32, local_idx: ir.LocalIdx, value: *Value) !void {
        const gop = try self.defvars.getOrPut(block_id);
        if (!gop.found_existing) gop.value_ptr.* = std.AutoHashMap(ir.LocalIdx, *Value).init(self.allocator);
        const inner_gop = try gop.value_ptr.getOrPut(local_idx);
        if (!inner_gop.found_existing) inner_gop.value_ptr.* = value;
    }

    fn lookupVarOutgoing(self: *SSABuilder, block: *Block, local_idx: ir.LocalIdx, type_idx: TypeIndex, fwd_refs: *std.ArrayListUnmanaged(*Value)) !*Value {
        var cur = block;
        while (true) {
            if (self.defvars.get(cur.id)) |block_defs| {
                if (block_defs.get(local_idx)) |val| return val;
            }
            if (cur.preds.len == 1) { cur = cur.preds[0].b; continue; }
            break;
        }

        if (cur.preds.len == 0) {
            if (self.func.entry) |entry_blk| {
                if (self.defvars.get(entry_blk.id)) |entry_defs| {
                    if (entry_defs.get(local_idx)) |val| return val;
                }
            }
            debug.log(.ssa, "WARNING: lookupVarOutgoing failed for local {d} in 0-pred block b{d}", .{ local_idx, cur.id });
            const undef = try self.func.newValue(.const_int, type_idx, cur, self.cur_pos);
            try cur.addValue(self.allocator, undef);
            return undef;
        }

        const new_fwd = try self.func.newValue(.fwd_ref, type_idx, cur, self.cur_pos);
        new_fwd.aux_int = @intCast(local_idx);
        try cur.addValue(self.allocator, new_fwd);

        const gop = try self.defvars.getOrPut(cur.id);
        if (!gop.found_existing) gop.value_ptr.* = std.AutoHashMap(ir.LocalIdx, *Value).init(self.allocator);
        try gop.value_ptr.put(local_idx, new_fwd);
        try fwd_refs.append(self.allocator, new_fwd);

        return new_fwd;
    }

    pub fn verify(self: *SSABuilder) !void {
        for (self.func.blocks.items) |block| {
            var seen_non_phi = false;
            for (block.values.items) |v| {
                if (v.op == .phi) {
                    if (seen_non_phi) return error.PhiNotAtBlockStart;
                    if (v.argsLen() != block.preds.len) return error.PhiArgCountMismatch;
                } else seen_non_phi = true;
                if (v.op == .fwd_ref) {
                    debug.log(.ssa, "UNRESOLVED FwdRef: v{d} local_idx={d} type={d} block=b{d} func={s}", .{
                        v.id, v.aux_int, @intFromEnum(v.type_idx), block.id, self.func.name,
                    });
                    return error.UnresolvedFwdRef;
                }
            }
            switch (block.kind) {
                .ret => if (block.succs.len != 0) return error.RetBlockHasSuccessors,
                .if_ => {
                    if (block.succs.len != 2) return error.IfBlockWrongSuccessors;
                    if (block.controls[0] == null) return error.IfBlockNoCondition;
                },
                else => {},
            }
        }
    }
};

test "SSABuilder basic init" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = @enumFromInt(0), .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func_ptr = builder.func;
        builder.deinit();
        func_ptr.deinit();
        allocator.destroy(func_ptr);
    }

    try std.testing.expectEqualStrings("test", builder.func.name);
    try std.testing.expect(builder.func.entry != null);
}

test "SSABuilder block transitions" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = @enumFromInt(0), .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func_ptr = builder.func;
        builder.deinit();
        func_ptr.deinit();
        allocator.destroy(func_ptr);
    }

    try std.testing.expect(builder.cur_block != null);

    const block2 = try builder.func.newBlock(.plain);
    builder.startBlock(block2);
    try std.testing.expectEqual(block2, builder.cur_block.?);

    const ended = builder.endBlock();
    try std.testing.expectEqual(block2, ended.?);
    try std.testing.expect(builder.cur_block == null);
}

test "SSABuilder variable tracking" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = @enumFromInt(0), .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func_ptr = builder.func;
        builder.deinit();
        func_ptr.deinit();
        allocator.destroy(func_ptr);
    }

    const cur = builder.cur_block.?;

    const const_val = try builder.func.newValue(.const_int, TypeRegistry.I64, cur, .{});
    const_val.aux_int = 42;
    try cur.addValue(allocator, const_val);

    builder.assign(0, const_val);

    const looked_up = try builder.variable(0, TypeRegistry.I64);
    try std.testing.expectEqual(const_val, looked_up);
}
