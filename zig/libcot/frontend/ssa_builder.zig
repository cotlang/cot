//! IR to SSA Conversion - Go's FwdRef pattern for deferred phi insertion.

const std = @import("std");
const ir = @import("ir.zig");
const types = @import("types.zig");
const source = @import("source.zig");
const target_mod = @import("target.zig");
const ssa = @import("../ssa/func.zig");
const ssa_block = @import("../ssa/block.zig");
const ssa_value = @import("../ssa/value.zig");
const ssa_op = @import("../ssa/op.zig");
const debug = @import("../debug.zig");

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

pub const ConvertError = error{
    MissingValue,
    NoCurrentBlock,
    OutOfMemory,
    NeedAllocator,
};

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
    /// Maps IR local_idx → frame slot offset (in 8-byte units).
    /// Accounts for multi-word locals (structs, tuples) occupying multiple slots.
    local_slot_offsets: []u32,

    const LoopContext = struct { continue_block: *Block, break_block: *Block };

    /// Go: memVar = ssaMarker("mem") — sentinel local index for memory state.
    /// Memory is tracked as a regular variable in vars/defvars so that insertPhis
    /// automatically creates memory Phi nodes at block join points.
    /// Uses maxInt to avoid collision with real IR local indices.
    pub const MEM_VAR: ir.LocalIdx = std.math.maxInt(ir.LocalIdx);

    /// Go: s.mem() — get current memory state for this block.
    /// If not defined in the current block, creates a FwdRef that insertPhis resolves.
    fn mem(self: *SSABuilder) !*Value {
        return self.variable(MEM_VAR, TypeRegistry.SSA_MEM);
    }

    /// Go: s.vars[memVar] = v — set current memory state.
    /// If v doesn't have SSA_MEM type (e.g., a call returning pointer), wrap it
    /// in a copy<SSA_MEM> so that all memory args have consistent SSA_MEM type.
    /// This enables the wasm stripping pass to identify memory args reliably.
    fn setMem(self: *SSABuilder, v: *Value) void {
        if (v.type_idx != TypeRegistry.SSA_MEM) {
            // Wrap non-SSA_MEM value in a copy with SSA_MEM type
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

        // Compute local slot offsets: each IR local gets a frame slot
        // Multi-word locals (structs, tuples) occupy multiple 8-byte slots
        //
        // Overlap group support: locals in the same overlap_group but different
        // overlap_arm are mutually exclusive (e.g., switch arms) and share the
        // same stack region. The region is sized to the max arm total.
        const num_locals = ir_func.locals.len;
        const slot_offsets = try allocator.alloc(u32, num_locals);
        {
            // Check if any locals use overlap groups
            var has_overlap = false;
            for (ir_func.locals) |local| {
                if (local.overlap_group > 0) { has_overlap = true; break; }
            }

            if (!has_overlap) {
                // Fast path: no overlap groups, sequential allocation
                var next_slot: u32 = 0;
                for (ir_func.locals, 0..) |local, idx| {
                    slot_offsets[idx] = next_slot;
                    const num_slots = @max(1, (local.size + 7) / 8);
                    next_slot += num_slots;
                }


            } else {
                // Overlap group path: compute max arm sizes, share stack regions
                // Pass 1: compute per-arm sizes within each group
                // Key: (group << 16 | arm), Value: total 8-byte slots for that arm
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

                // Pass 2: compute max size per group
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

                // Pass 3: assign offsets — group locals share a region
                var group_base = std.AutoHashMapUnmanaged(u16, u32){};
                defer group_base.deinit(allocator);
                // Track per-arm running offset: (group << 16 | arm) → offset within group
                var arm_offsets = std.AutoHashMapUnmanaged(u32, u32){};
                defer arm_offsets.deinit(allocator);
                var next_slot: u32 = 0;
                for (ir_func.locals, 0..) |local, idx| {
                    if (local.overlap_group == 0) {
                        slot_offsets[idx] = next_slot;
                        const num_slots = @max(1, (local.size + 7) / 8);
                        next_slot += num_slots;
                    } else {
                        // First time seeing this group? Allocate its region.
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

        // Go: s.startmem = s.entryNewValue0(OpInitMem, TypeMem); s.vars[memVar] = s.startmem
        // InitMem at entry block — initial memory state stored as MEM_VAR variable.
        const init_mem = try func.newValue(.init_mem, TypeRegistry.SSA_MEM, entry, .{});
        try entry.addValue(allocator, init_mem);
        try vars.put(MEM_VAR, init_mem);

        // Initialize parameters - emit arg ops for each param
        var phys_reg_idx: i32 = 0;
        const is_wasm_gc = target.isWasmGC();
        for (ir_func.params, 0..) |param, i| {
            const local_type = type_registry.get(param.type_idx);
            const is_string_or_slice = param.type_idx == TypeRegistry.STRING or local_type == .slice;
            const type_size = type_registry.sizeOf(param.type_idx);
            const is_opt_ptr = local_type == .optional and blk: {
                const ei = type_registry.get(local_type.optional.elem);
                break :blk ei == .pointer and ei.pointer.managed;
            };
            const is_compound_opt = local_type == .optional and type_registry.get(local_type.optional.elem) != .pointer and !is_opt_ptr;
            // Compound optionals are always linear memory (not GC refs), so decompose on all targets.
            // Structs/unions/tuples skip decomposition on WasmGC (they use GC refs).
            const is_large_struct = (!is_wasm_gc and (local_type == .struct_type or local_type == .union_type or local_type == .tuple) and type_size > 8) or (is_compound_opt and type_size > 8);

            if (is_opt_ptr) {
                // ?*T: two registers (tag, payload) — Go interface pattern (ITab, IData)
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

                // Store to stack for address-taken variables
                const addr = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                addr.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, addr);
                const tag_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                tag_store.addArg3(addr, tag_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, tag_store);
                try vars.put(MEM_VAR, tag_store);
                const data_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                data_addr.aux_int = 8;
                data_addr.addArg(addr);
                try entry.addValue(allocator, data_addr);
                const data_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                data_store.addArg3(data_addr, data_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, data_store);
                try vars.put(MEM_VAR, data_store);
            } else if (is_string_or_slice) {
                // String/slice: two registers (ptr, len)
                const ptr_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                ptr_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, ptr_val);
                phys_reg_idx += 1;

                const len_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                len_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, len_val);
                phys_reg_idx += 1;

                // Go: SliceMake always has 3 args (ptr, len, cap)
                // For params, cap = len (Go convention: ssagen/ssa.go newValue3)
                const slice_val = try func.newValue(.slice_make, param.type_idx, entry, .{});
                slice_val.addArg(ptr_val);
                slice_val.addArg(len_val);
                try slice_val.addArgAlloc(len_val, allocator); // cap = len
                try entry.addValue(allocator, slice_val);
                try vars.put(@intCast(i), slice_val);

                // Store to stack for address-taken variables
                const addr = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                addr.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, addr);
                const ptr_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                ptr_store.addArg3(addr, ptr_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, ptr_store);
                try vars.put(MEM_VAR, ptr_store);
                const len_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                len_addr.aux_int = 8;
                len_addr.addArg(addr);
                try entry.addValue(allocator, len_addr);
                const len_store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                len_store.addArg3(len_addr, len_val, vars.get(MEM_VAR).?);
                try entry.addValue(allocator, len_store);
                try vars.put(MEM_VAR, len_store);
            } else if (is_large_struct) {
                // Large struct: N i64 registers (one per 8-byte chunk)
                const num_slots: u32 = @intCast((type_size + 7) / 8);
                const base_addr = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                base_addr.aux_int = @intCast(slot_offsets[i]);
                try entry.addValue(allocator, base_addr);

                for (0..num_slots) |slot| {
                    const chunk_val = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
                    chunk_val.aux_int = phys_reg_idx;
                    try entry.addValue(allocator, chunk_val);
                    phys_reg_idx += 1;

                    if (slot == 0) {
                        const store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                        store.addArg3(base_addr, chunk_val, vars.get(MEM_VAR).?);
                        try entry.addValue(allocator, store);
                        try vars.put(MEM_VAR, store);
                    } else {
                        const off_addr = try func.newValue(.off_ptr, TypeRegistry.VOID, entry, .{});
                        off_addr.aux_int = @intCast(slot * 8);
                        off_addr.addArg(base_addr);
                        try entry.addValue(allocator, off_addr);
                        const store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                        store.addArg3(off_addr, chunk_val, vars.get(MEM_VAR).?);
                        try entry.addValue(allocator, store);
                        try vars.put(MEM_VAR, store);
                    }
                }
            } else {
                // Regular param: single register
                const arg_val = try func.newValue(.arg, param.type_idx, entry, .{});
                arg_val.aux_int = phys_reg_idx;
                try entry.addValue(allocator, arg_val);
                phys_reg_idx += 1;
                try vars.put(@intCast(i), arg_val);

                // Store to stack — but NOT for WasmGC struct/pointer-to-struct params
                // (they're GC refs, not linear memory values). This covers both
                // direct struct params and self: *Type method params.
                // Only managed pointers (from `new`) are GC refs; raw pointers (from @intToPtr) stay as i64.
                // Reference: Kotlin/Dart WasmGC — struct refs stay in Wasm locals.
                const is_gc_ref_param = is_wasm_gc and (local_type == .struct_type or
                    (local_type == .pointer and local_type.pointer.managed and type_registry.get(local_type.pointer.elem) == .struct_type));
                if (!is_gc_ref_param) {
                    const addr = try func.newValue(.local_addr, TypeRegistry.VOID, entry, .{});
                    addr.aux_int = @intCast(slot_offsets[i]);
                    try entry.addValue(allocator, addr);
                    const store = try func.newValue(.store, TypeRegistry.SSA_MEM, entry, .{});
                    store.addArg3(addr, arg_val, vars.get(MEM_VAR).?);
                    try entry.addValue(allocator, store);
                    try vars.put(MEM_VAR, store);
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

    /// Report NoCurrentBlock with source location for debugging.
    /// This is a compiler internal error — the IR lowerer emitted code after a block
    /// terminator (return/branch), which left cur_block = null.
    fn reportNoCurrentBlock(self: *const SSABuilder, span: source.Span, context: []const u8) void {
        const func_name = self.ir_func.name;
        const offset = span.start.offset;
        std.debug.print("INTERNAL ERROR: NoCurrentBlock in {s}() during SSA conversion of '{s}' (source offset {d})\n", .{ context, func_name, offset });
        std.debug.print("  This means the IR lowerer emitted code after a block terminator.\n", .{});
        std.debug.print("  Check the orelse/if-else/return lowering for the expression at this offset.\n", .{});
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
        var it = self.vars.iterator();
        while (it.next()) |entry| gop.value_ptr.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
    }

    pub fn assign(self: *SSABuilder, local_idx: ir.LocalIdx, value: *Value) void {
        self.vars.put(local_idx, value) catch {};
    }

    /// Go: s.vars[memVar] = s.newValue3A(OpStore, TypeMem, ..., s.mem())
    /// Store takes (addr, value, mem) and produces SSA_MEM (like Go's TypeMem).
    fn emitMemStore(self: *SSABuilder, store_op: Op, addr: *Value, value: *Value, cur: *Block) !*Value {
        const store = try self.func.newValue(store_op, TypeRegistry.SSA_MEM, cur, self.cur_pos);
        store.addArg2(addr, value);
        const cur_mem = try self.mem();
        try store.addArgAlloc(cur_mem, self.allocator);
        try cur.addValue(self.allocator, store);
        self.setMem(store);
        return store;
    }

    /// Go: loads take (addr, s.mem()) for ordering.
    /// Load takes (addr, mem) — reads memory but doesn't produce new state.
    fn emitMemLoad(self: *SSABuilder, load_op: Op, addr: *Value, type_idx: TypeIndex, cur: *Block) !*Value {
        const load = try self.func.newValue(load_op, type_idx, cur, self.cur_pos);
        load.addArg(addr);
        const cur_mem = try self.mem();
        try load.addArgAlloc(cur_mem, self.allocator);
        try cur.addValue(self.allocator, load);
        return load;
    }

    fn variable(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex) !*Value {
        if (self.vars.get(local_idx)) |v| return v;
        if (self.fwd_vars.get(local_idx)) |v| return v;
        const cur = self.cur_block orelse {
            self.reportNoCurrentBlock(source.Span.zero, "variable");
            return error.NoCurrentBlock;
        };
        const fwd = try self.func.newValue(.fwd_ref, type_idx, cur, self.cur_pos);
        fwd.aux_int = @intCast(local_idx);
        try cur.addValue(self.allocator, fwd);
        try self.fwd_vars.put(local_idx, fwd);
        return fwd;
    }

    pub fn build(self: *SSABuilder) !*Func {
        // Copy local sizes and overlap metadata for stack allocation
        if (self.ir_func.locals.len > 0) {
            const n = self.ir_func.locals.len;
            const sizes = try self.allocator.alloc(u32, n);
            const groups = try self.allocator.alloc(u16, n);
            const arms = try self.allocator.alloc(u16, n);
            for (self.ir_func.locals, 0..) |local, i| {
                sizes[i] = local.size;
                groups[i] = local.overlap_group;
                arms[i] = local.overlap_arm;
            }
            self.func.local_sizes = sizes;
            self.func.local_overlap_groups = groups;
            self.func.local_overlap_arms = arms;
            // Copy pre-computed slot offsets (overlap-aware) for native backend
            const offsets = try self.allocator.alloc(u32, n);
            for (0..n) |i| offsets[i] = self.local_slot_offsets[i];
            self.func.local_slot_offsets = offsets;
        }
        if (self.ir_func.string_literals.len > 0) {
            self.func.string_literals = self.ir_func.string_literals;
        }

        // Pre-scan for logical operands
        var logical_operands = std.AutoHashMapUnmanaged(ir.NodeIndex, void){};
        defer logical_operands.deinit(self.allocator);
        for (self.ir_func.blocks) |ir_block| {
            for (ir_block.nodes) |node_idx| {
                const node = self.ir_func.getNode(node_idx);
                if (node.data == .binary) {
                    const b = node.data.binary;
                    if (b.op.isLogical()) {
                        try self.markLogicalOperands(b.left, &logical_operands);
                        try self.markLogicalOperands(b.right, &logical_operands);
                    }
                }
            }
        }

        // Walk all IR blocks
        // Go: init_mem is in vars[MEM_VAR] from init(). Each block starts fresh
        // (startBlock clears vars), so memory state creates FwdRefs at block boundaries
        // that insertPhis resolves into memory Phi nodes.
        for (self.ir_func.blocks, 0..) |ir_block, i| {
            const ssa_block_ptr = try self.getOrCreateBlock(@intCast(i));
            if (i != 0) self.startBlock(ssa_block_ptr);
            for (ir_block.nodes) |node_idx| {
                if (logical_operands.contains(node_idx)) continue;
                _ = self.convertNode(node_idx) catch |e| {
                    const failing_node = self.ir_func.getNode(node_idx);
                    debug.log(.ssa, "SSA error in func '{s}' block {d} node {d} tag={s} err={s}", .{
                        self.ir_func.name, i, node_idx,
                        @tagName(failing_node.data),
                        @errorName(e),
                    });
                    return e;
                };
            }
        }

        // Go: endBlock() saves final block's vars to defvars.
        // Without this, the last block's memory state is missing from defvars
        // and lookupVarOutgoing can't find it during phi insertion.
        self.saveDefvars();

        try self.insertPhis();
        try self.verify();
        const f = self.takeFunc();
        var total_values: usize = 0;
        for (f.blocks.items) |b| total_values += b.values.items.len;
        debug.log(.ssa, "=== SSA built for '{s}': {d} blocks, {d} values, {d} locals ===", .{
            f.name, f.blocks.items.len, total_values, f.local_sizes.len,
        });
        return f;
    }

    fn getOrCreateBlock(self: *SSABuilder, ir_block_idx: ir.BlockIndex) !*Block {
        const gop = try self.block_map.getOrPut(ir_block_idx);
        if (!gop.found_existing) gop.value_ptr.* = try self.func.newBlock(.plain);
        return gop.value_ptr.*;
    }

    fn convertNode(self: *SSABuilder, node_idx: ir.NodeIndex) anyerror!?*Value {
        if (node_idx == ir.null_node) return null;
        if (self.node_values.get(node_idx)) |existing| return existing;
        const node = self.ir_func.getNode(node_idx);
        const cur = self.cur_block orelse {
            self.reportNoCurrentBlock(node.span, "convertNode");
            return error.NoCurrentBlock;
        };
        self.cur_pos = .{ .line = node.span.start.offset, .col = 0 };

        const result: ?*Value = switch (node.data) {
            .const_int => |c| try self.emitConst(.const_int, node.type_idx, c.value, cur),
            .const_float => |c| try self.emitConst(.const_float, node.type_idx, @bitCast(c.value), cur),
            .const_bool => |c| try self.emitConst(.const_bool, node.type_idx, if (c.value) 1 else 0, cur),
            .const_null => try self.emitConst(.const_nil, node.type_idx, 0, cur),
            .const_slice => |c| try self.emitConst(.const_string, node.type_idx, c.string_index, cur),

            .load_local => |l| try self.convertLoadLocal(l.local_idx, node.type_idx, cur),
            .store_local => |s| try self.convertStoreLocal(s.local_idx, s.value, cur),
            .local_ref => |l| try self.emitLocalAddr(l.local_idx, node.type_idx, cur),
            .addr_local => |l| try self.emitLocalAddr(l.local_idx, node.type_idx, cur),

            .global_ref => |g| try self.convertGlobalRef(g.name, node.type_idx, cur),
            .global_store => |g| try self.convertGlobalStore(g.name, g.global_idx, g.value, cur),
            .addr_global => |g| blk: {
                const val = try self.func.newValue(.global_addr, node.type_idx, cur, self.cur_pos);
                val.aux = .{ .string = g.name };
                try cur.addValue(self.allocator, val);
                break :blk val;
            },

            .type_metadata => |m| blk: {
                const val = try self.func.newValue(.metadata_addr, node.type_idx, cur, self.cur_pos);
                val.aux = .{ .string = m.type_name };
                try cur.addValue(self.allocator, val);
                break :blk val;
            },

            .wasm_global_read => |g| blk: {
                const val = try self.func.newValue(.wasm_global_get, node.type_idx, cur, self.cur_pos);
                val.aux_int = @intCast(g.global_idx);
                try cur.addValue(self.allocator, val);
                break :blk val;
            },

            .binary => |b| try self.convertBinary(b, node.type_idx, cur),
            .unary => |u| try self.convertUnary(u, node.type_idx, cur),

            .call => |c| try self.convertCall(c.func_name, c.args, node.type_idx, cur),
            .call_indirect => |c| try self.convertCallIndirect(c.callee, c.args, node.type_idx, cur),
            .closure_call => |c| try self.convertClosureCall(c.callee, c.context, c.args, node.type_idx, cur),

            .ret => |r| blk: {
                cur.kind = .ret;
                if (r.value) |v| {
                    const ret_val = try self.convertNode(v) orelse return error.MissingValue;
                    cur.setControl(ret_val);
                }
                _ = self.endBlock();
                break :blk null;
            },
            .jump => |j| blk: {
                const target = try self.getOrCreateBlock(j.target);
                try cur.addEdgeTo(self.allocator, target);
                _ = self.endBlock();
                break :blk null;
            },
            .branch => |br| blk: {
                const cond = try self.convertNode(br.condition) orelse return error.MissingValue;
                const then_block = try self.getOrCreateBlock(br.then_block);
                const else_block = try self.getOrCreateBlock(br.else_block);
                cur.kind = .if_;
                cur.setControl(cond);
                try cur.addEdgeTo(self.allocator, then_block);
                try cur.addEdgeTo(self.allocator, else_block);
                _ = self.endBlock();
                break :blk null;
            },

            .field_local => |f| try self.convertFieldLocal(f, node.type_idx, cur),
            .store_local_field => |f| try self.convertStoreLocalField(f, cur),
            .field_value => |f| try self.convertFieldValue(f, node.type_idx, cur),
            .store_field => |f| try self.convertStoreField(f, cur),

            .index_local => |i| try self.convertIndexLocal(i, node.type_idx, cur),
            .index_value => |i| try self.convertIndexValue(i, node.type_idx, cur),
            .store_index_local => |s| try self.convertStoreIndexLocal(s, cur),
            .store_index_value => |s| try self.convertStoreIndexValue(s, cur),

            .slice_local => |s| try self.convertSliceLocal(s, node.type_idx, cur),
            .slice_value => |s| try self.convertSliceValue(s, node.type_idx, cur),
            .slice_ptr => |s| try self.convertSliceOp(.slice_ptr, s.slice, node.type_idx, cur),
            .slice_len => |s| try self.convertSliceOp(.slice_len, s.slice, node.type_idx, cur),
            .slice_cap => |s| try self.convertSliceOp(.slice_cap, s.slice, node.type_idx, cur),

            .ptr_load => |p| try self.convertPtrLoad(p.ptr_local, node.type_idx, cur),
            .ptr_store => |p| try self.convertPtrStore(p, cur),
            .ptr_load_value => |p| try self.convertPtrLoadValue(p.ptr, node.type_idx, cur),
            .ptr_store_value => |p| try self.convertPtrStoreValue(p, cur),
            .ptr_field => |p| try self.convertPtrField(p, node.type_idx, cur),
            .ptr_field_store => |p| try self.convertPtrFieldStore(p, cur),

            .func_addr => |f| blk: {
                const val = try self.func.newValue(.addr, node.type_idx, cur, self.cur_pos);
                val.aux = .{ .string = f.name };
                try cur.addValue(self.allocator, val);
                break :blk val;
            },
            .addr_offset => |ao| blk: {
                const base_val = try self.convertNode(ao.base) orelse return error.MissingValue;
                const val = try self.func.newValue(.off_ptr, node.type_idx, cur, self.cur_pos);
                val.addArg(base_val);
                val.aux_int = ao.offset;
                try cur.addValue(self.allocator, val);
                break :blk val;
            },
            .addr_index => |ai| blk: {
                const base = try self.convertNode(ai.base) orelse return error.MissingValue;
                const idx = try self.convertNode(ai.index) orelse return error.MissingValue;
                const val = try self.func.newValue(.add_ptr, node.type_idx, cur, self.cur_pos);
                // Multiply index by element size
                const scaled_idx = try self.func.newValue(.mul, TypeRegistry.I64, cur, self.cur_pos);
                const elem_size = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
                elem_size.aux_int = ai.elem_size;
                try cur.addValue(self.allocator, elem_size);
                scaled_idx.addArg2(idx, elem_size);
                try cur.addValue(self.allocator, scaled_idx);
                val.addArg2(base, scaled_idx);
                try cur.addValue(self.allocator, val);
                break :blk val;
            },

            .select => |s| try self.convertSelect(s, node.type_idx, cur),
            .atomic_cas => |ac| blk: {
                const ptr = try self.convertNode(ac.ptr) orelse return error.MissingValue;
                const expected = try self.convertNode(ac.expected) orelse return error.MissingValue;
                const new_val = try self.convertNode(ac.new_val) orelse return error.MissingValue;
                const val = try self.func.newValue(.atomic_cas64, node.type_idx, cur, self.cur_pos);
                val.addArg(ptr);
                val.addArg(expected);
                val.addArg(new_val);
                try cur.addValue(self.allocator, val);
                break :blk val;
            },
            .convert => |c| try self.convertConvert(c, node.type_idx, cur),
            .phi => null, // Handled by insertPhis
            .nop => null,
            .trap => blk: {
                // Zig: unreachable → trap instruction. Wasm: opcode 0x00 (unreachable).
                // The lowerer places a dead block after trap, so this block continues
                // normally — the trap value is emitted as a side-effect-only SSA op.
                const val = try self.func.newValue(.wasm_unreachable, node.type_idx, cur, self.cur_pos);
                try cur.addValue(self.allocator, val);
                break :blk null;
            },

            .str_concat => |s| try self.convertStrConcat(s, node.type_idx, cur),
            .string_header => |s| try self.convertStringHeader(s, node.type_idx, cur),
            .slice_header => |s| try self.convertSliceHeader(s, node.type_idx, cur),

            .union_init => |u| try self.convertUnionInit(u, node.type_idx, cur),
            .union_tag => |u| try self.convertUnionTag(u, node.type_idx, cur),
            .union_payload => |u| try self.convertUnionPayload(u, node.type_idx, cur),

            .list_new, .list_push, .list_get, .list_set, .list_len, .list_free,
            .map_new, .map_set, .map_get, .map_has, .map_free => null, // Runtime calls

            .ptr_cast => |p| try self.convertCast(p.operand, node.type_idx, cur),
            .int_to_ptr => |p| try self.convertCast(p.operand, node.type_idx, cur),
            .ptr_to_int => |p| try self.convertCast(p.operand, node.type_idx, cur),

            // WasmGC struct operations - converted to Wasm-level ops
            .gc_struct_new => |gc| try self.convertGcStructNew(gc, node.type_idx, cur),
            .gc_struct_get => |gc| try self.convertGcStructGet(gc, node.type_idx, cur),
            .gc_struct_set => |gc| try self.convertGcStructSet(gc, cur),

            // WasmGC array operations
            .gc_array_new => |gc| try self.convertGcArrayNew(gc, node.type_idx, cur),
            .gc_array_new_default => |gc| try self.convertGcArrayNewDefault(gc, node.type_idx, cur),
            .gc_array_new_fixed => |gc| try self.convertGcArrayNewFixed(gc, node.type_idx, cur),
            .gc_array_new_data => |gc| try self.convertGcArrayNewData(gc, node.type_idx, cur),
            .gc_array_get => |gc| try self.convertGcArrayGet(gc, node.type_idx, cur),
            .gc_array_set => |gc| try self.convertGcArraySet(gc, cur),
            .gc_array_len => |gc| try self.convertGcArrayLen(gc, cur),
            .gc_array_copy => |gc| try self.convertGcArrayCopy(gc, cur),

            // WasmGC reference operations
            .gc_ref_null => |gc| try self.convertGcRefNull(gc, node.type_idx, cur),
            .gc_ref_is_null => |gc| try self.convertGcRefIsNull(gc, cur),
            .gc_ref_eq => |gc| try self.convertGcRefEq(gc, cur),
            .gc_ref_cast => |gc| try self.convertGcRefCast(gc, node.type_idx, cur),
            .gc_ref_test => |gc| try self.convertGcRefTest(gc, cur),
        };

        if (result) |v| try self.node_values.put(node_idx, v);
        return result;
    }

    // === Helper emit functions ===

    fn emitConst(self: *SSABuilder, op: Op, type_idx: TypeIndex, value: i64, cur: *Block) !*Value {
        const val = try self.func.newValue(op, type_idx, cur, self.cur_pos);
        val.aux_int = value;
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn emitLocalAddr(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.local_addr, type_idx, cur, self.cur_pos);
        // Use slot offset (accounts for multi-word locals) instead of raw local_idx
        val.aux_int = @intCast(self.getSlotOffset(local_idx));
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn getSlotOffset(self: *const SSABuilder, local_idx: ir.LocalIdx) u32 {
        if (local_idx < self.local_slot_offsets.len) {
            return self.local_slot_offsets[local_idx];
        }
        // Dynamically-added locals (e.g., __interp_buf from string interpolation):
        // Compute offset by summing sizes of all preceding locals.
        // Cannot use local_idx directly — multi-slot locals (strings=2, buffers=3)
        // would overlap.
        var next_slot: u32 = 0;
        for (0..self.ir_func.locals.len) |i| {
            if (i == local_idx) return next_slot;
            const num_slots = @max(1, (self.ir_func.locals[i].size + 7) / 8);
            next_slot += num_slots;
        }
        return next_slot;
    }

    fn convertLoadLocal(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex, cur: *Block) !*Value {
        // Swift GenOpaque.cpp — address-only types are never decomposed.
        // Return the local's address directly; consumers use memcpy/VWT witnesses.
        if (local_idx < self.ir_func.locals.len and self.ir_func.locals[local_idx].is_address_only) {
            const addr_val = try self.emitLocalAddr(local_idx, type_idx, cur);
            return addr_val;
        }

        const load_type = self.type_registry.get(type_idx);

        // WasmGC: struct and managed pointer-to-struct locals hold GC refs — return SSA value directly.
        // Covers both direct struct vars and self: *Type method params.
        // Raw pointers (@intToPtr) are i64 linear memory addresses, not GC refs.
        // Reference: Kotlin/Dart WasmGC — struct refs are Wasm locals, not memory.
        const is_gc_ref = self.target.isWasmGC() and (load_type == .struct_type or
            (load_type == .pointer and load_type.pointer.managed and self.type_registry.get(load_type.pointer.elem) == .struct_type));
        if (is_gc_ref) {
            if (self.vars.get(local_idx)) |v| return v;
            // Check sealed block defs
            var it = self.defvars.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.get(local_idx)) |v| return v;
            }
            // GC ref not found — should not happen if assign() was called properly
            return error.MissingValue;
        }

        const addr_val = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);

        if (load_type == .slice) {
            // Slice: load ptr, len, cap separately, combine with slice_make
            // Go: SliceMake always has 3 args (ptr, len, cap)
            const ptr_load = try self.emitMemLoad(.load, addr_val, TypeRegistry.I64, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(addr_val);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            cap_addr.aux_int = 16;
            cap_addr.addArg(addr_val);
            try cur.addValue(self.allocator, cap_addr);

            const cap_load = try self.emitMemLoad(.load, cap_addr, TypeRegistry.I64, cur);

            const slice_val = try self.func.newValue(.slice_make, type_idx, cur, self.cur_pos);
            slice_val.addArg(ptr_load);
            slice_val.addArg(len_load);
            try slice_val.addArgAlloc(cap_load, self.allocator); // cap
            try cur.addValue(self.allocator, slice_val);
            return slice_val;
        }

        // ?*T optional pointer: decompose into tag + payload (Go interface pattern)
        if (load_type == .optional) {
            const elem_info = self.type_registry.get(load_type.optional.elem);
            if (elem_info == .pointer and elem_info.pointer.managed) {
                // Load tag@0
                const tag_load = try self.emitMemLoad(.load, addr_val, TypeRegistry.I64, cur);
                // Load payload@8
                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(addr_val);
                try cur.addValue(self.allocator, data_addr);
                const data_load = try self.emitMemLoad(.load, data_addr, TypeRegistry.I64, cur);
                // Compose: opt_make(tag, payload)
                const opt_val = try self.func.newValue(.opt_make, type_idx, cur, self.cur_pos);
                opt_val.addArg(tag_load);
                opt_val.addArg(data_load);
                try cur.addValue(self.allocator, opt_val);
                return opt_val;
            }
        }

        // For large structs (>8 bytes), return the local's address directly.
        // Non-pointer-like optionals (?i64, ?f64) are compound (tag+payload = 16 bytes).
        const type_size = self.type_registry.sizeOf(type_idx);
        const is_compound_optional = load_type == .optional and blk: {
            const elem_info = self.type_registry.get(load_type.optional.elem);
            break :blk elem_info != .pointer;
        };
        if (((load_type == .struct_type or load_type == .tuple or load_type == .union_type or load_type == .existential) and type_size > 8) or is_compound_optional) {
            addr_val.type_idx = type_idx;
            return addr_val;
        }

        // Use type-aware load op for integers (sign/zero-extend correctly).
        // Floats use generic .load — CLIF handles float types directly.
        const type_info = self.type_registry.get(type_idx);
        const is_narrow_int = type_info == .basic and type_info.basic.isInteger() and type_info.basic.size() < 8;
        const load_op: Op = if (is_narrow_int) self.getLoadOp(type_idx) else .load;
        return self.emitMemLoad(load_op, addr_val, type_idx, cur);
    }

    fn convertStoreLocal(self: *SSABuilder, local_idx: ir.LocalIdx, value_idx: ir.NodeIndex, cur: *Block) !*Value {
        const value = try self.convertNode(value_idx) orelse return error.MissingValue;
        const value_type = self.type_registry.get(value.type_idx);

        // Swift GenOpaque.cpp — address-only stores use memcpy, not type-specific decomposition.
        // The value is an address (from convertLoadLocal on another address-only local).
        // Store via memcpy(local_addr, value_addr, local.size).
        if (local_idx < self.ir_func.locals.len and self.ir_func.locals[local_idx].is_address_only) {
            const dst = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
            const local_size = self.ir_func.locals[local_idx].size;
            const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
            size_val.aux_int = @intCast(local_size);
            try cur.addValue(self.allocator, size_val);
            const mc = try self.func.newValue(.call, TypeRegistry.VOID, cur, self.cur_pos);
            mc.aux = .{ .string = "memcpy" };
            try mc.addArgAlloc(dst, self.allocator);
            try mc.addArgAlloc(value, self.allocator);
            try mc.addArgAlloc(size_val, self.allocator);
            {
                const cur_mem = try self.mem();
                try mc.addArgAlloc(cur_mem, self.allocator);
            }
            try cur.addValue(self.allocator, mc);
            self.setMem(mc);
            self.assign(local_idx, value);
            return value;
        }

        // WasmGC: struct/array values are GC refs — assign directly to local, no memory store.
        // The Wasm codegen (setReg) emits local.set for the assigned value.
        // Covers: struct.new, array.new, call results returning structs, managed pointer-to-struct.
        // Raw pointers (@intToPtr) are i64 values, not GC refs.
        if (self.target.isWasmGC()) {
            const is_gc_ref = value.op == .wasm_gc_struct_new or
                value.op == .wasm_gc_array_new or value.op == .wasm_gc_array_new_default or
                value.op == .wasm_gc_array_new_fixed or
                value.op == .wasm_gc_ref_cast or
                value_type == .struct_type or value_type == .union_type or
                (value_type == .pointer and value_type.pointer.managed and self.type_registry.get(value_type.pointer.elem) == .struct_type);
            if (is_gc_ref) {
                self.assign(local_idx, value);
                return value;
            }

            // Storing null to a GC ref optional: emit ref.null instead of const_nil
            if (value.op == .const_nil and local_idx < self.ir_func.locals.len) {
                const local_type = self.ir_func.locals[local_idx].type_idx;
                const local_info = self.type_registry.get(local_type);
                if (local_info == .optional) {
                    const elem_info = self.type_registry.get(local_info.optional.elem);
                    if (elem_info == .struct_type or elem_info == .union_type) {
                        // Emit ref.null for this GC type
                        const type_name = switch (elem_info) {
                            .struct_type => |st| st.name,
                            .union_type => |ut| ut.name,
                            else => "",
                        };
                        const ref_null_val = try self.func.newValue(.wasm_gc_ref_null, local_type, cur, self.cur_pos);
                        ref_null_val.aux = .{ .string = type_name };
                        try cur.addValue(self.allocator, ref_null_val);
                        self.assign(local_idx, ref_null_val);
                        return ref_null_val;
                    }
                }
            }
        }

        // String/slice compound store: decompose into ptr@0, len@8 (and cap@16 for slices).
        // Handles both decomposed values (string_make/slice_make with args) and
        // non-decomposed values (const_string, field loads) via slice_ptr/slice_len ops.
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;
            var cap_component: ?*Value = null;

            const is_decomposed = (value.op == .slice_make or value.op == .string_make) and value.args.len >= 2;
            if (is_decomposed) {
                ptr_component = value.args[0];
                len_component = value.args[1];
                if (value.op == .slice_make and value.args.len >= 3) cap_component = value.args[2];
            } else {
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            const addr_val = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
            _ = try self.emitMemStore(.store, addr_val, ptr_component, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(addr_val);
            try cur.addValue(self.allocator, len_addr);

            _ = try self.emitMemStore(.store, len_addr, len_component, cur);

            // Store cap at offset 16 (Go slice layout) for slices with cap
            if (cap_component) |cap_val| {
                const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                cap_addr.aux_int = 16;
                cap_addr.addArg(addr_val);
                try cur.addValue(self.allocator, cap_addr);

                _ = try self.emitMemStore(.store, cap_addr, cap_val, cur);
            } else if (value.op == .slice_make) {
                // cap defaults to len for slices without explicit cap
                const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                cap_addr.aux_int = 16;
                cap_addr.addArg(addr_val);
                try cur.addValue(self.allocator, cap_addr);

                _ = try self.emitMemStore(.store, cap_addr, len_component, cur);
            }

            self.assign(local_idx, value);
            return value;
        }

        // ?*T optional pointer store: decompose into tag@0, payload@8.
        // Go interface pattern (ITab, IData) — same shape as string/slice.
        if (value_type == .optional) {
            const opt_elem_info = self.type_registry.get(value_type.optional.elem);
            if (opt_elem_info == .pointer and opt_elem_info.pointer.managed) {
                var tag_component: *Value = undefined;
                var data_component: *Value = undefined;

                if (value.op == .opt_make and value.args.len >= 2) {
                    tag_component = value.args[0];
                    data_component = value.args[1];
                } else {
                    tag_component = try self.func.newValue(.opt_tag, TypeRegistry.I64, cur, self.cur_pos);
                    tag_component.addArg(value);
                    try cur.addValue(self.allocator, tag_component);

                    data_component = try self.func.newValue(.opt_data, TypeRegistry.I64, cur, self.cur_pos);
                    data_component.addArg(value);
                    try cur.addValue(self.allocator, data_component);
                }

                const addr_val = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
                _ = try self.emitMemStore(.store, addr_val, tag_component, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(addr_val);
                try cur.addValue(self.allocator, data_addr);

                _ = try self.emitMemStore(.store, data_addr, data_component, cur);

                self.assign(local_idx, value);
                return value;
            }
        }

        const addr_val = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
        const type_size = self.type_registry.sizeOf(value.type_idx);
        const is_compound_opt = value_type == .optional and blk: {
            const elem_info = self.type_registry.get(value_type.optional.elem);
            break :blk elem_info != .pointer;
        };
        var is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8) or is_compound_opt;

        // When the value is a VOID-typed address (from convertFieldValue/convertFieldLocal
        // for compound struct/array/union fields), check the TARGET local's type to determine
        // if bulk copy is needed. convertFieldLocal returns off_ptr with VOID type for
        // struct/array/union fields — the address is valid but untyped. The local knows
        // the expected compound type.
        // NOTE: includes ALL struct/tuple/union sizes (not just >8), because the value is
        // an address, not a loaded scalar. Even single-field structs (8 bytes) need OpMove
        // to dereference the address and copy the data. Without this, the address itself
        // gets stored as the scalar value, corrupting the local's contents.
        if (!is_large_struct and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
            const local_type_idx = self.ir_func.locals[local_idx].type_idx;
            const local_type = self.type_registry.get(local_type_idx);
            const local_is_compound_opt = local_type == .optional and blk: {
                const elem_info = self.type_registry.get(local_type.optional.elem);
                break :blk elem_info != .pointer;
            };
            if ((local_type == .struct_type or local_type == .tuple or local_type == .union_type or local_type == .existential) or local_is_compound_opt) {
                is_large_struct = true;
            }
        }

        if (is_large_struct) {
            // Use OpMove for bulk memory copy.
            // Source address: convertLoadLocal returns local_addr (op=local_addr), so
            // we use value directly. convertPtrLoadValue returns a loaded pointer
            // (op=load), which IS the address to copy from — use value directly too.
            // Both cases: use value as src_addr.
            const src_addr = value;
            const move_val = try self.func.newValue(.move, TypeRegistry.SSA_MEM, cur, self.cur_pos);
            move_val.addArg2(addr_val, src_addr);
            {
                const cur_mem = try self.mem();
                try move_val.addArgAlloc(cur_mem, self.allocator);
            }
            // Use value's type_size when available, fall back to local's size for
            // VOID-typed addresses (from convertFieldValue for struct fields).
            const move_size = if (type_size > 0) type_size else self.ir_func.locals[local_idx].size;
            move_val.aux_int = @intCast(move_size);
            try cur.addValue(self.allocator, move_val);
            self.setMem(move_val);
            self.assign(local_idx, value);
            return value;
        }

        _ = try self.emitMemStore(.store, addr_val, value, cur);
        self.assign(local_idx, value);
        return value;
    }

    fn convertGlobalRef(self: *SSABuilder, name: []const u8, type_idx: TypeIndex, cur: *Block) !*Value {
        const addr_val = try self.func.newValue(.global_addr, TypeRegistry.VOID, cur, self.cur_pos);
        addr_val.aux = .{ .string = name };
        try cur.addValue(self.allocator, addr_val);

        const load_type = self.type_registry.get(type_idx);
        if (load_type == .slice) {
            // Go: SliceMake always has 3 args (ptr, len, cap)
            const ptr_load = try self.emitMemLoad(.load, addr_val, TypeRegistry.I64, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(addr_val);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            cap_addr.aux_int = 16;
            cap_addr.addArg(addr_val);
            try cur.addValue(self.allocator, cap_addr);

            const cap_load = try self.emitMemLoad(.load, cap_addr, TypeRegistry.I64, cur);

            const slice_val = try self.func.newValue(.slice_make, type_idx, cur, self.cur_pos);
            slice_val.addArg(ptr_load);
            slice_val.addArg(len_load);
            try slice_val.addArgAlloc(cap_load, self.allocator);
            try cur.addValue(self.allocator, slice_val);
            return slice_val;
        }

        // Use type-aware load op for integers (sign/zero-extend correctly).
        // Floats use generic .load — CLIF handles float types directly.
        const gtype_info = self.type_registry.get(type_idx);
        const g_is_narrow_int = gtype_info == .basic and gtype_info.basic.isInteger() and gtype_info.basic.size() < 8;
        const gload_op: Op = if (g_is_narrow_int) self.getLoadOp(type_idx) else .load;
        return self.emitMemLoad(gload_op, addr_val, type_idx, cur);
    }

    fn convertGlobalStore(self: *SSABuilder, name: []const u8, global_idx: ir.GlobalIdx, value_idx: ir.NodeIndex, cur: *Block) !*Value {
        const value = try self.convertNode(value_idx) orelse return error.MissingValue;
        const addr_val = try self.func.newValue(.global_addr, TypeRegistry.VOID, cur, self.cur_pos);
        addr_val.aux = .{ .string = name };
        try cur.addValue(self.allocator, addr_val);

        const value_type = self.type_registry.get(value.type_idx);
        const type_size = self.type_registry.sizeOf(value.type_idx);

        // String/slice: field-by-field decomposition (same as convertPtrStoreValue)
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;

            if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
                ptr_component = value.args[0];
                len_component = value.args[1];
            } else {
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            _ = try self.emitMemStore(.store, addr_val, ptr_component, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(addr_val);
            try cur.addValue(self.allocator, len_addr);

            _ = try self.emitMemStore(.store, len_addr, len_component, cur);

            if (value.op == .slice_make and value.args.len >= 3) {
                const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                cap_addr.aux_int = 16;
                cap_addr.addArg(addr_val);
                try cur.addValue(self.allocator, cap_addr);

                _ = try self.emitMemStore(.store, cap_addr, value.args[2], cur);
            }

            return value;
        }

        // Compound optional (non-pointer element): bulk copy (same as convertStoreLocal)
        const is_compound_opt = value_type == .optional and blk: {
            const elem_info = self.type_registry.get(value_type.optional.elem);
            break :blk elem_info != .pointer;
        };

        // Large struct/tuple/union: bulk .move copy (same as convertStoreLocal)
        var is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8) or is_compound_opt;

        // When the value is a VOID-typed address (from convertFieldValue/convertFieldLocal
        // for compound struct/array/union fields), check the TARGET global's type to determine
        // if bulk copy is needed. These functions return off_ptr with VOID type for compound
        // fields — the address is valid but untyped. The global knows the expected compound type.
        // NOTE: includes ALL struct/tuple/union sizes (not just >8) — same fix as convertStoreLocal.
        if (!is_large_struct and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
            const g_idx: usize = @intCast(global_idx);
            if (g_idx < self.ir_globals.len) {
                const global_type_idx = self.ir_globals[g_idx].type_idx;
                const global_type = self.type_registry.get(global_type_idx);
                const global_is_compound_opt = global_type == .optional and blk: {
                    const elem_info = self.type_registry.get(global_type.optional.elem);
                    break :blk elem_info != .pointer;
                };
                if ((global_type == .struct_type or global_type == .tuple or global_type == .union_type or global_type == .existential) or global_is_compound_opt) {
                    is_large_struct = true;
                }
            }
        }

        if (is_large_struct) {
            const src_addr = value;
            const move_val = try self.func.newValue(.move, TypeRegistry.SSA_MEM, cur, self.cur_pos);
            move_val.addArg2(addr_val, src_addr);
            {
                const cur_mem = try self.mem();
                try move_val.addArgAlloc(cur_mem, self.allocator);
            }
            // Use value's type_size when available, fall back to global's size for
            // VOID-typed addresses (from convertFieldValue for struct fields).
            const g_idx: usize = @intCast(global_idx);
            const move_size = if (type_size > 0) type_size else if (g_idx < self.ir_globals.len) self.ir_globals[g_idx].size else 8;
            move_val.aux_int = @intCast(move_size);
            try cur.addValue(self.allocator, move_val);
            self.setMem(move_val);
            return value;
        }

        // Scalar path — single 8-byte .store (types <= 8 bytes)
        _ = try self.emitMemStore(.store, addr_val, value, cur);
        return value;
    }

    fn convertBinary(self: *SSABuilder, b: ir.Binary, type_idx: TypeIndex, cur: *Block) !*Value {
        if (b.op.isLogical()) return self.convertLogicalOp(b, type_idx);

        const left = try self.convertNode(b.left) orelse return error.MissingValue;
        const right = try self.convertNode(b.right) orelse return error.MissingValue;

        // WasmGC: comparing a GC ref against null → use ref.is_null
        if (self.target.isWasmGC() and (b.op == .eq or b.op == .ne)) {
            const left_is_null = left.op == .const_nil;
            const right_is_null = right.op == .const_nil;
            if (left_is_null or right_is_null) {
                const ref_val = if (left_is_null) right else left;
                const ref_type_info = self.type_registry.get(ref_val.type_idx);
                const is_gc_ref = ref_type_info == .struct_type or ref_type_info == .union_type;
                if (is_gc_ref) {
                    const is_null_val = try self.func.newValue(.wasm_gc_ref_is_null, TypeRegistry.BOOL, cur, self.cur_pos);
                    is_null_val.addArg(ref_val);
                    try cur.addValue(self.allocator, is_null_val);
                    if (b.op == .ne) {
                        // ne null → NOT ref.is_null
                        const not_val = try self.func.newValue(.bool_not, TypeRegistry.BOOL, cur, self.cur_pos);
                        not_val.addArg(is_null_val);
                        try cur.addValue(self.allocator, not_val);
                        return not_val;
                    }
                    return is_null_val;
                }
            }
        }

        // For comparisons, result type is bool - check operand type instead
        const operand_type = left.type_idx;
        const is_float = operand_type == TypeRegistry.F64 or operand_type == TypeRegistry.F32 or operand_type == TypeRegistry.UNTYPED_FLOAT;

        const op_kind: Op = if (is_float) switch (b.op) {
            .add => .add64f, .sub => .sub64f, .mul => .mul64f, .div => .div64f,
            .eq => .eq64f, .ne => .ne64f, .lt => .lt64f, .le => .le64f, .gt => .gt64f, .ge => .ge64f,
            .fmin => .wasm_f64_min, .fmax => .wasm_f64_max,
            .mod => return error.MissingValue, // No float modulo in Wasm
            .bit_and, .bit_or, .bit_xor, .shl, .shr => return error.MissingValue, // Bitwise ops don't apply to floats
            .lt_u, .le_u, .gt_u, .ge_u => return error.MissingValue, // Unsigned ops don't apply to floats
            .atomic_store, .atomic_add, .atomic_exchange => return error.MissingValue, // Atomics are integer-only
            .@"and", .@"or" => unreachable,
        } else switch (b.op) {
            .add => .add, .sub => .sub, .mul => .mul, .div => .div, .mod => .mod,
            .eq => .eq, .ne => .ne, .lt => .lt, .le => .le, .gt => .gt, .ge => .ge,
            .lt_u => .ult, .le_u => .ule, .gt_u => .ugt, .ge_u => .uge,
            .bit_and => .and_, .bit_or => .or_, .bit_xor => .xor, .shl => .shl, .shr => .sar, // >> is arithmetic (Zig parity: signed int)
            .fmin => .wasm_f64_min, .fmax => .wasm_f64_max, // Always float ops
            .atomic_store => .atomic_store64, .atomic_add => .atomic_add64, .atomic_exchange => .atomic_exchange64,
            .@"and", .@"or" => unreachable, // Handled by convertLogicalOp
        };

        const val = try self.func.newValue(op_kind, type_idx, cur, self.cur_pos);
        val.addArg2(left, right);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertUnary(self: *SSABuilder, u: ir.Unary, type_idx: TypeIndex, cur: *Block) !*Value {
        const operand = try self.convertNode(u.operand) orelse return error.MissingValue;
        const is_float = type_idx == TypeRegistry.F64 or type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.UNTYPED_FLOAT;
        const op_kind: Op = switch (u.op) {
            .neg => if (is_float) .neg64f else .neg,
            .not => .bool_not, .bit_not => .not, .optional_unwrap => .copy,
            // Math builtins — emit Wasm f64 ops directly (already in lower_wasm pass-through)
            .abs => .wasm_f64_abs,
            .ceil => .wasm_f64_ceil,
            .floor => .wasm_f64_floor,
            .trunc_float => .wasm_f64_trunc,
            .nearest => .wasm_f64_nearest,
            .sqrt => .wasm_f64_sqrt,
            // Reinterpret casts — Wasm 0xBD/0xBF (Zig @bitCast between f64/i64)
            .f64_reinterpret_i64 => .wasm_f64_reinterpret_i64,
            .i64_reinterpret_f64 => .wasm_i64_reinterpret_f64,
            // Bit manipulation — Wasm i64.ctz/clz/popcnt (0x7A/0x79/0x7B)
            .ctz => .ctz64,
            .clz => .clz64,
            .popcnt => .popcnt64,
            .atomic_load => .atomic_load64,
        };
        const val = try self.func.newValue(op_kind, type_idx, cur, self.cur_pos);
        val.addArg(operand);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertCall(self: *SSABuilder, func_name: []const u8, args: []const ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const call_val = try self.func.newValue(.static_call, type_idx, cur, self.cur_pos);
        call_val.aux = .{ .string = func_name };

        for (args, 0..) |arg_idx, argi| {
            const arg_val = try self.convertNode(arg_idx) orelse {
                std.debug.print("convertCall '{s}' in '{s}': arg[{d}] node={d} returned null\n", .{ func_name, self.func.name, argi, arg_idx });
                return error.MissingValue;
            };
            try self.addCallArg(call_val, arg_val, cur);
        }
        // Go: calls take mem as last arg and produce new memory state
        {
            const cur_mem = try self.mem();
            try call_val.addArgAlloc(cur_mem, self.allocator);
        }
        try cur.addValue(self.allocator, call_val);
        self.setMem(call_val);
        return call_val;
    }

    /// Decompose compound call arguments (string/slice → ptr+len, large struct → lo+hi).
    /// Must match callee param decomposition in buildSSA.
    /// Go reference: ssagen/ssa.go uses OSPTR()/OLEN() to decompose slice args at call sites.
    fn addCallArg(self: *SSABuilder, call_val: *Value, arg_val: *Value, cur: *Block) !void {
        // WasmGC: struct/GC-ref args are single ref values — no decomposition.
        // But strings/slices still decompose to (ptr, len) since they're in linear memory.
        // Raw pointers (@intToPtr) are i64 values, not GC refs — they decompose normally.
        if (self.target.isWasmGC()) {
            const arg_type = self.type_registry.get(arg_val.type_idx);
            const is_gc_ref = arg_type == .struct_type or
                (arg_type == .pointer and arg_type.pointer.managed and self.type_registry.get(arg_type.pointer.elem) == .struct_type);
            if (is_gc_ref) {
                try call_val.addArgAlloc(arg_val, self.allocator);
                return;
            }
        }

        const arg_type = self.type_registry.get(arg_val.type_idx);
        const type_size = self.type_registry.sizeOf(arg_val.type_idx);

        // String/slice decomposition: compound types are passed as 2 i64 values (ptr, len)
        const is_string_or_slice = arg_val.type_idx == TypeRegistry.STRING or arg_type == .slice;
        if (is_string_or_slice) {
            // Extract ptr component
            const ptr_val = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
            ptr_val.addArg(arg_val);
            try cur.addValue(self.allocator, ptr_val);
            try call_val.addArgAlloc(ptr_val, self.allocator);

            // Extract len component
            const len_val = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
            len_val.addArg(arg_val);
            try cur.addValue(self.allocator, len_val);
            try call_val.addArgAlloc(len_val, self.allocator);
            return;
        }

        // Large struct decomposition: structs/unions/compound optionals >8 bytes passed as N i64 values
        const is_compound_opt_arg = arg_type == .optional and self.type_registry.get(arg_type.optional.elem) != .pointer;
        const is_large_struct = (arg_type == .struct_type or arg_type == .union_type or arg_type == .tuple or is_compound_opt_arg) and type_size > 8;
        if (is_large_struct) {
            const addr = try self.getStructAddr(arg_val, cur);
            const num_slots: u32 = @intCast((type_size + 7) / 8);

            for (0..num_slots) |slot| {
                if (slot == 0) {
                    const chunk_val = try self.emitMemLoad(.load, addr, TypeRegistry.I64, cur);
                    try call_val.addArgAlloc(chunk_val, self.allocator);
                } else {
                    const off_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                    off_addr.aux_int = @intCast(slot * 8);
                    off_addr.addArg(addr);
                    try cur.addValue(self.allocator, off_addr);

                    const chunk_val = try self.emitMemLoad(.load, off_addr, TypeRegistry.I64, cur);
                    try call_val.addArgAlloc(chunk_val, self.allocator);
                }
            }
            return;
        }

        // Regular arg: single value
        try call_val.addArgAlloc(arg_val, self.allocator);
    }

    /// Get the address of a struct value (for decomposition)
    fn getStructAddr(_: *SSABuilder, val: *Value, cur: *Block) !*Value {
        // If the value is already an address (local_addr, off_ptr), use it directly.
        // .copy wraps a loaded pointer (from ptr_load_value) — the value IS the struct address.
        if (val.op == .local_addr or val.op == .off_ptr or val.op == .global_addr or val.op == .copy) {
            return val;
        }
        // If it's a load, use the address that was loaded from
        if (val.op == .load and val.args.len > 0) {
            return val.args[0];
        }
        // Otherwise, the struct should be stored in a local - find its address
        // This handles struct literals which are stored to locals
        // Search backwards for a store that stored this value
        for (cur.values.items) |v| {
            if (v.op == .store and v.args.len >= 2 and v.args[1] == val) {
                return v.args[0];
            }
        }
        // Fallback: struct should have been stored, but if we reach here,
        // allocate a temp local (shouldn't normally happen)
        return error.MissingValue;
    }

    fn convertCallIndirect(self: *SSABuilder, callee_idx: ir.NodeIndex, args: []const ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const callee = try self.convertNode(callee_idx) orelse return error.MissingValue;
        const call_val = try self.func.newValue(.inter_call, type_idx, cur, self.cur_pos);
        try call_val.addArgAlloc(callee, self.allocator);
        for (args) |arg_idx| {
            const arg_val = try self.convertNode(arg_idx) orelse return error.MissingValue;
            try self.addCallArg(call_val, arg_val, cur);
        }
        {
            const cur_mem = try self.mem();
            try call_val.addArgAlloc(cur_mem, self.allocator);
        }
        try cur.addValue(self.allocator, call_val);
        self.setMem(call_val);
        return call_val;
    }

    fn convertClosureCall(self: *SSABuilder, callee_idx: ir.NodeIndex, context_idx: ir.NodeIndex, args: []const ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const callee = try self.convertNode(callee_idx) orelse return error.MissingValue;
        const context = try self.convertNode(context_idx) orelse return error.MissingValue;
        const call_val = try self.func.newValue(.closure_call, type_idx, cur, self.cur_pos);
        try call_val.addArgAlloc(callee, self.allocator);
        try call_val.addArgAlloc(context, self.allocator);
        for (args) |arg_idx| {
            const arg_val = try self.convertNode(arg_idx) orelse return error.MissingValue;
            try self.addCallArg(call_val, arg_val, cur);
        }
        {
            const cur_mem = try self.mem();
            try call_val.addArgAlloc(cur_mem, self.allocator);
        }
        try cur.addValue(self.allocator, call_val);
        self.setMem(call_val);
        return call_val;
    }

    fn convertFieldLocal(self: *SSABuilder, f: ir.FieldLocal, type_idx: TypeIndex, cur: *Block) !*Value {
        const addr_val = try self.emitLocalAddr(f.local_idx, TypeRegistry.VOID, cur);
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(addr_val);
        off_val.aux_int = f.offset;
        try cur.addValue(self.allocator, off_val);

        const field_type = self.type_registry.get(type_idx);
        if (field_type == .struct_type or field_type == .array or field_type == .union_type) return off_val;

        // ?*T optional pointer: decompose into tag@0, payload@8 (Go interface pattern)
        if (field_type == .optional) {
            const elem_info = self.type_registry.get(field_type.optional.elem);
            if (elem_info == .pointer and elem_info.pointer.managed) {
                const tag_load = try self.emitMemLoad(.load, off_val, TypeRegistry.I64, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(off_val);
                try cur.addValue(self.allocator, data_addr);

                const data_load = try self.emitMemLoad(.load, data_addr, TypeRegistry.I64, cur);

                const opt_val = try self.func.newValue(.opt_make, type_idx, cur, self.cur_pos);
                opt_val.addArg(tag_load);
                opt_val.addArg(data_load);
                try cur.addValue(self.allocator, opt_val);
                return opt_val;
            }
        }

        // Compound optional: return address (like struct), not loaded scalar
        if (field_type == .optional) {
            const elem_info = self.type_registry.get(field_type.optional.elem);
            if (elem_info != .pointer) {
                off_val.type_idx = type_idx;
                return off_val;
            }
        }

        // String/slice compound load: decompose into ptr@0, len@8, create string_make/slice_make
        const is_string_or_slice = type_idx == TypeRegistry.STRING or field_type == .slice;
        if (is_string_or_slice) {
            const ptr_load = try self.emitMemLoad(.load, off_val, TypeRegistry.I64, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(off_val);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            const make_op: Op = if (type_idx == TypeRegistry.STRING) .string_make else .slice_make;
            const make_val = try self.func.newValue(make_op, type_idx, cur, self.cur_pos);
            make_val.addArg2(ptr_load, len_load);
            try cur.addValue(self.allocator, make_val);
            return make_val;
        }

        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, off_val, type_idx, cur);
    }

    fn convertStoreLocalField(self: *SSABuilder, f: ir.StoreLocalField, cur: *Block) !*Value {
        const value = try self.convertNode(f.value) orelse return error.MissingValue;
        const addr_val = try self.emitLocalAddr(f.local_idx, TypeRegistry.VOID, cur);
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(addr_val);
        off_val.aux_int = f.offset;
        try cur.addValue(self.allocator, off_val);

        // String/slice compound store: decompose into ptr@0, len@8
        const value_type = self.type_registry.get(value.type_idx);
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;

            if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
                ptr_component = value.args[0];
                len_component = value.args[1];
            } else {
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            _ = try self.emitMemStore(.store, off_val, ptr_component, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(off_val);
            try cur.addValue(self.allocator, len_addr);

            return self.emitMemStore(.store, len_addr, len_component, cur);
        }

        // ?*T optional pointer store: decompose into tag@0, payload@8
        if (value_type == .optional) {
            const opt_elem_info = self.type_registry.get(value_type.optional.elem);
            if (opt_elem_info == .pointer and opt_elem_info.pointer.managed) {
                var tag_component: *Value = undefined;
                var data_component: *Value = undefined;

                if (value.op == .opt_make and value.args.len >= 2) {
                    tag_component = value.args[0];
                    data_component = value.args[1];
                } else {
                    tag_component = try self.func.newValue(.opt_tag, TypeRegistry.I64, cur, self.cur_pos);
                    tag_component.addArg(value);
                    try cur.addValue(self.allocator, tag_component);

                    data_component = try self.func.newValue(.opt_data, TypeRegistry.I64, cur, self.cur_pos);
                    data_component.addArg(value);
                    try cur.addValue(self.allocator, data_component);
                }

                _ = try self.emitMemStore(.store, off_val, tag_component, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(off_val);
                try cur.addValue(self.allocator, data_addr);

                return self.emitMemStore(.store, data_addr, data_component, cur);
            }
        }

        // Large struct/tuple: use OpMove for bulk memory copy (same as convertStoreLocal).
        // Extract source address from load result, copy to dest field offset.
        const type_size = self.type_registry.sizeOf(value.type_idx);
        var is_large = (value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8;

        // When the value is a VOID-typed address (from convertFieldValue/convertFieldLocal
        // for compound struct/array/union fields), check the IR node's type to determine
        // if bulk copy is needed. These functions return off_ptr with VOID type for compound
        // fields — the address is valid but untyped. The IR node knows the expected type.
        if (!is_large and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
            const ir_node = self.ir_func.nodes[f.value];
            const ir_type = self.type_registry.get(ir_node.type_idx);
            const ir_size = self.type_registry.sizeOf(ir_node.type_idx);
            const ir_is_compound_opt = ir_type == .optional and blk: {
                const elem_info = self.type_registry.get(ir_type.optional.elem);
                break :blk elem_info != .pointer;
            };
            if (((ir_type == .struct_type or ir_type == .tuple or ir_type == .union_type or ir_type == .existential) and ir_size > 8) or ir_is_compound_opt) {
                is_large = true;
            }
        }

        if (is_large) {
            const src_addr = if (value.op == .load and value.args.len > 0) value.args[0] else value;
            const move_val = try self.func.newValue(.move, TypeRegistry.SSA_MEM, cur, self.cur_pos);
            move_val.addArg2(off_val, src_addr);
            {
                const cur_mem = try self.mem();
                try move_val.addArgAlloc(cur_mem, self.allocator);
            }
            // Use value's type_size when available, fall back to IR node's size for
            // VOID-typed addresses (from convertFieldValue for struct fields).
            const move_size = if (type_size > 0) type_size else self.type_registry.sizeOf(self.ir_func.nodes[f.value].type_idx);
            move_val.aux_int = @intCast(move_size);
            try cur.addValue(self.allocator, move_val);
            self.setMem(move_val);
            return move_val;
        }

        const store_op = self.getStoreOp(value.type_idx);
        return self.emitMemStore(store_op, off_val, value, cur);
    }

    fn convertFieldValue(self: *SSABuilder, f: ir.FieldValue, type_idx: TypeIndex, cur: *Block) !*Value {
        const base = try self.convertNode(f.base) orelse return error.MissingValue;
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(base);
        off_val.aux_int = f.offset;
        try cur.addValue(self.allocator, off_val);

        const field_type = self.type_registry.get(type_idx);
        // Small structs (≤8 bytes): load value so it's usable as a scalar (return, arg).
        // Without this load, returning an 8-byte struct field returns the ADDRESS, not VALUE.
        // Large structs stay as addresses (accessed via SRET or further field ops).
        // Reference: cg_clif value_and_place.rs — CValue::ByVal for small structs.
        if (field_type == .struct_type and self.type_registry.sizeOf(type_idx) <= 8) {
            return self.emitMemLoad(.load, off_val, type_idx, cur);
        }
        if (field_type == .struct_type or field_type == .array or field_type == .union_type) return off_val;

        // ?*T optional pointer: decompose into tag@0, payload@8 (Go interface pattern)
        if (field_type == .optional) {
            const elem_info = self.type_registry.get(field_type.optional.elem);
            if (elem_info == .pointer and elem_info.pointer.managed) {
                const tag_load = try self.emitMemLoad(.load, off_val, TypeRegistry.I64, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(off_val);
                try cur.addValue(self.allocator, data_addr);

                const data_load = try self.emitMemLoad(.load, data_addr, TypeRegistry.I64, cur);

                const opt_val = try self.func.newValue(.opt_make, type_idx, cur, self.cur_pos);
                opt_val.addArg(tag_load);
                opt_val.addArg(data_load);
                try cur.addValue(self.allocator, opt_val);
                return opt_val;
            }
        }

        // Compound optional: return address (like struct), not loaded scalar
        if (field_type == .optional) {
            const elem_info = self.type_registry.get(field_type.optional.elem);
            if (elem_info != .pointer) {
                off_val.type_idx = type_idx;
                return off_val;
            }
        }

        // String/slice compound load: decompose into ptr@0, len@8, create string_make/slice_make
        const is_string_or_slice = type_idx == TypeRegistry.STRING or field_type == .slice;
        if (is_string_or_slice) {
            const ptr_load = try self.emitMemLoad(.load, off_val, TypeRegistry.I64, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(off_val);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            const make_op: Op = if (type_idx == TypeRegistry.STRING) .string_make else .slice_make;
            const make_val = try self.func.newValue(make_op, type_idx, cur, self.cur_pos);
            make_val.addArg2(ptr_load, len_load);
            try cur.addValue(self.allocator, make_val);
            return make_val;
        }

        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, off_val, type_idx, cur);
    }

    fn convertStoreField(self: *SSABuilder, f: ir.StoreField, cur: *Block) !*Value {
        const base = try self.convertNode(f.base) orelse return error.MissingValue;
        const value = try self.convertNode(f.value) orelse return error.MissingValue;
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(base);
        off_val.aux_int = f.offset;
        try cur.addValue(self.allocator, off_val);

        // String/slice compound store: decompose into ptr@0, len@8
        const value_type = self.type_registry.get(value.type_idx);
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;

            if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
                ptr_component = value.args[0];
                len_component = value.args[1];
            } else {
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            _ = try self.emitMemStore(.store, off_val, ptr_component, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(off_val);
            try cur.addValue(self.allocator, len_addr);

            return self.emitMemStore(.store, len_addr, len_component, cur);
        }

        // ?*T optional pointer store: decompose into tag@0, payload@8
        if (value_type == .optional) {
            const opt_elem_info = self.type_registry.get(value_type.optional.elem);
            if (opt_elem_info == .pointer and opt_elem_info.pointer.managed) {
                var tag_component: *Value = undefined;
                var data_component: *Value = undefined;

                if (value.op == .opt_make and value.args.len >= 2) {
                    tag_component = value.args[0];
                    data_component = value.args[1];
                } else {
                    tag_component = try self.func.newValue(.opt_tag, TypeRegistry.I64, cur, self.cur_pos);
                    tag_component.addArg(value);
                    try cur.addValue(self.allocator, tag_component);

                    data_component = try self.func.newValue(.opt_data, TypeRegistry.I64, cur, self.cur_pos);
                    data_component.addArg(value);
                    try cur.addValue(self.allocator, data_component);
                }

                _ = try self.emitMemStore(.store, off_val, tag_component, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(off_val);
                try cur.addValue(self.allocator, data_addr);

                return self.emitMemStore(.store, data_addr, data_component, cur);
            }
        }

        // Compound struct/tuple/union (> 8 bytes): use .move (memcpy)
        // Same pattern as convertStoreLocal and convertPtrStoreValue.
        const type_size = self.type_registry.sizeOf(value.type_idx);
        const is_compound_opt = value_type == .optional and blk: {
            const elem_info = self.type_registry.get(value_type.optional.elem);
            break :blk elem_info != .pointer;
        };
        var is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8) or is_compound_opt;

        // When the value is a VOID-typed address (from convertFieldValue/convertFieldLocal
        // for compound struct/array/union fields), check the IR node's type to determine
        // if bulk copy is needed. These functions return off_ptr with VOID type for compound
        // fields — the address is valid but untyped. The IR node knows the expected type.
        // NOTE: includes ALL struct/tuple/union sizes (not just >8) — same fix as convertStoreLocal.
        if (!is_large_struct and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
            const ir_node = self.ir_func.nodes[f.value];
            const ir_type = self.type_registry.get(ir_node.type_idx);
            const ir_is_compound_opt = ir_type == .optional and blk: {
                const elem_info = self.type_registry.get(ir_type.optional.elem);
                break :blk elem_info != .pointer;
            };
            if ((ir_type == .struct_type or ir_type == .tuple or ir_type == .union_type or ir_type == .existential) or ir_is_compound_opt) {
                is_large_struct = true;
            }
        }

        if (is_large_struct) {
            const src_addr = if (value.op == .load and value.args.len > 0) value.args[0] else value;
            const move_val = try self.func.newValue(.move, TypeRegistry.SSA_MEM, cur, self.cur_pos);
            move_val.addArg2(off_val, src_addr);
            {
                const cur_mem = try self.mem();
                try move_val.addArgAlloc(cur_mem, self.allocator);
            }
            // Use value's type_size when available, fall back to IR node's size for
            // VOID-typed addresses (from convertFieldValue for struct fields).
            const move_size = if (type_size > 0) type_size else self.type_registry.sizeOf(self.ir_func.nodes[f.value].type_idx);
            move_val.aux_int = @intCast(move_size);
            try cur.addValue(self.allocator, move_val);
            self.setMem(move_val);
            return move_val;
        }

        const store_op = self.getStoreOp(value.type_idx);
        return self.emitMemStore(store_op, off_val, value, cur);
    }

    fn convertIndexLocal(self: *SSABuilder, idx: ir.IndexLocal, type_idx: TypeIndex, cur: *Block) !*Value {
        const addr = try self.emitLocalAddr(idx.local_idx, TypeRegistry.VOID, cur);
        const index = try self.convertNode(idx.index) orelse return error.MissingValue;
        return self.emitIndexedLoad(addr, index, idx.elem_size, type_idx, cur);
    }

    fn convertIndexValue(self: *SSABuilder, idx: ir.IndexValue, type_idx: TypeIndex, cur: *Block) !*Value {
        const base = try self.convertNode(idx.base) orelse return error.MissingValue;
        const index = try self.convertNode(idx.index) orelse return error.MissingValue;
        return self.emitIndexedLoad(base, index, idx.elem_size, type_idx, cur);
    }

    fn emitIndexedLoad(self: *SSABuilder, base: *Value, index: *Value, elem_size: u32, type_idx: TypeIndex, cur: *Block) !*Value {
        const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
        size_val.aux_int = elem_size;
        try cur.addValue(self.allocator, size_val);

        const offset = try self.func.newValue(.mul, TypeRegistry.I64, cur, self.cur_pos);
        offset.addArg2(index, size_val);
        try cur.addValue(self.allocator, offset);

        const ptr = try self.func.newValue(.add_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        ptr.addArg2(base, offset);
        try cur.addValue(self.allocator, ptr);

        // String/slice compound load: decompose into ptr@0, len@8, create string_make/slice_make.
        // Same pattern as convertFieldLocal (line 881): compound types need two loads.
        const field_type = self.type_registry.get(type_idx);
        const is_string_or_slice = type_idx == TypeRegistry.STRING or field_type == .slice;
        if (is_string_or_slice) {
            const ptr_load = try self.emitMemLoad(.load, ptr, TypeRegistry.I64, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(ptr);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            const make_op: Op = if (type_idx == TypeRegistry.STRING) .string_make else .slice_make;
            const make_val = try self.func.newValue(make_op, type_idx, cur, self.cur_pos);
            make_val.addArg2(ptr_load, len_load);
            try cur.addValue(self.allocator, make_val);
            return make_val;
        }

        // Choose the right load op based on type size
        // Go reference: wasm/ssa.go loadOp() function
        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, ptr, type_idx, cur);
    }

    /// Choose the correct load operation based on type size and signedness.
    /// Follows Go's loadOp pattern from wasm/ssa.go:535-566.
    /// Swift TypeLowering::isAddressOnly() — check if a type should be treated
    /// as opaque in this function. True when any local with this type_idx is
    /// marked is_address_only (i.e., the type is a generic type parameter in a
    /// shared generic body). Reference: Swift TypeInfo.h.
    fn isAddressOnlyType(self: *SSABuilder, type_idx: TypeIndex) bool {
        for (self.ir_func.locals) |local| {
            if (local.is_address_only and local.type_idx == type_idx) return true;
        }
        return false;
    }

    fn getLoadOp(self: *SSABuilder, type_idx: TypeIndex) Op {
        const type_info = self.type_registry.get(type_idx);
        if (type_info == .basic) {
            return switch (type_info.basic) {
                .bool_type => .load8, // Go: AI64Load8U (1-byte, zero-extend)
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
        // Enum types: delegate to backing type (e.g. enum(u8) → load8)
        if (type_info == .enum_type) {
            return self.getLoadOp(type_info.enum_type.backing_type);
        }
        return .load;
    }

    /// Choose the correct store operation based on type size.
    /// Follows Go's storeOp pattern from wasm/ssa.go.
    fn getStoreOp(self: *SSABuilder, type_idx: TypeIndex) Op {
        const type_info = self.type_registry.get(type_idx);
        if (type_info == .basic) {
            return switch (type_info.basic) {
                .bool_type, .i8_type, .u8_type => .store8, // Go: AI64Store8 (1-byte truncate)
                .i16_type, .u16_type => .store16,
                .i32_type, .u32_type, .f32_type => .store32,
                else => .store,
            };
        }
        // Enum types: delegate to backing type (e.g. enum(u8) → store8)
        if (type_info == .enum_type) {
            return self.getStoreOp(type_info.enum_type.backing_type);
        }
        return .store;
    }

    fn convertStoreIndexLocal(self: *SSABuilder, s: ir.StoreIndexLocal, cur: *Block) !*Value {
        const addr = try self.emitLocalAddr(s.local_idx, TypeRegistry.VOID, cur);
        const index = try self.convertNode(s.index) orelse return error.MissingValue;
        const value = try self.convertNode(s.value) orelse return error.MissingValue;
        return self.emitIndexedStore(addr, index, value, s.elem_size, cur);
    }

    fn convertStoreIndexValue(self: *SSABuilder, s: ir.StoreIndexValue, cur: *Block) !*Value {
        const base = try self.convertNode(s.base) orelse return error.MissingValue;
        const index = try self.convertNode(s.index) orelse return error.MissingValue;
        const value = try self.convertNode(s.value) orelse return error.MissingValue;
        return self.emitIndexedStore(base, index, value, s.elem_size, cur);
    }

    fn emitIndexedStore(self: *SSABuilder, base: *Value, index: *Value, value: *Value, elem_size: u32, cur: *Block) !*Value {
        const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
        size_val.aux_int = elem_size;
        try cur.addValue(self.allocator, size_val);

        const offset = try self.func.newValue(.mul, TypeRegistry.I64, cur, self.cur_pos);
        offset.addArg2(index, size_val);
        try cur.addValue(self.allocator, offset);

        const ptr = try self.func.newValue(.add_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        ptr.addArg2(base, offset);
        try cur.addValue(self.allocator, ptr);

        // String/slice compound store: decompose into ptr@0, len@8.
        // Same pattern as convertStoreLocalField (line 918): compound types need two stores.
        const value_type = self.type_registry.get(value.type_idx);
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;

            if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
                ptr_component = value.args[0];
                len_component = value.args[1];
            } else {
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            _ = try self.emitMemStore(.store, ptr, ptr_component, cur);

            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(ptr);
            try cur.addValue(self.allocator, len_addr);

            return self.emitMemStore(.store, len_addr, len_component, cur);
        }

        const store_op = self.getStoreOp(value.type_idx);
        return self.emitMemStore(store_op, ptr, value, cur);
    }

    fn convertSliceLocal(self: *SSABuilder, s: ir.SliceLocal, type_idx: TypeIndex, cur: *Block) !*Value {
        const addr = try self.emitLocalAddr(s.local_idx, TypeRegistry.VOID, cur);
        const start = if (s.start) |st| try self.convertNode(st) else null;
        const end = if (s.end) |en| try self.convertNode(en) else null;
        return self.emitSlice(addr, start, end, s.elem_size, type_idx, cur);
    }

    fn convertSliceValue(self: *SSABuilder, s: ir.SliceValue, type_idx: TypeIndex, cur: *Block) !*Value {
        const base = try self.convertNode(s.base) orelse return error.MissingValue;
        const start = if (s.start) |st| try self.convertNode(st) else null;
        const end = if (s.end) |en| try self.convertNode(en) else null;
        return self.emitSlice(base, start, end, s.elem_size, type_idx, cur);
    }

    fn emitSlice(self: *SSABuilder, base: *Value, start: ?*Value, end: ?*Value, elem_size: u32, type_idx: TypeIndex, cur: *Block) !*Value {
        // Compute new pointer: base + start * elem_size
        var new_ptr = base;
        if (start) |s| {
            const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
            size_val.aux_int = elem_size;
            try cur.addValue(self.allocator, size_val);

            const offset = try self.func.newValue(.mul, TypeRegistry.I64, cur, self.cur_pos);
            offset.addArg2(s, size_val);
            try cur.addValue(self.allocator, offset);

            new_ptr = try self.func.newValue(.add_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            new_ptr.addArg2(base, offset);
            try cur.addValue(self.allocator, new_ptr);
        }

        // Compute new length: end - start (or use end directly if no start)
        const new_len = if (start != null and end != null) blk: {
            const len = try self.func.newValue(.sub, TypeRegistry.I64, cur, self.cur_pos);
            len.addArg2(end.?, start.?);
            try cur.addValue(self.allocator, len);
            break :blk len;
        } else end;

        if (new_len) |len| {
            // Go: SliceMake always has 3 args (ptr, len, cap)
            // For arr[start:end], cap = len (Go convention: ssagen/ssa.go)
            const slice_val = try self.func.newValue(.slice_make, type_idx, cur, self.cur_pos);
            slice_val.addArg(new_ptr);
            slice_val.addArg(len);
            try slice_val.addArgAlloc(len, self.allocator); // cap = len
            try cur.addValue(self.allocator, slice_val);
            return slice_val;
        }

        return new_ptr;
    }

    fn convertSliceOp(self: *SSABuilder, op: Op, slice_idx: ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const slice = try self.convertNode(slice_idx) orelse return error.MissingValue;
        const val = try self.func.newValue(op, type_idx, cur, self.cur_pos);
        val.addArg(slice);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertPtrLoad(self: *SSABuilder, ptr_local: ir.LocalIdx, type_idx: TypeIndex, cur: *Block) !*Value {
        const ptr_val = try self.variable(ptr_local, type_idx);
        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, ptr_val, type_idx, cur);
    }

    fn convertPtrStore(self: *SSABuilder, p: ir.PtrStore, cur: *Block) !*Value {
        const ptr_val = try self.variable(p.ptr_local, TypeRegistry.VOID);
        const value = try self.convertNode(p.value) orelse return error.MissingValue;
        const store_op = self.getStoreOp(value.type_idx);
        return self.emitMemStore(store_op, ptr_val, value, cur);
    }

    /// Convert ptr.* load — dereference a pointer to get its value.
    /// Go reference: ssagen/ssa.go ODEREF loads — compound types decompose to ptr@0, len@8.
    fn convertPtrLoadValue(self: *SSABuilder, ptr_idx: ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const ptr_val = try self.convertNode(ptr_idx) orelse {
            debug.log(.ssa, "convertPtrLoadValue: ptr_idx={d} returned null (not in node_values)", .{ptr_idx});
            return error.MissingValue;
        };

        // String/slice compound load: load ptr@0 and len@8, create string_make/slice_make
        const value_type = self.type_registry.get(type_idx);
        const is_string_or_slice = type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            // Load ptr component from base address
            const ptr_load = try self.emitMemLoad(.load, ptr_val, TypeRegistry.I64, cur);

            // Load len component from base + 8
            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(ptr_val);
            try cur.addValue(self.allocator, len_addr);

            const len_load = try self.emitMemLoad(.load, len_addr, TypeRegistry.I64, cur);

            // Create string_make/slice_make to bundle the components
            const make_op: Op = if (type_idx == TypeRegistry.STRING) .string_make else .slice_make;
            const make_val = try self.func.newValue(make_op, type_idx, cur, self.cur_pos);
            make_val.addArg2(ptr_load, len_load);
            try cur.addValue(self.allocator, make_val);
            return make_val;
        }

        // ?*T optional pointer: decompose into tag@0, payload@8 (Go interface pattern)
        if (value_type == .optional) {
            const opt_elem_info = self.type_registry.get(value_type.optional.elem);
            if (opt_elem_info == .pointer and opt_elem_info.pointer.managed) {
                const tag_load = try self.emitMemLoad(.load, ptr_val, TypeRegistry.I64, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(ptr_val);
                try cur.addValue(self.allocator, data_addr);

                const data_load = try self.emitMemLoad(.load, data_addr, TypeRegistry.I64, cur);

                const opt_val = try self.func.newValue(.opt_make, type_idx, cur, self.cur_pos);
                opt_val.addArg(tag_load);
                opt_val.addArg(data_load);
                try cur.addValue(self.allocator, opt_val);
                return opt_val;
            }
        }

        // Large compound types (structs, tuples, unions > 8 bytes, compound optionals):
        // the loaded pointer IS the struct's address in memory. We wrap it in a .copy
        // with the struct type so that consumers (convertStoreLocalField, getStructAddr)
        // treat the VALUE as an address rather than extracting .load's args[0] (which
        // would be the pointer local's address, not the struct's address).
        const type_size = self.type_registry.sizeOf(type_idx);
        const is_compound_opt_load = value_type == .optional and blk: {
            const elem_info = self.type_registry.get(value_type.optional.elem);
            break :blk elem_info != .pointer;
        };
        if (((value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8) or is_compound_opt_load) {
            if (ptr_val.op == .load) {
                // Loaded pointer: wrap in .copy so convertStoreLocalField/getStructAddr
                // use the loaded value (= struct address) directly, not .load's args[0].
                const copy_val = try self.func.newValue(.copy, type_idx, cur, self.cur_pos);
                copy_val.addArg(ptr_val);
                try cur.addValue(self.allocator, copy_val);
                return copy_val;
            }
            ptr_val.type_idx = type_idx;
            return ptr_val;
        }

        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, ptr_val, type_idx, cur);
    }

    /// Convert ptr.* = value store.
    /// Go reference: ssagen/ssa.go ODEREF stores — compound types decomposed to separate stores.
    fn convertPtrStoreValue(self: *SSABuilder, p: ir.PtrStoreValue, cur: *Block) !*Value {
        const ptr_val = try self.convertNode(p.ptr) orelse return error.MissingValue;
        const value = try self.convertNode(p.value) orelse return error.MissingValue;

        // Note: address-only ptr stores for T-typed values are handled by lower.zig
        // Phase 8.5 VWT dispatch (T-indirect store path). The SSA builder handles
        // address-only only for LOCAL load/store where we know the exact local_idx.

        const value_type = self.type_registry.get(value.type_idx);
        const type_size = self.type_registry.sizeOf(value.type_idx);

        // String/slice compound store: decompose into ptr@0, len@8 (and cap@16 for slices)
        // Must match convertStoreLocal's compound handling pattern.
        const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
        if (is_string_or_slice) {
            // Extract ptr and len components
            var ptr_component: *Value = undefined;
            var len_component: *Value = undefined;

            if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
                // Direct string_make/slice_make: components are args
                ptr_component = value.args[0];
                len_component = value.args[1];
            } else {
                // Value from local/param: use slice_ptr/slice_len extraction ops
                ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
                ptr_component.addArg(value);
                try cur.addValue(self.allocator, ptr_component);

                len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
                len_component.addArg(value);
                try cur.addValue(self.allocator, len_component);
            }

            // Store ptr at base address
            _ = try self.emitMemStore(.store, ptr_val, ptr_component, cur);

            // Store len at base + 8
            const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            len_addr.aux_int = 8;
            len_addr.addArg(ptr_val);
            try cur.addValue(self.allocator, len_addr);

            const len_store = try self.emitMemStore(.store, len_addr, len_component, cur);

            // Store cap at base + 16 for slices
            if (value.op == .slice_make and value.args.len >= 3) {
                const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                cap_addr.aux_int = 16;
                cap_addr.addArg(ptr_val);
                try cur.addValue(self.allocator, cap_addr);

                _ = try self.emitMemStore(.store, cap_addr, value.args[2], cur);
            }

            return len_store;
        }

        // ?*T optional pointer store: decompose into tag@0, payload@8
        if (value_type == .optional) {
            const opt_elem_info = self.type_registry.get(value_type.optional.elem);
            if (opt_elem_info == .pointer and opt_elem_info.pointer.managed) {
                var tag_component: *Value = undefined;
                var data_component: *Value = undefined;

                if (value.op == .opt_make and value.args.len >= 2) {
                    tag_component = value.args[0];
                    data_component = value.args[1];
                } else {
                    tag_component = try self.func.newValue(.opt_tag, TypeRegistry.I64, cur, self.cur_pos);
                    tag_component.addArg(value);
                    try cur.addValue(self.allocator, tag_component);

                    data_component = try self.func.newValue(.opt_data, TypeRegistry.I64, cur, self.cur_pos);
                    data_component.addArg(value);
                    try cur.addValue(self.allocator, data_component);
                }

                _ = try self.emitMemStore(.store, ptr_val, tag_component, cur);

                const data_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
                data_addr.aux_int = 8;
                data_addr.addArg(ptr_val);
                try cur.addValue(self.allocator, data_addr);

                return self.emitMemStore(.store, data_addr, data_component, cur);
            }
        }

        const is_compound_opt_store = value_type == .optional and blk: {
            const elem_info = self.type_registry.get(value_type.optional.elem);
            break :blk elem_info != .pointer;
        };
        var is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type or value_type == .existential) and type_size > 8) or is_compound_opt_store;

        // When the value is a VOID-typed address (from convertFieldValue/convertFieldLocal
        // for compound struct/array/union fields), check the IR node's type to determine
        // if bulk copy is needed. Same pattern as convertStoreLocalField.
        if (!is_large_struct and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
            const ir_node = self.ir_func.nodes[p.value];
            const ir_type = self.type_registry.get(ir_node.type_idx);
            const ir_size = self.type_registry.sizeOf(ir_node.type_idx);
            const ir_is_compound_opt = ir_type == .optional and blk: {
                const elem_info = self.type_registry.get(ir_type.optional.elem);
                break :blk elem_info != .pointer;
            };
            if (((ir_type == .struct_type or ir_type == .tuple or ir_type == .union_type or ir_type == .existential) and ir_size > 8) or ir_is_compound_opt) {
                is_large_struct = true;
            }
        }

        if (is_large_struct) {
            // Use value directly as src_addr — convertPtrLoadValue returns the
            // pointer itself for large structs (type rewritten but value is addr).
            // Same pattern as convertStoreLocal (line ~673).
            const src_addr = value;
            const move_val = try self.func.newValue(.move, TypeRegistry.SSA_MEM, cur, self.cur_pos);
            move_val.addArg2(ptr_val, src_addr);
            {
                const cur_mem = try self.mem();
                try move_val.addArgAlloc(cur_mem, self.allocator);
            }
            // Use value's type_size when available, fall back to IR node's size for
            // VOID-typed addresses (from convertFieldValue for struct fields).
            const move_size = if (type_size > 0) type_size else self.type_registry.sizeOf(self.ir_func.nodes[p.value].type_idx);
            move_val.aux_int = @intCast(move_size);
            try cur.addValue(self.allocator, move_val);
            self.setMem(move_val);
            return move_val;
        }

        // Use the pointee type from the pointer to determine store width.
        // The value type may be wider (e.g. i64 literal stored through *u8 pointer).
        // Reference: C store semantics — width comes from lvalue type, not rvalue.
        const ptr_type = self.type_registry.get(ptr_val.type_idx);
        const store_type = if (ptr_type == .pointer) ptr_type.pointer.elem else value.type_idx;
        const store_op = self.getStoreOp(store_type);
        return self.emitMemStore(store_op, ptr_val, value, cur);
    }

    fn convertPtrField(self: *SSABuilder, p: ir.PtrField, type_idx: TypeIndex, cur: *Block) !*Value {
        const ptr_val = try self.variable(p.ptr_local, TypeRegistry.VOID);
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(ptr_val);
        off_val.aux_int = p.offset;
        try cur.addValue(self.allocator, off_val);

        const field_type = self.type_registry.get(type_idx);
        if (field_type == .struct_type or field_type == .array) return off_val;

        const load_op = self.getLoadOp(type_idx);
        return self.emitMemLoad(load_op, off_val, type_idx, cur);
    }

    fn convertPtrFieldStore(self: *SSABuilder, p: ir.PtrFieldStore, cur: *Block) !*Value {
        const ptr_val = try self.variable(p.ptr_local, TypeRegistry.VOID);
        const value = try self.convertNode(p.value) orelse return error.MissingValue;
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(ptr_val);
        off_val.aux_int = p.offset;
        try cur.addValue(self.allocator, off_val);

        const store_op = self.getStoreOp(value.type_idx);
        return self.emitMemStore(store_op, off_val, value, cur);
    }

    fn convertSelect(self: *SSABuilder, s: ir.Select, type_idx: TypeIndex, cur: *Block) !*Value {
        const cond = try self.convertNode(s.condition) orelse return error.MissingValue;
        const then_val = try self.convertNode(s.then_value) orelse return error.MissingValue;
        // else_value can be null_node for if-optional without else branch
        const else_val = if (s.else_value == ir.null_node)
            try self.func.constInt(type_idx, 0)
        else
            try self.convertNode(s.else_value) orelse return error.MissingValue;

        // Compound type decomposition: string/slice are (ptr, len) pairs.
        // Wasm `select` only works on single i64, so decompose into
        // separate selects for each component, then recombine.
        const val_type = self.type_registry.get(type_idx);
        const is_string_or_slice = type_idx == TypeRegistry.STRING or val_type == .slice;
        if (is_string_or_slice) {
            const make_op: Op = if (type_idx == TypeRegistry.STRING) .string_make else .slice_make;

            // Extract ptr from both sides
            const then_ptr = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
            then_ptr.addArg(then_val);
            try cur.addValue(self.allocator, then_ptr);
            const else_ptr = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
            else_ptr.addArg(else_val);
            try cur.addValue(self.allocator, else_ptr);

            // Select ptr
            const sel_ptr = try self.func.newValue(.cond_select, TypeRegistry.I64, cur, self.cur_pos);
            sel_ptr.addArg3(cond, then_ptr, else_ptr);
            try cur.addValue(self.allocator, sel_ptr);

            // Extract len from both sides
            const then_len = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
            then_len.addArg(then_val);
            try cur.addValue(self.allocator, then_len);
            const else_len = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
            else_len.addArg(else_val);
            try cur.addValue(self.allocator, else_len);

            // Select len
            const sel_len = try self.func.newValue(.cond_select, TypeRegistry.I64, cur, self.cur_pos);
            sel_len.addArg3(cond, then_len, else_len);
            try cur.addValue(self.allocator, sel_len);

            // Recombine into string_make/slice_make
            const result = try self.func.newValue(make_op, type_idx, cur, self.cur_pos);
            result.addArg2(sel_ptr, sel_len);
            try cur.addValue(self.allocator, result);
            return result;
        }

        const val = try self.func.newValue(.cond_select, type_idx, cur, self.cur_pos);
        val.addArg3(cond, then_val, else_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertConvert(self: *SSABuilder, c: ir.Convert, type_idx: TypeIndex, cur: *Block) !*Value {
        const operand = try self.convertNode(c.operand) orelse return error.MissingValue;
        const val = try self.func.newValue(.convert, type_idx, cur, self.cur_pos);
        val.addArg(operand);
        val.aux = .{ .type_ref = c.from_type };
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertCast(self: *SSABuilder, operand_idx: ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
        const operand = try self.convertNode(operand_idx) orelse return error.MissingValue;
        const val = try self.func.newValue(.copy, type_idx, cur, self.cur_pos);
        val.addArg(operand);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertStrConcat(self: *SSABuilder, s: ir.StrConcat, type_idx: TypeIndex, cur: *Block) !*Value {
        const left = try self.convertNode(s.left) orelse return error.MissingValue;
        const right = try self.convertNode(s.right) orelse return error.MissingValue;
        const val = try self.func.newValue(.string_concat, type_idx, cur, self.cur_pos);
        val.addArg2(left, right);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertStringHeader(self: *SSABuilder, s: ir.StringHeader, type_idx: TypeIndex, cur: *Block) !*Value {
        const ptr = try self.convertNode(s.ptr) orelse return error.MissingValue;
        const len = try self.convertNode(s.len) orelse return error.MissingValue;
        const val = try self.func.newValue(.string_make, type_idx, cur, self.cur_pos);
        val.addArg2(ptr, len);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertSliceHeader(self: *SSABuilder, s: ir.SliceHeader, type_idx: TypeIndex, cur: *Block) !*Value {
        const ptr = try self.convertNode(s.ptr) orelse return error.MissingValue;
        const len = try self.convertNode(s.len) orelse return error.MissingValue;
        const cap = try self.convertNode(s.cap) orelse return error.MissingValue;
        const val = try self.func.newValue(.slice_make, type_idx, cur, self.cur_pos);
        // Go slice: (ptr, len, cap) - 3 arguments
        val.addArg(ptr);
        val.addArg(len);
        val.addArg(cap);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertUnionInit(self: *SSABuilder, u: ir.UnionInit, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.const_int, type_idx, cur, self.cur_pos);
        val.aux_int = u.variant_idx;
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertUnionTag(self: *SSABuilder, u: ir.UnionTag, type_idx: TypeIndex, cur: *Block) !*Value {
        const union_val = try self.convertNode(u.value) orelse return error.MissingValue;
        return self.emitMemLoad(.load, union_val, type_idx, cur);
    }

    fn convertUnionPayload(self: *SSABuilder, u: ir.UnionPayload, type_idx: TypeIndex, cur: *Block) !*Value {
        const union_val = try self.convertNode(u.value) orelse return error.MissingValue;
        const off_val = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        off_val.addArg(union_val);
        off_val.aux_int = 8; // Payload after tag
        try cur.addValue(self.allocator, off_val);

        return self.emitMemLoad(.load, off_val, type_idx, cur);
    }

    fn convertLogicalOp(self: *SSABuilder, b: ir.Binary, result_type: TypeIndex) anyerror!*Value {
        const is_and = b.op == .@"and";
        const left = try self.convertNode(b.left) orelse return error.MissingValue;
        const cur = self.cur_block orelse {
            self.reportNoCurrentBlock(source.Span.zero, "convertLogicalOp");
            return error.NoCurrentBlock;
        };

        const eval_right_block = try self.func.newBlock(.plain);
        const short_circuit_block = try self.func.newBlock(.plain);
        const merge_block = try self.func.newBlock(.plain);

        cur.kind = .if_;
        cur.setControl(left);
        if (is_and) {
            try cur.addEdgeTo(self.allocator, eval_right_block); // true -> eval right
            try cur.addEdgeTo(self.allocator, short_circuit_block); // false -> short circuit
        } else {
            try cur.addEdgeTo(self.allocator, short_circuit_block); // true -> short circuit
            try cur.addEdgeTo(self.allocator, eval_right_block); // false -> eval right
        }
        _ = self.endBlock();

        // Short circuit block
        self.startBlock(short_circuit_block);
        const short_val = try self.func.newValue(.const_bool, result_type, short_circuit_block, self.cur_pos);
        short_val.aux_int = if (is_and) 0 else 1;
        try short_circuit_block.addValue(self.allocator, short_val);
        try short_circuit_block.addEdgeTo(self.allocator, merge_block);
        _ = self.endBlock();

        // Eval right block
        self.startBlock(eval_right_block);
        const right = try self.convertNode(b.right) orelse return error.MissingValue;
        const right_block = self.cur_block orelse {
            self.reportNoCurrentBlock(source.Span.zero, "convertLogicalOp.right");
            return error.NoCurrentBlock;
        };
        try right_block.addEdgeTo(self.allocator, merge_block);
        _ = self.endBlock();

        // Merge block with phi
        self.startBlock(merge_block);
        const phi = try self.func.newValue(.phi, result_type, merge_block, self.cur_pos);
        phi.addArg2(short_val, right);
        try merge_block.addValue(self.allocator, phi);

        return phi;
    }

    fn markLogicalOperands(self: *SSABuilder, node_idx: ir.NodeIndex, set: *std.AutoHashMapUnmanaged(ir.NodeIndex, void)) !void {
        try set.put(self.allocator, node_idx, {});
        const node = self.ir_func.getNode(node_idx);
        switch (node.data) {
            .binary => |b| {
                try self.markLogicalOperands(b.left, set);
                try self.markLogicalOperands(b.right, set);
            },
            .unary => |u| try self.markLogicalOperands(u.operand, set),
            else => {},
        }
    }

    // === Phi insertion ===

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

        var args = std.ArrayListUnmanaged(*Value){};
        defer args.deinit(self.allocator);

        while (fwd_refs.pop()) |fwd| {
            const block = fwd.block orelse continue;
            if (block == self.func.entry) continue;
            // 0-pred blocks (orphan/dead code after @trap): resolve from entry block's
            // defvars instead of skipping. Without this, FwdRefs for memory state in
            // dead code remain unresolved and cause compilation failures.
            if (block.preds.len == 0) {
                const local_idx_orphan: ir.LocalIdx = @intCast(fwd.aux_int);
                if (self.func.entry) |entry| {
                    if (self.defvars.get(entry.id)) |entry_defs| {
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
            args.clearRetainingCapacity();

            for (block.preds) |pred_edge| {
                const val = try self.lookupVarOutgoing(pred_edge.b, local_idx, fwd.type_idx, &fwd_refs);
                try args.append(self.allocator, val);
            }

            var witness: ?*Value = null;
            var need_phi = false;
            for (args.items) |a| {
                if (a == fwd) continue;
                if (witness == null) witness = a else if (a != witness) { need_phi = true; break; }
            }

            if (need_phi) {
                fwd.op = .phi;
                for (args.items) |v| try fwd.addArgAlloc(v, self.allocator);
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

    /// Go: phi.go lookupVarOutgoing — walk back through single-pred chain.
    /// If we hit a multi-pred block, create FwdRef for further resolution.
    /// If we hit a 0-pred block (entry or orphan), check entry block's defvars.
    fn lookupVarOutgoing(self: *SSABuilder, block: *Block, local_idx: ir.LocalIdx, type_idx: TypeIndex, fwd_refs: *std.ArrayListUnmanaged(*Value)) !*Value {
        var cur = block;
        while (true) {
            if (self.defvars.get(cur.id)) |block_defs| {
                if (block_defs.get(local_idx)) |val| return val;
            }
            if (cur.preds.len == 1) { cur = cur.preds[0].b; continue; }
            break;
        }

        // Reached a block with 0 or 2+ predecessors without finding the variable.
        // For 0-pred blocks (entry/orphan): check the entry block's defvars directly.
        // Go: entry block always has memVar defined; 0-pred orphan blocks shouldn't exist.
        if (cur.preds.len == 0) {
            if (self.func.entry) |entry| {
                if (self.defvars.get(entry.id)) |entry_defs| {
                    if (entry_defs.get(local_idx)) |val| return val;
                }
            }
            // Variable not found anywhere — use a zero/undef value to avoid crash
            debug.log(.ssa, "WARNING: lookupVarOutgoing failed for local {d} in 0-pred block b{d}", .{ local_idx, cur.id });
            const undef = try self.func.newValue(.const_int, type_idx, cur, self.cur_pos);
            try cur.addValue(self.allocator, undef);
            return undef;
        }

        // Multi-pred block: create FwdRef for phi insertion
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
                        v.id, v.aux_int, v.type_idx, block.id, self.func.name,
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

    // === WasmGC struct conversion helpers ===

    fn convertGcStructNew(self: *SSABuilder, gc: ir.GcStructNew, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_struct_new, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        for (gc.field_values) |field_val| {
            const arg = try self.convertNode(field_val) orelse continue;
            try val.addArgAlloc(arg, self.allocator);
        }
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcStructGet(self: *SSABuilder, gc: ir.GcStructGet, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_struct_get, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        val.aux_int = @intCast(gc.field_idx);
        const base_val = try self.convertNode(gc.base) orelse return val;
        val.addArg(base_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcStructSet(self: *SSABuilder, gc: ir.GcStructSet, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_struct_set, TypeRegistry.VOID, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        val.aux_int = @intCast(gc.field_idx);
        const base_val = try self.convertNode(gc.base) orelse return val;
        const value_val = try self.convertNode(gc.value) orelse return val;
        val.addArg2(base_val, value_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    // === WasmGC array conversion helpers ===

    fn convertGcArrayNew(self: *SSABuilder, gc: ir.GcArrayNew, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_new, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const init_val = try self.convertNode(gc.init_val) orelse return val;
        const length = try self.convertNode(gc.length) orelse return val;
        val.addArg2(init_val, length);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayNewDefault(self: *SSABuilder, gc: ir.GcArrayNewDefault, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_new_default, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const length = try self.convertNode(gc.length) orelse return val;
        val.addArg(length);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayNewFixed(self: *SSABuilder, gc: ir.GcArrayNewFixed, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_new_fixed, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        val.aux_int = @intCast(gc.values.len);
        for (gc.values) |v_idx| {
            const arg = try self.convertNode(v_idx) orelse continue;
            try val.addArgAlloc(arg, self.allocator);
        }
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayNewData(self: *SSABuilder, gc: ir.GcArrayNewData, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_new_data, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        val.aux_int = @intCast(gc.data_idx);
        const offset_val = try self.convertNode(gc.offset) orelse return val;
        const length_val = try self.convertNode(gc.length) orelse return val;
        val.addArg2(offset_val, length_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayGet(self: *SSABuilder, gc: ir.GcArrayGet, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_get, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const array_val = try self.convertNode(gc.array) orelse return val;
        const index_val = try self.convertNode(gc.index) orelse return val;
        val.addArg2(array_val, index_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArraySet(self: *SSABuilder, gc: ir.GcArraySet, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_set, TypeRegistry.VOID, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const array_val = try self.convertNode(gc.array) orelse return val;
        const index_val = try self.convertNode(gc.index) orelse return val;
        const value_val = try self.convertNode(gc.value) orelse return val;
        try val.addArgAlloc(array_val, self.allocator);
        try val.addArgAlloc(index_val, self.allocator);
        try val.addArgAlloc(value_val, self.allocator);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayLen(self: *SSABuilder, gc: ir.GcArrayLen, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_len, TypeRegistry.I32, cur, self.cur_pos);
        const array_val = try self.convertNode(gc.array) orelse return val;
        val.addArg(array_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcArrayCopy(self: *SSABuilder, gc: ir.GcArrayCopy, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_array_copy, TypeRegistry.VOID, cur, self.cur_pos);
        val.aux = .{ .string = gc.dst_type_name };
        // src type index resolved at codegen via gc_array_name_map
        const dst = try self.convertNode(gc.dst) orelse return val;
        const dst_off = try self.convertNode(gc.dst_offset) orelse return val;
        const src = try self.convertNode(gc.src) orelse return val;
        const src_off = try self.convertNode(gc.src_offset) orelse return val;
        const len = try self.convertNode(gc.length) orelse return val;
        try val.addArgAlloc(dst, self.allocator);
        try val.addArgAlloc(dst_off, self.allocator);
        try val.addArgAlloc(src, self.allocator);
        try val.addArgAlloc(src_off, self.allocator);
        try val.addArgAlloc(len, self.allocator);
        try cur.addValue(self.allocator, val);
        return val;
    }

    // === WasmGC reference conversion helpers ===

    fn convertGcRefNull(self: *SSABuilder, gc: ir.GcRefNull, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_ref_null, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcRefIsNull(self: *SSABuilder, gc: ir.GcRefIsNull, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_ref_is_null, TypeRegistry.BOOL, cur, self.cur_pos);
        const ref_val = try self.convertNode(gc.value) orelse return val;
        val.addArg(ref_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcRefEq(self: *SSABuilder, gc: ir.GcRefEq, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_ref_eq, TypeRegistry.BOOL, cur, self.cur_pos);
        const left = try self.convertNode(gc.left) orelse return val;
        const right = try self.convertNode(gc.right) orelse return val;
        val.addArg2(left, right);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcRefCast(self: *SSABuilder, gc: ir.GcRefCast, type_idx: TypeIndex, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_ref_cast, type_idx, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const ref_val = try self.convertNode(gc.value) orelse return val;
        val.addArg(ref_val);
        try cur.addValue(self.allocator, val);
        return val;
    }

    fn convertGcRefTest(self: *SSABuilder, gc: ir.GcRefTest, cur: *Block) !*Value {
        const val = try self.func.newValue(.wasm_gc_ref_test, TypeRegistry.BOOL, cur, self.cur_pos);
        val.aux = .{ .string = gc.type_name };
        const ref_val = try self.convertNode(gc.value) orelse return val;
        val.addArg(ref_val);
        try cur.addValue(self.allocator, val);
        return val;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "SSABuilder basic init" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = 0, .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func = builder.func;
        builder.deinit();
        func.deinit();
        allocator.destroy(func);
    }

    try std.testing.expectEqualStrings("test", builder.func.name);
    try std.testing.expect(builder.func.entry != null);
}

test "SSABuilder block transitions" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = 0, .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func = builder.func;
        builder.deinit();
        func.deinit();
        allocator.destroy(func);
    }

    // Entry block is current
    try std.testing.expect(builder.cur_block != null);
    const entry = builder.cur_block.?;

    // Create and start a new block
    const block2 = try builder.func.newBlock(.plain);
    builder.startBlock(block2);
    try std.testing.expectEqual(block2, builder.cur_block.?);

    // End block
    const ended = builder.endBlock();
    try std.testing.expectEqual(block2, ended.?);
    try std.testing.expect(builder.cur_block == null);

    _ = entry;
}

test "SSABuilder variable tracking" {
    const allocator = std.testing.allocator;
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    var ir_func = ir.Func{ .name = "test", .type_idx = 0, .return_type = TypeRegistry.VOID, .params = &.{}, .locals = &.{}, .blocks = &.{}, .entry = 0, .nodes = &.{}, .span = source.Span.zero };

    var builder = try SSABuilder.init(allocator, &ir_func, &.{}, &type_reg, Target.native());
    defer {
        const func = builder.func;
        builder.deinit();
        func.deinit();
        allocator.destroy(func);
    }

    const cur = builder.cur_block.?;

    // Create a constant value
    const const_val = try builder.func.newValue(.const_int, TypeRegistry.I64, cur, .{});
    const_val.aux_int = 42;
    try cur.addValue(allocator, const_val);

    // Assign to variable
    builder.assign(0, const_val);

    // Lookup should return same value
    const looked_up = try builder.variable(0, TypeRegistry.I64);
    try std.testing.expectEqual(const_val, looked_up);
}
