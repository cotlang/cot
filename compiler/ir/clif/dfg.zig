//! Data flow graph tracking Instructions, Values, and Blocks.
//!
//! Port of cranelift/codegen/src/ir/dfg.rs
//! Port of cranelift/codegen/src/ir/entities.rs

const std = @import("std");
const types = @import("types.zig");
const jumptable_mod = @import("jumptable.zig");
const builder_mod = @import("builder.zig");

const Type = types.Type;
pub const JumpTables = jumptable_mod.JumpTables;
pub const JumpTableData = jumptable_mod.JumpTableData;
pub const BlockCall = jumptable_mod.BlockCall;

// Import InstructionData from builder - this is the ONLY instruction storage format
// Port of cranelift: pub struct Insts(PrimaryMap<Inst, InstructionData>);
pub const InstructionData = builder_mod.InstructionData;
pub const InstructionFormat = builder_mod.InstructionFormat;
pub const MemFlags = builder_mod.MemFlags;
pub const TrapCode = builder_mod.TrapCode;

// ============================================================================
// Entity Types
// Port of cranelift/codegen/src/ir/entities.rs
// ============================================================================

/// An opaque reference to a basic block in a Function.
pub const Block = struct {
    index: u32,

    pub const RESERVED: Block = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) Block {
        return .{ .index = index };
    }

    pub fn asU32(self: Block) u32 {
        return self.index;
    }

    pub fn eql(self: Block, other: Block) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: Block,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("block{d}", .{self.index});
    }
};

/// An opaque reference to an SSA value.
pub const Value = struct {
    index: u32,

    pub const RESERVED: Value = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) Value {
        return .{ .index = index };
    }

    pub fn asU32(self: Value) u32 {
        return self.index;
    }

    pub fn eql(self: Value, other: Value) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: Value,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("v{d}", .{self.index});
    }
};

/// An opaque reference to an instruction in a Function.
pub const Inst = struct {
    index: u32,

    pub const RESERVED: Inst = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) Inst {
        return .{ .index = index };
    }

    pub fn asU32(self: Inst) u32 {
        return self.index;
    }

    pub fn eql(self: Inst, other: Inst) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: Inst,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("inst{d}", .{self.index});
    }
};

/// An opaque reference to a stack slot.
pub const StackSlot = struct {
    index: u32,

    pub const RESERVED: StackSlot = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) StackSlot {
        return .{ .index = index };
    }

    pub fn asU32(self: StackSlot) u32 {
        return self.index;
    }

    pub fn eql(self: StackSlot, other: StackSlot) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: StackSlot,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("ss{d}", .{self.index});
    }
};

/// An opaque reference to a function reference.
pub const FuncRef = struct {
    index: u32,

    pub const RESERVED: FuncRef = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) FuncRef {
        return .{ .index = index };
    }

    pub fn asU32(self: FuncRef) u32 {
        return self.index;
    }

    pub fn eql(self: FuncRef, other: FuncRef) bool {
        return self.index == other.index;
    }
};

/// An opaque reference to a signature reference.
pub const SigRef = struct {
    index: u32,

    pub const RESERVED: SigRef = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) SigRef {
        return .{ .index = index };
    }

    pub fn asU32(self: SigRef) u32 {
        return self.index;
    }

    pub fn eql(self: SigRef, other: SigRef) bool {
        return self.index == other.index;
    }
};

/// An opaque reference to a jump table.
pub const JumpTable = struct {
    index: u32,

    pub const RESERVED: JumpTable = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) JumpTable {
        return .{ .index = index };
    }

    pub fn asU32(self: JumpTable) u32 {
        return self.index;
    }

    pub fn eql(self: JumpTable, other: JumpTable) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: JumpTable,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("jt{d}", .{self.index});
    }
};

/// An opaque reference to a global value.
pub const GlobalValue = struct {
    index: u32,

    pub const RESERVED: GlobalValue = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) GlobalValue {
        return .{ .index = index };
    }

    pub fn asU32(self: GlobalValue) u32 {
        return self.index;
    }

    pub fn eql(self: GlobalValue, other: GlobalValue) bool {
        return self.index == other.index;
    }

    pub fn withNumber(n: u32) ?GlobalValue {
        if (n < std.math.maxInt(u32)) {
            return GlobalValue{ .index = n };
        } else {
            return null;
        }
    }

    pub fn format(
        self: GlobalValue,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("gv{d}", .{self.index});
    }
};

// ============================================================================
// Value List
// ============================================================================

/// A list of values stored in the value list pool.
pub const ValueList = struct {
    /// Index into the pool where this list starts.
    /// The first element at this index is the length.
    base: u32,

    const Self = @This();

    pub const EMPTY: ValueList = .{ .base = 0 };

    /// Create an empty value list.
    pub fn init() Self {
        return EMPTY;
    }

    /// Check if this is an empty list.
    pub fn isEmpty(self: Self) bool {
        return self.base == 0;
    }
};

/// Memory pool for holding value lists.
pub const ValueListPool = struct {
    data: std.ArrayListUnmanaged(u32),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        var data = std.ArrayListUnmanaged(u32){};
        // Reserve index 0 for empty lists
        data.append(allocator, 0) catch unreachable;
        return .{
            .data = data,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.data.deinit(self.allocator);
    }

    pub fn clear(self: *Self) void {
        self.data.clearRetainingCapacity();
        self.data.append(self.allocator, 0) catch unreachable;
    }

    /// Allocate a new list with the given values.
    pub fn alloc(self: *Self, values: []const Value) !ValueList {
        if (values.len == 0) return ValueList.EMPTY;

        const base: u32 = @intCast(self.data.items.len);

        // Store length first
        try self.data.append(self.allocator, @intCast(values.len));

        // Store values
        for (values) |v| {
            try self.data.append(self.allocator, v.index);
        }

        return .{ .base = base };
    }

    /// Get the values in a list.
    pub fn getSlice(self: *const Self, list: ValueList) []const Value {
        if (list.isEmpty()) return &[_]Value{};

        const list_len = self.data.items[list.base];
        const start = list.base + 1;
        const end = start + list_len;

        // Return a slice reinterpreted as Values
        const data_slice = self.data.items[start..end];
        return @as([*]const Value, @ptrCast(data_slice.ptr))[0..list_len];
    }

    /// Get the values in a list (mutable version for resolveAllAliases).
    pub fn getMutSlice(self: *Self, list: ValueList) []Value {
        if (list.isEmpty()) return &[_]Value{};

        const list_len = self.data.items[list.base];
        const start = list.base + 1;
        const end = start + list_len;
        _ = end;

        // Return a mutable slice reinterpreted as Values
        const data_slice = self.data.items[start..][0..list_len];
        return @as([*]Value, @ptrCast(data_slice.ptr))[0..list_len];
    }

    /// Get the length of a list.
    pub fn len(self: *const Self, list: ValueList) usize {
        if (list.isEmpty()) return 0;
        return self.data.items[list.base];
    }

    /// Push a value to an existing list (creates a new list).
    pub fn push(self: *Self, list: ValueList, value: Value) !ValueList {
        // CRITICAL: Get old list data using INDICES, not pointers!
        // The getSlice() returns a pointer to self.data.items which becomes
        // invalid if append() causes reallocation. Use base indices instead.
        const old_len = if (list.isEmpty()) 0 else self.data.items[list.base];
        const old_start = list.base + 1;

        const base: u32 = @intCast(self.data.items.len);

        // Store new length
        try self.data.append(self.allocator, @intCast(old_len + 1));

        // Copy old values by index (safe even if reallocation occurs within loop
        // since we re-read from self.data.items each iteration)
        for (0..old_len) |i| {
            const old_value_index = self.data.items[old_start + i];
            try self.data.append(self.allocator, old_value_index);
        }

        // Append new value
        try self.data.append(self.allocator, value.index);

        return .{ .base = base };
    }

    /// Remove a value at a specific index from a list (creates a new list).
    /// Port of cranelift EntityList::remove.
    pub fn remove(self: *Self, list: ValueList, index: usize) !ValueList {
        // CRITICAL: Use indices, not pointers, to avoid invalidation from reallocation
        const old_len = if (list.isEmpty()) 0 else self.data.items[list.base];
        if (index >= old_len) return list;
        if (old_len == 1) return ValueList.EMPTY;

        const old_start = list.base + 1;
        const base: u32 = @intCast(self.data.items.len);
        try self.data.append(self.allocator, @intCast(old_len - 1));

        for (0..old_len) |i| {
            if (i != index) {
                const old_value_index = self.data.items[old_start + i];
                try self.data.append(self.allocator, old_value_index);
            }
        }

        return .{ .base = base };
    }
};

// ============================================================================
// Value Definition
// Port of cranelift/codegen/src/ir/dfg.rs ValueDef
// ============================================================================

/// Where did a value come from?
pub const ValueDef = union(enum) {
    /// Value is the n'th result of an instruction.
    result: struct { inst: Inst, num: u16 },
    /// Value is the n'th parameter to a block.
    param: struct { block: Block, num: u16 },
    /// Value is an alias of another value.
    alias: struct { original: Value },

    const Self = @This();

    /// Get the instruction where the value was defined, if any.
    pub fn inst(self: Self) ?Inst {
        return switch (self) {
            .result => |r| r.inst,
            else => null,
        };
    }

    /// Unwrap the instruction, or panic.
    pub fn unwrapInst(self: Self) Inst {
        return self.inst() orelse @panic("Value is not an instruction result");
    }

    /// Get the block where the parameter is defined, if any.
    pub fn block(self: Self) ?Block {
        return switch (self) {
            .param => |p| p.block,
            else => null,
        };
    }

    /// Get the number component of this definition.
    pub fn num(self: Self) u16 {
        return switch (self) {
            .result => |r| r.num,
            .param => |p| p.num,
            .alias => 0,
        };
    }
};

// ============================================================================
// Value Data
// Port of cranelift/codegen/src/ir/dfg.rs ValueData
// ============================================================================

/// Internal table storage for value data.
pub const ValueData = struct {
    /// Type of this value.
    ty: Type,
    /// Definition of this value.
    def: ValueDef,
};

// ============================================================================
// Block Data
// Port of cranelift/codegen/src/ir/dfg.rs BlockData
// ============================================================================

/// Data associated with a basic block.
pub const BlockData = struct {
    /// List of block parameters.
    params: ValueList = ValueList.EMPTY,
};

// ============================================================================
// Opcode re-exports
// ============================================================================

const instructions = @import("instructions.zig");
pub const Opcode = instructions.Opcode;
pub const IntCC = instructions.IntCC;
pub const FloatCC = instructions.FloatCC;

// ============================================================================
// Data Flow Graph
// Port of cranelift/codegen/src/ir/dfg.rs DataFlowGraph
//
// CRITICAL: Stores InstructionData (typed union) directly, NOT a flat struct.
// This matches Cranelift exactly: pub struct Insts(PrimaryMap<Inst, InstructionData>);
// ============================================================================

/// A data flow graph defines all instructions and basic blocks in a function as well as
/// the data flow dependencies between them.
pub const DataFlowGraph = struct {
    allocator: std.mem.Allocator,

    /// Storage for values.
    values: std.ArrayListUnmanaged(ValueData),

    /// Storage for blocks.
    blocks: std.ArrayListUnmanaged(BlockData),

    /// Storage for instruction data.
    /// Port of cranelift: pub insts: Insts (which is PrimaryMap<Inst, InstructionData>)
    insts: std.ArrayListUnmanaged(InstructionData),

    /// Storage for instruction results (value lists per instruction).
    results: std.ArrayListUnmanaged(ValueList),

    /// Memory pool for value lists.
    value_lists: ValueListPool,

    /// Jump tables used by br_table instructions.
    jump_tables: JumpTables,

    /// Number of instructions created (for generating Inst IDs).
    inst_count: u32,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .values = .{},
            .blocks = .{},
            .insts = .{},
            .results = .{},
            .value_lists = ValueListPool.init(allocator),
            .jump_tables = JumpTables.init(allocator),
            .inst_count = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
        self.results.deinit(self.allocator);
        self.value_lists.deinit();
        self.jump_tables.deinit();
    }

    pub fn clear(self: *Self) void {
        self.values.clearRetainingCapacity();
        self.blocks.clearRetainingCapacity();
        self.insts.clearRetainingCapacity();
        self.results.clearRetainingCapacity();
        self.value_lists.clear();
        self.jump_tables.clear();
        self.inst_count = 0;
    }

    // ------------------------------------------------------------------------
    // Instructions
    // Port of cranelift/codegen/src/ir/dfg.rs make_inst
    // ------------------------------------------------------------------------

    /// Create a new instruction with data.
    /// Port of cranelift/codegen/src/ir/dfg.rs:868-873
    pub fn makeInst(self: *Self, data: InstructionData) !Inst {
        const index = self.inst_count;
        self.inst_count += 1;
        // Ensure storage is sized correctly
        while (self.insts.items.len < index) {
            try self.insts.append(self.allocator, .{ .nullary = .{ .opcode = .nop } });
        }
        try self.insts.append(self.allocator, data);
        return Inst.fromIndex(index);
    }

    /// Get the instruction data.
    /// Port of cranelift: self.insts[inst] returning &InstructionData
    pub fn getInst(self: *const Self, inst: Inst) *const InstructionData {
        return &self.insts.items[inst.index];
    }

    /// Get the instruction data mutably.
    pub fn getInstMut(self: *Self, inst: Inst) *InstructionData {
        return &self.insts.items[inst.index];
    }

    /// Replace instruction data.
    pub fn replaceInst(self: *Self, inst: Inst, data: InstructionData) void {
        self.insts.items[inst.index] = data;
    }

    /// Append a branch argument to the branch targeting dest_block.
    /// Port of cranelift/codegen/src/ir/dfg.rs append_branch_arg
    ///
    /// This is used during SSA construction when predecessors disagree on the
    /// value of a variable. The block parameter is kept and each predecessor
    /// branch must pass its value as an argument.
    pub fn appendBranchArg(self: *Self, inst: Inst, dest_block: Block, val: Value) !void {
        const data = self.getInstMut(inst);
        switch (data.*) {
            .jump => |*j| {
                // Jump has a single destination
                if (j.destination.block.index == dest_block.index) {
                    j.destination.args = try self.value_lists.push(j.destination.args, val);
                }
            },
            .brif => |*b| {
                // Check both then and else blocks
                if (b.blocks[0].block.index == dest_block.index) {
                    b.blocks[0].args = try self.value_lists.push(b.blocks[0].args, val);
                }
                if (b.blocks[1].block.index == dest_block.index) {
                    b.blocks[1].args = try self.value_lists.push(b.blocks[1].args, val);
                }
            },
            .branch_table => {
                // Branch tables are not typically modified during SSA construction.
                // If needed, this would require updating all entries targeting dest_block
                // in the jump table. For now, this is not implemented.
            },
            else => {
                // Not a branch instruction, nothing to do
            },
        }
    }

    // ------------------------------------------------------------------------
    // Values
    // ------------------------------------------------------------------------

    /// Create a new value.
    fn makeValue(self: *Self, data: ValueData) !Value {
        const index: u32 = @intCast(self.values.items.len);
        try self.values.append(self.allocator, data);
        return Value.fromIndex(index);
    }

    /// Check if a value reference is valid.
    pub fn valueIsValid(self: *const Self, v: Value) bool {
        return v.index < self.values.items.len;
    }

    /// Get the type of a value.
    pub fn valueType(self: *const Self, v: Value) Type {
        return self.values.items[v.index].ty;
    }

    /// Get the definition of a value.
    pub fn valueDef(self: *const Self, v: Value) ValueDef {
        return self.values.items[v.index].def;
    }

    /// Resolve aliases to get the original value.
    pub fn resolveAliases(self: *const Self, v: Value) Value {
        var current = v;
        while (true) {
            const def = self.values.items[current.index].def;
            switch (def) {
                .alias => |a| current = a.original,
                else => return current,
            }
        }
    }

    /// Create an alias from `dest` to `src`.
    pub fn changeToAliasOf(self: *Self, dest: Value, src: Value) void {
        const original = self.resolveAliases(src);
        const ty = self.values.items[dest.index].ty;
        self.values.items[dest.index] = .{
            .ty = ty,
            .def = .{ .alias = .{ .original = original } },
        };
    }

    /// Replace all uses of value aliases with their resolved values, and delete the aliases.
    ///
    /// Port of cranelift/codegen/src/ir/dfg.rs resolve_all_aliases.
    /// This must be called before lowering to machine instructions.
    pub fn resolveAllAliases(self: *Self) void {
        // First pass: flatten all alias chains so each alias points directly to final value
        for (self.values.items, 0..) |*vdata, idx| {
            switch (vdata.def) {
                .alias => |a| {
                    // Resolve to final value
                    const resolved = self.resolveAliases(a.original);
                    vdata.def = .{ .alias = .{ .original = resolved } };
                    _ = idx;
                },
                else => {},
            }
        }

        // Second pass: rewrite all instruction arguments to use resolved values
        for (self.insts.items) |*inst| {
            self.mapInstValues(inst);
        }

        // Third pass: rewrite branch arguments in jump tables
        for (self.jump_tables.tables.items) |*jt| {
            // Update all branch entries (including default which is at index 0)
            for (jt.allBranchesMut()) |*bc| {
                self.mapBlockCallValues(bc);
            }
        }
    }

    /// Helper: resolve aliases in a single value, returning the resolved value.
    fn resolveValueAlias(self: *Self, v: Value) Value {
        switch (self.values.items[v.index].def) {
            .alias => |a| return a.original,
            else => return v,
        }
    }

    /// Helper: map values in an instruction to their resolved aliases.
    fn mapInstValues(self: *Self, inst: *InstructionData) void {
        switch (inst.*) {
            .nullary, .unary_imm, .unary_ieee32, .unary_ieee64, .trap, .stack_load, .func_addr, .unary_global_value => {},
            .unary => |*d| d.arg = self.resolveValueAlias(d.arg),
            .binary => |*d| {
                d.args[0] = self.resolveValueAlias(d.args[0]);
                d.args[1] = self.resolveValueAlias(d.args[1]);
            },
            .binary_imm64 => |*d| d.arg = self.resolveValueAlias(d.arg),
            .ternary => |*d| {
                d.args[0] = self.resolveValueAlias(d.args[0]);
                d.args[1] = self.resolveValueAlias(d.args[1]);
                d.args[2] = self.resolveValueAlias(d.args[2]);
            },
            .int_compare => |*d| {
                d.args[0] = self.resolveValueAlias(d.args[0]);
                d.args[1] = self.resolveValueAlias(d.args[1]);
            },
            .float_compare => |*d| {
                d.args[0] = self.resolveValueAlias(d.args[0]);
                d.args[1] = self.resolveValueAlias(d.args[1]);
            },
            .jump => |*d| self.mapBlockCallValues(&d.destination),
            .brif => |*d| {
                d.arg = self.resolveValueAlias(d.arg);
                self.mapBlockCallValues(&d.blocks[0]);
                self.mapBlockCallValues(&d.blocks[1]);
            },
            .branch_table => |*d| d.arg = self.resolveValueAlias(d.arg),
            .call => |*d| self.mapValueListValues(d.args),
            .call_indirect => |*d| {
                d.callee = self.resolveValueAlias(d.callee);
                self.mapValueListValues(d.args);
            },
            .cond_trap => |*d| d.arg = self.resolveValueAlias(d.arg),
            .load => |*d| d.arg = self.resolveValueAlias(d.arg),
            .store => |*d| {
                d.args[0] = self.resolveValueAlias(d.args[0]);
                d.args[1] = self.resolveValueAlias(d.args[1]);
            },
            .stack_store => |*d| d.arg = self.resolveValueAlias(d.arg),
            .multi_ary => |*d| self.mapValueListValues(d.args),
        }
    }

    /// Helper: map values in a BlockCall.
    fn mapBlockCallValues(self: *Self, bc: *BlockCall) void {
        if (!bc.args.isEmpty()) {
            self.mapValueListValues(bc.args);
        }
    }

    /// Helper: map values in a ValueList.
    fn mapValueListValues(self: *Self, vl: ValueList) void {
        if (vl.isEmpty()) return;
        const values = self.value_lists.getMutSlice(vl);
        for (values) |*v| {
            v.* = self.resolveValueAlias(v.*);
        }
    }

    // ------------------------------------------------------------------------
    // Blocks
    // ------------------------------------------------------------------------

    /// Create a new basic block.
    pub fn makeBlock(self: *Self) !Block {
        const index: u32 = @intCast(self.blocks.items.len);
        try self.blocks.append(self.allocator, .{});
        return Block.fromIndex(index);
    }

    /// Check if a block reference is valid.
    pub fn blockIsValid(self: *const Self, block: Block) bool {
        return block.index < self.blocks.items.len;
    }

    /// Get the number of block parameters.
    pub fn numBlockParams(self: *const Self, block: Block) usize {
        return self.value_lists.len(self.blocks.items[block.index].params);
    }

    /// Get the block parameters.
    pub fn blockParams(self: *const Self, block: Block) []const Value {
        return self.value_lists.getSlice(self.blocks.items[block.index].params);
    }

    /// Append a block parameter.
    pub fn appendBlockParam(self: *Self, block: Block, ty: Type) !Value {
        const num: u16 = @intCast(self.numBlockParams(block));

        const value = try self.makeValue(.{
            .ty = ty,
            .def = .{ .param = .{ .block = block, .num = num } },
        });

        // Update the block's parameter list
        const old_list = self.blocks.items[block.index].params;
        self.blocks.items[block.index].params = try self.value_lists.push(old_list, value);

        return value;
    }

    /// Remove a block parameter.
    /// Port of cranelift/codegen/src/ir/dfg.rs remove_block_param.
    ///
    /// Removes the block parameter `val` from its block. The value must be a
    /// block parameter. After removal, the `num` fields of subsequent parameters
    /// are decremented to maintain correct indices.
    pub fn removeBlockParam(self: *Self, val: Value) !void {
        const def = self.valueDef(val);
        const block = def.block() orelse @panic("removeBlockParam: value is not a block param");
        const num = def.num();

        // Remove from the block's params list
        const old_list = self.blocks.items[block.index].params;
        self.blocks.items[block.index].params = try self.value_lists.remove(old_list, num);

        // Update the num field of remaining params that come after
        const new_params = self.blockParams(block);
        for (new_params, 0..) |param, i| {
            if (i >= num) {
                // This param needs its num decremented
                const param_data = &self.values.items[param.index];
                if (param_data.def == .param) {
                    param_data.def.param.num = @intCast(i);
                }
            }
        }
    }

    /// Make a BlockCall, bundling together the block and its arguments.
    /// Port of cranelift/codegen/src/ir/dfg.rs:239-246
    pub fn blockCall(self: *Self, block: Block, args: []const Value) !BlockCall {
        return BlockCall.withArgsSlice(block, args, &self.value_lists);
    }

    // ------------------------------------------------------------------------
    // Instruction Results
    // ------------------------------------------------------------------------

    /// Reserve space for instruction results.
    pub fn reserveResults(self: *Self, inst: Inst) !void {
        while (self.results.items.len <= inst.index) {
            try self.results.append(self.allocator, ValueList.EMPTY);
        }
    }

    /// Make a result value for an instruction.
    pub fn makeInstResult(self: *Self, inst: Inst, ty: Type) !Value {
        try self.reserveResults(inst);

        const num: u16 = @intCast(self.value_lists.len(self.results.items[inst.index]));
        const value = try self.makeValue(.{
            .ty = ty,
            .def = .{ .result = .{ .inst = inst, .num = num } },
        });

        // Update the instruction's result list
        const old_list = self.results.items[inst.index];
        self.results.items[inst.index] = try self.value_lists.push(old_list, value);

        return value;
    }

    /// Get the results of an instruction.
    pub fn instResults(self: *const Self, inst: Inst) []const Value {
        if (inst.index >= self.results.items.len) return &[_]Value{};
        return self.value_lists.getSlice(self.results.items[inst.index]);
    }

    /// Get the first result of an instruction.
    pub fn firstResult(self: *const Self, inst: Inst) ?Value {
        const results = self.instResults(inst);
        return if (results.len > 0) results[0] else null;
    }

    /// Get the number of results.
    pub fn numResults(self: *const Self, inst: Inst) usize {
        return self.instResults(inst).len;
    }

    // ------------------------------------------------------------------------
    // Instruction data queries
    // ------------------------------------------------------------------------

    /// Get the instruction opcode.
    pub fn instOpcode(self: *const Self, inst: Inst) Opcode {
        return self.getInst(inst).opcode();
    }

    /// Get the instruction arguments (input values).
    /// Uses pointer captures to return slices into the actual stored data.
    pub fn instArgs(self: *const Self, inst: Inst) []const Value {
        const data = self.getInst(inst);
        // Use pointer captures to get references into the actual stored data
        return switch (data.*) {
            .nullary => &[_]Value{},
            .unary => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .unary_imm, .unary_ieee32, .unary_ieee64 => &[_]Value{},
            .binary => |*d| &d.args,
            .binary_imm64 => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .ternary => |*d| &d.args,
            .int_compare => |*d| &d.args,
            .float_compare => |*d| &d.args,
            .jump => &[_]Value{},
            .brif => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .branch_table => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .call => |d| self.value_lists.getSlice(d.args),
            .call_indirect => |d| self.value_lists.getSlice(d.args),
            .trap => &[_]Value{},
            .cond_trap => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .load => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .store => |*d| &d.args,
            .stack_load => &[_]Value{},
            .stack_store => |*d| @as(*const [1]Value, &d.arg)[0..1],
            .func_addr => &[_]Value{},
            .unary_global_value => &[_]Value{},
            .multi_ary => |d| self.value_lists.getSlice(d.args),
        };
    }

    /// Get an iterator over all instruction input values, including branch arguments.
    ///
    /// Port of cranelift/codegen/src/ir/dfg.rs:897-920 inst_values()
    ///
    /// Cranelift's inst_values() chains:
    /// 1. inst_args(inst) - regular instruction arguments
    /// 2. branch_destination().args() - values passed to branch targets
    /// 3. exception_table().contexts() - exception contexts (not implemented yet)
    ///
    /// This iterator yields Values in the same order.
    pub fn instValues(self: *const Self, inst: Inst) InstValuesIterator {
        return InstValuesIterator.init(self, inst);
    }

    /// Get only the regular instruction arguments (not including branch args).
    /// Use instValues() to get all values including branch arguments.
    pub fn instArgsOnly(self: *const Self, inst: Inst) []const Value {
        return self.instArgs(inst);
    }

    /// Get the controlling type for a polymorphic instruction.
    pub fn ctrlType(self: *const Self, inst: Inst) Type {
        const data = self.getInst(inst);
        return switch (data.*) {
            .unary_imm => |d| d.imm_type(),
            .binary_imm64 => |d| d.imm_type(),
            else => Type.INVALID,
        };
    }

    // ------------------------------------------------------------------------
    // Value queries
    // ------------------------------------------------------------------------

    /// Get the number of values.
    pub fn numValues(self: *const Self) usize {
        return self.values.items.len;
    }

    /// Check if a value is "real" (not the reserved sentinel).
    pub fn valueIsReal(self: *const Self, v: Value) bool {
        return v.index != Value.RESERVED.index and self.valueIsValid(v);
    }

    /// Display an instruction for debugging.
    pub fn displayInst(self: *const Self, inst: Inst) []const u8 {
        _ = self;
        _ = inst;
        return "<inst>";
    }
};

// ============================================================================
// InstValuesIterator
// Port of cranelift/codegen/src/ir/dfg.rs:897-920 inst_values()
//
// Cranelift returns an iterator that chains:
// 1. inst_args(inst).iter().copied()
// 2. branch_destination().flat_map(|branch| branch.args().filter_map(|arg| arg.as_value()))
// 3. exception_table().flat_map(|et| exception_tables[et].contexts())
//
// We don't have exception tables yet, so we only implement #1 and #2.
// ============================================================================

/// Iterator over all values used by an instruction.
///
/// This includes:
/// - Regular instruction arguments (from instArgs)
/// - Branch destination arguments (values passed to target blocks)
///
/// This matches Cranelift's inst_values() behavior exactly.
pub const InstValuesIterator = struct {
    dfg: *const DataFlowGraph,
    inst: Inst,
    phase: Phase,
    // For args phase
    arg_index: usize,
    args_slice: []const Value,
    // For branch_args phase
    branch_dest_index: usize,
    branch_arg_index: usize,
    branch_destinations: []const BlockCall,

    const Phase = enum {
        args,
        branch_args,
        done,
    };

    const Self = @This();

    pub fn init(dfg: *const DataFlowGraph, inst: Inst) Self {
        const args_slice = dfg.instArgs(inst);
        const data = dfg.getInst(inst);
        const branch_destinations = data.branchDestination(&dfg.jump_tables);

        return .{
            .dfg = dfg,
            .inst = inst,
            .phase = .args,
            .arg_index = 0,
            .args_slice = args_slice,
            .branch_dest_index = 0,
            .branch_arg_index = 0,
            .branch_destinations = branch_destinations,
        };
    }

    /// Get the next value, or null if iteration is complete.
    pub fn next(self: *Self) ?Value {
        switch (self.phase) {
            .args => {
                // Phase 1: yield regular instruction arguments
                if (self.arg_index < self.args_slice.len) {
                    const v = self.args_slice[self.arg_index];
                    self.arg_index += 1;
                    return v;
                }
                // Transition to branch_args phase
                self.phase = .branch_args;
                return self.next();
            },
            .branch_args => {
                // Phase 2: yield branch destination arguments
                // Iterate through all branch destinations
                while (self.branch_dest_index < self.branch_destinations.len) {
                    const bc = self.branch_destinations[self.branch_dest_index];
                    const branch_args = self.dfg.value_lists.getSlice(bc.args);

                    if (self.branch_arg_index < branch_args.len) {
                        const v = branch_args[self.branch_arg_index];
                        self.branch_arg_index += 1;
                        return v;
                    }
                    // Move to next destination
                    self.branch_dest_index += 1;
                    self.branch_arg_index = 0;
                }
                // No more branch args
                self.phase = .done;
                return null;
            },
            .done => return null,
        }
    }

    /// Reset the iterator to the beginning.
    pub fn reset(self: *Self) void {
        self.phase = .args;
        self.arg_index = 0;
        self.branch_dest_index = 0;
        self.branch_arg_index = 0;
    }

    /// Collect all values into a slice (allocates).
    /// Useful when you need random access or need to iterate multiple times.
    pub fn collect(self: *Self, allocator: std.mem.Allocator) ![]Value {
        var result = std.ArrayList(Value).init(allocator);
        errdefer result.deinit();

        self.reset();
        while (self.next()) |v| {
            try result.append(v);
        }
        return result.toOwnedSlice();
    }

    /// Count the total number of values.
    pub fn count(self: *Self) usize {
        var n: usize = 0;
        self.reset();
        while (self.next()) |_| {
            n += 1;
        }
        self.reset();
        return n;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "entity formatting" {
    const testing = std.testing;
    var buf: [32]u8 = undefined;

    {
        const v = Value.fromIndex(42);
        var fbs = std.io.fixedBufferStream(&buf);
        try v.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("v42", fbs.getWritten());
    }

    {
        const block = Block.fromIndex(5);
        var fbs = std.io.fixedBufferStream(&buf);
        try block.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("block5", fbs.getWritten());
    }

    {
        const inst = Inst.fromIndex(10);
        var fbs = std.io.fixedBufferStream(&buf);
        try inst.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("inst10", fbs.getWritten());
    }
}

test "value list operations" {
    const testing = std.testing;

    var pool = ValueListPool.init(testing.allocator);
    defer pool.deinit();

    // Empty list
    try testing.expectEqual(@as(usize, 0), pool.len(ValueList.EMPTY));
    try testing.expectEqual(@as(usize, 0), pool.getSlice(ValueList.EMPTY).len);

    // Create a list
    const values = [_]Value{ Value.fromIndex(1), Value.fromIndex(2), Value.fromIndex(3) };
    const list = try pool.alloc(&values);

    try testing.expectEqual(@as(usize, 3), pool.len(list));

    const slice = pool.getSlice(list);
    try testing.expectEqual(@as(usize, 3), slice.len);
    try testing.expect(slice[0].eql(Value.fromIndex(1)));
    try testing.expect(slice[1].eql(Value.fromIndex(2)));
    try testing.expect(slice[2].eql(Value.fromIndex(3)));

    // Push to list
    const list2 = try pool.push(list, Value.fromIndex(4));
    try testing.expectEqual(@as(usize, 4), pool.len(list2));
}

test "dfg basic operations" {
    const testing = std.testing;

    var dfg = DataFlowGraph.init(testing.allocator);
    defer dfg.deinit();

    // Create a block
    const block0 = try dfg.makeBlock();
    try testing.expectEqual(@as(u32, 0), block0.index);
    try testing.expect(dfg.blockIsValid(block0));

    // Add block parameters
    const param0 = try dfg.appendBlockParam(block0, Type.I32);
    const param1 = try dfg.appendBlockParam(block0, Type.I64);

    try testing.expectEqual(@as(usize, 2), dfg.numBlockParams(block0));

    const params = dfg.blockParams(block0);
    try testing.expectEqual(@as(usize, 2), params.len);
    try testing.expect(params[0].eql(param0));
    try testing.expect(params[1].eql(param1));

    // Check value types
    try testing.expect(dfg.valueType(param0).eql(Type.I32));
    try testing.expect(dfg.valueType(param1).eql(Type.I64));

    // Check value definitions
    const def0 = dfg.valueDef(param0);
    try testing.expect(def0.block().?.eql(block0));
    try testing.expectEqual(@as(u16, 0), def0.num());

    const def1 = dfg.valueDef(param1);
    try testing.expect(def1.block().?.eql(block0));
    try testing.expectEqual(@as(u16, 1), def1.num());
}

test "value aliasing" {
    const testing = std.testing;

    var dfg = DataFlowGraph.init(testing.allocator);
    defer dfg.deinit();

    const block0 = try dfg.makeBlock();
    const v0 = try dfg.appendBlockParam(block0, Type.I32);
    const v1 = try dfg.appendBlockParam(block0, Type.I32);

    // Create alias: v1 -> v0
    dfg.changeToAliasOf(v1, v0);

    // Resolve alias
    const resolved = dfg.resolveAliases(v1);
    try testing.expect(resolved.eql(v0));
}

test "InstValuesIterator includes branch arguments" {
    // Port of cranelift inst_values() behavior verification
    // This test verifies that instValues() returns both regular args AND branch args
    const testing = std.testing;

    var dfg = DataFlowGraph.init(testing.allocator);
    defer dfg.deinit();

    // Create blocks
    const block0 = try dfg.makeBlock();
    const block1 = try dfg.makeBlock();

    // Create values that will be used
    const cond = try dfg.appendBlockParam(block0, Type.I32);
    const v1 = try dfg.appendBlockParam(block0, Type.I32);
    const v2 = try dfg.appendBlockParam(block0, Type.I32);

    // Add block param to block1 (the target)
    _ = try dfg.appendBlockParam(block1, Type.I32);

    // Create a brif instruction:
    // brif cond, block1(v1), block1(v2)
    // This has 1 regular arg (cond) + 2 branch args (v1, v2)
    const then_args = try dfg.value_lists.alloc(&[_]Value{v1});
    const else_args = try dfg.value_lists.alloc(&[_]Value{v2});

    const brif_data = InstructionData{
        .brif = .{
            .opcode = .brif,
            .arg = cond,
            .blocks = .{
                BlockCall{ .block = block1, .args = then_args },
                BlockCall{ .block = block1, .args = else_args },
            },
        },
    };

    const brif_inst = try dfg.makeInst(brif_data);

    // Test instArgs - should return only the condition (regular arg)
    const args_only = dfg.instArgs(brif_inst);
    try testing.expectEqual(@as(usize, 1), args_only.len);
    try testing.expect(args_only[0].eql(cond));

    // Test instValues - should return cond + v1 + v2 (all values)
    var iter = dfg.instValues(brif_inst);
    var collected: std.ArrayListUnmanaged(Value) = .{};
    defer collected.deinit(testing.allocator);

    while (iter.next()) |v| {
        try collected.append(testing.allocator, v);
    }

    // Should have 3 values: cond (regular), v1 (then branch), v2 (else branch)
    try testing.expectEqual(@as(usize, 3), collected.items.len);
    try testing.expect(collected.items[0].eql(cond)); // Regular arg first
    try testing.expect(collected.items[1].eql(v1)); // Then branch arg
    try testing.expect(collected.items[2].eql(v2)); // Else branch arg
}

test "InstValuesIterator for non-branch instruction" {
    // Verify that non-branch instructions work correctly
    const testing = std.testing;

    var dfg = DataFlowGraph.init(testing.allocator);
    defer dfg.deinit();

    const block0 = try dfg.makeBlock();
    const v0 = try dfg.appendBlockParam(block0, Type.I32);
    const v1 = try dfg.appendBlockParam(block0, Type.I32);

    // Create an iadd instruction: v0 + v1
    const iadd_data = InstructionData{
        .binary = .{
            .opcode = .iadd,
            .args = .{ v0, v1 },
        },
    };

    const iadd_inst = try dfg.makeInst(iadd_data);

    // Both instArgs and instValues should return the same for non-branch
    const args_only = dfg.instArgs(iadd_inst);
    try testing.expectEqual(@as(usize, 2), args_only.len);

    var iter = dfg.instValues(iadd_inst);
    var count: usize = 0;
    while (iter.next()) |v| {
        try testing.expect(v.eql(args_only[count]));
        count += 1;
    }
    try testing.expectEqual(@as(usize, 2), count);
}
