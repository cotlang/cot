//! Data flow graph tracking Instructions, Values, and Blocks.
//!
//! Port of cranelift/codegen/src/ir/dfg.rs
//! Port of cranelift/codegen/src/ir/entities.rs

const std = @import("std");
const types = @import("types.zig");

const Type = types.Type;

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

    /// Get the length of a list.
    pub fn len(self: *const Self, list: ValueList) usize {
        if (list.isEmpty()) return 0;
        return self.data.items[list.base];
    }

    /// Push a value to an existing list (creates a new list).
    pub fn push(self: *Self, list: ValueList, value: Value) !ValueList {
        const old_values = self.getSlice(list);
        const base: u32 = @intCast(self.data.items.len);

        try self.data.append(self.allocator, @intCast(old_values.len + 1));

        for (old_values) |v| {
            try self.data.append(self.allocator, v.index);
        }
        try self.data.append(self.allocator, value.index);

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
// Instruction Data (simplified storage for DFG)
// ============================================================================

/// Opcode - instruction operation type.
/// This is a simplified version for DFG; full opcodes are in instructions.zig.
pub const Opcode = @import("instructions.zig").Opcode;

/// Simplified instruction data storage for the DFG.
/// Full InstructionData with all formats is in builder.zig.
pub const InstData = struct {
    /// Instruction opcode.
    opcode: Opcode,
    /// Instruction arguments (input values).
    args: ValueList,
    /// Controlling type variable (for polymorphic instructions).
    ctrl_type: Type,

    pub const EMPTY: InstData = .{
        .opcode = .nop,
        .args = ValueList.EMPTY,
        .ctrl_type = Type.INVALID,
    };
};

// ============================================================================
// Data Flow Graph
// Port of cranelift/codegen/src/ir/dfg.rs DataFlowGraph
// ============================================================================

/// A data flow graph defines all instructions and basic blocks in a function as well as
/// the data flow dependencies between them.
pub const DataFlowGraph = struct {
    allocator: std.mem.Allocator,

    /// Storage for values.
    values: std.ArrayListUnmanaged(ValueData),

    /// Storage for blocks.
    blocks: std.ArrayListUnmanaged(BlockData),

    /// Storage for instruction data (opcode, args).
    insts: std.ArrayListUnmanaged(InstData),

    /// Storage for instruction results (value lists per instruction).
    results: std.ArrayListUnmanaged(ValueList),

    /// Memory pool for value lists.
    value_lists: ValueListPool,

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
            .inst_count = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
        self.results.deinit(self.allocator);
        self.value_lists.deinit();
    }

    pub fn clear(self: *Self) void {
        self.values.clearRetainingCapacity();
        self.blocks.clearRetainingCapacity();
        self.insts.clearRetainingCapacity();
        self.results.clearRetainingCapacity();
        self.value_lists.clear();
        self.inst_count = 0;
    }

    /// Create a new instruction ID and reserve storage.
    pub fn makeInst(self: *Self) Inst {
        const index = self.inst_count;
        self.inst_count += 1;
        // Ensure storage
        while (self.insts.items.len <= index) {
            self.insts.append(self.allocator, InstData.EMPTY) catch unreachable;
        }
        return Inst.fromIndex(index);
    }

    /// Create a new instruction with data.
    pub fn makeInstWithData(self: *Self, data: InstData) !Inst {
        const index = self.inst_count;
        self.inst_count += 1;
        while (self.insts.items.len < index) {
            try self.insts.append(self.allocator, InstData.EMPTY);
        }
        try self.insts.append(self.allocator, data);
        return Inst.fromIndex(index);
    }

    /// Set the instruction data for an existing instruction.
    pub fn setInstData(self: *Self, inst: Inst, data: InstData) !void {
        while (self.insts.items.len <= inst.index) {
            try self.insts.append(self.allocator, InstData.EMPTY);
        }
        self.insts.items[inst.index] = data;
    }

    /// Get the instruction data.
    pub fn getInstData(self: *const Self, inst: Inst) InstData {
        if (inst.index >= self.insts.items.len) return InstData.EMPTY;
        return self.insts.items[inst.index];
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

    // ------------------------------------------------------------------------
    // Instructions
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
    // Instruction data queries (for machinst integration)
    // ------------------------------------------------------------------------

    /// Get the instruction opcode.
    pub fn instOpcode(self: *const Self, inst: Inst) Opcode {
        return self.getInstData(inst).opcode;
    }

    /// Get the instruction arguments (input values).
    pub fn instArgs(self: *const Self, inst: Inst) []const Value {
        const data = self.getInstData(inst);
        return self.value_lists.getSlice(data.args);
    }

    /// Get the instruction input values (alias for instArgs).
    pub fn instValues(self: *const Self, inst: Inst) []const Value {
        return self.instArgs(inst);
    }

    /// Get the controlling type for a polymorphic instruction.
    pub fn ctrlType(self: *const Self, inst: Inst) Type {
        return self.getInstData(inst).ctrl_type;
    }

    // ------------------------------------------------------------------------
    // Value queries (for machinst integration)
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
