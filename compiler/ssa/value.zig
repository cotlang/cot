//! SSA Value representation - the result of an SSA operation.

const std = @import("std");
const types = @import("../core/types.zig");
const Op = @import("op.zig").Op;
const abi = @import("../codegen/native/abi.zig");

pub const ID = types.ID;
pub const INVALID_ID = types.INVALID_ID;
pub const TypeIndex = types.TypeIndex;
pub const Pos = types.Pos;

// SSA size limits
pub const MAX_STRUCT_FIELDS: usize = 4;
pub const MAX_SSA_SIZE: u32 = MAX_STRUCT_FIELDS * 8; // 32 bytes

pub const CondCode = enum(u8) { eq, ne, lt, le, gt, ge, ult, ule, ugt, uge };

pub const SymbolOff = struct { sym: ?*anyopaque = null, offset: i64 = 0 };

pub const Aux = union(enum) {
    none,
    string: []const u8,
    symbol: ?*anyopaque,
    symbol_off: SymbolOff,
    call: *AuxCall,
    type_ref: TypeIndex,
    cond: CondCode,
};

/// Auxiliary data for function calls.
/// Contains ABI information used by register allocation and native codegen.
/// Reference: Go's ssa/op.go:118-122, bootstrap-0.2/src/ssa/value.zig
pub const AuxCall = struct {
    fn_name: []const u8 = "",
    func_sym: ?*anyopaque = null,
    allocator: ?std.mem.Allocator = null,
    /// ABI information describing how parameters and results are passed.
    abi_info: ?*const abi.ABIParamResultInfo = null,
    // Register info computed lazily by regalloc
    inputs: []const u8 = &.{},
    outputs: []const u8 = &.{},
    clobbers: u64 = 0,

    pub fn init(alloc: std.mem.Allocator) AuxCall { return .{ .allocator = alloc }; }

    /// Check if this call uses hidden return pointer (>16B return on ARM64/AMD64)
    pub fn usesHiddenReturn(self: *const AuxCall) bool {
        if (self.abi_info) |info| {
            return info.uses_hidden_return;
        }
        return false;
    }

    /// Get the hidden return size (0 if not using hidden return)
    pub fn hiddenReturnSize(self: *const AuxCall) u32 {
        if (self.abi_info) |info| {
            return info.hidden_return_size;
        }
        return 0;
    }

    /// Get registers used for argument N
    pub fn regsOfArg(self: *const AuxCall, which: usize) []const abi.RegIndex {
        if (self.abi_info) |info| {
            return info.regsOfArg(which);
        }
        return &.{};
    }

    /// Get registers used for result N
    pub fn regsOfResult(self: *const AuxCall, which: usize) []const abi.RegIndex {
        if (self.abi_info) |info| {
            return info.regsOfResult(which);
        }
        return &.{};
    }

    /// Get stack offset for argument N
    pub fn offsetOfArg(self: *const AuxCall, which: usize) i32 {
        if (self.abi_info) |info| {
            return info.offsetOfArg(which);
        }
        return 0;
    }

    /// Get stack offset for result N
    pub fn offsetOfResult(self: *const AuxCall, which: usize) i32 {
        if (self.abi_info) |info| {
            return info.offsetOfResult(which);
        }
        return 0;
    }
};

/// SSA Value - represents a single operation's result.
pub const Value = struct {
    id: ID = INVALID_ID,
    op: Op = .invalid,
    type_idx: TypeIndex = 0,
    aux_int: i64 = 0,
    aux: Aux = .none,
    aux_call: ?*AuxCall = null,
    args: []*Value = &.{},
    args_storage: [3]*Value = undefined,
    args_dynamic: bool = false,
    args_capacity: usize = 0,
    block: ?*Block = null,
    pos: Pos = .{},
    uses: i32 = 0,
    in_cache: bool = false,
    next_free: ?*Value = null,
    // Register allocation result
    home: ?Location = null,

    pub fn init(id: ID, op: Op, type_idx: TypeIndex, block: ?*Block, pos: Pos) Value {
        return .{ .id = id, .op = op, .type_idx = type_idx, .block = block, .pos = pos };
    }

    pub fn addArg(self: *Value, arg: *Value) void {
        self.addArgAlloc(arg, null) catch @panic("addArg failed: need allocator for >3 args");
    }

    pub fn addArgAlloc(self: *Value, arg: *Value, allocator: ?std.mem.Allocator) !void {
        arg.uses += 1;
        const count = self.args.len;
        if (count < 3) {
            self.args_storage[count] = arg;
            self.args = self.args_storage[0 .. count + 1];
        } else if (!self.args_dynamic) {
            const alloc = allocator orelse return error.NeedAllocator;
            try self.transitionToDynamic(arg, alloc);
        } else {
            const alloc = allocator orelse return error.NeedAllocator;
            try self.growDynamicArgs(arg, alloc);
        }
    }

    fn transitionToDynamic(self: *Value, arg: *Value, allocator: std.mem.Allocator) !void {
        const new_cap: usize = 8;
        const new_args = try allocator.alloc(*Value, new_cap);
        @memcpy(new_args[0..3], self.args_storage[0..3]);
        new_args[3] = arg;
        self.args = new_args[0..4];
        self.args_capacity = new_cap;
        self.args_dynamic = true;
    }

    fn growDynamicArgs(self: *Value, arg: *Value, allocator: std.mem.Allocator) !void {
        const old_len = self.args.len;
        const old_cap = self.args_capacity;
        if (old_len < old_cap) {
            const full_slice = self.args.ptr[0..old_cap];
            full_slice[old_len] = arg;
            self.args = full_slice[0 .. old_len + 1];
        } else {
            const new_cap = if (old_cap == 0) 8 else old_cap * 2;
            const new_args = try allocator.alloc(*Value, new_cap);
            @memcpy(new_args[0..old_len], self.args);
            new_args[old_len] = arg;
            if (old_cap > 0) allocator.free(self.args.ptr[0..old_cap]);
            self.args = new_args[0 .. old_len + 1];
            self.args_capacity = new_cap;
        }
    }

    pub fn addArg2(self: *Value, arg0: *Value, arg1: *Value) void {
        self.addArg(arg0);
        self.addArg(arg1);
    }

    pub fn addArg3(self: *Value, arg0: *Value, arg1: *Value, arg2: *Value) void {
        self.addArg(arg0);
        self.addArg(arg1);
        self.addArg(arg2);
    }

    pub fn setArg(self: *Value, idx: usize, new_arg: *Value) void {
        if (idx < self.args.len) self.args[idx].uses -= 1;
        new_arg.uses += 1;
        self.args[idx] = new_arg;
    }

    pub fn resetArgs(self: *Value) void {
        for (self.args) |arg| arg.uses -= 1;
        self.args = &.{};
        self.args_dynamic = false;
        self.args_capacity = 0;
    }

    pub fn resetArgsFree(self: *Value, allocator: ?std.mem.Allocator) void {
        for (self.args) |arg| arg.uses -= 1;
        if (self.args_dynamic and self.args_capacity > 0) {
            if (allocator) |alloc| alloc.free(self.args.ptr[0..self.args_capacity]);
        }
        self.args = &.{};
        self.args_dynamic = false;
        self.args_capacity = 0;
    }

    pub fn argsLen(self: *const Value) usize { return self.args.len; }
    pub fn isConst(self: *const Value) bool {
        return switch (self.op) { .const_int, .const_bool, .const_nil, .const_string, .const_float => true, else => false };
    }
    pub fn isRematerializable(self: *const Value) bool { return self.op.info().rematerializable; }
    pub fn hasSideEffects(self: *const Value) bool { return self.op.info().has_side_effects; }
    pub fn readsMemory(self: *const Value) bool { return self.op.info().reads_memory; }
    pub fn writesMemory(self: *const Value) bool { return self.op.info().writes_memory; }
    pub fn memoryArg(self: *const Value) ?*Value {
        if (!self.readsMemory() or self.args.len == 0) return null;
        return self.args[self.args.len - 1];
    }

    /// Get the assigned home location (register or stack slot).
    /// This is set by the register allocator via func.setHome().
    /// Pattern from Go's SSA: value.getHome() delegates to func.reg_alloc[id].
    pub fn getHome(self: *const Value) ?Location {
        const b = self.block orelse return self.home;
        return b.func.getHome(self.id) orelse self.home;
    }

    pub fn getReg(self: *const Value) u8 {
        const loc = self.getHome() orelse @panic("Value.getReg: no location");
        return switch (loc) { .register => |r| r, .stack => @panic("Value.getReg: in stack") };
    }
    pub fn regOrNull(self: *const Value) ?u8 {
        const loc = self.getHome() orelse return null;
        return switch (loc) { .register => |r| r, .stack => null };
    }
    pub fn hasReg(self: *const Value) bool {
        const loc = self.getHome() orelse return false;
        return loc == .register;
    }

    pub fn format(self: *const Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("v{d} = {s}", .{ self.id, @tagName(self.op) });
        if (self.args.len > 0) {
            try writer.writeAll(" ");
            for (self.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("v{d}", .{arg.id});
            }
        }
        if (self.aux_int != 0) try writer.print(" [{d}]", .{self.aux_int});
    }
};

pub const Location = union(enum) {
    register: u8,
    stack: i32,

    pub fn reg(self: Location) u8 { return switch (self) { .register => |r| r, .stack => @panic("not a register") }; }
    pub fn isReg(self: Location) bool { return self == .register; }
    pub fn isStack(self: Location) bool { return self == .stack; }
    pub fn stackOffset(self: Location) i32 { return switch (self) { .stack => |s| s, .register => @panic("not a stack slot") }; }
};

pub const Block = @import("block.zig").Block;

// ============================================================================
// Tests
// ============================================================================

test "Value creation" {
    var v = Value.init(1, .const_int, 0, null, .{});
    v.aux_int = 42;
    try std.testing.expectEqual(@as(ID, 1), v.id);
    try std.testing.expectEqual(Op.const_int, v.op);
    try std.testing.expectEqual(@as(i64, 42), v.aux_int);
}

test "Value use count tracking" {
    var v1 = Value.init(1, .const_int, 0, null, .{});
    var v2 = Value.init(2, .const_int, 0, null, .{});
    var v3 = Value.init(3, .add, 0, null, .{});
    v3.addArg(&v1);
    v3.addArg(&v2);
    try std.testing.expectEqual(@as(i32, 1), v1.uses);
    try std.testing.expectEqual(@as(i32, 1), v2.uses);
    try std.testing.expectEqual(@as(usize, 2), v3.argsLen());
    v3.resetArgs();
    try std.testing.expectEqual(@as(i32, 0), v1.uses);
    try std.testing.expectEqual(@as(i32, 0), v2.uses);
}

test "Value setArg replaces correctly" {
    var v1 = Value.init(1, .const_int, 0, null, .{});
    var v2 = Value.init(2, .const_int, 0, null, .{});
    var v3 = Value.init(3, .add, 0, null, .{});
    v3.addArg(&v1);
    v3.setArg(0, &v2);
    try std.testing.expectEqual(@as(i32, 0), v1.uses);
    try std.testing.expectEqual(@as(i32, 1), v2.uses);
}

test "Value isConst" {
    var v1 = Value.init(1, .const_int, 0, null, .{});
    var v2 = Value.init(2, .add, 0, null, .{});
    try std.testing.expect(v1.isConst());
    try std.testing.expect(!v2.isConst());
}

test "Value dynamic argument allocation" {
    const allocator = std.testing.allocator;
    var args: [6]Value = undefined;
    for (&args, 0..) |*arg, i| arg.* = Value.init(@intCast(i + 1), .const_int, 0, null, .{});

    var phi = Value.init(10, .phi, 0, null, .{});
    try phi.addArgAlloc(&args[0], allocator);
    try phi.addArgAlloc(&args[1], allocator);
    try phi.addArgAlloc(&args[2], allocator);
    try std.testing.expect(!phi.args_dynamic);
    try phi.addArgAlloc(&args[3], allocator);
    try std.testing.expect(phi.args_dynamic);
    try phi.addArgAlloc(&args[4], allocator);
    try phi.addArgAlloc(&args[5], allocator);
    try std.testing.expectEqual(@as(usize, 6), phi.argsLen());

    phi.resetArgsFree(allocator);
    try std.testing.expectEqual(@as(usize, 0), phi.argsLen());
    for (&args) |*arg| try std.testing.expectEqual(@as(i32, 0), arg.uses);
}
