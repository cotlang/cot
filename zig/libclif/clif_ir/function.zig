//! Intermediate representation of a function.
//!
//! Port of cranelift/codegen/src/ir/function.rs
//! Port of cranelift/codegen/src/ir/extfunc.rs
//! Port of cranelift/codegen/src/ir/stackslot.rs
//! Port of cranelift/codegen/src/isa/call_conv.rs
//!
//! The `Function` struct owns all of its basic blocks and instructions.

const std = @import("std");
const dfg_mod = @import("dfg.zig");
const layout_mod = @import("layout.zig");
const types = @import("types.zig");

pub const DataFlowGraph = dfg_mod.DataFlowGraph;
pub const Layout = layout_mod.Layout;
pub const Block = dfg_mod.Block;
pub const Inst = dfg_mod.Inst;
pub const Value = dfg_mod.Value;
pub const Type = types.Type;
pub const SigRef = dfg_mod.SigRef;
pub const FuncRef = dfg_mod.FuncRef;
pub const StackSlot = dfg_mod.StackSlot;
pub const JumpTable = dfg_mod.JumpTable;
pub const JumpTableData = dfg_mod.JumpTableData;
pub const BlockCall = dfg_mod.BlockCall;
pub const GlobalValue = dfg_mod.GlobalValue;

// Import GlobalValueData from globalvalue.zig
const globalvalue_mod = @import("globalvalue.zig");
pub const GlobalValueData = globalvalue_mod.GlobalValueData;

// ============================================================================
// Calling Convention
// Port of cranelift/codegen/src/isa/call_conv.rs
// ============================================================================

/// Calling convention identifiers.
pub const CallConv = enum {
    /// Best performance, not ABI-stable.
    fast,
    /// Supports tail calls, not ABI-stable.
    tail,
    /// System V-style convention used on many platforms.
    system_v,
    /// Windows "fastcall" convention, also used for x64 and ARM.
    windows_fastcall,
    /// Mac aarch64 calling convention.
    apple_aarch64,
    /// Specialized convention for the probestack function.
    probestack,
    /// The winch calling convention, not ABI-stable.
    winch,
    /// Calling convention optimized for callsite efficiency.
    preserve_all,

    const Self = @This();

    /// Does this calling convention support tail calls?
    pub fn supportsTailCalls(self: Self) bool {
        return self == .tail;
    }

    /// Get string representation.
    pub fn toStr(self: Self) []const u8 {
        return switch (self) {
            .fast => "fast",
            .tail => "tail",
            .system_v => "system_v",
            .windows_fastcall => "windows_fastcall",
            .apple_aarch64 => "apple_aarch64",
            .probestack => "probestack",
            .winch => "winch",
            .preserve_all => "preserve_all",
        };
    }
};

// ============================================================================
// Argument Extension
// Port of cranelift/codegen/src/ir/extfunc.rs ArgumentExtension
// ============================================================================

/// Function argument extension options.
///
/// On some architectures, small integer function arguments are extended to
/// the width of a general-purpose register.
pub const ArgumentExtension = enum {
    /// No extension, high bits are indeterminate.
    none,
    /// Unsigned extension: high bits in register are 0.
    uext,
    /// Signed extension: high bits in register replicate sign bit.
    sext,

    pub fn toStr(self: ArgumentExtension) []const u8 {
        return switch (self) {
            .none => "",
            .uext => "uext",
            .sext => "sext",
        };
    }
};

// ============================================================================
// Argument Purpose
// Port of cranelift/codegen/src/ir/extfunc.rs ArgumentPurpose
// ============================================================================

/// The special purpose of a function argument.
pub const ArgumentPurpose = union(enum) {
    /// A normal user program value.
    normal,
    /// A C struct passed as argument.
    struct_argument: u32, // size in bytes
    /// Struct return pointer.
    struct_return,
    /// A VM context pointer.
    vmctx,

    pub fn toStr(self: ArgumentPurpose) []const u8 {
        return switch (self) {
            .normal => "normal",
            .struct_argument => "sarg",
            .struct_return => "sret",
            .vmctx => "vmctx",
        };
    }

    pub fn isNormal(self: ArgumentPurpose) bool {
        return self == .normal;
    }
};

// ============================================================================
// ABI Parameter
// Port of cranelift/codegen/src/ir/extfunc.rs AbiParam
// ============================================================================

/// Function parameter or return value descriptor.
pub const AbiParam = struct {
    /// Type of the argument value.
    value_type: Type,
    /// Special purpose of argument, or normal.
    purpose: ArgumentPurpose = .normal,
    /// Method for extending argument to a full register.
    extension: ArgumentExtension = .none,

    const Self = @This();

    /// Create a parameter with default flags.
    pub fn init(vt: Type) Self {
        return .{
            .value_type = vt,
            .purpose = .normal,
            .extension = .none,
        };
    }

    /// Create a special-purpose parameter.
    pub fn special(vt: Type, purpose: ArgumentPurpose) Self {
        return .{
            .value_type = vt,
            .purpose = purpose,
            .extension = .none,
        };
    }

    /// Convert to a parameter with uext flag set.
    pub fn uext(self: Self) Self {
        return .{
            .value_type = self.value_type,
            .purpose = self.purpose,
            .extension = .uext,
        };
    }

    /// Convert to a parameter with sext flag set.
    pub fn sext(self: Self) Self {
        return .{
            .value_type = self.value_type,
            .purpose = self.purpose,
            .extension = .sext,
        };
    }
};

// ============================================================================
// Signature
// Port of cranelift/codegen/src/ir/extfunc.rs Signature
// ============================================================================

/// Function signature.
///
/// The function signature describes the types of formal parameters and return values
/// along with other details needed to call a function correctly.
pub const Signature = struct {
    /// The arguments passed to the function.
    params: std.ArrayListUnmanaged(AbiParam) = .{},
    /// Values returned from the function.
    returns: std.ArrayListUnmanaged(AbiParam) = .{},
    /// Calling convention.
    call_conv: CallConv = .fast,

    const Self = @This();

    /// Create a new blank signature.
    pub fn init(call_conv: CallConv) Self {
        return .{
            .params = .{},
            .returns = .{},
            .call_conv = call_conv,
        };
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.params.deinit(allocator);
        self.returns.deinit(allocator);
    }

    /// Clear the signature.
    pub fn clear(self: *Self, call_conv: CallConv) void {
        self.params.clearRetainingCapacity();
        self.returns.clearRetainingCapacity();
        self.call_conv = call_conv;
    }

    /// Add a parameter.
    pub fn addParam(self: *Self, allocator: std.mem.Allocator, param: AbiParam) !void {
        try self.params.append(allocator, param);
    }

    /// Add a return value.
    pub fn addReturn(self: *Self, allocator: std.mem.Allocator, ret: AbiParam) !void {
        try self.returns.append(allocator, ret);
    }

    /// Find the index of a special-purpose parameter.
    pub fn specialParamIndex(self: Self, purpose: ArgumentPurpose) ?usize {
        // Search from end (rposition in Rust)
        var i = self.params.items.len;
        while (i > 0) {
            i -= 1;
            if (std.meta.eql(self.params.items[i].purpose, purpose)) {
                return i;
            }
        }
        return null;
    }

    /// Does this signature have a parameter with the given purpose?
    pub fn usesSpecialParam(self: Self, purpose: ArgumentPurpose) bool {
        return self.specialParamIndex(purpose) != null;
    }

    /// Does this signature use struct return?
    pub fn usesStructReturnParam(self: Self) bool {
        return self.usesSpecialParam(.struct_return);
    }

    /// How many special parameters does this function have?
    pub fn numSpecialParams(self: Self) usize {
        var count: usize = 0;
        for (self.params.items) |p| {
            if (!p.purpose.isNormal()) count += 1;
        }
        return count;
    }

    /// Does this return more than one normal value?
    pub fn isMultiReturn(self: Self) bool {
        var count: usize = 0;
        for (self.returns.items) |r| {
            if (r.purpose.isNormal()) count += 1;
        }
        return count > 1;
    }
};

// ============================================================================
// Stack Slot
// Port of cranelift/codegen/src/ir/stackslot.rs
// ============================================================================

/// The size of an object on the stack.
pub const StackSize = u32;

/// The kind of a stack slot.
pub const StackSlotKind = enum {
    /// An explicit stack slot for stack_load/stack_store.
    explicit_slot,
    /// An explicit stack slot for dynamic vector types.
    explicit_dynamic_slot,

    pub fn toStr(self: StackSlotKind) []const u8 {
        return switch (self) {
            .explicit_slot => "explicit_slot",
            .explicit_dynamic_slot => "explicit_dynamic_slot",
        };
    }
};

/// Contents of a stack slot.
pub const StackSlotData = struct {
    /// The kind of stack slot.
    kind: StackSlotKind,
    /// Size of stack slot in bytes.
    size: StackSize,
    /// Alignment as power-of-two exponent (log2 value).
    align_shift: u8,

    const Self = @This();

    /// Create a stack slot with the specified byte size and alignment.
    pub fn init(kind: StackSlotKind, size: StackSize, align_shift: u8) Self {
        return .{
            .kind = kind,
            .size = size,
            .align_shift = align_shift,
        };
    }

    /// Create an explicit slot.
    pub fn explicit(size: StackSize, align_shift: u8) Self {
        return init(.explicit_slot, size, align_shift);
    }

    /// Get alignment in bytes.
    pub fn alignment(self: Self) u32 {
        return @as(u32, 1) << @intCast(self.align_shift);
    }
};

// ============================================================================
// External Function
// Port of cranelift/codegen/src/ir/extfunc.rs ExtFuncData
// ============================================================================

/// External function name.
pub const ExternalName = union(enum) {
    /// User-defined name.
    user: struct {
        namespace: u32,
        index: u32,
    },
    /// Library call.
    libcall: []const u8,

    pub fn initUser(namespace: u32, index: u32) ExternalName {
        return .{ .user = .{ .namespace = namespace, .index = index } };
    }

    pub fn initLibcall(name: []const u8) ExternalName {
        return .{ .libcall = name };
    }
};

/// An external function.
pub const ExtFuncData = struct {
    /// Name of the external function.
    name: ExternalName,
    /// Call signature of function.
    signature: SigRef,
    /// Is function defined nearby (can use direct call)?
    colocated: bool,

    const Self = @This();

    pub fn init(name: ExternalName, signature: SigRef, colocated: bool) Self {
        return .{
            .name = name,
            .signature = signature,
            .colocated = colocated,
        };
    }
};

// ============================================================================
// Function
// Port of cranelift/codegen/src/ir/function.rs
// ============================================================================

/// Intermediate representation of a function.
///
/// Port of cranelift/codegen/src/ir/function.rs
pub const Function = struct {
    /// Name of this function.
    name: []const u8,

    /// Signature of this function.
    signature: Signature,

    /// Sized stack slots allocated in this function.
    stack_slots: std.ArrayListUnmanaged(StackSlotData),

    /// Data flow graph containing instructions, blocks, and values.
    dfg: DataFlowGraph,

    /// Layout of blocks and instructions in the function body.
    layout: Layout,

    /// External functions that can be called.
    ext_funcs: std.ArrayListUnmanaged(ExtFuncData),

    /// Imported signatures for indirect calls.
    signatures: std.ArrayListUnmanaged(Signature),

    /// Global value definitions.
    ///
    /// Global values can represent various kinds of addresses that are computed
    /// at runtime: VM context pointers, addresses loaded from other global values,
    /// offsets from global values, and symbolic references resolved by the linker.
    ///
    /// Port of cranelift/codegen/src/ir/function.rs:174
    global_values: std.ArrayListUnmanaged(GlobalValueData),

    /// Source file byte offsets indexed by instruction. Populated by ssa_to_clif.zig
    /// from SSA Value.pos.line (which stores the source byte offset). Used by
    /// machinst/lower.zig to emit source maps for crash diagnostics.
    /// 0 = no source info for this instruction.
    source_offsets: std.ArrayListUnmanaged(u32) = .{},

    const Self = @This();

    /// Create a new empty function.
    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .name = "",
            .signature = .{},
            .stack_slots = .{},
            .dfg = DataFlowGraph.init(allocator),
            .layout = .{},
            .ext_funcs = .{},
            .signatures = .{},
            .global_values = .{},
        };
    }

    /// Create a function with the given name and signature.
    pub fn withNameSignature(allocator: std.mem.Allocator, name: []const u8, sig: Signature) Self {
        return .{
            .name = name,
            .signature = sig,
            .stack_slots = .{},
            .dfg = DataFlowGraph.init(allocator),
            .layout = .{},
            .ext_funcs = .{},
            .signatures = .{},
            .global_values = .{},
        };
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        const allocator = self.dfg.allocator;
        self.signature.deinit(allocator);
        self.stack_slots.deinit(allocator);
        self.dfg.deinit();
        self.layout.deinit(allocator);
        self.ext_funcs.deinit(allocator);
        for (self.signatures.items) |*sig| {
            sig.deinit(allocator);
        }
        self.signatures.deinit(allocator);
        self.global_values.deinit(allocator);
    }

    /// Clear all data structures.
    pub fn clear(self: *Self) void {
        self.name = "";
        self.signature.clear(.fast);
        self.stack_slots.clearRetainingCapacity();
        self.dfg.clear();
        self.layout.clear();
        self.ext_funcs.clearRetainingCapacity();
        self.signatures.clearRetainingCapacity();
        self.global_values.clearRetainingCapacity();
    }

    /// Get the entry block, or null if function is empty.
    pub fn entryBlock(self: Self) ?Block {
        return self.layout.entryBlock();
    }

    /// Create a sized stack slot.
    pub fn createStackSlot(self: *Self, allocator: std.mem.Allocator, data: StackSlotData) !StackSlot {
        const idx = self.stack_slots.items.len;
        try self.stack_slots.append(allocator, data);
        return StackSlot.fromIndex(@intCast(idx));
    }

    /// Import a signature for indirect calls.
    pub fn importSignature(self: *Self, allocator: std.mem.Allocator, sig: Signature) !SigRef {
        const idx = self.signatures.items.len;
        try self.signatures.append(allocator, sig);
        return SigRef.fromIndex(@intCast(idx));
    }

    /// Import an external function.
    pub fn importFunction(self: *Self, allocator: std.mem.Allocator, data: ExtFuncData) !FuncRef {
        const idx = self.ext_funcs.items.len;
        try self.ext_funcs.append(allocator, data);
        return FuncRef.fromIndex(@intCast(idx));
    }

    /// Get total size occupied by all stack slots.
    pub fn fixedStackSize(self: Self) u32 {
        var total: u32 = 0;
        for (self.stack_slots.items) |ss| {
            total += ss.size;
        }
        return total;
    }

    /// Find a special-purpose block parameter value.
    pub fn specialParam(self: Self, purpose: ArgumentPurpose) ?Value {
        const entry = self.layout.entryBlock() orelse return null;
        const index = self.signature.specialParamIndex(purpose) orelse return null;
        const params = self.dfg.blockParams(entry);
        if (index >= params.len) return null;
        return params[index];
    }

    /// Get the signature for a SigRef.
    pub fn getSignature(self: Self, sig_ref: SigRef) ?*const Signature {
        const idx = sig_ref.asU32();
        if (idx >= self.signatures.items.len) return null;
        return &self.signatures.items[idx];
    }

    /// Get the external function data for a FuncRef.
    pub fn getExtFunc(self: Self, func_ref: FuncRef) ?*const ExtFuncData {
        const idx = func_ref.asU32();
        if (idx >= self.ext_funcs.items.len) return null;
        return &self.ext_funcs.items[idx];
    }

    /// Get the stack slot data for a StackSlot.
    pub fn getStackSlot(self: Self, slot: StackSlot) ?*const StackSlotData {
        const idx = slot.asU32();
        if (idx >= self.stack_slots.items.len) return null;
        return &self.stack_slots.items[idx];
    }

    /// Create a jump table with the specified data.
    ///
    /// Port of cranelift/codegen/src/ir/function.rs:233-236
    pub fn createJumpTable(self: *Self, data: JumpTableData) !JumpTable {
        return self.dfg.jump_tables.create(data);
    }

    /// Get a jump table by reference.
    pub fn getJumpTable(self: Self, jt: JumpTable) ?*const JumpTableData {
        return self.dfg.jump_tables.get(jt);
    }

    // ========================================================================
    // Global Values
    // Port of cranelift/codegen/src/ir/function.rs global value methods
    // ========================================================================

    /// Create a new global value with the specified data.
    ///
    /// Returns a GlobalValue reference that can be used with the `global_value`
    /// instruction to compute the value at runtime.
    ///
    /// Port of cranelift/codegen/src/ir/function.rs:180-183
    pub fn createGlobalValue(self: *Self, data: GlobalValueData) !GlobalValue {
        const allocator = self.dfg.allocator;
        const idx: u32 = @intCast(self.global_values.items.len);
        try self.global_values.append(allocator, data);
        return GlobalValue.fromIndex(idx);
    }

    /// Get the data for a global value.
    ///
    /// Port of cranelift/codegen/src/ir/function.rs:185-187
    pub fn getGlobalValueData(self: Self, gv: GlobalValue) GlobalValueData {
        return self.global_values.items[gv.asU32()];
    }

    /// Check if a global value is valid.
    pub fn globalValueIsValid(self: Self, gv: GlobalValue) bool {
        return gv.asU32() < self.global_values.items.len;
    }

    /// Get the number of global values.
    pub fn numGlobalValues(self: Self) usize {
        return self.global_values.items.len;
    }

    // ========================================================================
    // Debug Utilities
    // D3: Layout vs DFG comparison utility for diagnosing block insertion issues
    // ========================================================================

    /// Result of comparing Layout to DFG blocks.
    pub const LayoutDfgComparison = struct {
        dfg_block_count: usize,
        layout_block_count: usize,
        blocks_in_dfg_not_layout: std.ArrayListUnmanaged(Block),
        entry_block: ?Block,
        is_consistent: bool,

        pub fn deinit(self: *LayoutDfgComparison, allocator: std.mem.Allocator) void {
            self.blocks_in_dfg_not_layout.deinit(allocator);
        }
    };

    /// Compare Layout blocks to DFG blocks and identify discrepancies.
    /// This is critical for diagnosing the 0-byte emission bug where blocks
    /// exist in DFG but not in Layout.
    pub fn compareLayoutToDfg(self: Self) LayoutDfgComparison {
        const allocator = self.dfg.allocator;
        var result = LayoutDfgComparison{
            .dfg_block_count = self.dfg.blocks.items.len,
            .layout_block_count = 0,
            .blocks_in_dfg_not_layout = .{},
            .entry_block = self.layout.entryBlock(),
            .is_consistent = true,
        };

        // Count blocks in Layout
        var layout_iter = self.layout.blocks();
        while (layout_iter.next()) |_| {
            result.layout_block_count += 1;
        }

        // Find blocks in DFG but not in Layout
        for (self.dfg.blocks.items, 0..) |_, idx| {
            const block = Block.fromIndex(@intCast(idx));
            if (!self.layout.isBlockInserted(block)) {
                result.blocks_in_dfg_not_layout.append(allocator, block) catch {};
                result.is_consistent = false;
            }
        }

        return result;
    }

    /// Log Layout vs DFG comparison to debug output.
    pub fn logLayoutComparison(self: Self, comptime pipeline_debug: type) void {
        var comparison = self.compareLayoutToDfg();
        defer comparison.deinit(self.dfg.allocator);

        pipeline_debug.log(.codegen, "=== Layout vs DFG Comparison ===", .{});
        pipeline_debug.log(.codegen, "  DFG blocks: {d}", .{comparison.dfg_block_count});
        pipeline_debug.log(.codegen, "  Layout blocks: {d}", .{comparison.layout_block_count});

        if (comparison.entry_block) |entry| {
            pipeline_debug.log(.codegen, "  Entry block: {d}", .{entry.index});
        } else {
            pipeline_debug.log(.codegen, "  Entry block: NULL (CRITICAL!)", .{});
        }

        if (comparison.blocks_in_dfg_not_layout.items.len > 0) {
            pipeline_debug.log(.codegen, "  Blocks in DFG but NOT in Layout:", .{});
            for (comparison.blocks_in_dfg_not_layout.items) |block| {
                pipeline_debug.log(.codegen, "    - Block {d}", .{block.index});
            }
        }

        if (comparison.is_consistent) {
            pipeline_debug.log(.codegen, "  Status: CONSISTENT", .{});
        } else {
            pipeline_debug.log(.codegen, "  Status: INCONSISTENT - blocks missing from Layout!", .{});
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "signature creation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var sig = Signature.init(.system_v);
    defer sig.deinit(allocator);

    try sig.addParam(allocator, AbiParam.init(Type.I32));
    try sig.addParam(allocator, AbiParam.init(Type.I64));
    try sig.addReturn(allocator, AbiParam.init(Type.I32));

    try testing.expectEqual(@as(usize, 2), sig.params.items.len);
    try testing.expectEqual(@as(usize, 1), sig.returns.items.len);
    try testing.expectEqual(CallConv.system_v, sig.call_conv);
}

test "abi param extensions" {
    const testing = std.testing;

    const param = AbiParam.init(Type.I8);
    try testing.expectEqual(ArgumentExtension.none, param.extension);

    const uext_param = param.uext();
    try testing.expectEqual(ArgumentExtension.uext, uext_param.extension);

    const sext_param = param.sext();
    try testing.expectEqual(ArgumentExtension.sext, sext_param.extension);
}

test "stack slot data" {
    const testing = std.testing;

    const slot = StackSlotData.explicit(16, 3);
    try testing.expectEqual(StackSlotKind.explicit_slot, slot.kind);
    try testing.expectEqual(@as(StackSize, 16), slot.size);
    try testing.expectEqual(@as(u8, 3), slot.align_shift);
    try testing.expectEqual(@as(u32, 8), slot.alignment());
}

test "function creation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    // Add a stack slot
    const slot = try func.createStackSlot(allocator, StackSlotData.explicit(8, 2));
    try testing.expectEqual(@as(u32, 0), slot.asU32());
    try testing.expectEqual(@as(u32, 8), func.fixedStackSize());

    // Add another
    _ = try func.createStackSlot(allocator, StackSlotData.explicit(16, 3));
    try testing.expectEqual(@as(u32, 24), func.fixedStackSize());
}

test "special params" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var sig = Signature.init(.fast);
    defer sig.deinit(allocator);

    try sig.addParam(allocator, AbiParam.init(Type.I64));
    try sig.addParam(allocator, AbiParam.special(Type.I64, .vmctx));
    try sig.addParam(allocator, AbiParam.init(Type.I32));

    try testing.expect(sig.usesSpecialParam(.vmctx));
    try testing.expect(!sig.usesSpecialParam(.struct_return));
    try testing.expectEqual(@as(?usize, 1), sig.specialParamIndex(.vmctx));
    try testing.expectEqual(@as(usize, 1), sig.numSpecialParams());
}

test "call conv" {
    const testing = std.testing;

    try testing.expect(!CallConv.fast.supportsTailCalls());
    try testing.expect(CallConv.tail.supportsTailCalls());
    try testing.expectEqualStrings("system_v", CallConv.system_v.toStr());
}
