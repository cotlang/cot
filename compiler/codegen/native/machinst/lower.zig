//! This module implements lowering (instruction selection) from Cranelift IR
//! to machine instructions with virtual registers. This is *almost* the final
//! machine code, except for register allocation.
//!
//! Port of cranelift/codegen/src/machinst/lower.rs (1799 lines)
//!
//! The lowering context (`Lower`) maintains a correspondence from original CLIF
//! Inst to MachInsts. It performs:
//! - Instruction coloring for side-effect analysis
//! - Value use-state tracking (Unused, Once, Multiple)
//! - Pattern matching through instructions for combining
//! - VReg allocation during lowering
//! - Block ordering and critical edge handling

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import from other machinst modules
const reg_mod = @import("reg.zig");
const inst_mod = @import("inst.zig");
const vcode_mod = @import("vcode.zig");
const abi_mod = @import("abi.zig");
const blockorder_mod = @import("blockorder.zig");

// Import CLIF IR types from the real implementation
const clif = @import("../../../ir/clif/mod.zig");

// Re-export key types from reg module
pub const VReg = reg_mod.VReg;
pub const Reg = reg_mod.Reg;
pub const PReg = reg_mod.PReg;
pub const RealReg = reg_mod.RealReg;
pub const VirtualReg = reg_mod.VirtualReg;
pub const Writable = reg_mod.Writable;
pub const RegClass = reg_mod.RegClass;
pub const PRegSet = reg_mod.PRegSet;
pub const SpillSlot = reg_mod.SpillSlot;

// Re-export key types from inst module
// Use CLIF Type for compatibility with DFG
pub const Type = clif.Type;
pub const RelSourceLoc = inst_mod.RelSourceLoc;
pub const MachLabel = inst_mod.MachLabel;
pub const MachTerminator = inst_mod.MachTerminator;

// Re-export key types from vcode module
pub const InsnIndex = vcode_mod.InsnIndex;
pub const BackwardsInsnIndex = vcode_mod.BackwardsInsnIndex;
// Use blockorder's BlockIndex for compatibility with BlockLoweringOrder
pub const BlockIndex = blockorder_mod.BlockIndex;
pub const VCode = vcode_mod.VCode;
pub const VCodeBuilder = vcode_mod.VCodeBuilder;
pub const VCodeBuildDirection = vcode_mod.VCodeBuildDirection;
pub const VRegAllocator = vcode_mod.VRegAllocator;
pub const VCodeConstants = vcode_mod.VCodeConstants;
pub const VCodeConstant = vcode_mod.VCodeConstant;
pub const VCodeConstantData = vcode_mod.VCodeConstantData;

// Re-export from blockorder
pub const BlockLoweringOrder = blockorder_mod.BlockLoweringOrder;
pub const LoweredBlock = blockorder_mod.LoweredBlock;

// Re-export from abi
pub const Callee = abi_mod.Callee;
pub const Sig = abi_mod.Sig;
pub const SigSet = abi_mod.SigSet;
pub const SmallInstVec = abi_mod.SmallInstVec;

// =============================================================================
// CLIF IR Types
// Imported from compiler/ir/clif/. Follows Cranelift pattern exactly:
// use crate::ir::{Block, Function, Inst, Value, DataFlowGraph, Layout, ...};
// =============================================================================

// Entity references
pub const Block = clif.Block;
pub const Value = clif.Value;
pub const Inst = clif.Inst;
pub const StackSlot = clif.StackSlot;
pub const FuncRef = clif.FuncRef;
pub const SigRef = clif.SigRef;
pub const JumpTable = clif.JumpTable;

// Core data structures
pub const Function = clif.Function;
pub const DataFlowGraph = clif.DataFlowGraph;
pub const Layout = clif.Layout;

// Value management
pub const ValueDef = clif.ValueDef;
pub const ValueData = clif.ValueData;
pub const BlockData = clif.BlockData;
pub const InstData = clif.InstData;
pub const ValueList = clif.ValueList;
pub const ValueListPool = clif.ValueListPool;

// Iterators
pub const BlockIterator = clif.BlockIterator;
pub const InstIterator = clif.InstIterator;

// Function/ABI types
pub const Signature = clif.Signature;
pub const AbiParam = clif.AbiParam;
pub const ArgumentPurpose = clif.ArgumentPurpose;
pub const ArgumentExtension = clif.ArgumentExtension;

// Instructions
pub const Opcode = clif.Opcode;
pub const InstructionData = clif.InstructionData;
pub const MemFlags = clif.MemFlags;

// Calling conventions and condition codes
pub const CallConv = clif.CallConv;
pub const IntCC = clif.IntCC;
pub const FloatCC = clif.FloatCC;
pub const TrapCode = clif.TrapCode;

// Jump tables
pub const BlockCall = clif.BlockCall;
pub const JumpTableData = clif.JumpTableData;
pub const JumpTables = clif.JumpTables;

// External names
pub const ExternalName = clif.ExternalName;

/// Reference to a global value.
pub const GlobalValue = struct {
    index: u32,
};

/// Reference to a constant.
pub const Constant = struct {
    index: u32,
};

/// Reference to an immediate value.
pub const Immediate = struct {
    index: u32,
};

/// Constant data storage.
pub const ConstantData = struct {
    bytes: []const u8,
};

// MemFlags and ExternalName are now imported from CLIF above

/// Global value data.
pub const GlobalValueData = union(enum) {
    symbol: struct {
        name: ExternalName,
        offset: i64,
        colocated: bool,
    },
    load: struct {
        base: GlobalValue,
        offset: i32,
        global_type: Type,
    },
    iadd_imm: struct {
        base: GlobalValue,
        offset: i64,
        global_type: Type,
    },
};

// ValueDef and Function are now imported from CLIF above

pub const RelSourceLocMap = struct {
    func: *const Function,

    pub fn get(_: RelSourceLocMap, _: Inst) RelSourceLoc {
        return RelSourceLoc.default();
    }
};

// ArgumentPurpose is now imported from CLIF above

// DataFlowGraph is now imported from CLIF above

// InstructionData, Opcode, JumpTableData are now imported from CLIF above

// Exception handling stubs - not yet in CLIF
pub const ExceptionTableData = struct {};
pub const ExceptionTable = struct {};

// BlockCall and ValueListPool are now imported from CLIF above

// BlockArg stub - not yet in CLIF
pub const BlockArg = union(enum) {
    value: Value,
    try_call_ret: u32,
    try_call_exn: u32,
};
pub const ConstantPool = struct {
    pub fn get(_: *const ConstantPool, _: Constant) *const ConstantData {
        return undefined;
    }

    pub fn len(_: *const ConstantPool) usize {
        return 0;
    }
};

// Signature, AbiParam, CallConv, IntCC, FloatCC, TrapCode are now imported from CLIF above

pub const ValueLabelAssignments = struct {};
pub const ValueLabelStart = struct {
    label: ValueLabel,
    from: u32,
};
pub const ValueLabel = struct {
    index: u32,

    pub fn asU32(self: ValueLabel) u32 {
        return self.index;
    }
};

// Layout, BlockIterator, InstIterator are now imported from CLIF above

// ReverseInstIterator - used for reverse iteration over instructions in a block
pub const ReverseInstIterator = struct {
    current: u32,
    start: u32,

    pub fn next(self: *ReverseInstIterator) ?Inst {
        if (self.current <= self.start) return null;
        self.current -= 1;
        return Inst.fromIndex(self.current);
    }
};

/// Fact for proof-carrying code.
pub const Fact = struct {
    kind: FactKind,

    pub const FactKind = union(enum) {
        range: struct {
            bit_width: u16,
            min: u64,
            max: u64,
        },
    };
};

/// Compilation flags.
pub const Flags = struct {
    enable_pcc: bool = false,
    log2_min_function_alignment: u8 = 0,

    pub fn enablePcc(self: Flags) bool {
        return self.enable_pcc;
    }

    pub fn log2MinFunctionAlignment(self: Flags) u8 {
        return self.log2_min_function_alignment;
    }
};

// =============================================================================
// InstColor
// Port of cranelift/codegen/src/machinst/lower.rs InstColor
// =============================================================================

/// An "instruction color" partitions CLIF instructions by side-effecting ops.
/// All instructions with the same "color" are guaranteed not to be separated by
/// any side-effecting op (for this purpose, loads are also considered
/// side-effecting, to avoid subtle questions w.r.t. the memory model), and
/// furthermore, it is guaranteed that for any two instructions A and B such
/// that color(A) == color(B), either A dominates B and B postdominates A, or
/// vice-versa. (For now, in practice, only ops in the same basic block can ever
/// have the same color, trivially providing the second condition.) Intuitively,
/// this means that the ops of the same color must always execute "together", as
/// part of one atomic contiguous section of the dynamic execution trace, and
/// they can be freely permuted (modulo true dataflow dependencies) without
/// affecting program behavior.
pub const InstColor = struct {
    value: u32,

    const Self = @This();

    pub fn new(n: u32) Self {
        return .{ .value = n };
    }

    /// Get an arbitrary index representing this color. The index is unique
    /// *within a single function compilation*, but indices may be reused across
    /// functions.
    pub fn get(self: Self) u32 {
        return self.value;
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.value == other.value;
    }
};

// =============================================================================
// NonRegInput / InputSourceInst
// Port of cranelift/codegen/src/machinst/lower.rs
// =============================================================================

/// A representation of all of the ways in which a value is available, aside
/// from as a direct register.
///
/// - An instruction, if it would be allowed to occur at the current location
///   instead (see `getInputAsSourceOrConst()` for more details).
///
/// - A constant, if the value is known to be a constant.
pub const NonRegInput = struct {
    /// An instruction produces this value (as the given output), and its
    /// computation (and side-effect if applicable) could occur at the
    /// current instruction's location instead.
    ///
    /// If this instruction's operation is merged into the current instruction,
    /// the backend must call `sinkInst()`.
    ///
    /// This enum indicates whether this use of the source instruction
    /// is unique or not.
    inst: InputSourceInst,

    /// The value is a known constant.
    constant: ?u64,
};

/// When examining an input to an instruction, this enum provides one
/// of several options: there is or isn't a single instruction (that
/// we can see and merge with) that produces that input's value, and
/// we are or aren't the single user of that instruction.
pub const InputSourceInst = union(enum) {
    /// The input in question is the single, unique use of the given
    /// instruction and output index, and it can be sunk to the
    /// location of this input.
    unique_use: struct {
        inst: Inst,
        output_idx: usize,
    },
    /// The input in question is one of multiple uses of the given
    /// instruction. It can still be sunk to the location of this
    /// input.
    use: struct {
        inst: Inst,
        output_idx: usize,
    },
    /// We cannot determine which instruction produced the input, or
    /// it is one of several instructions (e.g., due to a control-flow
    /// merge and blockparam), or the source instruction cannot be
    /// allowed to sink to the current location due to side-effects.
    none,

    const Self = @This();

    /// Get the instruction and output index for this source, whether
    /// we are its single or one of many users.
    pub fn asInst(self: Self) ?struct { inst: Inst, output_idx: usize } {
        return switch (self) {
            .unique_use => |u| .{ .inst = u.inst, .output_idx = u.output_idx },
            .use => |u| .{ .inst = u.inst, .output_idx = u.output_idx },
            .none => null,
        };
    }
};

// =============================================================================
// ValueUseState
// Port of cranelift/codegen/src/machinst/lower.rs ValueUseState
// =============================================================================

/// How is a value used in the IR?
///
/// This can be seen as a coarsening of an integer count. We only need
/// distinct states for zero, one, or many.
///
/// This analysis tells us whether, if every op merged all of its operand tree,
/// a given op could be codegen'd in more than one place.
pub const ValueUseState = enum {
    /// Not used at all.
    unused,
    /// Used exactly once.
    once,
    /// Used multiple times.
    multiple,

    const Self = @This();

    /// Add one use.
    pub fn inc(self: *Self) void {
        self.* = switch (self.*) {
            .unused => .once,
            .once, .multiple => .multiple,
        };
    }
};

// =============================================================================
// RelocDistance
// Port of cranelift/codegen/src/machinst/lower.rs RelocDistance
// =============================================================================

/// Notion of "relocation distance". This gives an estimate of how far away a
/// symbol will be from a reference.
pub const RelocDistance = enum {
    /// Target of relocation is "nearby". The threshold for this is fuzzy but
    /// should be interpreted as approximately "within the compiled output of
    /// one module"; e.g., within AArch64's +/- 128MB offset.
    near,
    /// Target of relocation could be anywhere in the address space.
    far,
};

// =============================================================================
// LowerBackend trait
// Port of cranelift/codegen/src/machinst/lower.rs LowerBackend
// =============================================================================

/// A machine backend. Implemented as a comptime interface check.
///
/// Each ISA backend must implement:
/// - `lower(ctx, inst) -> ?InstOutput` - Lower a single instruction
/// - `lowerBranch(ctx, inst, targets) -> ?void` - Lower a branch
/// - `maybePinnedReg() -> ?Reg` - Get pinned register if any
pub fn isLowerBackend(comptime T: type) bool {
    return @hasDecl(T, "MInst") and
        @hasDecl(T, "lower") and
        @hasDecl(T, "lowerBranch");
}

// =============================================================================
// InstOutput
// Port of cranelift/codegen/src/machinst/lower.rs InstOutput
// =============================================================================

/// A vector of ValueRegs, used to represent the outputs of an instruction.
pub const InstOutput = abi_mod.BoundedArray(ValueRegs(Reg), 2);

/// ValueRegs - multiple registers for a single value (for wide types).
pub fn ValueRegs(comptime R: type) type {
    return struct {
        regs_array: [2]R,
        len: u8,

        const Self = @This();

        pub fn invalid() Self {
            return .{
                .regs_array = .{ R.invalid(), R.invalid() },
                .len = 0,
            };
        }

        pub fn one(r: R) Self {
            return .{
                .regs_array = .{ r, R.invalid() },
                .len = 1,
            };
        }

        pub fn two(r1: R, r2: R) Self {
            return .{
                .regs_array = .{ r1, r2 },
                .len = 2,
            };
        }

        pub fn regs(self: *const Self) []const R {
            return self.regs_array[0..self.len];
        }

        pub fn isValid(self: Self) bool {
            return self.len > 0;
        }

        pub fn isInvalid(self: Self) bool {
            return self.len == 0;
        }

        pub fn length(self: Self) usize {
            return self.len;
        }

        pub fn onlyReg(self: Self) ?R {
            if (self.len == 1) return self.regs_array[0];
            return null;
        }

        pub fn mapTo(self: Self, comptime NewR: type, comptime f: fn (R) NewR) ValueRegs(NewR) {
            var result: ValueRegs(NewR) = undefined;
            result.len = self.len;
            for (0..self.len) |i| {
                result.regs_array[i] = f(self.regs_array[i]);
            }
            return result;
        }
    };
}

/// Convert ValueRegs to writable form.
pub fn writableValueRegs(vregs: ValueRegs(Reg)) ValueRegs(Writable(Reg)) {
    var result: ValueRegs(Writable(Reg)) = undefined;
    result.len = vregs.len;
    for (0..vregs.len) |i| {
        result.regs_array[i] = Writable(Reg).fromReg(vregs.regs_array[i]);
    }
    return result;
}

// =============================================================================
// SecondaryMap
// A map with default values for missing keys.
// =============================================================================

pub fn SecondaryMap(comptime K: type, comptime V: type) type {
    return struct {
        items: std.ArrayListUnmanaged(V),
        default_val: V,
        allocator: Allocator,

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                .items = .{},
                .default_val = undefined,
                .allocator = allocator,
            };
        }

        pub fn withDefault(allocator: Allocator, default: V) Self {
            return .{
                .items = .{},
                .default_val = default,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit(self.allocator);
        }

        fn keyToIndex(key: K) usize {
            if (@hasField(K, "index")) {
                return key.index;
            } else if (@hasDecl(K, "asU32")) {
                return key.asU32();
            } else {
                return @intCast(key);
            }
        }

        pub fn get(self: *const Self, key: K) V {
            const idx = keyToIndex(key);
            if (idx < self.items.items.len) {
                return self.items.items[idx];
            }
            return self.default_val;
        }

        pub fn getPtr(self: *Self, key: K) *V {
            const idx = keyToIndex(key);
            self.ensureCapacity(idx + 1) catch return &self.default_val;
            return &self.items.items[idx];
        }

        pub fn set(self: *Self, key: K, value: V) !void {
            const idx = keyToIndex(key);
            try self.ensureCapacity(idx + 1);
            self.items.items[idx] = value;
        }

        fn ensureCapacity(self: *Self, capacity: usize) !void {
            if (capacity > self.items.items.len) {
                const old_len = self.items.items.len;
                try self.items.resize(self.allocator, capacity);
                for (old_len..capacity) |i| {
                    self.items.items[i] = self.default_val;
                }
            }
        }
    };
}

// =============================================================================
// Lower struct
// Port of cranelift/codegen/src/machinst/lower.rs Lower
// =============================================================================

/// Machine-independent lowering driver / machine-instruction container.
/// Maintains a correspondence from original Inst to MachInsts.
pub fn Lower(comptime I: type) type {
    return struct {
        const Self = @This();

        /// Allocator for all allocations.
        allocator: Allocator,

        /// The function to lower.
        f: *const Function,

        /// Block lowering order for CFG traversal.
        block_order: BlockLoweringOrder,

        /// Lowered machine instructions.
        vcode: VCodeBuilder(I),

        /// VReg allocation context.
        vregs: VRegAllocator(I),

        /// Mapping from `Value` (SSA value in IR) to virtual register.
        value_regs: SecondaryMap(Value, ValueRegs(Reg)),

        /// sret registers, if needed.
        sret_reg: ?ValueRegs(Reg),

        /// Instruction colors at block exits.
        block_end_colors: SecondaryMap(Block, InstColor),

        /// Instruction colors at side-effecting ops (entry color).
        side_effect_inst_entry_colors: std.AutoHashMapUnmanaged(Inst, InstColor),

        /// Current color as we scan during lowering.
        cur_scan_entry_color: ?InstColor,

        /// Current instruction as we scan during lowering.
        cur_inst: ?Inst,

        /// Instruction constant values, if known.
        inst_constants: std.AutoHashMapUnmanaged(Inst, u64),

        /// Use-counts per SSA value, as counted in the input IR.
        value_ir_uses: SecondaryMap(Value, ValueUseState),

        /// Actual uses of each SSA value so far, incremented while lowering.
        value_lowered_uses: SecondaryMap(Value, u32),

        /// Effectful instructions that have been sunk.
        inst_sunk: std.AutoHashMapUnmanaged(Inst, void),

        /// Instructions collected for the CLIF inst in progress, in forward order.
        ir_insts: std.ArrayListUnmanaged(I),

        /// Try-call block arg normal-return values, indexed by instruction.
        try_call_rets: std.AutoHashMapUnmanaged(Inst, std.ArrayListUnmanaged(ValueRegs(Writable(Reg)))),

        /// Try-call block arg exceptional-return payloads.
        try_call_payloads: std.AutoHashMapUnmanaged(Inst, std.ArrayListUnmanaged(Writable(Reg))),

        /// The register to use for GetPinnedReg, if any.
        pinned_reg: ?Reg,

        /// Compilation flags.
        flags: Flags,

        // =====================================================================
        // Constructor
        // =====================================================================

        /// Prepare a new lowering context for the given IR function.
        pub fn init(
            allocator: Allocator,
            f: *const Function,
            block_order: BlockLoweringOrder,
            flags: Flags,
        ) !Self {
            const vcode = try VCodeBuilder(I).init(
                allocator,
                block_order.loweredOrder().len,
                .backward,
                flags.log2MinFunctionAlignment(),
            );

            // Initial capacity based on number of values in the function
            const initial_capacity = f.dfg.numValues();
            var vregs = try VRegAllocator(I).init(allocator, initial_capacity);

            var value_regs = SecondaryMap(Value, ValueRegs(Reg)).withDefault(
                allocator,
                ValueRegs(Reg).invalid(),
            );

            // Assign a vreg to each block param and each inst result.
            var block_iter = f.layout.blocks();
            while (block_iter.next()) |block| {
                for (f.dfg.blockParams(block)) |param| {
                    const ty = f.dfg.valueType(param);
                    if (value_regs.get(param).isInvalid()) {
                        const rc = ty.regClass();
                        const reg = try vregs.alloc(ty, rc);
                        try value_regs.set(param, ValueRegs(Reg).one(reg));
                    }
                }
                var inst_iter = f.layout.blockInsts(block);
                while (inst_iter.next()) |inst| {
                    for (f.dfg.instResults(inst)) |result| {
                        const ty = f.dfg.valueType(result);
                        if (value_regs.get(result).isInvalid() and !ty.eql(clif.Type.INVALID)) {
                            const rc = ty.regClass();
                            const reg = try vregs.alloc(ty, rc);
                            try value_regs.set(result, ValueRegs(Reg).one(reg));
                        }
                    }
                }
            }

            // Compute instruction colors.
            var cur_color: u32 = 0;
            var block_end_colors = SecondaryMap(Block, InstColor).withDefault(
                allocator,
                InstColor.new(0),
            );
            var side_effect_inst_entry_colors = std.AutoHashMapUnmanaged(Inst, InstColor){};
            var inst_constants = std.AutoHashMapUnmanaged(Inst, u64){};

            block_iter = f.layout.blocks();
            while (block_iter.next()) |block| {
                cur_color += 1;
                var inst_iter = f.layout.blockInsts(block);
                while (inst_iter.next()) |inst| {
                    const side_effect = hasLoweringSideEffect(f, inst);
                    if (side_effect) {
                        try side_effect_inst_entry_colors.put(allocator, inst, InstColor.new(cur_color));
                        cur_color += 1;
                    }

                    // Check for constants.
                    if (isConstant64Bit(f, inst)) |c| {
                        try inst_constants.put(allocator, inst, c);
                    }
                }
                try block_end_colors.set(block, InstColor.new(cur_color));
            }

            const value_ir_uses = try computeUseStates(allocator, f, null);

            return .{
                .allocator = allocator,
                .f = f,
                .block_order = block_order,
                .vcode = vcode,
                .vregs = vregs,
                .value_regs = value_regs,
                .sret_reg = null,
                .block_end_colors = block_end_colors,
                .side_effect_inst_entry_colors = side_effect_inst_entry_colors,
                .cur_scan_entry_color = null,
                .cur_inst = null,
                .inst_constants = inst_constants,
                .value_ir_uses = value_ir_uses,
                .value_lowered_uses = SecondaryMap(Value, u32).withDefault(allocator, 0),
                .inst_sunk = .{},
                .ir_insts = .{},
                .try_call_rets = .{},
                .try_call_payloads = .{},
                .pinned_reg = null,
                .flags = flags,
            };
        }

        pub fn deinit(self: *Self) void {
            self.vcode.deinit();
            self.vregs.deinit();
            self.value_regs.deinit();
            self.block_end_colors.deinit();
            self.side_effect_inst_entry_colors.deinit(self.allocator);
            self.inst_constants.deinit(self.allocator);
            self.value_ir_uses.deinit();
            self.value_lowered_uses.deinit();
            self.inst_sunk.deinit(self.allocator);
            self.ir_insts.deinit(self.allocator);
        }

        // =====================================================================
        // Function-level queries
        // =====================================================================

        pub fn dfg(self: *const Self) *const DataFlowGraph {
            return &self.f.dfg;
        }

        // =====================================================================
        // Instruction input/output queries
        // =====================================================================

        /// Get the instdata for a given IR instruction.
        pub fn data(self: *const Self, ir_inst: Inst) InstData {
            return self.f.dfg.getInstData(ir_inst);
        }

        /// Get the source location for a given instruction.
        pub fn srcloc(_: *const Self, _: Inst) RelSourceLoc {
            // TODO: Implement source location tracking in CLIF Function
            return RelSourceLoc.default();
        }

        /// Get the number of inputs to the given IR instruction.
        pub fn numInputs(self: *const Self, ir_inst: Inst) usize {
            return self.f.dfg.instArgs(ir_inst).len;
        }

        /// Get the number of outputs to the given IR instruction.
        pub fn numOutputs(self: *const Self, ir_inst: Inst) usize {
            return self.f.dfg.instResults(ir_inst).len;
        }

        /// Get the type for an instruction's input.
        pub fn inputTy(self: *const Self, ir_inst: Inst, idx: usize) Type {
            const val = self.inputAsValue(ir_inst, idx);
            return self.valueTy(val);
        }

        /// Get the type for a value.
        pub fn valueTy(self: *const Self, val: Value) Type {
            return self.f.dfg.valueType(val);
        }

        /// Get the type for an instruction's output.
        pub fn outputTy(self: *const Self, ir_inst: Inst, idx: usize) Type {
            return self.f.dfg.valueType(self.f.dfg.instResults(ir_inst)[idx]);
        }

        /// Get the value of a constant instruction as a 64-bit value.
        pub fn getConstant(self: *const Self, ir_inst: Inst) ?u64 {
            return self.inst_constants.get(ir_inst);
        }

        /// Resolves a particular input of an instruction to the Value it represents.
        pub fn inputAsValue(self: *const Self, ir_inst: Inst, idx: usize) Value {
            const val = self.f.dfg.instArgs(ir_inst)[idx];
            std.debug.assert(self.f.dfg.valueIsReal(val));
            return val;
        }

        /// Get the input as a source instruction or constant.
        pub fn getInputAsSourceOrConst(self: *const Self, ir_inst: Inst, idx: usize) NonRegInput {
            const val = self.inputAsValue(ir_inst, idx);
            return self.getValueAsSourceOrConst(val);
        }

        /// Resolves a Value definition to the source instruction.
        pub fn getValueAsSourceOrConst(self: *const Self, val: Value) NonRegInput {
            const inst_source = blk: {
                const def = self.f.dfg.valueDef(val);
                switch (def) {
                    .result => |r| {
                        const src_inst = r.inst;
                        const result_idx = r.num;
                        const src_side_effect = hasLoweringSideEffect(self.f, src_inst);

                        if (isValueUseRoot(self.f, src_inst)) {
                            break :blk InputSourceInst{ .none = {} };
                        } else if (!src_side_effect) {
                            if (self.value_ir_uses.get(val) == .once) {
                                break :blk InputSourceInst{ .unique_use = .{
                                    .inst = src_inst,
                                    .output_idx = result_idx,
                                } };
                            } else {
                                break :blk InputSourceInst{ .use = .{
                                    .inst = src_inst,
                                    .output_idx = result_idx,
                                } };
                            }
                        } else {
                            // Side-effect: test color matching.
                            if (self.cur_scan_entry_color) |cur_color| {
                                if (self.value_ir_uses.get(val) == .once and
                                    self.numOutputs(src_inst) == 1)
                                {
                                    if (self.side_effect_inst_entry_colors.get(src_inst)) |entry_color| {
                                        if (entry_color.get() + 1 == cur_color.get()) {
                                            break :blk InputSourceInst{ .unique_use = .{
                                                .inst = src_inst,
                                                .output_idx = 0,
                                            } };
                                        }
                                    }
                                }
                            }
                            break :blk InputSourceInst{ .none = {} };
                        }
                    },
                    .param => {
                        break :blk InputSourceInst{ .none = {} };
                    },
                    .alias => |a| {
                        // Recursively resolve the alias
                        return self.getValueAsSourceOrConst(a.original);
                    },
                }
            };

            const constant = if (inst_source.asInst()) |src|
                self.getConstant(src.inst)
            else
                null;

            return .{
                .inst = inst_source,
                .constant = constant,
            };
        }

        /// Has this instruction been sunk to a use-site?
        pub fn isInstSunk(self: *const Self, inst: Inst) bool {
            return self.inst_sunk.contains(inst);
        }

        /// Is any result of this instruction needed?
        fn isAnyInstResultNeeded(self: *const Self, inst: Inst) bool {
            for (self.f.dfg.instResults(inst)) |result| {
                if (self.value_lowered_uses.get(result) > 0) {
                    return true;
                }
            }
            return false;
        }

        /// Increment the reference count for the Value.
        pub fn incrementLoweredUses(self: *Self, val: Value) void {
            const ptr = self.value_lowered_uses.getPtr(val);
            ptr.* += 1;
        }

        /// Put the `idx`th input into register(s) and return the assigned register.
        pub fn putInputInRegs(self: *Self, ir_inst: Inst, idx: usize) ValueRegs(Reg) {
            const val = self.f.dfg.instArgs(ir_inst)[idx];
            return self.putValueInRegs(val);
        }

        /// Put the given value into register(s) and return the assigned register.
        pub fn putValueInRegs(self: *Self, val: Value) ValueRegs(Reg) {
            std.debug.assert(self.f.dfg.valueIsReal(val));

            if (self.f.dfg.valueDef(val).inst()) |inst| {
                std.debug.assert(!self.inst_sunk.contains(inst));
            }

            const regs = self.value_regs.get(val);
            std.debug.assert(regs.isValid());

            self.incrementLoweredUses(val);

            return regs;
        }

        /// Check if a value is unused.
        pub fn valueIsUnused(self: *const Self, val: Value) bool {
            return self.value_ir_uses.get(val) == .unused;
        }

        // =====================================================================
        // Codegen primitives
        // =====================================================================

        /// Get a new temp.
        pub fn allocTmp(self: *Self, ty: Type) !ValueRegs(Writable(Reg)) {
            const rc = ty.regClass();
            const reg = try self.vregs.alloc(ty, rc);
            return ValueRegs(Writable(Reg)).one(Writable(Reg).fromReg(reg));
        }

        /// Get the current root instruction that we are lowering.
        pub fn curInst(self: *const Self) Inst {
            return self.cur_inst.?;
        }

        /// Emit a machine instruction.
        pub fn emit(self: *Self, mach_inst: I) !void {
            try self.ir_insts.append(self.allocator, mach_inst);
        }

        /// Indicate that a side-effecting instruction has been sunk.
        pub fn sinkInst(self: *Self, ir_inst: Inst) !void {
            std.debug.assert(hasLoweringSideEffect(self.f, ir_inst));
            std.debug.assert(self.cur_scan_entry_color != null);

            for (self.dfg().instResults(ir_inst)) |result| {
                std.debug.assert(self.value_lowered_uses.get(result) == 0);
            }

            const sunk_inst_entry_color = self.side_effect_inst_entry_colors.get(ir_inst).?;
            const sunk_inst_exit_color = InstColor.new(sunk_inst_entry_color.get() + 1);
            std.debug.assert(sunk_inst_exit_color.eql(self.cur_scan_entry_color.?));
            self.cur_scan_entry_color = sunk_inst_entry_color;
            try self.inst_sunk.put(self.allocator, ir_inst, {});
        }

        /// Retrieve constant data given a handle.
        pub fn getConstantData(self: *const Self, constant_handle: Constant) *const ConstantData {
            return self.f.dfg.constants.get(constant_handle);
        }

        /// Cause the value in `reg` to be in a virtual reg.
        pub fn ensureInVreg(self: *Self, r: Reg, ty: Type) !Reg {
            if (r.toVirtualReg() != null) {
                return r;
            } else {
                const new_reg = (try self.allocTmp(ty)).onlyReg().?;
                try self.emit(I.genMove(new_reg, r, ty));
                return new_reg.toReg();
            }
        }

        /// Add a range fact to a register, if no other fact is present.
        pub fn addRangeFact(self: *Self, r: Reg, bit_width: u16, min: u64, max: u64) void {
            if (self.flags.enablePcc()) {
                if (r.toVirtualReg()) |vreg| {
                    self.vregs.setFactIfMissing(vreg, .{
                        .kind = .{ .range = .{
                            .bit_width = bit_width,
                            .min = min,
                            .max = max,
                        } },
                    });
                }
            }
        }

        // =====================================================================
        // Block successor helpers
        // =====================================================================

        /// Get the label for a block successor.
        pub fn blockSuccessorLabel(self: *const Self, block: Block, succ: usize) MachLabel {
            const lowered = self.block_order.loweredIndexForBlock(block) orelse
                @panic("Unreachable block");
            const succs = self.block_order.succIndices(lowered).succs;
            const succ_block = succs[succ];
            return MachLabel.fromBlock(succ_block);
        }

        // =====================================================================
        // Internal lowering methods
        // =====================================================================

        fn finishIrInst(self: *Self, loc: RelSourceLoc) !void {
            // VCodeBuilder builds in reverse order, but ir_insts is in forward order.
            var i = self.ir_insts.items.len;
            while (i > 0) {
                i -= 1;
                try self.vcode.push(self.ir_insts.items[i], loc);
            }
            self.ir_insts.clearRetainingCapacity();
        }

        fn finishBb(self: *Self) !void {
            try self.vcode.endBb();
        }

        fn addBlockParams(self: *Self, block: Block) !void {
            for (self.f.dfg.blockParams(block)) |param| {
                const regs = self.value_regs.get(param);
                for (regs.regs()) |r| {
                    const vreg = r.toVirtualReg().?;
                    try self.vcode.addBlockParam(vreg);
                }
            }
        }

        /// Lower a single CLIF block.
        fn lowerClifBlock(
            self: *Self,
            comptime B: type,
            backend: *const B,
            block: Block,
        ) !void {
            self.cur_scan_entry_color = self.block_end_colors.get(block);

            // Lowering loop: for each non-branch instruction, in reverse order.
            var inst_iter = self.f.layout.blockInsts(block);
            while (inst_iter.nextBack()) |inst| {
                const inst_data = &self.f.dfg.insts.items[inst.index];
                const has_side_effect = hasLoweringSideEffect(self.f, inst);

                // Skip sunk instructions.
                if (self.isInstSunk(inst)) {
                    continue;
                }

                // Are any outputs used?
                const value_needed = self.isAnyInstResultNeeded(inst);

                // Update scan state.
                self.cur_inst = inst;
                if (has_side_effect) {
                    if (self.side_effect_inst_entry_colors.get(inst)) |entry_color| {
                        self.cur_scan_entry_color = entry_color;
                    }
                }

                // Skip branches (handled separately).
                if (inst_data.opcode.isBranch()) {
                    continue;
                }

                // Codegen if side-effecting or any output is used.
                if (has_side_effect or value_needed) {
                    const temp_regs = backend.lower(self, inst) orelse {
                        return error.UnsupportedInstruction;
                    };

                    // Set up aliases for the lowered registers.
                    const results = self.f.dfg.instResults(inst);
                    std.debug.assert(temp_regs.len == results.len);
                    for (temp_regs.slice(), results) |regs, result| {
                        const dsts = self.value_regs.get(result);
                        var reg_iter: usize = 0;
                        for (dsts.regs()) |dst| {
                            const temp = if (reg_iter < regs.length())
                                regs.regs()[reg_iter]
                            else
                                Reg.invalid();
                            try self.vregs.setVregAlias(dst, temp);
                            reg_iter += 1;
                        }
                    }
                }

                const loc = self.srcloc(inst);
                try self.finishIrInst(loc);
            }

            // Add block params.
            try self.addBlockParams(block);

            self.cur_scan_entry_color = null;
        }

        /// Lower a branch instruction.
        fn lowerClifBranch(
            self: *Self,
            comptime B: type,
            backend: *const B,
            bindex: BlockIndex,
            _: Block, // unused, but kept for API compatibility
            branch: Inst,
            targets: []const MachLabel,
        ) !void {
            self.cur_inst = branch;
            backend.lowerBranch(self, branch, targets) orelse {
                @panic("Unimplemented branch lowering");
            };
            const loc = self.srcloc(branch);
            try self.finishIrInst(loc);
            try self.lowerBranchBlockparamArgs(bindex);
        }

        fn lowerBranchBlockparamArgs(self: *Self, block: BlockIndex) !void {
            var branch_arg_vregs: abi_mod.BoundedArray(Reg, 16) = .{};
            const succs = self.block_order.succIndices(block).succs;

            for (0..succs.len) |succ_idx| {
                branch_arg_vregs.len = 0;
                const result = self.collectBlockCall(block, succ_idx, &branch_arg_vregs);
                try self.vcode.addSucc(result.succ, result.args);
            }
        }

        fn collectBranchAndTargets(
            self: *const Self,
            bindex: BlockIndex,
            targets: *abi_mod.BoundedArray(MachLabel, 2),
        ) ?Inst {
            targets.len = 0;
            const result = self.block_order.succIndices(bindex);
            const opt_inst = result.opt_inst;
            const succs = result.succs;
            for (succs) |succ| {
                targets.appendAssumeCapacity(MachLabel.fromBlock(succ));
            }
            return opt_inst;
        }

        fn collectBlockCall(
            self: *Self,
            block: BlockIndex,
            succ_idx: usize,
            buffer: *abi_mod.BoundedArray(Reg, 16),
        ) struct { succ: BlockIndex, args: []const Reg } {
            const block_order = self.block_order;
            const succs = block_order.succIndices(block).succs;
            const succ = succs[succ_idx];
            const this_lb = block_order.loweredOrder()[block.index()];
            const succ_lb = block_order.loweredOrder()[succ.index()];

            // Handle critical edge case - successor is a split-critical-edge block
            if (succ_lb == .critical_edge) {
                return .{ .succ = succ, .args = &[_]Reg{} };
            }

            // Get branch info based on this_lb
            const BranchInfo = struct { branch_inst: Inst, succ_idx: u32 };
            const branch_info: BranchInfo = switch (this_lb) {
                .critical_edge => |ce| blk: {
                    const branch_inst = self.f.layout.lastInst(ce.pred).?;
                    break :blk .{ .branch_inst = branch_inst, .succ_idx = ce.succ_idx };
                },
                .orig => |o| blk: {
                    const branch_inst = self.f.layout.lastInst(o.block).?;
                    break :blk .{ .branch_inst = branch_inst, .succ_idx = @intCast(succ_idx) };
                },
            };

            // TODO: Implement block call argument handling once InstData has branchDestination
            // For now, blocks are connected without arguments (simple CFG)
            _ = branch_info;
            return .{ .succ = succ, .args = buffer.slice() };
        }

        // =====================================================================
        // Main lowering entry point
        // =====================================================================

        /// Lower the function.
        pub fn lower(
            self: *Self,
            comptime B: type,
            backend: *const B,
        ) !VCode(I) {
            // Get the pinned reg if any.
            if (@hasDecl(B, "maybePinnedReg")) {
                self.pinned_reg = backend.maybePinnedReg();
            }

            self.vcode.setEntry(BlockIndex.new(0));

            // Reused vectors for branch lowering.
            var targets: abi_mod.BoundedArray(MachLabel, 2) = .{};

            // Get a copy of the lowered order.
            const lowered_order = self.block_order.loweredOrder();

            // Main lowering loop over lowered blocks (in reverse).
            var bindex_iter: usize = lowered_order.len;
            while (bindex_iter > 0) {
                bindex_iter -= 1;
                const bindex = BlockIndex.new(bindex_iter);
                const lb = lowered_order[bindex_iter];

                // End branch.
                if (lb.origBlock()) |bb| {
                    if (self.collectBranchAndTargets(bindex, &targets)) |branch| {
                        try self.lowerClifBranch(B, backend, bindex, bb, branch, targets.slice());
                        try self.finishIrInst(self.srcloc(branch));
                    }
                } else {
                    // Pure edge block; emit a jump.
                    const succ = self.block_order.succIndices(bindex).succs[0];
                    try self.emit(I.genJump(MachLabel.fromBlock(succ)));
                    try self.finishIrInst(RelSourceLoc.default());
                    try self.lowerBranchBlockparamArgs(bindex);
                }

                // Original block body.
                if (lb.origBlock()) |bb| {
                    try self.lowerClifBlock(B, backend, bb);
                }

                // Arg setup for entry block.
                if (bindex_iter == 0) {
                    try self.genArgSetup();
                    try self.finishIrInst(RelSourceLoc.default());
                }

                try self.finishBb();
            }

            // Build and return the VCode.
            return try self.vcode.build(&self.vregs);
        }

        fn genArgSetup(self: *Self) !void {
            // Generate argument setup instructions.
            // This is a placeholder - full implementation would copy args from ABI locations.
            _ = self;
        }

        /// Generate list of registers to hold the output of a call.
        pub fn genCallOutput(self: *Self, sig: *const Signature) !InstOutput {
            var rets: InstOutput = .{};
            for (sig.returns) |ret| {
                const regs = try self.vregs.alloc(ret.value_type);
                try rets.append(regs);
            }
            return rets;
        }

        /// Generate the return instruction.
        pub fn genReturn(self: *Self, rets: []const ValueRegs(Reg)) !void {
            // Placeholder - would generate actual return sequence.
            _ = rets;
            _ = self;
        }
    };
}

// =============================================================================
// Helper functions
// Port of cranelift/codegen/src/machinst/lower.rs helper functions
// =============================================================================

/// Determine if an instruction has a side effect for lowering purposes.
/// Loads are considered side-effecting to avoid subtle memory model issues.
pub fn hasLoweringSideEffect(f: *const Function, inst: Inst) bool {
    const opcode = f.dfg.insts.items[inst.index].opcode;
    return switch (opcode) {
        .load, .store => true,
        .call, .call_indirect => true,
        .@"return" => true,
        else => false,
    };
}

/// Check if an instruction is a 64-bit constant.
pub fn isConstant64Bit(f: *const Function, inst: Inst) ?u64 {
    const inst_data = f.dfg.getInstData(inst);
    if (inst_data.opcode == .iconst) {
        // Extract the constant value from the immediate field
        return inst_data.getImmediate();
    }
    return null;
}

/// Definition of a "root" instruction for the calculation of ValueUseState.
/// Currently defined as multi-result instructions.
fn isValueUseRoot(f: *const Function, inst: Inst) bool {
    return f.dfg.instResults(inst).len > 1;
}

/// Pre-analysis: compute `value_ir_uses`.
pub fn computeUseStates(
    allocator: Allocator,
    f: *const Function,
    sret_param: ?Value,
) !SecondaryMap(Value, ValueUseState) {
    var value_ir_uses = SecondaryMap(Value, ValueUseState).withDefault(allocator, .unused);

    if (sret_param) |param| {
        try value_ir_uses.set(param, .multiple);
    }

    // Stack for DFS to mark Multiple-state subtrees.
    var stack = std.ArrayListUnmanaged([]const Value){};
    defer stack.deinit(allocator);

    // Find the args for the inst corresponding to the given value.
    const getUses = struct {
        fn get(func: *const Function, value: Value) ?[]const Value {
            const def = func.dfg.valueDef(value);
            switch (def) {
                .result => |r| {
                    if (isValueUseRoot(func, r.inst)) {
                        return null;
                    }
                    return func.dfg.instValues(r.inst);
                },
                .param => return null,
                .alias => |a| {
                    // For aliases, follow to the original value
                    return get(func, a.original);
                },
            }
        }
    }.get;

    // Iterate over all instructions.
    var block_iter = f.layout.blocks();
    while (block_iter.next()) |block| {
        var inst_iter = f.layout.blockInsts(block);
        while (inst_iter.next()) |inst| {
            for (f.dfg.instValues(inst)) |arg| {
                std.debug.assert(f.dfg.valueIsReal(arg));
                const old = value_ir_uses.get(arg);
                const ptr = value_ir_uses.getPtr(arg);
                ptr.inc();
                const new = value_ir_uses.get(arg);

                // On transition to Multiple, do DFS.
                if (old == .multiple or new != .multiple) {
                    continue;
                }
                if (getUses(f, arg)) |uses| {
                    try stack.append(allocator, uses);
                }
                while (stack.items.len > 0) {
                    const uses_slice = stack.items[stack.items.len - 1];
                    if (uses_slice.len == 0) {
                        _ = stack.pop();
                        continue;
                    }
                    const value = uses_slice[0];
                    stack.items[stack.items.len - 1] = uses_slice[1..];

                    std.debug.assert(f.dfg.valueIsReal(value));
                    if (value_ir_uses.get(value) == .multiple) {
                        continue;
                    }
                    try value_ir_uses.set(value, .multiple);
                    if (getUses(f, value)) |more_uses| {
                        try stack.append(allocator, more_uses);
                    }
                }
            }
        }
    }

    return value_ir_uses;
}

// =============================================================================
// Tests
// =============================================================================

test "InstColor creation and equality" {
    const c1 = InstColor.new(42);
    const c2 = InstColor.new(42);
    const c3 = InstColor.new(43);

    try std.testing.expect(c1.eql(c2));
    try std.testing.expect(!c1.eql(c3));
    try std.testing.expectEqual(@as(u32, 42), c1.get());
}

test "ValueUseState increment" {
    var state = ValueUseState.unused;
    try std.testing.expectEqual(ValueUseState.unused, state);

    state.inc();
    try std.testing.expectEqual(ValueUseState.once, state);

    state.inc();
    try std.testing.expectEqual(ValueUseState.multiple, state);

    state.inc();
    try std.testing.expectEqual(ValueUseState.multiple, state);
}

test "InputSourceInst asInst" {
    const unique = InputSourceInst{ .unique_use = .{
        .inst = Inst.fromIndex(5),
        .output_idx = 0,
    } };
    const use = InputSourceInst{ .use = .{
        .inst = Inst.fromIndex(10),
        .output_idx = 1,
    } };
    const none = InputSourceInst{ .none = {} };

    const unique_result = unique.asInst();
    try std.testing.expect(unique_result != null);
    try std.testing.expectEqual(@as(u32, 5), unique_result.?.inst.index);
    try std.testing.expectEqual(@as(usize, 0), unique_result.?.output_idx);

    const use_result = use.asInst();
    try std.testing.expect(use_result != null);
    try std.testing.expectEqual(@as(u32, 10), use_result.?.inst.index);
    try std.testing.expectEqual(@as(usize, 1), use_result.?.output_idx);

    try std.testing.expect(none.asInst() == null);
}

test "ValueRegs creation" {
    // Create test registers using VReg
    const vreg1 = VReg.init(200, .int); // User vreg (>192)
    const vreg2 = VReg.init(201, .int);
    const reg1 = Reg.fromVReg(vreg1);
    const reg2 = Reg.fromVReg(vreg2);

    const single = ValueRegs(Reg).one(reg1);
    try std.testing.expectEqual(@as(usize, 1), single.length());
    try std.testing.expect(single.isValid());
    try std.testing.expect(single.onlyReg() != null);

    const double = ValueRegs(Reg).two(reg1, reg2);
    try std.testing.expectEqual(@as(usize, 2), double.length());
    try std.testing.expect(double.isValid());
    try std.testing.expect(double.onlyReg() == null);

    const invalid_regs = ValueRegs(Reg).invalid();
    try std.testing.expect(invalid_regs.isInvalid());
    try std.testing.expectEqual(@as(usize, 0), invalid_regs.length());
}

test "SecondaryMap with default" {
    const allocator = std.testing.allocator;
    var map = SecondaryMap(Value, u32).withDefault(allocator, 999);
    defer map.deinit();

    // Default value for missing keys
    try std.testing.expectEqual(@as(u32, 999), map.get(Value.fromIndex(100)));

    // Set a value
    try map.set(Value.fromIndex(5), 42);
    try std.testing.expectEqual(@as(u32, 42), map.get(Value.fromIndex(5)));

    // Unset value still returns default
    try std.testing.expectEqual(@as(u32, 999), map.get(Value.fromIndex(10)));
}

test "RelocDistance enum" {
    const near = RelocDistance.near;
    const far = RelocDistance.far;

    try std.testing.expect(near != far);
}

test "NonRegInput struct" {
    const input = NonRegInput{
        .inst = InputSourceInst{ .unique_use = .{
            .inst = Inst.fromIndex(10),
            .output_idx = 0,
        } },
        .constant = 42,
    };

    try std.testing.expect(input.inst.asInst() != null);
    try std.testing.expectEqual(@as(?u64, 42), input.constant);

    const no_const = NonRegInput{
        .inst = InputSourceInst{ .none = {} },
        .constant = null,
    };
    try std.testing.expect(no_const.constant == null);
}

test "Opcode isBranch" {
    try std.testing.expect(Opcode.jump.isBranch());
    try std.testing.expect(Opcode.brif.isBranch());
    try std.testing.expect(Opcode.br_table.isBranch());
    // return is a terminator but not a branch - it exits the function rather than jumping to a block
    try std.testing.expect(!Opcode.@"return".isBranch());
    try std.testing.expect(Opcode.@"return".isTerminator());
    try std.testing.expect(!Opcode.iadd.isBranch());
    try std.testing.expect(!Opcode.load.isBranch());
}
