//! This module exposes the machine-specific backend definition pieces.
//!
//! Port of cranelift/codegen/src/machinst/mod.rs (lines 1-630)
//!
//! The MachInst infrastructure is the compiler backend, from CLIF
//! (ir::Function) to machine code. The purpose of this infrastructure is, at a
//! high level, to do instruction selection/lowering (to machine instructions),
//! register allocation, and then perform all the fixups to branches, constant
//! data references, etc., needed to actually generate machine code.
//!
//! The container for machine instructions, at various stages of construction,
//! is the `VCode` struct. We refer to a sequence of machine instructions organized
//! into basic blocks as "vcode". This is short for "virtual-register code".
//!
//! The compilation pipeline, from an `ir::Function` (already optimized as much as
//! you like by machine-independent optimization passes) onward, is as follows.
//!
//!     ir::Function                (SSA IR, machine-independent opcodes)
//!         |
//!         |  [lower]
//!         |
//!     VCode<arch_backend::Inst>   (machine instructions:
//!         |                        - mostly virtual registers.
//!         |                        - cond branches in two-target form.
//!         |                        - branch targets are block indices.
//!         |                        - in-memory constants held by insns,
//!         |                          with unknown offsets.
//!         |                        - critical edges (actually all edges)
//!         |                          are split.)
//!         |
//!         | [regalloc --> `regalloc2::Output`; VCode is unchanged]
//!         |
//!         | [binary emission via MachBuffer]
//!         |
//!     Vec<u8>                     (machine code:
//!         |                        - two-dest branches resolved via
//!         |                          streaming branch resolution/simplification.
//!         |                        - regalloc `Allocation` results used directly
//!         |                          by instruction emission code.
//!         |                        - prologue and epilogue(s) built and emitted
//!         |                          directly during emission.
//!         |                        - SP-relative offsets resolved by tracking
//!         |                          EmitState.)

const std = @import("std");
const reg = @import("reg.zig");

// Re-exports from reg.zig
pub const Reg = reg.Reg;
pub const VReg = reg.VReg;
pub const RegClass = reg.RegClass;
pub const Writable = reg.Writable;
pub const RealReg = reg.RealReg;
pub const PReg = reg.PReg;
pub const PRegSet = reg.PRegSet;
pub const Operand = reg.Operand;
pub const OperandKind = reg.OperandKind;
pub const OperandPos = reg.OperandPos;
pub const OperandConstraint = reg.OperandConstraint;

// ============================================================================
// Type aliases from binemit (lines 47-48)
// ============================================================================

/// An addend for relocations.
pub const Addend = i64;

/// Code offset within a function.
pub const CodeOffset = u32;

// ============================================================================
// Reloc (from binemit::Reloc)
// ============================================================================

/// A relocation kind.
pub const Reloc = enum(u8) {
    /// Absolute 4-byte reference.
    Abs4,
    /// Absolute 8-byte reference.
    Abs8,
    /// x86 PC-relative 4-byte (RIP-relative).
    X86PCRel4,
    /// x86 PC-relative 4-byte with sign extension.
    X86PCRelRodata4,
    /// x86 GOT PC-relative 4-byte.
    X86GOTPCRel4,
    /// x86 PLT-relative 4-byte.
    X86PLTRel4,
    /// ARM64 call-relative 26-bit.
    Arm64Call,
    /// RISC-V call placeholder.
    RiscvCallPlt,
    /// s390x PC-relative 32-bit.
    S390xPCRel32Dbl,
};

// ============================================================================
// CodeInfo (from binemit::CodeInfo, line 48)
// ============================================================================

/// Information about generated code.
pub const CodeInfo = struct {
    /// Total size of the generated code in bytes.
    total_size: u32,
};

// ============================================================================
// Type (from ir::Type, line 49)
// ============================================================================

/// Cranelift IR types.
pub const Type = enum(u8) {
    /// Invalid type.
    INVALID = 0,
    /// 8-bit integer.
    I8 = 0x70,
    /// 16-bit integer.
    I16 = 0x71,
    /// 32-bit integer.
    I32 = 0x72,
    /// 64-bit integer.
    I64 = 0x73,
    /// 128-bit integer.
    I128 = 0x74,
    /// 32-bit floating point.
    F32 = 0x7b,
    /// 64-bit floating point.
    F64 = 0x7c,
    /// 128-bit floating point.
    F128 = 0x7d,
    /// 128-bit vector.
    I8X16 = 0x8b,
    /// Reference type.
    R64 = 0x7e,

    const Self = @This();

    /// Get the number of bits in this type.
    pub fn bits(self: Self) u16 {
        return switch (self) {
            .INVALID => 0,
            .I8 => 8,
            .I16 => 16,
            .I32 => 32,
            .I64 => 64,
            .I128 => 128,
            .F32 => 32,
            .F64 => 64,
            .F128 => 128,
            .I8X16 => 128,
            .R64 => 64,
        };
    }

    /// Get the number of bytes in this type.
    pub fn bytes(self: Self) u8 {
        return @intCast(self.bits() / 8);
    }

    /// Is this a float type?
    pub fn isFloat(self: Self) bool {
        return switch (self) {
            .F32, .F64, .F128 => true,
            else => false,
        };
    }

    /// Is this a vector type?
    pub fn isVector(self: Self) bool {
        return switch (self) {
            .I8X16 => true,
            else => false,
        };
    }

    /// Get the register class for this type.
    pub fn regClass(self: Self) @import("reg.zig").RegClass {
        if (self.isVector()) return .vector;
        if (self.isFloat()) return .float;
        return .int;
    }
};

// ============================================================================
// RelSourceLoc (from ir::RelSourceLoc, line 49)
// ============================================================================

/// A relative source location.
pub const RelSourceLoc = struct {
    /// Offset from the function's base source location.
    offset: u32,

    const Self = @This();

    pub fn init(offset: u32) Self {
        return .{ .offset = offset };
    }

    pub fn default() Self {
        return .{ .offset = 0 };
    }
};

// ============================================================================
// SourceLoc (absolute source location)
// ============================================================================

/// An absolute source location.
pub const SourceLoc = struct {
    bits: u32,

    const Self = @This();

    pub fn init(bits: u32) Self {
        return .{ .bits = bits };
    }

    pub fn default() Self {
        return .{ .bits = 0 };
    }

    pub fn isDefault(self: Self) bool {
        return self.bits == 0;
    }
};

// ============================================================================
// StackSlot and DynamicStackSlot (from ir, line 49)
// ============================================================================

/// A reference to a stack slot.
pub const StackSlot = struct {
    index: u32,

    const Self = @This();

    pub fn init(index: u32) Self {
        return .{ .index = index };
    }

    pub fn asU32(self: Self) u32 {
        return self.index;
    }
};

/// A reference to a dynamic stack slot.
pub const DynamicStackSlot = struct {
    index: u32,

    const Self = @This();

    pub fn init(index: u32) Self {
        return .{ .index = index };
    }

    pub fn asU32(self: Self) u32 {
        return self.index;
    }
};

// ============================================================================
// FunctionAlignment (from isa::FunctionAlignment, line 51)
// ============================================================================

/// Alignment requirements for functions.
pub const FunctionAlignment = struct {
    /// Minimum alignment required (power of 2).
    minimum: u32,
    /// Preferred alignment (power of 2).
    preferred: u32,

    const Self = @This();

    pub fn init(minimum: u32, preferred: u32) Self {
        return .{ .minimum = minimum, .preferred = preferred };
    }
};

// ============================================================================
// MachLabel (line 85, from reg module but also used extensively)
// ============================================================================

/// A label in machine code, pointing to a code offset.
/// The first N MachLabels are reserved for the N blocks in the vcode.
pub const MachLabel = struct {
    index: u32,

    const Self = @This();

    const UNKNOWN: u32 = 0xffff_ffff;

    /// Create a new label with the given index.
    pub fn init(index: u32) Self {
        return .{ .index = index };
    }

    /// Create a label from a u32 (alias for init).
    pub fn fromU32(index: u32) Self {
        return .{ .index = index };
    }

    /// Get a label for a block.
    pub fn fromBlock(bindex: BlockIndex) Self {
        return .{ .index = @intCast(bindex.index()) };
    }

    /// Get the index of this label.
    pub fn getIndex(self: Self) u32 {
        return self.index;
    }

    /// Get the index as u32 (alias for getIndex, for compatibility).
    pub fn asU32(self: Self) u32 {
        return self.index;
    }

    /// Get the block index from this label.
    pub fn toBlock(self: Self) BlockIndex {
        return BlockIndex.new(self.index);
    }

    /// Invalid/unknown label constant.
    pub const INVALID = Self{ .index = UNKNOWN };

    /// Invalid/unknown label sentinel function.
    pub fn invalid() Self {
        return INVALID;
    }

    pub fn isValid(self: Self) bool {
        return self.index != UNKNOWN;
    }

    /// Compare two labels for equality.
    pub fn eql(self: Self, other: Self) bool {
        return self.index == other.index;
    }

    /// Creates a string representing this label.
    pub fn toString(self: Self, buf: []u8) []const u8 {
        return std.fmt.bufPrint(buf, "label{d}", .{self.index}) catch "label?";
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("label{d}", .{self.index});
    }
};

// ============================================================================
// BlockIndex (used by MachLabel::from_block)
// ============================================================================

/// Index referring to a basic block in VCode.
pub const BlockIndex = struct {
    index_val: u32,

    const Self = @This();

    const INVALID: u32 = 0xffff_ffff;

    pub fn init(idx: usize) Self {
        return .{ .index_val = @intCast(idx) };
    }

    pub fn new(idx: usize) Self {
        return init(idx);
    }

    pub fn index(self: Self) usize {
        return @intCast(self.index_val);
    }

    pub fn invalid() Self {
        return .{ .index_val = INVALID };
    }

    pub fn isValid(self: Self) bool {
        return self.index_val != INVALID;
    }

    /// Check if two BlockIndex values are equal.
    pub fn eql(self: Self, other: Self) bool {
        return self.index_val == other.index_val;
    }
};

// ============================================================================
// CallType (lines 280-289)
// ============================================================================

/// Classification of call instruction types for granular analysis.
pub const CallType = enum {
    /// Not a call instruction.
    None,
    /// Regular call that returns to the caller.
    Regular,
    /// Tail call that doesn't return to the caller.
    TailCall,
};

// ============================================================================
// FunctionCalls (lines 291-327)
// ============================================================================

/// Function classification based on call patterns.
///
/// This enum classifies functions based on their calling behavior to enable
/// targeted optimizations. Functions are categorized as:
/// - `None`: No calls at all (can use simplified calling conventions)
/// - `TailOnly`: Only tail calls (may skip frame setup in some cases)
/// - `Regular`: Has regular calls (requires full calling convention support)
pub const FunctionCalls = enum {
    /// Function makes no calls at all.
    None,
    /// Function only makes tail calls (no regular calls).
    TailOnly,
    /// Function makes at least one regular call (may also have tail calls).
    Regular,

    const Self = @This();

    /// Update the function classification based on a new call instruction.
    ///
    /// This method implements the merge logic for accumulating call patterns:
    /// - Any regular call makes the function Regular
    /// - Tail calls upgrade None to TailOnly
    /// - Regular always stays Regular
    pub fn update(self: *Self, call_type: CallType) void {
        self.* = switch (self.*) {
            .None => switch (call_type) {
                .None => .None,
                .Regular => .Regular,
                .TailCall => .TailOnly,
            },
            .TailOnly => switch (call_type) {
                .None => .TailOnly,
                .Regular => .Regular,
                .TailCall => .TailOnly,
            },
            .Regular => .Regular,
        };
    }
};

// ============================================================================
// MachTerminator (lines 329-344)
// ============================================================================

/// Describes a block terminator (not call) in the VCode.
///
/// Actual targets are not included: the single-source-of-truth for
/// those is the VCode itself, which holds, for each block, successors
/// and outgoing branch args per successor.
pub const MachTerminator = enum {
    /// Not a terminator.
    None,
    /// A return instruction.
    Ret,
    /// A tail call.
    RetCall,
    /// A branch.
    Branch,
};

// ============================================================================
// SmallVec equivalent - SmallArray
// ============================================================================

/// A small vector with inline storage, similar to Rust's SmallVec.
/// Stores up to `inline_capacity` items inline, then promotes to heap.
/// Following Cranelift's SmallVec pattern from the smallvec crate.
pub fn SmallVec(comptime T: type, comptime inline_capacity: usize) type {
    return struct {
        inline_buf: [inline_capacity]T,
        inline_len: usize,
        heap_buf: ?[*]T,
        heap_len: usize,
        heap_cap: usize,
        allocator: ?std.mem.Allocator,

        const Self = @This();

        /// Error types for SmallVec operations.
        pub const Error = error{
            OutOfMemory,
            NoAllocator,
        };

        pub fn init() Self {
            return .{
                .inline_buf = undefined,
                .inline_len = 0,
                .heap_buf = null,
                .heap_len = 0,
                .heap_cap = 0,
                .allocator = null,
            };
        }

        pub fn initWithAllocator(allocator: std.mem.Allocator) Self {
            return .{
                .inline_buf = undefined,
                .inline_len = 0,
                .heap_buf = null,
                .heap_len = 0,
                .heap_cap = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.heap_buf) |buf| {
                if (self.allocator) |alloc| {
                    alloc.free(buf[0..self.heap_cap]);
                }
            }
            self.heap_buf = null;
            self.heap_len = 0;
            self.heap_cap = 0;
            self.inline_len = 0;
        }

        pub fn len(self: *const Self) usize {
            if (self.heap_buf != null) {
                return self.heap_len;
            }
            return self.inline_len;
        }

        pub fn items(self: *const Self) []const T {
            if (self.heap_buf) |buf| {
                return buf[0..self.heap_len];
            }
            return self.inline_buf[0..self.inline_len];
        }

        pub fn itemsMut(self: *Self) []T {
            if (self.heap_buf) |buf| {
                return buf[0..self.heap_len];
            }
            return self.inline_buf[0..self.inline_len];
        }

        /// Push with proper error handling.
        /// Returns error.NoAllocator if heap allocation is needed but no allocator was provided.
        /// Returns error.OutOfMemory if allocation fails.
        pub fn push(self: *Self, item: T) Error!void {
            if (self.heap_buf) |buf| {
                // Already on heap - grow if needed
                if (self.heap_len >= self.heap_cap) {
                    const alloc = self.allocator orelse return error.NoAllocator;

                    // Check for overflow before doubling
                    const new_cap = std.math.mul(usize, self.heap_cap, 2) catch {
                        return error.OutOfMemory;
                    };

                    const old_slice = buf[0..self.heap_cap];
                    // Allocate new buffer and copy
                    const new_buf = alloc.alloc(T, new_cap) catch return error.OutOfMemory;
                    @memcpy(new_buf[0..self.heap_len], buf[0..self.heap_len]);
                    alloc.free(old_slice);
                    self.heap_buf = new_buf.ptr;
                    self.heap_cap = new_cap;
                }
                self.heap_buf.?[self.heap_len] = item;
                self.heap_len += 1;
            } else if (self.inline_len < inline_capacity) {
                // Still fits inline
                self.inline_buf[self.inline_len] = item;
                self.inline_len += 1;
            } else {
                // Promote to heap
                const alloc = self.allocator orelse return error.NoAllocator;
                const new_cap = inline_capacity * 2;
                const heap = alloc.alloc(T, new_cap) catch return error.OutOfMemory;
                @memcpy(heap[0..inline_capacity], self.inline_buf[0..inline_capacity]);
                heap[inline_capacity] = item;
                self.heap_buf = heap.ptr;
                self.heap_cap = new_cap;
                self.heap_len = inline_capacity + 1;
            }
        }

        /// Push that panics on error (for places where OOM is unrecoverable).
        /// Use this when working in code paths where error handling isn't possible.
        pub fn pushAssumeCapacity(self: *Self, item: T) void {
            self.push(item) catch |err| {
                switch (err) {
                    error.NoAllocator => @panic("SmallVec needs allocator for heap promotion"),
                    error.OutOfMemory => @panic("SmallVec out of memory"),
                }
            };
        }

        pub fn clear(self: *Self) void {
            if (self.heap_buf) |buf| {
                if (self.allocator) |alloc| {
                    alloc.free(buf[0..self.heap_cap]);
                }
                self.heap_buf = null;
                self.heap_len = 0;
                self.heap_cap = 0;
            }
            self.inline_len = 0;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.len() == 0;
        }
    };
}

// ============================================================================
// MachInst trait (lines 90-229)
// ============================================================================

/// A machine instruction.
/// This is the Zig equivalent of Cranelift's MachInst trait.
///
/// In Rust this is: `pub trait MachInst: Clone + Debug { ... }`
/// In Zig we use a comptime interface pattern.
pub fn MachInst(comptime Self: type) type {
    return struct {
        /// Return the registers referenced by this machine instruction along with
        /// the modes of reference (use, def, modify).
        /// Rust: fn get_operands(&mut self, collector: &mut impl OperandVisitor);
        pub const getOperands = if (@hasDecl(Self, "getOperands"))
            Self.getOperands
        else
            @compileError("MachInst requires getOperands");

        /// If this is a simple move, return the (source, destination) tuple of registers.
        /// Rust: fn is_move(&self) -> Option<(Writable<Reg>, Reg)>;
        pub const isMove = if (@hasDecl(Self, "isMove"))
            Self.isMove
        else
            @compileError("MachInst requires isMove");

        /// Is this a terminator (branch or ret)? If so, return its type
        /// (ret/uncond/cond) and target if applicable.
        /// Rust: fn is_term(&self) -> MachTerminator;
        pub const isTerm = if (@hasDecl(Self, "isTerm"))
            Self.isTerm
        else
            @compileError("MachInst requires isTerm");

        /// Is this an unconditional trap?
        /// Rust: fn is_trap(&self) -> bool;
        pub const isTrap = if (@hasDecl(Self, "isTrap"))
            Self.isTrap
        else
            @compileError("MachInst requires isTrap");

        /// Is this an "args" pseudoinst?
        /// Rust: fn is_args(&self) -> bool;
        pub const isArgs = if (@hasDecl(Self, "isArgs"))
            Self.isArgs
        else
            @compileError("MachInst requires isArgs");

        /// Classify the type of call instruction this is.
        /// Rust: fn call_type(&self) -> CallType;
        pub const callType = if (@hasDecl(Self, "callType"))
            Self.callType
        else
            @compileError("MachInst requires callType");

        /// Should this instruction's clobber-list be included in the clobber-set?
        /// Rust: fn is_included_in_clobbers(&self) -> bool;
        pub const isIncludedInClobbers = if (@hasDecl(Self, "isIncludedInClobbers"))
            Self.isIncludedInClobbers
        else
            @compileError("MachInst requires isIncludedInClobbers");

        /// Does this instruction access memory?
        /// Rust: fn is_mem_access(&self) -> bool;
        pub const isMemAccess = if (@hasDecl(Self, "isMemAccess"))
            Self.isMemAccess
        else
            @compileError("MachInst requires isMemAccess");

        /// Generate a move.
        /// Rust: fn gen_move(to_reg: Writable<Reg>, from_reg: Reg, ty: Type) -> Self;
        pub const genMove = if (@hasDecl(Self, "genMove"))
            Self.genMove
        else
            @compileError("MachInst requires genMove");

        /// Generate a dummy instruction that will keep a value alive but
        /// has no other purpose.
        /// Rust: fn gen_dummy_use(reg: Reg) -> Self;
        pub const genDummyUse = if (@hasDecl(Self, "genDummyUse"))
            Self.genDummyUse
        else
            @compileError("MachInst requires genDummyUse");

        /// Determine register class(es) to store the given Cranelift type.
        /// Rust: fn rc_for_type(ty: Type) -> CodegenResult<(&'static [RegClass], &'static [Type])>;
        pub const rcForType = if (@hasDecl(Self, "rcForType"))
            Self.rcForType
        else
            @compileError("MachInst requires rcForType");

        /// Get an appropriate type that can fully hold a value in a given register class.
        /// Rust: fn canonical_type_for_rc(rc: RegClass) -> Type;
        pub const canonicalTypeForRc = if (@hasDecl(Self, "canonicalTypeForRc"))
            Self.canonicalTypeForRc
        else
            @compileError("MachInst requires canonicalTypeForRc");

        /// Generate a jump to another target.
        /// Rust: fn gen_jump(target: MachLabel) -> Self;
        pub const genJump = if (@hasDecl(Self, "genJump"))
            Self.genJump
        else
            @compileError("MachInst requires genJump");

        /// Generate a store of an immediate 64-bit integer to a register.
        /// Rust: fn gen_imm_u64(_value: u64, _dst: Writable<Reg>) -> Option<Self> { None }
        pub fn genImmU64(value: u64, dst: Writable(Reg)) ?Self {
            if (@hasDecl(Self, "genImmU64")) {
                return Self.genImmU64(value, dst);
            }
            return null;
        }

        /// Generate a store of an immediate 64-bit float to a register.
        /// Rust: fn gen_imm_f64(...) -> SmallVec<[Self; 2]> { SmallVec::new() }
        pub fn genImmF64(value: f64, tmp: Writable(Reg), dst: Writable(Reg)) SmallVec(Self, 2) {
            if (@hasDecl(Self, "genImmF64")) {
                return Self.genImmF64(value, tmp, dst);
            }
            return SmallVec(Self, 2).init();
        }

        /// Generate a NOP.
        /// Rust: fn gen_nop(preferred_size: usize) -> Self;
        pub const genNop = if (@hasDecl(Self, "genNop"))
            Self.genNop
        else
            @compileError("MachInst requires genNop");

        /// The various kinds of NOP, with size, sorted in ascending-size order.
        /// Rust: fn gen_nop_units() -> Vec<Vec<u8>>;
        pub const genNopUnits = if (@hasDecl(Self, "genNopUnits"))
            Self.genNopUnits
        else
            @compileError("MachInst requires genNopUnits");

        /// Align a basic block offset (from start of function). By default, no alignment occurs.
        /// Rust: fn align_basic_block(offset: CodeOffset) -> CodeOffset { offset }
        pub fn alignBasicBlock(offset: CodeOffset) CodeOffset {
            if (@hasDecl(Self, "alignBasicBlock")) {
                return Self.alignBasicBlock(offset);
            }
            return offset;
        }

        /// What is the worst-case instruction size emitted by this instruction type?
        /// Rust: fn worst_case_size() -> CodeOffset;
        pub const worstCaseSize = if (@hasDecl(Self, "worstCaseSize"))
            Self.worstCaseSize
        else
            @compileError("MachInst requires worstCaseSize");

        /// What is the register class used for reference types?
        /// Rust: fn ref_type_regclass(_flags: &Flags) -> RegClass;
        pub const refTypeRegclass = if (@hasDecl(Self, "refTypeRegclass"))
            Self.refTypeRegclass
        else
            @compileError("MachInst requires refTypeRegclass");

        /// Is this a safepoint?
        /// Rust: fn is_safepoint(&self) -> bool;
        pub const isSafepoint = if (@hasDecl(Self, "isSafepoint"))
            Self.isSafepoint
        else
            @compileError("MachInst requires isSafepoint");

        /// Generate an instruction that must appear at the beginning of a basic block.
        /// Rust: fn gen_block_start(...) -> Option<Self> { None }
        pub fn genBlockStart(is_indirect_branch_target: bool, is_forward_edge_cfi_enabled: bool) ?Self {
            if (@hasDecl(Self, "genBlockStart")) {
                return Self.genBlockStart(is_indirect_branch_target, is_forward_edge_cfi_enabled);
            }
            return null;
        }

        /// Returns a description of the alignment required for functions.
        /// Rust: fn function_alignment() -> FunctionAlignment;
        pub const functionAlignment = if (@hasDecl(Self, "functionAlignment"))
            Self.functionAlignment
        else
            @compileError("MachInst requires functionAlignment");

        /// Is this a low-level, one-way branch?
        /// Rust: fn is_low_level_branch(&self) -> bool { false }
        pub fn isLowLevelBranch(self: *const Self) bool {
            if (@hasDecl(Self, "isLowLevelBranch")) {
                return self.isLowLevelBranch();
            }
            return false;
        }

        /// The trap opcode bytes.
        /// Rust: const TRAP_OPCODE: &'static [u8];
        pub const TRAP_OPCODE = if (@hasDecl(Self, "TRAP_OPCODE"))
            Self.TRAP_OPCODE
        else
            @compileError("MachInst requires TRAP_OPCODE");
    };
}

// ============================================================================
// MachInstLabelUse trait (lines 231-278)
// ============================================================================

/// A descriptor of a label reference (use) in an instruction set.
/// Rust: pub trait MachInstLabelUse: Clone + Copy + Debug + Eq { ... }
pub fn MachInstLabelUse(comptime Self: type) type {
    return struct {
        /// Required alignment for any veneer.
        /// Rust: const ALIGN: CodeOffset;
        pub const ALIGN = if (@hasDecl(Self, "ALIGN"))
            Self.ALIGN
        else
            @compileError("MachInstLabelUse requires ALIGN");

        /// What is the maximum PC-relative range (positive)?
        /// Rust: fn max_pos_range(self) -> CodeOffset;
        pub const maxPosRange = if (@hasDecl(Self, "maxPosRange"))
            Self.maxPosRange
        else
            @compileError("MachInstLabelUse requires maxPosRange");

        /// What is the maximum PC-relative range (negative)?
        /// Rust: fn max_neg_range(self) -> CodeOffset;
        pub const maxNegRange = if (@hasDecl(Self, "maxNegRange"))
            Self.maxNegRange
        else
            @compileError("MachInstLabelUse requires maxNegRange");

        /// What is the size of code-buffer slice this label-use needs to patch?
        /// Rust: fn patch_size(self) -> CodeOffset;
        pub const patchSize = if (@hasDecl(Self, "patchSize"))
            Self.patchSize
        else
            @compileError("MachInstLabelUse requires patchSize");

        /// Perform a code-patch.
        /// Rust: fn patch(self, buffer: &mut [u8], use_offset: CodeOffset, label_offset: CodeOffset);
        pub const patch = if (@hasDecl(Self, "patch"))
            Self.patch
        else
            @compileError("MachInstLabelUse requires patch");

        /// Can the label-use be patched to a veneer?
        /// Rust: fn supports_veneer(self) -> bool;
        pub const supportsVeneer = if (@hasDecl(Self, "supportsVeneer"))
            Self.supportsVeneer
        else
            @compileError("MachInstLabelUse requires supportsVeneer");

        /// How many bytes are needed for a veneer?
        /// Rust: fn veneer_size(self) -> CodeOffset;
        pub const veneerSize = if (@hasDecl(Self, "veneerSize"))
            Self.veneerSize
        else
            @compileError("MachInstLabelUse requires veneerSize");

        /// What's the largest possible veneer that may be generated?
        /// Rust: fn worst_case_veneer_size() -> CodeOffset;
        pub const worstCaseVeneerSize = if (@hasDecl(Self, "worstCaseVeneerSize"))
            Self.worstCaseVeneerSize
        else
            @compileError("MachInstLabelUse requires worstCaseVeneerSize");

        /// Generate a veneer.
        /// Rust: fn generate_veneer(self, buffer: &mut [u8], veneer_offset: CodeOffset) -> (CodeOffset, Self);
        pub const generateVeneer = if (@hasDecl(Self, "generateVeneer"))
            Self.generateVeneer
        else
            @compileError("MachInstLabelUse requires generateVeneer");

        /// Returns the corresponding label-use for the relocation specified.
        /// Rust: fn from_reloc(reloc: Reloc, addend: Addend) -> Option<Self>;
        pub const fromReloc = if (@hasDecl(Self, "fromReloc"))
            Self.fromReloc
        else
            @compileError("MachInstLabelUse requires fromReloc");
    };
}

// ============================================================================
// MachInstEmit trait (lines 346-359)
// ============================================================================

/// A trait describing the ability to encode a MachInst into binary machine code.
/// Rust: pub trait MachInstEmit: MachInst { ... }
pub fn MachInstEmit(comptime Self: type, comptime StateType: type, comptime InfoType: type, comptime BufferType: type) type {
    return struct {
        /// Emit the instruction.
        /// Rust: fn emit(&self, code: &mut MachBuffer<Self>, info: &Self::Info, state: &mut Self::State);
        pub const emit = if (@hasDecl(Self, "emit"))
            Self.emit
        else
            @compileError("MachInstEmit requires emit");

        /// Pretty-print the instruction.
        /// Rust: fn pretty_print_inst(&self, state: &mut Self::State) -> String;
        pub const prettyPrintInst = if (@hasDecl(Self, "prettyPrintInst"))
            Self.prettyPrintInst
        else
            @compileError("MachInstEmit requires prettyPrintInst");

        // Associate types for documentation
        pub const State = StateType;
        pub const Info = InfoType;
        pub const Buffer = BufferType;
    };
}

// ============================================================================
// MachInstEmitState trait (lines 361-387)
// ============================================================================

/// A trait describing the emission state carried between MachInsts when
/// emitting a function body.
/// Rust: pub trait MachInstEmitState<I: VCodeInst>: Default + Clone + Debug { ... }
pub fn MachInstEmitState(comptime Self: type, comptime FrameLayoutType: type) type {
    return struct {
        /// Create a new emission state given the ABI object.
        /// Rust: fn new(abi: &Callee<I::ABIMachineSpec>, ctrl_plane: ControlPlane) -> Self;
        pub const new = if (@hasDecl(Self, "new"))
            Self.new
        else
            @compileError("MachInstEmitState requires new");

        /// Update the emission state before emitting an instruction that is a safepoint.
        /// Rust: fn pre_safepoint(&mut self, user_stack_map: Option<ir::UserStackMap>);
        pub const preSafepoint = if (@hasDecl(Self, "preSafepoint"))
            Self.preSafepoint
        else
            @compileError("MachInstEmitState requires preSafepoint");

        /// Get mutable access to the control plane.
        /// Rust: fn ctrl_plane_mut(&mut self) -> &mut ControlPlane;
        pub const ctrlPlaneMut = if (@hasDecl(Self, "ctrlPlaneMut"))
            Self.ctrlPlaneMut
        else
            @compileError("MachInstEmitState requires ctrlPlaneMut");

        /// Take ownership of the control plane.
        /// Rust: fn take_ctrl_plane(self) -> ControlPlane;
        pub const takeCtrlPlane = if (@hasDecl(Self, "takeCtrlPlane"))
            Self.takeCtrlPlane
        else
            @compileError("MachInstEmitState requires takeCtrlPlane");

        /// A hook that triggers when first emitting a new block.
        /// Rust: fn on_new_block(&mut self) {}
        pub fn onNewBlock(self: *Self) void {
            if (@hasDecl(Self, "onNewBlock")) {
                self.onNewBlock();
            }
            // Default: do nothing
        }

        /// The FrameLayout for the function currently being compiled.
        /// Rust: fn frame_layout(&self) -> &FrameLayout;
        pub const frameLayout = if (@hasDecl(Self, "frameLayout"))
            Self.frameLayout
        else
            @compileError("MachInstEmitState requires frameLayout");

        pub const FrameLayout = FrameLayoutType;
    };
}

// ============================================================================
// CompilePhase marker types (from buffer.rs, referenced in line 393)
// ============================================================================

/// Status of a compiled artifact that needs patching before being used.
pub const Stencil = struct {};

/// Status of a compiled artifact ready to use.
pub const Final = struct {};

// ============================================================================
// ValueLabelsRanges (from value_label.rs, line 55)
// ============================================================================

/// Debug info: value labels to registers/stackslots at code offsets.
/// This is a map from ValueLabel to a list of (CodeOffset range, location).
pub const ValueLabelsRanges = struct {
    /// Internal storage.
    data: std.ArrayListUnmanaged(ValueLabelRange),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub const ValueLabelRange = struct {
        label: u32,
        start: CodeOffset,
        end: CodeOffset,
        loc: ValueLoc,
    };

    pub const ValueLoc = union(enum) {
        reg: Reg,
        stack: i32,
    };

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .data = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.data.deinit(self.allocator);
    }

    pub fn default(allocator: std.mem.Allocator) Self {
        return init(allocator);
    }
};

// ============================================================================
// CompiledCodeBase (lines 389-412)
// ============================================================================

/// The result of a `MachBackend::compile_function()` call. Contains machine
/// code (as bytes) and a disassembly, if requested.
///
/// Rust: pub struct CompiledCodeBase<T: CompilePhase> { ... }
pub fn CompiledCodeBase(comptime T: type, comptime MachBufferFinalizedType: type) type {
    _ = T; // Used for compile-phase distinction
    return struct {
        /// Machine code.
        buffer: MachBufferFinalizedType,
        /// Disassembly, if requested.
        vcode: ?[]const u8,
        /// Debug info: value labels to registers/stackslots at code offsets.
        value_labels_ranges: ValueLabelsRanges,
        /// Basic-block layout info: block start offsets.
        /// This info is generated only if the `machine_code_cfg_info` flag is set.
        bb_starts: std.ArrayListUnmanaged(CodeOffset),
        /// Basic-block layout info: block edges. Each edge is `(from, to)`.
        /// This info is generated only if the `machine_code_cfg_info` flag is set.
        bb_edges: std.ArrayListUnmanaged(struct { from: CodeOffset, to: CodeOffset }),
        /// Allocator for internal storage.
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, buffer: MachBufferFinalizedType) Self {
            return .{
                .buffer = buffer,
                .vcode = null,
                .value_labels_ranges = ValueLabelsRanges.init(allocator),
                .bb_starts = .{},
                .bb_edges = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.value_labels_ranges.deinit();
            self.bb_starts.deinit(self.allocator);
            self.bb_edges.deinit(self.allocator);
        }

        /// Get a `CodeInfo` describing section sizes from this compilation result.
        /// Rust: pub fn code_info(&self) -> CodeInfo { ... }
        pub fn codeInfo(self: *const Self) CodeInfo {
            return .{ .total_size = self.buffer.totalSize() };
        }

        /// Returns a reference to the machine code generated for this function compilation.
        /// Rust: pub fn code_buffer(&self) -> &[u8] { self.buffer.data() }
        pub fn codeBuffer(self: *const Self) []const u8 {
            return self.buffer.data();
        }

        /// Get the code layout info.
        /// Rust (on CompiledCode): pub fn get_code_bb_layout(&self) -> ... { ... }
        pub fn getCodeBbLayout(self: *const Self) struct {
            starts: []const CodeOffset,
            edges: []const struct { from: CodeOffset, to: CodeOffset },
        } {
            return .{
                .starts = self.bb_starts.items,
                .edges = self.bb_edges.items,
            };
        }
    };
}

// ============================================================================
// TextSectionBuilder trait (lines 579-629)
// ============================================================================

/// An object that can be used to create the text section of an executable.
///
/// This primarily handles resolving relative relocations at
/// text-section-assembly time rather than at load/link time. This
/// architecture-specific logic is sort of like a linker, but only for one
/// object file at a time.
///
/// Rust: pub trait TextSectionBuilder { ... }
pub fn TextSectionBuilder(comptime Self: type) type {
    return struct {
        /// Appends `data` to the text section with the `align` specified.
        ///
        /// If `labeled` is `true` then this also binds the appended data to the
        /// `n`th label for how many times this has been called with `labeled:
        /// true`. The label target can be passed as the `target` argument to
        /// `resolve_reloc`.
        ///
        /// This function returns the offset at which the data was placed in the
        /// text section.
        ///
        /// Rust: fn append(&mut self, labeled: bool, data: &[u8], align: u32, ctrl_plane: &mut ControlPlane) -> u64;
        pub const append = if (@hasDecl(Self, "append"))
            Self.append
        else
            @compileError("TextSectionBuilder requires append");

        /// Attempts to resolve a relocation for this function.
        ///
        /// The `offset` is the offset of the relocation, within the text section.
        /// The `reloc` is the kind of relocation.
        /// The `addend` is the value to add to the relocation.
        /// The `target` is the labeled function that is the target of this
        /// relocation.
        ///
        /// If this builder does not know how to handle `reloc` then this function
        /// will return `false`. Otherwise this function will return `true` and this
        /// relocation will be resolved in the final bytes returned by `finish`.
        ///
        /// Rust: fn resolve_reloc(&mut self, offset: u64, reloc: Reloc, addend: Addend, target: usize) -> bool;
        pub const resolveReloc = if (@hasDecl(Self, "resolveReloc"))
            Self.resolveReloc
        else
            @compileError("TextSectionBuilder requires resolveReloc");

        /// A debug-only option which is used to force veneers.
        /// Rust: fn force_veneers(&mut self);
        pub const forceVeneers = if (@hasDecl(Self, "forceVeneers"))
            Self.forceVeneers
        else
            @compileError("TextSectionBuilder requires forceVeneers");

        /// Write the `data` provided at `offset`, for example when resolving a relocation.
        /// Rust: fn write(&mut self, offset: u64, data: &[u8]);
        pub const write = if (@hasDecl(Self, "write"))
            Self.write
        else
            @compileError("TextSectionBuilder requires write");

        /// Completes this text section, filling out any final details, and returns
        /// the bytes of the text section.
        /// Rust: fn finish(&mut self, ctrl_plane: &mut ControlPlane) -> Vec<u8>;
        pub const finish = if (@hasDecl(Self, "finish"))
            Self.finish
        else
            @compileError("TextSectionBuilder requires finish");
    };
}

// ============================================================================
// Tests
// ============================================================================

test "CallType enum" {
    const testing = std.testing;

    const ct = CallType.None;
    try testing.expectEqual(CallType.None, ct);

    const ct2 = CallType.Regular;
    try testing.expectEqual(CallType.Regular, ct2);

    const ct3 = CallType.TailCall;
    try testing.expectEqual(CallType.TailCall, ct3);
}

test "FunctionCalls update" {
    const testing = std.testing;

    var fc = FunctionCalls.None;
    try testing.expectEqual(FunctionCalls.None, fc);

    fc.update(.None);
    try testing.expectEqual(FunctionCalls.None, fc);

    fc.update(.TailCall);
    try testing.expectEqual(FunctionCalls.TailOnly, fc);

    fc.update(.Regular);
    try testing.expectEqual(FunctionCalls.Regular, fc);

    // Regular stays Regular
    fc.update(.None);
    try testing.expectEqual(FunctionCalls.Regular, fc);

    fc.update(.TailCall);
    try testing.expectEqual(FunctionCalls.Regular, fc);
}

test "MachTerminator enum" {
    const testing = std.testing;

    const mt = MachTerminator.None;
    try testing.expectEqual(MachTerminator.None, mt);

    const mt2 = MachTerminator.Ret;
    try testing.expectEqual(MachTerminator.Ret, mt2);

    const mt3 = MachTerminator.RetCall;
    try testing.expectEqual(MachTerminator.RetCall, mt3);

    const mt4 = MachTerminator.Branch;
    try testing.expectEqual(MachTerminator.Branch, mt4);
}

test "MachLabel" {
    const testing = std.testing;

    const label = MachLabel.init(42);
    try testing.expectEqual(@as(u32, 42), label.getIndex());
    try testing.expect(label.isValid());

    const invalid = MachLabel.invalid();
    try testing.expect(!invalid.isValid());

    const block_label = MachLabel.fromBlock(BlockIndex.init(5));
    try testing.expectEqual(@as(u32, 5), block_label.getIndex());
}

test "BlockIndex" {
    const testing = std.testing;

    const idx = BlockIndex.init(10);
    try testing.expectEqual(@as(usize, 10), idx.index());
    try testing.expect(idx.isValid());

    const invalid = BlockIndex.invalid();
    try testing.expect(!invalid.isValid());
}

test "Type" {
    const testing = std.testing;

    try testing.expectEqual(@as(u16, 8), Type.I8.bits());
    try testing.expectEqual(@as(u16, 16), Type.I16.bits());
    try testing.expectEqual(@as(u16, 32), Type.I32.bits());
    try testing.expectEqual(@as(u16, 64), Type.I64.bits());
    try testing.expectEqual(@as(u16, 128), Type.I128.bits());

    try testing.expectEqual(@as(u8, 1), Type.I8.bytes());
    try testing.expectEqual(@as(u8, 4), Type.I32.bytes());
    try testing.expectEqual(@as(u8, 8), Type.I64.bytes());

    try testing.expect(Type.F32.isFloat());
    try testing.expect(Type.F64.isFloat());
    try testing.expect(!Type.I64.isFloat());

    try testing.expect(Type.I8X16.isVector());
    try testing.expect(!Type.I64.isVector());
}

test "FunctionAlignment" {
    const testing = std.testing;

    const fa = FunctionAlignment.init(4, 16);
    try testing.expectEqual(@as(u32, 4), fa.minimum);
    try testing.expectEqual(@as(u32, 16), fa.preferred);
}

test "SmallVec basic" {
    const testing = std.testing;

    var sv = SmallVec(u32, 4).init();
    try testing.expect(sv.isEmpty());
    try testing.expectEqual(@as(usize, 0), sv.len());

    // These stay within inline capacity (4), so no allocator needed
    try sv.push(1);
    try sv.push(2);
    try sv.push(3);

    try testing.expectEqual(@as(usize, 3), sv.len());
    try testing.expect(!sv.isEmpty());

    const items = sv.items();
    try testing.expectEqual(@as(u32, 1), items[0]);
    try testing.expectEqual(@as(u32, 2), items[1]);
    try testing.expectEqual(@as(u32, 3), items[2]);

    sv.clear();
    try testing.expect(sv.isEmpty());
}

test "SmallVec with allocator" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var sv = SmallVec(u32, 2).initWithAllocator(allocator);
    defer sv.deinit();

    // Fill inline capacity
    try sv.push(1);
    try sv.push(2);
    try testing.expectEqual(@as(usize, 2), sv.len());

    // This should promote to heap (exceeds inline capacity of 2)
    try sv.push(3);
    try testing.expectEqual(@as(usize, 3), sv.len());

    // Verify values
    const items = sv.items();
    try testing.expectEqual(@as(u32, 1), items[0]);
    try testing.expectEqual(@as(u32, 2), items[1]);
    try testing.expectEqual(@as(u32, 3), items[2]);
}

test "SmallVec no allocator error" {
    const testing = std.testing;

    var sv = SmallVec(u32, 2).init(); // No allocator

    // Fill inline capacity - should work
    try sv.push(1);
    try sv.push(2);

    // Try to exceed capacity without allocator - should fail
    const result = sv.push(3);
    try testing.expectError(error.NoAllocator, result);
}
