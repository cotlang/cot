//! Implementation of a vanilla ABI, shared between several machines. The
//! implementation here assumes that arguments will be passed in registers
//! first, then additional args on the stack; that the stack grows downward,
//! contains a standard frame (return address and frame pointer), and the
//! compiler is otherwise free to allocate space below that with its choice of
//! layout; and that the machine has some notion of caller- and callee-save
//! registers. Most modern machines, e.g. x86-64 and AArch64, should fit this
//! mold and thus both of these backends use this shared implementation.
//!
//! See the documentation in specific machine backends for the "instantiation"
//! of this generic ABI, i.e., which registers are caller/callee-save, arguments
//! and return values, and any other special requirements.
//!
//! For now the implementation here assumes a 64-bit machine, but we intend to
//! make this 32/64-bit-generic shortly.
//!
//! # Vanilla ABI
//!
//! First, arguments and return values are passed in registers up to a certain
//! fixed count, after which they overflow onto the stack. Multiple return
//! values either fit in registers, or are returned in a separate return-value
//! area on the stack, given by a hidden extra parameter.
//!
//! # Stack Layout
//!
//! The stack looks like:
//!
//! ```plain
//!   (high address)
//!                              |          ...              |
//!                              | caller frames             |
//!                              |          ...              |
//!                              +===========================+
//!                              |          ...              |
//!                              | stack args                |
//! Canonical Frame Address -->  | (accessed via FP)         |
//!                              +---------------------------+
//! SP at function entry ----->  | return address            |
//!                              +---------------------------+
//! FP after prologue -------->  | FP (pushed by prologue)   |
//!                              +---------------------------+           -----
//!                              |          ...              |             |
//!                              | clobbered callee-saves    |             |
//! unwind-frame base -------->  | (pushed by prologue)      |             |
//!                              +---------------------------+   -----     |
//!                              |          ...              |     |       |
//!                              | spill slots               |     |       |
//!                              | (accessed via SP)         |   fixed   active
//!                              |          ...              |   frame    size
//!                              | stack slots               |  storage    |
//!                              | (accessed via SP)         |    size     |
//!                              | (alloc'd by prologue)     |     |       |
//!                              +---------------------------+   -----     |
//!                              | [alignment as needed]     |             |
//!                              |          ...              |             |
//!                              | args for largest call     |             |
//! SP ----------------------->  | (alloc'd by prologue)     |             |
//!                              +===========================+           -----
//!
//!   (low address)
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import from other machinst modules
const reg_mod = @import("reg.zig");
const inst_mod = @import("inst.zig");
const vcode_mod = @import("vcode.zig");

// Re-export key types
pub const VReg = reg_mod.VReg;
pub const Reg = reg_mod.Reg;
pub const PReg = reg_mod.PReg;
pub const RealReg = reg_mod.RealReg;
pub const VirtualReg = reg_mod.VirtualReg;
pub const Writable = reg_mod.Writable;
pub const RegClass = reg_mod.RegClass;
pub const PRegSet = reg_mod.PRegSet;
pub const SpillSlot = reg_mod.SpillSlot;

pub const Type = inst_mod.Type;
pub const MachLabel = inst_mod.MachLabel;
pub const FunctionCalls = inst_mod.FunctionCalls;

// ============================================================================
// BoundedArray - Fixed capacity array
// ============================================================================

/// A bounded array with fixed maximum capacity.
pub fn BoundedArray(comptime T: type, comptime capacity: usize) type {
    return struct {
        buffer: [capacity]T = undefined,
        len: usize = 0,

        const Self = @This();

        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            std.debug.assert(self.len < capacity);
            self.buffer[self.len] = item;
            self.len += 1;
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.len >= capacity) return error.Overflow;
            self.appendAssumeCapacity(item);
        }

        pub fn slice(self: *const Self) []const T {
            return self.buffer[0..self.len];
        }

        pub fn sliceMut(self: *Self) []T {
            return self.buffer[0..self.len];
        }

        pub fn extend(self: *Self, items: []const T) !void {
            for (items) |item| {
                try self.append(item);
            }
        }
    };
}

// ============================================================================
// SmallInstVec - A small vector of instructions
// ============================================================================

/// A small vector of instructions (with some reasonable size); appropriate for
/// a small fixed sequence implementing one operation.
pub fn SmallInstVec(comptime I: type) type {
    return BoundedArray(I, 4);
}

/// A larger vector for clobber save/restore sequences.
pub fn LargeInstVec(comptime I: type) type {
    return BoundedArray(I, 16);
}

// ============================================================================
// ArgPair and RetPair
// ============================================================================

/// A type used by backends to track argument-binding info in the "args"
/// pseudoinst. The pseudoinst holds a vec of `ArgPair` structs.
pub const ArgPair = struct {
    /// The vreg that is defined by this args pseudoinst.
    vreg: Writable(Reg),
    /// The preg that the arg arrives in; this constrains the vreg's
    /// placement at the pseudoinst.
    preg: Reg,

    pub fn init(vreg: Writable(Reg), preg: Reg) ArgPair {
        return .{ .vreg = vreg, .preg = preg };
    }
};

/// A type used by backends to track return register binding info in the "ret"
/// pseudoinst. The pseudoinst holds a vec of `RetPair` structs.
pub const RetPair = struct {
    /// The vreg that is returned by this pseudoinst.
    vreg: Reg,
    /// The preg that the arg is returned through; this constrains the vreg's
    /// placement at the pseudoinst.
    preg: Reg,

    pub fn init(vreg: Reg, preg: Reg) RetPair {
        return .{ .vreg = vreg, .preg = preg };
    }
};

// ============================================================================
// ArgumentExtension
// ============================================================================

/// How to extend an argument or return value.
pub const ArgumentExtension = enum {
    /// No extension.
    none,
    /// Unsigned extension.
    uext,
    /// Signed extension.
    sext,
};

// ============================================================================
// ArgumentPurpose
// ============================================================================

/// Special purpose of an argument.
pub const ArgumentPurpose = enum {
    /// Normal argument.
    normal,
    /// Pointer to struct return value.
    struct_return,
    /// VM context pointer.
    vm_context,
    /// Stack limit.
    stack_limit,
    /// Callee TLS.
    callee_tls,
    /// Caller TLS.
    caller_tls,
};

// ============================================================================
// ABIArgSlot
// ============================================================================

/// A location for (part of) an argument or return value. These "storage slots"
/// are specified for each register-sized part of an argument.
pub const ABIArgSlot = union(enum) {
    /// In a real register.
    reg: struct {
        /// Register that holds this arg.
        reg: RealReg,
        /// Value type of this arg.
        ty: Type,
        /// Should this arg be zero- or sign-extended?
        extension: ArgumentExtension,
    },
    /// Arguments only: on stack, at given offset from SP at entry.
    stack: struct {
        /// Offset of this arg relative to the base of stack args.
        offset: i64,
        /// Value type of this arg.
        ty: Type,
        /// Should this arg be zero- or sign-extended?
        extension: ArgumentExtension,
    },

    const Self = @This();

    /// The type of the value that will be stored in this slot.
    pub fn getType(self: Self) Type {
        return switch (self) {
            .reg => |r| r.ty,
            .stack => |s| s.ty,
        };
    }

    /// Create a register slot.
    pub fn fromReg(reg: RealReg, ty: Type, extension: ArgumentExtension) Self {
        return .{ .reg = .{ .reg = reg, .ty = ty, .extension = extension } };
    }

    /// Create a stack slot.
    pub fn fromStack(offset: i64, ty: Type, extension: ArgumentExtension) Self {
        return .{ .stack = .{ .offset = offset, .ty = ty, .extension = extension } };
    }
};

/// A vector of `ABIArgSlot`s. Inline capacity for one element because basically
/// 100% of values use one slot. Only `i128`s need multiple slots.
pub const ABIArgSlotVec = BoundedArray(ABIArgSlot, 2);

// ============================================================================
// ABIArg
// ============================================================================

/// An ABIArg is composed of one or more parts. This allows for a CLIF-level
/// Value to be passed with its parts in more than one location at the ABI
/// level. For example, a 128-bit integer may be passed in two 64-bit registers.
pub const ABIArg = union(enum) {
    /// Storage slots (registers or stack locations) for each part of the
    /// argument value.
    slots: struct {
        /// Slots, one per register part.
        slots: ABIArgSlotVec,
        /// Purpose of this arg.
        purpose: ArgumentPurpose,
    },
    /// Structure argument. We reserve stack space for it, but the CLIF-level
    /// semantics are a little weird: the value passed to the call instruction,
    /// and received in the corresponding block param, is a *pointer*.
    struct_arg: struct {
        /// Offset of this arg relative to base of stack args.
        offset: i64,
        /// Size of this arg on the stack.
        size: u64,
        /// Purpose of this arg.
        purpose: ArgumentPurpose,
    },
    /// Implicit argument. Similar to a StructArg, except that we have the
    /// target type, not a pointer type, at the CLIF-level.
    implicit_ptr_arg: struct {
        /// Register or stack slot holding a pointer to the buffer.
        pointer: ABIArgSlot,
        /// Offset of the argument buffer.
        offset: i64,
        /// Type of the implicit argument.
        ty: Type,
        /// Purpose of this arg.
        purpose: ArgumentPurpose,
    },

    const Self = @This();

    /// Create an ABIArg from one register.
    pub fn fromReg(
        reg: RealReg,
        ty: Type,
        extension: ArgumentExtension,
        purpose: ArgumentPurpose,
    ) Self {
        var slots = ABIArgSlotVec{};
        slots.appendAssumeCapacity(ABIArgSlot.fromReg(reg, ty, extension));
        return .{ .slots = .{ .slots = slots, .purpose = purpose } };
    }

    /// Create an ABIArg from one stack slot.
    pub fn fromStack(
        offset: i64,
        ty: Type,
        extension: ArgumentExtension,
        purpose: ArgumentPurpose,
    ) Self {
        var slots = ABIArgSlotVec{};
        slots.appendAssumeCapacity(ABIArgSlot.fromStack(offset, ty, extension));
        return .{ .slots = .{ .slots = slots, .purpose = purpose } };
    }

    /// Create a struct argument.
    pub fn fromStructArg(offset: i64, size: u64, purpose: ArgumentPurpose) Self {
        return .{ .struct_arg = .{ .offset = offset, .size = size, .purpose = purpose } };
    }

    /// Get the purpose of this argument.
    pub fn getPurpose(self: Self) ArgumentPurpose {
        return switch (self) {
            .slots => |s| s.purpose,
            .struct_arg => |s| s.purpose,
            .implicit_ptr_arg => |s| s.purpose,
        };
    }
};

// ============================================================================
// ArgsOrRets
// ============================================================================

/// Are we computing information about arguments or return values?
pub const ArgsOrRets = enum {
    /// Arguments.
    args,
    /// Return values.
    rets,
};

// ============================================================================
// StackAMode
// ============================================================================

/// Abstract location for a machine-specific ABI impl to translate into the
/// appropriate addressing mode.
pub const StackAMode = union(enum) {
    /// Offset into the current frame's argument area.
    incoming_arg: struct {
        offset: i64,
        size: u32,
    },
    /// Offset within the stack slots in the current frame.
    slot: i64,
    /// Offset into the callee frame's argument area.
    outgoing_arg: i64,

    const Self = @This();

    pub fn offsetBy(self: Self, offset: u32) Self {
        const off_i64: i64 = @intCast(offset);
        return switch (self) {
            .incoming_arg => |a| .{ .incoming_arg = .{
                .offset = a.offset + off_i64,
                .size = a.size,
            } },
            .slot => |s| .{ .slot = s + off_i64 },
            .outgoing_arg => |o| .{ .outgoing_arg = o + off_i64 },
        };
    }
};

// ============================================================================
// CallConv
// ============================================================================

/// Calling convention.
pub const CallConv = enum {
    /// System V AMD64 ABI.
    system_v,
    /// Fast calling convention.
    fast,
    /// Tail call convention.
    tail,
    /// Windows fastcall.
    windows_fastcall,
    /// Apple AArch64.
    apple_aarch64,
    /// Winch (WebAssembly).
    winch,
    /// Preserve all registers.
    preserve_all,
};

// ============================================================================
// ArgsAccumulator
// ============================================================================

/// Used as an out-parameter to accumulate a sequence of `ABIArg`s.
pub const ArgsAccumulator = struct {
    abi_args: *std.ArrayListUnmanaged(ABIArg),
    allocator: Allocator,
    start: usize,
    non_formal_flag: bool,

    const Self = @This();

    pub fn init(allocator: Allocator, abi_args: *std.ArrayListUnmanaged(ABIArg)) Self {
        return .{
            .abi_args = abi_args,
            .allocator = allocator,
            .start = abi_args.items.len,
            .non_formal_flag = false,
        };
    }

    pub fn push(self: *Self, arg: ABIArg) !void {
        std.debug.assert(!self.non_formal_flag);
        try self.abi_args.append(self.allocator, arg);
    }

    pub fn pushNonFormal(self: *Self, arg: ABIArg) !void {
        self.non_formal_flag = true;
        try self.abi_args.append(self.allocator, arg);
    }

    pub fn args(self: *const Self) []const ABIArg {
        return self.abi_args.items[self.start..];
    }

    pub fn argsMut(self: *Self) []ABIArg {
        return self.abi_args.items[self.start..];
    }
};

// ============================================================================
// CallInfo
// ============================================================================

/// Out-of-line data for calls, to keep the size of `Inst` down.
pub fn CallInfo(comptime T: type) type {
    return struct {
        const Self = @This();

        /// Receiver of this call.
        dest: T,
        /// Register uses of this call.
        uses: std.ArrayListUnmanaged(Reg),
        /// Register defs of this call.
        defs: std.ArrayListUnmanaged(Writable(Reg)),
        /// Registers clobbered by this call, as per its calling convention.
        clobbers: PRegSet,
        /// The calling convention of the callee.
        callee_conv: CallConv,
        /// The calling convention of the caller.
        caller_conv: CallConv,
        /// The number of bytes that the callee will pop from the stack.
        callee_pop_size: u32,
        /// Information for a try-call, if this is one.
        try_call_info: ?TryCallInfo,
        /// Whether this call is patchable.
        patchable: bool,

        /// Creates an empty set of info with no clobbers/uses/etc.
        pub fn empty(dest: T, call_conv: CallConv) Self {
            return .{
                .dest = dest,
                .uses = .{},
                .defs = .{},
                .clobbers = PRegSet.empty(),
                .callee_conv = call_conv,
                .caller_conv = call_conv,
                .callee_pop_size = 0,
                .try_call_info = null,
                .patchable = false,
            };
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.uses.deinit(allocator);
            self.defs.deinit(allocator);
            if (self.try_call_info) |*tci| {
                tci.deinit(allocator);
            }
        }
    };
}

/// Out-of-line information present on `try_call` instructions only.
pub const TryCallInfo = struct {
    /// The target to jump to on a normal return.
    continuation: MachLabel,
    /// Exception tags to catch and corresponding destination labels.
    exception_handlers: std.ArrayListUnmanaged(TryCallHandler),

    pub fn deinit(self: *TryCallInfo, allocator: Allocator) void {
        self.exception_handlers.deinit(allocator);
    }
};

/// Information about an individual handler at a try-call site.
pub const TryCallHandler = union(enum) {
    /// If the tag matches, recover at the label.
    tag: struct {
        exception_tag: u32,
        label: MachLabel,
    },
    /// Recover at the label unconditionally.
    default: MachLabel,
    /// Set the dynamic context for interpreting tags.
    context: Reg,
};

// ============================================================================
// Sig and SigData
// ============================================================================

/// The id of an ABI signature within the `SigSet`.
pub const Sig = struct {
    index: u32,

    const Self = @This();

    pub fn init(index: u32) Self {
        return .{ .index = index };
    }

    pub fn prev(self: Self) ?Self {
        if (self.index == 0) return null;
        return .{ .index = self.index - 1 };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.index == other.index;
    }
};

/// ABI information shared between body (callee) and caller.
pub const SigData = struct {
    /// Argument location ending offset.
    args_end: u32,
    /// Return-value location ending offset.
    rets_end: u32,
    /// Space on stack used to store arguments.
    sized_stack_arg_space: u32,
    /// Space on stack used to store return values.
    sized_stack_ret_space: u32,
    /// Index in `args` of the stack-return-value-area argument.
    stack_ret_arg: ?u16,
    /// Calling convention used.
    call_conv: CallConv,

    const Self = @This();

    /// Get total stack space required for arguments.
    pub fn sizedStackArgSpace(self: Self) u32 {
        return self.sized_stack_arg_space;
    }

    /// Get total stack space required for return values.
    pub fn sizedStackRetSpace(self: Self) u32 {
        return self.sized_stack_ret_space;
    }

    /// Get calling convention used.
    pub fn callConv(self: Self) CallConv {
        return self.call_conv;
    }

    /// The index of the stack-return-value-area argument, if any.
    pub fn stackRetArg(self: Self) ?u16 {
        return self.stack_ret_arg;
    }
};

// ============================================================================
// SigSet
// ============================================================================

/// A (mostly) deduplicated set of ABI signatures.
pub const SigSet = struct {
    allocator: Allocator,
    /// A single, shared allocation for all `ABIArg`s.
    abi_args: std.ArrayListUnmanaged(ABIArg),
    /// The actual ABI signatures, keyed by `Sig`.
    sigs: std.ArrayListUnmanaged(SigData),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .abi_args = .{},
            .sigs = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.abi_args.deinit(self.allocator);
        self.sigs.deinit(self.allocator);
    }

    /// Add a new signature and return its Sig.
    pub fn addSig(self: *Self, sig_data: SigData) !Sig {
        const index: u32 = @intCast(self.sigs.items.len);
        try self.sigs.append(self.allocator, sig_data);
        return Sig.init(index);
    }

    /// Get a signature by Sig.
    pub fn get(self: *const Self, sig: Sig) *const SigData {
        return &self.sigs.items[sig.index];
    }

    /// Get this signature's ABI arguments.
    pub fn args(self: *const Self, sig: Sig) []const ABIArg {
        const sig_data = self.get(sig);
        const start: usize = sig_data.rets_end;
        const end: usize = sig_data.args_end;
        return self.abi_args.items[start..end];
    }

    /// Get this signature's ABI returns.
    pub fn rets(self: *const Self, sig: Sig) []const ABIArg {
        const sig_data = self.get(sig);
        const start: usize = if (sig.prev()) |prev|
            self.get(prev).args_end
        else
            0;
        const end: usize = sig_data.rets_end;
        return self.abi_args.items[start..end];
    }

    /// Get information specifying how to pass the implicit pointer
    /// to the return-value area on the stack, if required.
    pub fn getRetArg(self: *const Self, sig: Sig) ?ABIArg {
        const sig_data = self.get(sig);
        if (sig_data.stack_ret_arg) |i| {
            return self.args(sig)[i];
        }
        return null;
    }

    /// Get information specifying how to pass one argument.
    pub fn getArg(self: *const Self, sig: Sig, idx: usize) ABIArg {
        return self.args(sig)[idx];
    }

    /// Get information specifying how to pass one return value.
    pub fn getRet(self: *const Self, sig: Sig, idx: usize) ABIArg {
        return self.rets(sig)[idx];
    }

    /// Get the number of arguments expected.
    pub fn numArgs(self: *const Self, sig: Sig) usize {
        const len = self.args(sig).len;
        if (self.get(sig).stack_ret_arg != null) {
            return len - 1;
        }
        return len;
    }

    /// Get the number of return values expected.
    pub fn numRets(self: *const Self, sig: Sig) usize {
        return self.rets(sig).len;
    }
};

// ============================================================================
// FrameLayout
// ============================================================================

/// Structure describing the layout of a function's stack frame.
pub const FrameLayout = struct {
    /// Word size in bytes.
    word_bytes: u32 = 8,

    /// Size of incoming arguments on the stack.
    incoming_args_size: u32 = 0,

    /// The size of the incoming argument area, taking into account any
    /// potential increase in size required for tail calls.
    tail_args_size: u32 = 0,

    /// Size of the "setup area", typically holding the return address
    /// and/or the saved frame pointer.
    setup_area_size: u32 = 0,

    /// Size of the area used to save callee-saved clobbered registers.
    clobber_size: u32 = 0,

    /// Storage allocated for the fixed part of the stack frame.
    fixed_frame_storage_size: u32 = 0,

    /// The size of all stackslots.
    stackslots_size: u32 = 0,

    /// Stack size to be reserved for outgoing arguments.
    outgoing_args_size: u32 = 0,

    /// Sorted list of callee-saved registers that are clobbered.
    clobbered_callee_saves: std.ArrayListUnmanaged(Writable(RealReg)) = .{},

    /// The function's call pattern classification.
    function_calls: FunctionCalls = .None,

    const Self = @This();

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.clobbered_callee_saves.deinit(allocator);
    }

    /// Split the clobbered callee-save registers into integer-class and
    /// float-class groups.
    pub fn clobberedCalleeSavesByClass(self: *const Self) struct {
        ints: []const Writable(RealReg),
        floats: []const Writable(RealReg),
    } {
        var partition_point: usize = 0;
        for (self.clobbered_callee_saves.items, 0..) |r, i| {
            if (r.toReg().class() == .int) {
                partition_point = i + 1;
            } else {
                break;
            }
        }
        return .{
            .ints = self.clobbered_callee_saves.items[0..partition_point],
            .floats = self.clobbered_callee_saves.items[partition_point..],
        };
    }

    /// The size of FP to SP while the frame is active.
    pub fn activeSize(self: Self) u32 {
        return self.outgoing_args_size + self.fixed_frame_storage_size + self.clobber_size;
    }

    /// Get the offset from the SP to the sized stack slots area.
    pub fn spToSizedStackSlots(self: Self) u32 {
        return self.outgoing_args_size;
    }

    /// Get the offset of a spill slot from SP.
    pub fn spillslotOffset(self: Self, spillslot: SpillSlot) i64 {
        const islot: i64 = @intCast(spillslot.index());
        const spill_off = islot * @as(i64, self.word_bytes);
        const sp_off = @as(i64, self.stackslots_size) + spill_off;
        return sp_off;
    }

    /// Get the offset from SP up to FP.
    pub fn spToFp(self: Self) u32 {
        return self.outgoing_args_size + self.fixed_frame_storage_size + self.clobber_size;
    }
};

// ============================================================================
// StackSlot and DynamicStackSlot
// ============================================================================

/// A sized stack slot.
pub const StackSlot = struct {
    index: u32,

    pub fn init(index: u32) StackSlot {
        return .{ .index = index };
    }
};

/// A dynamic stack slot.
pub const DynamicStackSlot = struct {
    index: u32,

    pub fn init(index: u32) DynamicStackSlot {
        return .{ .index = index };
    }
};

// ============================================================================
// Callee
// ============================================================================

/// ABI object for a function body.
pub fn Callee(comptime M: type) type {
    return struct {
        const Self = @This();
        const I = M.Inst;

        allocator: Allocator,

        /// Signature id.
        sig: Sig,

        /// Offsets to each sized stackslot.
        sized_stackslots: std.ArrayListUnmanaged(u32),

        /// Total stack size of all stackslots.
        stackslots_size: u32,

        /// Stack size to be reserved for outgoing arguments.
        outgoing_args_size: u32,

        /// Initially the number of bytes originating in the callers frame
        /// where stack arguments will live.
        tail_args_size: u32,

        /// Register-argument defs.
        reg_args: std.ArrayListUnmanaged(ArgPair),

        /// Finalized frame layout for this function.
        frame_layout: ?FrameLayout,

        /// The register holding the return-area pointer, if needed.
        ret_area_ptr: ?Reg,

        /// Calling convention this function expects.
        call_conv: CallConv,

        pub fn init(allocator: Allocator, sig: Sig, call_conv: CallConv) Self {
            return .{
                .allocator = allocator,
                .sig = sig,
                .sized_stackslots = .{},
                .stackslots_size = 0,
                .outgoing_args_size = 0,
                .tail_args_size = 0,
                .reg_args = .{},
                .frame_layout = null,
                .ret_area_ptr = null,
                .call_conv = call_conv,
            };
        }

        pub fn deinit(self: *Self) void {
            self.sized_stackslots.deinit(self.allocator);
            self.reg_args.deinit(self.allocator);
            if (self.frame_layout) |*fl| {
                fl.deinit(self.allocator);
            }
        }

        /// Get the machine environment.
        pub fn machineEnv(self: *const Self) M.MachineEnv {
            return M.getMachineEnv(self.call_conv);
        }

        /// Get the spillslot size for a register class.
        pub fn getSpillslotSize(self: *const Self, rc: RegClass) u32 {
            _ = self;
            return M.getSpillslotSize(rc);
        }

        /// Get the offset of a sized stack slot.
        pub fn sizedStackslotOffsets(self: *const Self) []const u32 {
            return self.sized_stackslots.items;
        }

        /// Get the spillslot offset.
        pub fn getSpillslotOffset(self: *const Self, slot: SpillSlot) i64 {
            if (self.frame_layout) |fl| {
                return fl.spillslotOffset(slot);
            }
            return 0;
        }

        /// Get slot base to caller SP offset.
        pub fn slotBaseToCallerSpOffset(self: *const Self) u32 {
            if (self.frame_layout) |fl| {
                return fl.setup_area_size + fl.clobber_size + fl.fixed_frame_storage_size;
            }
            return 0;
        }

        /// Is forward-edge CFI enabled?
        pub fn isForwardEdgeCfiEnabled(self: *const Self) bool {
            _ = self;
            return false; // TODO: implement based on ISA flags
        }

        /// Compute the frame layout.
        pub fn computeFrameLayout(
            self: *Self,
            sigs: *const SigSet,
            num_spillslots: usize,
            clobbers: []const Writable(RealReg),
            function_calls: FunctionCalls,
        ) !void {
            _ = sigs;
            _ = num_spillslots;

            var layout = FrameLayout{
                .word_bytes = M.wordBytes(),
                .function_calls = function_calls,
            };

            // Copy clobbered registers
            try layout.clobbered_callee_saves.appendSlice(self.allocator, clobbers);

            // Sort by register class
            std.sort.pdq(Writable(RealReg), layout.clobbered_callee_saves.items, {}, struct {
                fn lessThan(_: void, a: Writable(RealReg), b: Writable(RealReg)) bool {
                    const a_class = a.toReg().class();
                    const b_class = b.toReg().class();
                    if (a_class != b_class) {
                        return @intFromEnum(a_class) < @intFromEnum(b_class);
                    }
                    return a.toReg().hwEnc() < b.toReg().hwEnc();
                }
            }.lessThan);

            self.frame_layout = layout;
        }

        /// Generate the prologue.
        pub fn genPrologue(self: *const Self) SmallInstVec(I) {
            _ = self;
            return .{};
        }

        /// Generate the epilogue.
        pub fn genEpilogue(self: *const Self) SmallInstVec(I) {
            _ = self;
            return .{};
        }

        /// Generate a spill instruction.
        pub fn genSpill(self: *const Self, to: SpillSlot, from: RealReg) I {
            _ = self;
            _ = to;
            _ = from;
            @panic("genSpill not implemented");
        }

        /// Generate a reload instruction.
        pub fn genReload(self: *const Self, to: Writable(RealReg), from: SpillSlot) I {
            _ = self;
            _ = to;
            _ = from;
            @panic("genReload not implemented");
        }

        /// Get the frame slot metadata.
        pub fn frameSlotMetadata(self: *const Self) ?*const FrameLayout {
            return if (self.frame_layout) |*fl| fl else null;
        }
    };
}

// ============================================================================
// ABIMachineSpec trait (comptime interface)
// ============================================================================

/// Check if a type implements the ABIMachineSpec interface.
pub fn isABIMachineSpec(comptime T: type) bool {
    return @hasDecl(T, "Inst") and
        @hasDecl(T, "wordBits") and
        @hasDecl(T, "wordBytes") and
        @hasDecl(T, "stackAlign") and
        @hasDecl(T, "computeArgLocs") and
        @hasDecl(T, "genLoadStack") and
        @hasDecl(T, "genStoreStack") and
        @hasDecl(T, "genMove") and
        @hasDecl(T, "genExtend") and
        @hasDecl(T, "genArgs") and
        @hasDecl(T, "genRets") and
        @hasDecl(T, "getMachineEnv") and
        @hasDecl(T, "getSpillslotSize");
}

// ============================================================================
// Utility functions
// ============================================================================

/// Round up a value to alignment.
pub fn checkedRoundUp(val: u32, mask: u32) ?u32 {
    const sum = @addWithOverflow(val, mask);
    if (sum[1] != 0) return null;
    return sum[0] & ~mask;
}

// ============================================================================
// Tests
// ============================================================================

test "ABIArgSlot basic" {
    const slot = ABIArgSlot.fromReg(
        RealReg{ .preg = PReg.init(0, .int) },
        .I64,
        .none,
    );
    try std.testing.expectEqual(Type.I64, slot.getType());
}

test "ABIArg from reg" {
    const arg = ABIArg.fromReg(
        RealReg{ .preg = PReg.init(0, .int) },
        .I64,
        .none,
        .normal,
    );
    try std.testing.expectEqual(ArgumentPurpose.normal, arg.getPurpose());
}

test "ABIArg from stack" {
    const arg = ABIArg.fromStack(8, .I32, .none, .normal);
    try std.testing.expectEqual(ArgumentPurpose.normal, arg.getPurpose());
}

test "StackAMode offsetBy" {
    const mode = StackAMode{ .slot = 100 };
    const offset_mode = mode.offsetBy(20);
    try std.testing.expectEqual(@as(i64, 120), offset_mode.slot);
}

test "Sig prev" {
    const sig = Sig.init(5);
    const prev = sig.prev().?;
    try std.testing.expectEqual(@as(u32, 4), prev.index);

    const zero_sig = Sig.init(0);
    try std.testing.expect(zero_sig.prev() == null);
}

test "SigData accessors" {
    const sig_data = SigData{
        .args_end = 10,
        .rets_end = 5,
        .sized_stack_arg_space = 32,
        .sized_stack_ret_space = 16,
        .stack_ret_arg = null,
        .call_conv = .system_v,
    };
    try std.testing.expectEqual(@as(u32, 32), sig_data.sizedStackArgSpace());
    try std.testing.expectEqual(@as(u32, 16), sig_data.sizedStackRetSpace());
    try std.testing.expectEqual(CallConv.system_v, sig_data.callConv());
}

test "FrameLayout activeSize" {
    const layout = FrameLayout{
        .outgoing_args_size = 32,
        .fixed_frame_storage_size = 64,
        .clobber_size = 16,
    };
    try std.testing.expectEqual(@as(u32, 112), layout.activeSize());
}

test "FrameLayout spillslotOffset" {
    const layout = FrameLayout{
        .word_bytes = 8,
        .stackslots_size = 64,
    };
    const slot = SpillSlot.new(2);
    try std.testing.expectEqual(@as(i64, 80), layout.spillslotOffset(slot));
}

test "checkedRoundUp" {
    try std.testing.expectEqual(@as(?u32, 16), checkedRoundUp(10, 7));
    try std.testing.expectEqual(@as(?u32, 8), checkedRoundUp(8, 7));
    try std.testing.expectEqual(@as(?u32, 8), checkedRoundUp(1, 7));
}

test "ArgPair and RetPair" {
    const vreg = VReg.init(100, .int);
    const reg = Reg.fromVReg(vreg);
    const preg = PReg.init(0, .int);
    const preg_reg = Reg.fromPReg(preg);

    const arg_pair = ArgPair.init(Writable(Reg).fromReg(reg), preg_reg);
    try std.testing.expect(arg_pair.vreg.toReg().bits == reg.bits);

    const ret_pair = RetPair.init(reg, preg_reg);
    try std.testing.expect(ret_pair.vreg.bits == reg.bits);
}

test "SigSet basic operations" {
    const allocator = std.testing.allocator;
    var sig_set = SigSet.init(allocator);
    defer sig_set.deinit();

    const sig_data = SigData{
        .args_end = 2,
        .rets_end = 1,
        .sized_stack_arg_space = 0,
        .sized_stack_ret_space = 0,
        .stack_ret_arg = null,
        .call_conv = .system_v,
    };

    const sig = try sig_set.addSig(sig_data);
    try std.testing.expectEqual(@as(u32, 0), sig.index);

    const retrieved = sig_set.get(sig);
    try std.testing.expectEqual(@as(u32, 2), retrieved.args_end);
}
