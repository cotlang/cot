//! Implementation of a standard AArch64 ABI.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/abi.rs`
//!
//! This module implements the AArch64 Application Binary Interface (ABI),
//! which defines how functions call each other, pass arguments, return values,
//! and manage the call stack.

const std = @import("std");
const Allocator = std.mem.Allocator;

//=============================================================================
// BoundedArray implementation for Zig 0.15 compatibility
// (BoundedArray was removed in Zig 0.15)

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
            if (self.len >= capacity) return error.OutOfMemory;
            self.appendAssumeCapacity(item);
        }

        pub fn slice(self: *Self) []T {
            return self.buffer[0..self.len];
        }

        pub fn constSlice(self: *const Self) []const T {
            return self.buffer[0..self.len];
        }

        pub fn get(self: *const Self, index: usize) T {
            std.debug.assert(index < self.len);
            return self.buffer[index];
        }
    };
}

// Import instruction module types
const inst = @import("inst/mod.zig");
const regs = inst.regs;
const imms = inst.imms;
const args = inst.args;

// Re-export commonly used types
const Inst = inst.Inst;
const ALUOp = inst.ALUOp;
const AMode = inst.AMode;
const PairAMode = inst.PairAMode;
const OperandSize = inst.OperandSize;
const ScalarSize = inst.ScalarSize;
const Cond = inst.Cond;
const CondBrKind = inst.CondBrKind;
const BranchTarget = inst.BranchTarget;
const ExtendOp = inst.ExtendOp;
const MachLabel = args.MachLabel;
const MemFlags = inst.MemFlags;
const APIKey = inst.APIKey;
const BranchTargetType = inst.BranchTargetType;
const Type = inst.Type;
const Imm12 = inst.Imm12;
const SImm7Scaled = inst.SImm7Scaled;
const SImm9 = inst.SImm9;

// Register types
const Reg = args.Reg;
const PReg = args.PReg;
const VReg = args.VReg;
const RealReg = args.RealReg;
const RegClass = args.RegClass;
const Writable = regs.Writable;

// Register constructors
const xreg = regs.xreg;
const vreg = regs.vreg;
const xregPreg = regs.xregPreg;
const vregPreg = regs.vregPreg;
const writableXreg = regs.writableXreg;
const writableVreg = regs.writableVreg;
const zeroReg = regs.zeroReg;
const writableZeroReg = regs.writableZeroReg;
const stackReg = regs.stackReg;
const writableStackReg = regs.writableStackReg;
const linkReg = regs.linkReg;
const writableLinkReg = regs.writableLinkReg;
const fpReg = regs.fpReg;
const writableFpReg = regs.writableFpReg;
const spilltmpReg = regs.spilltmpReg;
const writableSpilltmpReg = regs.writableSilltmpReg;
const tmp2Reg = regs.tmp2Reg;
const writableTmp2Reg = regs.writableTmp2Reg;
const PINNED_REG = regs.PINNED_REG;

//=============================================================================
// Calling convention definitions

/// Calling convention enum.
pub const CallConv = enum {
    /// System V ABI (standard Unix/Linux ABI).
    system_v,
    /// Apple's variant of the AAPCS64.
    apple_aarch64,
    /// Tail call convention.
    tail,
    /// Winch (WebAssembly interpreter) calling convention.
    winch,
    /// Preserve all registers calling convention.
    preserve_all,
};

/// Arguments or return values?
pub const ArgsOrRets = enum {
    args,
    rets,
};

/// Whether a function is a leaf (calls no other functions) or not.
pub const FunctionCalls = enum {
    none,
    some,
};

/// Argument extension mode.
pub const ArgumentExtension = enum {
    none,
    uext,
    sext,
};

/// Argument purpose.
pub const ArgumentPurpose = enum {
    normal,
    struct_return,
    struct_argument,
    vmctx,
    sigid,
    callee_tlsbase,
    callee_vm_runtime_limits,
};

//=============================================================================
// ABI parameter types

/// An ABI parameter (argument or return value).
pub const AbiParam = struct {
    /// The value type.
    value_type: Type,
    /// Extension mode.
    extension: ArgumentExtension = .none,
    /// Purpose of this argument.
    purpose: ArgumentPurpose = .normal,
};

/// A slot in an ABI argument: either in a register or on the stack.
pub const ABIArgSlot = union(enum) {
    /// In a register.
    reg: struct {
        reg: RealReg,
        ty: Type,
        extension: ArgumentExtension,
    },
    /// On the stack.
    stack: struct {
        offset: i64,
        ty: Type,
        extension: ArgumentExtension,
    },
};

/// An ABI argument (collection of slots).
pub const ABIArg = union(enum) {
    /// Slots for this argument.
    slots: struct {
        slots: BoundedArray(ABIArgSlot, 4),
        purpose: ArgumentPurpose,
    },

    /// Create a single-slot register argument.
    pub fn reg(r: RealReg, ty: Type, extension: ArgumentExtension, purpose: ArgumentPurpose) ABIArg {
        var slots_array = BoundedArray(ABIArgSlot, 4){};
        slots_array.appendAssumeCapacity(.{
            .reg = .{
                .reg = r,
                .ty = ty,
                .extension = extension,
            },
        });
        return .{
            .slots = .{
                .slots = slots_array,
                .purpose = purpose,
            },
        };
    }
};

/// Stack addressing mode for ABI.
pub const StackAMode = union(enum) {
    /// Incoming argument on stack.
    incoming_arg: struct { off: i64, stack_args_size: u32 },
    /// Stack slot offset.
    slot: i64,
    /// Outgoing argument.
    outgoing_arg: i64,

    /// Convert to AMode.
    pub fn toAMode(self: StackAMode) AMode {
        return switch (self) {
            .incoming_arg => |ia| .{ .incoming_arg = .{ .offset = @as(i64, ia.stack_args_size) - ia.off } },
            .slot => |off| .{ .slot_offset = .{ .offset = off } },
            .outgoing_arg => |off| .{ .sp_offset = .{ .offset = off } },
        };
    }
};

//=============================================================================
// Frame layout

/// Layout of the stack frame.
pub const FrameLayout = struct {
    /// Word size in bytes (8 for AArch64).
    word_bytes: u32 = 8,
    /// Size of incoming arguments on the stack.
    incoming_args_size: u32 = 0,
    /// Size of tail call arguments area.
    tail_args_size: u32 = 0,
    /// Size of the setup area (FP/LR save).
    setup_area_size: u32 = 0,
    /// Size of clobber save area.
    clobber_size: u32 = 0,
    /// Size of fixed frame storage.
    fixed_frame_storage_size: u32 = 0,
    /// Size of stack slots.
    stackslots_size: u32 = 0,
    /// Size of outgoing arguments area.
    outgoing_args_size: u32 = 0,
    /// Clobbered callee-saved registers.
    clobbered_callee_saves: BoundedArray(Writable(RealReg), 32) = .{},
    /// Function calls status.
    function_calls: FunctionCalls = .none,

    /// Get the clobbered callee-saves by class.
    pub fn clobberedCalleeSavesByClass(self: *const FrameLayout) struct {
        int_regs: []const Writable(RealReg),
        vec_regs: []const Writable(RealReg),
    } {
        var int_count: usize = 0;
        var vec_start: usize = 0;

        // Count int registers first
        for (self.clobbered_callee_saves.constSlice(), 0..) |reg, i| {
            if (reg.toReg().class() == .int) {
                int_count += 1;
            } else {
                vec_start = i;
                break;
            }
        }

        if (int_count == self.clobbered_callee_saves.len) {
            // All int, no vec
            return .{
                .int_regs = self.clobbered_callee_saves.constSlice(),
                .vec_regs = &[_]Writable(RealReg){},
            };
        }

        return .{
            .int_regs = self.clobbered_callee_saves.constSlice()[0..int_count],
            .vec_regs = self.clobbered_callee_saves.constSlice()[vec_start..],
        };
    }
};

//=============================================================================
// Physical register set

/// A set of physical registers.
pub const PRegSet = struct {
    /// Bitmap of integer registers (x0-x30).
    int_regs: u32 = 0,
    /// Bitmap of vector/float registers (v0-v31).
    vec_regs: u32 = 0,

    pub const empty = PRegSet{};

    /// Add a register to the set.
    pub fn with(self: PRegSet, preg: PReg) PRegSet {
        var result = self;
        switch (preg.class()) {
            .int => result.int_regs |= @as(u32, 1) << @intCast(preg.index()),
            .float => result.vec_regs |= @as(u32, 1) << @intCast(preg.index()),
            .vector => unreachable,
        }
        return result;
    }

    /// Check if a register is in the set.
    pub fn contains(self: PRegSet, preg: PReg) bool {
        return switch (preg.class()) {
            .int => (self.int_regs & (@as(u32, 1) << @intCast(preg.index()))) != 0,
            .float => (self.vec_regs & (@as(u32, 1) << @intCast(preg.index()))) != 0,
            .vector => false,
        };
    }
};

//=============================================================================
// Machine environment

/// Machine environment for register allocation.
pub const MachineEnv = struct {
    /// Preferred registers by class (caller-saved, easy to allocate).
    preferred_regs_by_class: [3]BoundedArray(PReg, 32),
    /// Non-preferred registers by class (callee-saved, need to be preserved).
    non_preferred_regs_by_class: [3]BoundedArray(PReg, 16),
    /// Fixed stack slots.
    fixed_stack_slots: []const u32,
    /// Scratch register by class.
    scratch_by_class: [3]?PReg,
};

//=============================================================================
// ISA flags

/// AArch64 ISA-specific flags.
pub const IsaFlags = struct {
    /// Use pointer authentication (PAC).
    use_pac: bool = false,
    /// Use branch target identification (BTI).
    use_bti: bool = false,
    /// Sign return addresses.
    sign_return_address: bool = false,
    /// Sign all return addresses (not just non-leaf).
    sign_return_address_all: bool = false,
    /// Use B key for return address signing.
    sign_return_address_with_bkey: bool = false,
    /// Hardware supports PAC instructions.
    has_pauth: bool = false,

    pub const default = IsaFlags{};

    pub fn isForwardEdgeCfiEnabled(self: IsaFlags) bool {
        return self.use_bti;
    }
};

/// General settings flags.
pub const SettingsFlags = struct {
    /// Preserve frame pointers.
    preserve_frame_pointers: bool = true,
    /// Enable pinned register.
    enable_pinned_reg: bool = false,
    /// Generate unwind info.
    unwind_info: bool = true,
    /// Enable multi-return implicit sret.
    enable_multi_ret_implicit_sret: bool = false,
    /// Enable LLVM ABI extensions.
    enable_llvm_abi_extensions: bool = false,

    pub const default = SettingsFlags{};
};

//=============================================================================
// Signature

/// Function signature.
pub const Signature = struct {
    /// Parameters.
    params: []const AbiParam,
    /// Return values.
    returns: []const AbiParam,
};

//=============================================================================
// Unwind instructions

/// Unwind instruction for DWARF/etc.
pub const UnwindInst = union(enum) {
    /// Define a new frame.
    define_new_frame: struct {
        offset_downward_to_clobbers: u32,
        offset_upward_to_caller_sp: u32,
    },
    /// Push frame registers (FP, LR).
    push_frame_regs: struct {
        offset_upward_to_caller_sp: u32,
    },
    /// Save a register.
    save_reg: struct {
        clobber_offset: u32,
        reg: RealReg,
    },
    /// Stack allocation.
    stack_alloc: struct {
        size: u32,
    },
    /// Set pointer authentication state.
    aarch64_set_pointer_auth: struct {
        return_addresses: bool,
    },
};

//=============================================================================
// AArch64MachineDeps - main ABI implementation

/// AArch64-specific ABI behavior. This struct just serves as an implementation
/// point for the trait; it is never actually instantiated.
pub const AArch64MachineDeps = struct {
    const Self = @This();

    /// Instruction type.
    pub const I = Inst;

    /// ISA flags type.
    pub const F = IsaFlags;

    /// This is the limit for the size of argument and return-value areas on the
    /// stack. We place a reasonable limit here to avoid integer overflow issues
    /// with 32-bit arithmetic: for now, 128 MB.
    pub const STACK_ARG_RET_SIZE_LIMIT: u32 = 128 * 1024 * 1024;

    /// Word size in bits.
    pub fn wordBits() u32 {
        return 64;
    }

    /// Word type.
    pub fn wordType() Type {
        return Type.i64;
    }

    /// Return required stack alignment in bytes.
    pub fn stackAlign(call_conv: CallConv) u32 {
        _ = call_conv;
        return 16;
    }

    /// Get the return class for a type.
    pub fn rcForType(value_type: Type) struct { rcs: []const RegClass, reg_types: []const Type } {
        // Simple implementation: all integer types use int class, floats use float class
        const static_int: []const RegClass = &[_]RegClass{.int};
        const static_float: []const RegClass = &[_]RegClass{.float};
        const static_int_pair: []const RegClass = &[_]RegClass{ .int, .int };

        const static_i64: []const Type = &[_]Type{Type.i64};
        const static_f64: []const Type = &[_]Type{Type.f64};
        const static_i64_pair: []const Type = &[_]Type{ Type.i64, Type.i64 };

        return switch (value_type.kind) {
            .int8, .int16, .int32, .int64 => .{ .rcs = static_int, .reg_types = static_i64 },
            .int128 => .{ .rcs = static_int_pair, .reg_types = static_i64_pair },
            .float16, .float32, .float64 => .{ .rcs = static_float, .reg_types = static_f64 },
            .float128 => .{ .rcs = static_float, .reg_types = static_f64 },
            .vec_i8x16 => .{ .rcs = static_float, .reg_types = static_f64 },
        };
    }

    /// Compute argument/return value locations.
    ///
    /// See AArch64 ABI (https://github.com/ARM-software/abi-aa/blob/2021Q1/aapcs64/aapcs64.rst#64parameter-passing), sections 6.4.
    ///
    /// MacOS aarch64 is slightly different, see also
    /// https://developer.apple.com/documentation/xcode/writing_arm64_code_for_apple_platforms.
    pub fn computeArgLocs(
        call_conv: CallConv,
        flags: SettingsFlags,
        params: []const AbiParam,
        args_or_rets: ArgsOrRets,
        add_ret_area_ptr: bool,
        allocator: Allocator,
    ) !struct { stack_size: u32, ret_area_ptr_idx: ?usize, args: std.ArrayListUnmanaged(ABIArg) } {
        const is_apple_cc = call_conv == .apple_aarch64;
        const is_winch_return = call_conv == .winch and args_or_rets == .rets;

        var next_xreg: u8 = if (call_conv == .tail) 2 else 0;
        var next_vreg: u8 = 0;
        var next_stack: u32 = 0;

        const max_per_class_reg_vals: u8 = 8; // x0-x7 and v0-v7
        var remaining_reg_vals: u8 = 16;

        var args_list = std.ArrayListUnmanaged(ABIArg){};

        // Handle return area pointer
        var ret_area_ptr: ?ABIArg = null;
        if (add_ret_area_ptr) {
            std.debug.assert(args_or_rets == .args);
            if (call_conv != .winch) {
                // In the AAPCS64 calling convention the return area pointer is
                // stored in x8.
                ret_area_ptr = ABIArg.reg(
                    RealReg{ .hw_enc_val = 8, .cls = .int },
                    Type.i64,
                    .none,
                    .normal,
                );
            } else {
                // Use x0 for the return area pointer in the Winch calling convention
                next_xreg += 1;
                ret_area_ptr = ABIArg.reg(
                    RealReg{ .hw_enc_val = 0, .cls = .int },
                    Type.i64,
                    .none,
                    .normal,
                );
            }
        }

        for (params, 0..) |param, i| {
            // Check for unsupported F128 on Apple
            if (is_apple_cc and param.value_type.kind == .float128 and !flags.enable_llvm_abi_extensions) {
                return error.UnsupportedF128OnApple;
            }

            const rc_info = rcForType(param.value_type);
            const rcs = rc_info.rcs;
            const reg_types = rc_info.reg_types;

            // Handle StructReturn
            if (param.purpose == .struct_return) {
                std.debug.assert(call_conv != .tail); // Not supported for tail yet
                std.debug.assert(param.value_type.kind == .int64);

                var slots_array = BoundedArray(ABIArgSlot, 4){};
                slots_array.appendAssumeCapacity(.{
                    .reg = .{
                        .reg = RealReg{ .hw_enc_val = 8, .cls = .int },
                        .ty = Type.i64,
                        .extension = param.extension,
                    },
                });

                try args_list.append(allocator, .{
                    .slots = .{
                        .slots = slots_array,
                        .purpose = .struct_return,
                    },
                });
                continue;
            }

            // Handle StructArgument
            if (param.purpose == .struct_argument) {
                return error.StructArgumentNotSupported;
            }

            // Handle multi-register params (i128)
            const is_multi_reg = rcs.len >= 2;
            if (is_multi_reg) {
                std.debug.assert(rcs.len == 2);
                std.debug.assert(rcs[0] == .int and rcs[1] == .int);

                const reg_class_space = max_per_class_reg_vals - next_xreg;
                const reg_space = remaining_reg_vals;

                if (reg_space >= 2 and reg_class_space >= 2) {
                    // The aarch64 ABI does not allow us to start a split argument
                    // at an odd numbered register. So we need to skip one register
                    if (!is_apple_cc and (next_xreg % 2) != 0) {
                        next_xreg += 1;
                    }

                    var slots_array = BoundedArray(ABIArgSlot, 4){};
                    slots_array.appendAssumeCapacity(.{
                        .reg = .{
                            .reg = RealReg{ .hw_enc_val = next_xreg, .cls = .int },
                            .ty = reg_types[0],
                            .extension = param.extension,
                        },
                    });
                    slots_array.appendAssumeCapacity(.{
                        .reg = .{
                            .reg = RealReg{ .hw_enc_val = next_xreg + 1, .cls = .int },
                            .ty = reg_types[1],
                            .extension = param.extension,
                        },
                    });

                    try args_list.append(allocator, .{
                        .slots = .{
                            .slots = slots_array,
                            .purpose = param.purpose,
                        },
                    });

                    next_xreg += 2;
                    remaining_reg_vals -= 2;
                    continue;
                }
            } else {
                // Single Register parameters
                const rc = rcs[0];
                const next_reg = if (rc == .int) &next_xreg else &next_vreg;

                const push_to_reg = if (is_winch_return)
                    // Winch uses the first register to return the last result
                    i == params.len - 1
                else
                    // Use max_per_class_reg_vals & remaining_reg_vals otherwise
                    next_reg.* < max_per_class_reg_vals and remaining_reg_vals > 0;

                if (push_to_reg) {
                    const reg_class: RegClass = if (rc == .int) .int else .float;
                    const reg_enc = next_reg.*;

                    try args_list.append(allocator, ABIArg.reg(
                        RealReg{ .hw_enc_val = reg_enc, .cls = reg_class },
                        param.value_type,
                        param.extension,
                        param.purpose,
                    ));
                    next_reg.* += 1;
                    remaining_reg_vals -= 1;
                    continue;
                }
            }

            // Spill to the stack
            if (args_or_rets == .rets and !flags.enable_multi_ret_implicit_sret) {
                return error.TooManyReturnValues;
            }

            // Compute the stack slot's size
            var size: u32 = @intCast(param.value_type.bits() / 8);

            if (is_apple_cc or is_winch_return) {
                // MacOS and Winch aarch64 allows stack slots with
                // sizes less than 8 bytes.
            } else {
                // Every arg takes a minimum slot of 8 bytes
                size = @max(size, 8);
            }

            if (!is_winch_return) {
                // Align the stack slot
                std.debug.assert(std.math.isPowerOfTwo(size));
                next_stack = alignTo(next_stack, size);
            }

            // Build stack slot(s)
            var slots_array = BoundedArray(ABIArgSlot, 4){};
            var slot_offset: i64 = @intCast(next_stack);
            for (reg_types) |ty| {
                slots_array.appendAssumeCapacity(.{
                    .stack = .{
                        .offset = slot_offset,
                        .ty = ty,
                        .extension = param.extension,
                    },
                });
                slot_offset += @intCast(ty.bits() / 8);
            }

            try args_list.append(allocator, .{
                .slots = .{
                    .slots = slots_array,
                    .purpose = param.purpose,
                },
            });

            next_stack += size;
        }

        // Handle return area pointer
        var ret_area_ptr_idx: ?usize = null;
        if (ret_area_ptr) |ptr| {
            try args_list.append(allocator, ptr);
            ret_area_ptr_idx = args_list.items.len - 1;
        }

        // Align stack to 16 bytes
        next_stack = alignTo(next_stack, 16);

        return .{
            .stack_size = next_stack,
            .ret_area_ptr_idx = ret_area_ptr_idx,
            .args = args_list,
        };
    }

    /// Generate a load from stack.
    pub fn genLoadStack(mem: StackAMode, into_reg: Writable(Reg), ty: Type) Inst {
        return Inst.genLoad(into_reg, mem.toAMode(), ty, MemFlags.empty);
    }

    /// Generate a store to stack.
    pub fn genStoreStack(mem: StackAMode, from_reg: Reg, ty: Type) Inst {
        return Inst.genStore(mem.toAMode(), from_reg, ty, MemFlags.empty);
    }

    /// Generate a move between registers.
    pub fn genMove(to_reg: Writable(Reg), from_reg: Reg, ty: Type) Inst {
        return Inst.genMove(to_reg, from_reg, ty);
    }

    /// Generate an extend instruction.
    pub fn genExtend(
        to_reg: Writable(Reg),
        from_reg: Reg,
        signed: bool,
        from_bits: u8,
        to_bits: u8,
    ) Inst {
        std.debug.assert(from_bits < to_bits);
        return .{
            .extend = .{
                .rd = to_reg,
                .rn = from_reg,
                .signed = signed,
                .from_bits = from_bits,
                .to_bits = to_bits,
            },
        };
    }

    /// Generate an add with immediate.
    pub fn genAddImm(
        call_conv: CallConv,
        into_reg: Writable(Reg),
        from_reg: Reg,
        imm: u32,
    ) BoundedArray(Inst, 4) {
        _ = call_conv;
        var insts = BoundedArray(Inst, 4){};

        if (Imm12.maybeFromU64(@as(u64, imm))) |imm12| {
            insts.appendAssumeCapacity(.{
                .alu_rr_imm12 = .{
                    .alu_op = .add,
                    .size = .size64,
                    .rd = into_reg,
                    .rn = from_reg,
                    .imm12 = imm12,
                },
            });
        } else {
            // Need to load constant into scratch2 first
            const scratch2 = writableTmp2Reg();
            std.debug.assert(scratch2.toReg().bits != from_reg.bits);

            // Load constant
            const load_insts = loadConstant(scratch2, @as(u64, imm));
            for (load_insts.constSlice()) |load_inst| {
                insts.appendAssumeCapacity(load_inst);
            }

            // Add using extended register form
            insts.appendAssumeCapacity(.{
                .alu_rrr_extend = .{
                    .alu_op = .add,
                    .size = .size64,
                    .rd = into_reg,
                    .rn = from_reg,
                    .rm = scratch2.toReg(),
                    .extendop = .uxtx,
                },
            });
        }
        return insts;
    }

    /// Generate stack lower bound trap.
    pub fn genStackLowerBoundTrap(limit_reg: Reg) BoundedArray(Inst, 4) {
        var insts = BoundedArray(Inst, 4){};

        // Compare SP with limit: SUBS xzr, sp, limit_reg
        insts.appendAssumeCapacity(.{
            .alu_rrr_extend = .{
                .alu_op = .subs,
                .size = .size64,
                .rd = writableZeroReg(),
                .rn = stackReg(),
                .rm = limit_reg,
                .extendop = .uxtx,
            },
        });

        // Trap if below: B.LO trap
        insts.appendAssumeCapacity(.{
            .trap_if = .{
                .kind = .{ .cond = .lo },
                .trap_code = 0, // STACK_OVERFLOW
            },
        });

        return insts;
    }

    /// Get address of stack slot.
    pub fn genGetStackAddr(mem: StackAMode, into_reg: Writable(Reg)) Inst {
        return .{
            .load_addr = .{
                .rd = into_reg,
                .mem = mem.toAMode(),
            },
        };
    }

    /// Get the stack limit register.
    pub fn getStacklimitReg(call_conv: CallConv) Reg {
        _ = call_conv;
        return spilltmpReg();
    }

    /// Generate a load from base + offset.
    pub fn genLoadBaseOffset(into_reg: Writable(Reg), base: Reg, offset: i32, ty: Type) Inst {
        const mem = AMode{ .reg_offset = .{ .rn = base, .offset = @as(i64, offset), .ty = ty } };
        return Inst.genLoad(into_reg, mem, ty, MemFlags.empty);
    }

    /// Generate a store to base + offset.
    pub fn genStoreBaseOffset(base: Reg, offset: i32, from_reg: Reg, ty: Type) Inst {
        const mem = AMode{ .reg_offset = .{ .rn = base, .offset = @as(i64, offset), .ty = ty } };
        return Inst.genStore(mem, from_reg, ty, MemFlags.empty);
    }

    /// Generate SP register adjustment.
    pub fn genSpRegAdjust(amount: i32) BoundedArray(Inst, 4) {
        var ret = BoundedArray(Inst, 4){};

        if (amount == 0) {
            return ret;
        }

        const abs_amount: u64 = if (amount > 0)
            @intCast(amount)
        else
            @intCast(-amount);
        const is_sub = amount < 0;
        const alu_op: ALUOp = if (is_sub) .sub else .add;

        if (Imm12.maybeFromU64(abs_amount)) |imm12| {
            ret.appendAssumeCapacity(.{
                .alu_rr_imm12 = .{
                    .alu_op = alu_op,
                    .size = .size64,
                    .rd = writableStackReg(),
                    .rn = stackReg(),
                    .imm12 = imm12,
                },
            });
        } else {
            const tmp = writableSpilltmpReg();
            const const_insts = loadConstant(tmp, abs_amount);
            for (const_insts.constSlice()) |inst_item| {
                ret.appendAssumeCapacity(inst_item);
            }
            ret.appendAssumeCapacity(.{
                .alu_rrr_extend = .{
                    .alu_op = alu_op,
                    .size = .size64,
                    .rd = writableStackReg(),
                    .rn = stackReg(),
                    .rm = tmp.toReg(),
                    .extendop = .uxtx,
                },
            });
        }

        return ret;
    }

    /// Generate prologue frame setup.
    pub fn genPrologueFrameSetup(
        call_conv: CallConv,
        flags: SettingsFlags,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 16) {
        const setup_frame = frame_layout.setup_area_size > 0;
        var insts = BoundedArray(Inst, 16){};

        // Handle pointer authentication
        if (selectApiKey(isa_flags, call_conv, setup_frame)) |key| {
            insts.appendAssumeCapacity(.{
                // PACI instruction placeholder
                .nop4 = {},
            });
            _ = key;

            if (flags.unwind_info) {
                // Unwind instruction for pointer auth
            }
        } else {
            if (isa_flags.use_bti) {
                insts.appendAssumeCapacity(.{
                    .bti = .{ .targets = .c },
                });
            }
        }

        if (setup_frame) {
            // stp fp (x29), lr (x30), [sp, #-16]!
            insts.appendAssumeCapacity(.{
                .store_p64 = .{
                    .rt = fpReg(),
                    .rt2 = linkReg(),
                    .mem = .{
                        .sp_pre_indexed = .{
                            .simm7 = SImm7Scaled.maybeFromI64(-16, Type.i64).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });

            // mov fp (x29), sp
            // This uses the ADDI rd, rs, 0 form of `MOV` because
            // the usual encoding (`ORR`) does not work with SP.
            insts.appendAssumeCapacity(.{
                .alu_rr_imm12 = .{
                    .alu_op = .add,
                    .size = .size64,
                    .rd = writableFpReg(),
                    .rn = stackReg(),
                    .imm12 = Imm12{ .bits = 0, .shift12 = false },
                },
            });
        }

        return insts;
    }

    /// Generate epilogue frame restore.
    pub fn genEpilogueFrameRestore(
        call_conv: CallConv,
        flags: SettingsFlags,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 8) {
        _ = flags;
        _ = isa_flags;
        const setup_frame = frame_layout.setup_area_size > 0;
        var insts = BoundedArray(Inst, 8){};

        if (setup_frame) {
            // ldp fp, lr, [sp], #16
            insts.appendAssumeCapacity(.{
                .load_p64 = .{
                    .rt = writableFpReg(),
                    .rt2 = writableLinkReg(),
                    .mem = .{
                        .sp_post_indexed = .{
                            .simm7 = SImm7Scaled.maybeFromI64(16, Type.i64).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        // Handle tail call convention
        if (call_conv == .tail and frame_layout.tail_args_size > 0) {
            const adj_insts = genSpRegAdjust(@intCast(frame_layout.tail_args_size));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
        }

        return insts;
    }

    /// Generate return instruction.
    pub fn genReturn(
        call_conv: CallConv,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 2) {
        const setup_frame = frame_layout.setup_area_size > 0;
        var insts = BoundedArray(Inst, 2){};

        if (selectApiKey(isa_flags, call_conv, setup_frame)) |_| {
            // Authenticated return
            insts.appendAssumeCapacity(.ret);
        } else {
            insts.appendAssumeCapacity(.ret);
        }

        return insts;
    }

    /// Generate clobber save.
    pub fn genClobberSave(
        call_conv: CallConv,
        flags: SettingsFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 32) {
        _ = call_conv;
        var insts = BoundedArray(Inst, 32){};
        const clobbered = frame_layout.clobberedCalleeSavesByClass();
        const setup_frame = frame_layout.setup_area_size > 0;

        // Handle tail call incoming args adjustment
        const incoming_args_diff: i32 = @as(i32, @intCast(frame_layout.tail_args_size)) -
            @as(i32, @intCast(frame_layout.incoming_args_size));
        if (incoming_args_diff > 0) {
            const adj_insts = genSpRegAdjust(-incoming_args_diff);
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }

            if (setup_frame) {
                // Reload and re-store frame pointer
                insts.appendAssumeCapacity(.{
                    .uload64 = .{
                        .rd = writableFpReg(),
                        .mem = .{ .sp_offset = .{ .offset = @intCast(incoming_args_diff) } },
                        .flags = MemFlags.empty,
                    },
                });

                insts.appendAssumeCapacity(.{
                    .store_p64 = .{
                        .rt = fpReg(),
                        .rt2 = linkReg(),
                        .mem = .{
                            .signed_offset = .{
                                .reg = stackReg(),
                                .simm7 = SImm7Scaled.maybeFromI64(0, Type.i64).?,
                            },
                        },
                        .flags = MemFlags.empty,
                    },
                });

                // Keep FP in sync
                insts.appendAssumeCapacity(Inst.genMove(
                    writableFpReg(),
                    stackReg(),
                    Type.i64,
                ));
            }
        }

        // Save integer registers (in pairs when possible)
        const int_regs = clobbered.int_regs;
        var i: usize = 0;

        // Handle odd register first if present
        if (int_regs.len % 2 == 1) {
            const rd = int_regs[int_regs.len - 1];
            insts.appendAssumeCapacity(.{
                .store64 = .{
                    .rd = Reg.fromRealReg(PReg.init(rd.toReg().hw_enc_val, .int)),
                    .mem = .{
                        .sp_pre_indexed = .{
                            .simm9 = SImm9.maybeFromI64(-16).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        // Save pairs in reverse order
        while (i + 1 < int_regs.len) : (i += 2) {
            const idx = int_regs.len - 2 - i;
            if (idx + 1 < int_regs.len) {
                const rt = int_regs[idx];
                const rt2 = int_regs[idx + 1];
                insts.appendAssumeCapacity(.{
                    .store_p64 = .{
                        .rt = Reg.fromRealReg(PReg.init(rt.toReg().hw_enc_val, .int)),
                        .rt2 = Reg.fromRealReg(PReg.init(rt2.toReg().hw_enc_val, .int)),
                        .mem = .{
                            .sp_pre_indexed = .{
                                .simm7 = SImm7Scaled.maybeFromI64(-16, Type.i64).?,
                            },
                        },
                        .flags = MemFlags.empty,
                    },
                });
            }
        }

        // Save vector registers similarly
        const vec_regs = clobbered.vec_regs;
        i = 0;

        // Handle odd register first if present
        if (vec_regs.len % 2 == 1) {
            const rd = vec_regs[vec_regs.len - 1];
            insts.appendAssumeCapacity(.{
                .fpu_store64 = .{
                    .rd = Reg.fromRealReg(PReg.init(rd.toReg().hw_enc_val, .float)),
                    .mem = .{
                        .sp_pre_indexed = .{
                            .simm9 = SImm9.maybeFromI64(-16).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        // Allocate the fixed frame below the clobbers if necessary
        const stack_size = frame_layout.fixed_frame_storage_size + frame_layout.outgoing_args_size;
        if (stack_size > 0) {
            const adj_insts = genSpRegAdjust(-@as(i32, @intCast(stack_size)));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
            if (flags.unwind_info) {
                // Add unwind info
            }
        }

        return insts;
    }

    /// Generate clobber restore.
    pub fn genClobberRestore(
        call_conv: CallConv,
        flags: SettingsFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 32) {
        _ = call_conv;
        _ = flags;
        var insts = BoundedArray(Inst, 32){};
        const clobbered = frame_layout.clobberedCalleeSavesByClass();

        // Free the fixed frame if necessary
        const stack_size = frame_layout.fixed_frame_storage_size + frame_layout.outgoing_args_size;
        if (stack_size > 0) {
            const adj_insts = genSpRegAdjust(@intCast(stack_size));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
        }

        // Restore vector registers first (in pairs)
        const vec_regs = clobbered.vec_regs;
        var i: usize = 0;
        while (i + 1 < vec_regs.len) : (i += 2) {
            const rt = vec_regs[i];
            const rt2 = vec_regs[i + 1];
            insts.appendAssumeCapacity(.{
                // FpuLoadP64 - load pair of 64-bit FP registers
                .fpu_load64 = .{
                    .rd = Writable(Reg).fromReg(Reg.fromRealReg(PReg.init(rt.toReg().hw_enc_val, .float))),
                    .mem = .{
                        .sp_post_indexed = .{
                            .simm9 = SImm9.maybeFromI64(16).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
            _ = rt2;
        }

        // Handle odd vector register if present
        if (vec_regs.len % 2 == 1) {
            const rd = vec_regs[vec_regs.len - 1];
            insts.appendAssumeCapacity(.{
                .fpu_load64 = .{
                    .rd = Writable(Reg).fromReg(Reg.fromRealReg(PReg.init(rd.toReg().hw_enc_val, .float))),
                    .mem = .{
                        .sp_post_indexed = .{
                            .simm9 = SImm9.maybeFromI64(16).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        // Restore integer registers
        const int_regs = clobbered.int_regs;
        i = 0;
        while (i + 1 < int_regs.len) : (i += 2) {
            const rt = int_regs[i];
            const rt2 = int_regs[i + 1];
            insts.appendAssumeCapacity(.{
                .load_p64 = .{
                    .rt = Writable(Reg).fromReg(Reg.fromRealReg(PReg.init(rt.toReg().hw_enc_val, .int))),
                    .rt2 = Writable(Reg).fromReg(Reg.fromRealReg(PReg.init(rt2.toReg().hw_enc_val, .int))),
                    .mem = .{
                        .sp_post_indexed = .{
                            .simm7 = SImm7Scaled.maybeFromI64(16, Type.i64).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        // Handle odd integer register if present
        if (int_regs.len % 2 == 1) {
            const rd = int_regs[int_regs.len - 1];
            insts.appendAssumeCapacity(.{
                .uload64 = .{
                    .rd = Writable(Reg).fromReg(Reg.fromRealReg(PReg.init(rd.toReg().hw_enc_val, .int))),
                    .mem = .{
                        .sp_post_indexed = .{
                            .simm9 = SImm9.maybeFromI64(16).?,
                        },
                    },
                    .flags = MemFlags.empty,
                },
            });
        }

        return insts;
    }

    /// Get number of spill slots for a value.
    pub fn getNumberOfSpillslotsForValue(
        rc: RegClass,
        vector_size: u32,
        isa_flags: IsaFlags,
    ) u32 {
        _ = isa_flags;
        std.debug.assert(vector_size % 8 == 0);
        // We allocate in terms of 8-byte slots
        return switch (rc) {
            .int => 1,
            .float => vector_size / 8,
            .vector => unreachable,
        };
    }

    /// Get registers clobbered by call.
    pub fn getRegsClobberedByCall(call_conv: CallConv, is_exception: bool) PRegSet {
        return switch (call_conv) {
            .tail => if (is_exception) ALL_CLOBBERS else DEFAULT_AAPCS_CLOBBERS,
            .winch => if (is_exception) ALL_CLOBBERS else WINCH_CLOBBERS,
            .preserve_all => if (is_exception) ALL_CLOBBERS else NO_CLOBBERS,
            .system_v => DEFAULT_AAPCS_CLOBBERS,
            else => if (is_exception) DEFAULT_AAPCS_CLOBBERS else DEFAULT_AAPCS_CLOBBERS,
        };
    }

    /// Get extension mode for a calling convention.
    pub fn getExtMode(
        call_conv: CallConv,
        specified: ArgumentExtension,
    ) ArgumentExtension {
        if (call_conv == .apple_aarch64) {
            return specified;
        } else {
            return .none;
        }
    }

    /// Compute frame layout.
    pub fn computeFrameLayout(
        call_conv: CallConv,
        flags: SettingsFlags,
        sig: Signature,
        clobbered_regs: []const Writable(RealReg),
        function_calls: FunctionCalls,
        incoming_args_size: u32,
        tail_args_size: u32,
        stackslots_size: u32,
        fixed_frame_storage_size: u32,
        outgoing_args_size: u32,
    ) FrameLayout {
        // Filter to callee-saved registers
        var callee_saves = BoundedArray(Writable(RealReg), 32){};
        for (clobbered_regs) |reg| {
            if (isRegSavedInPrologue(call_conv, flags.enable_pinned_reg, sig, reg.toReg())) {
                callee_saves.appendAssumeCapacity(reg);
            }
        }

        // Sort registers for deterministic output
        std.mem.sort(
            Writable(RealReg),
            callee_saves.slice(),
            {},
            struct {
                fn cmp(_: void, a: Writable(RealReg), b: Writable(RealReg)) bool {
                    const a_enc = a.toReg().hw_enc_val;
                    const b_enc = b.toReg().hw_enc_val;
                    return a_enc < b_enc;
                }
            }.cmp,
        );

        // Compute clobber size
        const clobber_size = computeClobberSize(callee_saves.constSlice());

        // Compute setup area size
        const setup_area_size: u32 = if (flags.preserve_frame_pointers or
            function_calls != .none or
            incoming_args_size > 0 or
            clobber_size > 0 or
            fixed_frame_storage_size > 0)
            16 // FP, LR
        else
            0;

        return FrameLayout{
            .word_bytes = 8,
            .incoming_args_size = incoming_args_size,
            .tail_args_size = tail_args_size,
            .setup_area_size = setup_area_size,
            .clobber_size = clobber_size,
            .fixed_frame_storage_size = fixed_frame_storage_size,
            .stackslots_size = stackslots_size,
            .outgoing_args_size = outgoing_args_size,
            .clobbered_callee_saves = callee_saves,
            .function_calls = function_calls,
        };
    }

    /// Get return value temp register.
    pub fn retvalTempReg(call_conv_of_callee: CallConv) Writable(Reg) {
        _ = call_conv_of_callee;
        // Use x9 as a temp if needed: clobbered, not a retval.
        return writableXreg(9);
    }

    /// Get exception payload registers.
    pub fn exceptionPayloadRegs(call_conv: CallConv) []const Reg {
        const payload_regs = &[_]Reg{ xreg(0), xreg(1) };
        return switch (call_conv) {
            .system_v, .tail, .preserve_all => payload_regs,
            else => &[_]Reg{},
        };
    }

    /// Select PAC API key.
    pub fn selectApiKey(
        isa_flags: IsaFlags,
        call_conv: CallConv,
        setup_frame: bool,
    ) ?APIKey {
        if (isa_flags.sign_return_address and (setup_frame or isa_flags.sign_return_address_all)) {
            if (isa_flags.sign_return_address_with_bkey) {
                return if (call_conv == .tail) .bz else .bsp;
            } else {
                return if (call_conv == .tail) .az else .asp;
            }
        }
        return null;
    }

    /// Generate probestack (unrolled version).
    pub fn genProbestackUnroll(guard_size: u32, probe_count: u32) BoundedArray(Inst, 16) {
        var insts = BoundedArray(Inst, 16){};

        var i: u32 = 0;
        while (i < probe_count) : (i += 1) {
            // Adjust SP
            const adj = genSpRegAdjust(-@as(i32, @intCast(guard_size)));
            for (adj.constSlice()) |inst_item| {
                insts.appendAssumeCapacity(inst_item);
            }

            // Store zero to probe
            insts.appendAssumeCapacity(Inst.genStore(
                AMode{ .sp_offset = .{ .offset = 0 } },
                zeroReg(),
                Type.i32,
                MemFlags.empty,
            ));
        }

        // Restore SP
        const restore = genSpRegAdjust(@intCast(guard_size * probe_count));
        for (restore.constSlice()) |inst_item| {
            insts.appendAssumeCapacity(inst_item);
        }

        return insts;
    }

    /// Generate inline probestack.
    pub fn genInlineProbestack(
        call_conv: CallConv,
        frame_size: u32,
        guard_size: u32,
    ) BoundedArray(Inst, 16) {
        _ = call_conv;
        const PROBE_MAX_UNROLL: u32 = 3;

        const probe_count = frame_size / guard_size;
        if (probe_count == 0) {
            return BoundedArray(Inst, 16){};
        } else if (probe_count <= PROBE_MAX_UNROLL) {
            return genProbestackUnroll(guard_size, probe_count);
        } else {
            // Loop version - requires StackProbeLoop instruction
            // TODO: implement loop version with StackProbeLoop
            return BoundedArray(Inst, 16){};
        }
    }
};

//=============================================================================
// Helper functions

/// Align value to alignment.
fn alignTo(value: u32, alignment: u32) u32 {
    return (value + alignment - 1) & ~(alignment - 1);
}

/// Compute clobber size.
fn computeClobberSize(clobbered_callee_saves: []const Writable(RealReg)) u32 {
    var int_regs: u32 = 0;
    var vec_regs: u32 = 0;

    for (clobbered_callee_saves) |reg| {
        switch (reg.toReg().class()) {
            .int => int_regs += 1,
            .float => vec_regs += 1,
            .vector => unreachable,
        }
    }

    // Round up to multiple of 2, to keep 16-byte stack alignment
    const int_save_bytes = (int_regs + (int_regs & 1)) * 8;

    // AAPCS64 mandates saving only the bottom 8 bytes of vector registers
    const vec_reg_size: u32 = 8;
    const vec_save_padding = vec_regs & 1;
    const vec_save_bytes = (vec_regs + vec_save_padding) * vec_reg_size;

    return int_save_bytes + vec_save_bytes;
}

/// Is the given register saved in the prologue if clobbered?
fn isRegSavedInPrologue(
    call_conv: CallConv,
    enable_pinned_reg: bool,
    sig: Signature,
    r: RealReg,
) bool {
    if (call_conv == .preserve_all) {
        return true;
    }

    // Check if function uses dynamic vector types
    var save_z_regs = false;
    for (sig.params) |p| {
        if (p.value_type.isVector()) {
            save_z_regs = true;
            break;
        }
    }

    return switch (r.class()) {
        .int => {
            // x19 - x28 inclusive are callee-saves
            // However, x21 is the pinned reg if enable_pinned_reg is set
            if (enable_pinned_reg and r.hwEnc() == PINNED_REG) {
                return false;
            }
            return r.hwEnc() >= 19 and r.hwEnc() <= 28;
        },
        .float => {
            if (save_z_regs) {
                // z8-z23 for SVE
                return r.hwEnc() >= 8 and r.hwEnc() <= 23;
            } else {
                // v8 - v15 inclusive are callee-saves
                return r.hwEnc() >= 8 and r.hwEnc() <= 15;
            }
        },
        .vector => unreachable,
    };
}

/// Load a constant into a register.
fn loadConstant(rd: Writable(Reg), value: u64) BoundedArray(Inst, 4) {
    var insts = BoundedArray(Inst, 4){};

    // Simple case: fits in a single MOVZ
    if (value <= 0xFFFF) {
        insts.appendAssumeCapacity(.{
            .mov_wide = .{
                .op = .movz,
                .rd = rd,
                .imm = imms.MoveWideConst{
                    .bits_val = @truncate(value),
                    .shift = 0,
                },
                .size = .size64,
            },
        });
        return insts;
    }

    // Multi-instruction case using MOVZ + MOVK
    var remaining = value;
    var first = true;
    var shift: u8 = 0;

    while (remaining != 0 or first) : (shift += 16) {
        const hw: u16 = @truncate(remaining);
        remaining >>= 16;

        if (hw != 0 or first) {
            if (first) {
                insts.appendAssumeCapacity(.{
                    .mov_wide = .{
                        .op = .movz,
                        .rd = rd,
                        .imm = imms.MoveWideConst{
                            .bits_val = hw,
                            .shift = shift / 16,
                        },
                        .size = .size64,
                    },
                });
                first = false;
            } else {
                insts.appendAssumeCapacity(.{
                    .movk = .{
                        .rd = rd,
                        .rn = rd.toReg(),
                        .imm = imms.MoveWideConst{
                            .bits_val = hw,
                            .shift = shift / 16,
                        },
                        .size = .size64,
                    },
                });
            }
        }

        if (shift >= 48) break;
    }

    return insts;
}

//=============================================================================
// Clobber register sets

/// Default AAPCS64 clobbers (caller-saved registers).
pub const DEFAULT_AAPCS_CLOBBERS: PRegSet = blk: {
    var set = PRegSet.empty;
    // x0 - x17 inclusive are caller-saves
    var i: u8 = 0;
    while (i <= 17) : (i += 1) {
        set = set.with(xregPreg(i));
    }
    // v0 - v31 inclusive are caller-saves (conservatively)
    i = 0;
    while (i < 32) : (i += 1) {
        set = set.with(vregPreg(i));
    }
    break :blk set;
};

/// Winch calling convention clobbers.
pub const WINCH_CLOBBERS: PRegSet = blk: {
    var set = PRegSet.empty;
    // x0 - x17 are caller-saved
    var i: u8 = 0;
    while (i <= 17) : (i += 1) {
        set = set.with(xregPreg(i));
    }
    // x19 - x27 are also caller-saved in Winch
    i = 19;
    while (i <= 27) : (i += 1) {
        set = set.with(xregPreg(i));
    }
    // All vregs are caller-saved
    i = 0;
    while (i < 32) : (i += 1) {
        set = set.with(vregPreg(i));
    }
    break :blk set;
};

/// All clobbers (for exceptions).
pub const ALL_CLOBBERS: PRegSet = blk: {
    var set = PRegSet.empty;
    // x0 - x28 inclusive
    var i: u8 = 0;
    while (i <= 28) : (i += 1) {
        set = set.with(xregPreg(i));
    }
    // v0 - v31 inclusive
    i = 0;
    while (i < 32) : (i += 1) {
        set = set.with(vregPreg(i));
    }
    break :blk set;
};

/// No clobbers (for preserve_all).
pub const NO_CLOBBERS: PRegSet = PRegSet.empty;

//=============================================================================
// Machine environment creation

/// Create the register environment for register allocation.
pub fn createRegEnv(enable_pinned_reg: bool) MachineEnv {
    var env = MachineEnv{
        .preferred_regs_by_class = .{
            BoundedArray(PReg, 32){}, // int
            BoundedArray(PReg, 32){}, // float
            BoundedArray(PReg, 32){}, // vector (unused)
        },
        .non_preferred_regs_by_class = .{
            BoundedArray(PReg, 16){}, // int
            BoundedArray(PReg, 16){}, // float
            BoundedArray(PReg, 16){}, // vector (unused)
        },
        .fixed_stack_slots = &[_]u32{},
        .scratch_by_class = .{ null, null, null },
    };

    // Preferred integer registers: x0-x15 (caller-saved)
    var i: u8 = 0;
    while (i <= 15) : (i += 1) {
        env.preferred_regs_by_class[0].appendAssumeCapacity(xregPreg(i));
    }
    // x16 and x17 are spilltmp and tmp2
    // x18 is platform register (not allocatable)

    // Preferred float registers: v0-v7, v16-v31 (caller-saved)
    i = 0;
    while (i <= 7) : (i += 1) {
        env.preferred_regs_by_class[1].appendAssumeCapacity(vregPreg(i));
    }
    i = 16;
    while (i < 32) : (i += 1) {
        env.preferred_regs_by_class[1].appendAssumeCapacity(vregPreg(i));
    }

    // Non-preferred integer registers: x19-x28 (callee-saved)
    i = 19;
    while (i <= 28) : (i += 1) {
        if (!enable_pinned_reg or i != PINNED_REG) {
            env.non_preferred_regs_by_class[0].appendAssumeCapacity(xregPreg(i));
        }
    }

    // Non-preferred float registers: v8-v15 (callee-saved)
    i = 8;
    while (i <= 15) : (i += 1) {
        env.non_preferred_regs_by_class[1].appendAssumeCapacity(vregPreg(i));
    }

    // Add pinned reg to non-preferred if not enabled
    if (!enable_pinned_reg) {
        std.debug.assert(PINNED_REG == 21);
        // Already added above in the loop
    }

    return env;
}

//=============================================================================
// Tests

test "AArch64MachineDeps basic" {
    const testing = std.testing;
    try testing.expectEqual(@as(u32, 64), AArch64MachineDeps.wordBits());
    try testing.expectEqual(@as(u32, 16), AArch64MachineDeps.stackAlign(.system_v));
}

test "computeClobberSize" {
    const testing = std.testing;

    // No clobbers
    try testing.expectEqual(@as(u32, 0), computeClobberSize(&[_]Writable(RealReg){}));

    // One integer register - rounds up to 16 bytes
    const one_int = [_]Writable(RealReg){
        Writable(RealReg){ .reg = RealReg{ .hw_enc_val = 19, .cls = .int } },
    };
    try testing.expectEqual(@as(u32, 16), computeClobberSize(&one_int));

    // Two integer registers - 16 bytes
    const two_int = [_]Writable(RealReg){
        Writable(RealReg){ .reg = RealReg{ .hw_enc_val = 19, .cls = .int } },
        Writable(RealReg){ .reg = RealReg{ .hw_enc_val = 20, .cls = .int } },
    };
    try testing.expectEqual(@as(u32, 16), computeClobberSize(&two_int));
}

test "isRegSavedInPrologue" {
    const testing = std.testing;
    const sig = Signature{ .params = &[_]AbiParam{}, .returns = &[_]AbiParam{} };

    // x19-x28 are callee-saved
    try testing.expect(isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 19, .cls = .int }));
    try testing.expect(isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 28, .cls = .int }));

    // x0-x18 are caller-saved (not saved in prologue)
    try testing.expect(!isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 0, .cls = .int }));
    try testing.expect(!isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 17, .cls = .int }));

    // v8-v15 are callee-saved
    try testing.expect(isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 8, .cls = .float }));
    try testing.expect(isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 15, .cls = .float }));

    // v0-v7 are caller-saved
    try testing.expect(!isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 0, .cls = .float }));
    try testing.expect(!isRegSavedInPrologue(.system_v, false, sig, RealReg{ .hw_enc_val = 7, .cls = .float }));
}

test "alignTo" {
    const testing = std.testing;
    try testing.expectEqual(@as(u32, 0), alignTo(0, 16));
    try testing.expectEqual(@as(u32, 16), alignTo(1, 16));
    try testing.expectEqual(@as(u32, 16), alignTo(15, 16));
    try testing.expectEqual(@as(u32, 16), alignTo(16, 16));
    try testing.expectEqual(@as(u32, 32), alignTo(17, 16));
}

test "PRegSet operations" {
    const testing = std.testing;
    var set = PRegSet.empty;

    try testing.expect(!set.contains(xregPreg(0)));
    set = set.with(xregPreg(0));
    try testing.expect(set.contains(xregPreg(0)));
    try testing.expect(!set.contains(xregPreg(1)));

    set = set.with(vregPreg(5));
    try testing.expect(set.contains(vregPreg(5)));
    try testing.expect(!set.contains(vregPreg(6)));
}

test "loadConstant" {
    const testing = std.testing;
    const rd = writableXreg(0);

    // Small constant - single MOVZ
    const small = loadConstant(rd, 42);
    try testing.expectEqual(@as(usize, 1), small.len);

    // Large constant - multiple instructions
    const large = loadConstant(rd, 0x1234_5678_9ABC_DEF0);
    try testing.expect(large.len > 1);
}

test "DEFAULT_AAPCS_CLOBBERS" {
    const testing = std.testing;

    // x0-x17 should be in the set
    try testing.expect(DEFAULT_AAPCS_CLOBBERS.contains(xregPreg(0)));
    try testing.expect(DEFAULT_AAPCS_CLOBBERS.contains(xregPreg(17)));

    // x19-x28 should NOT be in the set (callee-saved)
    try testing.expect(!DEFAULT_AAPCS_CLOBBERS.contains(xregPreg(19)));
    try testing.expect(!DEFAULT_AAPCS_CLOBBERS.contains(xregPreg(28)));

    // All vector registers are in the set (conservatively)
    try testing.expect(DEFAULT_AAPCS_CLOBBERS.contains(vregPreg(0)));
    try testing.expect(DEFAULT_AAPCS_CLOBBERS.contains(vregPreg(31)));
}

test "createRegEnv" {
    const testing = std.testing;

    const env = createRegEnv(false);

    // Should have preferred int regs (x0-x15)
    try testing.expectEqual(@as(usize, 16), env.preferred_regs_by_class[0].len);

    // Should have non-preferred int regs (x19-x28 = 10 regs)
    try testing.expectEqual(@as(usize, 10), env.non_preferred_regs_by_class[0].len);

    // With pinned reg enabled, x21 is excluded from non-preferred
    const env_pinned = createRegEnv(true);
    try testing.expectEqual(@as(usize, 9), env_pinned.non_preferred_regs_by_class[0].len);
}
