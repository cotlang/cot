//! Instruction formats and opcodes.
//!
//! Port of cranelift/codegen/src/ir/instructions.rs
//! Port of cranelift/codegen/src/ir/condcodes.rs
//! Port of cranelift/codegen/src/ir/trapcode.rs

const std = @import("std");

// ============================================================================
// Integer Condition Codes
// Port of cranelift/codegen/src/ir/condcodes.rs IntCC
// ============================================================================

/// Condition code for comparing integers.
///
/// This condition code is used by the `icmp` instruction to compare integer values. There are
/// separate codes for comparing the integers as signed or unsigned numbers where it makes a
/// difference.
pub const IntCC = enum {
    /// `==`.
    eq,
    /// `!=`.
    ne,
    /// Signed `<`.
    slt,
    /// Signed `>=`.
    sge,
    /// Signed `>`.
    sgt,
    /// Signed `<=`.
    sle,
    /// Unsigned `<`.
    ult,
    /// Unsigned `>=`.
    uge,
    /// Unsigned `>`.
    ugt,
    /// Unsigned `<=`.
    ule,

    const Self = @This();

    /// Get the complemented condition code of `self`.
    ///
    /// The complemented condition code produces the opposite result for all comparisons.
    /// That is, `cmp CC, x, y` is true if and only if `cmp CC.complement(), x, y` is false.
    pub fn complement(self: Self) Self {
        return switch (self) {
            .eq => .ne,
            .ne => .eq,
            .slt => .sge,
            .sge => .slt,
            .sgt => .sle,
            .sle => .sgt,
            .ult => .uge,
            .uge => .ult,
            .ugt => .ule,
            .ule => .ugt,
        };
    }

    /// Get the swapped args condition code for `self`.
    ///
    /// The swapped args condition code produces the same result as swapping `x` and `y` in the
    /// comparison. That is, `cmp CC, x, y` is the same as `cmp CC.swap_args(), y, x`.
    pub fn swapArgs(self: Self) Self {
        return switch (self) {
            .eq => .eq,
            .ne => .ne,
            .sgt => .slt,
            .sge => .sle,
            .slt => .sgt,
            .sle => .sge,
            .ugt => .ult,
            .uge => .ule,
            .ult => .ugt,
            .ule => .uge,
        };
    }

    /// Get the corresponding IntCC with the equal component removed.
    pub fn withoutEqual(self: Self) Self {
        return switch (self) {
            .sgt, .sge => .sgt,
            .slt, .sle => .slt,
            .ugt, .uge => .ugt,
            .ult, .ule => .ult,
            else => self,
        };
    }

    /// Get the corresponding IntCC with the signed component removed.
    pub fn unsigned(self: Self) Self {
        return switch (self) {
            .sgt, .ugt => .ugt,
            .sge, .uge => .uge,
            .slt, .ult => .ult,
            .sle, .ule => .ule,
            else => self,
        };
    }

    /// Get the corresponding string representation.
    pub fn toStr(self: Self) []const u8 {
        return switch (self) {
            .eq => "eq",
            .ne => "ne",
            .sgt => "sgt",
            .sge => "sge",
            .slt => "slt",
            .sle => "sle",
            .ugt => "ugt",
            .uge => "uge",
            .ult => "ult",
            .ule => "ule",
        };
    }

    /// Format for display.
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll(self.toStr());
    }
};

// ============================================================================
// Floating Point Condition Codes
// Port of cranelift/codegen/src/ir/condcodes.rs FloatCC
// ============================================================================

/// Condition code for comparing floating point numbers.
///
/// This condition code is used by the `fcmp` instruction to compare floating point values. Two
/// IEEE floating point values relate in exactly one of four ways:
///
/// 1. `UN` - unordered when either value is NaN.
/// 2. `EQ` - equal numerical value.
/// 3. `LT` - `x` is less than `y`.
/// 4. `GT` - `x` is greater than `y`.
pub const FloatCC = enum {
    /// EQ | LT | GT
    ord,
    /// UN
    uno,
    /// EQ
    eq,
    /// UN | LT | GT
    ne,
    /// LT | GT
    one,
    /// UN | EQ
    ueq,
    /// LT
    lt,
    /// LT | EQ
    le,
    /// GT
    gt,
    /// GT | EQ
    ge,
    /// UN | LT
    ult,
    /// UN | LT | EQ
    ule,
    /// UN | GT
    ugt,
    /// UN | GT | EQ
    uge,

    const Self = @This();

    /// Get the complemented condition code.
    pub fn complement(self: Self) Self {
        return switch (self) {
            .ord => .uno,
            .uno => .ord,
            .eq => .ne,
            .ne => .eq,
            .one => .ueq,
            .ueq => .one,
            .lt => .uge,
            .le => .ugt,
            .gt => .ule,
            .ge => .ult,
            .ult => .ge,
            .ule => .gt,
            .ugt => .le,
            .uge => .lt,
        };
    }

    /// Get the swapped args condition code.
    pub fn swapArgs(self: Self) Self {
        return switch (self) {
            .ord => .ord,
            .uno => .uno,
            .eq => .eq,
            .ne => .ne,
            .one => .one,
            .ueq => .ueq,
            .lt => .gt,
            .le => .ge,
            .gt => .lt,
            .ge => .le,
            .ult => .ugt,
            .ule => .uge,
            .ugt => .ult,
            .uge => .ule,
        };
    }

    /// Get the corresponding string representation.
    pub fn toStr(self: Self) []const u8 {
        return switch (self) {
            .ord => "ord",
            .uno => "uno",
            .eq => "eq",
            .ne => "ne",
            .one => "one",
            .ueq => "ueq",
            .lt => "lt",
            .le => "le",
            .gt => "gt",
            .ge => "ge",
            .ult => "ult",
            .ule => "ule",
            .ugt => "ugt",
            .uge => "uge",
        };
    }
};

// ============================================================================
// Trap Codes
// Port of cranelift/codegen/src/ir/trapcode.rs
// ============================================================================

/// A trap code describing the reason for a trap.
///
/// All trap instructions have an explicit trap code.
pub const TrapCode = enum(u8) {
    /// The current stack space was exhausted.
    stack_overflow = 251,
    /// An integer arithmetic operation caused an overflow.
    integer_overflow = 252,
    /// A `heap_addr` instruction detected an out-of-bounds error.
    heap_out_of_bounds = 253,
    /// An integer division by zero.
    integer_division_by_zero = 254,
    /// Failed float-to-int conversion.
    bad_conversion_to_integer = 255,
    /// Code that was supposed to have been unreachable was reached.
    unreachable_code_reached = 0,
    /// User-defined trap code 1.
    user1 = 1,
    /// User-defined trap code 2.
    user2 = 2,
    /// Table access out of bounds.
    table_out_of_bounds = 3,
    /// Indirect call to null funcref.
    indirect_call_to_null = 4,
    /// Indirect call signature mismatch.
    bad_signature = 5,

    const Self = @This();

    /// Get the corresponding string representation.
    pub fn toStr(self: Self) []const u8 {
        return switch (self) {
            .stack_overflow => "stk_ovf",
            .integer_overflow => "int_ovf",
            .heap_out_of_bounds => "heap_oob",
            .integer_division_by_zero => "int_divz",
            .bad_conversion_to_integer => "bad_toint",
            .unreachable_code_reached => "unreachable",
            .user1 => "user1",
            .user2 => "user2",
            .table_out_of_bounds => "table_oob",
            .indirect_call_to_null => "icall_null",
            .bad_signature => "bad_sig",
        };
    }
};

// ============================================================================
// Opcodes
// Essential opcodes for Wasm->native translation
// Based on cranelift/codegen/meta/src/shared/instructions.rs
// ============================================================================

/// An opcode for a CLIF instruction.
pub const Opcode = enum {
    // ------------------------------------------------------------------------
    // Control Flow (from define_control_flow)
    // ------------------------------------------------------------------------

    /// Unconditional jump.
    jump,
    /// Conditional branch when cond is non-zero.
    brif,
    /// Indirect branch via jump table.
    br_table,
    /// Return from the current function.
    @"return",
    /// Call a function.
    call,
    /// Call a function indirectly via signature.
    call_indirect,

    // ------------------------------------------------------------------------
    // Traps
    // ------------------------------------------------------------------------

    /// Unconditional trap.
    trap,
    /// Trap when condition is non-zero.
    trapnz,
    /// Trap when condition is zero.
    trapz,

    // ------------------------------------------------------------------------
    // Integer Arithmetic
    // ------------------------------------------------------------------------

    /// Integer constant.
    iconst,
    /// Copy a value.
    copy,
    /// Integer addition.
    iadd,
    /// Integer subtraction.
    isub,
    /// Integer negation.
    ineg,
    /// Integer multiplication.
    imul,
    /// Unsigned integer division.
    udiv,
    /// Signed integer division.
    sdiv,
    /// Unsigned integer remainder.
    urem,
    /// Signed integer remainder.
    srem,
    /// Integer addition with overflow check.
    iadd_overflow,
    /// Integer subtraction with overflow check.
    isub_overflow,
    /// Integer multiplication with overflow check.
    imul_overflow,

    // ------------------------------------------------------------------------
    // Bitwise Operations
    // ------------------------------------------------------------------------

    /// Bitwise AND.
    band,
    /// Bitwise OR.
    bor,
    /// Bitwise XOR.
    bxor,
    /// Bitwise NOT.
    bnot,
    /// Shift left.
    ishl,
    /// Unsigned shift right.
    ushr,
    /// Signed shift right.
    sshr,
    /// Rotate left.
    rotl,
    /// Rotate right.
    rotr,

    // ------------------------------------------------------------------------
    // Integer Comparison
    // ------------------------------------------------------------------------

    /// Integer comparison.
    icmp,

    // ------------------------------------------------------------------------
    // Floating Point Arithmetic
    // ------------------------------------------------------------------------

    /// 32-bit floating point constant.
    f32const,
    /// 64-bit floating point constant.
    f64const,
    /// Floating point addition.
    fadd,
    /// Floating point subtraction.
    fsub,
    /// Floating point multiplication.
    fmul,
    /// Floating point division.
    fdiv,
    /// Floating point negation.
    fneg,
    /// Floating point absolute value.
    fabs,
    /// Floating point square root.
    sqrt,
    /// Floating point comparison.
    fcmp,

    // ------------------------------------------------------------------------
    // Conversions
    // ------------------------------------------------------------------------

    /// Zero-extend integer.
    uextend,
    /// Sign-extend integer.
    sextend,
    /// Reduce integer width.
    ireduce,
    /// Bitcast (reinterpret bits).
    bitcast,
    /// Convert float to signed integer.
    fcvt_to_sint,
    /// Convert float to unsigned integer.
    fcvt_to_uint,
    /// Convert signed integer to float.
    fcvt_from_sint,
    /// Convert unsigned integer to float.
    fcvt_from_uint,
    /// Promote float to wider float.
    fpromote,
    /// Demote float to narrower float.
    fdemote,

    // ------------------------------------------------------------------------
    // Memory Operations
    // ------------------------------------------------------------------------

    /// Load from memory.
    load,
    /// Store to memory.
    store,
    /// Load from stack slot.
    stack_load,
    /// Store to stack slot.
    stack_store,

    // ------------------------------------------------------------------------
    // Global Values
    // Port of cranelift/codegen/src/ir/instructions.rs
    // ------------------------------------------------------------------------

    /// Compute the value of global GV.
    ///
    /// global_value is a pseudo-instruction that turns into machine code
    /// to compute the value of global GV. The result is the address of
    /// the global value, which can then be used with load/store instructions.
    ///
    /// Port of cranelift Opcode::GlobalValue
    global_value,

    // ------------------------------------------------------------------------
    // Select/Conditional
    // ------------------------------------------------------------------------

    /// Conditional select.
    select,

    // ------------------------------------------------------------------------
    // Miscellaneous
    // ------------------------------------------------------------------------

    /// No operation.
    nop,
    /// Function address.
    func_addr,

    const Self = @This();

    /// Check if this opcode is a branch.
    pub fn isBranch(self: Self) bool {
        return switch (self) {
            .jump, .brif, .br_table => true,
            else => false,
        };
    }

    /// Check if this opcode is a terminator.
    pub fn isTerminator(self: Self) bool {
        return switch (self) {
            .jump, .brif, .br_table, .@"return", .trap => true,
            else => false,
        };
    }

    /// Check if this opcode is a call.
    pub fn isCall(self: Self) bool {
        return switch (self) {
            .call, .call_indirect => true,
            else => false,
        };
    }

    /// Check if this opcode has side effects.
    pub fn hasSideEffects(self: Self) bool {
        return switch (self) {
            .store, .stack_store, .call, .call_indirect, .trap, .trapnz, .trapz => true,
            else => false,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "IntCC complement" {
    const testing = std.testing;

    try testing.expect(IntCC.eq.complement() == .ne);
    try testing.expect(IntCC.ne.complement() == .eq);
    try testing.expect(IntCC.slt.complement() == .sge);
    try testing.expect(IntCC.sge.complement() == .slt);
    try testing.expect(IntCC.ult.complement() == .uge);
}

test "IntCC swap_args" {
    const testing = std.testing;

    try testing.expect(IntCC.eq.swapArgs() == .eq);
    try testing.expect(IntCC.sgt.swapArgs() == .slt);
    try testing.expect(IntCC.slt.swapArgs() == .sgt);
    try testing.expect(IntCC.ugt.swapArgs() == .ult);
}

test "FloatCC complement" {
    const testing = std.testing;

    try testing.expect(FloatCC.ord.complement() == .uno);
    try testing.expect(FloatCC.uno.complement() == .ord);
    try testing.expect(FloatCC.eq.complement() == .ne);
    try testing.expect(FloatCC.lt.complement() == .uge);
}

test "FloatCC swap_args" {
    const testing = std.testing;

    try testing.expect(FloatCC.eq.swapArgs() == .eq);
    try testing.expect(FloatCC.lt.swapArgs() == .gt);
    try testing.expect(FloatCC.gt.swapArgs() == .lt);
}

test "Opcode properties" {
    const testing = std.testing;

    try testing.expect(Opcode.jump.isBranch());
    try testing.expect(Opcode.brif.isBranch());
    try testing.expect(!Opcode.iadd.isBranch());

    try testing.expect(Opcode.@"return".isTerminator());
    try testing.expect(Opcode.trap.isTerminator());
    try testing.expect(!Opcode.iadd.isTerminator());

    try testing.expect(Opcode.call.isCall());
    try testing.expect(!Opcode.iadd.isCall());

    try testing.expect(Opcode.store.hasSideEffects());
    try testing.expect(!Opcode.iadd.hasSideEffects());
}

test "TrapCode to_str" {
    const testing = std.testing;

    try testing.expectEqualStrings("stk_ovf", TrapCode.stack_overflow.toStr());
    try testing.expectEqualStrings("int_divz", TrapCode.integer_division_by_zero.toStr());
}
