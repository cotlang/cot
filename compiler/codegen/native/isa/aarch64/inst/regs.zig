//! AArch64 ISA definitions: registers.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/regs.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const args = @import("args.zig");
pub const Reg = args.Reg;
pub const PReg = args.PReg;
pub const RegClass = args.RegClass;
pub const VReg = args.VReg;
pub const RealReg = args.RealReg;
// Import Writable from machinst for type compatibility
pub const Writable = args.Writable;
pub const OperandSize = args.OperandSize;
pub const ScalarSize = args.ScalarSize;
pub const VectorSize = args.VectorSize;

//=============================================================================
// Registers, the Universe thereof, and printing

/// The pinned register on this architecture.
/// It must be the same as Spidermonkey's HeapReg.
/// https://searchfox.org/mozilla-central/source/js/src/jit/arm64/Assembler-arm64.h#103
pub const PINNED_REG: u8 = 21;

/// Get a reference to an X-register (integer register). Do not use
/// this for xsp / xzr; we have two special registers for those.
pub fn xreg(num: u8) Reg {
    return Reg.fromPReg(xregPreg(num));
}

/// Get the given X-register as a PReg.
pub fn xregPreg(num: u8) PReg {
    std.debug.assert(num < 31);
    return PReg.init(num, .int);
}

/// Get a writable reference to an X-register.
pub fn writableXreg(num: u8) Writable(Reg) {
    return Writable(Reg).fromReg(xreg(num));
}

/// Get a reference to a V-register (vector/FP register).
pub fn vreg(num: u8) Reg {
    return Reg.fromPReg(vregPreg(num));
}

/// Get the given V-register as a PReg.
pub fn vregPreg(num: u8) PReg {
    std.debug.assert(num < 32);
    return PReg.init(num, .float);
}

/// Get a writable reference to a V-register.
pub fn writableVreg(num: u8) Writable(Reg) {
    return Writable(Reg).fromReg(vreg(num));
}

/// Get a reference to the zero-register.
pub fn zeroReg() Reg {
    const preg = PReg.init(31, .int);
    return Reg.fromVReg(VReg.init(@intCast(preg.index()), .int));
}

/// Get a writable reference to the zero-register (this discards a result).
pub fn writableZeroReg() Writable(Reg) {
    return Writable(Reg).fromReg(zeroReg());
}

/// Get a reference to the stack-pointer register.
/// XSP (stack) and XZR (zero) are logically different registers
/// which have the same hardware encoding, and whose meaning, in
/// real aarch64 instructions, is context-dependent.
///
/// We represent XZR as if it were xreg(31); XSP is xreg(31 + 32).
/// The PReg bit-packing allows 6 bits (64 registers) so we make use
/// of this extra space to distinguish xzr and xsp. We mask off the
/// 6th bit (hw_enc & 31) to get the actual hardware register encoding.
pub fn stackReg() Reg {
    const preg = PReg.init(31 + 32, .int);
    return Reg.fromVReg(VReg.init(@intCast(preg.index()), .int));
}

/// Get a writable reference to the stack-pointer register.
pub fn writableStackReg() Writable(Reg) {
    return Writable(Reg).fromReg(stackReg());
}

/// Get a reference to the link register (x30).
pub fn linkReg() Reg {
    return xreg(30);
}

/// Get a reference to the pinned register (x21).
pub fn pinnedReg() Reg {
    return xreg(PINNED_REG);
}

/// Get a writable reference to the link register.
pub fn writableLinkReg() Writable(Reg) {
    return Writable(Reg).fromReg(linkReg());
}

/// Get a reference to the frame pointer (x29).
pub fn fpReg() Reg {
    return xreg(29);
}

/// Get a writable reference to the frame pointer.
pub fn writableFpReg() Writable(Reg) {
    return Writable(Reg).fromReg(fpReg());
}

/// Get a reference to the first temporary, sometimes "spill temporary", register.
/// This register is used to compute the address of a spill slot when a direct
/// offset addressing mode from FP is not sufficient (+/- 2^11 words).
///
/// We use x16 for this (aka IP0 in the AArch64 ABI) because it's a scratch
/// register but is slightly special (used for linker veneers). We're free to
/// use it as long as we don't expect it to live through call instructions.
pub fn spilltmpReg() Reg {
    return xreg(16);
}

/// Get a writable reference to the spilltmp reg.
pub fn writableSilltmpReg() Writable(Reg) {
    return Writable(Reg).fromReg(spilltmpReg());
}

/// Get a reference to the second temp register. We need this in some edge cases
/// where we need both the spilltmp and another temporary.
///
/// We use x17 (aka IP1), the other "interprocedural"/linker-veneer scratch reg
/// that is free to use otherwise.
pub fn tmp2Reg() Reg {
    return xreg(17);
}

/// Get a writable reference to the tmp2 reg.
pub fn writableTmp2Reg() Writable(Reg) {
    return Writable(Reg).fromReg(tmp2Reg());
}

//=============================================================================
// Register printing

fn showIreg(reg: RealReg) []const u8 {
    return switch (reg.hwEnc()) {
        29 => "fp",
        30 => "lr",
        31 => "xzr",
        63 => "sp",
        else => "x?",
    };
}

fn showVreg(reg: RealReg) []const u8 {
    _ = reg;
    return "v?";
}

fn showReg(reg: Reg) []const u8 {
    if (reg.toRealReg()) |rreg| {
        return switch (rreg.class()) {
            .int => showIreg(rreg),
            .float => showVreg(rreg),
            .vector => unreachable,
        };
    } else {
        return "%v?"; // Virtual register
    }
}

pub fn prettyPrintReg(reg: Reg) []const u8 {
    return showReg(reg);
}

fn showRegSized(reg: Reg, size: OperandSize) []const u8 {
    return switch (reg.class()) {
        .int => showIregSized(reg, size),
        .float => showReg(reg),
        .vector => unreachable,
    };
}

pub fn prettyPrintRegSized(reg: Reg, size: OperandSize) []const u8 {
    return showRegSized(reg, size);
}

/// If `reg` denotes an Int-classed reg, make a best-effort attempt to show
/// its name at the 32-bit size.
pub fn showIregSized(reg: Reg, size: OperandSize) []const u8 {
    const s = showReg(reg);
    if (reg.class() != .int or !size.is32()) {
        return s;
    }
    return s;
}

/// Show a vector register used in a scalar context.
pub fn showVregScalar(reg: Reg, size: ScalarSize) []const u8 {
    _ = size;
    const s = showReg(reg);
    if (reg.class() != .float) {
        return s;
    }
    return s;
}

/// Show a vector register.
pub fn showVregVector(reg: Reg, size: VectorSize) []const u8 {
    _ = size;
    std.debug.assert(reg.class() == .float);
    return showReg(reg);
}

/// Show an indexed vector element.
pub fn showVregElement(reg: Reg, idx: u8, size: ScalarSize) []const u8 {
    _ = idx;
    _ = size;
    std.debug.assert(reg.class() == .float);
    return showReg(reg);
}

pub fn prettyPrintIreg(reg: Reg, size: OperandSize) []const u8 {
    return showIregSized(reg, size);
}

pub fn prettyPrintVregScalar(reg: Reg, size: ScalarSize) []const u8 {
    return showVregScalar(reg, size);
}

pub fn prettyPrintVregVector(reg: Reg, size: VectorSize) []const u8 {
    return showVregVector(reg, size);
}

pub fn prettyPrintVregElement(reg: Reg, idx: usize, size: ScalarSize) []const u8 {
    return showVregElement(reg, @intCast(idx), size);
}

//=============================================================================
// Tests

test "xreg creation" {
    const testing = std.testing;
    const x0 = xreg(0);
    const x30 = xreg(30);
    try testing.expectEqual(RegClass.int, x0.class());
    try testing.expectEqual(RegClass.int, x30.class());
}

test "vreg creation" {
    const testing = std.testing;
    const v0 = vreg(0);
    const v31 = vreg(31);
    try testing.expectEqual(RegClass.float, v0.class());
    try testing.expectEqual(RegClass.float, v31.class());
}

test "special registers" {
    const testing = std.testing;
    const lr = linkReg();
    const fp = fpReg();
    const sp = spilltmpReg();
    try testing.expectEqual(RegClass.int, lr.class());
    try testing.expectEqual(RegClass.int, fp.class());
    try testing.expectEqual(RegClass.int, sp.class());
}

test "pinned register" {
    const testing = std.testing;
    const pinned = pinnedReg();
    try testing.expectEqual(RegClass.int, pinned.class());
}
