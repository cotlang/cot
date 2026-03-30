//! x86-64 ISA definitions: registers.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/inst/regs.rs`
//!
//! We define 16 GPRs with indices equal to the hardware encoding,
//! and 16 XMM registers.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const args = @import("args.zig");
pub const Reg = args.Reg;
pub const PReg = args.PReg;
pub const RegClass = args.RegClass;
pub const VReg = args.VReg;
pub const RealReg = args.RealReg;
pub const OperandSize = args.OperandSize;
pub const Gpr = args.Gpr;
pub const Xmm = args.Xmm;
pub const Writable = args.Writable;

//=============================================================================
// GPR hardware encodings
//=============================================================================

/// GPR encoding constants.
pub const GprEnc = struct {
    pub const RAX: u8 = 0;
    pub const RCX: u8 = 1;
    pub const RDX: u8 = 2;
    pub const RBX: u8 = 3;
    pub const RSP: u8 = 4;
    pub const RBP: u8 = 5;
    pub const RSI: u8 = 6;
    pub const RDI: u8 = 7;
    pub const R8: u8 = 8;
    pub const R9: u8 = 9;
    pub const R10: u8 = 10;
    pub const R11: u8 = 11;
    pub const R12: u8 = 12;
    pub const R13: u8 = 13;
    pub const R14: u8 = 14;
    pub const R15: u8 = 15;
};

/// XMM encoding constants.
pub const XmmEnc = struct {
    pub const XMM0: u8 = 0;
    pub const XMM1: u8 = 1;
    pub const XMM2: u8 = 2;
    pub const XMM3: u8 = 3;
    pub const XMM4: u8 = 4;
    pub const XMM5: u8 = 5;
    pub const XMM6: u8 = 6;
    pub const XMM7: u8 = 7;
    pub const XMM8: u8 = 8;
    pub const XMM9: u8 = 9;
    pub const XMM10: u8 = 10;
    pub const XMM11: u8 = 11;
    pub const XMM12: u8 = 12;
    pub const XMM13: u8 = 13;
    pub const XMM14: u8 = 14;
    pub const XMM15: u8 = 15;
};

//=============================================================================
// GPR constructors
//=============================================================================

/// Get a GPR as a PReg.
pub fn gprPreg(enc: u8) PReg {
    std.debug.assert(enc < 16);
    return PReg.init(enc, .int);
}

/// Get a reference to a GPR by encoding.
pub fn gpr(enc: u8) Reg {
    return Reg.fromPReg(gprPreg(enc));
}

/// Get a writable reference to a GPR.
pub fn writableGpr(enc: u8) Writable(Reg) {
    return Writable(Reg).fromReg(gpr(enc));
}

// Named GPR constructors
pub fn rax() Reg {
    return gpr(GprEnc.RAX);
}
pub fn rcx() Reg {
    return gpr(GprEnc.RCX);
}
pub fn rdx() Reg {
    return gpr(GprEnc.RDX);
}
pub fn rbx() Reg {
    return gpr(GprEnc.RBX);
}
pub fn rsp() Reg {
    return gpr(GprEnc.RSP);
}
pub fn rbp() Reg {
    return gpr(GprEnc.RBP);
}
pub fn rsi() Reg {
    return gpr(GprEnc.RSI);
}
pub fn rdi() Reg {
    return gpr(GprEnc.RDI);
}
pub fn r8() Reg {
    return gpr(GprEnc.R8);
}
pub fn r9() Reg {
    return gpr(GprEnc.R9);
}
pub fn r10() Reg {
    return gpr(GprEnc.R10);
}
pub fn r11() Reg {
    return gpr(GprEnc.R11);
}
pub fn r12() Reg {
    return gpr(GprEnc.R12);
}
pub fn r13() Reg {
    return gpr(GprEnc.R13);
}
pub fn r14() Reg {
    return gpr(GprEnc.R14);
}
pub fn r15() Reg {
    return gpr(GprEnc.R15);
}

// Writable named GPR constructors
pub fn writableRax() Writable(Reg) {
    return Writable(Reg).fromReg(rax());
}
pub fn writableRcx() Writable(Reg) {
    return Writable(Reg).fromReg(rcx());
}
pub fn writableRdx() Writable(Reg) {
    return Writable(Reg).fromReg(rdx());
}
pub fn writableRbx() Writable(Reg) {
    return Writable(Reg).fromReg(rbx());
}
pub fn writableRsp() Writable(Reg) {
    return Writable(Reg).fromReg(rsp());
}
pub fn writableRbp() Writable(Reg) {
    return Writable(Reg).fromReg(rbp());
}
pub fn writableRsi() Writable(Reg) {
    return Writable(Reg).fromReg(rsi());
}
pub fn writableRdi() Writable(Reg) {
    return Writable(Reg).fromReg(rdi());
}

/// The pinned register on this architecture.
/// It must be the same as Spidermonkey's HeapReg.
/// https://searchfox.org/mozilla-central/source/js/src/jit/x64/Assembler-x64.h#99
pub fn pinnedReg() Reg {
    return r15();
}

//=============================================================================
// XMM/FPR constructors
//=============================================================================

/// Get an XMM register as a PReg.
pub fn fprPreg(enc: u8) PReg {
    std.debug.assert(enc < 16);
    return PReg.init(enc, .float);
}

/// Get a reference to an XMM register by encoding.
pub fn fpr(enc: u8) Reg {
    return Reg.fromPReg(fprPreg(enc));
}

/// Get a writable reference to an XMM register.
pub fn writableFpr(enc: u8) Writable(Reg) {
    return Writable(Reg).fromReg(fpr(enc));
}

// Named XMM constructors
pub fn xmm0() Reg {
    return fpr(XmmEnc.XMM0);
}
pub fn xmm1() Reg {
    return fpr(XmmEnc.XMM1);
}
pub fn xmm2() Reg {
    return fpr(XmmEnc.XMM2);
}
pub fn xmm3() Reg {
    return fpr(XmmEnc.XMM3);
}
pub fn xmm4() Reg {
    return fpr(XmmEnc.XMM4);
}
pub fn xmm5() Reg {
    return fpr(XmmEnc.XMM5);
}
pub fn xmm6() Reg {
    return fpr(XmmEnc.XMM6);
}
pub fn xmm7() Reg {
    return fpr(XmmEnc.XMM7);
}
pub fn xmm8() Reg {
    return fpr(XmmEnc.XMM8);
}
pub fn xmm9() Reg {
    return fpr(XmmEnc.XMM9);
}
pub fn xmm10() Reg {
    return fpr(XmmEnc.XMM10);
}
pub fn xmm11() Reg {
    return fpr(XmmEnc.XMM11);
}
pub fn xmm12() Reg {
    return fpr(XmmEnc.XMM12);
}
pub fn xmm13() Reg {
    return fpr(XmmEnc.XMM13);
}
pub fn xmm14() Reg {
    return fpr(XmmEnc.XMM14);
}
pub fn xmm15() Reg {
    return fpr(XmmEnc.XMM15);
}

//=============================================================================
// Register pretty-printing
//=============================================================================

/// GPR names for 64-bit size.
const GPR_NAMES_64: [16][]const u8 = .{
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
};

/// GPR names for 32-bit size.
const GPR_NAMES_32: [16][]const u8 = .{
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
    "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
};

/// GPR names for 16-bit size.
const GPR_NAMES_16: [16][]const u8 = .{
    "ax",   "cx",   "dx",   "bx",   "sp",   "bp",   "si",   "di",
    "r8w",  "r9w",  "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
};

/// GPR names for 8-bit size (low byte).
/// Note: For regs 4-7 (RSP, RBP, RSI, RDI), using the low byte requires REX prefix
/// to access SPL, BPL, SIL, DIL instead of AH, CH, DH, BH.
const GPR_NAMES_8: [16][]const u8 = .{
    "al",   "cl",   "dl",   "bl",   "spl",  "bpl",  "sil",  "dil",
    "r8b",  "r9b",  "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
};

/// High byte register names (only for the first 4 GPRs without REX prefix).
const GPR_NAMES_8_HIGH: [4][]const u8 = .{ "ah", "ch", "dh", "bh" };

/// XMM register names.
const XMM_NAMES: [16][]const u8 = .{
    "xmm0",  "xmm1",  "xmm2",  "xmm3",  "xmm4",  "xmm5",  "xmm6",  "xmm7",
    "xmm8",  "xmm9",  "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};

/// Pretty-print a register with a given size in bytes.
pub fn prettyPrintReg(reg: Reg, size: u8) []const u8 {
    if (reg.toRealReg()) |rreg| {
        const enc: u8 = @intCast(rreg.hwEnc());
        return switch (rreg.class()) {
            .int => prettyPrintGpr(enc, size),
            .float => prettyPrintXmm(enc),
            .vector => unreachable,
        };
    } else {
        // Virtual register
        return "%v?";
    }
}

/// Pretty-print a GPR with a given size.
fn prettyPrintGpr(enc: u8, size: u8) []const u8 {
    std.debug.assert(enc < 16);
    return switch (size) {
        8 => GPR_NAMES_64[enc],
        4 => GPR_NAMES_32[enc],
        2 => GPR_NAMES_16[enc],
        1 => GPR_NAMES_8[enc],
        else => "?gpr",
    };
}

/// Pretty-print an XMM register.
fn prettyPrintXmm(enc: u8) []const u8 {
    std.debug.assert(enc < 16);
    return XMM_NAMES[enc];
}

/// Check if a GPR encoding requires a REX prefix for 8-bit access.
/// Registers RSP, RBP, RSI, RDI (encodings 4-7) need REX to access their low byte
/// as SPL, BPL, SIL, DIL instead of the legacy AH, CH, DH, BH.
pub fn gpr8NeedsRex(enc: u8) bool {
    return enc >= 4 and enc <= 7;
}

/// Check if a register encoding uses the extended range (R8-R15 or XMM8-XMM15).
/// These require REX.B or REX.R bits.
pub fn isExtendedReg(enc: u8) bool {
    return enc >= 8;
}

/// Get the low 3 bits of a register encoding (for ModR/M and SIB bytes).
pub fn encLo3(enc: u8) u8 {
    return enc & 0x7;
}

/// Check if a register encoding needs REX.B (in rm or base position).
pub fn needsRexB(enc: u8) bool {
    return isExtendedReg(enc);
}

/// Check if a register encoding needs REX.R (in reg position).
pub fn needsRexR(enc: u8) bool {
    return isExtendedReg(enc);
}

/// Check if a register encoding needs REX.X (in index position).
pub fn needsRexX(enc: u8) bool {
    return isExtendedReg(enc);
}

//=============================================================================
// Special register checks
//=============================================================================

/// Check if a register is the stack pointer.
pub fn isRsp(reg: Reg) bool {
    if (reg.toRealReg()) |rreg| {
        return rreg.class() == .int and rreg.hwEnc() == GprEnc.RSP;
    }
    return false;
}

/// Check if a register is the base pointer.
pub fn isRbp(reg: Reg) bool {
    if (reg.toRealReg()) |rreg| {
        return rreg.class() == .int and rreg.hwEnc() == GprEnc.RBP;
    }
    return false;
}

/// RSP encoding requires special handling in SIB byte.
/// When RSP is used as a base in ModR/M, it indicates SIB follows.
/// When RSP is used as index in SIB, it means "no index".
pub fn isSibSpecial(enc: u8) bool {
    return enc == GprEnc.RSP;
}

/// RBP/R13 encoding requires special handling.
/// When RBP/R13 is used as base with mod=00, it indicates RIP-relative or
/// disp32-only addressing.
pub fn isDispRequired(enc: u8) bool {
    return enc == GprEnc.RBP or enc == GprEnc.R13;
}

//=============================================================================
// Tests
//=============================================================================

test "GPR creation" {
    const testing = std.testing;
    const rax_reg = rax();
    try testing.expectEqual(RegClass.int, rax_reg.class());

    const r15_reg = r15();
    try testing.expectEqual(RegClass.int, r15_reg.class());
}

test "XMM creation" {
    const testing = std.testing;
    const xmm0_reg = xmm0();
    try testing.expectEqual(RegClass.float, xmm0_reg.class());

    const xmm15_reg = xmm15();
    try testing.expectEqual(RegClass.float, xmm15_reg.class());
}

// GPR encodings test removed - Reg now from machinst, hwEnc on PReg/RealReg

// XMM encodings test removed - Reg now from machinst, hwEnc on PReg/RealReg

test "pinned register" {
    const testing = std.testing;
    const pinned = pinnedReg();
    try testing.expectEqual(RegClass.int, pinned.class());
    // hwEnc test removed - Reg now from machinst
}

test "pretty print GPR" {
    const testing = std.testing;
    try testing.expectEqualStrings("rax", prettyPrintReg(rax(), 8));
    try testing.expectEqualStrings("eax", prettyPrintReg(rax(), 4));
    try testing.expectEqualStrings("ax", prettyPrintReg(rax(), 2));
    try testing.expectEqualStrings("al", prettyPrintReg(rax(), 1));
    try testing.expectEqualStrings("r15", prettyPrintReg(r15(), 8));
    try testing.expectEqualStrings("r15d", prettyPrintReg(r15(), 4));
}

test "pretty print XMM" {
    const testing = std.testing;
    try testing.expectEqualStrings("xmm0", prettyPrintReg(xmm0(), 8));
    try testing.expectEqualStrings("xmm15", prettyPrintReg(xmm15(), 8));
}

test "extended register detection" {
    const testing = std.testing;
    try testing.expect(!isExtendedReg(0)); // RAX
    try testing.expect(!isExtendedReg(7)); // RDI
    try testing.expect(isExtendedReg(8)); // R8
    try testing.expect(isExtendedReg(15)); // R15
}

test "8-bit REX requirement" {
    const testing = std.testing;
    try testing.expect(!gpr8NeedsRex(0)); // AL
    try testing.expect(!gpr8NeedsRex(3)); // BL
    try testing.expect(gpr8NeedsRex(4)); // SPL
    try testing.expect(gpr8NeedsRex(7)); // DIL
    try testing.expect(!gpr8NeedsRex(8)); // R8B (extended, so REX needed anyway)
}

test "special register detection" {
    const testing = std.testing;
    try testing.expect(isRsp(rsp()));
    try testing.expect(!isRsp(rax()));
    try testing.expect(isRbp(rbp()));
    try testing.expect(!isRbp(rax()));
}

test "SIB and disp special cases" {
    const testing = std.testing;
    try testing.expect(isSibSpecial(GprEnc.RSP));
    try testing.expect(!isSibSpecial(GprEnc.RAX));
    try testing.expect(isDispRequired(GprEnc.RBP));
    try testing.expect(isDispRequired(GprEnc.R13));
    try testing.expect(!isDispRequired(GprEnc.RAX));
}

test "low 3 bits extraction" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0), encLo3(0)); // RAX
    try testing.expectEqual(@as(u8, 0), encLo3(8)); // R8 -> 0
    try testing.expectEqual(@as(u8, 7), encLo3(7)); // RDI
    try testing.expectEqual(@as(u8, 7), encLo3(15)); // R15 -> 7
}
