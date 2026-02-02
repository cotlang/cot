# x86-64 Regs Module Audit

**Cranelift Sources:**
- `cranelift/codegen/src/isa/x64/inst/regs.rs` (176 lines)
- `cranelift/assembler-x64/src/gpr.rs` (246 lines) - GPR encoding constants

**Cot Target:** `compiler/codegen/native/isa/x64/inst/regs.zig` (478 lines)

---

## Register Encoding Constants

### Cranelift gpr.rs Encodings (gpr::enc module)

| Constant | Value | Register |
|----------|-------|----------|
| RAX | 0 | %rax |
| RCX | 1 | %rcx |
| RDX | 2 | %rdx |
| RBX | 3 | %rbx |
| RSP | 4 | %rsp |
| RBP | 5 | %rbp |
| RSI | 6 | %rsi |
| RDI | 7 | %rdi |
| R8 | 8 | %r8 |
| R9 | 9 | %r9 |
| R10 | 10 | %r10 |
| R11 | 11 | %r11 |
| R12 | 12 | %r12 |
| R13 | 13 | %r13 |
| R14 | 14 | %r14 |
| R15 | 15 | %r15 |

### Zig GprEnc (regs.zig:20-45)

| Constant | Value | Parity |
|----------|-------|--------|
| `rax` | 0 | ✅ **100%** |
| `rcx` | 1 | ✅ **100%** |
| `rdx` | 2 | ✅ **100%** |
| `rbx` | 3 | ✅ **100%** |
| `rsp` | 4 | ✅ **100%** |
| `rbp` | 5 | ✅ **100%** |
| `rsi` | 6 | ✅ **100%** |
| `rdi` | 7 | ✅ **100%** |
| `r8` | 8 | ✅ **100%** |
| `r9` | 9 | ✅ **100%** |
| `r10` | 10 | ✅ **100%** |
| `r11` | 11 | ✅ **100%** |
| `r12` | 12 | ✅ **100%** |
| `r13` | 13 | ✅ **100%** |
| `r14` | 14 | ✅ **100%** |
| `r15` | 15 | ✅ **100%** |

---

## XMM Register Encoding Constants

### Cranelift xmm.rs Encodings

| Constant | Value | Register |
|----------|-------|----------|
| XMM0 | 0 | %xmm0 |
| XMM1 | 1 | %xmm1 |
| ... | ... | ... |
| XMM15 | 15 | %xmm15 |

### Zig XmmEnc (regs.zig:50-75)

| Constant | Value | Parity |
|----------|-------|--------|
| `xmm0` | 0 | ✅ **100%** |
| `xmm1` | 1 | ✅ **100%** |
| ... | ... | ... |
| `xmm15` | 15 | ✅ **100%** |

---

## Register Constructor Functions

### Cranelift regs.rs Functions

| Function | Line | Purpose |
|----------|------|---------|
| `gpr(enc)` | 16-19 | Create GPR from encoding |
| `gpr_preg(enc)` | 20-22 | Create PReg for GPR |
| `rax()` | 24-26 | Get RAX register |
| `rcx()` | 27-29 | Get RCX register |
| ... | ... | ... |
| `r15()` | 69-71 | Get R15 register |
| `pinned_reg()` | 73-78 | Get pinned register (R15) |
| `fpr(enc)` | 80-83 | Create XMM from encoding |
| `fpr_preg(enc)` | 85-87 | Create PReg for XMM |
| `xmm0()` - `xmm15()` | 89-136 | Get XMM registers |
| `pretty_print_reg()` | 145-176 | Format register name |

### Zig regs.zig Functions

| Function | Line | Parity |
|----------|------|--------|
| `gpr(enc)` | 85-90 | ✅ **100%** |
| `gprPreg(enc)` | 93-95 | ✅ **100%** |
| `rax()` | 100 | ✅ **100%** |
| `rcx()` | 103 | ✅ **100%** |
| ... | ... | ... |
| `r15()` | 145 | ✅ **100%** |
| `pinnedReg()` | 150-152 | ✅ **100%** - Returns R15 |
| `fpr(enc)` | 160-165 | ✅ **100%** |
| `fprPreg(enc)` | 168-170 | ✅ **100%** |
| `xmm0()` - `xmm15()` | 175-222 | ✅ **100%** |
| `prettyPrintReg()` | 230-280 | ✅ **100%** |

---

## Register Class and PReg Construction

### Cranelift Pattern

```rust
const fn gpr(enc: u8) -> Reg {
    let preg = gpr_preg(enc);
    Reg::from_virtual_reg(VReg::new(preg.index(), RegClass::Int))
}

pub(crate) const fn gpr_preg(enc: u8) -> PReg {
    PReg::new(enc as usize, RegClass::Int)
}
```

### Zig Implementation

```zig
fn gpr(enc: u8) Reg {
    const preg = gprPreg(enc);
    return Reg.fromVirtualReg(VReg.new(preg.index(), .int));
}

pub fn gprPreg(enc: u8) PReg {
    return PReg.new(@intCast(enc), .int);
}
```

✅ **IDENTICAL** - Same construction pattern

---

## Pretty Print Implementation

### Cranelift pretty_print_reg() (regs.rs:145-176)

```rust
pub fn pretty_print_reg(reg: Reg, size: u8) -> String {
    if let Some(rreg) = reg.to_real_reg() {
        let enc = rreg.hw_enc();
        let name = match rreg.class() {
            RegClass::Int => {
                let size = match size {
                    8 => gpr::Size::Quadword,
                    4 => gpr::Size::Doubleword,
                    2 => gpr::Size::Word,
                    1 => gpr::Size::Byte,
                    _ => unreachable!("invalid size"),
                };
                gpr::enc::to_string(enc, size)
            }
            RegClass::Float => xmm::enc::to_string(enc),
            RegClass::Vector => unreachable!(),
        };
        name.to_string()
    } else {
        // Virtual register formatting...
    }
}
```

### Zig prettyPrintReg() (regs.zig:230-280)

```zig
pub fn prettyPrintReg(reg: Reg, size: u8) []const u8 {
    if (reg.toRealReg()) |rreg| {
        const enc = rreg.hwEnc();
        return switch (rreg.class()) {
            .int => gprToString(enc, switch (size) {
                8 => .quadword,
                4 => .doubleword,
                2 => .word,
                1 => .byte,
                else => unreachable,
            }),
            .float => xmmToString(enc),
            .vector => unreachable,
        };
    } else {
        // Virtual register formatting...
    }
}
```

✅ **IDENTICAL** - Same logic, same size handling

---

## GPR Name Lookup Tables

### Cranelift gpr::enc::to_string() (gpr.rs)

Uses static string tables for each size:
- Quadword: "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8"..."r15"
- Doubleword: "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d"..."r15d"
- Word: "ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w"..."r15w"
- Byte: "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil", "r8b"..."r15b"

### Zig gprToString() (regs.zig:285-340)

```zig
const gpr_names_64 = [_][]const u8{
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
};
const gpr_names_32 = [_][]const u8{
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
    "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
};
const gpr_names_16 = [_][]const u8{
    "ax", "cx", "dx", "bx", "sp", "bp", "si", "di",
    "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
};
const gpr_names_8 = [_][]const u8{
    "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil",
    "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
};
```

✅ **IDENTICAL** - Same name strings from Intel documentation

---

## Pinned Register

### Cranelift pinned_reg() (regs.rs:73-78)

```rust
/// The pinned register on this architecture.
/// It must be the same as Spidermonkey's HeapReg
pub(crate) const fn pinned_reg() -> Reg {
    r15()
}
```

### Zig pinnedReg() (regs.zig:150-152)

```zig
/// The pinned register on this architecture.
/// Must match Spidermonkey's HeapReg.
pub fn pinnedReg() Reg {
    return r15();
}
```

✅ **IDENTICAL** - Both use R15 as pinned register

---

## Helper Functions for Encoding

### Additional Zig Helpers (regs.zig:350-430)

| Function | Purpose | Status |
|----------|---------|--------|
| `isExtended(enc)` | Check if register needs REX.B/R | ✅ Extra helper |
| `needs8BitRex(enc)` | Check if 8-bit access needs REX | ✅ Extra helper |
| `isRsp(enc)` | Check for RSP special case | ✅ Extra helper |
| `isRbp(enc)` | Check for RBP special case | ✅ Extra helper |
| `needsSib(enc)` | Check if SIB byte needed | ✅ Extra helper |
| `needsDisp(enc)` | Check if displacement needed | ✅ Extra helper |
| `low3(enc)` | Extract low 3 bits for ModRM/SIB | ✅ Extra helper |

These helpers encapsulate special-case knowledge from the Intel manual:
- RSP (enc=4) requires SIB byte
- RBP (enc=5) requires displacement
- Registers 4-7 require REX for 8-bit low access (SPL, BPL, SIL, DIL)
- Registers 8-15 are "extended" and need REX.B/R/X

---

## Test Coverage

| Test | What it Verifies |
|------|------------------|
| `GPR creation` | All 16 GPRs have correct encoding |
| `XMM creation` | All 16 XMMs have correct encoding |
| `GPR encodings` | RAX=0, RCX=1, ..., R15=15 |
| `XMM encodings` | XMM0=0, XMM1=1, ..., XMM15=15 |
| `pinned register` | R15 is the pinned register |
| `pretty print GPR` | Names match Intel documentation |
| `pretty print XMM` | Names match Intel documentation |
| `extended register detection` | R8-R15 detected as extended |
| `8-bit REX requirement` | RSP/RBP/RSI/RDI need REX for 8-bit |
| `special register detection` | RSP/RBP special cases |
| `SIB and disp special cases` | RSP needs SIB, RBP needs disp |
| `low 3 bits extraction` | Correct for all 16 registers |

---

## Encoding Verification

The encoding values match Intel's Software Developer Manual Vol. 2, Table 2-2:

```
Register    Encoding    REX.B=0    REX.B=1
RAX/EAX/AX    000         %rax       %r8
RCX/ECX/CX    001         %rcx       %r9
RDX/EDX/DX    010         %rdx       %r10
RBX/EBX/BX    011         %rbx       %r11
RSP/ESP/SP    100         %rsp       %r12
RBP/EBP/BP    101         %rbp       %r13
RSI/ESI/SI    110         %rsi       %r14
RDI/EDI/DI    111         %rdi       %r15
```

✅ All encodings verified against Intel documentation.

---

## Parity Guarantee

This module was ported by:
1. Reading Cranelift regs.rs line-by-line
2. Creating equivalent Zig functions
3. Using identical encoding constants from gpr.rs
4. Implementing identical pretty-print logic
5. Adding helper functions that encapsulate Intel-documented special cases

The register definitions are **identical** to Cranelift.
