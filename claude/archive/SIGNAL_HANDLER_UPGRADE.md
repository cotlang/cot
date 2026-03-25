# Signal Handler Upgrade: Port Go's Crash Diagnostics

**Date:** 2026-03-18
**Status:** Implementation plan
**Goal:** When a Cot binary crashes, the output should identify the cause without manual disassembly.

---

## Current Output (Cot)

```
fatal error: signal 11
0   selfcot  0x100146da8 __stencil_Map(17;5)_getOrNull + 92
1   selfcot  0x100140108 _SSABuilder_convertNode + 1180
...
```

No faulting address, no registers, no signal-specific message.

## Target Output (Go-style)

```
SIGSEGV: segmentation fault
PC=0x100146da8 sigcode=1 addr=0x0000000000000020
goroutine 1 [running]:
  0   selfcot  0x100146da8 __stencil_Map(17;5)_getOrNull + 92
  1   selfcot  0x100140108 _SSABuilder_convertNode + 1180
  ...

x0    0x0000000000000000
x1    0x000000010050a800
x2    0x0000000000000020
...
x28   0x0000000000000000
fp    0x000000016fdfe9a0
lr    0x0000000100140108
sp    0x000000016fdfe960
pc    0x0000000100146da8
fault 0x0000000000000020
```

For SIGILL, Go also prints instruction bytes at the faulting PC.

---

## Go's Implementation (Reference)

### Key Design: SA_SIGINFO 3-Argument Handler

Go uses `SA_SIGINFO` flag on `sigaction` to get a 3-argument handler:
```c
void handler(int sig, siginfo_t *info, void *ucontext)
```

This provides:
- `info->si_addr` — faulting memory address
- `info->si_code` — signal subtype (SEGV_MAPERR, ILL_ILLOPC, etc.)
- `ucontext->uc_mcontext->ss` — all register values at crash time

### macOS ARM64 Struct Layout (defs_darwin_arm64.go:136-200)

```
siginfo_t (128 bytes):
  [0]   si_signo  i32
  [4]   si_errno  i32
  [8]   si_code   i32
  [12]  si_pid    i32
  [16]  si_uid    u32
  [20]  si_status i32
  [24]  si_addr   *byte (8 bytes)
  [32]  si_value  [8]byte
  [40]  si_band   i64
  [48]  __pad     [56]byte (7 × u64)

ucontext_t:
  [0]   uc_onstack   i32
  [4]   uc_sigmask   u32
  [8]   uc_stack     stack_t (24 bytes: sp(8) + size(8) + flags(4) + pad(4))
  [32]  uc_link      *ucontext (8 bytes)
  [40]  uc_mcsize    u64
  [48]  uc_mcontext  *mcontext64 (8 bytes — POINTER, not inline)

mcontext64:
  [0]   es  exceptionstate64 (16 bytes: far(8) + esr(4) + exc(4))
  [16]  ss  regs64

regs64 (272 bytes):
  [0]    x[0..28]  29 × u64 = 232 bytes
  [232]  fp        u64 (x29)
  [240]  lr        u64 (x30)
  [248]  sp        u64 (x31)
  [256]  pc        u64
  [264]  cpsr      u32
  [268]  __pad     u32
```

### Go's fatalsignal Output (signal_unix.go:845-886)

1. Print signal name: `"SIGSEGV\n"`
2. Print PC + sigcode + optional addr: `"PC=0x... m=0 sigcode=1 addr=0x..."`
3. For SIGILL/SIGFPE: print 16 instruction bytes at PC
4. Print backtrace
5. Print register dump (dumpregs)

### Go's Register Dump (signal_arm64.go:16-51)

Prints all 29 GP registers (x0-x28), fp, lr, sp, pc, and fault address.

---

## Implementation Plan

### Step 1: Switch to SA_SIGINFO 3-Argument Handler

**Current:** `sa_handler(int sig)` — only gets signal number.
**New:** `sa_sigaction(int sig, siginfo_t *info, void *ucontext)` — gets everything.

Changes in `signal_native.zig`:
- `generateSignalHandler`: Change signature to `(sig: i64, info: i64, ucontext: i64) -> void`
- `generateInstallSignals`: Add `SA_SIGINFO` (0x40 on macOS) to `sa_flags`
- Use `__sigaction_u` field (8 bytes at offset 0) for the handler pointer
- macOS sigaction struct: `{ __sigaction_u[8], sa_tramp[8], sa_mask[4], sa_flags[4] }` = 24 bytes (not 16!)

### Step 2: Extract Crash Info from siginfo_t

In the signal handler, read from the `info` pointer:
- `si_code = load_i32(info + 8)` — signal subtype
- `si_addr = load_i64(info + 24)` — faulting address

### Step 3: Extract Registers from ucontext_t

Read the mcontext pointer, then read registers:
```
mcontext_ptr = load_i64(ucontext + 48)
regs_base = mcontext_ptr + 16  // skip exceptionstate64
x[i] = load_i64(regs_base + i*8)     // x0-x28
fp   = load_i64(regs_base + 232)     // x29
lr   = load_i64(regs_base + 240)     // x30
sp   = load_i64(regs_base + 248)     // x31
pc   = load_i64(regs_base + 256)
```

### Step 4: Print Signal-Specific Message

Map signal numbers to names:
- 4 → "SIGILL: illegal instruction"
- 6 → "SIGABRT: abort"
- 8 → "SIGFPE: floating point exception"
- 10 → "SIGBUS: bus error"
- 11 → "SIGSEGV: segmentation fault"

Print PC and fault address:
```
SIGSEGV: segmentation fault
PC=0x100146da8 sigcode=1 addr=0x0000000000000020
```

For SIGILL: also print instruction bytes at PC (Go fatalsignal lines 865-883).

### Step 5: Print Register Dump

Print key registers that help diagnose crashes:
```
x0    0x0000000000000000
x1    0x000000010050a800
...
fp    0x000000016fdfe9a0
lr    0x0000000100140108
sp    0x000000016fdfe960
pc    0x0000000100146da8
fault 0x0000000000000020
```

### Step 6: Print Backtrace (existing)

Keep the existing `__cot_print_backtrace()` using libc `backtrace()` + `backtrace_symbols_fd()`.

---

## Hex Printing Helper

All values need to be printed as hex. The signal handler can't use malloc (we might be crashing due to heap corruption). Need a stack-based hex printer.

Generate a `__cot_print_hex(value: i64)` helper that:
1. Allocates a 16-byte stack buffer
2. Converts value to hex digits (0-9, a-f) right-to-left
3. Writes "0x" prefix + digits to stderr

Go reference: `hex()` function in `runtime/print.go`.

---

## Platform Considerations

### macOS vs Linux

| Aspect | macOS ARM64 | Linux ARM64 |
|--------|-------------|-------------|
| SA_SIGINFO | 0x40 | 0x4 |
| SA_ONSTACK | 0x1 | 0x08000000 |
| sigaction struct | 24 bytes (has sa_tramp) | 32 bytes (has sa_restorer) |
| si_addr offset | 24 | 16 (via sigfault union) |
| uc_mcontext | pointer at offset 48 | inline at offset 176 |

For V1, implement macOS ARM64 only (primary dev platform). Add Linux support when needed.

---

## File Changes

| File | Changes |
|------|---------|
| `compiler/codegen/native/signal_native.zig` | Rewrite signal handler with SA_SIGINFO, register dump, hex printing |
| `compiler/driver.zig` | Register `__cot_print_hex` as runtime function |

No changes to selfcot or stdlib needed.
