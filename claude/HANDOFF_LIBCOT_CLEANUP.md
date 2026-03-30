# Handoff: Remove Native Codegen from libcot

**Date:** 2026-03-31
**Status:** COMPLETE
**Result:** 80K lines deleted. libcot 152K → 72K. Clean CIR boundary.

---

## What Was Done

1. **Direct SSA → CIR** via `codegen/ssa_to_cir.zig` (replaces SSA → CLIF IR → CIR two-step)
2. **Runtime functions as CIR** — arc, io, print, signal, test generators emit CIR directly
3. **Data sections in CIR** — string data, globals, metadata in CIR Section 0x04
4. **Entry wrapper as CIR** — `__cot_entry` function (stores argc/argv/envp, calls _cot_main)
5. **Deleted `codegen/native/`** (68K lines) and **`ir/clif/`** (6K lines) from libcot
6. **Deleted all old functions** — generateNativeCode, generateMachODirect, generateMachO, generateElf, all wrapper generators
7. **`--backend=zig`** now uses same CIR path, links `zig/libclif` instead of `rust/libclif`

## Pipeline

```
libcot: Cot source → parse → check → lower → SSA → CIR bytes
libclif: CIR bytes → Cranelift IR → native .o
linker: .o → executable
```

Both `rust/libclif` and `zig/libclif` implement the same `clif_compile` C ABI.
