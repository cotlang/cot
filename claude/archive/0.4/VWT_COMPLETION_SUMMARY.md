# VWT Phase 8: Address-Only Generics — COMPLETE

**Date:** 2026-03-25
**Result:** 370/370 features pass, 22/22 cases, selfcot builds, all green.
**Status:** All Zig compiler VWT work complete. Selfcot port pending.

## What Was Built

Swift-style address-only generic convention for shared function bodies. Every generic function in Cot now compiles to ONE body shared across all instantiations, with type parameters passed as indirect pointers and sizes loaded from metadata at runtime.

### Architecture (maps 1:1 to Swift IRGen)

| Cot Implementation | Swift Reference | Purpose |
|---|---|---|
| T params as `__ptr_{name}` (I64 pointer) | `GenCall.cpp:747-764` ArgumentConvention::Indirect | Uniform param layout regardless of T size |
| Scalar shadow at entry | `GenProto.cpp:2936-2943` EmitPolymorphicParameters | Body can use T for arithmetic/comparison |
| `emitRuntimeSizeOf()` from metadata | `GenOpaque.cpp:1025` emitLoadOfSize | Runtime @sizeOf(T) in shared bodies |
| SRET return via memcpy | `GenCall.cpp:677-685` addIndirectResult | Return T with runtime-sized copy |
| VWT initializeWithCopy for T stores | `GenOpaque.cpp` VWT dispatch | `ptr.* = value` → call_indirect through VWT |
| Runtime T equality dispatch | `GenProto.cpp` Equatable PWT | `==`/`!=` via memcmp or string_eq based on metadata type_idx |
| T-indirect ptr forwarding | Swift metadata flow | Inner generic calls forward `__ptr_` directly (no scalar truncation) |
| `isGenericParamIndirect()` | `GenProto.cpp:4381-4401` expandPolymorphicSignature | Detect T-typed params (fn + struct level) |
| TypeMetadata with type_idx | Swift Metadata.Kind | `[vwt_ptr, size, stride, type_idx]` per type |

### Commits (7 total)

1. **8c7276c** — Core Phase 8.5+8.7: T params indirect, call site wrapping, @sizeOf runtime, memcpy stores, @arcRetain noop (141→153)
2. **36d3e98** — SRET returns: field access + deref + T-indirect param patterns (153→155)
3. **79dd489** — Map for-in: COW buffer layout fix, empty map guard, monomorphized→generic rename (155→370)
4. **73303da** — Fix array element store through pointer parameter (*[N]T)
5. **82f6ba2** — Documentation audit: archive 8 completed docs
6. **fe10157** — VWT initializeWithCopy dispatch for T-indirect stores (call_indirect through VWT)
7. **1afed36** — Phase 8.8: T-typed equality dispatch (memcmp/string_eq) + metadata type_idx

### Key Functions (all in `compiler/frontend/lower.zig`)

| Function | Purpose |
|---|---|
| `emitRuntimeSizeOf(fb, tp_name, span)` | Load T size from `__metadata_{T}[1]` |
| `isGenericParamIndirect(inst_info, idx)` | Check fn + struct type params |
| `lowerGenericFnInstanceVWT()` | Shared body with T-indirect params |
| `emitLoadSizeFromMetadata()` | @sizeOf(T) → metadata runtime load |
| `emitTypeMetadataRef()` | Look up `__type_metadata_{Type}` global |
| `appendMetadataArgs()` | Forward metadata through call chains |
| `computeGenericBaseName()` | Strip type args: `List(5)_append` → `List_append` |

## Remaining: Selfcot VWT Port

Port the VWT changes from `compiler/frontend/lower.zig` to `self/build/lower.cot`.
Reference: `claude/VWT_SELFCOT_PORT_PLAN.md`

### What Must Change in selfcot

| selfcot (current) | Required change |
|---|---|
| One body per concrete name (`List(5)_append`) | One body per base name (`List_append`) |
| No metadata params | Append `__metadata_T` after user params |
| Direct calls with concrete name | Call base name with metadata args |
| No `has_indirect_result` | SRET for T-typed returns |
| No T-indirect params | `__ptr_{name}` + scalar shadow |
| No `@sizeOf(T)` runtime | Runtime load from metadata[1] |
| Shape stenciling code | DELETE — replaced by VWT |
| Monomorphization queue | Base-name dedup queue |
