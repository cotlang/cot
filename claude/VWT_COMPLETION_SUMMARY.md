# VWT Phase 8: Address-Only Generics — Completion Summary

**Date:** 2026-03-25
**Result:** 370/370 features pass, 22/22 cases, selfcot builds, all green.

## What Was Built

Swift-style address-only generic convention for shared function bodies. Every generic function in Cot now compiles to ONE body shared across all instantiations, with type parameters passed as indirect pointers and sizes loaded from metadata at runtime.

### Architecture (maps 1:1 to Swift IRGen)

| Cot Implementation | Swift Reference | Purpose |
|---|---|---|
| T params as `__ptr_{name}` (I64 pointer) | `GenCall.cpp:747-764` ArgumentConvention::Indirect | Uniform param layout regardless of T size |
| Scalar shadow at entry | `GenProto.cpp:2936-2943` EmitPolymorphicParameters | Body can use T for arithmetic/comparison |
| `emitRuntimeSizeOf()` from metadata | `GenOpaque.cpp:1025` emitLoadOfSize | Runtime @sizeOf(T) in shared bodies |
| SRET return via memcpy | `GenCall.cpp:677-685` addIndirectResult | Return T with runtime-sized copy |
| memcpy for T stores | GenOpaque.cpp VWT assignWithCopy | `ptr.* = value` uses runtime size |
| `isGenericParamIndirect()` | `GenProto.cpp:4381-4401` expandPolymorphicSignature | Detect T-typed params (fn + struct level) |
| TypeMetadata globals | Swift TypeMetadata | [vwt_ptr, size, stride, kind] per type |

### Key Functions (all in `compiler/frontend/lower.zig`)

| Function | Line | Purpose |
|---|---|---|
| `emitRuntimeSizeOf(fb, tp_name, span)` | ~9935 | Load T size from `__metadata_{T}[1]` |
| `isGenericParamIndirect(inst_info, idx)` | ~9867 | Check fn + struct type params |
| `lowerGenericFnInstanceVWT()` | ~9645 | Shared body with T-indirect params |
| `emitLoadSizeFromMetadata()` | ~9894 | @sizeOf(T) → metadata runtime load |
| `emitTypeMetadataRef()` | ~9960 | Look up `__type_metadata_{Type}` global |
| `appendMetadataArgs()` | ~9890 | Forward metadata through call chains |
| `lowerForMap()` | ~4471 | Fixed: COW buffer layout for Map iteration |

### What Changed (4 commits)

1. **8c7276c** — Core Phase 8.5+8.7: T params indirect, call site wrapping, @sizeOf runtime, memcpy stores, @arcRetain noop. 12 tests fixed (141→153).

2. **36d3e98** — SRET returns: field access + deref + T-indirect param patterns. 2 more tests fixed (153→155).

3. **79dd489** — Map for-in: fixed COW buffer layout, empty map guard, renamed monomorphized→generic. 15+ more tests fixed (155→370). Also cleaned up naming.

## What's Deferred

| Item | When Needed | Complexity |
|---|---|---|
| VWT-based @arcRetain/@arcRelease dispatch | `List(List(i64))` nested collections | Medium — call VWT initializeWithCopy/destroy |
| Protocol witness dispatch for T ops (`a == b`) | `fn indexOf(value: T)` on collections | Medium — Phase 8.8 |
| Generic closures (`fn(T) -> i64`) | Higher-order generic functions | Low — same indirect convention |

## Remaining Path to 0.4

1. **selfcot2 codegen bugs** — arg parsing with 3+ CLI args, runtime crashes
2. **selfcot2 == selfcot3** — byte-identical Wasm output = full self-hosting
3. **Polish** — error messages, LSP, packaging
