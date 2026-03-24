# VWT Migration Plan: Monomorphization → Value Witness Tables

## Goal
Replace monomorphization + shape stenciling with Swift-style Value Witness Tables.
One generic function body per definition. ARC operations go through VWT indirect calls.
Specialization (optional) inlines hot paths later.

## Approach
Safe incremental: build VWT alongside monomorphization, switch over, then delete old code.
Every item below gets a checkbox. Nothing is "done" until every box is checked.

**Three audits completed:**
1. First audit: identified all monomorphization/shape stenciling code
2. Second audit: found 12 missed codebase items + 7 Swift VWT structural gaps
3. Third audit: complete 1:1 Swift VWT specification from apple/swift sources

---

## Swift VWT Reference (1:1 Port Target)

### Memory Layout (64-bit, 88 bytes base, 112 bytes with enum witnesses)

```
Offset  Field                              Type              Size
──────  ─────                              ────              ────
0x00    initializeBufferWithCopyOfBuffer   fn ptr            8
0x08    destroy                            fn ptr            8
0x10    initializeWithCopy                 fn ptr            8
0x18    assignWithCopy                     fn ptr            8
0x20    initializeWithTake                 fn ptr            8
0x28    assignWithTake                     fn ptr            8
0x30    getEnumTagSinglePayload            fn ptr            8
0x38    storeEnumTagSinglePayload          fn ptr            8
0x40    size                               StoredSize (u64)  8
0x48    stride                             StoredSize (u64)  8
0x50    flags                              u32               4
0x54    extraInhabitantCount               u32               4

--- Enum extension (only when HasEnumWitnesses flag set) ---
0x58    getEnumTag                         fn ptr            8
0x60    destructiveProjectEnumData         fn ptr            8
0x68    destructiveInjectEnumTag           fn ptr            8
```

### Function Signatures (all take *Metadata as final param)

```
OpaqueValue* initializeBufferWithCopyOfBuffer(ValueBuffer *dest, ValueBuffer *src, const Metadata *self)
void         destroy(OpaqueValue *object, const Metadata *self)
OpaqueValue* initializeWithCopy(OpaqueValue *dest, OpaqueValue *src, const Metadata *self)
OpaqueValue* assignWithCopy(OpaqueValue *dest, OpaqueValue *src, const Metadata *self)
OpaqueValue* initializeWithTake(OpaqueValue *dest, OpaqueValue *src, const Metadata *self)
OpaqueValue* assignWithTake(OpaqueValue *dest, OpaqueValue *src, const Metadata *self)
unsigned     getEnumTagSinglePayload(const OpaqueValue *value, unsigned emptyCases, const Metadata *self)
void         storeEnumTagSinglePayload(OpaqueValue *value, unsigned whichCase, unsigned emptyCases, const Metadata *self)

// Conditional enum witnesses:
unsigned     getEnumTag(const OpaqueValue *obj, const Metadata *self)
void         destructiveProjectEnumData(OpaqueValue *obj, const Metadata *self)
void         destructiveInjectEnumTag(OpaqueValue *obj, unsigned tag, const Metadata *self)
```

### Copy/Take Semantics (initialize vs assign × copy vs take)

| Witness | Dest before | Source after | ARC impl for managed ptr |
|---------|-------------|--------------|--------------------------|
| `initializeWithCopy` | uninitialized | valid | `retain(src); *dest = *src` |
| `assignWithCopy` | valid (destroy first) | valid | `old = *dest; retain(src); *dest = *src; release(old)` |
| `initializeWithTake` | uninitialized | invalid (moved) | `memcpy(dest, src, size)` |
| `assignWithTake` | valid (destroy first) | invalid (moved) | `old = *dest; *dest = *src; release(old)` |

### Flags (ValueWitnessFlags, u32)

```
AlignmentMask         = 0x000000FF  // bits 0-7:  alignment-1 (0-255)
                                    // getAlignment() = (flags & 0xFF) + 1
IsNonPOD              = 0x00010000  // bit 16: NOT plain-old-data (needs copy/destroy)
IsNonInline           = 0x00020000  // bit 17: does NOT fit in 3-word inline buffer
HasEnumWitnesses      = 0x00200000  // bit 21: VWT has enum witness extension
IsNonBitwiseTakable   = 0x00100000  // bit 20: NOT bitwise-takable (can't memcpy for take)
```

Flags Cot can skip (Swift/ObjC-specific):
- `HasSpareBits` (0x00080000) — unused even in Swift
- `Incomplete` (0x00400000) — runtime type resolution, not needed for static compilation
- `IsNonCopyable` (0x00800000) — ~Copyable/move-only types (future feature)
- `IsNonBitwiseBorrowable` (0x01000000) — Swift ownership system
- `IsAddressableForDependencies` (0x02000000) — Swift @_addressableForDependencies

### TypeMetadata (passed to all witness functions)

TypeMetadata is NOT the VWT. It's a larger struct that CONTAINS a pointer to the VWT:
```
[M - 8]  → pointer to ValueWitnessTable
[M + 0]  → Kind field (what type of type: struct, enum, class, tuple)
[M + 8]  → type-specific fields (descriptor, generic args, etc.)
```

Generic functions receive `*Metadata` (not `*VWT`). They access the VWT through `metadata[-1]`.

### Inline Buffer (3-word existential storage)

```
ValueBuffer = 3 pointers = 24 bytes on 64-bit
canBeInline = isBitwiseTakable && size <= 24 && alignment <= 8
```

If a value fits inline: stored directly in the buffer.
If out-of-line: heap-allocated box, buffer[0] = box pointer.

`initializeBufferWithCopyOfBuffer` handles both cases.

Note: This is for existential types (`any Protocol`). Cot can skip this if we don't have runtime existentials — but implementing it 1:1 means we CAN add existentials later without redesigning.

### VWT Generation Per Type Category

**POD struct** (e.g., `Point { x: i64, y: i64 }`):
- All witnesses = memcpy(size), destroy = noop
- flags: POD, inline (if ≤24B), bitwiseTakable

**Struct with ARC fields** (e.g., `Container { items: List(i64) }`):
- initializeWithCopy: retain each ARC field
- destroy: release each ARC field
- initializeWithTake: memcpy (bitwise-takable)
- assignWithTake: destroy old, memcpy new
- flags: NOT POD, bitwiseTakable

**Enum/Union** (e.g., `Result { ok: T, err: Error }`):
- destroy: tag-switch, destroy active payload
- initializeWithCopy: tag-switch, copy active payload, write tag
- flags: HasEnumWitnesses, NOT POD if any variant is non-POD
- Extra witnesses: getEnumTag, destructiveProjectEnumData, destructiveInjectEnumTag

**Optional** (single-payload enum):
- Uses extra inhabitants of payload type (e.g., null pointer = .none)
- getEnumTagSinglePayload / storeEnumTagSinglePayload for tag access

---

## Phase 1: Build VWT Infrastructure (new code only, nothing deleted)

### 1.1 TypeMetadata + VWT Struct Definition — DONE (commit 75c0d63)
- [x] Define `TypeMetadata` struct — kind + pointer to VWT + type_idx
- [x] Define `ValueWitnessTable` struct matching Swift layout (88 bytes base):
  - [x] 8 required function pointer fields (in Swift's exact order)
  - [x] `size: u64` (StoredSize, platform-native)
  - [x] `stride: u64`
  - [x] `flags: u32` (ValueWitnessFlags with alignment packed in bits 0-7)
  - [x] `extraInhabitantCount: u32`
- [x] Define `EnumValueWitnessTable` extending base with 3 enum witnesses
- [x] Define `ValueWitnessFlags` with all flag constants (AlignmentMask, IsNonPOD, IsNonInline, HasEnumWitnesses, IsNonBitwiseTakable)
- [x] Tests: VWT=88B, EnumVWT=112B, flags packing verified
- [x] TypeMetadata + VWT globals emitted per concrete type (`__type_metadata_{name}`, `__vwt_table_{name}`)

### 1.2 VWT Generation Per Concrete Type
**VWTEntry computation: DONE (commit 6ac33d5)**
- [x] `VWTGenerator.computeEntry()` — size, stride, flags, extraInhabitants, witness fn names
- [x] `isBitwiseTakable()` — all Cot types currently are
- [x] `computeExtraInhabitants()` — pointers/collections=1, basic=0, structs=min of fields
- [x] Tests: POD i64, raw pointer, optional

**Witness function bodies: COMPLETE for ALL type categories**
**Refactored to TypeCategory enum: pod, managed_ptr, collection, struct_with_arc, union_with_arc, optional_with_arc, error_union_with_arc, array_with_arc, tuple_with_arc**
**Also: getEnumTagSinglePayload, storeEnumTagSinglePayload, initializeBufferWithCopyOfBuffer — all Swift VWT slots implemented**
- [x] **POD types** (int, float, bool, raw pointers): all copy=memcpy, destroy=noop, flags=POD
- [x] **Managed pointers** (*T from new): copy=memcpy+retain, destroy=release, take=memcpy
- [x] **Collections** (List, Map — both .list/.map AND monomorphized .struct_type):
  - [x] copy=memcpy+retain(buf) if non-null, destroy=release(buf) if non-null, take=memcpy
- [x] **assignWithCopy**: destroy(dest) then initializeWithCopy — correct naive impl
- [x] **assignWithTake**: destroy(dest) then memcpy — take=move, no retain
- [x] Tests: 5 functions emitted per type, dedup works, collection has branch blocks
- [x] **Structs with ARC fields**: field-by-field using sub-VWTs (commit TBD)
  - [x] initializeWithCopy: memcpy whole struct + call field VWT.initializeWithCopy for each non-trivial field
  - [x] assignWithCopy: calls destroy(dest) then initializeWithCopy (via assignWithCopy witness, already works)
  - [x] destroy: for each non-trivial field (reverse order), call field's VWT.destroy
  - [x] initializeWithTake: memcpy (bitwise-takable structs — already worked)
  - [x] assignWithTake: destroy old, memcpy new (already worked)
  - [x] Recursive sub-type witness emission: struct with ARC fields triggers emitWitnesses for field types first
  - [x] Tests: POD struct (5 fns), struct with List field (10 fns), struct with managed ptr field (10 fns)
- [x] **Unions/Enums**: tag-conditional dispatch (commit TBD)
  - [x] getEnumTag: load tag from offset 0 of union
  - [x] destructiveProjectEnumData: return pointer to payload at offset 8
  - [x] destructiveInjectEnumTag: store tag at offset 0
  - [x] destroy: load tag → if-else chain → call variant payload's destroy witness
  - [x] initializeWithCopy: memcpy whole union → load tag → if-else chain → call variant payload's initializeWithCopy
  - [x] Recursive sub-type witness emission for non-trivial variant payloads
  - [x] Tests: POD union (8 fns), union with managed ptr variant (13 fns)
- [x] **Optionals**: single-payload enum path (commit TBD)
  - [x] destroy: if tag == 0 (some), call payload's destroy
  - [x] initializeWithCopy: memcpy + if tag == 0, call payload's initializeWithCopy
  - [x] getEnumTag/destructiveProjectEnumData/destructiveInjectEnumTag: same as union witnesses
  - [x] getEnumTagSinglePayload: reads tag from offset 0 (Cot uses explicit tags, no extra inhabitant optimization yet)
  - [x] storeEnumTagSinglePayload: writes whichCase to tag at offset 0
- [x] **Arrays**: element-loop via element VWT — destroy in reverse order, initializeWithCopy per element
- [x] **Tuples**: element-by-element via component VWTs — destroy in reverse, copy per non-trivial element
- [x] **Strings/Slices**: POD — handled by `.pod` category (ptr+len are raw values, no ARC)
- [x] **Error unions**: tag + payload — same pattern as optionals (single-payload with tag at offset 0)
- [x] **initializeBufferWithCopyOfBuffer**: inline=memcpy(24B), out-of-line=memcpy(8B ptr). Full box alloc deferred to existentials.

### 1.3 Witness Functions as Callable Code — DONE (uncommitted)
- [x] Each witness is a real IR function (emitted via FuncBuilder, flows through SSA → codegen)
- [x] Naming convention: `__vwt_destroy_Point`, `__vwt_initializeWithCopy_Container`, etc.
- [x] All witness functions take `*Metadata` as final parameter
- [x] Driver integration: `emitVWTWitnesses()` called after lowering, before getIR()
- [x] Both single-file and multi-file compilation paths wired up
- [x] Native + Wasm: witnesses compile through pipeline without errors (tested)
- [x] Dedup: VWTGenerator tracks emitted types, no duplicates across files

### 1.4 Generic Function Codegen with VWT — DONE
- [x] Generic functions receive `*Metadata` parameter(s) — one per type param (`lowerGenericFnInstanceVWT`)
- [x] `@arcRetain(value)` → call `vwt.initializeWithCopy` (via `use_vwt` dispatch in `emitCopyValue`)
- [x] `@arcRelease(value)` → call `vwt.destroy` (via `use_vwt` dispatch in `emitDestroyValue`)
- [x] Scope-exit cleanup → call `vwt.destroy` (cleanup stack calls `emitDestroyValue` which dispatches)
- [x] `new` field stores → goes through `emitCopyValue` which dispatches to VWT
- [x] Return (SRET) → goes through `emitCopyValue` which dispatches to VWT
- [x] Assignment to existing value → `emitCopyValue` dispatches to VWT `initializeWithCopy`
- [x] Move/forward → `initializeWithTake` = memcpy (all Cot types bitwise-takable, no separate dispatch needed)
- [x] Struct literal field init → VWT copy for non-trivial +0 fields (goes through emitCopyValue)
- [x] Access VWT via metadata pointer — metadata globals contain VWT ptr at offset 0, loadable at runtime
- [x] `@sizeOf(T)` — resolves at compile time via type substitution (runtime load deferred to existentials)

### 1.5 Call Sites — DONE
- [x] Calling generic fn with concrete T: `emitVWTWrapper` passes real `__type_metadata_{type}` global address
- [x] Multiple type params: metadata param added per type_param_name, each gets its own metadata global
- [x] Method calls on generic types: wrapper calls base function with metadata prepended
- [x] Nested generics: inner calls go through their own wrappers which inject appropriate metadata
- [x] TypeMetadata globals: wrappers pass real `__type_metadata_{type}` global addresses

### 1.6 Native Codegen — DONE (via standard pipeline)
VWT witnesses and metadata init functions are regular IR functions compiled through
the standard SSA → CLIF IR → native pipeline. No special native codegen needed.
- [x] VWT emitted as global data section (`__vwt_table_{type}`, 88 bytes)
- [x] TypeMetadata emitted as global data section (`__type_metadata_{type}`, 24 bytes)
- [x] Witness function pointers stored via `emitFuncAddr` — resolved at link time
- [x] Direct witness calls by name (indirect via VWT table deferred to existentials)

### 1.7 Wasm Codegen — DONE (via standard pipeline)
Same as native: witnesses are regular Wasm module functions.
- [x] Witness functions compiled as Wasm module functions with indices
- [x] Indirect dispatch via VWT table deferred to existentials (not needed for current generics)

---

## Phase 2: Switch Generic Codegen to VWT

### 2.1 Checker Changes — DONE (no changes needed)
The checker's monomorphization is kept for type checking (field types, method signatures,
expr_types preservation). VWT integration happens at lowering time — the lowerer reads
GenericInstInfo and computes base names + emits wrappers. The checker's concrete names
("List(5)_append") are used as wrapper names; the base names ("List_append") are computed
by the lowerer. Deletion of checker monomorphization is Phase 3 (after VWT is proven).
- [x] `resolveGenericInstance` still creates concrete types (needed for type checking)
- [x] Generic types carry metadata via `__type_metadata_{typeName}` globals (created at VWT emission)
- [x] Method resolution: checker uses concrete names, lowerer computes base names
- [x] `expr_types` preservation works (unchanged)
- [x] Trait bound checking unchanged (orthogonal to VWT)

### 2.2 Lowerer Changes — DONE (2026-03-25)
- [x] Generic function lowering emits ONE body with `*Metadata` params (`lowerGenericFnInstanceVWT`)
- [x] All ARC paths use VWT witnesses instead of inline code (`emitCopyValue`/`emitDestroyValue` call VWT witnesses directly)
- [x] `emitCopyValue` calls `__vwt_initializeWithCopy_{type}` for ALL non-trivial types
- [x] `emitDestroyValue` calls `__vwt_destroy_{type}` for ALL non-trivial types
- [x] Old inline ARC code deleted (450+ lines removed from emitCopyValue/emitDestroyValue)
- [x] `emitOptionalFieldRetain`, `emitVWTWrapper`, `isGenericReturnType` deleted (dead code)
- [x] `lowerMethodCall` on generic types: call sites use `computeGenericBaseName` → base name + metadata
- [x] Generic function queue: dispatches to `lowerGenericFnInstanceVWT` which emits base body

### 2.3 Verification — DONE (2026-03-25)
- [x] All 22 test/cases files pass (native and wasm)
- [x] selfcot builds in ~10.9s (482 VWT types, 135 unique witnesses, 2,558 total funcs)
- [x] `/tmp/selfcot version` → `cot 0.3.7 (self-hosted)`
- [x] VWT witness emission gated on native target (wasm has no ARC, skips VWT)
- [ ] selfcot2 builds (depends on selfcot — separate milestone)
- [ ] Binary size comparison: VWT binary vs monomorphized binary

---

## Phase 3: Delete Old Monomorphization Code

Every item must be checked off AND verified by grep.

### 3.1 types.zig — Delete Shape Infrastructure
- [ ] `Shape` struct — shape definition
- [ ] `Shape.ArcKind` enum — ARC categorization
- [ ] `Shape.RegClass` enum — register class
- [ ] `Shape.fromType()` — compute shape from type
- [ ] `Shape.eql()` — shape equality
- [ ] `Shape.key()` — shape suffix string
- [ ] `ShapeKey` struct — shape string buffer
- [ ] `isMonomorphizedCollection()` — name-based hack (still used by VWT classifyType — must replace first)
- [ ] Remove `isMonomorphizedCollection` call from `isTrivial()`
- [ ] Remove `isMonomorphizedCollection` call from `couldBeARC()`

### 3.2 checker.zig — KEEP for now
GenericInstInfo, instantiation_cache, generic_inst_by_name, resolveGenericInstance,
instantiateGenericImplMethods, etc. are all STILL USED by the VWT lowering path.
The VWT lowerer reads GenericInstInfo to get concrete_name, type_args, expr_types.
These cannot be deleted until the checker is refactored to not monomorphize at all.
**Defer to Phase 5 (post-VWT stabilization).**

### 3.3 lower.zig — DONE (778 lines deleted in commit c27e9b0)
- [x] `shape_stencils`, `dict_arg_names`, `generated_dict_helpers`, `current_dict_entries`, `current_dict_params` fields
- [x] `DictEntry` struct
- [x] `lowerGenericFnInstance()` (old non-VWT monomorphization)
- [x] `scanMethodCallDictEntries`, `scanNodeForMethodCalls`, `scanExprForMethodCalls`, `scanStmtForMethodCalls`, `receiverTypeParamIdx`
- [x] `buildDictArgNames`, `dictHelperName`, `generateDictHelpers`, `generateMethodCallHelper`, `generateDictWrapper`
- [x] Dict dispatch in `lowerMethodCall()`
- [x] `use_vwt` flag (VWT is now unconditional)
- [x] `isMonomorphizedCollection` calls in `emitCopyValue`/`emitDestroyValue`/`@arcRetain`/`@arcRelease` (replaced by VWT dispatch)
- [x] Verified: `grep -c` returns 0 for all deleted patterns

### 3.4 driver.zig — DONE (deleted in commit c27e9b0)
- [x] `shared_shape_stencils`, `shared_dict_arg_names`, `shared_generated_dict_helpers`
- [x] All code passing shape_stencils/dict_arg_names to lowerer instances
- [x] `use_vwt` assignment (VWT is now unconditional)
- [x] Verified: `grep -c` returns 0 for all deleted patterns

### 3.5 ast.zig — KEEP
- [x] `GenericInstance` struct — still used for parsing `Type(Args)` (KEEP)
- [x] `TypeKind.generic_instance` — still needed for type expression parsing (KEEP)

### 3.6 SSA Pass — KEEP
- [x] `rewritegeneric.zig` — misleading name, actually rewrites const_string→string_make (KEEP)

### 3.7 self/ (Self-Hosted Compiler) — STENCILING DELETED (2026-03-25, ~620 lines)
Dict/stenciling infrastructure removed. Core generic instantiation (ensureGenericFnQueued,
lowerGenericFnInstanceInner, lowerQueuedGenericFunctions) kept — selfcot still uses
monomorphization for generics (VWT not yet ported to self/).
- [x] `DictEntry` struct, `DICT_BINARY_OP`, `DICT_METHOD_CALL`, `STENCIL_*` constants
- [x] `dictOpName()`, `dictHelperName()`, `generateBinaryOpHelper()`, `generateMethodCallHelper()`
- [x] `analyzeStencilability()`, `collectNodeDictEntries()`, `collectExprDictEntries()`, `collectStmtDictEntries()`
- [x] `generateDictHelpers()`, `buildDictArgNames()`
- [x] `current_dict_entries`, `current_dict_params`, `has_dict_context` fields
- [x] `shape_stencils`, `shape_aliases`, `generated_dict_helpers`, `shape_analysis_cache`, `stencil_dict_entries`, `dict_arg_names` fields
- [x] Dict dispatch in lowerBinary and lowerMethodCall
- [x] All stenciling fields in `SharedLowerState` (main.cot) and `Builder` (builder.cot)
- [x] Test blocks for DictEntry and dictOpName
- [x] Verified: `grep -c` returns 0 for all stenciling patterns in self/
- [ ] `lowered_generics` — still used by generic instantiation (not stenciling)
- [ ] `ensureGenericFnQueued()`, `lowerGenericFnInstanceInner()` — still used for monomorphization

---

## Phase 4: Specialization (Optional, Post-VWT)

- [ ] When the optimizer can see the concrete type at a call site, inline the VWT calls
- [ ] Replace `vwt.initializeWithCopy` with direct `retain(buf)` for known List types
- [ ] Replace `vwt.destroy` with direct `release(ptr)` for known pointer types
- [ ] Replace `vwt.initializeWithTake` with direct `memcpy` for known bitwise-takable types
- [ ] POD types: eliminate all witness calls, replace with inline memcpy/noop
- [ ] Guided by profiling — only specialize hot paths
- [ ] `-O` enables specialization, `-Osize` limits it (Swift convention)

---

## Verification Checklist (run after each phase)

- [ ] `zig build test` — compiler unit tests
- [ ] All 22 test/cases/*.cot pass
- [ ] `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` — selfcot builds
- [ ] `/tmp/selfcot version` — selfcot runs

**After Phase 3 — zero-hit grep verification:**
```bash
# Must ALL return zero results:
grep -rn "shape_stencil\|ShapeKey\|Shape\b" compiler/frontend/types.zig
grep -rn "DictEntry\|dict_arg_names\|dictHelperName\|generateDictWrapper\|generateDictHelpers" compiler/frontend/lower.zig
grep -rn "isMonomorphizedCollection" compiler/
grep -rn "instantiation_cache\|generic_inst_by_name\|GenericInstInfo\|GenericImplInfo" compiler/
grep -rn "lowerGenericFnInstance\|lowerQueuedGeneric\|ensureGenericFnQueued" compiler/
grep -rn "shape_stencils\|lowered_generics" compiler/frontend/lower.zig compiler/driver.zig
grep -rn "dict_dispatch\|dict_context\|current_dict\|has_dict_context\|shape_alias\|stencil_dict" self/
grep -rn "lowered_generics\|shape_analysis_cache\|analyzeStencilability\|lowerGenericFnInstanceInner" self/
```

---

## Risk Notes

1. **Generic type resolution must still work** — VWT changes HOW generic code runs, not WHETHER types resolve. The parser still parses `List(T)`, the checker still resolves type args. Only codegen changes.

2. **Method dispatch** — Currently "List(5)_append" is a concrete function name. With VWT, "List_append" is one function that takes `*Metadata`. Method lookup changes from name-based to (base_name + metadata).

3. **stdlib List/Map** — These are the primary generic types. They must work through VWT first. All other generics follow the same pattern.

4. **Cross-file generics** — Currently handled by `SharedGenericContext` passing instantiation info across files. VWT simplifies this: the TypeMetadata for type T is generated once and referenced by name.

5. **The self-hosted compiler** — `self/` uses generics (List, Map) extensively but has NO generic infrastructure ported. VWT needs to work in the Zig compiler first, then be ported to self/.

6. **Inline buffer** — Swift's 3-word inline buffer exists for existential types. Cot doesn't have existentials yet, but implementing the buffer system 1:1 means we CAN add them later without redesigning the VWT.

7. **extraInhabitantCount** — Required for zero-overhead Optional. Without it, `?*T` costs an extra tag byte instead of using the null pointer as the `.none` representation.

8. **TypeMetadata vs VWT** — Generic functions receive `*Metadata` (not `*VWT` directly). This is because metadata contains type-specific info beyond the VWT (kind, descriptor, generic args). The VWT is always at `metadata[-1]`.
