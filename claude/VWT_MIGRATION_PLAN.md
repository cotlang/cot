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
- [ ] Add metadata/VWT index to TypeInfo (each concrete type gets metadata + VWT) — deferred to Phase 2

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

### 1.4 Generic Function Codegen with VWT
- [ ] Generic functions receive `*Metadata` parameter(s) — one per type param
- [ ] Access VWT via `metadata[-1]` (pointer at offset -8 from metadata)
- [ ] `@sizeOf(T)` → load `vwt.size`
- [ ] `@arcRetain(value)` → call `vwt.initializeWithCopy(dest, src, metadata)`
- [ ] `@arcRelease(value)` → call `vwt.destroy(value, metadata)`
- [ ] Scope-exit cleanup → call `vwt.destroy(local, metadata)`
- [ ] `new` field stores → call `vwt.initializeWithCopy` for each non-trivial field
- [ ] Return (SRET) → call `vwt.initializeWithCopy` for ownership transfer
- [ ] Assignment to existing value → call `vwt.assignWithCopy`
- [ ] Move/forward → call `vwt.initializeWithTake` (no retain, source invalidated)
- [ ] Struct literal field init → `vwt.initializeWithCopy` for non-trivial +0 fields

### 1.5 Call Sites
- [ ] Calling generic fn with concrete T: pass T's `*Metadata` as extra arg after explicit args
- [ ] Multiple type params (e.g., `Map(K, V)`): pass K's metadata AND V's metadata
- [ ] Nested generics: forward metadata parameter to inner generic calls
- [ ] Method calls on generic types: metadata comes from the type's instantiation context

### 1.6 Native Codegen
- [ ] Emit VWT as global constant data section
- [ ] Emit TypeMetadata as global constant (VWT pointer + kind + descriptor)
- [ ] Witness function pointers resolved at link time
- [ ] CLIF IR: indirect calls `call_indirect vwt.destroy, [value, metadata]`

### 1.7 Wasm Codegen
- [ ] Wasm VWT: all function pointers = Wasm function indices
- [ ] Trivial VWT: destroy=noop function, copy=memcpy function
- [ ] Non-trivial VWT: concrete witness functions in the Wasm module
- [ ] Wasm table for indirect calls through VWT function pointers

---

## Phase 2: Switch Generic Codegen to VWT

### 2.1 Checker Changes
- [ ] `resolveGenericInstance` creates a VWT-backed type (not a monomorphized .struct_type)
- [ ] Generic types carry their metadata index for VWT lookup
- [ ] Method resolution uses base generic name + type args (not "List(5)_append")
- [ ] `expr_types` preservation still works (VWT doesn't change type checking)
- [ ] Trait bound checking remains (orthogonal to VWT)

### 2.2 Lowerer Changes
- [ ] Generic function lowering emits ONE body with `*Metadata` params
- [ ] `lowerMethodCall` on generic types: emit indirect call via VWT, not concrete name
- [ ] All ARC paths use VWT witnesses instead of inline `emitCopyValue`/`emitDestroyValue`
- [ ] `@arcRetain`/`@arcRelease` builtins dispatch through VWT
- [ ] Struct init, new expr, SRET return all use VWT witnesses
- [ ] Remove generic function queue (one body per definition, no queue needed)

### 2.3 Verification
- [ ] All 22 test/cases files pass
- [ ] selfcot builds
- [ ] selfcot2 builds (or advances past current blocker)
- [ ] Binary size comparison: VWT binary vs monomorphized binary

---

## Phase 3: Delete Old Monomorphization Code

Every item must be checked off AND verified by grep.

### 3.1 types.zig — Delete Shape Infrastructure
- [ ] `Shape` struct (line 816) — shape definition
- [ ] `Shape.ArcKind` enum (line 826) — ARC categorization
- [ ] `Shape.RegClass` enum (line 836) — register class
- [ ] `Shape.fromType()` (line 843) — compute shape from type
- [ ] `Shape.eql()` (line 864) — shape equality
- [ ] `Shape.key()` (line 870) — shape suffix string
- [ ] `ShapeKey` struct (line 887) — shape string buffer
- [ ] `isMonomorphizedCollection()` (line 500) — name-based hack
- [ ] Remove `isMonomorphizedCollection` call from `isTrivial()` (line 536)
- [ ] Remove `isMonomorphizedCollection` call from `couldBeARC()` (line 585)

### 3.2 checker.zig — Delete Monomorphization
- [ ] `GenericInstInfo` struct (line 112) — all fields including:
  - [ ] `concrete_name: []const u8`
  - [ ] `generic_node: NodeIndex`
  - [ ] `type_args: []const TypeIndex`
  - [ ] `type_param_names: []const []const u8`
  - [ ] `tree: *const Ast`
  - [ ] `scope: ?*Scope`
  - [ ] `expr_types: ?std.AutoHashMap(NodeIndex, TypeIndex)` — **NOTE: may need VWT equivalent**
- [ ] `GenericImplInfo` struct (line 170) — generic impl block metadata
- [ ] `SharedGenericContext.instantiation_cache` (line 138) — dedup cache
- [ ] `SharedGenericContext.generic_inst_by_name` (line 139) — name→inst map for lowerer
- [ ] `SharedGenericContext.generic_impl_blocks` (line 140) — impl blocks per generic
- [ ] `resolveGenericInstance()` (line 3820) — instantiate generic struct
- [ ] `instantiateGenericImplMethods()` (line 3896) — monomorphize impl methods
- [ ] `instantiateGenericFunc()` (line 4030) — instantiate generic function
- [ ] `buildGenericCacheKey()` (line 4144) — dedup key builder
- [ ] `parseMapTypeArgs()` (line 179) — parse "Map(K;V)" format
- [ ] `type_substitution` field and ALL uses — substitution map during instantiation

**KEEP (still needed for VWT):**
- [ ] `GenericInfo` struct (line 103) — generic definition metadata (type_params, bounds)
- [ ] `SharedGenericContext.generic_structs` (line 136) — generic struct definitions
- [ ] `SharedGenericContext.generic_functions` (line 137) — generic function definitions
- [ ] `SharedGenericContext.trait_defs` (line 141) — trait definitions
- [ ] `SharedGenericContext.trait_impls` (line 142) — trait impl keys
- [ ] `buildStructType()` (line 4207) — still needed for non-generic structs

### 3.3 lower.zig — Delete Shape Stenciling & Dict Dispatch
- [ ] `shape_stencils` field (line 57) — stencil cache
- [ ] `dict_arg_names` field (line 60) — dict helper names
- [ ] `current_dict_entries` field (line 64) — runtime dict entries
- [ ] `current_dict_params` field (line 65) — runtime dict param locals
- [ ] `lowered_generics` field (line 52) — **MISSED IN AUDIT 1** — dedup map for generic functions
- [ ] `DictEntry` struct (line 491) — method call descriptor
- [ ] `lowerGenericFnInstance()` (~line 9546) — **CORRECTED NAME** — Phase 3 generic lowering
- [ ] `lowerQueuedGenericFunctions()` (line 9509) — **MISSED IN AUDIT 1** — process queued generics
- [ ] `ensureGenericFnQueued()` (line 9501) — generic function queue management
- [ ] `scanMethodCallDictEntries()` (line 9290) — scan AST for method calls
- [ ] `scanNodeForMethodCalls()` (line 9319) — recursive AST walk
- [ ] `scanExprForMethodCalls()` (line 9328) — expression walker
- [ ] `scanStmtForMethodCalls()` (line 9421) — statement walker
- [ ] `receiverTypeParamIdx()` (line 9457) — check if receiver is type param
- [ ] `buildDictArgNames()` (line 9840) — create dict helper names
- [ ] `dictHelperName()` (line 9853) — generate helper name
- [ ] `generateDictHelpers()` (line 9864) — generate trampolines
- [ ] `generateMethodCallHelper()` (line 9876) — generate trampoline function
- [ ] `generateDictWrapper()` (line 9764) — wrapper around stencil
- [ ] Dict dispatch in `lowerMethodCall()` (lines 10213-10273) — indirect call logic
- [ ] All `generic_fn_queue` / `pending_generic_fns` references
- [ ] `isMonomorphizedCollection` calls in `emitCopyValue` (line 4671)
- [ ] `isMonomorphizedCollection` calls in `emitDestroyValue` (line 4908)
- [ ] `isMonomorphizedCollection` calls in `@arcRetain` handler (line 11232)
- [ ] `isMonomorphizedCollection` calls in `@arcRelease` handler (line 11250)

### 3.4 driver.zig — Delete Shared Contexts
- [ ] `shared_shape_stencils` (line 542) — shared stencil cache
- [ ] `lowered_generics` sharing code — **MISSED IN AUDIT 1** — dedup across files
- [ ] All code passing shape_stencils to lowerer instances (lines 581, 593, 603, 612, 625)
- [ ] All code passing lowered_generics to lowerer instances

### 3.5 ast.zig — Evaluate
- [ ] `GenericInstance` struct (line 382) — **KEEP** — still used for parsing `Type(Args)`
- [ ] `TypeKind.generic_instance` (line 394) — **KEEP** — still needed for type expression parsing

### 3.6 SSA Pass
- [ ] `rewritegeneric.zig` — **KEEP** — misleading name, actually rewrites const_string→string_make

### 3.7 self/ (Self-Hosted Compiler) — Delete ALL Stenciling/Dict Infrastructure
- [ ] `DictEntry` struct in `self/build/lower.cot:71-76`
- [ ] `current_dict_entries` field in `self/build/lower.cot:158`
- [ ] `current_dict_params` field in `self/build/lower.cot:159`
- [ ] `has_dict_context` field and all uses
- [ ] `shape_aliases` in `self/build/lower.cot:153` — **MISSED IN AUDIT 1**
- [ ] `shape_analysis_cache` in `self/build/lower.cot:155` — **MISSED IN AUDIT 1**
- [ ] `stencil_dict_entries` in `self/build/lower.cot:156` — **MISSED IN AUDIT 1**
- [ ] `analyzeStencilability()` in `self/build/lower.cot:7326` — **MISSED IN AUDIT 1**
- [ ] `ensureGenericFnQueued()` in `self/build/lower.cot:8063` — **MISSED IN AUDIT 1**
- [ ] `lowerGenericFnInstanceInner()` in `self/build/lower.cot:8193` — **MISSED IN AUDIT 1**
- [ ] Dict dispatch in `self/build/lower.cot:2480-2500` (lowerBinary)
- [ ] Dict dispatch in `self/build/lower.cot:3700-3720` (lowerCall)
- [ ] All `current_dict_*` references in `self/build/lower.cot`
- [ ] All `lowered_generics` references in `self/main.cot:438, 626-627, 667, 710`
- [ ] `lowered_generics` field in `self/build/lower.cot:141`
- [ ] All `shape_*` references in `self/main.cot:440, 443, 628, 631, 669, 672, 712, 715`

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
