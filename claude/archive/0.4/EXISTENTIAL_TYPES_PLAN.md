# Existential Types: Execution Plan

## Overview

Add `any Trait` existential types to Cot. An existential is a type-erased container that holds any value conforming to a trait, enabling runtime polymorphism without generics.

```cot
trait Animal {
    fn speak(self: *Self) string
}

impl Animal for Dog { fn speak() string { return "woof" } }
impl Animal for Cat { fn speak() string { return "meow" } }

fn makeNoise(a: any Animal) void {
    println(a.speak())  // dispatches through witness table at runtime
}

fn main() void {
    makeNoise(Dog { .name = "Rex" })   // type-erased into existential container
    makeNoise(Cat { .color = "black" }) // same function, different concrete type
}
```

**Why now:** The VWT infrastructure (88-byte tables, TypeMetadata, all 11 witnesses, `initializeBufferWithCopyOfBuffer`, `canBeInline`) was built exactly for this. Existential types are the feature that consumes VWT.

---

## What Already Exists

| Component | Status | Location |
|-----------|--------|----------|
| `trait` declarations | Working | parser.zig:634, checker.zig:648 |
| `impl Trait for Type` | Working | parser.zig:656, checker.zig:665 |
| Trait bounds (`where T: Trait`) | Working | parser.zig:310, checker.zig:4088 |
| TraitDef (name + method_names) | Working | checker.zig:131 |
| trait_impls map ("Trait:Type" → trait) | Working | checker.zig:146 |
| VWT tables (88B, all 11 slots) | Working | vwt_gen.zig, types.zig:900 |
| TypeMetadata (24B: vwt_ptr, kind, type_idx) | Working | types.zig:970 |
| `canBeInline(size, alignment)` | Working | types.zig:927 |
| `initializeBufferWithCopyOfBuffer` | Partial | vwt_gen.zig:1023 (inline done, box TODO) |
| ValueWitnessFlags (IsNonInline, IsNonPOD) | Working | types.zig:826 |
| 6 trait tests (features.cot) | Passing | test/e2e/features.cot:939-1101 |
| Self-hosted trait infra | Working | self/check/checker.cot:241, 846 |

---

## Swift Existential Architecture (Reference)

### Container Layout (OpaqueExistentialContainer)

Swift reference: `stdlib/public/runtime/ExistentialContainer.h`

```
Offset  Field           Size    Description
──────  ─────           ────    ───────────
0x00    buffer[0]       8       Inline value word 0 (or box pointer if out-of-line)
0x08    buffer[1]       8       Inline value word 1
0x10    buffer[2]       8       Inline value word 2
0x18    type_metadata   8       *TypeMetadata — concrete type's metadata
0x20    witness_table   8       *ProtocolWitnessTable — trait conformance vtable
```

**Total: 40 bytes** (5 words) for a single-protocol existential.

Multi-protocol: 40 + 8 per additional protocol witness table.

### Inline vs Boxed Storage

Decision via `canBeInline(isBitwiseTakable, size, alignment)`:
- **Inline** (size ≤ 24, alignment ≤ 8, bitwise-takable): value stored directly in buffer[0..2]
- **Boxed** (otherwise): heap-allocate box, store pointer in buffer[0]

Swift ref: `GenExistential.cpp:emitExistentialErasure()`

### Protocol Witness Table (PWT)

Per-conformance table mapping trait method names to concrete implementations:

```
Offset  Field                   Description
──────  ─────                   ───────────
0x00    protocol_descriptor     Pointer to protocol metadata (or null)
0x08    method_0                Function pointer: first trait method
0x10    method_1                Function pointer: second trait method
...     ...                     One slot per trait method
```

Swift ref: `GenProto.cpp:emitWitnessTable()`

### Existential Operations

| Operation | Swift Implementation | Cot Equivalent |
|-----------|---------------------|----------------|
| **Erase** (concrete → existential) | `emitExistentialErasure` | Copy value to buffer, set metadata + PWT |
| **Open** (existential → abstract) | `emitOpenExistential` | Load metadata + PWT, project value address |
| **Method dispatch** | `emitWitnessMethodRef` → `call_indirect` | Load fn ptr from PWT, call with projected value |
| **Copy** | `initializeBufferWithCopyOfBuffer` via metadata VWT | Already implemented in vwt_gen.zig |
| **Destroy** | `destroy` via metadata VWT | Already implemented in vwt_gen.zig |
| **Cast** (`as?`) | `dynamic_cast` via metadata kind check | Load metadata.type_idx, compare |

---

## Execution Steps

### Phase 1: Type System (types.zig, checker.zig)

#### Step 1.1: Add Existential Type to Type Union

**File:** `compiler/frontend/types.zig`

Add new type variant:
```zig
pub const ExistentialType = struct {
    trait_name: []const u8,
    /// Cached protocol witness table info for fast method dispatch.
    method_names: []const []const u8,
    /// Number of methods in the trait (= PWT size in slots, excluding descriptor).
    method_count: u32,
};
```

Add to Type union: `.existential => |e| ...`

Size: 40 bytes (5 words — 3 buffer + metadata + PWT pointer).

Add to `sizeOf`: `.existential => 40`
Add to `alignmentOf`: `.existential => 8`
Add to `isTrivial`: `.existential => false` (has VWT-managed value inside)
Add to `couldBeARC`: `.existential => true`
Add to `typeName`: `.existential => |e| e.trait_name`

#### Step 1.2: Parse `any Trait` Syntax

**File:** `compiler/frontend/parser.zig`

In type expression parsing, when encountering `any` keyword followed by an identifier:
```
any Animal  →  TypeExpr { kind: .existential, data1: trait_name_node }
```

Add `kw_any` to scanner keywords. Add `TYPE_EXISTENTIAL` kind to type expression.

Alternative syntax options:
- `any Animal` (Swift style — recommended, familiar)
- `dyn Animal` (Rust style)
- `Animal` bare (Go style — implicit interface, not recommended for Cot)

**Recommendation: `any Animal`** — matches Swift, clear intent, distinguishes from concrete types.

#### Step 1.3: Resolve Existential Types in Checker

**File:** `compiler/frontend/checker.zig`

When `resolveTypeExpr` encounters a `TYPE_EXISTENTIAL` node:
1. Look up the trait in `trait_defs`
2. Create an `ExistentialType` in the type registry
3. Cache it (same trait → same type index)

```zig
if (kind == TYPE_EXISTENTIAL) {
    const trait_name = self.resolveIdentName(d1);
    const trait_def = self.generics.trait_defs.get(trait_name) orelse {
        self.err.error("undefined trait");
        return invalid_type;
    };
    return self.types.makeExistential(trait_name, trait_def.method_names);
}
```

---

### Phase 2: Protocol Witness Tables (lower.zig, driver.zig)

#### Step 2.1: Generate PWTs for Each Conformance

**File:** `compiler/frontend/lower.zig` or new file `compiler/frontend/pwt_gen.zig`

For each `impl Trait for Type` block, generate a global PWT:

**Global name:** `__pwt_{TraitName}_{TypeName}` (e.g., `__pwt_Animal_Dog`)

**Layout:**
```
[0]  method_0 fn ptr  (e.g., Dog_speak)
[1]  method_1 fn ptr  (if trait has 2+ methods)
...
```

**Size:** `method_count * 8` bytes.

**Emission:** In `driver.zig` after lowering (alongside VWT emission), or lazily in the lowerer when an existential type is first used.

#### Step 2.2: Emit PWT Init Functions

Like VWT init functions (`__vwt_init_{type}`), emit PWT init functions that store function addresses into the global:

```
fn __pwt_init_Animal_Dog():
    pwt_addr = addr_global(__pwt_Animal_Dog)
    store pwt_addr[0] = func_addr(Dog_speak)
    // ... one store per trait method
```

Register these in `__cot_init_globals` to run at startup.

---

### Phase 3: Type Erasure (Concrete → Existential)

#### Step 3.1: Implicit Coercion

When assigning a concrete type to an `any Trait` variable/parameter:

**Checker:** Verify the concrete type implements the trait (check `trait_impls` map).

**Lowerer:** Emit existential container construction:

```
// fn foo(a: any Animal) called with Dog { ... }
// 1. Allocate existential container (40 bytes on stack)
var container: [40]u8
// 2. Store value in buffer (inline or boxed)
if (canBeInline(Dog)):
    memcpy(&container[0], &dog_value, sizeof(Dog))
else:
    box = alloc(0, sizeof(Dog))
    memcpy(box, &dog_value, sizeof(Dog))
    store(&container[0], box)
// 3. Store TypeMetadata pointer
store(&container[24], addr_global(__type_metadata_Dog))
// 4. Store PWT pointer
store(&container[32], addr_global(__pwt_Animal_Dog))
```

This is Swift's `emitExistentialErasure` (GenExistential.cpp:350-420).

#### Step 3.2: Value Buffer Management

The `initializeBufferWithCopyOfBuffer` VWT witness handles copy. Complete the out-of-line path in `vwt_gen.zig:1040-1046`:

```zig
// Out-of-line: allocate new box, deep-copy via initializeWithCopy
const size_val = emitConstInt(entry.size);
const new_box = emitCall("alloc", [0, size_val]);
const copy_name = "__vwt_initializeWithCopy_" + type_name;
emitCall(copy_name, [new_box, src_box_ptr, metadata]);
emitPtrStoreValue(dest, new_box);
```

---

### Phase 4: Method Dispatch (Indirect Calls)

#### Step 4.1: Lower Method Calls on Existentials

When `a.speak()` is called and `a: any Animal`:

```
// 1. Load PWT from container offset 32
pwt_ptr = load(&container[32])
// 2. Load method function pointer from PWT
//    speak is method index 0 in trait Animal
fn_ptr = load(pwt_ptr + 0 * 8)
// 3. Load TypeMetadata from container offset 24
metadata = load(&container[24])
// 4. Project value address (buffer start = container offset 0)
value_ptr = &container[0]
// 5. Call indirectly: fn_ptr(value_ptr, ...)
call_indirect(fn_ptr, [value_ptr, ...args])
```

This maps to Swift's `emitWitnessMethodRef` + `emitApply`.

**Key detail:** The trait method's `self: *Self` parameter receives a POINTER to the value buffer (or to the boxed value). The concrete method implementation takes `self: *Dog`. The existential dispatch passes the value pointer directly — no cast needed since both are i64 pointers.

#### Step 4.2: Handle Inline vs Boxed Value Projection

For inline values: `value_ptr = &container[0]` (buffer start)
For boxed values: `value_ptr = load(&container[0])` (deref box pointer)

**Decision at call site:** Read `metadata.vwt.flags.IsNonInline`:
```
metadata_ptr = load(&container[24])
vwt_ptr = load(metadata_ptr + 0)     // VWT at metadata[0]
flags = load(vwt_ptr + 80)            // flags at VWT offset 0x50
is_boxed = flags & 0x00020000         // IsNonInline bit 17
if (is_boxed):
    value_ptr = load(&container[0])   // deref box
else:
    value_ptr = &container[0]         // direct buffer
```

---

### Phase 5: Existential Lifecycle (ARC)

#### Step 5.1: Copy Existential

When copying an `any Trait` value:
1. Copy the 40-byte container
2. Call `initializeBufferWithCopyOfBuffer` through the VWT to properly copy/retain the value

```
// Load VWT from source container's metadata
src_metadata = load(&src[24])
src_vwt = load(src_metadata + 0)
buffer_copy_fn = load(src_vwt + 0)  // slot 0 = initializeBufferWithCopyOfBuffer
call_indirect(buffer_copy_fn, [&dest[0], &src[0], src_metadata])
// Copy metadata and PWT pointers
store(&dest[24], src_metadata)
store(&dest[32], load(&src[32]))
```

#### Step 5.2: Destroy Existential

When destroying an `any Trait` value:
1. Load VWT from metadata
2. Call `destroy` through VWT on the value buffer
3. If boxed: the destroy witness handles deallocation

```
metadata = load(&container[24])
vwt = load(metadata + 0)
destroy_fn = load(vwt + 8)  // slot 1 = destroy
// Project value address (inline or boxed)
value_ptr = project_value(&container[0], vwt)
call_indirect(destroy_fn, [value_ptr, metadata])
```

#### Step 5.3: VWT Dispatch for Existentials

Add existential case to `emitCopyValue` and `emitDestroyValue` in lower.zig:

```zig
if (info == .existential) {
    // Copy: use VWT initializeBufferWithCopyOfBuffer
    // Destroy: use VWT destroy
    // Both loaded from metadata stored in the container
}
```

---

### Phase 6: Dynamic Casting

#### Step 6.1: `as?` Cast (Existential → Concrete)

```cot
fn tryGetDog(a: any Animal) ?*Dog {
    return a as? Dog
}
```

Implementation: load `metadata.type_idx` from the container, compare with Dog's type index. If match, return pointer to value buffer. If not, return null.

```
metadata = load(&container[24])
type_idx = load(metadata + 16)  // type_idx at metadata[2]
if (type_idx == Dog_type_idx):
    return project_value(&container[0], vwt)
else:
    return null
```

#### Step 6.2: `is` Check

```cot
if (a is Dog) { ... }
```

Same as `as?` but returns bool, doesn't project value.

---

### Phase 7: Self-Hosted Port

Port all of the above to `self/` files:
- `self/check/types.cot` — ExistentialType
- `self/check/checker.cot` — `any Trait` resolution, coercion checking
- `self/build/lower.cot` — container construction, PWT generation, indirect dispatch

---

### Phase 8: Opaque Generic Bodies — Full Swift IRGen Port

**Goal:** A shared generic function body must NEVER know the concrete type of `T`. Every
operation on `T` values dispatches through metadata + VWT + protocol witness tables at runtime.
This is the full Swift `GenOpaque.cpp` / `GenCall.cpp` / `GenProto.cpp` pattern.

**Current state (BROKEN):** Base-name sharing emits ONE body for all `T`, but the body bakes
in compile-time type substitution from the first instantiation. `@sizeOf(T)` resolves to a
constant. `a + b` compiles to `i64.add` or `i32.add`. `ptr.*` loads a fixed number of bytes.
Metadata params exist but call sites pass `0`. This means `generic_add(i32)` and
`generic_add(i64)` share a body that does `i64.add` — producing garbage for i32 values.

**The fix:** Make T fully opaque in the shared body. Every operation on T dispatches through
runtime metadata. This is 10 changes, in dependency order.

**Swift reference map:**

| Cot operation on T | Swift IRGen function | File |
|---------------------|---------------------|------|
| `@sizeOf(T)` | `emitLoadOfSize()` | GenOpaque.cpp:1025 |
| `@alignOf(T)` | `emitLoadOfAlignmentMask()` | GenOpaque.cpp:1043 |
| `var x: T` (stack alloc) | `emitDynamicAlloca()` | IRGenFunction.cpp |
| `ptr.* = value` (store T) | `emitInitializeWithCopyCall()` | GenOpaque.cpp:1117 |
| `ptr.*` (load T) | `emitInitializeWithCopyCall()` to local | GenOpaque.cpp:1117 |
| `return x` (return T) | `addIndirectResult()` — always SRET | GenCall.cpp:677 |
| `f(x)` (pass T arg) | indirect convention — pass by address | GenCall.cpp |
| `var b = a` (copy T) | `emitInitializeWithCopyCall()` via VWT | GenOpaque.cpp:1117 |
| `a + b` (arithmetic on T) | `emitWitnessMethodRef()` → PWT fn ptr | GenProto.cpp:3802 |
| scope exit (destroy T) | `emitDestroyCall()` via VWT | GenOpaque.cpp |

---

#### Step 8.1: Call Sites Pass Real Metadata

Replace `emitConstInt(0)` metadata placeholders with real `__type_metadata_{type}` global
addresses at ALL generic call sites.

**Swift ref:** `GenProto.cpp:3947-4001 emitPolymorphicArguments()` — callers pass type
metadata pointers as trailing arguments for every generic type parameter.

```zig
// Current (broken):
const meta = try fb.emitConstInt(0, TypeRegistry.I64, call.span);

// Fixed:
const type_name = self.type_reg.typeName(inst_info.type_args[i]);
const meta_global = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{type_name});
const meta = if (self.builder.lookupGlobal(meta_global)) |gi|
    try fb.emitAddrGlobal(gi.idx, meta_global, TypeRegistry.I64, call.span)
else
    try fb.emitConstInt(0, TypeRegistry.I64, call.span);
```

**Locations (4 sites):**
- `lowerCall` — free generic function calls (lower.zig, generic call path)
- `lowerMethodCall` — generic method calls (lower.zig, method call path)
- `lowerGenericFnInstanceVWT` — calls from within a generic body to other generics
- Same 3 sites in selfcot `self/build/lower.cot`

---

#### Step 8.2: VWT Metadata for ALL Types (Including Trivials)

`emitVWTWitnesses` in `driver.zig` currently emits metadata only for non-trivial types (structs,
unions). But `i64`, `i32`, `bool`, `f64` etc. also need metadata when used as type arguments —
the shared body loads `@sizeOf(T)` from metadata at runtime.

**Swift ref:** Every type has metadata. `swift/stdlib/public/core/BuiltinIntegers.swift` defines
metadata for `Int`, `Int32`, etc.

**Fix:** After emitting VWT for non-trivial types, also emit lightweight metadata for every
trivial type that appears as a generic type argument:

```zig
// In driver.zig, after emitVWTWitnesses:
for (generic_type_args) |type_idx| {
    if (type_reg.isTrivial(type_idx)) {
        const name = type_reg.typeName(type_idx);
        const meta_name = "__type_metadata_" ++ name;
        if (builder.lookupGlobal(meta_name) != null) continue;
        // Emit: global [vwt_ptr=0, size, stride, flags=trivial]
        // VWT ptr is 0 for trivials — @sizeOf reads from metadata.size directly
        emitTrivialTypeMetadata(builder, name, type_reg.sizeOf(type_idx));
    }
}
```

**TypeMetadata layout (24 bytes):**
```
Offset  Field       Value for trivial i64
0x00    vwt_ptr     0 (no VWT — trivial types don't need witnesses)
0x08    size        8
0x10    stride      8
```

---

#### Step 8.3: `@sizeOf(T)` Loads from Metadata at Runtime

When `@sizeOf(T)` appears inside a generic body and `T` is a type parameter, emit a runtime
load from the metadata parameter instead of compile-time resolution.

**Swift ref:** `GenOpaque.cpp:1025 emitLoadOfSize()` → `emitLoadOfValueWitnessValueFromMetadata()`

```zig
// In lowerBuiltinSizeOf:
if (self.type_substitution) |sub| {
    // Check if the type came from a type parameter
    if (self.isTypeParamOrigin(type_idx, sub)) {
        const param_name = self.getTypeParamName(type_idx, sub);
        const meta_local_name = try std.fmt.allocPrint(self.allocator,
            "__metadata_{s}", .{param_name});
        const meta_local = fb.lookupLocal(meta_local_name) orelse
            return try fb.emitConstInt(@intCast(self.type_reg.sizeOf(type_idx)),
                TypeRegistry.I64, span);
        const meta_val = try fb.emitLoadLocal(meta_local, TypeRegistry.I64, span);
        // metadata.size is at offset 0x08 (past vwt_ptr)
        const size_offset = try fb.emitConstInt(8, TypeRegistry.I64, span);
        const size_addr = try fb.emitBinary(.add, meta_val, size_offset,
            TypeRegistry.I64, span);
        return try fb.emitPtrLoadValue(size_addr, TypeRegistry.I64, span);
    }
}
```

---

#### Step 8.4: Opaque Stack Allocation of T

When a local variable of type T is declared inside a generic body, allocate stack space using
the runtime size from metadata instead of compile-time size.

**Swift ref:** `IRGenFunction.cpp emitDynamicAlloca()` — `alloca` with runtime size value.

In Cot's IR, `addLocalWithSize` takes a compile-time size. For opaque T, this must use the
runtime-loaded size from Step 8.3:

```zig
// Current (broken):
const local = try fb.addLocalWithSize("x", type_idx, false, self.type_reg.sizeOf(type_idx));

// Fixed (when T is a type parameter):
if (self.isTypeParamOrigin(type_idx, sub)) {
    const runtime_size = try self.emitLoadSizeFromMetadata(fb, type_idx, sub, span);
    const local = try fb.addDynamicLocal("x", type_idx, false, runtime_size);
} else {
    const local = try fb.addLocalWithSize("x", type_idx, false, self.type_reg.sizeOf(type_idx));
}
```

This requires a new IR node `addDynamicLocal` that takes a runtime size value instead of a
compile-time constant. The SSA builder must emit `alloca` with the dynamic size.

For native codegen: Cranelift's `stack_slot` is fixed-size, so dynamic locals need
`stack_addr` + dynamic offset management, or use `alloca`-style frame pointer bumping.

For wasm codegen: Wasm locals are fixed-size. Dynamic locals must be heap-allocated or
use a shadow stack region.

**Simpler alternative:** Use a fixed maximum buffer size (e.g., 24 bytes = 3 words, matching
Swift's `FixedBuffer`). If the type fits in the buffer, use it inline. If not, heap-allocate
and store a pointer. This avoids dynamic alloca entirely.

---

#### Step 8.5: Opaque Load/Store of T Values

When the body reads or writes a value of type T through a pointer (`ptr.*`, `ptr.* = val`),
it must use runtime-sized memcpy instead of fixed-width load/store.

**Swift ref:** `GenOpaque.cpp:1117 emitInitializeWithCopyCall()` — copies `metadata.size`
bytes via VWT `initializeWithCopy` witness. For trivial types, this is just memcpy.

```zig
// Current (broken — bakes in i64-width load):
const val = try fb.emitPtrLoadValue(ptr, type_idx, span);

// Fixed (when T is a type parameter):
if (self.isTypeParamOrigin(type_idx, sub)) {
    // Allocate buffer for the value
    const size = try self.emitLoadSizeFromMetadata(fb, type_idx, sub, span);
    const buf = try fb.emitCall("alloca_dynamic", &.{size}, false, TypeRegistry.I64, span);
    // memcpy(buf, ptr, size) — opaque copy
    var args = [_]ir.NodeIndex{ buf, ptr, size };
    _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, span);
    return buf;  // return address of copied value
} else {
    return try fb.emitPtrLoadValue(ptr, type_idx, span);
}
```

**Key insight:** In the opaque body, values of type T are ALWAYS passed by address, never
by value. `ptr.*` returns an ADDRESS to the value, not the value itself. This matches Swift's
`Address` type throughout IRGen — generic values are never SSA scalars.

---

#### Step 8.6: Opaque Return (SRET for Generic Return Types)

**Already partially done.** `has_indirect_result` forces SRET when the return type is a type
parameter. The body writes the return value to the `__sret` pointer via memcpy of
`metadata.size` bytes.

**Swift ref:** `GenCall.cpp:677 addIndirectResult()` — generic returns are ALWAYS indirect.
The caller allocates destination space and passes a pointer as the first argument.

```zig
// In the body's return path, when return type is T:
if (self.isTypeParamOrigin(return_type, sub)) {
    const sret_ptr = try fb.emitLoadLocal(fb.lookupLocal("__sret").?, TypeRegistry.I64, span);
    const size = try self.emitLoadSizeFromMetadata(fb, return_type, sub, span);
    const val_addr = ...; // address of the value to return
    var args = [_]ir.NodeIndex{ sret_ptr, val_addr, size };
    _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, span);
    _ = try fb.emitRet(null, span);
}
```

---

#### Step 8.7: Opaque Argument Passing (Indirect Convention for T Params)

Function parameters of type T must be passed by address (pointer), not by value. The caller
stores the value to a temp local and passes its address. The callee receives a pointer.

**Swift ref:** `GenCall.cpp` — `@in_guaranteed` convention for non-trivial/opaque types.
The SIL convention passes large/opaque values by reference.

```
// Caller: f(x) where x: T
temp = alloca(sizeof(T))
memcpy(temp, &x, sizeof(T))
call f(temp, metadata)

// Callee: fn f(x: T)
// x is actually *T (pointer to the value)
```

**Already partially done** for existential types (`@in_guaranteed` in checker.zig). Extend
to all generic type parameters in shared bodies.

---

#### Step 8.8: Arithmetic on T via Protocol Witness Tables

When the shared body does `a + b` where `a, b: T`, the `+` must dispatch through a protocol
witness table function pointer — NOT compile to a fixed-width add instruction.

**Swift ref:** `GenProto.cpp:3802 emitWitnessTableRef()` + `emitWitnessMethodRef()`.
In Swift, `a + b` on `T: Numeric` calls `Numeric.+` through the PWT:

```
pwt = witness_table_for(T, Numeric)   // passed as hidden arg
add_fn = load(pwt + offset_of_add)    // load fn pointer from PWT
result = call_indirect(add_fn, &a, &b, metadata)
```

**For Cot:** Cot doesn't yet require trait bounds on generic type params for arithmetic.
`fn add(T)(a: T, b: T) T` works for any T without a bound. Two options:

**Option A (Swift-faithful):** Require trait bounds for arithmetic. `fn add(T)(a: T, b: T) T
where T: Addable` — PWT dispatch through `Addable.+`. This is the correct long-term path.

**Option B (Pragmatic):** For primitive types (i64, i32, f64, etc.), the metadata can include
a "kind" field that selects the correct operation width at runtime:
```
metadata.kind == INT64 → i64.add
metadata.kind == INT32 → i32.add
metadata.kind == FLOAT64 → f64.add
```
This avoids requiring trait bounds for basic arithmetic but doesn't scale to user types.

**Recommendation:** Option A (trait bounds) is the Swift-correct path. It also makes generic
code self-documenting: `fn add(T: Addable)(a: T, b: T) T` explicitly declares what operations
the body needs.

---

#### Step 8.9: Opaque Copy/Destroy via VWT

When copying or destroying a value of type T in a generic body, dispatch through VWT witnesses
instead of compile-time ARC inline code.

**Swift ref:** `GenOpaque.cpp:1117 emitInitializeWithCopyCall()`,
`GenOpaque.cpp emitDestroyCall()`.

**Already partially done** — `emitCopyValue` and `emitDestroyValue` have VWT dispatch blocks.
Extend to recognize when the type is a generic type parameter and use the metadata parameter's
VWT instead of the compile-time type's VWT.

---

#### Step 8.10: Enable True Base-Name Sharing

Once Steps 8.1-8.9 are complete, the shared body is FULLY opaque with respect to T:
- `@sizeOf(T)` → runtime load from metadata
- Stack alloc of T → dynamic size from metadata
- Load/store T → memcpy of metadata.size bytes
- Return T → SRET + memcpy
- Pass T → by address
- Arithmetic on T → PWT dispatch
- Copy/destroy T → VWT dispatch

At this point, `hasFunc(base_name)` correctly skips redundant body emission — one body serves
ALL instantiations with different metadata. Monomorphization becomes an OPTIONAL specialization
pass (like Swift's `GenericSpecializer.cpp`), not required for correctness.

---

#### Swift Audit Findings (2026-03-25)

**Swift GenArchetype.cpp, ResilientTypeInfo.h, SILFunctionType.cpp audit:**

In unspecialized generic bodies, T is **ALWAYS address-only** (`ResilientTypeInfo`,
never `LoadableTypeInfo`). The body **NEVER** loads T to registers. ALL operations
go through VWT witness function calls. This is non-negotiable — it's hardcoded in
Swift's TypeInfo class hierarchy.

| Operation | Swift implementation | Cot equivalent |
|-----------|---------------------|----------------|
| Param `value: T` | @in indirect (pointer) | `__indirect_value: *T` param |
| Use `value` in body | Address-only, never scalar | Keep as pointer, deref through VWT |
| `a + b` on T | PWT witness call | `call_indirect(pwt[add_idx], &a, &b)` |
| `ptr.* = value` | VWT `assignWithCopy(dest, src, meta)` | VWT call, not fixed-width store |
| `return value` | SRET + memcpy(sret, &value, meta.size) | Already have SRET for generic returns |
| `@sizeOf(T)` | `emitLoadOfSize(meta)` → VWT indirection | `load(metadata + 8)` |
| `var x: T` | Dynamic alloca(metadata.size) | Runtime-sized local allocation |

**Key constraint:** The shared body's parameter layout must be IDENTICAL across all
instantiations. This is why T params MUST be indirect (one pointer slot) — different
T sizes (i64=8B, string=16B) use different numbers of value slots, causing metadata
params to land at wrong offsets. Indirect params fix this.

**Root cause of 8.3 regression (discovered during implementation):**
`List(string).append(value: string)` compiles to 4 param slots (self + str_ptr +
str_len + metadata). `List(i64).append(value: i64)` uses 3 slots (self + value +
metadata). The shared body was compiled for string (4 slots), so when called with
i64 (3 slots), `__metadata_T` was at slot 3 instead of slot 2 → read garbage.

#### Implementation Order

| Step | Depends on | Scope | Files | Status |
|------|-----------|-------|-------|--------|
| 8.1 Call sites pass metadata | — | Small | lower.zig (4 sites) | **DONE** |
| 8.2 Trivial type metadata | — | Small | driver.zig, vwt_gen.zig | **DONE** |
| 8.5+8.7 Opaque T body + indirect params | 8.1, 8.2 | **LARGE** | lower.zig | **DONE** (commit 8c7276c, 36d3e98) |
| 8.3 @sizeOf from metadata | 8.5+8.7 | Small | lower.zig | Ready (blocked) |
| 8.4 Dynamic stack alloc | 8.3 | Medium | ir.zig, ssa_builder, codegen | Pending |
| 8.6 Opaque return (SRET memcpy) | 8.5 | Small | lower.zig | Mostly done |
| 8.8 PWT arithmetic | 8.5+8.7 | Large | lower.zig, checker.zig | Pending |
| 8.9 VWT copy/destroy | 8.3 | Small | lower.zig | Mostly done |
| 8.10 Base-name sharing | ALL above | Trivial | lower.zig | Already structured |
| 9 Self-hosted port | ALL 8.x | Large | self/**/*.cot | Pending |

**Corrected critical path:** 8.1 → 8.2 → **8.5+8.7** → 8.3 → 8.10.

Steps 8.5+8.7 are the **blocker** — the body must treat T as fully opaque (address-only).
This is inseparable: indirect params (8.7) requires the body to not load T to scalars
(8.5), and opaque body (8.5) requires params to be pointers (8.7).

Step 8.8 (PWT arithmetic) can be deferred — stdlib containers (List, Map) only use
`@sizeOf(T)` and pointer arithmetic on T, never `a + b` on T values directly.

**Estimated scope:** ~800 lines Zig compiler, ~400 lines selfcot port.

---

## Implementation Order

| Step | Files | Lines est. | Depends on |
|------|-------|------------|------------|
| 1.1 ExistentialType in type union | types.zig | ~30 | — |
| 1.2 Parse `any Trait` | parser.zig, scanner.zig | ~20 | — |
| 1.3 Resolve existential types | checker.zig | ~30 | 1.1, 1.2 |
| 2.1 Generate PWTs | lower.zig or pwt_gen.zig | ~80 | 1.3 |
| 2.2 PWT init functions | lower.zig, driver.zig | ~40 | 2.1 |
| 3.1 Type erasure (concrete → existential) | lower.zig, checker.zig | ~60 | 2.1, 1.3 |
| 3.2 Complete initializeBufferWithCopyOfBuffer | vwt_gen.zig | ~20 | — |
| 4.1 Method dispatch on existentials | lower.zig | ~80 | 3.1, 2.1 |
| 4.2 Inline vs boxed value projection | lower.zig | ~30 | 4.1 |
| 5.1 Copy existential | lower.zig | ~30 | 3.2 |
| 5.2 Destroy existential | lower.zig | ~20 | 5.1 |
| 5.3 VWT dispatch for existentials | lower.zig | ~20 | 5.1, 5.2 |
| 6.1 Dynamic cast (`as?`) | lower.zig, checker.zig | ~40 | 4.1 |
| 6.2 `is` check | lower.zig, checker.zig | ~15 | 6.1 |
| 7 Self-hosted port | self/**/*.cot | ~200 | All above |
| 8.1 Call sites pass real metadata | lower.zig (4 call sites) | ~40 | 2.2 |
| 8.2 Trivial type metadata emission | driver.zig, vwt_gen.zig | ~60 | — |
| 8.3 @sizeOf(T) from metadata | lower.zig (lowerBuiltinSizeOf) | ~50 | 8.1, 8.2 |
| 8.4 Dynamic stack alloc for T | ir.zig, ssa_builder, codegen | ~80 | 8.3 |
| 8.5 Opaque load/store of T | lower.zig (ptr deref paths) | ~120 | 8.3, 8.4 |
| 8.6 Opaque return (SRET memcpy) | lower.zig (return path) | ~30 | 8.5 |
| 8.7 Indirect T params (by address) | lower.zig, checker.zig | ~60 | 8.5 |
| 8.8 PWT arithmetic dispatch | lower.zig, checker.zig | ~150 | 8.7 |
| 8.9 VWT copy/destroy for T params | lower.zig | ~30 | 8.3 |
| 8.10 Enable true base-name sharing | lower.zig (already structured) | ~10 | ALL 8.x |
| 9 Self-hosted port of Phase 8 | self/**/*.cot | ~300 | ALL 8.x |

**Total: ~1,250 lines across ~10 files** (Zig compiler) + ~400 lines self-hosted port.

---

## Test Plan

```cot
trait Animal {
    fn speak(self: *Self) string
    fn age(self: *Self) int
}

struct Dog { name: string, years: int }
struct Cat { color: string, lives: int }

impl Animal for Dog {
    fn speak() string { return "woof" }
    fn age() int { return self.years }
}

impl Animal for Cat {
    fn speak() string { return "meow" }
    fn age() int { return self.lives * 7 }
}

test "existential basic" {
    var a: any Animal = Dog { .name = "Rex", .years = 5 }
    @assertEq(a.speak(), "woof")
    @assertEq(a.age(), 5)
}

test "existential reassign" {
    var a: any Animal = Dog { .name = "Rex", .years = 5 }
    a = Cat { .color = "black", .lives = 9 }
    @assertEq(a.speak(), "meow")
    @assertEq(a.age(), 63)
}

test "existential parameter" {
    fn describe(a: any Animal) string {
        return a.speak()
    }
    @assertEq(describe(Dog { .name = "Spot", .years = 3 }), "woof")
    @assertEq(describe(Cat { .color = "white", .lives = 7 }), "meow")
}

test "existential in list" {
    var animals: List(any Animal) = .{}
    animals.append(Dog { .name = "Rex", .years = 5 })
    animals.append(Cat { .color = "black", .lives = 9 })
    @assertEq(animals.get(0).speak(), "woof")
    @assertEq(animals.get(1).speak(), "meow")
}

test "existential dynamic cast" {
    var a: any Animal = Dog { .name = "Rex", .years = 5 }
    if (a as? Dog) |dog| {
        @assertEq(dog.name, "Rex")
    }
    @assert(!(a is Cat))
}

test "existential copy" {
    var a: any Animal = Dog { .name = "Rex", .years = 5 }
    var b = a  // copies via initializeBufferWithCopyOfBuffer
    @assertEq(b.speak(), "woof")
}
```

---

## Implementation Status (2026-03-25)

### Completed (Phases 1-4 + fixes):

| Component | Status | Swift Reference | Proof |
|-----------|--------|-----------------|-------|
| ExistentialType struct | DONE | ExistentialContainer.h | 40B = 5 words, fields match Swift |
| `any Trait` parsing | DONE | Type expression for `any Protocol` | kw_any + TYPE_EXISTENTIAL |
| Type resolution | DONE | Protocol type lookup | trait_defs + conforming_types |
| sizeOf = 40 | DONE | OpaqueExistentialContainer | 24 buffer + 8 metadata + 8 PWT |
| isTrivial = false | DONE | TypeLowering | Non-trivial (contains VWT value) |
| couldBeARC = true | DONE | TypeLowering | Contains reference-managed values |
| isAssignable coercion | DONE | Implicit coercion Dog → any Animal | conforming_types check |
| SSA large-type (13 sites) | DONE | GenOpaque.cpp | Address-based access for >8B |
| Container construction | DONE | GenExistential.cpp emitExistentialErasure | memcpy buffer + metadata@24 + PWT@32 |
| Inline PWT allocation | DONE | GenProto.cpp emitWitnessTable | alloc + func_addr per method |
| Method dispatch | DONE | GenProto.cpp emitWitnessMethodRef | PWT[idx*8] → call_indirect |
| Self-stripping in checker | DONE | Protocol method signatures | Remove self param for caller |
| resolveTypeNode | DONE | — | Existential case added to lowerer |
| Native dispatch | DONE | GenExistential.cpp CreateStructGEP | Address-based 40B container access via SSA large-type |
| Wasm dispatch | DONE | — | `first=100 second=200` through PWT indirect call |
| CSE aux comparison | DONE | Go cse.go:419-457 cmpVal | Hash + compare Aux field (string, symbol, etc.) |

### Resolved Bugs:

1. **Wasm multi-method PWT ordering** — FIXED. Root cause: CSE pass compared `aux_int` (both 0) but not `aux.string` (different function names), incorrectly merging two distinct `func_addr` values. Fix: ported Go's `cmpVal` Aux comparison to Cot's `valuesEqual` + `hashValue`.

2. **Native SIGSEGV on dispatch** — FIXED. Root cause: SSA `convertLoadLocal` only recognized struct/union/tuple as large types (>8B) needing address-based access. Existential (40B) was loaded as 8B scalar. Fix: added `.existential` to all 13 large-type checks in `ssa_builder.zig`.

### Known Limitations:

1. **Out-of-line storage incomplete** — `emitExistentialErasure` copies only 8 bytes for types > 24B. Should allocate box and store box pointer. Currently all Cot types used with existentials are ≤ 24 bytes so this doesn't trigger in practice.

2. **Global PWT init not called** — `emitProtocolWitnessTables` in driver.zig emits `__pwt_init_{Trait}_{Type}` functions but they are never registered in `__cot_init_globals`. The inline PWT in `emitExistentialErasure` works instead. Global PWT is dead code — may be useful for future optimization (shared PWTs across call sites).

### Not Yet Implemented:

| Feature | Phase | Depends on | Swift Reference |
|---------|-------|------------|-----------------|
| Existential reassignment (`a = c`) | 3 | Coercion in assignment lowering | GenExistential.cpp emitExistentialErasure at assign sites |
| Function parameter coercion | 3 | Coercion in call arg lowering | GenExistential.cpp emitExistentialErasure at call sites |
| Existential ARC lifecycle (copy) | 5 | VWT initializeBufferWithCopyOfBuffer | TypeLowering.cpp emitCopyValue for existentials |
| Existential ARC lifecycle (destroy) | 5 | VWT destroy through metadata | TypeLowering.cpp emitDestroyValue for existentials |
| Dynamic casting (`as?`) | 6 | Metadata type_idx check | GenCast.cpp emitConditionalCheckedCast |
| `is` type check | 6 | Metadata type_idx comparison | GenCast.cpp emitIsType |
| Self-hosted port | 7 | All above | — |
| Real metadata at generic call sites | 8 | Existential infrastructure complete | GenOpaque.cpp emitLoadOfSize |

### Audit Proof: Swift 1:1 Mapping

| Cot Code | Swift Reference | Match |
|----------|-----------------|-------|
| Container 40B layout | OpaqueExistentialContainer (5 words) | ✓ Exact |
| Buffer offset 0 | GenExistential.cpp projectValue() CreateStructGEP(0) | ✓ Exact |
| Metadata offset 24 | GenExistential.cpp projectMetadataRef() offset=getFixedBufferSize() | ✓ Exact |
| PWT offset 32 | GenExistential.cpp projectWitnessTable() offset=24+8 | ✓ Exact |
| PWT method order | GenProto.cpp emitWitnessTable() trait declaration order | ✓ Exact |
| Address-based access | GenExistential.cpp CreateStructGEP pattern | ✓ Exact (SSA large-type) |
| Method dispatch | GenProto.cpp emitWitnessMethodRef() → indirect call | ✓ Exact |
| Self as buffer ptr | GenExistential.cpp projectValue() → address | ✓ Exact |
| CSE aux comparison | Go cse.go:419-457 cmpVal with auxmap | ✓ Exact (hash + equality) |

---

## Risk Notes

1. **Inline vs boxed storage** — Most Cot types are ≤24 bytes (int, string, pointers, small structs). Boxed path only needed for large structs. Start with inline-only, add boxing when needed.

2. **Wasm target** — existentials need `call_indirect` for method dispatch. Wasm already supports this via table. The VWT is native-only but existential dispatch is needed on both targets.

3. **ARC interaction** — Existential containers are non-trivial (contain VWT-managed values). They need VWT dispatch in `emitCopyValue`/`emitDestroyValue`. The container itself is on the stack; the VALUE inside may be ARC-managed.

4. **`Self` type resolution** — Trait methods use `self: *Self`. In concrete impls, `Self` resolves to the concrete type. In existential dispatch, `Self` is opaque — the method receives a raw pointer. The concrete implementation doesn't know it was called through an existential.

5. **Multi-protocol existentials** (`any Animal & Serializable`) — deferred. Single-protocol first. Multi adds 8 bytes per additional protocol.

6. **Performance** — Existential dispatch adds one indirect call per method invocation. Monomorphized generics are still faster. Use existentials for heterogeneous collections and runtime polymorphism; use generics for homogeneous hot paths.
