# Cot Compiler Roadmap: Phase 2

## Strategy Reminder (from CLAUDE.md)

**CRITICAL: Copy Go/Swift code exactly. Do NOT invent logic.**

This document provides extensive reference material from Go and Swift compilers. Each task includes:
1. Exact file paths and line numbers in `~/learning/go/` and `~/learning/swift/`
2. Code snippets to copy
3. Test cases to write first (TDD)
4. Implementation checklist

---

## Phase 1: Heap & ARC Integration

### M17: Frontend Emits Retain/Release

**Goal:** Automatically insert `retain` and `release` calls during compilation.

#### Reference: Swift's Approach

Swift inserts ARC calls during **SIL generation**, not as a separate pass. The key abstraction is `ManagedValue` which pairs a value with a cleanup handle.

**Files to study:**
- `/Users/johnc/learning/swift/lib/SILGen/ManagedValue.h` (lines 40-456)
- `/Users/johnc/learning/swift/lib/SILGen/Cleanup.h` (lines 85-317)
- `/Users/johnc/learning/swift/lib/SILGen/SILGenExpr.cpp` (lines 70-109)

**Swift's ManagedValue (ManagedValue.h:59-95):**
```cpp
class ManagedValue {
  llvm::PointerIntPair<SILValue, 1, bool> valueAndFlag;
  CleanupHandle cleanup;  // Handle to deferred release
};
```

**Swift's Rules for Retain:**
1. When a value enters a new ownership context (+1 value)
2. When a value is passed as an @owned parameter
3. When a value is copied

**Swift's Rules for Release:**
1. At scope exit for owned values
2. When cleanup is emitted (LIFO stack order)
3. NOT when ownership is forwarded to another scope

**Swift's emitManagedCopy (SILGenExpr.cpp:70-87):**
```cpp
ManagedValue SILGenFunction::emitManagedCopy(SILLocation loc, SILValue v,
                                             const TypeLowering &lowering) {
  if (lowering.isTrivial())
    return ManagedValue::forRValueWithoutOwnership(v);  // Skip for trivial types
  v = lowering.emitCopyValue(B, loc, v);                // Emit retain
  return emitManagedRValueWithCleanup(v, lowering);     // Register cleanup
}
```

#### Cot Implementation Plan

**New file:** `compiler/frontend/arc_insertion.zig`

```zig
// ManagedValue equivalent for Cot
pub const ManagedValue = struct {
    value: *ir.Value,
    cleanup: ?CleanupHandle,

    pub fn forOwned(value: *ir.Value, cleanup: CleanupHandle) ManagedValue {
        return .{ .value = value, .cleanup = cleanup };
    }

    pub fn forTrivial(value: *ir.Value) ManagedValue {
        return .{ .value = value, .cleanup = null };
    }

    pub fn forward(self: *ManagedValue, scope: *Scope) *ir.Value {
        // Disable cleanup - ownership transferred
        if (self.cleanup) |c| scope.disableCleanup(c);
        return self.value;
    }
};

// Cleanup stack (LIFO)
pub const CleanupStack = struct {
    items: std.ArrayList(Cleanup),

    pub fn push(self: *CleanupStack, cleanup: Cleanup) CleanupHandle { ... }
    pub fn emitAll(self: *CleanupStack, builder: *IrBuilder) void { ... }
};

pub const Cleanup = struct {
    kind: enum { release, end_borrow },
    value: *ir.Value,
    active: bool,
};
```

**Insert points in `compiler/frontend/lower.zig`:**

| Location | Action |
|----------|--------|
| Function parameter (owned) | Register cleanup for release at scope exit |
| Assignment to variable | Retain RHS, release old value if any |
| Return statement | Forward ownership (disable cleanup) |
| Scope exit | Emit all active cleanups in reverse order |
| Function call (owned param) | Retain argument before call |

#### Tests (Write First)

**File:** `test/cases/arc/retain_on_assign.cot`
```cot
// EXPECT: exit_code=1
fn main() i64 {
    let obj = new Object { value: 1 }  // refcount = 1
    let obj2 = obj                      // retain → refcount = 2
    return retain_count(obj)            // Should be 2
}
```

**File:** `test/cases/arc/release_on_scope_exit.cot`
```cot
// EXPECT: exit_code=0
fn main() i64 {
    {
        let obj = new Object { value: 1 }  // refcount = 1
    }  // release → refcount = 0, freed
    return 0
}
```

**File:** `test/cases/arc/no_retain_on_forward.cot`
```cot
// EXPECT: exit_code=1
fn take_ownership(obj: Object) i64 {
    return retain_count(obj)  // Should be 1 (not 2)
}

fn main() i64 {
    let obj = new Object { value: 1 }
    return take_ownership(obj)  // Forward, don't retain
}
```

#### Checklist

- [ ] Create `ManagedValue` struct in `arc_insertion.zig`
- [ ] Create `CleanupStack` with push/emit operations
- [ ] Modify `lower.zig` to track owned values
- [ ] Insert `retain` on assignment (copy semantics)
- [ ] Insert `release` at scope exit
- [ ] Forward ownership on return (disable cleanup)
- [ ] Skip ARC for trivial types (i64, f64, bool)
- [ ] Write 5+ test cases
- [ ] Verify with Wasm execution

---

### M18: Heap Allocation (new keyword)

**Goal:** Parse `new Type { ... }` and generate heap allocation calls.

#### Reference: Go's Approach

Go uses escape analysis to decide heap vs stack, then emits `newobject` calls.

**Files to study:**
- `/Users/johnc/learning/go/src/cmd/compile/internal/ssagen/ssa.go` (lines 864-889)
- `/Users/johnc/learning/go/src/runtime/malloc.go` (lines 2208-2213)
- `/Users/johnc/learning/go/src/cmd/compile/internal/walk/builtin.go` (lines 601-616)

**Go's newObject (ssagen/ssa.go:864-877):**
```go
func (s *state) newObject(typ *types.Type) *ssa.Value {
    if typ.Size() == 0 {
        return s.newValue1A(ssa.OpAddr, types.NewPtr(typ), ir.Syms.Zerobase, s.sb)
    }
    rtype := s.reflectType(typ)
    return s.rtcall(ir.Syms.Newobject, true, []*types.Type{types.NewPtr(typ)}, rtype)[0]
}
```

**Go's runtime newobject (malloc.go:2208-2213):**
```go
func newobject(typ *_type) unsafe.Pointer {
    return mallocgc(typ.Size_, typ, true)  // Always zeroed
}
```

**Go's walkNew decision (walk/builtin.go:601-616):**
```go
func walkNew(n *ir.UnaryExpr, init *ir.Nodes) ir.Node {
    t := n.Type().Elem()
    if n.Esc() == ir.EscNone {  // Stack allocation possible
        return stackTempAddr(init, t)
    }
    return n  // Leave for SSA (heap allocation)
}
```

#### Cot Implementation Plan

**Syntax:** `new Type { field1: value1, field2: value2 }`

**Parser changes (`compiler/frontend/parser.zig`):**
```zig
fn parseNewExpr(self: *Parser) !*Expr {
    _ = try self.expect(.keyword_new);
    const type_name = try self.parseTypeName();
    _ = try self.expect(.lbrace);
    const fields = try self.parseFieldInitList();
    _ = try self.expect(.rbrace);
    return self.createNewExpr(type_name, fields);
}
```

**IR generation (`compiler/frontend/lower.zig`):**
```zig
fn lowerNewExpr(self: *Lowerer, expr: *ast.NewExpr) !*ir.Value {
    const type_info = self.resolveType(expr.type_name);
    const size = type_info.size;

    // Call cot_alloc(metadata, size) → ptr
    const metadata = self.emitMetadataConstant(type_info);
    const ptr = self.emitCall("cot_alloc", &[_]*ir.Value{ metadata, size });

    // Initialize fields
    for (expr.fields) |field| {
        const offset = type_info.fieldOffset(field.name);
        const field_ptr = self.emitOffPtr(ptr, offset);
        self.emitStore(field_ptr, self.lowerExpr(field.value));
    }

    // Register cleanup for release
    self.registerCleanup(.{ .kind = .release, .value = ptr });

    return ptr;
}
```

**SSA op additions (`compiler/ssa/op.zig`):**
```zig
// Heap allocation
alloc,          // alloc(metadata, size) -> ptr
```

**Wasm codegen (`compiler/codegen/wasm/gen.zig`):**
```zig
.alloc => {
    // Push args: metadata, size
    try self.getValue64(v.args[0]);  // metadata (type descriptor)
    try self.getValue64(v.args[1]);  // size in bytes
    // Call cot_alloc (already in func_indices from arc.zig)
    const func_idx = self.func_indices.get("cot_alloc") orelse 0;
    const p = try self.builder.append(.call);
    p.to = prog_mod.constAddr(func_idx);
},
```

#### Tests (Write First)

**File:** `test/cases/arc/new_simple.cot`
```cot
// EXPECT: exit_code=42
struct Point { x: i64, y: i64 }

fn main() i64 {
    let p = new Point { x: 40, y: 2 }
    return p.x + p.y
}
```

**File:** `test/cases/arc/new_refcount.cot`
```cot
// EXPECT: exit_code=1
struct Box { value: i64 }

fn main() i64 {
    let b = new Box { value: 42 }
    return retain_count(b)  // Should be 1
}
```

**File:** `test/cases/arc/new_auto_release.cot`
```cot
// EXPECT: exit_code=0
struct Data { x: i64 }

fn main() i64 {
    {
        let d = new Data { x: 99 }
        // d goes out of scope, should be released
    }
    return 0  // No memory leak
}
```

#### Checklist

- [ ] Add `keyword_new` to scanner
- [ ] Parse `new Type { ... }` expression
- [ ] Add `alloc` SSA op
- [ ] Generate `cot_alloc` call in IR lowering
- [ ] Initialize struct fields after allocation
- [ ] Register cleanup for automatic release
- [ ] Add `cot_alloc` to arc.zig (already exists in legacy API)
- [ ] Wire `cot_alloc` into Linker
- [ ] Write 5+ test cases
- [ ] Verify memory is freed on scope exit

---

### M19: Destructor Calls on Release

**Goal:** When refcount reaches 0, call type-specific destructor before freeing.

#### Reference: Swift's Approach

Swift stores destructor pointer in type metadata and calls it via indirect call.

**Files to study:**
- `/Users/johnc/learning/swift/stdlib/public/runtime/HeapObject.cpp` (lines 216-268)
- `/Users/johnc/learning/swift/include/swift/Runtime/Metadata.h`

**Swift's release path (HeapObject.cpp):**
```cpp
void swift::swift_deallocObject(HeapObject *object, size_t allocatedSize,
                                 size_t allocatedAlignMask) {
  // ... weak reference handling ...
  auto metadata = object->metadata;

  // Call destructor via metadata
  if (metadata->getDestructor()) {
    metadata->getDestructor()(object);
  }

  swift_deallocClassInstance(object, allocatedSize, allocatedAlignMask);
}
```

#### Cot Implementation Plan

**Metadata layout:**
```
Offset 0: type_id (i32)
Offset 4: size (i32)
Offset 8: destructor_ptr (i32) - function index or 0 if none
```

**Updated release in `arc.zig`:**
```zig
fn generateReleaseBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // ... existing null/immortal checks ...

    // if (new_count == 0) { call destructor }
    try code.emitLocalGet(3);  // new_count
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);

    // Load destructor_ptr from metadata
    // header_ptr is in local 1
    try code.emitLocalGet(1);  // header_ptr
    try code.emitI32WrapI64();
    try code.emitI32Load(2, METADATA_OFFSET);  // Load metadata ptr
    try code.emitI32Load(2, 8);                // Load destructor_ptr at offset 8

    // If destructor_ptr != 0, call it
    try code.emitLocalTee(4);  // Save destructor_ptr to local 4
    try code.emitIf(BLOCK_VOID);
    try code.emitLocalGet(0);  // Push object as argument
    try code.emitLocalGet(4);  // Push destructor_ptr
    try code.emitCallIndirect(destructor_type_idx);  // Indirect call
    try code.emitEnd();

    // TODO: Add to free list
    try code.emitEnd();

    return code.finish();
}
```

**Frontend support for destructors:**

When a struct has a `deinit` method:
```cot
struct File {
    handle: i64

    fn deinit(self) {
        close(self.handle)
    }
}
```

The compiler:
1. Generates a destructor function: `__File_deinit(ptr)`
2. Stores its index in the type's metadata
3. On release to 0, the indirect call invokes it

#### Tests (Write First)

**File:** `test/cases/arc/destructor_called.cot`
```cot
// EXPECT: exit_code=99
var destructor_called: i64 = 0

struct Tracer {
    fn deinit(self) {
        destructor_called = 99
    }
}

fn main() i64 {
    {
        let t = new Tracer {}
    }  // deinit should be called here
    return destructor_called
}
```

#### Checklist

- [ ] Define metadata struct layout
- [ ] Generate metadata for each struct type
- [ ] Parse `fn deinit(self)` in struct body
- [ ] Generate destructor wrapper function
- [ ] Store destructor index in metadata
- [ ] Add `call_indirect` to release path
- [ ] Add Wasm table section for indirect calls
- [ ] Write test cases

---

## Phase 2: Richer Types

### M20: String Concatenation & Indexing

**Goal:** Implement `+` for strings and `s[i]` indexing.

#### Reference: Go's Approach

**Files to study:**
- `/Users/johnc/learning/go/src/runtime/string.go` (lines 23-79)
- `/Users/johnc/learning/go/src/cmd/compile/internal/walk/expr.go` (line 519)
- `/Users/johnc/learning/go/src/internal/bytealg/compare_generic.go` (lines 55-76)

**Go's concatstrings (runtime/string.go:28-59):**
```go
func concatstrings(buf *tmpBuf, a []string) string {
    idx := 0
    l := 0
    count := 0

    // Calculate total length
    for i, x := range a {
        n := len(x)
        if n == 0 { continue }
        if l+n < l { throw("string concatenation too long") }
        l += n
        count++
        idx = i
    }

    // Single non-empty string: return directly
    if count == 1 {
        return a[idx]
    }

    // Allocate and copy
    s, b := rawstringtmp(buf, l)
    for _, x := range a {
        n := copy(b, x)
        b = b[n:]
    }
    return s
}
```

**Go's string indexing (walk/expr.go:826-831):**
- Bounds check: `if index >= len { panic }`
- Memory load: `memory[ptr + index]` (single byte)

#### Cot Implementation Plan

**Runtime function:** `cot_string_concat(s1_ptr, s1_len, s2_ptr, s2_len) -> (ptr, len)`

```zig
// In arc.zig or new strings_runtime.zig
fn generateStringConcatBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: s1_ptr(0), s1_len(1), s2_ptr(2), s2_len(3)
    // Locals: new_len(4), new_ptr(5)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64 });

    // new_len = s1_len + s2_len
    try code.emitLocalGet(1);
    try code.emitLocalGet(3);
    try code.emitI64Add();
    try code.emitLocalSet(4);

    // new_ptr = malloc(new_len)
    try code.emitLocalGet(4);
    try code.emitCall(malloc_idx);
    try code.emitLocalSet(5);

    // memcpy(new_ptr, s1_ptr, s1_len)
    try code.emitLocalGet(5);
    try code.emitLocalGet(0);
    try code.emitLocalGet(1);
    try code.emitCall(memcpy_idx);

    // memcpy(new_ptr + s1_len, s2_ptr, s2_len)
    try code.emitLocalGet(5);
    try code.emitLocalGet(1);
    try code.emitI64Add();
    try code.emitLocalGet(2);
    try code.emitLocalGet(3);
    try code.emitCall(memcpy_idx);

    // Return (new_ptr, new_len) as string
    try code.emitLocalGet(5);
    try code.emitLocalGet(4);

    return code.finish();
}
```

**Frontend support:**

In `lower.zig`, when lowering `+` operator:
```zig
if (left_type.isString() and right_type.isString()) {
    // Decompose strings
    const s1_ptr = self.emit(.string_ptr, left);
    const s1_len = self.emit(.string_len, left);
    const s2_ptr = self.emit(.string_ptr, right);
    const s2_len = self.emit(.string_len, right);

    // Call runtime
    const result = self.emitCall("cot_string_concat",
        &[_]*ir.Value{ s1_ptr, s1_len, s2_ptr, s2_len });

    // Build new string (result is ptr, len pair)
    return self.emit(.string_make, result.ptr, result.len);
}
```

**String indexing:**
```zig
// s[i] where s is string
fn lowerStringIndex(self: *Lowerer, s: *ir.Value, i: *ir.Value) !*ir.Value {
    const ptr = self.emit(.string_ptr, s);
    const len = self.emit(.string_len, s);

    // Bounds check
    self.emitBoundsCheck(i, len);

    // Load single byte
    const addr = self.emit(.add_ptr, ptr, i);
    return self.emit(.load_u8, addr);
}
```

#### Tests (Write First)

**File:** `test/cases/strings/concat_simple.cot`
```cot
// EXPECT: exit_code=11
fn main() i64 {
    let s1 = "hello"
    let s2 = " world"
    let s3 = s1 + s2
    return len(s3)  // Should be 11
}
```

**File:** `test/cases/strings/index_first.cot`
```cot
// EXPECT: exit_code=104
fn main() i64 {
    let s = "hello"
    return s[0]  // 'h' = 104
}
```

**File:** `test/cases/strings/index_last.cot`
```cot
// EXPECT: exit_code=111
fn main() i64 {
    let s = "hello"
    return s[4]  // 'o' = 111
}
```

#### Checklist

- [ ] Add `string_concat` SSA op
- [ ] Generate `cot_string_concat` runtime function
- [ ] Lower `+` operator for strings
- [ ] Implement string indexing with bounds check
- [ ] Add `load_u8` for byte access
- [ ] Write 5+ test cases

---

### M21: Array Literals & Append

**Goal:** Support `[1, 2, 3]` literals and `append(slice, elem)`.

#### Reference: Go's Approach

**Files to study:**
- `/Users/johnc/learning/go/src/runtime/slice.go` (lines 102-118, 178-287)
- `/Users/johnc/learning/go/src/cmd/compile/internal/walk/builtin.go` (lines 44-128)

**Go's slice layout (runtime/slice.go:16-20):**
```go
type slice struct {
    array unsafe.Pointer  // ptr to backing array
    len   int
    cap   int
}
```

**Go's makeslice (runtime/slice.go:102-118):**
```go
func makeslice(et *_type, len, cap int) unsafe.Pointer {
    mem, overflow := math.MulUintptr(et.Size_, uintptr(cap))
    if overflow || mem > maxAlloc || len < 0 || len > cap {
        // error handling
    }
    return mallocgc(mem, et, true)
}
```

**Go's growslice (runtime/slice.go:325-358):**
```go
func nextslicecap(newLen, oldCap int) int {
    newcap := oldCap
    doublecap := newcap + newcap
    if newLen > doublecap {
        return newLen
    }
    const threshold = 256
    if oldCap < threshold {
        return doublecap
    }
    for {
        newcap += (newcap + 3*threshold) >> 2  // Grow by ~25%
        if newcap >= newLen {
            return newcap
        }
    }
}
```

**Go's append lowering (walk/builtin.go:44-128):**
```go
// Expand append(slice, elem1, elem2) into:
newLen := slice.len + numElems
if cap >= newLen {
    // Fast path: just update length
    slice = slice[:newLen]
} else {
    // Slow path: grow and copy
    slice = growslice(slice.ptr, newLen, slice.cap, numElems, elemType)
}
slice[oldLen] = elem1
slice[oldLen+1] = elem2
```

#### Cot Implementation Plan

**Slice SSA ops (already in op.zig):**
```zig
slice_make,     // (ptr, len, cap) -> slice
slice_ptr,      // slice -> ptr
slice_len,      // slice -> len
slice_cap,      // slice -> cap
```

**New runtime functions:**
- `cot_makeslice(elem_size, len, cap) -> ptr`
- `cot_growslice(old_ptr, new_len, old_cap, num, elem_size) -> (ptr, len, cap)`

**Array literal lowering:**
```zig
// [1, 2, 3] with type []i64
fn lowerArrayLiteral(self: *Lowerer, elements: []*ast.Expr) !*ir.Value {
    const len = elements.len;
    const cap = len;  // Initial capacity = length
    const elem_size = 8;  // i64

    // Allocate backing array
    const ptr = self.emitCall("cot_makeslice",
        &[_]*ir.Value{ elem_size, len, cap });

    // Store each element
    for (elements, 0..) |elem, i| {
        const offset = i * elem_size;
        const addr = self.emit(.add_ptr, ptr, offset);
        self.emit(.store, addr, self.lowerExpr(elem));
    }

    // Create slice struct
    return self.emit(.slice_make, ptr, len, cap);
}
```

**Append lowering:**
```zig
// append(slice, elem)
fn lowerAppend(self: *Lowerer, slice: *ir.Value, elem: *ir.Value) !*ir.Value {
    const ptr = self.emit(.slice_ptr, slice);
    const len = self.emit(.slice_len, slice);
    const cap = self.emit(.slice_cap, slice);

    const new_len = self.emit(.add, len, 1);

    // if (new_len > cap) { grow }
    const needs_grow = self.emit(.gt, new_len, cap);

    // Branch: grow path vs fast path
    // ... emit control flow ...

    // Store element at slice[len]
    const offset = self.emit(.mul, len, elem_size);
    const addr = self.emit(.add_ptr, new_ptr, offset);
    self.emit(.store, addr, elem);

    return self.emit(.slice_make, new_ptr, new_len, new_cap);
}
```

#### Tests (Write First)

**File:** `test/cases/arrays/literal_simple.cot`
```cot
// EXPECT: exit_code=6
fn main() i64 {
    let arr = [1, 2, 3]
    return arr[0] + arr[1] + arr[2]
}
```

**File:** `test/cases/arrays/append_one.cot`
```cot
// EXPECT: exit_code=4
fn main() i64 {
    let arr = [1, 2, 3]
    let arr2 = append(arr, 4)
    return len(arr2)
}
```

**File:** `test/cases/arrays/append_grow.cot`
```cot
// EXPECT: exit_code=10
fn main() i64 {
    var arr = []i64{}
    for i in 0..10 {
        arr = append(arr, i)
    }
    return len(arr)
}
```

#### Checklist

- [ ] Parse array literal `[1, 2, 3]`
- [ ] Add `slice_cap` SSA op
- [ ] Generate `cot_makeslice` runtime
- [ ] Generate `cot_growslice` runtime
- [ ] Lower array literals
- [ ] Lower `append` builtin
- [ ] Implement grow logic with capacity doubling
- [ ] Write 5+ test cases

---

### M22: For-Range Loops

**Goal:** Support `for item in collection { ... }` syntax.

#### Reference: Go's Approach

**Files to study:**
- `/Users/johnc/learning/go/src/cmd/compile/internal/walk/range.go` (lines 100-239)

**Go's range lowering (walk/range.go:162-208):**
```go
// for i, v := range slice
// becomes:
hv1 := 0
hn := len(slice)
for hv1 < hn {
    v := slice[hv1]
    // ... body ...
    hv1++
}
```

#### Cot Implementation Plan

**Syntax:**
```cot
for item in slice { ... }
for i, item in slice { ... }
for i in 0..10 { ... }
```

**Lowering `for item in slice`:**
```zig
fn lowerForRange(self: *Lowerer, loop: *ast.ForRange) !void {
    const collection = self.lowerExpr(loop.collection);
    const len = self.emit(.slice_len, collection);
    const ptr = self.emit(.slice_ptr, collection);

    // Create index variable
    const idx = self.createLocal("__idx", .i64);
    self.emit(.store, idx, self.const_i64(0));

    // Loop header
    const loop_block = self.createBlock();
    const body_block = self.createBlock();
    const exit_block = self.createBlock();

    // Check: idx < len
    self.setCurrentBlock(loop_block);
    const cond = self.emit(.lt, self.emit(.load, idx), len);
    self.emit(.br_if, cond, body_block, exit_block);

    // Body
    self.setCurrentBlock(body_block);

    // Load current element
    const offset = self.emit(.mul, self.emit(.load, idx), elem_size);
    const addr = self.emit(.add_ptr, ptr, offset);
    const item = self.emit(.load, addr);
    self.bindVariable(loop.item_name, item);

    // Lower body statements
    for (loop.body) |stmt| {
        try self.lowerStmt(stmt);
    }

    // Increment index
    const next_idx = self.emit(.add, self.emit(.load, idx), 1);
    self.emit(.store, idx, next_idx);
    self.emit(.br, loop_block);

    self.setCurrentBlock(exit_block);
}
```

#### Tests (Write First)

**File:** `test/cases/loops/for_range_sum.cot`
```cot
// EXPECT: exit_code=15
fn main() i64 {
    let arr = [1, 2, 3, 4, 5]
    var sum: i64 = 0
    for x in arr {
        sum = sum + x
    }
    return sum
}
```

**File:** `test/cases/loops/for_range_index.cot`
```cot
// EXPECT: exit_code=10
fn main() i64 {
    let arr = [10, 20, 30]
    var sum: i64 = 0
    for i, _ in arr {
        sum = sum + i
    }
    return sum  // 0 + 1 + 2 = 3... wait, should be index sum
}
```

**File:** `test/cases/loops/for_range_numeric.cot`
```cot
// EXPECT: exit_code=45
fn main() i64 {
    var sum: i64 = 0
    for i in 0..10 {
        sum = sum + i
    }
    return sum  // 0+1+2+...+9 = 45
}
```

#### Checklist

- [ ] Parse `for item in collection { }` syntax
- [ ] Parse `for i, item in collection { }` with index
- [ ] Parse `for i in start..end { }` numeric range
- [ ] Lower to while loop with index
- [ ] Handle break/continue in for-range
- [ ] Write 5+ test cases

---

## Phase 3: Native Stability

### M23: Debug Native AOT for Complex Programs

**Goal:** Fix hangs in native-compiled complex programs (e.g., fib_small).

#### Investigation Plan

1. **Identify hanging point:**
   - Add debug output in wasm_to_ssa.zig conversion
   - Check if issue is in SSA generation or native codegen

2. **Compare Wasm vs Native:**
   - Run same program through both paths
   - Compare SSA output

3. **Check control flow:**
   - Native codegen may mishandle loops or recursion
   - Verify register allocation for recursive calls

#### Checklist

- [ ] Add debug logging to wasm_to_ssa.zig
- [ ] Identify which function/block causes hang
- [ ] Compare SSA output between Wasm and Native paths
- [ ] Fix control flow issue
- [ ] Test fib_small, fib2 natively
- [ ] Run full test suite natively

---

### M24: Enable All Skipped Native Tests

**Goal:** Un-skip and fix 5 remaining native tests.

#### Skipped Tests

| File | Test | Reason |
|------|------|--------|
| liveness.zig | computeLiveness on simple function | Block count expectations |
| liveness.zig | computeLiveness straight-line code | Block count expectations |
| liveness.zig | computeLiveness with loop | Block count expectations |
| elf.zig | (1 test) | Not tested on macOS |
| macho.zig | (1 test) | Unknown |

#### Checklist

- [ ] Fix liveness.zig test expectations
- [ ] Fix or skip elf.zig on macOS
- [ ] Fix macho.zig test
- [ ] Verify all 5 tests pass
- [ ] Update AOT_EXECUTION_PLAN.md

---

## Summary: Implementation Order

| Phase | Task | Priority | Dependencies | Est. Tests |
|-------|------|----------|--------------|------------|
| 1 | M17: ARC Emission | HIGH | None | 5+ |
| 1 | M18: Heap Allocation | HIGH | M17 | 5+ |
| 1 | M19: Destructors | MEDIUM | M18 | 3+ |
| 2 | M20: String Ops | HIGH | M17-M18 | 5+ |
| 2 | M21: Array/Slice Ops | HIGH | M17-M18 | 5+ |
| 2 | M22: For-Range | MEDIUM | M21 | 5+ |
| 3 | M23: Native Debug | MEDIUM | None | N/A |
| 3 | M24: Native Tests | LOW | M23 | 5 |

**Total new tests to write: ~35+**

---

## File Reference Quick Links

### Go Compiler
| Topic | Path |
|-------|------|
| Escape analysis | `~/learning/go/src/cmd/compile/internal/escape/escape.go` |
| SSA generation | `~/learning/go/src/cmd/compile/internal/ssagen/ssa.go` |
| Walk/lowering | `~/learning/go/src/cmd/compile/internal/walk/` |
| Runtime malloc | `~/learning/go/src/runtime/malloc.go` |
| Runtime strings | `~/learning/go/src/runtime/string.go` |
| Runtime slices | `~/learning/go/src/runtime/slice.go` |

### Swift Compiler
| Topic | Path |
|-------|------|
| ManagedValue | `~/learning/swift/lib/SILGen/ManagedValue.h` |
| Cleanup system | `~/learning/swift/lib/SILGen/Cleanup.h` |
| ARC insertion | `~/learning/swift/lib/SILGen/SILGenExpr.cpp` |
| ARC optimization | `~/learning/swift/lib/SILOptimizer/ARC/` |
| Runtime | `~/learning/swift/stdlib/public/runtime/HeapObject.cpp` |

---

## Next Action

**Start with M17 (ARC Emission):**
1. Read Swift's ManagedValue.h thoroughly
2. Write test cases in `test/cases/arc/`
3. Create `compiler/frontend/arc_insertion.zig`
4. Modify `lower.zig` to emit retain/release
5. Verify tests pass
