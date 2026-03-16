# ARC Use-After-Free Fix Plan

**Date:** 2026-03-16
**Bug:** Multi-file selfcot compilation crashes with use-after-free (confirmed via MallocScribble=1)
**Impact:** ir.cot and all larger files fail to compile

---

## Root Cause

The selfcot stores managed pointers (`*Ast`, `*Checker`, `*GenericInfo`, `*GenericInstInfo`) as raw integers via `@ptrToInt` in Maps. When locals holding these managed pointers go out of scope, ARC decrements the refcount. If refcount hits 0, the object is freed. But the raw integer in the Map still points to freed memory.

### The @ptrToInt fix (commit 81f65b8)

Implemented Swift's `forwardCleanup` pattern: when `@ptrToInt(local)` is called on a managed pointer local, the cleanup for that local is disabled via `disableForLocal()`. This prevents ARC from releasing the object at scope exit.

**Limitation:** This only works for `Expr.ident` (direct local variables). It does NOT work for:
- Field access: `@ptrToInt(self.ast)` — `self.ast` is not a local
- Nested expressions: `@ptrToInt(foo.bar)` — not a simple ident

### Remaining crash (confirmed via MallocScribble)

`MallocScribble=1` fills freed memory with `0x55`. Crash: `ast.cot:46: trap` with List count = `0x5555555555555555`. This means an `*Ast` was freed and its `nodes: List(Node)` has scribbled memory.

The freed `*Ast` is from a file processed in an earlier iteration. When file 5 (source.cot) is lowered, it accesses generic info that references the AST of file 1 (list.cot) or file 2 (map.cot). If that AST was freed, the access crashes.

---

## All @ptrToInt Sites in selfcot That Need Protection

### checker.cot — GenericInfo allocation (lines 513-516, 681-684)
```cot
var info = new GenericInfo { node_idx: idx, ast_ptr: @ptrToInt(self.ast) }
retain(@ptrToInt(self.ast))   // AST escapes — manual retain
retain(@ptrToInt(info))       // GenericInfo escapes — manual retain
@intToPtr(*Map(string, int), self.generic_structs).set(name, @ptrToInt(info))
```
**Status:** Manual `retain()` present. The `@ptrToInt(info)` on `info` (local) is handled by the new cleanup forwarding. But `@ptrToInt(self.ast)` on `self.ast` (field) is NOT — fields don't have scope cleanups.
**Risk:** LOW — `self.ast` is a field of the Checker struct. Fields aren't released at scope exit. The manual `retain` keeps the AST alive.

### checker.cot — GenericInstInfo allocation (lines 4098-4112)
```cot
var inst = new GenericInstInfo { concrete_name: name_copy, ... ast_ptr: ast_ptr }
retain(@ptrToInt(inst))
retain(@ptrOf(name_copy))
retain(@ptrOf(ta_copy))
retain(@ptrOf(tp_copy))
@intToPtr(*Map(string, int), self.generic_inst_by_name).set(name_copy, @ptrToInt(inst))
```
**Status:** Manual `retain()` present. `@ptrToInt(inst)` on `inst` (local) handled by cleanup forwarding. Strings retained manually.
**Risk:** LOW — retains are in place.

### checker.cot — Generic impl entries (lines 720-726)
```cot
retain(@ptrToInt(self.ast))
...
ast_ptr: @ptrToInt(self.ast),
```
**Status:** Manual `retain()` present for `self.ast`.
**Risk:** LOW.

### checker.cot — Symbol source_tree (line 401)
```cot
s.source_tree = @ptrToInt(self.ast)
```
**Status:** NO retain. The AST pointer is stored in a Symbol as `source_tree`. No manual retain.
**Risk:** MEDIUM — if the Symbol outlives the AST, the source_tree is dangling. But Symbols are in the checker's scope which should outlive the AST.

### main.cot — checkAndStoreChecker (lines 997-1006)
```cot
const chk_heap = alloc(0, @sizeOf(Checker))
copyBytes(chk_heap, @ptrToInt(&checker), @sizeOf(Checker))
zeroBytes(@ptrToInt(&checker), @sizeOf(Checker))
```
**Status:** Stack checker zeroed after copy to prevent ARC from freeing shared data.
**Risk:** HIGH — the `zeroBytes` must zero ALL managed pointer fields. If `@sizeOf(Checker)` is wrong or the struct layout has padding, some fields may survive zeroing. Also: the ErrorReporter is zeroed but its `*Source` field might be a managed pointer.

### main.cot — parsed_files AST pointers (line 960)
```cot
parsed_files.append(ParsedFile { path: file_path, content: content, ast: ast })
```
**Status:** `ast` is `*Ast` from `Ast.initHeap()`. When `parseFileRecursive` returns, the local `ast` goes out of scope. ARC releases it (refcount 1→0). The `parsed_files` List has a copy of the pointer but `couldBeARC(ParsedFile)` is false for structs, so the List doesn't retain it.
**Risk:** CRITICAL — This is the most likely source of the use-after-free. The `*Ast` in ParsedFile is a dangling pointer after `parseFileRecursive` returns.

---

## The Fix

### Fix 1: Retain AST in parseFileRecursive (CRITICAL)

The `*Ast` returned by `Ast.initHeap()` has refcount 1. When `parseFileRecursive` returns, ARC decrements it to 0 and frees the AST. But `parsed_files` still holds the pointer.

**Fix:** Disable the cleanup for `ast` before the function returns, OR manually retain it.

Since `ast` is a local variable and `parsed_files.append(ParsedFile{..., ast: ast})` is the last use, the cleanup forwarding from `@ptrToInt` won't help (we're not calling `@ptrToInt`). Instead, we need to either:

a) Call `retain(@ptrToInt(ast))` before appending to parsed_files
b) OR: the compiler should recognize that a managed pointer stored in a struct literal that's appended to a List needs a retain

Option (a) is the immediate fix:

```cot
// In parseFileRecursive, before parsed_files.append:
retain(@ptrToInt(ast))  // Keep AST alive past this function's scope
parsed_files.append(ParsedFile { path: file_path, content: content, ast: ast })
```

### Fix 2: Retain AST in checkAndStoreChecker's checked_scopes (line 984)

```cot
checked_scopes.set(pf.path, CheckedScopeEntry {
    scope_map_ptr: checker.getFileScopeMapPtr(),
    ast_ptr: @ptrToInt(pf.ast),
})
```

The `pf.ast` is accessed via `@ptrToInt` — if `pf.ast` is already freed (from Fix 1 not being in place), this is a dangling access. With Fix 1, the AST survives, so `ast_ptr` is valid.

### Fix 3: Verify zeroBytes covers entire Checker struct

Add a runtime check: `@assertEq(@sizeOf(Checker), 360)` or similar to ensure the size hasn't changed.

---

## Implementation Order

1. Fix 1: Add `retain(@ptrToInt(ast))` in parseFileRecursive
2. Test: Does MallocScribble=1 still crash?
3. Fix 2: Verify checked_scopes ast_ptr is valid
4. Test: Does ir.cot compile?
5. Test all 12 frontend files
