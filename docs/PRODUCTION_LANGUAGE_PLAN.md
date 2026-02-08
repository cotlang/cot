# Cot: From Test Harness to Production Language

## The Problem

Cot doesn't work as a language. It works as a test.

Every feature we've built — generics, List(T), traits, closures, ARC — exists only inside `test/native/e2e_all.cot`, a 2,200-line monolithic test file. There is no stdlib. There is no working multi-file compilation for generics. There is no I/O. A user cannot open an IDE, write a Cot program across multiple files, and run it.

The entire language is designed around `zig build test` returning 0. Not around a developer building an application.

### What a developer sees today

```bash
$ cat hello.cot
fn main() i64 {
    print("Hello, world!")
    return 0
}

$ ./zig-out/bin/cot hello.cot -o hello
$ ./hello
# ... silence. print() compiles but does nothing.
```

```bash
$ cat app.cot
import "stdlib/list.cot"

fn main() i64 {
    var list: List(i64) = .{}
    list.append(42)
    return list.get(0)
}

$ ./zig-out/bin/cot app.cot -o app
# error: generic structs don't work across file boundaries
```

### How we got here

1. **Test-first was correct** — building the compiler incrementally with E2E tests caught hundreds of bugs and proved each feature works.

2. **But we never moved beyond test-first.** Every new feature was added to the test file, verified, and declared done. The test file became the de facto stdlib, the language demo, and the only working program.

3. **The import system is half-built.** `driver.zig` has `compileFile` with recursive import parsing, shared scope, shared type registry. But the checker stores generic struct/function/impl registrations per-checker-instance, not in the shared scope. So `import "list.cot"` works for plain structs and functions but **breaks for generics** — which is everything that matters (List(T), Pair(T,U), Box(T), etc.).

4. **There is no I/O.** `print()` and `println()` are recognized by the checker (they type-check) but have no code generation. A Cot program cannot produce output.

5. **stdlib/ is an empty directory.** It was created on Jan 30, waiting for files that never came. Every "stdlib" implementation lives inline in test files.

### What actually exists vs. what's missing

| Works | Doesn't work |
|-------|-------------|
| CLI: `cot file.cot -o out` | Multi-file generics (import + List(T)) |
| Single-file compilation | `print()` / any I/O |
| Wasm + native AOT backends | stdlib (empty directory) |
| Generics, traits, closures, ARC | Real multi-file programs |
| 107 E2E sub-tests passing | Any program besides test harnesses |
| String type + concatenation | String formatting / interpolation |
| `import` for plain types/functions | `import` for generic types |

---

## The Fix: Three Phases

### Phase 1: Make the Language Work (imports + stdlib + I/O)

**Goal:** A developer can write a multi-file Cot program that imports List(T) from stdlib and prints output.

#### 1A. Fix generic imports (compiler change)

**The bug:** `checker.zig` stores `generic_structs`, `generic_functions`, `generic_impl_blocks`, `trait_defs`, and `trait_impls` as per-instance hash maps. In `driver.zig:compileFile`, each file gets its own `Checker` instance. Generic registrations from file A are invisible to file B.

**The fix (two options):**

**Option 1 — Shared registrations (Go pattern).** Move generic/trait maps from `Checker` to a shared `RegistrationContext` that all checkers reference. This is how Go's `cmd/compile` works — type information is global.

```
// New shared context (lives alongside TypeRegistry and Scope):
pub const GenericContext = struct {
    generic_structs: StringHashMap(GenericInfo),
    generic_functions: StringHashMap(GenericInfo),
    generic_impl_blocks: StringHashMap(ArrayListUnmanaged(GenericImplInfo)),
    trait_defs: StringHashMap(TraitDef),
    trait_impls: StringHashMap([]const u8),
    instantiation_cache: StringHashMap(TypeIndex),
    generic_inst_by_name: StringHashMap(GenericInstInfo),
};
```

Create once in `compileFile`, pass to all `Checker.init()` calls. Same pattern as `TypeRegistry` and `Scope` — already shared across checkers.

**Option 2 — Source-level include (pragmatic).** Before parsing, prepend stdlib source to user source. Simple, no compiler changes. But doesn't scale and doesn't solve the real problem.

**Recommendation: Option 1.** It's the same pattern already used for `TypeRegistry` and `Scope`. The change is mechanical — extract maps from `Checker`, put them in a shared struct, pass that struct to `Checker.init()`.

**Files to change:**
- `compiler/frontend/checker.zig` — Extract 7 maps into `GenericContext`, reference it instead of owning
- `compiler/driver.zig` — Create `GenericContext` in `compileFile`, pass to each `Checker`
- `compiler/driver.zig:compileSource` — Create a local `GenericContext` for single-file mode (no behavior change)

**Reference:** Go's `cmd/compile/internal/types2` uses a shared `Config` object across all type-checking phases. Zig's `@import` resolves through a global `Package` registry.

#### 1B. Create stdlib/list.cot

Extract the List(T) implementation from `test/native/e2e_all.cot` into `stdlib/list.cot`. This is the canonical source — not a copy, not a duplicate.

```
stdlib/
  list.cot      — List(T): 35 methods, Go growth, bounds-checked
```

The test file imports it: `import "../../stdlib/list.cot"`

#### 1C. Implement print/println

**The pattern (Go):** In Go, `println` is a builtin that writes to stderr. `fmt.Println` writes to stdout. Both ultimately call `write(fd, buf, len)` — a syscall.

**For Cot native (Wasm → CLIF → ARM64/x64):**

`print(value)` needs to:
1. Convert value to string representation (i64 → decimal digits, string → as-is)
2. Call the platform write syscall

**Implementation path:**
- Add `cot_print_i64(value: i64)` and `cot_print_str(ptr: i64, len: i64)` as runtime functions in `arc.zig`
- For native: these emit a syscall (`write(1, buf, len)` on Linux/macOS)
- For Wasm: these call WASI `fd_write` (or a host import)
- Lower `print()` calls in `lower.zig` to the appropriate runtime function based on argument type
- Wire through `func_indices` in `driver.zig` (same pattern as `@alloc`, `@dealloc`)

**Reference implementations:**
- Go: `runtime/print.go` — `printint()`, `printstring()`, `printnl()` — direct fd writes
- Zig: `std.debug.print` — format + write to stderr via syscall
- Wasm: WASI `fd_write(fd, iovs, iovs_len, nwritten)` — standardized I/O

**Files to change:**
- `compiler/codegen/arc.zig` — Add `generatePrintI64Body`, `generatePrintStrBody` (runtime functions)
- `compiler/frontend/lower.zig` — Lower `print()` calls to runtime function calls
- `compiler/driver.zig` — Register print functions in `func_indices`
- For native: syscall wrappers in the generated code (same as `memory.grow` pattern)

#### 1D. Stdlib discovery

When the compiler runs, it needs to find stdlib files. Pattern from Go's GOROOT discovery:

```
1. Check COT_HOME environment variable
2. Infer from executable location: $(dirname $(which cot))/../stdlib/
3. Check ./stdlib/ relative to cwd (for development)
```

**Implementation:** Add `findStdlibPath()` to `driver.zig`. When resolving `import "std/list"`, prepend the stdlib path.

### Phase 2: Make Development Real (CLI + project structure)

**Goal:** `cot build` and `cot run` work like `go build` and `go run`.

#### 2A. CLI subcommands

Current: `cot file.cot -o out`

Target:
```bash
cot build app.cot          # compile to ./app
cot build app.cot -o myapp # compile to ./myapp
cot run app.cot            # compile + run
cot test app_test.cot      # run tests
```

**Pattern (Zig):** Direct if/else command dispatch in main.zig. No registry framework needed at this scale.

**Files to change:**
- `compiler/main.zig` — Add command dispatch, keep existing single-file path as `build` default

#### 2B. Project manifest

```
// cot.toml or cot.zon
name = "myapp"
version = "0.1.0"
entry = "src/main.cot"
```

Not needed immediately. The compiler already takes a file path. Add when we have packages.

#### 2C. Standard import paths

```cot
import "std/list"          // → $COT_HOME/stdlib/list.cot
import "std/map"           // → $COT_HOME/stdlib/map.cot
import "./utils.cot"       // → relative to current file
import "../shared/types"   // → relative path
```

**Implementation:** Extend `parseFileRecursive` in `driver.zig` to resolve `std/` prefixed imports via the stdlib path.

### Phase 3: Production Collections + I/O

**Goal:** Build real applications.

#### 3A. Map(K, V)

Depends on: working List(T) in stdlib, trait bounds (`where K: Hash + Eq`)

Reference: Go `runtime/map.go` (hash map with buckets), Zig `std.hash_map.zig`

Location: `stdlib/map.cot`

#### 3B. File I/O

```cot
import "std/fs"

fn main() i64 {
    let content = fs.readFile("config.txt")
    // ...
}
```

**Native:** Syscall wrappers (open, read, write, close)
**Wasm:** WASI interface (fd_open, fd_read, fd_write, fd_close)

#### 3C. String formatting

```cot
let msg = fmt("Hello, {}! You have {} items.", name, count)
```

Depends on: working strings, type-aware formatting, variadic args or builder pattern

---

## Implementation Order

| Step | What | Unblocks | Effort |
|------|------|----------|--------|
| **1** | Fix generic imports (shared GenericContext) | Everything | Medium — mechanical refactor |
| **2** | Create stdlib/list.cot, tests import it | Real stdlib | Small — extract existing code |
| **3** | Implement print/println | Any useful program | Medium — runtime functions + syscalls |
| **4** | Stdlib discovery (COT_HOME) | `import "std/list"` | Small |
| **5** | CLI: `cot build`, `cot run` | Developer workflow | Small |
| **6** | Map(K, V) in stdlib | Real applications | Large — needs hash function |
| **7** | File I/O | Real applications | Medium — syscall wrappers |

**Steps 1-3 are the critical path.** After those, a developer can:
```cot
import "std/list"

fn main() i64 {
    var nums: List(i64) = .{}
    nums.append(10)
    nums.append(20)
    nums.append(30)
    println(nums.get(0) + nums.get(1) + nums.get(2))  // prints 60
    nums.free()
    return 0
}
```

Compile: `cot build app.cot`
Run: `./app`
Output: `60`

That's a real language.

---

## Known Compiler Bugs to Fix

| Bug | Impact | Root Cause |
|-----|--------|------------|
| Generic imports don't work across files | Blocks stdlib | `GenericInfo` uses per-checker node_idx that references wrong AST |
| Non-void sibling method calls in generic impl fail on native | Forces inlining workarounds | Unknown codegen issue in monomorphized method dispatch |
| `_deinit` suffix hijacked by ARC scanner | Can't name methods `deinit` | `driver.zig:1081` scans ALL function names, not just struct-level ones |
| `sort()` in generic impl produces wrong results on native | sort() doesn't work | Unknown — selection sort logic is correct, likely codegen issue with `var` mutation in nested loops inside generic impl |

---

## Reference Architecture

| Component | Copy From | Why |
|-----------|-----------|-----|
| Shared type context | Go `cmd/compile/internal/types2.Config` | Already proven pattern in our codebase (TypeRegistry, Scope) |
| Stdlib discovery | Go `cfg.go:SetGOROOT()` | Priority cascade: env → binary-relative → cwd |
| CLI dispatch | Zig `main.zig` | Simple if/else, no framework needed |
| Print runtime | Go `runtime/print.go` | Direct syscall writes, no allocations |
| Import resolution | Go `cmd/compile/internal/importer` | Recursive parse, shared symbol table |
| List(T) | Go `slices/` + Zig `std/array_list.zig` | Already ported, needs to move to stdlib |
| Map(K,V) | Go `runtime/map.go` | Bucket-based hash map |

---

## What Success Looks Like

A developer creates a project:

```
myapp/
  main.cot
  utils.cot
```

```cot
// main.cot
import "std/list"
import "./utils.cot"

fn main() i64 {
    var items: List(i64) = .{}
    items.append(10)
    items.append(20)
    items.append(30)

    let total = sum_list(&items)
    println(total)  // prints 60

    items.sort(cmp_asc)
    println(items.get(0))  // prints 10

    items.free()
    return 0
}
```

```cot
// utils.cot
fn sum_list(list: *List(i64)) i64 {
    var total: i64 = 0
    var i: i64 = 0
    while i < list.len() {
        total = total + list.get(i)
        i = i + 1
    }
    return total
}

fn cmp_asc(a: i64, b: i64) i64 {
    return a - b
}
```

```bash
$ cot build main.cot -o myapp
$ ./myapp
60
10
```

That's the bar. We're not there yet. This document is the plan to get there.
