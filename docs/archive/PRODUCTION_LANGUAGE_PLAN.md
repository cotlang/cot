# Cot: From Test Harness to Production Language

## Status: Phase 1 Complete (Feb 2026)

Cot now works as a language. A developer can write multi-file programs, import generics from stdlib, produce output, and run tests — all from the CLI.

### What a developer sees today

```bash
$ cat app.cot
import "std/list"

fn main() i64 {
    var nums: List(i64) = .{}
    nums.append(10)
    nums.append(20)
    nums.append(30)
    println(nums.get(0) + nums.get(1) + nums.get(2))
    nums.free()
    return 0
}

$ cot run app.cot
60
```

```bash
$ cat math_test.cot
test "addition" { @assert_eq(2 + 2, 4) }
test "negative" { @assert_eq(-3 + 3, 0) }

$ cot test math_test.cot
2 passed
```

---

## What Was Fixed (Phase 1 — Complete)

### 1A. Cross-file generic imports

**Was:** `checker.zig` stored generic registrations per-instance. Generic types from file A were invisible to file B.

**Fix:** `SharedGenericContext` holds all generic maps shared across checkers. Each checker has its own `expr_types` (to avoid NodeIndex collisions across ASTs). Generic instance symbols are stored in `global_scope`. The lowerer's `lowerGenericFnInstance` does tree-swap + fresh expr_types for cross-file methods.

### 1B. stdlib/list.cot

Extracted from test files into `stdlib/list.cot`. Works via `import "std/list"`.

### 1C. print/println/eprint/eprintln

Implemented as runtime functions with native syscalls. `print` and `println` write to stdout, `eprint` and `eprintln` write to stderr. Integer-to-string conversion happens in generated Wasm runtime code.

### 1D. Stdlib discovery

`import "std/list"` resolves via executable-relative path lookup. Works in both development and installed contexts.

### 2A. CLI subcommands

```bash
cot build app.cot          # compile to ./app
cot build app.cot -o myapp # compile to ./myapp
cot run app.cot            # compile + run + cleanup
cot test app_test.cot      # compile + run inline tests
cot version                # cot 0.3.1 (arm64-macos)
cot help                   # usage info
```

### 2B. Test framework

Inline test blocks with per-test error-union isolation:

```cot
test "name" {
    @assert(condition)
    @assert_eq(actual, expected)
}
```

Summary output: `N passed, M failed`. Wired into `zig build test` for CI. ~730 tests in inline format.

### 2C. LSP server

Basic LSP with diagnostics, hover, goto definition, and document symbols.

---

## What Remains (Phase 2+)

### Collections

| What | Status | Depends On |
|------|--------|------------|
| Map(K,V) | **Done** — open addressing, linear probing, splitmix64 hash, 75% load factor | Hash function, `Hash + Eq` trait impls |
| Set(T) | **Done** — thin wrapper over Map(T, i64), auto-free support | Map(K,V) |

**Reference:** Go `runtime/map.go` (bucket-based hash map), Zig `std/hash_map.zig`

### 2D. Auto scope-exit cleanup

**Was:** Structs with `free()` methods required manual cleanup at every return point.

**Fix:** Compiler detects structs with a `free()` method and inserts scope-exit cleanup calls automatically, like Zig's `defer`. Works with `Map(K,V)` and any user-defined struct with `free()`.

### 2E. Field/method name collision fix

**Was:** `checker.zig:checkFieldAccess()` checked fields before methods. If a struct had field `keys: i64` AND method `fn keys()`, calling `m.keys()` returned the field type (i64), not the method.

**Fix:** `checkCall()` now uses `resolveMethodCall()` to prefer methods in call position. `m.keys` is the field, `m.keys()` calls the method.

### Type System

| What | Status |
|------|--------|
| Trait bounds on generics (`where T: Trait`) | **Done** — working with monomorphized dispatch |
| String interpolation (`"Hello, ${name}"`) | **Done** — `${expr}` syntax, integer auto-conversion |
| String equality (`@assert_eq` for strings) | **Done** — byte-compare via `cot_string_eq` runtime |
| Iterator protocol (`for x in collection`) | **Done** — `for x in collection`, `for i, x in arr`, `for i in start..end` |
| Pattern matching (`match` expressions) | **Done** — wildcards (`_`), guards (`if`), ranges (`1..10`) via `switch` |
| Multiple return values (tuples) | **Done** — tuples with `.0`/`.1` access, SRET for multi-word returns |
| `weak` references (ARC cycle breaker) | Not started |

### I/O and Standard Library

| What | Status |
|------|--------|
| `std/fs` — file I/O | Not started |
| `std/os` — process args, env vars | Not started |
| `std/fmt` — string formatting | Not started |
| `std/math` — math functions | Not started |
| `std/json` — JSON parse/serialize | Not started |
| `std/dom` — browser DOM API | Not started |
| WASI target (`--target=wasm32-wasi`) | Not started |

### Developer Experience

| What | Status |
|------|--------|
| Syntax highlighting (VS Code/Cursor) | **Done** — TextMate grammar + LSP client extension |
| Project manifest (cot.toml) | Not started |
| `cot fmt` — auto-formatter | Not started |
| Error messages with source locations | Partial |

---

## Known Compiler Bugs (Fixed)

| Bug | Status | Fix |
|-----|--------|-----|
| Non-void sibling method calls in generic impl fail on native | **Fixed** | Root cause was stale expr_types during cross-file generic re-checking. Fixed by fresh expr_types per instantiation + global scope for generic instance symbols. |
| `_deinit` suffix hijacked by ARC scanner | **Fixed** | `driver.zig` now uses semantic `is_destructor` flag (set during lowering) instead of scanning function names for `_deinit` suffix. No false positives on generic methods. |
| `sort()` in generic impl produces wrong results on native | **Fixed** | Same root cause as sibling method calls — function pointer calls (`call_indirect`) in generic impl methods now work correctly after the expr_types isolation fix. |

---

## Implementation Order (Remaining)

| Step | What | Unblocks | Effort |
|------|------|----------|--------|
| **1** | ~~Map(K,V) in stdlib~~ | ~~Real applications~~ | **Done** |
| **2** | ~~Set(T) in stdlib~~ | ~~Convenience~~ | **Done** |
| **3** | ~~String interpolation~~ | ~~Readable output~~ | **Done** |
| **4** | ~~Iterator protocol~~ | ~~Ergonomic loops~~ | **Done** |
| **5** | ~~Pattern matching~~ | ~~Exhaustive switch~~ | **Done** |
| **6** | ~~Tuples / multiple return~~ | ~~Multi-value returns~~ | **Done** |
| **7** | `std/fs` — file I/O | Real applications | Medium — syscall wrappers |
| **8** | `std/os` — args, env | CLI tools | Small |

---

## Reference Architecture

| Component | Copy From | Why |
|-----------|-----------|-----|
| Map(K,V) | Go `runtime/map.go` | Bucket-based hash map |
| String interpolation | Zig `std.fmt` | Comptime format parsing |
| File I/O | Go `os/file.go` + WASI `fd_read`/`fd_write` | Cross-target I/O model |
| Iterator protocol | Zig `for` over slices | Simple interface, no allocations |
| Pattern matching | Rust `match` | Exhaustiveness checking |

---

## What Success Looks Like (Next Milestone)

A developer creates a project:

```cot
import "std/list"
import "std/map"

fn main() i64 {
    var scores: Map([]u8, i64) = .{}
    scores.set("alice", 95)
    scores.set("bob", 87)

    var names: List([]u8) = scores.keys()
    var i: i64 = 0
    while i < names.len() {
        println("${names.get(i)}: ${scores.get(names.get(i))}")
        i = i + 1
    }

    names.free()
    scores.free()
    return 0
}
```

```bash
$ cot run app.cot
alice: 95
bob: 87
```

Map(K,V), trait bounds, and string interpolation are now **done**. The next bar is file I/O and iterators.
