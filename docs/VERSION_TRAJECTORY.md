# Cot Version Trajectory — Benchmarked Against Zig

Zig took 10 years and 36+ contributors to reach 0.15. Cot can move faster with LLM-assisted development, but the milestones are real engineering gates that can't be skipped. This document maps Cot's planned trajectory against Zig's actual history.

## Where Zig Was at Each Version

| Zig Version | Date | Key Milestone | Contributors |
|-------------|------|---------------|-------------|
| 0.1.1 | Oct 2017 | First beta (C++ compiler, LLVM backend) | 3 |
| 0.2.0 | Mar 2018 | Error handling (`try`/`catch`), coroutines introduced | 16 |
| 0.3.0 | Sep 2018 | comptime reflection, `@typeInfo`, WebAssembly (experimental) | 36 |
| 0.4.0 | Apr 2019 | `zig cc`, SIMD vectors, bundled libc, Wasm default-enabled | 46 |
| 0.5.0 | Sep 2019 | Async redesign, WASI Tier 2, result location semantics | 67 |
| 0.6.0 | Apr 2020 | Tuples, `@as`, sentinel pointers, ZLS repo created | 122 |
| 0.7.0 | Nov 2020 | ZLS 0.1.0 released (1 week after), macOS cross-compilation | 187 |
| 0.8.0 | Jun 2021 | Self-hosted compiler major push, multiple native backends | 144 |
| 0.9.0 | Dec 2021 | WASI Tier 1, saturating arithmetic | 177 |
| **0.10.0** | **Oct 2022** | **Self-hosted compiler becomes DEFAULT** | 272 |
| **0.11.0** | **Aug 2023** | **Package manager debuts** (`build.zig.zon`) | 269 |
| 0.12.0 | Apr 2024 | x86 backend at 97%, lazy package deps | 268 |
| 0.13.0 | Jun 2024 | LLVM 18, small release | 73 |
| 0.14.0 | Mar 2025 | Incremental compilation, labeled switch | 251 |
| 0.15.0 | Aug 2025 | Self-hosted x86 backend default, async/await REMOVED | 162 |

**Key Zig timelines:**
- First commit → 0.3: **3 years** (Cot: 6 weeks)
- 0.3 → self-hosting (0.10): **4 years**
- 0.3 → package manager (0.11): **5 years**
- First commit → LSP (ZLS): **5 years** (Apr 2020)
- Total: first commit → 0.15: **10 years**

---

## Where Cot Is Now (0.3.1)

**Time from first commit:** ~7 weeks
**Contributors:** 1 + Claude
**Tests:** ~785 Cot language tests + ~163 Zig compiler tests

**Cot 0.3.1 already has things Zig 0.4 didn't:**
- LSP server with 5 features (Zig: ZLS came 14 months after 0.4)
- First-class closures with capture (Zig: still doesn't have this at 0.15)
- String interpolation (Zig: still doesn't have this at 0.15)
- ARC memory management (no manual allocator interface)
- Auto scope-exit cleanup for heap objects
- Traits with monomorphized dispatch (Zig uses comptime duck typing)
- Tuples (Zig: 0.6, 14 months after 0.4)
- Semantic tokens in editor
- Generic `Set(T)` (Zig had `BufSet` only at 0.4)

**Cot 0.3.1 lacks things Zig 0.4 had:**
- comptime (Zig's killer feature — full compile-time execution)
- Async/coroutines with event loops
- ~50-module standard library (math, json, crypto, net, sort, fmt, unicode, etc.)
- String methods (split, trim, indexOf — Zig had these via slice operations)
- 20+ architecture targets (via LLVM)
- Cross-compilation
- Code formatter (`zig fmt`)
- C interop (`@cImport`, `zig cc`, `translate-c`)
- Build system (`build.zig`)
- `errdefer`, named error sets
- Integer overflow detection, debug build mode

See the detailed **Zig 0.4 audit** below for the full feature-by-feature comparison.

---

## Audit: Zig 0.4.0 vs Cot 0.3.1 — Feature-by-Feature Comparison

Zig 0.4.0 (April 2019) represented 18 months from first beta, 46 contributors, 875 commits. It was Zig's "make it real" release — `zig cc`, SIMD vectors, bundled libc, Wasm default-enabled. This section compares every feature area against Cot 0.3.1 (February 2026, 6 weeks, 1 contributor + Claude).

### Legend

- **Cot has it** — feature exists and works on both Wasm and native
- **Partial** — feature exists but with limitations vs Zig's version
- **Missing** — feature doesn't exist in Cot
- **Different by design** — Cot intentionally takes a different approach
- **N/A** — not applicable to Cot's design goals

---

### 1. Type System

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| `i8`-`i64`, `u8`-`u64` | Yes | Yes | **Has it** |
| `i128`, `u128` | Yes | No | Missing |
| Arbitrary bit-width (`i7`, `u23`) | Yes | No | Missing |
| `f32`, `f64` | Yes | Yes | **Has it** |
| `f16`, `f128` | Yes | No | Missing |
| `bool` | Yes | Yes | **Has it** |
| `void` | Yes | Yes | **Has it** |
| `noreturn` | Yes | No | Missing |
| `type` (first-class type values) | Yes | No | **Different** — Cot uses monomorphization |
| `comptime_int` / `comptime_float` | Yes | No | Missing (Cot has integer literals) |
| C ABI types (`c_int`, etc.) | Yes | No | Missing (no C interop) |
| `isize` / `usize` | Yes | No | Missing (Cot uses `i64` everywhere) |

**Gap summary:** Cot covers the common integer/float/bool types. The big miss is `i128`/`u128` and arbitrary bit-width — useful but not urgent for web development. `type` as first-class is fundamental to Zig's comptime; Cot's monomorphization serves the same purpose differently.

### 2. Composite Types

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Fixed-size arrays | Yes | Yes | **Has it** |
| Slices (`[]T`) | Yes | Yes | **Has it** |
| SIMD vectors (`@Vector`) | Yes | No | Missing |
| Structs | Yes | Yes | **Has it** |
| `extern struct` (C ABI layout) | Yes | No | Missing |
| `packed struct` (bit-level) | Yes | No | Missing |
| Enums | Yes | Yes | **Has it** |
| Enums with explicit tag type | Yes | No | Missing |
| `extern enum` / `packed enum` | Yes | No | Missing |
| Tagged unions (`union(enum)`) | Yes | Yes | **Has it** |
| Bare unions (untagged) | Yes | No | Missing |
| `extern union` / `packed union` | Yes | No | Missing |
| Optional types (`?T`) | Yes | Yes | **Has it** |
| Error unions (`E!T`) | Yes | Yes | **Has it** |
| Named error sets (`error{Foo, Bar}`) | Yes | No | **Partial** — Cot has error unions but no named error sets |
| Opaque types | Yes | No | Missing |
| Function pointers | Yes | Yes | **Has it** |
| Tuples | No (0.6) | Yes | **Cot ahead** |
| Closures with capture | No (never) | Yes | **Cot ahead** |
| String interpolation | No (never) | Yes | **Cot ahead** |

**Gap summary:** Core composite types (structs, enums, tagged unions, optionals, error unions) are solid. The `extern`/`packed` variants are C-interop features Cot doesn't need yet. Named error sets would be valuable — they make error handling more expressive.

### 3. Pointer Types

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Single-item pointer (`*T`) | Yes | Yes | **Has it** (via `new`) |
| Many-item pointer (`[*]T`) | Yes | No | Missing |
| Slice (`[]T`, ptr+len) | Yes | Yes | **Has it** |
| C pointer (`[*c]T`) | Yes | No | N/A (no C interop) |
| Optional pointer (`?*T`) | Yes | Partial | Partial |
| `*const T` | Yes | No | Missing (no const pointers) |
| `*volatile T` | Yes | No | Missing |
| Alignment specification | Yes | No | Missing |
| Sentinel-terminated pointers | Yes | No | Missing |
| Address-of (`&`) | Yes | No | **Different** — Cot uses `new` for heap allocation |
| Pointer arithmetic | Yes | Limited | Partial (`@intToPtr`, `@ptrToInt`) |

**Gap summary:** Cot's pointer model is intentionally simpler — ARC-managed heap pointers via `new`, no manual address-of operator. The missing pieces here are mostly relevant to C interop and low-level systems work, not web development.

### 4. Control Flow

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| `if`/`else` expressions | Yes | Yes | **Has it** |
| `if` with optional unwrap | Yes | No | Missing |
| `if` with error unwrap | Yes | No | Missing |
| `switch` expressions | Yes | Yes | **Has it** |
| `switch` exhaustiveness checking | Yes | Partial | Partial (enums only) |
| `switch` with range prongs | Yes | No | Missing |
| `switch` on tagged union with payload | Yes | Yes | **Has it** |
| `while` loops | Yes | Yes | **Has it** |
| `while` with optional capture | Yes | No | Missing |
| `while` with error capture | Yes | No | Missing |
| `for` loops over arrays/slices | Yes | Yes | **Has it** |
| `for` with index capture | Yes | Yes | **Has it** |
| `inline while` / `inline for` | Yes | No | Missing |
| Labeled loops with `break`/`continue` | Yes | Partial | Partial (break only, no labeled continue) |
| Labeled blocks (`blk: { break :blk val }`) | Yes | No | Missing |

**Gap summary:** Basic control flow is solid. The biggest practical gaps are `if` with optional unwrap (very ergonomic in Zig — `if (maybe_val) |val| { ... }`) and labeled blocks. Range switch prongs would also help.

### 5. Functions

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Named parameters + types | Yes | Yes | **Has it** |
| Explicit return types | Yes | Yes | **Has it** |
| Methods (`self` parameter) | Yes | Yes | **Has it** |
| Closures with capture | No | Yes | **Cot ahead** |
| Generic functions | Yes (via comptime) | Yes (monomorphization) | **Has it** (different mechanism) |
| `export` functions | Yes | No | Missing |
| `extern` functions | Yes | No | Missing |
| `inline` functions | Yes | No | Missing |
| `noinline` functions | Yes | No | Missing |
| Naked functions | Yes | No | Missing |
| Variadic functions | Yes | No | Missing |
| Calling conventions | Yes (6 options) | No | Missing |
| Multiple return values | Yes (via anon structs) | No | Missing |
| Functions returning types | Yes | No | **Different** — monomorphization covers this use case |

**Gap summary:** Core function features are solid. `export`/`extern` are needed for C interop (a 0.6+ concern). Multiple return values would be nice. Closures are a genuine lead over Zig.

### 6. Comptime / Compile-Time Evaluation

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| `comptime` keyword | Yes | No | **Different by design** |
| Types as first-class values | Yes | No | **Different** — monomorphization |
| Compile-time function evaluation | Yes (full) | No | Missing |
| `@compileError` | Yes | No | Missing |
| `@compileLog` | Yes | No | Missing |
| `@typeInfo` reflection | Yes | No | Missing |
| `@typeName` | Yes | No | Missing |
| `@sizeOf` | Yes | Yes | **Has it** |
| `@alignOf` | Yes | No | Missing |
| `@memberCount`/`@memberName`/`@memberType` | Yes | No | Missing |
| `@This()` | Yes | No | Missing |
| `@field(obj, "name")` | Yes | No | Missing |
| `comptime { }` blocks | Yes | No | Missing |
| `@setEvalBranchQuota` | Yes | No | Missing |
| `@target_os()` / `@target_arch()` | No (via std.os) | Yes | **Has it** |
| Const-fold if-expressions | Yes | Yes | **Has it** |

**Gap summary:** This is Cot's largest deliberate divergence from Zig. Zig's comptime is its killer feature — full compile-time execution where types are values. Cot's monomorphized generics + const-fold achieves 80% of the practical benefit with 10% of the compiler complexity. For a web-focused language, this is the right trade. More const-eval (evaluating pure functions at compile time) would still be valuable.

### 7. Error Handling

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Error unions (`E!T`) | Yes | Yes | **Has it** |
| `try` (unwrap or propagate) | Yes | Yes | **Has it** |
| `catch` (handle error) | Yes | Yes | **Has it** |
| `catch` with capture (`catch \|err\|`) | Yes | Partial | Partial |
| `defer` | Yes | Yes | **Has it** |
| `errdefer` | Yes | No | Missing |
| Named error sets | Yes | No | Missing |
| Inferred error sets | Yes | No | Missing |
| Error set merging (`\|\|`) | Yes | No | Missing |
| Error return traces | Yes | No | Missing |
| `@errorName` | Yes | No | Missing |

**Gap summary:** Cot has the core error union pattern (`!T`, `try`, `catch`), which handles the common case. `errdefer` would be a high-value addition — it's Zig's most elegant resource cleanup pattern. Named error sets add expressiveness but aren't critical for early adoption.

### 8. Memory Management

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| No garbage collector | Yes | Yes | **Has it** |
| Explicit allocator interface | Yes | No | **Different by design** |
| `ArenaAllocator` | Yes | No | **Different** — ARC handles this |
| `FixedBufferAllocator` | Yes | No | **Different** — ARC handles this |
| ARC (automatic ref counting) | No | Yes | **Cot ahead** |
| `new` heap allocation | No (allocator.create) | Yes | **Cot ahead** — more ergonomic |
| `@alloc` / `@dealloc` / `@realloc` | No (allocator methods) | Yes | **Has it** |
| `@memcpy` / `@memset` | Yes | `@memcpy` only | Partial |
| Auto scope-exit cleanup | No (manual defer) | Yes | **Cot ahead** |
| `defer allocator.free(...)` pattern | Yes | Yes (via `defer`) | **Has it** |
| Destructor integration | No (manual) | Yes (`deinit`, auto) | **Cot ahead** |

**Gap summary:** This is an area where Cot is genuinely ahead for its target audience. ARC + auto cleanup is dramatically more ergonomic than Zig's explicit allocator interface. Web developers don't want to think about allocators. `@memset` should be added for completeness.

### 9. Standard Library

| Module | Zig 0.4 | Cot 0.3.1 | Status |
|--------|---------|-----------|--------|
| ArrayList / dynamic array | Yes | Yes (`List(T)`) | **Has it** |
| HashMap | Yes | Yes (`Map(K,V)`) | **Has it** |
| Set | No (`BufSet` only) | Yes (`Set(T)`) | **Cot ahead** |
| LinkedList | Yes | No | Missing |
| PriorityQueue | Yes | No | Missing |
| Red-Black tree | Yes | No | Missing |
| Sorting | Yes (`std.sort`) | No | Missing |
| Math (trig, log, pow, etc.) | Yes (comprehensive) | `@sqrt` only | **Major gap** |
| String formatting | Yes (`std.fmt`) | String interpolation | **Partial** (interp covers some cases) |
| File I/O | Yes (`std.os.file`) | Yes (`std/fs`) | **Has it** |
| Process args/env | Yes (`std.os`) | Yes (`std/os`) | **Has it** |
| Time | Yes (`std.os.time`) | Yes (`std/time`) | **Has it** |
| Random | Yes (`std.rand`) | Yes (`std/random`) | **Has it** |
| JSON | Yes (`std.json`) | No | Missing |
| Networking (TCP) | Yes (`std.net`) | No | Missing |
| Crypto (SHA, BLAKE2, etc.) | Yes (7+ algorithms) | No | Missing |
| Hash functions (CRC, FNV, etc.) | Yes | `splitmix64` only | **Partial** |
| ASCII / Unicode | Yes | No | Missing |
| Base64 | Yes | No | Missing |
| Atomic operations | Yes (`std.atomic`) | No | Missing |
| Mutex / concurrency primitives | Yes | No | Missing |
| Binary format parsing (ELF, etc.) | Yes | No | N/A |
| Regex | No | No | Both missing |
| Debug / stack traces | Yes (`std.debug`) | No | Missing |
| Testing assertions | Yes (`std.testing`) | Yes (`@assert`, `@assert_eq`) | **Has it** |
| Build system API | Yes (`std.build`) | No | Missing |
| String methods (split, trim, etc.) | Via slices | No | **Major gap** |

**Gap summary:** Cot has the essentials (collections, file I/O, time, random, testing). The biggest gaps that affect day-to-day programming:
1. **String methods** — split, trim, indexOf, contains, startsWith, endsWith. Critical for any real program.
2. **Math** — trig, log, pow, abs, min, max. Required for anything beyond trivial programs.
3. **Sorting** — every program needs to sort things eventually.
4. **JSON** — critical for web development, APIs, configuration files.

### 10. Async / Concurrency

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Stackless coroutines | Yes | No | Missing |
| `async` / `await` | Yes | No | Missing |
| `suspend` / `resume` | Yes | No | Missing |
| Event loop (epoll/kqueue) | Yes | No | Missing |
| Channels | Yes | No | Missing |
| Futures | Yes | No | Missing |
| Async file I/O | Yes | No | Missing |
| Async networking | Yes | No | Missing |
| Threads (`spawnThread`) | Yes | No | Missing |
| Mutex / Spinlock | Yes | No | Missing |
| Atomic operations | Yes | No | Missing |
| `threadlocal` | Yes | No | Missing |

**Gap summary:** Zig had a working (if imperfect) async system at 0.4. This is a large gap, but it's intentionally deferred to Cot 0.5. Zig later removed their async at 0.15 because the design was wrong — Cot's strategy of waiting until 0.5 to design it right is sound.

### 11. Tooling

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| `build` command | Yes | Yes (`cot build`) | **Has it** |
| `run` command | Yes | Yes (`cot run`) | **Has it** |
| `test` command | Yes | Yes (`cot test`) | **Has it** |
| `fmt` (formatter) | Yes | No | **Missing — priority for 0.4** |
| `cc` (C compiler) | Yes | No | N/A (no C interop goal) |
| `translate-c` | Yes | No | N/A |
| Build system (`build.zig`) | Yes | No | Missing |
| LSP server | No (ZLS came at 0.7) | Yes (5 features) | **Cot ahead by 3 versions** |
| Editor extension | VS Code plugin | VS Code + Cursor | **Cot ahead** |
| Semantic tokens | No | Yes | **Cot ahead** |
| Version command | No (added later) | Yes (`cot version`) | **Cot ahead** |
| Help command | No | Yes (`cot help`) | **Cot ahead** |

**Gap summary:** Cot's tooling is strong for its stage. The formatter is the main gap — Zig had `zig fmt` from 0.3, and it's a table-stakes developer experience feature. A build system (`cot.toml` or `build.cot`) would also help but is less urgent.

### 12. Targets & Cross-Compilation

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| x86_64 Linux | Tier 1 | Yes | **Has it** |
| x86_64 macOS | Tier 1 | No | Missing |
| ARM64 macOS | Tier 2 | Yes | **Has it** |
| ARM64 Linux | Tier 2 | No | Missing |
| x86_64 Windows | Tier 1 | No | Missing |
| Wasm32 | Tier 3 | Yes (first-class) | **Has it** |
| WASI | Experimental | Yes (`--target=wasm32-wasi`) | **Has it** |
| Cross-compilation | Yes (any→any) | No (host-only) | Missing |
| Ships libc headers | Yes (40+ variants) | No | N/A |
| i386 | Yes | No | Missing |
| ARM 32-bit | Yes | No | Missing |
| 15+ other architectures | Yes (via LLVM) | No | Missing |

**Gap summary:** Cot has 3 working targets (ARM64 macOS, x86_64 Linux, Wasm32). Zig had ~20+ via LLVM. For web development, Cot's targets cover the common cases. ARM64 Linux and x86_64 macOS would round out the practical set. Cross-compilation would be valuable but is a 0.6 goal.

### 13. C Interoperability

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| `@cImport` | Yes | No | N/A for now |
| `zig cc` | Yes | No | N/A |
| `translate-c` | Yes | No | N/A |
| `extern struct/union/enum` | Yes | No | Missing |
| C calling convention | Yes | No | Missing |
| Header generation | Yes | No | N/A |

**Gap summary:** Cot has zero C interop. This is fine for a web-focused language at 0.3 — JavaScript interop (via Wasm imports) matters more. C FFI becomes important if Cot wants to interface with databases, crypto libraries, etc. Planned for 0.9.

### 14. Safety Features

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Bounds checking | Yes | Yes | **Has it** |
| Integer overflow detection | Yes | No | Missing |
| Wrapping arithmetic ops (`+%`, etc.) | Yes | No | Missing |
| Optional null checking | Yes (compile-time) | Yes | **Has it** |
| Stack traces on panic | Yes | No | Missing |
| Error return traces | Yes | No | Missing |
| `unreachable` | Yes | Yes (`@trap`) | **Has it** |
| Build modes (Debug/Release) | Yes (4 modes) | No (always optimized) | Missing |
| Valgrind integration | Yes | No | N/A |

**Gap summary:** Integer overflow detection would catch real bugs. Build modes (debug vs release) are important for development — debug mode with safety checks + stack traces would make Cot much more pleasant to debug.

### 15. Test System

| Feature | Zig 0.4 | Cot 0.3.1 | Status |
|---------|---------|-----------|--------|
| Inline `test "name" { }` | Yes | Yes | **Has it** |
| Test assertions | Yes (`expect`) | Yes (`@assert`, `@assert_eq`) | **Has it** |
| Test isolation (continue on failure) | Yes | Yes | **Has it** |
| Test summary output | Yes | Yes | **Has it** |
| Build system test integration | Yes (`builder.addTest`) | No | Missing (but `cot test` works) |
| Test filtering by name | Yes | No | Missing |
| Comptime tests | Yes | No | N/A |
| `test` blocks in any file | Yes | Yes | **Has it** |

**Gap summary:** Test systems are quite comparable. Cot's test runner is solid. Test filtering by name would help during development.

---

### Summary Scorecard

| Category | Zig 0.4 Features | Cot Has | Cot Partial | Cot Missing | Cot Ahead |
|----------|:-:|:-:|:-:|:-:|:-:|
| Type system | 12 | 5 | 0 | 7 | 0 |
| Composite types | 19 | 10 | 1 | 6 | 3 |
| Pointers | 11 | 2 | 2 | 6 | 0 |
| Control flow | 15 | 7 | 2 | 6 | 0 |
| Functions | 14 | 4 | 0 | 9 | 1 |
| Comptime | 16 | 2 | 0 | 13 | 1 |
| Error handling | 11 | 3 | 1 | 7 | 0 |
| Memory | 11 | 3 | 1 | 0 | 4 |
| Stdlib | 33 | 8 | 2 | 20 | 1 |
| Async | 12 | 0 | 0 | 12 | 0 |
| Tooling | 12 | 3 | 0 | 2 | 5 |
| Targets | 12 | 3 | 0 | 8 | 0 |
| C interop | 6 | 0 | 0 | 2 | 0 |
| Safety | 9 | 3 | 0 | 5 | 0 |
| Testing | 8 | 5 | 0 | 2 | 0 |
| **Total** | **201** | **58** | **9** | **105** | **15** |

**Cot covers ~33% of Zig 0.4's feature set** (58 has + 9 partial + 15 ahead = 82 out of 201+15). But this number understates Cot's practical capability — many "missing" features are C-interop or systems-programming features that don't apply to Cot's web-focused mission.

**Adjusted for Cot's mission** (excluding C interop, inline assembly, LLVM-specific targets, packed/extern types, and Valgrind):
- Relevant Zig 0.4 features: ~155
- Cot has or exceeds: ~82
- **Mission-adjusted coverage: ~53%**

---

### Priority Gap List: What Matters Most for Cot 0.4

Based on this audit, here are the highest-impact gaps to close, ordered by value to Cot developers:

**Tier 1 — Would unblock real programs:**

| # | Feature | Why | Effort |
|---|---------|-----|--------|
| 1 | String methods (split, trim, indexOf, contains, startsWith, endsWith, replace) | Every real program needs string manipulation | Medium |
| 2 | `std/math` (abs, min, max, pow, floor, ceil, sin, cos, log, exp) | Basic math is table-stakes | Medium |
| 3 | `std/json` (parse + serialize) | Critical for web dev, APIs, config files | Large |
| 4 | `std/sort` (array/list sorting) | Universal need | Small |
| 5 | `cot fmt` (auto-formatter) | Table-stakes DX, Zig had it at 0.3 | Large |
| 6 | `errdefer` | Zig's most elegant cleanup pattern | Small |
| 7 | StringBuilder | Efficient string building (currently only interpolation) | Small |
| 8 | Multiple return values | Common need, Zig uses anon structs | Medium |

**Tier 2 — Would improve developer experience:**

| # | Feature | Why | Effort |
|---|---------|-----|--------|
| 9 | `if (optional) \|val\|` unwrap syntax | Very ergonomic in Zig, common pattern | Medium |
| 10 | Named error sets | More expressive error handling | Medium |
| 11 | `switch` range prongs (`1...10`) | Common pattern matching need | Small |
| 12 | Labeled blocks / labeled continue | Control flow completeness | Small |
| 13 | Debug build mode (safety checks + traces) | Makes development much more pleasant | Large |
| 14 | Integer overflow detection | Catches real bugs | Medium |
| 15 | Test filtering by name | Development productivity | Small |
| 16 | `@memset` | Completeness alongside `@memcpy` | Small |

**Tier 3 — Nice to have, can wait:**

| # | Feature | Why | Effort |
|---|---------|-----|--------|
| 17 | ARM64 Linux / x86_64 macOS targets | Rounds out practical target set | Medium |
| 18 | LinkedList, PriorityQueue | Less common data structures | Medium |
| 19 | `noreturn` type | Type system correctness | Small |
| 20 | ASCII / Unicode utilities | String handling completeness | Medium |

**Intentionally deferred (not for 0.4):**
- Async (0.5), C interop (0.9), package manager (0.6), comptime blocks (0.8), cross-compilation (0.6), build system (0.4-0.5), SIMD (when needed)

---

## Cot's Planned Trajectory

### 0.3 → 0.4: Make It Pleasant to Use

**Zig parallel: 0.3 → 0.4 (6 months)**
Zig added `zig cc`, SIMD, bundled libc. Cot focuses on closing the highest-impact gaps identified in the Zig 0.4 audit above. Goal: raise mission-adjusted coverage from ~53% to ~70%.

**Already done (completed in late 0.3):** `std/fs`, `std/os`, `std/time`, `std/random`, `Set(T)`, string interpolation, comptime target builtins.

| Priority | Feature | Notes |
|:--------:|---------|-------|
| 1 | String methods | split, trim, indexOf, contains, startsWith, endsWith, replace |
| 2 | `std/math` | abs, min, max, pow, floor, ceil, sin, cos, log, exp |
| 3 | `std/json` | Recursive descent parser + serializer |
| 4 | `std/sort` | Array/List sorting with comparator |
| 5 | `cot fmt` | Auto-formatter (Zig had this at 0.3) |
| 6 | `errdefer` | Zig's most elegant cleanup pattern |
| 7 | StringBuilder | Efficient append-based string building |
| 8 | Multiple return values | `fn divmod(a, b: i64) (i64, i64)` |
| 9 | `if (optional) \|val\|` syntax | Zig's ergonomic optional unwrap |
| 10 | `for key, value in map` | Iterator protocol |
| 11 | Named error sets | `error{NotFound, PermissionDenied}` |
| 12 | `switch` range prongs | `1...10 =>` |
| 13 | Improved error messages | Source locations, underlines, suggestions |
| 14 | LSP: autocomplete | The biggest missing IDE feature |
| 15 | Test filtering by name | Development productivity |

### 0.5: Make It Production-Capable

**Zig parallel: 0.5 (async redesign, WASI Tier 2)**
Both languages hit async at 0.5. Cot adds the full-stack web story.

| Feature | Notes |
|---------|-------|
| `async fn` / `await` | Language-level async |
| Native event loop | epoll (Linux), kqueue (macOS) |
| Browser async | JS Promise interop via Wasm |
| `std/net` | TCP/HTTP server and client |
| IR split (`lower_clif.zig`) | Bypass Wasm for async on native (if needed) |
| Web framework prototype | `@server` / `@client` annotations |
| WASI target | `--target=wasm32-wasi` |

### 0.6: Make It Community-Ready

**Zig parallel: 0.6 (tuples, ZLS created)**
Cot already has tuples and LSP. Focus shifts to ecosystem.

| Feature | Notes |
|---------|-------|
| Package manager | `cot add`, `cot remove`, dependency resolution |
| Package registry | cot.land |
| Cross-compilation | Native binary for any target from any host |
| Multi-file module system | Explicit exports, proper namespacing |

### 0.7: Expand the Standard Library

**Zig parallel: 0.7 (macOS cross-compilation, ZLS 0.1.0)**
Cot's LSP is already ahead of where ZLS was at 0.7.

**Already done (completed in 0.3):** `std/random`, `std/time`.

| Feature | Notes |
|---------|-------|
| `std/crypto` | Hash functions, HMAC |
| `std/regex` | Regular expressions |
| `std/dom` | Browser DOM API |
| Cot framework v1 | Full-stack Next.js-style experience |
| Documentation | Language guide, API docs, tutorials |

### 0.8: Optimize the Compiler

**Zig parallel: 0.8 (self-hosted compiler major push)**
Both languages optimize their compilers at 0.8.

| Feature | Notes |
|---------|-------|
| Incremental compilation | Only recompile changed files |
| Compile-time evaluation | Expand const eval toward comptime |
| Compiler performance | Benchmark and optimize hot paths |
| Additional native targets | RISC-V, other ARM variants |
| Debug info | DWARF output for native debugging |

### 0.9: Mature the Ecosystem

**Zig parallel: 0.9 (WASI Tier 1, iOS targeting)**

| Feature | Notes |
|---------|-------|
| WASI Tier 1 | Full WASI support, tested against runtimes |
| Mobile targets | iOS, Android (via Wasm or native) |
| FFI | C interop, JS npm interop |
| Concurrency model | Decision: actors, message passing, or shared memory |
| Benchmark suite | Performance tracking across versions |

### 0.10: Prepare for Self-Hosting

**Zig parallel: 0.10 (self-hosted compiler becomes default)**
Zig achieved self-hosting here. Cot starts the self-hosting work.

| Feature | Notes |
|---------|-------|
| Cot parser in Cot | Rewrite scanner + parser in Cot |
| Cot checker in Cot | Type checking, scope resolution |
| Cot IR in Cot | SSA construction |
| Bootstrap chain | `cot-stage0` (Zig) compiles `cot-stage1` (Cot) |
| Language spec draft | Formal syntax and semantics |

### 0.11: Self-Hosting

**Zig parallel: 0.11 (package manager debuts)**
Zig's package manager came at 0.11. Cot targets self-hosting here — the compiler is written in Cot and compiles itself.

| Feature | Notes |
|---------|-------|
| **Self-hosted compiler** | Cot compiler written in Cot, compiles itself |
| Bootstrap verified | stage0 (Zig) → stage1 (Cot) → stage2 (Cot) produces identical binary |
| Zig dependency eliminated | The Zig codebase becomes bootstrap-only |
| Performance parity | Self-hosted compiler matches or beats Zig-hosted |

**What self-hosting means for Cot:**
- The compiler can evolve in its own language
- New contributors only need to know Cot (not Zig)
- Proves the language is powerful enough for systems programming
- Removes the "toy language" perception

**What self-hosting requires from Cot:**
- File I/O (reading source files)
- String manipulation (parsing, error messages)
- Hash maps (symbol tables, type registries)
- Tree data structures (AST)
- Binary output (Wasm bytecode, ELF/MachO)
- The entire compiler pipeline reimplemented

### 0.12 → 1.0: Polish and Stabilize

| Version | Focus |
|---------|-------|
| 0.12 | Optimize self-hosted compiler, incremental compilation |
| 0.13 | Language spec finalized, syntax frozen |
| 0.14 | Ecosystem maturity, package registry, framework v2 |
| 0.15 | Release candidates, stability testing |
| **1.0** | **Public release — stability commitment, semver** |

---

## Velocity Comparison

| Milestone | Zig (from first commit) | Cot (projected) | Speedup |
|-----------|------------------------|-----------------|---------|
| First beta (0.1) | 2 years | ~2 weeks | ~50x |
| Language real (0.3) | 3 years | ~6 weeks | ~26x |
| LSP | 5 years | 6 weeks | ~43x |
| Package manager | 8 years | ~0.6 (TBD) | — |
| Self-hosting | 7 years | ~0.11 (TBD) | — |
| 1.0 | Not reached (10+ years) | TBD | — |

The early-stage speedup is ~26x — LLM-assisted development compresses the "write boilerplate, port reference implementations" phase dramatically. 6 weeks to reach what took Zig 3 years with 36 contributors. The later milestones (self-hosting, ecosystem) involve design decisions and community building that can't be compressed the same way, but the velocity advantage should remain significant.

---

## Key Differences in Trajectory

### Things Cot Won't Copy from Zig

1. **Zig removed async at 0.15.** Cot should learn from this — design async right the first time (0.5) rather than adding and removing it.
2. **Zig's comptime is its killer feature but adds enormous compiler complexity.** Cot's monomorphized generics + const eval may be the right tradeoff for a language targeting web developers.
3. **Zig took 7 years to self-host.** With LLM assistance and Cot's simpler type system, 0.11 is achievable.
4. **Zig's package manager came very late (0.11, 8 years in).** Cot should have it at 0.6 — modern languages need packages early.

### Things Cot Should Copy from Zig

1. **Version discipline.** Zig doesn't rush to 1.0. Each version represents real progress.
2. **Test infrastructure.** Zig's behavior test suite (1,900+ tests) catches regressions. Cot's 1,600 tests are a good start.
3. **Cross-compilation as a feature.** Zig's ability to target any platform from any platform is a genuine differentiator.
4. **`zig fmt` existed at 0.3.** Cot should have `cot fmt` by 0.4 at the latest.

---

## Tracking Progress

Update this section as versions ship:

| Version | Target Date | Actual Date | Key Deliverable | Status |
|---------|-------------|-------------|-----------------|--------|
| 0.1 | — | Dec 2025 | First working compiler | Done |
| 0.2 | — | Jan 2026 | Generics, closures, native AOT | Done |
| 0.3 | — | Feb 2026 | CLI, LSP, stdlib, traits, tests | Done |
| 0.4 | TBD | — | Formatter, file I/O, better errors | Planned |
| 0.5 | TBD | — | Async, web framework | Planned |
| 0.6 | TBD | — | Package manager, cross-compilation | Planned |
| 0.7 | TBD | — | Expanded stdlib, framework v1 | Planned |
| 0.8 | TBD | — | Compiler optimization | Planned |
| 0.9 | TBD | — | WASI Tier 1, mobile, FFI | Planned |
| 0.10 | TBD | — | Self-hosting preparation | Planned |
| 0.11 | TBD | — | **Self-hosted compiler** | Planned |
| 1.0 | TBD | — | Public release | Planned |
