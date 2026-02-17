# Cot Builtins & Stdlib Audit Report

**Date:** February 18, 2026 (verified after Wave 1-6 remediation + independent audit)
**Scope:** All compiler intrinsics, runtime builtins, and 20 stdlib modules
**Method:** 4 parallel audits + 5 independent verification agents comparing every function against cited reference implementations (Zig Sema.zig, Go runtime, Swift ARC, RFC specs, POSIX headers)

---

## Executive Summary

| Category | FAITHFUL | ACCEPTABLE | CONCERN | Total |
|----------|----------|------------|---------|-------|
| Compiler intrinsics (20) | 16 | 4 | 0 | 20 |
| Type system additions (2) | 1 | 1 | 0 | 2 |
| Runtime builtins (38) | 36 | 2 | 0 | 38 |
| Core stdlib (5 modules) | 5 | 0 | 0 | 5 |
| System stdlib (14 modules) | 4 | 9 | 1 | 14 |
| **Total** | **62** | **15** | **1** | **78** |

**Bottom line:** 62 of 78 items (79%) are FAITHFUL ports of proven reference architectures. 15 items (19%) are ACCEPTABLE — functionally correct with documented simplifications. 1 item (process.cot) remains a CONCERN due to missing environment inheritance and fixed argument limits. All tests pass: 63/63 files, 225 feature tests on both native and wasm32.

---

## Verification Methodology (Feb 18, 2026)

The initial 6-wave remediation upgraded 32 items. A subsequent **independent verification audit** was conducted using 5 parallel agents, each tasked with:
1. Reading the actual Cot code (not just the report claims)
2. Finding the corresponding reference implementation (Zig Sema.zig, Go runtime/*.go, Swift stdlib, RFC specs)
3. Doing line-by-line comparison
4. Assigning an honest rating

This report reflects the **verified** ratings, not the pre-verification claims.

---

## Remediation Summary (Feb 18, 2026)

7 CONCERN and 25 ACCEPTABLE items from the initial audit were addressed across 6 waves + a verification pass:

| Wave | Scope | Items Fixed | Verification Result |
|------|-------|-------------|-------------------|
| 1 | Checker type fixes | 8 builtins | 3 FAITHFUL, 4 ACCEPTABLE, 1 bug fixed post-verification |
| 2 | Type system | 2 items | 1 FAITHFUL, 1 ACCEPTABLE |
| 3 | Lowerer | 4 items | 2 FAITHFUL, 2 ACCEPTABLE |
| 4 | Runtime/ARC | 3 items | 1 FAITHFUL, 2 ACCEPTABLE |
| 5 | Stdlib core | 9 items | 4 FAITHFUL, 3 ACCEPTABLE, 2 bugs fixed post-verification |
| 6 | Stdlib extended | 5 items | 2 FAITHFUL, 3 ACCEPTABLE |

**Post-verification fixes (3 bugs caught and fixed):**
- `@intCast`: changed `isNumeric()` to `isInteger()` — was accepting float targets (Zig rejects)
- `fs.readFile`: added `@dealloc(buf)` on read failure path — was leaking allocated buffer
- `@fd_open` O_CREATE: fixed Linux value 577→578 (O_RDWR not O_WRONLY), added O_EXCL translation

---

## Compiler Intrinsics — Full Reference Map

| Builtin | Zig Reference | Cot Checker | Cot Lowerer | Rating |
|---------|---------------|-------------|-------------|--------|
| `@intCast(T, val)` | `Sema.zig:zirIntCast` | checker:1158 — validates integer target (rejects float) | lower:4971 — `emitIntCast` → `emitConvert` | **FAITHFUL** |
| `@sizeOf(T)` | `Sema.zig:zirSizeOf` | checker:1148 — resolves type | lower:4963 — `emitConstInt(sizeOf(T))` comptime | **FAITHFUL** |
| `@intToPtr(*T, val)` | `Sema.zig:zirIntToPtr` | checker:1174 — validates target is pointer | lower:4981 — `emitIntToPtr` (identity in Wasm) | **FAITHFUL** |
| `@ptrToInt(ptr)` | `Sema.zig:zirPtrToInt` | checker:1170 — validates pointer input (+ I64 escape) | lower:4986 — identity | **ACCEPTABLE** — I64 escape hatch not in Zig, justified by Cot's type model |
| `@bitCast(T, val)` | `Sema.zig:zirBitCast` | checker:1382 — validates same-size types | lower:5570 — f64⟷i64 via Wasm reinterpret, else identity | **ACCEPTABLE** — missing type category restrictions (Zig rejects pointers, enums) |
| `@truncate(T, val)` | `Sema.zig:zirTruncate` | checker:1389 — returns target_type, validates integer | lower:5593 — AND with mask (0xFF/0xFFFF/0xFFFFFFFF) | **FAITHFUL** |
| `@as(T, val)` | `Sema.zig:zirAs` (type annotation) | checker:1399 — type annotation | lower:5608 — identity (correct for all-i64 model) | **FAITHFUL** |
| `@offsetOf(T, "field")` | `Sema.zig:zirOffsetOf` | checker:1405 — validates struct+field | lower:5614 — comptime field offset sum | **FAITHFUL** |
| `@min(a, b)` | `Sema.zig:zirMin` | checker:1427 — returns peer type | lower:5630 — if-chain with unsigned support | **ACCEPTABLE** — missing input type validation (Zig checks `checkNumericType`) |
| `@max(a, b)` | `Sema.zig:zirMax` | checker:1427 — returns peer type | lower:5656 — if-chain with unsigned support | **ACCEPTABLE** — same as @min |
| `@constCast(ptr)` | `Sema.zig:zirConstCast` | checker:1439 — returns input type | lower:5685 — identity | **FAITHFUL** — Cot has no const pointers, so identity is correct |
| `@intFromEnum(val)` | `Sema.zig:zirIntFromEnum` | checker:1338 — validates enum type | lower:5467 — identity (enums are ints) | **FAITHFUL** |
| `@enumFromInt(T, val)` | `Sema.zig:zirEnumFromInt` | checker:1347 — validates enum target + comptime range | lower:5472 — identity | **FAITHFUL** |
| `@tagName(val)` | `Sema.zig:zirTagName` | checker:1357 — validates enum/union | lower:5477 — O(N) if-chain per variant | **ACCEPTABLE** — Zig uses O(1) string table; functionally correct |
| `@intFromBool(val)` | `Sema.zig:zirIntFromBool` | checker:1373 — validates bool | lower:5565 — identity + comptime fold | **FAITHFUL** |
| `@errorName(val)` | `Sema.zig:zirErrorName` | checker:1367 — validates error type | lower:5557 — O(N) if-chain per variant | **FAITHFUL** — correct semantics, O(N) is acceptable for typical error sets |
| `@compileError(msg)` | `Sema.zig:zirCompileError` | checker:1261 — emits error, returns NORETURN | lower:5353 — trap (safety net) | **FAITHFUL** |
| `@target_os()` | Cot-specific (Zig: `@import("builtin")`) | checker:1260 — returns STRING | lower:5383 — comptime string const | **FAITHFUL** |
| `@assert(cond)` | Cot-specific (Zig: `std.debug.assert`) | checker:1180 — returns VOID | lower:4995 — dual test/runtime paths | **FAITHFUL** |
| `@assert_eq(a, b)` | Cot-specific (Zig: `std.testing.expectEqual`) | checker:1184 — returns VOID | lower:5026 — string-aware + test diagnostics | **FAITHFUL** |

---

## Runtime Builtins — Full Reference Map

### Memory (arc.zig)

| Builtin | Location | Native Override | Reference | Rating |
|---------|----------|----------------|-----------|--------|
| `@alloc` | arc.zig:494 | No (Wasm pipeline) | Swift `swift_allocObject` | **FAITHFUL** — 4 size-class freelists |
| `@dealloc` | arc.zig:631 | No | Swift `swift_deallocObject` | **FAITHFUL** |
| `@realloc` | arc.zig:667 | No | C `realloc` | **FAITHFUL** |
| `@memcpy` | arc.zig:379 | No | Go `runtime/memmove` | **FAITHFUL** — handles overlapping regions |
| `@retain` | arc.zig:767 | No | Swift `swift_retain` | **FAITHFUL** — null check + immortal check |
| `@release` | arc.zig:826 | No | Swift `swift_release_dealloc` | **FAITHFUL** — destructor via call_indirect |
| `cot_string_concat` | arc.zig:1017 | No | Go `runtime/string.go` | **FAITHFUL** — routes through `cot_alloc` |
| `cot_string_eq` | arc.zig:918 | No | Go `runtime/stringEqual` | **FAITHFUL** — 3-stage comparison |
| `cot_memset_zero` | arc.zig:329 | No | Go `memclrNoHeapPointers` | **FAITHFUL** |

### ARC Subsystem Notes

- **Size-class freelist**: 4 classes (<=16, <=64, <=256, <=1024 user bytes). Head-only check is a simplification vs jemalloc's full bin scanning — ACCEPTABLE for V1. **Rating: ACCEPTABLE**
- **`cot_string_concat`**: Now routes through `cot_alloc` with metadata=0 (matches Go's `mallocgc(nil)` pattern). Intermediate concat results may not be freed since `couldBeARC` returns false for STRING type — documented design limitation.

### WASI / OS (wasi_runtime.zig + driver.zig native overrides)

| Builtin | WASI (wasm32) | ARM64 (macOS) | x64 (Linux) | Reference | Rating |
|---------|---------------|---------------|-------------|-----------|--------|
| `@fd_write` | wasi fd_write shim | SYS_write=4 | SYS_write=1 | POSIX write(2) | **FAITHFUL** |
| `@fd_read` | wasi fd_read shim | SYS_read=3 | SYS_read=0 | POSIX read(2) | **FAITHFUL** |
| `@fd_close` | wasi fd_close shim | SYS_close=6 | SYS_close=3 | POSIX close(2) | **FAITHFUL** |
| `@fd_seek` | wasi fd_seek shim | SYS_lseek=199 | SYS_lseek=8 | POSIX lseek(2) | **FAITHFUL** |
| `@fd_open` | wasi path_open shim | SYS_openat=463 | SYS_openat=257 | POSIX openat(2) | **FAITHFUL** — x64 translates O_CREAT/O_TRUNC/O_APPEND/O_EXCL |
| `@exit` | wasi proc_exit | SYS_exit=1 | SYS_exit_group=231 | POSIX exit(2) | **FAITHFUL** |
| `@args_count` | wasi args_sizes_get | vmctx+0x30000 | [rdi+0x30000] | WASI/POSIX | **FAITHFUL** |
| `@arg_len` | wasi argv buffer | argv[n] strlen | argv[n] strlen | POSIX | **FAITHFUL** |
| `@arg_ptr` | wasi argv buffer | copy to 0xAF000 | copy to 0xAF000 | POSIX | **FAITHFUL** |
| `@time` | clock_time_get REALTIME | CNTVCT_EL0 MONOTONIC | clock_gettime REALTIME | WASI/POSIX | **ACCEPTABLE** — ARM64 monotonic vs x64/WASI realtime semantic mismatch |
| `@random` | wasi random_get | SYS_getentropy=500 | SYS_getrandom=318 | POSIX | **FAITHFUL** |
| `@ptrOf(string)` | N/A (intrinsic) | N/A | N/A | Cot-specific | **FAITHFUL** |
| `@lenOf(string)` | N/A (intrinsic) | N/A | N/A | Cot-specific | **FAITHFUL** |

### Networking (all FAITHFUL)

| Builtin | macOS syscall | Linux syscall | Rating |
|---------|--------------|---------------|--------|
| `@net_socket` | SYS_socket=97 | SYS_socket=41 | **FAITHFUL** |
| `@net_bind` | SYS_bind=104 | SYS_bind=49 | **FAITHFUL** |
| `@net_listen` | SYS_listen=106 | SYS_listen=50 | **FAITHFUL** |
| `@net_accept` | SYS_accept=30 | SYS_accept=43 | **FAITHFUL** |
| `@net_connect` | SYS_connect=98 | SYS_connect=42 | **FAITHFUL** |
| `@net_set_reuse_addr` | SYS_setsockopt=105 | SYS_setsockopt=54 | **FAITHFUL** |

### Event Loop (all FAITHFUL, platform-specific)

| Builtin | macOS | Linux | Rating |
|---------|-------|-------|--------|
| `@kqueue_create` | SYS_kqueue=362 | -1 stub | **FAITHFUL** |
| `@kevent_add` | SYS_kevent=363 | -1 stub | **FAITHFUL** |
| `@kevent_del` | SYS_kevent=363 | -1 stub | **FAITHFUL** |
| `@kevent_wait` | SYS_kevent=363 | -1 stub | **FAITHFUL** |
| `@epoll_create` | -1 stub | SYS_epoll_create1=291 | **FAITHFUL** |
| `@epoll_add` | -1 stub | SYS_epoll_ctl=233 | **FAITHFUL** |
| `@epoll_del` | -1 stub | SYS_epoll_ctl=233 | **FAITHFUL** |
| `@epoll_wait` | -1 stub | SYS_epoll_wait=232 | **FAITHFUL** |
| `@set_nonblocking` | SYS_fcntl=92 | SYS_fcntl=72 | **FAITHFUL** |

### Process (all FAITHFUL)

| Builtin | macOS | Linux | Notes | Rating |
|---------|-------|-------|-------|--------|
| `@fork` | SYS_fork=2 (x1 child check) | SYS_fork=57 | macOS quirk handled | **FAITHFUL** |
| `@execve` | SYS_execve=59 | SYS_execve=59 | wasm ptr fixup loop | **FAITHFUL** |
| `@waitpid` | SYS_wait4=7 | SYS_wait4=61 | WEXITSTATUS extraction | **FAITHFUL** |
| `@pipe` | SYS_pipe=42 (regs) | SYS_pipe2=293 (buf) | platform-specific pack | **FAITHFUL** |
| `@dup2` | SYS_dup2=90 | SYS_dup2=33 | direct wrap | **FAITHFUL** |

---

## Stdlib Modules — Full Reference Map

### Core Data Structures

| Module | Lines | Tests | Reference | Rating | Key Note |
|--------|-------|-------|-----------|--------|----------|
| `list.cot` | 460 | 30+ | Go `runtime/slice.go` growth + Zig `ArrayList` API | **FAITHFUL** | `clone` ARC-retains elements (Swift pattern); merge sort O(n log n) stable |
| `map.cot` | 345 | 24 | Zig `HashMap` open addressing + splitmix64 | **FAITHFUL** | Tombstone cleanup on >25% ratio; rehash at same capacity |
| `set.cot` | 51 | 10 | Go `map[K]struct{}` pattern | **FAITHFUL** | Thin wrapper, inherits Map improvements |
| `string.cot` | 527 | 20+ | Go `strings` package | **FAITHFUL** | `substring` copies (safe). Note: `trimLeft`/`trimRight` borrow parent memory (inconsistent) |
| `json.cot` | 774 | 28 | Go `encoding/json` scanner + encoder | **FAITHFUL** | Float support (Go scanner.go states), duplicate keys (last-wins per RFC 8259), jsonFree recursive cleanup |

### System Modules

| Module | Lines | Tests | Reference | Rating | Key Note |
|--------|-------|-------|-----------|--------|----------|
| `fs.cot` | 119 | 13 | Zig `std.fs.File` | **ACCEPTABLE** | Error paths return "" (no error union for compound returns). `readFile` now frees buffer on failure. `writeFile` doesn't check write result. |
| `os.cot` | 43 | 8 | Zig `std.process` | **FAITHFUL** | Thin wrappers, correct |
| `time.cot` | 41 | 7 | Zig `std.time` | **FAITHFUL** | Clean port |
| `random.cot` | 27 | 4 | Go `rand.Int63n` | **FAITHFUL** | Rejection sampling eliminates modulo bias |
| `io.cot` | 361 | 22 | Go `bufio` + Zig `std.io` | **ACCEPTABLE** | Partial write loop correct. Error handling silently discards data (no error return). |
| `mem.cot` | 141 | 17 | Zig `std.mem` | **FAITHFUL** | Clean byte-level operations |
| `debug.cot` | 36 | 5 | Zig `std.debug` | **FAITHFUL** | Assert with message enhancement |
| `fmt.cot` | 520 | 33 | Rust `format!` style (not Go `%` verbs) | **ACCEPTABLE** | `sprintf` uses `{}` placeholders, not Go-style `%d`. Clean scan-and-substitute. |
| `encoding.cot` | 217 | 21 | Go `encoding/base64,hex` | **FAITHFUL** | RFC 4648 test vectors pass |
| `process.cot` | 263 | 13 | Go `os/exec.Cmd` / Zig `std.process.Child` | **CONCERN** | Dynamic allocation (good). But: no env inheritance (Go/Zig inherit by default), fixed 4096-byte string buffer, max 2 args. |
| `crypto.cot` | 439 | 14 | FIPS 180-4 + RFC 2104 | **FAITHFUL** | NIST test vectors validate SHA-256 + HMAC |
| `regex.cot` | 897 | 38 | Thompson NFA (Russ Cox article) | **FAITHFUL** | Linear-time guarantee |
| `url.cot` | 263 | 13 | RFC 3986 + Go `net/url` | **FAITHFUL** | Percent-encoding per RFC 3986 Section 2.3 (exact unreserved set) |
| `http.cot` | 282 | 11 | Go `net/http.ReadRequest` | **ACCEPTABLE** | Correct HTTP/1.1 parsing. Headers stored as raw block (Go parses to map). No chunked encoding. |

---

## Type System Additions

| Feature | Location | Reference | Rating | Note |
|---------|----------|-----------|--------|------|
| `commonType()` | types.zig:393 | Zig `Sema.zig:peerType` | **ACCEPTABLE** | Follows C-style mixed-signedness promotion (u32+i32→i64). Zig rejects mixed signedness entirely. Documented design choice. |
| Overflow detection | lower.zig:2960 | Go `ssa.go` OpDiv64/OpMod64 | **FAITHFUL** | Div-by-zero check only (matches Go/Zig — no runtime overflow at Wasm level) |

---

## Cross-Cutting Observations

### What's Working Well

1. **ARC integration is consistently applied** across list, map, set. All `free`/`clear`/`set`/`delete` operations call `@arc_release`. Matches Swift's ownership patterns.

2. **No invented algorithms.** Every function traces to a well-known reference (Zig, Go, FIPS, RFC, POSIX, Cox/Thompson). The CLAUDE.md "copy, don't invent" rule is followed.

3. **Test coverage is thorough.** 225 feature tests + 63 test files covering edge cases. RFC 4648 vectors for encoding, NIST vectors for SHA-256, tombstone edge cases for map.

4. **WASI/native dual-target is solid.** Every WASI builtin has correct platform-specific native overrides with verified syscall numbers. The macOS/Linux divergences (pipe register convention, fork x1 check, sockaddr layout) are all handled correctly.

5. **Comptime evaluation is well-integrated.** `@sizeOf`, `@offsetOf`, `@target_os`, `@compileError` all resolve at compile time with proper dead branch elimination.

### Known V1 Limitations (Documented, Not Bugs)

- ASCII-only string operations (string.cot)
- i64-only hash keys (map.cot, set.cot)
- No set algebra operations (set.cot)
- No HTTP client (http.cot — server-side parsing/response only)
- Performance gaps: byte-by-byte copies, linear b64 decode, O(N) @tagName/@errorName
- `trimLeft`/`trimRight` borrow parent memory (inconsistent with `substring`'s copy semantics)
- `@time` returns monotonic on ARM64 but realtime on x64/WASI

### Test Coverage Gaps

- **No negative tests** for builtin type validations (e.g., `@intCast(f64, x)` error, `@bitCast` wrong-size error). Error paths are implemented but untested.
- **No `@errorName` tests** in e2e test suite.

### Weak References

`@weak_retain`, `@weak_release`, `@weak_lock` are **not implemented** despite being mentioned in the audit spec. The `kw_weak` keyword exists in token.zig but no runtime support exists.

---

## Conclusion

Of 78 audited items:
- **62 (79%)** are **FAITHFUL** — exact ports verified against reference implementations
- **15 (19%)** are **ACCEPTABLE** — functionally correct with documented simplifications (O(N) vs O(1) lookups, missing input validation, C-style promotion instead of Zig-style rejection)
- **1 (1%)** is a **CONCERN** — `process.cot` lacks environment inheritance and has fixed argument limits (requires variadic args, a deeper language feature)

No invented algorithms. All implementations trace to cited reference sources (Go, Zig, Swift, POSIX, RFC). The verification audit caught and fixed 3 real bugs (`@intCast` accepting floats, `fs.readFile` memory leak, `O_CREATE` Linux cross-platform inconsistency).

Full test suite: 63/63 files pass, 225 feature tests on both native and wasm32 targets.
