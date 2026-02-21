# DX Parity: Cot vs Go vs Zig

**Goal:** A native Cot app should feel as polished as Go or Zig to build, run, test, and debug. This document tracks behavior-level gaps — not features, but what actually happens when developers use the toolchain.

---

## Build Command (`cot build` vs `go build` vs `zig build`)

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| No-arg build from project dir | `go build .` uses `go.mod` + `package main` | `zig build` uses `build.zig` | `cot build` uses `cot.json` `main` field | DONE |
| Output location | CWD (`./pkgname`) | `zig-out/bin/name` | CWD (`./name`) | DONE (matches Go) |
| Intermediate .o cleanup | Always cleaned (temp dir in `$TMPDIR`) | `.zig-cache/` persists | Cleaned after successful link | DONE |
| Implicit file detection | Scans dir for `package main` files | Requires `build.zig` wiring | Requires `cot.json` `main` field | DONE |
| Output naming | Last path element of module | Explicit in `build.zig` | Strip dir + `.cot` extension | DONE |
| Build cache | Content-addressed `$GOCACHE` | Content-addressed `.zig-cache/` | None | GAP |
| Incremental compilation | Per-package | Per-file (experimental) | None (full recompile) | GAP |
| Parallel compilation | Automatic, all CPUs | `-j N`, default all CPUs | Single-threaded | GAP |

---

## Run Command (`cot run` vs `go run` vs `zig run`)

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Exit code forwarding | Full | Full | Full | DONE |
| Temp file cleanup | Always (unless `-work`) | `.zig-cache/` persists | Always (`/tmp/cot-run/`) | DONE |
| Signal handling | Prints goroutine trace | SIGSEGV shown with trace | `fatal error: SIGNAME` + pc + addr, exit 2 | PARTIAL (no trace) |
| Run with no file arg | `go run .` scans package | `zig run file.zig` | Uses `cot.json` `main` | DONE |
| Wasm run | `GOOS=js GOARCH=wasm` | `zig run -target wasm32-wasi` | `cot run --target=wasm32` (via wasmtime) | DONE |
| Watch mode | No | `zig build --watch` (0.15+) | `cot run --watch` (300ms poll) | DONE |
| Pass args to program | `go run . -- args` | `zig run file.zig -- args` | `cot run file.cot -- args` | DONE |

---

## Test Runner (`cot test` vs `go test` vs `zig test`)

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Default output on pass | Silent (no `-v`) | `All N tests passed.` | Per-test `ok` lines + summary | DONE (Deno-style) |
| Failure output | Name + message | Name + message + source + trace | Name + `expected:` / `received:` | DONE |
| Test filtering | `-run regexp` | `--test-filter substring` | `--filter substring` | DONE |
| Timing per test | Shown in verbose | Shown | Shown (`ok (2ms)`) | DONE |
| Exit code | 0 pass, 1 fail | 0 pass, 1 fail | N = number of failed tests | DONE |
| Cross-target testing | `GOOS=... go test` | `zig build test -Dtarget=...` | `cot test file.cot --target=wasm32` (via wasmtime) | DONE |
| Test caching | Yes (content-addressed) | No | No | ACCEPTABLE |
| Fail-fast | `-failfast` | No | `--fail-fast` / `-x` | DONE |
| Parallel tests | `-parallel N` | Automatic | Sequential | GAP |
| @assert_eq diagnostics | Custom via `t.Errorf` | `expected N, found M` + source | `expected: N` / `received: N` (ints and strings) | DONE |

---

## Error Messages

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Format | `file:line:col: message` | `file:line:col: error: message` | `file:line:col: error[E300]: message` | DONE |
| Colors | None (always plain) | Auto-detect TTY | Auto-detect TTY | DONE (better than Go) |
| Source line shown | No | Yes, with `^~~~` caret | Yes, with `^~~~` caret | DONE (matches Zig) |
| Error codes | No | No | Yes (`E1xx`-`E4xx`, `W0xx`) | DONE (better than both) |
| Secondary notes | No | `note:` lines with source | `note:` with source + caret (E302) | DONE (matches Zig) |
| Fix suggestions | No | Some (`help:` lines) | No | GAP |
| Error limit | Sorted, limited | All shown | Max 10, then stops | DONE |
| Warning categories | Via `go vet` (separate tool) | Built-in | Built-in (`cot lint`) | DONE |

---

## Runtime Crashes & Diagnostics

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Panic message | `panic: <message>` | `thread N panic: <message>` | `file:line: panic: message` | DONE (matches Go format) |
| Stack trace on crash | Full goroutine trace with `file:line +0xaddr` | Full trace with source lines + carets | pc + fault addr (no trace) | GAP (needs DWARF) |
| Bounds check | Runtime panic with index info | Runtime panic with index info | `file:line: panic: index out of range [N] with length M` | DONE (matches Go) |
| Null deref | Panic with trace | `attempt to use null value` + trace | `fatal error: SIGBUS` + pc + addr | PARTIAL (no trace) |
| Integer overflow | Wraps silently | Panic in debug, wraps in release | Wraps silently | ACCEPTABLE (matches Go) |
| Stack overflow | `goroutine stack exceeds N-byte limit` | SIGSEGV | SIGSEGV | GAP |
| Division by zero | Panic with trace | Panic with trace | Wasm trap | GAP |
| @panic builtin | N/A | `@panic("msg")` | `file:line: panic: message` + exit 2 | DONE (with file:line) |

**Signal handler + bounds checking + @panic all implemented.** Bounds checks emit Go-style `"index out of range [N] with length M"` with file:line. @panic embeds compile-time file:line. All exit with code 2 (Go pattern). Slice bounds use `<=` (Go `OpIsSliceInBounds`), element access uses `<` (Go `OpIsInBounds`).

### What Cot gives today:
```
test.cot:3: panic: index out of range [5] with length 3
```
```
test.cot:2: panic: something went wrong
```
```
fatal error: SIGBUS
pc=0x00000001043d4d84
addr=0x00000001e2eec010
```

**Next steps:** Stack traces (DWARF), division-by-zero panic message.

---

## Cross-Compilation

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Syntax | `GOOS=linux GOARCH=amd64` | `-target x86_64-linux-gnu` | `--target=amd64-linux` | DONE |
| All targets included | Yes (pure Go) | Yes (bundled LLVM) | Via `zig cc` | DONE |
| Wasm target | `GOOS=js GOARCH=wasm` | `-target wasm32-wasi` | `--target=wasm32` | DONE |
| Static linking | Default (no CGO) | Default (musl) | Via `zig cc` | DONE |
| macOS → Linux | Just works | Just works | Just works (via `zig cc`) | DONE |
| Linux → macOS | Just works | Just works | Untested | UNKNOWN |

---

## Project Structure & Imports

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Project manifest | `go.mod` (module path + deps) | `build.zig.zon` (name + deps + hash) | `cot.json` (name + version + main) | DONE (minimal) |
| Project init | `go mod init` | `zig init` | `cot init [name]` | DONE |
| Local imports | Package paths, no relative | `@import("./file.zig")` | `import "file"` (relative to source) | DONE |
| Stdlib imports | Bare: `"fmt"`, `"os"` | `@import("std")` | `import "std/list"` | DONE |
| Package dependencies | `go.mod` + `go get` | `build.zig.zon` + `zig fetch` | None | GAP (deferred to 0.5+) |
| Circular import detection | Compile error | Compile error | `error: circular import detected: a.cot → b.cot → a.cot` | DONE |
| Lockfile | `go.sum` | `build.zig.zon` hash | None | GAP |

---

## Stack & Memory Defaults

| Metric | Go | Zig | Cot | Status |
|--------|-----|-----|-----|--------|
| Main thread stack | 1MB initial, 1GB max (growable) | OS default (8MB macOS/Linux) | 8MB (fixed, in linear memory) | DONE |
| Spawned thread stack | 2KB-8KB initial, 1GB max (growable) | 16MB (fixed) | N/A (single-threaded) | N/A |
| Heap | GC-managed, growable | Allocator-based, growable | ARC, linear memory (~248MB vmctx) | DONE |
| OS process stack | 8MB (OS default) | 8MB (OS default) | 256MB (`-Wl,-stack_size`) | DONE |
| Max heap | Limited by OS | Limited by OS | ~248MB (vmctx linear memory via BSS) | DONE |

**Note:** vmctx is 256MB total (1MB initialized on disk + 255MB BSS zero-fill). Binary size ~1.1MB. Heap has ~248MB usable. For apps needing more, future work: mmap-based heap expansion.

---

## Formatting & Linting

| Behavior | Go | Zig | Cot | Status |
|----------|-----|-----|-----|--------|
| Format tool | `gofmt` / `go fmt` | `zig fmt` | `cot fmt` | DONE |
| Format check (CI) | `gofmt -l` (list unformatted) | `zig fmt --check` | `cot fmt --check` | DONE |
| Format directory | `go fmt ./...` | `zig fmt src/` | `cot fmt dir/` (recursive `.cot` files) | DONE |
| Lint tool | `go vet` (separate) | Built-in warnings | `cot lint` | DONE |
| Lint rules | ~30 vet checks | Compile warnings | 5 warning codes (W001-W005) | GAP (few rules) |

---

## Priority Fix Order

### P0 — Crash diagnostics (blocks real-world adoption)
1. ~~**Signal handler** that catches SIGSEGV/SIGBUS/SIGILL and prints a message instead of silent death~~ **DONE** — `fatal error: SIGNAME` + pc + fault addr, exit code 2. Both ARM64 macOS and x64 Linux. Ported from Go (`signal_unix.go`, `os_darwin.go`, `os_linux.go`).
2. ~~**Bounds checking** with meaningful panic messages~~ **DONE** — `file:line: panic: index out of range [N] with length M`. Go `OpIsInBounds` (strict `<`) for element access, `OpIsSliceInBounds` (`<=`) for slice bounds. Exit code 2.
3. ~~**@panic** should print file:line~~ **DONE** — `file:line: panic: message`. Compile-time embedded source location (Zig pattern). Exit code 2.
4. ~~**DWARF debug info**~~ **DONE** — Function-level DWARF line tables in Mach-O binaries. `dwarfdump` shows file:line per function. `atos -o binary 0xPC` resolves to `funcname (in binary) (file.cot:line)`. Crash diagnostic `pc=0x...` is now actionable. Full per-instruction line tables deferred (requires source position threading through Wasm→CLIF→MachInst pipeline).

### P1 — Build ergonomics
5. ~~**`cot run --target=wasm32`** via wasmtime~~ **DONE** — same wasmtime execution path as `cot test`, builds to `/tmp` then runs.
6. ~~**Circular import error** instead of silent dedup~~ **DONE** — in-progress set tracks recursion stack, prints full cycle path: `a.cot → b.cot → a.cot`.
7. ~~**`cot fmt` on directories**~~ **DONE** — `cot fmt dir/` recursively formats all `.cot` files. `--check` mode works with directories too.

### P1.5 — Test ergonomics
8. ~~**`--fail-fast` / `-x` flag for `cot test`**~~ **DONE** — stops after first failure, prints summary, exits with code 1.

### P2 — Error message quality
9. ~~**`note:` secondary diagnostics**~~ **DONE** — E302 (redefined identifier) now shows `note: previously defined here` with source line + caret (Zig pattern). Infrastructure supports notes on any error.
10. **Fix suggestions** ("help:" lines, e.g., "did you mean X?")
11. ~~**Larger heap**~~ **DONE** — 256MB vmctx (1MB disk + 255MB BSS), binary size reduced from 16MB to ~1.1MB

### P3 — Build performance (matters at scale)
12. **Build cache** (content-addressed, like Go)
13. **Incremental compilation** (recompile only changed files)
14. **Parallel compilation** (multi-file)

---

## References

- Go build internals: `references/go/src/cmd/go/internal/work/build.go`
- Go test runner: `references/go/src/testing/testing.go`
- Go panic/trace: `references/go/src/runtime/panic.go`, `traceback.go`
- Zig build system: `references/` (not in-repo — see ziglang/zig)
- Cot CLI: `compiler/cli.zig`
- Cot build: `compiler/main.zig`
- Cot project: `compiler/project.zig`
- Cot test runtime: `compiler/codegen/test_runtime.zig`
- Cot errors: `compiler/frontend/errors.zig`
- Cot DWARF: `compiler/codegen/native/dwarf.zig`
