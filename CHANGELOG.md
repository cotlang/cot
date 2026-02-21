# Changelog

All notable changes to the Cot compiler will be documented in this file.

Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **`@safe` auto-ref**: Structs passed by reference automatically — no `&` needed. `foo(myStruct)` passes the original, mutations visible to caller. TypeScript/C#-style object semantics.
- **Self-hosted parser**: Recursive descent parser (`self/frontend/parser.cot`, ~2,650 lines, 85 tests) — generic structs, generic impl, where clauses, switch range/guard, 53 builtins, quoted identifiers
- CI/CD pipeline with GitHub Actions (test on every commit, release on tag)
- Pre-built binaries for macOS (ARM64, x64) and Linux (x64)
- Curl installer: `curl -fsSL https://raw.githubusercontent.com/cot-land/cot/main/install.sh | sh`

### Fixed
- **`@safe` auto-ref correctness**: Auto-ref now takes address of original local variable instead of creating a temporary copy. Previously, mutations through pointer params didn't propagate back to the caller.
- **Chained pointer field access**: `outer.scanner.pos` where `scanner` is `*Inner` now correctly loads the pointer value before field access (was returning pointer address instead of field value).

## [0.3.2] - 2026-02-21

### Added
- **Builtin cleanup**: ~55 runtime builtins moved from `@` syntax to `extern fn` in `stdlib/sys.cot`
- **`extern fn` support**: Declare external functions that link to Wasm module functions
- **`stdlib/math.cot`**: Math functions (`sqrt`, `abs`, `ceil`, `floor`, etc.) as regular functions wrapping internal builtins
- **`stdlib/sys.cot`**: Low-level system functions (memory, I/O, networking, process) as `extern fn`
- **Consistent camelCase**: All stdlib APIs renamed to camelCase (`fd_write` → `fdWrite`, `args_count` → `argsCount`, etc.)
- **Comptime infrastructure**: `@typeInfo(T)`, `@typeName(T)`, `@enumName(T, idx)`, `comptime {}` blocks with mutable vars, `inline for` over type info
- **Self-hosted compiler progress**: Scanner, token, AST, source, errors in `self/frontend/` (2,054 LOC)
- **@safe mode**: Project-level `cot.json` `"safe": true` with colon struct init, implicit self, field shorthand
- **Walk-up cot.json resolution**: npm-style directory search for project config
- **Single-arg `@enumFromInt`**: `@as(Token, @enumFromInt(val))` pattern (Zig parity)
- **`cot-out/` build directory**: Build artifacts in project-local directory

### Changed
- `@` builtins reduced from ~94 to ~35 compiler intrinsics
- All stdlib modules updated to use `extern fn` from `sys.cot` instead of `@` builtins
- `self/` files restructured to `self/frontend/` mirroring `compiler/frontend/`

### Fixed
- `@safe` var annotation coercion in lowerer
- `comptime_value_vars` leaking across functions in lowerer

## [0.3.1] - 2026-02-11

### Added
- Buffered I/O module (`std/io`): `BufferedReader`, `BufferedWriter`, `readLine`, `readByte`
- MCP server example app (`mcp/cot-mcp.cot`) demonstrating stdlib usage
- WasmGC target (`--target=wasm32-gc`): all structs as GC-managed objects
- JSON parser + encoder (`std/json`): recursive descent parser, StringBuilder-based encoder
- Sort module (`std/sort`): insertion sort + reverse for `List(T)`
- File I/O module (`std/fs`): `File` struct, `openFile`, `createFile`, `readFile`, `writeFile`
- OS module (`std/os`): `exit`, `arg(n)`, `environ(n)`
- Time module (`std/time`): `nanoTimestamp`, `milliTimestamp`, `Timer` struct
- Random module (`std/random`): `fillBytes`, `randomInt`, `randomRange`
- Comptime: `comptime {}` blocks, `@compileError`, const-fold if-expressions, dead branch elimination
- `@target_os()`, `@target_arch()` comptime builtins
- Math builtins: `@abs`, `@min`, `@max`, `@clz`, `@ctz`, `@popcount`
- Math module (`std/math`): integer/float utilities
- String module (`std/string`): ~25 functions + `StringBuilder`
- Wasm 2.0/3.0: `return_call`, `trunc_sat`, `memory.copy`, data count section
- `wasm32-wasi` target with all WASI builtins (file I/O, process, clock, random)
- x64 Linux native backend with syscall overrides for all WASI functions
- `cot test --target=wasm32` runs Wasm tests via wasmtime
- `test/run_all.sh` comprehensive test runner (43 files, ~900 tests)
- Enum switch exhaustiveness checking
- `@safe` file annotation for bounds checking
- Compound assignment operators (`+=`, `-=`, `*=`, etc.)

### Fixed
- ARC `scope_destroy`: use `deinit` not `free` for automatic cleanup
- String/slice compound reassignment (var reassignment losing length)
- x64 codegen: epilogue callee-save restoration, shift/cmov 3-operand, fneg/fabs/fcmp
- Compound return types (string returns on native)
- Struct param decomposition for >16 byte structs
- ARM64 br_table Imm12 limit (>4095 entries)
- x64 br_table SIB R13/RBP base encoding
- Method call compound param decomposition
- Cross-file generic lowering (expr_types isolation, global scope, iterator invalidation)
- vmctx_ext_idx collision with large function counts
- ARM64 CBZ offset encoding off-by-one

## [0.3.0] - 2026-01-27

### Added
- Generics: `List(T)`, `Map(K,V)`, `Set(T)` with pure monomorphization
- Traits: `trait`/`impl Trait for Type` (monomorphized, no vtables)
- Error unions (`E!T`), `try`, `catch` error handling
- Multi-file imports with `import "std/list"` and cross-file generic instantiation
- Native AOT backend: Wasm -> CLIF IR -> regalloc2 -> ARM64/x64 -> executable
- Register allocator (regalloc2 port)
- ARM64 macOS backend
- x64 Linux backend
- ARC memory management (retain/release, unified cleanup stack)
- `new`/`@alloc`/`@dealloc` heap allocation
- `defer` statement
- `print`, `println`, `eprint`, `eprintln` I/O
- `cot build`, `cot run`, `cot test`, `cot version`, `cot help` CLI
- LSP server (`cot lsp`): diagnostics, hover, goto-def, document symbols, semantic tokens
- VS Code/Cursor extension with syntax highlighting + LSP client

[Unreleased]: https://github.com/cot-land/cot/compare/v0.3.2...HEAD
[0.3.2]: https://github.com/cot-land/cot/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/cot-land/cot/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/cot-land/cot/releases/tag/v0.3.0
