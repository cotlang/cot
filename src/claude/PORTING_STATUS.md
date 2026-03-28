# Porting Status: compiler/ → src/libcot-zig/

**Last updated:** 2026-03-28

---

## Completed

| # | File | Lines | Docs | Notes |
|---|------|-------|------|-------|
| 1 | `token.zig` | 581 | `ac/src/libcot-zig/token.md` (210 lines) | Token enum, keyword table, precedence. Zero deps. |
| 2 | `scanner.zig` | 665 | `ac/src/libcot-zig/scanner.md` (429 lines) | Lexer. Depends on token, source, errors. |

---

## Next (safe to port now — no concurrency changes)

Port in this order — each file only depends on files above it:

| # | File | Original Lines | Deps | Status |
|---|------|---------------|------|--------|
| 3 | `source.zig` | ~300 | none | **Ready** — untouched by concurrency |
| 4 | `errors.zig` | ~200 | source | **Ready** — untouched by concurrency |
| 5 | `target.zig` | ~100 | none | **Ready** — untouched by concurrency |
| 6 | `pipeline_debug.zig` | ~150 | none | **Ready** — debug logging utility |
| 7 | `vwt_gen.zig` | ~1,500 | types, lower | **Ready** — untouched, but depends on blocked files |

After source + errors are ported, scanner.zig can actually compile in the new structure.

---

## Blocked (waiting for concurrency work to freeze)

These files are actively being modified by the Swift concurrency port. Do NOT port until concurrency is complete.

| # | File | Original Lines | Concurrency Commits | What's changing |
|---|------|---------------|--------------------|----|
| 8 | `ast.zig` | ~800 | 6 | New AST nodes for actor, async let, Task {} |
| 9 | `parser.zig` | ~2,400 | 7 | Parsing actor, nonisolated, sending, Task {} |
| 10 | `types.zig` | ~2,500 | 4 | Actor type, TaskType, Sendable checking |
| 11 | `checker.zig` | ~5,000 | 8 | Actor isolation, Sendable enforcement, async checking |
| 12 | `lower.zig` | ~13,000 | 18 | State machine transform, actor enqueue, Task creation |
| 13 | `formatter.zig` | ~600 | 3 | Formatting new keywords |

**Concurrency status:** M1-M6 complete (565 tests). Remaining work: ThrowingTaskGroup, AsyncThrowingStream, Clock/Duration, Executor protocols, region analysis. See `claude/CONCURRENCY_SWIFT_PORT.md`.

---

## libcir-zig (IR + Passes + Wasm) — all blocked

These move as a unit after libcot-zig is complete:

| File | Original Lines | Notes |
|------|---------------|-------|
| `ssa_builder.zig` | ~2,900 | Memory threading was added this session |
| `ir/func.zig` | ~500 | |
| `ir/block.zig` | ~300 | |
| `ir/value.zig` | ~400 | |
| `ir/mod.zig` | ~200 | |
| `ssa/func.zig` | ~800 | |
| `ssa/block.zig` | ~400 | |
| `ssa/value.zig` | ~600 | |
| `ssa/op.zig` | ~800 | Op flags changed this session |
| `ssa/passes/*.zig` | ~5,000 | Schedule, deadcode, lower_wasm, etc. |
| `codegen/wasm/*.zig` | ~5,500 | Wasm gen, linker, assembler |

---

## libclif-zig (Native Backend) — port last

68K lines. Port as-is or replace with Rust Cranelift crate. Decision deferred.

---

## Process Reminder

For each file ported:
1. Write cleaned code to `src/libcot-zig/<file>.zig`
2. Write companion docs to `ac/src/libcot-zig/<file>.md`
3. ~200-300 lines per chunk, explain what the code does
4. Remove reference comments, simplify naming, add doc comments
5. See `src/claude/PORTING_RULES.md` for full rules
