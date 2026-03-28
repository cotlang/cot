# Porting Status: compiler/ → src/libcot-zig/

**Last updated:** 2026-03-28

---

## Completed

| # | File | Lines | Docs | Notes |
|---|------|-------|------|-------|
| 1 | `token.zig` | 581 | `ac/src/libcot-zig/token.md` (210 lines) | Token enum, keyword table, precedence. Zero deps. |
| 2 | `source.zig` | 264 | `ac/src/libcot-zig/source.md` | Position tracking, lazy line offset computation. |
| 3 | `errors.zig` | 352 | `ac/src/libcot-zig/errors.md` | Error/warning reporting with codes and source context. |
| 4 | `target.zig` | 143 | `ac/src/libcot-zig/target.md` | Platform config: Arch, Os, CLI target parsing. |
| 5 | `debug.zig` | 157 | `ac/src/libcot-zig/debug.md` | Pipeline debug logging. `@typeInfo` reflection transform. |
| 6 | `scanner.zig` | 665 | `ac/src/libcot-zig/scanner.md` (429 lines) | Lexer. Depends on token, source, errors. |
| 7 | `ast.zig` | 830 | `ac/src/libcot-zig/ast.md` | AST nodes. Comptime `snakeToCamel` + `@typeInfo` map for BuiltinKind. |

---

## Next (safe to port now — no concurrency changes)

All leaf files are ported. Next candidates depend on blocked files:

| # | File | Original Lines | Deps | Status |
|---|------|---------------|------|--------|
| 8 | `vwt_gen.zig` | ~1,500 | types, lower | **Blocked** — depends on types.zig and lower.zig |

All other ready files have been completed (source, errors, target, debug, ast).

---

## Ready to port (concurrency complete — 618 tests, 46/50 Swift parity)

These files are now stable. See `src/claude/CONCURRENCY.md` for full status.

| # | File | Original Lines | Deps | Notes |
|---|------|---------------|------|-------|
| 9 | `parser.zig` | ~2,400 | token, scanner, ast, source, errors | Core parser |
| 10 | `types.zig` | ~2,500 | ast | Type system definitions |
| 11 | `checker.zig` | ~5,000 | ast, types, errors | Type checking + method resolution |
| 12 | `lower.zig` | ~13,000 | ast, types, ssa | IR lowering (will need splitting) |
| 13 | `formatter.zig` | ~600 | ast, token | Code formatter |

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
