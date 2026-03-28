# Porting Status: compiler/ → src/libcot-zig/

**Last updated:** 2026-03-29

---

## Completed (11 files, 9,573 lines, 87 tests)

| # | File | Lines | Tests | Docs | Notes |
|---|------|-------|-------|------|-------|
| 1 | `token.zig` | 565 | 4 | `ac/src/libcot-zig/token.md` | Token enum, keyword table, precedence |
| 2 | `source.zig` | 264 | 9 | `ac/src/libcot-zig/source.md` | Position tracking, lazy line offsets |
| 3 | `errors.zig` | 415 | 7 | `ac/src/libcot-zig/errors.md` | Error/warning reporting with codes |
| 4 | `scanner.zig` | 643 | 11 | `ac/src/libcot-zig/scanner.md` | Lexer with string interpolation |
| 5 | `target.zig` | 153 | 4 | `ac/src/libcot-zig/target.md` | Platform config: Arch, Os |
| 6 | `debug.zig` | 168 | 4 | `ac/src/libcot-zig/debug.md` | Pipeline debug logging. @typeInfo transform |
| 7 | `ast.zig` | 1,594 | 8 | `ac/src/libcot-zig/ast.md` | **Zig-style data-oriented AST.** 125 Tag variants, full.* accessors |
| 8 | `parser.zig` | 3,218 | 18 | `ac/src/libcot-zig/parser.md` | **Zig-style compact output.** Pre-tokenized, scratch buffer, all syntax |
| 9 | `comptime.zig` | 108 | 4 | `ac/src/libcot-zig/comptime.md` | Compile-time value system |
| 10 | `types.zig` | 1,027 | 22 | `ac/src/libcot-zig/types.md` | Type system. TypeIndex as enum(u32), Swift VWT |
| 11 | `formatter.zig` | 1,399 | 0 | — | First consumer of compact AST. Tests need integration |

---

## Next to port — libcot-zig frontend

| # | File | Original Lines | Deps (all met?) | Status |
|---|------|---------------|-----------------|--------|
| 12 | `checker.zig` | 5,162 | ast, types, errors, comptime, target (**all done**) | **Ready** |
| 13 | `ir.zig` | 816 | types, source (**all done**) | **Ready** |
| 14 | `arc_insertion.zig` | 456 | ir, types | Blocked on ir.zig |
| 15 | `lower.zig` | 13,528 | ast, ir, types, checker, comptime, arc_insertion, target | Blocked on checker + ir + arc_insertion |
| 16 | `vwt_gen.zig` | 1,378 | ir, types, source | Blocked on ir.zig |
| 17 | `ssa_builder.zig` | 2,908 | ir, types, source, target | Blocked on ir.zig |

**Critical path:** checker.zig + ir.zig → arc_insertion.zig → lower.zig

---

## libcir-zig (IR + Passes + Wasm) — blocked on libcot-zig completion

| File group | Lines | Notes |
|-----------|-------|-------|
| SSA core (8 files) | ~3,700 | op.zig, value.zig, func.zig, block.zig, dom.zig, debug.zig, html.zig |
| SSA passes (13 files) | ~5,200 | schedule, deadcode, lower_wasm, async_split, cse, etc. |
| CLIF IR (9 files) | ~7,400 | dfg.zig, builder.zig, layout.zig, function.zig, etc. |
| Wasm codegen (12 files) | ~10,800 | Wasm gen, linker, assembler |

---

## libclif-zig (Native Backend) — port last

68K lines. Decision deferred: Zig port vs Rust Cranelift crate.

---

## Architecture alignment check

Per `COT_IR_ARCHITECTURE.md`, the three-library split is:
- **libcot** = frontend (scanner → parser → checker → lowerer) — **11 of ~17 files done**
- **libcir** = IR + passes + Wasm emit — 0% started
- **libclif** = native backend — 0% started

The current work is correctly focused on completing libcot-zig before moving to libcir-zig.

Files NOT in the architecture plan that exist in compiler/:
- `project.zig` (cot.json loading) — belongs in cli-zig, not libcot
- `lsp/*.zig` (16 files) — belongs in libcot-zig/lsp/ per the structure doc
- `driver.zig`, `main.zig`, `cli.zig` — belong in cli-zig

---

## Process

For each file ported:
1. Read compiler/ source, understand every function
2. Write cleaned code to src/libcot-zig/ with transformations applied
3. Write companion docs to ac/src/libcot-zig/
4. Remove reference comments, simplify naming, add /// doc comments
5. See `src/claude/PORTING_RULES.md` for full rules
