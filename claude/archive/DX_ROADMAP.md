# Cot Developer Experience Roadmap

**Purpose:** This document defines the parallel DX workstream — tooling that makes Cot pleasant to write and to work on with AI assistance. It is designed for a dedicated Claude session to execute independently of the language features workstream.

**Context:** The project architect wants to learn the compiler and hand-write Cot for self-hosting. This requires production-quality DX: an LSP that doesn't lie, an MCP that gives Claude deep compiler context, and an editor experience that makes writing Cot feel as polished as writing TypeScript.

---

## Two Parallel Workstreams

| Workstream | Owner | Focus | Key Files |
|-----------|-------|-------|-----------|
| **Language** | Claude session A | Compiler features, codegen, stdlib, Wasm | `compiler/frontend/`, `compiler/ssa/`, `compiler/codegen/`, `stdlib/` |
| **DX** | Claude session B | LSP, MCP, editor extension, CLI polish | `compiler/lsp/`, `compiler/cli.zig`, `editors/vscode/` |

**Shared boundary:** Both workstreams touch `compiler/frontend/checker.zig` (type info) and `compiler/main.zig` (CLI dispatch). Coordinate via git — DX reads from checker, never modifies it.

---

## Current State

### LSP (compiler/lsp/ — 15 files, ~2,800 lines)

| Feature | Quality | Notes |
|---------|---------|-------|
| Diagnostics | Production | Full error reporting with codes |
| Semantic tokens | Production | 17 token types, 5 modifiers |
| Document symbols | Production | Hierarchical tree (structs → fields, impls → methods) |
| Hover | **Production** | Full fn signatures (param types + return type), struct field expansion, enum variants, method lists |
| Completion (general) | **Production** | Scope symbols + keywords, prefix filtering, type detail in completion items |
| Completion (dot) | **Production** | After `.`: struct fields with types, methods with signatures, enum variants |
| Completion (snippets) | **Production** | fn, test, if, while, for, struct, impl, switch, import |
| Signature help | **Production** | Parameter hints on `(` and `,`, active parameter tracking, method-aware |
| Formatting | **Production** | `cot fmt` wired via `textDocument/formatting` |
| Go-to-definition | MVP | **Single-file only** — no cross-file navigation |
| References | MVP | **Single-file only** — string matching, not semantic |
| Rename | MVP | **Single-file only** — no validation |
| Code actions | Missing | — |
| Workspace symbols | Missing | — |

### MCP (compiler/lsp/mcp_server.zig — Zig-based, 10 tools)

Compiler-integrated MCP server (`cot mcp`), replacing the old Cot-written version:
- **Static:** `get_syntax_reference`, `get_stdlib_docs` (with module param), `get_project_info`
- **Dynamic:** `check_file`, `list_symbols`, `get_type`, `find_definition`, `find_references`, `build`, `run_tests`
- **Resources:** `cot://syntax-reference`, `cot://project-info`, `cot://stdlib/{module}`
- **Prompts:** `debug-error`, `add-test`, `explain-code`
- **Annotations:** All tools have `readOnlyHint`/`destructiveHint`/`idempotentHint`/`openWorldHint`
- Protocol: MCP 2024-11-05, JSON-RPC 2.0 over stdio, newline-delimited
- Config: `.mcp.json` points to `cot mcp`

The old Cot-written MCP (`mcp/cot-mcp.cot`) remains as a stdlib dogfooding example.

### VS Code Extension (editors/vscode/ — 4 files, ~363 lines)

- TextMate grammar (syntax highlighting)
- LSP client (spawns `cot lsp`)
- Language configuration (brackets, comments, folding)
- No snippets, no tasks, no debugger

---

## DX Roadmap: Four Tiers

### Tier 1: Foundation — Make the LSP Trustworthy

**Goal:** A human writing Cot in Cursor can trust what the LSP tells them. No wrong hover info, no missed errors, no stale state.

**Priority:** Highest — blocks the architect's self-hosting work.

| # | Task | Files | Status | Notes |
|---|------|-------|--------|-------|
| 1.1 | Wire `cot fmt` to LSP formatting | `server.zig` | **DONE** | `textDocument/formatting` handler calls existing formatter |
| 1.2 | Dot-completion for fields and methods | `completion.zig` | **DONE** | After `.`, resolves type → offers struct fields + impl methods |
| 1.3 | Signature help (parameter hints) | `signature_help.zig` | **DONE** | Shows `fn name(param1: Type, param2: Type)` while typing args |
| 1.4 | Hover: show struct fields, function signatures | `hover.zig` | **DONE** | Full fn signatures with param types + return type, struct field expansion, method lists |
| 1.5 | Completion: filter by prefix as user types | `completion.zig` | **DONE** | Case-insensitive prefix filtering |
| 1.6 | Completion: add snippet templates | `completion.zig` | **DONE** | fn, test, if, if-else, while, for, struct, impl, switch, import |

### Tier 2: Cross-File Intelligence

**Goal:** Go-to-definition and references work across imports. This is the single biggest gap vs production LSPs.

| # | Task | Files | Effort | Notes |
|---|------|-------|--------|-------|
| 2.1 | Multi-file document store | `document_store.zig`, `analysis.zig` | 1-2 days | Track imported files, analyze dependency graph |
| 2.2 | Cross-file go-to-definition | `goto.zig` | 1 day | Follow `import "std/list"` → resolve to stdlib file → find symbol |
| 2.3 | Cross-file references | `references.zig` | 1 day | Search all open/imported files for symbol usage |
| 2.4 | Cross-file rename | `server.zig` | Hours | Build WorkspaceEdit spanning multiple files |
| 2.5 | Workspace symbols | New: `workspace_symbol.zig` | 1 day | Search all project symbols by name |
| 2.6 | Import completion | `completion.zig` | Hours | After `import "`, suggest available stdlib modules and local files |

### Tier 3: Zig-Based MCP Server

**Goal:** Replace the Cot-written MCP with a compiler-integrated Zig MCP (`cot mcp`). Give Claude deep compiler context for writing Cot code.

**Spec:** `claude/specs/MCP_SERVER_SPEC.md` (already written, 308 lines)

| # | Task | Files | Status | Notes |
|---|------|-------|--------|-------|
| 3.1 | Wire `cot mcp` CLI subcommand | `cli.zig`, `main.zig` | **DONE** | Command enum + dispatch + help text |
| 3.2 | MCP main loop + JSON-RPC dispatch | `lsp/mcp_main.zig` | **DONE** | Newline-delimited, per-message arena |
| 3.3 | MCP server: initialize + tools/list | `lsp/mcp_server.zig` | **DONE** | Full MCP 2024-11-05 protocol handshake |
| 3.4 | Port static tools from Cot MCP | `lsp/mcp_server.zig` | **DONE** | get_syntax_reference, get_stdlib_docs, get_project_info |
| 3.5 | `check_file` tool (dynamic) | `lsp/mcp_server.zig` | **DONE** | Reuses `analysis.analyze()`, formats errors with line:col |
| 3.6 | `get_type` tool (dynamic) | `lsp/mcp_server.zig` | **DONE** | Reuses `hover.getHover()`, position-based type info |
| 3.7 | `find_definition` tool (dynamic) | `lsp/mcp_server.zig` | **DONE** | Reuses `goto.getDefinition()`, returns `file:line:col` |
| 3.8 | `list_symbols` tool (dynamic) | `lsp/mcp_server.zig` | **DONE** | Reuses `document_symbol.getDocumentSymbols()` |
| 3.9 | `build` tool | `lsp/mcp_server.zig` | **DONE** | Spawns `cot build`, captures stdout/stderr |
| 3.10 | `run_tests` tool | `lsp/mcp_server.zig` | **DONE** | Spawns `cot test`, supports filter param |
| 3.11 | Update `.mcp.json` | `.mcp.json` | **DONE** | Points to `./zig-out/bin/cot mcp` |

**After Tier 3:** The Cot-written MCP (`mcp/cot-mcp.cot`) stays in repo as stdlib dogfooding example.

### Tier 4: Editor Polish

**Goal:** The editor experience feels professional — snippets, tasks, visual polish.

| # | Task | Files | Effort | Notes |
|---|------|-------|--------|-------|
| 4.1 | VS Code snippets | New: `editors/vscode/snippets/cot.json` | Hours | fn, struct, impl, test, if, while, for, match |
| 4.2 | Build task integration | New: `editors/vscode/tasks.json` template | Hours | Ctrl+Shift+B → `cot build` |
| 4.3 | Code actions: quick fixes | New: `code_actions.zig` | 1-2 days | "Add import", "Fix typo", "Add missing field" |
| 4.4 | Inlay hints | New: `inlay_hints.zig` | 1 day | Show inferred types for `var x = ...` |
| 4.5 | Document links | `server.zig` | Hours | Make `import "std/list"` clickable |
| 4.6 | Tree-sitter grammar | New: `tree-sitter-cot/` | 2-3 days | Enables Neovim, Helix, Zed, GitHub highlighting |

---

## Execution Order

**Phase A** (enables human Cot writing):
1. Tier 1 (LSP trustworthy) — do first, all items
2. Tier 3.1–3.4 (MCP foundation + static tools) — quick win

**Phase B** (enables productive multi-file projects):
3. Tier 2.1–2.3 (cross-file goto/refs) — biggest impact
4. Tier 3.5–3.8 (MCP dynamic tools) — Claude writes better Cot
5. Tier 1 leftovers if any

**Phase C** (enables ecosystem):
6. Tier 3.9–3.11 (MCP build/test tools) — Claude can build and test
7. Tier 4 (editor polish) — nice to have
8. Tier 2.4–2.6 (cross-file rename, workspace symbols, import completion)

---

## Dependencies on Language Workstream

| DX Task | Depends On | Notes |
|---------|-----------|-------|
| Dot-completion for methods | Checker's type resolution | Read-only — DX queries checker results |
| Cross-file go-to-def | Import resolution in checker | May need checker changes to expose import graph |
| `build` MCP tool | Stable `compileAndLink()` | Spawns `cot build` as subprocess — no coupling |
| Inlay hints | Type inference in checker | Read-only |

**Rule:** DX never modifies the checker or lowerer. If a DX feature needs compiler changes, file it as a language workstream task.

---

## Success Criteria

### For the Architect (Human Writing Cot)

1. Open a `.cot` file in Cursor → see syntax highlighted, errors underlined
2. Type `list.` → see `append`, `get`, `set`, `len`, `pop`, etc. with signatures
3. Hover over a variable → see its full type (including generic params)
4. Ctrl+click a function call → jump to its definition (even in stdlib)
5. Rename a symbol → all references updated across the file
6. `cot fmt` on save → code auto-formatted
7. Write `test "my test" { }` → `cot test --filter="my test"` runs just that test

### For Claude (AI Writing Cot)

1. MCP `get_syntax_reference` → knows exact syntax (no guessing)
2. MCP `check_file` → sees errors before user does
3. MCP `get_type` → understands types at any position
4. MCP `list_symbols` → knows what's defined in a file
5. MCP `build` → can compile and report success/failure
6. MCP `run_tests` → can run tests and report results

### For Dogfooding Apps (cot.land, cot.dev)

1. Claude can write Cot code that compiles on first attempt (MCP-guided)
2. LSP provides real-time feedback as code is developed
3. Cross-file navigation works for multi-file projects
4. Build/test cycle is fast (no manual steps)

---

## What NOT To Do

- **Don't build a debugger** — not needed for self-hosting or dogfooding
- **Don't build a REPL** — Cot is compiled, not interpreted
- **Don't over-engineer MCP** — 9 tools is enough; don't add more until proven needed
- **Don't optimize LSP performance** — correctness first, speed later
- **Don't build multi-project workspaces** — single-project is fine for now

---

## File Map (New Files This Roadmap Creates)

```
compiler/lsp/
  mcp_main.zig          — MCP entry point (Tier 3.2)
  mcp_server.zig        — MCP method dispatch + tool handlers (Tier 3.3)
  signature_help.zig    — Parameter hints (Tier 1.3)
  workspace_symbol.zig  — Workspace-wide symbol search (Tier 2.5)
  code_actions.zig      — Quick fixes (Tier 4.3)
  inlay_hints.zig       — Inferred type hints (Tier 4.4)

editors/vscode/
  snippets/cot.json     — Code snippet templates (Tier 4.1)
```

**Modified files:** `server.zig` (new handlers), `completion.zig` (dot-completion, filtering, snippets), `hover.zig` (expanded info), `goto.zig` (cross-file), `references.zig` (cross-file), `document_store.zig` (multi-file), `analysis.zig` (dependency graph), `cli.zig` (mcp command), `main.zig` (mcp dispatch), `.mcp.json` (point to `cot mcp`)
