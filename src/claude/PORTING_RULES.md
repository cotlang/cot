# Porting Rules: compiler/ → src/

**These rules apply to ALL code moved from `compiler/` to `src/`.**

---

## 1. Remove Reference Comments

The code in `compiler/` is littered with comments like:
```
// Go reference: cmd/compile/internal/ssa/schedule.go line 260
// Swift: GenCall.cpp address-only convention
// Reference: cg_clif abi/mod.rs:97-115
// Cranelift pattern: ...
```

**Delete all of these.** The code is proven and tested. The reference comments were useful during initial development but are noise in production code. If someone needs the reference, it's in `compiler/` (frozen) or in git history.

**Keep:** Comments that explain WHY the code does something non-obvious. Delete comments that explain WHERE the code came from.

```
// BAD — remove:
// Go: ssaGenValue lines 280-284 — stores have no setReg call
// Swift Phase 8.5: T-indirect var init → memcpy from __ptr_ to copy full value
// Reference: wasmparser crate's Module parsing

// GOOD — keep:
// Skip memory args — they exist for SSA ordering but have no wasm representation
// COW: deep copy buffer only if refcount > 1
// Non-reentrant by default — see SWIFT_CONCURRENCY_PORT.md Appendix D.4
```

## 2. Remove Dead Code

`compiler/` accumulated dead functions, commented-out blocks, and TODO stubs over 3 months of rapid development. The port is an opportunity to delete:

- Unused functions (check with `zig build` warnings)
- Commented-out code blocks
- `TODO` / `FIXME` / `HACK` comments with no associated code
- Debug print statements left from debugging sessions
- Unused imports

## 3. Clean Up Naming

Standardize names during the port:

- Remove Hungarian notation remnants (`saved_tree_m`, `f_is_static_n`, `effective_static_gi`)
- Use descriptive names (`has_explicit_self` not `has_self_gi`)
- Consistent casing: `snake_case` for functions/variables, `PascalCase` for types
- Remove numeric suffixes from copy-pasted variables (`v1`, `v2`, `v3` → meaningful names)

## 4. Simplify Where Possible

The `compiler/` code has accidental complexity from iterative bug fixes. Each function ported should be reviewed:

- Can the function be shorter?
- Are there branches that can never be reached?
- Are there parameters that are always the same value?
- Can nested `if` chains become a `switch`?

Do NOT change logic. Do simplify expression of that logic.

## 5. One File, One Responsibility

`compiler/` has some files that grew too large:
- `lower.zig` — 13K lines (lowerer + VWT gen + generic instantiation + async + everything)
- `driver.zig` — 6.7K lines (compilation + MachO gen + ELF gen + linking + debugging)
- `checker.zig` — 5K lines (type checking + method resolution + generic instantiation)

Split during the port where natural boundaries exist. The library structure already enforces the big splits (frontend vs IR vs backend). Within each library, aim for files under 2K lines.

## 6. C ABI Boundary is the Contract

The headers in `src/include/` define the contract between libraries. When porting:

- Every public function in a library must be callable through its C ABI header
- Internal functions should be `fn` (private), not `pub fn`
- Data structures that cross the boundary use opaque pointers (`*anyopaque` / `void*`)
- No Zig-specific types in the C ABI (no slices, no optionals, no error unions)

## 7. Tests Port With the Code

Every function ported to `src/` must be testable through the new structure. The existing test suite (`test/e2e/`, `test/cases/`) runs against the final linked binary — it doesn't care which directory the code lives in.

Verification at each step: `370+ features pass, 860+ unit tests pass, selfcot builds.`

## 8. Don't Port What's Being Deleted

The concurrency code is being rewritten from Go-style to Swift-style. Don't port:
- Old spawn/select lowering
- Old scheduler code
- Old channel implementation
- Old async event loop

Wait for the Swift concurrency port to land in `compiler/`, then port the new code.

## 9. Git Discipline

- One commit per logical unit (one file or one closely-related group)
- Commit message: `port: <file> from compiler/ to src/libX-zig/`
- Never mix porting with bug fixes — if you find a bug while porting, fix it in `compiler/` first, verify tests pass, then port the fixed version
