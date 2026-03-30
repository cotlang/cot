# Porting Rules: compiler/ → src/

**These rules apply to ALL code moved from `compiler/` to `src/`.**

**Academic documentation:** Every ported file gets a companion `.md` file in `~/cotlang/ac/` that mirrors the source path exactly. The markdown explains the code section by section with matching line numbers. This becomes the educational content for `cot.ac`.

```
Source:  ~/cotlang/cot/src/libcot-zig/scanner.zig
Docs:    ~/cotlang/ac/src/libcot-zig/scanner.md
```

---

## 1. Transform, Don't Copy

**This is NOT a bulk copy with cleanup. Every file must be actively improved during the port.**

Before writing a single line, read the entire original file and ask:
- Can any repeated patterns be replaced with comptime reflection or data-driven design?
- Are there if-chains or switches that could be a lookup table or enum method?
- Are there functions doing too many things that should be split?
- Can types be made more expressive (named fields vs positional args, enums vs bools)?
- Are there better Zig idioms for what the code is doing (see `references/zig/lib/std/`)?

The `debug.zig` port is the standard to meet — it replaced 13 explicit bool fields + 13 if-statements + a 13-way switch with one `[N]bool` array + `inline for` over `@typeInfo`. That's the level of improvement expected on EVERY file. If the ported file looks the same as the original with just comments removed, it hasn't been ported properly.

**Spend time studying the Zig standard library before porting complex files.** Read how `std.hash_map.zig`, `std.mem.zig`, `std.fmt.zig` solve similar problems. Use their patterns.

---

## 2. Remove Reference Comments

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

## 9. Slow and Methodical — Educational Porting

This is not a bulk copy. Each file is ported in small chunks (~200-300 lines) with a summary explaining what the code does. The goal is for John to understand every line of the compiler by reading the port.

**Process for each chunk:**

1. Read ~200-300 lines from `compiler/`
2. Explain what the code does in plain English — what problem it solves, why it's shaped this way
3. Port it to `src/` with cleanup applied (remove reference comments, simplify, rename)
4. Show the before/after so the changes are visible
5. Wait for confirmation before continuing to the next chunk

**The pace is the point.** This is a learning exercise, not a speed exercise. A file that took Claude 30 seconds to write originally should take an hour to port properly with explanations.

**Order for each chunk:**
1. Write the cleaned code to `src/` FIRST
2. Then write the companion documentation to `ac/`
3. Line numbers in the docs must match the source file exactly

**What the documentation should cover:**
- What this section of code does (high-level purpose)
- How it fits into the compilation pipeline
- Any non-obvious design decisions and why they were made
- What was cleaned up from the original and why
- A "changes from original" table at the end of each file

## 10. Git Discipline

- One commit per logical unit (one file or one closely-related group)
- Commit message: `port: <file> from compiler/ to src/libX-zig/`
- Never mix porting with bug fixes — if you find a bug while porting, fix it in `compiler/` first, verify tests pass, then port the fixed version
