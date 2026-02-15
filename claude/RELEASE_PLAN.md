# Cot Release Plan: 0.4 and Beyond

## The Goal

A developer runs `brew install cot`, types `cot init myapp`, and within 5 minutes has a working project with formatting, testing, LSP, and clear error messages — comparable to installing Zig or Deno.

**0.4 theme: "A language developers take seriously."**

---

## Branding & Identity

### Name

The language is **Cot** (capital C in prose, lowercase `cot` in CLI/code). Named after the creator's surname, **Cottrell** — an old English word for someone who lived in a small cottage, typically a tenant farmer working the land.

**Capitalization rules:**
- "Cot" in prose (proper noun, like Zig, Rust, Deno)
- `cot` in code and CLI context (like `zig`, `rustc`, `deno`)
- Never "CoT" (reads as "Chain of Thought", an AI acronym)
- Never "COT" (screaming case, looks like an abbreviation)

**Why keep the 3-letter name:**
- Fast to type: `cot build`, `cot run`, `cot test`
- Easy to remember, easy to search ("cot programming language" is unique)
- Follows the trend: go, zig, nim, roc, lua, c, php
- The "baby bed" association fades with recognition (like Rust = corrosion, Go = board game)

**Cottrell as origin story, not formal name:**
The About page on cot.dev tells the story: *"Cot takes its name from Cottrell — old English for a cottage dweller. The language follows that spirit: build something solid, comfortable, and yours."*

### Ecosystem Names

| Name | Domain | Purpose |
|------|--------|---------|
| **Cot** | — | The language and compiler |
| **Cotland** | cot.land | Package registry (real English word: land held by a cotter) |
| **cot.dev** | cot.dev | Documentation site + interactive playground |
| **cot-land** | github.com/cot-land | GitHub organization |

### Visual Identity

**Direction:** Anti-Vercel. Country/classy, not tech/futuristic. Getting back to nature.

Every dev tool currently looks the same: dark backgrounds, neon gradients, geometric shapes, "blazingly fast" copy, rocket/lightning metaphors. Cot takes the opposite direction — warm, solid, trustworthy. Like the difference between a silicon valley co-working space and a well-built stone cottage.

**Logo:** Minimal cottage silhouette. Clean architectural line art — pitched roof, possibly a chimney with a subtle curl of smoke. Simple enough to work as a 16x16 favicon. Not cartoonish, not overly detailed.

**Color palette:**

| Role | Color | Hex (approx) | Notes |
|------|-------|--------------|-------|
| Primary | Deep forest green | `#2D5016` | Trust, nature, stability |
| Accent | Warm amber/gold | `#D4A843` | Warmth, quality, attention |
| Background | Cream/parchment | `#FAF7F0` | Clean, warm, inviting |
| Text | Stone dark | `#3A3632` | Readable, warm (not pure black) |
| Secondary | Warm brown | `#8B7355` | Earth, wood, groundedness |
| Code bg | Light sage | `#F2F5EE` | Gentle contrast for code blocks |
| Error | Muted red | `#A8423A` | Clear but not alarming |
| Success | Garden green | `#4A8C3F` | Positive, natural |

**Typography:**
- Headings: Serif or semi-serif (Georgia, Playfair Display, or similar). Signals quality and permanence.
- Body: Clean sans-serif (system stack or similar). Readable, not trendy.
- Code: Standard monospace. Warm-tinted background.

**Design principles for cot.dev:**
- Light mode by default (dark mode available). Warm aesthetics lose impact in dark mode.
- Code blocks on cream/sage backgrounds, not dark gray.
- Illustrations: hand-drawn or woodcut style, not vector/geometric.
- Photography (if any): countryside, stone walls, morning light — not stock photos of laptops.
- Generous whitespace. Feels spacious, not cramped.
- The vibe: English countryside meets Scandinavian clean design. Like early Stripe, but earth tones.

**Tagline options:**
- "Write like TypeScript, run like Rust, deploy anywhere, never think about memory." (current, good)
- "Build something solid." (short, matches cottage metaphor)
- "Come home to real types." (playful, differentiates from TypeScript)
- "A comfortable language for serious work." (captures the vibe)

### Brand Metaphor

The cottage metaphor extends naturally through the entire product:

| Concept | Metaphor |
|---------|----------|
| The compiler | The foundation — solid, handles everything underneath |
| ARC memory | The house takes care of itself (plumbing, heating, cleanup) |
| Stdlib modules | Rooms — each well-built, everything you need |
| `cot.land` | The land you build on (package registry) |
| `cot.dev` | The blueprints and guidebook (documentation) |
| Error messages | A helpful neighbor pointing out exactly what's wrong |
| No GC pauses | No surprise renovations while you're living in the house |
| No runtime | You own the house outright (no landlord/VM taking a cut) |

---

## Current State Audit (0.3.1)

### What Works Well

| Area | Status | Notes |
|------|--------|-------|
| CLI | 11 subcommands | build, run, test, bench, check, lint, fmt, init, lsp, mcp, version |
| Error messages | Excellent | file:line:col, ANSI colors, caret underlines, error codes |
| Test runner | Good | Deno-style output, timing, `--filter` support |
| Formatter | Works | AST-based, comment-preserving, 4-space canonical |
| LSP | Feature-rich | Hover, goto-def, completions, rename, references, semantic tokens, cross-file |
| VS Code extension | Functional | TextMate grammar + LSP client |
| Install script | Professional | OS/arch detection, GitHub releases, `~/.cot/bin` |
| CI/CD | Automated | Test on push, binary releases on tag |
| CHANGELOG | Detailed | Keep a Changelog format, SemVer |
| Documentation | New | Getting started guide, stdlib reference, 8 examples, syntax reference |

### What's Missing (Deal-Breakers for "Genuine Language")

| Gap | Impact | Fix Effort |
|-----|--------|-----------|
| No Homebrew formula | macOS devs can't find us | Hours |
| No x86_64-macos binary | Intel Mac users excluded | Hours (CI matrix) |
| VS Code extension not on marketplace | Must install manually | Hours |
| ~~`cot fmt` doesn't modify in-place by default~~ | ~~Confusing UX~~ | **Done** (Feb 15) |
| ~~`cot.json` does nothing useful~~ | ~~Scaffolded but fake~~ | **Done** (Feb 15) |
| ~~No error suggestions ("did you mean X?")~~ | ~~Every modern language has this~~ | **Done** (Feb 15) |
| No watch mode | Must manually re-run after edits | Days |
| No `cot doc` | Can't generate API docs | Days |
| No `cot upgrade` | Can't self-update | Days |
| No shell completions | No `cot <tab>` | Days |

### Comparison vs Zig and Deno

**What Deno has that Cot doesn't:** 123 lint rules, watch mode on everything, `deno doc --html`, `deno upgrade`, `deno completions`, `deno add` (package manager), REPL, Jupyter kernel, test coverage, JUnit XML export, `deno audit`, multiple test reporters.

**What Zig has that Cot doesn't:** Homebrew formula, cross-compilation to any target, `zig cc` (drop-in C compiler), `zig zen`, incremental compilation.

**What Cot has that neither does:**
- AOT compilation to native binary from Wasm (no runtime needed)
- ARC memory management (no GC, no borrow checker)
- Wasm as first-class browser target
- MCP server for AI-assisted development
- Both server-native and browser-Wasm from same source

---

## Development Strategy

**Build first, distribute later.** The next 2-3 months are pure feature development — rapidly iterating through 0.4 to ~0.8. Distribution (Homebrew, VS Code marketplace, etc.) happens when there's something genuinely worth someone's time. Planning for distribution is done; implementation is deferred.

**Version cadence:** Ship versions frequently with significant changes per version. No small feature-set versions. Each version should feel like a meaningful step toward a real language.

---

## Phase 1: Distribution (DEFERRED — implement at ~0.8 before public launch)

These are planned and ready to execute when the language is mature enough for public use.

| # | Item | Description | Effort |
|---|------|-------------|--------|
| D1 | Homebrew tap | Create `homebrew-tap` repo, write formula, publish. `brew install cot-land/tap/cot` | Hours |
| D2 | x86_64-macos binary | Add to release matrix in `.github/workflows/release.yml`. Cross-compile or add runner. | Hours |
| D3 | VS Code marketplace | Publish `cot-lang` extension to marketplace. Configure `package.json` publisher, icon, README. | Hours |
| D4 | `cot upgrade` | Self-update: fetch latest release from GitHub API, download binary, replace self. | 1 day |
| D5 | Shell completions | `cot completions zsh`, `cot completions bash`, `cot completions fish`. Output completion scripts. | 1 day |
| D6 | Logo & brand assets | Commission or create cottage logo, apply color palette to cot.dev, generate social preview images. | Days |
| D7 | cot.dev launch | Documentation site with branded design, getting started guide, playground. | 1-2 weeks |

---

## Phase 2: First-Five-Minutes Polish (ACTIVE)

These fix the immediate pain points a developer hits after installing.

| # | Item | Description | Effort |
|---|------|-------------|--------|
| P1 | ~~`cot fmt` in-place default~~ | **Done** (Feb 15). Default is in-place (like gofmt/rustfmt/zig fmt). `--check` for CI, `--stdout` for piping. Skips write when unchanged. | **Done** |
| P2 | ~~Error suggestions~~ | **Done** (Feb 15). Levenshtein distance on identifiers, fields, variants, types. "did you mean 'X'?" on 12 error sites. | **Done** |
| P3 | ~~`cot.json` integration~~ | **Done** (Feb 15). All 7 file-requiring commands read `main` from `cot.json` when no input file given. `cot init myapp && cd myapp && cot run` works. | **Done** |
| P4 | Improved test failures | Show expected vs actual diff on `@assert_eq` failure (like Deno). Color-coded diff. | 1 day |
| P5 | `cot init` improvements | Better template: include a test file (`src/main_test.cot`), print "next steps" with exact commands. Consider `cot init --lib` variant. | Hours |

### Phase 3: Daily Workflow Tooling

Features developers use every day during development.

| # | Item | Description | Effort |
|---|------|-------------|--------|
| T1 | Watch mode | **Done** (11e676f). `cot run --watch`, `cot test --watch` via fsevents/inotify. | **Done** |
| T2 | Doc comments (`///`) | **Done** (11e676f). Parse `///` comments above declarations, store in AST. | **Done** |
| T3 | `cot doc` | **Done** (11e676f). Generate HTML documentation from `///` doc comments. | **Done** |
| T4 | `cot info` | Show project info: version, entry point, targets, dependencies (when package manager exists). | Hours |
| T5 | Expanded lint rules | **Done** (11e676f). Unused imports, unreachable code after return, empty blocks, shadowed variables. | **Done** |
| T6 | `cot task` | **Done** (11e676f). Run tasks from `cot.json`. | **Done** |

### Phase 4: Language Features (Waves 4-6 from Roadmap)

These run in parallel with Phases 1-3. Some are already in progress from the other Claude session.

**Wave 4 (ecosystem polish) — ALL DONE:**

| # | Feature | Status | Notes |
|---|---------|--------|-------|
| 24 | `cot check` | **DONE** | Type-check without compile (3ddbe10) |
| 25 | `cot lint` (basic) | **DONE** | Unused vars, unreachable, shadowing + expanded rules (3ddbe10, 11e676f) |
| 26 | Improved test output | **DONE** | Colors, timing, failure diffs (3ddbe10) |
| 27 | Watch mode | **DONE** | `--watch` flag via fsevents/inotify (11e676f) |
| 28 | `cot bench` | **DONE** | bench blocks, Go-style adaptive calibration, ns/op (742ec8e) |
| 29 | `cot task` | **DONE** | Tasks from cot.json (11e676f) |

**Wave 5 (language maturity — Zig ports):**

| # | Feature | Priority | Notes |
|---|---------|----------|-------|
| 30 | Destructuring | **DONE** | `const a, b = getTuple()`, `const a, b, c = getTriple()` |
| 31 | Inferred error sets (`!T`) | **DONE** | `fn read() ![]u8` — parser + checker handle inferred sets |
| 32 | `noreturn` type | **DONE** | Bottom type for @exit, @trap. Coerces to any type. |
| 33 | Doc comments (`///`) | **DONE** | Parse `///`, store in AST (11e676f) |
| 34 | `cot doc` | **DONE** | Generate HTML API docs from `///` comments (11e676f) |
| 35 | `@embedFile("path")` | **DONE** | Compile-time file embedding, path relative to source file |
| 36-39 | Reflection builtins | Low | `@TypeOf`, `@hasField`, `@field`, `inline for` |
| 40 | Runtime safety | Medium | Overflow/bounds checks in debug |
| 41 | Error set merge | Low | `const All = A || B` |

**Wave 6 (stdlib expansion — Deno ports):**

| # | Feature | Priority | Notes |
|---|---------|----------|-------|
| 42 | `std/path` | **DONE** | basename, dirname, extname, isAbsolute, join, clean, relative. 38 tests (d363cc5) |
| 43 | `std/crypto` | **DONE** | SHA-256 (FIPS 180-4), HMAC-SHA256 (RFC 2104). 17 tests |
| 44 | `std/regex` | Medium | NFA-based, match/find/replace |
| 45 | `std/fmt` | **DONE** | ANSI colors, text styles, stripAnsi, formatBytes, formatDuration, padding, hex. 36 tests |
| 46 | `std/log` | **DONE** | Structured logging with levels, timestamps, key-value. 14 tests |
| 47 | `std/dotenv` | **DONE** | parseEnv, get/has/entryCount/entryKey/entryValue. 12 tests (d363cc5) |
| 48 | `std/cli` | **DONE** | --flag=value, -f, positional, getFlag/hasFlag/getFlagInt. 13 tests |
| 49 | `std/uuid` | **DONE** | UUID v4 generation, isValid, version. 10 tests (d363cc5) |
| 50 | `std/semver` | **DONE** | parse, cmp, gt/gte/lt/lte/eq, format, incMajor/Minor/Patch. 28 tests (d363cc5) |
| 51 | `std/testing` | **DONE** | assertContains, assertStartsWith/EndsWith, assertGt/Lt/InRange, assertTrue/False, assertEmpty/Len. 21 tests |
| 52 | `std/process` | High | Subprocess spawning |

### 0.4 Release Criteria

Before tagging 0.4, all of these must be true:

1. `brew install cot-land/tap/cot` works on macOS (ARM64 + x86_64)
2. ~~`cot init myapp && cd myapp && cot run` works end-to-end~~ **Done** (Feb 15)
3. ~~`cot fmt src/main.cot` modifies the file in-place~~ **Done** (Feb 15)
4. `cot test` shows colored pass/fail with timing and failure diffs
5. `cot doc` generates HTML from `///` comments
6. `cot upgrade` updates to the latest release
7. VS Code marketplace extension installs and provides LSP
8. ~~Error messages include "did you mean X?" suggestions~~ **Done** (Feb 15)
9. ~~`cot.json` `main` field is used by all file-requiring commands~~ **Done** (Feb 15)
10. Watch mode works: `cot run --watch`, `cot test --watch`
11. Shell completions work for zsh and bash
12. At least `std/path`, `std/crypto`, `std/cli`, `std/process` are added
13. Doc comments (`///`) are parsed and stored
14. All existing tests still pass on both native and Wasm targets

### Implementation Order (Suggested)

**Week 1: Distribution + quick wins**
- D1: Homebrew tap
- D2: x86_64-macos binary
- D3: VS Code marketplace
- ~~P1: `cot fmt` in-place default~~ **Done** (Feb 15)
- P5: `cot init` improvements

**Week 2: Polish**
- ~~P2: Error suggestions ("did you mean")~~ **Done** (Feb 15)
- ~~P3: `cot.json` integration~~ **Done** (Feb 15)
- P4: Improved test failure output
- D4: `cot upgrade`
- D5: Shell completions

**Week 3: Tooling**
- T1: Watch mode
- T2: Doc comments (`///`)
- T6: `cot task`
- T4: `cot info`

**Week 4: `cot doc` + stdlib**
- T3: `cot doc` HTML generation
- T5: Expanded lint rules
- Wave 6 stdlib: `std/path`, `std/cli`, `std/process`

**Week 5+: Language features + stdlib**
- Wave 5 language features (destructuring, `!T`, `@embedFile`, etc.)
- Wave 6 stdlib continued (`std/crypto`, `std/regex`, `std/fmt`, etc.)

---

## 0.5 Plan (Preview)

**Theme: "Build real products."**

0.5 is the release where Cot gets a package manager, web framework prototype, and database driver. Developers can build and ship real applications.

| Category | Features |
|----------|----------|
| Package manager | `cot add`, `cot remove`, `cot publish`, lockfile, version resolution |
| Registry | cot.land — browse, search, publish packages |
| Web framework | `@server`/`@client` annotations, shared types, auto-serialization |
| Database | `std/db` — SQLite driver, parameterized queries |
| Browser | `std/dom` — DOM manipulation for `--target=wasm32` |
| Cross-compile | `cot build --target=x86_64-linux` from macOS |
| Test coverage | `cot coverage` — line/branch coverage, lcov output |
| Language | Packed structs, wrapping arithmetic, weak references |

---

## 0.6+ Plan (Preview)

**Theme: "Production-grade."**

| Category | Features |
|----------|----------|
| Concurrency | `spawn {}` blocks, `Channel(T)`, `select`, work-stealing scheduler |
| Safety | Permission system (`--allow-read`, `--allow-net`), atomic ARC |
| Observability | Built-in OpenTelemetry, auto-instrument HTTP |
| Performance | SIMD vectors, incremental compilation |
| Interop | Sentinel-terminated types for C FFI, subprocess management |

---

## Deferred (Not in Scope)

| Feature | Reason | When |
|---------|--------|------|
| REPL | Compiled language, low priority (Zig doesn't have one) | Maybe never |
| Self-hosting | Graduation ceremony, not entrance exam | Post-1.0 |
| `cot deploy` | Needs infrastructure (cot.land) first | 0.6+ |
| Jupyter kernel | Niche | Maybe never |
| JUnit XML export | CI integration, low priority | 0.5 |
| npm interop | Different ecosystem | Evaluate at 0.6 |

---

## Version Bumping Strategy

**When to release 0.4:**
- All 14 release criteria above are met
- No known crashes or wrong-output bugs in the test suite
- Documentation (getting-started, stdlib reference, examples) is up to date

**Process:**
1. Update `VERSION` to `0.4.0`
2. Update `CHANGELOG.md` with all changes since 0.3.1
3. Tag `v0.4.0`
4. CI builds binaries and creates GitHub Release
5. Update Homebrew formula
6. Update VS Code marketplace extension
7. Announce (GitHub, social media, Hacker News)
