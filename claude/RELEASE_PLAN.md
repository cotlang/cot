# Cot Release Plan: Branding & Distribution

See `ROADMAP.md` for version milestones. This document covers branding, identity, and distribution details.

---

## Branding & Identity

### Name

The language is **Cot** (capital C in prose, lowercase `cot` in CLI/code). Named after the creator's surname, **Cottrell** — an old English word for someone who lived in a small cottage, typically a tenant farmer working the land.

**Capitalization rules:**
- "Cot" in prose (proper noun, like Zig, Rust, Deno)
- `cot` in code and CLI context (like `zig`, `rustc`, `deno`)
- Never "CoT" (reads as "Chain of Thought", an AI acronym)
- Never "COT" (screaming case, looks like an abbreviation)

### Ecosystem Names

| Name | Domain | Purpose |
|------|--------|---------|
| **Cot** | — | The language and compiler |
| **Cotland** | cot.land | Package registry (real English word: land held by a cotter) |
| **cot.dev** | cot.dev | Documentation site + interactive playground |
| **cotlang** | github.com/cotlang | GitHub organization |

### Visual Identity

**Direction:** Anti-Vercel. Country/classy, not tech/futuristic. Getting back to nature.

**Logo:** Minimal cottage silhouette. Clean architectural line art — pitched roof, possibly a chimney with a subtle curl of smoke. Simple enough to work as a 16x16 favicon.

**Color palette:**

| Role | Color | Hex | Notes |
|------|-------|-----|-------|
| Primary | Deep forest green | `#2D5016` | Trust, nature, stability |
| Accent | Warm amber/gold | `#D4A843` | Warmth, quality, attention |
| Background | Cream/parchment | `#FAF7F0` | Clean, warm, inviting |
| Text | Stone dark | `#3A3632` | Readable, warm (not pure black) |
| Secondary | Warm brown | `#8B7355` | Earth, wood, groundedness |
| Code bg | Light sage | `#F2F5EE` | Gentle contrast for code blocks |
| Error | Muted red | `#A8423A` | Clear but not alarming |
| Success | Garden green | `#4A8C3F` | Positive, natural |

**Typography:**
- Headings: Serif or semi-serif (Georgia, Playfair Display). Signals quality and permanence.
- Body: Clean sans-serif (system stack). Readable, not trendy.
- Code: Standard monospace. Warm-tinted background.

**Design principles for cot.dev:**
- Light mode by default (dark mode available)
- Code blocks on cream/sage backgrounds, not dark gray
- Illustrations: hand-drawn or woodcut style, not vector/geometric
- Generous whitespace. Feels spacious, not cramped.
- The vibe: English countryside meets Scandinavian clean design

**Tagline options:**
- "Write like TypeScript, run like Rust, deploy anywhere, never think about memory."
- "Build something solid."
- "A comfortable language for serious work."

### Brand Metaphor

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

## Distribution Checklist (for 0.7 public release)

| # | Item | Description | Effort |
|---|------|-------------|--------|
| 1 | Homebrew tap | `homebrew-tap` repo, formula. `brew install cotlang/tap/cot` | Hours |
| 2 | All platform binaries | aarch64-macos, x86_64-macos, x86_64-linux, aarch64-linux | Hours (CI matrix) |
| 3 | VS Code marketplace | Publish `cot-lang` extension | Hours |
| 4 | `cot upgrade` | Self-update from GitHub releases | 1 day |
| 5 | Shell completions | `cot completions zsh/bash/fish` | 1 day |
| 6 | Logo & brand assets | Cottage logo, color palette applied to cot.dev | Days |
| 7 | cot.dev launch | Docs site, getting started, playground | 1-2 weeks |

---

## Bootstrap Story (for 0.4+)

After Wasm self-hosting (0.4), the install story becomes:

```bash
# Clone and bootstrap — no Zig needed
git clone https://github.com/cotlang/cot.git && cd cot
wasmtime bootstrap/cot.wasm build self/main.cot -o cot
./cot build self/main.cot -o cot   # now self-hosting natively (after 0.5)
```

The `bootstrap/cot.wasm` binary is architecture-independent — one blob works on every OS and architecture. This is cleaner than Go's bootstrap (required Go 1.4 C binary per-platform) and Zig's (requires stage1 C++ compiler).
