# Cot Wasm Platform Vision — What Self-Hosting Unlocks

**Date:** 2026-03-21
**Prerequisite:** Wasm self-hosting (selfcot compiles itself to .wasm)
**Status:** 13/13 frontend files compile. Codegen/emit remaining.

---

## The Core Unlock

Today: `cot build main.cot -o app.wasm` runs on your laptop.
After self-hosting: `wasmtime selfcot.wasm build main.cot -o app.wasm` runs ANYWHERE Wasm runs — browser, edge, phone, embedded.

The compiler becomes a **portable, sandboxed, zero-install tool** that runs on every device with a Wasm runtime. That's every modern browser, every cloud edge node, every IoT device.

---

## Cotty in the Browser

Cotty today: native macOS app (Swift + Metal + libcotty.dylib).

Cotty in browser: the SAME editor, compiled to Wasm. Here's what changes:

**Terminal emulator** — Cotty already has a VT100 parser, grid renderer, PTY interface. In the browser:
- Terminal grid renders to Canvas2D or WebGL (replacing Metal)
- Shell connects to a WebSocket backend (replacing PTY) — or runs a Wasm shell locally
- The VT parser, cursor movement, scrollback — all the same Cot code, compiled to Wasm

**Code editor** — Cotty's editor surface (Document, Cursor, Selection, Transaction) is pure Cot. It compiles to Wasm directly. The rendering layer swaps from Metal to Canvas.

**Compiler integration** — THIS is where it gets powerful. The compiler runs IN Cotty. When you save a file:
1. Cotty calls `selfcot.wasm build file.cot -o output.wasm` — in-process, no IPC
2. Compilation takes milliseconds (Wasm JIT is fast, compiler is already loaded)
3. Output module is instantiated immediately
4. Hot reload — running app updates without page refresh

**LSP** — `cot lsp` runs as Wasm. Diagnostics, autocomplete, go-to-definition — all in the browser tab. No language server process, no WebSocket to a backend.

**File system** — Browser has no real filesystem, but:
- IndexedDB for persistent project storage
- File System Access API for reading/writing local files (Chrome, Edge)
- Virtual filesystem in Wasm memory for temp files
- Git via isomorphic-git (pure JS/Wasm git client)

---

## Business Use Cases

### 1. Zero-Install Development Platform

Replit, CodeSandbox, StackBlitz — all run compilers on servers. Server costs scale with users. Cot's approach: the compiler runs ON THE CLIENT.

- User opens `cot.dev/playground`
- Browser downloads selfcot.wasm (~2MB) + stdlib
- User writes code, hits Run
- Compiler executes in their browser tab
- Output runs in the same tab
- **Zero server cost for compilation. Infinite horizontal scale.**

Revenue: freemium (free playground, paid for collaboration, deployment, private projects).

### 2. Edge-Native Deployment Pipeline

Traditional: push code → CI server compiles → deploy binary.
Cot: push SOURCE CODE to the edge. Edge node compiles on first request, caches binary.

```
Developer pushes main.cot to Cloudflare
→ Edge node receives request
→ Loads selfcot.wasm (cached)
→ Compiles main.cot to app.wasm (cached)
→ Executes app.wasm to serve the request
→ Subsequent requests hit the compiled cache
```

No build step. No CI. Push source, it runs. The edge IS your build server.

### 3. Embeddable Scripting Engine

Games, productivity apps, no-code platforms — they all need scripting/plugins. Today they use Lua, JavaScript, or proprietary DSLs.

Cot as an embedded scripting engine:
- App ships selfcot.wasm as a 2MB module
- Users write Cot plugins/scripts
- App compiles scripts in-process (sandboxed by Wasm)
- Compiled plugins run at near-native speed
- **Type-safe, compiled, sandboxed scripting** — not interpreted, not eval()

Example: a design tool where users write Cot plugins:
```cot
fn onSelectionChange(items: List(*Shape)) void {
    for i in 0..items.count {
        items.get(i).opacity = 0.5
    }
}
```

Compiles to Wasm in the browser, runs sandboxed, can't access filesystem or network unless explicitly granted.

### 4. Collaborative Real-Time IDE (Cotty Cloud)

Cotty in the browser + WebRTC for collaboration:
- Multiple users edit the same Cot project in their browsers
- Each user has their own compiler instance (Wasm)
- OT/CRDT for conflict resolution (Cotty's Document model)
- Compile/run happens locally — no server bottleneck
- Share a link → collaborator opens it → full IDE loads → they're editing

Google Docs for code, but with a REAL compiler, not syntax highlighting on a textarea.

### 5. Education Platform

Teaching programming with Cot:
- Student opens a lesson URL
- Interactive exercises with inline editor
- Code compiles and runs in the browser instantly
- No setup, no installation, no "download Python first"
- Works on Chromebooks (schools), phones (developing countries), any device

The compile-run loop is < 100ms because the compiler is already loaded and Wasm JIT is warm. Students get instant feedback — critical for learning.

### 6. Serverless Functions in Cot

AWS Lambda, Cloudflare Workers, Vercel Edge Functions — all support Wasm.

```cot
import "std/http"

fn handler(req: Request) Response {
    return Response.json({ "hello": req.query("name") })
}
```

Deploy with `cot deploy` → pushes source to edge → edge compiles with selfcot.wasm → serves requests. Cold start is just Wasm instantiation (~5ms), not container boot (~500ms).

### 7. Mobile Development

WKWebView (iOS) and WebView (Android) both run Wasm. A Cot app compiled to Wasm runs in a WebView wrapper:
- Single codebase → iOS + Android + Web + Desktop
- No React Native bridge overhead
- No Flutter engine (200MB)
- Just Wasm + Canvas + a thin native shell

Cotty itself could run this way — a WebView-wrapped browser app that IS a code editor + terminal + compiler. Install from App Store, runs the same Wasm everywhere.

---

## Advanced Apps That Become Possible

### Browser-Native Database

SQLite compiled to Wasm already exists. A Cot app can embed SQLite, run queries, store data — all in the browser. IndexedDB for persistence, SQLite for SQL queries, Cot for application logic. Full-stack app with zero server.

### Browser-Native AI Inference

ONNX Runtime compiles to Wasm. A Cot app can load an ML model, run inference in the browser. Image classification, text generation, code completion — all client-side, all private (data never leaves the device).

### Peer-to-Peer Applications

WebRTC for data channels, selfcot.wasm for compiling shared code, WasmGC for memory management. Two users connect directly, share a Cot project, compile and run each other's code — no server in the middle.

### Self-Evolving Applications

An app that rewrites parts of itself at runtime:
```cot
// App detects a performance bottleneck
// Generates optimized Cot code
// Compiles it with selfcot.wasm
// Hot-swaps the module
// Performance improves — without a deploy
```

Architecturally possible when the compiler is in the runtime.

---

## Cotty as Self-Contained IDE — Single App, Full Toolchain

### The Vision

Cotty bundles the entire Cot toolchain inside a single macOS app (and eventually Linux/browser). Download `Cotty.app`, open it, write Cot. Compiler, LSP, formatter, linter, test runner — everything works immediately with zero setup.

### What Gets Bundled

The `cot` binary is already a single executable that contains every tool:

| Tool | Command | Ships Inside Cotty |
|------|---------|-------------------|
| Compiler | `cot build` | Yes — `Cotty.app/Contents/Resources/cot` |
| LSP | `cot lsp` | Yes — same binary, Cotty's LSP client points to it |
| Formatter | `cot fmt` | Yes — format on save, zero config |
| Linter | `cot lint` | Yes — inline diagnostics |
| Test runner | `cot test` | Yes — run tests from editor, results in panel |
| REPL/runner | `cot run` | Yes — run scripts from terminal pane |

One binary (~5MB), all tools. No separate installs.

### User Experience: New User

1. Download `Cotty.app` (signed, notarized, Sparkle auto-updates — Ghostty installer pattern)
2. Open it
3. `File → New Project` creates `cot.json`, `src/main.cot`
4. Write code — get completions, diagnostics, hover docs immediately (bundled LSP)
5. `Cmd+B` builds (bundled compiler)
6. `Cmd+R` runs (output in terminal pane)
7. `Cmd+T` runs tests (results in test panel)

No `brew install`, no PATH setup, no "install the Cot extension", no "point to your compiler". Just works.

### Architecture

```
Cotty.app/
  Contents/
    MacOS/
      Cotty              ← Swift/Metal shell (thin)
    Frameworks/
      libcotty.dylib     ← all IDE logic (Cot)
    Resources/
      cot                ← compiler/LSP/fmt/lint/test binary
      stdlib/            ← standard library source
      shell-integration/ ← zsh/bash integration scripts
```

Cotty's LSP client spawns `Resources/cot lsp` — no system PATH lookup. Build commands invoke `Resources/cot build`. The bundled stdlib means `import "std/json"` works out of the box.

### Version Coupling

When you ship a new Cotty, it ships with the matching compiler version. No version mismatch headaches. Auto-update via Sparkle keeps compiler and IDE in sync. The compiler version is shown in the status bar.

### The External Install Still Matters

The standalone toolchain (`brew install cot`) remains for:
- **CI/CD** — headless builds, no GUI needed
- **Other editors** — VS Code, Neovim, Cursor users get `cot lsp` via PATH
- **Scripting** — `cot run script.cot` from any terminal
- **Building without a GUI** — servers, containers, automation

Same compiler, two distribution paths: `brew install cot` for the toolchain, `Cotty.app` for the batteries-included experience.

### Why This Works for Cot Specifically

Most languages can't do this cleanly because their toolchains are sprawling (cargo + rustup + clippy + rust-analyzer, or npm + node + tsc + eslint + prettier). Cot's toolchain is ONE binary with subcommands. That's what makes bundling trivial — it's already self-contained.

The self-hosted compiler (selfcot) compiles to a single native binary with zero dependencies. No runtime, no VM, no framework. Just a binary and stdlib source files.

### Future: Browser Cotty

When Cotty runs in the browser (Phase 4), the same pattern applies but with Wasm:
- `selfcot.wasm` replaces `Resources/cot` — compiler runs in-browser
- LSP runs as Wasm — diagnostics without a server
- stdlib bundled as virtual filesystem
- Zero install, zero server cost, infinite scale

---

## Competitive Position

No other language has all of these at once:

| Capability | Go | Rust | Kotlin | Zig | Cot |
|-----------|-----|------|--------|-----|-----|
| AOT compiled to Wasm | Yes (10MB runtime) | Yes (manual memory) | Yes | Yes (no GC) | **Yes** |
| WasmGC (browser GC) | No | No | Yes | No | **Yes** |
| Self-hosting in Wasm | No | No | No | No | **In progress** |
| TypeScript-like syntax | No | No | Kotlin-like | C-like | **Yes** |
| ARC on native | No | Ownership | No (JVM GC) | Manual | **Yes (Swift pattern)** |
| Full stdlib | Yes | Yes (crates) | Yes | Minimal | **Yes (35 modules)** |

Cot is positioned to be the **first language designed for both native performance (ARC) and browser portability (WasmGC)** with a self-hosting compiler that runs everywhere.

---

## Path to Browser Platform

### Phase 1: Complete Wasm Self-Hosting (current)
- [x] 13/13 frontend files compile via selfcot
- [ ] Codegen/emit files compile via selfcot
- [ ] `selfcot build self/main.cot -o selfcot.wasm` produces working compiler
- [ ] Bootstrap: `wasmtime selfcot.wasm build self/main.cot` succeeds

### Phase 2: Browser Compiler
- [ ] selfcot.wasm loads in browser via WebAssembly.instantiate
- [ ] Virtual filesystem for source files + stdlib
- [ ] Compile Cot source → Wasm module in browser
- [ ] Execute compiled module in same page

### Phase 3: Playground
- [ ] Monaco/CodeMirror editor with Cot syntax
- [ ] Compile on keystroke (debounced)
- [ ] Output panel for stdout/stderr
- [ ] Share via URL (source encoded in hash)

### Phase 4: Cotty Browser
- [ ] Port Cotty editor surface to Canvas/WebGL rendering
- [ ] Port terminal emulator to Canvas rendering
- [ ] Integrate selfcot.wasm for in-editor compilation
- [ ] LSP via selfcot.wasm (diagnostics, autocomplete)
- [ ] File System Access API for local project editing

### Phase 5: Platform
- [ ] Collaboration (WebRTC + CRDT)
- [ ] Deployment (edge compilation)
- [ ] Package registry
- [ ] Plugin marketplace

---

## Full-Stack Cot: Browser + Server, One Language

### Why Both Targets Matter

**Browser (Wasm):** UI, client logic, offline capability, zero-install.
**Server (native):** Database access, authentication, background jobs, heavy computation, secrets.

An ERP, SaaS app, or any real business application needs both. You don't want database credentials in a Wasm binary the user can inspect. You don't want 10GB of data downloaded to the browser. You don't want long-running reports blocking the UI thread.

The power of Cot: **same language, same stdlib, both sides.**

```
Browser (WasmGC)                    Server (native ARC)
─────────────────                   ───────────────────
std/ui (Canvas components)          std/http (server)
std/http (fetch client)             std/sqlite (database)
std/json (serialize)                std/json (serialize)
std/crypto (client-side hash)       std/crypto (auth/tokens)
                    │                       │
                    └──── std/json ─────────┘
                         same types
                         same structs
                         same validation
```

### Shared Code: The Killer Feature

In a typical business app:

```cot
// shared/invoice.cot — used by BOTH client and server
struct Invoice {
    id: i64,
    customer: string,
    items: List(LineItem),
    total: f64,
    status: InvoiceStatus,

    fn validate() ?string {
        if (self.items.count == 0) { return "Invoice must have at least one item" }
        if (self.total < 0) { return "Total cannot be negative" }
        return null
    }
}
```

This struct, this validation, these types — compiled to Wasm for the browser, compiled to native for the server. No duplication. No TypeScript/Go mismatch. No "the client validates differently than the server" bugs.

### What Runs Where

| Component | Browser (Wasm) | Server (native) | Why |
|-----------|---------------|-----------------|-----|
| UI rendering | Yes | No | Canvas is browser-only |
| Form validation | Yes | Yes | Same code both sides |
| Type definitions | Yes | Yes | Shared structs |
| API client | Yes | No | fetch from browser |
| API server | No | Yes | listens on port |
| Database queries | No | Yes | credentials stay server-side |
| Auth/sessions | No | Yes | secrets never in browser |
| Report generation | No | Yes | CPU-heavy, runs in background |
| Offline cache | Yes | No | IndexedDB in browser |
| Real-time updates | Yes (WebSocket client) | Yes (WebSocket server) | Both ends in Cot |
| PDF generation | Either | Either | Pure computation, works both sides |
| Full-text search | No | Yes (SQLite FTS) | Large index stays on server |

### Project Structure

```
my-erp/
  shared/           ← compiles to BOTH targets
    invoice.cot
    customer.cot
    types.cot
    validation.cot
  client/            ← compiles to Wasm
    app.cot
    views/
      dashboard.cot
      invoice_list.cot
      invoice_form.cot
  server/            ← compiles to native
    main.cot
    routes/
      invoices.cot
      customers.cot
      reports.cot
    db/
      migrations.cot
      queries.cot
```

One `cot build` command. Two outputs: `client.wasm` (serve statically) and `server` (run on your box). Same language, same types, same validation logic, zero duplication.

This is what every full-stack framework promises but none deliver cleanly — because they're all two different languages bolted together (TypeScript + Go, TypeScript + Python, Dart + Dart-but-different-on-server). Cot is genuinely one language with two compilation targets, designed from day one for both.

---

## Canvas UI: The shadcn of Wasm

### Why Canvas Over DOM

**DOM is a 1990s document renderer being abused as an app platform.** Every DOM element carries layout, accessibility, style inheritance, event propagation, reflow — hundreds of properties you don't need for an ERP grid showing 10,000 rows. That's why Google moved Docs and Sheets rendering to Canvas.

### Performance Comparison: 10,000 Row ERP Grid

| Approach | Render time | Memory | Scroll perf |
|----------|------------|--------|-------------|
| DOM (React table) | ~2s initial, jank on scroll | 500MB+ (10K DOM nodes) | Poor (reflow on every frame) |
| Virtual DOM (TanStack) | ~200ms, better scroll | 50MB (windowed) | Good (only visible rows in DOM) |
| Canvas (Cot) | ~16ms (one draw call) | 10MB (data only, no DOM) | 60fps (just redraw visible rect) |

Canvas doesn't create DOM nodes for rows. It draws text directly to pixels. Scrolling is just changing the draw offset and redrawing — one frame, 16ms budget, done.

### A Canvas UI Kit for Cot: `std/ui`

**The gap in the market:** No Canvas-rendered, Wasm-native component library exists with a shadcn-level developer experience. Flutter Web is closest but it's Dart-only and heavy.

```cot
import "std/ui"
import "std/ui/components"

fn main() {
    var app = ui.App.init("Acme ERP")
    var root = ui.Column.init()

    // shadcn-style components — Canvas-rendered, not DOM
    var nav = NavBar.init("Acme Corp")
    nav.addItem("Dashboard", icon: "home")
    nav.addItem("Invoices", icon: "file-text", badge: 3)
    nav.addItem("Customers", icon: "users")

    var table = DataTable.init()
    table.columns(["Invoice", "Customer", "Amount", "Status", "Due"])
    table.data(invoices)  // 10,000 rows — Canvas handles this instantly
    table.sortable(true)
    table.filterable(true)
    table.onRowClick(fn(row: int) { openInvoice(row) })

    var sidebar = Card.init()
    sidebar.add(StatCard.init("Revenue", "$142,800", trend: +12.5))
    sidebar.add(StatCard.init("Outstanding", "$23,400", trend: -3.2))

    root.add(nav)
    root.split(sidebar, table, ratio: 0.25)
    app.setRoot(root)
    app.run()
}
```

Every component renders to Canvas. Text drawn with font atlases (same technique as game engines and Ghostty). Layout is flexbox-like but computed in Cot, not by the browser. Hit testing is coordinate-based, not DOM event bubbling.

### Cross-Platform Rendering

On native: `std/ui` renders with Metal (macOS), Vulkan (Linux), Direct3D (Windows).
In browser: `std/ui` renders with WebGPU/Canvas2D.
Same Cot code, same components, different rendering backends.

This is what Ghostty does for terminals — GPU-accelerated rendering on every platform. Cotty already follows this pattern with Metal. The browser version would use WebGPU (shipping in all major browsers now).

### The Opportunity

**A shadcn-quality component library, Canvas-rendered, Wasm-native, cross-platform.** Beautiful defaults (rounded corners, subtle shadows, clean typography), instant performance (no DOM reflow), works everywhere Wasm runs.

For business apps like ERP: forms, data tables, charts, dashboards, reports — ALL better on Canvas than DOM. An ERP doesn't need SEO. It doesn't need accessibility on every internal admin cell (though ARIA annotations via a thin DOM overlay handle compliance). It needs SPEED for large datasets and CONSISTENCY across platforms.

**First mover advantage is real.** Nobody has built this. The market is wide open.
