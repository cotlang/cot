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
