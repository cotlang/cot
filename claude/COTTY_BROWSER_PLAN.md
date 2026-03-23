# Cotty Browser — WebGPU Canvas Terminal + Editor in Wasm

**Date:** 2026-03-23
**Status:** Planning
**Goal:** Run Cotty in the browser via Wasm + WebGPU, with native desktop via WKWebView/WebKitGTK

---

## Architecture

```
ONE codebase (Cot → Wasm):
  libcotty.wasm + index.html + bridge.js + shaders.wgsl

THREE delivery modes:
  1. Browser:     open URL → full IDE in browser tab
  2. macOS app:   WKWebView loads index.html (WebKit = Safari engine)
  3. Linux app:   WebKitWebView loads index.html (WebKitGTK = same WebKit)

TWO data paths:
  Editor:    WASI filesystem / File System Access API (local files, no server)
  Terminal:  WebSocket to `cot serve` backend (PTY + shell on server)
```

---

## Why WebGPU (Not Canvas2D)

| Approach | Frame time | Technique | GPU? |
|----------|-----------|-----------|------|
| Ghostty (Metal) | ~0.1ms | Glyph atlas + instanced quads | Yes |
| **WebGPU** | **~0.2ms** | **Same glyph atlas + instanced quads** | **Yes** |
| Canvas2D | ~1-2ms | fillText per cell, CPU composited | No |
| DOM/HTML | ~5-10ms | One element per cell, reflow | No |

WebGPU maps directly to Metal (macOS), Vulkan (Linux), D3D12 (Windows).
Inside WKWebView on macOS, WebGPU calls go straight to the same Metal API
that Ghostty uses — zero translation overhead.

---

## What Already Exists (Reuse from libcotty)

All of these compile to Wasm with zero changes:

| Component | File | Lines | Status |
|-----------|------|-------|--------|
| VT100 parser | `src/terminal.cot` | ~3,500 | Complete |
| Terminal grid/cells | `src/terminal.cot` | (included) | Complete |
| Scrollback buffer | `src/terminal.cot` | (included) | Complete |
| Editor (Document, Cursor) | `src/editor.cot` | ~2,000 | Complete |
| Editor input handling | `src/input.cot` | ~500 | Complete |
| Syntax highlighting | `src/treesitter.cot` | ~400 | Complete |
| Command palette | `src/palette.cot` | ~300 | Complete |
| File finder | `src/file_finder.cot` | ~200 | Complete |
| Project search | `src/project_search.cot` | ~300 | Complete |
| Theme system | `src/config.cot` | ~400 | Complete |
| Workspace/tabs | `src/workspace.cot` | ~300 | Complete |
| Split panes | `src/split.cot` | ~200 | Complete |
| Shell integration (VT) | `shell-integration/` | ~100 | Complete |
| **Total reusable** | | **~8,200** | |

---

## What Needs Building

### Phase 1: JS Bridge + Extern Fns (~300 lines Cot + ~200 lines JS)

The Wasm ↔ JS bridge for WebGPU rendering and input:

**Cot extern fn declarations** (in `stdlib/webgpu.cot` or `src/render.cot`):
```cot
// WebGPU rendering
extern fn gpu_init(canvas_id_ptr: i64, canvas_id_len: i64, width: i64, height: i64) i64
extern fn gpu_resize(ctx: i64, width: i64, height: i64) void
extern fn gpu_begin_frame(ctx: i64) void
extern fn gpu_draw_cells(ctx: i64, cells_ptr: i64, cell_count: i64) void
extern fn gpu_end_frame(ctx: i64) void
extern fn gpu_update_atlas(ctx: i64, glyph_ptr: i64, glyph_count: i64) void

// Input (JS → Wasm via exported functions, not imports)
// These are Cot functions exported from Wasm:
export fn cotty_key_down(key_code: i64, modifiers: i64) void
export fn cotty_key_press(char_code: i64) void
export fn cotty_mouse_down(x: i64, y: i64, button: i64) void
export fn cotty_mouse_move(x: i64, y: i64) void
export fn cotty_mouse_up(x: i64, y: i64, button: i64) void
export fn cotty_scroll(dx: i64, dy: i64) void
export fn cotty_resize(width: i64, height: i64) void

// WebSocket (terminal data channel)
extern fn ws_connect(url_ptr: i64, url_len: i64) i64
extern fn ws_send(ws: i64, data_ptr: i64, data_len: i64) void
extern fn ws_close(ws: i64) void
// ws_recv is push-based: JS calls cotty_ws_data(ws, ptr, len) on message

// Timer (requestAnimationFrame loop)
extern fn request_animation_frame() void
// JS calls cotty_frame() each frame

// File System Access API
extern fn fs_open_file_picker() void          // async — calls cotty_file_opened(ptr, len)
extern fn fs_save_file(path_ptr: i64, path_len: i64, data_ptr: i64, data_len: i64) void
extern fn fs_read_file(path_ptr: i64, path_len: i64) void  // async — calls cotty_file_read(ptr, len)
```

**JS bridge** (`bridge.js`):
- Provides the `env` module imports for gpu_*, ws_*, fs_*
- Sets up WebGPU device, pipeline, glyph atlas texture
- Forwards keyboard/mouse events to Wasm exports
- Manages WebSocket connections
- Manages requestAnimationFrame loop

### Phase 2: WebGPU Renderer (~400 lines JS + WGSL shaders)

**Glyph atlas approach** (same as Ghostty `GlyphAtlas.swift`):

1. **Glyph rasterization:** Use OffscreenCanvas + Canvas2D to render each unique
   character at the configured font/size. Pack into a texture atlas.

2. **Cell buffer:** Each frame, Cotty writes a flat array of cell data to Wasm
   linear memory:
   ```
   struct CellGPU {
       row: u16, col: u16,        // grid position
       glyph_index: u16,          // index into atlas
       fg_r: u8, fg_g: u8, fg_b: u8,  // foreground color
       bg_r: u8, bg_g: u8, bg_b: u8,  // background color
       flags: u8,                 // bold, italic, underline, inverse
   }
   ```

3. **Vertex shader:** Expands each cell into a textured quad at the right grid position.

4. **Fragment shader:** Samples the glyph atlas, applies fg/bg colors.

5. **One draw call** per frame renders the entire grid (instanced rendering).

**WGSL shaders** (`shaders/cell.wgsl`):
```wgsl
struct Cell {
    @location(0) pos: vec2<f32>,      // grid row, col
    @location(1) glyph: f32,          // atlas index
    @location(2) fg: vec3<f32>,       // foreground RGB
    @location(3) bg: vec3<f32>,       // background RGB
};

@vertex fn vs_main(@builtin(vertex_index) vi: u32, cell: Cell) -> ... {
    // Compute quad corners from grid position + cell size
    // Compute UV from glyph atlas index
}

@fragment fn fs_main(...) -> @location(0) vec4<f32> {
    // Sample glyph atlas texture
    // Apply foreground color where glyph is white
    // Apply background color where glyph is transparent
}
```

### Phase 3: Cot Render Layer (`src/render_web.cot` ~500 lines)

Replace the FFI export layer that Swift calls with one that writes to the
GPU cell buffer:

```cot
/// Called each frame by requestAnimationFrame (via JS bridge)
export fn cotty_frame() void {
    // Check if terminal surface is dirty
    if (g_surface.checkAndClearDirty() != 0) {
        // Build cell buffer from terminal grid
        var cell_count: i64 = 0
        for row in 0..g_terminal.rows {
            for col in 0..g_terminal.cols {
                const cell = g_terminal.getCell(row, col)
                writeCellGPU(g_cell_buffer, cell_count, row, col, cell)
                cell_count += 1
            }
        }
        // Submit to GPU
        gpu_begin_frame(g_gpu_ctx)
        gpu_draw_cells(g_gpu_ctx, g_cell_buffer, cell_count)
        gpu_end_frame(g_gpu_ctx)
    }
    request_animation_frame()
}
```

### Phase 4: Terminal WebSocket (`src/ws_terminal.cot` ~200 lines)

```cot
/// Connect terminal to a remote shell via WebSocket
fn connectTerminal(url: string) void {
    g_ws = ws_connect(@ptrOf(url), @lenOf(url))
}

/// Called by JS when WebSocket receives data
export fn cotty_ws_data(ws: i64, ptr: i64, len: i64) void {
    // Feed bytes to VT parser (same as PTY read path)
    for i in 0..len {
        g_surface.parser.feed(g_terminal, @intToPtr(*u8, ptr + i).*)
    }
    g_surface.markRenderDirty()
}

/// Called by input handler when user types
fn sendToTerminal(data: string) void {
    ws_send(g_ws, @ptrOf(data), @lenOf(data))
}
```

### Phase 5: `cot serve` Backend (~300 lines Cot)

```cot
/// Server: PTY + WebSocket bridge
/// Usage: cot serve [--port 8080]
fn serve(port: i64) void {
    const server = net_socket(...)
    net_bind(server, port)
    net_listen(server, 128)

    while (true) {
        const client = net_accept(server)
        // WebSocket handshake
        wsHandshake(client)
        // Spawn PTY
        const pty = Pty.spawn(24, 80, "")
        // Bridge: PTY ↔ WebSocket
        bridge(client, pty)
    }
}
```

### Phase 6: Editor File Access (~200 lines)

```cot
/// Open file via File System Access API
export fn cotty_file_opened(ptr: i64, len: i64) void {
    const content = @string(ptr, len)
    g_workspace.openEditorWithContent(content)
}

/// Save file
fn saveCurrentFile() void {
    const doc = g_editor.document
    fs_save_file(@ptrOf(doc.path), @lenOf(doc.path),
                 @ptrOf(doc.content), @lenOf(doc.content))
}
```

### Phase 7: HTML Shell (`index.html` ~50 lines)

```html
<!DOCTYPE html>
<html>
<head>
    <title>Cotty</title>
    <style>
        body { margin: 0; background: #0c0c0c; overflow: hidden; }
        canvas { width: 100vw; height: 100vh; display: block; }
    </style>
</head>
<body>
    <canvas id="cotty"></canvas>
    <script type="module" src="bridge.js"></script>
</body>
</html>
```

### Phase 8: Desktop Wrappers

**macOS** (already have Swift app — add webview mode):
```swift
let webView = WKWebView(frame: window.contentView!.bounds)
webView.loadFileURL(Bundle.main.url(forResource: "index", withExtension: "html")!)
window.contentView = webView
```

**Linux** (already have GTK app — add webkit mode):
```cot
// WebKitWebView widget in GTK
extern fn webkit_web_view_new() i64
extern fn webkit_web_view_load_uri(view: i64, uri_ptr: i64, uri_len: i64) void
```

---

## Compiler Prerequisites

These must work before Cotty Browser can build:

| Feature | Status | Needed For |
|---------|--------|-----------|
| `--target=js` extern fn imports | **In progress** (driver.zig changes) | All GPU/WS/FS calls |
| JS glue with import stubs | **In progress** (js_glue.zig) | Module instantiation |
| `@tagName` in interpolation | **Fixed** (this session) | Debug logging |
| Anonymous enum literal args | **Fixed** (this session) | API calls |
| Enum `== .variant` | **Fixed** (this session) | State checks |
| Multi-file Wasm compilation | **Working** | libcotty is multi-file |
| String passing (ptr+len) | **Working** | All string APIs |

---

## Build & Run

```bash
# Build libcotty to Wasm (JS target — no WASI, browser imports)
cd libcotty
cot build src/web_main.cot --target=js -o dist/cotty.wasm

# Serve locally
python3 -m http.server 8080 -d dist/

# Open browser
open http://localhost:8080

# Or with terminal backend:
cot serve --port 9090 &
open http://localhost:8080?terminal=ws://localhost:9090
```

---

## Implementation Order

```
Phase 1: JS bridge + extern fns     ← Foundation (compiler + bridge.js)
Phase 2: WebGPU renderer            ← See something on screen
Phase 3: Cot render layer           ← Terminal cells → GPU
Phase 4: Terminal WebSocket          ← Type in terminal, see output
Phase 5: cot serve backend           ← Real shell access
Phase 6: Editor file access          ← Open/save files
Phase 7: HTML shell                  ← Package it
Phase 8: Desktop wrappers            ← WKWebView + WebKitGTK
```

**Phase 1-3 = proof of concept.** Render a terminal grid in the browser.
**Phase 4-5 = usable terminal.** Connect to a real shell.
**Phase 6-7 = usable IDE.** Edit files in the browser.
**Phase 8 = desktop app.** Same code, native window.

---

## Effort Estimate

| Phase | New Code | Complexity |
|-------|----------|-----------|
| 1. JS bridge | ~500 lines (Cot + JS) | Medium |
| 2. WebGPU renderer | ~400 lines (JS + WGSL) | High |
| 3. Cot render layer | ~500 lines (Cot) | Medium |
| 4. Terminal WebSocket | ~200 lines (Cot) | Low |
| 5. cot serve | ~300 lines (Cot) | Medium |
| 6. Editor file access | ~200 lines (Cot) | Low |
| 7. HTML shell | ~50 lines | Low |
| 8. Desktop wrappers | ~100 lines | Low |
| **Total** | **~2,250 lines** | |

Most of the logic (8,200 lines) already exists in libcotty.
The new code is just the rendering bridge and network layer.

---

## Success Criteria

1. Terminal grid renders in browser at 60fps via WebGPU
2. Keyboard input works (type commands, see output)
3. WebSocket connects to `cot serve`, shell works
4. Editor opens/saves local files via File System Access API
5. Syntax highlighting works in editor
6. Tabs, split panes, command palette all functional
7. macOS WKWebView wrapper launches as native app
8. Theme system works (Monokai, Catppuccin)
9. Same Wasm binary works in browser AND desktop webview
