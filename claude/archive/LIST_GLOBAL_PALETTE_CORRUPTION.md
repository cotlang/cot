# Bug: Global Struct Assignment Still Broken — `THEME_PALETTE = cfg.palette` Writes Address Instead of Value

**Status**: FIXED — root cause was missing VOID off_ptr fallback in `convertGlobalStore`
**Related**: `STRUCT_GLOBAL_ASSIGNMENT_BUG.md` (first fix handled typed values; this fix adds the VOID off_ptr edge case)
**Priority**: Critical — Cotty terminal is unusable without palette colors

---

## 1. The Bug in One Sentence

`THEME_PALETTE = cfg.palette` (a `List(i64)` global assignment, 24 bytes) is still emitting a single 8-byte `.store` instead of a 24-byte `.move`, causing `THEME_PALETTE.items` to contain the **address of the source struct** rather than its value, while `count` and `capacity` retain stale values from a prior `ensureThemeReady()` call.

---

## 2. Proof: Debug Output

```
[DEBUG] palette_count=48 default_fg_r=217 current_fg_r=217
[PAL] 41956018072 48 64 | 1 0 7304276993 | 0 0 0 | 0 0 0 | 0 0 0 | 0 0 0 | 0 0 0 |
```

- `palette_count=48` → `terminal.palette.count` is correct (48 entries expected)
- `palette.get(0) = 41956018072` → this is NOT a color value (should be 26). It's a **heap address** — specifically `cfg.palette.items` (the source struct's items pointer)
- `palette.get(1) = 48` → this is `cfg.palette.count`, NOT color data
- `palette.get(2) = 64` → this is `cfg.palette.capacity`, NOT color data
- `palette.get(3..20)` → garbage from memory past the 24-byte List struct

The palette's `items` pointer is pointing at the **List struct itself** (in `g_app.config.palette`), not at the items array. So `palette.get(N)` dereferences the List struct fields as if they were color data.

---

## 3. Exact Data Flow (Where Corruption Happens)

### Step 1: `ensureThemeReady()` (config.cot:299-312)

```cot
fn ensureThemeReady() void {
    if (THEME_READY == 1) { return }
    THEME_PALETTE.items = 0        // field store (scalar, works)
    THEME_PALETTE.count = 0        // field store (scalar, works)
    THEME_PALETTE.capacity = 0     // field store (scalar, works)
    appendPaletteDefaults(THEME_PALETTE)  // pass by ref, fills heap array
    THEME_READY = 1
}
```

After this: `THEME_PALETTE = {items: <heap_ptr_A>, count: 48, capacity: 64}`
All 3 fields are correct because they were written individually (scalar field stores always work).

### Step 2: `initThemeDefaults()` (config.cot:316-325)

```cot
fn initThemeDefaults(cfg: *Config) void {
    ensureThemeReady()            // sets THEME_READY=1, fills palette from defaults
    THEME_FG_R = cfg.fg_r        // scalar global store (works)
    THEME_FG_G = cfg.fg_g        // scalar global store (works)
    // ...
    THEME_PALETTE = cfg.palette   // ← BUG: 24-byte struct, only 8 bytes written
}
```

**What should happen**: Copy 24 bytes from `cfg.palette` to `THEME_PALETTE`:
- items = cfg.palette.items (heap address of color data)
- count = cfg.palette.count (48)
- capacity = cfg.palette.capacity (64)

**What actually happens**: `.store` writes only 8 bytes — the **address** of `cfg.palette` — into bytes 0-7 of `THEME_PALETTE`:
- items = **address of cfg.palette struct** (wrong — this is where the List struct lives, not the array)
- count = 48 (leftover from Step 1's `appendPaletteDefaults`, NOT from the assignment)
- capacity = 64 (leftover from Step 1, NOT from the assignment)

### Step 3: `TerminalState.init()` (terminal.cot:179-212)

```cot
static fn init(rows: i64, cols: i64) TerminalState {
    ensureThemeReady()        // THEME_READY=1, returns immediately
    return TerminalState {
        // ...
        palette: THEME_PALETTE,   // copies the corrupted global
        // ...
    }
}
```

Copies the corrupted `THEME_PALETTE` into the terminal state. Now `terminal.palette.items` points at `g_app.config.palette` (the List struct), not at the actual color array.

### Step 4: VT Parser reads palette (vt_parser.cot)

```cot
fn basicColor(idx: i64, palette: *List(i64)) (i64, i64, i64) {
    const base = idx * 3
    return (palette.get(base), palette.get(base + 1), palette.get(base + 2))
}
```

Dereferences the corrupted items pointer → reads List struct fields as color values → returns garbage → all ANSI colors render as (0,0,0) or garbage → black-on-black terminal.

---

## 4. Why `count` Appears Correct but `items` Is Wrong

This is the key insight. It's NOT that the assignment partially worked. The assignment wrote **only 8 bytes** (the `.store` bug), overwriting only `THEME_PALETTE.items` (bytes 0-7). But `ensureThemeReady()` had already set count=48 and capacity=64 via field-by-field stores in the same function call (lines 307-310). Those stale values were **never overwritten** because the `.store` only touched bytes 0-7.

So:
- `items` = corrupted (overwritten with source address by buggy `.store`)
- `count` = 48 (stale from `ensureThemeReady`, looks correct by coincidence)
- `capacity` = 64 (stale from `ensureThemeReady`)

This makes the bug look like "items is stale but count/capacity are fine," but in reality **none of the 3 fields were correctly assigned** by `THEME_PALETTE = cfg.palette`.

---

## 5. Relationship to STRUCT_GLOBAL_ASSIGNMENT_BUG.md

That document describes this exact codegen issue in `convertGlobalStore` (ssa_builder.zig:745-755) and provides a detailed fix. It's marked "FIXED."

**Possible explanations:**
1. The fix was applied to source but the compiler wasn't rebuilt (`zig build`)
2. The fix was applied but has a regression or edge case
3. The fix was applied to a different branch

**Action needed:** Verify that `convertGlobalStore` in the **active compiler binary** includes the size-aware `.move` branching described in that document. Rebuild the compiler if needed:

```bash
cd ~/cotlang/cot
zig build
```

Then rebuild Cotty:
```bash
cd ~/cot-land/cotty
cot build src/ffi.cot --lib -o libcotty.dylib
```

---

## 6. Verification After Fix

```bash
# 1. Rebuild compiler
cd ~/cotlang/cot && zig build

# 2. Rebuild Cotty dylib
cd ~/cot-land/cotty && cot build src/ffi.cot --lib -o libcotty.dylib

# 3. Verify with objdump — initThemeDefaults should have 3x LDR/STR pairs
objdump -d libcotty.dylib | grep -A30 '_initThemeDefaults'
# EXPECTED: 3 load/store pairs (24 bytes = items + count + capacity)
# BROKEN: single STR (8 bytes = just the source address)

# 4. Rebuild macOS app
cd macos && swift build

# 5. Run Cotty — terminal text should have colored prompt
# zsh sends ESC[36m (cyan) for directory name, ESC[32m (green) for prompt arrow
# These should now render in correct Monokai palette colors instead of black
```

---

## 7. Minimal Reproduction

```cot
import "std/list"

var GLOBAL_LIST: List(i64) = undefined

fn setup() void {
    GLOBAL_LIST.items = 0
    GLOBAL_LIST.count = 0
    GLOBAL_LIST.capacity = 0
    GLOBAL_LIST.append(111)
    GLOBAL_LIST.append(222)
    GLOBAL_LIST.append(333)
}

fn overwrite(src: *List(i64)) void {
    GLOBAL_LIST = src.*    // Bug: writes 8 bytes (address) not 24 bytes (value)
}

test "global list assignment copies all fields" {
    setup()
    // GLOBAL_LIST = {items: <heap_A>, count: 3, capacity: 8}

    var other: List(i64) = undefined
    other.items = 0
    other.count = 0
    other.capacity = 0
    other.append(999)
    other.append(888)
    // other = {items: <heap_B>, count: 2, capacity: 8}

    overwrite(&other)
    // After fix: GLOBAL_LIST = {items: <heap_B>, count: 2, capacity: 8}
    // Bug: GLOBAL_LIST = {items: <addr_of_other>, count: 3, capacity: 8}
    //                     ^^^ address, not value    ^^^ stale from setup()

    @assertEq(GLOBAL_LIST.count, 2)     // FAILS: returns 3 (stale)
    @assertEq(GLOBAL_LIST.get(0), 999)  // FAILS: returns garbage
    @assertEq(GLOBAL_LIST.get(1), 888)  // FAILS: returns garbage
}
```

---

## 8. Impact on Cotty

Without this fix, the terminal renders all ANSI-colored text as black-on-black:
- Shell prompt (green arrow, cyan directory) → invisible
- `ls --color` output → invisible
- Git branch coloring → invisible
- Any program using SGR color codes 30-37, 40-47, 90-97, 100-107 → wrong colors

24-bit color (`ESC[38;2;R;G;Bm`) works correctly because it bypasses the palette entirely.
Default foreground (`ESC[0m` reset) works correctly because it uses scalar globals (`THEME_FG_R`, etc.), not the List.
