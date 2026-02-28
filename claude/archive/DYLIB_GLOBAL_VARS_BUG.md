# Bug: Mutable Global Variable Writes Don't Persist in Dylib Mode

**Status**: SUPERSEDED by `STRUCT_GLOBAL_ASSIGNMENT_BUG.md` — root cause was `convertGlobalStore` using single 8-byte `.store` for struct globals > 8 bytes. Scalar globals were never broken. Fix applied to `ssa_builder.zig`.

## Summary

When building with `--lib` (shared library / dylib), runtime writes to mutable global variables (`var`) don't persist across function calls. The write executes but subsequent reads return 0 (the initial zero-initialized value).

## Reproduction

```cot
// config.cot
var THEME_FG_R: i64 = 0

fn ensureThemeReady() void {
    THEME_FG_R = 217
}

fn readThemeFgR() i64 {
    return THEME_FG_R
}
```

```cot
// ffi.cot
import "config"

export fn init() void {
    ensureThemeReady()
    // At this point THEME_FG_R should be 217
}

export fn get_fg_r() i64 {
    return readThemeFgR()  // Returns 0 instead of 217
}
```

```bash
cot build main.cot --lib -o libtest.dylib
# Call init() then get_fg_r() from C/Swift → returns 0
```

## Evidence from Cotty

- `ensureThemeReady()` writes `THEME_FG_R = 217` and `THEME_BG_R = 12`
- `nm libcotty.dylib` shows exactly one `_THEME_FG_R` symbol (no duplicates)
- `cotty_config_fg_r()` returns 217 (reads from `g_app.config.fg_r` struct field — works fine)
- But `resetAttributes()` reading `THEME_FG_R` gets 0
- All unit tests pass (`cot test` compiles as a single binary, not a dylib)
- Bug only manifests in `--lib` dylib builds

## Compiler-Side Investigation (Feb 28, 2026)

### Codegen Path Verified Correct

The full codegen path for global variables in dylib mode was traced and found to be correct:

1. **IR**: `var` declarations → `ir.Global` → zero-initialized data section entries (`driver.zig:1334-1348`, `.Local` linkage, `writable=true`)
2. **SSA**: Global reads/writes → `global_addr` + `load`/`store` ops (`ssa_builder.zig:701-755`)
3. **CLIF**: `global_addr` → `GlobalValue` with `colocated=true`, symbol reference via `global_symbol_map` (`ssa_to_clif.zig:789-816`)
4. **ARM64**: `colocated=true` → `load_ext_name_near` → ADRP + ADD pair (`isa/aarch64/lower.zig:2945-2966`)
5. **Relocations**: `ARM64_RELOC_PAGE21` (pc_rel) + `ARM64_RELOC_PAGEOFF12` (`macho.zig:41-42`)
6. **Object file**: Data goes into `__DATA,__data` section (writable), symbols registered with `SECT_DATA`

### Disassembly Confirms Correct Addressing

Built the reproduction case and disassembled the dylib. Both writer and reader compute the same address:

```
_ensureThemeReady:
    mov  x1, #0xd9              ; 217
    adrp x2, 0x8000             ; page of _THEME_FG_R
    add  x2, x2, #0x100        ; x2 = 0x8100 = exact address
    str  x1, [x2]              ; store 217

_get_fg_r (or readThemeFgR):
    adrp x1, 0x8000             ; same page
    add  x1, x1, #0x100        ; x1 = 0x8100 = same address
    ldr  x0, [x1]              ; load from same address
```

`nm` output: `0000000000008100 d _THEME_FG_R` — single symbol, correct section.

### All Test Scenarios Pass

Tested with C host (`dlopen` + `dlsym`), all return correct values:

| Scenario | Result |
|----------|--------|
| Single-file dylib: `set_value(42)` then `get_value()` | Returns 42 |
| Multi-file: global in config.cot, read via wrapper fn in main.cot | Returns 217 |
| Multi-file: global in config.cot, read directly from importing file | Returns 217 |
| Three files, 7 globals, deep import chain (config→sgr→main) | Returns 217 |

### Conclusion: Bug Is NOT in the Cot Compiler's Codegen

The compiler's global variable addressing in dylib mode is correct. ADRP+ADD with PAGE21/PAGEOFF12 relocations works for PIC shared libraries. The original hypotheses (GOT/PIC addressing, relocation type, TLS) are ruled out.

## Likely Root Cause — Cotty-Side

The bug is likely in the Cotty project, not the Cot compiler. Investigate these in the Cotty codebase:

1. **Call ordering**: Is `ensureThemeReady()` actually called before `resetAttributes()`? Add a print/debug statement in `ensureThemeReady()` to confirm it runs. The struct field (`g_app.config.fg_r`) returning 217 suggests the init DOES run, but maybe `resetAttributes()` is called from a path that bypasses the init check.

2. **Global shadowing**: Does any other file re-declare `var THEME_FG_R: i64 = 0`? The Cot compiler would create a separate data section entry per declaration. Both would show as `_THEME_FG_R` in nm but at different offsets. Check `nm libcotty.dylib | grep THEME_FG_R` for duplicate addresses.

3. **Stale global after re-init**: Does anything reset `THEME_READY` back to 0, causing `ensureThemeReady()` to be a no-op on subsequent calls? Or does something zero out the globals region?

4. **Swift vs C host**: If calling from Swift, check that the function signatures match exactly. Swift may pass hidden arguments or use different calling conventions for certain return types. Test with a plain C host first to isolate.

5. **`resetAttributes()` code path**: Verify that `resetAttributes()` reads from the global `THEME_FG_R` and not from a local copy, a function parameter, or a cached value. Disassemble `_resetAttributes` in `libcotty.dylib` and confirm it uses ADRP+ADD to the same address as `_ensureThemeReady`.

### Quick Diagnostic Commands

```bash
# Check for duplicate global symbols (different addresses = bug)
nm libcotty.dylib | grep THEME_FG_R

# Disassemble the writer and reader — verify same ADRP+ADD target
objdump -d libcotty.dylib | grep -A20 '<_ensureThemeReady>:'
objdump -d libcotty.dylib | grep -A20 '<_resetAttributes>:'

# Test with plain C host to rule out Swift calling convention issues
# (see test.c pattern above)
```

## Impact

Blocks Cotty's config/theme system. All theme colors read as 0 (black) in the terminal because SGR reset reads default colors from globals that were written but didn't persist.

## Workaround

None acceptable per CLAUDE.md. The correct Cot code (`var THEME_FG_R: i64 = 0` + runtime write) should work in dylib mode.
