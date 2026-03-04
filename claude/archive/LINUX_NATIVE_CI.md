# Linux Native Compilation — FIXED

## What Changed

The `Linker failed: error.FileNotFound` on x86_64-linux was caused by `cot` hardcoding `zig cc` as the linker. Standalone installs (via `install.sh`) didn't include Zig.

Two fixes shipped together:

1. **`compiler/main.zig`** — Linker fallback: tries `zig cc` first, falls back to system `cc` for same-target builds. Cross-compilation still requires `zig cc`. Clear error messages if no linker is found.

2. **`install.sh`** — Now installs Zig 0.15.2 alongside cot into `~/.cot/zig/`, with a symlink at `~/.cot/bin/zig`. Skips if zig is already on PATH.

## For `cot.land/pkg` CI (GitHub Actions ubuntu-latest)

The deploy workflow needs zig available for linking. Two options:

### Option A: Use install.sh (recommended)

`install.sh` now handles everything — installs both cot and zig:

```yaml
- name: Install cot
  run: curl -fsSL https://raw.githubusercontent.com/cotlang/cot/main/install.sh | sh
  env:
    COT_VERSION: "0.3.5"  # or omit for latest

- name: Add to PATH
  run: echo "$HOME/.cot/bin" >> $GITHUB_PATH
```

### Option B: Install zig separately

If the workflow already installs cot manually (e.g. downloading the tarball directly), just ensure zig or cc is on PATH:

```yaml
- uses: mlugg/setup-zig@v2
  with:
    version: 0.15.2
```

Or rely on the system `cc` fallback — ubuntu-latest has `gcc` pre-installed, so `cc` is available. No extra step needed as long as cot is built from a version that includes the linker fallback (any release after this fix).

## Linker Behavior

| Scenario | What happens |
|----------|-------------|
| `zig` on PATH | Uses `zig cc` (preferred, bundles libc) |
| No `zig`, `cc` on PATH, same-target | Uses system `cc` (gcc/clang) |
| No `zig`, cross-compilation | Error: "cross-compilation requires 'zig'" |
| No `zig`, no `cc` | Error: "no linker found" |

## Status

All paths verified on macOS ARM64. The `cc` fallback works for same-target native builds. ubuntu-latest has `gcc` pre-installed so both `zig cc` and `cc` paths work on GitHub Actions Linux runners.
