# Stdlib Distribution — Release Tarball & CI/CD Gap

## Problem

The `install.sh` script downloads only the `cot` binary. The stdlib (`cotlang/std`) is a separate git submodule not included in release tarballs. Users and CI pipelines must separately `git clone https://github.com/cotlang/std.git` to get the stdlib, and there's no way to pin a compatible version.

This was discovered setting up CI/CD for `cot.land/pkg` — the GitHub Actions workflow needs both the compiler and a matching stdlib, but the install script only provides the compiler.

### Current stdlib resolution (driver.zig:826-853)

```
1. COT_STDLIB env var (explicit override)
2. Walk up from source file looking for stdlib/ directory
3. CWD fallback (./stdlib/)
```

### Current install.sh output

```
~/.cot/bin/cot     # binary only — no stdlib
```

## Proposal: Bundle stdlib in release tarball

**Reference:** Zig ships `lib/std/` alongside the binary. Go ships the entire standard library. Rust ships libstd via rustup. Every mature compiled language bundles its stdlib with the compiler.

### Changes

**1. Release workflow (`.github/workflows/release.yml`)**

Include the stdlib submodule contents in the tarball:

```yaml
- name: Package
  run: |
    mkdir -p dist/lib
    cp zig-out/bin/cot dist/
    cp -r stdlib dist/lib/std
    cd dist && tar -czf ../${{ matrix.artifact }}.tar.gz .
```

Tarball contents become:
```
cot                    # compiler binary
lib/std/string.cot     # stdlib modules
lib/std/list.cot
lib/std/json.cot
...
```

**2. Install script (`install.sh`)**

Extract to `~/.cot/` so the layout becomes:
```
~/.cot/bin/cot
~/.cot/lib/std/string.cot
~/.cot/lib/std/list.cot
...
```

**3. Compiler stdlib resolution (driver.zig)**

Add a new tier: resolve relative to the compiler binary's own path (like Zig's `findZigLibDirFromSelfExe`):

```
1. COT_STDLIB env var
2. Walk up from source file looking for stdlib/
3. <compiler_binary>/../lib/std/        ← NEW
4. CWD fallback (./stdlib/)
```

This makes `curl | sh` fully self-contained — install once, everything works.

**4. Project-local stdlib still takes priority**

Projects with a local `stdlib/` symlink or directory (like `cot.land/pkg` and `cotty`) continue to work unchanged, because tier 2 (walk-up) fires before tier 3 (binary-relative).

## Alternative: Tag the std repo

If bundling is too much for 0.3.5, a lighter fix: tag `cotlang/std` with `v0.3.5` alongside each compiler release. CI workflows can then:

```yaml
- run: git clone --depth 1 --branch v0.3.5 https://github.com/cotlang/std.git stdlib
```

This is how Go handled stdlib before modules — the stdlib repo was tagged in lockstep with the compiler. It works but requires users to know the version pairing.

## Target: 0.3.5

All three changes (tarball bundling, install script, binary-relative resolution) ship in 0.3.5.

## Impact

- `cot.land/pkg` CI/CD workflow works out of the box once 0.3.5 is released
- Any future project using `cot` from GitHub Actions or Docker benefits
- `install.sh` becomes a true one-liner install (no second git clone step)
