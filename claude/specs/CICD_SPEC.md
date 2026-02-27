# CI/CD & Release Pipeline

**Status:** Spec
**Target:** 0.4
**Parallel-safe:** Yes — touches only `.github/`, `install.sh`, `CHANGELOG.md`, `README.md`

---

## Overview

Set up GitHub Actions CI/CD, automated releases with pre-built binaries, and a curl installer. Goal: users can `curl | sh` to install Cot, like Deno.

---

## Current State

- **Version:** `VERSION` file → `build.zig` → `build_options.version` → `cot version` (working)
- **Build:** `zig build` → `zig-out/bin/cot` (single command, ~10-30s)
- **Tests:** `zig build test` (163 compiler tests) + `./test/run_all.sh` (~914 Cot tests across 35 files)
- **Targets:** native, arm64-macos, amd64-linux, wasm32, wasm32-wasi
- **CI:** None
- **Releases:** None
- **Distribution:** Build from source only

---

## Phase 1: CI Testing (every commit)

### `.github/workflows/test.yml`

```yaml
name: Test
on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test-macos:
    runs-on: macos-14  # ARM64 runner
    steps:
      - uses: actions/checkout@v4
      - uses: mlugg/setup-zig@v2
        with:
          version: 0.15.0
      - name: Compiler tests
        run: zig build test
      - name: Build compiler
        run: zig build
      - name: Cot E2E tests (native)
        run: ./test/run_all.sh
      - name: Install wasmtime
        run: curl https://wasmtime.dev/install.sh -sSf | bash
      - name: Cot E2E tests (wasm)
        run: |
          export PATH="$HOME/.wasmtime/bin:$PATH"
          ./test/run_all.sh --target=wasm32

  test-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: mlugg/setup-zig@v2
        with:
          version: 0.15.0
      - name: Compiler tests
        run: zig build test
      - name: Build compiler
        run: zig build
      - name: Cot E2E tests (native)
        run: ./test/run_all.sh
      - name: Install wasmtime
        run: curl https://wasmtime.dev/install.sh -sSf | bash
      - name: Cot E2E tests (wasm)
        run: |
          export PATH="$HOME/.wasmtime/bin:$PATH"
          ./test/run_all.sh --target=wasm32
```

**Notes:**
- `macos-14` gives ARM64 (M1). This matches the primary dev target.
- `ubuntu-latest` tests x64 Linux. The native backend (`amd64-linux`) exercises x64 codegen.
- `mlugg/setup-zig` is the standard Zig GitHub Action. Pin to 0.15.0 (current Zig version).
- Wasm tests need wasmtime installed on the runner.
- `./test/run_all.sh` must exit non-zero on failure (verify this).

### Verify `run_all.sh` exits correctly

The script must `exit $fail_count` or `exit 1` on failure for CI to detect problems. Check and fix if needed.

---

## Phase 2: Release Pipeline (on tag)

### `.github/workflows/release.yml`

```yaml
name: Release
on:
  push:
    tags: ['v*']

permissions:
  contents: write

jobs:
  build:
    strategy:
      matrix:
        include:
          - os: macos-14
            target: aarch64-macos
            artifact: cot-aarch64-macos
          - os: macos-13
            target: x86_64-macos
            artifact: cot-x86_64-macos
          - os: ubuntu-latest
            target: x86_64-linux
            artifact: cot-x86_64-linux
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: mlugg/setup-zig@v2
        with:
          version: 0.15.0

      - name: Build
        run: zig build -Doptimize=ReleaseFast

      - name: Test
        run: |
          zig build test
          ./test/run_all.sh

      - name: Package
        run: |
          cd zig-out/bin
          tar -czf ${{ matrix.artifact }}.tar.gz cot
          shasum -a 256 ${{ matrix.artifact }}.tar.gz > ${{ matrix.artifact }}.tar.gz.sha256

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: ${{ matrix.artifact }}
          path: zig-out/bin/${{ matrix.artifact }}.tar.gz*

  release:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/download-artifact@v4
        with:
          merge-multiple: true

      - name: Create release
        uses: softprops/action-gh-release@v2
        with:
          files: |
            cot-aarch64-macos.tar.gz
            cot-aarch64-macos.tar.gz.sha256
            cot-x86_64-macos.tar.gz
            cot-x86_64-macos.tar.gz.sha256
            cot-x86_64-linux.tar.gz
            cot-x86_64-linux.tar.gz.sha256
          generate_release_notes: true
```

**Notes:**
- `macos-14` = ARM64, `macos-13` = x86_64. Both needed for universal macOS support.
- `ReleaseFast` for smaller, faster binaries in releases.
- SHA256 checksums for each artifact (security best practice).
- `softprops/action-gh-release` creates the GitHub Release with auto-generated notes from commits.
- Tests run before packaging — release only if tests pass.

### Cross-compilation alternative

Zig can cross-compile, so we could build all targets from a single runner:

```bash
zig build -Dtarget=aarch64-macos -Doptimize=ReleaseFast
zig build -Dtarget=x86_64-linux-gnu -Doptimize=ReleaseFast
```

This requires `build.zig` to accept a `-Dtarget` option. Currently it doesn't — this is a prerequisite. If cross-compilation works reliably, we can simplify to a single `ubuntu-latest` runner.

**Prerequisite for cross-compilation:** Add target option to `build.zig`:
```zig
const target = b.standardTargetOptions(.{});
const exe = b.addExecutable(.{
    .name = "cot",
    .target = target,
    ...
});
```

---

## Phase 3: Installer Script

### `install.sh`

```bash
#!/bin/sh
set -e

# Cot installer — downloads pre-built binary from GitHub Releases
# Usage: curl -fsSL https://cot.dev/install.sh | sh

COT_DIR="${COT_INSTALL_DIR:-$HOME/.cot}"
BIN_DIR="$COT_DIR/bin"

OS=$(uname -s)
ARCH=$(uname -m)

case "$OS" in
    Darwin) OS_NAME="macos" ;;
    Linux)  OS_NAME="linux" ;;
    *)      echo "Error: unsupported OS: $OS"; exit 1 ;;
esac

case "$ARCH" in
    arm64|aarch64) ARCH_NAME="aarch64" ;;
    x86_64|amd64)  ARCH_NAME="x86_64" ;;
    *)              echo "Error: unsupported architecture: $ARCH"; exit 1 ;;
esac

# Fetch latest version from GitHub API
if [ -z "$COT_VERSION" ]; then
    COT_VERSION=$(curl -fsSL https://api.github.com/repos/cotlang/cot/releases/latest | grep '"tag_name"' | sed 's/.*"v\(.*\)".*/\1/')
fi

FILENAME="cot-${ARCH_NAME}-${OS_NAME}.tar.gz"
URL="https://github.com/cotlang/cot/releases/download/v${COT_VERSION}/${FILENAME}"

echo "Installing cot ${COT_VERSION} (${ARCH_NAME}-${OS_NAME})..."

mkdir -p "$BIN_DIR"
curl -fsSL "$URL" | tar -xz -C "$BIN_DIR"
chmod +x "$BIN_DIR/cot"

echo ""
echo "Cot ${COT_VERSION} installed to ${BIN_DIR}/cot"
echo ""

# Check if BIN_DIR is in PATH
case ":$PATH:" in
    *":$BIN_DIR:"*) ;;
    *)
        echo "Add to your shell profile:"
        echo ""
        echo "  export PATH=\"${BIN_DIR}:\$PATH\""
        echo ""
        ;;
esac
```

**Environment variables:**
- `COT_INSTALL_DIR` — override install location (default: `~/.cot`)
- `COT_VERSION` — pin to specific version (default: latest)

---

## Phase 4: Version Bump & Release Process

### Manual release workflow (for 0.4)

```bash
# 1. Update version
echo "0.4.0" > VERSION

# 2. Update CHANGELOG.md
# (manually write release notes)

# 3. Commit and tag
git add VERSION CHANGELOG.md
git commit -m "Release 0.4.0"
git tag v0.4.0

# 4. Push (triggers CI release)
git push origin main --tags
```

### CHANGELOG.md format

```markdown
# Changelog

## [0.4.0] - 2026-02-XX

### Added
- `cot mcp` subcommand — compiler-powered MCP server for Claude Code
- CI/CD pipeline with GitHub Actions
- Pre-built binaries for macOS (ARM64, x64) and Linux (x64)
- Curl installer: `curl -fsSL https://cot.dev/install.sh | sh`

### Changed
- ...

### Fixed
- ...

## [0.3.1] - 2026-02-11

### Added
- Buffered I/O (`stdlib/io.cot`)
- JSON parser + encoder (`stdlib/json.cot`)
- WasmGC (wasm32 target now uses WasmGC by default)
- Comptime evaluation (Tier 1 + Tier 2)
- Sort stdlib module

### Fixed
- ARC scope_destroy: use `deinit` not `free` for automatic cleanup
- String/slice compound reassignment
- x64 codegen: epilogue callee-save restoration, shift/cmov, fneg/fabs/fcmp
```

---

## Phase 5: README Update

Add to `README.md`:

```markdown
## Installation

### Quick Install (macOS / Linux)

```sh
curl -fsSL https://cot.dev/install.sh | sh
```

### From GitHub Releases

Download the latest binary from [GitHub Releases](https://github.com/cotlang/cot/releases).

### Build from Source

Requires [Zig 0.15+](https://ziglang.org/download/).

```sh
git clone https://github.com/cotlang/cot.git
cd cot
zig build
./zig-out/bin/cot version
```
```

---

## Future Phases (post-0.4)

### Homebrew (0.5)

Create `homebrew-cot` tap repo:
```ruby
class Cot < Formula
  desc "Compiled language for full-stack web development"
  homepage "https://cot.dev"
  url "https://github.com/cotlang/cot/releases/download/v0.5.0/cot-aarch64-macos.tar.gz"
  sha256 "..."
  def install
    bin.install "cot"
  end
  test do
    assert_match "cot", shell_output("#{bin}/cot version")
  end
end
```

Usage: `brew install cotlang/tap/cot`

### npm wrapper (0.5)

Like Deno's `deno-npm` package — a thin npm package that downloads the binary:
```
npx cot run app.cot
```

This targets the web developer audience who already have Node.js.

### Performance benchmarks in CI (0.6)

Track compile times across versions. Alert on >10% regression.

### Signed binaries (1.0)

macOS notarization + Linux GPG signing for production trust.

---

## Prerequisites / Blockers

1. **`build.zig` target option** — Need `b.standardTargetOptions()` for cross-compilation in release builds. Without this, must build on each platform natively (the matrix approach above handles this).

2. **`run_all.sh` exit code** — Must exit non-zero on test failure. Verify.

3. **`run_all.sh --target=wasm32`** — Must support `--target` flag to pass through to `cot test`. Verify or add.

4. **GitHub repo settings** — Enable Actions (should be enabled by default on public repos).

---

## Files Created/Modified

| File | Action | Description |
|------|--------|-------------|
| `.github/workflows/test.yml` | CREATE | CI test on every commit |
| `.github/workflows/release.yml` | CREATE | Build + release on tag |
| `install.sh` | CREATE | Curl installer |
| `CHANGELOG.md` | CREATE | Release notes |
| `README.md` | MODIFY | Add install instructions |
| `build.zig` | MODIFY | Add `standardTargetOptions` (optional, for cross-compile) |
| `test/run_all.sh` | VERIFY | Exit code on failure, `--target` flag |
