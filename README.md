# Cot

A compiler toolkit for business-oriented languages.

## Build

Requires Zig 0.15.2.

```
zig build
```

## Run

```
./zig-out/bin/cot --help
```

## Structure

- `src/` - Compiler, VM, framework
- `src/runtime/` - Bytecode VM
- `src/tui_ext/` - TUI extension
- `src/cotdb/` - ISAM database
- `src/dbl/` - DBL syntax frontend
- `tools/lsp/` - Language server (needs rewrite)
