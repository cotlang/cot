# Build System Plan: Three Libraries with Shared Foundation

**Date:** 2026-03-29
**References:** Zig dep_diamond test pattern, Roc compiler crate architecture

---

## The Problem

libcot and libcir both need `types.zig` and `source.zig`. Zig 0.15 requires that shared files are imported as **named modules** created once in the build system — not as file paths from multiple modules.

## The Solution: Diamond Dependency Pattern

Following Zig's own `dep_diamond` test pattern and Roc's `roc_types`/`roc_module` shared crates:

```
            ┌──────────────────┐
            │   cot-foundation  │
            │   (shared types)  │
            │                   │
            │   types.zig       │
            │   source.zig      │
            │   token.zig       │
            │   errors.zig      │
            │   target.zig      │
            │   debug.zig       │
            │   comptime.zig    │
            └────────┬──────────┘
                     │
            ┌────────┴────────┐
            ▼                 ▼
    ┌──────────────┐  ┌──────────────┐
    │   libcot      │  │   libcir      │
    │   (frontend)  │  │   (IR+passes) │
    │               │  │               │
    │   ast.zig     │  │   ir.zig      │
    │   parser.zig  │  │   ssa/*.zig   │
    │   scanner.zig │  │   arc.zig     │
    │   checker.zig │  │   vwt.zig     │
    │   lower.zig   │  │   wasm/*.zig  │
    │   formatter.zig│  │               │
    └──────┬────────┘  └──────┬────────┘
           │                  │
           └────────┬─────────┘
                    ▼
            ┌──────────────┐
            │   cot (CLI)   │
            │   driver.zig  │
            │   cli.zig     │
            └──────────────┘
```

**cot-foundation** is the shared module that both libraries depend on. It contains types that cross the library boundary: `TypeIndex`, `TypeRegistry`, `BasicKind`, `Span`, `Pos`, `Token`, `ErrorReporter`, `Target`, `ComptimeValue`.

This matches Roc's pattern where `roc_types` and `roc_module` are shared crates imported by both the frontend (`roc_can`, `roc_solve`) and the backend (`roc_gen_llvm`, `roc_gen_wasm`).

---

## File Organization

```
src/
├── build.zig                        Root build — wires everything together
│
├── foundation/                      Shared types (both libs depend on this)
│   ├── types.zig                    ← from libcot-zig/
│   ├── source.zig                   ← from libcot-zig/
│   ├── token.zig                    ← from libcot-zig/
│   ├── errors.zig                   ← from libcot-zig/
│   ├── target.zig                   ← from libcot-zig/
│   ├── debug.zig                    ← from libcot-zig/
│   └── comptime.zig                 ← from libcot-zig/
│
├── libcot-zig/                      Frontend (language-specific)
│   ├── lib.zig                      Library root
│   ├── ast.zig                      Imports foundation via @import("types") etc.
│   ├── parser.zig
│   ├── scanner.zig
│   ├── checker.zig
│   ├── lower.zig                    Imports ir via @import("ir")
│   └── formatter.zig
│
├── libcir-zig/                      IR + passes (language-agnostic)
│   ├── lib.zig                      Library root
│   ├── ir.zig                       Imports foundation via @import("types") etc.
│   ├── arc_insertion.zig
│   ├── ssa_builder.zig
│   ├── vwt_gen.zig
│   ├── ssa/
│   └── wasm/
│
├── libclif-zig/                     Native backend (future)
│   └── ...
│
└── include/                         C ABI headers
    ├── cot.h
    ├── cir.h
    └── clif.h
```

---

## Root build.zig

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Foundation module — shared types that cross library boundaries
    const foundation = b.createModule(.{
        .root_source_file = b.path("foundation/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    // libcir module — imports foundation for types/source
    const libcir = b.createModule(.{
        .root_source_file = b.path("libcir-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    libcir.addImport("foundation", foundation);

    // libcot module — imports foundation + libcir
    const libcot = b.createModule(.{
        .root_source_file = b.path("libcot-zig/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    libcot.addImport("foundation", foundation);
    libcot.addImport("cir", libcir);

    // Tests for each library
    const test_foundation = b.addTest(.{ .root_module = foundation });
    const test_libcir = b.addTest(.{ .root_module = libcir });
    const test_libcot = b.addTest(.{ .root_module = libcot });

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(test_foundation).step);
    test_step.dependOn(&b.addRunArtifact(test_libcir).step);
    test_step.dependOn(&b.addRunArtifact(test_libcot).step);
}
```

---

## Import Pattern in Source Files

```zig
// In libcot-zig/lower.zig:
const types = @import("foundation").types;   // shared types
const source = @import("foundation").source; // shared source
const ir = @import("cir").ir;                // from libcir

// In libcir-zig/ir.zig:
const types = @import("foundation").types;   // same shared instance
const source = @import("foundation").source; // same shared instance

// In libcot-zig/ast.zig:
const types = @import("foundation").types;   // same shared instance
```

The key: `@import("foundation")` in both libcot and libcir resolves to the **same module instance** because `foundation` was created once in the build system and added to both. Type identity is preserved.

---

## foundation/lib.zig

```zig
//! Shared foundation types for the Cot compiler.
//! Both libcot (frontend) and libcir (IR) depend on these.

pub const types = @import("types.zig");
pub const source = @import("source.zig");
pub const token = @import("token.zig");
pub const errors = @import("errors.zig");
pub const target = @import("target.zig");
pub const debug = @import("debug.zig");
pub const comptime_val = @import("comptime.zig");
```

---

## Migration Steps

1. **Create `src/foundation/` directory** and move 7 shared files there
2. **Create `foundation/lib.zig`** re-exporting all shared modules
3. **Create `src/build.zig`** with the diamond dependency wiring
4. **Update all `@import("types.zig")` to `@import("foundation").types`** in libcot and libcir files
5. **Move ir.zig to libcir-zig/** — it can now import foundation properly
6. **Verify: `zig build test` passes for all three modules**

The migration is mechanical — find-and-replace `@import("types.zig")` with `@import("foundation").types` across all files. No logic changes.

---

## What This Enables

- **ir.zig in libcir-zig** — properly separated, imports types via foundation
- **libcir can be built independently** — `zig build test -Dlibcir` tests only the IR library
- **Another frontend** can depend on foundation + libcir without touching libcot
- **C ABI boundary** sits between libcot.lower.zig and libcir.ir.zig — clean separation
- **Incremental compilation** — changing a libcot file doesn't recompile libcir

---

## Why Foundation, Not Just Duplicating

Roc's `roc_types` crate is used by 15+ other crates. Duplicating it would mean maintaining N copies. The foundation module is the same idea — one source of truth for types that cross boundaries.

The 7 files in foundation (2,728 lines) are all stable leaf modules with zero coupling to frontend or IR logic. They define data types and utilities, not algorithms.
