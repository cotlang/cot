# Checker Refactor Plan: Split Into Focused Modules

**Date:** 2026-03-29
**Status:** Ready to execute during port
**Source:** `compiler/frontend/checker.zig` (5,162 lines, 122 functions)
**Target:** `src/libcot-zig/checker.zig` (~3,270 lines) + 4 submodules (~1,890 lines)
**Reference:** Zig's Sema.zig (37,745 lines core + 4 submodules in Sema/)

---

## Rationale

5,162 lines in a single file is manageable but hides four distinct responsibilities that have clean boundaries. Zig's own type checker (Sema) follows the same pattern: a large core orchestrator with specialized domains extracted into submodules (`Sema/arith.zig`, `Sema/bitcast.zig`, `Sema/comptime_ptr_access.zig`).

The refactor is non-breaking — functions move files, signatures stay identical.

---

## File Structure

```
src/libcot-zig/
├── checker.zig                    (~3,270 lines) Core type checker
├── checker/
│   ├── builtins.zig               (~600 lines) Builtin call dispatch
│   ├── comptime_eval.zig          (~730 lines) Compile-time evaluation
│   ├── diagnostics.zig            (~290 lines) Error suggestions
│   └── generics.zig               (~270 lines) Generic instantiation
```

---

## What Moves Where

### checker/builtins.zig (~600 lines)

All builtin function type checking. Currently the largest single function in the file.

| Function | Lines | What it does |
|----------|-------|-------------|
| `checkBuiltinCall` | 426 | Dispatch on BuiltinKind, validate args, return result type |
| `checkBuiltinLen` | 8 | @len special case |
| `checkBuiltinAppend` | 15 | @append special case |
| `checkCall` (builtin path) | ~150 | Builtin-specific call resolution |

**Why extract:** Pure dispatch table — maps BuiltinKind to type checking logic. No dependency on scope, generic context, or comptime state. Takes the Checker as a parameter, returns TypeIndex.

### checker/comptime_eval.zig (~730 lines)

Compile-time expression evaluation engine.

| Function | Lines | What it does |
|----------|-------|-------------|
| `evalComptimeValue` | 288 | Evaluate a comptime block to a ComptimeValue |
| `evalComptimeBlock` | 58 | Evaluate block statements at comptime |
| `evalComptimeAssign` | 39 | Handle comptime variable assignment |
| `getComptimeIdentName` | 10 | Resolve identifier in comptime scope |
| `evalComptimeArrayType` | 19 | Evaluate array type expressions at comptime |
| `evalComptimeInlineFor` | 86 | Unroll inline for at comptime |
| `evalConstExpr` | 175 | Evaluate constant expressions (integers, strings) |
| `evalConstFloat` | 32 | Evaluate float constant expressions |
| `isFloatType` | 5 | Helper predicate |
| `evalConstString` | 21 | Evaluate string constant expressions |

**Why extract:** Self-contained evaluation engine. Takes an AST node and a scope, returns a ComptimeValue. No mutation of the main type checking state except through well-defined interfaces (looking up symbols, resolving types).

### checker/diagnostics.zig (~290 lines)

Error message improvement — "did you mean?" suggestions.

| Function | Lines | What it does |
|----------|-------|-------------|
| `editDistance` | 27 | Levenshtein distance between two strings |
| `isUserVisibleName` | 10 | Filter out compiler-internal names |
| `findSimilarName` | 53 | Find closest variable name in scope |
| `findSimilarField` | 15 | Find closest field name in struct |
| `findSimilarVariant` | 15 | Find closest variant name in enum/union |
| `findSimilarType` | 32 | Find closest type name in registry |
| `findSimilarTrait` | 17 | Find closest trait name |
| `editDistSuggest` | 15 | Format suggestion string |
| `errWithSuggestion` | 93 | Report error with "did you mean?" attached |
| `reportRedefined` | 10 | Report symbol redefinition with location of original |

**Why extract:** Pure utility code. No type checking logic. Takes strings and scope, returns strings. Could be unit tested in isolation.

### checker/generics.zig (~270 lines)

Generic type and function instantiation.

| Function | Lines | What it does |
|----------|-------|-------------|
| `instantiateGenericImplMethods` | 141 | Clone impl methods for concrete type args |
| `instantiateGenericFunc` | 115 | Clone a generic function with concrete type substitution |
| `buildGenericCacheKey` | 16 | Build cache key for deduplication |

**Why extract:** Complex but isolated. Uses the Checker's type registry and scope but doesn't participate in expression/statement checking. Self-contained input (type args + generic definition) → output (new TypeIndex).

---

## What Stays in checker.zig (~3,270 lines)

The core type checking orchestrator:

| Category | Lines | Functions |
|----------|-------|-----------|
| Expression type inference | 764 | checkExpr, checkBinary, checkUnary, checkFieldAccess, etc. |
| Declaration checking | 499 | checkDecl, collectDecl, checkFnDecl, checkVarDecl |
| Statement checking | 442 | checkStmt, checkReturn, checkIfStmt, checkForStmt, etc. |
| Complex expressions | 509 | checkClosureExpr, checkStructInit, checkNewExpr, etc. |
| Type resolution | 216 | resolveType, resolveTypeExpr, resolveGenericInstance |
| Type building | 229 | buildFuncType, buildStructType, buildEnumType, etc. |
| Call checking | ~150 | checkCall (non-builtin path), resolveMethodCall |
| Scope management | 128 | Symbol, Scope, define, lookup |
| Sendable checking | 91 | isSendable, isCrossActorCallExpr |
| File/lint checking | 113 | checkFile, runLintChecks, checkScopeUnused |
| Infrastructure | 59 | init, deinit, SharedGenericContext |

This is the actual type checking logic that needs to see everything. It stays together because expression checking calls into statement checking, which calls back into expression checking, which needs scope and type resolution — tight coupling that's correct to keep unified.

---

## Import Pattern

```zig
// checker.zig
const builtins = @import("checker/builtins.zig");
const comptime_eval = @import("checker/comptime_eval.zig");
const diagnostics = @import("checker/diagnostics.zig");
const generics = @import("checker/generics.zig");

// In checkExpr:
.builtin_call => return builtins.checkBuiltinCall(self, node),

// In evalComptimeValue:
return comptime_eval.evalComptimeValue(self, node, scope),

// In error reporting:
diagnostics.errWithSuggestion(self, span, msg, scope),

// In generic resolution:
return generics.instantiateGenericFunc(self, func_idx, type_args),
```

Each submodule takes `*Checker` as its first parameter — full access to the type registry, scope stack, and error reporter. The submodule boundary is about code organization, not encapsulation.

---

## Verification

After the split, running all existing tests must produce identical results. The refactor changes no logic — only file locations. Each submodule can also gain focused unit tests:

- `builtins.zig`: test each builtin kind returns correct type
- `comptime_eval.zig`: test constant folding (int arithmetic, string concat)
- `diagnostics.zig`: test editDistance, findSimilarName
- `generics.zig`: test instantiation with concrete types
