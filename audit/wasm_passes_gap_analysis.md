# Wasm SSA Passes - Gap Analysis

## Go's SSA Pass Structure (relevant for strings)

| Go File | Purpose | Order |
|---------|---------|-------|
| `rewritegeneric.go` | ConstString → StringMake(Addr, Const) | Early |
| `expand_calls.go` | Decompose aggregate args/returns | 17 |
| `decompose.go` | String phi decomposition | 18 |
| `rewritedec.go` | StringPtr/Len extraction, Load/Store decomposition | During lower |
| `lower.go` | Generic ops → arch-specific ops | 32 |

## Cot's Current Structure

| Cot File | Purpose | Problem |
|----------|---------|---------|
| `lower_wasm.zig` | Everything crammed here | Not matching Go |
| `native/decompose.zig` | Native-only decompose | Not used for Wasm |
| `native/expand_calls.zig` | Native-only expand | Not used for Wasm |

## Current Wasm Pipeline (driver.zig:399-402)

```zig
try schedule.schedule(ssa_func);
try layout.layout(ssa_func);
try lower_wasm.lower(ssa_func);  // <-- Everything crammed here
```

## Required: Go-Matching File Structure

Create these files in `compiler/ssa/passes/`:

| New File | Go Equivalent | Purpose |
|----------|---------------|---------|
| `rewritegeneric.zig` | `rewritegeneric.go` | ConstString → StringMake(ptr_const, len_const) |
| `rewritedec.zig` | `rewritedec.go` | StringPtr/Len decomposition, Load/Store<string> decomposition |
| `decompose.zig` | `decompose.go` | String phi decomposition |
| `expand_calls.zig` | `expand_calls.go` | (Optional for Wasm - simpler ABI) |

Keep:
- `lower_wasm.zig` - Only for generic → wasm_* op lowering (Go's `lower.go`)

## Required Pipeline Order

```zig
// New Wasm pipeline in driver.zig
try rewritegeneric.rewrite(ssa_func, &string_offsets);  // NEW
try decompose.decompose(ssa_func);                       // NEW
try rewritedec.rewrite(ssa_func);                        // NEW
try schedule.schedule(ssa_func);
try layout.layout(ssa_func);
try lower_wasm.lower(ssa_func);                          // Existing (ops only)
```

## Key Transformations Needed

### 1. rewritegeneric.zig (Go: rewritegeneric.go:6424-6492)

```
// ConstString with data section offset
(const_string {idx}) =>
  (string_make
    (const_64 [string_offsets[idx]])  // ptr to data section
    (const_64 [string_literals[idx].len]))  // length
```

### 2. rewritedec.zig (Go: rewritedec.go:755-820)

```
// Extract ptr from StringMake
(slice_ptr (string_make ptr _)) => ptr
(string_ptr (string_make ptr _)) => ptr

// Extract len from StringMake
(slice_len (string_make _ len)) => len
(string_len (string_make _ len)) => len

// Load<string> decomposition
(load<string> ptr mem) =>
  (string_make
    (load<i64> ptr mem)
    (load<i64> (off_ptr ptr 8) mem))

// Store<string> decomposition
(store dst (string_make ptr len) mem) =>
  (store dst ptr mem)
  (store (off_ptr dst 8) len mem)
```

### 3. decompose.zig (Go: decompose.go:143-157)

```
// String phi decomposition
(phi<string> a b c ...) =>
  (string_make
    (phi (string_ptr a) (string_ptr b) (string_ptr c) ...)
    (phi (string_len a) (string_len b) (string_len c) ...))
```

### 4. lower_wasm.zig (CLEAN UP - ops only)

Remove decomposition logic. Keep only:
- `add` → `wasm_i64_add`
- `sub` → `wasm_i64_sub`
- `load` → `wasm_i64_load`
- etc.

## Current Bugs This Will Fix

1. `len("hello")` doesn't work because:
   - `const_string` stays as-is (not converted to `string_make`)
   - `slice_len(const_string)` has no decomposition rule
   - Falls through to gen.zig which has hacky special-case code

2. String in struct field doesn't work because:
   - `load<string>` not decomposed to two loads
   - `store<string>` not decomposed to two stores

3. String as function argument doesn't work because:
   - No expand_calls to split into (ptr, len) args

## Implementation Order

1. Create `rewritegeneric.zig` - handle ConstString first
2. Create `rewritedec.zig` - move decomposition from lower_wasm.zig
3. Create `decompose.zig` - phi decomposition (if needed)
4. Clean up `lower_wasm.zig` - remove decomposition, ops only
5. Update `driver.zig` - new pass order
6. Remove hacky code from `gen.zig` - slice_ptr/len/string_ptr/len handlers
