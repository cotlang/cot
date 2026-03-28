# Comptime Map Implementation Plan

**Date:** 2026-03-28
**Status:** Ready for implementation
**Effort:** ~2-3 days
**Prerequisite:** None — comptime arrays already work

---

## Step 0: Fix Comptime Array Literal Init (prerequisite)

**Problem:** Comptime blocks only accept `var arr: [3]i64 = undefined` for array creation. The literal syntax `var arr = [0, 0, 0]` fails with "unable to evaluate comptime expression". Both should work — Zig accepts all of these in comptime:

```zig
// Zig — all valid in comptime:
var s: [3]i64 = undefined;           // typed + undefined  (works in Cot ✓)
var s = [_]i64{ 0, 0, 0 };          // inferred literal    (fails in Cot ✗)
var s: [3]i64 = .{ 0, 0, 0 };       // typed + init list   (fails in Cot ✗)
```

**Fix:** In `checker.zig`, `evalComptimeBlock`'s var_decl handling, when the initializer is an array literal:

1. Evaluate each element as a comptime value
2. Construct a `ComptimeArray` from the results
3. Store in `comptime_vars`

```zig
// In evalComptimeBlock, var_decl with array literal init:
if (init_expr == .array_literal) {
    var elements = std.ArrayListUnmanaged(ComptimeValue){};
    for (init_expr.array_literal.items) |elem_idx| {
        const elem_val = self.evalComptimeValue(elem_idx) orelse return null;
        elements.append(self.allocator, elem_val) catch return null;
    }
    const arr = ComptimeValue{ .array = .{
        .elements = elements,
        .elem_type_name = "",  // inferred from elements
    }};
    comptime_vars.put(var_name, arr);
}
```

**Location:** `compiler/frontend/checker.zig`, inside `evalComptimeBlock` near the existing `undefined` array init handling.

**Lines:** ~20-30

**Test:**
```cot
test "comptime array literal init" {
    const s = comptime {
        var arr = [10, 20, 12]
        arr[0] = 99
        arr
    }
    @assertEq(s[0], 99)
    @assertEq(s[1], 20)
    @assertEq(s[2], 12)
}
```

This must be done BEFORE the map work — the comptime evaluator's value handling should be complete for arrays before extending to maps.

---

## Context

Cot needs compile-time map support for patterns like keyword lookup tables:

```cot
const keywords = comptime {
    var m = ComptimeMap(string, Token).{}
    m.set("fn", .kw_fn)
    m.set("var", .kw_var)
    m.set("if", .kw_if)
    // ... 60+ keywords
    m
}

pub fn lookup(name: string) Token {
    return keywords.get(name) orelse .ident
}
```

Comptime arrays already work (confirmed — `ComptimeValue.array` exists with element mutation, inline for iteration, and lowering to runtime values). Maps follow the same pattern.

---

## Current Comptime Infrastructure

**Already implemented and working:**
- `ComptimeValue` union in `compiler/frontend/comptime.zig` (64 lines)
- `ComptimeArray` with `elements: ArrayListUnmanaged(ComptimeValue)`
- Mutable comptime variables via `checker.comptime_vars: StringHashMap(ComptimeValue)`
- Array element mutation: `arr[i] = val` in comptime blocks
- Inline for over comptime arrays
- Comptime block evaluation returning last expression
- Lowering comptime arrays to runtime via `emitComptimeArray()` in `lower.zig`

**Files that need changes:**
1. `compiler/frontend/comptime.zig` — add ComptimeMap type
2. `compiler/frontend/checker.zig` — evaluate map operations in comptime blocks
3. `compiler/frontend/lower.zig` — materialize comptime maps to runtime
4. `test/e2e/features.cot` — add tests

---

## Step 1: Extend ComptimeValue (comptime.zig, ~25 lines)

Add `map` variant to the ComptimeValue union:

```zig
pub const ComptimeValue = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    boolean: bool,
    array: ComptimeArray,
    map: ComptimeMap,           // ADD THIS
    enum_field: EnumFieldInfo,
    type_info: TypeInfo,
    undefined_val,

    // ... existing methods ...
};

// ADD THIS:
pub const ComptimeMap = struct {
    keys: std.ArrayListUnmanaged(ComptimeValue),
    values: std.ArrayListUnmanaged(ComptimeValue),
    key_type_name: []const u8,
    value_type_name: []const u8,

    pub fn get(self: *const ComptimeMap, key: ComptimeValue) ?ComptimeValue {
        for (self.keys.items, 0..) |k, i| {
            if (comptimeEqual(k, key)) return self.values.items[i];
        }
        return null;
    }

    pub fn set(self: *ComptimeMap, allocator: Allocator, key: ComptimeValue, value: ComptimeValue) void {
        // Update existing key if found
        for (self.keys.items, 0..) |k, i| {
            if (comptimeEqual(k, key)) {
                self.values.items[i] = value;
                return;
            }
        }
        // Insert new key-value pair
        self.keys.append(allocator, key) catch {};
        self.values.append(allocator, value) catch {};
    }

    pub fn count(self: *const ComptimeMap) usize {
        return self.keys.items.len;
    }
};

fn comptimeEqual(a: ComptimeValue, b: ComptimeValue) bool {
    if (@as(std.meta.Tag(ComptimeValue), a) != @as(std.meta.Tag(ComptimeValue), b)) return false;
    return switch (a) {
        .int => |v| v == b.int,
        .string => |v| std.mem.eql(u8, v, b.string),
        .boolean => |v| v == b.boolean,
        else => false,
    };
}
```

Two parallel arrays (keys + values) rather than a hash map — simpler, correct, and fast enough for comptime where N is small (< 100 entries).

---

## Step 2: Checker — Evaluate Map Operations (checker.zig, ~120 lines)

### 2a: Handle map literal syntax in comptime blocks

The map is created as an empty `ComptimeMap` and populated via method calls. Inside `evalComptimeBlock`, when processing assignment statements that are method calls on a comptime map variable:

In the `evalComptimeBlock` function (around line 1319 where `evalComptimeAssign` is), add handling for:

```
var m: Map(string, Token) = .{}    → create empty ComptimeMap in comptime_vars
m.set("fn", .kw_fn)                → call ComptimeMap.set()
m.get("fn")                        → call ComptimeMap.get()
```

### 2b: Handle `.{}` empty map initialization

When the checker evaluates `var m: Map(K, V) = .{}` inside a comptime block, and the type is a Map, create an empty `ComptimeMap`:

```zig
// In evalComptimeBlock, var_decl handling:
if (type is Map(K, V) and init is .zero_init) {
    const map = ComptimeValue{ .map = .{
        .keys = .{},
        .values = .{},
        .key_type_name = K_name,
        .value_type_name = V_name,
    }};
    comptime_vars.put(var_name, map);
}
```

### 2c: Handle method calls on comptime maps

When evaluating `m.set(key, value)` or `m.get(key)` inside a comptime block:

```zig
// In evalComptimeBlock, expression statement handling:
// Detect: comptime_var.method_name(args...)
if (expr is method_call and receiver is comptime_var) {
    const map_ptr = comptime_vars.getPtr(receiver_name);
    if (map_ptr.* == .map) {
        if (method_name == "set") {
            const key = evalComptimeValue(args[0]);
            const val = evalComptimeValue(args[1]);
            map_ptr.map.set(allocator, key, val);
        } else if (method_name == "get") {
            const key = evalComptimeValue(args[0]);
            return map_ptr.map.get(key);
        }
    }
}
```

### 2d: Handle comptime map as return value

When the comptime block's last expression is a map variable, return the `ComptimeValue.map`. This already works for arrays — extend the same pattern.

### 2e: Handle for iteration over comptime maps

```cot
comptime {
    inline for (key, value) in m {
        // key and value are comptime-known
    }
}
```

In `evalComptimeInlineFor`, add map iteration alongside existing array iteration:

```zig
if (iterable == .map) {
    for (iterable.map.keys.items, iterable.map.values.items) |k, v| {
        // Bind loop variables, re-evaluate body
    }
}
```

---

## Step 3: Lowerer — Materialize Comptime Maps (lower.zig, ~80 lines)

When a comptime map is used at runtime, it needs to be materialized into actual memory. Two strategies:

### Strategy A: Lower to sequential scan (simple, correct)

Materialize as two parallel arrays (keys[] and values[]) with a linear scan lookup. For < 100 entries this is fast enough.

```zig
fn emitComptimeMap(self: *Lowerer, map: ComptimeMap, span: Span) !ir.NodeIndex {
    const fb = self.current_func orelse return ir.null_node;
    const n = map.count();

    // Allocate buffer: [keys...][values...]
    const key_size = self.type_reg.sizeOf(key_type);
    const val_size = self.type_reg.sizeOf(val_type);
    const total = n * (key_size + val_size);
    const buf = try fb.emitCall("alloc", &.{zero, total_const}, ...);

    // Store keys
    for (map.keys.items, 0..) |key, i| {
        const addr = try fb.emitBinary(.add, buf, offset_const);
        try self.emitComptimeStore(addr, key);
    }

    // Store values
    for (map.values.items, 0..) |val, i| {
        const addr = try fb.emitBinary(.add, buf, offset_const);
        try self.emitComptimeStore(addr, val);
    }

    return buf;
}
```

### Strategy B: Lower to switch statement (optimized, for keyword lookup)

For string→enum maps specifically, generate a switch on first character + length. This is what Go does and is the fastest approach for keyword tables:

```zig
fn emitComptimeMapAsSwitch(self: *Lowerer, map: ComptimeMap) !ir.NodeIndex {
    // Group entries by string length
    // Generate: switch (key.len) { 2 => ..., 3 => ..., ... }
    // Within each length: compare first char, then full string
}
```

**Recommendation:** Start with Strategy A. Add Strategy B later as an optimization for the specific `string → Token` pattern used in keyword lookup.

---

## Step 4: Tests (test/e2e/features.cot, ~40 lines)

```cot
test "comptime map basic" {
    const m = comptime {
        var map: Map(string, i64) = .{}
        map.set("a", 1)
        map.set("b", 2)
        map.set("c", 3)
        map
    }
    @assertEq(m.get("a"), 1)
    @assertEq(m.get("b"), 2)
    @assertEq(m.get("c"), 3)
}

test "comptime map iteration" {
    const total = comptime {
        var map: Map(string, i64) = .{}
        map.set("x", 10)
        map.set("y", 20)
        var sum: i64 = 0
        inline for (key, value) in map {
            sum += value
        }
        sum
    }
    @assertEq(total, 30)
}

test "comptime map overwrite" {
    const m = comptime {
        var map: Map(string, i64) = .{}
        map.set("a", 1)
        map.set("a", 99)
        map
    }
    @assertEq(m.get("a"), 99)
}
```

---

## Step 5: Port to selfcot (self/check/checker.cot, ~80 lines)

Mirror the same changes in the self-hosted checker. The `ComptimeValue` in selfcot already has the array variant — add map alongside it.

---

## Summary

| File | Changes | Lines |
|------|---------|-------|
| `compiler/frontend/comptime.zig` | Add ComptimeMap, comptimeEqual | ~50 |
| `compiler/frontend/checker.zig` | Map init, .set(), .get(), for iteration | ~120 |
| `compiler/frontend/lower.zig` | emitComptimeMap materialization | ~80 |
| `test/e2e/features.cot` | 3 test cases | ~40 |
| `self/check/checker.cot` | Mirror checker changes | ~80 |
| **Total** | | **~370 lines** |

**Effort:** 2-3 days. No architectural changes — extends existing comptime infrastructure.

**Key insight from research:** Cot's comptime arrays were implemented 5 weeks ago and already handle mutation, iteration, and lowering. Comptime maps are the same pattern with key-value pairs instead of indexed elements. The `ComptimeValue` union just needs one more variant.

---

## Correction: Comptime Arrays Already Work

The earlier assessment that "comptime array init is missing" was **wrong**. Testing confirmed:

```cot
// This WORKS today — typed array with undefined init:
const s = comptime {
    var arr: [3]i64 = undefined
    arr[0] = 10
    arr[1] = 20
    arr[2] = 12
    arr
}
// s[0] + s[1] + s[2] = 42
```

**Required syntax:** `var arr: [N]Type = undefined` then indexed assignment. The literal syntax `var arr = [0, 0, 0]` does NOT work in comptime blocks — the evaluator needs the typed+undefined form to create a mutable comptime array. This matches how the existing tests use it (`inline for i in 0..5 { s[i] = i * i }`).
