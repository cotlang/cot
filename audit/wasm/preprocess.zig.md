# Audit: wasm/preprocess.zig

## Status: WORKING - 60% PARITY

| Metric | Value |
|--------|-------|
| Lines | ~443 |
| Go Reference | cmd/internal/obj/wasm/wasmobj.go preprocess() (lines 156-748) |
| Tests | 2 unit tests |

---

## Go Reference Mapping

### Main Function

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| preprocess | 156-748 | preprocess | 34-205 | **PARTIAL** |

### Prologue Generation (Go: lines 207-214)

| Step | Go Code | Our Code | Parity |
|------|---------|----------|--------|
| Get SP | `Get SP` | `appendAfter(.get, regAddr(.sp))` | **YES** |
| Push framesize | `i32.const framesize` | `appendAfter(.i32_const, constAddr(framesize))` | **YES** |
| Subtract | `i32.sub` | `appendAfter(.i32_sub)` | **YES** |
| Set SP | `Set SP` | `appendAfter(.set, regAddr(.sp))` | **YES** |

### Instruction Transformations

| Go Section | Go Lines | Our Implementation | Parity |
|------------|----------|-------------------|--------|
| CALL expansion | 438-502 | Minimal (line 90-94) | **SIMPLIFIED** |
| RET epilogue | 504-544 | Full (lines 108-126) | **YES** |
| Get with TYPE_ADDR | 565-585 | expandGetAddr (lines 212-241) | **YES** |
| Load with TYPE_MEM | 587-600 | expandLoadMem (lines 244-262) | **YES** |
| MOV expansion | 602-687 | expandMov (lines 265-344) | **YES** |
| Offset adjustment | 547-562 | Lines 179-202 | **YES** |

### Epilogue Generation (Go: lines 504-544)

| Step | Go Code | Our Code | Parity |
|------|---------|----------|--------|
| Get SP | `Get SP` | `insertBefore(.get, regAddr(.sp))` | **YES** |
| Push framesize | `i32.const framesize` | `insertBefore(.i32_const, constAddr(framesize))` | **YES** |
| Add | `i32.add` | `insertBefore(.i32_add)` | **YES** |
| Set SP | `Set SP` | `insertBefore(.set, regAddr(.sp))` | **YES** |

### Helper Functions

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| N/A (inline) | 565-585 | expandGetAddr | 212-241 | **YES** |
| N/A (inline) | 587-600 | expandLoadMem | 244-262 | **YES** |
| N/A (inline) | 602-687 | expandMov | 265-344 | **YES** |
| N/A | - | appendAfter | 350-361 | **ADDED** |
| N/A | - | insertBefore | 363-375 | **ADDED** |

---

## What's Not Implemented

### Go Features Not Yet Needed

| Feature | Go Lines | Status | Reason |
|---------|----------|--------|--------|
| Stack growth check | 207-240 | **SKIPPED** | Wasm manages stack |
| Resume points | 248-284 | **SKIPPED** | No goroutines yet |
| morestack | 288-340 | **SKIPPED** | Wasm manages stack |
| Return address push | 438-480 | **SKIPPED** | Using Wasm call stack |
| Closure handling | 482-502 | **SKIPPED** | No closures yet |
| Unwind check | 510-520 | **SKIPPED** | No goroutines |
| PC_B handling | 697-748 | **SKIPPED** | No goroutines |

---

## Offset Adjustment Logic

Go reference: wasmobj.go lines 547-562

```
Auto variables: SP + offset (within frame)
Param variables: SP + framesize + 8 + offset (after frame and return addr)
```

Our implementation (lines 179-202) matches this exactly:

```zig
switch (current.from.name) {
    .auto => current.from.offset += framesize,
    .param => {
        current.from.reg = .sp;
        current.from.offset += framesize + 8;
    },
}
```

---

## Verification

```bash
$ zig test compiler/codegen/wasm/preprocess.zig
All 2 tests passed.
```

---

## Future Work

1. **CALL expansion**: Full implementation with return address management
2. **Closure calls**: Handle OpWasmLoweredClosureCall
3. **Resume points**: For goroutine support

**VERDICT: 60% parity. Core frame management implemented. Advanced features (goroutines, closures) not yet needed.**
