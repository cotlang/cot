# Index Set Module Audit (Phase 6.8)

**Source**: `regalloc2/src/indexset.rs`
**Target**: `compiler/codegen/native/regalloc/indexset.zig`
**Status**: ✅ Complete (~430 LOC, 7 tests)

---

## Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `SetBitsIter` | `SetBitsIter` | indexset.rs:287 | Iterator over set bits in u64 |
| `AdaptiveMap` | `AdaptiveMap` | indexset.rs:21 | Hybrid small/large mode map |
| `AdaptiveMap::Small` | `Mode.small` | indexset.rs:22-27 | Inline storage (12 entries) |
| `AdaptiveMap::Large` | `Mode.large` | indexset.rs:28 | HashMap storage |
| `AdaptiveMap::new()` | `AdaptiveMap.init()` | indexset.rs:33 | ✅ |
| `AdaptiveMap::get_or_insert()` | `AdaptiveMap.getOrInsert()` | indexset.rs:42 | ✅ |
| `AdaptiveMap::get_mut()` | `AdaptiveMap.getMut()` | indexset.rs:96 | ✅ |
| `AdaptiveMap::get()` | `AdaptiveMap.get()` | indexset.rs:114 | ✅ |
| `AdaptiveMap::iter()` | `AdaptiveMap.iterator()` | indexset.rs:135 | ✅ |
| `AdaptiveMap::is_empty()` | `AdaptiveMap.isEmpty()` | indexset.rs:146 | ✅ |
| `IndexSet` | `IndexSet` | indexset.rs:183 | Sparse bit set |
| `IndexSet.elems` | `IndexSet.elems` | indexset.rs:184 | AdaptiveMap backing |
| `IndexSet.cache` | `IndexSet.cache_key/cache_value` | indexset.rs:185 | Streaming cache |
| `IndexSet::new()` | `IndexSet.init()` | indexset.rs:191 | ✅ |
| `IndexSet::set()` | `IndexSet.set()` | indexset.rs:227 | ✅ |
| `IndexSet::get()` | `IndexSet.get()` | indexset.rs:242 | ✅ |
| `IndexSet::assign()` | `IndexSet.assign()` | indexset.rs:236 | ✅ |
| `IndexSet::union_with()` | `IndexSet.unionWith()` | indexset.rs:251 | ✅ Returns changed |
| `IndexSet::iter()` | `IndexSet.iter()` | indexset.rs:265 | ✅ |
| `IndexSet::is_small()` | `IndexSet.isSmall()` | indexset.rs:274 | ✅ |
| `IndexSet::is_empty()` | `IndexSet.isEmpty()` | indexset.rs:282 | ✅ |

---

## Constants

| Rust Constant | Zig Constant | Value | Notes |
|---------------|--------------|-------|-------|
| `SMALL_ELEMS` | `SMALL_ELEMS` | 12 | Max inline entries before expanding |
| `BITS_PER_WORD` | `BITS_PER_WORD` | 64 | Bits per u64 word |
| `INVALID` | `INVALID` | 0xffff_ffff | Sentinel for empty/invalid keys |

---

## SetBitsIter Algorithm

Iterates over set bits in a u64 word using bit manipulation:

```
next():
  if word == 0:
    return null
  bit = @ctz(word)      // Count trailing zeros
  word &= word - 1      // Clear lowest set bit
  return bit
```

This is the most efficient way to enumerate set bits.

---

## AdaptiveMap Modes

### Small Mode
- Stores up to 12 (key, value) pairs inline
- Keys in `keys: [12]u32` array
- Values in `values: [12]u64` array
- O(n) lookup but excellent cache locality
- Can reuse slots when value becomes zero

### Large Mode
- Uses `std.AutoHashMap(u32, u64)` for arbitrary capacity
- O(1) average lookup
- Only transitions to large mode when:
  1. All 12 slots are in use
  2. All values are non-zero
  3. A new key is needed

---

## IndexSet Structure

The IndexSet is a sparse bit set that maps indices to bits:

```
index -> word_index = index / 64
         bit_index  = index % 64

storage: AdaptiveMap<word_index, u64_bitmask>
```

### Streaming Cache

One-entry cache optimizes sequential access patterns:
- `cache_key`: Word index of cached entry
- `cache_value`: Pointer to cached u64

Cache is invalidated when the cached word is modified.

---

## Key Operations

### set(index, value)
```
word_idx = index / 64
bit_idx = index % 64
word = elems.getOrInsert(word_idx)
if value:
  word |= (1 << bit_idx)
else:
  word &= ~(1 << bit_idx)
```

### get(index)
```
word_idx = index / 64
bit_idx = index % 64
word = elems.get(word_idx) or 0
return (word >> bit_idx) & 1 == 1
```

### unionWith(other)
```
changed = false
for (key, value) in other.elems:
  my_value = elems.getOrInsert(key)
  new_value = my_value | value
  if new_value != my_value:
    elems.set(key, new_value)
    changed = true
return changed
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| SetBitsIter | ✅ | Bit enumeration |
| IndexSet basic operations | ✅ | set, get, assign |
| IndexSet iteration | ✅ | iter over all set bits |
| IndexSet small mode preservation | ✅ | Stay in small mode when possible |
| IndexSet union | ✅ | unionWith semantics |
| IndexSet expand to large mode | ✅ | Transition when needed |
| AdaptiveMap basic | ✅ | get, getOrInsert, getMut |

