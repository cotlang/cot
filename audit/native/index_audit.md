# Core Types Module Audit (Phase 6.1)

**Source**: `regalloc2/src/lib.rs` lines 73-600, `src/index.rs`
**Target**: `compiler/codegen/native/regalloc/index.zig`
**Status**: ✅ Complete (727 LOC, 8 tests)

---

## Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `RegClass` | `RegClass` | lib.rs:86-92 | enum(u2) { int, float, vector } |
| `PReg` | `PReg` | lib.rs:108-180 | Bit-packed: class:2 + hw_enc:6 = 8 bits |
| `PReg::MAX_BITS` | `PReg.MAX_BITS` | lib.rs:115 | 6 |
| `PReg::MAX` | `PReg.MAX` | lib.rs:116 | 63 |
| `PReg::NUM_INDEX` | `PReg.NUM_INDEX` | lib.rs:117 | 256 |
| `PReg::new()` | `PReg.new()` | lib.rs:122 | ✅ Identical encoding |
| `PReg::hw_enc()` | `PReg.hwEnc()` | lib.rs:131 | ✅ |
| `PReg::class()` | `PReg.class()` | lib.rs:137 | ✅ |
| `PReg::index()` | `PReg.index()` | lib.rs:150 | ✅ |
| `PReg::from_index()` | `PReg.fromIndex()` | lib.rs:156 | ✅ |
| `PReg::invalid()` | `PReg.invalid()` | lib.rs:165 | ✅ |
| `PRegSet` | `PRegSet` | lib.rs:221-400 | Bitset, 4×64-bit words |
| `PRegSet::empty()` | `PRegSet.empty()` | lib.rs:233 | ✅ |
| `PRegSet::contains()` | `PRegSet.contains()` | lib.rs:246 | ✅ |
| `PRegSet::with()` | `PRegSet.with()` | lib.rs:252 | ✅ Immutable add |
| `PRegSet::add()` | `PRegSet.add()` | lib.rs:260 | ✅ Mutable add |
| `PRegSet::remove()` | `PRegSet.remove()` | lib.rs:266 | ✅ |
| `PRegSet::union_from()` | `PRegSet.unionFrom()` | lib.rs:273 | ✅ |
| `PRegSet::intersect_from()` | `PRegSet.intersectFrom()` | lib.rs:279 | ✅ |
| `PRegSet::invert()` | `PRegSet.invert()` | lib.rs:285 | ✅ |
| `PRegSet::len()` | `PRegSet.count()` | lib.rs:298 | ✅ |
| `PRegSetIter` | `PRegSetIterator` | lib.rs:356-375 | ✅ |
| `VReg` | `VReg` | lib.rs:422-488 | Bit-packed: vreg:21 + class:2 = 23 bits (in u32) |
| `VReg::MAX_BITS` | `VReg.MAX_BITS` | lib.rs:429 | 21 |
| `VReg::MAX` | `VReg.MAX` | lib.rs:430 | 2,097,151 |
| `VReg::new()` | `VReg.new()` | lib.rs:433 | ✅ Identical encoding |
| `VReg::vreg()` | `VReg.vreg()` | lib.rs:441 | ✅ |
| `VReg::class()` | `VReg.class()` | lib.rs:447 | ✅ |
| `VReg::invalid()` | `VReg.invalid()` | lib.rs:457 | ✅ |
| `SpillSlot` | `SpillSlot` | lib.rs:498-548 | 24-bit index in u32 |
| `SpillSlot::MAX` | `SpillSlot.MAX` | lib.rs:504 | 16,777,215 |
| `SpillSlot::new()` | `SpillSlot.new()` | lib.rs:508 | ✅ |
| `SpillSlot::index()` | `SpillSlot.index()` | lib.rs:515 | ✅ |
| `SpillSlot::plus()` | `SpillSlot.plus()` | lib.rs:521 | ✅ |
| `SpillSlot::invalid()` | `SpillSlot.invalid()` | lib.rs:527 | ✅ |
| `Block` | `Block` | index.rs:137 | u32 index |
| `Block::new()` | `Block.new()` | index.rs | ✅ |
| `Block::index()` | `Block.idx()` | index.rs | ✅ |
| `Block::invalid()` | `Block.invalid()` | index.rs | ✅ |
| `Block::next()` | `Block.next()` | index.rs | ✅ |
| `Block::prev()` | `Block.prev()` | index.rs | ✅ |
| `Inst` | `Inst` | index.rs:136 | u32 index |
| `Inst::new()` | `Inst.new()` | index.rs | ✅ |
| `Inst::index()` | `Inst.idx()` | index.rs | ✅ |
| `Inst::invalid()` | `Inst.invalid()` | index.rs | ✅ |
| `InstRange` | `InstRange` | index.rs:144 | Half-open range [from, to) |
| `InstRange::new()` | `InstRange.new()` | index.rs:148 | ✅ |
| `InstRange::first()` | `InstRange.first()` | index.rs:154 | ✅ |
| `InstRange::last()` | `InstRange.last()` | index.rs:160 | ✅ |
| `InstRange::rest()` | `InstRange.rest()` | index.rs:166 | ✅ |
| `InstRange::len()` | `InstRange.len()` | index.rs:172 | ✅ |
| `InstRange::iter()` | `InstRange.iter()` | index.rs:177 | ✅ |

---

## Bit Encoding Verification

**PReg** (8 bits):
```
Rust:  bits = (class << 6) | hw_enc
Zig:   bits = (class << 6) | hw_enc
       ✅ Identical
```

**VReg** (32 bits, using 23):
```
Rust:  bits = (vreg << 2) | class
Zig:   bits = (vreg << 2) | class
       ✅ Identical
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| Block creation and operations | ✅ | new, idx, invalid, next, prev |
| Inst creation and operations | ✅ | new, idx, invalid |
| InstRange iteration | ✅ | first, last, rest, iter |
| PReg creation and encoding | ✅ | new, hwEnc, class, index, fromIndex |
| PRegSet operations | ✅ | add, contains, remove, union, intersect |
| PRegSet iteration | ✅ | iterator over set bits |
| VReg creation | ✅ | new, vreg, class |
| SpillSlot creation | ✅ | new, index, plus |

