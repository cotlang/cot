# Operands Module Audit (Phase 6.2)

**Source**: `regalloc2/src/lib.rs` lines 550-1180
**Target**: `compiler/codegen/native/regalloc/operand.zig`
**Status**: ✅ Complete (762 LOC, 7 tests)

---

## Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `OperandConstraint` | `OperandConstraint` | lib.rs:562-580 | Tagged union |
| `OperandConstraint::Any` | `.any` | lib.rs:564 | ✅ |
| `OperandConstraint::Reg` | `.reg` | lib.rs:566 | ✅ |
| `OperandConstraint::Stack` | `.stack` | lib.rs:568 | ✅ |
| `OperandConstraint::FixedReg(PReg)` | `.{ .fixed_reg = PReg }` | lib.rs:570 | ✅ |
| `OperandConstraint::Reuse(usize)` | `.{ .reuse = usize }` | lib.rs:572 | ✅ |
| `OperandConstraint::Limit(usize)` | `.{ .limit = usize }` | lib.rs:579 | ✅ |
| `OperandKind` | `OperandKind` | lib.rs:597-602 | enum(u1) |
| `OperandKind::Def` | `.def` | lib.rs:600 | = 0 |
| `OperandKind::Use` | `.use` | lib.rs:601 | = 1 |
| `OperandPos` | `OperandPos` | lib.rs:622-627 | enum(u1) |
| `OperandPos::Early` | `.early` | lib.rs:625 | = 0 |
| `OperandPos::Late` | `.late` | lib.rs:626 | = 1 |
| `Operand` | `Operand` | lib.rs:653-1000 | 32-bit packed |
| `Operand::new()` | `Operand.new()` | lib.rs:676 | ✅ |
| `Operand::reg_use()` | `Operand.regUse()` | lib.rs:721 | ✅ |
| `Operand::reg_use_at_end()` | `Operand.regUseAtEnd()` | lib.rs:734 | ✅ |
| `Operand::reg_def()` | `Operand.regDef()` | lib.rs:748 | ✅ |
| `Operand::reg_def_at_start()` | `Operand.regDefAtStart()` | lib.rs:769 | ✅ |
| `Operand::reg_temp()` | `Operand.regTemp()` | lib.rs:791 | ✅ |
| `Operand::reg_reuse_def()` | `Operand.regReuseDef()` | lib.rs:809 | ✅ |
| `Operand::reg_fixed_use()` | `Operand.regFixedUse()` | lib.rs:823 | ✅ |
| `Operand::reg_fixed_def()` | `Operand.regFixedDef()` | lib.rs:837 | ✅ |
| `Operand::any_use()` | `Operand.anyUse()` | lib.rs:872 | ✅ |
| `Operand::any_def()` | `Operand.anyDef()` | lib.rs:885 | ✅ |
| `Operand::fixed_nonallocatable()` | `Operand.fixedNonAllocatable()` | lib.rs:898 | ✅ |
| `Operand::vreg()` | `Operand.vreg()` | lib.rs:912 | ✅ |
| `Operand::class()` | `Operand.class()` | lib.rs:919 | ✅ |
| `Operand::kind()` | `Operand.kind()` | lib.rs:932 | ✅ |
| `Operand::pos()` | `Operand.pos()` | lib.rs:946 | ✅ |
| `Operand::constraint()` | `Operand.constraint()` | lib.rs:958 | ✅ |
| `AllocationKind` | `AllocationKind` | lib.rs:1169-1178 | enum(u3) |
| `AllocationKind::None` | `.none` | lib.rs:1175 | = 0 |
| `AllocationKind::Reg` | `.reg` | lib.rs:1176 | = 1 |
| `AllocationKind::Stack` | `.stack` | lib.rs:1177 | = 2 |
| `Allocation` | `Allocation` | lib.rs:1037-1167 | 32-bit packed |
| `Allocation::none()` | `Allocation.none()` | lib.rs:1075 | ✅ |
| `Allocation::reg()` | `Allocation.reg()` | lib.rs:1081 | ✅ |
| `Allocation::stack()` | `Allocation.stack()` | lib.rs:1087 | ✅ |
| `Allocation::kind()` | `Allocation.kind()` | lib.rs:1093 | ✅ |
| `Allocation::is_none()` | `Allocation.isNone()` | lib.rs:1104 | ✅ |
| `Allocation::is_reg()` | `Allocation.isReg()` | lib.rs:1116 | ✅ |
| `Allocation::is_stack()` | `Allocation.isStack()` | lib.rs:1122 | ✅ |
| `Allocation::index()` | `Allocation.index()` | lib.rs:1129 | ✅ |
| `Allocation::as_reg()` | `Allocation.asReg()` | lib.rs:1135 | ✅ |
| `Allocation::as_stack()` | `Allocation.asStack()` | lib.rs:1145 | ✅ |
| `InstPosition` | `InstPosition` | lib.rs:1341-1344 | enum(u1) |
| `InstPosition::Before` | `.before` | lib.rs:1342 | = 0 |
| `InstPosition::After` | `.after` | lib.rs:1343 | = 1 |
| `ProgPoint` | `ProgPoint` | lib.rs:1349-1441 | 32-bit packed |
| `ProgPoint::new()` | `ProgPoint.new()` | lib.rs:1370 | ✅ |
| `ProgPoint::before()` | `ProgPoint.before()` | lib.rs:1377 | ✅ |
| `ProgPoint::after()` | `ProgPoint.after()` | lib.rs:1383 | ✅ |
| `ProgPoint::inst()` | `ProgPoint.inst()` | lib.rs:1389 | ✅ |
| `ProgPoint::pos()` | `ProgPoint.pos()` | lib.rs:1398 | ✅ |
| `ProgPoint::next()` | `ProgPoint.nextPoint()` | lib.rs:1410 | ✅ |
| `ProgPoint::prev()` | `ProgPoint.prevPoint()` | lib.rs:1419 | ✅ |
| `Edit` | `Edit` | lib.rs:1446-1455 | Tagged union |
| `Edit::Move` | `.move` | lib.rs:1454 | ✅ |

---

## Bit Encoding Verification

**Operand** (32 bits):
```
Rust:  constraint:7 | kind:1 | pos:1 | class:2 | vreg:21
Zig:   constraint:7 | kind:1 | pos:1 | class:2 | vreg:21
       ✅ Identical

Constraint encoding:
  1xxxxxx = FixedReg(hw_enc = xxxxxx)
  01xxxxx = Reuse(index = xxxxx)
  001xxxx = Limit(max = 1 << xxxx)
  0000000 = Any
  0000001 = Reg
  0000010 = Stack
```

**Allocation** (32 bits):
```
Rust:  kind:3 | unused:1 | index:28
Zig:   kind:3 | unused:1 | index:28
       ✅ Identical
```

**ProgPoint** (32 bits):
```
Rust:  inst:31 | pos:1
Zig:   inst:31 | pos:1
       ✅ Identical
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| OperandConstraint formatting | ✅ | All constraint types |
| Operand creation and accessors | ✅ | new, vreg, class, kind, pos, constraint |
| Operand bit packing round-trip | ✅ | Encode/decode verification |
| Allocation creation and accessors | ✅ | none, reg, stack, kind, asReg, asStack |
| ProgPoint creation and accessors | ✅ | before, after, inst, pos, nextPoint, prevPoint |
| Edit formatting | ✅ | Move edit display |
| fixedNonAllocatable | ✅ | PReg marking |

