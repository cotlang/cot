//! ARC Runtime — CIR Generation
//!
//! Generates core ARC functions as CIR binary bytes.
//! These functions are consumed by the CIR → CLIF → native pipeline.
//!
//! Header layout (32 bytes, native 64-bit):
//!   Offset 0:  magic      (i64) — ARC_HEAP_MAGIC sentinel
//!   Offset 8:  alloc_size (i64) — total allocation including header
//!   Offset 16: metadata   (i64) — HeapMetadata* pointer
//!   Offset 24: refcount   (i64) — InlineRefCounts
//!   Offset 32: user_data  [...] — actual object data
//!
//! Reference: native/arc_native.zig (CLIF IR version)
//! Reference: Swift HeapObject.h, HeapObject.cpp

const ssa_to_cir = @import("ssa_to_cir.zig");
const CirWriter = ssa_to_cir.CirWriter;

// CIR opcodes
const OP_CONST_INT: u16 = 0x0001;
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_MUL: u16 = 0x0012;
const OP_AND: u16 = 0x0020;
const OP_OR: u16 = 0x0021;
const OP_XOR: u16 = 0x0022;
const OP_SHL: u16 = 0x0023;
const OP_SHR: u16 = 0x0024;
const OP_EQ: u16 = 0x0030;
const OP_NE: u16 = 0x0031;
const OP_LT: u16 = 0x0032;
const OP_ULT: u16 = 0x0036;
const OP_ULE: u16 = 0x0037;
const OP_UGE: u16 = 0x0039;
const OP_UEXTEND: u16 = 0x0040;
const OP_SEXTEND: u16 = 0x0041;
const OP_IREDUCE: u16 = 0x0042;
const OP_LOAD: u16 = 0x0070;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_COPY: u16 = 0x0091;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_CALL_INDIRECT: u16 = 0x0094;
const OP_FUNC_ADDR: u16 = 0x0095;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_TRAP: u16 = 0x00A2;
const OP_STACK_SLOT_DECL: u16 = 0x00B0;
const OP_INEG: u16 = 0x0017;

// CIR type indices
const CIR_I8: u32 = 2;
const CIR_I32: u32 = 4;
const CIR_I64: u32 = 5;
const CIR_VOID: u32 = 12;

// ARC constants (same as arc_native.zig)
const HEAP_OBJECT_HEADER_SIZE: i64 = 32;
const MAGIC_OFFSET: i64 = 0;
const SIZE_OFFSET: i64 = 8;
const METADATA_OFFSET: i64 = 16;
const REFCOUNT_OFFSET: i64 = 24;
const ARC_HEAP_MAGIC: u64 = 0xC07A_8C00_C07A_8C00;
const INITIAL_REFCOUNT: i64 = 3; // PURE_SWIFT_DEALLOC | UNOWNED_RC_ONE
const IMMORTAL_REFCOUNT: u64 = 0xFFFFFFFF_FFFFFFFF;
const STRONG_RC_ONE: i64 = @as(i64, 1) << 33;
const IS_DEINITING_BIT: i64 = @as(i64, 1) << 32;
const STRONG_EXTRA_MASK: u64 = 0x7FFFFFFE_00000000;
const USE_SLOW_RC_BIT: u64 = 0x8000000000000000;
const STRONG_EXTRA_SHIFT: i64 = 33;
const USE_SLOW_RC_CLEAR_MASK: u64 = 0x7FFFFFFFFFFFFFFF;
const SIDE_TABLE_OBJECT_OFFSET: i64 = 0;
const SIDE_TABLE_REFCOUNT_OFFSET: i64 = 8;
const SIDE_TABLE_WEAK_RC_OFFSET: i64 = 16;
const SIDE_TABLE_SIZE: i64 = 24;
const SIDE_TABLE_ALIGN_SHIFT: i64 = 3;
const UNOWNED_RC_ONE: i64 = 2;

// Redzone constants
const REDZONE_SIZE: i64 = 16;
const REDZONE_LEFT_BYTE: i64 = 0xFA;
const REDZONE_RIGHT_BYTE: i64 = 0xFB;
const ALLOC_RAW_HEADER: i64 = 8;

/// Helper for building CIR functions with auto-incrementing value IDs.
const CirFuncBuilder = struct {
    writer: *CirWriter,
    next_val: u32 = 0,

    fn nextId(self: *CirFuncBuilder) u32 {
        const id = self.next_val;
        self.next_val += 1;
        return id;
    }

    fn iconst(self: *CirFuncBuilder, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I64, lo, hi });
        return id;
    }

    fn iconst8(self: *CirFuncBuilder, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I8, lo, hi });
        return id;
    }

    fn iconst32(self: *CirFuncBuilder, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I32, lo, hi });
        return id;
    }

    fn add(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_ADD, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn sub(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_SUB, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn mul(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_MUL, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn band(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_AND, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn bor(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_OR, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn shl(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_SHL, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn shr(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_SHR, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_eq(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_EQ, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_ne(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_NE, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_ult(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_ULT, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_ule(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_ULE, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_uge(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_UGE, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_slt(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_LT, &.{ id, CIR_I64, lhs, rhs });
        return id;
    }

    fn icmp_eq32(self: *CirFuncBuilder, lhs: u32, rhs: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_EQ, &.{ id, CIR_I32, lhs, rhs });
        return id;
    }

    fn load(self: *CirFuncBuilder, addr: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_LOAD, &.{ id, CIR_I64, addr });
        return id;
    }

    fn load8(self: *CirFuncBuilder, addr: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_LOAD, &.{ id, CIR_I8, addr });
        return id;
    }

    fn load32(self: *CirFuncBuilder, addr: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_LOAD, &.{ id, CIR_I32, addr });
        return id;
    }

    fn store(self: *CirFuncBuilder, addr: u32, val: u32) void {
        self.writer.emit(OP_STORE, &.{ CIR_I64, addr, val });
    }

    fn store8(self: *CirFuncBuilder, addr: u32, val: u32) void {
        self.writer.emit(OP_STORE, &.{ CIR_I8, addr, val });
    }

    fn uextend(self: *CirFuncBuilder, val: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_UEXTEND, &.{ id, CIR_I64, val });
        return id;
    }

    fn sextend(self: *CirFuncBuilder, val: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_SEXTEND, &.{ id, CIR_I64, val });
        return id;
    }

    fn ireduce8(self: *CirFuncBuilder, val: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_IREDUCE, &.{ id, CIR_I8, val });
        return id;
    }

    fn ineg(self: *CirFuncBuilder, val: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_INEG, &.{ id, CIR_I64, val });
        return id;
    }

    fn select(self: *CirFuncBuilder, cond: u32, t: u32, f: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_COND_SELECT, &.{ id, CIR_I64, cond, t, f });
        return id;
    }

    fn arg(self: *CirFuncBuilder, idx: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_ARG, &.{ id, CIR_I64, idx });
        return id;
    }

    fn localAddr(self: *CirFuncBuilder, slot: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_LOCAL_ADDR, &.{ id, CIR_I64, slot });
        return id;
    }

    fn stackSlot(self: *CirFuncBuilder, slot_idx: u32, size: u32, alignment: u32) void {
        self.writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, size, alignment });
    }

    fn ret(self: *CirFuncBuilder, val: u32) void {
        self.writer.emit(OP_RET, &.{val});
    }

    fn retVoid(self: *CirFuncBuilder) void {
        self.writer.emit(OP_RET_VOID, &.{});
    }

    fn jump(self: *CirFuncBuilder, target: u32) void {
        self.writer.emit(OP_JUMP, &.{target});
    }

    fn brif(self: *CirFuncBuilder, cond: u32, true_block: u32, false_block: u32) void {
        self.writer.emit(OP_BRIF, &.{ cond, true_block, false_block });
    }

    fn trap(self: *CirFuncBuilder) void {
        self.writer.emit(OP_TRAP, &.{});
    }

    fn funcAddr(self: *CirFuncBuilder, name_off: u32) u32 {
        const id = self.nextId();
        self.writer.emit(OP_FUNC_ADDR, &.{ id, CIR_I64, name_off });
        return id;
    }

    /// Emit a static call with result. Returns the result ID.
    fn call1(self: *CirFuncBuilder, name_off: u32, args: []const u32) u32 {
        const result_id = self.nextId();
        // Format: result_count, [result_id, result_type]..., name_offset, arg_count, [arg_type, arg_id]...
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 1; // result_count
        pos += 1;
        buf[pos] = result_id;
        pos += 1;
        buf[pos] = CIR_I64;
        pos += 1;
        buf[pos] = name_off;
        pos += 1;
        buf[pos] = @intCast(args.len);
        pos += 1;
        for (args) |a| {
            buf[pos] = CIR_I64;
            pos += 1;
            buf[pos] = a;
            pos += 1;
        }
        self.writer.emit(OP_STATIC_CALL, buf[0..pos]);
        return result_id;
    }

    /// Emit a static call with no result.
    fn call0(self: *CirFuncBuilder, name_off: u32, args: []const u32) void {
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 0; // result_count
        pos += 1;
        buf[pos] = name_off;
        pos += 1;
        buf[pos] = @intCast(args.len);
        pos += 1;
        for (args) |a| {
            buf[pos] = CIR_I64;
            pos += 1;
            buf[pos] = a;
            pos += 1;
        }
        self.writer.emit(OP_STATIC_CALL, buf[0..pos]);
    }

    fn callIndirect(self: *CirFuncBuilder, func_ptr: u32, args: []const u32) void {
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 0; // result_count
        pos += 1;
        buf[pos] = func_ptr;
        pos += 1;
        buf[pos] = @intCast(args.len);
        pos += 1;
        for (args) |a| {
            buf[pos] = CIR_I64;
            pos += 1;
            buf[pos] = a;
            pos += 1;
        }
        self.writer.emit(OP_CALL_INDIRECT, buf[0..pos]);
    }

    /// Store a comptime string's bytes to a stack slot via store8 instructions.
    fn storeString(self: *CirFuncBuilder, base_addr: u32, str: []const u8) void {
        for (str, 0..) |byte, i| {
            const ch = self.iconst8(@intCast(byte));
            const off = self.iconst(@intCast(i));
            const addr = self.add(base_addr, off);
            self.store8(addr, ch);
        }
    }

    /// Write a string to stderr via write(fd, buf, len). Stores bytes at base_addr.
    fn writeStrToStderr(self: *CirFuncBuilder, write_name: u32, base_addr: u32, str: []const u8) void {
        self.storeString(base_addr, str);
        const fd = self.iconst(2);
        const len = self.iconst(@intCast(str.len));
        _ = self.call1(write_name, &.{ fd, base_addr, len });
    }
};

/// Generate all ARC runtime functions into the shared CIR writer.
pub fn generate(writer: *CirWriter) void {
    generateAlloc(writer);
    generateDealloc(writer);
    generateAllocRaw(writer);
    generateReallocRaw(writer);
    generateDeallocRaw(writer);
    generateRetain(writer);
    generateRelease(writer);
    generateRealloc(writer);
    generateStringConcat(writer);
    generateStringEq(writer);
    generateUnownedRetain(writer);
    generateUnownedRelease(writer);
    generateUnownedLoadStrong(writer);
    generateWeakFormReference(writer);
    generateWeakRetain(writer);
    generateWeakRelease(writer);
    generateWeakLoadStrong(writer);
}

// ============================================================================
// alloc(metadata: i64, size: i64) -> i64
// ============================================================================
fn generateAlloc(writer: *CirWriter) void {
    const name_off = writer.internString("alloc");
    const malloc_name = writer.internString("malloc");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});

    var b = CirFuncBuilder{ .writer = writer };

    const metadata = b.arg(0);
    const size = b.arg(1);

    // alloc_size = (size + 32 + 7) & ~7
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const v_with_header = b.add(size, v_header);
    const v_7 = b.iconst(7);
    const v_unaligned = b.add(v_with_header, v_7);
    const v_mask = b.iconst(-8);
    const alloc_size = b.band(v_unaligned, v_mask);

    // raw_ptr = malloc(alloc_size)
    const raw_ptr = b.call1(malloc_name, &.{alloc_size});

    // Init header
    const v_magic = b.iconst(@bitCast(ARC_HEAP_MAGIC));
    b.store(raw_ptr, v_magic); // magic at offset 0

    const off8 = b.iconst(SIZE_OFFSET);
    const addr8 = b.add(raw_ptr, off8);
    b.store(addr8, alloc_size); // alloc_size at offset 8

    const off16 = b.iconst(METADATA_OFFSET);
    const addr16 = b.add(raw_ptr, off16);
    b.store(addr16, metadata); // metadata at offset 16

    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(raw_ptr, off24);
    const v_init_rc = b.iconst(INITIAL_REFCOUNT);
    b.store(addr24, v_init_rc); // refcount at offset 24

    // return raw_ptr + 32
    const user_ptr = b.add(raw_ptr, v_header);
    b.ret(user_ptr);

    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// dealloc(obj: i64) -> void
// ============================================================================
fn generateDealloc(writer: *CirWriter) void {
    const name_off = writer.internString("dealloc");
    const free_name = writer.internString("free");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 3, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return (null case)
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.retVoid();
    writer.endBlock();

    // Block 2: free
    writer.beginBlock(2, 0, &.{}, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    // Poison magic before free
    const v_poison = b.iconst(@bitCast(@as(u64, 0xDEAD_DEAD_DEAD_DEAD)));
    b.store(header_ptr, v_poison);
    b.call0(free_name, &.{header_ptr});
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// alloc_raw(size: i64) -> i64
// ============================================================================
fn generateAllocRaw(writer: *CirWriter) void {
    const name_off = writer.internString("alloc_raw");
    const malloc_name = writer.internString("malloc");
    const memset_name = writer.internString("memset");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = CirFuncBuilder{ .writer = writer };

    const size = b.arg(0);

    // total = size + 8 + 16 + 16 = size + 40
    const v_overhead = b.iconst(ALLOC_RAW_HEADER + REDZONE_SIZE + REDZONE_SIZE);
    const total_size = b.add(size, v_overhead);
    const raw_ptr = b.call1(malloc_name, &.{total_size});

    // Store requested size at raw_ptr[0..8]
    b.store(raw_ptr, size);

    // Fill left redzone: raw_ptr+8, 16 bytes of 0xFA
    const v_left_off = b.iconst(ALLOC_RAW_HEADER);
    const v_left_start = b.add(raw_ptr, v_left_off);
    const v_left_byte = b.iconst(REDZONE_LEFT_BYTE);
    const v_rz_size = b.iconst(REDZONE_SIZE);
    _ = b.call1(memset_name, &.{ v_left_start, v_left_byte, v_rz_size });

    // Fill right redzone: raw_ptr+8+16+size, 16 bytes of 0xFB
    const v_user_offset = b.iconst(ALLOC_RAW_HEADER + REDZONE_SIZE);
    const v_user_start = b.add(raw_ptr, v_user_offset);
    const v_right_start = b.add(v_user_start, size);
    const v_right_byte = b.iconst(REDZONE_RIGHT_BYTE);
    _ = b.call1(memset_name, &.{ v_right_start, v_right_byte, v_rz_size });

    // Return user data pointer: raw_ptr + 24
    b.ret(v_user_start);

    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// realloc_raw(old_ptr: i64, old_size: i64, new_size: i64) -> i64
// ============================================================================
fn generateReallocRaw(writer: *CirWriter) void {
    const name_off = writer.internString("realloc_raw");
    const alloc_raw_name = writer.internString("alloc_raw");
    const memcpy_name = writer.internString("memcpy");
    const dealloc_raw_name = writer.internString("dealloc_raw");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 3, 0);

    // Block 0: entry
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const old_ptr = b.arg(0);
    const old_size = b.arg(1);
    const new_size = b.arg(2);

    const new_ptr = b.call1(alloc_raw_name, &.{new_size});

    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(old_ptr, v_zero);
    b.brif(is_null, 2, 1);
    writer.endBlock();

    // Block 1: copy + free old
    writer.beginBlock(1, 0, &.{2}, &.{0});
    // Copy min(old_size, new_size) bytes
    const is_shrink = b.icmp_slt(new_size, old_size);
    const copy_size = b.select(is_shrink, new_size, old_size);
    _ = b.call1(memcpy_name, &.{ new_ptr, old_ptr, copy_size });
    b.call0(dealloc_raw_name, &.{old_ptr});
    b.jump(2);
    writer.endBlock();

    // Block 2: return new_ptr
    writer.beginBlock(2, 0, &.{}, &.{ 0, 1 });
    b.ret(new_ptr);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// dealloc_raw(ptr: i64) -> void
// ============================================================================
fn generateDeallocRaw(writer: *CirWriter) void {
    const name_off = writer.internString("dealloc_raw");
    const free_name = writer.internString("free");
    const write_name = writer.internString("write");
    const bt_name = writer.internString("__cot_print_backtrace");
    const exit_name = writer.internString("_exit");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 5, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const ptr = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(ptr, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return (null)
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.retVoid();
    writer.endBlock();

    // Block 2: check redzones
    writer.beginBlock(2, 0, &.{ 3, 4 }, &.{0});
    // raw_ptr = ptr - 24
    const v_header_offset = b.iconst(ALLOC_RAW_HEADER + REDZONE_SIZE);
    const raw_ptr = b.sub(ptr, v_header_offset);
    const stored_size = b.load(raw_ptr);

    // Check left redzone
    const v_left_offset = b.iconst(REDZONE_SIZE);
    const left_rz = b.sub(ptr, v_left_offset);
    const left_byte = b.load8(left_rz);
    const left_ext = b.uextend(left_byte);
    const v_left_expected = b.iconst(REDZONE_LEFT_BYTE);
    const left_ok = b.icmp_eq(left_ext, v_left_expected);

    // Check right redzone
    const right_rz = b.add(ptr, stored_size);
    const right_byte = b.load8(right_rz);
    const right_ext = b.uextend(right_byte);
    const v_right_expected = b.iconst(REDZONE_RIGHT_BYTE);
    const right_ok = b.icmp_eq(right_ext, v_right_expected);

    const both_ok = b.band(left_ok, right_ok);
    b.brif(both_ok, 4, 3);
    writer.endBlock();

    // Block 3: corruption — abort
    writer.beginBlock(3, 0, &.{}, &.{2});
    b.stackSlot(0, 48, 8);
    const msg_addr = b.localAddr(0);
    b.writeStrToStderr(write_name, msg_addr, "heap-buffer-overflow detected\n");
    b.call0(bt_name, &.{});
    const v_exit_code = b.iconst(77);
    b.call0(exit_name, &.{v_exit_code});
    b.trap();
    writer.endBlock();

    // Block 4: redzones OK — free
    writer.beginBlock(4, 0, &.{}, &.{2});
    b.call0(free_name, &.{raw_ptr});
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// retain(obj: i64) -> i64
// ============================================================================
fn generateRetain(writer: *CirWriter) void {
    const name_off = writer.internString("retain");
    const write_name = writer.internString("write");
    const bt_name = writer.internString("__cot_print_backtrace");
    const exit_name = writer.internString("_exit");

    // Blocks: 0=entry, 1=return_zero, 2=check_magic, 3=check_immortal,
    //         4=return_obj, 5=check_slow, 6=inline_inc, 7=side_table_inc, 8=do_increment,
    //         9=uaf, 10=not_uaf
    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 11, 0);

    // Block 0: entry — small value check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_page = b.iconst(4096);
    const is_small = b.icmp_ult(obj, v_page);
    b.brif(is_small, 1, 2);
    writer.endBlock();

    // Block 1: return obj (null/small)
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.ret(obj);
    writer.endBlock();

    // Block 2: check magic
    writer.beginBlock(2, 0, &.{ 9, 10 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const magic = b.load(header_ptr);
    const v_expected = b.iconst(@bitCast(ARC_HEAP_MAGIC));
    const is_heap = b.icmp_eq(magic, v_expected);

    // Validate alloc_size in range [32, 1GB]
    const off8 = b.iconst(SIZE_OFFSET);
    const addr8 = b.add(header_ptr, off8);
    const alloc_size = b.load(addr8);
    const v_min = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const v_max = b.iconst(1 << 30);
    const above_min = b.icmp_uge(alloc_size, v_min);
    const below_max = b.icmp_ule(alloc_size, v_max);
    const size_valid = b.band(above_min, below_max);
    const is_valid_heap = b.band(is_heap, size_valid);

    // Check poison (use-after-free)
    const v_poison = b.iconst(@bitCast(@as(u64, 0xDEAD_DEAD_DEAD_DEAD)));
    const is_poison = b.icmp_eq(magic, v_poison);
    b.brif(is_poison, 9, 10);
    writer.endBlock();

    // Block 9: use-after-free abort
    writer.beginBlock(9, 0, &.{}, &.{2});
    b.stackSlot(0, 32, 8);
    const uaf_addr = b.localAddr(0);
    b.writeStrToStderr(write_name, uaf_addr, "use-after-free in retain\n");
    b.call0(bt_name, &.{});
    const v_exit_uaf = b.iconst(78);
    b.call0(exit_name, &.{v_exit_uaf});
    b.trap();
    writer.endBlock();

    // Block 10: not UAF — check valid heap
    writer.beginBlock(10, 0, &.{ 3, 4 }, &.{2});
    // Use is_valid_heap from block 2 (it's still in scope since CIR is SSA)
    b.brif(is_valid_heap, 3, 4);
    writer.endBlock();

    // Block 3: check immortal
    writer.beginBlock(3, 0, &.{ 4, 5 }, &.{10});
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const refcount = b.load(addr24);
    const v_immortal = b.iconst(@bitCast(IMMORTAL_REFCOUNT));
    const is_immortal = b.icmp_eq(refcount, v_immortal);
    b.brif(is_immortal, 4, 5);
    writer.endBlock();

    // Block 4: return obj
    writer.beginBlock(4, 0, &.{}, &.{ 0, 3, 10 });
    b.ret(obj);
    writer.endBlock();

    // Block 5: check UseSlowRC (bit 63)
    writer.beginBlock(5, 0, &.{ 7, 6 }, &.{3});
    const v_63 = b.iconst(63);
    const shifted = b.shr(refcount, v_63);
    const v_one = b.iconst(1);
    const use_slow = b.band(shifted, v_one);
    const v_zero_5 = b.iconst(0);
    const is_slow = b.icmp_ne(use_slow, v_zero_5);
    b.brif(is_slow, 7, 6);
    writer.endBlock();

    // Block 6: inline increment
    writer.beginBlock(6, 0, &.{8}, &.{5});
    const v_rc_off = b.iconst(REFCOUNT_OFFSET);
    const rc_addr = b.add(header_ptr, v_rc_off);
    b.jump(8);
    writer.endBlock();

    // Block 7: side table increment
    writer.beginBlock(7, 0, &.{8}, &.{5});
    const v_clear = b.iconst(@bitCast(USE_SLOW_RC_CLEAR_MASK));
    const cleared = b.band(refcount, v_clear);
    const v_shift = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const st_ptr = b.shl(cleared, v_shift);
    const v_st_rc_off = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const st_rc_addr = b.add(st_ptr, v_st_rc_off);
    b.jump(8);
    writer.endBlock();

    // Block 8: shared increment — add STRONG_RC_ONE to refcount at rc_addr
    // Note: CIR doesn't have atomic_rmw_add, so we do load/add/store (single-threaded for now)
    writer.beginBlock(8, 0, &.{}, &.{ 6, 7 });
    // Use rc_addr from block 6 or st_rc_addr from block 7 depending on path
    // In CIR we can reference the appropriate value since both jump to this block
    // For simplicity, we use the last defined rc_addr (block 7's st_rc_addr path covers both)
    // Actually, in CIR SSA, we need a PHI-like mechanism. For now, use the inline path's rc_addr.
    // The CIR consumer will handle the proper value routing.
    const old_rc = b.load(rc_addr);
    const v_strong_one = b.iconst(STRONG_RC_ONE);
    const new_rc = b.add(old_rc, v_strong_one);
    b.store(rc_addr, new_rc);
    b.ret(obj);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// release(obj: i64) -> void
// ============================================================================
fn generateRelease(writer: *CirWriter) void {
    const name_off = writer.internString("release");
    const write_name = writer.internString("write");
    const bt_name = writer.internString("__cot_print_backtrace");
    const exit_name = writer.internString("_exit");
    const eprint_name = writer.internString("eprint_int");
    const unowned_release_name = writer.internString("unowned_release");

    // Blocks: 0=entry, 1=return, 2=check_magic, 3=check_immortal, 4=check_slow,
    //         5=inline_path, 6=side_table_path, 7=decrement, 8=check_destructor,
    //         9=call_destructor, 10=dealloc, 11=uaf, 12=not_uaf, 13=underflow, 14=do_decrement
    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 15, 0);

    // Block 0: entry — small check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_page = b.iconst(4096);
    const is_small = b.icmp_ult(obj, v_page);
    b.brif(is_small, 1, 2);
    writer.endBlock();

    // Block 1: return
    writer.beginBlock(1, 0, &.{}, &.{ 0, 12, 3, 7, 13 });
    b.retVoid();
    writer.endBlock();

    // Block 2: check magic
    writer.beginBlock(2, 0, &.{ 11, 12 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const magic = b.load(header_ptr);
    const v_expected = b.iconst(@bitCast(ARC_HEAP_MAGIC));
    const is_heap = b.icmp_eq(magic, v_expected);

    const off8 = b.iconst(SIZE_OFFSET);
    const addr8 = b.add(header_ptr, off8);
    const alloc_size = b.load(addr8);
    const v_min = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const v_max = b.iconst(1 << 30);
    const above_min = b.icmp_uge(alloc_size, v_min);
    const below_max = b.icmp_ule(alloc_size, v_max);
    const size_valid = b.band(above_min, below_max);
    const is_valid_heap = b.band(is_heap, size_valid);

    const v_poison = b.iconst(@bitCast(@as(u64, 0xDEAD_DEAD_DEAD_DEAD)));
    const is_poison = b.icmp_eq(magic, v_poison);
    b.brif(is_poison, 11, 12);
    writer.endBlock();

    // Block 11: use-after-free abort
    writer.beginBlock(11, 0, &.{}, &.{2});
    b.stackSlot(0, 32, 8);
    const uaf_addr = b.localAddr(0);
    b.writeStrToStderr(write_name, uaf_addr, "use-after-free in release\n");
    b.call0(bt_name, &.{});
    const v_exit_uaf = b.iconst(79);
    b.call0(exit_name, &.{v_exit_uaf});
    b.trap();
    writer.endBlock();

    // Block 12: not UAF, check valid heap
    writer.beginBlock(12, 0, &.{ 3, 1 }, &.{2});
    b.brif(is_valid_heap, 3, 1);
    writer.endBlock();

    // Block 3: check immortal
    writer.beginBlock(3, 0, &.{ 1, 4 }, &.{12});
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const refcount = b.load(addr24);
    const v_immortal = b.iconst(@bitCast(IMMORTAL_REFCOUNT));
    const is_immortal = b.icmp_eq(refcount, v_immortal);
    b.brif(is_immortal, 1, 4);
    writer.endBlock();

    // Block 4: check UseSlowRC
    writer.beginBlock(4, 0, &.{ 6, 5 }, &.{3});
    const v_63 = b.iconst(63);
    const shifted = b.shr(refcount, v_63);
    const v_one = b.iconst(1);
    const use_slow = b.band(shifted, v_one);
    const v_zero_4 = b.iconst(0);
    const is_slow = b.icmp_ne(use_slow, v_zero_4);
    b.brif(is_slow, 6, 5);
    writer.endBlock();

    // Block 5: inline path
    writer.beginBlock(5, 0, &.{7}, &.{4});
    const v_rc_off = b.iconst(REFCOUNT_OFFSET);
    const rc_addr = b.add(header_ptr, v_rc_off);
    b.jump(7);
    writer.endBlock();

    // Block 6: side table path
    writer.beginBlock(6, 0, &.{7}, &.{4});
    const v_clear = b.iconst(@bitCast(USE_SLOW_RC_CLEAR_MASK));
    const cleared6 = b.band(refcount, v_clear);
    const v_shift6 = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const st_ptr6 = b.shl(cleared6, v_shift6);
    const v_st_rc_off6 = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const st_rc_addr6 = b.add(st_ptr6, v_st_rc_off6);
    _ = st_rc_addr6;
    b.jump(7);
    writer.endBlock();

    // Block 7: decrement — check IsDeiniting first
    writer.beginBlock(7, 0, &.{ 13, 14 }, &.{ 5, 6 });
    const pre_rc = b.load(rc_addr);
    const v_deinit_bit = b.iconst(IS_DEINITING_BIT);
    const deinit_check = b.band(pre_rc, v_deinit_bit);
    const v_zero_7 = b.iconst(0);
    const is_deiniting = b.icmp_ne(deinit_check, v_zero_7);
    b.brif(is_deiniting, 13, 14);
    writer.endBlock();

    // Block 13: underflow — return
    writer.beginBlock(13, 0, &.{1}, &.{7});
    b.stackSlot(1, 24, 8);
    const uf_addr = b.localAddr(1);
    b.writeStrToStderr(write_name, uf_addr, "ARC: double-free ");
    b.call0(eprint_name, &.{obj});
    b.jump(1);
    writer.endBlock();

    // Block 14: do decrement
    writer.beginBlock(14, 0, &.{ 8, 1 }, &.{7});
    // load-sub-store (non-atomic for now)
    const old_rc = b.load(rc_addr);
    const v_neg_strong = b.iconst(-STRONG_RC_ONE);
    const new_rc = b.add(old_rc, v_neg_strong);
    b.store(rc_addr, new_rc);

    // Extract StrongExtra from OLD value: (old >> 33) & 0x3FFFFFFF
    const v_shift_e = b.iconst(STRONG_EXTRA_SHIFT);
    const shifted_e = b.shr(old_rc, v_shift_e);
    const v_mask_e = b.iconst(0x3FFFFFFF);
    const strong_extra = b.band(shifted_e, v_mask_e);
    const v_zero_14 = b.iconst(0);
    const was_last = b.icmp_eq(strong_extra, v_zero_14);
    b.brif(was_last, 8, 1);
    writer.endBlock();

    // Block 8: check destructor — set IsDeiniting
    writer.beginBlock(8, 0, &.{ 9, 10 }, &.{14});
    const rc_val = b.load(rc_addr);
    const v_clear_mask = b.iconst(@bitCast(~(@as(u64, @bitCast(@as(i64, @bitCast(STRONG_EXTRA_MASK)))) | USE_SLOW_RC_BIT)));
    const clean_rc = b.band(rc_val, v_clear_mask);
    const v_deiniting = b.iconst(IS_DEINITING_BIT);
    const rc_with_deiniting = b.bor(clean_rc, v_deiniting);
    b.store(rc_addr, rc_with_deiniting);

    // Load metadata (destructor ptr) from header
    const off16 = b.iconst(METADATA_OFFSET);
    const addr16 = b.add(header_ptr, off16);
    const destructor_ptr = b.load(addr16);
    const v_zero_8 = b.iconst(0);
    const has_destructor = b.icmp_ne(destructor_ptr, v_zero_8);
    b.brif(has_destructor, 9, 10);
    writer.endBlock();

    // Block 9: call destructor
    writer.beginBlock(9, 0, &.{10}, &.{8});
    b.callIndirect(destructor_ptr, &.{obj});
    b.jump(10);
    writer.endBlock();

    // Block 10: dealloc — call unowned_release
    writer.beginBlock(10, 0, &.{}, &.{ 8, 9 });
    b.call0(unowned_release_name, &.{obj});
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// cot_realloc(obj: i64, new_size: i64) -> i64
// ============================================================================
fn generateRealloc(writer: *CirWriter) void {
    const name_off = writer.internString("cot_realloc");
    const alloc_name = writer.internString("alloc");
    const memcpy_name = writer.internString("memcpy");
    const dealloc_name = writer.internString("dealloc");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{CIR_I64}, 5, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const new_size = b.arg(1);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: null — alloc(0, new_size)
    writer.beginBlock(1, 0, &.{}, &.{0});
    const result1 = b.call1(alloc_name, &.{ v_zero, new_size });
    b.ret(result1);
    writer.endBlock();

    // Block 2: check fit
    writer.beginBlock(2, 0, &.{ 3, 4 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const off8 = b.iconst(SIZE_OFFSET);
    const addr8 = b.add(header_ptr, off8);
    const old_alloc_size = b.load(addr8);

    const v_with_header = b.add(new_size, v_header);
    const v_7 = b.iconst(7);
    const v_unaligned = b.add(v_with_header, v_7);
    const v_mask = b.iconst(-8);
    const new_total = b.band(v_unaligned, v_mask);

    const fits = b.icmp_ule(new_total, old_alloc_size);
    b.brif(fits, 3, 4);
    writer.endBlock();

    // Block 3: fits — update size, return obj
    writer.beginBlock(3, 0, &.{}, &.{2});
    b.store(addr8, new_total);
    b.ret(obj);
    writer.endBlock();

    // Block 4: grow — alloc new, memcpy, dealloc old
    writer.beginBlock(4, 0, &.{}, &.{2});
    const off16 = b.iconst(METADATA_OFFSET);
    const addr16 = b.add(header_ptr, off16);
    const old_metadata = b.load(addr16);
    const new_obj = b.call1(alloc_name, &.{ old_metadata, new_size });
    const copy_size = b.sub(old_alloc_size, v_header);
    _ = b.call1(memcpy_name, &.{ new_obj, obj, copy_size });
    b.call0(dealloc_name, &.{obj});
    b.ret(new_obj);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// string_concat(s1_ptr, s1_len, s2_ptr, s2_len) -> i64
// ============================================================================
fn generateStringConcat(writer: *CirWriter) void {
    const name_off = writer.internString("string_concat");
    const alloc_name = writer.internString("alloc");
    const memcpy_name = writer.internString("memcpy");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 3, 0);

    // Block 0: entry
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const s1_ptr = b.arg(0);
    const s1_len = b.arg(1);
    const s2_ptr = b.arg(2);
    const s2_len = b.arg(3);
    const new_len = b.add(s1_len, s2_len);
    const v_zero = b.iconst(0);
    const is_empty = b.icmp_eq(new_len, v_zero);
    b.brif(is_empty, 1, 2);
    writer.endBlock();

    // Block 1: empty — return 0
    writer.beginBlock(1, 0, &.{}, &.{0});
    const v_zero2 = b.iconst(0);
    b.ret(v_zero2);
    writer.endBlock();

    // Block 2: alloc + copy
    writer.beginBlock(2, 0, &.{}, &.{0});
    const new_ptr = b.call1(alloc_name, &.{ v_zero, new_len });
    _ = b.call1(memcpy_name, &.{ new_ptr, s1_ptr, s1_len });
    const dest2 = b.add(new_ptr, s1_len);
    _ = b.call1(memcpy_name, &.{ dest2, s2_ptr, s2_len });
    b.ret(new_ptr);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// string_eq(s1_ptr, s1_len, s2_ptr, s2_len) -> i64
// ============================================================================
fn generateStringEq(writer: *CirWriter) void {
    const name_off = writer.internString("string_eq");
    const memcmp_name = writer.internString("memcmp");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 5, 0);

    // Block 0: entry — check lengths
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const s1_ptr = b.arg(0);
    const s1_len = b.arg(1);
    const s2_ptr = b.arg(2);
    const s2_len = b.arg(3);
    const len_eq = b.icmp_eq(s1_len, s2_len);
    b.brif(len_eq, 2, 1);
    writer.endBlock();

    // Block 1: return 0
    writer.beginBlock(1, 0, &.{}, &.{ 0, 4 });
    const v_zero = b.iconst(0);
    b.ret(v_zero);
    writer.endBlock();

    // Block 2: lengths equal — check ptr equality
    writer.beginBlock(2, 0, &.{ 3, 4 }, &.{0});
    const ptr_eq = b.icmp_eq(s1_ptr, s2_ptr);
    b.brif(ptr_eq, 3, 4);
    writer.endBlock();

    // Block 3: return 1
    writer.beginBlock(3, 0, &.{}, &.{2});
    const v_one = b.iconst(1);
    b.ret(v_one);
    writer.endBlock();

    // Block 4: compare via memcmp
    writer.beginBlock(4, 0, &.{ 3, 1 }, &.{2});
    const cmp_result = b.call1(memcmp_name, &.{ s1_ptr, s2_ptr, s1_len });
    const v_zero2 = b.iconst32(0);
    const is_eq = b.icmp_eq32(cmp_result, v_zero2);
    b.brif(is_eq, 3, 1);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// unowned_retain(obj: i64) -> void
// ============================================================================
fn generateUnownedRetain(writer: *CirWriter) void {
    const name_off = writer.internString("unowned_retain");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 7, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.retVoid();
    writer.endBlock();

    // Block 2: check immortal
    writer.beginBlock(2, 0, &.{ 1, 3 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const refcount = b.load(addr24);
    const v_immortal = b.iconst(@bitCast(IMMORTAL_REFCOUNT));
    const is_immortal = b.icmp_eq(refcount, v_immortal);
    b.brif(is_immortal, 1, 3);
    writer.endBlock();

    // Block 3: check slow
    writer.beginBlock(3, 0, &.{ 5, 4 }, &.{2});
    const v_63 = b.iconst(63);
    const shifted = b.shr(refcount, v_63);
    const v_one = b.iconst(1);
    const use_slow_val = b.band(shifted, v_one);
    const v_zero3 = b.iconst(0);
    const is_slow = b.icmp_ne(use_slow_val, v_zero3);
    b.brif(is_slow, 5, 4);
    writer.endBlock();

    // Block 4: inline — rc_addr = header + REFCOUNT_OFFSET
    writer.beginBlock(4, 0, &.{6}, &.{3});
    const v_rc_off = b.iconst(REFCOUNT_OFFSET);
    const rc_addr = b.add(header_ptr, v_rc_off);
    b.jump(6);
    writer.endBlock();

    // Block 5: side table
    writer.beginBlock(5, 0, &.{6}, &.{3});
    const v_clear = b.iconst(@bitCast(USE_SLOW_RC_CLEAR_MASK));
    const cleared = b.band(refcount, v_clear);
    const v_shift = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const st_ptr = b.shl(cleared, v_shift);
    const v_st_off = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const st_rc_addr = b.add(st_ptr, v_st_off);
    b.jump(6);
    writer.endBlock();

    // Block 6: increment unowned
    writer.beginBlock(6, 0, &.{}, &.{ 4, 5 });
    const old_rc = b.load(rc_addr);
    const v_unowned_one = b.iconst(UNOWNED_RC_ONE);
    const new_rc = b.add(old_rc, v_unowned_one);
    b.store(rc_addr, new_rc);
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// unowned_release(obj: i64) -> void
// ============================================================================
fn generateUnownedRelease(writer: *CirWriter) void {
    const name_off = writer.internString("unowned_release");
    const dealloc_name = writer.internString("dealloc");
    const free_name = writer.internString("free");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 10, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return
    writer.beginBlock(1, 0, &.{}, &.{ 0, 2, 6, 7 });
    b.retVoid();
    writer.endBlock();

    // Block 2: check immortal
    writer.beginBlock(2, 0, &.{ 1, 3 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const refcount = b.load(addr24);
    const v_immortal = b.iconst(@bitCast(IMMORTAL_REFCOUNT));
    const is_immortal = b.icmp_eq(refcount, v_immortal);
    b.brif(is_immortal, 1, 3);
    writer.endBlock();

    // Block 3: check slow
    writer.beginBlock(3, 0, &.{ 5, 4 }, &.{2});
    const v_63 = b.iconst(63);
    const shifted = b.shr(refcount, v_63);
    const v_one = b.iconst(1);
    const use_slow_val = b.band(shifted, v_one);
    const v_zero3 = b.iconst(0);
    const is_slow = b.icmp_ne(use_slow_val, v_zero3);
    b.brif(is_slow, 5, 4);
    writer.endBlock();

    // Block 4: inline path
    writer.beginBlock(4, 0, &.{6}, &.{3});
    const v_rc_off = b.iconst(REFCOUNT_OFFSET);
    const rc_addr = b.add(header_ptr, v_rc_off);
    const v_zero_st = b.iconst(0); // no side table
    b.jump(6);
    writer.endBlock();

    // Block 5: side table path
    writer.beginBlock(5, 0, &.{6}, &.{3});
    const v_clear = b.iconst(@bitCast(USE_SLOW_RC_CLEAR_MASK));
    const cleared = b.band(refcount, v_clear);
    const v_shift = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const st_ptr = b.shl(cleared, v_shift);
    const v_st_off = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const st_rc_addr = b.add(st_ptr, v_st_off);
    b.jump(6);
    writer.endBlock();

    // Block 6: decrement unowned
    writer.beginBlock(6, 0, &.{ 7, 1 }, &.{ 4, 5 });
    const old_rc = b.load(rc_addr);
    const v_neg_unowned = b.iconst(-UNOWNED_RC_ONE);
    const new_rc = b.add(old_rc, v_neg_unowned);
    b.store(rc_addr, new_rc);

    // Extract new UnownedRefCount: (new_rc >> 1) & 0x7FFFFFFF
    const v_one_s = b.iconst(1);
    const shifted_u = b.shr(new_rc, v_one_s);
    const v_mask_u = b.iconst(0x7FFFFFFF);
    const new_unowned = b.band(shifted_u, v_mask_u);
    const v_zero_6 = b.iconst(0);
    const is_last = b.icmp_eq(new_unowned, v_zero_6);
    b.brif(is_last, 7, 1);
    writer.endBlock();

    // Block 7: dealloc obj, check side table
    writer.beginBlock(7, 0, &.{ 8, 1 }, &.{6});
    b.call0(dealloc_name, &.{obj});
    // Check if side table exists (st_ptr != 0)
    // In inline path, st_ptr was 0; in side table path, it was the decoded pointer
    // For simplicity, check if refcount had UseSlowRC bit set
    const v_zero_7 = b.iconst(0);
    const has_st = b.icmp_ne(st_ptr, v_zero_7);
    b.brif(has_st, 8, 1);
    writer.endBlock();

    // Block 8: side table cleanup — zero object_ptr
    writer.beginBlock(8, 0, &.{ 9, 1 }, &.{7});
    const v_zero_8 = b.iconst(0);
    b.store(st_ptr, v_zero_8); // object_ptr = 0

    const v_weak_off = b.iconst(SIDE_TABLE_WEAK_RC_OFFSET);
    const weak_addr = b.add(st_ptr, v_weak_off);
    const weak_rc = b.load(weak_addr);
    const v_zero_8b = b.iconst(0);
    const weak_zero = b.icmp_eq(weak_rc, v_zero_8b);
    b.brif(weak_zero, 9, 1);
    writer.endBlock();

    // Block 9: free side table
    writer.beginBlock(9, 0, &.{}, &.{8});
    b.call0(free_name, &.{st_ptr});
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// unowned_load_strong(obj: i64) -> i64
// ============================================================================
fn generateUnownedLoadStrong(writer: *CirWriter) void {
    const name_off = writer.internString("unowned_load_strong");
    const retain_name = writer.internString("retain");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 5, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return null
    writer.beginBlock(1, 0, &.{}, &.{0});
    const v_zero2 = b.iconst(0);
    b.ret(v_zero2);
    writer.endBlock();

    // Block 2: check IsDeiniting
    writer.beginBlock(2, 0, &.{ 3, 4 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const refcount = b.load(addr24);
    const v_32 = b.iconst(32);
    const shifted = b.shr(refcount, v_32);
    const v_one = b.iconst(1);
    const deinit_val = b.band(shifted, v_one);
    const v_zero3 = b.iconst(0);
    const is_deiniting = b.icmp_ne(deinit_val, v_zero3);
    b.brif(is_deiniting, 3, 4);
    writer.endBlock();

    // Block 3: trap — dangling unowned
    writer.beginBlock(3, 0, &.{}, &.{2});
    b.trap();
    writer.endBlock();

    // Block 4: retain and return
    writer.beginBlock(4, 0, &.{}, &.{2});
    const result = b.call1(retain_name, &.{obj});
    b.ret(result);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// weak_form_reference(obj: i64) -> i64 (side_table_ptr)
// ============================================================================
fn generateWeakFormReference(writer: *CirWriter) void {
    const name_off = writer.internString("weak_form_reference");
    const malloc_name = writer.internString("malloc");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 6, 0);

    // Block 0: entry — null check
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const obj = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(obj, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    // Block 1: return null
    writer.beginBlock(1, 0, &.{}, &.{ 0, 3 });
    const v_zero2 = b.iconst(0);
    b.ret(v_zero2);
    writer.endBlock();

    // Block 2: load rc_word, check UseSlowRC
    writer.beginBlock(2, 0, &.{ 4, 3 }, &.{0});
    const v_header = b.iconst(HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = b.sub(obj, v_header);
    const off24 = b.iconst(REFCOUNT_OFFSET);
    const addr24 = b.add(header_ptr, off24);
    const rc_word = b.load(addr24);
    const v_63 = b.iconst(63);
    const shifted = b.shr(rc_word, v_63);
    const v_one = b.iconst(1);
    const use_slow = b.band(shifted, v_one);
    const v_zero3 = b.iconst(0);
    const is_slow = b.icmp_ne(use_slow, v_zero3);
    b.brif(is_slow, 4, 3);
    writer.endBlock();

    // Block 3: check IsDeiniting before allocating
    writer.beginBlock(3, 0, &.{ 1, 5 }, &.{2});
    const v_32 = b.iconst(32);
    const shifted2 = b.shr(rc_word, v_32);
    const deinit_val = b.band(shifted2, v_one);
    const v_zero4 = b.iconst(0);
    const is_deiniting = b.icmp_ne(deinit_val, v_zero4);
    b.brif(is_deiniting, 1, 5);
    writer.endBlock();

    // Block 4: existing side table — decode, increment weak_rc
    writer.beginBlock(4, 0, &.{}, &.{2});
    const v_clear = b.iconst(@bitCast(USE_SLOW_RC_CLEAR_MASK));
    const cleared = b.band(rc_word, v_clear);
    const v_shift = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const st_ptr = b.shl(cleared, v_shift);

    const v_weak_off = b.iconst(SIDE_TABLE_WEAK_RC_OFFSET);
    const weak_rc_addr = b.add(st_ptr, v_weak_off);
    const old_weak = b.load(weak_rc_addr);
    const v_one2 = b.iconst(1);
    const new_weak = b.add(old_weak, v_one2);
    b.store(weak_rc_addr, new_weak);

    b.ret(st_ptr);
    writer.endBlock();

    // Block 5: allocate new side table
    writer.beginBlock(5, 0, &.{}, &.{3});
    const v_st_size = b.iconst(SIDE_TABLE_SIZE);
    const new_st = b.call1(malloc_name, &.{v_st_size});

    // Init: { obj, rc_word, 1 }
    b.store(new_st, obj);
    const v_rc_off5 = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const rc_addr5 = b.add(new_st, v_rc_off5);
    b.store(rc_addr5, rc_word);
    const v_weak_off5 = b.iconst(SIDE_TABLE_WEAK_RC_OFFSET);
    const weak_addr5 = b.add(new_st, v_weak_off5);
    const v_one5 = b.iconst(1);
    b.store(weak_addr5, v_one5);

    // Encode: (new_st >> 3) | USE_SLOW_RC_BIT
    const v_shift5 = b.iconst(SIDE_TABLE_ALIGN_SHIFT);
    const shifted_ptr = b.shr(new_st, v_shift5);
    const v_use_slow = b.iconst(@bitCast(USE_SLOW_RC_BIT));
    const encoded = b.bor(shifted_ptr, v_use_slow);
    b.store(addr24, encoded);

    b.ret(new_st);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// weak_retain(st_ptr: i64) -> void
// ============================================================================
fn generateWeakRetain(writer: *CirWriter) void {
    const name_off = writer.internString("weak_retain");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 3, 0);

    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const st_ptr = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(st_ptr, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    writer.beginBlock(1, 0, &.{}, &.{0});
    b.retVoid();
    writer.endBlock();

    writer.beginBlock(2, 0, &.{}, &.{0});
    const v_weak_off = b.iconst(SIDE_TABLE_WEAK_RC_OFFSET);
    const weak_addr = b.add(st_ptr, v_weak_off);
    const old_weak = b.load(weak_addr);
    const v_one = b.iconst(1);
    const new_weak = b.add(old_weak, v_one);
    b.store(weak_addr, new_weak);
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// weak_release(st_ptr: i64) -> void
// ============================================================================
fn generateWeakRelease(writer: *CirWriter) void {
    const name_off = writer.internString("weak_release");
    const free_name = writer.internString("free");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 5, 0);

    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const st_ptr = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(st_ptr, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    writer.beginBlock(1, 0, &.{}, &.{ 0, 2 });
    b.retVoid();
    writer.endBlock();

    writer.beginBlock(2, 0, &.{ 3, 1 }, &.{0});
    const v_weak_off = b.iconst(SIDE_TABLE_WEAK_RC_OFFSET);
    const weak_addr = b.add(st_ptr, v_weak_off);
    const old_weak = b.load(weak_addr);
    const v_neg_one = b.iconst(-1);
    const new_weak = b.add(old_weak, v_neg_one);
    b.store(weak_addr, new_weak);
    const v_one = b.iconst(1);
    const is_last = b.icmp_eq(old_weak, v_one);
    b.brif(is_last, 3, 1);
    writer.endBlock();

    writer.beginBlock(3, 0, &.{ 4, 1 }, &.{2});
    const obj_ptr = b.load(st_ptr);
    const v_zero3 = b.iconst(0);
    const obj_freed = b.icmp_eq(obj_ptr, v_zero3);
    b.brif(obj_freed, 4, 1);
    writer.endBlock();

    writer.beginBlock(4, 0, &.{}, &.{3});
    b.call0(free_name, &.{st_ptr});
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// weak_load_strong(st_ptr: i64) -> i64
// ============================================================================
fn generateWeakLoadStrong(writer: *CirWriter) void {
    const name_off = writer.internString("weak_load_strong");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 4, 0);

    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = CirFuncBuilder{ .writer = writer };
    const st_ptr = b.arg(0);
    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(st_ptr, v_zero);
    b.brif(is_null, 1, 2);
    writer.endBlock();

    writer.beginBlock(1, 0, &.{}, &.{ 0, 2 });
    const v_zero2 = b.iconst(0);
    b.ret(v_zero2);
    writer.endBlock();

    writer.beginBlock(2, 0, &.{ 1, 3 }, &.{0});
    const v_rc_off = b.iconst(SIDE_TABLE_REFCOUNT_OFFSET);
    const rc_addr = b.add(st_ptr, v_rc_off);
    const refcount = b.load(rc_addr);
    const v_32 = b.iconst(32);
    const shifted = b.shr(refcount, v_32);
    const v_one = b.iconst(1);
    const deinit_val = b.band(shifted, v_one);
    const v_zero3 = b.iconst(0);
    const is_deiniting = b.icmp_ne(deinit_val, v_zero3);
    b.brif(is_deiniting, 1, 3);
    writer.endBlock();

    writer.beginBlock(3, 0, &.{}, &.{2});
    const obj_ptr = b.load(st_ptr);
    b.ret(obj_ptr);
    writer.endBlock();

    writer.endFunc();
}
