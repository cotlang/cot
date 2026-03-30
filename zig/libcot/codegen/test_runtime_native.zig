//! Test Runtime — CIR Generation
//!
//! Generates test runner functions as CIR binary bytes.
//! Tracks pass/fail counts and outputs test results via write(fd, buf, len).
//!
//! Reference: native/test_native.zig (CLIF IR version)

const ssa_to_cir = @import("ssa_to_cir.zig");
const CirWriter = ssa_to_cir.CirWriter;

// CIR opcodes
const OP_CONST_INT: u16 = 0x0001;
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_UDIV: u16 = 0x0014;
const OP_UMOD: u16 = 0x0016;
const OP_EQ: u16 = 0x0030;
const OP_LT: u16 = 0x0032;
const OP_ULT: u16 = 0x0036;
const OP_IREDUCE: u16 = 0x0042;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_STACK_SLOT_DECL: u16 = 0x00B0;

const CIR_I8: u32 = 2;
const CIR_I64: u32 = 5;

const B = struct {
    writer: *CirWriter,
    next_val: u32 = 0,
    fn nextId(self: *B) u32 { const id = self.next_val; self.next_val += 1; return id; }
    fn iconst(self: *B, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I64, lo, hi });
        return id;
    }
    fn iconst8(self: *B, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I8, lo, hi });
        return id;
    }
    fn add(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ADD, &.{ id, CIR_I64, l, r }); return id; }
    fn sub(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_SUB, &.{ id, CIR_I64, l, r }); return id; }
    fn udiv(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UDIV, &.{ id, CIR_I64, l, r }); return id; }
    fn urem(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UMOD, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_sgt(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LT, &.{ id, CIR_I64, r, l }); return id; }
    fn icmp_ult(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ULT, &.{ id, CIR_I64, l, r }); return id; }
    fn ireduce8(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_IREDUCE, &.{ id, CIR_I8, val }); return id; }
    fn select(self: *B, c: u32, t: u32, f: u32) u32 { const id = self.nextId(); self.writer.emit(OP_COND_SELECT, &.{ id, CIR_I64, c, t, f }); return id; }
    fn store8(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I8, addr, val }); }
    fn store(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I64, addr, val }); }
    fn arg(self: *B, idx: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ARG, &.{ id, CIR_I64, idx }); return id; }
    fn localAddr(self: *B, slot: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOCAL_ADDR, &.{ id, CIR_I64, slot }); return id; }
    fn stackSlot(self: *B, slot_idx: u32, size: u32, alignment: u32) void { self.writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, size, alignment }); }
    fn ret(self: *B, val: u32) void { self.writer.emit(OP_RET, &.{val}); }
    fn retVoid(self: *B) void { self.writer.emit(OP_RET_VOID, &.{}); }
    fn jump(self: *B, target: u32) void { self.writer.emit(OP_JUMP, &.{ target, 0 }); }
    fn brif(self: *B, cond: u32, tb: u32, fb: u32) void { self.writer.emit(OP_BRIF, &.{ cond, tb, fb, 0, 0 }); }
    fn call1(self: *B, name_off: u32, args: []const u32) u32 {
        const result_id = self.nextId();
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 1; pos += 1;
        buf[pos] = result_id; pos += 1;
        buf[pos] = CIR_I64; pos += 1;
        buf[pos] = name_off; pos += 1;
        buf[pos] = @intCast(args.len); pos += 1;
        for (args) |a| { buf[pos] = CIR_I64; pos += 1; buf[pos] = a; pos += 1; }
        self.writer.emit(OP_STATIC_CALL, buf[0..pos]);
        return result_id;
    }
    fn call0(self: *B, name_off: u32, args: []const u32) void {
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 0; pos += 1;
        buf[pos] = name_off; pos += 1;
        buf[pos] = @intCast(args.len); pos += 1;
        for (args) |a| { buf[pos] = CIR_I64; pos += 1; buf[pos] = a; pos += 1; }
        self.writer.emit(OP_STATIC_CALL, buf[0..pos]);
    }

    fn storeString(self: *B, base_addr: u32, str: []const u8) void {
        for (str, 0..) |byte, i| {
            const ch = self.iconst8(@intCast(byte));
            const off = self.iconst(@intCast(i));
            const addr = self.add(base_addr, off);
            self.store8(addr, ch);
        }
    }

    fn writeStr(self: *B, write_name: u32, base_addr: u32, fd: u32, str: []const u8) void {
        self.storeString(base_addr, str);
        const len = self.iconst(@intCast(str.len));
        _ = self.call1(write_name, &.{ fd, base_addr, len });
    }
};

/// Generate all test runtime functions into the shared CIR writer.
pub fn generate(writer: *CirWriter) void {
    genNoop(writer, "__test_begin", 0);
    genTestPrintName(writer);
    genTestWriteStr(writer, "__test_pass", "ok\n");
    genTestWriteStr(writer, "__test_fail", "FAIL\n");
    genTestSummary(writer);
    genNoop(writer, "__test_store_fail_values", 5);
}

// ============================================================================
// No-op function: (optional params) -> void
// ============================================================================
fn genNoop(writer: *CirWriter, name: []const u8, num_params: u32) void {
    const name_off = writer.internString(name);
    var params: [8]u32 = undefined;
    for (0..num_params) |i| params[i] = CIR_I64;
    writer.beginFuncWithSig(name_off, params[0..num_params], &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    for (0..num_params) |i| _ = b.arg(@intCast(i));
    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __test_print_name(ptr: i64, len: i64) -> void
// Writes: 'test "' + name + '" ... '
// ============================================================================
fn genTestPrintName(writer: *CirWriter) void {
    const name_off = writer.internString("__test_print_name");
    const write_name = writer.internString("write");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 16, 8);

    const name_ptr = b.arg(0);
    const name_len = b.arg(1);
    const fd = b.iconst(2);
    const buf_addr = b.localAddr(0);

    // Write: 'test "'
    b.writeStr(write_name, buf_addr, fd, "test \"");

    // Write name
    _ = b.call1(write_name, &.{ fd, name_ptr, name_len });

    // Write: '" ... '
    b.writeStr(write_name, buf_addr, fd, "\" ... ");

    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// Write a fixed string to stderr
// ============================================================================
fn genTestWriteStr(writer: *CirWriter, name: []const u8, str: []const u8) void {
    const name_off = writer.internString(name);
    const write_name = writer.internString("write");

    writer.beginFuncWithSig(name_off, &.{}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, @intCast(str.len), 1);
    const buf_addr = b.localAddr(0);
    const fd = b.iconst(2);
    b.writeStr(write_name, buf_addr, fd, str);
    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __test_summary(pass_count: i64, fail_count: i64) -> void
// Prints: "\nok | N passed\n" or "\nFAILED | N passed | M failed\n"
// ============================================================================
fn genTestSummary(writer: *CirWriter) void {
    const name_off = writer.internString("__test_summary");
    const write_name = writer.internString("write");

    // Blocks: 0=entry, 1=failed, 2=ok, 3=merge
    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{}, 4, 0);

    // Block 0: entry
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 64, 8);
    const pass_count = b.arg(0);
    const fail_count = b.arg(1);
    const fd = b.iconst(2);
    const buf = b.localAddr(0);

    // Write "\n"
    b.writeStr(write_name, buf, fd, "\n");

    const zero = b.iconst(0);
    const has_fails = b.icmp_sgt(fail_count, zero);
    b.brif(has_fails, 1, 2);
    writer.endBlock();

    // Block 1: failed path
    writer.beginBlock(1, 0, &.{3}, &.{0});
    b.writeStr(write_name, buf, fd, "FAILED | ");
    emitPrintInt(&b, write_name, buf, fd, pass_count);
    b.writeStr(write_name, buf, fd, " passed | ");
    emitPrintInt(&b, write_name, buf, fd, fail_count);
    b.writeStr(write_name, buf, fd, " failed\n");
    b.jump(3);
    writer.endBlock();

    // Block 2: ok path
    writer.beginBlock(2, 0, &.{3}, &.{0});
    b.writeStr(write_name, buf, fd, "ok | ");
    emitPrintInt(&b, write_name, buf, fd, pass_count);
    b.writeStr(write_name, buf, fd, " passed\n");
    b.jump(3);
    writer.endBlock();

    // Block 3: merge
    writer.beginBlock(3, 0, &.{}, &.{ 1, 2 });
    b.retVoid();
    writer.endBlock();

    writer.endFunc();
}

/// Emit inline integer printing (up to 4 digits for test counts).
fn emitPrintInt(b: *B, write_name: u32, buf: u32, fd: u32, val: u32) void {
    const ten = b.iconst(10);
    const zero_char = b.iconst8('0');
    const buf_off = b.iconst(32); // use offset 32 in buffer
    const num_buf = b.add(buf, buf_off);

    // Digit 0 (ones)
    const d0 = b.urem(val, ten);
    const d0_i8 = b.ireduce8(d0);
    const d0_ch = b.add(d0_i8, zero_char);
    const off9 = b.iconst(9);
    const addr9 = b.add(num_buf, off9);
    b.store8(addr9, d0_ch);

    // Digit 1 (tens)
    const v1 = b.udiv(val, ten);
    const d1 = b.urem(v1, ten);
    const d1_i8 = b.ireduce8(d1);
    const d1_ch = b.add(d1_i8, zero_char);
    const off8 = b.iconst(8);
    const addr8 = b.add(num_buf, off8);
    b.store8(addr8, d1_ch);

    // Digit 2 (hundreds)
    const v2 = b.udiv(v1, ten);
    const d2 = b.urem(v2, ten);
    const d2_i8 = b.ireduce8(d2);
    const d2_ch = b.add(d2_i8, zero_char);
    const off7 = b.iconst(7);
    const addr7 = b.add(num_buf, off7);
    b.store8(addr7, d2_ch);

    // Digit 3 (thousands)
    const v3 = b.udiv(v2, ten);
    const d3 = b.urem(v3, ten);
    const d3_i8 = b.ireduce8(d3);
    const d3_ch = b.add(d3_i8, zero_char);
    const off6 = b.iconst(6);
    const addr6 = b.add(num_buf, off6);
    b.store8(addr6, d3_ch);

    // Find start position
    const hundred = b.iconst(100);
    const thousand = b.iconst(1000);

    // Default: >= 1000 (4 digits from offset 6)
    var start = b.iconst(6);
    var len = b.iconst(4);

    const lt1000 = b.icmp_ult(val, thousand);
    start = b.select(lt1000, b.iconst(7), start);
    len = b.select(lt1000, b.iconst(3), len);

    const lt100 = b.icmp_ult(val, hundred);
    start = b.select(lt100, b.iconst(8), start);
    len = b.select(lt100, b.iconst(2), len);

    const lt10 = b.icmp_ult(val, ten);
    start = b.select(lt10, b.iconst(9), start);
    len = b.select(lt10, b.iconst(1), len);

    const write_addr = b.add(num_buf, start);
    _ = b.call1(write_name, &.{ fd, write_addr, len });
}
