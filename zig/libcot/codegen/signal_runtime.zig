//! Signal Handler Runtime — CIR Generation
//!
//! Generates signal handling functions as CIR binary bytes.
//! Installs handlers for SIGILL, SIGSEGV, SIGBUS, SIGFPE, SIGABRT.
//!
//! Reference: native/signal_native.zig (CLIF IR version)
//! Reference: Go runtime/signal_unix.go (fatalsignal, sighandler)

const ssa_to_cir = @import("ssa_to_cir.zig");
const CirWriter = ssa_to_cir.CirWriter;

// CIR opcodes
const OP_CONST_INT: u16 = 0x0001;
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_UDIV: u16 = 0x0014;
const OP_UMOD: u16 = 0x0016;
const OP_AND: u16 = 0x0020;
const OP_OR: u16 = 0x0021;
const OP_SHR: u16 = 0x0024;
const OP_EQ: u16 = 0x0030;
const OP_NE: u16 = 0x0031;
const OP_ULT: u16 = 0x0036;
const OP_IREDUCE: u16 = 0x0042;
const OP_LOAD: u16 = 0x0070;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_FUNC_ADDR: u16 = 0x0095;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_TRAP: u16 = 0x00A2;
const OP_STACK_SLOT_DECL: u16 = 0x00B0;
const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
const OP_GLOBAL_VALUE: u16 = 0x0081;
const OP_SEXTEND: u16 = 0x0041;

const CIR_I8: u32 = 2;
const CIR_I32: u32 = 4;
const CIR_I64: u32 = 5;

// macOS ARM64 offsets
const SI_CODE_OFFSET: i64 = 8;
const SI_ADDR_OFFSET: i64 = 24;
const UC_MCONTEXT_OFFSET: i64 = 48;
const REGS_OFFSET: i64 = 16;
const FP_OFFSET: i64 = 232;
const LR_OFFSET: i64 = 240;
const SP_OFFSET: i64 = 248;
const PC_OFFSET: i64 = 256;

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
    fn band(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_AND, &.{ id, CIR_I64, l, r }); return id; }
    fn bor(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_OR, &.{ id, CIR_I64, l, r }); return id; }
    fn shr(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_SHR, &.{ id, CIR_I64, l, r }); return id; }
    fn udiv(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UDIV, &.{ id, CIR_I64, l, r }); return id; }
    fn urem(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UMOD, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_eq(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_EQ, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_ne(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_NE, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_ult(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ULT, &.{ id, CIR_I64, l, r }); return id; }
    fn load(self: *B, addr: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOAD, &.{ id, CIR_I64, addr }); return id; }
    fn load32(self: *B, addr: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOAD, &.{ id, CIR_I32, addr }); return id; }
    fn store(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I64, addr, val }); }
    fn store8(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I8, addr, val }); }
    fn ireduce8(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_IREDUCE, &.{ id, CIR_I8, val }); return id; }
    fn sextend(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_SEXTEND, &.{ id, CIR_I64, val }); return id; }
    fn select(self: *B, c: u32, t: u32, f: u32) u32 { const id = self.nextId(); self.writer.emit(OP_COND_SELECT, &.{ id, CIR_I64, c, t, f }); return id; }
    fn arg(self: *B, idx: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ARG, &.{ id, CIR_I64, idx }); return id; }
    fn localAddr(self: *B, slot: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOCAL_ADDR, &.{ id, CIR_I64, slot }); return id; }
    fn stackSlot(self: *B, slot_idx: u32, size: u32, alignment: u32) void { self.writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, size, alignment }); }
    fn funcAddr(self: *B, name_off: u32, param_count: u32, return_count: u32) u32 {
        const id = self.nextId();
        // Format: result_id, type, name_off, param_count, [param_types...], return_count, [return_types...]
        var buf: [32]u32 = undefined;
        buf[0] = id;
        buf[1] = CIR_I64;
        buf[2] = name_off;
        buf[3] = param_count;
        var pos: usize = 4;
        for (0..param_count) |_| { buf[pos] = CIR_I64; pos += 1; }
        buf[pos] = return_count;
        pos += 1;
        for (0..return_count) |_| { buf[pos] = CIR_I64; pos += 1; }
        self.writer.emit(OP_FUNC_ADDR, buf[0..pos]);
        return id;
    }
    fn ret(self: *B, val: u32) void { self.writer.emit(OP_RET, &.{val}); }
    fn retVoid(self: *B) void { self.writer.emit(OP_RET_VOID, &.{}); }
    fn jump(self: *B, target: u32) void { self.writer.emit(OP_JUMP, &.{ target, 0 }); }
    fn brif(self: *B, cond: u32, tb: u32, fb: u32) void { self.writer.emit(OP_BRIF, &.{ cond, tb, fb, 0, 0 }); }
    fn trap(self: *B) void { self.writer.emit(OP_TRAP, &.{}); }
    fn call1(self: *B, name_off: u32, args: []const u32) u32 {
        const result_id = self.nextId();
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 1; pos += 1; buf[pos] = result_id; pos += 1; buf[pos] = CIR_I64; pos += 1;
        buf[pos] = name_off; pos += 1; buf[pos] = @intCast(args.len); pos += 1;
        for (args) |a| { buf[pos] = CIR_I64; pos += 1; buf[pos] = a; pos += 1; }
        self.writer.emit(OP_STATIC_CALL, buf[0..pos]);
        return result_id;
    }
    fn call0(self: *B, name_off: u32, args: []const u32) void {
        var buf: [64]u32 = undefined;
        var pos: usize = 0;
        buf[pos] = 0; pos += 1; buf[pos] = name_off; pos += 1;
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

/// Generate signal handler runtime functions into the shared CIR writer.
pub fn generate(writer: *CirWriter) void {
    genPrintHex(writer);
    genSignalHandler(writer);
    genInstallSignals(writer);
    genPrintBacktrace(writer);
    genPrintSourceLoc(writer);
}

// ============================================================================
// __cot_print_hex(value: i64) -> void
// Stack-based hex printer. "0x" + 16 hex digits.
// ============================================================================
fn genPrintHex(writer: *CirWriter) void {
    const name_off = writer.internString("__cot_print_hex");
    const write_name = writer.internString("write");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 18, 1);
    const value = b.arg(0);
    const buf = b.localAddr(0);

    // Store "0x" prefix
    const ch_0 = b.iconst8('0');
    b.store8(buf, ch_0);
    const off1 = b.iconst(1);
    const buf1 = b.add(buf, off1);
    const ch_x = b.iconst8('x');
    b.store8(buf1, ch_x);

    // Convert to hex: 16 unrolled iterations
    const v_0xf = b.iconst(0xF);
    for (0..16) |i| {
        const shift_amt: i64 = @intCast(60 - @as(i32, @intCast(i)) * 4);
        const v_shift = b.iconst(shift_amt);
        const shifted = b.shr(value, v_shift);
        const nibble = b.band(shifted, v_0xf);

        const v_10 = b.iconst(10);
        const is_digit = b.icmp_ult(nibble, v_10);
        const v_0 = b.iconst('0');
        const v_a = b.iconst('a' - 10);
        const base = b.select(is_digit, v_0, v_a);
        const ch = b.add(nibble, base);
        const ch8 = b.ireduce8(ch);
        const off_i = b.iconst(@intCast(2 + i));
        const addr_i = b.add(buf, off_i);
        b.store8(addr_i, ch8);
    }

    // write(2, buf, 18)
    const fd = b.iconst(2);
    const len = b.iconst(18);
    _ = b.call1(write_name, &.{ fd, buf, len });
    b.retVoid();

    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __cot_signal_handler(sig, info, ucontext) -> void
// Go-style crash diagnostics.
// ============================================================================
fn genSignalHandler(writer: *CirWriter) void {
    const name_off = writer.internString("__cot_signal_handler");
    const write_name = writer.internString("write");
    const hex_name = writer.internString("__cot_print_hex");
    const bt_name = writer.internString("__cot_print_backtrace");
    const srcloc_name = writer.internString("__cot_print_source_loc");
    const exit_name = writer.internString("_exit");

    // Blocks: 0=entry, 1=sigill, 2=check_abrt, 3=abrt, 4=check_fpe, 5=fpe,
    //         6=check_bus, 7=bus, 8=check_segv, 9=segv, 10=other, 11=after_name
    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64 }, &.{}, 12, 0);

    // Block 0: entry — dispatch signal name
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 48, 8);
    const sig = b.arg(0);
    const info = b.arg(1);
    const ucontext = b.arg(2);

    const v_4 = b.iconst(4);
    const is_ill = b.icmp_eq(sig, v_4);
    b.brif(is_ill, 1, 2);
    writer.endBlock();

    // Block 1: SIGILL
    writer.beginBlock(1, 0, &.{11}, &.{0});
    const buf1 = b.localAddr(0);
    b.writeStr(write_name, buf1, b.iconst(2), "SIGILL: illegal instruction\n");
    b.jump(11);
    writer.endBlock();

    // Block 2: check SIGABRT
    writer.beginBlock(2, 0, &.{ 3, 4 }, &.{0});
    const v_6 = b.iconst(6);
    const is_abrt = b.icmp_eq(sig, v_6);
    b.brif(is_abrt, 3, 4);
    writer.endBlock();

    // Block 3: SIGABRT
    writer.beginBlock(3, 0, &.{11}, &.{2});
    const buf3 = b.localAddr(0);
    b.writeStr(write_name, buf3, b.iconst(2), "SIGABRT: abort\n");
    b.jump(11);
    writer.endBlock();

    // Block 4: check SIGFPE
    writer.beginBlock(4, 0, &.{ 5, 6 }, &.{2});
    const v_8 = b.iconst(8);
    const is_fpe = b.icmp_eq(sig, v_8);
    b.brif(is_fpe, 5, 6);
    writer.endBlock();

    // Block 5: SIGFPE
    writer.beginBlock(5, 0, &.{11}, &.{4});
    const buf5 = b.localAddr(0);
    b.writeStr(write_name, buf5, b.iconst(2), "SIGFPE: floating point exception\n");
    b.jump(11);
    writer.endBlock();

    // Block 6: check SIGBUS
    writer.beginBlock(6, 0, &.{ 7, 8 }, &.{4});
    const v_10 = b.iconst(10);
    const is_bus = b.icmp_eq(sig, v_10);
    b.brif(is_bus, 7, 8);
    writer.endBlock();

    // Block 7: SIGBUS
    writer.beginBlock(7, 0, &.{11}, &.{6});
    const buf7 = b.localAddr(0);
    b.writeStr(write_name, buf7, b.iconst(2), "SIGBUS: bus error\n");
    b.jump(11);
    writer.endBlock();

    // Block 8: check SIGSEGV
    writer.beginBlock(8, 0, &.{ 9, 10 }, &.{6});
    const v_11 = b.iconst(11);
    const is_segv = b.icmp_eq(sig, v_11);
    b.brif(is_segv, 9, 10);
    writer.endBlock();

    // Block 9: SIGSEGV
    writer.beginBlock(9, 0, &.{11}, &.{8});
    const buf9 = b.localAddr(0);
    b.writeStr(write_name, buf9, b.iconst(2), "SIGSEGV: segmentation fault\n");
    b.jump(11);
    writer.endBlock();

    // Block 10: other signal
    writer.beginBlock(10, 0, &.{11}, &.{8});
    const buf10 = b.localAddr(0);
    b.writeStr(write_name, buf10, b.iconst(2), "Signal\n");
    b.jump(11);
    writer.endBlock();

    // Block 11: after signal name — print PC, registers, backtrace, exit
    writer.beginBlock(11, 0, &.{}, &.{ 1, 3, 5, 7, 9, 10 });
    const buf11 = b.localAddr(0);
    const fd = b.iconst(2);

    // Load mcontext from ucontext
    const uc_off = b.iconst(UC_MCONTEXT_OFFSET);
    const mctx_addr = b.add(ucontext, uc_off);
    const mctx_ptr = b.load(mctx_addr);

    // regs_base = mcontext + 16
    const regs_off = b.iconst(REGS_OFFSET);
    const regs_base = b.add(mctx_ptr, regs_off);

    // Load PC
    const pc_off = b.iconst(PC_OFFSET);
    const pc_addr = b.add(regs_base, pc_off);
    const pc = b.load(pc_addr);

    // Load si_code
    const si_off = b.iconst(SI_CODE_OFFSET);
    const si_addr = b.add(info, si_off);
    const si_code_32 = b.load32(si_addr);
    const si_code = b.sextend(si_code_32);

    // Load fault address
    const fault_off = b.iconst(SI_ADDR_OFFSET);
    const fault_addr = b.add(info, fault_off);
    const fault = b.load(fault_addr);

    // Print "PC="
    b.writeStr(write_name, buf11, fd, "PC=");
    b.call0(hex_name, &.{pc});
    b.writeStr(write_name, buf11, fd, " sigcode=");
    b.call0(hex_name, &.{si_code});
    b.writeStr(write_name, buf11, fd, " addr=");
    b.call0(hex_name, &.{fault});
    b.writeStr(write_name, buf11, fd, "\n");

    // Print key registers: x0-x7, fp, lr, sp, pc
    const reg_names = [_][]const u8{ "x0  ", "x1  ", "x2  ", "x3  ", "x4  ", "x5  ", "x6  ", "x7  " };
    const reg_offsets = [_]i64{ 0, 8, 16, 24, 32, 40, 48, 56 };

    for (reg_names, reg_offsets) |rname, roff| {
        b.writeStr(write_name, buf11, fd, rname);
        const r_off = b.iconst(roff);
        const r_addr = b.add(regs_base, r_off);
        const r_val = b.load(r_addr);
        b.call0(hex_name, &.{r_val});
        b.writeStr(write_name, buf11, fd, "\n");
    }

    // Print fp, lr, sp, pc
    const special = [_]struct { name: []const u8, offset: i64 }{
        .{ .name = "fp  ", .offset = FP_OFFSET },
        .{ .name = "lr  ", .offset = LR_OFFSET },
        .{ .name = "sp  ", .offset = SP_OFFSET },
        .{ .name = "pc  ", .offset = PC_OFFSET },
    };
    for (special) |s| {
        b.writeStr(write_name, buf11, fd, s.name);
        const s_off = b.iconst(s.offset);
        const s_addr = b.add(regs_base, s_off);
        const s_val = b.load(s_addr);
        b.call0(hex_name, &.{s_val});
        b.writeStr(write_name, buf11, fd, "\n");
    }

    // Source location
    b.call0(srcloc_name, &.{pc});

    // Backtrace
    b.call0(bt_name, &.{});

    // Exit: sig + 128
    const v_128 = b.iconst(128);
    const exit_code = b.add(sig, v_128);
    b.call0(exit_name, &.{exit_code});
    b.trap();

    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __cot_install_signals() -> void
// ============================================================================
fn genInstallSignals(writer: *CirWriter) void {
    const name_off = writer.internString("__cot_install_signals");
    const malloc_name = writer.internString("malloc");
    const sigaltstack_name = writer.internString("sigaltstack");
    const sigaction_name = writer.internString("sigaction");
    const handler_name = writer.internString("__cot_signal_handler");

    writer.beginFuncWithSig(name_off, &.{}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 24, 8); // stack_t for sigaltstack
    b.stackSlot(1, 24, 8); // struct sigaction

    // Step 1: Allocate alternate signal stack
    const v_stksz = b.iconst(65536);
    const alt_stack = b.call1(malloc_name, &.{v_stksz});

    // Step 2: sigaltstack
    const ss_addr = b.localAddr(0);
    b.store(ss_addr, alt_stack); // ss_sp
    const off8 = b.iconst(8);
    const ss_size_addr = b.add(ss_addr, off8);
    b.store(ss_size_addr, v_stksz); // ss_size
    const off16 = b.iconst(16);
    const ss_flags_addr = b.add(ss_addr, off16);
    const v_zero_flags = b.iconst(0);
    b.store(ss_flags_addr, v_zero_flags);
    const v_null = b.iconst(0);
    _ = b.call1(sigaltstack_name, &.{ ss_addr, v_null });

    // Step 3: Setup sigaction struct
    const sa_addr = b.localAddr(1);
    const handler_addr = b.funcAddr(handler_name, 3, 0);
    b.store(sa_addr, handler_addr); // handler pointer at offset 0

    const sa_off8 = b.iconst(8);
    const sa_tramp = b.add(sa_addr, sa_off8);
    const v_zero_tramp = b.iconst(0);
    b.store(sa_tramp, v_zero_tramp); // sa_tramp = 0

    const sa_off16 = b.iconst(16);
    const sa_mask_addr = b.add(sa_addr, sa_off16);
    const v_zero_mask = b.iconst(0);
    b.store(sa_mask_addr, v_zero_mask); // sa_mask = 0

    const sa_off20 = b.iconst(20);
    const sa_flags_addr = b.add(sa_addr, sa_off20);
    // SA_SIGINFO=0x40 | SA_ONSTACK=0x1 = 0x41
    // Store as i32 at offset 20. Use i64 store and let backend truncate.
    const v_flags = b.iconst(0x41);
    b.store(sa_flags_addr, v_flags);

    // Install for each signal: 4(SIGILL), 6(SIGABRT), 8(SIGFPE), 10(SIGBUS), 11(SIGSEGV)
    const signals = [_]i64{ 4, 6, 8, 10, 11 };
    for (signals) |signum| {
        const v_sig = b.iconst(signum);
        const v_null_old = b.iconst(0);
        _ = b.call1(sigaction_name, &.{ v_sig, sa_addr, v_null_old });
    }

    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __cot_print_backtrace() -> void
// Calls libc backtrace() + backtrace_symbols_fd()
// ============================================================================
fn genPrintBacktrace(writer: *CirWriter) void {
    const name_off = writer.internString("__cot_print_backtrace");
    const bt_func_name = writer.internString("backtrace");
    const bt_syms_name = writer.internString("backtrace_symbols_fd");

    writer.beginFuncWithSig(name_off, &.{}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    // Stack slot for 128 void* pointers = 128 * 8 = 1024 bytes
    b.stackSlot(0, 1024, 8);

    const buf = b.localAddr(0);
    const v_128 = b.iconst(128);

    // nptrs = backtrace(buf, 128)
    const nptrs = b.call1(bt_func_name, &.{ buf, v_128 });

    // backtrace_symbols_fd(buf, nptrs, 2)
    const fd = b.iconst(2);
    b.call0(bt_syms_name, &.{ buf, nptrs, fd });

    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// __cot_print_source_loc(pc: i64) -> void
// Resolve PC to file:line via runtime source map (pctab/functab).
// Simplified: just a stub that calls the hex printer with the PC.
// The full implementation requires pctab/functab symbol access which
// is handled by the CIR consumer during compilation.
// ============================================================================
fn genPrintSourceLoc(writer: *CirWriter) void {
    const name_off = writer.internString("__cot_print_source_loc");
    const write_name = writer.internString("write");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer };
    b.stackSlot(0, 16, 8);

    _ = b.arg(0);

    // Write "  at <unknown>\n"
    const buf = b.localAddr(0);
    const fd = b.iconst(2);
    b.writeStr(write_name, buf, fd, "  at <unknown>\n");

    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}
