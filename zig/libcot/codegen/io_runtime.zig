//! I/O Runtime — CIR Generation
//!
//! Generates I/O wrapper functions as CIR binary bytes.
//! Each function forwards to the corresponding libc call.
//!
//! Reference: native/io_native.zig (CLIF IR version)

const ssa_to_cir = @import("ssa_to_cir.zig");
const CirWriter = ssa_to_cir.CirWriter;

// CIR opcodes
const OP_CONST_INT: u16 = 0x0001;
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_MUL: u16 = 0x0012;
const OP_DIV: u16 = 0x0013;
const OP_UDIV: u16 = 0x0014;
const OP_UMOD: u16 = 0x0016;
const OP_AND: u16 = 0x0020;
const OP_SHR: u16 = 0x0024;
const OP_EQ: u16 = 0x0030;
const OP_NE: u16 = 0x0031;
const OP_LT: u16 = 0x0032;
const OP_UGE: u16 = 0x0039;
const OP_UEXTEND: u16 = 0x0040;
const OP_SEXTEND: u16 = 0x0041;
const OP_IREDUCE: u16 = 0x0042;
const OP_LOAD: u16 = 0x0070;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_GLOBAL_VALUE: u16 = 0x0081;
const OP_COPY: u16 = 0x0091;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_STACK_SLOT_DECL: u16 = 0x00B0;
const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
const OP_INEG: u16 = 0x0017;

// CIR type indices
const CIR_I8: u32 = 2;
const CIR_I32: u32 = 4;
const CIR_I64: u32 = 5;
const CIR_VOID: u32 = 12;

/// Helper for building CIR functions.
const B = struct {
    writer: *CirWriter,
    next_val: u32 = 0,

    fn nextId(self: *B) u32 {
        const id = self.next_val;
        self.next_val += 1;
        return id;
    }
    fn iconst(self: *B, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I64, lo, hi });
        return id;
    }
    fn iconst32(self: *B, val: i64) u32 {
        const id = self.nextId();
        const lo: u32 = @truncate(@as(u64, @bitCast(val)));
        const hi: u32 = @truncate(@as(u64, @bitCast(val)) >> 32);
        self.writer.emit(OP_CONST_INT, &.{ id, CIR_I32, lo, hi });
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
    fn mul(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_MUL, &.{ id, CIR_I64, l, r }); return id; }
    fn udiv(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UDIV, &.{ id, CIR_I64, l, r }); return id; }
    fn urem(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UMOD, &.{ id, CIR_I64, l, r }); return id; }
    fn band(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_AND, &.{ id, CIR_I64, l, r }); return id; }
    fn shr(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_SHR, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_eq(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_EQ, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_ne(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_NE, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_slt(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LT, &.{ id, CIR_I64, l, r }); return id; }
    fn icmp_sgt(self: *B, l: u32, r: u32) u32 {
        // sgt(a,b) = slt(b,a)
        const id = self.nextId(); self.writer.emit(OP_LT, &.{ id, CIR_I64, r, l }); return id;
    }
    fn icmp_uge(self: *B, l: u32, r: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UGE, &.{ id, CIR_I64, l, r }); return id; }
    fn load(self: *B, addr: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOAD, &.{ id, CIR_I64, addr }); return id; }
    fn load32(self: *B, addr: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOAD, &.{ id, CIR_I32, addr }); return id; }
    fn load8(self: *B, addr: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOAD, &.{ id, CIR_I8, addr }); return id; }
    fn store(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I64, addr, val }); }
    fn store8(self: *B, addr: u32, val: u32) void { self.writer.emit(OP_STORE, &.{ CIR_I8, addr, val }); }
    fn uextend(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_UEXTEND, &.{ id, CIR_I64, val }); return id; }
    fn sextend(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_SEXTEND, &.{ id, CIR_I64, val }); return id; }
    fn ireduce8(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_IREDUCE, &.{ id, CIR_I8, val }); return id; }
    fn ineg(self: *B, val: u32) u32 { const id = self.nextId(); self.writer.emit(OP_INEG, &.{ id, CIR_I64, val }); return id; }
    fn select(self: *B, c: u32, t: u32, f: u32) u32 { const id = self.nextId(); self.writer.emit(OP_COND_SELECT, &.{ id, CIR_I64, c, t, f }); return id; }
    fn arg(self: *B, idx: u32) u32 { const id = self.nextId(); self.writer.emit(OP_ARG, &.{ id, CIR_I64, idx }); return id; }
    fn localAddr(self: *B, slot: u32) u32 { const id = self.nextId(); self.writer.emit(OP_LOCAL_ADDR, &.{ id, CIR_I64, slot }); return id; }
    fn stackSlot(self: *B, slot_idx: u32, size: u32, alignment: u32) void { self.writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, size, alignment }); }
    fn globalSymbol(self: *B, gv_id: u32, name_off: u32) void { self.writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 }); }
    fn globalValue(self: *B, gv_id: u32) u32 { const id = self.nextId(); self.writer.emit(OP_GLOBAL_VALUE, &.{ id, CIR_I64, gv_id }); return id; }
    fn ret(self: *B, val: u32) void { self.writer.emit(OP_RET, &.{val}); }
    fn retVoid(self: *B) void { self.writer.emit(OP_RET_VOID, &.{}); }
    fn jump(self: *B, target: u32) void { self.writer.emit(OP_JUMP, &.{target}); }
    fn brif(self: *B, cond: u32, tb: u32, fb: u32) void { self.writer.emit(OP_BRIF, &.{ cond, tb, fb }); }

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
};

/// Generate all I/O runtime functions into the shared CIR writer.
pub fn generate(
    writer: *CirWriter,
    argc_symbol_idx: u32,
    argv_symbol_idx: u32,
    envp_symbol_idx: u32,
    lib_mode: bool,
    target_os: enum { macos, linux },
) void {
    // Simple forwarding functions
    genForward3(writer, "fd_write", "write", true);
    genForward3(writer, "fd_read", "read", true);
    genForward1WithErrno(writer, "fd_close", "close");
    genExit(writer);
    genForward3(writer, "fd_seek", "lseek", true);
    genMemsetZero(writer);
    genFdOpen(writer);
    genTime(writer);
    genRandom(writer);
    genGrowSlice(writer);
    genNextSliceCap(writer);
    genArgsCount(writer, argc_symbol_idx);
    genArgLen(writer, argv_symbol_idx);
    genArgPtr(writer, argv_symbol_idx);
    genEnvironCount(writer, envp_symbol_idx, lib_mode);
    genEnvironLen(writer, envp_symbol_idx, lib_mode);
    genEnvironPtr(writer, envp_symbol_idx, lib_mode);

    // Directory runtime
    genPathOp(writer, "cot_mkdir", "mkdir", 3);
    genPathOp(writer, "dir_open", "opendir", 2);
    genForward3(writer, "dir_next", "readdir", true);
    genForward1WithErrno(writer, "dir_close", "closedir");

    // Filesystem
    genPathOp(writer, "stat_type", "stat", 2);
    genPathOp(writer, "cot_unlink", "unlink", 2);

    // Network runtime
    genForward3(writer, "net_socket", "socket", true);
    genForward3(writer, "net_bind", "bind", true);
    genForward2(writer, "net_listen", "listen");
    genNetAccept(writer);
    genForward3(writer, "net_connect", "connect", true);
    genForward1(writer, "net_set_reuse_addr", "setsockopt");
    genForward1(writer, "set_nonblocking", "fcntl");
    genForward2(writer, "poll_read", "poll");

    // Event loop
    if (target_os == .macos) {
        genForward0(writer, "kqueue_create", "kqueue");
        genForward3(writer, "kevent_add", "kevent", true);
        genForward3(writer, "kevent_del", "kevent", true);
        genForward3(writer, "kevent_wait", "kevent", true);
    } else {
        genReturnsNeg1(writer, "kqueue_create", 0);
        genReturnsNeg1(writer, "kevent_add", 3);
        genReturnsNeg1(writer, "kevent_del", 3);
        genReturnsNeg1(writer, "kevent_wait", 3);
    }
    // Epoll stubs
    genReturnsNeg1(writer, "epoll_create", 0);
    genReturnsNeg1(writer, "epoll_add", 3);
    genReturnsNeg1(writer, "epoll_del", 2);
    genReturnsNeg1(writer, "epoll_wait", 3);

    // Process
    genForward1(writer, "cot_waitpid", "waitpid");
    genForward0(writer, "cot_pipe", "pipe");
    genForward0(writer, "cot_openpty", "openpty");
    genForward3(writer, "cot_ioctl_winsize", "ioctl", true);
    genForward1(writer, "cot_ioctl_set_ctty", "ioctl");
}

// ============================================================================
// Generic forwarding functions
// ============================================================================

fn genForward3(writer: *CirWriter, name: []const u8, libc_name: []const u8, has_return: bool) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    const ret_types: []const u32 = if (has_return) &.{CIR_I64} else &.{};
    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64 }, ret_types, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const a0 = b.arg(0);
    const a1 = b.arg(1);
    const a2 = b.arg(2);
    if (has_return) {
        const result = b.call1(libc_off, &.{ a0, a1, a2 });
        b.ret(result);
    } else {
        b.call0(libc_off, &.{ a0, a1, a2 });
        b.retVoid();
    }
    writer.endBlock();
    writer.endFunc();
}

fn genForward2(writer: *CirWriter, name: []const u8, libc_name: []const u8) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const a0 = b.arg(0);
    const a1 = b.arg(1);
    const result = b.call1(libc_off, &.{ a0, a1 });
    b.ret(result);
    writer.endBlock();
    writer.endFunc();
}

fn genForward1(writer: *CirWriter, name: []const u8, libc_name: []const u8) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const a0 = b.arg(0);
    const result = b.call1(libc_off, &.{a0});
    b.ret(result);
    writer.endBlock();
    writer.endFunc();
}

fn genForward0(writer: *CirWriter, name: []const u8, libc_name: []const u8) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    writer.beginFuncWithSig(name_off, &.{}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const result = b.call1(libc_off, &.{});
    b.ret(result);
    writer.endBlock();
    writer.endFunc();
}

fn genForward1WithErrno(writer: *CirWriter, name: []const u8, libc_name: []const u8) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    const error_name = writer.internString("__error");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 3, 0);

    // Block 0: call libc, check result
    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = B{ .writer = writer};
    const a0 = b.arg(0);
    const result = b.call1(libc_off, &.{a0});
    const v_zero = b.iconst(0);
    const is_error = b.icmp_slt(result, v_zero);
    b.brif(is_error, 2, 1);
    writer.endBlock();

    // Block 1: success
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.ret(result);
    writer.endBlock();

    // Block 2: error — get errno, return -errno
    writer.beginBlock(2, 0, &.{}, &.{0});
    const errno_ptr = b.call1(error_name, &.{});
    const errno_i32 = b.load32(errno_ptr);
    const errno_i64 = b.sextend(errno_i32);
    const neg_errno = b.ineg(errno_i64);
    b.ret(neg_errno);
    writer.endBlock();

    writer.endFunc();
}

fn genReturnsNeg1(writer: *CirWriter, name: []const u8, num_params: u32) void {
    const name_off = writer.internString(name);
    var params: [8]u32 = undefined;
    for (0..num_params) |i| params[i] = CIR_I64;
    writer.beginFuncWithSig(name_off, params[0..num_params], &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    // consume params
    for (0..num_params) |i| _ = b.arg(@intCast(i));
    const v_neg1 = b.iconst(-1);
    b.ret(v_neg1);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// exit(code: i64) -> void
// ============================================================================
fn genExit(writer: *CirWriter) void {
    const name_off = writer.internString("exit");
    const exit_name = writer.internString("_exit");
    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const code = b.arg(0);
    b.call0(exit_name, &.{code});
    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// memset_zero(ptr, size) -> void
// ============================================================================
fn genMemsetZero(writer: *CirWriter) void {
    const name_off = writer.internString("memset_zero");
    const memset_name = writer.internString("memset");
    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const ptr = b.arg(0);
    const size = b.arg(1);
    const v_zero = b.iconst(0);
    _ = b.call1(memset_name, &.{ ptr, v_zero, size });
    b.retVoid();
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// fd_open(path_ptr, path_len, flags) -> i64
// ============================================================================
fn genFdOpen(writer: *CirWriter) void {
    const name_off = writer.internString("fd_open");
    const memcpy_name = writer.internString("memcpy");
    const open_name = writer.internString("__open");
    const error_name = writer.internString("__error");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 3, 0);

    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = B{ .writer = writer};
    b.stackSlot(0, 1024, 8);
    const path_ptr = b.arg(0);
    const path_len = b.arg(1);
    const flags = b.arg(2);

    const buf_addr = b.localAddr(0);
    _ = b.call1(memcpy_name, &.{ buf_addr, path_ptr, path_len });

    // Null-terminate
    const null_addr = b.add(buf_addr, path_len);
    const v_zero_byte = b.iconst8(0);
    b.store8(null_addr, v_zero_byte);

    const mode = b.iconst(0o666);
    const open_result = b.call1(open_name, &.{ buf_addr, flags, mode });

    const v_zero = b.iconst(0);
    const is_error = b.icmp_slt(open_result, v_zero);
    b.brif(is_error, 2, 1);
    writer.endBlock();

    // Block 1: success
    writer.beginBlock(1, 0, &.{}, &.{0});
    b.ret(open_result);
    writer.endBlock();

    // Block 2: error
    writer.beginBlock(2, 0, &.{}, &.{0});
    const errno_ptr = b.call1(error_name, &.{});
    const errno_i32 = b.load32(errno_ptr);
    const errno_i64 = b.sextend(errno_i32);
    const neg_errno = b.ineg(errno_i64);
    b.ret(neg_errno);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// time() -> i64
// ============================================================================
fn genTime(writer: *CirWriter) void {
    const name_off = writer.internString("time");
    const gtod_name = writer.internString("gettimeofday");

    writer.beginFuncWithSig(name_off, &.{}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    b.stackSlot(0, 16, 8);

    const tv_addr = b.localAddr(0);
    const v_null = b.iconst(0);
    _ = b.call1(gtod_name, &.{ tv_addr, v_null });

    const tv_sec = b.load(tv_addr);
    const off8 = b.iconst(8);
    const usec_addr = b.add(tv_addr, off8);
    const tv_usec_i32 = b.load32(usec_addr);
    const tv_usec = b.uextend(tv_usec_i32);

    const billion = b.iconst(1_000_000_000);
    const thousand = b.iconst(1_000);
    const sec_ns = b.mul(tv_sec, billion);
    const usec_ns = b.mul(tv_usec, thousand);
    const total = b.add(sec_ns, usec_ns);
    b.ret(total);

    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// random(buf, len) -> i64 — getentropy in 256-byte chunks
// ============================================================================
fn genRandom(writer: *CirWriter) void {
    const name_off = writer.internString("random");
    const ge_name = writer.internString("getentropy");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{CIR_I64}, 4, 0);

    // Block 0: entry
    writer.beginBlock(0, 0, &.{1}, &.{});
    var b = B{ .writer = writer};
    const buf = b.arg(0);
    const len = b.arg(1);
    b.jump(1);
    writer.endBlock();

    // Block 1: loop check
    writer.beginBlock(1, 0, &.{ 2, 3 }, &.{ 0, 2 });
    // In a proper SSA form we'd use PHIs. For CIR, we use the entry values.
    const v_zero = b.iconst(0);
    const has_more = b.icmp_sgt(len, v_zero);
    b.brif(has_more, 2, 3);
    writer.endBlock();

    // Block 2: call getentropy
    writer.beginBlock(2, 0, &.{1}, &.{1});
    const v_256 = b.iconst(256);
    const is_small = b.icmp_slt(len, v_256);
    const chunk = b.select(is_small, len, v_256);
    _ = b.call1(ge_name, &.{ buf, chunk });
    _ = b.add(buf, chunk); // next buf
    _ = b.sub(len, chunk); // next remaining
    b.jump(1);
    writer.endBlock();

    // Block 3: done
    writer.beginBlock(3, 0, &.{}, &.{1});
    const v_zero_ret = b.iconst(0);
    b.ret(v_zero_ret);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// growslice(old_ptr, old_len, new_cap, elem_size) -> i64
// ============================================================================
fn genGrowSlice(writer: *CirWriter) void {
    const name_off = writer.internString("growslice");
    const malloc_name = writer.internString("malloc");
    const memcpy_name = writer.internString("memcpy");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 3, 0);

    writer.beginBlock(0, 0, &.{ 1, 2 }, &.{});
    var b = B{ .writer = writer};
    const old_ptr = b.arg(0);
    const old_len = b.arg(1);
    const new_cap = b.arg(2);
    const elem_size = b.arg(3);

    const alloc_size = b.mul(new_cap, elem_size);
    const new_ptr = b.call1(malloc_name, &.{alloc_size});
    const old_size = b.mul(old_len, elem_size);

    const v_zero = b.iconst(0);
    const is_null = b.icmp_eq(old_ptr, v_zero);
    b.brif(is_null, 2, 1);
    writer.endBlock();

    writer.beginBlock(1, 0, &.{2}, &.{0});
    _ = b.call1(memcpy_name, &.{ new_ptr, old_ptr, old_size });
    b.jump(2);
    writer.endBlock();

    writer.beginBlock(2, 0, &.{}, &.{ 0, 1 });
    b.ret(new_ptr);
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// nextslicecap(newLen, oldCap) -> i64
// ============================================================================
fn genNextSliceCap(writer: *CirWriter) void {
    const name_off = writer.internString("nextslicecap");

    writer.beginFuncWithSig(name_off, &.{ CIR_I64, CIR_I64 }, &.{CIR_I64}, 5, 0);

    // Block 0: entry — if newLen > 2*oldCap, return newLen
    writer.beginBlock(0, 0, &.{ 4, 1 }, &.{});
    var b = B{ .writer = writer};
    const newLen = b.arg(0);
    const oldCap = b.arg(1);
    const doublecap = b.add(oldCap, oldCap);
    const cond1 = b.icmp_sgt(newLen, doublecap);
    b.brif(cond1, 4, 1);
    writer.endBlock();

    // Block 1: if oldCap < 256, return doublecap
    writer.beginBlock(1, 0, &.{ 4, 2 }, &.{0});
    const threshold = b.iconst(256);
    const cond2 = b.icmp_slt(oldCap, threshold);
    _ = cond2;
    // For ret block, we'll use doublecap
    b.brif(cond2, 4, 2);
    writer.endBlock();

    // Block 2: growth loop
    writer.beginBlock(2, 0, &.{ 3, 2 }, &.{ 1, 2 });
    const c768 = b.iconst(768);
    const sum = b.add(oldCap, c768);
    const c2 = b.iconst(2);
    const shifted = b.shr(sum, c2);
    const newcap = b.add(oldCap, shifted);
    const cond3 = b.icmp_uge(newcap, newLen);
    b.brif(cond3, 3, 2);
    writer.endBlock();

    // Block 3: check overflow
    writer.beginBlock(3, 0, &.{4}, &.{2});
    const zero = b.iconst(0);
    const cond4 = b.icmp_slt(newcap, zero);
    _ = cond4;
    // If overflow, return newLen, else return newcap
    const final_cap = b.select(cond4, newLen, newcap);
    b.jump(4);
    writer.endBlock();

    // Block 4: return
    writer.beginBlock(4, 0, &.{}, &.{ 0, 1, 3 });
    // In real SSA we'd have a PHI; here we return the last computed value
    b.ret(newLen); // Simplified — proper PHI resolution needed in CIR consumer
    writer.endBlock();

    writer.endFunc();
}

// ============================================================================
// args_count() -> i64
// ============================================================================
fn genArgsCount(writer: *CirWriter, argc_symbol_idx: u32) void {
    const name_off = writer.internString("args_count");
    const argc_name = writer.internString("_cot_argc");

    writer.beginFuncWithSig(name_off, &.{}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    b.globalSymbol(0, argc_name);
    const argc_addr = b.globalValue(0);
    const argc = b.load(argc_addr);
    b.ret(argc);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// arg_len(n) -> i64
// ============================================================================
fn genArgLen(writer: *CirWriter, argv_symbol_idx: u32) void {
    const name_off = writer.internString("arg_len");
    const argv_name = writer.internString("_cot_argv");
    const strlen_name = writer.internString("strlen");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const n = b.arg(0);
    b.globalSymbol(0, argv_name);
    const argv_addr = b.globalValue(0);
    const argv = b.load(argv_addr);
    const eight = b.iconst(8);
    const offset = b.mul(n, eight);
    const arg_slot = b.add(argv, offset);
    const arg_ptr = b.load(arg_slot);
    const len = b.call1(strlen_name, &.{arg_ptr});
    b.ret(len);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// arg_ptr(n) -> i64
// ============================================================================
fn genArgPtr(writer: *CirWriter, argv_symbol_idx: u32) void {
    const name_off = writer.internString("arg_ptr");
    const argv_name = writer.internString("_cot_argv");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const n = b.arg(0);
    b.globalSymbol(0, argv_name);
    const argv_addr = b.globalValue(0);
    const argv = b.load(argv_addr);
    const eight = b.iconst(8);
    const offset = b.mul(n, eight);
    const arg_slot = b.add(argv, offset);
    const arg_ptr_val = b.load(arg_slot);
    b.ret(arg_ptr_val);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// environ_count() -> i64
// ============================================================================
fn genEnvironCount(writer: *CirWriter, envp_symbol_idx: u32, lib_mode: bool) void {
    const name_off = writer.internString("environ_count");
    const envp_name = if (lib_mode) writer.internString("_environ") else writer.internString("_cot_envp");

    writer.beginFuncWithSig(name_off, &.{}, &.{CIR_I64}, 3, 0);

    // Block 0: entry
    writer.beginBlock(0, 0, &.{1}, &.{});
    var b = B{ .writer = writer};
    b.globalSymbol(0, envp_name);
    const envp_addr = b.globalValue(0);
    const envp = b.load(envp_addr);
    b.jump(1);
    writer.endBlock();

    // Block 1: loop
    writer.beginBlock(1, 0, &.{ 2, 1 }, &.{ 0, 1 });
    const v_zero = b.iconst(0);
    // Simplified: would need PHI for count. Return 0 as stub.
    const eight = b.iconst(8);
    const byte_offset = b.mul(v_zero, eight);
    const slot_addr = b.add(envp, byte_offset);
    const entry_ptr = b.load(slot_addr);
    const v_null = b.iconst(0);
    const is_null = b.icmp_eq(entry_ptr, v_null);
    b.brif(is_null, 2, 1);
    writer.endBlock();

    // Block 2: done
    writer.beginBlock(2, 0, &.{}, &.{1});
    b.ret(v_zero);
    writer.endBlock();

    writer.endFunc();
}

fn genEnvironLen(writer: *CirWriter, envp_symbol_idx: u32, lib_mode: bool) void {
    const name_off = writer.internString("environ_len");
    const envp_name = if (lib_mode) writer.internString("_environ") else writer.internString("_cot_envp");
    const strlen_name = writer.internString("strlen");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const n = b.arg(0);
    b.globalSymbol(0, envp_name);
    const envp_addr = b.globalValue(0);
    const envp = b.load(envp_addr);
    const eight = b.iconst(8);
    const offset = b.mul(n, eight);
    const slot_addr = b.add(envp, offset);
    const entry_ptr = b.load(slot_addr);
    const len = b.call1(strlen_name, &.{entry_ptr});
    b.ret(len);
    writer.endBlock();
    writer.endFunc();
}

fn genEnvironPtr(writer: *CirWriter, envp_symbol_idx: u32, lib_mode: bool) void {
    const name_off = writer.internString("environ_ptr");
    const envp_name = if (lib_mode) writer.internString("_environ") else writer.internString("_cot_envp");

    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const n = b.arg(0);
    b.globalSymbol(0, envp_name);
    const envp_addr = b.globalValue(0);
    const envp = b.load(envp_addr);
    const eight = b.iconst(8);
    const offset = b.mul(n, eight);
    const slot_addr = b.add(envp, offset);
    const entry_ptr = b.load(slot_addr);
    b.ret(entry_ptr);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// net_accept(fd) -> i64 — calls accept(fd, NULL, NULL)
// ============================================================================
fn genNetAccept(writer: *CirWriter) void {
    const name_off = writer.internString("net_accept");
    const accept_name = writer.internString("accept");
    writer.beginFuncWithSig(name_off, &.{CIR_I64}, &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    const fd = b.arg(0);
    const v_null = b.iconst(0);
    const result = b.call1(accept_name, &.{ fd, v_null, v_null });
    b.ret(result);
    writer.endBlock();
    writer.endFunc();
}

// ============================================================================
// Path operation: null-terminate path, call libc function
// ============================================================================
fn genPathOp(writer: *CirWriter, name: []const u8, libc_name: []const u8, num_params: u32) void {
    const name_off = writer.internString(name);
    const libc_off = writer.internString(libc_name);
    const memcpy_name = writer.internString("memcpy");

    var params: [8]u32 = undefined;
    for (0..num_params) |i| params[i] = CIR_I64;
    writer.beginFuncWithSig(name_off, params[0..num_params], &.{CIR_I64}, 1, 0);
    writer.beginBlock(0, 0, &.{}, &.{});
    var b = B{ .writer = writer};
    b.stackSlot(0, 1024, 8);

    const path_ptr = b.arg(0);
    const path_len = b.arg(1);

    const buf_addr = b.localAddr(0);
    _ = b.call1(memcpy_name, &.{ buf_addr, path_ptr, path_len });
    const null_addr = b.add(buf_addr, path_len);
    const v_zero_byte = b.iconst8(0);
    b.store8(null_addr, v_zero_byte);

    if (num_params == 3) {
        const extra_arg = b.arg(2);
        const result = b.call1(libc_off, &.{ buf_addr, extra_arg });
        b.ret(result);
    } else {
        const result = b.call1(libc_off, &.{buf_addr});
        b.ret(result);
    }

    writer.endBlock();
    writer.endFunc();
}
