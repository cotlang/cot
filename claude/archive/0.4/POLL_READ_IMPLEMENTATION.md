# `poll_read` Runtime Function — Implementation Investigation

## Status: BLOCKED — aarch64-linux backend bug with stack slots in call args

The `generatePollRead` code is correct — it follows the exact same pattern as `generateKeventAdd`/`generateKeventWait`. The crash is a compiler backend bug: the `stackStore` + `stackAddr` + pass-address-to-call pattern has never been tested on aarch64-linux because all existing users (kevent_add, kevent_delete, kevent_wait) are guarded by `if (target_os == .macos)`.

## Why This Is Needed

Cotty's terminal IO reader thread does blocking reads one chunk at a time. Between chunks, the main thread renders and sees partial terminal state — specifically, zsh's PROMPT_SP `%` character is visible before the overwrite sequence (`\r` + space + `\r`) is processed.

Ghostty solves this with a non-blocking drain pattern (Exec.zig:1248-1347):
1. `set_nonblocking(fd)` — make PTY fd non-blocking
2. `poll(fd, POLLIN, -1)` — block until data available
3. Tight `read()` loop — drain ALL available data under one mutex lock
4. Signal render — main thread only sees complete state

Cot already has `set_nonblocking` in the runtime. It needs `poll_read(fd, timeout_ms)` to wait for data on a non-blocking fd.

## What Was Attempted

### Changes Made (all on `main` branch, uncommitted in working tree)

**1. `compiler/driver.zig`** — Added `"poll"` to `runtime_func_names` at the END of the array (line ~1395):
```zig
"dladdr",
// poll — used by poll_read runtime for non-blocking IO drain
"poll",
```
**CRITICAL**: Must be at the END. Adding it in the middle shifts all subsequent function indices and breaks every existing extern call.

**2. `stdlib/sys.cot`** — Added extern declaration after `set_nonblocking` (line 90):
```cot
extern fn poll_read(fd: i64, timeout: i64) i64
```

**3. `compiler/codegen/native/io_native.zig`** — Registration after `set_nonblocking` (line ~228):
```zig
// poll_read(fd, timeout_ms) → i64  (calls libc poll)
try result.append(allocator, .{
    .name = "poll_read",
    .compiled = try generatePollRead(allocator, isa, ctrl_plane, func_index_map),
});
```

**4. `compiler/codegen/native/io_native.zig`** — Implementation after `generateSetNonblocking` (line ~1870):

```zig
fn generatePollRead(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const fd = params[0];
    const timeout_ms = params[1];

    // Build struct pollfd on stack: { fd: i32, events: i16 = POLLIN, revents: i16 = 0 }
    // Pack as single i64: lower 32 bits = fd, bits 32-47 = POLLIN (1)
    const pfd_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 3));
    const fd_masked = try ins.band(fd, try ins.iconst(clif.Type.I64, 0xFFFFFFFF));
    const pollin_shifted = try ins.iconst(clif.Type.I64, @as(i64, 1) << 32);
    const packed_pfd = try ins.bor(fd_masked, pollin_shifted);
    _ = try ins.stackStore(packed_pfd, pfd_slot, 0);
    const pfd_addr = try ins.stackAddr(clif.Type.I64, pfd_slot, 0);

    // Call poll(fds, nfds, timeout)
    const poll_idx = func_index_map.get("poll") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = poll_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_one = try ins.iconst(clif.Type.I64, 1);
    const call_result = try ins.call(func_ref, &[_]clif.Value{ pfd_addr, v_one, timeout_ms });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
```

## The Crash

Every compiled binary crashes with SIGSEGV (signal 11) at startup. The crash occurs during native code compilation, not at runtime — `generatePollRead` is compiled for EVERY binary as part of the runtime function set, so even `cot test /tmp/hello.cot` crashes.

### Reproduction
```bash
# 1. Apply the changes above
# 2. Build compiler
zig build
# 3. Any cot command crashes:
./zig-out/bin/cot test test/e2e/features.cot
# Output: Signal 1 / PC= / Test program killed by signal: 11
```

### Isolation
- With `generatePollRead` registration commented out → all 360 tests pass
- With `generatePollRead` registered → immediate SIGSEGV on any binary
- The `"poll"` addition to `runtime_func_names` alone (without generatePollRead) is fine

## What Was Investigated

### Approach 1: `store` with sub-word types (I32, I16)
```zig
const fd_i32 = try ins.ireduce(clif.Type.I32, fd);
_ = try ins.store(.{}, fd_i32, pfd_addr, 0);
const pollin = try ins.iconst(clif.Type.I16, 1);
_ = try ins.store(.{}, pollin, pfd_addr, 4);
```
Result: Same crash. Sub-word `store` to stack addresses may have issues on aarch64-linux.

### Approach 2: Pack as I64 + `stackStore`
```zig
const packed_pfd = try ins.bor(fd_masked, pollin_shifted);
_ = try ins.stackStore(packed_pfd, pfd_slot, 0);
```
Result: Same crash. The combination of `stackStore` + `stackAddr` + `call` crashes.

### Approach 3: Return constant (no stack, no call)
NOT tested yet — would confirm whether the crash is in the stack slot allocation, the external call, or something else.

## Reference Patterns That DO Work

### `generateSetNonblocking` (io_native.zig:1812-1865)
- Simple 2-param function calling `fcntl` (variadic)
- No stack allocation, no struct packing
- Works on both macOS and aarch64-linux

### `generateKeventWait` (io_native.zig:2049-2115)
- Builds `struct timespec` on stack (16 bytes) using `createSizedStackSlot` + `stackStore`
- Calls `kevent` with stack address as argument
- **Only compiled on macOS** (`if (target_os == .macos)`) — never tested on aarch64-linux

This is a key clue: ALL the functions that use `createSizedStackSlot` + `stackStore` + `stackAddr` + pass-to-call are guarded by `if (target_os == .macos)`:
- `generateKeventAdd` — macOS only
- `generateKeventDelete` — macOS only
- `generateKeventWait` — macOS only
- `generateIoctlWinsize` — macOS only

**The stack-struct-to-call pattern has NEVER been tested on aarch64-linux.** The poll_read crash likely exposes a bug in the aarch64 backend's handling of stack slots passed as function call arguments.

## How to Reproduce on macOS

The crash is aarch64-linux specific. On macOS arm64, the same code might work (since the `kevent_*` functions use the same pattern and they work on macOS). To investigate:

1. **Apply the changes** listed in "Files Changed" below
2. **Build**: `zig build`
3. **Test on macOS**: `./zig-out/bin/cot test test/e2e/features.cot` — may pass on macOS
4. **Cross-compile test**: `./zig-out/bin/cot build test/e2e/features.cot --target=arm64-linux` — this should reproduce the CLIF lowering crash since the codegen runs on macOS but targets aarch64-linux
5. **Use `COT_DEBUG=codegen`** to dump the CLIF IR and compare with `generateKeventWait`

If the crash only happens on the Linux target, the bug is in `compiler/codegen/native/isa/aarch64/` — specifically how stack slot addresses are handled in call arguments.

If it crashes on both targets, the bug is in the shared `machinst/` or `ir/clif/` layers.

## Recommended Investigation

### ROOT CAUSE IDENTIFIED: stackStore codegen drops the store instruction

**Disassembly of `poll_read` shows the `stackStore` value (x10) is computed but NEVER written to the stack:**

```asm
sub sp, sp, #0x10        ; allocate stack slot
and x10, x0, x9          ; mask fd
orr x10, x10, x11        ; OR with POLLIN << 32
mov x0, sp               ; x0 = &pollfd (UNINITIALIZED!)
mov x2, x1               ; x2 = timeout
mov x1, #1               ; x1 = nfds
bl poll@plt               ; poll gets garbage struct
```

**Missing instruction:** `str x10, [sp]` should appear between the `orr` and `mov x0, sp`. The CLIF `stack_store` instruction is lowered by `lowerStackStore` (lower.zig:2990) which calls `ctx.emit(Inst.genStore(...))` — but the store is silently dropped.

This means `poll()` receives an uninitialized `struct pollfd` on the stack, which has random values for `fd` and `events`, causing it to either timeout immediately or monitor the wrong fd.

**This is the same bug that would affect ALL kevent_* functions on Linux** — they're currently macOS-only so the bug was never exposed.

### Previous (stale) root cause analysis

The crash is NOT in the CLIF IR lowering. The binary compiles successfully (`Success: /tmp/test_trivial`). The crash is at **runtime** during `__cot_init_globals` → `sched_select`:

```
#0  0x00000000010140c4 in sched_select ()
#1  0x0000000001014bf8 in __cot_signal_handler ()
#2  0x000000000101145c in __cot_init_globals ()
#3  0x000000000101143c in __cot_main ()
#4  0x00000000010159a0 in main ()
```

The signal handler installation crashes, which means **function indices are shifted** by adding the new `poll_read` runtime function.

**Key insight**: `generateMachODirect` (driver.zig:3426-3431) uses `num_funcs + N` for hardcoded external name indices:
```zig
const signal_handler_ext_idx: u32 = num_funcs + 3;
const sigaction_ext_idx: u32 = num_funcs + 4;
```

Adding `poll_read` from io_native.zig increases `compiled_funcs.len` (which is `num_funcs`), which shifts these indices. BUT this should be self-consistent — `num_funcs` already includes the new function.

The real issue may be that the signal handler's CLIF IR generates `call` instructions to `sigaction` using the function index from `func_index_map`, but the signal handler's code is generated BEFORE `generateMachODirect` assigns the final external name indices. So the signal handler's `sigaction` call uses a stale index.

**Investigation path**: Check `signal_native.zig` — does it look up `"sigaction"` from `func_index_map`? If so, that index was set during `runtime_func_names` registration, which uses `runtime_start_idx + position`. Adding `"poll"` at the end of `runtime_func_names` doesn't shift existing entries. But the compiled functions from io_native.zig are in a SEPARATE list — `compiled_funcs` — and adding one there shifts the base indices for external names.

**Trace the index flow**:
1. `func_index_map["sigaction"]` = `runtime_start_idx + 57` (its position in runtime_func_names)
2. Signal handler CLIF IR calls function at index `runtime_start_idx + 57`
3. In `generateMachODirect`, external name at index `runtime_start_idx + 57` should resolve to `sigaction`
4. But if adding `poll_read` shifts the external name assignment, index 57 now points to a different function

### Step 1: Confirm the crash is stack-related
Create a minimal `generatePollRead` that just returns a constant (no stack, no call):
```zig
const v = try ins.iconst(clif.Type.I64, 42);
_ = try ins.return_(&[_]clif.Value{v});
```
If this works, the crash is in the stack/call code, not in the function registration.

### Step 2: Isolate stack vs call
Try just the stack allocation without the call:
```zig
const slot = try builder.createSizedStackSlot(...);
_ = try ins.stackStore(packed, slot, 0);
const addr = try ins.stackAddr(clif.Type.I64, slot, 0);
_ = try ins.return_(&[_]clif.Value{addr});
```
If this crashes, the bug is in stack slot handling on aarch64.

### Step 3: Reference comparison
Compare the CLIF IR generated by `generatePollRead` with `generateKeventWait`. Use `COT_DEBUG=codegen` to dump the IR. Look for differences in how stack slots are referenced in the `call` instruction arguments.

### Step 4: Check Cranelift reference
The stack slot → call argument pattern maps to Cranelift's `stack_addr` + `call` pattern. Check:
- `references/wasmtime/cranelift/codegen/src/isa/aarch64/lower.rs` — how `stack_addr` is lowered
- `references/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs` — how the address is materialized

## Files Changed (Uncommitted)

| File | Change |
|------|--------|
| `compiler/driver.zig:~1395` | Added `"poll"` to `runtime_func_names` (at END) |
| `compiler/codegen/native/io_native.zig:~228` | Registered `generatePollRead` in `generate()` |
| `compiler/codegen/native/io_native.zig:~1870` | Implemented `generatePollRead()` |
| `stdlib/sys.cot:90` | Added `extern fn poll_read(fd: i64, timeout: i64) i64` |

## End Goal

Once `poll_read` works, implement the Ghostty IO drain pattern in `libcotty/src/surface.cot`:

```cot
fn ioReaderMain(arg: i64) void {
    const surface = @intToPtr(*Surface, arg)
    const fd = surface.shell.master_fd
    set_nonblocking(fd)
    var buf = alloc(0, 4096)
    defer dealloc(buf)
    while (true) {
        const poll_result = poll_read(fd, -1)
        if (poll_result < 0) { break }
        surface.terminal_mutex.lock()
        var got_data = false
        while (true) {
            const n = fd_read(fd, buf, 4096)
            if (n <= 0) { break }
            got_data = true
            for i in 0..n {
                surface.parser.feed(surface.terminal, @intToPtr(*u8, buf + i).*)
            }
        }
        surface.terminal_mutex.unlock()
        if (not got_data) { break }
        fd_write(surface.notify_pipe_write, @ptrOf("!"), 1)
    }
}
```

Then remove the `setopt no_prompt_sp` hack from `libcotty/shell-integration/zsh/.zshenv`.
