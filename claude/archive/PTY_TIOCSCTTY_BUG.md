# Bug: PTY child has no controlling terminal — SIGWINCH never delivered

## Summary

Cotty's `Pty.spawn()` calls `setsid()` in the child process but never calls `ioctl(slave_fd, TIOCSCTTY, 0)` to set the controlling terminal. Without this, the kernel does not deliver SIGWINCH when TIOCSWINSZ is called on the master fd. This means **terminal resize is completely broken** — no child process (top, vim, claude, etc.) ever receives SIGWINCH.

## Root Cause

On macOS (and POSIX generally), after `setsid()` creates a new session, the process has no controlling terminal. Opening a terminal device would normally set it as the controlling terminal, but in the fork+exec pattern used by PTY spawn, the slave fd was opened **before** `setsid()` (in the parent, before fork). The `dup2()` calls in the child don't count as "opening" the device, so the controlling terminal is never set.

The fix is to call `ioctl(slave_fd, TIOCSCTTY, 0)` in the child after `setsid()` and before closing the slave fd. This is exactly what Ghostty does — see `references/ghostty/src/pty.zig` lines 237-246:

```zig
if (setsid() < 0) return error.ProcessGroupFailed;

// Set controlling terminal
switch (posix.errno(c.ioctl(self.slave, TIOCSCTTY, @as(c_ulong, 0)))) {
    .SUCCESS => {},
    else => |err| {
        log.err("error setting controlling terminal errno={}", .{err});
        return error.SetControllingTerminalFailed;
    },
}
```

## Evidence

Verified in Cotty with extensive debugging:
- `ioctl(master_fd, TIOCSWINSZ, &ws)` returns 0 (success)
- `ioctl(master_fd, TIOCGWINSZ, &ws2)` reads back correct values
- But child processes (`top`, `claude`, `vim`) never redraw on resize
- No SIGWINCH is delivered because there is no controlling terminal for the session

## Required Fix

### 1. Add `ioctl_set_ctty(fd)` runtime function

Add a new runtime function that calls `ioctl(fd, TIOCSCTTY, 0)`.

**macOS constant:** `TIOCSCTTY = 0x20007461`
- Computed as: `_IOW('t', 97, int)` = `0x80000000 | ((4 & 0x1fff) << 16) | (0x74 << 8) | 0x61`

Wait, let me verify. Ghostty uses `536900705` which is `0x20007461`:
- `0x20007461` = `536900705` ✓

This is `_IO('t', 97)` on macOS = `(0x20000000 | (0x74 << 8) | 0x61)` = `0x20007461`.

**Implementation pattern:** Same as `ioctl_winsize` in `compiler/codegen/native/io_native.zig`, but simpler:
- Takes 1 parameter: `fd` (i64)
- Builds ioctl call: `ioctl(fd, TIOCSCTTY, 0)`
- Uses same ARM64 variadic padding pattern as `ioctl_winsize`
- Returns ioctl result (i64)

In `io_native.zig`, add near `generateIoctlWinsize`:

```zig
// ioctl_set_ctty(fd) → i64: ioctl(fd, TIOCSCTTY, 0)
// macOS: TIOCSCTTY = 0x20007461
fn generateIoctlSetCtty(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    // Same pattern as ioctl_winsize but with:
    // - Only 1 param (fd)
    // - TIOCSCTTY = 0x20007461
    // - Third arg to ioctl = 0 (not a pointer)
    // ioctl(fd, TIOCSCTTY, 0)
}
```

### 2. Register the function

In `io_native.zig`, in the function that builds the runtime function list, add:

```zig
try result.append(allocator, .{
    .name = "cot_ioctl_set_ctty",
    .compiled = try generateIoctlSetCtty(allocator, isa, ctrl_plane, func_index_map),
});
```

### 3. Add extern declaration in `stdlib/sys.cot`

```cot
extern fn ioctl_set_ctty(fd: i64) i64
```

### 4. Add the declaration to the compiler's known externs

In `compiler/driver.zig` (or wherever `ioctl_winsize` is registered as a known extern), add `ioctl_set_ctty` with the same pattern.

### 5. Use it in Cotty's `pty.cot`

After this fix, update `~/cot-land/cotty/src/pty.cot` line 63:

```cot
if (pid == 0) {
    // -- Child process --
    fd_close(master)
    setsid()
    ioctl_set_ctty(slave)   // ← ADD THIS LINE
    dup2(slave, 0)
    dup2(slave, 1)
    dup2(slave, 2)
    if (slave > 2) {
        fd_close(slave)
    }
    execve(path, argv, envp)
    exit(127)
}
```

## Files to Modify

| File | Change |
|------|--------|
| `compiler/codegen/native/io_native.zig` | Add `generateIoctlSetCtty` function, register it |
| `compiler/driver.zig` | Register `ioctl_set_ctty` as known extern (same as `ioctl_winsize`) |
| `stdlib/sys.cot` | Add `extern fn ioctl_set_ctty(fd: i64) i64` |

## Verification

After the compiler fix, update Cotty's `pty.cot` and test:

```bash
cd ~/cot-land/cotty
# Edit src/pty.cot to add ioctl_set_ctty(slave) after setsid()
cot build src/ffi.cot --lib -o libcotty.dylib
cp libcotty.dylib ~/cot-land/libcotty.dylib
cd macos && swift build
# Launch Cotty, run 'top', resize window — top should redraw at new size
```

## Priority

**Critical.** Without this fix, no TUI app can resize in Cotty. This blocks all terminal resize functionality including Claude Code, vim, top, htop, etc.
