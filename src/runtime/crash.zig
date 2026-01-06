//! Central crash handler for Cot runtime
//!
//! Provides signal-based crash handling with:
//! - Stack trace printing with symbol resolution
//! - VM crash context integration
//! - Hooks for modules (ISAM, etc.) to register cleanup callbacks

const std = @import("std");
const builtin = @import("builtin");

const is_posix = switch (builtin.os.tag) {
    .linux, .macos, .freebsd, .netbsd, .openbsd, .dragonfly => true,
    else => false,
};

/// Cleanup callback type - called before crash output
pub const CleanupCallback = *const fn () void;

/// Global cleanup callbacks (up to 8)
var cleanup_callbacks: [8]?CleanupCallback = [_]?CleanupCallback{null} ** 8;
var callback_count: usize = 0;

/// Whether crash handlers are installed
var handlers_installed: bool = false;

/// Register a cleanup callback to be called on crash
/// Returns true if registered, false if no slots available
pub fn registerCleanup(callback: CleanupCallback) bool {
    if (callback_count >= cleanup_callbacks.len) return false;
    cleanup_callbacks[callback_count] = callback;
    callback_count += 1;
    return true;
}

/// Install crash signal handlers (SEGV, ABRT, BUS, etc.)
/// Safe to call multiple times - only installs once
pub fn installHandlers() void {
    if (handlers_installed) return;
    if (!is_posix) return;

    const signals = [_]c_int{
        std.posix.SIG.SEGV,
        std.posix.SIG.ABRT,
        std.posix.SIG.BUS,
        std.posix.SIG.FPE,
        std.posix.SIG.ILL,
    };

    for (signals) |sig| {
        const action: std.posix.Sigaction = .{
            .handler = .{ .handler = crashSignalHandler },
            .mask = std.posix.sigemptyset(),
            .flags = std.posix.SA.RESETHAND, // One-shot, reset to default after handling
        };
        std.posix.sigaction(@intCast(sig), &action, null);
    }

    handlers_installed = true;
}

/// Print stack trace
fn printStackTrace() void {
    _ = std.posix.write(2, "\n\x1b[1;36mStack trace:\x1b[0m\n") catch return;

    // On macOS, Zig reads DWARF from .o files at RELATIVE paths like .zig-cache/o/.../
    // We need to change to the executable's build directory for those paths to resolve.
    // Get the executable's directory and change to it.
    if (comptime builtin.os.tag == .macos) {
        var exe_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.selfExePath(&exe_path_buf)) |exe_path| {
            if (std.fs.path.dirname(exe_path)) |exe_dir| {
                // Go up from zig-out/bin to the project root
                const parent1 = std.fs.path.dirname(exe_dir) orelse exe_dir;
                const project_root = std.fs.path.dirname(parent1) orelse parent1;
                std.posix.chdir(project_root) catch {};
            }
        } else |_| {}
    }

    // Access crash context to check for pre-captured stack
    const vm = @import("bytecode/vm.zig");
    const ctx = &vm.crash_context;

    // Use pre-captured addresses if available, otherwise capture now
    var fresh_addrs: [32]usize = undefined;
    const addresses: []const usize = if (ctx.stack_count > 0) blk: {
        break :blk ctx.stack_addrs[0..ctx.stack_count];
    } else blk: {
        var trace: std.builtin.StackTrace = .{
            .index = 0,
            .instruction_addresses = &fresh_addrs,
        };
        std.debug.captureStackTrace(@returnAddress(), &trace);
        break :blk fresh_addrs[0..trace.index];
    };

    // Get debug info for symbolization
    var debug_info = std.debug.getSelfDebugInfo() catch {
        _ = std.posix.write(2, "  (debug info unavailable)\n") catch {};
        return;
    };

    var buf: [512]u8 = undefined;
    for (addresses) |addr| {
        if (addr == 0) break;

        // First get the module for this address, then get symbol from module
        const module = debug_info.getModuleForAddress(addr) catch {
            const line = std.fmt.bufPrint(&buf, "  0x{x:0>12} (no module)\n", .{addr}) catch continue;
            _ = std.posix.write(2, line) catch {};
            continue;
        };

        const symbol = module.getSymbolAtAddress(std.heap.page_allocator, addr) catch {
            const line = std.fmt.bufPrint(&buf, "  0x{x:0>12} (no symbol)\n", .{addr}) catch continue;
            _ = std.posix.write(2, line) catch {};
            continue;
        };

        // Format: file:line function_name OR compile_unit function_name
        if (symbol.source_location) |loc| {
            const line = std.fmt.bufPrint(&buf, "  \x1b[2m{s}:{d}\x1b[0m {s}\n", .{
                loc.file_name,
                loc.line,
                symbol.name,
            }) catch continue;
            _ = std.posix.write(2, line) catch {};
        } else if (!std.mem.eql(u8, symbol.compile_unit_name, "???")) {
            // We have compile unit (source file) but no line number
            const line = std.fmt.bufPrint(&buf, "  \x1b[2m{s}\x1b[0m {s}\n", .{
                symbol.compile_unit_name,
                symbol.name,
            }) catch continue;
            _ = std.posix.write(2, line) catch {};
        } else if (!std.mem.eql(u8, symbol.name, "???")) {
            // Just function name
            const line = std.fmt.bufPrint(&buf, "  {s}\n", .{symbol.name}) catch continue;
            _ = std.posix.write(2, line) catch {};
        } else {
            // Raw address fallback
            const line = std.fmt.bufPrint(&buf, "  0x{x:0>12}\n", .{addr}) catch continue;
            _ = std.posix.write(2, line) catch {};
        }
    }
}

/// Central signal handler for all crash signals
fn crashSignalHandler(sig: c_int) callconv(.c) void {
    // Call registered cleanup callbacks
    for (cleanup_callbacks[0..callback_count]) |maybe_cb| {
        if (maybe_cb) |cb| {
            cb();
        }
    }

    // Access VM crash context for detailed error info
    const vm = @import("bytecode/vm.zig");
    const ctx = &vm.crash_context;

    // Print error message
    const msg = switch (sig) {
        std.posix.SIG.SEGV => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Segmentation fault in Cot program\n",
        std.posix.SIG.ABRT => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Abort signal in Cot program\n",
        std.posix.SIG.BUS => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Bus error in Cot program\n",
        std.posix.SIG.FPE => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Floating point exception in Cot program\n",
        std.posix.SIG.ILL => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Illegal instruction in Cot program\n",
        else => "\n\x1b[1;31m[RUNTIME ERROR]\x1b[0m Fatal signal in Cot program\n",
    };
    _ = std.posix.write(2, msg) catch {};

    var buf: [512]u8 = undefined;

    // Print native context if we were in native code
    if (ctx.native_context) |native_ctx| {
        const native_info = std.fmt.bufPrint(&buf, "  Native context: {s}\n", .{native_ctx}) catch "";
        _ = std.posix.write(2, native_info) catch {};

        if (ctx.native_file) |file| {
            const file_info = std.fmt.bufPrint(&buf, "  Location: {s}:{d}\n", .{ file, ctx.native_line }) catch "";
            _ = std.posix.write(2, file_info) catch {};
        }
    }

    // Print crash context if VM was active
    if (ctx.active) {
        if (ctx.last_opcode) |op| {
            const info = std.fmt.bufPrint(&buf, "  Last opcode: {s} at IP=0x{x:0>4}\n", .{ @tagName(op), ctx.ip }) catch "";
            _ = std.posix.write(2, info) catch {};
        }
        if (ctx.source_line > 0) {
            const line_info = std.fmt.bufPrint(&buf, "  Source line: {d}\n", .{ctx.source_line}) catch "";
            _ = std.posix.write(2, line_info) catch {};
        }
        if (ctx.routine_name) |routine| {
            const routine_info = std.fmt.bufPrint(&buf, "  Routine: {s}\n", .{routine}) catch "";
            _ = std.posix.write(2, routine_info) catch {};
        }
    } else if (ctx.native_context == null) {
        _ = std.posix.write(2, "  Crash occurred outside VM execution (during init or native code)\n") catch {};
    }

    // Fork a child process to print the stack trace
    // fork() is async-signal-safe, and the child can safely use Zig's debug facilities
    const pid = std.c.fork();
    if (pid == 0) {
        // Child process - can safely allocate and use debug functions
        printStackTrace();
        std.posix.exit(0);
    } else if (pid > 0) {
        // Parent - wait for child to finish printing
        _ = std.c.waitpid(pid, null, 0);
    }
    // If fork failed (pid < 0), just skip stack trace

    _ = std.posix.write(2, "\n") catch {};

    // Re-raise signal with default handler for proper crash behavior
    const default_action: std.posix.Sigaction = .{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(@intCast(sig), &default_action, null);
    _ = std.c.raise(sig);
}
