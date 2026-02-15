//! WASI Runtime for Cot (WebAssembly)
//!
//! Provides WASI-compatible I/O functions.
//! Reference: WASI preview1 (Go: syscall/fs_wasip1.go, Wasmtime: wasi-common)
//!
//! Two modes:
//!   - Native/freestanding: Wasm stub functions (return 0 or trap). ARM64/x64 overrides in driver.zig.
//!   - WASI target: Real WASI host imports + adapter shim functions that translate
//!     Cot's i64 calling convention to WASI's i32 iov-pointer convention.
//!
//! Reference for WASI ABI:
//!   wasmtime/crates/wasi-common/witx/preview1/wasi_snapshot_preview1.witx

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;
const WasmImport = wasm_link.WasmImport;
const Target = @import("../core/target.zig").Target;

const wasm_op = @import("wasm_opcodes.zig");

// =============================================================================
// Function Names
// =============================================================================

pub const FD_WRITE_NAME = "wasi_fd_write";
pub const FD_WRITE_SIMPLE_NAME = "cot_fd_write_simple";
pub const FD_READ_SIMPLE_NAME = "cot_fd_read_simple";
pub const FD_CLOSE_NAME = "cot_fd_close";
pub const FD_SEEK_NAME = "cot_fd_seek";
pub const FD_OPEN_NAME = "cot_fd_open";
pub const TIME_NAME = "cot_time";
pub const RANDOM_NAME = "cot_random";
pub const EXIT_NAME = "cot_exit";
pub const ARGS_COUNT_NAME = "cot_args_count";
pub const ARG_LEN_NAME = "cot_arg_len";
pub const ARG_PTR_NAME = "cot_arg_ptr";
pub const ENVIRON_COUNT_NAME = "cot_environ_count";
pub const ENVIRON_LEN_NAME = "cot_environ_len";
pub const ENVIRON_PTR_NAME = "cot_environ_ptr";
// Networking
pub const NET_SOCKET_NAME = "cot_net_socket";
pub const NET_BIND_NAME = "cot_net_bind";
pub const NET_LISTEN_NAME = "cot_net_listen";
pub const NET_ACCEPT_NAME = "cot_net_accept";
pub const NET_CONNECT_NAME = "cot_net_connect";
pub const NET_SET_REUSE_ADDR_NAME = "cot_net_set_reuse_addr";
// Event loop (kqueue/epoll)
pub const KQUEUE_CREATE_NAME = "cot_kqueue_create";
pub const KEVENT_ADD_NAME = "cot_kevent_add";
pub const KEVENT_DEL_NAME = "cot_kevent_del";
pub const KEVENT_WAIT_NAME = "cot_kevent_wait";
pub const EPOLL_CREATE_NAME = "cot_epoll_create";
pub const EPOLL_ADD_NAME = "cot_epoll_add";
pub const EPOLL_DEL_NAME = "cot_epoll_del";
pub const EPOLL_WAIT_NAME = "cot_epoll_wait";
pub const SET_NONBLOCK_NAME = "cot_set_nonblocking";
// Process spawning
pub const FORK_NAME = "cot_fork";
pub const EXECVE_NAME = "cot_execve";
pub const WAITPID_NAME = "cot_waitpid";
pub const PIPE_NAME = "cot_pipe";
pub const DUP2_NAME = "cot_dup2";

// WASI scratch memory addresses in linear memory
// Used by adapter shims to build iov structs and read WASI output params
const SCRATCH_BASE: i32 = 0xE0000; // iov struct: 8 bytes (ptr + len)
const SCRATCH_NWRITTEN: i32 = 0xE0008; // nwritten/nread output: 4 bytes
const SCRATCH_RESULT: i32 = 0xE000C; // generic i32/i64 result output: 8 bytes
const SCRATCH_ARGV: i32 = 0xE1000; // argv pointer array (1024 entries * 4 bytes)
const SCRATCH_ARGV_BUF: i32 = 0xE2000; // argv string buffer (8KB)
const SCRATCH_ENVIRON: i32 = 0xE4000; // environ pointer array
const SCRATCH_ENVIRON_BUF: i32 = 0xE5000; // environ string buffer (8KB)

// =============================================================================
// Return Type
// =============================================================================

pub const WasiFunctions = struct {
    fd_write_idx: u32,
    fd_write_simple_idx: u32,
    fd_read_simple_idx: u32,
    fd_close_idx: u32,
    fd_seek_idx: u32,
    fd_open_idx: u32,
    time_idx: u32,
    random_idx: u32,
    exit_idx: u32,
    args_count_idx: u32,
    arg_len_idx: u32,
    arg_ptr_idx: u32,
    environ_count_idx: u32,
    environ_len_idx: u32,
    environ_ptr_idx: u32,
    // Networking
    net_socket_idx: u32,
    net_bind_idx: u32,
    net_listen_idx: u32,
    net_accept_idx: u32,
    net_connect_idx: u32,
    net_set_reuse_addr_idx: u32,
    // Event loop
    kqueue_create_idx: u32,
    kevent_add_idx: u32,
    kevent_del_idx: u32,
    kevent_wait_idx: u32,
    epoll_create_idx: u32,
    epoll_add_idx: u32,
    epoll_del_idx: u32,
    epoll_wait_idx: u32,
    set_nonblocking_idx: u32,
    // Process spawning
    fork_idx: u32,
    execve_idx: u32,
    waitpid_idx: u32,
    pipe_idx: u32,
    dup2_idx: u32,
};

// =============================================================================
// addToLinker — register all WASI runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker, target: Target) !WasiFunctions {
    if (target.isWasi()) {
        return addWasiImports(allocator, linker);
    } else {
        return addNativeStubs(allocator, linker);
    }
}

// =============================================================================
// WASI target: import real WASI functions + create adapter shims
// Reference: WASI snapshot_preview1 ABI
// =============================================================================

fn addWasiImports(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker) !WasiFunctions {
    const WASI_MODULE = "wasi_snapshot_preview1";

    // --- Import real WASI host functions ---

    // fd_write(fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> i32
    const wasi_fd_write_type = try linker.addType(
        &[_]ValType{ .i32, .i32, .i32, .i32 },
        &[_]ValType{.i32},
    );
    const wasi_fd_write_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "fd_write",
        .type_idx = wasi_fd_write_type,
    });

    // fd_read(fd: i32, iovs: i32, iovs_len: i32, nread: i32) -> i32
    const wasi_fd_read_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "fd_read",
        .type_idx = wasi_fd_write_type, // Same signature
    });

    // fd_close(fd: i32) -> i32
    const wasi_fd_close_type = try linker.addType(
        &[_]ValType{.i32},
        &[_]ValType{.i32},
    );
    const wasi_fd_close_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "fd_close",
        .type_idx = wasi_fd_close_type,
    });

    // fd_seek(fd: i32, offset: i64, whence: i32, newoffset_ptr: i32) -> i32
    const wasi_fd_seek_type = try linker.addType(
        &[_]ValType{ .i32, .i64, .i32, .i32 },
        &[_]ValType{.i32},
    );
    const wasi_fd_seek_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "fd_seek",
        .type_idx = wasi_fd_seek_type,
    });

    // path_open(dirfd:i32, dirflags:i32, path:i32, path_len:i32, oflags:i32,
    //           fs_rights_base:i64, fs_rights_inheriting:i64, fdflags:i32, opened_fd:i32) -> i32
    const wasi_path_open_type = try linker.addType(
        &[_]ValType{ .i32, .i32, .i32, .i32, .i32, .i64, .i64, .i32, .i32 },
        &[_]ValType{.i32},
    );
    const wasi_path_open_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "path_open",
        .type_idx = wasi_path_open_type,
    });

    // proc_exit(code: i32) -> noreturn
    const wasi_proc_exit_type = try linker.addType(
        &[_]ValType{.i32},
        &[_]ValType{},
    );
    const wasi_proc_exit_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "proc_exit",
        .type_idx = wasi_proc_exit_type,
    });

    // args_sizes_get(argc_ptr: i32, argv_buf_size_ptr: i32) -> i32
    const wasi_args_sizes_get_type = try linker.addType(
        &[_]ValType{ .i32, .i32 },
        &[_]ValType{.i32},
    );
    const wasi_args_sizes_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "args_sizes_get",
        .type_idx = wasi_args_sizes_get_type,
    });

    // args_get(argv: i32, argv_buf: i32) -> i32
    const wasi_args_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "args_get",
        .type_idx = wasi_args_sizes_get_type, // Same type: (i32, i32) -> i32
    });

    // clock_time_get(clock_id: i32, precision: i64, time_ptr: i32) -> i32
    const wasi_clock_time_get_type = try linker.addType(
        &[_]ValType{ .i32, .i64, .i32 },
        &[_]ValType{.i32},
    );
    const wasi_clock_time_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "clock_time_get",
        .type_idx = wasi_clock_time_get_type,
    });

    // random_get(buf: i32, buf_len: i32) -> i32
    const wasi_random_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "random_get",
        .type_idx = wasi_args_sizes_get_type, // Same type: (i32, i32) -> i32
    });

    // environ_sizes_get(count_ptr: i32, buf_size_ptr: i32) -> i32
    const wasi_environ_sizes_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "environ_sizes_get",
        .type_idx = wasi_args_sizes_get_type, // Same type: (i32, i32) -> i32
    });

    // environ_get(environ: i32, environ_buf: i32) -> i32
    const wasi_environ_get_idx = try linker.addImport(.{
        .module = WASI_MODULE,
        .name = "environ_get",
        .type_idx = wasi_args_sizes_get_type, // Same type: (i32, i32) -> i32
    });

    // --- Import count for function index offset ---
    // In Wasm binary: imports get indices 0..N-1, module functions get N, N+1, ...
    // addImport returns the import's LOCAL index (0-based within imports).
    // The actual Wasm function index = import local index (they're 0-based in order).
    // Module functions returned by addFunc are 0-based within funcs.
    // Their actual Wasm function index = import_count + func_local_index.
    const import_count = linker.numImports();

    // --- Create adapter shim functions ---
    // These have Cot's i64 signatures and call the imported WASI functions.
    // Import function indices are 0..import_count-1 (directly usable in call).

    // wasi_fd_write shim: (fd:i64, iovs:i64, iovs_len:i64, nwritten:i64) -> i64
    const fd_write_cot_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_body = try generateWasiFdWriteShim(allocator, wasi_fd_write_idx);
    const fd_write_idx = try linker.addFunc(.{
        .name = FD_WRITE_NAME,
        .type_idx = fd_write_cot_type,
        .code = fd_write_body,
        .exported = false,
    });

    // cot_fd_write_simple shim: (fd:i64, ptr:i64, len:i64) -> i64
    const fd_write_simple_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_simple_body = try generateWasiWriteSimpleShim(allocator, wasi_fd_write_idx);
    const fd_write_simple_idx = try linker.addFunc(.{
        .name = FD_WRITE_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_write_simple_body,
        .exported = false,
    });

    // cot_fd_read_simple shim: (fd:i64, buf:i64, len:i64) -> i64
    const fd_read_simple_body = try generateWasiReadSimpleShim(allocator, wasi_fd_read_idx);
    const fd_read_simple_idx = try linker.addFunc(.{
        .name = FD_READ_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_read_simple_body,
        .exported = false,
    });

    // cot_fd_close shim: (fd:i64) -> i64
    const fd_close_cot_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{.i64},
    );
    const fd_close_body = try generateWasiFdCloseShim(allocator, wasi_fd_close_idx);
    const fd_close_idx = try linker.addFunc(.{
        .name = FD_CLOSE_NAME,
        .type_idx = fd_close_cot_type,
        .code = fd_close_body,
        .exported = false,
    });

    // cot_fd_seek shim: (fd:i64, offset:i64, whence:i64) -> i64
    const fd_seek_body = try generateWasiFdSeekShim(allocator, wasi_fd_seek_idx);
    const fd_seek_idx = try linker.addFunc(.{
        .name = FD_SEEK_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_seek_body,
        .exported = false,
    });

    // cot_fd_open shim: (path_ptr:i64, path_len:i64, flags:i64) -> i64
    const fd_open_body = try generateWasiPathOpenShim(allocator, wasi_path_open_idx);
    const fd_open_idx = try linker.addFunc(.{
        .name = FD_OPEN_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_open_body,
        .exported = false,
    });

    // cot_time shim: () -> i64
    const time_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{.i64},
    );
    const time_body = try generateWasiTimeShim(allocator, wasi_clock_time_get_idx);
    const time_idx = try linker.addFunc(.{
        .name = TIME_NAME,
        .type_idx = time_type,
        .code = time_body,
        .exported = false,
    });

    // cot_random shim: (buf:i64, len:i64) -> i64
    const random_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const random_body = try generateWasiRandomShim(allocator, wasi_random_get_idx);
    const random_idx = try linker.addFunc(.{
        .name = RANDOM_NAME,
        .type_idx = random_type,
        .code = random_body,
        .exported = false,
    });

    // cot_exit shim: (code:i64) -> void
    const exit_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const exit_body = try generateWasiExitShim(allocator, wasi_proc_exit_idx);
    const exit_idx = try linker.addFunc(.{
        .name = EXIT_NAME,
        .type_idx = exit_type,
        .code = exit_body,
        .exported = false,
    });

    // cot_args_count shim: () -> i64
    const args_count_body = try generateWasiArgsCountShim(allocator, wasi_args_sizes_get_idx);
    const args_count_idx = try linker.addFunc(.{
        .name = ARGS_COUNT_NAME,
        .type_idx = time_type,
        .code = args_count_body,
        .exported = false,
    });

    // cot_arg_len shim: (n:i64) -> i64
    const arg_one_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{.i64},
    );
    const arg_len_body = try generateWasiArgLenShim(allocator, wasi_args_sizes_get_idx, wasi_args_get_idx);
    const arg_len_idx = try linker.addFunc(.{
        .name = ARG_LEN_NAME,
        .type_idx = arg_one_type,
        .code = arg_len_body,
        .exported = false,
    });

    // cot_arg_ptr shim: (n:i64) -> i64
    const arg_ptr_body = try generateWasiArgPtrShim(allocator, wasi_args_sizes_get_idx, wasi_args_get_idx);
    const arg_ptr_idx = try linker.addFunc(.{
        .name = ARG_PTR_NAME,
        .type_idx = arg_one_type,
        .code = arg_ptr_body,
        .exported = false,
    });

    // cot_environ_count shim: () -> i64
    const environ_count_body = try generateWasiEnvironCountShim(allocator, wasi_environ_sizes_get_idx);
    const environ_count_idx = try linker.addFunc(.{
        .name = ENVIRON_COUNT_NAME,
        .type_idx = time_type,
        .code = environ_count_body,
        .exported = false,
    });

    // cot_environ_len shim: (n:i64) -> i64
    const environ_len_body = try generateWasiEnvironLenShim(allocator, wasi_environ_sizes_get_idx, wasi_environ_get_idx);
    const environ_len_idx = try linker.addFunc(.{
        .name = ENVIRON_LEN_NAME,
        .type_idx = arg_one_type,
        .code = environ_len_body,
        .exported = false,
    });

    // cot_environ_ptr shim: (n:i64) -> i64
    const environ_ptr_body = try generateWasiEnvironPtrShim(allocator, wasi_environ_sizes_get_idx, wasi_environ_get_idx);
    const environ_ptr_idx = try linker.addFunc(.{
        .name = ENVIRON_PTR_NAME,
        .type_idx = arg_one_type,
        .code = environ_ptr_body,
        .exported = false,
    });

    // Network stubs for WASI (no WASI socket support in preview1)
    const net_3arg_type = try linker.addType(&[_]ValType{ .i64, .i64, .i64 }, &[_]ValType{.i64});
    const net_2arg_type = try linker.addType(&[_]ValType{ .i64, .i64 }, &[_]ValType{.i64});

    const net_socket_body = try generateStubReturnsNegOne(allocator);
    const net_socket_idx = try linker.addFunc(.{ .name = NET_SOCKET_NAME, .type_idx = net_3arg_type, .code = net_socket_body, .exported = false });
    const net_bind_body = try generateStubReturnsNegOne(allocator);
    const net_bind_idx = try linker.addFunc(.{ .name = NET_BIND_NAME, .type_idx = net_3arg_type, .code = net_bind_body, .exported = false });
    const net_listen_body = try generateStubReturnsNegOne(allocator);
    const net_listen_idx = try linker.addFunc(.{ .name = NET_LISTEN_NAME, .type_idx = net_2arg_type, .code = net_listen_body, .exported = false });
    const net_accept_body = try generateStubReturnsNegOne(allocator);
    const net_accept_idx = try linker.addFunc(.{ .name = NET_ACCEPT_NAME, .type_idx = arg_one_type, .code = net_accept_body, .exported = false });
    const net_connect_body = try generateStubReturnsNegOne(allocator);
    const net_connect_idx = try linker.addFunc(.{ .name = NET_CONNECT_NAME, .type_idx = net_3arg_type, .code = net_connect_body, .exported = false });
    const net_reuse_body = try generateStubReturnsZero(allocator);
    const net_reuse_idx = try linker.addFunc(.{ .name = NET_SET_REUSE_ADDR_NAME, .type_idx = arg_one_type, .code = net_reuse_body, .exported = false });

    // Event loop stubs for WASI (no kqueue/epoll in WASI preview1)
    const kqueue_create_body = try generateStubReturnsNegOne(allocator);
    const kqueue_create_idx = try linker.addFunc(.{ .name = KQUEUE_CREATE_NAME, .type_idx = time_type, .code = kqueue_create_body, .exported = false });
    const kevent_add_body = try generateStubReturnsNegOne(allocator);
    const kevent_add_idx = try linker.addFunc(.{ .name = KEVENT_ADD_NAME, .type_idx = net_3arg_type, .code = kevent_add_body, .exported = false });
    const kevent_del_body = try generateStubReturnsNegOne(allocator);
    const kevent_del_idx = try linker.addFunc(.{ .name = KEVENT_DEL_NAME, .type_idx = net_3arg_type, .code = kevent_del_body, .exported = false });
    const kevent_wait_body = try generateStubReturnsNegOne(allocator);
    const kevent_wait_idx = try linker.addFunc(.{ .name = KEVENT_WAIT_NAME, .type_idx = net_3arg_type, .code = kevent_wait_body, .exported = false });
    const epoll_create_body = try generateStubReturnsNegOne(allocator);
    const epoll_create_idx = try linker.addFunc(.{ .name = EPOLL_CREATE_NAME, .type_idx = time_type, .code = epoll_create_body, .exported = false });
    const epoll_add_body = try generateStubReturnsNegOne(allocator);
    const epoll_add_idx = try linker.addFunc(.{ .name = EPOLL_ADD_NAME, .type_idx = net_3arg_type, .code = epoll_add_body, .exported = false });
    const epoll_del_body = try generateStubReturnsNegOne(allocator);
    const epoll_del_idx = try linker.addFunc(.{ .name = EPOLL_DEL_NAME, .type_idx = net_2arg_type, .code = epoll_del_body, .exported = false });
    const epoll_wait_body = try generateStubReturnsNegOne(allocator);
    const epoll_wait_idx = try linker.addFunc(.{ .name = EPOLL_WAIT_NAME, .type_idx = net_3arg_type, .code = epoll_wait_body, .exported = false });
    const set_nonblock_body = try generateStubReturnsNegOne(allocator);
    const set_nonblock_idx = try linker.addFunc(.{ .name = SET_NONBLOCK_NAME, .type_idx = arg_one_type, .code = set_nonblock_body, .exported = false });

    // Process spawning stubs (WASI has no process spawning — return -1)
    const fork_body = try generateStubReturnsNegOne(allocator);
    const fork_idx = try linker.addFunc(.{ .name = FORK_NAME, .type_idx = time_type, .code = fork_body, .exported = false });
    const execve_body = try generateStubReturnsNegOne(allocator);
    const execve_idx = try linker.addFunc(.{ .name = EXECVE_NAME, .type_idx = net_3arg_type, .code = execve_body, .exported = false });
    const waitpid_body = try generateStubReturnsNegOne(allocator);
    const waitpid_idx = try linker.addFunc(.{ .name = WAITPID_NAME, .type_idx = arg_one_type, .code = waitpid_body, .exported = false });
    const pipe_body = try generateStubReturnsNegOne(allocator);
    const pipe_idx = try linker.addFunc(.{ .name = PIPE_NAME, .type_idx = time_type, .code = pipe_body, .exported = false });
    const dup2_body = try generateStubReturnsNegOne(allocator);
    const dup2_idx = try linker.addFunc(.{ .name = DUP2_NAME, .type_idx = net_2arg_type, .code = dup2_body, .exported = false });

    // Return indices: addFunc returns 0-based func indices.
    // Actual Wasm function index = import_count + func_local_index.
    return WasiFunctions{
        .fd_write_idx = fd_write_idx + import_count,
        .fd_write_simple_idx = fd_write_simple_idx + import_count,
        .fd_read_simple_idx = fd_read_simple_idx + import_count,
        .fd_close_idx = fd_close_idx + import_count,
        .fd_seek_idx = fd_seek_idx + import_count,
        .fd_open_idx = fd_open_idx + import_count,
        .time_idx = time_idx + import_count,
        .random_idx = random_idx + import_count,
        .exit_idx = exit_idx + import_count,
        .args_count_idx = args_count_idx + import_count,
        .arg_len_idx = arg_len_idx + import_count,
        .arg_ptr_idx = arg_ptr_idx + import_count,
        .environ_count_idx = environ_count_idx + import_count,
        .environ_len_idx = environ_len_idx + import_count,
        .environ_ptr_idx = environ_ptr_idx + import_count,
        .net_socket_idx = net_socket_idx + import_count,
        .net_bind_idx = net_bind_idx + import_count,
        .net_listen_idx = net_listen_idx + import_count,
        .net_accept_idx = net_accept_idx + import_count,
        .net_connect_idx = net_connect_idx + import_count,
        .net_set_reuse_addr_idx = net_reuse_idx + import_count,
        .kqueue_create_idx = kqueue_create_idx + import_count,
        .kevent_add_idx = kevent_add_idx + import_count,
        .kevent_del_idx = kevent_del_idx + import_count,
        .kevent_wait_idx = kevent_wait_idx + import_count,
        .epoll_create_idx = epoll_create_idx + import_count,
        .epoll_add_idx = epoll_add_idx + import_count,
        .epoll_del_idx = epoll_del_idx + import_count,
        .epoll_wait_idx = epoll_wait_idx + import_count,
        .set_nonblocking_idx = set_nonblock_idx + import_count,
        .fork_idx = fork_idx + import_count,
        .execve_idx = execve_idx + import_count,
        .waitpid_idx = waitpid_idx + import_count,
        .pipe_idx = pipe_idx + import_count,
        .dup2_idx = dup2_idx + import_count,
    };
}

// =============================================================================
// Native/freestanding: stub functions (existing behavior)
// =============================================================================

fn addNativeStubs(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker) !WasiFunctions {
    // wasi_fd_write: (fd: i64, iovs: i64, iovs_len: i64, nwritten: i64) -> i64
    const fd_write_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_body = try generateFdWriteStubBody(allocator);
    const fd_write_idx = try linker.addFunc(.{
        .name = FD_WRITE_NAME,
        .type_idx = fd_write_type,
        .code = fd_write_body,
        .exported = true,
    });

    const fd_write_simple_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_simple_body = try generateStubReturnsZero(allocator);
    const fd_write_simple_idx = try linker.addFunc(.{
        .name = FD_WRITE_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_write_simple_body,
        .exported = true,
    });

    const fd_read_simple_body = try generateStubReturnsZero(allocator);
    const fd_read_simple_idx = try linker.addFunc(.{
        .name = FD_READ_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_read_simple_body,
        .exported = true,
    });

    const fd_close_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{.i64},
    );
    const fd_close_body = try generateStubReturnsZero(allocator);
    const fd_close_idx = try linker.addFunc(.{
        .name = FD_CLOSE_NAME,
        .type_idx = fd_close_type,
        .code = fd_close_body,
        .exported = true,
    });

    const fd_seek_body = try generateStubReturnsZero(allocator);
    const fd_seek_idx = try linker.addFunc(.{
        .name = FD_SEEK_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_seek_body,
        .exported = true,
    });

    const fd_open_body = try generateStubReturnsZero(allocator);
    const fd_open_idx = try linker.addFunc(.{
        .name = FD_OPEN_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_open_body,
        .exported = true,
    });

    const time_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{.i64},
    );
    const time_body = try generateStubReturnsZero(allocator);
    const time_idx = try linker.addFunc(.{
        .name = TIME_NAME,
        .type_idx = time_type,
        .code = time_body,
        .exported = true,
    });

    const random_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const random_body = try generateStubReturnsZero(allocator);
    const random_idx = try linker.addFunc(.{
        .name = RANDOM_NAME,
        .type_idx = random_type,
        .code = random_body,
        .exported = true,
    });

    const exit_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const exit_body = try generateStubTrap(allocator);
    const exit_idx = try linker.addFunc(.{
        .name = EXIT_NAME,
        .type_idx = exit_type,
        .code = exit_body,
        .exported = true,
    });

    const args_count_body = try generateStubReturnsZero(allocator);
    const args_count_idx = try linker.addFunc(.{
        .name = ARGS_COUNT_NAME,
        .type_idx = time_type,
        .code = args_count_body,
        .exported = true,
    });

    const arg_len_body = try generateStubReturnsZero(allocator);
    const arg_len_idx = try linker.addFunc(.{
        .name = ARG_LEN_NAME,
        .type_idx = fd_close_type,
        .code = arg_len_body,
        .exported = true,
    });

    const arg_ptr_body = try generateStubReturnsZero(allocator);
    const arg_ptr_idx = try linker.addFunc(.{
        .name = ARG_PTR_NAME,
        .type_idx = fd_close_type,
        .code = arg_ptr_body,
        .exported = true,
    });

    const environ_count_body = try generateStubReturnsZero(allocator);
    const environ_count_idx = try linker.addFunc(.{
        .name = ENVIRON_COUNT_NAME,
        .type_idx = time_type,
        .code = environ_count_body,
        .exported = true,
    });

    const environ_len_body = try generateStubReturnsZero(allocator);
    const environ_len_idx = try linker.addFunc(.{
        .name = ENVIRON_LEN_NAME,
        .type_idx = fd_close_type,
        .code = environ_len_body,
        .exported = true,
    });

    const environ_ptr_body = try generateStubReturnsZero(allocator);
    const environ_ptr_idx = try linker.addFunc(.{
        .name = ENVIRON_PTR_NAME,
        .type_idx = fd_close_type,
        .code = environ_ptr_body,
        .exported = true,
    });

    // Networking stubs (native overrides in driver.zig provide real syscalls)
    const net_3arg_type = try linker.addType(&[_]ValType{ .i64, .i64, .i64 }, &[_]ValType{.i64});
    const net_2arg_type = try linker.addType(&[_]ValType{ .i64, .i64 }, &[_]ValType{.i64});

    const net_socket_body = try generateStubReturnsNegOne(allocator);
    const net_socket_idx = try linker.addFunc(.{ .name = NET_SOCKET_NAME, .type_idx = net_3arg_type, .code = net_socket_body, .exported = true });
    const net_bind_body = try generateStubReturnsNegOne(allocator);
    const net_bind_idx = try linker.addFunc(.{ .name = NET_BIND_NAME, .type_idx = net_3arg_type, .code = net_bind_body, .exported = true });
    const net_listen_body = try generateStubReturnsNegOne(allocator);
    const net_listen_idx = try linker.addFunc(.{ .name = NET_LISTEN_NAME, .type_idx = net_2arg_type, .code = net_listen_body, .exported = true });
    const net_accept_body = try generateStubReturnsNegOne(allocator);
    const net_accept_idx = try linker.addFunc(.{ .name = NET_ACCEPT_NAME, .type_idx = fd_close_type, .code = net_accept_body, .exported = true });
    const net_connect_body = try generateStubReturnsNegOne(allocator);
    const net_connect_idx = try linker.addFunc(.{ .name = NET_CONNECT_NAME, .type_idx = net_3arg_type, .code = net_connect_body, .exported = true });
    const net_reuse_body = try generateStubReturnsZero(allocator);
    const net_reuse_idx = try linker.addFunc(.{ .name = NET_SET_REUSE_ADDR_NAME, .type_idx = fd_close_type, .code = net_reuse_body, .exported = true });

    // Event loop stubs (native overrides in driver.zig provide real syscalls)
    // kqueue (macOS): 0-arg create, 3-arg add/del/wait
    const kqueue_create_body = try generateStubReturnsNegOne(allocator);
    const kqueue_create_idx = try linker.addFunc(.{ .name = KQUEUE_CREATE_NAME, .type_idx = time_type, .code = kqueue_create_body, .exported = true });
    const kevent_add_body = try generateStubReturnsNegOne(allocator);
    const kevent_add_idx = try linker.addFunc(.{ .name = KEVENT_ADD_NAME, .type_idx = net_3arg_type, .code = kevent_add_body, .exported = true });
    const kevent_del_body = try generateStubReturnsNegOne(allocator);
    const kevent_del_idx = try linker.addFunc(.{ .name = KEVENT_DEL_NAME, .type_idx = net_3arg_type, .code = kevent_del_body, .exported = true });
    const kevent_wait_body = try generateStubReturnsNegOne(allocator);
    const kevent_wait_idx = try linker.addFunc(.{ .name = KEVENT_WAIT_NAME, .type_idx = net_3arg_type, .code = kevent_wait_body, .exported = true });
    // epoll (Linux): 0-arg create, 3-arg add, 2-arg del, 3-arg wait
    const epoll_create_body = try generateStubReturnsNegOne(allocator);
    const epoll_create_idx = try linker.addFunc(.{ .name = EPOLL_CREATE_NAME, .type_idx = time_type, .code = epoll_create_body, .exported = true });
    const epoll_add_body = try generateStubReturnsNegOne(allocator);
    const epoll_add_idx = try linker.addFunc(.{ .name = EPOLL_ADD_NAME, .type_idx = net_3arg_type, .code = epoll_add_body, .exported = true });
    const epoll_del_body = try generateStubReturnsNegOne(allocator);
    const epoll_del_idx = try linker.addFunc(.{ .name = EPOLL_DEL_NAME, .type_idx = net_2arg_type, .code = epoll_del_body, .exported = true });
    const epoll_wait_body = try generateStubReturnsNegOne(allocator);
    const epoll_wait_idx = try linker.addFunc(.{ .name = EPOLL_WAIT_NAME, .type_idx = net_3arg_type, .code = epoll_wait_body, .exported = true });
    // set_nonblocking: 1-arg
    const set_nonblock_body = try generateStubReturnsNegOne(allocator);
    const set_nonblock_idx = try linker.addFunc(.{ .name = SET_NONBLOCK_NAME, .type_idx = fd_close_type, .code = set_nonblock_body, .exported = true });

    // Process spawning stubs (native overrides in driver.zig provide real syscalls)
    const fork_body = try generateStubReturnsNegOne(allocator);
    const fork_idx = try linker.addFunc(.{ .name = FORK_NAME, .type_idx = time_type, .code = fork_body, .exported = true });
    const execve_body = try generateStubReturnsNegOne(allocator);
    const execve_idx = try linker.addFunc(.{ .name = EXECVE_NAME, .type_idx = net_3arg_type, .code = execve_body, .exported = true });
    const waitpid_body = try generateStubReturnsNegOne(allocator);
    const waitpid_idx = try linker.addFunc(.{ .name = WAITPID_NAME, .type_idx = fd_close_type, .code = waitpid_body, .exported = true });
    const pipe_body = try generateStubReturnsNegOne(allocator);
    const pipe_idx = try linker.addFunc(.{ .name = PIPE_NAME, .type_idx = time_type, .code = pipe_body, .exported = true });
    const dup2_body = try generateStubReturnsNegOne(allocator);
    const dup2_idx = try linker.addFunc(.{ .name = DUP2_NAME, .type_idx = net_2arg_type, .code = dup2_body, .exported = true });

    return WasiFunctions{
        .fd_write_idx = fd_write_idx,
        .fd_write_simple_idx = fd_write_simple_idx,
        .fd_read_simple_idx = fd_read_simple_idx,
        .fd_close_idx = fd_close_idx,
        .fd_seek_idx = fd_seek_idx,
        .fd_open_idx = fd_open_idx,
        .time_idx = time_idx,
        .random_idx = random_idx,
        .exit_idx = exit_idx,
        .args_count_idx = args_count_idx,
        .arg_len_idx = arg_len_idx,
        .arg_ptr_idx = arg_ptr_idx,
        .environ_count_idx = environ_count_idx,
        .environ_len_idx = environ_len_idx,
        .environ_ptr_idx = environ_ptr_idx,
        .net_socket_idx = net_socket_idx,
        .net_bind_idx = net_bind_idx,
        .net_listen_idx = net_listen_idx,
        .net_accept_idx = net_accept_idx,
        .net_connect_idx = net_connect_idx,
        .net_set_reuse_addr_idx = net_reuse_idx,
        .kqueue_create_idx = kqueue_create_idx,
        .kevent_add_idx = kevent_add_idx,
        .kevent_del_idx = kevent_del_idx,
        .kevent_wait_idx = kevent_wait_idx,
        .epoll_create_idx = epoll_create_idx,
        .epoll_add_idx = epoll_add_idx,
        .epoll_del_idx = epoll_del_idx,
        .epoll_wait_idx = epoll_wait_idx,
        .set_nonblocking_idx = set_nonblock_idx,
        .fork_idx = fork_idx,
        .execve_idx = execve_idx,
        .waitpid_idx = waitpid_idx,
        .pipe_idx = pipe_idx,
        .dup2_idx = dup2_idx,
    };
}

// =============================================================================
// WASI adapter shim generators
// Each takes Cot's i64 params, wraps to i32, calls WASI import, returns i64
// =============================================================================

/// wasi_fd_write shim: wraps legacy 4-arg Cot signature to WASI fd_write
fn generateWasiFdWriteShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // params: fd(0), iovs(1), iovs_len(2), nwritten(3) — all i64
    // Call WASI fd_write(i32.wrap(fd), i32.wrap(iovs), i32.wrap(iovs_len), i32.wrap(nwritten))
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalGet(3);
    try code.emitI32WrapI64();
    try code.emitCall(wasi_func_idx);
    try code.emitI64ExtendI32U(); // result: i32 -> i64
    return try code.finish();
}

/// cot_fd_write_simple shim: (fd:i64, ptr:i64, len:i64) -> i64
/// Builds a 1-element iov at SCRATCH_BASE, calls WASI fd_write, returns bytes written
fn generateWasiWriteSimpleShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // params: fd(0), ptr(1), len(2)

    // Build iov at SCRATCH_BASE: { .buf = i32(ptr), .buf_len = i32(len) }
    try code.emitI32Const(SCRATCH_BASE); // addr for iov[0].buf
    try code.emitLocalGet(1); // ptr
    try code.emitI32WrapI64();
    try code.emitI32Store(2, 0); // store buf ptr at SCRATCH_BASE+0

    try code.emitI32Const(SCRATCH_BASE); // addr for iov[0].buf_len
    try code.emitLocalGet(2); // len
    try code.emitI32WrapI64();
    try code.emitI32Store(2, 4); // store buf len at SCRATCH_BASE+4

    // Call WASI fd_write(i32(fd), SCRATCH_BASE, 1, SCRATCH_NWRITTEN)
    try code.emitLocalGet(0); // fd
    try code.emitI32WrapI64();
    try code.emitI32Const(SCRATCH_BASE); // iovs ptr
    try code.emitI32Const(1); // iovs_len = 1
    try code.emitI32Const(SCRATCH_NWRITTEN); // nwritten output ptr
    try code.emitCall(wasi_func_idx);
    try code.emitDrop(); // drop errno

    // Load nwritten from SCRATCH_NWRITTEN, extend to i64
    try code.emitI32Const(SCRATCH_NWRITTEN);
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_fd_read_simple shim: (fd:i64, buf:i64, len:i64) -> i64
fn generateWasiReadSimpleShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // Same pattern as write: build iov, call fd_read, return nread

    try code.emitI32Const(SCRATCH_BASE);
    try code.emitLocalGet(1); // buf
    try code.emitI32WrapI64();
    try code.emitI32Store(2, 0);

    try code.emitI32Const(SCRATCH_BASE);
    try code.emitLocalGet(2); // len
    try code.emitI32WrapI64();
    try code.emitI32Store(2, 4);

    try code.emitLocalGet(0); // fd
    try code.emitI32WrapI64();
    try code.emitI32Const(SCRATCH_BASE);
    try code.emitI32Const(1);
    try code.emitI32Const(SCRATCH_NWRITTEN); // nread output
    try code.emitCall(wasi_func_idx);
    try code.emitDrop();

    try code.emitI32Const(SCRATCH_NWRITTEN);
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_fd_close shim: (fd:i64) -> i64
fn generateWasiFdCloseShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitCall(wasi_func_idx);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_fd_seek shim: (fd:i64, offset:i64, whence:i64) -> i64
fn generateWasiFdSeekShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI fd_seek(fd:i32, offset:i64, whence:i32, newoffset_ptr:i32) -> i32
    try code.emitLocalGet(0); // fd
    try code.emitI32WrapI64();
    try code.emitLocalGet(1); // offset (already i64 in WASI)
    try code.emitLocalGet(2); // whence
    try code.emitI32WrapI64();
    try code.emitI32Const(SCRATCH_RESULT); // newoffset output ptr
    try code.emitCall(wasi_func_idx);
    try code.emitDrop(); // drop errno

    // Load new offset (i64) from SCRATCH_RESULT
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI64Load(3, 0);
    return try code.finish();
}

/// cot_fd_open shim: (path_ptr:i64, path_len:i64, flags:i64) -> i64
/// Maps to WASI path_open with sensible defaults
fn generateWasiPathOpenShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI path_open(dirfd, dirflags, path, path_len, oflags, rights_base, rights_inheriting, fdflags, opened_fd_ptr)
    // We use: dirfd=3 (preopened dir), dirflags=1 (SYMLINK_FOLLOW), oflags from flags param
    // rights = all bits set, fdflags = 0

    try code.emitI32Const(3); // dirfd = preopened dir (fd 3 is conventional in WASI)
    try code.emitI32Const(1); // dirflags = LOOKUPFLAGS_SYMLINK_FOLLOW
    try code.emitLocalGet(0); // path_ptr
    try code.emitI32WrapI64();
    try code.emitLocalGet(1); // path_len
    try code.emitI32WrapI64();
    try code.emitLocalGet(2); // oflags (from Cot flags param)
    try code.emitI32WrapI64();
    try code.emitI64Const(0x1FFFFFFF); // fs_rights_base = all read/write rights
    try code.emitI64Const(0x1FFFFFFF); // fs_rights_inheriting = all
    try code.emitI32Const(0); // fdflags = 0
    try code.emitI32Const(SCRATCH_RESULT); // opened_fd output ptr
    try code.emitCall(wasi_func_idx);
    try code.emitDrop(); // drop errno

    // Load opened fd from SCRATCH_RESULT
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_time shim: () -> i64
fn generateWasiTimeShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI clock_time_get(clock_id=0 [REALTIME], precision=1, time_ptr) -> i32
    try code.emitI32Const(0); // CLOCK_REALTIME
    try code.emitI64Const(1); // precision = 1ns
    try code.emitI32Const(SCRATCH_RESULT); // time output ptr
    try code.emitCall(wasi_func_idx);
    try code.emitDrop(); // drop errno

    // Load timestamp (i64) from SCRATCH_RESULT
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI64Load(3, 0);
    return try code.finish();
}

/// cot_random shim: (buf:i64, len:i64) -> i64
fn generateWasiRandomShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI random_get(buf:i32, buf_len:i32) -> i32
    try code.emitLocalGet(0); // buf
    try code.emitI32WrapI64();
    try code.emitLocalGet(1); // len
    try code.emitI32WrapI64();
    try code.emitCall(wasi_func_idx);
    try code.emitI64ExtendI32U(); // return errno as i64 (0 = success)
    return try code.finish();
}

/// cot_exit shim: (code:i64) -> void
fn generateWasiExitShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI proc_exit(code:i32) -> noreturn
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitCall(wasi_func_idx);
    try code.emitUnreachable();
    return try code.finish();
}

/// cot_args_count shim: () -> i64
fn generateWasiArgsCountShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // WASI args_sizes_get(argc_ptr:i32, argv_buf_size_ptr:i32) -> i32
    try code.emitI32Const(SCRATCH_RESULT); // argc output
    try code.emitI32Const(SCRATCH_RESULT + 4); // argv_buf_size output
    try code.emitCall(wasi_func_idx);
    try code.emitDrop(); // drop errno

    // Load argc from SCRATCH_RESULT
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_arg_len shim: (n:i64) -> i64
/// Calls args_sizes_get + args_get to populate argv, then computes strlen(argv[n])
fn generateWasiArgLenShim(allocator: std.mem.Allocator, wasi_args_sizes_get_idx: u32, wasi_args_get_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Declare locals: counter(1)
    _ = try code.declareLocals(&[_]wasm.ValType{.i32});
    // param 0 = n (i64), local 1 = counter (i32)

    // 1. args_sizes_get to get counts
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Const(SCRATCH_RESULT + 4);
    try code.emitCall(wasi_args_sizes_get_idx);
    try code.emitDrop();

    // 2. args_get to populate argv array and string buffer
    try code.emitI32Const(SCRATCH_ARGV);
    try code.emitI32Const(SCRATCH_ARGV_BUF);
    try code.emitCall(wasi_args_get_idx);
    try code.emitDrop();

    // 3. Load argv[n] pointer
    // argv[n] is at SCRATCH_ARGV + n * 4
    try code.emitI32Const(SCRATCH_ARGV);
    try code.emitLocalGet(0); // n
    try code.emitI32WrapI64();
    try code.emitI32Const(4);
    try code.emitI32Mul();
    try code.emitI32Add();
    try code.emitI32Load(2, 0); // argv[n] = pointer to string
    try code.emitLocalSet(1); // counter = argv[n] (reuse as string ptr)

    // 4. strlen: count bytes until null terminator
    // Use counter(1) as pointer, walk until *ptr == 0
    // Save base pointer
    _ = try code.declareLocals(&[_]wasm.ValType{.i32}); // local 2 = base_ptr
    try code.emitLocalGet(1);
    try code.emitLocalSet(2); // base_ptr = argv[n]

    // Loop: while (*counter != 0) counter++
    try code.emitBlock(wasm_op.BLOCK_VOID); // block (break target)
    try code.emitLoop(wasm_op.BLOCK_VOID); // loop
    {
        // Load byte at counter
        try code.emitLocalGet(1);
        try code.emitI32Load(0, 0); // load byte (will load 4 bytes but we mask)
        try code.emitI32Const(0xFF);
        try code.emitI32And(); // mask to single byte
        try code.emitI32Eqz(); // == 0?
        try code.emitBrIf(1); // if zero, break out of block

        // counter++
        try code.emitLocalGet(1);
        try code.emitI32Const(1);
        try code.emitI32Add();
        try code.emitLocalSet(1);

        try code.emitBr(0); // continue loop
    }
    try code.emitEnd(); // end loop
    try code.emitEnd(); // end block

    // Return counter - base_ptr (= strlen)
    try code.emitLocalGet(1);
    try code.emitLocalGet(2);
    try code.emitI32Sub();
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_arg_ptr shim: (n:i64) -> i64
/// Returns the wasm pointer to argv[n] string data
fn generateWasiArgPtrShim(allocator: std.mem.Allocator, wasi_args_sizes_get_idx: u32, wasi_args_get_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // 1. args_sizes_get
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Const(SCRATCH_RESULT + 4);
    try code.emitCall(wasi_args_sizes_get_idx);
    try code.emitDrop();

    // 2. args_get
    try code.emitI32Const(SCRATCH_ARGV);
    try code.emitI32Const(SCRATCH_ARGV_BUF);
    try code.emitCall(wasi_args_get_idx);
    try code.emitDrop();

    // 3. Return argv[n] pointer as i64
    try code.emitI32Const(SCRATCH_ARGV);
    try code.emitLocalGet(0); // n
    try code.emitI32WrapI64();
    try code.emitI32Const(4);
    try code.emitI32Mul();
    try code.emitI32Add();
    try code.emitI32Load(2, 0); // argv[n]
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_environ_count shim: () -> i64
fn generateWasiEnvironCountShim(allocator: std.mem.Allocator, wasi_func_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Const(SCRATCH_RESULT + 4);
    try code.emitCall(wasi_func_idx);
    try code.emitDrop();

    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_environ_len shim: (n:i64) -> i64
fn generateWasiEnvironLenShim(allocator: std.mem.Allocator, wasi_environ_sizes_get_idx: u32, wasi_environ_get_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32 });
    // param 0 = n, local 1 = ptr, local 2 = base

    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Const(SCRATCH_RESULT + 4);
    try code.emitCall(wasi_environ_sizes_get_idx);
    try code.emitDrop();

    try code.emitI32Const(SCRATCH_ENVIRON);
    try code.emitI32Const(SCRATCH_ENVIRON_BUF);
    try code.emitCall(wasi_environ_get_idx);
    try code.emitDrop();

    // Load environ[n] pointer
    try code.emitI32Const(SCRATCH_ENVIRON);
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitI32Const(4);
    try code.emitI32Mul();
    try code.emitI32Add();
    try code.emitI32Load(2, 0);
    try code.emitLocalTee(1);
    try code.emitLocalSet(2); // base = ptr

    // strlen loop
    try code.emitBlock(wasm_op.BLOCK_VOID);
    try code.emitLoop(wasm_op.BLOCK_VOID);
    {
        try code.emitLocalGet(1);
        try code.emitI32Load(0, 0);
        try code.emitI32Const(0xFF);
        try code.emitI32And();
        try code.emitI32Eqz();
        try code.emitBrIf(1);

        try code.emitLocalGet(1);
        try code.emitI32Const(1);
        try code.emitI32Add();
        try code.emitLocalSet(1);
        try code.emitBr(0);
    }
    try code.emitEnd();
    try code.emitEnd();

    try code.emitLocalGet(1);
    try code.emitLocalGet(2);
    try code.emitI32Sub();
    try code.emitI64ExtendI32U();
    return try code.finish();
}

/// cot_environ_ptr shim: (n:i64) -> i64
fn generateWasiEnvironPtrShim(allocator: std.mem.Allocator, wasi_environ_sizes_get_idx: u32, wasi_environ_get_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitI32Const(SCRATCH_RESULT);
    try code.emitI32Const(SCRATCH_RESULT + 4);
    try code.emitCall(wasi_environ_sizes_get_idx);
    try code.emitDrop();

    try code.emitI32Const(SCRATCH_ENVIRON);
    try code.emitI32Const(SCRATCH_ENVIRON_BUF);
    try code.emitCall(wasi_environ_get_idx);
    try code.emitDrop();

    try code.emitI32Const(SCRATCH_ENVIRON);
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitI32Const(4);
    try code.emitI32Mul();
    try code.emitI32Add();
    try code.emitI32Load(2, 0);
    try code.emitI64ExtendI32U();
    return try code.finish();
}

// =============================================================================
// Native stub bodies
// =============================================================================

fn generateFdWriteStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(52); // ENOSYS
    return try code.finish();
}

fn generateStubReturnsZero(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(0);
    return try code.finish();
}

fn generateStubReturnsNegOne(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(-1);
    return try code.finish();
}

fn generateStubTrap(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitUnreachable();
    return try code.finish();
}
