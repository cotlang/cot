//! Cranelift JIT FFI Declarations
//!
//! This file declares the C API that the Rust/Cranelift bridge will implement.
//! When building without JIT, these are stubbed out.
//!
//! The Rust side (src/jit/cranelift-bridge/) will implement these functions.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

/// Check if JIT is enabled at compile time
pub const jit_enabled = if (@hasDecl(build_options, "enable_jit")) build_options.enable_jit else false;

/// Opaque handle to the JIT context (Rust side owns the memory)
pub const JitContext = opaque {};

/// Result of JIT compilation
pub const CompiledFunction = extern struct {
    /// Pointer to the compiled native code
    code_ptr: ?*const u8,
    /// Size of the compiled code in bytes
    code_size: usize,
    /// Error code (0 = success)
    error_code: u32,
    /// Error message (null-terminated, or null if no error)
    error_message: ?[*:0]const u8,

    pub fn isSuccess(self: CompiledFunction) bool {
        return self.error_code == 0 and self.code_ptr != null;
    }

    pub fn getError(self: CompiledFunction) ?[]const u8 {
        if (self.error_message) |msg| {
            return std.mem.sliceTo(msg, 0);
        }
        return null;
    }
};

/// JIT compilation options
pub const CompileOptions = extern struct {
    /// Optimization level: 0 = none, 1 = basic, 2 = full
    opt_level: u8 = 1,
    /// Enable debug info generation
    debug_info: bool = false,
    /// Enable bounds checking (can be disabled for performance)
    bounds_checks: bool = true,

    pub const default: CompileOptions = .{};
};

// =============================================================================
// FFI Function Declarations
// =============================================================================
// These are implemented by the Rust crate (cot-cranelift) and linked at build time.
// When JIT is disabled, we provide stub implementations below.

extern "C" {
    /// Initialize the JIT context
    /// Returns: Opaque pointer to JIT context, or null on failure
    fn cot_jit_init() ?*JitContext;

    /// Destroy the JIT context and free all resources
    fn cot_jit_destroy(ctx: *JitContext) void;

    /// Register a runtime function that JIT code can call
    /// name: null-terminated function name
    /// ptr: function pointer with C calling convention
    fn cot_jit_register_runtime_fn(ctx: *JitContext, name: [*:0]const u8, ptr: *const anyopaque) void;

    /// Compile IR to native code
    /// ir_bytes: pointer to serialized IR
    /// ir_len: length of serialized IR
    /// options: compilation options
    /// Returns: CompiledFunction with code pointer or error
    fn cot_jit_compile(
        ctx: *JitContext,
        ir_bytes: [*]const u8,
        ir_len: usize,
        options: CompileOptions,
    ) CompiledFunction;

    /// Free a compiled function's resources
    fn cot_jit_free_compiled(ctx: *JitContext, func: *CompiledFunction) void;

    /// Get the last error message (for debugging)
    fn cot_jit_get_last_error(ctx: *JitContext) ?[*:0]const u8;

    /// Get Cranelift version string
    fn cot_jit_version() [*:0]const u8;
}

// =============================================================================
// Zig Wrappers (safe interface)
// =============================================================================

/// Initialize JIT - returns null if JIT is disabled or initialization fails
pub fn init() ?*JitContext {
    if (!jit_enabled) return null;
    return cot_jit_init();
}

/// Destroy JIT context
pub fn destroy(ctx: *JitContext) void {
    if (!jit_enabled) return;
    cot_jit_destroy(ctx);
}

/// Register a runtime function
pub fn registerRuntimeFn(ctx: *JitContext, name: [:0]const u8, ptr: *const anyopaque) void {
    if (!jit_enabled) return;
    cot_jit_register_runtime_fn(ctx, name.ptr, ptr);
}

/// Compile IR bytes to native code
pub fn compile(ctx: *JitContext, ir_bytes: []const u8, options: CompileOptions) CompiledFunction {
    if (!jit_enabled) {
        return .{
            .code_ptr = null,
            .code_size = 0,
            .error_code = 1,
            .error_message = "JIT not enabled",
        };
    }
    return cot_jit_compile(ctx, ir_bytes.ptr, ir_bytes.len, options);
}

/// Free compiled function
pub fn freeCompiled(ctx: *JitContext, func: *CompiledFunction) void {
    if (!jit_enabled) return;
    cot_jit_free_compiled(ctx, func);
}

/// Get last error
pub fn getLastError(ctx: *JitContext) ?[]const u8 {
    if (!jit_enabled) return null;
    if (cot_jit_get_last_error(ctx)) |msg| {
        return std.mem.sliceTo(msg, 0);
    }
    return null;
}

/// Get Cranelift version
pub fn getVersion() []const u8 {
    if (!jit_enabled) return "JIT disabled";
    return std.mem.sliceTo(cot_jit_version(), 0);
}

// =============================================================================
// Stub implementations (when JIT is disabled)
// =============================================================================

comptime {
    if (!jit_enabled) {
        // When JIT is disabled, we need to provide stub implementations
        // so the program links without the Rust library
        @export(&stub_jit_init, .{ .name = "cot_jit_init" });
        @export(&stub_jit_destroy, .{ .name = "cot_jit_destroy" });
        @export(&stub_jit_register_runtime_fn, .{ .name = "cot_jit_register_runtime_fn" });
        @export(&stub_jit_compile, .{ .name = "cot_jit_compile" });
        @export(&stub_jit_free_compiled, .{ .name = "cot_jit_free_compiled" });
        @export(&stub_jit_get_last_error, .{ .name = "cot_jit_get_last_error" });
        @export(&stub_jit_version, .{ .name = "cot_jit_version" });
    }
}

fn stub_jit_init() ?*JitContext {
    return null;
}

fn stub_jit_destroy(_: *JitContext) void {}

fn stub_jit_register_runtime_fn(_: *JitContext, _: [*:0]const u8, _: *const anyopaque) void {}

fn stub_jit_compile(_: *JitContext, _: [*]const u8, _: usize, _: CompileOptions) CompiledFunction {
    return .{
        .code_ptr = null,
        .code_size = 0,
        .error_code = 1,
        .error_message = "JIT not enabled",
    };
}

fn stub_jit_free_compiled(_: *JitContext, _: *CompiledFunction) void {}

fn stub_jit_get_last_error(_: *JitContext) ?[*:0]const u8 {
    return "JIT not enabled";
}

fn stub_jit_version() [*:0]const u8 {
    return "disabled";
}

// =============================================================================
// Tests
// =============================================================================

test "stub functions work when JIT disabled" {
    // These should not crash even without the Rust library
    const ctx = init();
    try std.testing.expect(ctx == null);

    const version = getVersion();
    try std.testing.expect(version.len > 0);
}
