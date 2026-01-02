//! Cot JIT Module
//!
//! This module provides JIT (Just-In-Time) compilation for Cot via Cranelift.
//! JIT is optional - when disabled, Cot uses the bytecode interpreter.
//!
//! ## Usage
//!
//! ```zig
//! const jit = @import("jit/jit.zig");
//!
//! // Check if JIT is available
//! if (jit.isEnabled()) {
//!     var manager = try jit.JitManager.init(allocator);
//!     defer manager.deinit();
//!
//!     try manager.compileModule(ir_module);
//! }
//! ```
//!
//! ## Build Configuration
//!
//! Enable JIT with: `zig build -Djit=true`
//!
//! This requires the Cranelift Rust bridge library to be built and linked.

const std = @import("std");

// Re-export submodules
pub const ffi = @import("ffi.zig");
pub const ir_serializer = @import("ir_serializer.zig");
pub const runtime_exports = @import("runtime_exports.zig");
pub const jit_manager = @import("jit_manager.zig");

// Main types
pub const JitManager = jit_manager.JitManager;
pub const IrSerializer = ir_serializer.IrSerializer;
pub const CompiledFn = jit_manager.CompiledFn;
pub const Strategy = jit_manager.Strategy;

// FFI types
pub const JitContext = ffi.JitContext;
pub const CompiledFunction = ffi.CompiledFunction;
pub const CompileOptions = ffi.CompileOptions;

/// Check if JIT is enabled at compile time
pub const jit_enabled = ffi.jit_enabled;

/// Convenience function to check if JIT is available
pub fn isEnabled() bool {
    return ffi.jit_enabled;
}

/// Get Cranelift version string
pub fn getVersion() []const u8 {
    return ffi.getVersion();
}

// Global JIT manager functions
pub const initGlobal = jit_manager.initGlobal;
pub const getGlobal = jit_manager.getGlobal;
pub const deinitGlobal = jit_manager.deinitGlobal;

// =============================================================================
// Tests
// =============================================================================

test "jit module exports" {
    // Verify all exports are accessible
    _ = JitManager;
    _ = IrSerializer;
    _ = jit_enabled;
    _ = isEnabled();
    _ = getVersion();
}

test {
    // Run all submodule tests
    std.testing.refAllDecls(@This());
}
