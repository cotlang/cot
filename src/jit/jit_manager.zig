//! JIT Manager
//!
//! Orchestrates JIT compilation for Cot. This is the main entry point
//! for compiling IR to native code via Cranelift.
//!
//! Usage:
//! ```zig
//! var jit = try JitManager.init(allocator);
//! defer jit.deinit();
//!
//! // Compile a module
//! try jit.compileModule(ir_module);
//!
//! // Get compiled function
//! if (jit.getFunction("my_func")) |func_ptr| {
//!     const result = @call(.auto, func_ptr, .{arg1, arg2});
//! }
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/ir.zig");
const ffi = @import("ffi.zig");
const IrSerializer = @import("ir_serializer.zig").IrSerializer;
const runtime_exports = @import("runtime_exports.zig");

/// Compilation strategy
pub const Strategy = enum {
    /// Interpret everything (JIT disabled)
    interpret_only,

    /// Compile all functions ahead of time when module is loaded
    compile_all,

    /// Compile functions on first call
    compile_on_demand,

    /// Profile and compile hot functions (tiered compilation)
    tiered,
};

/// Compiled function metadata
pub const CompiledFn = struct {
    /// Native code pointer
    code_ptr: *const anyopaque,
    /// Size of compiled code
    code_size: usize,
    /// Original function name
    name: []const u8,
    /// Function signature for type checking
    param_count: u8,
    has_return: bool,
};

/// JIT Manager
pub const JitManager = struct {
    allocator: Allocator,
    jit_ctx: ?*ffi.JitContext,
    serializer: IrSerializer,
    compiled_functions: std.StringHashMap(CompiledFn),
    strategy: Strategy,
    enabled: bool,

    const Self = @This();

    /// Initialize the JIT manager
    pub fn init(allocator: Allocator) !Self {
        var self = Self{
            .allocator = allocator,
            .jit_ctx = null,
            .serializer = IrSerializer.init(allocator),
            .compiled_functions = std.StringHashMap(CompiledFn).init(allocator),
            .strategy = .compile_all,
            .enabled = ffi.jit_enabled,
        };

        if (self.enabled) {
            self.jit_ctx = ffi.init();
            if (self.jit_ctx) |ctx| {
                // Register all runtime functions
                runtime_exports.registerAll(ctx);
            } else {
                // JIT initialization failed, fall back to interpreter
                self.enabled = false;
            }
        }

        return self;
    }

    /// Deinitialize the JIT manager
    pub fn deinit(self: *Self) void {
        if (self.jit_ctx) |ctx| {
            ffi.destroy(ctx);
        }
        self.serializer.deinit();
        self.compiled_functions.deinit();
    }

    /// Check if JIT is available and enabled
    pub fn isEnabled(self: *const Self) bool {
        return self.enabled and self.jit_ctx != null;
    }

    /// Get the Cranelift version string
    pub fn getVersion(self: *const Self) []const u8 {
        _ = self;
        return ffi.getVersion();
    }

    /// Set the compilation strategy
    pub fn setStrategy(self: *Self, strategy: Strategy) void {
        self.strategy = strategy;
    }

    /// Compile an entire IR module
    pub fn compileModule(self: *Self, module: *const ir.Module) !void {
        if (!self.isEnabled()) return error.JitNotEnabled;

        const ctx = self.jit_ctx orelse return error.JitNotEnabled;

        // Serialize the module to binary format
        const ir_bytes = try self.serializer.serializeModule(module);

        // Compile via Cranelift
        var result = ffi.compile(ctx, ir_bytes, .{});
        defer ffi.freeCompiled(ctx, &result);

        if (!result.isSuccess()) {
            if (result.getError()) |err_msg| {
                std.log.err("JIT compilation failed: {s}", .{err_msg});
            }
            return error.CompilationFailed;
        }

        // Store compiled functions
        for (module.functions.items) |func| {
            const name = try self.allocator.dupe(u8, func.name);
            try self.compiled_functions.put(name, .{
                .code_ptr = @ptrCast(result.code_ptr.?),
                .code_size = result.code_size,
                .name = name,
                .param_count = @intCast(func.signature.params.len),
                .has_return = func.signature.return_type != .void,
            });
        }
    }

    /// Compile a single function
    pub fn compileFunction(self: *Self, func: *const ir.Function) !void {
        if (!self.isEnabled()) return error.JitNotEnabled;

        // Create a temporary module containing just this function
        var temp_module = ir.Module.init(self.allocator, "temp");
        defer temp_module.deinit();

        // Note: We need to add the function without transferring ownership
        // For now, just serialize it directly
        _ = func;

        // TODO: Implement single-function compilation
        return error.NotImplemented;
    }

    /// Get a compiled function by name
    pub fn getFunction(self: *Self, name: []const u8) ?*const anyopaque {
        if (self.compiled_functions.get(name)) |compiled| {
            return compiled.code_ptr;
        }
        return null;
    }

    /// Get compiled function metadata
    pub fn getFunctionInfo(self: *Self, name: []const u8) ?CompiledFn {
        return self.compiled_functions.get(name);
    }

    /// Check if a function is compiled
    pub fn isCompiled(self: *Self, name: []const u8) bool {
        return self.compiled_functions.contains(name);
    }

    /// Get compilation statistics
    pub fn getStats(self: *const Self) Stats {
        var total_code_size: usize = 0;
        var iter = self.compiled_functions.valueIterator();
        while (iter.next()) |compiled| {
            total_code_size += compiled.code_size;
        }

        return .{
            .functions_compiled = self.compiled_functions.count(),
            .total_code_size = total_code_size,
            .jit_enabled = self.enabled,
        };
    }

    pub const Stats = struct {
        functions_compiled: usize,
        total_code_size: usize,
        jit_enabled: bool,
    };
};

/// Execute a compiled function with the given arguments
/// This is a helper for calling JIT-compiled code
pub fn callCompiled(
    comptime ReturnType: type,
    comptime ArgTypes: type,
    func_ptr: *const anyopaque,
    args: ArgTypes,
) ReturnType {
    const FnType = *const fn (ArgTypes) callconv(.C) ReturnType;
    const func: FnType = @ptrCast(func_ptr);
    return @call(.auto, func, args);
}

// =============================================================================
// Convenience Functions
// =============================================================================

/// Global JIT manager instance (optional singleton pattern)
var global_jit: ?*JitManager = null;

/// Initialize the global JIT manager
pub fn initGlobal(allocator: Allocator) !void {
    if (global_jit != null) return error.AlreadyInitialized;

    const jit = try allocator.create(JitManager);
    jit.* = try JitManager.init(allocator);
    global_jit = jit;
}

/// Get the global JIT manager
pub fn getGlobal() ?*JitManager {
    return global_jit;
}

/// Deinitialize the global JIT manager
pub fn deinitGlobal(allocator: Allocator) void {
    if (global_jit) |jit| {
        jit.deinit();
        allocator.destroy(jit);
        global_jit = null;
    }
}

// =============================================================================
// Tests
// =============================================================================

test "JitManager initialization" {
    const allocator = std.testing.allocator;
    var jit = try JitManager.init(allocator);
    defer jit.deinit();

    // JIT may or may not be enabled depending on build
    const stats = jit.getStats();
    try std.testing.expectEqual(@as(usize, 0), stats.functions_compiled);
}

test "JitManager version" {
    const allocator = std.testing.allocator;
    var jit = try JitManager.init(allocator);
    defer jit.deinit();

    const version = jit.getVersion();
    try std.testing.expect(version.len > 0);
}
