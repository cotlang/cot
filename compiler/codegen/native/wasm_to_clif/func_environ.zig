//! Wasm function environment for global variable handling.
//!
//! Port of wasmtime/crates/cranelift/src/func_environ.rs
//!
//! This module provides the FuncEnvironment struct which manages how Wasm
//! constructs (globals, memories, tables) are mapped to CLIF IR.

const std = @import("std");
const frontend_mod = @import("../frontend/mod.zig");

// Re-export CLIF types
pub const clif = frontend_mod;
pub const Type = frontend_mod.Type;
pub const Function = frontend_mod.Function;
pub const GlobalValue = frontend_mod.GlobalValue;
pub const GlobalValueData = frontend_mod.GlobalValueData;

// ============================================================================
// ConstantValue
// Port of wasmtime_environ::GlobalConstValue
// ============================================================================

/// Constant value for a Wasm global.
///
/// Used when a global is known to be constant and can be emitted directly
/// without memory access.
pub const ConstantValue = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};

// ============================================================================
// GlobalVariable
// Port of cranelift-wasm/src/environ/spec.rs GlobalVariable enum
// ============================================================================

/// Represents how a Wasm global variable is implemented in CLIF IR.
///
/// Following Cranelift's architecture:
/// - Constant globals emit iconst/fconst directly
/// - Memory globals use global_value instruction + load/store
pub const GlobalVariable = union(enum) {
    /// Global is a compile-time constant.
    ///
    /// The value can be emitted directly as an iconst/fconst instruction
    /// without any memory access.
    constant: ConstantValue,

    /// Global is stored in memory.
    ///
    /// The address is computed via a GlobalValue chain (typically vmctx → iadd_imm).
    /// Access requires a load/store instruction.
    memory: struct {
        /// GlobalValue that computes the address.
        gv: GlobalValue,
        /// Additional offset from the GlobalValue address.
        offset: i32,
        /// Type of the global value.
        ty: Type,
    },
};

// ============================================================================
// FuncEnvironment
// Port of wasmtime/crates/cranelift/src/func_environ.rs FuncEnvironment
// ============================================================================

/// Function environment for Wasm to CLIF translation.
///
/// Manages the mapping of Wasm constructs (globals, memories, tables) to CLIF IR.
/// This is created once per function translation and caches GlobalValue entries
/// to avoid redundant allocations.
///
/// Following Cranelift's pattern:
/// - VMContext is a single GlobalValue representing the runtime context
/// - Individual globals are accessed via a chain: vmctx → iadd_imm → load/store
pub const FuncEnvironment = struct {
    allocator: std.mem.Allocator,

    /// Pointer type for the target (typically I64 for 64-bit).
    pointer_type: Type,

    /// VMContext global value (created once per function).
    /// This is the base address for all runtime data access.
    vmctx: ?GlobalValue,

    /// Cached global variable info per wasm global index.
    /// Avoids creating duplicate GlobalValue entries.
    globals: std.AutoHashMapUnmanaged(u32, GlobalVariable),

    const Self = @This();

    /// Create a new function environment.
    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .pointer_type = Type.I64,
            .vmctx = null,
            .globals = .{},
        };
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        self.globals.deinit(self.allocator);
    }

    /// Clear cached state for reuse.
    pub fn clear(self: *Self) void {
        self.vmctx = null;
        self.globals.clearRetainingCapacity();
    }

    /// Get or create the VMContext global value.
    ///
    /// The VMContext is a special global value that represents the base address
    /// of the runtime context structure. All other globals are accessed relative
    /// to this address.
    ///
    /// Port of func_environ.rs:vmctx()
    pub fn vmctxVal(self: *Self, func: *Function) !GlobalValue {
        if (self.vmctx) |gv| return gv;

        const gv = try func.createGlobalValue(.vmcontext);
        self.vmctx = gv;
        return gv;
    }

    /// Get or create GlobalVariable info for a Wasm global.
    ///
    /// This follows Cranelift's pattern from func_environ.rs:get_or_create_global():
    /// 1. Check if already cached
    /// 2. For constants: return ConstantValue directly
    /// 3. For memory globals: create vmctx → iadd_imm chain
    ///
    /// The offset calculation assumes globals are stored contiguously at a known
    /// offset from vmctx. In a full implementation, this would consult the
    /// module's global layout.
    pub fn getOrCreateGlobal(
        self: *Self,
        func: *Function,
        global_index: u32,
        global_type: Type,
        is_constant: bool,
        constant_value: ?ConstantValue,
    ) !GlobalVariable {
        // Check cache first
        if (self.globals.get(global_index)) |gv| return gv;

        // Handle constant globals
        if (is_constant) {
            const var_info: GlobalVariable = .{
                .constant = constant_value.?,
            };
            try self.globals.put(self.allocator, global_index, var_info);
            return var_info;
        }

        // Memory global: create vmctx → iadd_imm chain
        // Following Cranelift's pattern from func_environ.rs:3160-3180
        const vmctx = try self.vmctxVal(func);

        // Calculate offset: globals are stored at a base offset from vmctx.
        // Global base is at offset 0x10000 from vmctx, then each global
        // is at its index * stride.
        const global_base_offset: i64 = 0x10000;
        const stride: i64 = if (global_type.eql(Type.I32) or global_type.eql(Type.F32))
            4
        else if (global_type.eql(Type.I64) or global_type.eql(Type.F64))
            8
        else
            8; // Default to 8 for unknown types
        const offset = global_base_offset + @as(i64, global_index) * stride;

        // Create iadd_imm GlobalValue: vmctx + offset
        const gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = offset,
                .global_type = self.pointer_type,
            },
        });

        const var_info: GlobalVariable = .{
            .memory = .{
                .gv = gv,
                .offset = 0,
                .ty = global_type,
            },
        };
        try self.globals.put(self.allocator, global_index, var_info);
        return var_info;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "FuncEnvironment basic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var env = FuncEnvironment.init(allocator);
    defer env.deinit();

    try testing.expect(env.vmctx == null);
    try testing.expectEqual(Type.I64, env.pointer_type);
}

test "FuncEnvironment vmctxVal creates once" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var env = FuncEnvironment.init(allocator);
    defer env.deinit();

    const gv1 = try env.vmctxVal(&func);
    const gv2 = try env.vmctxVal(&func);

    try testing.expect(gv1.eql(gv2));
    try testing.expectEqual(@as(usize, 1), func.numGlobalValues());
}

test "FuncEnvironment getOrCreateGlobal constant" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var env = FuncEnvironment.init(allocator);
    defer env.deinit();

    const var_info = try env.getOrCreateGlobal(
        &func,
        0,
        Type.I32,
        true,
        .{ .i32 = 42 },
    );

    switch (var_info) {
        .constant => |c| {
            switch (c) {
                .i32 => |v| try testing.expectEqual(@as(i32, 42), v),
                else => return error.UnexpectedConstantType,
            }
        },
        .memory => return error.ExpectedConstant,
    }
}

test "FuncEnvironment getOrCreateGlobal memory" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var env = FuncEnvironment.init(allocator);
    defer env.deinit();

    const var_info = try env.getOrCreateGlobal(
        &func,
        0,
        Type.I32,
        false,
        null,
    );

    switch (var_info) {
        .memory => |m| {
            try testing.expectEqual(Type.I32, m.ty);
            try testing.expectEqual(@as(i32, 0), m.offset);
            // Should have created 2 global values: vmctx and iadd_imm
            try testing.expectEqual(@as(usize, 2), func.numGlobalValues());
        },
        .constant => return error.ExpectedMemory,
    }
}

test "FuncEnvironment getOrCreateGlobal caching" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var env = FuncEnvironment.init(allocator);
    defer env.deinit();

    // Create first global
    _ = try env.getOrCreateGlobal(&func, 0, Type.I32, false, null);
    const count1 = func.numGlobalValues();

    // Request same global again - should use cache
    _ = try env.getOrCreateGlobal(&func, 0, Type.I32, false, null);
    const count2 = func.numGlobalValues();

    try testing.expectEqual(count1, count2);
}
