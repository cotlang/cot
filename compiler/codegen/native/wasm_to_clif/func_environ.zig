//! Wasm function environment for global variable handling.
//!
//! Port of wasmtime/crates/cranelift/src/func_environ.rs
//!
//! This module provides the FuncEnvironment struct which manages how Wasm
//! constructs (globals, memories, tables) are mapped to CLIF IR.

const std = @import("std");
const frontend_mod = @import("../frontend/mod.zig");
const heap_mod = @import("heap.zig");

// Re-export CLIF types
pub const clif = frontend_mod;
pub const Type = frontend_mod.Type;
pub const Function = frontend_mod.Function;
pub const GlobalValue = frontend_mod.GlobalValue;
pub const GlobalValueData = frontend_mod.GlobalValueData;
pub const FuncRef = frontend_mod.FuncRef;
pub const SigRef = frontend_mod.SigRef;
pub const Signature = frontend_mod.Signature;
pub const AbiParam = frontend_mod.AbiParam;

// Import ExtFuncData directly from clif module
const clif_ir = @import("../../../ir/clif/mod.zig");
pub const ExtFuncData = clif_ir.ExtFuncData;
pub const ExternalName = clif_ir.ExternalName;

// Re-export heap types
pub const HeapData = heap_mod.HeapData;
pub const MemArg = heap_mod.MemArg;

// ============================================================================
// TableData
// Port of cranelift table handling
// ============================================================================

/// Table data for WebAssembly tables.
///
/// Similar to HeapData but for function tables (funcref tables).
pub const TableData = struct {
    /// GlobalValue that computes the base address of the table.
    base: GlobalValue,

    /// GlobalValue that computes the address of the table bound.
    bound: GlobalValue,

    /// Size of each table element in bytes (typically 8 for funcref on 64-bit).
    element_size: u32,
};

// ============================================================================
// VMFuncRef Layout
// Port of wasmtime VM layout for function references
// ============================================================================

/// VMFuncRef offsets.
/// A funcref in Wasmtime's representation contains:
/// - wasm_call: pointer to the Wasm-calling-convention entry point
/// - vmctx: pointer to the callee's VMContext
/// - type_index: the type ID for runtime signature checking
pub const VMFuncRefOffsets = struct {
    /// Offset of wasm_call function pointer.
    pub const wasm_call: i32 = 0;
    /// Offset of vmctx pointer.
    pub const vmctx: i32 = 8;
    /// Offset of type_index.
    pub const type_index: i32 = 16;
    /// Total size of a VMFuncRef.
    pub const size: u32 = 24;
};

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
/// Wasm value type for function signatures.
/// Port of wasmtime_environ WasmValType.
/// Must match translator.zig WasmValType for type compatibility.
pub const WasmValType = enum {
    i32,
    i64,
    f32,
    f64,
    v128,
    funcref,
    externref,

    /// Convert to CLIF Type.
    pub fn toClifType(self: WasmValType) Type {
        return switch (self) {
            .i32 => Type.I32,
            .i64 => Type.I64,
            .f32 => Type.F32,
            .f64 => Type.F64,
            else => Type.I64, // References are pointers
        };
    }
};

/// Wasm function type signature.
/// Port of wasmtime_environ WasmFuncType.
pub const WasmFuncType = struct {
    params: []const WasmValType,
    results: []const WasmValType,
};

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

    /// Cached heap info per wasm memory index.
    /// Port of func_environ.rs heaps field.
    heaps: std.AutoHashMapUnmanaged(u32, HeapData),

    /// Cached function references per wasm function index.
    /// Port of func_environ.rs func_refs/sig_refs fields.
    func_refs: std.AutoHashMapUnmanaged(u32, FuncRef),

    /// Cached table data per wasm table index.
    /// Port of func_environ.rs tables field.
    tables: std.AutoHashMapUnmanaged(u32, TableData),

    /// Cached type IDs per wasm type index.
    /// Maps type_index -> GlobalValue for the type ID location.
    type_ids: std.AutoHashMapUnmanaged(u32, GlobalValue),

    /// Mapping from function_index -> type_index.
    /// Port of wasmtime_environ module.functions[].signature.
    func_to_type: []const u32,

    /// Function type signatures indexed by type_index.
    /// Port of wasmtime_environ module.types[].
    func_types: []const WasmFuncType,

    const Self = @This();

    /// Create a new function environment.
    pub fn init(allocator: std.mem.Allocator) Self {
        return initWithTypes(allocator, &[_]u32{}, &[_]WasmFuncType{});
    }

    /// Create a new function environment with type information.
    /// Port of Cranelift's FuncEnvironment with module access.
    pub fn initWithTypes(allocator: std.mem.Allocator, func_to_type: []const u32, func_types: []const WasmFuncType) Self {
        return .{
            .allocator = allocator,
            .pointer_type = Type.I64,
            .vmctx = null,
            .globals = .{},
            .heaps = .{},
            .func_refs = .{},
            .tables = .{},
            .type_ids = .{},
            .func_to_type = func_to_type,
            .func_types = func_types,
        };
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        self.globals.deinit(self.allocator);
        self.heaps.deinit(self.allocator);
        self.func_refs.deinit(self.allocator);
        self.tables.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
    }

    /// Clear cached state for reuse.
    pub fn clear(self: *Self) void {
        self.vmctx = null;
        self.globals.clearRetainingCapacity();
        self.heaps.clearRetainingCapacity();
        self.func_refs.clearRetainingCapacity();
        self.tables.clearRetainingCapacity();
        self.type_ids.clearRetainingCapacity();
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
        // is at its index * fixed stride.
        // Reference: Cranelift vmoffsets.rs — VMGlobalDefinition is ALWAYS 16 bytes
        // regardless of type (i32/i64/f32/f64 all padded to 16).
        // vmctx_vmglobal_definition(index) = globals_begin + index * 16
        const global_base_offset: i64 = 0x10000;
        const stride: i64 = 16; // Fixed 16-byte stride per Cranelift
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

    /// Get or create HeapData for a Wasm memory.
    ///
    /// Port of func_environ.rs:get_or_create_heap()
    ///
    /// Creates GlobalValue references for the heap's base address and bound.
    /// These are stored in the vmctx at known offsets.
    pub fn getOrCreateHeap(self: *Self, func: *Function, memory_index: u32) !*HeapData {
        // Check cache first
        if (self.heaps.getPtr(memory_index)) |heap| return heap;

        const vmctx = try self.vmctxVal(func);

        // VMContext layout for heaps (simplified):
        // Heap base is at vmctx + heap_data_offset + memory_index * heap_stride + 0
        // Heap bound is at vmctx + heap_data_offset + memory_index * heap_stride + 8
        const heap_data_offset: i64 = 0x20000; // After globals
        const heap_stride: i64 = 24; // base (8) + bound (8) + flags (8)

        const base_offset = heap_data_offset + @as(i64, memory_index) * heap_stride;
        const bound_offset = base_offset + 8;

        // Create GlobalValue for heap base address
        const base_gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = base_offset,
                .global_type = self.pointer_type,
            },
        });

        // Create GlobalValue for heap bound address
        const bound_gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = bound_offset,
                .global_type = self.pointer_type,
            },
        });

        const heap = HeapData{
            .base = base_gv,
            .bound = bound_gv,
            .index_type = Type.I32, // Wasm32 uses i32 indices
            .min_size = 0,
            .max_size = null,
            .offset_guard_size = 0, // No guard pages for now
        };

        try self.heaps.put(self.allocator, memory_index, heap);
        return self.heaps.getPtr(memory_index).?;
    }

    /// Get a previously created heap.
    pub fn getHeap(self: *Self, memory_index: u32) ?*const HeapData {
        return self.heaps.getPtr(memory_index);
    }

    /// Get or create a FuncRef for a Wasm function.
    ///
    /// Port of func_environ.rs:get_or_create_defined_func_ref()
    ///
    /// Creates an external function reference that can be used for direct calls.
    /// The signature includes the Wasm calling convention:
    /// params = [callee_vmctx, caller_vmctx, ...wasm_params] -> ...wasm_returns
    ///
    /// Reference: Cranelift code_translator.rs:654-660
    ///   let ty = environ.module.functions[function_index].signature.unwrap_module_type_index();
    ///   let sig_ref = environ.get_or_create_interned_sig_ref(builder.func, ty);
    pub fn getOrCreateFuncRef(self: *Self, func: *Function, function_index: u32) !FuncRef {
        // Check cache first
        if (self.func_refs.get(function_index)) |ref| return ref;

        // Build the signature with vmctx params
        // Wasm functions have signature: (callee_vmctx, caller_vmctx, ...wasm_params) -> ...wasm_returns
        var sig = Signature.init(.fast); // Use fast calling convention

        // Add the two vmctx parameters (required by Wasm calling convention)
        try sig.params.append(self.allocator, AbiParam.init(self.pointer_type));
        try sig.params.append(self.allocator, AbiParam.init(self.pointer_type));

        // Look up the function's type using the function→type mapping
        // Port of Cranelift: environ.module.functions[function_index].signature
        if (function_index < self.func_to_type.len) {
            const type_idx = self.func_to_type[function_index];
            if (type_idx < self.func_types.len) {
                const wasm_func_type = self.func_types[type_idx];

                // Add Wasm parameters
                for (wasm_func_type.params) |param| {
                    try sig.params.append(self.allocator, AbiParam.init(param.toClifType()));
                }

                // Add Wasm returns
                for (wasm_func_type.results) |result| {
                    try sig.returns.append(self.allocator, AbiParam.init(result.toClifType()));
                }
            }
        }

        // Create the signature ref
        const sig_ref = try func.importSignature(self.allocator, sig);

        // Create the function reference
        const ext_func = ExtFuncData{
            .name = .{ .user = .{ .namespace = 0, .index = function_index } },
            .signature = sig_ref,
            .colocated = true, // Local function in same module
        };

        const func_ref = try func.importFunction(self.allocator, ext_func);
        try self.func_refs.put(self.allocator, function_index, func_ref);
        return func_ref;
    }

    /// Get or create TableData for a Wasm table.
    ///
    /// Port of func_environ.rs:get_or_create_table()
    ///
    /// Creates GlobalValue references for the table's base address and bound.
    pub fn getOrCreateTable(self: *Self, func: *Function, table_index: u32) !*TableData {
        // Check cache first
        if (self.tables.getPtr(table_index)) |table| return table;

        const vmctx = try self.vmctxVal(func);

        // VMContext layout for tables (simplified):
        // Table base is at vmctx + table_data_offset + table_index * table_stride + 0
        // Table bound is at vmctx + table_data_offset + table_index * table_stride + 8
        const table_data_offset: i64 = 0x30000; // After heaps
        const table_stride: i64 = 24; // base (8) + bound (8) + flags (8)

        const base_offset = table_data_offset + @as(i64, table_index) * table_stride;
        const bound_offset = base_offset + 8;

        // Create GlobalValue for table base address
        const base_gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = base_offset,
                .global_type = self.pointer_type,
            },
        });

        // Create GlobalValue for table bound address
        const bound_gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = bound_offset,
                .global_type = self.pointer_type,
            },
        });

        const table = TableData{
            .base = base_gv,
            .bound = bound_gv,
            .element_size = VMFuncRefOffsets.size, // Each element is a funcref
        };

        try self.tables.put(self.allocator, table_index, table);
        return self.tables.getPtr(table_index).?;
    }

    /// Get or create a GlobalValue for a type ID location.
    ///
    /// Port of func_environ.rs:module_interned_to_shared_ty()
    ///
    /// The type ID is stored in a type IDs array in vmctx.
    pub fn getOrCreateTypeIdGV(self: *Self, func: *Function, type_index: u32) !GlobalValue {
        // Check cache first
        if (self.type_ids.get(type_index)) |gv| return gv;

        const vmctx = try self.vmctxVal(func);

        // VMContext layout for type IDs:
        // Type IDs array is at vmctx + type_ids_offset
        // Each type ID is a u32
        const type_ids_offset: i64 = 0x40000; // After tables
        const offset = type_ids_offset + @as(i64, type_index) * 4;

        const gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = offset,
                .global_type = Type.I32, // Type IDs are u32
            },
        });

        try self.type_ids.put(self.allocator, type_index, gv);
        return gv;
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
