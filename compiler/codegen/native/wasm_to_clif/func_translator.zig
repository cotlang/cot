//! Function-level Wasm to CLIF translator.
//!
//! Port of wasmtime/crates/cranelift/src/translate/func_translator.rs
//!
//! This module provides a higher-level interface for translating entire
//! WebAssembly functions using FuncTranslator from translator.zig.

const std = @import("std");
const stack_mod = @import("stack.zig");
const translator_mod = @import("translator.zig");
const frontend_mod = @import("../frontend/mod.zig");
const heap_mod = @import("heap.zig");

// Re-export types
pub const clif = stack_mod.clif;
pub const Block = stack_mod.Block;
pub const Value = stack_mod.Value;
pub const TranslationState = stack_mod.TranslationState;
pub const FuncTranslator = translator_mod.FuncTranslator;
pub const WasmOpcode = translator_mod.WasmOpcode;
pub const WasmGlobalType = translator_mod.WasmGlobalType;
pub const WasmFuncType = translator_mod.WasmFuncType;
pub const TranslatorWasmValType = translator_mod.WasmValType;
pub const Type = frontend_mod.Type;
pub const Function = frontend_mod.Function;
pub const FunctionBuilder = frontend_mod.FunctionBuilder;
pub const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;

// Heap/memory types
pub const MemArg = heap_mod.MemArg;

// ============================================================================
// Wasm Value Type
// ============================================================================

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

// ============================================================================
// Local Declaration
// ============================================================================

pub const LocalDecl = struct {
    count: u32,
    val_type: WasmValType,
};

// ============================================================================
// Function Signature (simplified)
// ============================================================================

pub const FuncSignature = struct {
    params: []const WasmValType,
    results: []const WasmValType,
};

// ============================================================================
// WasmFuncTranslator
// High-level function translator using FunctionBuilder
// ============================================================================

/// WebAssembly to CLIF IR function translator.
///
/// A `WasmFuncTranslator` is used to translate a binary WebAssembly function into CLIF IR.
/// It manages the FunctionBuilder and FuncTranslator for translation.
pub const WasmFuncTranslator = struct {
    /// Allocator.
    allocator: std.mem.Allocator,
    /// Reusable builder context.
    builder_ctx: FunctionBuilderContext,
    /// Module-level globals (for type lookup during translation).
    globals: []const WasmGlobalType,
    /// Module-level function types (for indirect call signature lookup).
    func_types: []const WasmFuncType,
    /// Mapping from function_index -> type_index.
    /// Port of wasmtime_environ module.functions[].signature.
    func_to_type: []const u32,
    /// Table elements from the element section: table_index -> function_index.
    /// Used for AOT call_indirect resolution.
    table_elements: []const u32,

    const Self = @This();

    /// Create a new translator with module-level information.
    /// Port of Cranelift's FuncTranslator with module access.
    pub fn init(
        allocator: std.mem.Allocator,
        globals: []const WasmGlobalType,
        func_types: []const WasmFuncType,
        func_to_type: []const u32,
        table_elements: []const u32,
    ) Self {
        return .{
            .allocator = allocator,
            .builder_ctx = FunctionBuilderContext.init(allocator),
            .globals = globals,
            .func_types = func_types,
            .func_to_type = func_to_type,
            .table_elements = table_elements,
        };
    }

    /// Create a new translator without module info (backwards compatibility).
    pub fn initWithoutGlobals(allocator: std.mem.Allocator) Self {
        return init(allocator, &[_]WasmGlobalType{}, &[_]WasmFuncType{}, &[_]u32{}, &[_]u32{});
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        self.builder_ctx.deinit();
    }

    /// Translate a WebAssembly function into a CLIF Function.
    ///
    /// The function body is provided as a sequence of operators.
    /// Local declarations are provided separately.
    pub fn translateFunction(
        self: *Self,
        func: *Function,
        signature: FuncSignature,
        locals: []const LocalDecl,
        operators: []const WasmOperator,
    ) !void {

        // 1. Set up function signature with parameters and returns
        //    (must be done before creating FunctionBuilder)
        //
        //    Port of Cranelift's Wasm calling convention:
        //    params = [callee_vmctx, caller_vmctx, ...wasm_params] -> ...wasm_returns
        //    Reference: wasmtime/cranelift/src/func_environ.rs
        func.signature.params.clearRetainingCapacity();
        func.signature.returns.clearRetainingCapacity();

        // Add vmctx parameters (required by Wasm calling convention)
        // callee_vmctx with vmctx purpose - this is what GlobalValue::VMContext resolves to
        try func.signature.params.append(self.allocator, clif.AbiParam.special(clif.Type.I64, .vmctx));
        // caller_vmctx (normal purpose, used for calls between functions)
        try func.signature.params.append(self.allocator, clif.AbiParam.init(clif.Type.I64));

        // Add Wasm parameters
        for (signature.params) |p| {
            try func.signature.params.append(self.allocator, clif.AbiParam.init(p.toClifType()));
        }
        for (signature.results) |r| {
            try func.signature.returns.append(self.allocator, clif.AbiParam.init(r.toClifType()));
        }

        // 2. Set up function builder
        var builder = FunctionBuilder.init(func, &self.builder_ctx);

        // 3. Create translator with module info for type lookup
        var translator = FuncTranslator.init(self.allocator, &builder, self.globals, self.func_types, self.func_to_type, self.table_elements);
        defer translator.deinit();

        // 4. Calculate total number of locals (params + declared locals)
        var num_locals: u32 = 0;
        for (locals) |local| {
            num_locals += local.count;
        }

        // 5. Build type arrays
        var param_types = try self.allocator.alloc(Type, signature.params.len);
        defer self.allocator.free(param_types);
        for (signature.params, 0..) |p, i| {
            param_types[i] = p.toClifType();
        }

        var local_types = try self.allocator.alloc(Type, num_locals);
        defer self.allocator.free(local_types);
        var local_idx: usize = 0;
        for (locals) |local| {
            const ty = local.val_type.toClifType();
            for (0..local.count) |_| {
                local_types[local_idx] = ty;
                local_idx += 1;
            }
        }

        // 6. Initialize the translator
        try translator.initializeFunction(
            @intCast(signature.params.len),
            num_locals,
            param_types,
            local_types,
            signature.results.len,
        );

        // 7. Translate the function body
        for (operators) |op| {
            try self.translateOperator(&translator, op);
        }

        // 8. Emit return instruction AFTER all operators
        // Following Cranelift's func_translator.rs:271-283:
        // "The final `End` operator left us in the exit block where we need to
        // manually add a return instruction."
        if (translator.state.reachable) {
            // Get return values from the stack (pushed by the final End)
            const return_count = signature.results.len;
            const return_args = translator.state.peekn(return_count);
            _ = try translator.builder.ins().return_(return_args);
        }

        // 9. Finalize the builder
        builder.finalize();
    }

    /// Translate a single Wasm operator.
    /// Following Cranelift's pattern (code_translator.rs:129-131 and translate_unreachable_operator),
    /// we check reachability before translating operators. Unreachable control flow operators
    /// push placeholder entries onto the control stack but don't emit code.
    fn translateOperator(self: *Self, translator: *FuncTranslator, op: WasmOperator) !void {
        _ = self;

        // Handle unreachable code specially (Cranelift: translate_unreachable_operator)
        if (!translator.state.reachable) {
            switch (op) {
                // Control flow operators still need to update control stack
                .block => |data| return try translator.translateUnreachableBlock(data.params, data.results),
                .loop => |data| return try translator.translateUnreachableBlock(data.params, data.results),
                .if_op => |data| return try translator.translateUnreachableIf(data.params, data.results),
                .else_op => return try translator.translateElse(),
                .end => return try translator.translateEnd(),
                // All other operators are skipped when unreachable
                else => return,
            }
        }

        // Normal (reachable) operator translation
        switch (op) {
            // Control flow - block structure
            .block => |data| try translator.translateBlock(data.params, data.results),
            .loop => |data| try translator.translateLoop(data.params, data.results),
            .if_op => |data| try translator.translateIf(data.params, data.results),
            .else_op => try translator.translateElse(),
            .end => try translator.translateEnd(),
            .nop => {},

            // Control flow - branching (sets reachable=false)
            .br => |depth| try translator.translateBr(depth),
            .br_if => |depth| try translator.translateBrIf(depth),
            .br_table => |data| try translator.translateBrTable(data.targets, data.default),
            .return_op => try translator.translateReturn(),
            .unreachable_op => try translator.translateUnreachable(),

            // Variables
            .local_get => |idx| try translator.translateLocalGet(idx),
            .local_set => |idx| try translator.translateLocalSet(idx),
            .local_tee => |idx| try translator.translateLocalTee(idx),
            .global_get => |idx| try translator.translateGlobalGet(idx),
            .global_set => |idx| try translator.translateGlobalSet(idx),

            // Constants
            .i32_const => |val| try translator.translateI32Const(val),
            .i64_const => |val| try translator.translateI64Const(val),

            // Arithmetic (unified i32/i64 - CLIF instructions are type-agnostic)
            // Port of code_translator.rs:1193-1268
            .i32_add, .i64_add => try translator.translateIAdd(),
            .i32_sub, .i64_sub => try translator.translateISub(),
            .i32_mul, .i64_mul => try translator.translateIMul(),
            .i32_div_s, .i64_div_s => try translator.translateSDivWithGuard(),
            .i32_div_u, .i64_div_u => try translator.translateUDivWithGuard(),
            .i32_rem_s, .i64_rem_s => try translator.translateSRemWithGuard(),
            .i32_rem_u, .i64_rem_u => try translator.translateURemWithGuard(),
            .i32_and, .i64_and => try translator.translateBAnd(),
            .i32_or, .i64_or => try translator.translateBOr(),
            .i32_xor, .i64_xor => try translator.translateBXor(),
            .i32_shl, .i64_shl => try translator.translateIShl(),
            .i32_shr_s, .i64_shr_s => try translator.translateSShr(),
            .i32_shr_u, .i64_shr_u => try translator.translateUShr(),
            .i32_rotl, .i64_rotl => try translator.translateRotl(),
            .i32_rotr, .i64_rotr => try translator.translateRotr(),

            // Comparison (unified i32/i64)
            // Port of code_translator.rs:1286-1318
            .i32_eqz, .i64_eqz => try translator.translateIEqz(),
            .i32_eq, .i64_eq => try translator.translateIEq(),
            .i32_ne, .i64_ne => try translator.translateINe(),
            .i32_lt_s, .i64_lt_s => try translator.translateILtS(),
            .i32_lt_u, .i64_lt_u => try translator.translateILtU(),
            .i32_gt_s, .i64_gt_s => try translator.translateIGtS(),
            .i32_gt_u, .i64_gt_u => try translator.translateIGtU(),
            .i32_le_s, .i64_le_s => try translator.translateILeS(),
            .i32_le_u, .i64_le_u => try translator.translateILeU(),
            .i32_ge_s, .i64_ge_s => try translator.translateIGeS(),
            .i32_ge_u, .i64_ge_u => try translator.translateIGeU(),

            // Conversions
            .i32_wrap_i64 => try translator.translateI32WrapI64(),
            .i64_extend_i32_s => try translator.translateI64ExtendI32S(),
            .i64_extend_i32_u => try translator.translateI64ExtendI32U(),

            // Parametric
            .drop => try translator.translateDrop(),
            .select => try translator.translateSelect(),

            // Memory loads
            // Port of code_translator.rs:818-902
            .i32_load => |m| try translator.translateLoad(m, Type.I32),
            .i64_load => |m| try translator.translateLoad(m, Type.I64),
            .f32_load => |m| try translator.translateLoad(m, Type.F32),
            .f64_load => |m| try translator.translateLoad(m, Type.F64),
            .i32_load8_s => |m| try translator.translateSLoad(m, 1, Type.I32),
            .i32_load8_u => |m| try translator.translateULoad(m, 1, Type.I32),
            .i32_load16_s => |m| try translator.translateSLoad(m, 2, Type.I32),
            .i32_load16_u => |m| try translator.translateULoad(m, 2, Type.I32),
            .i64_load8_s => |m| try translator.translateSLoad(m, 1, Type.I64),
            .i64_load8_u => |m| try translator.translateULoad(m, 1, Type.I64),
            .i64_load16_s => |m| try translator.translateSLoad(m, 2, Type.I64),
            .i64_load16_u => |m| try translator.translateULoad(m, 2, Type.I64),
            .i64_load32_s => |m| try translator.translateSLoad(m, 4, Type.I64),
            .i64_load32_u => |m| try translator.translateULoad(m, 4, Type.I64),

            // Memory stores
            // Port of code_translator.rs:962-977
            .i32_store => |m| try translator.translateStore(m, Type.I32),
            .i64_store => |m| try translator.translateStore(m, Type.I64),
            .f32_store => |m| try translator.translateStore(m, Type.F32),
            .f64_store => |m| try translator.translateStore(m, Type.F64),
            .i32_store8 => |m| try translator.translateTruncStore(m, 1),
            .i32_store16 => |m| try translator.translateTruncStore(m, 2),
            .i64_store8 => |m| try translator.translateTruncStore(m, 1),
            .i64_store16 => |m| try translator.translateTruncStore(m, 2),
            .i64_store32 => |m| try translator.translateTruncStore(m, 4),

            // Calls
            // Port of code_translator.rs:654-717
            .call => |idx| try translator.translateCall(idx),
            .call_indirect => |data| try translator.translateCallIndirect(data.type_index, data.table_index),

            // Float constants
            .f32_const => |val| try translator.translateF32Const(val),
            .f64_const => |val| try translator.translateF64Const(val),

            // Float arithmetic (binary)
            .f32_add, .f64_add => try translator.translateFAdd(),
            .f32_sub, .f64_sub => try translator.translateFSub(),
            .f32_mul, .f64_mul => try translator.translateFMul(),
            .f32_div, .f64_div => try translator.translateFDiv(),
            .f32_min, .f64_min => try translator.translateFMin(),
            .f32_max, .f64_max => try translator.translateFMax(),
            .f32_copysign, .f64_copysign => try translator.translateFCopysign(),

            // Float arithmetic (unary)
            .f32_abs, .f64_abs => try translator.translateFAbs(),
            .f32_neg, .f64_neg => try translator.translateFNeg(),
            .f32_ceil, .f64_ceil => try translator.translateFCeil(),
            .f32_floor, .f64_floor => try translator.translateFFloor(),
            .f32_trunc, .f64_trunc => try translator.translateFTrunc(),
            .f32_nearest, .f64_nearest => try translator.translateFNearest(),
            .f32_sqrt, .f64_sqrt => try translator.translateFSqrt(),

            // Float comparisons
            .f32_eq, .f64_eq => try translator.translateFEq(),
            .f32_ne, .f64_ne => try translator.translateFNe(),
            .f32_lt, .f64_lt => try translator.translateFLt(),
            .f32_gt, .f64_gt => try translator.translateFGt(),
            .f32_le, .f64_le => try translator.translateFLe(),
            .f32_ge, .f64_ge => try translator.translateFGe(),

            // Float truncation to int
            .i32_trunc_f32_s, .i32_trunc_f64_s => try translator.translateTruncFS(Type.I32),
            .i32_trunc_f32_u, .i32_trunc_f64_u => try translator.translateTruncFU(Type.I32),
            .i64_trunc_f32_s, .i64_trunc_f64_s => try translator.translateTruncFS(Type.I64),
            .i64_trunc_f32_u, .i64_trunc_f64_u => try translator.translateTruncFU(Type.I64),

            // Int to float conversion
            .f32_convert_i32_s, .f32_convert_i64_s => try translator.translateConvertFS(Type.F32),
            .f32_convert_i32_u, .f32_convert_i64_u => try translator.translateConvertFU(Type.F32),
            .f64_convert_i32_s, .f64_convert_i64_s => try translator.translateConvertFS(Type.F64),
            .f64_convert_i32_u, .f64_convert_i64_u => try translator.translateConvertFU(Type.F64),

            // Float promotion/demotion
            .f32_demote_f64 => try translator.translateF32DemoteF64(),
            .f64_promote_f32 => try translator.translateF64PromoteF32(),

            // Reinterpret
            .i32_reinterpret_f32 => try translator.translateReinterpret(Type.I32, Type.F32),
            .i64_reinterpret_f64 => try translator.translateReinterpret(Type.I64, Type.F64),
            .f32_reinterpret_i32 => try translator.translateReinterpret(Type.F32, Type.I32),
            .f64_reinterpret_i64 => try translator.translateReinterpret(Type.F64, Type.I64),

            // Memory size and grow
            .memory_size => |mem_idx| try translator.translateMemorySize(mem_idx),
            .memory_grow => |mem_idx| try translator.translateMemoryGrow(mem_idx),
        }
    }
};

// ============================================================================
// WasmOperator - Tagged union for operators
// ============================================================================

pub const BlockData = struct {
    params: usize,
    results: usize,
};

/// Data for br_table instruction
pub const BrTableData = struct {
    targets: []const u32,
    default: u32,
};

pub const WasmOperator = union(enum) {
    // Control flow
    block: BlockData,
    loop: BlockData,
    if_op: BlockData,
    else_op,
    end,
    br: u32,
    br_if: u32,
    br_table: BrTableData,
    return_op,
    unreachable_op,
    nop,

    // Variables
    local_get: u32,
    local_set: u32,
    local_tee: u32,
    global_get: u32,
    global_set: u32,

    // Constants
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,

    // Arithmetic i32/i64 (unified - CLIF instructions are type-agnostic)
    // Port of code_translator.rs:1193-1268
    i32_add,
    i64_add,
    i32_sub,
    i64_sub,
    i32_mul,
    i64_mul,
    i32_div_s,
    i64_div_s,
    i32_div_u,
    i64_div_u,
    i32_rem_s,
    i64_rem_s,
    i32_rem_u,
    i64_rem_u,
    i32_and,
    i64_and,
    i32_or,
    i64_or,
    i32_xor,
    i64_xor,
    i32_shl,
    i64_shl,
    i32_shr_s,
    i64_shr_s,
    i32_shr_u,
    i64_shr_u,
    i32_rotl,
    i64_rotl,
    i32_rotr,
    i64_rotr,

    // Comparison i32/i64 (unified)
    // Port of code_translator.rs:1286-1318
    i32_eqz,
    i64_eqz,
    i32_eq,
    i64_eq,
    i32_ne,
    i64_ne,
    i32_lt_s,
    i64_lt_s,
    i32_lt_u,
    i64_lt_u,
    i32_gt_s,
    i64_gt_s,
    i32_gt_u,
    i64_gt_u,
    i32_le_s,
    i64_le_s,
    i32_le_u,
    i64_le_u,
    i32_ge_s,
    i64_ge_s,
    i32_ge_u,
    i64_ge_u,

    // Arithmetic f32
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,
    f32_copysign,

    // Arithmetic f64
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,
    f64_copysign,

    // Comparison f32
    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,

    // Comparison f64
    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,

    // Conversions - integer
    i32_wrap_i64,
    i64_extend_i32_s,
    i64_extend_i32_u,

    // Conversions - float truncation
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,

    // Conversions - float promotion/demotion
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f32_demote_f64,
    f64_promote_f32,

    // Reinterpret
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,

    // Memory operations
    memory_size: u32,
    memory_grow: u32,

    // Parametric
    drop,
    select,

    // Memory (loads)
    // Port of code_translator.rs:818-902
    i32_load: MemArg,
    i64_load: MemArg,
    f32_load: MemArg,
    f64_load: MemArg,
    i32_load8_s: MemArg,
    i32_load8_u: MemArg,
    i32_load16_s: MemArg,
    i32_load16_u: MemArg,
    i64_load8_s: MemArg,
    i64_load8_u: MemArg,
    i64_load16_s: MemArg,
    i64_load16_u: MemArg,
    i64_load32_s: MemArg,
    i64_load32_u: MemArg,

    // Memory (stores)
    // Port of code_translator.rs:962-977
    i32_store: MemArg,
    i64_store: MemArg,
    f32_store: MemArg,
    f64_store: MemArg,
    i32_store8: MemArg,
    i32_store16: MemArg,
    i64_store8: MemArg,
    i64_store16: MemArg,
    i64_store32: MemArg,

    // Calls
    // Port of code_translator.rs:654-717
    call: u32, // function_index
    call_indirect: struct {
        type_index: u32,
        table_index: u32,
    },
};

// ============================================================================
// Tests
// ============================================================================

test "translate simple function: (i32, i32) -> i32 { local.get 0 + local.get 1 }" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{ .i32, .i32 },
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .local_get = 0 },
        .{ .local_get = 1 },
        .i32_add,
        .end,
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Should have created blocks in the function
    try testing.expect(func.layout.entryBlock() != null);
}

test "translate function with local: () -> i32 { local i32; local.set 0 = 42; local.get 0 }" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{},
        .results = &[_]WasmValType{.i32},
    };

    const locals = [_]LocalDecl{
        .{ .count = 1, .val_type = .i32 },
    };

    const operators = [_]WasmOperator{
        .{ .i32_const = 42 },
        .{ .local_set = 0 },
        .{ .local_get = 0 },
        .end,
    };

    try ft.translateFunction(&func, signature, &locals, &operators);

    // Should have created blocks in the function
    try testing.expect(func.layout.entryBlock() != null);
}

test "translate function with block" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .block = .{ .params = 0, .results = 1 } },
        .{ .i32_const = 42 },
        .end, // end block
        .end, // end function
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Should complete without error
    try testing.expect(func.layout.entryBlock() != null);
}

test "translate function with loop" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{},
        .results = &[_]WasmValType{},
    };

    const operators = [_]WasmOperator{
        .{ .loop = .{ .params = 0, .results = 0 } },
        .{ .br = 0 }, // branch back to loop header
        .end, // end loop
        .end, // end function
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Should have created multiple blocks (entry, loop header, exit)
    try testing.expect(func.layout.entryBlock() != null);
}

test "translate function with if-else" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{.i32},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .local_get = 0 },
        .{ .if_op = .{ .params = 0, .results = 1 } },
        .{ .i32_const = 1 }, // then branch
        .else_op,
        .{ .i32_const = 0 }, // else branch
        .end, // end if
        .end, // end function
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Should have created blocks for if/else structure
    try testing.expect(func.layout.entryBlock() != null);
}

// ============================================================================
// V1: E2E Layout Population Tests
// These tests verify the F1 fix - that all blocks are properly added to Layout
// ============================================================================

test "V1: Layout populated for simple return function" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    // Simple function: () -> i32 { return 42 }
    const signature = FuncSignature{
        .params = &[_]WasmValType{},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .i32_const = 42 },
        .end,
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Critical verification: Layout must have entry block
    const entry = func.layout.entryBlock();
    try testing.expect(entry != null);

    // Count blocks in Layout
    var layout_count: usize = 0;
    var layout_iter = func.layout.blocks();
    while (layout_iter.next()) |_| layout_count += 1;

    // Must have at least entry block in Layout
    try testing.expect(layout_count >= 1);

    // DFG block count should match or exceed Layout (exit block may not be in Layout if unreachable)
    try testing.expect(func.dfg.blocks.items.len >= layout_count);
}

test "V1: Layout populated for function with no locals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    // Function with params but no locals: (i32) -> i32 { return param }
    const signature = FuncSignature{
        .params = &[_]WasmValType{.i32},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .local_get = 0 },
        .end,
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // This specifically tests F1 fix - entry block must be in Layout
    // even when there are no local variable initializations
    const entry = func.layout.entryBlock();
    try testing.expect(entry != null);
}

test "V1: Layout consistent for branching function" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    // Function with br_if: (i32) -> i32 { if param { return 1 } else { return 0 } }
    // Uses a block with results=0, then pushes result after the block
    const signature = FuncSignature{
        .params = &[_]WasmValType{.i32},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        // block (results=0) - just for control flow
        .{ .block = .{ .params = 0, .results = 0 } },
        .{ .local_get = 0 }, // condition
        .{ .br_if = 0 }, // branch out if param is true (no values needed)
        .{ .i32_const = 0 }, // false case: return 0
        .{ .br = 1 }, // skip to function end
        .end,
        // After block: true case
        .{ .i32_const = 1 }, // true case: return 1
        .end,
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Verify entry block exists
    try testing.expect(func.layout.entryBlock() != null);

    // Count Layout blocks - should have multiple due to branching
    var layout_count: usize = 0;
    var layout_iter = func.layout.blocks();
    while (layout_iter.next()) |_| layout_count += 1;

    // br_if creates additional blocks
    try testing.expect(layout_count >= 2);
}

test "V1: Compare Layout vs DFG block counts" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ft = WasmFuncTranslator.initWithoutGlobals(allocator);
    defer ft.deinit();

    const signature = FuncSignature{
        .params = &[_]WasmValType{},
        .results = &[_]WasmValType{.i32},
    };

    const operators = [_]WasmOperator{
        .{ .i32_const = 42 },
        .end,
    };

    try ft.translateFunction(&func, signature, &[_]LocalDecl{}, &operators);

    // Use the D3 comparison utility
    var comparison = func.compareLayoutToDfg();
    defer comparison.deinit(allocator);

    // Entry block must exist
    try testing.expect(comparison.entry_block != null);

    // Layout should have at least one block
    try testing.expect(comparison.layout_block_count >= 1);

    // Log discrepancies (for debugging, doesn't fail test)
    // Unreachable blocks (like exit block) may be in DFG but not Layout - that's OK
}
