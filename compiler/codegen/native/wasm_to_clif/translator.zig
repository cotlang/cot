//! Wasm to CLIF code translator.
//!
//! Port of wasmtime/crates/cranelift/src/translate/code_translator.rs
//!
//! This module translates WebAssembly operators into CLIF IR instructions.
//! The translation is done in one pass, opcode by opcode. Two main data structures
//! are used: the value stack and the control stack.

const std = @import("std");
const stack_mod = @import("stack.zig");
const frontend_mod = @import("../frontend/mod.zig");
const func_environ_mod = @import("func_environ.zig");
const bounds_checks = @import("bounds_checks.zig");
const heap_mod = @import("heap.zig");
const debug = @import("../../../pipeline_debug.zig");

// Re-export CLIF types from stack module
pub const clif = stack_mod.clif;
pub const Block = stack_mod.Block;
pub const Value = stack_mod.Value;
pub const Inst = stack_mod.Inst;
pub const TranslationState = stack_mod.TranslationState;
pub const ControlStackFrame = stack_mod.ControlStackFrame;
pub const ElseData = stack_mod.ElseData;

// Frontend types
pub const FunctionBuilder = frontend_mod.FunctionBuilder;
pub const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
pub const Variable = frontend_mod.Variable;
pub const Type = frontend_mod.Type;
pub const Function = frontend_mod.Function;
pub const GlobalValue = frontend_mod.GlobalValue;

// FuncEnvironment types
pub const FuncEnvironment = func_environ_mod.FuncEnvironment;
pub const GlobalVariable = func_environ_mod.GlobalVariable;
pub const ConstantValue = func_environ_mod.ConstantValue;

// Heap/memory types
pub const MemArg = heap_mod.MemArg;
pub const HeapData = heap_mod.HeapData;

// ============================================================================
// Wasm Opcode
// Subset of Wasm opcodes we translate
// ============================================================================

pub const WasmOpcode = enum(u8) {
    // Control flow
    unreachable_op = 0x00,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    if_op = 0x04,
    else_op = 0x05,
    end = 0x0B,
    br = 0x0C,
    br_if = 0x0D,
    br_table = 0x0E,
    return_op = 0x0F,
    call = 0x10,
    call_indirect = 0x11,

    // Parametric
    drop = 0x1A,
    select = 0x1B,

    // Variable
    local_get = 0x20,
    local_set = 0x21,
    local_tee = 0x22,
    global_get = 0x23,
    global_set = 0x24,

    // Memory
    i32_load = 0x28,
    i64_load = 0x29,
    f32_load = 0x2A,
    f64_load = 0x2B,
    i32_load8_s = 0x2C,
    i32_load8_u = 0x2D,
    i32_load16_s = 0x2E,
    i32_load16_u = 0x2F,
    i64_load8_s = 0x30,
    i64_load8_u = 0x31,
    i64_load16_s = 0x32,
    i64_load16_u = 0x33,
    i64_load32_s = 0x34,
    i64_load32_u = 0x35,
    i32_store = 0x36,
    i64_store = 0x37,
    f32_store = 0x38,
    f64_store = 0x39,
    i32_store8 = 0x3A,
    i32_store16 = 0x3B,
    i64_store8 = 0x3C,
    i64_store16 = 0x3D,
    i64_store32 = 0x3E,

    // Constants
    i32_const = 0x41,
    i64_const = 0x42,
    f32_const = 0x43,
    f64_const = 0x44,

    // Comparison
    i32_eqz = 0x45,
    i32_eq = 0x46,
    i32_ne = 0x47,
    i32_lt_s = 0x48,
    i32_lt_u = 0x49,
    i32_gt_s = 0x4A,
    i32_gt_u = 0x4B,
    i32_le_s = 0x4C,
    i32_le_u = 0x4D,
    i32_ge_s = 0x4E,
    i32_ge_u = 0x4F,
    i64_eqz = 0x50,
    i64_eq = 0x51,
    i64_ne = 0x52,
    i64_lt_s = 0x53,
    i64_lt_u = 0x54,
    i64_gt_s = 0x55,
    i64_gt_u = 0x56,
    i64_le_s = 0x57,
    i64_le_u = 0x58,
    i64_ge_s = 0x59,
    i64_ge_u = 0x5A,

    // Numeric i32
    i32_clz = 0x67,
    i32_ctz = 0x68,
    i32_popcnt = 0x69,
    i32_add = 0x6A,
    i32_sub = 0x6B,
    i32_mul = 0x6C,
    i32_div_s = 0x6D,
    i32_div_u = 0x6E,
    i32_rem_s = 0x6F,
    i32_rem_u = 0x70,
    i32_and = 0x71,
    i32_or = 0x72,
    i32_xor = 0x73,
    i32_shl = 0x74,
    i32_shr_s = 0x75,
    i32_shr_u = 0x76,
    i32_rotl = 0x77,
    i32_rotr = 0x78,

    // Numeric i64
    i64_clz = 0x79,
    i64_ctz = 0x7A,
    i64_popcnt = 0x7B,
    i64_add = 0x7C,
    i64_sub = 0x7D,
    i64_mul = 0x7E,
    i64_div_s = 0x7F,
    i64_div_u = 0x80,
    i64_rem_s = 0x81,
    i64_rem_u = 0x82,
    i64_and = 0x83,
    i64_or = 0x84,
    i64_xor = 0x85,
    i64_shl = 0x86,
    i64_shr_s = 0x87,
    i64_shr_u = 0x88,
    i64_rotl = 0x89,
    i64_rotr = 0x8A,

    // Conversions
    i32_wrap_i64 = 0xA7,
    i64_extend_i32_s = 0xAC,
    i64_extend_i32_u = 0xAD,
};

// ============================================================================
// Global Type Information
// Used to track types of module-level globals
// ============================================================================

/// Wasm global value type (matches wasm_parser.GlobalType simplified)
pub const WasmGlobalType = struct {
    val_type: WasmValType,
    mutable: bool,
};

pub const WasmValType = enum {
    i32,
    i64,
    f32,
    f64,

    pub fn toClifType(self: WasmValType) Type {
        return switch (self) {
            .i32 => Type.I32,
            .i64 => Type.I64,
            .f32 => Type.F32,
            .f64 => Type.F64,
        };
    }
};

/// Wasm function type (matches wasm_parser.FuncType simplified)
pub const WasmFuncType = struct {
    params: []const WasmValType,
    results: []const WasmValType,
};

// ============================================================================
// FuncTranslator
// Port of wasmtime func_translator.rs
//
// Translates Wasm operators to CLIF IR using FunctionBuilder.
// ============================================================================

pub const FuncTranslator = struct {
    /// Translation state (value stack, control stack, reachability).
    state: TranslationState,
    /// The function builder.
    builder: *FunctionBuilder,
    /// Local variable mappings (Wasm local index -> Variable).
    locals: std.ArrayListUnmanaged(Variable),
    /// Module-level globals (for type lookup).
    globals: []const WasmGlobalType,
    /// Module-level function types (for indirect call signature lookup).
    func_types: []const WasmFuncType,
    /// Function environment for global variable management.
    /// Port of Cranelift's FuncEnvironment.
    env: FuncEnvironment,
    /// Allocator for dynamic storage.
    allocator: std.mem.Allocator,
    /// Table elements from element section: table_index -> function_index.
    /// Used for AOT call_indirect resolution.
    table_elements: []const u32,

    const Self = @This();

    /// Create a new translator with module-level information.
    /// Port of Cranelift's FuncTranslator with module access.
    pub fn init(
        allocator: std.mem.Allocator,
        builder: *FunctionBuilder,
        globals: []const WasmGlobalType,
        func_types: []const WasmFuncType,
        func_to_type: []const u32,
        table_elements: []const u32,
    ) Self {
        // Convert func_types to FuncEnvironment format
        // Note: func_environ_mod.WasmFuncType uses the same structure
        const env_func_types = @as([]const func_environ_mod.WasmFuncType, @ptrCast(func_types));
        return .{
            .state = TranslationState.init(allocator),
            .builder = builder,
            .locals = .{},
            .globals = globals,
            .func_types = func_types,
            .env = FuncEnvironment.initWithTypes(allocator, func_to_type, env_func_types),
            .allocator = allocator,
            .table_elements = table_elements,
        };
    }

    /// Create a new translator without module info (backwards compatibility).
    pub fn initWithoutGlobals(allocator: std.mem.Allocator, builder: *FunctionBuilder) Self {
        return init(allocator, builder, &[_]WasmGlobalType{}, &[_]WasmFuncType{}, &[_]u32{}, &[_]u32{});
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self) void {
        self.state.deinit();
        self.locals.deinit(self.allocator);
        self.env.deinit();
    }

    /// Initialize for translating a function.
    ///
    /// Sets up the entry block, exit block, and local variables.
    /// F1 Fix: Explicitly ensures entry block is in Layout.
    pub fn initializeFunction(self: *Self, num_params: u32, num_locals: u32, param_types: []const Type, local_types: []const Type, num_returns: usize) !void {
        // Create entry and exit blocks
        const entry_block = try self.builder.createBlock();
        const exit_block = try self.builder.createBlock();

        // Add block params to exit block for function returns.
        // Reference: wasmtime/crates/cranelift/src/translate/func_translator.rs:86-87
        //   let exit_block = builder.create_block();
        //   builder.append_block_params_for_function_returns(exit_block);
        try self.builder.appendBlockParamsForFunctionReturns(exit_block);

        // Switch to entry block
        self.builder.switchToBlock(entry_block);

        // Add function parameters to entry block
        // Note: This must be done while block is still pristine
        try self.builder.appendBlockParamsForFunctionParams(entry_block);

        // F1 CRITICAL FIX: Explicitly ensure entry block is in Layout
        // Cranelift adds blocks to Layout when first instruction is inserted via
        // ensureInsertedBlock(). However, if num_locals==0 and the first Wasm
        // instruction doesn't trigger instruction insertion, the entry block
        // may never be added to Layout. This explicit call guarantees it.
        // Reference: cranelift-frontend/src/frontend.rs ensure_inserted_block()
        try self.builder.ensureInsertedBlock();

        // Seal entry block (no predecessors except function entry)
        try self.builder.sealBlock(entry_block);

        // Initialize translation state with exit block
        try self.state.initialize(exit_block, num_returns);

        // Declare variables for all Wasm locals (params + locals)
        const total_locals = num_params + num_locals;
        self.locals.clearRetainingCapacity();
        try self.locals.ensureTotalCapacity(self.allocator, total_locals);

        // Declare variables for parameters and define them with block params
        // Note: Entry block params include vmctx params first:
        //   [callee_vmctx, caller_vmctx, wasm_param0, wasm_param1, ...]
        // So Wasm params start at index 2.
        const entry_params = self.builder.blockParams(entry_block);
        const vmctx_param_count: usize = 2; // callee_vmctx and caller_vmctx
        for (0..num_params) |i| {
            const var_type = param_types[i];
            const variable = try self.builder.declareVar(var_type);
            self.locals.appendAssumeCapacity(variable);

            // Define parameter variable with block param value (offset by vmctx params)
            try self.builder.defVar(variable, entry_params[vmctx_param_count + i]);
        }

        // Declare variables for non-parameter locals (initialized to zero)
        for (0..num_locals) |i| {
            const var_type = local_types[i];
            const variable = try self.builder.declareVar(var_type);
            self.locals.appendAssumeCapacity(variable);

            // Initialize local to zero (Cranelift: func_translator.rs:174-195)
            const zero = if (var_type.isFloat())
                if (var_type.eql(Type.F32))
                    try self.builder.ins().f32const(0.0)
                else
                    try self.builder.ins().f64const(0.0)
            else
                try self.builder.ins().iconst(var_type, 0);
            try self.builder.defVar(variable, zero);
        }
    }

    /// Initialize function with simple signature (all I32).
    pub fn initializeFunctionSimple(self: *Self, num_params: u32, num_locals: u32, num_returns: usize) !void {
        // Build type arrays
        var param_types = try self.allocator.alloc(Type, num_params);
        defer self.allocator.free(param_types);
        for (0..num_params) |i| {
            param_types[i] = Type.I32;
        }

        var local_types = try self.allocator.alloc(Type, num_locals);
        defer self.allocator.free(local_types);
        for (0..num_locals) |i| {
            local_types[i] = Type.I32;
        }

        try self.initializeFunction(num_params, num_locals, param_types, local_types, num_returns);
    }

    // ========================================================================
    // Control Flow Translation
    // ========================================================================

    /// Translate a block instruction.
    pub fn translateBlock(self: *Self, num_params: usize, num_results: usize) !void {
        const next = try self.builder.createBlock();

        // Add block params for results
        for (0..num_results) |_| {
            _ = try self.builder.appendBlockParam(next, Type.I32);
        }

        try self.state.pushBlock(next, num_params, num_results);
    }

    /// Handle block/loop when unreachable (Cranelift: translate_unreachable_operator).
    /// Pushes a placeholder control stack entry without emitting code.
    pub fn translateUnreachableBlock(self: *Self, num_params: usize, num_results: usize) !void {
        // Create a dummy block for the control stack
        const placeholder = try self.builder.createBlock();
        try self.state.pushBlock(placeholder, num_params, num_results);
    }

    /// Handle if when unreachable (Cranelift: translate_unreachable_operator).
    /// Pushes a placeholder if frame without emitting code or popping condition.
    pub fn translateUnreachableIf(self: *Self, num_params: usize, num_results: usize) !void {
        // Create placeholder blocks
        const placeholder = try self.builder.createBlock();
        // Push a placeholder if frame - no actual branches are emitted
        const else_data = ElseData{ .no_else = .{
            .branch_inst = Inst.RESERVED,
            .placeholder = placeholder,
        } };
        try self.state.pushIf(placeholder, else_data, num_params, num_results);
    }

    /// Translate a loop instruction.
    pub fn translateLoop(self: *Self, num_params: usize, num_results: usize) !void {
        const loop_body = try self.builder.createBlock();
        const next = try self.builder.createBlock();

        // Loop body block gets params
        for (0..num_params) |_| {
            _ = try self.builder.appendBlockParam(loop_body, Type.I32);
        }

        // Get current params from stack
        const params = self.state.peekn(num_params);

        // Jump to loop header with current params
        _ = try self.builder.ins().jump(loop_body, params);

        try self.state.pushLoop(loop_body, next, num_params, num_results);

        // Switch to loop body
        // NOTE: Do NOT seal the loop header here! It must remain unsealed until
        // the End instruction so that back-edges (br to loop header) can add
        // themselves as predecessors. This follows Cranelift's pattern from
        // code_translator.rs lines 430-431.
        self.builder.switchToBlock(loop_body);

        // F1 Fix: Ensure loop body block is in Layout after switching
        try self.builder.ensureInsertedBlock();

        // Pop old values and push block params
        self.state.popn(num_params);
        const loop_params = self.builder.blockParams(loop_body);
        for (loop_params) |param| {
            try self.state.push1(param);
        }
    }

    /// Translate an if instruction.
    pub fn translateIf(self: *Self, num_params: usize, num_results: usize) !void {
        const condition = self.state.pop1();

        const then_block = try self.builder.createBlock();
        const destination = try self.builder.createBlock();

        // Add params for then_block
        for (0..num_params) |_| {
            _ = try self.builder.appendBlockParam(then_block, Type.I32);
        }

        // Add params for destination block (results)
        for (0..num_results) |_| {
            _ = try self.builder.appendBlockParam(destination, Type.I32);
        }

        // Get current params from stack
        const params = self.state.peekn(num_params);

        // Emit conditional branch
        const else_data = if (num_params == num_results) blk: {
            // May not have else, branch to destination if false
            const inst = try self.builder.ins().brif(condition, then_block, params, destination, params);
            break :blk ElseData{ .no_else = .{
                .branch_inst = inst,
                .placeholder = destination,
            } };
        } else blk: {
            // Must have an else, pre-allocate it
            const else_block = try self.builder.createBlock();
            for (0..num_params) |_| {
                _ = try self.builder.appendBlockParam(else_block, Type.I32);
            }
            _ = try self.builder.ins().brif(condition, then_block, params, else_block, params);
            break :blk ElseData{ .with_else = .{ .else_block = else_block } };
        };

        try self.state.pushIf(destination, else_data, num_params, num_results);

        // Switch to then block
        self.builder.switchToBlock(then_block);
        try self.builder.sealBlock(then_block);

        // F1 Fix: Ensure then block is in Layout after switching
        try self.builder.ensureInsertedBlock();

        // Pop old values and push block params
        self.state.popn(num_params);
        const then_params = self.builder.blockParams(then_block);
        for (then_params) |param| {
            try self.state.push1(param);
        }
    }

    /// Translate an else instruction.
    pub fn translateElse(self: *Self) !void {
        const frame = self.state.getFrameMut(0);
        switch (frame.*) {
            .if_frame => |*f| {
                // Record that consequent is ending
                f.consequent_ends_reachable = self.state.reachable;

                if (self.state.reachable) {
                    // Get return values from stack
                    const return_vals = self.state.peekn(f.num_return_values);

                    // Jump to destination with return values
                    _ = try self.builder.ins().jump(f.destination, return_vals);
                }

                // Get else block
                // Cranelift: code_translator.rs translate_else()
                // When else_data is no_else, we didn't expect an else but got one.
                // Must create a new else block and redirect the brif's false branch
                // away from the destination (placeholder). Without this, the
                // destination gets sealed here but translateEnd still needs to
                // add a predecessor to it → assertion failure.
                const else_block = switch (f.else_data) {
                    .no_else => |ne| blk: {
                        const new_else = try self.builder.createBlock();
                        for (0..f.num_param_values) |_| {
                            _ = try self.builder.appendBlockParam(new_else, Type.I32);
                        }
                        try self.builder.changeJumpDestination(ne.branch_inst, ne.placeholder, new_else);
                        break :blk new_else;
                    },
                    .with_else => |we| we.else_block,
                };

                // Truncate stack to else params
                frame.truncateValueStackToElseParams(&self.state.stack);

                // Switch to else block
                self.builder.switchToBlock(else_block);
                try self.builder.sealBlock(else_block);

                // F1 Fix: Ensure else block is in Layout after switching
                try self.builder.ensureInsertedBlock();

                // Push block params
                const else_params = self.builder.blockParams(else_block);
                for (else_params) |param| {
                    try self.state.push1(param);
                }

                // Restore reachability
                if (f.head_is_reachable) {
                    self.state.reachable = true;
                }
            },
            else => unreachable,
        }
    }

    /// Translate an end instruction.
    pub fn translateEnd(self: *Self) !void {
        const frame = self.state.popFrame();
        const next_block = frame.followingCode();

        // Cranelift has TWO separate End handlers:
        // 1. Reachable case (code_translator.rs:412-444): UNCONDITIONALLY emit jump, switch, seal
        // 2. Unreachable case (code_translator.rs:3389-3437): Conditionally switch based on reachability
        //
        // We MUST split these cases to match Cranelift's behavior exactly.

        if (self.state.reachable) {
            // REACHABLE CASE (Cranelift lines 412-444)
            // When entering with reachable=true, we ALWAYS:
            // 1. Emit jump to next_block with return values
            // 2. Switch to next_block
            // 3. Seal next_block
            // 4. Push block params

            const return_count = frame.numReturnValues();
            const return_vals = self.state.peekn(return_count);

            // Emit jump to next block with return values
            _ = try self.builder.ins().jump(next_block, return_vals);

            // Switch to next block - UNCONDITIONAL in reachable case
            self.builder.switchToBlock(next_block);
            try self.builder.sealBlock(next_block);

            // If it is a loop we also have to seal the body loop block (Cranelift line 430-432)
            if (frame == .loop_frame) {
                try self.builder.sealBlock(frame.loop_frame.header);
            }

            // Truncate stack to original size
            frame.truncateValueStackToOriginalSize(&self.state.stack);

            // Ensure next block is in Layout after switching
            try self.builder.ensureInsertedBlock();

            // Push block params onto stack (Cranelift line 443)
            const next_params = self.builder.blockParams(next_block);
            for (next_params) |param| {
                try self.state.push1(param);
            }
            // state.reachable remains true
        } else {
            // UNREACHABLE CASE (Cranelift lines 3389-3437)
            // When entering with reachable=false, we:
            // 1. Truncate stack first (no return values to emit)
            // 2. Calculate reachable_anyway
            // 3. Conditionally switch/seal/push based on exit_is_branched_to || reachable_anyway

            // Truncate stack to original size FIRST (Cranelift line 3398)
            frame.truncateValueStackToOriginalSize(&self.state.stack);

            // Calculate reachable_anyway (Cranelift lines 3400-3427)
            const reachable_anyway = switch (frame) {
                .loop_frame => |f| blk: {
                    // For loops, seal the header block now (Cranelift line 3403)
                    try self.builder.sealBlock(f.header);
                    // Loops can't have branches to the end (Cranelift line 3405)
                    break :blk false;
                },
                .if_frame => |f| blk: {
                    if (f.consequent_ends_reachable) |conseq_reachable| {
                        // We have an else, and since we're unreachable, the alternative just
                        // ended unreachable. Check if consequent ended reachable. (Cranelift 3420-3424)
                        break :blk f.head_is_reachable and conseq_reachable;
                    } else {
                        // No else: we're finishing the consequent now.
                        // Reachable if head was reachable. (Cranelift lines 3407-3415)
                        break :blk f.head_is_reachable;
                    }
                },
                .block_frame => false, // Cranelift line 3426
            };

            // Conditionally switch to next block (Cranelift lines 3429-3436)
            if (frame.exitIsBranchedTo() or reachable_anyway) {
                self.builder.switchToBlock(next_block);
                try self.builder.sealBlock(next_block);

                // Ensure next block is in Layout after switching
                try self.builder.ensureInsertedBlock();

                // Push block params onto stack
                const next_params = self.builder.blockParams(next_block);
                for (next_params) |param| {
                    try self.state.push1(param);
                }
                self.state.reachable = true;
            }
            // If neither condition is met, state.reachable remains false
        }
    }

    /// Translate a br instruction.
    pub fn translateBr(self: *Self, relative_depth: u32) !void {
        const frame = self.state.getFrameMut(relative_depth);
        frame.setBranchedToExit();

        const return_count = if (frame.isLoop())
            frame.numParamValues()
        else
            frame.numReturnValues();

        const destination = frame.brDestination();

        // Get args and jump
        const args = self.state.peekn(return_count);
        _ = try self.builder.ins().jump(destination, args);

        self.state.popn(return_count);
        self.state.reachable = false;
    }

    /// Translate a br_if instruction.
    pub fn translateBrIf(self: *Self, relative_depth: u32) !void {
        const condition = self.state.pop1();

        const frame = self.state.getFrameMut(relative_depth);
        frame.setBranchedToExit();

        const return_count = if (frame.isLoop())
            frame.numParamValues()
        else
            frame.numReturnValues();

        const destination = frame.brDestination();

        // Get args for branch
        const args = self.state.peekn(return_count);

        // Create fall-through block
        const next_block = try self.builder.createBlock();
        for (0..return_count) |_| {
            _ = try self.builder.appendBlockParam(next_block, Type.I32);
        }

        // Emit conditional branch
        _ = try self.builder.ins().brif(condition, destination, args, next_block, args);

        // Switch to next block
        self.builder.switchToBlock(next_block);
        try self.builder.sealBlock(next_block);

        // F1 Fix: Ensure next block is in Layout after switching
        try self.builder.ensureInsertedBlock();

        // Replace stack values with block params
        self.state.popn(return_count);
        const next_params = self.builder.blockParams(next_block);
        for (next_params) |param| {
            try self.state.push1(param);
        }
    }

    /// Translate a br_table instruction.
    /// Port of code_translator.rs Operator::BrTable handling.
    pub fn translateBrTable(self: *Self, targets: []const u32, default: u32) !void {
        // Find minimum depth to determine jump args count
        var min_depth = default;
        for (targets) |depth| {
            if (depth < min_depth) {
                min_depth = depth;
            }
        }

        // Get number of values to pass through the branch
        const jump_args_count = blk: {
            const frame = self.state.getFrame(min_depth);
            break :blk if (frame.isLoop())
                frame.numParamValues()
            else
                frame.numReturnValues();
        };

        // Pop the selector value
        const selector = self.state.pop1();

        if (jump_args_count == 0) {
            // No jump arguments - simple case
            // Build jump table entries
            const allocator = self.builder.getAllocator();
            var jt_targets = try allocator.alloc(Block, targets.len);
            defer allocator.free(jt_targets);

            for (targets, 0..) |depth, i| {
                const frame = self.state.getFrameMut(depth);
                frame.setBranchedToExit();
                jt_targets[i] = frame.brDestination();
            }

            // Default target
            const default_frame = self.state.getFrameMut(default);
            default_frame.setBranchedToExit();
            const default_block = default_frame.brDestination();

            // Create jump table and emit br_table
            const jt = try self.builder.createJumpTable(default_block, jt_targets);
            _ = try self.builder.ins().brTable(selector, jt);
        } else {
            // Jump arguments case - need to split edges
            // Port of code_translator.rs lines 526-568
            // Create intermediate blocks with NO args, then fill them in after br_table
            const allocator = self.builder.getAllocator();

            // Track unique depths -> intermediate blocks
            var dest_block_sequence = std.ArrayListUnmanaged(struct { depth: usize, block: Block }){};
            defer dest_block_sequence.deinit(allocator);
            var dest_block_map = std.AutoHashMapUnmanaged(usize, Block){};
            defer dest_block_map.deinit(allocator);

            var jt_targets = try allocator.alloc(Block, targets.len);
            defer allocator.free(jt_targets);

            for (targets, 0..) |depth, i| {
                const gop = try dest_block_map.getOrPut(allocator, @intCast(depth));
                if (!gop.found_existing) {
                    const block = try self.builder.createBlock();
                    try dest_block_sequence.append(allocator, .{ .depth = @intCast(depth), .block = block });
                    gop.value_ptr.* = block;
                }
                jt_targets[i] = gop.value_ptr.*;
            }

            // Default target intermediate block
            const default_gop = try dest_block_map.getOrPut(allocator, @intCast(default));
            if (!default_gop.found_existing) {
                const block = try self.builder.createBlock();
                try dest_block_sequence.append(allocator, .{ .depth = @intCast(default), .block = block });
                default_gop.value_ptr.* = block;
            }
            const default_branch_block = default_gop.value_ptr.*;

            // Emit br_table to intermediate blocks (no args)
            const jt = try self.builder.createJumpTable(default_branch_block, jt_targets);
            _ = try self.builder.ins().brTable(selector, jt);

            // Fill in each intermediate block: jump to real destination with args
            for (dest_block_sequence.items) |entry| {
                self.builder.switchToBlock(entry.block);
                try self.builder.sealBlock(entry.block);
                try self.builder.ensureInsertedBlock();

                const real_frame = self.state.getFrameMut(@intCast(entry.depth));
                real_frame.setBranchedToExit();
                const real_dest = real_frame.brDestination();
                const args = self.state.peekn(jump_args_count);
                _ = try self.builder.ins().jump(real_dest, args);
            }
        }

        self.state.popn(jump_args_count);
        self.state.reachable = false;
    }

    /// Translate a return instruction.
    pub fn translateReturn(self: *Self) !void {
        // Get function frame (bottom of control stack)
        if (self.state.controlStackLen() == 0) {
            // No control frames - just emit return
            debug.log(.codegen, "translateReturn: no control frames, emitting empty return", .{});
            _ = try self.builder.ins().return_(&[_]Value{});
            self.state.reachable = false;
            return;
        }

        const func_frame = &self.state.control_stack.items[0];
        const return_count = func_frame.numReturnValues();

        debug.log(.codegen, "translateReturn: control_stack_len={d}, return_count={d}, stack_size={d}", .{ self.state.controlStackLen(), return_count, self.state.stack.items.len });

        const args = self.state.peekn(return_count);
        debug.log(.codegen, "translateReturn: emitting return with {d} args", .{args.len});
        for (args, 0..) |arg, i| {
            debug.log(.codegen, "translateReturn: arg[{d}] = v{d}", .{ i, arg.asU32() });
        }
        _ = try self.builder.ins().return_(args);

        self.state.popn(return_count);
        self.state.reachable = false;
    }

    // ========================================================================
    // Local Variable Translation
    // ========================================================================

    /// Translate local.get
    pub fn translateLocalGet(self: *Self, local_index: u32) !void {
        const variable = self.locals.items[local_index];
        const val = try self.builder.useVar(variable);
        try self.state.push1(val);
    }

    /// Translate local.set
    pub fn translateLocalSet(self: *Self, local_index: u32) !void {
        const val = self.state.pop1();
        const variable = self.locals.items[local_index];
        try self.builder.defVar(variable, val);
    }

    /// Translate local.tee
    pub fn translateLocalTee(self: *Self, local_index: u32) !void {
        const val = self.state.peek1();
        const variable = self.locals.items[local_index];
        try self.builder.defVar(variable, val);
    }

    // ========================================================================
    // Global Variable Translation
    // Port of code_translator.rs translate_global_get/set
    //
    // Cranelift implementation (func_environ.rs:3134) handles globals via:
    // 1. Constant globals: emit iconst/fconst with the constant value
    // 2. Memory globals: use global_value instruction to get address, then load
    //
    // We use FuncEnvironment to manage GlobalValue creation and caching.
    // ========================================================================

    /// Translate global.get
    ///
    /// Port of code_translator.rs Operator::GlobalGet handling.
    /// Uses FuncEnvironment.getOrCreateGlobal() to get GlobalVariable info,
    /// then emits either a constant or global_value + load.
    pub fn translateGlobalGet(self: *Self, global_index: u32) !void {
        // Get global type from module info (default to i64 for safety)
        const global_val_type: WasmValType = if (global_index < self.globals.len)
            self.globals[global_index].val_type
        else
            .i64;
        const global_type = global_val_type.toClifType();

        // Determine if constant (immutable) or memory-based
        // Immutable globals can be treated as constants
        const is_constant = if (global_index < self.globals.len)
            !self.globals[global_index].mutable
        else
            false; // Unknown globals treated as mutable for safety

        const var_info = try self.env.getOrCreateGlobal(
            self.builder.func,
            global_index,
            global_type,
            is_constant,
            null, // constant_value - could be populated from init expr
        );

        switch (var_info) {
            .constant => |c| {
                // Emit constant directly
                const val = switch (c) {
                    .i32 => |v| try self.builder.ins().iconst(Type.I32, v),
                    .i64 => |v| try self.builder.ins().iconst(Type.I64, v),
                    .f32 => |v| try self.builder.ins().f32const(v),
                    .f64 => |v| try self.builder.ins().f64const(v),
                };
                try self.state.push1(val);
            },
            .memory => |m| {
                // Use global_value instruction to compute address
                const addr = try self.builder.ins().globalValue(self.env.pointer_type, m.gv);
                // Load the value from memory
                const value = try self.builder.ins().load(m.ty, clif.MemFlags.DEFAULT, addr, m.offset);
                try self.state.push1(value);
            },
        }
    }

    /// Translate global.set
    ///
    /// Port of code_translator.rs Operator::GlobalSet handling.
    /// Uses FuncEnvironment.getOrCreateGlobal() to get GlobalVariable info,
    /// then emits global_value + store.
    pub fn translateGlobalSet(self: *Self, global_index: u32) !void {
        const value = self.state.pop1();

        // Get global type from module info (default to i64 for safety)
        const global_val_type: WasmValType = if (global_index < self.globals.len)
            self.globals[global_index].val_type
        else
            .i64;
        const global_type = global_val_type.toClifType();

        const var_info = try self.env.getOrCreateGlobal(
            self.builder.func,
            global_index,
            global_type,
            false, // is_constant - constants can't be set
            null,
        );

        switch (var_info) {
            .constant => unreachable, // Cannot set constant globals
            .memory => |m| {
                // Use global_value instruction to compute address
                const addr = try self.builder.ins().globalValue(self.env.pointer_type, m.gv);
                // Store the value to memory
                _ = try self.builder.ins().store(clif.MemFlags.DEFAULT, value, addr, m.offset);
            },
        }
    }

    // ========================================================================
    // Constant Translation
    // ========================================================================

    /// Translate i32.const
    pub fn translateI32Const(self: *Self, value: i32) !void {
        const result = try self.builder.ins().iconst(Type.I32, value);
        try self.state.push1(result);
    }

    /// Translate i64.const
    pub fn translateI64Const(self: *Self, value: i64) !void {
        const result = try self.builder.ins().iconst(Type.I64, value);
        try self.state.push1(result);
    }

    // ========================================================================
    // Binary Arithmetic Translation
    // ========================================================================

    // ========================================================================
    // Arithmetic Translation (unified i32/i64)
    // Port of code_translator.rs:1193-1268
    // CLIF instructions are type-agnostic - they infer types from operands
    // ========================================================================

    pub fn translateIAdd(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().iadd(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateISub(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().isub(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIMul(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().imul(args[0], args[1]);
        try self.state.push1(result);
    }

    /// Signed division with trap guards.
    /// Port of func_environ.rs:4467-4478 (translate_sdiv)
    /// Guards: trap if rhs == 0, trap if lhs == MIN_INT and rhs == -1
    pub fn translateSDivWithGuard(self: *Self) !void {
        const args = self.state.pop2();
        const lhs = args[0];
        const rhs = args[1];

        // Guard: trap if rhs == 0 (division by zero)
        try self.guardZeroDivisor(rhs);

        // Guard: trap if signed overflow (MIN_INT / -1)
        // This is deferred for now - CLIF sdiv traps on overflow
        // TODO: Add explicit guard for platforms that don't trap

        const result = try self.builder.ins().sdiv(lhs, rhs);
        try self.state.push1(result);
    }

    /// Unsigned division with trap guard.
    /// Port of func_environ.rs:4480-4486 (translate_udiv)
    pub fn translateUDivWithGuard(self: *Self) !void {
        const args = self.state.pop2();
        const lhs = args[0];
        const rhs = args[1];

        // Guard: trap if rhs == 0
        try self.guardZeroDivisor(rhs);

        const result = try self.builder.ins().udiv(lhs, rhs);
        try self.state.push1(result);
    }

    /// Signed remainder with trap guard.
    /// Port of func_environ.rs:4488-4496 (translate_srem)
    pub fn translateSRemWithGuard(self: *Self) !void {
        const args = self.state.pop2();
        const lhs = args[0];
        const rhs = args[1];

        // Guard: trap if rhs == 0
        try self.guardZeroDivisor(rhs);

        const result = try self.builder.ins().srem(lhs, rhs);
        try self.state.push1(result);
    }

    /// Unsigned remainder with trap guard.
    /// Port of func_environ.rs:4498-4505 (translate_urem)
    pub fn translateURemWithGuard(self: *Self) !void {
        const args = self.state.pop2();
        const lhs = args[0];
        const rhs = args[1];

        // Guard: trap if rhs == 0
        try self.guardZeroDivisor(rhs);

        const result = try self.builder.ins().urem(lhs, rhs);
        try self.state.push1(result);
    }

    /// Guard against division by zero.
    /// Port of func_environ.rs guard_zero_divisor pattern
    fn guardZeroDivisor(self: *Self, divisor: Value) !void {
        // Get the type of the divisor to create the right zero constant
        const val_type = self.builder.func.dfg.valueType(divisor);
        const zero = try self.builder.ins().iconst(val_type, 0);
        const is_zero = try self.builder.ins().icmp(clif.IntCC.eq, divisor, zero);

        // Trap if divisor is zero
        _ = try self.builder.ins().trapnz(is_zero, clif.TrapCode.integer_division_by_zero);
    }

    pub fn translateBAnd(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().band(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateBOr(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().bor(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateBXor(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().bxor(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIShl(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().ishl(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateSShr(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().sshr(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateUShr(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().ushr(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateRotl(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().rotl(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateRotr(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().rotr(args[0], args[1]);
        try self.state.push1(result);
    }

    // ========================================================================
    // Comparison Translation (unified i32/i64)
    // Port of code_translator.rs:1286-1318 and translate_icmp helper (3736-3740)
    // CLIF icmp is type-agnostic - infers types from operands
    // ========================================================================

    pub fn translateIEq(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.eq, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateINe(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ne, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateILtS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.slt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateILtU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ult, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIGtS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sgt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIGtU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ugt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateILeS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sle, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateILeU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ule, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIGeS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sge, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateIGeU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.uge, args[0], args[1]);
        try self.state.push1(result);
    }

    /// Translate eqz (compare equal to zero) - unified for i32/i64
    /// Port of code_translator.rs:1310-1314
    pub fn translateIEqz(self: *Self) !void {
        const arg = self.state.pop1();
        // Get the type of the operand to create the right zero constant
        const val_type = self.builder.func.dfg.valueType(arg);
        const zero = try self.builder.ins().iconst(val_type, 0);
        const result = try self.builder.ins().icmp(clif.IntCC.eq, arg, zero);
        try self.state.push1(result);
    }

    // ========================================================================
    // Conversion Translation
    // ========================================================================

    pub fn translateI32WrapI64(self: *Self) !void {
        const arg = self.state.pop1();
        const result = try self.builder.ins().ireduce(Type.I32, arg);
        try self.state.push1(result);
    }

    pub fn translateI64ExtendI32S(self: *Self) !void {
        const arg = self.state.pop1();
        const result = try self.builder.ins().sextend(Type.I64, arg);
        try self.state.push1(result);
    }

    pub fn translateI64ExtendI32U(self: *Self) !void {
        const arg = self.state.pop1();
        const result = try self.builder.ins().uextend(Type.I64, arg);
        try self.state.push1(result);
    }

    // ========================================================================
    // Parametric Translation
    // ========================================================================

    pub fn translateDrop(self: *Self) !void {
        _ = self.state.pop1();
    }

    pub fn translateSelect(self: *Self) !void {
        const args = self.state.pop3();
        // args = (val1, val2, cond)
        const ty = self.builder.func.dfg.valueType(args[0]);
        const result = try self.builder.ins().select(ty, args[2], args[0], args[1]);
        try self.state.push1(result);
    }

    // ========================================================================
    // Trap Translation
    // ========================================================================

    pub fn translateUnreachable(self: *Self) !void {
        _ = try self.builder.ins().trap(clif.TrapCode.unreachable_code_reached);
        self.state.reachable = false;
    }

    // ========================================================================
    // Memory Translation
    // Port of code_translator.rs:3680-3724 (translate_load/store)
    // and code_translator.rs:3459-3628 (prepare_addr)
    // ========================================================================

    /// Prepare address for a memory access.
    ///
    /// Port of code_translator.rs:3459-3628 (prepare_addr)
    ///
    /// Pops the index from the stack, performs bounds checking, and returns
    /// the native address for the memory access.
    fn prepareAddr(self: *Self, memarg: MemArg, access_size: u8) !struct { flags: clif.MemFlags, addr: Value } {
        const index = self.state.pop1();
        const memory_index = memarg.memory;

        // Get or create heap for this memory
        const heap = try self.env.getOrCreateHeap(self.builder.func, memory_index);

        // Compute address with bounds checking
        const addr = try bounds_checks.boundsCheckAndComputeAddr(
            self.builder,
            heap,
            index,
            memarg.offset,
            access_size,
        );

        // Set memory flags (little-endian for Wasm)
        // Note: Cranelift sets endianness but our flags struct doesn't support it yet
        const flags = clif.MemFlags.DEFAULT;

        return .{ .flags = flags, .addr = addr };
    }

    /// Translate a load instruction.
    ///
    /// Port of code_translator.rs:3680-3700 (translate_load)
    pub fn translateLoad(self: *Self, memarg: MemArg, result_ty: Type) !void {
        const access_size: u8 = @intCast(result_ty.bytes());
        const prepared = try self.prepareAddr(memarg, access_size);
        const value = try self.builder.ins().load(result_ty, prepared.flags, prepared.addr, 0);
        try self.state.push1(value);
    }

    /// Translate a store instruction.
    ///
    /// Port of code_translator.rs:3703-3724 (translate_store)
    pub fn translateStore(self: *Self, memarg: MemArg, val_ty: Type) !void {
        const value = self.state.pop1();
        const access_size: u8 = @intCast(val_ty.bytes());
        const prepared = try self.prepareAddr(memarg, access_size);
        _ = try self.builder.ins().store(prepared.flags, value, prepared.addr, 0);
    }

    /// Translate a sign-extending load (load8_s, load16_s, load32_s).
    ///
    /// Loads a smaller value and sign-extends to the result type.
    pub fn translateSLoad(self: *Self, memarg: MemArg, load_size: u8, result_ty: Type) !void {
        const prepared = try self.prepareAddr(memarg, load_size);

        // Determine the load type based on size
        const load_ty = switch (load_size) {
            1 => Type.I8,
            2 => Type.I16,
            4 => Type.I32,
            else => unreachable,
        };

        const loaded = try self.builder.ins().load(load_ty, prepared.flags, prepared.addr, 0);
        const extended = try self.builder.ins().sextend(result_ty, loaded);
        try self.state.push1(extended);
    }

    /// Translate a zero-extending load (load8_u, load16_u, load32_u).
    ///
    /// Loads a smaller value and zero-extends to the result type.
    pub fn translateULoad(self: *Self, memarg: MemArg, load_size: u8, result_ty: Type) !void {
        const prepared = try self.prepareAddr(memarg, load_size);

        // Determine the load type based on size
        const load_ty = switch (load_size) {
            1 => Type.I8,
            2 => Type.I16,
            4 => Type.I32,
            else => unreachable,
        };

        const loaded = try self.builder.ins().load(load_ty, prepared.flags, prepared.addr, 0);
        const extended = try self.builder.ins().uextend(result_ty, loaded);
        try self.state.push1(extended);
    }

    /// Translate a truncating store (store8, store16, store32).
    ///
    /// Truncates the value and stores a smaller type.
    pub fn translateTruncStore(self: *Self, memarg: MemArg, store_size: u8) !void {
        const value = self.state.pop1();
        const prepared = try self.prepareAddr(memarg, store_size);

        // Determine the store type based on size
        const store_ty = switch (store_size) {
            1 => Type.I8,
            2 => Type.I16,
            4 => Type.I32,
            else => unreachable,
        };

        // Truncate value if needed
        const val_ty = self.builder.func.dfg.valueType(value);
        const truncated = if (val_ty.bits() > store_ty.bits())
            try self.builder.ins().ireduce(store_ty, value)
        else
            value;

        _ = try self.builder.ins().store(prepared.flags, truncated, prepared.addr, 0);
    }

    // ========================================================================
    // Call Translation
    // Port of code_translator.rs:654-717 and func_environ.rs Call struct
    // ========================================================================

    /// Translate a direct function call.
    ///
    /// Port of code_translator.rs:654-676 (Operator::Call)
    /// and func_environ.rs:1940-2040 (Call::direct_call)
    ///
    /// Wasm call ABI: real_call_args = [callee_vmctx, caller_vmctx, ...wasm_args]
    pub fn translateCall(self: *Self, function_index: u32) !void {
        // Get the function reference from the environment
        const func_ref = try self.env.getOrCreateFuncRef(self.builder.func, function_index);

        // Get function signature to determine number of params/returns
        const ext_func = self.builder.func.getExtFunc(func_ref) orelse
            return error.InvalidFuncRef;
        const sig = self.builder.func.getSignature(ext_func.signature) orelse
            return error.InvalidSignature;

        // Wasm params don't include vmctx - calculate actual Wasm param count
        // The sig includes callee_vmctx and caller_vmctx as first two params
        const num_wasm_params = if (sig.params.items.len >= 2)
            sig.params.items.len - 2
        else
            0;
        const num_returns = sig.returns.items.len;

        // Pop wasm args from stack
        const wasm_args = self.state.peekn(num_wasm_params);

        // Build real_call_args: [callee_vmctx, caller_vmctx, ...wasm_args]
        // For local functions, callee_vmctx == caller_vmctx
        const caller_vmctx = try self.env.vmctxVal(self.builder.func);
        const vmctx_val = try self.builder.ins().globalValue(Type.I64, caller_vmctx);

        // Allocate call args: 2 vmctx values + wasm args
        var real_args = try self.allocator.alloc(Value, 2 + num_wasm_params);
        defer self.allocator.free(real_args);
        real_args[0] = vmctx_val; // callee_vmctx (same as caller for local funcs)
        real_args[1] = vmctx_val; // caller_vmctx
        for (wasm_args, 0..) |arg, i| {
            real_args[2 + i] = arg;
        }

        // Emit call instruction
        const call_result = try self.builder.ins().call(func_ref, real_args);

        // Pop args, push results
        self.state.popn(num_wasm_params);
        for (call_result.results) |result| {
            try self.state.push1(result);
        }

        _ = num_returns;
    }

    /// Translate an indirect function call (AOT version).
    ///
    /// For AOT compilation, we know the table contents at compile time from the
    /// element section. Instead of using runtime funcref tables, we generate an
    /// if-else dispatch chain that maps each table index to a direct call.
    ///
    /// For table_elements = [func_2, func_7, func_8]:
    ///   if callee_index == 0: call func_2(args...)
    ///   elif callee_index == 1: call func_7(args...)
    ///   elif callee_index == 2: call func_8(args...)
    ///   else: trap
    pub fn translateCallIndirect(self: *Self, type_index: u32, table_index: u32) !void {
        _ = table_index; // We only support table 0

        // Pop callee index from stack (i32 table index)
        const callee_index = self.state.pop1();

        // Look up function type to know how many params/results
        const func_type = if (type_index < self.func_types.len)
            self.func_types[type_index]
        else
            WasmFuncType{ .params = &[_]WasmValType{}, .results = &[_]WasmValType{} };

        const num_wasm_params = func_type.params.len;
        const num_results = func_type.results.len;

        // Pop wasm args from stack (we'll pass them to each branch)
        const wasm_args = self.state.peekn(num_wasm_params);

        // Save args in a local array since we'll reference them in each branch
        const saved_args = try self.allocator.alloc(Value, num_wasm_params);
        defer self.allocator.free(saved_args);
        @memcpy(saved_args, wasm_args);

        // Pop args from state now (before branches)
        self.state.popn(num_wasm_params);

        // Create merge block where all branches converge with the result
        const merge_block = try self.builder.createBlock();
        // Add block params for the result values
        var merge_params = try self.allocator.alloc(Value, num_results);
        defer self.allocator.free(merge_params);
        for (0..num_results) |i| {
            merge_params[i] = try self.builder.appendBlockParam(merge_block, func_type.results[i].toClifType());
        }

        const num_entries = self.table_elements.len;

        if (num_entries == 0) {
            // No table entries — always trap
            _ = try self.builder.ins().trap(clif.TrapCode.table_out_of_bounds);
            self.builder.switchToBlock(merge_block);
            try self.builder.sealBlock(merge_block);
            try self.builder.ensureInsertedBlock();
            for (merge_params) |p| {
                try self.state.push1(p);
            }
            return;
        }

        // Generate if-else chain for table entries whose signature matches type_index.
        // Only entries with matching signatures are valid targets for this call_indirect.
        // Non-matching entries (or out of bounds) trap, matching Wasm's runtime signature check.
        for (0..num_entries) |i| {
            const func_index = self.table_elements[i];

            // Check if this function's signature matches the expected type_index
            const func_type_idx: ?u32 = if (func_index < self.env.func_to_type.len)
                self.env.func_to_type[func_index]
            else
                null;
            if (func_type_idx == null or func_type_idx.? != type_index) continue;

            // Compare callee_index with this table index
            const table_idx_val = try self.builder.ins().iconst(Type.I32, @intCast(i));
            const is_match = try self.builder.ins().icmp(clif.IntCC.eq, callee_index, table_idx_val);

            // Create blocks: match_block (for this entry), next_block (continue checking)
            const match_block = try self.builder.createBlock();
            const next_block = try self.builder.createBlock();

            // Branch: if match, go to match_block; else go to next_block
            _ = try self.builder.ins().brif(is_match, match_block, &[_]Value{}, next_block, &[_]Value{});

            // === Match block: emit direct call ===
            self.builder.switchToBlock(match_block);
            try self.builder.sealBlock(match_block);
            try self.builder.ensureInsertedBlock();

            // Get function reference for the target function
            const func_ref = try self.env.getOrCreateFuncRef(self.builder.func, func_index);

            // Build call args: [callee_vmctx, caller_vmctx, ...wasm_args]
            const vmctx_gv = try self.env.vmctxVal(self.builder.func);
            const vmctx_val = try self.builder.ins().globalValue(Type.I64, vmctx_gv);

            var real_args = try self.allocator.alloc(Value, 2 + num_wasm_params);
            defer self.allocator.free(real_args);
            real_args[0] = vmctx_val;
            real_args[1] = vmctx_val;
            for (saved_args, 0..) |arg, j| {
                real_args[2 + j] = arg;
            }

            // Emit direct call
            const call_result = try self.builder.ins().call(func_ref, real_args);

            // Jump to merge block with results
            _ = try self.builder.ins().jump(merge_block, call_result.results);

            // === Next block: continue checking ===
            self.builder.switchToBlock(next_block);
            try self.builder.sealBlock(next_block);
            try self.builder.ensureInsertedBlock();
        }

        // After all entries: trap (out of bounds)
        _ = try self.builder.ins().trap(clif.TrapCode.table_out_of_bounds);

        // Switch to merge block and push results
        self.builder.switchToBlock(merge_block);
        try self.builder.sealBlock(merge_block);
        try self.builder.ensureInsertedBlock();

        for (merge_params) |p| {
            try self.state.push1(p);
        }
    }

    // ========================================================================
    // Float Constant Translation
    // ========================================================================

    pub fn translateF32Const(self: *Self, val: f32) !void {
        const result = try self.builder.ins().f32const(val);
        try self.state.push1(result);
    }

    pub fn translateF64Const(self: *Self, val: f64) !void {
        const result = try self.builder.ins().f64const(val);
        try self.state.push1(result);
    }

    // ========================================================================
    // Float Arithmetic Translation (binary)
    // Port of code_translator.rs float binary operations
    // ========================================================================

    pub fn translateFAdd(self: *Self) !void {
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const result = try self.builder.ins().fadd(ty, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFSub(self: *Self) !void {
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const result = try self.builder.ins().fsub(ty, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFMul(self: *Self) !void {
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const result = try self.builder.ins().fmul(ty, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFDiv(self: *Self) !void {
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const result = try self.builder.ins().fdiv(ty, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFMin(self: *Self) !void {
        // fmin is not directly available in builder, use fcmp + select pattern
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const cmp = try self.builder.ins().fcmp(clif.FloatCC.lt, args[0], args[1]);
        const result = try self.builder.ins().select(ty, cmp, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFMax(self: *Self) !void {
        // fmax is not directly available in builder, use fcmp + select pattern
        const args = self.state.pop2();
        const ty = self.builder.func.dfg.valueType(args[0]);
        const cmp = try self.builder.ins().fcmp(clif.FloatCC.gt, args[0], args[1]);
        const result = try self.builder.ins().select(ty, cmp, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFCopysign(self: *Self) !void {
        // copysign copies the sign of b to a
        // For now, stub implementation - proper implementation needs bitwise operations
        const args = self.state.pop2();
        _ = args;
        // TODO: Implement properly using bitwise operations
        // For now, just return the first argument
        const result = try self.builder.ins().iconst(Type.I32, 0);
        try self.state.push1(result);
    }

    // ========================================================================
    // Float Arithmetic Translation (unary)
    // ========================================================================

    pub fn translateFAbs(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().fabs(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFNeg(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().fneg(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFCeil(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().ceil(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFFloor(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().floor(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFTrunc(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().ftrunc(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFNearest(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().nearest(ty, arg);
        try self.state.push1(result);
    }

    pub fn translateFSqrt(self: *Self) !void {
        const arg = self.state.pop1();
        const ty = self.builder.func.dfg.valueType(arg);
        const result = try self.builder.ins().sqrt(ty, arg);
        try self.state.push1(result);
    }

    // ========================================================================
    // Float Comparison Translation
    // Port of code_translator.rs translate_fcmp pattern
    // Following the same pattern as integer comparisons
    // ========================================================================

    pub fn translateFEq(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.eq, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFNe(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.ne, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFLt(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.lt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFGt(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.gt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFLe(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.le, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateFGe(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().fcmp(clif.FloatCC.ge, args[0], args[1]);
        try self.state.push1(result);
    }

    // ========================================================================
    // Float Conversion Translation
    // ========================================================================

    pub fn translateTruncFS(self: *Self, result_ty: Type) !void {
        const arg = self.state.pop1();
        // fcvt_to_sint converts float to signed int
        const result = try self.builder.ins().fcvtToSint(result_ty, arg);
        try self.state.push1(result);
    }

    pub fn translateTruncFU(self: *Self, result_ty: Type) !void {
        const arg = self.state.pop1();
        // fcvt_to_uint converts float to unsigned int
        const result = try self.builder.ins().fcvtToUint(result_ty, arg);
        try self.state.push1(result);
    }

    pub fn translateConvertFS(self: *Self, result_ty: Type) !void {
        const arg = self.state.pop1();
        // fcvt_from_sint converts signed int to float
        const result = try self.builder.ins().fcvtFromSint(result_ty, arg);
        try self.state.push1(result);
    }

    pub fn translateConvertFU(self: *Self, result_ty: Type) !void {
        const arg = self.state.pop1();
        // fcvt_from_uint converts unsigned int to float
        const result = try self.builder.ins().fcvtFromUint(result_ty, arg);
        try self.state.push1(result);
    }

    pub fn translateF32DemoteF64(self: *Self) !void {
        const arg = self.state.pop1();
        const result = try self.builder.ins().fdemote(Type.F32, arg);
        try self.state.push1(result);
    }

    pub fn translateF64PromoteF32(self: *Self) !void {
        const arg = self.state.pop1();
        const result = try self.builder.ins().fpromote(Type.F64, arg);
        try self.state.push1(result);
    }

    pub fn translateReinterpret(self: *Self, to_ty: Type, from_ty: Type) !void {
        const arg = self.state.pop1();
        _ = from_ty;
        // bitcast reinterprets the bits without conversion
        const result = try self.builder.ins().bitcast(to_ty, arg);
        try self.state.push1(result);
    }

    // ========================================================================
    // Memory Size/Grow Translation
    // Port of code_translator.rs:795-808
    // ========================================================================

    /// Translate Wasm memory.size instruction.
    ///
    /// Port of Cranelift func_environ.rs:3443-3525 (translate_memory_size)
    ///
    /// Loads current memory length in bytes from vmctx heap bound,
    /// then divides by page size (right-shift by 16) to get pages.
    pub fn translateMemorySize(self: *Self, mem_index: u32) !void {
        // Get or create heap to access the bound GlobalValue
        const heap = try self.env.getOrCreateHeap(self.builder.func, mem_index);

        // Load current_length_in_bytes from vmctx heap bound
        // Reference: Cranelift loads VMMemoryDefinition.current_length
        const bound_addr = try self.builder.ins().globalValue(self.env.pointer_type, heap.bound);
        const current_length = try self.builder.ins().load(
            Type.I64,
            clif.MemFlags.DEFAULT,
            bound_addr,
            0,
        );

        // Divide by page size: ushr_imm by page_size_log2 (16 for 64KB pages)
        // Reference: Cranelift: pos.ins().ushr_imm(current_length_in_bytes, page_size_log2)
        const shift = try self.builder.ins().iconst(Type.I64, 16);
        const pages_i64 = try self.builder.ins().ushr(current_length, shift);

        // Truncate to i32 (Wasm32 memory.size returns i32)
        // Reference: Cranelift: convert_pointer_to_index_type → ireduce
        const pages_i32 = try self.builder.ins().ireduce(Type.I32, pages_i64);
        try self.state.push1(pages_i32);
    }

    /// Translate Wasm memory.grow instruction.
    ///
    /// Port of Cranelift func_environ.rs:3412-3441 (translate_memory_grow)
    ///
    /// For AOT with pre-allocated memory:
    /// - Load current bound from vmctx
    /// - Check if new_bound <= max_heap_size (pre-allocated limit)
    /// - If ok: store new bound, return old page count
    /// - If fail: return -1
    ///
    /// Cranelift uses a libcall to wasmtime's memory_grow runtime function.
    /// We inline the logic since our AOT model has fixed pre-allocated memory.
    /// The semantics are identical: return old pages on success, -1 on failure.
    pub fn translateMemoryGrow(self: *Self, mem_index: u32) !void {
        const delta_i32 = self.state.pop1();

        // Get or create heap for bound access
        const heap = try self.env.getOrCreateHeap(self.builder.func, mem_index);

        // Extend delta to i64 (Wasm i32 -> internal i64)
        // Reference: Cranelift: cast_index_to_i64 → uextend
        const delta = try self.builder.ins().uextend(Type.I64, delta_i32);

        // Load current heap bound (bytes) from vmctx
        const bound_addr = try self.builder.ins().globalValue(self.env.pointer_type, heap.bound);
        const current_bound = try self.builder.ins().load(
            Type.I64,
            clif.MemFlags.DEFAULT,
            bound_addr,
            0,
        );

        // delta_bytes = delta * 65536 (delta << 16)
        const page_shift = try self.builder.ins().iconst(Type.I64, 16);
        const delta_bytes = try self.builder.ins().ishl(delta, page_shift);

        // new_bound = current_bound + delta_bytes
        const new_bound = try self.builder.ins().iadd(current_bound, delta_bytes);

        // max_heap = pre-allocated limit (vmctx_size - linear_memory_base)
        // vmctx is 16MB (0x1000000), linear_memory_base is 0x40000
        const max_heap: i64 = 0x1000000 - 0x40000; // ~15.7MB
        const max_heap_val = try self.builder.ins().iconst(Type.I64, max_heap);

        // cond = new_bound > max_heap (unsigned)
        const cond = try self.builder.ins().icmp(clif.IntCC.ugt, new_bound, max_heap_val);

        // Create success and failure blocks, plus merge block
        const success_block = try self.builder.createBlock();
        const fail_block = try self.builder.createBlock();
        const merge_block = try self.builder.createBlock();
        const merge_param = try self.builder.appendBlockParam(merge_block, Type.I32);

        // brif cond -> fail_block, else -> success_block
        _ = try self.builder.ins().brif(cond, fail_block, &[_]Value{}, success_block, &[_]Value{});

        // --- Fail block: return -1 ---
        self.builder.switchToBlock(fail_block);
        try self.builder.sealBlock(fail_block);
        try self.builder.ensureInsertedBlock();
        const neg_one = try self.builder.ins().iconst(Type.I32, @as(i32, -1));
        _ = try self.builder.ins().jump(merge_block, &[_]Value{neg_one});

        // --- Success block: store new bound, return old pages ---
        self.builder.switchToBlock(success_block);
        try self.builder.sealBlock(success_block);
        try self.builder.ensureInsertedBlock();

        // Store new bound to vmctx
        const bound_addr2 = try self.builder.ins().globalValue(self.env.pointer_type, heap.bound);
        _ = try self.builder.ins().store(clif.MemFlags.DEFAULT, new_bound, bound_addr2, 0);

        // old_pages = current_bound >> 16 (divide by page size)
        const old_pages_i64 = try self.builder.ins().ushr(current_bound, page_shift);
        const old_pages = try self.builder.ins().ireduce(Type.I32, old_pages_i64);
        _ = try self.builder.ins().jump(merge_block, &[_]Value{old_pages});

        // --- Merge block: result is the block param ---
        self.builder.switchToBlock(merge_block);
        try self.builder.sealBlock(merge_block);
        try self.builder.ensureInsertedBlock();
        try self.state.push1(merge_param);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "translate i32.const and i32.add with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 0, 1);

    try translator.translateI32Const(10);
    try translator.translateI32Const(20);
    try translator.translateIAdd();

    try testing.expectEqual(@as(usize, 1), translator.state.stackLen());

    // Verify CLIF function was built
    const entry_block = func.layout.entryBlock();
    try testing.expect(entry_block != null);
}

test "translate block and end with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 0, 0);

    try testing.expectEqual(@as(usize, 1), translator.state.controlStackLen());

    try translator.translateBlock(0, 0);
    try testing.expectEqual(@as(usize, 2), translator.state.controlStackLen());

    try translator.translateEnd();
    try testing.expectEqual(@as(usize, 1), translator.state.controlStackLen());
}

test "translate loop with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 0, 0);

    try translator.translateLoop(0, 0);

    const frame = translator.state.getFrame(0);
    try testing.expect(frame.isLoop());

    // For loop, br_destination should be header (not exit)
    const header = frame.brDestination();
    const exit = frame.followingCode();
    try testing.expect(header.asU32() != exit.asU32());
}

test "translate br with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 0, 0);
    try translator.translateBlock(0, 0);

    try translator.translateBr(0);

    try testing.expect(!translator.state.reachable);
}

test "translate local.get and local.set with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 2, 0);

    // local.set 0
    try translator.translateI32Const(42);
    try translator.translateLocalSet(0);

    // local.get 0
    try translator.translateLocalGet(0);

    try testing.expectEqual(@as(usize, 1), translator.state.stackLen());
}

test "translate comparison with FunctionBuilder" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var func = Function.init(allocator);
    defer func.deinit();

    var ctx = FunctionBuilderContext.init(allocator);
    defer ctx.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    var translator = FuncTranslator.initWithoutGlobals(allocator, &builder);
    defer translator.deinit();

    try translator.initializeFunctionSimple(0, 0, 0);

    try translator.translateI32Const(10);
    try translator.translateI32Const(20);
    try translator.translateILtS();

    try testing.expectEqual(@as(usize, 1), translator.state.stackLen());
}
