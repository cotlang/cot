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
    /// Function environment for global variable management.
    /// Port of Cranelift's FuncEnvironment.
    env: FuncEnvironment,
    /// Allocator for dynamic storage.
    allocator: std.mem.Allocator,

    const Self = @This();

    /// Create a new translator with module-level globals information.
    pub fn init(allocator: std.mem.Allocator, builder: *FunctionBuilder, globals: []const WasmGlobalType) Self {
        return .{
            .state = TranslationState.init(allocator),
            .builder = builder,
            .locals = .{},
            .globals = globals,
            .env = FuncEnvironment.init(allocator),
            .allocator = allocator,
        };
    }

    /// Create a new translator without globals (backwards compatibility).
    pub fn initWithoutGlobals(allocator: std.mem.Allocator, builder: *FunctionBuilder) Self {
        return init(allocator, builder, &[_]WasmGlobalType{});
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
    pub fn initializeFunction(self: *Self, num_params: u32, num_locals: u32, param_types: []const Type, local_types: []const Type, num_returns: usize) !void {
        // Create entry and exit blocks
        const entry_block = try self.builder.createBlock();
        const exit_block = try self.builder.createBlock();

        // Switch to entry block
        self.builder.switchToBlock(entry_block);

        // Add function parameters to entry block
        try self.builder.appendBlockParamsForFunctionParams(entry_block);

        // Seal entry block (no predecessors except function entry)
        try self.builder.sealBlock(entry_block);

        // Initialize translation state with exit block
        try self.state.initialize(exit_block, num_returns);

        // Declare variables for all Wasm locals (params + locals)
        const total_locals = num_params + num_locals;
        self.locals.clearRetainingCapacity();
        try self.locals.ensureTotalCapacity(self.allocator, total_locals);

        // Declare variables for parameters and define them with block params
        const entry_params = self.builder.blockParams(entry_block);
        for (0..num_params) |i| {
            const var_type = param_types[i];
            const variable = try self.builder.declareVar(var_type);
            self.locals.appendAssumeCapacity(variable);

            // Define parameter variable with block param value
            try self.builder.defVar(variable, entry_params[i]);
        }

        // Declare variables for non-parameter locals (initialized to zero)
        for (0..num_locals) |i| {
            const var_type = local_types[i];
            const variable = try self.builder.declareVar(var_type);
            self.locals.appendAssumeCapacity(variable);

            // Initialize local to zero
            const zero = try self.builder.ins().iconst(var_type, 0);
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
        self.builder.switchToBlock(loop_body);
        try self.builder.sealBlock(loop_body);

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
                const else_block = switch (f.else_data) {
                    .no_else => |ne| ne.placeholder,
                    .with_else => |we| we.else_block,
                };

                // Truncate stack to else params
                frame.truncateValueStackToElseParams(&self.state.stack);

                // Switch to else block
                self.builder.switchToBlock(else_block);
                try self.builder.sealBlock(else_block);

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
        const return_count = frame.numReturnValues();

        if (self.state.reachable) {
            // Get return values from stack
            const return_vals = self.state.peekn(return_count);

            // Jump to next block with return values
            _ = try self.builder.ins().jump(next_block, return_vals);
        }

        // Truncate stack to original size
        frame.truncateValueStackToOriginalSize(&self.state.stack);

        // Determine if next block is reachable
        const next_reachable = switch (frame) {
            .if_frame => |f| blk: {
                const conseq_reachable = f.consequent_ends_reachable orelse self.state.reachable;
                break :blk (f.head_is_reachable and conseq_reachable) or f.exit_is_branched_to;
            },
            .block_frame => |f| self.state.reachable or f.exit_is_branched_to,
            .loop_frame => self.state.reachable,
        };

        if (next_reachable) {
            // Switch to next block
            self.builder.switchToBlock(next_block);
            try self.builder.sealBlock(next_block);

            // Push block params as results
            const next_params = self.builder.blockParams(next_block);
            for (next_params) |param| {
                try self.state.push1(param);
            }

            self.state.reachable = true;
        } else {
            self.state.reachable = false;
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
            // For now, emit a series of conditional branches as fallback
            // This matches Cranelift's edge-splitting approach but simplified

            for (targets, 0..) |depth, i| {
                const frame = self.state.getFrameMut(depth);
                frame.setBranchedToExit();
                const destination = frame.brDestination();
                const args = self.state.peekn(jump_args_count);

                // Create comparison: selector == i
                const idx_val = try self.builder.ins().iconst(Type.I32, @intCast(i));
                const cmp = try self.builder.ins().icmp(.eq, selector, idx_val);

                // Create next block for fall-through
                const next_block = try self.builder.createBlock();

                // Conditional branch
                _ = try self.builder.ins().brif(cmp, destination, args, next_block, &[_]Value{});

                self.builder.switchToBlock(next_block);
                try self.builder.sealBlock(next_block);
            }

            // Default case
            const default_frame = self.state.getFrameMut(default);
            default_frame.setBranchedToExit();
            const default_dest = default_frame.brDestination();
            const args = self.state.peekn(jump_args_count);
            _ = try self.builder.ins().jump(default_dest, args);
        }

        self.state.popn(jump_args_count);
        self.state.reachable = false;
    }

    /// Translate a return instruction.
    pub fn translateReturn(self: *Self) !void {
        // Get function frame (bottom of control stack)
        if (self.state.controlStackLen() == 0) {
            // No control frames - just emit return
            _ = try self.builder.ins().return_(&[_]Value{});
            self.state.reachable = false;
            return;
        }

        const func_frame = &self.state.control_stack.items[0];
        const return_count = func_frame.numReturnValues();

        const args = self.state.peekn(return_count);
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
        // For now, treat all as memory-based (future: check mutability from globals array)
        const var_info = try self.env.getOrCreateGlobal(
            self.builder.func,
            global_index,
            global_type,
            false, // is_constant - TODO: check globals[index].mutable
            null, // constant_value
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

    pub fn translateI32Add(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().iadd(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Sub(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().isub(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Mul(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().imul(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32DivS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().sdiv(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32DivU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().udiv(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32RemS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().srem(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32RemU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().urem(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32And(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().band(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Or(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().bor(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Xor(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().bxor(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Shl(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().ishl(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32ShrS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().sshr(args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32ShrU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().ushr(args[0], args[1]);
        try self.state.push1(result);
    }

    // ========================================================================
    // Comparison Translation
    // ========================================================================

    pub fn translateI32Eq(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.eq, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32Ne(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ne, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32LtS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.slt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32LtU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ult, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32GtS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sgt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32GtU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ugt, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32LeS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sle, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32LeU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.ule, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32GeS(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.sge, args[0], args[1]);
        try self.state.push1(result);
    }

    pub fn translateI32GeU(self: *Self) !void {
        const args = self.state.pop2();
        const result = try self.builder.ins().icmp(clif.IntCC.uge, args[0], args[1]);
        try self.state.push1(result);
    }

    /// Translate i32.eqz (compare equal to zero)
    pub fn translateI32Eqz(self: *Self) !void {
        const arg = self.state.pop1();
        const zero = try self.builder.ins().iconst(Type.I32, 0);
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
        const result = try self.builder.ins().select(args[2], args[0], args[1]);
        try self.state.push1(result);
    }

    // ========================================================================
    // Trap Translation
    // ========================================================================

    pub fn translateUnreachable(self: *Self) !void {
        _ = try self.builder.ins().trap(clif.TrapCode.unreachable_code_reached);
        self.state.reachable = false;
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
    try translator.translateI32Add();

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
    try translator.translateI32LtS();

    try testing.expectEqual(@as(usize, 1), translator.state.stackLen());
}
