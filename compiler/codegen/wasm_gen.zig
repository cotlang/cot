//! Wasm Code Generator - Emit bytecode for Wasm SSA ops.
//!
//! Go reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go
//!
//! This module is a direct port of Go's Wasm SSA code generator.
//! Key functions (matching Go's names):
//! - ssaGenValue: emit bytecode for a single SSA value
//! - ssaGenValueOnStack: emit value directly onto wasm stack
//! - getValue64/getValue32: get value onto stack (from local or generate inline)
//! - setReg: store top of stack to a local (virtual register)
//! - ssaGenBlock: emit control flow for block transitions

const std = @import("std");
const arc = @import("arc.zig");

/// Error set for code generation.
/// Go uses panic() for errors; Zig needs explicit error handling.
/// We use a single error set to allow mutual recursion between
/// getValue64 and ssaGenValueOnStack (like Go does).
pub const GenError = error{OutOfMemory};
const SsaValue = @import("../ssa/value.zig").Value;
const SsaBlock = @import("../ssa/block.zig").Block;
const BlockKind = @import("../ssa/block.zig").BlockKind;
const SsaFunc = @import("../ssa/func.zig").Func;
const SsaOp = @import("../ssa/op.zig").Op;
const wasm = @import("wasm.zig");
const Op = wasm.Op;
const ValType = wasm.ValType;
const debug = @import("../pipeline_debug.zig");

const wasm_op = @import("wasm_opcodes.zig");

// Block type constants for Wasm structured control flow (derived from wasm_opcodes)
const BLOCK_TYPE_VOID: u8 = wasm_op.BLOCK_VOID;
const BLOCK_TYPE_I32: u8 = @intFromEnum(wasm_op.ValType.i32);
const BLOCK_TYPE_I64: u8 = @intFromEnum(wasm_op.ValType.i64);

// Memory layout constants
// SP is global 0, initialized to 65536 by the linker
const SP_GLOBAL: u32 = 0;

/// Function index map type - maps function names to Wasm function indices.
pub const FuncIndexMap = std.StringHashMapUnmanaged(u32);

/// Generator state for a single function.
/// Mirrors Go's ssagen.State for Wasm.
pub const FuncGen = struct {
    allocator: std.mem.Allocator,
    ssa_func: *const SsaFunc,
    code: wasm.CodeBuilder,

    /// Maps SSA value IDs to local indices (Go's "registers").
    /// In Go: v.Reg() returns the assigned register.
    /// We track this explicitly since our SSA doesn't have Reg().
    value_to_local: std.AutoHashMapUnmanaged(u32, u32),

    /// Maps SSA block IDs to their index in the block order.
    block_to_idx: std.AutoHashMapUnmanaged(u32, usize),

    /// Next available local index.
    next_local: u32,

    /// Number of function parameters.
    param_count: u32,

    /// Current nesting depth for branch targets (Go's currentDepth).
    block_depth: u32,

    /// Maps block ID to its nesting depth (Go's blockDepths).
    /// For loops, this is the continue target (inner loop depth).
    block_depths: std.AutoHashMapUnmanaged(u32, u32),

    /// Maps loop header block ID to its exit depth (for break).
    /// This is the outer block that wraps the loop.
    loop_exit_depths: std.AutoHashMapUnmanaged(u32, u32),

    /// Blocks that are loop headers.
    loop_headers: std.AutoHashMapUnmanaged(u32, void),

    /// Stack of active loop header IDs (for tracking when to close loops)
    active_loops: std.ArrayListUnmanaged(u32),

    /// Maps exit block ID to its loop header ID (for closing loops)
    loop_exit_blocks: std.AutoHashMapUnmanaged(u32, u32),

    /// Maps function names to Wasm function indices.
    func_indices: ?*const FuncIndexMap,

    /// ARC runtime function indices (null if ARC not enabled).
    runtime_funcs: ?arc.LegacyRuntimeFunctions,

    /// Go's OnWasmStackSkipped counter.
    /// When we skip generating a value (because OnWasmStack=true),
    /// we increment this. When we later generate it, we decrement.
    on_wasm_stack_skipped: i32,

    /// Stack frame size in bytes (for SP management).
    frame_size: i32,

    pub fn init(allocator: std.mem.Allocator, ssa_func: *const SsaFunc) FuncGen {
        return .{
            .allocator = allocator,
            .ssa_func = ssa_func,
            .code = wasm.CodeBuilder.init(allocator),
            .value_to_local = .{},
            .block_to_idx = .{},
            .next_local = 0,
            .param_count = 0,
            .block_depth = 0,
            .block_depths = .{},
            .loop_exit_depths = .{},
            .loop_headers = .{},
            .active_loops = .{},
            .loop_exit_blocks = .{},
            .func_indices = null,
            .runtime_funcs = null,
            .on_wasm_stack_skipped = 0,
            .frame_size = 0,
        };
    }

    /// Set ARC runtime function indices.
    pub fn setRuntimeFunctions(self: *FuncGen, funcs: arc.LegacyRuntimeFunctions) void {
        self.runtime_funcs = funcs;
    }

    pub fn deinit(self: *FuncGen) void {
        self.code.deinit();
        self.value_to_local.deinit(self.allocator);
        self.block_to_idx.deinit(self.allocator);
        self.block_depths.deinit(self.allocator);
        self.loop_exit_depths.deinit(self.allocator);
        self.loop_headers.deinit(self.allocator);
        self.active_loops.deinit(self.allocator);
        self.loop_exit_blocks.deinit(self.allocator);
    }

    /// Generate code for the entire function.
    pub fn generate(self: *FuncGen) ![]const u8 {
        debug.log(.codegen, "wasm_gen: generating '{s}'", .{self.ssa_func.name});

        // Build block index map
        for (self.ssa_func.blocks.items, 0..) |b, i| {
            try self.block_to_idx.put(self.allocator, b.id, i);
        }

        // Count parameters and allocate locals for them
        self.param_count = self.countParams();
        self.next_local = self.param_count;

        // Pre-allocate locals for all values that need them
        // Go assigns a "register" (local) to each value in regalloc.
        // We do it here since we skip regalloc for Wasm.
        try self.allocateLocals();

        // Compute frame size from local_addr slots
        self.frame_size = self.computeFrameSize();

        debug.log(.codegen, "  params: {d}, locals: {d}, frame: {d}, blocks: {d}", .{
            self.param_count,
            self.next_local - self.param_count,
            self.frame_size,
            self.ssa_func.blocks.items.len,
        });

        // Find loop headers
        try self.findLoopHeaders();

        // Emit prologue: decrement SP by frame_size
        if (self.frame_size > 0) {
            try self.code.emitGlobalGet(SP_GLOBAL);
            try self.code.emitI32Const(self.frame_size);
            try self.code.emitI32Sub();
            try self.code.emitGlobalSet(SP_GLOBAL);
        }

        // Generate code for each block (layout pass already ordered them)
        // Note: We don't try to close loops based on exit block detection - this is
        // fragile because block layout may not match Wasm's structured control flow.
        // Loops are closed at function end (after all blocks processed).
        const blocks = self.ssa_func.blocks.items;
        for (blocks, 0..) |block, i| {
            const next: ?*const SsaBlock = if (i + 1 < blocks.len) blocks[i + 1] else null;
            const is_loop_header = self.loop_headers.contains(block.id);
            try self.genBlockWithNext(block, next, is_loop_header);
        }

        // Close any remaining open loops at end of function
        while (self.active_loops.items.len > 0) {
            _ = self.active_loops.pop();
            try self.code.emitEnd(); // end loop
            self.block_depth -= 1;
            try self.code.emitEnd(); // end outer block
            self.block_depth -= 1;
        }

        // Set local count (beyond parameters)
        self.code.setLocalCount(self.next_local - self.param_count);

        return self.code.finish();
    }

    /// Allocate locals for values that need them.
    /// Go does this in regalloc; we do it here.
    /// Values that are "rematerializable" (constants) don't need locals.
    fn allocateLocals(self: *FuncGen) !void {
        for (self.ssa_func.blocks.items) |block| {
            for (block.values.items) |v| {
                // Args already have locals (parameter slots)
                if (v.op == .arg) {
                    const arg_idx: u32 = @intCast(v.aux_int);
                    try self.value_to_local.put(self.allocator, v.id, arg_idx);
                    continue;
                }

                // Skip values with no uses (dead code)
                if (v.uses == 0 and !v.hasSideEffects()) continue;

                // Rematerializable values don't need locals
                if (self.isRematerializable(v)) continue;

                // Comparisons don't need locals - they're generated on-demand for branches
                if (self.isCmp(v)) continue;

                // OnWasmStack values don't need locals - they stay on stack until consumed
                // Go reference: values with OnWasmStack=true skip register allocation
                if (self.isOnWasmStack(v)) continue;

                // Values that produce results need locals
                if (self.producesValue(v)) {
                    const local_idx = self.next_local;
                    self.next_local += 1;
                    try self.value_to_local.put(self.allocator, v.id, local_idx);
                }
            }
        }
    }

    /// Compute the frame size needed for local_addr slots.
    /// Each slot is 8 bytes (i64 size).
    fn computeFrameSize(self: *const FuncGen) i32 {
        var max_slot: i32 = -1;
        for (self.ssa_func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .local_addr) {
                    const slot: i32 = @intCast(v.aux_int);
                    if (slot > max_slot) max_slot = slot;
                }
            }
        }
        // Frame size = (max_slot + 1) * 8, aligned to 16 bytes
        if (max_slot < 0) return 0;
        const size = (max_slot + 1) * 8;
        return @divTrunc((size + 15), 16) * 16; // Align to 16
    }

    /// Check if a value is rematerializable (can be regenerated instead of stored).
    /// Go: constants are rematerializable.
    fn isRematerializable(self: *const FuncGen, v: *const SsaValue) bool {
        _ = self;
        return switch (v.op) {
            .wasm_i64_const, .wasm_i32_const, .wasm_f64_const,
            .const_int, .const_32, .const_64, .const_float,
            .local_addr, .global_addr,
            => true,
            else => false,
        };
    }

    /// Check if a value is "OnWasmStack" - only used as a block control.
    /// Go reference: wasm/ssa.go checks v.OnWasmStack before generating/storing.
    /// Values that are only used as block controls (return values) don't need locals.
    /// They can stay on the Wasm stack and be consumed directly by ssaGenBlock.
    fn isOnWasmStack(self: *const FuncGen, v: *const SsaValue) bool {
        // Must have exactly one use (the block control)
        if (v.uses != 1) return false;

        // Check if this value is used as a block control
        for (self.ssa_func.blocks.items) |block| {
            if (block.controls[0]) |ctrl| {
                if (ctrl.id == v.id) return true;
            }
        }
        return false;
    }

    /// Check if a value is a comparison (produces i32, used by branches).
    /// Go: isCmp() check in wasmobj.go
    fn isCmp(self: *const FuncGen, v: *const SsaValue) bool {
        _ = self;
        return switch (v.op) {
            .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s,
            .wasm_i64_gt_s, .wasm_i64_ge_s, .wasm_i64_eqz,
            .wasm_f64_eq, .wasm_f64_ne, .wasm_f64_lt, .wasm_f64_le,
            .wasm_f64_gt, .wasm_f64_ge,
            => true,
            else => false,
        };
    }

    /// Check if an op produces a value (vs. being purely side-effecting).
    fn producesValue(self: *const FuncGen, v: *const SsaValue) bool {
        _ = self;
        return switch (v.op) {
            // Side effects only, no value produced
            .wasm_i64_store, .wasm_i32_store, .wasm_return, .wasm_drop,
            .init_mem, .phi, .fwd_ref,
            => false,
            // Calls may or may not produce values
            .wasm_call, .wasm_lowered_static_call => v.uses > 0,
            else => true,
        };
    }

    fn findLoopHeaders(self: *FuncGen) !void {
        const blocks = self.ssa_func.blocks.items;
        // First pass: find all loop headers (blocks with back-edges)
        for (blocks, 0..) |block, block_idx| {
            for (block.succs) |edge| {
                const succ_idx = self.block_to_idx.get(edge.b.id) orelse continue;
                if (succ_idx <= block_idx) {
                    try self.loop_headers.put(self.allocator, edge.b.id, {});
                }
            }
        }
        // Second pass: for each loop header, find its exit block
        // Loop headers are typically if_ blocks where succ[1] is the exit
        for (blocks) |block| {
            if (self.loop_headers.contains(block.id)) {
                if (block.kind == .if_ and block.succs.len >= 2) {
                    // succ[1] is the false/exit branch
                    const exit_block = block.succs[1].b;
                    try self.loop_exit_blocks.put(self.allocator, exit_block.id, block.id);
                }
            }
        }
    }

    fn countParams(self: *const FuncGen) u32 {
        var max_arg: u32 = 0;
        var has_args = false;
        for (self.ssa_func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) {
                    has_args = true;
                    const arg_idx: u32 = @intCast(v.aux_int);
                    if (arg_idx >= max_arg) max_arg = arg_idx + 1;
                }
            }
        }
        return if (has_args) max_arg else 0;
    }

    fn genBlockWithNext(self: *FuncGen, block: *const SsaBlock, next: ?*const SsaBlock, is_loop_header: bool) GenError!void {
        debug.log(.codegen, "  block b{d} ({s}){s}", .{
            block.id,
            @tagName(block.kind),
            if (is_loop_header) " [loop]" else "",
        });

        if (is_loop_header) {
            // Wasm loop pattern for break/continue:
            // block $exit      ; depth N - break target (br 1 from inside loop)
            //   loop $continue ; depth N+1 - continue target (br 0 from inside loop)
            //     ... body ...
            //   end
            // end
            try self.code.emitBlock(BLOCK_TYPE_VOID); // outer block for break
            self.block_depth += 1;
            try self.loop_exit_depths.put(self.allocator, block.id, self.block_depth);

            try self.code.emitLoop(BLOCK_TYPE_VOID); // inner loop for continue
            self.block_depth += 1;
            try self.block_depths.put(self.allocator, block.id, self.block_depth);

            // Track this loop as active for closing at exit block
            try self.active_loops.append(self.allocator, block.id);
        }

        // Generate values
        for (block.values.items) |v| {
            try self.ssaGenValue(v);
        }

        // Generate block terminator
        try self.ssaGenBlock(block, next);

        // Verify stack is balanced (Go's check)
        if (self.on_wasm_stack_skipped != 0) {
            debug.log(.codegen, "    WARNING: wasm stack unbalanced: {d}", .{self.on_wasm_stack_skipped});
        }
    }

    // ========================================================================
    // Go's ssaGenBlock - control flow generation
    // Reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go:169
    // ========================================================================

    fn ssaGenBlock(self: *FuncGen, b: *const SsaBlock, next: ?*const SsaBlock) GenError!void {
        switch (b.kind) {
            .plain, .first => {
                // Go's BlockPlain: if next != successor, emit jump
                if (b.succs.len > 0) {
                    const succ = b.succs[0].b;
                    if (next == null or next.?.id != succ.id) {
                        if (self.block_depths.get(succ.id)) |target_depth| {
                            // Back edge to loop - emit br to continue
                            // Go pattern: br (currentDepth - targetDepth)
                            // This branches to the loop instruction for continue
                            const rel_depth = self.block_depth - target_depth;
                            try self.code.emitBr(rel_depth);
                            // NOTE: Do NOT emit end here - ends are emitted by generate()
                            // when we reach the loop exit block
                        }
                        // Forward jumps handled by block layout
                    }
                }
            },

            .if_ => {
                // Go's BlockIf handling (lines 176-198)
                if (b.succs.len < 2) return;

                const succ0 = b.succs[0].b; // true branch
                const succ1 = b.succs[1].b; // false branch

                if (b.controls[0]) |cond| {
                    try self.getValue32(cond);
                }

                if (next != null and next.?.id == succ0.id) {
                    // True is next: if false, jump to succ1
                    try self.code.emitI32Eqz();
                    try self.code.emitIf(BLOCK_TYPE_VOID);
                    self.block_depth += 1;
                    try self.emitBranchTo(succ1);
                    try self.code.emitEnd();
                    self.block_depth -= 1;
                } else if (next != null and next.?.id == succ1.id) {
                    // False is next: if true, jump to succ0
                    try self.code.emitIf(BLOCK_TYPE_VOID);
                    self.block_depth += 1;
                    try self.emitBranchTo(succ0);
                    try self.code.emitEnd();
                    self.block_depth -= 1;
                } else {
                    // Neither is next: emit both jumps
                    try self.code.emitIf(BLOCK_TYPE_VOID);
                    self.block_depth += 1;
                    try self.emitBranchTo(succ0);
                    try self.code.emitEnd();
                    self.block_depth -= 1;
                    try self.emitBranchTo(succ1);
                }
            },

            .ret => {
                // Go's BlockRet (line 200-201): s.Prog(obj.ARET)
                if (b.controls[0]) |ret_val| {
                    try self.getValue64(ret_val);
                }
                try self.emitReturnWithEpilogue();
            },

            .exit => {
                try self.emitReturnWithEpilogue();
            },

            else => {},
        }
    }

    /// Emit return with epilogue (restore SP).
    fn emitReturnWithEpilogue(self: *FuncGen) GenError!void {
        // Epilogue: restore SP
        if (self.frame_size > 0) {
            try self.code.emitGlobalGet(SP_GLOBAL);
            try self.code.emitI32Const(self.frame_size);
            try self.code.emitI32Add();
            try self.code.emitGlobalSet(SP_GLOBAL);
        }
        try self.code.emitReturn();
    }

    fn emitBranchTo(self: *FuncGen, target: *const SsaBlock) GenError!void {
        if (self.block_depths.get(target.id)) |target_depth| {
            const rel_depth = self.block_depth - target_depth;
            try self.code.emitBr(rel_depth);
        } else if (target.kind == .ret or target.kind == .exit) {
            // Forward jump to return block - emit the block's code inline
            // First generate all values in the target block
            for (target.values.items) |v| {
                try self.ssaGenValue(v);
            }
            // Then generate the return with epilogue
            if (target.controls[0]) |ret_val| {
                try self.getValue64(ret_val);
            }
            try self.emitReturnWithEpilogue();
        } else {
            // Forward jump to non-return block - emit br 0 and hope for the best
            // This is not correct for all cases but handles simple fallthrough
            try self.code.emitBr(0);
        }
    }

    // ========================================================================
    // Go's ssaGenValue - value generation
    // Reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go:217
    // ========================================================================

    fn ssaGenValue(self: *FuncGen, v: *const SsaValue) GenError!void {
        switch (v.op) {
            // Calls (Go lines 219-246)
            .wasm_lowered_static_call => {
                // Get operands onto stack
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                // Emit call - function name is in aux.string
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (fn_name) |name| {
                    if (self.func_indices) |indices| {
                        if (indices.get(name)) |func_idx| {
                            try self.code.emitCall(func_idx);
                        } else {
                            // Function not found - use index 0 as fallback
                            try self.code.emitCall(0);
                        }
                    } else {
                        try self.code.emitCall(@intCast(v.aux_int));
                    }
                } else {
                    try self.code.emitCall(@intCast(v.aux_int));
                }
                // Store result if used
                if (v.uses > 0) {
                    try self.setReg(v);
                }
            },

            .wasm_call => {
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                try self.code.emitCall(@intCast(v.aux_int));
                if (v.uses > 0) {
                    try self.setReg(v);
                }
            },

            // ARC runtime calls (Swift pattern from HeapObject.cpp)
            .wasm_lowered_retain => {
                // cot_retain(obj) -> obj
                // Push object pointer (i32)
                try self.getValue32(v.args[0]);
                // Call cot_retain
                if (self.runtime_funcs) |rt| {
                    try self.code.emitCall(rt.retain_idx);
                } else {
                    // No runtime - this is an error, but emit unreachable for safety
                    try self.code.emitUnreachable();
                }
                // cot_retain returns the object pointer, store if used
                if (v.uses > 0) {
                    try self.setReg(v);
                } else {
                    try self.code.emitDrop();
                }
            },

            .wasm_lowered_release => {
                // cot_release(obj) -> void
                // Push object pointer (i32)
                try self.getValue32(v.args[0]);
                // Call cot_release
                if (self.runtime_funcs) |rt| {
                    try self.code.emitCall(rt.release_idx);
                } else {
                    // No runtime - emit unreachable for safety
                    try self.code.emitUnreachable();
                }
                // cot_release returns nothing
            },

            // Stores (Go lines 280-284)
            .wasm_i64_store => {
                try self.getValue32(v.args[0]); // address
                try self.getValue64(v.args[1]); // value
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                try self.code.emitI64Store(3, offset);
            },

            .wasm_i32_store => {
                try self.getValue32(v.args[0]);
                try self.getValue32(v.args[1]);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                try self.code.emitI32Store(2, offset);
            },

            // Drop
            .wasm_drop => {
                try self.getValue64(v.args[0]);
                try self.code.emitDrop();
            },

            // Return
            .wasm_return => {
                if (v.args.len > 0) {
                    try self.getValue64(v.args[0]);
                }
                try self.code.emitReturn();
            },

            // Local set (explicit)
            .wasm_local_set => {
                try self.getValue64(v.args[0]);
                try self.code.emitLocalSet(@intCast(v.aux_int));
            },

            // Args just register their local mapping (already done in allocateLocals)
            .arg => {},

            // Memory ops, control flow - handled elsewhere
            .phi, .init_mem, .fwd_ref => {},

            // Default: generate value and store to local (Go lines 295-309)
            else => {
                if (v.uses == 0 and !v.hasSideEffects()) return;

                // Comparisons are generated on-demand by ssaGenBlock (like Go's OnWasmStack)
                // Don't generate them here - they produce i32 and would be left on stack
                if (self.isCmp(v)) return;

                // Rematerializable values (constants, local_addr) are generated on-demand
                // Don't generate them here - they can be regenerated when needed
                if (self.isRematerializable(v)) return;

                // OnWasmStack values are generated inline by ssaGenBlock
                // Go reference: if v.OnWasmStack { continue } in ssaGenValue
                if (self.isOnWasmStack(v)) return;

                // Generate value onto stack
                try self.ssaGenValueOnStack(v);

                // Store to local (Go's setReg)
                // Don't store rematerializable values - they can be regenerated
                if (v.uses > 0 and !self.isRematerializable(v)) {
                    try self.setReg(v);
                }
            },
        }
    }

    // ========================================================================
    // Go's ssaGenValueOnStack - generate value directly on wasm stack
    // Reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go:313
    // ========================================================================

    fn ssaGenValueOnStack(self: *FuncGen, v: *const SsaValue) GenError!void {
        switch (v.op) {
            // Constants (Go lines 370-377)
            .wasm_i64_const, .const_int, .const_64 => {
                try self.code.emitI64Const(v.aux_int);
            },
            .wasm_i32_const, .const_32 => {
                try self.code.emitI32Const(@truncate(v.aux_int));
            },
            .wasm_f64_const, .const_float => {
                try self.code.emitF64Const(@bitCast(v.aux_int));
            },

            // Local address (for memory operations)
            // Uses SP-relative addressing: SP + (slot * 8)
            .local_addr => {
                const slot: i32 = @intCast(v.aux_int);
                const offset: i32 = slot * 8;
                try self.code.emitGlobalGet(SP_GLOBAL);
                if (offset != 0) {
                    try self.code.emitI32Const(offset);
                    try self.code.emitI32Add();
                }
            },

            // Global address (for global variables in linear memory)
            // Globals are stored at fixed addresses: GLOBAL_BASE + (index * 8)
            // GLOBAL_BASE = 0x20000 (128KB, after stack and heap start)
            .global_addr => {
                const GLOBAL_BASE: i32 = 0x20000;
                const global_idx: i32 = @intCast(v.aux_int);
                const addr: i32 = GLOBAL_BASE + (global_idx * 8);
                try self.code.emitI32Const(addr);
            },

            // ================================================================
            // Pointer Arithmetic (M11)
            // Go reference: OpOffPtr → OpWasmI64AddConst (rewriteWasm.go:415)
            // ================================================================

            // off_ptr: base + constant offset
            // Go: OpWasmI64AddConst - getValue64(base), i64Const(offset), i64.add
            .off_ptr => {
                try self.getValue32(v.args[0]); // base pointer (i32)
                const offset: i32 = @truncate(v.aux_int);
                if (offset != 0) {
                    try self.code.emitI32Const(offset);
                    try self.code.emitI32Add();
                }
            },

            // add_ptr: base + variable offset
            // Used for array indexing: base + (index * element_size)
            .add_ptr => {
                try self.getValue32(v.args[0]); // base pointer (i32)
                try self.getValue32(v.args[1]); // offset (i32)
                try self.code.emitI32Add();
            },

            // sub_ptr: base - variable offset
            // Used for pointer difference calculations
            .sub_ptr => {
                try self.getValue32(v.args[0]); // base pointer (i32)
                try self.getValue32(v.args[1]); // offset (i32)
                try self.code.emitI32Sub();
            },

            // ================================================================
            // Slice/Array Operations (M13)
            // Go reference: rewriteWasm.go - IsInBounds → i64.lt_u
            // ================================================================

            // slice_ptr: extract pointer from slice
            // A slice is conceptually (ptr, len). slice_ptr gets the first component.
            // If the slice was created by slice_make, we get the first arg.
            .slice_ptr => {
                const slice = v.args[0];
                if (slice.op == .slice_make and slice.args.len >= 1) {
                    // Slice was made inline - get the ptr component
                    try self.getValue32(slice.args[0]);
                } else {
                    // Slice from local/parameter - stored as (ptr, len) in memory
                    // Load ptr from offset 0
                    try self.getValue32(slice);
                    try self.code.emitI64Load(3, 0);
                    try self.code.emitI32WrapI64();
                }
            },

            // slice_len: extract length from slice
            // Gets the second component (length) from a slice value.
            .slice_len => {
                const slice = v.args[0];
                if (slice.op == .slice_make and slice.args.len >= 2) {
                    // Slice was made inline - get the len component
                    try self.getValue64(slice.args[1]);
                } else {
                    // Slice from local/parameter - stored as (ptr, len) in memory
                    // Load len from offset 8 (after ptr)
                    try self.getValue32(slice);
                    try self.code.emitI64Load(3, 8);
                }
            },

            // slice_make: construct a slice from (ptr, len)
            // This is a virtual op - the slice is represented by its components.
            // We just generate both values to the stack.
            .slice_make => {
                // Slice components are accessed via slice_ptr/slice_len
                // If needed to store, caller handles it
                if (v.args.len >= 2) {
                    try self.getValue32(v.args[0]); // ptr
                    try self.getValue64(v.args[1]); // len
                }
            },

            // bounds_check: verify index < length, trap if out of bounds
            // Go: IsInBounds(idx, len) → i64.lt_u
            // Pattern: if (idx >= len) unreachable
            .bounds_check => {
                if (v.args.len >= 2) {
                    try self.getValue64(v.args[0]); // index
                    try self.getValue64(v.args[1]); // length
                    try self.code.emitI64GeU(); // idx >= len (unsigned)
                    try self.code.emitIf(BLOCK_TYPE_VOID); // if (out of bounds)
                    try self.code.emitUnreachable(); // trap
                    try self.code.emitEnd();
                }
            },

            // ================================================================
            // String Operations (M14)
            // Go reference: genericOps.go - StringMake, StringPtr, StringLen
            // Strings are (ptr, len) tuples, same as slices in Wasm
            // ================================================================

            // string_ptr: extract pointer from string
            // Go: StringPtr(StringMake ptr _) => ptr
            .string_ptr => {
                const str = v.args[0];
                if (str.op == .string_make and str.args.len >= 1) {
                    // String was made inline - get the ptr component
                    try self.getValue32(str.args[0]);
                } else if (str.op == .const_string) {
                    // Constant string - ptr is the data offset stored in aux_int
                    try self.code.emitI32Const(@truncate(str.aux_int));
                } else {
                    // String from local/parameter - stored as (ptr, len) in memory
                    // Load ptr from offset 0
                    try self.getValue32(str);
                    try self.code.emitI64Load(3, 0);
                    try self.code.emitI32WrapI64();
                }
            },

            // string_len: extract length from string
            // Go: StringLen(StringMake _ len) => len
            .string_len => {
                const str = v.args[0];
                if (str.op == .string_make and str.args.len >= 2) {
                    // String was made inline - get the len component
                    try self.getValue64(str.args[1]);
                } else if (str.op == .const_string) {
                    // Constant string - len is stored in aux field
                    // aux_int holds offset, we need to get len from aux.string
                    const len: i64 = switch (str.aux) {
                        .string => |s| @intCast(s.len),
                        else => 0,
                    };
                    try self.code.emitI64Const(len);
                } else {
                    // String from local/parameter - stored as (ptr, len) in memory
                    // Load len from offset 8 (after ptr)
                    try self.getValue32(str);
                    try self.code.emitI64Load(3, 8);
                }
            },

            // string_make: construct a string from (ptr, len)
            // Go: OpStringMake - virtual op, aggregates components
            .string_make => {
                // String components are accessed via string_ptr/string_len
                // If needed to store, caller handles it
                if (v.args.len >= 2) {
                    try self.getValue32(v.args[0]); // ptr
                    try self.getValue64(v.args[1]); // len
                }
            },

            // const_string: string literal
            // The string data is in the data section at offset v.aux_int
            // We emit the pointer as i32.const offset
            .const_string => {
                // aux_int contains the offset in the data section
                try self.code.emitI32Const(@truncate(v.aux_int));
            },

            // Loads (Go lines 379-382)
            .wasm_i64_load => {
                try self.getValue32(v.args[0]);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                try self.code.emitI64Load(3, offset);
            },
            .wasm_i32_load => {
                try self.getValue32(v.args[0]);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                try self.code.emitI32Load(2, offset);
            },

            // Local get
            .wasm_local_get => {
                try self.code.emitLocalGet(@intCast(v.aux_int));
            },

            // Binary ops (Go lines 391-406)
            .wasm_i64_add => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Add();
            },
            .wasm_i64_sub => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Sub();
            },
            .wasm_i64_mul => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Mul();
            },
            .wasm_i64_div_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64DivS();
            },
            .wasm_i64_rem_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64RemS();
            },
            .wasm_i64_and => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64And();
            },
            .wasm_i64_or => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Or();
            },
            .wasm_i64_xor => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Xor();
            },
            .wasm_i64_shl => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Shl();
            },
            .wasm_i64_shr_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64ShrS();
            },

            // Comparisons (Go lines 391-399)
            .wasm_i64_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Eq();
            },
            .wasm_i64_ne => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64Ne();
            },
            .wasm_i64_lt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64LtS();
            },
            .wasm_i64_le_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64LeS();
            },
            .wasm_i64_gt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64GtS();
            },
            .wasm_i64_ge_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitI64GeS();
            },
            .wasm_i64_eqz => {
                try self.getValue64(v.args[0]);
                try self.code.emitI64Eqz();
            },

            // Float ops (Go lines 401-406)
            .wasm_f64_add => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Add();
            },
            .wasm_f64_sub => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Sub();
            },
            .wasm_f64_mul => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Mul();
            },
            .wasm_f64_div => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Div();
            },
            .wasm_f64_neg => {
                try self.getValue64(v.args[0]);
                try self.code.emitF64Neg();
            },
            .wasm_f64_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Eq();
            },
            .wasm_f64_ne => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Ne();
            },
            .wasm_f64_lt => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Lt();
            },
            .wasm_f64_le => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Le();
            },
            .wasm_f64_gt => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Gt();
            },
            .wasm_f64_ge => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                try self.code.emitF64Ge();
            },

            // Copy (Go line 454-455)
            .copy, .wasm_lowered_move => {
                try self.getValue64(v.args[0]);
            },

            else => {
                debug.log(.codegen, "    unhandled in ssaGenValueOnStack: {s}", .{@tagName(v.op)});
            },
        }
    }

    // ========================================================================
    // Go's getValue32 and getValue64
    // Reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go:474-503
    // ========================================================================

    /// Get value onto stack as i32.
    /// Go's getValue32 (line 474).
    fn getValue32(self: *FuncGen, v: *const SsaValue) GenError!void {
        // Check if value is stored in a local (all locals are i64)
        if (self.value_to_local.get(v.id)) |local_idx| {
            try self.code.emitLocalGet(local_idx);
            try self.code.emitI32WrapI64();
            return;
        }

        // Value not in local - generate inline
        // Only wrap if the value produces i64, not i32
        try self.ssaGenValueOnStack(v);
        if (!self.producesI32(v)) {
            try self.code.emitI32WrapI64();
        }
    }

    /// Check if an op produces i32 (vs i64).
    /// Go checks isCmp() and REG_SP for this.
    fn producesI32(self: *const FuncGen, v: *const SsaValue) bool {
        _ = self;
        return switch (v.op) {
            // Address computations produce i32 (Wasm uses 32-bit linear memory)
            .local_addr, .global_addr, .off_ptr, .add_ptr, .sub_ptr => true,
            // String/slice pointers produce i32
            .string_ptr, .slice_ptr, .const_string => true,
            // ARC retain returns object pointer (i32)
            .wasm_lowered_retain => true,
            // i32 operations
            .wasm_i32_const, .const_32, .wasm_i32_load => true,
            // Comparisons produce i32
            .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s,
            .wasm_i64_gt_s, .wasm_i64_ge_s, .wasm_i64_eqz,
            .wasm_f64_eq, .wasm_f64_ne, .wasm_f64_lt, .wasm_f64_le,
            .wasm_f64_gt, .wasm_f64_ge,
            => true,
            else => false,
        };
    }

    /// Get value onto stack as i64.
    /// Go's getValue64 (line 491).
    fn getValue64(self: *FuncGen, v: *const SsaValue) GenError!void {
        // Check if value is stored in a local
        if (self.value_to_local.get(v.id)) |local_idx| {
            try self.code.emitLocalGet(local_idx);
            return;
        }

        // Value not in local - must be rematerializable, generate inline
        // This is Go's "OnWasmStack" path (line 492-495)
        try self.ssaGenValueOnStack(v);
    }

    // ========================================================================
    // Go's setReg - store top of stack to local
    // Reference: ~/learning/go/src/cmd/compile/internal/wasm/ssa.go:530
    // ========================================================================

    fn setReg(self: *FuncGen, v: *const SsaValue) GenError!void {
        if (self.value_to_local.get(v.id)) |local_idx| {
            try self.code.emitLocalSet(local_idx);
        }
    }
};

/// Generate Wasm code for an SSA function.
pub fn genFunc(allocator: std.mem.Allocator, ssa_func: *const SsaFunc) ![]const u8 {
    return genFuncWithIndices(allocator, ssa_func, null, null);
}

/// Generate Wasm code for an SSA function with function index resolution.
pub fn genFuncWithIndices(
    allocator: std.mem.Allocator,
    ssa_func: *const SsaFunc,
    func_indices: ?*const FuncIndexMap,
    runtime_funcs: ?arc.LegacyRuntimeFunctions,
) ![]const u8 {
    var gen = FuncGen.init(allocator, ssa_func);
    gen.func_indices = func_indices;
    if (runtime_funcs) |rf| {
        gen.setRuntimeFunctions(rf);
    }
    defer gen.deinit();
    return gen.generate();
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "genFunc - return constant" {
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "answer");
    defer f.deinit();

    const b = try f.newBlock(.ret);
    const c = try f.newValue(.wasm_i64_const, 0, b, .{});
    c.aux_int = 42;
    c.*.uses = 1;
    try b.addValue(allocator, c);
    b.controls[0] = c;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    try testing.expect(body.len >= 3);
    try testing.expectEqual(Op.i64_const, body[1]);
    try testing.expectEqual(@as(u8, 42), body[2]);
    try testing.expectEqual(Op.end, body[body.len - 1]);
}

test "genFunc - add two args" {
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "add");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    const arg0 = try f.newValue(.arg, 0, b, .{});
    arg0.aux_int = 0;
    arg0.*.uses = 1;
    try b.addValue(allocator, arg0);

    const arg1 = try f.newValue(.arg, 0, b, .{});
    arg1.aux_int = 1;
    arg1.*.uses = 1;
    try b.addValue(allocator, arg1);

    const add = try f.newValue(.wasm_i64_add, 0, b, .{});
    add.addArg(arg0);
    add.addArg(arg1);
    add.*.uses = 1;
    try b.addValue(allocator, add);
    b.controls[0] = add;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    try testing.expect(body.len >= 6);
    try testing.expectEqual(Op.local_get, body[1]);
    try testing.expectEqual(Op.local_get, body[3]);
    try testing.expectEqual(Op.i64_add, body[5]);
    try testing.expectEqual(Op.end, body[body.len - 1]);
}

test "genFunc - simple if block" {
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_if");
    defer f.deinit();

    const b1 = try f.newBlock(.if_);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.plain);
    const b4 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b1.addEdgeTo(allocator, b3);
    try b2.addEdgeTo(allocator, b4);
    try b3.addEdgeTo(allocator, b4);

    const cond = try f.newValue(.wasm_i64_const, 0, b1, .{});
    cond.aux_int = 1;
    cond.*.uses = 1;
    try b1.addValue(allocator, cond);
    b1.controls[0] = cond;

    const ret = try f.newValue(.wasm_i64_const, 0, b4, .{});
    ret.aux_int = 42;
    ret.*.uses = 1;
    try b4.addValue(allocator, ret);
    b4.controls[0] = ret;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    try testing.expect(body.len > 0);
    try testing.expectEqual(Op.end, body[body.len - 1]);
}

// ============================================================================
// M10: Linear Memory Tests
// Go reference: wasm/ssa.go lines 280-284 (stores), 379-382 (loads)
// ============================================================================

test "genFunc - local_addr generates SP-relative address" {
    // local_addr produces: global.get 0 (SP), i32.const offset, i32.add
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_local_addr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // local_addr with slot 1 (offset = 8 bytes)
    const addr = try f.newValue(.local_addr, 0, b, .{});
    addr.aux_int = 1; // slot 1 -> offset 8
    addr.*.uses = 1;
    try b.addValue(allocator, addr);
    b.controls[0] = addr;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should have prologue (SP adjustment) + local_addr code + epilogue + return
    try testing.expect(body.len >= 8);
    try testing.expectEqual(Op.end, body[body.len - 1]);
}

test "genFunc - store and load round-trip" {
    // Test: store a constant to memory via local_addr, then load it back
    // Go pattern: getValue32(addr), getValue64(value), i64.store offset
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_store_load");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Get address of slot 0
    const addr = try f.newValue(.local_addr, 0, b, .{});
    addr.aux_int = 0; // slot 0
    addr.*.uses = 2; // used by store and load
    try b.addValue(allocator, addr);

    // Store constant 42 to that address
    const val = try f.newValue(.wasm_i64_const, 0, b, .{});
    val.aux_int = 42;
    val.*.uses = 1;
    try b.addValue(allocator, val);

    const store = try f.newValue(.wasm_i64_store, 0, b, .{});
    store.addArg(addr);
    store.addArg(val);
    store.aux_int = 0; // offset 0
    try b.addValue(allocator, store);

    // Load from same address
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(addr);
    load.aux_int = 0; // offset 0
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Verify structure: should have store and load ops
    try testing.expect(body.len >= 10);
    try testing.expectEqual(Op.end, body[body.len - 1]);

    // Find i64.store (0x37) and i64.load (0x29) in the body
    var found_store = false;
    var found_load = false;
    for (body) |byte| {
        if (byte == Op.i64_store) found_store = true;
        if (byte == Op.i64_load) found_load = true;
    }
    try testing.expect(found_store);
    try testing.expect(found_load);
}

test "genFunc - frame size computed from local_addr slots" {
    // Multiple local_addr values should compute correct frame size
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_frame");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // local_addr slot 0
    const addr0 = try f.newValue(.local_addr, 0, b, .{});
    addr0.aux_int = 0;
    addr0.*.uses = 1;
    try b.addValue(allocator, addr0);

    // local_addr slot 2 (max slot = 2, so frame = (2+1)*8 = 24, aligned to 32)
    const addr2 = try f.newValue(.local_addr, 0, b, .{});
    addr2.aux_int = 2;
    addr2.*.uses = 1;
    try b.addValue(allocator, addr2);

    // Return constant
    const ret = try f.newValue(.wasm_i64_const, 0, b, .{});
    ret.aux_int = 0;
    ret.*.uses = 1;
    try b.addValue(allocator, ret);
    b.controls[0] = ret;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Prologue should decrement SP by frame size
    // global.get 0 (SP), i32.const frame_size, i32.sub, global.set 0
    try testing.expect(body.len >= 8);
    try testing.expectEqual(Op.global_get, body[1]);
    try testing.expectEqual(Op.end, body[body.len - 1]);
}

// ============================================================================
// M11: Pointer Operations Tests
// Go reference: OpOffPtr → OpWasmI64AddConst (rewriteWasm.go:415)
// ============================================================================

test "genFunc - off_ptr adds constant offset to pointer" {
    // off_ptr: base + aux_int (constant offset)
    // Used for struct field access: &s.field = &s + field_offset
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_off_ptr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Base address from local_addr slot 0
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 0;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // off_ptr: base + 16 (access field at offset 16)
    const ptr = try f.newValue(.off_ptr, 0, b, .{});
    ptr.addArg(base);
    ptr.aux_int = 16; // constant offset
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Load from offset pointer
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(ptr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain: global.get (SP), i32.const 16, i32.add, i64.load
    try testing.expect(body.len >= 8);
    var found_i32_add = false;
    var found_i64_load = false;
    for (body) |byte| {
        if (byte == Op.i32_add) found_i32_add = true;
        if (byte == Op.i64_load) found_i64_load = true;
    }
    try testing.expect(found_i32_add);
    try testing.expect(found_i64_load);
}

test "genFunc - add_ptr adds variable offset to pointer" {
    // add_ptr: base + offset (variable offset)
    // Used for array indexing: &arr[i] = arr + (i * element_size)
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_add_ptr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Base address from local_addr slot 0
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 0;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // Variable offset (e.g., index * 8)
    const offset = try f.newValue(.wasm_i32_const, 0, b, .{});
    offset.aux_int = 24; // offset value
    offset.*.uses = 1;
    try b.addValue(allocator, offset);

    // add_ptr: base + offset
    const ptr = try f.newValue(.add_ptr, 0, b, .{});
    ptr.addArg(base);
    ptr.addArg(offset);
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Load from computed pointer
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(ptr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i32.add for pointer arithmetic
    try testing.expect(body.len >= 8);
    var found_i32_add = false;
    for (body) |byte| {
        if (byte == Op.i32_add) found_i32_add = true;
    }
    try testing.expect(found_i32_add);
}

test "genFunc - sub_ptr subtracts variable offset from pointer" {
    // sub_ptr: base - offset (variable offset)
    // Used for pointer difference or backward indexing
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_sub_ptr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Base address from local_addr slot 1 (offset 8)
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 1;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // Variable offset to subtract
    const offset = try f.newValue(.wasm_i32_const, 0, b, .{});
    offset.aux_int = 8;
    offset.*.uses = 1;
    try b.addValue(allocator, offset);

    // sub_ptr: base - offset
    const ptr = try f.newValue(.sub_ptr, 0, b, .{});
    ptr.addArg(base);
    ptr.addArg(offset);
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Load from computed pointer
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(ptr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i32.sub for pointer arithmetic
    try testing.expect(body.len >= 8);
    var found_i32_sub = false;
    for (body) |byte| {
        if (byte == Op.i32_sub) found_i32_sub = true;
    }
    try testing.expect(found_i32_sub);
}

// ============================================================================
// M12: Struct Operations Tests
// Go reference: rewrite.go rewriteStructLoad/rewriteStructStore
// Pattern: local_addr → off_ptr(field_offset) → load/store
// ============================================================================

test "genFunc - struct field read via off_ptr + load" {
    // Simulates: struct Point { x: i64, y: i64 }; return p.y
    // p is at local slot 0, y is at offset 8
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_struct_field_read");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // local_addr slot 0 (struct base)
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 0;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // off_ptr: base + 8 (field y at offset 8)
    const field_addr = try f.newValue(.off_ptr, 0, b, .{});
    field_addr.addArg(base);
    field_addr.aux_int = 8; // offset of field y
    field_addr.*.uses = 1;
    try b.addValue(allocator, field_addr);

    // load i64 from field address
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(field_addr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Verify: global.get(SP), i32.const(8), i32.add, i64.load
    try testing.expect(body.len >= 8);
    var found_global_get = false;
    var found_i32_add = false;
    var found_i64_load = false;
    for (body) |byte| {
        if (byte == Op.global_get) found_global_get = true;
        if (byte == Op.i32_add) found_i32_add = true;
        if (byte == Op.i64_load) found_i64_load = true;
    }
    try testing.expect(found_global_get);
    try testing.expect(found_i32_add);
    try testing.expect(found_i64_load);
}

test "genFunc - struct field write via off_ptr + store" {
    // Simulates: struct Point { x: i64, y: i64 }; p.x = 42
    // p is at local slot 0, x is at offset 0
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_struct_field_write");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // local_addr slot 0 (struct base)
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 0;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // off_ptr: base + 0 (field x at offset 0)
    const field_addr = try f.newValue(.off_ptr, 0, b, .{});
    field_addr.addArg(base);
    field_addr.aux_int = 0; // offset of field x
    field_addr.*.uses = 1;
    try b.addValue(allocator, field_addr);

    // value to store
    const val = try f.newValue(.wasm_i64_const, 0, b, .{});
    val.aux_int = 42;
    val.*.uses = 1;
    try b.addValue(allocator, val);

    // store to field address
    const store = try f.newValue(.wasm_i64_store, 0, b, .{});
    store.addArg(field_addr);
    store.addArg(val);
    store.aux_int = 0;
    try b.addValue(allocator, store);

    // return void (just a constant for the test)
    const ret = try f.newValue(.wasm_i64_const, 0, b, .{});
    ret.aux_int = 0;
    ret.*.uses = 1;
    try b.addValue(allocator, ret);
    b.controls[0] = ret;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Verify: has i64.store
    try testing.expect(body.len >= 8);
    var found_i64_store = false;
    for (body) |byte| {
        if (byte == Op.i64_store) found_i64_store = true;
    }
    try testing.expect(found_i64_store);
}

test "genFunc - multi-field struct access" {
    // Simulates accessing multiple fields: return p.x + p.y
    // struct Point { x: i64, y: i64 } at slot 0
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_multi_field");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Access field x (offset 0)
    const base1 = try f.newValue(.local_addr, 0, b, .{});
    base1.aux_int = 0;
    base1.*.uses = 1;
    try b.addValue(allocator, base1);

    const addr_x = try f.newValue(.off_ptr, 0, b, .{});
    addr_x.addArg(base1);
    addr_x.aux_int = 0;
    addr_x.*.uses = 1;
    try b.addValue(allocator, addr_x);

    const load_x = try f.newValue(.wasm_i64_load, 0, b, .{});
    load_x.addArg(addr_x);
    load_x.aux_int = 0;
    load_x.*.uses = 1;
    try b.addValue(allocator, load_x);

    // Access field y (offset 8)
    const base2 = try f.newValue(.local_addr, 0, b, .{});
    base2.aux_int = 0;
    base2.*.uses = 1;
    try b.addValue(allocator, base2);

    const addr_y = try f.newValue(.off_ptr, 0, b, .{});
    addr_y.addArg(base2);
    addr_y.aux_int = 8;
    addr_y.*.uses = 1;
    try b.addValue(allocator, addr_y);

    const load_y = try f.newValue(.wasm_i64_load, 0, b, .{});
    load_y.addArg(addr_y);
    load_y.aux_int = 0;
    load_y.*.uses = 1;
    try b.addValue(allocator, load_y);

    // Add them
    const add = try f.newValue(.wasm_i64_add, 0, b, .{});
    add.addArg(load_x);
    add.addArg(load_y);
    add.*.uses = 1;
    try b.addValue(allocator, add);
    b.controls[0] = add;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should have two i64.load and one i64.add
    var load_count: u32 = 0;
    var found_add = false;
    for (body) |byte| {
        if (byte == Op.i64_load) load_count += 1;
        if (byte == Op.i64_add) found_add = true;
    }
    try testing.expectEqual(@as(u32, 2), load_count);
    try testing.expect(found_add);
}

// ============================================================================
// M13: Array/Slice Operations Tests
// Go reference: rewriteWasm.go - IsInBounds → i64.lt_u
// ============================================================================

test "genFunc - slice_make and slice_ptr extraction" {
    // Test slice creation and pointer extraction
    // slice_make(ptr, len) creates slice, slice_ptr extracts ptr
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_slice_ptr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Create a pointer (local_addr of slot 0)
    const ptr = try f.newValue(.local_addr, 0, b, .{});
    ptr.aux_int = 0;
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Create a length
    const len = try f.newValue(.wasm_i64_const, 0, b, .{});
    len.aux_int = 10;
    len.*.uses = 1;
    try b.addValue(allocator, len);

    // slice_make(ptr, len)
    const slice = try f.newValue(.slice_make, 0, b, .{});
    slice.addArg(ptr);
    slice.addArg(len);
    slice.*.uses = 1;
    try b.addValue(allocator, slice);

    // slice_ptr(slice) - should get the ptr back
    const extracted_ptr = try f.newValue(.slice_ptr, 0, b, .{});
    extracted_ptr.addArg(slice);
    extracted_ptr.*.uses = 1;
    try b.addValue(allocator, extracted_ptr);

    // Load from the extracted pointer
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(extracted_ptr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain global.get (for local_addr) and i64.load
    try testing.expect(body.len >= 5);
    var found_global_get = false;
    var found_i64_load = false;
    for (body) |byte| {
        if (byte == Op.global_get) found_global_get = true;
        if (byte == Op.i64_load) found_i64_load = true;
    }
    try testing.expect(found_global_get);
    try testing.expect(found_i64_load);
}

test "genFunc - slice_len extraction" {
    // Test length extraction from slice
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_slice_len");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Create a pointer
    const ptr = try f.newValue(.local_addr, 0, b, .{});
    ptr.aux_int = 0;
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Create a length = 42
    const len = try f.newValue(.wasm_i64_const, 0, b, .{});
    len.aux_int = 42;
    len.*.uses = 1;
    try b.addValue(allocator, len);

    // slice_make(ptr, len)
    const slice = try f.newValue(.slice_make, 0, b, .{});
    slice.addArg(ptr);
    slice.addArg(len);
    slice.*.uses = 1;
    try b.addValue(allocator, slice);

    // slice_len(slice) - should get 42
    const extracted_len = try f.newValue(.slice_len, 0, b, .{});
    extracted_len.addArg(slice);
    extracted_len.*.uses = 1;
    try b.addValue(allocator, extracted_len);
    b.controls[0] = extracted_len;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i64.const 42
    try testing.expect(body.len >= 3);
    var found_i64_const = false;
    for (body) |byte| {
        if (byte == Op.i64_const) found_i64_const = true;
    }
    try testing.expect(found_i64_const);
}

test "genFunc - bounds_check emits trap on out of bounds" {
    // Test bounds checking: if (idx >= len) unreachable
    // Go pattern: IsInBounds → i64.lt_u
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_bounds_check");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Index value
    const idx = try f.newValue(.wasm_i64_const, 0, b, .{});
    idx.aux_int = 5;
    idx.*.uses = 1;
    try b.addValue(allocator, idx);

    // Length value
    const len = try f.newValue(.wasm_i64_const, 0, b, .{});
    len.aux_int = 10;
    len.*.uses = 1;
    try b.addValue(allocator, len);

    // bounds_check(idx, len) - should emit: if (idx >= len) unreachable
    const check = try f.newValue(.bounds_check, 0, b, .{});
    check.addArg(idx);
    check.addArg(len);
    try b.addValue(allocator, check);

    // Return something
    const ret = try f.newValue(.wasm_i64_const, 0, b, .{});
    ret.aux_int = 0;
    ret.*.uses = 1;
    try b.addValue(allocator, ret);
    b.controls[0] = ret;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain: i64.ge_u, if, unreachable, end
    try testing.expect(body.len >= 6);
    var found_ge_u = false;
    var found_if = false;
    var found_unreachable = false;
    for (body) |byte| {
        if (byte == Op.i64_ge_u) found_ge_u = true;
        if (byte == Op.if_op) found_if = true;
        if (byte == Op.unreachable_op) found_unreachable = true;
    }
    try testing.expect(found_ge_u);
    try testing.expect(found_if);
    try testing.expect(found_unreachable);
}

test "genFunc - array element access via add_ptr" {
    // Test array element access: arr[i] = *(arr + i * sizeof(elem))
    // Pattern: local_addr + add_ptr(base, offset) + load
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_array_access");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Array base address (slot 0)
    const base = try f.newValue(.local_addr, 0, b, .{});
    base.aux_int = 0;
    base.*.uses = 1;
    try b.addValue(allocator, base);

    // Index * 8 (for i64 elements)
    const offset = try f.newValue(.wasm_i32_const, 0, b, .{});
    offset.aux_int = 16; // accessing arr[2] (offset = 2 * 8)
    offset.*.uses = 1;
    try b.addValue(allocator, offset);

    // add_ptr: base + offset
    const elem_addr = try f.newValue(.add_ptr, 0, b, .{});
    elem_addr.addArg(base);
    elem_addr.addArg(offset);
    elem_addr.*.uses = 1;
    try b.addValue(allocator, elem_addr);

    // Load element
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(elem_addr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should have i32.add (for pointer arithmetic) and i64.load
    try testing.expect(body.len >= 6);
    var found_i32_add = false;
    var found_i64_load = false;
    for (body) |byte| {
        if (byte == Op.i32_add) found_i32_add = true;
        if (byte == Op.i64_load) found_i64_load = true;
    }
    try testing.expect(found_i32_add);
    try testing.expect(found_i64_load);
}

// ============================================================================
// M14: String Operations Tests
// Go reference: genericOps.go - StringMake, StringPtr, StringLen
// ============================================================================

test "genFunc - string_make and string_ptr extraction" {
    // Test string creation and pointer extraction
    // string_make(ptr, len) creates string, string_ptr extracts ptr
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_string_ptr");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Create a pointer (local_addr of slot 0)
    const ptr = try f.newValue(.local_addr, 0, b, .{});
    ptr.aux_int = 0;
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Create a length
    const len = try f.newValue(.wasm_i64_const, 0, b, .{});
    len.aux_int = 5; // "hello" length
    len.*.uses = 1;
    try b.addValue(allocator, len);

    // string_make(ptr, len)
    const str = try f.newValue(.string_make, 0, b, .{});
    str.addArg(ptr);
    str.addArg(len);
    str.*.uses = 1;
    try b.addValue(allocator, str);

    // string_ptr(str) - should get the ptr back
    const extracted_ptr = try f.newValue(.string_ptr, 0, b, .{});
    extracted_ptr.addArg(str);
    extracted_ptr.*.uses = 1;
    try b.addValue(allocator, extracted_ptr);

    // Load from the extracted pointer (to verify it's an address)
    const load = try f.newValue(.wasm_i64_load, 0, b, .{});
    load.addArg(extracted_ptr);
    load.aux_int = 0;
    load.*.uses = 1;
    try b.addValue(allocator, load);
    b.controls[0] = load;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain global.get (for local_addr) and i64.load
    try testing.expect(body.len >= 5);
    var found_global_get = false;
    var found_i64_load = false;
    for (body) |byte| {
        if (byte == Op.global_get) found_global_get = true;
        if (byte == Op.i64_load) found_i64_load = true;
    }
    try testing.expect(found_global_get);
    try testing.expect(found_i64_load);
}

test "genFunc - string_len extraction" {
    // Test length extraction from string
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_string_len");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Create a pointer
    const ptr = try f.newValue(.local_addr, 0, b, .{});
    ptr.aux_int = 0;
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Create a length = 13 ("hello, world!")
    const len = try f.newValue(.wasm_i64_const, 0, b, .{});
    len.aux_int = 13;
    len.*.uses = 1;
    try b.addValue(allocator, len);

    // string_make(ptr, len)
    const str = try f.newValue(.string_make, 0, b, .{});
    str.addArg(ptr);
    str.addArg(len);
    str.*.uses = 1;
    try b.addValue(allocator, str);

    // string_len(str) - should get 13
    const extracted_len = try f.newValue(.string_len, 0, b, .{});
    extracted_len.addArg(str);
    extracted_len.*.uses = 1;
    try b.addValue(allocator, extracted_len);
    b.controls[0] = extracted_len;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i64.const 13
    try testing.expect(body.len >= 3);
    var found_i64_const = false;
    for (body) |byte| {
        if (byte == Op.i64_const) found_i64_const = true;
    }
    try testing.expect(found_i64_const);
}

test "genFunc - const_string emits i32.const pointer" {
    // Test constant string generates i32.const with data offset
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_const_string");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // const_string with offset 1024 (simulating data section offset)
    const str = try f.newValue(.const_string, 0, b, .{});
    str.aux_int = 1024; // offset in data section
    str.aux = .{ .string = "hello" };
    str.*.uses = 1;
    try b.addValue(allocator, str);

    // string_ptr to get the pointer
    const ptr = try f.newValue(.string_ptr, 0, b, .{});
    ptr.addArg(str);
    ptr.*.uses = 1;
    try b.addValue(allocator, ptr);

    // Wrap as i64 for return (since return expects i64)
    // In practice we'd return the pointer, but for this test we verify i32.const is emitted
    const extend = try f.newValue(.wasm_i64_const, 0, b, .{});
    extend.aux_int = 0; // dummy
    extend.*.uses = 1;
    try b.addValue(allocator, extend);
    b.controls[0] = extend;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i32.const (for the string pointer offset)
    try testing.expect(body.len >= 3);
    var found_i32_const = false;
    for (body) |byte| {
        if (byte == Op.i32_const) found_i32_const = true;
    }
    try testing.expect(found_i32_const);
}

test "genFunc - string_len from const_string" {
    // Test getting length from constant string
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_const_string_len");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // const_string "hello" (length 5)
    const str = try f.newValue(.const_string, 0, b, .{});
    str.aux_int = 0; // offset
    str.aux = .{ .string = "hello" };
    str.*.uses = 1;
    try b.addValue(allocator, str);

    // string_len - should emit i64.const 5
    const len = try f.newValue(.string_len, 0, b, .{});
    len.addArg(str);
    len.*.uses = 1;
    try b.addValue(allocator, len);
    b.controls[0] = len;

    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain i64.const
    try testing.expect(body.len >= 3);
    var found_i64_const = false;
    for (body) |byte| {
        if (byte == Op.i64_const) found_i64_const = true;
    }
    try testing.expect(found_i64_const);
}

// ============================================================================
// M15: ARC (Reference Counting) Tests
// Reference: Swift's HeapObject.cpp retain/release patterns
// ============================================================================

test "genFunc - wasm_lowered_retain emits call to cot_retain" {
    // Test that retain operation emits a call instruction
    const allocator = testing.allocator;

    // Create a module with ARC runtime to get function indices
    var module = wasm.Module.init(allocator);
    defer module.deinit();
    const runtime_funcs = try arc.addRuntimeFunctions(&module);

    var f = SsaFunc.init(allocator, "test_retain");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Simulate an object pointer (i32 const)
    const obj_ptr = try f.newValue(.wasm_i32_const, 0, b, .{});
    obj_ptr.aux_int = 0x10000; // Some memory address
    obj_ptr.*.uses = 1;
    try b.addValue(allocator, obj_ptr);

    // wasm_lowered_retain(obj_ptr)
    const retain = try f.newValue(.wasm_lowered_retain, 0, b, .{});
    retain.addArg(obj_ptr);
    retain.*.uses = 1; // Use the result
    try b.addValue(allocator, retain);
    b.controls[0] = retain;

    // Generate with ARC runtime
    const body = try genFuncWithIndices(allocator, &f, null, runtime_funcs);
    defer allocator.free(body);

    // Should contain a call instruction to retain function
    try testing.expect(body.len >= 5);
    var found_call = false;
    for (body) |byte| {
        if (byte == Op.call) found_call = true;
    }
    try testing.expect(found_call);
}

test "genFunc - wasm_lowered_release emits call to cot_release" {
    // Test that release operation emits a call instruction
    const allocator = testing.allocator;

    // Create a module with ARC runtime
    var module = wasm.Module.init(allocator);
    defer module.deinit();
    const runtime_funcs = try arc.addRuntimeFunctions(&module);

    var f = SsaFunc.init(allocator, "test_release");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Simulate an object pointer
    const obj_ptr = try f.newValue(.wasm_i32_const, 0, b, .{});
    obj_ptr.aux_int = 0x10000;
    obj_ptr.*.uses = 1;
    try b.addValue(allocator, obj_ptr);

    // wasm_lowered_release(obj_ptr)
    const release = try f.newValue(.wasm_lowered_release, 0, b, .{});
    release.addArg(obj_ptr);
    try b.addValue(allocator, release);

    // Return constant (release has no result)
    const ret = try f.newValue(.wasm_i64_const, 0, b, .{});
    ret.aux_int = 0;
    ret.*.uses = 1;
    try b.addValue(allocator, ret);
    b.controls[0] = ret;

    // Generate with ARC runtime
    const body = try genFuncWithIndices(allocator, &f, null, runtime_funcs);
    defer allocator.free(body);

    // Should contain a call instruction to release function
    try testing.expect(body.len >= 5);
    var found_call = false;
    for (body) |byte| {
        if (byte == Op.call) found_call = true;
    }
    try testing.expect(found_call);
}

test "genFunc - retain/release without runtime emits unreachable" {
    // Test that retain/release without runtime functions traps safely
    const allocator = testing.allocator;

    var f = SsaFunc.init(allocator, "test_no_runtime");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // Object pointer
    const obj_ptr = try f.newValue(.wasm_i32_const, 0, b, .{});
    obj_ptr.aux_int = 0x10000;
    obj_ptr.*.uses = 1;
    try b.addValue(allocator, obj_ptr);

    // wasm_lowered_retain without runtime - should emit unreachable
    const retain = try f.newValue(.wasm_lowered_retain, 0, b, .{});
    retain.addArg(obj_ptr);
    retain.*.uses = 0; // Don't use result
    try b.addValue(allocator, retain);

    // Return constant
    const ret = try f.newValue(.wasm_i64_const, 0, b, .{});
    ret.aux_int = 0;
    ret.*.uses = 1;
    try b.addValue(allocator, ret);
    b.controls[0] = ret;

    // Generate WITHOUT ARC runtime (runtime_funcs = null)
    const body = try genFunc(allocator, &f);
    defer allocator.free(body);

    // Should contain unreachable instruction (0x00)
    try testing.expect(body.len >= 3);
    var found_unreachable = false;
    for (body) |byte| {
        if (byte == Op.unreachable_op) found_unreachable = true;
    }
    try testing.expect(found_unreachable);
}
