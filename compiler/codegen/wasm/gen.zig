//! SSA Code Generator for WebAssembly
//!
//! Go reference: cmd/compile/internal/wasm/ssa.go
//!
//! This generates Prog chains from SSA values and blocks.
//! Key functions matching Go:
//! - ssaGenValue: Generate code for a single SSA value
//! - ssaGenBlock: Generate code for block control flow (emits pseudo-jumps)
//! - getValue32/getValue64: Get value onto wasm stack
//! - setReg: Store value to register (local)
//!
//! IMPORTANT: This emits PSEUDO-JUMPS (obj.AJMP) not real branches.
//! The preprocess pass later transforms these into the dispatch loop pattern.

const std = @import("std");
const c = @import("constants.zig");
const prog_mod = @import("prog.zig");
const Prog = prog_mod.Prog;
const Addr = prog_mod.Addr;
const Symbol = prog_mod.Symbol;
const ProgBuilder = prog_mod.ProgBuilder;

// Import real SSA types from compiler/ssa/
const SsaValue = @import("../../ssa/value.zig").Value;
const SsaBlock = @import("../../ssa/block.zig").Block;
const BlockKind = @import("../../ssa/block.zig").BlockKind;
const SsaFunc = @import("../../ssa/func.zig").Func;
const SsaOp = @import("../../ssa/op.zig").Op;

const debug = @import("../../pipeline_debug.zig");

/// Error type for code generation
/// Uses explicit set to allow mutual recursion between getValue64 and ssaGenValueOnStack
pub const GenError = error{OutOfMemory};

/// Branch tracking - records pseudo-jumps for later resolution
/// Go reference: cmd/compile/internal/ssagen/ssa.go Branch struct
pub const Branch = struct {
    prog: *Prog, // The AJMP instruction
    target_block_id: u32, // Target block ID
};

/// Function index map type - maps function names to Wasm function indices
pub const FuncIndexMap = std.StringHashMapUnmanaged(u32);

/// State for SSA code generation
/// Go reference: cmd/compile/internal/ssagen/ssa.go State struct
pub const GenState = struct {
    allocator: std.mem.Allocator,
    builder: ProgBuilder,
    func: *const SsaFunc,

    /// Value to local mapping (Go: regalloc assigns registers)
    value_to_local: std.AutoHashMapUnmanaged(u32, u32),
    /// Next local index (set by allocateLocals after counting params)
    next_local: u32 = 0,
    param_count: u32 = 0,

    /// Branch tracking (Go: s.Branches)
    branches: std.ArrayListUnmanaged(Branch),

    /// First Prog of each block (Go: s.bstart)
    bstart: std.AutoHashMapUnmanaged(u32, *Prog),

    /// OnWasmStack tracking (Go: s.OnWasmStackSkipped)
    on_wasm_stack_skipped: i32 = 0,

    /// Frame size for locals
    frame_size: i32 = 0,

    /// Maps function names to Wasm function indices
    func_indices: ?*const FuncIndexMap = null,

    /// Maps string literal content to memory offsets
    string_offsets: ?*const std.StringHashMap(i32) = null,

    /// Maps type names to metadata memory offsets
    metadata_offsets: ?*const std.StringHashMap(i32) = null,

    pub fn init(allocator: std.mem.Allocator, func: *const SsaFunc) GenState {
        return .{
            .allocator = allocator,
            .builder = ProgBuilder.init(allocator),
            .func = func,
            .value_to_local = .{},
            .branches = .{},
            .bstart = .{},
        };
    }

    pub fn setFuncIndices(self: *GenState, indices: *const FuncIndexMap) void {
        self.func_indices = indices;
    }

    pub fn setStringOffsets(self: *GenState, offsets: *const std.StringHashMap(i32)) void {
        self.string_offsets = offsets;
    }

    pub fn setMetadataOffsets(self: *GenState, offsets: *const std.StringHashMap(i32)) void {
        self.metadata_offsets = offsets;
    }

    pub fn deinit(self: *GenState) void {
        self.builder.deinit();
        self.value_to_local.deinit(self.allocator);
        self.branches.deinit(self.allocator);
        self.bstart.deinit(self.allocator);
    }

    // ========================================================================
    // Go's s.Br - emit pseudo-jump
    // Reference: cmd/compile/internal/ssagen/ssa.go lines 6711-6716
    // ========================================================================

    /// Emit a pseudo-jump to target block. Will be resolved later.
    pub fn br(self: *GenState, target: *const SsaBlock) !*Prog {
        const p = try self.builder.append(.jmp);
        p.to.type = .branch;
        // Record for later resolution
        try self.branches.append(self.allocator, .{
            .prog = p,
            .target_block_id = target.id,
        });
        return p;
    }

    // ========================================================================
    // Go's ssaGenBlock - control flow generation with PSEUDO-JUMPS
    // Reference: cmd/compile/internal/wasm/ssa.go lines 169-215
    // ========================================================================

    pub fn ssaGenBlock(self: *GenState, b: *const SsaBlock, next: ?*const SsaBlock) !void {
        switch (b.kind) {
            // Go: BlockPlain, BlockDefer (lines 171-174)
            .plain, .first, .defer_ => {
                if (b.succs.len > 0) {
                    const succ = b.succs[0].b;
                    if (next == null or next.?.id != succ.id) {
                        _ = try self.br(succ);
                    }
                }
            },

            // Go: BlockIf (lines 176-198)
            .if_ => {
                if (b.succs.len < 2) return;

                const succ0 = b.succs[0].b; // true branch
                const succ1 = b.succs[1].b; // false branch

                if (b.controls[0]) |cond| {
                    try self.getValue32(cond);
                }

                if (next != null and next.?.id == succ0.id) {
                    // if false, jump to succ1 (Go: lines 178-184)
                    _ = try self.builder.append(.i32_eqz);
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ1);
                    _ = try self.builder.append(.end);
                } else if (next != null and next.?.id == succ1.id) {
                    // if true, jump to succ0 (Go: lines 185-190)
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ0);
                    _ = try self.builder.append(.end);
                } else {
                    // neither is next (Go: lines 191-197)
                    _ = try self.builder.append(.@"if");
                    _ = try self.br(succ0);
                    _ = try self.builder.append(.end);
                    _ = try self.br(succ1);
                }
            },

            // Go: BlockRet (lines 200-201)
            .ret => {
                // Get return value if any
                if (b.controls[0]) |ret_val| {
                    try self.getValue64(ret_val);
                }
                // Emit ARET pseudo-instruction, NOT real wasm return
                // Go: s.Prog(obj.ARET) - transformed by preprocess
                _ = try self.builder.append(.aret);
            },

            // Go: BlockExit (lines 203-204)
            .exit => {
                // Nothing - handled by preprocess
            },

            else => {},
        }

        // Go: Every block ends with ARESUMEPOINT (line 210)
        // This marks the entry point for the next block
        _ = try self.builder.append(.resume_point);

        // Go: Check stack balance (lines 212-214)
        if (self.on_wasm_stack_skipped != 0) {
            debug.log(.codegen, "wasm: bad stack in block b{d}", .{b.id});
        }
    }

    // ========================================================================
    // Value access helpers
    // Go reference: cmd/compile/internal/wasm/ssa.go lines 474-533
    // ========================================================================

    /// Get 32-bit value onto wasm stack
    /// Go: getValue32 (lines 474-489)
    pub fn getValue32(self: *GenState, v: *const SsaValue) GenError!void {
        // Generate value on stack
        try self.ssaGenValueOnStack(v);

        // Wrap to i32 if not already i32
        if (!isCmp(v)) {
            _ = try self.builder.append(.i32_wrap_i64);
        }
    }

    /// Get 64-bit value onto wasm stack
    /// Go: getValue64 (lines 491-503)
    pub fn getValue64(self: *GenState, v: *const SsaValue) GenError!void {
        // Check if rematerializable (constants)
        if (isRematerializable(v)) {
            try self.ssaGenValueOnStack(v);
            return;
        }

        // Get from local
        if (self.value_to_local.get(v.id)) |local_idx| {
            // Use local_get directly with local index
            _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            return;
        }

        // Generate on stack
        try self.ssaGenValueOnStack(v);
    }

    /// Store top of stack to local
    /// Go: setReg (lines 530-533)
    pub fn setReg(self: *GenState, v: *const SsaValue) GenError!void {
        if (self.value_to_local.get(v.id)) |local_idx| {
            // Use local_set directly with local index
            _ = try self.builder.appendTo(.local_set, prog_mod.constAddr(local_idx));
        }
    }

    /// Generate value directly onto wasm stack
    /// Go: ssaGenValueOnStack (lines 313-461)
    pub fn ssaGenValueOnStack(self: *GenState, v: *const SsaValue) GenError!void {
        switch (v.op) {
            // Constants
            .wasm_i64_const, .const_int, .const_64 => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(v.aux_int));
            },
            .wasm_i32_const, .const_32 => {
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(v.aux_int));
            },
            .wasm_f64_const, .const_float => {
                // Float value is bit-cast stored in aux_int
                _ = try self.builder.appendFrom(.f64_const, prog_mod.floatAddr(@bitCast(v.aux_int)));
            },
            .const_bool => {
                // Booleans are i32 in Wasm (0 or 1)
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(if (v.aux_int != 0) @as(i64, 1) else @as(i64, 0)));
            },

            // NOTE: const_string is rewritten to string_make by rewritegeneric.zig
            // NOTE: string_ptr/string_len/slice_ptr/slice_len are decomposed by rewritedec.zig

            // Compound type ops - these are conceptual groupings, no code generated
            // The components are accessed via extraction ops which get decomposed
            .string_make, .slice_make => {
                // No code - these values are decomposed when accessed
                debug.log(.codegen, "wasm/gen: skip compound type op {s}", .{@tagName(v.op)});
            },

            // Extraction ops for compound types - should be decomposed by rewritedec
            // If they reach here, they're on values that couldn't be decomposed (e.g., loaded from memory)
            .string_ptr, .string_len, .slice_ptr, .slice_len => {
                // These should have been decomposed - log warning
                debug.log(.codegen, "wasm/gen: undecomposed extraction op {s}", .{@tagName(v.op)});
            },

            // Arithmetic (i64)
            .wasm_i64_add => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_add);
            },
            .wasm_i64_sub => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_sub);
            },
            .wasm_i64_mul => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_mul);
            },
            .wasm_i64_div_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_div_s);
            },
            .wasm_i64_rem_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_rem_s);
            },

            // Comparisons (produce i32)
            .wasm_i64_eq => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_eq);
            },
            .wasm_i64_ne => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_ne);
            },
            .wasm_i64_lt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_lt_s);
            },
            .wasm_i64_le_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_le_s);
            },
            .wasm_i64_gt_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_gt_s);
            },
            .wasm_i64_ge_s => {
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_ge_s);
            },
            .wasm_i64_eqz => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i64_eqz);
            },

            // Memory operations
            .wasm_i64_load => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load);
                p.from = prog_mod.constAddr(v.aux_int); // offset
            },
            .wasm_i64_store => {
                try self.getValue64(v.args[0]); // address
                _ = try self.builder.append(.i32_wrap_i64);
                try self.getValue64(v.args[1]); // value
                const p = try self.builder.append(.i64_store);
                p.to = prog_mod.constAddr(v.aux_int); // offset
            },

            // Sized memory loads - Go reference: wasm/ssa.go loadOp()
            .wasm_i64_load8_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load8_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load8_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load8_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load16_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load16_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load16_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load16_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load32_u => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load32_u);
                p.from = prog_mod.constAddr(v.aux_int);
            },
            .wasm_i64_load32_s => {
                try self.getValue64(v.args[0]);
                _ = try self.builder.append(.i32_wrap_i64);
                const p = try self.builder.append(.i64_load32_s);
                p.from = prog_mod.constAddr(v.aux_int);
            },

            // Local address
            .local_addr => {
                // SP + actual_byte_offset
                // Use local_sizes to compute correct offset (handles variable-sized locals like strings)
                _ = try self.builder.appendFrom(.get, prog_mod.regAddr(.sp));
                _ = try self.builder.append(.i64_extend_i32_u);
                const local_idx: usize = @intCast(v.aux_int);
                const offset = self.getLocalOffset(local_idx);
                if (offset != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    _ = try self.builder.append(.i64_add);
                }
            },

            // Global address (for global variables in linear memory)
            // Globals are stored at fixed addresses: GLOBAL_BASE + (index * 8)
            // GLOBAL_BASE = 0x20000 (128KB, after stack and heap start)
            .global_addr => {
                const GLOBAL_BASE: i64 = 0x20000;
                const global_idx: i64 = v.aux_int;
                const addr: i64 = GLOBAL_BASE + (global_idx * 8);
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(addr));
            },

            // Type metadata address (for ARC destructor lookup)
            // Resolved at link time: metadata_offsets[type_name]
            .metadata_addr => {
                const type_name = v.aux.string;
                if (self.metadata_offsets) |offsets| {
                    if (offsets.get(type_name)) |offset| {
                        _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    } else {
                        // Type has no destructor - pass 0 (null metadata)
                        _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                    }
                } else {
                    // No metadata available - pass 0
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(0));
                }
            },

            // Pointer offset (for struct field access)
            .off_ptr => {
                // base_ptr + offset
                try self.getValue64(v.args[0]);
                const offset = v.aux_int;
                if (offset != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    _ = try self.builder.append(.i64_add);
                }
            },

            // Pointer arithmetic
            .add_ptr => {
                // base_ptr + (index * elem_size)
                try self.getValue64(v.args[0]); // base
                try self.getValue64(v.args[1]); // offset (already scaled)
                _ = try self.builder.append(.i64_add);
            },

            .sub_ptr => {
                // ptr1 - ptr2 (returns offset)
                try self.getValue64(v.args[0]);
                try self.getValue64(v.args[1]);
                _ = try self.builder.append(.i64_sub);
            },

            // Arguments - params are in locals 0..param_count-1
            .arg => {
                const local_idx = self.value_to_local.get(v.id) orelse @as(u32, @intCast(v.aux_int));
                _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            },

            // Copy
            .copy => {
                try self.getValue64(v.args[0]);
            },

            // Function calls
            .wasm_call => {
                // Push arguments onto stack
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                // Emit call with function index from aux_int
                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(v.aux_int);
            },

            .wasm_lowered_static_call => {
                // Push arguments onto stack
                for (v.args) |arg| {
                    try self.getValue64(arg);
                }
                // Get function index from name
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                const func_idx: i64 = if (fn_name) |name| blk: {
                    if (self.func_indices) |indices| {
                        if (indices.get(name)) |idx| {
                            break :blk @intCast(idx);
                        }
                    }
                    break :blk v.aux_int; // Fallback to aux_int
                } else v.aux_int;

                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(func_idx);
            },

            .wasm_lowered_retain => {
                // ARC retain: call cot_retain(obj) -> obj
                // Push the object pointer argument
                try self.getValue64(v.args[0]);
                // Look up cot_retain function index
                const func_idx: i64 = if (self.func_indices) |indices| blk: {
                    if (indices.get("cot_retain")) |idx| {
                        break :blk @intCast(idx);
                    }
                    break :blk 0; // Fallback (should not happen)
                } else 0;
                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(func_idx);
                // Result is on stack, store to local if needed
                if (self.value_to_local.get(v.id)) |local| {
                    const set_p = try self.builder.append(.local_set);
                    set_p.to = prog_mod.constAddr(local);
                }
            },

            .wasm_lowered_release => {
                // ARC release: call cot_release(obj) -> void
                // Push the object pointer argument
                try self.getValue64(v.args[0]);
                // Look up cot_release function index
                const func_idx: i64 = if (self.func_indices) |indices| blk: {
                    if (indices.get("cot_release")) |idx| {
                        break :blk @intCast(idx);
                    }
                    break :blk 0; // Fallback (should not happen)
                } else 0;
                const p = try self.builder.append(.call);
                p.to = prog_mod.constAddr(func_idx);
                // No result (void function)
            },

            else => {
                debug.log(.codegen, "wasm/gen: unhandled op {s}", .{@tagName(v.op)});
            },
        }
    }

    // ========================================================================
    // Generate entire function
    // ========================================================================

    pub fn generate(self: *GenState) !void {
        const blocks = self.func.blocks.items;

        debug.log(.codegen, "wasm/gen: generating '{s}' ({d} blocks)", .{
            self.func.name,
            blocks.len,
        });

        // Allocate locals for values
        try self.allocateLocals();

        // Compute frame size
        self.frame_size = self.computeFrameSize();

        // Emit TEXT marker at start
        const text = try self.builder.append(.text);
        text.from = prog_mod.constAddr(self.frame_size);

        // Generate code for each block
        for (blocks, 0..) |block, i| {
            const next: ?*const SsaBlock = if (i + 1 < blocks.len) blocks[i + 1] else null;

            // Record first Prog of this block
            const first_prog = self.builder.last;

            // Generate values
            for (block.values.items) |v| {
                try self.ssaGenValue(v);
            }

            // Record bstart after values but before control flow
            // (or use current position if no values)
            if (self.builder.last != first_prog) {
                if (self.builder.last) |last| {
                    try self.bstart.put(self.allocator, block.id, last);
                }
            } else if (self.builder.last) |last| {
                try self.bstart.put(self.allocator, block.id, last);
            }

            // Generate block control flow
            try self.ssaGenBlock(block, next);
        }

        // Resolve branches (Go: lines 7312-7320)
        for (self.branches.items) |branch| {
            if (self.bstart.get(branch.target_block_id)) |target_prog| {
                branch.prog.to.branch_target = target_prog;
            }
        }

        debug.log(.codegen, "wasm/gen: generated {d} instructions, {d} branches", .{
            self.builder.count,
            self.branches.items.len,
        });
    }

    /// Generate code for a single SSA value
    /// Go: ssaGenValue (lines 217-311)
    pub fn ssaGenValue(self: *GenState, v: *const SsaValue) !void {
        // Skip values with no uses (dead code) unless they have side effects
        if (v.uses == 0 and !v.hasSideEffects()) return;

        // Skip rematerializable values (generate on demand)
        if (isRematerializable(v)) return;

        // Skip comparisons - they're generated on-demand by ssaGenBlock
        // Go: these have OnWasmStack = true and are handled inline
        if (isCmp(v)) return;

        // Generate value and store to local
        try self.ssaGenValueOnStack(v);
        if (v.uses > 0) {
            try self.setReg(v);
        }
    }

    /// Allocate locals for values that need them
    fn allocateLocals(self: *GenState) !void {
        // Count parameters - Wasm assigns them to locals 0..param_count-1 automatically
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) {
                    const arg_idx: u32 = @intCast(v.aux_int);
                    // Params use locals 0..param_count-1 (assigned by Wasm)
                    try self.value_to_local.put(self.allocator, v.id, arg_idx);
                    if (arg_idx >= self.param_count) {
                        self.param_count = arg_idx + 1;
                    }
                }
            }
        }
        // PC_B is first declared local (index = param_count)
        // Value locals start after PC_B (param_count+1, param_count+2, ...)
        self.next_local = self.param_count + 1;

        // Allocate locals for other values
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .arg) continue;
                if (v.uses == 0 and !v.hasSideEffects()) continue;
                if (isRematerializable(v)) continue;
                if (isCmp(v)) continue;

                const local_idx = self.next_local;
                self.next_local += 1;
                try self.value_to_local.put(self.allocator, v.id, local_idx);
            }
        }
    }

    /// Compute byte offset for a local variable by summing sizes of all preceding locals.
    /// This handles variable-sized locals (e.g., strings are 16 bytes, i64 is 8 bytes).
    fn getLocalOffset(self: *const GenState, local_idx: usize) i64 {
        if (self.func.local_sizes.len == 0) {
            // Fallback: assume 8 bytes per slot
            return @intCast(local_idx * 8);
        }
        var offset: i64 = 0;
        const count = @min(local_idx, self.func.local_sizes.len);
        for (0..count) |i| {
            offset += @intCast(self.func.local_sizes[i]);
        }
        return offset;
    }

    /// Compute frame size from local sizes (copied from IR)
    /// Go reference: This matches how Go computes frame layout in obj/link.go
    fn computeFrameSize(self: *const GenState) i32 {
        // Use actual local sizes from IR (populated by ssa_builder.zig)
        if (self.func.local_sizes.len > 0) {
            var total: i32 = 0;
            for (self.func.local_sizes) |size| {
                total += @intCast(size);
            }
            // Align to 16 bytes
            return @divTrunc((total + 15), 16) * 16;
        }

        // Fallback: scan for local_addr ops (for backwards compatibility)
        var max_slot: i32 = -1;
        for (self.func.blocks.items) |block| {
            for (block.values.items) |v| {
                if (v.op == .local_addr) {
                    const slot: i32 = @intCast(v.aux_int);
                    if (slot > max_slot) max_slot = slot;
                }
            }
        }
        if (max_slot < 0) return 0;
        const size = (max_slot + 1) * 8;
        return @divTrunc((size + 15), 16) * 16;
    }
};

// ============================================================================
// Helper functions
// ============================================================================

fn isRematerializable(v: *const SsaValue) bool {
    return switch (v.op) {
        .wasm_i64_const, .wasm_i32_const, .wasm_f64_const,
        .const_int, .const_32, .const_64, .const_float, .const_bool,
        .local_addr, .global_addr, .metadata_addr,
        => true,
        else => false,
    };
}

fn isCmp(v: *const SsaValue) bool {
    // Returns true if value is already i32 (comparisons and booleans)
    return switch (v.op) {
        .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s,
        .wasm_i64_gt_s, .wasm_i64_ge_s, .wasm_i64_eqz,
        .wasm_f64_eq, .wasm_f64_ne, .wasm_f64_lt, .wasm_f64_le,
        .wasm_f64_gt, .wasm_f64_ge,
        .const_bool, // Booleans are already i32
        => true,
        else => false,
    };
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "GenState init/deinit" {
    const allocator = testing.allocator;

    // Create minimal SSA func for testing
    var func = SsaFunc.init(allocator, "test");
    defer func.deinit();

    var state = GenState.init(allocator, &func);
    defer state.deinit();

    // next_local starts at 0 (set by allocateLocals after counting params)
    try testing.expectEqual(@as(u32, 0), state.next_local);
}
