//! SSA Code Generator for WebAssembly
//!
//! Go reference: cmd/compile/internal/wasm/ssa.go
//!
//! This generates Prog chains from SSA values and blocks.
//! Key functions matching Go:
//! - ssaGenValue: Generate code for a single SSA value
//! - ssaGenBlock: Generate code for block control flow
//! - getValue32/getValue64: Get value onto wasm stack
//! - setReg: Store value to register (local)

const std = @import("std");
const c = @import("constants.zig");
const prog_mod = @import("prog.zig");
const Prog = prog_mod.Prog;
const Addr = prog_mod.Addr;
const Symbol = prog_mod.Symbol;
const ProgBuilder = prog_mod.ProgBuilder;

// SSA types - import from main compiler when integrated
// For now, define minimal interfaces

/// SSA Value interface
pub const SsaValue = struct {
    id: u32,
    op: SsaOp,
    args: [4]?*const SsaValue = .{ null, null, null, null },
    aux_int: i64 = 0,
    aux_float: f64 = 0,
    uses: u32 = 0,
    block: ?*const SsaBlock = null,

    // Go's OnWasmStack optimization
    on_wasm_stack: bool = false,
};

/// SSA Block interface
pub const SsaBlock = struct {
    id: u32,
    kind: BlockKind,
    values: []const *const SsaValue = &.{},
    succs: [2]?*const SsaBlock = .{ null, null },
    controls: [1]?*const SsaValue = .{null},
    likely: BranchPrediction = .unknown,
};

pub const BlockKind = enum {
    plain,
    if_,
    ret,
    exit,
};

pub const BranchPrediction = enum {
    unknown,
    likely,
    unlikely,
};

/// SSA Op - subset for wasm lowered ops
pub const SsaOp = enum {
    // Constants
    wasm_i64_const,
    wasm_i32_const,
    wasm_f64_const,
    wasm_f32_const,

    // Arithmetic
    wasm_i64_add,
    wasm_i64_sub,
    wasm_i64_mul,
    wasm_i64_div_s,
    wasm_i64_rem_s,

    // Bitwise
    wasm_i64_and,
    wasm_i64_or,
    wasm_i64_xor,
    wasm_i64_shl,
    wasm_i64_shr_s,
    wasm_i64_shr_u,

    // Comparisons
    wasm_i64_eq,
    wasm_i64_ne,
    wasm_i64_lt_s,
    wasm_i64_le_s,
    wasm_i64_gt_s,
    wasm_i64_ge_s,
    wasm_i64_eqz,

    // Float
    wasm_f64_add,
    wasm_f64_sub,
    wasm_f64_mul,
    wasm_f64_div,
    wasm_f64_neg,

    // Memory
    wasm_i64_load,
    wasm_i64_store,
    wasm_i32_load,
    wasm_i32_store,

    // Calls
    wasm_lowered_static_call,
    wasm_call,

    // Memory operations
    wasm_lowered_move,
    wasm_lowered_zero,
    wasm_lowered_nil_check,

    // Control
    copy,
    phi,
    arg,
    local_addr,

    // Other
    invalid,
};

/// State for SSA code generation
pub const GenState = struct {
    allocator: std.mem.Allocator,
    builder: ProgBuilder,
    sym: *Symbol,

    // Value to local mapping
    value_to_local: std.AutoHashMapUnmanaged(u32, u32) = .{},
    next_local: u32 = 0,

    // OnWasmStack tracking (Go: s.OnWasmStackSkipped)
    on_wasm_stack_skipped: i32 = 0,

    pub fn init(allocator: std.mem.Allocator, sym: *Symbol) GenState {
        return .{
            .allocator = allocator,
            .builder = ProgBuilder.init(allocator),
            .sym = sym,
        };
    }

    pub fn deinit(self: *GenState) void {
        self.builder.deinit();
        self.value_to_local.deinit(self.allocator);
    }

    // ========================================================================
    // Main generation functions (Go: ssa.go)
    // ========================================================================

    /// Generate code for a single value
    /// Go reference: ssa.go ssaGenValue (lines 217-311)
    pub fn ssaGenValue(self: *GenState, v: *const SsaValue) !void {
        switch (v.op) {
            // ----------------------------------------------------------------
            // Calls (Go: lines 219-246)
            // ----------------------------------------------------------------
            .wasm_lowered_static_call => {
                // For now, just emit call
                // Go does: PrepareCall, handle closures, emit call
                const p = try self.builder.appendTo(.call, prog_mod.constAddr(v.aux_int));
                _ = p;
            },

            .wasm_call => {
                const p = try self.builder.appendTo(.call, prog_mod.constAddr(v.aux_int));
                _ = p;
            },

            // ----------------------------------------------------------------
            // Memory move/zero (Go: lines 248-258)
            // ----------------------------------------------------------------
            .wasm_lowered_move => {
                // memory.copy: dst, src, len
                if (v.args[0]) |dst| try self.getValue32(dst);
                if (v.args[1]) |src| try self.getValue32(src);
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(v.aux_int));
                _ = try self.builder.append(.memory_copy);
            },

            .wasm_lowered_zero => {
                // memory.fill: dst, val(0), len
                if (v.args[0]) |dst| try self.getValue32(dst);
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(0));
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(v.aux_int));
                _ = try self.builder.append(.memory_fill);
            },

            // ----------------------------------------------------------------
            // Nil check (Go: lines 260-272)
            // ----------------------------------------------------------------
            .wasm_lowered_nil_check => {
                if (v.args[0]) |ptr| try self.getValue64(ptr);
                _ = try self.builder.append(.i64_eqz);
                _ = try self.builder.append(.@"if");
                _ = try self.builder.append(.@"unreachable");
                _ = try self.builder.append(.end);
            },

            // ----------------------------------------------------------------
            // Stores (Go: lines 280-284)
            // ----------------------------------------------------------------
            .wasm_i64_store => {
                if (v.args[0]) |addr| try self.getValue32(addr);
                if (v.args[1]) |val| try self.getValue64(val);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                _ = try self.builder.appendTo(.i64_store, prog_mod.constAddr(offset));
            },

            .wasm_i32_store => {
                if (v.args[0]) |addr| try self.getValue32(addr);
                if (v.args[1]) |val| try self.getValue32(val);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                _ = try self.builder.appendTo(.i32_store, prog_mod.constAddr(offset));
            },

            // ----------------------------------------------------------------
            // Default: generate value on stack, then store to local
            // (Go: lines 295-309)
            // ----------------------------------------------------------------
            else => {
                // Check OnWasmStack (Go: lines 299-304)
                if (v.on_wasm_stack) {
                    self.on_wasm_stack_skipped += 1;
                    return;
                }

                // Generate value on stack
                try self.ssaGenValueOnStack(v);

                // Store to local if used
                if (v.uses > 0) {
                    try self.setReg(v);
                }
            },
        }
    }

    /// Generate value directly on the wasm stack
    /// Go reference: ssa.go ssaGenValueOnStack (lines 313-460)
    pub fn ssaGenValueOnStack(self: *GenState, v: *const SsaValue) !void {
        switch (v.op) {
            // ----------------------------------------------------------------
            // Constants (Go: lines 370-377)
            // ----------------------------------------------------------------
            .wasm_i64_const => {
                _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(v.aux_int));
            },

            .wasm_i32_const => {
                _ = try self.builder.appendFrom(.i32_const, prog_mod.constAddr(v.aux_int));
            },

            .wasm_f64_const => {
                _ = try self.builder.appendFrom(.f64_const, prog_mod.floatAddr(v.aux_float));
            },

            .wasm_f32_const => {
                _ = try self.builder.appendFrom(.f32_const, prog_mod.floatAddr(@floatCast(v.aux_float)));
            },

            // ----------------------------------------------------------------
            // Loads (Go: lines 379-382)
            // ----------------------------------------------------------------
            .wasm_i64_load => {
                if (v.args[0]) |addr| try self.getValue32(addr);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                _ = try self.builder.appendFrom(.i64_load, prog_mod.constAddr(offset));
            },

            .wasm_i32_load => {
                if (v.args[0]) |addr| try self.getValue32(addr);
                const offset: u32 = @intCast(@as(u32, @bitCast(@as(i32, @truncate(v.aux_int)))));
                _ = try self.builder.appendFrom(.i32_load, prog_mod.constAddr(offset));
            },

            // ----------------------------------------------------------------
            // Binary i64 ops (Go: lines 401-406)
            // ----------------------------------------------------------------
            .wasm_i64_add, .wasm_i64_sub, .wasm_i64_mul, .wasm_i64_div_s, .wasm_i64_rem_s, .wasm_i64_and, .wasm_i64_or, .wasm_i64_xor, .wasm_i64_shl, .wasm_i64_shr_s, .wasm_i64_shr_u => {
                if (v.args[0]) |a| try self.getValue64(a);
                if (v.args[1]) |b| try self.getValue64(b);
                const as = opToAs(v.op);
                _ = try self.builder.append(as);
            },

            // ----------------------------------------------------------------
            // Comparisons (Go: lines 391-399)
            // ----------------------------------------------------------------
            .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s, .wasm_i64_gt_s, .wasm_i64_ge_s => {
                if (v.args[0]) |a| try self.getValue64(a);
                if (v.args[1]) |b| try self.getValue64(b);
                const as = opToAs(v.op);
                _ = try self.builder.append(as);
                // Go extends to i64 here, but we keep as i32 for control flow
            },

            .wasm_i64_eqz => {
                if (v.args[0]) |a| try self.getValue64(a);
                _ = try self.builder.append(.i64_eqz);
            },

            // ----------------------------------------------------------------
            // Float ops (Go: lines 402-406)
            // ----------------------------------------------------------------
            .wasm_f64_add, .wasm_f64_sub, .wasm_f64_mul, .wasm_f64_div => {
                if (v.args[0]) |a| try self.getValue64(a);
                if (v.args[1]) |b| try self.getValue64(b);
                const as = opToAs(v.op);
                _ = try self.builder.append(as);
            },

            .wasm_f64_neg => {
                if (v.args[0]) |a| try self.getValue64(a);
                _ = try self.builder.append(.f64_neg);
            },

            // ----------------------------------------------------------------
            // Copy (Go: lines 454-455)
            // ----------------------------------------------------------------
            .copy => {
                if (v.args[0]) |a| try self.getValue64(a);
            },

            // ----------------------------------------------------------------
            // Arg - function argument
            // ----------------------------------------------------------------
            .arg => {
                // Arguments are stored on the stack at known offsets
                // Get SP, add offset, load
                _ = try self.builder.appendFrom(.get, prog_mod.regAddr(.sp));
                _ = try self.builder.append(.i64_extend_i32_u);
                const offset = v.aux_int + 8; // +8 for return address
                if (offset != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(offset));
                    _ = try self.builder.append(.i64_add);
                }
                _ = try self.builder.appendFrom(.i64_load, prog_mod.constAddr(0));
            },

            // ----------------------------------------------------------------
            // Local address
            // ----------------------------------------------------------------
            .local_addr => {
                // Compute address of local variable
                _ = try self.builder.appendFrom(.get, prog_mod.regAddr(.sp));
                _ = try self.builder.append(.i64_extend_i32_u);
                if (v.aux_int != 0) {
                    _ = try self.builder.appendFrom(.i64_const, prog_mod.constAddr(v.aux_int));
                    _ = try self.builder.append(.i64_add);
                }
            },

            // ----------------------------------------------------------------
            // Phi - should be handled by reg alloc
            // ----------------------------------------------------------------
            .phi => {
                // Phi nodes should have been eliminated
            },

            else => {
                // Unknown op - error in debug builds
            },
        }
    }

    /// Generate code for block control flow
    /// Go reference: ssa.go ssaGenBlock (lines 169-215)
    pub fn ssaGenBlock(self: *GenState, b: *const SsaBlock, next: ?*const SsaBlock) !void {
        switch (b.kind) {
            // ----------------------------------------------------------------
            // Plain block (Go: lines 171-173)
            // ----------------------------------------------------------------
            .plain => {
                // Jump to successor if not fall-through
                if (b.succs[0]) |succ| {
                    if (next == null or next.?.id != succ.id) {
                        // Need explicit jump (br to loop)
                        // For now we rely on block ordering
                    }
                }
            },

            // ----------------------------------------------------------------
            // If block (Go: lines 176-198)
            // ----------------------------------------------------------------
            .if_ => {
                const ctrl = b.controls[0] orelse return;
                const succ0 = b.succs[0];
                const succ1 = b.succs[1];

                if (next != null and succ0 != null and next.?.id == succ0.?.id) {
                    // Fall through to true branch, jump on false
                    try self.getValue32(ctrl);
                    _ = try self.builder.append(.i32_eqz);
                    _ = try self.builder.append(.@"if");
                    // Jump to false branch - need br_table or structured control flow
                    _ = try self.builder.append(.end);
                } else if (next != null and succ1 != null and next.?.id == succ1.?.id) {
                    // Fall through to false branch, jump on true
                    try self.getValue32(ctrl);
                    _ = try self.builder.append(.@"if");
                    // Jump to true branch
                    _ = try self.builder.append(.end);
                } else {
                    // Neither is fall-through
                    try self.getValue32(ctrl);
                    _ = try self.builder.append(.@"if");
                    // Jump to true
                    _ = try self.builder.append(.end);
                    // Jump to false
                }
            },

            // ----------------------------------------------------------------
            // Return block (Go: lines 200-201)
            // ----------------------------------------------------------------
            .ret => {
                // Get return value if any
                if (b.controls[0]) |ret_val| {
                    try self.getValue64(ret_val);
                }
                _ = try self.builder.append(.@"return");
            },

            // ----------------------------------------------------------------
            // Exit block (Go: lines 203-204)
            // ----------------------------------------------------------------
            .exit => {
                // Nothing to do
            },
        }
    }

    // ========================================================================
    // Value access helpers (Go: ssa.go lines 474-503)
    // ========================================================================

    /// Get 32-bit value onto wasm stack
    /// Go reference: ssa.go getValue32 (lines 474-489)
    pub fn getValue32(self: *GenState, v: *const SsaValue) !void {
        // Check OnWasmStack (Go: lines 475-481)
        if (v.on_wasm_stack) {
            self.on_wasm_stack_skipped -= 1;
            try self.ssaGenValueOnStack(v);
            if (!isCmp(v)) {
                _ = try self.builder.append(.i32_wrap_i64);
            }
            return;
        }

        // Get from local
        if (self.value_to_local.get(v.id)) |local_idx| {
            _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            _ = try self.builder.append(.i32_wrap_i64);
            return;
        }

        // Generate inline
        try self.ssaGenValueOnStack(v);
        if (!isCmp(v)) {
            _ = try self.builder.append(.i32_wrap_i64);
        }
    }

    /// Get 64-bit value onto wasm stack
    /// Go reference: ssa.go getValue64 (lines 491-503)
    pub fn getValue64(self: *GenState, v: *const SsaValue) !void {
        // Check OnWasmStack (Go: lines 492-496)
        if (v.on_wasm_stack) {
            self.on_wasm_stack_skipped -= 1;
            try self.ssaGenValueOnStack(v);
            return;
        }

        // Get from local
        if (self.value_to_local.get(v.id)) |local_idx| {
            _ = try self.builder.appendFrom(.local_get, prog_mod.constAddr(local_idx));
            return;
        }

        // Generate inline
        try self.ssaGenValueOnStack(v);
    }

    /// Store value from wasm stack to local
    /// Go reference: ssa.go setReg (lines 530-533)
    pub fn setReg(self: *GenState, v: *const SsaValue) !void {
        const local_idx = try self.allocateLocal(v);
        _ = try self.builder.appendTo(.local_set, prog_mod.constAddr(local_idx));
    }

    /// Allocate a local for a value
    fn allocateLocal(self: *GenState, v: *const SsaValue) !u32 {
        if (self.value_to_local.get(v.id)) |idx| {
            return idx;
        }
        const idx = self.next_local;
        self.next_local += 1;
        try self.value_to_local.put(self.allocator, v.id, idx);
        return idx;
    }
};

// ============================================================================
// Helper functions
// ============================================================================

/// Check if value is a comparison (produces i32)
/// Go reference: ssa.go isCmp (lines 463-472)
fn isCmp(v: *const SsaValue) bool {
    return switch (v.op) {
        .wasm_i64_eqz,
        .wasm_i64_eq,
        .wasm_i64_ne,
        .wasm_i64_lt_s,
        .wasm_i64_le_s,
        .wasm_i64_gt_s,
        .wasm_i64_ge_s,
        => true,
        else => false,
    };
}

/// Map SSA op to assembler instruction
fn opToAs(op: SsaOp) c.As {
    return switch (op) {
        .wasm_i64_add => .i64_add,
        .wasm_i64_sub => .i64_sub,
        .wasm_i64_mul => .i64_mul,
        .wasm_i64_div_s => .i64_div_s,
        .wasm_i64_rem_s => .i64_rem_s,
        .wasm_i64_and => .i64_and,
        .wasm_i64_or => .i64_or,
        .wasm_i64_xor => .i64_xor,
        .wasm_i64_shl => .i64_shl,
        .wasm_i64_shr_s => .i64_shr_s,
        .wasm_i64_shr_u => .i64_shr_u,
        .wasm_i64_eq => .i64_eq,
        .wasm_i64_ne => .i64_ne,
        .wasm_i64_lt_s => .i64_lt_s,
        .wasm_i64_le_s => .i64_le_s,
        .wasm_i64_gt_s => .i64_gt_s,
        .wasm_i64_ge_s => .i64_ge_s,
        .wasm_f64_add => .f64_add,
        .wasm_f64_sub => .f64_sub,
        .wasm_f64_mul => .f64_mul,
        .wasm_f64_div => .f64_div,
        else => .nop,
    };
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "gen state init" {
    const allocator = testing.allocator;
    var sym = Symbol.init("test");

    var state = GenState.init(allocator, &sym);
    defer state.deinit();

    try testing.expectEqual(@as(u32, 0), state.next_local);
}

test "isCmp" {
    const v1 = SsaValue{ .id = 0, .op = .wasm_i64_eq };
    const v2 = SsaValue{ .id = 1, .op = .wasm_i64_add };

    try testing.expect(isCmp(&v1));
    try testing.expect(!isCmp(&v2));
}

test "opToAs" {
    try testing.expectEqual(c.As.i64_add, opToAs(.wasm_i64_add));
    try testing.expectEqual(c.As.i64_eq, opToAs(.wasm_i64_eq));
    try testing.expectEqual(c.As.f64_mul, opToAs(.wasm_f64_mul));
}
