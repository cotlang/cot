//! SSA Compilation Pass Infrastructure (Stub)
//!
//! NOTE: Native codegen is being rewritten with Cranelift-style architecture.
//! See CRANELIFT_PORT_MASTER_PLAN.md for progress.
//!
//! This file previously orchestrated passes for native code generation:
//! - expand_calls: decompose aggregates for ABI
//! - decompose: break 16-byte values into 8-byte components
//! - schedule: order values within blocks for emission
//! - regalloc: assign physical registers
//! - stackalloc: assign stack slots to spills
//!
//! These passes have been removed pending the Cranelift port which will
//! use a fundamentally different architecture (block parameters instead
//! of phi nodes, proper MachInst framework, regalloc2).

const std = @import("std");
const Func = @import("func.zig").Func;

// SSA passes (Wasm path - still available)
const schedule_mod = @import("passes/schedule.zig");

/// Compilation result placeholder - will be replaced by Cranelift port
pub const CompileResult = struct {
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompileResult) void {
        _ = self;
    }
};

/// Native compilation is not yet implemented.
/// Cranelift-style port is in progress.
pub fn compile(
    allocator: std.mem.Allocator,
    f: *Func,
    type_reg: anytype,
    target: anytype,
) !CompileResult {
    _ = f;
    _ = type_reg;
    _ = target;
    std.debug.print("ERROR: Native codegen not implemented. See CRANELIFT_PORT_MASTER_PLAN.md\n", .{});
    return CompileResult{ .allocator = allocator };
}

/// Run schedule pass only (still available for Wasm path).
pub fn schedule(f: *Func) !void {
    try schedule_mod.schedule(f);
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "compile stub returns result" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    _ = try f.newBlock(.first);

    var result = try compile(allocator, &f, null, .{});
    result.deinit();
}
