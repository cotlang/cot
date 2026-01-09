//! Register Allocation for Cot Bytecode
//!
//! This module implements Go-style linear scan register allocation.
//! Unlike traditional pre-pass allocators, this works WITH the bytecode emitter:
//!
//! 1. Pre-pass: Compute use distances (backward walk)
//!    - For each value, build a linked list of upcoming uses
//!    - Distance = instruction index where value is used
//!
//! 2. During emission: Allocate registers on demand
//!    - allocValToReg: get a value into a register
//!    - allocReg: find a free register, or spill farthest-next-use
//!    - advanceUses: after each instruction, free dead values
//!
//! Key insight from Go's regalloc.go:
//! "The greedy allocator moves values into registers just before they
//! are used, spills registers only when necessary, and spills the
//! value whose next use is farthest in the future."
//!
//! References:
//! - Go's regalloc.go: cmd/compile/internal/ssa/regalloc.go
//! - Poletto & Sarkar, "Linear Scan Register Allocation", 1999

const std = @import("std");
const ir = @import("ir.zig");
const Allocator = std.mem.Allocator;

const log = std.log.scoped(.regalloc);

/// Number of general-purpose registers (r0-r13, r14-r15 reserved)
pub const NUM_REGISTERS: u4 = 14;

/// Mask of all allocatable registers
pub const ALLOCATABLE_MASK: u16 = (1 << NUM_REGISTERS) - 1; // 0x3FFF

// ============================================================================
// Use Tracking
// ============================================================================

/// A use record tracks when a value will next be used.
/// Uses are stored in a linked list, ordered by distance (nondecreasing).
pub const Use = struct {
    /// Distance from current instruction to this use (in instruction count)
    dist: u32,
    /// Next use (further in the future), or null if this is the last use
    next: ?*Use,

    /// Sentinel distance for "no more uses"
    pub const MAX_DIST: u32 = std.math.maxInt(u32);
};

// ============================================================================
// Per-Value State
// ============================================================================

/// State for each IR value during register allocation.
pub const ValueState = struct {
    /// Bitmask of registers currently holding this value (usually 0 or 1 bit set)
    regs: u16 = 0,

    /// Linked list of upcoming uses (head = soonest use)
    uses: ?*Use = null,

    /// Stack slot if this value has been spilled (null = not spilled)
    spill_slot: ?u16 = null,

    /// True if this value needs a register (false for void, memory, etc.)
    needs_reg: bool = true,

    /// True if this value can be rematerialized (constants)
    is_rematerializable: bool = false,

    /// Constant pool index if rematerializable
    const_idx: ?u16 = null,

    /// Local slot index if this is an alloca result
    local_slot: ?u16 = null,

    /// Get distance to next use, or MAX_DIST if no more uses
    pub fn nextUseDist(self: *const ValueState) u32 {
        return if (self.uses) |u| u.dist else Use.MAX_DIST;
    }
};

// ============================================================================
// Per-Register State
// ============================================================================

/// State for each physical register.
pub const RegState = struct {
    /// Value ID currently in this register (null = free)
    value_id: ?u32 = null,
};

// ============================================================================
// Register Allocator State
// ============================================================================

/// Main register allocator state. Create one per function.
pub const RegAllocState = struct {
    allocator: Allocator,

    /// Per-value state, indexed by value_id
    /// Grows dynamically as we encounter new values
    values: std.AutoHashMapUnmanaged(u32, ValueState),

    /// Per-register state (16 registers, indices 0-15)
    regs: [16]RegState,

    /// Bitmask of registers currently in use
    used: u16,

    /// Bitmask of registers that cannot be spilled (holding operands for current instruction)
    nospill: u16,

    /// Free list for Use structs (avoids allocation during processing)
    free_uses: ?*Use,

    /// Pool of allocated Use structs (for cleanup)
    use_pool: std.ArrayListUnmanaged(*Use),

    /// Current instruction index (for distance calculation)
    cur_idx: u32,

    /// Next available spill slot
    next_spill_slot: u16,

    /// Base slot for spills (set by emitter based on local_count)
    spill_slot_base: u16,

    /// Maximum spill slots used (for updating local_count)
    max_spill_slots: u16,

    const Self = @This();

    /// Initialize allocator state
    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .values = .{},
            .regs = [_]RegState{.{}} ** 16,
            .used = 0,
            .nospill = 0,
            .free_uses = null,
            .use_pool = .{},
            .cur_idx = 0,
            .next_spill_slot = 0,
            .spill_slot_base = 0,
            .max_spill_slots = 0,
        };
    }

    /// Clean up allocator state
    pub fn deinit(self: *Self) void {
        self.values.deinit(self.allocator);
        // Free all Use structs
        for (self.use_pool.items) |use_ptr| {
            self.allocator.destroy(use_ptr);
        }
        self.use_pool.deinit(self.allocator);
    }

    /// Reset state for a new function
    pub fn reset(self: *Self) void {
        self.values.clearRetainingCapacity();
        self.regs = [_]RegState{.{}} ** 16;
        self.used = 0;
        self.nospill = 0;
        // Return all uses to free list (don't deallocate)
        // We keep the use_pool for reuse
        self.free_uses = null;
        for (self.use_pool.items) |use_ptr| {
            use_ptr.next = self.free_uses;
            self.free_uses = use_ptr;
        }
        self.cur_idx = 0;
        self.next_spill_slot = 0;
        self.max_spill_slots = 0;
    }

    /// Set the base slot for spilling (should be called after locals are allocated)
    pub fn setSpillBase(self: *Self, base: u16) void {
        self.spill_slot_base = base;
        self.next_spill_slot = base;
    }

    // ========================================================================
    // Use Distance Computation (Pre-pass)
    // ========================================================================

    /// Compute use distances by walking backwards through the function.
    /// Must be called before processing instructions.
    pub fn computeUseDistances(self: *Self, func: *const ir.Function) !void {
        // Count total instructions to compute distances
        var total_insts: u32 = 0;
        for (func.blocks.items) |block| {
            total_insts += @intCast(block.instructions.items.len);
        }

        // Walk backwards through all blocks
        var inst_idx: u32 = total_insts;
        for (func.blocks.items) |block| {
            // Walk backwards through instructions in this block
            var i = block.instructions.items.len;
            while (i > 0) {
                i -= 1;
                inst_idx -= 1;
                const inst = block.instructions.items[i];

                // Get operands of this instruction
                const operands = inst.getOperands();
                for (operands) |operand| {
                    try self.addUse(operand.id, inst_idx);
                }

                // Track special instruction types
                switch (inst) {
                    .iconst => |c| {
                        var vs = try self.getOrCreateValueState(c.result.id);
                        vs.is_rematerializable = true;
                        vs.needs_reg = true;
                    },
                    .f32const => |c| {
                        var vs = try self.getOrCreateValueState(c.result.id);
                        vs.is_rematerializable = true;
                        vs.needs_reg = true;
                    },
                    .f64const => |c| {
                        var vs = try self.getOrCreateValueState(c.result.id);
                        vs.is_rematerializable = true;
                        vs.needs_reg = true;
                    },
                    .const_string => |c| {
                        var vs = try self.getOrCreateValueState(c.result.id);
                        vs.is_rematerializable = true;
                        vs.needs_reg = true;
                    },
                    .const_null => |c| {
                        var vs = try self.getOrCreateValueState(c.result.id);
                        vs.is_rematerializable = true;
                        vs.needs_reg = true;
                    },
                    .alloca => |a| {
                        var vs = try self.getOrCreateValueState(a.result.id);
                        vs.needs_reg = false; // Allocas don't produce register values
                    },
                    else => {},
                }
            }
        }

        log.debug("Computed use distances for {d} instructions", .{total_insts});
    }

    /// Add a use record for a value at the given instruction index.
    fn addUse(self: *Self, value_id: u32, dist: u32) !void {
        var vs = try self.getOrCreateValueState(value_id);

        // Get or allocate a Use struct
        const use = if (self.free_uses) |u| blk: {
            self.free_uses = u.next;
            break :blk u;
        } else blk: {
            const u = try self.allocator.create(Use);
            try self.use_pool.append(self.allocator, u);
            break :blk u;
        };

        // Insert into the use list maintaining sorted order (nondecreasing dist)
        use.dist = dist;

        if (vs.uses == null or dist < vs.uses.?.dist) {
            // Insert at head
            use.next = vs.uses;
            vs.uses = use;
        } else {
            // Find insertion point
            var prev = vs.uses.?;
            while (prev.next != null and prev.next.?.dist <= dist) {
                prev = prev.next.?;
            }
            use.next = prev.next;
            prev.next = use;
        }
    }

    /// Get or create ValueState for a value_id
    fn getOrCreateValueState(self: *Self, value_id: u32) !*ValueState {
        const result = try self.values.getOrPut(self.allocator, value_id);
        if (!result.found_existing) {
            result.value_ptr.* = .{};
        }
        return result.value_ptr;
    }

    /// Get ValueState for a value_id (returns null if not found)
    pub fn getValueState(self: *Self, value_id: u32) ?*ValueState {
        return self.values.getPtr(value_id);
    }

    // ========================================================================
    // Register Allocation Core
    // ========================================================================

    /// Free a register, removing any value from it.
    pub fn freeReg(self: *Self, r: u4) void {
        if (r >= NUM_REGISTERS) return;

        if (self.regs[r].value_id) |vid| {
            // Update value state
            if (self.values.getPtr(vid)) |vs| {
                vs.regs &= ~(@as(u16, 1) << r);
            }
        }
        self.regs[r].value_id = null;
        self.used &= ~(@as(u16, 1) << r);
    }

    /// Free all registers in the given mask.
    pub fn freeRegs(self: *Self, mask: u16) void {
        var m = mask;
        while (m != 0) {
            const r: u4 = @intCast(@ctz(m));
            self.freeReg(r);
            m &= ~(@as(u16, 1) << r);
        }
    }

    /// Allocate a spill slot.
    fn allocSpillSlot(self: *Self) u16 {
        const slot = self.next_spill_slot;
        self.next_spill_slot += 1;
        if (self.next_spill_slot - self.spill_slot_base > self.max_spill_slots) {
            self.max_spill_slots = self.next_spill_slot - self.spill_slot_base;
        }
        return slot;
    }

    /// Spill a register's contents to a stack slot.
    /// Returns the spill slot, or null if the value was already spilled.
    pub fn spillReg(self: *Self, r: u4) ?u16 {
        const value_id = self.regs[r].value_id orelse return null;
        const vs = self.values.getPtr(value_id) orelse return null;

        // If already spilled, just free the register
        if (vs.spill_slot != null) {
            self.freeReg(r);
            return vs.spill_slot;
        }

        // Allocate a spill slot
        const slot = self.allocSpillSlot();
        vs.spill_slot = slot;

        log.debug("SPILL: value {d} from r{d} to slot {d}", .{ value_id, r, slot });

        self.freeReg(r);
        return slot;
    }

    /// Allocate a register from the given mask.
    /// If all registers are in use, spills the one with the farthest next use.
    pub fn allocReg(self: *Self, mask: u16) !u4 {
        const available = mask & ALLOCATABLE_MASK & ~self.used;

        if (available != 0) {
            // Free register available - pick the lowest
            const r: u4 = @intCast(@ctz(available));
            self.used |= @as(u16, 1) << r;
            return r;
        }

        // No free register - find one to spill
        // Pick the register holding the value with the farthest next use
        var best_reg: ?u4 = null;
        var best_dist: u32 = 0;

        for (0..NUM_REGISTERS) |i| {
            const r: u4 = @intCast(i);
            if (mask >> r & 1 == 0) continue; // Not in allowed mask
            if (self.nospill >> r & 1 != 0) continue; // Can't spill this one

            const value_id = self.regs[r].value_id orelse continue;
            const vs = self.values.getPtr(value_id) orelse continue;

            // Get distance to next use
            const dist = vs.nextUseDist();

            if (dist > best_dist or best_reg == null) {
                best_dist = dist;
                best_reg = r;
            }
        }

        const r = best_reg orelse return error.NoRegisterAvailable;

        // Spill this register
        _ = self.spillReg(r);

        self.used |= @as(u16, 1) << r;
        return r;
    }

    /// Assign a value to a specific register.
    /// The register should already be free or the value should already be in it.
    pub fn assignReg(self: *Self, value_id: u32, r: u4) !void {
        if (r >= NUM_REGISTERS) return;

        var vs = try self.getOrCreateValueState(value_id);
        vs.regs |= @as(u16, 1) << r;
        self.regs[r].value_id = value_id;
        self.used |= @as(u16, 1) << r;
    }

    /// Get a value into a register from the given mask.
    /// Returns the register containing the value.
    /// If the value is already in a suitable register, returns that.
    /// Otherwise allocates a register and records that a load is needed.
    pub fn allocValToReg(self: *Self, value_id: u32, mask: u16) !struct { reg: u4, needs_load: bool, source: LoadSource } {
        var vs = try self.getOrCreateValueState(value_id);

        // Check if value is already in a suitable register
        const suitable = vs.regs & mask & ALLOCATABLE_MASK;
        if (suitable != 0) {
            const r: u4 = @intCast(@ctz(suitable));
            return .{ .reg = r, .needs_load = false, .source = .none };
        }

        // Need to load into a register
        const r = try self.allocReg(mask);

        // Determine load source
        const source: LoadSource = if (vs.spill_slot) |slot|
            .{ .spill = slot }
        else if (vs.is_rematerializable)
            .rematerialize
        else if (vs.local_slot) |slot|
            .{ .local = slot }
        else
            .none; // Value should be computed fresh

        // Track that value is now in this register
        vs.regs |= @as(u16, 1) << r;
        self.regs[r].value_id = value_id;

        return .{ .reg = r, .needs_load = source != .none, .source = source };
    }

    /// Where to load a value from
    pub const LoadSource = union(enum) {
        none,
        spill: u16,
        local: u16,
        rematerialize,
    };

    /// Advance uses after processing an instruction.
    /// This pops the current use from each operand's use list and
    /// frees registers holding values that have no more uses.
    pub fn advanceUses(self: *Self, inst: ir.Instruction) void {
        const operands = inst.getOperands();
        for (operands) |operand| {
            self.advanceValueUse(operand.id);
        }
    }

    /// Advance uses for a single value.
    fn advanceValueUse(self: *Self, value_id: u32) void {
        const vs = self.values.getPtr(value_id) orelse return;

        if (vs.uses) |use| {
            // Pop this use
            vs.uses = use.next;

            // Return Use to free list
            use.next = self.free_uses;
            self.free_uses = use;

            // If no more uses, free all registers holding this value
            if (vs.uses == null) {
                self.freeRegs(vs.regs);
                vs.regs = 0;
            }
        }
    }

    /// Set the current instruction index.
    pub fn setCurrentIdx(self: *Self, idx: u32) void {
        self.cur_idx = idx;
    }

    /// Mark a register as "no spill" for the current instruction.
    /// Used to protect operand registers from being spilled while loading other operands.
    pub fn markNoSpill(self: *Self, r: u4) void {
        self.nospill |= @as(u16, 1) << r;
    }

    /// Clear all "no spill" marks.
    pub fn clearNoSpill(self: *Self) void {
        self.nospill = 0;
    }

    /// Check if a register is free.
    pub fn isRegFree(self: *Self, r: u4) bool {
        return self.used >> r & 1 == 0;
    }

    /// Get the value currently in a register (if any).
    pub fn getRegValue(self: *Self, r: u4) ?u32 {
        return self.regs[r].value_id;
    }

    /// Get the register holding a value, if any.
    /// Returns the lowest register number if value is in multiple registers.
    pub fn getRegister(self: *Self, value_id: u32) ?u4 {
        const vs = self.values.getPtr(value_id) orelse return null;
        if (vs.regs == 0) return null;
        return @intCast(@ctz(vs.regs));
    }

    /// Get the spill slot for a value, if spilled.
    pub fn getSpillSlot(self: *Self, value_id: u32) ?u16 {
        const vs = self.values.getPtr(value_id) orelse return null;
        return vs.spill_slot;
    }

    /// Set the spill slot for a value.
    pub fn setSpillSlot(self: *Self, value_id: u32, slot: u16) !void {
        var vs = try self.getOrCreateValueState(value_id);
        vs.spill_slot = slot;
    }

    /// Mark a value as a constant (rematerializable).
    pub fn markAsConst(self: *Self, value_id: u32, const_idx: u16) !void {
        var vs = try self.getOrCreateValueState(value_id);
        vs.is_rematerializable = true;
        vs.const_idx = const_idx;
    }

    /// Mark a value as living in a local slot.
    pub fn markAsLocal(self: *Self, value_id: u32, slot: u16) !void {
        var vs = try self.getOrCreateValueState(value_id);
        vs.local_slot = slot;
    }

    /// Debug: dump current register state
    pub fn dumpState(self: *Self) void {
        log.debug("Register state:", .{});
        for (0..NUM_REGISTERS) |i| {
            const r: u4 = @intCast(i);
            if (self.regs[r].value_id) |vid| {
                const vs = self.values.getPtr(vid);
                const dist = if (vs) |v| v.nextUseDist() else 0;
                log.debug("  r{d}: value {d} (next use at {d})", .{ r, vid, dist });
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "basic allocation" {
    const allocator = std.testing.allocator;
    var state = RegAllocState.init(allocator);
    defer state.deinit();

    // Allocate first register
    const r1 = try state.allocReg(ALLOCATABLE_MASK);
    try std.testing.expectEqual(@as(u4, 0), r1);

    // Assign a value to it
    try state.assignReg(100, r1);

    // Allocate second register
    const r2 = try state.allocReg(ALLOCATABLE_MASK);
    try std.testing.expectEqual(@as(u4, 1), r2);

    // Free first register
    state.freeReg(r1);

    // Allocate again - should get r0
    const r3 = try state.allocReg(ALLOCATABLE_MASK);
    try std.testing.expectEqual(@as(u4, 0), r3);
}

test "spill farthest use" {
    const allocator = std.testing.allocator;
    var state = RegAllocState.init(allocator);
    defer state.deinit();

    // Add uses for all values - varying distances
    // Value 1 has the farthest use (dist 100), should be spilled first
    for (1..NUM_REGISTERS + 1) |i| {
        const dist: u32 = if (i == 1) 100 else @intCast(i); // Value 1 has farthest use
        try state.addUse(@intCast(i), dist);
    }

    // Allocate all registers
    for (0..NUM_REGISTERS) |i| {
        const r = try state.allocReg(ALLOCATABLE_MASK);
        try state.assignReg(@intCast(i + 1), r);
    }

    // Now all registers are full. Allocate one more.
    // Should spill value 1 (farthest use at dist 100)
    state.setSpillBase(200);
    const r = try state.allocReg(ALLOCATABLE_MASK);

    // Verify we got a register and value 1 was spilled
    try std.testing.expect(r < NUM_REGISTERS);
    const vs1 = state.getValueState(1).?;
    try std.testing.expectEqual(@as(?u16, 200), vs1.spill_slot);
}

test "use list ordering" {
    const allocator = std.testing.allocator;
    var state = RegAllocState.init(allocator);
    defer state.deinit();

    // Add uses in random order
    try state.addUse(1, 20);
    try state.addUse(1, 5);
    try state.addUse(1, 15);
    try state.addUse(1, 10);

    // Verify they're in sorted order
    const vs = state.getValueState(1).?;
    var u = vs.uses;
    var last_dist: u32 = 0;
    while (u) |use| {
        try std.testing.expect(use.dist >= last_dist);
        last_dist = use.dist;
        u = use.next;
    }
}
