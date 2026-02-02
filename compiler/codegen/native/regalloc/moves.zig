//! Parallel moves: convert parallel move semantics to sequential moves.
//!
//! Ported from regalloc2's `src/moves.rs`.
//!
//! A parallel move set represents moves where all reads happen before all
//! writes (as if simultaneous). This module converts them to a sequence of
//! moves that achieves the same effect, using a scratch register if needed
//! to break cycles.

const std = @import("std");
const operand_mod = @import("operand.zig");
const index = @import("index.zig");

const Allocation = operand_mod.Allocation;
const PReg = index.PReg;

//=============================================================================
// Move Types
//=============================================================================

/// A single move with auxiliary data.
pub fn Move(comptime T: type) type {
    return struct {
        from: Allocation,
        to: Allocation,
        data: T,
    };
}

/// A list of moves to be performed in sequence.
/// Using ArrayList for dynamic sizing (Rust uses SmallVec<16>).
pub fn MoveVec(comptime T: type) type {
    return std.ArrayListUnmanaged(Move(T));
}

/// A list of moves that may need a scratch register.
/// If scratch is needed, Allocation.none() appears in the moves.
pub fn MoveVecWithScratch(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        /// No scratch was needed.
        no_scratch: MoveVec(T),
        /// A scratch space was used (Allocation.none() in moves).
        scratch: MoveVec(T),

        /// Fill in the scratch space with the given allocation.
        /// Returns a final list of moves.
        pub fn withScratch(self: *Self, allocator: std.mem.Allocator, scratch: Allocation) !MoveVec(T) {
            switch (self.*) {
                .no_scratch => |moves| return moves,
                .scratch => |*moves| {
                    for (moves.items) |*m| {
                        if (m.from.isNone()) {
                            m.from = scratch;
                        }
                        if (m.to.isNone()) {
                            m.to = scratch;
                        }
                    }
                    _ = allocator;
                    return moves.*;
                },
            }
        }

        /// Unwrap without a scratch register. Returns null if scratch is needed.
        pub fn withoutScratch(self: Self) ?MoveVec(T) {
            return switch (self) {
                .no_scratch => |moves| moves,
                .scratch => null,
            };
        }

        /// Do we need a scratch register?
        pub fn needsScratch(self: Self) bool {
            return switch (self) {
                .no_scratch => false,
                .scratch => true,
            };
        }

        /// Get the underlying moves (regardless of scratch status).
        pub fn getMoves(self: *Self) *MoveVec(T) {
            return switch (self.*) {
                .no_scratch => |*m| m,
                .scratch => |*m| m,
            };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .no_scratch => |*m| m.deinit(allocator),
                .scratch => |*m| m.deinit(allocator),
            }
        }
    };
}

//=============================================================================
// ParallelMoves - Resolve parallel moves to sequential
//=============================================================================

/// A `ParallelMoves` represents a list of alloc-to-alloc moves that
/// must happen in parallel -- i.e., all reads of sources semantically
/// happen before all writes of destinations, and destinations are
/// allowed to overwrite sources. It can compute a list of sequential
/// moves that will produce the equivalent data movement, possibly
/// using a scratch register if one is necessary.
pub fn ParallelMoves(comptime T: type) type {
    return struct {
        const Self = @This();

        parallel_moves: MoveVec(T),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .parallel_moves = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.parallel_moves.deinit(self.allocator);
        }

        pub fn add(self: *Self, from: Allocation, to: Allocation, data: T) !void {
            try self.parallel_moves.append(self.allocator, .{
                .from = from,
                .to = to,
                .data = data,
            });
        }

        /// Check if any source overlaps any destination.
        /// Assumes moves are already sorted by destination.
        fn sourcesOverlapDests(self: *const Self) bool {
            for (self.parallel_moves.items) |m| {
                // Binary search for this source in destinations
                const src_bits = m.from.bits;
                var left: usize = 0;
                var right: usize = self.parallel_moves.items.len;
                while (left < right) {
                    const mid = left + (right - left) / 2;
                    const dst_bits = self.parallel_moves.items[mid].to.bits;
                    if (dst_bits < src_bits) {
                        left = mid + 1;
                    } else if (dst_bits > src_bits) {
                        right = mid;
                    } else {
                        return true; // Found overlap
                    }
                }
            }
            return false;
        }

        /// Create a u64 sort key from two u32 values (dst in high bits, src in low bits).
        fn u64Key(dst: u32, src: u32) u64 {
            return @as(u64, src) | (@as(u64, dst) << 32);
        }

        /// Resolve the parallel-moves problem to a sequence of separate moves.
        ///
        /// Sometimes, if there is a cycle, a scratch register is necessary.
        /// In this case, `Allocation.none()` is returned to represent the
        /// scratch register. The caller may choose to always hold a separate
        /// scratch register unused, or may dynamically find/create one.
        pub fn resolve(self: *Self) !MoveVecWithScratch(T) {
            // Easy case: zero or one move
            if (self.parallel_moves.items.len <= 1) {
                const moves = self.parallel_moves;
                self.parallel_moves = .{}; // Take ownership
                return .{ .no_scratch = moves };
            }

            // Sort moves by (dst, src) for efficient lookup
            std.mem.sort(Move(T), self.parallel_moves.items, {}, struct {
                fn lessThan(_: void, a: Move(T), b: Move(T)) bool {
                    const key_a = u64Key(a.to.bits, a.from.bits);
                    const key_b = u64Key(b.to.bits, b.from.bits);
                    return key_a < key_b;
                }
            }.lessThan);

            // Remove duplicates (cheap since sorted)
            if (self.parallel_moves.items.len > 1) {
                var write_idx: usize = 1;
                for (self.parallel_moves.items[1..]) |m| {
                    const prev = self.parallel_moves.items[write_idx - 1];
                    if (m.from.bits != prev.from.bits or m.to.bits != prev.to.bits) {
                        self.parallel_moves.items[write_idx] = m;
                        write_idx += 1;
                    }
                }
                self.parallel_moves.shrinkRetainingCapacity(write_idx);
            }

            // Remove self-moves (src == dst)
            {
                var write_idx: usize = 0;
                for (self.parallel_moves.items) |m| {
                    if (m.from.bits != m.to.bits) {
                        self.parallel_moves.items[write_idx] = m;
                        write_idx += 1;
                    }
                }
                self.parallel_moves.shrinkRetainingCapacity(write_idx);
            }

            // If no sources overlap destinations, just return
            if (!self.sourcesOverlapDests()) {
                const moves = self.parallel_moves;
                self.parallel_moves = .{};
                return .{ .no_scratch = moves };
            }

            // Construct must_come_before graph: move[i] must come before move[must_come_before[i]]
            const NONE = std.math.maxInt(usize);
            var must_come_before = try self.allocator.alloc(usize, self.parallel_moves.items.len);
            defer self.allocator.free(must_come_before);

            for (self.parallel_moves.items, 0..) |m, i| {
                // Binary search for a move whose destination == this source
                const src_bits = m.from.bits;
                var left: usize = 0;
                var right: usize = self.parallel_moves.items.len;
                var found: usize = NONE;
                while (left < right) {
                    const mid = left + (right - left) / 2;
                    const dst_bits = self.parallel_moves.items[mid].to.bits;
                    if (dst_bits < src_bits) {
                        left = mid + 1;
                    } else if (dst_bits > src_bits) {
                        right = mid;
                    } else {
                        found = mid;
                        break;
                    }
                }
                must_come_before[i] = found;
            }

            // DFS state
            const State = enum { todo, pending, done };
            var state = try self.allocator.alloc(State, self.parallel_moves.items.len);
            defer self.allocator.free(state);
            @memset(state, .todo);

            var stack = std.ArrayListUnmanaged(usize){};
            defer stack.deinit(self.allocator);

            var ret = MoveVec(T){};
            errdefer ret.deinit(self.allocator);

            var scratch_used = false;

            // Process all nodes
            while (std.mem.indexOfScalar(State, state, .todo)) |next| {
                try stack.append(self.allocator, next);
                state[next] = .pending;

                while (stack.items.len > 0) {
                    const top = stack.items[stack.items.len - 1];

                    const next_move = must_come_before[top];
                    if (next_move == NONE or state[next_move] == .done) {
                        // No dependency or dependency done - emit this move
                        try ret.append(self.allocator, self.parallel_moves.items[top]);
                        state[top] = .done;
                        _ = stack.pop();
                        // Also emit all remaining on stack
                        while (stack.items.len > 0) {
                            const idx = stack.pop().?;
                            try ret.append(self.allocator, self.parallel_moves.items[idx]);
                            state[idx] = .done;
                        }
                    } else if (state[next_move] == .todo) {
                        // Push dependency
                        try stack.append(self.allocator, next_move);
                        state[next_move] = .pending;
                    } else {
                        // Cycle found! Emit cyclic-move sequence.
                        // For cycle { B := A, C := B, A := C } we emit:
                        //   A := scratch
                        //   B := A
                        //   C := B
                        //   scratch := C
                        // Then reverse to get:
                        //   scratch := C
                        //   C := B
                        //   B := A
                        //   A := scratch

                        state[top] = .done;
                        _ = stack.pop();

                        const m = self.parallel_moves.items[top];
                        scratch_used = true;

                        // Emit: scratch -> dst (will become: dst <- scratch after reverse)
                        try ret.append(self.allocator, .{
                            .from = Allocation.none(),
                            .to = m.to,
                            .data = m.data,
                        });

                        // Emit moves in cycle until we hit next_move
                        while (stack.items.len > 0) {
                            const move_idx = stack.pop().?;
                            state[move_idx] = .done;
                            try ret.append(self.allocator, self.parallel_moves.items[move_idx]);
                            if (move_idx == next_move) {
                                break;
                            }
                        }

                        // Emit: src -> scratch (will become: scratch <- src after reverse)
                        try ret.append(self.allocator, .{
                            .from = m.from,
                            .to = Allocation.none(),
                            .data = std.mem.zeroes(T),
                        });
                    }
                }
            }

            // Reverse for correct order
            std.mem.reverse(Move(T), ret.items);

            self.parallel_moves.deinit(self.allocator);
            self.parallel_moves = .{};

            if (scratch_used) {
                return .{ .scratch = ret };
            } else {
                return .{ .no_scratch = ret };
            }
        }
    };
}

//=============================================================================
// MoveAndScratchResolver - Handle scratch registers and stack-to-stack moves
//=============================================================================

/// Final stage of move resolution: finding or using scratch registers,
/// creating them if necessary by using stackslots, and ensuring that the
/// final list of moves contains no stack-to-stack moves.
pub fn MoveAndScratchResolver(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        /// Function to find a free register at the current location.
        find_free_reg: *const fn () ?Allocation,
        /// Function to get a stackslot if needed.
        get_stackslot: *const fn () Allocation,
        /// Function to check if an allocation is a stack slot.
        is_stack_alloc: *const fn (Allocation) bool,
        /// Use this register if no free register is available for stack-to-stack moves.
        borrowed_scratch_reg: PReg,

        pub fn compute(self: Self, moves_with_scratch: *MoveVecWithScratch(T)) !MoveVec(T) {
            // First, resolve cycles using scratch
            var moves: MoveVec(T) = undefined;
            if (moves_with_scratch.needsScratch()) {
                const scratch = self.find_free_reg() orelse self.get_stackslot();
                moves = try moves_with_scratch.withScratch(self.allocator, scratch);
            } else {
                moves = moves_with_scratch.withoutScratch().?;
            }

            // Check for stack-to-stack moves
            var has_stack_to_stack = false;
            for (moves.items) |m| {
                if (self.is_stack_alloc(m.from) and self.is_stack_alloc(m.to)) {
                    has_stack_to_stack = true;
                    break;
                }
            }
            if (!has_stack_to_stack) {
                return moves;
            }

            // Allocate scratch register for stack-to-stack moves
            const scratch_reg = self.find_free_reg() orelse Allocation.reg(self.borrowed_scratch_reg);
            const save_slot: ?Allocation = if (self.find_free_reg() == null) self.get_stackslot() else null;

            var scratch_dirty = false;
            var save_dirty = true;

            var result = MoveVec(T){};
            errdefer result.deinit(self.allocator);

            for (moves.items) |m| {
                if (self.is_stack_alloc(m.from) and self.is_stack_alloc(m.to)) {
                    // Stack-to-stack move: need to go through scratch register

                    // Save scratch if needed
                    if (save_slot) |slot| {
                        if (save_dirty) {
                            try result.append(self.allocator, .{
                                .from = scratch_reg,
                                .to = slot,
                                .data = std.mem.zeroes(T),
                            });
                            save_dirty = false;
                        }
                    }

                    // Stack -> scratch -> stack
                    try result.append(self.allocator, .{
                        .from = m.from,
                        .to = scratch_reg,
                        .data = m.data,
                    });
                    try result.append(self.allocator, .{
                        .from = scratch_reg,
                        .to = m.to,
                        .data = m.data,
                    });
                    scratch_dirty = true;
                } else {
                    // Not stack-to-stack, but handle scratch register state
                    if (m.from.bits == scratch_reg.bits and scratch_dirty) {
                        // Restore scratch before reading from it
                        if (save_slot) |slot| {
                            try result.append(self.allocator, .{
                                .from = slot,
                                .to = scratch_reg,
                                .data = std.mem.zeroes(T),
                            });
                            scratch_dirty = false;
                        }
                    }
                    if (m.to.bits == scratch_reg.bits) {
                        // Writing to scratch, so it doesn't matter what was there
                        scratch_dirty = false;
                        save_dirty = true;
                    }
                    try result.append(self.allocator, m);
                }
            }

            // Restore scratch if dirty at the end
            if (save_slot) |slot| {
                if (scratch_dirty) {
                    try result.append(self.allocator, .{
                        .from = slot,
                        .to = scratch_reg,
                        .data = std.mem.zeroes(T),
                    });
                }
            }

            moves.deinit(self.allocator);
            return result;
        }
    };
}

//=============================================================================
// Tests
//=============================================================================

test "ParallelMoves - no moves" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(!result.needsScratch());
    try std.testing.expectEqual(@as(usize, 0), result.getMoves().items.len);
}

test "ParallelMoves - single move" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    const r0 = Allocation.reg(PReg.new(0, .int));
    const r1 = Allocation.reg(PReg.new(1, .int));
    try pm.add(r0, r1, {});

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(!result.needsScratch());
    const moves = result.getMoves().items;
    try std.testing.expectEqual(@as(usize, 1), moves.len);
    try std.testing.expect(moves[0].from.asReg().?.eql(PReg.new(0, .int)));
    try std.testing.expect(moves[0].to.asReg().?.eql(PReg.new(1, .int)));
}

test "ParallelMoves - no overlap" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    const r0 = Allocation.reg(PReg.new(0, .int));
    const r1 = Allocation.reg(PReg.new(1, .int));
    const r2 = Allocation.reg(PReg.new(2, .int));
    const r3 = Allocation.reg(PReg.new(3, .int));

    // r0 -> r2, r1 -> r3 (no overlap)
    try pm.add(r0, r2, {});
    try pm.add(r1, r3, {});

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(!result.needsScratch());
    try std.testing.expectEqual(@as(usize, 2), result.getMoves().items.len);
}

test "ParallelMoves - chain (no cycle)" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    const r0 = Allocation.reg(PReg.new(0, .int));
    const r1 = Allocation.reg(PReg.new(1, .int));
    const r2 = Allocation.reg(PReg.new(2, .int));

    // r0 -> r1 -> r2 (chain, not cycle)
    try pm.add(r0, r1, {});
    try pm.add(r1, r2, {});

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(!result.needsScratch());
    const moves = result.getMoves().items;
    try std.testing.expectEqual(@as(usize, 2), moves.len);

    // Must do r1->r2 before r0->r1
    try std.testing.expect(moves[0].from.asReg().?.eql(PReg.new(1, .int)));
    try std.testing.expect(moves[0].to.asReg().?.eql(PReg.new(2, .int)));
    try std.testing.expect(moves[1].from.asReg().?.eql(PReg.new(0, .int)));
    try std.testing.expect(moves[1].to.asReg().?.eql(PReg.new(1, .int)));
}

test "ParallelMoves - simple swap (cycle)" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    const r0 = Allocation.reg(PReg.new(0, .int));
    const r1 = Allocation.reg(PReg.new(1, .int));

    // r0 <-> r1 (swap = cycle)
    try pm.add(r0, r1, {});
    try pm.add(r1, r0, {});

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(result.needsScratch());
    const moves = result.getMoves().items;
    // Should have 3 moves: scratch := one, one := other, other := scratch
    try std.testing.expect(moves.len >= 3);

    // Verify scratch is used
    var has_none_src = false;
    var has_none_dst = false;
    for (moves) |m| {
        if (m.from.isNone()) has_none_src = true;
        if (m.to.isNone()) has_none_dst = true;
    }
    try std.testing.expect(has_none_src);
    try std.testing.expect(has_none_dst);
}

test "ParallelMoves - self move removed" {
    var pm = ParallelMoves(void).init(std.testing.allocator);
    defer pm.deinit();

    const r0 = Allocation.reg(PReg.new(0, .int));
    const r1 = Allocation.reg(PReg.new(1, .int));

    // r0 -> r0 (self move, should be removed)
    // r0 -> r1 (real move)
    try pm.add(r0, r0, {});
    try pm.add(r0, r1, {});

    var result = try pm.resolve();
    defer result.deinit(std.testing.allocator);

    try std.testing.expect(!result.needsScratch());
    const moves = result.getMoves().items;
    try std.testing.expectEqual(@as(usize, 1), moves.len);
    try std.testing.expect(moves[0].from.asReg().?.eql(PReg.new(0, .int)));
    try std.testing.expect(moves[0].to.asReg().?.eql(PReg.new(1, .int)));
}

test "MoveVecWithScratch - withScratch" {
    var moves = MoveVec(void){};
    defer moves.deinit(std.testing.allocator);

    const r0 = Allocation.reg(PReg.new(0, .int));
    const scratch = Allocation.reg(PReg.new(10, .int));

    try moves.append(std.testing.allocator, .{ .from = Allocation.none(), .to = r0, .data = {} });
    try moves.append(std.testing.allocator, .{ .from = r0, .to = Allocation.none(), .data = {} });

    var with_scratch = MoveVecWithScratch(void){ .scratch = moves };
    const result = try with_scratch.withScratch(std.testing.allocator, scratch);

    // Check scratch was filled in
    try std.testing.expect(result.items[0].from.asReg().?.eql(PReg.new(10, .int)));
    try std.testing.expect(result.items[1].to.asReg().?.eql(PReg.new(10, .int)));
}
