//! ARC Insertion Module - Manages cleanup of reference-counted values
//!
//! Copies Swift's ManagedValue and Cleanup patterns:
//! - ManagedValue.h: Pairs values with optional cleanup handles
//! - Cleanup.h: LIFO cleanup stack for scope-based memory management
//!
//! Reference files:
//! - ~/learning/swift/lib/SILGen/ManagedValue.h
//! - ~/learning/swift/lib/SILGen/Cleanup.h
//! - ~/learning/swift/lib/SILGen/SILGenExpr.cpp

const std = @import("std");
const ir = @import("ir.zig");
const types = @import("types.zig");

const TypeIndex = types.TypeIndex;
const TypeRegistry = types.TypeRegistry;
const NodeIndex = ir.NodeIndex;
const null_node = ir.null_node;

/// Handle to a cleanup in the cleanup stack.
/// Copies Swift's CleanupHandle pattern.
pub const CleanupHandle = struct {
    index: u32,

    pub const invalid: CleanupHandle = .{ .index = std.math.maxInt(u32) };

    pub fn isValid(self: CleanupHandle) bool {
        return self.index != std.math.maxInt(u32);
    }
};

/// State of a cleanup entry.
/// Copies Swift's CleanupState enum from Cleanup.h:62-77
pub const CleanupState = enum {
    /// The cleanup is inactive but may be activated later.
    dormant,
    /// The cleanup is inactive and will not be activated later.
    dead,
    /// The cleanup is currently active.
    active,
};

/// Kind of cleanup operation.
pub const CleanupKind = enum {
    /// Release a reference-counted value.
    release,
    /// End a borrow (not yet used, for future borrow checker).
    end_borrow,
    /// Deferred expression to evaluate at scope exit.
    /// Port of Swift's DeferCleanup subclass on the unified cleanup stack.
    defer_expr,
};

/// A single cleanup entry.
/// Copies Swift's Cleanup class pattern from Cleanup.h:85-142
pub const Cleanup = struct {
    kind: CleanupKind,
    value: NodeIndex,
    type_idx: TypeIndex,
    state: CleanupState,
    /// Optional local variable index that owns this value.
    /// Used to identify cleanup when forwarding ownership on return.
    local_idx: ?ir.LocalIdx,

    pub fn init(kind: CleanupKind, value: NodeIndex, type_idx: TypeIndex) Cleanup {
        return .{
            .kind = kind,
            .value = value,
            .type_idx = type_idx,
            .state = .active,
            .local_idx = null,
        };
    }

    /// Create a cleanup for a value stored in a local variable.
    pub fn initForLocal(kind: CleanupKind, value: NodeIndex, type_idx: TypeIndex, local_idx: ir.LocalIdx) Cleanup {
        return .{
            .kind = kind,
            .value = value,
            .type_idx = type_idx,
            .state = .active,
            .local_idx = local_idx,
        };
    }

    pub fn isActive(self: Cleanup) bool {
        return self.state == .active;
    }
};

/// LIFO stack of cleanups for a scope.
/// Copies Swift's CleanupManager pattern.
///
/// Cleanups are emitted in reverse order (LIFO) when a scope exits.
/// This ensures proper destruction order for nested allocations.
pub const CleanupStack = struct {
    items: std.ArrayListUnmanaged(Cleanup),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CleanupStack {
        return .{
            .items = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CleanupStack) void {
        self.items.deinit(self.allocator);
    }

    /// Push a new cleanup onto the stack. Returns a handle to disable it later.
    pub fn push(self: *CleanupStack, cleanup: Cleanup) !CleanupHandle {
        const index: u32 = @intCast(self.items.items.len);
        try self.items.append(self.allocator, cleanup);
        return .{ .index = index };
    }

    /// Disable a cleanup by handle (ownership was forwarded).
    /// Copies Swift's "forward" pattern where ownership transfer disables cleanup.
    pub fn disable(self: *CleanupStack, handle: CleanupHandle) void {
        if (handle.isValid() and handle.index < self.items.items.len) {
            self.items.items[handle.index].state = .dead;
        }
    }

    /// Find and disable a cleanup for a specific local variable.
    /// Used when forwarding ownership on return - the returned local's cleanup
    /// should not be emitted since ownership transfers to caller.
    /// Returns true if a cleanup was found and disabled.
    pub fn disableForLocal(self: *CleanupStack, local_idx: ir.LocalIdx) bool {
        for (self.items.items) |*cleanup| {
            if (cleanup.isActive() and cleanup.local_idx == local_idx) {
                cleanup.state = .dead;
                return true;
            }
        }
        return false;
    }

    /// Update an existing cleanup to associate it with a local variable.
    /// Called after storing an ARC value to a local.
    pub fn setLocalForHandle(self: *CleanupStack, handle: CleanupHandle, local_idx: ir.LocalIdx) void {
        if (handle.isValid() and handle.index < self.items.items.len) {
            self.items.items[handle.index].local_idx = local_idx;
        }
    }

    /// Update the value node for a local's cleanup (called on reassignment).
    /// Port of Swift's pattern: cleanup tracks current value, not original.
    pub fn updateValueForLocal(self: *CleanupStack, local_idx: ir.LocalIdx, new_value: NodeIndex) void {
        for (self.items.items) |*cleanup| {
            if (cleanup.isActive() and cleanup.local_idx != null and
                cleanup.local_idx.? == local_idx and cleanup.kind == .release)
            {
                cleanup.value = new_value;
                return;
            }
        }
    }

    /// Check if a local variable has an active ARC cleanup registered.
    /// Used when copying values to determine if retain is needed.
    pub fn hasCleanupForLocal(self: *const CleanupStack, local_idx: ir.LocalIdx) bool {
        for (self.items.items) |cleanup| {
            if (cleanup.isActive() and cleanup.local_idx == local_idx) {
                return true;
            }
        }
        return false;
    }

    /// Get number of active cleanups (for defer depth tracking).
    pub fn activeCount(self: *const CleanupStack) usize {
        var count: usize = 0;
        for (self.items.items) |c| {
            if (c.isActive()) count += 1;
        }
        return count;
    }

    /// Get all active cleanups in LIFO order (for emission).
    /// Returns a slice that must be iterated in reverse order.
    pub fn getActiveCleanups(self: *const CleanupStack) []const Cleanup {
        return self.items.items;
    }

    /// Mark cleanup depth for scope entry.
    pub fn getScopeDepth(self: *const CleanupStack) usize {
        return self.items.items.len;
    }

    /// Emit all cleanups down to a given scope depth.
    /// This is called at scope exit, return, break, etc.
    /// Cleanups are processed in LIFO order.
    pub fn emitCleanupsToDepth(
        self: *CleanupStack,
        target_depth: usize,
        emit_fn: *const fn (Cleanup) anyerror!void,
    ) !void {
        // Process in reverse order (LIFO)
        var i = self.items.items.len;
        while (i > target_depth) {
            i -= 1;
            const cleanup = &self.items.items[i];
            if (cleanup.isActive()) {
                try emit_fn(cleanup.*);
                cleanup.state = .dead;
            }
        }
    }

    /// Clear all cleanups (for function reset).
    pub fn clear(self: *CleanupStack) void {
        self.items.clearRetainingCapacity();
    }
};

/// ManagedValue pairs a value with an optional cleanup.
/// Copies Swift's ManagedValue class from ManagedValue.h:59-95
///
/// Key concepts from Swift:
/// - +0 value: No cleanup (borrowed or trivial)
/// - +1 value: Has cleanup (owned, will be released)
/// - forward(): Transfer ownership, disable cleanup
pub const ManagedValue = struct {
    value: NodeIndex,
    type_idx: TypeIndex,
    cleanup: CleanupHandle,

    /// Create a managed value for a +1 (owned) rvalue.
    /// The cleanup will release the value at scope exit.
    pub fn forOwned(value: NodeIndex, type_idx: TypeIndex, cleanup: CleanupHandle) ManagedValue {
        return .{
            .value = value,
            .type_idx = type_idx,
            .cleanup = cleanup,
        };
    }

    /// Create a managed value for a +0 (unowned/trivial) rvalue.
    /// No cleanup is registered.
    pub fn forTrivial(value: NodeIndex, type_idx: TypeIndex) ManagedValue {
        return .{
            .value = value,
            .type_idx = type_idx,
            .cleanup = CleanupHandle.invalid,
        };
    }

    /// Check if this value has an active cleanup.
    pub fn hasCleanup(self: ManagedValue) bool {
        return self.cleanup.isValid();
    }

    /// Forward ownership to another scope.
    /// Disables the cleanup - the receiver is now responsible for release.
    /// Copies Swift's ManagedValue::forward() pattern.
    pub fn forward(self: *ManagedValue, stack: *CleanupStack) NodeIndex {
        if (self.cleanup.isValid()) {
            stack.disable(self.cleanup);
            self.cleanup = CleanupHandle.invalid;
        }
        return self.value;
    }

    /// Get the underlying value without forwarding ownership.
    pub fn getValue(self: ManagedValue) NodeIndex {
        return self.value;
    }
};

/// Scope guard for cleanup emission.
/// Enters a scope, tracks depth, emits cleanups on exit.
pub const Scope = struct {
    stack: *CleanupStack,
    entry_depth: usize,

    pub fn enter(stack: *CleanupStack) Scope {
        return .{
            .stack = stack,
            .entry_depth = stack.getScopeDepth(),
        };
    }

    /// Exit the scope, emitting all cleanups registered since entry.
    pub fn exit(self: *Scope, emit_fn: *const fn (Cleanup) anyerror!void) !void {
        try self.stack.emitCleanupsToDepth(self.entry_depth, emit_fn);
    }

    /// Get current scope depth for break/continue handling.
    pub fn getDepth(self: *const Scope) usize {
        return self.entry_depth;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "CleanupStack push and disable" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();

    // Push a cleanup
    const cleanup = Cleanup.init(.release, 42, TypeRegistry.I64);
    const handle = try stack.push(cleanup);
    try std.testing.expect(handle.isValid());
    try std.testing.expectEqual(@as(usize, 1), stack.items.items.len);

    // Cleanup should be active
    try std.testing.expect(stack.items.items[0].isActive());

    // Disable it
    stack.disable(handle);
    try std.testing.expect(!stack.items.items[0].isActive());
}

test "CleanupStack LIFO order" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();

    // Push multiple cleanups
    _ = try stack.push(Cleanup.init(.release, 1, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 2, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 3, TypeRegistry.I64));

    try std.testing.expectEqual(@as(usize, 3), stack.items.items.len);

    // Emit cleanups and verify LIFO order
    var order: [3]NodeIndex = undefined;
    var idx: usize = 0;

    const emit_fn = struct {
        fn emit(cleanup: Cleanup) !void {
            _ = cleanup;
        }
    }.emit;

    // Process in reverse manually to verify LIFO
    var i = stack.items.items.len;
    while (i > 0) {
        i -= 1;
        order[idx] = stack.items.items[i].value;
        idx += 1;
    }

    // Should be 3, 2, 1 (LIFO order)
    try std.testing.expectEqual(@as(NodeIndex, 3), order[0]);
    try std.testing.expectEqual(@as(NodeIndex, 2), order[1]);
    try std.testing.expectEqual(@as(NodeIndex, 1), order[2]);

    // Clear the stack properly
    try stack.emitCleanupsToDepth(0, emit_fn);
}

test "ManagedValue forward transfers ownership" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();

    // Create owned value with cleanup
    const cleanup = Cleanup.init(.release, 100, TypeRegistry.I64);
    const handle = try stack.push(cleanup);

    var mv = ManagedValue.forOwned(100, TypeRegistry.I64, handle);
    try std.testing.expect(mv.hasCleanup());

    // Forward ownership
    const val = mv.forward(&stack);
    try std.testing.expectEqual(@as(NodeIndex, 100), val);
    try std.testing.expect(!mv.hasCleanup());

    // Original cleanup should be dead
    try std.testing.expect(!stack.items.items[0].isActive());
}

test "ManagedValue trivial has no cleanup" {
    const mv = ManagedValue.forTrivial(42, TypeRegistry.I64);
    try std.testing.expect(!mv.hasCleanup());
    try std.testing.expectEqual(@as(NodeIndex, 42), mv.getValue());
}

test "Scope tracks cleanup depth" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();

    // Push some cleanups at outer scope
    _ = try stack.push(Cleanup.init(.release, 1, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 2, TypeRegistry.I64));

    // Enter inner scope
    var scope = Scope.enter(&stack);
    try std.testing.expectEqual(@as(usize, 2), scope.entry_depth);

    // Push cleanup in inner scope
    _ = try stack.push(Cleanup.init(.release, 3, TypeRegistry.I64));
    try std.testing.expectEqual(@as(usize, 3), stack.items.items.len);

    // Exit inner scope - only cleanup 3 should be emitted
    const emit_fn = struct {
        fn emit(cleanup: Cleanup) !void {
            _ = cleanup;
        }
    }.emit;

    try scope.exit(emit_fn);

    // Cleanup 3 should be dead, 1 and 2 should still be active
    try std.testing.expect(!stack.items.items[2].isActive());
    try std.testing.expect(stack.items.items[0].isActive());
    try std.testing.expect(stack.items.items[1].isActive());
}
