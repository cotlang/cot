//! ARC cleanup stack — manages reference-counted value lifetimes.
//!
//! Implements Swift's ManagedValue and Cleanup patterns:
//! - CleanupStack: LIFO stack of cleanup entries per scope
//! - ManagedValue: pairs a value with an optional cleanup handle
//! - Scope: tracks cleanup depth for scope-based emission
//!
//! Cleanups are emitted in reverse order (LIFO) at scope exit, ensuring
//! proper destruction order for nested allocations.

const std = @import("std");
const foundation = @import("foundation");
const ir = @import("ir.zig");

const TypeIndex = foundation.types.TypeIndex;
const TypeRegistry = foundation.types.TypeRegistry;
const NodeIndex = ir.NodeIndex;

/// Handle to a cleanup in the cleanup stack.
pub const CleanupHandle = struct {
    index: u32,

    pub const invalid: CleanupHandle = .{ .index = std.math.maxInt(u32) };

    pub fn isValid(self: CleanupHandle) bool {
        return self.index != std.math.maxInt(u32);
    }
};

/// State of a cleanup entry.
pub const CleanupState = enum {
    dormant,
    dead,
    active,
};

/// Kind of cleanup operation.
pub const CleanupKind = enum {
    release,
    unowned_release,
    weak_release,
    end_borrow,
    defer_expr,
    errdefer_expr,
    scope_destroy,
};

/// A single cleanup entry.
pub const Cleanup = struct {
    kind: CleanupKind,
    value: NodeIndex,
    type_idx: TypeIndex,
    state: CleanupState,
    local_idx: ?ir.LocalIdx,
    func_name: ?[]const u8,

    pub fn init(kind: CleanupKind, value: NodeIndex, type_idx: TypeIndex) Cleanup {
        return .{ .kind = kind, .value = value, .type_idx = type_idx, .state = .active, .local_idx = null, .func_name = null };
    }

    pub fn initForLocal(kind: CleanupKind, value: NodeIndex, type_idx: TypeIndex, local_idx: ir.LocalIdx) Cleanup {
        return .{ .kind = kind, .value = value, .type_idx = type_idx, .state = .active, .local_idx = local_idx, .func_name = null };
    }

    pub fn initScopeDestroy(local_idx: ir.LocalIdx, type_idx: TypeIndex, func_name: []const u8) Cleanup {
        return .{ .kind = .scope_destroy, .value = 0, .type_idx = type_idx, .state = .active, .local_idx = local_idx, .func_name = func_name };
    }

    pub fn isActive(self: Cleanup) bool {
        return self.state == .active;
    }
};

/// LIFO stack of cleanups for a scope.
pub const CleanupStack = struct {
    items: std.ArrayListUnmanaged(Cleanup),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CleanupStack {
        return .{ .items = .{}, .allocator = allocator };
    }

    pub fn deinit(self: *CleanupStack) void {
        self.items.deinit(self.allocator);
    }

    pub fn push(self: *CleanupStack, cleanup: Cleanup) !CleanupHandle {
        const index: u32 = @intCast(self.items.items.len);
        try self.items.append(self.allocator, cleanup);
        return .{ .index = index };
    }

    pub fn disable(self: *CleanupStack, handle: CleanupHandle) void {
        if (handle.isValid() and handle.index < self.items.items.len) {
            self.items.items[handle.index].state = .dead;
        }
    }

    pub fn disableForLocal(self: *CleanupStack, local_idx: ir.LocalIdx) bool {
        for (self.items.items) |*cleanup| {
            if (cleanup.isActive() and cleanup.local_idx == local_idx) {
                cleanup.state = .dead;
                return true;
            }
        }
        return false;
    }

    pub fn setLocalForHandle(self: *CleanupStack, handle: CleanupHandle, local_idx: ir.LocalIdx) void {
        if (handle.isValid() and handle.index < self.items.items.len) {
            self.items.items[handle.index].local_idx = local_idx;
        }
    }

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

    pub fn hasCleanupForLocal(self: *const CleanupStack, local_idx: ir.LocalIdx) bool {
        for (self.items.items) |cleanup| {
            if (cleanup.isActive() and cleanup.local_idx == local_idx) return true;
        }
        return false;
    }

    pub fn findCleanupForLocal(self: *const CleanupStack, local_idx: ir.LocalIdx) ?CleanupHandle {
        for (self.items.items, 0..) |cleanup, i| {
            if (cleanup.isActive() and cleanup.local_idx != null and cleanup.local_idx.? == local_idx)
                return .{ .index = @intCast(i) };
        }
        return null;
    }

    pub fn activeCount(self: *const CleanupStack) usize {
        var count: usize = 0;
        for (self.items.items) |c| {
            if (c.isActive()) count += 1;
        }
        return count;
    }

    pub fn getActiveCleanups(self: *const CleanupStack) []const Cleanup {
        return self.items.items;
    }

    pub fn getScopeDepth(self: *const CleanupStack) usize {
        return self.items.items.len;
    }

    pub fn emitCleanupsToDepth(self: *CleanupStack, target_depth: usize, emit_fn: *const fn (Cleanup) anyerror!void) !void {
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

    pub fn clear(self: *CleanupStack) void {
        self.items.clearRetainingCapacity();
    }
};

/// ManagedValue pairs a value with an optional cleanup.
/// +0 value: no cleanup (borrowed or trivial).
/// +1 value: has cleanup (owned, released at scope exit).
pub const ManagedValue = struct {
    value: NodeIndex,
    type_idx: TypeIndex,
    cleanup: CleanupHandle,

    pub fn forOwned(value: NodeIndex, type_idx: TypeIndex, cleanup: CleanupHandle) ManagedValue {
        return .{ .value = value, .type_idx = type_idx, .cleanup = cleanup };
    }

    pub fn forTrivial(value: NodeIndex, type_idx: TypeIndex) ManagedValue {
        return .{ .value = value, .type_idx = type_idx, .cleanup = CleanupHandle.invalid };
    }

    pub fn hasCleanup(self: ManagedValue) bool {
        return self.cleanup.isValid();
    }

    pub fn forward(self: *ManagedValue, stack: *CleanupStack) NodeIndex {
        if (self.cleanup.isValid()) {
            stack.disable(self.cleanup);
            self.cleanup = CleanupHandle.invalid;
        }
        return self.value;
    }

    pub fn getValue(self: ManagedValue) NodeIndex {
        return self.value;
    }
};

/// Scope guard for cleanup emission.
pub const Scope = struct {
    stack: *CleanupStack,
    entry_depth: usize,

    pub fn enter(stack: *CleanupStack) Scope {
        return .{ .stack = stack, .entry_depth = stack.getScopeDepth() };
    }

    pub fn exit(self: *Scope, emit_fn: *const fn (Cleanup) anyerror!void) !void {
        try self.stack.emitCleanupsToDepth(self.entry_depth, emit_fn);
    }

    pub fn getDepth(self: *const Scope) usize {
        return self.entry_depth;
    }
};

test "CleanupStack push and disable" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();
    const handle = try stack.push(Cleanup.init(.release, 42, TypeRegistry.I64));
    try std.testing.expect(handle.isValid());
    try std.testing.expect(stack.items.items[0].isActive());
    stack.disable(handle);
    try std.testing.expect(!stack.items.items[0].isActive());
}

test "CleanupStack LIFO order" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();
    _ = try stack.push(Cleanup.init(.release, 1, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 2, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 3, TypeRegistry.I64));
    var order: [3]NodeIndex = undefined;
    var idx: usize = 0;
    var i = stack.items.items.len;
    while (i > 0) {
        i -= 1;
        order[idx] = stack.items.items[i].value;
        idx += 1;
    }
    try std.testing.expectEqual(@as(NodeIndex, 3), order[0]);
    try std.testing.expectEqual(@as(NodeIndex, 2), order[1]);
    try std.testing.expectEqual(@as(NodeIndex, 1), order[2]);
}

test "ManagedValue forward transfers ownership" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();
    const handle = try stack.push(Cleanup.init(.release, 100, TypeRegistry.I64));
    var mv = ManagedValue.forOwned(100, TypeRegistry.I64, handle);
    try std.testing.expect(mv.hasCleanup());
    const val = mv.forward(&stack);
    try std.testing.expectEqual(@as(NodeIndex, 100), val);
    try std.testing.expect(!mv.hasCleanup());
    try std.testing.expect(!stack.items.items[0].isActive());
}

test "Scope tracks cleanup depth" {
    var stack = CleanupStack.init(std.testing.allocator);
    defer stack.deinit();
    _ = try stack.push(Cleanup.init(.release, 1, TypeRegistry.I64));
    _ = try stack.push(Cleanup.init(.release, 2, TypeRegistry.I64));
    var scope = Scope.enter(&stack);
    _ = try stack.push(Cleanup.init(.release, 3, TypeRegistry.I64));
    const emit_fn = struct {
        fn emit(_: Cleanup) !void {}
    }.emit;
    try scope.exit(emit_fn);
    try std.testing.expect(!stack.items.items[2].isActive());
    try std.testing.expect(stack.items.items[0].isActive());
    try std.testing.expect(stack.items.items[1].isActive());
}
