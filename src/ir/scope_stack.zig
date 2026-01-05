//! Scope Stack for Variable Management
//!
//! Provides hierarchical scope management for the IR lowerer.
//! Replaces manual save/restore patterns with a cleaner push/pop interface.
//!
//! Features:
//! - Nested scopes with parent chain lookup
//! - Variables in inner scopes shadow outer scope variables
//! - Automatic cleanup when popping scopes
//!
//! Usage:
//!   var scopes = ScopeStack.init(allocator);
//!   defer scopes.deinit();
//!
//!   // Enter function scope
//!   try scopes.push();
//!   try scopes.put("x", some_value);
//!
//!   // Enter nested scope (e.g., loop body)
//!   try scopes.push();
//!   try scopes.put("i", loop_counter);  // shadows any outer "i"
//!
//!   // Lookup checks current scope, then parent scopes
//!   const x = scopes.get("x");  // finds in parent scope
//!
//!   // Exit nested scope
//!   scopes.pop();
//!
//!   // Exit function scope
//!   scopes.pop();

const std = @import("std");
const ir = @import("ir.zig");

const Allocator = std.mem.Allocator;

/// A single scope containing variable bindings
pub const Scope = struct {
    /// Variables defined in this scope
    variables: std.StringHashMap(ir.Value),
    /// Parent scope (null for root/global scope)
    parent: ?*Scope,
    /// Allocator used for this scope
    allocator: Allocator,

    pub fn init(allocator: Allocator, parent: ?*Scope) Scope {
        return .{
            .variables = std.StringHashMap(ir.Value).init(allocator),
            .parent = parent,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.variables.deinit();
    }

    /// Look up a variable in this scope only (not parents)
    pub fn getLocal(self: *const Scope, name: []const u8) ?ir.Value {
        return self.variables.get(name);
    }

    /// Look up a variable, checking this scope then parents
    pub fn get(self: *const Scope, name: []const u8) ?ir.Value {
        if (self.variables.get(name)) |v| return v;
        if (self.parent) |p| return p.get(name);
        return null;
    }

    /// Check if a variable exists in this scope or any parent
    pub fn contains(self: *const Scope, name: []const u8) bool {
        return self.get(name) != null;
    }

    /// Put a variable in this scope
    pub fn put(self: *Scope, name: []const u8, value: ir.Value) !void {
        try self.variables.put(name, value);
    }
};

/// Stack of scopes for managing variable visibility
pub const ScopeStack = struct {
    /// The current (innermost) scope
    current: *Scope,
    /// Allocator for creating new scopes
    allocator: Allocator,
    /// Track allocated scopes for cleanup
    allocated_scopes: std.ArrayList(*Scope),

    /// Initialize with a root scope
    pub fn init(allocator: Allocator) !ScopeStack {
        const root = try allocator.create(Scope);
        root.* = Scope.init(allocator, null);

        var allocated: std.ArrayList(*Scope) = .empty;
        try allocated.append(allocator, root);

        return .{
            .current = root,
            .allocator = allocator,
            .allocated_scopes = allocated,
        };
    }

    /// Clean up all scopes
    pub fn deinit(self: *ScopeStack) void {
        for (self.allocated_scopes.items) |scope| {
            scope.deinit();
            self.allocator.destroy(scope);
        }
        self.allocated_scopes.deinit(self.allocator);
    }

    /// Push a new scope (enter a new block/function)
    pub fn push(self: *ScopeStack) !void {
        const new_scope = try self.allocator.create(Scope);
        new_scope.* = Scope.init(self.allocator, self.current);
        try self.allocated_scopes.append(self.allocator, new_scope);
        self.current = new_scope;
    }

    /// Pop the current scope (exit a block/function)
    /// Note: Does NOT deallocate - all scopes freed on deinit()
    pub fn pop(self: *ScopeStack) void {
        if (self.current.parent) |parent| {
            // We don't deallocate here - just move up the chain
            // All scopes are tracked in allocated_scopes and freed on deinit
            self.current = parent;
        }
    }

    /// Look up a variable in the current scope chain
    pub fn get(self: *const ScopeStack, name: []const u8) ?ir.Value {
        return self.current.get(name);
    }

    /// Check if a variable exists in the current scope chain
    pub fn contains(self: *const ScopeStack, name: []const u8) bool {
        return self.current.contains(name);
    }

    /// Put a variable in the current scope
    pub fn put(self: *ScopeStack, name: []const u8, value: ir.Value) !void {
        try self.current.put(name, value);
    }

    /// Get the current scope depth (0 = root)
    pub fn depth(self: *const ScopeStack) usize {
        var d: usize = 0;
        var scope: ?*const Scope = self.current;
        while (scope) |s| {
            if (s.parent == null) break;
            d += 1;
            scope = s.parent;
        }
        return d;
    }

    /// Clear the current scope's variables (for function re-entry)
    pub fn clearCurrent(self: *ScopeStack) void {
        self.current.variables.clearRetainingCapacity();
    }

    /// Reset to root scope (clear all but root, clear root variables)
    pub fn reset(self: *ScopeStack) void {
        // Find root scope
        var root = self.current;
        while (root.parent) |p| {
            root = p;
        }
        self.current = root;

        // Clear root variables
        root.variables.clearRetainingCapacity();

        // Note: Non-root scopes remain allocated but orphaned
        // They'll be freed on deinit()
    }

    /// Iterator for all variables visible in current scope (including parent scopes)
    pub fn visibleVariables(self: *const ScopeStack) VisibleIterator {
        return VisibleIterator{ .scope = self.current };
    }

    /// Iterator for variables in only the current scope (not parent scopes)
    /// Use this for scope-exit cleanup to release only variables declared in the exiting scope
    pub fn currentScopeVariables(self: *const ScopeStack) CurrentScopeIterator {
        return CurrentScopeIterator.init(self.current);
    }

    pub const CurrentScopeIterator = struct {
        inner_iter: std.StringHashMap(ir.Value).Iterator,

        pub fn init(scope: *const Scope) CurrentScopeIterator {
            return .{
                .inner_iter = scope.variables.iterator(),
            };
        }

        pub fn next(self: *CurrentScopeIterator) ?struct { key: []const u8, value: ir.Value } {
            if (self.inner_iter.next()) |entry| {
                return .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
            }
            return null;
        }
    };

    pub const VisibleIterator = struct {
        scope: ?*const Scope,
        inner_iter: ?std.StringHashMap(ir.Value).Iterator = null,

        pub fn next(self: *VisibleIterator) ?struct { key: []const u8, value: ir.Value } {
            while (true) {
                if (self.inner_iter) |*iter| {
                    if (iter.next()) |entry| {
                        return .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
                    }
                }
                // Move to parent scope
                if (self.scope) |s| {
                    self.inner_iter = s.variables.iterator();
                    self.scope = s.parent;
                } else {
                    return null;
                }
            }
        }
    };
};

test "basic scope operations" {
    const allocator = std.testing.allocator;

    var scopes = try ScopeStack.init(allocator);
    defer scopes.deinit();

    // Put in root scope
    const val1 = ir.Value{ .id = 1, .ty = .void };
    try scopes.put("x", val1);

    try std.testing.expect(scopes.get("x") != null);
    try std.testing.expectEqual(@as(usize, 0), scopes.depth());

    // Push new scope
    try scopes.push();
    try std.testing.expectEqual(@as(usize, 1), scopes.depth());

    // Can still see parent variable
    try std.testing.expect(scopes.get("x") != null);

    // Shadow with local variable
    const val2 = ir.Value{ .id = 2, .ty = .void };
    try scopes.put("x", val2);
    try std.testing.expectEqual(@as(u32, 2), scopes.get("x").?.id);

    // Pop scope - back to original
    scopes.pop();
    try std.testing.expectEqual(@as(u32, 1), scopes.get("x").?.id);
}
