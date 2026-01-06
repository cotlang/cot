//! Cycle Collector for ARC
//!
//! Detects and collects reference cycles that pure reference counting cannot handle.
//! Uses a trial-deletion algorithm similar to Python's garbage collector:
//!
//! 1. Track "cycle candidates" - objects whose refcount was decremented but didn't reach 0
//! 2. Periodically run cycle detection:
//!    - Trial-decrement: For each candidate, simulate decrementing references
//!    - Scan: Objects with trial refcount == 0 are garbage (only reachable via cycles)
//!    - Collect: Free the garbage objects
//!
//! ## Usage
//!
//! ```zig
//! var collector = CycleCollector.init(allocator);
//! defer collector.deinit();
//!
//! // When arc.release decrements but doesn't free:
//! collector.addCandidate(value);
//!
//! // Periodically (or when memory pressure is high):
//! const freed = collector.collect();
//! ```
//!
//! ## Container Types
//!
//! Only container types (maps, records, objects) can form cycles.
//! Leaf types (strings, decimals, boxed ints) cannot reference other objects.

const std = @import("std");
const arc = @import("arc.zig");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Tag = value_mod.Tag;
const OrderedMap = @import("ordered_map.zig").OrderedMap;

const log = std.log.scoped(.cycle_collector);

/// Statistics about cycle collection
pub const CollectStats = struct {
    /// Number of candidates examined
    candidates_examined: usize = 0,
    /// Number of objects freed
    objects_freed: usize = 0,
    /// Number of cycles detected
    cycles_detected: usize = 0,
};

/// Cycle Collector state
pub const CycleCollector = struct {
    allocator: std.mem.Allocator,

    /// Set of potential cycle members (objects with refcount > 0 after decrement)
    /// Key is the raw pointer value, Value is the Value struct
    candidates: std.AutoHashMap(usize, Value),

    /// Threshold for automatic collection (collect when candidates exceed this)
    collection_threshold: usize,

    /// Total objects freed by this collector
    total_freed: usize,

    /// Total collections performed
    total_collections: usize,

    const Self = @This();

    /// Default threshold for automatic collection
    const DEFAULT_THRESHOLD: usize = 1000;

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .candidates = std.AutoHashMap(usize, Value).init(allocator),
            .collection_threshold = DEFAULT_THRESHOLD,
            .total_freed = 0,
            .total_collections = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.candidates.deinit();
    }

    /// Add a value as a potential cycle member.
    /// Called when refcount is decremented but doesn't reach 0.
    /// Only container types (maps, objects, records) can form cycles.
    pub fn addCandidate(self: *Self, value: Value) void {
        // Only container types can form cycles
        if (!canFormCycle(value)) return;

        // Get the heap pointer
        const ptr = getHeapPtr(value) orelse return;
        const key = @intFromPtr(ptr);

        // Mark as cycle candidate in header
        const header = arc.getHeader(ptr);
        var flags = header.getFlags();
        flags.cycle_candidate = true;
        header.setFlags(flags);

        // Add to candidates set
        self.candidates.put(key, value) catch {
            log.warn("Failed to add cycle candidate - out of memory", .{});
        };

        log.debug("Added cycle candidate: ptr={*} refcount={d}", .{ ptr, header.refcount });

        // Check if we should run collection
        if (self.candidates.count() >= self.collection_threshold) {
            _ = self.collect();
        }
    }

    /// Remove a candidate (called when object is freed normally)
    pub fn removeCandidate(self: *Self, value: Value) void {
        const ptr = getHeapPtr(value) orelse return;
        const key = @intFromPtr(ptr);
        _ = self.candidates.remove(key);
    }

    /// Run cycle collection.
    /// Returns statistics about what was collected.
    pub fn collect(self: *Self) CollectStats {
        var stats = CollectStats{};

        if (self.candidates.count() == 0) {
            return stats;
        }

        log.debug("Starting cycle collection with {d} candidates", .{self.candidates.count()});

        self.total_collections += 1;
        stats.candidates_examined = self.candidates.count();

        // Phase 1: Trial decrement - simulate decrementing all outgoing references
        // We store trial refcounts in a separate map to avoid modifying actual headers
        var trial_refcounts = std.AutoHashMap(usize, i32).init(self.allocator);
        defer trial_refcounts.deinit();

        // Initialize trial refcounts from actual refcounts
        var iter = self.candidates.iterator();
        while (iter.next()) |entry| {
            const ptr: *anyopaque = @ptrFromInt(entry.key_ptr.*);
            const header = arc.getHeaderConst(ptr);
            trial_refcounts.put(entry.key_ptr.*, @intCast(header.refcount)) catch continue;
        }

        // Decrement trial refcounts for all outgoing references from candidates
        iter = self.candidates.iterator();
        while (iter.next()) |entry| {
            const value = entry.value_ptr.*;
            self.trialDecrementReferences(value, &trial_refcounts);
        }

        // Phase 2: Scan - find objects with trial refcount <= 0
        // These are only reachable through the cycle
        var garbage: std.ArrayListUnmanaged(Value) = .empty;
        defer garbage.deinit(self.allocator);

        var trial_iter = trial_refcounts.iterator();
        while (trial_iter.next()) |entry| {
            if (entry.value_ptr.* <= 0) {
                // This object is garbage - only reachable via cycle
                if (self.candidates.get(entry.key_ptr.*)) |value| {
                    garbage.append(self.allocator, value) catch continue;
                    stats.cycles_detected += 1;
                }
            }
        }

        // Phase 3: Collect - free garbage objects
        // First, break all cycles by clearing container contents
        for (garbage.items) |value| {
            self.breakCycles(value);
        }

        // Now free the objects
        for (garbage.items) |value| {
            const ptr = getHeapPtr(value) orelse continue;
            const key = @intFromPtr(ptr);

            // Remove from candidates
            _ = self.candidates.remove(key);

            // Force free the object
            arc.freeObjectForced(value, ptr, self.allocator);
            stats.objects_freed += 1;
        }

        self.total_freed += stats.objects_freed;

        log.debug("Cycle collection complete: examined={d} freed={d} cycles={d}", .{
            stats.candidates_examined,
            stats.objects_freed,
            stats.cycles_detected,
        });

        return stats;
    }

    /// Trial-decrement references from a container
    fn trialDecrementReferences(self: *Self, value: Value, trial_refcounts: *std.AutoHashMap(usize, i32)) void {
        _ = self;
        switch (value.tag()) {
            .object => {
                // Check if it's a map
                if (value.asMap()) |map| {
                    for (map.entries.items) |entry| {
                        if (!entry.deleted) {
                            // Decrement trial refcount for referenced value
                            if (getHeapPtr(entry.value)) |ref_ptr| {
                                const ref_key = @intFromPtr(ref_ptr);
                                if (trial_refcounts.getPtr(ref_key)) |rc| {
                                    rc.* -= 1;
                                }
                            }
                        }
                    }
                }
            },
            .record_ref => {
                // TODO: Handle record references when records can contain Values
            },
            else => {},
        }
    }

    /// Break cycles by clearing container contents
    fn breakCycles(self: *Self, value: Value) void {
        _ = self;
        switch (value.tag()) {
            .object => {
                if (value.asMap()) |map| {
                    // Clear the map to break cycles
                    // The map's deinit will be called when freed
                    map.entries.clearRetainingCapacity();
                    map.index.clearRetainingCapacity();
                    map.access_order.clearRetainingCapacity();
                }
            },
            .record_ref => {
                // TODO: Handle record cycle breaking
            },
            else => {},
        }
    }

    /// Clear all candidates without collecting
    pub fn clear(self: *Self) void {
        // Clear cycle_candidate flag on all candidates
        var iter = self.candidates.iterator();
        while (iter.next()) |entry| {
            const ptr: *anyopaque = @ptrFromInt(entry.key_ptr.*);
            const header = arc.getHeader(ptr);
            var flags = header.getFlags();
            flags.cycle_candidate = false;
            header.setFlags(flags);
        }
        self.candidates.clearRetainingCapacity();
    }

    /// Get number of current candidates
    pub fn candidateCount(self: *const Self) usize {
        return self.candidates.count();
    }

    /// Set collection threshold
    pub fn setThreshold(self: *Self, threshold: usize) void {
        self.collection_threshold = threshold;
    }
};

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a value type can form cycles
fn canFormCycle(value: Value) bool {
    return switch (value.tag()) {
        .object, .record_ref => true,
        else => false,
    };
}

/// Extract heap pointer from a Value
fn getHeapPtr(value: Value) ?*anyopaque {
    return switch (value.tag()) {
        .string => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .fixed_string => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .implied_decimal, .fixed_decimal => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .record_ref => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .object => blk: {
            const ptr = value.bits & Value.OBJ_PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .null_val, .boolean, .integer, .float, .handle => null,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "CycleCollector: init and deinit" {
    var collector = CycleCollector.init(std.testing.allocator);
    defer collector.deinit();

    try std.testing.expectEqual(@as(usize, 0), collector.candidateCount());
    try std.testing.expectEqual(@as(usize, 0), collector.total_freed);
}

test "CycleCollector: collect with no candidates" {
    var collector = CycleCollector.init(std.testing.allocator);
    defer collector.deinit();

    const stats = collector.collect();
    try std.testing.expectEqual(@as(usize, 0), stats.candidates_examined);
    try std.testing.expectEqual(@as(usize, 0), stats.objects_freed);
}

test "CycleCollector: add non-container type is ignored" {
    var collector = CycleCollector.init(std.testing.allocator);
    defer collector.deinit();

    // Integers and other leaf types should be ignored
    collector.addCandidate(Value.initInt(42));
    try std.testing.expectEqual(@as(usize, 0), collector.candidateCount());

    // Null should be ignored
    collector.addCandidate(Value.null_val);
    try std.testing.expectEqual(@as(usize, 0), collector.candidateCount());
}

test "CycleCollector: add map as candidate" {
    const allocator = std.testing.allocator;
    var collector = CycleCollector.init(allocator);
    defer collector.deinit();

    // Create a map
    const map = try arc.create(allocator, OrderedMap);
    map.* = OrderedMap.init(allocator, .{});
    const map_val = Value.initMap(map);

    // Add as candidate
    collector.addCandidate(map_val);
    try std.testing.expectEqual(@as(usize, 1), collector.candidateCount());

    // Check cycle_candidate flag was set
    const header = arc.getHeader(@ptrFromInt(map_val.bits & Value.OBJ_PTR_MASK));
    try std.testing.expect(header.getFlags().cycle_candidate);

    // Clean up - remove from candidates and release
    collector.clear();
    arc.release(map_val, allocator);
}

test "CycleCollector: detect simple cycle" {
    const allocator = std.testing.allocator;
    var collector = CycleCollector.init(allocator);
    collector.setThreshold(1000); // Prevent auto-collection
    defer collector.deinit();

    // Create two maps that reference each other
    const map1 = try arc.create(allocator, OrderedMap);
    map1.* = OrderedMap.init(allocator, .{});
    const map1_val = Value.initMap(map1);

    const map2 = try arc.create(allocator, OrderedMap);
    map2.* = OrderedMap.init(allocator, .{});
    const map2_val = Value.initMap(map2);

    // map1["other"] = map2, map2["other"] = map1
    arc.retain(map2_val);
    _ = try map1.set("other", map2_val);

    arc.retain(map1_val);
    _ = try map2.set("other", map1_val);

    // Both maps now have refcount 2 (original + reference from other map)
    try std.testing.expectEqual(@as(?u32, 2), arc.getRefcount(map1_val));
    try std.testing.expectEqual(@as(?u32, 2), arc.getRefcount(map2_val));

    // Simulate "dropping" the original references
    arc.release(map1_val, allocator);
    arc.release(map2_val, allocator);

    // Both should still have refcount 1 (from cycle)
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(map1_val));
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(map2_val));

    // Add as cycle candidates
    collector.addCandidate(map1_val);
    collector.addCandidate(map2_val);

    // Run collection - should detect and free the cycle
    const stats = collector.collect();

    try std.testing.expect(stats.objects_freed >= 2);
    try std.testing.expect(stats.cycles_detected >= 2);
}

test "CycleCollector: non-cycle container not collected" {
    const allocator = std.testing.allocator;
    var collector = CycleCollector.init(allocator);
    collector.setThreshold(1000);
    defer collector.deinit();

    // Create a map with no cycles
    const map = try arc.create(allocator, OrderedMap);
    map.* = OrderedMap.init(allocator, .{});
    const map_val = Value.initMap(map);

    // Add some non-container values
    _ = try map.set("x", Value.initInt(42));
    _ = try map.set("y", Value.initInt(99));

    // Retain to simulate external reference
    arc.retain(map_val);

    // Add as candidate (simulating refcount decrement that didn't reach 0)
    collector.addCandidate(map_val);

    // Run collection - should NOT free because there's an external reference
    const stats = collector.collect();

    // Map should not be freed (refcount 2, trial becomes 2, > 0)
    try std.testing.expectEqual(@as(usize, 0), stats.objects_freed);

    // Clean up
    collector.clear();
    arc.release(map_val, allocator);
    arc.release(map_val, allocator);
}
