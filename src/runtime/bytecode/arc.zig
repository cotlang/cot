//! Automatic Reference Counting (ARC) for Cot Runtime
//!
//! Swift-inspired memory management with three layers:
//! 1. ARC (this module) - compiler-inserted retain/release, immediate deallocation
//! 2. Cycle Collector - background detection of reference cycles (separate module)
//! 3. weak references - for intentional cycles (language feature)
//!
//! ## Memory Layout
//!
//! All heap-allocated objects have an 8-byte header prefix:
//!
//! ```
//! ┌──────────────┬─────────────────────────────────────┐
//! │ ArcHeader    │ Object Data                         │
//! │ (8 bytes)    │ (type-specific)                     │
//! ├──────────────┼─────────────────────────────────────┤
//! │ refcount: u32│ StringRef / Decimal / Record / etc. │
//! │ flags: u32   │                                     │
//! └──────────────┴─────────────────────────────────────┘
//!        ^                    ^
//!        │                    │
//!    header_ptr          object_ptr (what Value stores)
//! ```
//!
//! The NaN-boxed Value pointer points to object data, not the header.
//! To get header: `header_ptr = object_ptr - 8`
//!
//! ## Usage
//!
//! ```zig
//! // Allocate with ARC header
//! const str_ref = try arc.create(allocator, StringRef);
//! str_ref.* = .{ .ptr = data.ptr, .len = data.len };
//! // refcount starts at 1
//!
//! // When copying the value
//! arc.retain(value);  // refcount++
//!
//! // When value goes out of scope
//! arc.release(value, allocator);  // refcount--, free if 0
//! ```

const std = @import("std");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Tag = value_mod.Tag;
const Decimal = value_mod.Decimal;
const BoxedInt = value_mod.BoxedInt;
const StringRef = value_mod.StringRef;
const FixedStringRef = value_mod.FixedStringRef;
const Record = value_mod.Record;
const OrderedMap = @import("ordered_map.zig").OrderedMap;

// Scoped logging
const log = std.log.scoped(.arc);

// ============================================================================
// ARC Header
// ============================================================================

/// Header prefix for all ARC-managed heap objects.
/// Exactly 8 bytes, placed immediately before object data.
pub const ArcHeader = extern struct {
    /// Reference count. Object is freed when this reaches 0.
    /// Starts at 1 on allocation.
    refcount: u32,

    /// Status flags for ARC and cycle collection.
    flags: u32,

    const Self = @This();

    /// Size of ARC header in bytes (for pointer arithmetic)
    pub const SIZE: usize = @sizeOf(Self);

    comptime {
        // Verify header is exactly 8 bytes as specified
        std.debug.assert(SIZE == 8);
    }

    /// Flag bits
    pub const Flags = packed struct(u32) {
        /// Object may be part of a reference cycle (for cycle collector)
        cycle_candidate: bool = false,
        /// Object has weak references pointing to it
        weak_target: bool = false,
        /// Object is being freed - do not access
        poisoned: bool = false,
        /// Reserved for future use
        _reserved: u29 = 0,
    };

    /// Create a new header with refcount = 1
    pub fn init() Self {
        return .{
            .refcount = 1,
            .flags = 0,
        };
    }

    /// Get flags as structured type
    pub fn getFlags(self: *const Self) Flags {
        return @bitCast(self.flags);
    }

    /// Set flags from structured type
    pub fn setFlags(self: *Self, flags: Flags) void {
        self.flags = @bitCast(flags);
    }

    /// Check if object is poisoned (freed or being freed)
    pub fn isPoisoned(self: *const Self) bool {
        return self.getFlags().poisoned;
    }

    /// Mark as poisoned (called during free)
    pub fn poison(self: *Self) void {
        var flags = self.getFlags();
        flags.poisoned = true;
        self.setFlags(flags);
    }
};

// ============================================================================
// Allocation Functions
// ============================================================================

/// Allocate a heap object with ARC header prefix.
/// Returns pointer to object data (header is at ptr - 8).
/// Object starts with refcount = 1.
///
/// NOTE: T must have alignment <= 8 bytes. This is true for all Cot value types.
pub fn create(allocator: std.mem.Allocator, comptime T: type) !*T {
    comptime {
        std.debug.assert(ArcHeader.SIZE == 8);
        // Ensure data follows immediately after header (no padding)
        // This is required for getHeader() to work correctly
        if (@alignOf(T) > 8) {
            @compileError("ARC create: type alignment > 8 not supported, would break header layout");
        }
    }

    const total_size = ArcHeader.SIZE + @sizeOf(T);

    // Allocate with 8-byte alignment (sufficient for header and all our types)
    const bytes = try allocator.alignedAlloc(u8, .@"8", total_size);
    errdefer allocator.free(bytes);

    // Initialize header at the beginning
    const header: *ArcHeader = @ptrCast(@alignCast(bytes.ptr));
    header.* = ArcHeader.init();

    // Get pointer to data (exactly 8 bytes after header start)
    const data_ptr: *T = @ptrCast(@alignCast(bytes.ptr + ArcHeader.SIZE));

    log.debug("ARC create: type={s} size={d} ptr={*} refcount=1", .{
        @typeName(T),
        @sizeOf(T),
        data_ptr,
    });

    return data_ptr;
}

/// Allocate a heap object with ARC header, returning raw pointer.
/// Use when you need to store as *anyopaque.
pub fn createRaw(allocator: std.mem.Allocator, comptime size: usize) !*anyopaque {
    const total_size = ArcHeader.SIZE + size;

    // Allocate with 8-byte alignment
    const bytes = try allocator.alignedAlloc(u8, .@"8", total_size);
    errdefer allocator.free(bytes);

    // Initialize header at the beginning
    const header: *ArcHeader = @ptrCast(@alignCast(bytes.ptr));
    header.* = ArcHeader.init();

    // Return pointer to data area
    return @ptrCast(bytes.ptr + ArcHeader.SIZE);
}

/// Free an ARC-allocated object by typed pointer.
/// Use this when you have a direct pointer to the object, not a Value.
/// This is the inverse of create().
pub fn destroy(allocator: std.mem.Allocator, comptime T: type, ptr: *T) void {
    const header = getHeaderTyped(T, ptr);
    const total_size = ArcHeader.SIZE + @sizeOf(T);

    log.debug("ARC destroy: type={s} ptr={*}", .{ @typeName(T), ptr });

    // Mark as poisoned to catch use-after-free
    header.poison();

    // Get pointer to beginning of allocation (the header)
    const bytes_ptr: [*]align(8) u8 = @ptrCast(@alignCast(header));
    const bytes = bytes_ptr[0..total_size];

    allocator.free(bytes);
}

// ============================================================================
// Header Access
// ============================================================================

/// Get the ARC header for an object pointer.
/// The header is located 8 bytes before the object data.
pub fn getHeader(ptr: *anyopaque) *ArcHeader {
    const byte_ptr: [*]u8 = @ptrCast(ptr);
    return @ptrCast(@alignCast(byte_ptr - ArcHeader.SIZE));
}

/// Get the ARC header for a typed object pointer.
pub fn getHeaderTyped(comptime T: type, ptr: *T) *ArcHeader {
    const byte_ptr: [*]u8 = @ptrCast(ptr);
    return @ptrCast(@alignCast(byte_ptr - ArcHeader.SIZE));
}

/// Get const header (for reading refcount without mutation)
pub fn getHeaderConst(ptr: *const anyopaque) *const ArcHeader {
    const byte_ptr: [*]const u8 = @ptrCast(ptr);
    return @ptrCast(@alignCast(byte_ptr - ArcHeader.SIZE));
}

// ============================================================================
// Reference Counting Operations
// ============================================================================

/// Increment reference count for a Value (if it's heap-allocated).
/// Safe to call on any Value - does nothing for inline types.
pub fn retain(value: Value) void {
    if (getHeapPtr(value)) |ptr| {
        const header = getHeader(ptr);

        if (header.isPoisoned()) {
            log.err("ARC retain called on poisoned object: ptr={*}", .{ptr});
            @panic("ARC: retain called on freed object");
        }

        header.refcount += 1;

        log.debug("ARC retain: ptr={*} refcount={d}", .{ ptr, header.refcount });
    }
}

/// Decrement reference count for a Value (if it's heap-allocated).
/// Frees the object if refcount reaches 0.
/// Safe to call on any Value - does nothing for inline types.
pub fn release(value: Value, allocator: std.mem.Allocator) void {
    if (getHeapPtr(value)) |ptr| {
        const header = getHeader(ptr);

        if (header.isPoisoned()) {
            log.err("ARC release called on poisoned object: ptr={*}", .{ptr});
            @panic("ARC: release called on freed object");
        }

        if (header.refcount == 0) {
            log.err("ARC release called with refcount=0: ptr={*}", .{ptr});
            @panic("ARC: release called on object with zero refcount");
        }

        header.refcount -= 1;

        log.debug("ARC release: ptr={*} refcount={d}", .{ ptr, header.refcount });

        if (header.refcount == 0) {
            freeObject(value, ptr, allocator);
        }
    }
}

/// Get current reference count for a Value (for debugging/testing).
/// Returns null for inline types (they don't have refcounts).
pub fn getRefcount(value: Value) ?u32 {
    if (getHeapPtr(value)) |ptr| {
        const header = getHeaderConst(ptr);
        return header.refcount;
    }
    return null;
}

/// Check if a Value needs ARC management (is heap-allocated).
pub fn needsArc(value: Value) bool {
    return getHeapPtr(value) != null;
}

// ============================================================================
// Internal Helpers
// ============================================================================

/// Extract heap pointer from a Value, or null if it's an inline type.
/// Public for use by weak reference handling in vm_opcodes.
pub fn getHeapPtr(value: Value) ?*anyopaque {
    return switch (value.tag()) {
        .string => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .fixed_string => blk: {
            const ptr = value.bits & Value.PTR_MASK;
            break :blk if (ptr != 0) @ptrFromInt(ptr) else null;
        },
        .decimal => blk: {
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
        // Inline types - no heap allocation
        .null_val, .boolean, .integer, .handle => null,
    };
}

/// Free an ARC-allocated object given its data pointer and size.
/// This is the inverse of create().
fn freeWithHeader(ptr: *anyopaque, comptime data_size: usize, allocator: std.mem.Allocator) void {
    const header: *ArcHeader = getHeader(ptr);
    const total_size = ArcHeader.SIZE + data_size;

    // Get pointer to beginning of allocation (the header)
    const bytes_ptr: [*]align(8) u8 = @ptrCast(@alignCast(header));
    const bytes = bytes_ptr[0..total_size];

    allocator.free(bytes);
}

/// Free a heap-allocated object.
/// Handles recursive release for containers (maps).
fn freeObject(value: Value, ptr: *anyopaque, allocator: std.mem.Allocator) void {
    const header = getHeader(ptr);

    log.debug("ARC free: tag={s} ptr={*}", .{ @tagName(value.tag()), ptr });

    // Mark as poisoned to catch use-after-free
    header.poison();

    switch (value.tag()) {
        .string => {
            freeWithHeader(ptr, @sizeOf(StringRef), allocator);
        },

        .fixed_string => {
            freeWithHeader(ptr, @sizeOf(FixedStringRef), allocator);
        },

        .decimal => {
            freeWithHeader(ptr, @sizeOf(Decimal), allocator);
        },

        .record_ref => {
            // Records have variable size - need to get actual size
            // For now, just free the Record struct itself
            // TODO: Handle record data properly
            freeWithHeader(ptr, @sizeOf(Record), allocator);
        },

        .object => {
            // Check if it's a map (type_id 16)
            if (value.asMap()) |map| {
                // Release all values stored in the map
                for (map.entries.items) |entry| {
                    if (!entry.deleted) {
                        release(entry.value, allocator);
                    }
                }
                // Deinit map internals (but not the map struct itself)
                map.deinit();
            }

            freeWithHeader(ptr, @sizeOf(OrderedMap), allocator);
        },

        // Inline types should never reach here
        .null_val, .boolean, .integer, .handle => unreachable,
    }
}

/// Free a heap-allocated object without recursive release.
/// Used by the cycle collector after it has already broken cycles.
/// This avoids double-freeing values that are part of the same cycle.
pub fn freeObjectForced(value: Value, ptr: *anyopaque, allocator: std.mem.Allocator) void {
    const header = getHeader(ptr);

    log.debug("ARC forced free: tag={s} ptr={*}", .{ @tagName(value.tag()), ptr });

    // Mark as poisoned to catch use-after-free
    header.poison();

    switch (value.tag()) {
        .string => {
            freeWithHeader(ptr, @sizeOf(StringRef), allocator);
        },

        .fixed_string => {
            freeWithHeader(ptr, @sizeOf(FixedStringRef), allocator);
        },

        .decimal => {
            freeWithHeader(ptr, @sizeOf(Decimal), allocator);
        },

        .record_ref => {
            freeWithHeader(ptr, @sizeOf(Record), allocator);
        },

        .object => {
            // Free map structure without recursively releasing values
            // (cycle collector already handled breaking the cycle)
            if (value.asMap()) |map| {
                map.deinit();
            }
            freeWithHeader(ptr, @sizeOf(OrderedMap), allocator);
        },

        .null_val, .boolean, .integer, .handle => unreachable,
    }
}

// ============================================================================
// Weak Reference Registry
// ============================================================================

/// Registry of weak references for zeroing when targets are freed.
///
/// When an object has weak references pointing to it:
/// 1. Each weak ref location is registered with this registry
/// 2. When the object is freed, all registered weak refs are set to null
/// 3. The object's weak_target flag is set in its ARC header
///
/// ## Usage
///
/// ```zig
/// var registry = WeakRegistry.init(allocator);
/// defer registry.deinit();
///
/// // Create weak reference
/// try registry.registerWeakRef(target_value, &weak_ref_location);
///
/// // When target is released and freed, call:
/// registry.invalidateWeakRefs(target_value);
/// ```
pub const WeakRegistry = struct {
    allocator: std.mem.Allocator,
    /// Map from object address to list of weak reference locations
    /// Key: address of heap object (from getHeapPtr)
    /// Value: list of pointers to Value locations that hold weak refs
    refs: std.AutoHashMap(usize, std.ArrayListUnmanaged(*Value)),
    /// Statistics for debugging
    total_registered: usize,
    total_invalidated: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .refs = std.AutoHashMap(usize, std.ArrayListUnmanaged(*Value)).init(allocator),
            .total_registered = 0,
            .total_invalidated = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all the ArrayListUnmanaged entries
        var it = self.refs.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.refs.deinit();
    }

    /// Register a weak reference location that points to a target value.
    /// The weak_ref_loc points to the Value that will be set to null when target is freed.
    pub fn registerWeakRef(self: *Self, target: Value, weak_ref_loc: *Value) !void {
        const ptr = getHeapPtr(target) orelse return; // Non-heap values can't be weak targets
        const addr = @intFromPtr(ptr);

        // Get or create the list of weak refs for this target
        const entry = try self.refs.getOrPut(addr);
        if (!entry.found_existing) {
            entry.value_ptr.* = .empty;
        }

        // Add the weak ref location to the list
        try entry.value_ptr.append(self.allocator, weak_ref_loc);
        self.total_registered += 1;

        // Mark target as having weak references
        const header = getHeader(ptr);
        var flags = header.getFlags();
        flags.weak_target = true;
        header.setFlags(flags);

        log.debug("WeakRegistry: registered weak ref to {*}, total refs to target: {d}", .{
            ptr,
            entry.value_ptr.items.len,
        });
    }

    /// Unregister a weak reference (e.g., when the weak ref location is being overwritten).
    pub fn unregisterWeakRef(self: *Self, target: Value, weak_ref_loc: *Value) void {
        const ptr = getHeapPtr(target) orelse return;
        const addr = @intFromPtr(ptr);

        if (self.refs.getPtr(addr)) |list| {
            // Find and remove the weak ref location
            for (list.items, 0..) |loc, i| {
                if (loc == weak_ref_loc) {
                    _ = list.swapRemove(i);
                    break;
                }
            }

            // If no more weak refs, clear the flag
            if (list.items.len == 0) {
                const header = getHeader(ptr);
                var flags = header.getFlags();
                flags.weak_target = false;
                header.setFlags(flags);
            }
        }
    }

    /// Invalidate (set to null) all weak references to a target that is being freed.
    /// Call this before freeing an object that has the weak_target flag set.
    pub fn invalidateWeakRefs(self: *Self, target: Value) void {
        const ptr = getHeapPtr(target) orelse return;
        const addr = @intFromPtr(ptr);

        if (self.refs.fetchRemove(addr)) |kv| {
            var list = kv.value;

            // Zero all weak reference locations
            for (list.items) |weak_ref_loc| {
                weak_ref_loc.* = Value.null_val;
                self.total_invalidated += 1;
            }

            log.debug("WeakRegistry: invalidated {d} weak refs to {*}", .{
                list.items.len,
                ptr,
            });

            list.deinit(self.allocator);
        }
    }

    /// Check if a target has any weak references pointing to it.
    pub fn hasWeakRefs(self: *Self, target: Value) bool {
        const ptr = getHeapPtr(target) orelse return false;
        const addr = @intFromPtr(ptr);

        if (self.refs.get(addr)) |list| {
            return list.items.len > 0;
        }
        return false;
    }

    /// Get statistics for debugging.
    pub fn getStats(self: *const Self) struct {
        targets_with_weak_refs: usize,
        total_registered: usize,
        total_invalidated: usize,
    } {
        return .{
            .targets_with_weak_refs = self.refs.count(),
            .total_registered = self.total_registered,
            .total_invalidated = self.total_invalidated,
        };
    }
};

/// Release with weak reference support.
/// Use this instead of release() when you have a WeakRegistry.
pub fn releaseWithWeakSupport(value: Value, allocator: std.mem.Allocator, weak_registry: *WeakRegistry) void {
    if (getHeapPtr(value)) |ptr| {
        const header = getHeader(ptr);

        if (header.isPoisoned()) {
            log.err("ARC release called on poisoned object: ptr={*}", .{ptr});
            @panic("ARC: release called on freed object");
        }

        if (header.refcount == 0) {
            log.err("ARC release called with refcount=0: ptr={*}", .{ptr});
            @panic("ARC: release called on object with zero refcount");
        }

        header.refcount -= 1;

        log.debug("ARC release (with weak): ptr={*} refcount={d}", .{ ptr, header.refcount });

        if (header.refcount == 0) {
            // Invalidate weak references before freeing
            if (header.getFlags().weak_target) {
                weak_registry.invalidateWeakRefs(value);
            }
            freeObject(value, ptr, allocator);
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "ArcHeader size is 8 bytes" {
    try std.testing.expectEqual(@as(usize, 8), ArcHeader.SIZE);
}

test "ArcHeader init sets refcount to 1" {
    const header = ArcHeader.init();
    try std.testing.expectEqual(@as(u32, 1), header.refcount);
    try std.testing.expectEqual(@as(u32, 0), header.flags);
}

test "ArcHeader flags" {
    var header = ArcHeader.init();

    // Set cycle_candidate
    var flags = header.getFlags();
    flags.cycle_candidate = true;
    header.setFlags(flags);

    try std.testing.expect(header.getFlags().cycle_candidate);
    try std.testing.expect(!header.getFlags().poisoned);

    // Set poisoned
    header.poison();
    try std.testing.expect(header.isPoisoned());
}

test "create allocates with header" {
    const allocator = std.testing.allocator;

    const TestStruct = struct {
        a: u64,
        b: u64,
    };

    const ptr = try create(allocator, TestStruct);
    defer {
        // Manual free for test (normally done via release)
        const header = getHeaderTyped(TestStruct, ptr);
        const total_size = ArcHeader.SIZE + @sizeOf(TestStruct);
        const bytes_ptr: [*]align(8) u8 = @ptrCast(@alignCast(header));
        allocator.free(bytes_ptr[0..total_size]);
    }

    // Check header is accessible and initialized
    const header = getHeaderTyped(TestStruct, ptr);
    try std.testing.expectEqual(@as(u32, 1), header.refcount);

    // Check we can write to object
    ptr.* = .{ .a = 42, .b = 99 };
    try std.testing.expectEqual(@as(u64, 42), ptr.a);
    try std.testing.expectEqual(@as(u64, 99), ptr.b);
}

test "retain increments refcount" {
    const allocator = std.testing.allocator;

    // Create a decimal value using ARC allocation
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 12345, .precision = 2 };

    // Manually construct value with decimal tag
    // TAG_DECIMAL = 0x7FFF_8000_0000_0000
    const value = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Initial refcount should be 1
    try std.testing.expectEqual(@as(?u32, 1), getRefcount(value));

    // Retain should increment
    retain(value);
    try std.testing.expectEqual(@as(?u32, 2), getRefcount(value));

    retain(value);
    try std.testing.expectEqual(@as(?u32, 3), getRefcount(value));

    // Clean up
    release(value, allocator);
    release(value, allocator);
    release(value, allocator); // This one frees
}

test "release decrements refcount and frees at zero" {
    const allocator = std.testing.allocator;

    // Create a decimal value with ARC header
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 99999, .precision = 4 };

    // TAG_DECIMAL = 0x7FFF_8000_0000_0000
    const value = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Retain twice (total refcount = 3)
    retain(value);
    retain(value);
    try std.testing.expectEqual(@as(?u32, 3), getRefcount(value));

    // Release back down
    release(value, allocator);
    try std.testing.expectEqual(@as(?u32, 2), getRefcount(value));

    release(value, allocator);
    try std.testing.expectEqual(@as(?u32, 1), getRefcount(value));

    // Final release frees the object
    release(value, allocator);
    // Can't check refcount - object is freed
}

test "inline types return null refcount" {
    try std.testing.expectEqual(@as(?u32, null), getRefcount(Value.null_val));
    try std.testing.expectEqual(@as(?u32, null), getRefcount(Value.true_val));
    try std.testing.expectEqual(@as(?u32, null), getRefcount(Value.initInt(42)));
}

test "needsArc returns correct values" {
    const allocator = std.testing.allocator;

    // Inline types don't need ARC
    try std.testing.expect(!needsArc(Value.null_val));
    try std.testing.expect(!needsArc(Value.true_val));
    try std.testing.expect(!needsArc(Value.initInt(42)));

    // Heap types need ARC
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 100, .precision = 2 };
    // TAG_DECIMAL = 0x7FFF_8000_0000_0000
    const dec_value = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    try std.testing.expect(needsArc(dec_value));

    // Clean up
    release(dec_value, allocator);
}

test "WeakRegistry basic operations" {
    const allocator = std.testing.allocator;

    var registry = WeakRegistry.init(allocator);
    defer registry.deinit();

    // Create a target value
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 42, .precision = 2 };
    const target = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Create a weak reference location
    var weak_ref: Value = target; // Initially points to target

    // Register the weak reference
    try registry.registerWeakRef(target, &weak_ref);

    // Should have weak_target flag set
    const header = getHeader(dec_ptr);
    try std.testing.expect(header.getFlags().weak_target);

    // Invalidate weak refs (simulating target being freed)
    registry.invalidateWeakRefs(target);

    // Weak ref should be null now
    try std.testing.expect(weak_ref.isNull());

    // Stats should reflect the operations
    const stats = registry.getStats();
    try std.testing.expectEqual(@as(usize, 1), stats.total_registered);
    try std.testing.expectEqual(@as(usize, 1), stats.total_invalidated);

    // Clean up (release the target value)
    release(target, allocator);
}

test "WeakRegistry multiple weak refs to same target" {
    const allocator = std.testing.allocator;

    var registry = WeakRegistry.init(allocator);
    defer registry.deinit();

    // Create a target value
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 100, .precision = 0 };
    const target = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Create multiple weak reference locations
    var weak_ref1: Value = target;
    var weak_ref2: Value = target;
    var weak_ref3: Value = target;

    // Register all weak references
    try registry.registerWeakRef(target, &weak_ref1);
    try registry.registerWeakRef(target, &weak_ref2);
    try registry.registerWeakRef(target, &weak_ref3);

    // Check hasWeakRefs
    try std.testing.expect(registry.hasWeakRefs(target));

    // Invalidate all weak refs
    registry.invalidateWeakRefs(target);

    // All weak refs should be null now
    try std.testing.expect(weak_ref1.isNull());
    try std.testing.expect(weak_ref2.isNull());
    try std.testing.expect(weak_ref3.isNull());

    // Stats should reflect 3 registered, 3 invalidated
    const stats = registry.getStats();
    try std.testing.expectEqual(@as(usize, 3), stats.total_registered);
    try std.testing.expectEqual(@as(usize, 3), stats.total_invalidated);

    // Clean up
    release(target, allocator);
}

test "WeakRegistry unregister" {
    const allocator = std.testing.allocator;

    var registry = WeakRegistry.init(allocator);
    defer registry.deinit();

    // Create a target value
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 50, .precision = 1 };
    const target = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Create and register weak reference
    var weak_ref: Value = target;
    try registry.registerWeakRef(target, &weak_ref);

    // Should have weak refs
    try std.testing.expect(registry.hasWeakRefs(target));

    // Unregister the weak ref
    registry.unregisterWeakRef(target, &weak_ref);

    // Should no longer have weak refs
    try std.testing.expect(!registry.hasWeakRefs(target));

    // Clean up
    release(target, allocator);
}

test "releaseWithWeakSupport invalidates weak refs" {
    const allocator = std.testing.allocator;

    var registry = WeakRegistry.init(allocator);
    defer registry.deinit();

    // Create a target value
    const dec_ptr = try create(allocator, Decimal);
    dec_ptr.* = .{ .value = 999, .precision = 3 };
    const target = Value{ .bits = 0x7FFF_8000_0000_0000 | @intFromPtr(dec_ptr) };

    // Create weak reference
    var weak_ref: Value = target;
    try registry.registerWeakRef(target, &weak_ref);

    // Weak ref should still point to target
    try std.testing.expect(!weak_ref.isNull());

    // Release with weak support - this should invalidate weak refs and free
    releaseWithWeakSupport(target, allocator, &registry);

    // Weak ref should be null now
    try std.testing.expect(weak_ref.isNull());
}
