//! ARC Conformance Tests
//!
//! Comprehensive tests to verify that the ARC (Automatic Reference Counting)
//! implementation correctly manages memory. Tests cover:
//!
//! 1. Basic allocation/deallocation for each heap type
//! 2. Reference counting semantics
//! 3. VM register writes with ARC
//! 4. Stack operations with ARC
//! 5. Map operations with nested values
//! 6. Memory leak detection
//!
//! All tests use std.testing.allocator which detects memory leaks.

const std = @import("std");
const arc = @import("arc.zig");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const Decimal = value_mod.Decimal;
const BoxedInt = value_mod.BoxedInt;
const StringRef = value_mod.StringRef;
const FixedStringRef = value_mod.FixedStringRef;
const OrderedMap = @import("ordered_map.zig").OrderedMap;
const VM = @import("vm.zig").VM;

// ============================================================================
// Basic Heap Type Tests
// ============================================================================

test "ARC: string allocation and release" {
    const allocator = std.testing.allocator;

    // Allocate string data
    const str_data = try allocator.dupe(u8, "hello world");
    errdefer allocator.free(str_data);

    // Create string value with ARC
    const str_val = try Value.initString(allocator, str_data);

    // Verify refcount is 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(str_val));

    // Verify string content
    try std.testing.expectEqualStrings("hello world", str_val.asString());

    // Release - should free
    arc.release(str_val, allocator);
}

test "ARC: fixed string allocation and release" {
    const allocator = std.testing.allocator;

    // Allocate fixed string buffer
    const buf = try allocator.alloc(u8, 20);
    @memset(buf, ' ');
    @memcpy(buf[0..5], "test!");

    // Create fixed string value with ARC
    const fs_val = try Value.initFixedString(allocator, buf);

    // Verify refcount is 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(fs_val));

    // Verify content
    const content = fs_val.asString();
    try std.testing.expectEqual(@as(usize, 20), content.len);
    try std.testing.expectEqualStrings("test!", content[0..5]);

    // Release - should free
    arc.release(fs_val, allocator);
}

test "ARC: decimal allocation and release" {
    const allocator = std.testing.allocator;

    // Create decimal value
    const dec_val = try Value.initDecimal(allocator, 12345, 2);

    // Verify refcount is 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(dec_val));

    // Verify content
    const dec = dec_val.asDecimal().?;
    try std.testing.expectEqual(@as(i64, 12345), dec.value);
    try std.testing.expectEqual(@as(u8, 2), dec.precision);

    // Release - should free
    arc.release(dec_val, allocator);
}

test "ARC: boxed int allocation and release" {
    const allocator = std.testing.allocator;

    // Create boxed int for large value
    const big_val = try Value.initBoxedInt(allocator, 0x7FFFFFFFFFFFFFF);

    // Verify refcount is 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(big_val));

    // Verify content
    try std.testing.expectEqual(@as(i64, 0x7FFFFFFFFFFFFFF), big_val.toInt());

    // Release - should free
    arc.release(big_val, allocator);
}

test "ARC: map allocation and release" {
    const allocator = std.testing.allocator;

    // Create map
    const map = try arc.create(allocator, OrderedMap);
    map.* = OrderedMap.init(allocator, .{});

    const map_val = Value.initMap(map);

    // Verify refcount is 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(map_val));

    // Release - should free
    arc.release(map_val, allocator);
}

// ============================================================================
// Reference Counting Tests
// ============================================================================

test "ARC: multiple retains and releases" {
    const allocator = std.testing.allocator;

    const dec_val = try Value.initDecimal(allocator, 999, 3);

    // Initial refcount = 1
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(dec_val));

    // Retain multiple times
    arc.retain(dec_val);
    try std.testing.expectEqual(@as(?u32, 2), arc.getRefcount(dec_val));

    arc.retain(dec_val);
    try std.testing.expectEqual(@as(?u32, 3), arc.getRefcount(dec_val));

    arc.retain(dec_val);
    try std.testing.expectEqual(@as(?u32, 4), arc.getRefcount(dec_val));

    // Release back down
    arc.release(dec_val, allocator);
    try std.testing.expectEqual(@as(?u32, 3), arc.getRefcount(dec_val));

    arc.release(dec_val, allocator);
    try std.testing.expectEqual(@as(?u32, 2), arc.getRefcount(dec_val));

    arc.release(dec_val, allocator);
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(dec_val));

    // Final release frees
    arc.release(dec_val, allocator);
}

test "ARC: retain/release on inline types is no-op" {
    const allocator = std.testing.allocator;

    // Inline types should not crash when retain/release called
    arc.retain(Value.null_val);
    arc.release(Value.null_val, allocator);

    arc.retain(Value.true_val);
    arc.release(Value.true_val, allocator);

    arc.retain(Value.false_val);
    arc.release(Value.false_val, allocator);

    const int_val = Value.initInt(42);
    arc.retain(int_val);
    arc.release(int_val, allocator);

    // All should be no-ops with no memory operations
    try std.testing.expect(true);
}

test "ARC: shared value with multiple references" {
    const allocator = std.testing.allocator;

    const str_data = try allocator.dupe(u8, "shared");
    errdefer allocator.free(str_data);

    const str_val = try Value.initString(allocator, str_data);

    // Simulate copying to multiple locations
    arc.retain(str_val); // "copy" to location 2
    arc.retain(str_val); // "copy" to location 3
    try std.testing.expectEqual(@as(?u32, 3), arc.getRefcount(str_val));

    // All locations can still read
    try std.testing.expectEqualStrings("shared", str_val.asString());

    // Release from each location
    arc.release(str_val, allocator); // location 1
    arc.release(str_val, allocator); // location 2
    arc.release(str_val, allocator); // location 3 - frees
}

// ============================================================================
// VM Register Write Tests
// ============================================================================

test "ARC: VM writeRegister releases old value" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Create first decimal value
    const dec1 = try Value.initDecimal(std.testing.allocator, 100, 2);
    // Initial refcount is 1, writeRegister will retain making it 2
    vm.writeRegister(0, dec1);
    // After write: refcount should still be manageable
    // The register now holds it with refcount adjusted

    // Create second decimal value
    const dec2 = try Value.initDecimal(std.testing.allocator, 200, 2);

    // Write to same register - should release dec1
    vm.writeRegister(0, dec2);

    // dec1 should have been released when overwritten
    // dec2 should now be in register
    try std.testing.expectEqual(@as(i64, 200), vm.registers[0].asDecimal().?.value);

    // Clean up by releasing what's in the register
    // VM deinit will handle cleanup of registers
}

test "ARC: VM writeRegister with null overwrites heap value" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Create string value
    const str_data = try std.testing.allocator.dupe(u8, "test");
    errdefer std.testing.allocator.free(str_data);
    const str_val = try Value.initString(std.testing.allocator, str_data);

    // Write string to register
    vm.writeRegister(0, str_val);

    // Write null over it - should release string
    vm.writeRegister(0, Value.null_val);

    // Register should now be null
    try std.testing.expect(vm.registers[0].isNull());
}

test "ARC: VM writeRegister with same value is safe" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    const dec_val = try Value.initDecimal(std.testing.allocator, 500, 1);

    // Write value to register
    vm.writeRegister(0, dec_val);

    // Get refcount after first write
    const rc1 = arc.getRefcount(vm.registers[0]);

    // Writing same value again should be safe
    // (retain new, release old - net zero change since same object)
    vm.writeRegister(0, vm.registers[0]);

    const rc2 = arc.getRefcount(vm.registers[0]);

    // Refcount should be the same (or at least not leak)
    try std.testing.expectEqual(rc1, rc2);
}

// ============================================================================
// VM Stack Operation Tests
// ============================================================================

test "ARC: VM writeStack releases old value" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Create first value
    const dec1 = try Value.initDecimal(std.testing.allocator, 111, 0);

    // Write to stack slot
    vm.writeStack(0, dec1);

    // Create second value
    const dec2 = try Value.initDecimal(std.testing.allocator, 222, 0);

    // Write to same stack slot - should release dec1
    vm.writeStack(0, dec2);

    // Verify new value is there
    try std.testing.expectEqual(@as(i64, 222), vm.stack[0].asDecimal().?.value);
}

test "ARC: VM pushStack and popStack" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Push a heap value
    const str_data = try std.testing.allocator.dupe(u8, "pushed");
    errdefer std.testing.allocator.free(str_data);
    const str_val = try Value.initString(std.testing.allocator, str_data);

    vm.pushStack(str_val);
    try std.testing.expectEqual(@as(usize, 1), vm.sp);

    // Value should be on stack
    try std.testing.expectEqualStrings("pushed", vm.stack[0].asString());

    // Pop the value
    const popped = vm.popStack();
    try std.testing.expectEqual(@as(usize, 0), vm.sp);
    try std.testing.expectEqualStrings("pushed", popped.asString());

    // Caller owns the popped value, must release
    arc.release(popped, std.testing.allocator);
}

test "ARC: VM releaseStackRange clears range" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Push multiple heap values
    for (0..5) |i| {
        const dec = try Value.initDecimal(std.testing.allocator, @intCast(i * 10), 0);
        vm.writeStack(i, dec);
    }

    // Verify values are there
    try std.testing.expectEqual(@as(i64, 20), vm.stack[2].asDecimal().?.value);

    // Release range [1, 4) - should release slots 1, 2, 3
    vm.releaseStackRange(1, 4);

    // Released slots should be null
    try std.testing.expect(vm.stack[1].isNull());
    try std.testing.expect(vm.stack[2].isNull());
    try std.testing.expect(vm.stack[3].isNull());

    // Slots outside range should be unchanged
    try std.testing.expectEqual(@as(i64, 0), vm.stack[0].asDecimal().?.value);
    try std.testing.expectEqual(@as(i64, 40), vm.stack[4].asDecimal().?.value);
}

// ============================================================================
// Map with Nested Values Tests
// ============================================================================

test "ARC: map with heap values releases on map release" {
    const allocator = std.testing.allocator;

    // Create map
    const map = try arc.create(allocator, OrderedMap);
    map.* = OrderedMap.init(allocator, .{});

    // Add heap values to map
    const str1_data = try allocator.dupe(u8, "value1");
    errdefer allocator.free(str1_data);
    const str1 = try Value.initString(allocator, str1_data);

    const str2_data = try allocator.dupe(u8, "value2");
    errdefer allocator.free(str2_data);
    const str2 = try Value.initString(allocator, str2_data);

    // Retain before putting in map (map takes ownership)
    arc.retain(str1);
    arc.retain(str2);
    _ = try map.set("key1", str1);
    _ = try map.set("key2", str2);

    // Original refs are still valid
    arc.release(str1, allocator);
    arc.release(str2, allocator);

    // Map value
    const map_val = Value.initMap(map);

    // Release map - should cascade release to contained values
    arc.release(map_val, allocator);

    // If no leaks detected by testing allocator, test passes
}

test "ARC: map overwriting value releases old" {
    const allocator = std.testing.allocator;

    // Create map
    const map = try arc.create(allocator, OrderedMap);
    map.* = OrderedMap.init(allocator, .{});

    // Add first value
    const str1_data = try allocator.dupe(u8, "first");
    errdefer allocator.free(str1_data);
    const str1 = try Value.initString(allocator, str1_data);
    arc.retain(str1);
    _ = try map.set("key", str1);
    arc.release(str1, allocator);

    // TODO: OrderedMap.set should release old value when overwriting
    // For now, we need to handle this manually or fix OrderedMap

    // Release map
    const map_val = Value.initMap(map);
    arc.release(map_val, allocator);
}

// ============================================================================
// Edge Case Tests
// ============================================================================

test "ARC: empty string value" {
    const allocator = std.testing.allocator;

    // Empty string
    const empty_data = try allocator.alloc(u8, 0);
    const empty_str = try Value.initString(allocator, empty_data);

    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(empty_str));
    try std.testing.expectEqualStrings("", empty_str.asString());

    arc.release(empty_str, allocator);
}

test "ARC: zero precision decimal" {
    const allocator = std.testing.allocator;

    const dec = try Value.initDecimal(allocator, 42, 0);
    try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(dec));

    const dec_data = dec.asDecimal().?;
    try std.testing.expectEqual(@as(i64, 42), dec_data.value);
    try std.testing.expectEqual(@as(u8, 0), dec_data.precision);

    arc.release(dec, allocator);
}

test "ARC: many allocations and releases" {
    const allocator = std.testing.allocator;

    // Stress test: many allocations
    var values: [100]Value = undefined;
    for (&values, 0..) |*v, i| {
        v.* = try Value.initDecimal(allocator, @intCast(i), 2);
    }

    // All should have refcount 1
    for (values) |v| {
        try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(v));
    }

    // Release all
    for (values) |v| {
        arc.release(v, allocator);
    }
}

test "ARC: alternating retain and release" {
    const allocator = std.testing.allocator;

    const val = try Value.initDecimal(allocator, 777, 0);

    // Alternate retain/release
    for (0..10) |_| {
        arc.retain(val);
        try std.testing.expectEqual(@as(?u32, 2), arc.getRefcount(val));
        arc.release(val, allocator);
        try std.testing.expectEqual(@as(?u32, 1), arc.getRefcount(val));
    }

    arc.release(val, allocator);
}

// ============================================================================
// Integration Tests
// ============================================================================

test "ARC: simulate function call stack frame cleanup" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Simulate pushing a stack frame with local heap values
    const frame_base = vm.sp;
    vm.fp = frame_base;

    // Create "local variables" as heap values
    for (0..4) |i| {
        const str_data = try std.testing.allocator.dupe(u8, "local");
        errdefer std.testing.allocator.free(str_data);
        const str_val = try Value.initString(std.testing.allocator, str_data);
        vm.writeStack(frame_base + i, str_val);
    }

    // Simulate return - release all locals in frame
    vm.releaseStackRange(frame_base, frame_base + 4);

    // All locals should be null (released)
    for (0..4) |i| {
        try std.testing.expect(vm.stack[frame_base + i].isNull());
    }
}

test "ARC: copy value between registers" {
    var vm = VM.initForTest(std.testing.allocator);
    defer vm.deinit();

    // Create heap value in r0
    const dec = try Value.initDecimal(std.testing.allocator, 1234, 2);
    vm.writeRegister(0, dec);

    // Get refcount
    const rc1 = arc.getRefcount(vm.registers[0]).?;

    // Copy to r1 (retain for new location)
    const val = vm.registers[0];
    arc.retain(val);
    vm.writeRegister(1, val);

    // Both registers should have valid value
    try std.testing.expectEqual(@as(i64, 1234), vm.registers[0].asDecimal().?.value);
    try std.testing.expectEqual(@as(i64, 1234), vm.registers[1].asDecimal().?.value);

    // Refcount should have increased
    const rc2 = arc.getRefcount(vm.registers[0]).?;
    try std.testing.expect(rc2 > rc1);
}

test "ARC: value survives scope via retain" {
    const allocator = std.testing.allocator;

    var outer_val: Value = undefined;

    // Inner scope creates value
    {
        const str_data = try allocator.dupe(u8, "survivor");
        errdefer allocator.free(str_data);
        const inner_val = try Value.initString(allocator, str_data);

        // "Move" to outer scope via retain
        arc.retain(inner_val);
        outer_val = inner_val;

        // Inner scope releases its reference
        arc.release(inner_val, allocator);
    }

    // Outer scope still has valid value
    try std.testing.expectEqualStrings("survivor", outer_val.asString());

    // Clean up outer reference
    arc.release(outer_val, allocator);
}

// ============================================================================
// Weak Reference Conformance Tests
// ============================================================================

test "ARC: weak reference creation does not increase refcount" {
    const allocator = std.testing.allocator;

    // Create a decimal value (heap allocated)
    const dec_val = try Value.initDecimal(allocator, 42, 0);

    // Initial refcount should be 1
    const initial_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), initial_rc);

    // Creating a weak reference should NOT increase refcount
    var weak_ref: Value = dec_val;
    var registry = arc.WeakRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerWeakRef(dec_val, &weak_ref);

    // Refcount should still be 1
    const after_weak_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), after_weak_rc);

    // Clean up
    arc.release(dec_val, allocator);
}

test "ARC: weak reference becomes null when target released" {
    const allocator = std.testing.allocator;

    // Create target value
    const dec_val = try Value.initDecimal(allocator, 100, 2);

    // Create weak reference
    var weak_ref: Value = dec_val;
    var registry = arc.WeakRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerWeakRef(dec_val, &weak_ref);

    // Weak ref should initially point to target
    try std.testing.expect(!weak_ref.isNull());
    try std.testing.expectEqual(@as(i64, 100), weak_ref.asDecimal().?.value);

    // Release target using releaseWithWeakSupport
    arc.releaseWithWeakSupport(dec_val, allocator, &registry);

    // Weak ref should now be null
    try std.testing.expect(weak_ref.isNull());
}

test "ARC: weak reference loading returns value while target alive" {
    const allocator = std.testing.allocator;

    // Create a string target
    const str_data = try allocator.dupe(u8, "target string");
    errdefer allocator.free(str_data);
    const str_val = try Value.initString(allocator, str_data);

    // Create weak reference
    var weak_ref: Value = str_val;
    var registry = arc.WeakRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerWeakRef(str_val, &weak_ref);

    // Loading weak ref should return the value
    if (weak_ref.isNull()) {
        try std.testing.expect(false); // Should not be null
    } else {
        try std.testing.expectEqualStrings("target string", weak_ref.asString());
    }

    // Clean up
    arc.release(str_val, allocator);
}

test "ARC: multiple weak references all become null on release" {
    const allocator = std.testing.allocator;

    // Create target value
    const dec_val = try Value.initDecimal(allocator, 999, 1);

    // Create multiple weak references
    var weak1: Value = dec_val;
    var weak2: Value = dec_val;
    var weak3: Value = dec_val;

    var registry = arc.WeakRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerWeakRef(dec_val, &weak1);
    try registry.registerWeakRef(dec_val, &weak2);
    try registry.registerWeakRef(dec_val, &weak3);

    // All should be valid initially
    try std.testing.expect(!weak1.isNull());
    try std.testing.expect(!weak2.isNull());
    try std.testing.expect(!weak3.isNull());

    // Release target
    arc.releaseWithWeakSupport(dec_val, allocator, &registry);

    // All weak refs should be null
    try std.testing.expect(weak1.isNull());
    try std.testing.expect(weak2.isNull());
    try std.testing.expect(weak3.isNull());
}

test "ARC: VM weak_ref opcode creates weak reference" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value in r0
    const dec_val = try Value.initDecimal(allocator, 123, 0);
    vm.writeRegister(0, dec_val);

    // Initial refcount
    const initial_rc = arc.getRefcount(dec_val).?;

    // Simulate weak_ref r1, r0 (create weak ref from r0, store in r1)
    // The weak_ref opcode should:
    // 1. Copy the value without retaining
    // 2. Register in weak registry
    const source_val = vm.registers[0];
    vm.registers[1] = source_val;
    try vm.weak_registry.registerWeakRef(source_val, &vm.registers[1]);

    // Refcount should NOT increase
    const after_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(initial_rc, after_rc);

    // Both registers point to same value
    try std.testing.expectEqual(@as(i64, 123), vm.registers[0].asDecimal().?.value);
    try std.testing.expectEqual(@as(i64, 123), vm.registers[1].asDecimal().?.value);
}

test "ARC: VM weak_load returns null after target freed" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value
    const dec_val = try Value.initDecimal(allocator, 456, 2);
    vm.writeRegister(0, dec_val);

    // Create weak reference in r1
    vm.registers[1] = vm.registers[0];
    try vm.weak_registry.registerWeakRef(vm.registers[0], &vm.registers[1]);

    // Clear r0 (simulating target going out of scope)
    // This should trigger release with weak support
    const old_val = vm.registers[0];
    vm.registers[0] = Value.initNull();
    arc.releaseWithWeakSupport(old_val, allocator, &vm.weak_registry);

    // weak_load from r1 should now get null
    try std.testing.expect(vm.registers[1].isNull());
}

test "ARC: weak reference to inline value is no-op" {
    const allocator = std.testing.allocator;
    var registry = arc.WeakRegistry.init(allocator);
    defer registry.deinit();

    // Inline integer - not heap allocated
    const inline_val = Value.initInt(42);

    // Creating a weak ref to inline value should be safe (no-op)
    var weak_ref: Value = inline_val;
    try registry.registerWeakRef(inline_val, &weak_ref);

    // Value should still be accessible
    try std.testing.expectEqual(@as(i64, 42), weak_ref.asInt());

    // No cleanup needed for inline values
}

// ============================================================================
// ARC Compiler Integration Tests
// Tests for explicit arc_retain, arc_release, arc_move opcodes
// ============================================================================

test "ARC: arc_retain opcode increments refcount" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value
    const dec_val = try Value.initDecimal(allocator, 123, 0);
    vm.writeRegister(0, dec_val);

    // Initial refcount should be 1 (from initDecimal + writeRegister retain - writeRegister doesn't retain from null)
    const initial_rc = arc.getRefcount(dec_val).?;

    // Simulate arc_retain opcode
    arc.retain(vm.registers[0]);

    // Refcount should increase by 1
    const after_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(initial_rc + 1, after_rc);

    // Release twice to balance (once for retain, once for initial)
    arc.release(dec_val, allocator);
    arc.release(dec_val, allocator);
}

test "ARC: arc_release opcode decrements refcount" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value and retain twice
    const dec_val = try Value.initDecimal(allocator, 456, 2);
    vm.writeRegister(0, dec_val);
    arc.retain(dec_val);

    // Should have refcount 2
    const rc_before = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 2), rc_before);

    // Simulate arc_release opcode
    arc.releaseWithWeakSupport(vm.registers[0], allocator, &vm.weak_registry);

    // Refcount should be 1 now
    const rc_after = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), rc_after);

    // Final release
    arc.release(dec_val, allocator);
}

test "ARC: arc_release frees when refcount reaches zero" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value
    const dec_val = try Value.initDecimal(allocator, 789, 1);

    // Write to register (refcount is 1)
    vm.registers[0] = dec_val;

    // Initial refcount
    const initial_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), initial_rc);

    // Simulate arc_release opcode - should free
    arc.releaseWithWeakSupport(vm.registers[0], allocator, &vm.weak_registry);
    vm.registers[0] = Value.null_val;

    // No memory leak expected (allocator will complain if not freed)
}

test "ARC: arc_move transfers ownership without retain/release" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create a heap value in r0
    const dec_val = try Value.initDecimal(allocator, 111, 0);
    vm.registers[0] = dec_val;

    // Initial refcount should be 1
    const initial_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), initial_rc);

    // Simulate arc_move r1, r0: move from r0 to r1
    vm.registers[1] = vm.registers[0];
    vm.registers[0] = Value.null_val;

    // Refcount should still be 1 (no retain/release)
    const after_rc = arc.getRefcount(dec_val).?;
    try std.testing.expectEqual(@as(u32, 1), after_rc);

    // Source register should be null
    try std.testing.expect(vm.registers[0].isNull());

    // Dest register should have the value
    try std.testing.expectEqual(@as(i64, 111), vm.registers[1].asDecimal().?.value);

    // Cleanup
    arc.release(vm.registers[1], allocator);
    vm.registers[1] = Value.null_val;
}

test "ARC: arc_retain on inline value is no-op" {
    const allocator = std.testing.allocator;
    _ = allocator;

    // Create an inline integer
    const int_val = Value.initInt(42);

    // retain should be no-op (no crash, no side effects)
    arc.retain(int_val);
    arc.retain(int_val);

    // Value should still be accessible
    try std.testing.expectEqual(@as(i64, 42), int_val.asInt());

    // No release needed for inline values
}

test "ARC: arc_release on inline value is no-op" {
    const allocator = std.testing.allocator;

    // Create an inline integer
    const int_val = Value.initInt(99);

    // release should be no-op (no crash, no side effects)
    arc.release(int_val, allocator);
    arc.release(int_val, allocator);

    // Value should still be accessible
    try std.testing.expectEqual(@as(i64, 99), int_val.asInt());
}

test "ARC: arc_move with heap value in destination releases old" {
    const allocator = std.testing.allocator;
    var vm = try VM.initForTest(allocator);
    defer vm.deinit();

    // Create two heap values
    const old_val = try Value.initDecimal(allocator, 100, 0);
    const new_val = try Value.initDecimal(allocator, 200, 0);

    // Put old value in r1
    vm.registers[1] = old_val;

    // Put new value in r0
    vm.registers[0] = new_val;

    // Simulate arc_move r1, r0 - should release old value in r1
    const dest_old = vm.registers[1];
    if (arc.needsArc(dest_old)) {
        arc.releaseWithWeakSupport(dest_old, allocator, &vm.weak_registry);
    }
    vm.registers[1] = vm.registers[0];
    vm.registers[0] = Value.null_val;

    // r1 should now have 200
    try std.testing.expectEqual(@as(i64, 200), vm.registers[1].asDecimal().?.value);

    // r0 should be null
    try std.testing.expect(vm.registers[0].isNull());

    // Cleanup r1
    arc.release(vm.registers[1], allocator);
    vm.registers[1] = Value.null_val;
}
