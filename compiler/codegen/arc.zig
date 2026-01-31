//! ARC Runtime for Cot (WebAssembly)
//!
//! Provides reference counting runtime functions following Swift's ARC patterns.
//! See docs/ARC_DESIGN.md for design rationale.
//!
//! Reference: ~/learning/swift/stdlib/public/runtime/HeapObject.cpp

const std = @import("std");
const wasm = @import("wasm.zig");
const Op = @import("wasm_opcodes.zig").Op;
const enc = @import("wasm_encode.zig");
const ValType = wasm.ValType;

// Block type constants for Wasm control flow (from spec)
const BLOCK_VOID: u8 = 0x40;
const BLOCK_I32: u8 = 0x7F;
const BLOCK_I64: u8 = 0x7E;

// =============================================================================
// Memory Layout Constants (matching Swift's patterns)
// =============================================================================

/// Size of heap object header: metadata(4) + refcount(8) = 12 bytes
pub const HEAP_OBJECT_HEADER_SIZE: u32 = 12;

/// Offset of metadata pointer in header
pub const METADATA_OFFSET: u32 = 0;

/// Offset of refcount in header
pub const REFCOUNT_OFFSET: u32 = 4;

/// Offset of user data (after header)
pub const USER_DATA_OFFSET: u32 = 12;

/// Immortal refcount value (Swift's EmbeddedImmortalRefCount pattern)
/// Objects with this refcount are never freed.
pub const IMMORTAL_REFCOUNT: i64 = 0x7FFFFFFFFFFFFFFF;

/// Initial refcount for newly allocated objects
pub const INITIAL_REFCOUNT: i64 = 1;

/// Memory layout globals
pub const HEAP_START: u32 = 0x10000; // 64KB reserved for stack

// =============================================================================
// Runtime Function Indices
// =============================================================================

pub const RuntimeFunctions = struct {
    /// cot_alloc(metadata: i32, size: i32) -> i32
    alloc_idx: u32,

    /// cot_retain(obj: i32) -> i32
    retain_idx: u32,

    /// cot_release(obj: i32) -> void
    release_idx: u32,

    /// cot_retain_count(obj: i32) -> i64
    retain_count_idx: u32,

    /// cot_is_uniquely_referenced(obj: i32) -> i32
    is_unique_idx: u32,

    /// heap_ptr global index
    heap_ptr_global: u32,
};

// =============================================================================
// Code Generation for Runtime Functions
// =============================================================================

/// Adds ARC runtime functions to a Wasm module.
/// Returns the indices of the generated functions.
pub fn addRuntimeFunctions(module: *wasm.Module) !RuntimeFunctions {
    // Add heap_ptr global (mutable i32, initialized to HEAP_START)
    const heap_ptr_global = try module.addGlobal(.i32, true, HEAP_START);

    // Generate runtime functions
    const alloc_idx = try generateAllocFunction(module, heap_ptr_global);
    const retain_idx = try generateRetainFunction(module);
    const release_idx = try generateReleaseFunction(module, heap_ptr_global);
    const retain_count_idx = try generateRetainCountFunction(module);
    const is_unique_idx = try generateIsUniqueFunction(module, retain_count_idx);

    return RuntimeFunctions{
        .alloc_idx = alloc_idx,
        .retain_idx = retain_idx,
        .release_idx = release_idx,
        .retain_count_idx = retain_count_idx,
        .is_unique_idx = is_unique_idx,
        .heap_ptr_global = heap_ptr_global,
    };
}

/// Generates: cot_alloc(metadata: i32, size: i32) -> i32
/// Allocates a new heap object with initial refcount of 1.
fn generateAllocFunction(module: *wasm.Module, heap_ptr_global: u32) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameters: metadata (local 0), size (local 1)
    // Local 2: ptr (allocated address)
    // Local 3: total_size

    // Declare locals: ptr: i32, total_size: i32
    _ = try code.declareLocals(&[_]ValType{ .i32, .i32 });

    // total_size = size + HEAP_OBJECT_HEADER_SIZE
    try code.emitLocalGet(1); // size
    try code.emitI32Const(@intCast(HEAP_OBJECT_HEADER_SIZE));
    try code.emitI32Add();

    // Align to 8 bytes: (total_size + 7) & ~7
    try code.emitI32Const(7);
    try code.emitI32Add();
    try code.emitI32Const(-8); // ~7 = -8 in two's complement
    try code.emitI32And();
    try code.emitLocalSet(3); // total_size

    // ptr = heap_ptr
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalTee(2); // ptr

    // heap_ptr = heap_ptr + total_size
    try code.emitLocalGet(3);
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // Store metadata at ptr + METADATA_OFFSET
    try code.emitLocalGet(2); // ptr
    try code.emitLocalGet(0); // metadata
    try code.emitI32Store(2, METADATA_OFFSET);

    // Store initial refcount at ptr + REFCOUNT_OFFSET
    try code.emitLocalGet(2); // ptr
    try code.emitI64Const(INITIAL_REFCOUNT);
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // Return ptr + USER_DATA_OFFSET
    try code.emitLocalGet(2);
    try code.emitI32Const(@intCast(USER_DATA_OFFSET));
    try code.emitI32Add();

    // Function type: (i32, i32) -> i32
    const type_idx = try module.addFuncType(&[_]ValType{ .i32, .i32 }, &[_]ValType{.i32});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_retain(obj: i32) -> i32
/// Increments refcount. Returns obj for tail call optimization (Swift pattern).
fn generateRetainFunction(module: *wasm.Module) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameter: obj (local 0)
    // Local 1: header_ptr (i32)
    // Local 2: old_count (i64)

    _ = try code.declareLocals(&[_]ValType{ .i32, .i64 });

    // if (obj == 0) return 0
    try code.emitLocalGet(0);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_I32);
    try code.emitI32Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = obj - USER_DATA_OFFSET
    try code.emitLocalGet(0);
    try code.emitI32Const(@intCast(USER_DATA_OFFSET));
    try code.emitI32Sub();
    try code.emitLocalSet(1);

    // old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    try code.emitLocalGet(1);
    try code.emitI64Load(3, REFCOUNT_OFFSET);
    try code.emitLocalSet(2);

    // if (old_count >= IMMORTAL_REFCOUNT) return obj
    try code.emitLocalGet(2);
    try code.emitI64Const(IMMORTAL_REFCOUNT);
    try code.emitI64GeS();
    try code.emitIf(BLOCK_I32);
    try code.emitLocalGet(0);
    try code.emitReturn();
    try code.emitEnd();

    // i64.store(header_ptr + REFCOUNT_OFFSET, old_count + 1)
    try code.emitLocalGet(1);
    try code.emitLocalGet(2);
    try code.emitI64Const(1);
    try code.emitI64Add();
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // return obj
    try code.emitLocalGet(0);

    // Function type: (i32) -> i32
    const type_idx = try module.addFuncType(&[_]ValType{.i32}, &[_]ValType{.i32});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_release(obj: i32) -> void
/// Decrements refcount. Frees object if count reaches zero.
fn generateReleaseFunction(module: *wasm.Module, heap_ptr_global: u32) !u32 {
    _ = heap_ptr_global; // Will be used for free list in future

    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameter: obj (local 0)
    // Local 1: header_ptr (i32)
    // Local 2: old_count (i64)
    // Local 3: new_count (i64)

    _ = try code.declareLocals(&[_]ValType{ .i32, .i64, .i64 });

    // if (obj == 0) return
    try code.emitLocalGet(0);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = obj - USER_DATA_OFFSET
    try code.emitLocalGet(0);
    try code.emitI32Const(@intCast(USER_DATA_OFFSET));
    try code.emitI32Sub();
    try code.emitLocalSet(1);

    // old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    try code.emitLocalGet(1);
    try code.emitI64Load(3, REFCOUNT_OFFSET);
    try code.emitLocalSet(2);

    // if (old_count >= IMMORTAL_REFCOUNT) return
    try code.emitLocalGet(2);
    try code.emitI64Const(IMMORTAL_REFCOUNT);
    try code.emitI64GeS();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // new_count = old_count - 1
    try code.emitLocalGet(2);
    try code.emitI64Const(1);
    try code.emitI64Sub();
    try code.emitLocalTee(3);

    // i64.store(header_ptr + REFCOUNT_OFFSET, new_count)
    try code.emitLocalGet(1);
    try code.emitLocalGet(3);
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // if (new_count == 0) { /* free object */ }
    // For M15, we don't actually free memory (bump allocator)
    // Future: call destructor, add to free list
    try code.emitLocalGet(3);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    // TODO: Call destructor via metadata lookup
    // TODO: Add to free list for reuse
    // For now, just a marker that the object is dead
    try code.emitEnd();

    // Function type: (i32) -> void
    const type_idx = try module.addFuncType(&[_]ValType{.i32}, &[_]ValType{});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_retain_count(obj: i32) -> i64
/// Returns the current reference count.
fn generateRetainCountFunction(module: *wasm.Module) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // if (obj == 0) return 0
    try code.emitLocalGet(0);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_I64);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = obj - USER_DATA_OFFSET
    // return i64.load(header_ptr + REFCOUNT_OFFSET)
    try code.emitLocalGet(0);
    try code.emitI32Const(@intCast(USER_DATA_OFFSET));
    try code.emitI32Sub();
    try code.emitI64Load(3, REFCOUNT_OFFSET);

    // Function type: (i32) -> i64
    const type_idx = try module.addFuncType(&[_]ValType{.i32}, &[_]ValType{.i64});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_is_uniquely_referenced(obj: i32) -> i32
/// Returns 1 if refcount is exactly 1, 0 otherwise.
fn generateIsUniqueFunction(module: *wasm.Module, retain_count_idx: u32) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // if (obj == 0) return 0
    try code.emitLocalGet(0);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_I32);
    try code.emitI32Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // Call retain_count and check if == 1
    try code.emitLocalGet(0);
    try code.emitCall(retain_count_idx);
    try code.emitI64Const(1);
    try code.emitI64Eq();

    // Function type: (i32) -> i32
    const type_idx = try module.addFuncType(&[_]ValType{.i32}, &[_]ValType{.i32});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

// =============================================================================
// Tests
// =============================================================================

test "memory layout constants" {
    // Verify header size matches design
    try std.testing.expectEqual(@as(u32, 12), HEAP_OBJECT_HEADER_SIZE);
    try std.testing.expectEqual(@as(u32, 0), METADATA_OFFSET);
    try std.testing.expectEqual(@as(u32, 4), REFCOUNT_OFFSET);
    try std.testing.expectEqual(@as(u32, 12), USER_DATA_OFFSET);

    // Verify immortal refcount is max positive i64
    try std.testing.expectEqual(@as(i64, 0x7FFFFFFFFFFFFFFF), IMMORTAL_REFCOUNT);
}

test "addRuntimeFunctions creates all functions" {
    const allocator = std.testing.allocator;
    var module = wasm.Module.init(allocator);
    defer module.deinit();

    const funcs = try addRuntimeFunctions(&module);

    // Verify we got distinct function indices
    try std.testing.expect(funcs.alloc_idx != funcs.retain_idx);
    try std.testing.expect(funcs.retain_idx != funcs.release_idx);
    try std.testing.expect(funcs.release_idx != funcs.retain_count_idx);
    try std.testing.expect(funcs.retain_count_idx != funcs.is_unique_idx);

    // Verify global was created
    try std.testing.expectEqual(@as(u32, 0), funcs.heap_ptr_global);
}

test "generated module emits valid wasm" {
    const allocator = std.testing.allocator;
    var module = wasm.Module.init(allocator);
    defer module.deinit();

    _ = try addRuntimeFunctions(&module);

    // Emit the module
    var output: std.ArrayListUnmanaged(u8) = .{};
    defer output.deinit(allocator);

    try module.emit(output.writer(allocator));

    // Verify magic number
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x00, 0x61, 0x73, 0x6D }, output.items[0..4]);

    // Verify version
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x01, 0x00, 0x00, 0x00 }, output.items[4..8]);
}
