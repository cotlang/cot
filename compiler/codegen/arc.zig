//! ARC Runtime for Cot (WebAssembly)
//!
//! Provides reference counting runtime functions following Swift's ARC patterns.
//! See docs/ARC_DESIGN.md for design rationale.
//!
//! Reference: ~/learning/swift/stdlib/public/runtime/HeapObject.cpp

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;

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
// Runtime Function Info
// =============================================================================

/// Runtime function indices (new simplified API for Linker)
pub const RuntimeFunctions = struct {
    /// cot_retain index
    retain_idx: u32,

    /// cot_release index
    release_idx: u32,
};

/// Legacy runtime function indices (old API for wasm.Module)
/// Kept for backward compatibility with E2E tests
pub const LegacyRuntimeFunctions = struct {
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

/// ARC function names for lookup
pub const RETAIN_NAME = "cot_retain";
pub const RELEASE_NAME = "cot_release";

// =============================================================================
// Code Generation for Runtime Functions (for new Linker API)
// =============================================================================

/// Adds ARC runtime functions to a Wasm Linker.
/// Returns the indices of the generated functions.
pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker) !RuntimeFunctions {
    // Generate retain function: (i64) -> i64
    // Note: Using i64 for pointers to match Cot's default type
    const retain_type = try linker.addType(&[_]ValType{.i64}, &[_]ValType{.i64});
    const retain_body = try generateRetainBody(allocator);
    const retain_idx = try linker.addFunc(.{
        .name = RETAIN_NAME,
        .type_idx = retain_type,
        .code = retain_body,
        .exported = false,
    });

    // Generate release function: (i64) -> void
    const release_type = try linker.addType(&[_]ValType{.i64}, &[_]ValType{});
    const release_body = try generateReleaseBody(allocator);
    const release_idx = try linker.addFunc(.{
        .name = RELEASE_NAME,
        .type_idx = release_type,
        .code = release_body,
        .exported = false,
    });

    return RuntimeFunctions{
        .retain_idx = retain_idx,
        .release_idx = release_idx,
    };
}

/// Generates bytecode for cot_retain(obj: i64) -> i64
/// Increments refcount. Returns obj for tail call optimization (Swift pattern).
fn generateRetainBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i64)
    // Local 1: header_ptr (i64)
    // Local 2: old_count (i64)

    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64 });

    // if (obj == 0) return 0
    try code.emitLocalGet(0);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID); // void type since we return inside
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = obj - USER_DATA_OFFSET
    try code.emitLocalGet(0);
    try code.emitI64Const(@intCast(USER_DATA_OFFSET));
    try code.emitI64Sub();
    try code.emitLocalSet(1);

    // old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    // Need i32 address for memory ops, so wrap
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitI64Load(3, REFCOUNT_OFFSET);
    try code.emitLocalSet(2);

    // if (old_count >= IMMORTAL_REFCOUNT) return obj
    try code.emitLocalGet(2);
    try code.emitI64Const(IMMORTAL_REFCOUNT);
    try code.emitI64GeS();
    try code.emitIf(BLOCK_VOID); // void type since we return inside
    try code.emitLocalGet(0);
    try code.emitReturn();
    try code.emitEnd();

    // i64.store(header_ptr + REFCOUNT_OFFSET, old_count + 1)
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalGet(2);
    try code.emitI64Const(1);
    try code.emitI64Add();
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // return obj
    try code.emitLocalGet(0);

    return code.finish();
}

/// Generates bytecode for cot_release(obj: i64) -> void
/// Decrements refcount. Frees object if count reaches zero.
fn generateReleaseBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i64)
    // Local 1: header_ptr (i64)
    // Local 2: old_count (i64)
    // Local 3: new_count (i64)

    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64, .i64 });

    // if (obj == 0) return
    try code.emitLocalGet(0);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = obj - USER_DATA_OFFSET
    try code.emitLocalGet(0);
    try code.emitI64Const(@intCast(USER_DATA_OFFSET));
    try code.emitI64Sub();
    try code.emitLocalSet(1);

    // old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
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
    try code.emitLocalSet(3); // Store to local without leaving on stack

    // i64.store(header_ptr + REFCOUNT_OFFSET, new_count)
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
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

    return code.finish();
}

// =============================================================================
// Legacy API (for wasm.Module - used by E2E tests)
// =============================================================================

/// Adds ARC runtime functions to a Wasm module (legacy API).
/// Returns the indices of the generated functions.
/// DEPRECATED: Use addToLinker() with the new Linker API instead.
pub fn addRuntimeFunctions(module: *wasm.Module) !LegacyRuntimeFunctions {
    // Add heap_ptr global (mutable i32, initialized to HEAP_START)
    const heap_ptr_global = try module.addGlobal(.i32, true, HEAP_START);

    // Generate runtime functions using i32 pointers (legacy behavior)
    const alloc_idx = try generateLegacyAllocFunction(module, heap_ptr_global);
    const retain_idx = try generateLegacyRetainFunction(module);
    const release_idx = try generateLegacyReleaseFunction(module);
    const retain_count_idx = try generateLegacyRetainCountFunction(module);
    const is_unique_idx = try generateLegacyIsUniqueFunction(module, retain_count_idx);

    return LegacyRuntimeFunctions{
        .alloc_idx = alloc_idx,
        .retain_idx = retain_idx,
        .release_idx = release_idx,
        .retain_count_idx = retain_count_idx,
        .is_unique_idx = is_unique_idx,
        .heap_ptr_global = heap_ptr_global,
    };
}

/// Generates: cot_alloc(metadata: i32, size: i32) -> i32
fn generateLegacyAllocFunction(module: *wasm.Module, heap_ptr_global: u32) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameters: metadata (local 0), size (local 1)
    // Local 2: ptr (allocated address)
    // Local 3: total_size
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32 });

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
    const type_idx = try module.addFuncType(&[_]wasm.ValType{ .i32, .i32 }, &[_]wasm.ValType{.i32});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_retain(obj: i32) -> i32 (legacy i32 pointer version)
fn generateLegacyRetainFunction(module: *wasm.Module) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameter: obj (local 0)
    // Local 1: header_ptr (i32)
    // Local 2: old_count (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64 });

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
    const type_idx = try module.addFuncType(&[_]wasm.ValType{.i32}, &[_]wasm.ValType{.i32});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_release(obj: i32) -> void (legacy i32 pointer version)
fn generateLegacyReleaseFunction(module: *wasm.Module) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameter: obj (local 0)
    // Local 1: header_ptr (i32)
    // Local 2: old_count (i64)
    // Local 3: new_count (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64, .i64 });

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
    try code.emitLocalSet(3); // Store to local without leaving on stack

    // i64.store(header_ptr + REFCOUNT_OFFSET, new_count)
    try code.emitLocalGet(1);
    try code.emitLocalGet(3);
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // if (new_count == 0) { /* free object */ }
    try code.emitLocalGet(3);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitEnd();

    // Function type: (i32) -> void
    const type_idx = try module.addFuncType(&[_]wasm.ValType{.i32}, &[_]wasm.ValType{});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_retain_count(obj: i32) -> i64
fn generateLegacyRetainCountFunction(module: *wasm.Module) !u32 {
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
    const type_idx = try module.addFuncType(&[_]wasm.ValType{.i32}, &[_]wasm.ValType{.i64});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_is_uniquely_referenced(obj: i32) -> i32
fn generateLegacyIsUniqueFunction(module: *wasm.Module, retain_count_idx: u32) !u32 {
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
    const type_idx = try module.addFuncType(&[_]wasm.ValType{.i32}, &[_]wasm.ValType{.i32});
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

test "generateRetainBody produces valid bytecode" {
    const allocator = std.testing.allocator;
    const body = try generateRetainBody(allocator);
    defer allocator.free(body);

    // Should have content
    try std.testing.expect(body.len > 0);

    // Should end with 0x0b (end opcode)
    try std.testing.expectEqual(@as(u8, 0x0b), body[body.len - 1]);
}

test "generateReleaseBody produces valid bytecode" {
    const allocator = std.testing.allocator;
    const body = try generateReleaseBody(allocator);
    defer allocator.free(body);

    // Should have content
    try std.testing.expect(body.len > 0);

    // Should end with 0x0b (end opcode)
    try std.testing.expectEqual(@as(u8, 0x0b), body[body.len - 1]);
}

test "addToLinker creates functions" {
    const allocator = std.testing.allocator;
    var linker = wasm_link.Linker.init(allocator);
    defer linker.deinit();

    const funcs = try addToLinker(allocator, &linker);

    // Verify we got distinct function indices
    try std.testing.expect(funcs.retain_idx != funcs.release_idx);

    // Verify functions were added
    try std.testing.expectEqual(@as(usize, 2), linker.funcs.items.len);
}
