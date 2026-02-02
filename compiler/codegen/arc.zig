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
    /// cot_alloc index
    alloc_idx: u32,

    /// cot_retain index
    retain_idx: u32,

    /// cot_release index
    release_idx: u32,

    /// cot_string_concat index
    string_concat_idx: u32,

    /// cot_append index (stub for now)
    append_idx: u32,

    /// cot_memset_zero index (stub for now)
    memset_zero_idx: u32,

    /// heap_ptr global index
    heap_ptr_global: u32,

    /// Destructor function type index for call_indirect
    destructor_type: u32,
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
pub const ALLOC_NAME = "cot_alloc";
pub const RETAIN_NAME = "cot_retain";
pub const RELEASE_NAME = "cot_release";
pub const STRING_CONCAT_NAME = "cot_string_concat";
pub const APPEND_NAME = "cot_append";
pub const MEMSET_ZERO_NAME = "cot_memset_zero";

// =============================================================================
// Code Generation for Runtime Functions (for new Linker API)
// =============================================================================

/// Adds ARC runtime functions to a Wasm Linker.
/// Returns the indices of the generated functions.
pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker) !RuntimeFunctions {
    // Add heap pointer global (mutable i32, starts at HEAP_START)
    // Note: addGlobal returns index in dynamic list, but SP is at index 0
    // so actual global index is dynamic_idx + 1
    const heap_ptr_dynamic_idx = try linker.addGlobal(.{
        .val_type = .i32,
        .mutable = true,
        .init_i32 = @intCast(HEAP_START),
    });
    const heap_ptr_global = heap_ptr_dynamic_idx + 1; // Offset by SP

    // Generate alloc function: (i64, i64) -> i64
    // Takes (metadata_ptr, size), returns pointer to user data (after header)
    // Reference: Swift's swift_allocObject(metadata, size, align)
    const alloc_type = try linker.addType(&[_]ValType{ .i64, .i64 }, &[_]ValType{.i64});
    const alloc_body = try generateAllocBody(allocator, heap_ptr_global);
    const alloc_idx = try linker.addFunc(.{
        .name = ALLOC_NAME,
        .type_idx = alloc_type,
        .code = alloc_body,
        .exported = false,
    });

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

    // Add destructor type for call_indirect: (i64) -> void
    // Reference: Swift's HeapObjectDestroyer type
    const destructor_type = try linker.addType(&[_]ValType{.i64}, &[_]ValType{});

    // Generate release function: (i64) -> void
    const release_type = destructor_type; // Same signature as destructors
    const release_body = try generateReleaseBody(allocator, destructor_type);
    const release_idx = try linker.addFunc(.{
        .name = RELEASE_NAME,
        .type_idx = release_type,
        .code = release_body,
        .exported = false,
    });

    // Generate string_concat function: (i64, i64, i64, i64) -> i64
    // Takes (s1_ptr, s1_len, s2_ptr, s2_len), returns new_ptr
    // Reference: Go's runtime/string.go concatstrings
    const string_concat_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const string_concat_body = try generateStringConcatBody(allocator, heap_ptr_global);
    const string_concat_idx = try linker.addFunc(.{
        .name = STRING_CONCAT_NAME,
        .type_idx = string_concat_type,
        .code = string_concat_body,
        .exported = false,
    });

    // Generate stub append function (TODO: implement properly for arrays)
    // Takes (arr_ptr, element_ptr) -> new_arr_ptr
    const append_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const append_body = try generateStubBody(allocator);
    const append_idx = try linker.addFunc(.{
        .name = APPEND_NAME,
        .type_idx = append_type,
        .code = append_body,
        .exported = false,
    });

    // Generate stub memset_zero function (TODO: implement properly)
    // Takes (ptr, size) -> void
    const memset_zero_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const memset_zero_body = try generateStubBody(allocator);
    const memset_zero_idx = try linker.addFunc(.{
        .name = MEMSET_ZERO_NAME,
        .type_idx = memset_zero_type,
        .code = memset_zero_body,
        .exported = false,
    });

    return RuntimeFunctions{
        .alloc_idx = alloc_idx,
        .retain_idx = retain_idx,
        .release_idx = release_idx,
        .string_concat_idx = string_concat_idx,
        .append_idx = append_idx,
        .memset_zero_idx = memset_zero_idx,
        .heap_ptr_global = heap_ptr_global,
        .destructor_type = destructor_type,
    };
}

/// Generates a stub function body that returns 0 (for i64 returns) or does nothing (for void)
/// Used for unimplemented functions
fn generateStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Return 0 for any return type (harmless for void returns)
    try code.emitI64Const(0);
    // Note: finish() adds the end opcode automatically

    return try code.finish();
}

/// Generates bytecode for cot_alloc(metadata_ptr: i64, size: i64) -> i64
/// Allocates heap memory with header, returns pointer to user data.
/// Reference: Swift's swift_allocObject (HeapObject.cpp:247-270)
fn generateAllocBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: metadata_ptr (local 0, i64), size (local 1, i64)
    // Local 2: ptr (allocated address, i32)
    // Local 3: total_size (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32 });

    // total_size = (i32)size + HEAP_OBJECT_HEADER_SIZE
    try code.emitLocalGet(1); // size (i64)
    try code.emitI32WrapI64(); // convert to i32
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

    // Store metadata_ptr at ptr + METADATA_OFFSET
    // Reference: Swift stores metadata pointer at object start
    try code.emitLocalGet(2); // ptr
    try code.emitLocalGet(0); // metadata_ptr (i64)
    try code.emitI32WrapI64(); // convert to i32
    try code.emitI32Store(2, METADATA_OFFSET);

    // Store initial refcount at ptr + REFCOUNT_OFFSET
    try code.emitLocalGet(2); // ptr
    try code.emitI64Const(INITIAL_REFCOUNT);
    try code.emitI64Store(3, REFCOUNT_OFFSET);

    // Return (i64)(ptr + USER_DATA_OFFSET)
    try code.emitLocalGet(2);
    try code.emitI32Const(@intCast(USER_DATA_OFFSET));
    try code.emitI32Add();
    try code.emitI64ExtendI32U(); // zero-extend to i64

    return code.finish();
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

/// Offset of destructor function index in metadata
pub const DESTRUCTOR_OFFSET: u32 = 8;

/// Generates bytecode for cot_release(obj: i64) -> void
/// Decrements refcount. Calls destructor if count reaches zero.
/// Reference: Swift's _swift_release_dealloc (HeapObject.cpp:835-836)
fn generateReleaseBody(allocator: std.mem.Allocator, destructor_type_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i64)
    // Local 1: header_ptr (i64)
    // Local 2: old_count (i64)
    // Local 3: new_count (i64)
    // Local 4: destructor_ptr (i32)

    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64, .i64, .i32 });

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

    // if (new_count == 0) { call destructor }
    // Reference: Swift's asFullMetadata(object->metadata)->destroy(object)
    try code.emitLocalGet(3);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);

    // Load metadata_ptr from header
    try code.emitLocalGet(1); // header_ptr
    try code.emitI32WrapI64();
    try code.emitI32Load(2, METADATA_OFFSET); // Load metadata_ptr (i32)

    // Load destructor_ptr from metadata at offset 8
    try code.emitI32Load(2, DESTRUCTOR_OFFSET); // destructor_ptr (i32)
    try code.emitLocalTee(4); // Save to local 4

    // if (destructor_ptr != 0) { call_indirect }
    try code.emitIf(BLOCK_VOID);
    try code.emitLocalGet(0); // Push object ptr as argument
    try code.emitLocalGet(4); // Push destructor table index
    try code.emitCallIndirect(destructor_type_idx, 0); // call_indirect (table 0)
    try code.emitEnd();

    try code.emitEnd();

    return code.finish();
}

/// Generates bytecode for cot_string_concat(s1_ptr, s1_len, s2_ptr, s2_len) -> new_ptr
/// Allocates a new buffer on the heap and copies both strings into it.
/// Reference: Go's runtime/string.go concatstrings
fn generateStringConcatBody(allocator: std.mem.Allocator, heap_ptr_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters:
    //   local 0: s1_ptr (i64)
    //   local 1: s1_len (i64)
    //   local 2: s2_ptr (i64)
    //   local 3: s2_len (i64)
    // Locals:
    //   local 4: new_len (i32)
    //   local 5: new_ptr (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32 });

    // new_len = (i32)s1_len + (i32)s2_len
    try code.emitLocalGet(1); // s1_len (i64)
    try code.emitI32WrapI64();
    try code.emitLocalGet(3); // s2_len (i64)
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitLocalTee(4); // new_len

    // Check for zero length - if both strings empty, return 0
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // Allocate buffer: new_ptr = heap_ptr
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalTee(5); // new_ptr

    // heap_ptr = heap_ptr + ((new_len + 7) & ~7)  // 8-byte aligned
    try code.emitLocalGet(4); // new_len
    try code.emitI32Const(7);
    try code.emitI32Add();
    try code.emitI32Const(-8);
    try code.emitI32And();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // memory.copy(new_ptr, s1_ptr, s1_len)
    // Stack: [dest, src, len] all i32
    try code.emitLocalGet(5); // dest = new_ptr (i32)
    try code.emitLocalGet(0); // src = s1_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalGet(1); // len = s1_len (i64)
    try code.emitI32WrapI64();
    try code.emitMemoryCopy();

    // memory.copy(new_ptr + s1_len, s2_ptr, s2_len)
    try code.emitLocalGet(5); // new_ptr
    try code.emitLocalGet(1); // s1_len (i64)
    try code.emitI32WrapI64();
    try code.emitI32Add(); // dest = new_ptr + s1_len
    try code.emitLocalGet(2); // src = s2_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalGet(3); // len = s2_len (i64)
    try code.emitI32WrapI64();
    try code.emitMemoryCopy();

    // Return (i64)new_ptr
    try code.emitLocalGet(5);
    try code.emitI64ExtendI32U();

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
    const body = try generateReleaseBody(allocator, 0); // type_idx 0 for test
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
    try std.testing.expect(funcs.alloc_idx != funcs.retain_idx);
    try std.testing.expect(funcs.retain_idx != funcs.release_idx);
    try std.testing.expect(funcs.release_idx != funcs.string_concat_idx);

    // Verify functions were added (alloc, retain, release, string_concat, append, memset_zero = 6)
    try std.testing.expectEqual(@as(usize, 6), linker.funcs.items.len);

    // Verify global was added (heap_ptr)
    try std.testing.expectEqual(@as(usize, 1), linker.globals.items.len);
}
