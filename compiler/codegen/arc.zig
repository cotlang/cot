//! ARC Runtime for Cot (WebAssembly)
//!
//! Provides reference counting runtime functions following Swift's ARC patterns.
//! See docs/ARC_DESIGN.md for design rationale.
//!
//! Reference: references/swift/stdlib/public/runtime/HeapObject.cpp

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;

// Block type constants for Wasm control flow (from spec)
const wasm_op = @import("wasm_opcodes.zig");
const BLOCK_VOID: u8 = wasm_op.BLOCK_VOID;
const BLOCK_I32: u8 = @intFromEnum(wasm_op.ValType.i32);
const BLOCK_I64: u8 = @intFromEnum(wasm_op.ValType.i64);

// =============================================================================
// Memory Layout Constants (matching Swift's patterns)
// =============================================================================

/// Size of heap object header: total_size(4) + metadata(4) + refcount(8) = 16 bytes
pub const HEAP_OBJECT_HEADER_SIZE: u32 = 16;

/// Offset of total block size in header (used by freelist allocator)
pub const SIZE_OFFSET: u32 = 0;

/// Offset of metadata pointer in header
pub const METADATA_OFFSET: u32 = 4;

/// Offset of refcount in header
pub const REFCOUNT_OFFSET: u32 = 8;

/// Offset of user data (after header)
pub const USER_DATA_OFFSET: u32 = 16;

/// Offset where freelist next pointer is stored in freed blocks.
/// Uses METADATA_OFFSET (4) instead of SIZE_OFFSET (0) to preserve block size
/// for first-fit allocation. Metadata is irrelevant in freed blocks.
pub const FREELIST_NEXT_OFFSET: u32 = METADATA_OFFSET;

/// Immortal refcount value (Swift's EmbeddedImmortalRefCount pattern)
/// Objects with this refcount are never freed.
pub const IMMORTAL_REFCOUNT: i64 = 0x7FFFFFFFFFFFFFFF;

/// Initial refcount for newly allocated objects
pub const INITIAL_REFCOUNT: i64 = 1;

/// Memory layout globals
pub const HEAP_START: u32 = 0x10000; // 64KB reserved for stack

// =============================================================================
// Alignment Constants (used by alloc, realloc, growslice, string_concat)
// =============================================================================

/// Alignment for heap allocations (8 bytes)
pub const ALIGNMENT: u32 = 8;

/// Alignment - 1 (for rounding up: (size + ALIGN_MINUS_ONE) & ALIGN_MASK)
pub const ALIGN_MINUS_ONE: i32 = @as(i32, @intCast(ALIGNMENT)) - 1;

/// Alignment mask (~(ALIGNMENT-1) in two's complement, for: size & ALIGN_MASK)
pub const ALIGN_MASK: i32 = -@as(i32, @intCast(ALIGNMENT));

// =============================================================================
// Wasm Page Constants (for memory.grow)
// =============================================================================

/// Wasm linear memory page size in bytes
pub const WASM_PAGE_SIZE: u32 = 65536;

/// log2(WASM_PAGE_SIZE) — used with shl/shr to convert pages ↔ bytes
pub const WASM_PAGE_SIZE_LOG2: i32 = 16;

/// WASM_PAGE_SIZE - 1 (for rounding up pages)
pub const WASM_PAGE_SIZE_MINUS_ONE: i32 = @as(i32, @intCast(WASM_PAGE_SIZE)) - 1;

/// OOM indicator from memory.grow (-1 = failure)
pub const MEMORY_GROW_FAILED: i32 = -1;

// =============================================================================
// Runtime Function Info
// =============================================================================

/// Runtime function indices (new simplified API for Linker)
/// Swift ARC functions only - slice functions are in slice_runtime.zig
pub const RuntimeFunctions = struct {
    /// cot_alloc index
    alloc_idx: u32,

    /// cot_retain index
    retain_idx: u32,

    /// cot_release index
    release_idx: u32,

    /// cot_dealloc index
    dealloc_idx: u32,

    /// cot_realloc index
    realloc_idx: u32,

    /// cot_string_concat index
    string_concat_idx: u32,

    /// cot_string_eq index
    string_eq_idx: u32,

    /// cot_memset_zero index
    memset_zero_idx: u32,

    /// memcpy index (Go's memmove / Zig's @memcpy)
    memcpy_idx: u32,

    /// heap_ptr global index
    heap_ptr_global: u32,

    /// freelist_head global index
    freelist_head_global: u32,

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

/// ARC function names for lookup (Swift ARC only)
pub const ALLOC_NAME = "cot_alloc";
pub const RETAIN_NAME = "cot_retain";
pub const RELEASE_NAME = "cot_release";
pub const DEALLOC_NAME = "cot_dealloc";
pub const REALLOC_NAME = "cot_realloc";
pub const STRING_CONCAT_NAME = "cot_string_concat";
pub const STRING_EQ_NAME = "cot_string_eq";
pub const MEMSET_ZERO_NAME = "cot_memset_zero";
pub const MEMCPY_NAME = "memcpy";

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

    // Add freelist head global (mutable i32, starts at 0 = empty)
    const freelist_dynamic_idx = try linker.addGlobal(.{
        .val_type = .i32,
        .mutable = true,
        .init_i32 = 0,
    });
    const freelist_head_global = freelist_dynamic_idx + 1; // Offset by SP

    // Generate alloc function: (i64, i64) -> i64
    // Takes (metadata_ptr, size), returns pointer to user data (after header)
    // Reference: Swift's swift_allocObject(metadata, size, align)
    const alloc_type = try linker.addType(&[_]ValType{ .i64, .i64 }, &[_]ValType{.i64});
    const alloc_body = try generateAllocBody(allocator, heap_ptr_global, freelist_head_global);
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

    // Generate dealloc function: (i64) -> void
    // Returns memory to freelist
    // Reference: Swift's swift_deallocObject (HeapObject.cpp:967-1070)
    const dealloc_type = destructor_type; // Same signature: (i64) -> void
    const dealloc_body = try generateDeallocBody(allocator, freelist_head_global);
    const dealloc_idx = try linker.addFunc(.{
        .name = DEALLOC_NAME,
        .type_idx = dealloc_type,
        .code = dealloc_body,
        .exported = false,
    });

    // Generate release function: (i64) -> void
    const release_type = destructor_type; // Same signature as destructors
    const release_body = try generateReleaseBody(allocator, destructor_type, dealloc_idx);
    const release_idx = try linker.addFunc(.{
        .name = RELEASE_NAME,
        .type_idx = release_type,
        .code = release_body,
        .exported = false,
    });

    // Generate realloc function: (i64, i64) -> i64
    // Takes (obj, new_size), returns new pointer to user data
    const realloc_type = alloc_type; // Same signature: (i64, i64) -> i64
    const realloc_body = try generateReallocBody(allocator, heap_ptr_global, freelist_head_global, alloc_idx, dealloc_idx);
    const realloc_idx = try linker.addFunc(.{
        .name = REALLOC_NAME,
        .type_idx = realloc_type,
        .code = realloc_body,
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

    // Generate string_eq function: (i64, i64, i64, i64) -> i64
    // Takes (s1_ptr, s1_len, s2_ptr, s2_len), returns 1 if equal, 0 if not
    // Reference: Go runtime/string.go stringEqual
    const string_eq_type = string_concat_type; // Same signature: (i64, i64, i64, i64) -> i64
    const string_eq_body = try generateStringEqBody(allocator);
    const string_eq_idx = try linker.addFunc(.{
        .name = STRING_EQ_NAME,
        .type_idx = string_eq_type,
        .code = string_eq_body,
        .exported = false,
    });

    // Generate memset_zero function
    // Takes (ptr: i64, size: i64) -> void — zeros `size` bytes starting at `ptr`
    const memset_zero_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const memset_zero_body = try generateMemsetZeroBody(allocator);
    const memset_zero_idx = try linker.addFunc(.{
        .name = MEMSET_ZERO_NAME,
        .type_idx = memset_zero_type,
        .code = memset_zero_body,
        .exported = false,
    });

    // Generate memcpy function: (dst: i64, src: i64, num_bytes: i64) -> void
    // Go's memmove semantics: handles overlapping regions safely.
    // Reference: Go runtime/memmove_*.s, C memmove(3)
    const memcpy_type = memset_zero_type; // Reuse type if same, else create new
    _ = memcpy_type;
    const memcpy_fn_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{},
    );
    const memcpy_body = try generateMemcpyBody(allocator);
    const memcpy_idx = try linker.addFunc(.{
        .name = MEMCPY_NAME,
        .type_idx = memcpy_fn_type,
        .code = memcpy_body,
        .exported = false,
    });

    return RuntimeFunctions{
        .alloc_idx = alloc_idx,
        .retain_idx = retain_idx,
        .release_idx = release_idx,
        .dealloc_idx = dealloc_idx,
        .realloc_idx = realloc_idx,
        .string_concat_idx = string_concat_idx,
        .string_eq_idx = string_eq_idx,
        .memset_zero_idx = memset_zero_idx,
        .memcpy_idx = memcpy_idx,
        .heap_ptr_global = heap_ptr_global,
        .freelist_head_global = freelist_head_global,
        .destructor_type = destructor_type,
    };
}

/// Generates a stub function body for void functions.
/// Reference: Swift void functions have empty bodies (HeapObject.cpp)
/// Wasm validation requires stack to be empty at end for void functions.
fn generateVoidStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // Empty body - finish() adds the end opcode automatically
    return try code.finish();
}

/// Generates bytecode for cot_memset_zero(ptr: i64, size: i64) -> void
/// Zeros `size` bytes starting at `ptr` using a byte-store loop.
/// Reference: Go's memclrNoHeapPointers / C's memset(p, 0, n)
fn generateMemsetZeroBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: ptr (local 0, i64), size (local 1, i64)
    // Local 2: addr_i32 (i32) - ptr as i32 for memory access
    // Local 3: len_i32 (i32) - size as i32
    // Local 4: counter (i32) - loop counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32 });

    // addr_i32 = (i32)ptr
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(2);
    // len_i32 = (i32)size
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(3);

    // Byte-zero loop: i=0; while(i < len) { store8(addr+i, 0); i++ }
    try code.emitI32Const(0);
    try code.emitLocalSet(4);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    // if (counter >= len) break
    try code.emitLocalGet(4);
    try code.emitLocalGet(3);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    // store8(addr + counter, 0)
    try code.emitLocalGet(2);
    try code.emitLocalGet(4);
    try code.emitI32Add();
    try code.emitI64Const(0);
    try code.emitI64Store8(0);
    // counter++
    try code.emitLocalGet(4);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(4);
    try code.emitBr(0);
    try code.emitEnd();
    try code.emitEnd();

    return code.finish();
}

/// Generates bytecode for memcpy(dst: i64, src: i64, num_bytes: i64) -> void
/// Copies `num_bytes` bytes from `src` to `dst`.
/// Handles overlapping regions safely (memmove semantics).
/// Reference: Go runtime/memmove_*.s — forward copy if dst <= src, backward if dst > src
/// Uses i64 load8_u/store8 ops matching the memset_zero pattern.
fn generateMemcpyBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: dst (local 0, i64), src (local 1, i64), num_bytes (local 2, i64)
    // Local 3: dst_i32 (i32)
    // Local 4: src_i32 (i32)
    // Local 5: len_i32 (i32)
    // Local 6: counter (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i32 });

    // Convert i64 params to i32 for Wasm memory ops
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(3); // dst_i32
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(4); // src_i32
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalSet(5); // len_i32

    // if (len == 0) return
    try code.emitLocalGet(5);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // Go memmove: if dst > src && overlap possible, copy backward
    try code.emitLocalGet(3);
    try code.emitLocalGet(4);
    try code.emitI32GtU();
    try code.emitIf(BLOCK_VOID);

    // --- Backward copy: counter = len-1; while(counter >= 0) { dst[c] = src[c]; c-- } ---
    try code.emitLocalGet(5);
    try code.emitI32Const(1);
    try code.emitI32Sub();
    try code.emitLocalSet(6); // counter = len - 1
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    // if counter < 0, break (i32.lt_s for signed comparison)
    try code.emitLocalGet(6);
    try code.emitI32Const(0);
    try code.buf.append(allocator, wasm_op.Op.i32_lt_s);
    try code.emitBrIf(1);
    // mem[dst+counter] = mem[src+counter]  (byte copy via i64.load8_u / i64.store8)
    // i64.store8 stack: [addr:i32, value:i64] → []
    try code.emitLocalGet(3);  // dst_i32
    try code.emitLocalGet(6);  // counter
    try code.emitI32Add();     // dst + counter (i32 addr for store)
    try code.emitLocalGet(4);  // src_i32
    try code.emitLocalGet(6);  // counter
    try code.emitI32Add();     // src + counter (i32 addr for load)
    try code.emitI64Load8U(0); // load byte → i64
    try code.emitI64Store8(0); // store byte ← i64
    // counter--
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Sub();
    try code.emitLocalSet(6);
    try code.emitBr(0);
    try code.emitEnd(); // end loop
    try code.emitEnd(); // end block

    try code.emitElse();

    // --- Forward copy: counter = 0; while(counter < len) { dst[c] = src[c]; c++ } ---
    try code.emitI32Const(0);
    try code.emitLocalSet(6); // counter = 0
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    // if counter >= len, break
    try code.emitLocalGet(6);
    try code.emitLocalGet(5);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    // mem[dst+counter] = mem[src+counter]
    try code.emitLocalGet(3);  // dst_i32
    try code.emitLocalGet(6);  // counter
    try code.emitI32Add();     // dst + counter
    try code.emitLocalGet(4);  // src_i32
    try code.emitLocalGet(6);  // counter
    try code.emitI32Add();     // src + counter
    try code.emitI64Load8U(0); // load byte
    try code.emitI64Store8(0); // store byte
    // counter++
    try code.emitLocalGet(6);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(6);
    try code.emitBr(0);
    try code.emitEnd(); // end loop
    try code.emitEnd(); // end block

    try code.emitEnd(); // end if/else

    return code.finish();
}

/// Generates a stub function body that returns 0 (for i64 returns).
/// Used for unimplemented functions that need to return a value.
fn generateI64StubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(0);
    return try code.finish();
}

/// Generates bytecode for cot_alloc(metadata_ptr: i64, size: i64) -> i64
/// Allocates heap memory with header, returns pointer to user data.
/// Uses bump allocation. Freed blocks go to freelist (checked by Wasm-side alloc only).
/// Reference: Swift's swift_allocObject (HeapObject.cpp:247-270)
fn generateAllocBody(allocator: std.mem.Allocator, heap_ptr_global: u32, freelist_head_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: metadata_ptr (local 0, i64), size (local 1, i64)
    // Local 2: ptr (allocated address, i32)
    // Local 3: total_size (i32)
    // Local 4: found (i32, flag for freelist hit)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32 });

    // total_size = (i32)size + HEAP_OBJECT_HEADER_SIZE
    try code.emitLocalGet(1); // size (i64)
    try code.emitI32WrapI64(); // convert to i32
    try code.emitI32Const(@intCast(HEAP_OBJECT_HEADER_SIZE));
    try code.emitI32Add();

    // Align to 8 bytes: (total_size + ALIGN_MINUS_ONE) & ALIGN_MASK
    try code.emitI32Const(ALIGN_MINUS_ONE);
    try code.emitI32Add();
    try code.emitI32Const(ALIGN_MASK);
    try code.emitI32And();
    try code.emitLocalSet(3); // total_size

    // --- Freelist first-fit (head only) ---
    // Check if freelist head has a block that fits
    try code.emitGlobalGet(freelist_head_global);
    try code.emitLocalTee(2); // ptr = freelist_head
    try code.emitIf(BLOCK_VOID); // if (freelist_head != 0)

    // block_size = i32.load(ptr + SIZE_OFFSET)
    try code.emitLocalGet(2);
    try code.emitI32Load(2, SIZE_OFFSET);
    // block_size >= total_size?
    try code.emitLocalGet(3);
    try code.emitI32GeU();
    try code.emitIf(BLOCK_VOID); // if (block_size >= total_size)

    // Unlink head: freelist_head = i32.load(ptr + FREELIST_NEXT_OFFSET)
    try code.emitLocalGet(2);
    try code.emitI32Load(2, FREELIST_NEXT_OFFSET);
    try code.emitGlobalSet(freelist_head_global);

    // found = 1
    try code.emitI32Const(1);
    try code.emitLocalSet(4);

    try code.emitEnd(); // end block_size >= total_size
    try code.emitEnd(); // end freelist_head != 0

    // --- Bump allocation fallback ---
    // if (!found) bump allocate with bounds check + memory.grow
    // Reference: Go runtime/mem_wasm.go sbrk():
    //   if bl+n > blocMax { grow := (bl+n-blocMax)/pageSize; growMemory(grow) }
    try code.emitLocalGet(4);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);

    // ptr = heap_ptr (save current position)
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalSet(2); // ptr = heap_ptr

    // Check bounds: heap_ptr + total_size > memory.size * WASM_PAGE_SIZE
    // Reference: Go sbrk: if bl+n > blocMax
    try code.emitLocalGet(2); // heap_ptr
    try code.emitLocalGet(3); // total_size
    try code.emitI32Add(); // new_top
    try code.emitMemorySize(); // current pages (i32)
    try code.emitI32Const(WASM_PAGE_SIZE_LOG2);
    try code.emitI32Shl(); // current_bytes = pages << WASM_PAGE_SIZE_LOG2
    try code.emitI32GtU(); // new_top > current_bytes?
    try code.emitIf(BLOCK_VOID);

    // Need more memory. Compute pages needed:
    // grow = (new_top - current_bytes + WASM_PAGE_SIZE_MINUS_ONE) / WASM_PAGE_SIZE
    // Reference: Go sbrk: grow := divRoundUp(bl+n-blocMax, physPageSize)
    try code.emitLocalGet(2); // heap_ptr
    try code.emitLocalGet(3); // total_size
    try code.emitI32Add(); // new_top
    try code.emitMemorySize(); // current pages
    try code.emitI32Const(WASM_PAGE_SIZE_LOG2);
    try code.emitI32Shl(); // current_bytes
    try code.emitI32Sub(); // new_top - current_bytes
    try code.emitI32Const(WASM_PAGE_SIZE_MINUS_ONE);
    try code.emitI32Add(); // round up
    try code.emitI32Const(WASM_PAGE_SIZE_LOG2);
    try code.emitI32ShrU(); // / WASM_PAGE_SIZE = pages needed

    // memory.grow(pages)
    // Reference: Go sbrk: if growMemory(grow) < 0 { return nil }
    try code.emitMemoryGrow();
    try code.emitI32Const(MEMORY_GROW_FAILED);
    try code.emitI32Eq();
    try code.emitIf(BLOCK_VOID);
    // OOM: trap (Swift: swift_abortAllocationFailure)
    try code.emitUnreachable();
    try code.emitEnd(); // end OOM check

    try code.emitEnd(); // end memory.grow needed

    // Bump: heap_ptr += total_size
    try code.emitLocalGet(2); // ptr = old heap_ptr
    try code.emitLocalGet(3); // total_size
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global); // heap_ptr = ptr + total_size

    try code.emitEnd(); // end bump allocation

    // --- Initialize header ---
    // Store total_size at ptr + SIZE_OFFSET
    try code.emitLocalGet(2);
    try code.emitLocalGet(3);
    try code.emitI32Store(2, SIZE_OFFSET);

    // Store metadata_ptr at ptr + METADATA_OFFSET
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

/// Generates bytecode for cot_dealloc(obj: i64) -> void
/// Returns memory to freelist. Reference: Swift's swift_deallocObject.
fn generateDeallocBody(allocator: std.mem.Allocator, freelist_head_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i64)
    // Local 1: header_ptr (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{.i32});

    // if (obj == 0) return
    try code.emitLocalGet(0);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = (i32)(obj - USER_DATA_OFFSET)
    try code.emitLocalGet(0);
    try code.emitI64Const(@intCast(USER_DATA_OFFSET));
    try code.emitI64Sub();
    try code.emitI32WrapI64();
    try code.emitLocalSet(1);

    // Push onto freelist: i32.store(header_ptr + FREELIST_NEXT_OFFSET, freelist_head)
    try code.emitLocalGet(1);
    try code.emitGlobalGet(freelist_head_global);
    try code.emitI32Store(2, FREELIST_NEXT_OFFSET);

    // freelist_head = header_ptr
    try code.emitLocalGet(1);
    try code.emitGlobalSet(freelist_head_global);

    return code.finish();
}

/// Generates bytecode for cot_realloc(obj: i64, new_size: i64) -> i64
/// Reallocates memory: if new size fits, reuse; else alloc new, copy, dealloc old.
/// Note: On grow, metadata is passed as 0 since cot_realloc is only used by the
/// @realloc builtin (raw allocations), not by ARC objects (which have fixed sizes).
fn generateReallocBody(allocator: std.mem.Allocator, heap_ptr_global: u32, freelist_head_global: u32, alloc_idx: u32, dealloc_idx: u32) ![]const u8 {
    _ = heap_ptr_global;
    _ = freelist_head_global;
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: obj (local 0, i64), new_size (local 1, i64)
    // Local 2: header_ptr (i32)
    // Local 3: old_total (i32)
    // Local 4: new_total (i32)
    // Local 5: new_obj (i64)
    // Local 6: copy_size (i32)
    // Local 7: i (i32) - byte-copy loop counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i64, .i32, .i32 });

    // if (obj == 0) return cot_alloc(0, new_size)
    try code.emitLocalGet(0);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0); // metadata = 0
    try code.emitLocalGet(1); // new_size
    try code.emitCall(alloc_idx);
    try code.emitReturn();
    try code.emitEnd();

    // header_ptr = (i32)(obj - USER_DATA_OFFSET)
    try code.emitLocalGet(0);
    try code.emitI64Const(@intCast(USER_DATA_OFFSET));
    try code.emitI64Sub();
    try code.emitI32WrapI64();
    try code.emitLocalSet(2);

    // old_total = i32.load(header_ptr + SIZE_OFFSET)
    try code.emitLocalGet(2);
    try code.emitI32Load(2, SIZE_OFFSET);
    try code.emitLocalSet(3);

    // new_total = (new_size + HEADER_SIZE + ALIGN_MINUS_ONE) & ALIGN_MASK
    try code.emitLocalGet(1); // new_size (i64)
    try code.emitI32WrapI64();
    try code.emitI32Const(@intCast(HEAP_OBJECT_HEADER_SIZE));
    try code.emitI32Add();
    try code.emitI32Const(ALIGN_MINUS_ONE);
    try code.emitI32Add();
    try code.emitI32Const(ALIGN_MASK);
    try code.emitI32And();
    try code.emitLocalSet(4);

    // if (new_total <= old_total) { update size, return obj }
    try code.emitLocalGet(4);
    try code.emitLocalGet(3);
    try code.emitI32LeU();
    try code.emitIf(BLOCK_VOID);
    try code.emitLocalGet(2);
    try code.emitLocalGet(4);
    try code.emitI32Store(2, SIZE_OFFSET); // update size
    try code.emitLocalGet(0); // return original obj
    try code.emitReturn();
    try code.emitEnd();

    // Grow: allocate new block, copy, free old
    // new_obj = cot_alloc(0, new_size)
    try code.emitI64Const(0); // metadata = 0
    try code.emitLocalGet(1); // new_size
    try code.emitCall(alloc_idx);
    try code.emitLocalSet(5); // new_obj

    // copy_size = old_total - HEADER_SIZE (old payload size)
    try code.emitLocalGet(3); // old_total
    try code.emitI32Const(@intCast(HEAP_OBJECT_HEADER_SIZE));
    try code.emitI32Sub();
    try code.emitLocalSet(6);

    // Byte-copy: copy old payload into new allocation
    // Store dest/src as i32 into temp locals for the byte-copy loop
    // Reuse local 2 (header_ptr, no longer needed) as dest_i32
    // Reuse local 4 (new_total, no longer needed) as src_i32
    try code.emitLocalGet(5); // new_obj (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(2); // dest_i32
    try code.emitLocalGet(0); // obj (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(4); // src_i32
    // copy_size is in local 6, loop counter in local 7
    try code.emitByteCopyLoop(2, 4, 6, 7);

    // cot_dealloc(obj)
    try code.emitLocalGet(0);
    try code.emitCall(dealloc_idx);

    // return new_obj
    try code.emitLocalGet(5);

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
/// Decrements refcount. Calls destructor then dealloc if count reaches zero.
/// Reference: Swift's _swift_release_dealloc (HeapObject.cpp:835-837)
fn generateReleaseBody(allocator: std.mem.Allocator, destructor_type_idx: u32, dealloc_idx: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i64)
    // Local 1: header_ptr (i64)
    // Local 2: old_count (i64)
    // Local 3: new_count (i64)
    // Local 4: metadata_ptr (i32)
    // Local 5: destructor_ptr (i32)

    _ = try code.declareLocals(&[_]wasm.ValType{ .i64, .i64, .i64, .i32, .i32 });

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

    // if (new_count == 0) { load metadata, check destructor, call dealloc }
    try code.emitLocalGet(3);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);

    // Load metadata_ptr from header
    try code.emitLocalGet(1); // header_ptr
    try code.emitI32WrapI64();
    try code.emitI32Load(2, METADATA_OFFSET); // Load metadata_ptr (i32)
    try code.emitLocalTee(4); // Save to local 4

    // if (metadata_ptr != 0) - only look up destructor if metadata exists
    try code.emitIf(BLOCK_VOID);

    // Load destructor_ptr from metadata at offset 8
    try code.emitLocalGet(4); // metadata_ptr
    try code.emitI32Load(2, DESTRUCTOR_OFFSET); // destructor_ptr (i32)
    try code.emitLocalTee(5); // Save to local 5

    // if (destructor_ptr != 0) { call_indirect }
    try code.emitIf(BLOCK_VOID);
    try code.emitLocalGet(0); // Push object ptr as argument
    try code.emitLocalGet(5); // Push destructor table index
    try code.emitCallIndirect(destructor_type_idx, 0); // call_indirect (table 0)
    try code.emitEnd();

    try code.emitEnd(); // end metadata_ptr != 0

    // Dealloc: return memory to freelist
    try code.emitLocalGet(0); // obj
    try code.emitCall(dealloc_idx);

    try code.emitEnd();

    return code.finish();
}

/// Generates bytecode for cot_string_eq(s1_ptr, s1_len, s2_ptr, s2_len) -> i64
/// Returns 1 if strings are equal, 0 if not.
/// Reference: Go runtime/string.go stringEqual
fn generateStringEqBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters:
    //   local 0: s1_ptr (i64)
    //   local 1: s1_len (i64)
    //   local 2: s2_ptr (i64)
    //   local 3: s2_len (i64)
    // Locals:
    //   local 4: len_i32 (i32) - length as i32
    //   local 5: p1_i32 (i32) - ptr1 as i32
    //   local 6: p2_i32 (i32) - ptr2 as i32
    //   local 7: counter (i32) - byte-compare loop counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i32 });

    // if (s1_len != s2_len) return 0
    try code.emitLocalGet(1); // s1_len
    try code.emitLocalGet(3); // s2_len
    try code.emitI64Ne();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();

    // Pointer equality fast path (Go memeqbody: if ptrs equal, return 1)
    try code.emitLocalGet(0); // s1_ptr
    try code.emitLocalGet(2); // s2_ptr
    try code.emitI64Eq();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(1);
    try code.emitReturn();
    try code.emitEnd();

    // len_i32 = (i32)s1_len
    try code.emitLocalGet(1);
    try code.emitI32WrapI64();
    try code.emitLocalSet(4);

    // if (len == 0) return 1  (both empty)
    try code.emitLocalGet(4);
    try code.emitI32Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(1);
    try code.emitReturn();
    try code.emitEnd();

    // p1_i32 = (i32)s1_ptr, p2_i32 = (i32)s2_ptr
    try code.emitLocalGet(0);
    try code.emitI32WrapI64();
    try code.emitLocalSet(5);
    try code.emitLocalGet(2);
    try code.emitI32WrapI64();
    try code.emitLocalSet(6);

    // Byte-compare loop: counter=0; while(counter < len) { if(p1[c] != p2[c]) return 0; c++ }
    try code.emitI32Const(0);
    try code.emitLocalSet(7);
    try code.emitBlock(BLOCK_VOID);
    try code.emitLoop(BLOCK_VOID);
    // if (counter >= len) break
    try code.emitLocalGet(7);
    try code.emitLocalGet(4);
    try code.emitI32GeU();
    try code.emitBrIf(1);
    // load byte from p1[counter]
    try code.emitLocalGet(5);
    try code.emitLocalGet(7);
    try code.emitI32Add();
    try code.emitI64Load8U(0); // byte1 (i64)
    // load byte from p2[counter]
    try code.emitLocalGet(6);
    try code.emitLocalGet(7);
    try code.emitI32Add();
    try code.emitI64Load8U(0); // byte2 (i64)
    // if (byte1 != byte2) return 0
    try code.emitI64Ne();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(0);
    try code.emitReturn();
    try code.emitEnd();
    // counter++
    try code.emitLocalGet(7);
    try code.emitI32Const(1);
    try code.emitI32Add();
    try code.emitLocalSet(7);
    try code.emitBr(0);
    try code.emitEnd(); // end loop
    try code.emitEnd(); // end block

    // All bytes matched, return 1
    try code.emitI64Const(1);

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
    //   local 6: tmp_src (i32) - for byte-copy loop
    //   local 7: tmp_len (i32) - for byte-copy loop
    //   local 8: tmp_dest (i32) - for byte-copy loop (2nd copy)
    //   local 9: counter (i32) - byte-copy loop counter
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i32, .i32, .i32, .i32, .i32 });

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

    // heap_ptr = heap_ptr + ((new_len + ALIGN_MINUS_ONE) & ALIGN_MASK)
    try code.emitLocalGet(4); // new_len
    try code.emitI32Const(ALIGN_MINUS_ONE);
    try code.emitI32Add();
    try code.emitI32Const(ALIGN_MASK);
    try code.emitI32And();
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // Copy s1 into new buffer: byte_copy(dest=new_ptr, src=s1_ptr, len=s1_len)
    try code.emitLocalGet(0); // s1_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(6); // tmp_src = s1_ptr as i32
    try code.emitLocalGet(1); // s1_len (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(7); // tmp_len = s1_len as i32
    // dest is local 5 (new_ptr), already i32
    try code.emitByteCopyLoop(5, 6, 7, 9);

    // Copy s2 into new buffer: byte_copy(dest=new_ptr+s1_len, src=s2_ptr, len=s2_len)
    try code.emitLocalGet(5); // new_ptr
    try code.emitLocalGet(1); // s1_len (i64)
    try code.emitI32WrapI64();
    try code.emitI32Add();
    try code.emitLocalSet(8); // tmp_dest = new_ptr + s1_len
    try code.emitLocalGet(2); // s2_ptr (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(6); // tmp_src = s2_ptr as i32
    try code.emitLocalGet(3); // s2_len (i64)
    try code.emitI32WrapI64();
    try code.emitLocalSet(7); // tmp_len = s2_len as i32
    try code.emitByteCopyLoop(8, 6, 7, 9);

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

    // Add freelist_head global (mutable i32, starts at 0 = empty)
    const freelist_head_global = try module.addGlobal(.i32, true, 0);

    // Generate runtime functions using i32 pointers (legacy behavior)
    const alloc_idx = try generateLegacyAllocFunction(module, heap_ptr_global);
    const retain_idx = try generateLegacyRetainFunction(module);
    const dealloc_idx = try generateLegacyDeallocFunction(module, freelist_head_global);
    const release_idx = try generateLegacyReleaseFunction(module, dealloc_idx);
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

    // Align to 8 bytes: (total_size + ALIGN_MINUS_ONE) & ALIGN_MASK
    try code.emitI32Const(ALIGN_MINUS_ONE);
    try code.emitI32Add();
    try code.emitI32Const(ALIGN_MASK);
    try code.emitI32And();
    try code.emitLocalSet(3); // total_size

    // ptr = heap_ptr
    try code.emitGlobalGet(heap_ptr_global);
    try code.emitLocalTee(2); // ptr

    // heap_ptr = heap_ptr + total_size
    try code.emitLocalGet(3);
    try code.emitI32Add();
    try code.emitGlobalSet(heap_ptr_global);

    // Store total_size at ptr + SIZE_OFFSET
    try code.emitLocalGet(2); // ptr
    try code.emitLocalGet(3); // total_size
    try code.emitI32Store(2, SIZE_OFFSET);

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

/// Generates: cot_dealloc(obj: i32) -> void (legacy i32 pointer version)
/// Pushes freed block onto freelist.
fn generateLegacyDeallocFunction(module: *wasm.Module, freelist_head_global: u32) !u32 {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Parameter: obj (local 0, i32)
    // Local 1: header_ptr (i32)
    _ = try code.declareLocals(&[_]wasm.ValType{.i32});

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

    // i32.store(header_ptr + FREELIST_NEXT_OFFSET, freelist_head)
    try code.emitLocalGet(1);
    try code.emitGlobalGet(freelist_head_global);
    try code.emitI32Store(2, FREELIST_NEXT_OFFSET);

    // freelist_head = header_ptr
    try code.emitLocalGet(1);
    try code.emitGlobalSet(freelist_head_global);

    // Function type: (i32) -> void
    const type_idx = try module.addFuncType(&[_]wasm.ValType{.i32}, &[_]wasm.ValType{});
    const func_idx = try module.addFunc(type_idx);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);

    return func_idx;
}

/// Generates: cot_release(obj: i32) -> void (legacy i32 pointer version)
fn generateLegacyReleaseFunction(module: *wasm.Module, dealloc_idx: u32) !u32 {
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

    // if (new_count == 0) { dealloc }
    try code.emitLocalGet(3);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    try code.emitLocalGet(0); // obj
    try code.emitCall(dealloc_idx);
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
    // Verify header size matches design: total_size(4) + metadata(4) + refcount(8) = 16
    try std.testing.expectEqual(@as(u32, 16), HEAP_OBJECT_HEADER_SIZE);
    try std.testing.expectEqual(@as(u32, 0), SIZE_OFFSET);
    try std.testing.expectEqual(@as(u32, 4), METADATA_OFFSET);
    try std.testing.expectEqual(@as(u32, 8), REFCOUNT_OFFSET);
    try std.testing.expectEqual(@as(u32, 16), USER_DATA_OFFSET);

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
    const body = try generateReleaseBody(allocator, 0, 0); // type_idx 0, dealloc_idx 0 for test
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
    try std.testing.expect(funcs.dealloc_idx != funcs.alloc_idx);
    try std.testing.expect(funcs.realloc_idx != funcs.alloc_idx);

    // Verify functions were added (alloc, retain, dealloc, release, realloc, string_concat, string_eq, memset_zero, memcpy = 9)
    try std.testing.expectEqual(@as(usize, 9), linker.funcs.items.len);

    // Verify globals were added (heap_ptr, freelist_head)
    try std.testing.expectEqual(@as(usize, 2), linker.globals.items.len);
}
