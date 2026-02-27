//! ARC Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates core ARC functions as CLIF IR, compiled through native_compile.compile().
//! Uses libc malloc/free for memory management (correct semantics, no freelist optimization).
//!
//! Header layout (24 bytes, native 64-bit):
//!   Offset 0:  alloc_size (i64) — total allocation including header (for realloc)
//!   Offset 8:  metadata   (i64) — pointer to type metadata struct (HeapMetadata*)
//!   Offset 16: refcount   (i64) — reference count (InlineRefCounts)
//!   Offset 24: user_data  [...] — actual object data starts here
//!
//! Swift HeapObject layout (HeapObject.h):
//!   struct HeapObject {
//!     HeapMetadata const *metadata;   // 8 bytes — type metadata pointer
//!     InlineRefCounts refCounts;      // 8 bytes — reference count
//!   };
//!
//! We add alloc_size for realloc support (Swift tracks this via malloc_size()).
//!
//! Metadata struct layout (when non-null):
//!   Offset 0: type_id          (i64) — unique type identifier
//!   Offset 8: destructor_ptr   (i64) — function pointer to destructor (0 if none)
//!
//! Destructor dispatch (Swift _swift_release_dealloc pattern):
//!   When refcount reaches 0, release() loads metadata pointer from header,
//!   then loads destructor function pointer from metadata+8. If non-zero,
//!   calls destructor(obj) via call_indirect before calling dealloc(obj).
//!
//! Reference: compiler/codegen/arc.zig (Wasm ARC — similar logic, 32-bit pointers)
//! Reference: swift/stdlib/public/runtime/HeapObject.cpp:835-837 (_swift_release_dealloc)
//! Reference: swift/stdlib/public/SwiftShims/swift/shims/HeapObject.h
//! Reference: cg_clif abi/mod.rs:183-201 (lib_call pattern for external calls)

const std = @import("std");
const Allocator = std.mem.Allocator;

const clif = @import("../../ir/clif/mod.zig");
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
const native_compile = @import("compile.zig");

const debug = @import("../../pipeline_debug.zig");

// Native 64-bit ARC header (24 bytes):
//   Offset 0:  alloc_size (i64) — total allocation size including header
//   Offset 8:  metadata   (i64) — HeapMetadata* (full 64-bit pointer)
//   Offset 16: refcount   (i64) — reference count (InlineRefCounts)
//   Offset 24: user_data  [...] — actual object data
//
// Wasm path uses 16-byte header with i32 fields (arc.zig).
// Native needs i64 metadata for 64-bit function pointers in destructor dispatch.
//
// Reference: Swift HeapObject.h — metadata is a full pointer
// Reference: arc.zig — Wasm header layout (SIZE_OFFSET=0/i32, METADATA_OFFSET=4/i32, REFCOUNT_OFFSET=8/i64)
const HEAP_OBJECT_HEADER_SIZE: i64 = 24;
const SIZE_OFFSET: i32 = 0; // alloc_size (i64) — needed for realloc copy calculation
const METADATA_OFFSET: i32 = 8; // HeapMetadata* (i64) — type metadata pointer
const REFCOUNT_OFFSET: i32 = 16; // InlineRefCounts (i64) — reference count

// Swift InlineRefCounts bit layout (RefCount.h:241-285)
//
//   Bit  0:       PureSwiftDealloc (1 bit)  — always 1 (no ObjC)
//   Bits 1-31:    UnownedRefCount  (31 bits) — extra count (+1 = 1 logical)
//   Bits 0-31:    IsImmortal       (32 bits) — overlaps above; all 32 bits set = immortal
//   Bit  32:      IsDeiniting      (1 bit)   — strong RC hit zero, destructor running
//   Bits 33-62:   StrongExtraRC    (30 bits) — extra count (+1 = 1 logical)
//   Bit  63:      UseSlowRC        (1 bit)   — side table mode (Phase 3)
//
// Strong uses "extra count": physical 0 = logical 1 (new object: StrongExtra=0 → 1 strong ref).
// Unowned uses DIRECT count: physical 1 = logical 1 (initial +1 on behalf of strong refs).
const STRONG_RC_ONE: i64 = @as(i64, 1) << 33; // 0x0000000200000000
const IS_DEINITING_BIT: i64 = @as(i64, 1) << 32; // 0x0000000100000000
const UNOWNED_RC_ONE: i64 = @as(i64, 1) << 1; // 0x0000000000000002
const PURE_SWIFT_DEALLOC: i64 = 1; // 0x0000000000000001
// Initial: PureSwiftDealloc=1, UnownedRefCount=1 (the +1 on behalf of strong references).
// Swift RefCount.h:751 — RefCountBits(0, 1) = (0 << 33) | (1 << 0) | (1 << 1) = 3
// NOTE: Unowned uses DIRECT count (physical=logical), NOT extra count.
// Only StrongExtra uses extra count (physical 0 = logical 1).
const INITIAL_REFCOUNT: i64 = PURE_SWIFT_DEALLOC | UNOWNED_RC_ONE; // = 3
// Immortal: all 64 bits set — passes Swift's IsImmortal mask (low 32 bits all set)
const IMMORTAL_REFCOUNT: i64 = @bitCast(@as(u64, 0xFFFFFFFF_FFFFFFFF));
const STRONG_EXTRA_MASK: i64 = @bitCast(@as(u64, 0x7FFFFFFE_00000000)); // bits 33-62
const USE_SLOW_RC_BIT: i64 = @bitCast(@as(u64, 0x8000000000000000)); // bit 63
const STRONG_EXTRA_SHIFT: u6 = 33;

// Side table layout (24 bytes, Swift RefCount.h SideTableRefCountBits):
//   Offset 0:  object_ptr  (i64) — pointer to object's user data (0 when freed)
//   Offset 8:  refcounts   (i64) — same InlineRefCounts bit layout
//   Offset 16: weak_rc     (i64) — weak reference count (direct count)
//
// Encoding: side_table_ptr is 8-byte aligned (malloc guarantees),
// so we shift right 3 to fit in 60 bits, then set USE_SLOW_RC_BIT.
// Decoding: clear bit 63, shift left 3.
const SIDE_TABLE_OBJECT_OFFSET: i32 = 0;
const SIDE_TABLE_REFCOUNT_OFFSET: i32 = 8;
const SIDE_TABLE_WEAK_RC_OFFSET: i32 = 16;
const SIDE_TABLE_SIZE: i64 = 24;
const SIDE_TABLE_ALIGN_SHIFT: i64 = 3;
const USE_SLOW_RC_CLEAR_MASK: i64 = @bitCast(@as(u64, 0x7FFFFFFFFFFFFFFF));

// Metadata struct offsets (when metadata pointer is non-null)
// Reference: Swift TypeMetadata struct — contains destroy function pointer
const METADATA_TYPE_ID_OFFSET: i32 = 0; // type_id (i64)
const METADATA_DESTRUCTOR_OFFSET: i32 = 8; // destructor function pointer (i64)

/// Result of generating a runtime function.
pub const RuntimeFunc = struct {
    name: []const u8,
    compiled: native_compile.CompiledCode,
};

/// Generate all ARC runtime functions as compiled native code.
/// Returns a list of RuntimeFunc (name + compiled code).
///
/// The func_index_map is used to look up indices for external function calls
/// (e.g., malloc, free) so relocations target the correct symbol.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    // Generate each ARC function
    try result.append(allocator, .{
        .name = "alloc",
        .compiled = try generateAlloc(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "dealloc",
        .compiled = try generateDealloc(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "retain",
        .compiled = try generateRetain(allocator, isa, ctrl_plane),
    });
    try result.append(allocator, .{
        .name = "release",
        .compiled = try generateRelease(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "realloc",
        .compiled = try generateRealloc(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "string_concat",
        .compiled = try generateStringConcat(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "string_eq",
        .compiled = try generateStringEq(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "unowned_retain",
        .compiled = try generateUnownedRetain(allocator, isa, ctrl_plane),
    });
    try result.append(allocator, .{
        .name = "unowned_release",
        .compiled = try generateUnownedRelease(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "unowned_load_strong",
        .compiled = try generateUnownedLoadStrong(allocator, isa, ctrl_plane, func_index_map),
    });

    // Weak reference runtime (Phase 3 — side tables)
    try result.append(allocator, .{
        .name = "weak_form_reference",
        .compiled = try generateWeakFormReference(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "weak_retain",
        .compiled = try generateWeakRetain(allocator, isa, ctrl_plane),
    });
    try result.append(allocator, .{
        .name = "weak_release",
        .compiled = try generateWeakRelease(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "weak_load_strong",
        .compiled = try generateWeakLoadStrong(allocator, isa, ctrl_plane),
    });

    return result;
}

// ============================================================================
// alloc(metadata: i64, size: i64) -> i64
//
// Native alloc: call malloc(aligned_size), init header, return ptr + 24.
//
// Header init (24-byte native layout):
//   store alloc_size (i64) at raw + 0
//   store metadata   (i64) at raw + 8
//   store refcount=1 (i64) at raw + 16
//
// Reference: arc.zig:515-683 (Wasm alloc — similar logic with total_size)
// Reference: Swift swift_allocObject (HeapObject.cpp:424-440)
// ============================================================================

fn generateAlloc(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64, i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const metadata = params[0]; // HeapMetadata* (i64 pointer, 0 if no metadata)
    const size = params[1]; // requested user data size

    // alloc_size = (size + HEADER_SIZE + 7) & ~7  (8-byte aligned)
    const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
    const v_with_header = try ins.iadd(size, v_header);
    const v_7 = try ins.iconst(clif.Type.I64, 7);
    const v_unaligned = try ins.iadd(v_with_header, v_7);
    const v_mask = try ins.iconst(clif.Type.I64, -8); // 0xFFFFFFFFFFFFFFF8
    const alloc_size = try ins.band(v_unaligned, v_mask);

    // raw_ptr = malloc(alloc_size)
    const malloc_idx = func_index_map.get("malloc") orelse func_index_map.get("_malloc") orelse 0;
    var malloc_sig = clif.Signature.init(.system_v);
    try malloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try malloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const malloc_sig_ref = try builder.importSignature(malloc_sig);
    const malloc_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = malloc_idx } },
        .signature = malloc_sig_ref,
        .colocated = false, // External — libc
    });
    const malloc_result = try ins.call(malloc_ref, &[_]clif.Value{alloc_size});
    const raw_ptr = malloc_result.results[0];

    // Init header (24-byte native layout):
    //   raw_ptr + 0:  alloc_size (i64) — total allocation size
    //   raw_ptr + 8:  metadata   (i64) — HeapMetadata pointer
    //   raw_ptr + 16: refcount   (i64) — initial count = 1
    _ = try ins.store(clif.MemFlags.DEFAULT, alloc_size, raw_ptr, SIZE_OFFSET);
    _ = try ins.store(clif.MemFlags.DEFAULT, metadata, raw_ptr, METADATA_OFFSET);

    const v_init_rc = try ins.iconst(clif.Type.I64, INITIAL_REFCOUNT);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_init_rc, raw_ptr, REFCOUNT_OFFSET);

    // return raw_ptr + HEADER_SIZE (pointer to user data)
    const user_ptr = try ins.iadd(raw_ptr, v_header);
    _ = try ins.return_(&[_]clif.Value{user_ptr});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// dealloc(obj: i64) -> void
//
// Simplified native dealloc: call free(obj - 24).
// Reference: arc.zig:689-748 (simplified, no freelist).
// ============================================================================

fn generateDealloc(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_free = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const obj = builder.blockParams(block_entry)[0];
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const is_null = try ins.icmp(.eq, obj, v_zero);
    _ = try ins.brif(is_null, block_return, &.{}, block_free, &.{});

    // Return block
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // Free block: header_ptr = obj - 16, call free(header_ptr)
    builder.switchToBlock(block_free);
    try builder.ensureInsertedBlock();
    const ins2 = builder.ins();
    const v_header = try ins2.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
    const header_ptr = try ins2.isub(obj, v_header);

    const free_idx = func_index_map.get("free") orelse func_index_map.get("_free") orelse 0;
    var free_sig = clif.Signature.init(.system_v);
    try free_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const free_sig_ref = try builder.importSignature(free_sig);
    const free_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = free_idx } },
        .signature = free_sig_ref,
        .colocated = false,
    });
    _ = try ins2.call(free_ref, &[_]clif.Value{header_ptr});
    _ = try ins2.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// retain(obj: i64) -> i64
//
// Increment reference count. Returns obj for tail-call optimization.
// Reference: arc.zig:852-903 (Swift _swift_retain_ pattern)
// Reference: HeapObject.cpp:474-489
// ============================================================================

fn generateRetain(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_zero = try builder.createBlock();
    const block_check_immortal = try builder.createBlock();
    const block_return_obj = try builder.createBlock();
    const block_check_slow = try builder.createBlock();
    const block_inline_inc = try builder.createBlock();
    const block_side_table_inc = try builder.createBlock();
    // Shared increment block: takes rc_addr as block param
    const block_do_increment = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return_zero, &.{}, block_check_immortal, &.{});
    }

    // Return zero (null case)
    builder.switchToBlock(block_return_zero);
    try builder.ensureInsertedBlock();
    {
        const v_zero2 = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().return_(&[_]clif.Value{v_zero2});
    }

    // Check immortal: header_ptr = obj - 24, load refcount
    builder.switchToBlock(block_check_immortal);
    try builder.ensureInsertedBlock();
    {
        const ins3 = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins3.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins3.isub(obj, v_header);
        const refcount = try ins3.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);
        const v_immortal = try ins3.iconst(clif.Type.I64, IMMORTAL_REFCOUNT);
        const is_immortal = try ins3.icmp(.eq, refcount, v_immortal);
        _ = try ins3.brif(is_immortal, block_return_obj, &.{}, block_check_slow, &[_]clif.Value{refcount});
    }

    // Return obj (immortal case)
    builder.switchToBlock(block_return_obj);
    try builder.ensureInsertedBlock();
    {
        const obj = builder.blockParams(block_entry)[0];
        _ = try builder.ins().return_(&[_]clif.Value{obj});
    }

    // Check UseSlowRC (bit 63)
    _ = try builder.appendBlockParam(block_check_slow, clif.Type.I64); // rc_word
    builder.switchToBlock(block_check_slow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_check_slow)[0];
        const v_63 = try ins.iconst(clif.Type.I64, 63);
        const shifted = try ins.ushr(rc_word, v_63);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const use_slow = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_slow = try ins.icmp(.ne, use_slow, v_zero);
        _ = try ins.brif(is_slow, block_side_table_inc, &[_]clif.Value{rc_word}, block_inline_inc, &.{});
    }

    // Inline increment: rc_addr = header_ptr + REFCOUNT_OFFSET
    builder.switchToBlock(block_inline_inc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(header_ptr, v_rc_off);
        _ = try ins.jump(block_do_increment, &[_]clif.Value{rc_addr});
    }

    // Side table increment: decode st_ptr, rc_addr = st_ptr + 8
    _ = try builder.appendBlockParam(block_side_table_inc, clif.Type.I64); // rc_word
    builder.switchToBlock(block_side_table_inc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_side_table_inc)[0];
        const v_clear = try ins.iconst(clif.Type.I64, USE_SLOW_RC_CLEAR_MASK);
        const cleared = try ins.band(rc_word, v_clear);
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const st_ptr = try ins.ishl(cleared, v_shift);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, SIDE_TABLE_REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(st_ptr, v_rc_off);
        _ = try ins.jump(block_do_increment, &[_]clif.Value{rc_addr});
    }

    // Shared increment: load from rc_addr, add STRONG_RC_ONE, store back
    _ = try builder.appendBlockParam(block_do_increment, clif.Type.I64); // rc_addr
    builder.switchToBlock(block_do_increment);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const rc_addr = builder.blockParams(block_do_increment)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);
        const v_strong_one = try ins.iconst(clif.Type.I64, STRONG_RC_ONE);
        const new_rc = try ins.iadd(refcount, v_strong_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_rc, rc_addr, 0);
        _ = try ins.return_(&[_]clif.Value{obj});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// release(obj: i64) -> void
//
// Decrement reference count. If zero, dispatch destructor then dealloc.
//
// Swift _swift_release_dealloc pattern (HeapObject.cpp:835-837):
//   if (object->refCounts.decrementShouldDeallocate()) {
//     auto metadata = object->metadata;
//     metadata->destroy(object);
//   }
//
// Native flow (simplified — metadata IS the destructor function pointer):
//   1. if obj == 0: return
//   2. header = obj - 24
//   3. rc = load_i64(header + 16)
//   4. if rc >= IMMORTAL: return
//   5. rc -= 1; store_i64(header + 16, rc)
//   6. if rc != 0: return
//   7. destructor = load_i64(header + 8)   // destructor function pointer (0 if none)
//   8. if destructor == 0: goto dealloc
//   9. call_indirect(destructor, obj)       // destroy(obj)
//  10. dealloc: call dealloc(obj)
//
// Note: In the Wasm path, metadata points to a struct with type_id+destructor_idx.
// In the native path, metadata stores the destructor function pointer directly
// (set by ssa_to_clif.zig's metadata_addr handler using funcAddr).
//
// Reference: arc.zig:912-998 (Wasm release with destructor dispatch)
// Reference: swift/stdlib/public/runtime/HeapObject.cpp:548-552, 835-837
// ============================================================================

fn generateRelease(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_check_immortal = try builder.createBlock();
    const block_check_slow = try builder.createBlock();
    const block_inline_path = try builder.createBlock();
    const block_side_table_path = try builder.createBlock();
    const block_decrement = try builder.createBlock();
    const block_store_and_return = try builder.createBlock();
    const block_check_destructor = try builder.createBlock();
    const block_call_destructor = try builder.createBlock();
    const block_dealloc = try builder.createBlock();

    // ---- Entry: null check ----
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_check_immortal, &.{});
    }

    // ---- Return block (null, immortal, or rc > 0) ----
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // ---- Check immortal: load refcount, skip if == IMMORTAL ----
    builder.switchToBlock(block_check_immortal);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);
        const v_immortal = try ins.iconst(clif.Type.I64, IMMORTAL_REFCOUNT);
        const is_immortal = try ins.icmp(.eq, refcount, v_immortal);
        _ = try ins.brif(is_immortal, block_return, &.{}, block_check_slow, &[_]clif.Value{refcount});
    }

    // ---- Check UseSlowRC (bit 63) ----
    _ = try builder.appendBlockParam(block_check_slow, clif.Type.I64); // rc_word
    builder.switchToBlock(block_check_slow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_check_slow)[0];
        const v_63 = try ins.iconst(clif.Type.I64, 63);
        const shifted = try ins.ushr(rc_word, v_63);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const use_slow = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_slow = try ins.icmp(.ne, use_slow, v_zero);
        _ = try ins.brif(is_slow, block_side_table_path, &[_]clif.Value{rc_word}, block_inline_path, &.{});
    }

    // ---- Inline path: rc_addr = header_ptr + REFCOUNT_OFFSET ----
    builder.switchToBlock(block_inline_path);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(header_ptr, v_rc_off);
        _ = try ins.jump(block_decrement, &[_]clif.Value{rc_addr});
    }

    // ---- Side table path: decode, rc_addr = side_table + 8 ----
    _ = try builder.appendBlockParam(block_side_table_path, clif.Type.I64); // rc_word
    builder.switchToBlock(block_side_table_path);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_side_table_path)[0];
        const v_clear = try ins.iconst(clif.Type.I64, USE_SLOW_RC_CLEAR_MASK);
        const cleared = try ins.band(rc_word, v_clear);
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const st_ptr = try ins.ishl(cleared, v_shift);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, SIDE_TABLE_REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(st_ptr, v_rc_off);
        _ = try ins.jump(block_decrement, &[_]clif.Value{rc_addr});
    }

    // ---- Decrement: load from rc_addr, check StrongExtra ----
    // Swift doDecrementSlow pattern (RefCount.h:1040-1048)
    _ = try builder.appendBlockParam(block_decrement, clif.Type.I64); // rc_addr
    builder.switchToBlock(block_decrement);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_addr = builder.blockParams(block_decrement)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);

        // Extract StrongExtra from original: (rc >> 33) & 0x3FFFFFFF
        const v_shift = try ins.iconst(clif.Type.I64, @as(i64, STRONG_EXTRA_SHIFT));
        const shifted = try ins.ushr(refcount, v_shift);
        const v_mask = try ins.iconst(clif.Type.I64, 0x3FFFFFFF);
        const strong_extra = try ins.band(shifted, v_mask);

        // If StrongExtra was 0 → this was the last strong reference
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const was_last = try ins.icmp(.eq, strong_extra, v_zero);
        _ = try ins.brif(was_last, block_check_destructor, &[_]clif.Value{rc_addr}, block_store_and_return, &[_]clif.Value{rc_addr});
    }

    // ---- Store decremented rc and return (not last strong ref) ----
    _ = try builder.appendBlockParam(block_store_and_return, clif.Type.I64); // rc_addr
    builder.switchToBlock(block_store_and_return);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_addr = builder.blockParams(block_store_and_return)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);
        const v_strong_one = try ins.iconst(clif.Type.I64, STRONG_RC_ONE);
        const new_rc = try ins.isub(refcount, v_strong_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_rc, rc_addr, 0);
        _ = try ins.return_(&[_]clif.Value{});
    }

    // ---- Check destructor: set IsDeiniting, check metadata ----
    // Destructor metadata is ALWAYS in the object header (never in side table).
    // IsDeiniting is written to rc_addr (may be side table or inline).
    _ = try builder.appendBlockParam(block_check_destructor, clif.Type.I64); // rc_addr
    builder.switchToBlock(block_check_destructor);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const rc_addr = builder.blockParams(block_check_destructor)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);

        // Clear StrongExtra (bits 33-62) and UseSlowRC (bit 63), keep low 33 bits
        const v_clear_mask = try ins.iconst(clif.Type.I64, @bitCast(~@as(u64, @bitCast(@as(i64, STRONG_EXTRA_MASK) | USE_SLOW_RC_BIT))));
        const clean_rc = try ins.band(refcount, v_clear_mask);

        // Set IsDeiniting flag
        const v_deiniting = try ins.iconst(clif.Type.I64, IS_DEINITING_BIT);
        const rc_with_deiniting = try ins.bor(clean_rc, v_deiniting);
        _ = try ins.store(clif.MemFlags.DEFAULT, rc_with_deiniting, rc_addr, 0);

        // Load destructor function pointer from object header metadata field
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const destructor_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, METADATA_OFFSET);

        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const has_destructor = try ins.icmp(.ne, destructor_ptr, v_zero);
        _ = try ins.brif(has_destructor, block_call_destructor, &[_]clif.Value{destructor_ptr}, block_dealloc, &.{});
    }

    // ---- Call destructor: call_indirect(destructor_ptr, obj) ----
    _ = try builder.appendBlockParam(block_call_destructor, clif.Type.I64); // destructor_ptr
    builder.switchToBlock(block_call_destructor);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const destructor_ptr = builder.blockParams(block_call_destructor)[0];

        var dtor_sig = clif.Signature.init(.system_v);
        try dtor_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const dtor_sig_ref = try builder.importSignature(dtor_sig);
        _ = try ins.callIndirect(dtor_sig_ref, destructor_ptr, &[_]clif.Value{obj});

        _ = try ins.jump(block_dealloc, &.{});
    }

    // ---- Dealloc block: call unowned_release(obj) ----
    builder.switchToBlock(block_dealloc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const unowned_release_idx = func_index_map.get("unowned_release") orelse 0;
        var sig = clif.Signature.init(.system_v);
        try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const sig_ref = try builder.importSignature(sig);
        const func_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = unowned_release_idx } },
            .signature = sig_ref,
            .colocated = true,
        });
        _ = try ins.call(func_ref, &[_]clif.Value{obj});
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// realloc(obj: i64, new_size: i64) -> i64
//
// If obj is null, calls alloc(0, new_size).
// If new allocation fits in old allocation, updates size and returns obj.
// Otherwise: alloc new, memcpy old data, dealloc old, return new.
//
// Reference: arc.zig:750-848 (Wasm realloc)
// ============================================================================

fn generateRealloc(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (obj: i64, new_size: i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_null = try builder.createBlock();
    const block_check_fit = try builder.createBlock();
    const block_fits = try builder.createBlock();
    const block_grow = try builder.createBlock();

    // --- Entry: null check ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_null, &.{}, block_check_fit, &.{});
    }

    // --- Null block: return alloc(0, new_size) ---
    builder.switchToBlock(block_null);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const new_size = builder.blockParams(block_entry)[1];
        const alloc_idx = func_index_map.get("alloc") orelse 0;
        var alloc_sig = clif.Signature.init(.system_v);
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const asig_ref = try builder.importSignature(alloc_sig);
        const aref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = alloc_idx } },
            .signature = asig_ref,
            .colocated = true,
        });
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const result = try ins.call(aref, &[_]clif.Value{ v_zero, new_size });
        _ = try ins.return_(&[_]clif.Value{result.results[0]});
    }

    // --- Check fit: compute new_total, compare with old alloc_size ---
    builder.switchToBlock(block_check_fit);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const new_size = builder.blockParams(block_entry)[1];

        // header_ptr = obj - HEADER_SIZE
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);

        // old_alloc_size = load i64 from header + SIZE_OFFSET
        const old_alloc_size = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, SIZE_OFFSET);

        // new_total = (new_size + HEADER_SIZE + 7) & ~7
        const v_with_header = try ins.iadd(new_size, v_header);
        const v_7 = try ins.iconst(clif.Type.I64, 7);
        const v_unaligned = try ins.iadd(v_with_header, v_7);
        const v_mask = try ins.iconst(clif.Type.I64, -8);
        const new_total = try ins.band(v_unaligned, v_mask);

        // if new_total <= old_alloc_size → fits in place
        const fits = try ins.icmp(.ule, new_total, old_alloc_size);
        _ = try ins.brif(fits, block_fits, &[_]clif.Value{new_total}, block_grow, &.{});
    }

    // --- Fits: update alloc_size, return obj ---
    _ = try builder.appendBlockParam(block_fits, clif.Type.I64); // new_total
    builder.switchToBlock(block_fits);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const new_total = builder.blockParams(block_fits)[0];

        // Store new alloc_size
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_total, header_ptr, SIZE_OFFSET);
        _ = try ins.return_(&[_]clif.Value{obj});
    }

    // --- Grow: alloc new, memcpy old user data, dealloc old ---
    builder.switchToBlock(block_grow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const new_size = builder.blockParams(block_entry)[1];

        // Load old metadata to preserve in new allocation
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const old_metadata = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, METADATA_OFFSET);

        // new_obj = alloc(old_metadata, new_size)
        const alloc_idx = func_index_map.get("alloc") orelse 0;
        var alloc_sig = clif.Signature.init(.system_v);
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const asig_ref = try builder.importSignature(alloc_sig);
        const aref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = alloc_idx } },
            .signature = asig_ref,
            .colocated = true,
        });
        const alloc_result = try ins.call(aref, &[_]clif.Value{ old_metadata, new_size });
        const new_obj = alloc_result.results[0];

        // old payload size = old_alloc_size - HEADER_SIZE
        const old_alloc_size = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, SIZE_OFFSET);
        const copy_size = try ins.isub(old_alloc_size, v_header);

        // memcpy(new_obj, obj, copy_size)
        const memcpy_idx = func_index_map.get("memcpy") orelse 0;
        var memcpy_sig = clif.Signature.init(.system_v);
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const msig_ref = try builder.importSignature(memcpy_sig);
        const mref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = memcpy_idx } },
            .signature = msig_ref,
            .colocated = false, // libc memcpy
        });
        _ = try ins.call(mref, &[_]clif.Value{ new_obj, obj, copy_size });

        // dealloc(obj) — free old allocation
        const dealloc_idx = func_index_map.get("dealloc") orelse 0;
        var dealloc_sig = clif.Signature.init(.system_v);
        try dealloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const dsig_ref = try builder.importSignature(dealloc_sig);
        const dref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = dealloc_idx } },
            .signature = dsig_ref,
            .colocated = true,
        });
        _ = try ins.call(dref, &[_]clif.Value{obj});

        // return new_obj
        _ = try ins.return_(&[_]clif.Value{new_obj});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// string_concat(s1_ptr: i64, s1_len: i64, s2_ptr: i64, s2_len: i64) -> i64
//
// Allocates via alloc(0, s1_len + s2_len), copies both strings, returns ptr.
// Reference: arc.zig:1100-1174 (Go runtime/string.go concatstrings)
// ============================================================================

fn generateStringConcat(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64, i64, i64, i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_empty = try builder.createBlock();
    const block_alloc = try builder.createBlock();

    // --- Entry ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const s1_len = params[1];
        const s2_len = params[3];

        // new_len = s1_len + s2_len
        const new_len = try ins.iadd(s1_len, s2_len);

        // if new_len == 0, return 0
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_empty = try ins.icmp(.eq, new_len, v_zero);
        _ = try ins.brif(is_empty, block_empty, &.{}, block_alloc, &[_]clif.Value{new_len});
    }

    // --- Empty: return 0 ---
    builder.switchToBlock(block_empty);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.return_(&[_]clif.Value{v_zero});
    }

    // --- Alloc + copy ---
    _ = try builder.appendBlockParam(block_alloc, clif.Type.I64); // new_len
    builder.switchToBlock(block_alloc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const s1_ptr = params[0];
        const s1_len = params[1];
        const s2_ptr = params[2];
        const s2_len = params[3];
        const new_len = builder.blockParams(block_alloc)[0];

        // new_ptr = alloc(0, new_len)
        const alloc_idx = func_index_map.get("alloc") orelse 0;
        var alloc_sig = clif.Signature.init(.system_v);
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try alloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const asig_ref = try builder.importSignature(alloc_sig);
        const aref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = alloc_idx } },
            .signature = asig_ref,
            .colocated = true,
        });
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const alloc_result = try ins.call(aref, &[_]clif.Value{ v_zero, new_len });
        const new_ptr = alloc_result.results[0];

        // memcpy(new_ptr, s1_ptr, s1_len)
        const memcpy_idx = func_index_map.get("memcpy") orelse 0;
        var memcpy_sig = clif.Signature.init(.system_v);
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcpy_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const msig_ref = try builder.importSignature(memcpy_sig);
        const mref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = memcpy_idx } },
            .signature = msig_ref,
            .colocated = false,
        });
        _ = try ins.call(mref, &[_]clif.Value{ new_ptr, s1_ptr, s1_len });

        // memcpy(new_ptr + s1_len, s2_ptr, s2_len)
        const dest2 = try ins.iadd(new_ptr, s1_len);
        _ = try ins.call(mref, &[_]clif.Value{ dest2, s2_ptr, s2_len });

        _ = try ins.return_(&[_]clif.Value{new_ptr});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// string_eq(s1_ptr: i64, s1_len: i64, s2_ptr: i64, s2_len: i64) -> i64
//
// Returns 1 if equal, 0 if not.
// Reference: arc.zig:1001-1098 (Go runtime/string.go stringEqual)
// Native: use libc memcmp for efficient comparison.
// ============================================================================

fn generateStringEq(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64, i64, i64, i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_0 = try builder.createBlock();
    const block_return_1 = try builder.createBlock();
    const block_len_eq = try builder.createBlock();
    const block_compare = try builder.createBlock();

    // --- Entry: check lengths ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const s1_len = params[1];
        const s2_len = params[3];

        const len_eq = try ins.icmp(.eq, s1_len, s2_len);
        _ = try ins.brif(len_eq, block_len_eq, &.{}, block_return_0, &.{});
    }

    // --- Return 0 ---
    builder.switchToBlock(block_return_0);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.return_(&[_]clif.Value{v_zero});
    }

    // --- Return 1 ---
    builder.switchToBlock(block_return_1);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_one = try ins.iconst(clif.Type.I64, 1);
        _ = try ins.return_(&[_]clif.Value{v_one});
    }

    // --- Lengths equal: check pointer equality ---
    builder.switchToBlock(block_len_eq);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const s1_ptr = params[0];
        const s2_ptr = params[2];

        const ptr_eq = try ins.icmp(.eq, s1_ptr, s2_ptr);
        _ = try ins.brif(ptr_eq, block_return_1, &.{}, block_compare, &.{});
    }

    // --- Compare bytes using memcmp ---
    builder.switchToBlock(block_compare);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const s1_ptr = params[0];
        const s1_len = params[1];
        const s2_ptr = params[2];

        // result = memcmp(s1_ptr, s2_ptr, s1_len)
        const memcmp_idx = func_index_map.get("memcmp") orelse 0;
        var memcmp_sig = clif.Signature.init(.system_v);
        try memcmp_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcmp_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcmp_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try memcmp_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I32));
        const csig_ref = try builder.importSignature(memcmp_sig);
        const cref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = memcmp_idx } },
            .signature = csig_ref,
            .colocated = false,
        });
        const cmp_result = try ins.call(cref, &[_]clif.Value{ s1_ptr, s2_ptr, s1_len });
        const cmp_i32 = cmp_result.results[0];

        // Branch on memcmp result: 0 → equal, non-zero → not equal
        const v_zero_i32 = try ins.iconst(clif.Type.I32, 0);
        const is_eq = try ins.icmp(.eq, cmp_i32, v_zero_i32);
        _ = try ins.brif(is_eq, block_return_1, &.{}, block_return_0, &.{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// unowned_retain(obj: i64) -> void
//
// Increment unowned reference count (bits 1-31).
// Called when binding an `unowned var`.
// Reference: Swift swift_unownedRetain (HeapObject.cpp:596-600)
// ============================================================================

fn generateUnownedRetain(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_check_immortal = try builder.createBlock();
    const block_check_slow = try builder.createBlock();
    const block_inline_inc = try builder.createBlock();
    const block_side_table_inc = try builder.createBlock();
    const block_do_increment = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_check_immortal, &.{});
    }

    // Return block
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // Check immortal
    builder.switchToBlock(block_check_immortal);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);
        const v_immortal = try ins.iconst(clif.Type.I64, IMMORTAL_REFCOUNT);
        const is_immortal = try ins.icmp(.eq, refcount, v_immortal);
        _ = try ins.brif(is_immortal, block_return, &.{}, block_check_slow, &[_]clif.Value{refcount});
    }

    // Check UseSlowRC
    _ = try builder.appendBlockParam(block_check_slow, clif.Type.I64); // rc_word
    builder.switchToBlock(block_check_slow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_check_slow)[0];
        const v_63 = try ins.iconst(clif.Type.I64, 63);
        const shifted = try ins.ushr(rc_word, v_63);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const use_slow = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_slow = try ins.icmp(.ne, use_slow, v_zero);
        _ = try ins.brif(is_slow, block_side_table_inc, &[_]clif.Value{rc_word}, block_inline_inc, &.{});
    }

    // Inline path: rc_addr = header_ptr + REFCOUNT_OFFSET
    builder.switchToBlock(block_inline_inc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(header_ptr, v_rc_off);
        _ = try ins.jump(block_do_increment, &[_]clif.Value{rc_addr});
    }

    // Side table path: decode, rc_addr = side_table + 8
    _ = try builder.appendBlockParam(block_side_table_inc, clif.Type.I64); // rc_word
    builder.switchToBlock(block_side_table_inc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_side_table_inc)[0];
        const v_clear = try ins.iconst(clif.Type.I64, USE_SLOW_RC_CLEAR_MASK);
        const cleared = try ins.band(rc_word, v_clear);
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const st_ptr = try ins.ishl(cleared, v_shift);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, SIDE_TABLE_REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(st_ptr, v_rc_off);
        _ = try ins.jump(block_do_increment, &[_]clif.Value{rc_addr});
    }

    // Shared increment: load from rc_addr, add UNOWNED_RC_ONE, store back
    _ = try builder.appendBlockParam(block_do_increment, clif.Type.I64); // rc_addr
    builder.switchToBlock(block_do_increment);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_addr = builder.blockParams(block_do_increment)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);
        const v_unowned_one = try ins.iconst(clif.Type.I64, UNOWNED_RC_ONE);
        const new_rc = try ins.iadd(refcount, v_unowned_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_rc, rc_addr, 0);
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// unowned_release(obj: i64) -> void
//
// Decrement unowned reference count (bits 1-31). If unowned hits 0, dealloc.
// Called at scope exit for `unowned var`, and by release() after deinit.
// Reference: Swift swift_unownedRelease (HeapObject.cpp:608-620)
// ============================================================================

fn generateUnownedRelease(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_check_immortal = try builder.createBlock();
    const block_check_slow = try builder.createBlock();
    const block_inline_path = try builder.createBlock();
    const block_side_table_path = try builder.createBlock();
    const block_decrement = try builder.createBlock();
    const block_dealloc = try builder.createBlock();
    // Side table cleanup: dealloc obj, zero object_ptr, maybe free side table
    const block_st_dealloc = try builder.createBlock();
    const block_st_free = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_check_immortal, &.{});
    }

    // Return block
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // Check immortal
    builder.switchToBlock(block_check_immortal);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);
        const v_immortal = try ins.iconst(clif.Type.I64, IMMORTAL_REFCOUNT);
        const is_immortal = try ins.icmp(.eq, refcount, v_immortal);
        _ = try ins.brif(is_immortal, block_return, &.{}, block_check_slow, &[_]clif.Value{refcount});
    }

    // Check UseSlowRC
    _ = try builder.appendBlockParam(block_check_slow, clif.Type.I64); // rc_word
    builder.switchToBlock(block_check_slow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_check_slow)[0];
        const v_63 = try ins.iconst(clif.Type.I64, 63);
        const shifted = try ins.ushr(rc_word, v_63);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const use_slow = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_slow = try ins.icmp(.ne, use_slow, v_zero);
        _ = try ins.brif(is_slow, block_side_table_path, &[_]clif.Value{rc_word}, block_inline_path, &.{});
    }

    // Inline path: rc_addr = header_ptr + REFCOUNT_OFFSET
    builder.switchToBlock(block_inline_path);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(header_ptr, v_rc_off);
        // Pass 0 as st_ptr (no side table in inline path)
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.jump(block_decrement, &[_]clif.Value{ rc_addr, v_zero });
    }

    // Side table path: decode, rc_addr = side_table + 8
    _ = try builder.appendBlockParam(block_side_table_path, clif.Type.I64); // rc_word
    builder.switchToBlock(block_side_table_path);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_side_table_path)[0];
        const v_clear = try ins.iconst(clif.Type.I64, USE_SLOW_RC_CLEAR_MASK);
        const cleared = try ins.band(rc_word, v_clear);
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const st_ptr = try ins.ishl(cleared, v_shift);
        const v_rc_off = try ins.iconst(clif.Type.I64, @as(i64, SIDE_TABLE_REFCOUNT_OFFSET));
        const rc_addr = try ins.iadd(st_ptr, v_rc_off);
        _ = try ins.jump(block_decrement, &[_]clif.Value{ rc_addr, st_ptr });
    }

    // Decrement unowned: subtract, extract NEW count, check if zero
    _ = try builder.appendBlockParam(block_decrement, clif.Type.I64); // rc_addr
    _ = try builder.appendBlockParam(block_decrement, clif.Type.I64); // st_ptr (0 if inline)
    builder.switchToBlock(block_decrement);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_addr = builder.blockParams(block_decrement)[0];
        const st_ptr = builder.blockParams(block_decrement)[1];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, rc_addr, 0);

        // Subtract UNOWNED_RC_ONE
        const v_unowned_one = try ins.iconst(clif.Type.I64, UNOWNED_RC_ONE);
        const new_rc = try ins.isub(refcount, v_unowned_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_rc, rc_addr, 0);

        // Extract NEW UnownedRefCount: (new_rc >> 1) & 0x7FFFFFFF
        const v_one_shift = try ins.iconst(clif.Type.I64, 1);
        const shifted = try ins.ushr(new_rc, v_one_shift);
        const v_mask = try ins.iconst(clif.Type.I64, 0x7FFFFFFF);
        const new_unowned = try ins.band(shifted, v_mask);

        // If unowned hits 0 → free memory (dealloc handles side table check)
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_last = try ins.icmp(.eq, new_unowned, v_zero);
        _ = try ins.brif(is_last, block_dealloc, &[_]clif.Value{st_ptr}, block_return, &.{});
    }

    // Dealloc: dealloc(obj), then check if side table cleanup needed
    _ = try builder.appendBlockParam(block_dealloc, clif.Type.I64); // st_ptr
    builder.switchToBlock(block_dealloc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const st_ptr = builder.blockParams(block_dealloc)[0];
        const dealloc_idx = func_index_map.get("dealloc") orelse 0;
        var dealloc_sig = clif.Signature.init(.system_v);
        try dealloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const dealloc_sig_ref = try builder.importSignature(dealloc_sig);
        const dealloc_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = dealloc_idx } },
            .signature = dealloc_sig_ref,
            .colocated = true,
        });
        _ = try ins.call(dealloc_ref, &[_]clif.Value{obj});

        // If side table exists, do side table cleanup
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const has_st = try ins.icmp(.ne, st_ptr, v_zero);
        _ = try ins.brif(has_st, block_st_dealloc, &[_]clif.Value{st_ptr}, block_return, &.{});
    }

    // Side table cleanup: zero object_ptr, check weak_rc
    _ = try builder.appendBlockParam(block_st_dealloc, clif.Type.I64); // st_ptr
    builder.switchToBlock(block_st_dealloc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_st_dealloc)[0];

        // Set object_ptr = 0 (signals "object freed" to weak_load_strong)
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, v_zero, st_ptr, SIDE_TABLE_OBJECT_OFFSET);

        // Load weak_rc, check if 0 → can free side table
        const weak_rc = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);
        const weak_zero = try ins.icmp(.eq, weak_rc, v_zero);
        _ = try ins.brif(weak_zero, block_st_free, &[_]clif.Value{st_ptr}, block_return, &.{});
    }

    // Free side table (no more references of any kind)
    _ = try builder.appendBlockParam(block_st_free, clif.Type.I64); // st_ptr
    builder.switchToBlock(block_st_free);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_st_free)[0];
        const free_idx = func_index_map.get("free") orelse func_index_map.get("_free") orelse 0;
        var free_sig = clif.Signature.init(.system_v);
        try free_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const fsig_ref = try builder.importSignature(free_sig);
        const fref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = free_idx } },
            .signature = fsig_ref,
            .colocated = false,
        });
        _ = try ins.call(fref, &[_]clif.Value{st_ptr});
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// unowned_load_strong(obj: i64) -> i64
//
// Check if object is still alive (IsDeiniting not set), then retain and return.
// If deiniting, trap (fatal error — dangling unowned reference).
// Reference: Swift swift_unownedRetainStrong (HeapObject.cpp:665-680)
// ============================================================================

fn generateUnownedLoadStrong(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_null = try builder.createBlock();
    const block_check = try builder.createBlock();
    const block_trap = try builder.createBlock();
    const block_retain = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return_null, &.{}, block_check, &.{});
    }

    // Return null
    builder.switchToBlock(block_return_null);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.return_(&[_]clif.Value{v_zero});
    }

    // Check IsDeiniting
    builder.switchToBlock(block_check);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);

        // Extract IsDeiniting: (rc >> 32) & 1
        const v_32 = try ins.iconst(clif.Type.I64, 32);
        const shifted = try ins.ushr(refcount, v_32);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const is_deiniting_val = try ins.band(shifted, v_one);

        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_deiniting = try ins.icmp(.ne, is_deiniting_val, v_zero);
        _ = try ins.brif(is_deiniting, block_trap, &.{}, block_retain, &.{});
    }

    // Trap: dangling unowned reference
    builder.switchToBlock(block_trap);
    try builder.ensureInsertedBlock();
    {
        _ = try builder.ins().trap(.user1);
    }

    // Retain and return
    builder.switchToBlock(block_retain);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const retain_idx = func_index_map.get("retain") orelse 0;
        var retain_sig = clif.Signature.init(.system_v);
        try retain_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try retain_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const sig_ref = try builder.importSignature(retain_sig);
        const func_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = retain_idx } },
            .signature = sig_ref,
            .colocated = true,
        });
        const result = try ins.call(func_ref, &[_]clif.Value{obj});
        _ = try ins.return_(&[_]clif.Value{result.results[0]});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// weak_form_reference(obj: i64) -> i64  (side_table_ptr)
//
// Lazily allocate a side table for the object. If UseSlowRC is already set,
// the side table exists — just increment its weak_rc. Otherwise, allocate a
// 24-byte side table, copy refcounts, set UseSlowRC in object header.
//
// Reference: Swift allocateSideTable (HeapObject.cpp:782-812)
// Reference: Swift swift_weakInit (WeakReference.h)
// ============================================================================

fn generateWeakFormReference(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (obj: i64) -> i64 (side_table_ptr)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_null = try builder.createBlock();
    const block_load_rc = try builder.createBlock();
    const block_existing_st = try builder.createBlock();
    const block_check_deiniting = try builder.createBlock();
    const block_alloc_st = try builder.createBlock();

    // ---- Entry: null check ----
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, obj, v_zero);
        _ = try ins.brif(is_null, block_return_null, &.{}, block_load_rc, &.{});
    }

    // ---- Return null (obj was null, or deiniting) ----
    builder.switchToBlock(block_return_null);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().return_(&[_]clif.Value{v_zero});
    }

    // ---- Load rc_word, check UseSlowRC ----
    builder.switchToBlock(block_load_rc);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        const rc_word = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, header_ptr, REFCOUNT_OFFSET);

        // Check UseSlowRC (bit 63): (rc_word >> 63) & 1
        const v_63 = try ins.iconst(clif.Type.I64, 63);
        const shifted = try ins.ushr(rc_word, v_63);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const use_slow = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_slow = try ins.icmp(.ne, use_slow, v_zero);
        _ = try ins.brif(is_slow, block_existing_st, &[_]clif.Value{rc_word}, block_check_deiniting, &[_]clif.Value{rc_word});
    }

    // ---- Check IsDeiniting before allocating (Swift allocateSideTable(failIfDeiniting=true)) ----
    // Reference: RefCount.cpp:32-34 — "Already past the start of deinit. Do nothing."
    _ = try builder.appendBlockParam(block_check_deiniting, clif.Type.I64); // rc_word
    builder.switchToBlock(block_check_deiniting);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_check_deiniting)[0];
        // Extract IsDeiniting: (rc_word >> 32) & 1
        const v_32 = try ins.iconst(clif.Type.I64, 32);
        const shifted = try ins.ushr(rc_word, v_32);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const is_deiniting_val = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_deiniting = try ins.icmp(.ne, is_deiniting_val, v_zero);
        _ = try ins.brif(is_deiniting, block_return_null, &.{}, block_alloc_st, &[_]clif.Value{rc_word});
    }

    // ---- Existing side table: decode, increment weak_rc, return ----
    _ = try builder.appendBlockParam(block_existing_st, clif.Type.I64); // rc_word
    builder.switchToBlock(block_existing_st);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const rc_word = builder.blockParams(block_existing_st)[0];

        // Decode: side_table_ptr = (rc_word & 0x7FFF...) << 3
        const v_clear = try ins.iconst(clif.Type.I64, USE_SLOW_RC_CLEAR_MASK);
        const cleared = try ins.band(rc_word, v_clear);
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const st_ptr = try ins.ishl(cleared, v_shift);

        // Increment weak_rc
        const weak_rc = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_weak_rc = try ins.iadd(weak_rc, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_weak_rc, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);

        _ = try ins.return_(&[_]clif.Value{st_ptr});
    }

    // ---- Allocate new side table ----
    _ = try builder.appendBlockParam(block_alloc_st, clif.Type.I64); // rc_word
    builder.switchToBlock(block_alloc_st);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const obj = builder.blockParams(block_entry)[0];
        const rc_word = builder.blockParams(block_alloc_st)[0];

        // malloc(24) for side table
        const malloc_idx = func_index_map.get("malloc") orelse 0;
        var malloc_sig = clif.Signature.init(.system_v);
        try malloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try malloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const msig_ref = try builder.importSignature(malloc_sig);
        const mref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = malloc_idx } },
            .signature = msig_ref,
            .colocated = false,
        });
        const v_size = try ins.iconst(clif.Type.I64, SIDE_TABLE_SIZE);
        const malloc_result = try ins.call(mref, &[_]clif.Value{v_size});
        const st_ptr = malloc_result.results[0];

        // Init side table: { obj, rc_word, 1 }
        _ = try ins.store(clif.MemFlags.DEFAULT, obj, st_ptr, SIDE_TABLE_OBJECT_OFFSET);
        _ = try ins.store(clif.MemFlags.DEFAULT, rc_word, st_ptr, SIDE_TABLE_REFCOUNT_OFFSET);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        _ = try ins.store(clif.MemFlags.DEFAULT, v_one, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);

        // Encode side table pointer in object header:
        // encoded = (st_ptr >> 3) | USE_SLOW_RC_BIT
        const v_shift = try ins.iconst(clif.Type.I64, SIDE_TABLE_ALIGN_SHIFT);
        const shifted_ptr = try ins.ushr(st_ptr, v_shift);
        const v_use_slow = try ins.iconst(clif.Type.I64, USE_SLOW_RC_BIT);
        const encoded = try ins.bor(shifted_ptr, v_use_slow);

        // Store encoded value in object's refcount field
        const v_header = try ins.iconst(clif.Type.I64, HEAP_OBJECT_HEADER_SIZE);
        const header_ptr = try ins.isub(obj, v_header);
        _ = try ins.store(clif.MemFlags.DEFAULT, encoded, header_ptr, REFCOUNT_OFFSET);

        _ = try ins.return_(&[_]clif.Value{st_ptr});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// weak_retain(side_table_ptr: i64) -> void
//
// Increment weak reference count in side table.
// Reference: Swift swift_weakRetain (HeapObject.cpp)
// ============================================================================

fn generateWeakRetain(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_increment = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, st_ptr, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_increment, &.{});
    }

    // Return
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // Increment weak_rc
    builder.switchToBlock(block_increment);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const weak_rc = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_weak_rc = try ins.iadd(weak_rc, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_weak_rc, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// weak_release(side_table_ptr: i64) -> void
//
// Decrement weak reference count. If weak_rc hits 0 AND object_ptr is 0
// (object already freed), free the side table.
// Reference: Swift swift_weakRelease (HeapObject.cpp)
// ============================================================================

fn generateWeakRelease(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_decrement = try builder.createBlock();
    const block_check_free = try builder.createBlock();
    const block_free_st = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, st_ptr, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_decrement, &.{});
    }

    // Return
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // Decrement weak_rc, check if zero
    builder.switchToBlock(block_decrement);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const weak_rc = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_weak_rc = try ins.isub(weak_rc, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_weak_rc, st_ptr, SIDE_TABLE_WEAK_RC_OFFSET);

        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_last = try ins.icmp(.eq, new_weak_rc, v_zero);
        _ = try ins.brif(is_last, block_check_free, &.{}, block_return, &.{});
    }

    // Check if object_ptr == 0 (object already freed) → free side table
    builder.switchToBlock(block_check_free);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const obj_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_OBJECT_OFFSET);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const obj_freed = try ins.icmp(.eq, obj_ptr, v_zero);
        _ = try ins.brif(obj_freed, block_free_st, &.{}, block_return, &.{});
    }

    // Free side table
    builder.switchToBlock(block_free_st);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const free_idx = func_index_map.get("free") orelse func_index_map.get("_free") orelse 0;
        var free_sig = clif.Signature.init(.system_v);
        try free_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const fsig_ref = try builder.importSignature(free_sig);
        const fref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = free_idx } },
            .signature = fsig_ref,
            .colocated = false,
        });
        _ = try ins.call(fref, &[_]clif.Value{st_ptr});
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// weak_load_strong(side_table_ptr: i64) -> i64 (obj_or_null)
//
// Load a strong reference from a weak reference's side table.
// Returns the object pointer if alive, 0 (null) if deiniting/freed.
// Does NOT retain — safe in single-threaded; will add retain for concurrency.
//
// Reference: Swift swift_weakLoadStrong (HeapObject.cpp:856-880)
// ============================================================================

fn generateWeakLoadStrong(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_null = try builder.createBlock();
    const block_check = try builder.createBlock();
    const block_return_obj = try builder.createBlock();

    // Entry: null check
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, st_ptr, v_zero);
        _ = try ins.brif(is_null, block_return_null, &.{}, block_check, &.{});
    }

    // Return null
    builder.switchToBlock(block_return_null);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().return_(&[_]clif.Value{v_zero});
    }

    // Check: load refcount from side table, check IsDeiniting
    builder.switchToBlock(block_check);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const refcount = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_REFCOUNT_OFFSET);

        // Check IsDeiniting: (rc >> 32) & 1
        const v_32 = try ins.iconst(clif.Type.I64, 32);
        const shifted = try ins.ushr(refcount, v_32);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const is_deiniting_val = try ins.band(shifted, v_one);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_deiniting = try ins.icmp(.ne, is_deiniting_val, v_zero);
        _ = try ins.brif(is_deiniting, block_return_null, &.{}, block_return_obj, &.{});
    }

    // Return object pointer (alive)
    builder.switchToBlock(block_return_obj);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const st_ptr = builder.blockParams(block_entry)[0];
        const obj_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, st_ptr, SIDE_TABLE_OBJECT_OFFSET);
        _ = try ins.return_(&[_]clif.Value{obj_ptr});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
