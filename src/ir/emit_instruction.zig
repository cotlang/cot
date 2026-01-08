//! Instruction Emission Handlers
//!
//! Converts IR instructions to bytecode.
//! These are free functions that take the BytecodeEmitter as the first parameter.
//!
//! This module handles emission of all IR instruction types to bytecode.

const std = @import("std");
const log = std.log.scoped(.@"bytecode-emit");
const ir = @import("ir.zig");
const cot_runtime = @import("cot_runtime");
const module = cot_runtime.bytecode.module;
const opcodes = cot_runtime.bytecode.opcodes;
const debug = cot_runtime.debug;

// Import emitter types
const emit_mod = @import("emit_bytecode.zig");
const BytecodeEmitter = emit_mod.BytecodeEmitter;
const EmitError = emit_mod.EmitError;

// Builtin definitions
const emit_builtins = @import("emit_builtins.zig");
const opcode_builtins = emit_builtins.opcode_builtins;
const io_functions = emit_builtins.io_functions;
const native_functions = emit_builtins.native_functions;

const Opcode = opcodes.Opcode;

// ============================================================================
// Type Utilities
// ============================================================================

/// Calculate the total number of slots needed for a type.
/// For structs, this recursively counts all nested struct fields.
/// For non-struct types, returns 1.
pub fn getSlotCount(ty: ir.Type) u32 {
    if (ty == .@"struct") {
        const struct_type = ty.@"struct";
        var count: u32 = 0;
        for (struct_type.fields) |field| {
            count += getSlotCount(field.ty);
        }
        return count;
    }
    return 1;
}

/// Calculate the slot offset for a field within a struct.
/// This sums the slot counts of all preceding fields.
pub fn getFieldOffset(struct_type: *const ir.StructType, field_index: u32) u32 {
    debug.print(.emit, "getFieldOffset: struct='{s}' field_index={d} num_fields={d}", .{
        struct_type.name,
        field_index,
        struct_type.fields.len,
    });
    var offset: u32 = 0;
    for (struct_type.fields[0..field_index], 0..) |field, i| {
        const slot_count = getSlotCount(field.ty);
        debug.print(.emit, "  field[{d}] '{s}': type={s} slot_count={d}", .{
            i,
            field.name,
            @tagName(field.ty),
            slot_count,
        });
        offset += slot_count;
    }
    debug.print(.emit, "  -> total offset={d}", .{offset});
    return offset;
}

// Type aliases for inline anonymous struct payloads in ir.Instruction
const Iconst = std.meta.TagPayload(ir.Instruction, .iconst);
const F32const = std.meta.TagPayload(ir.Instruction, .f32const);
const F64const = std.meta.TagPayload(ir.Instruction, .f64const);
const ConstString = std.meta.TagPayload(ir.Instruction, .const_string);
const ConstNull = std.meta.TagPayload(ir.Instruction, .const_null);
const IoClose = std.meta.TagPayload(ir.Instruction, .io_close);
const IoDelete = std.meta.TagPayload(ir.Instruction, .io_delete);
const LoadStructBuf = std.meta.TagPayload(ir.Instruction, .load_struct_buf);
const StoreStructBuf = std.meta.TagPayload(ir.Instruction, .store_struct_buf);
const DebugLine = std.meta.TagPayload(ir.Instruction, .debug_line);

// ============================================================================
// Slot Allocation Helpers
// ============================================================================

/// Recursively allocate local slots for struct fields, handling nested structs.
/// For a struct like Outer{ x: i64, inner: Inner{ a: i64, b: i64 }, y: i64 },
/// this allocates slots as: x, inner.a, inner.b, y (4 slots, not 3).
fn allocateStructFieldSlots(e: *BytecodeEmitter, prefix: []const u8, struct_type: *const ir.StructType) EmitError!void {
    for (struct_type.fields) |field| {
        const qualified_name = std.fmt.allocPrint(
            e.allocator,
            "{s}.{s}",
            .{ prefix, field.name },
        ) catch return EmitError.OutOfMemory;
        // Track allocated string for cleanup
        e.allocated_strings.append(e.allocator, qualified_name) catch return EmitError.OutOfMemory;

        // Register this field's base slot
        try e.locals.put(qualified_name, .{
            .slot = e.local_count,
            .is_global = false,
        });
        debug.print(.emit, "  field {s} -> slot {d}", .{ qualified_name, e.local_count });

        if (field.ty == .@"struct") {
            // Nested struct: recursively allocate slots for its fields
            try allocateStructFieldSlots(e, qualified_name, field.ty.@"struct");
        } else {
            // Primitive type: allocate single slot
            e.local_count += 1;
        }
    }
}

// ============================================================================
// Memory Operations
// ============================================================================

/// Emit alloca instruction - allocate local variable
pub fn emitAlloca(e: *BytecodeEmitter, a: ir.Instruction.Alloca) EmitError!void {
    // Debug: Log ALL alloca instructions to trace type confusion bugs
    const pointee_type_str: []const u8 = if (a.ty == .ptr) @tagName(a.ty.ptr.*) else "N/A";
    log.debug("emitAlloca: name={s} result.id={d} type={s} pointee={s} local_count={d}", .{
        a.name,
        a.result.id,
        @tagName(a.ty),
        pointee_type_str,
        e.local_count,
    });

    // Struct types: check escape analysis to determine stack vs heap allocation
    if (a.ty == .@"struct") {
        // Check if this is a parameter (already registered as local)
        if (e.locals.get(a.name)) |local_info| {
            // Use struct_base_slots for struct pointers - they're compile-time slot
            // references, not loadable runtime values
            try e.struct_base_slots.put(a.result.id, local_info.slot);
            return;
        }

        // Check escape analysis - if local escapes, heap allocate instead
        if (e.localNeedsHeap(a.name)) {
            const struct_type = a.ty.@"struct";
            log.debug("emitAlloca: struct '{s}' escapes, using heap allocation", .{a.name});

            // Emit heap allocation: new_record rd, field_count
            const dest_reg: u4 = 0;
            e.prepareDestReg(dest_reg);
            try e.emitOpcode(.new_record);
            try e.emitU8((@as(u8, dest_reg) << 4) | 0);
            try e.emitU16(@intCast(struct_type.fields.len));

            // Store the heap record in a local slot
            const slot = e.local_count;
            e.local_count += 1;
            try e.locals.put(a.name, .{
                .slot = slot,
                .is_global = false,
            });
            // Track as value_slots (loadable runtime value, not struct_base_slots)
            try e.value_slots.put(a.result.id, slot);

            // Store the heap record to the local slot
            try e.emitOpcode(.store_local);
            try e.emitU8(@as(u8, dest_reg) << 4);
            try e.emitU8(@intCast(slot));

            e.setLastResult(a.result.id, dest_reg);
            return;
        }

        // Stack allocation (default) - allocate slots for struct fields
        const struct_type = a.ty.@"struct";
        const base_slot = e.local_count;
        // Register the struct base slot
        try e.locals.put(a.name, .{
            .slot = base_slot,
            .is_global = false,
        });
        // Use struct_base_slots instead of value_slots - struct pointers are
        // compile-time concepts pointing to where fields live, not loadable values
        try e.struct_base_slots.put(a.result.id, base_slot);
        debug.print(.emit, "alloca struct: name={s} result.id={d} base_slot={d}", .{ a.name, a.result.id, base_slot });
        // Allocate slots for each field, recursively handling nested structs
        try allocateStructFieldSlots(e, a.name, struct_type);
        return;
    }

    // Pointer-to-struct types: these are stack pointer parameters
    // The callee receives a StackPtr value and accesses fields via load_indirect
    if (a.ty == .ptr) {
        log.debug("alloca: ptr type detected, pointee={s}", .{@tagName(a.ty.ptr.*)});
        if (a.ty.ptr.* == .@"struct") {
            log.debug("alloca: ptr-to-struct detected, name='{s}' stack_ptr_params.count={d}", .{
                a.name, e.stack_ptr_params.count(),
            });
            // Check if this is a stack pointer parameter (already registered)
            if (e.stack_ptr_params.get(a.name)) |ptr_slot| {
                // Map the alloca result to the stack pointer param slot
                // This tells emitFieldPtr to use indirect access
                try e.value_slots.put(a.result.id, ptr_slot);
                // Also track this value ID as a stack pointer for indirect field access
                try e.stack_ptr_value_ids.put(a.result.id, ptr_slot);
                log.debug("alloca ptr-to-struct param: name={s} result.id={d} ptr_slot={d}", .{
                    a.name, a.result.id, ptr_slot,
                });
                return;
            }
            // Non-param pointer-to-struct local variable (e.g., var p: *MyStruct = &obj)
            // Allocate a slot for the pointer variable and track it
            // so that loads from this slot can be identified as dynamic stack pointers
            const ptr_slot = e.local_count;
            try e.locals.put(a.name, .{
                .slot = ptr_slot,
                .is_global = false,
            });
            try e.value_slots.put(a.result.id, ptr_slot);
            try e.ptr_to_struct_local_allocas.put(a.result.id, ptr_slot);
            e.local_count += 1;
            log.debug("alloca ptr-to-struct local: name={s} result.id={d} slot={d}", .{
                a.name, a.result.id, ptr_slot,
            });
            debug.print(.emit, "alloca ptr-to-struct local: name={s} result.id={d} slot={d}", .{
                a.name, a.result.id, ptr_slot,
            });
            return;
        }
    }

    // Array types: u8 arrays (DBL buffers) use single slot, other arrays use multiple slots
    if (a.ty == .array) {
        log.debug("emitAlloca: array detected - name={s} element_type={s} length={d}", .{
            a.name,
            @tagName(a.ty.array.element.*),
            a.ty.array.length,
        });

        // Check if this is a parameter (already registered as local) - use existing slot
        if (e.locals.get(a.name)) |local_info| {
            try e.value_slots.put(a.result.id, local_info.slot);
            debug.print(.emit, "alloca array (existing param): {s} slot={d}", .{ a.name, local_info.slot });
            return;
        }

        const base_slot = e.local_count;
        const array_len = a.ty.array.length;

        // Register the array base slot
        try e.locals.put(a.name, .{
            .slot = base_slot,
            .is_global = false,
        });
        try e.value_slots.put(a.result.id, base_slot);

        // u8 arrays (DBL alpha/string buffers): single slot holding a buffer value
        if (a.ty.array.element.* == .u8) {
            e.local_count += 1;
            log.debug("emitAlloca: EMITTING alloc_buffer for {s} at slot {d} size={d}", .{ a.name, base_slot, array_len });
            debug.print(.emit, "alloca buffer: {s} slot={d} size={d}", .{ a.name, base_slot, array_len });

            // Emit buffer initialization: alloc_buffer rd, size
            // This creates a mutable FixedString at the slot
            try e.emitOpcode(.alloc_buffer);
            try e.emitU8(@intCast(base_slot)); // slot to store buffer
            try e.emitU16(@intCast(array_len)); // buffer size
            return;
        }

        // Other array types: one slot per element (legacy behavior)
        e.local_count += @intCast(array_len);
        debug.print(.emit, "alloca array: {s} base_slot={d} length={d} next_slot={d}", .{ a.name, base_slot, array_len, e.local_count });
        return;
    }

    // Non-struct types: check for existing registrations
    if (e.globals.get(a.name)) |global_info| {
        // Use the existing global slot (0x8000 bit marks it as global)
        try e.value_slots.put(a.result.id, global_info.slot | 0x8000);
    } else if (e.locals.get(a.name)) |local_info| {
        // This is a parameter - use the existing parameter slot
        try e.value_slots.put(a.result.id, local_info.slot);
    } else {
        // Allocate a new local variable slot (primitive type)
        try e.locals.put(a.name, .{
            .slot = e.local_count,
            .is_global = false,
        });
        try e.value_slots.put(a.result.id, e.local_count);
        e.local_count += 1;
    }
}

/// Emit load instruction
pub fn emitLoad(e: *BytecodeEmitter, l: ir.Instruction.Load) EmitError!void {
    // Propagate array pointer target tracking through loads
    // If l.ptr points to an array, the result should also point to that array
    if (e.array_ptr_targets.get(l.ptr.id)) |array_slot| {
        try e.array_ptr_targets.put(l.result.id, array_slot);
        debug.print(.emit, "load: propagate array ptr id={d} -> result={d} array_slot={d}", .{ l.ptr.id, l.result.id, array_slot });
    }

    // Propagate stack pointer value ID tracking through loads
    // When loading from a stack pointer param's alloca, the loaded value is also a stack pointer
    log.debug("load: checking stack_ptr_value_ids for ptr.id={d}", .{l.ptr.id});
    if (e.stack_ptr_value_ids.get(l.ptr.id)) |ptr_slot| {
        try e.stack_ptr_value_ids.put(l.result.id, ptr_slot);
        try e.value_slots.put(l.result.id, ptr_slot);
        log.debug("load: propagate stack_ptr_value_ids ptr.id={d} -> result.id={d} ptr_slot={d}", .{
            l.ptr.id, l.result.id, ptr_slot,
        });
    }

    // Track loads from ptr-to-struct local variables
    // When we load from a local like `var p: *MyStruct = &obj`, the loaded value
    // is a dynamic stack pointer that needs load_indirect for field access
    if (e.ptr_to_struct_local_allocas.get(l.ptr.id)) |ptr_slot| {
        // The loaded value is a stack pointer stored at ptr_slot
        // Track this so emitFieldPtr knows to use indirect access
        try e.dynamic_stack_ptr_sources.put(l.result.id, ptr_slot);
        log.debug("load: dynamic stack ptr from local: ptr.id={d} -> result.id={d} ptr_slot={d}", .{
            l.ptr.id, l.result.id, ptr_slot,
        });
        debug.print(.emit, "load: dynamic stack ptr from local: ptr.id={d} -> result.id={d} ptr_slot={d}", .{
            l.ptr.id, l.result.id, ptr_slot,
        });
    }

    // Propagate derived stack pointer tracking through loads
    // When loading from a struct field accessed through a stack pointer, the result is also a derived stack pointer
    if (e.derived_stack_ptrs.get(l.ptr.id)) |info| {
        try e.derived_stack_ptrs.put(l.result.id, info);
        log.debug("load: propagate derived_stack_ptrs ptr.id={d} -> result.id={d} base_slot={d} field_offset={d}", .{
            l.ptr.id, l.result.id, info.base_slot, info.field_offset,
        });
    }

    // Propagate indirect field access info through loads
    // When loading from a field_ptr result that's in indirect_fields, propagate the info
    if (e.indirect_fields.get(l.ptr.id)) |info| {
        try e.indirect_fields.put(l.result.id, info);
        log.debug("load: propagate indirect_fields ptr.id={d} -> result.id={d} ptr_slot={d} field_offset={d}", .{
            l.ptr.id, l.result.id, info.ptr_slot, info.field_offset,
        });
        // Note: we don't put this in value_slots - it will be handled by getValueInReg's indirect_fields check
        return; // No bytecode emitted for loads from indirect fields
    }

    // Propagate struct_base_slots - when loading from a struct pointer,
    // the loaded value also needs to know where its fields are stored
    if (e.struct_base_slots.get(l.ptr.id)) |base_slot| {
        try e.struct_base_slots.put(l.result.id, base_slot);
        // Also put in value_slots so writeback mechanism can find it for is_ref parameters
        try e.value_slots.put(l.result.id, base_slot);
        log.debug("load: propagate struct_base_slots ptr.id={d} -> result.id={d} base_slot={d}", .{ l.ptr.id, l.result.id, base_slot });
    }

    if (e.value_slots.get(l.ptr.id)) |slot_info| {
        try e.value_slots.put(l.result.id, slot_info);
        log.debug("load: propagate value_slots ptr.id={d} -> result.id={d} slot={d}", .{ l.ptr.id, l.result.id, slot_info });
    } else if (e.value_consts.get(l.ptr.id)) |const_idx| {
        try e.value_consts.put(l.result.id, const_idx);
    } else if (e.struct_base_slots.get(l.ptr.id)) |_| {
        // Already handled above - struct values are tracked in struct_base_slots, not value_slots
    } else if (e.last_result_value) |last_id| {
        if (last_id == l.ptr.id) {
            e.setLastResult(l.result.id, e.last_result_reg);
        } else {
            log.debug("WARNING: .load ptr.id={d} not found, last_result={d}", .{ l.ptr.id, last_id });
        }
    } else {
        log.debug("WARNING: .load ptr.id={d} not found", .{l.ptr.id});
    }
}

/// Emit store instruction
/// For DBL field views (overlays), uses str_slice_store to write through to the
/// underlying buffer at the field's byte offset. This implements DBL RECORD
/// semantics where field assignments modify the record buffer in place.
pub fn emitStore(e: *BytecodeEmitter, s: ir.Instruction.Store) EmitError!void {
    // Check if this is a field view (overlay) store that needs write-through
    if (e.field_view_info.get(s.ptr.id)) |fv_info| {
        // Field view store: use str_slice_store for write-through semantics
        // Load value FIRST (into r3) to avoid clobbering other registers
        // Then load buffer, offset, length into r0, r1, r2
        // Finally emit str_slice_store and store the result back

        debug.print(.emit, "store: field_view id={d} offset={d} len={d} -> str_slice_store", .{
            s.ptr.id, fv_info.byte_offset, fv_info.length,
        });

        // Load value into r3 FIRST (emitValueToReg may use r0-r2 as temps)
        try e.emitValueToReg(s.value, 3);

        // Load the base buffer into r0
        if (fv_info.is_global) {
            try e.emitOpcode(.load_global);
            try e.emitU8(0); // r0
            try e.emitU16(fv_info.base_slot);
        } else {
            try e.emitOpcode(.load_local);
            try e.emitU8(0 << 4); // r0
            try e.emitU8(@intCast(fv_info.base_slot));
        }

        // Load offset constant into r1
        const offset_idx = try e.addConstant(.{ .integer = fv_info.byte_offset });
        try e.emitOpcode(.load_const);
        try e.emitU8(1 << 4); // r1 in upper nibble
        try e.emitU16(offset_idx);

        // Load length constant into r2
        const len_idx = try e.addConstant(.{ .integer = fv_info.length });
        try e.emitOpcode(.load_const);
        try e.emitU8(2 << 4); // r2 in upper nibble
        try e.emitU16(len_idx);

        // Emit str_slice_store r0, r1, r2, r3 (target, start, len, val)
        try e.emitOpcode(.str_slice_store);
        try e.emitU8((0 << 4) | 1); // r0 (target), r1 (start)
        try e.emitU8((2 << 4) | 3); // r2 (len), r3 (val)
        try e.emitU8(1); // is_length = true

        // Store the modified buffer back to base slot
        if (fv_info.is_global) {
            try e.emitRegStoreGlobal(0, fv_info.base_slot);
        } else {
            try e.emitRegStoreLocal(0, @intCast(fv_info.base_slot));
        }
        return;
    }

    // Check if this is an indirect field store (storing through stack pointer)
    if (e.indirect_fields.get(s.ptr.id)) |info| {
        debug.print(.emit, "store: indirect field ptr_slot={d} field_offset={d}", .{
            info.ptr_slot, info.field_offset,
        });
        // Load the value to store into r0
        try e.emitValueToReg(s.value, 0);
        // Load the stack pointer value into r1
        if (info.ptr_slot < 256) {
            try e.emitOpcode(.load_local);
            try e.emitU8(1 << 4); // r1
            try e.emitU8(@intCast(info.ptr_slot));
        } else {
            try e.emitOpcode(.load_local16);
            try e.emitU8(1 << 4); // r1
            try e.emitU16(info.ptr_slot);
        }
        // Emit store_indirect: rs_ptr, offset, rs_val
        // Format: [opcode] [rs_ptr:4|rs_val:4] [offset:16]
        try e.emitOpcode(.store_indirect);
        try e.emitU8((1 << 4) | 0); // r1 (ptr), r0 (val)
        try e.emitU16(info.field_offset);
        return;
    }

    // Regular store
    // Check if this is a struct value being stored (by VALUE, not a pointer)
    // IMPORTANT: Pointer-to-struct types (like *Expr) are NOT struct values!
    // A pointer is always a single 8-byte value regardless of what it points to.
    // Only actual struct types need multi-slot storage.
    //
    // SPECIAL CASE: Nested struct initialization
    // When storing a ptr-to-struct to a struct field (the field expects a struct value),
    // we need to COPY all fields from the source struct to the destination.
    // This happens when: .span = Span{ .start = 0, .end = 2 }
    // The Span{} creates a temp struct and returns a pointer, but we need to copy values.
    const struct_type_opt: ?*const ir.StructType = if (s.value.ty == .@"struct")
        s.value.ty.@"struct"
    else if (s.value.ty == .ptr and s.value.ty.ptr.* == .@"struct" and
        s.ptr.ty == .ptr and s.ptr.ty.ptr.* == .@"struct")
        // Both source and destination are pointers to structs - copy the struct fields
        s.value.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Use flattened slot count to handle nested structs
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });

        // Check if source value has slots (from local struct or struct_base_slots)
        const value_slot_opt: ?u16 = e.value_slots.get(s.value.id) orelse e.struct_base_slots.get(s.value.id);
        const ptr_slot_opt: ?u16 = e.value_slots.get(s.ptr.id) orelse e.struct_base_slots.get(s.ptr.id);
        if (value_slot_opt) |value_slot| {
            if (ptr_slot_opt) |ptr_slot| {
                // Struct-to-struct copy: copy all fields from source slots to dest slots
                debug.print(.emit, "store struct: copying {d} fields from slot {d} to slot {d}", .{
                    field_count, value_slot & 0x7FFF, ptr_slot & 0x7FFF,
                });
                const src_base = value_slot & 0x7FFF;
                const dst_base = ptr_slot & 0x7FFF;
                for (0..field_count) |field_idx| {
                    const src_slot = src_base + field_idx;
                    const dst_slot = dst_base + field_idx;
                    // Load from source (use 16-bit variant if slot >= 256)
                    if (src_slot < 256) {
                        try e.emitOpcode(.load_local);
                        try e.emitU8(0 << 4); // r0
                        try e.emitU8(@intCast(src_slot));
                    } else {
                        try e.emitOpcode(.load_local16);
                        try e.emitU8(0 << 4); // r0
                        try e.emitU16(@intCast(src_slot));
                    }
                    // Store to dest (use 16-bit variant if slot >= 256)
                    if (dst_slot < 256) {
                        try e.emitOpcode(.store_local);
                        try e.emitU8(0 << 4); // r0
                        try e.emitU8(@intCast(dst_slot));
                    } else {
                        try e.emitOpcode(.store_local16);
                        try e.emitU8(0 << 4); // r0
                        try e.emitU16(@intCast(dst_slot));
                    }
                }
                return;
            }
        }

        // Check if source is the last result from a call (struct returned in high registers)
        // Note: This only works for structs with ≤16 fields. Larger structs use slot-based storage.
        if (e.last_result_value) |last_id| {
            if (last_id == s.value.id and field_count <= 16) {
                // This store is for a struct returned from a function call
                // The struct fields are in consecutive high registers starting at last_result_reg
                // Check both value_slots (for loaded values) and struct_base_slots (for alloca'd structs)
                const call_ptr_slot_opt: ?u16 = e.value_slots.get(s.ptr.id) orelse e.struct_base_slots.get(s.ptr.id);
                if (call_ptr_slot_opt) |ptr_slot| {
                    const return_base_reg = e.last_result_reg;
                    const dst_base = ptr_slot & 0x7FFF;
                    debug.print(.emit, "store struct from call: {d} fields from r{d} to slot {d}", .{
                        field_count, return_base_reg, dst_base,
                    });
                    for (0..field_count) |field_idx| {
                        const src_reg: u8 = return_base_reg + @as(u8, @intCast(field_idx));
                        const dst_slot = dst_base + field_idx;
                        debug.print(.emit, "  store r{d} -> slot {d}", .{ src_reg, dst_slot });
                        // Use 16-bit variant if slot >= 256
                        if (dst_slot < 256) {
                            try e.emitOpcode(.store_local);
                            try e.emitU8(src_reg << 4);
                            try e.emitU8(@intCast(dst_slot));
                        } else {
                            try e.emitOpcode(.store_local16);
                            try e.emitU8(src_reg << 4);
                            try e.emitU16(@intCast(dst_slot));
                        }
                    }
                    return;
                }
            }
        }
    }

    // Special case: storing an array value/pointer into a pointer slot
    // Track the array slot so array_load can use it when accessing through the pointer
    // BUT: If the destination is a struct field (has a slot), we must emit the actual store
    //      because struct fields need the value copied, not just pointer tracking.
    if (s.value.ty == .array or (s.value.ty == .ptr and s.value.ty.ptr.* == .array)) {
        const dest_is_struct_field = e.value_slots.get(s.ptr.id) != null;
        if (!dest_is_struct_field) {
            if (e.value_slots.get(s.value.id)) |value_slot| {
                const array_slot = value_slot & 0x7FFF;
                // Record that the target pointer (s.ptr.id) points to this array slot
                try e.array_ptr_targets.put(s.ptr.id, array_slot);
                debug.print(.emit, "store: array ptr tracking id={d} -> array_slot={d}", .{ s.ptr.id, array_slot });
                // For array pointer stores, no actual bytecode needed - just tracking
                // The array is accessed directly by slot in array_load
                return;
            }
        } else {
            debug.print(.emit, "store: array to struct field, emitting actual store", .{});
        }
    }

    const src_reg = try e.getValueInReg(s.value, 0);
    if (e.value_slots.get(s.ptr.id)) |slot_info| {
        if (slot_info & 0x8000 != 0) {
            const slot = slot_info & 0x7FFF;
            try e.emitRegStoreGlobal(src_reg, slot);
        } else {
            // Local variable - use 16-bit variant for slots >= 256
            if (slot_info < 256) {
                try e.emitRegStoreLocal(src_reg, @intCast(slot_info));
            } else {
                try e.emitOpcode(.store_local16);
                try e.emitU8(@as(u8, src_reg) << 4);
                try e.emitU16(@intCast(slot_info));
            }
        }
    }
}

/// Emit field_ptr instruction
/// Uses getFieldOffset to properly calculate slot offset for nested structs.
pub fn emitFieldPtr(e: *BytecodeEmitter, fp: ir.Instruction.FieldPtr) EmitError!void {
    log.debug("emitFieldPtr: struct_ptr.id={d} checking stack_ptr_value_ids (count={d})", .{
        fp.struct_ptr.id, e.stack_ptr_value_ids.count(),
    });
    // Check if this is accessing through a stack pointer parameter
    // If so, we need to use indirect field access (load_indirect/store_indirect)
    if (e.stack_ptr_value_ids.get(fp.struct_ptr.id)) |ptr_slot| {
        log.debug("emitFieldPtr: FOUND in stack_ptr_value_ids! ptr_slot={d}", .{ptr_slot});
        // Get the struct type to calculate field offset
        const struct_type: ?*const ir.StructType = if (fp.struct_ptr.ty == .@"struct")
            fp.struct_ptr.ty.@"struct"
        else if (fp.struct_ptr.ty == .ptr and fp.struct_ptr.ty.ptr.* == .@"struct")
            fp.struct_ptr.ty.ptr.*.@"struct"
        else
            null;

        const field_offset: u32 = if (struct_type) |st|
            getFieldOffset(st, fp.field_index)
        else
            fp.field_index;

        // Check if the field type is a struct (meaning we need a derived StackPtr for method calls)
        // vs a primitive (where we need load_indirect to get the value)
        // The result type of field_ptr is *FieldType, so check if the pointee is a struct
        log.debug("emitFieldPtr: result.ty={s}", .{@tagName(fp.result.ty)});
        const field_is_struct = if (fp.result.ty == .ptr) blk: {
            log.debug("emitFieldPtr: result is ptr, pointee={s}", .{@tagName(fp.result.ty.ptr.*)});
            break :blk fp.result.ty.ptr.* == .@"struct";
        } else false;

        if (field_is_struct) {
            // Field is a struct - track as derived stack pointer for method calls
            // When loading this value, we'll emit ptr_offset to create a derived StackPtr
            log.debug("emitFieldPtr: DERIVED STACK PTR - struct field result.id={d} base={d} offset={d}", .{
                fp.result.id, ptr_slot, field_offset,
            });
            try e.derived_stack_ptrs.put(fp.result.id, .{
                .base_slot = ptr_slot,
                .field_offset = @intCast(field_offset),
            });
            debug.print(.emit, "field_ptr (derived ptr): struct_ptr.id={d} ptr_slot={d} field_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, ptr_slot, field_offset, fp.result.id,
            });
        } else {
            // Field is primitive - track for indirect field access
            // When loading this value, we'll emit load_indirect instead of load_local
            log.debug("emitFieldPtr: INDIRECT FIELD - primitive field result.id={d} ptr_slot={d} offset={d}", .{
                fp.result.id, ptr_slot, field_offset,
            });
            try e.indirect_fields.put(fp.result.id, .{
                .ptr_slot = ptr_slot,
                .field_offset = @intCast(field_offset),
            });
            debug.print(.emit, "field_ptr (indirect): struct_ptr.id={d} ptr_slot={d} field_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, ptr_slot, field_offset, fp.result.id,
            });
        }
        return;
    }

    // Check if this is accessing through a dynamic stack pointer (loaded from ptr-to-struct local)
    // This is similar to stack_ptr_value_ids but for local pointer variables, not parameters
    if (e.dynamic_stack_ptr_sources.get(fp.struct_ptr.id)) |ptr_slot| {
        log.debug("emitFieldPtr: FOUND in dynamic_stack_ptr_sources! ptr_slot={d}", .{ptr_slot});
        // Get the struct type to calculate field offset
        const struct_type: ?*const ir.StructType = if (fp.struct_ptr.ty == .@"struct")
            fp.struct_ptr.ty.@"struct"
        else if (fp.struct_ptr.ty == .ptr and fp.struct_ptr.ty.ptr.* == .@"struct")
            fp.struct_ptr.ty.ptr.*.@"struct"
        else
            null;

        const field_offset: u32 = if (struct_type) |st|
            getFieldOffset(st, fp.field_index)
        else
            fp.field_index;

        // Check if the field type is a struct (needs derived StackPtr) or primitive (needs load_indirect)
        log.debug("emitFieldPtr: dynamic ptr result.ty={s}", .{@tagName(fp.result.ty)});
        const field_is_struct = if (fp.result.ty == .ptr) blk: {
            log.debug("emitFieldPtr: result is ptr, pointee={s}", .{@tagName(fp.result.ty.ptr.*)});
            break :blk fp.result.ty.ptr.* == .@"struct";
        } else false;

        if (field_is_struct) {
            // Field is a struct - track as derived stack pointer
            log.debug("emitFieldPtr: DYNAMIC DERIVED PTR - struct field result.id={d} ptr_slot={d} offset={d}", .{
                fp.result.id, ptr_slot, field_offset,
            });
            try e.derived_stack_ptrs.put(fp.result.id, .{
                .base_slot = ptr_slot,
                .field_offset = @intCast(field_offset),
            });
            debug.print(.emit, "field_ptr (dynamic derived ptr): struct_ptr.id={d} ptr_slot={d} field_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, ptr_slot, field_offset, fp.result.id,
            });
        } else {
            // Field is primitive - track for indirect field access
            log.debug("emitFieldPtr: DYNAMIC INDIRECT FIELD - primitive field result.id={d} ptr_slot={d} offset={d}", .{
                fp.result.id, ptr_slot, field_offset,
            });
            try e.indirect_fields.put(fp.result.id, .{
                .ptr_slot = ptr_slot,
                .field_offset = @intCast(field_offset),
            });
            debug.print(.emit, "field_ptr (dynamic indirect): struct_ptr.id={d} ptr_slot={d} field_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, ptr_slot, field_offset, fp.result.id,
            });
        }
        return;
    }

    // Check if this is accessing through a derived stack pointer (nested struct field through stack pointer)
    // This handles cases like outer_ptr.inner.value where .inner is a struct field
    if (e.derived_stack_ptrs.get(fp.struct_ptr.id)) |info| {
        log.debug("emitFieldPtr: FOUND in derived_stack_ptrs! base_slot={d} field_offset={d}", .{
            info.base_slot, info.field_offset,
        });
        // Get the struct type to calculate field offset
        const struct_type: ?*const ir.StructType = if (fp.struct_ptr.ty == .@"struct")
            fp.struct_ptr.ty.@"struct"
        else if (fp.struct_ptr.ty == .ptr and fp.struct_ptr.ty.ptr.* == .@"struct")
            fp.struct_ptr.ty.ptr.*.@"struct"
        else
            null;

        const new_field_offset: u32 = if (struct_type) |st|
            getFieldOffset(st, fp.field_index)
        else
            fp.field_index;

        // Combined offset = existing offset + new field offset
        const combined_offset = info.field_offset + @as(u16, @intCast(new_field_offset));

        // Check if the field type is a struct (needs derived StackPtr) or primitive (needs load_indirect)
        log.debug("emitFieldPtr: derived ptr result.ty={s}", .{@tagName(fp.result.ty)});
        const field_is_struct = if (fp.result.ty == .ptr) blk: {
            log.debug("emitFieldPtr: result is ptr, pointee={s}", .{@tagName(fp.result.ty.ptr.*)});
            break :blk fp.result.ty.ptr.* == .@"struct";
        } else false;

        if (field_is_struct) {
            // Field is a struct - create new derived stack pointer with combined offset
            log.debug("emitFieldPtr: NESTED DERIVED PTR - struct field result.id={d} base_slot={d} offset={d}", .{
                fp.result.id, info.base_slot, combined_offset,
            });
            try e.derived_stack_ptrs.put(fp.result.id, .{
                .base_slot = info.base_slot,
                .field_offset = combined_offset,
            });
            debug.print(.emit, "field_ptr (nested derived ptr): struct_ptr.id={d} base_slot={d} combined_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, info.base_slot, combined_offset, fp.result.id,
            });
        } else {
            // Field is primitive - track for indirect field access with combined offset
            log.debug("emitFieldPtr: NESTED INDIRECT FIELD - primitive field result.id={d} ptr_slot={d} offset={d}", .{
                fp.result.id, info.base_slot, combined_offset,
            });
            try e.indirect_fields.put(fp.result.id, .{
                .ptr_slot = info.base_slot,
                .field_offset = combined_offset,
            });
            debug.print(.emit, "field_ptr (nested indirect): struct_ptr.id={d} base_slot={d} combined_offset={d} -> result.id={d}", .{
                fp.struct_ptr.id, info.base_slot, combined_offset, fp.result.id,
            });
        }
        return;
    }

    // Direct field access - First check struct_base_slots (for stack-allocated structs from alloca),
    // then fall back to value_slots (for other pointer sources like parameters)
    const struct_slot = e.struct_base_slots.get(fp.struct_ptr.id) orelse
        e.value_slots.get(fp.struct_ptr.id) orelse {
        debug.print(.emit, "WARNING: field_ptr struct_ptr.id={d} not found in struct_base_slots or value_slots", .{fp.struct_ptr.id});
        return;
    };

    // Get the struct type to calculate proper offset
    const struct_type: ?*const ir.StructType = if (fp.struct_ptr.ty == .@"struct")
        fp.struct_ptr.ty.@"struct"
    else if (fp.struct_ptr.ty == .ptr and fp.struct_ptr.ty.ptr.* == .@"struct")
        fp.struct_ptr.ty.ptr.*.@"struct"
    else
        null;

    // Debug: show type information
    debug.print(.emit, "emitFieldPtr: struct_ptr.ty={s} field_index={d} struct_type={s}", .{
        @tagName(fp.struct_ptr.ty),
        fp.field_index,
        if (struct_type) |st| st.name else "<null>",
    });

    // Calculate field offset accounting for nested struct sizes
    const field_offset: u32 = if (struct_type) |st|
        getFieldOffset(st, fp.field_index)
    else blk: {
        debug.print(.emit, "  WARNING: No struct type for field_ptr, using raw index!", .{});
        break :blk fp.field_index;
    };

    const field_slot = (struct_slot & 0x7FFF) + @as(u16, @intCast(field_offset));
    const is_global = (struct_slot & 0x8000) != 0;
    // Field pointers go in value_slots - they ARE loadable values (pointing to individual field slots)
    try e.value_slots.put(fp.result.id, field_slot | (if (is_global) @as(u16, 0x8000) else 0));
    debug.print(.emit, "field_ptr: struct_slot={d} field_index={d} field_offset={d} -> slot {d}", .{
        struct_slot & 0x7FFF, fp.field_index, field_offset, field_slot,
    });
}

// ============================================================================
// Constants
// ============================================================================

/// Emit iconst instruction
pub fn emitIconst(e: *BytecodeEmitter, c: Iconst) EmitError!void {
    // Handle boolean constants separately to preserve type at runtime
    if (c.ty == .bool) {
        const const_idx = try e.addConstant(.{ .boolean = c.value != 0 });
        try e.value_consts.put(c.result.id, const_idx);
    } else {
        const const_idx = try e.addConstant(.{ .integer = c.value });
        try e.value_consts.put(c.result.id, const_idx);
    }
}

/// Emit f32const instruction
pub fn emitF32const(e: *BytecodeEmitter, c: F32const) EmitError!void {
    const const_idx = try e.addConstant(.{ .float = @as(f64, c.value) });
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit f64const instruction
pub fn emitF64const(e: *BytecodeEmitter, c: F64const) EmitError!void {
    const const_idx = try e.addConstant(.{ .float = c.value });
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit const_string instruction
pub fn emitConstString(e: *BytecodeEmitter, c: ConstString) EmitError!void {
    const const_idx = try e.addString(c.value);
    try e.value_consts.put(c.result.id, const_idx);
}

/// Emit const_null instruction - load null into a register
pub fn emitConstNull(e: *BytecodeEmitter, c: ConstNull) EmitError!void {
    const dest_reg: u4 = 0;
    try e.emitRegLoadLiteral(.load_null, dest_reg);
    e.setLastResult(c.result.id, dest_reg);
}

// ============================================================================
// Arithmetic Operations
// ============================================================================

/// Emit binary arithmetic instruction
pub fn emitBinaryArith(e: *BytecodeEmitter, op: Opcode, lhs: ir.Value, rhs: ir.Value, result: ir.Value) EmitError!void {
    const lhs_reg = try e.getValueInReg(lhs, 0);
    // Choose a temp_reg for rhs that won't conflict with lhs_reg.
    // If lhs ended up in r1 (e.g., from last_result), use r0 for rhs instead.
    const rhs_temp_reg: u4 = if (lhs_reg == 1) 0 else 1;
    const rhs_reg = try e.getValueInReg(rhs, rhs_temp_reg);

    // Allocate a fresh register for the result, spilling if necessary
    const dest_reg = try e.allocateWithSpill(result.id);

    try e.emitRegArith(op, dest_reg, lhs_reg, rhs_reg);
    e.setLastResult(result.id, dest_reg);
}

/// Emit ineg instruction
pub fn emitIneg(e: *BytecodeEmitter, n: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(n.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.neg, dest_reg, src_reg);
    e.setLastResult(n.result.id, dest_reg);
}

/// Emit round instruction (DBL ## operator - true rounding)
/// fn_round rd, rs, prec - Format: [rd:4|rs:4] [prec:8]
pub fn emitRound(e: *BytecodeEmitter, r: ir.Instruction.RoundOp) EmitError!void {
    const value_reg = try e.getValueInReg(r.value, 0);
    const dest_reg: u4 = 1;

    // Get precision value - must be a constant or loaded value
    const prec: u8 = if (e.value_consts.get(r.places.id)) |const_idx| blk: {
        // Load the constant value - for now assume it's a small integer
        _ = const_idx;
        break :blk 2; // Default precision
    } else if (e.last_result_value) |last_id| blk: {
        if (last_id == r.places.id) {
            // Places value is the last computed result - use as precision
            // For simplicity, default to 2 decimal places
            break :blk 2;
        }
        break :blk 2;
    } else 2;

    try e.emitOpcode(.fn_round);
    try e.emitU8((@as(u8, dest_reg) << 4) | value_reg);
    try e.emitU8(prec);
    e.setLastResult(r.result.id, dest_reg);
}

/// Emit trunc instruction (DBL # operator - truncating round)
/// fn_trunc rd, rs - Format: [rd:4|rs:4] [0]
pub fn emitTrunc(e: *BytecodeEmitter, t: ir.Instruction.RoundOp) EmitError!void {
    const value_reg = try e.getValueInReg(t.value, 0);
    const dest_reg: u4 = 1;

    // fn_trunc uses the places value as precision
    // For DBL compatibility, places specifies decimal positions to keep
    const prec: u8 = if (e.value_consts.get(t.places.id)) |const_idx| blk: {
        _ = const_idx;
        break :blk 0; // Truncate to integer by default
    } else 0;

    try e.emitOpcode(.fn_trunc);
    try e.emitU8((@as(u8, dest_reg) << 4) | value_reg);
    try e.emitU8(prec);
    e.setLastResult(t.result.id, dest_reg);
}

/// Check if a type is a string type (directly or via pointer/array)
fn isStringType(ty: ir.Type) bool {
    return switch (ty) {
        .string => true,
        .ptr => |pointee| pointee.* == .string,
        .array => |arr| arr.element.* == .u8, // []u8 arrays are strings
        else => false,
    };
}

/// Emit icmp instruction
pub fn emitIcmp(e: *BytecodeEmitter, c: ir.Instruction.IcmpOp) EmitError!void {
    const lhs_reg = try e.getValueInReg(c.lhs, 0);
    // Choose a temp_reg for rhs that won't conflict with lhs_reg.
    const rhs_temp_reg: u4 = if (lhs_reg == 1) 0 else 1;
    const rhs_reg = try e.getValueInReg(c.rhs, rhs_temp_reg);

    // Allocate a fresh register for the result, spilling if necessary
    const dest_reg = try e.allocateWithSpill(c.result.id);

    // Check if type is string, or a pointer to string (for record fields)
    const is_string = isStringType(c.lhs.ty);
    const opcode: Opcode = switch (c.cond) {
        .eq => if (is_string) .cmp_str_eq else .cmp_eq,
        .ne => if (is_string) .cmp_str_ne else .cmp_ne,
        .slt, .ult => if (is_string) .cmp_str_lt else .cmp_lt,
        .sle, .ule => if (is_string) .cmp_str_le else .cmp_le,
        .sgt, .ugt => if (is_string) .cmp_str_gt else .cmp_gt,
        .sge, .uge => if (is_string) .cmp_str_ge else .cmp_ge,
    };
    try e.emitRegArith(opcode, dest_reg, lhs_reg, rhs_reg);
    e.setLastResult(c.result.id, dest_reg);
}

/// Emit logical not instruction
pub fn emitLogNot(e: *BytecodeEmitter, l: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(l.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.log_not, dest_reg, src_reg);
    e.setLastResult(l.result.id, dest_reg);
}

/// Emit bitwise not instruction
pub fn emitBitNot(e: *BytecodeEmitter, b: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(b.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.bit_not, dest_reg, src_reg);
    e.setLastResult(b.result.id, dest_reg);
}

// ============================================================================
// String Operations
// ============================================================================

/// Emit str_concat instruction
pub fn emitStrConcat(e: *BytecodeEmitter, s: ir.Instruction.BinaryOp) EmitError!void {
    debug.print(.emit, "str_concat: last_result_value={?d}, last_result_reg={d}, rhs.id={d}", .{
        e.last_result_value,
        e.last_result_reg,
        s.rhs.id,
    });
    var rs2: u4 = undefined;
    if (e.last_result_value) |last_id| {
        if (last_id == s.rhs.id and e.last_result_reg == 0) {
            try e.emitOpcode(.mov);
            try e.emitU8((1 << 4) | 0);
            try e.emitU8(0);
            rs2 = 1;
            e.last_result_value = null;
        } else {
            rs2 = try e.getValueInReg(s.rhs, 1);
        }
    } else {
        rs2 = try e.getValueInReg(s.rhs, 1);
    }
    const rs1 = try e.getValueInReg(s.lhs, 0);
    const rd: u4 = 2;
    try e.emitOpcode(.str_concat);
    try e.emitU8((@as(u8, rd) << 4) | rs1);
    try e.emitU8(@as(u8, rs2) << 4);
    e.setLastResult(s.result.id, rd);
}

/// Emit str_slice instruction
pub fn emitStrSlice(e: *BytecodeEmitter, s: ir.Instruction.StrSlice) EmitError!void {
    const src_reg = try e.getValueInReg(s.source, 0);
    const start_reg = try e.getValueInReg(s.start, 1);
    const len_reg = try e.getValueInReg(s.length_or_end, 2);
    const dest_reg: u4 = 3;
    try e.emitOpcode(.str_slice);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8((@as(u8, start_reg) << 4) | len_reg);
    try e.emitU8(if (s.is_length) 1 else 0);
    e.setLastResult(s.result.id, dest_reg);
}

/// Emit str_slice_store instruction
pub fn emitStrSliceStore(e: *BytecodeEmitter, s: ir.Instruction.StrSliceStore) EmitError!void {
    const target_reg = try e.getValueInReg(s.target, 0);
    const start_reg = try e.getValueInReg(s.start, 1);
    const len_reg = try e.getValueInReg(s.length_or_end, 2);
    const value_reg = try e.getValueInReg(s.value, 3);

    try e.emitOpcode(.str_slice_store);
    try e.emitU8((@as(u8, target_reg) << 4) | start_reg);
    try e.emitU8((@as(u8, len_reg) << 4) | value_reg);
    try e.emitU8(if (s.is_length) 1 else 0);

    if (e.value_slots.get(s.target_ptr.id)) |slot_info| {
        const is_global = slot_info & 0x8000 != 0;
        const slot = slot_info & 0x7FFF;
        if (is_global) {
            try e.emitRegStoreGlobal(target_reg, slot);
        } else {
            if (slot < 256) {
                try e.emitRegStoreLocal(target_reg, @intCast(slot));
            }
        }
    } else {
        debug.print(.emit, "WARNING: str_substr_store target_ptr id={d} not found in value_slots", .{s.target_ptr.id});
    }
}

/// Emit str_byte_at instruction: s[i] -> byte value (i64)
pub fn emitStrByteAt(e: *BytecodeEmitter, s: ir.Instruction.StrByteAt) EmitError!void {
    const str_reg = try e.getValueInReg(s.string, 0);
    const idx_reg = try e.getValueInReg(s.index, 1);
    const dest_reg: u4 = 2;
    // str_index rd, rs, idx_reg - rd = rs[idx_reg]
    // Format: [rd:4|rs:4] [idx_reg:4|0]
    try e.emitOpcode(.str_index);
    try e.emitU8((@as(u8, dest_reg) << 4) | str_reg);
    try e.emitU8(@as(u8, idx_reg) << 4);
    e.setLastResult(s.result.id, dest_reg);
}

// ============================================================================
// Control Flow
// ============================================================================

/// Emit jump instruction
pub fn emitJump(e: *BytecodeEmitter, b: ir.Instruction.Branch) EmitError!void {
    try e.emitRegJmp(b.target);
}

/// Emit brif instruction
pub fn emitBrif(e: *BytecodeEmitter, c: ir.Instruction.CondBranch) EmitError!void {
    const cond_reg = try e.getValueInReg(c.condition, 0);
    try e.emitRegCondJmp(.jz, cond_reg, c.else_block);
    try e.emitRegJmp(c.then_block);
}

/// Emit return instruction
/// For functions with is_ref parameters, we must copy modified values back to registers
/// before returning, so the caller can write them back to the original slots.
/// For struct returns, all fields are loaded into consecutive high registers (r14, r15 for 2-field, etc.)
pub fn emitReturn(e: *BytecodeEmitter, r: ?ir.Value) EmitError!void {
    // Debug: trace return value type for debugging generic/trait issues
    if (r) |val| {
        log.debug("emitReturn: val.id={d} val.ty={s} func={s}", .{
            val.id,
            @tagName(val.ty),
            if (e.current_func) |f| f.name else "(no func)",
        });
    }

    // IMPORTANT: Handle ref parameter copy-back without clobbering the return value.
    // If there's a return value currently in a low register (r0-r7) and we need to
    // copy ref params back to those registers, we must spill the return value first.
    var return_spill_slot: ?u16 = null;
    if (e.current_func) |func| {
        // Calculate total registers needed for ref params
        var reg_offset: u8 = 0;
        for (func.signature.params) |param| {
            const slot_count: u8 = @intCast(getSlotCount(param.ty));
            reg_offset += slot_count;
        }

        // If return value is in a register that will be clobbered, spill it first
        if (r != null and reg_offset > 0) {
            if (e.last_result_value != null and e.last_result_reg < reg_offset) {
                // Spill the return value to a temp slot before it gets clobbered
                return_spill_slot = e.allocateSpillSlot();
                debug.print(.emit, "emitReturn: spilling return value from r{d} to slot {d} before ref param copy-back", .{
                    e.last_result_reg, return_spill_slot.?,
                });
                try e.emitSpillStore(e.last_result_reg, return_spill_slot.?);
                // Track the spill so getValueInReg can find it
                try e.spilled_values.put(e.last_result_value.?, return_spill_slot.?);
                e.last_result_value = null;
            }
        }

        // Now copy ref params back to registers (and stack for overflow)
        // For large structs (>16 slots), first 16 slots go to r0-r15,
        // remaining slots are pushed to arg stack for caller to pop
        reg_offset = 0;
        var ref_overflow_count: u8 = 0; // Track overflow slots pushed for ref params
        for (func.signature.params) |param| {
            const slot_count: u8 = @intCast(getSlotCount(param.ty));

            if (param.is_ref) {
                if (e.locals.get(param.name)) |local_info| {
                    // First pass: load slots 0-15 into registers r0-r15
                    for (0..slot_count) |field_idx| {
                        const slot: u16 = @intCast(local_info.slot + field_idx);
                        const reg: u8 = reg_offset + @as(u8, @intCast(field_idx));
                        if (reg < 16) {
                            try e.emitOpcode(.load_local);
                            try e.emitU8(reg << 4);
                            try e.emitU8(@intCast(slot));
                        }
                    }
                    // Second pass: push overflow slots (15+) to arg stack
                    // These will be popped by the caller after the call returns
                    for (0..slot_count) |field_idx| {
                        const reg: u8 = reg_offset + @as(u8, @intCast(field_idx));
                        if (reg >= 15) {
                            const slot: u16 = @intCast(local_info.slot + field_idx);
                            // push_arg format: [opcode] [slot:16] - 3 bytes total
                            try e.emitOpcode(.push_arg);
                            try e.emitU16(slot);
                            ref_overflow_count += 1;
                        }
                    }
                }
            }
            reg_offset += slot_count;
        }

        // If we pushed overflow slots for ref param writeback, use ret with count > 0
        // to preserve them on the stack for the caller to pop
        if (ref_overflow_count > 0 and r == null) {
            // Void return with ref param overflow: use ret with count
            debug.print(.emit, "emitReturn: void return with {d} ref overflow slots, using ret count={d}", .{ ref_overflow_count, ref_overflow_count });
            try e.emitOpcode(.ret);
            try e.emitU8(ref_overflow_count);
            try e.emitU8(0);
            return;
        }

        // Clear register allocator state for clobbered registers (max 16)
        const clear_count = @min(reg_offset, 16);
        if (clear_count > 0) {
            for (0..clear_count) |reg_idx| {
                if (e.reg_alloc.reg_to_value[@intCast(reg_idx)]) |value_id| {
                    _ = e.reg_alloc.value_to_reg.remove(value_id);
                    e.reg_alloc.reg_to_value[@intCast(reg_idx)] = null;
                    e.reg_alloc.free_regs |= @as(u16, 1) << @intCast(reg_idx);
                }
            }
        }
    }

    // Determine how many registers the return value needs
    // Struct returns use multiple high registers (e.g., r14, r15 for 2 fields)
    var return_field_count: u8 = 0;
    var return_base_reg: u8 = 15;

    if (r) |val| {
        // Check if the FUNCTION'S declared return type is a struct (by value return).
        // This is the key distinction:
        //   - Function returns `Lexer` → return struct by value (expand all fields)
        //   - Function returns `*Expr` → return pointer (single 8-byte value)
        // The value's type may be `.ptr` in both cases (struct literals create stack pointers),
        // but the function signature determines whether we expand fields or not.
        const struct_type_opt: ?*const ir.StructType = blk: {
            // First check the function's declared return type
            if (e.current_func) |func| {
                if (func.signature.return_type == .@"struct") {
                    break :blk func.signature.return_type.@"struct";
                }
            }
            // Fall back to checking the value's type (for cases without function context)
            if (val.ty == .@"struct") {
                break :blk val.ty.@"struct";
            }
            break :blk null;
        };

        // Additional debug for all types - print before the main line to see pointee
        const ptr_to_type_str: []const u8 = if (val.ty == .ptr) @tagName(val.ty.ptr.*) else "N/A";
        const func_ret_type_str: []const u8 = if (e.current_func) |f| @tagName(f.signature.return_type) else "?";
        const has_value_slot = e.value_slots.get(val.id) != null;
        const has_struct_slot = e.struct_base_slots.get(val.id) != null;
        debug.print(.emit, "emitReturn: val.id={d} val.ty={s} ptr_to={s} func_ret={s} struct_type_opt={s} value_slot={} struct_slot={} func={s}", .{
            val.id,
            @tagName(val.ty),
            ptr_to_type_str,
            func_ret_type_str,
            if (struct_type_opt != null) "yes" else "no",
            has_value_slot,
            has_struct_slot,
            if (e.current_func) |f| f.name else "?",
        });

        if (struct_type_opt) |struct_type| {
            // Use flattened slot count to handle nested structs properly
            const total_field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });

            // Check if struct is too large to fit in registers (max 16)
            if (total_field_count > 16) {
                // Large struct: push all fields to stack for ret with count
                // The caller will pop them after the call returns
                const large_base_slot_opt: ?u16 = e.value_slots.get(val.id) orelse e.struct_base_slots.get(val.id);
                if (large_base_slot_opt) |base_slot| {
                    debug.print(.emit, "emitReturn: large struct with {d} fields, base_slot={d}, using stack-based return", .{ total_field_count, base_slot });
                    for (0..total_field_count) |field_idx| {
                        const slot = base_slot + field_idx;
                        // push_arg format: [opcode] [slot:16] - 3 bytes total
                        try e.emitOpcode(.push_arg);
                        try e.emitU16(@intCast(slot));
                    }
                    // Mark for ret with count - use special value to indicate stack-based return
                    return_field_count = @intCast(total_field_count);
                    return_base_reg = 255; // Sentinel value indicating large struct return
                } else {
                    // Struct not in slots - shouldn't happen for large returns
                    debug.print(.emit, "WARNING: large struct return value not in slots", .{});
                    try e.emitValueToReg(val, 15);
                    return_field_count = 1;
                    return_base_reg = 15;
                }
            } else {
                return_field_count = @intCast(total_field_count);
                // Use high registers: for 2 fields use r14, r15; for 3 use r13, r14, r15, etc.
                return_base_reg = 16 - return_field_count;

                debug.print(.emit, "emitReturn: struct with {d} fields, loading into r{d}-r{d}", .{
                    return_field_count,
                    return_base_reg,
                    return_base_reg + return_field_count - 1,
                });

                // Load ALL struct fields (including nested) into consecutive high registers
                // Check both value_slots (for loaded values) and struct_base_slots (for alloca'd structs)
                const base_slot_opt: ?u16 = e.value_slots.get(val.id) orelse e.struct_base_slots.get(val.id);
                if (base_slot_opt) |base_slot| {
                    debug.print(.emit, "emitReturn: loading struct fields from slot {d}, count={d}", .{ base_slot, return_field_count });
                    for (0..return_field_count) |field_idx| {
                        const slot = base_slot + field_idx;
                        const reg: u8 = return_base_reg + @as(u8, @intCast(field_idx));
                        debug.print(.emit, "  field {d}: slot {d} -> r{d}", .{ field_idx, slot, reg });
                        if (slot < 256) {
                            try e.emitOpcode(.load_local);
                            try e.emitU8(reg << 4);
                            try e.emitU8(@intCast(slot));
                        } else if (slot < 65536) {
                            try e.emitOpcode(.load_local16);
                            try e.emitU8(reg << 4);
                            try e.emitU16(@intCast(slot));
                        }
                    }
                    debug.print(.emit, "emitReturn: done loading {d} fields", .{return_field_count});
                } else if (e.last_result_value != null and e.last_result_value.? == val.id) {
                    // Struct returned from a function call - already in registers
                    // The struct fields are in consecutive high registers starting at last_result_reg
                    // We just need to use ret (not ret_val) to preserve all the registers
                    debug.print(.emit, "emitReturn: struct from call result, already in r{d}-r{d}", .{
                        e.last_result_reg,
                        e.last_result_reg + @as(u8, @intCast(return_field_count)) - 1,
                    });
                    // Fields are already in the right registers, just keep the count for ret decision
                    return_base_reg = e.last_result_reg;
                } else {
                    // Fallback: struct value not in slots and not from call - use single register
                    debug.print(.emit, "WARNING: struct return value not in slots or last_result, falling back to single register", .{});
                    try e.emitValueToReg(val, 15);
                    return_field_count = 1;
                    return_base_reg = 15;
                }
            }
        } else {
            // Non-struct return: single value into r15
            try e.emitValueToReg(val, 15);
            return_field_count = 1;
        }
    }

    // Emit the actual return
    // For struct returns with multiple fields, use 'ret' (no value) instead of 'ret_val'
    // because ret_val copies rs -> r15 which would destroy the second field in r15.
    // The struct fields are already in high registers and will survive the return.
    if (r != null) {
        if (return_base_reg == 255) {
            // Large struct return: use ret with count
            // Format: [count:8] [0]
            try e.emitOpcode(.ret);
            try e.emitU8(return_field_count);
            try e.emitU8(0);
        } else if (return_field_count > 1) {
            // Multi-field struct return: use ret (registers already set up)
            try e.emitRegRet(null);
        } else {
            // Single value return: use ret_val
            try e.emitRegRet(@intCast(return_base_reg));
        }
    } else {
        try e.emitRegRet(null);
    }
}

// ============================================================================
// Function Calls
// ============================================================================

/// Emit call instruction - handles all call types
pub fn emitCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Check for built-in method calls (Synergy.NET compatibility aliases)
    // Note: DBL parser lowercases identifiers, so we check lowercase
    if (std.mem.eql(u8, c.callee, "console.writeline")) {
        try emitConsoleCall(e, c, .println);
        return;
    } else if (std.mem.eql(u8, c.callee, "console.write")) {
        try emitConsoleCall(e, c, .print);
        return;
    }

    // Core Cot I/O functions: print(), println()
    // Load ALL arguments to registers (r0, r1, r2, ...)
    if (io_functions.get(c.callee)) |opcode| {
        for (c.args, 0..) |arg, i| {
            if (i < 8) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
        try e.emitOpcode(opcode);
        const argc: u8 = @intCast(c.args.len);
        try e.emitU8((0 << 4) | (argc & 0xF));
        try e.emitU8(0);
        return;
    }

    // Performance-critical functions with dedicated opcodes
    if (opcode_builtins.get(c.callee)) |builtin| {
        for (c.args, 0..) |arg, i| {
            if (i < 8) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
        try e.emitOpcode(builtin.opcode);
        // Emit operands for string operations: [rd:4|rs:4] [0]
        // Result goes to r0, source is in r0 (first argument)
        try e.emitU8(0x00); // rd=0, rs=0
        try e.emitU8(0x00); // padding
        if (c.result) |result| {
            e.setLastResult(result.id, 0);
        }
        return;
    }

    // User-defined functions take precedence over native functions
    // This allows users to shadow builtin names like "log" with their own functions
    if (e.function_indices.get(c.callee)) |routine_idx| {
        try emitUserCall(e, c, routine_idx);
        return;
    }

    // Native functions (via xcall) - only if no user function with same name
    if (native_functions.has(c.callee)) {
        try emitNativeCall(e, c);
        return;
    }

    // Dynamic call (function not found at compile time)
    try emitDynamicCall(e, c);
}

fn emitConsoleCall(e: *BytecodeEmitter, c: ir.Instruction.Call, opcode: Opcode) EmitError!void {
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    try e.emitOpcode(opcode);
    const argc: u8 = @intCast(c.args.len);
    try e.emitU8((0 << 4) | (argc & 0xF));
    try e.emitU8(0);
}

fn emitNativeCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Two-pass: first emit last_result args, then others
    if (e.last_result_value) |last_id| {
        for (c.args, 0..) |arg, i| {
            if (i < 8 and arg.id == last_id) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
    }
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            if (e.last_result_value) |last_id| {
                if (arg.id == last_id) continue;
            }
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    const name_idx = try e.addIdentifier(c.callee);
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(@intCast(c.args.len << 4));
    try e.emitU16(name_idx);
    if (c.result) |result| {
        e.setLastResult(result.id, 0);
    }
}

fn emitUserCall(e: *BytecodeEmitter, c: ir.Instruction.Call, routine_idx: u16) EmitError!void {
    // CRITICAL: Before making a call, spill any live intermediate value.
    // Calls clobber ALL registers - there are no callee-saved registers.
    // This is essential for expressions like `!f() and g()` where the result of !f()
    // might be in any register (e.g., r1 from log_not) and would be clobbered by g().
    log.debug("emitUserCall: callee={s} last_result_value={?d} last_result_reg={d}", .{
        c.callee,
        e.last_result_value,
        e.last_result_reg,
    });
    if (e.last_result_value) |prev_value_id| {
        // Spill the previous result regardless of which register it's in.
        // The callee can clobber any register during execution.
        if (e.spilled_values.get(prev_value_id) == null) {
            const spill_slot = e.allocateSpillSlot();
            log.debug("SPILL (pre-call): value_id={d} from r{d} to slot {d}", .{ prev_value_id, e.last_result_reg, spill_slot });
            try e.emitSpillStore(e.last_result_reg, spill_slot);
            try e.spilled_values.put(prev_value_id, spill_slot);
        }
        // Remove from register allocator so getValueInReg will reload from spill slot
        const spill_reg = e.last_result_reg;
        _ = e.reg_alloc.value_to_reg.remove(prev_value_id);
        e.reg_alloc.reg_to_value[spill_reg] = null;
        e.reg_alloc.free_regs |= @as(u16, 1) << spill_reg;
        // Invalidate last_result since we've spilled it
        e.last_result_value = null;
    }

    // Load arguments to registers
    // For struct arguments, we check if the callee expects a pointer-to-struct parameter:
    // - Pointer-to-struct param (*MyStruct): Pass a single StackPtr value via get_local_ptr.
    //   This is the standard mechanism for method calls (self: *MyStruct).
    // - Direct struct param with is_ref (DBL types): Expand all fields for copy-back semantics.
    // - Direct struct param without callee info: Fall back to field expansion.
    //
    // Get callee function to determine parameter types
    const callee_func_opt = blk: {
        if (e.ir_module) |ir_mod| {
            if (routine_idx < ir_mod.functions.items.len) {
                break :blk ir_mod.functions.items[routine_idx];
            }
        }
        break :blk null;
    };

    // First pass: calculate total slots needed to determine if we need stack overflow
    var total_slots: usize = 0;
    for (c.args, 0..) |arg, arg_idx| {
        // Check for struct type by value (NOT pointer-to-struct).
        // A pointer like *Expr is just 8 bytes regardless of what it points to.
        // Only actual struct values need multiple slots.
        const struct_type_opt: ?*const ir.StructType = if (arg.ty == .@"struct")
            arg.ty.@"struct"
        else
            null;
        if (struct_type_opt) |struct_type| {
            // Check if we'll use stack pointer for this arg (single slot instead of expansion)
            const callee_expects_ptr = blk: {
                if (callee_func_opt) |callee_func| {
                    if (arg_idx < callee_func.signature.params.len) {
                        const param_ty = callee_func.signature.params[arg_idx].ty;
                        if (param_ty == .ptr and param_ty.ptr.* == .@"struct") {
                            // Also check it's not is_ref (DBL needs expansion)
                            if (!callee_func.signature.params[arg_idx].is_ref) {
                                // Check if we have the struct in local slots
                                const has_slot = e.value_slots.contains(arg.id) or e.struct_base_slots.contains(arg.id);
                                if (has_slot) break :blk true;
                            }
                        }
                    }
                }
                break :blk false;
            };
            if (callee_expects_ptr) {
                total_slots += 1; // Stack pointer is a single value
            } else {
                const slot_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
                total_slots += slot_count;
            }
        } else {
            total_slots += 1;
        }
    }

    // Determine how many args go in registers vs stack
    // Note: argc field in call opcode is 4 bits (max 15), so max register args is 15
    // This means r0-r14 for args, with r15 available for return value
    const reg_argc: u8 = @intCast(@min(total_slots, 15));
    const stack_argc: u8 = if (total_slots > 15) @intCast(total_slots - 15) else 0;

    log.debug("emitUserCall: total_slots={d} reg_argc={d} stack_argc={d}", .{ total_slots, reg_argc, stack_argc });

    // If overflow, push overflow args to stack FIRST (in order: slot 15, 16, 17, ...)
    // These will be copied to callee's local slots 15+ by the call handler
    if (stack_argc > 0) {
        var slot_idx: usize = 0; // Track absolute slot position across all args
        for (c.args, 0..) |arg, arg_idx| {
            // Check for struct type by value (NOT pointer-to-struct, same as first pass)
            const struct_type_opt: ?*const ir.StructType = if (arg.ty == .@"struct")
                arg.ty.@"struct"
            else
                null;

            // Check if we're using stack pointer for this arg (consistent with first pass)
            const using_stack_ptr = blk: {
                if (struct_type_opt) |_| {
                    if (callee_func_opt) |callee_func| {
                        if (arg_idx < callee_func.signature.params.len) {
                            const param_ty = callee_func.signature.params[arg_idx].ty;
                            if (param_ty == .ptr and param_ty.ptr.* == .@"struct") {
                                if (!callee_func.signature.params[arg_idx].is_ref) {
                                    const has_slot = e.value_slots.contains(arg.id) or e.struct_base_slots.contains(arg.id);
                                    if (has_slot) break :blk true;
                                }
                            }
                        }
                    }
                }
                break :blk false;
            };

            const slot_count: usize = if (using_stack_ptr)
                1 // Stack pointer is a single value
            else if (struct_type_opt) |st|
                getSlotCount(ir.Type{ .@"struct" = st })
            else
                1;

            // For each slot of this arg, check if it goes to stack (slot_idx >= 15)
            for (0..slot_count) |field_offset| {
                const abs_slot = slot_idx + field_offset;
                if (abs_slot >= 15) {
                    // This slot goes to stack
                    if (using_stack_ptr) {
                        // Stack pointer - emit get_local_ptr and push
                        const base_slot = e.value_slots.get(arg.id) orelse e.struct_base_slots.get(arg.id).?;
                        try e.emitOpcode(.get_local_ptr);
                        try e.emitU8(0 << 4); // temp register r0
                        try e.emitU16(@intCast(base_slot));
                        try e.emitOpcode(.push_arg_reg);
                        try e.emitU8(0 << 4);
                        try e.emitU8(0);
                    } else {
                        // Check both value_slots (for loaded values) and struct_base_slots (for alloca'd structs)
                        const arg_base_slot = e.value_slots.get(arg.id) orelse e.struct_base_slots.get(arg.id);
                        if (arg_base_slot) |base_slot| {
                            const src_slot = base_slot + field_offset;
                            // push_arg format: [opcode] [slot:16] - 3 bytes total
                            try e.emitOpcode(.push_arg);
                            try e.emitU16(@intCast(src_slot));
                        } else {
                            // Value not in slot - load to temp register and push
                            try e.emitValueToReg(arg, 0);
                            try e.emitOpcode(.push_arg_reg);
                            try e.emitU8(0 << 4);
                            try e.emitU8(0);
                        }
                    }
                }
            }
            slot_idx += slot_count;
        }
    }

    // Load register arguments (slots 0-15) to r0-r15
    var reg_offset: u8 = 0;
    log.debug("emitUserCall: loading {d} args to registers", .{c.args.len});
    for (c.args, 0..) |arg, arg_idx| {
        log.debug("emitUserCall: processing arg[{d}] id={d} type={s}", .{ arg_idx, arg.id, @tagName(arg.ty) });
        // Check if this param is by-reference (was *struct, converted to struct with is_ref=true)
        const is_ref_param = blk: {
            if (callee_func_opt) |callee_func| {
                if (arg_idx < callee_func.signature.params.len) {
                    break :blk callee_func.signature.params[arg_idx].is_ref;
                }
            }
            break :blk false;
        };

        // Check if callee expects a pointer-to-struct parameter
        // If so, we should pass a stack pointer instead of expanding fields
        const callee_expects_ptr_to_struct = blk: {
            if (callee_func_opt) |callee_func| {
                if (arg_idx < callee_func.signature.params.len) {
                    const param_ty = callee_func.signature.params[arg_idx].ty;
                    if (param_ty == .ptr) {
                        if (param_ty.ptr.* == .@"struct") {
                            break :blk true;
                        }
                    }
                }
            }
            break :blk false;
        };

        // Check for struct type by value (NOT pointer-to-struct)
        // A pointer like *Expr is passed as a single 8-byte value.
        debug.print(.emit, "emitUserCall: arg[{d}] id={d} type={s}", .{
            arg_idx, arg.id, @tagName(arg.ty),
        });
        const struct_type_opt: ?*const ir.StructType = if (arg.ty == .@"struct")
            arg.ty.@"struct"
        else
            null;

        if (struct_type_opt) |struct_type| {
            const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
            log.debug("emitUserCall: arg[{d}] is struct type={s} field_count={d} arg.id={d}", .{
                arg_idx, struct_type.name, field_count, arg.id,
            });

            // Check both value_slots (for loaded values) and struct_base_slots (for alloca'd structs)
            const value_slot = e.value_slots.get(arg.id);
            const struct_slot = e.struct_base_slots.get(arg.id);
            log.debug("emitUserCall: arg[{d}] value_slots={?d} struct_base_slots={?d}", .{
                arg_idx, value_slot, struct_slot,
            });
            const struct_base_slot = value_slot orelse struct_slot;

            // If callee expects a pointer-to-struct and this is a LOCAL STACK-ALLOCATED struct,
            // emit get_local_ptr instead of expanding fields.
            // This allows passing large structs efficiently as a single value.
            // IMPORTANT: Only use stack pointer when struct_base_slots is set (actual stack struct).
            // value_slots alone means it's a heap pointer stored in a slot - pass it directly.
            // Exception: DBL records with is_ref need field expansion for copy-back semantics.
            const use_stack_ptr = callee_expects_ptr_to_struct and !is_ref_param and struct_slot != null;

            // Check if this is a heap pointer stored in value_slots (not a stack-allocated struct)
            // Heap pointers should be passed as single values, not expanded
            const is_heap_ptr = callee_expects_ptr_to_struct and value_slot != null and struct_slot == null;

            if (is_heap_ptr) {
                // Heap record pointer - load as single value
                log.debug("emitUserCall: heap pointer arg[{d}] at slot={d}", .{ arg_idx, value_slot.? });
                if (reg_offset < 15) {
                    if (value_slot.? < 256) {
                        try e.emitOpcode(.load_local);
                        try e.emitU8(reg_offset << 4);
                        try e.emitU8(@intCast(value_slot.?));
                    } else {
                        try e.emitOpcode(.load_local16);
                        try e.emitU8(reg_offset << 4);
                        try e.emitU16(value_slot.?);
                    }
                    reg_offset += 1;
                }
            } else if (use_stack_ptr) {
                const base_slot = struct_base_slot.?;
                log.debug("emitUserCall: using stack pointer for struct arg[{d}] base_slot={d}", .{
                    arg_idx, base_slot,
                });
                // Emit get_local_ptr to create a stack pointer value
                // Format: [rd:4|0] [slot:16]
                try e.emitOpcode(.get_local_ptr);
                try e.emitU8(reg_offset << 4);
                try e.emitU16(@intCast(base_slot));
                reg_offset += 1;
            } else if (struct_base_slot) |base_slot| {
                log.debug("emitUserCall: loading struct fields from base_slot={d} to regs starting at r{d}", .{
                    base_slot, reg_offset,
                });
                for (0..field_count) |field_idx| {
                    if (reg_offset >= 15) break; // Rest goes to stack (max 15 register args)
                    const slot = base_slot + field_idx;
                    if (slot < 256) {
                        try e.emitOpcode(.load_local);
                        try e.emitU8(reg_offset << 4);
                        try e.emitU8(@intCast(slot));
                    } else if (slot < 65536) {
                        try e.emitOpcode(.load_local16);
                        try e.emitU8(reg_offset << 4);
                        try e.emitU16(@intCast(slot));
                    }
                    reg_offset += 1;
                }
            } else {
                log.debug("emitUserCall: arg[{d}] struct NOT found in slots, using emitValueToReg", .{arg_idx});
                if (reg_offset < 15) {
                    try e.emitValueToReg(arg, @intCast(reg_offset));
                    const advance = @min(field_count, 15 - reg_offset);
                    reg_offset += @intCast(advance);
                }
            }
        } else {
            // Non-struct argument: load single value to single register
            if (reg_offset < 15) {
                try e.emitValueToReg(arg, @intCast(reg_offset));
                reg_offset += 1;
            }
        }
    }

    try e.emitOpcode(.call);
    // Format: [argc:4|stack_argc_low:4] [routine_idx:16] [stack_argc_ext:16 if stack_argc_low==15]
    // argc = number of register args (0-15)
    // If stack_argc_low < 15: stack_argc = stack_argc_low
    // If stack_argc_low == 15: stack_argc = next 16-bit value (extended format for >14 overflow args)
    if (stack_argc <= 14) {
        try e.emitU8((reg_argc << 4) | stack_argc);
        try e.emitU16(routine_idx);
    } else {
        // Extended format: use 15 as sentinel, then emit actual stack_argc as u16 after routine_idx
        try e.emitU8((reg_argc << 4) | 0x0F);
        try e.emitU16(routine_idx);
        try e.emitU16(stack_argc);
    }

    // Emit store-back for ref parameters and pointer-to-struct parameters
    // For struct parameters, we need to write back ALL fields, not just the first slot
    // For large structs (>16 slots), first 16 come from registers, rest from arg stack
    if (e.ir_module) |ir_mod| {
        if (routine_idx < ir_mod.functions.items.len) {
            const callee_func = ir_mod.functions.items[routine_idx];
            var param_reg_offset: usize = 0; // Track which register each param starts at
            for (callee_func.signature.params, 0..) |param, param_idx| {
                // Determine how many slots/registers this parameter uses (flattened for nested structs)
                const slot_count: usize = getSlotCount(param.ty);

                // Write back ONLY for is_ref parameters (DBL types with copy-back semantics)
                if (param.is_ref and param_idx < c.args.len) {
                    if (e.value_slots.get(c.args[param_idx].id)) |base_slot| {
                        // First: write back slots 0-15 from registers
                        for (0..slot_count) |field_idx| {
                            const reg = param_reg_offset + field_idx;
                            if (reg < 16) {
                                const slot = base_slot + field_idx;
                                if (slot <= 255) {
                                    try e.emitOpcode(.store_local);
                                    // store_local format: [rs:4|0:4] slot - register in high nibble
                                    try e.emitU8(@as(u8, @intCast(reg)) << 4);
                                    try e.emitU8(@intCast(slot));
                                } else if (slot <= 65535) {
                                    try e.emitOpcode(.store_local16);
                                    try e.emitU8(@as(u8, @intCast(reg)) << 4);
                                    try e.emitU16(@intCast(slot));
                                }
                            }
                        }
                        // Second: pop overflow slots (15+) from arg stack
                        // Callee pushed these in forward order, so we pop in reverse order
                        // to get them into the correct destination slots
                        var overflow_count: usize = 0;
                        for (0..slot_count) |field_idx| {
                            const reg = param_reg_offset + field_idx;
                            if (reg >= 15) overflow_count += 1;
                        }
                        if (overflow_count > 0) {
                            // Pop in reverse order (stack is LIFO)
                            var i: usize = slot_count;
                            while (i > 0) {
                                i -= 1;
                                const reg = param_reg_offset + i;
                                if (reg >= 15) {
                                    const slot = base_slot + i;
                                    // pop_arg format: [opcode] [slot:16] - 3 bytes total
                                    try e.emitOpcode(.pop_arg);
                                    try e.emitU16(@intCast(slot));
                                }
                            }
                        }
                    }
                }
                param_reg_offset += slot_count;
            }
        }
    }

    // Handle return value
    // For struct returns, the callee puts all fields into consecutive high registers
    // (r14, r15 for 2-field struct, etc.) - we need to store them all to result slots
    if (c.result) |result| {
        if (e.ir_module) |ir_mod| {
            if (routine_idx < ir_mod.functions.items.len) {
                const callee_func = ir_mod.functions.items[routine_idx];
                const return_type = callee_func.signature.return_type;

                debug.print(.emit, "emitUserCall: callee={s} return_type={s} result.id={d} result_slot={?d}", .{
                    c.callee,
                    @tagName(return_type),
                    result.id,
                    e.value_slots.get(result.id),
                });

                if (return_type == .@"struct") {
                    // Use flattened slot count for nested structs
                    const field_count = getSlotCount(return_type);

                    // Handle large structs that exceed register capacity
                    if (field_count > 16) {
                        // Large struct: return values are on the stack after ret with count
                        // Pop them into result slots
                        debug.print(.emit, "  large struct return: slot_count={d} > 16, using stack-based return", .{field_count});

                        // Allocate slots for the result if not already allocated
                        // Check both value_slots (for loaded values) and struct_base_slots (for alloca'd structs)
                        const result_base_slot = if (e.value_slots.get(result.id)) |slot|
                            slot
                        else if (e.struct_base_slots.get(result.id)) |slot|
                            slot
                        else blk: {
                            const base = e.local_count;
                            e.local_count += @intCast(field_count);
                            try e.value_slots.put(result.id, @intCast(base));
                            break :blk @as(u16, @intCast(base));
                        };

                        // Pop values from stack in reverse order (stack is LIFO)
                        // Values were pushed 0, 1, 2, ... n-1, so we pop n-1, n-2, ... 0
                        var i: usize = field_count;
                        while (i > 0) {
                            i -= 1;
                            const dst_slot = result_base_slot + i;
                            // pop_arg format: [opcode] [slot:16] - 3 bytes total
                            try e.emitOpcode(.pop_arg);
                            try e.emitU16(@intCast(dst_slot));
                        }
                        return;
                    }

                    const return_base_reg: u8 = 16 - @as(u8, @intCast(field_count));
                    debug.print(.emit, "  struct return: slot_count={d} return_base_reg={d}", .{ field_count, return_base_reg });

                    // For struct returns, set last_result so the following store instruction
                    // knows where to find the struct fields (in consecutive high registers)
                    // The store instruction will handle copying all fields to the destination slots
                    e.setLastResult(result.id, @intCast(return_base_reg));
                    return;
                }
            }
        }
        // Non-struct return: value is in r15
        // CRITICAL: Store the result to a slot immediately to preserve it across subsequent instructions.
        // Without this, expressions like `fib(n-1) + fib(n-2)` fail because:
        //   1. fib(n-1) returns in r15, set as last_result
        //   2. Computing n-2 (a sub instruction) overwrites last_result with reg=0
        //   3. fib(n-2) returns in r15
        //   4. When emitting the add, fib(n-1)'s result is lost (neither in last_result nor spilled)
        // By storing to a slot immediately, we ensure the value can be recovered via value_slots.
        const spill_slot = e.allocateSpillSlot();
        log.debug("STORE call result: value_id={d} from r15 to slot {d}", .{ result.id, spill_slot });
        try e.emitSpillStore(15, spill_slot);
        try e.value_slots.put(result.id, spill_slot);
        e.setLastResult(result.id, 15);
    }
}

fn emitDynamicCall(e: *BytecodeEmitter, c: ir.Instruction.Call) EmitError!void {
    // Two-pass argument loading
    if (e.last_result_value) |last_id| {
        for (c.args, 0..) |arg, i| {
            if (i < 8 and arg.id == last_id) {
                try e.emitValueToReg(arg, @intCast(i));
            }
        }
    }
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            if (e.last_result_value) |last_id| {
                if (arg.id == last_id) continue;
            }
            try e.emitValueToReg(arg, @intCast(i));
        }
    }
    const name_idx = try e.addIdentifier(c.callee);
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(@intCast(c.args.len << 4));
    try e.emitU16(name_idx);
    if (c.result) |result| {
        e.setLastResult(result.id, 0);
    }
}

/// Emit call_indirect instruction - call a closure/function pointer
/// Format: [rd:4|closure_reg:4] [argc:8]
pub fn emitCallIndirect(e: *BytecodeEmitter, c: ir.Instruction.CallIndirect) EmitError!void {
    // Load the callee (closure) into register 8 (avoid arg registers 0-7)
    const closure_reg: u4 = 8;
    try e.emitValueToReg(c.callee, closure_reg);

    // Load arguments to registers 0-7
    for (c.args, 0..) |arg, i| {
        if (i < 8) {
            try e.emitValueToReg(arg, @intCast(i));
        }
    }

    // Emit call_closure: [opcode] [rd:4|closure_reg:4] [argc:8]
    try e.emitOpcode(.call_closure);
    const rd: u4 = 15; // Return value register
    try e.emitU8((@as(u8, rd) << 4) | closure_reg);
    try e.emitU8(@intCast(c.args.len));

    if (c.result) |result| {
        e.setLastResult(result.id, rd);
    }
}

// ============================================================================
// I/O Operations
// ============================================================================

/// Emit io_open instruction
pub fn emitIoOpen(e: *BytecodeEmitter, io: ir.Instruction.IoOpen) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    try e.emitValueToReg(io.filename, 1);
    const name_idx = try e.addIdentifier("open");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(2 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_close instruction
pub fn emitIoClose(e: *BytecodeEmitter, io: IoClose) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    const name_idx = try e.addIdentifier("close");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(1 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_delete instruction
pub fn emitIoDelete(e: *BytecodeEmitter, io: IoDelete) EmitError!void {
    try e.emitValueToReg(io.channel, 0);
    const name_idx = try e.addIdentifier("delete");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(1 << 4);
    try e.emitU16(name_idx);
}

/// Emit io_read instruction
pub fn emitIoRead(e: *BytecodeEmitter, r: ir.Instruction.IoRead) EmitError!void {
    try e.emitValueToReg(r.channel, 0);
    if (r.key) |key| {
        // Keyed read: read(cursor_id, key_num, key_value)
        // Load key_num as constant to r1
        const key_num_idx = try e.addConstant(.{ .integer = r.qualifiers.key_index });
        try e.emitOpcode(.load_const);
        try e.emitU8(1); // r1 as destination
        try e.emitU16(key_num_idx);
        // Load key value to r2
        try e.emitValueToReg(key, 2);
        const name_idx = try e.addIdentifier("read");
        try e.emitOpcode(.call_dynamic);
        try e.emitU8(3 << 4);
        try e.emitU16(name_idx);
    } else {
        // Sequential read: reads(cursor_id)
        const name_idx = try e.addIdentifier("reads");
        try e.emitOpcode(.call_dynamic);
        try e.emitU8(1 << 4);
        try e.emitU16(name_idx);
    }
    // If this is a structure read, emit store_record_buf to distribute to locals
    if (r.struct_name) |struct_name| {
        try e.emitStoreRecordBuf(struct_name, r.base_name);
    }
}

/// Emit io_write instruction
pub fn emitIoWrite(e: *BytecodeEmitter, w: ir.Instruction.IoWrite) EmitError!void {
    try e.emitValueToReg(w.channel, 0);
    try e.emitValueToReg(w.buffer, 1);
    const name_idx = try e.addIdentifier(if (w.is_insert) "store" else "write");
    try e.emitOpcode(.call_dynamic);
    try e.emitU8(2 << 4);
    try e.emitU16(name_idx);
}

// ============================================================================
// Struct Buffer Operations
// ============================================================================

/// Emit load_struct_buf instruction
/// Format: [rd:4|flags:4] [type_idx:16] [base:16]
/// flags: 0=locals, 1=globals
pub fn emitLoadStructBuf(e: *BytecodeEmitter, sb: LoadStructBuf) EmitError!void {
    var type_idx: u16 = 0;
    var found = false;
    var type_def: ?*const module.TypeDef = null;
    debug.print(.emit, "load_struct_buf: base='{s}' struct='{s}', types.len={d}", .{ sb.base_name, sb.struct_name, e.types.items.len });

    for (e.types.items, 0..) |*t, i| {
        if (t.name_index < e.constants.items.len) {
            const name_const = e.constants.items[t.name_index];
            const type_name = switch (name_const) {
                .string => |s| s,
                .identifier => |s| s,
                else => continue,
            };
            debug.print(.emit, "  type[{d}] name='{s}'", .{ i, type_name });
            if (std.mem.eql(u8, type_name, sb.struct_name)) {
                type_idx = @intCast(i);
                type_def = t;
                found = true;
                break;
            }
        }
    }
    if (!found) {
        debug.print(.emit, "WARNING: struct '{s}' not found in types!", .{sb.struct_name});
    }

    var base: u16 = 0;
    var is_global: bool = false;
    if (type_def) |td| {
        if (td.fields.len > 0) {
            const first_field = td.fields[0];
            if (first_field.name_index < e.constants.items.len) {
                const field_name_const = e.constants.items[first_field.name_index];
                const field_name = switch (field_name_const) {
                    .string => |s| s,
                    .identifier => |s| s,
                    else => "",
                };
                debug.print(.emit, "load_struct_buf: looking up first field '{s}'", .{field_name});

                // Try locals first
                if (e.locals.get(field_name)) |loc| {
                    base = loc.slot;
                    is_global = false;
                    debug.print(.emit, "load_struct_buf: base={d} (local)", .{base});
                } else if (e.globals.get(field_name)) |glob| {
                    base = glob.slot;
                    is_global = true;
                    debug.print(.emit, "load_struct_buf: base={d} (global)", .{base});
                } else {
                    // Try qualified names
                    var qualified_buf: [256]u8 = undefined;
                    const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ sb.base_name, field_name }) catch "";
                    debug.print(.emit, "load_struct_buf: trying qualified name '{s}'", .{qualified_name});
                    if (e.locals.get(qualified_name)) |loc| {
                        base = loc.slot;
                        is_global = false;
                        debug.print(.emit, "load_struct_buf: base={d} (local qualified)", .{base});
                    } else if (e.globals.get(qualified_name)) |glob| {
                        base = glob.slot;
                        is_global = true;
                        debug.print(.emit, "load_struct_buf: base={d} (global qualified)", .{base});
                    } else {
                        debug.print(.emit, "WARNING: first field '{s}' not found!", .{field_name});
                    }
                }
            }
        }
    }

    const rd = try e.getOrAllocReg(sb.result);
    const flags: u4 = if (is_global) 1 else 0;
    debug.print(.emit, "load_struct_buf: rd=r{d} flags={d} (is_global={any})", .{ rd, flags, is_global });
    try e.emitOpcode(.load_record_buf);
    try e.emitU8((@as(u8, rd) << 4) | flags);
    try e.emitU16(type_idx);
    try e.emitU16(base);
    e.last_result_value = sb.result.id;
    e.last_result_reg = rd;
}

/// Emit store_struct_buf instruction
pub fn emitStoreStructBuf(e: *BytecodeEmitter, sb: StoreStructBuf) EmitError!void {
    try e.emitValueToReg(sb.value, 0);
    debug.print(.emit, "store_struct_buf: base='{s}' struct='{s}'", .{ sb.base_name, sb.struct_name });
    try e.emitStoreRecordBuf(sb.struct_name, sb.base_name);
}

// ============================================================================
// Debug
// ============================================================================

/// Emit debug_line instruction
/// Format: [opcode][0][line_lo][line_hi] = 4 bytes
pub fn emitDebugLine(e: *BytecodeEmitter, d: DebugLine) EmitError!void {
    try e.emitOpcode(.debug_line);
    try e.emitU8(0);
    try e.emitU16(@intCast(d.line));
}

// ============================================================================
// Exception Handling
// ============================================================================

/// Emit try_begin instruction
pub fn emitTryBegin(e: *BytecodeEmitter, t: ir.Instruction.TryBegin) EmitError!void {
    try e.emitOpcode(.set_error_handler);
    try e.addPendingJump(t.catch_block, false);
    try e.emitI16(0);
}

/// Emit try_end instruction
pub fn emitTryEnd(e: *BytecodeEmitter) EmitError!void {
    try e.emitOpcode(.clear_error_handler);
}

/// Emit catch_begin instruction
/// If error_value is provided, stores r0 (the caught error) into that variable
pub fn emitCatchBegin(e: *BytecodeEmitter, c: ir.Instruction.CatchBegin) EmitError!void {
    if (c.error_value) |err_val| {
        // The caught exception is in r0, store it to the error binding variable
        // Get the slot for the error variable
        if (e.value_slots.get(err_val.id)) |slot| {
            debug.print(.emit, "catch_begin: storing r0 to slot {d} for error binding (value_id={d})", .{ slot, err_val.id });
            // store_local slot, r0
            try e.emitOpcode(.store_local);
            try e.emitU8(0); // r0 contains the error
            try e.emitU8(@intCast(slot & 0xFF));
        } else {
            debug.print(.emit, "catch_begin: error binding value_id={d} has no slot, emitting nop", .{err_val.id});
            // Should not happen, but emit nop as fallback
            try e.emitOpcode(.nop);
        }
    } else {
        try e.emitOpcode(.nop);
    }
}

/// Emit throw instruction
pub fn emitThrow(e: *BytecodeEmitter, t: ir.Instruction.Throw) EmitError!void {
    // Load exception value into r0
    try e.emitValueToReg(t.value, 0);
    // Emit throw opcode - VM will jump to current error handler
    try e.emitOpcode(.throw);
    try e.emitU8(0); // rs=0 (exception value in r0), unused nibble
    try e.emitU8(0); // padding byte
}

// ============================================================================
// Array Operations
// ============================================================================

/// Emit array_load instruction
pub fn emitArrayLoad(e: *BytecodeEmitter, al: ir.Instruction.ArrayOp) EmitError!void {
    // First check array_ptr_targets for pointer indirection
    // This handles cases like: arr = [1,2,3]; x = arr[i]
    // where arr is a pointer to the actual array
    const array_slot: u16 = if (e.array_ptr_targets.get(al.array_ptr.id)) |slot|
        slot
    else if (e.value_slots.get(al.array_ptr.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    debug.print(.emit, "array_load: ptr.id={d} -> array_slot={d}", .{ al.array_ptr.id, array_slot });

    try e.emitValueToReg(al.index, 0);
    try e.emitOpcode(.array_load);
    try e.emitU8(0);
    try e.emitU16(array_slot);
    e.setLastResult(al.result.id, 0);
}

/// Emit array_load_opt instruction (optional indexing - returns null on out-of-bounds)
pub fn emitArrayLoadOpt(e: *BytecodeEmitter, al: ir.Instruction.ArrayOp) EmitError!void {
    const array_slot: u16 = if (e.array_ptr_targets.get(al.array_ptr.id)) |slot|
        slot
    else if (e.value_slots.get(al.array_ptr.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    // Get the array length from the type
    const array_len: u16 = switch (al.array_ptr.ty) {
        .array => |a| @intCast(a.length),
        .ptr => |p| switch (p.*) {
            .array => |a| @intCast(a.length),
            else => 0,
        },
        else => 0,
    };

    debug.print(.emit, "array_load_opt: ptr.id={d} -> array_slot={d}, len={d}", .{ al.array_ptr.id, array_slot, array_len });

    try e.emitValueToReg(al.index, 0);
    try e.emitOpcode(.array_load_opt);
    try e.emitU8(0);
    try e.emitU16(array_slot);
    try e.emitU16(array_len);
    e.setLastResult(al.result.id, 0);
}

/// Emit array_store instruction
pub fn emitArrayStore(e: *BytecodeEmitter, as: ir.Instruction.ArrayStore) EmitError!void {
    // First check array_ptr_targets for pointer indirection
    const array_slot: u16 = if (e.array_ptr_targets.get(as.array_ptr.id)) |slot|
        slot
    else if (e.value_slots.get(as.array_ptr.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    try e.emitValueToReg(as.index, 0);
    try e.emitValueToReg(as.value, 1);
    try e.emitOpcode(.array_store);
    try e.emitU8((0 << 4) | 1);
    try e.emitU16(array_slot);
}

/// Emit array_len instruction
/// For fixed-size arrays, the length is known at compile time from the type
pub fn emitArrayLen(e: *BytecodeEmitter, al: ir.Instruction.UnaryOp) EmitError!void {
    // Get the array type to extract the length
    const operand_ty = al.operand.ty;

    // Determine the array length from the type
    const length: u32 = switch (operand_ty) {
        .array => |arr| arr.length,
        .ptr => |ptr_ty| switch (ptr_ty.*) {
            .array => |arr| arr.length,
            else => 0,
        },
        else => 0,
    };

    // Emit the length as a constant
    const idx = try e.addConstant(.{ .integer = @intCast(length) });
    try e.emitOpcode(.load_const);
    try e.emitU8(0); // Load into r0
    try e.emitU16(idx);
    e.setLastResult(al.result.id, 0);
}

/// Emit array_slice or str_slice instruction depending on source type
/// For arrays: Format: [rd:4|inclusive:1|0:3] [start_reg:4|end_reg:4] [slot_lo:8] [slot_hi:8]
/// For strings: Format: [dest_reg:4|src_reg:4] [start_reg:4|end_reg:4] [is_length:8]
pub fn emitArraySlice(e: *BytecodeEmitter, as: ir.Instruction.ArraySlice) EmitError!void {
    // Check if source is a string type - use str_slice instead of array_slice
    if (as.source.isString()) {
        debug.print(.emit, "str_slice: source.id={d} source.ty={}", .{ as.source.id, as.source.ty });

        // Load source string into register (r3)
        try e.emitValueToReg(as.source, 3);
        // Load start index into r1
        try e.emitValueToReg(as.start, 1);
        // Load end index into r2
        try e.emitValueToReg(as.end, 2);

        // For inclusive slicing (..=), we need to add 1 to the end index to make it exclusive
        // The str_slice opcode expects exclusive end index when is_length=0
        if (as.inclusive) {
            // end = end + 1
            try e.emitOpcode(.add);
            try e.emitU8((2 << 4) | 2); // rd=2, rs1=2
            try e.emitU8(0); // rs2=0 (will load constant 1)
            // Load constant 1 into r4 and add
            const one_idx = try e.addConstant(.{ .integer = 1 });
            try e.emitOpcode(.load_const);
            try e.emitU8(4); // r4
            try e.emitU16(one_idx);
            try e.emitOpcode(.add);
            try e.emitU8((2 << 4) | 2); // rd=2, rs1=2
            try e.emitU8(4); // rs2=4
        }

        // Emit: str_slice rd=0, rs=3, start_reg=1, end_reg=2, is_length=0
        // Format: [dest_reg:4|src_reg:4] [start_reg:4|end_reg:4] [is_length:8]
        try e.emitOpcode(.str_slice);
        try e.emitU8((0 << 4) | 3); // dest_reg=0, src_reg=3
        try e.emitU8((1 << 4) | 2); // start_reg=1, end_reg=2
        try e.emitU8(0); // is_length=0 (Cot mode: end is exclusive index)

        e.setLastResult(as.result.id, 0);
        return;
    }

    // Array slicing: Look up the source array's stack slot (same logic as array_load)
    const array_slot: u16 = if (e.array_ptr_targets.get(as.source.id)) |slot|
        slot
    else if (e.value_slots.get(as.source.id)) |slot_info|
        slot_info & 0x7FFF
    else
        0;

    debug.print(.emit, "array_slice: source.id={d} -> array_slot={d} inclusive={}", .{ as.source.id, array_slot, as.inclusive });

    // Load start and end indices into registers
    try e.emitValueToReg(as.start, 1);
    try e.emitValueToReg(as.end, 2);

    // Emit: array_slice rd=0, start_reg=1, end_reg=2, slot
    // Encode inclusive flag in bit 3 of first byte
    const inclusive_flag: u8 = if (as.inclusive) 0x08 else 0x00;
    try e.emitOpcode(.array_slice);
    try e.emitU8((0 << 4) | inclusive_flag); // rd=0, inclusive flag in bit 3
    try e.emitU8((1 << 4) | 2); // start_reg=1, end_reg=2
    try e.emitU16(array_slot);

    e.setLastResult(as.result.id, 0);
}

// ============================================================================
// Switch/Branch Table
// ============================================================================

/// Emit br_table instruction
pub fn emitBrTable(e: *BytecodeEmitter, s: ir.Instruction.Switch) EmitError!void {
    const val_reg = try e.getValueInReg(s.value, 0);
    _ = val_reg;

    for (s.cases) |case| {
        const case_idx = try e.addConstant(.{ .integer = case.value });
        // load_const format: [rd << 4] [idx_lo] [idx_hi]
        // Load case constant into r1
        try e.emitOpcode(.load_const);
        try e.emitU8(1 << 4); // dest=r1 (in high nibble)
        try e.emitU16(case_idx);

        // cmp_eq format: [dest << 4 | src1] [src2 << 4]
        // Compare r0 (scrutinee) with r1 (case constant), result in r2
        try e.emitOpcode(.cmp_eq);
        try e.emitU8((2 << 4) | 0); // dest=r2, src1=r0
        try e.emitU8(1 << 4); // src2=r1

        try e.emitRegCondJmp(.jnz, 2, case.target);
    }

    try e.emitRegJmp(s.default);
}

// ============================================================================
// Decimal Formatting
// ============================================================================

/// Emit format_decimal instruction
pub fn emitFormatDecimal(e: *BytecodeEmitter, fd: ir.Instruction.FormatDecimal) EmitError!void {
    const src_reg = try e.getValueInReg(fd.value, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.format_decimal);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(@intCast(fd.width));
    e.setLastResult(fd.result.id, dest_reg);
}

/// Emit parse_decimal instruction
pub fn emitParseDecimal(e: *BytecodeEmitter, pd: ir.Instruction.ParseDecimal) EmitError!void {
    const src_reg = try e.getValueInReg(pd.value, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.parse_decimal);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(0);
    e.setLastResult(pd.result.id, dest_reg);
}

// ============================================================================
// Map Operations
// ============================================================================

/// Emit map_new instruction
pub fn emitMapNew(e: *BytecodeEmitter, mn: ir.Instruction.MapNew) EmitError!void {
    const dest_reg: u4 = 0;
    // Spill any existing value in r0 before overwriting
    e.prepareDestReg(dest_reg);
    try e.emitOpcode(.map_new);
    try e.emitU8((@as(u8, dest_reg) << 4) | (mn.flags & 0x0F));
    try e.emitU8(0);
    e.setLastResult(mn.result.id, dest_reg);
}

/// Emit map_set instruction
pub fn emitMapSet(e: *BytecodeEmitter, ms: ir.Instruction.MapSet) EmitError!void {
    // Check if this is a struct value being stored
    const struct_type_opt: ?*const ir.StructType = if (ms.value.ty == .@"struct")
        ms.value.ty.@"struct"
    else if (ms.value.ty == .ptr and ms.value.ty.ptr.* == .@"struct")
        ms.value.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct set: emit map_set_struct with base_slot and field_count
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        if (e.value_slots.get(ms.value.id)) |base_slot| {
            const map_reg = try e.getValueInReg(ms.map, 0);
            const key_reg = try e.getValueInReg(ms.key, 1);
            debug.print(.emit, "map_set_struct: map_reg={d} key_reg={d} field_count={d} base_slot={d}", .{
                map_reg, key_reg, field_count, base_slot & 0x7FFF,
            });
            // Format: [map:4|key:4] [field_count:8] [base_slot:16]
            try e.emitOpcode(.map_set_struct);
            try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
            try e.emitU8(@intCast(field_count));
            try e.emitU16(@intCast(base_slot & 0x7FFF));
            return;
        }
    }

    // Non-struct or no slot: use regular map_set
    const map_reg = try e.getValueInReg(ms.map, 0);
    const key_reg = try e.getValueInReg(ms.key, 1);
    const val_reg = try e.getValueInReg(ms.value, 2);
    try e.emitOpcode(.map_set);
    try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
    try e.emitU8((@as(u8, val_reg) << 4) | 0);
}

/// Emit map_get instruction
/// For struct value types, emits map_get_struct to expand to high registers.
pub fn emitMapGet(e: *BytecodeEmitter, mg: ir.Instruction.MapGet) EmitError!void {
    // Check if the result type is a struct
    const struct_type_opt: ?*const ir.StructType = if (mg.result.ty == .@"struct")
        mg.result.ty.@"struct"
    else if (mg.result.ty == .ptr and mg.result.ty.ptr.* == .@"struct")
        mg.result.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct get: emit map_get_struct to expand to high registers
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        const map_reg = try e.getValueInReg(mg.map, 0);
        const key_reg = try e.getValueInReg(mg.key, 1);

        // Use high registers like function returns: for 2 fields use r14, r15
        const return_base_reg: u8 = 16 - @as(u8, @intCast(field_count));
        debug.print(.emit, "map_get_struct: map_reg={d} key_reg={d} field_count={d} return_base_reg={d}", .{
            map_reg, key_reg, field_count, return_base_reg,
        });

        // Format: [map:4|key:4] [field_count:8] [base_reg:8] [0]
        try e.emitOpcode(.map_get_struct);
        try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
        try e.emitU8(@intCast(field_count));
        try e.emitU8(return_base_reg);
        try e.emitU8(0);

        // Set last_result so emitStore knows where to find the struct fields
        e.setLastResult(mg.result.id, @intCast(return_base_reg));
        return;
    }

    // Non-struct: use regular map_get
    const map_reg = try e.getValueInReg(mg.map, 0);
    const key_reg = try e.getValueInReg(mg.key, 1);
    const dest_reg: u4 = 2;
    try e.emitOpcode(.map_get);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8((@as(u8, key_reg) << 4) | 0);
    e.setLastResult(mg.result.id, dest_reg);
}

/// Emit map_delete instruction
pub fn emitMapDelete(e: *BytecodeEmitter, md: ir.Instruction.MapDelete) EmitError!void {
    const map_reg = try e.getValueInReg(md.map, 0);
    const key_reg = try e.getValueInReg(md.key, 1);
    try e.emitOpcode(.map_delete);
    try e.emitU8((@as(u8, map_reg) << 4) | key_reg);
    try e.emitU8(0);
}

/// Emit map_has instruction
pub fn emitMapHas(e: *BytecodeEmitter, mh: ir.Instruction.MapHas) EmitError!void {
    const map_reg = try e.getValueInReg(mh.map, 0);
    const key_reg = try e.getValueInReg(mh.key, 1);
    const dest_reg: u4 = 2;
    try e.emitOpcode(.map_has);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8((@as(u8, key_reg) << 4) | 0);
    e.setLastResult(mh.result.id, dest_reg);
}

/// Emit map_len instruction
pub fn emitMapLen(e: *BytecodeEmitter, ml: ir.Instruction.MapLen) EmitError!void {
    const map_reg = try e.getValueInReg(ml.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_len);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(ml.result.id, dest_reg);
}

/// Emit map_clear instruction
pub fn emitMapClear(e: *BytecodeEmitter, mc: ir.Instruction.MapClear) EmitError!void {
    const map_reg = try e.getValueInReg(mc.map, 0);
    try e.emitOpcode(.map_clear);
    try e.emitU8((@as(u8, map_reg) << 4) | 0);
    try e.emitU8(0);
}

/// Emit map_keys instruction
pub fn emitMapKeys(e: *BytecodeEmitter, mk: ir.Instruction.MapKeys) EmitError!void {
    const map_reg = try e.getValueInReg(mk.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_keys);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(mk.result.id, dest_reg);
}

/// Emit map_values instruction
pub fn emitMapValues(e: *BytecodeEmitter, mv: ir.Instruction.MapValues) EmitError!void {
    const map_reg = try e.getValueInReg(mv.map, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.map_values);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(0);
    e.setLastResult(mv.result.id, dest_reg);
}

/// Emit map_key_at instruction - get key at index position
pub fn emitMapKeyAt(e: *BytecodeEmitter, mk: ir.Instruction.MapKeyAt) EmitError!void {
    const map_reg = try e.getValueInReg(mk.map, 0);
    const idx_reg = try e.getValueInReg(mk.index, 1);
    const dest_reg: u4 = 2;
    try e.emitOpcode(.map_key_at);
    try e.emitU8((@as(u8, dest_reg) << 4) | map_reg);
    try e.emitU8(@as(u8, idx_reg) << 4);
    e.setLastResult(mk.result.id, dest_reg);
}

// ============================================================================
// List Operations
// ============================================================================

/// Emit list_new instruction
pub fn emitListNew(e: *BytecodeEmitter, ln: ir.Instruction.ListNew) EmitError!void {
    const dest_reg: u4 = 0;
    // Spill any existing value in r0 before overwriting
    e.prepareDestReg(dest_reg);
    try e.emitOpcode(.list_new);
    try e.emitU8((@as(u8, dest_reg) << 4) | 0);
    try e.emitU8(0);
    e.setLastResult(ln.result.id, dest_reg);
}

/// Emit list_push instruction
/// For struct types, emits list_push_struct to handle multi-slot values.
pub fn emitListPush(e: *BytecodeEmitter, lp: ir.Instruction.ListPush) EmitError!void {
    // Check if this is a struct value being pushed
    const struct_type_opt: ?*const ir.StructType = if (lp.value.ty == .@"struct")
        lp.value.ty.@"struct"
    else if (lp.value.ty == .ptr and lp.value.ty.ptr.* == .@"struct")
        lp.value.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct push: emit list_push_struct with base_slot and field_count
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        if (e.value_slots.get(lp.value.id)) |base_slot| {
            const list_reg = try e.getValueInReg(lp.list, 0);
            debug.print(.emit, "list_push_struct: list_reg={d} field_count={d} base_slot={d}", .{
                list_reg, field_count, base_slot & 0x7FFF,
            });
            // Format: [list:4|0] [field_count:8] [base_slot:16]
            try e.emitOpcode(.list_push_struct);
            try e.emitU8(@as(u8, list_reg) << 4);
            try e.emitU8(@intCast(field_count));
            try e.emitU16(@intCast(base_slot & 0x7FFF));
            return;
        }
    }

    // Non-struct or no slot: use regular list_push
    const list_reg = try e.getValueInReg(lp.list, 0);
    const val_reg = try e.getValueInReg(lp.value, 1);
    try e.emitOpcode(.list_push);
    try e.emitU8((@as(u8, list_reg) << 4) | val_reg);
    try e.emitU8(0);
}

/// Emit list_pop instruction
/// For struct element types, emits list_pop_struct to expand to consecutive slots.
pub fn emitListPop(e: *BytecodeEmitter, lp: ir.Instruction.ListPop) EmitError!void {
    // Check if the result type is a struct
    const struct_type_opt: ?*const ir.StructType = if (lp.result.ty == .@"struct")
        lp.result.ty.@"struct"
    else if (lp.result.ty == .ptr and lp.result.ty.ptr.* == .@"struct")
        lp.result.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct pop: emit list_pop_struct to expand to consecutive slots
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        if (e.value_slots.get(lp.result.id)) |dest_slot| {
            const list_reg = try e.getValueInReg(lp.list, 0);
            debug.print(.emit, "list_pop_struct: list_reg={d} field_count={d} dest_slot={d}", .{
                list_reg, field_count, dest_slot & 0x7FFF,
            });
            // Format: [list:4|0] [field_count:8] [dest_slot:16]
            try e.emitOpcode(.list_pop_struct);
            try e.emitU8(@as(u8, list_reg) << 4);
            try e.emitU8(@intCast(field_count));
            try e.emitU16(@intCast(dest_slot & 0x7FFF));
            return;
        }
    }

    // Non-struct: use regular list_pop
    const list_reg = try e.getValueInReg(lp.list, 0);
    const dest_reg: u4 = 1;
    // Spill any existing value in dest_reg BEFORE the instruction clobbers it
    e.prepareDestReg(dest_reg);
    try e.emitOpcode(.list_pop);
    try e.emitU8((@as(u8, dest_reg) << 4) | list_reg);
    try e.emitU8(0);
    e.setLastResult(lp.result.id, dest_reg);
}

/// Emit list_get instruction
/// For struct element types, emits list_get_struct which writes directly to slots.
/// This avoids the 16-register limitation by using slot-based storage.
pub fn emitListGet(e: *BytecodeEmitter, lg: ir.Instruction.ListGet) EmitError!void {
    // Check if the result type is a struct
    const struct_type_opt: ?*const ir.StructType = if (lg.result.ty == .@"struct")
        lg.result.ty.@"struct"
    else if (lg.result.ty == .ptr and lg.result.ty.ptr.* == .@"struct")
        lg.result.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct get: emit list_get_struct to write directly to slots
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        const list_reg = try e.getValueInReg(lg.list, 0);
        const idx_reg = try e.getValueInReg(lg.index, 1);

        // Allocate slots for the struct result
        const dest_slot = try e.allocateSlots(lg.result.id, field_count);
        debug.print(.emit, "list_get_struct: list_reg={d} idx_reg={d} field_count={d} dest_slot={d}", .{
            list_reg, idx_reg, field_count, dest_slot,
        });

        // Format: [list:4|idx:4] [field_count:8] [dest_slot:16]
        try e.emitOpcode(.list_get_struct);
        try e.emitU8((@as(u8, list_reg) << 4) | idx_reg);
        try e.emitU8(@intCast(field_count));
        try e.emitU16(@intCast(dest_slot));

        // Store slot mapping so field access can find the struct fields
        try e.value_slots.put(lg.result.id, @intCast(dest_slot));
        return;
    }

    // Non-struct: use regular list_get
    const list_reg = try e.getValueInReg(lg.list, 0);
    const idx_reg = try e.getValueInReg(lg.index, 1);
    const dest_reg: u4 = 2;
    // Spill any existing value in dest_reg BEFORE the instruction clobbers it
    e.prepareDestReg(dest_reg);
    try e.emitOpcode(.list_get);
    try e.emitU8((@as(u8, dest_reg) << 4) | list_reg);
    try e.emitU8((@as(u8, idx_reg) << 4) | 0);
    e.setLastResult(lg.result.id, dest_reg);
}

/// Emit list_set instruction
/// For struct element types, emits list_set_struct to pack from consecutive slots.
pub fn emitListSet(e: *BytecodeEmitter, ls: ir.Instruction.ListSet) EmitError!void {
    // Check if the value type is a struct
    const struct_type_opt: ?*const ir.StructType = if (ls.value.ty == .@"struct")
        ls.value.ty.@"struct"
    else if (ls.value.ty == .ptr and ls.value.ty.ptr.* == .@"struct")
        ls.value.ty.ptr.*.@"struct"
    else
        null;

    if (struct_type_opt) |struct_type| {
        // Struct set: emit list_set_struct with base_slot and field_count
        const field_count = getSlotCount(ir.Type{ .@"struct" = struct_type });
        if (e.value_slots.get(ls.value.id)) |base_slot| {
            const list_reg = try e.getValueInReg(ls.list, 0);
            const idx_reg = try e.getValueInReg(ls.index, 1);
            debug.print(.emit, "list_set_struct: list_reg={d} idx_reg={d} field_count={d} base_slot={d}", .{
                list_reg, idx_reg, field_count, base_slot & 0x7FFF,
            });
            // Format: [list:4|idx:4] [field_count:8] [base_slot:16]
            try e.emitOpcode(.list_set_struct);
            try e.emitU8((@as(u8, list_reg) << 4) | idx_reg);
            try e.emitU8(@intCast(field_count));
            try e.emitU16(@intCast(base_slot & 0x7FFF));
            return;
        }
    }

    // Non-struct: use regular list_set
    const list_reg = try e.getValueInReg(ls.list, 0);
    const idx_reg = try e.getValueInReg(ls.index, 1);
    const val_reg = try e.getValueInReg(ls.value, 2);
    try e.emitOpcode(.list_set);
    try e.emitU8((@as(u8, list_reg) << 4) | idx_reg);
    try e.emitU8((@as(u8, val_reg) << 4) | 0);
}

/// Emit list_len instruction
pub fn emitListLen(e: *BytecodeEmitter, ll: ir.Instruction.ListLen) EmitError!void {
    const list_reg = try e.getValueInReg(ll.list, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.list_len);
    try e.emitU8((@as(u8, dest_reg) << 4) | list_reg);
    try e.emitU8(0);
    e.setLastResult(ll.result.id, dest_reg);
}

/// Emit list_clear instruction
pub fn emitListClear(e: *BytecodeEmitter, lc: ir.Instruction.ListClear) EmitError!void {
    const list_reg = try e.getValueInReg(lc.list, 0);
    try e.emitOpcode(.list_clear);
    try e.emitU8((@as(u8, list_reg) << 4) | 0);
    try e.emitU8(0);
}

/// Emit list_to_slice instruction: converts List<T> to []T
pub fn emitListToSlice(e: *BytecodeEmitter, lts: ir.Instruction.ListToSlice) EmitError!void {
    const list_reg = try e.getValueInReg(lts.list, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.list_to_slice);
    try e.emitU8((@as(u8, dest_reg) << 4) | list_reg);
    try e.emitU8(0);
    e.setLastResult(lts.result.id, dest_reg);
}

// ============================================================================
// Null/Optional Operations
// ============================================================================

/// Emit is_null instruction: rd = (rs == null)
pub fn emitIsNull(e: *BytecodeEmitter, n: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(n.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.is_null);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(0);
    e.setLastResult(n.result.id, dest_reg);
}

/// Emit is_type instruction: rd = (rs is type_tag)
pub fn emitIsType(e: *BytecodeEmitter, op: ir.Instruction.IsTypeOp) EmitError!void {
    const src_reg = try e.getValueInReg(op.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.is_type);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(@intFromEnum(op.type_tag));
    e.setLastResult(op.result.id, dest_reg);
}

/// Emit select instruction: rd = cond ? rtrue : rfalse
pub fn emitSelect(e: *BytecodeEmitter, s: ir.Instruction.Select) EmitError!void {
    const cond_reg = try e.getValueInReg(s.condition, 0);
    const true_reg = try e.getValueInReg(s.true_val, 1);
    const false_reg = try e.getValueInReg(s.false_val, 2);
    const dest_reg: u4 = 3;
    try e.emitOpcode(.select);
    try e.emitU8((@as(u8, dest_reg) << 4) | cond_reg);
    try e.emitU8((@as(u8, true_reg) << 4) | false_reg);
    e.setLastResult(s.result.id, dest_reg);
}

/// Emit ptr_offset instruction: rd = base + offset (byte-level pointer arithmetic)
/// For DBL field views, this records FieldViewInfo for write-through semantics.
/// When a field view is assigned, emitStore uses str_slice_store to write
/// into the base buffer at the correct offset.
pub fn emitPtrOffset(e: *BytecodeEmitter, p: ir.Instruction.PtrOffset) EmitError!void {
    // Get base slot and global flag for field view tracking
    const base_slot_info = e.value_slots.get(p.base_ptr.id);

    // Determine the field length from the result type
    // Field views have ptr type pointing to the field type (e.g., *[6]u8)
    const field_length: u16 = blk: {
        if (p.result.ty == .ptr) {
            break :blk @intCast(p.result.ty.ptr.*.sizeInBytes());
        } else {
            break :blk @intCast(p.result.ty.sizeInBytes());
        }
    };

    // Record field view info for ALL ptr_offset results (including offset 0)
    // This enables write-through semantics for DBL record field overlays
    if (base_slot_info) |slot_info| {
        const base_slot = slot_info & 0x7FFF;
        const is_global = (slot_info & 0x8000) != 0;

        try e.field_view_info.put(p.result.id, .{
            .base_slot = base_slot,
            .byte_offset = @intCast(p.offset),
            .length = field_length,
            .is_global = is_global,
        });

        debug.print(.emit, "ptr_offset: id={d} -> base_slot={d} offset={d} len={d}", .{
            p.result.id, base_slot, p.offset, field_length,
        });
    }

    // For field views with offset 0, we just need to create an alias in value_slots
    // The new pointer points to the same memory location as the base pointer
    if (p.offset == 0) {
        if (base_slot_info) |slot_info| {
            try e.value_slots.put(p.result.id, slot_info);
            return;
        }
    }

    const base_reg = try e.getValueInReg(p.base_ptr, 0);
    const dest_reg: u4 = 1;
    try e.emitOpcode(.ptr_offset);
    try e.emitU8((@as(u8, dest_reg) << 4) | base_reg);
    // Emit offset as signed 16-bit
    const offset_u16: u16 = @bitCast(@as(i16, @intCast(p.offset)));
    try e.emitU8(@truncate(offset_u16));
    try e.emitU8(@truncate(offset_u16 >> 8));
    e.setLastResult(p.result.id, dest_reg);

    // Also register in value_slots for subsequent loads
    if (base_slot_info) |slot_info| {
        try e.value_slots.put(p.result.id, slot_info);
    }
}

// ============================================================================
// Weak Reference Operations
// ============================================================================

/// Emit weak_ref instruction - create a weak reference from a value
/// weak_ref rd, rs - Creates weak reference without retaining
pub fn emitWeakRef(e: *BytecodeEmitter, w: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(w.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.weak_ref, dest_reg, src_reg);
    e.setLastResult(w.result.id, dest_reg);
}

/// Emit weak_load instruction - load from a weak reference
/// weak_load rd, rs - Returns the value or null if target was freed
pub fn emitWeakLoad(e: *BytecodeEmitter, w: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(w.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.weak_load, dest_reg, src_reg);
    e.setLastResult(w.result.id, dest_reg);
}

// ============================================================================
// ARC Operations
// ============================================================================

/// Emit arc_retain instruction - increment reference count
/// arc_retain rs - No-op for inline values
pub fn emitArcRetain(e: *BytecodeEmitter, a: ir.Instruction.ArcOp) EmitError!void {
    const src_reg = try e.getValueInReg(a.value, 0);
    try e.emitOpcode(.arc_retain);
    try e.emitU8(@as(u8, src_reg) << 4); // [rs:4|0]
    try e.emitU8(0);
}

/// Emit arc_release instruction - decrement reference count (may free)
/// arc_release rs - No-op for inline values
pub fn emitArcRelease(e: *BytecodeEmitter, a: ir.Instruction.ArcOp) EmitError!void {
    const src_reg = try e.getValueInReg(a.value, 0);
    try e.emitOpcode(.arc_release);
    try e.emitU8(@as(u8, src_reg) << 4); // [rs:4|0]
    try e.emitU8(0);
}

/// Emit arc_move instruction - move value without ARC
/// arc_move rd, rs - Copies rs to rd, nulls rs
pub fn emitArcMove(e: *BytecodeEmitter, a: ir.Instruction.UnaryOp) EmitError!void {
    const src_reg = try e.getValueInReg(a.operand, 0);
    const dest_reg: u4 = 1;
    try e.emitRegUnary(.arc_move, dest_reg, src_reg);
    e.setLastResult(a.result.id, dest_reg);
}

// ============================================================================
// Closure Operations
// ============================================================================

/// Emit make_closure instruction - create closure from function and environment
/// make_closure rd, env_reg, fn_idx - Creates a closure value
pub fn emitMakeClosure(e: *BytecodeEmitter, c: ir.Instruction.MakeClosure) EmitError!void {
    const env_reg = try e.getValueInReg(c.env, 0);
    const dest_reg: u4 = 1;

    // Look up the function index by name
    var fn_idx: u16 = 0;
    for (e.routines.items, 0..) |r, i| {
        // Match by routine name
        if (r.name_index < e.constants.items.len) {
            const constant = e.constants.items[r.name_index];
            // Extract string from constant (identifier or string type)
            const name = switch (constant) {
                .identifier => |s| s,
                .string => |s| s,
                else => continue,
            };
            if (std.mem.eql(u8, name, c.func_name)) {
                fn_idx = @intCast(i);
                break;
            }
        }
    }

    // Emit: [opcode] [rd:4|env_reg:4] [fn_idx:16]
    try e.emitOpcode(.make_closure);
    try e.emitU8((@as(u8, dest_reg) << 4) | env_reg);
    try e.emitU16(fn_idx);
    e.setLastResult(c.result.id, dest_reg);
}

// ============================================================================
// Trait Object Operations
// ============================================================================

/// Emit make_trait_object instruction - create trait object from value
/// make_trait_object rd, src_reg, vtable_idx - Creates a trait object (fat pointer)
pub fn emitMakeTraitObject(e: *BytecodeEmitter, m: ir.Instruction.MakeTraitObject) EmitError!void {
    const src_reg = try e.getValueInReg(m.value, 0);
    const dest_reg: u4 = 1;

    // Look up the vtable index by trait_name and type_name
    var vtable_idx: u16 = 0;
    if (e.ir_module) |ir_mod| {
        for (ir_mod.vtables.items, 0..) |vt, i| {
            if (std.mem.eql(u8, vt.trait_name, m.trait_name) and
                std.mem.eql(u8, vt.type_name, m.type_name))
            {
                vtable_idx = @intCast(i);
                break;
            }
        }
    }

    // Emit: [opcode] [rd:4|src_reg:4] [vtable_idx:16]
    try e.emitOpcode(.make_trait_object);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU16(vtable_idx);
    e.setLastResult(m.result.id, dest_reg);
}

/// Emit call_trait_method instruction - call method on trait object via vtable
/// call_trait_method trait_obj_reg, method_idx, argc - Calls method via dynamic dispatch
pub fn emitCallTraitMethod(e: *BytecodeEmitter, c: ir.Instruction.CallTraitMethod) EmitError!void {
    // Get trait object into a register
    const trait_obj_reg = try e.getValueInReg(c.trait_object, 0);

    // Get the trait name from the trait object's type
    const trait_name = if (c.trait_object.ty == .trait_object)
        c.trait_object.ty.trait_object.trait_name
    else
        return EmitError.InvalidInstruction;

    // Look up method index in the trait definition
    var method_idx: u8 = 0;
    if (e.ir_module) |ir_mod| {
        // Find the trait definition
        for (ir_mod.traits.items) |trait_def| {
            if (std.mem.eql(u8, trait_def.name, trait_name)) {
                // Find the method index
                for (trait_def.methods, 0..) |method, i| {
                    if (std.mem.eql(u8, method.name, c.method_name)) {
                        method_idx = @intCast(i);
                        break;
                    }
                }
                break;
            }
        }
    }

    // Place arguments in registers (starting at r1, r2, etc.)
    for (c.args, 0..) |arg, i| {
        const arg_reg: u4 = @intCast(i + 1);
        _ = try e.getValueInReg(arg, arg_reg);
    }

    const argc: u8 = @intCast(c.args.len);

    // Emit: [opcode] [trait_obj_reg:4|0:4] [method_idx:8] [argc:8]
    try e.emitOpcode(.call_trait_method);
    try e.emitU8((@as(u8, trait_obj_reg) << 4) | 0);
    try e.emitU8(method_idx);
    try e.emitU8(argc);

    // Return value is in r15 (placed there by ret_val)
    e.setLastResult(c.result.id, 15);
}

// ============================================================================
// Heap Record Operations (for `new` keyword)
// ============================================================================

/// Emit heap_alloc instruction - allocate a heap record
/// Format: [opcode] [rd:4|0:4] [field_count:16]
pub fn emitHeapAlloc(e: *BytecodeEmitter, h: ir.Instruction.HeapAlloc) EmitError!void {
    const dest_reg: u4 = 0;
    // Spill any existing value in r0 before overwriting
    e.prepareDestReg(dest_reg);

    // Emit: new_record rd, field_count
    try e.emitOpcode(.new_record);
    try e.emitU8((@as(u8, dest_reg) << 4) | 0);
    try e.emitU16(@intCast(h.field_count));

    e.setLastResult(h.result.id, dest_reg);
}

/// Emit store_field_heap instruction - store value to heap record field
/// Format: [opcode] [record_reg:4|val_reg:4] [field_idx:16]
pub fn emitStoreFieldHeap(e: *BytecodeEmitter, s: ir.Instruction.StoreFieldHeap) EmitError!void {
    const record_reg = try e.getValueInReg(s.record, 0);
    const val_reg = try e.getValueInReg(s.value, 1);

    try e.emitOpcode(.store_field);
    try e.emitU8((@as(u8, record_reg) << 4) | val_reg);
    try e.emitU16(@intCast(s.field_index));
}

/// Emit load_field_heap instruction - load value from heap record field
/// Format: [opcode] [rd:4|record_reg:4] [field_idx:16]
pub fn emitLoadFieldHeap(e: *BytecodeEmitter, l: ir.Instruction.LoadFieldHeap) EmitError!void {
    const record_reg = try e.getValueInReg(l.record, 0);
    const dest_reg: u4 = 1;

    try e.emitOpcode(.load_field);
    try e.emitU8((@as(u8, dest_reg) << 4) | record_reg);
    try e.emitU16(@intCast(l.field_index));

    e.setLastResult(l.result.id, dest_reg);
}

// ============================================================================
// Variant (Sum Type) Operations
// ============================================================================

/// Emit variant_construct instruction - construct a variant with payload
/// Format: [opcode] [rd:4|argc:4] [tag:16]
/// Payload values are expected to be in r0..r(argc-1)
pub fn emitVariantConstruct(e: *BytecodeEmitter, v: ir.Instruction.VariantConstruct) EmitError!void {
    const argc: u4 = @intCast(v.payload.len);

    // Load payload values into registers r0..r(argc-1)
    for (v.payload, 0..) |payload_val, i| {
        _ = try e.getValueInReg(payload_val, @intCast(i));
    }

    // Result goes to r0 (or first register after payload setup)
    const dest_reg: u4 = 0;

    try e.emitOpcode(.variant_construct);
    try e.emitU8((@as(u8, dest_reg) << 4) | argc);
    try e.emitU16(v.tag);

    e.setLastResult(v.result.id, dest_reg);
}

/// Emit variant_get_tag instruction - get the tag from a variant
/// Format: [opcode] [rd:4|src_reg:4] [0]
pub fn emitVariantGetTag(e: *BytecodeEmitter, v: ir.Instruction.VariantGetTag) EmitError!void {
    const src_reg = try e.getValueInReg(v.variant, 0);
    // Allocate a fresh register for the result, spilling if necessary
    const dest_reg = try e.allocateWithSpill(v.result.id);

    try e.emitOpcode(.variant_get_tag);
    try e.emitU8((@as(u8, dest_reg) << 4) | src_reg);
    try e.emitU8(0); // padding

    e.setLastResult(v.result.id, dest_reg);
}
