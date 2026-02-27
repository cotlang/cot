//! Assemble - Convert Prog chain to Wasm bytes
//!
//! Go reference: cmd/internal/obj/wasm/wasmobj.go assemble()
//!
//! This converts the preprocessed Prog chain into actual Wasm binary bytes.
//! Key responsibilities:
//! - Local variable declarations
//! - Register to local index mapping
//! - Opcode encoding
//! - Operand encoding (LEB128 for integers, raw for floats)

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const Prog = prog.Prog;
const Addr = prog.Addr;
const Symbol = prog.Symbol;

// Debug logging - conditionally import if available
const debug_enabled = false; // Set to true when integrated with main build
fn debugLog(comptime fmt: []const u8, args: anytype) void {
    if (debug_enabled) {
        @import("std").debug.print(fmt ++ "\n", args);
    }
}

/// Result of assembling a function
pub const AssembledFunc = struct {
    code: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *AssembledFunc) void {
        self.allocator.free(self.code);
    }
};

/// Register variable info (like Go's regVar)
const RegVar = struct {
    global: bool,
    index: u64,
};

/// Local variable declaration
const VarDecl = struct {
    count: u64,
    typ: c.ValType,
};

/// Assemble a function's Prog chain into Wasm bytes.
///
/// Go reference: wasmobj.go lines 1039-1352
pub fn assemble(allocator: std.mem.Allocator, sym: *Symbol) !AssembledFunc {
    debugLog( "assemble: '{s}'", .{sym.name});

    var w = std.ArrayListUnmanaged(u8){};
    errdefer w.deinit(allocator);

    // ========================================================================
    // Scan for highest local index and check for dispatch loop
    // ========================================================================
    // The new gen.zig emits local_get/local_set with const_int indices directly,
    // so we scan the Prog chain to find the highest index used.
    // Also check if br_table is used (indicates dispatch loop needs PC_B).

    var max_local_idx: u64 = 0;
    var has_dispatch_loop = false;
    var p: ?*Prog = sym.text;
    while (p) |current| {
        switch (current.as) {
            .local_get => {
                if (current.from.type == .const_int) {
                    const idx: u64 = @intCast(current.from.offset);
                    if (idx >= max_local_idx) max_local_idx = idx + 1;
                }
            },
            .local_set, .local_tee => {
                if (current.to.type == .const_int) {
                    const idx: u64 = @intCast(current.to.offset);
                    if (idx >= max_local_idx) max_local_idx = idx + 1;
                }
            },
            .br_table => has_dispatch_loop = true,
            else => {},
        }
        p = current.link;
    }

    // Setup register to local mapping for .get/.set instructions (globals only)
    // These are used when preprocess.zig emits get/set for SP, etc.
    var reg_vars: [128]?RegVar = [_]?RegVar{null} ** 128;
    reg_vars[@intFromEnum(c.Reg.sp)] = .{ .global = true, .index = 0 };
    reg_vars[@intFromEnum(c.Reg.ctxt)] = .{ .global = true, .index = 1 };
    reg_vars[@intFromEnum(c.Reg.g)] = .{ .global = true, .index = 2 };
    reg_vars[@intFromEnum(c.Reg.ret0)] = .{ .global = true, .index = 3 };
    reg_vars[@intFromEnum(c.Reg.ret1)] = .{ .global = true, .index = 4 };
    reg_vars[@intFromEnum(c.Reg.ret2)] = .{ .global = true, .index = 5 };
    reg_vars[@intFromEnum(c.Reg.ret3)] = .{ .global = true, .index = 6 };
    reg_vars[@intFromEnum(c.Reg.pause)] = .{ .global = true, .index = 7 };
    // PC_B is the first declared local (index = param_count)
    // Wasm assigns params to locals 0..param_count-1, declared locals start at param_count
    reg_vars[@intFromEnum(c.Reg.pc_b)] = .{ .global = false, .index = sym.param_count };

    // ========================================================================
    // Write local declarations
    // ========================================================================
    // Go reference: wasmobj.go lines 1146-1150
    // Wasm params are locals 0..param_count-1 (not declared, part of func signature)
    // Declared locals start at index param_count:
    // - PC_B (i32) at index param_count (if dispatch loop)
    // - i64 value locals at indices param_count+1, ..., param_count+N
    // - f64 value locals at indices param_count+N+1, ..., param_count+N+M

    const param_count: u64 = sym.param_count;
    const float_count: u64 = sym.float_local_count;

    // GC ref locals: declared after i64 and f64 locals
    const gc_ref_locals = sym.gc_ref_locals;
    const gc_ref_count: u64 = gc_ref_locals.len;

    // Calculate number of declared value locals (after PC_B)
    // max_local_idx includes params, so subtract param_count and 1 for PC_B
    // Also subtract GC ref locals — they occupy the highest local indices but
    // are declared separately as ref types, not as i64.
    const total_value_locals: u64 = if (max_local_idx > param_count + 1)
        max_local_idx - param_count - 1
    else
        0;
    const i64_value_locals: u64 = if (total_value_locals >= float_count + gc_ref_count)
        total_value_locals - float_count - gc_ref_count
    else if (total_value_locals >= float_count)
        total_value_locals - float_count
    else
        total_value_locals;

    // Count how many local groups we need
    var num_groups: u64 = 0;
    if (has_dispatch_loop) num_groups += 1; // PC_B (i32)
    if (i64_value_locals > 0) num_groups += 1; // i64 locals
    if (float_count > 0) num_groups += 1; // f64 locals
    // GC ref locals: each ref local is its own group (different type index)
    num_groups += gc_ref_count;

    if (num_groups == 0 and !has_dispatch_loop and max_local_idx <= param_count) {
        try writeULEB128(allocator, &w, 0); // 0 local groups
    } else {
        try writeULEB128(allocator, &w, num_groups);
        if (has_dispatch_loop) {
            try writeULEB128(allocator, &w, 1); // 1 i32 local (PC_B)
            try w.append(allocator, @intFromEnum(c.ValType.i32));
        }
        if (i64_value_locals > 0) {
            try writeULEB128(allocator, &w, i64_value_locals);
            try w.append(allocator, @intFromEnum(c.ValType.i64));
        }
        if (float_count > 0) {
            try writeULEB128(allocator, &w, float_count);
            try w.append(allocator, @intFromEnum(c.ValType.f64));
        }
        // GC ref locals: each declared as (ref null $typeidx)
        for (gc_ref_locals) |gc_type_idx| {
            try writeULEB128(allocator, &w, 1); // 1 local of this ref type
            try w.append(allocator, c.GC_REF_TYPE_NULL); // 0x63 = (ref null ...)
            try writeULEB128(allocator, &w, gc_type_idx); // type index
        }
    }

    debugLog( "  {d} total locals, {d} params, {d} i64, {d} f64", .{ max_local_idx, param_count, i64_value_locals, float_count });

    // WasmGC: explicitly initialize ref locals at function entry.
    // Reference: Kotlin/Dart WasmGC — the br_table dispatch loop means ref locals
    // may be read in blocks before the linear code flow sets them. The GC spec
    // requires ref locals to be provably initialized before use. This must be
    // emitted BEFORE the dispatch loop (which wraps the Prog chain), not inside it.
    // Initialize GC ref locals at function entry using struct.new_default.
    // struct.new_default (0xFB 0x01) produces (ref $T) — non-nullable, which
    // satisfies wasmtime's validation for struct.get instructions on the local.
    // br_table dispatch loop means ref locals may be read before linear set.
    if (gc_ref_locals.len > 0) {
        const ref_local_start: u64 = if (has_dispatch_loop) param_count + 1 + i64_value_locals + float_count else param_count + i64_value_locals + float_count;
        for (gc_ref_locals, 0..) |gc_type_idx, i| {
            const ref_local_idx = ref_local_start + i;
            try w.append(allocator, c.GC_PREFIX); // 0xFB
            try w.append(allocator, c.GC_STRUCT_NEW_DEFAULT); // 0x01
            try writeULEB128(allocator, &w, gc_type_idx);
            try w.append(allocator, 0x21); // local.set
            try writeULEB128(allocator, &w, ref_local_idx);
        }
    }

    // ========================================================================
    // Encode instructions
    // ========================================================================
    // Go: wasmobj.go lines 1157-1347

    p = sym.text;
    while (p) |current| {
        try encodeInstruction(allocator, &w, current, &reg_vars);
        p = current.link;
    }

    // ========================================================================
    // End opcode
    // ========================================================================

    try w.append(allocator, c.As.end.opcode().?);

    debugLog( "  assembled {d} bytes", .{w.items.len});

    return .{
        .code = try w.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

/// Encode a single instruction
fn encodeInstruction(
    allocator: std.mem.Allocator,
    w: *std.ArrayListUnmanaged(u8),
    p: *const Prog,
    reg_vars: *const [128]?RegVar,
) !void {
    switch (p.as) {
        // ====================================================================
        // Pseudo-instructions that map to real ops
        // ====================================================================

        .get => {
            // Go: wasmobj.go lines 1159-1179
            if (p.from.type != .reg) {
                return error.BadGetOperand;
            }
            const reg = p.from.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            if (v.global) {
                try writeOpcode(allocator, w, .global_get);
            } else {
                try writeOpcode(allocator, w, .local_get);
            }
            try writeULEB128(allocator, w, v.index);
        },

        .set => {
            // Go: wasmobj.go lines 1181-1205
            if (p.to.type != .reg) {
                return error.BadSetOperand;
            }
            const reg = p.to.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            if (v.global) {
                try writeOpcode(allocator, w, .global_set);
            } else {
                try writeOpcode(allocator, w, .local_set);
            }
            try writeULEB128(allocator, w, v.index);
        },

        .tee => {
            // Go: wasmobj.go lines 1207-1218
            if (p.to.type != .reg) {
                return error.BadTeeOperand;
            }
            const reg = p.to.reg;
            const idx = @as(usize, @intCast(@intFromEnum(reg)));
            const v = reg_vars[idx] orelse return error.InvalidRegister;

            try writeOpcode(allocator, w, .local_tee);
            try writeULEB128(allocator, w, v.index);
        },

        .not => {
            // Alias for i32.eqz
            try writeOpcode(allocator, w, .i32_eqz);
        },

        // ====================================================================
        // Block instructions with block type
        // ====================================================================

        .block, .loop, .@"if" => {
            // Go: wasmobj.go lines 1236-1242
            try writeOpcode(allocator, w, p.as);
            if (p.from.offset != 0) {
                // Block type (rarely used)
                // Block type is stored as negative offset, convert to unsigned byte
                const block_type: u8 = @truncate(@as(u64, @bitCast(0x80 - p.from.offset)));
                try w.append(allocator, block_type);
            } else {
                try w.append(allocator, c.BLOCK_VOID);
            }
        },

        // ====================================================================
        // Branch instructions
        // ====================================================================

        .br, .br_if => {
            // Go: wasmobj.go lines 1244-1248
            try writeOpcode(allocator, w, p.as);
            if (p.to.type != .const_int) {
                return error.BadBranchOperand;
            }
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .br_table => {
            // Go: wasmobj.go lines 1250-1255
            try writeOpcode(allocator, w, .br_table);
            const idxs = p.to.val.br_table;
            try writeULEB128(allocator, w, idxs.len - 1);
            for (idxs) |idx| {
                try writeULEB128(allocator, w, idx);
            }
        },

        // ====================================================================
        // Call instructions
        // ====================================================================

        .call, .return_call => {
            // Go: wasmobj.go lines 1257-1284
            // return_call has same encoding as call (opcode + funcidx)
            try writeOpcode(allocator, w, p.as);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            } else if (p.to.sym) |s| {
                // Relocation needed - for now emit placeholder
                try writeULEB128(allocator, w, s.index);
            } else {
                return error.BadCallOperand;
            }
        },

        .call_indirect => {
            // Go: wasmobj.go lines 1286-1292
            try writeOpcode(allocator, w, .call_indirect);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            try w.append(allocator, c.TABLE_IDX_ZERO); // table index
        },

        // ====================================================================
        // Constant instructions
        // ====================================================================

        .i32_const, .i64_const => {
            // Go: wasmobj.go lines 1294-1305
            try writeOpcode(allocator, w, p.as);
            if (p.from.name == .@"extern") {
                // Symbol reference - emit placeholder for relocation
                if (p.from.sym) |s| {
                    try writeSLEB128(allocator, w, @as(i64, @intCast(s.index)) + p.from.offset);
                } else {
                    try writeSLEB128(allocator, w, p.from.offset);
                }
            } else {
                try writeSLEB128(allocator, w, p.from.offset);
            }
        },

        .f32_const => {
            // Go: wasmobj.go lines 1307-1310
            try writeOpcode(allocator, w, .f32_const);
            const val: f32 = @floatCast(p.from.val.float);
            const bytes = @as([4]u8, @bitCast(val));
            try w.appendSlice(allocator, &bytes);
        },

        .f64_const => {
            // Go: wasmobj.go lines 1312-1315
            try writeOpcode(allocator, w, .f64_const);
            const bytes = @as([8]u8, @bitCast(p.from.val.float));
            try w.appendSlice(allocator, &bytes);
        },

        // ====================================================================
        // Load instructions
        // ====================================================================

        .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
            // Go: wasmobj.go lines 1317-1328
            try writeOpcode(allocator, w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        // ====================================================================
        // Store instructions
        // ====================================================================

        .i32_store, .i64_store, .f32_store, .f64_store, .i32_store8, .i32_store16, .i64_store8, .i64_store16, .i64_store32 => {
            // Go: wasmobj.go lines 1330-1338
            try writeOpcode(allocator, w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        // ====================================================================
        // Memory instructions
        // ====================================================================

        .memory_size, .memory_grow, .memory_fill => {
            // Go: wasmobj.go lines 1340-1341
            try writeOpcode(allocator, w, p.as);
            try w.append(allocator, c.MEMORY_IDX_ZERO);
        },

        .memory_copy => {
            // Go: wasmobj.go lines 1343-1345
            try writeOpcode(allocator, w, p.as);
            try w.append(allocator, c.MEMORY_IDX_ZERO); // dest memory
            try w.append(allocator, c.MEMORY_IDX_ZERO); // src memory
        },

        // ====================================================================
        // Skip pseudo-instructions
        // ====================================================================

        .nop, .text, .func_data, .pc_data => {
            // Don't emit anything
        },

        .undef => {
            try writeOpcode(allocator, w, .@"unreachable");
        },

        // ====================================================================
        // Direct local access (emitted by gen.zig with const_int index)
        // ====================================================================

        .local_get => {
            try writeOpcode(allocator, w, .local_get);
            if (p.from.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            }
        },

        .local_set => {
            try writeOpcode(allocator, w, .local_set);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        .local_tee => {
            try writeOpcode(allocator, w, .local_tee);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        .global_get => {
            try writeOpcode(allocator, w, .global_get);
            if (p.from.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            }
        },

        .global_set => {
            try writeOpcode(allocator, w, .global_set);
            if (p.to.type == .const_int) {
                try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            }
        },

        // ====================================================================
        // WasmGC struct operations (0xFB prefix)
        // ====================================================================

        .gc_struct_new => {
            // 0xFB 0x00 <type_idx>
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_NEW);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        .gc_struct_get => {
            // 0xFB 0x02 <type_idx> <field_idx>
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_GET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .gc_struct_set => {
            // 0xFB 0x05 <type_idx> <field_idx>
            try w.append(allocator, c.GC_PREFIX);
            try w.append(allocator, c.GC_STRUCT_SET);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        // ====================================================================
        // All other instructions - direct opcode
        // ====================================================================

        else => {
            try writeOpcode(allocator, w, p.as);
        },
    }
}

/// Get alignment for load/store instructions
/// Go: wasmobj.go lines 1408-1421
fn alignment(as: c.As) u64 {
    return switch (as) {
        .i32_load8_s, .i32_load8_u, .i64_load8_s, .i64_load8_u, .i32_store8, .i64_store8 => 0,
        .i32_load16_s, .i32_load16_u, .i64_load16_s, .i64_load16_u, .i32_store16, .i64_store16 => 1,
        .i32_load, .f32_load, .i64_load32_s, .i64_load32_u, .i32_store, .f32_store, .i64_store32 => 2,
        .i64_load, .f64_load, .i64_store, .f64_store => 3,
        else => 0,
    };
}

/// Write opcode byte(s)
fn writeOpcode(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), as: c.As) !void {
    if (as.isFcPrefixed()) {
        try w.append(allocator, c.FC_PREFIX);
        try w.append(allocator, as.fcOpcode().?);
    } else if (as.opcode()) |op| {
        try w.append(allocator, op);
    }
}

/// LEB128 encoding constants (WebAssembly spec, binary encoding)
const LEB128_LOW_BITS_MASK: u8 = 0x7F; // Extract low 7 data bits
const LEB128_CONTINUATION_BIT: u8 = 0x80; // High bit = more bytes follow
const LEB128_SIGN_BIT: u8 = 0x40; // Bit 6 = sign extension for SLEB128

/// Unsigned LEB128 encoding
/// Go: wasmobj.go lines 1423-1437
pub fn writeULEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: u64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(v & LEB128_LOW_BITS_MASK);
        v >>= 7;
        if (v == 0) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | LEB128_CONTINUATION_BIT);
    }
}

/// Signed LEB128 encoding
/// Go: wasmobj.go lines 1440-1451
pub fn writeSLEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(@as(u64, @bitCast(v)) & LEB128_LOW_BITS_MASK);
        const s: u8 = @truncate(@as(u64, @bitCast(v)) & LEB128_SIGN_BIT);
        v >>= 7;
        const done = (v == 0 and s == 0) or (v == -1 and s != 0);
        if (done) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | LEB128_CONTINUATION_BIT);
    }
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "ULEB128 encoding" {
    const allocator = testing.allocator;
    var w = std.ArrayListUnmanaged(u8){};
    defer w.deinit(allocator);

    try writeULEB128(allocator, &w, 0);
    try testing.expectEqualSlices(u8, &[_]u8{0x00}, w.items);

    w.clearRetainingCapacity();
    try writeULEB128(allocator, &w, 127);
    try testing.expectEqualSlices(u8, &[_]u8{0x7F}, w.items);

    w.clearRetainingCapacity();
    try writeULEB128(allocator, &w, 128);
    try testing.expectEqualSlices(u8, &[_]u8{ 0x80, 0x01 }, w.items);
}

test "SLEB128 encoding" {
    const allocator = testing.allocator;
    var w = std.ArrayListUnmanaged(u8){};
    defer w.deinit(allocator);

    try writeSLEB128(allocator, &w, 0);
    try testing.expectEqualSlices(u8, &[_]u8{0x00}, w.items);

    w.clearRetainingCapacity();
    try writeSLEB128(allocator, &w, -1);
    try testing.expectEqualSlices(u8, &[_]u8{0x7F}, w.items);

    w.clearRetainingCapacity();
    try writeSLEB128(allocator, &w, 42);
    try testing.expectEqualSlices(u8, &[_]u8{42}, w.items);
}

test "alignment values" {
    try testing.expectEqual(@as(u64, 0), alignment(.i64_load8_s));
    try testing.expectEqual(@as(u64, 1), alignment(.i64_load16_s));
    try testing.expectEqual(@as(u64, 2), alignment(.i32_load));
    try testing.expectEqual(@as(u64, 3), alignment(.i64_load));
}
