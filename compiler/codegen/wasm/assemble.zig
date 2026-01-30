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
    code: []const u8,
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
    // Setup register to local mapping
    // ========================================================================
    // Go: wasmobj.go lines 1040-1142
    //
    // Globals 0-7: SP, CTXT, g, RET0-3, PAUSE
    // Locals: depend on function type and register usage

    var reg_vars: [64]?RegVar = [_]?RegVar{null} ** 64;

    // Global registers (Go: lines 1051-1059)
    reg_vars[@intFromEnum(c.Reg.sp)] = .{ .global = true, .index = 0 };
    reg_vars[@intFromEnum(c.Reg.ctxt)] = .{ .global = true, .index = 1 };
    reg_vars[@intFromEnum(c.Reg.g)] = .{ .global = true, .index = 2 };
    reg_vars[@intFromEnum(c.Reg.ret0)] = .{ .global = true, .index = 3 };
    reg_vars[@intFromEnum(c.Reg.ret1)] = .{ .global = true, .index = 4 };
    reg_vars[@intFromEnum(c.Reg.ret2)] = .{ .global = true, .index = 5 };
    reg_vars[@intFromEnum(c.Reg.ret3)] = .{ .global = true, .index = 6 };
    reg_vars[@intFromEnum(c.Reg.pause)] = .{ .global = true, .index = 7 };

    // Determine which registers are used
    var reg_used: [64]bool = [_]bool{false} ** 64;
    var p: ?*Prog = sym.text;
    while (p) |current| {
        if (current.from.reg != .none) {
            const idx = @as(usize, @intCast(@intFromEnum(current.from.reg)));
            if (idx < 64) reg_used[idx] = true;
        }
        if (current.to.reg != .none) {
            const idx = @as(usize, @intCast(@intFromEnum(current.to.reg)));
            if (idx < 64) reg_used[idx] = true;
        }
        p = current.link;
    }

    // Build local variable declarations
    var var_decls = std.ArrayListUnmanaged(VarDecl){};
    defer var_decls.deinit(allocator);

    // For Cot functions: locals are just the computed registers
    // (Go has PC_B as parameter and SP cache as first local)
    var local_idx: u64 = 0;

    // Allocate locals for used registers
    var last_type: ?c.ValType = null;
    for (0..64) |i| {
        const reg: c.Reg = @enumFromInt(@as(i16, @intCast(i)));
        if (reg.isGlobal()) continue; // Skip globals
        if (!reg_used[i]) continue;

        const typ = reg.valType();
        if (last_type == null or last_type.? != typ) {
            try var_decls.append(allocator, .{ .count = 1, .typ = typ });
            last_type = typ;
        } else {
            var_decls.items[var_decls.items.len - 1].count += 1;
        }
        reg_vars[i] = .{ .global = false, .index = local_idx };
        local_idx += 1;
    }

    // ========================================================================
    // Write local declarations
    // ========================================================================
    // Go: wasmobj.go lines 1146-1150

    try writeULEB128(allocator, &w, var_decls.items.len);
    for (var_decls.items) |decl| {
        try writeULEB128(allocator, &w, decl.count);
        try w.append(allocator, @intFromEnum(decl.typ));
    }

    debugLog( "  {d} local groups, {d} total locals", .{ var_decls.items.len, local_idx });

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

    try w.append(allocator, 0x0B); // end

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
    reg_vars: *const [64]?RegVar,
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
                try writeOpcode(w, .global_get);
            } else {
                try writeOpcode(w, .local_get);
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
                try writeOpcode(w, .global_set);
            } else {
                try writeOpcode(w, .local_set);
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

            try writeOpcode(w, .local_tee);
            try writeULEB128(allocator, w, v.index);
        },

        .not => {
            // Alias for i32.eqz
            try writeOpcode(w, .i32_eqz);
        },

        // ====================================================================
        // Block instructions with block type
        // ====================================================================

        .block, .loop, .@"if" => {
            // Go: wasmobj.go lines 1236-1242
            try writeOpcode(w, p.as);
            if (p.from.offset != 0) {
                // Block type (rarely used)
                try w.append(allocator, @as(u8, @intCast(0x80 - @as(i8, @intCast(p.from.offset)))));
            } else {
                try w.append(allocator, 0x40); // void block type
            }
        },

        // ====================================================================
        // Branch instructions
        // ====================================================================

        .br, .br_if => {
            // Go: wasmobj.go lines 1244-1248
            try writeOpcode(w, p.as);
            if (p.to.type != .const_int) {
                return error.BadBranchOperand;
            }
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        .br_table => {
            // Go: wasmobj.go lines 1250-1255
            try writeOpcode(w, .br_table);
            const idxs = p.to.val.br_table;
            try writeULEB128(allocator, w, idxs.len - 1);
            for (idxs) |idx| {
                try writeULEB128(allocator, w, idx);
            }
        },

        // ====================================================================
        // Call instructions
        // ====================================================================

        .call => {
            // Go: wasmobj.go lines 1257-1284
            try writeOpcode(w, .call);
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
            try writeOpcode(w, .call_indirect);
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
            try w.append(allocator, 0x00); // reserved
        },

        // ====================================================================
        // Constant instructions
        // ====================================================================

        .i32_const, .i64_const => {
            // Go: wasmobj.go lines 1294-1305
            try writeOpcode(w, p.as);
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
            try writeOpcode(w, .f32_const);
            const val: f32 = @floatCast(p.from.val.float);
            const bytes = @as([4]u8, @bitCast(val));
            try w.appendSlice(allocator, &bytes);
        },

        .f64_const => {
            // Go: wasmobj.go lines 1312-1315
            try writeOpcode(w, .f64_const);
            const bytes = @as([8]u8, @bitCast(p.from.val.float));
            try w.appendSlice(allocator, &bytes);
        },

        // ====================================================================
        // Load instructions
        // ====================================================================

        .i32_load, .i64_load, .f32_load, .f64_load, .i32_load8_s, .i32_load8_u, .i32_load16_s, .i32_load16_u, .i64_load8_s, .i64_load8_u, .i64_load16_s, .i64_load16_u, .i64_load32_s, .i64_load32_u => {
            // Go: wasmobj.go lines 1317-1328
            try writeOpcode(w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.from.offset)));
        },

        // ====================================================================
        // Store instructions
        // ====================================================================

        .i32_store, .i64_store, .f32_store, .f64_store, .i32_store8, .i32_store16, .i64_store8, .i64_store16, .i64_store32 => {
            // Go: wasmobj.go lines 1330-1338
            try writeOpcode(w, p.as);
            try writeULEB128(allocator, w, alignment(p.as));
            try writeULEB128(allocator, w, @as(u64, @intCast(p.to.offset)));
        },

        // ====================================================================
        // Memory instructions
        // ====================================================================

        .memory_size, .memory_grow, .memory_fill => {
            // Go: wasmobj.go lines 1340-1341
            try writeOpcode(w, p.as);
            try w.append(allocator, 0x00);
        },

        .memory_copy => {
            // Go: wasmobj.go lines 1343-1345
            try writeOpcode(w, p.as);
            try w.append(allocator, 0x00);
            try w.append(allocator, 0x00);
        },

        // ====================================================================
        // Skip pseudo-instructions
        // ====================================================================

        .nop, .text, .func_data, .pc_data => {
            // Don't emit anything
        },

        .undef => {
            try writeOpcode(w, .@"unreachable");
        },

        // ====================================================================
        // All other instructions - direct opcode
        // ====================================================================

        else => {
            try writeOpcode(w, p.as);
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
fn writeOpcode(w: *std.ArrayListUnmanaged(u8), as: c.As) !void {
    if (as.isFcPrefixed()) {
        try w.append(w.allocator.?, 0xFC);
        try w.append(w.allocator.?, as.fcOpcode().?);
    } else if (as.opcode()) |op| {
        try w.append(w.allocator.?, op);
    }
}

/// Unsigned LEB128 encoding
/// Go: wasmobj.go lines 1423-1437
pub fn writeULEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: u64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(v & 0x7F);
        v >>= 7;
        if (v == 0) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | 0x80);
    }
}

/// Signed LEB128 encoding
/// Go: wasmobj.go lines 1440-1451
pub fn writeSLEB128(allocator: std.mem.Allocator, w: *std.ArrayListUnmanaged(u8), value: i64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(@as(u64, @bitCast(v)) & 0x7F);
        const s: u8 = @truncate(@as(u64, @bitCast(v)) & 0x40);
        v >>= 7;
        const done = (v == 0 and s == 0) or (v == -1 and s != 0);
        if (done) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | 0x80);
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
