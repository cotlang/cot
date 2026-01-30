//! WebAssembly code generator.
//! Compiles Cot IR to WebAssembly binary format.

const std = @import("std");
const wasm_op = @import("wasm_opcodes.zig");
const enc = @import("wasm_encode.zig");

pub const Op = wasm_op.Op;
pub const ValType = wasm_op.ValType;
pub const Section = wasm_op.Section;
pub const ExportKind = wasm_op.ExportKind;

// ============================================================================
// Module Builder
// ============================================================================

pub const Module = struct {
    allocator: std.mem.Allocator,

    // Section data
    types: std.ArrayListUnmanaged(u8) = .{},
    funcs: std.ArrayListUnmanaged(u8) = .{},
    exports: std.ArrayListUnmanaged(u8) = .{},
    codes: std.ArrayListUnmanaged(u8) = .{},

    // Counters
    type_count: u32 = 0,
    func_count: u32 = 0,
    export_count: u32 = 0,

    // Memory configuration
    has_memory: bool = false,
    memory_min_pages: u32 = 0, // Minimum pages (64KB each)
    memory_max_pages: ?u32 = null, // Maximum pages (optional)

    pub fn init(allocator: std.mem.Allocator) Module {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Module) void {
        self.types.deinit(self.allocator);
        self.funcs.deinit(self.allocator);
        self.exports.deinit(self.allocator);
        self.codes.deinit(self.allocator);
    }

    /// Add linear memory with specified page limits.
    /// Each page is 64KB. min_pages is required, max_pages is optional.
    pub fn addMemory(self: *Module, min_pages: u32, max_pages: ?u32) void {
        self.has_memory = true;
        self.memory_min_pages = min_pages;
        self.memory_max_pages = max_pages;
    }

    /// Add a function type and return its index.
    pub fn addFuncType(self: *Module, params: []const ValType, results: []const ValType) !u32 {
        const idx = self.type_count;
        try enc.writeFuncType(self.types.writer(self.allocator), params, results);
        self.type_count += 1;
        return idx;
    }

    /// Add a function (references a type by index).
    pub fn addFunc(self: *Module, type_idx: u32) !u32 {
        const idx = self.func_count;
        try enc.encodeULEB128(self.funcs.writer(self.allocator), type_idx);
        self.func_count += 1;
        return idx;
    }

    /// Add an export.
    pub fn addExport(self: *Module, name: []const u8, kind: ExportKind, idx: u32) !void {
        const writer = self.exports.writer(self.allocator);
        try enc.writeName(writer, name);
        try writer.writeByte(@intFromEnum(kind));
        try enc.encodeULEB128(writer, idx);
        self.export_count += 1;
    }

    /// Add function code (body bytes including local declarations and end opcode).
    pub fn addCode(self: *Module, body: []const u8) !void {
        const writer = self.codes.writer(self.allocator);
        try enc.encodeULEB128(writer, body.len);
        try writer.writeAll(body);
    }

    /// Emit the complete Wasm binary.
    pub fn emit(self: *Module, output: anytype) !void {
        // Header
        try enc.writeHeader(output);

        // Type section
        if (self.type_count > 0) {
            var type_section: std.ArrayListUnmanaged(u8) = .{};
            defer type_section.deinit(self.allocator);
            try enc.encodeULEB128(type_section.writer(self.allocator), self.type_count);
            try type_section.appendSlice(self.allocator, self.types.items);
            try enc.writeSection(output, .type, type_section.items);
        }

        // Function section
        if (self.func_count > 0) {
            var func_section: std.ArrayListUnmanaged(u8) = .{};
            defer func_section.deinit(self.allocator);
            try enc.encodeULEB128(func_section.writer(self.allocator), self.func_count);
            try func_section.appendSlice(self.allocator, self.funcs.items);
            try enc.writeSection(output, .function, func_section.items);
        }

        // Memory section (section ID 5)
        if (self.has_memory) {
            var mem_section: std.ArrayListUnmanaged(u8) = .{};
            defer mem_section.deinit(self.allocator);
            const mem_writer = mem_section.writer(self.allocator);
            try enc.encodeULEB128(mem_writer, 1); // 1 memory
            if (self.memory_max_pages) |max| {
                try mem_writer.writeByte(0x01); // limits with max
                try enc.encodeULEB128(mem_writer, self.memory_min_pages);
                try enc.encodeULEB128(mem_writer, max);
            } else {
                try mem_writer.writeByte(0x00); // limits without max
                try enc.encodeULEB128(mem_writer, self.memory_min_pages);
            }
            try enc.writeSection(output, .memory, mem_section.items);
        }

        // Export section
        if (self.export_count > 0) {
            var export_section: std.ArrayListUnmanaged(u8) = .{};
            defer export_section.deinit(self.allocator);
            try enc.encodeULEB128(export_section.writer(self.allocator), self.export_count);
            try export_section.appendSlice(self.allocator, self.exports.items);
            try enc.writeSection(output, .@"export", export_section.items);
        }

        // Code section
        if (self.func_count > 0) {
            var code_section: std.ArrayListUnmanaged(u8) = .{};
            defer code_section.deinit(self.allocator);
            try enc.encodeULEB128(code_section.writer(self.allocator), self.func_count);
            try code_section.appendSlice(self.allocator, self.codes.items);
            try enc.writeSection(output, .code, code_section.items);
        }
    }
};

// ============================================================================
// Code Builder (for function bodies)
// ============================================================================

pub const CodeBuilder = struct {
    allocator: std.mem.Allocator,
    buf: std.ArrayListUnmanaged(u8) = .{},
    local_count: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) CodeBuilder {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *CodeBuilder) void {
        self.buf.deinit(self.allocator);
    }

    fn writer(self: *CodeBuilder) std.ArrayListUnmanaged(u8).Writer {
        return self.buf.writer(self.allocator);
    }

    /// Emit i64.const instruction.
    pub fn emitI64Const(self: *CodeBuilder, value: i64) !void {
        try self.buf.append(self.allocator, Op.i64_const);
        try enc.encodeSLEB128(self.writer(), value);
    }

    /// Emit i32.const instruction.
    pub fn emitI32Const(self: *CodeBuilder, value: i32) !void {
        try self.buf.append(self.allocator, Op.i32_const);
        try enc.encodeSLEB128(self.writer(), value);
    }

    /// Emit local.get instruction.
    pub fn emitLocalGet(self: *CodeBuilder, idx: u32) !void {
        try self.buf.append(self.allocator, Op.local_get);
        try enc.encodeULEB128(self.writer(), idx);
    }

    /// Emit local.set instruction.
    pub fn emitLocalSet(self: *CodeBuilder, idx: u32) !void {
        try self.buf.append(self.allocator, Op.local_set);
        try enc.encodeULEB128(self.writer(), idx);
    }

    /// Emit global.get instruction.
    pub fn emitGlobalGet(self: *CodeBuilder, idx: u32) !void {
        try self.buf.append(self.allocator, Op.global_get);
        try enc.encodeULEB128(self.writer(), idx);
    }

    /// Emit global.set instruction.
    pub fn emitGlobalSet(self: *CodeBuilder, idx: u32) !void {
        try self.buf.append(self.allocator, Op.global_set);
        try enc.encodeULEB128(self.writer(), idx);
    }

    /// Emit i64.add instruction.
    pub fn emitI64Add(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_add);
    }

    /// Emit i64.sub instruction.
    pub fn emitI64Sub(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_sub);
    }

    /// Emit i64.mul instruction.
    pub fn emitI64Mul(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_mul);
    }

    /// Emit i64.div_s instruction.
    pub fn emitI64DivS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_div_s);
    }

    /// Emit i64.rem_s instruction.
    pub fn emitI64RemS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_rem_s);
    }

    /// Emit i64.eq instruction.
    pub fn emitI64Eq(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_eq);
    }

    /// Emit i64.ne instruction.
    pub fn emitI64Ne(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_ne);
    }

    /// Emit i64.lt_s instruction.
    pub fn emitI64LtS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_lt_s);
    }

    /// Emit i64.le_s instruction.
    pub fn emitI64LeS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_le_s);
    }

    /// Emit i64.gt_s instruction.
    pub fn emitI64GtS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_gt_s);
    }

    /// Emit i64.ge_s instruction.
    pub fn emitI64GeS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_ge_s);
    }

    /// Emit i64.and instruction.
    pub fn emitI64And(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_and);
    }

    /// Emit i64.or instruction.
    pub fn emitI64Or(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_or);
    }

    /// Emit i64.xor instruction.
    pub fn emitI64Xor(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_xor);
    }

    /// Emit i64.shl instruction.
    pub fn emitI64Shl(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_shl);
    }

    /// Emit i64.shr_s instruction.
    pub fn emitI64ShrS(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_shr_s);
    }

    /// Emit f64.const instruction.
    pub fn emitF64Const(self: *CodeBuilder, value: f64) !void {
        try self.buf.append(self.allocator, Op.f64_const);
        const bytes = @as([8]u8, @bitCast(value));
        try self.buf.appendSlice(self.allocator, &bytes);
    }

    /// Emit f64.add instruction.
    pub fn emitF64Add(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_add);
    }

    /// Emit f64.sub instruction.
    pub fn emitF64Sub(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_sub);
    }

    /// Emit f64.mul instruction.
    pub fn emitF64Mul(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_mul);
    }

    /// Emit f64.div instruction.
    pub fn emitF64Div(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_div);
    }

    /// Emit f64.eq instruction.
    pub fn emitF64Eq(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_eq);
    }

    /// Emit f64.ne instruction.
    pub fn emitF64Ne(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_ne);
    }

    /// Emit f64.lt instruction.
    pub fn emitF64Lt(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_lt);
    }

    /// Emit f64.le instruction.
    pub fn emitF64Le(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_le);
    }

    /// Emit f64.gt instruction.
    pub fn emitF64Gt(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_gt);
    }

    /// Emit f64.ge instruction.
    pub fn emitF64Ge(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_ge);
    }

    /// Emit f64.neg instruction.
    pub fn emitF64Neg(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.f64_neg);
    }

    /// Emit i32.eqz instruction (for boolean not).
    pub fn emitI32Eqz(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i32_eqz);
    }

    /// Emit i64.eqz instruction.
    pub fn emitI64Eqz(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i64_eqz);
    }

    /// Emit i32.add instruction.
    pub fn emitI32Add(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i32_add);
    }

    /// Emit i32.sub instruction.
    pub fn emitI32Sub(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i32_sub);
    }

    /// Emit call instruction.
    pub fn emitCall(self: *CodeBuilder, func_idx: u32) !void {
        try self.buf.append(self.allocator, Op.call);
        try enc.encodeULEB128(self.writer(), func_idx);
    }

    /// Emit drop instruction.
    pub fn emitDrop(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.drop);
    }

    /// Emit return instruction.
    pub fn emitReturn(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.return_op);
    }

    /// Emit end instruction.
    pub fn emitEnd(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.end);
    }

    /// Emit block instruction with result type.
    /// block_type: 0x40 = void, 0x7F = i32, 0x7E = i64, 0x7D = f32, 0x7C = f64
    pub fn emitBlock(self: *CodeBuilder, block_type: u8) !void {
        try self.buf.append(self.allocator, Op.block);
        try self.buf.append(self.allocator, block_type);
    }

    /// Emit loop instruction with result type.
    pub fn emitLoop(self: *CodeBuilder, block_type: u8) !void {
        try self.buf.append(self.allocator, Op.loop);
        try self.buf.append(self.allocator, block_type);
    }

    /// Emit if instruction with result type.
    pub fn emitIf(self: *CodeBuilder, block_type: u8) !void {
        try self.buf.append(self.allocator, Op.if_op);
        try self.buf.append(self.allocator, block_type);
    }

    /// Emit else instruction.
    pub fn emitElse(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.else_op);
    }

    /// Emit br instruction (unconditional branch).
    pub fn emitBr(self: *CodeBuilder, label_idx: u32) !void {
        try self.buf.append(self.allocator, Op.br);
        try enc.encodeULEB128(self.writer(), label_idx);
    }

    /// Emit br_if instruction (conditional branch).
    pub fn emitBrIf(self: *CodeBuilder, label_idx: u32) !void {
        try self.buf.append(self.allocator, Op.br_if);
        try enc.encodeULEB128(self.writer(), label_idx);
    }

    /// Emit unreachable instruction.
    pub fn emitUnreachable(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.unreachable_op);
    }

    /// Emit nop instruction.
    pub fn emitNop(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.nop);
    }

    /// Emit i32.wrap_i64 instruction.
    pub fn emitI32WrapI64(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.i32_wrap_i64);
    }

    /// Emit i64.load instruction with alignment and offset.
    /// Memory address should be on the stack (i32).
    pub fn emitI64Load(self: *CodeBuilder, align_log2: u32, offset: u32) !void {
        try self.buf.append(self.allocator, Op.i64_load);
        try enc.encodeULEB128(self.writer(), align_log2); // alignment (log2)
        try enc.encodeULEB128(self.writer(), offset); // offset
    }

    /// Emit i64.store instruction with alignment and offset.
    /// Stack: [address (i32), value (i64)] -> []
    pub fn emitI64Store(self: *CodeBuilder, align_log2: u32, offset: u32) !void {
        try self.buf.append(self.allocator, Op.i64_store);
        try enc.encodeULEB128(self.writer(), align_log2); // alignment (log2)
        try enc.encodeULEB128(self.writer(), offset); // offset
    }

    /// Emit i32.load instruction with alignment and offset.
    pub fn emitI32Load(self: *CodeBuilder, align_log2: u32, offset: u32) !void {
        try self.buf.append(self.allocator, Op.i32_load);
        try enc.encodeULEB128(self.writer(), align_log2);
        try enc.encodeULEB128(self.writer(), offset);
    }

    /// Emit i32.store instruction with alignment and offset.
    pub fn emitI32Store(self: *CodeBuilder, align_log2: u32, offset: u32) !void {
        try self.buf.append(self.allocator, Op.i32_store);
        try enc.encodeULEB128(self.writer(), align_log2);
        try enc.encodeULEB128(self.writer(), offset);
    }

    /// Set the number of locals (beyond parameters).
    pub fn setLocalCount(self: *CodeBuilder, count: u32) void {
        self.local_count = count;
    }

    /// Finish building and return the function body bytes.
    /// Includes local declarations and end opcode.
    pub fn finish(self: *CodeBuilder) ![]const u8 {
        var body: std.ArrayListUnmanaged(u8) = .{};
        errdefer body.deinit(self.allocator);

        // Local declarations
        if (self.local_count > 0) {
            // One entry: count of i64 locals
            try enc.encodeULEB128(body.writer(self.allocator), 1); // 1 local type group
            try enc.encodeULEB128(body.writer(self.allocator), self.local_count); // count
            try body.append(self.allocator, @intFromEnum(ValType.i64)); // type
        } else {
            try enc.encodeULEB128(body.writer(self.allocator), 0); // no locals
        }

        // Instructions
        try body.appendSlice(self.allocator, self.buf.items);

        // End
        try body.append(self.allocator, Op.end);

        return body.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "emit minimal module - return 42" {
    const allocator = std.testing.allocator;

    var module = Module.init(allocator);
    defer module.deinit();

    // Add type: () -> i64
    const type_idx = try module.addFuncType(&[_]ValType{}, &[_]ValType{.i64});

    // Add function
    const func_idx = try module.addFunc(type_idx);

    // Add export
    try module.addExport("answer", .func, func_idx);

    // Build function body: return 42
    var code = CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(42);
    const body = try code.finish();
    defer allocator.free(body);
    try module.addCode(body);

    // Emit
    var output: std.ArrayListUnmanaged(u8) = .{};
    defer output.deinit(allocator);
    try module.emit(output.writer(allocator));

    // Verify header
    try std.testing.expectEqualSlices(u8, "\x00asm", output.items[0..4]);
    try std.testing.expectEqual(@as(u8, 1), output.items[4]); // version

    // The module should be valid Wasm - test with wasmtime in integration tests
    try std.testing.expect(output.items.len > 8);
}

test "emit add function" {
    const allocator = std.testing.allocator;

    var module = Module.init(allocator);
    defer module.deinit();

    // Add type: (i64, i64) -> i64
    const type_idx = try module.addFuncType(&[_]ValType{ .i64, .i64 }, &[_]ValType{.i64});

    // Add function
    const func_idx = try module.addFunc(type_idx);

    // Add export
    try module.addExport("add", .func, func_idx);

    // Build function body: return a + b
    var code = CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitLocalGet(0); // a
    try code.emitLocalGet(1); // b
    try code.emitI64Add();
    const body = try code.finish();
    defer allocator.free(body);
    try module.addCode(body);

    // Emit
    var output: std.ArrayListUnmanaged(u8) = .{};
    defer output.deinit(allocator);
    try module.emit(output.writer(allocator));

    // Verify it's non-empty valid-looking Wasm
    try std.testing.expectEqualSlices(u8, "\x00asm", output.items[0..4]);
    try std.testing.expect(output.items.len > 20);
}

test "code builder - i64 const" {
    const allocator = std.testing.allocator;

    var code = CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitI64Const(42);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        Op.i64_const, // 0x42
        42,           // LEB128(42)
    }, code.buf.items);
}

test "code builder - negative const" {
    const allocator = std.testing.allocator;

    var code = CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitI64Const(-1);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        Op.i64_const, // 0x42
        0x7F,         // LEB128(-1)
    }, code.buf.items);
}

test "code builder - local get" {
    const allocator = std.testing.allocator;

    var code = CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitLocalGet(0);
    try code.emitLocalGet(1);

    try std.testing.expectEqualSlices(u8, &[_]u8{
        Op.local_get, 0,
        Op.local_get, 1,
    }, code.buf.items);
}
