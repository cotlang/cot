//! WebAssembly code generator.
//! Compiles Cot IR to WebAssembly binary format.

const std = @import("std");
const op = @import("wasm_opcodes.zig");
const enc = @import("wasm_encode.zig");

pub const Op = op.Op;
pub const ValType = op.ValType;
pub const Section = op.Section;
pub const ExportKind = op.ExportKind;

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

    pub fn init(allocator: std.mem.Allocator) Module {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Module) void {
        self.types.deinit(self.allocator);
        self.funcs.deinit(self.allocator);
        self.exports.deinit(self.allocator);
        self.codes.deinit(self.allocator);
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

    /// Emit return instruction.
    pub fn emitReturn(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.return_op);
    }

    /// Emit end instruction.
    pub fn emitEnd(self: *CodeBuilder) !void {
        try self.buf.append(self.allocator, Op.end);
    }

    /// Finish building and return the function body bytes.
    /// Includes local declarations (empty for now) and end opcode.
    pub fn finish(self: *CodeBuilder) ![]const u8 {
        var body: std.ArrayListUnmanaged(u8) = .{};
        errdefer body.deinit(self.allocator);

        // Local declarations (0 for now)
        try enc.encodeULEB128(body.writer(self.allocator), 0);

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
