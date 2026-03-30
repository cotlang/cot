//! WebAssembly binary format parser for AOT compilation.
//!
//! Parses Wasm modules to extract types, functions, exports, and code bodies.
//! Used by the AOT pipeline: Wasm -> SSA -> Native.

const std = @import("std");
const wasm = @import("../wasm_opcodes.zig");
const leb = @import("../wasm_encode.zig");

// ============================================================================
// Parsed Module Structures
// ============================================================================

pub const WasmModule = struct {
    allocator: std.mem.Allocator,
    types: []FuncType,
    funcs: []u32, // function index -> type index
    memory: ?MemoryType,
    globals: []GlobalType,
    exports: []Export,
    code: []FuncCode,
    data_segments: []DataSegment,
    /// Table element entries: table_index -> function_index mapping.
    /// Populated from the element section. Used for AOT call_indirect.
    table_elements: []u32,

    pub fn deinit(self: *WasmModule) void {
        for (self.types) |*t| {
            self.allocator.free(t.params);
            self.allocator.free(t.results);
        }
        self.allocator.free(self.types);
        self.allocator.free(self.funcs);
        self.allocator.free(self.globals);
        for (self.exports) |*e| self.allocator.free(e.name);
        self.allocator.free(self.exports);
        for (self.code) |*c| self.allocator.free(c.locals);
        self.allocator.free(self.code);
        for (self.data_segments) |*d| self.allocator.free(d.data);
        self.allocator.free(self.data_segments);
        self.allocator.free(self.table_elements);
    }
};

pub const DataSegment = struct {
    memory_index: u32,
    offset: u32, // offset in linear memory
    data: []const u8,
};

pub const FuncType = struct {
    params: []wasm.ValType,
    results: []wasm.ValType,
};

pub const MemoryType = struct {
    min: u32,
    max: ?u32,
};

pub const GlobalType = struct {
    val_type: wasm.ValType,
    mutable: bool,
    init_value: i64,
};

pub const Export = struct {
    name: []const u8,
    kind: wasm.ExportKind,
    index: u32,
};

pub const FuncCode = struct {
    locals: []LocalDecl,
    body: []const u8, // raw bytecode (slice into original bytes)
};

pub const LocalDecl = struct {
    count: u32,
    val_type: wasm.ValType,
};

// ============================================================================
// Parser
// ============================================================================

pub const ParseError = error{
    InvalidMagic,
    InvalidVersion,
    UnexpectedEnd,
    InvalidSection,
    InvalidType,
    InvalidValType,
    OutOfMemory,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    bytes: []const u8,
    pos: usize,

    // Accumulated results
    types: std.ArrayListUnmanaged(FuncType),
    funcs: std.ArrayListUnmanaged(u32),
    memory: ?MemoryType,
    globals: std.ArrayListUnmanaged(GlobalType),
    exports: std.ArrayListUnmanaged(Export),
    code: std.ArrayListUnmanaged(FuncCode),
    data_segments: std.ArrayListUnmanaged(DataSegment),
    table_elements: std.ArrayListUnmanaged(u32),

    pub fn init(allocator: std.mem.Allocator, bytes: []const u8) Parser {
        return .{
            .allocator = allocator,
            .bytes = bytes,
            .pos = 0,
            .types = .{},
            .funcs = .{},
            .memory = null,
            .globals = .{},
            .exports = .{},
            .code = .{},
            .data_segments = .{},
            .table_elements = .{},
        };
    }

    pub fn deinit(self: *Parser) void {
        for (self.types.items) |*t| {
            self.allocator.free(t.params);
            self.allocator.free(t.results);
        }
        self.types.deinit(self.allocator);
        self.funcs.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        for (self.exports.items) |*e| self.allocator.free(e.name);
        self.exports.deinit(self.allocator);
        for (self.code.items) |*c| self.allocator.free(c.locals);
        self.code.deinit(self.allocator);
        for (self.data_segments.items) |*d| self.allocator.free(d.data);
        self.data_segments.deinit(self.allocator);
        self.table_elements.deinit(self.allocator);
    }

    pub fn parse(self: *Parser) !WasmModule {
        try self.parseHeader();
        try self.parseSections();
        return .{
            .allocator = self.allocator,
            .types = try self.types.toOwnedSlice(self.allocator),
            .funcs = try self.funcs.toOwnedSlice(self.allocator),
            .memory = self.memory,
            .globals = try self.globals.toOwnedSlice(self.allocator),
            .exports = try self.exports.toOwnedSlice(self.allocator),
            .code = try self.code.toOwnedSlice(self.allocator),
            .data_segments = try self.data_segments.toOwnedSlice(self.allocator),
            .table_elements = try self.table_elements.toOwnedSlice(self.allocator),
        };
    }

    fn parseHeader(self: *Parser) !void {
        if (self.bytes.len < 8) return ParseError.UnexpectedEnd;
        if (!std.mem.eql(u8, self.bytes[0..4], &wasm.MAGIC)) return ParseError.InvalidMagic;
        if (!std.mem.eql(u8, self.bytes[4..8], &wasm.VERSION)) return ParseError.InvalidVersion;
        self.pos = 8;
    }

    fn parseSections(self: *Parser) !void {
        while (self.pos < self.bytes.len) {
            const section_id = self.readByte() orelse return;
            const section_len = self.readULEB128();
            const section_end = self.pos + @as(usize, @intCast(section_len));

            switch (section_id) {
                @intFromEnum(wasm.Section.type) => try self.parseTypeSection(section_end),
                @intFromEnum(wasm.Section.function) => try self.parseFunctionSection(section_end),
                @intFromEnum(wasm.Section.table) => self.pos = section_end, // Table declaration (we only need element data)
                @intFromEnum(wasm.Section.memory) => try self.parseMemorySection(section_end),
                @intFromEnum(wasm.Section.global) => try self.parseGlobalSection(section_end),
                @intFromEnum(wasm.Section.@"export") => try self.parseExportSection(section_end),
                @intFromEnum(wasm.Section.element) => try self.parseElementSection(section_end),
                @intFromEnum(wasm.Section.code) => try self.parseCodeSection(section_end),
                @intFromEnum(wasm.Section.data) => try self.parseDataSection(section_end),
                else => self.pos = section_end, // Skip unknown sections
            }
        }
    }

    fn parseTypeSection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const tag = self.readByte() orelse return ParseError.UnexpectedEnd;
            if (tag != wasm.FUNC_TYPE_TAG) return ParseError.InvalidType;

            const param_count = self.readULEB128();
            var params = try self.allocator.alloc(wasm.ValType, @intCast(param_count));
            for (0..@intCast(param_count)) |i| {
                params[i] = try self.readValType();
            }

            const result_count = self.readULEB128();
            var results = try self.allocator.alloc(wasm.ValType, @intCast(result_count));
            for (0..@intCast(result_count)) |i| {
                results[i] = try self.readValType();
            }

            try self.types.append(self.allocator, .{ .params = params, .results = results });
        }
    }

    fn parseFunctionSection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const type_idx = self.readULEB128();
            try self.funcs.append(self.allocator, @intCast(type_idx));
        }
    }

    fn parseMemorySection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        if (count > 0) {
            const flags = self.readByte() orelse return ParseError.UnexpectedEnd;
            const min = self.readULEB128();
            const max: ?u32 = if (flags & wasm.LIMITS_WITH_MAX != 0) @intCast(self.readULEB128()) else null;
            self.memory = .{ .min = @intCast(min), .max = max };
        }
    }

    fn parseGlobalSection(self: *Parser, end: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const val_type = try self.readValType();
            const mutable = (self.readByte() orelse return ParseError.UnexpectedEnd) != 0;

            // Parse init expression (simplified: only i32.const/i64.const + end)
            var init_value: i64 = 0;
            const opcode = self.readByte() orelse return ParseError.UnexpectedEnd;
            if (opcode == wasm.Op.i32_const or opcode == wasm.Op.i64_const) {
                init_value = self.readSLEB128();
            }
            // Skip to end opcode
            while (self.pos < end) {
                if ((self.readByte() orelse break) == wasm.Op.end) break;
            }

            try self.globals.append(self.allocator, .{
                .val_type = val_type,
                .mutable = mutable,
                .init_value = init_value,
            });
        }
    }

    fn parseExportSection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const name_len = self.readULEB128();
            const name = try self.allocator.dupe(u8, self.bytes[self.pos .. self.pos + @as(usize, @intCast(name_len))]);
            self.pos += @intCast(name_len);

            const kind_byte = self.readByte() orelse return ParseError.UnexpectedEnd;
            const kind: wasm.ExportKind = @enumFromInt(kind_byte);
            const index = self.readULEB128();

            try self.exports.append(self.allocator, .{
                .name = name,
                .kind = kind,
                .index = @intCast(index),
            });
        }
    }

    fn parseCodeSection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const func_size = self.readULEB128();
            const func_end = self.pos + @as(usize, @intCast(func_size));

            // Parse locals
            const local_count = self.readULEB128();
            var locals = try self.allocator.alloc(LocalDecl, @intCast(local_count));
            for (0..@intCast(local_count)) |i| {
                const n = self.readULEB128();
                const t = try self.readValType();
                locals[i] = .{ .count = @intCast(n), .val_type = t };
            }

            // Body is the remaining bytes until func_end
            const body = self.bytes[self.pos..func_end];
            self.pos = func_end;

            try self.code.append(self.allocator, .{ .locals = locals, .body = body });
        }
    }

    fn parseDataSection(self: *Parser, end: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            // Data segment kind: 0 = active (memory 0), 1 = passive, 2 = active (explicit mem)
            const kind = self.readULEB128();

            var memory_index: u32 = 0;
            var offset: u32 = 0;

            if (kind == 0) {
                // Active data segment for memory 0
                // Parse offset expression (simplified: i32.const N + end)
                const opcode = self.readByte() orelse return ParseError.UnexpectedEnd;
                if (opcode == wasm.Op.i32_const) {
                    const val = self.readSLEB128();
                    offset = @intCast(@as(u32, @bitCast(@as(i32, @truncate(val)))));
                }
                // Skip to end opcode
                while (self.pos < end) {
                    if ((self.readByte() orelse break) == wasm.Op.end) break;
                }
            } else if (kind == 2) {
                // Active data segment with explicit memory index
                memory_index = @intCast(self.readULEB128());
                const opcode = self.readByte() orelse return ParseError.UnexpectedEnd;
                if (opcode == wasm.Op.i32_const) {
                    const val = self.readSLEB128();
                    offset = @intCast(@as(u32, @bitCast(@as(i32, @truncate(val)))));
                }
                while (self.pos < end) {
                    if ((self.readByte() orelse break) == wasm.Op.end) break;
                }
            }
            // kind == 1 is passive (no offset expr), just read data

            // Read data bytes
            const data_len = self.readULEB128();
            const data = try self.allocator.alloc(u8, @intCast(data_len));
            @memcpy(data, self.bytes[self.pos..][0..@intCast(data_len)]);
            self.pos += @intCast(data_len);

            try self.data_segments.append(self.allocator, .{
                .memory_index = memory_index,
                .offset = offset,
                .data = data,
            });
        }
    }

    /// Parse the element section (section 9).
    /// Extracts function indices for active table initialization segments.
    /// Reference: wasmparser readers/core/elements.rs — flag-based parsing
    /// Wasm spec: flags encode kind (bit 0 = passive/declared, bit 1 = explicit table, bit 2 = expressions)
    fn parseElementSection(self: *Parser, _: usize) !void {
        const count = self.readULEB128();
        for (0..@intCast(count)) |_| {
            const flags: u32 = @intCast(self.readULEB128());

            // wasmparser: bit 0 determines active vs passive/declared
            const is_active = (flags & 0b001) == 0;
            const has_explicit_table = (flags & 0b010) != 0;
            const has_expressions = (flags & 0b100) != 0;

            // Active segments have optional table index + offset expression
            if (is_active) {
                if (has_explicit_table) {
                    _ = self.readULEB128(); // table_index (we only use table 0)
                }
                self.skipConstExpr(); // offset expression
            }

            // Non-kind-0 segments have an elemkind or reftype byte
            // wasmparser: `if flags & 0b011 != 0` — covers kinds 1, 2, 3 (and 5, 6, 7)
            if ((flags & 0b011) != 0) {
                _ = self.readByte(); // elemkind (0x00 = funcref) or reftype
            }

            // Read element items
            const num_elems = self.readULEB128();
            if (has_expressions) {
                // Expression items — skip const exprs (we don't need them for AOT)
                for (0..@intCast(num_elems)) |_| {
                    self.skipConstExpr();
                }
            } else {
                // Function index items
                for (0..@intCast(num_elems)) |_| {
                    const func_idx: u32 = @intCast(self.readULEB128());
                    // Only store entries from active segments (passive/declared don't initialize table)
                    if (is_active) {
                        try self.table_elements.append(self.allocator, func_idx);
                    }
                }
            }
        }
    }

    /// Skip a constant expression (instructions ending with 0x0B end opcode).
    /// Reference: wasmparser ConstExpr parsing — reads opcodes with their operands
    fn skipConstExpr(self: *Parser) void {
        while (self.pos < self.bytes.len) {
            const opcode = self.readByte() orelse return;
            if (opcode == wasm.Op.end) return;
            switch (opcode) {
                wasm.Op.i32_const, wasm.Op.i64_const => _ = self.readSLEB128(),
                wasm.Op.f32_const => self.pos += 4,
                wasm.Op.f64_const => self.pos += 8,
                wasm.Op.global_get => _ = self.readULEB128(),
                wasm.REF_NULL => _ = self.readByte(), // ref.null heaptype
                wasm.REF_FUNC => _ = self.readULEB128(), // ref.func funcidx
                else => {},
            }
        }
    }

    // ========================================================================
    // Reading primitives
    // ========================================================================

    fn readByte(self: *Parser) ?u8 {
        if (self.pos >= self.bytes.len) return null;
        const b = self.bytes[self.pos];
        self.pos += 1;
        return b;
    }

    fn readULEB128(self: *Parser) u64 {
        var result: u64 = 0;
        var shift: u6 = 0;
        while (true) {
            const byte = self.readByte() orelse return result;
            result |= @as(u64, byte & leb.LEB128_LOW_BITS_MASK) << shift;
            if (byte & leb.LEB128_CONTINUATION_BIT == 0) return result;
            shift +|= 7;
        }
    }

    fn readSLEB128(self: *Parser) i64 {
        var result: i64 = 0;
        var shift: u6 = 0;
        var byte: u8 = 0;
        while (true) {
            byte = self.readByte() orelse return result;
            const low7: u7 = @truncate(byte);
            result |= @as(i64, low7) << shift;
            shift +|= 7;
            if (byte & leb.LEB128_CONTINUATION_BIT == 0) break;
        }
        // Sign extend
        if (shift < 64 and (byte & leb.LEB128_SIGN_BIT) != 0) {
            result |= ~@as(i64, 0) << shift;
        }
        return result;
    }

    fn readValType(self: *Parser) !wasm.ValType {
        const byte = self.readByte() orelse return ParseError.UnexpectedEnd;
        return std.meta.intToEnum(wasm.ValType, byte) catch ParseError.InvalidValType;
    }
};

/// Parse a Wasm module from bytes.
pub fn parse(allocator: std.mem.Allocator, bytes: []const u8) !WasmModule {
    var parser = Parser.init(allocator, bytes);
    errdefer parser.deinit();
    return parser.parse();
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "parse minimal module" {
    // Minimal valid Wasm: just header
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
    };
    var module = try parse(testing.allocator, &bytes);
    defer module.deinit();

    try testing.expectEqual(@as(usize, 0), module.types.len);
    try testing.expectEqual(@as(usize, 0), module.funcs.len);
    try testing.expectEqual(@as(usize, 0), module.exports.len);
    try testing.expectEqual(@as(usize, 0), module.code.len);
}

test "parse invalid magic" {
    const bytes = [_]u8{ 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00 };
    const result = parse(testing.allocator, &bytes);
    try testing.expectError(ParseError.InvalidMagic, result);
}

test "parse type section" {
    // Module with one type: fn(i64, i64) -> i64
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        0x01,                   // type section id
        0x07,                   // section length
        0x01,                   // 1 type
        0x60,                   // func type tag
        0x02,                   // 2 params
        0x7E, 0x7E,             // i64, i64
        0x01,                   // 1 result
        0x7E,                   // i64
    };
    var module = try parse(testing.allocator, &bytes);
    defer module.deinit();

    try testing.expectEqual(@as(usize, 1), module.types.len);
    try testing.expectEqual(@as(usize, 2), module.types[0].params.len);
    try testing.expectEqual(wasm.ValType.i64, module.types[0].params[0]);
    try testing.expectEqual(wasm.ValType.i64, module.types[0].params[1]);
    try testing.expectEqual(@as(usize, 1), module.types[0].results.len);
    try testing.expectEqual(wasm.ValType.i64, module.types[0].results[0]);
}

test "parse function and code sections" {
    // Module: (func (result i64) (i64.const 42))
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Type section
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7E, // fn() -> i64
        // Function section
        0x03, 0x02, 0x01, 0x00, // 1 function, type 0
        // Code section
        0x0A, 0x06, 0x01, // 1 function body
        0x04,             // body size = 4
        0x00,             // 0 locals
        0x42, 0x2A,       // i64.const 42
        0x0B,             // end
    };
    var module = try parse(testing.allocator, &bytes);
    defer module.deinit();

    try testing.expectEqual(@as(usize, 1), module.funcs.len);
    try testing.expectEqual(@as(u32, 0), module.funcs[0]); // type index 0
    try testing.expectEqual(@as(usize, 1), module.code.len);
    try testing.expectEqual(@as(usize, 0), module.code[0].locals.len);
    try testing.expect(module.code[0].body.len >= 3); // i64.const 42 end
}

test "parse export section" {
    // Module with export "main" -> func 0
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Export section
        0x07, 0x08,                   // section id, length
        0x01,                         // 1 export
        0x04, 'm', 'a', 'i', 'n',     // name "main"
        0x00,                         // func kind
        0x00,                         // func index 0
    };
    var module = try parse(testing.allocator, &bytes);
    defer module.deinit();

    try testing.expectEqual(@as(usize, 1), module.exports.len);
    try testing.expectEqualStrings("main", module.exports[0].name);
    try testing.expectEqual(wasm.ExportKind.func, module.exports[0].kind);
    try testing.expectEqual(@as(u32, 0), module.exports[0].index);
}

test "ULEB128 decoding" {
    var parser = Parser.init(testing.allocator, &[_]u8{ 0xE5, 0x8E, 0x26 });
    const value = parser.readULEB128();
    try testing.expectEqual(@as(u64, 624485), value);
}

test "SLEB128 decoding negative" {
    var parser = Parser.init(testing.allocator, &[_]u8{ 0xC0, 0xBB, 0x78 });
    const value = parser.readSLEB128();
    try testing.expectEqual(@as(i64, -123456), value);
}
