//! Link - Write complete Wasm module
//!
//! Go reference: cmd/link/internal/wasm/asm.go
//!
//! This writes the final WebAssembly module binary with all sections:
//! - Type section: function signatures
//! - Import section: host imports (future)
//! - Function section: function type indices
//! - Table section: indirect call table (future)
//! - Memory section: linear memory
//! - Global section: global variables (SP, etc.)
//! - Export section: exported functions
//! - Element section: table initialization (future)
//! - Code section: function bodies
//! - Data section: static data (future)

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const assemble = @import("assemble.zig");
const Symbol = prog.Symbol;

// Debug logging - conditionally import if available
const debug_enabled = false; // Set to true when integrated with main build
fn debugLog(comptime fmt: []const u8, args: anytype) void {
    if (debug_enabled) {
        @import("std").debug.print(fmt ++ "\n", args);
    }
}

/// Function type signature
pub const FuncType = struct {
    params: []const c.ValType,
    results: []const c.ValType,

    pub fn eql(self: FuncType, other: FuncType) bool {
        return std.mem.eql(c.ValType, self.params, other.params) and
            std.mem.eql(c.ValType, self.results, other.results);
    }
};

/// Assembled function ready for linking
pub const WasmFunc = struct {
    name: []const u8,
    type_idx: u32,
    code: []const u8,
    exported: bool = false,

    pub fn deinit(self: *WasmFunc, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
    }
};

/// Module linker state
pub const Linker = struct {
    allocator: std.mem.Allocator,

    // Type section
    types: std.ArrayListUnmanaged(FuncType) = .{},
    type_storage: std.ArrayListUnmanaged(c.ValType) = .{}, // Storage for param/result slices

    // Functions
    funcs: std.ArrayListUnmanaged(WasmFunc) = .{},

    // Memory configuration
    memory_min_pages: u32 = 1, // 64KB minimum
    memory_max_pages: ?u32 = null,

    // Data segments (for future)
    data_segments: std.ArrayListUnmanaged(DataSegment) = .{},

    pub fn init(allocator: std.mem.Allocator) Linker {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Linker) void {
        for (self.funcs.items) |*f| {
            f.deinit(self.allocator);
        }
        self.funcs.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.type_storage.deinit(self.allocator);
        self.data_segments.deinit(self.allocator);
    }

    /// Add or find a function type, return its index
    pub fn addType(self: *Linker, params: []const c.ValType, results: []const c.ValType) !u32 {
        const sig = FuncType{ .params = params, .results = results };

        // Check if type already exists
        for (self.types.items, 0..) |existing, i| {
            if (sig.eql(existing)) {
                return @intCast(i);
            }
        }

        // Store the slices
        const params_start = self.type_storage.items.len;
        try self.type_storage.appendSlice(self.allocator, params);
        const params_end = self.type_storage.items.len;

        const results_start = self.type_storage.items.len;
        try self.type_storage.appendSlice(self.allocator, results);
        const results_end = self.type_storage.items.len;

        // Add new type with references to stored slices
        try self.types.append(self.allocator, .{
            .params = self.type_storage.items[params_start..params_end],
            .results = self.type_storage.items[results_start..results_end],
        });

        return @intCast(self.types.items.len - 1);
    }

    /// Add a function
    pub fn addFunc(self: *Linker, func: WasmFunc) !u32 {
        const idx: u32 = @intCast(self.funcs.items.len);
        try self.funcs.append(self.allocator, func);
        return idx;
    }

    /// Set memory size
    pub fn setMemory(self: *Linker, min_pages: u32, max_pages: ?u32) void {
        self.memory_min_pages = min_pages;
        self.memory_max_pages = max_pages;
    }

    /// Link and emit the Wasm module
    pub fn emit(self: *Linker, writer: anytype) !void {
        debugLog( "link: emitting module with {d} types, {d} funcs", .{
            self.types.items.len,
            self.funcs.items.len,
        });

        // ====================================================================
        // Header
        // ====================================================================
        try writer.writeAll(&c.WASM_MAGIC);
        try writer.writeAll(&c.WASM_VERSION);

        // ====================================================================
        // Type Section (Go: asm.go writeTypeSec)
        // ====================================================================
        if (self.types.items.len > 0) {
            var type_buf = std.ArrayListUnmanaged(u8){};
            defer type_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &type_buf, self.types.items.len);
            for (self.types.items) |t| {
                try type_buf.append(self.allocator, c.FUNC_TYPE_TAG); // 0x60
                try assemble.writeULEB128(self.allocator, &type_buf, t.params.len);
                for (t.params) |p| {
                    try type_buf.append(self.allocator, @intFromEnum(p));
                }
                try assemble.writeULEB128(self.allocator, &type_buf, t.results.len);
                for (t.results) |r| {
                    try type_buf.append(self.allocator, @intFromEnum(r));
                }
            }
            try writeSection(writer, self.allocator, .type, type_buf.items);
        }

        // ====================================================================
        // Function Section (Go: asm.go writeFunctionSec)
        // ====================================================================
        if (self.funcs.items.len > 0) {
            var func_buf = std.ArrayListUnmanaged(u8){};
            defer func_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &func_buf, self.funcs.items.len);
            for (self.funcs.items) |f| {
                try assemble.writeULEB128(self.allocator, &func_buf, f.type_idx);
            }
            try writeSection(writer, self.allocator, .function, func_buf.items);
        }

        // ====================================================================
        // Memory Section (Go: asm.go writeMemorySec)
        // ====================================================================
        {
            var mem_buf = std.ArrayListUnmanaged(u8){};
            defer mem_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &mem_buf, 1); // 1 memory
            if (self.memory_max_pages) |max| {
                try mem_buf.append(self.allocator, 0x01); // limits with max
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
                try assemble.writeULEB128(self.allocator, &mem_buf, max);
            } else {
                try mem_buf.append(self.allocator, 0x00); // limits without max
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
            }
            try writeSection(writer, self.allocator, .memory, mem_buf.items);
        }

        // ====================================================================
        // Global Section (Go: asm.go writeGlobalSec)
        // ====================================================================
        // Go defines 8 globals: SP(i32), CTXT(i64), g(i64), RET0-3(i64), PAUSE(i32)
        // For Cot, we start with just SP
        {
            var global_buf = std.ArrayListUnmanaged(u8){};
            defer global_buf.deinit(self.allocator);

            const globals = [_]struct { typ: c.ValType, init: i64 }{
                .{ .typ = .i32, .init = 65536 }, // SP - start at 64KB
            };

            try assemble.writeULEB128(self.allocator, &global_buf, globals.len);
            for (globals) |g| {
                try global_buf.append(self.allocator, @intFromEnum(g.typ));
                try global_buf.append(self.allocator, 0x01); // mutable
                // Init expression
                if (g.typ == .i32) {
                    try global_buf.append(self.allocator, 0x41); // i32.const
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init);
                } else {
                    try global_buf.append(self.allocator, 0x42); // i64.const
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init);
                }
                try global_buf.append(self.allocator, 0x0B); // end
            }
            try writeSection(writer, self.allocator, .global, global_buf.items);
        }

        // ====================================================================
        // Export Section (Go: asm.go writeExportSec)
        // ====================================================================
        {
            var export_buf = std.ArrayListUnmanaged(u8){};
            defer export_buf.deinit(self.allocator);

            // Count exports
            var export_count: usize = 1; // Always export memory
            for (self.funcs.items) |f| {
                if (f.exported) export_count += 1;
            }

            try assemble.writeULEB128(self.allocator, &export_buf, export_count);

            // Export functions
            for (self.funcs.items, 0..) |f, i| {
                if (f.exported) {
                    try assemble.writeULEB128(self.allocator, &export_buf, f.name.len);
                    try export_buf.appendSlice(self.allocator, f.name);
                    try export_buf.append(self.allocator, @intFromEnum(c.ExportKind.func));
                    try assemble.writeULEB128(self.allocator, &export_buf, i);
                }
            }

            // Export memory
            const mem_name = "memory";
            try assemble.writeULEB128(self.allocator, &export_buf, mem_name.len);
            try export_buf.appendSlice(self.allocator, mem_name);
            try export_buf.append(self.allocator, @intFromEnum(c.ExportKind.memory));
            try assemble.writeULEB128(self.allocator, &export_buf, 0); // memory index 0

            try writeSection(writer, self.allocator, .@"export", export_buf.items);
        }

        // ====================================================================
        // Code Section (Go: asm.go writeCodeSec)
        // ====================================================================
        if (self.funcs.items.len > 0) {
            var code_buf = std.ArrayListUnmanaged(u8){};
            defer code_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &code_buf, self.funcs.items.len);
            for (self.funcs.items) |f| {
                try assemble.writeULEB128(self.allocator, &code_buf, f.code.len);
                try code_buf.appendSlice(self.allocator, f.code);
            }
            try writeSection(writer, self.allocator, .code, code_buf.items);
        }

        debugLog( "  link complete", .{});
    }
};

/// Data segment for data section
pub const DataSegment = struct {
    offset: i32,
    data: []const u8,
};

/// Write a section with ID and content
fn writeSection(writer: anytype, allocator: std.mem.Allocator, section: c.Section, content: []const u8) !void {
    try writer.writeByte(@intFromEnum(section));

    // Write size as LEB128
    var size_buf = std.ArrayListUnmanaged(u8){};
    defer size_buf.deinit(allocator);
    try assemble.writeULEB128(allocator, &size_buf, content.len);
    try writer.writeAll(size_buf.items);

    try writer.writeAll(content);
}

// ============================================================================
// High-level API for compiling a function
// ============================================================================

/// Compile a symbol (function) and add it to the linker
pub fn compileFunc(
    linker: *Linker,
    sym: *Symbol,
    params: []const c.ValType,
    results: []const c.ValType,
    exported: bool,
) !u32 {
    // Add the function type
    const type_idx = try linker.addType(params, results);

    // Assemble the function body
    var assembled = try assemble.assemble(linker.allocator, sym);
    errdefer assembled.deinit();

    // Add to linker
    return try linker.addFunc(.{
        .name = sym.name,
        .type_idx = type_idx,
        .code = assembled.code,
        .exported = exported,
    });
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "linker basic" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    // Add a simple function type () -> i64
    const type_idx = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), type_idx);

    // Same type should return same index
    const type_idx2 = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), type_idx2);

    // Different type gets new index
    const type_idx3 = try linker.addType(&[_]c.ValType{ .i64, .i64 }, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 1), type_idx3);
}

test "linker emit" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    // Add type
    const type_idx = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});

    // Add function with simple body: i64.const 42, end
    const code = &[_]u8{
        0x00, // 0 locals
        0x42, 0x2A, // i64.const 42
        0x0B, // end
    };
    _ = try linker.addFunc(.{
        .name = "main",
        .type_idx = type_idx,
        .code = try allocator.dupe(u8, code),
        .exported = true,
    });

    // Emit
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try linker.emit(output.writer(allocator));

    // Verify header
    try testing.expectEqualSlices(u8, &c.WASM_MAGIC, output.items[0..4]);
    try testing.expectEqual(@as(u8, 1), output.items[4]); // version
}
