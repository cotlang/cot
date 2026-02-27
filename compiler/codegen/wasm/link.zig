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
    params_offset: u32,
    params_len: u32,
    results_offset: u32,
    results_len: u32,

    pub fn getParams(self: FuncType, storage: []const c.WasmType) []const c.WasmType {
        return storage[self.params_offset..][0..self.params_len];
    }

    pub fn getResults(self: FuncType, storage: []const c.WasmType) []const c.WasmType {
        return storage[self.results_offset..][0..self.results_len];
    }

    pub fn eqlWith(self: FuncType, other: FuncType, storage: []const c.WasmType) bool {
        const sp = self.getParams(storage);
        const op = other.getParams(storage);
        if (sp.len != op.len) return false;
        for (sp, op) |a, b| {
            if (!a.eql(b)) return false;
        }
        const sr = self.getResults(storage);
        const or_ = other.getResults(storage);
        if (sr.len != or_.len) return false;
        for (sr, or_) |a, b| {
            if (!a.eql(b)) return false;
        }
        return true;
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

/// WasmGC field type definition
pub const GcFieldType = struct {
    val_type: c.ValType,
    mutable: bool,
    /// If non-null, this field is a GC ref type (ref null $gc_ref_idx)
    gc_ref: ?u32 = null,
};

/// WasmGC struct type definition
pub const GcStructType = struct {
    name: []const u8,
    field_count: u32,
    field_offset: u32, // index into gc_field_storage
};

/// Host import definition (Go: hostImport in asm.go)
pub const WasmImport = struct {
    module: []const u8, // e.g., "env", "wasi_snapshot_preview1"
    name: []const u8, // e.g., "console_log"
    type_idx: u32, // Function type index
};

/// Global variable definition
pub const WasmGlobal = struct {
    val_type: c.ValType,
    mutable: bool,
    init_i32: i32 = 0,
    init_i64: i64 = 0,
};

/// Table element for indirect calls (destructors, vtables)
pub const TableEntry = struct {
    func_idx: u32, // Function index to call
};

/// Element segment for table initialization
pub const ElementSegment = struct {
    offset: u32, // Offset in table
    funcs: []const u32, // Function indices
};

/// Module linker state
pub const Linker = struct {
    allocator: std.mem.Allocator,

    // Type section
    types: std.ArrayListUnmanaged(FuncType) = .{},
    type_storage: std.ArrayListUnmanaged(c.WasmType) = .{}, // Storage for param/result slices

    // Import section (Go: hostImports in asm.go)
    imports: std.ArrayListUnmanaged(WasmImport) = .{},

    // Global section
    globals: std.ArrayListUnmanaged(WasmGlobal) = .{},

    // Functions
    funcs: std.ArrayListUnmanaged(WasmFunc) = .{},

    // Memory configuration
    memory_min_pages: u32 = 1, // 64KB minimum
    memory_max_pages: ?u32 = null,

    // Data segments (for future)
    data_segments: std.ArrayListUnmanaged(DataSegment) = .{},

    // Table for indirect calls (destructors, vtables)
    table_size: u32 = 0, // 0 means no table
    table_funcs: std.ArrayListUnmanaged(u32) = .{}, // Functions to put in table

    // WasmGC struct types (type indices 0..N-1, before function types)
    gc_struct_types: std.ArrayListUnmanaged(GcStructType) = .{},
    gc_field_storage: std.ArrayListUnmanaged(GcFieldType) = .{},
    gc_struct_name_map: std.StringHashMapUnmanaged(u32) = .{}, // struct name → gc type index

    pub fn init(allocator: std.mem.Allocator) Linker {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Linker) void {
        for (self.funcs.items) |*f| {
            f.deinit(self.allocator);
        }
        self.funcs.deinit(self.allocator);
        self.imports.deinit(self.allocator);
        self.globals.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.type_storage.deinit(self.allocator);
        self.data_segments.deinit(self.allocator);
        self.table_funcs.deinit(self.allocator);
        self.gc_struct_types.deinit(self.allocator);
        self.gc_field_storage.deinit(self.allocator);
        self.gc_struct_name_map.deinit(self.allocator);
    }

    /// Get the number of GC struct types (used to offset function type indices)
    pub fn gcTypeCount(self: *const Linker) u32 {
        return @intCast(self.gc_struct_types.items.len);
    }

    /// Convert a raw function type index to an adjusted index that accounts for
    /// GC struct types occupying the first N type indices.
    pub fn funcTypeIndex(self: *const Linker, raw_idx: u32) u32 {
        return raw_idx + self.gcTypeCount();
    }

    /// Add or find a GC struct type, return its type index.
    /// The type index is in the range 0..N-1 (before function types).
    pub fn addGcStructType(self: *Linker, type_name: []const u8, fields: []const GcFieldType) !u32 {
        // Check if already registered
        if (self.gc_struct_name_map.get(type_name)) |idx| {
            return idx;
        }

        const type_idx: u32 = @intCast(self.gc_struct_types.items.len);
        const field_offset: u32 = @intCast(self.gc_field_storage.items.len);

        try self.gc_field_storage.appendSlice(self.allocator, fields);
        try self.gc_struct_types.append(self.allocator, .{
            .name = type_name,
            .field_count = @intCast(fields.len),
            .field_offset = field_offset,
        });
        try self.gc_struct_name_map.put(self.allocator, type_name, type_idx);

        return type_idx;
    }

    /// Add or find a function type, return its index.
    /// Accepts simple ValType params/results (backward-compatible with runtime code).
    /// NOTE: The returned index is a raw index (0-based among function types).
    /// When emitting to the Wasm binary, use funcTypeIndex() to get the
    /// adjusted index that accounts for GC struct types.
    pub fn addType(self: *Linker, params: []const c.ValType, results: []const c.ValType) !u32 {
        // Convert ValType to WasmType and delegate to addTypeWasm
        var param_buf: [32]c.WasmType = undefined;
        var result_buf: [32]c.WasmType = undefined;
        for (params, 0..) |p, i| param_buf[i] = c.WasmType.fromVal(p);
        for (results, 0..) |r, i| result_buf[i] = c.WasmType.fromVal(r);
        return self.addTypeWasm(param_buf[0..params.len], result_buf[0..results.len]);
    }

    /// Add or find a function type with GC ref type support.
    /// Accepts WasmType params/results which can represent (ref null $T).
    /// Reference: Kotlin/Wasm type section emission
    pub fn addTypeWasm(self: *Linker, params: []const c.WasmType, results: []const c.WasmType) !u32 {
        // Store first, then compare with stored data to avoid dangling slices.
        // type_storage may reallocate, so we use offsets, not slices.
        const params_start: u32 = @intCast(self.type_storage.items.len);
        try self.type_storage.appendSlice(self.allocator, params);
        const results_start: u32 = @intCast(self.type_storage.items.len);
        try self.type_storage.appendSlice(self.allocator, results);

        const new_type = FuncType{
            .params_offset = params_start,
            .params_len = @intCast(params.len),
            .results_offset = results_start,
            .results_len = @intCast(results.len),
        };

        // Check if type already exists (compare via storage)
        for (self.types.items, 0..) |existing, i| {
            if (new_type.eqlWith(existing, self.type_storage.items)) {
                // Duplicate found — roll back type_storage
                self.type_storage.shrinkRetainingCapacity(params_start);
                return @intCast(i);
            }
        }

        try self.types.append(self.allocator, new_type);
        return @intCast(self.types.items.len - 1);
    }

    /// Add a host import (Go: collecting hostImports in asm.go)
    /// Returns the import's function index (imports get indices 0..N-1)
    pub fn addImport(self: *Linker, import: WasmImport) !u32 {
        const idx: u32 = @intCast(self.imports.items.len);
        try self.imports.append(self.allocator, import);
        return idx;
    }

    /// Get the number of imports (for offsetting native function indices)
    pub fn numImports(self: *const Linker) u32 {
        return @intCast(self.imports.items.len);
    }

    /// Get the number of functions added
    pub fn funcCount(self: *const Linker) u32 {
        return @intCast(self.funcs.items.len);
    }

    /// Add a global variable
    /// Returns the global's index
    pub fn addGlobal(self: *Linker, global: WasmGlobal) !u32 {
        const idx: u32 = @intCast(self.globals.items.len);
        try self.globals.append(self.allocator, global);
        return idx;
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

    /// Add a function to the indirect call table
    /// Returns the table index for call_indirect
    pub fn addTableFunc(self: *Linker, func_idx: u32) !u32 {
        const table_idx: u32 = @intCast(self.table_funcs.items.len);
        try self.table_funcs.append(self.allocator, func_idx);
        // Update table size
        if (table_idx + 1 > self.table_size) {
            self.table_size = table_idx + 1;
        }
        return table_idx;
    }

    /// Set minimum table size (for reserved slots like null entry)
    pub fn setTableSize(self: *Linker, size: u32) void {
        if (size > self.table_size) {
            self.table_size = size;
        }
    }

    /// Add a data segment for static data (string literals, etc.)
    /// Returns the offset in memory where the data will be placed
    pub fn addData(self: *Linker, data: []const u8) !i32 {
        // Data starts after stack area (at low memory)
        // Stack grows down from 1MB, data grows up from 0
        var offset: i32 = 0;
        for (self.data_segments.items) |seg| {
            const seg_end = seg.offset + @as(i32, @intCast(seg.data.len));
            if (seg_end > offset) offset = seg_end;
        }
        // Align to 8 bytes
        const arc = @import("../arc.zig");
        offset = (offset + arc.ALIGN_MINUS_ONE) & arc.ALIGN_MASK;

        try self.data_segments.append(self.allocator, .{
            .offset = offset,
            .data = data,
        });
        return offset;
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
        // GC struct types get indices 0..N-1, function types get N..N+M-1
        // ====================================================================
        {
            const gc_count = self.gc_struct_types.items.len;
            const func_count = self.types.items.len;
            const total_count = gc_count + func_count;

            if (total_count > 0) {
                var type_buf = std.ArrayListUnmanaged(u8){};
                defer type_buf.deinit(self.allocator);

                try assemble.writeULEB128(self.allocator, &type_buf, total_count);

                // Emit GC struct types first (indices 0..N-1)
                for (self.gc_struct_types.items) |st| {
                    try type_buf.append(self.allocator, c.GC_STRUCT_TYPE); // 0x5F
                    try assemble.writeULEB128(self.allocator, &type_buf, st.field_count);
                    const fields = self.gc_field_storage.items[st.field_offset .. st.field_offset + st.field_count];
                    for (fields) |f| {
                        if (f.gc_ref) |gc_idx| {
                            // Field is a GC ref type: (ref null $typeidx)
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL); // 0x63
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            try type_buf.append(self.allocator, @intFromEnum(f.val_type));
                        }
                        try type_buf.append(self.allocator, if (f.mutable) c.GC_FIELD_MUT else c.GC_FIELD_IMMUT);
                    }
                }

                // Emit function types (indices N..N+M-1)
                // Reference: Kotlin/Wasm encodes ref types as 0x64 + LEB128(typeidx)
                for (self.types.items) |t| {
                    const t_params = t.getParams(self.type_storage.items);
                    const t_results = t.getResults(self.type_storage.items);
                    try type_buf.append(self.allocator, c.FUNC_TYPE_TAG); // 0x60
                    try assemble.writeULEB128(self.allocator, &type_buf, t_params.len);
                    for (t_params) |p| {
                        if (p.gc_ref) |gc_idx| {
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL); // 0x63
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            try type_buf.append(self.allocator, @intFromEnum(p.val));
                        }
                    }
                    try assemble.writeULEB128(self.allocator, &type_buf, t_results.len);
                    for (t_results) |r| {
                        if (r.gc_ref) |gc_idx| {
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL); // 0x63
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            try type_buf.append(self.allocator, @intFromEnum(r.val));
                        }
                    }
                }
                try writeSection(writer, self.allocator, .type, type_buf.items);
            }
        }

        // ====================================================================
        // Import Section (Go: asm.go writeImportSec)
        // ====================================================================
        if (self.imports.items.len > 0) {
            var import_buf = std.ArrayListUnmanaged(u8){};
            defer import_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &import_buf, self.imports.items.len);
            for (self.imports.items) |imp| {
                // Module name
                try assemble.writeULEB128(self.allocator, &import_buf, imp.module.len);
                try import_buf.appendSlice(self.allocator, imp.module);
                // Function name
                try assemble.writeULEB128(self.allocator, &import_buf, imp.name.len);
                try import_buf.appendSlice(self.allocator, imp.name);
                // Import kind: function
                try import_buf.append(self.allocator, c.IMPORT_KIND_FUNC);
                // Function type index
                try assemble.writeULEB128(self.allocator, &import_buf, imp.type_idx);
            }
            try writeSection(writer, self.allocator, .import, import_buf.items);
        }

        // ====================================================================
        // Function Section (Go: asm.go writeFunctionSec)
        // ====================================================================
        if (self.funcs.items.len > 0) {
            var func_buf = std.ArrayListUnmanaged(u8){};
            defer func_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &func_buf, self.funcs.items.len);
            const gc_offset = self.gcTypeCount();
            for (self.funcs.items) |f| {
                try assemble.writeULEB128(self.allocator, &func_buf, f.type_idx + gc_offset);
            }
            try writeSection(writer, self.allocator, .function, func_buf.items);
        }

        // ====================================================================
        // Table Section (for call_indirect - destructors, vtables)
        // ====================================================================
        if (self.table_size > 0) {
            var table_buf = std.ArrayListUnmanaged(u8){};
            defer table_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &table_buf, 1); // 1 table
            try table_buf.append(self.allocator, @intFromEnum(c.ValType.funcref)); // funcref type
            try table_buf.append(self.allocator, c.LIMITS_NO_MAX);
            try assemble.writeULEB128(self.allocator, &table_buf, self.table_size);
            try writeSection(writer, self.allocator, .table, table_buf.items);
        }

        // ====================================================================
        // Memory Section (Go: asm.go writeMemorySec)
        // ====================================================================
        {
            var mem_buf = std.ArrayListUnmanaged(u8){};
            defer mem_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &mem_buf, 1); // 1 memory
            if (self.memory_max_pages) |max| {
                try mem_buf.append(self.allocator, c.LIMITS_WITH_MAX);
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
                try assemble.writeULEB128(self.allocator, &mem_buf, max);
            } else {
                try mem_buf.append(self.allocator, c.LIMITS_NO_MAX);
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
            }
            try writeSection(writer, self.allocator, .memory, mem_buf.items);
        }

        // ====================================================================
        // Global Section (Go: asm.go writeGlobalSec)
        // ====================================================================
        // Go defines 8 globals: SP(i32), CTXT(i64), g(i64), RET0-3(i64), PAUSE(i32)
        // Cot uses: SP(i32), heap_ptr(i32), plus any dynamically added globals
        {
            var global_buf = std.ArrayListUnmanaged(u8){};
            defer global_buf.deinit(self.allocator);

            // Count: SP (1) + dynamic globals
            const total_globals = 1 + self.globals.items.len;
            try assemble.writeULEB128(self.allocator, &global_buf, total_globals);

            // Global 0: SP (stack pointer) - always present
            try global_buf.append(self.allocator, @intFromEnum(c.ValType.i32));
            try global_buf.append(self.allocator, c.GLOBAL_MUTABLE);
            try global_buf.append(self.allocator, c.As.i32_const.opcode().?);
            try assemble.writeSLEB128(self.allocator, &global_buf, c.STACK_SIZE);
            try global_buf.append(self.allocator, c.As.end.opcode().?);

            // Dynamic globals (heap_ptr, etc.)
            for (self.globals.items) |g| {
                try global_buf.append(self.allocator, @intFromEnum(g.val_type));
                try global_buf.append(self.allocator, if (g.mutable) c.GLOBAL_MUTABLE else c.GLOBAL_IMMUTABLE);
                // Init expression
                if (g.val_type == .i32) {
                    try global_buf.append(self.allocator, c.As.i32_const.opcode().?);
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init_i32);
                } else {
                    try global_buf.append(self.allocator, c.As.i64_const.opcode().?);
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init_i64);
                }
                try global_buf.append(self.allocator, c.As.end.opcode().?);
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

            // Export functions (indices offset by import count)
            const import_count = self.imports.items.len;
            for (self.funcs.items, 0..) |f, i| {
                if (f.exported) {
                    try assemble.writeULEB128(self.allocator, &export_buf, f.name.len);
                    try export_buf.appendSlice(self.allocator, f.name);
                    try export_buf.append(self.allocator, @intFromEnum(c.ExportKind.func));
                    // Function index = import_count + native_index (Go: asm.go line 356)
                    try assemble.writeULEB128(self.allocator, &export_buf, import_count + i);
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
        // Element Section (initialize table with function references)
        // ====================================================================
        if (self.table_funcs.items.len > 0) {
            var elem_buf = std.ArrayListUnmanaged(u8){};
            defer elem_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &elem_buf, 1); // 1 element segment
            // Table index 0
            try assemble.writeULEB128(self.allocator, &elem_buf, 0);
            // Offset expression: i32.const 0, end
            try elem_buf.append(self.allocator, c.As.i32_const.opcode().?);
            try assemble.writeSLEB128(self.allocator, &elem_buf, 0);
            try elem_buf.append(self.allocator, c.As.end.opcode().?);
            // Function indices
            try assemble.writeULEB128(self.allocator, &elem_buf, self.table_funcs.items.len);
            const import_count = self.imports.items.len;
            for (self.table_funcs.items) |func_idx| {
                // Function indices must account for imports
                try assemble.writeULEB128(self.allocator, &elem_buf, import_count + func_idx);
            }
            try writeSection(writer, self.allocator, .element, elem_buf.items);
        }

        // ====================================================================
        // Data Count Section (Wasm 2.0 - required for bulk memory ops)
        // Must appear between element section and code section.
        // ====================================================================
        if (self.data_segments.items.len > 0) {
            var dc_buf = std.ArrayListUnmanaged(u8){};
            defer dc_buf.deinit(self.allocator);
            try assemble.writeULEB128(self.allocator, &dc_buf, self.data_segments.items.len);
            try writeSection(writer, self.allocator, .data_count, dc_buf.items);
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

        // ====================================================================
        // Data Section (Go: asm.go writeDataSec)
        // ====================================================================
        if (self.data_segments.items.len > 0) {
            var data_buf = std.ArrayListUnmanaged(u8){};
            defer data_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &data_buf, self.data_segments.items.len);
            for (self.data_segments.items) |seg| {
                // Memory index (always 0)
                try assemble.writeULEB128(self.allocator, &data_buf, 0);
                // i32.const offset
                try data_buf.append(self.allocator, c.As.i32_const.opcode().?);
                try assemble.writeSLEB128(self.allocator, &data_buf, seg.offset);
                // end
                try data_buf.append(self.allocator, c.As.end.opcode().?);
                // data
                try assemble.writeULEB128(self.allocator, &data_buf, seg.data.len);
                try data_buf.appendSlice(self.allocator, seg.data);
            }
            try writeSection(writer, self.allocator, .data, data_buf.items);
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

test "linker gc struct types" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    // Add a GC struct type: Point { x: i64, y: i64 }
    const fields = [_]GcFieldType{
        .{ .val_type = .i64, .mutable = true },
        .{ .val_type = .i64, .mutable = true },
    };
    const type_idx = try linker.addGcStructType("Point", &fields);
    try testing.expectEqual(@as(u32, 0), type_idx);

    // Same name returns same index
    const type_idx2 = try linker.addGcStructType("Point", &fields);
    try testing.expectEqual(@as(u32, 0), type_idx2);

    // Different struct gets new index
    const fields2 = [_]GcFieldType{
        .{ .val_type = .i64, .mutable = true },
    };
    const type_idx3 = try linker.addGcStructType("Foo", &fields2);
    try testing.expectEqual(@as(u32, 1), type_idx3);

    // Function type indices should be offset
    const func_type = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), func_type); // Raw index
    try testing.expectEqual(@as(u32, 2), linker.funcTypeIndex(func_type)); // Adjusted: 0 + 2 gc types

    try testing.expectEqual(@as(u32, 2), linker.gcTypeCount());
}

test "linker with imports" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    // Add import type: (i64) -> void
    const import_type = try linker.addType(&[_]c.ValType{.i64}, &[_]c.ValType{});

    // Add import: env.console_log
    const import_idx = try linker.addImport(.{
        .module = "env",
        .name = "console_log",
        .type_idx = import_type,
    });
    try testing.expectEqual(@as(u32, 0), import_idx);

    // Add native function type: () -> i64
    const func_type = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});

    // Add native function: main
    const code = &[_]u8{
        0x00, // 0 locals
        0x42, 0x2A, // i64.const 42
        0x0B, // end
    };
    const func_idx = try linker.addFunc(.{
        .name = "main",
        .type_idx = func_type,
        .code = try allocator.dupe(u8, code),
        .exported = true,
    });
    try testing.expectEqual(@as(u32, 0), func_idx); // Native index is 0

    // numImports should be 1
    try testing.expectEqual(@as(u32, 1), linker.numImports());

    // Emit
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try linker.emit(output.writer(allocator));

    // Verify header
    try testing.expectEqualSlices(u8, &c.WASM_MAGIC, output.items[0..4]);

    // Find import section (section ID 2)
    var found_import_section = false;
    var i: usize = 8; // Skip header
    while (i < output.items.len) {
        const section_id = output.items[i];
        if (section_id == @intFromEnum(c.Section.import)) {
            found_import_section = true;
            break;
        }
        // Skip to next section
        i += 1;
        if (i >= output.items.len) break;
        const size = output.items[i]; // Simplified: assumes size < 128
        i += 1 + size;
    }
    try testing.expect(found_import_section);
}
