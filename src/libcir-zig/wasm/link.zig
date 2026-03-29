//! Link — write complete Wasm module binary with all sections.

const std = @import("std");
const c = @import("constants.zig");
const prog = @import("prog.zig");
const assemble = @import("assemble.zig");
const Symbol = prog.Symbol;

const debug = @import("foundation").debug;
fn debugLog(comptime fmt: []const u8, args: anytype) void {
    debug.log(.codegen, fmt, args);
}

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

pub const WasmFunc = struct {
    name: []const u8,
    type_idx: u32,
    code: []const u8,
    exported: bool = false,

    pub fn deinit(self: *WasmFunc, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
    }
};

pub const GcFieldType = struct {
    val_type: c.ValType,
    mutable: bool,
    gc_ref: ?u32 = null,
};

pub const GcStructType = struct {
    name: []const u8,
    field_count: u32,
    field_offset: u32,
    super_type: ?u32 = null,
};

pub const GcArrayType = struct {
    name: []const u8,
    elem_type: GcFieldType,
};

pub const WasmImport = struct {
    module: []const u8,
    name: []const u8,
    type_idx: u32,
};

pub const WasmGlobal = struct {
    val_type: c.ValType,
    mutable: bool,
    init_i32: i32 = 0,
    init_i64: i64 = 0,
};

pub const TableEntry = struct {
    func_idx: u32,
};

pub const ElementSegment = struct {
    offset: u32,
    funcs: []const u32,
};

pub const Linker = struct {
    allocator: std.mem.Allocator,

    types: std.ArrayListUnmanaged(FuncType) = .{},
    type_storage: std.ArrayListUnmanaged(c.WasmType) = .{},

    imports: std.ArrayListUnmanaged(WasmImport) = .{},

    globals: std.ArrayListUnmanaged(WasmGlobal) = .{},

    funcs: std.ArrayListUnmanaged(WasmFunc) = .{},

    memory_min_pages: u32 = 1,
    memory_max_pages: ?u32 = null,

    data_segments: std.ArrayListUnmanaged(DataSegment) = .{},

    table_size: u32 = 0,
    table_funcs: std.ArrayListUnmanaged(u32) = .{},

    gc_struct_types: std.ArrayListUnmanaged(GcStructType) = .{},
    gc_field_storage: std.ArrayListUnmanaged(GcFieldType) = .{},
    gc_struct_name_map: std.StringHashMapUnmanaged(u32) = .{},

    gc_array_types: std.ArrayListUnmanaged(GcArrayType) = .{},
    gc_array_name_map: std.StringHashMapUnmanaged(u32) = .{},

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
        self.gc_array_types.deinit(self.allocator);
        self.gc_array_name_map.deinit(self.allocator);
    }

    pub fn gcTypeCount(self: *const Linker) u32 {
        return @intCast(self.gc_struct_types.items.len + self.gc_array_types.items.len);
    }

    pub fn funcTypeIndex(self: *const Linker, raw_idx: u32) u32 {
        return raw_idx + self.gcTypeCount();
    }

    pub fn addGcStructType(self: *Linker, type_name: []const u8, fields: []const GcFieldType) !u32 {
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

    pub fn addGcArrayType(self: *Linker, type_name: []const u8, elem_type: GcFieldType) !u32 {
        if (self.gc_array_name_map.get(type_name)) |idx| {
            return idx;
        }

        const type_idx: u32 = @intCast(self.gc_struct_types.items.len + self.gc_array_types.items.len);

        try self.gc_array_types.append(self.allocator, .{
            .name = type_name,
            .elem_type = elem_type,
        });
        try self.gc_array_name_map.put(self.allocator, type_name, type_idx);

        return type_idx;
    }

    pub fn addType(self: *Linker, params: []const c.ValType, results: []const c.ValType) !u32 {
        var param_buf: [32]c.WasmType = undefined;
        var result_buf: [32]c.WasmType = undefined;
        for (params, 0..) |p, i| param_buf[i] = c.WasmType.fromVal(p);
        for (results, 0..) |r, i| result_buf[i] = c.WasmType.fromVal(r);
        return self.addTypeWasm(param_buf[0..params.len], result_buf[0..results.len]);
    }

    pub fn addTypeWasm(self: *Linker, params: []const c.WasmType, results: []const c.WasmType) !u32 {
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

        for (self.types.items, 0..) |existing, i| {
            if (new_type.eqlWith(existing, self.type_storage.items)) {
                self.type_storage.shrinkRetainingCapacity(params_start);
                return @intCast(i);
            }
        }

        try self.types.append(self.allocator, new_type);
        const new_idx: u32 = @intCast(self.types.items.len - 1);
        for (params, 0..) |p, pi| {
            const vb = @intFromEnum(p.val);
            if (vb != 0x7F and vb != 0x7E and vb != 0x7D and vb != 0x7C and vb != 0x70 and vb != 0x6F and p.gc_ref == null) {
                debugLog("  !!! addTypeWasm: NEW type[{d}] param[{d}] invalid val=0x{X:0>2}", .{ new_idx, pi, vb });
            }
        }
        for (results, 0..) |r, ri| {
            const vb = @intFromEnum(r.val);
            if (vb != 0x7F and vb != 0x7E and vb != 0x7D and vb != 0x7C and vb != 0x70 and vb != 0x6F and r.gc_ref == null) {
                debugLog("  !!! addTypeWasm: NEW type[{d}] result[{d}] invalid val=0x{X:0>2}", .{ new_idx, ri, vb });
            }
        }
        return new_idx;
    }

    pub fn addImport(self: *Linker, import: WasmImport) !u32 {
        const idx: u32 = @intCast(self.imports.items.len);
        try self.imports.append(self.allocator, import);
        return idx;
    }

    pub fn numImports(self: *const Linker) u32 {
        return @intCast(self.imports.items.len);
    }

    pub fn funcCount(self: *const Linker) u32 {
        return @intCast(self.funcs.items.len);
    }

    pub fn addGlobal(self: *Linker, global: WasmGlobal) !u32 {
        const idx: u32 = @intCast(self.globals.items.len);
        try self.globals.append(self.allocator, global);
        return idx;
    }

    pub fn addFunc(self: *Linker, func: WasmFunc) !u32 {
        const idx: u32 = @intCast(self.funcs.items.len);
        try self.funcs.append(self.allocator, func);
        return idx;
    }

    pub fn setMemory(self: *Linker, min_pages: u32, max_pages: ?u32) void {
        self.memory_min_pages = min_pages;
        self.memory_max_pages = max_pages;
    }

    pub fn addTableFunc(self: *Linker, func_idx: u32) !u32 {
        const table_idx: u32 = @intCast(self.table_funcs.items.len);
        try self.table_funcs.append(self.allocator, func_idx);
        if (table_idx + 1 > self.table_size) {
            self.table_size = table_idx + 1;
        }
        return table_idx;
    }

    pub fn setTableSize(self: *Linker, size: u32) void {
        if (size > self.table_size) {
            self.table_size = size;
        }
    }

    pub fn addData(self: *Linker, data: []const u8) !i32 {
        var offset: i32 = 0;
        for (self.data_segments.items) |seg| {
            const seg_end = seg.offset + @as(i32, @intCast(seg.data.len));
            if (seg_end > offset) offset = seg_end;
        }
        offset = (offset + 7) & -@as(i32, 8);

        try self.data_segments.append(self.allocator, .{
            .offset = offset,
            .data = data,
        });
        return offset;
    }

    pub fn emit(self: *Linker, output: *std.ArrayListUnmanaged(u8)) !void {
        debugLog("link: emitting module ({d} types, {d} imports, {d} funcs, {d} globals, {d} data segments)", .{
            self.types.items.len,
            self.imports.items.len,
            self.funcs.items.len,
            self.globals.items.len,
            self.data_segments.items.len,
        });

        try output.appendSlice(self.allocator, &c.WASM_MAGIC);
        try output.appendSlice(self.allocator, &c.WASM_VERSION);

        {
            const gc_struct_count = self.gc_struct_types.items.len;
            const gc_array_count = self.gc_array_types.items.len;
            const func_count = self.types.items.len;
            const total_count = gc_struct_count + gc_array_count + func_count;

            if (total_count > 0) {
                var type_buf = std.ArrayListUnmanaged(u8){};
                defer type_buf.deinit(self.allocator);

                try assemble.writeULEB128(self.allocator, &type_buf, total_count);

                for (self.gc_struct_types.items) |st| {
                    if (st.super_type) |parent_idx| {
                        try type_buf.append(self.allocator, c.GC_SUB_TYPE);
                        try assemble.writeULEB128(self.allocator, &type_buf, @as(u64, 1));
                        try assemble.writeULEB128(self.allocator, &type_buf, parent_idx);
                    }
                    try type_buf.append(self.allocator, c.GC_STRUCT_TYPE);
                    try assemble.writeULEB128(self.allocator, &type_buf, st.field_count);
                    const fields = self.gc_field_storage.items[st.field_offset .. st.field_offset + st.field_count];
                    for (fields) |f| {
                        if (f.gc_ref) |gc_idx| {
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL);
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            try type_buf.append(self.allocator, @intFromEnum(f.val_type));
                        }
                        try type_buf.append(self.allocator, if (f.mutable) c.GC_FIELD_MUT else c.GC_FIELD_IMMUT);
                    }
                }

                for (self.gc_array_types.items) |at| {
                    try type_buf.append(self.allocator, c.GC_ARRAY_TYPE);
                    const elem = at.elem_type;
                    if (elem.gc_ref) |gc_idx| {
                        try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL);
                        try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                    } else {
                        try type_buf.append(self.allocator, @intFromEnum(elem.val_type));
                    }
                    try type_buf.append(self.allocator, if (elem.mutable) c.GC_FIELD_MUT else c.GC_FIELD_IMMUT);
                }

                for (self.types.items, 0..) |t, type_idx| {
                    const t_params = t.getParams(self.type_storage.items);
                    const t_results = t.getResults(self.type_storage.items);
                    try type_buf.append(self.allocator, c.FUNC_TYPE_TAG);
                    try assemble.writeULEB128(self.allocator, &type_buf, t_params.len);
                    for (t_params, 0..) |p, pi| {
                        const val_byte = @intFromEnum(p.val);
                        if (p.gc_ref) |gc_idx| {
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL);
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            if (val_byte != 0x7F and val_byte != 0x7E and val_byte != 0x7D and val_byte != 0x7C and val_byte != 0x70 and val_byte != 0x6F) {
                                debugLog("  !!! type[{d}] param[{d}] has invalid val_byte=0x{X:0>2}", .{ type_idx, pi, val_byte });
                            }
                            try type_buf.append(self.allocator, val_byte);
                        }
                    }
                    try assemble.writeULEB128(self.allocator, &type_buf, t_results.len);
                    for (t_results, 0..) |r, ri| {
                        const val_byte = @intFromEnum(r.val);
                        if (r.gc_ref) |gc_idx| {
                            try type_buf.append(self.allocator, c.GC_REF_TYPE_NULL);
                            try assemble.writeULEB128(self.allocator, &type_buf, gc_idx);
                        } else {
                            if (val_byte != 0x7F and val_byte != 0x7E and val_byte != 0x7D and val_byte != 0x7C and val_byte != 0x70 and val_byte != 0x6F) {
                                debugLog("  !!! type[{d}] result[{d}] has invalid val_byte=0x{X:0>2}", .{ type_idx, ri, val_byte });
                            }
                            try type_buf.append(self.allocator, val_byte);
                        }
                    }
                }
                try writeSection(output, self.allocator, .type, type_buf.items);
            }
        }

        if (self.imports.items.len > 0) {
            var import_buf = std.ArrayListUnmanaged(u8){};
            defer import_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &import_buf, self.imports.items.len);
            for (self.imports.items) |imp| {
                try assemble.writeULEB128(self.allocator, &import_buf, imp.module.len);
                try import_buf.appendSlice(self.allocator, imp.module);
                try assemble.writeULEB128(self.allocator, &import_buf, imp.name.len);
                try import_buf.appendSlice(self.allocator, imp.name);
                try import_buf.append(self.allocator, c.IMPORT_KIND_FUNC);
                try assemble.writeULEB128(self.allocator, &import_buf, imp.type_idx);
            }
            try writeSection(output, self.allocator, .import, import_buf.items);
        }

        if (self.funcs.items.len > 0) {
            var func_buf = std.ArrayListUnmanaged(u8){};
            defer func_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &func_buf, self.funcs.items.len);
            const gc_offset = self.gcTypeCount();
            for (self.funcs.items) |f| {
                try assemble.writeULEB128(self.allocator, &func_buf, f.type_idx + gc_offset);
            }
            try writeSection(output, self.allocator, .function, func_buf.items);
        }

        if (self.table_size > 0) {
            var table_buf = std.ArrayListUnmanaged(u8){};
            defer table_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &table_buf, 1);
            try table_buf.append(self.allocator, @intFromEnum(c.ValType.funcref));
            try table_buf.append(self.allocator, c.LIMITS_NO_MAX);
            try assemble.writeULEB128(self.allocator, &table_buf, self.table_size);
            try writeSection(output, self.allocator, .table, table_buf.items);
        }

        {
            var mem_buf = std.ArrayListUnmanaged(u8){};
            defer mem_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &mem_buf, 1);
            if (self.memory_max_pages) |max| {
                try mem_buf.append(self.allocator, c.LIMITS_WITH_MAX);
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
                try assemble.writeULEB128(self.allocator, &mem_buf, max);
            } else {
                try mem_buf.append(self.allocator, c.LIMITS_NO_MAX);
                try assemble.writeULEB128(self.allocator, &mem_buf, self.memory_min_pages);
            }
            try writeSection(output, self.allocator, .memory, mem_buf.items);
        }

        {
            var global_buf = std.ArrayListUnmanaged(u8){};
            defer global_buf.deinit(self.allocator);

            const total_globals = 1 + self.globals.items.len;
            try assemble.writeULEB128(self.allocator, &global_buf, total_globals);

            try global_buf.append(self.allocator, @intFromEnum(c.ValType.i32));
            try global_buf.append(self.allocator, c.GLOBAL_MUTABLE);
            try global_buf.append(self.allocator, c.As.i32_const.opcode().?);
            try assemble.writeSLEB128(self.allocator, &global_buf, c.STACK_SIZE);
            try global_buf.append(self.allocator, c.As.end.opcode().?);

            for (self.globals.items) |g| {
                try global_buf.append(self.allocator, @intFromEnum(g.val_type));
                try global_buf.append(self.allocator, if (g.mutable) c.GLOBAL_MUTABLE else c.GLOBAL_IMMUTABLE);
                if (g.val_type == .i32) {
                    try global_buf.append(self.allocator, c.As.i32_const.opcode().?);
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init_i32);
                } else {
                    try global_buf.append(self.allocator, c.As.i64_const.opcode().?);
                    try assemble.writeSLEB128(self.allocator, &global_buf, g.init_i64);
                }
                try global_buf.append(self.allocator, c.As.end.opcode().?);
            }
            try writeSection(output, self.allocator, .global, global_buf.items);
        }

        {
            var export_buf = std.ArrayListUnmanaged(u8){};
            defer export_buf.deinit(self.allocator);

            var export_count: usize = 1;
            for (self.funcs.items) |f| {
                if (f.exported) export_count += 1;
            }

            try assemble.writeULEB128(self.allocator, &export_buf, export_count);

            const import_count = self.imports.items.len;
            for (self.funcs.items, 0..) |f, i| {
                if (f.exported) {
                    try assemble.writeULEB128(self.allocator, &export_buf, f.name.len);
                    try export_buf.appendSlice(self.allocator, f.name);
                    try export_buf.append(self.allocator, @intFromEnum(c.ExportKind.func));
                    try assemble.writeULEB128(self.allocator, &export_buf, import_count + i);
                }
            }

            const mem_name = "memory";
            try assemble.writeULEB128(self.allocator, &export_buf, mem_name.len);
            try export_buf.appendSlice(self.allocator, mem_name);
            try export_buf.append(self.allocator, @intFromEnum(c.ExportKind.memory));
            try assemble.writeULEB128(self.allocator, &export_buf, 0);

            try writeSection(output, self.allocator, .@"export", export_buf.items);
        }

        if (self.table_funcs.items.len > 0) {
            var elem_buf = std.ArrayListUnmanaged(u8){};
            defer elem_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &elem_buf, 1);
            try assemble.writeULEB128(self.allocator, &elem_buf, 0);
            try elem_buf.append(self.allocator, c.As.i32_const.opcode().?);
            try assemble.writeSLEB128(self.allocator, &elem_buf, 0);
            try elem_buf.append(self.allocator, c.As.end.opcode().?);
            try assemble.writeULEB128(self.allocator, &elem_buf, self.table_funcs.items.len);
            const import_count = self.imports.items.len;
            for (self.table_funcs.items) |func_idx| {
                try assemble.writeULEB128(self.allocator, &elem_buf, import_count + func_idx);
            }
            try writeSection(output, self.allocator, .element, elem_buf.items);
        }

        if (self.data_segments.items.len > 0) {
            var dc_buf = std.ArrayListUnmanaged(u8){};
            defer dc_buf.deinit(self.allocator);
            try assemble.writeULEB128(self.allocator, &dc_buf, self.data_segments.items.len);
            try writeSection(output, self.allocator, .data_count, dc_buf.items);
        }

        if (self.funcs.items.len > 0) {
            var code_buf = std.ArrayListUnmanaged(u8){};
            defer code_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &code_buf, self.funcs.items.len);
            for (self.funcs.items) |f| {
                try assemble.writeULEB128(self.allocator, &code_buf, f.code.len);
                try code_buf.appendSlice(self.allocator, f.code);
            }
            try writeSection(output, self.allocator, .code, code_buf.items);
        }

        if (self.data_segments.items.len > 0) {
            var data_buf = std.ArrayListUnmanaged(u8){};
            defer data_buf.deinit(self.allocator);

            try assemble.writeULEB128(self.allocator, &data_buf, self.data_segments.items.len);
            for (self.data_segments.items) |seg| {
                try assemble.writeULEB128(self.allocator, &data_buf, 0);
                try data_buf.append(self.allocator, c.As.i32_const.opcode().?);
                try assemble.writeSLEB128(self.allocator, &data_buf, seg.offset);
                try data_buf.append(self.allocator, c.As.end.opcode().?);
                try assemble.writeULEB128(self.allocator, &data_buf, seg.data.len);
                try data_buf.appendSlice(self.allocator, seg.data);
            }
            try writeSection(output, self.allocator, .data, data_buf.items);
        }

        debugLog("  link complete", .{});
    }
};

pub const DataSegment = struct {
    offset: i32,
    data: []const u8,
};

fn writeSection(output: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, section: c.Section, content: []const u8) !void {
    debugLog("  section {s}: {d} bytes", .{ @tagName(section), content.len });
    try output.append(allocator, @intFromEnum(section));

    var size_buf = std.ArrayListUnmanaged(u8){};
    defer size_buf.deinit(allocator);
    try assemble.writeULEB128(allocator, &size_buf, content.len);
    try output.appendSlice(allocator, size_buf.items);

    try output.appendSlice(allocator, content);
}

const testing = std.testing;

test "linker basic" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    const type_idx = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), type_idx);

    const type_idx2 = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), type_idx2);

    const type_idx3 = try linker.addType(&[_]c.ValType{ .i64, .i64 }, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 1), type_idx3);
}

test "linker emit" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    const type_idx = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});

    const code = &[_]u8{
        0x00,
        0x42, 0x2A,
        0x0B,
    };
    _ = try linker.addFunc(.{
        .name = "main",
        .type_idx = type_idx,
        .code = try allocator.dupe(u8, code),
        .exported = true,
    });

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try linker.emit(&output);

    try testing.expectEqualSlices(u8, &c.WASM_MAGIC, output.items[0..4]);
    try testing.expectEqual(@as(u8, 1), output.items[4]);
}

test "linker gc struct types" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    const fields = [_]GcFieldType{
        .{ .val_type = .i64, .mutable = true },
        .{ .val_type = .i64, .mutable = true },
    };
    const type_idx = try linker.addGcStructType("Point", &fields);
    try testing.expectEqual(@as(u32, 0), type_idx);

    const type_idx2 = try linker.addGcStructType("Point", &fields);
    try testing.expectEqual(@as(u32, 0), type_idx2);

    const fields2 = [_]GcFieldType{
        .{ .val_type = .i64, .mutable = true },
    };
    const type_idx3 = try linker.addGcStructType("Foo", &fields2);
    try testing.expectEqual(@as(u32, 1), type_idx3);

    const func_type = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});
    try testing.expectEqual(@as(u32, 0), func_type);
    try testing.expectEqual(@as(u32, 2), linker.funcTypeIndex(func_type));

    try testing.expectEqual(@as(u32, 2), linker.gcTypeCount());
}

test "linker with imports" {
    const allocator = testing.allocator;
    var linker = Linker.init(allocator);
    defer linker.deinit();

    const import_type = try linker.addType(&[_]c.ValType{.i64}, &[_]c.ValType{});

    const import_idx = try linker.addImport(.{
        .module = "env",
        .name = "console_log",
        .type_idx = import_type,
    });
    try testing.expectEqual(@as(u32, 0), import_idx);

    const func_type = try linker.addType(&[_]c.ValType{}, &[_]c.ValType{.i64});

    const code = &[_]u8{
        0x00,
        0x42, 0x2A,
        0x0B,
    };
    const func_idx = try linker.addFunc(.{
        .name = "main",
        .type_idx = func_type,
        .code = try allocator.dupe(u8, code),
        .exported = true,
    });
    try testing.expectEqual(@as(u32, 0), func_idx);

    try testing.expectEqual(@as(u32, 1), linker.numImports());

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try linker.emit(&output);

    try testing.expectEqualSlices(u8, &c.WASM_MAGIC, output.items[0..4]);

    var found_import_section = false;
    var i: usize = 8;
    while (i < output.items.len) {
        const section_id = output.items[i];
        if (section_id == @intFromEnum(c.Section.import)) {
            found_import_section = true;
            break;
        }
        i += 1;
        if (i >= output.items.len) break;
        const size = output.items[i];
        i += 1 + size;
    }
    try testing.expect(found_import_section);
}
