// DWARF debug information generation
// Reference: Go's cmd/internal/dwarf and cmd/internal/obj/dwarf.go

const std = @import("std");

// DWARF Constants
pub const DW_TAG_compile_unit: u8 = 0x11;
pub const DW_TAG_subprogram: u8 = 0x2e;
pub const DW_TAG_variable: u8 = 0x34;
pub const DW_TAG_formal_parameter: u8 = 0x05;
pub const DW_TAG_base_type: u8 = 0x24;

pub const DW_CHILDREN_no: u8 = 0x00;
pub const DW_CHILDREN_yes: u8 = 0x01;

pub const DW_AT_name: u8 = 0x03;
pub const DW_AT_stmt_list: u8 = 0x10;
pub const DW_AT_low_pc: u8 = 0x11;
pub const DW_AT_high_pc: u8 = 0x12;
pub const DW_AT_language: u8 = 0x13;
pub const DW_AT_comp_dir: u8 = 0x1b;
pub const DW_AT_producer: u8 = 0x25;
pub const DW_AT_decl_file: u8 = 0x3a;
pub const DW_AT_decl_line: u8 = 0x3b;
pub const DW_AT_external: u8 = 0x3f;
pub const DW_AT_frame_base: u8 = 0x40;
pub const DW_AT_type: u8 = 0x49;
pub const DW_AT_location: u8 = 0x02;
pub const DW_AT_byte_size: u8 = 0x0b;
pub const DW_AT_encoding: u8 = 0x3e;
pub const DW_AT_data_member_location: u8 = 0x38;

pub const DW_FORM_exprloc: u8 = 0x18;
pub const DW_FORM_ref4: u8 = 0x13;

pub const DW_LANG_C: u8 = 0x02;

pub const DW_FORM_addr: u8 = 0x01;
pub const DW_FORM_data1: u8 = 0x0b;
pub const DW_FORM_data4: u8 = 0x06;
pub const DW_FORM_data8: u8 = 0x07;
pub const DW_FORM_string: u8 = 0x08;
pub const DW_FORM_block1: u8 = 0x0a;
pub const DW_FORM_flag: u8 = 0x0c;
pub const DW_FORM_udata: u8 = 0x0f;
pub const DW_FORM_sec_offset: u8 = 0x17;

pub const DW_OP_call_frame_cfa: u8 = 0x9c;
pub const DW_OP_fbreg: u8 = 0x91;

pub const DW_ATE_signed: u8 = 0x05;
pub const DW_ATE_unsigned: u8 = 0x07;
pub const DW_ATE_float: u8 = 0x04;
pub const DW_ATE_boolean: u8 = 0x02;

pub const DW_TAG_pointer_type: u8 = 0x0f;
pub const DW_TAG_structure_type: u8 = 0x13;

pub const DW_LNS_copy: u8 = 0x01;
pub const DW_LNS_advance_pc: u8 = 0x02;
pub const DW_LNS_advance_line: u8 = 0x03;
pub const DW_LNS_set_file: u8 = 0x04;
pub const DW_LNS_set_column: u8 = 0x05;
pub const DW_LNS_negate_stmt: u8 = 0x06;
pub const DW_LNS_set_basic_block: u8 = 0x07;
pub const DW_LNS_const_add_pc: u8 = 0x08;
pub const DW_LNS_fixed_advance_pc: u8 = 0x09;
pub const DW_LNS_set_prologue_end: u8 = 0x0a;

pub const DW_LNE_end_sequence: u8 = 0x01;
pub const DW_LNE_set_address: u8 = 0x02;
pub const DW_LNE_define_file: u8 = 0x03;

// Call Frame Information opcodes (DWARF .debug_frame)
pub const DW_CFA_def_cfa: u8 = 0x0c;
pub const DW_CFA_def_cfa_offset: u8 = 0x0e;
pub const DW_CFA_def_cfa_register: u8 = 0x0d;
pub const DW_CFA_offset: u8 = 0x80; // high 2 bits = 10, low 6 bits = register
pub const DW_CFA_advance_loc: u8 = 0x40; // high 2 bits = 01, low 6 bits = delta/code_align
pub const DW_CFA_advance_loc1: u8 = 0x02;
pub const DW_CFA_advance_loc2: u8 = 0x03;
pub const DW_CFA_nop: u8 = 0x00;

// ARM64 DWARF register numbers
pub const DWARF_REG_FP: u8 = 29; // x29
pub const DWARF_REG_LR: u8 = 30; // x30
pub const DWARF_REG_SP: u8 = 31; // SP

// Line number program constants (from Go)
pub const LINE_BASE: i8 = -4;
pub const LINE_RANGE: u8 = 10;
pub const OPCODE_BASE: u8 = 11;
pub const PC_RANGE: u8 = (255 - OPCODE_BASE) / LINE_RANGE;

// LEB128 Encoding

pub fn appendUleb128(buf: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, v: u64) !void {
    var value = v;
    while (true) {
        var c: u8 = @truncate(value & 0x7f);
        value >>= 7;
        if (value != 0) c |= 0x80;
        try buf.append(allocator, c);
        if (c & 0x80 == 0) break;
    }
}

pub fn appendSleb128(buf: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, v: i64) !void {
    var value = v;
    while (true) {
        const c: u8 = @truncate(@as(u64, @bitCast(value)) & 0x7f);
        const s: u8 = @truncate(@as(u64, @bitCast(value)) & 0x40);
        value >>= 7;
        if ((value != -1 or s == 0) and (value != 0 or s != 0)) {
            try buf.append(allocator, c | 0x80);
        } else {
            try buf.append(allocator, c);
            break;
        }
    }
}

pub const LineEntry = struct {
    code_offset: u32,
    source_offset: u32,
};

pub const DebugReloc = struct {
    offset: u32,
    symbol_idx: u32,
};

pub const DebugLocalInfo = struct {
    name: []const u8,
    type_name: []const u8, // e.g., "i64", "string", "*Scope"
    frame_offset: i32, // DW_OP_fbreg offset
    size: u32,
    is_param: bool,
};

pub const DebugFuncInfo = struct {
    name: []const u8,
    code_offset: u32,
    code_size: u32,
    source_line: u32, // declaration line number
    locals: []const DebugLocalInfo = &.{},
    frame_size: u32 = 0, // total stack frame size (for .debug_frame FDE)
};

pub const DwarfBuilder = struct {
    allocator: std.mem.Allocator,
    debug_line: std.ArrayListUnmanaged(u8) = .{},
    debug_abbrev: std.ArrayListUnmanaged(u8) = .{},
    debug_info: std.ArrayListUnmanaged(u8) = .{},
    debug_frame: std.ArrayListUnmanaged(u8) = .{},
    debug_line_relocs: std.ArrayListUnmanaged(DebugReloc) = .{},
    debug_info_relocs: std.ArrayListUnmanaged(DebugReloc) = .{},
    debug_frame_relocs: std.ArrayListUnmanaged(DebugReloc) = .{},
    source_file: []const u8 = "",
    source_text: []const u8 = "",
    comp_dir: []const u8 = "",
    text_size: u64 = 0,
    func_infos: []const DebugFuncInfo = &.{},
    type_reg: ?*const @import("../../frontend/types.zig").TypeRegistry = null,

    pub fn init(allocator: std.mem.Allocator) DwarfBuilder {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *DwarfBuilder) void {
        self.debug_line.deinit(self.allocator);
        self.debug_abbrev.deinit(self.allocator);
        self.debug_info.deinit(self.allocator);
        self.debug_frame.deinit(self.allocator);
        self.debug_line_relocs.deinit(self.allocator);
        self.debug_info_relocs.deinit(self.allocator);
        self.debug_frame_relocs.deinit(self.allocator);
    }

    pub fn setSourceInfo(self: *DwarfBuilder, file: []const u8, text: []const u8) void {
        self.source_file = file;
        self.source_text = text;
        self.comp_dir = if (std.mem.lastIndexOf(u8, file, "/")) |idx| file[0..idx] else ".";
    }

    pub fn setTextSize(self: *DwarfBuilder, size: u64) void {
        self.text_size = size;
    }

    pub fn setFuncInfos(self: *DwarfBuilder, infos: []const DebugFuncInfo) void {
        self.func_infos = infos;
    }

    pub fn setTypeRegistry(self: *DwarfBuilder, reg: *const @import("../../frontend/types.zig").TypeRegistry) void {
        self.type_reg = reg;
    }

    fn sourceOffsetToLine(self: *DwarfBuilder, offset: u32) u32 {
        if (offset >= self.source_text.len) return 1;
        var line: u32 = 1;
        for (self.source_text[0..offset]) |c| {
            if (c == '\n') line += 1;
        }
        return line;
    }

    pub fn generate(self: *DwarfBuilder, line_entries: []const LineEntry, text_symbol_idx: u32) !void {
        try self.generateDebugAbbrev();
        try self.generateDebugInfo(text_symbol_idx);
        try self.generateDebugLine(line_entries, text_symbol_idx);
        try self.generateDebugFrame(text_symbol_idx);
    }

    fn generateDebugAbbrev(self: *DwarfBuilder) !void {
        const buf = &self.debug_abbrev;
        const alloc = self.allocator;

        // Abbreviation 1: DW_TAG_compile_unit (DW_CHILDREN_yes — contains subprograms)
        try appendUleb128(buf, alloc, 1); // Abbreviation code
        try appendUleb128(buf, alloc, DW_TAG_compile_unit);
        try buf.append(alloc, DW_CHILDREN_yes);

        // Attributes following Go's DW_ABRV_COMPUNIT order:
        // name, language, stmt_list, low_pc, high_pc, comp_dir, producer
        const cu_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_language, DW_FORM_data1 },
            .{ DW_AT_stmt_list, DW_FORM_sec_offset },
            .{ DW_AT_low_pc, DW_FORM_addr },
            .{ DW_AT_high_pc, DW_FORM_data8 },
            .{ DW_AT_comp_dir, DW_FORM_string },
            .{ DW_AT_producer, DW_FORM_string },
        };
        for (cu_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0); // End attr list
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 2: DW_TAG_subprogram (DW_CHILDREN_yes — allows child variable DIEs later)
        // Following Go's DW_ABRV_FUNCTION pattern (dwarf.go line 462-474)
        try appendUleb128(buf, alloc, 2); // Abbreviation code
        try appendUleb128(buf, alloc, DW_TAG_subprogram);
        try buf.append(alloc, DW_CHILDREN_yes);

        const sp_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_low_pc, DW_FORM_addr },
            .{ DW_AT_high_pc, DW_FORM_data8 },
            .{ DW_AT_frame_base, DW_FORM_block1 },
            .{ DW_AT_decl_file, DW_FORM_data4 },
            .{ DW_AT_decl_line, DW_FORM_data4 },
            .{ DW_AT_external, DW_FORM_flag },
        };
        for (sp_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0); // End attr list
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 3: DW_TAG_formal_parameter (DW_CHILDREN_no)
        try appendUleb128(buf, alloc, 3);
        try appendUleb128(buf, alloc, DW_TAG_formal_parameter);
        try buf.append(alloc, DW_CHILDREN_no);
        const param_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_location, DW_FORM_exprloc },
            .{ DW_AT_type, DW_FORM_ref4 },
        };
        for (param_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 4: DW_TAG_variable (DW_CHILDREN_no)
        try appendUleb128(buf, alloc, 4);
        try appendUleb128(buf, alloc, DW_TAG_variable);
        try buf.append(alloc, DW_CHILDREN_no);
        const var_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_location, DW_FORM_exprloc },
            .{ DW_AT_type, DW_FORM_ref4 },
        };
        for (var_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 5: DW_TAG_base_type (DW_CHILDREN_no)
        try appendUleb128(buf, alloc, 5);
        try appendUleb128(buf, alloc, DW_TAG_base_type);
        try buf.append(alloc, DW_CHILDREN_no);
        const bt_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_byte_size, DW_FORM_data1 },
            .{ DW_AT_encoding, DW_FORM_data1 },
        };
        for (bt_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 6: DW_TAG_pointer_type (DW_CHILDREN_no)
        try appendUleb128(buf, alloc, 6);
        try appendUleb128(buf, alloc, DW_TAG_pointer_type);
        try buf.append(alloc, DW_CHILDREN_no);
        const ptr_attrs = [_][2]u8{
            .{ DW_AT_byte_size, DW_FORM_data1 },
            .{ DW_AT_type, DW_FORM_ref4 },
        };
        for (ptr_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 7: DW_TAG_structure_type (DW_CHILDREN_yes — has member children)
        try appendUleb128(buf, alloc, 7);
        try appendUleb128(buf, alloc, DW_TAG_structure_type);
        try buf.append(alloc, DW_CHILDREN_yes);
        const struct_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_byte_size, DW_FORM_udata },
        };
        for (struct_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        // Abbreviation 8: DW_TAG_member (DW_CHILDREN_no)
        try appendUleb128(buf, alloc, 8);
        try appendUleb128(buf, alloc, @as(u8, 0x0d)); // DW_TAG_member
        try buf.append(alloc, DW_CHILDREN_no);
        const member_attrs = [_][2]u8{
            .{ DW_AT_name, DW_FORM_string },
            .{ DW_AT_data_member_location, DW_FORM_udata },
            .{ DW_AT_type, DW_FORM_ref4 },
        };
        for (member_attrs) |attr| {
            try appendUleb128(buf, alloc, attr[0]);
            try appendUleb128(buf, alloc, attr[1]);
        }
        try appendUleb128(buf, alloc, 0);
        try appendUleb128(buf, alloc, 0);

        try appendUleb128(buf, alloc, 0); // End abbreviations table
    }

    fn generateDebugInfo(self: *DwarfBuilder, text_symbol_idx: u32) !void {
        const buf = &self.debug_info;
        const alloc = self.allocator;

        const unit_length_offset = buf.items.len;
        try buf.appendNTimes(alloc, 0, 4);
        const unit_start = buf.items.len;

        // DWARF version 4, abbrev offset 0, address size 8
        try buf.appendSlice(alloc, &[_]u8{ 4, 0, 0, 0, 0, 0, 8 });

        try appendUleb128(buf, alloc, 1); // DIE: compile_unit

        // DW_AT_name (DW_FORM_string)
        try buf.appendSlice(alloc, self.source_file);
        try buf.append(alloc, 0);

        // DW_AT_language (DW_FORM_data1) — DW_LANG_C for lldb compatibility
        try buf.append(alloc, DW_LANG_C);

        // DW_AT_stmt_list (DW_FORM_sec_offset)
        try buf.appendNTimes(alloc, 0, 4);

        // DW_AT_low_pc (DW_FORM_addr) — needs relocation
        try self.debug_info_relocs.append(alloc, .{
            .offset = @intCast(buf.items.len),
            .symbol_idx = text_symbol_idx,
        });
        try buf.appendNTimes(alloc, 0, 8);

        // DW_AT_high_pc (DW_FORM_data8) — size of text section
        try buf.appendSlice(alloc, &std.mem.toBytes(self.text_size));

        // DW_AT_comp_dir (DW_FORM_string)
        try buf.appendSlice(alloc, self.comp_dir);
        try buf.append(alloc, 0);

        // DW_AT_producer (DW_FORM_string)
        try buf.appendSlice(alloc, "Cot 0.3.7");
        try buf.append(alloc, 0);

        // Emit base type DIEs BEFORE subprogram DIEs so type references are valid.
        // Track each type's offset in .debug_info for DW_FORM_ref4 references.
        const BaseTypeEntry = struct {
            name: []const u8,
            byte_size: u8,
            encoding: u8,
        };
        const base_types = [_]BaseTypeEntry{
            .{ .name = "i8", .byte_size = 1, .encoding = DW_ATE_signed },
            .{ .name = "i16", .byte_size = 2, .encoding = DW_ATE_signed },
            .{ .name = "i32", .byte_size = 4, .encoding = DW_ATE_signed },
            .{ .name = "i64", .byte_size = 8, .encoding = DW_ATE_signed },
            .{ .name = "u8", .byte_size = 1, .encoding = DW_ATE_unsigned },
            .{ .name = "u16", .byte_size = 2, .encoding = DW_ATE_unsigned },
            .{ .name = "u32", .byte_size = 4, .encoding = DW_ATE_unsigned },
            .{ .name = "u64", .byte_size = 8, .encoding = DW_ATE_unsigned },
            .{ .name = "f32", .byte_size = 4, .encoding = DW_ATE_float },
            .{ .name = "f64", .byte_size = 8, .encoding = DW_ATE_float },
            .{ .name = "bool", .byte_size = 1, .encoding = DW_ATE_boolean },
            .{ .name = "void", .byte_size = 0, .encoding = DW_ATE_signed },
        };

        // Map type name → offset in .debug_info for DW_FORM_ref4 references
        var type_offsets: std.StringHashMapUnmanaged(u32) = .{};
        defer type_offsets.deinit(alloc);

        for (base_types) |bt| {
            const die_offset: u32 = @intCast(buf.items.len);
            try type_offsets.put(alloc, bt.name, die_offset);

            try appendUleb128(buf, alloc, 5); // Abbreviation code 5 = base_type
            try buf.appendSlice(alloc, bt.name);
            try buf.append(alloc, 0); // null terminator
            try buf.append(alloc, bt.byte_size); // DW_AT_byte_size
            try buf.append(alloc, bt.encoding); // DW_AT_encoding
        }

        // Also emit a pointer type DIE pointing to i64 (used for pointer-typed variables)
        const ptr_die_offset: u32 = @intCast(buf.items.len);
        try type_offsets.put(alloc, "pointer", ptr_die_offset);
        try appendUleb128(buf, alloc, 6); // Abbreviation code 6 = pointer_type
        try buf.append(alloc, 8); // DW_AT_byte_size = 8 (64-bit pointer)
        // DW_AT_type: reference to i64 type DIE
        const i64_offset = type_offsets.get("i64") orelse 0;
        try buf.appendSlice(alloc, &std.mem.toBytes(i64_offset));

        // Emit struct type DIEs from the type registry
        if (self.type_reg) |reg| {
            const types_mod = @import("../../frontend/types.zig");
            for (reg.types.items) |t| {
                if (t != .struct_type) continue;
                const st = t.struct_type;
                if (st.name.len == 0) continue; // skip anonymous

                // Record this struct's DIE offset for type references
                const struct_die_offset: u32 = @intCast(buf.items.len);
                try type_offsets.put(alloc, st.name, struct_die_offset);

                // Emit DW_TAG_structure_type (abbrev 7)
                try appendUleb128(buf, alloc, 7);
                try buf.appendSlice(alloc, st.name);
                try buf.append(alloc, 0); // null terminator
                try appendUleb128(buf, alloc, st.size); // byte_size

                // Emit DW_TAG_member for each field (abbrev 8)
                for (st.fields) |field| {
                    try appendUleb128(buf, alloc, 8);
                    try buf.appendSlice(alloc, field.name);
                    try buf.append(alloc, 0); // null terminator
                    try appendUleb128(buf, alloc, field.offset); // member location

                    // Resolve field type to DIE offset
                    const field_type_name = resolveTypeNameForDwarf(reg, field.type_idx);
                    const field_type_offset = type_offsets.get(field_type_name) orelse
                        type_offsets.get("i64") orelse 0;
                    try buf.appendSlice(alloc, &std.mem.toBytes(field_type_offset));
                }

                // Null terminator to close struct's children list
                try buf.append(alloc, 0);
            }
            _ = types_mod;
        }

        // Emit DW_TAG_subprogram DIEs as children of compile_unit
        for (self.func_infos) |func_info| {
            try appendUleb128(buf, alloc, 2); // Abbreviation code 2 = subprogram

            // DW_AT_name: null-terminated string
            try buf.appendSlice(alloc, func_info.name);
            try buf.append(alloc, 0);

            // DW_AT_low_pc: 8-byte address (needs relocation to text symbol + func offset)
            // Write code_offset as the value — linker adds symbol's address to this
            try self.debug_info_relocs.append(alloc, .{
                .offset = @intCast(buf.items.len),
                .symbol_idx = text_symbol_idx,
            });
            try buf.appendSlice(alloc, &std.mem.toBytes(@as(u64, func_info.code_offset)));

            // DW_AT_high_pc: 8-byte function size (DWARF4 convention)
            try buf.appendSlice(alloc, &std.mem.toBytes(@as(u64, func_info.code_size)));

            // DW_AT_frame_base: 1-byte block containing DW_OP_call_frame_cfa
            try buf.append(alloc, 1); // block length
            try buf.append(alloc, DW_OP_call_frame_cfa);

            // DW_AT_decl_file: 4-byte file index (1 = main source file)
            try buf.appendSlice(alloc, &std.mem.toBytes(@as(u32, 1)));

            // DW_AT_decl_line: 4-byte line number
            try buf.appendSlice(alloc, &std.mem.toBytes(func_info.source_line));

            // DW_AT_external: 1-byte flag (1 if externally visible)
            const is_external: u8 = if (func_info.name.len > 0 and func_info.name[0] != '_') 1 else 0;
            try buf.append(alloc, is_external);

            // Emit child DW_TAG_formal_parameter and DW_TAG_variable DIEs
            for (func_info.locals) |local| {
                // Abbreviation code: 3 for parameter, 4 for variable
                const abbrev_code: u64 = if (local.is_param) 3 else 4;
                try appendUleb128(buf, alloc, abbrev_code);

                // DW_AT_name: null-terminated string
                try buf.appendSlice(alloc, local.name);
                try buf.append(alloc, 0);

                // DW_AT_location: DW_FORM_exprloc
                // Encode DW_OP_fbreg <frame_offset> using a stack buffer
                var expr_bytes: [16]u8 = undefined;
                var expr_len: usize = 0;
                expr_bytes[0] = DW_OP_fbreg;
                expr_len = 1;
                // Encode SLEB128 of frame_offset into expr_bytes
                {
                    var value = local.frame_offset;
                    while (true) {
                        const c: u8 = @truncate(@as(u64, @bitCast(@as(i64, value))) & 0x7f);
                        const s: u8 = @truncate(@as(u64, @bitCast(@as(i64, value))) & 0x40);
                        value >>= 7;
                        if ((value != -1 or s == 0) and (value != 0 or s != 0)) {
                            expr_bytes[expr_len] = c | 0x80;
                            expr_len += 1;
                        } else {
                            expr_bytes[expr_len] = c;
                            expr_len += 1;
                            break;
                        }
                    }
                }
                // Emit ULEB128 expression length, then expression bytes
                try appendUleb128(buf, alloc, expr_len);
                try buf.appendSlice(alloc, expr_bytes[0..expr_len]);

                // DW_AT_type: DW_FORM_ref4 — offset to type DIE in .debug_info
                const type_ref = type_offsets.get(local.type_name) orelse
                    type_offsets.get("i64") orelse 0;
                try buf.appendSlice(alloc, &std.mem.toBytes(type_ref));
            }

            // Null DIE terminator to close subprogram's children list
            try buf.append(alloc, 0);
        }

        // Null DIE terminator to close compile_unit's children list
        try buf.append(alloc, 0);

        const unit_length: u32 = @intCast(buf.items.len - unit_start);
        @memcpy(buf.items[unit_length_offset..][0..4], &std.mem.toBytes(unit_length));
    }

    fn generateDebugLine(self: *DwarfBuilder, line_entries: []const LineEntry, text_symbol_idx: u32) !void {
        const buf = &self.debug_line;
        const alloc = self.allocator;

        const total_length_offset = buf.items.len;
        try buf.appendNTimes(alloc, 0, 4);
        const header_start = buf.items.len;

        // Version 4
        try buf.appendSlice(alloc, &[_]u8{ 4, 0 });

        const header_length_offset = buf.items.len;
        try buf.appendNTimes(alloc, 0, 4);
        const prologue_start = buf.items.len;

        // min_inst_len, max_ops, default_is_stmt, line_base, line_range, opcode_base
        try buf.appendSlice(alloc, &[_]u8{ 1, 1, 1, @bitCast(LINE_BASE), LINE_RANGE, OPCODE_BASE });

        // standard_opcode_lengths
        try buf.appendSlice(alloc, &[_]u8{ 0, 1, 1, 1, 1, 0, 0, 0, 1, 0 });

        // Directory table
        try buf.appendSlice(alloc, self.comp_dir);
        try buf.appendSlice(alloc, &[_]u8{ 0, 0 });

        // File table
        const filename = if (std.mem.lastIndexOf(u8, self.source_file, "/")) |idx|
            self.source_file[idx + 1 ..]
        else
            self.source_file;
        try buf.appendSlice(alloc, filename);
        try buf.appendSlice(alloc, &[_]u8{ 0, 1, 0, 0, 0 }); // null, dir_index=1, mtime=0, len=0, end

        const header_length: u32 = @intCast(buf.items.len - prologue_start);
        @memcpy(buf.items[header_length_offset..][0..4], &std.mem.toBytes(header_length));

        // Line number program
        try buf.append(alloc, 0); // Extended opcode marker
        try appendUleb128(buf, alloc, 1 + 8);
        try buf.append(alloc, DW_LNE_set_address);

        try self.debug_line_relocs.append(alloc, .{
            .offset = @intCast(buf.items.len),
            .symbol_idx = text_symbol_idx,
        });
        try buf.appendNTimes(alloc, 0, 8);

        var pc: u64 = 0;
        var line: i64 = 1;

        for (line_entries) |entry| {
            const new_line = self.sourceOffsetToLine(entry.source_offset);
            const delta_pc = entry.code_offset - pc;
            const delta_line: i64 = @as(i64, new_line) - line;
            try self.putPcLcDelta(delta_pc, delta_line);
            pc = entry.code_offset;
            line = new_line;
        }

        const final_delta = self.text_size - pc;
        if (final_delta > 0) {
            try buf.append(alloc, DW_LNS_advance_pc);
            try appendUleb128(buf, alloc, final_delta);
        }

        // End sequence
        try buf.appendSlice(alloc, &[_]u8{ 0, 1, DW_LNE_end_sequence });

        const total_length: u32 = @intCast(buf.items.len - header_start);
        @memcpy(buf.items[total_length_offset..][0..4], &std.mem.toBytes(total_length));
    }

    /// Generate .debug_frame section with CIE + per-function FDEs for stack unwinding.
    /// ARM64 CIE/FDE pattern following Cranelift's approach:
    ///   CIE: code_align=4, data_align=-8, return_reg=30 (LR)
    ///   FDE: CFA = SP + frame_size after stp, then CFA = FP after mov x29, sp
    fn generateDebugFrame(self: *DwarfBuilder, text_symbol_idx: u32) !void {
        const buf = &self.debug_frame;
        const alloc = self.allocator;

        // ===== CIE (Common Information Entry) =====
        const cie_start = buf.items.len;

        // Length placeholder (4 bytes)
        const cie_length_offset = buf.items.len;
        try buf.appendNTimes(alloc, 0, 4);
        const cie_data_start = buf.items.len;

        // CIE ID: 0xFFFFFFFF (DWARF32 marker for CIE in .debug_frame)
        try buf.appendSlice(alloc, &[_]u8{ 0xff, 0xff, 0xff, 0xff });

        // Version: 1
        try buf.append(alloc, 1);

        // Augmentation: empty string
        try buf.append(alloc, 0);

        // Code alignment factor: 4 (ARM64 instructions are 4 bytes)
        try appendUleb128(buf, alloc, 4);

        // Data alignment factor: -8 (signed, 8-byte stack slots)
        try appendSleb128(buf, alloc, -8);

        // Return address register: 30 (x30/LR on ARM64)
        try appendUleb128(buf, alloc, DWARF_REG_LR);

        // Initial instructions: CFA = SP + 0
        try buf.append(alloc, DW_CFA_def_cfa);
        try appendUleb128(buf, alloc, DWARF_REG_SP);
        try appendUleb128(buf, alloc, 0);

        // Pad CIE to 8-byte alignment
        while ((buf.items.len - cie_start) % 8 != 0) {
            try buf.append(alloc, DW_CFA_nop);
        }

        // Write CIE length
        const cie_length: u32 = @intCast(buf.items.len - cie_data_start);
        @memcpy(buf.items[cie_length_offset..][0..4], &std.mem.toBytes(cie_length));

        // ===== FDEs (Frame Description Entry, one per function) =====
        for (self.func_infos) |func| {
            if (func.code_size == 0) continue;

            const fde_length_offset = buf.items.len;
            try buf.appendNTimes(alloc, 0, 4);
            const fde_data_start = buf.items.len;

            // CIE pointer: offset from the start of .debug_frame section to the CIE
            // (In .debug_frame, this is an absolute section offset, not relative like .eh_frame)
            const cie_pointer: u32 = @intCast(cie_start);
            try buf.appendSlice(alloc, &std.mem.toBytes(cie_pointer));

            // Initial location: function start address (needs relocation)
            try self.debug_frame_relocs.append(alloc, .{
                .offset = @intCast(buf.items.len),
                .symbol_idx = text_symbol_idx,
            });
            // Write code_offset as the addend (linker adds text section base)
            try buf.appendSlice(alloc, &std.mem.toBytes(@as(u64, func.code_offset)));

            // Address range: function size
            try buf.appendSlice(alloc, &std.mem.toBytes(@as(u64, func.code_size)));

            // Frame size: use actual frame_size from compiled function metadata,
            // fall back to 16 (minimum: FP + LR) if not set.
            const frame_size: u32 = if (func.frame_size > 0) func.frame_size else 16;

            // Instructions:
            // After first instruction (stp x29, x30, [sp, #-frame_size]!):
            //   CFA = SP + frame_size
            //   x29 saved at CFA - frame_size (= SP + 0)
            //   x30 saved at CFA - frame_size + 8 (= SP + 8)

            // After instruction 0 (4 bytes): stp x29, x30, [sp, #-N]!
            // Advance by 1 instruction (code_align=4, so delta=1 means 4 bytes)
            try buf.append(alloc, DW_CFA_advance_loc | 1);

            // CFA = SP + frame_size
            try buf.append(alloc, DW_CFA_def_cfa_offset);
            try appendUleb128(buf, alloc, frame_size);

            // x29 (FP) saved at CFA - frame_size
            // DW_CFA_offset encodes: reg in low 6 bits, offset as ULEB128 factored by |data_align|
            // offset = frame_size / 8 (data_align = -8, so factor = frame_size/8)
            try buf.append(alloc, DW_CFA_offset | DWARF_REG_FP);
            try appendUleb128(buf, alloc, frame_size / 8);

            // x30 (LR) saved at CFA - frame_size + 8
            // factored offset = (frame_size - 8) / 8
            try buf.append(alloc, DW_CFA_offset | DWARF_REG_LR);
            try appendUleb128(buf, alloc, (frame_size - 8) / 8);

            // After instruction 1 (4 more bytes): mov x29, sp
            // CFA now tracks FP register
            try buf.append(alloc, DW_CFA_advance_loc | 1);
            try buf.append(alloc, DW_CFA_def_cfa_register);
            try appendUleb128(buf, alloc, DWARF_REG_FP);

            // Pad FDE to 8-byte alignment
            while ((buf.items.len - fde_length_offset) % 8 != 0) {
                try buf.append(alloc, DW_CFA_nop);
            }

            // Write FDE length
            const fde_length: u32 = @intCast(buf.items.len - fde_data_start);
            @memcpy(buf.items[fde_length_offset..][0..4], &std.mem.toBytes(fde_length));
        }
    }

    fn putPcLcDelta(self: *DwarfBuilder, delta_pc: u64, delta_lc: i64) !void {
        const buf = &self.debug_line;
        const alloc = self.allocator;

        var opcode: i64 = undefined;
        if (delta_lc < LINE_BASE) {
            opcode = OPCODE_BASE + (LINE_RANGE * @min(@as(i64, @intCast(delta_pc)), PC_RANGE));
        } else if (delta_lc < LINE_BASE + LINE_RANGE) {
            opcode = OPCODE_BASE + (delta_lc - LINE_BASE) + (LINE_RANGE * @min(@as(i64, @intCast(delta_pc)), PC_RANGE));
            if (opcode > 255) opcode -= LINE_RANGE;
        } else {
            opcode = OPCODE_BASE + (LINE_RANGE - 1) + (LINE_RANGE * @min(@as(i64, @intCast(delta_pc)), PC_RANGE));
            if (opcode > 255) opcode = 255;
        }

        const remaining_pc = delta_pc - @as(u64, @intCast(@divFloor(opcode - OPCODE_BASE, LINE_RANGE)));
        const remaining_lc = delta_lc - (@mod(opcode - OPCODE_BASE, LINE_RANGE) + LINE_BASE);

        if (remaining_pc != 0) {
            if (remaining_pc <= PC_RANGE) {
                opcode -= LINE_RANGE * @as(i64, @intCast(PC_RANGE - remaining_pc));
                try buf.append(alloc, DW_LNS_const_add_pc);
            } else if (remaining_pc >= (1 << 14) and remaining_pc < (1 << 16)) {
                try buf.append(alloc, DW_LNS_fixed_advance_pc);
                try buf.appendSlice(alloc, &std.mem.toBytes(@as(u16, @intCast(remaining_pc))));
            } else {
                try buf.append(alloc, DW_LNS_advance_pc);
                try appendUleb128(buf, alloc, remaining_pc);
            }
        }

        if (remaining_lc != 0) {
            try buf.append(alloc, DW_LNS_advance_line);
            try appendSleb128(buf, alloc, remaining_lc);
        }

        try buf.append(alloc, @intCast(@as(u8, @truncate(@as(u64, @bitCast(opcode))))));
    }
};

/// Resolve a type index to a DWARF type name for struct field references.
fn resolveTypeNameForDwarf(reg: *const @import("../../frontend/types.zig").TypeRegistry, type_idx: anytype) []const u8 {
    const t = reg.get(type_idx);
    return switch (t) {
        .basic => |bk| bk.name(),
        .struct_type => |st| st.name,
        .pointer => "pointer",
        else => "i64",
    };
}

// Tests

test "appendUleb128" {
    const alloc = std.testing.allocator;
    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(alloc);

    try appendUleb128(&buf, alloc, 0);
    try std.testing.expectEqual(@as(u8, 0), buf.items[0]);

    buf.clearRetainingCapacity();
    try appendUleb128(&buf, alloc, 127);
    try std.testing.expectEqual(@as(u8, 127), buf.items[0]);

    buf.clearRetainingCapacity();
    try appendUleb128(&buf, alloc, 128);
    try std.testing.expectEqual(@as(usize, 2), buf.items.len);
}

test "appendSleb128" {
    const alloc = std.testing.allocator;
    var buf = std.ArrayListUnmanaged(u8){};
    defer buf.deinit(alloc);

    try appendSleb128(&buf, alloc, 0);
    try std.testing.expectEqual(@as(u8, 0), buf.items[0]);

    buf.clearRetainingCapacity();
    try appendSleb128(&buf, alloc, -1);
    try std.testing.expectEqual(@as(u8, 0x7f), buf.items[0]);
}

test "DwarfBuilder init/deinit" {
    const alloc = std.testing.allocator;
    var builder = DwarfBuilder.init(alloc);
    defer builder.deinit();
    try std.testing.expectEqual(@as(usize, 0), builder.debug_line.items.len);
}

test "DwarfBuilder setSourceInfo" {
    const alloc = std.testing.allocator;
    var builder = DwarfBuilder.init(alloc);
    defer builder.deinit();

    builder.setSourceInfo("/path/to/file.cot", "fn main() {}");
    try std.testing.expectEqualStrings("/path/to", builder.comp_dir);
}

test "sourceOffsetToLine" {
    const alloc = std.testing.allocator;
    var builder = DwarfBuilder.init(alloc);
    defer builder.deinit();

    builder.source_text = "line1\nline2\nline3";
    try std.testing.expectEqual(@as(u32, 1), builder.sourceOffsetToLine(0));
    try std.testing.expectEqual(@as(u32, 2), builder.sourceOffsetToLine(6));
    try std.testing.expectEqual(@as(u32, 3), builder.sourceOffsetToLine(12));
}
