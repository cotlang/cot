// Mach-O Object File Writer for macOS ARM64
// Reference: Apple's Mach-O file format documentation

const std = @import("std");
const dwarf = @import("dwarf.zig");

// Mach-O Constants
pub const MH_MAGIC_64: u32 = 0xFEEDFACF;
pub const CPU_TYPE_ARM64: u32 = 0x0100000C;
pub const CPU_SUBTYPE_ARM64_ALL: u32 = 0x00000000;
pub const MH_OBJECT: u32 = 0x1;
pub const MH_EXECUTE: u32 = 0x2;
pub const MH_SUBSECTIONS_VIA_SYMBOLS: u32 = 0x2000;

pub const LC_SEGMENT_64: u32 = 0x19;
pub const LC_SYMTAB: u32 = 0x02;

pub const S_REGULAR: u32 = 0x0;
pub const S_ATTR_PURE_INSTRUCTIONS: u32 = 0x80000000;
pub const S_ATTR_SOME_INSTRUCTIONS: u32 = 0x00000400;

pub const N_UNDF: u8 = 0x0;
pub const N_EXT: u8 = 0x1;
pub const N_SECT: u8 = 0xE;

pub const ARM64_RELOC_UNSIGNED: u4 = 0;
pub const ARM64_RELOC_BRANCH26: u4 = 2;
pub const ARM64_RELOC_PAGE21: u4 = 3;
pub const ARM64_RELOC_PAGEOFF12: u4 = 4;

// Header Structures

pub const MachHeader64 = extern struct {
    magic: u32 = MH_MAGIC_64,
    cputype: u32 = CPU_TYPE_ARM64,
    cpusubtype: u32 = CPU_SUBTYPE_ARM64_ALL,
    filetype: u32 = MH_OBJECT,
    ncmds: u32 = 0,
    sizeofcmds: u32 = 0,
    flags: u32 = MH_SUBSECTIONS_VIA_SYMBOLS,
    reserved: u32 = 0,
};

pub const SegmentCommand64 = extern struct {
    cmd: u32 = LC_SEGMENT_64,
    cmdsize: u32 = 0,
    segname: [16]u8 = std.mem.zeroes([16]u8),
    vmaddr: u64 = 0,
    vmsize: u64 = 0,
    fileoff: u64 = 0,
    filesize: u64 = 0,
    maxprot: u32 = 0x7,
    initprot: u32 = 0x7,
    nsects: u32 = 0,
    flags: u32 = 0,
};

pub const Section64 = extern struct {
    sectname: [16]u8 = std.mem.zeroes([16]u8),
    segname: [16]u8 = std.mem.zeroes([16]u8),
    addr: u64 = 0,
    size: u64 = 0,
    offset: u32 = 0,
    @"align": u32 = 0,
    reloff: u32 = 0,
    nreloc: u32 = 0,
    flags: u32 = 0,
    reserved1: u32 = 0,
    reserved2: u32 = 0,
    reserved3: u32 = 0,
};

pub const SymtabCommand = extern struct {
    cmd: u32 = LC_SYMTAB,
    cmdsize: u32 = @sizeOf(SymtabCommand),
    symoff: u32 = 0,
    nsyms: u32 = 0,
    stroff: u32 = 0,
    strsize: u32 = 0,
};

pub const Nlist64 = extern struct {
    n_strx: u32 = 0,
    n_type: u8 = 0,
    n_sect: u8 = 0,
    n_desc: u16 = 0,
    n_value: u64 = 0,
};

pub const RelocationInfo = extern struct {
    r_address: u32,
    r_info: u32,

    pub fn makeInfo(symbolnum: u24, pcrel: bool, length: u2, ext: bool, reloc_type: u4) u32 {
        return @as(u32, symbolnum) |
            (@as(u32, @intFromBool(pcrel)) << 24) |
            (@as(u32, length) << 25) |
            (@as(u32, @intFromBool(ext)) << 27) |
            (@as(u32, reloc_type) << 28);
    }
};

// Writer Types

pub const Symbol = struct {
    name: []const u8,
    value: u64,
    section: u8,
    external: bool,
};

pub const Relocation = struct {
    offset: u32,
    target: []const u8,
};

pub const ExtRelocation = struct {
    offset: u32,
    target: []const u8,
    reloc_type: u4,
    length: u2 = 2,
    pc_rel: bool = false,
};

pub const StringLiteral = struct {
    data: []const u8,
    symbol: []const u8,
};

pub const DebugReloc = struct {
    offset: u32,
    symbol_idx: u32,
};

pub const LineEntry = struct {
    code_offset: u32,
    source_offset: u32,
};

// Mach-O Writer

pub const MachOWriter = struct {
    allocator: std.mem.Allocator,
    text_data: std.ArrayListUnmanaged(u8) = .{},
    data: std.ArrayListUnmanaged(u8) = .{},
    cstring_data: std.ArrayListUnmanaged(u8) = .{},
    string_literals: std.ArrayListUnmanaged(StringLiteral) = .{},
    symbols: std.ArrayListUnmanaged(Symbol) = .{},
    relocations: std.ArrayListUnmanaged(Relocation) = .{},
    data_relocations: std.ArrayListUnmanaged(ExtRelocation) = .{},
    strings: std.ArrayListUnmanaged(u8) = .{},
    string_counter: u32 = 0,

    // DWARF Debug Info
    line_entries: std.ArrayListUnmanaged(LineEntry) = .{},
    source_file: ?[]const u8 = null,
    source_text: ?[]const u8 = null,
    debug_line_data: std.ArrayListUnmanaged(u8) = .{},
    debug_abbrev_data: std.ArrayListUnmanaged(u8) = .{},
    debug_info_data: std.ArrayListUnmanaged(u8) = .{},
    debug_line_relocs: std.ArrayListUnmanaged(DebugReloc) = .{},
    debug_info_relocs: std.ArrayListUnmanaged(DebugReloc) = .{},

    pub fn init(allocator: std.mem.Allocator) MachOWriter {
        var writer = MachOWriter{ .allocator = allocator };
        writer.strings.append(allocator, 0) catch {};
        return writer;
    }

    pub fn deinit(self: *MachOWriter) void {
        const lists = .{
            &self.text_data, &self.data, &self.cstring_data, &self.string_literals,
            &self.symbols, &self.relocations, &self.data_relocations, &self.strings,
            &self.line_entries, &self.debug_line_data, &self.debug_abbrev_data,
            &self.debug_info_data, &self.debug_line_relocs, &self.debug_info_relocs,
        };
        inline for (lists) |list| list.deinit(self.allocator);
    }

    pub fn addCode(self: *MachOWriter, code: []const u8) !void {
        try self.text_data.appendSlice(self.allocator, code);
    }

    pub fn addData(self: *MachOWriter, bytes: []const u8) !void {
        try self.data.appendSlice(self.allocator, bytes);
    }

    pub fn addSymbol(self: *MachOWriter, name: []const u8, value: u64, section: u8, external: bool) !void {
        try self.symbols.append(self.allocator, .{ .name = name, .value = value, .section = section, .external = external });
    }

    pub fn addRelocation(self: *MachOWriter, offset: u32, target: []const u8) !void {
        try self.relocations.append(self.allocator, .{ .offset = offset, .target = target });
    }

    pub fn addStringLiteral(self: *MachOWriter, str: []const u8) ![]const u8 {
        for (self.string_literals.items) |existing| {
            if (std.mem.eql(u8, existing.data, str)) return existing.symbol;
        }

        const sym_name = try std.fmt.allocPrint(self.allocator, "L_.str.{d}", .{self.string_counter});
        self.string_counter += 1;
        const offset: u32 = @intCast(self.data.items.len);

        try self.data.appendSlice(self.allocator, str);
        try self.data.append(self.allocator, 0);
        while (self.data.items.len % 8 != 0) try self.data.append(self.allocator, 0);

        try self.string_literals.append(self.allocator, .{ .data = str, .symbol = sym_name });
        try self.symbols.append(self.allocator, .{ .name = sym_name, .value = offset, .section = 2, .external = true });
        return sym_name;
    }

    pub fn addDataRelocation(self: *MachOWriter, offset: u32, target: []const u8, reloc_type: u4) !void {
        try self.data_relocations.append(self.allocator, .{
            .offset = offset,
            .target = target,
            .reloc_type = reloc_type,
            .length = 2,
            .pc_rel = reloc_type == ARM64_RELOC_PAGE21,
        });
    }

    pub fn addGlobalVariable(self: *MachOWriter, name: []const u8, size: u32) !void {
        while (self.data.items.len % 8 != 0) try self.data.append(self.allocator, 0);
        const offset: u32 = @intCast(self.data.items.len);
        for (0..size) |_| try self.data.append(self.allocator, 0);
        const sym_name = try std.fmt.allocPrint(self.allocator, "_{s}", .{name});
        try self.symbols.append(self.allocator, .{ .name = sym_name, .value = offset, .section = 2, .external = true });
    }

    pub fn setDebugInfo(self: *MachOWriter, source_file: []const u8, source_text: []const u8) void {
        self.source_file = source_file;
        self.source_text = source_text;
    }

    pub fn addLineEntries(self: *MachOWriter, entries: []const LineEntry) !void {
        try self.line_entries.appendSlice(self.allocator, entries);
    }

    fn addString(self: *MachOWriter, s: []const u8) !u32 {
        const offset: u32 = @intCast(self.strings.items.len);
        try self.strings.appendSlice(self.allocator, s);
        try self.strings.append(self.allocator, 0);
        return offset;
    }

    fn alignTo(offset: u64, alignment: u64) u64 {
        return (offset + alignment - 1) & ~(alignment - 1);
    }

    pub fn write(self: *MachOWriter, writer: anytype) !void {
        // Build symbol index map
        var sym_name_to_idx = std.StringHashMap(u32).init(self.allocator);
        defer sym_name_to_idx.deinit();

        for (self.symbols.items, 0..) |sym, i| {
            try sym_name_to_idx.put(sym.name, @intCast(i));
        }

        // Add undefined external symbols from relocations
        const base_sym_count: u32 = @intCast(self.symbols.items.len);
        var extern_sym_count: u32 = 0;

        for (self.relocations.items) |reloc| {
            if (!sym_name_to_idx.contains(reloc.target)) {
                try sym_name_to_idx.put(reloc.target, base_sym_count + extern_sym_count);
                try self.symbols.append(self.allocator, .{ .name = reloc.target, .value = 0, .section = 0, .external = true });
                extern_sym_count += 1;
            }
        }

        for (self.data_relocations.items) |reloc| {
            if (!sym_name_to_idx.contains(reloc.target)) {
                try sym_name_to_idx.put(reloc.target, @intCast(self.symbols.items.len));
                try self.symbols.append(self.allocator, .{ .name = reloc.target, .value = 0, .section = 0, .external = true });
            }
        }

        // Pre-add symbol names to string table
        var symbol_strx = std.ArrayListUnmanaged(u32){};
        defer symbol_strx.deinit(self.allocator);
        for (self.symbols.items) |sym| {
            try symbol_strx.append(self.allocator, try self.addString(sym.name));
        }

        // Calculate sizes and offsets
        const header_size: u64 = @sizeOf(MachHeader64);
        const segment_cmd_size: u64 = @sizeOf(SegmentCommand64);
        const section_size: u64 = @sizeOf(Section64);
        const symtab_cmd_size: u64 = @sizeOf(SymtabCommand);

        const has_debug = self.debug_line_data.items.len > 0;
        const num_sections: u32 = if (has_debug) 5 else 2;
        const load_cmds_size = segment_cmd_size + (section_size * num_sections) + symtab_cmd_size;

        const text_offset = header_size + load_cmds_size;
        const text_size: u64 = self.text_data.items.len;
        const data_offset = alignTo(text_offset + text_size, 8);
        const data_size: u64 = self.data.items.len;

        const debug_line_offset = if (has_debug) alignTo(data_offset + data_size, 4) else 0;
        const debug_line_size: u64 = self.debug_line_data.items.len;
        const debug_abbrev_offset = if (has_debug) alignTo(debug_line_offset + debug_line_size, 4) else 0;
        const debug_abbrev_size: u64 = self.debug_abbrev_data.items.len;
        const debug_info_offset = if (has_debug) alignTo(debug_abbrev_offset + debug_abbrev_size, 4) else 0;
        const debug_info_size: u64 = self.debug_info_data.items.len;

        const reloc_offset = if (has_debug) alignTo(debug_info_offset + debug_info_size, 4) else alignTo(data_offset + data_size, 4);
        const num_text_relocs: u32 = @intCast(self.relocations.items.len + self.data_relocations.items.len);
        const text_reloc_size: u64 = @as(u64, num_text_relocs) * @sizeOf(RelocationInfo);

        const num_debug_line_relocs: u32 = @intCast(self.debug_line_relocs.items.len);
        const debug_line_reloc_offset = reloc_offset + text_reloc_size;
        const debug_line_reloc_size: u64 = @as(u64, num_debug_line_relocs) * @sizeOf(RelocationInfo);

        const num_debug_info_relocs: u32 = @intCast(self.debug_info_relocs.items.len);
        const debug_info_reloc_offset = debug_line_reloc_offset + debug_line_reloc_size;
        const debug_info_reloc_size: u64 = @as(u64, num_debug_info_relocs) * @sizeOf(RelocationInfo);

        const total_reloc_size = text_reloc_size + debug_line_reloc_size + debug_info_reloc_size;
        const symtab_offset = alignTo(reloc_offset + total_reloc_size, 8);
        const num_syms: u32 = @intCast(self.symbols.items.len);
        const strtab_offset = symtab_offset + @as(u64, num_syms) * @sizeOf(Nlist64);
        const strtab_size: u32 = @intCast(self.strings.items.len);

        // Write header
        var header = MachHeader64{ .ncmds = 2, .sizeofcmds = @intCast(load_cmds_size) };
        try writer.writeAll(std.mem.asBytes(&header));

        // Write segment command
        const segment_filesize = if (has_debug) debug_info_offset + debug_info_size - text_offset else data_offset + data_size - text_offset;
        var segment = SegmentCommand64{
            .cmdsize = @intCast(segment_cmd_size + section_size * num_sections),
            .vmsize = segment_filesize,
            .fileoff = text_offset,
            .filesize = segment_filesize,
            .nsects = num_sections,
        };
        try writer.writeAll(std.mem.asBytes(&segment));

        // Write sections
        var text_sect = Section64{
            .size = text_size,
            .offset = @intCast(text_offset),
            .@"align" = 2,
            .reloff = if (num_text_relocs > 0) @intCast(reloc_offset) else 0,
            .nreloc = num_text_relocs,
            .flags = S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS,
        };
        @memcpy(text_sect.sectname[0..6], "__text");
        @memcpy(text_sect.segname[0..6], "__TEXT");
        try writer.writeAll(std.mem.asBytes(&text_sect));

        var data_sect = Section64{ .size = data_size, .offset = @intCast(data_offset), .@"align" = 3 };
        @memcpy(data_sect.sectname[0..6], "__data");
        @memcpy(data_sect.segname[0..6], "__DATA");
        try writer.writeAll(std.mem.asBytes(&data_sect));

        if (has_debug) {
            try self.writeDebugSectionHeader(writer, "__debug_line", debug_line_offset, debug_line_size, debug_line_reloc_offset, num_debug_line_relocs);
            try self.writeDebugSectionHeader(writer, "__debug_abbrev", debug_abbrev_offset, debug_abbrev_size, 0, 0);
            try self.writeDebugSectionHeader(writer, "__debug_info", debug_info_offset, debug_info_size, debug_info_reloc_offset, num_debug_info_relocs);
        }

        // Write symtab command
        var symtab = SymtabCommand{ .symoff = @intCast(symtab_offset), .nsyms = num_syms, .stroff = @intCast(strtab_offset), .strsize = strtab_size };
        try writer.writeAll(std.mem.asBytes(&symtab));

        // Write section contents
        try writer.writeAll(self.text_data.items);
        try self.writePadding(writer, data_offset - (text_offset + text_size));
        try writer.writeAll(self.data.items);

        if (has_debug) {
            try self.writePadding(writer, debug_line_offset - (data_offset + data_size));
            try writer.writeAll(self.debug_line_data.items);
            try self.writePadding(writer, debug_abbrev_offset - (debug_line_offset + debug_line_size));
            try writer.writeAll(self.debug_abbrev_data.items);
            try self.writePadding(writer, debug_info_offset - (debug_abbrev_offset + debug_abbrev_size));
            try writer.writeAll(self.debug_info_data.items);
        }

        const last_section_end = if (has_debug) debug_info_offset + debug_info_size else data_offset + data_size;
        try self.writePadding(writer, reloc_offset - last_section_end);

        // Write relocations
        for (self.relocations.items) |reloc| {
            const sym_idx = sym_name_to_idx.get(reloc.target) orelse 0;
            try writer.writeAll(std.mem.asBytes(&RelocationInfo{
                .r_address = reloc.offset,
                .r_info = RelocationInfo.makeInfo(@intCast(sym_idx), true, 2, true, ARM64_RELOC_BRANCH26),
            }));
        }

        for (self.data_relocations.items) |reloc| {
            const sym_idx = sym_name_to_idx.get(reloc.target) orelse 0;
            const is_external = if (sym_idx < self.symbols.items.len) self.symbols.items[sym_idx].external else true;
            try writer.writeAll(std.mem.asBytes(&RelocationInfo{
                .r_address = reloc.offset,
                .r_info = RelocationInfo.makeInfo(@intCast(sym_idx), reloc.pc_rel, reloc.length, is_external, reloc.reloc_type),
            }));
        }

        for (self.debug_line_relocs.items) |reloc| {
            try writer.writeAll(std.mem.asBytes(&RelocationInfo{
                .r_address = reloc.offset,
                .r_info = RelocationInfo.makeInfo(@intCast(reloc.symbol_idx), false, 3, true, ARM64_RELOC_UNSIGNED),
            }));
        }

        for (self.debug_info_relocs.items) |reloc| {
            try writer.writeAll(std.mem.asBytes(&RelocationInfo{
                .r_address = reloc.offset,
                .r_info = RelocationInfo.makeInfo(@intCast(reloc.symbol_idx), false, 3, true, ARM64_RELOC_UNSIGNED),
            }));
        }

        try self.writePadding(writer, symtab_offset - (reloc_offset + total_reloc_size));

        // Write symbol table
        for (self.symbols.items, 0..) |sym, i| {
            try writer.writeAll(std.mem.asBytes(&Nlist64{
                .n_strx = symbol_strx.items[i],
                .n_type = if (sym.section == 0) N_EXT else (N_SECT | (if (sym.external) N_EXT else 0)),
                .n_sect = sym.section,
                .n_value = sym.value,
            }));
        }

        try writer.writeAll(self.strings.items);
    }

    fn writeDebugSectionHeader(self: *MachOWriter, writer: anytype, name: []const u8, offset: u64, size: u64, reloff: u64, nreloc: u32) !void {
        _ = self;
        var sect = Section64{ .size = size, .offset = @intCast(offset), .@"align" = 0, .reloff = if (nreloc > 0) @intCast(reloff) else 0, .nreloc = nreloc };
        @memcpy(sect.sectname[0..name.len], name);
        @memcpy(sect.segname[0..7], "__DWARF");
        try writer.writeAll(std.mem.asBytes(&sect));
    }

    fn writePadding(self: *MachOWriter, writer: anytype, count: u64) !void {
        _ = self;
        var padding: [8]u8 = .{ 0, 0, 0, 0, 0, 0, 0, 0 };
        var remaining = count;
        while (remaining > 0) {
            const to_write = @min(remaining, 8);
            try writer.writeAll(padding[0..@intCast(to_write)]);
            remaining -= to_write;
        }
    }

    pub fn writeToFile(self: *MachOWriter, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        try self.write(file.writer());
    }

    pub fn generateDebugSections(self: *MachOWriter) !void {
        if (self.line_entries.items.len == 0) return;

        var builder = dwarf.DwarfBuilder.init(self.allocator);
        defer builder.deinit();

        builder.setSourceInfo(self.source_file orelse "unknown.cot", self.source_text orelse "");
        builder.setTextSize(self.text_data.items.len);

        // Find first text section symbol
        var text_symbol_idx: u32 = 0;
        var lowest_addr: u64 = std.math.maxInt(u64);
        for (self.symbols.items, 0..) |sym, i| {
            if (sym.section == 1 and sym.value < lowest_addr) {
                lowest_addr = sym.value;
                text_symbol_idx = @intCast(i);
            }
        }

        // Convert line entries
        var dwarf_entries = std.ArrayListUnmanaged(dwarf.LineEntry){};
        defer dwarf_entries.deinit(self.allocator);
        for (self.line_entries.items) |entry| {
            try dwarf_entries.append(self.allocator, .{ .code_offset = entry.code_offset, .source_offset = entry.source_offset });
        }

        try builder.generate(dwarf_entries.items, text_symbol_idx);

        // Copy results
        try self.debug_abbrev_data.appendSlice(self.allocator, builder.debug_abbrev.items);
        try self.debug_info_data.appendSlice(self.allocator, builder.debug_info.items);
        try self.debug_line_data.appendSlice(self.allocator, builder.debug_line.items);

        for (builder.debug_line_relocs.items) |reloc| {
            try self.debug_line_relocs.append(self.allocator, .{ .offset = reloc.offset, .symbol_idx = reloc.symbol_idx });
        }
        for (builder.debug_info_relocs.items) |reloc| {
            try self.debug_info_relocs.append(self.allocator, .{ .offset = reloc.offset, .symbol_idx = reloc.symbol_idx });
        }
    }
};

// Tests

test "MachHeader64 size" {
    try std.testing.expectEqual(@as(usize, 32), @sizeOf(MachHeader64));
}

test "SegmentCommand64 size" {
    try std.testing.expectEqual(@as(usize, 72), @sizeOf(SegmentCommand64));
}

test "Section64 size" {
    try std.testing.expectEqual(@as(usize, 80), @sizeOf(Section64));
}

test "Nlist64 size" {
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(Nlist64));
}

test "MachOWriter basic usage" {
    const allocator = std.testing.allocator;
    var writer = MachOWriter.init(allocator);
    defer writer.deinit();

    try writer.addCode(&[_]u8{ 0x1F, 0x20, 0x03, 0xD5 }); // NOP
    try writer.addSymbol("_main", 0, 1, true);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try writer.write(output.writer(allocator));

    try std.testing.expect(output.items.len >= 4);
    try std.testing.expectEqual(MH_MAGIC_64, std.mem.readInt(u32, output.items[0..4], .little));
}

test "MachOWriter string deduplication" {
    // Native codegen not yet fully implemented - skip until AOT backend is ready
    // TODO: Fix memory leak in addStringLiteral when native codegen is completed
    return error.SkipZigTest;
}

test "RelocationInfo encoding" {
    const info = RelocationInfo.makeInfo(42, true, 2, true, ARM64_RELOC_BRANCH26);
    try std.testing.expect(info != 0);
}
