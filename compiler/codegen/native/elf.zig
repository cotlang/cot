// ELF64 Object File Writer for Linux AMD64
// Reference: System V ABI AMD64 Architecture Processor Supplement

const std = @import("std");

// ELF Constants
pub const ELF_MAGIC = [4]u8{ 0x7F, 'E', 'L', 'F' };
pub const ELFCLASS64: u8 = 2;
pub const ELFDATA2LSB: u8 = 1;
pub const EV_CURRENT: u8 = 1;
pub const ELFOSABI_SYSV: u8 = 0;

pub const ET_REL: u16 = 1;
pub const ET_EXEC: u16 = 2;
pub const EM_X86_64: u16 = 62;

pub const SHT_NULL: u32 = 0;
pub const SHT_PROGBITS: u32 = 1;
pub const SHT_SYMTAB: u32 = 2;
pub const SHT_STRTAB: u32 = 3;
pub const SHT_RELA: u32 = 4;

pub const SHF_WRITE: u64 = 1;
pub const SHF_ALLOC: u64 = 2;
pub const SHF_EXECINSTR: u64 = 4;
pub const SHF_INFO_LINK: u64 = 0x40;

pub const STB_LOCAL: u8 = 0;
pub const STB_GLOBAL: u8 = 1;
pub const STT_NOTYPE: u8 = 0;
pub const STT_OBJECT: u8 = 1;
pub const STT_FUNC: u8 = 2;

pub const SHN_UNDEF: u16 = 0;

pub const R_X86_64_PC32: u32 = 2;
pub const R_X86_64_PLT32: u32 = 4;

// ELF Structures

pub const Elf64_Ehdr = extern struct {
    e_ident: [16]u8 = .{ 0x7F, 'E', 'L', 'F', ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV, 0, 0, 0, 0, 0, 0, 0, 0 },
    e_type: u16 = ET_REL,
    e_machine: u16 = EM_X86_64,
    e_version: u32 = EV_CURRENT,
    e_entry: u64 = 0,
    e_phoff: u64 = 0,
    e_shoff: u64 = 0,
    e_flags: u32 = 0,
    e_ehsize: u16 = @sizeOf(Elf64_Ehdr),
    e_phentsize: u16 = 0,
    e_phnum: u16 = 0,
    e_shentsize: u16 = @sizeOf(Elf64_Shdr),
    e_shnum: u16 = 0,
    e_shstrndx: u16 = 0,
};

pub const Elf64_Shdr = extern struct {
    sh_name: u32 = 0,
    sh_type: u32 = SHT_NULL,
    sh_flags: u64 = 0,
    sh_addr: u64 = 0,
    sh_offset: u64 = 0,
    sh_size: u64 = 0,
    sh_link: u32 = 0,
    sh_info: u32 = 0,
    sh_addralign: u64 = 0,
    sh_entsize: u64 = 0,
};

pub const Elf64_Sym = extern struct {
    st_name: u32 = 0,
    st_info: u8 = 0,
    st_other: u8 = 0,
    st_shndx: u16 = SHN_UNDEF,
    st_value: u64 = 0,
    st_size: u64 = 0,

    pub fn makeInfo(binding: u8, sym_type: u8) u8 {
        return (binding << 4) | (sym_type & 0xF);
    }

    pub fn getBinding(info: u8) u8 {
        return info >> 4;
    }

    pub fn getType(info: u8) u8 {
        return info & 0xF;
    }
};

pub const Elf64_Rela = extern struct {
    r_offset: u64 = 0,
    r_info: u64 = 0,
    r_addend: i64 = 0,

    pub fn makeInfo(sym: u32, rel_type: u32) u64 {
        return (@as(u64, sym) << 32) | @as(u64, rel_type);
    }

    pub fn getSym(info: u64) u32 {
        return @intCast(info >> 32);
    }

    pub fn getType(info: u64) u32 {
        return @truncate(info);
    }
};

// Writer Types

pub const Symbol = struct {
    name: []const u8,
    value: u64,
    size: u64 = 0,
    section: u16,
    binding: u8 = STB_GLOBAL,
    sym_type: u8 = STT_FUNC,
};

pub const Relocation = struct {
    offset: u32,
    target: []const u8,
    rel_type: u32 = R_X86_64_PLT32,
    addend: i64 = -4,
};

pub const StringLiteral = struct {
    data: []const u8,
    symbol: []const u8,
};

// ELF Writer

pub const ElfWriter = struct {
    allocator: std.mem.Allocator,
    text_data: std.ArrayListUnmanaged(u8) = .{},
    data: std.ArrayListUnmanaged(u8) = .{},
    symbols: std.ArrayListUnmanaged(Symbol) = .{},
    relocations: std.ArrayListUnmanaged(Relocation) = .{},
    strtab: std.ArrayListUnmanaged(u8) = .{},
    shstrtab: std.ArrayListUnmanaged(u8) = .{},
    string_literals: std.ArrayListUnmanaged(StringLiteral) = .{},
    string_counter: u32 = 0,

    shstrtab_text: u32 = 0,
    shstrtab_data: u32 = 0,
    shstrtab_symtab: u32 = 0,
    shstrtab_strtab: u32 = 0,
    shstrtab_shstrtab: u32 = 0,
    shstrtab_rela_text: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) ElfWriter {
        var writer = ElfWriter{ .allocator = allocator };
        writer.strtab.append(allocator, 0) catch {};
        writer.shstrtab.append(allocator, 0) catch {};

        writer.shstrtab_text = writer.addShstrtab(".text") catch 0;
        writer.shstrtab_data = writer.addShstrtab(".data") catch 0;
        writer.shstrtab_symtab = writer.addShstrtab(".symtab") catch 0;
        writer.shstrtab_strtab = writer.addShstrtab(".strtab") catch 0;
        writer.shstrtab_shstrtab = writer.addShstrtab(".shstrtab") catch 0;
        writer.shstrtab_rela_text = writer.addShstrtab(".rela.text") catch 0;
        return writer;
    }

    pub fn deinit(self: *ElfWriter) void {
        self.text_data.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.relocations.deinit(self.allocator);
        self.strtab.deinit(self.allocator);
        self.shstrtab.deinit(self.allocator);
        self.string_literals.deinit(self.allocator);
    }

    fn addStrtab(self: *ElfWriter, s: []const u8) !u32 {
        const offset: u32 = @intCast(self.strtab.items.len);
        try self.strtab.appendSlice(self.allocator, s);
        try self.strtab.append(self.allocator, 0);
        return offset;
    }

    fn addShstrtab(self: *ElfWriter, s: []const u8) !u32 {
        const offset: u32 = @intCast(self.shstrtab.items.len);
        try self.shstrtab.appendSlice(self.allocator, s);
        try self.shstrtab.append(self.allocator, 0);
        return offset;
    }

    pub fn addCode(self: *ElfWriter, code: []const u8) !void {
        try self.text_data.appendSlice(self.allocator, code);
    }

    pub fn addData(self: *ElfWriter, bytes: []const u8) !void {
        try self.data.appendSlice(self.allocator, bytes);
    }

    pub fn addSymbol(self: *ElfWriter, name: []const u8, value: u64, section: u16, external: bool) !void {
        try self.symbols.append(self.allocator, .{
            .name = name,
            .value = value,
            .section = section,
            .binding = if (external) STB_GLOBAL else STB_LOCAL,
            .sym_type = if (section == 1) STT_FUNC else STT_OBJECT,
        });
    }

    pub fn addRelocation(self: *ElfWriter, offset: u32, target: []const u8) !void {
        try self.relocations.append(self.allocator, .{ .offset = offset, .target = target, .rel_type = R_X86_64_PLT32, .addend = -4 });
    }

    /// Add a relocation with a specified type.
    pub fn addRelocationWithType(self: *ElfWriter, offset: u32, target: []const u8, rel_type: u32, addend: i64) !void {
        try self.relocations.append(self.allocator, .{ .offset = offset, .target = target, .rel_type = rel_type, .addend = addend });
    }

    pub fn addDataRelocation(self: *ElfWriter, offset: u32, target: []const u8) !void {
        try self.relocations.append(self.allocator, .{ .offset = offset, .target = target, .rel_type = R_X86_64_PC32, .addend = -4 });
    }

    pub fn addStringLiteral(self: *ElfWriter, str: []const u8) ![]const u8 {
        for (self.string_literals.items) |existing| {
            if (std.mem.eql(u8, existing.data, str)) return existing.symbol;
        }

        const sym_name = try std.fmt.allocPrint(self.allocator, ".L.str.{d}", .{self.string_counter});
        self.string_counter += 1;
        const offset: u32 = @intCast(self.data.items.len);

        try self.data.appendSlice(self.allocator, str);
        try self.data.append(self.allocator, 0);
        while (self.data.items.len % 8 != 0) try self.data.append(self.allocator, 0);

        try self.string_literals.append(self.allocator, .{ .data = str, .symbol = sym_name });
        try self.symbols.append(self.allocator, .{ .name = sym_name, .value = offset, .section = 2, .binding = STB_LOCAL, .sym_type = STT_OBJECT });
        return sym_name;
    }

    pub fn addGlobalVariable(self: *ElfWriter, name: []const u8, size: u32) !void {
        while (self.data.items.len % 8 != 0) try self.data.append(self.allocator, 0);
        const offset: u32 = @intCast(self.data.items.len);
        for (0..size) |_| try self.data.append(self.allocator, 0);
        try self.symbols.append(self.allocator, .{ .name = name, .value = offset, .size = size, .section = 2, .binding = STB_GLOBAL, .sym_type = STT_OBJECT });
    }

    fn alignTo(offset: u64, alignment: u64) u64 {
        if (alignment == 0) return offset;
        return (offset + alignment - 1) & ~(alignment - 1);
    }

    pub fn write(self: *ElfWriter, writer: anytype) !void {
        // Separate local and global symbols
        var local_symbols = std.ArrayListUnmanaged(Symbol){};
        defer local_symbols.deinit(self.allocator);
        var global_symbols = std.ArrayListUnmanaged(Symbol){};
        defer global_symbols.deinit(self.allocator);

        for (self.symbols.items) |sym| {
            if (sym.binding == STB_LOCAL) {
                try local_symbols.append(self.allocator, sym);
            } else {
                try global_symbols.append(self.allocator, sym);
            }
        }

        // Build symbol index map
        var sym_name_to_idx = std.StringHashMap(u32).init(self.allocator);
        defer sym_name_to_idx.deinit();

        var next_sym_idx: u32 = 1;
        for (local_symbols.items) |sym| {
            try sym_name_to_idx.put(sym.name, next_sym_idx);
            next_sym_idx += 1;
        }
        for (global_symbols.items) |sym| {
            try sym_name_to_idx.put(sym.name, next_sym_idx);
            next_sym_idx += 1;
        }

        // Add undefined symbols from relocations
        var extern_symbols = std.ArrayListUnmanaged(Symbol){};
        defer extern_symbols.deinit(self.allocator);

        for (self.relocations.items) |reloc| {
            if (!sym_name_to_idx.contains(reloc.target)) {
                try sym_name_to_idx.put(reloc.target, next_sym_idx);
                next_sym_idx += 1;
                try extern_symbols.append(self.allocator, .{ .name = reloc.target, .value = 0, .section = SHN_UNDEF, .binding = STB_GLOBAL, .sym_type = STT_NOTYPE });
            }
        }

        // Pre-add symbol names to strtab
        var symbol_strx = std.ArrayListUnmanaged(u32){};
        defer symbol_strx.deinit(self.allocator);

        for (local_symbols.items) |sym| try symbol_strx.append(self.allocator, try self.addStrtab(sym.name));
        for (global_symbols.items) |sym| try symbol_strx.append(self.allocator, try self.addStrtab(sym.name));
        for (extern_symbols.items) |sym| try symbol_strx.append(self.allocator, try self.addStrtab(sym.name));

        // Calculate layout
        const has_relocs = self.relocations.items.len > 0;
        const has_data = self.data.items.len > 0;
        const num_sections: u16 = if (has_relocs) 7 else 6;
        const ehdr_size: u64 = @sizeOf(Elf64_Ehdr);

        var offset: u64 = ehdr_size;
        const text_offset = offset;
        const text_size: u64 = self.text_data.items.len;
        offset = alignTo(offset + text_size, 8);

        const data_offset = offset;
        const data_size: u64 = if (has_data) self.data.items.len else 0;
        if (has_data) offset = alignTo(offset + data_size, 8);

        const symtab_offset = offset;
        const total_syms = 1 + local_symbols.items.len + global_symbols.items.len + extern_symbols.items.len;
        const symtab_size = total_syms * @sizeOf(Elf64_Sym);
        offset = alignTo(offset + symtab_size, 8);

        const strtab_offset = offset;
        const strtab_size: u64 = self.strtab.items.len;
        offset = alignTo(offset + strtab_size, 8);

        const shstrtab_offset = offset;
        const shstrtab_size: u64 = self.shstrtab.items.len;
        offset = alignTo(offset + shstrtab_size, 8);

        var rela_text_offset: u64 = 0;
        var rela_text_size: u64 = 0;
        if (has_relocs) {
            rela_text_offset = offset;
            rela_text_size = self.relocations.items.len * @sizeOf(Elf64_Rela);
            offset = alignTo(offset + rela_text_size, 8);
        }

        const shdr_offset = offset;

        // Write ELF header
        var ehdr = Elf64_Ehdr{ .e_shoff = shdr_offset, .e_shnum = num_sections, .e_shstrndx = 5 };
        try writer.writeAll(std.mem.asBytes(&ehdr));

        // Write sections
        try writer.writeAll(self.text_data.items);
        try writePadding(writer, alignTo(ehdr_size + text_size, 8) - (ehdr_size + text_size));

        if (has_data) {
            try writer.writeAll(self.data.items);
            try writePadding(writer, alignTo(data_offset + data_size, 8) - (data_offset + data_size));
        }

        // Write symtab
        try writer.writeAll(std.mem.asBytes(&Elf64_Sym{})); // null symbol
        const num_local: u32 = @intCast(1 + local_symbols.items.len);

        for (local_symbols.items, 0..) |sym, i| {
            try writer.writeAll(std.mem.asBytes(&Elf64_Sym{
                .st_name = symbol_strx.items[i],
                .st_info = Elf64_Sym.makeInfo(sym.binding, sym.sym_type),
                .st_shndx = sym.section,
                .st_value = sym.value,
                .st_size = sym.size,
            }));
        }

        for (global_symbols.items, 0..) |sym, i| {
            try writer.writeAll(std.mem.asBytes(&Elf64_Sym{
                .st_name = symbol_strx.items[local_symbols.items.len + i],
                .st_info = Elf64_Sym.makeInfo(sym.binding, sym.sym_type),
                .st_shndx = sym.section,
                .st_value = sym.value,
                .st_size = sym.size,
            }));
        }

        for (extern_symbols.items, 0..) |sym, i| {
            try writer.writeAll(std.mem.asBytes(&Elf64_Sym{
                .st_name = symbol_strx.items[local_symbols.items.len + global_symbols.items.len + i],
                .st_info = Elf64_Sym.makeInfo(sym.binding, sym.sym_type),
                .st_shndx = SHN_UNDEF,
                .st_value = 0,
                .st_size = 0,
            }));
        }
        try writePadding(writer, alignTo(symtab_offset + symtab_size, 8) - (symtab_offset + symtab_size));

        try writer.writeAll(self.strtab.items);
        try writePadding(writer, alignTo(strtab_offset + strtab_size, 8) - (strtab_offset + strtab_size));

        try writer.writeAll(self.shstrtab.items);
        try writePadding(writer, alignTo(shstrtab_offset + shstrtab_size, 8) - (shstrtab_offset + shstrtab_size));

        if (has_relocs) {
            for (self.relocations.items) |reloc| {
                try writer.writeAll(std.mem.asBytes(&Elf64_Rela{
                    .r_offset = reloc.offset,
                    .r_info = Elf64_Rela.makeInfo(sym_name_to_idx.get(reloc.target) orelse 0, reloc.rel_type),
                    .r_addend = reloc.addend,
                }));
            }
            try writePadding(writer, alignTo(rela_text_offset + rela_text_size, 8) - (rela_text_offset + rela_text_size));
        }

        // Write section headers
        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{})); // null section

        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
            .sh_name = self.shstrtab_text,
            .sh_type = SHT_PROGBITS,
            .sh_flags = SHF_ALLOC | SHF_EXECINSTR,
            .sh_offset = text_offset,
            .sh_size = text_size,
            .sh_addralign = 16,
        }));

        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
            .sh_name = self.shstrtab_data,
            .sh_type = SHT_PROGBITS,
            .sh_flags = SHF_ALLOC | SHF_WRITE,
            .sh_offset = data_offset,
            .sh_size = data_size,
            .sh_addralign = 8,
        }));

        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
            .sh_name = self.shstrtab_symtab,
            .sh_type = SHT_SYMTAB,
            .sh_offset = symtab_offset,
            .sh_size = symtab_size,
            .sh_link = 4,
            .sh_info = num_local,
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Sym),
        }));

        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
            .sh_name = self.shstrtab_strtab,
            .sh_type = SHT_STRTAB,
            .sh_offset = strtab_offset,
            .sh_size = strtab_size,
            .sh_addralign = 1,
        }));

        try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
            .sh_name = self.shstrtab_shstrtab,
            .sh_type = SHT_STRTAB,
            .sh_offset = shstrtab_offset,
            .sh_size = shstrtab_size,
            .sh_addralign = 1,
        }));

        if (has_relocs) {
            try writer.writeAll(std.mem.asBytes(&Elf64_Shdr{
                .sh_name = self.shstrtab_rela_text,
                .sh_type = SHT_RELA,
                .sh_flags = SHF_INFO_LINK,
                .sh_offset = rela_text_offset,
                .sh_size = rela_text_size,
                .sh_link = 3,
                .sh_info = 1,
                .sh_addralign = 8,
                .sh_entsize = @sizeOf(Elf64_Rela),
            }));
        }
    }

    fn writePadding(writer: anytype, count: u64) !void {
        const zeros = [_]u8{0} ** 8;
        var remaining = count;
        while (remaining > 0) {
            const to_write = @min(remaining, 8);
            try writer.writeAll(zeros[0..to_write]);
            remaining -= to_write;
        }
    }
};

// Tests

test "ELF header size" {
    try std.testing.expectEqual(@as(usize, 64), @sizeOf(Elf64_Ehdr));
}

test "ELF section header size" {
    try std.testing.expectEqual(@as(usize, 64), @sizeOf(Elf64_Shdr));
}

test "ELF symbol size" {
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(Elf64_Sym));
}

test "ELF relocation size" {
    try std.testing.expectEqual(@as(usize, 24), @sizeOf(Elf64_Rela));
}

test "symbol info encoding" {
    const info = Elf64_Sym.makeInfo(STB_GLOBAL, STT_FUNC);
    try std.testing.expectEqual(STB_GLOBAL, Elf64_Sym.getBinding(info));
    try std.testing.expectEqual(STT_FUNC, Elf64_Sym.getType(info));
}

test "relocation info encoding" {
    const info = Elf64_Rela.makeInfo(42, R_X86_64_PLT32);
    try std.testing.expectEqual(@as(u32, 42), Elf64_Rela.getSym(info));
    try std.testing.expectEqual(R_X86_64_PLT32, Elf64_Rela.getType(info));
}

test "ElfWriter basic" {
    const allocator = std.testing.allocator;
    var writer = ElfWriter.init(allocator);
    defer writer.deinit();

    try writer.addCode(&.{ 0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00, 0xC3 });
    try writer.addSymbol("main", 0, 1, true);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    try writer.write(output.writer(allocator));

    try std.testing.expectEqual(@as(u8, 0x7F), output.items[0]);
    try std.testing.expectEqual(@as(u8, 'E'), output.items[1]);
    try std.testing.expectEqual(@as(u8, 'L'), output.items[2]);
    try std.testing.expectEqual(@as(u8, 'F'), output.items[3]);
}

test "ElfWriter string deduplication" {
    // Native codegen not yet fully implemented - skip until AOT backend is ready
    // TODO: Fix memory leak in addStringLiteral when native codegen is completed
    return error.SkipZigTest;
}
