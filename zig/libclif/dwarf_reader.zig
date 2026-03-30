//! DWARF Runtime Reader — PC → file:line:column resolution
//!
//! Linked into debug Cot binaries. At crash time, reads DWARF .debug_line
//! from the binary's own executable to resolve PC addresses to source locations.
//!
//! Algorithm (ported from Zig lib/std/debug/Dwarf.zig line program decoder):
//! 1. On first call: find own binary path and mmap it
//! 2. Parse Mach-O/ELF headers to find .debug_line section
//! 3. Execute DWARF line number program state machine
//! 4. Print "  at <filename>:<line>:<column>\n" to stderr
//!
//! Signal-safe: uses mmap (not malloc), stack buffers for temp work.
//! No external dependencies — only libc via Zig builtins.

const builtin = @import("builtin");
const std = @import("std");

// ============================================================================
// Platform-specific constants
// ============================================================================

const is_macos = builtin.os.tag == .macos;
const is_linux = builtin.os.tag == .linux;

// Mach-O constants
const MH_MAGIC_64: u32 = 0xFEEDFACF;
const LC_SEGMENT_64: u32 = 0x19;

// ELF constants
const ELF_MAGIC: [4]u8 = .{ 0x7F, 'E', 'L', 'F' };
const ELFCLASS64: u8 = 2;
const SHT_PROGBITS: u32 = 1;

// DWARF line program opcodes
const DW_LNS_copy: u8 = 1;
const DW_LNS_advance_pc: u8 = 2;
const DW_LNS_advance_line: u8 = 3;
const DW_LNS_set_file: u8 = 4;
const DW_LNS_set_column: u8 = 5;
const DW_LNS_negate_stmt: u8 = 6;
const DW_LNS_set_basic_block: u8 = 7;
const DW_LNS_const_add_pc: u8 = 8;
const DW_LNS_fixed_advance_pc: u8 = 9;
const DW_LNS_set_prologue_end: u8 = 10;
const DW_LNS_set_epilogue_begin: u8 = 11;
const DW_LNS_set_isa: u8 = 12;

// DWARF line program extended opcodes
const DW_LNE_end_sequence: u8 = 1;
const DW_LNE_set_address: u8 = 2;
const DW_LNE_define_file: u8 = 3;

// mmap constants
const PROT_READ: c_int = 0x1;
const MAP_PRIVATE: c_int = if (is_macos) 0x2 else 0x2;
const MAP_FAILED: usize = ~@as(usize, 0);

// ============================================================================
// Mach-O structures (matching <mach-o/loader.h>)
// ============================================================================

const MachHeader64 = extern struct {
    magic: u32,
    cputype: i32,
    cpusubtype: i32,
    filetype: u32,
    ncmds: u32,
    sizeofcmds: u32,
    flags: u32,
    reserved: u32,
};

const LoadCommand = extern struct {
    cmd: u32,
    cmdsize: u32,
};

const SegmentCommand64 = extern struct {
    cmd: u32,
    cmdsize: u32,
    segname: [16]u8,
    vmaddr: u64,
    vmsize: u64,
    fileoff: u64,
    filesize: u64,
    maxprot: i32,
    initprot: i32,
    nsects: u32,
    flags: u32,
};

const Section64 = extern struct {
    sectname: [16]u8,
    segname: [16]u8,
    addr: u64,
    size: u64,
    offset: u32,
    @"align": u32,
    reloff: u32,
    nreloc: u32,
    flags: u32,
    reserved1: u32,
    reserved2: u32,
    reserved3: u32,
};

// ============================================================================
// ELF structures
// ============================================================================

const Elf64Header = extern struct {
    e_ident: [16]u8,
    e_type: u16,
    e_machine: u16,
    e_version: u32,
    e_entry: u64,
    e_phoff: u64,
    e_shoff: u64,
    e_flags: u32,
    e_ehsize: u16,
    e_phentsize: u16,
    e_phnum: u16,
    e_shentsize: u16,
    e_shnum: u16,
    e_shstrndx: u16,
};

const Elf64SectionHeader = extern struct {
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
};

// ============================================================================
// DWARF line program state machine
// ============================================================================

const LineState = struct {
    address: u64 = 0,
    file: u32 = 1,
    line: i64 = 1,
    column: u32 = 0,
    is_stmt: bool = true,
    basic_block: bool = false,
    end_sequence: bool = false,
    prologue_end: bool = false,
    epilogue_begin: bool = false,
    isa: u32 = 0,
};

const LineProgramHeader = struct {
    unit_length: u64 = 0,
    version: u16 = 0,
    header_length: u64 = 0,
    min_instr_length: u8 = 1,
    max_ops_per_instr: u8 = 1,
    default_is_stmt: bool = true,
    line_base: i8 = -5,
    line_range: u8 = 14,
    opcode_base: u8 = 13,
    // Offsets into the mapped binary for file/dir tables
    dirs_start: usize = 0,
    dirs_end: usize = 0,
    files_start: usize = 0,
    files_end: usize = 0,
    program_start: usize = 0,
    program_end: usize = 0,
};

// Max files we track in the file table
const MAX_FILES = 256;
const MAX_DIRS = 64;

// ============================================================================
// Global state (initialized on first call via mmap)
// ============================================================================

var g_initialized: bool = false;
var g_binary_base: [*]const u8 = undefined;
var g_binary_len: usize = 0;
var g_debug_line_offset: usize = 0;
var g_debug_line_size: usize = 0;
var g_load_addr_offset: i64 = 0; // Slide: runtime addr - file addr

// ============================================================================
// Libc imports
// ============================================================================

extern "c" fn write(fd: c_int, buf: [*]const u8, count: usize) isize;
extern "c" fn open(path: [*:0]const u8, flags: c_int) c_int;
extern "c" fn close(fd: c_int) c_int;
extern "c" fn mmap(addr: ?*anyopaque, length: usize, prot: c_int, flags: c_int, fd: c_int, offset: i64) ?*anyopaque;

// Platform-specific exe path
extern "c" fn _NSGetExecutablePath(buf: [*]u8, bufsize: *u32) c_int;
extern "c" fn readlink(path: [*:0]const u8, buf: [*]u8, bufsize: usize) isize;

// fstat for file size
const Stat = if (is_macos) extern struct {
    st_dev: i32,
    st_mode: u16,
    st_nlink: u16,
    st_ino: u64,
    st_uid: u32,
    st_gid: u32,
    st_rdev: i32,
    st_atimespec: [16]u8,
    st_mtimespec: [16]u8,
    st_ctimespec: [16]u8,
    st_birthtimespec: [16]u8,
    st_size: i64,
    // ... more fields, but we only need st_size
} else extern struct {
    st_dev: u64,
    st_ino: u64,
    st_nlink: u64,
    st_mode: u32,
    st_uid: u32,
    st_gid: u32,
    __pad0: c_int,
    st_rdev: u64,
    st_size: i64,
    // ... more fields
};

extern "c" fn fstat(fd: c_int, buf: *Stat) c_int;

// ============================================================================
// Utility functions
// ============================================================================

fn writeStderr(msg: []const u8) void {
    _ = write(2, msg.ptr, msg.len);
}

fn readU8(data: [*]const u8, off: usize) u8 {
    return data[off];
}

fn readU16(data: [*]const u8, off: usize) u16 {
    return @as(u16, data[off]) | (@as(u16, data[off + 1]) << 8);
}

fn readU32(data: [*]const u8, off: usize) u32 {
    return @as(u32, data[off]) |
        (@as(u32, data[off + 1]) << 8) |
        (@as(u32, data[off + 2]) << 16) |
        (@as(u32, data[off + 3]) << 24);
}

fn readU64(data: [*]const u8, off: usize) u64 {
    return @as(u64, readU32(data, off)) | (@as(u64, readU32(data, off + 4)) << 32);
}

fn readI8(data: [*]const u8, off: usize) i8 {
    return @bitCast(data[off]);
}

/// Read an unsigned LEB128 value. Returns (value, bytes_consumed).
fn readULEB128(data: [*]const u8, off: usize, limit: usize) struct { val: u64, len: usize } {
    var result: u64 = 0;
    var shift: u6 = 0;
    var i: usize = 0;
    while (off + i < limit) : (i += 1) {
        const byte = data[off + i];
        const payload: u64 = @intCast(byte & 0x7F);
        if (shift < 64) {
            result |= payload << shift;
        }
        if (byte & 0x80 == 0) {
            return .{ .val = result, .len = i + 1 };
        }
        shift +%= 7;
    }
    return .{ .val = result, .len = i };
}

/// Read a signed LEB128 value. Returns (value, bytes_consumed).
fn readSLEB128(data: [*]const u8, off: usize, limit: usize) struct { val: i64, len: usize } {
    var result: i64 = 0;
    var shift: u6 = 0;
    var i: usize = 0;
    var last_byte: u8 = 0;
    while (off + i < limit) : (i += 1) {
        last_byte = data[off + i];
        const payload: u64 = @intCast(last_byte & 0x7F);
        if (shift < 64) {
            result |= @bitCast(payload << shift);
        }
        if (last_byte & 0x80 == 0) {
            // Sign extend
            if (shift < 63 and (last_byte & 0x40) != 0) {
                const sign_bits: i64 = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFF) << (shift + 7));
                result |= sign_bits;
            }
            return .{ .val = result, .len = i + 1 };
        }
        shift +%= 7;
    }
    return .{ .val = result, .len = i };
}

/// Print a decimal number to a stack buffer and write to stderr.
fn printDecimal(n: i64) void {
    if (n < 0) {
        writeStderr("-");
        printDecimal(-n);
        return;
    }
    var buf: [20]u8 = undefined;
    var val: u64 = @intCast(n);
    var pos: usize = 20;
    if (val == 0) {
        writeStderr("0");
        return;
    }
    while (val > 0) {
        pos -= 1;
        buf[pos] = @intCast('0' + val % 10);
        val /= 10;
    }
    _ = write(2, @as([*]const u8, &buf) + pos, 20 - pos);
}

/// Print a null-terminated C string from mapped memory.
fn printCStr(data: [*]const u8, off: usize, limit: usize) void {
    var end = off;
    while (end < limit and data[end] != 0) : (end += 1) {}
    if (end > off) {
        _ = write(2, data + off, end - off);
    }
}

/// Measure length of a null-terminated string in mapped data.
fn cstrLen(data: [*]const u8, off: usize, limit: usize) usize {
    var end = off;
    while (end < limit and data[end] != 0) : (end += 1) {}
    return end - off;
}

/// Compare a segment/section name from a fixed-length buffer with a string literal.
fn nameEql(buf: []const u8, expected: []const u8) bool {
    if (buf.len < expected.len) return false;
    for (expected, 0..) |c, i| {
        if (buf[i] != c) return false;
    }
    // Rest must be null padding
    for (expected.len..buf.len) |i| {
        if (buf[i] != 0) return false;
    }
    return true;
}

// ============================================================================
// Platform binary initialization
// ============================================================================

fn initBinary() bool {
    // Get executable path
    var path_buf: [1024]u8 = undefined;
    var path_len: usize = 0;

    if (is_macos) {
        var bufsize: u32 = @intCast(path_buf.len);
        if (_NSGetExecutablePath(&path_buf, &bufsize) != 0) return false;
        // Find null terminator
        path_len = 0;
        while (path_len < path_buf.len and path_buf[path_len] != 0) : (path_len += 1) {}
    } else if (is_linux) {
        const n = readlink("/proc/self/exe", &path_buf, path_buf.len);
        if (n <= 0) return false;
        path_len = @intCast(n);
    } else {
        return false;
    }

    // Null-terminate
    if (path_len >= path_buf.len) return false;
    path_buf[path_len] = 0;

    // Open and stat
    const fd = open(@ptrCast(&path_buf), 0); // O_RDONLY = 0
    if (fd < 0) return false;

    var st: Stat = undefined;
    if (fstat(fd, &st) != 0) {
        _ = close(fd);
        return false;
    }
    const file_size: usize = @intCast(st.st_size);

    // mmap the entire binary
    const result = mmap(null, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
    _ = close(fd);

    if (result == null or @intFromPtr(result.?) == MAP_FAILED) return false;

    g_binary_base = @ptrCast(result.?);
    g_binary_len = file_size;

    // Parse headers to find .debug_line
    if (is_macos) {
        return parseMachO();
    } else if (is_linux) {
        return parseELF();
    }
    return false;
}

fn parseMachO() bool {
    if (g_binary_len < @sizeOf(MachHeader64)) return false;

    const magic = readU32(g_binary_base, 0);
    if (magic != MH_MAGIC_64) return false;

    const ncmds = readU32(g_binary_base, 16); // offset of ncmds in mach_header_64
    var cmd_offset: usize = @sizeOf(MachHeader64); // 32 bytes

    for (0..ncmds) |_| {
        if (cmd_offset + 8 > g_binary_len) break;

        const cmd = readU32(g_binary_base, cmd_offset);
        const cmdsize = readU32(g_binary_base, cmd_offset + 4);

        if (cmd == LC_SEGMENT_64) {
            if (cmd_offset + @sizeOf(SegmentCommand64) > g_binary_len) break;

            // Check segment name: "__DWARF"
            const segname_off = cmd_offset + 8; // offset of segname in segment_command_64
            if (nameEql(g_binary_base[segname_off .. segname_off + 16], "__DWARF")) {
                const nsects = readU32(g_binary_base, cmd_offset + 64); // offset of nsects
                var sect_offset = cmd_offset + @sizeOf(SegmentCommand64); // 72 bytes

                for (0..nsects) |_| {
                    if (sect_offset + @sizeOf(Section64) > g_binary_len) break;

                    // Check section name: "__debug_line"
                    const sectname_off = sect_offset; // sectname is first field
                    if (nameEql(g_binary_base[sectname_off .. sectname_off + 16], "__debug_line")) {
                        g_debug_line_offset = @intCast(readU32(g_binary_base, sect_offset + 48)); // offset field
                        g_debug_line_size = @intCast(readU64(g_binary_base, sect_offset + 32)); // size field
                        return true;
                    }
                    sect_offset += @sizeOf(Section64); // 80 bytes
                }
            }
        }

        cmd_offset += cmdsize;
    }

    return false;
}

fn parseELF() bool {
    if (g_binary_len < @sizeOf(Elf64Header)) return false;

    // Check ELF magic
    if (g_binary_base[0] != ELF_MAGIC[0] or g_binary_base[1] != ELF_MAGIC[1] or
        g_binary_base[2] != ELF_MAGIC[2] or g_binary_base[3] != ELF_MAGIC[3])
    {
        return false;
    }

    // Must be 64-bit
    if (g_binary_base[4] != ELFCLASS64) return false;

    const e_shoff = readU64(g_binary_base, 40); // offset of e_shoff
    const e_shentsize = readU16(g_binary_base, 58); // offset of e_shentsize
    const e_shnum = readU16(g_binary_base, 60); // offset of e_shnum
    const e_shstrndx = readU16(g_binary_base, 62); // offset of e_shstrndx

    if (e_shoff == 0 or e_shnum == 0) return false;

    // Get section header string table
    const shstrtab_off = e_shoff + @as(u64, e_shstrndx) * @as(u64, e_shentsize);
    if (shstrtab_off + @sizeOf(Elf64SectionHeader) > g_binary_len) return false;
    const shstrtab_offset = readU64(g_binary_base, @intCast(shstrtab_off + 24)); // sh_offset
    const shstrtab_size = readU64(g_binary_base, @intCast(shstrtab_off + 32)); // sh_size

    // Scan section headers for .debug_line
    for (0..e_shnum) |i| {
        const sh_off = e_shoff + @as(u64, @intCast(i)) * @as(u64, e_shentsize);
        if (sh_off + @sizeOf(Elf64SectionHeader) > g_binary_len) break;

        const sh_name_idx = readU32(g_binary_base, @intCast(sh_off));
        const sh_type = readU32(g_binary_base, @intCast(sh_off + 4));

        if (sh_type != SHT_PROGBITS) continue;

        // Check name
        const name_off = shstrtab_offset + @as(u64, sh_name_idx);
        if (name_off + 12 > shstrtab_offset + shstrtab_size) continue;

        // Compare ".debug_line"
        const name_ptr = @as(usize, @intCast(name_off));
        if (name_ptr + 11 >= g_binary_len) continue;

        const expected = ".debug_line";
        var match = true;
        for (expected, 0..) |c, j| {
            if (g_binary_base[name_ptr + j] != c) {
                match = false;
                break;
            }
        }
        // Must be exactly ".debug_line" not ".debug_line_str" etc.
        if (match and g_binary_base[name_ptr + 11] == 0) {
            g_debug_line_offset = @intCast(readU64(g_binary_base, @intCast(sh_off + 24))); // sh_offset
            g_debug_line_size = @intCast(readU64(g_binary_base, @intCast(sh_off + 32))); // sh_size
            return true;
        }
    }

    return false;
}

// ============================================================================
// DWARF .debug_line parser
// Reference: Zig lib/std/debug/Dwarf.zig lines 950-1192
// ============================================================================

/// Parse a DWARF v4 line program header.
/// Returns the header info and positions within the mapped binary.
fn parseLineProgramHeader(base_offset: usize) ?LineProgramHeader {
    const data = g_binary_base;
    const limit = g_debug_line_offset + g_debug_line_size;
    var off = base_offset;

    if (off + 4 > limit) return null;

    var hdr: LineProgramHeader = .{};

    // Unit length (4 bytes for 32-bit DWARF, could be 0xFFFFFFFF for 64-bit)
    const initial_length = readU32(data, off);
    off += 4;

    if (initial_length == 0xFFFFFFFF) {
        // 64-bit DWARF — read 8-byte length
        if (off + 8 > limit) return null;
        hdr.unit_length = readU64(data, off);
        off += 8;
    } else {
        hdr.unit_length = initial_length;
    }

    const unit_end = off + @as(usize, @intCast(hdr.unit_length));
    if (unit_end > limit) return null;

    // Version
    if (off + 2 > unit_end) return null;
    hdr.version = readU16(data, off);
    off += 2;

    // Only support DWARF v2-v5 line programs
    if (hdr.version < 2 or hdr.version > 5) return null;

    // DWARF v5: address_size, segment_selector_size before header_length
    if (hdr.version >= 5) {
        if (off + 2 > unit_end) return null;
        // address_size (1 byte) + segment_selector_size (1 byte)
        off += 2;
    }

    // Header length
    if (off + 4 > unit_end) return null;
    if (initial_length == 0xFFFFFFFF) {
        hdr.header_length = readU64(data, off);
        off += 8;
    } else {
        hdr.header_length = readU32(data, off);
        off += 4;
    }

    const program_start = off + @as(usize, @intCast(hdr.header_length));

    // Minimum instruction length
    if (off + 1 > unit_end) return null;
    hdr.min_instr_length = readU8(data, off);
    off += 1;

    // Maximum operations per instruction (DWARF v4+)
    if (hdr.version >= 4) {
        if (off + 1 > unit_end) return null;
        hdr.max_ops_per_instr = readU8(data, off);
        off += 1;
    }

    // Default is_stmt
    if (off + 1 > unit_end) return null;
    hdr.default_is_stmt = readU8(data, off) != 0;
    off += 1;

    // line_base (signed)
    if (off + 1 > unit_end) return null;
    hdr.line_base = readI8(data, off);
    off += 1;

    // line_range
    if (off + 1 > unit_end) return null;
    hdr.line_range = readU8(data, off);
    off += 1;

    // opcode_base
    if (off + 1 > unit_end) return null;
    hdr.opcode_base = readU8(data, off);
    off += 1;

    // Standard opcode lengths (opcode_base - 1 entries)
    if (hdr.opcode_base > 1) {
        const num_std = @as(usize, hdr.opcode_base) - 1;
        if (off + num_std > unit_end) return null;
        off += num_std;
    }

    // DWARF v5 uses different directory/file table format
    if (hdr.version >= 5) {
        // Directory entry format count
        if (off + 1 > unit_end) return null;
        const dir_entry_format_count = readU8(data, off);
        off += 1;
        // Skip directory entry formats (pairs of ULEB128)
        for (0..dir_entry_format_count) |_| {
            const r1 = readULEB128(data, off, unit_end);
            off += r1.len;
            const r2 = readULEB128(data, off, unit_end);
            off += r2.len;
        }
        // Directory count
        const dir_count_r = readULEB128(data, off, unit_end);
        off += dir_count_r.len;
        // Skip directory entries — each has dir_entry_format_count fields
        hdr.dirs_start = off;
        for (0..@intCast(dir_count_r.val)) |_| {
            for (0..dir_entry_format_count) |_| {
                // Each field is a form, skip based on form type
                // For simplicity, scan for null-terminated strings (DW_FORM_string)
                // or ULEB128 values. This is a simplification.
                // Skip null-terminated string
                while (off < unit_end and data[off] != 0) : (off += 1) {}
                if (off < unit_end) off += 1; // skip null
            }
        }
        hdr.dirs_end = off;

        // File name entry format count
        if (off + 1 > unit_end) return null;
        const file_entry_format_count = readU8(data, off);
        off += 1;
        // Skip file entry formats
        for (0..file_entry_format_count) |_| {
            const r1 = readULEB128(data, off, unit_end);
            off += r1.len;
            const r2 = readULEB128(data, off, unit_end);
            off += r2.len;
        }
        // File count
        const file_count_r = readULEB128(data, off, unit_end);
        off += file_count_r.len;
        hdr.files_start = off;
        // Skip file entries
        for (0..@intCast(file_count_r.val)) |_| {
            for (0..file_entry_format_count) |_| {
                while (off < unit_end and data[off] != 0) : (off += 1) {}
                if (off < unit_end) off += 1;
            }
        }
        hdr.files_end = off;
    } else {
        // DWARF v2-v4: directory and file tables are null-terminated string sequences

        // Include directories: sequence of null-terminated strings, terminated by empty string
        hdr.dirs_start = off;
        while (off < unit_end and data[off] != 0) {
            // Skip directory string
            while (off < unit_end and data[off] != 0) : (off += 1) {}
            if (off < unit_end) off += 1; // skip null terminator
        }
        hdr.dirs_end = off;
        if (off < unit_end) off += 1; // skip final null byte

        // File names: sequence of entries, terminated by empty string
        // Each entry: name (null-terminated), dir_index (ULEB128), time (ULEB128), size (ULEB128)
        hdr.files_start = off;
        while (off < unit_end and data[off] != 0) {
            // Skip filename string
            while (off < unit_end and data[off] != 0) : (off += 1) {}
            if (off < unit_end) off += 1; // skip null
            // Skip dir_index, time, size (ULEB128 each)
            const r1 = readULEB128(data, off, unit_end);
            off += r1.len;
            const r2 = readULEB128(data, off, unit_end);
            off += r2.len;
            const r3 = readULEB128(data, off, unit_end);
            off += r3.len;
        }
        hdr.files_end = off;
        if (off < unit_end) off += 1; // skip final null byte
    }

    hdr.program_start = program_start;
    hdr.program_end = unit_end;

    return hdr;
}

/// Given a file index from the line program, print the filename.
/// DWARF v2-v4: file indices are 1-based, file table starts at hdr.files_start.
fn printFileFromTable(hdr: *const LineProgramHeader, file_idx: u32) void {
    if (file_idx == 0) {
        writeStderr("<unknown>");
        return;
    }

    const data = g_binary_base;
    var off = hdr.files_start;
    const limit = hdr.files_end;

    // Walk to the file_idx-th entry (1-based)
    var idx: u32 = 1;
    while (off < limit and data[off] != 0) {
        if (idx == file_idx) {
            // Print the filename (null-terminated string at off)
            printCStr(data, off, limit);
            return;
        }
        // Skip filename
        while (off < limit and data[off] != 0) : (off += 1) {}
        if (off < limit) off += 1;
        // Skip dir_index, time, size
        if (hdr.version < 5) {
            const r1 = readULEB128(data, off, limit);
            off += r1.len;
            const r2 = readULEB128(data, off, limit);
            off += r2.len;
            const r3 = readULEB128(data, off, limit);
            off += r3.len;
        }
        idx += 1;
    }

    writeStderr("<unknown>");
}

/// Execute the line number program state machine for a single compilation unit.
/// Returns true if the target PC was found and printed.
/// Reference: Zig Dwarf.zig lines 1091-1192
fn executeLineProgram(hdr: *const LineProgramHeader, target_pc: u64) bool {
    const data = g_binary_base;
    var off = hdr.program_start;
    const end = hdr.program_end;

    var state: LineState = .{
        .is_stmt = hdr.default_is_stmt,
    };

    // Best match tracking: closest address <= target_pc
    var best_file: u32 = 0;
    var best_line: i64 = 0;
    var best_column: u32 = 0;
    var best_addr: u64 = 0;
    var found_any = false;

    while (off < end) {
        const opcode = data[off];
        off += 1;

        if (opcode == 0) {
            // Extended opcode
            const ext_len_r = readULEB128(data, off, end);
            off += ext_len_r.len;
            const ext_len = @as(usize, @intCast(ext_len_r.val));
            if (ext_len == 0 or off >= end) continue;
            const ext_off = off;

            const ext_opcode = data[off];
            off += 1;

            switch (ext_opcode) {
                DW_LNE_end_sequence => {
                    // Emit row, then check if we passed the target
                    if (state.address <= target_pc and state.address > best_addr and !state.end_sequence) {
                        best_addr = state.address;
                        best_file = state.file;
                        best_line = state.line;
                        best_column = state.column;
                        found_any = true;
                    }
                    state.end_sequence = true;
                    // If this sequence contains the target PC, we're done after it
                    if (found_any and best_addr <= target_pc) {
                        // This is potentially the right answer, but keep looking
                        // in case a later sequence has a closer match
                    }
                    // Reset state
                    state = .{ .is_stmt = hdr.default_is_stmt };
                },
                DW_LNE_set_address => {
                    // Read address (8 bytes for 64-bit)
                    if (off + 8 <= end) {
                        state.address = readU64(data, off);
                    }
                    off = ext_off + ext_len;
                },
                DW_LNE_define_file => {
                    // Skip inline file definition: name + 3 ULEB128s
                    off = ext_off + ext_len;
                },
                else => {
                    // Unknown extended opcode — skip
                    off = ext_off + ext_len;
                },
            }
            // Ensure we advance past the extended opcode data
            if (off < ext_off + ext_len) {
                off = ext_off + ext_len;
            }
        } else if (opcode < hdr.opcode_base) {
            // Standard opcode
            switch (opcode) {
                DW_LNS_copy => {
                    // Emit row
                    if (state.address <= target_pc and state.address >= best_addr) {
                        best_addr = state.address;
                        best_file = state.file;
                        best_line = state.line;
                        best_column = state.column;
                        found_any = true;
                    }
                    state.basic_block = false;
                    state.prologue_end = false;
                    state.epilogue_begin = false;
                },
                DW_LNS_advance_pc => {
                    const operand = readULEB128(data, off, end);
                    off += operand.len;
                    state.address += operand.val * hdr.min_instr_length;
                },
                DW_LNS_advance_line => {
                    const operand = readSLEB128(data, off, end);
                    off += operand.len;
                    state.line += operand.val;
                },
                DW_LNS_set_file => {
                    const operand = readULEB128(data, off, end);
                    off += operand.len;
                    state.file = @intCast(operand.val);
                },
                DW_LNS_set_column => {
                    const operand = readULEB128(data, off, end);
                    off += operand.len;
                    state.column = @intCast(operand.val);
                },
                DW_LNS_negate_stmt => {
                    state.is_stmt = !state.is_stmt;
                },
                DW_LNS_set_basic_block => {
                    state.basic_block = true;
                },
                DW_LNS_const_add_pc => {
                    // Advance address by the amount of a special opcode 255
                    const adjusted: u64 = 255 - @as(u64, hdr.opcode_base);
                    state.address += hdr.min_instr_length * (adjusted / hdr.line_range);
                },
                DW_LNS_fixed_advance_pc => {
                    if (off + 2 <= end) {
                        state.address += readU16(data, off);
                    }
                    off += 2;
                },
                DW_LNS_set_prologue_end => {
                    state.prologue_end = true;
                },
                DW_LNS_set_epilogue_begin => {
                    state.epilogue_begin = true;
                },
                DW_LNS_set_isa => {
                    const operand = readULEB128(data, off, end);
                    off += operand.len;
                    state.isa = @intCast(operand.val);
                },
                else => {
                    // Unknown standard opcode — skip operands
                    // This shouldn't happen with well-formed DWARF, but be safe
                    break;
                },
            }
        } else {
            // Special opcode (>= opcode_base)
            // Reference: Zig Dwarf.zig lines 1096-1107
            const adjusted: u64 = @as(u64, opcode) - @as(u64, hdr.opcode_base);
            const addr_advance = hdr.min_instr_length * (adjusted / hdr.line_range);
            const line_advance = @as(i64, hdr.line_base) + @as(i64, @intCast(adjusted % hdr.line_range));

            state.address += addr_advance;
            state.line += line_advance;

            // Emit row (special opcodes always emit)
            if (state.address <= target_pc and state.address >= best_addr) {
                best_addr = state.address;
                best_file = state.file;
                best_line = state.line;
                best_column = state.column;
                found_any = true;
            }

            state.basic_block = false;
            state.prologue_end = false;
            state.epilogue_begin = false;
        }

        // Early exit: if we've gone past the target PC address in the line program,
        // and we have a match, stop scanning this CU
        if (state.address > target_pc and found_any) {
            break;
        }
    }

    if (found_any) {
        // Print "  at <filename>:<line>:<column>\n"
        writeStderr("  at ");
        printFileFromTable(hdr, best_file);
        writeStderr(":");
        printDecimal(best_line);
        if (best_column > 0) {
            writeStderr(":");
            printDecimal(@intCast(best_column));
        }
        writeStderr("\n");
        return true;
    }

    return false;
}

// ============================================================================
// ASLR slide detection
// ============================================================================

/// On macOS, ASLR slides the binary in memory. We need to know the slide
/// to convert a runtime PC back to the file address used in DWARF.
///
/// The slide = actual load address - intended load address (from __TEXT vmaddr).
/// We detect this by looking at _NSGetMachExecuteHeader or by reading __TEXT vmaddr.
fn computeSlide() void {
    if (!is_macos) {
        g_load_addr_offset = 0;
        return;
    }

    // On macOS, the __TEXT segment's vmaddr in the file tells us the intended
    // load address. The actual load address comes from the Mach-O header in memory.
    // For position-dependent executables, vmaddr is typically 0x100000000.
    // For PIE executables, the kernel slides the binary, so:
    //   slide = actual_load_address - vmaddr
    //   file_pc = runtime_pc - slide

    // Find __TEXT vmaddr from the file
    const data = g_binary_base;
    const ncmds = readU32(data, 16);
    var cmd_offset: usize = @sizeOf(MachHeader64);

    for (0..ncmds) |_| {
        if (cmd_offset + 8 > g_binary_len) break;
        const cmd = readU32(data, cmd_offset);
        const cmdsize = readU32(data, cmd_offset + 4);

        if (cmd == LC_SEGMENT_64) {
            const segname_off = cmd_offset + 8;
            if (nameEql(data[segname_off .. segname_off + 16], "__TEXT")) {
                const text_vmaddr = readU64(data, cmd_offset + 24);
                // The slide will be computed at resolve time by comparing
                // the runtime PC range with the DWARF address range.
                // For now, store the intended text vmaddr.
                // We'll use a simpler approach: try the PC as-is first,
                // then try subtracting the typical PIE slide.
                g_load_addr_offset = @bitCast(text_vmaddr);
                return;
            }
        }
        cmd_offset += cmdsize;
    }
}

// ============================================================================
// Exported entry point
// ============================================================================

/// Resolve a program counter to file:line:column using DWARF .debug_line.
/// If DWARF sections are not present (release build), returns silently.
/// Signal-safe: uses mmap and stack buffers only.
export fn __cot_dwarf_resolve(pc: usize) void {
    // Lazy init
    if (!g_initialized) {
        g_initialized = true;
        if (!initBinary()) {
            // No binary loaded or no .debug_line — silent return
            return;
        }
        computeSlide();
    }

    if (g_debug_line_size == 0) return;

    const target_pc: u64 = @intCast(pc);

    // Try resolving with the raw PC first. If we're a non-PIE binary or
    // the DWARF addresses match runtime addresses, this works directly.
    if (tryResolve(target_pc)) return;

    // If the raw PC didn't match, try adjusting for ASLR slide.
    // On macOS with PIE, runtime_PC = file_PC + slide.
    // slide = runtime __TEXT load addr - file __TEXT vmaddr.
    // We stored file __TEXT vmaddr in g_load_addr_offset.
    // The runtime __TEXT load addr can be inferred: if the PC is in the
    // typical macOS user-space range (0x100000000+), the slide is usually
    // the difference between the actual load and the file vmaddr.
    //
    // Heuristic: if file vmaddr is 0x100000000 (standard macOS main binary),
    // the kernel may slide it. Try PC - slide for various possible slides.
    // A better approach: use _dyld_get_image_vmaddr_slide(0) but that
    // requires linking libdyld, which we want to avoid.
    //
    // For Cot binaries (linked with -pie), the typical slide is applied by
    // the kernel. We detect it by checking if any line program's address
    // range could contain the PC after adjustment.

    // Don't try ASLR adjustment on Linux — addresses match in non-PIE binaries
    // and PIE on Linux uses a different mechanism
}

fn tryResolve(target_pc: u64) bool {
    // Scan all compilation units in .debug_line
    var cu_offset = g_debug_line_offset;
    const section_end = g_debug_line_offset + g_debug_line_size;

    while (cu_offset < section_end) {
        if (parseLineProgramHeader(cu_offset)) |hdr| {
            if (executeLineProgram(&hdr, target_pc)) {
                return true;
            }
            // Advance to next CU
            // Unit length field is 4 bytes (32-bit DWARF) or 12 bytes (64-bit DWARF)
            const initial_length = readU32(g_binary_base, cu_offset);
            if (initial_length == 0xFFFFFFFF) {
                cu_offset += 12 + @as(usize, @intCast(hdr.unit_length)); // 4 + 8 + unit_length
            } else {
                cu_offset += 4 + @as(usize, @intCast(hdr.unit_length)); // 4 + unit_length
            }
        } else {
            break;
        }
    }

    return false;
}
