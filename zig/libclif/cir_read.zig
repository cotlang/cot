//! CIR binary format reader (Zig implementation).
//!
//! Reads the CIR binary format produced by cir_write.zig and returns
//! parsed structures that can be translated to the hand-ported CLIF IR.
//! This is the Zig equivalent of rust/libclif/src/cir.rs.

const std = @import("std");

// CIR constants (must match cir_write.zig and rust/libclif/src/cir.rs)
pub const MAGIC: u32 = 0x00434952;
pub const SECTION_STRING_HEAP: u16 = 0x01;
pub const SECTION_FUNC_DEFS: u16 = 0x06;

pub const CirModule = struct {
    string_heap: []const u8,
    functions: []CirFunction,
    bound: u32,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CirModule) void {
        for (self.functions) |*f| {
            for (f.blocks) |*b| {
                for (b.instructions) |*inst| {
                    self.allocator.free(inst.words);
                }
                self.allocator.free(b.instructions);
            }
            self.allocator.free(f.blocks);
            self.allocator.free(f.param_types);
            self.allocator.free(f.return_types);
        }
        self.allocator.free(self.functions);
    }

    pub fn getString(self: *const CirModule, offset: u32) []const u8 {
        const off = offset;
        if (off + 4 > self.string_heap.len) return "";
        const len = std.mem.readInt(u32, self.string_heap[off..][0..4], .little);
        const start = off + 4;
        const end = start + len;
        if (end > self.string_heap.len) return "";
        return self.string_heap[start..end];
    }
};

pub const CirFunction = struct {
    name_offset: u32,
    param_count: u32,
    return_count: u32,
    flags: u32,
    param_types: []u32,
    return_types: []u32,
    blocks: []CirBlock,
};

pub const CirBlock = struct {
    id: u32,
    kind: u8,
    instructions: []CirInst,
};

pub const CirInst = struct {
    opcode: u16,
    words: []u32,
};

pub fn readModule(allocator: std.mem.Allocator, data: []const u8) !CirModule {
    var pos: usize = 0;

    // Header
    const magic = readU32(data, &pos);
    if (magic != MAGIC) return error.BadMagic;
    _ = readU32(data, &pos); // version
    _ = readU32(data, &pos); // generator
    const bound = readU32(data, &pos);
    _ = readU32(data, &pos); // reserved

    var string_heap: []const u8 = &.{};
    var functions = std.ArrayListUnmanaged(CirFunction){};

    // Read sections
    while (pos < data.len) {
        const section_word = readU32(data, &pos);
        const section_wc = section_word >> 16;
        const section_id: u16 = @truncate(section_word & 0xFFFF);
        const section_start = pos;
        const section_byte_len = (@as(usize, section_wc) -| 1) * 4;

        switch (section_id) {
            SECTION_STRING_HEAP => {
                const byte_count = readU32(data, &pos);
                string_heap = data[pos..][0..byte_count];
                pos += ((byte_count + 3) / 4) * 4;
            },
            SECTION_FUNC_DEFS => {
                const end = if (section_wc > 0) section_start + section_byte_len else data.len;
                while (pos < end) {
                    const header = readU32(data, &pos);
                    const opcode: u16 = @truncate(header & 0xFFFF);
                    if (opcode == 0xFF00) { // FUNC_BEGIN
                        const name_offset = readU32(data, &pos);
                        const param_count = readU32(data, &pos);
                        const return_count = readU32(data, &pos);
                        _ = readU32(data, &pos); // block_count
                        const flags = readU32(data, &pos);

                        const param_types = try allocator.alloc(u32, param_count);
                        for (0..param_count) |i| param_types[i] = readU32(data, &pos);
                        const return_types = try allocator.alloc(u32, return_count);
                        for (0..return_count) |i| return_types[i] = readU32(data, &pos);

                        // Read blocks until FUNC_END
                        var blocks = std.ArrayListUnmanaged(CirBlock){};
                        while (pos < end) {
                            const bh = readU32(data, &pos);
                            const bop: u16 = @truncate(bh & 0xFFFF);
                            if (bop == 0xFF01) break; // FUNC_END
                            if (bop == 0xFF02) { // BLOCK_BEGIN
                                const id = readU32(data, &pos);
                                const kind: u8 = @truncate(readU32(data, &pos));
                                const succ_count = readU32(data, &pos);
                                for (0..succ_count) |_| _ = readU32(data, &pos);
                                const pred_count = readU32(data, &pos);
                                for (0..pred_count) |_| _ = readU32(data, &pos);

                                // Read instructions until BLOCK_END or next block/func marker
                                var insts = std.ArrayListUnmanaged(CirInst){};
                                while (pos < end) {
                                    const peek = std.mem.readInt(u32, data[pos..][0..4], .little);
                                    const peek_op: u16 = @truncate(peek & 0xFFFF);
                                    if (peek_op == 0xFF03 or peek_op == 0xFF02 or peek_op == 0xFF01) {
                                        if (peek_op == 0xFF03) pos += 4; // consume BLOCK_END
                                        break;
                                    }
                                    pos += 4;
                                    const wc = peek >> 16;
                                    const operand_count = @as(usize, wc) -| 1;
                                    const words = try allocator.alloc(u32, operand_count);
                                    for (0..operand_count) |i| words[i] = readU32(data, &pos);
                                    try insts.append(allocator, .{ .opcode = peek_op, .words = words });
                                }
                                try blocks.append(allocator, .{
                                    .id = id,
                                    .kind = kind,
                                    .instructions = try insts.toOwnedSlice(allocator),
                                });
                            }
                        }

                        try functions.append(allocator, .{
                            .name_offset = name_offset,
                            .param_count = param_count,
                            .return_count = return_count,
                            .flags = flags,
                            .param_types = param_types,
                            .return_types = return_types,
                            .blocks = try blocks.toOwnedSlice(allocator),
                        });
                    }
                }
            },
            else => {
                if (section_wc > 0) pos = section_start + section_byte_len;
            },
        }
    }

    return .{
        .string_heap = string_heap,
        .functions = try functions.toOwnedSlice(allocator),
        .bound = bound,
        .allocator = allocator,
    };
}

fn readU32(data: []const u8, pos: *usize) u32 {
    if (pos.* + 4 > data.len) return 0;
    const val = std.mem.readInt(u32, data[pos.*..][0..4], .little);
    pos.* += 4;
    return val;
}
