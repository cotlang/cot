//! IR Binary Serializer
//!
//! Serializes Cot IR to a compact binary format for passing to the Cranelift JIT.
//! The format is designed for efficient parsing on the Rust side.
//!
//! Binary Format:
//! ```
//! Header:
//!   magic: u32 = 0x434F5449 ("COTI")
//!   version: u16
//!   function_count: u16
//!
//! Function:
//!   name_len: u16
//!   name: [u8; name_len]
//!   param_count: u8
//!   params: [Param; param_count]
//!   return_type: TypeId
//!   block_count: u16
//!   blocks: [Block; block_count]
//!
//! Block:
//!   label_id: u16 (index)
//!   instruction_count: u16
//!   instructions: [Instruction; instruction_count]
//! ```

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/ir.zig");

pub const MAGIC: u32 = 0x434F5449; // "COTI"
pub const VERSION: u16 = 1;

/// Opcode values for the binary format
pub const Opcode = enum(u8) {
    // Memory
    alloca = 0x01,
    load = 0x02,
    store = 0x03,
    field_ptr = 0x04,

    // Arithmetic
    iadd = 0x10,
    isub = 0x11,
    imul = 0x12,
    sdiv = 0x13,
    udiv = 0x14,
    srem = 0x15,
    urem = 0x16,
    ineg = 0x17,

    // Comparison
    icmp = 0x20,

    // Bitwise
    band = 0x30,
    bor = 0x31,
    bxor = 0x32,
    bnot = 0x33,
    ishl = 0x34,
    sshr = 0x35,
    ushr = 0x36,

    // Logical
    log_and = 0x38,
    log_or = 0x39,
    log_not = 0x3A,

    // Control flow
    jump = 0x40,
    brif = 0x41,
    br_table = 0x42,
    return_ = 0x43,
    trap = 0x44,

    // Calls
    call = 0x50,
    call_indirect = 0x51,
    runtime_call = 0x52, // Call to Zig runtime function

    // Constants
    iconst = 0x60,
    f32const = 0x61,
    f64const = 0x62,
    const_string = 0x63,
    const_null = 0x64,

    // Type conversions
    bitcast = 0x70,
    fcvt_from_sint = 0x71,
    fcvt_from_uint = 0x72,
    fcvt_to_sint = 0x73,
    fcvt_to_uint = 0x74,
    sextend = 0x75,
    uextend = 0x76,
    ireduce = 0x77,

    // String (runtime calls)
    str_concat = 0x80,
    str_compare = 0x81,
    str_copy = 0x82,
    str_slice = 0x83,
    str_len = 0x84,

    // Array
    array_load = 0x90,
    array_store = 0x91,
    array_len = 0x92,

    // Optional
    wrap_optional = 0xA0,
    unwrap_optional = 0xA1,
    is_null = 0xA2,

    // I/O (runtime calls)
    io_open = 0xB0,
    io_close = 0xB1,
    io_read = 0xB2,
    io_write = 0xB3,
    io_delete = 0xB4,
    io_unlock = 0xB5,

    // Exception handling
    try_begin = 0xC0,
    try_end = 0xC1,
    catch_begin = 0xC2,
    throw = 0xC3,

    // Debug
    debug_line = 0xF0,
};

/// Type encoding for the binary format
pub const TypeId = enum(u8) {
    void = 0x00,
    bool = 0x01,
    i8 = 0x02,
    i16 = 0x03,
    i32 = 0x04,
    i64 = 0x05,
    u8 = 0x06,
    u16 = 0x07,
    u32 = 0x08,
    u64 = 0x09,
    f32 = 0x0A,
    f64 = 0x0B,
    string = 0x0C,
    string_fixed = 0x0D, // followed by u32 length
    decimal = 0x0E, // followed by u8 precision, u8 scale
    ptr = 0x0F, // followed by TypeId
    optional = 0x10, // followed by TypeId
    array = 0x11, // followed by TypeId, u32 length
    slice = 0x12, // followed by TypeId
    @"struct" = 0x13, // followed by u16 name_len, name
    function = 0x14,
};

/// Condition code encoding
pub const CondCode = enum(u8) {
    eq = 0,
    ne = 1,
    slt = 2,
    sge = 3,
    sgt = 4,
    sle = 5,
    ult = 6,
    uge = 7,
    ugt = 8,
    ule = 9,
};

/// IR Serializer
pub const IrSerializer = struct {
    allocator: Allocator,
    buffer: std.ArrayList(u8),
    block_indices: std.AutoHashMap(*const ir.Block, u16),
    string_pool: std.StringHashMap(u16),
    next_string_id: u16,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .buffer = std.ArrayList(u8).init(allocator),
            .block_indices = std.AutoHashMap(*const ir.Block, u16).init(allocator),
            .string_pool = std.StringHashMap(u16).init(allocator),
            .next_string_id = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
        self.block_indices.deinit();
        self.string_pool.deinit();
    }

    /// Serialize an entire module to bytes
    pub fn serializeModule(self: *Self, module: *const ir.Module) ![]const u8 {
        self.buffer.clearRetainingCapacity();
        self.block_indices.clearRetainingCapacity();
        self.string_pool.clearRetainingCapacity();
        self.next_string_id = 0;

        // Header
        try self.writeU32(MAGIC);
        try self.writeU16(VERSION);
        try self.writeU16(@intCast(module.functions.items.len));

        // Functions
        for (module.functions.items) |func| {
            try self.serializeFunction(func);
        }

        return self.buffer.items;
    }

    /// Serialize a single function
    fn serializeFunction(self: *Self, func: *const ir.Function) !void {
        // Build block index map
        self.block_indices.clearRetainingCapacity();
        for (func.blocks.items, 0..) |block, i| {
            try self.block_indices.put(block, @intCast(i));
        }

        // Function name
        try self.writeString(func.name);

        // Parameters
        try self.writeU8(@intCast(func.signature.params.len));
        for (func.signature.params) |param| {
            try self.writeString(param.name);
            try self.writeType(param.ty);
            try self.writeU8(@intFromEnum(param.direction));
        }

        // Return type
        try self.writeType(func.signature.return_type);

        // Blocks
        try self.writeU16(@intCast(func.blocks.items.len));
        for (func.blocks.items) |block| {
            try self.serializeBlock(block);
        }
    }

    /// Serialize a basic block
    fn serializeBlock(self: *Self, block: *const ir.Block) !void {
        try self.writeString(block.label);
        try self.writeU16(@intCast(block.instructions.items.len));

        for (block.instructions.items) |inst| {
            try self.serializeInstruction(inst);
        }
    }

    /// Serialize a single instruction
    fn serializeInstruction(self: *Self, inst: ir.Instruction) !void {
        switch (inst) {
            // Memory
            .alloca => |a| {
                try self.writeU8(@intFromEnum(Opcode.alloca));
                try self.writeType(a.ty);
                try self.writeString(a.name);
                try self.writeValue(a.result);
            },
            .load => |l| {
                try self.writeU8(@intFromEnum(Opcode.load));
                try self.writeValue(l.ptr);
                try self.writeValue(l.result);
            },
            .store => |s| {
                try self.writeU8(@intFromEnum(Opcode.store));
                try self.writeValue(s.ptr);
                try self.writeValue(s.value);
            },
            .field_ptr => |f| {
                try self.writeU8(@intFromEnum(Opcode.field_ptr));
                try self.writeValue(f.struct_ptr);
                try self.writeU32(f.field_index);
                try self.writeValue(f.result);
            },

            // Arithmetic
            .iadd => |op| try self.writeBinaryOp(Opcode.iadd, op),
            .isub => |op| try self.writeBinaryOp(Opcode.isub, op),
            .imul => |op| try self.writeBinaryOp(Opcode.imul, op),
            .sdiv => |op| try self.writeBinaryOp(Opcode.sdiv, op),
            .udiv => |op| try self.writeBinaryOp(Opcode.udiv, op),
            .srem => |op| try self.writeBinaryOp(Opcode.srem, op),
            .urem => |op| try self.writeBinaryOp(Opcode.urem, op),
            .ineg => |op| try self.writeUnaryOp(Opcode.ineg, op),

            // Comparison
            .icmp => |op| {
                try self.writeU8(@intFromEnum(Opcode.icmp));
                try self.writeU8(@intFromEnum(irCondToCondCode(op.cond)));
                try self.writeValue(op.lhs);
                try self.writeValue(op.rhs);
                try self.writeValue(op.result);
            },

            // Bitwise
            .band => |op| try self.writeBinaryOp(Opcode.band, op),
            .bor => |op| try self.writeBinaryOp(Opcode.bor, op),
            .bxor => |op| try self.writeBinaryOp(Opcode.bxor, op),
            .bnot => |op| try self.writeUnaryOp(Opcode.bnot, op),
            .ishl => |op| try self.writeBinaryOp(Opcode.ishl, op),
            .sshr => |op| try self.writeBinaryOp(Opcode.sshr, op),
            .ushr => |op| try self.writeBinaryOp(Opcode.ushr, op),

            // Logical
            .log_and => |op| try self.writeBinaryOp(Opcode.log_and, op),
            .log_or => |op| try self.writeBinaryOp(Opcode.log_or, op),
            .log_not => |op| try self.writeUnaryOp(Opcode.log_not, op),

            // Control flow
            .jump => |j| {
                try self.writeU8(@intFromEnum(Opcode.jump));
                try self.writeBlockRef(j.target);
            },
            .brif => |b| {
                try self.writeU8(@intFromEnum(Opcode.brif));
                try self.writeValue(b.condition);
                try self.writeBlockRef(b.then_block);
                try self.writeBlockRef(b.else_block);
            },
            .br_table => |s| {
                try self.writeU8(@intFromEnum(Opcode.br_table));
                try self.writeValue(s.value);
                try self.writeU16(@intCast(s.cases.len));
                for (s.cases) |case| {
                    try self.writeI64(case.value);
                    try self.writeBlockRef(case.target);
                }
                try self.writeBlockRef(s.default);
            },
            .return_ => |v| {
                try self.writeU8(@intFromEnum(Opcode.return_));
                if (v) |val| {
                    try self.writeU8(1);
                    try self.writeValue(val);
                } else {
                    try self.writeU8(0);
                }
            },
            .trap => |t| {
                try self.writeU8(@intFromEnum(Opcode.trap));
                try self.writeU8(@intFromEnum(t));
            },

            // Calls
            .call => |c| {
                try self.writeU8(@intFromEnum(Opcode.call));
                try self.writeString(c.callee);
                try self.writeU8(@intCast(c.args.len));
                for (c.args) |arg| {
                    try self.writeValue(arg);
                }
                if (c.result) |r| {
                    try self.writeU8(1);
                    try self.writeValue(r);
                } else {
                    try self.writeU8(0);
                }
            },
            .call_indirect => |c| {
                try self.writeU8(@intFromEnum(Opcode.call_indirect));
                try self.writeValue(c.callee);
                try self.writeU8(@intCast(c.args.len));
                for (c.args) |arg| {
                    try self.writeValue(arg);
                }
                if (c.result) |r| {
                    try self.writeU8(1);
                    try self.writeValue(r);
                } else {
                    try self.writeU8(0);
                }
            },

            // Constants
            .iconst => |c| {
                try self.writeU8(@intFromEnum(Opcode.iconst));
                try self.writeType(c.ty);
                try self.writeI64(c.value);
                try self.writeValue(c.result);
            },
            .f32const => |c| {
                try self.writeU8(@intFromEnum(Opcode.f32const));
                try self.writeF32(c.value);
                try self.writeValue(c.result);
            },
            .f64const => |c| {
                try self.writeU8(@intFromEnum(Opcode.f64const));
                try self.writeF64(c.value);
                try self.writeValue(c.result);
            },
            .const_string => |c| {
                try self.writeU8(@intFromEnum(Opcode.const_string));
                try self.writeString(c.value);
                try self.writeValue(c.result);
            },
            .const_null => |c| {
                try self.writeU8(@intFromEnum(Opcode.const_null));
                try self.writeType(c.ty);
                try self.writeValue(c.result);
            },

            // Type conversions
            .bitcast => |c| {
                try self.writeU8(@intFromEnum(Opcode.bitcast));
                try self.writeValue(c.operand);
                try self.writeType(c.target_type);
                try self.writeValue(c.result);
            },
            .fcvt_from_sint => |op| try self.writeUnaryOp(Opcode.fcvt_from_sint, op),
            .fcvt_from_uint => |op| try self.writeUnaryOp(Opcode.fcvt_from_uint, op),
            .fcvt_to_sint => |op| try self.writeUnaryOp(Opcode.fcvt_to_sint, op),
            .fcvt_to_uint => |op| try self.writeUnaryOp(Opcode.fcvt_to_uint, op),
            .sextend => |e| {
                try self.writeU8(@intFromEnum(Opcode.sextend));
                try self.writeValue(e.operand);
                try self.writeValue(e.result);
            },
            .uextend => |e| {
                try self.writeU8(@intFromEnum(Opcode.uextend));
                try self.writeValue(e.operand);
                try self.writeValue(e.result);
            },
            .ireduce => |op| try self.writeUnaryOp(Opcode.ireduce, op),

            // String operations - these will call runtime
            .str_concat => |op| try self.writeBinaryOp(Opcode.str_concat, op),
            .str_compare => |op| try self.writeBinaryOp(Opcode.str_compare, op),
            .str_copy => |s| {
                try self.writeU8(@intFromEnum(Opcode.str_copy));
                try self.writeValue(s.dest);
                try self.writeValue(s.src);
            },
            .str_slice => |s| {
                try self.writeU8(@intFromEnum(Opcode.str_slice));
                try self.writeValue(s.source);
                try self.writeValue(s.start);
                try self.writeValue(s.length_or_end);
                try self.writeU8(if (s.is_length) 1 else 0);
                try self.writeValue(s.result);
            },
            .str_len => |op| try self.writeUnaryOp(Opcode.str_len, op),

            // Array operations
            .array_load => |a| {
                try self.writeU8(@intFromEnum(Opcode.array_load));
                try self.writeValue(a.array_ptr);
                try self.writeValue(a.index);
                try self.writeValue(a.result);
            },
            .array_store => |a| {
                try self.writeU8(@intFromEnum(Opcode.array_store));
                try self.writeValue(a.array_ptr);
                try self.writeValue(a.index);
                try self.writeValue(a.value);
            },
            .array_len => |op| try self.writeUnaryOp(Opcode.array_len, op),

            // Optional
            .wrap_optional => |op| try self.writeUnaryOp(Opcode.wrap_optional, op),
            .unwrap_optional => |op| try self.writeUnaryOp(Opcode.unwrap_optional, op),
            .is_null => |op| try self.writeUnaryOp(Opcode.is_null, op),

            // I/O - will call runtime
            .io_open => |o| {
                try self.writeU8(@intFromEnum(Opcode.io_open));
                try self.writeValue(o.channel);
                try self.writeU8(@intFromEnum(o.mode));
                try self.writeValue(o.filename);
            },
            .io_close => |c| {
                try self.writeU8(@intFromEnum(Opcode.io_close));
                try self.writeValue(c.channel);
            },
            .io_read => |r| {
                try self.writeU8(@intFromEnum(Opcode.io_read));
                try self.writeValue(r.channel);
                try self.writeValue(r.buffer);
                if (r.key) |k| {
                    try self.writeU8(1);
                    try self.writeValue(k);
                } else {
                    try self.writeU8(0);
                }
            },
            .io_write => |w| {
                try self.writeU8(@intFromEnum(Opcode.io_write));
                try self.writeValue(w.channel);
                try self.writeValue(w.buffer);
                try self.writeU8(if (w.is_insert) 1 else 0);
            },
            .io_delete => |d| {
                try self.writeU8(@intFromEnum(Opcode.io_delete));
                try self.writeValue(d.channel);
            },
            .io_unlock => |u| {
                try self.writeU8(@intFromEnum(Opcode.io_unlock));
                try self.writeValue(u.channel);
            },

            // Exception handling
            .try_begin => |t| {
                try self.writeU8(@intFromEnum(Opcode.try_begin));
                try self.writeBlockRef(t.catch_block);
            },
            .try_end => {
                try self.writeU8(@intFromEnum(Opcode.try_end));
            },
            .catch_begin => |c| {
                try self.writeU8(@intFromEnum(Opcode.catch_begin));
                if (c.error_type) |et| {
                    try self.writeU8(1);
                    try self.writeString(et);
                } else {
                    try self.writeU8(0);
                }
            },
            .throw => |t| {
                try self.writeU8(@intFromEnum(Opcode.throw));
                try self.writeValue(t.value);
            },

            // Struct buffers - will call runtime
            .load_struct_buf => |sb| {
                // Encode as runtime call
                try self.writeU8(@intFromEnum(Opcode.runtime_call));
                try self.writeString("load_struct_buf");
                try self.writeString(sb.struct_name);
                try self.writeValue(sb.result);
            },
            .store_struct_buf => |sb| {
                try self.writeU8(@intFromEnum(Opcode.runtime_call));
                try self.writeString("store_struct_buf");
                try self.writeString(sb.struct_name);
                try self.writeValue(sb.value);
            },

            // Debug
            .debug_line => |d| {
                try self.writeU8(@intFromEnum(Opcode.debug_line));
                try self.writeU32(d.line);
                try self.writeU32(d.column);
            },

            .str_slice_store => {
                // Complex operation - encode as runtime call
                try self.writeU8(@intFromEnum(Opcode.runtime_call));
                try self.writeString("str_slice_store");
            },
        }
    }

    // Helper functions

    fn writeBinaryOp(self: *Self, opcode: Opcode, op: ir.Instruction.BinaryOp) !void {
        try self.writeU8(@intFromEnum(opcode));
        try self.writeValue(op.lhs);
        try self.writeValue(op.rhs);
        try self.writeValue(op.result);
    }

    fn writeUnaryOp(self: *Self, opcode: Opcode, op: ir.Instruction.UnaryOp) !void {
        try self.writeU8(@intFromEnum(opcode));
        try self.writeValue(op.operand);
        try self.writeValue(op.result);
    }

    fn writeValue(self: *Self, value: ir.Value) !void {
        try self.writeU32(value.id);
        try self.writeType(value.ty);
    }

    fn writeBlockRef(self: *Self, block: *ir.Block) !void {
        if (self.block_indices.get(block)) |idx| {
            try self.writeU16(idx);
        } else {
            try self.writeU16(0xFFFF); // Invalid block reference
        }
    }

    fn writeType(self: *Self, ty: ir.Type) !void {
        switch (ty) {
            .void => try self.writeU8(@intFromEnum(TypeId.void)),
            .bool => try self.writeU8(@intFromEnum(TypeId.bool)),
            .i8 => try self.writeU8(@intFromEnum(TypeId.i8)),
            .i16 => try self.writeU8(@intFromEnum(TypeId.i16)),
            .i32 => try self.writeU8(@intFromEnum(TypeId.i32)),
            .i64 => try self.writeU8(@intFromEnum(TypeId.i64)),
            .u8 => try self.writeU8(@intFromEnum(TypeId.u8)),
            .u16 => try self.writeU8(@intFromEnum(TypeId.u16)),
            .u32 => try self.writeU8(@intFromEnum(TypeId.u32)),
            .u64 => try self.writeU8(@intFromEnum(TypeId.u64)),
            .f32 => try self.writeU8(@intFromEnum(TypeId.f32)),
            .f64 => try self.writeU8(@intFromEnum(TypeId.f64)),
            .string => try self.writeU8(@intFromEnum(TypeId.string)),
            .string_fixed => |len| {
                try self.writeU8(@intFromEnum(TypeId.string_fixed));
                try self.writeU32(len);
            },
            .decimal => |d| {
                try self.writeU8(@intFromEnum(TypeId.decimal));
                try self.writeU8(@intCast(d.precision));
                try self.writeU8(d.scale);
            },
            .ptr => |p| {
                try self.writeU8(@intFromEnum(TypeId.ptr));
                try self.writeType(p.*);
            },
            .optional => |o| {
                try self.writeU8(@intFromEnum(TypeId.optional));
                try self.writeType(o.*);
            },
            .array => |a| {
                try self.writeU8(@intFromEnum(TypeId.array));
                try self.writeType(a.element.*);
                try self.writeU32(a.length);
            },
            .slice => |s| {
                try self.writeU8(@intFromEnum(TypeId.slice));
                try self.writeType(s.*);
            },
            .@"struct" => |s| {
                try self.writeU8(@intFromEnum(TypeId.@"struct"));
                try self.writeString(s.name);
            },
            .function => {
                try self.writeU8(@intFromEnum(TypeId.function));
            },
        }
    }

    fn writeString(self: *Self, str: []const u8) !void {
        try self.writeU16(@intCast(str.len));
        try self.buffer.appendSlice(str);
    }

    fn writeU8(self: *Self, val: u8) !void {
        try self.buffer.append(val);
    }

    fn writeU16(self: *Self, val: u16) !void {
        try self.buffer.appendSlice(&std.mem.toBytes(val));
    }

    fn writeU32(self: *Self, val: u32) !void {
        try self.buffer.appendSlice(&std.mem.toBytes(val));
    }

    fn writeI64(self: *Self, val: i64) !void {
        try self.buffer.appendSlice(&std.mem.toBytes(val));
    }

    fn writeF32(self: *Self, val: f32) !void {
        try self.buffer.appendSlice(&std.mem.toBytes(val));
    }

    fn writeF64(self: *Self, val: f64) !void {
        try self.buffer.appendSlice(&std.mem.toBytes(val));
    }
};

fn irCondToCondCode(cond: ir.IntCC) CondCode {
    return switch (cond) {
        .eq => .eq,
        .ne => .ne,
        .slt => .slt,
        .sge => .sge,
        .sgt => .sgt,
        .sle => .sle,
        .ult => .ult,
        .uge => .uge,
        .ugt => .ugt,
        .ule => .ule,
    };
}

// =============================================================================
// Tests
// =============================================================================

test "serialize empty module" {
    const allocator = std.testing.allocator;
    var serializer = IrSerializer.init(allocator);
    defer serializer.deinit();

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const bytes = try serializer.serializeModule(&module);

    // Check header
    try std.testing.expectEqual(MAGIC, std.mem.readInt(u32, bytes[0..4], .little));
    try std.testing.expectEqual(VERSION, std.mem.readInt(u16, bytes[4..6], .little));
    try std.testing.expectEqual(@as(u16, 0), std.mem.readInt(u16, bytes[6..8], .little)); // 0 functions
}

test "serialize simple function" {
    const allocator = std.testing.allocator;
    var serializer = IrSerializer.init(allocator);
    defer serializer.deinit();

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const sig = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "main", sig);
    try func.entry.append(.{ .return_ = null });
    try module.addFunction(func);

    const bytes = try serializer.serializeModule(&module);

    // Should have header + function data
    try std.testing.expect(bytes.len > 8);
    try std.testing.expectEqual(@as(u16, 1), std.mem.readInt(u16, bytes[6..8], .little)); // 1 function
}
