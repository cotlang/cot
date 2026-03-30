//! CIR binary format writer.
//!
//! Serializes clif.Function objects to the CIR binary format
//! defined in src/claude/CIR_FORMAT_SPEC.md.
//! The Rust libclif library reads this format and compiles with real Cranelift.

const std = @import("std");
const clif = @import("../../ir/clif/mod.zig");
const types_mod = @import("../../frontend/types.zig");
const debug = @import("../../pipeline_debug.zig");

const Allocator = std.mem.Allocator;

// CIR constants (must match rust/libclif/src/cir.rs)
const MAGIC: u32 = 0x00434952; // "CIR\0"
const VERSION_1_0: u32 = 0x00010000;

const SECTION_STRING_HEAP: u16 = 0x01;
const SECTION_FUNC_DEFS: u16 = 0x06;

// CIR opcodes (must match rust/libclif/src/cir.rs)
pub const OP_CONST_BOOL: u16 = 0x0000;
pub const OP_CONST_INT: u16 = 0x0001;
pub const OP_CONST_FLOAT: u16 = 0x0002;
pub const OP_ADD: u16 = 0x0010;
pub const OP_SUB: u16 = 0x0011;
pub const OP_MUL: u16 = 0x0012;
pub const OP_DIV: u16 = 0x0013;
pub const OP_UDIV: u16 = 0x0014;
pub const OP_AND: u16 = 0x0020;
pub const OP_OR: u16 = 0x0021;
pub const OP_XOR: u16 = 0x0022;
pub const OP_SHL: u16 = 0x0023;
pub const OP_SHR: u16 = 0x0024;
pub const OP_SAR: u16 = 0x0025;
pub const OP_EQ: u16 = 0x0030;
pub const OP_NE: u16 = 0x0031;
pub const OP_LT: u16 = 0x0032;
pub const OP_LE: u16 = 0x0033;
pub const OP_GT: u16 = 0x0034;
pub const OP_GE: u16 = 0x0035;
pub const OP_ADD_F: u16 = 0x0050;
pub const OP_SUB_F: u16 = 0x0051;
pub const OP_MUL_F: u16 = 0x0052;
pub const OP_DIV_F: u16 = 0x0053;
pub const OP_NEG_F: u16 = 0x0054;
pub const OP_NOT: u16 = 0x0026;
pub const OP_MOD: u16 = 0x0015;
pub const OP_LOAD: u16 = 0x0070;
pub const OP_STORE: u16 = 0x0071;
pub const OP_COPY: u16 = 0x0091;
pub const OP_ARG: u16 = 0x0092;
pub const OP_STATIC_CALL: u16 = 0x0093;
pub const OP_RET: u16 = 0x0097;
pub const OP_RET_VOID: u16 = 0x0098;
pub const OP_JUMP: u16 = 0x00A0;
pub const OP_BRIF: u16 = 0x00A1;
pub const OP_COND_SELECT: u16 = 0x0099;

pub const OP_GLOBAL_VALUE: u16 = 0x0081;
pub const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
pub const OP_GLOBAL_VALUE_IADD: u16 = 0x00B2;

pub const OP_STACK_SLOT_DECL: u16 = 0x00B0;

pub const OP_RETAIN: u16 = 0x00C0;
pub const OP_RELEASE: u16 = 0x00C1;

// Float comparison opcodes
pub const OP_EQ_F: u16 = 0x0060;
pub const OP_NE_F: u16 = 0x0061;
pub const OP_LT_F: u16 = 0x0062;
pub const OP_LE_F: u16 = 0x0063;
pub const OP_GT_F: u16 = 0x0064;
pub const OP_GE_F: u16 = 0x0065;

// Unsigned comparison opcodes
pub const OP_ULT: u16 = 0x0036;
pub const OP_ULE: u16 = 0x0037;
pub const OP_UGT: u16 = 0x0038;
pub const OP_UGE: u16 = 0x0039;

pub const OP_UMOD: u16 = 0x0016;

// Section IDs
const SECTION_FUNC_DECLS: u16 = 0x05;
const SECTION_STACK_SLOTS: u16 = 0x09;

// Structure markers
const FUNC_BEGIN: u16 = 0xFF00;
const FUNC_END: u16 = 0xFF01;
const BLOCK_BEGIN: u16 = 0xFF02;
const BLOCK_END: u16 = 0xFF03;

pub const CirWriter = struct {
    /// Function definition words — written during serialization.
    /// String heap is assembled separately and prepended at finish() time.
    words: std.ArrayListUnmanaged(u32),
    string_heap: std.ArrayListUnmanaged(u8),
    string_offsets: std.StringHashMapUnmanaged(u32),
    allocator: Allocator,

    pub fn init(allocator: Allocator) CirWriter {
        return CirWriter{
            .words = .{},
            .string_heap = .{},
            .string_offsets = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CirWriter) void {
        self.words.deinit(self.allocator);
        self.string_heap.deinit(self.allocator);
        self.string_offsets.deinit(self.allocator);
    }

    /// Intern a string, returning its offset in the string heap.
    pub fn internString(self: *CirWriter, s: []const u8) u32 {
        if (self.string_offsets.get(s)) |off| return off;

        const offset: u32 = @intCast(self.string_heap.items.len);
        const len: u32 = @intCast(s.len);

        // Write length + bytes + padding
        self.string_heap.appendSlice(self.allocator, std.mem.asBytes(&len)) catch return 0;
        self.string_heap.appendSlice(self.allocator, s) catch return 0;
        while (self.string_heap.items.len % 4 != 0) {
            self.string_heap.append(self.allocator, 0) catch {};
        }

        self.string_offsets.put(self.allocator, s, offset) catch {};
        return offset;
    }

    /// Begin function definitions section — no-op, func defs go into self.words directly.
    /// The section header is written at finish() time.
    pub fn beginFuncDefs(self: *CirWriter) void {
        _ = self;
    }

    /// Write a function begin marker with full signature.
    /// Format: name_offset, param_count, return_count, block_count, flags, [param_types...], [return_types...]
    pub fn beginFuncWithSig(self: *CirWriter, name_offset: u32, param_types: []const u32, return_types: []const u32, block_count: u32, flags: u32) void {
        var operands_buf: [128]u32 = undefined;
        operands_buf[0] = name_offset;
        operands_buf[1] = @intCast(param_types.len);
        operands_buf[2] = @intCast(return_types.len);
        operands_buf[3] = block_count;
        operands_buf[4] = flags;
        var pos: usize = 5;
        for (param_types) |pt| {
            operands_buf[pos] = pt;
            pos += 1;
        }
        for (return_types) |rt| {
            operands_buf[pos] = rt;
            pos += 1;
        }
        self.emitRaw(FUNC_BEGIN, operands_buf[0..pos]);
    }

    pub fn endFunc(self: *CirWriter) void {
        self.words.append(self.allocator, (@as(u32, 1) << 16) | @as(u32, FUNC_END)) catch {};
    }

    pub fn beginBlock(self: *CirWriter, id: u32, kind: u8, successors: []const u32, predecessors: []const u32) void {
        const word_count: u32 = @intCast(5 + successors.len + predecessors.len);
        self.words.append(self.allocator, (word_count << 16) | @as(u32, BLOCK_BEGIN)) catch {};
        self.words.append(self.allocator, id) catch {};
        self.words.append(self.allocator, kind) catch {};
        self.words.append(self.allocator, @intCast(successors.len)) catch {};
        for (successors) |s| self.words.append(self.allocator, s) catch {};
        self.words.append(self.allocator, @intCast(predecessors.len)) catch {};
        for (predecessors) |p| self.words.append(self.allocator, p) catch {};
    }

    pub fn endBlock(self: *CirWriter) void {
        self.words.append(self.allocator, (@as(u32, 1) << 16) | @as(u32, BLOCK_END)) catch {};
    }

    /// Emit a CIR instruction.
    pub fn emit(self: *CirWriter, opcode: u16, operands: []const u32) void {
        self.emitRaw(opcode, operands);
    }

    fn emitRaw(self: *CirWriter, opcode: u16, operands: []const u32) void {
        const word_count: u32 = @intCast(1 + operands.len);
        self.words.append(self.allocator, (word_count << 16) | @as(u32, opcode)) catch {};
        for (operands) |op| self.words.append(self.allocator, op) catch {};
    }

    /// Map a Cot TypeIndex to the CIR pre-registered type index.
    /// These must match the constants in rust/libclif/src/cir.rs.
    pub fn mapTypeIndex(type_idx: types_mod.TypeIndex) u32 {
        // Cot TypeRegistry indices match CIR pre-registered types directly
        return @intCast(type_idx);
    }

    /// Finalize and return the CIR bytes.
    /// Assembles: [header] [string_heap_section] [func_defs_section]
    /// The string heap is written LAST so all strings interned during func serialization are included.
    pub fn finish(self: *CirWriter) []const u8 {
        var out = std.ArrayListUnmanaged(u32){};

        // Header (5 words)
        out.append(self.allocator, MAGIC) catch {};
        out.append(self.allocator, VERSION_1_0) catch {};
        out.append(self.allocator, 0) catch {}; // generator
        out.append(self.allocator, 0) catch {}; // bound
        out.append(self.allocator, 0) catch {}; // reserved

        // String heap section
        const heap_bytes = self.string_heap.items.len;
        const heap_words = (heap_bytes + 3) / 4;
        const heap_section_wc: u32 = @intCast(2 + heap_words);
        out.append(self.allocator, (heap_section_wc << 16) | @as(u32, SECTION_STRING_HEAP)) catch {};
        out.append(self.allocator, @intCast(heap_bytes)) catch {};
        {
            var i: usize = 0;
            while (i < heap_bytes) : (i += 4) {
                var word: [4]u8 = .{ 0, 0, 0, 0 };
                const remaining = @min(4, heap_bytes - i);
                @memcpy(word[0..remaining], self.string_heap.items[i..][0..remaining]);
                out.append(self.allocator, std.mem.bytesAsValue(u32, &word).*) catch {};
            }
        }

        // Func defs section (word count 0 = read until end)
        out.append(self.allocator, @as(u32, SECTION_FUNC_DEFS)) catch {};
        out.appendSlice(self.allocator, self.words.items) catch {};

        return std.mem.sliceAsBytes(out.items);
    }
};

const Opcode = clif.Opcode;
const IntCC = clif.IntCC;
const FloatCC = clif.FloatCC;

/// Reverse mapping from func_index_map index → function name.
/// Built once, passed to serializeClifFunction.
pub const FuncNameResolver = struct {
    /// Map from index → name (built from func_index_map)
    names: std.AutoHashMapUnmanaged(u32, []const u8),
    allocator: Allocator,

    pub fn init(allocator: Allocator, func_index_map: *const std.StringHashMapUnmanaged(u32)) FuncNameResolver {
        var names = std.AutoHashMapUnmanaged(u32, []const u8){};
        var it = func_index_map.iterator();
        while (it.next()) |entry| {
            names.put(allocator, entry.value_ptr.*, entry.key_ptr.*) catch {};
        }
        return .{ .names = names, .allocator = allocator };
    }

    /// Build from a pre-inverted map (index → name).
    pub fn initFromReversed(allocator: Allocator, reversed: *const std.AutoHashMapUnmanaged(u32, []const u8)) FuncNameResolver {
        var names = std.AutoHashMapUnmanaged(u32, []const u8){};
        var it = reversed.iterator();
        while (it.next()) |entry| {
            names.put(allocator, entry.key_ptr.*, entry.value_ptr.*) catch {};
        }
        return .{ .names = names, .allocator = allocator };
    }

    pub fn deinit(self: *FuncNameResolver) void {
        self.names.deinit(self.allocator);
    }

    pub fn resolve(self: *const FuncNameResolver, index: u32) []const u8 {
        return self.names.get(index) orelse "__unknown";
    }
};

/// Serialize a clif.Function's blocks and instructions to CIR format.
/// Called between beginFunc() and endFunc() on the writer.
pub fn serializeClifFunction(writer: *CirWriter, func: *const clif.Function, resolver: *const FuncNameResolver) !void {
    // Reset synthetic ID counter for each function
    next_synthetic_id = 0xC0000000;

    // Build block index map: Block → sequential index
    var block_index_map = std.AutoHashMapUnmanaged(u32, u32){};
    defer block_index_map.deinit(writer.allocator);
    {
        var bi = func.layout.blocks();
        var idx: u32 = 0;
        while (bi.next()) |b| {
            block_index_map.put(writer.allocator, b.asU32(), idx) catch {};
            idx += 1;
        }
    }

    var block_iter = func.layout.blocks();
    var block_idx: u32 = 0;

    while (block_iter.next()) |block| {
        // Determine block kind
        const kind: u8 = if (block_idx == 0) 0x05 else 0x00; // entry or plain

        writer.beginBlock(block_idx, kind, &.{}, &.{});

        // Entry block: emit stack slot declarations + OP_ARGs
        if (block_idx == 0) {
            // Stack slot declarations must be inside the first block
            for (func.stack_slots.items, 0..) |slot, si| {
                writer.emit(OP_STACK_SLOT_DECL, &.{
                    @as(u32, @intCast(si)),
                    slot.size,
                    @as(u32, 1) << @intCast(slot.align_shift),
                });
            }

            // Global value declarations — emit symbol references for each global
            for (func.global_values.items, 0..) |gv, gi| {
                switch (gv) {
                    .symbol => |sym| {
                        // Resolve symbol name from ExternalName
                        const sym_name = switch (sym.name) {
                            .user => |u| blk: {
                                const resolved = resolver.resolve(u.index);
                                if (std.mem.eql(u8, resolved, "__unknown")) {
                                    debug.log(.codegen, "CIR: unresolved global symbol at user index {d}", .{u.index});
                                }
                                break :blk resolved;
                            },
                            .libcall => |n| n,
                        };
                        const name_off = writer.internString(sym_name);
                        const offset_lo: u32 = @truncate(@as(u64, @bitCast(sym.offset)));
                        const offset_hi: u32 = @truncate(@as(u64, @bitCast(sym.offset)) >> 32);
                        writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{
                            @as(u32, @intCast(gi)), // gv index
                            name_off,               // symbol name
                            offset_lo,              // offset low
                            offset_hi,              // offset high
                            @as(u32, if (sym.colocated) 1 else 0), // colocated
                        });
                    },
                    .iadd_imm => |add| {
                        const offset_lo: u32 = @truncate(@as(u64, @bitCast(add.offset)));
                        const offset_hi: u32 = @truncate(@as(u64, @bitCast(add.offset)) >> 32);
                        writer.emit(OP_GLOBAL_VALUE_IADD, &.{
                            @as(u32, @intCast(gi)), // gv index
                            add.base.asU32(),       // base gv index
                            offset_lo,
                            offset_hi,
                        });
                    },
                    .vmcontext => {
                        // vmcontext is special — emit as symbol "__vmctx"
                        const name_off = writer.internString("__vmctx");
                        writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{
                            @as(u32, @intCast(gi)),
                            name_off,
                            0, 0, 0,
                        });
                    },
                    .load => {},      // TODO: handle if needed
                    .dyn_scale_target_const => {},
                }
            }

            const params = func.dfg.blockParams(block);
            for (params, 0..) |param, pi| {
                const ty = func.dfg.valueType(param);
                writer.emit(OP_ARG, &.{ param.asU32(), mapClifType(ty), @as(u32, @intCast(pi)) });
            }
        } else {
            // Non-entry blocks: emit OP_ARG for block params (phi values)
            const params = func.dfg.blockParams(block);
            for (params, 0..) |param, pi| {
                const ty = func.dfg.valueType(param);
                writer.emit(OP_ARG, &.{ param.asU32(), mapClifType(ty), @as(u32, @intCast(pi)) });
            }
        }

        // Emit instructions
        var inst_iter = func.layout.blockInsts(block);
        while (inst_iter.next()) |inst| {
            const data = func.dfg.insts.items[inst.index];
            const results = func.dfg.instResults(inst);
            const result_id: u32 = if (results.len > 0) results[0].asU32() else 0;
            const result_type: u32 = if (results.len > 0) mapClifType(func.dfg.valueType(results[0])) else 12;

            serializeInstruction(writer, data, result_id, result_type, results, func, resolver, &block_index_map);
        }

        writer.endBlock();
        block_idx += 1;
    }
}

pub fn mapClifType(ty: clif.Type) u32 {
    if (ty.eql(clif.Type.I8)) return 2;
    if (ty.eql(clif.Type.I16)) return 3;
    if (ty.eql(clif.Type.I32)) return 4;
    if (ty.eql(clif.Type.I64)) return 5;
    if (ty.eql(clif.Type.F32)) return 10;
    if (ty.eql(clif.Type.F64)) return 11;
    return 5; // default to I64
}

fn mapIntCC(cond: IntCC) u16 {
    return switch (cond) {
        .eq => OP_EQ,
        .ne => OP_NE,
        .slt => OP_LT,
        .sle => OP_LE,
        .sgt => OP_GT,
        .sge => OP_GE,
        .ult => 0x0036, // OP_ULT
        .ule => 0x0037,
        .ugt => 0x0038,
        .uge => 0x0039,
    };
}

var next_synthetic_id: u32 = 0xC0000000; // Counter for unique synthetic value IDs

fn nextSyntheticId() u32 {
    const id = next_synthetic_id;
    next_synthetic_id += 1;
    return id;
}

fn serializeInstruction(writer: *CirWriter, data: clif.InstructionData, result_id: u32, result_type: u32, all_results: []const clif.Value, func: *const clif.Function, resolver: *const FuncNameResolver, block_index_map: *const std.AutoHashMapUnmanaged(u32, u32)) void {
    switch (data) {
        .nullary => |d| {
            switch (d.opcode) {
                .@"return" => writer.emit(OP_RET_VOID, &.{}),
                .trap => writer.emit(0x00A2, &.{}), // OP_TRAP
                else => {},
            }
        },

        .unary => |d| {
            const arg = d.arg.asU32();
            const ty = mapClifType(func.dfg.valueType(d.arg));
            switch (d.opcode) {
                .@"return" => writer.emit(OP_RET, &.{arg}),
                .copy => writer.emit(OP_COPY, &.{ result_id, ty, arg }),
                .fneg => writer.emit(OP_NEG_F, &.{ result_id, ty, arg }),
                .bnot => writer.emit(OP_NOT, &.{ result_id, ty, arg }),
                .uextend => writer.emit(0x0040, &.{ result_id, result_type, arg }), // OP_UEXTEND
                .sextend => writer.emit(0x0041, &.{ result_id, result_type, arg }), // OP_SEXTEND
                .ireduce => writer.emit(0x0042, &.{ result_id, result_type, arg }), // OP_IREDUCE
                .ineg => writer.emit(0x0017, &.{ result_id, result_type, arg }),
                .bitcast => writer.emit(0x0049, &.{ result_id, result_type, arg }), // OP_BITCAST
                .fcvt_from_sint => writer.emit(0x0043, &.{ result_id, result_type, arg }),
                .fcvt_to_sint => writer.emit(0x0044, &.{ result_id, result_type, arg }),
                .fcvt_to_uint => writer.emit(0x0048, &.{ result_id, result_type, arg }),
                .fcvt_from_uint => writer.emit(0x0045, &.{ result_id, result_type, arg }),
                .fpromote => writer.emit(0x0046, &.{ result_id, result_type, arg }),
                .fdemote => writer.emit(0x0047, &.{ result_id, result_type, arg }),
                // Float math builtins
                .fabs => writer.emit(0x0055, &.{ result_id, result_type, arg }),
                .sqrt => writer.emit(0x0056, &.{ result_id, result_type, arg }),
                .ceil => writer.emit(0x0057, &.{ result_id, result_type, arg }),
                .floor => writer.emit(0x0058, &.{ result_id, result_type, arg }),
                .trunc => writer.emit(0x0059, &.{ result_id, result_type, arg }),
                .nearest => writer.emit(0x005A, &.{ result_id, result_type, arg }),
                // Bit counting
                .popcnt => writer.emit(0x0028, &.{ result_id, result_type, arg }),
                .clz => writer.emit(0x0029, &.{ result_id, result_type, arg }),
                .ctz => writer.emit(0x002A, &.{ result_id, result_type, arg }),
                else => {
                    // Log unhandled opcodes so they don't silently break
                    if (std.posix.getenv("CIR_TRACE")) |_| {
                        std.debug.print("CIR SKIP unary: {s} v{d}\n", .{ @tagName(d.opcode), result_id });
                    }
                },
            }
        },

        .unary_imm => |d| {
            const imm = d.imm;
            const imm_lo: u32 = @truncate(@as(u64, @bitCast(imm)));
            const imm_hi: u32 = @truncate(@as(u64, @bitCast(imm)) >> 32);
            if (imm_hi != 0) {
                writer.emit(OP_CONST_INT, &.{ result_id, result_type, imm_lo, imm_hi });
            } else {
                writer.emit(OP_CONST_INT, &.{ result_id, result_type, imm_lo });
            }
        },

        .unary_ieee64 => |d| {
            const bits: u64 = @bitCast(d.imm);
            writer.emit(OP_CONST_FLOAT, &.{ result_id, 11, @truncate(bits), @truncate(bits >> 32) }); // F64
        },

        .unary_ieee32 => |d| {
            const bits: u32 = @bitCast(d.imm);
            writer.emit(OP_CONST_FLOAT, &.{ result_id, 10, bits, 0 }); // F32
        },

        .binary => |d| {
            const lhs = d.args[0].asU32();
            const rhs = d.args[1].asU32();
            const ty = mapClifType(func.dfg.valueType(d.args[0]));
            const op: ?u16 = switch (d.opcode) {
                .iadd => OP_ADD,
                .isub => OP_SUB,
                .imul => OP_MUL,
                .sdiv => OP_DIV,
                .udiv => OP_UDIV,
                .srem => OP_MOD,
                .urem => OP_UMOD,
                .band => OP_AND,
                .bor => OP_OR,
                .bxor => OP_XOR,
                .ishl => OP_SHL,
                .ushr => OP_SHR,
                .sshr => OP_SAR,
                .fadd => OP_ADD_F,
                .fsub => OP_SUB_F,
                .fmul => OP_MUL_F,
                .fdiv => OP_DIV_F,
                .fmin => 0x005B,
                .fmax => 0x005C,
                .fcopysign => 0x005D,
                else => null,
            };
            if (op) |cir_op| {
                writer.emit(cir_op, &.{ result_id, ty, lhs, rhs });
            }
        },

        .binary_imm64 => |d| {
            // iadd_imm: emit as const + add
            const arg = d.arg.asU32();
            const ty = mapClifType(func.dfg.valueType(d.arg));
            const imm_lo: u32 = @truncate(@as(u64, @bitCast(d.imm)));
            const imm_hi: u32 = @truncate(@as(u64, @bitCast(d.imm)) >> 32);
            const const_id: u32 = nextSyntheticId();
            if (imm_hi != 0) {
                writer.emit(OP_CONST_INT, &.{ const_id, ty, imm_lo, imm_hi });
            } else {
                writer.emit(OP_CONST_INT, &.{ const_id, ty, imm_lo });
            }
            writer.emit(OP_ADD, &.{ result_id, ty, arg, const_id });
        },

        .int_compare => |d| {
            const lhs = d.args[0].asU32();
            const rhs = d.args[1].asU32();
            // Use the operand type for comparison (not result type which is i8)
            const operand_ty = mapClifType(func.dfg.valueType(d.args[0]));
            writer.emit(mapIntCC(d.cond), &.{ result_id, operand_ty, lhs, rhs });
        },

        .float_compare => |d| {
            const lhs = d.args[0].asU32();
            const rhs = d.args[1].asU32();
            const operand_ty = mapClifType(func.dfg.valueType(d.args[0]));
            const op: u16 = switch (d.cond) {
                .eq => OP_EQ_F,
                .ne => OP_NE_F,
                .lt => OP_LT_F,
                .le => OP_LE_F,
                .gt => OP_GT_F,
                .ge => OP_GE_F,
                else => OP_EQ_F,
            };
            writer.emit(op, &.{ result_id, operand_ty, lhs, rhs });
        },

        .load => |d| {
            const addr = d.arg.asU32();
            if (d.offset != 0) {
                // Load with offset: addr + offset
                const off_id = nextSyntheticId();
                const adj_id = nextSyntheticId();
                writer.emit(OP_CONST_INT, &.{ off_id, 5, @as(u32, @bitCast(d.offset)) });
                writer.emit(OP_ADD, &.{ adj_id, 5, addr, off_id });
                writer.emit(OP_LOAD, &.{ result_id, result_type, adj_id });
            } else {
                writer.emit(OP_LOAD, &.{ result_id, result_type, addr });
            }
        },

        .store => |d| {
            const val = d.args[0].asU32();
            const addr = d.args[1].asU32();
            const val_type = mapClifType(func.dfg.valueType(d.args[0]));
            if (d.offset != 0) {
                // Store with offset: addr + offset
                const off_id = nextSyntheticId();
                const adj_id = nextSyntheticId();
                writer.emit(OP_CONST_INT, &.{ off_id, 5, @as(u32, @bitCast(d.offset)) });
                writer.emit(OP_ADD, &.{ adj_id, 5, addr, off_id });
                writer.emit(OP_STORE, &.{ val_type, adj_id, val });
            } else {
                writer.emit(OP_STORE, &.{ val_type, addr, val });
            }
        },

        .stack_load => |d| {
            if (d.opcode == .stack_addr) {
                // stack_addr: get address of stack slot
                writer.emit(0x0080, &.{ result_id, 5, d.slot.asU32() }); // OP_LOCAL_ADDR
            } else if (d.opcode == .stack_load) {
                // stack_load: load value from stack slot
                // Emit as LOCAL_ADDR + LOAD
                const addr_id = nextSyntheticId();
                writer.emit(0x0080, &.{ addr_id, 5, d.slot.asU32() }); // OP_LOCAL_ADDR
                writer.emit(OP_LOAD, &.{ result_id, 5, addr_id }); // LOAD from addr
            }
        },

        .call => |d| {
            // Resolve function name from FuncRef using the resolver
            const ext_func = func.ext_funcs.items[d.func_ref.index];
            const name = switch (ext_func.name) {
                .user => |u| blk: {
                    const resolved = resolver.resolve(u.index);
                    if (std.mem.eql(u8, resolved, "__unknown")) {
                        debug.log(.codegen, "CIR: unresolved call target at user index {d} (func_ref={d})", .{ u.index, d.func_ref.index });
                    }
                    break :blk resolved;
                },
                .libcall => |n| n,
            };
            const name_off = writer.internString(name);

            // Get callee's actual signature for parameter types
            const callee_sig = func.getSignature(ext_func.signature);

            // Collect args from ValueList
            const args = func.dfg.value_lists.getSlice(d.args);
            var operands_buf: [256]u32 = undefined;
            // Format: (result_count, [result_id, result_type]..., name_offset, arg_count, [arg_type, arg_value_id]...)
            const result_count = all_results.len;
            operands_buf[0] = @intCast(result_count);
            var pos: usize = 1;
            for (all_results) |r| {
                operands_buf[pos] = r.asU32();
                operands_buf[pos + 1] = mapClifType(func.dfg.valueType(r));
                pos += 2;
            }
            operands_buf[pos] = name_off;
            pos += 1;
            const arg_count = @min(args.len, (256 - pos - 1) / 2);
            operands_buf[pos] = @intCast(arg_count);
            pos += 1;
            for (args[0..arg_count], 0..) |a, i| {
                // Emit callee's expected parameter type from its signature
                const param_type: u32 = if (callee_sig) |sig| blk: {
                    break :blk if (i < sig.params.items.len) mapClifType(sig.params.items[i].value_type) else 5;
                } else 5; // fallback to I64
                operands_buf[pos] = param_type;
                operands_buf[pos + 1] = a.asU32();
                pos += 2;
            }
            writer.emit(OP_STATIC_CALL, operands_buf[0..pos]);
        },

        .jump => |d| {
            // Emit jump with target block ID and block arguments
            const target_idx = block_index_map.get(d.destination.block.asU32()) orelse 0;
            const args = d.destination.getArgs(&func.dfg.value_lists);
            var operands_buf: [33]u32 = undefined;
            operands_buf[0] = target_idx;
            operands_buf[1] = @intCast(args.len);
            const arg_count = @min(args.len, 31);
            for (args[0..arg_count], 0..) |a, i| {
                operands_buf[2 + i] = a.asU32();
            }
            writer.emit(OP_JUMP, operands_buf[0 .. 2 + arg_count]);
        },

        .brif => |d| {
            // Conditional branch: condition, then_block, else_block, then_args, else_args
            const cond = d.arg.asU32();
            const then_idx = block_index_map.get(d.blocks[0].block.asU32()) orelse 0;
            const else_idx = block_index_map.get(d.blocks[1].block.asU32()) orelse 0;
            const then_args = d.blocks[0].getArgs(&func.dfg.value_lists);
            const else_args = d.blocks[1].getArgs(&func.dfg.value_lists);

            var operands_buf: [64]u32 = undefined;
            operands_buf[0] = cond;
            operands_buf[1] = then_idx;
            operands_buf[2] = else_idx;
            operands_buf[3] = @intCast(then_args.len);
            var pos: usize = 4;
            for (then_args) |a| {
                operands_buf[pos] = a.asU32();
                pos += 1;
            }
            operands_buf[pos] = @intCast(else_args.len);
            pos += 1;
            for (else_args) |a| {
                operands_buf[pos] = a.asU32();
                pos += 1;
            }
            writer.emit(OP_BRIF, operands_buf[0..pos]);
        },

        .ternary => |d| {
            // select
            if (d.opcode == .select) {
                const cond = d.args[0].asU32();
                const true_val = d.args[1].asU32();
                const false_val = d.args[2].asU32();
                writer.emit(OP_COND_SELECT, &.{ result_id, 5, cond, true_val, false_val });
            }
        },

        .stack_store => |d| {
            // stack_store: store value to stack slot
            const val = d.arg.asU32();
            // Emit as LOCAL_ADDR(slot) + STORE with unique synthetic ID
            const slot_id = nextSyntheticId();
            writer.emit(0x0080, &.{ slot_id, 5, d.slot.asU32() }); // OP_LOCAL_ADDR
            writer.emit(OP_STORE, &.{ 5, slot_id, val });
        },

        .multi_ary => |d| {
            // return with args — emit ALL return values for multi-return functions
            if (d.opcode == .@"return") {
                const args = func.dfg.value_lists.getSlice(d.args);
                if (args.len == 0) {
                    writer.emit(OP_RET_VOID, &.{});
                } else if (args.len == 1) {
                    writer.emit(OP_RET, &.{args[0].asU32()});
                } else {
                    // Multi-return: emit OP_RET with all value IDs
                    var operands_buf: [16]u32 = undefined;
                    for (args, 0..) |a, i| {
                        operands_buf[i] = a.asU32();
                    }
                    writer.emit(OP_RET, operands_buf[0..args.len]);
                }
            }
        },

        .func_addr => |d| {
            // Function address — emit name + full signature so Rust side can declare correctly
            const ext_func = func.ext_funcs.items[d.func_ref.index];
            const name = switch (ext_func.name) {
                .user => |u| blk: {
                    const resolved = resolver.resolve(u.index);
                    if (std.mem.eql(u8, resolved, "__unknown")) {
                        debug.log(.codegen, "CIR: unresolved func_addr at user index {d} (func_ref={d})", .{ u.index, d.func_ref.index });
                    }
                    break :blk resolved;
                },
                .libcall => |n| n,
            };
            const name_off = writer.internString(name);
            const sig = func.getSignature(ext_func.signature);
            // Format: (result_id, type_idx, name_offset, param_count, [param_types...], return_count, [return_types...])
            var operands_buf: [64]u32 = undefined;
            operands_buf[0] = result_id;
            operands_buf[1] = 5; // I64
            operands_buf[2] = name_off;
            var pos: usize = 3;
            if (sig) |s| {
                operands_buf[pos] = @intCast(s.params.items.len);
                pos += 1;
                for (s.params.items) |p| {
                    operands_buf[pos] = mapClifType(p.value_type);
                    pos += 1;
                }
                operands_buf[pos] = @intCast(s.returns.items.len);
                pos += 1;
                for (s.returns.items) |r| {
                    operands_buf[pos] = mapClifType(r.value_type);
                    pos += 1;
                }
            } else {
                operands_buf[pos] = 0; // param_count
                pos += 1;
                operands_buf[pos] = 0; // return_count
                pos += 1;
            }
            writer.emit(0x0095, operands_buf[0..pos]); // OP_FUNC_ADDR
        },

        .unary_global_value => |d| {
            // global_value — reference a declared global value by index
            writer.emit(OP_GLOBAL_VALUE, &.{ result_id, 5, d.global_value.asU32() });
        },

        .trap => {
            writer.emit(0x00A2, &.{}); // OP_TRAP
        },
        .cond_trap => |d| {
            const arg = d.arg.asU32();
            writer.emit(0x00A3, &.{arg}); // OP_COND_TRAP
        },
        .branch_table => |d| {
            // br_table: arg is the index, table has default + entries
            const index_val = d.arg.asU32();
            const jt = func.dfg.jump_tables.get(d.table) orelse return;
            // Format: (index_value, entry_count, [block_id, arg_count, args...]...)
            // First entry is default, rest are indexed
            var operands_buf: [256]u32 = undefined;
            operands_buf[0] = index_val;
            operands_buf[1] = @intCast(jt.table.items.len);
            var pos: usize = 2;
            for (jt.table.items) |bc| {
                const target_idx = block_index_map.get(bc.block.asU32()) orelse 0;
                const args = bc.getArgs(&func.dfg.value_lists);
                operands_buf[pos] = target_idx;
                operands_buf[pos + 1] = @intCast(args.len);
                pos += 2;
                for (args) |a| {
                    operands_buf[pos] = a.asU32();
                    pos += 1;
                }
            }
            writer.emit(0x00A4, operands_buf[0..pos]); // OP_BR_TABLE
        },
        .call_indirect => |d| {
            // Indirect call: callee is a value (function pointer), not a FuncRef
            // NOTE: d.args includes [callee, arg0, arg1, ...] — skip first element (callee)
            const callee = d.callee.asU32();
            const all_args = func.dfg.value_lists.getSlice(d.args);
            const call_args = if (all_args.len > 0) all_args[1..] else all_args; // skip callee

            // Get the indirect call signature for parameter types
            const ind_sig = func.getSignature(d.sig_ref);

            var operands_buf: [256]u32 = undefined;
            // Format: (result_count, [result_id, result_type]..., callee_value, arg_count, [arg_type, arg_value_id]...)
            const result_count = all_results.len;
            operands_buf[0] = @intCast(result_count);
            var pos: usize = 1;
            for (all_results) |r| {
                operands_buf[pos] = r.asU32();
                operands_buf[pos + 1] = mapClifType(func.dfg.valueType(r));
                pos += 2;
            }
            operands_buf[pos] = callee;
            pos += 1;
            const arg_count = @min(call_args.len, (256 - pos - 1) / 2);
            operands_buf[pos] = @intCast(arg_count);
            pos += 1;
            for (call_args[0..arg_count], 0..) |a, i| {
                const param_type: u32 = if (ind_sig) |sig| blk: {
                    break :blk if (i < sig.params.items.len) mapClifType(sig.params.items[i].value_type) else 5;
                } else 5;
                operands_buf[pos] = param_type;
                operands_buf[pos + 1] = a.asU32();
                pos += 2;
            }
            writer.emit(0x0094, operands_buf[0..pos]); // OP_CALL_INDIRECT
        },
    }
}
