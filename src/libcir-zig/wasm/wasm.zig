//! WebAssembly codegen entry point — ties together gen, preprocess, and assemble.

const std = @import("std");

pub const constants = @import("constants.zig");
pub const prog = @import("prog.zig");
pub const preprocess_mod = @import("preprocess.zig");
pub const assemble_mod = @import("assemble.zig");
pub const link = @import("link.zig");
pub const gen = @import("gen.zig");

pub const As = constants.As;
pub const Reg = constants.Reg;
pub const ValType = constants.ValType;
pub const WasmType = constants.WasmType;
pub const Section = constants.Section;
pub const ExportKind = constants.ExportKind;

pub const Prog = prog.Prog;
pub const Addr = prog.Addr;
pub const Symbol = prog.Symbol;
pub const ProgBuilder = prog.ProgBuilder;

pub const Linker = link.Linker;
pub const FuncType = link.FuncType;
pub const WasmFunc = link.WasmFunc;

pub const GenState = gen.GenState;

pub const constAddr = prog.constAddr;
pub const regAddr = prog.regAddr;
pub const floatAddr = prog.floatAddr;
pub const memAddr = prog.memAddr;

const SsaFunc = @import("../ssa/func.zig").Func;

const foundation = @import("foundation");
const debug = foundation.debug;

pub const FuncIndexMap = gen.FuncIndexMap;

pub const StringOffsetMap = std.StringHashMap(i32);

/// Generate WebAssembly bytecode for a function.
///
/// This is the main entry point that ties together the two-pass architecture:
/// 1. gen.zig: SSA -> Prog chain
/// 2. preprocess.zig: Transform pseudo-instructions, add dispatch loop
/// 3. assemble.zig: Prog chain -> bytes
pub fn generateFunc(
    allocator: std.mem.Allocator,
    ssa_func: *const SsaFunc,
    func_indices: ?*const FuncIndexMap,
    string_offsets: ?*const StringOffsetMap,
    metadata_offsets: ?*const StringOffsetMap,
    func_table_indices: ?*const std.StringHashMap(u32),
    gc_struct_name_map: ?*const std.StringHashMapUnmanaged(u32),
    gc_array_name_map: ?*const std.StringHashMapUnmanaged(u32),
    type_reg: ?*const foundation.types.TypeRegistry,
) ![]u8 {
    debug.log(.codegen, "wasm: generateFunc '{s}'", .{ssa_func.name});

    var gen_state = GenState.init(allocator, ssa_func);
    defer gen_state.deinit();

    if (func_indices) |indices| {
        gen_state.setFuncIndices(indices);
    }
    if (string_offsets) |offsets| {
        gen_state.setStringOffsets(offsets);
    }
    if (metadata_offsets) |offsets| {
        gen_state.setMetadataOffsets(offsets);
    }
    if (func_table_indices) |indices| {
        gen_state.setFuncTableIndices(indices);
    }
    if (gc_struct_name_map) |m| {
        gen_state.setGcStructNameMap(m);
    }
    if (gc_array_name_map) |m| {
        gen_state.setGcArrayNameMap(m);
    }
    if (type_reg) |reg| {
        gen_state.setTypeReg(reg);
    }

    try gen_state.generate();

    var sym = Symbol.init(ssa_func.name);
    sym.frame_size = gen_state.frame_size;
    sym.param_count = gen_state.param_count;
    sym.float_local_count = gen_state.float_local_count;
    sym.gc_ref_locals = gen_state.gc_ref_locals.items;
    sym.text = gen_state.builder.first;

    try preprocess_mod.preprocess(allocator, &sym);

    {
        debug.log(.codegen, "wasm: Prog chain for '{s}':", .{ssa_func.name});
        var depth: i32 = 0;
        var dbg_p: ?*Prog = sym.text;
        while (dbg_p) |current| {
            switch (current.as) {
                .block, .loop, .@"if" => {
                    debug.log(.codegen, "  [{d}] {s}", .{ depth, @tagName(current.as) });
                    depth += 1;
                },
                .end => {
                    depth -= 1;
                    debug.log(.codegen, "  [{d}] {s}", .{ depth, @tagName(current.as) });
                },
                .br_table => debug.log(.codegen, "  [{d}] {s} ({d} entries)", .{ depth, @tagName(current.as), current.to.val.br_table.len }),
                else => debug.log(.codegen, "  [{d}] {s}", .{ depth, @tagName(current.as) }),
            }
            dbg_p = current.link;
        }
        debug.log(.codegen, "  final depth: {d}", .{depth});
    }

    const assembled = try assemble_mod.assemble(allocator, &sym);

    debug.log(.codegen, "wasm: generated {d} bytes for '{s}'", .{ assembled.code.len, ssa_func.name });

    var p: ?*Prog = sym.text;
    while (p) |current| {
        const next = current.link;
        if (current.as == .br_table) {
            if (current.to.val.br_table.len > 0) {
                allocator.free(current.to.val.br_table);
            }
        }
        allocator.destroy(current);
        p = next;
    }
    gen_state.builder.first = null;
    gen_state.builder.last = null;

    return assembled.code;
}

test {
    @import("std").testing.refAllDecls(@This());
}
