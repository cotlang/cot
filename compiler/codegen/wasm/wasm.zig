//! WebAssembly Code Generation Package
//!
//! This package provides WebAssembly code generation following Go's architecture:
//!
//! - constants.zig: Instructions and registers (like Go's a.out.go)
//! - prog.zig: Instruction chain structure (like Go's obj.Prog)
//! - preprocess.zig: High-level to low-level transform (like Go's wasmobj.go preprocess)
//! - assemble.zig: Prog to bytes (like Go's wasmobj.go assemble)
//! - link.zig: Module assembly (like Go's asm.go)
//! - gen.zig: SSA to Prog chain (like Go's ssa.go)
//!
//! Go reference files:
//! - cmd/compile/internal/wasm/ssa.go (595 lines)
//! - cmd/internal/obj/wasm/a.out.go (342 lines)
//! - cmd/internal/obj/wasm/wasmobj.go (1453 lines)
//! - cmd/link/internal/wasm/asm.go (707 lines)

const std = @import("std");

pub const constants = @import("constants.zig");
pub const prog = @import("prog.zig");
pub const preprocess_mod = @import("preprocess.zig");
pub const assemble_mod = @import("assemble.zig");
pub const link = @import("link.zig");
pub const gen = @import("gen.zig");

// Re-export commonly used types
pub const As = constants.As;
pub const Reg = constants.Reg;
pub const ValType = constants.ValType;
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

// Helper functions
pub const constAddr = prog.constAddr;
pub const regAddr = prog.regAddr;
pub const floatAddr = prog.floatAddr;
pub const memAddr = prog.memAddr;

// Import SSA types
const SsaFunc = @import("../../ssa/func.zig").Func;

const debug = @import("../../pipeline_debug.zig");

/// Function index map type (re-export from gen)
pub const FuncIndexMap = gen.FuncIndexMap;

/// String offset map type
pub const StringOffsetMap = std.StringHashMap(i32);

/// Generate WebAssembly bytecode for a function.
///
/// This is the main entry point that ties together Go's two-pass architecture:
/// 1. gen.zig: SSA → Prog chain (like Go's ssa.go)
/// 2. preprocess.zig: Transform pseudo-instructions, add dispatch loop (like Go's wasmobj.go)
/// 3. assemble.zig: Prog chain → bytes (like Go's wasmobj.go)
///
/// @param func_indices: Optional map of function names to Wasm function indices (for calls)
/// @param string_offsets: Map of string literal content to memory offsets (for const_string)
/// @param metadata_offsets: Map of type names to metadata memory offsets (for ARC destructors)
/// @param func_table_indices: Map of function names to element table indices (for addr → call_indirect)
pub fn generateFunc(
    allocator: std.mem.Allocator,
    ssa_func: *const SsaFunc,
    func_indices: ?*const FuncIndexMap,
    string_offsets: ?*const StringOffsetMap,
    metadata_offsets: ?*const StringOffsetMap,
    func_table_indices: ?*const std.StringHashMap(u32),
) ![]u8 {
    debug.log(.codegen, "wasm: generateFunc '{s}'", .{ssa_func.name});

    // Step 1: Generate Prog chain from SSA (Go's ssa.go)
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

    try gen_state.generate();

    // Create a Symbol for preprocess/assemble
    var sym = Symbol.init(ssa_func.name);
    sym.frame_size = gen_state.frame_size;
    sym.param_count = gen_state.param_count;
    sym.float_local_count = gen_state.float_local_count;
    sym.text = gen_state.builder.first;

    // Step 2: Preprocess - transform pseudo-instructions, add dispatch loop (Go's wasmobj.go)
    try preprocess_mod.preprocess(allocator, &sym);

    // Debug: trace the Prog chain after preprocess
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

    // Step 3: Assemble - Prog chain to bytes (Go's wasmobj.go)
    const assembled = try assemble_mod.assemble(allocator, &sym);
    // Note: We return the code but don't call deinit() since we're transferring ownership

    debug.log(.codegen, "wasm: generated {d} bytes for '{s}'", .{ assembled.code.len, ssa_func.name });

    // Clean up the Prog chain (we own it now, not gen_state.builder)
    // gen_state.builder.deinit() will try to free these, but we've already
    // transferred ownership to sym.text, so we need to free manually
    var p: ?*Prog = sym.text;
    while (p) |current| {
        const next = current.link;
        // Free br_table data if present
        if (current.as == .br_table) {
            if (current.to.val.br_table.len > 0) {
                allocator.free(current.to.val.br_table);
            }
        }
        allocator.destroy(current);
        p = next;
    }
    // Prevent double-free by clearing builder's pointers
    gen_state.builder.first = null;
    gen_state.builder.last = null;

    return assembled.code;
}

test {
    @import("std").testing.refAllDecls(@This());
}
