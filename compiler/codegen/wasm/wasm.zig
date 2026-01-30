//! WebAssembly Code Generation Package
//!
//! This package provides WebAssembly code generation following Go's architecture:
//!
//! - constants.zig: Instructions and registers (like Go's a.out.go)
//! - prog.zig: Instruction chain structure (like Go's obj.Prog)
//! - preprocess.zig: High-level to low-level transform (like Go's wasmobj.go preprocess)
//! - assemble.zig: Prog to bytes (like Go's wasmobj.go assemble)
//! - link.zig: Module assembly (like Go's asm.go)
//!
//! Go reference files:
//! - cmd/compile/internal/wasm/ssa.go (595 lines)
//! - cmd/internal/obj/wasm/a.out.go (342 lines)
//! - cmd/internal/obj/wasm/wasmobj.go (1453 lines)
//! - cmd/link/internal/wasm/asm.go (707 lines)

pub const constants = @import("constants.zig");
pub const prog = @import("prog.zig");
pub const preprocess = @import("preprocess.zig");
pub const assemble = @import("assemble.zig");
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

test {
    @import("std").testing.refAllDecls(@This());
}
