//! zig/libclif — Native backend for the Cot compiler.
//!
//! Reads CIR bytes and compiles them to a native .o file using the
//! hand-ported Cranelift backend. Implements the same clif.h C ABI
//! as rust/libclif (real Cranelift).
//!
//! This library is self-contained — it does NOT import from libcot.

pub const compile_mod = @import("compile.zig");
pub const clif = @import("clif_ir/mod.zig");
pub const object_module_mod = @import("object_module.zig");
pub const dwarf_mod = @import("dwarf.zig");
pub const macho_mod = @import("macho.zig");
pub const elf_mod = @import("elf.zig");
pub const types_mod = @import("types.zig");
pub const target_mod = @import("target.zig");
pub const debug_mod = @import("debug.zig");
