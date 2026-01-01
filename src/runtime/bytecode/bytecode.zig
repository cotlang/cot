//! Cot Bytecode Module
//!
//! This module provides the bytecode format, VM, and disassembler
//! for executing compiled Cot programs.
//!
//! Bytecode compilation is handled by the IR emitter (ir/emit_bytecode.zig).

pub const opcodes = @import("opcodes.zig");
pub const module = @import("module.zig");
pub const vm = @import("vm.zig");
pub const disasm = @import("disasm.zig");

// Re-export commonly used types
pub const Opcode = opcodes.Opcode;
pub const OpenModeFlags = opcodes.OpenModeFlags;
pub const MatchMode = opcodes.MatchMode;

pub const Module = module.Module;
pub const ModuleHeader = module.ModuleHeader;
pub const Library = module.Library;
pub const Constant = module.Constant;
pub const ConstantTag = module.ConstantTag;
pub const TypeDef = module.TypeDef;
pub const FieldDef = module.FieldDef;
pub const RoutineDef = module.RoutineDef;
pub const DataTypeCode = module.DataTypeCode;

pub const VM = vm.VM;
pub const VMError = vm.VMError;
pub const Value = vm.Value;

pub const Disassembler = disasm.Disassembler;
pub const disassemble = disasm.disassemble;

test {
    @import("std").testing.refAllDecls(@This());
}
