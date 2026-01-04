//! Builtin Function Definitions
//!
//! Central registry of builtin functions for the bytecode emitter.
//! Defines which functions have dedicated opcodes vs routing through native calls.
//!
//! Categories:
//! - opcode_builtins: Performance-critical functions with dedicated opcodes
//! - io_functions: Core I/O functions (println, print)
//! - native_functions: Functions implemented in Zig, called via xcall

const std = @import("std");
const cot_runtime = @import("cot_runtime");
const Opcode = cot_runtime.bytecode.opcodes.Opcode;

/// Builtin function definition for performance-critical opcodes
pub const BuiltinDef = struct {
    opcode: Opcode,
    min_args: u8,
    max_args: u8,
};

/// Performance-critical functions that have dedicated opcodes
/// Everything else routes through xcall to native functions
pub const opcode_builtins = std.StaticStringMap(BuiltinDef).initComptime(.{
    // Only keep opcodes for extremely high-volume operations
    .{ "trim", BuiltinDef{ .opcode = .str_trim, .min_args = 1, .max_args = 1 } },
    .{ "atrim", BuiltinDef{ .opcode = .str_trim, .min_args = 1, .max_args = 1 } },
    .{ "len", BuiltinDef{ .opcode = .str_len, .min_args = 1, .max_args = 1 } },
    .{ "size", BuiltinDef{ .opcode = .fn_size, .min_args = 1, .max_args = 1 } },
});

/// Core Cot I/O functions with dedicated opcodes (no return value)
pub const io_functions = std.StaticStringMap(Opcode).initComptime(.{
    .{ "println", .println },
    .{ "print", .print },
});

/// Native functions - these are implemented in Zig and called via xcall
/// All other "builtin" functions now route through this mechanism
pub const native_functions = std.StaticStringMap(void).initComptime(.{
    // Type conversion
    .{ "string", {} },
    .{ "integer", {} },
    .{ "decimal", {} },
    .{ "char", {} },
    .{ "alpha", {} },
    .{ "boolean", {} },
    // String operations
    .{ "instr", {} },
    .{ "str_delete_last", {} },
    .{ "upper", {} },
    .{ "lower", {} },
    .{ "ltrim", {} },
    // Math functions
    .{ "abs", {} },
    .{ "sqrt", {} },
    .{ "sin", {} },
    .{ "cos", {} },
    .{ "tan", {} },
    .{ "log", {} },
    .{ "log10", {} },
    .{ "exp", {} },
    .{ "round", {} },
    .{ "trunc", {} },
    // Date/time
    .{ "date", {} },
    .{ "time", {} },
    // System
    .{ "error", {} },
    .{ "mem", {} },
    // File I/O (modern Cot core API)
    .{ "file.open", {} },
    .{ "file.close", {} },
    .{ "file.readline", {} },
    .{ "file.readall", {} },
    .{ "file.writeline", {} },
    .{ "file.write", {} },
    .{ "file.eof", {} },
    .{ "file.flush", {} },
});

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a function name is a builtin with a dedicated opcode
pub fn isOpcodeBuiltin(name: []const u8) bool {
    return opcode_builtins.has(name);
}

/// Check if a function name is a core I/O function
pub fn isIoFunction(name: []const u8) bool {
    return io_functions.has(name);
}

/// Check if a function name is a native function
pub fn isNativeFunction(name: []const u8) bool {
    return native_functions.has(name);
}

/// Check if a function name is any kind of builtin
pub fn isBuiltin(name: []const u8) bool {
    return isOpcodeBuiltin(name) or isIoFunction(name) or isNativeFunction(name);
}

/// Get the opcode for an I/O function
pub fn getIoOpcode(name: []const u8) ?Opcode {
    return io_functions.get(name);
}

/// Get the builtin definition for an opcode-based builtin
pub fn getOpcodeBuiltin(name: []const u8) ?BuiltinDef {
    return opcode_builtins.get(name);
}
