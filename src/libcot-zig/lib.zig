//! libcot — Cot compiler frontend library.
//!
//! Parses Cot source files, type-checks them, and lowers to IR instructions.
//! This is the language-specific layer — it knows about Cot syntax, @safe mode,
//! string interpolation, traits, actors, and all Cot-specific semantics.
//!
//! The output (IR instructions) feeds into libcir for SSA construction,
//! optimization, and code emission.

pub const foundation = @import("foundation");
pub const cir = @import("cir");

pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");
pub const checker = @import("checker.zig");
pub const lower = @import("lower.zig");
pub const formatter = @import("formatter.zig");
pub const scanner = @import("scanner.zig");
pub const token = @import("token.zig");
pub const errors = @import("errors.zig");
pub const comptime_val = @import("comptime.zig");

test {
    _ = foundation;
    _ = cir;
    _ = ast;
    _ = parser;
    _ = checker;
    _ = lower;
    _ = formatter;
    _ = scanner;
    _ = token;
    _ = errors;
    _ = comptime_val;
}
