//! libcot — Cot compiler frontend library.
//!
//! Parses Cot source files, type-checks them, and lowers to IR instructions.
//! This is the language-specific layer — it knows about Cot syntax, @safe mode,
//! string interpolation, traits, actors, and all Cot-specific semantics.
//!
//! The output (IR instructions) feeds into libcir for SSA construction,
//! optimization, and code emission.

pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");
pub const checker = @import("checker.zig");
pub const types = @import("types.zig");
pub const ir = @import("ir.zig");
pub const lower = @import("lower.zig");
pub const formatter = @import("formatter.zig");

pub const token = @import("token.zig");
pub const source = @import("source.zig");
pub const scanner = @import("scanner.zig");
pub const errors = @import("errors.zig");
pub const target = @import("target.zig");
pub const debug = @import("debug.zig");
pub const comptime_mod = @import("comptime.zig");

test {
    _ = ast;
    _ = parser;
    _ = checker;
    _ = types;
    _ = ir;
    _ = lower;
    _ = formatter;
    _ = token;
    _ = source;
    _ = scanner;
    _ = errors;
    _ = target;
    _ = debug;
    _ = comptime_mod;
}
