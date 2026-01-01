//! Base Module - Shared Utilities for the Cot Compiler
//!
//! This module provides foundational types and utilities used across
//! all compiler stages including lexer, parser, AST, and IR.

const std = @import("std");

// Re-export string interner
pub const string_interner = @import("string_interner.zig");
pub const StringInterner = string_interner.StringInterner;
pub const StringId = string_interner.StringId;

test {
    // Run all tests from submodules
    std.testing.refAllDecls(@This());
}
