//! libts — TypeScript/JavaScript frontend for Cot.
//!
//! Parses .ts/.js/.tsx/.jsx files and produces TS AST nodes
//! that are transformed to Cot AST by the driver.

pub const token = @import("token.zig");
pub const scanner = @import("scanner.zig");
pub const parser = @import("parser.zig");
pub const ts_ast = @import("ts_ast.zig");
pub const source = @import("source.zig");
pub const debug = @import("debug.zig");

pub const Token = token.Token;
pub const Scanner = scanner.Scanner;
pub const Parser = parser.Parser;
pub const Ast = ts_ast.Ast;
pub const Source = source.Source;
pub const Pos = source.Pos;
pub const Span = source.Span;
pub const NodeIndex = ts_ast.NodeIndex;
pub const null_node = ts_ast.null_node;
