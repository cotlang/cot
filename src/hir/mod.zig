//! HIR Module - High-level Intermediate Representation
//!
//! HIR sits between the AST and MIR in the compilation pipeline:
//!   Source -> AST -> HIR -> MIR -> Bytecode
//!
//! ## Purpose
//!
//! HIR provides a typed, desugared representation that simplifies MIR lowering.
//! After HIR lowering:
//! - All types are resolved (no inference needed)
//! - Control flow is desugared (for/while -> loop)
//! - Compound assignments are desugared (x += 1 -> x = x + 1)
//! - Variable references are resolved to declarations
//!
//! ## Usage
//!
//! ```zig
//! const hir = @import("hir/mod.zig");
//!
//! // Lower AST to HIR
//! var lowerer = hir.Lowerer.init(allocator, ast_store);
//! const module = try lowerer.lower();
//!
//! // Verify HIR
//! var verifier = hir.Verifier.init(allocator, module);
//! try verifier.verify();
//! ```

const std = @import("std");

// Re-export core types
pub const hir = @import("hir.zig");
pub const Type = hir.Type;
pub const Expr = hir.Expr;
pub const ExprKind = hir.ExprKind;
pub const BinaryOp = hir.BinaryOp;
pub const UnaryOp = hir.UnaryOp;
pub const Stmt = hir.Stmt;
pub const VarDecl = hir.VarDecl;
pub const Function = hir.Function;
pub const StructDef = hir.StructDef;
pub const Module = hir.Module;
pub const SourceLoc = hir.SourceLoc;

// I/O types
pub const IoOpen = hir.IoOpen;
pub const IoClose = hir.IoClose;
pub const IoRead = hir.IoRead;
pub const IoWrite = hir.IoWrite;

test {
    std.testing.refAllDecls(@This());
}
