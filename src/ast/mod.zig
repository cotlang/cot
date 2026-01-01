//! AST Module - Modern Abstract Syntax Tree
//!
//! Structure of Arrays (SoA) based AST with StringInterner integration.
//! All strings are deduplicated via StringId references.
//!
//! ## Architecture
//!
//! - **NodeStore**: Central storage using parallel arrays for cache efficiency
//! - **Typed Indices**: StmtIdx, ExprIdx, TypeIdx prevent accidental mixing
//! - **Views**: Structured access to packed NodeData
//! - **Scratch Buffers**: Mark/commit/rollback for incremental parsing
//!
//! ## Usage
//!
//! ```zig
//! var interner = StringInterner.init(allocator);
//! defer interner.deinit();
//!
//! var store = NodeStore.init(allocator, &interner);
//! defer store.deinit();
//!
//! const name = try interner.intern("x");
//! const id_expr = try store.addIdentifier(name, .zero);
//! const lit_expr = try store.addIntLiteral(42, .zero);
//! const bin_expr = try store.addBinary(id_expr, .add, lit_expr, .zero);
//!
//! // Use views for structured access
//! const view = BinaryExprView.from(&store, bin_expr);
//! ```

const std = @import("std");

// Re-export typed indices
pub const indices = @import("indices.zig");
pub const StmtIdx = indices.StmtIdx;
pub const ExprIdx = indices.ExprIdx;
pub const TypeIdx = indices.TypeIdx;
pub const ExtraIdx = indices.ExtraIdx;
pub const ExtraSpan = indices.ExtraSpan;

// Re-export tag enums
pub const statements = @import("statements.zig");
pub const StatementTag = statements.StatementTag;

pub const expressions = @import("expressions.zig");
pub const ExpressionTag = expressions.ExpressionTag;
pub const BinaryOp = expressions.BinaryOp;
pub const UnaryOp = expressions.UnaryOp;

pub const types = @import("types.zig");
pub const TypeTag = types.TypeTag;
pub const Mutability = types.Mutability;

// Re-export NodeStore and data types
pub const node_store = @import("node_store.zig");
pub const NodeStore = node_store.NodeStore;
pub const NodeData = node_store.NodeData;
pub const SourceLoc = node_store.SourceLoc;

// Re-export view structs
pub const views = @import("views.zig");

// Expression views
pub const BinaryExprView = views.BinaryExprView;
pub const UnaryExprView = views.UnaryExprView;
pub const IdentifierView = views.IdentifierView;
pub const IntLiteralView = views.IntLiteralView;
pub const FloatLiteralView = views.FloatLiteralView;
pub const StringLiteralView = views.StringLiteralView;
pub const BoolLiteralView = views.BoolLiteralView;
pub const MemberView = views.MemberView;
pub const IndexView = views.IndexView;
pub const CallView = views.CallView;
pub const GroupingView = views.GroupingView;

// Statement views
pub const ExprStmtView = views.ExprStmtView;
pub const AssignmentView = views.AssignmentView;
pub const LetDeclView = views.LetDeclView;
pub const ConstDeclView = views.ConstDeclView;
pub const ReturnView = views.ReturnView;
pub const BlockView = views.BlockView;
pub const IfStmtView = views.IfStmtView;
pub const WhileStmtView = views.WhileStmtView;
pub const ForStmtView = views.ForStmtView;
pub const LoopStmtView = views.LoopStmtView;
pub const FnDefView = views.FnDefView;
pub const ParamInfo = views.ParamInfo;
pub const ImportView = views.ImportView;

// Type views
pub const ArrayTypeView = views.ArrayTypeView;
pub const SliceTypeView = views.SliceTypeView;
pub const OptionalTypeView = views.OptionalTypeView;
pub const PointerTypeView = views.PointerTypeView;
pub const NamedTypeView = views.NamedTypeView;

// Re-export StringInterner types from base
const base = @import("../base/mod.zig");
pub const StringInterner = base.StringInterner;
pub const StringId = base.StringId;

// Re-export emitter for source code generation
pub const emitter = @import("emitter.zig");
pub const Emitter = emitter.Emitter;
pub const emitToString = emitter.emitToString;

// Re-export visitor for AST traversal
pub const visitor = @import("visitor.zig");
pub const WalkResult = visitor.WalkResult;
pub const ExprVisitor = visitor.ExprVisitor;
pub const StmtVisitor = visitor.StmtVisitor;
pub const walkExpr = visitor.walkExpr;
pub const walkStmt = visitor.walkStmt;
pub const walkAllStmts = visitor.walkAllStmts;
pub const NodeCounter = visitor.NodeCounter;

test {
    // Run all tests from submodules
    std.testing.refAllDecls(@This());
}
