//! textDocument/definition — go to definition.
//! Supports cross-file goto-def: if a symbol originates from an imported file,
//! the Location points to the dep file's URI and span.

const std = @import("std");
const ast_mod = @import("../frontend/ast.zig");
const checker_mod = @import("../frontend/checker.zig");
const lsp_types = @import("types.zig");
const analysis = @import("analysis.zig");
const hover_mod = @import("hover.zig");

const Ast = ast_mod.Ast;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Scope = checker_mod.Scope;
const AnalysisResult = analysis.AnalysisResult;
const Location = lsp_types.Location;

/// Get the definition location for the identifier at the given byte offset.
/// Supports cross-file goto: if the symbol is from an imported dep, returns
/// a Location pointing to the dep file.
pub fn getDefinition(result: *AnalysisResult, byte_offset: u32, uri: []const u8) ?Location {
    const node_idx = hover_mod.findNodeAtOffset(&result.tree, byte_offset) orelse return null;
    const node = result.tree.getNode(node_idx) orelse return null;

    // Only handle identifiers
    const ident = switch (node) {
        .expr => |e| switch (e) {
            .ident => |id| id,
            else => return null,
        },
        else => return null,
    };

    // Look up in scope
    const sym = result.global_scope.lookup(ident.name) orelse return null;

    // Check if symbol is from a dependency file
    if (result.symbol_origins.get(ident.name)) |origin| {
        if (origin.dep_idx < result.dep_files.len) {
            const dep = &result.dep_files[origin.dep_idx];
            const def_node = dep.tree.getNode(sym.node) orelse return null;
            const def_span = def_node.span();
            return Location{
                .uri = dep.uri,
                .range = lsp_types.spanToRange(dep.src.content, def_span),
            };
        }
    }

    // Symbol is in the current file
    const def_node = result.tree.getNode(sym.node) orelse return null;
    const def_span = def_node.span();

    return Location{
        .uri = uri,
        .range = lsp_types.spanToRange(result.src.content, def_span),
    };
}
