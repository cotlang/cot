//! textDocument/references — find all references to a symbol.
//! Supports cross-file references: scans both the current file and imported deps.

const std = @import("std");
const ast_mod = @import("../frontend/ast.zig");
const checker_mod = @import("../frontend/checker.zig");
const lsp_types = @import("types.zig");
const analysis = @import("analysis.zig");
const hover_mod = @import("hover.zig");

const Ast = ast_mod.Ast;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Node = ast_mod.Node;
const Expr = ast_mod.Expr;
const Decl = ast_mod.Decl;
const Stmt = ast_mod.Stmt;
const Scope = checker_mod.Scope;
const AnalysisResult = analysis.AnalysisResult;
const Location = lsp_types.Location;

/// Find all references to the symbol at the given byte offset.
/// Scans the current file and all imported dependency files.
pub fn getReferences(
    allocator: std.mem.Allocator,
    result: *AnalysisResult,
    byte_offset: u32,
    uri: []const u8,
    include_decl: bool,
) ![]Location {
    // Find identifier at offset
    const node_idx = hover_mod.findNodeAtOffset(&result.tree, byte_offset) orelse return &.{};
    const node = result.tree.getNode(node_idx) orelse return &.{};

    // Get the symbol name
    const name = getNodeName(node) orelse return &.{};

    // Look up in scope to get the definition node
    const sym = result.global_scope.lookup(name) orelse return &.{};
    const def_node_idx = sym.node;

    // Walk all nodes in current file and collect references
    var locations = std.ArrayListUnmanaged(Location){};

    for (result.tree.nodes.items, 0..) |n, i| {
        const idx: NodeIndex = @intCast(i);
        const ref_name = getNodeName(n) orelse continue;
        if (!std.mem.eql(u8, ref_name, name)) continue;

        // Check if this is the declaration — skip if !include_decl
        if (idx == def_node_idx and !include_decl) continue;

        const span = n.span();
        try locations.append(allocator, .{
            .uri = uri,
            .range = lsp_types.spanToRange(result.src.content, span),
        });
    }

    // Scan dependency files for references
    for (result.dep_files) |dep| {
        for (dep.tree.nodes.items) |n| {
            const ref_name = getNodeName(n) orelse continue;
            if (!std.mem.eql(u8, ref_name, name)) continue;
            const span = n.span();
            try locations.append(allocator, .{
                .uri = dep.uri,
                .range = lsp_types.spanToRange(dep.src.content, span),
            });
        }
    }

    return locations.toOwnedSlice(allocator);
}

/// Extract the identifier name from a node, if it names a symbol.
fn getNodeName(node: Node) ?[]const u8 {
    switch (node) {
        .expr => |e| switch (e) {
            .ident => |ident| return ident.name,
            else => return null,
        },
        .decl => |d| switch (d) {
            .fn_decl => |f| return f.name,
            .var_decl => |v| return v.name,
            .struct_decl => |s| return s.name,
            .enum_decl => |en| return en.name,
            .trait_decl => |t| return t.name,
            else => return null,
        },
        .stmt => |s| switch (s) {
            .var_stmt => |v| return v.name,
            else => return null,
        },
    }
}
