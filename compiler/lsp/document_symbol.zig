//! textDocument/documentSymbol — outline of declarations in a file.

const std = @import("std");
const ast_mod = @import("../frontend/ast.zig");
const lsp_types = @import("types.zig");
const analysis = @import("analysis.zig");

const Ast = ast_mod.Ast;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Decl = ast_mod.Decl;
const Node = ast_mod.Node;
const AnalysisResult = analysis.AnalysisResult;
const DocumentSymbol = lsp_types.DocumentSymbol;
const SymbolKind = lsp_types.SymbolKind;
const Range = lsp_types.Range;

/// Build document symbols from the analysis result.
pub fn getDocumentSymbols(allocator: std.mem.Allocator, result: *AnalysisResult) ![]DocumentSymbol {
    var symbols = std.ArrayListUnmanaged(DocumentSymbol){};

    for (result.tree.getRootDecls()) |decl_idx| {
        const node = result.tree.getNode(decl_idx) orelse continue;
        const decl = node.asDecl() orelse continue;

        const sym = declToSymbol(allocator, &result.tree, result.src.content, decl) catch continue;
        if (sym) |s| try symbols.append(allocator, s);
    }

    return symbols.toOwnedSlice(allocator);
}

fn declToSymbol(allocator: std.mem.Allocator, tree: *const Ast, content: []const u8, decl: Decl) !?DocumentSymbol {
    switch (decl) {
        .fn_decl => |f| {
            const range = lsp_types.spanToRange(content, f.span);
            // Selection range is just the name (approximate: use span start)
            return DocumentSymbol{
                .name = f.name,
                .kind = .function,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .var_decl => |v| {
            const range = lsp_types.spanToRange(content, v.span);
            return DocumentSymbol{
                .name = v.name,
                .kind = if (v.is_const) .constant else .variable,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .struct_decl => |s| {
            const range = lsp_types.spanToRange(content, s.span);
            // Build field children
            var children = std.ArrayListUnmanaged(DocumentSymbol){};
            for (s.fields) |field| {
                const field_range = lsp_types.spanToRange(content, field.span);
                try children.append(allocator, .{
                    .name = field.name,
                    .kind = .field,
                    .range = field_range,
                    .selection_range = field_range,
                    .children = &.{},
                });
            }
            return DocumentSymbol{
                .name = s.name,
                .kind = .@"struct",
                .range = range,
                .selection_range = range,
                .children = children.toOwnedSlice(allocator) catch &.{},
            };
        },
        .enum_decl => |e| {
            const range = lsp_types.spanToRange(content, e.span);
            var children = std.ArrayListUnmanaged(DocumentSymbol){};
            for (e.variants) |variant| {
                const v_range = lsp_types.spanToRange(content, variant.span);
                try children.append(allocator, .{
                    .name = variant.name,
                    .kind = .@"enum",
                    .range = v_range,
                    .selection_range = v_range,
                    .children = &.{},
                });
            }
            return DocumentSymbol{
                .name = e.name,
                .kind = .@"enum",
                .range = range,
                .selection_range = range,
                .children = children.toOwnedSlice(allocator) catch &.{},
            };
        },
        .impl_block => |ib| {
            const range = lsp_types.spanToRange(content, ib.span);
            var children = std.ArrayListUnmanaged(DocumentSymbol){};
            for (ib.methods) |method_idx| {
                const method_node = tree.getNode(method_idx) orelse continue;
                const method_decl = method_node.asDecl() orelse continue;
                if (method_decl == .fn_decl) {
                    const m = method_decl.fn_decl;
                    const m_range = lsp_types.spanToRange(content, m.span);
                    try children.append(allocator, .{
                        .name = m.name,
                        .kind = .method,
                        .range = m_range,
                        .selection_range = m_range,
                        .children = &.{},
                    });
                }
            }
            return DocumentSymbol{
                .name = ib.type_name,
                .kind = .class,
                .range = range,
                .selection_range = range,
                .children = children.toOwnedSlice(allocator) catch &.{},
            };
        },
        .trait_decl => |t| {
            const range = lsp_types.spanToRange(content, t.span);
            var children = std.ArrayListUnmanaged(DocumentSymbol){};
            for (t.methods) |method_idx| {
                const method_node = tree.getNode(method_idx) orelse continue;
                const method_decl = method_node.asDecl() orelse continue;
                if (method_decl == .fn_decl) {
                    const m = method_decl.fn_decl;
                    const m_range = lsp_types.spanToRange(content, m.span);
                    try children.append(allocator, .{
                        .name = m.name,
                        .kind = .method,
                        .range = m_range,
                        .selection_range = m_range,
                        .children = &.{},
                    });
                }
            }
            return DocumentSymbol{
                .name = t.name,
                .kind = .interface,
                .range = range,
                .selection_range = range,
                .children = children.toOwnedSlice(allocator) catch &.{},
            };
        },
        .impl_trait => |it| {
            const range = lsp_types.spanToRange(content, it.span);
            var children = std.ArrayListUnmanaged(DocumentSymbol){};
            for (it.methods) |method_idx| {
                const method_node = tree.getNode(method_idx) orelse continue;
                const method_decl = method_node.asDecl() orelse continue;
                if (method_decl == .fn_decl) {
                    const m = method_decl.fn_decl;
                    const m_range = lsp_types.spanToRange(content, m.span);
                    try children.append(allocator, .{
                        .name = m.name,
                        .kind = .method,
                        .range = m_range,
                        .selection_range = m_range,
                        .children = &.{},
                    });
                }
            }
            return DocumentSymbol{
                .name = it.target_type,
                .kind = .class,
                .range = range,
                .selection_range = range,
                .children = children.toOwnedSlice(allocator) catch &.{},
            };
        },
        .import_decl => |i| {
            const range = lsp_types.spanToRange(content, i.span);
            return DocumentSymbol{
                .name = i.path,
                .kind = .module,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .error_set_decl => |e| {
            const range = lsp_types.spanToRange(content, e.span);
            return DocumentSymbol{
                .name = e.name,
                .kind = .@"enum",
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .test_decl => |t| {
            const range = lsp_types.spanToRange(content, t.span);
            return DocumentSymbol{
                .name = t.name,
                .kind = .function,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .bench_decl => |b| {
            const range = lsp_types.spanToRange(content, b.span);
            return DocumentSymbol{
                .name = b.name,
                .kind = .function,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .type_alias => |ta| {
            const range = lsp_types.spanToRange(content, ta.span);
            return DocumentSymbol{
                .name = ta.name,
                .kind = .type_parameter,
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .union_decl => |u| {
            const range = lsp_types.spanToRange(content, u.span);
            return DocumentSymbol{
                .name = u.name,
                .kind = .@"enum",
                .range = range,
                .selection_range = range,
                .children = &.{},
            };
        },
        .bad_decl => return null,
    }
}
