//! textDocument/hover — type information on hover.

const std = @import("std");
const ast_mod = @import("../frontend/ast.zig");
const types_mod = @import("../frontend/types.zig");
const checker_mod = @import("../frontend/checker.zig");
const lsp_types = @import("types.zig");
const analysis = @import("analysis.zig");

const Ast = ast_mod.Ast;
const NodeIndex = ast_mod.NodeIndex;
const null_node = ast_mod.null_node;
const Node = ast_mod.Node;
const Expr = ast_mod.Expr;
const Decl = ast_mod.Decl;
const Stmt = ast_mod.Stmt;
const Span = @import("../frontend/source.zig").Span;
const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const invalid_type = types_mod.invalid_type;
const Scope = checker_mod.Scope;
const AnalysisResult = analysis.AnalysisResult;
const MarkupContent = lsp_types.MarkupContent;

/// Find the most specific AST node containing the given byte offset.
pub fn findNodeAtOffset(tree: *const Ast, offset: u32) ?NodeIndex {
    var best: ?NodeIndex = null;
    var best_len: u32 = std.math.maxInt(u32);

    for (tree.nodes.items, 0..) |node, i| {
        const span = node.span();
        if (span.start.offset <= offset and offset < span.end.offset) {
            const len = span.end.offset - span.start.offset;
            if (len < best_len) {
                best = @intCast(i);
                best_len = len;
            }
        }
    }
    return best;
}

/// Get hover info for the node at the given byte offset.
pub fn getHover(allocator: std.mem.Allocator, result: *AnalysisResult, byte_offset: u32) ?MarkupContent {
    const node_idx = findNodeAtOffset(&result.tree, byte_offset) orelse return null;
    const node = result.tree.getNode(node_idx) orelse return null;

    switch (node) {
        .expr => |expr| return exprHover(allocator, result, node_idx, expr),
        .decl => |decl| return declHover(allocator, result, decl),
        .stmt => |stmt| return stmtHover(allocator, result, stmt),
    }
}

fn exprHover(allocator: std.mem.Allocator, result: *AnalysisResult, node_idx: NodeIndex, expr: Expr) ?MarkupContent {
    switch (expr) {
        .ident => |ident| {
            // Try expr_types first
            if (result.checker) |*checker| {
                if (checker.expr_types.get(node_idx)) |type_idx| {
                    if (type_idx != invalid_type) {
                        const type_name = result.type_reg.typeName(type_idx);
                        const value = std.fmt.allocPrint(allocator, "```cot\n{s}: {s}\n```", .{ ident.name, type_name }) catch return null;
                        return .{ .kind = "markdown", .value = value };
                    }
                }
            }
            // Try scope lookup
            if (result.global_scope.lookup(ident.name)) |sym| {
                const kind_str: []const u8 = switch (sym.kind) {
                    .variable => "var",
                    .constant => "const",
                    .function => "fn",
                    .type_name => "type",
                    .parameter => "param",
                };
                const type_name = result.type_reg.typeName(sym.type_idx);
                const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}: {s}\n```", .{ kind_str, ident.name, type_name }) catch return null;
                return .{ .kind = "markdown", .value = value };
            }
            return null;
        },
        .literal => |lit| {
            const kind_str: []const u8 = switch (lit.kind) {
                .int => "integer",
                .float => "float",
                .string => "string",
                .char => "char",
                .true_lit, .false_lit => "bool",
                .null_lit => "null",
                .undefined_lit => "undefined",
            };
            const value = std.fmt.allocPrint(allocator, "```cot\n{s} literal: {s}\n```", .{ kind_str, lit.value }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        else => {
            // Try expr_types for any expression
            if (result.checker) |*checker| {
                if (checker.expr_types.get(node_idx)) |type_idx| {
                    if (type_idx != invalid_type) {
                        const type_name = result.type_reg.typeName(type_idx);
                        const value = std.fmt.allocPrint(allocator, "```cot\n{s}\n```", .{type_name}) catch return null;
                        return .{ .kind = "markdown", .value = value };
                    }
                }
            }
            return null;
        },
    }
}

fn declHover(allocator: std.mem.Allocator, result: *AnalysisResult, decl: Decl) ?MarkupContent {
    _ = result;
    switch (decl) {
        .fn_decl => |f| {
            // Build signature: fn name(params) -> return_type
            var sig = std.ArrayListUnmanaged(u8){};
            sig.appendSlice(allocator, "```cot\nfn ") catch return null;
            sig.appendSlice(allocator, f.name) catch return null;
            sig.append(allocator, '(') catch return null;
            for (f.params, 0..) |param, i| {
                if (i > 0) sig.appendSlice(allocator, ", ") catch return null;
                sig.appendSlice(allocator, param.name) catch return null;
            }
            sig.appendSlice(allocator, ")\n```") catch return null;
            return .{ .kind = "markdown", .value = sig.toOwnedSlice(allocator) catch return null };
        },
        .var_decl => |v| {
            const keyword: []const u8 = if (v.is_const) "const" else "var";
            const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}\n```", .{ keyword, v.name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .struct_decl => |s| {
            const value = std.fmt.allocPrint(allocator, "```cot\nstruct {s}\n```", .{s.name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .enum_decl => |e| {
            const value = std.fmt.allocPrint(allocator, "```cot\nenum {s}\n```", .{e.name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .trait_decl => |t| {
            const value = std.fmt.allocPrint(allocator, "```cot\ntrait {s}\n```", .{t.name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        else => return null,
    }
}

fn stmtHover(allocator: std.mem.Allocator, result: *AnalysisResult, stmt: Stmt) ?MarkupContent {
    _ = result;
    switch (stmt) {
        .var_stmt => |v| {
            const keyword: []const u8 = if (v.is_const) "const" else "var";
            const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}\n```", .{ keyword, v.name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        else => return null,
    }
}
