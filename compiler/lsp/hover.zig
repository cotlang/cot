//! textDocument/hover — type information on hover.
//! Shows full function signatures, struct field expansion, and resolved types.

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
const Type = types_mod.Type;
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
            // Try scope lookup for richer info
            if (result.global_scope.lookup(ident.name)) |sym| {
                return symbolHover(allocator, result, ident.name, sym);
            }
            // Fallback: try expr_types
            if (result.checker) |*checker| {
                if (checker.expr_types.get(node_idx)) |type_idx| {
                    if (type_idx != invalid_type) {
                        const type_name = result.type_reg.typeName(type_idx);
                        const value = std.fmt.allocPrint(allocator, "```cot\n{s}: {s}\n```", .{ ident.name, type_name }) catch return null;
                        return .{ .kind = "markdown", .value = value };
                    }
                }
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

/// Format hover for a resolved symbol — shows full type info.
fn symbolHover(allocator: std.mem.Allocator, result: *AnalysisResult, name: []const u8, sym: checker_mod.Symbol) ?MarkupContent {
    const type_info = result.type_reg.get(sym.type_idx);

    switch (sym.kind) {
        .function => {
            // Show full function signature with param types and return type
            return funcTypeHover(allocator, &result.type_reg, name, type_info);
        },
        .type_name => {
            // Show struct fields or enum variants
            return typeHover(allocator, &result.type_reg, name, type_info);
        },
        .variable, .parameter => {
            const keyword: []const u8 = if (sym.kind == .parameter) "param" else "var";
            const type_name = result.type_reg.typeName(sym.type_idx);

            // If it's a struct type, also show the struct fields
            const resolved = resolveToStruct(&result.type_reg, sym.type_idx);
            if (resolved) |st| {
                return structValueHover(allocator, &result.type_reg, keyword, name, type_name, st);
            }

            const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}: {s}\n```", .{ keyword, name, type_name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .constant => {
            const type_name = result.type_reg.typeName(sym.type_idx);
            if (sym.const_value) |cv| {
                const value = std.fmt.allocPrint(allocator, "```cot\nconst {s}: {s} = {d}\n```", .{ name, type_name, cv }) catch return null;
                return .{ .kind = "markdown", .value = value };
            }
            const value = std.fmt.allocPrint(allocator, "```cot\nconst {s}: {s}\n```", .{ name, type_name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
    }
}

/// Format a function type as a full signature.
fn funcTypeHover(allocator: std.mem.Allocator, reg: *const TypeRegistry, name: []const u8, type_info: Type) ?MarkupContent {
    switch (type_info) {
        .func => |func_type| {
            var sig = std.ArrayListUnmanaged(u8){};
            sig.appendSlice(allocator, "```cot\nfn ") catch return null;
            sig.appendSlice(allocator, name) catch return null;
            sig.append(allocator, '(') catch return null;
            for (func_type.params, 0..) |param, i| {
                if (i > 0) sig.appendSlice(allocator, ", ") catch return null;
                sig.appendSlice(allocator, param.name) catch return null;
                sig.appendSlice(allocator, ": ") catch return null;
                sig.appendSlice(allocator, reg.typeName(param.type_idx)) catch return null;
            }
            sig.appendSlice(allocator, ") ") catch return null;
            sig.appendSlice(allocator, reg.typeName(func_type.return_type)) catch return null;
            sig.appendSlice(allocator, "\n```") catch return null;
            return .{ .kind = "markdown", .value = sig.toOwnedSlice(allocator) catch return null };
        },
        else => {
            const value = std.fmt.allocPrint(allocator, "```cot\nfn {s}\n```", .{name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
    }
}

/// Format a type definition (struct, enum, etc.) with its members.
fn typeHover(allocator: std.mem.Allocator, reg: *const TypeRegistry, name: []const u8, type_info: Type) ?MarkupContent {
    switch (type_info) {
        .struct_type => |st| {
            var sig = std.ArrayListUnmanaged(u8){};
            sig.appendSlice(allocator, "```cot\nstruct ") catch return null;
            sig.appendSlice(allocator, name) catch return null;
            sig.appendSlice(allocator, " {\n") catch return null;
            for (st.fields) |field| {
                sig.appendSlice(allocator, "    ") catch return null;
                sig.appendSlice(allocator, field.name) catch return null;
                sig.appendSlice(allocator, ": ") catch return null;
                sig.appendSlice(allocator, reg.typeName(field.type_idx)) catch return null;
                sig.append(allocator, '\n') catch return null;
            }
            sig.appendSlice(allocator, "}\n```") catch return null;

            // Also show methods if any
            appendMethodList(allocator, reg, name, &sig);

            return .{ .kind = "markdown", .value = sig.toOwnedSlice(allocator) catch return null };
        },
        .enum_type => |et| {
            var sig = std.ArrayListUnmanaged(u8){};
            sig.appendSlice(allocator, "```cot\nenum ") catch return null;
            sig.appendSlice(allocator, name) catch return null;
            sig.appendSlice(allocator, " {\n") catch return null;
            for (et.variants) |v| {
                sig.appendSlice(allocator, "    ") catch return null;
                sig.appendSlice(allocator, v.name) catch return null;
                sig.append(allocator, '\n') catch return null;
            }
            sig.appendSlice(allocator, "}\n```") catch return null;
            return .{ .kind = "markdown", .value = sig.toOwnedSlice(allocator) catch return null };
        },
        else => {
            const value = std.fmt.allocPrint(allocator, "```cot\ntype {s}\n```", .{name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
    }
}

/// Format hover for a variable whose type is a struct — show the struct's fields.
fn structValueHover(allocator: std.mem.Allocator, reg: *const TypeRegistry, keyword: []const u8, name: []const u8, type_name: []const u8, st: types_mod.StructType) ?MarkupContent {
    var sig = std.ArrayListUnmanaged(u8){};
    sig.appendSlice(allocator, "```cot\n") catch return null;
    sig.appendSlice(allocator, keyword) catch return null;
    sig.appendSlice(allocator, " ") catch return null;
    sig.appendSlice(allocator, name) catch return null;
    sig.appendSlice(allocator, ": ") catch return null;
    sig.appendSlice(allocator, type_name) catch return null;
    sig.appendSlice(allocator, "\n```\n\n") catch return null;

    // Show struct fields
    sig.appendSlice(allocator, "```cot\nstruct ") catch return null;
    sig.appendSlice(allocator, type_name) catch return null;
    sig.appendSlice(allocator, " {\n") catch return null;
    for (st.fields) |field| {
        sig.appendSlice(allocator, "    ") catch return null;
        sig.appendSlice(allocator, field.name) catch return null;
        sig.appendSlice(allocator, ": ") catch return null;
        sig.appendSlice(allocator, reg.typeName(field.type_idx)) catch return null;
        sig.append(allocator, '\n') catch return null;
    }
    sig.appendSlice(allocator, "}\n```") catch return null;

    return .{ .kind = "markdown", .value = sig.toOwnedSlice(allocator) catch return null };
}

/// Append a method list section to a hover signature.
fn appendMethodList(allocator: std.mem.Allocator, reg: *const TypeRegistry, type_name: []const u8, sig: *std.ArrayListUnmanaged(u8)) void {
    const methods = reg.method_registry.get(type_name) orelse return;
    if (methods.items.len == 0) return;

    sig.appendSlice(allocator, "\n\n**Methods:**\n```cot\n") catch return;
    for (methods.items) |method| {
        sig.appendSlice(allocator, "fn ") catch return;
        sig.appendSlice(allocator, method.name) catch return;

        // Show method signature
        const mt = reg.get(method.func_type);
        switch (mt) {
            .func => |ft| {
                sig.append(allocator, '(') catch return;
                // Skip self param (first param) in display
                const start: usize = if (ft.params.len > 0) 1 else 0;
                for (ft.params[start..], 0..) |param, i| {
                    if (i > 0) sig.appendSlice(allocator, ", ") catch return;
                    sig.appendSlice(allocator, param.name) catch return;
                    sig.appendSlice(allocator, ": ") catch return;
                    sig.appendSlice(allocator, reg.typeName(param.type_idx)) catch return;
                }
                sig.appendSlice(allocator, ") ") catch return;
                sig.appendSlice(allocator, reg.typeName(ft.return_type)) catch return;
            },
            else => sig.appendSlice(allocator, "(...)") catch return,
        }
        sig.append(allocator, '\n') catch return;
    }
    sig.appendSlice(allocator, "```") catch return;
}

/// Resolve a type index through pointers to find the underlying struct type.
fn resolveToStruct(reg: *const TypeRegistry, type_idx: TypeIndex) ?types_mod.StructType {
    var idx = type_idx;
    // Follow one level of pointer
    const t = reg.get(idx);
    switch (t) {
        .pointer => |ptr| idx = ptr.elem,
        .struct_type => |st| return st,
        else => return null,
    }
    const t2 = reg.get(idx);
    switch (t2) {
        .struct_type => |st| return st,
        else => return null,
    }
}

fn declHover(allocator: std.mem.Allocator, result: *AnalysisResult, decl: Decl) ?MarkupContent {
    switch (decl) {
        .fn_decl => |f| {
            // Look up the function's resolved type for full signature
            if (result.global_scope.lookup(f.name)) |sym| {
                return funcTypeHover(allocator, &result.type_reg, f.name, result.type_reg.get(sym.type_idx));
            }
            // Fallback: build from AST params (no types)
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
            if (result.global_scope.lookup(v.name)) |sym| {
                const type_name = result.type_reg.typeName(sym.type_idx);
                const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}: {s}\n```", .{ keyword, v.name, type_name }) catch return null;
                return .{ .kind = "markdown", .value = value };
            }
            const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}\n```", .{ keyword, v.name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .struct_decl => |s| {
            // Show struct with fields
            if (result.type_reg.lookupByName(s.name)) |type_idx| {
                return typeHover(allocator, &result.type_reg, s.name, result.type_reg.get(type_idx));
            }
            const value = std.fmt.allocPrint(allocator, "```cot\nstruct {s}\n```", .{s.name}) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        .enum_decl => |e| {
            if (result.type_reg.lookupByName(e.name)) |type_idx| {
                return typeHover(allocator, &result.type_reg, e.name, result.type_reg.get(type_idx));
            }
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
    switch (stmt) {
        .var_stmt => |v| {
            const keyword: []const u8 = if (v.is_const) "const" else "var";
            // Try to get resolved type from checker
            if (result.global_scope.lookup(v.name)) |sym| {
                const type_name = result.type_reg.typeName(sym.type_idx);
                const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}: {s}\n```", .{ keyword, v.name, type_name }) catch return null;
                return .{ .kind = "markdown", .value = value };
            }
            const value = std.fmt.allocPrint(allocator, "```cot\n{s} {s}\n```", .{ keyword, v.name }) catch return null;
            return .{ .kind = "markdown", .value = value };
        },
        else => return null,
    }
}
