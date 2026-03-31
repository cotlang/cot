//! TS AST → Cot AST Transform
//!
//! The boundary between libts and libcot. Imports TS AST types from libts
//! (external module) and Cot AST types from libcot (relative imports, same module).

const std = @import("std");
const libts = @import("libts");
const ts_ast = libts.ts_ast;
const cot_ast = @import("frontend/ast.zig");
const cot_token = @import("frontend/token.zig");
const cot_source = @import("frontend/source.zig");

pub fn transformFile(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, filename: []const u8) error{OutOfMemory}!void {
    const sf = ts.source_file orelse return;

    var top_decls = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
    var main_stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};

    for (sf.statements) |stmt_idx| {
        const node = ts.getNode(stmt_idx) orelse continue;
        switch (node.*) {
            .decl => |d| switch (d) {
                .function => |func| {
                    const fn_idx = try transformTsFn(ts, cot, allocator, func);
                    try top_decls.append(allocator, fn_idx);
                },
                .class => |cls| {
                    try transformTsClass(ts, cot, allocator, cls, &top_decls);
                },
                .import_decl => |imp| {
                    // Transform TS import → Cot import_decl
                    // Strip quotes from module specifier and resolve to file path
                    var spec = imp.module_specifier;
                    if (spec.len >= 2 and (spec[0] == '\'' or spec[0] == '"')) {
                        spec = spec[1 .. spec.len - 1];
                    }
                    const import_idx = try cot.addDecl(.{ .import_decl = .{
                        .path = spec,
                        .span = cot_source.Span.zero,
                    } });
                    try top_decls.append(allocator, import_idx);
                },
                .interface => |iface| {
                    try transformTsInterface(ts, cot, allocator, iface, &top_decls);
                },
                .enum_decl => |en| {
                    const enum_idx = try transformTsEnum(ts, cot, allocator, en);
                    try top_decls.append(allocator, enum_idx);
                },
                .variable => |v| {
                    // Top-level const/let → Cot top-level var_decl
                    if (v.declarators.len > 0) {
                        const decl0 = v.declarators[0];
                        const vname = if (ts.getNode(decl0.binding)) |bn| switch (bn.*) {
                            .expr => |e| switch (e) {
                                .ident => |id| id.name,
                                else => @as([]const u8, "_"),
                            },
                            else => "_",
                        } else "_";
                        const type_expr = if (decl0.type_ann != ts_ast.null_node)
                            try transformTsType(ts, cot, allocator, decl0.type_ann)
                        else
                            cot_ast.null_node;
                        const value = if (decl0.init != ts_ast.null_node)
                            try transformTsExpr(ts, cot, allocator, decl0.init)
                        else
                            cot_ast.null_node;
                        const var_idx = try cot.addDecl(.{ .var_decl = .{
                            .name = vname,
                            .type_expr = type_expr,
                            .value = value,
                            .is_const = v.kind == .const_kw,
                            .span = cot_source.Span.zero,
                        } });
                        try top_decls.append(allocator, var_idx);
                    }
                },
                .type_alias => |ta| {
                    const type_node = try transformTsType(ts, cot, allocator, ta.type_node);
                    const alias_idx = try cot.addDecl(.{ .type_alias = .{
                        .name = ta.name,
                        .target = type_node,
                        .span = cot_source.Span.zero,
                    } });
                    try top_decls.append(allocator, alias_idx);
                },
                .export_decl => |exp| {
                    if (exp.declaration != ts_ast.null_node) {
                        const inner = ts.getNode(exp.declaration) orelse continue;
                        switch (inner.*) {
                            .decl => |inner_d| switch (inner_d) {
                                .function => |func| {
                                    var exported_func = func;
                                    exported_func.is_export = true;
                                    const fn_idx = try transformTsFn(ts, cot, allocator, exported_func);
                                    try top_decls.append(allocator, fn_idx);
                                },
                                .class => |cls| {
                                    try transformTsClass(ts, cot, allocator, cls, &top_decls);
                                },
                                .interface => |iface| {
                                    try transformTsInterface(ts, cot, allocator, iface, &top_decls);
                                },
                                .enum_decl => |en| {
                                    const enum_idx = try transformTsEnum(ts, cot, allocator, en);
                                    try top_decls.append(allocator, enum_idx);
                                },
                                .variable => |v| {
                                    if (v.declarators.len > 0) {
                                        const edecl = v.declarators[0];
                                        const vname = if (ts.getNode(edecl.binding)) |bn| switch (bn.*) {
                                            .expr => |e| switch (e) {
                                                .ident => |id| id.name,
                                                else => @as([]const u8, "_"),
                                            },
                                            else => "_",
                                        } else "_";
                                        const type_expr = if (edecl.type_ann != ts_ast.null_node)
                                            try transformTsType(ts, cot, allocator, edecl.type_ann)
                                        else
                                            cot_ast.null_node;
                                        const value = if (edecl.init != ts_ast.null_node)
                                            try transformTsExpr(ts, cot, allocator, edecl.init)
                                        else
                                            cot_ast.null_node;
                                        const var_idx = try cot.addDecl(.{ .var_decl = .{
                                            .name = vname,
                                            .type_expr = type_expr,
                                            .value = value,
                                            .is_const = v.kind == .const_kw,
                                            .span = cot_source.Span.zero,
                                        } });
                                        try top_decls.append(allocator, var_idx);
                                    }
                                },
                                else => {},
                            },
                            .stmt => |s| {
                                const cot_stmt = try transformTsStmt(ts, cot, allocator, s);
                                if (cot_stmt != cot_ast.null_node) {
                                    try main_stmts.append(allocator, cot_stmt);
                                }
                            },
                            else => {},
                        }
                    }
                    // export default expr
                    if (exp.default_expr != ts_ast.null_node) {
                        const inner = ts.getNode(exp.default_expr) orelse continue;
                        switch (inner.*) {
                            .decl => |inner_d| switch (inner_d) {
                                .function => |func| {
                                    var exported_func = func;
                                    exported_func.is_export = true;
                                    const fn_name = exported_func.name orelse "default_export";
                                    exported_func.name = fn_name;
                                    const fn_idx = try transformTsFn(ts, cot, allocator, exported_func);
                                    try top_decls.append(allocator, fn_idx);
                                },
                                .class => |cls| {
                                    try transformTsClass(ts, cot, allocator, cls, &top_decls);
                                },
                                else => {},
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            },
            .stmt => |s| {
                const cot_stmt = try transformTsStmt(ts, cot, allocator, s);
                if (cot_stmt != cot_ast.null_node) {
                    try main_stmts.append(allocator, cot_stmt);
                }
            },
            else => {},
        }
    }

    // Wrap top-level statements in fn main() void { ... }
    // Skip if the user already declared a main function.
    var has_user_main = false;
    for (sf.statements) |stmt_idx| {
        const node = ts.getNode(stmt_idx) orelse continue;
        switch (node.*) {
            .decl => |d| switch (d) {
                .function => |func| {
                    if (func.name != null and std.mem.eql(u8, func.name.?, "main")) has_user_main = true;
                },
                else => {},
            },
            else => {},
        }
    }
    if (main_stmts.items.len > 0 and !has_user_main) {
        const owned_stmts = try allocator.dupe(cot_ast.NodeIndex, main_stmts.items);
        const body = try cot.addExpr(.{ .block_expr = .{
            .stmts = owned_stmts,
            .expr = cot_ast.null_node,
            .span = cot_source.Span.zero,
        } });
        const void_type = try cot.addExpr(.{ .type_expr = .{
            .kind = .{ .named = "void" },
            .span = cot_source.Span.zero,
        } });
        const main_fn = try cot.addDecl(.{ .fn_decl = .{
            .name = "main",
            .params = &.{},
            .return_type = void_type,
            .body = body,
            .is_extern = false,
            .span = cot_source.Span.zero,
        } });
        try top_decls.append(allocator, main_fn);
    }

    cot.file = .{
        .filename = filename,
        .decls = try allocator.dupe(cot_ast.NodeIndex, top_decls.items),
        .span = cot_source.Span.zero,
        .safe_mode = true, // TS classes use this→self, which requires @safe mode
    };
}

fn transformTsFn(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, func: ts_ast.Decl.FunctionDecl) error{OutOfMemory}!cot_ast.NodeIndex {
    const name = func.name orelse "anonymous";
    const params = try transformTsParams(ts, cot, allocator, func.params);
    const return_type = if (func.return_type != ts_ast.null_node)
        try transformTsType(ts, cot, allocator, func.return_type)
    else
        try cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "void" }, .span = cot_source.Span.zero } });

    const body = if (func.body != ts_ast.null_node)
        try transformTsBlock(ts, cot, allocator, func.body)
    else
        cot_ast.null_node;

    // Extract generic type parameter names: <T, U> → ["T", "U"]
    const type_params = try extractTypeParamNames(ts, allocator, func.type_params);

    return cot.addDecl(.{ .fn_decl = .{
        .name = name,
        .type_params = type_params,
        .params = params,
        .return_type = return_type,
        .body = body,
        .is_extern = false,
        .is_export = func.is_export,
        .is_async = func.is_async,
        .span = cot_source.Span.zero,
    } });
}

/// Transform TS class → Cot struct_decl + impl_block.
/// Emits both the struct (fields) and the impl (methods) as separate top-level decls.
fn transformTsClass(
    ts: *const ts_ast.Ast,
    cot: *cot_ast.Ast,
    allocator: std.mem.Allocator,
    cls: ts_ast.Decl.ClassDecl,
    top_decls: *std.ArrayListUnmanaged(cot_ast.NodeIndex),
) error{OutOfMemory}!void {
    const class_name = cls.name orelse "AnonymousClass";

    // Collect fields from property declarations.
    // If class extends a base, find and copy base fields first.
    var fields = std.ArrayListUnmanaged(cot_ast.Field){};
    var methods = std.ArrayListUnmanaged(cot_ast.NodeIndex){};

    if (cls.extends != ts_ast.null_node) {
        // Find base class name and copy its fields
        const base_name = if (ts.getNode(cls.extends)) |bn| switch (bn.*) {
            .expr => |e| switch (e) {
                .ident => |id| id.name,
                else => @as(?[]const u8, null),
            },
            else => null,
        } else null;
        if (base_name) |bname| {
            // Search TS AST for the base class declaration to copy fields
            if (ts.source_file) |sf| {
                for (sf.statements) |s_idx| {
                    const s_node = ts.getNode(s_idx) orelse continue;
                    switch (s_node.*) {
                        .decl => |d| switch (d) {
                            .class => |base_cls| {
                                if (base_cls.name != null and std.mem.eql(u8, base_cls.name.?, bname)) {
                                    for (base_cls.members) |bm_idx| {
                                        const bm_node = ts.getNode(bm_idx) orelse continue;
                                        switch (bm_node.*) {
                                            .class_member => |bcm| switch (bcm) {
                                                .property => |bp| {
                                                    const bp_name = if (ts.getNode(bp.name)) |n| switch (n.*) {
                                                        .expr => |e| switch (e) { .ident => |id| id.name, else => "_" },
                                                        else => "_",
                                                    } else "_";
                                                    const bp_type = if (bp.type_ann != ts_ast.null_node)
                                                        try transformTsType(ts, cot, allocator, bp.type_ann)
                                                    else
                                                        cot_ast.null_node;
                                                    try fields.append(allocator, .{
                                                        .name = bp_name,
                                                        .type_expr = bp_type,
                                                        .default_value = cot_ast.null_node,
                                                        .span = cot_source.Span.zero,
                                                    });
                                                },
                                                .method => |bm| {
                                                    // Inherit base method into child impl
                                                    const bm_name = if (ts.getNode(bm.name)) |n| switch (n.*) {
                                                        .expr => |e| switch (e) { .ident => |id| id.name, else => "method" },
                                                        else => "method",
                                                    } else "method";
                                                    const bm_params = try transformTsParams(ts, cot, allocator, bm.params);
                                                    const params_with_self = if (!bm.is_static)
                                                        try prependSelfParam(cot, allocator, class_name, bm_params)
                                                    else
                                                        bm_params;
                                                    const bm_ret = if (bm.return_type != ts_ast.null_node)
                                                        try transformTsType(ts, cot, allocator, bm.return_type)
                                                    else
                                                        try cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "void" }, .span = cot_source.Span.zero } });
                                                    const bm_body = if (bm.body != ts_ast.null_node)
                                                        try transformTsBlock(ts, cot, allocator, bm.body)
                                                    else
                                                        cot_ast.null_node;
                                                    const fn_decl = try cot.addDecl(.{ .fn_decl = .{
                                                        .name = bm_name,
                                                        .params = params_with_self,
                                                        .return_type = bm_ret,
                                                        .body = bm_body,
                                                        .is_extern = false,
                                                        .is_async = bm.is_async,
                                                        .is_static = bm.is_static,
                                                        .span = cot_source.Span.zero,
                                                    } });
                                                    try methods.append(allocator, fn_decl);
                                                },
                                                else => {},
                                            },
                                            else => {},
                                        }
                                    }
                                }
                            },
                            else => {},
                        },
                        else => {},
                    }
                }
            }
        }
    }

    for (cls.members) |member_idx| {
        const member_node = ts.getNode(member_idx) orelse continue;
        switch (member_node.*) {
            .class_member => |cm| switch (cm) {
                .property => |prop| {
                    // Get property name
                    const prop_name = if (ts.getNode(prop.name)) |n| switch (n.*) {
                        .expr => |e| switch (e) {
                            .ident => |id| id.name,
                            else => "_",
                        },
                        else => "_",
                    } else "_";
                    const type_expr = if (prop.type_ann != ts_ast.null_node)
                        try transformTsType(ts, cot, allocator, prop.type_ann)
                    else
                        cot_ast.null_node;
                    const default_val = if (prop.init != ts_ast.null_node)
                        try transformTsExpr(ts, cot, allocator, prop.init)
                    else
                        cot_ast.null_node;
                    try fields.append(allocator, .{
                        .name = prop_name,
                        .type_expr = type_expr,
                        .default_value = default_val,
                        .span = cot_source.Span.zero,
                    });
                },
                .constructor => |ctor| {
                    // constructor → init method. Inject self param (safe mode requires it at parse time).
                    const user_params = try transformTsParams(ts, cot, allocator, ctor.params);
                    const params = try prependSelfParam(cot, allocator, class_name, user_params);
                    const body = if (ctor.body != ts_ast.null_node)
                        try transformTsBlock(ts, cot, allocator, ctor.body)
                    else
                        cot_ast.null_node;
                    const void_type = try cot.addExpr(.{ .type_expr = .{
                        .kind = .{ .named = "void" },
                        .span = cot_source.Span.zero,
                    } });
                    const init_fn = try cot.addDecl(.{ .fn_decl = .{
                        .name = "init",
                        .params = params,
                        .return_type = void_type,
                        .body = body,
                        .is_extern = false,
                        .span = cot_source.Span.zero,
                    } });
                    try methods.append(allocator, init_fn);
                },
                .method => |method| {
                    const method_name = if (ts.getNode(method.name)) |n| switch (n.*) {
                        .expr => |e| switch (e) {
                            .ident => |id| id.name,
                            else => "method",
                        },
                        else => "method",
                    } else "method";
                    const user_params = try transformTsParams(ts, cot, allocator, method.params);
                    // Inject self param for non-static methods
                    const params = if (!method.is_static)
                        try prependSelfParam(cot, allocator, class_name, user_params)
                    else
                        user_params;
                    const return_type = if (method.return_type != ts_ast.null_node)
                        try transformTsType(ts, cot, allocator, method.return_type)
                    else
                        try cot.addExpr(.{ .type_expr = .{
                            .kind = .{ .named = "void" },
                            .span = cot_source.Span.zero,
                        } });
                    const body = if (method.body != ts_ast.null_node)
                        try transformTsBlock(ts, cot, allocator, method.body)
                    else
                        cot_ast.null_node;
                    const fn_decl = try cot.addDecl(.{ .fn_decl = .{
                        .name = method_name,
                        .params = params,
                        .return_type = return_type,
                        .body = body,
                        .is_extern = false,
                        .is_async = method.is_async,
                        .is_static = method.is_static,
                        .span = cot_source.Span.zero,
                    } });
                    try methods.append(allocator, fn_decl);
                },
                else => {},
            },
            else => {},
        }
    }

    // Emit struct declaration
    const owned_fields = try allocator.dupe(cot_ast.Field, fields.items);
    const struct_decl = try cot.addDecl(.{ .struct_decl = .{
        .name = class_name,
        .fields = owned_fields,
        .span = cot_source.Span.zero,
    } });
    try top_decls.append(allocator, struct_decl);

    // Emit impl block (if there are methods)
    if (methods.items.len > 0) {
        const owned_methods = try allocator.dupe(cot_ast.NodeIndex, methods.items);
        const impl_block = try cot.addDecl(.{ .impl_block = .{
            .type_name = class_name,
            .methods = owned_methods,
            .span = cot_source.Span.zero,
        } });
        try top_decls.append(allocator, impl_block);
    }
}

/// Transform TS interface → Cot trait_decl.
fn transformTsInterface(
    ts: *const ts_ast.Ast,
    cot: *cot_ast.Ast,
    allocator: std.mem.Allocator,
    iface: ts_ast.Decl.InterfaceDecl,
    top_decls: *std.ArrayListUnmanaged(cot_ast.NodeIndex),
) error{OutOfMemory}!void {
    var methods = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
    for (iface.members) |member| {
        if (member.kind == .method) {
            const params = try transformTsParams(ts, cot, allocator, member.params);
            const return_type = if (member.return_type != ts_ast.null_node)
                try transformTsType(ts, cot, allocator, member.return_type)
            else
                try cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "void" }, .span = cot_source.Span.zero } });
            const fn_decl = try cot.addDecl(.{ .fn_decl = .{
                .name = member.name orelse "method",
                .params = params,
                .return_type = return_type,
                .body = cot_ast.null_node, // trait methods have no body
                .is_extern = false,
                .span = cot_source.Span.zero,
            } });
            try methods.append(allocator, fn_decl);
        }
    }

    const trait_decl = try cot.addDecl(.{ .trait_decl = .{
        .name = iface.name,
        .methods = try allocator.dupe(cot_ast.NodeIndex, methods.items),
        .span = cot_source.Span.zero,
    } });
    try top_decls.append(allocator, trait_decl);
}

/// Transform TS enum → Cot enum_decl (using const Name = enum { ... } pattern).
fn transformTsEnum(
    ts: *const ts_ast.Ast,
    cot: *cot_ast.Ast,
    allocator: std.mem.Allocator,
    en: ts_ast.Decl.EnumDecl,
) error{OutOfMemory}!cot_ast.NodeIndex {
    var variants = std.ArrayListUnmanaged(cot_ast.EnumVariant){};
    for (en.members) |member| {
        const value = if (member.init != ts_ast.null_node)
            try transformTsExpr(ts, cot, allocator, member.init)
        else
            cot_ast.null_node;
        try variants.append(allocator, .{
            .name = member.name,
            .value = value,
            .span = cot_source.Span.zero,
        });
    }

    return cot.addDecl(.{ .enum_decl = .{
        .name = en.name,
        .backing_type = cot_ast.null_node,
        .variants = try allocator.dupe(cot_ast.EnumVariant, variants.items),
        .span = cot_source.Span.zero,
    } });
}

fn transformTsStmt(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, stmt: ts_ast.Stmt) error{OutOfMemory}!cot_ast.NodeIndex {
    return switch (stmt) {
        .expr_stmt => |es| blk: {
            // Check if this is an assignment expression → emit assign_stmt
            const inner = ts.getNode(es.expr) orelse break :blk cot_ast.null_node;
            switch (inner.*) {
                .expr => |e| switch (e) {
                    .assign => |a| {
                        const target = try transformTsExpr(ts, cot, allocator, a.left);
                        const value = try transformTsExpr(ts, cot, allocator, a.right);
                        const op: cot_token.Token = switch (a.op) {
                            .assign => .assign,
                            .add_assign => .add_assign,
                            .sub_assign => .sub_assign,
                            .mul_assign => .mul_assign,
                            .div_assign => .quo_assign,
                            .rem_assign => .rem_assign,
                            else => .assign,
                        };
                        break :blk cot.addStmt(.{ .assign_stmt = .{
                            .target = target,
                            .value = value,
                            .op = op,
                            .span = cot_source.Span.zero,
                        } });
                    },
                    .update => |u| {
                        // i++ → i = i + 1, i-- → i = i - 1
                        const operand = try transformTsExpr(ts, cot, allocator, u.operand);
                        const one = try cot.addExpr(.{ .literal = .{
                            .kind = .float,
                            .value = "1.0",
                            .span = cot_source.Span.zero,
                        } });
                        const bin_op: cot_token.Token = if (u.op == .increment) .add else .sub;
                        const sum = try cot.addExpr(.{ .binary = .{
                            .op = bin_op,
                            .left = operand,
                            .right = one,
                            .span = cot_source.Span.zero,
                        } });
                        // Need a fresh reference for the target (can't reuse operand NodeIndex for both sides)
                        const target = try transformTsExpr(ts, cot, allocator, u.operand);
                        break :blk cot.addStmt(.{ .assign_stmt = .{
                            .target = target,
                            .value = sum,
                            .op = .assign,
                            .span = cot_source.Span.zero,
                        } });
                    },
                    else => {},
                },
                else => {},
            }
            const expr = try transformTsExpr(ts, cot, allocator, es.expr);
            break :blk cot.addStmt(.{ .expr_stmt = .{
                .expr = expr,
                .span = cot_source.Span.zero,
            } });
        },
        .return_stmt => |rs| blk: {
            const value = if (rs.value != ts_ast.null_node)
                try transformTsExpr(ts, cot, allocator, rs.value)
            else
                cot_ast.null_node;
            break :blk cot.addStmt(.{ .return_stmt = .{
                .value = value,
                .span = cot_source.Span.zero,
            } });
        },
        .if_stmt => |ifs| blk: {
            const cond = try transformTsExpr(ts, cot, allocator, ifs.condition);
            const then_b = try transformTsStmtNode(ts, cot, allocator, ifs.consequent);
            const else_b = if (ifs.alternate != ts_ast.null_node)
                try transformTsStmtNode(ts, cot, allocator, ifs.alternate)
            else
                cot_ast.null_node;
            break :blk cot.addStmt(.{ .if_stmt = .{
                .condition = cond,
                .then_branch = then_b,
                .else_branch = else_b,
                .span = cot_source.Span.zero,
            } });
        },
        .while_stmt => |ws| blk: {
            const cond = try transformTsExpr(ts, cot, allocator, ws.condition);
            const body = try transformTsStmtNode(ts, cot, allocator, ws.body);
            break :blk cot.addStmt(.{ .while_stmt = .{
                .condition = cond,
                .body = body,
                .span = cot_source.Span.zero,
            } });
        },
        .block => |bs| transformTsBlockStmt(ts, cot, allocator, bs),
        .var_stmt => |vs| blk: {
            const inner = ts.getNode(vs.decl) orelse break :blk cot_ast.null_node;
            switch (inner.*) {
                .decl => |d| switch (d) {
                    .variable => |v| {
                        if (v.declarators.len == 0) break :blk cot_ast.null_node;
                        const decl = v.declarators[0];
                        const vname = if (ts.getNode(decl.binding)) |bn| switch (bn.*) {
                            .expr => |e| switch (e) {
                                .ident => |id| id.name,
                                else => "_",
                            },
                            else => "_",
                        } else "_";
                        const type_expr = if (decl.type_ann != ts_ast.null_node)
                            try transformTsType(ts, cot, allocator, decl.type_ann)
                        else
                            cot_ast.null_node;
                        const value = if (decl.init != ts_ast.null_node)
                            try transformTsExpr(ts, cot, allocator, decl.init)
                        else
                            cot_ast.null_node;
                        break :blk cot.addStmt(.{ .var_stmt = .{
                            .name = vname,
                            .type_expr = type_expr,
                            .value = value,
                            .is_const = v.kind == .const_kw,
                            .span = cot_source.Span.zero,
                        } });
                    },
                    else => {},
                },
                else => {},
            }
            break :blk cot_ast.null_node;
        },
        .for_of => |fo| blk: {
            // for (const item of items) → for item in items
            const binding = ts.getNode(fo.left) orelse break :blk cot_ast.null_node;
            var iter_var: []const u8 = "_";
            // Extract variable name from binding (could be var_stmt wrapping a var decl)
            switch (binding.*) {
                .decl => |d| switch (d) {
                    .variable => |v| {
                        if (v.declarators.len > 0) {
                            if (ts.getNode(v.declarators[0].binding)) |bn| switch (bn.*) {
                                .expr => |e| switch (e) {
                                    .ident => |id| {
                                        iter_var = id.name;
                                    },
                                    else => {},
                                },
                                else => {},
                            };
                        }
                    },
                    else => {},
                },
                .expr => |e| switch (e) {
                    .ident => |id| {
                        iter_var = id.name;
                    },
                    else => {},
                },
                else => {},
            }
            const collection = try transformTsExpr(ts, cot, allocator, fo.right);
            const body = try transformTsStmtNode(ts, cot, allocator, fo.body);
            break :blk cot.addStmt(.{ .for_stmt = .{
                .binding = iter_var,
                .iterable = collection,
                .body = body,
                .span = cot_source.Span.zero,
            } });
        },
        .for_stmt => |fs| blk: {
            // for (init; cond; update) → init; while(cond) { body; update; }
            // Emit init as a separate statement, then while loop
            var stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            if (fs.init != ts_ast.null_node) {
                const init = try transformTsStmtNode(ts, cot, allocator, fs.init);
                if (init != cot_ast.null_node) try stmts.append(allocator, init);
            }
            const cond = if (fs.condition != ts_ast.null_node)
                try transformTsExpr(ts, cot, allocator, fs.condition)
            else
                try cot.addExpr(.{ .literal = .{ .kind = .true_lit, .value = "true", .span = cot_source.Span.zero } });

            // Build while body: original body + update
            var body_stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            const inner_body = try transformTsStmtNode(ts, cot, allocator, fs.body);
            if (inner_body != cot_ast.null_node) try body_stmts.append(allocator, inner_body);
            if (fs.update != ts_ast.null_node) {
                const update_node = ts.getNode(fs.update) orelse break :blk cot_ast.null_node;
                switch (update_node.*) {
                    .expr => |e| switch (e) {
                        .update => |u| {
                            const operand = try transformTsExpr(ts, cot, allocator, u.operand);
                            const one = try cot.addExpr(.{ .literal = .{ .kind = .float, .value = "1.0", .span = cot_source.Span.zero } });
                            const bin_op: cot_token.Token = if (u.op == .increment) .add else .sub;
                            const sum = try cot.addExpr(.{ .binary = .{ .op = bin_op, .left = operand, .right = one, .span = cot_source.Span.zero } });
                            const target = try transformTsExpr(ts, cot, allocator, u.operand);
                            const assign = try cot.addStmt(.{ .assign_stmt = .{ .target = target, .value = sum, .op = .assign, .span = cot_source.Span.zero } });
                            try body_stmts.append(allocator, assign);
                        },
                        else => {
                            const update_expr = try transformTsExpr(ts, cot, allocator, fs.update);
                            const update_stmt = try cot.addStmt(.{ .expr_stmt = .{ .expr = update_expr, .span = cot_source.Span.zero } });
                            try body_stmts.append(allocator, update_stmt);
                        },
                    },
                    else => {},
                }
            }
            const while_body = try cot.addExpr(.{ .block_expr = .{
                .stmts = try allocator.dupe(cot_ast.NodeIndex, body_stmts.items),
                .expr = cot_ast.null_node,
                .span = cot_source.Span.zero,
            } });
            const while_stmt = try cot.addStmt(.{ .while_stmt = .{
                .condition = cond,
                .body = while_body,
                .span = cot_source.Span.zero,
            } });
            try stmts.append(allocator, while_stmt);

            // Wrap init + while in a block
            break :blk cot.addExpr(.{ .block_expr = .{
                .stmts = try allocator.dupe(cot_ast.NodeIndex, stmts.items),
                .expr = cot_ast.null_node,
                .span = cot_source.Span.zero,
            } });
        },
        .throw_stmt => |ts_throw| blk: {
            // throw expr → return error.TsError
            _ = ts_throw;
            break :blk cot.addStmt(.{ .return_stmt = .{
                .value = try cot.addExpr(.{ .error_literal = .{
                    .error_name = "TsError",
                    .span = cot_source.Span.zero,
                } }),
                .span = cot_source.Span.zero,
            } });
        },
        .break_stmt => |bs| blk: {
            _ = bs;
            break :blk cot.addStmt(.{ .break_stmt = .{
                .label = null,
                .value = cot_ast.null_node,
                .span = cot_source.Span.zero,
            } });
        },
        .continue_stmt => |cs| blk: {
            _ = cs;
            break :blk cot.addStmt(.{ .continue_stmt = .{
                .label = null,
                .span = cot_source.Span.zero,
            } });
        },
        .do_while => |dw| blk: {
            // do { body } while (cond) → while(true) { body; if (!cond) break; }
            const body = try transformTsStmtNode(ts, cot, allocator, dw.body);
            const cond = try transformTsExpr(ts, cot, allocator, dw.condition);
            const neg_cond = try cot.addExpr(.{ .unary = .{ .op = .lnot, .operand = cond, .span = cot_source.Span.zero } });
            const break_stmt = try cot.addStmt(.{ .break_stmt = .{ .span = cot_source.Span.zero } });
            const if_break = try cot.addStmt(.{ .if_stmt = .{
                .condition = neg_cond,
                .then_branch = break_stmt,
                .else_branch = cot_ast.null_node,
                .span = cot_source.Span.zero,
            } });
            var do_stmts = [_]cot_ast.NodeIndex{ body, if_break };
            const do_body = try cot.addExpr(.{ .block_expr = .{
                .stmts = try allocator.dupe(cot_ast.NodeIndex, &do_stmts),
                .expr = cot_ast.null_node,
                .span = cot_source.Span.zero,
            } });
            const true_lit = try cot.addExpr(.{ .literal = .{ .kind = .true_lit, .value = "true", .span = cot_source.Span.zero } });
            break :blk cot.addStmt(.{ .while_stmt = .{
                .condition = true_lit,
                .body = do_body,
                .span = cot_source.Span.zero,
            } });
        },
        .for_in => |fi| blk: {
            // for (key in obj) → for key in obj.keys() (simplified)
            const binding = ts.getNode(fi.left) orelse break :blk cot_ast.null_node;
            var iter_var: []const u8 = "_";
            switch (binding.*) {
                .decl => |d| switch (d) {
                    .variable => |v| {
                        if (v.declarators.len > 0) {
                            if (ts.getNode(v.declarators[0].binding)) |bn| switch (bn.*) {
                                .expr => |e| switch (e) {
                                    .ident => |id| {
                                        iter_var = id.name;
                                    },
                                    else => {},
                                },
                                else => {},
                            };
                        }
                    },
                    else => {},
                },
                .expr => |e| switch (e) {
                    .ident => |id| {
                        iter_var = id.name;
                    },
                    else => {},
                },
                else => {},
            }
            const collection = try transformTsExpr(ts, cot, allocator, fi.right);
            const body = try transformTsStmtNode(ts, cot, allocator, fi.body);
            break :blk cot.addStmt(.{ .for_stmt = .{
                .binding = iter_var,
                .iterable = collection,
                .body = body,
                .span = cot_source.Span.zero,
            } });
        },
        .empty => cot_ast.null_node, // Empty statement → skip
        .debugger => cot_ast.null_node, // debugger → no-op
        .labeled => |ls| blk: {
            // label: stmt → just the stmt (labels not fully supported yet)
            break :blk transformTsStmtNode(ts, cot, allocator, ls.body);
        },
        .try_stmt => |try_s| blk: {
            // try { body } catch(e) { handler } → body (simplified: inline the try body)
            // Full error union transform requires deeper integration
            break :blk transformTsBlock(ts, cot, allocator, try_s.block);
        },
        .switch_stmt => |sw| blk: {
            const subject = try transformTsExpr(ts, cot, allocator, sw.discriminant);
            var cases = std.ArrayListUnmanaged(cot_ast.SwitchCase){};
            var else_body: cot_ast.NodeIndex = cot_ast.null_node;
            for (sw.cases) |sc| {
                if (sc.@"test" == ts_ast.null_node) {
                    // default case
                    if (sc.consequent.len > 0) {
                        var stmts_arr = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                        for (sc.consequent) |s_idx| {
                            const s = try transformTsStmtNode(ts, cot, allocator, s_idx);
                            if (s != cot_ast.null_node) try stmts_arr.append(allocator, s);
                        }
                        else_body = try cot.addExpr(.{ .block_expr = .{
                            .stmts = try allocator.dupe(cot_ast.NodeIndex, stmts_arr.items),
                            .expr = cot_ast.null_node,
                            .span = cot_source.Span.zero,
                        } });
                    }
                    continue;
                }
                const pattern = try transformTsExpr(ts, cot, allocator, sc.@"test");
                var case_stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                for (sc.consequent) |s_idx| {
                    const s = try transformTsStmtNode(ts, cot, allocator, s_idx);
                    if (s != cot_ast.null_node) try case_stmts.append(allocator, s);
                }
                const case_body = try cot.addExpr(.{ .block_expr = .{
                    .stmts = try allocator.dupe(cot_ast.NodeIndex, case_stmts.items),
                    .expr = cot_ast.null_node,
                    .span = cot_source.Span.zero,
                } });
                try cases.append(allocator, .{
                    .patterns = try allocator.dupe(cot_ast.NodeIndex, &.{pattern}),
                    .capture = "",
                    .body = case_body,
                    .span = cot_source.Span.zero,
                });
            }
            break :blk cot.addExpr(.{ .switch_expr = .{
                .subject = subject,
                .cases = try allocator.dupe(cot_ast.SwitchCase, cases.items),
                .else_body = else_body,
                .span = cot_source.Span.zero,
            } });
        },
        else => cot_ast.null_node,
    };
}

fn transformTsStmtNode(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, idx: ts_ast.NodeIndex) error{OutOfMemory}!cot_ast.NodeIndex {
    const node = ts.getNode(idx) orelse return cot_ast.null_node;
    switch (node.*) {
        .stmt => |s| return transformTsStmt(ts, cot, allocator, s),
        .decl => |d| switch (d) {
            .variable => |v| {
                if (v.declarators.len == 0) return cot_ast.null_node;
                const decl = v.declarators[0];
                const vname = if (ts.getNode(decl.binding)) |bn| switch (bn.*) {
                    .expr => |e| switch (e) {
                        .ident => |id| id.name,
                        else => @as([]const u8, "_"),
                    },
                    else => "_",
                } else "_";
                const type_expr = if (decl.type_ann != ts_ast.null_node)
                    try transformTsType(ts, cot, allocator, decl.type_ann)
                else
                    cot_ast.null_node;
                const value = if (decl.init != ts_ast.null_node)
                    try transformTsExpr(ts, cot, allocator, decl.init)
                else
                    cot_ast.null_node;
                return cot.addStmt(.{ .var_stmt = .{
                    .name = vname,
                    .type_expr = type_expr,
                    .value = value,
                    .is_const = v.kind == .const_kw,
                    .span = cot_source.Span.zero,
                } });
            },
            .function => |func| {
                // Nested function → const name = fn(params) { body }
                // (top-level fn_decl would create a separate symbol the linker can't find)
                const name = func.name orelse "anonymous";
                const params = try transformTsParams(ts, cot, allocator, func.params);
                const return_type = if (func.return_type != ts_ast.null_node)
                    try transformTsType(ts, cot, allocator, func.return_type)
                else
                    cot_ast.null_node;
                const body = if (func.body != ts_ast.null_node)
                    try transformTsBlock(ts, cot, allocator, func.body)
                else
                    cot_ast.null_node;
                const closure = try cot.addExpr(.{ .closure_expr = .{
                    .params = params,
                    .return_type = return_type,
                    .body = body,
                    .span = cot_source.Span.zero,
                } });
                return cot.addStmt(.{ .var_stmt = .{
                    .name = name,
                    .type_expr = cot_ast.null_node,
                    .value = closure,
                    .is_const = true,
                    .span = cot_source.Span.zero,
                } });
            },
            else => return cot_ast.null_node,
        },
        .expr => |e| {
            const expr = try transformTsExprNode(ts, cot, allocator, e);
            return cot.addStmt(.{ .expr_stmt = .{
                .expr = expr,
                .span = cot_source.Span.zero,
            } });
        },
        else => return cot_ast.null_node,
    }
}

fn transformTsBlock(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, idx: ts_ast.NodeIndex) error{OutOfMemory}!cot_ast.NodeIndex {
    const node = ts.getNode(idx) orelse return cot_ast.null_node;
    switch (node.*) {
        .stmt => |s| switch (s) {
            .block => |bs| return transformTsBlockStmt(ts, cot, allocator, bs),
            else => {},
        },
        else => {},
    }
    return cot_ast.null_node;
}

fn transformTsBlockStmt(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, bs: ts_ast.Stmt.BlockStmt) error{OutOfMemory}!cot_ast.NodeIndex {
    var stmts = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
    for (bs.stmts) |stmt_idx| {
        const cot_stmt = try transformTsStmtNode(ts, cot, allocator, stmt_idx);
        if (cot_stmt != cot_ast.null_node) {
            try stmts.append(allocator, cot_stmt);
        }
    }
    const owned = try allocator.dupe(cot_ast.NodeIndex, stmts.items);
    return cot.addExpr(.{ .block_expr = .{
        .stmts = owned,
        .expr = cot_ast.null_node,
        .span = cot_source.Span.zero,
    } });
}

fn transformTsExpr(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, idx: ts_ast.NodeIndex) error{OutOfMemory}!cot_ast.NodeIndex {
    const node = ts.getNode(idx) orelse return cot_ast.null_node;
    switch (node.*) {
        .expr => |e| return transformTsExprNode(ts, cot, allocator, e),
        else => return cot_ast.null_node,
    }
}

fn transformTsExprNode(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, expr: ts_ast.Expr) error{OutOfMemory}!cot_ast.NodeIndex {
    return switch (expr) {
        .ident => |id| cot.addExpr(.{ .ident = .{
            .name = id.name,
            .span = cot_source.Span.zero,
        } }),
        .literal => |lit| blk: {
            // In TS, all numbers are f64. Emit integer-looking literals as floats
            // so Cot's type checker sees them as f64, not int.
            const kind: cot_ast.LiteralKind = switch (lit.kind) {
                .string => .string,
                .number => .float, // TS number = f64, always
                .bigint => .int,
                .boolean => if (std.mem.eql(u8, lit.value, "true")) .true_lit else .false_lit,
                .null_lit => .null_lit,
                .undefined => .undefined_lit,
                .regex => .string,
            };
            // For number literals without a decimal point, append ".0" so the
            // Cot checker sees "42.0" (float) instead of "42" (int).
            var value = lit.value;
            if (lit.kind == .number and std.mem.indexOf(u8, lit.value, ".") == null and
                std.mem.indexOf(u8, lit.value, "e") == null and
                std.mem.indexOf(u8, lit.value, "E") == null)
            {
                value = std.fmt.allocPrint(allocator, "{s}.0", .{lit.value}) catch lit.value;
            }
            break :blk cot.addExpr(.{ .literal = .{
                .kind = kind,
                .value = value,
                .span = cot_source.Span.zero,
            } });
        },
        .call => |c| blk: {
            // console.log → println, console.error → eprintln
            const callee = if (isConsoleMethod(ts, c.callee, "log"))
                try cot.addExpr(.{ .ident = .{ .name = "println", .span = cot_source.Span.zero } })
            else if (isConsoleMethod(ts, c.callee, "error"))
                try cot.addExpr(.{ .ident = .{ .name = "eprintln", .span = cot_source.Span.zero } })
            else
                try transformTsExpr(ts, cot, allocator, c.callee);

            var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (c.args) |arg_idx| {
                const a = try transformTsExpr(ts, cot, allocator, arg_idx);
                try args.append(allocator, a);
            }
            const owned_args = try allocator.dupe(cot_ast.NodeIndex, args.items);
            break :blk cot.addExpr(.{ .call = .{
                .callee = callee,
                .args = owned_args,
                .span = cot_source.Span.zero,
            } });
        },
        .member => |m| blk: {
            const base = try transformTsExpr(ts, cot, allocator, m.object);
            break :blk cot.addExpr(.{ .field_access = .{
                .base = base,
                .field = m.property,
                .span = cot_source.Span.zero,
            } });
        },
        .binary => |b| blk: {
            const left = try transformTsExpr(ts, cot, allocator, b.left);
            const right = try transformTsExpr(ts, cot, allocator, b.right);
            const op: cot_token.Token = switch (b.op) {
                .add => .add,
                .sub => .sub,
                .mul => .mul,
                .div => .quo,
                .rem => .rem,
                .eql, .strict_eql => .eql,
                .neq, .strict_neq => .neq,
                .lt => .lss,
                .lte => .leq,
                .gt => .gtr,
                .gte => .geq,
                .land => .land,
                .lor => .lor,
                .nullish_coalesce => .kw_orelse,
                else => .add,
            };
            break :blk cot.addExpr(.{ .binary = .{
                .op = op,
                .left = left,
                .right = right,
                .span = cot_source.Span.zero,
            } });
        },
        .unary => |u| blk: {
            const operand = try transformTsExpr(ts, cot, allocator, u.operand);
            const op: cot_token.Token = switch (u.op) {
                .neg => .sub,
                .pos => .add,
                .lnot => .lnot,
                .bitnot => .not,
                else => .sub,
            };
            break :blk cot.addExpr(.{ .unary = .{
                .op = op,
                .operand = operand,
                .span = cot_source.Span.zero,
            } });
        },
        .new_expr => |n| blk: {
            const type_name = if (ts.getNode(n.callee)) |cn| switch (cn.*) {
                .expr => |e| switch (e) {
                    .ident => |id| id.name,
                    else => "Unknown",
                },
                else => "Unknown",
            } else "Unknown";
            var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (n.args) |arg_idx| {
                const a = try transformTsExpr(ts, cot, allocator, arg_idx);
                try args.append(allocator, a);
            }
            const owned_args = try allocator.dupe(cot_ast.NodeIndex, args.items);
            break :blk cot.addExpr(.{ .new_expr = .{
                .type_name = type_name,
                .fields = &.{},
                .constructor_args = owned_args,
                .is_constructor = true,
                .span = cot_source.Span.zero,
            } });
        },
        .this_expr => cot.addExpr(.{ .ident = .{ .name = "self", .span = cot_source.Span.zero } }),
        .paren => |p| blk: {
            if (p.inner == ts_ast.null_node) break :blk cot_ast.null_node;
            break :blk transformTsExpr(ts, cot, allocator, p.inner);
        },
        .template => |t| blk: {
            // Template literal → Cot string interpolation
            if (t.expressions.len == 0 and t.quasis.len == 1) {
                // No substitutions — plain string
                const raw = t.quasis[0];
                const stripped = if (raw.len >= 2 and raw[0] == '`' and raw[raw.len - 1] == '`')
                    raw[1 .. raw.len - 1]
                else
                    raw;
                const quoted = try std.fmt.allocPrint(allocator, "\"{s}\"", .{stripped});
                break :blk cot.addExpr(.{ .literal = .{
                    .kind = .string,
                    .value = quoted,
                    .span = cot_source.Span.zero,
                } });
            }
            // With substitutions: `hello ${name}!` → Cot string_interp
            var segments = std.ArrayListUnmanaged(cot_ast.StringSegment){};
            for (t.quasis, 0..) |quasi, qi| {
                // Strip template delimiters from quasi strings
                var text = quasi;
                if (qi == 0 and text.len > 0 and text[0] == '`') text = text[1..];
                if (qi == 0 and text.len >= 2 and text[text.len - 1] == '{' and text[text.len - 2] == '$')
                    text = text[0 .. text.len - 2];
                if (qi > 0 and text.len > 0 and text[0] == '}') text = text[1..];
                if (qi == t.quasis.len - 1 and text.len > 0 and text[text.len - 1] == '`')
                    text = text[0 .. text.len - 1];
                if (qi < t.quasis.len - 1 and text.len >= 2 and text[text.len - 1] == '{' and text[text.len - 2] == '$')
                    text = text[0 .. text.len - 2];

                if (text.len > 0) {
                    try segments.append(allocator, .{ .text = text });
                }
                // Add expression part if not the last quasi
                if (qi < t.expressions.len) {
                    const expr_part = try transformTsExpr(ts, cot, allocator, t.expressions[qi]);
                    try segments.append(allocator, .{ .expr = expr_part });
                }
            }
            const owned_segments = try allocator.dupe(cot_ast.StringSegment, segments.items);
            break :blk cot.addExpr(.{ .string_interp = .{
                .segments = owned_segments,
                .span = cot_source.Span.zero,
            } });
        },
        .array_literal => |al| blk: {
            var elems = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (al.elements) |elem_idx| {
                if (elem_idx != ts_ast.null_node) {
                    const e = try transformTsExpr(ts, cot, allocator, elem_idx);
                    try elems.append(allocator, e);
                }
            }
            const owned = try allocator.dupe(cot_ast.NodeIndex, elems.items);
            break :blk cot.addExpr(.{ .array_literal = .{
                .elements = owned,
                .span = cot_source.Span.zero,
            } });
        },
        .object_literal => |ol| blk: {
            // Object literal → Cot struct init (anonymous)
            var field_inits = std.ArrayListUnmanaged(cot_ast.FieldInit){};
            for (ol.properties) |prop| {
                if (prop.kind == .spread) continue; // Skip spread for now
                const key_name = if (ts.getNode(prop.key)) |kn| switch (kn.*) {
                    .expr => |ke| switch (ke) {
                        .ident => |kid| kid.name,
                        .literal => |klit| klit.value,
                        else => "unknown",
                    },
                    else => "unknown",
                } else "unknown";
                const val = if (prop.is_shorthand)
                    try cot.addExpr(.{ .ident = .{ .name = key_name, .span = cot_source.Span.zero } })
                else if (prop.value != ts_ast.null_node)
                    try transformTsExpr(ts, cot, allocator, prop.value)
                else
                    cot_ast.null_node;
                try field_inits.append(allocator, .{
                    .name = key_name,
                    .value = val,
                    .span = cot_source.Span.zero,
                });
            }
            const owned_fields = try allocator.dupe(cot_ast.FieldInit, field_inits.items);
            break :blk cot.addExpr(.{ .struct_init = .{
                .type_name = "",
                .fields = owned_fields,
                .span = cot_source.Span.zero,
            } });
        },
        .computed_member => |cm| blk: {
            // obj[expr] → obj[expr] (index expression)
            const base = try transformTsExpr(ts, cot, allocator, cm.object);
            const index = try transformTsExpr(ts, cot, allocator, cm.property);
            break :blk cot.addExpr(.{ .index = .{
                .base = base,
                .idx = index,
                .span = cot_source.Span.zero,
            } });
        },
        .optional_member => |om| blk: {
            // obj?.prop → obj?.prop (Cot has optional chaining)
            const base = try transformTsExpr(ts, cot, allocator, om.object);
            break :blk cot.addExpr(.{ .field_access = .{
                .base = base,
                .field = om.property,
                .span = cot_source.Span.zero,
            } });
        },
        .optional_call => |oc| blk: {
            // obj?.method(args) — for now just emit as regular call
            const callee = try transformTsExpr(ts, cot, allocator, oc.callee);
            var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
            for (oc.args) |arg_idx| {
                const a = try transformTsExpr(ts, cot, allocator, arg_idx);
                try args.append(allocator, a);
            }
            const owned_args = try allocator.dupe(cot_ast.NodeIndex, args.items);
            break :blk cot.addExpr(.{ .call = .{
                .callee = callee,
                .args = owned_args,
                .span = cot_source.Span.zero,
            } });
        },
        .spread => |sp| blk: {
            // ...expr — pass through the inner expression for now
            break :blk transformTsExpr(ts, cot, allocator, sp.operand);
        },
        .arrow_function => |af| blk: {
            const params = try transformTsParams(ts, cot, allocator, af.params);
            const return_type = if (af.return_type != ts_ast.null_node)
                try transformTsType(ts, cot, allocator, af.return_type)
            else
                cot_ast.null_node;
            const body = if (af.body != ts_ast.null_node)
                try transformTsExpr(ts, cot, allocator, af.body)
            else
                cot_ast.null_node;
            break :blk cot.addExpr(.{ .closure_expr = .{
                .params = params,
                .return_type = return_type,
                .body = body,
                .span = cot_source.Span.zero,
            } });
        },
        .function_expr => |fe| blk: {
            const params = try transformTsParams(ts, cot, allocator, fe.params);
            const return_type = if (fe.return_type != ts_ast.null_node)
                try transformTsType(ts, cot, allocator, fe.return_type)
            else
                cot_ast.null_node;
            const body = if (fe.body != ts_ast.null_node)
                try transformTsBlock(ts, cot, allocator, fe.body)
            else
                cot_ast.null_node;
            break :blk cot.addExpr(.{ .closure_expr = .{
                .params = params,
                .return_type = return_type,
                .body = body,
                .span = cot_source.Span.zero,
            } });
        },
        .await_expr => |aw| blk: {
            const arg = try transformTsExpr(ts, cot, allocator, aw.argument);
            break :blk cot.addExpr(.{ .await_expr = .{
                .operand = arg,
                .span = cot_source.Span.zero,
            } });
        },
        .conditional => |c| blk: {
            const cond = try transformTsExpr(ts, cot, allocator, c.condition);
            const then_val = try transformTsExpr(ts, cot, allocator, c.consequent);
            const else_val = try transformTsExpr(ts, cot, allocator, c.alternate);
            break :blk cot.addExpr(.{ .if_expr = .{
                .condition = cond,
                .then_branch = then_val,
                .else_branch = else_val,
                .span = cot_source.Span.zero,
            } });
        },
        .typeof_expr => |te| blk: {
            // typeof x → @TypeOf(x) or just pass through as string for now
            _ = te;
            break :blk cot.addExpr(.{ .literal = .{
                .kind = .string,
                .value = "\"object\"",
                .span = cot_source.Span.zero,
            } });
        },
        .void_expr => |ve| blk: {
            // void expr → evaluate expr, return undefined
            _ = try transformTsExpr(ts, cot, allocator, ve.operand);
            break :blk cot.addExpr(.{ .literal = .{
                .kind = .undefined_lit,
                .value = "undefined",
                .span = cot_source.Span.zero,
            } });
        },
        .non_null => |nn| blk: {
            // x! (non-null assertion) → x.? (unwrap optional)
            const inner = try transformTsExpr(ts, cot, allocator, nn.expr);
            break :blk cot.addExpr(.{ .deref = .{
                .operand = inner,
                .span = cot_source.Span.zero,
            } });
        },
        .as_expr => |ae| blk: {
            // x as Type → @as(Type, x) — for now just pass through the expr
            break :blk transformTsExpr(ts, cot, allocator, ae.expr);
        },
        .satisfies_expr => |se| blk: {
            // x satisfies Type → just the expression (compile-time check only)
            break :blk transformTsExpr(ts, cot, allocator, se.expr);
        },
        .super_expr => cot.addExpr(.{ .ident = .{ .name = "self", .span = cot_source.Span.zero } }),
        .sequence => |seq| blk: {
            // (a, b, c) → evaluate all, return last
            if (seq.exprs.len == 0) break :blk cot_ast.null_node;
            // For now just return the last expression
            break :blk transformTsExpr(ts, cot, allocator, seq.exprs[seq.exprs.len - 1]);
        },
        .yield_expr => |ye| blk: {
            // yield expr → await expr (simplified mapping)
            if (ye.argument != ts_ast.null_node) {
                const arg = try transformTsExpr(ts, cot, allocator, ye.argument);
                break :blk cot.addExpr(.{ .await_expr = .{
                    .operand = arg,
                    .span = cot_source.Span.zero,
                } });
            }
            break :blk cot_ast.null_node;
        },
        .delete_expr => |de| blk: {
            // delete obj.prop → just evaluate the expression (no-op for compiled code)
            _ = de;
            break :blk cot.addExpr(.{ .literal = .{
                .kind = .true_lit,
                .value = "true",
                .span = cot_source.Span.zero,
            } });
        },
        .tagged_template => |tt| blk: {
            // tag`template` → tag(template) simplified
            break :blk transformTsExpr(ts, cot, allocator, tt.template);
        },
        .update => |u| blk: {
            // i++ / i-- as expression — desugar to value
            break :blk transformTsExpr(ts, cot, allocator, u.operand);
        },
        .assign => |a| blk: {
            // Assignment as expression — just return the right side value
            break :blk transformTsExpr(ts, cot, allocator, a.right);
        },
        .class_expr => cot_ast.null_node, // Class expressions not yet supported
        else => cot_ast.null_node,
    };
}

fn transformTsType(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, idx: ts_ast.NodeIndex) error{OutOfMemory}!cot_ast.NodeIndex {
    const node = ts.getNode(idx) orelse return cot_ast.null_node;
    switch (node.*) {
        .type_node => |t| switch (t) {
            .keyword => |kw| {
                const name: []const u8 = switch (kw.kind) {
                    .number => "f64",
                    .string => "string",
                    .boolean => "bool",
                    .void_kw => "void",
                    .null_kw, .undefined => "void",
                    .any, .unknown, .object => "i64",
                    .symbol, .bigint => "i64",
                    .never => "noreturn",
                };
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = name }, .span = cot_source.Span.zero } });
            },
            .reference => |ref| {
                // Map well-known TS types to Cot types
                if (std.mem.eql(u8, ref.name, "Array") or std.mem.eql(u8, ref.name, "ReadonlyArray")) {
                    if (ref.type_args.len == 1) {
                        const elem = try transformTsType(ts, cot, allocator, ref.type_args[0]);
                        return cot.addExpr(.{ .type_expr = .{ .kind = .{ .list = elem }, .span = cot_source.Span.zero } });
                    }
                }
                if (std.mem.eql(u8, ref.name, "Map") or std.mem.eql(u8, ref.name, "ReadonlyMap")) {
                    if (ref.type_args.len == 2) {
                        const key = try transformTsType(ts, cot, allocator, ref.type_args[0]);
                        const val = try transformTsType(ts, cot, allocator, ref.type_args[1]);
                        return cot.addExpr(.{ .type_expr = .{ .kind = .{ .map = .{ .key = key, .value = val } }, .span = cot_source.Span.zero } });
                    }
                }
                if (std.mem.eql(u8, ref.name, "Promise")) {
                    // Promise<T> → just T (async fn return type)
                    if (ref.type_args.len == 1) {
                        return transformTsType(ts, cot, allocator, ref.type_args[0]);
                    }
                }
                // Generic type reference with args: Name<T, U> → Name(T, U)
                if (ref.type_args.len > 0) {
                    var args = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                    for (ref.type_args) |arg_idx| {
                        const a = try transformTsType(ts, cot, allocator, arg_idx);
                        try args.append(allocator, a);
                    }
                    const owned_args = try allocator.dupe(cot_ast.NodeIndex, args.items);
                    return cot.addExpr(.{ .type_expr = .{ .kind = .{ .generic_instance = .{
                        .name = ref.name,
                        .type_args = owned_args,
                    } }, .span = cot_source.Span.zero } });
                }
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = ref.name }, .span = cot_source.Span.zero } });
            },
            .array => |arr| {
                // T[] → List(T)
                const elem = try transformTsType(ts, cot, allocator, arr.element);
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .list = elem }, .span = cot_source.Span.zero } });
            },
            .tuple => |tup| {
                // [T, U] → (T, U)
                var elems = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                for (tup.elements) |elem_idx| {
                    const e = try transformTsType(ts, cot, allocator, elem_idx);
                    try elems.append(allocator, e);
                }
                const owned = try allocator.dupe(cot_ast.NodeIndex, elems.items);
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .tuple = owned }, .span = cot_source.Span.zero } });
            },
            .union_type => |ut| {
                // Check for T | null / T | undefined → ?T (optional)
                var non_null_types = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                var has_null = false;
                for (ut.types) |type_idx| {
                    const type_node_inner = ts.getNode(type_idx) orelse continue;
                    switch (type_node_inner.*) {
                        .type_node => |inner_t| switch (inner_t) {
                            .keyword => |kw| {
                                if (kw.kind == .null_kw or kw.kind == .undefined) {
                                    has_null = true;
                                    continue;
                                }
                            },
                            else => {},
                        },
                        else => {},
                    }
                    const transformed = try transformTsType(ts, cot, allocator, type_idx);
                    try non_null_types.append(allocator, transformed);
                }
                if (has_null and non_null_types.items.len == 1) {
                    // T | null → ?T
                    return cot.addExpr(.{ .type_expr = .{ .kind = .{ .optional = non_null_types.items[0] }, .span = cot_source.Span.zero } });
                }
                // Multi-type union — use first type as fallback for now
                if (non_null_types.items.len > 0) {
                    return non_null_types.items[0];
                }
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "i64" }, .span = cot_source.Span.zero } });
            },
            .function_type => |ft| {
                // (params) => ReturnType → fn type
                var param_types = std.ArrayListUnmanaged(cot_ast.NodeIndex){};
                for (ft.params) |p| {
                    if (p.type_ann != ts_ast.null_node) {
                        const pt = try transformTsType(ts, cot, allocator, p.type_ann);
                        try param_types.append(allocator, pt);
                    }
                }
                const ret = try transformTsType(ts, cot, allocator, ft.return_type);
                const owned_params = try allocator.dupe(cot_ast.NodeIndex, param_types.items);
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .function = .{
                    .params = owned_params,
                    .ret = ret,
                } }, .span = cot_source.Span.zero } });
            },
            .parenthesized => |pt| {
                return transformTsType(ts, cot, allocator, pt.inner);
            },
            .type_operator => |top| {
                // keyof T, readonly T, unique T — pass through the operand
                return transformTsType(ts, cot, allocator, top.operand);
            },
            .literal => |lt| {
                // Literal types (string literal type, etc.) → the base type
                _ = lt;
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "i64" }, .span = cot_source.Span.zero } });
            },
            .typeof_type => |tt| {
                _ = tt;
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "i64" }, .span = cot_source.Span.zero } });
            },
            else => return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "i64" }, .span = cot_source.Span.zero } }),
        },
        else => return cot_ast.null_node,
    }
}

fn transformTsParams(ts: *const ts_ast.Ast, cot: *cot_ast.Ast, allocator: std.mem.Allocator, params: []const ts_ast.Param) error{OutOfMemory}![]const cot_ast.Field {
    var fields = std.ArrayListUnmanaged(cot_ast.Field){};
    for (params) |p| {
        const name = if (ts.getNode(p.binding)) |bn| switch (bn.*) {
            .expr => |e| switch (e) {
                .ident => |id| id.name,
                else => "_",
            },
            else => "_",
        } else "_";
        var type_expr = if (p.type_ann != ts_ast.null_node)
            try transformTsType(ts, cot, allocator, p.type_ann)
        else
            cot_ast.null_node;
        // Optional params (title?: string) → wrap type in optional + default null
        if (p.is_optional and type_expr != cot_ast.null_node) {
            type_expr = try cot.addExpr(.{ .type_expr = .{
                .kind = .{ .optional = type_expr },
                .span = cot_source.Span.zero,
            } });
        }
        var default_value = if (p.default_value != ts_ast.null_node)
            try transformTsExpr(ts, cot, allocator, p.default_value)
        else
            cot_ast.null_node;
        // Optional params without explicit default → default to null
        if (p.is_optional and default_value == cot_ast.null_node) {
            default_value = try cot.addExpr(.{ .literal = .{
                .kind = .null_lit,
                .value = "null",
                .span = cot_source.Span.zero,
            } });
        }
        try fields.append(allocator, .{
            .name = name,
            .type_expr = type_expr,
            .default_value = default_value,
            .span = cot_source.Span.zero,
        });
    }
    return allocator.dupe(cot_ast.Field, fields.items);
}

/// Prepend `self: TypeName` parameter to a method's param list.
/// Mirrors what the Cot parser does in @safe mode for impl block methods.
fn prependSelfParam(cot: *cot_ast.Ast, allocator: std.mem.Allocator, type_name: []const u8, user_params: []const cot_ast.Field) error{OutOfMemory}![]const cot_ast.Field {
    const self_type = try cot.addExpr(.{ .type_expr = .{
        .kind = .{ .named = type_name },
        .span = cot_source.Span.zero,
    } });
    var new_params = try allocator.alloc(cot_ast.Field, user_params.len + 1);
    new_params[0] = .{
        .name = "self",
        .type_expr = self_type,
        .default_value = cot_ast.null_node,
        .span = cot_source.Span.zero,
    };
    @memcpy(new_params[1..], user_params);
    return new_params;
}

/// Extract type parameter names from TS type parameter nodes.
/// TS stores type params as AST nodes; Cot stores them as string names.
fn extractTypeParamNames(ts: *const ts_ast.Ast, allocator: std.mem.Allocator, type_params: ts_ast.NodeList) error{OutOfMemory}![]const []const u8 {
    if (type_params.len == 0) return &.{};
    var names = std.ArrayListUnmanaged([]const u8){};
    for (type_params) |tp_idx| {
        const tp_node = ts.getNode(tp_idx) orelse continue;
        switch (tp_node.*) {
            .type_node => |t| switch (t) {
                .reference => |ref| try names.append(allocator, ref.name),
                else => try names.append(allocator, "T"),
            },
            else => try names.append(allocator, "T"),
        }
    }
    return allocator.dupe([]const u8, names.items);
}

fn isConsoleMethod(ts: *const ts_ast.Ast, idx: ts_ast.NodeIndex, method: []const u8) bool {
    const node = ts.getNode(idx) orelse return false;
    switch (node.*) {
        .expr => |e| switch (e) {
            .member => |m| {
                if (!std.mem.eql(u8, m.property, method)) return false;
                const base = ts.getNode(m.object) orelse return false;
                switch (base.*) {
                    .expr => |be| switch (be) {
                        .ident => |id| return std.mem.eql(u8, id.name, "console"),
                        else => return false,
                    },
                    else => return false,
                }
            },
            else => return false,
        },
        else => return false,
    }
}
