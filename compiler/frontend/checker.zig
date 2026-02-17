//! Type checker for Cot.

const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");
const errors = @import("errors.zig");
const source = @import("source.zig");
const token = @import("token.zig");

const target_mod = @import("../core/target.zig");

const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const null_node = ast.null_node;
const Expr = ast.Expr;
const Stmt = ast.Stmt;
const Decl = ast.Decl;
const TypeKind = ast.TypeKind;
const LiteralKind = ast.LiteralKind;
const Token = token.Token;
const Type = types.Type;
const TypeIndex = types.TypeIndex;
const TypeRegistry = types.TypeRegistry;
const BasicKind = types.BasicKind;
const invalid_type = types.invalid_type;
const ErrorReporter = errors.ErrorReporter;
const Pos = source.Pos;
const Span = source.Span;

pub const CheckError = error{OutOfMemory};

pub const SymbolKind = enum { variable, constant, function, type_name, parameter };

pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    type_idx: TypeIndex,
    node: NodeIndex,
    mutable: bool,
    is_extern: bool = false,
    const_value: ?i64 = null,
    float_const_value: ?f64 = null,
    used: bool = false,

    pub fn init(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: NodeIndex, mutable: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable };
    }

    pub fn initExtern(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: NodeIndex, mutable: bool, is_extern: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable, .is_extern = is_extern };
    }

    pub fn initConst(name: []const u8, type_idx: TypeIndex, node: NodeIndex, value: i64) Symbol {
        return .{ .name = name, .kind = .constant, .type_idx = type_idx, .node = node, .mutable = false, .const_value = value };
    }

    pub fn initFloatConst(name: []const u8, type_idx: TypeIndex, node: NodeIndex, value: f64) Symbol {
        return .{ .name = name, .kind = .constant, .type_idx = type_idx, .node = node, .mutable = false, .float_const_value = value };
    }
};

pub const Scope = struct {
    parent: ?*Scope,
    symbols: std.StringHashMap(Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return .{ .parent = parent, .symbols = std.StringHashMap(Symbol).init(allocator), .allocator = allocator };
    }

    pub fn deinit(self: *Scope) void { self.symbols.deinit(); }
    pub fn define(self: *Scope, sym: Symbol) !void { try self.symbols.put(sym.name, sym); }
    pub fn lookupLocal(self: *const Scope, name: []const u8) ?Symbol { return self.symbols.get(name); }

    pub fn lookup(self: *const Scope, name: []const u8) ?Symbol {
        return self.symbols.get(name) orelse if (self.parent) |p| p.lookup(name) else null;
    }

    /// Mark a symbol as used (for lint: unused variable detection).
    /// Walks up scopes like lookup() but mutates via getPtr().
    pub fn markUsed(self: *Scope, name: []const u8) void {
        if (self.symbols.getPtr(name)) |ptr| {
            ptr.used = true;
        } else if (self.parent) |p| {
            p.markUsed(name);
        }
    }

    pub fn isDefined(self: *const Scope, name: []const u8) bool { return self.symbols.contains(name); }
};

/// Info about a generic struct or function definition.
/// Rust: hir::Generics stores param + bound. Go 1.18: types2.TypeParam stores Constraint.
pub const GenericInfo = struct {
    type_params: []const []const u8,
    type_param_bounds: []const ?[]const u8 = &.{}, // parallel array, null = unbounded
    node_idx: NodeIndex,
    tree: *const Ast, // which file's AST this node_idx belongs to (for cross-file generics)
};

/// Info about a resolved generic function instantiation for the lowerer.
pub const GenericInstInfo = struct {
    concrete_name: []const u8,
    generic_node: NodeIndex,
    type_args: []const TypeIndex,
    type_param_names: []const []const u8 = &.{}, // For impl block methods (type params come from impl, not fn)
    tree: *const Ast, // which file's AST this generic_node belongs to (for cross-file generics)
};

/// Info about a trait definition.
pub const TraitDef = struct {
    name: []const u8,
    method_names: []const []const u8,
};

/// Shared generic context — lives across all checkers in multi-file mode.
/// Created once in the driver, passed to all Checker instances so that
/// File B can see generic definitions from File A.
pub const SharedGenericContext = struct {
    generic_structs: std.StringHashMap(GenericInfo),
    generic_functions: std.StringHashMap(GenericInfo),
    instantiation_cache: std.StringHashMap(TypeIndex),
    generic_inst_by_name: std.StringHashMap(GenericInstInfo),
    generic_impl_blocks: std.StringHashMap(std.ArrayListUnmanaged(GenericImplInfo)),
    trait_defs: std.StringHashMap(TraitDef),
    trait_impls: std.StringHashMap([]const u8),

    pub fn init(allocator: std.mem.Allocator) SharedGenericContext {
        return .{
            .generic_structs = std.StringHashMap(GenericInfo).init(allocator),
            .generic_functions = std.StringHashMap(GenericInfo).init(allocator),
            .instantiation_cache = std.StringHashMap(TypeIndex).init(allocator),
            .generic_inst_by_name = std.StringHashMap(GenericInstInfo).init(allocator),
            .generic_impl_blocks = std.StringHashMap(std.ArrayListUnmanaged(GenericImplInfo)).init(allocator),
            .trait_defs = std.StringHashMap(TraitDef).init(allocator),
            .trait_impls = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *SharedGenericContext, allocator: std.mem.Allocator) void {
        self.generic_structs.deinit();
        self.generic_functions.deinit();
        self.instantiation_cache.deinit();
        self.generic_inst_by_name.deinit();
        var gib_it = self.generic_impl_blocks.valueIterator();
        while (gib_it.next()) |list| list.deinit(allocator);
        self.generic_impl_blocks.deinit();
        self.trait_defs.deinit();
        self.trait_impls.deinit();
    }
};

/// Info about a generic impl block definition.
pub const GenericImplInfo = struct {
    type_params: []const []const u8,
    methods: []const NodeIndex,
    tree: *const Ast, // which file's AST these method NodeIndexes belong to (for cross-file generics)
};

/// Parse type arguments from a monomorphized Map name like "Map(5;5)" → [K, V] TypeIndex values.
/// Returns null if the name cannot be parsed.
pub fn parseMapTypeArgs(name: []const u8) ?[2]TypeIndex {
    // Format: "Map(K;V)" where K and V are decimal TypeIndex integers
    const open = std.mem.indexOfScalar(u8, name, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, name, ')') orelse return null;
    if (close <= open + 1) return null;
    const args_str = name[open + 1 .. close];
    const semi = std.mem.indexOfScalar(u8, args_str, ';') orelse return null;
    const k_str = args_str[0..semi];
    const v_str = args_str[semi + 1 ..];
    const k = std.fmt.parseInt(TypeIndex, k_str, 10) catch return null;
    const v = std.fmt.parseInt(TypeIndex, v_str, 10) catch return null;
    return .{ k, v };
}

pub const Checker = struct {
    types: *TypeRegistry,
    scope: *Scope,
    /// Zig pattern: generic instances belong in the declaring module's namespace.
    /// We store the global scope so generic instance symbols (e.g. List_append(5))
    /// survive function scope destruction and remain accessible during lowering.
    global_scope: *Scope,
    err: *ErrorReporter,
    tree: *const Ast,
    allocator: std.mem.Allocator,
    expr_types: std.AutoHashMap(NodeIndex, TypeIndex),
    current_return_type: TypeIndex = TypeRegistry.VOID,
    /// Expected type for anonymous struct literal resolution
    expected_type: TypeIndex = invalid_type,
    in_loop: bool = false,
    in_async_fn: bool = false,
    // Generics support (Zig-style lazy monomorphization + Go checker flow)
    // Shared across all checkers in multi-file mode (owned by driver)
    generics: *SharedGenericContext,
    // Per-checker: keyed by call-site NodeIndex (file-specific)
    generic_instantiations: std.AutoHashMap(NodeIndex, GenericInstInfo) = undefined,
    /// Type substitution map, active during generic instantiation
    type_substitution: ?std.StringHashMap(TypeIndex) = null,
    /// Compilation target — used for @target_os(), @target_arch(), @target() comptime builtins
    target: target_mod.Target = target_mod.Target.native(),
    /// @safe file annotation: auto-wrap struct types with pointer in function signatures
    safe_mode: bool = false,
    /// Lint mode: track symbol usage for unused variable/parameter warnings
    lint_mode: bool = false,

    pub fn init(allocator: std.mem.Allocator, tree: *const Ast, type_reg: *TypeRegistry, reporter: *ErrorReporter, global_scope: *Scope, generic_ctx: *SharedGenericContext, target: target_mod.Target) Checker {
        return .{
            .types = type_reg,
            .scope = global_scope,
            .global_scope = global_scope,
            .err = reporter,
            .tree = tree,
            .allocator = allocator,
            .expr_types = std.AutoHashMap(NodeIndex, TypeIndex).init(allocator),
            .generics = generic_ctx,
            .generic_instantiations = std.AutoHashMap(NodeIndex, GenericInstInfo).init(allocator),
            .target = target,
        };
    }

    pub fn deinit(self: *Checker) void {
        self.expr_types.deinit();
        self.generic_instantiations.deinit();
        // SharedGenericContext is owned by the driver, not the checker
    }

    /// Run lint checks on the global scope (top-level unused functions/vars).
    /// Function-local lint checks are emitted inline during checkFnDeclWithName.
    pub fn runLintChecks(self: *Checker) void {
        self.checkScopeUnused(self.global_scope);
    }

    /// Emit warnings for unused symbols in a scope.
    fn checkScopeUnused(self: *Checker, scope_ptr: *const Scope) void {
        var it = scope_ptr.symbols.iterator();
        while (it.next()) |entry| {
            const sym = entry.value_ptr;
            if (sym.used) continue;
            // Skip _ prefixed names (intentionally unused, Zig/Go convention)
            if (sym.name.len > 0 and sym.name[0] == '_') continue;
            // Skip "self" parameter
            if (std.mem.eql(u8, sym.name, "self")) continue;
            // Skip functions and type names (only lint vars/consts/params)
            if (sym.kind == .function or sym.kind == .type_name) continue;

            // Get position from the node's span
            const pos = if (self.tree.getNode(sym.node)) |n| n.span().start else Pos.zero;
            if (sym.kind == .parameter) {
                self.err.warningWithCode(pos, .w002, sym.name);
            } else {
                self.err.warningWithCode(pos, .w001, sym.name);
            }
        }
    }

    /// Check a statement list for unreachable code after return/break/continue.
    /// Zig pattern: compiler warns on statements after `return` in a block.
    fn checkStmtsWithReachability(self: *Checker, stmts: []const NodeIndex) void {
        var seen_terminal = false;
        var warned = false;
        for (stmts) |stmt_idx| {
            if (seen_terminal and self.lint_mode and !warned) {
                // Warn once on the first unreachable statement (Zig pattern)
                const pos = if (self.tree.getNode(stmt_idx)) |n| n.span().start else Pos.zero;
                self.err.warningWithCode(pos, .w004, "unreachable code after return");
                warned = true;
            }
            if (!seen_terminal) {
                self.checkStmt(stmt_idx) catch {};
            }
            // Check if this statement is terminal (return, break, continue)
            if (!seen_terminal) {
                if (self.tree.getNode(stmt_idx)) |node| {
                    if (node.asStmt()) |stmt| {
                        switch (stmt) {
                            .return_stmt, .break_stmt, .continue_stmt => {
                                seen_terminal = true;
                            },
                            else => {},
                        }
                    }
                }
            }
        }
    }

    /// Check if a node is an empty block (for lint W005).
    fn isEmptyBlock(self: *Checker, idx: NodeIndex) bool {
        const node = self.tree.getNode(idx) orelse return false;
        if (node.asStmt()) |stmt| {
            if (stmt == .block_stmt) return stmt.block_stmt.stmts.len == 0;
        }
        if (node.asExpr()) |expr| {
            if (expr == .block_expr) return expr.block_expr.stmts.len == 0 and expr.block_expr.expr == ast.null_node;
        }
        return false;
    }

    pub fn checkFile(self: *Checker) CheckError!void {
        const file = self.tree.file orelse return;
        self.safe_mode = file.safe_mode;
        for (file.decls) |idx| try self.collectTypeDecl(idx);
        for (file.decls) |idx| try self.collectNonTypeDecl(idx);
        for (file.decls) |idx| try self.checkDecl(idx);
    }

    fn collectTypeDecl(self: *Checker, idx: NodeIndex) CheckError!void {
        const decl = (self.tree.getNode(idx) orelse return).asDecl() orelse return;
        switch (decl) {
            .struct_decl, .enum_decl, .union_decl, .type_alias, .error_set_decl, .trait_decl => try self.collectDecl(idx),
            else => {},
        }
    }

    fn collectNonTypeDecl(self: *Checker, idx: NodeIndex) CheckError!void {
        const decl = (self.tree.getNode(idx) orelse return).asDecl() orelse return;
        switch (decl) {
            .fn_decl, .var_decl, .impl_block, .impl_trait => try self.collectDecl(idx),
            else => {},
        }
    }

    fn collectDecl(self: *Checker, idx: NodeIndex) CheckError!void {
        const decl = (self.tree.getNode(idx) orelse return).asDecl() orelse return;
        switch (decl) {
            .fn_decl => |f| {
                if (self.scope.isDefined(f.name)) {
                    if (f.is_extern) if (self.scope.lookup(f.name)) |e| if (e.is_extern) return;
                    self.err.errorWithCode(f.span.start, .e302, "redefined identifier");
                    return;
                }
                // Generic functions: store definition, don't build concrete type yet
                if (f.type_params.len > 0) {
                    try self.generics.generic_functions.put(f.name, .{ .type_params = f.type_params, .type_param_bounds = f.type_param_bounds, .node_idx = idx, .tree = self.tree });
                    // Register as a type_name so checkIdentifier knows it's generic
                    try self.scope.define(Symbol.init(f.name, .type_name, invalid_type, idx, false));
                    return;
                }
                var func_type = try self.buildFuncType(f.params, f.return_type);
                // Async functions: wrap return type in Future(T)
                if (f.is_async) {
                    const inner_ret = self.types.get(func_type).func.return_type;
                    const future_ret = try self.types.makeFuture(inner_ret);
                    func_type = try self.types.makeFunc(self.types.get(func_type).func.params, future_ret);
                }
                try self.scope.define(Symbol.initExtern(f.name, .function, func_type, idx, false, f.is_extern));
                if (f.params.len > 0 and std.mem.eql(u8, f.params[0].name, "self"))
                    try self.registerMethod(f.name, f.params[0].type_expr, func_type);
            },
            .var_decl => |v| {
                if (self.scope.isDefined(v.name)) { self.err.errorWithCode(v.span.start, .e302, "redefined identifier"); return; }
                try self.scope.define(Symbol.init(v.name, if (v.is_const) .constant else .variable, invalid_type, idx, !v.is_const));
            },
            .struct_decl => |s| {
                if (self.scope.isDefined(s.name)) { self.err.errorWithCode(s.span.start, .e302, "redefined identifier"); return; }
                // Generic structs: store definition, don't build concrete type yet
                if (s.type_params.len > 0) {
                    try self.generics.generic_structs.put(s.name, .{ .type_params = s.type_params, .node_idx = idx, .tree = self.tree });
                    try self.scope.define(Symbol.init(s.name, .type_name, invalid_type, idx, false));
                    return;
                }
                const struct_type = try self.buildStructTypeWithLayout(s.name, s.fields, s.layout);
                try self.scope.define(Symbol.init(s.name, .type_name, struct_type, idx, false));
                try self.types.registerNamed(s.name, struct_type);
                // Register nested declarations with qualified names (e.g. Parser.Error → Parser_Error)
                for (s.nested_decls) |nested_idx| {
                    const nested = (self.tree.getNode(nested_idx) orelse continue).asDecl() orelse continue;
                    switch (nested) {
                        .error_set_decl => |es| {
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, es.name });
                            const es_type = try self.types.add(.{ .error_set = .{ .name = qualified, .variants = es.variants } });
                            try self.scope.define(Symbol.init(qualified, .type_name, es_type, nested_idx, false));
                            try self.types.registerNamed(qualified, es_type);
                        },
                        .enum_decl => |e| {
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, e.name });
                            const enum_type = try self.buildEnumType(e);
                            try self.scope.define(Symbol.init(qualified, .type_name, enum_type, nested_idx, false));
                            try self.types.registerNamed(qualified, enum_type);
                        },
                        .struct_decl => |ns| {
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, ns.name });
                            const ns_type = try self.buildStructTypeWithLayout(qualified, ns.fields, ns.layout);
                            try self.scope.define(Symbol.init(qualified, .type_name, ns_type, nested_idx, false));
                            try self.types.registerNamed(qualified, ns_type);
                        },
                        .type_alias => |t| {
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, t.name });
                            const target_type = self.resolveTypeExpr(t.target) catch invalid_type;
                            try self.scope.define(Symbol.init(qualified, .type_name, target_type, nested_idx, false));
                            try self.types.registerNamed(qualified, target_type);
                        },
                        .var_decl => |v| {
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, v.name });
                            try self.scope.define(Symbol.init(qualified, if (v.is_const) .constant else .variable, invalid_type, nested_idx, !v.is_const));
                        },
                        else => {},
                    }
                }
            },
            .enum_decl => |e| {
                if (self.scope.isDefined(e.name)) { self.err.errorWithCode(e.span.start, .e302, "redefined identifier"); return; }
                const enum_type = try self.buildEnumType(e);
                try self.scope.define(Symbol.init(e.name, .type_name, enum_type, idx, false));
                try self.types.registerNamed(e.name, enum_type);
            },
            .union_decl => |u| {
                if (self.scope.isDefined(u.name)) { self.err.errorWithCode(u.span.start, .e302, "redefined identifier"); return; }
                const union_type = try self.buildUnionType(u);
                try self.scope.define(Symbol.init(u.name, .type_name, union_type, idx, false));
                try self.types.registerNamed(u.name, union_type);
            },
            .error_set_decl => |es| {
                if (self.scope.isDefined(es.name)) { self.err.errorWithCode(es.span.start, .e302, "redefined identifier"); return; }
                const es_type = try self.types.add(.{ .error_set = .{ .name = es.name, .variants = es.variants } });
                try self.scope.define(Symbol.init(es.name, .type_name, es_type, idx, false));
                try self.types.registerNamed(es.name, es_type);
            },
            // Go reference: types2/alias.go - Alias stores RHS and resolves through it
            .type_alias => |t| {
                if (self.scope.isDefined(t.name)) { self.err.errorWithCode(t.span.start, .e302, "redefined identifier"); return; }
                const target_type = self.resolveTypeExpr(t.target) catch invalid_type;
                try self.scope.define(Symbol.init(t.name, .type_name, target_type, idx, false));
                try self.types.registerNamed(t.name, target_type);
            },
            .impl_block => |impl_b| {
                // Generic impl blocks: store definition, instantiate when struct is instantiated
                if (impl_b.type_params.len > 0) {
                    const gop = try self.generics.generic_impl_blocks.getOrPut(impl_b.type_name);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    try gop.value_ptr.append(self.allocator, .{ .type_params = impl_b.type_params, .methods = impl_b.methods, .tree = self.tree });
                    return;
                }
                for (impl_b.methods) |method_idx| {
                    const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                    if (method_decl == .fn_decl) {
                        const f = method_decl.fn_decl;
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_b.type_name, f.name });
                        const func_type = try self.buildFuncType(f.params, f.return_type);
                        // Detect receiver_is_ptr from resolved func type (accounts for @safe auto-wrapping)
                        var is_ptr = true;
                        const func_info = self.types.get(func_type);
                        if (func_info == .func and func_info.func.params.len > 0) {
                            const first_param_type = self.types.get(func_info.func.params[0].type_idx);
                            if (first_param_type != .pointer) is_ptr = false;
                        }
                        try self.scope.define(Symbol.initExtern(synth_name, .function, func_type, method_idx, false, false));
                        try self.types.registerMethod(impl_b.type_name, types.MethodInfo{ .name = f.name, .func_name = synth_name, .func_type = func_type, .receiver_is_ptr = is_ptr });
                    }
                }
            },
            .trait_decl => |td| {
                // Collect trait method names for validation
                var method_names = std.ArrayListUnmanaged([]const u8){};
                defer method_names.deinit(self.allocator);
                for (td.methods) |method_idx| {
                    const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                    if (method_decl == .fn_decl) try method_names.append(self.allocator, method_decl.fn_decl.name);
                }
                try self.generics.trait_defs.put(td.name, .{ .name = td.name, .method_names = try self.allocator.dupe([]const u8, method_names.items) });
            },
            .impl_trait => |it| {
                // Validate trait exists
                const trait_def = self.generics.trait_defs.get(it.trait_name) orelse {
                    self.err.errorWithCode(it.span.start, .e301, "undefined trait");
                    return;
                };
                // Validate all required methods are provided (name check)
                for (trait_def.method_names) |required_name| {
                    var found = false;
                    for (it.methods) |method_idx| {
                        const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                        if (method_decl == .fn_decl and std.mem.eql(u8, method_decl.fn_decl.name, required_name)) { found = true; break; }
                    }
                    if (!found) { self.err.errorWithCode(it.span.start, .e300, "missing required trait method"); return; }
                }
                // Register methods (same pattern as impl_block)
                for (it.methods) |method_idx| {
                    const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                    if (method_decl == .fn_decl) {
                        const f = method_decl.fn_decl;
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ it.target_type, f.name });
                        const func_type = try self.buildFuncType(f.params, f.return_type);
                        try self.scope.define(Symbol.initExtern(synth_name, .function, func_type, method_idx, false, false));
                        try self.types.registerMethod(it.target_type, types.MethodInfo{ .name = f.name, .func_name = synth_name, .func_type = func_type, .receiver_is_ptr = true });
                    }
                }
                const impl_key = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ it.trait_name, it.target_type });
                try self.generics.trait_impls.put(impl_key, it.trait_name);
            },
            else => {},
        }
    }

    fn registerMethod(self: *Checker, func_name: []const u8, self_type_expr: NodeIndex, func_type: TypeIndex) CheckError!void {
        const expr = (self.tree.getNode(self_type_expr) orelse return).asExpr() orelse return;
        if (expr != .type_expr) return;
        const te = expr.type_expr;
        var receiver_name: []const u8 = undefined;
        var is_ptr = false;
        switch (te.kind) {
            .named => |n| receiver_name = n,
            .pointer => |ptr_elem| {
                const elem_expr = (self.tree.getNode(ptr_elem) orelse return).asExpr() orelse return;
                if (elem_expr != .type_expr or elem_expr.type_expr.kind != .named) return;
                receiver_name = elem_expr.type_expr.kind.named;
                is_ptr = true;
            },
            else => return,
        }
        try self.types.registerMethod(receiver_name, types.MethodInfo{ .name = func_name, .func_name = func_name, .func_type = func_type, .receiver_is_ptr = is_ptr });
    }

    pub fn lookupMethod(self: *const Checker, type_name: []const u8, method_name: []const u8) ?types.MethodInfo {
        return self.types.lookupMethod(type_name, method_name);
    }

    fn checkDecl(self: *Checker, idx: NodeIndex) CheckError!void {
        const decl = (self.tree.getNode(idx) orelse return).asDecl() orelse return;
        switch (decl) {
            .fn_decl => |f| {
                // Skip generic function definitions — they're checked at instantiation time
                if (f.type_params.len > 0) return;
                try self.checkFnDeclWithName(f, idx, f.name);
            },
            .var_decl => |v| try self.checkVarDecl(v, idx),
            .impl_block => |impl_b| {
                // Generic impl blocks: bodies checked at instantiation time
                if (impl_b.type_params.len > 0) return;
                for (impl_b.methods) |method_idx| {
                    const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                    if (method_decl == .fn_decl) {
                        const f = method_decl.fn_decl;
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_b.type_name, f.name });
                        try self.checkFnDeclWithName(f, method_idx, synth_name);
                    }
                }
            },
            .impl_trait => |it| {
                for (it.methods) |method_idx| {
                    const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                    if (method_decl == .fn_decl) {
                        const f = method_decl.fn_decl;
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ it.target_type, f.name });
                        try self.checkFnDeclWithName(f, method_idx, synth_name);
                    }
                }
            },
            .test_decl => |t| try self.checkTestDecl(t),
            .bench_decl => |b| try self.checkBenchDecl(b),
            else => {},
        }
    }

    fn checkTestDecl(self: *Checker, t: ast.TestDecl) CheckError!void {
        var test_scope = Scope.init(self.allocator, self.scope);
        defer test_scope.deinit();
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &test_scope;
        self.current_return_type = TypeRegistry.VOID;
        if (t.body != null_node) try self.checkStmt(t.body);
        if (self.lint_mode) self.checkScopeUnused(&test_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    fn checkBenchDecl(self: *Checker, b: ast.BenchDecl) CheckError!void {
        var bench_scope = Scope.init(self.allocator, self.scope);
        defer bench_scope.deinit();
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &bench_scope;
        self.current_return_type = TypeRegistry.VOID;
        if (b.body != null_node) try self.checkStmt(b.body);
        if (self.lint_mode) self.checkScopeUnused(&bench_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    pub fn checkFnDeclWithName(self: *Checker, f: ast.FnDecl, idx: NodeIndex, lookup_name: []const u8) CheckError!void {
        const sym = self.scope.lookup(lookup_name) orelse return;
        const return_type = if (self.types.get(sym.type_idx) == .func) self.types.get(sym.type_idx).func.return_type else TypeRegistry.VOID;
        // For async functions, the registered return type is Future(T).
        // Inside the body, current_return_type should be T (the inner type).
        const body_return_type = if (f.is_async and self.types.get(return_type) == .future)
            self.types.get(return_type).future.result_type
        else
            return_type;
        var func_scope = Scope.init(self.allocator, self.scope);
        defer func_scope.deinit();
        for (f.params) |param| {
            var param_type = try self.resolveTypeExpr(param.type_expr);
            param_type = try self.safeWrapType(param_type);
            try func_scope.define(Symbol.init(param.name, .parameter, param_type, idx, false));
        }
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        const old_in_async = self.in_async_fn;
        self.scope = &func_scope;
        self.current_return_type = body_return_type;
        self.in_async_fn = f.is_async;
        if (f.body != null_node) try self.checkBlockExpr(f.body);
        // Lint: check for unused vars/params before scope exits
        if (self.lint_mode) self.checkScopeUnused(&func_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
        self.in_async_fn = old_in_async;
    }

    fn checkVarDecl(self: *Checker, v: ast.VarDecl, idx: NodeIndex) CheckError!void {
        var var_type: TypeIndex = if (v.type_expr != null_node) try self.resolveTypeExpr(v.type_expr) else invalid_type;
        if (v.value != null_node and !self.isUndefinedLit(v.value)) {
            const val_type = try self.checkExpr(v.value);
            if (var_type == invalid_type) var_type = self.materializeType(val_type)
            else if (!self.types.isAssignable(val_type, var_type)) self.err.errorWithCode(v.span.start, .e300, "type mismatch");
        }
        if (self.scope.lookupLocal(v.name) != null) {
            if (v.is_const and v.value != null_node) {
                if (self.evalConstExpr(v.value)) |cv| {
                    try self.scope.define(Symbol.initConst(v.name, var_type, idx, cv));
                    return;
                }
                if (self.isFloatType(var_type)) if (self.evalConstFloat(v.value)) |fv| {
                    try self.scope.define(Symbol.initFloatConst(v.name, var_type, idx, fv));
                    return;
                };
            }
            try self.scope.define(Symbol.init(v.name, if (v.is_const) .constant else .variable, var_type, idx, !v.is_const));
        }
        // Register error set consts as named types for use in type positions
        // Enables: const AllErrors = FileError || NetError
        if (v.is_const and var_type != invalid_type and self.types.get(var_type) == .error_set) {
            try self.types.registerNamed(v.name, var_type);
        }
    }

    fn isUndefinedLit(self: *Checker, idx: NodeIndex) bool {
        const expr = (self.tree.getNode(idx) orelse return false).asExpr() orelse return false;
        return expr == .literal and expr.literal.kind == .undefined_lit;
    }

    fn isZeroInitLit(self: *Checker, idx: NodeIndex) bool {
        const expr = (self.tree.getNode(idx) orelse return false).asExpr() orelse return false;
        return expr == .zero_init;
    }

    pub fn evalConstExpr(self: *Checker, idx: NodeIndex) ?i64 {
        const expr = (self.tree.getNode(idx) orelse return null).asExpr() orelse return null;
        return switch (expr) {
            .literal => |lit| switch (lit.kind) {
                .int => std.fmt.parseInt(i64, lit.value, 0) catch null,
                .true_lit => 1, .false_lit => 0, else => null,
            },
            .unary => |un| if (self.evalConstExpr(un.operand)) |op| switch (un.op) {
                .sub => -op, .not => ~op, .lnot => if (op == 0) @as(i64, 1) else @as(i64, 0), else => null,
            } else null,
            .binary => |bin| {
                // Try integer const-fold first
                if (self.evalConstExpr(bin.left)) |l| {
                    if (self.evalConstExpr(bin.right)) |r| {
                        return switch (bin.op) {
                            .add => l + r, .sub => l - r, .mul => l * r,
                            .quo => if (r != 0) @divTrunc(l, r) else null,
                            .rem => if (r != 0) @rem(l, r) else null,
                            .@"and" => l & r, .@"or" => l | r, .xor => l ^ r,
                            .shl => l << @intCast(r), .shr => l >> @intCast(r),
                            .eql => if (l == r) @as(i64, 1) else @as(i64, 0),
                            .neq => if (l != r) @as(i64, 1) else @as(i64, 0),
                            .lss => if (l < r) @as(i64, 1) else @as(i64, 0),
                            .leq => if (l <= r) @as(i64, 1) else @as(i64, 0),
                            .gtr => if (l > r) @as(i64, 1) else @as(i64, 0),
                            .geq => if (l >= r) @as(i64, 1) else @as(i64, 0),
                            else => null,
                        };
                    }
                }
                // Fallback: comptime string equality (for @target_os() == "linux" etc.)
                if (bin.op == .eql or bin.op == .neq) {
                    if (self.evalConstString(bin.left)) |ls| {
                        if (self.evalConstString(bin.right)) |rs| {
                            const equal = std.mem.eql(u8, ls, rs);
                            return if (bin.op == .eql)
                                (if (equal) @as(i64, 1) else @as(i64, 0))
                            else
                                (if (!equal) @as(i64, 1) else @as(i64, 0));
                        }
                    }
                }
                return null;
            },
            .paren => |p| self.evalConstExpr(p.inner),
            .ident => |id| if (self.scope.lookup(id.name)) |sym| if (sym.kind == .constant) sym.const_value else null else null,
            // Go: cmd/compile/internal/ir/const.go — const folding includes sizeof.
            // Zig: Sema.zig resolves @sizeOf at comptime. We follow Go's limited approach.
            .builtin_call => |bc| {
                if (bc.kind == .size_of) {
                    const type_idx = self.resolveTypeExpr(bc.type_arg) catch return null;
                    if (type_idx == invalid_type) return null;
                    return @as(i64, @intCast(self.types.sizeOf(type_idx)));
                }
                // @alignOf(T) — comptime, Zig Sema.zig
                if (bc.kind == .align_of) {
                    const type_idx = self.resolveTypeExpr(bc.type_arg) catch return null;
                    if (type_idx == invalid_type) return null;
                    return @as(i64, @intCast(self.types.alignmentOf(type_idx)));
                }
                // @offsetOf(T, "field") — always comptime, Zig Sema.zig:23060
                if (bc.kind == .offset_of) {
                    const type_idx = self.resolveTypeExpr(bc.type_arg) catch return null;
                    if (type_idx == invalid_type) return null;
                    const name_str = self.evalConstString(bc.args[0]) orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .struct_type) {
                        var offset: i64 = 0;
                        for (info.struct_type.fields) |sf| {
                            if (std.mem.eql(u8, sf.name, name_str)) return offset;
                            offset += @as(i64, @intCast(self.types.sizeOf(sf.type_idx)));
                        }
                    }
                    return null;
                }
                // @intFromBool(b) — comptime when arg is comptime-known
                if (bc.kind == .int_from_bool) {
                    if (self.evalConstExpr(bc.args[0])) |v| {
                        return if (v != 0) @as(i64, 1) else @as(i64, 0);
                    }
                    return null;
                }
                // @min(a, b) / @max(a, b) — comptime fold
                if (bc.kind == .min) {
                    if (self.evalConstExpr(bc.args[0])) |a| {
                        if (self.evalConstExpr(bc.args[1])) |b| {
                            return if (a < b) a else b;
                        }
                    }
                    return null;
                }
                if (bc.kind == .max) {
                    if (self.evalConstExpr(bc.args[0])) |a| {
                        if (self.evalConstExpr(bc.args[1])) |b| {
                            return if (a > b) a else b;
                        }
                    }
                    return null;
                }
                if (bc.kind == .has_field) {
                    const type_idx = self.resolveTypeExpr(bc.type_arg) catch return null;
                    if (type_idx == invalid_type) return null;
                    const name_str = self.evalConstString(bc.args[0]) orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .struct_type) {
                        for (info.struct_type.fields) |sf| {
                            if (std.mem.eql(u8, sf.name, name_str)) return 1;
                        }
                        return 0;
                    }
                    if (info == .enum_type) {
                        for (info.enum_type.variants) |v| {
                            if (std.mem.eql(u8, v.name, name_str)) return 1;
                        }
                        return 0;
                    }
                    if (info == .union_type) {
                        for (info.union_type.variants) |v| {
                            if (std.mem.eql(u8, v.name, name_str)) return 1;
                        }
                        return 0;
                    }
                    return 0;
                }
                return null;
            },
            // Comptime if-expression: fold when condition is comptime-known
            .if_expr => |ie| {
                const cond_val = self.evalConstExpr(ie.condition) orelse return null;
                if (cond_val != 0) return self.evalConstExpr(ie.then_branch);
                if (ie.else_branch != null_node) return self.evalConstExpr(ie.else_branch);
                return null;
            },
            // Block expression: { stmts; expr } — evaluate final expr if no stmts
            .block_expr => |blk| {
                if (blk.stmts.len == 0) return self.evalConstExpr(blk.expr);
                return null;
            },
            // comptime { body } — evaluate body at compile time
            .comptime_block => |cb| self.evalConstExpr(cb.body),
            else => null,
        };
    }

    /// Evaluate a comptime float expression. Returns the f64 value or null.
    pub fn evalConstFloat(self: *Checker, idx: NodeIndex) ?f64 {
        const expr = (self.tree.getNode(idx) orelse return null).asExpr() orelse return null;
        return switch (expr) {
            .literal => |lit| switch (lit.kind) {
                .float => std.fmt.parseFloat(f64, lit.value) catch null,
                .int => blk: {
                    const iv = std.fmt.parseInt(i64, lit.value, 0) catch break :blk null;
                    break :blk @floatFromInt(iv);
                },
                else => null,
            },
            .unary => |un| if (un.op == .sub) blk: {
                const v = self.evalConstFloat(un.operand) orelse break :blk null;
                break :blk -v;
            } else null,
            .binary => |bin| blk: {
                const l = self.evalConstFloat(bin.left) orelse break :blk null;
                const r = self.evalConstFloat(bin.right) orelse break :blk null;
                break :blk switch (bin.op) {
                    .add => l + r,
                    .sub => l - r,
                    .mul => l * r,
                    .quo => l / r,
                    else => null,
                };
            },
            .paren => |p| self.evalConstFloat(p.inner),
            .ident => |id| if (self.scope.lookup(id.name)) |sym| if (sym.kind == .constant) sym.float_const_value else null else null,
            else => null,
        };
    }

    fn isFloatType(_: *Checker, type_idx: TypeIndex) bool {
        return type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.F64;
    }

    /// Evaluate a comptime string expression. Returns the string value or null.
    pub fn evalConstString(self: *Checker, idx: NodeIndex) ?[]const u8 {
        const expr = (self.tree.getNode(idx) orelse return null).asExpr() orelse return null;
        return switch (expr) {
            .literal => |lit| if (lit.kind == .string) blk: {
                // Strip surrounding quotes from string literal (scanner includes them)
                const v = lit.value;
                break :blk if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') v[1 .. v.len - 1] else v;
            } else null,
            .paren => |p| self.evalConstString(p.inner),
            .builtin_call => |bc| {
                return switch (bc.kind) {
                    .target_os => self.target.os.name(),
                    .target_arch => self.target.arch.name(),
                    .target => self.target.name(),
                    else => null,
                };
            },
            else => null,
        };
    }

    pub fn checkExpr(self: *Checker, idx: NodeIndex) CheckError!TypeIndex {
        if (idx == null_node) return invalid_type;
        // During generic instantiation (type_substitution active), skip cache reads —
        // shared AST nodes need fresh type checking with the current substitution.
        // Go: each instantiation gets independent type checking (types2/instantiate.go).
        if (self.type_substitution == null) {
            if (self.expr_types.get(idx)) |t| return t;
        }
        const result = try self.checkExprInner(idx);
        try self.expr_types.put(idx, result);
        return result;
    }

    fn checkExprInner(self: *Checker, idx: NodeIndex) CheckError!TypeIndex {
        const expr = (self.tree.getNode(idx) orelse return invalid_type).asExpr() orelse return invalid_type;
        return switch (expr) {
            .ident => |id| self.checkIdentifier(id),
            .literal => |lit| self.checkLiteral(lit),
            .binary => |bin| self.checkBinary(bin),
            .unary => |un| self.checkUnary(un),
            .call => |c| self.checkCall(c),
            .index => |i| self.checkIndex(i),
            .slice_expr => |se| self.checkSliceExpr(se),
            .field_access => |f| self.checkFieldAccess(f),
            .array_literal => |al| self.checkArrayLiteral(al),
            .paren => |p| self.checkExpr(p.inner),
            .if_expr => |ie| self.checkIfExpr(ie),
            .switch_expr => |se| self.checkSwitchExpr(se),
            .block_expr => |b| self.checkBlock(b),
            .struct_init => |si| self.checkStructInit(si),
            .new_expr => |ne| self.checkNewExpr(ne),
            .builtin_call => |bc| self.checkBuiltinCall(bc),
            .string_interp => |si| self.checkStringInterp(si),
            .try_expr => |te| self.checkTryExpr(te),
            .await_expr => |ae| self.checkAwaitExpr(ae),
            .catch_expr => |ce| self.checkCatchExpr(ce),
            .error_literal => |el| self.checkErrorLiteral(el),
            .closure_expr => |ce| self.checkClosureExpr(ce),
            .addr_of => |ao| self.checkAddrOf(ao),
            .deref => |d| self.checkDeref(d),
            .tuple_literal => |tl| self.checkTupleLiteral(tl),
            .comptime_block => |cb| {
                // Zig Sema pattern: comptime {} body must be comptime-evaluable.
                // Check the body for type errors, then verify it produces a comptime value.
                const body_type = try self.checkExpr(cb.body);
                if (self.evalConstExpr(cb.body) == null and self.evalConstString(cb.body) == null) {
                    self.err.errorWithCode(cb.span.start, .e300, "unable to evaluate comptime expression");
                }
                return body_type;
            },
            .zero_init => TypeRegistry.VOID, // Type inferred from var decl context
            .type_expr, .bad_expr => invalid_type,
        };
    }

    fn checkIdentifier(self: *Checker, id: ast.Ident) TypeIndex {
        if (self.types.lookupByName(id.name)) |type_idx| {
            const t = self.types.get(type_idx);
            // Allow enum, union, and error_set types to be used as expressions
            // (enum/union for variant access, error_set for merge with ||)
            if (t == .enum_type or t == .union_type or t == .error_set) return type_idx;
            self.err.errorWithCode(id.span.start, .e301, "type name cannot be used as expression");
            return invalid_type;
        }
        // Go pattern: generic function names cannot be used without instantiation.
        // Go types2/call.go:33 — funcInst() requires IndexExpr (explicit type args).
        if (self.generics.generic_functions.contains(id.name)) {
            self.err.errorWithCode(id.span.start, .e300, "generic function requires type arguments");
            return invalid_type;
        }
        if (self.scope.lookup(id.name)) |sym| {
            if (self.lint_mode) self.scope.markUsed(id.name);
            return sym.type_idx;
        }
        // Check type substitution (for type params used as values)
        if (self.type_substitution) |sub| {
            if (sub.get(id.name)) |_| return invalid_type;
        }
        self.errWithSuggestion(id.span.start, "undefined identifier", self.findSimilarName(id.name));
        return invalid_type;
    }

    fn checkLiteral(_: *Checker, lit: ast.Literal) TypeIndex {
        return switch (lit.kind) {
            .int => TypeRegistry.UNTYPED_INT, .float => TypeRegistry.UNTYPED_FLOAT,
            .string => TypeRegistry.STRING, .char => TypeRegistry.U8,
            .true_lit, .false_lit => TypeRegistry.UNTYPED_BOOL,
            .null_lit, .undefined_lit => TypeRegistry.UNTYPED_NULL,
            .unreachable_lit => TypeRegistry.NORETURN,
        };
    }

    fn checkBinary(self: *Checker, bin: ast.Binary) CheckError!TypeIndex {
        const left_type = try self.checkExpr(bin.left);
        const right_type = try self.checkExpr(bin.right);
        const left = self.types.get(left_type);
        const right = self.types.get(right_type);

        switch (bin.op) {
            .add => {
                if (left_type == TypeRegistry.STRING and right_type == TypeRegistry.STRING) return TypeRegistry.STRING;
                if (left == .pointer and types.isInteger(right)) return left_type;
                if (types.isInteger(left) and right == .pointer) return right_type;
                if (!types.isNumeric(left) or !types.isNumeric(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                // Zig Sema.zig:peerType — common type of both operands
                return TypeRegistry.commonType(left_type, right_type);
            },
            .sub => {
                if (left == .pointer and types.isInteger(right)) return left_type;
                if (!types.isNumeric(left) or !types.isNumeric(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .mul, .quo, .rem => {
                if (!types.isNumeric(left) or !types.isNumeric(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .eql, .neq, .lss, .leq, .gtr, .geq => {
                if (!self.isComparable(left_type, right_type)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.BOOL;
            },
            .kw_and, .kw_or => {
                if (!types.isBool(left) or !types.isBool(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.BOOL;
            },
            .@"and", .@"or", .xor, .shl, .shr => {
                if (!types.isInteger(left) or !types.isInteger(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .land => {
                if (!types.isBool(left) or !types.isBool(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return TypeRegistry.BOOL;
            },
            .lor => {
                // Error set merge: FileError || NetError — Zig Sema.zig:8299
                if (left == .error_set and right == .error_set) {
                    return try self.mergeErrorSets(left.error_set, right.error_set);
                }
                // Bool logical OR
                if (types.isBool(left) and types.isBool(right)) return TypeRegistry.BOOL;
                self.err.errorWithCode(bin.span.start, .e300, "invalid operation");
                return invalid_type;
            },
            .coalesce => return if (left == .optional) left.optional.elem else left_type,
            else => return invalid_type,
        }
    }

    /// Merge two error sets into a new error set with deduplicated variants.
    /// Zig Sema.zig:8299 — union variant names, anyerror absorbs.
    fn mergeErrorSets(self: *Checker, a: types.ErrorSetType, b: types.ErrorSetType) !TypeIndex {
        var merged = std.ArrayListUnmanaged([]const u8){};
        defer merged.deinit(self.allocator);
        // Add all from a
        for (a.variants) |v| try merged.append(self.allocator, v);
        // Add from b, dedup
        for (b.variants) |v| {
            var found = false;
            for (a.variants) |av| {
                if (std.mem.eql(u8, av, v)) { found = true; break; }
            }
            if (!found) try merged.append(self.allocator, v);
        }
        const name = try std.fmt.allocPrint(self.allocator, "{s}||{s}", .{ a.name, b.name });
        return self.types.add(.{ .error_set = .{ .name = name, .variants = try self.allocator.dupe([]const u8, merged.items) } });
    }

    fn checkUnary(self: *Checker, un: ast.Unary) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(un.operand);
        const operand = self.types.get(operand_type);
        switch (un.op) {
            .sub => { if (!types.isNumeric(operand)) { self.err.errorWithCode(un.span.start, .e303, "unary '-' requires numeric operand"); return invalid_type; } return operand_type; },
            .lnot, .kw_not => { if (!types.isBool(operand)) { self.err.errorWithCode(un.span.start, .e303, "unary '!' requires bool operand"); return invalid_type; } return TypeRegistry.BOOL; },
            .not => { if (!types.isInteger(operand)) { self.err.errorWithCode(un.span.start, .e303, "unary '~' requires integer operand"); return invalid_type; } return operand_type; },
            .question => { if (operand == .optional) return operand.optional.elem; self.err.errorWithCode(un.span.start, .e303, "'.?' requires optional operand"); return invalid_type; },
            else => return invalid_type,
        }
    }

    fn checkClosureExpr(self: *Checker, ce: ast.ClosureExpr) CheckError!TypeIndex {
        // Build function type from params + return type
        const func_type = try self.buildFuncType(ce.params, ce.return_type);
        // Check body in a child scope with params defined
        var func_scope = Scope.init(self.allocator, self.scope);
        defer func_scope.deinit();
        for (ce.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try func_scope.define(Symbol.init(param.name, .parameter, param_type, null_node, false));
        }
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &func_scope;
        const ret_type = if (self.types.get(func_type) == .func) self.types.get(func_type).func.return_type else TypeRegistry.VOID;
        self.current_return_type = ret_type;
        if (ce.body != null_node) try self.checkBlockExpr(ce.body);
        self.scope = old_scope;
        self.current_return_type = old_return;
        return func_type;
    }

    fn checkCall(self: *Checker, c: ast.Call) CheckError!TypeIndex {
        if (self.tree.getNode(c.callee)) |cn| if (cn.asExpr()) |ce| if (ce == .ident) {
            const name = ce.ident.name;
            if (std.mem.eql(u8, name, "len")) return self.checkBuiltinLen(c);
            if (std.mem.eql(u8, name, "append")) return self.checkBuiltinAppend(c);
            if (std.mem.eql(u8, name, "print") or std.mem.eql(u8, name, "println") or std.mem.eql(u8, name, "eprint") or std.mem.eql(u8, name, "eprintln")) {
                if (c.args.len == 1) _ = try self.checkExpr(c.args[0]);
                return TypeRegistry.VOID;
            }
            if (std.mem.eql(u8, name, "__string_make")) {
                if (c.args.len != 2) { self.err.errorWithCode(c.span.start, .e300, "__string_make() expects 2 arguments"); return invalid_type; }
                _ = try self.checkExpr(c.args[0]);
                _ = try self.checkExpr(c.args[1]);
                return TypeRegistry.STRING;
            }
            // Generic function instantiation: max(i64) where max is generic
            if (self.generics.generic_functions.get(name)) |gen_info| {
                return try self.instantiateGenericFunc(c, gen_info, name);
            }
        };

        const callee_type = try self.checkExpr(c.callee);
        var callee = self.types.get(callee_type);
        const method_info = self.resolveMethodCall(c.callee);
        const is_method = method_info != null;

        // When a struct field shadows a method name, prefer the method in call position.
        // e.g. struct has field "keys: i64" AND impl has method "keys()" — m.keys() calls the method.
        if (callee != .func) {
            if (method_info) |mi| {
                callee = self.types.get(mi.func_type);
            } else {
                self.err.errorWithCode(c.span.start, .e300, "cannot call non-function");
                return invalid_type;
            }
        }
        if (callee != .func) { self.err.errorWithCode(c.span.start, .e300, "cannot call non-function"); return invalid_type; }
        const ft = callee.func;
        const expected_args = if (is_method and ft.params.len > 0) ft.params.len - 1 else ft.params.len;
        if (c.args.len != expected_args) { self.err.errorWithCode(c.span.start, .e300, "wrong number of arguments"); return invalid_type; }
        const param_offset: usize = if (is_method) 1 else 0;
        for (c.args, 0..) |arg_idx, i| {
            const arg_type = try self.checkExpr(arg_idx);
            if (!self.types.isAssignable(arg_type, ft.params[i + param_offset].type_idx))
                self.err.errorWithCode(c.span.start, .e300, "type mismatch");
        }
        return ft.return_type;
    }

    fn resolveMethodCall(self: *Checker, callee_idx: NodeIndex) ?types.MethodInfo {
        const ce = (self.tree.getNode(callee_idx) orelse return null).asExpr() orelse return null;
        if (ce != .field_access) return null;
        const fa = ce.field_access;
        const base_type_idx = self.expr_types.get(fa.base) orelse return null;
        const base_type = self.types.get(base_type_idx);
        const struct_name = switch (base_type) {
            .struct_type => |st| st.name,
            .pointer => |ptr| blk: {
                const inner = self.types.get(ptr.elem);
                break :blk switch (inner) {
                    .struct_type => |st| st.name,
                    .enum_type => |et| et.name,
                    else => return null,
                };
            },
            .basic => |bk| bk.name(),
            .enum_type => |et| et.name,
            else => return null,
        };
        return self.lookupMethod(struct_name, fa.field);
    }

    fn checkBuiltinLen(self: *Checker, c: ast.Call) CheckError!TypeIndex {
        if (c.args.len != 1) { self.err.errorWithCode(c.span.start, .e300, "len() expects one argument"); return invalid_type; }
        const arg_type = try self.checkExpr(c.args[0]);
        if (arg_type == TypeRegistry.STRING) return TypeRegistry.INT;
        const arg = self.types.get(arg_type);
        return switch (arg) { .array, .slice, .list => TypeRegistry.INT, else => blk: { self.err.errorWithCode(c.span.start, .e300, "len() argument must be string, array, slice, or list"); break :blk invalid_type; } };
    }

    fn checkBuiltinAppend(self: *Checker, c: ast.Call) CheckError!TypeIndex {
        if (c.args.len != 2) { self.err.errorWithCode(c.span.start, .e300, "append() expects two arguments"); return invalid_type; }
        const slice_type = try self.checkExpr(c.args[0]);
        const elem_type = try self.checkExpr(c.args[1]);
        const slice = self.types.get(slice_type);
        const expected_elem = switch (slice) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => { self.err.errorWithCode(c.span.start, .e300, "append() first argument must be array or slice"); return invalid_type; },
        };
        if (!self.types.isAssignable(elem_type, expected_elem))
            self.err.errorWithCode(c.span.start, .e300, "append() element type mismatch");
        return self.types.makeSlice(expected_elem) catch invalid_type;
    }

    fn checkBuiltinCall(self: *Checker, bc: ast.BuiltinCall) CheckError!TypeIndex {
        switch (bc.kind) {
            .size_of, .align_of => {
                const type_idx = try self.resolveTypeExpr(bc.type_arg);
                if (type_idx == invalid_type) { self.err.errorWithCode(bc.span.start, .e300, "requires valid type"); return invalid_type; }
                return TypeRegistry.I64;
            },
            .string => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.STRING;
            },
            .int_cast => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                if (!types.isNumeric(self.types.get(target_type))) { self.err.errorWithCode(bc.span.start, .e300, "@intCast target must be numeric"); return invalid_type; }
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            .ptr_cast => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(bc.span.start, .e300, "@ptrCast target must be pointer"); return invalid_type; }
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            .ptr_to_int => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            .int_to_ptr => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(bc.span.start, .e300, "@intToPtr target must be pointer"); return invalid_type; }
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            .assert => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.VOID;
            },
            .assert_eq => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.VOID;
            },
            .alloc => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            .dealloc => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.VOID;
            },
            .realloc => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.I64;
            },
            .memcpy => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                _ = try self.checkExpr(bc.args[2]);
                return TypeRegistry.VOID;
            },
            .fd_write, .fd_read, .fd_seek, .fd_open, .net_socket, .net_bind, .net_connect,
            .kevent_add, .kevent_del, .kevent_wait, .epoll_add, .epoll_wait,
            .execve,
            => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                _ = try self.checkExpr(bc.args[2]);
                return TypeRegistry.I64;
            },
            .fd_close, .net_accept, .net_set_reuse_addr, .set_nonblocking, .waitpid => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            .net_listen, .epoll_del, .dup2 => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.I64;
            },
            .kqueue_create, .epoll_create, .fork, .pipe => {
                return TypeRegistry.I64;
            },
            .ptr_of => {
                const arg_type = try self.checkExpr(bc.args[0]);
                if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(bc.span.start, .e300, "@ptrOf requires string argument"); return invalid_type; }
                return TypeRegistry.I64;
            },
            .len_of => {
                const arg_type = try self.checkExpr(bc.args[0]);
                if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(bc.span.start, .e300, "@lenOf requires string argument"); return invalid_type; }
                return TypeRegistry.I64;
            },
            .trap => return TypeRegistry.NORETURN,
            .exit => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.NORETURN;
            },
            .time => return TypeRegistry.I64,
            .random => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.I64;
            },
            .args_count => return TypeRegistry.I64,
            .arg_len, .arg_ptr => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            .environ_count => return TypeRegistry.I64,
            .environ_len, .environ_ptr => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            .target_os, .target_arch, .target => return TypeRegistry.STRING,
            .compile_error => {
                const msg = self.evalConstString(bc.args[0]) orelse "compile error";
                self.err.errorWithCode(bc.span.start, .e300, msg);
                return TypeRegistry.NORETURN;
            },
            .embed_file => {
                // Validate argument is a string literal
                const arg_node = self.tree.getNode(bc.args[0]);
                if (arg_node) |n| {
                    if (n.asExpr()) |e| {
                        if (e != .literal or e.literal.kind != .string) {
                            self.err.errorWithCode(bc.span.start, .e300, "@embedFile requires a string literal path");
                        }
                    }
                }
                return TypeRegistry.STRING;
            },
            .abs, .ceil, .floor, .trunc, .round, .sqrt => {
                const arg_type = try self.checkExpr(bc.args[0]);
                if (arg_type != TypeRegistry.F64 and arg_type != TypeRegistry.F32 and arg_type != TypeRegistry.UNTYPED_FLOAT) {
                    self.err.errorWithCode(bc.span.start, .e300, "math builtin requires float argument");
                    return invalid_type;
                }
                return TypeRegistry.F64;
            },
            .fmin, .fmax => {
                const arg1_type = try self.checkExpr(bc.args[0]);
                const arg2_type = try self.checkExpr(bc.args[1]);
                const a1_float = arg1_type == TypeRegistry.F64 or arg1_type == TypeRegistry.F32 or arg1_type == TypeRegistry.UNTYPED_FLOAT;
                const a2_float = arg2_type == TypeRegistry.F64 or arg2_type == TypeRegistry.F32 or arg2_type == TypeRegistry.UNTYPED_FLOAT;
                if (!a1_float or !a2_float) {
                    self.err.errorWithCode(bc.span.start, .e300, "@fmin/@fmax require float arguments");
                    return invalid_type;
                }
                return TypeRegistry.F64;
            },
            .has_field => {
                // @hasField(T, "name") — comptime bool, Zig Sema.zig:13785
                const type_idx = try self.resolveTypeExpr(bc.type_arg);
                if (type_idx == invalid_type) {
                    self.err.errorWithCode(bc.span.start, .e300, "@hasField requires valid type");
                    return invalid_type;
                }
                // Validate second arg is a string literal
                const name_str = self.evalConstString(bc.args[0]) orelse {
                    self.err.errorWithCode(bc.span.start, .e300, "@hasField requires string literal field name");
                    return invalid_type;
                };
                _ = name_str;
                return TypeRegistry.BOOL;
            },
            .type_of => {
                // @TypeOf(expr) — resolve operand type, return it
                // In expression position, just check the arg and return its type
                return try self.checkExpr(bc.args[0]);
            },
            .field => {
                // @field(value, "name") — comptime field access, Zig Sema.zig:26769
                var base_type = try self.checkExpr(bc.args[0]);
                // Auto-deref pointers (Zig pattern)
                while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
                const name_str = self.evalConstString(bc.args[1]) orelse {
                    self.err.errorWithCode(bc.span.start, .e300, "@field requires string literal field name");
                    return invalid_type;
                };
                const info = self.types.get(base_type);
                if (info == .struct_type) {
                    for (info.struct_type.fields) |sf| {
                        if (std.mem.eql(u8, sf.name, name_str)) return sf.type_idx;
                    }
                    self.err.errorWithCode(bc.span.start, .e300, "struct has no field with this name");
                    return invalid_type;
                }
                self.err.errorWithCode(bc.span.start, .e300, "@field requires struct type");
                return invalid_type;
            },
            // @intFromEnum(e) — Zig Sema.zig:8420: validate operand is enum, return i64
            .int_from_enum => {
                const arg_type = try self.checkExpr(bc.args[0]);
                if (self.types.get(arg_type) != .enum_type) {
                    self.err.errorWithCode(bc.span.start, .e300, "@intFromEnum requires enum argument");
                    return invalid_type;
                }
                return TypeRegistry.I64;
            },
            // @enumFromInt(T, i) — Zig Sema.zig:8480: validate T is enum, operand is int
            .enum_from_int => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                if (self.types.get(target_type) != .enum_type) {
                    self.err.errorWithCode(bc.span.start, .e300, "@enumFromInt target must be enum type");
                    return invalid_type;
                }
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            // @tagName(val) — Zig Sema.zig:20487: enum or union → string name
            .tag_name => {
                const arg_type = try self.checkExpr(bc.args[0]);
                const info = self.types.get(arg_type);
                if (info != .enum_type and info != .union_type) {
                    self.err.errorWithCode(bc.span.start, .e300, "@tagName requires enum or union argument");
                    return invalid_type;
                }
                return TypeRegistry.STRING;
            },
            // @errorName(err) — Zig Sema.zig:20375: error value → string name
            .error_name => {
                _ = try self.checkExpr(bc.args[0]);
                // Accept any type — at runtime the error tag is an integer index
                return TypeRegistry.STRING;
            },
            // @intFromBool(b) — Zig Sema.zig:20341: bool → i64 (0 or 1)
            .int_from_bool => {
                const arg_type = try self.checkExpr(bc.args[0]);
                if (arg_type != TypeRegistry.BOOL and arg_type != TypeRegistry.UNTYPED_BOOL) {
                    self.err.errorWithCode(bc.span.start, .e300, "@intFromBool requires bool argument");
                    return invalid_type;
                }
                return TypeRegistry.I64;
            },
            // @bitCast(T, val) — Zig Sema.zig:30554: reinterpret bits as target type
            .bit_cast => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            // @truncate(T, val) — Zig Sema.zig:22882: narrow integer to smaller type
            // Returns I64 because all Cot runtime values are i64; the AND mask handles narrowing.
            .truncate => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                if (!types.isNumeric(self.types.get(target_type))) {
                    self.err.errorWithCode(bc.span.start, .e300, "@truncate target must be numeric");
                    return invalid_type;
                }
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            // @as(T, val) — Zig Sema.zig:9659: explicit type coercion
            .as => {
                const target_type = try self.resolveTypeExpr(bc.type_arg);
                _ = try self.checkExpr(bc.args[0]);
                return target_type;
            },
            // @offsetOf(T, "field") — Zig Sema.zig:23060: comptime struct field offset
            .offset_of => {
                const type_idx = try self.resolveTypeExpr(bc.type_arg);
                const info = self.types.get(type_idx);
                if (info != .struct_type) {
                    self.err.errorWithCode(bc.span.start, .e300, "@offsetOf requires struct type");
                    return invalid_type;
                }
                const name_str = self.evalConstString(bc.args[0]) orelse {
                    self.err.errorWithCode(bc.span.start, .e300, "@offsetOf requires string literal field name");
                    return invalid_type;
                };
                var found = false;
                for (info.struct_type.fields) |sf| {
                    if (std.mem.eql(u8, sf.name, name_str)) { found = true; break; }
                }
                if (!found) {
                    self.err.errorWithCode(bc.span.start, .e300, "struct has no field with this name");
                    return invalid_type;
                }
                return TypeRegistry.I64;
            },
            // @min(a, b) / @max(a, b) — Zig Sema.zig:24678: integer min/max
            .min, .max => {
                _ = try self.checkExpr(bc.args[0]);
                _ = try self.checkExpr(bc.args[1]);
                return TypeRegistry.I64;
            },
            // @alignCast(alignment, ptr) — Zig: assert alignment, identity in release
            .align_cast => {
                _ = try self.resolveTypeExpr(bc.type_arg);
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            // @constCast(ptr) — Zig: remove const qualifier, type-system only
            .const_cast => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.I64;
            },
            // @arc_retain(val), @arc_release(val) — conditional ARC management
            // Emits cot_retain/cot_release only when arg type is ARC-managed.
            // No-op for non-ARC types. Used in generic collections.
            .arc_retain, .arc_release => {
                _ = try self.checkExpr(bc.args[0]);
                return TypeRegistry.VOID;
            },
        }
    }

    fn checkIndex(self: *Checker, i: ast.Index) CheckError!TypeIndex {
        var base_type = try self.checkExpr(i.base);
        const index_type = try self.checkExpr(i.idx);
        if (!types.isInteger(self.types.get(index_type))) { self.err.errorWithCode(i.span.start, .e300, "index must be integer"); return invalid_type; }
        while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
        if (base_type == TypeRegistry.STRING) return TypeRegistry.U8;
        const base = self.types.get(base_type);
        return switch (base) { .array => |a| a.elem, .slice => |s| s.elem, .list => |l| l.elem, else => blk: { self.err.errorWithCode(i.span.start, .e300, "cannot index this type"); break :blk invalid_type; } };
    }

    fn checkSliceExpr(self: *Checker, se: ast.SliceExpr) CheckError!TypeIndex {
        var base_type = try self.checkExpr(se.base);
        if (se.start != null_node) _ = try self.checkExpr(se.start);
        if (se.end != null_node) _ = try self.checkExpr(se.end);
        while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
        const base = self.types.get(base_type);
        return switch (base) { .array => |a| self.types.makeSlice(a.elem), .slice => base_type, else => blk: { self.err.errorWithCode(se.span.start, .e300, "cannot slice this type"); break :blk invalid_type; } };
    }

    fn checkFieldAccess(self: *Checker, f: ast.FieldAccess) CheckError!TypeIndex {
        if (f.base == null_node) return invalid_type;

        // Nested type namespace: TypeName.NestedType (e.g. Parser.Error)
        // Check if base is a struct type name before calling checkExpr (which would error)
        // Uses stack buffer to avoid heap allocation for lookups (Zig Sema pattern)
        if (self.tree.getNode(f.base)) |base_node| {
            if (base_node.asExpr()) |base_expr| {
                if (base_expr == .ident) {
                    const base_name = base_expr.ident.name;
                    var buf: [512]u8 = undefined;
                    const qualified = std.fmt.bufPrint(&buf, "{s}_{s}", .{ base_name, f.field }) catch "";
                    if (qualified.len > 0) {
                        if (self.types.lookupByName(qualified)) |nested_type_idx| {
                            return nested_type_idx;
                        }
                    }
                }
            }
        }

        var base_type = try self.checkExpr(f.base);

        // Auto-deref: unwrap pointer(s) before field lookup (Zig pattern)
        while (true) {
            switch (self.types.get(base_type)) {
                .pointer => |ptr| base_type = ptr.elem,
                else => break,
            }
        }

        const base = self.types.get(base_type);

        switch (base) {
            .struct_type => |st| {
                for (st.fields) |field| if (std.mem.eql(u8, field.name, f.field)) return field.type_idx;
                if (self.lookupMethod(st.name, f.field)) |m| return m.func_type;
                self.errWithSuggestion(f.span.start, "undefined field", findSimilarField(f.field, st.fields));
                return invalid_type;
            },
            .enum_type => |et| {
                for (et.variants) |v| if (std.mem.eql(u8, v.name, f.field)) return base_type;
                if (self.lookupMethod(et.name, f.field)) |m| return m.func_type;
                self.errWithSuggestion(f.span.start, "undefined variant", findSimilarVariant(f.field, et.variants));
                return invalid_type;
            },
            .union_type => |ut| {
                // .tag pseudo-field returns tag value as i64
                if (std.mem.eql(u8, f.field, "tag")) return TypeRegistry.I64;
                // Check if base is a type name (constructor) vs value (extraction)
                const is_type_access = blk: {
                    const base_node = self.tree.getNode(f.base);
                    const base_expr = if (base_node) |n| n.asExpr() else null;
                    if (base_expr) |e| {
                        if (e == .ident) {
                            if (self.scope.lookup(e.ident.name)) |sym| {
                                break :blk sym.kind == .type_name;
                            }
                        }
                    }
                    break :blk false;
                };
                for (ut.variants) |v| if (std.mem.eql(u8, v.name, f.field)) {
                    if (v.payload_type == invalid_type) return base_type;
                    if (!is_type_access) {
                        // Payload extraction: r.Ok returns the payload
                        return v.payload_type;
                    }
                    // Constructor: Result.Ok returns function type
                    const params = try self.allocator.alloc(types.FuncParam, 1);
                    params[0] = .{ .name = "payload", .type_idx = v.payload_type };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = base_type } });
                };
                self.errWithSuggestion(f.span.start, "undefined variant", findSimilarVariant(f.field, ut.variants));
                return invalid_type;
            },
            .map => |mt| {
                if (std.mem.eql(u8, f.field, "set")) {
                    const params = try self.allocator.alloc(types.FuncParam, 2);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    params[1] = .{ .name = "value", .type_idx = mt.value };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.VOID } });
                } else if (std.mem.eql(u8, f.field, "get")) {
                    const params = try self.allocator.alloc(types.FuncParam, 1);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = mt.value } });
                } else if (std.mem.eql(u8, f.field, "has")) {
                    const params = try self.allocator.alloc(types.FuncParam, 1);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.BOOL } });
                }
                self.errWithSuggestion(f.span.start, "undefined field", editDistSuggest(f.field, &.{ "set", "get", "has" }));
                return invalid_type;
            },
            .list => |lt| {
                if (std.mem.eql(u8, f.field, "push")) {
                    const params = try self.allocator.alloc(types.FuncParam, 1);
                    params[0] = .{ .name = "value", .type_idx = lt.elem };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.VOID } });
                } else if (std.mem.eql(u8, f.field, "get")) {
                    const params = try self.allocator.alloc(types.FuncParam, 1);
                    params[0] = .{ .name = "index", .type_idx = TypeRegistry.INT };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = lt.elem } });
                } else if (std.mem.eql(u8, f.field, "len")) {
                    return try self.types.add(.{ .func = .{ .params = &.{}, .return_type = TypeRegistry.INT } });
                }
                self.errWithSuggestion(f.span.start, "undefined field", editDistSuggest(f.field, &.{ "push", "get", "len" }));
                return invalid_type;
            },
            .slice => |sl| {
                if (std.mem.eql(u8, f.field, "ptr")) return try self.types.add(.{ .pointer = .{ .elem = sl.elem } })
                else if (std.mem.eql(u8, f.field, "len")) return TypeRegistry.I64;
                self.errWithSuggestion(f.span.start, "undefined field", editDistSuggest(f.field, &.{ "ptr", "len" }));
                return invalid_type;
            },
            .tuple => |tup| {
                const idx = std.fmt.parseInt(u32, f.field, 10) catch {
                    self.err.errorWithCode(f.span.start, .e300, "tuple fields must be numeric (e.g. .0, .1)");
                    return invalid_type;
                };
                if (idx >= tup.element_types.len) {
                    self.err.errorWithCode(f.span.start, .e300, "tuple index out of bounds");
                    return invalid_type;
                }
                return tup.element_types[idx];
            },
            .basic => |bk| {
                if (self.lookupMethod(bk.name(), f.field)) |m| return m.func_type;
                self.err.errorWithCode(f.span.start, .e300, "cannot access field on this type");
                return invalid_type;
            },
            else => {
                self.err.errorWithCode(f.span.start, .e300, "cannot access field on this type");
                return invalid_type;
            },
        }
    }

    fn checkStructInit(self: *Checker, si: ast.StructInit) CheckError!TypeIndex {
        const struct_type_idx = if (si.type_name.len == 0) blk: {
            // Anonymous struct literal: .{ .x = 1, .y = 2 } — resolve from expected type
            if (self.expected_type != invalid_type and self.types.get(self.expected_type) == .struct_type) {
                break :blk self.expected_type;
            }
            self.err.errorWithCode(si.span.start, .e300, "cannot infer type for anonymous struct literal");
            return invalid_type;
        } else if (si.type_args.len > 0)
            try self.resolveGenericInstance(.{ .name = si.type_name, .type_args = si.type_args }, si.span)
        else
            self.types.lookupByName(si.type_name) orelse {
                self.errWithSuggestion(si.span.start, "undefined type", self.findSimilarType(si.type_name));
                return invalid_type;
            };
        const struct_type = self.types.get(struct_type_idx);
        if (struct_type != .struct_type) { self.err.errorWithCode(si.span.start, .e300, "not a struct type"); return invalid_type; }
        for (si.fields) |fi| {
            var found = false;
            for (struct_type.struct_type.fields) |sf| if (std.mem.eql(u8, sf.name, fi.name)) {
                found = true;
                const vt = try self.checkExpr(fi.value);
                if (!self.types.isAssignable(vt, sf.type_idx)) self.err.errorWithCode(fi.span.start, .e300, "type mismatch in field");
                break;
            };
            if (!found) self.errWithSuggestion(fi.span.start, "unknown field", findSimilarField(fi.name, struct_type.struct_type.fields));
        }
        // Check that all fields without defaults are provided (Zig: Sema structInit)
        for (struct_type.struct_type.fields) |sf| {
            if (sf.default_value != null_node) continue; // has default, ok to omit
            var provided = false;
            for (si.fields) |fi| if (std.mem.eql(u8, sf.name, fi.name)) { provided = true; break; };
            if (!provided) self.err.errorWithCode(si.span.start, .e300, "missing field in struct init");
        }
        return struct_type_idx;
    }

    /// Check heap allocation expression: new Type { field: value, ... }
    /// Returns a pointer type to the struct.
    /// Reference: Go's walkNew (walk/builtin.go:601-616)
    fn checkNewExpr(self: *Checker, ne: ast.NewExpr) CheckError!TypeIndex {
        const struct_type_idx = if (ne.type_args.len > 0)
            try self.resolveGenericInstance(.{ .name = ne.type_name, .type_args = ne.type_args }, ne.span)
        else
            self.types.lookupByName(ne.type_name) orelse {
                self.errWithSuggestion(ne.span.start, "undefined type", self.findSimilarType(ne.type_name));
                return invalid_type;
            };
        const struct_type = self.types.get(struct_type_idx);
        if (struct_type != .struct_type) {
            self.err.errorWithCode(ne.span.start, .e300, "new requires a struct type");
            return invalid_type;
        }
        // Constructor sugar: `new Point(10, 20)` calls init() method
        if (ne.is_constructor) {
            const type_name = if (ne.type_args.len > 0) struct_type.struct_type.name else ne.type_name;
            const init_method = self.types.lookupMethod(type_name, "init") orelse {
                self.err.errorWithCode(ne.span.start, .e301, "no init method for constructor call");
                return invalid_type;
            };
            const func_type = self.types.get(init_method.func_type);
            if (func_type == .func) {
                const init_params = func_type.func.params;
                // init's first param is self, constructor args map to params[1..]
                const expected_args = if (init_params.len > 0) init_params.len - 1 else 0;
                if (ne.constructor_args.len != expected_args) {
                    self.err.errorWithCode(ne.span.start, .e300, "wrong number of constructor arguments");
                } else {
                    for (ne.constructor_args, 0..) |arg_idx, i| {
                        const arg_type = try self.checkExpr(arg_idx);
                        if (!self.types.isAssignable(arg_type, init_params[i + 1].type_idx)) {
                            self.err.errorWithCode(ne.span.start, .e300, "type mismatch in constructor argument");
                        }
                    }
                }
                // Validate init returns void
                if (func_type.func.return_type != TypeRegistry.VOID) {
                    self.err.errorWithCode(ne.span.start, .e300, "init method must return void");
                }
            }
            return self.types.makePointer(struct_type_idx) catch invalid_type;
        }
        // Validate field initializers (same as StructInit)
        for (ne.fields) |fi| {
            var found = false;
            for (struct_type.struct_type.fields) |sf| if (std.mem.eql(u8, sf.name, fi.name)) {
                found = true;
                const vt = try self.checkExpr(fi.value);
                if (!self.types.isAssignable(vt, sf.type_idx)) {
                    self.err.errorWithCode(fi.span.start, .e300, "type mismatch in field");
                }
                break;
            };
            if (!found) self.errWithSuggestion(fi.span.start, "unknown field", findSimilarField(fi.name, struct_type.struct_type.fields));
        }
        // Check that all fields without defaults are provided (Zig: Sema structInit)
        for (struct_type.struct_type.fields) |sf| {
            if (sf.default_value != null_node) continue;
            var provided = false;
            for (ne.fields) |fi| if (std.mem.eql(u8, sf.name, fi.name)) { provided = true; break; };
            if (!provided) self.err.errorWithCode(ne.span.start, .e300, "missing field in struct init");
        }
        // Return pointer to struct (heap-allocated object)
        return self.types.makePointer(struct_type_idx) catch invalid_type;
    }

    fn checkArrayLiteral(self: *Checker, al: ast.ArrayLiteral) CheckError!TypeIndex {
        if (al.elements.len == 0) { self.err.errorWithCode(al.span.start, .e300, "cannot infer type of empty array"); return invalid_type; }
        const first_type = try self.checkExpr(al.elements[0]);
        if (first_type == invalid_type) return invalid_type;
        for (al.elements[1..]) |elem_idx| {
            const elem_type = try self.checkExpr(elem_idx);
            if (!self.types.equal(first_type, elem_type) and !self.types.isAssignable(elem_type, first_type))
                self.err.errorWithCode(al.span.start, .e300, "array elements must have same type");
        }
        return self.types.makeArray(first_type, al.elements.len) catch invalid_type;
    }

    fn checkTupleLiteral(self: *Checker, tl: ast.TupleLiteral) CheckError!TypeIndex {
        if (tl.elements.len < 2) { self.err.errorWithCode(tl.span.start, .e300, "tuple must have at least 2 elements"); return invalid_type; }
        var elem_types = std.ArrayListUnmanaged(TypeIndex){};
        defer elem_types.deinit(self.allocator);
        for (tl.elements) |elem_idx| {
            const et = try self.checkExpr(elem_idx);
            elem_types.append(self.allocator, et) catch return invalid_type;
        }
        return self.types.makeTuple(elem_types.items) catch invalid_type;
    }

    fn checkIfExpr(self: *Checker, ie: ast.IfExpr) CheckError!TypeIndex {
        const cond_type = try self.checkExpr(ie.condition);
        // Optional unwrap: if expr |val| { ... } — Zig payload capture pattern
        if (ie.capture.len > 0) {
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) {
                self.err.errorWithCode(ie.span.start, .e300, "capture requires optional type");
                return TypeRegistry.VOID;
            }
            const elem_type = cond_info.optional.elem;
            // Check then-branch with capture variable in scope
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(ie.capture, .variable, elem_type, ast.null_node, false));
            const then_type = try self.checkExpr(ie.then_branch);
            self.scope = old_scope;
            if (ie.else_branch != null_node) {
                const else_type = try self.checkExpr(ie.else_branch);
                if (!self.types.equal(then_type, else_type) and !self.types.isAssignable(else_type, then_type) and !self.types.isAssignable(then_type, else_type))
                    self.err.errorWithCode(ie.span.start, .e300, "if branches have different types");
                return then_type;
            }
            return TypeRegistry.VOID;
        }
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(ie.span.start, .e300, "condition must be bool");
        // Comptime dead branch elimination: only check the taken branch when condition is comptime-known.
        // This allows @compileError in dead branches (Zig Sema pattern).
        if (self.evalConstExpr(ie.condition)) |cond_val| {
            if (cond_val != 0) return try self.checkExpr(ie.then_branch);
            if (ie.else_branch != null_node) return try self.checkExpr(ie.else_branch);
            return TypeRegistry.VOID;
        }
        const then_type = try self.checkExpr(ie.then_branch);
        if (ie.else_branch != null_node) {
            const else_type = try self.checkExpr(ie.else_branch);
            if (!self.types.equal(then_type, else_type)) self.err.errorWithCode(ie.span.start, .e300, "if branches have different types");
            return then_type;
        }
        return TypeRegistry.VOID;
    }

    fn checkSwitchExpr(self: *Checker, se: ast.SwitchExpr) CheckError!TypeIndex {
        const subject_type = try self.checkExpr(se.subject);
        const subject_info = self.types.get(subject_type);
        const is_union = subject_info == .union_type;
        const is_enum = subject_info == .enum_type;
        var result_type: TypeIndex = TypeRegistry.VOID;
        var first = true;
        for (se.cases) |case| {
            // Range patterns: check both start and end are valid expressions
            for (case.patterns) |val_idx| _ = try self.checkExpr(val_idx);
            // Rust: match guard type-checked as bool. Guard expression must be boolean.
            if (case.guard != ast.null_node) _ = try self.checkExpr(case.guard);

            // If union switch with capture, define capture variable in a new scope
            if (is_union and case.capture.len > 0) {
                const payload_type = self.resolveUnionCaptureType(subject_info.union_type, case.patterns);
                var capture_scope = Scope.init(self.allocator, self.scope);
                defer capture_scope.deinit();
                const old_scope = self.scope;
                self.scope = &capture_scope;
                try capture_scope.define(Symbol.init(case.capture, .variable, payload_type, ast.null_node, false));
                const body_type = try self.checkExpr(case.body);
                self.scope = old_scope;
                if (first) { result_type = self.materializeType(body_type); first = false; }
            } else {
                const body_type = try self.checkExpr(case.body);
                if (first) { result_type = self.materializeType(body_type); first = false; }
            }
        }
        if (se.else_body != null_node) _ = try self.checkExpr(se.else_body);

        // Enum switch exhaustiveness check (Zig pattern: switch on enum must be exhaustive or have else)
        if (is_enum and se.else_body == null_node) {
            const et = subject_info.enum_type;
            // Build coverage set: track which variants are covered by unguarded cases
            var covered = std.StringHashMap(void).init(self.allocator);
            defer covered.deinit();
            for (se.cases) |case| {
                // Cases with guards don't guarantee coverage
                if (case.guard != ast.null_node) continue;
                for (case.patterns) |pat_idx| {
                    const pat_node = self.tree.getNode(pat_idx) orelse continue;
                    const pat_expr = pat_node.asExpr() orelse continue;
                    if (pat_expr == .field_access) {
                        covered.put(pat_expr.field_access.field, {}) catch {};
                    }
                }
            }
            // Check if all variants are covered
            if (covered.count() < et.variants.len) {
                // Build list of missing variant names
                var missing = std.ArrayListUnmanaged(u8){};
                defer missing.deinit(self.allocator);
                var missing_count: usize = 0;
                for (et.variants) |v| {
                    if (!covered.contains(v.name)) {
                        if (missing_count > 0) missing.appendSlice(self.allocator, ", ") catch {};
                        missing.appendSlice(self.allocator, v.name) catch {};
                        missing_count += 1;
                    }
                }
                const msg = std.fmt.allocPrint(self.allocator, "switch on enum '{s}' must be exhaustive or have else branch; missing: {s}", .{ et.name, missing.items }) catch "non-exhaustive enum switch";
                self.err.errorWithCode(se.span.start, .e300, msg);
            }
        }

        return result_type;
    }

    /// Resolve the payload type for a union switch case capture from its patterns.
    fn resolveUnionCaptureType(self: *Checker, ut: types.UnionType, patterns: []const NodeIndex) TypeIndex {
        // Use the first pattern to determine the variant
        if (patterns.len == 0) return TypeRegistry.VOID;
        // Pattern is typically a field_access like Result.Ok
        const node = self.tree.getNode(patterns[0]) orelse return TypeRegistry.VOID;
        const expr = node.asExpr() orelse return TypeRegistry.VOID;
        const field_name = switch (expr) {
            .field_access => |fa| fa.field,
            else => return TypeRegistry.VOID,
        };
        for (ut.variants) |v| {
            if (std.mem.eql(u8, v.name, field_name)) return v.payload_type;
        }
        return TypeRegistry.VOID;
    }

    fn checkBlock(self: *Checker, b: ast.BlockExpr) CheckError!TypeIndex {
        var block_scope = Scope.init(self.allocator, self.scope);
        defer block_scope.deinit();
        const old_scope = self.scope;
        self.scope = &block_scope;
        self.checkStmtsWithReachability(b.stmts);
        if (self.lint_mode) self.checkScopeUnused(&block_scope);
        self.scope = old_scope;
        return if (b.expr != null_node) try self.checkExpr(b.expr) else TypeRegistry.VOID;
    }

    fn checkBlockExpr(self: *Checker, idx: NodeIndex) CheckError!void {
        const node = self.tree.getNode(idx) orelse return;
        if (node.asExpr()) |expr| if (expr == .block_expr) { _ = try self.checkBlock(expr.block_expr); return; };
        if (node.asStmt()) |stmt| if (stmt == .block_stmt) { try self.checkBlockStmt(stmt.block_stmt); return; };
    }

    fn checkStringInterp(self: *Checker, si: ast.StringInterp) CheckError!TypeIndex {
        for (si.segments) |seg| {
            if (seg == .expr) _ = try self.checkExpr(seg.expr);
        }
        return TypeRegistry.STRING;
    }

    fn checkAddrOf(self: *Checker, ao: ast.AddrOf) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(ao.operand);
        return try self.types.makePointer(operand_type);
    }

    fn checkTryExpr(self: *Checker, te: ast.TryExpr) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(te.operand);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .error_union) {
            self.err.errorWithCode(te.span.start, .e300, "try requires error union type");
            return invalid_type;
        }
        // Enclosing function must return an error union
        const ret_info = self.types.get(self.current_return_type);
        if (ret_info != .error_union) {
            self.err.errorWithCode(te.span.start, .e300, "try in non-error-returning function");
        }
        return operand_info.error_union.elem;
    }

    fn checkAwaitExpr(self: *Checker, ae: ast.AwaitExpr) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(ae.operand);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .future) {
            self.err.errorWithCode(ae.span.start, .e300, "cannot await non-future type");
            return invalid_type;
        }
        // await is allowed both in async and sync contexts.
        // In sync context, it blocks until the future completes (like block_on).
        // No function coloring — any code can await a Future.
        return operand_info.future.result_type;
    }

    fn checkCatchExpr(self: *Checker, ce: ast.CatchExpr) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(ce.operand);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .error_union) {
            self.err.errorWithCode(ce.span.start, .e300, "catch requires error union type");
            return try self.checkExpr(ce.fallback);
        }
        const elem_type = operand_info.error_union.elem;
        // Catch capture: catch |err| { ... } — bind err to error set type
        if (ce.capture.len > 0) {
            const err_type = if (operand_info.error_union.error_set != types.invalid_type) operand_info.error_union.error_set else TypeRegistry.I64;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(ce.capture, .variable, err_type, ast.null_node, false));
            _ = try self.checkExpr(ce.fallback);
            self.scope = old_scope;
        } else {
            _ = try self.checkExpr(ce.fallback);
        }
        return elem_type;
    }

    fn checkErrorLiteral(self: *Checker, el: ast.ErrorLiteral) CheckError!TypeIndex {
        // error.X returns an error set type; the specific set is inferred from context
        // First check the function's return type for the error set
        const ret_info = self.types.get(self.current_return_type);
        if (ret_info == .error_union and ret_info.error_union.error_set != types.invalid_type) {
            const es_info = self.types.get(ret_info.error_union.error_set);
            if (es_info == .error_set) {
                for (es_info.error_set.variants) |v| {
                    if (std.mem.eql(u8, v, el.error_name)) return ret_info.error_union.error_set;
                }
            }
        }
        // Check expected_type (for var init context): var e: MyError!i64 = error.Foo
        if (self.expected_type != invalid_type) {
            const exp_info = self.types.get(self.expected_type);
            if (exp_info == .error_union and exp_info.error_union.error_set != types.invalid_type) {
                const es_info = self.types.get(exp_info.error_union.error_set);
                if (es_info == .error_set) {
                    for (es_info.error_set.variants) |v| {
                        if (std.mem.eql(u8, v, el.error_name)) return exp_info.error_union.error_set;
                    }
                }
            }
            // expected_type might be an error_set directly
            if (exp_info == .error_set) {
                for (exp_info.error_set.variants) |v| {
                    if (std.mem.eql(u8, v, el.error_name)) return self.expected_type;
                }
            }
        }
        // Also check all error set types in scope (for switch on caught errors)
        // This allows error.X in switch patterns when catching from different error sets
        return self.current_return_type;
    }

    fn checkDeref(self: *Checker, d: ast.Deref) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(d.operand);
        if (self.types.isPointer(operand_type)) return self.types.pointerElem(operand_type);
        self.err.errorWithCode(d.span.start, .e300, "cannot dereference non-pointer");
        return invalid_type;
    }

    fn checkStmt(self: *Checker, idx: NodeIndex) CheckError!void {
        const stmt = (self.tree.getNode(idx) orelse return).asStmt() orelse return;
        switch (stmt) {
            .expr_stmt => |es| _ = try self.checkExpr(es.expr),
            .return_stmt => |rs| try self.checkReturn(rs),
            .var_stmt => |vs| try self.checkVarStmt(vs, idx),
            .assign_stmt => |as_stmt| try self.checkAssign(as_stmt),
            .if_stmt => |is| try self.checkIfStmt(is),
            .while_stmt => |ws| try self.checkWhileStmt(ws),
            .for_stmt => |fs| try self.checkForStmt(fs),
            .block_stmt => |bs| try self.checkBlockStmt(bs),
            .break_stmt => |bs| if (!self.in_loop) self.err.errorWithCode(bs.span.start, .e300, "break outside of loop"),
            .continue_stmt => |cs| if (!self.in_loop) self.err.errorWithCode(cs.span.start, .e300, "continue outside of loop"),
            .defer_stmt => |ds| _ = try self.checkExpr(ds.expr),
            .destructure_stmt => |ds| try self.checkDestructureStmt(ds, idx),
            .bad_stmt => {},
        }
    }

    fn checkReturn(self: *Checker, rs: ast.ReturnStmt) CheckError!void {
        if (self.current_return_type == TypeRegistry.NORETURN) {
            self.err.errorWithCode(rs.span.start, .e300, "noreturn function cannot return");
            return;
        }
        if (rs.value != null_node) {
            const saved_expected = self.expected_type;
            self.expected_type = self.current_return_type;
            defer self.expected_type = saved_expected;
            const val_type = try self.checkExpr(rs.value);
            if (self.current_return_type == TypeRegistry.VOID) self.err.errorWithCode(rs.span.start, .e300, "void function should not return a value")
            else if (!self.types.isAssignable(val_type, self.current_return_type)) self.err.errorWithCode(rs.span.start, .e300, "type mismatch");
        } else if (self.current_return_type != TypeRegistry.VOID) self.err.errorWithCode(rs.span.start, .e300, "non-void function must return a value");
    }

    fn checkVarStmt(self: *Checker, vs: ast.VarStmt, idx: NodeIndex) CheckError!void {
        if (self.scope.isDefined(vs.name)) { self.err.errorWithCode(vs.span.start, .e302, "redefined identifier"); return; }
        // Lint: check for variable shadowing (parent scope has same name)
        if (self.lint_mode and self.scope.parent != null) {
            if (self.scope.parent.?.lookup(vs.name) != null) {
                self.err.warningWithCode(vs.span.start, .w003, vs.name);
            }
        }
        var var_type: TypeIndex = if (vs.type_expr != null_node) try self.resolveTypeExpr(vs.type_expr) else invalid_type;
        if (vs.value != null_node and !self.isUndefinedLit(vs.value) and !self.isZeroInitLit(vs.value)) {
            const saved_expected = self.expected_type;
            if (var_type != invalid_type) self.expected_type = var_type;
            defer self.expected_type = saved_expected;
            const val_type = try self.checkExpr(vs.value);
            if (var_type == invalid_type) var_type = self.materializeType(val_type)
            else if (!self.types.isAssignable(val_type, var_type)) {
                // @safe coercion: Foo annotation accepts *Foo value (e.g. var f: Foo = new Foo{...})
                if (self.safe_mode and self.types.get(var_type) == .struct_type and self.types.get(val_type) == .pointer and
                    self.types.get(self.types.get(val_type).pointer.elem) == .struct_type and
                    self.types.isAssignable(self.types.get(val_type).pointer.elem, var_type))
                {
                    var_type = val_type; // upgrade to *Foo
                } else {
                    self.err.errorWithCode(vs.span.start, .e300, "type mismatch");
                }
            }
        }
        if (self.isZeroInitLit(vs.value) and var_type == invalid_type) {
            self.err.errorWithCode(vs.span.start, .e300, "zero init requires type annotation");
        }
        // Store const_value for comptime const-folding (Zig Sema pattern: local const propagation)
        if (vs.is_const and vs.value != null_node) {
            if (self.evalConstExpr(vs.value)) |cv| {
                try self.scope.define(Symbol.initConst(vs.name, var_type, idx, cv));
                return;
            }
        }
        try self.scope.define(Symbol.init(vs.name, if (vs.is_const) .constant else .variable, var_type, idx, !vs.is_const));
    }

    fn checkDestructureStmt(self: *Checker, ds: ast.DestructureStmt, idx: NodeIndex) CheckError!void {
        // Check for redefinitions
        for (ds.bindings) |b| {
            if (self.scope.isDefined(b.name)) {
                self.err.errorWithCode(b.span.start, .e302, "redefined identifier");
                return;
            }
        }

        // Check the RHS value type
        const val_type = try self.checkExpr(ds.value);
        const val_info = self.types.get(val_type);

        // RHS must be a tuple type
        if (val_info != .tuple) {
            self.err.errorWithCode(ds.span.start, .e300, "destructuring requires a tuple value");
            return;
        }
        const tup = val_info.tuple;

        // Count must match
        if (ds.bindings.len != tup.element_types.len) {
            self.err.errorWithCode(ds.span.start, .e300, "destructuring count mismatch");
            return;
        }

        // Define each binding with its element type
        for (ds.bindings, 0..) |b, i| {
            var elem_type = tup.element_types[i];
            if (b.type_expr != null_node) {
                const annotated = try self.resolveTypeExpr(b.type_expr);
                if (!self.types.isAssignable(elem_type, annotated)) {
                    self.err.errorWithCode(b.span.start, .e300, "type mismatch");
                }
                elem_type = annotated;
            } else {
                elem_type = self.materializeType(elem_type);
            }
            try self.scope.define(Symbol.init(b.name, if (ds.is_const) .constant else .variable, elem_type, idx, !ds.is_const));
        }
    }

    fn checkAssign(self: *Checker, as_stmt: ast.AssignStmt) CheckError!void {
        const target_type = try self.checkExpr(as_stmt.target);
        const value_type = try self.checkExpr(as_stmt.value);
        const target = (self.tree.getNode(as_stmt.target) orelse return).asExpr() orelse return;
        switch (target) {
            .ident => |id| if (self.scope.lookup(id.name)) |sym| if (!sym.mutable) { self.err.errorWithCode(as_stmt.span.start, .e300, "cannot assign to constant"); return; },
            .index, .field_access, .deref => {},
            else => { self.err.errorWithCode(as_stmt.span.start, .e300, "invalid assignment target"); return; },
        }
        if (!self.types.isAssignable(value_type, target_type)) self.err.errorWithCode(as_stmt.span.start, .e300, "type mismatch");
    }

    fn checkIfStmt(self: *Checker, is: ast.IfStmt) CheckError!void {
        const cond_type = try self.checkExpr(is.condition);
        // Optional unwrap: if expr |val| { ... } — Zig payload capture pattern
        if (is.capture.len > 0) {
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) {
                self.err.errorWithCode(is.span.start, .e300, "capture requires optional type");
                return;
            }
            const elem_type = cond_info.optional.elem;
            // Check then-branch with capture variable in scope
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(is.capture, .variable, elem_type, ast.null_node, false));
            try self.checkStmt(is.then_branch);
            self.scope = old_scope;
            if (is.else_branch != null_node) try self.checkStmt(is.else_branch);
            return;
        }
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(is.span.start, .e300, "condition must be bool");
        // Comptime dead branch elimination: only check the taken branch when condition is comptime-known.
        // This allows @compileError in dead branches (Zig Sema pattern).
        if (self.evalConstExpr(is.condition)) |cond_val| {
            if (cond_val != 0) { try self.checkStmt(is.then_branch); return; }
            if (is.else_branch != null_node) { try self.checkStmt(is.else_branch); return; }
            return;
        }
        // Lint: empty block detection (W005)
        if (self.lint_mode and self.isEmptyBlock(is.then_branch)) {
            self.err.warningWithCode(is.span.start, .w005, "empty if body");
        }
        try self.checkStmt(is.then_branch);
        if (is.else_branch != null_node) {
            if (self.lint_mode and self.isEmptyBlock(is.else_branch)) {
                self.err.warningWithCode(is.span.start, .w005, "empty else body");
            }
            try self.checkStmt(is.else_branch);
        }
    }

    fn checkWhileStmt(self: *Checker, ws: ast.WhileStmt) CheckError!void {
        const cond_type = try self.checkExpr(ws.condition);
        // Optional capture: while (expr) |val| { ... } — Zig payload capture pattern
        if (ws.capture.len > 0) {
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) {
                self.err.errorWithCode(ws.span.start, .e300, "capture requires optional type");
                return;
            }
            const elem_type = cond_info.optional.elem;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(ws.capture, .variable, elem_type, ast.null_node, false));
            const old_in_loop = self.in_loop;
            self.in_loop = true;
            if (ws.continue_expr != ast.null_node) try self.checkContinueExpr(ws.continue_expr);
            try self.checkStmt(ws.body);
            self.in_loop = old_in_loop;
            self.scope = old_scope;
            return;
        }
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(ws.span.start, .e300, "condition must be bool");
        // Lint: empty block detection (W005)
        if (self.lint_mode and self.isEmptyBlock(ws.body)) {
            self.err.warningWithCode(ws.span.start, .w005, "empty while body");
        }
        const old_in_loop = self.in_loop;
        self.in_loop = true;
        if (ws.continue_expr != ast.null_node) try self.checkContinueExpr(ws.continue_expr);
        try self.checkStmt(ws.body);
        self.in_loop = old_in_loop;
    }

    /// Check a while continue expression — may be a statement (assignment) or expression
    fn checkContinueExpr(self: *Checker, node_idx: ast.NodeIndex) CheckError!void {
        const node = self.tree.getNode(node_idx) orelse return;
        if (node.asStmt() != null) {
            try self.checkStmt(node_idx);
        } else {
            _ = try self.checkExpr(node_idx);
        }
    }

    fn checkForStmt(self: *Checker, fs: ast.ForStmt) CheckError!void {
        // Inline for: unroll at compile time — Zig AstGen.zig:6863
        if (fs.is_inline) {
            return self.checkInlineFor(fs);
        }
        var elem_type: TypeIndex = invalid_type;
        var idx_type: TypeIndex = TypeRegistry.I64;
        if (fs.isRange()) {
            const start_type = try self.checkExpr(fs.range_start);
            const end_type = try self.checkExpr(fs.range_end);
            if (!types.isInteger(self.types.get(start_type)) or !types.isInteger(self.types.get(end_type))) {
                self.err.errorWithCode(fs.span.start, .e300, "range bounds must be integers");
            } else {
                elem_type = start_type;
            }
        } else {
            const iter_type = try self.checkExpr(fs.iterable);
            const iter = self.types.get(iter_type);
            switch (iter) {
                .array => |a| elem_type = a.elem,
                .slice => |s| elem_type = s.elem,
                .map => |ma| {
                    // for k, v in map — binding=value, index_binding=key
                    elem_type = ma.value;
                    idx_type = ma.key;
                },
                .struct_type => |st| {
                    // Check if struct is a Map generic instance (name starts with "Map(")
                    // Go range.go:241-270: desugars `for k, v := range m` to mapIterStart/mapIterNext
                    if (std.mem.startsWith(u8, st.name, "Map(")) {
                        // Parse type args from monomorphized name: "Map(K;V)" where K,V are TypeIndex ints
                        if (parseMapTypeArgs(st.name)) |args| {
                            idx_type = args[0]; // K
                            elem_type = args[1]; // V
                        } else {
                            self.err.errorWithCode(fs.span.start, .e300, "cannot iterate over this type");
                        }
                    } else {
                        self.err.errorWithCode(fs.span.start, .e300, "cannot iterate over this type");
                    }
                },
                else => self.err.errorWithCode(fs.span.start, .e300, "cannot iterate over this type"),
            }
        }
        var loop_scope = Scope.init(self.allocator, self.scope);
        defer loop_scope.deinit();
        try loop_scope.define(Symbol.init(fs.binding, .variable, elem_type, null_node, false));
        // Define index/key binding if present (for i, x in arr OR for k, v in map)
        if (fs.index_binding) |idx_binding| {
            try loop_scope.define(Symbol.init(idx_binding, .variable, idx_type, null_node, false));
        }
        // Lint: empty block detection (W005)
        if (self.lint_mode and self.isEmptyBlock(fs.body)) {
            self.err.warningWithCode(fs.span.start, .w005, "empty for body");
        }
        const old_scope = self.scope;
        const old_in_loop = self.in_loop;
        self.scope = &loop_scope;
        self.in_loop = true;
        try self.checkStmt(fs.body);
        self.scope = old_scope;
        self.in_loop = old_in_loop;
    }

    /// Check inline for — unroll at compile time with comptime-known range bounds.
    /// Zig AstGen.zig:6863: block_inline tag, Sema materializes each iteration.
    fn checkInlineFor(self: *Checker, fs: ast.ForStmt) CheckError!void {
        if (!fs.isRange()) {
            self.err.errorWithCode(fs.span.start, .e300, "inline for requires comptime range (start..end)");
            return;
        }
        const start_val = self.evalConstExpr(fs.range_start) orelse {
            self.err.errorWithCode(fs.span.start, .e300, "inline for range start must be comptime-known");
            return;
        };
        const end_val = self.evalConstExpr(fs.range_end) orelse {
            self.err.errorWithCode(fs.span.start, .e300, "inline for range end must be comptime-known");
            return;
        };
        // Unroll: check body once per iteration with const binding
        var i = start_val;
        while (i < end_val) : (i += 1) {
            var iter_scope = Scope.init(self.allocator, self.scope);
            defer iter_scope.deinit();
            try iter_scope.define(Symbol.initConst(fs.binding, TypeRegistry.I64, null_node, i));
            const old_scope = self.scope;
            self.scope = &iter_scope;
            try self.checkStmt(fs.body);
            self.scope = old_scope;
        }
    }

    fn checkBlockStmt(self: *Checker, bs: ast.BlockStmt) CheckError!void {
        var block_scope = Scope.init(self.allocator, self.scope);
        defer block_scope.deinit();
        const old_scope = self.scope;
        self.scope = &block_scope;
        self.checkStmtsWithReachability(bs.stmts);
        if (self.lint_mode) self.checkScopeUnused(&block_scope);
        self.scope = old_scope;
    }

    fn resolveTypeExpr(self: *Checker, idx: NodeIndex) CheckError!TypeIndex {
        if (idx == null_node) return invalid_type;
        const expr = (self.tree.getNode(idx) orelse return invalid_type).asExpr() orelse return invalid_type;
        if (expr == .ident) {
            // Check type substitution first (active during generic instantiation)
            if (self.type_substitution) |sub| {
                if (sub.get(expr.ident.name)) |substituted| return substituted;
            }
            if (self.types.lookupByName(expr.ident.name)) |tidx| return tidx;
            if (self.scope.lookup(expr.ident.name)) |sym| if (sym.kind == .type_name) return sym.type_idx;
            self.errWithSuggestion(expr.ident.span.start, "undefined type", self.findSimilarType(expr.ident.name));
            return invalid_type;
        }
        // @TypeOf(expr) in type position — Zig Sema.zig:18007
        if (expr == .builtin_call and expr.builtin_call.kind == .type_of) {
            return try self.checkExpr(expr.builtin_call.args[0]);
        }
        if (expr != .type_expr) return invalid_type;
        return self.resolveType(expr.type_expr);
    }

    fn resolveType(self: *Checker, te: ast.TypeExpr) CheckError!TypeIndex {
        return switch (te.kind) {
            .named => |n| blk: {
                // Check type substitution first (active during generic instantiation)
                if (self.type_substitution) |sub| {
                    if (sub.get(n)) |substituted| break :blk substituted;
                }
                break :blk self.types.lookupByName(n) orelse if (self.scope.lookup(n)) |s| if (s.kind == .type_name) s.type_idx else invalid_type else {
                    self.errWithSuggestion(te.span.start, "undefined type", self.findSimilarType(n));
                    return invalid_type;
                };
            },
            .pointer => |e| self.types.makePointer(try self.resolveTypeExpr(e)),
            .optional => |e| self.types.makeOptional(try self.resolveTypeExpr(e)),
            .error_union => |eu| blk: {
                const elem = try self.resolveTypeExpr(eu.elem);
                if (eu.error_set != ast.null_node) {
                    const es_type = try self.resolveTypeExpr(eu.error_set);
                    break :blk self.types.makeErrorUnionWithSet(elem, es_type);
                }
                break :blk self.types.makeErrorUnion(elem);
            },
            .slice => |e| self.types.makeSlice(try self.resolveTypeExpr(e)),
            .array => |a| blk: {
                const elem = try self.resolveTypeExpr(a.elem);
                const size_expr = (self.tree.getNode(a.size) orelse break :blk invalid_type).asExpr() orelse break :blk invalid_type;
                const size: u64 = if (size_expr == .literal and size_expr.literal.kind == .int) std.fmt.parseInt(u64, size_expr.literal.value, 0) catch 0 else 0;
                break :blk try self.types.makeArray(elem, size);
            },
            .map => |m| self.types.makeMap(try self.resolveTypeExpr(m.key), try self.resolveTypeExpr(m.value)),
            .list => |e| self.types.makeList(try self.resolveTypeExpr(e)),
            .function => |f| blk: {
                var func_params = std.ArrayListUnmanaged(types.FuncParam){};
                defer func_params.deinit(self.allocator);
                for (f.params) |pt| try func_params.append(self.allocator, .{ .name = "", .type_idx = try self.resolveTypeExpr(pt) });
                const ret_type = if (f.ret != null_node) try self.resolveTypeExpr(f.ret) else TypeRegistry.VOID;
                break :blk try self.types.makeFunc(func_params.items, ret_type);
            },
            .tuple => |elems| blk: {
                var elem_types = std.ArrayListUnmanaged(TypeIndex){};
                defer elem_types.deinit(self.allocator);
                for (elems) |e| try elem_types.append(self.allocator, try self.resolveTypeExpr(e));
                break :blk try self.types.makeTuple(elem_types.items);
            },
            .generic_instance => |gi| try self.resolveGenericInstance(gi, te.span),
        };
    }

    /// Go pattern: types2/instantiate.go:87-125 — instance() for named types.
    /// Creates concrete struct type with substituted fields, deduplicates via cache.
    fn resolveGenericInstance(self: *Checker, gi: ast.GenericInstance, span: Span) CheckError!TypeIndex {
        const gen_info = self.generics.generic_structs.get(gi.name) orelse {
            self.errWithSuggestion(span.start, "undefined generic type", self.findSimilarType(gi.name));
            return invalid_type;
        };

        // Validate argument count
        if (gi.type_args.len != gen_info.type_params.len) {
            self.err.errorWithCode(span.start, .e300, "wrong number of type arguments");
            return invalid_type;
        }

        // Resolve each type argument
        var resolved_args = std.ArrayListUnmanaged(TypeIndex){};
        defer resolved_args.deinit(self.allocator);
        for (gi.type_args) |arg_node| {
            const arg_type = try self.resolveTypeExpr(arg_node);
            try resolved_args.append(self.allocator, arg_type);
        }

        // Build cache key for dedup: "Pair(5,17)" (TypeIndex values)
        const cache_key = try self.buildGenericCacheKey(gi.name, resolved_args.items);

        // Check instantiation cache
        if (self.generics.instantiation_cache.get(cache_key)) |cached| return cached;

        // Create type substitution map
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (gen_info.type_params, 0..) |param_name, i| {
            try sub_map.put(param_name, resolved_args.items[i]);
        }

        // Swap to the defining file's AST for cross-file generic resolution
        const saved_tree = self.tree;
        self.tree = gen_info.tree;
        defer self.tree = saved_tree;

        // Get the struct declaration AST node
        const struct_decl = (self.tree.getNode(gen_info.node_idx) orelse return invalid_type).asDecl() orelse return invalid_type;
        const s = struct_decl.struct_decl;

        // Build concrete struct type with substituted fields
        const old_sub = self.type_substitution;
        self.type_substitution = sub_map;
        const concrete_type = try self.buildStructType(cache_key, s.fields);
        self.type_substitution = old_sub;

        // Register and cache
        try self.types.registerNamed(cache_key, concrete_type);
        try self.generics.instantiation_cache.put(cache_key, concrete_type);

        // Instantiate generic impl block methods for this concrete struct
        // Pass the host tree (saved before swap) so cross-file detection works correctly
        try self.instantiateGenericImplMethods(gi.name, cache_key, resolved_args.items, saved_tree);

        return concrete_type;
    }

    /// Instantiate all methods from generic impl blocks for a concrete struct type.
    /// Called from resolveGenericInstance after creating concrete struct "List(5)".
    /// Go 1.18 pattern: methods on Stack[T] are monomorphized per concrete type.
    ///
    /// Two-pass approach (matches non-generic impl flow: collectDecl then checkDecl):
    ///   Pass 1: Register all method signatures in scope + method registry
    ///   Pass 2: Check all method bodies (sibling methods already visible)
    /// Go reference: named.go expandMethod() uses lazy expansion which naturally
    /// handles forward references; our eager approach needs explicit two-pass.
    fn instantiateGenericImplMethods(
        self: *Checker,
        base_name: []const u8,
        concrete_name: []const u8,
        resolved_args: []const TypeIndex,
        host_tree: *const Ast,
    ) CheckError!void {
        const impl_list = self.generics.generic_impl_blocks.get(base_name) orelse return;
        for (impl_list.items) |impl_info| {
            // Validate type param count matches (Go: named.go:489 checks RecvTypeParams.Len == targs.Len)
            if (impl_info.type_params.len != resolved_args.len) continue;

            // Swap to the defining file's AST and safe_mode for cross-file generic resolution
            const saved_tree = self.tree;
            const saved_safe_mode = self.safe_mode;
            self.tree = impl_info.tree;
            if (impl_info.tree.file) |file| self.safe_mode = file.safe_mode;
            defer {
                self.tree = saved_tree;
                self.safe_mode = saved_safe_mode;
            }

            // Build type substitution map: T -> concrete type
            var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
            defer sub_map.deinit();
            for (impl_info.type_params, 0..) |param_name, i| {
                try sub_map.put(param_name, resolved_args[i]);
            }

            const old_sub = self.type_substitution;
            self.type_substitution = sub_map;
            defer self.type_substitution = old_sub;

            // Pass 1: Register all method signatures (like collectDecl for non-generic impl)
            for (impl_info.methods) |method_idx| {
                const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                if (method_decl != .fn_decl) continue;
                const f = method_decl.fn_decl;

                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_name, f.name });
                const func_type = try self.buildFuncType(f.params, f.return_type);

                if (!self.global_scope.isDefined(synth_name)) {
                    try self.global_scope.define(Symbol.init(synth_name, .function, func_type, method_idx, false));
                }
                try self.types.registerMethod(concrete_name, types.MethodInfo{
                    .name = f.name,
                    .func_name = synth_name,
                    .func_type = func_type,
                    .receiver_is_ptr = true,
                });

                // Add to generic_inst_by_name so the lowerer can find and lower it
                const inst = GenericInstInfo{
                    .concrete_name = synth_name,
                    .generic_node = method_idx,
                    .type_args = try self.allocator.dupe(TypeIndex, resolved_args),
                    .type_param_names = impl_info.type_params,
                    .tree = impl_info.tree,
                };
                try self.generics.generic_inst_by_name.put(synth_name, inst);
            }

            // Pass 2: Check all method bodies (all sibling methods now visible in scope)
            // Cross-file only: save/restore expr_types because foreign AST NodeIndexes
            // collide with the host file's NodeIndex space (both start from 0).
            const is_cross_file = (impl_info.tree != host_tree);
            var saved_expr_types_2: std.AutoHashMap(NodeIndex, TypeIndex) = undefined;
            if (is_cross_file) {
                saved_expr_types_2 = self.expr_types;
                self.expr_types = std.AutoHashMap(NodeIndex, TypeIndex).init(self.allocator);
            }
            defer if (is_cross_file) {
                self.expr_types.deinit();
                self.expr_types = saved_expr_types_2;
            };
            for (impl_info.methods) |method_idx| {
                const method_decl = (self.tree.getNode(method_idx) orelse continue).asDecl() orelse continue;
                if (method_decl != .fn_decl) continue;
                const f = method_decl.fn_decl;

                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_name, f.name });
                try self.checkFnDeclWithName(f, method_idx, synth_name);
            }
        }
    }

    /// Go pattern: types2/instantiate.go:87-125 — instance() creates concrete func,
    /// then types2/call.go:152-166 — check.later() defers body verification.
    /// We eagerly check the body here (simpler than Go's deferred queue for single-threaded).
    fn instantiateGenericFunc(self: *Checker, c: ast.Call, gen_info: GenericInfo, name: []const u8) CheckError!TypeIndex {
        // Validate argument count matches type params
        if (c.args.len != gen_info.type_params.len) {
            self.err.errorWithCode(c.span.start, .e300, "wrong number of type arguments");
            return invalid_type;
        }

        // Resolve each type argument (Go: types2/call.go:46-82 resolves IndexExpr type args)
        var resolved_args = std.ArrayListUnmanaged(TypeIndex){};
        defer resolved_args.deinit(self.allocator);
        for (c.args) |arg_node| {
            const arg_type = try self.resolveTypeExpr(arg_node);
            if (arg_type == invalid_type) {
                const expr = (self.tree.getNode(arg_node) orelse return invalid_type).asExpr() orelse return invalid_type;
                if (expr == .ident) {
                    if (self.types.lookupByName(expr.ident.name)) |tidx| {
                        try resolved_args.append(self.allocator, tidx);
                        continue;
                    }
                }
                self.err.errorWithCode(c.span.start, .e300, "expected type argument");
                return invalid_type;
            }
            try resolved_args.append(self.allocator, arg_type);
        }

        // Rust: rustc_hir_analysis/src/check/wfcheck.rs — validate trait bounds at instantiation.
        // Go 1.18: types2/instantiate.go:115-125 — check.implements(TypeArg, Constraint).
        if (gen_info.type_param_bounds.len > 0) {
            for (gen_info.type_param_bounds, 0..) |bound, i| {
                if (bound) |trait_name| {
                    const type_name = self.types.typeName(resolved_args.items[i]);
                    const impl_key = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ trait_name, type_name });
                    defer self.allocator.free(impl_key);
                    if (self.generics.trait_impls.get(impl_key) == null) {
                        self.err.errorWithCode(c.span.start, .e300, "type does not satisfy trait bound");
                        return invalid_type;
                    }
                }
            }
        }

        // Go pattern: context.go:87-102 — hash-based dedup with identity verification
        const cache_key = try self.buildGenericCacheKey(name, resolved_args.items);

        // Swap to the defining file's AST for cross-file generic resolution
        const saved_tree = self.tree;
        self.tree = gen_info.tree;
        defer self.tree = saved_tree;

        // Get the fn_decl AST node
        const fn_node = (self.tree.getNode(gen_info.node_idx) orelse return invalid_type).asDecl() orelse return invalid_type;
        const f = fn_node.fn_decl;

        // Go pattern: subst.go:13-24 — create substMap mapping TypeParam → concrete Type
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (gen_info.type_params, 0..) |param_name, i| {
            try sub_map.put(param_name, resolved_args.items[i]);
        }

        // Build concrete function type with substituted params and return type
        const old_sub = self.type_substitution;
        self.type_substitution = sub_map;
        const func_type = try self.buildFuncType(f.params, f.return_type);

        // Zig pattern: generic instances belong in the global namespace (not function-local scope)
        if (!self.global_scope.isDefined(cache_key)) {
            try self.global_scope.define(Symbol.init(cache_key, .function, func_type, gen_info.node_idx, false));
        }

        // Go pattern: check.later() defers body verification (call.go:152-166).
        // We check eagerly here since we're single-threaded.
        // Cross-file only: save/restore expr_types to avoid NodeIndex collision.
        const is_cross_file = (gen_info.tree != saved_tree);
        if (is_cross_file) {
            const saved_et = self.expr_types;
            self.expr_types = std.AutoHashMap(NodeIndex, TypeIndex).init(self.allocator);
            try self.checkFnDeclWithName(f, gen_info.node_idx, cache_key);
            self.expr_types.deinit();
            self.expr_types = saved_et;
        } else {
            try self.checkFnDeclWithName(f, gen_info.node_idx, cache_key);
        }
        self.type_substitution = old_sub;

        // Record for the lowerer: this call node is a generic instantiation
        const inst = GenericInstInfo{
            .concrete_name = cache_key,
            .generic_node = gen_info.node_idx,
            .type_args = try self.allocator.dupe(TypeIndex, resolved_args.items),
            .tree = gen_info.tree,
        };
        try self.generic_instantiations.put(c.callee, inst);
        // Also store by concrete name (never overwritten, for nested generic calls)
        try self.generics.generic_inst_by_name.put(cache_key, inst);

        return func_type;
    }

    /// Go pattern: types2/context.go:65-83 — compute deterministic hash from origin + type args.
    /// Go uses typeHasher with '#' separator; we use explicit delimiters to avoid collision.
    /// Format: "Name(5;17)" — semicolons separate TypeIndex values (integers can't contain ';').
    fn buildGenericCacheKey(self: *Checker, name: []const u8, type_args: []const TypeIndex) CheckError![]const u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);
        const writer = buf.writer(self.allocator);
        writer.writeAll(name) catch return error.OutOfMemory;
        writer.writeByte('(') catch return error.OutOfMemory;
        for (type_args, 0..) |arg, i| {
            if (i > 0) writer.writeByte(';') catch return error.OutOfMemory;
            std.fmt.format(writer, "{d}", .{arg}) catch return error.OutOfMemory;
        }
        writer.writeByte(')') catch return error.OutOfMemory;
        return try self.allocator.dupe(u8, buf.items);
    }

    /// In @safe mode, wrap struct types with pointer (C#-style reference semantics).
    /// Handles: Foo → *Foo, Error!Foo → Error!*Foo. Leaves non-struct types unchanged.
    pub fn safeWrapType(self: *Checker, type_idx: TypeIndex) !TypeIndex {
        if (!self.safe_mode) return type_idx;
        const t = self.types.get(type_idx);
        if (t == .struct_type) return try self.types.makePointer(type_idx);
        if (t == .error_union) {
            if (self.types.get(t.error_union.elem) == .struct_type) {
                const wrapped = try self.types.makePointer(t.error_union.elem);
                return try self.types.makeErrorUnionWithSet(wrapped, t.error_union.error_set);
            }
        }
        return type_idx;
    }

    fn buildFuncType(self: *Checker, params: []const ast.Field, return_type_idx: NodeIndex) CheckError!TypeIndex {
        var func_params = std.ArrayListUnmanaged(types.FuncParam){};
        defer func_params.deinit(self.allocator);
        for (params) |param| {
            var type_idx = try self.resolveTypeExpr(param.type_expr);
            type_idx = try self.safeWrapType(type_idx);
            try func_params.append(self.allocator, .{ .name = param.name, .type_idx = type_idx });
        }
        const ret_type = if (return_type_idx != null_node) try self.resolveTypeExpr(return_type_idx) else TypeRegistry.VOID;
        return try self.types.add(.{ .func = .{ .params = try self.allocator.dupe(types.FuncParam, func_params.items), .return_type = ret_type } });
    }

    fn buildStructType(self: *Checker, name: []const u8, fields: []const ast.Field) CheckError!TypeIndex {
        return self.buildStructTypeWithLayout(name, fields, .auto);
    }

    fn buildStructTypeWithLayout(self: *Checker, name: []const u8, fields: []const ast.Field, layout: ast.StructLayout) CheckError!TypeIndex {
        var struct_fields = std.ArrayListUnmanaged(types.StructField){};
        defer struct_fields.deinit(self.allocator);
        var offset: u32 = 0;
        var max_align: u32 = 1;
        for (fields) |field| {
            const field_type = try self.resolveTypeExpr(field.type_expr);
            switch (layout) {
                .@"packed" => {
                    // Packed: no alignment, contiguous fields
                    try struct_fields.append(self.allocator, .{ .name = field.name, .type_idx = field_type, .offset = offset, .default_value = field.default_value });
                    offset += self.types.sizeOf(field_type);
                },
                .@"extern" => {
                    // Extern: C ABI layout, align each field to its natural alignment
                    const field_align = self.types.alignmentOf(field_type);
                    if (field_align > max_align) max_align = field_align;
                    if (field_align > 0) offset = (offset + field_align - 1) & ~(field_align - 1);
                    try struct_fields.append(self.allocator, .{ .name = field.name, .type_idx = field_type, .offset = offset, .default_value = field.default_value });
                    offset += self.types.sizeOf(field_type);
                },
                .auto => {
                    // Auto: existing behavior (align each field, 8-byte struct alignment)
                    const field_align = self.types.alignmentOf(field_type);
                    if (field_align > 0) offset = (offset + field_align - 1) & ~(field_align - 1);
                    try struct_fields.append(self.allocator, .{ .name = field.name, .type_idx = field_type, .offset = offset, .default_value = field.default_value });
                    offset += self.types.sizeOf(field_type);
                },
            }
        }
        // Final padding: packed = none, extern = align to max field alignment, auto = 8-byte
        const alignment: u8 = switch (layout) {
            .@"packed" => 1,
            .@"extern" => @intCast(max_align),
            .auto => 8,
        };
        if (layout != .@"packed" and alignment > 1) {
            const a = @as(u32, alignment);
            offset = (offset + a - 1) & ~(a - 1);
        }
        return try self.types.add(.{ .struct_type = .{ .name = name, .fields = try self.allocator.dupe(types.StructField, struct_fields.items), .size = offset, .alignment = alignment, .layout = layout } });
    }

    fn buildEnumType(self: *Checker, e: ast.EnumDecl) CheckError!TypeIndex {
        var backing_type: TypeIndex = TypeRegistry.I32;
        if (e.backing_type != null_node) backing_type = try self.resolveTypeExpr(e.backing_type);
        var enum_variants = std.ArrayListUnmanaged(types.EnumVariant){};
        defer enum_variants.deinit(self.allocator);
        var next_value: i64 = 0;
        for (e.variants) |variant| {
            var value = next_value;
            if (variant.value != null_node) {
                if (self.tree.getNode(variant.value)) |n| {
                    if (n.asExpr()) |ex| {
                        if (ex == .literal and ex.literal.kind == .int)
                            value = std.fmt.parseInt(i64, ex.literal.value, 0) catch 0;
                    }
                }
            }
            try enum_variants.append(self.allocator, .{ .name = variant.name, .value = value });
            next_value = value + 1;
        }
        return try self.types.add(.{ .enum_type = .{ .name = e.name, .backing_type = backing_type, .variants = try self.allocator.dupe(types.EnumVariant, enum_variants.items) } });
    }

    fn buildUnionType(self: *Checker, u: ast.UnionDecl) CheckError!TypeIndex {
        var union_variants = std.ArrayListUnmanaged(types.UnionVariant){};
        defer union_variants.deinit(self.allocator);
        for (u.variants) |variant| {
            const payload_type = if (variant.type_expr != null_node) try self.resolveTypeExpr(variant.type_expr) else invalid_type;
            try union_variants.append(self.allocator, .{ .name = variant.name, .payload_type = payload_type });
        }
        const tag_type: TypeIndex = if (u.variants.len <= 256) TypeRegistry.U8 else TypeRegistry.U16;
        return try self.types.add(.{ .union_type = .{ .name = u.name, .variants = try self.allocator.dupe(types.UnionVariant, union_variants.items), .tag_type = tag_type } });
    }

    fn materializeType(self: *Checker, idx: TypeIndex) TypeIndex {
        const t = self.types.get(idx);
        return switch (t) {
            .basic => |k| switch (k) { .untyped_int => TypeRegistry.INT, .untyped_float => TypeRegistry.FLOAT, .untyped_bool => TypeRegistry.BOOL, else => idx },
            .array => |arr| blk: { const me = self.materializeType(arr.elem); if (me == arr.elem) break :blk idx; break :blk self.types.makeArray(me, arr.length) catch idx; },
            .slice => |sl| blk: { const me = self.materializeType(sl.elem); if (me == sl.elem) break :blk idx; break :blk self.types.makeSlice(me) catch idx; },
            else => idx,
        };
    }

    fn isComparable(self: *Checker, a: TypeIndex, b: TypeIndex) bool {
        if (self.types.equal(a, b)) return true;
        const ta = self.types.get(a);
        const tb = self.types.get(b);
        if (types.isNumeric(ta) and types.isNumeric(tb)) return true;
        if (ta == .slice and tb == .slice and ta.slice.elem == TypeRegistry.U8 and tb.slice.elem == TypeRegistry.U8) return true;
        if (ta == .optional and tb == .basic and tb.basic == .untyped_null) return true;
        if (tb == .optional and ta == .basic and ta.basic == .untyped_null) return true;
        if (ta == .pointer and tb == .basic and tb.basic == .untyped_null) return true;
        if (tb == .pointer and ta == .basic and ta.basic == .untyped_null) return true;
        // Enum values are comparable to their backing type (integer) — Zig: @intFromEnum
        if (ta == .enum_type and types.isInteger(tb)) return true;
        if (tb == .enum_type and types.isInteger(ta)) return true;
        return false;
    }

    // ========================================================================
    // "Did you mean X?" — Levenshtein edit distance suggestions
    // Zig heuristic: distance <= 2 AND distance <= len/2
    // ========================================================================

    const MAX_EDIT_LEN = 64;

    /// Levenshtein edit distance. Stack-allocated, O(min(m,n)) space.
    /// Returns maxInt for names longer than MAX_EDIT_LEN.
    fn editDistance(a: []const u8, b: []const u8) usize {
        if (a.len > MAX_EDIT_LEN or b.len > MAX_EDIT_LEN) return std.math.maxInt(usize);
        if (a.len == 0) return b.len;
        if (b.len == 0) return a.len;
        if (std.mem.eql(u8, a, b)) return 0;

        // Use shorter string for the row to minimize stack usage
        const s1 = if (a.len <= b.len) a else b;
        const s2 = if (a.len <= b.len) b else a;

        var prev: [MAX_EDIT_LEN + 1]usize = undefined;
        var curr: [MAX_EDIT_LEN + 1]usize = undefined;

        for (0..s1.len + 1) |i| prev[i] = i;

        for (s2, 0..) |c2, j| {
            curr[0] = j + 1;
            for (s1, 0..) |c1, i| {
                const cost: usize = if (c1 == c2) 0 else 1;
                curr[i + 1] = @min(@min(curr[i] + 1, prev[i + 1] + 1), prev[i] + cost);
            }
            @memcpy(prev[0 .. s1.len + 1], curr[0 .. s1.len + 1]);
        }
        return prev[s1.len];
    }

    /// Check if a name is a valid suggestion candidate (not internal/generated).
    fn isUserVisibleName(name: []const u8) bool {
        if (name.len == 0) return false;
        // Skip generic instantiation names like "List_append(5)"
        if (std.mem.indexOfScalar(u8, name, '(') != null) return false;
        // Skip names starting with __ (internal)
        if (name.len >= 2 and name[0] == '_' and name[1] == '_') return false;
        return true;
    }

    /// Search scope chain + global scope for the closest match to `name`.
    fn findSimilarName(self: *Checker, name: []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);

        // Walk scope chain
        var scope: ?*const Scope = self.scope;
        while (scope) |s| {
            var it = s.symbols.iterator();
            while (it.next()) |entry| {
                const candidate = entry.key_ptr.*;
                if (std.mem.eql(u8, candidate, name)) continue;
                if (!isUserVisibleName(candidate)) continue;
                const d = editDistance(name, candidate);
                if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                    best_dist = d;
                    best = candidate;
                }
            }
            scope = s.parent;
        }

        // Also check global scope (may not be in chain if we're in a nested scope)
        {
            var it = self.global_scope.symbols.iterator();
            while (it.next()) |entry| {
                const candidate = entry.key_ptr.*;
                if (std.mem.eql(u8, candidate, name)) continue;
                if (!isUserVisibleName(candidate)) continue;
                const d = editDistance(name, candidate);
                if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                    best_dist = d;
                    best = candidate;
                }
            }
        }

        // Check builtin function names
        const builtins = [_][]const u8{
            "print", "println", "eprint", "eprintln", "len", "append",
        };
        for (builtins) |candidate| {
            if (std.mem.eql(u8, candidate, name)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = candidate;
            }
        }

        return best;
    }

    /// Search struct fields for the closest match to `name`.
    fn findSimilarField(name: []const u8, fields: []const types.StructField) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (fields) |field| {
            if (std.mem.eql(u8, field.name, name)) continue;
            const d = editDistance(name, field.name);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = field.name;
            }
        }
        return best;
    }

    /// Search enum/union variants for the closest match to `name`.
    fn findSimilarVariant(name: []const u8, variants: anytype) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (variants) |v| {
            if (std.mem.eql(u8, v.name, name)) continue;
            const d = editDistance(name, v.name);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = v.name;
            }
        }
        return best;
    }

    /// Search type names for the closest match.
    fn findSimilarType(self: *Checker, name: []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);

        var it = self.types.name_map.iterator();
        while (it.next()) |entry| {
            const candidate = entry.key_ptr.*;
            if (std.mem.eql(u8, candidate, name)) continue;
            if (!isUserVisibleName(candidate)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = candidate;
            }
        }

        // Also check generic struct names
        var git = self.generics.generic_structs.iterator();
        while (git.next()) |entry| {
            const candidate = entry.key_ptr.*;
            if (std.mem.eql(u8, candidate, name)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = candidate;
            }
        }

        return best;
    }

    /// Search a static list of names for the closest match.
    fn editDistSuggest(name: []const u8, candidates: []const []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (candidates) |c| {
            if (std.mem.eql(u8, c, name)) continue;
            const d = editDistance(name, c);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) {
                best_dist = d;
                best = c;
            }
        }
        return best;
    }

    /// Format an error message with a "did you mean" suggestion.
    fn errWithSuggestion(self: *Checker, pos: Pos, msg: []const u8, suggestion: ?[]const u8) void {
        if (suggestion) |s| {
            const full_msg = std.fmt.allocPrint(self.allocator, "{s}; did you mean '{s}'?", .{ msg, s }) catch {
                self.err.errorWithCode(pos, .e301, msg);
                return;
            };
            self.err.errorWithCode(pos, .e301, full_msg);
        } else {
            self.err.errorWithCode(pos, .e301, msg);
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Scope define and lookup" {
    var scope = Scope.init(std.testing.allocator, null);
    defer scope.deinit();
    try scope.define(Symbol.init("x", .variable, TypeRegistry.INT, 0, true));
    const sym = scope.lookup("x");
    try std.testing.expect(sym != null);
    try std.testing.expectEqualStrings("x", sym.?.name);
}

test "Scope parent lookup" {
    var parent = Scope.init(std.testing.allocator, null);
    defer parent.deinit();
    try parent.define(Symbol.init("x", .variable, TypeRegistry.INT, 0, true));
    var child = Scope.init(std.testing.allocator, &parent);
    defer child.deinit();
    try child.define(Symbol.init("y", .variable, TypeRegistry.BOOL, 1, false));
    try std.testing.expect(child.lookup("y") != null);
    try std.testing.expect(child.lookup("x") != null);
    try std.testing.expect(parent.lookup("y") == null);
}

test "Scope isDefined only checks local" {
    var parent = Scope.init(std.testing.allocator, null);
    defer parent.deinit();
    try parent.define(Symbol.init("x", .variable, TypeRegistry.INT, 0, true));
    var child = Scope.init(std.testing.allocator, &parent);
    defer child.deinit();
    try std.testing.expect(!child.isDefined("x"));
    try std.testing.expect(child.lookup("x") != null);
}

test "Symbol init" {
    const sym = Symbol.init("foo", .function, TypeRegistry.VOID, 42, false);
    try std.testing.expectEqualStrings("foo", sym.name);
    try std.testing.expectEqual(SymbolKind.function, sym.kind);
}

test "checker type registry lookup" {
    var type_reg = try TypeRegistry.init(std.testing.allocator);
    defer type_reg.deinit();
    try std.testing.expectEqual(TypeRegistry.INT, type_reg.lookupByName("int").?);
    try std.testing.expectEqual(TypeRegistry.BOOL, type_reg.lookupByName("bool").?);
}

test "editDistance: basic cases" {
    try std.testing.expectEqual(@as(usize, 0), Checker.editDistance("hello", "hello"));
    try std.testing.expectEqual(@as(usize, 1), Checker.editDistance("hello", "helo"));
    try std.testing.expectEqual(@as(usize, 1), Checker.editDistance("hello", "hullo"));
    try std.testing.expectEqual(@as(usize, 1), Checker.editDistance("hello", "hllo"));
    try std.testing.expectEqual(@as(usize, 2), Checker.editDistance("pritnln", "println"));
    try std.testing.expectEqual(@as(usize, 4), Checker.editDistance("hello", "world"));
}

test "editDistance: edge cases" {
    try std.testing.expectEqual(@as(usize, 0), Checker.editDistance("", ""));
    try std.testing.expectEqual(@as(usize, 3), Checker.editDistance("", "abc"));
    try std.testing.expectEqual(@as(usize, 3), Checker.editDistance("abc", ""));
    try std.testing.expectEqual(@as(usize, 1), Checker.editDistance("a", "b"));
}

test "editDistSuggest: finds closest match" {
    const result = Checker.editDistSuggest("pritnln", &.{ "print", "println", "eprint" });
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("println", result.?);
}

test "editDistSuggest: no match when too distant" {
    const result = Checker.editDistSuggest("xyz", &.{ "print", "println" });
    try std.testing.expect(result == null);
}

test "editDistSuggest: no match for short names with distant candidates" {
    // "ab" vs "xy" = distance 2, but 2 * 2 > 2 (len), so rejected by threshold
    const result = Checker.editDistSuggest("ab", &.{ "xy", "zz" });
    try std.testing.expect(result == null);
}
