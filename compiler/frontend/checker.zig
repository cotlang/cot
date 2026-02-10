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

    pub fn init(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: NodeIndex, mutable: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable };
    }

    pub fn initExtern(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: NodeIndex, mutable: bool, is_extern: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable, .is_extern = is_extern };
    }

    pub fn initConst(name: []const u8, type_idx: TypeIndex, node: NodeIndex, value: i64) Symbol {
        return .{ .name = name, .kind = .constant, .type_idx = type_idx, .node = node, .mutable = false, .const_value = value };
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
    in_loop: bool = false,
    // Generics support (Zig-style lazy monomorphization + Go checker flow)
    // Shared across all checkers in multi-file mode (owned by driver)
    generics: *SharedGenericContext,
    // Per-checker: keyed by call-site NodeIndex (file-specific)
    generic_instantiations: std.AutoHashMap(NodeIndex, GenericInstInfo) = undefined,
    /// Type substitution map, active during generic instantiation
    type_substitution: ?std.StringHashMap(TypeIndex) = null,
    /// Compilation target — used for @target_os(), @target_arch(), @target() comptime builtins
    target: target_mod.Target = target_mod.Target.native(),

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
    pub fn checkFile(self: *Checker) CheckError!void {
        const file = self.tree.file orelse return;
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
                const func_type = try self.buildFuncType(f.params, f.return_type);
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
                const struct_type = try self.buildStructType(s.name, s.fields);
                try self.scope.define(Symbol.init(s.name, .type_name, struct_type, idx, false));
                try self.types.registerNamed(s.name, struct_type);
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
                        try self.scope.define(Symbol.initExtern(synth_name, .function, func_type, method_idx, false, false));
                        try self.types.registerMethod(impl_b.type_name, types.MethodInfo{ .name = f.name, .func_name = synth_name, .func_type = func_type, .receiver_is_ptr = true });
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
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    pub fn checkFnDeclWithName(self: *Checker, f: ast.FnDecl, idx: NodeIndex, lookup_name: []const u8) CheckError!void {
        const sym = self.scope.lookup(lookup_name) orelse return;
        const return_type = if (self.types.get(sym.type_idx) == .func) self.types.get(sym.type_idx).func.return_type else TypeRegistry.VOID;
        var func_scope = Scope.init(self.allocator, self.scope);
        defer func_scope.deinit();
        for (f.params) |param| {
            const param_type = try self.resolveTypeExpr(param.type_expr);
            try func_scope.define(Symbol.init(param.name, .parameter, param_type, idx, false));
        }
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &func_scope;
        self.current_return_type = return_type;
        if (f.body != null_node) try self.checkBlockExpr(f.body);
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    fn checkVarDecl(self: *Checker, v: ast.VarDecl, idx: NodeIndex) CheckError!void {
        var var_type: TypeIndex = if (v.type_expr != null_node) try self.resolveTypeExpr(v.type_expr) else invalid_type;
        if (v.value != null_node and !self.isUndefinedLit(v.value)) {
            const val_type = try self.checkExpr(v.value);
            if (var_type == invalid_type) var_type = self.materializeType(val_type)
            else if (!self.types.isAssignable(val_type, var_type)) self.err.errorWithCode(v.span.start, .e300, "type mismatch");
        }
        if (self.scope.lookupLocal(v.name) != null) {
            if (v.is_const and v.value != null_node) if (self.evalConstExpr(v.value)) |cv| {
                try self.scope.define(Symbol.initConst(v.name, var_type, idx, cv));
                return;
            };
            try self.scope.define(Symbol.init(v.name, if (v.is_const) .constant else .variable, var_type, idx, !v.is_const));
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
                if (std.mem.eql(u8, bc.name, "sizeOf")) {
                    const type_idx = self.resolveTypeExpr(bc.type_arg) catch return null;
                    if (type_idx == invalid_type) return null;
                    return @as(i64, @intCast(self.types.sizeOf(type_idx)));
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
            else => null,
        };
    }

    /// Evaluate a comptime string expression. Returns the string value or null.
    fn evalConstString(self: *Checker, idx: NodeIndex) ?[]const u8 {
        const expr = (self.tree.getNode(idx) orelse return null).asExpr() orelse return null;
        return switch (expr) {
            .literal => |lit| if (lit.kind == .string) lit.value else null,
            .paren => |p| self.evalConstString(p.inner),
            .builtin_call => |bc| {
                if (std.mem.eql(u8, bc.name, "target_os")) return self.target.os.name();
                if (std.mem.eql(u8, bc.name, "target_arch")) return self.target.arch.name();
                if (std.mem.eql(u8, bc.name, "target")) return self.target.name();
                return null;
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
            .catch_expr => |ce| self.checkCatchExpr(ce),
            .error_literal => |el| self.checkErrorLiteral(el),
            .closure_expr => |ce| self.checkClosureExpr(ce),
            .addr_of => |ao| self.checkAddrOf(ao),
            .deref => |d| self.checkDeref(d),
            .tuple_literal => |tl| self.checkTupleLiteral(tl),
            .zero_init => TypeRegistry.VOID, // Type inferred from var decl context
            .type_expr, .bad_expr => invalid_type,
        };
    }

    fn checkIdentifier(self: *Checker, id: ast.Ident) TypeIndex {
        if (self.types.lookupByName(id.name)) |type_idx| {
            const t = self.types.get(type_idx);
            // Allow enum and union types to be used as expressions (for variant access)
            if (t == .enum_type or t == .union_type) return type_idx;
            self.err.errorWithCode(id.span.start, .e301, "type name cannot be used as expression");
            return invalid_type;
        }
        // Go pattern: generic function names cannot be used without instantiation.
        // Go types2/call.go:33 — funcInst() requires IndexExpr (explicit type args).
        if (self.generics.generic_functions.contains(id.name)) {
            self.err.errorWithCode(id.span.start, .e300, "generic function requires type arguments");
            return invalid_type;
        }
        if (self.scope.lookup(id.name)) |sym| return sym.type_idx;
        // Check type substitution (for type params used as values)
        if (self.type_substitution) |sub| {
            if (sub.get(id.name)) |_| return invalid_type;
        }
        self.err.errorWithCode(id.span.start, .e301, "undefined identifier");
        return invalid_type;
    }

    fn checkLiteral(_: *Checker, lit: ast.Literal) TypeIndex {
        return switch (lit.kind) {
            .int => TypeRegistry.UNTYPED_INT, .float => TypeRegistry.UNTYPED_FLOAT,
            .string => TypeRegistry.STRING, .char => TypeRegistry.U8,
            .true_lit, .false_lit => TypeRegistry.UNTYPED_BOOL,
            .null_lit, .undefined_lit => TypeRegistry.UNTYPED_NULL,
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
                return self.materializeType(left_type);
            },
            .sub => {
                if (left == .pointer and types.isInteger(right)) return left_type;
                if (!types.isNumeric(left) or !types.isNumeric(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return self.materializeType(left_type);
            },
            .mul, .quo, .rem => {
                if (!types.isNumeric(left) or !types.isNumeric(right)) { self.err.errorWithCode(bin.span.start, .e300, "invalid operation"); return invalid_type; }
                return self.materializeType(left_type);
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
                return self.materializeType(left_type);
            },
            .coalesce => return if (left == .optional) left.optional.elem else left_type,
            else => return invalid_type,
        }
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
            .pointer => |ptr| if (self.types.get(ptr.elem) == .struct_type) self.types.get(ptr.elem).struct_type.name else return null,
            .basic => |bk| bk.name(),
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
        if (std.mem.eql(u8, bc.name, "sizeOf") or std.mem.eql(u8, bc.name, "alignOf")) {
            const type_idx = try self.resolveTypeExpr(bc.type_arg);
            if (type_idx == invalid_type) { self.err.errorWithCode(bc.span.start, .e300, "requires valid type"); return invalid_type; }
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "string")) {
            _ = try self.checkExpr(bc.args[0]);
            _ = try self.checkExpr(bc.args[1]);
            return TypeRegistry.STRING;
        } else if (std.mem.eql(u8, bc.name, "intCast")) {
            const target_type = try self.resolveTypeExpr(bc.type_arg);
            if (!types.isInteger(self.types.get(target_type))) { self.err.errorWithCode(bc.span.start, .e300, "@intCast target must be integer"); return invalid_type; }
            _ = try self.checkExpr(bc.args[0]);
            return target_type;
        } else if (std.mem.eql(u8, bc.name, "ptrCast")) {
            const target_type = try self.resolveTypeExpr(bc.type_arg);
            if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(bc.span.start, .e300, "@ptrCast target must be pointer"); return invalid_type; }
            _ = try self.checkExpr(bc.args[0]);
            return target_type;
        } else if (std.mem.eql(u8, bc.name, "ptrToInt")) {
            _ = try self.checkExpr(bc.args[0]);
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "intToPtr")) {
            const target_type = try self.resolveTypeExpr(bc.type_arg);
            if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(bc.span.start, .e300, "@intToPtr target must be pointer"); return invalid_type; }
            _ = try self.checkExpr(bc.args[0]);
            return target_type;
        } else if (std.mem.eql(u8, bc.name, "assert")) {
            _ = try self.checkExpr(bc.args[0]);
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "assert_eq")) {
            _ = try self.checkExpr(bc.args[0]);
            _ = try self.checkExpr(bc.args[1]);
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "alloc")) {
            _ = try self.checkExpr(bc.args[0]); // size: i64
            return TypeRegistry.I64; // raw pointer as i64
        } else if (std.mem.eql(u8, bc.name, "dealloc")) {
            _ = try self.checkExpr(bc.args[0]); // ptr: i64
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "realloc")) {
            _ = try self.checkExpr(bc.args[0]); // ptr: i64
            _ = try self.checkExpr(bc.args[1]); // new_size: i64
            return TypeRegistry.I64; // new pointer as i64
        } else if (std.mem.eql(u8, bc.name, "memcpy")) {
            // @memcpy(dst, src, num_bytes) — Go's memmove / Wasm memory.copy pattern
            _ = try self.checkExpr(bc.args[0]); // dst: i64 (raw pointer)
            _ = try self.checkExpr(bc.args[1]); // src: i64 (raw pointer)
            _ = try self.checkExpr(bc.args[2]); // num_bytes: i64
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "fd_write")) {
            // @fd_write(fd, ptr, len) — WASI fd_write via adapter
            _ = try self.checkExpr(bc.args[0]); // fd: i64
            _ = try self.checkExpr(bc.args[1]); // ptr: i64
            _ = try self.checkExpr(bc.args[2]); // len: i64
            return TypeRegistry.I64; // returns bytes written
        } else if (std.mem.eql(u8, bc.name, "fd_read")) {
            // @fd_read(fd, buf, len) — read from fd into buffer
            // Reference: Go syscall/fs_wasip1.go:900 Read()
            _ = try self.checkExpr(bc.args[0]); // fd: i64
            _ = try self.checkExpr(bc.args[1]); // buf: i64 (pointer into linear memory)
            _ = try self.checkExpr(bc.args[2]); // len: i64 (max bytes to read)
            return TypeRegistry.I64; // returns bytes read (0 = EOF)
        } else if (std.mem.eql(u8, bc.name, "fd_close")) {
            // @fd_close(fd) — close a file descriptor
            // Reference: Go syscall/fs_wasip1.go fd_close(fd int32) Errno
            _ = try self.checkExpr(bc.args[0]); // fd: i64
            return TypeRegistry.I64; // returns 0 on success
        } else if (std.mem.eql(u8, bc.name, "fd_seek")) {
            // @fd_seek(fd, offset, whence) — seek in file descriptor
            // Reference: Go syscall/fs_wasip1.go:928 Seek() — lseek(fd, offset, whence)
            // Whence: 0=SEEK_SET, 1=SEEK_CUR, 2=SEEK_END
            _ = try self.checkExpr(bc.args[0]); // fd: i64
            _ = try self.checkExpr(bc.args[1]); // offset: i64 (signed)
            _ = try self.checkExpr(bc.args[2]); // whence: i64 (0=SET, 1=CUR, 2=END)
            return TypeRegistry.I64; // returns new offset
        } else if (std.mem.eql(u8, bc.name, "fd_open")) {
            // @fd_open(path_ptr, path_len, flags) — open file by path
            // Reference: Go zsyscall_darwin_arm64.go openat(AT_FDCWD, path, flags, mode)
            _ = try self.checkExpr(bc.args[0]); // path_ptr: i64 (pointer into linear memory)
            _ = try self.checkExpr(bc.args[1]); // path_len: i64
            _ = try self.checkExpr(bc.args[2]); // flags: i64 (O_RDONLY=0, O_WRONLY=1, O_RDWR=2, +O_CREAT, etc.)
            return TypeRegistry.I64; // returns fd on success, errno on error
        } else if (std.mem.eql(u8, bc.name, "ptrOf")) {
            // @ptrOf(string_expr) — get raw pointer of a string as i64
            const arg_type = try self.checkExpr(bc.args[0]);
            if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(bc.span.start, .e300, "@ptrOf requires string argument"); return invalid_type; }
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "lenOf")) {
            // @lenOf(string_expr) — get length of a string as i64
            const arg_type = try self.checkExpr(bc.args[0]);
            if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(bc.span.start, .e300, "@lenOf requires string argument"); return invalid_type; }
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "trap")) {
            // @trap() — Wasm unreachable / ARM64 brk #1 / x64 ud2
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "exit")) {
            // @exit(code) — exit the process with given exit code (no return)
            // Reference: WASI proc_exit(rval), macOS SYS_exit(1)
            _ = try self.checkExpr(bc.args[0]); // code: i64
            return TypeRegistry.VOID;
        } else if (std.mem.eql(u8, bc.name, "time")) {
            // @time() — returns nanoseconds since epoch as i64
            // Reference: Go runtime/sys_darwin_arm64.s walltime_trampoline → clock_gettime(CLOCK_REALTIME)
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "random")) {
            // @random(buf, len) — fill buffer with cryptographic random bytes
            // Reference: Go runtime/sys_darwin_arm64.s arc4random_buf_trampoline
            _ = try self.checkExpr(bc.args[0]); // buf: i64 (pointer into linear memory)
            _ = try self.checkExpr(bc.args[1]); // len: i64
            return TypeRegistry.I64; // returns 0 on success, errno on error
        } else if (std.mem.eql(u8, bc.name, "args_count")) {
            // @args_count() — returns number of CLI arguments (argc)
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "arg_len")) {
            // @arg_len(n) — returns length of CLI argument n (strlen(argv[n]))
            _ = try self.checkExpr(bc.args[0]); // n: i64 (argument index)
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "arg_ptr")) {
            // @arg_ptr(n) — copies CLI argument n into linear memory, returns wasm pointer
            _ = try self.checkExpr(bc.args[0]); // n: i64 (argument index)
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "environ_count")) {
            // @environ_count() — returns number of environment variables
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "environ_len")) {
            // @environ_len(n) — returns length of env var n
            _ = try self.checkExpr(bc.args[0]);
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "environ_ptr")) {
            // @environ_ptr(n) — copies env var n into linear memory, returns wasm pointer
            _ = try self.checkExpr(bc.args[0]);
            return TypeRegistry.I64;
        } else if (std.mem.eql(u8, bc.name, "target_os") or std.mem.eql(u8, bc.name, "target_arch") or std.mem.eql(u8, bc.name, "target")) {
            // Comptime builtins — resolve to string constants at compile time
            return TypeRegistry.STRING;
        }
        self.err.errorWithCode(bc.span.start, .e300, "unknown builtin");
        return invalid_type;
    }

    fn checkIndex(self: *Checker, i: ast.Index) CheckError!TypeIndex {
        const base_type = try self.checkExpr(i.base);
        const index_type = try self.checkExpr(i.idx);
        if (!types.isInteger(self.types.get(index_type))) { self.err.errorWithCode(i.span.start, .e300, "index must be integer"); return invalid_type; }
        if (base_type == TypeRegistry.STRING) return TypeRegistry.U8;
        const base = self.types.get(base_type);
        return switch (base) { .array => |a| a.elem, .slice => |s| s.elem, .list => |l| l.elem, else => blk: { self.err.errorWithCode(i.span.start, .e300, "cannot index this type"); break :blk invalid_type; } };
    }

    fn checkSliceExpr(self: *Checker, se: ast.SliceExpr) CheckError!TypeIndex {
        const base_type = try self.checkExpr(se.base);
        if (se.start != null_node) _ = try self.checkExpr(se.start);
        if (se.end != null_node) _ = try self.checkExpr(se.end);
        const base = self.types.get(base_type);
        return switch (base) { .array => |a| self.types.makeSlice(a.elem), .slice => base_type, else => blk: { self.err.errorWithCode(se.span.start, .e300, "cannot slice this type"); break :blk invalid_type; } };
    }

    fn checkFieldAccess(self: *Checker, f: ast.FieldAccess) CheckError!TypeIndex {
        if (f.base == null_node) return invalid_type;
        const base_type = try self.checkExpr(f.base);
        const base = self.types.get(base_type);

        switch (base) {
            .struct_type => |st| {
                for (st.fields) |field| if (std.mem.eql(u8, field.name, f.field)) return field.type_idx;
                if (self.lookupMethod(st.name, f.field)) |m| return m.func_type;
                self.err.errorWithCode(f.span.start, .e301, "undefined field");
                return invalid_type;
            },
            .enum_type => |et| {
                for (et.variants) |v| if (std.mem.eql(u8, v.name, f.field)) return base_type;
                self.err.errorWithCode(f.span.start, .e301, "undefined variant");
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
                self.err.errorWithCode(f.span.start, .e301, "undefined variant");
                return invalid_type;
            },
            .pointer => |ptr| {
                const elem = self.types.get(ptr.elem);
                if (elem == .struct_type) {
                    for (elem.struct_type.fields) |field| if (std.mem.eql(u8, field.name, f.field)) return field.type_idx;
                    if (self.lookupMethod(elem.struct_type.name, f.field)) |m| return m.func_type;
                }
                self.err.errorWithCode(f.span.start, .e300, "cannot access field on this type");
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
                self.err.errorWithCode(f.span.start, .e301, "undefined field");
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
                self.err.errorWithCode(f.span.start, .e301, "undefined field");
                return invalid_type;
            },
            .slice => |sl| {
                if (std.mem.eql(u8, f.field, "ptr")) return try self.types.add(.{ .pointer = .{ .elem = sl.elem } })
                else if (std.mem.eql(u8, f.field, "len")) return TypeRegistry.I64;
                self.err.errorWithCode(f.span.start, .e301, "undefined field");
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
        const struct_type_idx = if (si.type_args.len > 0)
            try self.resolveGenericInstance(.{ .name = si.type_name, .type_args = si.type_args }, si.span)
        else
            self.types.lookupByName(si.type_name) orelse {
                self.err.errorWithCode(si.span.start, .e301, "undefined type");
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
            if (!found) self.err.errorWithCode(fi.span.start, .e301, "unknown field");
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
                self.err.errorWithCode(ne.span.start, .e301, "undefined type");
                return invalid_type;
            };
        const struct_type = self.types.get(struct_type_idx);
        if (struct_type != .struct_type) {
            self.err.errorWithCode(ne.span.start, .e300, "new requires a struct type");
            return invalid_type;
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
            if (!found) self.err.errorWithCode(fi.span.start, .e301, "unknown field");
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
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(ie.span.start, .e300, "condition must be bool");
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
        for (b.stmts) |stmt_idx| try self.checkStmt(stmt_idx);
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

    fn checkCatchExpr(self: *Checker, ce: ast.CatchExpr) CheckError!TypeIndex {
        const operand_type = try self.checkExpr(ce.operand);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .error_union) {
            self.err.errorWithCode(ce.span.start, .e300, "catch requires error union type");
            return try self.checkExpr(ce.fallback);
        }
        const elem_type = operand_info.error_union.elem;
        _ = try self.checkExpr(ce.fallback);
        return elem_type;
    }

    fn checkErrorLiteral(self: *Checker, el: ast.ErrorLiteral) CheckError!TypeIndex {
        // error.X returns an error set type; the specific set is inferred from context
        // For now, look up any error set that has this variant
        const ret_info = self.types.get(self.current_return_type);
        if (ret_info == .error_union and ret_info.error_union.error_set != types.invalid_type) {
            const es_info = self.types.get(ret_info.error_union.error_set);
            if (es_info == .error_set) {
                for (es_info.error_set.variants) |v| {
                    if (std.mem.eql(u8, v, el.error_name)) return ret_info.error_union.error_set;
                }
            }
        }
        // Allow it through type checking, resolve at lowering time
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
            .bad_stmt => {},
        }
    }

    fn checkReturn(self: *Checker, rs: ast.ReturnStmt) CheckError!void {
        if (rs.value != null_node) {
            const val_type = try self.checkExpr(rs.value);
            if (self.current_return_type == TypeRegistry.VOID) self.err.errorWithCode(rs.span.start, .e300, "void function should not return a value")
            else if (!self.types.isAssignable(val_type, self.current_return_type)) self.err.errorWithCode(rs.span.start, .e300, "type mismatch");
        } else if (self.current_return_type != TypeRegistry.VOID) self.err.errorWithCode(rs.span.start, .e300, "non-void function must return a value");
    }

    fn checkVarStmt(self: *Checker, vs: ast.VarStmt, idx: NodeIndex) CheckError!void {
        if (self.scope.isDefined(vs.name)) { self.err.errorWithCode(vs.span.start, .e302, "redefined identifier"); return; }
        var var_type: TypeIndex = if (vs.type_expr != null_node) try self.resolveTypeExpr(vs.type_expr) else invalid_type;
        if (vs.value != null_node and !self.isUndefinedLit(vs.value) and !self.isZeroInitLit(vs.value)) {
            const val_type = try self.checkExpr(vs.value);
            if (var_type == invalid_type) var_type = self.materializeType(val_type)
            else if (!self.types.isAssignable(val_type, var_type)) self.err.errorWithCode(vs.span.start, .e300, "type mismatch");
        }
        if (self.isZeroInitLit(vs.value) and var_type == invalid_type) {
            self.err.errorWithCode(vs.span.start, .e300, "zero init requires type annotation");
        }
        try self.scope.define(Symbol.init(vs.name, if (vs.is_const) .constant else .variable, var_type, idx, !vs.is_const));
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
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(is.span.start, .e300, "condition must be bool");
        try self.checkStmt(is.then_branch);
        if (is.else_branch != null_node) try self.checkStmt(is.else_branch);
    }

    fn checkWhileStmt(self: *Checker, ws: ast.WhileStmt) CheckError!void {
        const cond_type = try self.checkExpr(ws.condition);
        if (!types.isBool(self.types.get(cond_type))) self.err.errorWithCode(ws.span.start, .e300, "condition must be bool");
        const old_in_loop = self.in_loop;
        self.in_loop = true;
        try self.checkStmt(ws.body);
        self.in_loop = old_in_loop;
    }

    fn checkForStmt(self: *Checker, fs: ast.ForStmt) CheckError!void {
        // Handle range loop: for i in start..end
        const elem_type: TypeIndex = if (fs.isRange()) blk: {
            const start_type = try self.checkExpr(fs.range_start);
            const end_type = try self.checkExpr(fs.range_end);
            // Both start and end must be integers
            if (!types.isInteger(self.types.get(start_type)) or !types.isInteger(self.types.get(end_type))) {
                self.err.errorWithCode(fs.span.start, .e300, "range bounds must be integers");
                break :blk invalid_type;
            }
            break :blk start_type; // Loop variable has same type as range start
        } else blk: {
            const iter_type = try self.checkExpr(fs.iterable);
            const iter = self.types.get(iter_type);
            break :blk switch (iter) {
                .array => |a| a.elem,
                .slice => |s| s.elem,
                else => {
                    self.err.errorWithCode(fs.span.start, .e300, "cannot iterate over this type");
                    break :blk invalid_type;
                },
            };
        };
        var loop_scope = Scope.init(self.allocator, self.scope);
        defer loop_scope.deinit();
        try loop_scope.define(Symbol.init(fs.binding, .variable, elem_type, null_node, false));
        // Define index binding if present (for i, x in arr)
        if (fs.index_binding) |idx_binding| {
            try loop_scope.define(Symbol.init(idx_binding, .variable, TypeRegistry.I64, null_node, false));
        }
        const old_scope = self.scope;
        const old_in_loop = self.in_loop;
        self.scope = &loop_scope;
        self.in_loop = true;
        try self.checkStmt(fs.body);
        self.scope = old_scope;
        self.in_loop = old_in_loop;
    }

    fn checkBlockStmt(self: *Checker, bs: ast.BlockStmt) CheckError!void {
        var block_scope = Scope.init(self.allocator, self.scope);
        defer block_scope.deinit();
        const old_scope = self.scope;
        self.scope = &block_scope;
        for (bs.stmts) |stmt_idx| try self.checkStmt(stmt_idx);
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
            self.err.errorWithCode(expr.ident.span.start, .e301, "undefined type");
            return invalid_type;
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
                    self.err.errorWithCode(te.span.start, .e301, "undefined type");
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
            self.err.errorWithCode(span.start, .e301, "undefined generic type");
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

            // Swap to the defining file's AST for cross-file generic resolution
            const saved_tree = self.tree;
            self.tree = impl_info.tree;
            defer self.tree = saved_tree;

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

    fn buildFuncType(self: *Checker, params: []const ast.Field, return_type_idx: NodeIndex) CheckError!TypeIndex {
        var func_params = std.ArrayListUnmanaged(types.FuncParam){};
        defer func_params.deinit(self.allocator);
        for (params) |param| try func_params.append(self.allocator, .{ .name = param.name, .type_idx = try self.resolveTypeExpr(param.type_expr) });
        const ret_type = if (return_type_idx != null_node) try self.resolveTypeExpr(return_type_idx) else TypeRegistry.VOID;
        return try self.types.add(.{ .func = .{ .params = try self.allocator.dupe(types.FuncParam, func_params.items), .return_type = ret_type } });
    }

    fn buildStructType(self: *Checker, name: []const u8, fields: []const ast.Field) CheckError!TypeIndex {
        var struct_fields = std.ArrayListUnmanaged(types.StructField){};
        defer struct_fields.deinit(self.allocator);
        var offset: u32 = 0;
        for (fields) |field| {
            const field_type = try self.resolveTypeExpr(field.type_expr);
            const field_align = self.types.alignmentOf(field_type);
            if (field_align > 0) offset = (offset + field_align - 1) & ~(field_align - 1);
            try struct_fields.append(self.allocator, .{ .name = field.name, .type_idx = field_type, .offset = offset });
            offset += self.types.sizeOf(field_type);
        }
        offset = (offset + 7) & ~@as(u32, 7);
        return try self.types.add(.{ .struct_type = .{ .name = name, .fields = try self.allocator.dupe(types.StructField, struct_fields.items), .size = offset, .alignment = 8 } });
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
        return false;
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
