//! Type checker for Cot.
//!
//! Walks the compact AST produced by the parser and verifies that every
//! expression, statement, and declaration is semantically valid. Resolves
//! types, checks assignments, infers expression types, instantiates generics,
//! and enforces actor isolation / Sendable rules.
//!
//! Split into focused submodules:
//!   checker/builtins.zig      — builtin call dispatch (@sizeOf, @intCast, etc.)
//!   checker/comptime_eval.zig — compile-time expression evaluation
//!   checker/diagnostics.zig   — "did you mean?" error suggestions
//!   checker/generics.zig      — generic type/function instantiation

const std = @import("std");
const ast = @import("ast.zig");
const types_mod = @import("types.zig");
const errors = @import("errors.zig");
const source = @import("source.zig");
const tok = @import("token.zig");
const comptime_mod = @import("comptime.zig");
const target_mod = @import("target.zig");
const debug = @import("debug.zig");

pub const ComptimeValue = comptime_mod.ComptimeValue;

const Ast = ast.Ast;
const Index = ast.Index;
const OptionalIndex = ast.OptionalIndex;
const TokenIndex = ast.TokenIndex;
const OptionalTokenIndex = ast.OptionalTokenIndex;
const SubRange = ast.SubRange;
const Tag = ast.Node.Tag;
const Token = tok.Token;
const Type = types_mod.Type;
const TypeIndex = types_mod.TypeIndex;
const TypeRegistry = types_mod.TypeRegistry;
const BasicKind = types_mod.BasicKind;
const ErrorReporter = errors.ErrorReporter;
const Pos = source.Pos;
const Span = source.Span;

pub const CheckError = error{OutOfMemory};

pub const SymbolKind = enum { variable, constant, function, type_name, parameter };

/// A named entity in a scope — variable, constant, function, type, or parameter.
pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    type_idx: TypeIndex,
    node: Index,
    mutable: bool,
    is_extern: bool = false,
    is_export: bool = false,
    const_value: ?i64 = null,
    float_const_value: ?f64 = null,
    comptime_val: ?ComptimeValue = null,
    used: bool = false,
    source_tree: ?*const Ast = null,

    pub fn init(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: Index, mutable: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable };
    }

    pub fn initExtern(name: []const u8, kind: SymbolKind, type_idx: TypeIndex, node: Index, mutable: bool, is_extern: bool) Symbol {
        return .{ .name = name, .kind = kind, .type_idx = type_idx, .node = node, .mutable = mutable, .is_extern = is_extern };
    }

    pub fn initConst(name: []const u8, type_idx: TypeIndex, node: Index, value: i64) Symbol {
        return .{ .name = name, .kind = .constant, .type_idx = type_idx, .node = node, .mutable = false, .const_value = value };
    }

    pub fn initFloatConst(name: []const u8, type_idx: TypeIndex, node: Index, value: f64) Symbol {
        return .{ .name = name, .kind = .constant, .type_idx = type_idx, .node = node, .mutable = false, .float_const_value = value };
    }
};

/// Lexical scope with symbol table and parent chain.
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

    pub fn markUsed(self: *Scope, name: []const u8) void {
        if (self.symbols.getPtr(name)) |ptr| {
            ptr.used = true;
        } else if (self.parent) |p| {
            p.markUsed(name);
        }
    }

    pub fn isDefined(self: *const Scope, name: []const u8) bool { return self.symbols.contains(name); }
};

/// Generic struct or function definition metadata.
pub const GenericInfo = struct {
    type_params: []const []const u8,
    type_param_bounds: []const ?[]const u8 = &.{},
    node_idx: Index,
    tree: *const Ast,
    scope: ?*Scope = null,
};

/// Resolved generic function instantiation for the lowerer.
pub const GenericInstInfo = struct {
    concrete_name: []const u8,
    generic_node: Index,
    type_args: []const TypeIndex,
    type_param_names: []const []const u8 = &.{},
    tree: *const Ast,
    scope: ?*Scope = null,
    expr_types: ?std.AutoHashMap(Index, TypeIndex) = null,
    has_indirect_result: bool = false,
};

/// Trait definition metadata.
pub const TraitDef = struct {
    name: []const u8,
    type_params: []const []const u8 = &.{},
    method_names: []const []const u8,
    assoc_type_names: []const []const u8 = &.{},
};

/// Shared generic context — lives across all checkers in multi-file mode.
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

/// Generic impl block definition.
pub const GenericImplInfo = struct {
    type_params: []const []const u8,
    methods: []const Index,
    tree: *const Ast,
    scope: ?*Scope = null,
};

/// Parse type arguments from a generic Map name like "Map(5;5)".
pub fn parseMapTypeArgs(name: []const u8) ?[2]TypeIndex {
    const open = std.mem.indexOfScalar(u8, name, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, name, ')') orelse return null;
    if (close <= open + 1) return null;
    const args_str = name[open + 1 .. close];
    const semi = std.mem.indexOfScalar(u8, args_str, ';') orelse return null;
    const k_str = args_str[0..semi];
    const v_str = args_str[semi + 1 ..];
    const k = std.fmt.parseInt(u32, k_str, 10) catch return null;
    const v = std.fmt.parseInt(u32, v_str, 10) catch return null;
    return .{ TypeIndex.fromInt(k), TypeIndex.fromInt(v) };
}

/// The main type checker. One instance per source file being checked.
pub const Checker = struct {
    types: *TypeRegistry,
    scope: *Scope,
    global_scope: *Scope,
    err: *ErrorReporter,
    tree: *const Ast,
    allocator: std.mem.Allocator,
    expr_types: std.AutoHashMap(Index, TypeIndex),
    current_return_type: TypeIndex = TypeRegistry.VOID,
    expected_type: TypeIndex = .invalid,
    in_loop: bool = false,
    in_labeled_block: u32 = 0,
    labeled_block_result_type: TypeIndex = .invalid,
    generics: *SharedGenericContext,
    generic_instantiations: std.AutoHashMap(Index, GenericInstInfo) = undefined,
    type_substitution: ?std.StringHashMap(TypeIndex) = null,
    target: target_mod.Target = target_mod.Target.native(),
    safe_mode: bool = false,
    lint_mode: bool = false,
    current_switch_enum_type: TypeIndex = .invalid,
    actor_types: std.StringHashMap(void) = std.StringHashMap(void).init(std.heap.page_allocator),
    unchecked_sendable_types: std.StringHashMap(void) = std.StringHashMap(void).init(std.heap.page_allocator),
    current_actor_type: ?[]const u8 = null,
    cross_actor_calls: std.AutoHashMap(u32, void) = std.AutoHashMap(u32, void).init(std.heap.page_allocator),
    sent_variables: std.StringHashMap(void) = std.StringHashMap(void).init(std.heap.page_allocator),
    global_actor_fns: std.StringHashMap([]const u8) = std.StringHashMap([]const u8).init(std.heap.page_allocator),
    in_await: bool = false,
    current_global_actor: ?[]const u8 = null,
    comptime_vars: ?std.StringHashMap(ComptimeValue) = null,

    pub fn init(
        allocator: std.mem.Allocator,
        tree: *const Ast,
        type_reg: *TypeRegistry,
        reporter: *ErrorReporter,
        file_scope: *Scope,
        global_scope: *Scope,
        generic_ctx: *SharedGenericContext,
        tgt: target_mod.Target,
    ) Checker {
        return .{
            .types = type_reg,
            .scope = file_scope,
            .global_scope = global_scope,
            .err = reporter,
            .tree = tree,
            .allocator = allocator,
            .expr_types = std.AutoHashMap(Index, TypeIndex).init(allocator),
            .generics = generic_ctx,
            .generic_instantiations = std.AutoHashMap(Index, GenericInstInfo).init(allocator),
            .target = tgt,
        };
    }

    pub fn deinit(self: *Checker) void {
        self.expr_types.deinit();
        self.generic_instantiations.deinit();
    }

    /// Define a symbol in scope with source_tree tracking.
    fn defineInFileScope(self: *Checker, sym: Symbol) !void {
        var s = sym;
        s.source_tree = self.tree;
        try self.scope.define(s);
    }

    /// Resolve a type name to its TypeIndex.
    pub fn resolveTypeByName(self: *Checker, name: []const u8) ?TypeIndex {
        if (self.type_substitution) |sub| {
            if (sub.get(name)) |idx| return idx;
        }
        return self.types.lookupByName(name);
    }

    /// Check if a name is a type parameter in the current generic context.
    pub fn isTypeParamName(self: *const Checker, name: []const u8) bool {
        if (self.type_substitution) |sub| return sub.contains(name);
        return false;
    }

    // ---------------------------------------------------------------
    // Declaration collection & checking (ported from old checker.zig)
    // ---------------------------------------------------------------

    /// Helper: get type-param name strings from a SubRange of token indices.
    fn typeParamNames(self: *const Checker, range: SubRange) []const u8 {
        _ = self;
        _ = range;
        // Placeholder — returns count; real usage iterates extraSlice.
        unreachable;
    }

    /// Helper: get the number of type params from a SubRange.
    fn typeParamCount(range: SubRange) u32 {
        return range.len();
    }

    /// Helper: allocate a []const []const u8 from a SubRange of token indices.
    fn typeParamNameSlice(self: *Checker, range: SubRange) CheckError![]const []const u8 {
        const raw = self.tree.extraSlice(range);
        const names = try self.allocator.alloc([]const u8, raw.len);
        for (raw, 0..) |tok_idx, i| {
            names[i] = self.tree.tokenSlice(tok_idx);
        }
        return names;
    }

    /// Helper: allocate a []const ?[]const u8 from a SubRange of bound token pairs.
    /// The param_bounds SubRange stores pairs of (type_param_token, bound_token) where
    /// bound_token == maxInt(u32) means no bound.
    fn typeParamBoundSlice(self: *Checker, range: SubRange) CheckError![]const ?[]const u8 {
        const raw = self.tree.extraSlice(range);
        const bounds = try self.allocator.alloc(?[]const u8, raw.len);
        for (raw, 0..) |tok_idx, i| {
            const oti: OptionalTokenIndex = @enumFromInt(tok_idx);
            bounds[i] = if (oti.unwrap()) |t| self.tree.tokenSlice(t) else null;
        }
        return bounds;
    }

    /// Helper: get param name from a Field extra_data entry.
    fn fieldParamName(self: *const Checker, extra_idx: ast.ExtraIndex) []const u8 {
        const field = self.tree.extraData(extra_idx, ast.Field);
        return self.tree.tokenSlice(field.name_token);
    }

    /// Helper: get param type_expr node from a Field extra_data entry.
    fn fieldParamTypeExpr(self: *const Checker, extra_idx: ast.ExtraIndex) Index {
        const field = self.tree.extraData(extra_idx, ast.Field);
        return field.type_expr;
    }

    /// Helper: get the ExtraIndex entries from a params SubRange.
    fn paramExtraIndices(self: *const Checker, range: SubRange) []const u32 {
        return self.tree.extraSlice(range);
    }

    /// Helper: get error set variant names from a SubRange of token indices.
    fn errorVariantNames(self: *Checker, range: SubRange) CheckError![]const []const u8 {
        const raw = self.tree.extraSlice(range);
        const names = try self.allocator.alloc([]const u8, raw.len);
        for (raw, 0..) |tok_idx, i| {
            names[i] = self.tree.tokenSlice(tok_idx);
        }
        return names;
    }

    /// Run lint checks on the file scope (top-level unused functions/vars).
    /// Function-local lint checks are emitted inline during checkFnDeclWithName.
    /// Go resolver.go pattern: lint checks run on file_scope (self.scope at top level).
    pub fn runLintChecks(self: *Checker) void {
        self.checkScopeUnused(self.scope);
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
            const pos = self.tree.nodeSpan(sym.node).start;
            if (sym.kind == .parameter) {
                self.err.warningWithCode(pos, .w002, sym.name);
            } else {
                self.err.warningWithCode(pos, .w001, sym.name);
            }
        }
    }

    /// Check a statement list for unreachable code after return/break/continue.
    /// Zig pattern: compiler warns on statements after `return` in a block.
    fn checkStmtsWithReachability(self: *Checker, stmts: []const Index) void {
        // Pre-pass: collect type declarations from statement list
        // (struct/enum/union decls that appear as Decl nodes in block context)
        for (stmts) |stmt_idx| {
            const tag = self.tree.nodeTag(stmt_idx);
            switch (tag) {
                .struct_decl, .enum_decl, .union_decl, .type_alias, .type_alias_distinct,
                .error_set_decl, .trait_decl, .fn_decl, .fn_decl_extern,
                => self.collectDecl(stmt_idx) catch {},
                else => {},
            }
        }
        // Post-collect: check method bodies for block-scoped structs
        // (collectDecl registers types+methods, checkDecl checks fn bodies)
        for (stmts) |stmt_idx| {
            const tag = self.tree.nodeTag(stmt_idx);
            if (tag == .struct_decl or tag == .fn_decl or tag == .fn_decl_extern)
                self.checkDecl(stmt_idx) catch {};
        }
        var seen_terminal = false;
        var warned = false;
        for (stmts) |stmt_idx| {
            if (seen_terminal and self.lint_mode and !warned) {
                // Warn once on the first unreachable statement (Zig pattern)
                const pos = self.tree.nodeSpan(stmt_idx).start;
                self.err.warningWithCode(pos, .w004, "unreachable code after return");
                warned = true;
            }
            if (!seen_terminal) {
                self.checkStmt(stmt_idx) catch {};
            }
            // Check if this statement is terminal (return, break, continue)
            if (!seen_terminal) {
                const tag = self.tree.nodeTag(stmt_idx);
                switch (tag) {
                    .return_expr, .return_void, .break_plain, .break_expr,
                    .continue_plain, .continue_labeled,
                    => {
                        seen_terminal = true;
                    },
                    else => {},
                }
            }
        }
    }

    /// Check if a node is an empty block (for lint W005).
    fn isEmptyBlock(self: *Checker, idx: Index) bool {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .block_stmt => {
                const data = self.tree.nodeData(idx);
                return data.extra_range.len() == 0;
            },
            .block => {
                const data = self.tree.nodeData(idx);
                return data.extra_range.len() == 0;
            },
            .block_one => return false, // has at least one stmt
            .block_two => return false, // has two stmts
            else => return false,
        }
    }

    pub fn checkFile(self: *Checker) CheckError!void {
        self.safe_mode = self.tree.safe_mode;
        // Get root node (always index 0) to access top-level declarations
        const root_data = self.tree.nodeData(@enumFromInt(0));
        const decls = self.tree.extraNodes(root_data.extra_range);
        debug.log(.check, "=== Type checking file ({d} declarations, safe={}) ===", .{
            decls.len, self.safe_mode,
        });

        // Pass 1: Collect type declarations (structs, enums, unions, type aliases)
        for (decls) |idx| try self.collectTypeDecl(idx);
        debug.log(.check, "  pass 1: type declarations collected", .{});

        // Pass 2: Collect non-type declarations (functions, variables, impl blocks)
        for (decls) |idx| try self.collectNonTypeDecl(idx);
        debug.log(.check, "  pass 2: non-type declarations collected", .{});

        // Pass 3: Check all declarations (type check function bodies, expressions)
        for (decls) |idx| try self.checkDecl(idx);
        debug.log(.check, "=== Type checking complete ({d} types registered) ===", .{
            self.types.types.items.len,
        });
    }

    fn collectTypeDecl(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .struct_decl, .enum_decl, .union_decl, .type_alias, .type_alias_distinct,
            .error_set_decl, .trait_decl,
            => try self.collectDecl(idx),
            else => {},
        }
    }

    fn collectNonTypeDecl(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .fn_decl, .fn_decl_extern, .var_decl, .impl_block, .impl_trait => try self.collectDecl(idx),
            else => {},
        }
    }

    fn collectDecl(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .fn_decl, .fn_decl_extern => {
                const f = self.tree.fnDeclData(idx);
                if (self.scope.isDefined(f.name)) {
                    if (f.is_extern) if (self.scope.lookup(f.name)) |e| if (e.is_extern) return;
                    self.reportRedefined(self.tree.nodeSpan(idx).start, f.name);
                    return;
                }
                // Export functions cannot be generic (generics aren't C ABI compatible)
                if (f.flags.is_export and typeParamCount(f.type_params) > 0) {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e302, "export functions cannot be generic");
                    return;
                }
                // Generic functions: store definition, don't build concrete type yet
                if (typeParamCount(f.type_params) > 0) {
                    const tp_names = try self.typeParamNameSlice(f.type_params);
                    const tp_bounds = try self.typeParamBoundSlice(f.param_bounds);
                    try self.generics.generic_functions.put(f.name, .{
                        .type_params = tp_names,
                        .type_param_bounds = tp_bounds,
                        .node_idx = idx,
                        .tree = self.tree,
                        .scope = self.scope,
                    });
                    // Register as a type_name so checkIdentifier knows it's generic
                    try self.defineInFileScope(Symbol.init(f.name, .type_name, .invalid, idx, false));
                    return;
                }
                var func_type = try self.buildFuncType(f.params, f.return_type);
                // async fn: wrap return type in Task(T), check Sendable
                if (f.flags.is_async) {
                    const inner_ret = self.types.get(func_type).func.return_type;
                    // Sendable check: return type must be safe to transfer across concurrency boundary
                    if (inner_ret != TypeRegistry.VOID and !self.isSendable(inner_ret)) {
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300,
                            "async function return type is not Sendable");
                    }
                    // Sendable check: parameters cross from caller to async context
                    for (self.types.get(func_type).func.params) |param| {
                        if (!self.isSendable(param.type_idx)) {
                            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300,
                                "async function parameter is not Sendable");
                        }
                    }
                    const task_ret = try self.types.makeTask(inner_ret);
                    func_type = try self.types.makeFunc(self.types.get(func_type).func.params, task_ret);
                }
                var sym = Symbol.initExtern(f.name, .function, func_type, idx, false, f.is_extern);
                sym.is_export = f.flags.is_export;
                try self.defineInFileScope(sym);
                // Check if first param is "self" to register as method
                const param_raw = self.tree.extraSlice(f.params);
                if (param_raw.len > 0) {
                    const first_field = self.tree.extraData(@enumFromInt(param_raw[0]), ast.Field);
                    if (std.mem.eql(u8, self.tree.tokenSlice(first_field.name_token), "self"))
                        try self.registerMethod(f.name, first_field.type_expr, func_type);
                }
                // Track @globalActor functions (SE-0316)
                if (f.global_actor.unwrap()) |actor_tok| {
                    self.global_actor_fns.put(f.name, self.tree.tokenSlice(actor_tok)) catch {};
                }
            },
            .var_decl => {
                const v = self.tree.varDeclData(idx);
                if (self.scope.isDefined(v.name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, v.name);
                    return;
                }
                try self.defineInFileScope(Symbol.init(
                    v.name,
                    if (v.is_const) .constant else .variable,
                    .invalid,
                    idx,
                    !v.is_const,
                ));
            },
            .struct_decl => {
                const s = self.tree.structDeclData(idx);
                if (self.scope.isDefined(s.name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, s.name);
                    return;
                }
                // Track actor types for isolation checking
                if (s.is_actor) self.actor_types.put(s.name, {}) catch {};
                // Generic structs: store definition, don't build concrete type yet
                if (typeParamCount(s.type_params) > 0) {
                    const tp_names = try self.typeParamNameSlice(s.type_params);
                    try self.generics.generic_structs.put(s.name, .{
                        .type_params = tp_names,
                        .node_idx = idx,
                        .tree = self.tree,
                        .scope = self.scope,
                    });
                    try self.defineInFileScope(Symbol.init(s.name, .type_name, .invalid, idx, false));
                    // Register nested fn_decls as generic impl block (inferred impl on generic struct)
                    if (s.nested_decls.len() > 0) {
                        var method_indices = std.ArrayListUnmanaged(Index){};
                        defer method_indices.deinit(self.allocator);
                        const nested_nodes = self.tree.extraNodes(s.nested_decls);
                        for (nested_nodes) |nested_idx| {
                            const nested_tag = self.tree.nodeTag(nested_idx);
                            if (nested_tag == .fn_decl or nested_tag == .fn_decl_extern)
                                try method_indices.append(self.allocator, nested_idx);
                        }
                        if (method_indices.items.len > 0) {
                            const gop = try self.generics.generic_impl_blocks.getOrPut(s.name);
                            if (!gop.found_existing) gop.value_ptr.* = .{};
                            try gop.value_ptr.append(self.allocator, .{
                                .type_params = tp_names,
                                .methods = try self.allocator.dupe(Index, method_indices.items),
                                .tree = self.tree,
                                .scope = self.scope,
                            });
                        }
                    }
                    return;
                }
                // Register name BEFORE resolving fields to support self-referential types
                // (e.g., LinkedList with next: ?*LinkedList). Placeholder type index is updated after.
                const placeholder = try self.types.add(.{ .struct_type = .{
                    .name = s.name,
                    .fields = &.{},
                    .size = 0,
                    .alignment = 8,
                } });
                try self.types.registerNamed(s.name, placeholder);
                try self.defineInFileScope(Symbol.init(s.name, .type_name, placeholder, idx, false));
                // Register nested TYPE declarations BEFORE resolving fields, so parent struct
                // fields can reference nested types (e.g. ComptimeValue has field of type ComptimeValue_ComptimeArray).
                // Non-type nested decls (fn_decl, var_decl) are registered after field resolution.
                const nested_nodes = self.tree.extraNodes(s.nested_decls);
                for (nested_nodes) |nested_idx| {
                    const nested_tag = self.tree.nodeTag(nested_idx);
                    switch (nested_tag) {
                        .error_set_decl => {
                            const es = self.tree.errorSetData(nested_idx);
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, es.name });
                            const variant_names = try self.errorVariantNames(es.variants);
                            const es_type = try self.types.add(.{ .error_set = .{ .name = qualified, .variants = variant_names } });
                            try self.defineInFileScope(Symbol.init(qualified, .type_name, es_type, nested_idx, false));
                            try self.types.registerNamed(qualified, es_type);
                        },
                        .enum_decl => {
                            const e = self.tree.enumDeclData(nested_idx);
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, e.name });
                            const enum_type = try self.buildEnumType(nested_idx);
                            try self.defineInFileScope(Symbol.init(qualified, .type_name, enum_type, nested_idx, false));
                            try self.types.registerNamed(qualified, enum_type);
                        },
                        .struct_decl => {
                            const ns = self.tree.structDeclData(nested_idx);
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, ns.name });
                            const ns_type = try self.buildStructTypeWithLayout(qualified, ns.fields, ns.layout);
                            try self.defineInFileScope(Symbol.init(qualified, .type_name, ns_type, nested_idx, false));
                            try self.types.registerNamed(qualified, ns_type);
                        },
                        .type_alias, .type_alias_distinct => {
                            const data = self.tree.nodeData(nested_idx);
                            const alias_name = self.tree.tokenSlice(data.token_and_node[0]);
                            const target_node = data.token_and_node[1];
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, alias_name });
                            const target_type = self.resolveTypeExpr(target_node) catch .invalid;
                            try self.defineInFileScope(Symbol.init(qualified, .type_name, target_type, nested_idx, false));
                            try self.types.registerNamed(qualified, target_type);
                        },
                        .var_decl => {
                            const v = self.tree.varDeclData(nested_idx);
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, v.name });
                            try self.defineInFileScope(Symbol.init(
                                qualified,
                                if (v.is_const) .constant else .variable,
                                .invalid,
                                nested_idx,
                                !v.is_const,
                            ));
                        },
                        .fn_decl, .fn_decl_extern => {
                            const nf = self.tree.fnDeclData(nested_idx);
                            const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, nf.name });
                            const func_type = try self.buildFuncType(nf.params, nf.return_type);
                            // Without @safe, a method with no self param is effectively static
                            // even if the parser didn't mark it. In @safe mode or actor mode, self gets injected.
                            // Swift: actor methods always have implicit isolated self.
                            const safe_or_actor = self.safe_mode or s.is_actor;
                            const param_raw = self.tree.extraSlice(nf.params);
                            const has_self_param = if (param_raw.len > 0) blk: {
                                const first_field = self.tree.extraData(@enumFromInt(param_raw[0]), ast.Field);
                                break :blk std.mem.eql(u8, self.tree.tokenSlice(first_field.name_token), "self");
                            } else false;
                            const effective_static = nf.flags.is_static or (!safe_or_actor and (param_raw.len == 0 or !has_self_param));
                            var is_ptr = !effective_static;
                            if (!effective_static) {
                                const func_info = self.types.get(func_type);
                                if (func_info == .func and func_info.func.params.len > 0) {
                                    const first_param_type = self.types.get(func_info.func.params[0].type_idx);
                                    if (first_param_type != .pointer) is_ptr = false;
                                }
                            }
                            try self.defineInFileScope(Symbol.initExtern(synth_name, .function, func_type, nested_idx, false, false));
                            try self.types.registerMethod(s.name, types_mod.MethodInfo{
                                .name = nf.name,
                                .func_name = synth_name,
                                .func_type = func_type,
                                .receiver_is_ptr = is_ptr,
                                .is_static = effective_static,
                                .is_nonisolated = nf.flags.is_nonisolated,
                                .source_tree = self.tree,
                            });
                        },
                        else => {},
                    }
                }
                // Build struct type AFTER nested types are registered (so fields can reference them)
                const struct_type = try self.buildStructTypeWithLayout(s.name, s.fields, s.layout);
                // Update the placeholder with the real type
                const st = self.types.get(struct_type).struct_type;
                self.types.types.items[@intCast(placeholder.toInt())] = .{ .struct_type = .{
                    .name = s.name,
                    .fields = st.fields,
                    .size = st.size,
                    .alignment = st.alignment,
                    .layout = s.layout,
                    .backing_int = st.backing_int,
                } };
            },
            .enum_decl => {
                const e = self.tree.enumDeclData(idx);
                if (self.scope.isDefined(e.name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, e.name);
                    return;
                }
                const enum_type = try self.buildEnumType(idx);
                try self.defineInFileScope(Symbol.init(e.name, .type_name, enum_type, idx, false));
                try self.types.registerNamed(e.name, enum_type);
                // Register nested declarations (inferred impl: fn/static fn/const inside enum body)
                const nested_nodes = self.tree.extraNodes(e.nested_decls);
                for (nested_nodes) |nested_idx| {
                    const nested_tag = self.tree.nodeTag(nested_idx);
                    switch (nested_tag) {
                        .fn_decl, .fn_decl_extern => {
                            const nf = self.tree.fnDeclData(nested_idx);
                            const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ e.name, nf.name });
                            const func_type = try self.buildFuncType(nf.params, nf.return_type);
                            const param_raw_e = self.tree.extraSlice(nf.params);
                            const has_self_param_e = if (param_raw_e.len > 0) blk: {
                                const first_field = self.tree.extraData(@enumFromInt(param_raw_e[0]), ast.Field);
                                break :blk std.mem.eql(u8, self.tree.tokenSlice(first_field.name_token), "self");
                            } else false;
                            const effective_static_e = nf.flags.is_static or (!self.safe_mode and (param_raw_e.len == 0 or !has_self_param_e));
                            var is_ptr = !effective_static_e;
                            if (!effective_static_e) {
                                const func_info = self.types.get(func_type);
                                if (func_info == .func and func_info.func.params.len > 0) {
                                    const first_param_type = self.types.get(func_info.func.params[0].type_idx);
                                    if (first_param_type != .pointer) is_ptr = false;
                                }
                            }
                            try self.defineInFileScope(Symbol.initExtern(synth_name, .function, func_type, nested_idx, false, false));
                            try self.types.registerMethod(e.name, types_mod.MethodInfo{
                                .name = nf.name,
                                .func_name = synth_name,
                                .func_type = func_type,
                                .receiver_is_ptr = is_ptr,
                                .is_static = effective_static_e,
                                .source_tree = self.tree,
                            });
                        },
                        .var_decl => {
                            const v = self.tree.varDeclData(nested_idx);
                            const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ e.name, v.name });
                            try self.defineInFileScope(Symbol.init(
                                qualified,
                                if (v.is_const) .constant else .variable,
                                .invalid,
                                nested_idx,
                                !v.is_const,
                            ));
                        },
                        else => {},
                    }
                }
            },
            .union_decl => {
                const u = self.tree.unionDeclData(idx);
                if (self.scope.isDefined(u.name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, u.name);
                    return;
                }
                const union_type = try self.buildUnionType(idx);
                try self.defineInFileScope(Symbol.init(u.name, .type_name, union_type, idx, false));
                try self.types.registerNamed(u.name, union_type);
            },
            .error_set_decl => {
                const es = self.tree.errorSetData(idx);
                if (self.scope.isDefined(es.name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, es.name);
                    return;
                }
                const variant_names = try self.errorVariantNames(es.variants);
                const es_type = try self.types.add(.{ .error_set = .{ .name = es.name, .variants = variant_names } });
                try self.defineInFileScope(Symbol.init(es.name, .type_name, es_type, idx, false));
                try self.types.registerNamed(es.name, es_type);
            },
            // Go reference: types2/alias.go - Alias stores RHS and resolves through it
            .type_alias, .type_alias_distinct => {
                const data = self.tree.nodeData(idx);
                const alias_name = self.tree.tokenSlice(data.token_and_node[0]);
                const target_node = data.token_and_node[1];
                if (self.scope.isDefined(alias_name)) {
                    self.reportRedefined(self.tree.nodeSpan(idx).start, alias_name);
                    return;
                }
                const target_type = self.resolveTypeExpr(target_node) catch .invalid;
                if (tag == .type_alias_distinct) {
                    // Rust reference: newtype pattern — create a nominally distinct type
                    // wrapping the underlying type. Prevents cross-type assignment at compile time.
                    const distinct_idx = try self.types.add(.{ .distinct = .{ .name = alias_name, .underlying = target_type } });
                    try self.defineInFileScope(Symbol.init(alias_name, .type_name, distinct_idx, idx, false));
                    try self.types.registerNamed(alias_name, distinct_idx);
                } else {
                    try self.defineInFileScope(Symbol.init(alias_name, .type_name, target_type, idx, false));
                    try self.types.registerNamed(alias_name, target_type);
                }
            },
            .impl_block => {
                const impl_b = self.tree.implBlockData(idx);
                // Generic impl blocks: store definition, instantiate when struct is instantiated
                if (typeParamCount(impl_b.type_params) > 0) {
                    const tp_names = try self.typeParamNameSlice(impl_b.type_params);
                    const method_nodes = self.tree.extraNodes(impl_b.methods);
                    const gop = try self.generics.generic_impl_blocks.getOrPut(impl_b.type_name);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    try gop.value_ptr.append(self.allocator, .{
                        .type_params = tp_names,
                        .methods = try self.allocator.dupe(Index, method_nodes),
                        .tree = self.tree,
                        .scope = self.scope,
                    });
                    return;
                }
                // Register associated constants with qualified names: TypeName_ConstName
                const const_nodes = self.tree.extraNodes(impl_b.consts);
                for (const_nodes) |const_idx| {
                    const const_tag = self.tree.nodeTag(const_idx);
                    if (const_tag == .var_decl) {
                        const v = self.tree.varDeclData(const_idx);
                        var const_type: TypeIndex = .invalid;
                        if (v.type_expr.unwrap()) |te| const_type = self.resolveTypeExpr(te) catch .invalid;
                        const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_b.type_name, v.name });
                        try self.defineInFileScope(Symbol.init(qualified, .constant, const_type, const_idx, false));
                    }
                }
                const method_nodes = self.tree.extraNodes(impl_b.methods);
                for (method_nodes) |method_idx| {
                    const method_tag = self.tree.nodeTag(method_idx);
                    if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                        const mf = self.tree.fnDeclData(method_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_b.type_name, mf.name });
                        const func_type = try self.buildFuncType(mf.params, mf.return_type);
                        const param_raw_i = self.tree.extraSlice(mf.params);
                        const has_self_param_i = if (param_raw_i.len > 0) blk: {
                            const first_field = self.tree.extraData(@enumFromInt(param_raw_i[0]), ast.Field);
                            break :blk std.mem.eql(u8, self.tree.tokenSlice(first_field.name_token), "self");
                        } else false;
                        const effective_static_i = mf.flags.is_static or (!self.safe_mode and (param_raw_i.len == 0 or !has_self_param_i));
                        var is_ptr = !effective_static_i;
                        if (!effective_static_i) {
                            const func_info = self.types.get(func_type);
                            if (func_info == .func and func_info.func.params.len > 0) {
                                const first_param_type = self.types.get(func_info.func.params[0].type_idx);
                                if (first_param_type != .pointer) is_ptr = false;
                            }
                        }
                        try self.defineInFileScope(Symbol.initExtern(synth_name, .function, func_type, method_idx, false, false));
                        try self.types.registerMethod(impl_b.type_name, types_mod.MethodInfo{
                            .name = mf.name,
                            .func_name = synth_name,
                            .func_type = func_type,
                            .receiver_is_ptr = is_ptr,
                            .is_static = effective_static_i,
                            .source_tree = self.tree,
                        });
                    }
                }
            },
            .trait_decl => {
                const td = self.tree.traitDeclData(idx);
                // Collect trait method names and associated type names.
                // Swift reference: protocol declaration with associated types (SE-0142).
                var method_names = std.ArrayListUnmanaged([]const u8){};
                defer method_names.deinit(self.allocator);
                const trait_method_nodes = self.tree.extraNodes(td.methods);
                for (trait_method_nodes) |method_idx| {
                    const method_tag = self.tree.nodeTag(method_idx);
                    if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                        const mf = self.tree.fnDeclData(method_idx);
                        try method_names.append(self.allocator, mf.name);
                    }
                }
                var assoc_type_names = std.ArrayListUnmanaged([]const u8){};
                defer assoc_type_names.deinit(self.allocator);
                // Associated types are stored as pairs (name_token, bound_token) in extra_data
                const assoc_raw = self.tree.extraSlice(td.assoc_types);
                var ai: usize = 0;
                while (ai < assoc_raw.len) : (ai += 1) {
                    const at = self.tree.extraData(@enumFromInt(assoc_raw[ai]), ast.AssocType);
                    try assoc_type_names.append(self.allocator, self.tree.tokenSlice(at.name_token));
                }
                const tp_names = try self.typeParamNameSlice(td.type_params);
                try self.generics.trait_defs.put(td.name, .{
                    .name = td.name,
                    .type_params = tp_names,
                    .method_names = try self.allocator.dupe([]const u8, method_names.items),
                    .assoc_type_names = try self.allocator.dupe([]const u8, assoc_type_names.items),
                });
            },
            .impl_trait => {
                const it = self.tree.implTraitData(idx);
                // Validate trait exists
                const trait_def = self.generics.trait_defs.get(it.trait_name) orelse {
                    self.errWithSuggestion(self.tree.nodeSpan(idx).start, "undefined trait", self.findSimilarTrait(it.trait_name));
                    return;
                };
                // Validate all required methods are provided (name check)
                for (trait_def.method_names) |required_name| {
                    var found = false;
                    const it_method_nodes = self.tree.extraNodes(it.methods);
                    for (it_method_nodes) |method_idx| {
                        const method_tag = self.tree.nodeTag(method_idx);
                        if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                            const mf = self.tree.fnDeclData(method_idx);
                            if (std.mem.eql(u8, mf.name, required_name)) {
                                found = true;
                                break;
                            }
                        }
                    }
                    if (!found) {
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "missing required trait method");
                        return;
                    }
                }
                // Register methods (same pattern as impl_block)
                const it_method_nodes = self.tree.extraNodes(it.methods);
                for (it_method_nodes) |method_idx| {
                    const method_tag = self.tree.nodeTag(method_idx);
                    if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                        const mf = self.tree.fnDeclData(method_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ it.target_type, mf.name });
                        const func_type = try self.buildFuncType(mf.params, mf.return_type);
                        try self.defineInFileScope(Symbol.initExtern(synth_name, .function, func_type, method_idx, false, false));
                        try self.types.registerMethod(it.target_type, types_mod.MethodInfo{
                            .name = mf.name,
                            .func_name = synth_name,
                            .func_type = func_type,
                            .receiver_is_ptr = true,
                            .source_tree = self.tree,
                        });
                    }
                }
                const impl_key = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ it.trait_name, it.target_type });
                try self.generics.trait_impls.put(impl_key, it.trait_name);
            },
            else => {},
        }
    }

    fn registerMethod(self: *Checker, func_name: []const u8, self_type_expr: Index, func_type: TypeIndex) CheckError!void {
        const te_tag = self.tree.nodeTag(self_type_expr);
        var receiver_name: []const u8 = undefined;
        var is_ptr = false;
        switch (te_tag) {
            .type_named => {
                receiver_name = self.tree.tokenSlice(self.tree.nodeMainToken(self_type_expr));
            },
            .type_pointer => {
                const ptr_data = self.tree.nodeData(self_type_expr);
                const elem_node: Index = ptr_data.node;
                const elem_tag = self.tree.nodeTag(elem_node);
                if (elem_tag != .type_named) return;
                receiver_name = self.tree.tokenSlice(self.tree.nodeMainToken(elem_node));
                is_ptr = true;
            },
            else => return,
        }
        try self.types.registerMethod(receiver_name, types_mod.MethodInfo{
            .name = func_name,
            .func_name = func_name,
            .func_type = func_type,
            .receiver_is_ptr = is_ptr,
            .source_tree = self.tree,
        });
    }

    pub fn lookupMethod(self: *const Checker, type_name: []const u8, method_name: []const u8) ?types_mod.MethodInfo {
        return self.types.lookupMethod(type_name, method_name);
    }

    /// Report E302 with a note pointing to the previous definition (Zig pattern).
    fn reportRedefined(self: *Checker, span_start: Pos, name: []const u8) void {
        if (self.scope.lookup(name)) |sym| {
            const prev_pos = self.tree.nodeSpan(sym.node).start;
            self.err.errorWithCodeAndNote(span_start, .e302, "redefined identifier", prev_pos, "previously defined here");
            return;
        }
        self.err.errorWithCode(span_start, .e302, "redefined identifier");
    }

    fn checkDecl(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .fn_decl, .fn_decl_extern => {
                const f = self.tree.fnDeclData(idx);
                // Skip generic function definitions — they're checked at instantiation time
                if (typeParamCount(f.type_params) > 0) return;
                try self.checkFnDeclWithName(f, idx, f.name);
            },
            .var_decl => {
                const v = self.tree.varDeclData(idx);
                try self.checkVarDecl(v, idx);
            },
            .impl_block => {
                const impl_b = self.tree.implBlockData(idx);
                // Generic impl blocks: bodies checked at instantiation time
                if (typeParamCount(impl_b.type_params) > 0) return;
                const method_nodes = self.tree.extraNodes(impl_b.methods);
                for (method_nodes) |method_idx| {
                    const method_tag = self.tree.nodeTag(method_idx);
                    if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                        const mf = self.tree.fnDeclData(method_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_b.type_name, mf.name });
                        try self.checkFnDeclWithName(mf, method_idx, synth_name);
                    }
                }
            },
            .impl_trait => {
                const it = self.tree.implTraitData(idx);
                const it_method_nodes = self.tree.extraNodes(it.methods);
                for (it_method_nodes) |method_idx| {
                    const method_tag = self.tree.nodeTag(method_idx);
                    if (method_tag == .fn_decl or method_tag == .fn_decl_extern) {
                        const mf = self.tree.fnDeclData(method_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ it.target_type, mf.name });
                        try self.checkFnDeclWithName(mf, method_idx, synth_name);
                    }
                }
            },
            .struct_decl => {
                const s = self.tree.structDeclData(idx);
                if (typeParamCount(s.type_params) > 0) return;
                // Set actor context for isolation checking inside actor method bodies
                const saved_actor = self.current_actor_type;
                if (s.is_actor) self.current_actor_type = s.name;
                defer self.current_actor_type = saved_actor;
                const nested_nodes = self.tree.extraNodes(s.nested_decls);
                for (nested_nodes) |nested_idx| {
                    const nested_tag = self.tree.nodeTag(nested_idx);
                    if (nested_tag == .fn_decl or nested_tag == .fn_decl_extern) {
                        const nf = self.tree.fnDeclData(nested_idx);
                        if (typeParamCount(nf.type_params) > 0) continue;
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ s.name, nf.name });
                        try self.checkFnDeclWithName(nf, nested_idx, synth_name);
                    }
                }
            },
            .enum_decl => {
                const e = self.tree.enumDeclData(idx);
                const nested_nodes = self.tree.extraNodes(e.nested_decls);
                for (nested_nodes) |nested_idx| {
                    const nested_tag = self.tree.nodeTag(nested_idx);
                    if (nested_tag == .fn_decl or nested_tag == .fn_decl_extern) {
                        const nf = self.tree.fnDeclData(nested_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ e.name, nf.name });
                        try self.checkFnDeclWithName(nf, nested_idx, synth_name);
                    }
                }
            },
            .unchecked_sendable => {
                // `@unchecked(Sendable) TypeName` — data: type_name token
                const us_data = self.tree.nodeData(idx);
                const type_name_tok = us_data.token;
                try self.unchecked_sendable_types.put(self.tree.tokenSlice(type_name_tok), {});
            },
            .test_decl => try self.checkTestDecl(idx),
            .bench_decl => try self.checkBenchDecl(idx),
            else => {},
        }
    }

    fn checkTestDecl(self: *Checker, idx: Index) CheckError!void {
        // test_decl data: name token + body node
        const data = self.tree.nodeData(idx);
        const body_node: Index = data.token_and_node[1];
        var test_scope = Scope.init(self.allocator, self.scope);
        defer test_scope.deinit();
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &test_scope;
        self.current_return_type = TypeRegistry.VOID;
        try self.checkStmt(body_node);
        if (self.lint_mode) self.checkScopeUnused(&test_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    fn checkBenchDecl(self: *Checker, idx: Index) CheckError!void {
        // bench_decl data: name token + body node
        const data = self.tree.nodeData(idx);
        const body_node: Index = data.token_and_node[1];
        var bench_scope = Scope.init(self.allocator, self.scope);
        defer bench_scope.deinit();
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &bench_scope;
        self.current_return_type = TypeRegistry.VOID;
        try self.checkStmt(body_node);
        if (self.lint_mode) self.checkScopeUnused(&bench_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
    }

    pub fn checkFnDeclWithName(self: *Checker, f: ast.full.FnDeclFull, idx: Index, lookup_name: []const u8) CheckError!void {
        const param_count = f.params.len();
        debug.log(.check, "  check fn '{s}' ({d} params, async={}, export={}, extern={})", .{
            lookup_name, param_count, f.flags.is_async, f.flags.is_export, f.is_extern,
        });
        const sym = self.scope.lookup(lookup_name) orelse return;
        const return_type = if (self.types.get(sym.type_idx) == .func) self.types.get(sym.type_idx).func.return_type else TypeRegistry.VOID;
        // For async functions, the registered return type is Task(T) — body returns T
        const body_return_type = if (f.flags.is_async and self.types.get(return_type) == .task)
            self.types.get(return_type).task.result_type
        else
            return_type;
        var func_scope = Scope.init(self.allocator, self.scope);
        defer func_scope.deinit();
        // Iterate params: each entry in the SubRange is an ExtraIndex pointing to a Field
        const param_raw = self.tree.extraSlice(f.params);
        for (param_raw) |raw_ei| {
            const field = self.tree.extraData(@enumFromInt(raw_ei), ast.Field);
            const param_name = self.tree.tokenSlice(field.name_token);
            var param_type = try self.resolveTypeExpr(field.type_expr);
            // Don't auto-ref parameters whose type is a generic type parameter (T).
            // Generic code uses value semantics for T; wrapping T to *T when T=struct
            // would break body code (e.g. `ptr.* = value` expects T, not *T).
            const is_generic_param = if (self.type_substitution) |sub| blk: {
                const te_tag = self.tree.nodeTag(field.type_expr);
                if (te_tag == .type_named) {
                    break :blk sub.contains(self.tree.tokenSlice(self.tree.nodeMainToken(field.type_expr)));
                }
                if (te_tag == .ident) {
                    break :blk sub.contains(self.tree.tokenSlice(self.tree.nodeMainToken(field.type_expr)));
                }
                break :blk false;
            } else false;
            if (!is_generic_param) param_type = try self.safeWrapType(param_type);
            try func_scope.define(Symbol.init(param_name, .parameter, param_type, idx, false));
        }
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        const old_global_actor = self.current_global_actor;
        self.scope = &func_scope;
        self.current_return_type = body_return_type;
        // Swift SE-0316: set global actor context for isolation checking
        if (f.global_actor.unwrap()) |ga_tok| self.current_global_actor = self.tree.tokenSlice(ga_tok);
        // Swift SE-0430: clear sent_variables at function boundary.
        // Each function has its own sending scope — sent state doesn't leak across functions.
        self.sent_variables.clearRetainingCapacity();
        if (f.body.unwrap()) |body| try self.checkBlockExpr(body);
        // Lint: check for unused vars/params before scope exits
        if (self.lint_mode) self.checkScopeUnused(&func_scope);
        self.scope = old_scope;
        self.current_return_type = old_return;
        self.current_global_actor = old_global_actor;
    }

    fn checkVarDecl(self: *Checker, v: ast.full.VarDeclFull, idx: Index) CheckError!void {
        var var_type: TypeIndex = if (v.type_expr.unwrap()) |te| try self.resolveTypeExpr(te) else .invalid;
        if (v.value.unwrap()) |val_node| {
            if (!self.isUndefinedLit(val_node)) {
                const val_type = try self.checkExpr(val_node);
                if (var_type == .invalid) {
                    var_type = self.materializeType(val_type);
                } else if (!self.types.isAssignable(val_type, var_type)) {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "type mismatch");
                }
            }
        }
        if (self.scope.lookupLocal(v.name) != null) {
            if (v.is_const) {
                if (v.value.unwrap()) |val_node| {
                    if (self.evalConstExpr(val_node)) |cv| {
                        try self.scope.define(Symbol.initConst(v.name, var_type, idx, cv));
                        return;
                    }
                    if (self.isFloatType(var_type)) if (self.evalConstFloat(val_node)) |fv| {
                        try self.scope.define(Symbol.initFloatConst(v.name, var_type, idx, fv));
                        return;
                    };
                }
            }
            try self.scope.define(Symbol.init(
                v.name,
                if (v.is_const) .constant else .variable,
                var_type,
                idx,
                !v.is_const,
            ));
        }
        // Register error set consts as named types for use in type positions
        // Enables: const AllErrors = FileError || NetError
        if (v.is_const and var_type != .invalid and self.types.get(var_type) == .error_set) {
            try self.types.registerNamed(v.name, var_type);
        }
    }

    fn isUndefinedLit(self: *Checker, idx: Index) bool {
        return self.tree.nodeTag(idx) == .literal_undefined;
    }

    fn isZeroInitLit(self: *Checker, idx: Index) bool {
        return self.tree.nodeTag(idx) == .zero_init;
    }

    // ---------------------------------------------------------------
    // Comptime evaluation (ported from compiler/ lines 973-1706)
    // ---------------------------------------------------------------

    /// Rich comptime evaluator — returns structured values (arrays, strings, ints, bools).
    /// Zig Sema pattern: resolveInstValue evaluates ZIR instructions to comptime values.
    /// Delegates to evalConstExpr/evalConstString for leaf cases, adds support for
    /// comptime blocks with statements, arrays, struct field access, etc.
    pub fn evalComptimeValue(self: *Checker, idx: Index) ?ComptimeValue {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .literal_int => {
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                return if (std.fmt.parseInt(i64, text, 0) catch null) |v| ComptimeValue{ .int = v } else null;
            },
            .literal_float => {
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                return if (std.fmt.parseFloat(f64, text) catch null) |v| ComptimeValue{ .float = v } else null;
            },
            .literal_true => ComptimeValue{ .boolean = true },
            .literal_false => ComptimeValue{ .boolean = false },
            .literal_string => blk: {
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                const s = if (text.len >= 2 and text[0] == '"' and text[text.len - 1] == '"') text[1 .. text.len - 1] else text;
                break :blk ComptimeValue{ .string = s };
            },
            .literal_undefined => ComptimeValue.undefined_val,
            .unary_neg => {
                const operand: Index = self.tree.nodeData(idx).node;
                if (self.evalComptimeValue(operand)) |op| return switch (op) {
                    .int => |v| ComptimeValue{ .int = -v },
                    else => null,
                } else return null;
            },
            .unary_bit_not => {
                const operand: Index = self.tree.nodeData(idx).node;
                if (self.evalComptimeValue(operand)) |op| return switch (op) {
                    .int => |v| ComptimeValue{ .int = ~v },
                    else => null,
                } else return null;
            },
            .unary_not => {
                const operand: Index = self.tree.nodeData(idx).node;
                if (self.evalComptimeValue(operand)) |op| return switch (op) {
                    .int => |v| ComptimeValue{ .int = if (v == 0) @as(i64, 1) else @as(i64, 0) },
                    .boolean => |b| ComptimeValue{ .boolean = !b },
                    else => null,
                } else return null;
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_bit_and, .binary_bit_or, .binary_bit_xor, .binary_shl, .binary_shr,
            .binary_eq, .binary_neq, .binary_lt, .binary_lte, .binary_gt, .binary_gte,
            => blk: {
                const data = self.tree.nodeData(idx).node_and_node;
                const l = self.evalComptimeValue(data[0]) orelse break :blk null;
                const r = self.evalComptimeValue(data[1]) orelse break :blk null;
                // Int-int binary
                if (l.asInt()) |li| {
                    if (r.asInt()) |ri| {
                        const result: ?i64 = switch (tag) {
                            .binary_add => li + ri,
                            .binary_sub => li - ri,
                            .binary_mul => li * ri,
                            .binary_div => if (ri != 0) @divTrunc(li, ri) else null,
                            .binary_mod => if (ri != 0) @rem(li, ri) else null,
                            .binary_bit_and => li & ri,
                            .binary_bit_or => li | ri,
                            .binary_bit_xor => li ^ ri,
                            .binary_shl => li << @intCast(ri),
                            .binary_shr => li >> @intCast(ri),
                            .binary_eq => if (li == ri) @as(i64, 1) else @as(i64, 0),
                            .binary_neq => if (li != ri) @as(i64, 1) else @as(i64, 0),
                            .binary_lt => if (li < ri) @as(i64, 1) else @as(i64, 0),
                            .binary_lte => if (li <= ri) @as(i64, 1) else @as(i64, 0),
                            .binary_gt => if (li > ri) @as(i64, 1) else @as(i64, 0),
                            .binary_gte => if (li >= ri) @as(i64, 1) else @as(i64, 0),
                            else => null,
                        };
                        break :blk if (result) |v| ComptimeValue{ .int = v } else null;
                    }
                }
                // String-string equality
                if (l.asString()) |ls| {
                    if (r.asString()) |rs| {
                        if (tag == .binary_eq or tag == .binary_neq) {
                            const equal = std.mem.eql(u8, ls, rs);
                            const v: i64 = if (tag == .binary_eq)
                                (if (equal) @as(i64, 1) else @as(i64, 0))
                            else
                                (if (!equal) @as(i64, 1) else @as(i64, 0));
                            break :blk ComptimeValue{ .int = v };
                        }
                    }
                }
                break :blk null;
            },
            .paren => self.evalComptimeValue(self.tree.nodeData(idx).node),
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                // Check comptime mutable vars first (Phase 2: ComptimeAlloc lookup)
                if (self.comptime_vars) |*cv| {
                    if (cv.get(name)) |val| return val;
                }
                // Fall back to const symbol lookup
                if (self.scope.lookup(name)) |sym| {
                    if (sym.kind == .constant) {
                        if (sym.const_value) |v| return ComptimeValue{ .int = v };
                        if (sym.float_const_value) |v| return ComptimeValue{ .float = v };
                        // Check for comptime_value on symbol (Phase 5: inline for bindings)
                        if (sym.comptime_val) |cv| return cv;
                    }
                }
                return null;
            },
            .builtin_call => {
                const bc = self.tree.builtinCallData(idx);
                if (bc.kind == .size_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    return ComptimeValue{ .int = @as(i64, @intCast(self.types.sizeOf(type_idx))) };
                }
                if (bc.kind == .align_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    return ComptimeValue{ .int = @as(i64, @intCast(self.types.alignmentOf(type_idx))) };
                }
                if (bc.kind == .offset_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const name_str = self.evalConstString(arg0) orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .struct_type) {
                        var offset: i64 = 0;
                        for (info.struct_type.fields) |sf| {
                            if (std.mem.eql(u8, sf.name, name_str)) return ComptimeValue{ .int = offset };
                            offset += @as(i64, @intCast(self.types.sizeOf(sf.type_idx)));
                        }
                    }
                    return null;
                }
                if (bc.kind == .int_from_bool) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    if (self.evalComptimeValue(arg0)) |v| {
                        const iv = v.asInt() orelse return null;
                        return ComptimeValue{ .int = if (iv != 0) @as(i64, 1) else @as(i64, 0) };
                    }
                    return null;
                }
                if (bc.kind == .min) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const arg1 = bc.arg1.unwrap() orelse return null;
                    const a = (self.evalComptimeValue(arg0) orelse return null).asInt() orelse return null;
                    const b = (self.evalComptimeValue(arg1) orelse return null).asInt() orelse return null;
                    return ComptimeValue{ .int = if (a < b) a else b };
                }
                if (bc.kind == .max) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const arg1 = bc.arg1.unwrap() orelse return null;
                    const a = (self.evalComptimeValue(arg0) orelse return null).asInt() orelse return null;
                    const b = (self.evalComptimeValue(arg1) orelse return null).asInt() orelse return null;
                    return ComptimeValue{ .int = if (a > b) a else b };
                }
                if (bc.kind == .has_field) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const name_str = self.evalConstString(arg0) orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .struct_type) {
                        for (info.struct_type.fields) |sf| {
                            if (std.mem.eql(u8, sf.name, name_str)) return ComptimeValue{ .int = 1 };
                        }
                        return ComptimeValue{ .int = 0 };
                    }
                    if (info == .enum_type) {
                        for (info.enum_type.variants) |v| {
                            if (std.mem.eql(u8, v.name, name_str)) return ComptimeValue{ .int = 1 };
                        }
                        return ComptimeValue{ .int = 0 };
                    }
                    if (info == .union_type) {
                        for (info.union_type.variants) |v| {
                            if (std.mem.eql(u8, v.name, name_str)) return ComptimeValue{ .int = 1 };
                        }
                        return ComptimeValue{ .int = 0 };
                    }
                    return ComptimeValue{ .int = 0 };
                }
                if (bc.kind == .enum_len) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const info = self.types.get(type_idx);
                    if (info == .enum_type) return ComptimeValue{ .int = @intCast(info.enum_type.variants.len) };
                    return null;
                }
                // String-producing builtins
                if (bc.kind == .target_os) return ComptimeValue{ .string = self.target.os.name() };
                if (bc.kind == .target_arch) return ComptimeValue{ .string = self.target.arch.name() };
                if (bc.kind == .target) return ComptimeValue{ .string = self.target.name() };
                // @typeName(T) — Phase 3
                if (bc.kind == .type_name) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    return ComptimeValue{ .string = self.types.typeName(type_idx) };
                }
                // @enumName(T, index) — Phase 3
                if (bc.kind == .enum_name) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const index_val = (self.evalComptimeValue(arg0) orelse return null).asInt() orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .enum_type) {
                        for (info.enum_type.variants) |v| {
                            if (v.value == index_val) return ComptimeValue{ .string = v.name };
                        }
                    }
                    return null;
                }
                // @typeInfo(T) — Phase 4
                if (bc.kind == .type_info) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const info = self.types.get(type_idx);
                    if (info == .enum_type) {
                        var fields = std.ArrayListUnmanaged(ComptimeValue){};
                        for (info.enum_type.variants) |v| {
                            fields.append(self.allocator, ComptimeValue{ .enum_field = .{
                                .name = v.name,
                                .value = v.value,
                            } }) catch return null;
                        }
                        return ComptimeValue{ .type_info = .{
                            .kind = .enum_info,
                            .name = info.enum_type.name,
                            .fields = fields,
                        } };
                    }
                    return null;
                }
                // @intFromEnum — comptime when arg is comptime-known
                if (bc.kind == .int_from_enum) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    if (self.evalConstExpr(arg0)) |v| return ComptimeValue{ .int = v };
                    return null;
                }
                return null;
            },
            .if_simple => {
                const data = self.tree.nodeData(idx).node_and_node;
                const cond_val = (self.evalComptimeValue(data[0]) orelse return null).asInt() orelse return null;
                if (cond_val != 0) return self.evalComptimeValue(data[1]);
                return null;
            },
            .if_full => {
                const data = self.tree.nodeData(idx);
                const cond_node = data.node_and_extra[0];
                const extra = self.tree.extraData(data.node_and_extra[1], ast.IfData);
                const cond_val = (self.evalComptimeValue(cond_node) orelse return null).asInt() orelse return null;
                if (cond_val != 0) return self.evalComptimeValue(extra.then_node);
                if (extra.else_node.unwrap()) |else_node| return self.evalComptimeValue(else_node);
                return null;
            },
            .block_one => {
                // Single-stmt block: data = stmt node + result node
                const data = self.tree.nodeData(idx).node_and_node;
                const result_node = data[1];
                // No stmts to evaluate in simple block
                return self.evalComptimeValue(result_node);
            },
            .block_two => {
                // Two-stmt block: no result expr
                return null;
            },
            .block => {
                const data = self.tree.nodeData(idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                if (stmts.len == 0) return null;
                // Last element may be the result expression
                // Phase 2: comptime blocks with statements
                if (self.comptime_vars != null) {
                    // Treat last stmt as the result expression, rest as statements
                    const result_node = stmts[stmts.len - 1];
                    return self.evalComptimeBlockFromSlice(stmts[0 .. stmts.len - 1], result_node);
                }
                return null;
            },
            .comptime_block => {
                // Try evaluating body as a comptime block with statements
                const body: Index = self.tree.nodeData(idx).node;
                const body_tag = self.tree.nodeTag(body);
                if (body_tag == .block) {
                    const body_data = self.tree.nodeData(body);
                    const stmts = self.tree.extraNodes(body_data.extra_range);
                    // Push comptime context, evaluate block with statements
                    const old_vars = self.comptime_vars;
                    const new_vars = std.StringHashMap(ComptimeValue).init(self.allocator);
                    self.comptime_vars = new_vars;
                    defer {
                        if (self.comptime_vars) |*cv| cv.deinit();
                        self.comptime_vars = old_vars;
                    }
                    if (stmts.len == 0) return null;
                    const result_node = stmts[stmts.len - 1];
                    return self.evalComptimeBlockFromSlice(stmts[0 .. stmts.len - 1], result_node);
                }
                if (body_tag == .block_one) {
                    const body_data = self.tree.nodeData(body).node_and_node;
                    const old_vars = self.comptime_vars;
                    const new_vars = std.StringHashMap(ComptimeValue).init(self.allocator);
                    self.comptime_vars = new_vars;
                    defer {
                        if (self.comptime_vars) |*cv| cv.deinit();
                        self.comptime_vars = old_vars;
                    }
                    return self.evalComptimeValue(body_data[1]);
                }
                // Single expression
                return self.evalComptimeValue(body);
            },
            .field_access => {
                // Support field access on comptime values (Phase 4: @typeInfo fields)
                const data = self.tree.nodeData(idx);
                const base_node: Index = data.node_and_token[0];
                const field_tok = data.node_and_token[1];
                const field_name = self.tree.tokenSlice(field_tok);
                const base_val = self.evalComptimeValue(base_node) orelse return null;
                return switch (base_val) {
                    .type_info => |ti| {
                        if (std.mem.eql(u8, field_name, "fields")) {
                            return ComptimeValue{ .array = .{
                                .elements = ti.fields,
                                .elem_type_name = "EnumField",
                            } };
                        }
                        if (std.mem.eql(u8, field_name, "name")) return ComptimeValue{ .string = ti.name };
                        return null;
                    },
                    .enum_field => |ef| {
                        if (std.mem.eql(u8, field_name, "name")) return ComptimeValue{ .string = ef.name };
                        if (std.mem.eql(u8, field_name, "value")) return ComptimeValue{ .int = ef.value };
                        return null;
                    },
                    .array => |arr| {
                        if (std.mem.eql(u8, field_name, "len")) return ComptimeValue{ .int = @intCast(arr.elements.items.len) };
                        return null;
                    },
                    else => null,
                };
            },
            .index => {
                // Support indexing comptime arrays: arr[i]
                const data = self.tree.nodeData(idx).node_and_node;
                const base_val = self.evalComptimeValue(data[0]) orelse return null;
                const idx_val = (self.evalComptimeValue(data[1]) orelse return null).asInt() orelse return null;
                if (base_val == .array) {
                    if (idx_val < 0 or idx_val >= @as(i64, @intCast(base_val.array.elements.items.len))) return null;
                    return base_val.array.elements.items[@intCast(idx_val)];
                }
                return null;
            },
            else => null,
        };
    }

    /// Evaluate a comptime block containing statements (var, assign, for, if) + final expression.
    /// Zig Sema: ComptimeAlloc list holds mutable values, runtime_index prevents time travel.
    fn evalComptimeBlockFromSlice(self: *Checker, stmts: []const Index, final_expr: Index) ?ComptimeValue {
        for (stmts) |stmt_idx| {
            const stmt_tag = self.tree.nodeTag(stmt_idx);
            switch (stmt_tag) {
                .var_local, .const_local => {
                    // Evaluate initializer, store in comptime_vars
                    const lv = self.tree.localVarData(stmt_idx);
                    var init_val: ComptimeValue = undefined;
                    if (lv.value.unwrap()) |val_node| {
                        const raw_val = self.evalComptimeValue(val_node) orelse return null;
                        // If initializing an array type with undefined, create a sized array
                        if (raw_val == .undefined_val) {
                            if (lv.type_expr.unwrap()) |te| {
                                if (self.evalComptimeArrayType(te)) |arr_info| {
                                    var elements = std.ArrayListUnmanaged(ComptimeValue){};
                                    elements.ensureTotalCapacity(self.allocator, @intCast(arr_info.size)) catch return null;
                                    var j: usize = 0;
                                    while (j < arr_info.size) : (j += 1) {
                                        elements.appendAssumeCapacity(ComptimeValue.undefined_val);
                                    }
                                    init_val = ComptimeValue{ .array = .{
                                        .elements = elements,
                                        .elem_type_name = arr_info.elem_name,
                                    } };
                                } else {
                                    init_val = raw_val;
                                }
                            } else {
                                init_val = raw_val;
                            }
                        } else {
                            init_val = raw_val;
                        }
                    } else {
                        init_val = ComptimeValue.undefined_val;
                    }
                    if (self.comptime_vars) |*cv| cv.put(lv.name, init_val) catch return null;
                },
                .assign, .assign_add, .assign_sub, .assign_mul => {
                    self.evalComptimeAssign(stmt_idx) orelse return null;
                },
                .for_stmt => {
                    const fs = self.tree.forData(stmt_idx);
                    if (fs.is_inline) {
                        self.evalComptimeInlineFor(fs) orelse return null;
                    } else {
                        return null; // Non-inline for not allowed in comptime
                    }
                },
                .expr_stmt => {
                    const expr_node: Index = self.tree.nodeData(stmt_idx).node;
                    _ = self.evalComptimeValue(expr_node);
                },
                else => {
                    // Try evaluating as expression
                    _ = self.evalComptimeValue(stmt_idx);
                },
            }
        }
        return self.evalComptimeValue(final_expr);
    }

    /// Evaluate comptime assignment: x = expr, x += expr, arr[i] = expr
    fn evalComptimeAssign(self: *Checker, idx: Index) ?void {
        const assign_tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx).node_and_node;
        const target_idx = data[0];
        const value_idx = data[1];
        const val = self.evalComptimeValue(value_idx) orelse return null;
        const target_tag = self.tree.nodeTag(target_idx);
        switch (target_tag) {
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(target_idx));
                if (self.comptime_vars) |*cv| {
                    // Handle compound assignment operators
                    if (assign_tag != .assign) {
                        const old = cv.get(name) orelse return null;
                        const old_int = old.asInt() orelse return null;
                        const val_int = val.asInt() orelse return null;
                        const new_val: i64 = switch (assign_tag) {
                            .assign_add => old_int + val_int,
                            .assign_sub => old_int - val_int,
                            .assign_mul => old_int * val_int,
                            else => return null,
                        };
                        cv.put(name, ComptimeValue{ .int = new_val }) catch return null;
                    } else {
                        cv.put(name, val) catch return null;
                    }
                }
            },
            .index => {
                // arr[i] = val — mutate comptime array element
                const ix_data = self.tree.nodeData(target_idx).node_and_node;
                const base_name = self.getComptimeIdentName(ix_data[0]) orelse return null;
                const index_val = (self.evalComptimeValue(ix_data[1]) orelse return null).asInt() orelse return null;
                if (self.comptime_vars) |*cv| {
                    const arr_ptr = cv.getPtr(base_name) orelse return null;
                    if (arr_ptr.* != .array) return null;
                    if (index_val < 0 or index_val >= @as(i64, @intCast(arr_ptr.array.elements.items.len))) return null;
                    arr_ptr.array.elements.items[@intCast(index_val)] = val;
                }
            },
            else => return null,
        }
    }

    /// Extract identifier name from a node (for comptime array mutation)
    fn getComptimeIdentName(self: *Checker, idx: Index) ?[]const u8 {
        if (self.tree.nodeTag(idx) == .ident) return self.tree.tokenSlice(self.tree.nodeMainToken(idx));
        return null;
    }

    const ComptimeArrayTypeInfo = struct { size: usize, elem_name: []const u8 };

    /// Extract comptime array type info from a type expression node.
    /// For `[5]i64`, returns { size: 5, elem_name: "i64" }.
    fn evalComptimeArrayType(self: *Checker, type_node_idx: Index) ?ComptimeArrayTypeInfo {
        const node_tag = self.tree.nodeTag(type_node_idx);
        if (node_tag != .type_array) return null;
        const data = self.tree.nodeData(type_node_idx).node_and_node;
        const size_node = data[0];
        const elem_node = data[1];
        // Evaluate array size as comptime int
        const size_val = self.evalConstExpr(size_node) orelse return null;
        if (size_val <= 0) return null;
        // Get element type name
        const elem_tag = self.tree.nodeTag(elem_node);
        if (elem_tag == .type_named) {
            return .{ .size = @intCast(size_val), .elem_name = self.tree.tokenSlice(self.tree.nodeMainToken(elem_node)) };
        }
        return .{ .size = @intCast(size_val), .elem_name = "unknown" };
    }

    /// Evaluate inline for in comptime context
    fn evalComptimeInlineFor(self: *Checker, fs: ast.full.ForFull) ?void {
        const is_range = fs.range_start != .none;
        if (is_range) {
            // Range iteration: inline for i in 0..N
            const start_node = fs.range_start.unwrap() orelse return null;
            const end_node = fs.range_end.unwrap() orelse return null;
            const start_val = (self.evalComptimeValue(start_node) orelse return null).asInt() orelse return null;
            const end_val = (self.evalComptimeValue(end_node) orelse return null).asInt() orelse return null;
            var i = start_val;
            while (i < end_val) : (i += 1) {
                if (self.comptime_vars) |*cv| cv.put(fs.binding, ComptimeValue{ .int = i }) catch return null;
                // Evaluate body as comptime statements
                self.evalComptimeForBody(fs.body) orelse return null;
            }
        } else {
            // Comptime array iteration: inline for field in @typeInfo(T).fields
            const iter_node = fs.iterable.unwrap() orelse return null;
            const iter_val = self.evalComptimeValue(iter_node) orelse return null;
            if (iter_val != .array) return null;
            for (iter_val.array.elements.items, 0..) |elem, eidx| {
                if (self.comptime_vars) |*cv| {
                    cv.put(fs.binding, elem) catch return null;
                    if (fs.index_binding_token.unwrap()) |ib_tok| cv.put(self.tree.tokenSlice(ib_tok), ComptimeValue{ .int = @intCast(eidx) }) catch return null;
                }
                // Evaluate body
                self.evalComptimeForBody(fs.body) orelse return null;
            }
        }
    }

    /// Helper: evaluate a for-loop body in comptime context.
    fn evalComptimeForBody(self: *Checker, body_idx: Index) ?void {
        const body_tag = self.tree.nodeTag(body_idx);
        switch (body_tag) {
            .block_stmt => {
                const data = self.tree.nodeData(body_idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                for (stmts) |stmt_idx| {
                    self.evalComptimeForBodyStmt(stmt_idx) orelse return null;
                }
            },
            .block_one => {
                const data = self.tree.nodeData(body_idx).node_and_node;
                self.evalComptimeForBodyStmt(data[0]) orelse return null;
            },
            .block_two => {
                const data = self.tree.nodeData(body_idx).node_and_node;
                self.evalComptimeForBodyStmt(data[0]) orelse return null;
                self.evalComptimeForBodyStmt(data[1]) orelse return null;
            },
            .assign, .assign_add, .assign_sub, .assign_mul => {
                self.evalComptimeAssign(body_idx) orelse return null;
            },
            .expr_stmt => {
                const expr_node: Index = self.tree.nodeData(body_idx).node;
                _ = self.evalComptimeValue(expr_node);
            },
            else => return null,
        }
    }

    /// Helper: evaluate a single statement inside a comptime for body.
    fn evalComptimeForBodyStmt(self: *Checker, stmt_idx: Index) ?void {
        const stmt_tag = self.tree.nodeTag(stmt_idx);
        switch (stmt_tag) {
            .assign, .assign_add, .assign_sub, .assign_mul => {
                self.evalComptimeAssign(stmt_idx) orelse return null;
            },
            .expr_stmt => {
                const expr_node: Index = self.tree.nodeData(stmt_idx).node;
                _ = self.evalComptimeValue(expr_node);
            },
            .var_local, .const_local => {
                const lv = self.tree.localVarData(stmt_idx);
                const init_val = if (lv.value.unwrap()) |val_node|
                    self.evalComptimeValue(val_node) orelse return null
                else
                    ComptimeValue.undefined_val;
                if (self.comptime_vars) |*cv| cv.put(lv.name, init_val) catch return null;
            },
            else => return null,
        }
    }

    // ---------------------------------------------------------------
    // evalConstExpr — integer constant folding (ported from compiler/ line 1473)
    // ---------------------------------------------------------------

    pub fn evalConstExpr(self: *Checker, idx: Index) ?i64 {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .literal_int => std.fmt.parseInt(i64, self.tree.tokenSlice(self.tree.nodeMainToken(idx)), 0) catch null,
            .literal_true => 1,
            .literal_false => 0,
            .unary_neg => if (self.evalConstExpr(self.tree.nodeData(idx).node)) |op| -op else null,
            .unary_bit_not => if (self.evalConstExpr(self.tree.nodeData(idx).node)) |op| ~op else null,
            .unary_not => if (self.evalConstExpr(self.tree.nodeData(idx).node)) |op| (if (op == 0) @as(i64, 1) else @as(i64, 0)) else null,
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_bit_and, .binary_bit_or, .binary_bit_xor, .binary_shl, .binary_shr,
            .binary_eq, .binary_neq, .binary_lt, .binary_lte, .binary_gt, .binary_gte,
            => {
                const data = self.tree.nodeData(idx).node_and_node;
                // Try integer const-fold first
                if (self.evalConstExpr(data[0])) |l| {
                    if (self.evalConstExpr(data[1])) |r| {
                        return switch (tag) {
                            .binary_add => l + r,
                            .binary_sub => l - r,
                            .binary_mul => l * r,
                            .binary_div => if (r != 0) @divTrunc(l, r) else null,
                            .binary_mod => if (r != 0) @rem(l, r) else null,
                            .binary_bit_and => l & r,
                            .binary_bit_or => l | r,
                            .binary_bit_xor => l ^ r,
                            .binary_shl => l << @intCast(r),
                            .binary_shr => l >> @intCast(r),
                            .binary_eq => if (l == r) @as(i64, 1) else @as(i64, 0),
                            .binary_neq => if (l != r) @as(i64, 1) else @as(i64, 0),
                            .binary_lt => if (l < r) @as(i64, 1) else @as(i64, 0),
                            .binary_lte => if (l <= r) @as(i64, 1) else @as(i64, 0),
                            .binary_gt => if (l > r) @as(i64, 1) else @as(i64, 0),
                            .binary_gte => if (l >= r) @as(i64, 1) else @as(i64, 0),
                            else => null,
                        };
                    }
                }
                // Fallback: comptime string equality (for @targetOs() == "linux" etc.)
                if (tag == .binary_eq or tag == .binary_neq) {
                    if (self.evalConstString(data[0])) |ls| {
                        if (self.evalConstString(data[1])) |rs| {
                            const equal = std.mem.eql(u8, ls, rs);
                            return if (tag == .binary_eq)
                                (if (equal) @as(i64, 1) else @as(i64, 0))
                            else
                                (if (!equal) @as(i64, 1) else @as(i64, 0));
                        }
                    }
                }
                return null;
            },
            .paren => self.evalConstExpr(self.tree.nodeData(idx).node),
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                if (self.scope.lookup(name)) |sym| {
                    if (sym.kind == .constant) return sym.const_value;
                }
                return null;
            },
            // Go: cmd/compile/internal/ir/const.go — const folding includes sizeof.
            // Zig: Sema.zig resolves @sizeOf at comptime. We follow Go's limited approach.
            .builtin_call => {
                const bc = self.tree.builtinCallData(idx);
                if (bc.kind == .size_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    return @as(i64, @intCast(self.types.sizeOf(type_idx)));
                }
                // @alignOf(T) — comptime, Zig Sema.zig
                if (bc.kind == .align_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    return @as(i64, @intCast(self.types.alignmentOf(type_idx)));
                }
                // @offsetOf(T, "field") — always comptime, Zig Sema.zig:23060
                if (bc.kind == .offset_of) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const name_str = self.evalConstString(arg0) orelse return null;
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
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    if (self.evalConstExpr(arg0)) |v| {
                        return if (v != 0) @as(i64, 1) else @as(i64, 0);
                    }
                    return null;
                }
                // @min(a, b) / @max(a, b) — comptime fold
                if (bc.kind == .min) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const arg1 = bc.arg1.unwrap() orelse return null;
                    if (self.evalConstExpr(arg0)) |a| {
                        if (self.evalConstExpr(arg1)) |b| {
                            return if (a < b) a else b;
                        }
                    }
                    return null;
                }
                if (bc.kind == .max) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const arg1 = bc.arg1.unwrap() orelse return null;
                    if (self.evalConstExpr(arg0)) |a| {
                        if (self.evalConstExpr(arg1)) |b| {
                            return if (a > b) a else b;
                        }
                    }
                    return null;
                }
                if (bc.kind == .has_field) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    const name_str = self.evalConstString(arg0) orelse return null;
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
                // @intFromEnum(e) — comptime when arg resolves to enum variant value
                // Zig Sema.zig:8420 — evaluate to backing integer at comptime.
                if (bc.kind == .int_from_enum) {
                    const arg0 = bc.arg0.unwrap() orelse return null;
                    if (self.evalConstExpr(arg0)) |v| return v;
                    return null;
                }
                // @enumLen(T) — comptime, returns number of enum variants
                if (bc.kind == .enum_len) {
                    const type_arg = bc.type_arg.unwrap() orelse return null;
                    const type_idx = self.resolveTypeExpr(type_arg) catch return null;
                    if (type_idx == .invalid) return null;
                    const info = self.types.get(type_idx);
                    if (info == .enum_type) return @intCast(info.enum_type.variants.len);
                    return null;
                }
                return null;
            },
            // Enum field access: Color.Red → integer variant value
            // Zig Sema.zig: enum values are comptime-known when accessed as Type.field.
            .field_access => {
                const data = self.tree.nodeData(idx);
                const base_node: Index = data.node_and_token[0];
                const field_tok = data.node_and_token[1];
                const field_name = self.tree.tokenSlice(field_tok);
                if (self.tree.nodeTag(base_node) == .ident) {
                    const base_name = self.tree.tokenSlice(self.tree.nodeMainToken(base_node));
                    const type_idx = self.resolveTypeByName(base_name) orelse return null;
                    const info = self.types.get(type_idx);
                    if (info == .enum_type) {
                        for (info.enum_type.variants) |v| {
                            if (std.mem.eql(u8, v.name, field_name)) return @intCast(v.value);
                        }
                    }
                }
                return null;
            },
            // Comptime if-expression: fold when condition is comptime-known
            .if_simple => {
                const data = self.tree.nodeData(idx).node_and_node;
                const cond_val = self.evalConstExpr(data[0]) orelse return null;
                if (cond_val != 0) return self.evalConstExpr(data[1]);
                return null;
            },
            .if_full => {
                const data = self.tree.nodeData(idx);
                const cond_node = data.node_and_extra[0];
                const extra = self.tree.extraData(data.node_and_extra[1], ast.IfData);
                const cond_val = self.evalConstExpr(cond_node) orelse return null;
                if (cond_val != 0) return self.evalConstExpr(extra.then_node);
                if (extra.else_node.unwrap()) |else_node| return self.evalConstExpr(else_node);
                return null;
            },
            // Block expression: { stmts; expr } — evaluate final expr if no stmts
            .block_one => {
                const data = self.tree.nodeData(idx).node_and_node;
                return self.evalConstExpr(data[1]);
            },
            .block => {
                const data = self.tree.nodeData(idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                if (stmts.len == 0) return null;
                // For a simple evalConstExpr, only evaluate the last expression
                return self.evalConstExpr(stmts[stmts.len - 1]);
            },
            // comptime { body } — evaluate body at compile time
            .comptime_block => self.evalConstExpr(self.tree.nodeData(idx).node),
            else => null,
        };
    }

    /// Evaluate a comptime float expression. Returns the f64 value or null.
    pub fn evalConstFloat(self: *Checker, idx: Index) ?f64 {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .literal_float => std.fmt.parseFloat(f64, self.tree.tokenSlice(self.tree.nodeMainToken(idx))) catch null,
            .literal_int => blk: {
                const iv = std.fmt.parseInt(i64, self.tree.tokenSlice(self.tree.nodeMainToken(idx)), 0) catch break :blk null;
                break :blk @floatFromInt(iv);
            },
            .unary_neg => blk: {
                const v = self.evalConstFloat(self.tree.nodeData(idx).node) orelse break :blk null;
                break :blk -v;
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div => blk: {
                const data = self.tree.nodeData(idx).node_and_node;
                const l = self.evalConstFloat(data[0]) orelse break :blk null;
                const r = self.evalConstFloat(data[1]) orelse break :blk null;
                break :blk switch (tag) {
                    .binary_add => l + r,
                    .binary_sub => l - r,
                    .binary_mul => l * r,
                    .binary_div => l / r,
                    else => null,
                };
            },
            .paren => self.evalConstFloat(self.tree.nodeData(idx).node),
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                if (self.scope.lookup(name)) |sym| {
                    if (sym.kind == .constant) return sym.float_const_value;
                }
                return null;
            },
            else => null,
        };
    }

    fn isFloatType(_: *Checker, type_idx: TypeIndex) bool {
        return type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.F64;
    }

    /// Evaluate a comptime string expression. Returns the string value or null.
    pub fn evalConstString(self: *Checker, idx: Index) ?[]const u8 {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .literal_string => blk: {
                // Strip surrounding quotes from string literal (scanner includes them)
                const v = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                break :blk if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') v[1 .. v.len - 1] else v;
            },
            .paren => self.evalConstString(self.tree.nodeData(idx).node),
            .builtin_call => {
                const bc = self.tree.builtinCallData(idx);
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

    // ---------------------------------------------------------------
    // Expression checking (ported from compiler/ lines 1706-2300)
    // ---------------------------------------------------------------

    pub fn checkExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
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

    fn checkExprInner(self: *Checker, idx: Index) CheckError!TypeIndex {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .ident => self.checkIdentifier(idx),
            .literal_int, .literal_float, .literal_string, .literal_char,
            .literal_true, .literal_false, .literal_null, .literal_undefined,
            .literal_unreachable,
            => self.checkLiteral(tag),
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte,
            .binary_and, .binary_or, .binary_bit_and, .binary_bit_or,
            .binary_bit_xor, .binary_shl, .binary_shr, .binary_concat, .binary_pipe,
            => self.checkBinary(idx, tag),
            .unary_neg, .unary_not, .unary_bit_not, .unary_unwrap => self.checkUnary(idx, tag),
            .call_zero, .call_one, .call => self.checkCall(idx),
            .index => self.checkIndex(idx),
            .slice => self.checkSliceExpr(idx),
            .field_access => self.checkFieldAccess(idx),
            .array_literal_empty, .array_literal_one, .array_literal => self.checkArrayLiteral(idx),
            .paren => self.checkExpr(self.tree.nodeData(idx).node),
            .if_simple, .if_full => self.checkIfExpr(idx),
            .switch_expr => self.checkSwitchExpr(idx),
            .block_one, .block_two, .block => self.checkBlock(idx),
            .struct_init_one, .struct_init => self.checkStructInit(idx),
            .new_expr => self.checkNewExpr(idx),
            .builtin_call => self.checkBuiltinCall(idx),
            .string_interp => self.checkStringInterp(idx),
            .unary_try => self.checkTryExpr(idx),
            .unary_await => self.checkAwaitExpr(idx),
            .task_expr => self.checkTaskExpr(idx),
            .catch_expr => self.checkCatchExpr(idx),
            .orelse_expr => self.checkOrElseExpr(idx),
            .error_literal => self.checkErrorLiteral(idx),
            .closure_expr => self.checkClosureExpr(idx),
            .unary_addr_of => self.checkAddrOf(idx),
            .unary_deref => self.checkDeref(idx),
            .tuple_literal => self.checkTupleLiteral(idx),
            .comptime_block => {
                // Zig Sema pattern: comptime {} body must be comptime-evaluable.
                // Check the body for type errors, then verify it produces a comptime value.
                const body: Index = self.tree.nodeData(idx).node;
                const body_tag = self.tree.nodeTag(body);
                if (body_tag == .block) {
                    const body_data = self.tree.nodeData(body);
                    const stmts = self.tree.extraNodes(body_data.extra_range);
                    // Type-check stmts in a new scope (vars defined in comptime block)
                    var block_scope = Scope.init(self.allocator, self.scope);
                    defer block_scope.deinit();
                    const old_scope = self.scope;
                    self.scope = &block_scope;
                    for (stmts) |stmt_idx| {
                        try self.checkStmt(stmt_idx);
                    }
                    self.scope = old_scope;
                    // Verify comptime evaluability — set up comptime context
                    const old_vars = self.comptime_vars;
                    const new_vars = std.StringHashMap(ComptimeValue).init(self.allocator);
                    self.comptime_vars = new_vars;
                    defer {
                        if (self.comptime_vars) |*cv| cv.deinit();
                        self.comptime_vars = old_vars;
                    }
                    if (stmts.len > 0) {
                        if (self.evalComptimeBlockFromSlice(stmts[0 .. stmts.len - 1], stmts[stmts.len - 1]) == null) {
                            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "unable to evaluate comptime expression");
                        }
                    }
                    return TypeRegistry.VOID;
                }
                // Single expression path
                const body_type = try self.checkExpr(body);
                if (self.evalComptimeValue(body) == null) {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "unable to evaluate comptime expression");
                }
                return body_type;
            },
            .zero_init => if (self.expected_type != .invalid) self.expected_type else TypeRegistry.VOID,
            .bad_node => .invalid,
            else => .invalid,
        };
    }

    fn checkIdentifier(self: *Checker, idx: Index) TypeIndex {
        const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
        const span_start = self.tree.nodeSpan(idx).start;
        // Swift SE-0430: use-after-send detection.
        // Swift reference: RegionAnalysis.cpp PartitionOpKind::Require
        //   A Require on a sent element produces LocalUseAfterSendError.
        // Cot Phase 1: checker-level tracking via sent_variables map.
        if (self.sent_variables.contains(name)) {
            self.err.errorWithCode(span_start, .e300,
                "use of variable after it has been sent; sending transfers ownership");
        }
        if (self.resolveTypeByName(name)) |type_idx| {
            const t = self.types.get(type_idx);
            // Allow enum, union, and error_set types to be used as expressions
            // (enum/union for variant access, error_set for merge with ||)
            if (t == .enum_type or t == .union_type or t == .error_set) return type_idx;
            self.err.errorWithCode(span_start, .e301, "type name cannot be used as expression");
            return .invalid;
        }
        // Go pattern: generic function names cannot be used without instantiation.
        // Go types2/call.go:33 — funcInst() requires IndexExpr (explicit type args).
        if (self.generics.generic_functions.contains(name)) {
            self.err.errorWithCode(span_start, .e300, "generic function requires type arguments");
            return .invalid;
        }
        if (self.scope.lookup(name)) |sym| {
            if (self.lint_mode) self.scope.markUsed(name);
            return sym.type_idx;
        }
        // Check type substitution (for type params used as values)
        if (self.type_substitution) |sub| {
            if (sub.get(name)) |_| return .invalid;
        }
        self.errWithSuggestion(span_start, "undefined identifier", self.findSimilarName(name));
        return .invalid;
    }

    fn checkLiteral(_: *Checker, tag: Tag) TypeIndex {
        return switch (tag) {
            .literal_int => TypeRegistry.UNTYPED_INT,
            .literal_float => TypeRegistry.UNTYPED_FLOAT,
            .literal_string => TypeRegistry.STRING,
            .literal_char => TypeRegistry.U8,
            .literal_true, .literal_false => TypeRegistry.UNTYPED_BOOL,
            .literal_null, .literal_undefined => TypeRegistry.UNTYPED_NULL,
            .literal_unreachable => TypeRegistry.NORETURN,
            else => .invalid,
        };
    }

    fn checkBinary(self: *Checker, idx: Index, tag: Tag) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx).node_and_node;
        const left_node = data[0];
        const right_node = data[1];

        const left_type = try self.checkExpr(left_node);

        // Enum/Union variant comparison: set context so .variant shorthand resolves for RHS.
        // Zig Sema pattern: same as switch enum context but for == / != with enum/union LHS.
        // @safe auto-deref: if LHS is *Enum/*Union, unwrap to Enum/Union for comparison context.
        const old_switch_enum = self.current_switch_enum_type;
        if (tag == .binary_eq or tag == .binary_neq) {
            var cmp_type = left_type;
            const left_info = self.types.get(left_type);
            if (left_info == .pointer) {
                const elem = self.types.get(left_info.pointer.elem);
                if (elem == .union_type or elem == .enum_type) {
                    cmp_type = left_info.pointer.elem;
                }
            }
            const cmp_info = self.types.get(cmp_type);
            if (cmp_info == .union_type or cmp_info == .enum_type) {
                self.current_switch_enum_type = cmp_type;
            }
        }
        const right_type = try self.checkExpr(right_node);
        self.current_switch_enum_type = old_switch_enum;

        const left = self.types.get(left_type);
        const right = self.types.get(right_type);

        // Distinct types: Go spec — "if one operand is an untyped constant and the other
        // is not, the constant is implicitly converted to the type of the other operand."
        // Go reference: expr.go:810 (identity check), predicates.go:37 (underlying check)
        {
            // Resolve: if one side is distinct and the other is untyped, treat as same-distinct
            var distinct_type: ?TypeIndex = null;

            if (left == .distinct and right == .distinct and
                std.mem.eql(u8, left.distinct.name, right.distinct.name))
            {
                distinct_type = left_type;
            } else if (left == .distinct and types_mod.isUntyped(right)) {
                if (self.types.isAssignable(right_type, left.distinct.underlying)) {
                    distinct_type = left_type;
                }
            } else if (right == .distinct and types_mod.isUntyped(left)) {
                if (self.types.isAssignable(left_type, right.distinct.underlying)) {
                    distinct_type = right_type;
                }
            }
            // Pointer arithmetic: distinct_int + typed_int → distinct_int
            // C reference: pointer + int → pointer (not Go — Go has no pointer arithmetic)
            // Only for add/sub, only when underlying is integer
            if (distinct_type == null) {
                if (left == .distinct and types_mod.isInteger(self.types.get(left.distinct.underlying)) and types_mod.isInteger(right)) {
                    if (tag == .binary_add or tag == .binary_sub) distinct_type = left_type;
                } else if (right == .distinct and types_mod.isInteger(self.types.get(right.distinct.underlying)) and types_mod.isInteger(left)) {
                    if (tag == .binary_add) distinct_type = right_type; // int + ptr → ptr (add only, not sub)
                }
            }

            if (distinct_type) |dt| {
                const ul = self.types.get(self.types.get(dt).distinct.underlying);
                switch (tag) {
                    .binary_eq, .binary_neq, .binary_lt, .binary_lte, .binary_gt, .binary_gte => {
                        if (types_mod.isNumeric(ul) or (ul == .basic and ul.basic == .bool_type)) return TypeRegistry.BOOL;
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation");
                        return .invalid;
                    },
                    .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod => {
                        if (types_mod.isNumeric(ul)) return dt;
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation");
                        return .invalid;
                    },
                    .binary_bit_and, .binary_bit_or, .binary_bit_xor, .binary_shl, .binary_shr => {
                        if (types_mod.isInteger(ul)) return dt;
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation");
                        return .invalid;
                    },
                    else => {},
                }
            }
        }

        switch (tag) {
            .binary_concat => {
                // String ++ String → String
                if (left_type == TypeRegistry.STRING and right_type == TypeRegistry.STRING) return TypeRegistry.STRING;
                // [N]T ++ [M]T → [N+M]T (array concat, Zig Sema.zig:zirArrayCat)
                if (left == .array and right == .array) {
                    if (left.array.elem == right.array.elem) {
                        return self.types.makeArray(left.array.elem, left.array.length + right.array.length) catch return .invalid;
                    }
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'++' requires matching element types");
                    return .invalid;
                }
                // []T ++ []T → []T (slice concat)
                if (left == .slice and right == .slice) {
                    if (left.slice.elem == right.slice.elem) return left_type;
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'++' requires matching element types");
                    return .invalid;
                }
                self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'++' requires two strings, arrays, or slices of the same type");
                return .invalid;
            },
            .binary_add => {
                // String + String: @safe desugars to ++, normal mode errors
                if (left_type == TypeRegistry.STRING and right_type == TypeRegistry.STRING) {
                    if (self.safe_mode) return TypeRegistry.STRING;
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'+' cannot concatenate strings; use '++' operator");
                    return .invalid;
                }
                // Array + Array: @safe desugars to ++, normal mode errors
                if (left == .array and right == .array and left.array.elem == right.array.elem) {
                    if (self.safe_mode) return self.types.makeArray(left.array.elem, left.array.length + right.array.length) catch return .invalid;
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'+' cannot concatenate arrays; use '++' operator");
                    return .invalid;
                }
                // Slice + Slice: @safe desugars to ++, normal mode errors
                if (left == .slice and right == .slice and left.slice.elem == right.slice.elem) {
                    if (self.safe_mode) return left_type;
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "'+' cannot concatenate slices; use '++' operator");
                    return .invalid;
                }
                if (left == .pointer and types_mod.isInteger(right)) return left_type;
                if (types_mod.isInteger(left) and right == .pointer) return right_type;
                if (!types_mod.isNumeric(left) or !types_mod.isNumeric(right)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                // Zig Sema.zig:peerType — common type of both operands
                return TypeRegistry.commonType(left_type, right_type);
            },
            .binary_sub => {
                if (left == .pointer and types_mod.isInteger(right)) return left_type;
                if (!types_mod.isNumeric(left) or !types_mod.isNumeric(right)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .binary_mul, .binary_div, .binary_mod => {
                if (!types_mod.isNumeric(left) or !types_mod.isNumeric(right)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .binary_eq, .binary_neq, .binary_lt, .binary_lte, .binary_gt, .binary_gte => {
                // Union variant tag comparison: union == .variant / union == Union.Variant
                // @safe auto-deref: *Union treated as Union for comparison
                if (tag == .binary_eq or tag == .binary_neq) {
                    const left_u = if (left == .pointer and self.types.get(left.pointer.elem) == .union_type) self.types.get(left.pointer.elem) else left;
                    const right_u = if (right == .pointer and self.types.get(right.pointer.elem) == .union_type) self.types.get(right.pointer.elem) else right;
                    if (left_u == .union_type and self.isUnionVariantRef(right_node, left_u.union_type)) return TypeRegistry.BOOL;
                    if (right_u == .union_type and self.isUnionVariantRef(left_node, right_u.union_type)) return TypeRegistry.BOOL;
                }
                if (!self.isComparable(left_type, right_type)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                return TypeRegistry.BOOL;
            },
            .binary_and, .binary_or => {
                // keyword `and` / `or` — boolean logical operators
                if (!types_mod.isBool(left) or !types_mod.isBool(right)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                return TypeRegistry.BOOL;
            },
            .binary_bit_and, .binary_bit_or, .binary_bit_xor, .binary_shl, .binary_shr => {
                if (!types_mod.isInteger(left) or !types_mod.isInteger(right)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation"); return .invalid; }
                return TypeRegistry.commonType(left_type, right_type);
            },
            .binary_pipe => {
                // Error set merge: FileError || NetError — Zig Sema.zig:8299
                // (binary_pipe used for `||` when parser maps it — currently may be `|>` pipe)
                if (left == .error_set and right == .error_set) {
                    return try self.mergeErrorSets(left.error_set, right.error_set);
                }
                // Bool logical OR
                if (types_mod.isBool(left) and types_mod.isBool(right)) return TypeRegistry.BOOL;
                self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "invalid operation");
                return .invalid;
            },
            else => return .invalid,
        }
    }

    /// Merge two error sets into a new error set with deduplicated variants.
    /// Zig Sema.zig:8299 — union variant names, anyerror absorbs.
    fn mergeErrorSets(self: *Checker, a: types_mod.ErrorSetType, b: types_mod.ErrorSetType) !TypeIndex {
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

    fn checkUnary(self: *Checker, idx: Index, tag: Tag) CheckError!TypeIndex {
        const operand_node: Index = self.tree.nodeData(idx).node;
        const operand_type = try self.checkExpr(operand_node);
        const operand = self.types.get(operand_type);
        switch (tag) {
            .unary_neg => { if (!types_mod.isNumeric(operand)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e303, "unary '-' requires numeric operand"); return .invalid; } return operand_type; },
            .unary_not => { if (!types_mod.isBool(operand)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e303, "unary '!' requires bool operand"); return .invalid; } return TypeRegistry.BOOL; },
            .unary_bit_not => { if (!types_mod.isInteger(operand)) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e303, "unary '~' requires integer operand"); return .invalid; } return operand_type; },
            .unary_unwrap => { if (operand == .optional) return operand.optional.elem; self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e303, "'.?' requires optional operand"); return .invalid; },
            else => return .invalid,
        }
    }

    fn checkClosureExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const ce = self.tree.closureData(idx);
        // Build function type from params + return type
        const func_type = try self.buildFuncType(ce.params, ce.return_type);
        // Check body in a child scope with params defined
        var func_scope = Scope.init(self.allocator, self.scope);
        defer func_scope.deinit();
        // Iterate closure params
        const param_raw = self.tree.extraSlice(ce.params);
        for (param_raw) |raw_ei| {
            const field = self.tree.extraData(@enumFromInt(raw_ei), ast.Field);
            const param_name = self.tree.tokenSlice(field.name_token);
            const param_type = try self.resolveTypeExpr(field.type_expr);
            try func_scope.define(Symbol.init(param_name, .parameter, param_type, idx, false));
        }
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &func_scope;
        const ret_type = if (self.types.get(func_type) == .func) self.types.get(func_type).func.return_type else TypeRegistry.VOID;
        self.current_return_type = ret_type;
        try self.checkBlockExpr(ce.body);
        self.scope = old_scope;
        self.current_return_type = old_return;

        // @Sendable closures: verify all captured variables are Sendable.
        // Swift reference: TypeCheckConcurrency.cpp:2971-3098 checkLocalCaptures
        // A captured variable is any identifier in the body that resolves to the
        // PARENT scope (not the closure's own scope or params).
        if (ce.is_sendable) {
            try self.checkSendableCaptures(ce.body, &func_scope, self.tree.nodeSpan(idx));
        }

        // Async closures: wrap in task type so await is valid on the call result.
        // Swift reference: async closure call returns Task<T>.
        if (ce.is_async) {
            const inner_ret = if (self.types.get(func_type) == .func) self.types.get(func_type).func.return_type else TypeRegistry.VOID;
            const task_type = try self.types.makeTask(inner_ret);
            // Build a new func type with task return for type inference on call sites.
            const async_func_params = if (self.types.get(func_type) == .func) self.types.get(func_type).func.params else &.{};
            return try self.types.makeFunc(async_func_params, task_type);
        }

        return func_type;
    }

    /// Check that all variables captured by a @Sendable closure are Sendable.
    /// Swift reference: TypeCheckConcurrency.cpp:2971-3098 checkLocalCaptures.
    /// Walks the closure body AST, finds identifiers that resolve to the enclosing
    /// scope (not the closure's own params), and verifies each is Sendable.
    fn checkSendableCaptures(self: *Checker, body: Index, closure_scope: *Scope, span: Span) CheckError!void {
        const body_tag = self.tree.nodeTag(body);
        // Check expressions for captured identifiers
        switch (body_tag) {
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(body));
                // If this ident resolves in the closure's own scope (params), skip.
                // If it resolves in the PARENT scope, it's a capture.
                if (closure_scope.lookupLocal(name) != null) return; // param, not capture
                if (self.scope.lookup(name)) |sym| {
                    if (sym.kind == .parameter or sym.kind == .variable or sym.kind == .constant) {
                        if (!self.isSendable(sym.type_idx)) {
                            self.err.errorWithCode(span.start, .e300,
                                "capture of non-Sendable type in @Sendable closure");
                        }
                    }
                }
            },
            .call_zero => {
                const callee: Index = self.tree.nodeData(body).node;
                try self.checkSendableCaptures(callee, closure_scope, span);
            },
            .call_one => {
                const call_data = self.tree.nodeData(body).node_and_node;
                try self.checkSendableCaptures(call_data[0], closure_scope, span);
                try self.checkSendableCaptures(call_data[1], closure_scope, span);
            },
            .call => {
                const call_data = self.tree.nodeData(body);
                const callee: Index = call_data.node_and_extra[0];
                try self.checkSendableCaptures(callee, closure_scope, span);
                const args_start = call_data.node_and_extra[1];
                const arg_count = self.tree.extra_data[@intFromEnum(args_start)];
                const args_begin = @intFromEnum(args_start) + 1;
                for (self.tree.extra_data[args_begin .. args_begin + arg_count]) |raw_arg| {
                    const arg_idx: Index = @enumFromInt(raw_arg);
                    try self.checkSendableCaptures(arg_idx, closure_scope, span);
                }
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte,
            .binary_and, .binary_or, .binary_bit_and, .binary_bit_or,
            .binary_bit_xor, .binary_shl, .binary_shr, .binary_concat, .binary_pipe,
            => {
                const bin_data = self.tree.nodeData(body).node_and_node;
                try self.checkSendableCaptures(bin_data[0], closure_scope, span);
                try self.checkSendableCaptures(bin_data[1], closure_scope, span);
            },
            .unary_neg, .unary_not, .unary_bit_not, .unary_addr_of, .unary_deref,
            .unary_try, .unary_await, .unary_unwrap,
            => {
                const operand: Index = self.tree.nodeData(body).node;
                try self.checkSendableCaptures(operand, closure_scope, span);
            },
            .field_access => {
                const fa_data = self.tree.nodeData(body);
                const base: Index = fa_data.node_and_token[0];
                try self.checkSendableCaptures(base, closure_scope, span);
            },
            .index => {
                const ix_data = self.tree.nodeData(body).node_and_node;
                try self.checkSendableCaptures(ix_data[0], closure_scope, span);
                try self.checkSendableCaptures(ix_data[1], closure_scope, span);
            },
            .block_one => {
                const blk_data = self.tree.nodeData(body).node_and_node;
                try self.checkSendableCaptures(blk_data[0], closure_scope, span);
                try self.checkSendableCaptures(blk_data[1], closure_scope, span);
            },
            .block => {
                const blk_data = self.tree.nodeData(body);
                const stmts = self.tree.extraNodes(blk_data.extra_range);
                for (stmts) |s| try self.checkSendableCaptures(s, closure_scope, span);
            },
            // Check statements
            .return_expr => {
                const val: Index = self.tree.nodeData(body).node;
                try self.checkSendableCaptures(val, closure_scope, span);
            },
            .var_local, .const_local => {
                const lv = self.tree.localVarData(body);
                if (lv.value.unwrap()) |val_node| try self.checkSendableCaptures(val_node, closure_scope, span);
            },
            .expr_stmt => {
                const expr: Index = self.tree.nodeData(body).node;
                try self.checkSendableCaptures(expr, closure_scope, span);
            },
            .if_stmt_simple => {
                const if_data = self.tree.nodeData(body).node_and_node;
                try self.checkSendableCaptures(if_data[0], closure_scope, span);
                try self.checkSendableCaptures(if_data[1], closure_scope, span);
            },
            .if_stmt => {
                const if_data = self.tree.nodeData(body);
                const cond: Index = if_data.node_and_extra[0];
                const extra = self.tree.extraData(if_data.node_and_extra[1], ast.IfData);
                try self.checkSendableCaptures(cond, closure_scope, span);
                try self.checkSendableCaptures(extra.then_node, closure_scope, span);
                if (extra.else_node.unwrap()) |else_node| try self.checkSendableCaptures(else_node, closure_scope, span);
            },
            .block_stmt => {
                const stmts = self.tree.extraNodes(self.tree.nodeData(body).extra_range);
                for (stmts) |s| try self.checkSendableCaptures(s, closure_scope, span);
            },
            else => {},
        }
    }

    /// Get arg nodes from a call expression. Returns a dynamically allocated slice
    /// because call_one stores the single arg differently.
    fn getCallArgNodes(self: *Checker, idx: Index) []const Index {
        const call_tag = self.tree.nodeTag(idx);
        return switch (call_tag) {
            .call_zero => &.{},
            .call_one => blk: {
                const single: Index = self.tree.nodeData(idx).node_and_node[1];
                // Return a pointer to the data — it's in the node array so it's stable.
                // We need to return a slice; use allocator for a single-element slice.
                const slice = self.allocator.alloc(Index, 1) catch return &.{};
                slice[0] = single;
                break :blk slice;
            },
            .call => blk: {
                const data = self.tree.nodeData(idx);
                const extra_idx = data.node_and_extra[1];
                const arg_count = self.tree.extra_data[@intFromEnum(extra_idx)];
                const args_begin = @intFromEnum(extra_idx) + 1;
                const raw = self.tree.extra_data[args_begin .. args_begin + arg_count];
                break :blk @ptrCast(raw);
            },
            else => &.{},
        };
    }

    fn checkCall(self: *Checker, idx: Index) CheckError!TypeIndex {
        const call = self.tree.callData(idx);
        const callee_idx = call.callee;
        const args = self.getCallArgNodes(idx);
        const arg_count = args.len;
        const span_start = self.tree.nodeSpan(idx).start;

        // Check if callee is a special builtin name
        if (self.tree.nodeTag(callee_idx) == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(callee_idx));
            if (std.mem.eql(u8, name, "len")) return self.checkBuiltinLen(idx);
            if (std.mem.eql(u8, name, "append")) return self.checkBuiltinAppend(idx);
            if (std.mem.eql(u8, name, "print") or std.mem.eql(u8, name, "println") or std.mem.eql(u8, name, "eprint") or std.mem.eql(u8, name, "eprintln")) {
                if (arg_count == 1) _ = try self.checkExpr(args[0]);
                return TypeRegistry.VOID;
            }
            if (std.mem.eql(u8, name, "__string_make")) {
                if (arg_count != 2) { self.err.errorWithCode(span_start, .e300, "__string_make() expects 2 arguments"); return .invalid; }
                _ = try self.checkExpr(args[0]);
                _ = try self.checkExpr(args[1]);
                return TypeRegistry.STRING;
            }
            // Distinct type constructor: RawPtr(expr) — Go reference: type conversion T(expr)
            if (self.types.lookupByName(name)) |type_idx| {
                const t = self.types.get(type_idx);
                if (t == .distinct) {
                    if (arg_count != 1) { self.err.errorWithCode(span_start, .e300, "type conversion requires exactly 1 argument"); return .invalid; }
                    const arg_type = try self.checkExpr(args[0]);
                    // Go conversions.go:153 — allow conversion when underlying types are identical:
                    //   underlying → distinct, same distinct → distinct, cross-distinct with same underlying
                    const arg_underlying = self.types.resolveDistinct(arg_type);
                    if (!self.types.isAssignable(arg_underlying, t.distinct.underlying) and !self.types.equal(arg_type, type_idx)) {
                        self.err.errorWithCode(span_start, .e300, "cannot convert to distinct type");
                        return .invalid;
                    }
                    try self.expr_types.put(callee_idx, type_idx);
                    return type_idx;
                }
            }
            // Generic function instantiation: max(i64) where max is generic
            if (self.generics.generic_functions.get(name)) |gen_info| {
                return try self.instantiateGenericFunc(idx, gen_info, name);
            }

            // Swift SE-0316: global actor isolation check for direct function calls.
            // If the called function is @MainActor and the caller is in a different
            // (or no) global actor context, require await.
            // Reference: TypeCheckConcurrency.cpp:3954 tryMarkImplicitlyAsync
            if (self.global_actor_fns.get(name)) |callee_actor| {
                const is_same_actor = if (self.current_global_actor) |cga|
                    std.mem.eql(u8, cga, callee_actor)
                else
                    false;
                if (!is_same_actor and !self.in_await) {
                    self.err.errorWithCode(span_start, .e300,
                        "call to global actor-isolated function requires 'await'");
                }
            }
        }

        const callee_type = try self.checkExpr(callee_idx);
        var callee = self.types.get(callee_type);
        const method_info = self.resolveMethodCall(callee_idx);
        const is_method = method_info != null;

        // When a struct field shadows a method name, prefer the method in call position.
        // e.g. struct has field "keys: i64" AND impl has method "keys()" — m.keys() calls the method.
        if (callee != .func) {
            if (method_info) |mi| {
                callee = self.types.get(mi.func_type);
            } else {
                self.err.errorWithCode(span_start, .e300, "cannot call non-function");
                return .invalid;
            }
        }
        if (callee != .func) { self.err.errorWithCode(span_start, .e300, "cannot call non-function"); return .invalid; }
        const ft = callee.func;
        const is_instance_method = is_method and (method_info == null or !method_info.?.is_static);
        const expected_args = if (is_instance_method and ft.params.len > 0) ft.params.len - 1 else ft.params.len;
        if (arg_count != expected_args) { self.err.errorWithCode(span_start, .e300, "wrong number of arguments"); return .invalid; }
        const param_offset: usize = if (is_instance_method) 1 else 0;
        for (args, 0..) |arg_idx, i| {
            // Zig Sema pattern: set expected_type from parameter type for result location inference
            const saved_expected = self.expected_type;
            self.expected_type = ft.params[i + param_offset].type_idx;
            const arg_type = try self.checkExpr(arg_idx);
            self.expected_type = saved_expected;
            if (!self.types.isAssignable(arg_type, ft.params[i + param_offset].type_idx)) {
                // @safe coercion: Struct arg → *Struct param (safeWrapType wraps struct params)
                const param_tidx = ft.params[i + param_offset].type_idx;
                const pt = self.types.get(param_tidx);
                const is_safe_struct = self.safe_mode and pt == .pointer and
                    (self.types.get(pt.pointer.elem) == .struct_type or self.types.get(pt.pointer.elem) == .union_type) and
                    self.types.isAssignable(arg_type, pt.pointer.elem);
                // @safe reverse coercion: *Struct/Union arg → Struct/Union param (generic methods keep T as value)
                // When a generic method has param T=Struct (not wrapped because is_substituted),
                // but the arg is *Struct (auto-ref'd by @safe), allow the deref coercion.
                const at = self.types.get(arg_type);
                const is_safe_deref = self.safe_mode and at == .pointer and
                    (self.types.get(at.pointer.elem) == .struct_type or self.types.get(at.pointer.elem) == .union_type) and
                    self.types.isAssignable(at.pointer.elem, param_tidx);
                if (!is_safe_struct and !is_safe_deref) {
                    self.err.errorWithCode(span_start, .e300, "type mismatch");
                }
            }
        }

        // Swift SE-0430: mark sending parameter arguments as "sent".
        // Swift reference: RegionAnalysis.cpp PartitionOpKind::Send
        //   When a non-Sendable value is passed to a sending parameter,
        //   the value's region is marked as sent. Any subsequent use is
        //   a use-after-send error (PartitionOpKind::Require → LocalUseAfterSendError).
        // Cot Phase 1: track sent variable names in sent_variables map.
        for (args, 0..) |arg_idx, i| {
            const param_idx = i + param_offset;
            if (param_idx < ft.params.len and ft.params[param_idx].is_sending) {
                // Extract the argument's variable name (if it's a simple identifier)
                if (self.tree.nodeTag(arg_idx) == .ident) {
                    const arg_name = self.tree.tokenSlice(self.tree.nodeMainToken(arg_idx));
                    self.sent_variables.put(arg_name, {}) catch {};
                }
            }
        }

        // Swift actor isolation: cross-actor method calls require `await`.
        // Reference: Swift TypeCheckConcurrency.cpp:8253 forReference(),
        //            SILGenApply.cpp:6155-6240 (hop_to_executor emission).
        if (is_method and method_info != null and !method_info.?.is_static) {
            const mi_check = method_info.?;
            const receiver_name = self.getCallReceiverActorName(callee_idx);
            if (receiver_name) |rn| {
                if (self.actor_types.contains(rn)) {
                    const is_same_actor = if (self.current_actor_type) |cat| std.mem.eql(u8, cat, rn) else false;
                    // nonisolated methods can be called without await (Swift SE-0313)
                    if (!is_same_actor and !self.in_await and !mi_check.is_nonisolated) {
                        self.err.errorWithCode(span_start, .e300,
                            "cross-actor call requires 'await'");
                    }
                }
            }
        }

        return ft.return_type;
    }

    fn resolveMethodCall(self: *Checker, callee_idx: Index) ?types_mod.MethodInfo {
        if (self.tree.nodeTag(callee_idx) != .field_access) return null;
        const data = self.tree.nodeData(callee_idx);
        const base_node: Index = data.node_and_token[0];
        const field_tok = data.node_and_token[1];
        const field_name = self.tree.tokenSlice(field_tok);
        const base_type_idx = self.expr_types.get(base_node) orelse return null;
        const base_type = self.types.get(base_type_idx);
        const struct_name: []const u8 = switch (base_type) {
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
            // string is stored as slice(u8) — look up methods registered under "string"
            .slice => if (base_type_idx == TypeRegistry.STRING) "string" else return null,
            else => return null,
        };
        return self.lookupMethod(struct_name, field_name);
    }

    // ---------------------------------------------------------------
    // Stub declarations for functions that will be ported in subsequent chunks.
    // These are referenced by the functions above and must exist for compilation.
    // ---------------------------------------------------------------
    fn buildFuncType(self: *Checker, params: SubRange, return_type: OptionalIndex) CheckError!TypeIndex {
        _ = self;
        _ = params;
        _ = return_type;
        @panic("buildFuncType not yet ported");
    }
    fn buildEnumType(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("buildEnumType not yet ported");
    }
    fn buildUnionType(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("buildUnionType not yet ported");
    }
    fn buildStructTypeWithLayout(self: *Checker, name: []const u8, fields: SubRange, layout: ast.StructLayout) CheckError!TypeIndex {
        _ = self;
        _ = name;
        _ = fields;
        _ = layout;
        @panic("buildStructTypeWithLayout not yet ported");
    }
    fn resolveTypeExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("resolveTypeExpr not yet ported");
    }
    fn safeWrapType(self: *Checker, type_idx: TypeIndex) CheckError!TypeIndex {
        _ = self;
        _ = type_idx;
        @panic("safeWrapType not yet ported");
    }
    fn isSendable(self: *Checker, type_idx: TypeIndex) bool {
        _ = self;
        _ = type_idx;
        @panic("isSendable not yet ported");
    }
    fn checkStmt(self: *Checker, idx: Index) CheckError!void {
        _ = self;
        _ = idx;
        @panic("checkStmt not yet ported");
    }
    fn checkBlockExpr(self: *Checker, idx: Index) CheckError!void {
        _ = self;
        _ = idx;
        @panic("checkBlockExpr not yet ported");
    }
    fn materializeType(self: *Checker, type_idx: TypeIndex) TypeIndex {
        _ = self;
        _ = type_idx;
        @panic("materializeType not yet ported");
    }
    fn errWithSuggestion(self: *Checker, pos: Pos, msg: []const u8, suggestion: ?[]const u8) void {
        _ = suggestion;
        self.err.errorWithCode(pos, .e300, msg);
    }
    fn findSimilarName(self: *Checker, name: []const u8) ?[]const u8 {
        _ = self;
        _ = name;
        return null; // Will be ported in a later chunk
    }
    fn findSimilarTrait(self: *Checker, name: []const u8) ?[]const u8 {
        _ = self;
        _ = name;
        return null; // Will be ported in a later chunk
    }
    fn checkIndex(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkIndex not yet ported");
    }
    fn checkSliceExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkSliceExpr not yet ported");
    }
    fn checkFieldAccess(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkFieldAccess not yet ported");
    }
    fn checkArrayLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkArrayLiteral not yet ported");
    }
    fn checkIfExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkIfExpr not yet ported");
    }
    fn checkSwitchExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkSwitchExpr not yet ported");
    }
    fn checkBlock(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkBlock not yet ported");
    }
    fn checkStructInit(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkStructInit not yet ported");
    }
    fn checkNewExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkNewExpr not yet ported");
    }
    fn checkBuiltinCall(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkBuiltinCall not yet ported");
    }
    fn checkStringInterp(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkStringInterp not yet ported");
    }
    fn checkTryExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkTryExpr not yet ported");
    }
    fn checkAwaitExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkAwaitExpr not yet ported");
    }
    fn checkTaskExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkTaskExpr not yet ported");
    }
    fn checkCatchExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkCatchExpr not yet ported");
    }
    fn checkOrElseExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkOrElseExpr not yet ported");
    }
    fn checkErrorLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkErrorLiteral not yet ported");
    }
    fn checkAddrOf(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkAddrOf not yet ported");
    }
    fn checkDeref(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkDeref not yet ported");
    }
    fn checkTupleLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkTupleLiteral not yet ported");
    }
    fn checkBuiltinLen(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkBuiltinLen not yet ported");
    }
    fn checkBuiltinAppend(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkBuiltinAppend not yet ported");
    }
    fn instantiateGenericFunc(self: *Checker, idx: Index, gen_info: GenericInfo, name: []const u8) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        _ = gen_info;
        _ = name;
        @panic("instantiateGenericFunc not yet ported");
    }
    fn isComparable(self: *Checker, a: TypeIndex, b: TypeIndex) bool {
        _ = self;
        _ = a;
        _ = b;
        @panic("isComparable not yet ported");
    }
    fn isUnionVariantRef(self: *Checker, node: Index, ut: types_mod.UnionType) bool {
        _ = self;
        _ = node;
        _ = ut;
        @panic("isUnionVariantRef not yet ported");
    }
    fn getCallReceiverActorName(self: *Checker, callee: Index) ?[]const u8 {
        _ = self;
        _ = callee;
        @panic("getCallReceiverActorName not yet ported");
    }
};

test "Symbol init" {
    const sym = Symbol.init("x", .variable, TypeRegistry.I64, @enumFromInt(0), true);
    try std.testing.expectEqualStrings("x", sym.name);
    try std.testing.expectEqual(SymbolKind.variable, sym.kind);
    try std.testing.expect(sym.mutable);
}

test "Scope define and lookup" {
    var scope = Scope.init(std.testing.allocator, null);
    defer scope.deinit();
    try scope.define(Symbol.init("x", .variable, TypeRegistry.I64, @enumFromInt(0), true));
    try std.testing.expect(scope.lookup("x") != null);
    try std.testing.expect(scope.lookup("y") == null);
}

test "Scope parent chain lookup" {
    var parent = Scope.init(std.testing.allocator, null);
    defer parent.deinit();
    try parent.define(Symbol.init("x", .variable, TypeRegistry.I64, @enumFromInt(0), true));

    var child = Scope.init(std.testing.allocator, &parent);
    defer child.deinit();
    try child.define(Symbol.init("y", .variable, TypeRegistry.I32, @enumFromInt(0), false));

    try std.testing.expect(child.lookup("x") != null);
    try std.testing.expect(child.lookup("y") != null);
    try std.testing.expect(child.lookup("z") == null);
}

test "Scope markUsed" {
    var scope = Scope.init(std.testing.allocator, null);
    defer scope.deinit();
    try scope.define(Symbol.init("x", .variable, TypeRegistry.I64, @enumFromInt(0), true));
    try std.testing.expect(!scope.lookup("x").?.used);
    scope.markUsed("x");
    try std.testing.expect(scope.lookup("x").?.used);
}

test "parseMapTypeArgs" {
    const args = parseMapTypeArgs("Map(5;17)").?;
    try std.testing.expectEqual(@as(u32, 5), args[0].toInt());
    try std.testing.expectEqual(@as(u32, 17), args[1].toInt());
    try std.testing.expect(parseMapTypeArgs("NotAMap") == null);
}

test "SharedGenericContext init and deinit" {
    var ctx = SharedGenericContext.init(std.testing.allocator);
    defer ctx.deinit(std.testing.allocator);
    try std.testing.expect(ctx.generic_structs.count() == 0);
}
