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

    // Stub declarations for functions that will be ported in subsequent chunks.
    // These are referenced by the functions above and must exist for compilation.
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
    fn checkExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        _ = self;
        _ = idx;
        @panic("checkExpr not yet ported");
    }
    fn materializeType(self: *Checker, type_idx: TypeIndex) TypeIndex {
        _ = self;
        _ = type_idx;
        @panic("materializeType not yet ported");
    }
    fn evalConstExpr(self: *Checker, idx: Index) ?i64 {
        _ = self;
        _ = idx;
        @panic("evalConstExpr not yet ported");
    }
    fn evalConstFloat(self: *Checker, idx: Index) ?f64 {
        _ = self;
        _ = idx;
        @panic("evalConstFloat not yet ported");
    }
    fn isFloatType(self: *Checker, type_idx: TypeIndex) bool {
        _ = self;
        _ = type_idx;
        @panic("isFloatType not yet ported");
    }
    fn errWithSuggestion(self: *Checker, pos: Pos, msg: []const u8, suggestion: ?[]const u8) void {
        _ = self;
        _ = pos;
        _ = msg;
        _ = suggestion;
        @panic("errWithSuggestion not yet ported");
    }
    fn findSimilarTrait(self: *Checker, name: []const u8) ?[]const u8 {
        _ = self;
        _ = name;
        @panic("findSimilarTrait not yet ported");
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
