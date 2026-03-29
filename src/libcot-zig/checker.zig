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
const types_mod = @import("foundation").types;
const errors = @import("foundation").errors;
const source = @import("foundation").source;
const tok = @import("foundation").token;
const comptime_mod = @import("foundation").comptime_val;
const target_mod = @import("foundation").target;
const debug = @import("foundation").debug;

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
    // Builtin checking (ported from compiler/ lines 2325-2773)
    // ---------------------------------------------------------------

    fn checkBuiltinLen(self: *Checker, call_idx: Index) CheckError!TypeIndex {
        const args = self.getCallArgNodes(call_idx);
        const span_start = self.tree.nodeSpan(call_idx).start;
        if (args.len != 1) { self.err.errorWithCode(span_start, .e300, "len() expects one argument"); return .invalid; }
        const arg_type = try self.checkExpr(args[0]);
        if (arg_type == TypeRegistry.STRING) return TypeRegistry.INT;
        const arg = self.types.get(arg_type);
        return switch (arg) { .array, .slice, .list => TypeRegistry.INT, else => blk: { self.err.errorWithCode(span_start, .e300, "len() argument must be string, array, slice, or list"); break :blk .invalid; } };
    }

    fn checkBuiltinAppend(self: *Checker, call_idx: Index) CheckError!TypeIndex {
        const args = self.getCallArgNodes(call_idx);
        const span_start = self.tree.nodeSpan(call_idx).start;
        if (args.len != 2) { self.err.errorWithCode(span_start, .e300, "append() expects two arguments"); return .invalid; }
        const slice_type = try self.checkExpr(args[0]);
        const elem_type = try self.checkExpr(args[1]);
        const slice = self.types.get(slice_type);
        const expected_elem = switch (slice) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => { self.err.errorWithCode(span_start, .e300, "append() first argument must be array or slice"); return .invalid; },
        };
        if (!self.types.isAssignable(elem_type, expected_elem))
            self.err.errorWithCode(span_start, .e300, "append() element type mismatch");
        return self.types.makeSlice(expected_elem) catch .invalid;
    }

    fn checkBuiltinCall(self: *Checker, idx: Index) CheckError!TypeIndex {
        const bc = self.tree.builtinCallData(idx);
        const span_start = self.tree.nodeSpan(idx).start;
        switch (bc.kind) {
            .size_of, .align_of => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "requires valid type"); return .invalid; }
                return TypeRegistry.I64;
            },
            .enum_len => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "@enumLen requires valid type"); return .invalid; }
                const info = self.types.get(type_idx);
                if (info != .enum_type) { self.err.errorWithCode(span_start, .e300, "@enumLen requires enum type"); return .invalid; }
                return TypeRegistry.I64;
            },
            .string => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                return TypeRegistry.STRING;
            },
            .int_cast => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                if (!types_mod.isInteger(self.types.get(target_type))) { self.err.errorWithCode(span_start, .e300, "@intCast target must be integer type"); return .invalid; }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .float_cast => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                const target_info = self.types.get(target_type);
                if (target_info != .basic or !target_info.basic.isFloat()) {
                    self.err.errorWithCode(span_start, .e300, "@floatCast target must be float type");
                    return .invalid;
                }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .float_from_int => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .int_from_float => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                const info = self.types.get(arg_type);
                if (info != .basic or !info.basic.isFloat()) {
                    self.err.errorWithCode(span_start, .e300, "@intFromFloat operand must be a float type");
                }
                return TypeRegistry.I64;
            },
            .ptr_cast => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@ptrCast target must be pointer"); return .invalid; }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .ptr_to_int => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                const arg_info = self.types.get(arg_type);
                if (arg_info != .pointer and arg_info != .func and arg_type != TypeRegistry.I64) {
                    self.err.errorWithCode(span_start, .e300, "@ptrToInt operand must be a pointer");
                    return .invalid;
                }
                return TypeRegistry.I64;
            },
            .int_to_ptr => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                if (self.types.get(target_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@intToPtr target must be pointer"); return .invalid; }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                const elem = self.types.get(target_type).pointer.elem;
                return self.types.makeRawPointer(elem) catch .invalid;
            },
            .assert => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.VOID;
            },
            .assert_eq => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                return TypeRegistry.VOID;
            },
            .ptr_of => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(span_start, .e300, "@ptrOf requires string argument"); return .invalid; }
                return TypeRegistry.I64;
            },
            .len_of => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (arg_type != TypeRegistry.STRING) { self.err.errorWithCode(span_start, .e300, "@lenOf requires string argument"); return .invalid; }
                return TypeRegistry.I64;
            },
            .trap => return TypeRegistry.NORETURN,
            .target_os, .target_arch, .target => return TypeRegistry.STRING,
            .compile_error => {
                const arg0 = bc.arg0.unwrap() orelse {
                    self.err.errorWithCode(span_start, .e300, "compile error");
                    return TypeRegistry.NORETURN;
                };
                const msg = self.evalConstString(arg0) orelse "compile error";
                self.err.errorWithCode(span_start, .e300, msg);
                return TypeRegistry.NORETURN;
            },
            .embed_file => {
                if (bc.arg0.unwrap()) |a0| {
                    const arg_tag = self.tree.nodeTag(a0);
                    if (arg_tag != .literal_string) {
                        self.err.errorWithCode(span_start, .e300, "@embedFile requires a string literal path");
                    }
                }
                return TypeRegistry.STRING;
            },
            .abs, .ceil, .floor, .trunc, .round, .sqrt => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (arg_type != TypeRegistry.F64 and arg_type != TypeRegistry.F32 and arg_type != TypeRegistry.UNTYPED_FLOAT) {
                    self.err.errorWithCode(span_start, .e300, "math builtin requires float argument");
                    return .invalid;
                }
                return TypeRegistry.F64;
            },
            .fmin, .fmax => {
                const a0 = bc.arg0.unwrap() orelse return .invalid;
                const a1 = bc.arg1.unwrap() orelse return .invalid;
                const arg1_type = try self.checkExpr(a0);
                const arg2_type = try self.checkExpr(a1);
                const a1_float = arg1_type == TypeRegistry.F64 or arg1_type == TypeRegistry.F32 or arg1_type == TypeRegistry.UNTYPED_FLOAT;
                const a2_float = arg2_type == TypeRegistry.F64 or arg2_type == TypeRegistry.F32 or arg2_type == TypeRegistry.UNTYPED_FLOAT;
                if (!a1_float or !a2_float) {
                    self.err.errorWithCode(span_start, .e300, "@fmin/@fmax require float arguments");
                    return .invalid;
                }
                return TypeRegistry.F64;
            },
            .has_field => {
                const type_arg = bc.type_arg.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@hasField requires valid type"); return .invalid; };
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "@hasField requires valid type"); return .invalid; }
                const arg0 = bc.arg0.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@hasField requires string literal field name"); return .invalid; };
                _ = self.evalConstString(arg0) orelse {
                    self.err.errorWithCode(span_start, .e300, "@hasField requires string literal field name");
                    return .invalid;
                };
                return TypeRegistry.BOOL;
            },
            .type_of => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                return try self.checkExpr(arg0);
            },
            .field => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg1 = bc.arg1.unwrap() orelse return .invalid;
                var base_type = try self.checkExpr(arg0);
                while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
                const name_str = self.evalConstString(arg1) orelse {
                    self.err.errorWithCode(span_start, .e300, "@field requires string literal field name");
                    return .invalid;
                };
                const info = self.types.get(base_type);
                if (info == .struct_type) {
                    for (info.struct_type.fields) |sf| {
                        if (std.mem.eql(u8, sf.name, name_str)) return sf.type_idx;
                    }
                    if (findSimilarField(name_str, info.struct_type.fields)) |suggestion| {
                        const fmsg = std.fmt.allocPrint(self.allocator, "struct has no field with this name; did you mean '{s}'?", .{suggestion}) catch "struct has no field with this name";
                        self.err.errorWithCode(span_start, .e300, fmsg);
                    } else {
                        self.err.errorWithCode(span_start, .e300, "struct has no field with this name");
                    }
                    return .invalid;
                }
                self.err.errorWithCode(span_start, .e300, "@field requires struct type");
                return .invalid;
            },
            .int_from_enum => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .enum_type) {
                    self.err.errorWithCode(span_start, .e300, "@intFromEnum requires enum argument");
                    return .invalid;
                }
                return TypeRegistry.I64;
            },
            .enum_from_int => {
                const target_type = blk: {
                    if (bc.type_arg.unwrap()) |ta| break :blk try self.resolveTypeExpr(ta);
                    if (self.expected_type != .invalid) break :blk self.expected_type;
                    self.err.errorWithCode(span_start, .e300, "@enumFromInt requires type argument or type context (use @as(T, @enumFromInt(val)))");
                    return .invalid;
                };
                const target_info = self.types.get(target_type);
                if (target_info != .enum_type) {
                    self.err.errorWithCode(span_start, .e300, "@enumFromInt target must be enum type");
                    return .invalid;
                }
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                _ = try self.checkExpr(arg0);
                if (self.evalConstExpr(arg0)) |val| {
                    const num_variants: i64 = @intCast(target_info.enum_type.variants.len);
                    if (val < 0 or val >= num_variants) {
                        self.err.errorWithCode(span_start, .e300, "@enumFromInt value out of range for enum type");
                        return .invalid;
                    }
                }
                return target_type;
            },
            .tag_name => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                const info = self.types.get(arg_type);
                if (info != .enum_type and info != .union_type) {
                    self.err.errorWithCode(span_start, .e300, "@tagName requires enum or union argument");
                    return .invalid;
                }
                return TypeRegistry.STRING;
            },
            .error_name => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                const arg_info = self.types.get(arg_type);
                if (arg_info != .error_set and arg_info != .error_union) {
                    self.err.errorWithCode(span_start, .e300, "@errorName operand must be error type");
                    return .invalid;
                }
                return TypeRegistry.STRING;
            },
            .int_from_bool => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (arg_type != TypeRegistry.BOOL and arg_type != TypeRegistry.UNTYPED_BOOL) {
                    self.err.errorWithCode(span_start, .e300, "@intFromBool requires bool argument");
                    return .invalid;
                }
                return TypeRegistry.I64;
            },
            .bit_cast => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                const arg_type = try self.checkExpr(arg0);
                const target_info = self.types.get(target_type);
                const arg_info = self.types.get(arg_type);
                if (target_info == .pointer or target_info == .enum_type) {
                    self.err.errorWithCode(span_start, .e300, "@bitCast target cannot be pointer or enum type");
                    return .invalid;
                }
                if (arg_info == .pointer or arg_info == .enum_type) {
                    self.err.errorWithCode(span_start, .e300, "@bitCast source cannot be pointer or enum type");
                    return .invalid;
                }
                const target_size = self.types.sizeOf(target_type);
                const arg_size = self.types.sizeOf(arg_type);
                if (target_size != arg_size) {
                    self.err.errorWithCode(span_start, .e300, "@bitCast requires same-size types");
                    return .invalid;
                }
                return target_type;
            },
            .truncate => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                if (!types_mod.isInteger(self.types.get(target_type))) {
                    self.err.errorWithCode(span_start, .e300, "@truncate target must be integer type");
                    return .invalid;
                }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .as => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const target_type = try self.resolveTypeExpr(type_arg);
                const saved_expected = self.expected_type;
                self.expected_type = target_type;
                defer self.expected_type = saved_expected;
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return target_type;
            },
            .offset_of => {
                const type_arg = bc.type_arg.unwrap() orelse return .invalid;
                const type_idx = try self.resolveTypeExpr(type_arg);
                const info = self.types.get(type_idx);
                if (info != .struct_type) {
                    self.err.errorWithCode(span_start, .e300, "@offsetOf requires struct type");
                    return .invalid;
                }
                const arg0 = bc.arg0.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@offsetOf requires string literal field name"); return .invalid; };
                const name_str = self.evalConstString(arg0) orelse {
                    self.err.errorWithCode(span_start, .e300, "@offsetOf requires string literal field name");
                    return .invalid;
                };
                var found = false;
                for (info.struct_type.fields) |sf| {
                    if (std.mem.eql(u8, sf.name, name_str)) { found = true; break; }
                }
                if (!found) {
                    self.err.errorWithCode(span_start, .e300, "struct has no field with this name");
                    return .invalid;
                }
                return TypeRegistry.I64;
            },
            .min, .max => {
                const a0 = bc.arg0.unwrap() orelse return .invalid;
                const a1 = bc.arg1.unwrap() orelse return .invalid;
                const a_type = try self.checkExpr(a0);
                const b_type = try self.checkExpr(a1);
                if (!types_mod.isNumeric(self.types.get(a_type))) {
                    self.err.errorWithCode(span_start, .e300, "@min/@max requires numeric types");
                    return .invalid;
                }
                if (!types_mod.isNumeric(self.types.get(b_type))) {
                    self.err.errorWithCode(span_start, .e300, "@min/@max requires numeric types");
                    return .invalid;
                }
                return TypeRegistry.commonType(a_type, b_type);
            },
            .align_cast => {
                if (bc.type_arg.unwrap()) |ta| _ = try self.resolveTypeExpr(ta);
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.I64;
            },
            .const_cast => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                return try self.checkExpr(arg0);
            },
            .arc_retain, .arc_release => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.VOID;
            },
            .is_unique => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.BOOL;
            },
            .panic => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.NORETURN;
            },
            .ctz, .clz, .pop_count => {
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.I64;
            },
            .type_name => {
                const type_arg = bc.type_arg.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@typeName requires valid type"); return .invalid; };
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "@typeName requires valid type"); return .invalid; }
                return TypeRegistry.STRING;
            },
            .enum_name => {
                const type_arg = bc.type_arg.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@enumName requires valid type"); return .invalid; };
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "@enumName requires valid type"); return .invalid; }
                const info = self.types.get(type_idx);
                if (info != .enum_type) { self.err.errorWithCode(span_start, .e300, "@enumName requires enum type"); return .invalid; }
                if (bc.arg0.unwrap()) |a0| _ = try self.checkExpr(a0);
                return TypeRegistry.STRING;
            },
            .type_info => {
                const type_arg = bc.type_arg.unwrap() orelse { self.err.errorWithCode(span_start, .e300, "@typeInfo requires valid type"); return .invalid; };
                const type_idx = try self.resolveTypeExpr(type_arg);
                if (type_idx == .invalid) { self.err.errorWithCode(span_start, .e300, "@typeInfo requires valid type"); return .invalid; }
                return TypeRegistry.I64;
            },
            .atomic_load => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@atomicLoad requires pointer argument"); return .invalid; }
                return TypeRegistry.I64;
            },
            .atomic_store => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@atomicStore requires pointer argument"); return .invalid; }
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                return TypeRegistry.VOID;
            },
            .atomic_add => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@atomicAdd requires pointer argument"); return .invalid; }
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                return TypeRegistry.I64;
            },
            .atomic_cas => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@atomicCAS requires pointer argument"); return .invalid; }
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                if (bc.arg2.unwrap()) |a2| _ = try self.checkExpr(a2);
                return TypeRegistry.I64;
            },
            .atomic_exchange => {
                const arg0 = bc.arg0.unwrap() orelse return .invalid;
                const arg_type = try self.checkExpr(arg0);
                if (self.types.get(arg_type) != .pointer) { self.err.errorWithCode(span_start, .e300, "@atomicExchange requires pointer argument"); return .invalid; }
                if (bc.arg1.unwrap()) |a1| _ = try self.checkExpr(a1);
                return TypeRegistry.I64;
            },
        }
    }

    // ---------------------------------------------------------------
    // Complex expressions (ported from compiler/ lines 2774-3706)
    // ---------------------------------------------------------------

    fn checkIndex(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx).node_and_node;
        const base_idx = data[0];
        const index_idx = data[1];
        // Check if base is a comptime array
        if (self.evalComptimeValue(base_idx)) |base_cv| {
            if (base_cv == .array) {
                if (base_cv.array.elements.items.len > 0) {
                    const first = base_cv.array.elements.items[0];
                    return switch (first) {
                        .int => TypeRegistry.I64,
                        .string => TypeRegistry.STRING,
                        .boolean => TypeRegistry.BOOL,
                        .enum_field => TypeRegistry.I64,
                        else => TypeRegistry.I64,
                    };
                }
                return TypeRegistry.I64;
            }
        }
        var base_type = try self.checkExpr(base_idx);
        const index_type = try self.checkExpr(index_idx);
        if (!types_mod.isInteger(self.types.get(index_type))) { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "index must be integer"); return .invalid; }
        while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
        if (base_type == TypeRegistry.STRING) return TypeRegistry.U8;
        const base = self.types.get(base_type);
        return switch (base) { .array => |a| a.elem, .slice => |s| s.elem, .list => |l| l.elem, else => blk: { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "cannot index this type"); break :blk .invalid; } };
    }

    fn checkSliceExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const base_idx: Index = data.node_and_extra[0];
        const extra = self.tree.extraData(data.node_and_extra[1], ast.SliceData);
        var base_type = try self.checkExpr(base_idx);
        if (extra.start.unwrap()) |s| _ = try self.checkExpr(s);
        if (extra.end.unwrap()) |e| _ = try self.checkExpr(e);
        while (self.types.get(base_type) == .pointer) base_type = self.types.get(base_type).pointer.elem;
        const base = self.types.get(base_type);
        if (base_type == TypeRegistry.STRING) return TypeRegistry.STRING;
        return switch (base) { .array => |a| self.types.makeSlice(a.elem), .slice => base_type, else => blk: { self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "cannot slice this type"); break :blk .invalid; } };
    }

    fn checkFieldAccess(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const base_node: Index = data.node_and_token[0];
        const field_tok = data.node_and_token[1];
        const field_name = self.tree.tokenSlice(field_tok);
        const span_start = self.tree.nodeSpan(idx).start;

        // Check if base is a comptime value
        if (self.evalComptimeValue(base_node)) |base_cv| {
            switch (base_cv) {
                .type_info => {
                    if (std.mem.eql(u8, field_name, "fields")) return TypeRegistry.I64;
                    if (std.mem.eql(u8, field_name, "name")) return TypeRegistry.STRING;
                    self.err.errorWithCode(span_start, .e300, "no such field on type info");
                    return .invalid;
                },
                .enum_field => {
                    if (std.mem.eql(u8, field_name, "name")) return TypeRegistry.STRING;
                    if (std.mem.eql(u8, field_name, "value")) return TypeRegistry.I64;
                    self.err.errorWithCode(span_start, .e300, "no such field on enum field");
                    return .invalid;
                },
                .array => {
                    if (std.mem.eql(u8, field_name, "len")) return TypeRegistry.I64;
                    self.err.errorWithCode(span_start, .e300, "no such field on array");
                    return .invalid;
                },
                else => {},
            }
        }

        // Check for shorthand .variant in enum/union context
        const base_tag = self.tree.nodeTag(base_node);
        _ = base_tag;
        // If base is "none" (shorthand), resolve from context
        // In the compact AST, field_access with no base = shorthand .variant.
        // The base node might be a sentinel/none. Check by examining if it's valid.
        // Actually in compact AST, field_access always has base + field_token.
        // Shorthand .variant uses a different representation or has a dummy base.

        // Check if this is enum shorthand: .variant in switch/comparison context
        // In the compact AST, shorthand .field has base_node that is an ident for empty string
        // or the base is simply the expression being accessed.

        // Nested type namespace: TypeName.NestedType
        if (self.tree.nodeTag(base_node) == .ident) {
            const base_name = self.tree.tokenSlice(self.tree.nodeMainToken(base_node));
            var buf: [512]u8 = undefined;
            const qualified = std.fmt.bufPrint(&buf, "{s}_{s}", .{ base_name, field_name }) catch "";
            if (qualified.len > 0) {
                if (self.resolveTypeByName(qualified)) |nested_type_idx| {
                    return nested_type_idx;
                }
                if (self.scope.lookup(qualified)) |sym| {
                    if (sym.kind == .constant) {
                        if (sym.type_idx != .invalid) return sym.type_idx;
                    }
                    if (sym.kind == .function and sym.type_idx != .invalid) {
                        if (self.lookupMethod(base_name, field_name)) |m| {
                            if (m.is_static) {
                                if (self.resolveTypeByName(base_name)) |base_type_idx| {
                                    try self.expr_types.put(base_node, base_type_idx);
                                }
                                return sym.type_idx;
                            }
                        }
                    }
                }
            }
        }

        var base_type = try self.checkExpr(base_node);

        // Auto-deref
        while (true) {
            switch (self.types.get(base_type)) {
                .pointer => |ptr| base_type = ptr.elem,
                else => break,
            }
        }

        const base = self.types.get(base_type);

        switch (base) {
            .struct_type => |st| {
                for (st.fields) |fld| if (std.mem.eql(u8, fld.name, field_name)) return fld.type_idx;
                if (self.lookupMethod(st.name, field_name)) |m| return m.func_type;
                self.errWithSuggestion(span_start, "undefined field", findSimilarField(field_name, st.fields));
                return .invalid;
            },
            .enum_type => |et| {
                for (et.variants) |v| if (std.mem.eql(u8, v.name, field_name)) return base_type;
                if (self.lookupMethod(et.name, field_name)) |m| return m.func_type;
                self.errWithSuggestion(span_start, "undefined variant", findSimilarVariant(field_name, et.variants));
                return .invalid;
            },
            .union_type => |ut| {
                if (std.mem.eql(u8, field_name, "tag")) return TypeRegistry.I64;
                // Check if base is a type name (constructor) vs value (extraction)
                const is_type_access = blk: {
                    if (self.tree.nodeTag(base_node) == .ident) {
                        const bname = self.tree.tokenSlice(self.tree.nodeMainToken(base_node));
                        if (self.scope.lookup(bname)) |sym| {
                            break :blk sym.kind == .type_name;
                        }
                    }
                    break :blk false;
                };
                for (ut.variants) |v| if (std.mem.eql(u8, v.name, field_name)) {
                    if (v.payload_type == .invalid) return base_type;
                    if (!is_type_access) return v.payload_type;
                    // Constructor: Result.Ok returns function type
                    const params = try self.allocator.alloc(types_mod.FuncParam, 1);
                    params[0] = .{ .name = "payload", .type_idx = v.payload_type };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = base_type } });
                };
                self.errWithSuggestion(span_start, "undefined variant", findSimilarVariant(field_name, ut.variants));
                return .invalid;
            },
            .map => |mt| {
                if (std.mem.eql(u8, field_name, "set")) {
                    const params = try self.allocator.alloc(types_mod.FuncParam, 2);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    params[1] = .{ .name = "value", .type_idx = mt.value };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.VOID } });
                } else if (std.mem.eql(u8, field_name, "get")) {
                    const params = try self.allocator.alloc(types_mod.FuncParam, 1);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = mt.value } });
                } else if (std.mem.eql(u8, field_name, "has")) {
                    const params = try self.allocator.alloc(types_mod.FuncParam, 1);
                    params[0] = .{ .name = "key", .type_idx = mt.key };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.BOOL } });
                }
                self.errWithSuggestion(span_start, "undefined field", editDistSuggest(field_name, &.{ "set", "get", "has" }));
                return .invalid;
            },
            .list => |lt| {
                if (std.mem.eql(u8, field_name, "push")) {
                    const params = try self.allocator.alloc(types_mod.FuncParam, 1);
                    params[0] = .{ .name = "value", .type_idx = lt.elem };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = TypeRegistry.VOID } });
                } else if (std.mem.eql(u8, field_name, "get")) {
                    const params = try self.allocator.alloc(types_mod.FuncParam, 1);
                    params[0] = .{ .name = "index", .type_idx = TypeRegistry.INT };
                    return try self.types.add(.{ .func = .{ .params = params, .return_type = lt.elem } });
                } else if (std.mem.eql(u8, field_name, "len")) {
                    return try self.types.add(.{ .func = .{ .params = &.{}, .return_type = TypeRegistry.INT } });
                }
                self.errWithSuggestion(span_start, "undefined field", editDistSuggest(field_name, &.{ "push", "get", "len" }));
                return .invalid;
            },
            .slice => |sl| {
                if (std.mem.eql(u8, field_name, "ptr")) return try self.types.add(.{ .pointer = .{ .elem = sl.elem } })
                else if (std.mem.eql(u8, field_name, "len")) return TypeRegistry.I64;
                if (base_type == TypeRegistry.STRING) {
                    if (self.lookupMethod("string", field_name)) |m| return m.func_type;
                }
                self.errWithSuggestion(span_start, "undefined field", editDistSuggest(field_name, &.{ "ptr", "len" }));
                return .invalid;
            },
            .tuple => |tup| {
                const tup_idx = std.fmt.parseInt(u32, field_name, 10) catch {
                    self.err.errorWithCode(span_start, .e300, "tuple fields must be numeric (e.g. .0, .1)");
                    return .invalid;
                };
                if (tup_idx >= tup.element_types.len) {
                    self.err.errorWithCode(span_start, .e300, "tuple index out of bounds");
                    return .invalid;
                }
                return tup.element_types[tup_idx];
            },
            .basic => |bk| {
                if (self.lookupMethod(bk.name(), field_name)) |m| return m.func_type;
                self.err.errorWithCode(span_start, .e300, "cannot access field on this type");
                return .invalid;
            },
            .existential => |exist| {
                for (exist.method_names) |mn| {
                    if (std.mem.eql(u8, mn, field_name)) {
                        if (exist.conforming_types.len > 0) {
                            if (self.lookupMethod(exist.conforming_types[0], field_name)) |m| {
                                const ft = self.types.get(m.func_type);
                                if (ft == .func and ft.func.params.len > 0) {
                                    const new_params = ft.func.params[1..];
                                    return try self.types.add(.{ .func = .{ .params = new_params, .return_type = ft.func.return_type } });
                                }
                                return m.func_type;
                            }
                        }
                        return TypeRegistry.VOID;
                    }
                }
                self.err.errorWithCode(span_start, .e300, "trait has no such method");
                return .invalid;
            },
            else => {
                self.err.errorWithCode(span_start, .e300, "cannot access field on this type");
                return .invalid;
            },
        }
    }

    fn checkStructInit(self: *Checker, idx: Index) CheckError!TypeIndex {
        const tag = self.tree.nodeTag(idx);
        const span_start = self.tree.nodeSpan(idx).start;
        // Extract type name, type args, and field info from compact AST
        var type_name: []const u8 = "";
        var field_pairs: []const u32 = &.{};
        var type_args_range = SubRange.empty;
        if (tag == .struct_init_one) {
            const data = self.tree.nodeData(idx);
            const si1 = self.tree.extraData(data.node_and_extra[1], ast.StructInitOne);
            if (si1.type_name_token.unwrap()) |tn_tok| type_name = self.tree.tokenSlice(tn_tok);
            // Single field: name_token + value_node
            const fname = self.tree.tokenSlice(si1.field_name_token);
            _ = fname;
            field_pairs = &.{}; // handled specially below
            // For struct_init_one, check the single field inline
            const struct_type_idx = if (type_name.len == 0) blk: {
                if (self.expected_type != .invalid and self.types.get(self.expected_type) == .struct_type) break :blk self.expected_type;
                self.err.errorWithCode(span_start, .e300, "cannot infer type for anonymous struct literal");
                return .invalid;
            } else self.resolveTypeByName(type_name) orelse {
                self.errWithSuggestion(span_start, "undefined type", self.findSimilarType(type_name));
                return .invalid;
            };
            const struct_type = self.types.get(struct_type_idx);
            if (struct_type != .struct_type) { self.err.errorWithCode(span_start, .e300, "not a struct type"); return .invalid; }
            // Check single field
            const fi_name = self.tree.tokenSlice(si1.field_name_token);
            var found = false;
            for (struct_type.struct_type.fields) |sf| if (std.mem.eql(u8, sf.name, fi_name)) {
                found = true;
                const saved_expected = self.expected_type;
                self.expected_type = sf.type_idx;
                const vt = try self.checkExpr(si1.field_value);
                self.expected_type = saved_expected;
                if (!self.types.isAssignable(vt, sf.type_idx)) {
                    const vt_t = self.types.get(vt);
                    const is_safe_deref = self.safe_mode and vt_t == .pointer and
                        self.types.isAssignable(vt_t.pointer.elem, sf.type_idx);
                    const sf_t = self.types.get(sf.type_idx);
                    const is_safe_ref = self.safe_mode and sf_t == .pointer and
                        self.types.isAssignable(vt, sf_t.pointer.elem);
                    if (!is_safe_deref and !is_safe_ref) self.err.errorWithCode(span_start, .e300, "type mismatch in field");
                }
                break;
            };
            if (!found) self.errWithSuggestion(span_start, "unknown field", findSimilarField(fi_name, struct_type.struct_type.fields));
            // Check required fields
            for (struct_type.struct_type.fields) |sf| {
                if (sf.default_value != .none) continue;
                if (!std.mem.eql(u8, sf.name, fi_name)) self.err.errorWithCode(span_start, .e300, "missing field in struct init");
            }
            if (self.actor_types.contains(struct_type.struct_type.name)) return try self.types.makePointer(struct_type_idx);
            return struct_type_idx;
        } else {
            // struct_init: multiple fields
            const data = self.tree.nodeData(idx);
            const si = self.tree.extraData(data.node_and_extra[1], ast.StructInit);
            if (si.type_name_token.unwrap()) |tn_tok| type_name = self.tree.tokenSlice(tn_tok);
            type_args_range = si.type_args;
            field_pairs = self.tree.extraSlice(si.fields);
        }
        const struct_type_idx = if (type_name.len == 0) blk: {
            if (self.expected_type != .invalid and self.types.get(self.expected_type) == .struct_type) break :blk self.expected_type;
            self.err.errorWithCode(span_start, .e300, "cannot infer type for anonymous struct literal");
            return .invalid;
        } else if (type_args_range.len() > 0)
            try self.resolveGenericInstance_byName(type_name, type_args_range, self.tree.nodeSpan(idx))
        else
            self.resolveTypeByName(type_name) orelse {
                self.errWithSuggestion(span_start, "undefined type", self.findSimilarType(type_name));
                return .invalid;
            };
        const struct_type = self.types.get(struct_type_idx);
        if (struct_type != .struct_type) { self.err.errorWithCode(span_start, .e300, "not a struct type"); return .invalid; }
        // Check field pairs (name_token, value_node alternating)
        var fi: usize = 0;
        while (fi + 1 < field_pairs.len) : (fi += 2) {
            const fname_tok: TokenIndex = @enumFromInt(field_pairs[fi]);
            const fval_node: Index = @enumFromInt(field_pairs[fi + 1]);
            const fi_name = self.tree.tokenSlice(fname_tok);
            var found = false;
            for (struct_type.struct_type.fields) |sf| if (std.mem.eql(u8, sf.name, fi_name)) {
                found = true;
                const saved_expected = self.expected_type;
                self.expected_type = sf.type_idx;
                const vt = try self.checkExpr(fval_node);
                self.expected_type = saved_expected;
                if (!self.types.isAssignable(vt, sf.type_idx)) {
                    const vt_t = self.types.get(vt);
                    const is_safe_deref = self.safe_mode and vt_t == .pointer and
                        self.types.isAssignable(vt_t.pointer.elem, sf.type_idx);
                    const sf_t = self.types.get(sf.type_idx);
                    const is_safe_ref = self.safe_mode and sf_t == .pointer and
                        self.types.isAssignable(vt, sf_t.pointer.elem);
                    if (!is_safe_deref and !is_safe_ref) self.err.errorWithCode(span_start, .e300, "type mismatch in field");
                }
                break;
            };
            if (!found) self.errWithSuggestion(span_start, "unknown field", findSimilarField(fi_name, struct_type.struct_type.fields));
        }
        // Check required fields
        for (struct_type.struct_type.fields) |sf| {
            if (sf.default_value != .none) continue;
            var provided = false;
            var fi2: usize = 0;
            while (fi2 + 1 < field_pairs.len) : (fi2 += 2) {
                const ftok: TokenIndex = @enumFromInt(field_pairs[fi2]);
                if (std.mem.eql(u8, sf.name, self.tree.tokenSlice(ftok))) { provided = true; break; }
            }
            if (!provided) self.err.errorWithCode(span_start, .e300, "missing field in struct init");
        }
        if (self.actor_types.contains(struct_type.struct_type.name)) return try self.types.makePointer(struct_type_idx);
        return struct_type_idx;
    }

    /// Helper for resolving generic instance from a name + type_args SubRange.
    fn resolveGenericInstance_byName(self: *Checker, name: []const u8, type_args_range: SubRange, span: Span) CheckError!TypeIndex {
        const type_arg_nodes = self.tree.extraNodes(type_args_range);
        // Use the same logic as resolveGenericInstance
        const gen_info = self.generics.generic_structs.get(name) orelse {
            if (std.mem.eql(u8, name, "List") and type_arg_nodes.len == 1) {
                const elem_type = try self.resolveTypeExpr(type_arg_nodes[0]);
                return try self.types.makeList(elem_type);
            }
            if (std.mem.eql(u8, name, "Map") and type_arg_nodes.len == 2) {
                const key_type = try self.resolveTypeExpr(type_arg_nodes[0]);
                const val_type = try self.resolveTypeExpr(type_arg_nodes[1]);
                return try self.types.makeMap(key_type, val_type);
            }
            self.errWithSuggestion(span.start, "undefined generic type", self.findSimilarType(name));
            return .invalid;
        };
        if (type_arg_nodes.len != gen_info.type_params.len) {
            self.err.errorWithCode(span.start, .e300, "wrong number of type arguments");
            return .invalid;
        }
        var resolved_args = std.ArrayListUnmanaged(TypeIndex){};
        defer resolved_args.deinit(self.allocator);
        for (type_arg_nodes) |arg_node| {
            try resolved_args.append(self.allocator, try self.resolveTypeExpr(arg_node));
        }
        const cache_key = try self.buildGenericCacheKey(name, resolved_args.items);
        if (self.generics.instantiation_cache.get(cache_key)) |cached| return cached;
        // Build the concrete type
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (gen_info.type_params, 0..) |param_name, i| {
            try sub_map.put(param_name, resolved_args.items[i]);
        }
        const saved_tree = self.tree;
        const saved_safe_mode = self.safe_mode;
        const saved_scope = self.scope;
        self.tree = gen_info.tree;
        if (gen_info.tree.file) |file| self.safe_mode = file.safe_mode;
        if (gen_info.scope) |s| self.scope = s;
        defer { self.tree = saved_tree; self.safe_mode = saved_safe_mode; self.scope = saved_scope; }
        const struct_decl = self.tree.structDeclData(gen_info.node_idx);
        const old_sub = self.type_substitution;
        self.type_substitution = sub_map;
        const concrete_type = try self.buildStructTypeWithLayout(cache_key, struct_decl.fields, struct_decl.layout);
        self.type_substitution = old_sub;
        try self.types.registerNamed(cache_key, concrete_type);
        try self.generics.instantiation_cache.put(cache_key, concrete_type);
        try self.instantiateGenericImplMethods(name, cache_key, resolved_args.items);
        return concrete_type;
    }

    fn checkNewExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const ne = self.tree.extraData(data.node_and_extra[1], ast.NewExprData);
        const type_name = self.tree.tokenSlice(ne.type_name_token);
        const span_start = self.tree.nodeSpan(idx).start;
        const type_args = self.tree.extraSlice(ne.type_args);
        const struct_type_idx = if (type_args.len > 0)
            try self.resolveGenericInstance_byName(type_name, ne.type_args, self.tree.nodeSpan(idx))
        else
            self.resolveTypeByName(type_name) orelse {
                self.errWithSuggestion(span_start, "undefined type", self.findSimilarType(type_name));
                return .invalid;
            };
        const struct_type = self.types.get(struct_type_idx);
        if (struct_type != .struct_type) {
            self.err.errorWithCode(span_start, .e300, "new requires a struct type");
            return .invalid;
        }
        // Constructor sugar: `new Point(10, 20)` calls init() method
        if (ne.flags.is_constructor) {
            const resolved_name = if (type_args.len > 0) struct_type.struct_type.name else type_name;
            const init_method = self.types.lookupMethod(resolved_name, "init") orelse {
                self.err.errorWithCode(span_start, .e301, "no init method for constructor call");
                return .invalid;
            };
            const func_type = self.types.get(init_method.func_type);
            if (func_type == .func) {
                const init_params = func_type.func.params;
                const expected_args = if (init_params.len > 0) init_params.len - 1 else 0;
                const constructor_arg_nodes = self.tree.extraNodes(ne.constructor_args);
                if (constructor_arg_nodes.len != expected_args) {
                    self.err.errorWithCode(span_start, .e300, "wrong number of constructor arguments");
                } else {
                    for (constructor_arg_nodes, 0..) |arg_idx, i| {
                        const arg_type = try self.checkExpr(arg_idx);
                        if (!self.types.isAssignable(arg_type, init_params[i + 1].type_idx)) {
                            self.err.errorWithCode(span_start, .e300, "type mismatch in constructor argument");
                        }
                    }
                }
                if (func_type.func.return_type != TypeRegistry.VOID) {
                    self.err.errorWithCode(span_start, .e300, "init method must return void");
                }
            }
            return self.types.makePointer(struct_type_idx) catch .invalid;
        }
        // Validate field initializers
        const field_pair_raw = self.tree.extraSlice(ne.fields);
        var fpi: usize = 0;
        while (fpi + 1 < field_pair_raw.len) : (fpi += 2) {
            const fname_tok: TokenIndex = @enumFromInt(field_pair_raw[fpi]);
            const fval_node: Index = @enumFromInt(field_pair_raw[fpi + 1]);
            const fi_name = self.tree.tokenSlice(fname_tok);
            var found = false;
            for (struct_type.struct_type.fields) |sf| if (std.mem.eql(u8, sf.name, fi_name)) {
                found = true;
                const saved_expected = self.expected_type;
                self.expected_type = sf.type_idx;
                const vt = try self.checkExpr(fval_node);
                self.expected_type = saved_expected;
                if (!self.types.isAssignable(vt, sf.type_idx)) {
                    const vt_t = self.types.get(vt);
                    const is_safe_deref = self.safe_mode and vt_t == .pointer and
                        self.types.isAssignable(vt_t.pointer.elem, sf.type_idx);
                    const sf_t = self.types.get(sf.type_idx);
                    const is_safe_ref = self.safe_mode and sf_t == .pointer and
                        self.types.isAssignable(vt, sf_t.pointer.elem);
                    if (!is_safe_deref and !is_safe_ref) self.err.errorWithCode(span_start, .e300, "type mismatch in field");
                }
                break;
            };
            if (!found) self.errWithSuggestion(span_start, "unknown field", findSimilarField(fi_name, struct_type.struct_type.fields));
        }
        for (struct_type.struct_type.fields) |sf| {
            if (sf.default_value != .none) continue;
            var provided = false;
            var fpi2: usize = 0;
            while (fpi2 + 1 < field_pair_raw.len) : (fpi2 += 2) {
                const ftok: TokenIndex = @enumFromInt(field_pair_raw[fpi2]);
                if (std.mem.eql(u8, sf.name, self.tree.tokenSlice(ftok))) { provided = true; break; }
            }
            if (!provided) self.err.errorWithCode(span_start, .e300, "missing field in struct init");
        }
        return self.types.makePointer(struct_type_idx) catch .invalid;
    }

    fn checkArrayLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        const tag = self.tree.nodeTag(idx);
        const span_start = self.tree.nodeSpan(idx).start;
        switch (tag) {
            .array_literal_empty => {
                self.err.errorWithCode(span_start, .e300, "cannot infer type of empty array");
                return .invalid;
            },
            .array_literal_one => {
                const elem_node: Index = self.tree.nodeData(idx).node;
                const first_type = try self.checkExpr(elem_node);
                if (first_type == .invalid) return .invalid;
                return self.types.makeArray(first_type, 1) catch .invalid;
            },
            .array_literal => {
                const elems = self.tree.extraNodes(self.tree.nodeData(idx).extra_range);
                if (elems.len == 0) { self.err.errorWithCode(span_start, .e300, "cannot infer type of empty array"); return .invalid; }
                const first_type = try self.checkExpr(elems[0]);
                if (first_type == .invalid) return .invalid;
                for (elems[1..]) |elem_idx| {
                    const elem_type = try self.checkExpr(elem_idx);
                    if (!self.types.equal(first_type, elem_type) and !self.types.isAssignable(elem_type, first_type))
                        self.err.errorWithCode(span_start, .e300, "array elements must have same type");
                }
                return self.types.makeArray(first_type, elems.len) catch .invalid;
            },
            else => return .invalid,
        }
    }

    fn checkTupleLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        const elems = self.tree.extraNodes(self.tree.nodeData(idx).extra_range);
        const span_start = self.tree.nodeSpan(idx).start;
        if (elems.len < 2) { self.err.errorWithCode(span_start, .e300, "tuple must have at least 2 elements"); return .invalid; }
        var elem_types = std.ArrayListUnmanaged(TypeIndex){};
        defer elem_types.deinit(self.allocator);
        for (elems) |elem_idx| {
            const et = try self.checkExpr(elem_idx);
            elem_types.append(self.allocator, et) catch return .invalid;
        }
        return self.types.makeTuple(elem_types.items) catch .invalid;
    }

    fn checkIfExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const ie = self.tree.ifData(idx);
        const cond_type = try self.checkExpr(ie.condition);
        const span_start = self.tree.nodeSpan(idx).start;
        // Optional unwrap: if expr |val| { ... }
        if (ie.capture_token.unwrap()) |cap_tok| {
            const capture_name = self.tree.tokenSlice(cap_tok);
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) {
                self.err.errorWithCode(span_start, .e300, "capture requires optional type");
                return TypeRegistry.VOID;
            }
            const elem_type = cond_info.optional.elem;
            const capture_type = if (ie.capture_is_ptr) self.types.makePointer(elem_type) catch elem_type else elem_type;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(capture_name, .variable, capture_type, idx, false));
            const then_type = try self.checkExpr(ie.then_branch);
            self.scope = old_scope;
            if (ie.else_branch.unwrap()) |else_node| {
                const else_type = try self.checkExpr(else_node);
                if (!self.types.equal(then_type, else_type) and !self.types.isAssignable(else_type, then_type) and !self.types.isAssignable(then_type, else_type))
                    self.err.errorWithCode(span_start, .e300, "if branches have different types");
                return then_type;
            }
            return TypeRegistry.VOID;
        }
        if (!types_mod.isBool(self.types.get(cond_type))) self.err.errorWithCode(span_start, .e300, "condition must be bool");
        if (self.evalConstExpr(ie.condition)) |cond_val| {
            if (cond_val != 0) return try self.checkExpr(ie.then_branch);
            if (ie.else_branch.unwrap()) |else_node| return try self.checkExpr(else_node);
            return TypeRegistry.VOID;
        }
        const then_type = try self.checkExpr(ie.then_branch);
        if (ie.else_branch.unwrap()) |else_node| {
            const else_type = try self.checkExpr(else_node);
            if (!self.types.equal(then_type, else_type) and !self.types.isAssignable(else_type, then_type) and !self.types.isAssignable(then_type, else_type))
                self.err.errorWithCode(span_start, .e300, "if branches have different types");
            return then_type;
        }
        return TypeRegistry.VOID;
    }

    fn checkSwitchExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const se = self.tree.switchData(idx);
        var subject_type = try self.checkExpr(se.subject);
        var subject_info = self.types.get(subject_type);
        if (self.safe_mode and subject_info == .pointer) {
            const elem = self.types.get(subject_info.pointer.elem);
            if (elem == .union_type) { subject_type = subject_info.pointer.elem; subject_info = elem; }
        }
        const is_union = subject_info == .union_type;
        const is_enum = subject_info == .enum_type;
        const old_switch_enum = self.current_switch_enum_type;
        if (is_enum) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = old_switch_enum;
        var result_type: TypeIndex = if (self.expected_type != .invalid) self.expected_type else TypeRegistry.VOID;
        var first = self.expected_type != .invalid;
        // Iterate switch cases from extra_data
        const case_raw = self.tree.extraSlice(se.cases);
        for (case_raw) |case_ei| {
            const case = self.tree.extraData(@enumFromInt(case_ei), ast.SwitchCaseData);
            // Check patterns
            const pattern_nodes = self.tree.extraNodes(case.patterns);
            for (pattern_nodes) |val_idx| _ = try self.checkExpr(val_idx);
            if (case.guard.unwrap()) |guard| _ = try self.checkExpr(guard);
            if (is_union and case.capture_token.unwrap() != null) {
                const payload_type = self.resolveUnionCaptureType(subject_info.union_type, pattern_nodes);
                const capture_type = if (case.flags.capture_is_ptr) self.types.makePointer(payload_type) catch payload_type else payload_type;
                var capture_scope = Scope.init(self.allocator, self.scope);
                defer capture_scope.deinit();
                const old_scope = self.scope;
                self.scope = &capture_scope;
                if (case.capture_token.unwrap()) |ct| try capture_scope.define(Symbol.init(self.tree.tokenSlice(ct), .variable, capture_type, idx, false));
                const body_type = try self.checkExpr(case.body);
                self.scope = old_scope;
                if (!first) { result_type = self.materializeType(body_type); first = true; }
            } else {
                const body_type = try self.checkExpr(case.body);
                if (!first) { result_type = self.materializeType(body_type); first = true; }
            }
        }
        if (se.else_body.unwrap()) |else_body| _ = try self.checkExpr(else_body);
        // Enum exhaustiveness check
        if (is_enum and se.else_body.unwrap() == null) {
            const et = subject_info.enum_type;
            var covered = std.StringHashMap(void).init(self.allocator);
            defer covered.deinit();
            for (case_raw) |case_ei| {
                const case = self.tree.extraData(@enumFromInt(case_ei), ast.SwitchCaseData);
                if (case.guard.unwrap() != null) continue;
                const pats = self.tree.extraNodes(case.patterns);
                for (pats) |pat_idx| {
                    if (self.tree.nodeTag(pat_idx) == .field_access) {
                        const fa_data = self.tree.nodeData(pat_idx);
                        const fa_field = self.tree.tokenSlice(fa_data.node_and_token[1]);
                        covered.put(fa_field, {}) catch {};
                    }
                }
            }
            if (covered.count() < et.variants.len) {
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
                self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, msg);
            }
        }
        return result_type;
    }

    fn resolveUnionCaptureType(self: *Checker, ut: types_mod.UnionType, patterns: []const Index) TypeIndex {
        if (patterns.len == 0) return TypeRegistry.VOID;
        if (self.tree.nodeTag(patterns[0]) != .field_access) return TypeRegistry.VOID;
        const fa_data = self.tree.nodeData(patterns[0]);
        const field_name = self.tree.tokenSlice(fa_data.node_and_token[1]);
        for (ut.variants) |v| {
            if (std.mem.eql(u8, v.name, field_name)) return v.payload_type;
        }
        return TypeRegistry.VOID;
    }

    fn checkBlock(self: *Checker, idx: Index) CheckError!TypeIndex {
        const tag = self.tree.nodeTag(idx);
        var block_scope = Scope.init(self.allocator, self.scope);
        defer block_scope.deinit();
        const old_scope = self.scope;
        self.scope = &block_scope;
        // TODO: handle labeled blocks (need to check if block has label)
        const saved_expected = self.expected_type;
        self.expected_type = .invalid;
        switch (tag) {
            .block_one => {
                const data = self.tree.nodeData(idx).node_and_node;
                self.checkStmtsWithReachability(&.{data[0]});
                self.expected_type = saved_expected;
                const result = try self.checkExpr(data[1]);
                if (self.lint_mode) self.checkScopeUnused(&block_scope);
                self.scope = old_scope;
                return result;
            },
            .block_two => {
                const data = self.tree.nodeData(idx).node_and_node;
                self.checkStmtsWithReachability(&.{ data[0], data[1] });
                self.expected_type = saved_expected;
                if (self.lint_mode) self.checkScopeUnused(&block_scope);
                self.scope = old_scope;
                return TypeRegistry.VOID;
            },
            .block => {
                const data = self.tree.nodeData(idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                self.checkStmtsWithReachability(stmts);
                self.expected_type = saved_expected;
                if (self.lint_mode) self.checkScopeUnused(&block_scope);
                self.scope = old_scope;
                return TypeRegistry.VOID;
            },
            else => {
                self.expected_type = saved_expected;
                self.scope = old_scope;
                return TypeRegistry.VOID;
            },
        }
    }

    fn checkBlockExpr(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .block_one, .block_two, .block => { _ = try self.checkBlock(idx); return; },
            .block_stmt => { try self.checkBlockStmt(idx); return; },
            else => {},
        }
    }

    fn checkStringInterp(self: *Checker, idx: Index) CheckError!TypeIndex {
        // String interpolation segments are stored in extra_data as pairs: (tag, data)
        // tag 0 = text (data = token index), tag 1 = expr (data = node index)
        const range = self.tree.nodeData(idx).extra_range;
        const raw = self.tree.extraSlice(range);
        var i: usize = 0;
        while (i + 1 < raw.len) : (i += 2) {
            const seg_tag = raw[i];
            if (seg_tag == ast.SEGMENT_EXPR) {
                const expr_node: Index = @enumFromInt(raw[i + 1]);
                _ = try self.checkExpr(expr_node);
            }
        }
        return TypeRegistry.STRING;
    }

    pub fn isSendable(self: *Checker, ty: TypeIndex) bool {
        const info = self.types.get(ty);
        return switch (info) {
            .basic => true,
            .struct_type => |s| {
                if (self.unchecked_sendable_types.contains(s.name)) return true;
                for (s.fields) |f| { if (!self.isSendable(f.type_idx)) return false; }
                return true;
            },
            .enum_type => true,
            .union_type => |u| { for (u.variants) |v| { if (v.payload_type != .invalid and !self.isSendable(v.payload_type)) return false; } return true; },
            .optional => |o| self.isSendable(o.elem),
            .error_union => |eu| self.isSendable(eu.elem),
            .list => |l| self.isSendable(l.elem),
            .map => |m| self.isSendable(m.key) and self.isSendable(m.value),
            .task => true,
            .tuple => |t| { for (t.element_types) |et| { if (!self.isSendable(et)) return false; } return true; },
            .distinct => |d| self.isSendable(d.underlying),
            .existential => |e| std.mem.eql(u8, e.trait_name, "Sendable"),
            .pointer => false,
            .func => false,
            .slice => ty == TypeRegistry.STRING,
            .array => |a| self.isSendable(a.elem),
            .error_set => true,
        };
    }

    fn sendableTypeName(self: *Checker, ty: TypeIndex) []const u8 {
        return self.types.get(ty).name();
    }

    fn checkAddrOf(self: *Checker, idx: Index) CheckError!TypeIndex {
        const operand_node: Index = self.tree.nodeData(idx).node;
        const operand_type = try self.checkExpr(operand_node);
        return try self.types.makePointer(operand_type);
    }

    fn checkTryExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const operand_node: Index = self.tree.nodeData(idx).node;
        const operand_type = try self.checkExpr(operand_node);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .error_union) {
            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "try requires error union type");
            return .invalid;
        }
        const ret_info = self.types.get(self.current_return_type);
        if (ret_info != .error_union) {
            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "try in non-error-returning function");
        }
        return operand_info.error_union.elem;
    }

    fn checkAwaitExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const operand_node: Index = self.tree.nodeData(idx).node;
        const saved_in_await = self.in_await;
        self.in_await = true;
        defer self.in_await = saved_in_await;
        const operand_type = try self.checkExpr(operand_node);
        if (self.isCrossActorCallExpr(operand_node) or self.isGlobalActorCallExpr(operand_node)) return operand_type;
        const operand_info = self.types.get(operand_type);
        if (operand_info != .task) {
            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "cannot await non-task type");
            return .invalid;
        }
        return operand_info.task.result_type;
    }

    fn checkTaskExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const te = self.tree.extraData(data.node_and_extra[1], ast.TaskData);
        var task_scope = Scope.init(self.allocator, self.scope);
        defer task_scope.deinit();
        const old_scope = self.scope;
        const old_return = self.current_return_type;
        self.scope = &task_scope;
        self.current_return_type = TypeRegistry.I64;
        try self.checkBlockExpr(te.body);
        self.scope = old_scope;
        self.current_return_type = old_return;
        return try self.types.makeTask(TypeRegistry.I64);
    }

    fn isCrossActorCallExpr(self: *Checker, operand_idx: Index) bool {
        const op_tag = self.tree.nodeTag(operand_idx);
        if (op_tag != .call_zero and op_tag != .call_one and op_tag != .call) return false;
        const call_d = self.tree.callData(operand_idx);
        const receiver_name = self.getCallReceiverActorName(call_d.callee);
        if (receiver_name) |rn| return self.actor_types.contains(rn);
        return false;
    }

    fn isGlobalActorCallExpr(self: *Checker, operand_idx: Index) bool {
        const op_tag = self.tree.nodeTag(operand_idx);
        if (op_tag != .call_zero and op_tag != .call_one and op_tag != .call) return false;
        const call_d = self.tree.callData(operand_idx);
        if (self.tree.nodeTag(call_d.callee) != .ident) return false;
        const callee_name = self.tree.tokenSlice(self.tree.nodeMainToken(call_d.callee));
        return self.global_actor_fns.contains(callee_name);
    }

    fn getCallReceiverActorName(self: *Checker, callee_idx: Index) ?[]const u8 {
        if (self.tree.nodeTag(callee_idx) != .field_access) return null;
        const fa_data = self.tree.nodeData(callee_idx);
        const base_node: Index = fa_data.node_and_token[0];
        const base_type_idx = self.expr_types.get(base_node) orelse return null;
        const base_type = self.types.get(base_type_idx);
        return switch (base_type) {
            .struct_type => |st| st.name,
            .pointer => |ptr| switch (self.types.get(ptr.elem)) {
                .struct_type => |st| st.name,
                else => null,
            },
            else => null,
        };
    }

    fn checkCatchExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const operand_node: Index = data.node_and_extra[0];
        const ce = self.tree.extraData(data.node_and_extra[1], ast.CatchData);
        const operand_type = try self.checkExpr(operand_node);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .error_union) {
            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "catch requires error union type");
            return try self.checkExpr(ce.fallback);
        }
        const elem_type = operand_info.error_union.elem;
        var fallback_type: TypeIndex = TypeRegistry.VOID;
        if (ce.capture_token.unwrap()) |cap_tok| {
            const err_type = if (operand_info.error_union.error_set != .invalid) operand_info.error_union.error_set else TypeRegistry.I64;
            const capture_type = if (ce.flags.capture_is_ptr) self.types.makePointer(err_type) catch err_type else err_type;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(self.tree.tokenSlice(cap_tok), .variable, capture_type, idx, false));
            fallback_type = try self.checkExpr(ce.fallback);
            self.scope = old_scope;
        } else {
            fallback_type = try self.checkExpr(ce.fallback);
        }
        if (elem_type == TypeRegistry.VOID and fallback_type != TypeRegistry.VOID) return fallback_type;
        return elem_type;
    }

    fn checkOrElseExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const data = self.tree.nodeData(idx);
        const operand_node: Index = data.node_and_extra[0];
        const oe = self.tree.extraData(data.node_and_extra[1], ast.OrElseData);
        const operand_type = try self.checkExpr(operand_node);
        const operand_info = self.types.get(operand_type);
        if (operand_info != .optional) {
            self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "orelse requires optional type");
            return operand_type;
        }
        const elem_type = operand_info.optional.elem;
        const fallback_kind: ast.OrElseFallback = @enumFromInt(oe.fallback_kind);
        switch (fallback_kind) {
            .expr => { _ = try self.checkExpr(oe.fallback); return elem_type; },
            .return_val => { _ = try self.checkExpr(oe.fallback); return elem_type; },
            .return_void, .break_val, .continue_val => return elem_type,
        }
    }

    fn checkErrorLiteral(self: *Checker, idx: Index) CheckError!TypeIndex {
        const error_name_tok = self.tree.nodeData(idx).token;
        const error_name = self.tree.tokenSlice(error_name_tok);
        // Check function return type for error set
        const ret_info = self.types.get(self.current_return_type);
        if (ret_info == .error_union and ret_info.error_union.error_set != .invalid) {
            const es_info = self.types.get(ret_info.error_union.error_set);
            if (es_info == .error_set) {
                for (es_info.error_set.variants) |v| {
                    if (std.mem.eql(u8, v, error_name)) return ret_info.error_union.error_set;
                }
            }
        }
        if (self.expected_type != .invalid) {
            const exp_info = self.types.get(self.expected_type);
            if (exp_info == .error_union and exp_info.error_union.error_set != .invalid) {
                const es_info = self.types.get(exp_info.error_union.error_set);
                if (es_info == .error_set) {
                    for (es_info.error_set.variants) |v| {
                        if (std.mem.eql(u8, v, error_name)) return exp_info.error_union.error_set;
                    }
                }
            }
            if (exp_info == .error_set) {
                for (exp_info.error_set.variants) |v| {
                    if (std.mem.eql(u8, v, error_name)) return self.expected_type;
                }
            }
        }
        return self.current_return_type;
    }

    fn checkDeref(self: *Checker, idx: Index) CheckError!TypeIndex {
        const operand_node: Index = self.tree.nodeData(idx).node;
        const operand_type = try self.checkExpr(operand_node);
        if (self.types.isPointer(operand_type)) return self.types.pointerElem(operand_type);
        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "cannot dereference non-pointer");
        return .invalid;
    }

    // ---------------------------------------------------------------
    // Statement checking (ported from compiler/ lines 3707-4148)
    // ---------------------------------------------------------------

    fn checkStmt(self: *Checker, idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .expr_stmt => {
                const expr_node: Index = self.tree.nodeData(idx).node;
                _ = try self.checkExpr(expr_node);
            },
            .return_expr => {
                const val_node: Index = self.tree.nodeData(idx).node;
                try self.checkReturn(val_node, self.tree.nodeSpan(idx).start);
            },
            .return_void => {
                if (self.current_return_type == TypeRegistry.NORETURN) {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "noreturn function cannot return");
                } else if (self.current_return_type != TypeRegistry.VOID) {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "non-void function must return a value");
                }
            },
            .var_local, .const_local => try self.checkVarStmt(idx),
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div,
            .assign_mod, .assign_bit_and, .assign_bit_or, .assign_bit_xor,
            => try self.checkAssign(idx),
            .if_stmt_simple, .if_stmt => try self.checkIfStmt(idx),
            .while_stmt => try self.checkWhileStmt(idx),
            .for_stmt => try self.checkForStmt(idx),
            .block_stmt => try self.checkBlockStmt(idx),
            .break_plain => {
                if (!self.in_loop and self.in_labeled_block == 0)
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "break outside of loop");
            },
            .break_expr => {
                const data = self.tree.nodeData(idx);
                const bd = self.tree.extraData(data.node_and_extra[1], ast.BreakData);
                if (bd.label_token.unwrap() != null) {
                    if (self.in_labeled_block == 0 and !self.in_loop)
                        self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "break label does not match any enclosing labeled block or loop");
                    if (bd.value.unwrap()) |val_node| {
                        const val_type = try self.checkExpr(val_node);
                        if (self.labeled_block_result_type == .invalid) self.labeled_block_result_type = val_type;
                    }
                } else if (!self.in_loop) self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "break outside of loop");
            },
            .continue_plain => {
                if (!self.in_loop) self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "continue outside of loop");
            },
            .continue_labeled => {
                if (!self.in_loop) self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e300, "continue outside of loop");
            },
            .defer_stmt, .errdefer_stmt => {
                const expr_node: Index = self.tree.nodeData(idx).node;
                _ = try self.checkExpr(expr_node);
            },
            .destructure => try self.checkDestructureStmt(idx),
            .async_let => {
                const data = self.tree.nodeData(idx);
                const name_tok = data.token_and_node[0];
                const value_node: Index = data.token_and_node[1];
                const al_name = self.tree.tokenSlice(name_tok);
                const val_type = try self.checkExpr(value_node);
                if (!self.scope.isDefined(al_name)) {
                    try self.scope.define(Symbol.init(al_name, .constant, val_type, idx, false));
                }
            },
            .bad_node => {},
            else => {},
        }
    }

    fn checkReturn(self: *Checker, val_node: Index, span_start: Pos) CheckError!void {
        if (self.current_return_type == TypeRegistry.NORETURN) {
            self.err.errorWithCode(span_start, .e300, "noreturn function cannot return");
            return;
        }
        const saved_expected = self.expected_type;
        self.expected_type = self.current_return_type;
        defer self.expected_type = saved_expected;
        const val_type = try self.checkExpr(val_node);
        if (self.current_return_type == TypeRegistry.VOID) self.err.errorWithCode(span_start, .e300, "void function should not return a value")
        else if (!self.types.isAssignable(val_type, self.current_return_type)) self.err.errorWithCode(span_start, .e300, "type mismatch");
    }

    fn checkVarStmt(self: *Checker, idx: Index) CheckError!void {
        const vs = self.tree.localVarData(idx);
        const span_start = self.tree.nodeSpan(idx).start;
        if (self.scope.isDefined(vs.name)) { self.reportRedefined(span_start, vs.name); return; }
        if (self.lint_mode and self.scope.parent != null) {
            if (self.scope.parent.?.lookup(vs.name) != null) {
                self.err.warningWithCode(span_start, .w003, vs.name);
            }
        }
        var var_type: TypeIndex = if (vs.type_expr.unwrap()) |te| try self.resolveTypeExpr(te) else .invalid;
        if (vs.value.unwrap()) |val_node| {
            if (!self.isUndefinedLit(val_node) and !self.isZeroInitLit(val_node)) {
                const saved_expected = self.expected_type;
                if (var_type != .invalid) self.expected_type = var_type;
                defer self.expected_type = saved_expected;
                const val_type = try self.checkExpr(val_node);
                if (var_type == .invalid) var_type = self.materializeType(val_type)
                else if (!self.types.isAssignable(val_type, var_type)) {
                    const vt_info = self.types.get(var_type);
                    const is_composite_var = vt_info == .struct_type or vt_info == .union_type;
                    if (self.safe_mode and is_composite_var and self.types.get(val_type) == .pointer and blk: {
                        const elem = self.types.get(val_type).pointer.elem;
                        const elem_info = self.types.get(elem);
                        break :blk (elem_info == .struct_type or elem_info == .union_type) and self.types.isAssignable(elem, var_type);
                    }) {
                        var_type = val_type;
                    } else {
                        self.err.errorWithCode(span_start, .e300, "type mismatch");
                    }
                }
            }
        }
        if (vs.value.unwrap()) |val_node| {
            if (self.isZeroInitLit(val_node) and var_type == .invalid) {
                self.err.errorWithCode(span_start, .e300, "zero init requires type annotation");
            }
        }
        if (vs.is_unowned) {
            if (!self.types.couldBeARC(var_type)) {
                self.err.errorWithCode(span_start, .e300, "'unowned' can only be used with ARC-managed pointer types");
            }
        }
        if (vs.is_weak) {
            if (!self.types.couldBeARC(var_type)) {
                self.err.errorWithCode(span_start, .e300, "'weak' can only be used with ARC-managed pointer types");
            }
        }
        if (vs.is_const) {
            if (vs.value.unwrap()) |val_node| {
                if (self.evalConstExpr(val_node)) |cv| {
                    try self.scope.define(Symbol.initConst(vs.name, var_type, idx, cv));
                    return;
                }
            }
        }
        try self.scope.define(Symbol.init(vs.name, if (vs.is_const) .constant else .variable, var_type, idx, !vs.is_const));
    }

    fn checkDestructureStmt(self: *Checker, idx: Index) CheckError!void {
        const data = self.tree.nodeData(idx);
        const ds = self.tree.extraData(data.node_and_extra[1], ast.DestructureData);
        const span_start = self.tree.nodeSpan(idx).start;
        // Bindings are stored as alternating (name_token, type_expr OptionalIndex) pairs
        const binding_raw = self.tree.extraSlice(ds.bindings);
        const binding_count = binding_raw.len / 2;
        // Check for redefinitions
        var bi: usize = 0;
        while (bi < binding_raw.len) : (bi += 2) {
            const name_tok: TokenIndex = @enumFromInt(binding_raw[bi]);
            const bname = self.tree.tokenSlice(name_tok);
            if (self.scope.isDefined(bname)) { self.reportRedefined(span_start, bname); return; }
        }
        const val_type = try self.checkExpr(ds.value);
        const val_info = self.types.get(val_type);
        if (val_info != .tuple) {
            self.err.errorWithCode(span_start, .e300, "destructuring requires a tuple value");
            return;
        }
        if (binding_count != val_info.tuple.element_types.len) {
            self.err.errorWithCode(span_start, .e300, "destructuring count mismatch");
            return;
        }
        bi = 0;
        var elem_i: usize = 0;
        while (bi < binding_raw.len) : ({ bi += 2; elem_i += 1; }) {
            const name_tok: TokenIndex = @enumFromInt(binding_raw[bi]);
            const type_expr_oi: OptionalIndex = @enumFromInt(binding_raw[bi + 1]);
            var elem_type = val_info.tuple.element_types[elem_i];
            if (type_expr_oi.unwrap()) |te| {
                const annotated = try self.resolveTypeExpr(te);
                if (!self.types.isAssignable(elem_type, annotated)) self.err.errorWithCode(span_start, .e300, "type mismatch");
                elem_type = annotated;
            } else {
                elem_type = self.materializeType(elem_type);
            }
            const bname = self.tree.tokenSlice(name_tok);
            try self.scope.define(Symbol.init(bname, if (ds.flags.is_const) .constant else .variable, elem_type, idx, !ds.flags.is_const));
        }
    }

    fn checkAssign(self: *Checker, idx: Index) CheckError!void {
        const data = self.tree.nodeData(idx).node_and_node;
        const target_idx = data[0];
        const value_idx = data[1];
        const span_start = self.tree.nodeSpan(idx).start;
        // Discard pattern: _ = expr
        if (self.tree.nodeTag(target_idx) == .ident) {
            const tname = self.tree.tokenSlice(self.tree.nodeMainToken(target_idx));
            if (std.mem.eql(u8, tname, "_")) {
                _ = try self.checkExpr(value_idx);
                return;
            }
        }
        const target_type = try self.checkExpr(target_idx);
        const old_expected = self.expected_type;
        self.expected_type = target_type;
        const value_type = try self.checkExpr(value_idx);
        self.expected_type = old_expected;
        const target_tag = self.tree.nodeTag(target_idx);
        switch (target_tag) {
            .ident => {
                const id_name = self.tree.tokenSlice(self.tree.nodeMainToken(target_idx));
                if (self.scope.lookup(id_name)) |sym| if (!sym.mutable) { self.err.errorWithCode(span_start, .e300, "cannot assign to constant"); return; };
            },
            .index, .field_access, .unary_deref => {},
            else => { self.err.errorWithCode(span_start, .e300, "invalid assignment target"); return; },
        }
        if (!self.types.isAssignable(value_type, target_type)) {
            const value_info = self.types.get(value_type);
            if (value_info != .pointer or !self.types.isAssignable(value_info.pointer.elem, target_type)) {
                self.err.errorWithCode(span_start, .e300, "type mismatch");
            }
        }
    }

    fn checkIfStmt(self: *Checker, idx: Index) CheckError!void {
        const ie = self.tree.ifData(idx);
        const cond_type = try self.checkExpr(ie.condition);
        const span_start = self.tree.nodeSpan(idx).start;
        if (ie.capture_token.unwrap()) |cap_tok| {
            const capture_name = self.tree.tokenSlice(cap_tok);
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) { self.err.errorWithCode(span_start, .e300, "capture requires optional type"); return; }
            const elem_type = cond_info.optional.elem;
            const capture_type = if (ie.capture_is_ptr) self.types.makePointer(elem_type) catch elem_type else elem_type;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(capture_name, .variable, capture_type, idx, false));
            try self.checkStmt(ie.then_branch);
            self.scope = old_scope;
            if (ie.else_branch.unwrap()) |else_node| try self.checkStmt(else_node);
            return;
        }
        if (!types_mod.isBool(self.types.get(cond_type))) self.err.errorWithCode(span_start, .e300, "condition must be bool");
        if (self.evalConstExpr(ie.condition)) |cond_val| {
            if (cond_val != 0) { try self.checkStmt(ie.then_branch); return; }
            if (ie.else_branch.unwrap()) |else_node| { try self.checkStmt(else_node); return; }
            return;
        }
        if (self.lint_mode and self.isEmptyBlock(ie.then_branch)) {
            self.err.warningWithCode(span_start, .w005, "empty if body");
        }
        try self.checkStmt(ie.then_branch);
        if (ie.else_branch.unwrap()) |else_node| {
            if (self.lint_mode and self.isEmptyBlock(else_node)) {
                self.err.warningWithCode(span_start, .w005, "empty else body");
            }
            try self.checkStmt(else_node);
        }
    }

    fn checkWhileStmt(self: *Checker, idx: Index) CheckError!void {
        const ws = self.tree.whileData(idx);
        const cond_type = try self.checkExpr(ws.condition);
        const span_start = self.tree.nodeSpan(idx).start;
        if (ws.capture_token.unwrap()) |cap_tok| {
            const cond_info = self.types.get(cond_type);
            if (cond_info != .optional) { self.err.errorWithCode(span_start, .e300, "capture requires optional type"); return; }
            const elem_type = cond_info.optional.elem;
            const capture_type = if (ws.capture_is_ptr) self.types.makePointer(elem_type) catch elem_type else elem_type;
            var capture_scope = Scope.init(self.allocator, self.scope);
            defer capture_scope.deinit();
            const old_scope = self.scope;
            self.scope = &capture_scope;
            try capture_scope.define(Symbol.init(self.tree.tokenSlice(cap_tok), .variable, capture_type, idx, false));
            const old_in_loop = self.in_loop;
            self.in_loop = true;
            if (ws.continue_expr.unwrap()) |ce| try self.checkContinueExpr(ce);
            try self.checkStmt(ws.body);
            self.in_loop = old_in_loop;
            self.scope = old_scope;
            return;
        }
        if (!types_mod.isBool(self.types.get(cond_type))) self.err.errorWithCode(span_start, .e300, "condition must be bool");
        if (self.lint_mode and self.isEmptyBlock(ws.body)) {
            self.err.warningWithCode(span_start, .w005, "empty while body");
        }
        const old_in_loop = self.in_loop;
        self.in_loop = true;
        if (ws.continue_expr.unwrap()) |ce| try self.checkContinueExpr(ce);
        try self.checkStmt(ws.body);
        self.in_loop = old_in_loop;
    }

    fn checkContinueExpr(self: *Checker, node_idx: Index) CheckError!void {
        const tag = self.tree.nodeTag(node_idx);
        switch (tag) {
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div,
            .assign_mod, .assign_bit_and, .assign_bit_or, .assign_bit_xor,
            => try self.checkStmt(node_idx),
            .expr_stmt => try self.checkStmt(node_idx),
            else => { _ = try self.checkExpr(node_idx); },
        }
    }

    fn checkForStmt(self: *Checker, idx: Index) CheckError!void {
        const fs = self.tree.forData(idx);
        const span_start = self.tree.nodeSpan(idx).start;
        if (fs.is_inline) return self.checkInlineFor(fs);
        if (fs.is_await) {
            const iter_node = fs.iterable.unwrap() orelse return;
            const iter_type = try self.checkExpr(iter_node);
            const iter_info = self.types.get(iter_type);
            const type_name: ?[]const u8 = switch (iter_info) {
                .struct_type => |st| st.name,
                .pointer => |ptr| switch (self.types.get(ptr.elem)) { .struct_type => |st| st.name, else => null },
                else => null,
            };
            var elem_type: TypeIndex = TypeRegistry.I64;
            if (type_name) |tn| {
                if (self.lookupMethod(tn, "next")) |method| {
                    const method_info = self.types.get(method.func_type);
                    if (method_info == .func) {
                        const ret = self.types.get(method_info.func.return_type);
                        if (ret == .optional) elem_type = ret.optional.elem;
                    }
                }
            }
            var loop_scope = Scope.init(self.allocator, self.scope);
            defer loop_scope.deinit();
            try loop_scope.define(Symbol.init(fs.binding, .variable, elem_type, idx, false));
            const old_scope = self.scope;
            const old_in_loop = self.in_loop;
            self.scope = &loop_scope;
            self.in_loop = true;
            try self.checkBlockExpr(fs.body);
            self.scope = old_scope;
            self.in_loop = old_in_loop;
            return;
        }
        const is_range = fs.range_start != .none;
        var elem_type: TypeIndex = .invalid;
        var idx_type: TypeIndex = TypeRegistry.I64;
        if (is_range) {
            const start_type = try self.checkExpr(fs.range_start.unwrap().?);
            const end_type = try self.checkExpr(fs.range_end.unwrap().?);
            if (!types_mod.isInteger(self.types.get(start_type)) or !types_mod.isInteger(self.types.get(end_type))) {
                self.err.errorWithCode(span_start, .e300, "range bounds must be integers");
            } else { elem_type = start_type; }
        } else {
            const iter_node = fs.iterable.unwrap() orelse return;
            const iter_type = try self.checkExpr(iter_node);
            const iter = self.types.get(iter_type);
            switch (iter) {
                .array => |a| elem_type = a.elem,
                .slice => |s| elem_type = s.elem,
                .map => |ma| { elem_type = ma.value; idx_type = ma.key; },
                .struct_type => |st| {
                    if (std.mem.startsWith(u8, st.name, "Map(")) {
                        if (parseMapTypeArgs(st.name)) |margs| { idx_type = margs[0]; elem_type = margs[1]; }
                        else self.err.errorWithCode(span_start, .e300, "cannot iterate over this type");
                    } else self.err.errorWithCode(span_start, .e300, "cannot iterate over this type");
                },
                else => self.err.errorWithCode(span_start, .e300, "cannot iterate over this type"),
            }
        }
        var loop_scope = Scope.init(self.allocator, self.scope);
        defer loop_scope.deinit();
        try loop_scope.define(Symbol.init(fs.binding, .variable, elem_type, idx, false));
        if (fs.index_binding_token.unwrap()) |ib_tok| {
            try loop_scope.define(Symbol.init(self.tree.tokenSlice(ib_tok), .variable, idx_type, idx, false));
        }
        if (self.lint_mode and self.isEmptyBlock(fs.body)) {
            self.err.warningWithCode(span_start, .w005, "empty for body");
        }
        const old_scope = self.scope;
        const old_in_loop = self.in_loop;
        self.scope = &loop_scope;
        self.in_loop = true;
        try self.checkStmt(fs.body);
        self.scope = old_scope;
        self.in_loop = old_in_loop;
    }

    fn checkInlineFor(self: *Checker, fs: ast.full.ForFull) CheckError!void {
        const is_range = fs.range_start != .none;
        if (is_range) {
            const start_node = fs.range_start.unwrap() orelse return;
            const end_node = fs.range_end.unwrap() orelse return;
            const start_val = self.evalConstExpr(start_node) orelse {
                self.err.errorWithCode(self.tree.nodeSpan(fs.body).start, .e300, "inline for range start must be comptime-known");
                return;
            };
            const end_val = self.evalConstExpr(end_node) orelse {
                self.err.errorWithCode(self.tree.nodeSpan(fs.body).start, .e300, "inline for range end must be comptime-known");
                return;
            };
            var i = start_val;
            while (i < end_val) : (i += 1) {
                var iter_scope = Scope.init(self.allocator, self.scope);
                defer iter_scope.deinit();
                try iter_scope.define(Symbol.initConst(fs.binding, TypeRegistry.I64, @enumFromInt(0), i));
                const old_scope = self.scope;
                self.scope = &iter_scope;
                try self.checkStmt(fs.body);
                self.scope = old_scope;
            }
            return;
        }
        const iter_node = fs.iterable.unwrap() orelse return;
        const iter_val = self.evalComptimeValue(iter_node) orelse {
            self.err.errorWithCode(self.tree.nodeSpan(fs.body).start, .e300, "inline for requires comptime-known iterable");
            return;
        };
        if (iter_val != .array) {
            self.err.errorWithCode(self.tree.nodeSpan(fs.body).start, .e300, "inline for iterable must be comptime array or range");
            return;
        }
        for (iter_val.array.elements.items, 0..) |elem, eidx| {
            var iter_scope = Scope.init(self.allocator, self.scope);
            defer iter_scope.deinit();
            var sym = Symbol.init(fs.binding, .constant, TypeRegistry.I64, @enumFromInt(0), false);
            sym.comptime_val = elem;
            if (elem == .int) sym.const_value = elem.int;
            try iter_scope.define(sym);
            if (fs.index_binding_token.unwrap()) |ib_tok| {
                try iter_scope.define(Symbol.initConst(self.tree.tokenSlice(ib_tok), TypeRegistry.I64, @enumFromInt(0), @intCast(eidx)));
            }
            const old_scope = self.scope;
            self.scope = &iter_scope;
            try self.checkStmt(fs.body);
            self.scope = old_scope;
        }
    }

    fn checkBlockStmt(self: *Checker, idx: Index) CheckError!void {
        var block_scope = Scope.init(self.allocator, self.scope);
        defer block_scope.deinit();
        const old_scope = self.scope;
        self.scope = &block_scope;
        const stmts = self.tree.extraNodes(self.tree.nodeData(idx).extra_range);
        self.checkStmtsWithReachability(stmts);
        if (self.lint_mode) self.checkScopeUnused(&block_scope);
        self.scope = old_scope;
    }

    // ---------------------------------------------------------------
    // Type resolution (ported from compiler/ lines 4149-4354)
    // ---------------------------------------------------------------

    fn resolveTypeExpr(self: *Checker, idx: Index) CheckError!TypeIndex {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                if (self.type_substitution) |sub| {
                    if (sub.get(name)) |substituted| return substituted;
                }
                if (self.resolveTypeByName(name)) |tidx| return tidx;
                if (self.types.lookupBitfieldType(name)) |tidx| return tidx;
                self.errWithSuggestion(self.tree.nodeSpan(idx).start, "undefined type", self.findSimilarType(name));
                return .invalid;
            },
            .type_named => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                if (self.type_substitution) |sub| {
                    if (sub.get(name)) |substituted| return substituted;
                }
                if (self.resolveTypeByName(name)) |tidx| return tidx;
                if (self.types.lookupBitfieldType(name)) |tidx| return tidx;
                if (self.scope.lookup(name) != null) return .invalid;
                self.errWithSuggestion(self.tree.nodeSpan(idx).start, "undefined type", self.findSimilarType(name));
                return .invalid;
            },
            .builtin_call => {
                const bc = self.tree.builtinCallData(idx);
                if (bc.kind == .type_of) {
                    const arg0 = bc.arg0.unwrap() orelse return .invalid;
                    return try self.checkExpr(arg0);
                }
                return .invalid;
            },
            .type_pointer => {
                const elem = try self.resolveTypeExpr(self.tree.nodeData(idx).node);
                return self.types.makePointer(elem);
            },
            .type_optional => {
                const elem = try self.resolveTypeExpr(self.tree.nodeData(idx).node);
                return self.types.makeOptional(elem);
            },
            .type_error_union => {
                const elem = try self.resolveTypeExpr(self.tree.nodeData(idx).node);
                return self.types.makeErrorUnion(elem);
            },
            .type_error_union_set => {
                const data = self.tree.nodeData(idx).node_and_node;
                const es_node = data[0];
                const elem_node = data[1];
                const elem = try self.resolveTypeExpr(elem_node);
                const es_type = try self.resolveTypeExpr(es_node);
                return self.types.makeErrorUnionWithSet(elem, es_type);
            },
            .type_slice => {
                const elem = try self.resolveTypeExpr(self.tree.nodeData(idx).node);
                return self.types.makeSlice(elem);
            },
            .type_array => {
                const data = self.tree.nodeData(idx).node_and_node;
                const size_node = data[0];
                const elem_node = data[1];
                const elem = try self.resolveTypeExpr(elem_node);
                const size: u64 = blk: {
                    const size_tag = self.tree.nodeTag(size_node);
                    if (size_tag == .literal_int) {
                        break :blk std.fmt.parseInt(u64, self.tree.tokenSlice(self.tree.nodeMainToken(size_node)), 0) catch 0;
                    }
                    if (self.evalConstExpr(size_node)) |v| {
                        if (v > 0) break :blk @as(u64, @intCast(v));
                    }
                    break :blk 0;
                };
                return try self.types.makeArray(elem, size);
            },
            .type_map => {
                const data = self.tree.nodeData(idx).node_and_node;
                return self.types.makeMap(try self.resolveTypeExpr(data[0]), try self.resolveTypeExpr(data[1]));
            },
            .type_list => {
                return self.types.makeList(try self.resolveTypeExpr(self.tree.nodeData(idx).node));
            },
            .type_function => {
                const data = self.tree.nodeData(idx);
                const ft = self.tree.extraData(data.node_and_extra[1], ast.FnType);
                var func_params = std.ArrayListUnmanaged(types_mod.FuncParam){};
                defer func_params.deinit(self.allocator);
                const param_raw = self.tree.extraSlice(ft.params);
                for (param_raw) |pt_raw| {
                    const pt_idx: Index = @enumFromInt(pt_raw);
                    var pt_type = try self.resolveTypeExpr(pt_idx);
                    pt_type = try self.safeWrapType(pt_type);
                    try func_params.append(self.allocator, .{ .name = "", .type_idx = pt_type });
                }
                const ret_type = if (ft.ret.unwrap()) |r| try self.resolveTypeExpr(r) else TypeRegistry.VOID;
                return try self.types.makeFunc(func_params.items, ret_type);
            },
            .type_tuple => {
                const elems_raw = self.tree.extraSlice(self.tree.nodeData(idx).extra_range);
                var elem_types = std.ArrayListUnmanaged(TypeIndex){};
                defer elem_types.deinit(self.allocator);
                for (elems_raw) |e_raw| try elem_types.append(self.allocator, try self.resolveTypeExpr(@enumFromInt(e_raw)));
                return try self.types.makeTuple(elem_types.items);
            },
            .type_generic => {
                const data = self.tree.nodeData(idx);
                const gi = self.tree.extraData(data.node_and_extra[1], ast.GenericInstance);
                const gi_name = self.tree.tokenSlice(gi.name_token);
                return try self.resolveGenericInstance_byName(gi_name, gi.type_args, self.tree.nodeSpan(idx));
            },
            .type_existential => {
                const trait_node: Index = self.tree.nodeData(idx).node;
                const trait_tag = self.tree.nodeTag(trait_node);
                const trait_name = if (trait_tag == .ident or trait_tag == .type_named)
                    self.tree.tokenSlice(self.tree.nodeMainToken(trait_node))
                else
                    return .invalid;
                const trait_def = self.generics.trait_defs.get(trait_name) orelse {
                    self.err.errorWithCode(self.tree.nodeSpan(idx).start, .e301, try std.fmt.allocPrint(self.allocator, "undefined trait '{s}'", .{trait_name}));
                    return .invalid;
                };
                var conforming = std.ArrayListUnmanaged([]const u8){};
                var impl_it = self.generics.trait_impls.iterator();
                while (impl_it.next()) |impl_entry| {
                    if (std.mem.eql(u8, impl_entry.value_ptr.*, trait_name)) {
                        const key = impl_entry.key_ptr.*;
                        if (std.mem.indexOf(u8, key, ":")) |colon| {
                            try conforming.append(self.allocator, key[colon + 1 ..]);
                        }
                    }
                }
                const exist_idx = try self.types.makeExistential(trait_name, trait_def.method_names);
                var exist_type = &self.types.types.items[@intCast(exist_idx)];
                exist_type.existential.conforming_types = try self.allocator.dupe([]const u8, conforming.items);
                return exist_idx;
            },
            else => .invalid,
        };
    }

    fn resolveGenericInstance(self: *Checker, name: []const u8, type_arg_nodes: []const Index, span: Span) CheckError!TypeIndex {
        const gen_info = self.generics.generic_structs.get(name) orelse {
            if (std.mem.eql(u8, name, "List") and type_arg_nodes.len == 1) return try self.types.makeList(try self.resolveTypeExpr(type_arg_nodes[0]));
            if (std.mem.eql(u8, name, "Map") and type_arg_nodes.len == 2) return try self.types.makeMap(try self.resolveTypeExpr(type_arg_nodes[0]), try self.resolveTypeExpr(type_arg_nodes[1]));
            self.errWithSuggestion(span.start, "undefined generic type", self.findSimilarType(name));
            return .invalid;
        };
        if (type_arg_nodes.len != gen_info.type_params.len) { self.err.errorWithCode(span.start, .e300, "wrong number of type arguments"); return .invalid; }
        var resolved_args = std.ArrayListUnmanaged(TypeIndex){};
        defer resolved_args.deinit(self.allocator);
        for (type_arg_nodes) |arg_node| try resolved_args.append(self.allocator, try self.resolveTypeExpr(arg_node));
        const cache_key = try self.buildGenericCacheKey(name, resolved_args.items);
        if (self.generics.instantiation_cache.get(cache_key)) |cached| return cached;
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (gen_info.type_params, 0..) |param_name, i| try sub_map.put(param_name, resolved_args.items[i]);
        const saved_tree = self.tree;
        const saved_safe_mode = self.safe_mode;
        const saved_scope = self.scope;
        self.tree = gen_info.tree;
        if (gen_info.tree.file) |file| self.safe_mode = file.safe_mode;
        if (gen_info.scope) |s| self.scope = s;
        defer { self.tree = saved_tree; self.safe_mode = saved_safe_mode; self.scope = saved_scope; }
        const struct_decl = self.tree.structDeclData(gen_info.node_idx);
        const old_sub = self.type_substitution;
        self.type_substitution = sub_map;
        const concrete_type = try self.buildStructTypeWithLayout(cache_key, struct_decl.fields, struct_decl.layout);
        self.type_substitution = old_sub;
        try self.types.registerNamed(cache_key, concrete_type);
        try self.generics.instantiation_cache.put(cache_key, concrete_type);
        try self.instantiateGenericImplMethods(name, cache_key, resolved_args.items);
        return concrete_type;
    }

    // ---------------------------------------------------------------
    // Type building (ported from compiler/ lines 4355-4855)
    // ---------------------------------------------------------------

    fn instantiateGenericImplMethods(
        self: *Checker,
        base_name: []const u8,
        concrete_name: []const u8,
        resolved_args: []const TypeIndex,
    ) CheckError!void {
        const impl_list = self.generics.generic_impl_blocks.get(base_name) orelse return;
        for (impl_list.items) |impl_info| {
            if (impl_info.type_params.len != resolved_args.len) continue;
            const saved_tree = self.tree;
            const saved_safe_mode = self.safe_mode;
            const saved_scope = self.scope;
            self.tree = impl_info.tree;
            if (impl_info.tree.file) |file| self.safe_mode = file.safe_mode;
            if (impl_info.scope) |s| self.scope = s;
            defer { self.tree = saved_tree; self.safe_mode = saved_safe_mode; self.scope = saved_scope; }
            var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
            defer sub_map.deinit();
            for (impl_info.type_params, 0..) |param_name, i| try sub_map.put(param_name, resolved_args[i]);
            const old_sub = self.type_substitution;
            self.type_substitution = sub_map;
            defer self.type_substitution = old_sub;
            const safe_self_type: ?TypeIndex = if (self.safe_mode) blk: {
                const concrete_type = self.generics.instantiation_cache.get(concrete_name) orelse break :blk null;
                break :blk self.types.makePointer(concrete_type) catch null;
            } else null;
            // Pass 1: Register all method signatures
            for (impl_info.methods) |method_idx| {
                const m_tag = self.tree.nodeTag(method_idx);
                if (m_tag != .fn_decl and m_tag != .fn_decl_extern) continue;
                const f = self.tree.fnDeclData(method_idx);
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_name, f.name });
                const func_type = if (safe_self_type != null and !f.flags.is_static) blk: {
                    const base_type = try self.buildFuncType(f.params, f.return_type);
                    const base_info = self.types.get(base_type);
                    if (base_info != .func) break :blk base_type;
                    var new_params = std.ArrayListUnmanaged(types_mod.FuncParam){};
                    defer new_params.deinit(self.allocator);
                    try new_params.append(self.allocator, .{ .name = "self", .type_idx = safe_self_type.? });
                    for (base_info.func.params) |p| try new_params.append(self.allocator, p);
                    break :blk try self.types.makeFunc(new_params.items, base_info.func.return_type);
                } else try self.buildFuncType(f.params, f.return_type);
                if (!self.global_scope.isDefined(synth_name)) {
                    try self.global_scope.define(Symbol.init(synth_name, .function, func_type, method_idx, false));
                }
                const param_raw = self.tree.extraSlice(f.params);
                const has_self_p = if (param_raw.len > 0) blk: {
                    const ff = self.tree.extraData(@enumFromInt(param_raw[0]), ast.Field);
                    break :blk std.mem.eql(u8, self.tree.tokenSlice(ff.name_token), "self");
                } else false;
                const effective_static_g = f.flags.is_static or (!self.safe_mode and (param_raw.len == 0 or !has_self_p));
                try self.types.registerMethod(concrete_name, types_mod.MethodInfo{
                    .name = f.name,
                    .func_name = synth_name,
                    .func_type = func_type,
                    .receiver_is_ptr = !effective_static_g,
                    .is_static = effective_static_g,
                    .source_tree = self.tree,
                });
                const indirect_result = self.isTypeParamNameInList(f.return_type, impl_info.type_params);
                const inst = GenericInstInfo{
                    .concrete_name = synth_name,
                    .generic_node = method_idx,
                    .type_args = try self.allocator.dupe(TypeIndex, resolved_args),
                    .type_param_names = impl_info.type_params,
                    .tree = impl_info.tree,
                    .scope = impl_info.scope,
                    .has_indirect_result = indirect_result,
                };
                try self.generics.generic_inst_by_name.put(synth_name, inst);
            }
            // Pass 2: Check method bodies
            for (impl_info.methods) |method_idx| {
                const m_tag = self.tree.nodeTag(method_idx);
                if (m_tag != .fn_decl and m_tag != .fn_decl_extern) continue;
                const f = self.tree.fnDeclData(method_idx);
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_name, f.name });
                var inst_expr_types = std.AutoHashMap(Index, TypeIndex).init(self.allocator);
                {
                    const saved_et = self.expr_types;
                    self.expr_types = inst_expr_types;
                    defer { inst_expr_types = self.expr_types; self.expr_types = saved_et; }
                    if (safe_self_type != null and !f.flags.is_static) {
                        var self_scope = Scope.init(self.allocator, self.scope);
                        defer self_scope.deinit();
                        try self_scope.define(Symbol.init("self", .parameter, safe_self_type.?, method_idx, true));
                        const old_scope_2 = self.scope;
                        self.scope = &self_scope;
                        defer self.scope = old_scope_2;
                        try self.checkFnDeclWithName(f, method_idx, synth_name);
                    } else {
                        try self.checkFnDeclWithName(f, method_idx, synth_name);
                    }
                }
                if (self.generics.generic_inst_by_name.getPtr(synth_name)) |ptr| {
                    ptr.expr_types = inst_expr_types;
                }
            }
        }
    }

    /// Check if a return type expression is a type parameter name.
    fn isTypeParamNameInList(self: *Checker, return_type: OptionalIndex, type_params: []const []const u8) bool {
        const rt_node = return_type.unwrap() orelse return false;
        const rt_tag = self.tree.nodeTag(rt_node);
        if (rt_tag == .type_named or rt_tag == .ident) {
            const rt_name = self.tree.tokenSlice(self.tree.nodeMainToken(rt_node));
            for (type_params) |tp| {
                if (std.mem.eql(u8, tp, rt_name)) return true;
            }
        }
        return false;
    }

    fn instantiateGenericFunc(self: *Checker, call_idx: Index, gen_info: GenericInfo, name: []const u8) CheckError!TypeIndex {
        const args = self.getCallArgNodes(call_idx);
        const span_start = self.tree.nodeSpan(call_idx).start;
        if (args.len != gen_info.type_params.len) {
            self.err.errorWithCode(span_start, .e300, "wrong number of type arguments");
            return .invalid;
        }
        var resolved_args = std.ArrayListUnmanaged(TypeIndex){};
        defer resolved_args.deinit(self.allocator);
        for (args) |arg_node| {
            const arg_type = try self.resolveTypeExpr(arg_node);
            if (arg_type == .invalid) {
                if (self.tree.nodeTag(arg_node) == .ident) {
                    if (self.resolveTypeByName(self.tree.tokenSlice(self.tree.nodeMainToken(arg_node)))) |tidx| {
                        try resolved_args.append(self.allocator, tidx);
                        continue;
                    }
                }
                self.err.errorWithCode(span_start, .e300, "expected type argument");
                return .invalid;
            }
            try resolved_args.append(self.allocator, arg_type);
        }
        // Validate trait bounds
        if (gen_info.type_param_bounds.len > 0) {
            for (gen_info.type_param_bounds, 0..) |bound, i| {
                if (bound) |trait_name| {
                    const tname = self.types.typeName(resolved_args.items[i]);
                    const impl_key = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ trait_name, tname });
                    defer self.allocator.free(impl_key);
                    if (self.generics.trait_impls.get(impl_key) == null) {
                        self.err.errorWithCode(span_start, .e300, "type does not satisfy trait bound");
                        return .invalid;
                    }
                }
            }
        }
        const cache_key = try self.buildGenericCacheKey(name, resolved_args.items);
        const saved_tree = self.tree;
        const saved_safe_mode = self.safe_mode;
        const saved_scope = self.scope;
        self.tree = gen_info.tree;
        if (gen_info.tree.file) |file| self.safe_mode = file.safe_mode;
        if (gen_info.scope) |s| self.scope = s;
        defer { self.tree = saved_tree; self.safe_mode = saved_safe_mode; self.scope = saved_scope; }
        const fn_decl = self.tree.fnDeclData(gen_info.node_idx);
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (gen_info.type_params, 0..) |param_name, i| try sub_map.put(param_name, resolved_args.items[i]);
        const old_sub = self.type_substitution;
        self.type_substitution = sub_map;
        const func_type = try self.buildFuncType(fn_decl.params, fn_decl.return_type);
        if (!self.global_scope.isDefined(cache_key)) {
            try self.global_scope.define(Symbol.init(cache_key, .function, func_type, gen_info.node_idx, false));
        }
        var inst_expr_types = std.AutoHashMap(Index, TypeIndex).init(self.allocator);
        {
            const saved_et = self.expr_types;
            self.expr_types = inst_expr_types;
            defer { inst_expr_types = self.expr_types; self.expr_types = saved_et; }
            try self.checkFnDeclWithName(fn_decl, gen_info.node_idx, cache_key);
        }
        self.type_substitution = old_sub;
        const inst = GenericInstInfo{
            .concrete_name = cache_key,
            .generic_node = gen_info.node_idx,
            .type_args = try self.allocator.dupe(TypeIndex, resolved_args.items),
            .tree = gen_info.tree,
            .scope = gen_info.scope,
            .expr_types = inst_expr_types,
            .has_indirect_result = self.isTypeParamNameInList(fn_decl.return_type, gen_info.type_params),
        };
        const callee_idx = self.tree.callData(call_idx).callee;
        try self.generic_instantiations.put(callee_idx, inst);
        try self.generics.generic_inst_by_name.put(cache_key, inst);
        return func_type;
    }

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

    pub fn safeWrapType(self: *Checker, type_idx: TypeIndex) !TypeIndex {
        const t_pre = self.types.get(type_idx);
        if (t_pre == .existential) return try self.types.makePointer(type_idx);
        if (t_pre == .struct_type and self.actor_types.contains(t_pre.struct_type.name))
            return try self.types.makePointer(type_idx);
        if (!self.safe_mode) return type_idx;
        const t = t_pre;
        if (t == .struct_type or t == .union_type) return try self.types.makePointer(type_idx);
        if (t == .error_union) {
            const elem = self.types.get(t.error_union.elem);
            if (elem == .struct_type or elem == .union_type) {
                const wrapped = try self.types.makePointer(t.error_union.elem);
                return try self.types.makeErrorUnionWithSet(wrapped, t.error_union.error_set);
            }
        }
        return type_idx;
    }

    fn buildFuncType(self: *Checker, params: SubRange, return_type_oi: OptionalIndex) CheckError!TypeIndex {
        var func_params = std.ArrayListUnmanaged(types_mod.FuncParam){};
        defer func_params.deinit(self.allocator);
        const param_raw = self.tree.extraSlice(params);
        for (param_raw) |raw_ei| {
            const field = self.tree.extraData(@enumFromInt(raw_ei), ast.Field);
            const param_name = self.tree.tokenSlice(field.name_token);
            var type_idx = try self.resolveTypeExpr(field.type_expr);
            const is_substituted = if (self.type_substitution) |sub| blk: {
                const te_tag = self.tree.nodeTag(field.type_expr);
                if (te_tag == .ident or te_tag == .type_named) {
                    break :blk sub.contains(self.tree.tokenSlice(self.tree.nodeMainToken(field.type_expr)));
                }
                break :blk false;
            } else false;
            if (!is_substituted) type_idx = try self.safeWrapType(type_idx);
            try func_params.append(self.allocator, .{ .name = param_name, .type_idx = type_idx, .is_sending = field.flags.is_sending });
        }
        const ret_type = if (return_type_oi.unwrap()) |rt| try self.resolveTypeExpr(rt) else TypeRegistry.VOID;
        return try self.types.add(.{ .func = .{ .params = try self.allocator.dupe(types_mod.FuncParam, func_params.items), .return_type = ret_type } });
    }

    fn buildStructType(self: *Checker, name: []const u8, fields: SubRange) CheckError!TypeIndex {
        return self.buildStructTypeWithLayout(name, fields, .auto);
    }

    fn buildStructTypeWithLayout(self: *Checker, name: []const u8, fields_range: SubRange, layout: ast.StructLayout) CheckError!TypeIndex {
        var struct_fields = std.ArrayListUnmanaged(types_mod.StructField){};
        defer struct_fields.deinit(self.allocator);
        var offset: u32 = 0;
        var max_align: u32 = 1;
        const field_raw = self.tree.extraSlice(fields_range);
        for (field_raw) |raw_ei| {
            const field = self.tree.extraData(@enumFromInt(raw_ei), ast.Field);
            const field_name = self.tree.tokenSlice(field.name_token);
            const field_type = try self.resolveTypeExpr(field.type_expr);
            switch (layout) {
                .@"packed" => {
                    var bit_width: u8 = 0;
                    const te_tag = self.tree.nodeTag(field.type_expr);
                    if (te_tag == .type_named) {
                        const te_name = self.tree.tokenSlice(self.tree.nodeMainToken(field.type_expr));
                        if (types_mod.TypeRegistry.parseBitWidth(te_name)) |bw| {
                            if (bw != 8 and bw != 16 and bw != 32 and bw != 64) bit_width = bw;
                        }
                    }
                    try struct_fields.append(self.allocator, .{
                        .name = field_name,
                        .type_idx = field_type,
                        .offset = offset,
                        .bit_offset = 0,
                        .bit_width = bit_width,
                        .default_value = field.default_value,
                    });
                    if (bit_width == 0) offset += self.types.sizeOf(field_type);
                },
                .@"extern" => {
                    const field_align = self.types.alignmentOf(field_type);
                    if (field_align > max_align) max_align = field_align;
                    if (field_align > 0) offset = (offset + field_align - 1) & ~(field_align - 1);
                    try struct_fields.append(self.allocator, .{ .name = field_name, .type_idx = field_type, .offset = offset, .default_value = field.default_value });
                    offset += self.types.sizeOf(field_type);
                },
                .auto => {
                    const field_align = self.types.alignmentOf(field_type);
                    if (field_align > 0) offset = (offset + field_align - 1) & ~(field_align - 1);
                    try struct_fields.append(self.allocator, .{ .name = field_name, .type_idx = field_type, .offset = offset, .default_value = field.default_value });
                    offset += self.types.sizeOf(field_type);
                },
            }
        }
        var has_bitfields = false;
        for (struct_fields.items) |f| { if (f.bit_width > 0) { has_bitfields = true; break; } }
        var backing_int: TypeIndex = @enumFromInt(0);
        if (has_bitfields and layout == .@"packed") {
            var bit_pos: u32 = 0;
            for (struct_fields.items) |*f| {
                f.bit_offset = @intCast(bit_pos);
                if (f.bit_width > 0) { bit_pos += f.bit_width; } else {
                    f.bit_width = @intCast(self.types.sizeOf(f.type_idx) * 8);
                    bit_pos += f.bit_width;
                }
                f.offset = 0;
            }
            if (bit_pos <= 8) { offset = 1; backing_int = TypeRegistry.U8; }
            else if (bit_pos <= 16) { offset = 2; backing_int = TypeRegistry.U16; }
            else if (bit_pos <= 32) { offset = 4; backing_int = TypeRegistry.U32; }
            else if (bit_pos <= 64) { offset = 8; backing_int = TypeRegistry.U64; }
            else {
                if (field_raw.len > 0) {
                    const first_field = self.tree.extraData(@enumFromInt(field_raw[0]), ast.Field);
                    _ = first_field;
                }
                self.err.errorWithCode(self.tree.nodeSpan(@enumFromInt(0)).start, .e302, "packed struct total bit width exceeds 64");
                offset = 8; backing_int = TypeRegistry.U64;
            }
        }
        const alignment: u8 = switch (layout) {
            .@"packed" => if (has_bitfields) @intCast(offset) else 1,
            .@"extern" => @intCast(max_align),
            .auto => 8,
        };
        if (layout != .@"packed" and alignment > 1) {
            const a = @as(u32, alignment);
            offset = (offset + a - 1) & ~(a - 1);
        }
        return try self.types.add(.{ .struct_type = .{
            .name = name,
            .fields = try self.allocator.dupe(types_mod.StructField, struct_fields.items),
            .size = offset,
            .alignment = alignment,
            .layout = layout,
            .backing_int = backing_int,
        } });
    }

    fn buildEnumType(self: *Checker, idx: Index) CheckError!TypeIndex {
        const e = self.tree.enumDeclData(idx);
        var backing_type: TypeIndex = TypeRegistry.I32;
        if (e.backing_type.unwrap()) |bt| backing_type = try self.resolveTypeExpr(bt);
        var enum_variants = std.ArrayListUnmanaged(types_mod.EnumVariant){};
        defer enum_variants.deinit(self.allocator);
        var next_value: i64 = 0;
        const variant_raw = self.tree.extraSlice(e.variants);
        for (variant_raw) |raw_ei| {
            const ev = self.tree.extraData(@enumFromInt(raw_ei), ast.EnumVariant);
            const vname = self.tree.tokenSlice(ev.name_token);
            var value = next_value;
            if (ev.value.unwrap()) |val_node| {
                if (self.tree.nodeTag(val_node) == .literal_int)
                    value = std.fmt.parseInt(i64, self.tree.tokenSlice(self.tree.nodeMainToken(val_node)), 0) catch 0;
            }
            try enum_variants.append(self.allocator, .{ .name = vname, .value = value });
            next_value = value + 1;
        }
        return try self.types.add(.{ .enum_type = .{ .name = e.name, .backing_type = backing_type, .variants = try self.allocator.dupe(types_mod.EnumVariant, enum_variants.items) } });
    }

    fn buildUnionType(self: *Checker, idx: Index) CheckError!TypeIndex {
        const u = self.tree.unionDeclData(idx);
        var union_variants = std.ArrayListUnmanaged(types_mod.UnionVariant){};
        defer union_variants.deinit(self.allocator);
        const variant_raw = self.tree.extraSlice(u.variants);
        for (variant_raw) |raw_ei| {
            const uv = self.tree.extraData(@enumFromInt(raw_ei), ast.UnionVariant);
            const vname = self.tree.tokenSlice(uv.name_token);
            const payload_type = if (self.tree.nodeTag(uv.type_expr) != .bad_node) try self.resolveTypeExpr(uv.type_expr) else .invalid;
            try union_variants.append(self.allocator, .{ .name = vname, .payload_type = payload_type });
        }
        const tag_type: TypeIndex = if (variant_raw.len <= 256) TypeRegistry.U8 else TypeRegistry.U16;
        return try self.types.add(.{ .union_type = .{ .name = u.name, .variants = try self.allocator.dupe(types_mod.UnionVariant, union_variants.items), .tag_type = tag_type } });
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

    fn isUnionVariantRef(self: *Checker, expr_idx: Index, ut: types_mod.UnionType) bool {
        if (self.tree.nodeTag(expr_idx) != .field_access) return false;
        const fa_data = self.tree.nodeData(expr_idx);
        const fa_field = self.tree.tokenSlice(fa_data.node_and_token[1]);
        for (ut.variants) |v| {
            if (std.mem.eql(u8, v.name, fa_field)) return true;
        }
        return false;
    }

    fn isComparable(self: *Checker, a: TypeIndex, b: TypeIndex) bool {
        if (self.types.equal(a, b)) return true;
        const ta = self.types.get(a);
        const tb = self.types.get(b);
        if (types_mod.isNumeric(ta) and types_mod.isNumeric(tb)) return true;
        if (types_mod.isBool(ta) and types_mod.isBool(tb)) return true;
        if (ta == .slice and tb == .slice and ta.slice.elem == TypeRegistry.U8 and tb.slice.elem == TypeRegistry.U8) return true;
        if (ta == .optional and tb == .basic and tb.basic == .untyped_null) return true;
        if (tb == .optional and ta == .basic and ta.basic == .untyped_null) return true;
        if (ta == .pointer and tb == .basic and tb.basic == .untyped_null) return true;
        if (tb == .pointer and ta == .basic and ta.basic == .untyped_null) return true;
        if (ta == .enum_type and types_mod.isInteger(tb)) return true;
        if (tb == .enum_type and types_mod.isInteger(ta)) return true;
        if (ta == .optional and self.isComparable(ta.optional.elem, b)) return true;
        if (tb == .optional and self.isComparable(a, tb.optional.elem)) return true;
        return false;
    }

    // ---------------------------------------------------------------
    // Diagnostics (ported from compiler/ lines 4856-5162)
    // ---------------------------------------------------------------

    const MAX_EDIT_LEN = 64;

    fn editDistance(a: []const u8, b: []const u8) usize {
        if (a.len > MAX_EDIT_LEN or b.len > MAX_EDIT_LEN) return std.math.maxInt(usize);
        if (a.len == 0) return b.len;
        if (b.len == 0) return a.len;
        if (std.mem.eql(u8, a, b)) return 0;
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

    fn isUserVisibleName(name: []const u8) bool {
        if (name.len == 0) return false;
        if (std.mem.indexOfScalar(u8, name, '(') != null) return false;
        if (name.len >= 2 and name[0] == '_' and name[1] == '_') return false;
        return true;
    }

    fn findSimilarName(self: *Checker, name: []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        var scope_ptr: ?*const Scope = self.scope;
        while (scope_ptr) |s| {
            var it = s.symbols.iterator();
            while (it.next()) |entry| {
                const candidate = entry.key_ptr.*;
                if (std.mem.eql(u8, candidate, name)) continue;
                if (!isUserVisibleName(candidate)) continue;
                const d = editDistance(name, candidate);
                if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
            }
            scope_ptr = s.parent;
        }
        {
            var it = self.global_scope.symbols.iterator();
            while (it.next()) |entry| {
                const candidate = entry.key_ptr.*;
                if (std.mem.eql(u8, candidate, name)) continue;
                if (!isUserVisibleName(candidate)) continue;
                const d = editDistance(name, candidate);
                if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
            }
        }
        const builtins = [_][]const u8{ "print", "println", "eprint", "eprintln", "len", "append" };
        for (builtins) |candidate| {
            if (std.mem.eql(u8, candidate, name)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
        }
        return best;
    }

    fn findSimilarField(name: []const u8, fields: []const types_mod.StructField) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (fields) |fld| {
            if (std.mem.eql(u8, fld.name, name)) continue;
            const d = editDistance(name, fld.name);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = fld.name; }
        }
        return best;
    }

    fn findSimilarVariant(name: []const u8, variants: anytype) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (variants) |v| {
            if (std.mem.eql(u8, v.name, name)) continue;
            const d = editDistance(name, v.name);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = v.name; }
        }
        return best;
    }

    fn findSimilarType(self: *Checker, name: []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        var it = self.types.name_map.iterator();
        while (it.next()) |entry| {
            const candidate = entry.key_ptr.*;
            if (std.mem.eql(u8, candidate, name)) continue;
            if (!isUserVisibleName(candidate)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
        }
        var git = self.generics.generic_structs.iterator();
        while (git.next()) |entry| {
            const candidate = entry.key_ptr.*;
            if (std.mem.eql(u8, candidate, name)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
        }
        return best;
    }

    fn findSimilarTrait(self: *Checker, name: []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        var it = self.generics.trait_defs.iterator();
        while (it.next()) |entry| {
            const candidate = entry.key_ptr.*;
            if (std.mem.eql(u8, candidate, name)) continue;
            const d = editDistance(name, candidate);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = candidate; }
        }
        return best;
    }

    fn editDistSuggest(name: []const u8, candidates: []const []const u8) ?[]const u8 {
        var best: ?[]const u8 = null;
        var best_dist: usize = std.math.maxInt(usize);
        for (candidates) |c| {
            if (std.mem.eql(u8, c, name)) continue;
            const d = editDistance(name, c);
            if (d < best_dist and d <= 2 and d * 2 <= name.len) { best_dist = d; best = c; }
        }
        return best;
    }

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
