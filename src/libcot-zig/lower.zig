//! AST-to-IR lowering pass for the compact AST representation.
//!
//! Transforms type-checked compact AST (from parser.zig) into flat IR
//! (from ir.zig). This is a mechanical port of compiler/frontend/lower.zig
//! adapted for the data-oriented compact AST.
//!
//! Key differences from compiler/frontend/lower.zig:
//!   - AST nodes accessed via `tree.nodeTag(idx)` + `tree.fnDeclData(idx)` etc.
//!   - Node indices are `ast_mod.Index` (enum(u32)) not plain u32
//!   - null_node → `.none` on OptionalIndex
//!   - types.invalid_type → `.invalid` on TypeIndex

const std = @import("std");
const ast_mod = @import("ast.zig");
const ir = @import("cir").ir;
const types_mod = @import("foundation").types;
const source_mod = @import("foundation").source;
const errors_mod = @import("errors.zig");
const checker_mod = @import("checker.zig");
const comptime_mod = @import("comptime.zig");
const target_mod = @import("foundation").target;
const token_mod = @import("token.zig");

const Token = token_mod.Token;

const Allocator = std.mem.Allocator;
const Ast = ast_mod.Ast;
const Index = ast_mod.Index;
const OptionalIndex = ast_mod.OptionalIndex;
const TokenIndex = ast_mod.TokenIndex;
const Tag = ast_mod.Node.Tag;
const SubRange = ast_mod.SubRange;
const TypeIndex = types_mod.TypeIndex;
const TypeRegistry = types_mod.TypeRegistry;
const Span = source_mod.Span;
const Pos = source_mod.Pos;
const ErrorReporter = errors_mod.ErrorReporter;

// ============================================================================
// Minimal CleanupStack stub — will be replaced by arc_insertion.zig port
// ============================================================================

pub const CleanupKind = enum {
    release,
    unowned_release,
    weak_release,
    defer_expr,
    errdefer_expr,
    scope_destroy,
    end_borrow,
};

pub const Cleanup = struct {
    kind: CleanupKind,
    /// For release/unowned/weak: the IR node index of the value.
    /// For defer/errdefer: the AST node index (cast to u32).
    value: ir.NodeIndex,
    type_idx: TypeIndex,
    state: enum { active, disabled } = .active,
    local_idx: ?ir.LocalIdx = null,
    func_name: ?[]const u8 = null,

    pub fn init(kind: CleanupKind, value: ir.NodeIndex, type_idx: TypeIndex) Cleanup {
        return .{ .kind = kind, .value = value, .type_idx = type_idx };
    }

    pub fn initForLocal(kind: CleanupKind, value: ir.NodeIndex, type_idx: TypeIndex, local_idx: ir.LocalIdx) Cleanup {
        return .{ .kind = kind, .value = value, .type_idx = type_idx, .local_idx = local_idx };
    }

    pub fn initScopeDestroy(local_idx: ir.LocalIdx, type_idx: TypeIndex, func_name: []const u8) Cleanup {
        return .{ .kind = .scope_destroy, .value = 0, .type_idx = type_idx, .local_idx = local_idx, .func_name = func_name };
    }

    pub fn isActive(self: *const Cleanup) bool {
        return self.state == .active;
    }
};

pub const CleanupStack = struct {
    items: std.ArrayListUnmanaged(Cleanup),
    allocator: Allocator,

    pub fn init(allocator: Allocator) CleanupStack {
        return .{ .items = .{}, .allocator = allocator };
    }

    pub fn deinit(self: *CleanupStack) void {
        self.items.deinit(self.allocator);
    }

    pub fn clear(self: *CleanupStack) void {
        self.items.clearRetainingCapacity();
    }

    pub fn push(self: *CleanupStack, cleanup: Cleanup) !usize {
        const handle = self.items.items.len;
        try self.items.append(self.allocator, cleanup);
        return handle;
    }

    pub fn getScopeDepth(self: *const CleanupStack) usize {
        return self.items.items.len;
    }

    pub fn getActiveCleanups(self: *const CleanupStack) []const Cleanup {
        return self.items.items;
    }

    pub fn disableForLocal(self: *CleanupStack, local_idx: ir.LocalIdx) bool {
        var i = self.items.items.len;
        while (i > 0) {
            i -= 1;
            if (self.items.items[i].local_idx) |li| {
                if (li == local_idx and self.items.items[i].isActive()) {
                    self.items.items[i].state = .disabled;
                    return true;
                }
            }
        }
        return false;
    }

    pub fn hasCleanupForLocal(self: *const CleanupStack, local_idx: ir.LocalIdx) bool {
        for (self.items.items) |cleanup| {
            if (cleanup.local_idx) |li| {
                if (li == local_idx and cleanup.isActive()) return true;
            }
        }
        return false;
    }

    pub fn updateValueForLocal(self: *CleanupStack, local_idx: ir.LocalIdx, new_value: ir.NodeIndex) void {
        var i = self.items.items.len;
        while (i > 0) {
            i -= 1;
            if (self.items.items[i].local_idx) |li| {
                if (li == local_idx and self.items.items[i].isActive()) {
                    self.items.items[i].value = new_value;
                    return;
                }
            }
        }
    }

    pub fn findCleanupForLocal(self: *const CleanupStack, local_idx: ir.LocalIdx) ?usize {
        var i = self.items.items.len;
        while (i > 0) {
            i -= 1;
            if (self.items.items[i].local_idx) |li| {
                if (li == local_idx and self.items.items[i].isActive()) return i;
            }
        }
        return null;
    }
};

// ============================================================================
// Lowerer
// ============================================================================

pub const Lowerer = struct {
    allocator: Allocator,
    tree: *const Ast,
    type_reg: *TypeRegistry,
    err: *ErrorReporter,
    builder: ir.Builder,
    chk: *checker_mod.Checker,
    current_func: ?*ir.FuncBuilder = null,
    temp_counter: u32 = 0,
    loop_stack: std.ArrayListUnmanaged(LoopContext),
    labeled_block_stack: std.ArrayListUnmanaged(LabeledBlockContext),
    cleanup_stack: CleanupStack,
    const_values: std.StringHashMap(i64),
    float_const_values: std.StringHashMap(f64),
    float_const_types: std.StringHashMap(TypeIndex),
    test_mode: bool = false,
    fail_fast: bool = false,
    test_names: std.ArrayListUnmanaged([]const u8),
    test_display_names: std.ArrayListUnmanaged([]const u8),
    current_test_name: ?[]const u8 = null,
    bench_mode: bool = false,
    bench_names: std.ArrayListUnmanaged([]const u8),
    bench_display_names: std.ArrayListUnmanaged([]const u8),
    closure_counter: u32 = 0,
    lowered_generics: std.StringHashMap(void),
    type_substitution: ?std.StringHashMap(TypeIndex) = null,
    is_inlinable_body: bool = false,
    indirect_t_params: std.StringHashMap([]const u8),
    target: target_mod.Target = target_mod.Target.native(),
    global_error_table: std.StringHashMap(i64),
    next_error_idx: i64 = 0,
    release_mode: bool = false,
    current_switch_enum_type: TypeIndex = .invalid,
    comptime_value_vars: std.StringHashMap(comptime_mod.ComptimeValue),
    global_comptime_values: std.StringHashMap(comptime_mod.ComptimeValue),
    weak_locals: std.AutoHashMapUnmanaged(ir.LocalIdx, void) = .{},
    pending_auto_deinits: std.ArrayListUnmanaged([]const u8) = .{},
    pending_global_inits: std.ArrayListUnmanaged(GlobalInit) = .{},
    module_name: []const u8 = "",
    tree_module_map: ?*const std.AutoHashMap(*const Ast, []const u8) = null,

    pub const Error = error{OutOfMemory};

    const LoopContext = struct {
        cond_block: ir.BlockIndex,
        exit_block: ir.BlockIndex,
        cleanup_depth: usize,
        label: ?[]const u8 = null,
    };

    const LabeledBlockContext = struct {
        label: []const u8,
        exit_block: ir.BlockIndex,
        result_local: ir.LocalIdx,
        result_type: TypeIndex,
        cleanup_depth: usize,
    };

    const GlobalInit = struct {
        global_idx: ir.GlobalIdx,
        value_node: Index,
        type_idx: TypeIndex,
        name: []const u8,
        span: Span,
    };

    // ========================================================================
    // Init / Deinit
    // ========================================================================

    pub fn init(
        allocator: Allocator,
        tree: *const Ast,
        type_reg: *TypeRegistry,
        err_reporter: *ErrorReporter,
        chk: *checker_mod.Checker,
        tgt: target_mod.Target,
    ) Lowerer {
        return initWithBuilder(allocator, tree, type_reg, err_reporter, chk, ir.Builder.init(allocator, type_reg), tgt);
    }

    pub fn initWithBuilder(
        allocator: Allocator,
        tree: *const Ast,
        type_reg: *TypeRegistry,
        err_reporter: *ErrorReporter,
        chk: *checker_mod.Checker,
        builder: ir.Builder,
        tgt: target_mod.Target,
    ) Lowerer {
        return .{
            .allocator = allocator,
            .tree = tree,
            .type_reg = type_reg,
            .err = err_reporter,
            .builder = builder,
            .chk = chk,
            .loop_stack = .{},
            .labeled_block_stack = .{},
            .cleanup_stack = CleanupStack.init(allocator),
            .const_values = std.StringHashMap(i64).init(allocator),
            .float_const_values = std.StringHashMap(f64).init(allocator),
            .float_const_types = std.StringHashMap(TypeIndex).init(allocator),
            .test_names = .{},
            .test_display_names = .{},
            .bench_names = .{},
            .bench_display_names = .{},
            .lowered_generics = std.StringHashMap(void).init(allocator),
            .indirect_t_params = std.StringHashMap([]const u8).init(allocator),
            .target = tgt,
            .global_error_table = std.StringHashMap(i64).init(allocator),
            .comptime_value_vars = std.StringHashMap(comptime_mod.ComptimeValue).init(allocator),
            .global_comptime_values = std.StringHashMap(comptime_mod.ComptimeValue).init(allocator),
        };
    }

    pub fn setTestMode(self: *Lowerer, enabled: bool) void {
        self.test_mode = enabled;
    }
    pub fn setFailFast(self: *Lowerer, enabled: bool) void {
        self.fail_fast = enabled;
    }
    pub fn addTestName(self: *Lowerer, name: []const u8) !void {
        try self.test_names.append(self.allocator, name);
    }
    pub fn addTestDisplayName(self: *Lowerer, name: []const u8) !void {
        try self.test_display_names.append(self.allocator, name);
    }
    pub fn getTestNames(self: *const Lowerer) []const []const u8 {
        return self.test_names.items;
    }
    pub fn getTestDisplayNames(self: *const Lowerer) []const []const u8 {
        return self.test_display_names.items;
    }
    pub fn setBenchMode(self: *Lowerer, enabled: bool) void {
        self.bench_mode = enabled;
    }
    pub fn addBenchName(self: *Lowerer, name: []const u8) !void {
        try self.bench_names.append(self.allocator, name);
    }
    pub fn addBenchDisplayName(self: *Lowerer, name: []const u8) !void {
        try self.bench_display_names.append(self.allocator, name);
    }
    pub fn getBenchNames(self: *const Lowerer) []const []const u8 {
        return self.bench_names.items;
    }
    pub fn getBenchDisplayNames(self: *const Lowerer) []const []const u8 {
        return self.bench_display_names.items;
    }

    pub fn deinit(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
        self.labeled_block_stack.deinit(self.allocator);
        self.cleanup_stack.deinit();
        self.const_values.deinit();
        self.float_const_values.deinit();
        self.float_const_types.deinit();
        self.test_names.deinit(self.allocator);
        self.test_display_names.deinit(self.allocator);
        self.bench_names.deinit(self.allocator);
        self.bench_display_names.deinit(self.allocator);
        self.lowered_generics.deinit();
        self.comptime_value_vars.deinit();
        self.global_comptime_values.deinit();
        self.weak_locals.deinit(self.allocator);
        self.pending_auto_deinits.deinit(self.allocator);
        self.pending_global_inits.deinit(self.allocator);
        self.builder.deinit();
    }

    pub fn deinitWithoutBuilder(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
        self.labeled_block_stack.deinit(self.allocator);
        self.const_values.deinit();
        self.float_const_values.deinit();
        self.float_const_types.deinit();
        self.test_names.deinit(self.allocator);
        self.test_display_names.deinit(self.allocator);
        self.bench_names.deinit(self.allocator);
        self.bench_display_names.deinit(self.allocator);
        self.comptime_value_vars.deinit();
        self.global_comptime_values.deinit();
        self.weak_locals.deinit(self.allocator);
        self.pending_auto_deinits.deinit(self.allocator);
        self.pending_global_inits.deinit(self.allocator);
    }

    // ========================================================================
    // needsSret / isPtrLikeOptional / isGcRefOptional
    // ========================================================================

    /// Zig SRET pattern: functions returning structs/tuples > 8 bytes use a hidden
    /// first parameter for the return value.
    fn needsSret(self: *const Lowerer, type_idx: TypeIndex) bool {
        const info = self.type_reg.get(type_idx);
        if ((info == .struct_type or info == .tuple or info == .union_type) and self.type_reg.sizeOf(type_idx) > 8) {
            if (self.target.isWasmGC() and info == .struct_type) return false;
            return true;
        }
        if (info == .optional and !self.isPtrLikeOptional(type_idx))
            return true;
        return false;
    }

    fn isPtrLikeOptional(self: *const Lowerer, type_idx: TypeIndex) bool {
        const info = self.type_reg.get(type_idx);
        if (info != .optional) return false;
        const elem_info = self.type_reg.get(info.optional.elem);
        if (elem_info == .pointer and !elem_info.pointer.managed) return true;
        if (self.target.isWasmGC() and (elem_info == .struct_type or elem_info == .union_type)) return true;
        return false;
    }

    fn isGcRefOptional(self: *const Lowerer, type_idx: TypeIndex) bool {
        const info = self.type_reg.get(type_idx);
        if (info != .optional) return false;
        if (!self.target.isWasmGC()) return false;
        const elem_info = self.type_reg.get(info.optional.elem);
        return elem_info == .struct_type or elem_info == .union_type;
    }

    // ========================================================================
    // Name qualification (Go LinkFuncName pattern)
    // ========================================================================

    fn qualifyName(self: *Lowerer, bare: []const u8) ![]const u8 {
        if (self.module_name.len == 0) return bare;
        return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ self.module_name, bare });
    }

    fn shouldSkipQualification(name: []const u8) bool {
        if (std.mem.eql(u8, name, "main")) return true;
        if (std.mem.startsWith(u8, name, "__cot_init")) return true;
        if (std.mem.startsWith(u8, name, "__test_")) return true;
        if (std.mem.startsWith(u8, name, "__bench_")) return true;
        return false;
    }

    fn resolveCallName(self: *Lowerer, bare: []const u8) ![]const u8 {
        if (self.module_name.len == 0) return bare;
        if (shouldSkipQualification(bare)) return bare;
        if (self.chk.scope.lookup(bare)) |sym| {
            if (sym.is_extern or sym.is_export) return bare;
            if (sym.source_tree) |st| {
                if (self.tree_module_map) |map| {
                    if (map.get(st)) |target_module| {
                        return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ target_module, bare });
                    }
                }
            }
        }
        return self.qualifyName(bare);
    }

    fn qualifyTypeName(self: *Lowerer, type_name: []const u8) ![]const u8 {
        if (self.module_name.len == 0) return type_name;
        if (self.chk.scope.lookup(type_name)) |sym| {
            if (sym.source_tree) |st| {
                if (self.tree_module_map) |map| {
                    if (map.get(st)) |target_module| {
                        return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ target_module, type_name });
                    }
                }
            }
        }
        return self.qualifyName(type_name);
    }

    // ========================================================================
    // tempName / nextTemp
    // ========================================================================

    fn tempName(self: *Lowerer, prefix: []const u8) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.temp_counter });
        self.temp_counter += 1;
        return name;
    }

    fn nextTemp(self: *Lowerer) u32 {
        const t = self.temp_counter;
        self.temp_counter += 1;
        return t;
    }

    // ========================================================================
    // resolveLocal
    // ========================================================================

    fn resolveLocal(self: *Lowerer, name: []const u8) ?ir.LocalIdx {
        const fb = self.current_func orelse return null;
        return fb.lookupLocal(name);
    }

    // ========================================================================
    // getSpan — compute span from AST node
    // ========================================================================

    fn getSpan(self: *const Lowerer, idx: Index) Span {
        return self.tree.nodeSpan(idx);
    }

    // ========================================================================
    // emitConst helpers
    // ========================================================================

    fn emitConst(self: *Lowerer, value: i64, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        return fb.emitConstInt(value, type_idx, span);
    }

    fn emitConstFloat(self: *Lowerer, value: f64, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        return fb.emitConstFloat(value, type_idx, span);
    }

    fn emitConstBool(self: *Lowerer, value: bool, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        return fb.emitConstBool(value, span);
    }

    fn emitConstNull(self: *Lowerer, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        return fb.emitConstNull(type_idx, span);
    }

    fn emitConstString(self: *Lowerer, str: []const u8, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const copied = try self.allocator.dupe(u8, str);
        const str_idx = try fb.addStringLiteral(copied);
        return fb.emitConstSlice(str_idx, span);
    }

    // ========================================================================
    // Entry point: lower / lowerToBuilder
    // ========================================================================

    pub fn lower(self: *Lowerer) !ir.IR {
        try self.lowerToBuilder();
        return try self.builder.getIR();
    }

    pub fn lowerToBuilder(self: *Lowerer) !void {
        // Get root declarations from the AST.
        // Root node (index 0) has tag .root and data is an extra_range of decl indices.
        const root_data = self.tree.nodeData(@enumFromInt(0));
        const decl_range = root_data.extra_range;
        const decl_indices = self.tree.extraNodes(decl_range);

        for (decl_indices) |decl_idx| {
            try self.lowerDecl(decl_idx);
        }
    }

    // ========================================================================
    // Declaration lowering
    // ========================================================================

    fn lowerDecl(self: *Lowerer, idx: Index) !void {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .fn_decl => try self.lowerFnDecl(idx),
            .fn_decl_extern => {}, // extern fns have no body
            .var_decl => try self.lowerGlobalVarDecl(idx),
            .struct_decl => try self.lowerStructDecl(idx),
            .impl_block => try self.lowerImplBlock(idx),
            .impl_trait => try self.lowerImplTraitBlock(idx),
            .test_decl => {
                if (self.test_mode) try self.lowerTestDecl(idx);
            },
            .bench_decl => {
                if (self.bench_mode) try self.lowerBenchDecl(idx);
            },
            .enum_decl => {
                const ed = self.tree.enumDeclData(idx);
                const nested_decl_slice = self.tree.extraNodes(ed.nested_decls);
                for (nested_decl_slice) |nested_idx| {
                    const nested_tag = self.tree.nodeTag(nested_idx);
                    if (nested_tag == .fn_decl) {
                        const nested_fn = self.tree.fnDeclData(nested_idx);
                        const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ ed.name, nested_fn.name });
                        const is_dtor = std.mem.eql(u8, nested_fn.name, "deinit");
                        try self.lowerMethodWithName(nested_idx, synth_name, is_dtor);
                    }
                }
            },
            .trait_decl, .union_decl, .type_alias, .type_alias_distinct,
            .import_decl, .error_set_decl, .unchecked_sendable,
            => {},
            else => {},
        }
    }

    fn lowerFnDecl(self: *Lowerer, idx: Index) !void {
        const fn_decl = self.tree.fnDeclData(idx);
        if (fn_decl.is_extern) return;
        if (fn_decl.type_params.len() > 0) return;
        if ((self.test_mode or self.bench_mode) and std.mem.eql(u8, fn_decl.name, "main")) return;

        const saved_cleanup = self.cleanup_stack;
        const saved_loop_stack_len = self.loop_stack.items.len;

        const return_type = if (fn_decl.return_type.unwrap()) |rt|
            self.resolveTypeNode(rt)
        else
            TypeRegistry.VOID;

        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        const span = self.getSpan(idx);

        const link_name = if (fn_decl.flags.is_export or std.mem.eql(u8, fn_decl.name, "main"))
            fn_decl.name
        else
            try self.qualifyName(fn_decl.name);

        self.builder.startFunc(link_name, TypeRegistry.VOID, wasm_return_type, span);
        if (self.builder.func()) |fb| {
            if (uses_sret) fb.sret_return_type = return_type;
            fb.is_destructor = std.mem.endsWith(u8, fn_decl.name, "_deinit");
            fb.is_export = fn_decl.flags.is_export;
            self.current_func = fb;
            self.cleanup_stack.clear();
            self.comptime_value_vars.clearRetainingCapacity();
            self.weak_locals.clearRetainingCapacity();

            // SRET: add hidden first parameter
            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }

            // Add function parameters
            const param_indices = self.tree.extraSlice(fn_decl.params);
            for (param_indices) |param_raw| {
                const param_idx: Index = @enumFromInt(param_raw);
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
            }

            // Initialize globals in main()
            if (std.mem.eql(u8, fn_decl.name, "main")) {
                var no_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__cot_init_globals", &no_args, false, TypeRegistry.VOID, span);
                _ = try fb.emitCall("__cot_init_metadata", &no_args, false, TypeRegistry.VOID, span);
            }

            if (fn_decl.body.unwrap()) |body| {
                _ = try self.lowerBlockNode(body);

                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, span);
                } else if (fb.needsTerminator()) {
                    const rti = self.type_reg.get(return_type);
                    if (rti == .error_union and rti.error_union.elem == TypeRegistry.VOID) {
                        try self.emitCleanups(0);
                        try self.emitErrorVoidSuccessReturn(fb, return_type, span);
                    }
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
        self.current_func = self.builder.func();
        self.cleanup_stack = saved_cleanup;
        self.loop_stack.shrinkRetainingCapacity(saved_loop_stack_len);
    }

    fn lowerGlobalVarDecl(self: *Lowerer, idx: Index) !void {
        const vd = self.tree.varDeclData(idx);
        var type_idx = TypeRegistry.VOID;
        if (vd.type_expr.unwrap()) |te| {
            type_idx = self.resolveTypeNode(te);
        } else if (vd.value.unwrap()) |ve| {
            type_idx = self.inferExprType(ve);
        }
        if (vd.is_const) {
            if (self.chk.scope.lookup(vd.name)) |sym| {
                if (sym.const_value) |value| {
                    try self.const_values.put(vd.name, value);
                    return;
                }
                if (sym.float_const_value) |fvalue| {
                    try self.float_const_values.put(vd.name, fvalue);
                    try self.float_const_types.put(vd.name, sym.type_idx);
                    return;
                }
            }
        }
        const type_size: u32 = @intCast(self.type_reg.sizeOf(type_idx));
        const span = self.getSpan(idx);
        try self.builder.addGlobal(ir.Global.initWithSize(vd.name, type_idx, vd.is_const, span, type_size));
        if (vd.value.unwrap()) |value_idx| {
            const ti = self.type_reg.get(type_idx);
            const is_type_alias = (ti == .error_set);
            if (!is_type_alias and type_idx != TypeRegistry.VOID) {
                try self.pending_global_inits.append(self.allocator, .{
                    .global_idx = @as(ir.GlobalIdx, @intCast(self.builder.globals.items.len - 1)),
                    .value_node = value_idx,
                    .type_idx = type_idx,
                    .name = vd.name,
                    .span = span,
                });
            }
        }
    }

    fn lowerStructDecl(self: *Lowerer, idx: Index) !void {
        const sd = self.tree.structDeclData(idx);
        if (sd.type_params.len() > 0) return;
        const struct_type_idx = self.type_reg.lookupByName(sd.name) orelse TypeRegistry.VOID;
        const span = self.getSpan(idx);
        try self.builder.addStruct(.{ .name = sd.name, .type_idx = struct_type_idx, .span = span });

        const nested_slice = self.tree.extraNodes(sd.nested_decls);
        for (nested_slice) |nested_idx| {
            const nested_tag = self.tree.nodeTag(nested_idx);
            switch (nested_tag) {
                .struct_decl => {
                    const ns = self.tree.structDeclData(nested_idx);
                    const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ sd.name, ns.name });
                    const ns_type = self.type_reg.lookupByName(qualified) orelse TypeRegistry.VOID;
                    try self.builder.addStruct(.{ .name = qualified, .type_idx = ns_type, .span = self.getSpan(nested_idx) });
                },
                .fn_decl => {
                    const fd = self.tree.fnDeclData(nested_idx);
                    const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ sd.name, fd.name });
                    const is_dtor = std.mem.eql(u8, fd.name, "deinit");
                    if (self.current_func != null) {
                        const saved_builder_func = self.builder.current_func;
                        const saved_current_func = self.current_func;
                        const saved_cleanup_items = self.cleanup_stack.items;
                        self.cleanup_stack.items = .{};
                        try self.lowerMethodWithName(nested_idx, synth_name, is_dtor);
                        self.cleanup_stack.items.deinit(self.cleanup_stack.allocator);
                        self.cleanup_stack.items = saved_cleanup_items;
                        self.builder.current_func = saved_builder_func;
                        self.current_func = saved_current_func;
                    } else {
                        try self.lowerMethodWithName(nested_idx, synth_name, is_dtor);
                    }
                },
                else => {},
            }
        }
    }

    fn lowerImplBlock(self: *Lowerer, idx: Index) !void {
        const ib = self.tree.implBlockData(idx);
        if (ib.type_params.len() > 0) return;
        const method_slice = self.tree.extraNodes(ib.methods);
        for (method_slice) |method_idx| {
            const method_tag = self.tree.nodeTag(method_idx);
            if (method_tag == .fn_decl) {
                const fd = self.tree.fnDeclData(method_idx);
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ ib.type_name, fd.name });
                const is_dtor = std.mem.eql(u8, fd.name, "deinit");
                try self.lowerMethodWithName(method_idx, synth_name, is_dtor);
            }
        }
    }

    fn lowerImplTraitBlock(self: *Lowerer, idx: Index) !void {
        const it = self.tree.implTraitData(idx);
        const method_slice = self.tree.extraNodes(it.methods);
        for (method_slice) |method_idx| {
            const method_tag = self.tree.nodeTag(method_idx);
            if (method_tag == .fn_decl) {
                const fd = self.tree.fnDeclData(method_idx);
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ it.target_type, fd.name });
                const is_dtor = std.mem.eql(u8, fd.name, "deinit");
                try self.lowerMethodWithName(method_idx, synth_name, is_dtor);
            }
        }
    }

    fn lowerMethodWithName(self: *Lowerer, idx: Index, synth_name: []const u8, is_destructor: bool) !void {
        const fn_decl = self.tree.fnDeclData(idx);
        const return_type = if (fn_decl.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        const method_link_name = try self.qualifyName(synth_name);
        const span = self.getSpan(idx);
        self.builder.startFunc(method_link_name, TypeRegistry.VOID, wasm_return_type, span);
        if (self.builder.func()) |fb| {
            fb.is_destructor = is_destructor;
            if (uses_sret) fb.sret_return_type = return_type;
            self.current_func = fb;
            self.cleanup_stack.clear();
            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }
            const param_indices = self.tree.extraSlice(fn_decl.params);
            for (param_indices) |param_raw| {
                const param_idx: Index = @enumFromInt(param_raw);
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
            }
            if (fn_decl.body.unwrap()) |body| {
                _ = try self.lowerBlockNode(body);

                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, span);
                } else if (fb.needsTerminator()) {
                    const rti3 = self.type_reg.get(return_type);
                    if (rti3 == .error_union and rti3.error_union.elem == TypeRegistry.VOID) {
                        try self.emitCleanups(0);
                        try self.emitErrorVoidSuccessReturn(fb, return_type, span);
                    }
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerTestDecl(self: *Lowerer, idx: Index) !void {
        const data = self.tree.nodeData(idx);
        const name_token = data.token_and_node[0];
        const body_idx: Index = data.token_and_node[1];
        const display_name = self.tree.tokenSlice(name_token);
        const test_name = try self.sanitizeTestName(display_name);
        const qualified_test = try self.qualifyName(test_name);
        try self.test_names.append(self.allocator, qualified_test);
        try self.test_display_names.append(self.allocator, display_name);
        const span = self.getSpan(idx);
        self.builder.startFunc(qualified_test, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.current_test_name = display_name;
            self.cleanup_stack.clear();
            self.comptime_value_vars.clearRetainingCapacity();
            self.weak_locals.clearRetainingCapacity();
            _ = try self.lowerBlockNode(body_idx);
            if (fb.needsTerminator()) {
                try self.emitCleanups(0);
                const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                _ = try fb.emitRet(tag_zero, span);
            }
            self.current_test_name = null;
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerBenchDecl(self: *Lowerer, idx: Index) !void {
        const data = self.tree.nodeData(idx);
        const name_token = data.token_and_node[0];
        const body_idx: Index = data.token_and_node[1];
        const display_name = self.tree.tokenSlice(name_token);
        const bench_name = try self.sanitizeBenchName(display_name);
        const qualified_bench = try self.qualifyName(bench_name);
        try self.bench_names.append(self.allocator, qualified_bench);
        try self.bench_display_names.append(self.allocator, display_name);
        const span = self.getSpan(idx);
        self.builder.startFunc(qualified_bench, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            _ = try self.lowerBlockNode(body_idx);
            if (fb.needsTerminator()) {
                try self.emitCleanups(0);
                const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                _ = try fb.emitRet(zero, span);
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn sanitizeTestName(self: *Lowerer, name: []const u8) ![]const u8 {
        var result = try self.allocator.alloc(u8, 5 + name.len);
        @memcpy(result[0..5], "test_");
        for (name, 0..) |c, i| {
            result[5 + i] = if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')) c else '_';
        }
        return result;
    }

    fn sanitizeBenchName(self: *Lowerer, name: []const u8) ![]const u8 {
        var result = try self.allocator.alloc(u8, 6 + name.len);
        @memcpy(result[0..6], "bench_");
        for (name, 0..) |c, i| {
            result[6 + i] = if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')) c else '_';
        }
        return result;
    }

    fn emitErrorVoidSuccessReturn(self: *Lowerer, fb: *ir.FuncBuilder, ret_type: TypeIndex, span: Span) !void {
        _ = self;
        const eu_size: u32 = 16; // error union = tag(8) + payload(8)
        const tmp_local = try fb.addLocalWithSize("__ret_eu", ret_type, false, eu_size);
        const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, span);
        const eu_ptr = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
        _ = try fb.emitRet(eu_ptr, span);
    }

    // ========================================================================
    // Statement lowering
    // ========================================================================

    fn lowerBlockNode(self: *Lowerer, idx: Index) Error!bool {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            // Statement tags
            .return_expr, .return_void,
            .var_local, .const_local,
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div,
            .assign_mod, .assign_bit_and, .assign_bit_or, .assign_bit_xor,
            .if_stmt_simple, .if_stmt,
            .while_stmt,
            .for_stmt,
            .block_stmt,
            .break_plain, .break_expr,
            .continue_plain, .continue_labeled,
            .defer_stmt, .errdefer_stmt,
            .expr_stmt,
            .async_let,
            .destructure,
            => return try self.lowerStmt(idx),

            // Block expressions
            .block_one, .block_two, .block => {
                const fb = self.current_func orelse return false;
                const scope_depth = fb.markScopeEntry();
                var terminated = false;
                // TODO: iterate block statements via compact data
                _ = &terminated;
                fb.restoreScope(scope_depth);
                return terminated;
            },

            // Other expressions — lower as expression statement
            else => {
                _ = try self.lowerExprNode(idx);
                return false;
            },
        }
    }

    fn lowerStmt(self: *Lowerer, idx: Index) Error!bool {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .return_expr, .return_void => {
                try self.lowerReturn(idx);
                return true;
            },
            .var_local, .const_local => {
                try self.lowerLocalVarDecl(idx);
                return false;
            },
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div,
            .assign_mod, .assign_bit_and, .assign_bit_or, .assign_bit_xor,
            => {
                try self.lowerAssign(idx);
                return false;
            },
            .if_stmt_simple, .if_stmt => {
                return try self.lowerIfStmt(idx);
            },
            .while_stmt => {
                try self.lowerWhileStmt(idx);
                return false;
            },
            .for_stmt => {
                try self.lowerForStmt(idx);
                return false;
            },
            .block_stmt => {
                return try self.lowerBlockStmt(idx);
            },
            .break_plain => {
                try self.lowerBreak(null, null);
                return true;
            },
            .break_expr => {
                const data = self.tree.nodeData(idx);
                const bd = self.tree.extraData(data.node_and_extra[1], ast_mod.BreakData);
                const label = if (bd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
                const value = bd.value.unwrap();
                try self.lowerBreak(label, value);
                return true;
            },
            .continue_plain => {
                try self.lowerContinue(null);
                return true;
            },
            .continue_labeled => {
                const data = self.tree.nodeData(idx);
                const label_tok = data.token;
                const label = self.tree.tokenSlice(label_tok);
                try self.lowerContinue(label);
                return true;
            },
            .defer_stmt => {
                try self.lowerDefer(idx, false);
                return false;
            },
            .errdefer_stmt => {
                try self.lowerDefer(idx, true);
                return false;
            },
            .expr_stmt => {
                const data = self.tree.nodeData(idx);
                const expr_idx = data.node;
                _ = try self.lowerExprNode(expr_idx);
                return false;
            },
            .async_let => {
                // TODO: async let lowering
                return false;
            },
            .destructure => {
                try self.lowerDestructureStmt(idx);
                return false;
            },
            else => return false,
        }
    }

    fn lowerReturn(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const tag = self.tree.nodeTag(idx);
        const span = self.getSpan(idx);

        if (tag == .return_void) {
            // Implicit return from error!void functions: return success error union (tag=0)
            const ret_type = fb.return_type;
            const ret_type_info = self.type_reg.get(ret_type);
            if (ret_type_info == .error_union) {
                const eu_size = self.type_reg.sizeOf(ret_type);
                const tmp_local = try fb.addLocalWithSize("__ret_eu", ret_type, false, eu_size);
                const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, span);
                const eu_ptr = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
                try self.emitCleanups(0);
                _ = try fb.emitRet(eu_ptr, span);
                return;
            }
            try self.emitCleanups(0);
            _ = try fb.emitRet(null, span);
            return;
        }

        // return_expr: data is the value node
        const data = self.tree.nodeData(idx);
        const value_idx = data.node;

        // Track whether this return is an error path (for errdefer)
        var is_error_return = false;
        var value_node: ?ir.NodeIndex = null;

        // Check if value is an error literal
        const value_tag = self.tree.nodeTag(value_idx);
        const is_error_literal = (value_tag == .error_literal);
        const ret_type = fb.return_type;
        const ret_type_info = self.type_reg.get(ret_type);
        const is_error_union_fn = ret_type_info == .error_union;

        if (is_error_literal and is_error_union_fn) {
            // error.X in error union function
            is_error_return = true;
            const lowered = try self.lowerExprNode(value_idx);
            if (lowered != ir.null_node) value_node = lowered;
        } else if (is_error_union_fn) {
            // Success value in error union function — wrap as tag=0, payload=value
            const lowered = try self.lowerExprNode(value_idx);
            if (lowered != ir.null_node) {
                const eu_size = self.type_reg.sizeOf(ret_type);
                const tmp_local = try fb.addLocalWithSize("__ret_eu", ret_type, false, eu_size);
                const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, span);
                // Check if payload is compound (string = ptr+len)
                const eu_elem = ret_type_info.error_union.elem;
                if (eu_elem == TypeRegistry.STRING) {
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const ptr_val = try fb.emitSlicePtr(lowered, ptr_type, span);
                    const len_val = try fb.emitSliceLen(lowered, span);
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, ptr_val, span);
                    _ = try fb.emitStoreLocalField(tmp_local, 2, 16, len_val, span);
                } else {
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, lowered, span);
                }
                value_node = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
            }
        } else if (fb.sret_return_type) |sret_type| {
            const sret_info = self.type_reg.get(sret_type);
            if (sret_info == .optional and !self.isPtrLikeOptional(sret_type)) {
                // Compound optional return: write tag+payload to __sret buffer
                const sret_idx_opt = fb.lookupLocal("__sret").?;
                const sret_ptr_opt = try fb.emitLoadLocal(sret_idx_opt, TypeRegistry.I64, span);

                // Detect null literal return
                const is_null_return = (value_tag == .literal_null);

                if (is_null_return) {
                    const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                    _ = try fb.emitStoreField(sret_ptr_opt, 0, 0, tag_zero, span);
                } else {
                    const lowered = try self.lowerExprNode(value_idx);
                    if (lowered != ir.null_node) {
                        const ret_expr_type = self.inferExprType(value_idx);
                        const ret_expr_info = self.type_reg.get(ret_expr_type);
                        if (ret_expr_info == .optional) {
                            // Already ?T — forward compound (copy all words)
                            const sret_size = self.type_reg.sizeOf(sret_type);
                            const num_words: u32 = sret_size / 8;
                            const tmp_local = try fb.addLocalWithSize("__ret_opt_src", sret_type, false, sret_size);
                            _ = try fb.emitStoreLocal(tmp_local, lowered, span);
                            for (0..num_words) |i| {
                                const offset: i64 = @intCast(i * 8);
                                const field = try fb.emitFieldLocal(tmp_local, @intCast(i), offset, TypeRegistry.I64, span);
                                _ = try fb.emitStoreField(sret_ptr_opt, @intCast(i), offset, field, span);
                            }
                        } else {
                            // Plain T → wrap as tag=1, payload=value
                            const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                            _ = try fb.emitStoreField(sret_ptr_opt, 0, 0, tag_one, span);
                            _ = try fb.emitStoreField(sret_ptr_opt, 1, 8, lowered, span);
                        }
                    }
                }

                // Forward ownership before cleanup
                if (value_tag == .ident) {
                    const name = self.tree.tokenSlice(self.tree.nodeMainToken(value_idx));
                    if (fb.lookupLocal(name)) |local_idx| {
                        _ = self.cleanup_stack.disableForLocal(local_idx);
                    }
                }

                if (self.hasDeferCleanups()) {
                    try self.emitCleanups(0);
                }
                _ = try fb.emitRet(null, span); // void return — SRET
                return;
            }

            // SRET: write return value to caller-allocated buffer via __sret pointer
            const sret_size = self.type_reg.sizeOf(sret_type);
            const sret_idx = fb.lookupLocal("__sret").?;
            const sret_ptr = try fb.emitLoadLocal(sret_idx, TypeRegistry.I64, span);
            const num_words = sret_size / 8;

            // Check if return expr is an ident (for local field copy)
            if (value_tag == .ident) {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(value_idx));
                if (fb.lookupLocal(name)) |src_local| {
                    // Swift ensurePlusOne: retain ARC fields before copy
                    if (!self.target.isWasm() and self.type_reg.couldBeARC(sret_type)) {
                        const src_val = try fb.emitLoadLocal(src_local, sret_type, span);
                        _ = try self.emitCopyValue(fb, src_val, sret_type, span);
                    }
                    for (0..num_words) |i| {
                        const offset: i64 = @intCast(i * 8);
                        const field = try fb.emitFieldLocal(src_local, @intCast(i), offset, TypeRegistry.I64, span);
                        _ = try fb.emitStoreField(sret_ptr, @intCast(i), offset, field, span);
                    }
                    _ = self.cleanup_stack.disableForLocal(src_local);
                    if (self.hasDeferCleanups()) {
                        try self.emitCleanups(0);
                    }
                    _ = try fb.emitRet(null, span);
                    return;
                }
            }

            {
                const lowered2 = try self.lowerExprNode(value_idx);
                if (lowered2 != ir.null_node) {
                    if (!self.target.isWasm() and self.type_reg.couldBeARC(sret_type)) {
                        _ = try self.emitCopyValue(fb, lowered2, sret_type, span);
                    }
                    const tmp_local = try fb.addLocalWithSize("__ret_sret_src", sret_type, false, sret_size);
                    _ = try fb.emitStoreLocal(tmp_local, lowered2, span);
                    for (0..num_words) |i| {
                        const offset: i64 = @intCast(i * 8);
                        const field = try fb.emitFieldLocal(tmp_local, @intCast(i), offset, TypeRegistry.I64, span);
                        _ = try fb.emitStoreField(sret_ptr, @intCast(i), offset, field, span);
                    }
                }
            }

            // Forward ownership before cleanup
            if (value_tag == .ident) {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(value_idx));
                if (fb.lookupLocal(name)) |local_idx| {
                    _ = self.cleanup_stack.disableForLocal(local_idx);
                }
            }

            if (self.hasDeferCleanups()) {
                try self.emitCleanups(0);
            }
            _ = try fb.emitRet(null, span);
            return;
        } else {
            // Normal return path
            const lowered = try self.lowerExprNode(value_idx);
            if (lowered != ir.null_node) {
                // Swift ensurePlusOne: retain +0 values for caller
                if (!self.target.isWasm() and self.type_reg.couldBeARC(ret_type)) {
                    value_node = try self.emitCopyValue(fb, lowered, ret_type, span);
                } else {
                    value_node = lowered;
                }
            }

            // Forward ownership: if return expression is an ident with a local cleanup
            if (value_tag == .ident) {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(value_idx));
                if (fb.lookupLocal(name)) |local_idx| {
                    _ = self.cleanup_stack.disableForLocal(local_idx);
                }
            }
        }

        // Handle implicit return from error!void functions
        if (value_node == null) {
            if (ret_type_info == .error_union) {
                const eu_size = self.type_reg.sizeOf(ret_type);
                const tmp_local = try fb.addLocalWithSize("__ret_eu", ret_type, false, eu_size);
                const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, span);
                value_node = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
            }
        }

        // Capture return value if defers on stack
        if (value_node != null and self.hasDeferCleanups()) {
            const ret_val = value_node.?;
            const ret_size = self.type_reg.sizeOf(ret_type);
            const tmp_local = try fb.addLocalWithSize("__ret_tmp", ret_type, false, ret_size);
            _ = try fb.emitStoreLocal(tmp_local, ret_val, span);
            if (is_error_return) {
                try self.emitCleanupsErrorPath(0);
            } else {
                try self.emitCleanups(0);
            }
            value_node = try fb.emitLoadLocal(tmp_local, ret_type, span);
            _ = try fb.emitRet(value_node, span);
            return;
        }

        if (is_error_return) {
            try self.emitCleanupsErrorPath(0);
        } else {
            try self.emitCleanups(0);
        }
        _ = try fb.emitRet(value_node, span);
    }

    fn lowerLocalVarDecl(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const vd = self.tree.localVarData(idx);
        const span = self.getSpan(idx);

        var type_idx = TypeRegistry.VOID;
        if (vd.type_expr.unwrap()) |te| {
            type_idx = self.resolveTypeNode(te);
            // @safe coercion: checker upgrades `var p: Foo = new Foo{...}` to *Foo,
            // but resolveTypeNode returns raw Foo from AST. Match the checker's coercion.
            if (self.chk.safe_mode) {
                if (vd.value.unwrap()) |ve| {
                    const val_type = self.inferExprType(ve);
                    if (val_type != TypeRegistry.VOID) {
                        const val_info = self.type_reg.get(val_type);
                        if (val_info == .pointer and val_info.pointer.elem == type_idx) {
                            type_idx = val_type;
                        }
                    }
                }
            }
        } else if (vd.value.unwrap()) |ve| {
            type_idx = self.inferExprType(ve);
        }

        const size = self.type_reg.sizeOf(type_idx);
        const local_idx = try fb.addLocalWithSize(vd.name, type_idx, !vd.is_const, size);

        if (vd.value.unwrap()) |value_idx| {
            const value_tag = self.tree.nodeTag(value_idx);

            // undefined / .{} zero init — zero memory
            if (value_tag == .literal_undefined or value_tag == .zero_init) {
                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, span);
                const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, span);
                var args = [_]ir.NodeIndex{ local_addr, size_node };
                _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, span);

                // Register ARC release cleanup for zero-init collections/structs with ARC fields
                if (value_tag == .zero_init) {
                    if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                        const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                        const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                        _ = try self.cleanup_stack.push(cleanup);
                    }
                }
                return;
            }

            // Existential coercion: concrete → any Trait
            if (self.type_reg.get(type_idx) == .existential) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node == ir.null_node) return;
                // If source is already existential, just store directly (copy, not erasure)
                _ = try fb.emitStoreLocal(local_idx, value_node, span);
                if (!self.target.isWasm()) {
                    const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                    const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                    _ = try self.cleanup_stack.push(cleanup);
                }
                return;
            }

            const is_struct_literal = (value_tag == .struct_init or value_tag == .struct_init_one);
            const is_tuple_literal = (value_tag == .tuple_literal);

            // String init: compound decomposition (ptr+len)
            if (type_idx == TypeRegistry.STRING) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const ptr_val = try fb.emitSlicePtr(value_node, ptr_type, span);
                    const len_val = try fb.emitSliceLen(value_node, span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
                    _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
                }
                return;
            }

            // Array init
            if (self.type_reg.isArray(type_idx)) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                }
                // Register ARC release cleanup for arrays with ARC elements
                if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                    const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                    const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                    _ = try self.cleanup_stack.push(cleanup);
                }
                return;
            }

            // Slice init
            if (self.type_reg.isSlice(type_idx)) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                }
                return;
            }

            // Struct literal init
            if (is_struct_literal) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                }
                // Register ARC release cleanup for structs with ARC fields
                if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                    const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                    const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                    _ = try self.cleanup_stack.push(cleanup);
                }
                return;
            }

            // Tuple literal init
            if (is_tuple_literal) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                }
                if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                    const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                    const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                    _ = try self.cleanup_stack.push(cleanup);
                }
                return;
            }

            // Union init
            if (self.type_reg.get(type_idx) == .union_type) {
                const value_node = try self.lowerExprNode(value_idx);
                if (value_node != ir.null_node) {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                }
                return;
            }

            // Compound optional init: T → ?T wrapping
            if (self.type_reg.get(type_idx) == .optional and !self.isPtrLikeOptional(type_idx)) {
                const is_null_init = (value_tag == .literal_null);
                if (is_null_init) {
                    // null → tag=0 (payload left uninitialized)
                    const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_zero, span);
                } else {
                    const value_node = try self.lowerExprNode(value_idx);
                    if (value_node == ir.null_node) return;
                    // Check if lowered is const_null
                    const is_lowered_null = fb.nodes.items[value_node].data == .const_null;
                    if (is_lowered_null) {
                        const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_zero, span);
                    } else {
                        const value_type = self.inferExprType(value_idx);
                        const value_info = self.type_reg.get(value_type);
                        if (value_info == .optional) {
                            // Already ?T (from function call SRET) — compound copy
                            _ = try fb.emitStoreLocal(local_idx, value_node, span);
                        } else {
                            // Plain T → wrap as tag=1, payload=value
                            const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                            _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_one, span);
                            _ = try fb.emitStoreLocalField(local_idx, 1, 8, value_node, span);
                        }
                    }
                }
                // Register ARC cleanup for compound optionals with managed payloads
                if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                    const loaded = try fb.emitLoadLocal(local_idx, type_idx, span);
                    const cleanup = Cleanup.initForLocal(.release, loaded, type_idx, local_idx);
                    _ = try self.cleanup_stack.push(cleanup);
                }
                return;
            }

            // General case: lower the value and store
            {
                if (!self.target.isWasm() and self.type_reg.couldBeARC(type_idx)) {
                    // ARC path: copy value for ownership
                    const value_node = try self.lowerExprNode(value_idx);
                    if (value_node != ir.null_node) {
                        _ = try self.emitCopyValue(fb, value_node, type_idx, span);
                        _ = try fb.emitStoreLocal(local_idx, value_node, span);
                        const cleanup = Cleanup.initForLocal(.release, value_node, type_idx, local_idx);
                        _ = try self.cleanup_stack.push(cleanup);
                    }
                } else {
                    // Non-ARC path (trivial types)
                    const value_node = try self.lowerExprNode(value_idx);
                    if (value_node != ir.null_node) {
                        _ = try fb.emitStoreLocal(local_idx, value_node, span);
                    }
                }
            }
        }
    }

    fn lowerAssign(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        const target_idx = data.node_and_node[0];
        const value_idx_raw = data.node_and_node[1];
        const span = self.getSpan(idx);

        const target_tag = self.tree.nodeTag(target_idx);

        // Discard pattern: _ = expr
        if (target_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(target_idx));
            if (std.mem.eql(u8, name, "_")) {
                _ = try self.lowerExprNode(value_idx_raw);
                return;
            }
        }

        // Compound assignment (+=, -=, etc.)
        const value_node = if (tag != .assign) blk: {
            const target_val = try self.lowerExprNode(target_idx);
            const rhs_val = try self.lowerExprNode(value_idx_raw);
            const result_type = self.inferExprType(target_idx);
            const op: ir.BinaryOp = switch (tag) {
                .assign_add => .add,
                .assign_sub => .sub,
                .assign_mul => .mul,
                .assign_div => .div,
                .assign_mod => .mod,
                .assign_bit_and => .bit_and,
                .assign_bit_or => .bit_or,
                .assign_bit_xor => .bit_xor,
                else => unreachable,
            };
            break :blk try fb.emitBinary(op, target_val, rhs_val, result_type, span);
        } else try self.lowerExprNode(value_idx_raw);

        // Store to target
        switch (target_tag) {
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(target_idx));
                if (fb.lookupLocal(name)) |local_idx| {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
                } else if (self.builder.lookupGlobal(name)) |g| {
                    _ = try fb.emitGlobalStore(g.idx, name, value_node, span);
                }
            },
            .field_access => {
                try self.lowerFieldAssign(target_idx, value_node, value_idx_raw, span);
            },
            .index => {
                try self.lowerIndexAssign(target_idx, value_node, span);
            },
            .unary_deref => {
                const operand_idx = self.tree.nodeData(target_idx).node;
                const ptr_node = try self.lowerExprNode(operand_idx);
                _ = try fb.emitPtrStoreValue(ptr_node, value_node, span);
            },
            else => {},
        }
    }

    fn lowerIfStmt(self: *Lowerer, idx: Index) !bool {
        const fb = self.current_func orelse return false;
        const if_data = self.tree.ifData(idx);
        const span = self.getSpan(idx);

        // Optional unwrap: if expr |val| { ... }
        if (if_data.capture_token.unwrap() != null) return try self.lowerIfOptional(idx);

        // Comptime const-fold
        if (self.chk.evalConstExpr(if_data.condition)) |cond_val| {
            if (cond_val != 0) {
                return try self.lowerBlockNode(if_data.then_branch);
            } else if (if_data.else_branch.unwrap()) |eb| {
                return try self.lowerBlockNode(eb);
            }
            return false;
        }

        const then_block = try fb.newBlock("then");
        const else_block = if (if_data.else_branch.unwrap() != null) try fb.newBlock("else") else null;
        const merge_block = try fb.newBlock("if.end");
        const cond_node = try self.lowerExprNode(if_data.condition);
        if (cond_node == ir.null_node) return false;
        _ = try fb.emitBranch(cond_node, then_block, else_block orelse merge_block, span);

        fb.beginOverlapGroup();
        fb.nextOverlapArm();
        fb.setBlock(then_block);
        if (!try self.lowerBlockNode(if_data.then_branch)) _ = try fb.emitJump(merge_block, span);
        if (else_block) |eb| {
            fb.nextOverlapArm();
            fb.setBlock(eb);
            if (if_data.else_branch.unwrap()) |else_idx| {
                if (!try self.lowerBlockNode(else_idx)) _ = try fb.emitJump(merge_block, span);
            }
        }
        fb.endOverlapGroup();
        fb.setBlock(merge_block);
        return false;
    }

    fn lowerWhileStmt(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const wd = self.tree.whileData(idx);
        const span = self.getSpan(idx);

        // Optional capture: while (expr) |val| { ... }
        if (wd.capture_token.unwrap() != null) return try self.lowerWhileOptional(idx);

        const cond_block = try fb.newBlock("while.cond");
        const body_block = try fb.newBlock("while.body");
        const exit_block = try fb.newBlock("while.end");
        const cont_block = if (wd.continue_expr.unwrap() != null) try fb.newBlock("while.cont") else cond_block;

        _ = try fb.emitJump(cond_block, span);
        fb.setBlock(cond_block);
        const cond_node = try self.lowerExprNode(wd.condition);
        if (cond_node == ir.null_node) return;
        _ = try fb.emitBranch(cond_node, body_block, exit_block, span);

        const label: ?[]const u8 = if (wd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
        try self.loop_stack.append(self.allocator, .{
            .cond_block = cont_block,
            .exit_block = exit_block,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
            .label = label,
        });

        fb.setBlock(body_block);
        if (!try self.lowerBlockNode(wd.body)) _ = try fb.emitJump(cont_block, span);

        if (wd.continue_expr.unwrap()) |cont_expr| {
            fb.setBlock(cont_block);
            _ = try self.lowerExprNode(cont_expr);
            _ = try fb.emitJump(cond_block, span);
        }

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    fn lowerForStmt(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const fd = self.tree.forData(idx);
        const span = self.getSpan(idx);

        // Handle numeric range: for i in start..end
        if (fd.range_start.unwrap() != null and fd.range_end.unwrap() != null) {
            try self.lowerForRange(idx);
            return;
        }

        // Array/slice/map iteration
        if (fd.iterable.unwrap()) |iterable_idx| {
            const iter_type = self.inferExprType(iterable_idx);
            const iter_info = self.type_reg.get(iter_type);

            // Map iteration
            if (iter_info == .map) {
                try self.lowerForMap(idx, iter_info.map);
                return;
            }
            if (iter_info == .struct_type and std.mem.startsWith(u8, iter_info.struct_type.name, "Map(")) {
                if (checker_mod.parseMapTypeArgs(iter_info.struct_type.name)) |args| {
                    try self.lowerForMap(idx, .{ .key = args[0], .value = args[1] });
                    return;
                }
            }

            const elem_type: TypeIndex = switch (iter_info) {
                .array => |a| a.elem,
                .slice => |s| s.elem,
                else => {
                    _ = try self.lowerBlockNode(fd.body);
                    return;
                },
            };
            const elem_size = self.type_reg.sizeOf(elem_type);

            // Index variable
            const idx_name = try self.tempName("__for_idx");
            const idx_local = try fb.addLocalWithSize(idx_name, TypeRegistry.I64, true, 8);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocal(idx_local, zero, span);

            // Length
            const len_name = try self.tempName("__for_len");
            const len_local = try fb.addLocalWithSize(len_name, TypeRegistry.I64, false, 8);
            switch (iter_info) {
                .array => |a| {
                    const len_val = try fb.emitConstInt(@intCast(a.length), TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(len_local, len_val, span);
                },
                .slice => {
                    const iter_tag = self.tree.nodeTag(iterable_idx);
                    if (iter_tag == .ident) {
                        const iname = self.tree.tokenSlice(self.tree.nodeMainToken(iterable_idx));
                        if (fb.lookupLocal(iname)) |slice_local| {
                            const slice_val = try fb.emitLoadLocal(slice_local, iter_type, span);
                            const len_val = try fb.emitSliceLen(slice_val, span);
                            _ = try fb.emitStoreLocal(len_local, len_val, span);
                        }
                    }
                },
                else => return,
            }

            const cond_block = try fb.newBlock("for.cond");
            const body_block = try fb.newBlock("for.body");
            const incr_block = try fb.newBlock("for.incr");
            const exit_block = try fb.newBlock("for.end");

            _ = try fb.emitJump(cond_block, span);
            fb.setBlock(cond_block);
            const idx_val = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
            const len_val_cond = try fb.emitLoadLocal(len_local, TypeRegistry.I64, span);
            const cond = try fb.emitBinary(.lt, idx_val, len_val_cond, TypeRegistry.BOOL, span);
            _ = try fb.emitBranch(cond, body_block, exit_block, span);

            const label: ?[]const u8 = if (fd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
            try self.loop_stack.append(self.allocator, .{
                .cond_block = incr_block,
                .exit_block = exit_block,
                .cleanup_depth = self.cleanup_stack.getScopeDepth(),
                .label = label,
            });

            fb.setBlock(body_block);
            const binding_local = try fb.addLocalWithSize(fd.binding, elem_type, false, elem_size);
            const cur_idx = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);

            // Bind index variable if present
            if (fd.index_binding_token.unwrap()) |ibt| {
                const idx_binding = self.tree.tokenSlice(ibt);
                const idx_binding_local = try fb.addLocalWithSize(idx_binding, TypeRegistry.I64, false, 8);
                _ = try fb.emitStoreLocal(idx_binding_local, cur_idx, span);
            }

            switch (iter_info) {
                .array => {
                    const iter_tag2 = self.tree.nodeTag(iterable_idx);
                    if (iter_tag2 == .ident) {
                        const iname2 = self.tree.tokenSlice(self.tree.nodeMainToken(iterable_idx));
                        if (fb.lookupLocal(iname2)) |arr_local| {
                            const elem_val = try fb.emitIndexLocal(arr_local, cur_idx, elem_size, elem_type, span);
                            _ = try fb.emitStoreLocal(binding_local, elem_val, span);
                        }
                    }
                },
                .slice => {
                    const iter_tag2 = self.tree.nodeTag(iterable_idx);
                    if (iter_tag2 == .ident) {
                        const iname2 = self.tree.tokenSlice(self.tree.nodeMainToken(iterable_idx));
                        if (fb.lookupLocal(iname2)) |slice_local| {
                            const slice_val = try fb.emitLoadLocal(slice_local, iter_type, span);
                            const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                            const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, span);
                            const elem_val = try fb.emitIndexValue(ptr_val, cur_idx, elem_size, elem_type, span);
                            _ = try fb.emitStoreLocal(binding_local, elem_val, span);
                        }
                    }
                },
                else => {},
            }

            if (!try self.lowerBlockNode(fd.body)) _ = try fb.emitJump(incr_block, span);

            fb.setBlock(incr_block);
            const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
            const one = try fb.emitConstInt(1, TypeRegistry.I64, span);
            const idx_after = try fb.emitBinary(.add, idx_before, one, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocal(idx_local, idx_after, span);
            _ = try fb.emitJump(cond_block, span);

            _ = self.loop_stack.pop();
            fb.setBlock(exit_block);
        }
    }

    fn lowerBlockStmt(self: *Lowerer, idx: Index) Error!bool {
        const fb = self.current_func orelse return false;
        const cleanup_depth = self.cleanup_stack.getScopeDepth();
        const scope_depth = fb.markScopeEntry();

        const data = self.tree.nodeData(idx);
        const stmt_range = data.extra_range;
        const stmt_indices = self.tree.extraNodes(stmt_range);

        for (stmt_indices) |stmt_idx| {
            const stmt_tag = self.tree.nodeTag(stmt_idx);
            switch (stmt_tag) {
                .struct_decl => try self.lowerStructDecl(stmt_idx),
                .fn_decl => try self.lowerFnDecl(stmt_idx),
                else => {
                    if (try self.lowerStmt(stmt_idx)) {
                        fb.restoreScope(scope_depth);
                        return true;
                    }
                },
            }
        }
        try self.emitCleanups(cleanup_depth);
        fb.restoreScope(scope_depth);
        return false;
    }

    fn lowerBreak(self: *Lowerer, target_label: ?[]const u8, value: ?Index) !void {
        const fb = self.current_func orelse return;

        // Check labeled block stack first
        if (target_label) |label| {
            if (self.findLabeledBlock(label)) |blk_ctx| {
                if (value) |val_idx| {
                    const val = try self.lowerExprNode(val_idx);
                    if (val != ir.null_node) {
                        _ = try fb.emitStoreLocal(blk_ctx.result_local, val, Span.zero);
                    }
                }
                try self.emitCleanupsNoPop(blk_ctx.cleanup_depth);
                _ = try fb.emitJump(blk_ctx.exit_block, Span.zero);
                return;
            }
        }

        if (self.loop_stack.items.len == 0) return;
        const ctx = if (target_label) |label|
            self.findLabeledLoop(label) orelse self.loop_stack.items[self.loop_stack.items.len - 1]
        else
            self.loop_stack.items[self.loop_stack.items.len - 1];
        try self.emitCleanupsNoPop(ctx.cleanup_depth);
        _ = try fb.emitJump(ctx.exit_block, Span.zero);
    }

    fn lowerContinue(self: *Lowerer, target_label: ?[]const u8) !void {
        const fb = self.current_func orelse return;
        if (self.loop_stack.items.len == 0) return;
        const ctx = if (target_label) |label|
            self.findLabeledLoop(label) orelse self.loop_stack.items[self.loop_stack.items.len - 1]
        else
            self.loop_stack.items[self.loop_stack.items.len - 1];
        try self.emitCleanupsNoPop(ctx.cleanup_depth);
        _ = try fb.emitJump(ctx.cond_block, Span.zero);
    }

    fn lowerDefer(self: *Lowerer, idx: Index, is_errdefer: bool) !void {
        const data = self.tree.nodeData(idx);
        const expr_node = data.node;
        const cleanup = Cleanup{
            .kind = if (is_errdefer) .errdefer_expr else .defer_expr,
            .value = @intFromEnum(expr_node),
            .type_idx = TypeRegistry.VOID,
            .state = .active,
            .local_idx = null,
            .func_name = null,
        };
        _ = try self.cleanup_stack.push(cleanup);
    }

    fn findLabeledBlock(self: *Lowerer, target_label: []const u8) ?LabeledBlockContext {
        var i: usize = self.labeled_block_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.labeled_block_stack.items[i].label, target_label))
                return self.labeled_block_stack.items[i];
        }
        return null;
    }

    fn findLabeledLoop(self: *Lowerer, target_label: []const u8) ?LoopContext {
        var i: usize = self.loop_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (self.loop_stack.items[i].label) |label| {
                if (std.mem.eql(u8, label, target_label)) return self.loop_stack.items[i];
            }
        }
        return null;
    }

    // ========================================================================
    // ARC cleanup functions
    // ========================================================================

    fn hasDeferCleanups(self: *const Lowerer) bool {
        for (self.cleanup_stack.items.items) |cleanup| {
            if (cleanup.isActive() and (cleanup.kind == .defer_expr or cleanup.kind == .errdefer_expr or cleanup.kind == .scope_destroy)) return true;
        }
        return false;
    }

    fn emitCleanups(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, false, true);
    }

    fn emitCleanupsErrorPath(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, true, true);
    }

    fn emitCleanupsNoPop(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, false, false);
    }

    fn emitCleanupsImpl(self: *Lowerer, target_depth: usize, is_error_path: bool, pop: bool) Error!void {
        const fb = self.current_func orelse return;
        const items = self.cleanup_stack.getActiveCleanups();

        var i = items.len;
        while (i > target_depth) {
            i -= 1;
            const cleanup = items[i];
            if (cleanup.isActive()) {
                switch (cleanup.kind) {
                    .release => {
                        if (self.target.isWasm()) continue;
                        // TODO: centralized emitDestroyValue
                        const value = if (cleanup.local_idx) |lidx|
                            try fb.emitLoadLocal(lidx, cleanup.type_idx, Span.zero)
                        else
                            cleanup.value;
                        var args = [_]ir.NodeIndex{value};
                        _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .unowned_release => {
                        if (self.target.isWasm()) continue;
                        const value = if (cleanup.local_idx) |lidx|
                            try fb.emitLoadLocal(lidx, cleanup.type_idx, Span.zero)
                        else
                            cleanup.value;
                        var args = [_]ir.NodeIndex{value};
                        _ = try fb.emitCall("unowned_release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .weak_release => {
                        if (self.target.isWasmGC()) continue;
                        const value = if (cleanup.local_idx) |lidx|
                            try fb.emitLoadLocal(lidx, cleanup.type_idx, Span.zero)
                        else
                            cleanup.value;
                        var args = [_]ir.NodeIndex{value};
                        _ = try fb.emitCall("weak_release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .defer_expr => {
                        try self.lowerDeferredNode(@enumFromInt(cleanup.value));
                    },
                    .errdefer_expr => {
                        if (is_error_path) {
                            try self.lowerDeferredNode(@enumFromInt(cleanup.value));
                        }
                    },
                    .scope_destroy => {
                        if (cleanup.local_idx) |lidx| {
                            if (cleanup.func_name) |fname| {
                                const ptr_type = self.type_reg.makePointer(cleanup.type_idx) catch TypeRegistry.I64;
                                const self_addr = try fb.emitAddrLocal(lidx, ptr_type, Span.zero);
                                var call_args = [_]ir.NodeIndex{self_addr};
                                _ = try fb.emitCall(fname, &call_args, false, TypeRegistry.VOID, Span.zero);
                            }
                        }
                    },
                    .end_borrow => {},
                }
            }
        }

        if (pop) {
            while (self.cleanup_stack.items.items.len > target_depth) {
                _ = self.cleanup_stack.items.pop();
            }
        }
    }

    fn lowerDeferredNode(self: *Lowerer, node_idx: Index) Error!void {
        const tag = self.tree.nodeTag(node_idx);
        switch (tag) {
            // Statement tags
            .return_expr, .return_void,
            .var_local, .const_local,
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div,
            .assign_mod, .assign_bit_and, .assign_bit_or, .assign_bit_xor,
            .if_stmt_simple, .if_stmt,
            .while_stmt, .for_stmt, .block_stmt,
            .break_plain, .break_expr,
            .continue_plain, .continue_labeled,
            .defer_stmt, .errdefer_stmt,
            .expr_stmt, .async_let, .destructure,
            => {
                _ = try self.lowerStmt(node_idx);
            },
            else => {
                _ = try self.lowerExprNode(node_idx);
            },
        }
    }

    // ========================================================================
    // Expression lowering — stubs for submodule functions
    // ========================================================================

    fn lowerExprNode(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const tag = self.tree.nodeTag(idx);
        const span = self.getSpan(idx);

        switch (tag) {
            .literal_int => {
                const fb = self.current_func orelse return ir.null_node;
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                const value = std.fmt.parseInt(i64, text, 10) catch 0;
                return fb.emitConstInt(value, TypeRegistry.I64, span);
            },
            .literal_float => {
                const fb = self.current_func orelse return ir.null_node;
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                const value = std.fmt.parseFloat(f64, text) catch 0.0;
                return fb.emitConstFloat(value, TypeRegistry.F64, span);
            },
            .literal_true => {
                const fb = self.current_func orelse return ir.null_node;
                return fb.emitConstBool(true, span);
            },
            .literal_false => {
                const fb = self.current_func orelse return ir.null_node;
                return fb.emitConstBool(false, span);
            },
            .literal_null => {
                const fb = self.current_func orelse return ir.null_node;
                return fb.emit(ir.Node.init(.{ .const_null = {} }, TypeRegistry.UNTYPED_NULL, span));
            },
            .literal_string => {
                const fb = self.current_func orelse return ir.null_node;
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                // Strip quotes
                const content = if (text.len >= 2) text[1 .. text.len - 1] else text;
                const copied = try self.allocator.dupe(u8, content);
                const str_idx = try fb.addStringLiteral(copied);
                return fb.emitConstSlice(str_idx, span);
            },
            .ident => {
                const fb = self.current_func orelse return ir.null_node;
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));

                // Check comptime const values first
                if (self.const_values.get(name)) |value| {
                    return fb.emitConstInt(value, TypeRegistry.I64, span);
                }
                if (self.float_const_values.get(name)) |fval| {
                    const ftype = self.float_const_types.get(name) orelse TypeRegistry.F64;
                    return fb.emitConstFloat(fval, ftype, span);
                }

                // Check locals
                if (fb.lookupLocal(name)) |local_idx| {
                    const local_type = fb.locals.items[local_idx].type_idx;
                    return fb.emitLoadLocal(local_idx, local_type, span);
                }

                // Check globals
                if (self.builder.lookupGlobal(name)) |g| {
                    return fb.emitGlobalRef(g.idx, name, g.global.type_idx, span);
                }

                return ir.null_node;
            },
            .paren => {
                const data = self.tree.nodeData(idx);
                return self.lowerExprNode(data.node);
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte,
            .binary_and, .binary_or,
            .binary_bit_and, .binary_bit_or, .binary_bit_xor, .binary_shl, .binary_shr,
            => {
                return self.lowerBinaryExpr(idx);
            },
            .unary_neg, .unary_not, .unary_bit_not => {
                return self.lowerUnaryExpr(idx);
            },
            .call_zero, .call_one, .call => {
                return self.lowerCallExpr(idx);
            },

            // Stubs for complex expression lowering
            .field_access => return self.lowerFieldAccess(idx),
            .index => return self.lowerIndexExpr(idx),
            .struct_init_one, .struct_init => return self.lowerStructInitExpr(idx),
            .new_expr => return self.lowerNewExpr(idx),
            .builtin_call => return self.lowerBuiltinCall(idx),
            .closure_expr => return self.lowerClosureExpr(idx),
            .string_interp => return self.lowerStringInterp(idx),
            .switch_expr => return self.lowerSwitchExpr(idx),
            .if_simple, .if_full => return self.lowerIfExpr(idx),
            .unary_deref => return self.lowerDerefExpr(idx),
            .unary_addr_of => return self.lowerAddrOfExpr(idx),
            .unary_try => return self.lowerTryExpr(idx),
            .unary_await => return self.lowerAwaitExpr(idx),
            .catch_expr => return self.lowerCatchExpr(idx),
            .orelse_expr => return self.lowerOrelseExpr(idx),
            .binary_concat => return self.lowerConcatExpr(idx),
            .binary_pipe => return self.lowerPipeExpr(idx),
            .error_literal => return self.lowerErrorLiteral(idx),
            .array_literal_empty, .array_literal_one, .array_literal => return self.lowerArrayLiteral(idx),
            .tuple_literal => return self.lowerTupleLiteral(idx),
            .slice => return self.lowerSliceExpr(idx),
            .literal_char => return self.lowerCharLiteral(idx),
            .block_one, .block_two, .block => return self.lowerBlockExpr(idx),
            .type_generic => return self.lowerGenericCall(idx),
            .comptime_block => return self.lowerComptimeBlock(idx),
            .task_expr => return self.lowerTaskExpr(idx),
            .unary_unwrap => return self.lowerUnwrapExpr(idx),
            .literal_undefined, .literal_unreachable => return ir.null_node,
            .zero_init => return ir.null_node,

            // Statement tags that might appear in expression position
            .expr_stmt => {
                const data = self.tree.nodeData(idx);
                return self.lowerExprNode(data.node);
            },

            else => return ir.null_node,
        }
    }

    // ========================================================================
    // Binary / Unary expression lowering
    // ========================================================================

    fn lowerBinaryExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        const left_idx = data.node_and_node[0];
        const right_idx = data.node_and_node[1];
        const span = self.getSpan(idx);

        const left = try self.lowerExprNode(left_idx);
        if (left == ir.null_node) return ir.null_node;
        const right = try self.lowerExprNode(right_idx);
        if (right == ir.null_node) return ir.null_node;

        const result_type = self.inferExprType(idx);
        const op: ir.BinaryOp = switch (tag) {
            .binary_add => .add,
            .binary_sub => .sub,
            .binary_mul => .mul,
            .binary_div => .div,
            .binary_mod => .mod,
            .binary_eq => .eq,
            .binary_neq => .ne,
            .binary_lt => .lt,
            .binary_gt => .gt,
            .binary_lte => .le,
            .binary_gte => .ge,
            .binary_and => .@"and",
            .binary_or => .@"or",
            .binary_bit_and => .bit_and,
            .binary_bit_or => .bit_or,
            .binary_bit_xor => .bit_xor,
            .binary_shl => .shl,
            .binary_shr => .shr,
            else => unreachable,
        };
        return fb.emitBinary(op, left, right, result_type, span);
    }

    fn lowerUnaryExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(idx);
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        const operand = try self.lowerExprNode(operand_idx);
        if (operand == ir.null_node) return ir.null_node;

        const result_type = self.inferExprType(idx);
        const op: ir.UnaryOp = switch (tag) {
            .unary_neg => .neg,
            .unary_not => .not,
            .unary_bit_not => .bit_not,
            else => unreachable,
        };
        return fb.emitUnary(op, operand, result_type, span);
    }

    fn lowerCallExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const cd = self.tree.callData(idx);
        const span = self.getSpan(idx);

        const callee_tag = self.tree.nodeTag(cd.callee);

        // Distinct type constructor: RawPtr(expr) — zero-cost, just lower the argument
        if (callee_tag == .ident) {
            const callee_name = self.tree.tokenSlice(self.tree.nodeMainToken(cd.callee));
            if (self.type_reg.lookupByName(callee_name)) |type_idx| {
                if (self.type_reg.get(type_idx) == .distinct) {
                    // Single arg = just lower it
                    if (cd.single_arg.unwrap()) |single| return try self.lowerExprNode(single);
                    const arg_indices = self.tree.extraNodes(cd.args);
                    if (arg_indices.len == 1) return try self.lowerExprNode(arg_indices[0]);
                    return ir.null_node;
                }
            }
        }

        // Method call (value.method(...))
        if (callee_tag == .field_access) {
            const fa_data = self.tree.nodeData(cd.callee);
            const base_idx = fa_data.node_and_token[0];
            const field_token = fa_data.node_and_token[1];
            const field_name = self.tree.tokenSlice(field_token);

            const base_type_idx = self.inferExprType(base_idx);
            const base_type = self.type_reg.get(base_type_idx);
            const type_name: ?[]const u8 = switch (base_type) {
                .struct_type => |s| s.name,
                .enum_type => |et| et.name,
                .pointer => |p| blk: {
                    const elem = self.type_reg.get(p.elem);
                    if (elem == .struct_type) break :blk elem.struct_type.name;
                    if (elem == .enum_type) break :blk elem.enum_type.name;
                    if (elem == .list) break :blk @as(?[]const u8, "List");
                    if (elem == .map) break :blk @as(?[]const u8, "Map");
                    break :blk null;
                },
                .basic => |bk| bk.name(),
                .slice => if (base_type_idx == TypeRegistry.STRING) "string" else null,
                .list => "List",
                .map => "Map",
                else => null,
            };

            if (type_name) |tname| {
                if (self.chk.lookupMethod(tname, field_name)) |method_info| {
                    return try self.lowerMethodCall(idx, base_idx, field_name, method_info, tname);
                }
            }

            // Union variant constructor: Result.Ok(42)
            if (base_type == .union_type) {
                for (base_type.union_type.variants, 0..) |v, i| {
                    if (std.mem.eql(u8, v.name, field_name)) {
                        const union_size = self.type_reg.sizeOf(base_type_idx);
                        if (union_size > 8) {
                            const tmp_local = try fb.addLocalWithSize("__union_tmp", base_type_idx, false, union_size);
                            const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                            _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_val, span);
                            // Lower payload arg
                            if (cd.single_arg.unwrap()) |single| {
                                const payload_val = try self.lowerExprNode(single);
                                _ = try fb.emitStoreLocalField(tmp_local, 1, 8, payload_val, span);
                            } else {
                                const arg_indices = self.tree.extraNodes(cd.args);
                                if (arg_indices.len > 0) {
                                    const payload_val = try self.lowerExprNode(arg_indices[0]);
                                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, payload_val, span);
                                }
                            }
                            return try fb.emitLoadLocal(tmp_local, base_type_idx, span);
                        }
                        // Small union
                        var payload: ?ir.NodeIndex = null;
                        if (cd.single_arg.unwrap()) |single| {
                            payload = try self.lowerExprNode(single);
                        } else {
                            const arg_indices = self.tree.extraNodes(cd.args);
                            if (arg_indices.len > 0) payload = try self.lowerExprNode(arg_indices[0]);
                        }
                        return try fb.emitUnionInit(@intCast(i), payload, base_type_idx, span);
                    }
                }
                return ir.null_node;
            }
        }

        // Generic function call: max(i64)(3, 5) — callee is a type_generic (the inner call)
        if (callee_tag == .type_generic) {
            // Look up generic instantiation from checker
            if (self.chk.generic_instantiations.get(cd.callee)) |inst_info| {
                try self.ensureGenericFnQueued(inst_info);
                // Lower value arguments
                var gen_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                defer gen_args.deinit(self.allocator);

                // Collect args
                if (cd.single_arg.unwrap()) |single| {
                    const arg = try self.lowerExprNode(single);
                    try gen_args.append(self.allocator, arg);
                } else {
                    const arg_indices = self.tree.extraNodes(cd.args);
                    for (arg_indices) |arg_idx| {
                        const arg = try self.lowerExprNode(arg_idx);
                        if (arg != ir.null_node) try gen_args.append(self.allocator, arg);
                    }
                }

                // Get concrete return type
                const concrete_func_type = blk: {
                    if (self.chk.scope.lookup(inst_info.concrete_name)) |sym| break :blk sym.type_idx;
                    break :blk self.type_reg.lookupByName(inst_info.concrete_name) orelse TypeRegistry.VOID;
                };
                const concrete_info = self.type_reg.get(concrete_func_type);
                const ret_type = if (concrete_info == .func) concrete_info.func.return_type else TypeRegistry.VOID;

                // VWT base-name sharing: call sites use the BASE name (shared body)
                const call_target = self.computeGenericBaseName(inst_info.concrete_name) catch inst_info.concrete_name;

                // SRET for generic function calls
                if (self.needsSret(ret_type)) {
                    const ret_size = self.type_reg.sizeOf(ret_type);
                    const sret_local = try fb.getOrCreateSretLocal(ret_type, ret_size);
                    const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, span);
                    var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                    defer sret_args.deinit(self.allocator);
                    try sret_args.append(self.allocator, sret_addr);
                    try sret_args.appendSlice(self.allocator, gen_args.items);
                    _ = try fb.emitCall(call_target, sret_args.items, false, TypeRegistry.VOID, span);
                    return try fb.emitLoadLocal(sret_local, ret_type, span);
                }
                return try fb.emitCall(call_target, gen_args.items, false, ret_type, span);
            }
        }

        // Regular function call
        if (callee_tag == .ident) {
            const func_name = self.tree.tokenSlice(self.tree.nodeMainToken(cd.callee));

            // Get function type for parameter checking
            const func_type_idx = self.inferExprType(cd.callee);
            const func_type_info = self.type_reg.get(func_type_idx);
            const param_types: ?[]const types_mod.FuncParam = if (func_type_info == .func) func_type_info.func.params else null;

            var args = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer args.deinit(self.allocator);

            // Collect args into a list
            var arg_list = try self.collectCallArgsList(cd);
            defer arg_list.deinit(self.allocator);
            for (arg_list.items, 0..) |arg_idx, arg_i| {
                var arg_node = try self.lowerExprNode(arg_idx);
                if (arg_node == ir.null_node) continue;

                // Determine parameter type for ABI decomposition
                var param_is_pointer = false;
                var param_is_compound = false;
                if (param_types) |params| {
                    if (arg_i < params.len) {
                        const param_type = self.type_reg.get(params[arg_i].type_idx);
                        param_is_pointer = (param_type == .pointer);
                        param_is_compound = (param_type == .slice) or (params[arg_i].type_idx == TypeRegistry.STRING);
                    }
                }

                // @safe auto-ref: pass struct value as pointer to *Struct param
                if (self.chk.safe_mode and param_is_pointer) {
                    if (param_types) |params| {
                        if (arg_i < params.len) {
                            const param_info = self.type_reg.get(params[arg_i].type_idx);
                            if (param_info == .pointer) {
                                const arg_type = self.inferExprType(arg_idx);
                                const arg_info = self.type_reg.get(arg_type);
                                if ((arg_info == .struct_type or arg_info == .union_type) and self.type_reg.isAssignable(arg_type, param_info.pointer.elem)) {
                                    if (!self.target.isWasmGC()) {
                                        const ptr_type = self.type_reg.makePointer(arg_type) catch TypeRegistry.I64;
                                        const arg_tag = self.tree.nodeTag(arg_idx);
                                        if (arg_tag == .ident) {
                                            const aname = self.tree.tokenSlice(self.tree.nodeMainToken(arg_idx));
                                            if (fb.lookupLocal(aname)) |local_idx_a| {
                                                arg_node = try fb.emitAddrLocal(local_idx_a, ptr_type, span);
                                            } else if (self.builder.lookupGlobal(aname)) |g| {
                                                arg_node = try fb.emitAddrGlobal(g.idx, aname, ptr_type, span);
                                            } else {
                                                const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                                _ = try fb.emitStoreLocal(tmp_local, arg_node, span);
                                                arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, span);
                                            }
                                        } else {
                                            const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                            _ = try fb.emitStoreLocal(tmp_local, arg_node, span);
                                            arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, span);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // @safe auto-deref: when arg is *T but param expects T
                if (self.chk.safe_mode and !param_is_pointer) {
                    if (param_types) |params| {
                        if (arg_i < params.len) {
                            const arg_type = self.inferExprType(arg_idx);
                            const arg_info = self.type_reg.get(arg_type);
                            const param_tidx = params[arg_i].type_idx;
                            if (arg_info == .pointer and (self.type_reg.get(arg_info.pointer.elem) == .struct_type or self.type_reg.get(arg_info.pointer.elem) == .union_type) and
                                self.type_reg.isAssignable(arg_info.pointer.elem, param_tidx))
                            {
                                arg_node = try fb.emitPtrLoadValue(arg_node, param_tidx, span);
                            }
                        }
                    }
                }

                // T → ?T wrapping at call sites
                if (param_types) |params| {
                    if (arg_i < params.len) {
                        const param_type = self.type_reg.get(params[arg_i].type_idx);
                        if (param_type == .optional and !self.isPtrLikeOptional(params[arg_i].type_idx)) {
                            const arg_type = self.inferExprType(arg_idx);
                            if (arg_type == TypeRegistry.UNTYPED_NULL) {
                                const opt_size = self.type_reg.sizeOf(params[arg_i].type_idx);
                                const tmp = try fb.addLocalWithSize("__opt_arg", params[arg_i].type_idx, false, opt_size);
                                const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                                _ = try fb.emitStoreLocalField(tmp, 0, 0, tag_zero, span);
                                arg_node = try fb.emitLoadLocal(tmp, params[arg_i].type_idx, span);
                            } else {
                                const arg_info = self.type_reg.get(arg_type);
                                if (arg_info != .optional) {
                                    const opt_size = self.type_reg.sizeOf(params[arg_i].type_idx);
                                    const tmp = try fb.addLocalWithSize("__opt_arg", params[arg_i].type_idx, false, opt_size);
                                    const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                                    _ = try fb.emitStoreLocalField(tmp, 0, 0, tag_one, span);
                                    _ = try fb.emitStoreLocalField(tmp, 1, 8, arg_node, span);
                                    arg_node = try fb.emitLoadLocal(tmp, params[arg_i].type_idx, span);
                                }
                            }
                        }
                    }
                }

                // Go pattern: decompose compound types (slice/string) into (ptr, len)
                if (param_is_compound) {
                    const ptr_val = try fb.emitSlicePtr(arg_node, TypeRegistry.I64, span);
                    const len_val = try fb.emitSliceLen(arg_node, span);
                    try args.append(self.allocator, ptr_val);
                    try args.append(self.allocator, len_val);
                } else {
                    // Decompose large compound args (struct/union/tuple > 8 bytes)
                    var is_large_compound = false;
                    if (param_types) |params| {
                        if (arg_i < params.len) {
                            const pt = self.type_reg.get(params[arg_i].type_idx);
                            const ps = self.type_reg.sizeOf(params[arg_i].type_idx);
                            const is_compound_opt = pt == .optional and !self.isPtrLikeOptional(params[arg_i].type_idx);
                            is_large_compound = !self.target.isWasmGC() and (pt == .struct_type or pt == .union_type or pt == .tuple or is_compound_opt) and ps > 8;
                        }
                    }
                    if (is_large_compound) {
                        const param_type_idx = param_types.?[arg_i].type_idx;
                        const param_size = self.type_reg.sizeOf(param_type_idx);
                        const num_slots: u32 = @intCast((param_size + 7) / 8);
                        for (0..num_slots) |slot| {
                            const offset: i64 = @intCast(slot * 8);
                            const field = try fb.emitFieldValue(arg_node, @intCast(slot), offset, TypeRegistry.I64, span);
                            try args.append(self.allocator, field);
                        }
                    } else {
                        try args.append(self.allocator, arg_node);
                    }
                }
            }

            const return_type = if (func_type_info == .func) func_type_info.func.return_type else self.inferExprType(idx);

            // Check if this is an indirect call (function pointer)
            const is_indirect = (fb.lookupLocal(func_name) != null);

            if (is_indirect) {
                // Closure call
                const closure_ptr = try self.lowerExprNode(cd.callee);
                const table_idx = try fb.emitPtrLoadValue(closure_ptr, TypeRegistry.I64, span);

                if (self.needsSret(return_type)) {
                    const ret_size = self.type_reg.sizeOf(return_type);
                    const tmp_local = try fb.addLocalWithSize("__indirect_sret", return_type, false, ret_size);
                    const tmp_addr = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
                    var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                    defer sret_args.deinit(self.allocator);
                    try sret_args.append(self.allocator, tmp_addr);
                    try sret_args.appendSlice(self.allocator, args.items);
                    _ = try fb.emitClosureCall(table_idx, closure_ptr, sret_args.items, TypeRegistry.VOID, span);
                    return try fb.emitLoadLocal(tmp_local, return_type, span);
                }
                return try fb.emitClosureCall(table_idx, closure_ptr, args.items, return_type, span);
            }

            // Direct call
            const link_name = try self.resolveCallName(func_name);

            // SRET: caller allocates temp, passes address as hidden first arg
            if (self.needsSret(return_type)) {
                const ret_size = self.type_reg.sizeOf(return_type);
                const sret_local = try fb.getOrCreateSretLocal(return_type, ret_size);
                const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, span);
                var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                defer sret_args.deinit(self.allocator);
                try sret_args.append(self.allocator, sret_addr);
                try sret_args.appendSlice(self.allocator, args.items);
                _ = try fb.emitCall(link_name, sret_args.items, false, TypeRegistry.VOID, span);
                return try fb.emitLoadLocal(sret_local, return_type, span);
            }

            return try fb.emitCall(link_name, args.items, false, return_type, span);
        }

        // Fallback: try to lower as generic expression call
        return ir.null_node;
    }

    /// Collect call arguments as a dynamic list for iteration.
    fn collectCallArgsList(self: *Lowerer, cd: ast_mod.full.CallFull) !std.ArrayListUnmanaged(Index) {
        var result = std.ArrayListUnmanaged(Index){};
        if (cd.single_arg.unwrap()) |single| {
            try result.append(self.allocator, single);
        } else {
            const arg_indices = self.tree.extraNodes(cd.args);
            for (arg_indices) |arg_idx| {
                try result.append(self.allocator, arg_idx);
            }
        }
        return result;
    }

    // ========================================================================
    // Expression lowering — ported from compiler/frontend/lower.zig
    // ========================================================================

    /// Lower `a.field` — enum variants, struct fields, union tags/payloads,
    /// slice ptr/len, tuple elements, associated constants.
    fn lowerFieldAccess(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const base_idx = data.node_and_token[0];
        const field_token = data.node_and_token[1];
        const field_name = self.tree.tokenSlice(field_token);
        const span = self.getSpan(idx);

        // Shorthand enum variant: .variant (base is the dot token, no base expr)
        // In compact AST, field_access with a special base tag means enum shorthand.
        // Check if base is a literal/ident that resolves to an enum type.
        const base_tag = self.tree.nodeTag(base_idx);

        // Associated constant: TypeName.CONST
        if (base_tag == .ident) {
            const base_name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            var buf: [512]u8 = undefined;
            const qualified = std.fmt.bufPrint(&buf, "{s}_{s}", .{ base_name, field_name }) catch "";
            if (qualified.len > 0) {
                if (self.chk.scope.lookup(qualified)) |sym| {
                    if (sym.kind == .constant) {
                        if (sym.const_value) |cv| return try fb.emitConstInt(cv, sym.type_idx, span);
                    }
                }
            }
        }

        const base_type_idx = self.inferExprType(base_idx);
        const base_type = self.type_reg.get(base_type_idx);

        // Enum variant: Type.variant
        if (base_type == .enum_type) {
            for (base_type.enum_type.variants) |variant| {
                if (std.mem.eql(u8, variant.name, field_name))
                    return try fb.emitConstInt(variant.value, base_type_idx, span);
            }
            return ir.null_node;
        }

        // Union: .tag pseudo-field, payload extraction, or variant tag
        var union_base_type_idx = base_type_idx;
        var union_base_type = base_type;
        if (base_type == .pointer) {
            const elem = self.type_reg.get(base_type.pointer.elem);
            if (elem == .union_type) {
                union_base_type_idx = base_type.pointer.elem;
                union_base_type = elem;
            }
        }
        if (union_base_type == .union_type) {
            const base_local: ?ir.LocalIdx = if (base_tag == .ident)
                fb.lookupLocal(self.tree.tokenSlice(self.tree.nodeMainToken(base_idx)))
            else
                null;

            if (std.mem.eql(u8, field_name, "tag")) {
                if (base_local) |lidx| return try fb.emitFieldLocal(lidx, 0, 0, TypeRegistry.I64, span);
                const base_val = try self.lowerExprNode(base_idx);
                return try fb.emitFieldValue(base_val, 0, 0, TypeRegistry.I64, span);
            }
            for (union_base_type.union_type.variants, 0..) |variant, i| {
                if (std.mem.eql(u8, variant.name, field_name)) {
                    if (variant.payload_type != .invalid) {
                        if (base_local) |lidx| return try fb.emitFieldLocal(lidx, 1, 8, variant.payload_type, span);
                        const base_val = try self.lowerExprNode(base_idx);
                        return try fb.emitFieldValue(base_val, 1, 8, variant.payload_type, span);
                    }
                    const union_size = self.type_reg.sizeOf(union_base_type_idx);
                    if (union_size > 8) {
                        const tmp_local = try fb.addLocalWithSize("__union_tmp", union_base_type_idx, false, union_size);
                        const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_val, span);
                        return try fb.emitLoadLocal(tmp_local, union_base_type_idx, span);
                    }
                    return try fb.emitConstInt(@intCast(i), union_base_type_idx, span);
                }
            }
            return ir.null_node;
        }

        // Slice ptr/len
        if (base_type == .slice) {
            const base_val = try self.lowerExprNode(base_idx);
            if (std.mem.eql(u8, field_name, "ptr")) return try fb.emitSlicePtr(base_val, self.type_reg.makePointer(base_type.slice.elem) catch TypeRegistry.I64, span);
            if (std.mem.eql(u8, field_name, "len")) return try fb.emitSliceLen(base_val, span);
            return ir.null_node;
        }

        // String ptr/len
        if (base_type_idx == TypeRegistry.STRING) {
            const base_val = try self.lowerExprNode(base_idx);
            if (std.mem.eql(u8, field_name, "ptr")) return try fb.emitSlicePtr(base_val, self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64, span);
            if (std.mem.eql(u8, field_name, "len")) return try fb.emitSliceLen(base_val, span);
            return ir.null_node;
        }

        // Tuple element access: t.0, t.1
        if (base_type == .tuple) {
            const tidx = std.fmt.parseInt(u32, field_name, 10) catch return ir.null_node;
            const tup = base_type.tuple;
            if (tidx >= tup.element_types.len) return ir.null_node;
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(base_type_idx, tidx));
            const elem_type = tup.element_types[tidx];
            if (base_tag == .ident) {
                if (fb.lookupLocal(self.tree.tokenSlice(self.tree.nodeMainToken(base_idx)))) |lidx|
                    return try fb.emitFieldLocal(lidx, tidx, offset, elem_type, span);
            }
            const base_val = try self.lowerExprNode(base_idx);
            return try fb.emitFieldValue(base_val, tidx, offset, elem_type, span);
        }

        // Struct field access
        const struct_type = switch (base_type) {
            .struct_type => |st| st,
            .pointer => |ptr| blk: {
                const elem_type = self.type_reg.get(ptr.elem);
                if (elem_type == .struct_type) break :blk elem_type.struct_type;
                return ir.null_node;
            },
            else => return ir.null_node,
        };

        var field_idx: u32 = 0;
        var field_offset: i64 = 0;
        var field_type: TypeIndex = TypeRegistry.VOID;
        for (struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                field_idx = @intCast(i);
                field_offset = @intCast(field.offset);
                field_type = field.type_idx;
                break;
            }
        }

        // Effective field type: unwrap packed structs with backing_int
        const effective_field_type = blk: {
            const fti = self.type_reg.get(field_type);
            if (fti == .struct_type and fti.struct_type.backing_int != 0) break :blk fti.struct_type.backing_int;
            break :blk field_type;
        };

        const base_is_pointer = base_type == .pointer;
        if (base_tag == .ident and !base_is_pointer) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            if (fb.lookupLocal(name)) |local_idx|
                return try fb.emitFieldLocal(local_idx, field_idx, field_offset, effective_field_type, span);
            if (self.builder.lookupGlobal(name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, name, ptr_type, span);
                return try fb.emitFieldValue(global_addr, field_idx, field_offset, effective_field_type, span);
            }
        }

        const base_val = try self.lowerExprNode(base_idx);

        // For struct value expressions, store to temp local first for field decomposition
        if (!base_is_pointer and (base_type == .struct_type or base_type == .tuple)) {
            const type_size = self.type_reg.sizeOf(base_type_idx);
            const tmp_local = try fb.addLocalWithSize("__field_tmp", base_type_idx, false, @intCast(type_size));
            _ = try fb.emitStoreLocal(tmp_local, base_val, span);
            return try fb.emitFieldLocal(tmp_local, field_idx, field_offset, effective_field_type, span);
        }

        return try fb.emitFieldValue(base_val, field_idx, field_offset, effective_field_type, span);
    }

    /// Lower `a[i]` — array/slice/string indexing.
    fn lowerIndexExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const base_idx = data.node_and_node[0];
        const index_idx = data.node_and_node[1];
        const span = self.getSpan(idx);

        var base_type_idx = self.inferExprType(base_idx);
        while (self.type_reg.get(base_type_idx) == .pointer) base_type_idx = self.type_reg.get(base_type_idx).pointer.elem;
        const base_type = self.type_reg.get(base_type_idx);
        const elem_type: TypeIndex = if (base_type_idx == TypeRegistry.STRING) TypeRegistry.U8 else switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type);

        // String indexing
        if (base_type_idx == TypeRegistry.STRING) {
            const index_node = try self.lowerExprNode(index_idx);
            const base_tag = self.tree.nodeTag(base_idx);
            if (base_tag == .ident) {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
                if (fb.lookupLocal(name)) |local_idx| {
                    const str_val = try fb.emitLoadLocal(local_idx, TypeRegistry.STRING, span);
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, span);
                }
            }
            const base_val = try self.lowerExprNode(base_idx);
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
            const ptr_val = try fb.emitSlicePtr(base_val, ptr_type, span);
            return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, span);
        }

        const index_node = try self.lowerExprNode(index_idx);
        const base_tag = self.tree.nodeTag(base_idx);

        if (base_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                if (local_type == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, span);
                    const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, span);
                }
                if (local.is_param and self.type_reg.isArray(local.type_idx)) {
                    const ptr_val = try fb.emitLoadLocal(local_idx, local.type_idx, span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, span);
                }
                return try fb.emitIndexLocal(local_idx, index_node, elem_size, elem_type, span);
            }
            if (self.builder.lookupGlobal(name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, name, ptr_type, span);
                return try fb.emitIndexValue(global_addr, index_node, elem_size, elem_type, span);
            }
        }

        const base_val = try self.lowerExprNode(base_idx);
        return try fb.emitIndexValue(base_val, index_node, elem_size, elem_type, span);
    }

    /// Lower struct literal: `Type { field: value, ... }` or anonymous `{ field: value }`.
    fn lowerStructInitExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(idx);
        const span = self.getSpan(idx);

        // Resolve struct type from checker
        const struct_type_idx: TypeIndex = blk: {
            if (self.chk.expr_types.get(idx)) |resolved| break :blk resolved;
            // Try type name from the AST
            if (tag == .struct_init_one) {
                const sio = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.StructInitOne);
                if (sio.type_name_token.unwrap()) |tn| {
                    const type_name = self.tree.tokenSlice(tn);
                    break :blk self.type_reg.lookupByName(type_name) orelse return ir.null_node;
                }
            } else if (tag == .struct_init) {
                const si_data = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.StructInit);
                if (si_data.type_name_token.unwrap()) |tn| {
                    const type_name = self.tree.tokenSlice(tn);
                    break :blk self.type_reg.lookupByName(type_name) orelse return ir.null_node;
                }
            }
            break :blk TypeRegistry.VOID;
        };
        if (struct_type_idx == TypeRegistry.VOID) return ir.null_node;

        var resolved_si_idx = struct_type_idx;
        var type_info = self.type_reg.get(resolved_si_idx);
        if (type_info == .pointer) {
            resolved_si_idx = type_info.pointer.elem;
            type_info = self.type_reg.get(resolved_si_idx);
        }
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return ir.null_node;

        const size = self.type_reg.sizeOf(struct_type_idx);
        const temp_idx = try fb.addLocalWithSize("__struct_tmp", struct_type_idx, true, size);

        // Store fields
        if (tag == .struct_init_one) {
            const sio = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.StructInitOne);
            const fname = self.tree.tokenSlice(sio.field_name_token);
            const value_node = try self.lowerExprNode(sio.field_value);
            for (struct_type.fields, 0..) |struct_field, i| {
                if (std.mem.eql(u8, struct_field.name, fname)) {
                    const fi: u32 = @intCast(i);
                    const fo: i64 = @intCast(struct_field.offset);
                    const field_type_info = self.type_reg.get(struct_field.type_idx);
                    if (struct_field.type_idx == TypeRegistry.STRING or field_type_info == .slice) {
                        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                        const ptr_ir = try fb.emitSlicePtr(value_node, ptr_type, span);
                        const len_ir = try fb.emitSliceLen(value_node, span);
                        _ = try fb.emitStoreLocalField(temp_idx, fi, fo, ptr_ir, span);
                        _ = try fb.emitStoreLocalField(temp_idx, fi + 1, fo + 8, len_ir, span);
                    } else {
                        _ = try fb.emitStoreLocalField(temp_idx, fi, fo, value_node, span);
                    }
                    break;
                }
            }
        } else {
            // struct_init with multiple fields
            const si_data = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.StructInit);
            const field_pairs = self.tree.extraSlice(si_data.fields);
            // Fields are stored as pairs: (name_token, value_node)
            var fi_idx: usize = 0;
            while (fi_idx + 1 < field_pairs.len) : (fi_idx += 2) {
                const fname_tok: TokenIndex = field_pairs[fi_idx];
                const value_raw: Index = @enumFromInt(field_pairs[fi_idx + 1]);
                const fname = self.tree.tokenSlice(fname_tok);
                const value_node = try self.lowerExprNode(value_raw);
                for (struct_type.fields, 0..) |struct_field, si| {
                    if (std.mem.eql(u8, struct_field.name, fname)) {
                        const sfi: u32 = @intCast(si);
                        const sfo: i64 = @intCast(struct_field.offset);
                        const field_type_info = self.type_reg.get(struct_field.type_idx);
                        if (struct_field.type_idx == TypeRegistry.STRING or field_type_info == .slice) {
                            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                            const ptr_ir = try fb.emitSlicePtr(value_node, ptr_type, span);
                            const len_ir = try fb.emitSliceLen(value_node, span);
                            _ = try fb.emitStoreLocalField(temp_idx, sfi, sfo, ptr_ir, span);
                            _ = try fb.emitStoreLocalField(temp_idx, sfi + 1, sfo + 8, len_ir, span);
                        } else {
                            _ = try fb.emitStoreLocalField(temp_idx, sfi, sfo, value_node, span);
                        }
                        break;
                    }
                }
            }
        }
        return try fb.emitLoadLocal(temp_idx, struct_type_idx, span);
    }

    /// Lower `new Type { fields }` — heap allocation + field stores.
    fn lowerNewExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const ne_data = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.NewExprData);
        const type_name = self.tree.tokenSlice(ne_data.type_name_token);
        const span = self.getSpan(idx);

        // Resolve struct type
        const struct_type_idx: TypeIndex = blk: {
            if (self.chk.expr_types.get(idx)) |resolved| {
                if (self.type_reg.isPointer(resolved))
                    break :blk self.type_reg.pointerElem(resolved)
                else
                    break :blk resolved;
            }
            break :blk self.type_reg.lookupByName(type_name) orelse return ir.null_node;
        };
        if (struct_type_idx == TypeRegistry.VOID) return ir.null_node;
        const type_info = self.type_reg.get(struct_type_idx);
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return ir.null_node;

        // ARC path: alloc + field stores via linear memory
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size = self.type_reg.sizeOf(struct_type_idx);
        const total_size = HEAP_HEADER_SIZE + payload_size;

        const qualified_metadata = try self.qualifyTypeName(type_name);
        const metadata_node = try fb.emitTypeMetadata(qualified_metadata, span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const ptr_type = self.type_reg.makePointer(struct_type_idx) catch TypeRegistry.I64;
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, ptr_type, span);

        const temp_name = try self.tempName("__new_ptr");
        const temp_local = try fb.addLocalWithSize(temp_name, ptr_type, true, 8);
        _ = try fb.emitStoreLocal(temp_local, alloc_result, span);
        const ptr_node = try fb.emitLoadLocal(temp_local, ptr_type, span);

        // Initialize fields
        const field_pairs = self.tree.extraSlice(ne_data.fields);
        var fi_idx: usize = 0;
        while (fi_idx + 1 < field_pairs.len) : (fi_idx += 2) {
            const fname_tok: TokenIndex = field_pairs[fi_idx];
            const value_raw: Index = @enumFromInt(field_pairs[fi_idx + 1]);
            const fname = self.tree.tokenSlice(fname_tok);
            const value_node = try self.lowerExprNode(value_raw);
            for (struct_type.fields) |struct_field| {
                if (std.mem.eql(u8, struct_field.name, fname)) {
                    const field_offset: i64 = @intCast(struct_field.offset);
                    const field_type_info = self.type_reg.get(struct_field.type_idx);
                    if (struct_field.type_idx == TypeRegistry.STRING or field_type_info == .slice) {
                        const u8_ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                        const ptr_ir = try fb.emitSlicePtr(value_node, u8_ptr_type, span);
                        const len_ir = try fb.emitSliceLen(value_node, span);
                        const i64_ptr_type = self.type_reg.makePointer(TypeRegistry.I64) catch TypeRegistry.VOID;
                        const ptr_addr = try fb.emitAddrOffset(ptr_node, field_offset, i64_ptr_type, span);
                        _ = try fb.emitPtrStoreValue(ptr_addr, ptr_ir, span);
                        const len_addr = try fb.emitAddrOffset(ptr_node, field_offset + 8, i64_ptr_type, span);
                        _ = try fb.emitPtrStoreValue(len_addr, len_ir, span);
                    } else {
                        const field_ptr_type = self.type_reg.makePointer(struct_field.type_idx) catch TypeRegistry.I64;
                        const field_addr = try fb.emitAddrOffset(ptr_node, field_offset, field_ptr_type, span);
                        _ = try fb.emitPtrStoreValue(field_addr, value_node, span);
                    }
                    break;
                }
            }
        }

        return try fb.emitLoadLocal(temp_local, ptr_type, span);
    }

    /// Lower `@builtin(args)` — dispatch by BuiltinKind.
    fn lowerBuiltinCall(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const bc = self.tree.builtinCallData(idx);
        const span = self.getSpan(idx);

        switch (bc.kind) {
            .size_of => {
                if (bc.type_arg.unwrap()) |ta| {
                    const t = self.resolveTypeNode(ta);
                    const sz = self.type_reg.sizeOf(t);
                    return try fb.emitConstInt(@intCast(sz), TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .assert => {
                if (bc.arg0.unwrap()) |arg| {
                    const cond = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{cond};
                    _ = try fb.emitCall("assert", &args, true, TypeRegistry.VOID, span);
                }
                return ir.null_node;
            },
            .assert_eq => {
                if (bc.arg0.unwrap()) |a0| {
                    if (bc.arg1.unwrap()) |a1| {
                        const left = try self.lowerExprNode(a0);
                        const right = try self.lowerExprNode(a1);
                        const result_type = self.inferExprType(a0);
                        if (result_type == TypeRegistry.STRING) {
                            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                            const l_ptr = try fb.emitSlicePtr(left, ptr_type, span);
                            const l_len = try fb.emitSliceLen(left, span);
                            const r_ptr = try fb.emitSlicePtr(right, ptr_type, span);
                            const r_len = try fb.emitSliceLen(right, span);
                            var args = [_]ir.NodeIndex{ l_ptr, l_len, r_ptr, r_len };
                            _ = try fb.emitCall("assert_eq_string", &args, true, TypeRegistry.VOID, span);
                        } else if (result_type == TypeRegistry.F64 or result_type == TypeRegistry.F32 or result_type == TypeRegistry.FLOAT) {
                            var args = [_]ir.NodeIndex{ left, right };
                            _ = try fb.emitCall("assert_eq_float", &args, true, TypeRegistry.VOID, span);
                        } else {
                            var args = [_]ir.NodeIndex{ left, right };
                            _ = try fb.emitCall("assert_eq", &args, true, TypeRegistry.VOID, span);
                        }
                    }
                }
                return ir.null_node;
            },
            .int_cast => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.I64;
                    return try fb.emitIntCast(operand, target_type, span);
                }
                return ir.null_node;
            },
            .float_cast => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.F64;
                    return try fb.emitConvert(operand, self.inferExprType(arg), target_type, span);
                }
                return ir.null_node;
            },
            .float_from_int => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitConvert(operand, TypeRegistry.I64, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .int_from_float => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitConvert(operand, TypeRegistry.F64, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .ptr_cast => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.I64;
                    return try fb.emitPtrCast(operand, target_type, span);
                }
                return ir.null_node;
            },
            .int_to_ptr => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.I64;
                    return try fb.emitIntToPtr(operand, target_type, span);
                }
                return ir.null_node;
            },
            .ptr_to_int => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitPtrToInt(operand, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .string => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const buf_local = try fb.addLocalWithSize("__str_buf", TypeRegistry.I64, true, 24);
                    const buf_addr = try fb.emitAddrLocal(buf_local, TypeRegistry.I64, span);
                    var args = [_]ir.NodeIndex{ operand, buf_addr };
                    const str_len = try fb.emitCall("int_to_string", &args, false, TypeRegistry.I64, span);
                    const twenty_one = try fb.emitConstInt(21, TypeRegistry.I64, span);
                    const offset = try fb.emitBinary(.sub, twenty_one, str_len, TypeRegistry.I64, span);
                    const str_ptr = try fb.emitBinary(.add, buf_addr, offset, TypeRegistry.I64, span);
                    return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = str_ptr, .len = str_len } }, TypeRegistry.STRING, span));
                }
                return ir.null_node;
            },
            .panic => {
                if (bc.arg0.unwrap()) |arg| {
                    const msg = try self.lowerExprNode(arg);
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const msg_ptr = try fb.emitSlicePtr(msg, ptr_type, span);
                    const msg_len = try fb.emitSliceLen(msg, span);
                    var args = [_]ir.NodeIndex{ msg_ptr, msg_len };
                    _ = try fb.emitCall("panic", &args, true, TypeRegistry.VOID, span);
                }
                return try fb.emitTrap(span);
            },
            .trap => return try fb.emitTrap(span),
            .int_from_bool => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitConvert(operand, TypeRegistry.BOOL, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .int_from_enum => {
                if (bc.arg0.unwrap()) |arg| return try self.lowerExprNode(arg);
                return ir.null_node;
            },
            .enum_from_int => {
                if (bc.arg0.unwrap()) |arg| return try self.lowerExprNode(arg);
                return ir.null_node;
            },
            .tag_name, .enum_name => {
                // These are runtime calls to convert enum value to string
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{operand};
                    return try fb.emitCall("enum_to_string", &args, false, TypeRegistry.STRING, span);
                }
                return ir.null_node;
            },
            .error_name => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{operand};
                    return try fb.emitCall("error_to_string", &args, false, TypeRegistry.STRING, span);
                }
                return ir.null_node;
            },
            .abs => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.abs, operand, self.inferExprType(arg), span);
                }
                return ir.null_node;
            },
            .sqrt => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.sqrt, operand, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .ceil => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.ceil, operand, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .floor => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.floor, operand, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .trunc => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.trunc, operand, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .round => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.nearest, operand, TypeRegistry.F64, span);
                }
                return ir.null_node;
            },
            .min => {
                if (bc.arg0.unwrap()) |a0| {
                    if (bc.arg1.unwrap()) |a1| {
                        const left = try self.lowerExprNode(a0);
                        const right = try self.lowerExprNode(a1);
                        const result_type = self.inferExprType(a0);
                        return try fb.emitBinary(.min, left, right, result_type, span);
                    }
                }
                return ir.null_node;
            },
            .max => {
                if (bc.arg0.unwrap()) |a0| {
                    if (bc.arg1.unwrap()) |a1| {
                        const left = try self.lowerExprNode(a0);
                        const right = try self.lowerExprNode(a1);
                        const result_type = self.inferExprType(a0);
                        return try fb.emitBinary(.max, left, right, result_type, span);
                    }
                }
                return ir.null_node;
            },
            .ctz => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.ctz, operand, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .clz => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.clz, operand, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .pop_count => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    return try fb.emitUnary(.popcnt, operand, TypeRegistry.I64, span);
                }
                return ir.null_node;
            },
            .bit_cast => {
                if (bc.arg0.unwrap()) |arg| return try self.lowerExprNode(arg);
                return ir.null_node;
            },
            .truncate => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.I64;
                    return try fb.emitIntCast(operand, target_type, span);
                }
                return ir.null_node;
            },
            .@"as" => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    const target_type = if (bc.type_arg.unwrap()) |ta| self.resolveTypeNode(ta) else TypeRegistry.I64;
                    return try fb.emitConvert(operand, self.inferExprType(arg), target_type, span);
                }
                return ir.null_node;
            },
            .ptr_of => {
                if (bc.arg0.unwrap()) |arg| {
                    const arg_tag = self.tree.nodeTag(arg);
                    if (arg_tag == .ident) {
                        const name = self.tree.tokenSlice(self.tree.nodeMainToken(arg));
                        if (fb.lookupLocal(name)) |local_idx| {
                            const local_type = fb.locals.items[local_idx].type_idx;
                            const ptr_type = self.type_reg.makePointer(local_type) catch TypeRegistry.I64;
                            return try fb.emitAddrLocal(local_idx, ptr_type, span);
                        }
                    }
                }
                return ir.null_node;
            },
            .len_of => {
                if (bc.arg0.unwrap()) |arg| {
                    const arg_type = self.inferExprType(arg);
                    const arg_info = self.type_reg.get(arg_type);
                    if (arg_info == .array) return try fb.emitConstInt(@intCast(arg_info.array.length), TypeRegistry.I64, span);
                    if (arg_type == TypeRegistry.STRING or arg_info == .slice) {
                        const val = try self.lowerExprNode(arg);
                        return try fb.emitSliceLen(val, span);
                    }
                }
                return ir.null_node;
            },
            .target_os => {
                const os_str = if (self.target.isWasm()) "wasm" else if (self.target.os == .macos) "macos" else "linux";
                return try self.emitConstString(os_str, span);
            },
            .target_arch => {
                const arch_str = if (self.target.isWasm()) "wasm32" else if (self.target.arch == .aarch64) "aarch64" else "x86_64";
                return try self.emitConstString(arch_str, span);
            },
            .target => {
                const tgt_str = if (self.target.isWasm()) "wasm" else "native";
                return try self.emitConstString(tgt_str, span);
            },
            .compile_error => return try fb.emitTrap(span),
            .arc_retain => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{operand};
                    _ = try fb.emitCall("retain", &args, false, TypeRegistry.VOID, span);
                }
                return ir.null_node;
            },
            .arc_release => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{operand};
                    _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, span);
                }
                return ir.null_node;
            },
            .is_unique => {
                if (bc.arg0.unwrap()) |arg| {
                    const operand = try self.lowerExprNode(arg);
                    var args = [_]ir.NodeIndex{operand};
                    return try fb.emitCall("is_unique", &args, false, TypeRegistry.BOOL, span);
                }
                return ir.null_node;
            },
            // Remaining builtins that need more complex handling
            .align_of, .enum_len, .has_field, .type_of, .field,
            .type_name, .type_info, .offset_of, .align_cast,
            .const_cast, .embed_file, .fmin, .fmax,
            .atomic_load, .atomic_store, .atomic_add, .atomic_cas, .atomic_exchange,
            => return ir.null_node,
        }
    }

    /// Lower closure expression: `fn(params) { body }` or `|params| body`.
    fn lowerClosureExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const cd = self.tree.closureData(idx);
        const span = self.getSpan(idx);

        // Generate unique name for closure body function
        const closure_bare = try std.fmt.allocPrint(self.allocator, "__closure_{d}", .{self.closure_counter});
        self.closure_counter += 1;
        const closure_name = try self.qualifyName(closure_bare);

        // Save parent function builder state
        const saved_builder_func = self.builder.current_func;

        const return_type = if (cd.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;
        const closure_uses_sret = self.needsSret(return_type);
        const closure_wasm_return_type = if (closure_uses_sret) TypeRegistry.VOID else return_type;
        self.builder.startFunc(closure_name, TypeRegistry.VOID, closure_wasm_return_type, span);
        if (self.builder.func()) |closure_fb| {
            self.current_func = closure_fb;
            self.cleanup_stack.clear();
            if (closure_uses_sret) closure_fb.sret_return_type = return_type;

            if (closure_uses_sret) {
                _ = try closure_fb.addParam("__sret", TypeRegistry.I64, 8);
            }

            // Add declared params
            const param_slice = self.tree.extraSlice(cd.params);
            for (param_slice) |param_raw| {
                const param_idx: Index = @enumFromInt(param_raw);
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try closure_fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
            }

            // Lower body
            _ = try self.lowerBlockNode(cd.body);
            if (return_type == TypeRegistry.VOID and closure_fb.needsTerminator()) {
                try self.emitCleanups(0);
                _ = try closure_fb.emitRet(null, span);
            }
            self.cleanup_stack.clear();
        }
        try self.builder.endFunc();
        self.builder.current_func = saved_builder_func;
        self.current_func = if (self.builder.current_func) |*bfb| bfb else null;

        // Allocate closure struct: { table_idx }
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size: u32 = 8; // one i64 for table_idx
        const total_size = HEAP_HEADER_SIZE + payload_size;

        const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

        const temp_name = try std.fmt.allocPrint(self.allocator, "__closure_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_local = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(temp_local, alloc_result, span);
        const ptr_node = try fb.emitLoadLocal(temp_local, TypeRegistry.I64, span);

        // Store table_idx at offset 0
        const table_idx_node = try fb.emitFuncAddr(closure_name, TypeRegistry.I64, span);
        _ = try fb.emitPtrStoreValue(ptr_node, table_idx_node, span);

        return try fb.emitLoadLocal(temp_local, TypeRegistry.I64, span);
    }

    /// Lower string interpolation: `"text ${expr} text"`.
    fn lowerStringInterp(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const seg_range = data.extra_range;
        const seg_data = self.tree.extraSlice(seg_range);
        const span = self.getSpan(idx);
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;

        // Segments are encoded as pairs: (kind_token, payload)
        // kind=0 → text literal (payload=token index), kind=1 → expr (payload=node index)
        var parts = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer parts.deinit(self.allocator);

        var si: usize = 0;
        while (si + 1 < seg_data.len) : (si += 2) {
            const kind = seg_data[si];
            const payload = seg_data[si + 1];
            if (kind == 0) {
                // Text segment
                const text = self.tree.tokenSlice(payload);
                if (text.len == 0) continue;
                const copied = try self.allocator.dupe(u8, text);
                const str_idx = try fb.addStringLiteral(copied);
                const str_node = try fb.emitConstSlice(str_idx, span);
                try parts.append(self.allocator, str_node);
            } else {
                // Expression segment
                const expr_idx: Index = @enumFromInt(payload);
                const expr_type = self.inferExprType(expr_idx);
                const expr_val = try self.lowerExprNode(expr_idx);

                if (expr_type == TypeRegistry.STRING) {
                    try parts.append(self.allocator, expr_val);
                } else {
                    // Integer → convert via int_to_string
                    const buf_local = try fb.addLocalWithSize("__interp_buf", TypeRegistry.I64, true, 24);
                    const buf_addr = try fb.emitAddrLocal(buf_local, TypeRegistry.I64, span);
                    var its_args = [_]ir.NodeIndex{ expr_val, buf_addr };
                    const str_len = try fb.emitCall("int_to_string", &its_args, false, TypeRegistry.I64, span);
                    const twenty_one = try fb.emitConstInt(21, TypeRegistry.I64, span);
                    const offset = try fb.emitBinary(.sub, twenty_one, str_len, TypeRegistry.I64, span);
                    const str_ptr = try fb.emitBinary(.add, buf_addr, offset, TypeRegistry.I64, span);
                    const str_node = try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = str_ptr, .len = str_len } }, TypeRegistry.STRING, span));
                    try parts.append(self.allocator, str_node);
                }
            }
        }

        if (parts.items.len == 0) {
            const empty = try self.allocator.dupe(u8, "");
            const str_idx = try fb.addStringLiteral(empty);
            return try fb.emitConstSlice(str_idx, span);
        }

        // Chain all parts with string_concat
        var result = parts.items[0];
        for (parts.items[1..]) |part| {
            const r_ptr = try fb.emitSlicePtr(result, ptr_type, span);
            const r_len = try fb.emitSliceLen(result, span);
            const p_ptr = try fb.emitSlicePtr(part, ptr_type, span);
            const p_len = try fb.emitSliceLen(part, span);
            var concat_args = [_]ir.NodeIndex{ r_ptr, r_len, p_ptr, p_len };
            const new_ptr = try fb.emitCall("string_concat", &concat_args, false, TypeRegistry.I64, span);
            const new_len = try fb.emitBinary(.add, r_len, p_len, TypeRegistry.I64, span);
            result = try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, span));
        }
        return result;
    }

    /// Lower `switch subject { cases }`.
    fn lowerSwitchExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const sw = self.tree.switchData(idx);
        const span = self.getSpan(idx);

        // Determine result type
        var result_type: TypeIndex = self.inferExprType(idx);
        if (result_type == TypeRegistry.VOID or result_type == TypeRegistry.UNTYPED_NULL) {
            result_type = TypeRegistry.VOID;
        }

        const subject_val = try self.lowerExprNode(sw.subject);
        if (subject_val == ir.null_node) return ir.null_node;

        const subject_type = self.inferExprType(sw.subject);
        const saved_enum_type = self.current_switch_enum_type;
        const si = self.type_reg.get(subject_type);
        if (si == .enum_type) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = saved_enum_type;

        // Build blocks for each case + else + merge
        // Cases SubRange contains ExtraIndex values pointing to SwitchCaseData
        const case_extra_indices = self.tree.extraSlice(sw.cases);
        const n_cases = case_extra_indices.len;

        const case_blocks = try self.allocator.alloc(ir.BlockIndex, n_cases);
        defer self.allocator.free(case_blocks);
        for (case_blocks, 0..) |*cb, i| {
            const case_label = try std.fmt.allocPrint(self.allocator, "sw.case.{d}", .{i});
            cb.* = try fb.newBlock(case_label);
        }
        const else_block = try fb.newBlock("sw.else");
        const merge_block = try fb.newBlock("sw.merge");

        // Result local for value-producing switches
        var result_local: ?ir.LocalIdx = null;
        if (result_type != TypeRegistry.VOID) {
            result_local = try fb.addLocalWithSize("__sw_result", result_type, false, self.type_reg.sizeOf(result_type));
        }

        // Emit condition chain: compare subject to each case pattern
        for (case_extra_indices, 0..) |case_extra_raw, ci| {
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_raw);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            const patterns = self.tree.extraSlice(case_data.patterns);

            if (patterns.len == 0) {
                // No patterns = else case
                _ = try fb.emitJump(case_blocks[ci], span);
            } else {
                // Compare subject to first pattern (patterns are node indices)
                const pat_idx: Index = @enumFromInt(patterns[0]);
                const pat_val = try self.lowerExprNode(pat_idx);
                var cond = try fb.emitBinary(.eq, subject_val, pat_val, TypeRegistry.BOOL, span);
                // Multiple patterns: OR them
                for (patterns[1..]) |extra_pat_raw| {
                    const extra_pat: Index = @enumFromInt(extra_pat_raw);
                    const extra_val = try self.lowerExprNode(extra_pat);
                    const extra_cond = try fb.emitBinary(.eq, subject_val, extra_val, TypeRegistry.BOOL, span);
                    cond = try fb.emitBinary(.@"or", cond, extra_cond, TypeRegistry.BOOL, span);
                }
                const next = if (ci + 1 < n_cases) case_blocks[ci + 1] else else_block;
                _ = try fb.emitBranch(cond, case_blocks[ci], next, span);
            }
        }
        // If no case matched and we haven't branched yet
        if (n_cases == 0) {
            _ = try fb.emitJump(else_block, span);
        }

        // Emit case bodies
        fb.beginOverlapGroup();
        for (case_extra_indices, 0..) |case_extra_raw, ci| {
            fb.nextOverlapArm();
            fb.setBlock(case_blocks[ci]);
            const case_extra2: ast_mod.ExtraIndex = @enumFromInt(case_extra_raw);
            const case_data = self.tree.extraData(case_extra2, ast_mod.SwitchCaseData);
            const body_val = try self.lowerExprNode(case_data.body);
            if (result_local) |rl| {
                if (body_val != ir.null_node) _ = try fb.emitStoreLocal(rl, body_val, span);
            }
            if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
        }

        // Else body
        fb.nextOverlapArm();
        fb.setBlock(else_block);
        if (sw.else_body.unwrap()) |eb| {
            const else_val = try self.lowerExprNode(eb);
            if (result_local) |rl| {
                if (else_val != ir.null_node) _ = try fb.emitStoreLocal(rl, else_val, span);
            }
        }
        if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
        fb.endOverlapGroup();

        fb.setBlock(merge_block);
        if (result_local) |rl| return try fb.emitLoadLocal(rl, result_type, span);
        return ir.null_node;
    }

    /// Lower if expression (ternary): `if cond then_val else else_val`.
    fn lowerIfExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const if_data = self.tree.ifData(idx);
        const span = self.getSpan(idx);

        // Comptime const-fold
        if (self.chk.evalConstExpr(if_data.condition)) |cond_val| {
            if (cond_val != 0) return self.lowerExprNode(if_data.then_branch);
            if (if_data.else_branch.unwrap()) |eb| return self.lowerExprNode(eb);
            return ir.null_node;
        }

        // Optional unwrap: if expr |val| { ... }
        if (if_data.capture_token.unwrap() != null) {
            return try self.lowerIfOptionalExpr(idx, if_data);
        }

        const cond = try self.lowerExprNode(if_data.condition);
        if (cond == ir.null_node) return ir.null_node;
        const then_val = try self.lowerExprNode(if_data.then_branch);
        if (then_val == ir.null_node) return ir.null_node;
        const else_val = if (if_data.else_branch.unwrap()) |eb| try self.lowerExprNode(eb) else ir.null_node;
        return try fb.emitSelect(cond, then_val, else_val, self.inferExprType(if_data.then_branch), span);
    }

    /// Lower if-optional expression with capture.
    fn lowerIfOptionalExpr(self: *Lowerer, idx: Index, if_data: ast_mod.full.IfFull) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(idx);

        const opt_type_idx = self.inferExprType(if_data.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const is_compound_opt = opt_info == .optional and !self.isPtrLikeOptional(opt_type_idx);

        const opt_val = try self.lowerExprNode(if_data.condition);
        if (opt_val == ir.null_node) return ir.null_node;

        const is_non_null = if (is_compound_opt) blk: {
            const opt_local = try fb.addLocalWithSize("__opt_ifx", opt_type_idx, false, self.type_reg.sizeOf(opt_type_idx));
            _ = try fb.emitStoreLocal(opt_local, opt_val, span);
            const tag_val = try fb.emitFieldLocal(opt_local, 0, 0, TypeRegistry.I64, span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            break :blk try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);
        } else blk: {
            const null_val = try fb.emit(ir.Node.init(.{ .const_null = {} }, TypeRegistry.UNTYPED_NULL, span));
            break :blk try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, span);
        };

        const result_type = self.inferExprType(if_data.then_branch);
        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__if_opt_result", result_type, false, result_size);

        const then_block = try fb.newBlock("if.opt.then");
        const else_block = try fb.newBlock("if.opt.else");
        const merge_block = try fb.newBlock("if.opt.end");
        _ = try fb.emitBranch(is_non_null, then_block, else_block, span);

        fb.beginOverlapGroup();
        fb.nextOverlapArm();
        fb.setBlock(then_block);
        const scope_depth = fb.markScopeEntry();
        const unwrapped = if (is_compound_opt) blk: {
            const opt_local = fb.lookupLocal("__opt_ifx").?;
            break :blk try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
        } else blk: {
            break :blk try fb.emitUnary(.optional_unwrap, opt_val, elem_type, span);
        };
        const capture_name = self.tree.tokenSlice(if_data.capture_token.unwrap().?);
        const capture_local = try fb.addLocalWithSize(capture_name, elem_type, false, self.type_reg.sizeOf(elem_type));
        _ = try fb.emitStoreLocal(capture_local, unwrapped, span);
        const then_val = try self.lowerExprNode(if_data.then_branch);
        _ = try fb.emitStoreLocal(result_local, then_val, span);
        fb.restoreScope(scope_depth);
        _ = try fb.emitJump(merge_block, span);

        fb.nextOverlapArm();
        fb.setBlock(else_block);
        if (if_data.else_branch.unwrap()) |eb| {
            const else_val = try self.lowerExprNode(eb);
            _ = try fb.emitStoreLocal(result_local, else_val, span);
        }
        _ = try fb.emitJump(merge_block, span);
        fb.endOverlapGroup();

        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, span);
    }

    /// Lower `ptr.*` — pointer dereference.
    fn lowerDerefExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        const operand = try self.lowerExprNode(operand_idx);
        if (operand == ir.null_node) return ir.null_node;

        const operand_type = self.inferExprType(operand_idx);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .pointer) return operand;

        return try fb.emitPtrLoadValue(operand, operand_info.pointer.elem, span);
    }

    /// Lower `&expr` — address-of.
    fn lowerAddrOfExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        const operand_tag = self.tree.nodeTag(operand_idx);
        if (operand_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(operand_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local_type = fb.locals.items[local_idx].type_idx;
                const ptr_type = self.type_reg.makePointer(local_type) catch TypeRegistry.I64;
                return try fb.emitAddrLocal(local_idx, ptr_type, span);
            }
            if (self.builder.lookupGlobal(name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.I64;
                return try fb.emitAddrGlobal(g.idx, name, ptr_type, span);
            }
        }

        // General case: lower expr to temp, take address
        const val = try self.lowerExprNode(operand_idx);
        if (val == ir.null_node) return ir.null_node;
        const val_type = self.inferExprType(operand_idx);
        const size = self.type_reg.sizeOf(val_type);
        const tmp = try fb.addLocalWithSize("__addr_tmp", val_type, false, size);
        _ = try fb.emitStoreLocal(tmp, val, span);
        const ptr_type = self.type_reg.makePointer(val_type) catch TypeRegistry.I64;
        return try fb.emitAddrLocal(tmp, ptr_type, span);
    }

    /// Lower `try expr` — unwrap error union, propagate error.
    fn lowerTryExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        const operand_type = self.inferExprType(operand_idx);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .error_union) return try self.lowerExprNode(operand_idx);
        const elem_type = operand_info.error_union.elem;

        // Call returns POINTER to error union
        const eu_ptr = try self.lowerExprNode(operand_idx);

        // Read tag at [ptr + 0]
        const tag_val = try fb.emitPtrLoadValue(eu_ptr, TypeRegistry.I64, span);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const is_error = try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);

        const err_block = try fb.newBlock("try.err");
        const ok_block = try fb.newBlock("try.ok");
        _ = try fb.emitBranch(is_error, err_block, ok_block, span);

        // Error block: propagate
        fb.setBlock(err_block);
        const our_ret_type = fb.return_type;
        const eu_size = self.type_reg.sizeOf(our_ret_type);
        const err_local = try fb.addLocalWithSize("__try_err", our_ret_type, false, eu_size);
        _ = try fb.emitStoreLocalField(err_local, 0, 0, tag_val, span);
        const payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, span);
        const err_payload = try fb.emitPtrLoadValue(payload_addr, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocalField(err_local, 1, 8, err_payload, span);
        const err_ret = try fb.emitAddrLocal(err_local, TypeRegistry.I64, span);
        try self.emitCleanupsErrorPath(0);
        _ = try fb.emitRet(err_ret, span);

        // OK block: read success payload at [ptr + 8]
        fb.setBlock(ok_block);
        const payload_addr_ok = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, span);
        return try fb.emitPtrLoadValue(payload_addr_ok, elem_type, span);
    }

    /// Lower `await expr` — await async task result.
    fn lowerAwaitExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        // Lower operand (returns task pointer)
        const task_ptr_raw = try self.lowerExprNode(operand_idx);

        const await_task_name = try self.tempName("__await_task");
        const await_task_local = try fb.addLocalWithSize(await_task_name, TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(await_task_local, task_ptr_raw, span);

        // Wasm cooperative polling
        if (self.target.isWasm()) {
            const task_ptr_for_poll = try fb.emitLoadLocal(await_task_local, TypeRegistry.I64, span);
            const sixteen = try fb.emitConstInt(16, TypeRegistry.I64, span);
            const poll_fn_addr = try fb.emitBinary(.add, task_ptr_for_poll, sixteen, TypeRegistry.I64, span);
            const poll_fn_idx = try fb.emitPtrLoadValue(poll_fn_addr, TypeRegistry.I64, span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            const needs_poll = try fb.emitBinary(.ne, poll_fn_idx, zero, TypeRegistry.BOOL, span);
            const poll_block = try fb.newBlock("await.poll");
            const done_block = try fb.newBlock("await.done");
            _ = try fb.emitBranch(needs_poll, poll_block, done_block, span);

            fb.setBlock(poll_block);
            const reload_ptr = try fb.emitLoadLocal(await_task_local, TypeRegistry.I64, span);
            const sixteen2 = try fb.emitConstInt(16, TypeRegistry.I64, span);
            const poll_addr2 = try fb.emitBinary(.add, reload_ptr, sixteen2, TypeRegistry.I64, span);
            const poll_idx2 = try fb.emitPtrLoadValue(poll_addr2, TypeRegistry.I64, span);
            const reload_frame = try fb.emitLoadLocal(await_task_local, TypeRegistry.I64, span);
            var run_args = [_]ir.NodeIndex{ poll_idx2, reload_frame };
            _ = try fb.emitCall("std.executor.run_until_task_done", &run_args, false, TypeRegistry.VOID, span);
            _ = try fb.emitJump(done_block, span);

            fb.setBlock(done_block);
        }

        // Load result from task user data (offset 0)
        const task_ptr = try fb.emitLoadLocal(await_task_local, TypeRegistry.I64, span);
        const result_type = self.inferExprType(operand_idx);
        const inner_type = if (self.type_reg.get(result_type) == .task)
            self.type_reg.get(result_type).task.result_type
        else
            TypeRegistry.I64;
        const result = try fb.emitPtrLoadValue(task_ptr, inner_type, span);

        // Release the task
        var release_args = [_]ir.NodeIndex{task_ptr};
        _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, span);
        return result;
    }

    /// Lower `expr catch |e| fallback` — error union unwrap with fallback.
    fn lowerCatchExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node_and_extra[0];
        const catch_data = self.tree.extraData(data.node_and_extra[1], ast_mod.CatchData);
        const span = self.getSpan(idx);

        const operand_type = self.inferExprType(operand_idx);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .error_union) return try self.lowerExprNode(operand_idx);
        const elem_type = operand_info.error_union.elem;

        const fallback_type_idx = self.inferExprType(catch_data.fallback);
        const is_void_eu_with_value = (elem_type == TypeRegistry.VOID and fallback_type_idx != TypeRegistry.VOID);
        const result_type = if (is_void_eu_with_value) fallback_type_idx else elem_type;

        // Call returns POINTER to error union
        const eu_ptr = try self.lowerExprNode(operand_idx);
        const tag_val = try fb.emitPtrLoadValue(eu_ptr, TypeRegistry.I64, span);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const is_ok = try fb.emitBinary(.eq, tag_val, zero, TypeRegistry.BOOL, span);

        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__catch_result", result_type, false, result_size);

        const ok_block = try fb.newBlock("catch.ok");
        const err_block = try fb.newBlock("catch.err");
        const merge_block = try fb.newBlock("catch.merge");
        _ = try fb.emitBranch(is_ok, ok_block, err_block, span);

        fb.beginOverlapGroup();
        fb.nextOverlapArm();
        // OK: read success payload
        fb.setBlock(ok_block);
        if (is_void_eu_with_value) {
            const default_val = try fb.emitConstInt(0, result_type, span);
            _ = try fb.emitStoreLocal(result_local, default_val, span);
        } else if (elem_type == TypeRegistry.STRING) {
            const ptr_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, span);
            const ptr_val = try fb.emitPtrLoadValue(ptr_addr, TypeRegistry.I64, span);
            const len_addr = try fb.emitAddrOffset(eu_ptr, 16, TypeRegistry.I64, span);
            const len_val = try fb.emitPtrLoadValue(len_addr, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocalField(result_local, 0, 0, ptr_val, span);
            _ = try fb.emitStoreLocalField(result_local, 1, 8, len_val, span);
        } else if (elem_type != TypeRegistry.VOID) {
            const payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, span);
            const success_val = try fb.emitPtrLoadValue(payload_addr, elem_type, span);
            _ = try fb.emitStoreLocal(result_local, success_val, span);
        }
        _ = try fb.emitJump(merge_block, span);

        // Error: evaluate fallback
        fb.nextOverlapArm();
        fb.setBlock(err_block);
        if (catch_data.capture_token.unwrap()) |cap_tok| {
            const capture_name = self.tree.tokenSlice(cap_tok);
            const scope_depth2 = fb.markScopeEntry();
            const err_payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, span);
            const err_val = try fb.emitPtrLoadValue(err_payload_addr, TypeRegistry.I64, span);
            const capture_local = try fb.addLocalWithSize(capture_name, TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(capture_local, err_val, span);
            const fallback_val = try self.lowerExprNode(catch_data.fallback);
            if (fallback_val != ir.null_node) _ = try fb.emitStoreLocal(result_local, fallback_val, span);
            fb.restoreScope(scope_depth2);
        } else {
            const fallback_val = try self.lowerExprNode(catch_data.fallback);
            if (fallback_val != ir.null_node) _ = try fb.emitStoreLocal(result_local, fallback_val, span);
        }
        if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
        fb.endOverlapGroup();

        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, span);
    }

    /// Lower `expr orelse fallback` — optional unwrap with fallback.
    fn lowerOrelseExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node_and_extra[0];
        const orelse_data = self.tree.extraData(data.node_and_extra[1], ast_mod.OrElseData);
        const span = self.getSpan(idx);

        const operand_type_idx = self.inferExprType(operand_idx);
        const operand_info = self.type_reg.get(operand_type_idx);
        if (operand_info != .optional) return try self.lowerExprNode(operand_idx);
        const elem_type = operand_info.optional.elem;
        const is_ptr_like = self.isPtrLikeOptional(operand_type_idx);

        const operand = try self.lowerExprNode(operand_idx);

        if (!is_ptr_like) {
            // Compound optional: tag + payload
            const opt_size = self.type_reg.sizeOf(operand_type_idx);
            const opt_local = try fb.addLocalWithSize("__orelse_opt", operand_type_idx, false, opt_size);
            _ = try fb.emitStoreLocal(opt_local, operand, span);
            const tag = try fb.emitFieldLocal(opt_local, 0, 0, TypeRegistry.I64, span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            const is_non_null = try fb.emitBinary(.ne, tag, zero, TypeRegistry.BOOL, span);

            const result_size = self.type_reg.sizeOf(elem_type);
            const result_local = try fb.addLocalWithSize("__orelse_result", elem_type, false, result_size);
            const then_block = try fb.newBlock("orelse.nonnull");
            const else_block = try fb.newBlock("orelse.null");
            const merge_block = try fb.newBlock("orelse.end");
            _ = try fb.emitBranch(is_non_null, then_block, else_block, span);

            fb.setBlock(then_block);
            const payload = try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
            _ = try fb.emitStoreLocal(result_local, payload, span);
            _ = try fb.emitJump(merge_block, span);

            fb.setBlock(else_block);
            const fallback = try self.lowerExprNode(orelse_data.fallback);
            if (fallback != ir.null_node) _ = try fb.emitStoreLocal(result_local, fallback, span);
            if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);

            fb.setBlock(merge_block);
            return try fb.emitLoadLocal(result_local, elem_type, span);
        } else {
            // Pointer-like optional: null=0 sentinel
            const null_val = try fb.emit(ir.Node.init(.{ .const_null = {} }, TypeRegistry.UNTYPED_NULL, span));
            const is_non_null = try fb.emitBinary(.ne, operand, null_val, TypeRegistry.BOOL, span);
            const then_block = try fb.newBlock("orelse.nonnull");
            const else_block = try fb.newBlock("orelse.null");
            const merge_block = try fb.newBlock("orelse.end");
            _ = try fb.emitBranch(is_non_null, then_block, else_block, span);

            fb.setBlock(then_block);
            const unwrapped = try fb.emitUnary(.optional_unwrap, operand, elem_type, span);
            const result_local = try fb.addLocalWithSize("__orelse_result", elem_type, false, self.type_reg.sizeOf(elem_type));
            _ = try fb.emitStoreLocal(result_local, unwrapped, span);
            _ = try fb.emitJump(merge_block, span);

            fb.setBlock(else_block);
            const fallback = try self.lowerExprNode(orelse_data.fallback);
            if (fallback != ir.null_node) _ = try fb.emitStoreLocal(result_local, fallback, span);
            if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);

            fb.setBlock(merge_block);
            return try fb.emitLoadLocal(result_local, elem_type, span);
        }
    }

    /// Lower `a ++ b` — string/slice/array concatenation.
    fn lowerConcatExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const left_idx = data.node_and_node[0];
        const right_idx = data.node_and_node[1];
        const span = self.getSpan(idx);

        const left = try self.lowerExprNode(left_idx);
        const right = try self.lowerExprNode(right_idx);
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;

        // String concat
        const r_ptr = try fb.emitSlicePtr(left, ptr_type, span);
        const r_len = try fb.emitSliceLen(left, span);
        const p_ptr = try fb.emitSlicePtr(right, ptr_type, span);
        const p_len = try fb.emitSliceLen(right, span);
        var concat_args = [_]ir.NodeIndex{ r_ptr, r_len, p_ptr, p_len };
        const new_ptr = try fb.emitCall("string_concat", &concat_args, false, TypeRegistry.I64, span);
        const new_len = try fb.emitBinary(.add, r_len, p_len, TypeRegistry.I64, span);
        return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, span));
    }

    /// Lower `a |> b` — pipe expression: `b(a)`.
    fn lowerPipeExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const left_idx = data.node_and_node[0];
        const right_idx = data.node_and_node[1];
        const span = self.getSpan(idx);

        const left_val = try self.lowerExprNode(left_idx);

        // Right side should be a function name (ident) — call it with left as arg
        const right_tag = self.tree.nodeTag(right_idx);
        if (right_tag == .ident) {
            const func_name = self.tree.tokenSlice(self.tree.nodeMainToken(right_idx));
            const resolved_name = try self.resolveCallName(func_name);
            var args = [_]ir.NodeIndex{left_val};
            const result_type = self.inferExprType(idx);
            return try fb.emitCall(resolved_name, &args, false, result_type, span);
        }

        return ir.null_node;
    }

    /// Lower `error.Name` — create error union with error tag.
    fn lowerErrorLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const name_token = data.token;
        const error_name = self.tree.tokenSlice(name_token);
        const span = self.getSpan(idx);

        const ret_type = fb.return_type;
        const ret_info = self.type_reg.get(ret_type);
        if (ret_info != .error_union) return ir.null_node;

        const error_idx = self.getGlobalErrorIndex(error_name);
        const size = self.type_reg.sizeOf(ret_type);
        const tmp_local = try fb.addLocalWithSize("__err_tmp", ret_type, false, size);
        const tag_val = try fb.emitConstInt(1, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_val, span);
        const idx_val = try fb.emitConstInt(error_idx, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocalField(tmp_local, 1, 8, idx_val, span);
        return try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, span);
    }

    /// Lower `[a, b, c]` — array literal.
    fn lowerArrayLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(idx);
        const span = self.getSpan(idx);

        if (tag == .array_literal_empty) return ir.null_node;

        var elements = std.ArrayListUnmanaged(Index){};
        defer elements.deinit(self.allocator);

        if (tag == .array_literal_one) {
            const data = self.tree.nodeData(idx);
            try elements.append(self.allocator, data.node);
        } else {
            const data = self.tree.nodeData(idx);
            const elem_range = data.extra_range;
            const elem_indices = self.tree.extraNodes(elem_range);
            for (elem_indices) |ei| try elements.append(self.allocator, ei);
        }

        if (elements.items.len == 0) return ir.null_node;
        const first_elem_type = self.inferExprType(elements.items[0]);
        const elem_size = self.type_reg.sizeOf(first_elem_type);
        const array_type = self.type_reg.makeArray(first_elem_type, elements.items.len) catch return ir.null_node;
        const array_size = self.type_reg.sizeOf(array_type);
        const temp_name = try std.fmt.allocPrint(self.allocator, "__arr_{d}", .{fb.locals.items.len});
        const local_idx = try fb.addLocalWithSize(temp_name, array_type, false, array_size);
        for (elements.items, 0..) |elem_idx, i| {
            const elem_node = try self.lowerExprNode(elem_idx);
            const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
            _ = try fb.emitStoreIndexLocal(local_idx, idx_node, elem_node, elem_size, span);
        }
        return try fb.emitAddrLocal(local_idx, array_type, span);
    }

    /// Lower `(a, b, c)` — tuple literal.
    fn lowerTupleLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const elem_range = data.extra_range;
        const elem_indices = self.tree.extraNodes(elem_range);
        const span = self.getSpan(idx);

        var elem_types = std.ArrayListUnmanaged(TypeIndex){};
        defer elem_types.deinit(self.allocator);
        for (elem_indices) |ei| {
            try elem_types.append(self.allocator, self.inferExprType(ei));
        }
        const tuple_type_idx = self.type_reg.makeTuple(elem_types.items) catch return ir.null_node;
        const size = self.type_reg.sizeOf(tuple_type_idx);
        const temp_local = try fb.addLocalWithSize("__tuple_tmp", tuple_type_idx, true, size);

        for (elem_indices, 0..) |ei, i| {
            const value_node = try self.lowerExprNode(ei);
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(i)));
            _ = try fb.emitStoreLocalField(temp_local, @intCast(i), offset, value_node, span);
        }
        return try fb.emitLoadLocal(temp_local, tuple_type_idx, span);
    }

    /// Lower `a[start..end]` — slice expression.
    fn lowerSliceExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const base_idx = data.node_and_extra[0];
        const slice_data = self.tree.extraData(data.node_and_extra[1], ast_mod.SliceData);
        const span = self.getSpan(idx);

        const base_type_idx = self.inferExprType(base_idx);
        const base_type = self.type_reg.get(base_type_idx);

        // Determine element type
        const elem_type_idx: TypeIndex = if (base_type_idx == TypeRegistry.STRING)
            TypeRegistry.U8
        else switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type_idx);
        const slice_type = if (base_type_idx == TypeRegistry.STRING)
            TypeRegistry.STRING
        else
            self.type_reg.makeSlice(elem_type_idx) catch return ir.null_node;

        var start_node: ?ir.NodeIndex = null;
        if (slice_data.start.unwrap()) |s| start_node = try self.lowerExprNode(s);

        var end_node: ?ir.NodeIndex = null;
        if (slice_data.end.unwrap()) |e| {
            end_node = try self.lowerExprNode(e);
        } else {
            switch (base_type) {
                .array => |a| end_node = try fb.emitConstInt(@intCast(a.length), TypeRegistry.I64, span),
                .slice => {
                    const base_tag = self.tree.nodeTag(base_idx);
                    if (base_tag == .ident) {
                        if (fb.lookupLocal(self.tree.tokenSlice(self.tree.nodeMainToken(base_idx)))) |slice_local| {
                            const slice_val = try fb.emitLoadLocal(slice_local, base_type_idx, span);
                            end_node = try fb.emitSliceLen(slice_val, span);
                        }
                    }
                },
                else => {},
            }
            if (end_node == null and base_type_idx == TypeRegistry.STRING) {
                const base_val = try self.lowerExprNode(base_idx);
                end_node = try fb.emitSliceLen(base_val, span);
            }
        }

        // String slicing
        if (base_type_idx == TypeRegistry.STRING) {
            const base_val = try self.lowerExprNode(base_idx);
            const str_ptr = try fb.emitSlicePtr(base_val, self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64, span);
            const start_val = start_node orelse try fb.emitConstInt(0, TypeRegistry.I64, span);
            const end_val = end_node orelse try fb.emitSliceLen(base_val, span);
            const new_ptr = try fb.emitBinary(.add, str_ptr, start_val, TypeRegistry.I64, span);
            const new_len = try fb.emitBinary(.sub, end_val, start_val, TypeRegistry.I64, span);
            return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, span));
        }

        // Array/slice slicing
        const base_tag = self.tree.nodeTag(base_idx);
        if (base_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                if (local_type == .array) {
                    const ptr_type = self.type_reg.makePointer(elem_type_idx) catch TypeRegistry.I64;
                    const base_addr = try fb.emitAddrLocal(local_idx, ptr_type, span);
                    return try fb.emitMakeSlice(base_addr, start_node, end_node orelse return ir.null_node, elem_size, slice_type, span);
                }
                if (local_type == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, span);
                    const ptr_type = self.type_reg.makePointer(elem_type_idx) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, span);
                    return try fb.emitMakeSlice(ptr_val, start_node, end_node orelse return ir.null_node, elem_size, slice_type, span);
                }
            }
        }

        const base_val = try self.lowerExprNode(base_idx);
        return try fb.emitMakeSlice(base_val, start_node, end_node orelse return ir.null_node, elem_size, slice_type, span);
    }

    /// Lower character literal: `'a'` → integer value.
    fn lowerCharLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
        const span = self.getSpan(idx);
        const char_val = parseCharLiteral(text);
        return try fb.emitConstInt(@intCast(char_val), TypeRegistry.U8, span);
    }

    /// Lower labeled block expression: `blk: { ... break :blk value; }`.
    fn lowerBlockExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(idx);
        const span = self.getSpan(idx);

        switch (tag) {
            .block_one => {
                const data = self.tree.nodeData(idx);
                const stmt_idx = data.node_and_node[0];
                _ = try self.lowerBlockNode(stmt_idx);
                return ir.null_node;
            },
            .block_two => {
                const data = self.tree.nodeData(idx);
                _ = try self.lowerBlockNode(data.node_and_node[0]);
                _ = try self.lowerBlockNode(data.node_and_node[1]);
                return ir.null_node;
            },
            .block => {
                const data = self.tree.nodeData(idx);
                const stmt_range = data.extra_range;
                const stmts = self.tree.extraNodes(stmt_range);
                for (stmts) |stmt_idx| {
                    if (try self.lowerBlockNode(stmt_idx)) break;
                }
                return ir.null_node;
            },
            else => return ir.null_node,
        }
        _ = fb;
        _ = span;
    }

    /// Lower generic type instantiation: `List(int)`, `Map(string, int)`.
    fn lowerGenericCall(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        // Generic calls are resolved at type level, not expression level.
        // The checker resolves them to concrete types. If this is reached
        // in expression position, it's a constructor pattern handled elsewhere.
        _ = self;
        _ = idx;
        return ir.null_node;
    }

    /// Lower `comptime { body }` — evaluate at compile time.
    fn lowerComptimeBlock(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const data = self.tree.nodeData(idx);
        const body_idx = data.node;
        // Lower the body normally — comptime evaluation is done by checker
        return try self.lowerExprNode(body_idx);
    }

    /// Lower `Task { body }` — create async task.
    fn lowerTaskExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const td = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.TaskData);
        const span = self.getSpan(idx);

        // Allocate TaskObject: alloc(metadata=0, size=24)
        const zero_metadata = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const twenty_four = try fb.emitConstInt(24, TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ zero_metadata, twenty_four };
        const task_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

        const task_local_name = try self.tempName("__task_ptr");
        const task_local = try fb.addLocalWithSize(task_local_name, TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(task_local, task_ptr, span);

        // Lower body as block expression
        const body_result = try self.lowerExprNode(td.body);

        // Store result at task[0]
        if (body_result != ir.null_node) {
            const reload_ptr = try fb.emitLoadLocal(task_local, TypeRegistry.I64, span);
            _ = try fb.emitPtrStoreValue(reload_ptr, body_result, span);
        }

        return try fb.emitLoadLocal(task_local, TypeRegistry.I64, span);
    }

    /// Lower `.?` — optional unwrap (force).
    fn lowerUnwrapExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const operand_idx = data.node;
        const span = self.getSpan(idx);

        const operand = try self.lowerExprNode(operand_idx);
        if (operand == ir.null_node) return ir.null_node;

        const operand_type = self.inferExprType(operand_idx);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .optional) return operand;

        const elem_type = operand_info.optional.elem;
        const is_compound = !self.isPtrLikeOptional(operand_type);

        if (is_compound) {
            const opt_size = self.type_reg.sizeOf(operand_type);
            const opt_local = try fb.addLocalWithSize("__unwrap_opt", operand_type, false, opt_size);
            _ = try fb.emitStoreLocal(opt_local, operand, span);
            return try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
        } else {
            return try fb.emitUnary(.optional_unwrap, operand, elem_type, span);
        }
    }

    // ========================================================================
    // Type resolution helpers — stubs, will be ported in subsequent steps
    // ========================================================================

    fn resolveTypeNode(self: *Lowerer, idx: Index) TypeIndex {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .type_named => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                return self.chk.resolveTypeByName(name) orelse .invalid;
            },
            .type_pointer => {
                const data = self.tree.nodeData(idx);
                const elem = self.resolveTypeNode(data.node);
                return self.type_reg.makePointer(elem) catch .invalid;
            },
            .type_optional => {
                const data = self.tree.nodeData(idx);
                const child = self.resolveTypeNode(data.node);
                return self.type_reg.makeOptional(child) catch .invalid;
            },
            else => return .invalid,
        }
    }

    fn resolveParamType(self: *Lowerer, param_idx: Index) TypeIndex {
        // Parameter nodes store type info — extract type expr from extra data
        const tag = self.tree.nodeTag(param_idx);
        _ = tag;
        // Parameters have main_token = name, data = type node
        const data = self.tree.nodeData(param_idx);
        const type_node = data.node_and_opt_node[1];
        if (type_node.unwrap()) |tn| {
            return self.resolveTypeNode(tn);
        }
        return TypeRegistry.I64; // fallback
    }

    fn inferExprType(self: *Lowerer, idx: Index) TypeIndex {
        if (self.chk.expr_types.get(idx)) |t| return t;
        // Fallback: infer from tag
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .literal_int => TypeRegistry.I64,
            .literal_float => TypeRegistry.F64,
            .literal_true, .literal_false => TypeRegistry.BOOL,
            .literal_null => TypeRegistry.UNTYPED_NULL,
            .literal_string => TypeRegistry.STRING,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt,
            .binary_lte, .binary_gte, .binary_and, .binary_or,
            => TypeRegistry.BOOL,
            else => TypeRegistry.I64,
        };
    }

    // ========================================================================
    // emitCopyValue — deep copy for non-trivial types (ARC)
    // Ported from compiler/frontend/lower.zig lines 5012-5261
    // ========================================================================

    fn emitCopyValue(self: *Lowerer, fb: *ir.FuncBuilder, value: ir.NodeIndex, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        if (self.target.isWasm()) return value;
        if (value == ir.null_node) return value;
        const info = self.type_reg.get(type_idx);

        // Trivial types: no copy needed
        if (self.type_reg.isTrivial(type_idx)) return value;

        // Existential copy: memcpy the full container (40 bytes)
        if (info == .existential) {
            const dst_tmp = try fb.addLocalWithSize("__exist_copy_dst", type_idx, false, 40);
            const dst_addr = try fb.emitAddrLocal(dst_tmp, TypeRegistry.I64, span);
            const size_40 = try fb.emitConstInt(40, TypeRegistry.I64, span);
            var mc_args = [_]ir.NodeIndex{ dst_addr, value, size_40 };
            _ = try fb.emitCall("memcpy", &mc_args, false, TypeRegistry.VOID, span);
            return try fb.emitLoadLocal(dst_tmp, type_idx, span);
        }

        // VWT dispatch: call witness function for all non-trivial types
        {
            const type_name = self.type_reg.typeName(type_idx);
            const copy_name = try std.fmt.allocPrint(self.allocator, "__vwt_initializeWithCopy_{s}", .{type_name});
            const val_size = self.type_reg.sizeOf(type_idx);
            const src_tmp = try fb.addLocalWithSize("__vwt_src", type_idx, false, val_size);
            _ = try fb.emitStoreLocal(src_tmp, value, span);
            const src_addr = try fb.emitAddrLocal(src_tmp, TypeRegistry.I64, span);
            const dst_tmp = try fb.addLocalWithSize("__vwt_dst", type_idx, false, val_size);
            const dst_addr = try fb.emitAddrLocal(dst_tmp, TypeRegistry.I64, span);
            const meta_name = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{type_name});
            const metadata = if (self.builder.lookupGlobal(meta_name)) |gi|
                try fb.emitAddrGlobal(gi.idx, meta_name, TypeRegistry.I64, span)
            else
                try fb.emitConstInt(0, TypeRegistry.I64, span);
            var args = [_]ir.NodeIndex{ dst_addr, src_addr, metadata };
            _ = try fb.emitCall(copy_name, &args, false, TypeRegistry.I64, span);
            return try fb.emitPtrLoadValue(dst_addr, type_idx, span);
        }

        // NOTE: Inline ARC fallback code below is unreachable when VWT witnesses
        // are emitted (the unconditional block above handles all cases).
        // Kept as documentation for the patterns VWT replaces.
    }

    // ========================================================================
    // emitDestroyValue — deep destroy at scope exit (ARC)
    // Ported from compiler/frontend/lower.zig lines 5266-5533
    // ========================================================================

    fn emitDestroyValue(self: *Lowerer, fb: *ir.FuncBuilder, value: ir.NodeIndex, type_idx: TypeIndex, span: Span) Error!void {
        if (self.target.isWasm()) return;
        if (value == ir.null_node) return;
        const info = self.type_reg.get(type_idx);

        // Trivial types: no destroy needed
        if (self.type_reg.isTrivial(type_idx)) return;

        // Existential destroy: noop for now (stack-allocated containers)
        if (info == .existential) return;

        // VWT dispatch: call witness function for all non-trivial types
        {
            const type_name = self.type_reg.typeName(type_idx);
            const destroy_name = try std.fmt.allocPrint(self.allocator, "__vwt_destroy_{s}", .{type_name});
            const val_size = self.type_reg.sizeOf(type_idx);
            const tmp = try fb.addLocalWithSize("__vwt_tmp", type_idx, false, val_size);
            _ = try fb.emitStoreLocal(tmp, value, span);
            const addr = try fb.emitAddrLocal(tmp, TypeRegistry.I64, span);
            const meta_name = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{type_name});
            const metadata = if (self.builder.lookupGlobal(meta_name)) |gi|
                try fb.emitAddrGlobal(gi.idx, meta_name, TypeRegistry.I64, span)
            else
                try fb.emitConstInt(0, TypeRegistry.I64, span);
            var args = [_]ir.NodeIndex{ addr, metadata };
            _ = try fb.emitCall(destroy_name, &args, false, TypeRegistry.VOID, span);
            return;
        }
    }

    // ========================================================================
    // emitOptionalFieldRetain — conditional retain for ?*T optional field values
    // Ported from compiler/frontend/lower.zig lines 5539-5564
    // ========================================================================

    fn emitOptionalFieldRetain(self: *Lowerer, fb: *ir.FuncBuilder, value_node: ir.NodeIndex, opt_type_idx: TypeIndex, span: Span) Error!void {
        const opt_info = self.type_reg.get(opt_type_idx);
        if (opt_info != .optional) return;
        const elem_info = self.type_reg.get(opt_info.optional.elem);
        if (elem_info != .pointer or !elem_info.pointer.managed) return;

        const opt_size = self.type_reg.sizeOf(opt_type_idx);
        const tmp = try fb.addLocalWithSize("__opt_retain_tmp", opt_type_idx, false, opt_size);
        _ = try fb.emitStoreLocal(tmp, value_node, span);
        const tag = try fb.emitFieldLocal(tmp, 0, 0, TypeRegistry.I64, span);
        const ptr_val = try fb.emitFieldLocal(tmp, 1, 8, opt_info.optional.elem, span);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const is_non_null = try fb.emitBinary(.ne, tag, zero, TypeRegistry.BOOL, span);
        const retain_blk = try fb.newBlock("opt_ret.retain");
        const skip_blk = try fb.newBlock("opt_ret.skip");
        _ = try fb.emitBranch(is_non_null, retain_blk, skip_blk, span);
        fb.setBlock(retain_blk);
        var retain_args = [_]ir.NodeIndex{ptr_val};
        _ = try fb.emitCall("retain", &retain_args, false, opt_info.optional.elem, span);
        _ = try fb.emitJump(skip_blk, span);
        fb.setBlock(skip_blk);
    }

    // ========================================================================
    // lowerMethodCall — method call lowering with receiver handling
    // Ported from compiler/frontend/lower.zig lines 10521-10908
    // ========================================================================

    fn lowerMethodCall(
        self: *Lowerer,
        call_idx: Index,
        base_idx: Index,
        _: []const u8, // field_name (used for debug logging in compiler)
        method_info: types_mod.MethodInfo,
        receiver_type_name: []const u8,
    ) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(call_idx);
        const cd = self.tree.callData(call_idx);

        // Queue generic impl method for deferred lowering
        if (self.chk.generics.generic_inst_by_name.get(method_info.func_name)) |inst_info| {
            try self.ensureGenericFnQueued(inst_info);
        }

        const is_generic_call = self.chk.generics.generic_inst_by_name.contains(method_info.func_name);

        var args = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer args.deinit(self.allocator);

        // Static methods: no receiver
        const receiver_offset: usize = if (method_info.is_static) 0 else 1;

        if (!method_info.is_static) {
            const base_type_idx = self.inferExprType(base_idx);
            const base_type = self.type_reg.get(base_type_idx);

            // Prepare receiver
            const receiver_val = blk: {
                if (method_info.receiver_is_ptr) {
                    if (base_type == .pointer) {
                        break :blk try self.lowerExprNode(base_idx);
                    } else {
                        // Need address-of for non-pointer base
                        const base_tag = self.tree.nodeTag(base_idx);
                        if (base_tag == .ident) {
                            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
                            if (fb.lookupLocal(name)) |local_idx| {
                                const ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.I64;
                                break :blk try fb.emitAddrLocal(local_idx, ptr_type, span);
                            }
                            if (self.builder.lookupGlobal(name)) |g| {
                                const ptr_type = self.type_reg.makePointer(base_type_idx) catch base_type_idx;
                                break :blk try fb.emitAddrGlobal(g.idx, name, ptr_type, span);
                            }
                        }
                        // Non-lvalue expression — spill to temp local
                        const val = try self.lowerExprNode(base_idx);
                        const ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.I64;
                        const tmp_local = try fb.addLocalWithSize("__method_recv", base_type_idx, true, self.type_reg.sizeOf(base_type_idx));
                        _ = try fb.emitStoreLocal(tmp_local, val, span);
                        break :blk try fb.emitAddrLocal(tmp_local, ptr_type, span);
                    }
                } else {
                    if (base_type == .pointer) {
                        const ptr_val = try self.lowerExprNode(base_idx);
                        break :blk try fb.emitPtrLoadValue(ptr_val, base_type_idx, span);
                    } else {
                        break :blk try self.lowerExprNode(base_idx);
                    }
                }
            };

            if (receiver_val == ir.null_node) return ir.null_node;
            try args.append(self.allocator, receiver_val);
        }

        // Get param types for ABI decisions
        const func_type_for_params = self.type_reg.get(method_info.func_type);
        var param_types: ?[]const types_mod.FuncParam = null;
        if (func_type_for_params == .func) param_types = func_type_for_params.func.params;

        // Collect call args
        var call_args = try self.collectCallArgsList(cd);
        defer call_args.deinit(self.allocator);

        for (call_args.items, 0..) |arg_idx, arg_i| {
            var arg_node = try self.lowerExprNode(arg_idx);
            if (arg_node == ir.null_node) continue;

            // Determine parameter type for ABI decomposition (offset by receiver_offset)
            var param_is_pointer = false;
            var param_is_compound = false;
            if (param_types) |params| {
                const param_idx = arg_i + receiver_offset;
                if (param_idx < params.len) {
                    const param_type = self.type_reg.get(params[param_idx].type_idx);
                    param_is_pointer = (param_type == .pointer);
                    param_is_compound = (param_type == .slice) or (params[param_idx].type_idx == TypeRegistry.STRING);
                }
            }

            // @safe auto-ref: pass struct value as pointer to *Struct param
            if (self.chk.safe_mode and param_is_pointer) {
                if (param_types) |params| {
                    const param_idx = arg_i + receiver_offset;
                    if (param_idx < params.len) {
                        const param_info = self.type_reg.get(params[param_idx].type_idx);
                        if (param_info == .pointer) {
                            const arg_type = self.inferExprType(arg_idx);
                            const arg_info = self.type_reg.get(arg_type);
                            if ((arg_info == .struct_type or arg_info == .union_type) and self.type_reg.isAssignable(arg_type, param_info.pointer.elem)) {
                                if (!self.target.isWasmGC()) {
                                    const ptr_type = self.type_reg.makePointer(arg_type) catch TypeRegistry.I64;
                                    const at = self.tree.nodeTag(arg_idx);
                                    if (at == .ident) {
                                        const aname = self.tree.tokenSlice(self.tree.nodeMainToken(arg_idx));
                                        if (fb.lookupLocal(aname)) |local_idx| {
                                            arg_node = try fb.emitAddrLocal(local_idx, ptr_type, span);
                                        } else {
                                            const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                            _ = try fb.emitStoreLocal(tmp_local, arg_node, span);
                                            arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, span);
                                        }
                                    } else {
                                        const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                        _ = try fb.emitStoreLocal(tmp_local, arg_node, span);
                                        arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, span);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // @safe auto-deref: when arg is *T but param expects T
            if (self.chk.safe_mode and !param_is_pointer) {
                if (param_types) |params| {
                    const param_idx_d = arg_i + receiver_offset;
                    if (param_idx_d < params.len) {
                        const arg_type = self.inferExprType(arg_idx);
                        const arg_info = self.type_reg.get(arg_type);
                        const param_tidx = params[param_idx_d].type_idx;
                        if (arg_info == .pointer and (self.type_reg.get(arg_info.pointer.elem) == .struct_type or self.type_reg.get(arg_info.pointer.elem) == .union_type) and
                            self.type_reg.isAssignable(arg_info.pointer.elem, param_tidx))
                        {
                            arg_node = try fb.emitPtrLoadValue(arg_node, param_tidx, span);
                        }
                    }
                }
            }

            // T → ?T wrapping at call sites
            if (param_types) |params| {
                const param_idx_opt = arg_i + receiver_offset;
                if (param_idx_opt < params.len) {
                    const param_type = self.type_reg.get(params[param_idx_opt].type_idx);
                    if (param_type == .optional and !self.isPtrLikeOptional(params[param_idx_opt].type_idx)) {
                        const arg_type = self.inferExprType(arg_idx);
                        if (arg_type == TypeRegistry.UNTYPED_NULL) {
                            const opt_size = self.type_reg.sizeOf(params[param_idx_opt].type_idx);
                            const tmp = try fb.addLocalWithSize("__opt_arg", params[param_idx_opt].type_idx, false, opt_size);
                            const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                            _ = try fb.emitStoreLocalField(tmp, 0, 0, tag_zero, span);
                            arg_node = try fb.emitLoadLocal(tmp, params[param_idx_opt].type_idx, span);
                        } else {
                            const arg_info = self.type_reg.get(arg_type);
                            if (arg_info != .optional) {
                                const opt_size = self.type_reg.sizeOf(params[param_idx_opt].type_idx);
                                const tmp = try fb.addLocalWithSize("__opt_arg", params[param_idx_opt].type_idx, false, opt_size);
                                const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                                _ = try fb.emitStoreLocalField(tmp, 0, 0, tag_one, span);
                                _ = try fb.emitStoreLocalField(tmp, 1, 8, arg_node, span);
                                arg_node = try fb.emitLoadLocal(tmp, params[param_idx_opt].type_idx, span);
                            }
                        }
                    }
                }
            }

            // Decompose compound types
            if (param_is_compound) {
                const ptr_val = try fb.emitSlicePtr(arg_node, TypeRegistry.I64, span);
                const len_val = try fb.emitSliceLen(arg_node, span);
                try args.append(self.allocator, ptr_val);
                try args.append(self.allocator, len_val);
            } else {
                // Decompose large compound args (struct/union/tuple > 8 bytes)
                var is_large_compound = false;
                if (param_types) |params| {
                    const param_idx = arg_i + receiver_offset;
                    if (param_idx < params.len) {
                        const pt = self.type_reg.get(params[param_idx].type_idx);
                        const ps = self.type_reg.sizeOf(params[param_idx].type_idx);
                        const is_compound_opt = pt == .optional and !self.isPtrLikeOptional(params[param_idx].type_idx);
                        is_large_compound = !self.target.isWasmGC() and (pt == .struct_type or pt == .union_type or pt == .tuple or is_compound_opt) and ps > 8;
                    }
                }
                if (is_large_compound) {
                    const param_idx = arg_i + receiver_offset;
                    const param_type_idx = param_types.?[param_idx].type_idx;
                    const param_size = self.type_reg.sizeOf(param_type_idx);
                    const num_slots: u32 = @intCast((param_size + 7) / 8);
                    for (0..num_slots) |slot| {
                        const offset: i64 = @intCast(slot * 8);
                        const field = try fb.emitFieldValue(arg_node, @intCast(slot), offset, TypeRegistry.I64, span);
                        try args.append(self.allocator, field);
                    }
                } else {
                    try args.append(self.allocator, arg_node);
                }
            }
        }

        const func_type = self.type_reg.get(method_info.func_type);
        const return_type = if (func_type == .func) func_type.func.return_type else TypeRegistry.VOID;

        // Resolve method call target
        const method_link_name = if (is_generic_call)
            self.computeGenericBaseName(method_info.func_name) catch method_info.func_name
        else
            try self.resolveMethodName(receiver_type_name, method_info.func_name, method_info.source_tree);

        // SRET for method calls
        if (self.needsSret(return_type)) {
            const ret_size = self.type_reg.sizeOf(return_type);
            const sret_local = try fb.getOrCreateSretLocal(return_type, ret_size);
            const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, span);
            var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer sret_args.deinit(self.allocator);
            try sret_args.append(self.allocator, sret_addr);
            try sret_args.appendSlice(self.allocator, args.items);
            _ = try fb.emitCall(method_link_name, sret_args.items, false, TypeRegistry.VOID, span);
            return try fb.emitLoadLocal(sret_local, return_type, span);
        }
        return try fb.emitCall(method_link_name, args.items, false, return_type, span);
    }

    // ========================================================================
    // Generic function helpers
    // ========================================================================

    /// VWT base-name sharing: strip type args to get shared body name.
    /// "myFunc(i64)" → "myFunc", "Type_method(i64;string)" → "Type_method"
    fn computeGenericBaseName(self: *Lowerer, concrete_name: []const u8) ![]const u8 {
        // Find the opening paren that starts type args
        var paren_pos: ?usize = null;
        for (concrete_name, 0..) |c, i| {
            if (c == '(') {
                paren_pos = i;
                break;
            }
        }
        if (paren_pos) |pos| {
            return try self.allocator.dupe(u8, concrete_name[0..pos]);
        }
        return concrete_name;
    }

    /// Resolve method call target name, qualifying with module if needed.
    fn resolveMethodName(self: *Lowerer, receiver_type_name: []const u8, func_name: []const u8, source_tree: ?*const Ast) ![]const u8 {
        // If method has a source_tree, resolve module name from tree_module_map
        if (source_tree) |st| {
            if (self.tree_module_map) |map| {
                if (map.get(st)) |target_module| {
                    return std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ target_module, func_name });
                }
            }
        }
        // Check if the func_name already contains a dot (already qualified)
        for (func_name) |c| {
            if (c == '.') return func_name;
        }
        // Check if the receiver type has associated module
        _ = receiver_type_name;
        return self.qualifyName(func_name);
    }

    // ========================================================================
    // Global init generation
    // ========================================================================

    pub fn generateGlobalInits(self: *Lowerer) !void {
        return self.generateGlobalInitsNamed("__cot_init_globals");
    }

    pub fn generateGlobalInitsNamed(self: *Lowerer, name: []const u8) !void {
        const span = Span.zero;
        self.builder.startFunc(name, TypeRegistry.VOID, TypeRegistry.VOID, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            for (self.pending_global_inits.items) |gi| {
                const value = try self.lowerExprNode(gi.value_node);
                _ = try fb.emitGlobalStore(gi.global_idx, gi.name, value, gi.span);
            }
            _ = try fb.emitRet(null, span);
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    // ========================================================================
    // Group 4: Compound optional helpers
    // Ported from compiler/frontend/lower.zig lines 248-469
    // ========================================================================

    /// Lower compound orelse: left orelse right, where left is a compound optional.
    fn lowerCompoundOrelse(self: *Lowerer, fb: *ir.FuncBuilder, left: ir.NodeIndex, right_idx: Index, left_type_idx: TypeIndex, result_type: TypeIndex, span: Span) Error!ir.NodeIndex {
        const opt_size = self.type_reg.sizeOf(left_type_idx);
        const opt_local = try fb.addLocalWithSize("__orelse_opt", left_type_idx, false, opt_size);
        _ = try fb.emitStoreLocal(opt_local, left, span);
        const tag = try fb.emitFieldLocal(opt_local, 0, 0, TypeRegistry.I64, span);
        const zero_tag = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const is_non_null = try fb.emitBinary(.ne, tag, zero_tag, TypeRegistry.BOOL, span);

        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__orelse_result", result_type, false, result_size);
        const then_block = try fb.newBlock("orelse.nonnull");
        const else_block = try fb.newBlock("orelse.null");
        const merge_block = try fb.newBlock("orelse.end");
        _ = try fb.emitBranch(is_non_null, then_block, else_block, span);

        // Non-null path: extract payload from compound optional
        fb.setBlock(then_block);
        const left_info = self.type_reg.get(left_type_idx);
        const elem_type = if (left_info == .optional) left_info.optional.elem else result_type;
        const payload = try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
        _ = try fb.emitStoreLocal(result_local, payload, span);
        _ = try fb.emitJump(merge_block, span);

        // Null path: lower fallback expression HERE (in else_block, not before the branch)
        fb.setBlock(else_block);
        const fallback = try self.lowerExprNode(right_idx);
        if (fallback != ir.null_node) {
            _ = try fb.emitStoreLocal(result_local, fallback, span);
        }
        _ = try fb.emitJump(merge_block, span);

        // Merge: load result
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, span);
    }

    /// Short-circuit and/or at IR level: defer right-side lowering until inside the branch.
    fn lowerShortCircuit(self: *Lowerer, fb: *ir.FuncBuilder, left: ir.NodeIndex, right_idx: Index, is_and: bool, span: Span) Error!ir.NodeIndex {
        const result_local = try fb.addLocalWithSize("__sc_result", TypeRegistry.BOOL, false, 8);
        const eval_right_block = try fb.newBlock("sc.eval_right");
        const short_circuit_block = try fb.newBlock("sc.short");
        const merge_block = try fb.newBlock("sc.merge");

        if (is_and) {
            _ = try fb.emitBranch(left, eval_right_block, short_circuit_block, span);
        } else {
            _ = try fb.emitBranch(left, short_circuit_block, eval_right_block, span);
        }

        // Short circuit block: result = false (for and) or true (for or)
        fb.setBlock(short_circuit_block);
        const short_val = try fb.emitConstBool(!is_and, span);
        _ = try fb.emitStoreLocal(result_local, short_val, span);
        _ = try fb.emitJump(merge_block, span);

        // Eval right block: lower right operand HERE (deferred, after branch)
        fb.setBlock(eval_right_block);
        const right = try self.lowerExprNode(right_idx);
        if (right != ir.null_node) {
            _ = try fb.emitStoreLocal(result_local, right, span);
        }
        _ = try fb.emitJump(merge_block, span);

        // Merge: load result
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, TypeRegistry.BOOL, span);
    }

    /// Store a value into a compound optional result local, wrapping T -> ?T as needed.
    fn storeCompoundOptArm(self: *Lowerer, fb: *ir.FuncBuilder, result_local: ir.LocalIdx, val: ir.NodeIndex, body_idx: Index, span: Span) !void {
        // Check if arm value is a null literal
        const body_tag = self.tree.nodeTag(body_idx);
        const is_null = (body_tag == .literal_null);
        if (is_null) {
            const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocalField(result_local, 0, 0, tag_zero, span);
        } else {
            const arm_type = self.inferExprType(body_idx);
            const arm_info = self.type_reg.get(arm_type);
            if (arm_info == .optional and !self.isPtrLikeOptional(arm_type)) {
                _ = try fb.emitStoreLocal(result_local, val, span);
            } else {
                const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(result_local, 0, 0, tag_one, span);
                const arm_size = self.type_reg.sizeOf(arm_type);
                if (arm_size > 8) {
                    const val_tmp = try fb.addLocalWithSize("__opt_arm_tmp", arm_type, false, arm_size);
                    _ = try fb.emitStoreLocal(val_tmp, val, span);
                    const num_words: u32 = @intCast((arm_size + 7) / 8);
                    for (0..num_words) |i| {
                        const off: i64 = @intCast(i * 8);
                        const word = try fb.emitFieldLocal(val_tmp, @intCast(i), off, TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(result_local, @as(u32, @intCast(1 + i)), @as(i64, 8) + off, word, span);
                    }
                } else {
                    _ = try fb.emitStoreLocalField(result_local, 1, 8, val, span);
                }
            }
        }
    }

    /// Store compound optional via pointer base (for struct field assign through pointer).
    fn storeCompoundOptFieldPtr(self: *Lowerer, fb: *ir.FuncBuilder, ptr: ir.NodeIndex, field_idx: u32, field_offset: i64, val: ir.NodeIndex, ast_idx: Index, span: Span) !void {
        const is_null = (self.tree.nodeTag(ast_idx) == .literal_null);
        if (is_null) {
            const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreFieldValue(ptr, field_idx, field_offset, tag_zero, span);
        } else {
            const arm_type = self.inferExprType(ast_idx);
            const arm_info = self.type_reg.get(arm_type);
            if (arm_info == .optional and !self.isPtrLikeOptional(arm_type)) {
                const arm_size = self.type_reg.sizeOf(arm_type);
                const num_words: u32 = arm_size / 8;
                const tmp = try fb.addLocalWithSize("__opt_fap_tmp", arm_type, false, arm_size);
                _ = try fb.emitStoreLocal(tmp, val, span);
                for (0..num_words) |i| {
                    const offset: i64 = @intCast(i * 8);
                    const field = try fb.emitFieldLocal(tmp, @intCast(i), offset, TypeRegistry.I64, span);
                    _ = try fb.emitStoreFieldValue(ptr, field_idx + @as(u32, @intCast(i)), field_offset + offset, field, span);
                }
            } else {
                const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                _ = try fb.emitStoreFieldValue(ptr, field_idx, field_offset, tag_one, span);
                const arm_size = self.type_reg.sizeOf(arm_type);
                if (arm_size > 8) {
                    const val_tmp = try fb.addLocalWithSize("__opt_fap_val", arm_type, false, arm_size);
                    _ = try fb.emitStoreLocal(val_tmp, val, span);
                    const num_words: u32 = @intCast((arm_size + 7) / 8);
                    for (0..num_words) |i| {
                        const off: i64 = @intCast(i * 8);
                        const word = try fb.emitFieldLocal(val_tmp, @intCast(i), off, TypeRegistry.I64, span);
                        _ = try fb.emitStoreFieldValue(ptr, field_idx + @as(u32, @intCast(1 + i)), field_offset + @as(i64, 8) + off, word, span);
                    }
                } else {
                    _ = try fb.emitStoreFieldValue(ptr, field_idx + 1, field_offset + 8, val, span);
                }
            }
        }
    }

    /// Store a value into a compound optional struct field, wrapping T -> ?T as needed.
    fn storeCompoundOptField(self: *Lowerer, fb: *ir.FuncBuilder, local_idx: ir.LocalIdx, field_idx: u32, field_offset: i64, val: ir.NodeIndex, ast_idx: Index, span: Span) !void {
        const is_null = (self.tree.nodeTag(ast_idx) == .literal_null);
        if (is_null) {
            const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocalField(local_idx, field_idx, field_offset, tag_zero, span);
        } else {
            const arm_type = self.inferExprType(ast_idx);
            const arm_info = self.type_reg.get(arm_type);
            if (arm_info == .optional and !self.isPtrLikeOptional(arm_type)) {
                const arm_size = self.type_reg.sizeOf(arm_type);
                const num_words: u32 = arm_size / 8;
                const tmp = try fb.addLocalWithSize("__opt_field_tmp", arm_type, false, arm_size);
                _ = try fb.emitStoreLocal(tmp, val, span);
                for (0..num_words) |i| {
                    const offset: i64 = @intCast(i * 8);
                    const field = try fb.emitFieldLocal(tmp, @intCast(i), offset, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocalField(local_idx, field_idx + @as(u32, @intCast(i)), field_offset + offset, field, span);
                }
            } else {
                const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(local_idx, field_idx, field_offset, tag_one, span);
                const arm_size = self.type_reg.sizeOf(arm_type);
                if (arm_size > 8) {
                    const val_tmp = try fb.addLocalWithSize("__opt_fld_val", arm_type, false, arm_size);
                    _ = try fb.emitStoreLocal(val_tmp, val, span);
                    const num_words: u32 = @intCast((arm_size + 7) / 8);
                    for (0..num_words) |i| {
                        const off: i64 = @intCast(i * 8);
                        const word = try fb.emitFieldLocal(val_tmp, @intCast(i), off, TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(local_idx, field_idx + @as(u32, @intCast(1 + i)), field_offset + @as(i64, 8) + off, word, span);
                    }
                } else {
                    _ = try fb.emitStoreLocalField(local_idx, field_idx + 1, field_offset + 8, val, span);
                }
            }
        }
    }

    // ========================================================================
    // Group 6: Assignment helpers
    // Ported from compiler/frontend/lower.zig lines 3535-3966
    // ========================================================================

    /// Lower field assignment: `obj.field = value`.
    fn lowerFieldAssign(self: *Lowerer, target_idx: Index, value_node: ir.NodeIndex, rhs_ast: Index, span: Span) !void {
        const fb = self.current_func orelse return;
        const data = self.tree.nodeData(target_idx);
        const base_idx = data.node_and_token[0];
        const field_token = data.node_and_token[1];
        const field_name = self.tree.tokenSlice(field_token);

        const base_type_idx = self.inferExprType(base_idx);
        const base_type = self.type_reg.get(base_type_idx);

        const struct_type = switch (base_type) {
            .struct_type => |st| st,
            .pointer => |ptr| blk: {
                const elem_type = self.type_reg.get(ptr.elem);
                if (elem_type == .struct_type) break :blk elem_type.struct_type;
                return;
            },
            else => return,
        };

        for (struct_type.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                const fld_idx: u32 = @intCast(i);
                const fld_offset: i64 = @intCast(field.offset);

                if (base_type == .pointer) {
                    const ptr_val = try self.lowerExprNode(base_idx);
                    const fld_info = self.type_reg.get(field.type_idx);
                    if (fld_info == .optional and !self.isPtrLikeOptional(field.type_idx)) {
                        try self.storeCompoundOptFieldPtr(fb, ptr_val, fld_idx, fld_offset, value_node, rhs_ast, span);
                    } else if ((fld_info == .struct_type or fld_info == .tuple or fld_info == .union_type) and self.type_reg.sizeOf(field.type_idx) > 8) {
                        const fld_size = self.type_reg.sizeOf(field.type_idx);
                        const num_words = fld_size / 8;
                        const tmp_local = try fb.addLocalWithSize("__fld_assign_src", field.type_idx, false, fld_size);
                        _ = try fb.emitStoreLocal(tmp_local, value_node, span);
                        for (0..num_words) |w| {
                            const w_offset: i64 = @intCast(w * 8);
                            const word = try fb.emitFieldLocal(tmp_local, @intCast(w), w_offset, TypeRegistry.I64, span);
                            _ = try fb.emitStoreField(ptr_val, fld_idx, fld_offset + w_offset, word, span);
                        }
                    } else {
                        // ARC: retain/release for managed pointer fields
                        if (self.type_reg.get(field.type_idx) == .pointer and self.type_reg.get(field.type_idx).pointer.managed and !self.target.isWasm()) {
                            const old_val = try fb.emitFieldValue(ptr_val, fld_idx, fld_offset, field.type_idx, span);
                            _ = try self.emitCopyValue(fb, value_node, field.type_idx, span);
                            _ = try fb.emitStoreFieldValue(ptr_val, fld_idx, fld_offset, value_node, span);
                            try self.emitDestroyValue(fb, old_val, field.type_idx, span);
                        } else {
                            _ = try fb.emitStoreFieldValue(ptr_val, fld_idx, fld_offset, value_node, span);
                        }
                    }
                } else {
                    // Base is an ident local
                    const base_tag = self.tree.nodeTag(base_idx);
                    if (base_tag == .ident) {
                        const base_name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
                        if (fb.lookupLocal(base_name)) |local_idx| {
                            const fld_info = self.type_reg.get(field.type_idx);
                            if (fld_info == .optional and !self.isPtrLikeOptional(field.type_idx)) {
                                try self.storeCompoundOptField(fb, local_idx, fld_idx, fld_offset, value_node, rhs_ast, span);
                            } else if (self.type_reg.get(field.type_idx) == .pointer and self.type_reg.get(field.type_idx).pointer.managed and !self.target.isWasm()) {
                                const old_val = try fb.emitFieldLocal(local_idx, fld_idx, fld_offset, field.type_idx, span);
                                _ = try self.emitCopyValue(fb, value_node, field.type_idx, span);
                                _ = try fb.emitStoreLocalField(local_idx, fld_idx, fld_offset, value_node, span);
                                try self.emitDestroyValue(fb, old_val, field.type_idx, span);
                            } else {
                                _ = try fb.emitStoreLocalField(local_idx, fld_idx, fld_offset, value_node, span);
                            }
                        } else if (self.builder.lookupGlobal(base_name)) |g| {
                            const ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.VOID;
                            const global_addr = try fb.emitAddrGlobal(g.idx, base_name, ptr_type, span);
                            _ = try fb.emitStoreFieldValue(global_addr, fld_idx, fld_offset, value_node, span);
                        }
                    } else if (base_tag == .field_access) {
                        // Nested struct field assign: o.inner.val = 42
                        const base_addr = try self.resolveStructFieldAddr(base_idx);
                        if (base_addr != ir.null_node) {
                            _ = try fb.emitStoreFieldValue(base_addr, fld_idx, fld_offset, value_node, span);
                        }
                    }
                }
                break;
            }
        }
    }

    /// Resolve a struct field access expression to its memory address.
    fn resolveStructFieldAddr(self: *Lowerer, node_idx: Index) !ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(node_idx);

        if (tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(node_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local_type = fb.locals.items[local_idx].type_idx;
                const ptr_type = self.type_reg.makePointer(local_type) catch TypeRegistry.VOID;
                return try fb.emitAddrLocal(local_idx, ptr_type, self.getSpan(node_idx));
            }
            if (self.builder.lookupGlobal(name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                return try fb.emitAddrGlobal(g.idx, name, ptr_type, self.getSpan(node_idx));
            }
            return ir.null_node;
        }

        if (tag == .field_access) {
            const data = self.tree.nodeData(node_idx);
            const base_idx = data.node_and_token[0];
            const field_token = data.node_and_token[1];
            const field_name = self.tree.tokenSlice(field_token);

            const base_type_idx = self.inferExprType(base_idx);
            const base_type_info = self.type_reg.get(base_type_idx);

            const struct_type = switch (base_type_info) {
                .struct_type => |st| st,
                .pointer => |ptr| blk: {
                    const elem = self.type_reg.get(ptr.elem);
                    if (elem == .struct_type) break :blk elem.struct_type;
                    return ir.null_node;
                },
                else => return ir.null_node,
            };

            const base_addr = if (base_type_info == .pointer)
                try self.lowerExprNode(base_idx)
            else
                try self.resolveStructFieldAddr(base_idx);

            if (base_addr == ir.null_node) return ir.null_node;

            for (struct_type.fields) |field| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    const field_offset: i64 = @intCast(field.offset);
                    if (field_offset == 0) return base_addr;
                    const field_ptr_type = self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID;
                    return try fb.emitAddrOffset(base_addr, field_offset, field_ptr_type, self.getSpan(node_idx));
                }
            }
            return ir.null_node;
        }

        return ir.null_node;
    }

    /// @safe auto-ref: take address of a struct value to produce *Struct.
    fn autoRefValue(self: *Lowerer, fb: *ir.FuncBuilder, value_ir: ir.NodeIndex, ast_idx: Index, span: Span) !ir.NodeIndex {
        const value_type_idx = fb.nodes.items[value_ir].type_idx;
        const ptr_type = self.type_reg.makePointer(value_type_idx) catch TypeRegistry.I64;
        // Try lvalue path
        const tag = self.tree.nodeTag(ast_idx);
        if (tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(ast_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                return fb.emitAddrLocal(local_idx, ptr_type, span);
            } else if (self.builder.lookupGlobal(name)) |g| {
                return fb.emitAddrGlobal(g.idx, name, ptr_type, span);
            }
        } else if (tag == .field_access) {
            const addr = try self.resolveStructFieldAddr(ast_idx);
            if (addr != ir.null_node) return addr;
        }
        // Fallback: temp copy
        const tmp_local = try fb.addLocalWithSize("__safe_ref", value_type_idx, true, self.type_reg.sizeOf(value_type_idx));
        _ = try fb.emitStoreLocal(tmp_local, value_ir, span);
        return fb.emitAddrLocal(tmp_local, ptr_type, span);
    }

    /// Lower index assignment: `arr[idx] = value`.
    fn lowerIndexAssign(self: *Lowerer, target_idx: Index, value_node: ir.NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        // Index expression: data = [base_node, index_node]
        const data = self.tree.nodeData(target_idx);
        const base_idx = data.node_and_node[0];
        const index_idx = data.node_and_node[1];

        const base_type_idx = self.inferExprType(base_idx);
        const base_type = self.type_reg.get(base_type_idx);
        const elem_type: TypeIndex = switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            .pointer => |p| blk: {
                const pointee = self.type_reg.get(p.elem);
                break :blk if (pointee == .array) pointee.array.elem else return;
            },
            else => return,
        };
        const elem_size = self.type_reg.sizeOf(elem_type);
        const index_node = try self.lowerExprNode(index_idx);
        const base_tag = self.tree.nodeTag(base_idx);

        if (base_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                if (self.type_reg.get(local.type_idx) == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, span);
                    const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, span);
                    _ = try fb.emitStoreIndexValue(ptr_val, index_node, value_node, elem_size, span);
                    return;
                }
                if (local.is_param and self.type_reg.isArray(local.type_idx)) {
                    const ptr_val = try fb.emitLoadLocal(local_idx, local.type_idx, span);
                    _ = try fb.emitStoreIndexValue(ptr_val, index_node, value_node, elem_size, span);
                    return;
                }
                const local_info = self.type_reg.get(local.type_idx);
                if (local_info == .pointer and self.type_reg.get(local_info.pointer.elem) == .array) {
                    const ptr_val = try fb.emitLoadLocal(local_idx, TypeRegistry.I64, span);
                    _ = try fb.emitStoreIndexValue(ptr_val, index_node, value_node, elem_size, span);
                    return;
                }
                _ = try fb.emitStoreIndexLocal(local_idx, index_node, value_node, elem_size, span);
                return;
            }
            if (self.builder.lookupGlobal(name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, name, ptr_type, span);
                _ = try fb.emitStoreIndexValue(global_addr, index_node, value_node, elem_size, span);
                return;
            }
        }

        switch (base_type) {
            .slice => {
                const base_val = try self.lowerExprNode(base_idx);
                const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                const ptr_val = try fb.emitSlicePtr(base_val, ptr_type, span);
                _ = try fb.emitStoreIndexValue(ptr_val, index_node, value_node, elem_size, span);
            },
            .pointer => {
                const base_val = try self.lowerExprNode(base_idx);
                _ = try fb.emitStoreIndexValue(base_val, index_node, value_node, elem_size, span);
            },
            .array => {
                if (base_tag == .field_access) {
                    const array_addr = try self.lowerExprNode(base_idx);
                    _ = try fb.emitStoreIndexValue(array_addr, index_node, value_node, elem_size, span);
                }
            },
            else => {},
        }
    }

    // ========================================================================
    // Group 3: Control flow with optional unwrapping
    // Ported from compiler/frontend/lower.zig lines 3967-4708
    // ========================================================================

    /// Lower if-optional statement: if expr |val| { ... } else { ... }
    fn lowerIfOptional(self: *Lowerer, idx: Index) !bool {
        const fb = self.current_func orelse return false;
        const if_data = self.tree.ifData(idx);
        const span = self.getSpan(idx);

        const opt_type_idx = self.inferExprType(if_data.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const is_compound_opt = opt_info == .optional and !self.isPtrLikeOptional(opt_type_idx);

        // Value capture
        const opt_val = try self.lowerExprNode(if_data.condition);
        if (opt_val == ir.null_node) return false;

        const is_non_null = if (is_compound_opt) blk: {
            const opt_local = try fb.addLocalWithSize("__opt_if", opt_type_idx, false, self.type_reg.sizeOf(opt_type_idx));
            _ = try fb.emitStoreLocal(opt_local, opt_val, span);
            const tag_val = try fb.emitFieldLocal(opt_local, 0, 0, TypeRegistry.I64, span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            break :blk try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);
        } else blk: {
            const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, span));
            break :blk try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, span);
        };

        const then_block = try fb.newBlock("if.opt.then");
        const else_block = if (if_data.else_branch.unwrap() != null) try fb.newBlock("if.opt.else") else null;
        const merge_block = try fb.newBlock("if.opt.end");
        _ = try fb.emitBranch(is_non_null, then_block, else_block orelse merge_block, span);

        fb.beginOverlapGroup();
        fb.nextOverlapArm();
        fb.setBlock(then_block);
        const scope_depth = fb.markScopeEntry();
        const unwrapped = if (is_compound_opt) blk: {
            const opt_local = fb.lookupLocal("__opt_if").?;
            break :blk try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
        } else blk: {
            break :blk try fb.emitUnary(.optional_unwrap, opt_val, elem_type, span);
        };
        // Bind capture variable
        if (if_data.capture_token.unwrap()) |ct| {
            const capture_name = self.tree.tokenSlice(ct);
            const capture_local = try fb.addLocalWithSize(capture_name, elem_type, false, self.type_reg.sizeOf(elem_type));
            _ = try fb.emitStoreLocal(capture_local, unwrapped, span);
        }
        if (!try self.lowerBlockNode(if_data.then_branch)) _ = try fb.emitJump(merge_block, span);
        fb.restoreScope(scope_depth);

        if (else_block) |eb| {
            fb.nextOverlapArm();
            fb.setBlock(eb);
            if (if_data.else_branch.unwrap()) |else_idx| {
                if (!try self.lowerBlockNode(else_idx)) _ = try fb.emitJump(merge_block, span);
            }
        }
        fb.endOverlapGroup();
        fb.setBlock(merge_block);
        return false;
    }

    /// Lower while-optional: while (expr) |val| { ... }
    fn lowerWhileOptional(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const wd = self.tree.whileData(idx);
        const span = self.getSpan(idx);

        const cond_block = try fb.newBlock("while.opt.cond");
        const body_block = try fb.newBlock("while.opt.body");
        const exit_block = try fb.newBlock("while.opt.end");
        const cont_block = if (wd.continue_expr.unwrap() != null) try fb.newBlock("while.opt.cont") else cond_block;

        const opt_type_idx = self.inferExprType(wd.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const is_compound_opt = opt_info == .optional and !self.isPtrLikeOptional(opt_type_idx);

        const opt_local = try fb.addLocalWithSize("__while_opt", opt_type_idx, true, self.type_reg.sizeOf(opt_type_idx));

        _ = try fb.emitJump(cond_block, span);
        fb.setBlock(cond_block);

        const opt_val = try self.lowerExprNode(wd.condition);
        if (opt_val == ir.null_node) return;
        _ = try fb.emitStoreLocal(opt_local, opt_val, span);

        const is_non_null = if (is_compound_opt) blk: {
            const tag_val = try fb.emitFieldLocal(opt_local, 0, 0, TypeRegistry.I64, span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            break :blk try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);
        } else blk: {
            const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, span));
            break :blk try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, span);
        };
        _ = try fb.emitBranch(is_non_null, body_block, exit_block, span);

        const label: ?[]const u8 = if (wd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
        try self.loop_stack.append(self.allocator, .{ .cond_block = cont_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth(), .label = label });
        fb.setBlock(body_block);
        const scope_depth2 = fb.markScopeEntry();
        const unwrapped2 = if (is_compound_opt) blk: {
            break :blk try fb.emitFieldLocal(opt_local, 1, 8, elem_type, span);
        } else blk: {
            const opt_reload = try fb.emitLoadLocal(opt_local, opt_type_idx, span);
            break :blk try fb.emitUnary(.optional_unwrap, opt_reload, elem_type, span);
        };
        if (wd.capture_token.unwrap()) |ct| {
            const capture_name = self.tree.tokenSlice(ct);
            const capture_local = try fb.addLocalWithSize(capture_name, elem_type, false, self.type_reg.sizeOf(elem_type));
            _ = try fb.emitStoreLocal(capture_local, unwrapped2, span);
        }
        if (!try self.lowerBlockNode(wd.body)) _ = try fb.emitJump(cont_block, span);
        fb.restoreScope(scope_depth2);

        if (wd.continue_expr.unwrap()) |cont_expr| {
            fb.setBlock(cont_block);
            _ = try self.lowerExprNode(cont_expr);
            _ = try fb.emitJump(cond_block, span);
        }

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Lower for-range: for i in start..end { body }
    fn lowerForRange(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const fd = self.tree.forData(idx);
        const span = self.getSpan(idx);

        const start_val = try self.lowerExprNode(fd.range_start.unwrap().?);
        const end_val = try self.lowerExprNode(fd.range_end.unwrap().?);

        const idx_local = try fb.addLocalWithSize(fd.binding, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(idx_local, start_val, span);

        const end_name = try self.tempName("__for_end");
        const end_local = try fb.addLocalWithSize(end_name, TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(end_local, end_val, span);

        const cond_block = try fb.newBlock("for.cond");
        const body_block = try fb.newBlock("for.body");
        const incr_block = try fb.newBlock("for.incr");
        const exit_block = try fb.newBlock("for.end");

        _ = try fb.emitJump(cond_block, span);
        fb.setBlock(cond_block);
        const idx_val = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
        const end_val_cond = try fb.emitLoadLocal(end_local, TypeRegistry.I64, span);
        const cond = try fb.emitBinary(.lt, idx_val, end_val_cond, TypeRegistry.BOOL, span);
        _ = try fb.emitBranch(cond, body_block, exit_block, span);

        const label: ?[]const u8 = if (fd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
        try self.loop_stack.append(self.allocator, .{
            .cond_block = incr_block,
            .exit_block = exit_block,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
            .label = label,
        });

        fb.setBlock(body_block);
        if (!try self.lowerBlockNode(fd.body)) _ = try fb.emitJump(incr_block, span);

        fb.setBlock(incr_block);
        const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, span);
        const idx_after = try fb.emitBinary(.add, idx_before, one, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(idx_local, idx_after, span);
        _ = try fb.emitJump(cond_block, span);

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Lower `for k, v in map { }` — iterates occupied slots in hash table.
    fn lowerForMap(self: *Lowerer, idx: Index, map_info: types_mod.MapType) !void {
        const fb = self.current_func orelse return;
        const fd = self.tree.forData(idx);
        const span = self.getSpan(idx);

        const key_type = map_info.key;
        const val_type = map_info.value;
        const key_size: u32 = self.type_reg.sizeOf(key_type);
        const val_size: u32 = self.type_reg.sizeOf(val_type);

        // Get the map local
        const iterable_idx = fd.iterable.unwrap() orelse return;
        const iter_tag = self.tree.nodeTag(iterable_idx);
        if (iter_tag != .ident) return;
        const iter_name = self.tree.tokenSlice(self.tree.nodeMainToken(iterable_idx));
        const map_local = fb.lookupLocal(iter_name) orelse return;

        // Loop index
        const idx_name = try self.tempName("__map_idx");
        const idx_local = try fb.addLocalWithSize(idx_name, TypeRegistry.I64, true, 8);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(idx_local, zero, span);

        // Load buf pointer from Map struct
        const buf_val = try fb.emitFieldLocal(map_local, 0, 0, TypeRegistry.I64, span);

        // Capacity local
        const cap_name = try self.tempName("__map_cap");
        const cap_local = try fb.addLocalWithSize(cap_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(cap_local, zero, span);
        const buf_is_null = try fb.emitBinary(.eq, buf_val, zero, TypeRegistry.BOOL, span);
        const buf_ok_block = try fb.newBlock("map.buf_ok");
        const buf_loaded_block = try fb.newBlock("map.buf_loaded");
        _ = try fb.emitBranch(buf_is_null, buf_loaded_block, buf_ok_block, span);

        fb.setBlock(buf_ok_block);
        const eight = try fb.emitConstInt(8, TypeRegistry.I64, span);
        const cap_addr = try fb.emitBinary(.add, buf_val, eight, TypeRegistry.I64, span);
        const cap_field = try fb.emitPtrLoadValue(cap_addr, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(cap_local, cap_field, span);

        const states_name = try self.tempName("__map_states");
        const states_local = try fb.addLocalWithSize(states_name, TypeRegistry.I64, true, 8);
        const keys_name = try self.tempName("__map_keys");
        const keys_local = try fb.addLocalWithSize(keys_name, TypeRegistry.I64, true, 8);
        const vals_name = try self.tempName("__map_vals");
        const vals_local = try fb.addLocalWithSize(vals_name, TypeRegistry.I64, true, 8);

        const sixteen = try fb.emitConstInt(16, TypeRegistry.I64, span);
        const states_base = try fb.emitBinary(.add, buf_val, sixteen, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(states_local, states_base, span);

        const cap_for_keys = try fb.emitLoadLocal(cap_local, TypeRegistry.I64, span);
        const states_size = try fb.emitBinary(.mul, cap_for_keys, eight, TypeRegistry.I64, span);
        const keys_base = try fb.emitBinary(.add, states_base, states_size, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(keys_local, keys_base, span);

        const key_size_val = try fb.emitConstInt(@intCast(key_size), TypeRegistry.I64, span);
        const keys_total = try fb.emitBinary(.mul, cap_for_keys, key_size_val, TypeRegistry.I64, span);
        const vals_base = try fb.emitBinary(.add, keys_base, keys_total, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(vals_local, vals_base, span);

        _ = try fb.emitJump(buf_loaded_block, span);
        fb.setBlock(buf_loaded_block);

        const cond_block = try fb.newBlock("map.cond");
        const body_block = try fb.newBlock("map.body");
        const incr_block = try fb.newBlock("map.incr");
        const exit_block = try fb.newBlock("map.end");

        _ = try fb.emitJump(cond_block, span);

        fb.setBlock(cond_block);
        const idx_cond = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
        const cap_cond = try fb.emitLoadLocal(cap_local, TypeRegistry.I64, span);
        const cond = try fb.emitBinary(.lt, idx_cond, cap_cond, TypeRegistry.BOOL, span);
        _ = try fb.emitBranch(cond, body_block, exit_block, span);

        const label: ?[]const u8 = if (fd.label_token.unwrap()) |lt| self.tree.tokenSlice(lt) else null;
        try self.loop_stack.append(self.allocator, .{
            .cond_block = incr_block,
            .exit_block = exit_block,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
            .label = label,
        });

        fb.setBlock(body_block);
        const cur_idx = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);

        const states_base2 = try fb.emitLoadLocal(states_local, TypeRegistry.I64, span);
        const state_val = try fb.emitIndexValue(states_base2, cur_idx, 8, TypeRegistry.I64, span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, span);
        const is_occupied = try fb.emitBinary(.eq, state_val, one, TypeRegistry.BOOL, span);

        const occupied_block = try fb.newBlock("map.occupied");
        _ = try fb.emitBranch(is_occupied, occupied_block, incr_block, span);

        fb.setBlock(occupied_block);
        const idx_load = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);

        const keys_base2 = try fb.emitLoadLocal(keys_local, TypeRegistry.I64, span);
        const key_val = try fb.emitIndexValue(keys_base2, idx_load, @intCast(key_size), key_type, span);
        const vals_base2 = try fb.emitLoadLocal(vals_local, TypeRegistry.I64, span);
        const val_val = try fb.emitIndexValue(vals_base2, idx_load, @intCast(val_size), val_type, span);

        // Bind value (binding = value)
        const val_local = try fb.addLocalWithSize(fd.binding, val_type, false, val_size);
        _ = try fb.emitStoreLocal(val_local, val_val, span);

        // Bind key (index_binding = key)
        if (fd.index_binding_token.unwrap()) |ibt| {
            const key_binding = self.tree.tokenSlice(ibt);
            const key_local = try fb.addLocalWithSize(key_binding, key_type, false, key_size);
            _ = try fb.emitStoreLocal(key_local, key_val, span);
        }

        if (!try self.lowerBlockNode(fd.body)) _ = try fb.emitJump(incr_block, span);

        fb.setBlock(incr_block);
        const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, span);
        const inc = try fb.emitConstInt(1, TypeRegistry.I64, span);
        const idx_after = try fb.emitBinary(.add, idx_before, inc, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(idx_local, idx_after, span);
        _ = try fb.emitJump(cond_block, span);

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Labeled block expression: label: { ... break :label value ... }
    fn lowerLabeledBlockExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(idx);

        // Extract block data — labeled blocks use block_expr tag
        const data = self.tree.nodeData(idx);
        const stmt_range = data.extra_range;
        const stmt_indices = self.tree.extraNodes(stmt_range);

        // Get label from the main token
        const label_token = self.tree.nodeMainToken(idx);
        const label = self.tree.tokenSlice(label_token);

        const result_type: TypeIndex = TypeRegistry.I64;
        const result_size = self.type_reg.sizeOf(result_type);

        const result_local = try fb.addLocalWithSize("__block_result", result_type, false, result_size);
        const exit_block = try fb.newBlock("block.exit");

        try self.labeled_block_stack.append(self.allocator, .{
            .label = label,
            .exit_block = exit_block,
            .result_local = result_local,
            .result_type = result_type,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
        });

        const scope_depth3 = fb.markScopeEntry();
        for (stmt_indices) |stmt_idx| {
            if (try self.lowerStmt(stmt_idx)) break;
        }

        if (fb.needsTerminator()) _ = try fb.emitJump(exit_block, span);

        fb.restoreScope(scope_depth3);
        _ = self.labeled_block_stack.pop();

        fb.setBlock(exit_block);
        return try fb.emitLoadLocal(result_local, result_type, span);
    }

    // ========================================================================
    // Group 5: Variable init helpers (destructure)
    // Ported from compiler/frontend/lower.zig lines 2643-2716
    // ========================================================================

    /// Lower destructuring statement: let (a, b) = tuple_expr
    fn lowerDestructureStmt(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const data = self.tree.nodeData(idx);
        const ds = self.tree.extraData(data.node_and_extra[1], ast_mod.DestructureData);
        const span = self.getSpan(idx);

        // value is the RHS expression
        const tuple_type_idx = self.inferExprType(ds.value);
        const tuple_info = self.type_reg.get(tuple_type_idx);
        if (tuple_info != .tuple) return;
        const tup = tuple_info.tuple;

        // Parse bindings from extra data (pairs of name_token, type_expr)
        const binding_extra = self.tree.extraSlice(ds.bindings);
        const is_const = ds.flags.is_const;

        // Lower RHS to a temp tuple local, then extract each element
        const tuple_size = self.type_reg.sizeOf(tuple_type_idx);
        const temp_idx = try fb.addLocalWithSize("__destruct_tmp", tuple_type_idx, true, tuple_size);
        const rhs_val = try self.lowerExprNode(ds.value);
        if (rhs_val == ir.null_node) return;
        _ = try fb.emitStoreLocal(temp_idx, rhs_val, span);

        // Each binding is a pair (name_token, type_opt) in extra_data
        var bi: usize = 0;
        var elem_i: usize = 0;
        while (bi < binding_extra.len and elem_i < tup.element_types.len) : ({
            bi += 2;
            elem_i += 1;
        }) {
            const name_token: ast_mod.TokenIndex = @enumFromInt(binding_extra[bi]);
            // binding_extra[bi+1] is the optional type expr (unused here)
            const binding_name = self.tree.tokenSlice(name_token);
            const elem_type = tup.element_types[elem_i];
            const elem_size = self.type_reg.sizeOf(elem_type);
            const local_idx = try fb.addLocalWithSize(binding_name, elem_type, !is_const, elem_size);
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(elem_i)));

            if (elem_type == TypeRegistry.STRING) {
                const ptr_val = try fb.emitFieldLocal(temp_idx, @intCast(elem_i * 2), offset, TypeRegistry.I64, span);
                const len_val = try fb.emitFieldLocal(temp_idx, @intCast(elem_i * 2 + 1), offset + 8, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
                _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
            } else {
                const elem_val = try fb.emitFieldLocal(temp_idx, @intCast(elem_i), offset, elem_type, span);
                _ = try fb.emitStoreLocal(local_idx, elem_val, span);
            }
        }
    }

    // ========================================================================
    // Group 1: Switch lowering
    // Ported from compiler/frontend/lower.zig lines 8368-9127
    // ========================================================================

    /// Check if any switch case has a capture.
    fn hasCapture(self: *Lowerer, sw: ast_mod.full.SwitchFull) bool {
        const case_extra_indices = self.tree.extraSlice(sw.cases);
        for (case_extra_indices) |case_extra_raw| {
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_raw);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            if (case_data.capture_token.unwrap() != null) return true;
        }
        return false;
    }

    /// Check if any switch arm body contains a function call (side effects).
    fn switchArmsHaveCalls(self: *Lowerer, sw: ast_mod.full.SwitchFull) bool {
        const case_extra_indices = self.tree.extraSlice(sw.cases);
        for (case_extra_indices) |case_extra_raw| {
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_raw);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            if (self.exprMayHaveSideEffects(case_data.body)) return true;
        }
        if (sw.else_body.unwrap()) |eb| {
            if (self.exprMayHaveSideEffects(eb)) return true;
        }
        return false;
    }

    /// Conservative check: does this expression potentially have side effects?
    fn exprMayHaveSideEffects(self: *Lowerer, idx: Index) bool {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .call, .call_one, .builtin_call, .new_expr => true,
            .literal_unreachable => true,
            .literal_int, .literal_float, .literal_true, .literal_false,
            .literal_null, .literal_string, .literal_char,
            .ident, .field_access,
            => false,
            // Conservative: unknown expression types assumed side-effectful
            else => true,
        };
    }

    /// Resolve an error literal pattern to its globally unique variant index, or lower normally.
    fn resolveErrorPatternOrLower(self: *Lowerer, pattern_idx: Index, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const ptag = self.tree.nodeTag(pattern_idx);
        if (ptag == .error_literal) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(pattern_idx));
            const error_idx = self.getGlobalErrorIndex(name);
            return try fb.emitConstInt(error_idx, TypeRegistry.I64, span);
        }
        return try self.lowerExprNode(pattern_idx);
    }

    /// Get globally unique index for an error variant name.
    fn getGlobalErrorIndex(self: *Lowerer, name: []const u8) i64 {
        if (self.global_error_table.get(name)) |idx2| return idx2;
        const idx2 = self.next_error_idx;
        self.next_error_idx += 1;
        self.global_error_table.put(name, idx2) catch {};
        return idx2;
    }

    /// Resolve a switch case pattern expression to a union variant index.
    fn resolveUnionVariantIndex(self: *Lowerer, pattern_idx: Index, ut: types_mod.UnionType) ?usize {
        const ptag = self.tree.nodeTag(pattern_idx);
        if (ptag != .field_access) return null;
        const d = self.tree.nodeData(pattern_idx);
        const field_token = d.node_and_token[1];
        const field_name = self.tree.tokenSlice(field_token);
        for (ut.variants, 0..) |v, i| {
            if (std.mem.eql(u8, v.name, field_name)) return i;
        }
        return null;
    }

    /// Resolve the payload type from a pattern's first variant.
    fn resolveUnionPayloadType(self: *Lowerer, patterns: []const u32, ut: types_mod.UnionType) TypeIndex {
        if (patterns.len == 0) return TypeRegistry.VOID;
        const pat_idx: Index = @enumFromInt(patterns[0]);
        if (self.resolveUnionVariantIndex(pat_idx, ut)) |vidx| {
            return ut.variants[vidx].payload_type;
        }
        return TypeRegistry.VOID;
    }

    /// Lower switch statement (non-expression form, no result value).
    fn lowerSwitchStatement(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const sw = self.tree.switchData(idx);
        const span = self.getSpan(idx);

        var subject_type = self.inferExprType(sw.subject);
        var subject_info = self.type_reg.get(subject_type);
        // @safe auto-deref
        if (self.chk.safe_mode and subject_info == .pointer and self.type_reg.get(subject_info.pointer.elem) == .union_type) {
            subject_type = subject_info.pointer.elem;
            subject_info = self.type_reg.get(subject_type);
        }
        const is_union = subject_info == .union_type;
        const old_switch_enum = self.current_switch_enum_type;
        if (subject_info == .enum_type) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = old_switch_enum;

        if (is_union) return try self.lowerUnionSwitch(sw, subject_type, subject_info.union_type, TypeRegistry.VOID, span);

        const subject = try self.lowerExprNode(sw.subject);
        const merge_block = try fb.newBlock("switch.end");

        // String switch: pre-extract ptr/len
        const is_string_switch = subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, span);
            subject_len = try fb.emitSliceLen(subject, span);
        }

        fb.beginOverlapGroup();

        const case_extra_indices = self.tree.extraSlice(sw.cases);
        var i: usize = 0;
        while (i < case_extra_indices.len) : (i += 1) {
            fb.nextOverlapArm();
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_indices[i]);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            const patterns = self.tree.extraSlice(case_data.patterns);
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case_data.flags.is_range and patterns.len >= 2) {
                const range_start = try self.lowerExprNode(@enumFromInt(patterns[0]));
                const range_end = try self.lowerExprNode(@enumFromInt(patterns[1]));
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, span);
            } else {
                for (patterns) |pat_raw| {
                    const pat_idx2: Index = @enumFromInt(pat_raw);
                    const pattern_val = try self.resolveErrorPatternOrLower(pat_idx2, span);
                    const pattern_cond = if (is_string_switch) blk: {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, span);
                        const p_len = try fb.emitSliceLen(pattern_val, span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, span);
                        const zero2 = try fb.emitConstInt(0, TypeRegistry.I64, span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero2, TypeRegistry.BOOL, span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, span);
                }
            }

            // Guard
            if (case_data.guard.unwrap()) |guard_idx| {
                if (case_cond != ir.null_node) {
                    const guard_cond = try self.lowerExprNode(guard_idx);
                    case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, span);
                }
            }

            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < case_extra_indices.len) try fb.newBlock("switch.next") else if (sw.else_body.unwrap() != null) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, span);
            fb.setBlock(case_block);
            if (!try self.lowerBlockNode(case_data.body)) _ = try fb.emitJump(merge_block, span);
            fb.setBlock(next_block);
        }

        if (sw.else_body.unwrap()) |eb| {
            fb.nextOverlapArm();
            if (!try self.lowerBlockNode(eb)) _ = try fb.emitJump(merge_block, span);
        }
        fb.endOverlapGroup();
        fb.setBlock(merge_block);
        return ir.null_node;
    }

    /// Lower union switch: dispatch on tag, extract payload via capture.
    fn lowerUnionSwitch(self: *Lowerer, sw: ast_mod.full.SwitchFull, subject_type: TypeIndex, ut: types_mod.UnionType, result_type: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const is_expr = result_type != TypeRegistry.VOID;
        const result_info = self.type_reg.get(result_type);
        const is_compound_opt = result_info == .optional and !self.isPtrLikeOptional(result_type);

        var result_local: ir.LocalIdx = 0;
        if (is_expr) {
            const result_size = self.type_reg.sizeOf(result_type);
            result_local = try fb.addLocalWithSize("__switch_result", result_type, true, result_size);
        }

        // Store subject into a local so we can extract tag/payload
        const subject_local = blk: {
            const subj_tag = self.tree.nodeTag(sw.subject);
            if (subj_tag == .ident) {
                const subj_name = self.tree.tokenSlice(self.tree.nodeMainToken(sw.subject));
                if (fb.lookupLocal(subj_name)) |li| break :blk li;
            }
            break :blk null;
        } orelse blk: {
            const size = self.type_reg.sizeOf(subject_type);
            const tmp_local = try fb.addLocalWithSize("__switch_subj", subject_type, false, size);
            const subj_val = try self.lowerExprNode(sw.subject);
            _ = try fb.emitStoreLocal(tmp_local, subj_val, span);
            break :blk tmp_local;
        };

        // @safe auto-deref
        var effective_local = subject_local;
        if (self.chk.safe_mode) {
            const local_type = fb.locals.items[subject_local].type_idx;
            const lt_info = self.type_reg.get(local_type);
            if (lt_info == .pointer and self.type_reg.get(lt_info.pointer.elem) == .union_type) {
                const union_type_idx = lt_info.pointer.elem;
                const size2 = self.type_reg.sizeOf(union_type_idx);
                const deref_local = try fb.addLocalWithSize("__union_deref", union_type_idx, false, size2);
                const ptr_val = try fb.emitLoadLocal(subject_local, local_type, span);
                const val = try fb.emitPtrLoadValue(ptr_val, union_type_idx, span);
                _ = try fb.emitStoreLocal(deref_local, val, span);
                effective_local = deref_local;
            }
        }

        const tag_val = try fb.emitFieldLocal(effective_local, 0, 0, TypeRegistry.I64, span);
        const merge_block = try fb.newBlock("switch.end");
        fb.beginOverlapGroup();

        const case_extra_indices = self.tree.extraSlice(sw.cases);
        var i: usize = 0;
        while (i < case_extra_indices.len) : (i += 1) {
            fb.nextOverlapArm();
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_indices[i]);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            const patterns = self.tree.extraSlice(case_data.patterns);
            var case_cond: ir.NodeIndex = ir.null_node;

            for (patterns) |pat_raw| {
                const pat_idx2: Index = @enumFromInt(pat_raw);
                const variant_idx = self.resolveUnionVariantIndex(pat_idx2, ut);
                if (variant_idx) |vidx| {
                    const idx_val = try fb.emitConstInt(@intCast(vidx), TypeRegistry.I64, span);
                    const pattern_cond = try fb.emitBinary(.eq, tag_val, idx_val, TypeRegistry.BOOL, span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, span);
                }
            }

            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < case_extra_indices.len) try fb.newBlock("switch.next") else if (sw.else_body.unwrap() != null) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, span);
            fb.setBlock(case_block);

            // Capture handling
            if (case_data.capture_token.unwrap()) |ct| {
                const scope_depth4 = fb.markScopeEntry();
                const payload_type = self.resolveUnionPayloadType(patterns, ut);
                const payload_size = self.type_reg.sizeOf(payload_type);
                const capture_name = self.tree.tokenSlice(ct);

                if (case_data.flags.capture_is_ptr) {
                    const ptr_type = self.type_reg.makePointer(payload_type) catch TypeRegistry.VOID;
                    const subj_ptr_type = self.type_reg.makePointer(subject_type) catch TypeRegistry.VOID;
                    const subj_addr = try fb.emitAddrLocal(effective_local, subj_ptr_type, span);
                    const payload_addr = try fb.emitAddrOffset(subj_addr, 8, ptr_type, span);
                    const capture_local = try fb.addLocalWithSize(capture_name, ptr_type, false, 8);
                    _ = try fb.emitStoreLocal(capture_local, payload_addr, span);
                } else {
                    const capture_local = try fb.addLocalWithSize(capture_name, payload_type, false, payload_size);
                    const num_chunks = (payload_size + 7) / 8;
                    for (0..num_chunks) |ci| {
                        const union_offset: i64 = 8 + @as(i64, @intCast(ci * 8));
                        const cap_offset: i64 = @intCast(ci * 8);
                        const chunk_val = try fb.emitFieldLocal(effective_local, @intCast(1 + ci), union_offset, TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(capture_local, @intCast(ci), cap_offset, chunk_val, span);
                    }
                }

                if (is_expr) {
                    const case_val = try self.lowerExprNode(case_data.body);
                    const arm_type = self.inferExprType(case_data.body);
                    if (case_val != ir.null_node and arm_type != TypeRegistry.VOID) {
                        if (is_compound_opt) {
                            try self.storeCompoundOptArm(fb, result_local, case_val, case_data.body, span);
                        } else {
                            _ = try fb.emitStoreLocal(result_local, case_val, span);
                        }
                    }
                    if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
                } else {
                    if (!try self.lowerBlockNode(case_data.body)) _ = try fb.emitJump(merge_block, span);
                }
                fb.restoreScope(scope_depth4);
            } else {
                if (is_expr) {
                    const case_val = try self.lowerExprNode(case_data.body);
                    const arm_type = self.inferExprType(case_data.body);
                    if (case_val != ir.null_node and arm_type != TypeRegistry.VOID) {
                        if (is_compound_opt) {
                            try self.storeCompoundOptArm(fb, result_local, case_val, case_data.body, span);
                        } else {
                            _ = try fb.emitStoreLocal(result_local, case_val, span);
                        }
                    }
                    if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
                } else {
                    if (!try self.lowerBlockNode(case_data.body)) _ = try fb.emitJump(merge_block, span);
                }
            }
            fb.setBlock(next_block);
        }

        if (sw.else_body.unwrap()) |eb| {
            fb.nextOverlapArm();
            if (is_expr) {
                const else_val = try self.lowerExprNode(eb);
                const else_type = self.inferExprType(eb);
                if (else_val != ir.null_node and else_type != TypeRegistry.VOID) {
                    if (is_compound_opt) {
                        try self.storeCompoundOptArm(fb, result_local, else_val, eb, span);
                    } else {
                        _ = try fb.emitStoreLocal(result_local, else_val, span);
                    }
                }
                if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
            } else {
                if (!try self.lowerBlockNode(eb)) _ = try fb.emitJump(merge_block, span);
            }
        }
        fb.endOverlapGroup();
        fb.setBlock(merge_block);
        if (is_expr) return try fb.emitLoadLocal(result_local, result_type, span);
        return ir.null_node;
    }

    /// Block-based switch expression lowering: each arm in its own block, result stored in local.
    fn lowerSwitchValueBlocks(self: *Lowerer, sw: ast_mod.full.SwitchFull, result_type: TypeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(sw.subject);
        const old_switch_enum = self.current_switch_enum_type;
        const subject_type = self.inferExprType(sw.subject);
        const subject_info = self.type_reg.get(subject_type);
        if (subject_info == .enum_type) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = old_switch_enum;

        const subject = try self.lowerExprNode(sw.subject);
        const merge_block = try fb.newBlock("switch.end");
        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__switch_result", result_type, true, result_size);
        const result_type_info = self.type_reg.get(result_type);
        const is_compound_opt = result_type_info == .optional and !self.isPtrLikeOptional(result_type);

        const is_string_switch = subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, span);
            subject_len = try fb.emitSliceLen(subject, span);
        }

        fb.beginOverlapGroup();
        const case_extra_indices = self.tree.extraSlice(sw.cases);
        var i: usize = 0;
        while (i < case_extra_indices.len) : (i += 1) {
            fb.nextOverlapArm();
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_indices[i]);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            const patterns = self.tree.extraSlice(case_data.patterns);
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case_data.flags.is_range and patterns.len >= 2) {
                const range_start = try self.lowerExprNode(@enumFromInt(patterns[0]));
                const range_end = try self.lowerExprNode(@enumFromInt(patterns[1]));
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, span);
            } else {
                for (patterns) |pat_raw| {
                    const pat_idx2: Index = @enumFromInt(pat_raw);
                    const pattern_val = try self.resolveErrorPatternOrLower(pat_idx2, span);
                    const pattern_cond = if (is_string_switch) blk: {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, span);
                        const p_len = try fb.emitSliceLen(pattern_val, span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, span);
                        const zero2 = try fb.emitConstInt(0, TypeRegistry.I64, span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero2, TypeRegistry.BOOL, span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, span);
                }
            }

            if (case_data.guard.unwrap()) |guard_idx| {
                if (case_cond != ir.null_node) {
                    const guard_cond = try self.lowerExprNode(guard_idx);
                    case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, span);
                }
            }

            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < case_extra_indices.len) try fb.newBlock("switch.next") else if (sw.else_body.unwrap() != null) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, span);
            fb.setBlock(case_block);
            const case_val = try self.lowerExprNode(case_data.body);
            if (case_val != ir.null_node and fb.nodes.items[case_val].type_idx != TypeRegistry.VOID) {
                if (is_compound_opt) {
                    try self.storeCompoundOptArm(fb, result_local, case_val, case_data.body, span);
                } else {
                    _ = try fb.emitStoreLocal(result_local, case_val, span);
                }
            }
            if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
            fb.setBlock(next_block);
        }

        if (sw.else_body.unwrap()) |eb| {
            fb.nextOverlapArm();
            const else_val = try self.lowerExprNode(eb);
            if (else_val != ir.null_node and fb.nodes.items[else_val].type_idx != TypeRegistry.VOID) {
                if (is_compound_opt) {
                    try self.storeCompoundOptArm(fb, result_local, else_val, eb, span);
                } else {
                    _ = try fb.emitStoreLocal(result_local, else_val, span);
                }
            }
            if (fb.needsTerminator()) _ = try fb.emitJump(merge_block, span);
        }
        fb.endOverlapGroup();
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, span);
    }

    /// Switch as select: pure expressions (no side effects), nest selects bottom-up.
    fn lowerSwitchAsSelect(self: *Lowerer, sw: ast_mod.full.SwitchFull, result_type: TypeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(sw.subject);
        const old_switch_enum = self.current_switch_enum_type;
        const sel_subject_type = self.inferExprType(sw.subject);
        if (self.type_reg.get(sel_subject_type) == .enum_type) self.current_switch_enum_type = sel_subject_type;
        defer self.current_switch_enum_type = old_switch_enum;
        const subject = try self.lowerExprNode(sw.subject);

        const is_string_switch = sel_subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, span);
            subject_len = try fb.emitSliceLen(subject, span);
        }

        var result = if (sw.else_body.unwrap()) |eb| try self.lowerExprNode(eb) else try fb.emitConstNull(result_type, span);
        if (result == ir.null_node) result = try fb.emitConstInt(0, result_type, span);

        const case_extra_indices = self.tree.extraSlice(sw.cases);
        var i: usize = case_extra_indices.len;
        while (i > 0) {
            i -= 1;
            const case_extra: ast_mod.ExtraIndex = @enumFromInt(case_extra_indices[i]);
            const case_data = self.tree.extraData(case_extra, ast_mod.SwitchCaseData);
            const patterns = self.tree.extraSlice(case_data.patterns);
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case_data.flags.is_range and patterns.len >= 2) {
                const range_start = try self.lowerExprNode(@enumFromInt(patterns[0]));
                const range_end = try self.lowerExprNode(@enumFromInt(patterns[1]));
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, span);
            } else {
                for (patterns) |pat_raw| {
                    const pat_idx2: Index = @enumFromInt(pat_raw);
                    const pattern_val = try self.resolveErrorPatternOrLower(pat_idx2, span);
                    const pattern_cond = if (is_string_switch) blk: {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, span);
                        const p_len = try fb.emitSliceLen(pattern_val, span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, span);
                        const zero2 = try fb.emitConstInt(0, TypeRegistry.I64, span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero2, TypeRegistry.BOOL, span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, span);
                }
            }

            if (case_data.guard.unwrap()) |guard_idx| {
                if (case_cond != ir.null_node) {
                    const guard_cond = try self.lowerExprNode(guard_idx);
                    case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, span);
                }
            }

            const case_val = try self.lowerExprNode(case_data.body);
            if (case_val == ir.null_node) continue;
            result = try fb.emitSelect(case_cond, case_val, result, result_type, span);
        }
        return result;
    }

    // ========================================================================
    // Group 2: Generic/VWT lowering
    // Ported from compiler/frontend/lower.zig lines 9804-10520
    // ========================================================================

    fn ensureGenericFnQueued(self: *Lowerer, inst_info_name: []const u8) !void {
        if (self.lowered_generics.contains(inst_info_name)) return;
        try self.lowered_generics.put(inst_info_name, {});
    }

    /// Pre-declare all @inlinable generic function names so hasFunc() returns true during lowering.
    fn predeclareInlinableGenerics(self: *Lowerer) !void {
        // Iterate generic instantiations, declare those marked @inlinable
        var it = self.chk.generics.generic_inst_by_name.valueIterator();
        while (it.next()) |inst_info| {
            if (!self.lowered_generics.contains(inst_info.concrete_name)) continue;
            if (self.builder.hasFunc(inst_info.concrete_name)) continue;
            const saved = self.tree;
            self.tree = inst_info.tree;
            defer self.tree = saved;
            const fn_node_tag = self.tree.nodeTag(inst_info.generic_node);
            _ = fn_node_tag;
            // Check @inlinable annotation from the fn_decl
            // For compact AST we check the fn decl flags
            try self.builder.declareFunc(inst_info.concrete_name);
        }
    }

    /// Process all queued generic instantiations as top-level functions.
    fn lowerQueuedGenericFunctions(self: *Lowerer) !void {
        var pending = std.ArrayListUnmanaged(checker_mod.GenericInstInfo){};
        defer pending.deinit(self.allocator);
        var made_progress = true;
        while (made_progress) {
            made_progress = false;
            pending.clearRetainingCapacity();
            var it = self.chk.generics.generic_inst_by_name.valueIterator();
            while (it.next()) |inst_info| {
                if (!self.lowered_generics.contains(inst_info.concrete_name)) continue;
                const bn = self.computeGenericBaseName(inst_info.concrete_name) catch continue;
                if (self.builder.hasFunc(bn)) continue;
                try pending.append(self.allocator, inst_info.*);
            }
            for (pending.items) |inst_info| {
                const bn = self.computeGenericBaseName(inst_info.concrete_name) catch continue;
                if (self.builder.hasFunc(bn)) continue;
                try self.lowerGenericFnInstance(inst_info, false);
                made_progress = true;
            }
        }
    }

    /// Lower one generic function instance (shared body or monomorphized).
    fn lowerGenericFnInstance(self: *Lowerer, inst_info: checker_mod.GenericInstInfo, is_inlinable: bool) !void {
        const func_name = if (is_inlinable)
            inst_info.concrete_name
        else
            try self.computeGenericBaseName(inst_info.concrete_name);

        if (self.builder.hasFunc(func_name)) return;

        // Swap to defining file's AST
        const saved_tree = self.tree;
        const saved_chk_tree = self.chk.tree;
        const saved_safe_mode = self.chk.safe_mode;
        self.tree = inst_info.tree;
        self.chk.tree = inst_info.tree;
        if (inst_info.tree.safe_mode) self.chk.safe_mode = true;
        defer {
            self.tree = saved_tree;
            self.chk.tree = saved_chk_tree;
            self.chk.safe_mode = saved_safe_mode;
        }

        // For the compact AST, we use the generic_node to get the fn decl
        const fn_tag = self.tree.nodeTag(inst_info.generic_node);
        if (fn_tag != .fn_decl) return;

        // Build type substitution map
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        for (inst_info.type_param_names, 0..) |param_name, i| {
            if (i < inst_info.type_args.len) {
                try sub_map.put(param_name, inst_info.type_args[i]);
            }
        }
        self.type_substitution = sub_map;
        defer {
            self.type_substitution = null;
            self.is_inlinable_body = false;
        }

        // Use preserved expr_types from checker
        const saved_expr_types = self.chk.expr_types;
        if (inst_info.expr_types) |et| {
            self.chk.expr_types = et;
        }
        defer {
            self.chk.expr_types = saved_expr_types;
        }

        // Resolve return type and start function
        const fd = self.tree.fnDeclData(inst_info.generic_node);
        const return_type = if (fd.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        const span = self.getSpan(inst_info.generic_node);

        self.builder.startFunc(func_name, TypeRegistry.VOID, wasm_return_type, span);
        if (self.builder.func()) |fb| {
            if (uses_sret) fb.sret_return_type = return_type;
            self.current_func = fb;
            self.cleanup_stack.clear();

            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }

            if (is_inlinable) {
                self.is_inlinable_body = true;
                // Concrete params
                const param_slice = self.tree.extraSlice(fd.params);
                for (param_slice) |param_raw| {
                    const param_idx: Index = @enumFromInt(param_raw);
                    const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                    var param_type = self.resolveParamType(param_idx);
                    param_type = self.chk.safeWrapType(param_type) catch param_type;
                    _ = try fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
                }
            } else {
                // Shared body: params + metadata
                const param_slice = self.tree.extraSlice(fd.params);
                for (param_slice) |param_raw| {
                    const param_idx: Index = @enumFromInt(param_raw);
                    const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                    var param_type = self.resolveParamType(param_idx);
                    param_type = self.chk.safeWrapType(param_type) catch param_type;
                    _ = try fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
                }
                // Metadata params
                for (inst_info.type_param_names) |pn| {
                    const meta_name = try std.fmt.allocPrint(self.allocator, "__metadata_{s}", .{pn});
                    _ = try fb.addParam(meta_name, TypeRegistry.I64, 8);
                }
            }

            if (fd.body.unwrap()) |body_idx| {
                _ = try self.lowerBlockNode(body_idx);
                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    /// Monomorphized generic function lowering.
    fn lowerGenericFnMonomorphized(self: *Lowerer, inst_info: checker_mod.GenericInstInfo) !void {
        return self.lowerGenericFnInstance(inst_info, true);
    }

    /// Check if a return type refers to a generic type parameter.
    fn isGenericReturnType(self: *Lowerer, return_type_node: Index, type_param_names: []const []const u8) bool {
        const tag = self.tree.nodeTag(return_type_node);
        const name = switch (tag) {
            .ident => self.tree.tokenSlice(self.tree.nodeMainToken(return_type_node)),
            .type_named => self.tree.tokenSlice(self.tree.nodeMainToken(return_type_node)),
            else => return false,
        };
        for (type_param_names) |pn| {
            if (std.mem.eql(u8, name, pn)) return true;
        }
        return false;
    }

    /// Check if a parameter is a type parameter (should be passed indirectly).
    fn isGenericParamIndirect(self: *Lowerer, inst_info: checker_mod.GenericInstInfo, param_idx_val: usize) bool {
        _ = self;
        // Check if param type matches a type parameter name
        _ = inst_info;
        _ = param_idx_val;
        return false; // Simplified — full implementation requires AST type node inspection
    }

    /// Append metadata arguments for a generic call.
    fn appendMetadataArgs(self: *Lowerer, fb: *ir.FuncBuilder, inst_info: checker_mod.GenericInstInfo, args: *std.ArrayListUnmanaged(ir.NodeIndex), span: Span) !void {
        if (self.type_substitution != null and !self.is_inlinable_body) {
            // Forward metadata from enclosing scope
            for (inst_info.type_args, 0..) |_, i| {
                const param_name: ?[]const u8 = if (i < inst_info.type_param_names.len) inst_info.type_param_names[i] else null;
                if (param_name) |pn| {
                    const meta_local_name = try std.fmt.allocPrint(self.allocator, "__metadata_{s}", .{pn});
                    if (fb.lookupLocal(meta_local_name)) |meta_local| {
                        const meta_val = try fb.emitLoadLocal(meta_local, TypeRegistry.I64, span);
                        try args.append(self.allocator, meta_val);
                        continue;
                    }
                }
                const meta = try self.emitTypeMetadataRef(fb, inst_info.type_args[i], span);
                try args.append(self.allocator, meta);
            }
        } else {
            for (inst_info.type_args) |type_arg| {
                const meta = try self.emitTypeMetadataRef(fb, type_arg, span);
                try args.append(self.allocator, meta);
            }
        }
    }

    /// Load @sizeOf(T) from metadata parameter at runtime.
    fn emitLoadSizeFromMetadata(self: *Lowerer, fb: *ir.FuncBuilder, type_arg_node: Index, sub: std.StringHashMap(TypeIndex), span: Span) !?ir.NodeIndex {
        const tag = self.tree.nodeTag(type_arg_node);
        const param_name = switch (tag) {
            .ident => self.tree.tokenSlice(self.tree.nodeMainToken(type_arg_node)),
            .type_named => self.tree.tokenSlice(self.tree.nodeMainToken(type_arg_node)),
            else => return null,
        };
        if (!sub.contains(param_name)) return null;

        const meta_local_name = try std.fmt.allocPrint(self.allocator, "__metadata_{s}", .{param_name});
        const meta_local_idx = fb.lookupLocal(meta_local_name) orelse return null;
        const meta_val = try fb.emitLoadLocal(meta_local_idx, TypeRegistry.I64, span);
        const size_offset = try fb.emitConstInt(8, TypeRegistry.I64, span);
        const size_addr = try fb.emitBinary(.add, meta_val, size_offset, TypeRegistry.I64, span);
        return try fb.emitPtrLoadValue(size_addr, TypeRegistry.I64, span);
    }

    /// Load runtime size of T from __metadata_{type_param_name} local.
    fn emitRuntimeSizeOf(self: *Lowerer, fb: *ir.FuncBuilder, type_param_name: []const u8, span: Span) !ir.NodeIndex {
        const meta_local_name = try std.fmt.allocPrint(self.allocator, "__metadata_{s}", .{type_param_name});
        const meta_local_idx = fb.lookupLocal(meta_local_name) orelse {
            return try fb.emitConstInt(8, TypeRegistry.I64, span);
        };
        const meta_val = try fb.emitLoadLocal(meta_local_idx, TypeRegistry.I64, span);
        const size_offset = try fb.emitConstInt(8, TypeRegistry.I64, span);
        const size_addr = try fb.emitBinary(.add, meta_val, size_offset, TypeRegistry.I64, span);
        return try fb.emitPtrLoadValue(size_addr, TypeRegistry.I64, span);
    }

    /// Emit a reference to the __type_metadata_{Type} global for a concrete type.
    fn emitTypeMetadataRef(self: *Lowerer, fb: *ir.FuncBuilder, type_idx: TypeIndex, span: Span) !ir.NodeIndex {
        const type_name = self.type_reg.typeName(type_idx);
        const meta_global = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{type_name});
        if (self.builder.lookupGlobal(meta_global)) |gi| {
            return try fb.emitAddrGlobal(gi.idx, meta_global, TypeRegistry.I64, span);
        }
        return try fb.emitConstInt(0, TypeRegistry.I64, span);
    }

    /// Phase 8.8: Runtime hash dispatch in shared generic bodies.
    fn emitRuntimeHashDispatch(self: *Lowerer, tp_name: []const u8, base_idx: Index, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const meta_local_name = try std.fmt.allocPrint(self.allocator, "__metadata_{s}", .{tp_name});
        const meta_local_idx = fb.lookupLocal(meta_local_name) orelse return ir.null_node;
        const meta_val = try fb.emitLoadLocal(meta_local_idx, TypeRegistry.I64, span);
        const hash_offset = try fb.emitConstInt(32, TypeRegistry.I64, span);
        const hash_fn_addr = try fb.emitBinary(.add, meta_val, hash_offset, TypeRegistry.I64, span);
        const hash_fn_ptr = try fb.emitPtrLoadValue(hash_fn_addr, TypeRegistry.I64, span);

        const base = try self.lowerExprNode(base_idx);
        const receiver_addr = blk: {
            const base_tag = self.tree.nodeTag(base_idx);
            if (base_tag == .ident) {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
                if (fb.lookupLocal(name)) |local_idx| {
                    break :blk try fb.emitAddrLocal(local_idx, TypeRegistry.I64, span);
                }
            }
            const tmp = try fb.addLocalWithSize("__hash_recv", TypeRegistry.I64, false, 24);
            _ = try fb.emitStoreLocal(tmp, base, span);
            break :blk try fb.emitAddrLocal(tmp, TypeRegistry.I64, span);
        };

        var hash_args = [_]ir.NodeIndex{receiver_addr};
        return try fb.emitCallIndirect(hash_fn_ptr, &hash_args, TypeRegistry.I64, span);
    }

    /// Emit a VWT wrapper under the concrete name that calls the base function.
    fn emitVWTWrapper(self: *Lowerer, inst_info: checker_mod.GenericInstInfo, base_name: []const u8) !void {
        if (self.builder.hasFunc(inst_info.concrete_name)) return;

        const saved_tree = self.tree;
        const saved_chk_tree = self.chk.tree;
        self.tree = inst_info.tree;
        self.chk.tree = inst_info.tree;
        defer {
            self.tree = saved_tree;
            self.chk.tree = saved_chk_tree;
        }

        const fn_tag = self.tree.nodeTag(inst_info.generic_node);
        if (fn_tag != .fn_decl) return;
        const fd = self.tree.fnDeclData(inst_info.generic_node);
        const return_type = if (fd.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        const span = self.getSpan(inst_info.generic_node);

        self.builder.startFunc(inst_info.concrete_name, TypeRegistry.VOID, wasm_return_type, span);
        if (self.builder.func()) |fb| {
            if (uses_sret) fb.sret_return_type = return_type;
            self.current_func = fb;
            self.cleanup_stack.clear();

            var call_args = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer call_args.deinit(self.allocator);

            if (uses_sret) {
                const sret_param = try fb.addParam("__sret", TypeRegistry.I64, 8);
                const sret_val = try fb.emitLoadLocal(sret_param, TypeRegistry.I64, span);
                try call_args.append(self.allocator, sret_val);
            }

            // Metadata args for each type param
            for (inst_info.type_args) |type_arg| {
                const meta = try self.emitTypeMetadataRef(fb, type_arg, span);
                try call_args.append(self.allocator, meta);
            }

            // Regular params
            const param_slice = self.tree.extraSlice(fd.params);
            for (param_slice) |param_raw| {
                const param_idx: Index = @enumFromInt(param_raw);
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const p = try fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
                const v = try fb.emitLoadLocal(p, param_type, span);
                try call_args.append(self.allocator, v);
            }

            const result = try fb.emitCall(base_name, call_args.items, false, wasm_return_type, span);
            if (wasm_return_type == TypeRegistry.VOID) {
                _ = try fb.emitRet(null, span);
            } else {
                _ = try fb.emitRet(result, span);
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    // ========================================================================
    // Test / Bench Runners
    // ========================================================================

    pub fn generateTestRunner(self: *Lowerer) !void {
        if (self.test_names.items.len == 0) return;
        const span = Span.init(Pos.zero, Pos.zero);
        self.builder.startFunc("main", TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;

            // Initialize globals and type metadata before any test code runs
            {
                var init_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__cot_init_globals", &init_args, false, TypeRegistry.VOID, span);
                _ = try fb.emitCall("__cot_init_metadata", &init_args, false, TypeRegistry.VOID, span);
            }

            const fail_count = try fb.addLocalWithSize("__fail_count", TypeRegistry.I64, true, 8);
            const zero_init = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocal(fail_count, zero_init, span);

            const pass_count = try fb.addLocalWithSize("__pass_count", TypeRegistry.I64, true, 8);
            _ = try fb.emitStoreLocal(pass_count, zero_init, span);

            for (self.test_names.items, self.test_display_names.items) |test_name, display_name| {
                const name_copy = try self.allocator.dupe(u8, display_name);
                const str_idx = try fb.addStringLiteral(name_copy);
                const str_slice = try fb.emitConstSlice(str_idx, span);
                const name_ptr = try fb.emitSlicePtr(str_slice, ptr_type, span);
                const name_len = try fb.emitSliceLen(str_slice, span);
                var print_args = [_]ir.NodeIndex{ name_ptr, name_len };
                _ = try fb.emitCall("__test_print_name", &print_args, false, TypeRegistry.VOID, span);

                var no_args = [_]ir.NodeIndex{};
                const tag_val = try fb.emitCall(test_name, &no_args, false, TypeRegistry.I64, span);

                const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                const is_error = try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);

                const fail_block = try fb.newBlock("test.fail");
                const pass_block = try fb.newBlock("test.pass");
                const merge_block = try fb.newBlock("test.merge");
                _ = try fb.emitBranch(is_error, fail_block, pass_block, span);

                fb.setBlock(fail_block);
                var fail_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__test_fail", &fail_args, false, TypeRegistry.VOID, span);
                const cur_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                const one_f = try fb.emitConstInt(1, TypeRegistry.I64, span);
                const new_fail = try fb.emitBinary(.add, cur_fail, one_f, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocal(fail_count, new_fail, span);

                if (self.fail_fast) {
                    const ff_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
                    const ff_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                    var ff_summary_args = [_]ir.NodeIndex{ ff_pass, ff_fail };
                    _ = try fb.emitCall("__test_summary", &ff_summary_args, false, TypeRegistry.VOID, span);
                    const ff_exit = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                    _ = try fb.emitRet(ff_exit, span);
                } else {
                    _ = try fb.emitJump(merge_block, span);
                }

                fb.setBlock(pass_block);
                var pass_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__test_pass", &pass_args, false, TypeRegistry.VOID, span);
                const cur_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
                const one_p = try fb.emitConstInt(1, TypeRegistry.I64, span);
                const new_pass = try fb.emitBinary(.add, cur_pass, one_p, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocal(pass_count, new_pass, span);
                _ = try fb.emitJump(merge_block, span);

                fb.setBlock(merge_block);
            }

            const final_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
            const final_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
            var summary_args = [_]ir.NodeIndex{ final_pass, final_fail };
            _ = try fb.emitCall("__test_summary", &summary_args, false, TypeRegistry.VOID, span);

            const final_count = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
            _ = try fb.emitRet(final_count, span);
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    pub fn generateBenchRunner(self: *Lowerer) !void {
        if (self.bench_names.items.len == 0) return;
        const span = Span.init(Pos.zero, Pos.zero);
        self.builder.startFunc("main", TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;

            {
                var init_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__cot_init_globals", &init_args, false, TypeRegistry.VOID, span);
            }

            const discard_local = try fb.addLocalWithSize("__bench_discard", TypeRegistry.I64, true, 8);
            const loop_counter = try fb.addLocalWithSize("__bench_i", TypeRegistry.I64, true, 8);

            for (self.bench_names.items, 0..) |bench_name, bench_idx| {
                const display_name = self.bench_display_names.items[bench_idx];

                {
                    const name_copy = try self.allocator.dupe(u8, display_name);
                    const str_idx = try fb.addStringLiteral(name_copy);
                    const str_slice = try fb.emitConstSlice(str_idx, span);
                    const name_ptr = try fb.emitSlicePtr(str_slice, ptr_type, span);
                    const name_len = try fb.emitSliceLen(str_slice, span);
                    var pn_args = [_]ir.NodeIndex{ name_ptr, name_len };
                    _ = try fb.emitCall("__bench_print_name", &pn_args, false, TypeRegistry.VOID, span);
                }

                // Warmup: 3 calls
                for (0..3) |_| {
                    var no_args = [_]ir.NodeIndex{};
                    const r = try fb.emitCall(bench_name, &no_args, false, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(discard_local, r, span);
                }

                // Calibrate
                {
                    var cs_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_calibrate_start", &cs_args, false, TypeRegistry.VOID, span);
                }
                {
                    var no_args = [_]ir.NodeIndex{};
                    const r = try fb.emitCall(bench_name, &no_args, false, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(discard_local, r, span);
                }
                {
                    var ce_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_calibrate_end", &ce_args, false, TypeRegistry.VOID, span);
                }

                // Measure
                {
                    var ms_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_measure_start", &ms_args, false, TypeRegistry.VOID, span);
                }
                {
                    const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(loop_counter, zero, span);
                }

                const loop_cond_block = try fb.newBlock("bench.loop.cond");
                const loop_body_block = try fb.newBlock("bench.loop.body");
                const loop_exit_block = try fb.newBlock("bench.loop.exit");
                _ = try fb.emitJump(loop_cond_block, span);

                fb.setBlock(loop_cond_block);
                {
                    var gn_args = [_]ir.NodeIndex{};
                    const n_val = try fb.emitCall("__bench_get_n", &gn_args, false, TypeRegistry.I64, span);
                    const i_val = try fb.emitLoadLocal(loop_counter, TypeRegistry.I64, span);
                    const cond = try fb.emitBinary(.lt, i_val, n_val, TypeRegistry.BOOL, span);
                    _ = try fb.emitBranch(cond, loop_body_block, loop_exit_block, span);
                }

                fb.setBlock(loop_body_block);
                {
                    var no_args = [_]ir.NodeIndex{};
                    const r = try fb.emitCall(bench_name, &no_args, false, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(discard_local, r, span);

                    const i_reload = try fb.emitLoadLocal(loop_counter, TypeRegistry.I64, span);
                    const one = try fb.emitConstInt(1, TypeRegistry.I64, span);
                    const i_next = try fb.emitBinary(.add, i_reload, one, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(loop_counter, i_next, span);
                    _ = try fb.emitJump(loop_cond_block, span);
                }

                fb.setBlock(loop_exit_block);
                {
                    var me_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_measure_end", &me_args, false, TypeRegistry.VOID, span);
                }
            }

            {
                const count = try fb.emitConstInt(@intCast(self.bench_names.items.len), TypeRegistry.I64, span);
                var sum_args = [_]ir.NodeIndex{count};
                _ = try fb.emitCall("__bench_summary", &sum_args, false, TypeRegistry.VOID, span);
            }

            const ret_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitRet(ret_zero, span);
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    // ========================================================================
    // ARC / Cleanup Helpers
    // ========================================================================

    fn maybeRegisterScopeDestroy(self: *Lowerer, local_idx: ir.LocalIdx, type_idx: TypeIndex) !void {
        const type_info = self.type_reg.get(type_idx);
        if (type_info != .struct_type) return;
        const struct_name = type_info.struct_type.name;

        if (self.chk.lookupMethod(struct_name, "deinit") == null and !self.target.isWasm()) {
            const struct_type = type_info.struct_type;
            var has_arc_fields = false;
            for (struct_type.fields) |field| {
                if (self.type_reg.couldBeARC(field.type_idx)) {
                    has_arc_fields = true;
                    break;
                }
            }
            if (has_arc_fields) {
                var already_queued = false;
                for (self.pending_auto_deinits.items) |name| {
                    if (std.mem.eql(u8, name, struct_name)) {
                        already_queued = true;
                        break;
                    }
                }
                if (!already_queued) {
                    try self.pending_auto_deinits.append(self.allocator, struct_name);
                }
            }
        }

        if (self.chk.lookupMethod(struct_name, "deinit")) |method_info| {
            if (self.chk.generics.generic_inst_by_name.get(method_info.func_name)) |inst_info| {
                try self.ensureGenericFnQueued(inst_info);
            }
            const qualified_deinit = if (self.chk.generics.generic_inst_by_name.contains(method_info.func_name))
                method_info.func_name
            else
                try self.resolveMethodName(struct_name, method_info.func_name, method_info.source_tree);
            const cleanup = Cleanup.initScopeDestroy(local_idx, type_idx, qualified_deinit);
            _ = try self.cleanup_stack.push(cleanup);
        } else {
            for (self.pending_auto_deinits.items) |auto_name| {
                if (std.mem.eql(u8, auto_name, struct_name)) {
                    const qualified_type = try self.qualifyTypeName(struct_name);
                    const deinit_name = try std.fmt.allocPrint(self.allocator, "{s}_deinit", .{qualified_type});
                    const cleanup = Cleanup.initScopeDestroy(local_idx, type_idx, deinit_name);
                    _ = try self.cleanup_stack.push(cleanup);
                    break;
                }
            }
        }
    }

    fn emitFieldReleases(self: *Lowerer, fb: *ir.FuncBuilder, deinit_name: []const u8, span: Span) !void {
        const suffix = "_deinit";
        if (!std.mem.endsWith(u8, deinit_name, suffix)) return;
        const type_name = deinit_name[0 .. deinit_name.len - suffix.len];
        const struct_type_idx = self.type_reg.lookupByName(type_name) orelse return;
        const type_info = self.type_reg.get(struct_type_idx);
        if (type_info != .struct_type) return;
        const struct_type = type_info.struct_type;

        const self_local = fb.lookupLocal("self") orelse return;
        const self_type_idx = fb.locals.items[self_local].type_idx;
        for (struct_type.fields, 0..) |field, fi| {
            if (!self.type_reg.isTrivial(field.type_idx)) {
                const self_ptr = try fb.emitLoadLocal(self_local, self_type_idx, span);
                const field_offset: i64 = @intCast(field.offset);
                const field_val = try fb.emitFieldValue(self_ptr, @intCast(fi), field_offset, field.type_idx, span);
                try self.emitDestroyValue(fb, field_val, field.type_idx, span);
            }
        }
    }

    pub fn emitPendingAutoDeinits(self: *Lowerer) !void {
        for (self.pending_auto_deinits.items) |type_name| {
            const struct_type_idx = self.type_reg.lookupByName(type_name) orelse continue;
            const type_info = self.type_reg.get(struct_type_idx);
            if (type_info != .struct_type) continue;
            const struct_type = type_info.struct_type;
            const ptr_type = try self.type_reg.makePointer(struct_type_idx);

            const qualified_type = try self.qualifyTypeName(type_name);
            const deinit_name = try std.fmt.allocPrint(self.allocator, "{s}_deinit", .{qualified_type});
            if (self.builder.hasFunc(deinit_name)) continue;
            self.builder.startFunc(deinit_name, TypeRegistry.VOID, TypeRegistry.VOID, Span.zero);
            if (self.builder.func()) |fb| {
                self.current_func = fb;
                fb.is_destructor = true;
                _ = try fb.addParam("self", ptr_type, 8);

                const self_local: ir.LocalIdx = 0;
                for (struct_type.fields, 0..) |field, fi| {
                    if (!self.type_reg.isTrivial(field.type_idx)) {
                        const self_ptr = try fb.emitLoadLocal(self_local, ptr_type, Span.zero);
                        const field_offset: i64 = @intCast(field.offset);
                        const field_val = try fb.emitFieldValue(self_ptr, @intCast(fi), field_offset, field.type_idx, Span.zero);
                        try self.emitDestroyValue(fb, field_val, field.type_idx, Span.zero);
                    }
                }

                _ = try fb.emitRet(null, Span.zero);
                self.current_func = null;
            }
            try self.builder.endFunc();
        }
    }

    fn baseHasCleanup(self: *Lowerer, base_idx: Index) bool {
        const tag = self.tree.nodeTag(base_idx);
        if (tag == .ident) {
            const fb = self.current_func orelse return false;
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                return self.cleanup_stack.hasCleanupForLocal(local_idx);
            }
        }
        if (tag == .field_access) {
            const data = self.tree.nodeData(base_idx);
            return self.baseHasCleanup(data.node_and_token[0]);
        }
        if (tag == .index) {
            const data = self.tree.nodeData(base_idx);
            return self.baseHasCleanup(data.node_and_node[0]);
        }
        if (tag == .unary_deref) {
            const data = self.tree.nodeData(base_idx);
            return self.baseHasCleanup(data.node);
        }
        return false;
    }

    // ========================================================================
    // Existential Types
    // ========================================================================

    fn lowerExistentialMethodCall(self: *Lowerer, cd: ast_mod.full.CallFull, base_idx: Index, field_name: []const u8, exist: types_mod.ExistentialType) !ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(base_idx);

        var method_idx: ?usize = null;
        for (exist.method_names, 0..) |mn, i| {
            if (std.mem.eql(u8, mn, field_name)) { method_idx = i; break; }
        }
        if (method_idx == null) return ir.null_node;

        const exist_addr = try self.lowerExprNode(base_idx);
        if (exist_addr == ir.null_node) return ir.null_node;

        const pwt_offset = try fb.emitConstInt(32, TypeRegistry.I64, span);
        const pwt_addr = try fb.emitBinary(.add, exist_addr, pwt_offset, TypeRegistry.I64, span);
        const pwt_ptr = try fb.emitPtrLoadValue(pwt_addr, TypeRegistry.I64, span);

        const fn_offset = try fb.emitConstInt(@intCast(method_idx.? * 8), TypeRegistry.I64, span);
        const fn_addr = try fb.emitBinary(.add, pwt_ptr, fn_offset, TypeRegistry.I64, span);
        const fn_ptr = try fb.emitPtrLoadValue(fn_addr, TypeRegistry.I64, span);

        const value_ptr = exist_addr;

        var args = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer args.deinit(self.allocator);
        try args.append(self.allocator, value_ptr);
        const call_args = self.collectCallArgsList(cd) catch return ir.null_node;
        defer call_args.deinit(self.allocator);
        for (call_args.items) |arg_idx| {
            const arg_val = try self.lowerExprNode(arg_idx);
            try args.append(self.allocator, arg_val);
        }

        var return_type = TypeRegistry.VOID;
        if (exist.conforming_types.len > 0) {
            const first_type = exist.conforming_types[0];
            if (self.chk.lookupMethod(first_type, field_name)) |mi| {
                const ft = self.type_reg.get(mi.func_type);
                if (ft == .func) return_type = ft.func.return_type;
            }
        }

        return try fb.emitCallIndirect(fn_ptr, args.items, return_type, span);
    }

    fn emitExistentialErasure(self: *Lowerer, fb: *ir.FuncBuilder, value: ir.NodeIndex, concrete_type: TypeIndex, existential_type: TypeIndex, span: Span) !ir.NodeIndex {
        const exist_info = self.type_reg.get(existential_type);
        if (exist_info != .existential) return value;
        if (self.type_reg.get(concrete_type) == .existential) return value;
        const concrete_name = self.type_reg.typeName(concrete_type);

        const container = try fb.addLocalWithSize("__exist_container", existential_type, false, 40);
        const container_addr = try fb.emitAddrLocal(container, TypeRegistry.I64, span);

        const val_size = self.type_reg.sizeOf(concrete_type);
        const val_tmp = try fb.addLocalWithSize("__exist_val", concrete_type, false, val_size);
        _ = try fb.emitStoreLocal(val_tmp, value, span);
        const val_addr = try fb.emitAddrLocal(val_tmp, TypeRegistry.I64, span);
        const size_const = try fb.emitConstInt(@intCast(if (val_size > 24) 8 else val_size), TypeRegistry.I64, span);
        var memcpy_args = [_]ir.NodeIndex{ container_addr, val_addr, size_const };
        _ = try fb.emitCall("memcpy", &memcpy_args, false, TypeRegistry.VOID, span);

        const meta_global = try std.fmt.allocPrint(self.allocator, "__type_metadata_{s}", .{concrete_name});
        const metadata = if (self.builder.lookupGlobal(meta_global)) |gi|
            try fb.emitAddrGlobal(gi.idx, meta_global, TypeRegistry.I64, span)
        else
            try fb.emitConstInt(0, TypeRegistry.I64, span);
        const meta_offset = try fb.emitConstInt(24, TypeRegistry.I64, span);
        const meta_addr = try fb.emitBinary(.add, container_addr, meta_offset, TypeRegistry.I64, span);
        _ = try fb.emitPtrStoreValue(meta_addr, metadata, span);

        const method_count = exist_info.existential.method_count;
        const pwt_size_val: u32 = method_count * 8;
        if (pwt_size_val > 0) {
            const pwt_buf_size = try fb.emitConstInt(@intCast(pwt_size_val), TypeRegistry.I64, span);
            const zero_tag = try fb.emitConstInt(0, TypeRegistry.I64, span);
            var alloc_args = [_]ir.NodeIndex{ zero_tag, pwt_buf_size };
            const pwt_buf = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

            for (exist_info.existential.method_names, 0..) |method_name, i| {
                const method_fn_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ concrete_name, method_name });
                const resolved_fn = if (self.builder.hasFunc(method_fn_name))
                    method_fn_name
                else blk: {
                    var found: []const u8 = method_fn_name;
                    for (self.builder.funcs.items) |func| {
                        if (std.mem.endsWith(u8, func.name, method_fn_name)) {
                            if (func.name.len > method_fn_name.len and func.name[func.name.len - method_fn_name.len - 1] == '.') {
                                found = func.name;
                                break;
                            }
                        }
                    }
                    break :blk found;
                };
                const fn_addr_val = try fb.emitFuncAddr(resolved_fn, TypeRegistry.I64, span);
                const slot_offset = try fb.emitConstInt(@intCast(i * 8), TypeRegistry.I64, span);
                const slot_addr = try fb.emitBinary(.add, pwt_buf, slot_offset, TypeRegistry.I64, span);
                _ = try fb.emitPtrStoreValue(slot_addr, fn_addr_val, span);
            }

            const pwt_offset = try fb.emitConstInt(32, TypeRegistry.I64, span);
            const pwt_dest = try fb.emitBinary(.add, container_addr, pwt_offset, TypeRegistry.I64, span);
            _ = try fb.emitPtrStoreValue(pwt_dest, pwt_buf, span);
        }

        return try fb.emitLoadLocal(container, existential_type, span);
    }

    // ========================================================================
    // Bounds Checks
    // ========================================================================

    fn emitBoundsCheck(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, span: Span) !void {
        try self.emitBoundsCheckImpl(fb, index_node, length_node, .lt, span);
    }

    fn emitSliceBoundsCheck(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, span: Span) !void {
        try self.emitBoundsCheckImpl(fb, index_node, length_node, .le, span);
    }

    fn emitBoundsCheckImpl(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, cmp_op: ir.BinaryOp, span: Span) !void {
        const in_bounds = try fb.emitBinary(cmp_op, index_node, length_node, TypeRegistry.BOOL, span);
        const ok_block = try fb.newBlock("bounds.ok");
        const fail_block = try fb.newBlock("bounds.fail");
        _ = try fb.emitBranch(in_bounds, ok_block, fail_block, span);
        fb.setBlock(fail_block);

        const fd = try fb.emitConstInt(2, TypeRegistry.I64, span);

        const pos = self.err.src.position(span.start);
        const loc_str = try std.fmt.allocPrint(self.allocator, "{s}:{d}: ", .{ pos.filename, pos.line });
        const loc_idx = try fb.addStringLiteral(loc_str);
        const loc_val = try fb.emitConstSlice(loc_idx, span);
        var loc_args = [_]ir.NodeIndex{ fd, loc_val };
        _ = try fb.emitCall("write", &loc_args, true, TypeRegistry.I64, span);

        const prefix = try fb.addStringLiteral(try self.allocator.dupe(u8, "panic: index out of range ["));
        const prefix_val = try fb.emitConstSlice(prefix, span);
        var prefix_args = [_]ir.NodeIndex{ fd, prefix_val };
        _ = try fb.emitCall("write", &prefix_args, true, TypeRegistry.I64, span);

        var idx_args = [_]ir.NodeIndex{index_node};
        _ = try fb.emitCall("eprint_int", &idx_args, false, TypeRegistry.VOID, span);

        const mid = try fb.addStringLiteral(try self.allocator.dupe(u8, "] with length "));
        const mid_val = try fb.emitConstSlice(mid, span);
        var mid_args = [_]ir.NodeIndex{ fd, mid_val };
        _ = try fb.emitCall("write", &mid_args, true, TypeRegistry.I64, span);

        var len_args = [_]ir.NodeIndex{length_node};
        _ = try fb.emitCall("eprint_int", &len_args, false, TypeRegistry.VOID, span);

        const nl = try fb.addStringLiteral(try self.allocator.dupe(u8, "\n"));
        const nl_val = try fb.emitConstSlice(nl, span);
        var nl_args = [_]ir.NodeIndex{ fd, nl_val };
        _ = try fb.emitCall("write", &nl_args, true, TypeRegistry.I64, span);

        const exit_code = try fb.emitConstInt(2, TypeRegistry.I64, span);
        var exit_args = [_]ir.NodeIndex{exit_code};
        _ = try fb.emitCall("exit", &exit_args, false, TypeRegistry.VOID, span);

        _ = try fb.emitTrap(span);
        fb.setBlock(ok_block);
    }

    // ========================================================================
    // Expression Helpers
    // ========================================================================

    fn isDivOp(op: Token) bool {
        return switch (op) {
            .quo, .rem => true,
            else => false,
        };
    }

    fn resolveConditionAddr(self: *Lowerer, expr_idx: Index, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const tag = self.tree.nodeTag(expr_idx);

        if (tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(expr_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                return try fb.emitAddrLocal(local_idx,
                    self.type_reg.makePointer(type_idx) catch TypeRegistry.VOID, span);
            }
            if (self.builder.lookupGlobal(name)) |g| {
                return try fb.emitAddrGlobal(g.idx, name,
                    self.type_reg.makePointer(type_idx) catch TypeRegistry.VOID, span);
            }
        }

        if (tag == .field_access) {
            const data = self.tree.nodeData(expr_idx);
            const base = data.node_and_token[0];
            const field_tok = data.node_and_token[1];
            const field_name = self.tree.tokenSlice(field_tok);
            const base_type_idx = self.inferExprType(base);
            const base_info = self.type_reg.get(base_type_idx);
            if (base_info == .struct_type) {
                for (base_info.struct_type.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_name)) {
                        const base_addr = try self.resolveConditionAddr(base, base_type_idx, span);
                        if (base_addr == ir.null_node) break;
                        return try fb.emitAddrOffset(base_addr, @intCast(field.offset),
                            self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID, span);
                    }
                }
            }
            if (base_info == .pointer) {
                const elem_info = self.type_reg.get(base_info.pointer.elem);
                if (elem_info == .struct_type) {
                    for (elem_info.struct_type.fields) |field| {
                        if (std.mem.eql(u8, field.name, field_name)) {
                            const ptr_val = try self.lowerExprNode(base);
                            return try fb.emitAddrOffset(ptr_val, @intCast(field.offset),
                                self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID, span);
                        }
                    }
                }
            }
        }

        if (tag == .unary_deref) {
            const data = self.tree.nodeData(expr_idx);
            return try self.lowerExprNode(data.node);
        }

        const val = try self.lowerExprNode(expr_idx);
        const size = self.type_reg.sizeOf(type_idx);
        const tmp = try fb.addLocalWithSize("__ptr_cap_tmp", type_idx, true, size);
        _ = try fb.emitStoreLocal(tmp, val, span);
        return try fb.emitAddrLocal(tmp,
            self.type_reg.makePointer(type_idx) catch TypeRegistry.VOID, span);
    }

    fn evalFloatLiteral(self: *Lowerer, idx: Index) ?f64 {
        const tag = self.tree.nodeTag(idx);
        switch (tag) {
            .literal_float => {
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                return std.fmt.parseFloat(f64, text) catch null;
            },
            .literal_int => {
                const text = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
                const iv = std.fmt.parseInt(i64, text, 10) catch return null;
                return @floatFromInt(iv);
            },
            .paren => {
                const data = self.tree.nodeData(idx);
                return self.evalFloatLiteral(data.node);
            },
            .unary_neg => {
                const data = self.tree.nodeData(idx);
                const v = self.evalFloatLiteral(data.node) orelse return null;
                return -v;
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div => {
                const data = self.tree.nodeData(idx);
                const l = self.evalFloatLiteral(data.node_and_node[0]) orelse return null;
                const r = self.evalFloatLiteral(data.node_and_node[1]) orelse return null;
                return switch (tag) {
                    .binary_add => l + r,
                    .binary_sub => l - r,
                    .binary_mul => l * r,
                    .binary_div => l / r,
                    else => null,
                };
            },
            else => return null,
        }
    }

    fn getStringLiteral(self: *Lowerer, idx: Index) ?[]const u8 {
        const tag = self.tree.nodeTag(idx);
        if (tag != .literal_string) return null;
        const v = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
        if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') return v[1 .. v.len - 1];
        return v;
    }

    fn inferBinaryType(self: *Lowerer, tag: ast_mod.Node.Tag, left: Index, right: Index) TypeIndex {
        return switch (tag) {
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte => TypeRegistry.BOOL,
            else => {
                const left_type = self.inferExprType(left);
                const right_type = self.inferExprType(right);
                if (left_type == TypeRegistry.UNTYPED_INT or left_type == TypeRegistry.VOID) {
                    if (right_type != TypeRegistry.UNTYPED_INT and right_type != TypeRegistry.VOID and
                        right_type != TypeRegistry.UNTYPED_FLOAT)
                    {
                        return right_type;
                    }
                    return TypeRegistry.I64;
                }
                if (left_type == TypeRegistry.UNTYPED_FLOAT) return TypeRegistry.F64;
                return left_type;
            },
        };
    }

    fn storeCatchCompound(self: *Lowerer, local_idx: ir.LocalIdx, val: ir.NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(val, ptr_type, span);
        const len_val = try fb.emitSliceLen(val, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
    }

    // ========================================================================
    // Type-Specific Init Helpers
    // ========================================================================

    fn lowerArrayInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, span: Span) !void {
        const fb = self.current_func orelse return;
        const local = fb.locals.items[local_idx];
        const local_type = self.type_reg.get(local.type_idx);
        const tag = self.tree.nodeTag(value_idx);

        if (tag == .array_literal_empty or tag == .array_literal_one or tag == .array_literal) {
            const elem_type = if (local_type == .array) local_type.array.elem else TypeRegistry.I64;
            const elem_size = self.type_reg.sizeOf(elem_type);
            const elements = self.getArrayLiteralElements(value_idx);
            for (elements, 0..) |elem_idx, i| {
                const elem_node = try self.lowerExprNode(elem_idx);
                const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                _ = try fb.emitStoreIndexLocal(local_idx, idx_node, elem_node, elem_size, span);
            }
        } else if (tag == .literal_undefined) {
            const type_size = self.type_reg.sizeOf(local.type_idx);
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
            const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, span);
            const size_node = try fb.emitConstInt(@intCast(type_size), TypeRegistry.I64, span);
            var args = [_]ir.NodeIndex{ local_addr, size_node };
            _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, span);
        } else {
            if (local_type == .array) {
                const elem_type = local_type.array.elem;
                const elem_size = self.type_reg.sizeOf(elem_type);
                const arr_len = local_type.array.length;
                const src_val = try self.lowerExprNode(value_idx);
                for (0..arr_len) |i| {
                    const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                    const src_elem = try fb.emitIndexValue(src_val, idx_node, elem_size, elem_type, span);
                    _ = try fb.emitStoreIndexLocal(local_idx, idx_node, src_elem, elem_size, span);
                }
            } else {
                const value_node_ir = try self.lowerExprNode(value_idx);
                _ = try fb.emitStoreLocal(local_idx, value_node_ir, span);
            }
        }
    }

    /// Helper to extract array literal element indices from the compact AST.
    fn getArrayLiteralElements(self: *Lowerer, idx: Index) []const Index {
        const tag = self.tree.nodeTag(idx);
        if (tag == .array_literal_empty) return &.{};
        if (tag == .array_literal_one) {
            const data = self.tree.nodeData(idx);
            // array_literal_one stores the single element in data.node
            return @as(*const [1]Index, &data.node);
        }
        // array_literal: extra_range of elements
        const data = self.tree.nodeData(idx);
        return self.tree.extraNodes(data.extra_range);
    }

    fn lowerStructInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, span: Span) !void {
        // In the compact AST, struct init is handled via lowerStructInitExpr
        // which stores into a temp local. For var decl init, we lower then store.
        const value_node_ir = try self.lowerExprNode(value_idx);
        if (value_node_ir == ir.null_node) return;
        const fb = self.current_func orelse return;
        _ = try fb.emitStoreLocal(local_idx, value_node_ir, span);
    }

    fn lowerTupleInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, span: Span) !void {
        const fb = self.current_func orelse return;
        const tag = self.tree.nodeTag(value_idx);
        if (tag != .tuple_literal) return;
        const data = self.tree.nodeData(value_idx);
        const elements = self.tree.extraNodes(data.extra_range);
        const tuple_type_idx = self.inferExprType(value_idx);
        for (elements, 0..) |elem_idx, i| {
            const value_ir = try self.lowerExprNode(elem_idx);
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(i)));
            _ = try fb.emitStoreLocalField(local_idx, @intCast(i), offset, value_ir, span);
        }
    }

    fn lowerUnionInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, type_idx: TypeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const type_info = self.type_reg.get(type_idx);
        const union_type = if (type_info == .union_type) type_info.union_type else return;
        const tag = self.tree.nodeTag(value_idx);

        // Case 1: Payload variant call - Result.Ok(42)
        if (tag == .call_zero or tag == .call_one or tag == .call) {
            const cd = self.tree.callData(value_idx);
            const callee_tag = self.tree.nodeTag(cd.callee);
            if (callee_tag == .field_access) {
                const fa_data = self.tree.nodeData(cd.callee);
                const field_tok = fa_data.node_and_token[1];
                const field_name = self.tree.tokenSlice(field_tok);
                const call_args = try self.collectCallArgsList(cd);
                defer call_args.deinit(self.allocator);
                for (union_type.variants, 0..) |v, i| {
                    if (std.mem.eql(u8, v.name, field_name)) {
                        const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_val, span);
                        if (call_args.items.len > 0) {
                            const payload_val = try self.lowerExprNode(call_args.items[0]);
                            _ = try fb.emitStoreLocalField(local_idx, 1, 8, payload_val, span);
                        }
                        return;
                    }
                }
            }
        }

        // Case 2: Unit variant - State.Running
        if (tag == .field_access) {
            const fa_data = self.tree.nodeData(value_idx);
            const field_tok = fa_data.node_and_token[1];
            const field_name = self.tree.tokenSlice(field_tok);
            for (union_type.variants, 0..) |v, i| {
                if (std.mem.eql(u8, v.name, field_name)) {
                    const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_val, span);
                    return;
                }
            }
        }

        // Fallback
        const val = try self.lowerExprNode(value_idx);
        if (val != ir.null_node) _ = try fb.emitStoreLocal(local_idx, val, span);
    }

    fn lowerStringInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, span: Span) !void {
        const fb = self.current_func orelse return;
        const str_node = try self.lowerExprNode(value_idx);
        if (str_node == ir.null_node) return;
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(str_node, ptr_type, span);
        const len_val = try fb.emitSliceLen(str_node, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
    }

    fn lowerSliceInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: Index, slice_type: TypeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const slice_node = try self.lowerExprNode(value_idx);
        if (slice_node == ir.null_node) return;
        const slice_info = self.type_reg.get(slice_type);
        const elem_type = if (slice_info == .slice) slice_info.slice.elem else TypeRegistry.U8;
        const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(slice_node, ptr_type, span);
        const len_val = try fb.emitSliceLen(slice_node, span);
        const cap_val = try fb.emitSliceCap(slice_node, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 2, 16, cap_val, span);
    }

    fn lowerZeroInitExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const span = self.getSpan(idx);
        const zi_type = self.inferExprType(idx);
        if (zi_type != .invalid and zi_type != TypeRegistry.VOID) {
            const size = self.type_reg.sizeOf(zi_type);
            if (size > 0) {
                const tmp_local = try fb.addLocalWithSize("__zero_tmp", zi_type, true, size);
                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                const local_addr = try fb.emitAddrLocal(tmp_local, ptr_type, span);
                const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, span);
                var args = [_]ir.NodeIndex{ local_addr, size_node };
                _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, span);
                return try fb.emitLoadLocal(tmp_local, zi_type, span);
            }
        }
        return try fb.emitConstInt(0, TypeRegistry.I64, span);
    }

    fn lowerActorInit(self: *Lowerer, idx: Index, struct_type: types_mod.StructType, struct_type_idx: TypeIndex, span: Span) !ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const payload_size = self.type_reg.sizeOf(struct_type_idx);
        const zero_metadata = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const size_val = try fb.emitConstInt(@intCast(payload_size), TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ zero_metadata, size_val };
        const actor_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

        const temp_name = try std.fmt.allocPrint(self.allocator, "__actor_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_local = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(temp_local, actor_ptr, span);

        // Store fields from struct init expression
        const si_tag = self.tree.nodeTag(idx);
        if (si_tag == .struct_init_one) {
            const sio = self.tree.extraData(self.tree.nodeData(idx).node_and_extra[1], ast_mod.StructInitOne);
            const fname = self.tree.tokenSlice(sio.field_name_token);
            const value_node_ir = try self.lowerExprNode(sio.field_value);
            for (struct_type.fields) |struct_field| {
                if (std.mem.eql(u8, struct_field.name, fname)) {
                    const field_offset: i64 = @intCast(struct_field.offset);
                    const ptr = try fb.emitLoadLocal(temp_local, TypeRegistry.I64, span);
                    const offset_const = try fb.emitConstInt(field_offset, TypeRegistry.I64, span);
                    const field_addr = try fb.emitBinary(.add, ptr, offset_const, TypeRegistry.I64, span);
                    _ = try fb.emitPtrStoreValue(field_addr, value_node_ir, span);
                    break;
                }
            }
        }

        return try fb.emitLoadLocal(temp_local, TypeRegistry.I64, span);
    }

    // ========================================================================
    // Array/Slice Concat and String Slice
    // ========================================================================

    fn lowerArrayConcat(self: *Lowerer, fb: *ir.FuncBuilder, left: ir.NodeIndex, right: ir.NodeIndex, la: types_mod.ArrayType, ra: types_mod.ArrayType, span: Span) Error!ir.NodeIndex {
        const elem_type = la.elem;
        const elem_size = self.type_reg.sizeOf(elem_type);
        const result_len = la.length + ra.length;
        const result_type = self.type_reg.makeArray(elem_type, result_len) catch return ir.null_node;
        const result_size = self.type_reg.sizeOf(result_type);
        const temp_name = try std.fmt.allocPrint(self.allocator, "__cat_{d}", .{fb.locals.items.len});
        const local_idx = try fb.addLocalWithSize(temp_name, result_type, false, result_size);
        for (0..la.length) |i| {
            const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
            const src_elem = try fb.emitIndexValue(left, idx_node, elem_size, elem_type, span);
            _ = try fb.emitStoreIndexLocal(local_idx, idx_node, src_elem, elem_size, span);
        }
        for (0..ra.length) |j| {
            const src_idx = try fb.emitConstInt(@intCast(j), TypeRegistry.I64, span);
            const dst_idx = try fb.emitConstInt(@intCast(la.length + j), TypeRegistry.I64, span);
            const src_elem = try fb.emitIndexValue(right, src_idx, elem_size, elem_type, span);
            _ = try fb.emitStoreIndexLocal(local_idx, dst_idx, src_elem, elem_size, span);
        }
        return try fb.emitAddrLocal(local_idx, result_type, span);
    }

    fn lowerSliceConcat(self: *Lowerer, fb: *ir.FuncBuilder, left: ir.NodeIndex, right: ir.NodeIndex, sl: types_mod.SliceType, slice_type: TypeIndex, span: Span) Error!ir.NodeIndex {
        const elem_type = sl.elem;
        const elem_size = self.type_reg.sizeOf(elem_type);
        const ptr_type = self.type_reg.makePointer(elem_type) catch return ir.null_node;
        const ptr1 = try fb.emitSlicePtr(left, ptr_type, span);
        const len1 = try fb.emitSliceLen(left, span);
        const ptr2 = try fb.emitSlicePtr(right, ptr_type, span);
        const len2 = try fb.emitSliceLen(right, span);
        const elem_size_node = try fb.emitConstInt(@intCast(elem_size), TypeRegistry.I64, span);
        const byte_len1 = try fb.emitBinary(.mul, len1, elem_size_node, TypeRegistry.I64, span);
        const byte_len2 = try fb.emitBinary(.mul, len2, elem_size_node, TypeRegistry.I64, span);
        var args = [_]ir.NodeIndex{ ptr1, byte_len1, ptr2, byte_len2 };
        const new_ptr = try fb.emitCall("string_concat", &args, false, TypeRegistry.I64, span);
        const new_len = try fb.emitBinary(.add, len1, len2, TypeRegistry.I64, span);
        return try fb.emit(ir.Node.init(.{ .slice_header = .{ .ptr = new_ptr, .len = new_len, .cap = new_len } }, slice_type, span));
    }

    fn lowerStringSlice(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const data = self.tree.nodeData(idx);
        const span = self.getSpan(idx);
        const slice_extra = self.tree.extraData(data.node_and_extra[1], ast_mod.SliceData);
        const base = data.node_and_extra[0];

        const str_val = try self.lowerExprNode(base);
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
        const str_ptr = try fb.emitSlicePtr(str_val, ptr_type, span);
        const str_len = try fb.emitSliceLen(str_val, span);

        var start_val: ir.NodeIndex = undefined;
        if (slice_extra.start.unwrap()) |s| {
            start_val = try self.lowerExprNode(s);
        } else {
            start_val = try fb.emitConstInt(0, TypeRegistry.I64, span);
        }

        var end_val: ir.NodeIndex = undefined;
        if (slice_extra.end.unwrap()) |e| {
            end_val = try self.lowerExprNode(e);
        } else {
            end_val = str_len;
        }

        if (!self.release_mode) {
            try self.emitSliceBoundsCheck(fb, start_val, str_len, span);
            try self.emitSliceBoundsCheck(fb, end_val, str_len, span);
        }

        const new_ptr = try fb.emitBinary(.add, str_ptr, start_val, TypeRegistry.I64, span);
        const new_len = try fb.emitBinary(.sub, end_val, start_val, TypeRegistry.I64, span);

        const result_local = try fb.addLocalWithSize("__str_slice", TypeRegistry.STRING, false, 16);
        _ = try fb.emitStoreLocalField(result_local, 0, 0, new_ptr, span);
        _ = try fb.emitStoreLocalField(result_local, 1, 8, new_len, span);
        return try fb.emitLoadLocal(result_local, TypeRegistry.STRING, span);
    }

    // ========================================================================
    // Closure / Async Helpers
    // ========================================================================

    fn createFuncValue(self: *Lowerer, func_name: []const u8, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size: u32 = 8;
        const total_size = HEAP_HEADER_SIZE + payload_size;

        const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

        const temp_name = try std.fmt.allocPrint(self.allocator, "__closure_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_idx = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(temp_idx, alloc_result, span);
        const ptr_node = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);

        const table_idx_node = try fb.emitFuncAddr(func_name, type_idx, span);
        _ = try fb.emitPtrStoreValue(ptr_node, table_idx_node, span);

        return try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);
    }

    const CaptureInfo = struct {
        name: []const u8,
        local_idx: ir.LocalIdx,
        type_idx: TypeIndex,
    };

    fn detectCaptures(self: *Lowerer, body_idx: Index, parent_fb: *ir.FuncBuilder, captures: *std.ArrayListUnmanaged(CaptureInfo)) Error!void {
        const tag = self.tree.nodeTag(body_idx);
        switch (tag) {
            .ident => {
                const name = self.tree.tokenSlice(self.tree.nodeMainToken(body_idx));
                if (parent_fb.lookupLocal(name)) |local_idx| {
                    for (captures.items) |cap| {
                        if (std.mem.eql(u8, cap.name, name)) return;
                    }
                    try captures.append(self.allocator, .{
                        .name = name,
                        .local_idx = local_idx,
                        .type_idx = parent_fb.locals.items[local_idx].type_idx,
                    });
                }
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte,
            .binary_and, .binary_or, .binary_bit_and, .binary_bit_or, .binary_bit_xor,
            .binary_shl, .binary_shr, .binary_concat,
            => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node_and_node[0], parent_fb, captures);
                try self.detectCaptures(data.node_and_node[1], parent_fb, captures);
            },
            .unary_neg, .unary_not, .unary_bit_not, .unary_deref, .unary_addr_of,
            .unary_try, .unary_await, .unary_unwrap,
            => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node, parent_fb, captures);
            },
            .paren, .expr_stmt => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node, parent_fb, captures);
            },
            .call_zero, .call_one, .call => {
                const cd = self.tree.callData(body_idx);
                try self.detectCaptures(cd.callee, parent_fb, captures);
                const call_args = self.collectCallArgsList(cd) catch return;
                defer call_args.deinit(self.allocator);
                for (call_args.items) |arg| try self.detectCaptures(arg, parent_fb, captures);
            },
            .field_access => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node_and_token[0], parent_fb, captures);
            },
            .index => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node_and_node[0], parent_fb, captures);
                try self.detectCaptures(data.node_and_node[1], parent_fb, captures);
            },
            .block_one, .block_two, .block => {
                const data = self.tree.nodeData(body_idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                for (stmts) |s| try self.detectCaptures(s, parent_fb, captures);
            },
            .if_simple, .if_full => {
                const if_data = self.tree.ifData(body_idx);
                try self.detectCaptures(if_data.condition, parent_fb, captures);
                try self.detectCaptures(if_data.then_branch, parent_fb, captures);
                if (if_data.else_branch.unwrap()) |eb| try self.detectCaptures(eb, parent_fb, captures);
            },
            .catch_expr, .orelse_expr => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node_and_node[0], parent_fb, captures);
                try self.detectCaptures(data.node_and_node[1], parent_fb, captures);
            },
            .return_stmt => {
                const data = self.tree.nodeData(body_idx);
                if (data.opt_node.unwrap()) |val| try self.detectCaptures(val, parent_fb, captures);
            },
            .var_decl_local => {
                const vd = self.tree.localVarData(body_idx);
                if (vd.value.unwrap()) |val| try self.detectCaptures(val, parent_fb, captures);
            },
            .assign => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node_and_node[0], parent_fb, captures);
                try self.detectCaptures(data.node_and_node[1], parent_fb, captures);
            },
            .@"while" => {
                const wd = self.tree.whileData(body_idx);
                try self.detectCaptures(wd.condition, parent_fb, captures);
                try self.detectCaptures(wd.body, parent_fb, captures);
            },
            .for_range, .for_iter => {
                const fd = self.tree.forData(body_idx);
                if (fd.iterable.unwrap()) |it| try self.detectCaptures(it, parent_fb, captures);
                if (fd.range_start.unwrap()) |rs| try self.detectCaptures(rs, parent_fb, captures);
                if (fd.range_end.unwrap()) |re| try self.detectCaptures(re, parent_fb, captures);
                try self.detectCaptures(fd.body, parent_fb, captures);
            },
            .closure_expr => {
                const ce = self.tree.closureData(body_idx);
                try self.detectCaptures(ce.body, parent_fb, captures);
            },
            .struct_init_one, .struct_init => {
                // detect captures in struct field values
                if (tag == .struct_init_one) {
                    const sio = self.tree.extraData(self.tree.nodeData(body_idx).node_and_extra[1], ast_mod.StructInitOne);
                    try self.detectCaptures(sio.field_value, parent_fb, captures);
                }
            },
            .array_literal_one => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node, parent_fb, captures);
            },
            .array_literal => {
                const data = self.tree.nodeData(body_idx);
                const elems = self.tree.extraNodes(data.extra_range);
                for (elems) |e| try self.detectCaptures(e, parent_fb, captures);
            },
            .defer_stmt => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node, parent_fb, captures);
            },
            .switch_expr => {
                const sw = self.tree.switchData(body_idx);
                try self.detectCaptures(sw.subject, parent_fb, captures);
                // Cases are in extra data — skip for now (complex traversal)
                if (sw.else_body.unwrap()) |eb| try self.detectCaptures(eb, parent_fb, captures);
            },
            .task_expr => {
                const data = self.tree.nodeData(body_idx);
                try self.detectCaptures(data.node, parent_fb, captures);
            },
            else => {},
        }
    }

    fn countAwaits(self: *Lowerer, idx: Index) u32 {
        const tag = self.tree.nodeTag(idx);
        return switch (tag) {
            .unary_await => {
                const data = self.tree.nodeData(idx);
                return 1 + self.countAwaits(data.node);
            },
            .call_zero, .call_one, .call => {
                const cd = self.tree.callData(idx);
                var count: u32 = self.countAwaits(cd.callee);
                const call_args = self.collectCallArgsList(cd) catch return count;
                defer call_args.deinit(self.allocator);
                for (call_args.items) |arg| count += self.countAwaits(arg);
                return count;
            },
            .binary_add, .binary_sub, .binary_mul, .binary_div, .binary_mod,
            .binary_eq, .binary_neq, .binary_lt, .binary_gt, .binary_lte, .binary_gte,
            .binary_and, .binary_or,
            => {
                const data = self.tree.nodeData(idx);
                return self.countAwaits(data.node_and_node[0]) + self.countAwaits(data.node_and_node[1]);
            },
            .unary_neg, .unary_not, .unary_bit_not, .unary_try, .unary_unwrap => {
                const data = self.tree.nodeData(idx);
                return self.countAwaits(data.node);
            },
            .block_one, .block_two, .block => {
                const data = self.tree.nodeData(idx);
                const stmts = self.tree.extraNodes(data.extra_range);
                var count: u32 = 0;
                for (stmts) |s| count += self.countAwaits(s);
                return count;
            },
            .return_stmt => {
                const data = self.tree.nodeData(idx);
                if (data.opt_node.unwrap()) |val| return self.countAwaits(val);
                return 0;
            },
            .var_decl_local => {
                const vd = self.tree.localVarData(idx);
                if (vd.value.unwrap()) |val| return self.countAwaits(val);
                return 0;
            },
            .assign => {
                const data = self.tree.nodeData(idx);
                return self.countAwaits(data.node_and_node[1]);
            },
            .if_simple, .if_full => {
                const id = self.tree.ifData(idx);
                var count: u32 = self.countAwaits(id.condition) + self.countAwaits(id.then_branch);
                if (id.else_branch.unwrap()) |eb| count += self.countAwaits(eb);
                return count;
            },
            .@"while" => {
                const wd = self.tree.whileData(idx);
                return self.countAwaits(wd.condition) + self.countAwaits(wd.body);
            },
            .for_range, .for_iter => {
                const fd = self.tree.forData(idx);
                var count: u32 = self.countAwaits(fd.body);
                if (fd.iterable.unwrap()) |it| count += self.countAwaits(it);
                return count;
            },
            .expr_stmt, .paren => {
                const data = self.tree.nodeData(idx);
                return self.countAwaits(data.node);
            },
            .defer_stmt => {
                const data = self.tree.nodeData(idx);
                return self.countAwaits(data.node);
            },
            else => 0,
        };
    }

    fn lowerAsyncFnEager(self: *Lowerer, fn_decl: ast_mod.full.FnDeclFull, idx: Index) !void {
        const span = self.getSpan(idx);
        const return_type = if (fn_decl.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;
        const link_name = if (fn_decl.flags.is_export or std.mem.eql(u8, fn_decl.name, "main"))
            fn_decl.name
        else
            try self.qualifyName(fn_decl.name);

        self.builder.startFunc(link_name, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            fb.is_export = fn_decl.flags.is_export;
            self.current_func = fb;
            self.cleanup_stack.clear();

            const param_indices = self.tree.extraNodes(fn_decl.params);
            for (param_indices) |param_idx| {
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const param_size = self.type_reg.sizeOf(param_type);
                _ = try fb.addParam(param_name, param_type, param_size);
            }

            const zero_metadata = try fb.emitConstInt(0, TypeRegistry.I64, span);
            const twenty_four = try fb.emitConstInt(24, TypeRegistry.I64, span);
            var alloc_args = [_]ir.NodeIndex{ zero_metadata, twenty_four };
            const task_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

            const task_local = try fb.addLocalWithSize("__async_task", TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(task_local, task_ptr, span);
            fb.async_task_local = task_local;
            fb.async_result_type = return_type;

            if (fn_decl.body.unwrap()) |body| {
                _ = try self.lowerBlockNode(body);
            }

            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerAsyncConstructorPoll(self: *Lowerer, fn_decl: ast_mod.full.FnDeclFull, idx: Index, link_name: []const u8, return_type: TypeIndex) !void {
        const span = self.getSpan(idx);
        const param_indices = self.tree.extraNodes(fn_decl.params);

        // Phase 1: Poll function
        const poll_name = try std.fmt.allocPrint(self.allocator, "{s}__poll", .{link_name});
        try self.builder.declareFunc(poll_name);

        self.builder.startFunc(poll_name, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();

            const frame_param_local = try fb.addParam("__frame", TypeRegistry.I64, 8);
            const frame_local = try fb.addLocalWithSize("__async_task", TypeRegistry.I64, false, 8);
            const frame_val = try fb.emitLoadLocal(frame_param_local, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocal(frame_local, frame_val, span);
            fb.async_task_local = frame_local;
            fb.async_result_type = return_type;
            fb.is_async_poll = true;

            const param_base_offset: i64 = 24;
            for (param_indices, 0..) |param_idx, pi| {
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const param_size = self.type_reg.sizeOf(param_type);

                const frame_ptr = try fb.emitLoadLocal(frame_local, TypeRegistry.I64, span);
                const offset_val = try fb.emitConstInt(param_base_offset + @as(i64, @intCast(pi)) * 8, TypeRegistry.I64, span);
                const param_addr = try fb.emitBinary(.add, frame_ptr, offset_val, TypeRegistry.I64, span);
                const param_val = try fb.emitPtrLoadValue(param_addr, param_type, span);

                const local_idx = try fb.addLocalWithSize(param_name, param_type, false, param_size);
                _ = try fb.emitStoreLocal(local_idx, param_val, span);
            }

            if (fn_decl.body.unwrap()) |body| {
                _ = try self.lowerBlockNode(body);
            }

            self.current_func = null;
        }
        try self.builder.endFunc();

        // Phase 2: Constructor
        self.builder.startFunc(link_name, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            fb.is_export = fn_decl.flags.is_export;
            self.current_func = fb;
            self.cleanup_stack.clear();

            for (param_indices) |param_idx| {
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                var param_type = self.resolveParamType(param_idx);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const param_size = self.type_reg.sizeOf(param_type);
                _ = try fb.addParam(param_name, param_type, param_size);
            }

            const num_params: i64 = @intCast(param_indices.len);
            const frame_size = 24 + num_params * 8 + 128;
            const zero_metadata = try fb.emitConstInt(0, TypeRegistry.I64, span);
            const size_val = try fb.emitConstInt(frame_size, TypeRegistry.I64, span);
            var alloc_args = [_]ir.NodeIndex{ zero_metadata, size_val };
            const frame_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

            const frame_local = try fb.addLocalWithSize("__frame", TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(frame_local, frame_ptr, span);

            const state_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            const eight = try fb.emitConstInt(8, TypeRegistry.I64, span);
            const state_addr = try fb.emitBinary(.add, frame_ptr, eight, TypeRegistry.I64, span);
            _ = try fb.emitPtrStoreValue(state_addr, state_zero, span);

            const param_base_offset: i64 = 24;
            for (param_indices, 0..) |param_idx, pi| {
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                const param_local = fb.local_map.get(param_name) orelse continue;
                const param_val = try fb.emitLoadLocal(param_local, TypeRegistry.I64, span);
                const frame_ptr2 = try fb.emitLoadLocal(frame_local, TypeRegistry.I64, span);
                const offset_val = try fb.emitConstInt(param_base_offset + @as(i64, @intCast(pi)) * 8, TypeRegistry.I64, span);
                const param_addr = try fb.emitBinary(.add, frame_ptr2, offset_val, TypeRegistry.I64, span);
                _ = try fb.emitPtrStoreValue(param_addr, param_val, span);
            }

            const poll_fn_type = try self.type_reg.add(.{ .func = .{
                .params = &.{},
                .return_type = TypeRegistry.I64,
            } });
            const poll_fn_idx = try fb.emitFuncAddr(poll_name, poll_fn_type, span);
            const frame_for_store = try fb.emitLoadLocal(frame_local, TypeRegistry.I64, span);
            const sixteen = try fb.emitConstInt(16, TypeRegistry.I64, span);
            const poll_fn_addr = try fb.emitBinary(.add, frame_for_store, sixteen, TypeRegistry.I64, span);
            _ = try fb.emitPtrStoreValue(poll_fn_addr, poll_fn_idx, span);

            const final_frame = try fb.emitLoadLocal(frame_local, TypeRegistry.I64, span);
            _ = try fb.emitRet(final_frame, span);

            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerAsyncClosureExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const ce = self.tree.closureData(idx);
        const span = self.getSpan(idx);

        var captures = std.ArrayListUnmanaged(CaptureInfo){};
        defer captures.deinit(self.allocator);
        try self.detectCaptures(ce.body, fb, &captures);

        const closure_bare = try std.fmt.allocPrint(self.allocator, "__async_closure_{d}", .{self.closure_counter});
        self.closure_counter += 1;
        const closure_name = try self.qualifyName(closure_bare);

        const saved_builder_func = self.builder.current_func;
        const return_type = if (ce.return_type.unwrap()) |rt| self.resolveTypeNode(rt) else TypeRegistry.VOID;

        self.builder.startFunc(closure_name, TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |closure_fb| {
            self.current_func = closure_fb;
            self.cleanup_stack.clear();

            const param_indices = self.tree.extraNodes(ce.params);
            for (param_indices) |param_idx| {
                const param_name = self.tree.tokenSlice(self.tree.nodeMainToken(param_idx));
                const param_type = self.resolveParamType(param_idx);
                _ = try closure_fb.addParam(param_name, param_type, self.type_reg.sizeOf(param_type));
            }

            if (captures.items.len > 0) {
                const ctxt_ptr = try closure_fb.emitWasmGlobalRead(1, TypeRegistry.I64, span);
                for (captures.items, 0..) |cap, i| {
                    const offset: i64 = @intCast((i + 1) * 8);
                    const cap_addr = try closure_fb.emitAddrOffset(ctxt_ptr, offset, TypeRegistry.I64, span);
                    const cap_val = try closure_fb.emitPtrLoadValue(cap_addr, cap.type_idx, span);
                    const local_idx = try closure_fb.addLocalWithSize(cap.name, cap.type_idx, true, 8);
                    _ = try closure_fb.emitStoreLocal(local_idx, cap_val, span);
                }
            }

            const zero_metadata = try closure_fb.emitConstInt(0, TypeRegistry.I64, span);
            const twenty_four = try closure_fb.emitConstInt(24, TypeRegistry.I64, span);
            var alloc_args = [_]ir.NodeIndex{ zero_metadata, twenty_four };
            const task_ptr = try closure_fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

            const task_local = try closure_fb.addLocalWithSize("__async_task", TypeRegistry.I64, false, 8);
            _ = try closure_fb.emitStoreLocal(task_local, task_ptr, span);
            closure_fb.async_task_local = task_local;
            closure_fb.async_result_type = return_type;

            _ = try self.lowerBlockNode(ce.body);

            self.current_func = null;
        }
        try self.builder.endFunc();
        self.builder.current_func = saved_builder_func;
        self.current_func = if (self.builder.current_func) |*bfb| bfb else null;

        const num_captures = captures.items.len;
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size: u32 = @intCast((1 + num_captures) * 8);
        const total_size = HEAP_HEADER_SIZE + payload_size;

        const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, span);
        var parent_alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const alloc_result = try fb.emitCall("alloc", &parent_alloc_args, false, TypeRegistry.I64, span);

        const temp_name = try std.fmt.allocPrint(self.allocator, "__closure_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_idx = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(temp_idx, alloc_result, span);
        const ptr_node = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);

        const table_idx_node = try fb.emitFuncAddr(closure_name, TypeRegistry.I64, span);
        _ = try fb.emitPtrStoreValue(ptr_node, table_idx_node, span);

        for (captures.items, 0..) |cap, i| {
            const offset: i64 = @intCast((i + 1) * 8);
            const reload_ptr = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);
            const cap_addr = try fb.emitAddrOffset(reload_ptr, offset, TypeRegistry.I64, span);
            const cap_val = try fb.emitLoadLocal(cap.local_idx, cap.type_idx, span);
            _ = try fb.emitPtrStoreValue(cap_addr, cap_val, span);
        }

        return try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);
    }

    // ========================================================================
    // Builtin Helpers
    // ========================================================================

    fn lowerBuiltinLen(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const bc = self.tree.builtinCallData(idx);
        const span = self.getSpan(idx);
        const arg_idx = bc.arg0.unwrap() orelse return ir.null_node;

        const arg_tag = self.tree.nodeTag(arg_idx);
        if (arg_tag == .literal_string) {
            const text = self.tree.tokenSlice(self.tree.nodeMainToken(arg_idx));
            var buf: [4096]u8 = undefined;
            const unescaped = parseStringLiteral(text, &buf);
            return try fb.emitConstInt(@intCast(unescaped.len), TypeRegistry.INT, span);
        }

        if (arg_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(arg_idx));
            if (fb.lookupLocal(name)) |local_idx| {
                const local_type_idx = fb.locals.items[local_idx].type_idx;
                const local_type = self.type_reg.get(local_type_idx);
                if (local_type_idx == TypeRegistry.STRING) return try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, span);
                if (local_type == .slice) return try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, span);
                if (local_type == .array) return try fb.emitConstInt(@intCast(local_type.array.length), TypeRegistry.INT, span);
            }
        }

        const arg_type = self.inferExprType(arg_idx);
        if (arg_type == TypeRegistry.STRING) {
            const str_val = try self.lowerExprNode(arg_idx);
            return try fb.emitSliceLen(str_val, span);
        }
        return ir.null_node;
    }

    fn lowerBuiltinStringMake(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const bc = self.tree.builtinCallData(idx);
        const span = self.getSpan(idx);
        const arg0 = bc.arg0.unwrap() orelse return ir.null_node;
        const arg1 = bc.arg1.unwrap() orelse return ir.null_node;
        const ptr_val = try self.lowerExprNode(arg0);
        const len_val = try self.lowerExprNode(arg1);
        return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = ptr_val, .len = len_val } }, TypeRegistry.STRING, span));
    }

    fn lowerBuiltinAppend(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const bc = self.tree.builtinCallData(idx);
        const span = self.getSpan(idx);
        const arg0 = bc.arg0.unwrap() orelse return ir.null_node;
        const arg1 = bc.arg1.unwrap() orelse return ir.null_node;

        const slice_type_idx = self.inferExprType(arg0);
        const slice_type = self.type_reg.get(slice_type_idx);
        const elem_type_idx: TypeIndex = switch (slice_type) {
            .slice => |s| s.elem,
            .array => |a| a.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type_idx);
        const result_type = self.type_reg.makeSlice(elem_type_idx) catch return ir.null_node;
        const ptr_type = try self.type_reg.makePointer(elem_type_idx);

        var old_ptr: ir.NodeIndex = ir.null_node;
        var old_len: ir.NodeIndex = ir.null_node;
        var old_cap: ir.NodeIndex = ir.null_node;

        const arg_tag = self.tree.nodeTag(arg0);
        if (slice_type == .array and arg_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(arg0));
            if (fb.lookupLocal(name)) |local_idx| {
                old_ptr = try fb.emitAddrLocal(local_idx, ptr_type, span);
                const arr_len = try fb.emitConstInt(@intCast(slice_type.array.length), TypeRegistry.I64, span);
                old_len = arr_len;
                old_cap = arr_len;
            }
        } else if (slice_type == .slice and arg_tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(arg0));
            if (fb.lookupLocal(name)) |local_idx| {
                old_ptr = try fb.emitFieldLocal(local_idx, 0, 0, ptr_type, span);
                old_len = try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, span);
                old_cap = try fb.emitFieldLocal(local_idx, 2, 16, TypeRegistry.I64, span);
            }
        }

        if (old_ptr == ir.null_node or old_len == ir.null_node or old_cap == ir.null_node) return ir.null_node;

        const one = try fb.emitConstInt(1, TypeRegistry.I64, span);
        const new_len = try fb.emitBinary(.add, old_len, one, TypeRegistry.I64, span);
        const elem_size_val = try fb.emitConstInt(@intCast(elem_size), TypeRegistry.I64, span);

        const cond = try fb.emitBinary(.le, new_len, old_cap, TypeRegistry.BOOL, span);

        const result_ptr_name = try std.fmt.allocPrint(self.allocator, "__append_ptr_{d}", .{fb.locals.items.len});
        const result_ptr_local = try fb.addLocalWithSize(result_ptr_name, TypeRegistry.I64, true, 8);
        const result_cap_name = try std.fmt.allocPrint(self.allocator, "__append_cap_{d}", .{fb.locals.items.len});
        const result_cap_local = try fb.addLocalWithSize(result_cap_name, TypeRegistry.I64, true, 8);

        const then_block = try fb.newBlock("append.fast");
        const else_block = try fb.newBlock("append.grow");
        const merge_block = try fb.newBlock("append.merge");

        _ = try fb.emitBranch(cond, then_block, else_block, span);

        fb.setBlock(then_block);
        _ = try fb.emitStoreLocal(result_ptr_local, old_ptr, span);
        _ = try fb.emitStoreLocal(result_cap_local, old_cap, span);
        _ = try fb.emitJump(merge_block, span);

        fb.setBlock(else_block);
        var cap_args = [_]ir.NodeIndex{ new_len, old_cap };
        const new_cap_grow = try fb.emitCall("nextslicecap", &cap_args, false, TypeRegistry.I64, span);
        var grow_args = [_]ir.NodeIndex{ old_ptr, old_len, new_cap_grow, elem_size_val };
        const new_ptr_grow = try fb.emitCall("growslice", &grow_args, false, TypeRegistry.I64, span);
        _ = try fb.emitStoreLocal(result_ptr_local, new_ptr_grow, span);
        _ = try fb.emitStoreLocal(result_cap_local, new_cap_grow, span);
        _ = try fb.emitJump(merge_block, span);

        fb.setBlock(merge_block);
        const new_ptr = try fb.emitLoadLocal(result_ptr_local, TypeRegistry.I64, span);
        const new_cap = try fb.emitLoadLocal(result_cap_local, TypeRegistry.I64, span);

        const elem_val = try self.lowerExprNode(arg1);
        const offset = try fb.emitBinary(.mul, old_len, elem_size_val, TypeRegistry.I64, span);
        const dest_ptr = try fb.emitBinary(.add, new_ptr, offset, ptr_type, span);
        _ = try fb.emitPtrStoreValue(dest_ptr, elem_val, span);

        return try fb.emit(ir.Node.init(.{ .slice_header = .{ .ptr = new_ptr, .len = new_len, .cap = new_cap } }, result_type, span));
    }

    fn lowerBuiltinPrint(self: *Lowerer, idx: Index, is_println: bool, fd: i32) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const bc = self.tree.builtinCallData(idx);
        const span = self.getSpan(idx);
        const arg0 = bc.arg0.unwrap() orelse return ir.null_node;
        const arg_type = self.inferExprType(arg0);
        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
        const type_info = self.type_reg.get(arg_type);
        const is_integer = type_info == .enum_type or arg_type == TypeRegistry.I8 or arg_type == TypeRegistry.I16 or arg_type == TypeRegistry.I32 or arg_type == TypeRegistry.I64 or arg_type == TypeRegistry.INT or arg_type == TypeRegistry.U8 or arg_type == TypeRegistry.U16 or arg_type == TypeRegistry.U32 or arg_type == TypeRegistry.U64 or arg_type == TypeRegistry.UNTYPED_INT;
        const is_bool = arg_type == TypeRegistry.BOOL or arg_type == TypeRegistry.UNTYPED_BOOL;
        const is_float = arg_type == TypeRegistry.F32 or arg_type == TypeRegistry.F64 or arg_type == TypeRegistry.FLOAT or arg_type == TypeRegistry.UNTYPED_FLOAT;

        if (is_integer) {
            const int_val = try self.lowerExprNode(arg0);
            var print_args = [_]ir.NodeIndex{int_val};
            _ = try fb.emitCall(if (fd == 2) "eprint_int" else "print_int", &print_args, false, TypeRegistry.VOID, span);
        } else if (is_bool) {
            const bool_val = try self.lowerExprNode(arg0);
            const true_idx = try fb.addStringLiteral("true");
            const true_str = try fb.emit(ir.Node.init(.{ .const_slice = .{ .string_index = true_idx } }, TypeRegistry.STRING, span));
            const true_ptr = try fb.emitSlicePtr(true_str, ptr_type, span);
            const true_len = try fb.emitConstInt(4, TypeRegistry.I64, span);
            const false_idx = try fb.addStringLiteral("false");
            const false_str = try fb.emit(ir.Node.init(.{ .const_slice = .{ .string_index = false_idx } }, TypeRegistry.STRING, span));
            const false_ptr = try fb.emitSlicePtr(false_str, ptr_type, span);
            const false_len = try fb.emitConstInt(5, TypeRegistry.I64, span);
            const sel_ptr = try fb.emitSelect(bool_val, true_ptr, false_ptr, ptr_type, span);
            const sel_len = try fb.emitSelect(bool_val, true_len, false_len, TypeRegistry.I64, span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I64, span);
            var write_args = [_]ir.NodeIndex{ fd_val, sel_ptr, sel_len };
            _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, span);
        } else if (is_float) {
            const float_val = try self.lowerExprNode(arg0);
            const bits_val = try fb.emitUnary(.i64_reinterpret_f64, float_val, TypeRegistry.I64, span);
            var print_args = [_]ir.NodeIndex{bits_val};
            _ = try fb.emitCall(if (fd == 2) "eprint_float" else "print_float", &print_args, false, TypeRegistry.VOID, span);
        } else {
            const str_val = try self.lowerExprNode(arg0);
            const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, span);
            const len_val = try fb.emitSliceLen(str_val, span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I64, span);
            var write_args = [_]ir.NodeIndex{ fd_val, ptr_val, len_val };
            _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, span);
        }

        if (is_println) {
            const nl_idx = try fb.addStringLiteral("\n");
            const nl_str = try fb.emit(ir.Node.init(.{ .const_slice = .{ .string_index = nl_idx } }, TypeRegistry.STRING, span));
            const nl_ptr = try fb.emitSlicePtr(nl_str, ptr_type, span);
            const nl_len = try fb.emitConstInt(1, TypeRegistry.I64, span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I64, span);
            var nl_args = [_]ir.NodeIndex{ fd_val, nl_ptr, nl_len };
            _ = try fb.emitCall("write", &nl_args, false, TypeRegistry.I64, span);
        }
        return ir.null_node;
    }

    fn lowerGenericFnInstanceVWT(self: *Lowerer, inst_info: checker_mod.GenericInstInfo) !void {
        return self.lowerGenericFnInstance(inst_info, false);
    }

    // ========================================================================
    // Comptime Helpers
    // ========================================================================

    fn resolveComptimeFieldAccess(self: *Lowerer, base_idx: Index, field_name: []const u8) ?comptime_mod.ComptimeValue {
        const base_tag = self.tree.nodeTag(base_idx);
        if (base_tag != .ident) return null;
        const name = self.tree.tokenSlice(self.tree.nodeMainToken(base_idx));
        const cv = self.lookupComptimeValue(name) orelse return null;
        return switch (cv) {
            .enum_field => |ef| {
                if (std.mem.eql(u8, field_name, "name")) return comptime_mod.ComptimeValue{ .string = ef.name };
                if (std.mem.eql(u8, field_name, "value")) return comptime_mod.ComptimeValue{ .int = ef.value };
                return null;
            },
            .type_info => |ti| {
                if (std.mem.eql(u8, field_name, "name")) return comptime_mod.ComptimeValue{ .string = ti.name };
                if (std.mem.eql(u8, field_name, "fields")) return comptime_mod.ComptimeValue{ .array = .{
                    .elements = ti.fields,
                    .elem_type_name = "EnumField",
                } };
                return null;
            },
            .array => |arr| {
                if (std.mem.eql(u8, field_name, "len")) return comptime_mod.ComptimeValue{ .int = @intCast(arr.elements.items.len) };
                return null;
            },
            else => null,
        };
    }

    fn emitComptimeValue(self: *Lowerer, val: comptime_mod.ComptimeValue, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        return switch (val) {
            .int => |v| try fb.emitConstInt(v, TypeRegistry.I64, span),
            .float => |v| try fb.emitConstFloat(v, TypeRegistry.F64, span),
            .string => |s| {
                const copied = try self.allocator.dupe(u8, s);
                const str_idx = try fb.addStringLiteral(copied);
                return try fb.emitConstSlice(str_idx, span);
            },
            .boolean => |b| try fb.emitConstBool(b, span),
            .array => |arr| try self.emitComptimeArray(arr, span),
            else => ir.null_node,
        };
    }

    fn emitComptimeArray(self: *Lowerer, arr: comptime_mod.ComptimeValue.ComptimeArray, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const elem_count = arr.elements.items.len;
        if (elem_count == 0) return try fb.emitConstInt(0, TypeRegistry.I64, span);

        const first = arr.elements.items[0];
        const elem_size: i64 = if (first == .string) @intCast(self.type_reg.sizeOf(TypeRegistry.STRING)) else 8;
        const total_size = elem_size * @as(i64, @intCast(elem_count));

        const size_node = try fb.emitConstInt(total_size, TypeRegistry.I64, span);
        const zero_node = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const base_ptr = try fb.emitCall("alloc", &.{ zero_node, size_node }, true, TypeRegistry.I64, span);

        for (arr.elements.items, 0..) |elem, i| {
            const byte_offset: i64 = elem_size * @as(i64, @intCast(i));
            switch (elem) {
                .int => |v| {
                    const val_node = try fb.emitConstInt(v, TypeRegistry.I64, span);
                    _ = try fb.emitStoreField(base_ptr, @intCast(i), byte_offset, val_node, span);
                },
                .boolean => |b| {
                    const val_node = try fb.emitConstInt(if (b) 1 else 0, TypeRegistry.I64, span);
                    _ = try fb.emitStoreField(base_ptr, @intCast(i), byte_offset, val_node, span);
                },
                .string => |s| {
                    const copied = try self.allocator.dupe(u8, s);
                    const str_idx = try fb.addStringLiteral(copied);
                    const str_node = try fb.emitConstSlice(str_idx, span);
                    const ptr_node = try fb.emitSlicePtr(str_node, TypeRegistry.I64, span);
                    const len_node = try fb.emitSliceLen(str_node, span);
                    _ = try fb.emitStoreField(base_ptr, @intCast(2 * i), byte_offset, ptr_node, span);
                    _ = try fb.emitStoreField(base_ptr, @intCast(2 * i + 1), byte_offset + 8, len_node, span);
                },
                else => {},
            }
        }
        return base_ptr;
    }

    fn lookupComptimeValue(self: *Lowerer, name: []const u8) ?comptime_mod.ComptimeValue {
        return self.comptime_value_vars.get(name) orelse self.global_comptime_values.get(name);
    }

    // ========================================================================
    // Generic Type Resolution Helpers
    // ========================================================================

    fn resolveGenericTypeName(self: *Lowerer, base_name: []const u8, type_args: []const Index) TypeIndex {
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);
        const writer = buf.writer(self.allocator);
        writer.writeAll(base_name) catch return TypeRegistry.VOID;
        writer.writeByte('(') catch return TypeRegistry.VOID;
        for (type_args, 0..) |arg_node, i| {
            if (i > 0) writer.writeByte(';') catch return TypeRegistry.VOID;
            const arg_type = self.resolveTypeArgNode(arg_node);
            std.fmt.format(writer, "{d}", .{arg_type}) catch return TypeRegistry.VOID;
        }
        writer.writeByte(')') catch return TypeRegistry.VOID;
        return self.type_reg.lookupByName(buf.items) orelse TypeRegistry.VOID;
    }

    fn resolveTypeArgNode(self: *Lowerer, idx: Index) TypeIndex {
        const tag = self.tree.nodeTag(idx);
        if (tag == .ident) {
            const name = self.tree.tokenSlice(self.tree.nodeMainToken(idx));
            if (self.type_substitution) |sub| {
                if (sub.get(name)) |substituted| return substituted;
            }
            if (std.mem.eql(u8, name, "void")) return TypeRegistry.VOID;
            if (std.mem.eql(u8, name, "bool")) return TypeRegistry.BOOL;
            if (std.mem.eql(u8, name, "i8")) return TypeRegistry.I8;
            if (std.mem.eql(u8, name, "i16")) return TypeRegistry.I16;
            if (std.mem.eql(u8, name, "i32")) return TypeRegistry.I32;
            if (std.mem.eql(u8, name, "i64")) return TypeRegistry.I64;
            if (std.mem.eql(u8, name, "int")) return TypeRegistry.INT;
            if (std.mem.eql(u8, name, "u8")) return TypeRegistry.U8;
            if (std.mem.eql(u8, name, "u16")) return TypeRegistry.U16;
            if (std.mem.eql(u8, name, "u32")) return TypeRegistry.U32;
            if (std.mem.eql(u8, name, "u64")) return TypeRegistry.U64;
            if (std.mem.eql(u8, name, "f32")) return TypeRegistry.F32;
            if (std.mem.eql(u8, name, "f64")) return TypeRegistry.F64;
            if (std.mem.eql(u8, name, "string")) return TypeRegistry.STRING;
            return self.type_reg.lookupByName(name) orelse TypeRegistry.VOID;
        }
        return self.resolveTypeNode(idx);
    }

    // ========================================================================
    // WasmGC Helpers
    // ========================================================================

    fn gcChunkIndex(self: *Lowerer, struct_type: anytype, semantic_idx: u32) u32 {
        var chunk: u32 = 0;
        for (struct_type.fields[0..semantic_idx]) |field| {
            chunk += self.gcFieldChunks(field.type_idx);
        }
        return chunk;
    }

    fn gcFieldChunks(self: *Lowerer, type_idx: TypeIndex) u32 {
        const info = self.type_reg.get(type_idx);
        if (info == .struct_type) return 1;
        if (info == .pointer and info.pointer.managed) {
            const elem = self.type_reg.get(info.pointer.elem);
            if (elem == .struct_type) return 1;
        }
        const size = self.type_reg.sizeOf(type_idx);
        return @max(1, (size + 7) / 8);
    }

    fn emitGcDefaultValue(self: *Lowerer, field_type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const field_info = self.type_reg.get(field_type_idx);
        if (field_info == .struct_type) {
            const inner_struct = field_info.struct_type;
            var inner_values = try self.allocator.alloc(ir.NodeIndex, inner_struct.fields.len);
            defer self.allocator.free(inner_values);
            for (inner_struct.fields, 0..) |inner_field, j| {
                if (inner_field.default_value != ast_mod.null_node) {
                    inner_values[j] = try self.lowerExprNode(@enumFromInt(inner_field.default_value));
                } else {
                    inner_values[j] = try self.emitGcDefaultValue(inner_field.type_idx, span);
                }
            }
            return try fb.emitGcStructNew(inner_struct.name, inner_values, field_type_idx, span);
        }
        if (field_type_idx == TypeRegistry.STRING or field_info == .slice) {
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = zero, .len = zero } }, field_type_idx, span));
        }
        return try fb.emitConstInt(0, field_type_idx, span);
    }

    fn emitGcStructNewExpanded(self: *Lowerer, type_name: []const u8, struct_type: anytype, semantic_values: []const ir.NodeIndex, struct_type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        var total_chunks: usize = 0;
        for (struct_type.fields) |field| {
            total_chunks += self.gcFieldChunks(field.type_idx);
        }
        if (total_chunks == semantic_values.len) {
            return try fb.emitGcStructNew(type_name, semantic_values, struct_type_idx, span);
        }
        var expanded = try self.allocator.alloc(ir.NodeIndex, total_chunks);
        defer self.allocator.free(expanded);
        var ci: usize = 0;
        for (struct_type.fields, 0..) |field, fi| {
            const chunks = self.gcFieldChunks(field.type_idx);
            if (chunks == 1) {
                expanded[ci] = semantic_values[fi];
                ci += 1;
            } else {
                const val = semantic_values[fi];
                const is_string_or_slice = field.type_idx == TypeRegistry.STRING or self.type_reg.get(field.type_idx) == .slice;
                if (is_string_or_slice) {
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    expanded[ci] = try fb.emitSlicePtr(val, ptr_type, span);
                    ci += 1;
                    expanded[ci] = try fb.emitSliceLen(val, span);
                    ci += 1;
                    if (chunks >= 3) {
                        expanded[ci] = try fb.emitSliceLen(val, span);
                        ci += 1;
                    }
                } else {
                    expanded[ci] = val;
                    ci += 1;
                    for (1..chunks) |_| {
                        expanded[ci] = try fb.emitConstInt(0, TypeRegistry.I64, span);
                        ci += 1;
                    }
                }
            }
        }
        return try fb.emitGcStructNew(type_name, expanded, struct_type_idx, span);
    }

    fn lowerUnionSwitchGC(self: *Lowerer, idx: Index, subject_type: TypeIndex, ut: types_mod.UnionType, result_type: TypeIndex) Error!ir.NodeIndex {
        // WasmGC union switch — stub returning null_node (full GC dispatch not yet needed)
        _ = self;
        _ = idx;
        _ = subject_type;
        _ = ut;
        _ = result_type;
        return ir.null_node;
    }

    // ========================================================================
    // Actor Helpers
    // ========================================================================

    fn isGlobalActorCall(self: *Lowerer, operand_idx: Index) bool {
        const tag = self.tree.nodeTag(operand_idx);
        if (tag != .call_zero and tag != .call_one and tag != .call) return false;
        const cd = self.tree.callData(operand_idx);
        const callee_tag = self.tree.nodeTag(cd.callee);
        if (callee_tag != .ident) return false;
        const name = self.tree.tokenSlice(self.tree.nodeMainToken(cd.callee));
        return self.chk.global_actor_fns.contains(name);
    }

    fn isCrossActorCall(self: *Lowerer, operand_idx: Index) bool {
        const tag = self.tree.nodeTag(operand_idx);
        if (tag != .call_zero and tag != .call_one and tag != .call) return false;
        const cd = self.tree.callData(operand_idx);
        const callee_tag = self.tree.nodeTag(cd.callee);
        if (callee_tag != .field_access) return false;
        const fa_data = self.tree.nodeData(cd.callee);
        const base_type = self.inferExprType(fa_data.node_and_token[0]);
        const base_info = self.type_reg.get(base_type);
        const struct_name = switch (base_info) {
            .struct_type => |st| st.name,
            .pointer => |ptr| switch (self.type_reg.get(ptr.elem)) {
                .struct_type => |st| st.name,
                else => return false,
            },
            else => return false,
        };
        return self.chk.actor_types.contains(struct_name);
    }

    // ========================================================================
    // Managed Value Stubs (arc_insertion not yet ported)
    // ========================================================================

    fn lowerExprManaged(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        // Without arc_insertion.zig, just lower the expression normally
        return self.lowerExprNode(idx);
    }

    fn managedFromLowered(self: *Lowerer, value: ir.NodeIndex, ast_idx: Index, type_idx: TypeIndex) !ir.NodeIndex {
        _ = ast_idx;
        _ = type_idx;
        _ = self;
        return value;
    }
};

// ============================================================================
// Helpers
// ============================================================================

fn tokenToBinaryOp(tok: Token) ir.BinaryOp {
    return switch (tok) {
        .add, .add_assign, .concat => .add,
        .sub, .sub_assign => .sub,
        .mul, .mul_assign => .mul,
        .quo, .quo_assign => .div,
        .rem, .rem_assign => .mod,
        .eql => .eq,
        .neq => .ne,
        .lss => .lt,
        .leq => .le,
        .gtr => .gt,
        .geq => .ge,
        .land, .kw_and => .@"and",
        .lor, .kw_or => .@"or",
        .@"and", .and_assign => .bit_and,
        .@"or", .or_assign => .bit_or,
        .xor, .xor_assign => .bit_xor,
        .shl => .shl,
        .shr => .shr,
        else => .add,
    };
}

fn tokenToUnaryOp(tok: Token) ir.UnaryOp {
    return switch (tok) {
        .sub => .neg,
        .lnot, .kw_not => .not,
        .not => .bit_not,
        .question => .optional_unwrap,
        else => .neg,
    };
}

/// Process escape sequences in a raw string (no surrounding quotes).
fn unescapeString(text: []const u8, out_buf: []u8) []const u8 {
    var out_idx: usize = 0;
    var i: usize = 0;
    while (i < text.len and out_idx < out_buf.len) {
        if (text[i] == '\\' and i + 1 < text.len) {
            const escaped = switch (text[i + 1]) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '0' => @as(u8, 0),
                'x' => blk: {
                    if (i + 3 < text.len) {
                        const val = std.fmt.parseInt(u8, text[i + 2 .. i + 4], 16) catch 0;
                        i += 2;
                        break :blk val;
                    }
                    break :blk text[i + 1];
                },
                else => text[i + 1],
            };
            out_buf[out_idx] = escaped;
            out_idx += 1;
            i += 2;
        } else {
            out_buf[out_idx] = text[i];
            out_idx += 1;
            i += 1;
        }
    }
    return out_buf[0..out_idx];
}

fn parseStringLiteral(text: []const u8, out_buf: []u8) []const u8 {
    if (text.len < 2 or text[0] != '"' or text[text.len - 1] != '"') return text;
    const inner = text[1 .. text.len - 1];
    var out_idx: usize = 0;
    var i: usize = 0;
    while (i < inner.len and out_idx < out_buf.len) {
        if (inner[i] == '\\' and i + 1 < inner.len) {
            const escaped = switch (inner[i + 1]) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '0' => @as(u8, 0),
                'x' => blk: {
                    if (i + 3 < inner.len) {
                        const val = std.fmt.parseInt(u8, inner[i + 2 .. i + 4], 16) catch 0;
                        i += 2;
                        break :blk val;
                    }
                    break :blk inner[i + 1];
                },
                else => inner[i + 1],
            };
            out_buf[out_idx] = escaped;
            out_idx += 1;
            i += 2;
        } else {
            out_buf[out_idx] = inner[i];
            out_idx += 1;
            i += 1;
        }
    }
    return out_buf[0..out_idx];
}

fn parseCharLiteral(text: []const u8) u8 {
    if (text.len < 3 or text[0] != '\'' or text[text.len - 1] != '\'') return 0;
    const inner = text[1 .. text.len - 1];
    if (inner.len == 0) return 0;
    if (inner[0] != '\\') return inner[0];
    if (inner.len < 2) return 0;
    return switch (inner[1]) {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '\\' => '\\',
        '\'' => '\'',
        '0' => 0,
        else => inner[1],
    };
}

// ============================================================================
// Tests
// ============================================================================

test "Lowerer struct compiles" {
    // Basic compilation test — verifies the struct and all its types resolve.
    const allocator = std.testing.allocator;
    _ = allocator;
    // Cannot easily construct a full Lowerer without a parsed AST, checker, etc.
    // This test verifies the struct definition compiles successfully.
}

test "CleanupStack basic operations" {
    const allocator = std.testing.allocator;
    var stack = CleanupStack.init(allocator);
    defer stack.deinit();

    try std.testing.expectEqual(@as(usize, 0), stack.getScopeDepth());

    const cleanup = Cleanup.init(.release, 42, TypeRegistry.I64);
    const handle = try stack.push(cleanup);
    try std.testing.expectEqual(@as(usize, 0), handle);
    try std.testing.expectEqual(@as(usize, 1), stack.getScopeDepth());

    const cleanup2 = Cleanup.initForLocal(.release, 43, TypeRegistry.I64, 5);
    _ = try stack.push(cleanup2);
    try std.testing.expect(stack.hasCleanupForLocal(5));
    try std.testing.expect(!stack.hasCleanupForLocal(6));

    _ = stack.disableForLocal(5);
    try std.testing.expect(!stack.hasCleanupForLocal(5));

    stack.clear();
    try std.testing.expectEqual(@as(usize, 0), stack.getScopeDepth());
}

test "shouldSkipQualification" {
    try std.testing.expect(Lowerer.shouldSkipQualification("main"));
    try std.testing.expect(Lowerer.shouldSkipQualification("__cot_init_globals"));
    try std.testing.expect(Lowerer.shouldSkipQualification("__test_foo"));
    try std.testing.expect(Lowerer.shouldSkipQualification("__bench_bar"));
    try std.testing.expect(!Lowerer.shouldSkipQualification("myFunc"));
    try std.testing.expect(!Lowerer.shouldSkipQualification(""));
}
