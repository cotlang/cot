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
const ir = @import("ir.zig");
const types_mod = @import("types.zig");
const source_mod = @import("source.zig");
const errors_mod = @import("errors.zig");
const checker_mod = @import("checker.zig");
const comptime_mod = @import("comptime.zig");
const target_mod = @import("target.zig");

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
                // TODO: destructure lowering
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
            try self.emitCleanups(0);
            _ = try fb.emitRet(null, span);
            return;
        }

        // return_expr: data is the value node
        const data = self.tree.nodeData(idx);
        const value_idx = data.node;

        // Simple path: lower value, emit cleanups, return
        const value_node = try self.lowerExprNode(value_idx);
        if (value_node == ir.null_node) {
            try self.emitCleanups(0);
            _ = try fb.emitRet(null, span);
            return;
        }

        // SRET path
        if (fb.sret_return_type) |_| {
            // TODO: full SRET return handling (compound copy to __sret pointer)
            try self.emitCleanups(0);
            _ = try fb.emitRet(null, span);
            return;
        }

        // Check for defers — capture return value first
        if (self.hasDeferCleanups()) {
            const ret_type = fb.return_type;
            const ret_size = self.type_reg.sizeOf(ret_type);
            const tmp_local = try fb.addLocalWithSize("__ret_tmp", ret_type, false, ret_size);
            _ = try fb.emitStoreLocal(tmp_local, value_node, span);
            try self.emitCleanups(0);
            const loaded = try fb.emitLoadLocal(tmp_local, ret_type, span);
            _ = try fb.emitRet(loaded, span);
            return;
        }

        try self.emitCleanups(0);
        _ = try fb.emitRet(value_node, span);
    }

    fn lowerLocalVarDecl(self: *Lowerer, idx: Index) !void {
        const fb = self.current_func orelse return;
        const vd = self.tree.localVarData(idx);
        const span = self.getSpan(idx);

        var type_idx = TypeRegistry.VOID;
        if (vd.type_expr.unwrap()) |te| {
            type_idx = self.resolveTypeNode(te);
        } else if (vd.value.unwrap()) |ve| {
            type_idx = self.inferExprType(ve);
        }

        const size = self.type_reg.sizeOf(type_idx);
        const local_idx = try fb.addLocalWithSize(vd.name, type_idx, !vd.is_const, size);

        if (vd.value.unwrap()) |value_idx| {
            const value_tag = self.tree.nodeTag(value_idx);

            // undefined / .{} zero init
            if (value_tag == .literal_undefined or value_tag == .zero_init) {
                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, span);
                const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, span);
                var args = [_]ir.NodeIndex{ local_addr, size_node };
                _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, span);
                return;
            }

            // General case: lower the value and store
            const value_node = try self.lowerExprNode(value_idx);
            if (value_node != ir.null_node) {
                // String compound init
                if (type_idx == TypeRegistry.STRING) {
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const ptr_val = try fb.emitSlicePtr(value_node, ptr_type, span);
                    const len_val = try fb.emitSliceLen(value_node, span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
                    _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
                } else {
                    _ = try fb.emitStoreLocal(local_idx, value_node, span);
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
                // TODO: full field assign — for now just evaluate target+value for side effects
            },
            .index => {
                // TODO: full index assign — for now just evaluate target+value for side effects
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
            return;
        }

        // Array/slice iteration — TODO: full implementation
        if (fd.iterable.unwrap()) |_| {
            // Stub: just lower the body once for now
            _ = try self.lowerBlockNode(fd.body);
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

        // Resolve callee name
        const callee_tag = self.tree.nodeTag(cd.callee);
        if (callee_tag == .ident) {
            const func_name = self.tree.tokenSlice(self.tree.nodeMainToken(cd.callee));
            const resolved_name = try self.resolveCallName(func_name);

            // Collect args
            var args_list = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer args_list.deinit(self.allocator);

            if (cd.single_arg.unwrap()) |single| {
                const arg = try self.lowerExprNode(single);
                try args_list.append(self.allocator, arg);
            } else {
                const arg_indices = self.tree.extraNodes(cd.args);
                for (arg_indices) |arg_idx| {
                    const arg = try self.lowerExprNode(arg_idx);
                    try args_list.append(self.allocator, arg);
                }
            }

            const result_type = self.inferExprType(idx);
            return fb.emitCall(resolved_name, args_list.items, false, result_type, span);
        }

        // TODO: method calls, indirect calls
        return ir.null_node;
    }

    // ========================================================================
    // Expression lowering stubs — will be ported in subsequent steps
    // ========================================================================

    fn lowerFieldAccess(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerFieldAccess not yet ported");
    }

    fn lowerIndexExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerIndexExpr not yet ported");
    }

    fn lowerStructInitExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerStructInitExpr not yet ported");
    }

    fn lowerNewExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerNewExpr not yet ported");
    }

    fn lowerBuiltinCall(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerBuiltinCall not yet ported");
    }

    fn lowerClosureExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerClosureExpr not yet ported");
    }

    fn lowerStringInterp(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerStringInterp not yet ported");
    }

    fn lowerSwitchExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerSwitchExpr not yet ported");
    }

    fn lowerIfExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerIfExpr not yet ported");
    }

    fn lowerDerefExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerDerefExpr not yet ported");
    }

    fn lowerAddrOfExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerAddrOfExpr not yet ported");
    }

    fn lowerTryExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerTryExpr not yet ported");
    }

    fn lowerAwaitExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerAwaitExpr not yet ported");
    }

    fn lowerCatchExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerCatchExpr not yet ported");
    }

    fn lowerOrelseExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerOrelseExpr not yet ported");
    }

    fn lowerConcatExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerConcatExpr not yet ported");
    }

    fn lowerPipeExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerPipeExpr not yet ported");
    }

    fn lowerErrorLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerErrorLiteral not yet ported");
    }

    fn lowerArrayLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerArrayLiteral not yet ported");
    }

    fn lowerTupleLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerTupleLiteral not yet ported");
    }

    fn lowerSliceExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerSliceExpr not yet ported");
    }

    fn lowerCharLiteral(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerCharLiteral not yet ported");
    }

    fn lowerBlockExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerBlockExpr not yet ported");
    }

    fn lowerGenericCall(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerGenericCall not yet ported");
    }

    fn lowerComptimeBlock(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerComptimeBlock not yet ported");
    }

    fn lowerTaskExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerTaskExpr not yet ported");
    }

    fn lowerUnwrapExpr(self: *Lowerer, idx: Index) Error!ir.NodeIndex {
        _ = self;
        _ = idx;
        @panic("TODO: lowerUnwrapExpr not yet ported");
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
};

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
