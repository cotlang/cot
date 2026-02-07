//! AST-to-IR lowering pass. Transforms type-checked AST into flat IR.

const std = @import("std");
const ast = @import("ast.zig");
const ir = @import("ir.zig");
const types = @import("types.zig");
const source = @import("source.zig");
const errors = @import("errors.zig");
const checker = @import("checker.zig");
const token = @import("token.zig");
const arc = @import("arc_insertion.zig");

const Allocator = std.mem.Allocator;
const Ast = ast.Ast;
const NodeIndex = ast.NodeIndex;
const null_node = ast.null_node;
const TypeIndex = types.TypeIndex;
const TypeRegistry = types.TypeRegistry;
const Span = source.Span;
const Pos = source.Pos;
const ErrorReporter = errors.ErrorReporter;
const Token = token.Token;

pub const Lowerer = struct {
    allocator: Allocator,
    tree: *const Ast,
    type_reg: *TypeRegistry,
    err: *ErrorReporter,
    builder: ir.Builder,
    chk: *checker.Checker,
    current_func: ?*ir.FuncBuilder = null,
    temp_counter: u32 = 0,
    loop_stack: std.ArrayListUnmanaged(LoopContext),
    cleanup_stack: arc.CleanupStack,
    const_values: std.StringHashMap(i64),
    test_mode: bool = false,
    test_names: std.ArrayListUnmanaged([]const u8),
    test_display_names: std.ArrayListUnmanaged([]const u8),
    current_test_name: ?[]const u8 = null,
    closure_counter: u32 = 0,
    lowered_generics: std.StringHashMap(void),
    type_substitution: ?std.StringHashMap(TypeIndex) = null,

    pub const Error = error{OutOfMemory};

    const LoopContext = struct {
        cond_block: ir.BlockIndex,
        exit_block: ir.BlockIndex,
        cleanup_depth: usize,
        label: ?[]const u8 = null,
    };

    pub fn init(allocator: Allocator, tree: *const Ast, type_reg: *TypeRegistry, err: *ErrorReporter, chk: *checker.Checker) Lowerer {
        return initWithBuilder(allocator, tree, type_reg, err, chk, ir.Builder.init(allocator, type_reg));
    }

    pub fn initWithBuilder(allocator: Allocator, tree: *const Ast, type_reg: *TypeRegistry, err: *ErrorReporter, chk: *checker.Checker, builder: ir.Builder) Lowerer {
        return .{
            .allocator = allocator,
            .tree = tree,
            .type_reg = type_reg,
            .err = err,
            .builder = builder,
            .chk = chk,
            .loop_stack = .{},
            .cleanup_stack = arc.CleanupStack.init(allocator),
            .const_values = std.StringHashMap(i64).init(allocator),
            .test_names = .{},
            .test_display_names = .{},
            .lowered_generics = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn setTestMode(self: *Lowerer, enabled: bool) void { self.test_mode = enabled; }
    pub fn addTestName(self: *Lowerer, name: []const u8) !void { try self.test_names.append(self.allocator, name); }
    pub fn addTestDisplayName(self: *Lowerer, name: []const u8) !void { try self.test_display_names.append(self.allocator, name); }
    pub fn getTestNames(self: *const Lowerer) []const []const u8 { return self.test_names.items; }
    pub fn getTestDisplayNames(self: *const Lowerer) []const []const u8 { return self.test_display_names.items; }

    pub fn deinit(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
        self.cleanup_stack.deinit();
        self.const_values.deinit();
        self.test_names.deinit(self.allocator);
        self.test_display_names.deinit(self.allocator);
        self.lowered_generics.deinit();
        self.builder.deinit();
    }

    pub fn deinitWithoutBuilder(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
        self.const_values.deinit();
        self.test_names.deinit(self.allocator);
        self.test_display_names.deinit(self.allocator);
        self.lowered_generics.deinit();
    }

    pub fn lower(self: *Lowerer) !ir.IR {
        try self.lowerToBuilder();
        return try self.builder.getIR();
    }

    pub fn lowerToBuilder(self: *Lowerer) !void {
        for (self.tree.getRootDecls()) |decl_idx| try self.lowerDecl(decl_idx);
        // Queue all generic impl block method instances for lowering.
        // Unlike regular generic functions (queued at call site via ensureGenericFnQueued),
        // impl block methods are registered by the checker during struct instantiation and
        // need to be pre-queued here so lowerQueuedGenericFunctions picks them up.
        var inst_it = self.chk.generic_inst_by_name.valueIterator();
        while (inst_it.next()) |inst_info| {
            if (inst_info.type_param_names.len > 0) {
                try self.lowered_generics.put(inst_info.concrete_name, {});
            }
        }
        // Zig pattern: process queued generic instantiations as top-level functions
        // (deferred, not inline — avoids corrupting builder state during nested lowering)
        try self.lowerQueuedGenericFunctions();
    }

    pub fn generateTestRunner(self: *Lowerer) !void {
        if (self.test_names.items.len == 0) return;
        const span = Span.init(Pos.zero, Pos.zero);
        self.builder.startFunc("main", TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
            for (self.test_names.items, self.test_display_names.items) |test_name, display_name| {
                const name_copy = try self.allocator.dupe(u8, display_name);
                const str_idx = try fb.addStringLiteral(name_copy);
                const str_slice = try fb.emitConstSlice(str_idx, span);
                const name_ptr = try fb.emitSlicePtr(str_slice, ptr_type, span);
                const name_len = try fb.emitSliceLen(str_slice, span);
                var print_args = [_]ir.NodeIndex{ name_ptr, name_len };
                _ = try fb.emitCall("__test_print_name", &print_args, false, TypeRegistry.VOID, span);
                var no_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall(test_name, &no_args, false, TypeRegistry.VOID, span);
                _ = try fb.emitCall("__test_pass", &no_args, false, TypeRegistry.VOID, span);
            }
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitRet(zero, span);
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    // ============================================================================
    // Declaration Lowering
    // ============================================================================

    fn lowerDecl(self: *Lowerer, idx: NodeIndex) !void {
        const node = self.tree.getNode(idx) orelse return;
        const decl = node.asDecl() orelse return;
        switch (decl) {
            .fn_decl => |d| try self.lowerFnDecl(d),
            .var_decl => |d| try self.lowerGlobalVarDecl(d),
            .struct_decl => |d| try self.lowerStructDecl(d),
            .impl_block => |d| try self.lowerImplBlock(d),
            .test_decl => |d| if (self.test_mode) try self.lowerTestDecl(d),
            .enum_decl, .union_decl, .type_alias, .import_decl, .error_set_decl, .bad_decl => {},
        }
    }

    fn lowerFnDecl(self: *Lowerer, fn_decl: ast.FnDecl) !void {
        if (fn_decl.is_extern) return;
        if (fn_decl.type_params.len > 0) return; // Skip generic fn defs — lowered on demand at call sites
        if (self.test_mode and std.mem.eql(u8, fn_decl.name, "main")) return;
        const return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;
        self.builder.startFunc(fn_decl.name, TypeRegistry.VOID, return_type, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            for (fn_decl.params) |param| {
                const param_type = self.resolveTypeNode(param.type_expr);
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }
            if (fn_decl.body != null_node) {
                _ = try self.lowerBlockNode(fn_decl.body);
                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, fn_decl.span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerGlobalVarDecl(self: *Lowerer, var_decl: ast.VarDecl) !void {
        var type_idx = TypeRegistry.VOID;
        if (var_decl.type_expr != null_node) {
            type_idx = self.resolveTypeNode(var_decl.type_expr);
        } else if (var_decl.value != null_node) {
            type_idx = self.inferExprType(var_decl.value);
        }
        if (var_decl.is_const) {
            if (self.chk.scope.lookup(var_decl.name)) |sym| {
                if (sym.const_value) |value| {
                    try self.const_values.put(var_decl.name, value);
                    return;
                }
            }
        }
        const type_size: u32 = @intCast(self.type_reg.sizeOf(type_idx));
        try self.builder.addGlobal(ir.Global.initWithSize(var_decl.name, type_idx, var_decl.is_const, var_decl.span, type_size));
    }

    fn lowerStructDecl(self: *Lowerer, struct_decl: ast.StructDecl) !void {
        if (struct_decl.type_params.len > 0) return; // Skip generic struct defs — instantiated on demand
        const struct_type_idx = self.type_reg.lookupByName(struct_decl.name) orelse TypeRegistry.VOID;
        try self.builder.addStruct(.{ .name = struct_decl.name, .type_idx = struct_type_idx, .span = struct_decl.span });
    }

    fn lowerImplBlock(self: *Lowerer, impl_block: ast.ImplBlock) !void {
        // Generic impl blocks: methods lowered via lowerQueuedGenericFunctions
        if (impl_block.type_params.len > 0) return;
        for (impl_block.methods) |method_idx| {
            const node = self.tree.getNode(method_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl == .fn_decl) {
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_block.type_name, decl.fn_decl.name });
                try self.lowerMethodWithName(decl.fn_decl, synth_name);
            }
        }
    }

    fn lowerMethodWithName(self: *Lowerer, fn_decl: ast.FnDecl, synth_name: []const u8) !void {
        const return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;
        self.builder.startFunc(synth_name, TypeRegistry.VOID, return_type, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            for (fn_decl.params) |param| {
                const param_type = self.resolveTypeNode(param.type_expr);
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }
            if (fn_decl.body != null_node) {
                _ = try self.lowerBlockNode(fn_decl.body);
                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, fn_decl.span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerTestDecl(self: *Lowerer, test_decl: ast.TestDecl) !void {
        const test_name = try self.sanitizeTestName(test_decl.name);
        try self.test_names.append(self.allocator, test_name);
        try self.test_display_names.append(self.allocator, test_decl.name);
        self.builder.startFunc(test_name, TypeRegistry.VOID, TypeRegistry.VOID, test_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.current_test_name = test_decl.name;
            self.cleanup_stack.clear();
            if (test_decl.body != null_node) {
                _ = try self.lowerBlockNode(test_decl.body);
                if (fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, test_decl.span);
                }
            }
            self.current_test_name = null;
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

    // ============================================================================
    // Statement Lowering
    // ============================================================================

    fn lowerBlockNode(self: *Lowerer, idx: NodeIndex) Error!bool {
        const node = self.tree.getNode(idx) orelse return false;
        if (node.asStmt()) |stmt| return try self.lowerStmt(stmt);
        if (node.asExpr()) |expr| {
            if (expr == .block_expr) {
                const fb = self.current_func orelse return false;
                const scope_depth = fb.markScopeEntry();
                var terminated = false;
                for (expr.block_expr.stmts) |stmt_idx| {
                    const stmt_node = self.tree.getNode(stmt_idx) orelse continue;
                    if (stmt_node.asStmt()) |s| {
                        if (try self.lowerStmt(s)) { terminated = true; break; }
                    }
                }
                if (!terminated and expr.block_expr.expr != null_node) _ = try self.lowerExprNode(expr.block_expr.expr);
                fb.restoreScope(scope_depth);
                return terminated;
            }
            _ = try self.lowerExpr(expr);
        }
        return false;
    }

    fn lowerStmt(self: *Lowerer, stmt: ast.Stmt) Error!bool {
        switch (stmt) {
            .return_stmt => |ret| { try self.lowerReturn(ret); return true; },
            .var_stmt => |v| { try self.lowerLocalVarDecl(v); return false; },
            .assign_stmt => |a| { try self.lowerAssign(a); return false; },
            .if_stmt => |i| { try self.lowerIf(i); return false; },
            .while_stmt => |w| { try self.lowerWhile(w); return false; },
            .for_stmt => |f| { try self.lowerFor(f); return false; },
            .block_stmt => |block| {
                const fb = self.current_func orelse return false;
                const cleanup_depth = self.cleanup_stack.getScopeDepth();
                const scope_depth = fb.markScopeEntry();
                for (block.stmts) |stmt_idx| {
                    const stmt_node = self.tree.getNode(stmt_idx) orelse continue;
                    if (stmt_node.asStmt()) |s| {
                        if (try self.lowerStmt(s)) { fb.restoreScope(scope_depth); return true; }
                    }
                }
                try self.emitCleanups(cleanup_depth);
                fb.restoreScope(scope_depth);
                return false;
            },
            .break_stmt => |bs| { try self.lowerBreak(bs.label); return true; },
            .continue_stmt => |cs| { try self.lowerContinue(cs.label); return true; },
            .expr_stmt => |es| { _ = try self.lowerExprNode(es.expr); return false; },
            .defer_stmt => |ds| {
                // Push defer as cleanup entry on unified stack (Swift's DeferCleanup pattern)
                const cleanup = arc.Cleanup{
                    .kind = .defer_expr,
                    .value = @intCast(ds.expr),
                    .type_idx = TypeRegistry.VOID,
                    .state = .active,
                    .local_idx = null,
                };
                _ = try self.cleanup_stack.push(cleanup);
                return false;
            },
            .bad_stmt => return false,
        }
    }

    fn lowerReturn(self: *Lowerer, ret: ast.ReturnStmt) !void {
        const fb = self.current_func orelse return;
        var value_node: ?ir.NodeIndex = null;
        if (ret.value != null_node) {
            // Check if the return expression is an error literal in an error union function
            const ret_node = self.tree.getNode(ret.value);
            const is_error_literal = if (ret_node) |n| if (n.asExpr()) |e| e == .error_literal else false else false;

            // Check if function returns error union and value is a success type (needs wrapping)
            const ret_type = fb.return_type;
            const ret_type_info = self.type_reg.get(ret_type);
            const is_error_union_fn = ret_type_info == .error_union;

            if (is_error_literal and is_error_union_fn) {
                // error.X in error union function — lowerErrorLiteral returns a pointer to the EU
                const lowered = try self.lowerExprNode(ret.value);
                if (lowered != ir.null_node) value_node = lowered;
            } else if (is_error_union_fn) {
                // Success value in error union function — wrap as tag=0, payload=value
                // Returns a POINTER to the error union in the stack frame
                const lowered = try self.lowerExprNode(ret.value);
                if (lowered != ir.null_node) {
                    const eu_size = self.type_reg.sizeOf(ret_type);
                    const tmp_local = try fb.addLocalWithSize("__ret_eu", ret_type, false, eu_size);
                    const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, ret.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, ret.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, lowered, ret.span);
                    value_node = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, ret.span);
                }
            } else {
                const lowered = try self.lowerExprNode(ret.value);
                if (lowered != ir.null_node) value_node = lowered;
            }

            // Forward ownership: if returning a local with ARC cleanup, disable it.
            // The caller receives ownership - we don't release here.
            // Reference: Swift's ManagedValue::forward() pattern
            if (ret_node) |node| {
                if (node.asExpr()) |expr| {
                    if (expr == .ident) {
                        if (fb.lookupLocal(expr.ident.name)) |local_idx| {
                            _ = self.cleanup_stack.disableForLocal(local_idx);
                        }
                    }
                }
            }

            // If there are defers on the cleanup stack, capture the return value into a temp
            // local so defers can't modify it. Zig semantics: `return x` captures x, then
            // defers run (but the captured value is returned, not the modified one).
            if (value_node != null and self.hasDeferCleanups()) {
                const ret_val = value_node.?;
                const ret_size = self.type_reg.sizeOf(ret_type);
                const tmp_local = try fb.addLocalWithSize("__ret_tmp", ret_type, false, ret_size);
                _ = try fb.emitStoreLocal(tmp_local, ret_val, ret.span);
                try self.emitCleanups(0);
                value_node = try fb.emitLoadLocal(tmp_local, ret_type, ret.span);
                _ = try fb.emitRet(value_node, ret.span);
                return;
            }
        }
        try self.emitCleanups(0);
        _ = try fb.emitRet(value_node, ret.span);
    }

    /// Check if there are any active defer cleanups on the stack.
    fn hasDeferCleanups(self: *const Lowerer) bool {
        for (self.cleanup_stack.items.items) |cleanup| {
            if (cleanup.isActive() and cleanup.kind == .defer_expr) return true;
        }
        return false;
    }

    /// Check if an expression's root local has an active ARC cleanup.
    /// Walks chains: a.b.c → checks if a has cleanup.
    /// Reference: Swift checks ManagedValue::hasCleanup() on base.
    fn baseHasCleanup(self: *Lowerer, base_idx: NodeIndex) bool {
        const base_node = self.tree.getNode(base_idx) orelse return false;
        const base_expr = base_node.asExpr() orelse return false;
        if (base_expr == .ident) {
            const fb = self.current_func orelse return false;
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                return self.cleanup_stack.hasCleanupForLocal(local_idx);
            }
        }
        if (base_expr == .field_access) return self.baseHasCleanup(base_expr.field_access.base);
        if (base_expr == .index) return self.baseHasCleanup(base_expr.index.base);
        return false;
    }

    /// Evaluate a deferred AST node — may be an expression or a statement.
    fn lowerDeferredNode(self: *Lowerer, node_idx: ast.NodeIndex) Error!void {
        const node = self.tree.getNode(node_idx) orelse return;
        if (node.asStmt()) |stmt| {
            _ = try self.lowerStmt(stmt);
        } else if (node.asExpr()) |_| {
            _ = try self.lowerExprNode(node_idx);
        }
    }

    /// Emit all cleanups (releases + defers) down to target_depth.
    /// Called at scope exit (block end, return, break, etc.)
    /// Unified stack handles both ARC releases and deferred expressions in LIFO order.
    /// Port of Swift's CleanupManager pattern.
    fn emitCleanups(self: *Lowerer, target_depth: usize) Error!void {
        const fb = self.current_func orelse return;
        const items = self.cleanup_stack.getActiveCleanups();

        // Process in reverse order (LIFO - last registered, first emitted)
        var i = items.len;
        while (i > target_depth) {
            i -= 1;
            const cleanup = items[i];
            if (cleanup.isActive()) {
                switch (cleanup.kind) {
                    .release => {
                        // For locals: reload current value (may have been reassigned)
                        const value = if (cleanup.local_idx) |lidx|
                            try fb.emitLoadLocal(lidx, cleanup.type_idx, Span.zero)
                        else
                            cleanup.value;
                        var args = [_]ir.NodeIndex{value};
                        _ = try fb.emitCall("cot_release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .defer_expr => {
                        try self.lowerDeferredNode(@intCast(cleanup.value));
                    },
                    .end_borrow => {},
                }
            }
        }

        // Pop the cleanups we just emitted
        while (self.cleanup_stack.items.items.len > target_depth) {
            _ = self.cleanup_stack.items.pop();
        }
    }

    /// Emit cleanups from current depth to target WITHOUT popping.
    /// Used for break/continue — other paths still need these cleanups.
    /// Port of Swift's emitBranchAndCleanups pattern.
    fn emitCleanupsNoPop(self: *Lowerer, target_depth: usize) Error!void {
        const fb = self.current_func orelse return;
        const items = self.cleanup_stack.getActiveCleanups();
        var i = items.len;
        while (i > target_depth) {
            i -= 1;
            const cleanup = items[i];
            if (cleanup.isActive()) {
                switch (cleanup.kind) {
                    .release => {
                        const value = if (cleanup.local_idx) |lidx|
                            try fb.emitLoadLocal(lidx, cleanup.type_idx, Span.zero)
                        else
                            cleanup.value;
                        var args = [_]ir.NodeIndex{value};
                        _ = try fb.emitCall("cot_release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .defer_expr => {
                        try self.lowerDeferredNode(@intCast(cleanup.value));
                    },
                    .end_borrow => {},
                }
            }
        }
        // Do NOT pop — other code paths still need these cleanups
    }

    fn lowerLocalVarDecl(self: *Lowerer, var_stmt: ast.VarStmt) !void {
        const fb = self.current_func orelse return;
        var type_idx = TypeRegistry.VOID;
        if (var_stmt.type_expr != null_node) {
            type_idx = self.resolveTypeNode(var_stmt.type_expr);
        } else if (var_stmt.value != null_node) {
            type_idx = self.inferExprType(var_stmt.value);
        }
        const size = self.type_reg.sizeOf(type_idx);
        const local_idx = try fb.addLocalWithSize(var_stmt.name, type_idx, !var_stmt.is_const, size);

        if (var_stmt.value != null_node) {
            const value_node_ast = self.tree.getNode(var_stmt.value);
            const value_expr = if (value_node_ast) |n| n.asExpr() else null;

            // Check for undefined literal or .{} zero init - zero memory
            const is_undefined = if (value_expr) |e| (e == .literal and e.literal.kind == .undefined_lit) else false;
            const is_zero_init = if (value_expr) |e| (e == .zero_init) else false;
            if (is_undefined or is_zero_init) {
                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, var_stmt.span);
                const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, var_stmt.span);
                var args = [_]ir.NodeIndex{ local_addr, size_node };
                _ = try fb.emitCall("cot_memset_zero", &args, false, TypeRegistry.VOID, var_stmt.span);
                return;
            }

            const is_array = self.type_reg.isArray(type_idx);
            const is_slice = self.type_reg.isSlice(type_idx);
            const is_struct_literal = if (value_expr) |e| e == .struct_init else false;

            if (type_idx == TypeRegistry.STRING) {
                try self.lowerStringInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (is_slice) {
                try self.lowerSliceInit(local_idx, var_stmt.value, type_idx, var_stmt.span);
            } else if (is_array) {
                try self.lowerArrayInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (is_struct_literal) {
                try self.lowerStructInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (self.type_reg.get(type_idx) == .union_type) {
                try self.lowerUnionInit(local_idx, var_stmt.value, type_idx, var_stmt.span);
            } else {
                const type_info = self.type_reg.get(type_idx);
                const is_struct_copy = type_info == .struct_type and value_expr != null and
                    (value_expr.? == .field_access or value_expr.? == .index or value_expr.? == .deref);
                if (is_struct_copy) {
                    const src_addr = try self.lowerExprNode(var_stmt.value);
                    if (src_addr == ir.null_node) return;
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const dst_addr = try fb.emitAddrLocal(local_idx, ptr_type, var_stmt.span);
                    const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, var_stmt.span);
                    var args = [_]ir.NodeIndex{ dst_addr, src_addr, size_node };
                    _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, var_stmt.span);
                } else {
                    const value_node = try self.lowerExprNode(var_stmt.value);
                    if (value_node == ir.null_node) return;
                    _ = try fb.emitStoreLocal(local_idx, value_node, var_stmt.span);

                    // Register cleanup for ARC values.
                    // Reference: Swift's emitManagedRValueWithCleanup (SILGenExpr.cpp:375-390)
                    if (self.type_reg.couldBeARC(type_idx)) {
                        const is_owned = if (value_expr) |e| (e == .new_expr or e == .call) else false;

                        if (is_owned) {
                            // +1 value (new or call returning owned): just register cleanup
                            const cleanup = arc.Cleanup.initForLocal(.release, value_node, type_idx, local_idx);
                            _ = try self.cleanup_stack.push(cleanup);
                        } else if (value_expr) |e| {
                            // +0 value (borrowed): retain + register cleanup
                            const needs_retain = blk: {
                                if (e == .ident) {
                                    if (fb.lookupLocal(e.ident.name)) |src_local_idx| {
                                        break :blk self.cleanup_stack.hasCleanupForLocal(src_local_idx);
                                    }
                                }
                                if (e == .field_access) break :blk self.baseHasCleanup(e.field_access.base);
                                if (e == .index) break :blk self.baseHasCleanup(e.index.base);
                                if (e == .deref) break :blk self.baseHasCleanup(e.deref.operand);
                                break :blk false;
                            };
                            if (needs_retain) {
                                var retain_args = [_]ir.NodeIndex{value_node};
                                const retained = try fb.emitCall("cot_retain", &retain_args, false, type_idx, var_stmt.span);
                                _ = try fb.emitStoreLocal(local_idx, retained, var_stmt.span);
                                const cleanup = arc.Cleanup.initForLocal(.release, retained, type_idx, local_idx);
                                _ = try self.cleanup_stack.push(cleanup);
                            }
                        }
                    }
                }
            }
        }
    }

    fn lowerArrayInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const value_node = self.tree.getNode(value_idx) orelse return;
        const value_expr = value_node.asExpr() orelse return;

        switch (value_expr) {
            .array_literal => |al| {
                if (al.elements.len == 0) return;
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                const elem_type = if (local_type == .array) local_type.array.elem else self.inferExprType(al.elements[0]);
                const elem_size = self.type_reg.sizeOf(elem_type);
                for (al.elements, 0..) |elem_idx, i| {
                    const elem_node = try self.lowerExprNode(elem_idx);
                    const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                    _ = try fb.emitStoreIndexLocal(local_idx, idx_node, elem_node, elem_size, span);
                }
            },
            .literal => |lit| {
                if (lit.kind == .undefined_lit) {
                    const local = fb.locals.items[local_idx];
                    const type_size = self.type_reg.sizeOf(local.type_idx);
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                    const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, span);
                    const size_node = try fb.emitConstInt(@intCast(type_size), TypeRegistry.I64, span);
                    var args = [_]ir.NodeIndex{ local_addr, size_node };
                    _ = try fb.emitCall("cot_memset_zero", &args, false, TypeRegistry.VOID, span);
                    return;
                }
                const value_node_ir = try self.lowerExprNode(value_idx);
                _ = try fb.emitStoreLocal(local_idx, value_node_ir, span);
            },
            else => {
                const local = fb.locals.items[local_idx];
                const arr_type = self.type_reg.get(local.type_idx);
                if (arr_type == .array) {
                    const elem_type = arr_type.array.elem;
                    const elem_size = self.type_reg.sizeOf(elem_type);
                    const arr_len = arr_type.array.length;
                    if (value_expr == .ident) {
                        if (fb.lookupLocal(value_expr.ident.name)) |src_local_idx| {
                            for (0..arr_len) |i| {
                                const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                                const src_elem = try fb.emitIndexLocal(src_local_idx, idx_node, elem_size, elem_type, span);
                                _ = try fb.emitStoreIndexLocal(local_idx, idx_node, src_elem, elem_size, span);
                            }
                            return;
                        }
                    }
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
            },
        }
    }

    fn lowerStructInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const value_node = self.tree.getNode(value_idx) orelse return;
        const value_expr = value_node.asExpr() orelse return;
        if (value_expr != .struct_init) return;
        const struct_init = value_expr.struct_init;
        const struct_type_idx = if (struct_init.type_args.len > 0)
            self.resolveGenericTypeName(struct_init.type_name, struct_init.type_args)
        else
            self.type_reg.lookupByName(struct_init.type_name) orelse return;
        if (struct_type_idx == TypeRegistry.VOID) return;
        const type_info = self.type_reg.get(struct_type_idx);
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return;

        for (struct_init.fields) |field_init| {
            for (struct_type.fields, 0..) |struct_field, i| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                    const field_idx: u32 = @intCast(i);
                    const field_offset: i64 = @intCast(struct_field.offset);
                    const value_node_ir = try self.lowerExprNode(field_init.value);
                    const field_type = self.type_reg.get(struct_field.type_idx);
                    const is_string_field = struct_field.type_idx == TypeRegistry.STRING or field_type == .slice;
                    if (is_string_field) {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const ptr_ir = try fb.emitSlicePtr(value_node_ir, ptr_type, span);
                        const len_ir = try fb.emitSliceLen(value_node_ir, span);
                        _ = try fb.emitStoreLocalField(local_idx, field_idx, field_offset, ptr_ir, span);
                        _ = try fb.emitStoreLocalField(local_idx, field_idx + 1, field_offset + 8, len_ir, span);
                    } else {
                        _ = try fb.emitStoreLocalField(local_idx, field_idx, field_offset, value_node_ir, span);
                    }
                    break;
                }
            }
        }
    }

    fn lowerUnionInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, type_idx: TypeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const type_info = self.type_reg.get(type_idx);
        const union_type = if (type_info == .union_type) type_info.union_type else return;
        const value_node_ast = self.tree.getNode(value_idx) orelse return;
        const value_expr = value_node_ast.asExpr() orelse return;

        // Case 1: Payload variant call - Result.Ok(42)
        if (value_expr == .call) {
            const call = value_expr.call;
            const callee_node_ast = self.tree.getNode(call.callee) orelse return;
            const callee_expr = callee_node_ast.asExpr() orelse return;
            if (callee_expr == .field_access) {
                const fa = callee_expr.field_access;
                for (union_type.variants, 0..) |v, i| {
                    if (std.mem.eql(u8, v.name, fa.field)) {
                        // Store tag at offset 0
                        const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                        _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_val, span);
                        // Store payload at offset 8
                        if (call.args.len > 0) {
                            const payload_val = try self.lowerExprNode(call.args[0]);
                            _ = try fb.emitStoreLocalField(local_idx, 1, 8, payload_val, span);
                        }
                        return;
                    }
                }
            }
        }

        // Case 2: Unit variant - State.Running
        if (value_expr == .field_access) {
            const fa = value_expr.field_access;
            for (union_type.variants, 0..) |v, i| {
                if (std.mem.eql(u8, v.name, fa.field)) {
                    const tag_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, tag_val, span);
                    return;
                }
            }
        }

        // Fallback: generic store
        const val = try self.lowerExprNode(value_idx);
        if (val != ir.null_node) _ = try fb.emitStoreLocal(local_idx, val, span);
    }

    fn lowerStringInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const str_node = try self.lowerExprNode(value_idx);
        if (str_node == ir.null_node) return;
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(str_node, ptr_type, span);
        const len_val = try fb.emitSliceLen(str_node, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
    }

    fn lowerSliceInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, slice_type: TypeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const slice_node = try self.lowerExprNode(value_idx);
        if (slice_node == ir.null_node) return;
        // Go slice layout: { array unsafe.Pointer; len int; cap int } = 24 bytes
        // Reference: ~/learning/go/src/runtime/slice.go lines 16-20
        const slice_info = self.type_reg.get(slice_type);
        const elem_type = if (slice_info == .slice) slice_info.slice.elem else TypeRegistry.U8;
        const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(slice_node, ptr_type, span);
        const len_val = try fb.emitSliceLen(slice_node, span);
        const cap_val = try fb.emitSliceCap(slice_node, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);   // ptr at offset 0
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);   // len at offset 8
        _ = try fb.emitStoreLocalField(local_idx, 2, 16, cap_val, span);  // cap at offset 16
    }

    fn lowerAssign(self: *Lowerer, assign: ast.AssignStmt) !void {
        const fb = self.current_func orelse return;
        const target_node = self.tree.getNode(assign.target) orelse return;
        const target_expr = target_node.asExpr() orelse return;

        // Handle compound assignment (+=, -=, etc.)
        // Go reference: walk/assign.go:50-52 - Rewrite x op= y into x = x op y
        const value_node = if (assign.op != .assign) blk: {
            const target_val = try self.lowerExprNode(assign.target);
            const rhs_val = try self.lowerExprNode(assign.value);
            const result_type = self.inferExprType(assign.target);
            break :blk try fb.emitBinary(tokenToBinaryOp(assign.op), target_val, rhs_val, result_type, assign.span);
        } else try self.lowerExprNode(assign.value);

        switch (target_expr) {
            .ident => |id| {
                if (fb.lookupLocal(id.name)) |local_idx| {
                    // Port of Swift's emitSemanticStore(IsNotInitialization):
                    // Release old value before storing new if local has ARC cleanup
                    if (self.cleanup_stack.hasCleanupForLocal(local_idx)) {
                        const local_type = fb.locals.items[local_idx].type_idx;
                        const old_value = try fb.emitLoadLocal(local_idx, local_type, assign.span);
                        var release_args = [_]ir.NodeIndex{old_value};
                        _ = try fb.emitCall("cot_release", &release_args, false, TypeRegistry.VOID, assign.span);
                        // Retain new value (we're taking ownership)
                        var retain_args = [_]ir.NodeIndex{value_node};
                        const retained = try fb.emitCall("cot_retain", &retain_args, false, local_type, assign.span);
                        _ = try fb.emitStoreLocal(local_idx, retained, assign.span);
                        self.cleanup_stack.updateValueForLocal(local_idx, retained);
                    } else {
                        _ = try fb.emitStoreLocal(local_idx, value_node, assign.span);
                    }
                } else if (self.builder.lookupGlobal(id.name)) |g| {
                    _ = try fb.emitGlobalStore(g.idx, id.name, value_node, assign.span);
                }
            },
            .field_access => |fa| try self.lowerFieldAssign(fa, value_node, assign.value, assign.span),
            .index => |idx| try self.lowerIndexAssign(idx, value_node, assign.value, assign.span),
            .deref => |d| {
                const ptr_node = try self.lowerExprNode(d.operand);
                // ARC: release old value at *ptr before storing new
                const ptr_type_idx = self.inferExprType(d.operand);
                const ptr_type_info = self.type_reg.get(ptr_type_idx);
                if (ptr_type_info == .pointer) {
                    const pointee_type = ptr_type_info.pointer.elem;
                    if (self.type_reg.couldBeARC(pointee_type) and self.baseHasCleanup(d.operand)) {
                        const old_val = try fb.emitPtrLoadValue(ptr_node, pointee_type, assign.span);
                        var release_args = [_]ir.NodeIndex{old_val};
                        _ = try fb.emitCall("cot_release", &release_args, false, TypeRegistry.VOID, assign.span);
                    }
                }
                _ = try fb.emitPtrStoreValue(ptr_node, value_node, assign.span);
            },
            else => {},
        }
    }

    fn lowerFieldAssign(self: *Lowerer, fa: ast.FieldAccess, value_node: ir.NodeIndex, rhs_ast: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const base_type_idx = self.inferExprType(fa.base);
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
            if (std.mem.eql(u8, field.name, fa.field)) {
                const field_idx: u32 = @intCast(i);
                const field_offset: i64 = @intCast(field.offset);
                const base_node = self.tree.getNode(fa.base) orelse return;
                const base_expr = base_node.asExpr() orelse return;

                if (base_type == .pointer) {
                    const ptr_val = try self.lowerExprNode(fa.base);

                    // ARC: release old field value before storing new
                    if (self.type_reg.couldBeARC(field.type_idx) and self.baseHasCleanup(fa.base)) {
                        const old_val = try fb.emitFieldValue(ptr_val, field_idx, field_offset, field.type_idx, span);
                        var release_args = [_]ir.NodeIndex{old_val};
                        _ = try fb.emitCall("cot_release", &release_args, false, TypeRegistry.VOID, span);

                        // Retain new value if borrowed (+0)
                        const rhs_node = self.tree.getNode(rhs_ast);
                        const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                        const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                        if (!is_owned) {
                            var retain_args = [_]ir.NodeIndex{value_node};
                            const retained = try fb.emitCall("cot_retain", &retain_args, false, field.type_idx, span);
                            _ = try fb.emitStoreFieldValue(ptr_val, field_idx, field_offset, retained, span);
                        } else {
                            _ = try fb.emitStoreFieldValue(ptr_val, field_idx, field_offset, value_node, span);
                        }
                    } else {
                        _ = try fb.emitStoreFieldValue(ptr_val, field_idx, field_offset, value_node, span);
                    }
                } else if (base_expr == .ident) {
                    if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                        _ = try fb.emitStoreLocalField(local_idx, field_idx, field_offset, value_node, span);
                    } else if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                        const ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.VOID;
                        const global_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, ptr_type, span);
                        _ = try fb.emitStoreFieldValue(global_addr, field_idx, field_offset, value_node, span);
                    }
                }
                break;
            }
        }
    }

    fn lowerIndexAssign(self: *Lowerer, idx: ast.Index, value_node: ir.NodeIndex, rhs_ast: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const base_type_idx = self.inferExprType(idx.base);
        const base_type = self.type_reg.get(base_type_idx);
        const elem_type: TypeIndex = switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return,
        };
        const elem_size = self.type_reg.sizeOf(elem_type);
        const index_node = try self.lowerExprNode(idx.idx);
        const base_node = self.tree.getNode(idx.base) orelse return;
        const base_expr = base_node.asExpr() orelse return;

        // ARC: release old element before storing new
        const arc_elem = self.type_reg.couldBeARC(elem_type) and self.baseHasCleanup(idx.base);
        if (arc_elem) {
            // Load old element and release it
            if (base_expr == .ident) {
                if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                    const old_val = try fb.emitIndexLocal(local_idx, index_node, elem_size, elem_type, span);
                    var release_args = [_]ir.NodeIndex{old_val};
                    _ = try fb.emitCall("cot_release", &release_args, false, TypeRegistry.VOID, span);

                    // Retain new value if borrowed (+0)
                    const rhs_node = self.tree.getNode(rhs_ast);
                    const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                    const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                    const store_val = if (!is_owned) blk: {
                        var retain_args = [_]ir.NodeIndex{value_node};
                        break :blk try fb.emitCall("cot_retain", &retain_args, false, elem_type, span);
                    } else value_node;
                    _ = try fb.emitStoreIndexLocal(local_idx, index_node, store_val, elem_size, span);
                    return;
                }
            }
        }

        if (base_expr == .ident) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
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
                _ = try fb.emitStoreIndexLocal(local_idx, index_node, value_node, elem_size, span);
                return;
            }
            if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, ptr_type, span);
                _ = try fb.emitStoreIndexValue(global_addr, index_node, value_node, elem_size, span);
                return;
            }
        }

        switch (base_type) {
            .slice => {
                const base_val = try self.lowerExprNode(idx.base);
                const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                const ptr_val = try fb.emitSlicePtr(base_val, ptr_type, span);
                _ = try fb.emitStoreIndexValue(ptr_val, index_node, value_node, elem_size, span);
            },
            .pointer => {
                const base_val = try self.lowerExprNode(idx.base);
                _ = try fb.emitStoreIndexValue(base_val, index_node, value_node, elem_size, span);
            },
            .array => {
                if (base_expr == .field_access) {
                    const array_addr = try self.lowerExprNode(idx.base);
                    _ = try fb.emitStoreIndexValue(array_addr, index_node, value_node, elem_size, span);
                }
            },
            else => {},
        }
    }

    fn lowerIf(self: *Lowerer, if_stmt: ast.IfStmt) !void {
        const fb = self.current_func orelse return;
        const then_block = try fb.newBlock("then");
        const else_block = if (if_stmt.else_branch != null_node) try fb.newBlock("else") else null;
        const merge_block = try fb.newBlock("if.end");
        const cond_node = try self.lowerExprNode(if_stmt.condition);
        if (cond_node == ir.null_node) return;
        _ = try fb.emitBranch(cond_node, then_block, else_block orelse merge_block, if_stmt.span);
        fb.setBlock(then_block);
        if (!try self.lowerBlockNode(if_stmt.then_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        if (else_block) |eb| {
            fb.setBlock(eb);
            if (!try self.lowerBlockNode(if_stmt.else_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        }
        fb.setBlock(merge_block);
    }

    fn lowerWhile(self: *Lowerer, while_stmt: ast.WhileStmt) !void {
        const fb = self.current_func orelse return;
        const cond_block = try fb.newBlock("while.cond");
        const body_block = try fb.newBlock("while.body");
        const exit_block = try fb.newBlock("while.end");
        _ = try fb.emitJump(cond_block, while_stmt.span);
        fb.setBlock(cond_block);
        const cond_node = try self.lowerExprNode(while_stmt.condition);
        if (cond_node == ir.null_node) return;
        _ = try fb.emitBranch(cond_node, body_block, exit_block, while_stmt.span);
        try self.loop_stack.append(self.allocator, .{ .cond_block = cond_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth(), .label = while_stmt.label });
        fb.setBlock(body_block);
        if (!try self.lowerBlockNode(while_stmt.body)) _ = try fb.emitJump(cond_block, while_stmt.span);
        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    fn lowerFor(self: *Lowerer, for_stmt: ast.ForStmt) !void {
        const fb = self.current_func orelse return;

        // Handle numeric range: for i in start..end { }
        if (for_stmt.isRange()) {
            try self.lowerForRange(for_stmt);
            return;
        }

        const iter_type = self.inferExprType(for_stmt.iterable);
        const iter_info = self.type_reg.get(iter_type);
        const elem_type: TypeIndex = switch (iter_info) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return,
        };
        const elem_size = self.type_reg.sizeOf(elem_type);

        const idx_name = try std.fmt.allocPrint(self.allocator, "__for_idx_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const len_name = try std.fmt.allocPrint(self.allocator, "__for_len_{d}", .{self.temp_counter});
        self.temp_counter += 1;

        const idx_local = try fb.addLocalWithSize(idx_name, TypeRegistry.I64, true, 8);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(idx_local, zero, for_stmt.span);

        const len_local = try fb.addLocalWithSize(len_name, TypeRegistry.I64, false, 8);
        switch (iter_info) {
            .array => |a| {
                const len_val = try fb.emitConstInt(@intCast(a.length), TypeRegistry.I64, for_stmt.span);
                _ = try fb.emitStoreLocal(len_local, len_val, for_stmt.span);
            },
            .slice => {
                const iter_node = self.tree.getNode(for_stmt.iterable) orelse return;
                const iter_expr = iter_node.asExpr() orelse return;
                if (iter_expr == .ident) {
                    if (fb.lookupLocal(iter_expr.ident.name)) |slice_local| {
                        const slice_val = try fb.emitLoadLocal(slice_local, iter_type, for_stmt.span);
                        const len_val = try fb.emitSliceLen(slice_val, for_stmt.span);
                        _ = try fb.emitStoreLocal(len_local, len_val, for_stmt.span);
                    }
                }
            },
            else => return,
        }

        const cond_block = try fb.newBlock("for.cond");
        const body_block = try fb.newBlock("for.body");
        const incr_block = try fb.newBlock("for.incr");
        const exit_block = try fb.newBlock("for.end");

        _ = try fb.emitJump(cond_block, for_stmt.span);
        fb.setBlock(cond_block);
        const idx_val = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const len_val_cond = try fb.emitLoadLocal(len_local, TypeRegistry.I64, for_stmt.span);
        const cond = try fb.emitBinary(.lt, idx_val, len_val_cond, TypeRegistry.BOOL, for_stmt.span);
        _ = try fb.emitBranch(cond, body_block, exit_block, for_stmt.span);

        try self.loop_stack.append(self.allocator, .{ .cond_block = incr_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth() });

        fb.setBlock(body_block);
        const binding_local = try fb.addLocalWithSize(for_stmt.binding, elem_type, false, elem_size);
        const cur_idx = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);

        // Bind the index variable if present (for i, x in arr)
        if (for_stmt.index_binding) |idx_binding| {
            const idx_binding_local = try fb.addLocalWithSize(idx_binding, TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(idx_binding_local, cur_idx, for_stmt.span);
        }

        const elem_type_info = self.type_reg.get(elem_type);
        const is_struct_elem = elem_type_info == .struct_type;

        switch (iter_info) {
            .array => {
                const iter_node = self.tree.getNode(for_stmt.iterable) orelse return;
                const iter_expr = iter_node.asExpr() orelse return;
                if (iter_expr == .ident) {
                    if (fb.lookupLocal(iter_expr.ident.name)) |arr_local| {
                        if (is_struct_elem) {
                            const u8_ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                            const arr_base = try fb.emitAddrLocal(arr_local, u8_ptr_type, for_stmt.span);
                            const src_addr = try fb.emitAddrIndex(arr_base, cur_idx, elem_size, u8_ptr_type, for_stmt.span);
                            const dst_addr = try fb.emitAddrLocal(binding_local, u8_ptr_type, for_stmt.span);
                            const size_node = try fb.emitConstInt(@intCast(elem_size), TypeRegistry.I64, for_stmt.span);
                            var args = [_]ir.NodeIndex{ dst_addr, src_addr, size_node };
                            _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, for_stmt.span);
                        } else {
                            const elem_val = try fb.emitIndexLocal(arr_local, cur_idx, elem_size, elem_type, for_stmt.span);
                            _ = try fb.emitStoreLocal(binding_local, elem_val, for_stmt.span);
                        }
                    }
                }
            },
            .slice => {
                const iter_node = self.tree.getNode(for_stmt.iterable) orelse return;
                const iter_expr = iter_node.asExpr() orelse return;
                if (iter_expr == .ident) {
                    if (fb.lookupLocal(iter_expr.ident.name)) |slice_local| {
                        const slice_val = try fb.emitLoadLocal(slice_local, iter_type, for_stmt.span);
                        const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                        const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, for_stmt.span);
                        if (is_struct_elem) {
                            const u8_ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                            const src_addr = try fb.emitAddrIndex(ptr_val, cur_idx, elem_size, u8_ptr_type, for_stmt.span);
                            const dst_addr = try fb.emitAddrLocal(binding_local, u8_ptr_type, for_stmt.span);
                            const size_node = try fb.emitConstInt(@intCast(elem_size), TypeRegistry.I64, for_stmt.span);
                            var args = [_]ir.NodeIndex{ dst_addr, src_addr, size_node };
                            _ = try fb.emitCall("memcpy", &args, false, TypeRegistry.VOID, for_stmt.span);
                        } else {
                            const elem_val = try fb.emitIndexValue(ptr_val, cur_idx, elem_size, elem_type, for_stmt.span);
                            _ = try fb.emitStoreLocal(binding_local, elem_val, for_stmt.span);
                        }
                    }
                }
            },
            else => {},
        }

        if (!try self.lowerBlockNode(for_stmt.body)) _ = try fb.emitJump(incr_block, for_stmt.span);

        fb.setBlock(incr_block);
        const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, for_stmt.span);
        const idx_after = try fb.emitBinary(.add, idx_before, one, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(idx_local, idx_after, for_stmt.span);
        _ = try fb.emitJump(cond_block, for_stmt.span);

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Lower numeric range for loop: for i in start..end { }
    /// Reference: Go's range lowering (walk/range.go:162-208)
    fn lowerForRange(self: *Lowerer, for_stmt: ast.ForStmt) !void {
        const fb = self.current_func orelse return;

        // Evaluate range start and end
        const start_val = try self.lowerExprNode(for_stmt.range_start);
        const end_val = try self.lowerExprNode(for_stmt.range_end);

        // Create index variable
        const idx_local = try fb.addLocalWithSize(for_stmt.binding, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(idx_local, start_val, for_stmt.span);

        // Store end value in a local
        const end_name = try std.fmt.allocPrint(self.allocator, "__for_end_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const end_local = try fb.addLocalWithSize(end_name, TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(end_local, end_val, for_stmt.span);

        // Create blocks
        const cond_block = try fb.newBlock("for.cond");
        const body_block = try fb.newBlock("for.body");
        const incr_block = try fb.newBlock("for.incr");
        const exit_block = try fb.newBlock("for.end");

        // Jump to condition
        _ = try fb.emitJump(cond_block, for_stmt.span);

        // Condition: i < end
        fb.setBlock(cond_block);
        const idx_val = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const end_val_cond = try fb.emitLoadLocal(end_local, TypeRegistry.I64, for_stmt.span);
        const cond = try fb.emitBinary(.lt, idx_val, end_val_cond, TypeRegistry.BOOL, for_stmt.span);
        _ = try fb.emitBranch(cond, body_block, exit_block, for_stmt.span);

        // Push loop context for break/continue
        try self.loop_stack.append(self.allocator, .{
            .cond_block = incr_block,
            .exit_block = exit_block,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
        });

        // Body
        fb.setBlock(body_block);
        if (!try self.lowerBlockNode(for_stmt.body)) {
            _ = try fb.emitJump(incr_block, for_stmt.span);
        }

        // Increment: i = i + 1
        fb.setBlock(incr_block);
        const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, for_stmt.span);
        const idx_after = try fb.emitBinary(.add, idx_before, one, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(idx_local, idx_after, for_stmt.span);
        _ = try fb.emitJump(cond_block, for_stmt.span);

        // Exit
        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    fn lowerBreak(self: *Lowerer, target_label: ?[]const u8) !void {
        const fb = self.current_func orelse return;
        if (self.loop_stack.items.len == 0) return;
        const ctx = if (target_label) |label| self.findLabeledLoop(label) orelse self.loop_stack.items[self.loop_stack.items.len - 1] else self.loop_stack.items[self.loop_stack.items.len - 1];
        // Emit ALL cleanups (defers + releases) without popping — Swift's emitBranchAndCleanups
        try self.emitCleanupsNoPop(ctx.cleanup_depth);
        _ = try fb.emitJump(ctx.exit_block, Span.fromPos(Pos.zero));
    }

    fn lowerContinue(self: *Lowerer, target_label: ?[]const u8) !void {
        const fb = self.current_func orelse return;
        if (self.loop_stack.items.len == 0) return;
        const ctx = if (target_label) |label| self.findLabeledLoop(label) orelse self.loop_stack.items[self.loop_stack.items.len - 1] else self.loop_stack.items[self.loop_stack.items.len - 1];
        // Emit ALL cleanups (defers + releases) without popping — Swift's emitBranchAndCleanups
        try self.emitCleanupsNoPop(ctx.cleanup_depth);
        _ = try fb.emitJump(ctx.cond_block, Span.fromPos(Pos.zero));
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

    // ============================================================================
    // Expression Lowering
    // ============================================================================

    fn lowerExprNode(self: *Lowerer, idx: NodeIndex) Error!ir.NodeIndex {
        const node = self.tree.getNode(idx) orelse return ir.null_node;
        const expr = node.asExpr() orelse return ir.null_node;
        return try self.lowerExpr(expr);
    }

    fn lowerExpr(self: *Lowerer, expr: ast.Expr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        switch (expr) {
            .literal => |lit| return try self.lowerLiteral(lit),
            .ident => |id| return try self.lowerIdent(id),
            .binary => |bin| return try self.lowerBinary(bin),
            .unary => |un| return try self.lowerUnary(un),
            .call => |call| return try self.lowerCall(call),
            .paren => |p| return try self.lowerExprNode(p.inner),
            .if_expr => |ie| return try self.lowerIfExpr(ie),
            .addr_of => |addr| return try self.lowerAddrOf(addr),
            .deref => |d| {
                const ptr_node = try self.lowerExprNode(d.operand);
                const ptr_type = self.inferExprType(d.operand);
                const elem_type = self.type_reg.pointerElem(ptr_type);
                return try fb.emitPtrLoadValue(ptr_node, if (elem_type == types.invalid_type) TypeRegistry.VOID else elem_type, d.span);
            },
            .field_access => |fa| return try self.lowerFieldAccess(fa),
            .index => |idx| return try self.lowerIndex(idx),
            .array_literal => |al| return try self.lowerArrayLiteral(al),
            .slice_expr => |se| return try self.lowerSliceExpr(se),
            .try_expr => |te| return try self.lowerTryExpr(te),
            .catch_expr => |ce| return try self.lowerCatchExpr(ce),
            .error_literal => |el| return try self.lowerErrorLiteral(el),
            .builtin_call => |bc| return try self.lowerBuiltinCall(bc),
            .switch_expr => |se| return try self.lowerSwitchExpr(se),
            .struct_init => |si| return try self.lowerStructInitExpr(si),
            .new_expr => |ne| return try self.lowerNewExpr(ne),
            .closure_expr => |ce| return try self.lowerClosureExpr(ce),
            .block_expr => |block| {
                const cleanup_depth = self.cleanup_stack.getScopeDepth();
                const scope_depth = fb.markScopeEntry();
                for (block.stmts) |stmt_idx| {
                    const stmt_node = self.tree.getNode(stmt_idx) orelse continue;
                    if (stmt_node.asStmt()) |s| if (try self.lowerStmt(s)) break;
                }
                try self.emitCleanups(cleanup_depth);
                var result: ir.NodeIndex = ir.null_node;
                if (block.expr != null_node) result = try self.lowerExprNode(block.expr);
                fb.restoreScope(scope_depth);
                return result;
            },
            else => return ir.null_node,
        }
    }

    fn lowerLiteral(self: *Lowerer, lit: ast.Literal) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        switch (lit.kind) {
            .int => return try fb.emitConstInt(std.fmt.parseInt(i64, lit.value, 0) catch 0, TypeRegistry.I64, lit.span),
            .float => return try fb.emitConstFloat(std.fmt.parseFloat(f64, lit.value) catch 0.0, TypeRegistry.F64, lit.span),
            .true_lit => return try fb.emitConstBool(true, lit.span),
            .false_lit => return try fb.emitConstBool(false, lit.span),
            .null_lit, .undefined_lit => return try fb.emitConstNull(TypeRegistry.UNTYPED_NULL, lit.span),
            .string => {
                var buf: [4096]u8 = undefined;
                const unescaped = parseStringLiteral(lit.value, &buf);
                const copied = try self.allocator.dupe(u8, unescaped);
                const str_idx = try fb.addStringLiteral(copied);
                return try fb.emitConstSlice(str_idx, lit.span);
            },
            .char => return try fb.emitConstInt(@intCast(parseCharLiteral(lit.value)), TypeRegistry.U8, lit.span),
        }
    }

    fn lowerIdent(self: *Lowerer, ident: ast.Ident) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (self.const_values.get(ident.name)) |value| return try fb.emitConstInt(value, TypeRegistry.I64, ident.span);
        if (fb.lookupLocal(ident.name)) |local_idx| {
            const local_type = fb.locals.items[local_idx].type_idx;
            if (self.type_reg.isArray(local_type)) return try fb.emitAddrLocal(local_idx, local_type, ident.span);
            return try fb.emitLoadLocal(local_idx, local_type, ident.span);
        }
        if (self.chk.scope.lookup(ident.name)) |sym| {
            if (sym.kind == .function) return try self.createFuncValue(ident.name, sym.type_idx, ident.span);
            if (sym.kind == .constant) if (sym.const_value) |value| return try fb.emitConstInt(value, TypeRegistry.I64, ident.span);
        }
        if (self.builder.lookupGlobal(ident.name)) |g| return try fb.emitGlobalRef(g.idx, ident.name, g.global.type_idx, ident.span);
        return ir.null_node;
    }

    fn lowerBinary(self: *Lowerer, bin: ast.Binary) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const left = try self.lowerExprNode(bin.left);
        if (left == ir.null_node) return ir.null_node;
        const right = try self.lowerExprNode(bin.right);
        if (right == ir.null_node) return ir.null_node;
        const result_type = self.inferBinaryType(bin.op, bin.left, bin.right);

        // String concatenation: emit call to cot_string_concat + string_make
        // Go reference: walk/expr.go:walkAddString transforms OADDSTR to concatstring call
        if (result_type == TypeRegistry.STRING and bin.op == .add) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);

            // Extract ptr1, len1 from left string
            const ptr1 = try fb.emitSlicePtr(left, ptr_type, bin.span);
            const len1 = try fb.emitSliceLen(left, bin.span);

            // Extract ptr2, len2 from right string
            const ptr2 = try fb.emitSlicePtr(right, ptr_type, bin.span);
            const len2 = try fb.emitSliceLen(right, bin.span);

            // Call cot_string_concat(ptr1, len1, ptr2, len2) -> new_ptr
            var args = [_]ir.NodeIndex{ ptr1, len1, ptr2, len2 };
            const new_ptr = try fb.emitCall("cot_string_concat", &args, false, TypeRegistry.I64, bin.span);

            // Compute new_len = len1 + len2
            const new_len = try fb.emitBinary(.add, len1, len2, TypeRegistry.I64, bin.span);

            // Create string_header(new_ptr, new_len) - this creates a string_make in SSA
            return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, bin.span));
        }
        if (bin.op == .coalesce) {
            const left_type_idx = self.inferExprType(bin.left);
            const left_type = self.type_reg.get(left_type_idx);
            if (left_type == .optional) {
                const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, bin.span));
                const condition = try fb.emitBinary(.ne, left, null_val, TypeRegistry.BOOL, bin.span);
                const unwrapped = try fb.emitUnary(.optional_unwrap, left, left_type.optional.elem, bin.span);
                return try fb.emitSelect(condition, unwrapped, right, result_type, bin.span);
            }
        }
        // Pointer arithmetic: p + n scales by element size (like Go's PtrIndex)
        if (bin.op == .add or bin.op == .sub) {
            const left_type_idx = self.inferExprType(bin.left);
            const left_type = self.type_reg.get(left_type_idx);
            if (left_type == .pointer) {
                const elem_size = self.type_reg.sizeOf(left_type.pointer.elem);
                // For add: use addr_index which handles scaling
                // For sub: negate the index first, then use addr_index
                if (bin.op == .add) {
                    return try fb.emitAddrIndex(left, right, elem_size, result_type, bin.span);
                } else {
                    // p - n => addr_index(p, 0-n, elem_size)
                    // Wasm has no integer neg, so use 0 - n
                    const right_type = self.inferExprType(bin.right);
                    const zero = try fb.emitConstInt(0, right_type, bin.span);
                    const neg_right = try fb.emitBinary(.sub, zero, right, right_type, bin.span);
                    return try fb.emitAddrIndex(left, neg_right, elem_size, result_type, bin.span);
                }
            }
        }
        return try fb.emitBinary(tokenToBinaryOp(bin.op), left, right, result_type, bin.span);
    }

    fn lowerUnary(self: *Lowerer, un: ast.Unary) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const operand = try self.lowerExprNode(un.operand);
        if (operand == ir.null_node) return ir.null_node;
        const operand_type_idx = self.inferExprType(un.operand);
        const result_type = if (un.op == .question) blk: {
            const operand_type = self.type_reg.get(operand_type_idx);
            break :blk if (operand_type == .optional) operand_type.optional.elem else operand_type_idx;
        } else operand_type_idx;
        return try fb.emitUnary(tokenToUnaryOp(un.op), operand, result_type, un.span);
    }

    fn lowerAddrOf(self: *Lowerer, addr: ast.AddrOf) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const operand_node = self.tree.getNode(addr.operand) orelse return ir.null_node;
        const operand_expr = operand_node.asExpr() orelse return ir.null_node;

        if (operand_expr == .index) {
            const idx = operand_expr.index;
            const base_type_idx = self.inferExprType(idx.base);
            const base_type = self.type_reg.get(base_type_idx);
            const elem_type: TypeIndex = switch (base_type) {
                .array => |a| a.elem,
                .slice => |s| s.elem,
                else => return ir.null_node,
            };
            const elem_size = self.type_reg.sizeOf(elem_type);
            const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
            const index_node = try self.lowerExprNode(idx.idx);
            const base_node = self.tree.getNode(idx.base) orelse return ir.null_node;
            const base_expr = base_node.asExpr() orelse return ir.null_node;
            if (base_expr == .ident) {
                if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                    const local_type = fb.locals.items[local_idx].type_idx;
                    const base_ptr_type = self.type_reg.makePointer(local_type) catch TypeRegistry.VOID;
                    const base_addr = try fb.emitAddrLocal(local_idx, base_ptr_type, addr.span);
                    return try fb.emitAddrIndex(base_addr, index_node, elem_size, ptr_type, addr.span);
                }
                if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                    const base_ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                    const base_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, base_ptr_type, addr.span);
                    return try fb.emitAddrIndex(base_addr, index_node, elem_size, ptr_type, addr.span);
                }
            }
            const base_val = try self.lowerExprNode(idx.base);
            if (base_type == .slice) {
                const slice_ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
                const ptr_val = try fb.emitSlicePtr(base_val, slice_ptr_type, addr.span);
                return try fb.emitAddrIndex(ptr_val, index_node, elem_size, ptr_type, addr.span);
            }
            return try fb.emitAddrIndex(base_val, index_node, elem_size, ptr_type, addr.span);
        }

        if (operand_expr == .ident) {
            if (fb.lookupLocal(operand_expr.ident.name)) |local_idx| {
                const local_type = fb.locals.items[local_idx].type_idx;
                return try fb.emitAddrLocal(local_idx, self.type_reg.makePointer(local_type) catch TypeRegistry.VOID, addr.span);
            }
            if (self.builder.lookupGlobal(operand_expr.ident.name)) |g| {
                return try fb.emitAddrGlobal(g.idx, operand_expr.ident.name, self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID, addr.span);
            }
        }

        if (operand_expr == .field_access) {
            const fa = operand_expr.field_access;
            const base_node = self.tree.getNode(fa.base) orelse return ir.null_node;
            const base_expr = base_node.asExpr() orelse return ir.null_node;
            if (base_expr == .ident) {
                const base_type_idx = self.inferExprType(fa.base);
                const base_type = self.type_reg.get(base_type_idx);
                if (base_type == .struct_type) {
                    for (base_type.struct_type.fields) |field| {
                        if (std.mem.eql(u8, field.name, fa.field)) {
                            const field_ptr_type = self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID;
                            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                                const struct_ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.VOID;
                                const local_addr = try fb.emitAddrLocal(local_idx, struct_ptr_type, addr.span);
                                return try fb.emitAddrOffset(local_addr, @intCast(field.offset), field_ptr_type, addr.span);
                            }
                            if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                                const struct_ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.VOID;
                                const global_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, struct_ptr_type, addr.span);
                                return try fb.emitAddrOffset(global_addr, @intCast(field.offset), field_ptr_type, addr.span);
                            }
                        }
                    }
                }
                if (base_type == .pointer) {
                    const elem_type = self.type_reg.get(base_type.pointer.elem);
                    if (elem_type == .struct_type) {
                        for (elem_type.struct_type.fields) |field| {
                            if (std.mem.eql(u8, field.name, fa.field)) {
                                if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                                    const ptr_val = try fb.emitLoadLocal(local_idx, base_type_idx, addr.span);
                                    const field_ptr_type = self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID;
                                    return try fb.emitAddrOffset(ptr_val, @intCast(field.offset), field_ptr_type, addr.span);
                                }
                            }
                        }
                    }
                }
            }
        }
        return ir.null_node;
    }

    fn lowerFieldAccess(self: *Lowerer, fa: ast.FieldAccess) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const base_type_idx = self.inferExprType(fa.base);
        const base_type = self.type_reg.get(base_type_idx);

        // Enum variant
        if (base_type == .enum_type) {
            for (base_type.enum_type.variants) |variant| {
                if (std.mem.eql(u8, variant.name, fa.field)) return try fb.emitConstInt(variant.value, base_type_idx, fa.span);
            }
            return ir.null_node;
        }

        // Union: .tag pseudo-field, payload extraction, or variant tag
        if (base_type == .union_type) {
            // Resolve base to local if possible (same pattern as struct field access)
            const base_node_ast = self.tree.getNode(fa.base);
            const base_expr = if (base_node_ast) |n| n.asExpr() else null;
            const base_local: ?ir.LocalIdx = if (base_expr != null and base_expr.? == .ident)
                fb.lookupLocal(base_expr.?.ident.name)
            else
                null;

            // .tag pseudo-field returns the tag value (offset 0)
            if (std.mem.eql(u8, fa.field, "tag")) {
                if (base_local) |lidx| {
                    return try fb.emitFieldLocal(lidx, 0, 0, TypeRegistry.I64, fa.span);
                }
                const base_val = try self.lowerExprNode(fa.base);
                return try fb.emitFieldValue(base_val, 0, 0, TypeRegistry.I64, fa.span);
            }
            for (base_type.union_type.variants, 0..) |variant, i| {
                if (std.mem.eql(u8, variant.name, fa.field)) {
                    if (variant.payload_type != types.invalid_type) {
                        // Payload extraction: load from union at offset 8
                        if (base_local) |lidx| {
                            return try fb.emitFieldLocal(lidx, 1, 8, variant.payload_type, fa.span);
                        }
                        const base_val = try self.lowerExprNode(fa.base);
                        return try fb.emitFieldValue(base_val, 1, 8, variant.payload_type, fa.span);
                    }
                    // Unit variant: return tag index
                    return try fb.emitConstInt(@intCast(i), base_type_idx, fa.span);
                }
            }
            return ir.null_node;
        }

        // Slice ptr/len
        if (base_type == .slice) {
            const base_val = try self.lowerExprNode(fa.base);
            if (std.mem.eql(u8, fa.field, "ptr")) return try fb.emitSlicePtr(base_val, try self.type_reg.add(.{ .pointer = .{ .elem = base_type.slice.elem } }), fa.span);
            if (std.mem.eql(u8, fa.field, "len")) return try fb.emitSliceLen(base_val, fa.span);
            return ir.null_node;
        }

        // String ptr/len
        if (base_type_idx == TypeRegistry.STRING) {
            const base_val = try self.lowerExprNode(fa.base);
            if (std.mem.eql(u8, fa.field, "ptr")) return try fb.emitSlicePtr(base_val, try self.type_reg.add(.{ .pointer = .{ .elem = TypeRegistry.U8 } }), fa.span);
            if (std.mem.eql(u8, fa.field, "len")) return try fb.emitSliceLen(base_val, fa.span);
            return ir.null_node;
        }

        // Struct field
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
            if (std.mem.eql(u8, field.name, fa.field)) {
                field_idx = @intCast(i);
                field_offset = @intCast(field.offset);
                field_type = field.type_idx;
                break;
            }
        }

        const base_is_pointer = base_type == .pointer;
        const base_node = self.tree.getNode(fa.base) orelse return ir.null_node;
        const base_expr = base_node.asExpr() orelse return ir.null_node;

        if (base_expr == .ident and !base_is_pointer) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| return try fb.emitFieldLocal(local_idx, field_idx, field_offset, field_type, fa.span);
            if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, ptr_type, fa.span);
                return try fb.emitFieldValue(global_addr, field_idx, field_offset, field_type, fa.span);
            }
        }

        if (base_expr == .deref) {
            const ptr_val = try self.lowerExprNode(base_expr.deref.operand);
            return try fb.emitFieldValue(ptr_val, field_idx, field_offset, field_type, fa.span);
        }

        if (base_expr == .index) {
            const idx = base_expr.index;
            const idx_base_type_idx = self.inferExprType(idx.base);
            const idx_base_type = self.type_reg.get(idx_base_type_idx);
            const elem_type: TypeIndex = switch (idx_base_type) {
                .array => |a| a.elem,
                else => {
                    const base_val = try self.lowerExprNode(fa.base);
                    return try fb.emitFieldValue(base_val, field_idx, field_offset, field_type, fa.span);
                },
            };
            const elem_size = self.type_reg.sizeOf(elem_type);
            const index_node = try self.lowerExprNode(idx.idx);
            const idx_base_node = self.tree.getNode(idx.base) orelse return ir.null_node;
            const idx_base_expr = idx_base_node.asExpr() orelse return ir.null_node;
            if (idx_base_expr == .ident) {
                if (fb.lookupLocal(idx_base_expr.ident.name)) |local_idx| {
                    const array_ptr_type = self.type_reg.makePointer(fb.locals.items[local_idx].type_idx) catch TypeRegistry.VOID;
                    const array_addr = try fb.emitAddrLocal(local_idx, array_ptr_type, fa.span);
                    const elem_ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
                    const elem_addr = try fb.emitAddrIndex(array_addr, index_node, elem_size, elem_ptr_type, fa.span);
                    return try fb.emitFieldValue(elem_addr, field_idx, field_offset, field_type, fa.span);
                }
                if (self.builder.lookupGlobal(idx_base_expr.ident.name)) |g| {
                    const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                    const global_addr = try fb.emitAddrGlobal(g.idx, idx_base_expr.ident.name, ptr_type, fa.span);
                    const elem_ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
                    const elem_addr = try fb.emitAddrIndex(global_addr, index_node, elem_size, elem_ptr_type, fa.span);
                    return try fb.emitFieldValue(elem_addr, field_idx, field_offset, field_type, fa.span);
                }
            }
            const base_val = try self.lowerExprNode(idx.base);
            const elem_ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
            const elem_addr = try fb.emitAddrIndex(base_val, index_node, elem_size, elem_ptr_type, fa.span);
            return try fb.emitFieldValue(elem_addr, field_idx, field_offset, field_type, fa.span);
        }

        const base_val = try self.lowerExprNode(fa.base);
        return try fb.emitFieldValue(base_val, field_idx, field_offset, field_type, fa.span);
    }

    fn lowerIndex(self: *Lowerer, idx: ast.Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const base_type_idx = self.inferExprType(idx.base);
        const base_type = self.type_reg.get(base_type_idx);
        const elem_type: TypeIndex = if (base_type_idx == TypeRegistry.STRING) TypeRegistry.U8 else switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type);

        if (base_type_idx == TypeRegistry.STRING) {
            const index_node = try self.lowerExprNode(idx.idx);
            const base_node = self.tree.getNode(idx.base) orelse return ir.null_node;
            const base_expr = base_node.asExpr() orelse return ir.null_node;
            if (base_expr == .ident) {
                if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                    const str_val = try fb.emitLoadLocal(local_idx, TypeRegistry.STRING, idx.span);
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, idx.span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
                }
            }
            const base_val = try self.lowerExprNode(idx.base);
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
            const ptr_val = try fb.emitSlicePtr(base_val, ptr_type, idx.span);
            return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
        }

        const index_node = try self.lowerExprNode(idx.idx);
        const base_node = self.tree.getNode(idx.base) orelse return ir.null_node;
        const base_expr = base_node.asExpr() orelse return ir.null_node;

        if (base_expr == .ident) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                if (local_type == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, idx.span);
                    const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, idx.span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
                }
                if (local.is_param and self.type_reg.isArray(local.type_idx)) {
                    const ptr_val = try fb.emitLoadLocal(local_idx, local.type_idx, idx.span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
                }
                return try fb.emitIndexLocal(local_idx, index_node, elem_size, elem_type, idx.span);
            }
            if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                const global_addr = try fb.emitAddrGlobal(g.idx, base_expr.ident.name, ptr_type, idx.span);
                return try fb.emitIndexValue(global_addr, index_node, elem_size, elem_type, idx.span);
            }
        }

        const base_val = try self.lowerExprNode(idx.base);
        return try fb.emitIndexValue(base_val, index_node, elem_size, elem_type, idx.span);
    }

    fn lowerArrayLiteral(self: *Lowerer, al: ast.ArrayLiteral) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (al.elements.len == 0) return ir.null_node;
        const first_elem_type = self.inferExprType(al.elements[0]);
        const elem_size = self.type_reg.sizeOf(first_elem_type);
        const array_type = self.type_reg.makeArray(first_elem_type, al.elements.len) catch return ir.null_node;
        const array_size = self.type_reg.sizeOf(array_type);
        const temp_name = try std.fmt.allocPrint(self.allocator, "__arr_{d}", .{fb.locals.items.len});
        const local_idx = try fb.addLocalWithSize(temp_name, array_type, false, array_size);
        for (al.elements, 0..) |elem_idx, i| {
            const elem_node = try self.lowerExprNode(elem_idx);
            const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, al.span);
            _ = try fb.emitStoreIndexLocal(local_idx, idx_node, elem_node, elem_size, al.span);
        }
        return try fb.emitAddrLocal(local_idx, array_type, al.span);
    }

    fn lowerSliceExpr(self: *Lowerer, se: ast.SliceExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const base_type_idx = self.inferExprType(se.base);
        const base_type = self.type_reg.get(base_type_idx);
        const elem_type_idx: TypeIndex = switch (base_type) {
            .array => |a| a.elem,
            .slice => |s| s.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type_idx);
        const slice_type = self.type_reg.makeSlice(elem_type_idx) catch return ir.null_node;

        var start_node: ?ir.NodeIndex = null;
        if (se.start != null_node) start_node = try self.lowerExprNode(se.start);

        var end_node: ?ir.NodeIndex = null;
        if (se.end != null_node) {
            end_node = try self.lowerExprNode(se.end);
        } else {
            switch (base_type) {
                .array => |a| end_node = try fb.emitConstInt(@intCast(a.length), TypeRegistry.I64, se.span),
                .slice => {
                    const base_node = self.tree.getNode(se.base) orelse return ir.null_node;
                    const base_expr = base_node.asExpr() orelse return ir.null_node;
                    if (base_expr == .ident) {
                        if (fb.lookupLocal(base_expr.ident.name)) |slice_local| {
                            const slice_val = try fb.emitLoadLocal(slice_local, base_type_idx, se.span);
                            end_node = try fb.emitSliceLen(slice_val, se.span);
                        }
                    }
                },
                else => {},
            }
        }

        const base_node = self.tree.getNode(se.base) orelse return ir.null_node;
        const base_expr = base_node.asExpr() orelse return ir.null_node;

        if (base_expr == .ident) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                if (local_type == .array) {
                    const ptr_type = self.type_reg.makePointer(elem_type_idx) catch TypeRegistry.I64;
                    const base_addr = try fb.emitAddrLocal(local_idx, ptr_type, se.span);
                    return try fb.emitMakeSlice(base_addr, start_node, end_node orelse return ir.null_node, elem_size, slice_type, se.span);
                }
                if (local_type == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, se.span);
                    const ptr_type = self.type_reg.makePointer(elem_type_idx) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(slice_val, ptr_type, se.span);
                    return try fb.emitMakeSlice(ptr_val, start_node, end_node orelse return ir.null_node, elem_size, slice_type, se.span);
                }
            }
        }

        const base_val = try self.lowerExprNode(se.base);
        return try fb.emitMakeSlice(base_val, start_node, end_node orelse return ir.null_node, elem_size, slice_type, se.span);
    }

    fn lowerStructInitExpr(self: *Lowerer, si: ast.StructInit) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const struct_type_idx = if (si.type_args.len > 0)
            self.resolveGenericTypeName(si.type_name, si.type_args)
        else
            self.type_reg.lookupByName(si.type_name) orelse return ir.null_node;
        if (struct_type_idx == TypeRegistry.VOID) return ir.null_node;
        const type_info = self.type_reg.get(struct_type_idx);
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return ir.null_node;
        const size = self.type_reg.sizeOf(struct_type_idx);
        const temp_idx = try fb.addLocalWithSize("__struct_tmp", struct_type_idx, true, size);

        for (si.fields) |field_init| {
            for (struct_type.fields, 0..) |struct_field, i| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                    const field_idx: u32 = @intCast(i);
                    const field_offset: i64 = @intCast(struct_field.offset);
                    const value_node = try self.lowerExprNode(field_init.value);
                    const field_type = self.type_reg.get(struct_field.type_idx);
                    const is_string_field = struct_field.type_idx == TypeRegistry.STRING or field_type == .slice;
                    if (is_string_field) {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const ptr_ir = try fb.emitSlicePtr(value_node, ptr_type, si.span);
                        const len_ir = try fb.emitSliceLen(value_node, si.span);
                        _ = try fb.emitStoreLocalField(temp_idx, field_idx, field_offset, ptr_ir, si.span);
                        _ = try fb.emitStoreLocalField(temp_idx, field_idx + 1, field_offset + 8, len_ir, si.span);
                    } else {
                        _ = try fb.emitStoreLocalField(temp_idx, field_idx, field_offset, value_node, si.span);
                    }
                    break;
                }
            }
        }
        return try fb.emitLoadLocal(temp_idx, struct_type_idx, si.span);
    }

    /// Lower heap allocation expression: new Type { field: value, ... }
    /// Emits cot_alloc call and initializes fields.
    /// Reference: Go's walkNew (walk/builtin.go:601-616)
    fn lowerNewExpr(self: *Lowerer, ne: ast.NewExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Lookup the struct type (generic or non-generic)
        const struct_type_idx = if (ne.type_args.len > 0)
            self.resolveGenericTypeName(ne.type_name, ne.type_args)
        else
            self.type_reg.lookupByName(ne.type_name) orelse return ir.null_node;
        if (struct_type_idx == TypeRegistry.VOID) return ir.null_node;
        const type_info = self.type_reg.get(struct_type_idx);
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return ir.null_node;

        // Calculate size including heap object header (16 bytes for 32-bit Wasm)
        // Header: total_size (4) + metadata ptr (4) + refcount (8) = 16 bytes
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size = self.type_reg.sizeOf(struct_type_idx);
        const total_size = HEAP_HEADER_SIZE + payload_size;

        // Call cot_alloc(metadata_ptr, size) -> ptr to user data (after header)
        // Reference: Swift's swift_allocObject(metadata, size, align)
        // Pass type metadata - resolved to actual address during Wasm codegen
        // For generic types, use the concrete type name from the struct type
        const metadata_name = if (ne.type_args.len > 0) struct_type.name else ne.type_name;
        const metadata_node = try fb.emitTypeMetadata(metadata_name, ne.span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, ne.span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const ptr_type = try self.type_reg.makePointer(struct_type_idx);
        const alloc_result = try fb.emitCall("cot_alloc", &alloc_args, false, ptr_type, ne.span);

        // Store pointer in a temp so we can access it multiple times
        const temp_idx = try fb.addLocalWithSize("__new_ptr", ptr_type, true, 8);
        _ = try fb.emitStoreLocal(temp_idx, alloc_result, ne.span);
        const ptr_node = try fb.emitLoadLocal(temp_idx, ptr_type, ne.span);

        // Initialize fields using ptr stores
        for (ne.fields) |field_init| {
            for (struct_type.fields, 0..) |struct_field, i| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                    const field_offset: i64 = @intCast(struct_field.offset);
                    const value_node = try self.lowerExprNode(field_init.value);
                    const field_type = self.type_reg.get(struct_field.type_idx);
                    const is_string_field = struct_field.type_idx == TypeRegistry.STRING or field_type == .slice;

                    if (is_string_field) {
                        // String/slice fields: store ptr and len
                        const u8_ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const ptr_ir = try fb.emitSlicePtr(value_node, u8_ptr_type, ne.span);
                        const len_ir = try fb.emitSliceLen(value_node, ne.span);
                        // Store ptr at offset
                        const ptr_addr = try fb.emitAddrOffset(ptr_node, field_offset, ptr_type, ne.span);
                        _ = try fb.emitPtrStoreValue(ptr_addr, ptr_ir, ne.span);
                        // Store len at offset+8
                        const len_addr = try fb.emitAddrOffset(ptr_node, field_offset + 8, ptr_type, ne.span);
                        _ = try fb.emitPtrStoreValue(len_addr, len_ir, ne.span);
                    } else {
                        // Simple field: store value at offset
                        const field_addr = try fb.emitAddrOffset(ptr_node, field_offset, ptr_type, ne.span);
                        _ = try fb.emitPtrStoreValue(field_addr, value_node, ne.span);
                    }
                    _ = i;
                    break;
                }
            }
        }

        // Return the pointer to the allocated object.
        // NOTE: Cleanup is NOT registered here - it's registered in lowerLocalVarDecl
        // when the result is stored to a local variable. This allows the cleanup to
        // be associated with the local for proper ownership tracking on return.
        return try fb.emitLoadLocal(temp_idx, ptr_type, ne.span);
    }

    /// Create a 1-word closure struct for a named function used as a value.
    /// Layout: { table_idx: i64 }
    fn createFuncValue(self: *Lowerer, func_name: []const u8, type_idx: TypeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size: u32 = 8; // one i64 for table_idx
        const total_size = HEAP_HEADER_SIZE + payload_size;

        // Allocate closure struct: cot_alloc(0, total_size)
        const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const alloc_result = try fb.emitCall("cot_alloc", &alloc_args, false, TypeRegistry.I64, span);

        // Store result in temp
        const temp_name = try std.fmt.allocPrint(self.allocator, "__closure_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_idx = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(temp_idx, alloc_result, span);
        const ptr_node = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);

        // Store table_idx at offset 0
        const table_idx_node = try fb.emitFuncAddr(func_name, type_idx, span);
        _ = try fb.emitPtrStoreValue(ptr_node, table_idx_node, span);

        return try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, span);
    }

    /// Lower a closure expression: detect captures, create body function, allocate struct.
    fn lowerClosureExpr(self: *Lowerer, ce: ast.ClosureExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Detect captured variables from parent scope
        var captures = std.ArrayListUnmanaged(CaptureInfo){};
        defer captures.deinit(self.allocator);
        try self.detectCaptures(ce.body, fb, &captures);

        // Generate a unique name for the closure body function
        const closure_name = try std.fmt.allocPrint(self.allocator, "__closure_{d}", .{self.closure_counter});
        self.closure_counter += 1;

        // Save parent function builder state (by value)
        const saved_builder_func = self.builder.current_func;

        // Create the closure body function
        const return_type = if (ce.return_type != null_node) self.resolveTypeNode(ce.return_type) else TypeRegistry.VOID;
        self.builder.startFunc(closure_name, TypeRegistry.VOID, return_type, ce.span);
        if (self.builder.func()) |closure_fb| {
            self.current_func = closure_fb;
            self.cleanup_stack.clear();

            // Add declared params
            for (ce.params) |param| {
                const param_type = self.resolveTypeNode(param.type_expr);
                _ = try closure_fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }

            // At function start: read captures from CTXT global
            // CTXT global is at Wasm index 1 (SP=0, CTXT=1)
            if (captures.items.len > 0) {
                const ctxt_ptr = try closure_fb.emitWasmGlobalRead(1, TypeRegistry.I64, ce.span);
                for (captures.items, 0..) |cap, i| {
                    // Load capture from offset (i+1)*8 (offset 0 = table_idx)
                    const offset: i64 = @intCast((i + 1) * 8);
                    const cap_addr = try closure_fb.emitAddrOffset(ctxt_ptr, offset, TypeRegistry.I64, ce.span);
                    const cap_val = try closure_fb.emitPtrLoadValue(cap_addr, cap.type_idx, ce.span);
                    // Store in local variable with the capture's name
                    const local_idx = try closure_fb.addLocalWithSize(cap.name, cap.type_idx, true, 8);
                    _ = try closure_fb.emitStoreLocal(local_idx, cap_val, ce.span);
                }
            }

            // Lower body
            if (ce.body != null_node) {
                _ = try self.lowerBlockNode(ce.body);
                if (return_type == TypeRegistry.VOID and closure_fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try closure_fb.emitRet(null, ce.span);
                }
            }

            self.cleanup_stack.clear();
        }
        // endFunc builds and appends the closure, sets builder.current_func = null
        try self.builder.endFunc();
        // Restore parent function builder
        self.builder.current_func = saved_builder_func;
        self.current_func = if (self.builder.current_func) |*bfb| bfb else null;

        // Now in parent function: allocate closure struct and fill it
        const num_captures = captures.items.len;
        const HEAP_HEADER_SIZE: u32 = 16;
        const payload_size: u32 = @intCast((1 + num_captures) * 8); // table_idx + captures
        const total_size = HEAP_HEADER_SIZE + payload_size;

        // Allocate: cot_alloc(0, total_size) - metadata_ptr=0 (no destructor)
        const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, ce.span);
        const size_node = try fb.emitConstInt(@intCast(total_size), TypeRegistry.I64, ce.span);
        var alloc_args = [_]ir.NodeIndex{ metadata_node, size_node };
        const alloc_result = try fb.emitCall("cot_alloc", &alloc_args, false, TypeRegistry.I64, ce.span);

        // Store in temp
        const temp_name = try std.fmt.allocPrint(self.allocator, "__closure_ptr_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const temp_idx = try fb.addLocalWithSize(temp_name, TypeRegistry.I64, true, 8);
        _ = try fb.emitStoreLocal(temp_idx, alloc_result, ce.span);
        const ptr_node = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, ce.span);

        // Store table_idx at offset 0
        // Build function type from closure params
        const func_type = blk: {
            var func_params = std.ArrayListUnmanaged(types.FuncParam){};
            defer func_params.deinit(self.allocator);
            for (ce.params) |param| {
                const param_type = self.resolveTypeNode(param.type_expr);
                try func_params.append(self.allocator, .{ .name = param.name, .type_idx = param_type });
            }
            const ret_type = if (ce.return_type != null_node) self.resolveTypeNode(ce.return_type) else TypeRegistry.VOID;
            break :blk try self.type_reg.add(.{ .func = .{ .params = try self.allocator.dupe(types.FuncParam, func_params.items), .return_type = ret_type } });
        };
        const table_idx_node = try fb.emitFuncAddr(closure_name, func_type, ce.span);
        _ = try fb.emitPtrStoreValue(ptr_node, table_idx_node, ce.span);

        // Store each capture at offset 8, 16, ...
        for (captures.items, 0..) |cap, i| {
            const offset: i64 = @intCast((i + 1) * 8);
            const reload_ptr = try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, ce.span);
            const cap_addr = try fb.emitAddrOffset(reload_ptr, offset, TypeRegistry.I64, ce.span);
            const cap_val = try fb.emitLoadLocal(cap.local_idx, cap.type_idx, ce.span);
            _ = try fb.emitPtrStoreValue(cap_addr, cap_val, ce.span);
        }

        return try fb.emitLoadLocal(temp_idx, TypeRegistry.I64, ce.span);
    }

    const CaptureInfo = struct {
        name: []const u8,
        local_idx: ir.LocalIdx,
        type_idx: TypeIndex,
    };

    /// Walk AST to detect captured variables (idents that resolve to parent's locals).
    fn detectCaptures(self: *Lowerer, body_idx: NodeIndex, parent_fb: *ir.FuncBuilder, captures: *std.ArrayListUnmanaged(CaptureInfo)) Error!void {
        const node = self.tree.getNode(body_idx) orelse return;
        switch (node) {
            .expr => |expr| switch (expr) {
                .ident => |id| {
                    // Check if this ident is a local in the parent function
                    if (parent_fb.lookupLocal(id.name)) |local_idx| {
                        // Don't add duplicates
                        for (captures.items) |cap| {
                            if (std.mem.eql(u8, cap.name, id.name)) return;
                        }
                        try captures.append(self.allocator, .{
                            .name = id.name,
                            .local_idx = local_idx,
                            .type_idx = parent_fb.locals.items[local_idx].type_idx,
                        });
                    }
                },
                .binary => |bin| {
                    try self.detectCaptures(bin.left, parent_fb, captures);
                    try self.detectCaptures(bin.right, parent_fb, captures);
                },
                .unary => |un| try self.detectCaptures(un.operand, parent_fb, captures),
                .call => |c| {
                    try self.detectCaptures(c.callee, parent_fb, captures);
                    for (c.args) |arg| try self.detectCaptures(arg, parent_fb, captures);
                },
                .paren => |p| try self.detectCaptures(p.inner, parent_fb, captures),
                .if_expr => |ie| {
                    try self.detectCaptures(ie.condition, parent_fb, captures);
                    try self.detectCaptures(ie.then_branch, parent_fb, captures);
                    if (ie.else_branch != null_node) try self.detectCaptures(ie.else_branch, parent_fb, captures);
                },
                .index => |idx| {
                    try self.detectCaptures(idx.base, parent_fb, captures);
                    try self.detectCaptures(idx.idx, parent_fb, captures);
                },
                .field_access => |fa| {
                    if (fa.base != null_node) try self.detectCaptures(fa.base, parent_fb, captures);
                },
                .block_expr => |b| {
                    for (b.stmts) |s| try self.detectCaptures(s, parent_fb, captures);
                    if (b.expr != null_node) try self.detectCaptures(b.expr, parent_fb, captures);
                },
                else => {},
            },
            .stmt => |stmt| switch (stmt) {
                .expr_stmt => |es| try self.detectCaptures(es.expr, parent_fb, captures),
                .return_stmt => |rs| {
                    if (rs.value != null_node) try self.detectCaptures(rs.value, parent_fb, captures);
                },
                .var_stmt => |vs| {
                    if (vs.value != null_node) try self.detectCaptures(vs.value, parent_fb, captures);
                },
                .assign_stmt => |as_stmt| {
                    try self.detectCaptures(as_stmt.target, parent_fb, captures);
                    try self.detectCaptures(as_stmt.value, parent_fb, captures);
                },
                .if_stmt => |is| {
                    try self.detectCaptures(is.condition, parent_fb, captures);
                    try self.detectCaptures(is.then_branch, parent_fb, captures);
                    if (is.else_branch != null_node) try self.detectCaptures(is.else_branch, parent_fb, captures);
                },
                .while_stmt => |ws| {
                    try self.detectCaptures(ws.condition, parent_fb, captures);
                    try self.detectCaptures(ws.body, parent_fb, captures);
                },
                .for_stmt => |fs| {
                    if (fs.iterable != null_node) try self.detectCaptures(fs.iterable, parent_fb, captures);
                    try self.detectCaptures(fs.body, parent_fb, captures);
                },
                .block_stmt => |bs| {
                    for (bs.stmts) |s| try self.detectCaptures(s, parent_fb, captures);
                },
                .defer_stmt => |ds| try self.detectCaptures(ds.expr, parent_fb, captures),
                .break_stmt, .continue_stmt, .bad_stmt => {},
            },
            .decl => {},
        }
    }

    fn lowerIfExpr(self: *Lowerer, if_expr: ast.IfExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const cond = try self.lowerExprNode(if_expr.condition);
        if (cond == ir.null_node) return ir.null_node;
        const then_val = try self.lowerExprNode(if_expr.then_branch);
        if (then_val == ir.null_node) return ir.null_node;
        const else_val = if (if_expr.else_branch != null_node) try self.lowerExprNode(if_expr.else_branch) else ir.null_node;
        return try fb.emitSelect(cond, then_val, else_val, self.inferExprType(if_expr.then_branch), if_expr.span);
    }

    fn lowerSwitchExpr(self: *Lowerer, se: ast.SwitchExpr) Error!ir.NodeIndex {
        const result_type = if (se.cases.len > 0) self.inferExprType(se.cases[0].body) else if (se.else_body != null_node) self.inferExprType(se.else_body) else TypeRegistry.VOID;
        // Union switches with captures always use statement form (need block scoping for captures)
        const subject_type = self.inferExprType(se.subject);
        const is_union = self.type_reg.get(subject_type) == .union_type;
        if (result_type == TypeRegistry.VOID or (is_union and self.hasCapture(se))) return try self.lowerSwitchStatement(se);
        return try self.lowerSwitchAsSelect(se, result_type);
    }

    fn hasCapture(self: *Lowerer, se: ast.SwitchExpr) bool {
        _ = self;
        for (se.cases) |case| {
            if (case.capture.len > 0) return true;
        }
        return false;
    }

    fn lowerSwitchStatement(self: *Lowerer, se: ast.SwitchExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const subject_type = self.inferExprType(se.subject);
        const subject_info = self.type_reg.get(subject_type);
        const is_union = subject_info == .union_type;

        if (is_union) return try self.lowerUnionSwitch(se, subject_type, subject_info.union_type);

        const subject = try self.lowerExprNode(se.subject);
        const merge_block = try fb.newBlock("switch.end");

        var i: usize = 0;
        while (i < se.cases.len) : (i += 1) {
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;
            for (case.patterns) |pattern_idx| {
                const pattern_val = try self.lowerExprNode(pattern_idx);
                const pattern_cond = try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, se.span);
                case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
            }
            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < se.cases.len) try fb.newBlock("switch.next") else if (se.else_body != null_node) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, se.span);
            fb.setBlock(case_block);
            if (!try self.lowerBlockNode(case.body)) _ = try fb.emitJump(merge_block, se.span);
            fb.setBlock(next_block);
        }

        if (se.else_body != null_node) {
            if (!try self.lowerBlockNode(se.else_body)) _ = try fb.emitJump(merge_block, se.span);
        }
        fb.setBlock(merge_block);
        return ir.null_node;
    }

    fn lowerUnionSwitch(self: *Lowerer, se: ast.SwitchExpr, subject_type: TypeIndex, ut: types.UnionType) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Store subject into a local so we can extract tag/payload via field offsets
        const subject_local = blk: {
            // Check if subject is already a local (ident)
            const subj_node = self.tree.getNode(se.subject) orelse break :blk null;
            const subj_expr = subj_node.asExpr() orelse break :blk null;
            if (subj_expr == .ident) {
                break :blk fb.lookupLocal(subj_expr.ident.name);
            }
            break :blk null;
        } orelse blk: {
            // Create temp local for the subject
            const size = self.type_reg.sizeOf(subject_type);
            const tmp_name = "__switch_subj";
            const tmp_local = try fb.addLocalWithSize(tmp_name, subject_type, false, size);
            const subj_val = try self.lowerExprNode(se.subject);
            _ = try fb.emitStoreLocal(tmp_local, subj_val, se.span);
            break :blk tmp_local;
        };

        // Extract tag from union (offset 0)
        const tag_val = try fb.emitFieldLocal(subject_local, 0, 0, TypeRegistry.I64, se.span);
        const merge_block = try fb.newBlock("switch.end");

        var i: usize = 0;
        while (i < se.cases.len) : (i += 1) {
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;

            for (case.patterns) |pattern_idx| {
                // Resolve pattern to variant index
                const variant_idx = self.resolveUnionVariantIndex(pattern_idx, ut);
                if (variant_idx) |vidx| {
                    const idx_val = try fb.emitConstInt(@intCast(vidx), TypeRegistry.I64, se.span);
                    const pattern_cond = try fb.emitBinary(.eq, tag_val, idx_val, TypeRegistry.BOOL, se.span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
                }
            }

            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < se.cases.len) try fb.newBlock("switch.next") else if (se.else_body != null_node) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, se.span);
            fb.setBlock(case_block);

            // If capture, create a scoped local for the payload
            if (case.capture.len > 0) {
                const scope_depth = fb.markScopeEntry();
                const payload_type = self.resolveUnionPayloadType(case.patterns, ut);
                const payload_size = self.type_reg.sizeOf(payload_type);
                const capture_local = try fb.addLocalWithSize(case.capture, payload_type, false, payload_size);
                const payload_val = try fb.emitFieldLocal(subject_local, 1, 8, payload_type, se.span);
                _ = try fb.emitStoreLocal(capture_local, payload_val, se.span);
                if (!try self.lowerBlockNode(case.body)) _ = try fb.emitJump(merge_block, se.span);
                fb.restoreScope(scope_depth);
            } else {
                if (!try self.lowerBlockNode(case.body)) _ = try fb.emitJump(merge_block, se.span);
            }

            fb.setBlock(next_block);
        }

        if (se.else_body != null_node) {
            if (!try self.lowerBlockNode(se.else_body)) _ = try fb.emitJump(merge_block, se.span);
        }
        fb.setBlock(merge_block);
        return ir.null_node;
    }

    /// Resolve a switch case pattern expression to a union variant index.
    fn resolveUnionVariantIndex(self: *Lowerer, pattern_idx: NodeIndex, ut: types.UnionType) ?usize {
        const node = self.tree.getNode(pattern_idx) orelse return null;
        const expr = node.asExpr() orelse return null;
        const field_name = switch (expr) {
            .field_access => |fa| fa.field,
            else => return null,
        };
        for (ut.variants, 0..) |v, i| {
            if (std.mem.eql(u8, v.name, field_name)) return i;
        }
        return null;
    }

    /// Resolve the payload type from a pattern's first variant.
    fn resolveUnionPayloadType(self: *Lowerer, patterns: []const ast.NodeIndex, ut: types.UnionType) TypeIndex {
        if (patterns.len == 0) return TypeRegistry.VOID;
        if (self.resolveUnionVariantIndex(patterns[0], ut)) |vidx| {
            return ut.variants[vidx].payload_type;
        }
        return TypeRegistry.VOID;
    }

    fn lowerSwitchAsSelect(self: *Lowerer, se: ast.SwitchExpr, result_type: TypeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const subject = try self.lowerExprNode(se.subject);
        var result = if (se.else_body != null_node) try self.lowerExprNode(se.else_body) else try fb.emitConstNull(result_type, se.span);
        var i: usize = se.cases.len;
        while (i > 0) {
            i -= 1;
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;
            for (case.patterns) |pattern_idx| {
                const pattern_val = try self.lowerExprNode(pattern_idx);
                const pattern_cond = try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, se.span);
                case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
            }
            const case_val = try self.lowerExprNode(case.body);
            result = try fb.emitSelect(case_cond, case_val, result, result_type, se.span);
        }
        return result;
    }

    fn lowerCall(self: *Lowerer, call: ast.Call) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const callee_node = self.tree.getNode(call.callee) orelse return ir.null_node;
        const callee_expr = callee_node.asExpr() orelse return ir.null_node;

        // Method call (Type.method or value.method)
        if (callee_expr == .field_access) {
            const fa = callee_expr.field_access;
            if (fa.base != null_node) {
                const base_type_idx = self.inferExprType(fa.base);
                const base_type = self.type_reg.get(base_type_idx);
                const type_name: ?[]const u8 = switch (base_type) {
                    .struct_type => |s| s.name,
                    .pointer => |p| blk: {
                        const elem = self.type_reg.get(p.elem);
                        if (elem == .struct_type) break :blk elem.struct_type.name;
                        break :blk null;
                    },
                    else => null,
                };
                if (type_name) |name| {
                    if (self.chk.lookupMethod(name, fa.field)) |method_info| {
                        return try self.lowerMethodCall(call, fa, method_info);
                    }
                }

                // Union variant constructor: Result.Ok(42)
                if (base_type == .union_type) {
                    for (base_type.union_type.variants, 0..) |v, i| {
                        if (std.mem.eql(u8, v.name, fa.field)) {
                            const payload: ?ir.NodeIndex = if (call.args.len > 0)
                                try self.lowerExprNode(call.args[0])
                            else
                                null;
                            return try fb.emitUnionInit(@intCast(i), payload, base_type_idx, fa.span);
                        }
                    }
                    return ir.null_node;
                }
            }
        }

        // Generic function call: max(i64)(3, 5) — callee is a call expr (the inner call)
        if (callee_expr == .call) {
            const inner_call = callee_expr.call;
            // When inside a monomorphized generic body (type_substitution active),
            // re-resolve type args through substitution. The generic_instantiations map
            // is keyed by AST node index which is shared across instantiations, so the
            // last-checked instantiation overwrites earlier ones. We must construct the
            // correct GenericInstInfo for the current type substitution context.
            // Go pattern: each monomorphized body has concrete types fully resolved.
            const resolved_inst_info = blk: {
                if (self.type_substitution) |sub| {
                    // Get the generic function name from the callee ident
                    const inner_callee_node = self.tree.getNode(inner_call.callee) orelse break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    const inner_callee_expr = inner_callee_node.asExpr() orelse break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    if (inner_callee_expr != .ident) break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    const gen_name = inner_callee_expr.ident.name;

                    // Look up generic function info to get the generic_node
                    const gen_info = self.chk.generic_functions.get(gen_name) orelse {
                        break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    };

                    // Resolve type args through current substitution
                    var resolved_type_args = std.ArrayListUnmanaged(TypeIndex){};
                    defer resolved_type_args.deinit(self.allocator);
                    for (inner_call.args) |arg_node_idx| {
                        const resolved = self.resolveTypeNode(arg_node_idx);
                        if (resolved != TypeRegistry.VOID) {
                            try resolved_type_args.append(self.allocator, resolved);
                        } else {
                            // Try as ident (type parameter name resolved through substitution)
                            const anode = self.tree.getNode(arg_node_idx) orelse continue;
                            const aexpr = anode.asExpr() orelse continue;
                            if (aexpr == .ident) {
                                if (sub.get(aexpr.ident.name)) |sub_type| {
                                    try resolved_type_args.append(self.allocator, sub_type);
                                }
                            }
                        }
                    }

                    // Build the concrete cache key: "funcName(typeIdx1;typeIdx2)"
                    var key_buf = std.ArrayListUnmanaged(u8){};
                    defer key_buf.deinit(self.allocator);
                    const kw = key_buf.writer(self.allocator);
                    kw.writeAll(gen_name) catch break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    kw.writeByte('(') catch break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    for (resolved_type_args.items, 0..) |arg, i| {
                        if (i > 0) kw.writeByte(';') catch break :blk self.chk.generic_instantiations.get(inner_call.callee);
                        std.fmt.format(kw, "{d}", .{arg}) catch break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    }
                    kw.writeByte(')') catch break :blk self.chk.generic_instantiations.get(inner_call.callee);
                    const concrete_name = self.allocator.dupe(u8, key_buf.items) catch break :blk self.chk.generic_instantiations.get(inner_call.callee);

                    // Construct GenericInstInfo directly — the generic_instantiations map
                    // (keyed by AST node) may have been overwritten by a later instantiation.
                    // Go pattern: mangled symbol name is the canonical identifier (noder/reader.go:863).
                    // Zig pattern: InternPool keys by (generic_owner, comptime_args) not call site.
                    break :blk @as(?checker.GenericInstInfo, .{
                        .concrete_name = concrete_name,
                        .generic_node = gen_info.node_idx,
                        .type_args = try self.allocator.dupe(TypeIndex, resolved_type_args.items),
                    });
                } else {
                    break :blk self.chk.generic_instantiations.get(inner_call.callee);
                }
            };
            if (resolved_inst_info) |inst_info| {
                try self.ensureGenericFnQueued(inst_info);
                // Lower value arguments
                var gen_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                defer gen_args.deinit(self.allocator);
                // Get concrete func type for param checking
                const concrete_func_type = self.type_reg.lookupByName(inst_info.concrete_name) orelse TypeRegistry.VOID;
                const concrete_info = self.type_reg.get(concrete_func_type);
                const cparam_types: ?[]const types.FuncParam = if (concrete_info == .func) concrete_info.func.params else null;
                for (call.args, 0..) |arg_idx, arg_i| {
                    const ast_node = self.tree.getNode(arg_idx) orelse continue;
                    const ast_expr = ast_node.asExpr() orelse continue;
                    var arg_node: ir.NodeIndex = ir.null_node;
                    if (ast_expr == .literal and ast_expr.literal.kind == .string) {
                        var param_is_pointer = false;
                        if (cparam_types) |params| {
                            if (arg_i < params.len) {
                                const param_type = self.type_reg.get(params[arg_i].type_idx);
                                param_is_pointer = (param_type == .pointer);
                            }
                        }
                        if (param_is_pointer) {
                            const str_node = try self.lowerLiteral(ast_expr.literal);
                            arg_node = try fb.emitSlicePtr(str_node, TypeRegistry.I64, call.span);
                        } else {
                            arg_node = try self.lowerLiteral(ast_expr.literal);
                        }
                    } else {
                        arg_node = try self.lowerExprNode(arg_idx);
                    }
                    if (arg_node == ir.null_node) continue;
                    try gen_args.append(self.allocator, arg_node);
                }
                const ret_type = if (concrete_info == .func) concrete_info.func.return_type else TypeRegistry.VOID;
                return try fb.emitCall(inst_info.concrete_name, gen_args.items, false, ret_type, call.span);
            }
        }

        // Check for builtins
        if (callee_expr == .ident) {
            const name = callee_expr.ident.name;
            if (std.mem.eql(u8, name, "len")) return try self.lowerBuiltinLen(call);
            if (std.mem.eql(u8, name, "print")) return try self.lowerBuiltinPrint(call, false, 1);
            if (std.mem.eql(u8, name, "println")) return try self.lowerBuiltinPrint(call, true, 1);
            if (std.mem.eql(u8, name, "eprint")) return try self.lowerBuiltinPrint(call, false, 2);
            if (std.mem.eql(u8, name, "eprintln")) return try self.lowerBuiltinPrint(call, true, 2);
            if (std.mem.eql(u8, name, "__string_make")) return try self.lowerBuiltinStringMake(call);
            if (std.mem.eql(u8, name, "append")) return try self.lowerBuiltinAppend(call);
        }

        // Regular function call
        var args = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer args.deinit(self.allocator);

        // Get function type for parameter checking
        const func_type_idx = self.inferExprType(call.callee);
        const func_type_info = self.type_reg.get(func_type_idx);
        const param_types: ?[]const types.FuncParam = if (func_type_info == .func) func_type_info.func.params else null;

        for (call.args, 0..) |arg_idx, arg_i| {
            const ast_node = self.tree.getNode(arg_idx) orelse continue;
            const ast_expr = ast_node.asExpr() orelse continue;
            var arg_node: ir.NodeIndex = ir.null_node;

            // Check if string literal passed to pointer param
            if (ast_expr == .literal and ast_expr.literal.kind == .string) {
                var param_is_pointer = false;
                if (param_types) |params| {
                    if (arg_i < params.len) {
                        const param_type = self.type_reg.get(params[arg_i].type_idx);
                        param_is_pointer = (param_type == .pointer);
                    }
                }
                if (param_is_pointer) {
                    const str_node = try self.lowerLiteral(ast_expr.literal);
                    arg_node = try fb.emitSlicePtr(str_node, TypeRegistry.I64, call.span);
                } else {
                    arg_node = try self.lowerLiteral(ast_expr.literal);
                }
            } else {
                arg_node = try self.lowerExprNode(arg_idx);
            }
            if (arg_node == ir.null_node) continue;
            try args.append(self.allocator, arg_node);
        }

        const return_type = if (func_type_info == .func) func_type_info.func.return_type else TypeRegistry.VOID;
        const func_name = if (callee_expr == .ident) callee_expr.ident.name else "";

        // Determine if this is an indirect call (function pointer)
        // Direct calls use function names; indirect calls use variables/expressions
        // An ident is indirect if it's a local variable (not a function declaration)
        const is_indirect = if (callee_expr != .ident)
            true
        else blk: {
            // Check if the identifier is a local variable (function pointer)
            // vs a function declaration (direct call)
            if (fb.lookupLocal(func_name) != null) {
                break :blk true; // Local variable → indirect call
            }
            break :blk false; // Function name → direct call
        };

        if (is_indirect) {
            // Closure struct decomposition: load table_idx from offset 0
            const closure_ptr = try self.lowerExprNode(call.callee);
            const table_idx = try fb.emitPtrLoadValue(closure_ptr, TypeRegistry.I64, call.span);
            // Use closure_call: callee=table_idx, context=closure_ptr, args
            return try fb.emitClosureCall(table_idx, closure_ptr, args.items, return_type, call.span);
        }
        return try fb.emitCall(func_name, args.items, false, return_type, call.span);
    }

    /// Go pattern: check.later() (types2/call.go:152-166) defers verification to after
    /// all instantiations are collected. We similarly defer lowering to after all regular
    /// declarations, preventing builder state corruption from nested startFunc/endFunc.
    fn ensureGenericFnQueued(self: *Lowerer, inst_info: checker.GenericInstInfo) !void {
        if (self.lowered_generics.contains(inst_info.concrete_name)) return;
        try self.lowered_generics.put(inst_info.concrete_name, {});
    }

    /// Go pattern: deferred actions run after all call sites processed (types2/call.go).
    /// Zig pattern: ensureFuncBodyAnalysisQueued (Zcu.zig:3522) queues analysis jobs.
    /// We process all queued generic instantiations as top-level functions.
    fn lowerQueuedGenericFunctions(self: *Lowerer) !void {
        // Use generic_inst_by_name which has ALL instantiations (never overwritten).
        // The generic_instantiations map is keyed by AST node index, so the second
        // instantiation of the same generic function overwrites the first.
        // generic_inst_by_name is keyed by concrete name, so all are preserved.
        //
        // We must loop until no new instantiations are queued, because lowering one
        // generic body may queue new ones (e.g., List_append calls List_ensureCapacity).
        var emitted = std.StringHashMap(void).init(self.allocator);
        defer emitted.deinit();
        var made_progress = true;
        while (made_progress) {
            made_progress = false;
            var it = self.chk.generic_inst_by_name.valueIterator();
            while (it.next()) |inst_info| {
                if (!self.lowered_generics.contains(inst_info.concrete_name)) continue;
                if (emitted.contains(inst_info.concrete_name)) continue;
                try emitted.put(inst_info.concrete_name, {});
                try self.lowerGenericFnInstance(inst_info.*);
                made_progress = true;
            }
        }
    }

    /// Lower one concrete generic function as a top-level function.
    fn lowerGenericFnInstance(self: *Lowerer, inst_info: checker.GenericInstInfo) !void {
        const fn_node = self.tree.getNode(inst_info.generic_node) orelse return;
        const fn_decl_node = fn_node.asDecl() orelse return;
        if (fn_decl_node != .fn_decl) return;
        const f = fn_decl_node.fn_decl;

        // Build type substitution map: T -> i64, U -> f64, etc.
        // For impl block methods, type_params come from the impl block (type_param_names),
        // not from the fn_decl (which has no type_params for impl methods).
        var sub_map = std.StringHashMap(TypeIndex).init(self.allocator);
        defer sub_map.deinit();
        const param_names = if (inst_info.type_param_names.len > 0) inst_info.type_param_names else f.type_params;
        for (param_names, 0..) |param_name, i| {
            if (i < inst_info.type_args.len) {
                try sub_map.put(param_name, inst_info.type_args[i]);
            }
        }

        // Zig pattern: ensureFuncBodyUpToDate (Zcu.zig:3522-3543) — re-analyze per instance.
        // AST nodes are shared across instantiations, so expr_types must be refreshed
        // before lowering each concrete instance (e.g., add(i64) vs add(i32) share the
        // same body AST but need different expr_types). The checker already eagerly checked
        // the body during instantiateGenericFunc; this re-check updates expr_types for the
        // current instantiation's concrete types.
        self.chk.type_substitution = sub_map;
        self.chk.checkFnDeclWithName(f, inst_info.generic_node, inst_info.concrete_name) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        self.chk.type_substitution = null;

        // Lower the concrete function body with type substitution active
        self.type_substitution = sub_map;
        defer self.type_substitution = null;

        const return_type = self.resolveTypeNode(f.return_type);
        self.builder.startFunc(inst_info.concrete_name, TypeRegistry.VOID, return_type, f.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            for (f.params) |param| {
                const param_type = self.resolveTypeNode(param.type_expr);
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }
            if (f.body != null_node) {
                _ = try self.lowerBlockNode(f.body);
                if (return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, f.span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn lowerMethodCall(self: *Lowerer, call: ast.Call, fa: ast.FieldAccess, method_info: types.MethodInfo) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        var args = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer args.deinit(self.allocator);

        const base_type_idx = self.inferExprType(fa.base);
        const base_type = self.type_reg.get(base_type_idx);

        // Prepare receiver
        const receiver_val = blk: {
            if (method_info.receiver_is_ptr) {
                if (base_type == .pointer) {
                    break :blk try self.lowerExprNode(fa.base);
                } else {
                    const base_node = self.tree.getNode(fa.base) orelse return ir.null_node;
                    const base_expr = base_node.asExpr() orelse return ir.null_node;
                    if (base_expr == .ident) {
                        if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                            // Use pointer type so SSA builder doesn't decompose as struct value
                            const ptr_type = self.type_reg.makePointer(base_type_idx) catch TypeRegistry.I64;
                            break :blk try fb.emitAddrLocal(local_idx, ptr_type, fa.span);
                        }
                        if (self.builder.lookupGlobal(base_expr.ident.name)) |g| {
                            const ptr_type = self.type_reg.makePointer(base_type_idx) catch base_type_idx;
                            break :blk try fb.emitAddrGlobal(g.idx, base_expr.ident.name, ptr_type, fa.span);
                        }
                    }
                    break :blk try self.lowerExprNode(fa.base);
                }
            } else {
                if (base_type == .pointer) {
                    const ptr_val = try self.lowerExprNode(fa.base);
                    break :blk try fb.emitPtrLoadValue(ptr_val, base_type_idx, fa.span);
                } else {
                    break :blk try self.lowerExprNode(fa.base);
                }
            }
        };

        if (receiver_val == ir.null_node) return ir.null_node;
        try args.append(self.allocator, receiver_val);

        // Get param types for string->ptr coercion
        const func_type_for_params = self.type_reg.get(method_info.func_type);
        var param_types: ?[]const types.FuncParam = null;
        if (func_type_for_params == .func) param_types = func_type_for_params.func.params;

        for (call.args, 0..) |arg_idx, arg_i| {
            const ast_node = self.tree.getNode(arg_idx) orelse continue;
            const ast_expr = ast_node.asExpr() orelse continue;
            var arg_node: ir.NodeIndex = ir.null_node;

            if (ast_expr == .literal and ast_expr.literal.kind == .string) {
                var param_is_pointer = false;
                if (param_types) |params| {
                    const param_idx = arg_i + 1;
                    if (param_idx < params.len) {
                        const param_type = self.type_reg.get(params[param_idx].type_idx);
                        param_is_pointer = (param_type == .pointer);
                    }
                }
                if (param_is_pointer) {
                    const str_node = try self.lowerLiteral(ast_expr.literal);
                    arg_node = try fb.emitSlicePtr(str_node, TypeRegistry.I64, call.span);
                } else {
                    arg_node = try self.lowerLiteral(ast_expr.literal);
                }
            } else {
                arg_node = try self.lowerExprNode(arg_idx);
            }
            if (arg_node == ir.null_node) continue;
            try args.append(self.allocator, arg_node);
        }

        const func_type = self.type_reg.get(method_info.func_type);
        const return_type = if (func_type == .func) func_type.func.return_type else TypeRegistry.VOID;
        return try fb.emitCall(method_info.func_name, args.items, false, return_type, call.span);
    }

    fn lowerBuiltinLen(self: *Lowerer, call: ast.Call) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (call.args.len != 1) return ir.null_node;
        const arg_idx = call.args[0];
        const arg_node = self.tree.getNode(arg_idx) orelse return ir.null_node;
        const arg_expr = arg_node.asExpr() orelse return ir.null_node;

        if (arg_expr == .literal and arg_expr.literal.kind == .string) {
            var buf: [4096]u8 = undefined;
            const unescaped = parseStringLiteral(arg_expr.literal.value, &buf);
            return try fb.emitConstInt(@intCast(unescaped.len), TypeRegistry.INT, call.span);
        }

        if (arg_expr == .ident) {
            if (fb.lookupLocal(arg_expr.ident.name)) |local_idx| {
                const local_type_idx = fb.locals.items[local_idx].type_idx;
                const local_type = self.type_reg.get(local_type_idx);
                if (local_type_idx == TypeRegistry.STRING) return try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, call.span);
                // Slices have same layout as strings: (ptr, len) - read len at offset 8
                if (local_type == .slice) return try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, call.span);
                if (local_type == .array) return try fb.emitConstInt(@intCast(local_type.array.length), TypeRegistry.INT, call.span);
            }
        }

        if (arg_expr == .field_access) {
            const arg_type = self.inferExprType(arg_idx);
            if (arg_type == TypeRegistry.STRING) {
                const str_val = try self.lowerFieldAccess(arg_expr.field_access);
                return try fb.emitSliceLen(str_val, call.span);
            }
        }

        const arg_type = self.inferExprType(arg_idx);
        if (arg_type == TypeRegistry.STRING) {
            const str_val = try self.lowerExprNode(arg_idx);
            return try fb.emitSliceLen(str_val, call.span);
        }
        return ir.null_node;
    }

    fn lowerBuiltinStringMake(self: *Lowerer, call: ast.Call) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (call.args.len != 2) return ir.null_node;
        const ptr_val = try self.lowerExprNode(call.args[0]);
        const len_val = try self.lowerExprNode(call.args[1]);
        return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = ptr_val, .len = len_val } }, TypeRegistry.STRING, call.span));
    }

    /// Lower append(slice, elem) builtin
    /// Go reference: walk/builtin.go walkAppend (lines 44-128)
    /// Go reference: runtime/slice.go growslice (lines 178-287)
    ///
    /// Go reference: cmd/compile/internal/walk/builtin.go walkAppend (lines 89-128)
    ///
    /// Go's exact pattern:
    ///   newLen := s.len + num                              // line 91
    ///   if uint(newLen) <= uint(s.cap) {                   // line 93-96
    ///       s = s[:newLen]                                 // line 98-103 (fast path)
    ///   } else {
    ///       s = growslice(s.ptr, newLen, s.cap, num, T)    // line 105-112 (slow path)
    ///   }
    ///   s[oldLen] = elem                                   // line 117-122 (after if/else)
    ///   return s
    fn lowerBuiltinAppend(self: *Lowerer, call: ast.Call) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (call.args.len != 2) return ir.null_node;

        // Infer types
        const slice_type_idx = self.inferExprType(call.args[0]);
        const slice_type = self.type_reg.get(slice_type_idx);
        const elem_type_idx: TypeIndex = switch (slice_type) {
            .slice => |s| s.elem,
            .array => |a| a.elem,
            else => return ir.null_node,
        };
        const elem_size = self.type_reg.sizeOf(elem_type_idx);
        const result_type = self.type_reg.makeSlice(elem_type_idx) catch return ir.null_node;
        const ptr_type = try self.type_reg.makePointer(elem_type_idx);

        // Get ptr, len, cap from source
        // Go reference: runtime/slice.go lines 16-20
        var old_ptr: ir.NodeIndex = ir.null_node;
        var old_len: ir.NodeIndex = ir.null_node;
        var old_cap: ir.NodeIndex = ir.null_node;

        const arg_node = self.tree.getNode(call.args[0]) orelse return ir.null_node;
        const arg_expr = arg_node.asExpr() orelse return ir.null_node;

        if (slice_type == .array) {
            if (arg_expr == .ident) {
                if (fb.lookupLocal(arg_expr.ident.name)) |local_idx| {
                    old_ptr = try fb.emitAddrLocal(local_idx, ptr_type, call.span);
                    const arr_len = try fb.emitConstInt(@intCast(slice_type.array.length), TypeRegistry.I64, call.span);
                    old_len = arr_len;
                    old_cap = arr_len;
                }
            }
        } else if (slice_type == .slice) {
            if (arg_expr == .ident) {
                if (fb.lookupLocal(arg_expr.ident.name)) |local_idx| {
                    old_ptr = try fb.emitFieldLocal(local_idx, 0, 0, ptr_type, call.span);
                    old_len = try fb.emitFieldLocal(local_idx, 1, 8, TypeRegistry.I64, call.span);
                    old_cap = try fb.emitFieldLocal(local_idx, 2, 16, TypeRegistry.I64, call.span);
                }
            }
        }

        if (old_ptr == ir.null_node or old_len == ir.null_node or old_cap == ir.null_node) return ir.null_node;

        // Go line 91: newLen := s.len + num
        const one = try fb.emitConstInt(1, TypeRegistry.I64, call.span);
        const new_len = try fb.emitBinary(.add, old_len, one, TypeRegistry.I64, call.span);
        const elem_size_val = try fb.emitConstInt(@intCast(elem_size), TypeRegistry.I64, call.span);

        // Go lines 93-96: if uint(newLen) <= uint(s.cap)
        const cond = try fb.emitBinary(.le, new_len, old_cap, TypeRegistry.BOOL, call.span);

        // Create temp locals for result (like Go's assignment to 's')
        // Go assigns to s in both branches; we use locals to carry values across blocks
        const result_ptr_name = try std.fmt.allocPrint(self.allocator, "__append_ptr_{d}", .{fb.locals.items.len});
        const result_ptr_local = try fb.addLocalWithSize(result_ptr_name, TypeRegistry.I64, true, 8);
        const result_cap_name = try std.fmt.allocPrint(self.allocator, "__append_cap_{d}", .{fb.locals.items.len});
        const result_cap_local = try fb.addLocalWithSize(result_cap_name, TypeRegistry.I64, true, 8);

        // Create blocks (Go lines 94-114)
        const then_block = try fb.newBlock("append.fast");
        const else_block = try fb.newBlock("append.grow");
        const merge_block = try fb.newBlock("append.merge");

        _ = try fb.emitBranch(cond, then_block, else_block, call.span);

        // Then block (Go lines 98-103): s = s[:newLen]
        // Fast path: no allocation, keep old_ptr and old_cap
        fb.setBlock(then_block);
        _ = try fb.emitStoreLocal(result_ptr_local, old_ptr, call.span);
        _ = try fb.emitStoreLocal(result_cap_local, old_cap, call.span);
        _ = try fb.emitJump(merge_block, call.span);

        // Else block (Go lines 105-112): s = growslice(...)
        // Slow path: allocate and copy
        fb.setBlock(else_block);
        // Go reference: runtime/slice.go nextslicecap (lines 326-358)
        var cap_args = [_]ir.NodeIndex{ new_len, old_cap };
        const new_cap_grow = try fb.emitCall("cot_nextslicecap", &cap_args, false, TypeRegistry.I64, call.span);
        // Go reference: runtime/slice.go growslice (lines 178-287)
        var grow_args = [_]ir.NodeIndex{ old_ptr, old_len, new_cap_grow, elem_size_val };
        const new_ptr_grow = try fb.emitCall("cot_growslice", &grow_args, false, TypeRegistry.I64, call.span);
        _ = try fb.emitStoreLocal(result_ptr_local, new_ptr_grow, call.span);
        _ = try fb.emitStoreLocal(result_cap_local, new_cap_grow, call.span);
        _ = try fb.emitJump(merge_block, call.span);

        // Merge block - read results from locals
        fb.setBlock(merge_block);
        const new_ptr = try fb.emitLoadLocal(result_ptr_local, TypeRegistry.I64, call.span);
        const new_cap = try fb.emitLoadLocal(result_cap_local, TypeRegistry.I64, call.span);

        // Go lines 117-122: s[oldLen] = elem (after if/else)
        const elem_val = try self.lowerExprNode(call.args[1]);
        const offset = try fb.emitBinary(.mul, old_len, elem_size_val, TypeRegistry.I64, call.span);
        const dest_ptr = try fb.emitBinary(.add, new_ptr, offset, ptr_type, call.span);
        _ = try fb.emitPtrStoreValue(dest_ptr, elem_val, call.span);

        // Return slice{new_ptr, new_len, new_cap}
        return try fb.emit(ir.Node.init(.{ .slice_header = .{ .ptr = new_ptr, .len = new_len, .cap = new_cap } }, result_type, call.span));
    }

    fn lowerBuiltinPrint(self: *Lowerer, call: ast.Call, is_println: bool, fd: i32) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (call.args.len != 1) return ir.null_node;
        const arg_type = self.inferExprType(call.args[0]);
        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
        const type_info = self.type_reg.get(arg_type);
        const is_integer = type_info == .enum_type or arg_type == TypeRegistry.I8 or arg_type == TypeRegistry.I16 or arg_type == TypeRegistry.I32 or arg_type == TypeRegistry.I64 or arg_type == TypeRegistry.INT or arg_type == TypeRegistry.U8 or arg_type == TypeRegistry.U16 or arg_type == TypeRegistry.U32 or arg_type == TypeRegistry.U64 or arg_type == TypeRegistry.UNTYPED_INT;

        if (is_integer) {
            const int_val = try self.lowerExprNode(call.args[0]);
            var print_args = [_]ir.NodeIndex{int_val};
            _ = try fb.emitCall(if (fd == 2) "__eprint_int" else "__print_int", &print_args, false, TypeRegistry.VOID, call.span);
        } else {
            const str_val = try self.lowerExprNode(call.args[0]);
            const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, call.span);
            const len_val = try fb.emitSliceLen(str_val, call.span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I32, call.span);
            var write_args = [_]ir.NodeIndex{ fd_val, ptr_val, len_val };
            _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, call.span);
        }

        if (is_println) {
            const nl_idx = try fb.addStringLiteral("\n");
            const nl_str = try fb.emit(ir.Node.init(.{ .const_slice = .{ .string_index = nl_idx } }, TypeRegistry.STRING, call.span));
            const nl_ptr = try fb.emitSlicePtr(nl_str, ptr_type, call.span);
            const nl_len = try fb.emitConstInt(1, TypeRegistry.I64, call.span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I32, call.span);
            var nl_args = [_]ir.NodeIndex{ fd_val, nl_ptr, nl_len };
            _ = try fb.emitCall("write", &nl_args, false, TypeRegistry.I64, call.span);
        }
        return ir.null_node;
    }

    fn lowerBuiltinCall(self: *Lowerer, bc: ast.BuiltinCall) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        if (std.mem.eql(u8, bc.name, "sizeOf")) {
            const type_idx = self.resolveTypeNode(bc.type_arg);
            return try fb.emitConstInt(@intCast(self.type_reg.sizeOf(type_idx)), TypeRegistry.I64, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "alignOf")) {
            const type_idx = self.resolveTypeNode(bc.type_arg);
            return try fb.emitConstInt(@intCast(self.type_reg.alignOf(type_idx)), TypeRegistry.I64, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "intCast")) {
            const target_type = self.resolveTypeNode(bc.type_arg);
            const value = try self.lowerExprNode(bc.args[0]);
            return try fb.emitIntCast(value, target_type, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "ptrCast")) {
            const target_type = self.resolveTypeNode(bc.type_arg);
            const value = try self.lowerExprNode(bc.args[0]);
            return try fb.emitPtrCast(value, target_type, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "intToPtr")) {
            const target_type = self.resolveTypeNode(bc.type_arg);
            const value = try self.lowerExprNode(bc.args[0]);
            return try fb.emitIntToPtr(value, target_type, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "ptrToInt")) {
            const value = try self.lowerExprNode(bc.args[0]);
            return try fb.emitPtrToInt(value, TypeRegistry.I64, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "string")) {
            const ptr_val = try self.lowerExprNode(bc.args[0]);
            const len_val = try self.lowerExprNode(bc.args[1]);
            return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = ptr_val, .len = len_val } }, TypeRegistry.STRING, bc.span));
        }
        if (std.mem.eql(u8, bc.name, "assert")) {
            const cond = try self.lowerExprNode(bc.args[0]);
            const then_block = try fb.newBlock("assert.ok");
            const fail_block = try fb.newBlock("assert.fail");
            _ = try fb.emitBranch(cond, then_block, fail_block, bc.span);
            fb.setBlock(fail_block);
            const msg = if (self.current_test_name) |name| try std.fmt.allocPrint(self.allocator, "assertion failed in test \"{s}\"\n", .{name}) else "assertion failed\n";
            const msg_copy = try self.allocator.dupe(u8, msg);
            const msg_idx = try fb.addStringLiteral(msg_copy);
            const msg_str = try fb.emitConstSlice(msg_idx, bc.span);
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            const msg_ptr = try fb.emitSlicePtr(msg_str, ptr_type, bc.span);
            const msg_len = try fb.emitSliceLen(msg_str, bc.span);
            const fd_val = try fb.emitConstInt(2, TypeRegistry.I32, bc.span);
            var write_args = [_]ir.NodeIndex{ fd_val, msg_ptr, msg_len };
            _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, bc.span);
            const exit_code = try fb.emitConstInt(1, TypeRegistry.I32, bc.span);
            var exit_args = [_]ir.NodeIndex{exit_code};
            _ = try fb.emitCall("exit", &exit_args, false, TypeRegistry.VOID, bc.span);
            _ = try fb.emitJump(then_block, bc.span);
            fb.setBlock(then_block);
            return ir.null_node;
        }
        if (std.mem.eql(u8, bc.name, "alloc")) {
            const size = try self.lowerExprNode(bc.args[0]);
            const metadata_node = try fb.emitConstInt(0, TypeRegistry.I64, bc.span);
            var alloc_args = [_]ir.NodeIndex{ metadata_node, size };
            return try fb.emitCall("cot_alloc", &alloc_args, false, TypeRegistry.I64, bc.span);
        }
        if (std.mem.eql(u8, bc.name, "dealloc")) {
            const ptr = try self.lowerExprNode(bc.args[0]);
            var dealloc_args = [_]ir.NodeIndex{ptr};
            _ = try fb.emitCall("cot_dealloc", &dealloc_args, false, TypeRegistry.VOID, bc.span);
            return ir.null_node;
        }
        if (std.mem.eql(u8, bc.name, "realloc")) {
            const ptr = try self.lowerExprNode(bc.args[0]);
            const new_size = try self.lowerExprNode(bc.args[1]);
            var realloc_args = [_]ir.NodeIndex{ ptr, new_size };
            return try fb.emitCall("cot_realloc", &realloc_args, false, TypeRegistry.I64, bc.span);
        }
        return ir.null_node;
    }

    // ============================================================================
    // Type Utilities
    // ============================================================================

    fn resolveTypeNode(self: *Lowerer, idx: NodeIndex) TypeIndex {
        const node = self.tree.getNode(idx) orelse return TypeRegistry.VOID;
        const expr = node.asExpr() orelse return TypeRegistry.VOID;
        if (expr != .type_expr) return TypeRegistry.VOID;
        const te = expr.type_expr;

        switch (te.kind) {
            .named => |name| {
                // Check type substitution first (active during generic instantiation)
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
            },
            .pointer => |inner| {
                const elem = self.resolveTypeNode(inner);
                return self.type_reg.makePointer(elem) catch TypeRegistry.VOID;
            },
            .array => |arr| {
                const elem = self.resolveTypeNode(arr.elem);
                const size_node = self.tree.getNode(arr.size) orelse return TypeRegistry.VOID;
                const size_expr = size_node.asExpr() orelse return TypeRegistry.VOID;
                if (size_expr != .literal) return TypeRegistry.VOID;
                const len = std.fmt.parseInt(usize, size_expr.literal.value, 10) catch return TypeRegistry.VOID;
                return self.type_reg.makeArray(elem, len) catch TypeRegistry.VOID;
            },
            .slice => |inner| {
                const elem = self.resolveTypeNode(inner);
                return self.type_reg.makeSlice(elem) catch TypeRegistry.VOID;
            },
            .optional => |inner| {
                const elem = self.resolveTypeNode(inner);
                return self.type_reg.makeOptional(elem) catch TypeRegistry.VOID;
            },
            .function => |fn_type| {
                var param_types = std.ArrayListUnmanaged(types.FuncParam){};
                defer param_types.deinit(self.allocator);
                for (fn_type.params) |param_idx| {
                    const param_type = self.resolveTypeNode(param_idx);
                    param_types.append(self.allocator, .{ .name = "", .type_idx = param_type }) catch return TypeRegistry.VOID;
                }
                const ret_type = self.resolveTypeNode(fn_type.ret);
                return self.type_reg.makeFunc(param_types.items, ret_type) catch TypeRegistry.VOID;
            },
            .error_union => |eu| {
                const elem = self.resolveTypeNode(eu.elem);
                if (eu.error_set != null_node) {
                    const es_type = self.resolveTypeNode(eu.error_set);
                    if (es_type != TypeRegistry.VOID) {
                        return self.type_reg.makeErrorUnionWithSet(elem, es_type) catch TypeRegistry.VOID;
                    }
                }
                return self.type_reg.makeErrorUnion(elem) catch TypeRegistry.VOID;
            },
            .generic_instance => |gi| {
                // Look up the concrete monomorphized type by cache key
                var buf = std.ArrayListUnmanaged(u8){};
                defer buf.deinit(self.allocator);
                const writer = buf.writer(self.allocator);
                writer.writeAll(gi.name) catch return TypeRegistry.VOID;
                writer.writeByte('(') catch return TypeRegistry.VOID;
                for (gi.type_args, 0..) |arg_node, i| {
                    if (i > 0) writer.writeByte(';') catch return TypeRegistry.VOID;
                    const arg_type = self.resolveTypeNode(arg_node);
                    std.fmt.format(writer, "{d}", .{arg_type}) catch return TypeRegistry.VOID;
                }
                writer.writeByte(')') catch return TypeRegistry.VOID;
                return self.type_reg.lookupByName(buf.items) orelse TypeRegistry.VOID;
            },
            else => return TypeRegistry.VOID,
        }
    }

    /// Resolve a generic type from base name + type_args NodeIndex array to a concrete TypeIndex.
    /// Uses the same cache key format as resolveTypeNode's generic_instance handler: "Name(5;17)".
    /// Handles both type_expr nodes (from parseType) and ident nodes (from parseExpr in call args).
    fn resolveGenericTypeName(self: *Lowerer, base_name: []const u8, type_args: []const NodeIndex) TypeIndex {
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

    /// Resolve a type argument that may be either a type_expr node (from parseType)
    /// or an ident node (from parseExpr, e.g., call args like `List(i64)`).
    fn resolveTypeArgNode(self: *Lowerer, idx: NodeIndex) TypeIndex {
        const node = self.tree.getNode(idx) orelse return TypeRegistry.VOID;
        const expr = node.asExpr() orelse return TypeRegistry.VOID;
        // Handle ident nodes (type keywords parsed as idents in expression context)
        if (expr == .ident) {
            const name = expr.ident.name;
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
        // Fall through to standard type_expr resolution
        return self.resolveTypeNode(idx);
    }

    /// Lower error.X literal — constructs an error union value with tag=1 and error index as payload.
    /// Returns a POINTER to the error union in the stack frame (16-byte value can't be
    /// returned by value in Wasm's single-i64 return convention).
    fn lowerErrorLiteral(self: *Lowerer, el: ast.ErrorLiteral) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        // Get the function's return type (must be error union)
        const ret_type = fb.return_type;
        const ret_info = self.type_reg.get(ret_type);
        if (ret_info != .error_union) return ir.null_node;

        // Look up the error variant index
        var error_idx: i64 = 0;
        if (ret_info.error_union.error_set != types.invalid_type) {
            const es_info = self.type_reg.get(ret_info.error_union.error_set);
            if (es_info == .error_set) {
                for (es_info.error_set.variants, 0..) |v, i| {
                    if (std.mem.eql(u8, v, el.error_name)) { error_idx = @intCast(i); break; }
                }
            }
        }

        // Create error union: tag=1 (error), payload=error_idx
        const size = self.type_reg.sizeOf(ret_type);
        const tmp_local = try fb.addLocalWithSize("__err_tmp", ret_type, false, size);
        const tag_val = try fb.emitConstInt(1, TypeRegistry.I64, el.span);
        _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_val, el.span);
        const idx_val = try fb.emitConstInt(error_idx, TypeRegistry.I64, el.span);
        _ = try fb.emitStoreLocalField(tmp_local, 1, 8, idx_val, el.span);
        // Return pointer to the error union (not the value itself)
        return try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, el.span);
    }

    /// Lower try expr — unwrap error union, propagate error to caller if tag != 0.
    /// The operand (function call) returns a POINTER to the error union in the callee's
    /// stack frame. We read through the pointer immediately (safe in single-threaded Wasm).
    fn lowerTryExpr(self: *Lowerer, te: ast.TryExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Get the error union type from the operand
        const operand_type = self.inferExprType(te.operand);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .error_union) {
            return try self.lowerExprNode(te.operand);
        }
        const elem_type = operand_info.error_union.elem;

        // Call returns a POINTER to the error union
        const eu_ptr = try self.lowerExprNode(te.operand);

        // Read tag at [ptr + 0]
        const tag_val = try fb.emitPtrLoadValue(eu_ptr, TypeRegistry.I64, te.span);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, te.span);
        const is_error = try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, te.span);

        // Branch: if error, propagate; if ok, continue
        const err_block = try fb.newBlock("try.err");
        const ok_block = try fb.newBlock("try.ok");
        _ = try fb.emitBranch(is_error, err_block, ok_block, te.span);

        // Error block: copy the error union to our own frame and return pointer to it
        fb.setBlock(err_block);
        const our_ret_type = fb.return_type;
        const eu_size = self.type_reg.sizeOf(our_ret_type);
        const err_local = try fb.addLocalWithSize("__try_err", our_ret_type, false, eu_size);
        // Copy tag
        _ = try fb.emitStoreLocalField(err_local, 0, 0, tag_val, te.span);
        // Copy error payload
        const payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, te.span);
        const err_payload = try fb.emitPtrLoadValue(payload_addr, TypeRegistry.I64, te.span);
        _ = try fb.emitStoreLocalField(err_local, 1, 8, err_payload, te.span);
        const err_ret = try fb.emitAddrLocal(err_local, TypeRegistry.I64, te.span);
        _ = try fb.emitRet(err_ret, te.span);

        // OK block: read the success payload at [ptr + 8]
        fb.setBlock(ok_block);
        const payload_addr_ok = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, te.span);
        return try fb.emitPtrLoadValue(payload_addr_ok, elem_type, te.span);
    }

    /// Lower catch expr — unwrap error union, use fallback on error.
    /// The operand (function call) returns a POINTER to the error union.
    fn lowerCatchExpr(self: *Lowerer, ce: ast.CatchExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Get the error union type from the operand
        const operand_type = self.inferExprType(ce.operand);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .error_union) {
            return try self.lowerExprNode(ce.operand);
        }
        const elem_type = operand_info.error_union.elem;

        // Call returns a POINTER to the error union
        const eu_ptr = try self.lowerExprNode(ce.operand);

        // Read tag at [ptr + 0]
        const tag_val = try fb.emitPtrLoadValue(eu_ptr, TypeRegistry.I64, ce.span);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, ce.span);
        const is_ok = try fb.emitBinary(.eq, tag_val, zero, TypeRegistry.BOOL, ce.span);

        // Allocate result local in caller's frame
        const result_size = self.type_reg.sizeOf(elem_type);
        const result_local = try fb.addLocalWithSize("__catch_result", elem_type, false, result_size);

        // Branch
        const ok_block = try fb.newBlock("catch.ok");
        const err_block = try fb.newBlock("catch.err");
        const merge_block = try fb.newBlock("catch.merge");
        _ = try fb.emitBranch(is_ok, ok_block, err_block, ce.span);

        // OK block: read success payload from [ptr + 8], store to result
        fb.setBlock(ok_block);
        const payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, ce.span);
        const success_val = try fb.emitPtrLoadValue(payload_addr, elem_type, ce.span);
        _ = try fb.emitStoreLocal(result_local, success_val, ce.span);
        _ = try fb.emitJump(merge_block, ce.span);

        // Error block: evaluate fallback
        fb.setBlock(err_block);
        if (ce.capture.len > 0) {
            const scope_depth = fb.markScopeEntry();
            const err_payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, ce.span);
            const err_val = try fb.emitPtrLoadValue(err_payload_addr, TypeRegistry.I64, ce.span);
            const capture_local = try fb.addLocalWithSize(ce.capture, TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(capture_local, err_val, ce.span);
            const fallback_val = try self.lowerExprNode(ce.fallback);
            _ = try fb.emitStoreLocal(result_local, fallback_val, ce.span);
            fb.restoreScope(scope_depth);
        } else {
            const fallback_val = try self.lowerExprNode(ce.fallback);
            _ = try fb.emitStoreLocal(result_local, fallback_val, ce.span);
        }
        _ = try fb.emitJump(merge_block, ce.span);

        // Merge block: load result
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, elem_type, ce.span);
    }

    fn inferExprType(self: *Lowerer, idx: NodeIndex) TypeIndex {
        return self.chk.expr_types.get(idx) orelse TypeRegistry.VOID;
    }

    fn inferBinaryType(self: *Lowerer, op: Token, left: NodeIndex, right: NodeIndex) TypeIndex {
        _ = right;
        return switch (op) {
            .eql, .neq, .lss, .leq, .gtr, .geq => TypeRegistry.BOOL,
            else => self.inferExprType(left),
        };
    }
};

// ============================================================================
// Helpers
// ============================================================================

fn tokenToBinaryOp(tok: Token) ir.BinaryOp {
    return switch (tok) {
        .add, .add_assign => .add,
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
        '"' => '"',
        '0' => 0,
        'x' => if (inner.len >= 4) std.fmt.parseInt(u8, inner[2..4], 16) catch 0 else 0,
        else => inner[1],
    };
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

// ============================================================================
// Tests
// ============================================================================

test "Lowerer basic init" {
    const allocator = std.testing.allocator;
    var tree = ast.Ast.init(allocator);
    defer tree.deinit();
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();
    var src = source.Source.init(allocator, "test.cot", "");
    defer src.deinit();
    var err = ErrorReporter.init(&src, null);
    var scope = checker.Scope.init(allocator, null);
    defer scope.deinit();
    var chk = checker.Checker.init(allocator, &tree, &type_reg, &err, &scope);
    defer chk.deinit();
    var lowerer = Lowerer.init(allocator, &tree, &type_reg, &err, &chk);
    defer lowerer.deinit();
    var ir_result = try lowerer.lower();
    defer ir_result.deinit();
    try std.testing.expectEqual(@as(usize, 0), ir_result.funcs.len);
}

// Full pipeline helper for end-to-end tests
const scanner_mod = @import("scanner.zig");
const parser_mod = @import("parser.zig");

const TestResult = struct {
    ir_data: ir.IR,
    type_reg: TypeRegistry,
    has_errors: bool,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *TestResult) void {
        self.arena.deinit();
    }
};

fn testPipeline(backing: std.mem.Allocator, code: []const u8) !TestResult {
    var arena = std.heap.ArenaAllocator.init(backing);
    errdefer arena.deinit();
    const allocator = arena.allocator();

    var src = source.Source.init(allocator, "test.cot", code);
    var err = ErrorReporter.init(&src, null);
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);
    var tree = ast.Ast.init(allocator);

    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    parser.parseFile() catch |e| {
        std.debug.print("Parse error: {}\n", .{e});
        return error.ParseFailed;
    };
    if (err.hasErrors()) return error.ParseErrors;

    var type_reg = try TypeRegistry.init(allocator);
    var global_scope = checker.Scope.init(allocator, null);
    var chk = checker.Checker.init(allocator, &tree, &type_reg, &err, &global_scope);

    chk.checkFile() catch |e| {
        std.debug.print("Check error: {}\n", .{e});
        return error.CheckFailed;
    };
    if (err.hasErrors()) return error.CheckErrors;

    var lowerer = Lowerer.init(allocator, &tree, &type_reg, &err, &chk);
    const ir_data = try lowerer.lower();
    return .{ .ir_data = ir_data, .type_reg = type_reg, .has_errors = err.hasErrors(), .arena = arena };
}

test "E2E: function returning constant" {
    const allocator = std.testing.allocator;
    const code = "fn answer() i64 { return 42 }";
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.ir_data.funcs.len);
    const func = &result.ir_data.funcs[0];
    try std.testing.expectEqualStrings("answer", func.name);

    // Verify we have a return node with const_int 42
    var found_const = false;
    var found_ret = false;
    for (func.nodes) |node| {
        if (node.data == .const_int and node.data.const_int.value == 42) found_const = true;
        if (node.data == .ret) found_ret = true;
    }
    try std.testing.expect(found_const);
    try std.testing.expect(found_ret);
}

test "E2E: function with parameters and binary op" {
    const allocator = std.testing.allocator;
    const code = "fn add(a: i64, b: i64) i64 { return a + b }";
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];
    try std.testing.expectEqualStrings("add", func.name);
    try std.testing.expectEqual(@as(usize, 2), func.params.len);

    // Verify binary add operation exists
    var found_add = false;
    for (func.nodes) |node| {
        if (node.data == .binary and node.data.binary.op == .add) found_add = true;
    }
    try std.testing.expect(found_add);
}

test "E2E: variable declaration and assignment" {
    const allocator = std.testing.allocator;
    const code =
        \\fn test_var() i64 {
        \\    var x = 10
        \\    x = x + 5
        \\    return x
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // Should have local for x
    try std.testing.expect(func.locals.len >= 1);

    // Verify store_local exists (for assignment)
    var found_store = false;
    for (func.nodes) |node| {
        if (node.data == .store_local) found_store = true;
    }
    try std.testing.expect(found_store);
}

test "E2E: if-else statement" {
    const allocator = std.testing.allocator;
    const code =
        \\fn max(a: i64, b: i64) i64 {
        \\    if a > b {
        \\        return a
        \\    } else {
        \\        return b
        \\    }
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // Should have branch node for if-else
    var found_branch = false;
    var found_gt = false;
    for (func.nodes) |node| {
        if (node.data == .branch) found_branch = true;
        if (node.data == .binary and node.data.binary.op == .gt) found_gt = true;
    }
    try std.testing.expect(found_branch);
    try std.testing.expect(found_gt);

    // Should have multiple blocks (entry, then, else, potentially merge)
    try std.testing.expect(func.blocks.len >= 3);
}

test "E2E: while loop" {
    const allocator = std.testing.allocator;
    const code =
        \\fn sum_to(n: i64) i64 {
        \\    var total = 0
        \\    var i = 0
        \\    while i < n {
        \\        total = total + i
        \\        i = i + 1
        \\    }
        \\    return total
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // While loop creates branch and jump nodes
    var found_branch = false;
    var found_jump = false;
    for (func.nodes) |node| {
        if (node.data == .branch) found_branch = true;
        if (node.data == .jump) found_jump = true;
    }
    try std.testing.expect(found_branch);
    try std.testing.expect(found_jump);

    // Should have loop blocks
    try std.testing.expect(func.blocks.len >= 3);
}

test "E2E: comparison operators" {
    const allocator = std.testing.allocator;
    const code =
        \\fn test_cmp(a: i64, b: i64) bool {
        \\    return a <= b
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    var found_le = false;
    for (func.nodes) |node| {
        if (node.data == .binary and node.data.binary.op == .le) found_le = true;
    }
    try std.testing.expect(found_le);
}

test "E2E: unary negation" {
    const allocator = std.testing.allocator;
    const code = "fn negate(x: i64) i64 { return -x }";
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    var found_neg = false;
    for (func.nodes) |node| {
        if (node.data == .unary and node.data.unary.op == .neg) found_neg = true;
    }
    try std.testing.expect(found_neg);
}

test "E2E: multiple functions" {
    const allocator = std.testing.allocator;
    const code =
        \\fn foo() i64 { return 1 }
        \\fn bar() i64 { return 2 }
        \\fn baz() i64 { return foo() + bar() }
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 3), result.ir_data.funcs.len);

    // Verify function calls in baz
    const baz_func = &result.ir_data.funcs[2];
    var call_count: usize = 0;
    for (baz_func.nodes) |node| {
        if (node.data == .call) call_count += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), call_count);
}

test "E2E: struct definition and access" {
    const allocator = std.testing.allocator;
    const code =
        \\struct Point { x: i64, y: i64 }
        \\fn get_x(p: Point) i64 { return p.x }
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.ir_data.funcs.len);

    // Verify struct type was registered
    try std.testing.expect(result.type_reg.lookupByName("Point") != null);
}

test "E2E: boolean operations" {
    const allocator = std.testing.allocator;
    const code =
        \\fn test_and(a: bool, b: bool) bool { return a and b }
        \\fn test_or(a: bool, b: bool) bool { return a or b }
        \\fn test_not(a: bool) bool { return not a }
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 3), result.ir_data.funcs.len);
}

test "E2E: nested if statements" {
    const allocator = std.testing.allocator;
    const code =
        \\fn classify(x: i64) i64 {
        \\    if x < 0 {
        \\        return -1
        \\    } else {
        \\        if x > 0 {
        \\            return 1
        \\        } else {
        \\            return 0
        \\        }
        \\    }
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // Count branch nodes - should have at least 2 for nested if
    var branch_count: usize = 0;
    for (func.nodes) |node| {
        if (node.data == .branch) branch_count += 1;
    }
    try std.testing.expect(branch_count >= 2);
}

test "E2E: recursive function" {
    const allocator = std.testing.allocator;
    const code =
        \\fn factorial(n: i64) i64 {
        \\    if n <= 1 {
        \\        return 1
        \\    } else {
        \\        return n * factorial(n - 1)
        \\    }
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];
    try std.testing.expectEqualStrings("factorial", func.name);

    // Should have recursive call to itself
    var found_recursive_call = false;
    for (func.nodes) |node| {
        if (node.data == .call and std.mem.eql(u8, node.data.call.func_name, "factorial")) {
            found_recursive_call = true;
        }
    }
    try std.testing.expect(found_recursive_call);
}

test "E2E: bitwise operators" {
    const allocator = std.testing.allocator;
    const code =
        \\fn test_bits(a: i64, b: i64) i64 {
        \\    return (a & b) | (a ^ b)
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    var found_and = false;
    var found_or = false;
    var found_xor = false;
    for (func.nodes) |node| {
        if (node.data == .binary) {
            switch (node.data.binary.op) {
                .bit_and => found_and = true,
                .bit_or => found_or = true,
                .bit_xor => found_xor = true,
                else => {},
            }
        }
    }
    try std.testing.expect(found_and);
    try std.testing.expect(found_or);
    try std.testing.expect(found_xor);
}

test "E2E: const declaration" {
    const allocator = std.testing.allocator;
    const code =
        \\fn test_const() i64 {
        \\    const x = 42
        \\    return x
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // Should have local for x (const locals still get created)
    try std.testing.expect(func.locals.len >= 1);
}

test "E2E: multiple statements in block" {
    const allocator = std.testing.allocator;
    const code =
        \\fn multi() i64 {
        \\    var a = 1
        \\    var b = 2
        \\    var c = 3
        \\    return a + b + c
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];

    // Should have 3 locals
    try std.testing.expectEqual(@as(usize, 3), func.locals.len);
}

test "E2E: expression as statement" {
    const allocator = std.testing.allocator;
    const code =
        \\fn side_effect(x: i64) i64 {
        \\    x + 1
        \\    return x
        \\}
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    // Expression statements are valid
    try std.testing.expect(!result.has_errors);
}

test "E2E: void function" {
    const allocator = std.testing.allocator;
    const code = "fn do_nothing() void { return }";
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    const func = &result.ir_data.funcs[0];
    try std.testing.expectEqualStrings("do_nothing", func.name);
}

test "E2E: enum definition" {
    const allocator = std.testing.allocator;
    const code =
        \\enum Color { red, green, blue }
        \\fn get_red() Color { return Color.red }
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.type_reg.lookupByName("Color") != null);
}
